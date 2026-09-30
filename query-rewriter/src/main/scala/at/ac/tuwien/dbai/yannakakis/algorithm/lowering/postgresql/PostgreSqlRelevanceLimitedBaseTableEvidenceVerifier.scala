package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.sql.{Connection, PreparedStatement, ResultSet}
import javax.sql.DataSource

import at.ac.tuwien.dbai.yannakakis.core.VariableId
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  QueryHypergraphConverter,
  QueryIR
}

import scala.util.control.NonFatal

/**
  * Exact PostgreSQL catalog admission for the relevance-limited COUNT path.
  *
  * Reusing one relational expression for multiple SQL occurrences is sound
  * only for stable base-table inputs in the current V4 scope. This verifier
  * therefore resolves every distinct QueryIR relation name with equality
  * predicates against `pg_catalog`, inside one read-only repeatable-read
  * transaction. It neither follows `search_path` nor accepts partitioned
  * tables, views, foreign tables, or row-security-enabled relations.
  */
private[yannakakis] object PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier {
  private val MaximumDistinctRelations = 4096
  private val MaximumColumnsPerRelation = 16384
  private val QueryTimeoutSeconds = 30

  private val DatabaseSql =
    "SELECT " +
      "database_catalog.datname::pg_catalog.text AS database_name, " +
      "database_catalog.oid::pg_catalog.int8 AS database_oid, " +
      "pg_catalog.current_setting('server_version_num')::pg_catalog.int4 " +
      "AS server_version_num " +
      "FROM pg_catalog.pg_database AS database_catalog " +
      "WHERE database_catalog.datname OPERATOR(pg_catalog.=) " +
      "pg_catalog.current_database()"

  private val RelationSql =
    "SELECT " +
      "namespace.nspname::pg_catalog.text AS namespace_name, " +
      "namespace.oid::pg_catalog.int8 AS namespace_oid, " +
      "relation.relname::pg_catalog.text AS relation_name, " +
      "relation.oid::pg_catalog.int8 AS relation_oid, " +
      "relation.relkind::pg_catalog.text AS relation_kind, " +
      "relation.relhassubclass AS has_subclass, " +
      "relation.relrowsecurity AS row_security, " +
      "relation.relforcerowsecurity AS force_row_security " +
      "FROM pg_catalog.pg_namespace AS namespace " +
      "JOIN pg_catalog.pg_class AS relation " +
      "ON relation.relnamespace OPERATOR(pg_catalog.=) namespace.oid " +
      "WHERE namespace.nspname OPERATOR(pg_catalog.=) ? " +
      "AND relation.relname OPERATOR(pg_catalog.=) ? " +
      "ORDER BY relation.oid"

  private val ColumnSql =
    "SELECT " +
      "attribute.attname::pg_catalog.text AS column_name, " +
      "attribute.attnum::pg_catalog.int4 AS attribute_number, " +
      "attribute.atttypid::pg_catalog.int8 AS type_oid " +
      "FROM pg_catalog.pg_attribute AS attribute " +
      "WHERE attribute.attrelid OPERATOR(pg_catalog.=) " +
      "CAST(? AS pg_catalog.oid) " +
      "AND attribute.attnum OPERATOR(pg_catalog.>) 0 " +
      "AND attribute.attisdropped IS FALSE " +
      "ORDER BY attribute.attnum"

  private final case class DatabaseIdentity(
      name: String,
      oid: Long,
      serverVersionNumber: Int
  )

  private final case class RequestedRelation(
      qualifiedName: Vector[String],
      catalogName: Option[String],
      schemaName: String,
      relationName: String,
      columns: Vector[RequestedColumn]
  )

  private final case class RequestedColumn(
      sourceName: String,
      expectedSqlTypeName: Option[String],
      structural: Boolean
  )

  def verify(
      dataSource: DataSource,
      query: QueryIR
  ): Either[
    PostgreSqlRelevanceLimitedCountFailure,
    PostgreSqlRelevanceLimitedBaseTableEvidence
  ] = {
    if (dataSource == null)
      return Left(invalid("catalog-data-source", "catalog data source is missing"))
    if (query == null)
      return Left(invalid("catalog-query", "catalog QueryIR is missing"))

    requestedRelations(query)
      .flatMap(relations => verifyRelations(dataSource, relations))
      .flatMap { evidence =>
        validateStructuralBindings(query, evidence).map(_ => evidence)
      }
  }

  /**
    * Re-observes the complete catalog identity carried by `expected`.
    *
    * Benchmark callers invoke this boundary immediately before executing or
    * timing V4 SQL. A successful construction-time observation is not enough:
    * a table may have been dropped and recreated, changed between ordinary
    * and partitioned form, or had row security enabled in the meantime.
    */
  def revalidate(
      dataSource: DataSource,
      expected: PostgreSqlRelevanceLimitedBaseTableEvidence
  ): Either[PostgreSqlRelevanceLimitedCountFailure, Unit] = {
    if (dataSource == null)
      return Left(invalid("catalog-data-source", "catalog data source is missing"))
    if (expected == null)
      return Left(
        invalid("catalog-evidence", "expected base-table evidence is missing")
      )

    requestedRelations(expected).flatMap(verifyRelations(dataSource, _)).flatMap { observed =>
      compareExpected(expected, observed)
    }
  }

  private def requestedRelations(
      query: QueryIR
  ): Either[PostgreSqlRelevanceLimitedCountFailure, Vector[RequestedRelation]] = {
    val hypergraph = QueryHypergraphConverter.convert(query) match {
      case Left(problems) =>
        return Left(
          invalid(
            "catalog-query-conversion",
            problems.map(problem => s"${problem.code.id}:${problem.message}").mkString("; ")
          )
        )
      case Right(value) => value
    }
    val structuralColumns = hypergraph.edges.iterator
      .flatMap(_.bindings.iterator.map(_._1))
      .toSet
    val requestedByOccurrence = query.relations.map { relation =>
      parseRelationName(relation.qualifiedName, "catalog QueryIR").map {
        case (catalogName, schemaName, relationName) =>
          RequestedRelation(
            relation.qualifiedName,
            catalogName,
            schemaName,
            relationName,
            relation.columns.map(column =>
              RequestedColumn(
                column.sourceName,
                Option.when(structuralColumns(column.ref.id))(column.sqlTypeName),
                structuralColumns(column.ref.id)
              )
            )
          )
      }
    }
    requestedByOccurrence.collectFirst { case Left(problem) => problem } match {
      case Some(problem) => Left(problem)
      case None =>
        mergeRepeatedRelations(requestedByOccurrence.collect { case Right(value) => value })
    }
  }

  private def requestedRelations(
      evidence: PostgreSqlRelevanceLimitedBaseTableEvidence
  ): Either[PostgreSqlRelevanceLimitedCountFailure, Vector[RequestedRelation]] = {
    if (evidence.relations.size > MaximumDistinctRelations)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.resource(
          "catalog-relations",
          s"catalog evidence has more than $MaximumDistinctRelations distinct relations"
        )
      )
    val requested = evidence.relations.map { relation =>
      parseRelationName(relation.qualifiedName, "catalog evidence").map {
        case (catalogName, schemaName, relationName) =>
          RequestedRelation(
            relation.qualifiedName,
            catalogName,
            schemaName,
            relationName,
            relation.columns.map(column =>
              RequestedColumn(
                column.sourceName,
                if (column.structural)
                  PostgreSqlRelevanceLimitedBaseColumn.sqlTypeName(column.typeOid)
                else None,
                column.structural
              )
            )
          )
      }
    }
    requested.collectFirst { case Left(problem) => problem } match {
      case Some(problem) => Left(problem)
      case None          => Right(requested.collect { case Right(value) => value })
    }
  }

  private def parseRelationName(
      qualifiedName: Vector[String],
      sourceDescription: String
  ): Either[
    PostgreSqlRelevanceLimitedCountFailure,
    (Option[String], String, String)
  ] =
    if (qualifiedName == null || !qualifiedName.forall(validNameComponent))
      Left(
        invalid(
          "catalog-relation-name",
          s"$sourceDescription relation name contains an empty or invalid component"
        )
      )
    else
      qualifiedName match {
        case Vector(schemaName, relationName) =>
          Right((None, schemaName, relationName))
        case Vector(catalogName, schemaName, relationName) =>
          Right((Some(catalogName), schemaName, relationName))
        case _ =>
          Left(
            invalid(
              "catalog-relation-name",
              "catalog relation names must be exactly schema.table or database.schema.table"
            )
          )
      }

  private def mergeRepeatedRelations(
      requested: Vector[RequestedRelation]
  ): Either[PostgreSqlRelevanceLimitedCountFailure, Vector[RequestedRelation]] = {
    if (requested.isEmpty)
      return Left(invalid("catalog-relations", "catalog QueryIR has no relations"))
    val grouped = requested.groupBy(_.qualifiedName).toVector.sortBy {
      case (qualifiedName, _) => qualifiedName.mkString("\u0000")
    }
    if (grouped.size > MaximumDistinctRelations)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.resource(
          "catalog-relations",
          s"catalog QueryIR has more than $MaximumDistinctRelations distinct relations"
        )
      )

    val merged = Vector.newBuilder[RequestedRelation]
    grouped.foreach { case (_, occurrences) =>
      val head = occurrences.head
      val nameVectors = occurrences.map(_.columns.map(_.sourceName)).distinct
      if (nameVectors.size != 1)
        return Left(
          invalid(
            "catalog-column-shape",
            s"repeated relation ${display(head.qualifiedName)} has inconsistent QueryIR columns"
          )
        )
      if (head.columns.size > MaximumColumnsPerRelation)
        return Left(
          PostgreSqlRelevanceLimitedCountFailure.resource(
            "catalog-columns",
            s"relation ${display(head.qualifiedName)} has more than $MaximumColumnsPerRelation columns"
          )
        )

      val columns = head.columns.indices.map { ordinal =>
        val alternatives = occurrences.map(_.columns(ordinal))
        val structural = alternatives.exists(_.structural)
        val expectedTypes = alternatives.iterator.flatMap(_.expectedSqlTypeName).toSet
        if (structural && expectedTypes.size != 1)
          return Left(
            invalid(
              "catalog-structural-type",
              s"repeated relation ${display(head.qualifiedName)} column " +
                s"${head.columns(ordinal).sourceName} has inconsistent structural types"
            )
          )
        RequestedColumn(
          head.columns(ordinal).sourceName,
          expectedTypes.headOption,
          structural
        )
      }.toVector
      merged += head.copy(columns = columns)
    }
    Right(merged.result())
  }

  private def validateStructuralBindings(
      query: QueryIR,
      evidence: PostgreSqlRelevanceLimitedBaseTableEvidence
  ): Either[PostgreSqlRelevanceLimitedCountFailure, Unit] = {
    val hypergraph = QueryHypergraphConverter.convert(query) match {
      case Left(problems) =>
        return Left(
          invalid(
            "catalog-query-conversion",
            problems.map(problem => s"${problem.code.id}:${problem.message}").mkString("; ")
          )
        )
      case Right(value) => value
    }
    val relationByOccurrence = query.relations.iterator
      .map(relation => relation.id -> relation)
      .toMap
    val metadataByColumn = query.relations.iterator
      .flatMap(_.columns.iterator.map(column => column.ref.id -> column))
      .toMap
    val oidByVariable = scala.collection.mutable.Map.empty[VariableId, Vector[Long]]

    hypergraph.edges.foreach { edge =>
      val relation = relationByOccurrence.getOrElse(
        edge.relationOccurrenceId,
        return Left(invalid("catalog-query", "hypergraph occurrence is absent from QueryIR"))
      )
      edge.bindings.foreach { case (columnId, variableId) =>
        val metadata = metadataByColumn.getOrElse(
          columnId,
          return Left(invalid("catalog-query", "hypergraph column is absent from QueryIR"))
        )
        val observed = evidence.column(relation.qualifiedName, metadata.sourceName)
          .getOrElse(
            return Left(
              notApplicable(
                "catalog-structural-column",
                s"catalog evidence lacks structural column ${display(relation.qualifiedName)}.${metadata.sourceName}"
              )
            )
          )
        val actualSqlType = PostgreSqlRelevanceLimitedBaseColumn.sqlTypeName(
          observed.typeOid
        )
        if (!observed.structural || actualSqlType != Some(metadata.sqlTypeName))
          return Left(
            notApplicable(
              "catalog-structural-type",
              s"catalog type OID ${observed.typeOid} for " +
                s"${display(relation.qualifiedName)}.${metadata.sourceName} " +
                s"does not prove Calcite ${metadata.sqlTypeName} as built-in int2/int4/int8"
            )
          )
        oidByVariable.update(
          variableId,
          oidByVariable.getOrElse(variableId, Vector.empty) :+ observed.typeOid
        )
      }
    }
    oidByVariable.toVector.sortBy(_._1).collectFirst {
      case (variableId, oids) if oids.distinct.size != 1 =>
        notApplicable(
          "catalog-structural-type",
          s"variable ${variableId.value} has non-uniform live PostgreSQL type OIDs " +
            oids.distinct.sorted.mkString(",")
        )
    } match {
      case Some(problem) => Left(problem)
      case None          => Right(())
    }
  }

  private def compareExpected(
      expected: PostgreSqlRelevanceLimitedBaseTableEvidence,
      observed: PostgreSqlRelevanceLimitedBaseTableEvidence
  ): Either[PostgreSqlRelevanceLimitedCountFailure, Unit] = {
    if (observed.databaseName != expected.databaseName)
      return Left(
        stale("current database name differs from the expected catalog evidence")
      )
    if (observed.serverVersionNumber != expected.serverVersionNumber)
      return Left(
        stale("server_version_num differs from the expected catalog evidence")
      )

    val expectedRelations = expected.relations.sortBy(relationSortKey)
    val observedRelations = observed.relations.sortBy(relationSortKey)
    if (
      expectedRelations.map(_.qualifiedName) !=
      observedRelations.map(_.qualifiedName)
    )
      return Left(
        stale("qualified relation names differ from the expected catalog evidence")
      )

    expectedRelations.zip(observedRelations).collectFirst {
      case (before, now) if before.databaseOid != now.databaseOid =>
        "current database OID"
      case (before, now) if before.namespaceOid != now.namespaceOid =>
        s"namespace OID for ${display(before.qualifiedName)}"
      case (before, now) if before.relationOid != now.relationOid =>
        s"relation OID for ${display(before.qualifiedName)}"
      case (before, now) if before.relkind != now.relkind =>
        s"relation kind for ${display(before.qualifiedName)}"
      case (before, now) if before.hasSubclass != now.hasSubclass =>
        s"inheritance-child flag for ${display(before.qualifiedName)}"
      case (before, now) if before.rowSecurity != now.rowSecurity =>
        s"row-security flag for ${display(before.qualifiedName)}"
      case (before, now) if before.forceRowSecurity != now.forceRowSecurity =>
        s"forced-row-security flag for ${display(before.qualifiedName)}"
      case (before, now) if before.columns != now.columns =>
        s"column identity/type evidence for ${display(before.qualifiedName)}"
    } match {
      case Some(field) =>
        Left(stale(s"$field differs from the expected catalog evidence"))
      case None if observed.id != expected.id =>
        Left(stale("catalog evidence identity differs after exact field comparison"))
      case None => Right(())
    }
  }

  private def relationSortKey(
      relation: PostgreSqlRelevanceLimitedBaseRelation
  ): String =
    relation.qualifiedName.mkString("\u0000")

  private def verifyRelations(
      dataSource: DataSource,
      requested: Vector[RequestedRelation]
  ): Either[
    PostgreSqlRelevanceLimitedCountFailure,
    PostgreSqlRelevanceLimitedBaseTableEvidence
  ] = {
    var connection: Connection = null
    var transactionStarted = false
    var completed = false
    try {
      connection = dataSource.getConnection
      if (connection == null)
        return Left(
          rendering("catalog-connection", "catalog data source returned no connection")
        )
      if (!connection.getAutoCommit)
        return Left(
          rendering(
            "catalog-connection-state",
            "catalog verifier requires a newly acquired auto-commit connection"
          )
        )

      connection.setReadOnly(true)
      connection.setTransactionIsolation(Connection.TRANSACTION_REPEATABLE_READ)
      connection.setAutoCommit(false)
      transactionStarted = true

      val database = readDatabaseIdentity(connection) match {
        case Left(problem) => return Left(problem)
        case Right(value)  => value
      }
      val wrongCatalog = requested.collectFirst {
        case relation
            if relation.catalogName.exists(_ != database.name) =>
          relation.qualifiedName
      }
      if (wrongCatalog.nonEmpty)
        return Left(
          invalid(
            "catalog-database-name",
            "three-part QueryIR relation name does not name the current database"
          )
        )

      val observations = Vector.newBuilder[PostgreSqlRelevanceLimitedBaseRelation]
      requested.foreach { relation =>
        readRelation(connection, database, relation) match {
          case Left(problem) => return Left(problem)
          case Right(value)  => observations += value
        }
      }

      val evidence = PostgreSqlRelevanceLimitedBaseTableEvidence.create(
        database.name,
        database.serverVersionNumber,
        observations.result()
      )
      evidence match {
        case Left(_) => ()
        case Right(_) =>
          connection.commit()
          completed = true
      }
      evidence
    } catch {
      case NonFatal(problem) =>
        Left(
          rendering(
            "catalog-query",
            s"catalog verification failed (${problem.getClass.getName})"
          )
        )
    } finally {
      if (connection != null) {
        if (transactionStarted && !completed) {
          try connection.rollback()
          catch { case NonFatal(_) => () }
        }
        try connection.close()
        catch { case NonFatal(_) => () }
      }
    }
  }

  private def readDatabaseIdentity(
      connection: Connection
  ): Either[PostgreSqlRelevanceLimitedCountFailure, DatabaseIdentity] = {
    var statement: PreparedStatement = null
    var result: ResultSet = null
    try {
      statement = connection.prepareStatement(DatabaseSql)
      statement.setQueryTimeout(QueryTimeoutSeconds)
      statement.setMaxRows(2)
      result = statement.executeQuery()
      if (!result.next())
        return Left(
          rendering("catalog-database", "current database has no exact catalog row")
        )

      val name = result.getString("database_name")
      val oid = result.getLong("database_oid")
      val oidWasNull = result.wasNull()
      val version = result.getInt("server_version_num")
      val versionWasNull = result.wasNull()
      if (result.next())
        Left(
          rendering("catalog-database", "current database has multiple catalog rows")
        )
      else if (
        name == null || name.isEmpty || name.indexOf('\u0000') >= 0 ||
        oidWasNull || oid <= 0L || versionWasNull || version <= 0
      )
        Left(
          rendering("catalog-database", "current database catalog row is invalid")
        )
      else Right(DatabaseIdentity(name, oid, version))
    } catch {
      case NonFatal(problem) =>
        Left(
          rendering(
            "catalog-database",
            s"current database lookup failed (${problem.getClass.getName})"
          )
        )
    } finally {
      close(result)
      close(statement)
    }
  }

  private def readRelation(
      connection: Connection,
      database: DatabaseIdentity,
      requested: RequestedRelation
  ): Either[
    PostgreSqlRelevanceLimitedCountFailure,
    PostgreSqlRelevanceLimitedBaseRelation
  ] = {
    var statement: PreparedStatement = null
    var result: ResultSet = null
    try {
      statement = connection.prepareStatement(RelationSql)
      statement.setQueryTimeout(QueryTimeoutSeconds)
      statement.setMaxRows(2)
      statement.setString(1, requested.schemaName)
      statement.setString(2, requested.relationName)
      result = statement.executeQuery()
      if (!result.next())
        return Left(
          notApplicable(
            "catalog-relation-missing",
            s"no exact base-table catalog row exists for ${display(requested.qualifiedName)}"
          )
        )

      val namespaceName = result.getString("namespace_name")
      val namespaceOid = result.getLong("namespace_oid")
      val namespaceOidWasNull = result.wasNull()
      val relationName = result.getString("relation_name")
      val relationOid = result.getLong("relation_oid")
      val relationOidWasNull = result.wasNull()
      val relkind = result.getString("relation_kind")
      val hasSubclass = result.getBoolean("has_subclass")
      val hasSubclassWasNull = result.wasNull()
      val rowSecurity = result.getBoolean("row_security")
      val rowSecurityWasNull = result.wasNull()
      val forceRowSecurity = result.getBoolean("force_row_security")
      val forceRowSecurityWasNull = result.wasNull()
      if (result.next())
        return Left(
          rendering(
            "catalog-relation-ambiguous",
            s"multiple exact catalog rows exist for ${display(requested.qualifiedName)}"
          )
        )
      if (
        namespaceName != requested.schemaName ||
        relationName != requested.relationName ||
        namespaceOidWasNull || namespaceOid <= 0L ||
        relationOidWasNull || relationOid <= 0L ||
        relkind == null || relkind.length != 1 ||
        hasSubclassWasNull || rowSecurityWasNull || forceRowSecurityWasNull
      )
        return Left(
          rendering(
            "catalog-relation-row",
            s"catalog row is invalid for ${display(requested.qualifiedName)}"
          )
        )
      if (relkind != "r")
        return Left(
          notApplicable(
            "catalog-relation-kind",
            s"relation ${display(requested.qualifiedName)} is not an ordinary base table"
          )
        )
      if (hasSubclass)
        return Left(
          notApplicable(
            "catalog-inheritance",
            s"relation ${display(requested.qualifiedName)} has inheritance or partition children"
          )
        )
      if (rowSecurity || forceRowSecurity)
        return Left(
          notApplicable(
            "catalog-row-security",
            s"relation ${display(requested.qualifiedName)} enables row security"
          )
        )

      close(result)
      result = null
      close(statement)
      statement = null
      val columns = readColumns(
        connection,
        requested,
        relationOid
      ) match {
        case Left(problem) => return Left(problem)
        case Right(value)  => value
      }

      Right(
        PostgreSqlRelevanceLimitedBaseRelation(
          requested.qualifiedName,
          database.oid,
          namespaceOid,
          relationOid,
          relkind,
          hasSubclass,
          rowSecurity,
          forceRowSecurity,
          columns
        )
      )
    } catch {
      case NonFatal(problem) =>
        Left(
          rendering(
            "catalog-relation-query",
            s"catalog relation lookup failed (${problem.getClass.getName})"
          )
        )
    } finally {
      close(result)
      close(statement)
    }
  }

  private def readColumns(
      connection: Connection,
      requested: RequestedRelation,
      relationOid: Long
  ): Either[
    PostgreSqlRelevanceLimitedCountFailure,
    Vector[PostgreSqlRelevanceLimitedBaseColumn]
  ] = {
    var statement: PreparedStatement = null
    var result: ResultSet = null
    try {
      statement = connection.prepareStatement(ColumnSql)
      statement.setQueryTimeout(QueryTimeoutSeconds)
      statement.setMaxRows(MaximumColumnsPerRelation + 1)
      statement.setLong(1, relationOid)
      result = statement.executeQuery()
      val rows = Vector.newBuilder[(String, Int, Long)]
      while (result.next()) {
        val name = result.getString("column_name")
        val attributeNumber = result.getInt("attribute_number")
        val attributeNumberWasNull = result.wasNull()
        val typeOid = result.getLong("type_oid")
        val typeOidWasNull = result.wasNull()
        if (
          name == null || name.isEmpty || name.indexOf('\u0000') >= 0 ||
          attributeNumberWasNull || attributeNumber <= 0 ||
          typeOidWasNull || typeOid <= 0L
        )
          return Left(
            rendering(
              "catalog-column-row",
              s"catalog column row is invalid for ${display(requested.qualifiedName)}"
            )
          )
        rows += ((name, attributeNumber, typeOid))
      }
      val exactRows = rows.result()
      if (exactRows.size > MaximumColumnsPerRelation)
        return Left(
          PostgreSqlRelevanceLimitedCountFailure.resource(
            "catalog-columns",
            s"relation ${display(requested.qualifiedName)} has more than $MaximumColumnsPerRelation columns"
          )
        )
      if (exactRows.map(_._1) != requested.columns.map(_.sourceName))
        return Left(
          notApplicable(
            "catalog-column-shape",
            s"live non-dropped user columns differ from QueryIR for ${display(requested.qualifiedName)}"
          )
        )

      val columns = exactRows.zip(requested.columns).map {
        case ((sourceName, attributeNumber, typeOid), expected) =>
          if (expected.structural) {
            val actualSqlType = PostgreSqlRelevanceLimitedBaseColumn.sqlTypeName(typeOid)
            if (actualSqlType != expected.expectedSqlTypeName)
              return Left(
                notApplicable(
                  "catalog-structural-type",
                  s"catalog type OID $typeOid for ${display(requested.qualifiedName)}.$sourceName " +
                    s"does not prove Calcite ${expected.expectedSqlTypeName.getOrElse("<missing>")} " +
                    "as built-in int2/int4/int8"
                )
              )
          }
          PostgreSqlRelevanceLimitedBaseColumn(
            sourceName,
            attributeNumber,
            typeOid,
            expected.structural
          )
      }
      Right(columns)
    } catch {
      case NonFatal(problem) =>
        Left(
          rendering(
            "catalog-column-query",
            s"catalog column lookup failed (${problem.getClass.getName})"
          )
        )
    } finally {
      close(result)
      close(statement)
    }
  }

  private def validNameComponent(value: String): Boolean =
    value != null && value.nonEmpty && value.indexOf('\u0000') < 0

  private def display(qualifiedName: Vector[String]): String =
    qualifiedName
      .map(value => "\"" + value.replace("\"", "\"\"") + "\"")
      .mkString(".")

  private def invalid(
      suffix: String,
      detail: String
  ): PostgreSqlRelevanceLimitedCountFailure =
    PostgreSqlRelevanceLimitedCountFailure.invalid(suffix, detail)

  private def notApplicable(
      suffix: String,
      detail: String
  ): PostgreSqlRelevanceLimitedCountFailure =
    PostgreSqlRelevanceLimitedCountFailure.notApplicable(suffix, detail)

  private def stale(detail: String): PostgreSqlRelevanceLimitedCountFailure =
    notApplicable("catalog-evidence-drift", detail)

  private def rendering(
      suffix: String,
      detail: String
  ): PostgreSqlRelevanceLimitedCountFailure =
    PostgreSqlRelevanceLimitedCountFailure.rendering(suffix, detail)

  private def close(closeable: AutoCloseable): Unit =
    if (closeable != null) {
      try closeable.close()
      catch { case NonFatal(_) => () }
    }
}
