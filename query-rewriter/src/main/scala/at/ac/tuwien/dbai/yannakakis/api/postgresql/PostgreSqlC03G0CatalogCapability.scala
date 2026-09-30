package at.ac.tuwien.dbai.yannakakis.api.postgresql

import javax.sql.DataSource

import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  C03G0CatalogBinding,
  CalciteSqlPlanningError,
  CalciteSqlPlanningStage
}

import scala.collection.mutable
import scala.util.Using
import scala.util.control.NonFatal

/**
  * PostgreSQL-catalog admission for the exact C03-G0 v2 capability.
  *
  * Calcite exposes `TEXT` as `VARCHAR`, but its column metadata does not bind
  * PostgreSQL collation determinism. This read-only repeatable-read receipt is
  * therefore mandatory before the opt-in planner can mint C03-G0 source
  * evidence. The legacy planner never calls this boundary.
  */
private[postgresql] object PostgreSqlC03G0CatalogCapability {
  private val RelationName = "synthetic_triangle_e0"
  private val GroupColumnName = "payload"
  private val MeasureColumnName = "row_id"
  private val RequiredServerMajorVersion = 18

  private val CatalogSql =
    "SELECT " +
      "database_catalog.datname AS database_name, " +
      "database_catalog.oid::pg_catalog.int8 AS database_oid, " +
      "namespace.oid::pg_catalog.int8 AS namespace_oid, " +
      "relation.oid::pg_catalog.int8 AS relation_oid, " +
      "pg_catalog.current_setting('server_version_num')::pg_catalog.int4 AS server_version_num, " +
      "attribute.attname AS column_name, " +
      "attribute.attnum::pg_catalog.int4 AS attribute_number, " +
      "attribute.atttypid::pg_catalog.int8 AS type_oid, " +
      "pg_catalog.format_type(attribute.atttypid, attribute.atttypmod) AS formatted_type, " +
      "attribute.attnotnull AS not_null, " +
      "attribute.attcollation::pg_catalog.int8 AS collation_oid, " +
      "collation_catalog.collname AS collation_name, " +
      "collation_catalog.collprovider::pg_catalog.text AS collation_provider, " +
      "collation_catalog.collisdeterministic AS collation_deterministic " +
      "FROM pg_catalog.pg_namespace AS namespace " +
      "JOIN pg_catalog.pg_database AS database_catalog " +
      "ON database_catalog.datname = pg_catalog.current_database() " +
      "JOIN pg_catalog.pg_class AS relation " +
      "ON relation.relnamespace = namespace.oid " +
      "JOIN pg_catalog.pg_attribute AS attribute " +
      "ON attribute.attrelid = relation.oid " +
      "LEFT JOIN pg_catalog.pg_collation AS collation_catalog " +
      "ON collation_catalog.oid = attribute.attcollation " +
      "WHERE namespace.nspname = ? " +
      "AND relation.relname = ? " +
      "AND relation.relkind IN ('r', 'p') " +
      "AND attribute.attnum > 0 " +
      "AND NOT attribute.attisdropped " +
      "AND attribute.attname IN (?, ?) " +
      "ORDER BY attribute.attname"

  private final case class CatalogColumn(
      databaseName: String,
      databaseOid: Long,
      namespaceOid: Long,
      relationOid: Long,
      serverVersionNumber: Int,
      name: String,
      attributeNumber: Int,
      typeOid: Long,
      formattedType: String,
      notNull: Boolean,
      collationOid: Long,
      collationName: Option[String],
      collationProvider: Option[String],
      collationDeterministic: Option[Boolean]
  )

  def verify(
      dataSource: DataSource,
      physicalSchemaName: String
  ): Either[CalciteSqlPlanningError, C03G0CatalogBinding] = {
    if (dataSource == null || physicalSchemaName == null ||
        physicalSchemaName.trim.isEmpty) {
      return Left(configurationFailure(None))
    }

    try {
      Using.Manager { use =>
        val connection = use(dataSource.getConnection)
        connection.setReadOnly(true)
        connection.setTransactionIsolation(
          java.sql.Connection.TRANSACTION_REPEATABLE_READ
        )
        connection.setAutoCommit(false)

        val statement = use(connection.prepareStatement(CatalogSql))
        statement.setString(1, physicalSchemaName)
        statement.setString(2, RelationName)
        statement.setString(3, GroupColumnName)
        statement.setString(4, MeasureColumnName)

        val result = use(statement.executeQuery())
        val columns = mutable.ArrayBuffer.empty[CatalogColumn]
        while (result.next()) {
          val collationName = Option(result.getString("collation_name"))
          val collationProvider = Option(
            result.getString("collation_provider")
          )
          val deterministicValue = result.getBoolean(
            "collation_deterministic"
          )
          val collationDeterministic =
            if (result.wasNull()) None else Some(deterministicValue)
          columns += CatalogColumn(
            databaseName = result.getString("database_name"),
            databaseOid = result.getLong("database_oid"),
            namespaceOid = result.getLong("namespace_oid"),
            relationOid = result.getLong("relation_oid"),
            serverVersionNumber = result.getInt("server_version_num"),
            name = result.getString("column_name"),
            attributeNumber = result.getInt("attribute_number"),
            typeOid = result.getLong("type_oid"),
            formattedType = result.getString("formatted_type"),
            notNull = result.getBoolean("not_null"),
            collationOid = result.getLong("collation_oid"),
            collationName = collationName,
            collationProvider = collationProvider,
            collationDeterministic = collationDeterministic
          )
        }
        connection.commit()
        certify(physicalSchemaName, columns.toVector)
      }.fold(
        problem => Left(configurationFailure(Some(problem.getClass.getName))),
        identity
      )
    } catch {
      case NonFatal(problem) =>
        Left(configurationFailure(Some(problem.getClass.getName)))
    }
  }

  private def certify(
      physicalSchemaName: String,
      columns: Vector[CatalogColumn]
  ): Either[CalciteSqlPlanningError, C03G0CatalogBinding] = {
    val byName = columns.groupBy(_.name)
    val group = byName.get(GroupColumnName).collect { case Vector(value) => value }
    val measure = byName.get(MeasureColumnName).collect {
      case Vector(value) => value
    }
    val commonIdentity = for {
      groupColumn <- group
      measureColumn <- measure
      if columns.size == 2
      if groupColumn.databaseName != null && groupColumn.databaseName.nonEmpty
      if groupColumn.databaseOid > 0
      if groupColumn.namespaceOid > 0
      if groupColumn.relationOid > 0
      if groupColumn.namespaceOid == measureColumn.namespaceOid
      if groupColumn.relationOid == measureColumn.relationOid
      if groupColumn.databaseName == measureColumn.databaseName
      if groupColumn.databaseOid == measureColumn.databaseOid
      if groupColumn.serverVersionNumber == measureColumn.serverVersionNumber
      if groupColumn.serverVersionNumber / 10000 == RequiredServerMajorVersion
    } yield (groupColumn, measureColumn)

    commonIdentity match {
      case Some((groupColumn, measureColumn))
          if groupColumn.formattedType == "text" &&
            groupColumn.attributeNumber == 4 &&
            groupColumn.typeOid == 25 &&
            !groupColumn.notNull &&
            groupColumn.collationOid > 0 &&
            groupColumn.collationName.exists(_.nonEmpty) &&
            groupColumn.collationProvider.exists(_.nonEmpty) &&
            groupColumn.collationDeterministic.contains(true) &&
            measureColumn.formattedType == "bigint" &&
            measureColumn.attributeNumber == 1 &&
            measureColumn.typeOid == 20 &&
            measureColumn.notNull &&
            measureColumn.collationOid == 0 &&
            measureColumn.collationName.isEmpty &&
            measureColumn.collationProvider.isEmpty &&
            measureColumn.collationDeterministic.isEmpty =>
        Right(
          C03G0CatalogBinding(
            physicalSchemaName = physicalSchemaName,
            databaseName = groupColumn.databaseName,
            databaseOid = groupColumn.databaseOid,
            relationName = RelationName,
            groupColumnName = GroupColumnName,
            measureColumnName = MeasureColumnName,
            serverMajorVersion = RequiredServerMajorVersion,
            namespaceOid = groupColumn.namespaceOid,
            relationOid = groupColumn.relationOid,
            groupAttnum = groupColumn.attributeNumber,
            groupTypeOid = groupColumn.typeOid,
            measureAttnum = measureColumn.attributeNumber,
            measureTypeOid = measureColumn.typeOid,
            groupCollationOid = groupColumn.collationOid,
            groupCollationName = groupColumn.collationName.get,
            groupCollationProvider = groupColumn.collationProvider.get,
            groupCollationDeterministic = true
          )
        )
      case _ => Left(configurationFailure(None))
    }
  }

  private def configurationFailure(
      exceptionClass: Option[String]
  ): CalciteSqlPlanningError =
    CalciteSqlPlanningError(
      stage = CalciteSqlPlanningStage.Configuration,
      message =
        "PostgreSQL C03-G0 catalog capability could not be certified",
      exceptionClass = exceptionClass
    )
}
