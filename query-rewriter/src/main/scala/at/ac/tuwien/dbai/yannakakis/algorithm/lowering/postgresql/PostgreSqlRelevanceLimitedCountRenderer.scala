package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  BagFactorPlanningLimits
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.{
  ColumnId,
  EdgeId,
  Hyperedge,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  ColumnMetadata,
  QueryIR,
  RelationOccurrence
}

import scala.collection.mutable

/**
  * Additive whole-query renderer for one relevance-limited shared frequency.
  *
  * The normal renderer is first used as a bounded semantic/identifier/type
  * validation boundary.  The specialized SQL then evaluates one representative
  * pendant wing over the set of keys visible in its anchor occurrence, stores
  * exact NUMERIC frequencies in one MATERIALIZED CTE, and reads that CTE once
  * for every isomorphic logical wing.  It is package-private, opt-in, and does
  * not alter default rewrite SQL or any frozen renderer profile.
  */
private[yannakakis] object PostgreSqlRelevanceLimitedCountRenderer {
  private val SharedCteBase = "yr_v4_shared_wing"
  private val DemandAlias = "yr_v4_demand"
  private val KeyColumn = "yr_key"
  private val FrequencyColumn = "yr_freq"

  private final case class SqlColumn(alias: String, sourceName: String)

  def render(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      baseTableEvidence: PostgreSqlRelevanceLimitedBaseTableEvidence,
      renderingLimits: PostgreSqlRenderingLimits =
        PostgreSqlRenderingLimits.default,
      planningLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default,
      cseLimits: PostgreSqlRelevanceLimitedCse.SearchLimits =
        PostgreSqlRelevanceLimitedCse.SearchLimits.default
  ): Either[
    PostgreSqlRelevanceLimitedCountFailure,
    PostgreSqlRelevanceLimitedCountRenderedQuery
  ] = {
    if (query == null || input == null || plan == null)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "input",
          "query, candidate input, and provenance plan are required"
        )
      )
    if (baseTableEvidence == null)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "base-table-evidence",
          "base-table catalog evidence is required"
        )
      )
    if (renderingLimits == null || planningLimits == null || cseLimits == null)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "limits",
          "rendering, planning, and CSE limits are required"
        )
      )

    // Compile through the established typed PostgreSQL boundary, but do not
    // allocate and immediately discard its multi-CTE SQL. V4 has its own exact
    // resource accounting below; the fixed default is the internal compiler
    // ceiling and the named statistics-safe profile is part of V4's identity.
    val validated = PostgreSqlBagFactorRenderer
      .compileValidatedWithEnumerationProfile(
        query,
        input,
        plan,
        PostgreSqlRenderingLimits.default,
        planningLimits,
        YannakakisReductionMode.NoReduction,
        PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
      )
      .fold(
        problems =>
          return Left(
            PostgreSqlRelevanceLimitedCountFailure.rendering(
              "validation",
              problems.map(problem => s"${problem.code}:${problem.message}").mkString("; ")
            )
          ),
        identity
      )

    val csePlan = PostgreSqlRelevanceLimitedCse
      .derive(query, input, cseLimits)
      .fold(
        problems => return Left(detectorFailure(problems)),
        identity
      )

    val edgeById = input.hypergraph.edges.iterator.map(edge => edge.id -> edge).toMap
    val relationByOccurrence = query.relations.iterator
      .map(relation => relation.id -> relation)
      .toMap
    val metadataByColumn = query.relations.iterator
      .flatMap(_.columns.iterator.map(column => column.ref.id -> column))
      .toMap
    val evidenceEdgeIds = input.hypergraph.edges.map(_.id)
    val missingBaseTables = evidenceEdgeIds.iterator
      .map(edgeById)
      .map(edge => relationByOccurrence(edge.relationOccurrenceId).qualifiedName)
      .toVector
      .distinct
      .filterNot(baseTableEvidence.contains)
    if (missingBaseTables.nonEmpty)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.notApplicable(
          "base-table-evidence",
          "V4 requires ordinary-base-table, no-inheritance, row-security-free evidence for " +
            missingBaseTables.map(_.mkString(".")).mkString(",")
        )
      )
    validateStructuralEvidence(
      input,
      relationByOccurrence,
      metadataByColumn,
      baseTableEvidence
    ).foreach(problem => return Left(problem))

    val representativeEdges = csePlan.sharedWing.representative.edgeIds.map(edgeById)
    val coreEdges = csePlan.coreEdgeIds.map(edgeById)
    val sharedPredicates = equalityPredicates(
      representativeEdges,
      representativeEdges.iterator.map(edge => edge.id -> wingAlias(edge.id)).toMap,
      metadataByColumn,
      Vector(
        csePlan.sharedWing.representative.interfaceVariable ->
          SqlColumn(DemandAlias, KeyColumn)
      )
    )
    val terminalSynthetic = csePlan.anchorInterfaces.zipWithIndex.map {
      case (interface, ordinal) =>
        interface.variableId -> SqlColumn(memberAlias(ordinal), KeyColumn)
    }
    val terminalPredicates = equalityPredicates(
      coreEdges,
      coreEdges.iterator.map(edge => edge.id -> coreAlias(edge.id)).toMap,
      metadataByColumn,
      terminalSynthetic
    )
    val sharedCte = freshSharedCteName(query)

    // Match PostgreSqlRenderingStatistics: count expanded source-relation
    // scans, not references to renderer-owned CTEs in the terminal.
    val expandedScans = BigInt(csePlan.anchorInterfaces.size) +
      representativeEdges.size + coreEdges.size
    val renderedTerms = BigInt(csePlan.anchorInterfaces.size) + 4 +
      sharedPredicates.size + terminalPredicates.size
    resourceCheck(
      "expanded-scans",
      expandedScans,
      renderingLimits.maxExpandedScans
    ).foreach(problem => return Left(problem))
    resourceCheck(
      "rendered-terms",
      renderedTerms,
      renderingLimits.maxRenderedTerms
    ).foreach(problem => return Left(problem))
    resourceCheck("ctes", BigInt(1), renderingLimits.maxCtes)
      .foreach(problem => return Left(problem))

    val sql = emit(
      csePlan,
      edgeById,
      relationByOccurrence,
      metadataByColumn,
      sharedPredicates,
      terminalPredicates,
      validated.output.head.name,
      sharedCte
    )
    resourceCheck(
      "sql-characters",
      BigInt(sql.length),
      renderingLimits.maxSqlCharacters
    ).foreach(problem => return Left(problem))

    val isomorphismByInterface = csePlan.sharedWing.isomorphisms.iterator
      .map(value => value.memberInterfaceVariable -> value)
      .toMap
    val interfaceByVariable = csePlan.anchorInterfaces.iterator
      .map(value => value.variableId -> value)
      .toMap
    val members = csePlan.sharedWing.members.map { wing =>
      val isomorphism = isomorphismByInterface(wing.interfaceVariable)
      PostgreSqlRelevanceLimitedCountMemberTrace(
        wing.interfaceVariable,
        interfaceByVariable(wing.interfaceVariable).anchorColumnId,
        wing.edgeIds,
        isomorphism.edgeMapping,
        isomorphism.variableMapping
      )
    }
    val anchor = edgeById(csePlan.anchorEdgeId)
    // This is gross logical-wing reuse, not necessarily a net scan saving:
    // constructing the relevance domain scans the anchor once per member.
    val eliminatedWingScans =
      (members.size - 1) * representativeEdges.size
    val anchorDemandScans = csePlan.anchorInterfaces.size
    val originalSourceScans = query.relations.size
    val netSourceScanDelta = expandedScans.toInt - originalSourceScans
    val trace = PostgreSqlRelevanceLimitedCountTrace(
      PostgreSqlRelevanceLimitedCountProfile.DetectorScheme,
      anchor.id,
      anchor.relationOccurrenceId,
      csePlan.sharedWing.representative.interfaceVariable,
      csePlan.sharedWing.representative.edgeIds,
      members,
      csePlan.coreEdgeIds,
      sharedCte,
      baseTableEvidence.id,
      physicalCteCount = 1,
      eliminatedWingScanCount = eliminatedWingScans,
      anchorDemandScanCount = anchorDemandScans,
      originalSourceScanCount = originalSourceScans,
      netSourceScanDelta = netSourceScanDelta
    )
    val statistics = PostgreSqlRelevanceLimitedCountStatistics(
      expandedScans.toLong,
      renderedTerms.toLong,
      cteCount = 1L,
      sqlCharacterCount = sql.length.toLong
    )
    Right(
      PostgreSqlRelevanceLimitedCountRenderedQuery(
        query,
        input,
        plan,
        sql,
        validated.output,
        trace,
        statistics
      )
    )
  }

  private def emit(
      plan: PostgreSqlRelevanceLimitedCse.Plan,
      edgeById: Map[EdgeId, Hyperedge],
      relationByOccurrence: Map[
        at.ac.tuwien.dbai.yannakakis.core.RelationOccurrenceId,
        RelationOccurrence
      ],
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      sharedPredicates: Vector[String],
      terminalPredicates: Vector[String],
      outputName: String,
      sharedCte: String
  ): String = {
    val anchor = edgeById(plan.anchorEdgeId)
    val anchorRelation = relationByOccurrence(anchor.relationOccurrenceId)
    val representativeEdges = plan.sharedWing.representative.edgeIds.map(edgeById)
    val coreEdges = plan.coreEdgeIds.map(edgeById)
    val sink = new StringBuilder

    sink.append("WITH ")
    sink.append(quoted(sharedCte))
    sink.append(" AS MATERIALIZED (\n")
    sink.append("  SELECT\n")
    sink.append("    ")
    sink.append(qualified(DemandAlias, KeyColumn))
    sink.append(" AS ")
    sink.append(quoted(KeyColumn))
    sink.append(",\n")
    sink.append("    SUM(CAST(1 AS NUMERIC)) AS ")
    sink.append(quoted(FrequencyColumn))
    sink.append("\n")
    sink.append("  FROM (\n")
    plan.anchorInterfaces.zipWithIndex.foreach { case (interface, ordinal) =>
      if (ordinal > 0) sink.append("    UNION\n")
      val alias = anchorAlias(ordinal)
      sink.append("    SELECT ")
      sink.append(
        qualified(alias, metadataByColumn(interface.anchorColumnId).sourceName)
      )
      sink.append(" AS ")
      sink.append(quoted(KeyColumn))
      sink.append("\n")
      sink.append("    FROM ")
      sink.append(qualifiedName(anchorRelation.qualifiedName))
      sink.append(" AS ")
      sink.append(quoted(alias))
      sink.append("\n")
    }
    sink.append("  ) AS ")
    sink.append(quoted(DemandAlias))
    sink.append("\n")
    representativeEdges.foreach { edge =>
      sink.append("  CROSS JOIN ")
      sink.append(qualifiedName(relationByOccurrence(edge.relationOccurrenceId).qualifiedName))
      sink.append(" AS ")
      sink.append(quoted(wingAlias(edge.id)))
      sink.append("\n")
    }
    emitWhere(sharedPredicates, "  ", sink)
    sink.append("  GROUP BY ")
    sink.append(qualified(DemandAlias, KeyColumn))
    sink.append("\n")
    sink.append(")\n")
    sink.append("SELECT\n")
    sink.append("  CAST(\n")
    sink.append("    COALESCE(\n")
    sink.append("      SUM(")
    plan.anchorInterfaces.indices.foreach { ordinal =>
      if (ordinal > 0) sink.append(" * ")
      sink.append(qualified(memberAlias(ordinal), FrequencyColumn))
    }
    sink.append("),\n")
    sink.append("      CAST(0 AS NUMERIC)\n")
    sink.append("    ) AS BIGINT\n")
    sink.append("  ) AS ")
    sink.append(quoted(outputName))
    sink.append("\n")
    sink.append("FROM ")
    val firstCore = coreEdges.head
    sink.append(qualifiedName(relationByOccurrence(firstCore.relationOccurrenceId).qualifiedName))
    sink.append(" AS ")
    sink.append(quoted(coreAlias(firstCore.id)))
    sink.append("\n")
    coreEdges.tail.foreach { edge =>
      sink.append("CROSS JOIN ")
      sink.append(qualifiedName(relationByOccurrence(edge.relationOccurrenceId).qualifiedName))
      sink.append(" AS ")
      sink.append(quoted(coreAlias(edge.id)))
      sink.append("\n")
    }
    plan.anchorInterfaces.indices.foreach { ordinal =>
      sink.append("CROSS JOIN ")
      sink.append(quoted(sharedCte))
      sink.append(" AS ")
      sink.append(quoted(memberAlias(ordinal)))
      sink.append("\n")
    }
    emitWhere(terminalPredicates, "", sink)
    sink.append(";\n")
    sink.result()
  }

  /**
    * A chain per structural variable is sufficient: under ordinary equality,
    * it is equivalent to every source equality in the same equivalence class,
    * including NULL rejection. The compiler and exact catalog-evidence checks
    * above jointly prove uniform built-in integral types.
    */
  private def equalityPredicates(
      edges: Vector[Hyperedge],
      aliases: Map[EdgeId, String],
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      synthetic: Vector[(VariableId, SqlColumn)]
  ): Vector[String] = {
    val byVariable = mutable.Map.empty[VariableId, Vector[SqlColumn]]
    synthetic.foreach { case (variableId, column) =>
      byVariable.update(
        variableId,
        byVariable.getOrElse(variableId, Vector.empty) :+ column
      )
    }
    edges.sortBy(_.id).foreach { edge =>
      edge.bindings.foreach { case (columnId, variableId) =>
        val column = SqlColumn(
          aliases(edge.id),
          metadataByColumn(columnId).sourceName
        )
        byVariable.update(
          variableId,
          byVariable.getOrElse(variableId, Vector.empty) :+ column
        )
      }
    }
    byVariable.toVector.sortBy(_._1).flatMap { case (_, columns) =>
      columns.sliding(2).collect {
        case Vector(left, right) =>
          s"${qualified(left.alias, left.sourceName)} OPERATOR(pg_catalog.=) " +
            qualified(right.alias, right.sourceName)
      }.toVector
    }
  }

  private def emitWhere(
      predicates: Vector[String],
      indent: String,
      sink: StringBuilder
  ): Unit = {
    require(predicates.nonEmpty, "V4 equality program must have a predicate")
    sink.append(indent)
    sink.append("WHERE\n")
    predicates.zipWithIndex.foreach { case (predicate, ordinal) =>
      sink.append(indent)
      sink.append(if (ordinal == 0) "  " else "  AND ")
      sink.append(predicate)
      sink.append("\n")
    }
  }

  private def detectorFailure(
      problems: Vector[PostgreSqlRelevanceLimitedCse.Error]
  ): PostgreSqlRelevanceLimitedCountFailure = {
    val first = problems.headOption.getOrElse(
      PostgreSqlRelevanceLimitedCse.Error(
        PostgreSqlRelevanceLimitedCse.ErrorCode.CandidateMismatch,
        "detector returned no diagnostic"
      )
    )
    val detail = problems.map(_.message).mkString("; ")
    first.code match {
      case PostgreSqlRelevanceLimitedCse.ErrorCode.NoReusableWing |
          PostgreSqlRelevanceLimitedCse.ErrorCode.UnsupportedResultShape |
          PostgreSqlRelevanceLimitedCse.ErrorCode.UnsupportedPredicate |
          PostgreSqlRelevanceLimitedCse.ErrorCode.UnsupportedKeyType =>
        PostgreSqlRelevanceLimitedCountFailure.notApplicable(
          first.code.value,
          detail
        )
      case PostgreSqlRelevanceLimitedCse.ErrorCode.ResourceLimitExceeded =>
        PostgreSqlRelevanceLimitedCountFailure.resource(
          "cse-search",
          detail
        )
      case _ =>
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "cse-input",
          detail
        )
    }
  }

  private def validateStructuralEvidence(
      input: CandidateSearchInput,
      relationByOccurrence: Map[
        at.ac.tuwien.dbai.yannakakis.core.RelationOccurrenceId,
        RelationOccurrence
      ],
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      evidence: PostgreSqlRelevanceLimitedBaseTableEvidence
  ): Option[PostgreSqlRelevanceLimitedCountFailure] = {
    val typeOidsByVariable = mutable.Map.empty[VariableId, Vector[Long]]
    input.hypergraph.edges.foreach { edge =>
      val relation = relationByOccurrence(edge.relationOccurrenceId)
      edge.bindings.foreach { case (columnId, variableId) =>
        val metadata = metadataByColumn(columnId)
        val observed = evidence.column(relation.qualifiedName, metadata.sourceName)
          .getOrElse(
            return Some(
              PostgreSqlRelevanceLimitedCountFailure.notApplicable(
                "catalog-structural-column",
                s"catalog evidence lacks structural column " +
                  s"${relation.qualifiedName.mkString(".")}.${metadata.sourceName}"
              )
            )
          )
        val actualSqlType =
          PostgreSqlRelevanceLimitedBaseColumn.sqlTypeName(observed.typeOid)
        if (!observed.structural || actualSqlType != Some(metadata.sqlTypeName))
          return Some(
            PostgreSqlRelevanceLimitedCountFailure.notApplicable(
              "catalog-structural-type",
              s"catalog type OID ${observed.typeOid} for " +
                s"${relation.qualifiedName.mkString(".")}.${metadata.sourceName} " +
                s"does not prove Calcite ${metadata.sqlTypeName} as built-in int2/int4/int8"
            )
          )
        typeOidsByVariable.update(
          variableId,
          typeOidsByVariable.getOrElse(variableId, Vector.empty) :+ observed.typeOid
        )
      }
    }
    typeOidsByVariable.toVector.sortBy(_._1).collectFirst {
      case (variableId, typeOids) if typeOids.distinct.size != 1 =>
        PostgreSqlRelevanceLimitedCountFailure.notApplicable(
          "catalog-structural-type",
          s"variable ${variableId.value} has non-uniform live PostgreSQL type OIDs " +
            typeOids.distinct.sorted.mkString(",")
        )
    }
  }

  private def resourceCheck(
      suffix: String,
      actual: BigInt,
      limit: Long
  ): Option[PostgreSqlRelevanceLimitedCountFailure] =
    Option.when(actual > BigInt(limit))(
      PostgreSqlRelevanceLimitedCountFailure.resource(
        suffix,
        s"$suffix actual=$actual limit=$limit"
      )
    )

  private def anchorAlias(ordinal: Int): String = s"yr_v4_anchor_$ordinal"
  private def wingAlias(edgeId: EdgeId): String = s"yr_v4_w_e${edgeId.value}"
  private def coreAlias(edgeId: EdgeId): String = s"yr_v4_c_e${edgeId.value}"
  private def memberAlias(ordinal: Int): String = s"yr_v4_m$ordinal"

  private def freshSharedCteName(query: QueryIR): String = {
    val occupied = query.relations.iterator.collect {
      case relation if relation.qualifiedName.size == 1 =>
        relation.qualifiedName.head
    }.toSet
    if (!occupied(SharedCteBase)) SharedCteBase
    else {
      var suffix = 1
      var candidate = s"${SharedCteBase}_$suffix"
      while (occupied(candidate)) {
        suffix += 1
        candidate = s"${SharedCteBase}_$suffix"
      }
      candidate
    }
  }

  private def qualifiedName(components: Vector[String]): String =
    components.iterator.map(quoted).mkString(".")

  private def qualified(alias: String, column: String): String =
    s"${quoted(alias)}.${quoted(column)}"

  private def quoted(value: String): String =
    "\"" + value.replace("\"", "\"\"") + "\""
}
