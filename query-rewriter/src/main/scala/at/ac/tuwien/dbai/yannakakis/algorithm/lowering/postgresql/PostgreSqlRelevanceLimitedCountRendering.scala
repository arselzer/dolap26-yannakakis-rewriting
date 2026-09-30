package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlan
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.core.{
  ColumnId,
  EdgeId,
  RelationOccurrenceId,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryIR

/**
  * Exact identity of the additive query-level scalar-COUNT realization.
  *
  * This is intentionally separate from renderer V3.  The optimization finds
  * isomorphic pendant query wings before bag ownership and therefore is not a
  * different physical spelling of the V3 bag-message program.
  */
private[yannakakis] object PostgreSqlRelevanceLimitedCountProfile {
  val Scheme: String = "postgresql_relevance_limited_count_profile_v1"
  val RealizationId: String = "relevance-limited-shared-pendant-v1"
  val DetectorScheme: String = "postgresql_relevance_limited_cse_v1"
  val RelevanceDomainScheme: String =
    "distinct-anchor-interface-union-v1"
  val FrequencyCarrierScheme: String = "numeric-unit-sum-v1"
  val FinalizationScheme: String =
    "numeric-frequency-product-sum-bigint-check-v1"
  val GeneratedNameScheme: String =
    "fresh-unqualified-source-name-suffix-v1"
  val ProvenanceScheme: String = "canonical-structural-head-v1"
  val RelationPurityScheme: String =
    "ordinary-base-table-no-inheritance-no-rls-v1"
  val StructuralTypeEvidenceScheme: String =
    "pg-attribute-built-in-int2-int4-int8-v1"
  val StructuralEqualityScheme: String =
    "pg-catalog-built-in-integral-equality-chain-v1"
  val EnumerationProfileScheme: String =
    "postgresql_enumeration_rendering_profile_v1"
  val EnumerationProfileId: String =
    PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2.id
  val ReductionMode: YannakakisReductionMode =
    YannakakisReductionMode.NoReduction
  val CteMaterialization: PostgreSqlCteMaterializationDirective =
    PostgreSqlCteMaterializationDirective.Materialized
  val CteMaterializationScope: String = "shared-pendant-cte-only-v1"

  val AxisIds: Vector[String] = Vector(
    RealizationId,
    DetectorScheme,
    RelevanceDomainScheme,
    FrequencyCarrierScheme,
    FinalizationScheme,
    GeneratedNameScheme,
    ProvenanceScheme,
    RelationPurityScheme,
    StructuralTypeEvidenceScheme,
    StructuralEqualityScheme,
    CteMaterialization.id
  )

  val Id: String = "sha256:" + sha256(
    Vector(
      Scheme,
      RealizationId,
      DetectorScheme,
      RelevanceDomainScheme,
      FrequencyCarrierScheme,
      FinalizationScheme,
      GeneratedNameScheme,
      ProvenanceScheme,
      RelationPurityScheme,
      StructuralTypeEvidenceScheme,
      StructuralEqualityScheme,
      EnumerationProfileScheme,
      EnumerationProfileId,
      ReductionMode.id,
      CteMaterializationScope,
      CteMaterialization.id
    ).iterator.map(lengthPrefix).mkString
  )

  private def lengthPrefix(value: String): String = {
    val size = value.getBytes(StandardCharsets.UTF_8).length
    s"$size:$value"
  }

  private def sha256(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}

/** One exact live pg_attribute row from an admitted base table. */
private[yannakakis] final case class PostgreSqlRelevanceLimitedBaseColumn(
    sourceName: String,
    attributeNumber: Int,
    typeOid: Long,
    structural: Boolean
) extends Serializable {
  require(
    sourceName != null && sourceName.nonEmpty && sourceName.indexOf('\u0000') < 0,
    "catalog column name must be nonempty and NUL-free"
  )
  require(attributeNumber > 0, "catalog user-column attribute number must be positive")
  require(typeOid > 0L, "catalog column type OID must be positive")
  require(
    !structural || PostgreSqlRelevanceLimitedBaseColumn.sqlTypeName(typeOid).nonEmpty,
    "structural columns must use built-in int2, int4, or int8"
  )
}

private[yannakakis] object PostgreSqlRelevanceLimitedBaseColumn {
  val SmallIntOid: Long = 21L
  val IntegerOid: Long = 23L
  val BigIntOid: Long = 20L

  def sqlTypeName(typeOid: Long): Option[String] = typeOid match {
    case SmallIntOid => Some("SMALLINT")
    case IntegerOid  => Some("INTEGER")
    case BigIntOid   => Some("BIGINT")
    case _           => None
  }
}

/** One exact ordinary-base-table observation used to permit CSE reuse. */
private[yannakakis] final case class PostgreSqlRelevanceLimitedBaseRelation(
    qualifiedName: Vector[String],
    databaseOid: Long,
    namespaceOid: Long,
    relationOid: Long,
    relkind: String,
    hasSubclass: Boolean,
    rowSecurity: Boolean,
    forceRowSecurity: Boolean,
    columns: Vector[PostgreSqlRelevanceLimitedBaseColumn]
) extends Serializable {
  require(
    qualifiedName != null && qualifiedName.nonEmpty &&
      qualifiedName.forall(value => value != null && value.nonEmpty),
    "catalog relation name must be nonempty"
  )
  require(databaseOid > 0L && namespaceOid > 0L && relationOid > 0L)
  require(relkind == "r", "shared expressions require an ordinary base table")
  require(
    !hasSubclass,
    "shared expressions require a table without inheritance children"
  )
  require(
    !rowSecurity && !forceRowSecurity,
    "shared expressions currently require row-security-free relations"
  )
  require(columns != null && columns.nonEmpty, "catalog columns must be nonempty")
  require(
    columns == columns.sortBy(_.attributeNumber),
    "catalog columns must be ordered by pg_attribute number"
  )
  require(
    columns.map(_.sourceName).distinct.size == columns.size &&
      columns.map(_.attributeNumber).distinct.size == columns.size,
    "catalog column names and attribute numbers must be unique"
  )
}

/**
  * Verifier-produced read-only catalog evidence for relations evaluated once and
  * reused.  This prevents a volatile view from being silently treated as a
  * table-valued pure expression.  The benchmark exporter obtains these rows
  * from pg_catalog through the same data-source/database boundary used to
  * analyze the query.
  */
private[yannakakis] final class PostgreSqlRelevanceLimitedBaseTableEvidence private (
    val databaseName: String,
    val serverVersionNumber: Int,
    val relations: Vector[PostgreSqlRelevanceLimitedBaseRelation],
    val id: String
) extends Serializable {
  private val names = relations.iterator.map(_.qualifiedName).toSet
  private val relationByName = relations.iterator.map(value => value.qualifiedName -> value).toMap

  def contains(qualifiedName: Vector[String]): Boolean =
    qualifiedName != null && names.contains(qualifiedName)

  def column(
      qualifiedName: Vector[String],
      sourceName: String
  ): Option[PostgreSqlRelevanceLimitedBaseColumn] = {
    if (qualifiedName == null || sourceName == null) None
    else
      relationByName
        .get(qualifiedName)
        .flatMap(_.columns.find(_.sourceName == sourceName))
  }

  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlRelevanceLimitedBaseTableEvidence => id == that.id
    case _                                                 => false
  }
  override def hashCode(): Int = id.hashCode
  override def toString: String =
    s"PostgreSqlRelevanceLimitedBaseTableEvidence($id)"
}

private[yannakakis] object PostgreSqlRelevanceLimitedBaseTableEvidence {
  val Scheme: String =
    "postgresql_relevance_limited_base_table_and_column_evidence_v1"

  def create(
      databaseName: String,
      serverVersionNumber: Int,
      relations: Vector[PostgreSqlRelevanceLimitedBaseRelation]
  ): Either[PostgreSqlRelevanceLimitedCountFailure, PostgreSqlRelevanceLimitedBaseTableEvidence] = {
    if (databaseName == null || databaseName.isEmpty)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "catalog-database",
          "catalog database name is missing"
        )
      )
    if (serverVersionNumber <= 0)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "catalog-version",
          "catalog server version is invalid"
        )
      )
    if (relations == null || relations.isEmpty || relations.exists(_ == null))
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "catalog-relations",
          "catalog relation observations are missing"
        )
      )
    if (relations.map(_.qualifiedName).distinct.size != relations.size)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "catalog-relations",
          "catalog relation observations must have unique qualified names"
        )
      )
    if (relations.map(_.relationOid).distinct.size != relations.size)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "catalog-relations",
          "catalog relation observations must have unique relation OIDs"
        )
      )
    if (relations.map(_.databaseOid).distinct.size != 1)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "catalog-relations",
          "catalog relation observations must belong to one database"
        )
      )

    val canonical = relations.sortBy(relation => relation.qualifiedName.mkString("\u0000"))
    val preimage = Vector(
      Scheme,
      databaseName,
      serverVersionNumber.toString
    ).iterator
      .concat(
        canonical.iterator.flatMap(relation =>
          Iterator(relation.qualifiedName.size.toString)
            .concat(relation.qualifiedName.iterator)
            .concat(Iterator(
            relation.databaseOid.toString,
            relation.namespaceOid.toString,
            relation.relationOid.toString,
            relation.relkind,
            relation.hasSubclass.toString,
            relation.rowSecurity.toString,
            relation.forceRowSecurity.toString,
            relation.columns.size.toString
          )).concat(
              relation.columns.iterator.flatMap(column =>
                Iterator(
                  column.sourceName,
                  column.attributeNumber.toString,
                  column.typeOid.toString,
                  column.structural.toString
                )
              )
            )
        )
      )
      .map(lengthPrefix)
      .mkString
    Right(
      new PostgreSqlRelevanceLimitedBaseTableEvidence(
        databaseName,
        serverVersionNumber,
        canonical,
        "sha256:" + sha256(preimage)
      )
    )
  }

  private def lengthPrefix(value: String): String = {
    val size = value.getBytes(StandardCharsets.UTF_8).length
    s"$size:$value"
  }

  private def sha256(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}

private[yannakakis] sealed trait PostgreSqlRelevanceLimitedCountFailureClass
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlRelevanceLimitedCountFailureClass {
  case object NotApplicable extends PostgreSqlRelevanceLimitedCountFailureClass {
    override val id: String = "not-applicable"
  }
  case object InvalidInput extends PostgreSqlRelevanceLimitedCountFailureClass {
    override val id: String = "invalid-input"
  }
  case object ResourceLimited
      extends PostgreSqlRelevanceLimitedCountFailureClass {
    override val id: String = "resource-limited"
  }
  case object RenderingFailed
      extends PostgreSqlRelevanceLimitedCountFailureClass {
    override val id: String = "rendering-failed"
  }
}

private[yannakakis] final case class PostgreSqlRelevanceLimitedCountFailure(
    failureClass: PostgreSqlRelevanceLimitedCountFailureClass,
    code: String,
    detail: String
) extends Serializable {
  require(failureClass != null)
  require(code != null && code.matches("[a-z0-9][a-z0-9.-]{0,127}"))
  require(detail != null && detail.nonEmpty)
}

private[yannakakis] object PostgreSqlRelevanceLimitedCountFailure {
  private val Prefix = "relevance-limited-count"

  def notApplicable(suffix: String, detail: String) =
    PostgreSqlRelevanceLimitedCountFailure(
      PostgreSqlRelevanceLimitedCountFailureClass.NotApplicable,
      s"$Prefix.not-applicable.$suffix",
      detail
    )

  def invalid(suffix: String, detail: String) =
    PostgreSqlRelevanceLimitedCountFailure(
      PostgreSqlRelevanceLimitedCountFailureClass.InvalidInput,
      s"$Prefix.invalid.$suffix",
      detail
    )

  def resource(suffix: String, detail: String) =
    PostgreSqlRelevanceLimitedCountFailure(
      PostgreSqlRelevanceLimitedCountFailureClass.ResourceLimited,
      s"$Prefix.resource.$suffix",
      detail
    )

  def rendering(suffix: String, detail: String) =
    PostgreSqlRelevanceLimitedCountFailure(
      PostgreSqlRelevanceLimitedCountFailureClass.RenderingFailed,
      s"$Prefix.rendering.$suffix",
      detail
    )
}

private[yannakakis] final case class PostgreSqlRelevanceLimitedCountMemberTrace(
    interfaceVariable: VariableId,
    anchorColumn: ColumnId,
    wingEdgeIds: Vector[EdgeId],
    edgeMapFromRepresentative: Vector[(EdgeId, EdgeId)],
    variableMapFromRepresentative: Vector[(VariableId, VariableId)]
) extends Serializable

private[yannakakis] final case class PostgreSqlRelevanceLimitedCountTrace(
    detectorScheme: String,
    anchorEdgeId: EdgeId,
    anchorOccurrenceId: RelationOccurrenceId,
    representativeInterface: VariableId,
    representativeWingEdgeIds: Vector[EdgeId],
    members: Vector[PostgreSqlRelevanceLimitedCountMemberTrace],
    coreEdgeIds: Vector[EdgeId],
    sharedCteName: String,
    baseTableEvidenceId: String,
    physicalCteCount: Int,
    eliminatedWingScanCount: Int,
    anchorDemandScanCount: Int,
    originalSourceScanCount: Int,
    netSourceScanDelta: Int
) extends Serializable {
  require(detectorScheme == PostgreSqlRelevanceLimitedCountProfile.DetectorScheme)
  require(anchorEdgeId != null && anchorOccurrenceId != null)
  require(representativeInterface != null)
  require(representativeWingEdgeIds.nonEmpty)
  require(members.size >= 2)
  require(members.map(_.interfaceVariable).distinct.size == members.size)
  require(coreEdgeIds.contains(anchorEdgeId))
  require(
    sharedCteName.matches("yr_v4_shared_wing(?:_[0-9]+)?"),
    "shared CTE must use the collision-safe V4 naming scheme"
  )
  require(baseTableEvidenceId.matches("sha256:[0-9a-f]{64}"))
  require(physicalCteCount == 1)
  require(eliminatedWingScanCount > 0)
  require(anchorDemandScanCount == members.size)
  require(originalSourceScanCount > 0)
}

private[yannakakis] final case class PostgreSqlRelevanceLimitedCountStatistics(
    expandedScanCount: Long,
    renderedTermCount: Long,
    cteCount: Long,
    sqlCharacterCount: Long
) extends Serializable {
  require(expandedScanCount > 0L)
  require(renderedTermCount > 0L)
  require(cteCount == 1L)
  require(sqlCharacterCount > 0L)
}

private[yannakakis] final case class PostgreSqlRelevanceLimitedCountRenderedQuery(
    sourceQuery: QueryIR,
    candidateInput: CandidateSearchInput,
    provenancePlan: BagFactorPlan,
    sql: String,
    output: Vector[PostgreSqlOutputColumn],
    trace: PostgreSqlRelevanceLimitedCountTrace,
    statistics: PostgreSqlRelevanceLimitedCountStatistics
) extends Serializable {
  require(sourceQuery != null && candidateInput != null && provenancePlan != null)
  require(sql != null && sql.nonEmpty && sql.indexOf('\u0000') < 0)
  require(output != null && output.size == 1 && output.head.ordinal == 0)
  require(trace != null && statistics != null)
  require(statistics.sqlCharacterCount == sql.length.toLong)
}
