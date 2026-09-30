package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  SemijoinPass
}
import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  ColumnId,
  EdgeId,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryIR
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

final case class PostgreSqlOutputColumn private[postgresql] (
    ordinal: Int,
    name: String,
    dataType: String,
    nullable: Boolean
) extends Serializable

final case class PostgreSqlVariableColumnTrace private[postgresql] (
    variableId: VariableId,
    columnName: String
) extends Serializable

final case class PostgreSqlOccurrenceColumnTrace private[postgresql] (
    edgeId: EdgeId,
    columnId: ColumnId,
    columnName: String
) extends Serializable

sealed trait PostgreSqlBagSupportStrategy extends Product with Serializable

object PostgreSqlBagSupportStrategy {
  /** A separate DISTINCT support CTE is required for this bag. */
  case object MaterializedDistinctSupport
      extends PostgreSqlBagSupportStrategy

  /** The bag's owned occurrences already contain its complete support cover. */
  case object ElidedByOwnedCover extends PostgreSqlBagSupportStrategy
}

final case class PostgreSqlBagCteTrace private[postgresql] (
    bagId: BagId,
    supportStrategy: PostgreSqlBagSupportStrategy,
    supportCte: Option[String],
    initialFactorCte: String,
    finalFactorCte: String,
    variableColumns: Vector[PostgreSqlVariableColumnTrace],
    /** Source-occurrence columns physically carried by this factor. */
    occurrenceColumns: Vector[PostgreSqlOccurrenceColumnTrace]
) extends Serializable

/**
  * One logical semijoin and the physical CTE that contains its EXISTS filter.
  * Adjacent same-pass, same-target logical steps may share target input and
  * output CTE names while retaining distinct source inputs and ordinals.
  */
final case class PostgreSqlSemijoinCteTrace private[postgresql] (
    pass: SemijoinPass,
    ordinal: Int,
    target: BagId,
    source: BagId,
    targetInputCte: String,
    sourceInputCte: String,
    outputCte: String
) extends Serializable

/** Canonical logical schedule and the request-selected emitted subset. */
final case class PostgreSqlReductionScheduleTrace private[postgresql] (
    mode: YannakakisReductionMode,
    reductionOrder: String,
    canonicalBottomUpStepCount: Int,
    canonicalTopDownStepCount: Int,
    selectedBottomUpStepCount: Int,
    selectedTopDownStepCount: Int,
    physicalReductionCteCount: Int
) extends Serializable {
  require(mode != null, "reduction mode must not be null")
  require(
    reductionOrder ==
      at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
    "reduction order must be the canonical plan schedule"
  )
  require(
    canonicalBottomUpStepCount >= 0 && canonicalTopDownStepCount >= 0,
    "canonical reduction step counts must not be negative"
  )
  require(
    selectedBottomUpStepCount >= 0 && selectedTopDownStepCount >= 0,
    "selected reduction step counts must not be negative"
  )
  require(
    selectedBottomUpStepCount <= canonicalBottomUpStepCount &&
      selectedTopDownStepCount <= canonicalTopDownStepCount,
    "selected reduction steps must be a canonical subset"
  )
  require(
    physicalReductionCteCount >= 0,
    "physical reduction CTE count must not be negative"
  )
  mode match {
    case YannakakisReductionMode.FullTwoPass =>
      require(
        selectedBottomUpStepCount == canonicalBottomUpStepCount &&
          selectedTopDownStepCount == canonicalTopDownStepCount,
        "full reduction must select both canonical passes"
      )
    case YannakakisReductionMode.BottomUpOnly =>
      require(
        selectedBottomUpStepCount == canonicalBottomUpStepCount &&
          selectedTopDownStepCount == 0,
        "bottom-up reduction must select only the canonical bottom-up pass"
      )
    case YannakakisReductionMode.NoReduction =>
      require(
        selectedBottomUpStepCount == 0 && selectedTopDownStepCount == 0,
        "no reduction must not select a semijoin step"
      )
  }
  private val selectedStepCount =
    selectedBottomUpStepCount + selectedTopDownStepCount
  require(
    physicalReductionCteCount <= selectedStepCount,
    "physical reduction CTEs must not exceed selected logical steps"
  )
  require(
    (physicalReductionCteCount == 0) == (selectedStepCount == 0),
    "physical reduction CTE presence must match selected logical steps"
  )
}

final case class PostgreSqlRenderTrace private[postgresql] (
    bags: Vector[PostgreSqlBagCteTrace],
    semijoins: Vector[PostgreSqlSemijoinCteTrace],
    reduction: PostgreSqlReductionScheduleTrace,
    /**
      * Canonical logical-to-physical stage mapping in SQL emission order.
      *
      * Elided support stages remain present as non-emitted provenance markers.
      * Filtering on `physicalForm.emitted` yields the statement's physical
      * CTEs followed by its terminal SELECT.
      */
    stages: Vector[PostgreSqlStageTrace]
) extends Serializable

/**
  * Exact observable renderer consumption.
  *
  * A rendered term is one projected or grouping scalar (including semijoin
  * `target.*` and `SELECT 1`) or one predicate/equality term in support,
  * factor, semijoin, reconstruction, or final-output SQL.
  */
final case class PostgreSqlRenderingStatistics private[postgresql] (
    expandedScanCount: Long,
    renderedTermCount: Long,
    cteCount: Long,
    sqlCharacterCount: Long
) extends Serializable

final case class PostgreSqlRenderedQuery private[postgresql] (
    sourceQuery: QueryIR,
    bagFactorPlan: BagFactorPlan,
    sql: String,
    output: Vector[PostgreSqlOutputColumn],
    trace: PostgreSqlRenderTrace,
    statistics: PostgreSqlRenderingStatistics
) extends Serializable
