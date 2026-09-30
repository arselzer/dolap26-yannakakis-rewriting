package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.SemijoinPass
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, VariableId}

/**
  * Stable identity of one renderer-owned PostgreSQL stage.
  *
  * Identity is local to one rendered query. Replay evidence spanning physical
  * variants must bind `(physicalVariantId, stageId)` rather than treating a
  * stage id as globally unique.
  */
sealed trait PostgreSqlStageId extends Product with Serializable {
  def code: String
}

object PostgreSqlStageId {
  val Scheme: String = "postgresql_renderer_stage_id_v1"

  /** Logical support projection for one bag, emitted or elided. */
  final case class BagSupport private[postgresql] (bagId: BagId)
      extends PostgreSqlStageId {
    override val code: String = s"support:b${bagId.value}"
  }

  /**
    * One physical factor version. Version zero is the initial factor; every
    * emitted fused reduction of the bag increments the version exactly once.
    */
  final case class BagFactorVersion private[postgresql] (
      bagId: BagId,
      version: Int
  ) extends PostgreSqlStageId {
    override val code: String = s"factor:b${bagId.value}:v$version"
  }

  /**
    * The terminal result SELECT, which is not a named CTE. The v1 identity
    * remains stable when an aggregate terminal avoids reconstruction.
    */
  case object FinalReconstruction extends PostgreSqlStageId {
    override val code: String = "final-reconstruction"
  }
}

sealed trait PostgreSqlStageKind extends Product with Serializable {
  def code: String
}

object PostgreSqlStageKind {
  case object Support extends PostgreSqlStageKind {
    override val code: String = "support"
  }
  case object InitialFactor extends PostgreSqlStageKind {
    override val code: String = "initial-factor"
  }
  case object FusedReduction extends PostgreSqlStageKind {
    override val code: String = "fused-reduction"
  }
  case object FinalReconstruction extends PostgreSqlStageKind {
    override val code: String = "final-reconstruction"
  }
}

/**
  * Exact physical form of a stage in the emitted statement.
  *
  * `PlannerControlledCte` deliberately does not claim engine
  * materialization: the current SQL uses neither `MATERIALIZED` nor
  * `NOT MATERIALIZED`, so PostgreSQL controls folding/materialization.
  */
sealed trait PostgreSqlStagePhysicalForm extends Product with Serializable {
  def code: String
  def emitted: Boolean
  def namedCte: Boolean
  def explicitlyMaterialized: Boolean
}

object PostgreSqlStagePhysicalForm {
  case object Elided extends PostgreSqlStagePhysicalForm {
    override val code: String = "elided"
    override val emitted: Boolean = false
    override val namedCte: Boolean = false
    override val explicitlyMaterialized: Boolean = false
  }

  case object PlannerControlledCte extends PostgreSqlStagePhysicalForm {
    override val code: String = "planner-controlled-cte"
    override val emitted: Boolean = true
    override val namedCte: Boolean = true
    override val explicitlyMaterialized: Boolean = false
  }

  case object MaterializedCte extends PostgreSqlStagePhysicalForm {
    override val code: String = "materialized-cte"
    override val emitted: Boolean = true
    override val namedCte: Boolean = true
    override val explicitlyMaterialized: Boolean = true
  }

  case object NotMaterializedCte extends PostgreSqlStagePhysicalForm {
    override val code: String = "not-materialized-cte"
    override val emitted: Boolean = true
    override val namedCte: Boolean = true
    override val explicitlyMaterialized: Boolean = false
  }

  case object TerminalSelect extends PostgreSqlStagePhysicalForm {
    override val code: String = "terminal-select"
    override val emitted: Boolean = true
    override val namedCte: Boolean = false
    override val explicitlyMaterialized: Boolean = false
  }
}

/** Exact logical semijoin represented inside one fused physical CTE. */
final case class PostgreSqlStageSemijoinMember private[postgresql] (
    pass: SemijoinPass,
    ordinal: Int,
    target: BagId,
    source: BagId,
    separator: Vector[VariableId]
) extends Serializable

/**
  * Ordered logical-to-physical provenance for one renderer-owned stage.
  *
  * `logicalBagIds` uses the stage's logical traversal order. Support and
  * initial-factor stages contain one bag; a reduction contains its target then
  * its distinct sources in member order. Reconstruction and frequency COUNT
  * terminals list factors from the root in canonical arc order. A partial-mode
  * staged extremum instead lists its guard owner, records the base source
  * occurrences in `logicalEdgeIds`, and claims no factor-stage input. A full
  * extremum lists the same owner with the exact latest factor-stage input and
  * no source-edge provenance. `logicalEdgeIds` otherwise records the support
  * cover or owned occurrences where applicable.
  *
  * A partial-mode guarded-extremum terminal may internally emit
  * decomposition-guided MATERIALIZED support CTEs. They are included in
  * rendering resource and SQL character counts, while the stable v1 public
  * stage identity represents the whole terminal program through its complete
  * `logicalEdgeIds` provenance and deliberately adds no new public stage kind.
  *
  * Diagnostic SQL is constructed separately by the package-private renderer,
  * which asks the emitter for an alternate terminal SELECT over the already-
  * compiled CTE structure. Neither this trace nor that path parses rendered
  * SQL or depends on PostgreSQL query-plan node order.
  */
final case class PostgreSqlStageTrace private[postgresql] (
    id: PostgreSqlStageId,
    kind: PostgreSqlStageKind,
    physicalForm: PostgreSqlStagePhysicalForm,
    physicalCteName: Option[String],
    logicalBagIds: Vector[BagId],
    logicalEdgeIds: Vector[EdgeId],
    semijoinMembers: Vector[PostgreSqlStageSemijoinMember],
    inputStageIds: Vector[PostgreSqlStageId]
) extends Serializable

object PostgreSqlStageTrace {
  /**
    * Ordering of `stages.filter(_.physicalForm.emitted)`. Elided supports stay
    * in canonical logical support slots but consume no emitted-order position.
    */
  val OrderScheme: String = "postgresql_cte_emission_then_terminal_v1"
}
