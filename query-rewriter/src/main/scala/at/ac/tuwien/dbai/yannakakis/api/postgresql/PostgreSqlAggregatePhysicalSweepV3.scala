package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorCoverAssignment,
  BagFactorPhysicalConfiguration,
  BagFactorPlan,
  BagFactorPlanner,
  BagFactorPlanningLimits,
  ConfiguredBagFactorPlanner
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlAggregateRenderingProfile,
  PostgreSqlBagFactorRenderer,
  PostgreSqlEnumerationRenderingProfile,
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingLimits
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  GlobalGuardedExtremumFunction,
  QueryResultShape
}

/** Closed aggregate family admitted by the development-only physical sweep. */
private[yannakakis] sealed trait PostgreSqlAggregatePhysicalSweepV3Family
    extends Product
    with Serializable {
  def id: String
  def semanticExecutionLane: String
  def groupKeyCount: Int
  def requiredRanks: Vector[Int]
  private[postgresql] def accepts(shape: QueryResultShape): Boolean
  private[postgresql] def forms: Vector[
    (YannakakisReductionMode, PostgreSqlCteMaterializationDirective)
  ]
}

private[yannakakis] object PostgreSqlAggregatePhysicalSweepV3Family {
  case object GlobalCount extends PostgreSqlAggregatePhysicalSweepV3Family {
    override val id: String = "global-count"
    override val semanticExecutionLane: String =
      "global-count-frequency-propagation"
    override val groupKeyCount: Int = 0
    override val requiredRanks: Vector[Int] = Vector(1, 5)
    override private[postgresql] def accepts(shape: QueryResultShape): Boolean =
      shape.isInstanceOf[QueryResultShape.GlobalCountStar]
    override private[postgresql] val forms = Vector(
      YannakakisReductionMode.FullTwoPass ->
        PostgreSqlCteMaterializationDirective.PlannerControlled,
      YannakakisReductionMode.FullTwoPass ->
        PostgreSqlCteMaterializationDirective.Materialized
    )
  }

  case object DistributedGroupedCount
      extends PostgreSqlAggregatePhysicalSweepV3Family {
    override val id: String = "u1-distributed-count"
    override val semanticExecutionLane: String =
      "distributed-u1-frequency-propagation"
    override val groupKeyCount: Int = 2
    override val requiredRanks: Vector[Int] = Vector(1, 10)
    override private[postgresql] def accepts(shape: QueryResultShape): Boolean =
      shape.isInstanceOf[QueryResultShape.DistributedGroupedCountStar]
    override private[postgresql] val forms = GlobalCount.forms
  }

  case object GuardedMin extends PostgreSqlAggregatePhysicalSweepV3Family {
    override val id: String = "guarded-min"
    override val semanticExecutionLane: String = "guarded-extremum"
    override val groupKeyCount: Int = 0
    override val requiredRanks: Vector[Int] = Vector(1, 2)
    override private[postgresql] def accepts(shape: QueryResultShape): Boolean =
      shape match {
        case value: QueryResultShape.GlobalGuardedExtremum =>
          value.function == GlobalGuardedExtremumFunction.Min
        case _ => false
      }
    override private[postgresql] val forms = Vector(
      YannakakisReductionMode.FullTwoPass ->
        PostgreSqlCteMaterializationDirective.PlannerControlled,
      YannakakisReductionMode.BottomUpOnly ->
        PostgreSqlCteMaterializationDirective.PlannerControlled
    )
  }

  case object GuardedMax extends PostgreSqlAggregatePhysicalSweepV3Family {
    override val id: String = "guarded-max"
    override val semanticExecutionLane: String = "guarded-extremum"
    override val groupKeyCount: Int = 0
    override val requiredRanks: Vector[Int] = Vector(1, 2)
    override private[postgresql] def accepts(shape: QueryResultShape): Boolean =
      shape match {
        case value: QueryResultShape.GlobalGuardedExtremum =>
          value.function == GlobalGuardedExtremumFunction.Max
        case _ => false
      }
    override private[postgresql] val forms = GuardedMin.forms
  }

  val Values: Vector[PostgreSqlAggregatePhysicalSweepV3Family] = Vector(
    GlobalCount,
    DistributedGroupedCount,
    GuardedMin,
    GuardedMax
  )

  def parse(value: String): Option[PostgreSqlAggregatePhysicalSweepV3Family] =
    if (value == null) None else Values.find(_.id == value)
}

/** One of exactly four registered SQL actions for an admitted family. */
private[yannakakis] final case class PostgreSqlAggregatePhysicalSweepV3Action(
    ordinal: Int,
    aliasId: String,
    structuralRank: Int,
    candidateId: CandidatePlanId,
    physicalVariantId: PostgreSqlPhysicalVariantId,
    configuration: BagFactorPhysicalConfiguration,
    reductionMode: YannakakisReductionMode,
    cteMaterializationDirective: PostgreSqlCteMaterializationDirective,
    renderedQuery: PostgreSqlRenderedQuery,
    renderedStatementSha256: String
) extends Serializable {
  require(ordinal > 0, "aggregate physical action ordinal must be positive")
  require(aliasId != null && aliasId.nonEmpty, "action alias must not be blank")
  require(structuralRank > 0, "action structural rank must be positive")
  require(candidateId != null, "action candidate id must not be null")
  require(physicalVariantId != null, "action physical id must not be null")
  require(configuration != null, "action configuration must not be null")
  require(reductionMode != null, "action reduction mode must not be null")
  require(
    cteMaterializationDirective != null,
    "action CTE directive must not be null"
  )
  require(renderedQuery != null, "action rendered query must not be null")
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}"),
    "action digest must be lowercase SHA-256"
  )

  val sql: String = renderedQuery.sql
}

private[yannakakis] final case class PostgreSqlAggregatePhysicalSweepV3Result(
    protocolId: String,
    family: PostgreSqlAggregatePhysicalSweepV3Family,
    resultShapeId: String,
    aggregateRenderingProfileId: String,
    enumerationRenderingProfileId: String,
    actions: Vector[PostgreSqlAggregatePhysicalSweepV3Action]
) extends Serializable {
  require(protocolId == PostgreSqlAggregatePhysicalSweepV3.ProtocolId)
  require(family != null, "aggregate physical family must not be null")
  require(resultShapeId != null && resultShapeId.nonEmpty)
  require(aggregateRenderingProfileId == PostgreSqlAggregatePhysicalSweepV3.AggregateProfileId)
  require(enumerationRenderingProfileId == PostgreSqlAggregatePhysicalSweepV3.EnumerationProfileId)
  require(
    actions != null && actions.size == 4,
    "aggregate physical sweep must contain exactly four actions"
  )
  require(actions.map(_.ordinal) == Vector(1, 2, 3, 4))
  require(actions.map(_.aliasId).distinct.size == actions.size)
  require(actions.map(_.physicalVariantId).distinct.size == actions.size)
}

private[yannakakis] final case class PostgreSqlAggregatePhysicalSweepV3Failure(
    code: String,
    detail: String
) extends Serializable {
  require(code != null && code.nonEmpty)
  require(detail != null && detail.nonEmpty)
}

/**
  * Development-only, one-search export seam for four high-information
  * aggregate physical cells. It does not alter rewrite defaults or serializers.
  */
private[yannakakis] object PostgreSqlAggregatePhysicalSweepV3 {
  val ProtocolId: String = "postgresql-aggregate-physical-sweep/current-v3"
  val RequiredPlanLimit: Int = 10
  val AggregateProfileId: String = PostgreSqlAggregateRenderingProfile.LegacyV1.id
  val EnumerationProfileId: String =
    PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2.id

  def produce(
      artifact: PostgreSqlRewriteArtifact,
      family: PostgreSqlAggregatePhysicalSweepV3Family,
      planningLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default,
      renderingLimits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default
  ): Either[
    PostgreSqlAggregatePhysicalSweepV3Failure,
    PostgreSqlAggregatePhysicalSweepV3Result
  ] = {
    if (artifact == null) return failed("artifact.missing", "rewrite artifact is missing")
    if (family == null) return failed("family.missing", "aggregate family is missing")
    if (planningLimits == null || renderingLimits == null)
      return failed("limits.missing", "planning or rendering limits are missing")
    if (!family.accepts(artifact.query.resultShape))
      return failed(
        "family.mismatch",
        s"family=${family.id},shape=${artifact.query.resultShape.id}"
      )
    if (
      artifact.candidateSearch.requestedLimit != RequiredPlanLimit ||
      artifact.candidateSearch.plans.size < family.requiredRanks.max
    )
      return failed(
        "candidate-prefix.incomplete",
        s"required-limit=$RequiredPlanLimit,retained=${artifact.candidateSearch.plans.size}"
      )
    if (
      artifact.candidateSelection.policy != CandidatePlanSelection.StructuralHead ||
      artifact.selectedPlan.rank != 1 ||
      artifact.candidateSelection.reductionMode !=
        YannakakisReductionMode.FullTwoPass
    )
      return failed(
        "anchor.mismatch",
        "one-pass anchor must be structural rank 1 with full reduction"
      )
    if (
      PostgreSqlEnumerationRenderingProfile.default !=
        PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
    )
      return failed(
        "profile.mismatch",
        "default enumeration renderer is not statistics-safe-v2"
      )

    val actions = Vector.newBuilder[PostgreSqlAggregatePhysicalSweepV3Action]
    var ordinal = 1
    family.requiredRanks.foreach { rank =>
      val candidate = artifact.candidateSearch.plans
        .find(_.rank == rank)
        .getOrElse(
          return failed("rank.not-retained", s"rank $rank was not retained")
        )
      if (candidate.candidateId != CandidatePlanId.from(candidate.decomposition))
        return failed("candidate.mismatch", s"rank $rank candidate id mismatch")

      val canonicalPlan = BagFactorPlanner
        .plan(artifact.candidateInput, candidate.decomposition, planningLimits)
        .fold(
          errors =>
            return failed(
              "planning.failed",
              s"rank=$rank,errors=${errors.map(_.code).mkString(",")}"
            ),
          identity
        )
      if (rank == 1 && canonicalPlan != artifact.bagFactorPlan)
        return failed(
          "anchor.mismatch",
          "rank-1 replanning differs from the rewrite artifact"
        )
      val configuration = canonicalConfiguration(canonicalPlan)
      val configured = ConfiguredBagFactorPlanner
        .plan(
          artifact.candidateInput,
          candidate.decomposition,
          configuration,
          planningLimits
        )
        .fold(
          errors =>
            return failed(
              "configured-planning.failed",
              s"rank=$rank,errors=${errors.map(_.code).mkString(",")}"
            ),
          identity
        )
      if (configured.plan != canonicalPlan)
        return failed(
          "configured-planning.mismatch",
          s"rank $rank did not reproduce the canonical plan"
        )

      family.forms.foreach { case (mode, directive) =>
        val rendered = PostgreSqlBagFactorRenderer
          .renderConfigured(
            artifact.query,
            artifact.candidateInput,
            configured,
            renderingLimits,
            planningLimits,
            mode,
            directive
          )
          .fold(
            errors =>
              return failed(
                "rendering.failed",
                s"rank=$rank,mode=${mode.id},directive=${directive.id},errors=${errors.map(_.code).mkString(",")}"
              ),
            identity
          )
        if (
          rendered.sourceQuery != artifact.query ||
          rendered.bagFactorPlan != canonicalPlan ||
          rendered.trace.reduction.mode != mode ||
          rendered.sql == null || rendered.sql.isEmpty ||
          rendered.sql.indexOf('\u0000') >= 0
        )
          return failed(
            "rendering.invariant",
            s"rank=$rank,mode=${mode.id},directive=${directive.id}"
          )
        if (
          rank == 1 &&
          mode == YannakakisReductionMode.FullTwoPass &&
          directive == PostgreSqlCteMaterializationDirective.PlannerControlled &&
          rendered.sql != artifact.renderedQuery.sql
        )
          return failed(
            "anchor.rendering-mismatch",
            "rank-1 full/planner SQL differs from the rewrite artifact"
          )
        val alias = aliasId(family, rank, mode, directive)
        actions += PostgreSqlAggregatePhysicalSweepV3Action(
          ordinal = ordinal,
          aliasId = alias,
          structuralRank = rank,
          candidateId = candidate.candidateId,
          physicalVariantId = PostgreSqlPhysicalVariantId.fromV2(
            candidate.candidateId,
            configuration,
            mode,
            directive
          ),
          configuration = configuration,
          reductionMode = mode,
          cteMaterializationDirective = directive,
          renderedQuery = rendered,
          renderedStatementSha256 =
            PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
              rendered.sql
            )
        )
        ordinal += 1
      }
    }

    Right(
      PostgreSqlAggregatePhysicalSweepV3Result(
        ProtocolId,
        family,
        artifact.query.resultShape.id,
        AggregateProfileId,
        EnumerationProfileId,
        actions.result()
      )
    )
  }

  private def canonicalConfiguration(
      plan: BagFactorPlan
  ): BagFactorPhysicalConfiguration =
    BagFactorPhysicalConfiguration(
      plan.root,
      plan.factors.sortBy(_.bagId).map(factor =>
        BagFactorCoverAssignment(factor.bagId, factor.supportCover)
      )
    )

  private def aliasId(
      family: PostgreSqlAggregatePhysicalSweepV3Family,
      rank: Int,
      mode: YannakakisReductionMode,
      directive: PostgreSqlCteMaterializationDirective
  ): String = {
    val modeId = mode match {
      case YannakakisReductionMode.FullTwoPass => "full"
      case YannakakisReductionMode.BottomUpOnly => "bu"
      case YannakakisReductionMode.NoReduction => "none"
    }
    val directiveId = directive match {
      case PostgreSqlCteMaterializationDirective.PlannerControlled => "pc"
      case PostgreSqlCteMaterializationDirective.Materialized => "mat"
      case PostgreSqlCteMaterializationDirective.NotMaterialized => "not-mat"
    }
    f"${family.id}-r$rank%02d-$modeId-$directiveId"
  }

  private def failed(
      suffix: String,
      detail: String
  ): Left[PostgreSqlAggregatePhysicalSweepV3Failure, Nothing] =
    Left(
      PostgreSqlAggregatePhysicalSweepV3Failure(
        s"aggregate-physical-v3.$suffix",
        detail
      )
    )
}
