package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  BagFactorPlanningLimits
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{QueryIR, QueryResultShape}

/** One of the two same-CandidateTD generated actions in the COUNT triplet. */
private[yannakakis] sealed trait PostgreSqlGlobalCountBenchmarkRealization
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlGlobalCountBenchmarkRealization {
  case object FullReductionControl
      extends PostgreSqlGlobalCountBenchmarkRealization {
    override val id: String = "full-v2-control"
  }

  case object FusedRawNoReduction
      extends PostgreSqlGlobalCountBenchmarkRealization {
    override val id: String = "fused-v3-raw-none"
  }

  val Values: Vector[PostgreSqlGlobalCountBenchmarkRealization] =
    Vector(FullReductionControl, FusedRawNoReduction)
}

/** Exact renderer-profile stack used by one generated COUNT action. */
private[yannakakis] final case class PostgreSqlGlobalCountRendererBinding(
    aggregateProfileV1Scheme: String,
    aggregateProfileV1Id: String,
    aggregateProfileV1AxisIds: Vector[String],
    aggregateProfileV2Scheme: String,
    aggregateProfileV2Id: String,
    aggregateProfileV2AxisIds: Vector[String],
    aggregateProfileV3Scheme: String,
    aggregateProfileV3Id: String,
    aggregateProfileV3AxisIds: Vector[String],
    enumerationProfileScheme: String,
    enumerationProfileId: String
) extends Serializable {
  require(
    Vector(
      aggregateProfileV1Scheme,
      aggregateProfileV1Id,
      aggregateProfileV2Scheme,
      aggregateProfileV2Id,
      aggregateProfileV3Scheme,
      aggregateProfileV3Id,
      enumerationProfileScheme,
      enumerationProfileId
    ).forall(value => value != null && value.nonEmpty)
  )
  require(
    aggregateProfileV1AxisIds != null &&
      aggregateProfileV2AxisIds != null &&
      aggregateProfileV3AxisIds != null &&
      (aggregateProfileV1AxisIds ++ aggregateProfileV2AxisIds ++
        aggregateProfileV3AxisIds).forall(value =>
        value != null && value.nonEmpty
      )
  )
}

/**
  * Exact development-only renderer boundary for the scalar COUNT triplet.
  *
  * This deliberately does not participate in the public/default renderer or
  * in the frozen aggregate physical sweep. The full control is the optimized
  * renderer-v2 composition used by the recognized-schema development result;
  * the fused action is the narrow renderer-v3 raw-factor realization.
  */
private[yannakakis] object PostgreSqlGlobalCountBenchmarkRenderer {
  val EnumerationProfileScheme: String =
    "postgresql_enumeration_rendering_profile_v1"

  private val enumerationProfile =
    PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2

  private val fullBaseProfileV1 = PostgreSqlAggregateRenderingProfile
    .create(
      PostgreSqlAggregateOneBagTerminalProfile.LegacyNestedV1,
      PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2,
      PostgreSqlReductionSupportProfile.DistinctSeparatorSupportV1
    )
    .fold(
      problems =>
        throw new IllegalStateException(
          problems.map(_.code).mkString("invalid full-control v1 profile: ", ",", "")
        ),
      identity
    )
  private val fullProfileV2 = PostgreSqlAggregateRenderingProfileV2
    .create(
      fullBaseProfileV1,
      PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2,
      PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2
    )
    .fold(
      problems =>
        throw new IllegalStateException(
          problems.map(_.code).mkString("invalid full-control v2 profile: ", ",", "")
        ),
      identity
    )
  private val fullProfileV3 = PostgreSqlAggregateRenderingProfileV3
    .baseline(fullProfileV2)
    .fold(
      problems =>
        throw new IllegalStateException(
          problems.map(_.code).mkString("invalid full-control v3 wrapper: ", ",", "")
        ),
      identity
    )

  private val fusedBaseProfileV1 =
    PostgreSqlAggregateRenderingProfile.LegacyV1
  private val fusedBaseProfileV2 = PostgreSqlAggregateRenderingProfileV2
    .baseline(fusedBaseProfileV1)
    .fold(
      problems =>
        throw new IllegalStateException(
          problems.map(_.code).mkString("invalid fused v2 baseline: ", ",", "")
        ),
      identity
    )
  private val fusedProfileV3 = PostgreSqlAggregateRenderingProfileV3
    .fusedRawFactorMessages(fusedBaseProfileV2)
    .fold(
      problems =>
        throw new IllegalStateException(
          problems.map(_.code).mkString("invalid fused v3 profile: ", ",", "")
        ),
      identity
    )

  val FullReductionMode: YannakakisReductionMode =
    YannakakisReductionMode.FullTwoPass
  val FusedReductionMode: YannakakisReductionMode =
    YannakakisReductionMode.NoReduction
  val CteDirective: PostgreSqlCteMaterializationDirective =
    PostgreSqlCteMaterializationDirective.PlannerControlled

  val FullControlBinding: PostgreSqlGlobalCountRendererBinding =
    binding(fullBaseProfileV1, fullProfileV2, fullProfileV3)
  val FusedBinding: PostgreSqlGlobalCountRendererBinding =
    binding(fusedBaseProfileV1, fusedBaseProfileV2, fusedProfileV3)

  def bindingFor(
      realization: PostgreSqlGlobalCountBenchmarkRealization
  ): PostgreSqlGlobalCountRendererBinding = realization match {
    case PostgreSqlGlobalCountBenchmarkRealization.FullReductionControl =>
      FullControlBinding
    case PostgreSqlGlobalCountBenchmarkRealization.FusedRawNoReduction =>
      FusedBinding
  }

  def reductionModeFor(
      realization: PostgreSqlGlobalCountBenchmarkRealization
  ): YannakakisReductionMode = realization match {
    case PostgreSqlGlobalCountBenchmarkRealization.FullReductionControl =>
      FullReductionMode
    case PostgreSqlGlobalCountBenchmarkRealization.FusedRawNoReduction =>
      FusedReductionMode
  }

  def render(
      realization: PostgreSqlGlobalCountBenchmarkRealization,
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      renderingLimits: PostgreSqlRenderingLimits =
        PostgreSqlRenderingLimits.default,
      planningLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] = {
    if (realization == null || query == null || input == null || plan == null)
      return invariant(
        "COUNT benchmark rendering requires realization, query, input, and plan"
      )
    if (!query.resultShape.isInstanceOf[QueryResultShape.GlobalCountStar])
      return invariant("COUNT benchmark rendering requires scalar global COUNT(*)")
    if (
      plan.factors.size <= 1 &&
      realization ==
        PostgreSqlGlobalCountBenchmarkRealization.FusedRawNoReduction
    )
      return invariant("one-bag scalar COUNT is not a distinct fused-v3 action")

    realization match {
      case PostgreSqlGlobalCountBenchmarkRealization.FullReductionControl =>
        PostgreSqlBagFactorRenderer
          .compileAndRenderWithAggregateProfileV2(
            query,
            input,
            plan,
            renderingLimits,
            planningLimits,
            FullReductionMode,
            fullProfileV2,
            enumerationProfile
          )
          .flatMap { case (compiled, rendered) =>
            val frequencyTerminal = compiled.terminal match {
              case CompiledPostgreSqlTerminal.GlobalCountStar(
                    _,
                    _: CompiledPostgreSqlTerminal.GlobalCountStrategy.FrequencyPropagation
                  ) => true
              case _ => false
            }
            validateExactBoundary(
              compiled,
              rendered,
              query,
              input,
              plan,
              fullBaseProfileV1,
              fullProfileV2,
              fullProfileV3,
              FullReductionMode,
              expectReductionCtes = plan.arcs.nonEmpty,
              frequencyTerminal
            )
          }

      case PostgreSqlGlobalCountBenchmarkRealization.FusedRawNoReduction =>
        PostgreSqlBagFactorRenderer
          .compileAndRenderWithAggregateProfileV3(
            query,
            input,
            plan,
            renderingLimits,
            planningLimits,
            FusedReductionMode,
            fusedProfileV3,
            enumerationProfile
          )
          .flatMap { case (compiled, rendered) =>
            val rawFrequencyTerminal = compiled.terminal match {
              case CompiledPostgreSqlTerminal.GlobalCountStar(
                    _,
                    _: CompiledPostgreSqlTerminal.GlobalCountStrategy.RawFrequencyPropagation
                  ) => true
              case _ => false
            }
            validateExactBoundary(
              compiled,
              rendered,
              query,
              input,
              plan,
              fusedBaseProfileV1,
              fusedBaseProfileV2,
              fusedProfileV3,
              FusedReductionMode,
              expectReductionCtes = false,
              rawFrequencyTerminal
            )
          }
    }
  }

  private def binding(
      profileV1: PostgreSqlAggregateRenderingProfile,
      profileV2: PostgreSqlAggregateRenderingProfileV2,
      profileV3: PostgreSqlAggregateRenderingProfileV3
  ): PostgreSqlGlobalCountRendererBinding =
    PostgreSqlGlobalCountRendererBinding(
      PostgreSqlAggregateRenderingProfile.Scheme,
      profileV1.id,
      profileV1.axisIds,
      PostgreSqlAggregateRenderingProfileV2.Scheme,
      profileV2.id,
      profileV2.axisIds,
      PostgreSqlAggregateRenderingProfileV3.Scheme,
      profileV3.id,
      profileV3.axisIds,
      EnumerationProfileScheme,
      enumerationProfile.id
    )

  private def validateExactBoundary(
      compiled: CompiledPostgreSqlRendering,
      rendered: PostgreSqlRenderedQuery,
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      profileV1: PostgreSqlAggregateRenderingProfile,
      profileV2: PostgreSqlAggregateRenderingProfileV2,
      profileV3: PostgreSqlAggregateRenderingProfileV3,
      reductionMode: YannakakisReductionMode,
      expectReductionCtes: Boolean,
      expectedTerminal: Boolean
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] = {
    val reduction = compiled.trace.reduction
    val expectedBottomUp =
      if (reductionMode == FullReductionMode) plan.schedule.bottomUp.size else 0
    val expectedTopDown =
      if (reductionMode == FullReductionMode) plan.schedule.topDown.size else 0
    val exactBoundary =
      compiled.query == query &&
        compiled.input == input &&
        compiled.plan == plan &&
        compiled.aggregateRenderingProfile == profileV1 &&
        compiled.aggregateRendererV2Trace.requestedProfile == profileV2 &&
        compiled.aggregateRenderingProfileV3 == profileV3 &&
        compiled.enumerationRenderingProfile == enumerationProfile &&
        compiled.cteMaterialization == CteDirective &&
        (compiled.reductionCtes.nonEmpty == expectReductionCtes) &&
        reduction.mode == reductionMode &&
        reduction.selectedBottomUpStepCount == expectedBottomUp &&
        reduction.selectedTopDownStepCount == expectedTopDown &&
        (reduction.physicalReductionCteCount > 0 == expectReductionCtes) &&
        (compiled.trace.semijoins.nonEmpty == expectReductionCtes) &&
        expectedTerminal &&
        rendered.sourceQuery == query &&
        rendered.bagFactorPlan == plan &&
        rendered.trace == compiled.trace &&
        rendered.sql != null && rendered.sql.nonEmpty &&
        rendered.sql.indexOf('\u0000') < 0
    if (exactBoundary) Right(rendered)
    else invariant("COUNT renderer escaped its fixed benchmark boundary")
  }

  private def invariant(
      detail: String
  ): Left[Vector[PostgreSqlRenderingError], Nothing] =
    Left(Vector(PostgreSqlRenderingError.ModelInvariantViolation(detail)))
}
