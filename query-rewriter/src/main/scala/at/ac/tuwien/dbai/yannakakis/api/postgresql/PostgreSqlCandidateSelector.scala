package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateResourceLimits,
  CandidatePlanDecision,
  CandidatePlanId,
  CandidateSearchInput,
  RankedCandidatePlan,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  BagFactorPlanningError,
  BagFactorPlanningLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingError,
  PostgreSqlRenderingLimits,
  PostgreSqlStageDiagnosticRendering,
  PostgreSqlStageId,
  PostgreSqlStagePhysicalForm
}
import at.ac.tuwien.dbai.yannakakis.api.CandidatePlanSelection
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  QueryIR,
  QueryResultShape
}

private[postgresql] final case class PostgreSqlSelectedCandidate(
    plan: RankedCandidatePlan,
    bagFactorPlan: BagFactorPlan,
    renderedQuery: PostgreSqlRenderedQuery,
    decision: PostgreSqlCandidateSelectionDecision.Selected
)

private[postgresql] final case class PostgreSqlRenderedCandidate(
    plan: RankedCandidatePlan,
    bagFactorPlan: BagFactorPlan,
    renderedQuery: PostgreSqlRenderedQuery,
    evaluation: PostgreSqlCandidateEvaluation,
    stageDiagnostics: Option[PostgreSqlStageDiagnosticRendering] = None
)

private[postgresql] sealed trait PostgreSqlCandidateSelectionFailure
    extends Product
    with Serializable {
  def decision: Option[PostgreSqlCandidateSelectionDecision]
}

private[postgresql] object PostgreSqlCandidateSelectionFailure {
  final case class InvalidCandidateSet(detail: String)
      extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] = None
  }

  final case class Inapplicable(
      policy: CandidatePlanSelection,
      resultShape: String,
      reason: String
  ) extends PostgreSqlCandidateSelectionFailure {
    require(policy != null, "candidate selection policy must not be null")
    require(resultShape != null && resultShape.nonEmpty, "result shape must not be blank")
    require(reason != null && reason.nonEmpty, "inapplicability reason must not be blank")
    override val decision: Option[PostgreSqlCandidateSelectionDecision] = None
  }

  final case class Unavailable(
      report: PostgreSqlCandidateSelectionDecision.Unavailable
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }

  final case class BagFactorPlanningFailed(
      plan: RankedCandidatePlan,
      problems: Vector[BagFactorPlanningError],
      report: PostgreSqlCandidateSelectionDecision.EvaluationFailed
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }

  final case class RenderingFailed(
      plan: RankedCandidatePlan,
      bagFactorPlan: BagFactorPlan,
      problems: Vector[PostgreSqlRenderingError],
      report: PostgreSqlCandidateSelectionDecision.EvaluationFailed
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }

  final case class ResourceLimitExceeded(
      plan: RankedCandidatePlan,
      resource: PostgreSqlCandidateSelectionResource,
      required: BigInt,
      limit: Long,
      report: PostgreSqlCandidateSelectionDecision.EvaluationFailed
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }

  final case class ExplainCostEvaluationFailed(
      plan: Option[RankedCandidatePlan],
      code: PostgreSqlExplainCostFailureCode,
      report: PostgreSqlCandidateSelectionDecision.ExplainEvaluationFailed
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }

  final case class NoAdmissibleEstimate(
      report: PostgreSqlCandidateSelectionDecision.NoAdmissibleEstimate
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }

  final case class FactorAwareEvaluationFailed(
      plan: Option[RankedCandidatePlan],
      code: PostgreSqlExplainCostFailureCode,
      report: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }

  final case class FactorAwarePlanningFailed(
      plan: RankedCandidatePlan,
      problems: Vector[BagFactorPlanningError],
      report: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }

  final case class FactorAwareRenderingFailed(
      plan: RankedCandidatePlan,
      bagFactorPlan: BagFactorPlan,
      problems: Vector[PostgreSqlRenderingError],
      report: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }

  final case class FactorAwareResourceLimitExceeded(
      plan: RankedCandidatePlan,
      resource: PostgreSqlCandidateSelectionResource,
      required: BigInt,
      limit: Long,
      report: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed
  ) extends PostgreSqlCandidateSelectionFailure {
    override val decision: Option[PostgreSqlCandidateSelectionDecision] =
      Some(report)
  }
}

/** Exact selector over the already retained and certified structural top-K. */
private[postgresql] object PostgreSqlCandidateSelector {
  private[postgresql] val AggregateRootRowGuardDegenerateReason: String =
    "aggregate-root-row-guard-degenerate"
  private[postgresql] val AggregateCountFactorAwareRequiresFullTwoPassReason: String =
    "aggregate-count-factor-aware-requires-full-two-pass"
  private[postgresql] val CountStageCostRequiresGlobalCountStarReason: String =
    "count-stage-cost-requires-certified-global-count-star"
  private[postgresql] val CountStageCostRequiresFullTwoPassReason: String =
    "count-stage-cost-requires-full-two-pass"
  private[postgresql] val GroupedCountRequiresFullTwoPassReason: String =
    "grouped-count-requires-full-two-pass"
  private[postgresql] val GroupedCountOptimizerPolicyUnsupportedReason: String =
    "grouped-count-optimizer-policy-unsupported"
  private[postgresql] val DistributedGroupedCountRequiresFullTwoPassReason: String =
    "distributed-grouped-count-requires-full-two-pass"
  private[postgresql] val DistributedGroupedCountOptimizerPolicyUnsupportedReason: String =
    "distributed-grouped-count-optimizer-policy-unsupported"
  private[postgresql] val C03G0RequiresFullTwoPassReason: String =
    "c03-g0-requires-full-two-pass"
  private[postgresql] val C03G0SelectionPolicyUnsupportedReason: String =
    "c03-g0-selection-policy-unsupported"
  private[postgresql] val C03G0OwnerUnavailableReason: String =
    "c03-g0-owner-unavailable"
  private[postgresql] val GuardedExtremumOwnerUnavailableReason: String =
    "guarded-extremum-owner-unavailable"

  /** Closed positive selection set for the opt-in C03-G0 capability. */
  private def isC03G0SelectionPolicy(
      policy: CandidatePlanSelection
  ): Boolean = policy match {
    case CandidatePlanSelection.StructuralHead |
        _: CandidatePlanSelection.StructuralRank |
        _: CandidatePlanSelection.ExactCandidateId |
        CandidatePlanSelection.PostgreSqlStaticShapeV1 => true
    case _ => false
  }

  private[postgresql] def aggregateRootRowGuardDegenerate(
      query: QueryIR,
      policy: CandidatePlanSelection
  ): Boolean =
    query != null && policy != null &&
      (query.resultShape.isInstanceOf[QueryResultShape.GlobalCountStar] ||
        query.resultShape
          .isInstanceOf[QueryResultShape.GlobalGuardedExtremum]) &&
      (policy == CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 ||
        policy ==
          CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ||
        policy ==
          CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1)

  /**
    * Applicability boundary for optimizer-backed selection over certified
    * aggregate roots.
    *
    * The guarded-total-cost and physical-portfolio contracts use the final
    * row estimate as an admissibility signal. That signal is degenerate for a
    * global aggregate and remains rejected. Factor-aware progressive
    * selection instead orders candidates by the estimated peak and cumulative
    * sizes of renderer-owned named stages before consulting final cost. It is
    * therefore informative for certified COUNT(*) frequency propagation, but
    * only under FullTwoPass where that aggregate lowering is active.
    * One-key grouped COUNT(*) has the same FullTwoPass requirement; its
    * rerooted factor terminal is admitted only by structural and progressive
    * stage-aware policies for now.
    *
    * Guarded extrema remain rejected: their direct EXISTS terminal can bypass
    * factor stages, so those stage estimates do not yet certify the executed
    * aggregate path.
    */
  private[postgresql] def aggregateSelectionInapplicability(
      query: QueryIR,
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode
  ): Option[String] = {
    require(query != null, "source QueryIR must not be null")
    require(policy != null, "candidate selection policy must not be null")
    require(reductionMode != null, "reduction mode must not be null")

    query.resultShape match {
      case _: QueryResultShape.C03G0AggregateVector
          if reductionMode != YannakakisReductionMode.FullTwoPass =>
        Some(C03G0RequiresFullTwoPassReason)
      case _: QueryResultShape.C03G0AggregateVector
          if !isC03G0SelectionPolicy(policy) =>
        Some(C03G0SelectionPolicyUnsupportedReason)
      case _: QueryResultShape.DistributedGroupedCountStar
          if reductionMode != YannakakisReductionMode.FullTwoPass =>
        Some(DistributedGroupedCountRequiresFullTwoPassReason)
      case _: QueryResultShape.DistributedGroupedCountStar
          if policy ==
            CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 ||
            policy ==
              CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ||
            policy ==
              CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 ||
            policy ==
              CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 =>
        Some(DistributedGroupedCountOptimizerPolicyUnsupportedReason)
      case _: QueryResultShape.GroupedCountStar
          if reductionMode != YannakakisReductionMode.FullTwoPass =>
        Some(GroupedCountRequiresFullTwoPassReason)
      case _: QueryResultShape.GroupedCountStar
          if policy ==
            CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 ||
            policy ==
              CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 =>
        Some(GroupedCountOptimizerPolicyUnsupportedReason)
      case _: QueryResultShape.GlobalCountStar
          if policy ==
            CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 =>
        if (reductionMode == YannakakisReductionMode.FullTwoPass) None
        else Some(CountStageCostRequiresFullTwoPassReason)
      case _: QueryResultShape.GlobalCountStar
          if policy ==
            CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 =>
        if (reductionMode == YannakakisReductionMode.FullTwoPass) None
        else Some(AggregateCountFactorAwareRequiresFullTwoPassReason)
      case _: QueryResultShape.GlobalCountStar |
          _: QueryResultShape.GlobalGuardedExtremum
          if aggregateRootRowGuardDegenerate(query, policy) =>
        Some(AggregateRootRowGuardDegenerateReason)
      case _
          if policy ==
            CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 =>
        Some(CountStageCostRequiresGlobalCountStarReason)
      case _ => None
    }
  }

  def select(
      query: QueryIR,
      input: CandidateSearchInput,
      found: CandidatePlanDecision.Found,
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      limits: PostgreSqlCandidateSelectionLimits,
      bagFactorLimits: BagFactorPlanningLimits,
      renderingLimits: PostgreSqlRenderingLimits,
      explainCostEvaluator: Option[PostgreSqlExplainCostEvaluator] = None,
      explainLimits: PostgreSqlExplainSelectionLimits =
        PostgreSqlExplainSelectionLimits.default,
      sourceSql: Option[String] = None,
      candidateResourceLimits: CandidateResourceLimits =
        CandidateResourceLimits.default,
      factorAwareLimits: PostgreSqlFactorAwareSelectionLimits =
        PostgreSqlFactorAwareSelectionLimits.default
  ): Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate] = {
    require(query != null, "source QueryIR must not be null")
    require(input != null, "candidate-search input must not be null")
    require(found != null, "found candidate decision must not be null")
    require(policy != null, "candidate selection policy must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    require(limits != null, "candidate selection limits must not be null")
    require(bagFactorLimits != null, "bag-factor limits must not be null")
    require(renderingLimits != null, "rendering limits must not be null")
    require(
      explainCostEvaluator != null && explainCostEvaluator.forall(_ != null),
      "EXPLAIN cost evaluator option must not be null or contain null"
    )
    require(explainLimits != null, "EXPLAIN selection limits must not be null")
    require(
      candidateResourceLimits != null,
      "candidate resource limits must not be null"
    )
    require(
      factorAwareLimits != null,
      "factor-aware selection limits must not be null"
    )
    require(
      sourceSql != null && sourceSql.forall(_ != null),
      "source SQL option must not be null or contain null"
    )

    aggregateSelectionInapplicability(query, policy, reductionMode).foreach {
      reason =>
      return Left(
        PostgreSqlCandidateSelectionFailure.Inapplicable(
          policy,
          query.resultShape.id,
          reason
        )
      )
    }

    validatePlans(found.plans).flatMap { plans =>
      val verifiedFactorPrefixes = if (
        policy ==
          CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ||
        policy ==
          CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
      ) {
        verifyProgressivePrefixes(input, found, candidateResourceLimits) match {
          case None =>
            return Left(
              factorContractFailure(
                policy,
                reductionMode,
                PostgreSqlExplainCostFailureCode.PrefixVerificationFailed,
                PostgreSqlExplainEvaluationStage.PrefixVerification,
                factorAwareLimits,
                Vector.empty
              )
            )
          case Some(value) => Some(value)
        }
      } else None
      targets(plans, policy, reductionMode) match {
        case Left(unavailable) => Left(unavailable)
        case Right(selectedTargets) =>
          evaluate(
            query,
            input,
            selectedTargets,
            policy,
            reductionMode,
            limits,
            bagFactorLimits,
            renderingLimits,
            explainCostEvaluator,
            explainLimits,
            sourceSql,
            factorAwareLimits,
            verifiedFactorPrefixes
          )
      }
    }
  }

  private[postgresql] def validatePlans(
      plans: Vector[RankedCandidatePlan]
  ): Either[
    PostgreSqlCandidateSelectionFailure.InvalidCandidateSet,
    Vector[RankedCandidatePlan]
  ] = {
    if (plans == null || plans.isEmpty) {
      Left(
        PostgreSqlCandidateSelectionFailure.InvalidCandidateSet(
          "Found candidate decision contains no plans"
        )
      )
    } else if (plans.exists(_ == null)) {
      Left(
        PostgreSqlCandidateSelectionFailure.InvalidCandidateSet(
          "Found candidate decision contains a null plan"
        )
      )
    } else if (plans.map(_.rank) != (1 to plans.size).toVector) {
      Left(
        PostgreSqlCandidateSelectionFailure.InvalidCandidateSet(
          "Retained candidate ranks are not unique, contiguous, and increasing"
        )
      )
    } else if (plans.exists(_.candidateId == null)) {
      Left(
        PostgreSqlCandidateSelectionFailure.InvalidCandidateSet(
          "Retained candidate contains no candidate id"
        )
      )
    } else if (
      plans.exists(plan =>
        plan.decomposition == null ||
          plan.candidateId != CandidatePlanId.from(plan.decomposition)
      )
    ) {
      Left(
        PostgreSqlCandidateSelectionFailure.InvalidCandidateSet(
          "Retained candidate id does not match its decomposition"
        )
      )
    } else if (plans.map(_.candidateId).distinct.size != plans.size) {
      Left(
        PostgreSqlCandidateSelectionFailure.InvalidCandidateSet(
          "Retained candidate ids are not unique"
        )
      )
    } else Right(plans)
  }

  private def targets(
      plans: Vector[RankedCandidatePlan],
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode
  ): Either[
    PostgreSqlCandidateSelectionFailure.Unavailable,
    Vector[RankedCandidatePlan]
  ] = policy match {
    case CandidatePlanSelection.StructuralHead => Right(Vector(plans.head))
    case CandidatePlanSelection.StructuralRank(rank) =>
      plans.find(_.rank == rank) match {
        case Some(plan) => Right(Vector(plan))
        case None =>
          Left(unavailable(policy, reductionMode, plans.size))
      }
    case CandidatePlanSelection.ExactCandidateId(candidateId) =>
      plans.find(_.candidateId == candidateId) match {
        case Some(plan) => Right(Vector(plan))
        case None =>
          Left(unavailable(policy, reductionMode, plans.size))
      }
    case CandidatePlanSelection.PostgreSqlStaticShapeV1 => Right(plans)
    case CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 => Right(plans)
    case CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 =>
      Right(plans)
    case CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 =>
      Right(plans)
    case CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 =>
      throw new IllegalArgumentException(
        "physical portfolio policy must use its separate selector"
      )
  }

  private def unavailable(
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      retainedCandidateCount: Int
  ): PostgreSqlCandidateSelectionFailure.Unavailable =
    PostgreSqlCandidateSelectionFailure.Unavailable(
      PostgreSqlCandidateSelectionDecision.Unavailable(
        policy,
        reductionMode,
        retainedCandidateCount
      )
    )

  private def evaluate(
      query: QueryIR,
      input: CandidateSearchInput,
      targets: Vector[RankedCandidatePlan],
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      limits: PostgreSqlCandidateSelectionLimits,
      bagFactorLimits: BagFactorPlanningLimits,
      renderingLimits: PostgreSqlRenderingLimits,
      explainCostEvaluator: Option[PostgreSqlExplainCostEvaluator],
      explainLimits: PostgreSqlExplainSelectionLimits,
      sourceSql: Option[String],
      factorAwareLimits: PostgreSqlFactorAwareSelectionLimits,
      verifiedFactorPrefixes: Option[
        Vector[PostgreSqlFactorAwareVerifiedPrefix]
      ]
  ): Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate] = {
    val factorAware =
      policy ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ||
        policy ==
          CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
    if (BigInt(targets.size) > BigInt(limits.maxEvaluatedCandidates)) {
      val plan = targets.head
      if (factorAware) {
        val failure = factorFailure(
          Some(plan),
          policy,
          reductionMode,
          None,
          Vector.empty,
          PostgreSqlExplainCostFailureCode.SessionResourceLimitExceeded,
          PostgreSqlExplainEvaluationStage.ResourceValidation,
          None,
          None,
          Vector.empty,
          Vector.empty,
          factorAwareLimits
        )
        return Left(
          PostgreSqlCandidateSelectionFailure.FactorAwareResourceLimitExceeded(
            plan,
            PostgreSqlCandidateSelectionResource.EvaluatedCandidates,
            targets.size,
            limits.maxEvaluatedCandidates,
            failure.report
          )
        )
      }
      return Left(
        resourceFailure(
          plan,
          policy,
          reductionMode,
          Vector.empty,
          PostgreSqlCandidateSelectionResource.EvaluatedCandidates,
          targets.size,
          limits.maxEvaluatedCandidates
        )
      )
    }
    if (factorAware && BigInt(targets.size) > BigInt(factorAwareLimits.maxCandidates)) {
      val plan = targets.head
      val failure = factorFailure(
        Some(plan),
        policy,
        reductionMode,
        None,
        Vector.empty,
        PostgreSqlExplainCostFailureCode.SessionResourceLimitExceeded,
        PostgreSqlExplainEvaluationStage.ResourceValidation,
        None,
        None,
        Vector.empty,
        Vector.empty,
        factorAwareLimits
      )
      return Left(
        PostgreSqlCandidateSelectionFailure.FactorAwareResourceLimitExceeded(
          plan,
          PostgreSqlCandidateSelectionResource.EvaluatedCandidates,
          BigInt(targets.size),
          factorAwareLimits.maxCandidates,
          failure.report
        )
      )
    }

    var completedValues = Vector.empty[PostgreSqlCandidateEvaluation]
    var renderedValues = Vector.empty[PostgreSqlRenderedCandidate]
    var best: Option[(
      PostgreSqlRenderedCandidate
    )] = None
    var cumulativeExpandedScans = BigInt(0)
    var cumulativeCtes = BigInt(0)
    var cumulativeRenderedTerms = BigInt(0)
    var cumulativeSqlCharacters = BigInt(0)
    var cumulativeStageProbes = BigInt(0)
    var cumulativeStageDiagnosticCharacters = BigInt(0)
    val iterator = targets.iterator

    while (iterator.hasNext) {
      val plan = iterator.next()
      val completedForFailure =
        if (
          policy == CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 ||
          factorAware
        )
          Vector.empty
        else completedValues
      val compiled = PostgreSqlCandidateCompiler.compileOne(
        query,
        input,
        plan,
        bagFactorLimits,
        renderingLimits,
        reductionMode,
        if (factorAware) Some(factorAwareLimits.stageDiagnosticLimits)
        else None
      ) match {
        case Left(
              PostgreSqlCandidateCompiler.Failure.BagFactorPlanningFailed(
                _,
                problems
              )
            ) =>
          if (factorAware) {
            val failure = factorFailure(
              Some(plan),
              policy,
              reductionMode,
              None,
              Vector.empty,
              PostgreSqlExplainCostFailureCode.FactorCandidatePlanningFailed,
              PostgreSqlExplainEvaluationStage.CandidatePlanning,
              None,
              None,
              Vector.empty,
              renderedValues.flatMap(renderedCandidateIdentity),
              factorAwareLimits
            )
            return Left(
              PostgreSqlCandidateSelectionFailure.FactorAwarePlanningFailed(
                plan,
                problems,
                failure.report
              )
            )
          }
          return Left(
            PostgreSqlCandidateSelectionFailure.BagFactorPlanningFailed(
              plan,
              problems,
              evaluationFailed(
                policy,
                reductionMode,
                completedForFailure,
                plan
              )
            )
          )
        case Left(
              PostgreSqlCandidateCompiler.Failure.Inapplicable(_, _, reason)
            ) =>
          return Left(
            PostgreSqlCandidateSelectionFailure.Inapplicable(
              policy,
              query.resultShape.id,
              reason
            )
          )
        case Left(
              PostgreSqlCandidateCompiler.Failure.RenderingFailed(
                _,
                bagFactorPlan,
                problems
              )
            ) =>
          if (factorAware) {
            val failure = factorFailure(
              Some(plan),
              policy,
              reductionMode,
              None,
              Vector.empty,
              PostgreSqlExplainCostFailureCode.FactorStageDiagnosticRenderingFailed,
              PostgreSqlExplainEvaluationStage.StageDiagnosticRendering,
              None,
              None,
              Vector.empty,
              renderedValues.flatMap(renderedCandidateIdentity),
              factorAwareLimits
            )
            return Left(
              PostgreSqlCandidateSelectionFailure.FactorAwareRenderingFailed(
                plan,
                bagFactorPlan,
                problems,
                failure.report
              )
            )
          }
          return Left(
            PostgreSqlCandidateSelectionFailure.RenderingFailed(
              plan,
              bagFactorPlan,
              problems,
              evaluationFailed(
                policy,
                reductionMode,
                completedForFailure,
                plan
              )
            )
          )
        case Right(value) => value
      }

      val bagFactorPlan = compiled.bagFactorPlan
      val rendered = compiled.renderedQuery
      val stageDiagnostics = compiled.stageDiagnostics

      val score = PostgreSqlStaticShapeScore.from(rendered, plan.rank)
      val evaluation = PostgreSqlCandidateEvaluation(
        plan.candidateId,
        plan.rank,
        reductionMode,
        PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
        PostgreSqlPhysicalVariantId.from(plan.candidateId, reductionMode),
        score,
        PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
          rendered.sql
        )
      )
      val renderedCandidate = PostgreSqlRenderedCandidate(
        plan,
        bagFactorPlan,
        rendered,
        evaluation,
        stageDiagnostics
      )

      cumulativeExpandedScans += score.expandedScanCount
      cumulativeCtes += score.cteCount
      cumulativeRenderedTerms += score.renderedTermCount
      cumulativeSqlCharacters += score.sqlCharacterCount
      stageDiagnostics.foreach { diagnostics =>
        cumulativeStageProbes += diagnostics.statements.size
        cumulativeStageDiagnosticCharacters += diagnostics.statements
          .foldLeft(BigInt(0))(_ + _.characterCount)
      }

      firstExceeded(
        cumulativeExpandedScans,
        cumulativeCtes,
        cumulativeRenderedTerms,
        cumulativeSqlCharacters,
        limits
      ) match {
        case Some((resource, required, limit)) =>
          if (factorAware) {
            val identities = boundedRenderedCandidateIdentities(
              renderedValues,
              renderedCandidate,
              factorAwareLimits
            )
            val failure = factorFailure(
              Some(plan),
              policy,
              reductionMode,
              None,
              Vector.empty,
              PostgreSqlExplainCostFailureCode.SessionResourceLimitExceeded,
              PostgreSqlExplainEvaluationStage.ResourceValidation,
              None,
              None,
              Vector.empty,
              identities,
              factorAwareLimits
            )
            return Left(
              PostgreSqlCandidateSelectionFailure.FactorAwareResourceLimitExceeded(
                plan,
                resource,
                required,
                limit,
                failure.report
              )
            )
          }
          return Left(
            resourceFailure(
              plan,
              policy,
              reductionMode,
              completedForFailure,
              resource,
              required,
              limit
            )
          )
        case None => ()
      }
      if (factorAware) {
        val factorExceeded = Vector(
          (
            PostgreSqlCandidateSelectionResource.CumulativeStageProbes,
            cumulativeStageProbes + 1,
            factorAwareLimits.maxExplainStatementsPerSession
          ),
          (
            PostgreSqlCandidateSelectionResource.CumulativeStageDiagnosticCharacters,
            cumulativeStageDiagnosticCharacters,
            factorAwareLimits.maxDiagnosticCharactersPerSession
          )
        ).collectFirst {
          case (resource, required, limit)
              if required > BigInt(limit) =>
            (resource, required, limit)
        }
        factorExceeded.foreach { case (resource, required, limit) =>
          val identities = renderedValues.flatMap(renderedCandidateIdentity)
          val failure = factorFailure(
            Some(plan),
            policy,
            reductionMode,
            None,
            Vector.empty,
            PostgreSqlExplainCostFailureCode.SessionResourceLimitExceeded,
            PostgreSqlExplainEvaluationStage.ResourceValidation,
            None,
            None,
            Vector.empty,
            identities,
            factorAwareLimits
          )
          return Left(
            PostgreSqlCandidateSelectionFailure.FactorAwareResourceLimitExceeded(
              plan,
              resource,
              required,
              limit,
              failure.report
            )
          )
        }
      }

      completedValues = completedValues :+ evaluation
      renderedValues = renderedValues :+ renderedCandidate
      best match {
        case None => best = Some(renderedCandidate)
        case Some(previous)
            if PostgreSqlStaticShapeScore.ordering.lt(
              evaluation.score,
              previous.evaluation.score
            ) =>
          best = Some(renderedCandidate)
        case _ => ()
      }
    }

    if (policy == CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1) {
      return selectByExplainCost(
        renderedValues,
        policy,
        reductionMode,
        explainCostEvaluator,
        explainLimits,
        sourceSql
      )
    }
    if (factorAware) {
      return selectByFactorAwareExplain(
        renderedValues,
        policy,
        reductionMode,
        explainCostEvaluator,
        sourceSql,
        factorAwareLimits,
        verifiedFactorPrefixes.getOrElse(
          return Left(
            factorContractFailure(
              policy,
              reductionMode,
              PostgreSqlExplainCostFailureCode.PrefixVerificationFailed,
              PostgreSqlExplainEvaluationStage.PrefixVerification,
              factorAwareLimits,
              Vector.empty
            )
          )
        )
      )
    }

    best match {
      case None =>
        Left(
          PostgreSqlCandidateSelectionFailure.InvalidCandidateSet(
            "Candidate selection received no evaluation targets"
          )
        )
      case Some(selected) =>
        val evaluation = selected.evaluation
        val decision = PostgreSqlCandidateSelectionDecision.Selected(
          policy,
          reductionMode,
          completedValues,
          evaluation.candidateId,
          evaluation.structuralRank,
          evaluation.physicalVariantId
        )
        Right(
          PostgreSqlSelectedCandidate(
            selected.plan,
            selected.bagFactorPlan,
            selected.renderedQuery,
            decision
          )
        )
    }
  }

  /** Owner-local aggregate terminals still require one certified owner. */
  private[postgresql] def aggregateOwnerInapplicability(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      reductionMode: YannakakisReductionMode
  ): Option[String] =
    PostgreSqlCandidateCompiler.aggregateOwnerInapplicability(
      query,
      input,
      plan,
      reductionMode
    )

  private def selectByExplainCost(
      rendered: Vector[PostgreSqlRenderedCandidate],
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      evaluator: Option[PostgreSqlExplainCostEvaluator],
      limits: PostgreSqlExplainSelectionLimits,
      sourceSql: Option[String]
  ): Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate] = {
    if (rendered.isEmpty) {
      return Left(
        PostgreSqlCandidateSelectionFailure.InvalidCandidateSet(
          "Candidate selection received no EXPLAIN evaluation targets"
        )
      )
    }

    val activeEvaluator = evaluator match {
      case Some(value) => value
      case None =>
        return Left(
          explainFailure(
            None,
            policy,
            reductionMode,
            None,
            Vector.empty,
            PostgreSqlExplainCostFailureCode.EvaluatorUnavailable,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
    }

    val originalSql = sourceSql.filter(_.nonEmpty) match {
      case Some(value) => value
      case None =>
        return Left(
          contractFailure(policy, reductionMode)
        )
    }
    val sourceStatement = PostgreSqlExplainSourceStatement(
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
        originalSql
      ),
      originalSql
    )

    val request = PostgreSqlExplainCostBatchRequest(
      sourceStatement,
      rendered.map { candidate =>
        val evaluation = candidate.evaluation
        PostgreSqlExplainCandidateStatement(
          evaluation.candidateId,
          evaluation.structuralRank,
          evaluation.reductionMode,
          evaluation.physicalVariantId,
          evaluation.renderedStatementSha256,
          candidate.renderedQuery.sql
        )
      }
    )

    val evaluated =
      try activeEvaluator.evaluate(request, limits)
      catch {
        case scala.util.control.NonFatal(_) =>
          return Left(
            contractFailure(policy, reductionMode)
          )
      }

    if (evaluated == null) {
      return Left(
        contractFailure(policy, reductionMode)
      )
    }

    evaluated match {
      case Left(null) =>
        Left(
          contractFailure(policy, reductionMode)
        )
      case Left(problem) =>
        validatedFailurePrefix(rendered, sourceStatement, problem) match {
          case None =>
            Left(contractFailure(policy, reductionMode))
          case Some((failed, source, completed)) =>
            Left(
              explainFailure(
                failed.map(_.plan),
                policy,
                reductionMode,
                source,
                completed,
                problem.code,
                problem.stage
              )
            )
        }
      case Right(null) =>
        Left(contractFailure(policy, reductionMode))
      case Right(batch) =>
        validatedSuccess(rendered, sourceStatement, batch) match {
          case None =>
            Left(contractFailure(policy, reductionMode))
          case Some((source, evaluations)) =>
            val admissible = evaluations.filter(
              _.rowGuardEvidence.exists(_.withinGuard)
            )
            if (admissible.isEmpty) {
              return Left(
                PostgreSqlCandidateSelectionFailure.NoAdmissibleEstimate(
                  PostgreSqlCandidateSelectionDecision.NoAdmissibleEstimate(
                    policy,
                    reductionMode,
                    evaluations,
                    source
                  )
                )
              )
            }
            val selectedEvaluation = admissible.min(
              PostgreSqlCandidateSelectionDecision.explainOrdering
            )
            val selected = rendered.find(candidate =>
              candidate.plan.candidateId == selectedEvaluation.candidateId &&
                candidate.plan.rank == selectedEvaluation.structuralRank
            ).get
            val decision = PostgreSqlCandidateSelectionDecision.Selected(
              policy,
              reductionMode,
              evaluations,
              selectedEvaluation.candidateId,
              selectedEvaluation.structuralRank,
              selectedEvaluation.physicalVariantId,
              Some(source)
            )
            Right(
              PostgreSqlSelectedCandidate(
                selected.plan,
                selected.bagFactorPlan,
                selected.renderedQuery,
                decision
              )
            )
        }
    }
  }

  private def selectByFactorAwareExplain(
      rendered: Vector[PostgreSqlRenderedCandidate],
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      evaluator: Option[PostgreSqlExplainCostEvaluator],
      sourceSql: Option[String],
      limits: PostgreSqlFactorAwareSelectionLimits,
      verifiedPrefixes: Vector[PostgreSqlFactorAwareVerifiedPrefix]
  ): Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate] = {
    if (rendered.isEmpty) {
      return Left(
        PostgreSqlCandidateSelectionFailure.InvalidCandidateSet(
          "Candidate selection received no factor-aware evaluation targets"
        )
      )
    }
    val renderedIdentities = rendered.map(renderedCandidateIdentity)
    if (renderedIdentities.exists(_.isEmpty)) {
      return Left(
        factorContractFailure(
          policy,
          reductionMode,
          PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
          PostgreSqlExplainEvaluationStage.Session,
          limits,
          Vector.empty
        )
      )
    }
    val safeRenderedIdentities = renderedIdentities.flatten

    def contract(
        code: PostgreSqlExplainCostFailureCode,
        stage: PostgreSqlExplainEvaluationStage
    ): PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed =
      factorContractFailure(
        policy,
        reductionMode,
        code,
        stage,
        limits,
        safeRenderedIdentities
      )
    val activeEvaluator = evaluator match {
      case Some(value) => value
      case None =>
        return Left(
          contract(
            PostgreSqlExplainCostFailureCode.EvaluatorUnavailable,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
    }
    val originalSql = sourceSql.filter(_.nonEmpty) match {
      case Some(value) => value
      case None =>
        return Left(
          contract(
            PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
    }
    val source = PostgreSqlExplainSourceStatement(
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(originalSql),
      originalSql
    )

    val requestedCandidates = rendered.map { candidate =>
      val diagnostics = candidate.stageDiagnostics match {
        case Some(value) => value
        case None =>
          return Left(
            contract(
              PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
              PostgreSqlExplainEvaluationStage.Session
            )
          )
      }
      val trace = diagnostics.renderedQuery.trace.stages
      val emitted = trace.filter(_.physicalForm.emitted)
      if (
        diagnostics.renderedQuery != candidate.renderedQuery ||
        diagnostics.statements.map(_.stageId) != emitted.map(_.id) ||
        diagnostics.statements.exists(statement =>
          statement == null || statement.sql == null ||
            statement.statementSha256 !=
              PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
                statement.sql
              )
        )
      ) {
        return Left(
          contract(
            PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
      }
      PostgreSqlFactorAwareExplainCandidate(
        candidate.plan.candidateId,
        candidate.plan.rank,
        reductionMode,
        candidate.evaluation.physicalVariantId,
        diagnostics.statements.map(statement =>
          PostgreSqlFactorAwareExplainStageStatement(
            statement.stageId,
            statement.statementSha256,
            statement.sql
          )
        )
      )
    }
    val request = PostgreSqlFactorAwareExplainBatchRequest(source, requestedCandidates)
    val evaluated =
      try activeEvaluator.evaluateFactorAware(request, limits)
      catch {
        case scala.util.control.NonFatal(_) =>
          return Left(
            contract(
              PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
              PostgreSqlExplainEvaluationStage.Session
            )
          )
      }
    if (evaluated == null) {
      return Left(
        contract(
          PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
          PostgreSqlExplainEvaluationStage.Session
        )
      )
    }

    evaluated match {
      case Left(null) =>
        Left(
          contract(
            PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
      case Left(problem) =>
        if (!PostgreSqlFactorAwareReplay.verifyFailurePrefix(request, problem)) {
          Left(
            contract(
              PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
              PostgreSqlExplainEvaluationStage.Session
            )
          )
        } else {
          val completed = completedFactorCandidatePrefix(
            rendered,
            request,
            problem.source,
            problem.completed,
            policy
          ).getOrElse(
            return Left(
              contract(
                PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
                PostgreSqlExplainEvaluationStage.Session
              )
            )
          )
          val failed = problem.failedStatementIndex.map(request.flattened)
          Left(
            factorFailure(
              failed.flatMap { case (candidate, _) =>
                rendered.find(_.plan.candidateId == candidate.candidateId)
              }.map(_.plan),
              policy,
              reductionMode,
              problem.source,
              completed,
              problem.code,
              problem.stage,
              failed.map(_._2.stageId),
              problem.failedStatementIndex,
              problem.completed,
              safeRenderedIdentities,
              limits
            )
          )
        }
      case Right(null) =>
        Left(
          contract(
            PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
      case Right(batch) =>
        PostgreSqlFactorAwareReplay.verify(request, batch) match {
          case None =>
            Left(
              contract(
                PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
                PostgreSqlExplainEvaluationStage.Session
              )
            )
          case Some(verified) =>
            try {
              val evaluations = rendered
                .zip(verified.observationsByCandidate)
                .map { case (candidate, observations) =>
                  factorEvaluatedCandidate(
                    candidate,
                    observations,
                    batch.provenance,
                    verified.source,
                    policy
                  )
                }
              if (evaluations.exists(_.isEmpty)) {
                return Left(
                  contract(
                    PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
                    PostgreSqlExplainEvaluationStage.Session
                  )
                )
              }
              val values = evaluations.flatten
              val factorCheckpoints =
                if (
                  policy ==
                    CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
                )
                  factorAwareCheckpoints(
                    values,
                    rendered.map(_.plan),
                    verified.source,
                    verifiedPrefixes,
                    limits
                  )
                else Some(Vector.empty)
              val countCheckpoints =
                if (
                  policy ==
                    CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
                )
                  countStageCostCheckpoints(
                    values,
                    rendered.map(_.plan),
                    verified.source,
                    verifiedPrefixes,
                    limits
                  )
                else Some(Vector.empty)
              if (factorCheckpoints.isEmpty || countCheckpoints.isEmpty)
                return Left(
                  contract(
                    PostgreSqlExplainCostFailureCode.PrefixVerificationFailed,
                    PostgreSqlExplainEvaluationStage.PrefixVerification
                  )
                )
              val admissible = values.filter(
                _.rowGuardEvidence.exists(_.withinGuard)
              )
              if (admissible.isEmpty) {
                Left(
                  PostgreSqlCandidateSelectionFailure.NoAdmissibleEstimate(
                    PostgreSqlCandidateSelectionDecision.NoAdmissibleEstimate(
                      policy = policy,
                      reductionMode = reductionMode,
                      evaluations = values,
                      source = verified.source,
                      factorAwareCheckpoints = factorCheckpoints.get,
                      factorAwareSelectionLimits =
                        if (
                          policy ==
                            CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
                        ) Some(limits)
                        else None,
                      countStageCostCheckpoints =
                        countCheckpoints.get,
                      countStageCostSelectionLimits =
                        if (
                          policy ==
                            CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
                        ) Some(limits)
                        else None
                    )
                  )
                )
              } else {
                val selectedEvaluation =
                  if (
                    policy ==
                      CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
                  )
                    admissible.min(
                      PostgreSqlCandidateSelectionDecision.countStageCostOrdering
                    )
                  else
                    admissible.min(
                      PostgreSqlCandidateSelectionDecision.factorAwareOrdering
                    )
                val selected = rendered.find(candidate =>
                  candidate.plan.candidateId == selectedEvaluation.candidateId &&
                    candidate.plan.rank == selectedEvaluation.structuralRank
                ).getOrElse(
                  return Left(
                    contract(
                      PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
                      PostgreSqlExplainEvaluationStage.Session
                    )
                  )
                )
                val decision = PostgreSqlCandidateSelectionDecision.Selected(
                  policy = policy,
                  reductionMode = reductionMode,
                  evaluations = values,
                  selectedCandidateId = selectedEvaluation.candidateId,
                  selectedStructuralRank = selectedEvaluation.structuralRank,
                  selectedPhysicalVariantId =
                    selectedEvaluation.physicalVariantId,
                  sourceEvaluation = Some(verified.source),
                  factorAwareCheckpoints = factorCheckpoints.get,
                  factorAwareSelectionLimits =
                    if (
                      policy ==
                        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
                    ) Some(limits)
                    else None,
                  countStageCostCheckpoints = countCheckpoints.get,
                  countStageCostSelectionLimits =
                    if (
                      policy ==
                        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
                    ) Some(limits)
                    else None
                )
                Right(
                  PostgreSqlSelectedCandidate(
                    selected.plan,
                    selected.bagFactorPlan,
                    selected.renderedQuery,
                    decision
                  )
                )
              }
            } catch {
              case scala.util.control.NonFatal(_) =>
                Left(
                  contract(
                    PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
                    PostgreSqlExplainEvaluationStage.Session
                  )
                )
            }
        }
    }
  }

  private def factorEvaluatedCandidate(
      rendered: PostgreSqlRenderedCandidate,
      observations: Vector[PostgreSqlFactorAwareExplainObservation],
      provenance: PostgreSqlExplainCostProvenance,
      source: PostgreSqlExplainSourceEvaluation,
      policy: CandidatePlanSelection
  ): Option[PostgreSqlCandidateEvaluation] =
    try
      factorEvaluatedCandidateUnsafe(
        rendered,
        observations,
        provenance,
        source,
        policy
      )
    catch { case scala.util.control.NonFatal(_) => None }

  private def factorEvaluatedCandidateUnsafe(
      rendered: PostgreSqlRenderedCandidate,
      observations: Vector[PostgreSqlFactorAwareExplainObservation],
      provenance: PostgreSqlExplainCostProvenance,
      source: PostgreSqlExplainSourceEvaluation,
      policy: CandidatePlanSelection
  ): Option[PostgreSqlCandidateEvaluation] = {
    val diagnostics = rendered.stageDiagnostics.getOrElse(return None)
    val trace = diagnostics.renderedQuery.trace.stages
    val emitted = trace.filter(_.physicalForm.emitted)
    if (
      observations == null || observations.size != emitted.size ||
      diagnostics.statements.size != emitted.size ||
      observations.zip(diagnostics.statements).zip(emitted).exists {
        case ((observation, statement), stage) =>
          observation == null || observation.estimate == null ||
          observation.physicalVariantId != rendered.evaluation.physicalVariantId ||
          observation.stageId != stage.id || statement.stageId != stage.id ||
          observation.statementSha256 != statement.statementSha256 ||
          observation.estimate.provenance != provenance
      }
    ) return None

    val emittedEvidence = emitted
      .zip(diagnostics.statements)
      .zip(observations)
      .map { case ((stage, statement), observation) =>
        stage.id -> PostgreSqlFactorAwareStageEvaluation(
          stage,
          Some(statement.statementSha256),
          Some(statement.characterCount),
          Some(statement.ctePrefixCount),
          Some(observation.estimate)
        )
      }
      .toMap
    val stages = trace.map { stage =>
      if (stage.physicalForm.emitted) emittedEvidence(stage.id)
      else
        PostgreSqlFactorAwareStageEvaluation(
          stage,
          None,
          None,
          None,
          None
        )
    }
    val named = stages.collect {
      case stage if stage.trace.physicalForm.namedCte =>
        val estimate = stage.estimate.get
        PostgreSqlFactorAwareScore.exactEstimatedBytes(
          estimate.planRows,
          estimate.planWidth
        )
    }
    val finalEstimate = stages.last.estimate.get
    val factorScore = PostgreSqlFactorAwareScore(
      named.foldLeft(BigDecimal(0))(_.max(_)),
      PostgreSqlFactorAwareScore.exactSum(named),
      finalEstimate.planTotalCost,
      rendered.plan.rank,
      rendered.plan.candidateId
    )
    val common = rendered.evaluation.copy(
      explainCostEstimate = Some(finalEstimate),
      rowGuardEvidence = Some(
        PostgreSqlExplainRowGuardEvidence.from(
          source.estimate.planRows,
          finalEstimate.planRows
        )
      )
    )
    policy match {
      case CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 =>
        val evidence = PostgreSqlFactorAwareCandidateEvidence(
          rendered.evaluation.physicalVariantId,
          stages,
          factorScore,
          PostgreSqlFactorAwareCandidateEvidence.StageIdScheme,
          PostgreSqlFactorAwareCandidateEvidence.StageOrderScheme,
          PostgreSqlFactorAwareCandidateEvidence.StatementHashScheme,
          PostgreSqlFactorAwareCandidateEvidence.CharacterCountScheme
        )
        Some(common.copy(factorAwareEvidence = Some(evidence)))
      case CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 =>
        val evidence = PostgreSqlCountStageCostCandidateEvidence(
          rendered.evaluation.physicalVariantId,
          stages,
          PostgreSqlCountStageCostScore.from(
            stages,
            rendered.plan.rank,
            rendered.plan.candidateId
          ),
          PostgreSqlCountStageCostCandidateEvidence.StageIdScheme,
          PostgreSqlCountStageCostCandidateEvidence.StageOrderScheme,
          PostgreSqlCountStageCostCandidateEvidence.StatementHashScheme,
          PostgreSqlCountStageCostCandidateEvidence.CharacterCountScheme
        )
        Some(common.copy(countStageCostEvidence = Some(evidence)))
      case _ => None
    }
  }

  private def completedFactorCandidatePrefix(
      rendered: Vector[PostgreSqlRenderedCandidate],
      request: PostgreSqlFactorAwareExplainBatchRequest,
      source: Option[PostgreSqlExplainSourceEvaluation],
      observations: Vector[PostgreSqlFactorAwareExplainObservation],
      policy: CandidatePlanSelection
  ): Option[Vector[PostgreSqlCandidateEvaluation]] = {
    if (observations.isEmpty) return Some(Vector.empty)
    val baseline = source.getOrElse(return None)
    val provenance = baseline.estimate.provenance
    var offset = 0
    val completed = Vector.newBuilder[PostgreSqlCandidateEvaluation]
    var index = 0
    while (index < request.candidates.size) {
      val size = request.candidates(index).stages.size
      if (offset + size <= observations.size) {
        factorEvaluatedCandidate(
          rendered(index),
          observations.slice(offset, offset + size),
          provenance,
          baseline,
          policy
        ) match {
          case Some(value) => completed += value
          case None        => return None
        }
        offset += size
      } else return Some(completed.result())
      index += 1
    }
    Some(completed.result())
  }

  private def factorAwareCheckpoints(
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      plans: Vector[RankedCandidatePlan],
      source: PostgreSqlExplainSourceEvaluation,
      verifiedPrefixes: Vector[PostgreSqlFactorAwareVerifiedPrefix],
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Option[Vector[PostgreSqlFactorAwareCheckpoint]] =
    try {
      if (
        verifiedPrefixes == null ||
        verifiedPrefixes.map(_.requestedPrefix) !=
          PostgreSqlFactorAwareProgressiveV1.Checkpoints
      ) return None
      val checkpoints = verifiedPrefixes.map { verified =>
        val requested = verified.requestedPrefix
        val effective = math.min(requested, evaluations.size)
        if (
          verified.effectiveCandidateCount != effective ||
          verified.truncatedBySearchExhaustion != (effective < requested) ||
          verified.verifiedPrefixSha256 !=
            PostgreSqlFactorAwarePrefixProof.digest(plans.take(effective))
        ) return None
        val admissible = evaluations.take(effective).filter(
          _.rowGuardEvidence.exists(_.withinGuard)
        )
        val winner =
          if (admissible.isEmpty) None
          else
            Some(
              admissible.min(
                PostgreSqlCandidateSelectionDecision.factorAwareOrdering
              )
            )
        PostgreSqlFactorAwareCheckpoint(
          requested,
          effective,
          effective < requested,
          winner.map(_.structuralRank),
          winner.map(_.candidateId),
          winner.map(_.physicalVariantId),
          winner.flatMap(_.factorAwareEvidence.map(_.score)),
          verified.verifiedPrefixSha256,
          PostgreSqlFactorAwarePrefixProof.Scheme
        )
      }
      if (
        PostgreSqlFactorAwareSelectionReplay.verify(
          plans,
          evaluations,
          source,
          checkpoints,
          limits,
          None
        )
      ) Some(checkpoints)
      else None
    } catch { case scala.util.control.NonFatal(_) => None }

  private def countStageCostCheckpoints(
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      plans: Vector[RankedCandidatePlan],
      source: PostgreSqlExplainSourceEvaluation,
      verifiedPrefixes: Vector[PostgreSqlFactorAwareVerifiedPrefix],
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Option[Vector[PostgreSqlCountStageCostCheckpoint]] =
    try {
      if (
        verifiedPrefixes == null ||
        verifiedPrefixes.map(_.requestedPrefix) !=
          PostgreSqlCountStageCostProgressiveV1.Checkpoints
      ) return None
      val checkpoints = verifiedPrefixes.map { verified =>
        val requested = verified.requestedPrefix
        val effective = math.min(requested, evaluations.size)
        if (
          verified.effectiveCandidateCount != effective ||
          verified.truncatedBySearchExhaustion != (effective < requested) ||
          verified.verifiedPrefixSha256 !=
            PostgreSqlFactorAwarePrefixProof.digest(plans.take(effective))
        ) return None
        val admissible = evaluations.take(effective).filter(
          _.rowGuardEvidence.exists(_.withinGuard)
        )
        val winner =
          if (admissible.isEmpty) None
          else
            Some(
              admissible.min(
                PostgreSqlCandidateSelectionDecision.countStageCostOrdering
              )
            )
        PostgreSqlCountStageCostCheckpoint(
          requested,
          effective,
          effective < requested,
          winner.map(_.structuralRank),
          winner.map(_.candidateId),
          winner.map(_.physicalVariantId),
          winner.flatMap(_.countStageCostEvidence.map(_.score)),
          verified.verifiedPrefixSha256,
          PostgreSqlFactorAwarePrefixProof.Scheme
        )
      }
      if (
        PostgreSqlCountStageCostSelectionReplay.verify(
          plans,
          evaluations,
          source,
          checkpoints,
          limits,
          None
        )
      ) Some(checkpoints)
      else None
    } catch { case scala.util.control.NonFatal(_) => None }

  private def validatedSuccess(
      rendered: Vector[PostgreSqlRenderedCandidate],
      expectedSource: PostgreSqlExplainSourceStatement,
      batch: PostgreSqlExplainCostBatch
  ): Option[(PostgreSqlExplainSourceEvaluation, Vector[PostgreSqlCandidateEvaluation])] = {
    if (
      batch.provenance == null || batch.source == null ||
      batch.source.sourceStatementSha256 != expectedSource.sourceStatementSha256 ||
      batch.source.estimate == null ||
      batch.source.estimate.provenance != batch.provenance ||
      batch.observations == null || batch.observations.size != rendered.size
    ) None
    else {
      val evaluations = rendered.zip(batch.observations).map {
        case (candidate, observation) =>
          evaluatedCandidate(
            candidate,
            observation,
            batch.provenance,
            batch.source
          )
      }
      if (evaluations.forall(_.nonEmpty))
        Some(batch.source -> evaluations.flatten)
      else None
    }
  }

  private def validatedFailurePrefix(
      rendered: Vector[PostgreSqlRenderedCandidate],
      expectedSource: PostgreSqlExplainSourceStatement,
      problem: PostgreSqlExplainCostEvaluationFailure
  ): Option[(
    Option[PostgreSqlRenderedCandidate],
    Option[PostgreSqlExplainSourceEvaluation],
    Vector[PostgreSqlCandidateEvaluation]
  )] = {
    val failed = problem.failedCandidateIndex.flatMap(rendered.lift)
    val stageContractValid =
      ((problem.stage == PostgreSqlExplainEvaluationStage.CandidateExplain) ==
        problem.failedCandidateIndex.nonEmpty) &&
        (problem.stage != PostgreSqlExplainEvaluationStage.CandidateExplain ||
          (problem.source.nonEmpty &&
            problem.failedCandidateIndex.contains(problem.completed.size))) &&
        (problem.stage != PostgreSqlExplainEvaluationStage.SourceExplain ||
          (problem.source.isEmpty && problem.completed.isEmpty))
    val sourceValid = problem.source.forall(source =>
      source.sourceStatementSha256 == expectedSource.sourceStatementSha256 &&
        source.estimate != null
    )
    if (
      problem.code == null || problem.stage == null ||
      problem.failedCandidateIndex.exists(index =>
        index < 0 || index >= rendered.size
      ) || problem.completed == null || !stageContractValid || !sourceValid ||
      problem.completed.size > rendered.size
    ) None
    else {
      val provenance = problem.source.map(_.estimate.provenance)
      val completed = rendered
        .take(problem.completed.size)
        .zip(problem.completed)
        .map { case (candidate, observation) =>
          for {
            value <- provenance
            source <- problem.source
            evaluated <- evaluatedCandidate(
              candidate,
              observation,
              value,
              source
            )
          } yield evaluated
        }
      if (
        completed.forall(_.nonEmpty) &&
        (problem.completed.isEmpty || problem.source.nonEmpty)
      ) Some((failed, problem.source, completed.flatten))
      else None
    }
  }

  private def evaluatedCandidate(
      rendered: PostgreSqlRenderedCandidate,
      observation: PostgreSqlExplainCostObservation,
      provenance: PostgreSqlExplainCostProvenance,
      source: PostgreSqlExplainSourceEvaluation
  ): Option[PostgreSqlCandidateEvaluation] = {
    val expected = rendered.evaluation
    if (
      observation == null || observation.candidateId != expected.candidateId ||
      observation.structuralRank != expected.structuralRank ||
      observation.physicalVariantId != expected.physicalVariantId ||
      observation.renderedStatementSha256 !=
        expected.renderedStatementSha256 || observation.estimate == null ||
      observation.estimate.provenance != provenance
    ) None
    else
      Some(
        expected.copy(
          explainCostEstimate = Some(observation.estimate),
          rowGuardEvidence = Some(
            PostgreSqlExplainRowGuardEvidence.from(
              source.estimate.planRows,
              observation.estimate.planRows
            )
          )
        )
      )
  }

  private def explainFailure(
      plan: Option[RankedCandidatePlan],
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      source: Option[PostgreSqlExplainSourceEvaluation],
      completed: Vector[PostgreSqlCandidateEvaluation],
      code: PostgreSqlExplainCostFailureCode,
      stage: PostgreSqlExplainEvaluationStage
  ): PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed =
    PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed(
      plan,
      code,
      PostgreSqlCandidateSelectionDecision.ExplainEvaluationFailed(
        policy,
        reductionMode,
        completed,
        source,
        stage,
        plan.map(_.candidateId),
        plan.map(_.rank),
        plan.map(candidate =>
          PostgreSqlPhysicalVariantId.from(candidate.candidateId, reductionMode)
        )
      )
    )

  private def contractFailure(
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode
  ): PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed =
    explainFailure(
      None,
      policy,
      reductionMode,
      None,
      Vector.empty,
      PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
      PostgreSqlExplainEvaluationStage.Session
    )

  private def verifyProgressivePrefixes(
      input: CandidateSearchInput,
      full: CandidatePlanDecision.Found,
      limits: CandidateResourceLimits
  ): Option[Vector[PostgreSqlFactorAwareVerifiedPrefix]] = {
    val reruns = Vector.newBuilder[(Int, CandidatePlanDecision)]
    val checkpoints = PostgreSqlFactorAwareProgressiveV1.Checkpoints.iterator
    try {
      while (checkpoints.hasNext) {
        val checkpoint = checkpoints.next()
        TopKCandidateTD.enumerate(input, checkpoint, limits) match {
          case Left(_)       => return None
          case Right(value)  => reruns += checkpoint -> value
        }
      }
    } catch {
      case scala.util.control.NonFatal(_) => return None
    }
    PostgreSqlFactorAwarePrefixVerifier.verify(full, reruns.result())
  }

  private def renderedCandidateIdentity(
      candidate: PostgreSqlRenderedCandidate
  ): Option[PostgreSqlFactorAwareRenderedCandidateIdentity] = {
    if (candidate == null || candidate.evaluation == null) return None
    val diagnostics = candidate.stageDiagnostics.getOrElse(return None)
    if (
      diagnostics == null || diagnostics.statements == null ||
      diagnostics.statements.isEmpty || diagnostics.statements.exists(_ == null)
    ) return None
    Some(
      PostgreSqlFactorAwareRenderedCandidateIdentity(
        candidate.plan.candidateId,
        candidate.plan.rank,
        candidate.evaluation.physicalVariantId,
        candidate.evaluation.renderedStatementSha256,
        diagnostics.statements.map(statement =>
          PostgreSqlFactorAwareRenderedStageIdentity(
            statement.stageId,
            statement.statementSha256,
            statement.characterCount,
            statement.ctePrefixCount
          )
        )
      )
    )
  }

  /**
    * Resource-validation failures retain the current rendered identity only
    * when the resulting diagnostic batch still fits the frozen factor
    * envelope. If the current candidate is the one that crosses that
    * envelope, the exact prior rendered prefix remains replayable.
    */
  private def boundedRenderedCandidateIdentities(
      prior: Vector[PostgreSqlRenderedCandidate],
      current: PostgreSqlRenderedCandidate,
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Vector[PostgreSqlFactorAwareRenderedCandidateIdentity] = {
    val priorIdentities = prior.flatMap(renderedCandidateIdentity)
    val allIdentities = priorIdentities ++ renderedCandidateIdentity(current)
    val usages = allIdentities.map(identity =>
      PostgreSqlFactorAwareDiagnosticReplay.renderedUsage(identity, limits)
    )
    if (
      allIdentities.size == prior.size + 1 && usages.forall(_.nonEmpty) &&
      PostgreSqlFactorAwareDiagnosticReplay.withinSession(
        usages.flatten,
        limits
      )
    ) allIdentities
    else priorIdentities
  }

  private def factorFailure(
      plan: Option[RankedCandidatePlan],
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      source: Option[PostgreSqlExplainSourceEvaluation],
      completed: Vector[PostgreSqlCandidateEvaluation],
      code: PostgreSqlExplainCostFailureCode,
      stage: PostgreSqlExplainEvaluationStage,
      failedStageId: Option[PostgreSqlStageId],
      failedStageObservationIndex: Option[Int],
      completedStageObservations: Vector[
        PostgreSqlFactorAwareExplainObservation
      ],
      renderedCandidates: Vector[
        PostgreSqlFactorAwareRenderedCandidateIdentity
      ],
      limits: PostgreSqlFactorAwareSelectionLimits
  ): PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed =
    PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed(
      plan,
      code,
      PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed(
        policy,
        reductionMode,
        completed,
        source,
        stage,
        code,
        plan.map(_.candidateId),
        plan.map(_.rank),
        plan.map(candidate =>
          PostgreSqlPhysicalVariantId.from(candidate.candidateId, reductionMode)
        ),
        failedStageId,
        failedStageObservationIndex,
        completedStageObservations,
        renderedCandidates,
        Some(limits)
      )
    )

  private def factorContractFailure(
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      code: PostgreSqlExplainCostFailureCode,
      stage: PostgreSqlExplainEvaluationStage,
      limits: PostgreSqlFactorAwareSelectionLimits,
      renderedCandidates: Vector[
        PostgreSqlFactorAwareRenderedCandidateIdentity
      ]
  ): PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed =
    factorFailure(
      None,
      policy,
      reductionMode,
      None,
      Vector.empty,
      code,
      stage,
      None,
      None,
      Vector.empty,
      renderedCandidates,
      limits
    )

  private def evaluationFailed(
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      completed: Vector[PostgreSqlCandidateEvaluation],
      failed: RankedCandidatePlan
  ): PostgreSqlCandidateSelectionDecision.EvaluationFailed =
    PostgreSqlCandidateSelectionDecision.EvaluationFailed(
      policy,
      reductionMode,
      completed,
      failed.candidateId,
      failed.rank,
      PostgreSqlPhysicalVariantId.from(failed.candidateId, reductionMode)
    )

  private def resourceFailure(
      plan: RankedCandidatePlan,
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      completed: Vector[PostgreSqlCandidateEvaluation],
      resource: PostgreSqlCandidateSelectionResource,
      required: BigInt,
      limit: Long
  ): PostgreSqlCandidateSelectionFailure.ResourceLimitExceeded =
    PostgreSqlCandidateSelectionFailure.ResourceLimitExceeded(
      plan,
      resource,
      required,
      limit,
      evaluationFailed(policy, reductionMode, completed, plan)
    )

  private def firstExceeded(
      expandedScans: BigInt,
      ctes: BigInt,
      renderedTerms: BigInt,
      sqlCharacters: BigInt,
      limits: PostgreSqlCandidateSelectionLimits
  ): Option[(PostgreSqlCandidateSelectionResource, BigInt, Long)] = {
    val values = Vector(
      (
        PostgreSqlCandidateSelectionResource.CumulativeExpandedScans,
        expandedScans,
        limits.maxCumulativeExpandedScans
      ),
      (
        PostgreSqlCandidateSelectionResource.CumulativeCtes,
        ctes,
        limits.maxCumulativeCtes
      ),
      (
        PostgreSqlCandidateSelectionResource.CumulativeRenderedTerms,
        renderedTerms,
        limits.maxCumulativeRenderedTerms
      ),
      (
        PostgreSqlCandidateSelectionResource.CumulativeSqlCharacters,
        sqlCharacters,
        limits.maxCumulativeSqlCharacters
      )
    )
    values.collectFirst {
      case (resource, required, limit) if required > BigInt(limit) =>
        (resource, required, limit)
    }
  }

}
