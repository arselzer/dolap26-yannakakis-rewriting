package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanId,
  CandidatePlanDecision,
  CandidateResourceLimits,
  CandidateTopKStatistics,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  BagFactorPlanningLimits,
  ConfiguredBagFactorPlanningError
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlBagSupportStrategy,
  PostgreSqlRenderingError,
  PostgreSqlRenderingLimits,
  PostgreSqlStagePhysicalForm,
  PostgreSqlStageTrace
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{
  AdmissibleSoftBagGeneration,
  SoftBagGeneration
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteDiagnostic,
  RewriteOutcome,
  RewriteRequest
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlRewriteArtifact,
  PostgreSqlRewriteReport,
  PostgreSqlRewriteResult,
  PostgreSqlCandidateEvaluation,
  PostgreSqlCandidateSelectionDecision,
  PostgreSqlExplainCostEstimate,
  PostgreSqlExplainCostProvenance,
  PostgreSqlExplainRowGuardEvidence,
  PostgreSqlExplainSourceEvaluation,
  PostgreSqlFactorAwareCandidateEvidence,
  PostgreSqlFactorAwareCheckpoint,
  PostgreSqlFactorAwareExplainObservation,
  PostgreSqlFactorAwareFailureReplay,
  PostgreSqlFactorAwareNumericEnvelope,
  PostgreSqlFactorAwareProgressiveV1,
  PostgreSqlFactorAwareRenderedCandidateIdentity,
  PostgreSqlFactorAwareScore,
  PostgreSqlFactorAwareSelectionReplay,
  PostgreSqlFactorAwareSelectionLimits,
  PostgreSqlFactorAwareStageEvaluation,
  PostgreSqlPhysicalVariantId,
  PostgreSqlStaticShapeScore
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql._
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.AnalysisReport

/** Explicit, versioned JSON projection of the public detailed result.
  *
  * This deliberately does not derive writers for the algorithm models. The
  * CLI schema can therefore remain stable when internal case classes evolve,
  * and only a small audited subset of metadata can reach standard output.
  */
private[cli] object PostgreSqlRewriteJson {
  val SchemaVersion: String = "yannakakis-postgresql-rewrite/v4"
  val FactorAwareSchemaVersion: String = "yannakakis-postgresql-rewrite/v5"
  val PhysicalPortfolioSchemaVersion: String =
    "yannakakis-postgresql-rewrite/v6"
  val CountStageCostSchemaVersion: String =
    "yannakakis-postgresql-rewrite/v7"
  val CoverPolicy: String = "first_canonical_admissible_cover_v1"

  def render(
      request: RewriteRequest,
      result: PostgreSqlRewriteResult
  ): String = {
    require(request != null, "JSON request must not be null")
    require(result != null, "JSON result must not be null")
    require(
      request.originalSql == result.outcome.originalSql,
      "JSON result must preserve the request SQL"
    )
    val physicalPortfolio =
      request.options.candidateSelection ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
    if (physicalPortfolio)
      return renderPhysicalPortfolio(request, result)
    require(
      result.report.candidateSelectionDecision.forall(
        _.reductionMode == request.options.reductionMode
      ),
      "JSON result must preserve the requested reduction schedule"
    )
    require(
      result.report.candidateSelectionDecision.forall(
        _.policy == request.options.candidateSelection
      ) && result.report.artifact.forall(
        _.candidateSelection.policy == request.options.candidateSelection
      ),
      "JSON result must preserve the requested candidate-selection policy"
    )

    val exitCode = PostgreSqlRewriteExitCode.forOutcome(result.outcome)
    val factorAware =
      request.options.candidateSelection ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
    val countStageCost =
      request.options.candidateSelection ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
    if (factorAware) {
      result.report.candidateSelectionDecision.foreach {
        case failure: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed =>
          require(
            PostgreSqlFactorAwareFailureReplay.verify(
              result.report.candidateDecision.collect {
                case found: CandidatePlanDecision.Found => found.plans
              }.getOrElse(
                throw new IllegalArgumentException(
                  "factor-aware failure serialization requires retained plans"
                )
              ),
              failure,
              Some(sha256(request.originalSql))
            ),
            "factor-aware failure evidence must pass pure replay before serialization"
          )
        case decision: PostgreSqlCandidateSelectionDecision.Selected =>
          val plans = result.report.candidateDecision.collect {
            case found: CandidatePlanDecision.Found => found.plans
          }.getOrElse(
            throw new IllegalArgumentException(
              "factor-aware selection serialization requires retained plans"
            )
          )
          require(
            PostgreSqlFactorAwareSelectionReplay.verify(
              plans,
              decision.evaluations,
              decision.sourceEvaluation.orNull,
              decision.factorAwareCheckpoints,
              decision.factorAwareSelectionLimits.orNull,
              Some(sha256(request.originalSql))
            ),
            "factor-aware selection evidence must pass pure replay before serialization"
          )
        case decision: PostgreSqlCandidateSelectionDecision.NoAdmissibleEstimate =>
          val plans = result.report.candidateDecision.collect {
            case found: CandidatePlanDecision.Found => found.plans
          }.getOrElse(
            throw new IllegalArgumentException(
              "factor-aware no-admissible serialization requires retained plans"
            )
          )
          require(
            PostgreSqlFactorAwareSelectionReplay.verify(
              plans,
              decision.evaluations,
              decision.source,
              decision.factorAwareCheckpoints,
              decision.factorAwareSelectionLimits.orNull,
              Some(sha256(request.originalSql))
            ),
            "factor-aware no-admissible evidence must pass pure replay before serialization"
          )
        case _ =>
          throw new IllegalArgumentException(
            "factor-aware serialization received an incompatible selection decision"
          )
      }
    }
    if (countStageCost) {
      require(
        request.options.planLimit ==
          PostgreSqlCountStageCostProgressiveV1.RequiredPlanLimit &&
          request.options.reductionMode ==
            at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode.FullTwoPass,
        "v7 COUNT stage-cost request must use exact K=100 and FullTwoPass"
      )
      result.report.candidateSelectionDecision.foreach {
        case failure: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed =>
          require(
            PostgreSqlCountStageCostFailureReplay.verify(
              result.report.candidateDecision.collect {
                case found: CandidatePlanDecision.Found => found.plans
              }.getOrElse(
                throw new IllegalArgumentException(
                  "COUNT stage-cost failure serialization requires retained plans"
                )
              ),
              failure,
              Some(sha256(request.originalSql))
            ),
            "COUNT stage-cost failure evidence must pass pure replay before serialization"
          )
        case decision: PostgreSqlCandidateSelectionDecision.Selected =>
          val plans = result.report.candidateDecision.collect {
            case found: CandidatePlanDecision.Found => found.plans
          }.getOrElse(
            throw new IllegalArgumentException(
              "COUNT stage-cost selection serialization requires retained plans"
            )
          )
          require(
            PostgreSqlCountStageCostSelectionReplay.verify(
              plans,
              decision.evaluations,
              decision.sourceEvaluation.orNull,
              decision.countStageCostCheckpoints,
              decision.countStageCostSelectionLimits.orNull,
              Some(sha256(request.originalSql))
            ),
            "COUNT stage-cost selection evidence must pass pure replay before serialization"
          )
        case decision: PostgreSqlCandidateSelectionDecision.NoAdmissibleEstimate =>
          val plans = result.report.candidateDecision.collect {
            case found: CandidatePlanDecision.Found => found.plans
          }.getOrElse(
            throw new IllegalArgumentException(
              "COUNT stage-cost no-admissible serialization requires retained plans"
            )
          )
          require(
            PostgreSqlCountStageCostSelectionReplay.verify(
              plans,
              decision.evaluations,
              decision.source,
              decision.countStageCostCheckpoints,
              decision.countStageCostSelectionLimits.orNull,
              Some(sha256(request.originalSql))
            ),
            "COUNT stage-cost no-admissible evidence must pass pure replay before serialization"
          )
        case _ =>
          throw new IllegalArgumentException(
            "COUNT stage-cost serialization received an incompatible selection decision"
          )
      }
    }
    Json.render(
      Json.obj(
        "schema_version" -> Json.string(
          if (factorAware) FactorAwareSchemaVersion
          else if (countStageCost) CountStageCostSchemaVersion
          else SchemaVersion
        ),
        "status" -> Json.string(outcomeStatus(result.outcome)),
        "exit_code" -> Json.number(exitCode),
        "request" -> requestJson(request),
        "outcome" -> outcomeJson(result.outcome),
        "report" -> reportJson(result.report, factorAware, countStageCost)
      )
    )
  }

  private def renderPhysicalPortfolio(
      request: RewriteRequest,
      result: PostgreSqlRewriteResult
  ): String = {
    require(
      request.options.planLimit ==
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit &&
        request.options.reductionMode ==
          at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode.FullTwoPass,
      "v6 physical portfolio request must use K=100 and the neutral full sentinel"
    )
    val attempt = result.report.physicalPortfolioAttempt
    require(
      result.report.candidateSelectionDecision.isEmpty &&
        result.report.artifact.isEmpty && result.report.bagFactorPlan.isEmpty,
      "v6 physical portfolio must not inhabit legacy selection evidence"
    )
    if (attempt.isEmpty) {
      val completedOrdinal =
        PostgreSqlRewritePhase.values.indexOf(result.report.completedPhase)
      val candidateSelectionOrdinal = PostgreSqlRewritePhase.values.indexOf(
        PostgreSqlRewritePhase.CandidateSelectionCompleted
      )
      val candidateDecisionMatchesPhase =
        if (
          result.report.completedPhase ==
            PostgreSqlRewritePhase.CandidateSearchCompleted
        )
          result.report.candidateDecision.exists(
            _.isInstanceOf[CandidatePlanDecision.Rejected]
          )
        else result.report.candidateDecision.isEmpty
      require(
        result.report.physicalPortfolioArtifact.isEmpty &&
          completedOrdinal >= 0 &&
          completedOrdinal < candidateSelectionOrdinal &&
          candidateDecisionMatchesPhase,
        "v6 report without a physical attempt must stop before candidate selection and bind the exact rejected-search phase"
      )
    }
    attempt.foreach { value =>
      require(value.sourceSql == request.originalSql)
      require(
        PostgreSqlPhysicalPortfolioSelectionReplay.verifyAttempt(value),
        "v6 physical portfolio attempt must exactly regenerate before serialization"
      )
      require(
        value.decision.policyId == request.options.candidateSelection.id &&
          result.report.candidateDecision.contains(value.candidateSearch)
      )
      require(
        PostgreSqlPhysicalPortfolioOutcome.verify(
          request.originalSql,
          value.decision,
          result.report.physicalPortfolioArtifact.map(_.renderedQuery.sql),
          result.outcome
        ),
        "v6 terminal outcome must exactly match its physical portfolio decision"
      )
      value.decision match {
        case _: PostgreSqlPhysicalPortfolioDecision.Selected =>
          require(
            result.outcome.isInstanceOf[RewriteOutcome.Rewritten],
            "v6 selected decision requires a rewritten outcome"
          )
        case _: PostgreSqlPhysicalPortfolioDecision.NoAdmissible |
            _: PostgreSqlPhysicalPortfolioDecision.Failed =>
          require(
            result.outcome.isInstanceOf[RewriteOutcome.Failed],
            "v6 non-selected terminal decision requires a failed outcome"
          )
      }
    }
    require(
      result.report.physicalPortfolioArtifact.forall(artifact =>
        attempt.contains(artifact.attempt) &&
          artifact.attempt.decision
            .isInstanceOf[PostgreSqlPhysicalPortfolioDecision.Selected]
      )
    )
    require(
      result.outcome.isInstanceOf[RewriteOutcome.Rewritten] ==
        result.report.physicalPortfolioArtifact.nonEmpty,
      "v6 rewritten status must match the selected physical artifact"
    )
    (result.outcome, result.report.physicalPortfolioArtifact) match {
      case (rewritten: RewriteOutcome.Rewritten, Some(artifact)) =>
        require(
          rewritten.rewrittenStatements == Vector(artifact.renderedQuery.sql),
          "v6 rewritten statement must exactly match the selected physical rendering"
        )
      case (_: RewriteOutcome.Rewritten, None) | (_, Some(_)) =>
        throw new IllegalArgumentException(
          "v6 rewritten status must exactly match one selected physical rendering"
        )
      case _ => ()
    }
    Json.render(
      Json.obj(
        "schema_version" -> Json.string(PhysicalPortfolioSchemaVersion),
        "status" -> Json.string(outcomeStatus(result.outcome)),
        "exit_code" -> Json.number(
          PostgreSqlRewriteExitCode.forOutcome(result.outcome)
        ),
        "request" -> physicalPortfolioRequestJson(request),
        "outcome" -> outcomeJson(result.outcome),
        "report" -> physicalPortfolioReportJson(result.report)
      )
    )
  }

  private def physicalPortfolioRequestJson(
      request: RewriteRequest
  ): Json.Value =
    Json.obj(
      "target_dialect" -> Json.string(request.targetDialect.id),
      "options" -> Json.obj(
        "width_bound" -> Json.number(request.options.widthBound),
        "plan_limit" -> Json.number(request.options.planLimit),
        "require_connected_cover" -> Json.bool(
          request.options.requireConnectedCover
        ),
        "candidate_selection" -> requestedSelectionJson(
          request.options.candidateSelection
        ),
        "neutral_reduction_schedule" -> Json.string(
          request.options.reductionMode.id
        ),
        "portfolio_reduction_schedule_order" -> Json.array(
          PostgreSqlExplainFactorAwarePhysicalPortfolioV1.ReductionModeOrder
            .map(mode => Json.string(mode.id))
        ),
        "portfolio_materialization_directive_order" -> Json.array(
          PostgreSqlExplainFactorAwarePhysicalPortfolioV1.MaterializationDirectiveOrder
            .map(value => Json.string(value.id))
        ),
        "materialization_scope" -> Json.string(
          PostgreSqlCteMaterializationDirective.Scope
        )
      ),
      "original_sql" -> Json.string(request.originalSql),
      "original_sql_sha256" -> Json.string(sha256(request.originalSql))
    )

  private def physicalPortfolioReportJson(
      report: PostgreSqlRewriteReport
  ): Json.Value =
    Json.obj(
      "completed_phase" -> Json.string(report.completedPhase.id),
      "analysis" -> optional(report.analysis)(analysisJson),
      "hypergraph" -> optional(report.hypergraph) { hypergraph =>
        Json.obj(
          "edge_count" -> Json.number(hypergraph.edges.size),
          "variable_count" -> Json.number(hypergraph.variables.size),
          "column_count" -> Json.number(hypergraph.columns.size)
        )
      },
      "soft_generation" -> optional(report.softGeneration)(
        physicalPortfolioSoftGenerationJson
      ),
      "admissible_generation" -> optional(report.admissibleGeneration)(
        physicalPortfolioAdmissibleGenerationJson
      ),
      "candidate_input" -> optional(report.candidateInput) { input =>
        Json.obj(
          "width_bound" -> Json.number(input.widthBound),
          "require_connected_cover" -> Json.bool(
            input.requireConnectedCover
          ),
          "candidate_bag_count" -> Json.number(input.candidateBags.size)
        )
      },
      "candidate_search" -> optional(report.candidateDecision)(
        physicalPortfolioCandidateDecisionJson
      ),
      "physical_portfolio" -> optional(report.physicalPortfolioAttempt)(
        physicalPortfolioAttemptJson
      ),
      "selected_plan" -> optional(report.physicalPortfolioArtifact)(
        physicalPortfolioSelectedPlanJson
      ),
      "bag_factor_plan" -> optional(report.physicalPortfolioArtifact)(
        artifact => bagFactorSummary(artifact.configuredPlan.plan)
      ),
      "rendering" -> optional(report.physicalPortfolioArtifact)(
        physicalPortfolioRenderingJson
      )
    )

  private def physicalPortfolioAttemptJson(
      attempt: PostgreSqlPhysicalPortfolioAttempt
  ): Json.Value =
    Json.obj(
      "policy" -> Json.string(attempt.decision.policyId),
      "selection_scope" -> Json.string(attempt.decision.scope),
      "global_optimality_claim" -> Json.bool(
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.GlobalOptimality
      ),
      "requested_plan_limit" -> Json.number(
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit
      ),
      "structural_search_exhausted_before_limit" -> Json.bool(
        attempt.structuralSearchExhaustedBeforeLimit
      ),
      "enumeration_scheme" -> Json.string(
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.EnumerationScheme
      ),
      "physical_variant_id_scheme" -> Json.string(
        PostgreSqlPhysicalVariantId.SchemeV2
      ),
      "workload_identity_scheme" -> Json.string(
        PostgreSqlPhysicalPortfolioWorkloadId.Scheme
      ),
      "deduplication_scheme" -> Json.string(
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.DeduplicationScheme
      ),
      "candidate_resource_limits" -> candidateResourceLimitsJson(
        attempt.candidateResourceLimits
      ),
      "physical_portfolio_limits" -> physicalPortfolioLimitsJson(
        attempt.physicalPortfolioLimits
      ),
      "rendering_limits" -> renderingLimitsJson(attempt.renderingLimits),
      "bag_factor_planning_limits" -> bagFactorPlanningLimitsJson(
        attempt.bagFactorPlanningLimits
      ),
      "evaluation_limits" -> physicalPortfolioEvaluationLimitsJson(
        attempt.evaluationLimits
      ),
      "decision" -> physicalPortfolioDecisionJson(attempt.decision)
    )

  private def physicalPortfolioSelectedPlanJson(
      artifact: PostgreSqlPhysicalPortfolioArtifact
  ): Json.Value = {
    val selected = artifact.attempt.decision match {
      case value: PostgreSqlPhysicalPortfolioDecision.Selected => value
      case _ =>
        throw new IllegalArgumentException(
          "v6 selected-plan serialization requires a selected decision"
        )
    }
    val entry = selected.enumeration.entries
      .find(
        _.variant.physicalVariantId == selected.selectedPhysicalVariantId
      )
      .getOrElse(
        throw new IllegalArgumentException(
          "v6 selected physical variant is absent from its enumeration"
        )
      )
    Json.obj(
      "selection_policy" -> Json.string(selected.policyId),
      "selection_scope" -> Json.string(selected.scope),
      "cost_selected" -> Json.bool(true),
      "estimated_cost_selected" -> Json.bool(true),
      "measured_runtime_selected" -> Json.bool(false),
      "uses_catalog_statistics" -> Json.bool(true),
      "uses_execution_feedback" -> Json.bool(false),
      "structural_plan" -> physicalPortfolioCandidatePlanJson(
        artifact.selectedPlan
      ),
      "physical_variant" -> physicalVariantJson(entry.variant),
      "enumeration_ordinal" -> Json.number(entry.enumerationOrdinal),
      "enumeration_phase" -> Json.string(entry.phase),
      "enumeration_lane" -> optional(entry.lane)(Json.string),
      "selected_workload_id" -> Json.string(
        selected.selectedWorkloadId.value
      )
    )
  }

  private def physicalPortfolioRenderingJson(
      artifact: PostgreSqlPhysicalPortfolioArtifact
  ): Json.Value = {
    val rendered = artifact.renderedQuery
    val statistics = rendered.statistics
    val reduction = rendered.trace.reduction
    val selected = artifact.attempt.decision match {
      case value: PostgreSqlPhysicalPortfolioDecision.Selected => value
      case _ =>
        throw new IllegalArgumentException(
          "v6 rendering serialization requires a selected decision"
        )
    }
    val entry = selected.enumeration.entries
      .find(_.variant.physicalVariantId == selected.selectedPhysicalVariantId)
      .get
    Json.obj(
      "rendered_statement_sha256" -> Json.string(sha256(rendered.sql)),
      "output_column_count" -> Json.number(rendered.output.size),
      "expanded_scan_count" -> Json.number(statistics.expandedScanCount),
      "materialized_distinct_support_count" -> Json.number(
        rendered.trace.bags.count(
          _.supportStrategy ==
            PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
        )
      ),
      "rendered_term_count" -> Json.number(statistics.renderedTermCount),
      "cte_count" -> Json.number(statistics.cteCount),
      "sql_character_count" -> Json.number(statistics.sqlCharacterCount),
      "reduction_schedule" -> Json.string(reduction.mode.id),
      "reduction_order" -> Json.string(reduction.reductionOrder),
      "materialization_directive" -> Json.string(
        entry.variant.materialization.id
      ),
      "materialization_scope" -> Json.string(
        entry.variant.materializationScope
      ),
      "canonical_bottom_up_step_count" -> Json.number(
        reduction.canonicalBottomUpStepCount
      ),
      "canonical_top_down_step_count" -> Json.number(
        reduction.canonicalTopDownStepCount
      ),
      "selected_bottom_up_step_count" -> Json.number(
        reduction.selectedBottomUpStepCount
      ),
      "selected_top_down_step_count" -> Json.number(
        reduction.selectedTopDownStepCount
      ),
      "physical_reduction_cte_count" -> Json.number(
        reduction.physicalReductionCteCount
      ),
      "stages" -> Json.array(rendered.trace.stages.map(physicalStageTraceJson))
    )
  }

  private def physicalPortfolioDecisionJson(
      decision: PostgreSqlPhysicalPortfolioDecision
  ): Json.Value = decision match {
    case selected: PostgreSqlPhysicalPortfolioDecision.Selected =>
      Json.obj(
        physicalPortfolioDecisionBase(
          "selected",
          selected.enumeration,
          selected.rawRenderings,
          Some(selected.deduplication),
          Some(selected.explainRequest),
          selected.evaluationLimits,
          Some(selected.evaluationUsage),
          None,
          None,
          Some(selected.explainBatch),
          selected.evaluations
        ) ++ Vector(
          "selected_workload_id" -> Json.string(
            selected.selectedWorkloadId.value
          ),
          "selected_physical_variant_id" -> Json.string(
            selected.selectedPhysicalVariantId.value
          )
        )
      )
    case noAdmissible: PostgreSqlPhysicalPortfolioDecision.NoAdmissible =>
      Json.obj(
        physicalPortfolioDecisionBase(
          "no_admissible",
          noAdmissible.enumeration,
          noAdmissible.rawRenderings,
          Some(noAdmissible.deduplication),
          Some(noAdmissible.explainRequest),
          noAdmissible.evaluationLimits,
          Some(noAdmissible.evaluationUsage),
          None,
          None,
          Some(noAdmissible.explainBatch),
          noAdmissible.evaluations
        )
      )
    case failed: PostgreSqlPhysicalPortfolioDecision.Failed =>
      val enumerationFields = failed.enumeration match {
        case Some(value) =>
          Vector("enumeration" -> physicalPortfolioEnumerationJson(value))
        case None => Vector("enumeration" -> Json.Null)
      }
      val collisionFields =
        PostgreSqlPhysicalPortfolioCollisionProjection.from(failed).toVector
          .map(value => "collision_witness" -> collisionProjectionJson(value))
      Json.obj(
        Vector(
          "status" -> Json.string("failed"),
          "policy" -> Json.string(failed.policyId),
          "selection_scope" -> Json.string(failed.scope),
          "failure_kind" -> Json.string(failed.failureKind.id),
          "failure_stage" -> Json.string(failed.failureKind.stage.id),
          "detail_code" -> Json.string(failed.detailCode),
          "failed_enumeration_ordinal" -> optional(
            failed.failedEnumerationOrdinal
          )(Json.number),
          "enumeration_failure" -> optional(failed.enumerationFailure)(
            physicalPortfolioEnumerationFailureJson
          ),
          "planning_problems" -> Json.array(
            failed.planningProblems.map(configuredPlanningProblemJson)
          ),
          "rendering_problems" -> Json.array(
            failed.renderingProblems.map(renderingProblemJson)
          )
        ) ++ enumerationFields ++ Vector(
          "raw_render_prefix" -> Json.array(
            failed.rawRenderPrefix.map(physicalRawRenderingJson)
          ),
          "deduplication" -> optional(failed.deduplication)(
            physicalDeduplicationJson
          ),
          "explain_request" -> optional(failed.explainRequest)(
            physicalExplainRequestJson
          ),
          "evaluation_limits" -> physicalPortfolioEvaluationLimitsJson(
            failed.evaluationLimits
          ),
          "evaluation_usage" -> optional(failed.evaluationUsage)(
            physicalEvaluationUsageJson
          ),
          "resource_exceeded" -> optional(failed.resourceExceeded)(
            physicalEvaluationResourceExceededJson
          ),
          "explain_failure" -> optional(failed.explainFailure)(
            physicalExplainFailureJson
          ),
          "explain_batch" -> optional(failed.explainBatch)(
            physicalExplainBatchJson
          ),
          "completed_evaluations" -> Json.array(
            failed.completedEvaluations.map(physicalEvaluationJson)
          )
        ) ++ collisionFields
      )
  }

  private def physicalPortfolioDecisionBase(
      status: String,
      enumeration: PostgreSqlPhysicalPortfolioEnumeration,
      rawRenderings: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      deduplication: Option[PostgreSqlPhysicalPortfolioDeduplicationEvidence],
      explainRequest: Option[PostgreSqlPhysicalPortfolioExplainBatchRequest],
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      evaluationUsage: Option[PostgreSqlPhysicalPortfolioEvaluationUsage],
      resourceExceeded: Option[
        PostgreSqlPhysicalPortfolioEvaluationResourceExceeded
      ],
      explainFailure: Option[PostgreSqlPhysicalPortfolioExplainFailure],
      explainBatch: Option[PostgreSqlPhysicalPortfolioExplainBatch],
      evaluations: Vector[PostgreSqlPhysicalPortfolioEvaluationEvidence]
  ): Vector[(String, Json.Value)] =
    Vector(
      "status" -> Json.string(status),
      "policy" -> Json.string(
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.PolicyId
      ),
      "selection_scope" -> Json.string(
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.Scope
      ),
      "score_order" -> Json.array(
        PostgreSqlPhysicalPortfolioEvaluationV1.ScoreOrder.map(Json.string)
      ),
      "enumeration" -> physicalPortfolioEnumerationJson(enumeration),
      "raw_renderings" -> Json.array(
        rawRenderings.map(physicalRawRenderingJson)
      ),
      "deduplication" -> optional(deduplication)(physicalDeduplicationJson),
      "explain_request" -> optional(explainRequest)(physicalExplainRequestJson),
      "evaluation_limits" -> physicalPortfolioEvaluationLimitsJson(
        evaluationLimits
      ),
      "evaluation_usage" -> optional(evaluationUsage)(
        physicalEvaluationUsageJson
      ),
      "resource_exceeded" -> optional(resourceExceeded)(
        physicalEvaluationResourceExceededJson
      ),
      "explain_failure" -> optional(explainFailure)(physicalExplainFailureJson),
      "explain_batch" -> optional(explainBatch)(physicalExplainBatchJson),
      "evaluations" -> Json.array(evaluations.map(physicalEvaluationJson))
    )

  private def physicalPortfolioSoftGenerationJson(
      generation: SoftBagGeneration
  ): Json.Value = {
    val witnessCount = generation.candidates.iterator
      .map(candidate => BigInt(candidate.witnesses.size))
      .sum
    val physicalCoverCount = generation.candidates.iterator
      .map(candidate => BigInt(candidate.covers.size))
      .sum
    Json.obj(
      "width_bound" -> Json.number(generation.widthBound),
      "enumerated_cover_count" -> Json.number(generation.enumeratedCovers.size),
      "candidate_count" -> Json.number(generation.candidates.size),
      "witness_count" -> canonicalBigIntJson(witnessCount),
      "physical_cover_count" -> canonicalBigIntJson(physicalCoverCount),
      "connected_physical_cover_count" -> Json.number(
        generation.candidates.iterator.flatMap(_.covers).count(_.isConnected)
      )
    )
  }

  private def physicalPortfolioAdmissibleGenerationJson(
      generation: AdmissibleSoftBagGeneration
  ): Json.Value = {
    val witnessCount = generation.candidates.iterator
      .map(candidate => BigInt(candidate.witnesses.size))
      .sum
    val physicalCoverCount = generation.candidates.iterator
      .map(candidate => BigInt(candidate.physicalCovers.size))
      .sum
    Json.obj(
      "width_bound" -> Json.number(generation.widthBound),
      "require_connected_cover" -> Json.bool(
        generation.requireConnectedCover
      ),
      "definition_cover_count" -> Json.number(
        generation.definitionCovers.size
      ),
      "candidate_count" -> Json.number(generation.candidates.size),
      "witness_count" -> canonicalBigIntJson(witnessCount),
      "physical_cover_count" -> canonicalBigIntJson(physicalCoverCount),
      "connected_physical_cover_count" -> Json.number(
        generation.candidates.iterator
          .flatMap(_.physicalCovers)
          .count(_.isConnected)
      )
    )
  }

  private def physicalPortfolioCandidateDecisionJson(
      decision: CandidatePlanDecision
  ): Json.Value = decision match {
    case found: CandidatePlanDecision.Found =>
      val plans = found.plans.sortBy(_.rank)
      Json.obj(
        "decision" -> Json.string("found"),
        "candidate_id_scheme" -> Json.string(CandidatePlanId.Scheme),
        "requested_limit" -> Json.number(found.requestedLimit),
        "returned_plan_count" -> Json.number(plans.size),
        "statistics" -> candidateStatisticsJson(found.statistics),
        "plans" -> Json.array(plans.map(physicalPortfolioCandidatePlanJson))
      )
    case rejected: CandidatePlanDecision.Rejected =>
      Json.obj(
        "decision" -> Json.string("rejected"),
        "candidate_id_scheme" -> Json.string(CandidatePlanId.Scheme),
        "requested_limit" -> Json.number(rejected.requestedLimit),
        "returned_plan_count" -> Json.number(0),
        "statistics" -> candidateStatisticsJson(rejected.statistics),
        "plans" -> Json.array(Vector.empty)
      )
  }

  private def physicalPortfolioCandidatePlanJson(
      plan: RankedCandidatePlan
  ): Json.Value =
    Json.obj(
      "rank" -> Json.number(plan.rank),
      "candidate_id" -> Json.string(plan.candidateId.value),
      "structural_score" -> Json.obj(
        "node_count" -> canonicalBigIntJson(plan.score.nodeCount)
      ),
      "cover_selection_policy" -> Json.string(CoverPolicy),
      "certificate" -> Json.obj(
        "bag_count" -> Json.number(plan.certificate.bagCount),
        "component_arc_count" -> Json.number(
          plan.certificate.componentArcs.size
        ),
        "require_connected_cover" -> Json.bool(
          plan.certificate.requireConnectedCover
        )
      ),
      "decomposition" -> decompositionJson(plan.decomposition)
    )

  private def candidateResourceLimitsJson(
      limits: CandidateResourceLimits
  ): Json.Value =
    Json.obj(
      "max_blocks" -> Json.number(limits.maxBlocks),
      "max_basis_checks" -> Json.number(limits.maxBasisChecks),
      "max_basis_alternatives" -> Json.number(limits.maxBasisAlternatives),
      "max_witness_nodes" -> Json.number(limits.maxWitnessNodes),
      "max_plan_limit" -> Json.number(limits.maxPlanLimit),
      "max_retained_partial_plans" -> Json.number(
        limits.maxRetainedPartialPlans
      ),
      "max_combination_states" -> Json.number(limits.maxCombinationStates)
    )

  private def physicalPortfolioLimitsJson(
      limits: PostgreSqlPhysicalPortfolioLimits
  ): Json.Value =
    Json.obj(
      "max_structural_candidates" -> Json.number(
        limits.maxStructuralCandidates
      ),
      "max_roots_per_candidate" -> Json.number(limits.maxRootsPerCandidate),
      "max_covers_per_bag" -> Json.number(limits.maxCoversPerBag),
      "max_cover_states_per_candidate" -> Json.number(
        limits.maxCoverStatesPerCandidate
      ),
      "max_variants_per_candidate" -> Json.number(
        limits.maxVariantsPerCandidate
      ),
      "max_raw_variants_per_session" -> Json.number(
        limits.maxRawVariantsPerSession
      ),
      "max_unique_workloads_per_session" -> Json.number(
        limits.maxUniqueWorkloadsPerSession
      )
    )

  private def renderingLimitsJson(
      limits: PostgreSqlRenderingLimits
  ): Json.Value =
    Json.obj(
      "max_expanded_scans" -> Json.number(limits.maxExpandedScans),
      "max_rendered_terms" -> Json.number(limits.maxRenderedTerms),
      "max_ctes" -> Json.number(limits.maxCtes),
      "max_sql_characters" -> Json.number(limits.maxSqlCharacters)
    )

  private def bagFactorPlanningLimitsJson(
      limits: BagFactorPlanningLimits
  ): Json.Value =
    Json.obj(
      "max_bags" -> Json.number(limits.maxBags),
      "max_ownership_checks" -> Json.number(limits.maxOwnershipChecks),
      "max_tree_edges" -> Json.number(limits.maxTreeEdges),
      "max_source_edges" -> Json.number(limits.maxSourceEdges)
    )

  private def physicalPortfolioEvaluationLimitsJson(
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits
  ): Json.Value =
    Json.obj(
      "max_workloads" -> Json.number(limits.maxWorkloads),
      "max_stage_probes_per_workload" -> Json.number(
        limits.maxStageProbesPerWorkload
      ),
      "max_characters_per_probe" -> Json.number(
        limits.maxCharactersPerProbe
      ),
      "max_diagnostic_characters_per_workload" -> Json.number(
        limits.maxDiagnosticCharactersPerWorkload
      ),
      "max_raw_diagnostic_characters_retained" -> Json.number(
        limits.maxRawDiagnosticCharactersRetained
      ),
      "max_explain_statements_per_session" -> Json.number(
        limits.maxExplainStatementsPerSession
      ),
      "max_diagnostic_characters_per_session" -> Json.number(
        limits.maxDiagnosticCharactersPerSession
      ),
      "max_plan_json_characters" -> Json.number(
        limits.maxPlanJsonCharacters
      ),
      "max_cumulative_plan_json_characters" -> Json.number(
        limits.maxCumulativePlanJsonCharacters
      ),
      "statement_timeout_seconds" -> Json.number(
        limits.statementTimeoutSeconds
      ),
      "explain_statement_budget_seconds" -> Json.number(
        limits.explainStatementBudgetSeconds
      )
    )

  private def physicalPortfolioEnumerationJson(
      enumeration: PostgreSqlPhysicalPortfolioEnumeration
  ): Json.Value =
    Json.obj(
      "enumeration_scheme" -> Json.string(
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.EnumerationScheme
      ),
      "exact_logical_variant_count" -> canonicalBigIntJson(
        enumeration.exactLogicalVariantCount
      ),
      "cross_product_reserve_target" -> Json.number(
        enumeration.crossProductReserveTarget
      ),
      "global_wave_search_exhausted" -> Json.bool(
        enumeration.globalWaveSearchExhausted
      ),
      "limits" -> physicalPortfolioLimitsJson(enumeration.limits),
      "candidates" -> Json.array(
        enumeration.candidates.map(physicalCandidateEnumerationJson)
      ),
      "entries" -> Json.array(
        enumeration.entries.map(physicalPortfolioEntryJson)
      )
    )

  private def physicalCandidateEnumerationJson(
      evidence: PostgreSqlPhysicalCandidateEnumerationEvidence
  ): Json.Value =
    Json.obj(
      "candidate_id" -> Json.string(evidence.candidateId.value),
      "structural_rank" -> Json.number(evidence.structuralRank),
      "exact_root_count" -> canonicalBigIntJson(evidence.exactRootCount),
      "exact_cover_assignment_count" -> canonicalBigIntJson(
        evidence.exactCoverAssignmentCount
      ),
      "exact_logical_variant_count" -> canonicalBigIntJson(
        evidence.exactLogicalVariantCount
      ),
      "applicable_axes" -> Json.array(
        evidence.applicableAxes.map(Json.string)
      ),
      "retained_cover_state_count" -> Json.number(
        evidence.retainedCoverStateCount
      ),
      "cover_state_search_exhausted" -> Json.bool(
        evidence.coverStateSearchExhausted
      ),
      "retained_local_variant_count" -> Json.number(
        evidence.retainedLocalVariantCount
      ),
      "retained_cross_product_state_count" -> Json.number(
        evidence.retainedCrossProductStateCount
      ),
      "local_variant_search_exhausted" -> Json.bool(
        evidence.localVariantSearchExhausted
      )
    )

  private def physicalPortfolioEntryJson(
      entry: PostgreSqlPhysicalPortfolioEntry
  ): Json.Value =
    Json.obj(
      "enumeration_ordinal" -> Json.number(entry.enumerationOrdinal),
      "phase" -> Json.string(entry.phase),
      "lane" -> optional(entry.lane)(Json.string),
      "variant" -> physicalVariantJson(entry.variant),
      "heap_key" -> physicalHeapKeyJson(entry.heapKey)
    )

  private def physicalVariantJson(
      variant: PostgreSqlPhysicalVariant
  ): Json.Value =
    Json.obj(
      "candidate_id" -> Json.string(variant.candidateId.value),
      "structural_rank" -> Json.number(variant.structuralRank),
      "physical_variant_id" -> Json.string(variant.physicalVariantId.value),
      "physical_variant_id_scheme" -> Json.string(
        PostgreSqlPhysicalVariantId.SchemeV2
      ),
      "local_ordinal" -> Json.number(variant.localOrdinal),
      "execution_root_bag_id" -> Json.number(
        variant.configuration.executionRoot.value
      ),
      "cover_assignments" -> Json.array(
        variant.configuration.coverAssignments.map { assignment =>
          Json.obj(
            "bag_id" -> Json.number(assignment.bagId.value),
            "edge_ids" -> Json.array(
              assignment.edgeIds.map(edge => Json.number(edge.value))
            )
          )
        }
      ),
      "reduction_schedule" -> Json.string(variant.reductionMode.id),
      "reduction_order" -> Json.string(variant.reductionOrder),
      "materialization_directive" -> Json.string(
        variant.materialization.id
      ),
      "materialization_scope" -> Json.string(variant.materializationScope)
    )

  private def physicalHeapKeyJson(
      key: PostgreSqlPhysicalVariantHeapKey
  ): Json.Value =
    Json.obj(
      "deviation_magnitude" -> canonicalBigIntJson(key.deviationMagnitude),
      "root_ordinal" -> Json.number(key.rootOrdinal),
      "cover_index_vector" -> Json.array(
        key.coverIndexVector.map(Json.number)
      ),
      "mode_ordinal" -> Json.number(key.modeOrdinal),
      "directive_ordinal" -> Json.number(key.directiveOrdinal),
      "physical_variant_id" -> Json.string(key.physicalVariantId.value)
    )

  private def physicalRawRenderingJson(
      evidence: PostgreSqlPhysicalPortfolioRawRenderingEvidence
  ): Json.Value =
    Json.obj(
      "entry" -> physicalPortfolioEntryJson(evidence.entry),
      "workload" -> physicalWorkloadDescriptorJson(evidence.workload)
    )

  private def physicalWorkloadDescriptorJson(
      descriptor: PostgreSqlPhysicalPortfolioWorkloadDescriptor
  ): Json.Value =
    Json.obj(
      "workload_id" -> Json.string(descriptor.workloadId.value),
      "workload_identity_scheme" -> Json.string(
        descriptor.workloadIdentityScheme
      ),
      "stage_id_scheme" -> Json.string(descriptor.stageIdScheme),
      "stage_order_scheme" -> Json.string(descriptor.stageOrderScheme),
      "statement_hash_scheme" -> Json.string(descriptor.statementHashScheme),
      "character_count_scheme" -> Json.string(
        descriptor.characterCountScheme
      ),
      "rendered_statement_sha256" -> Json.string(
        descriptor.renderedStatementSha256
      ),
      "rendered_character_count" -> Json.number(
        descriptor.renderedCharacterCount
      ),
      "emitted_stage_count" -> Json.number(descriptor.emittedStageCount),
      "materialization_directive" -> Json.string(
        descriptor.materialization.id
      ),
      "materialization_scope" -> Json.string(
        descriptor.materializationScope
      ),
      "stages" -> Json.array(
        descriptor.stages.map(physicalStageWorkloadIdentityJson)
      )
    )

  private def physicalStageWorkloadIdentityJson(
      stage: PostgreSqlPhysicalPortfolioStageWorkloadIdentity
  ): Json.Value =
    Json.obj(
      "stage_id" -> Json.string(stage.stageId.code),
      "statement_sha256" -> Json.string(stage.statementSha256),
      "character_count" -> Json.number(stage.characterCount),
      "cte_prefix_count" -> Json.number(stage.ctePrefixCount)
    )

  private def physicalWorkloadAliasJson(
      alias: PostgreSqlPhysicalPortfolioWorkloadAlias
  ): Json.Value =
    Json.obj(
      "enumeration_ordinal" -> Json.number(alias.enumerationOrdinal),
      "structural_rank" -> Json.number(alias.structuralRank),
      "candidate_id" -> Json.string(alias.candidateId.value),
      "physical_variant_id" -> Json.string(alias.physicalVariantId.value)
    )

  private def physicalWorkloadGroupJson(
      group: PostgreSqlPhysicalPortfolioWorkloadGroupEvidence
  ): Json.Value =
    Json.obj(
      "descriptor" -> physicalWorkloadDescriptorJson(group.descriptor),
      "representative" -> physicalWorkloadAliasJson(group.representative),
      "aliases" -> Json.array(group.aliases.map(physicalWorkloadAliasJson)),
      "earliest_enumeration_ordinal" -> Json.number(
        group.earliestEnumerationOrdinal
      )
    )

  private def physicalDeduplicationJson(
      evidence: PostgreSqlPhysicalPortfolioDeduplicationEvidence
  ): Json.Value =
    Json.obj(
      "deduplication_scheme" -> Json.string(evidence.deduplicationScheme),
      "raw_variant_count" -> Json.number(evidence.rawVariantCount),
      "unique_workload_count" -> Json.number(evidence.uniqueWorkloadCount),
      "groups" -> Json.array(evidence.groups.map(physicalWorkloadGroupJson))
    )

  private def collisionProjectionJson(
      projection: PostgreSqlPhysicalPortfolioCollisionProjection
  ): Json.Value =
    Json.obj(
      "prior" -> collisionSqlVectorJson(projection.prior),
      "offending" -> collisionSqlVectorJson(projection.offending)
    )

  private def collisionSqlVectorJson(
      value: PostgreSqlPhysicalPortfolioCollisionSqlVector
  ): Json.Value =
    Json.obj(
      "enumeration_ordinal" -> Json.number(value.enumerationOrdinal),
      "descriptor" -> physicalWorkloadDescriptorJson(value.descriptor),
      "ordered_stage_sql" -> Json.array(value.orderedStageSql.map(Json.string))
    )

  private def physicalExplainRequestJson(
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest
  ): Json.Value =
    Json.obj(
      "minimum_server_version_num" -> Json.number(
        request.minimumServerVersionNum
      ),
      "materialization_scope" -> Json.string(request.materializationScope),
      "source" -> Json.obj(
        "source_statement_sha256" -> Json.string(
          request.source.sourceStatementSha256
        )
      ),
      "workloads" -> Json.array(
        request.workloads.map(physicalExplainWorkloadJson)
      )
    )

  private def physicalExplainWorkloadJson(
      workload: PostgreSqlPhysicalPortfolioExplainWorkload
  ): Json.Value =
    Json.obj(
      "group" -> physicalWorkloadGroupJson(workload.group),
      "stages" -> Json.array(workload.stages.map(physicalExplainStageJson))
    )

  private def physicalExplainStageJson(
      stage: PostgreSqlPhysicalPortfolioExplainStageStatement
  ): Json.Value =
    Json.obj(
      "stage_id" -> Json.string(stage.stageId.code),
      "statement_sha256" -> Json.string(stage.statementSha256),
      "character_count" -> Json.number(stage.characterCount),
      "cte_prefix_count" -> Json.number(stage.ctePrefixCount)
    )

  private def physicalEvaluationUsageJson(
      usage: PostgreSqlPhysicalPortfolioEvaluationUsage
  ): Json.Value =
    Json.obj(
      "workload_count" -> canonicalBigIntJson(usage.workloadCount),
      "explain_statement_count" -> canonicalBigIntJson(
        usage.explainStatementCount
      ),
      "diagnostic_character_count" -> canonicalBigIntJson(
        usage.diagnosticCharacterCount
      )
    )

  private def physicalEvaluationResourceExceededJson(
      exceeded: PostgreSqlPhysicalPortfolioEvaluationResourceExceeded
  ): Json.Value =
    Json.obj(
      "resource" -> Json.string(exceeded.resource.code),
      "workload_id" -> optional(exceeded.workloadId)(value =>
        Json.string(value.value)
      ),
      "required" -> canonicalBigIntJson(exceeded.required),
      "limit" -> Json.number(exceeded.limit)
    )

  private def physicalExplainBatchJson(
      batch: PostgreSqlPhysicalPortfolioExplainBatch
  ): Json.Value =
    Json.obj(
      "provenance" -> explainProvenanceJson(batch.provenance),
      "source" -> explainSourceJson(batch.source),
      "observations" -> Json.array(
        batch.observations.map(physicalExplainObservationJson)
      )
    )

  private def physicalExplainObservationJson(
      observation: PostgreSqlPhysicalPortfolioExplainObservation
  ): Json.Value =
    Json.obj(
      "workload_id" -> Json.string(observation.workloadId.value),
      "representative_physical_variant_id" -> Json.string(
        observation.representativePhysicalVariantId.value
      ),
      "stage_id" -> Json.string(observation.stageId.code),
      "statement_sha256" -> Json.string(observation.statementSha256),
      "optimizer_estimate" -> explainEstimateJson(observation.estimate)
    )

  private def physicalExplainFailureJson(
      failure: PostgreSqlPhysicalPortfolioExplainFailure
  ): Json.Value =
    Json.obj(
      "code" -> Json.string(failure.code.id),
      "stage" -> Json.string(failure.stage.id),
      "failed_statement_index" -> optional(failure.failedStatementIndex)(
        Json.number
      ),
      "source" -> optional(failure.source)(explainSourceJson),
      "completed" -> Json.array(
        failure.completed.map(physicalExplainObservationJson)
      ),
      "requested_stage_statement_count" -> Json.number(
        failure.requestedStageStatementCount
      ),
      "observed_server_version_num" -> optional(
        failure.observedServerVersionNum
      )(Json.number),
      "required_server_version_num" -> optional(
        failure.requiredServerVersionNum
      )(Json.number)
    )

  private def physicalEvaluationJson(
      evaluation: PostgreSqlPhysicalPortfolioEvaluationEvidence
  ): Json.Value =
    Json.obj(
      "workload" -> physicalWorkloadGroupJson(evaluation.workload),
      "source" -> explainSourceJson(evaluation.source),
      "stages" -> Json.array(
        evaluation.stages.map(physicalStageEvaluationJson)
      ),
      "final_frontier_stage_ids" -> Json.array(
        evaluation.finalFrontierStageIds.map(stage => Json.string(stage.code))
      ),
      "row_guard" -> explainRowGuardJson(evaluation.rowGuard),
      "score" -> physicalPortfolioScoreJson(evaluation.score),
      "score_scheme" -> Json.string(evaluation.scoreScheme),
      "final_frontier_scheme" -> Json.string(
        evaluation.finalFrontierScheme
      ),
      "numeric_envelope_scheme" -> Json.string(
        evaluation.numericEnvelopeScheme
      )
    )

  private def physicalPortfolioScoreJson(
      score: PostgreSqlPhysicalPortfolioScore
  ): Json.Value =
    Json.obj(
      "max_all_named_stage_plan_rows_times_plan_width" -> Json.number(
        score.maxAllNamedStagePlanRowsTimesPlanWidth
      ),
      "sum_final_frontier_plan_rows_times_plan_width" -> Json.number(
        score.sumFinalFrontierPlanRowsTimesPlanWidth
      ),
      "final_plan_total_cost" -> Json.number(score.finalPlanTotalCost),
      "structural_rank" -> Json.number(score.structuralRank),
      "candidate_id" -> Json.string(score.candidateId.value),
      "representative_physical_variant_id" -> Json.string(
        score.representativePhysicalVariantId.value
      )
    )

  private def physicalStageEvaluationJson(
      stage: PostgreSqlPhysicalPortfolioStageEvaluation
  ): Json.Value =
    Json.obj(
      "trace" -> physicalStageTraceJson(stage.trace),
      "diagnostic_statement_sha256" -> optional(
        stage.diagnosticStatementSha256
      )(Json.string),
      "diagnostic_character_count" -> optional(
        stage.diagnosticCharacterCount
      )(Json.number),
      "diagnostic_cte_prefix_count" -> optional(
        stage.diagnosticCtePrefixCount
      )(Json.number),
      "optimizer_estimate" -> optional(stage.estimate)(explainEstimateJson),
      "final_frontier" -> Json.bool(stage.finalFrontier)
    )

  private def physicalStageTraceJson(
      trace: PostgreSqlStageTrace
  ): Json.Value =
    Json.obj(
      "stage_id" -> Json.string(trace.id.code),
      "kind" -> Json.string(trace.kind.code),
      "physical_form" -> Json.string(physicalStageFormCode(trace.physicalForm)),
      "physical_cte_name" -> optional(trace.physicalCteName)(Json.string),
      "logical_bag_ids" -> Json.array(
        trace.logicalBagIds.map(id => Json.number(id.value))
      ),
      "logical_edge_ids" -> Json.array(
        trace.logicalEdgeIds.map(id => Json.number(id.value))
      ),
      "semijoin_members" -> Json.array(trace.semijoinMembers.map { member =>
        Json.obj(
          "pass" -> Json.string(member.pass.code),
          "ordinal" -> Json.number(member.ordinal),
          "target_bag_id" -> Json.number(member.target.value),
          "source_bag_id" -> Json.number(member.source.value),
          "separator_variable_ids" -> Json.array(
            member.separator.map(id => Json.number(id.value))
          )
        )
      }),
      "input_stage_ids" -> Json.array(
        trace.inputStageIds.map(id => Json.string(id.code))
      )
    )

  private def physicalStageFormCode(
      form: PostgreSqlStagePhysicalForm
  ): String = form match {
    case PostgreSqlStagePhysicalForm.Elided              => "elided"
    case PostgreSqlStagePhysicalForm.PlannerControlledCte =>
      "planner-controlled-cte"
    case PostgreSqlStagePhysicalForm.MaterializedCte => "materialized-cte"
    case PostgreSqlStagePhysicalForm.NotMaterializedCte =>
      "not-materialized-cte"
    case PostgreSqlStagePhysicalForm.TerminalSelect => "terminal-select"
  }

  private def physicalPortfolioEnumerationFailureJson(
      problem: PostgreSqlPhysicalPortfolioEnumerationFailure
  ): Json.Value = {
    import PostgreSqlPhysicalPortfolioEnumerationFailure._
    val details: Vector[(String, Json.Value)] = problem match {
      case MissingValue(path) => Vector("path" -> Json.string(path))
      case InvalidCandidateSet(detail) =>
        Vector("detail" -> Json.string(detail))
      case InvalidLimits(errors) =>
        Vector(
          "limit_errors" -> Json.array(errors.map { error =>
            Json.obj(
              "code" -> Json.string(error.code),
              "message" -> Json.string(error.message)
            )
          })
        )
      case ResourceLimitExceeded(
            resource,
            candidateId,
            bagId,
            required,
            limit
          ) =>
        Vector(
          "resource" -> Json.string(resource.code),
          "candidate_id" -> optional(candidateId)(id =>
            Json.string(id.value)
          ),
          "bag_id" -> optional(bagId)(id => Json.number(id.value)),
          "required" -> canonicalBigIntJson(required),
          "limit" -> Json.number(limit)
        )
      case MissingAdmissibleCovers(candidateId, bagId) =>
        Vector(
          "candidate_id" -> Json.string(candidateId.value),
          "bag_id" -> Json.number(bagId.value)
        )
      case StructuralCoverNotAdmissible(candidateId, bagId) =>
        Vector(
          "candidate_id" -> Json.string(candidateId.value),
          "bag_id" -> Json.number(bagId.value)
        )
    }
    Json.obj(
      Vector(
        "code" -> Json.string(problem.code),
        "message" -> Json.string(problem.message)
      ) ++ details
    )
  }

  private def configuredPlanningProblemJson(
      problem: ConfiguredBagFactorPlanningError
  ): Json.Value =
    Json.obj(
      "code" -> Json.string(problem.code),
      "message" -> Json.string(problem.message)
    )

  private def renderingProblemJson(
      problem: PostgreSqlRenderingError
  ): Json.Value =
    Json.obj(
      "code" -> Json.string(problem.code),
      "message" -> Json.string(problem.message)
    )

  def renderFailure(problem: PostgreSqlRewriteCliFailure): String = {
    require(problem != null, "CLI failure must not be null")
    Json.render(
      Json.obj(
        "schema_version" -> Json.string(SchemaVersion),
        "status" -> Json.string(problem.status),
        "exit_code" -> Json.number(problem.exitCode),
        "request" -> Json.Null,
        "outcome" -> Json.obj(
          "rewritten_statements" -> Json.array(Vector.empty),
          "rewritten_statement_sha256" -> Json.array(Vector.empty),
          "diagnostics" -> Json.array(
            Vector(
              diagnosticFields(
                problem.code,
                "error",
                problem.message,
                problem.context
              )
            )
          )
        ),
        "report" -> Json.Null
      )
    )
  }

  private def requestJson(request: RewriteRequest): Json.Value =
    Json.obj(
      "target_dialect" -> Json.string(request.targetDialect.id),
      "options" -> Json.obj(
        "width_bound" -> Json.number(request.options.widthBound),
        "plan_limit" -> Json.number(request.options.planLimit),
        "require_connected_cover" -> Json.bool(
          request.options.requireConnectedCover
        ),
        "candidate_selection" -> requestedSelectionJson(
          request.options.candidateSelection
        ),
        "reduction_schedule" -> Json.string(
          request.options.reductionMode.id
        )
      ),
      "original_sql" -> Json.string(request.originalSql),
      "original_sql_sha256" -> Json.string(sha256(request.originalSql))
    )

  private def outcomeJson(outcome: RewriteOutcome): Json.Value = {
    val rewritten = outcome match {
      case value: RewriteOutcome.Rewritten => value.rewrittenStatements
      case _                               => Vector.empty
    }
    Json.obj(
      "rewritten_statements" -> Json.array(rewritten.map(Json.string)),
      "rewritten_statement_sha256" -> Json.array(
        rewritten.map(statement => Json.string(sha256(statement)))
      ),
      "diagnostics" -> Json.array(outcome.diagnostics.map(diagnosticJson))
    )
  }

  private def diagnosticJson(diagnostic: RewriteDiagnostic): Json.Value =
    diagnosticFields(
      diagnostic.code.id,
      diagnostic.severity.id,
      diagnostic.message,
      diagnostic.context
    )

  private def diagnosticFields(
      code: String,
      severity: String,
      message: String,
      context: Map[String, String]
  ): Json.Value =
    Json.obj(
      "code" -> Json.string(code),
      "severity" -> Json.string(severity),
      "message" -> Json.string(message),
      "context" -> Json.obj(
        safeContext(context).map { case (key, value) =>
          key -> Json.string(value)
        }
      )
    )

  private def reportJson(
      report: PostgreSqlRewriteReport,
      factorAware: Boolean,
      countStageCost: Boolean
  ): Json.Value = {
    val decision = report.candidateDecision

    Json.obj(
      "completed_phase" -> Json.string(report.completedPhase.id),
      "analysis" -> optional(report.analysis)(analysisJson),
      "hypergraph" -> optional(report.hypergraph) { hypergraph =>
        Json.obj(
          "edge_count" -> Json.number(hypergraph.edges.size),
          "variable_count" -> Json.number(hypergraph.variables.size),
          "column_count" -> Json.number(hypergraph.columns.size)
        )
      },
      "soft_generation" -> optional(report.softGeneration)(softGenerationJson),
      "admissible_generation" -> optional(report.admissibleGeneration)(
        admissibleGenerationJson
      ),
      "candidate_input" -> optional(report.candidateInput) { input =>
        Json.obj(
          "width_bound" -> Json.number(input.widthBound),
          "require_connected_cover" -> Json.bool(
            input.requireConnectedCover
          ),
          "candidate_bag_count" -> Json.number(input.candidateBags.size)
        )
      },
      "candidate_search" -> optional(decision)(candidateDecisionJson),
      "candidate_selection" -> optional(report.candidateSelectionDecision)(
        decision =>
          candidateSelectionJson(decision, factorAware, countStageCost)
      ),
      "selected_plan" -> optional(report.artifact)(artifact =>
        selectedPlanJson(artifact, factorAware, countStageCost)
      ),
      "bag_factor_plan" -> optional(report.bagFactorPlan)(bagFactorSummary),
      "rendering" -> optional(report.artifact)(renderingSummary)
    )
  }

  private def analysisJson(report: AnalysisReport): Json.Value =
    Json.obj(
      "supported" -> Json.bool(report.isSupported),
      "relation_count" -> Json.number(report.query.relations.size),
      "predicate_count" -> Json.number(report.query.predicates.size),
      "output_column_count" -> Json.number(report.query.output.size),
      "diagnostic_count" -> Json.number(report.diagnostics.size)
    )

  private def softGenerationJson(
      generation: SoftBagGeneration
  ): Json.Value = {
    val witnessCount = generation.candidates.iterator
      .map(candidate => BigInt(candidate.witnesses.size))
      .sum
    val physicalCoverCount = generation.candidates.iterator
      .map(candidate => BigInt(candidate.covers.size))
      .sum
    val connectedCoverCount = generation.candidates.iterator
      .flatMap(_.covers)
      .count(_.isConnected)
    Json.obj(
      "width_bound" -> Json.number(generation.widthBound),
      "enumerated_cover_count" -> Json.number(generation.enumeratedCovers.size),
      "candidate_count" -> Json.number(generation.candidates.size),
      "witness_count" -> Json.number(witnessCount),
      "physical_cover_count" -> Json.number(physicalCoverCount),
      "connected_physical_cover_count" -> Json.number(connectedCoverCount)
    )
  }

  private def admissibleGenerationJson(
      generation: AdmissibleSoftBagGeneration
  ): Json.Value = {
    val witnessCount = generation.candidates.iterator
      .map(candidate => BigInt(candidate.witnesses.size))
      .sum
    val physicalCoverCount = generation.candidates.iterator
      .map(candidate => BigInt(candidate.physicalCovers.size))
      .sum
    val connectedCoverCount = generation.candidates.iterator
      .flatMap(_.physicalCovers)
      .count(_.isConnected)
    Json.obj(
      "width_bound" -> Json.number(generation.widthBound),
      "require_connected_cover" -> Json.bool(
        generation.requireConnectedCover
      ),
      "definition_cover_count" -> Json.number(
        generation.definitionCovers.size
      ),
      "candidate_count" -> Json.number(generation.candidates.size),
      "witness_count" -> Json.number(witnessCount),
      "physical_cover_count" -> Json.number(physicalCoverCount),
      "connected_physical_cover_count" -> Json.number(connectedCoverCount)
    )
  }

  private def candidateDecisionJson(
      decision: CandidatePlanDecision
  ): Json.Value = decision match {
    case found: CandidatePlanDecision.Found =>
      val plans = found.plans.sortBy(_.rank)
      Json.obj(
        "decision" -> Json.string("found"),
        "candidate_id_scheme" -> Json.string(CandidatePlanId.Scheme),
        "requested_limit" -> Json.number(found.requestedLimit),
        "returned_plan_count" -> Json.number(plans.size),
        "statistics" -> candidateStatisticsJson(found.statistics),
        "plans" -> Json.array(plans.map(candidatePlanJson))
      )
    case rejected: CandidatePlanDecision.Rejected =>
      Json.obj(
        "decision" -> Json.string("rejected"),
        "candidate_id_scheme" -> Json.string(CandidatePlanId.Scheme),
        "requested_limit" -> Json.number(rejected.requestedLimit),
        "returned_plan_count" -> Json.number(0),
        "statistics" -> candidateStatisticsJson(rejected.statistics),
        "plans" -> Json.array(Vector.empty)
      )
  }

  private def candidateStatisticsJson(
      statistics: CandidateTopKStatistics
  ): Json.Value =
    Json.obj(
      "candidate_bag_count" -> Json.number(
        statistics.base.candidateBagCount
      ),
      "block_count" -> Json.number(statistics.base.blockCount),
      "basis_alternative_count" -> Json.number(
        statistics.base.basisAlternativeCount
      ),
      "satisfied_block_count" -> Json.number(
        statistics.base.satisfiedBlockCount
      ),
      "retained_partial_plan_count" -> Json.number(
        statistics.retainedPartialPlanCount
      ),
      "combination_state_count" -> Json.number(
        statistics.combinationStateCount
      )
    )

  private def candidatePlanJson(plan: RankedCandidatePlan): Json.Value =
    Json.obj(
      "rank" -> Json.number(plan.rank),
      "candidate_id" -> Json.string(plan.candidateId.value),
      "structural_score" -> Json.obj(
        "node_count" -> Json.number(plan.score.nodeCount)
      ),
      "cover_selection_policy" -> Json.string(CoverPolicy),
      "certificate" -> Json.obj(
        "bag_count" -> Json.number(plan.certificate.bagCount),
        "component_arc_count" -> Json.number(
          plan.certificate.componentArcs.size
        ),
        "require_connected_cover" -> Json.bool(
          plan.certificate.requireConnectedCover
        )
      ),
      "decomposition" -> decompositionJson(plan.decomposition)
    )

  private def selectedPlanJson(
      artifact: PostgreSqlRewriteArtifact,
      factorAware: Boolean,
      countStageCost: Boolean
  ): Json.Value = {
    val plan = artifact.selectedPlan
    val selection = artifact.candidateSelection
    val evaluation = selection.evaluations.find(candidate =>
      candidate.candidateId == plan.candidateId &&
        candidate.structuralRank == plan.rank &&
        candidate.physicalVariantId == selection.selectedPhysicalVariantId
    ).getOrElse(
      throw new IllegalStateException(
        "selected candidate has no physical evaluation"
      )
    )
    val base = Vector(
      "selection_policy" -> Json.string(selection.policy.id),
      "selection_scope" -> Json.string(selection.scope),
      "cost_selected" -> Json.bool(selection.costSelected),
      "estimated_cost_selected" -> Json.bool(
        selection.estimatedCostSelected
      ),
      "measured_runtime_selected" -> Json.bool(
        selection.measuredRuntimeSelected
      ),
      "uses_catalog_statistics" -> Json.bool(
        selection.usesCatalogStatistics
      ),
      "uses_execution_feedback" -> Json.bool(
        selection.usesExecutionFeedback
      ),
      "rank" -> Json.number(plan.rank),
      "candidate_id" -> Json.string(plan.candidateId.value),
      "physical_variant_id" -> Json.string(evaluation.physicalVariantId.value),
      "reduction_schedule" -> Json.string(evaluation.reductionMode.id),
      "reduction_order" -> Json.string(evaluation.reductionOrder),
      "physical_shape_score" -> physicalScoreJson(evaluation.score),
      "optimizer_estimate" -> optional(evaluation.explainCostEstimate)(
        explainEstimateJson
      ),
      "row_guard" -> optional(evaluation.rowGuardEvidence)(
        explainRowGuardJson
      ),
      "source_optimizer_estimate" -> optional(
        selection.sourceEvaluation
      )(explainSourceJson),
      "decomposition" -> decompositionJson(plan.decomposition)
    )
    Json.obj(
      base ++
        (if (factorAware)
           Vector(
             "factor_aware_evidence" -> optional(
               evaluation.factorAwareEvidence
             )(factorAwareCandidateEvidenceJson)
           )
         else if (countStageCost)
           Vector(
             "count_stage_cost_evidence" -> optional(
               evaluation.countStageCostEvidence
             )(countStageCostCandidateEvidenceJson)
           )
         else Vector.empty)
    )
  }

  private def requestedSelectionJson(
      selection: CandidatePlanSelection
  ): Json.Value = {
    val requestedRank = selection match {
      case CandidatePlanSelection.StructuralRank(rank) => Some(rank)
      case _                                           => None
    }
    val requestedCandidateId = selection match {
      case CandidatePlanSelection.ExactCandidateId(candidateId) =>
        Some(candidateId.value)
      case _ => None
    }
    Json.obj(
      "policy" -> Json.string(selection.id),
      "requested_rank" -> optional(requestedRank)(value => Json.number(value)),
      "requested_candidate_id" -> optional(requestedCandidateId)(value =>
        Json.string(value)
      )
    )
  }

  private def candidateSelectionJson(
      decision: PostgreSqlCandidateSelectionDecision,
      factorAware: Boolean,
      countStageCost: Boolean
  ): Json.Value = {
    val (
      status,
      selectedRank,
      selectedId,
      selectedPhysicalId,
      failedRank,
      failedId,
      failedPhysicalId,
      retainedCount,
      explainFailureStage
    ) =
      decision match {
        case selected: PostgreSqlCandidateSelectionDecision.Selected =>
          (
            "selected",
            Some(selected.selectedStructuralRank),
            Some(selected.selectedCandidateId.value),
            Some(selected.selectedPhysicalVariantId.value),
            None,
            None,
            None,
            None,
            None
          )
        case unavailable: PostgreSqlCandidateSelectionDecision.Unavailable =>
          (
            "unavailable",
            None,
            None,
            None,
            None,
            None,
            None,
            Some(unavailable.retainedCandidateCount),
            None
          )
        case failed: PostgreSqlCandidateSelectionDecision.EvaluationFailed =>
          (
            "evaluation_failed",
            None,
            None,
            None,
            Some(failed.failedStructuralRank),
            Some(failed.failedCandidateId.value),
            Some(failed.failedPhysicalVariantId.value),
            None,
            None
          )
        case failed: PostgreSqlCandidateSelectionDecision.ExplainEvaluationFailed =>
          (
            "explain_evaluation_failed",
            None,
            None,
            None,
            failed.failedStructuralRank,
            failed.failedCandidateId.map(_.value),
            failed.failedPhysicalVariantId.map(_.value),
            None,
            Some(failed.stage.id)
          )
        case _: PostgreSqlCandidateSelectionDecision.NoAdmissibleEstimate =>
          (
            "no_admissible_estimate",
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None
          )
        case failed: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed =>
          (
            if (countStageCost) "count_stage_cost_evaluation_failed"
            else "factor_aware_evaluation_failed",
            None,
            None,
            None,
            failed.failedStructuralRank,
            failed.failedCandidateId.map(_.value),
            failed.failedPhysicalVariantId.map(_.value),
            None,
            Some(failed.stage.id)
          )
      }
    val base = Vector(
      "decision" -> Json.string(status),
      "policy" -> Json.string(decision.policy.id),
      "selection_scope" -> Json.string(decision.scope),
      "physical_variant_id_scheme" -> Json.string(
        PostgreSqlPhysicalVariantId.Scheme
      ),
      "reduction_schedule" -> Json.string(decision.reductionMode.id),
      "reduction_order" -> Json.string(decision.reductionOrder),
      "cost_selected" -> Json.bool(decision.costSelected),
      "estimated_cost_selected" -> Json.bool(
        decision.estimatedCostSelected
      ),
      "measured_runtime_selected" -> Json.bool(
        decision.measuredRuntimeSelected
      ),
      "uses_catalog_statistics" -> Json.bool(decision.usesCatalogStatistics),
      "uses_execution_feedback" -> Json.bool(decision.usesExecutionFeedback),
      "score_order" -> Json.array(
        PostgreSqlStaticShapeScore.OrderedFields.map(Json.string)
      ),
      "cost_order" ->
        (if (decision.costSelected)
           Json.array(
             (if (factorAware) PostgreSqlFactorAwareProgressiveV1.ScoreOrder
              else if (countStageCost)
                PostgreSqlCountStageCostProgressiveV1.ScoreOrder
              else PostgreSqlExplainCostEstimate.OrderedFields).map(Json.string)
           )
         else Json.Null),
      "cost_provenance" -> optional(decision.explainCostProvenance)(
        explainProvenanceJson
      ),
      "source_optimizer_estimate" -> optional(decision.sourceEvaluation)(
        explainSourceJson
      ),
      "explain_failure_stage" -> optional(explainFailureStage)(Json.string),
      "evaluated_candidate_count" -> Json.number(decision.evaluations.size),
      "evaluations" -> Json.array(
        decision.evaluations.map(evaluation =>
          candidateEvaluationJson(evaluation, factorAware, countStageCost)
        )
      ),
      "selected_rank" -> optional(selectedRank)(value => Json.number(value)),
      "selected_candidate_id" -> optional(selectedId)(value =>
        Json.string(value)
      ),
      "selected_physical_variant_id" -> optional(selectedPhysicalId)(value =>
        Json.string(value)
      ),
      "failed_rank" -> optional(failedRank)(value => Json.number(value)),
      "failed_candidate_id" -> optional(failedId)(value => Json.string(value)),
      "failed_physical_variant_id" -> optional(failedPhysicalId)(value =>
        Json.string(value)
      ),
      "retained_candidate_count" -> optional(retainedCount)(value =>
        Json.number(value)
      )
    )
    Json.obj(
      base ++
        (if (factorAware)
           Vector("factor_aware" -> factorAwareDecisionJson(decision))
         else if (countStageCost)
           Vector(
             "count_stage_cost" -> countStageCostDecisionJson(decision)
           )
         else Vector.empty)
    )
  }

  private def candidateEvaluationJson(
      evaluation: PostgreSqlCandidateEvaluation,
      factorAware: Boolean,
      countStageCost: Boolean
  ): Json.Value =
    Json.obj(Vector(
      "rank" -> Json.number(evaluation.structuralRank),
      "candidate_id" -> Json.string(evaluation.candidateId.value),
      "physical_variant_id" -> Json.string(evaluation.physicalVariantId.value),
      "reduction_schedule" -> Json.string(evaluation.reductionMode.id),
      "reduction_order" -> Json.string(evaluation.reductionOrder),
      "rendered_statement_sha256" -> Json.string(
        evaluation.renderedStatementSha256
      ),
      "physical_shape_score" -> physicalScoreJson(evaluation.score),
      "optimizer_estimate" -> optional(evaluation.explainCostEstimate)(
        explainEstimateJson
      ),
      "row_guard" -> optional(evaluation.rowGuardEvidence)(
        explainRowGuardJson
      )
    ) ++
      (if (factorAware)
         Vector(
           "factor_aware_evidence" -> optional(
             evaluation.factorAwareEvidence
           )(factorAwareCandidateEvidenceJson)
         )
       else if (countStageCost)
         Vector(
           "count_stage_cost_evidence" -> optional(
             evaluation.countStageCostEvidence
           )(countStageCostCandidateEvidenceJson)
         )
       else Vector.empty))

  private def factorAwareDecisionJson(
      decision: PostgreSqlCandidateSelectionDecision
  ): Json.Value = {
    val (rendered, completed, failureCode, failedStageId, failedStageIndex) =
      decision match {
      case failed: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed =>
        (
          failed.renderedCandidates.map(factorAwareRenderedCandidateJson),
          failed.completedStageObservations.map(factorAwareObservationJson),
          Some(failed.failureCode.id),
          failed.failedStageId.map(_.code),
          failed.failedStageObservationIndex
        )
      case _ =>
        (
          decision.evaluations.map(factorAwareRenderedEvaluationJson),
          Vector.empty,
          None,
          None,
          None
        )
    }
    Json.obj(
      "selection_scope" -> Json.string(
        "best_within_retained_structural_prefix"
      ),
      "global_optimality_claim" -> Json.bool(false),
      "requested_plan_limit" -> Json.number(
        PostgreSqlFactorAwareProgressiveV1.RequiredPlanLimit
      ),
      "terminal_effective_candidate_count" -> optional(
        decision.factorAwareCheckpoints.lastOption.map(
          _.effectiveCandidateCount
        )
      )(Json.number),
      "search_exhausted_before_limit" -> optional(
        decision.factorAwareCheckpoints.lastOption.map(
          _.truncatedBySearchExhaustion
        )
      )(Json.bool),
      "checkpoint_schedule" -> Json.array(
        PostgreSqlFactorAwareProgressiveV1.Checkpoints.map(Json.number)
      ),
      "score_order" -> Json.array(
        PostgreSqlFactorAwareProgressiveV1.ScoreOrder.map(Json.string)
      ),
      "prefix_proof_scheme" -> Json.string(
        at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlFactorAwarePrefixProof.Scheme
      ),
      "stage_id_scheme" -> Json.string(
        PostgreSqlFactorAwareCandidateEvidence.StageIdScheme
      ),
      "stage_order_scheme" -> Json.string(
        PostgreSqlFactorAwareCandidateEvidence.StageOrderScheme
      ),
      "statement_hash_scheme" -> Json.string(
        PostgreSqlFactorAwareCandidateEvidence.StatementHashScheme
      ),
      "character_count_scheme" -> Json.string(
        PostgreSqlFactorAwareCandidateEvidence.CharacterCountScheme
      ),
      "numeric_envelope" -> Json.obj(
        "scheme" -> Json.string(PostgreSqlFactorAwareNumericEnvelope.Scheme),
        "max_input_precision" -> Json.number(
          PostgreSqlFactorAwareNumericEnvelope.MaxInputPrecision
        ),
        "max_derived_precision" -> Json.number(
          PostgreSqlFactorAwareNumericEnvelope.MaxDerivedPrecision
        ),
        "max_absolute_scale" -> Json.number(
          PostgreSqlFactorAwareNumericEnvelope.MaxAbsoluteScale
        )
      ),
      "limits" -> optional(decision.factorAwareSelectionLimits)(
        factorAwareLimitsJson
      ),
      "checkpoints" -> Json.array(
        decision.factorAwareCheckpoints.map(factorAwareCheckpointJson)
      ),
      "rendered_candidates" -> Json.array(rendered),
      "completed_stage_observations" -> Json.array(completed),
      "failure_code" -> optional(failureCode)(Json.string),
      "failed_stage_id" -> optional(failedStageId)(Json.string),
      "failed_stage_observation_index" -> optional(failedStageIndex)(Json.number)
    )
  }

  private def countStageCostDecisionJson(
      decision: PostgreSqlCandidateSelectionDecision
  ): Json.Value = {
    val (rendered, completed, failureCode, failedStageId, failedStageIndex) =
      decision match {
        case failed: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed =>
          (
            failed.renderedCandidates.map(factorAwareRenderedCandidateJson),
            failed.completedStageObservations.map(factorAwareObservationJson),
            Some(failed.failureCode.id),
            failed.failedStageId.map(_.code),
            failed.failedStageObservationIndex
          )
        case _ =>
          (
            decision.evaluations.map(countStageCostRenderedEvaluationJson),
            Vector.empty,
            None,
            None,
            None
          )
      }
    val limits = decision match {
      case failed: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed =>
        failed.factorAwareSelectionLimits
      case _ => decision.countStageCostSelectionLimits
    }
    Json.obj(
      "selection_scope" -> Json.string(
        PostgreSqlCountStageCostProgressiveV1.Scope
      ),
      "global_optimality_claim" -> Json.bool(false),
      "applicable_query_shape" -> Json.string("certified_global_count_star"),
      "required_reduction_schedule" -> Json.string(
        at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode.FullTwoPass.id
      ),
      "requested_plan_limit" -> Json.number(
        PostgreSqlCountStageCostProgressiveV1.RequiredPlanLimit
      ),
      "terminal_effective_candidate_count" -> optional(
        decision.countStageCostCheckpoints.lastOption.map(
          _.effectiveCandidateCount
        )
      )(Json.number),
      "search_exhausted_before_limit" -> optional(
        decision.countStageCostCheckpoints.lastOption.map(
          _.truncatedBySearchExhaustion
        )
      )(Json.bool),
      "checkpoint_schedule" -> Json.array(
        PostgreSqlCountStageCostProgressiveV1.Checkpoints.map(Json.number)
      ),
      "score_order" -> Json.array(
        PostgreSqlCountStageCostProgressiveV1.ScoreOrder.map(Json.string)
      ),
      "prefix_proof_scheme" -> Json.string(
        at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlFactorAwarePrefixProof.Scheme
      ),
      "stage_id_scheme" -> Json.string(
        PostgreSqlCountStageCostCandidateEvidence.StageIdScheme
      ),
      "stage_order_scheme" -> Json.string(
        PostgreSqlCountStageCostCandidateEvidence.StageOrderScheme
      ),
      "statement_hash_scheme" -> Json.string(
        PostgreSqlCountStageCostCandidateEvidence.StatementHashScheme
      ),
      "character_count_scheme" -> Json.string(
        PostgreSqlCountStageCostCandidateEvidence.CharacterCountScheme
      ),
      "numeric_envelope" -> Json.obj(
        "scheme" -> Json.string(PostgreSqlFactorAwareNumericEnvelope.Scheme),
        "max_input_precision" -> Json.number(
          PostgreSqlFactorAwareNumericEnvelope.MaxInputPrecision
        ),
        "max_derived_precision" -> Json.number(
          PostgreSqlFactorAwareNumericEnvelope.MaxDerivedPrecision
        ),
        "max_absolute_scale" -> Json.number(
          PostgreSqlFactorAwareNumericEnvelope.MaxAbsoluteScale
        )
      ),
      "limits" -> optional(limits)(factorAwareLimitsJson),
      "checkpoints" -> Json.array(
        decision.countStageCostCheckpoints.map(countStageCostCheckpointJson)
      ),
      "rendered_candidates" -> Json.array(rendered),
      "completed_stage_observations" -> Json.array(completed),
      "failure_code" -> optional(failureCode)(Json.string),
      "failed_stage_id" -> optional(failedStageId)(Json.string),
      "failed_stage_observation_index" -> optional(failedStageIndex)(Json.number)
    )
  }

  private def factorAwareLimitsJson(
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Json.Value =
    Json.obj(
      "max_candidates" -> Json.number(limits.maxCandidates),
      "max_stage_probes_per_candidate" -> Json.number(
        limits.maxStageProbesPerCandidate
      ),
      "max_characters_per_probe" -> Json.number(limits.maxCharactersPerProbe),
      "max_diagnostic_characters_per_candidate" -> Json.number(
        limits.maxDiagnosticCharactersPerCandidate
      ),
      "max_explain_statements_per_session" -> Json.number(
        limits.maxExplainStatementsPerSession
      ),
      "max_diagnostic_characters_per_session" -> Json.number(
        limits.maxDiagnosticCharactersPerSession
      ),
      "max_plan_json_characters" -> Json.number(
        limits.maxPlanJsonCharacters
      ),
      "max_cumulative_plan_json_characters" -> Json.number(
        limits.maxCumulativePlanJsonCharacters
      ),
      "statement_timeout_seconds" -> Json.number(
        limits.statementTimeoutSeconds
      ),
      "explain_statement_budget_seconds" -> Json.number(
        limits.explainStatementBudgetSeconds
      )
    )

  private def factorAwareCheckpointJson(
      checkpoint: PostgreSqlFactorAwareCheckpoint
  ): Json.Value =
    Json.obj(
      "requested_prefix" -> Json.number(checkpoint.requestedPrefix),
      "effective_candidate_count" -> Json.number(
        checkpoint.effectiveCandidateCount
      ),
      "truncated_by_search_exhaustion" -> Json.bool(
        checkpoint.truncatedBySearchExhaustion
      ),
      "verified_prefix_sha256" -> Json.string(
        checkpoint.verifiedPrefixSha256
      ),
      "prefix_proof_scheme" -> Json.string(checkpoint.prefixProofScheme),
      "winner_rank" -> optional(checkpoint.winnerStructuralRank)(Json.number),
      "winner_candidate_id" -> optional(checkpoint.winnerCandidateId)(id =>
        Json.string(id.value)
      ),
      "winner_physical_variant_id" -> optional(
        checkpoint.winnerPhysicalVariantId
      )(id => Json.string(id.value)),
      "winner_score" -> optional(checkpoint.winnerScore)(factorAwareScoreJson)
    )

  private def countStageCostCheckpointJson(
      checkpoint: PostgreSqlCountStageCostCheckpoint
  ): Json.Value =
    Json.obj(
      "requested_prefix" -> Json.number(checkpoint.requestedPrefix),
      "effective_candidate_count" -> Json.number(
        checkpoint.effectiveCandidateCount
      ),
      "truncated_by_search_exhaustion" -> Json.bool(
        checkpoint.truncatedBySearchExhaustion
      ),
      "verified_prefix_sha256" -> Json.string(
        checkpoint.verifiedPrefixSha256
      ),
      "prefix_proof_scheme" -> Json.string(checkpoint.prefixProofScheme),
      "winner_rank" -> optional(checkpoint.winnerStructuralRank)(Json.number),
      "winner_candidate_id" -> optional(checkpoint.winnerCandidateId)(id =>
        Json.string(id.value)
      ),
      "winner_physical_variant_id" -> optional(
        checkpoint.winnerPhysicalVariantId
      )(id => Json.string(id.value)),
      "winner_score" -> optional(checkpoint.winnerScore)(
        countStageCostScoreJson
      )
    )

  private def factorAwareScoreJson(
      score: PostgreSqlFactorAwareScore
  ): Json.Value =
    Json.obj(
      "max_named_stage_plan_rows_times_plan_width" -> Json.number(
        score.maxNamedStagePlanRowsTimesPlanWidth
      ),
      "sum_named_stage_plan_rows_times_plan_width" -> Json.number(
        score.sumNamedStagePlanRowsTimesPlanWidth
      ),
      "final_plan_total_cost" -> Json.number(score.finalPlanTotalCost),
      "structural_rank" -> Json.number(score.structuralRank),
      "candidate_id" -> Json.string(score.candidateId.value)
    )

  private def countStageCostScoreJson(
      score: PostgreSqlCountStageCostScore
  ): Json.Value =
    Json.obj(
      "sum_named_stage_plan_total_cost" -> Json.number(
        score.sumNamedStagePlanTotalCost
      ),
      "max_named_stage_plan_rows_times_plan_width" -> Json.number(
        score.maxNamedStagePlanRowsTimesPlanWidth
      ),
      "sum_named_stage_plan_rows_times_plan_width" -> Json.number(
        score.sumNamedStagePlanRowsTimesPlanWidth
      ),
      "final_plan_total_cost" -> Json.number(score.finalPlanTotalCost),
      "structural_rank" -> Json.number(score.structuralRank),
      "candidate_id" -> Json.string(score.candidateId.value)
    )

  private def factorAwareCandidateEvidenceJson(
      evidence: PostgreSqlFactorAwareCandidateEvidence
  ): Json.Value =
    Json.obj(
      "physical_variant_id" -> Json.string(evidence.physicalVariantId.value),
      "stage_id_scheme" -> Json.string(evidence.stageIdScheme),
      "stage_order_scheme" -> Json.string(evidence.stageOrderScheme),
      "statement_hash_scheme" -> Json.string(evidence.statementHashScheme),
      "character_count_scheme" -> Json.string(evidence.characterCountScheme),
      "score" -> factorAwareScoreJson(evidence.score),
      "stages" -> Json.array(evidence.stages.map(factorAwareStageJson))
    )

  private def countStageCostCandidateEvidenceJson(
      evidence: PostgreSqlCountStageCostCandidateEvidence
  ): Json.Value =
    Json.obj(
      "physical_variant_id" -> Json.string(evidence.physicalVariantId.value),
      "stage_id_scheme" -> Json.string(evidence.stageIdScheme),
      "stage_order_scheme" -> Json.string(evidence.stageOrderScheme),
      "statement_hash_scheme" -> Json.string(evidence.statementHashScheme),
      "character_count_scheme" -> Json.string(evidence.characterCountScheme),
      "score" -> countStageCostScoreJson(evidence.score),
      "stages" -> Json.array(evidence.stages.map(factorAwareStageJson))
    )

  private def factorAwareStageJson(
      stage: PostgreSqlFactorAwareStageEvaluation
  ): Json.Value = {
    val trace = stage.trace
    val physicalForm = trace.physicalForm match {
      case PostgreSqlStagePhysicalForm.Elided => "elided"
      case PostgreSqlStagePhysicalForm.PlannerControlledCte =>
        "planner-controlled-cte"
      case PostgreSqlStagePhysicalForm.MaterializedCte |
          PostgreSqlStagePhysicalForm.NotMaterializedCte =>
        throw new IllegalArgumentException(
          "v5 factor-aware JSON cannot serialize portfolio materialization forms"
        )
      case PostgreSqlStagePhysicalForm.TerminalSelect => "terminal-select"
    }
    Json.obj(
      "stage_id" -> Json.string(trace.id.code),
      "kind" -> Json.string(trace.kind.code),
      "physical_form" -> Json.string(physicalForm),
      "physical_cte_name" -> optional(trace.physicalCteName)(Json.string),
      "logical_bag_ids" -> Json.array(
        trace.logicalBagIds.map(id => Json.number(id.value))
      ),
      "logical_edge_ids" -> Json.array(
        trace.logicalEdgeIds.map(id => Json.number(id.value))
      ),
      "semijoin_members" -> Json.array(trace.semijoinMembers.map { member =>
        Json.obj(
          "pass" -> Json.string(member.pass.code),
          "ordinal" -> Json.number(member.ordinal),
          "target_bag_id" -> Json.number(member.target.value),
          "source_bag_id" -> Json.number(member.source.value),
          "separator_variable_ids" -> Json.array(
            member.separator.map(id => Json.number(id.value))
          )
        )
      }),
      "input_stage_ids" -> Json.array(
        trace.inputStageIds.map(id => Json.string(id.code))
      ),
      "diagnostic_statement_sha256" -> optional(
        stage.diagnosticStatementSha256
      )(Json.string),
      "diagnostic_character_count" -> optional(
        stage.diagnosticCharacterCount
      )(Json.number),
      "diagnostic_cte_prefix_count" -> optional(
        stage.diagnosticCtePrefixCount
      )(Json.number),
      "optimizer_estimate" -> optional(stage.estimate)(explainEstimateJson)
    )
  }

  private def factorAwareRenderedEvaluationJson(
      evaluation: PostgreSqlCandidateEvaluation
  ): Json.Value = {
    val evidence = evaluation.factorAwareEvidence.getOrElse(
      throw new IllegalStateException(
        "factor-aware evaluation has no stage evidence"
      )
    )
    Json.obj(
      "rank" -> Json.number(evaluation.structuralRank),
      "candidate_id" -> Json.string(evaluation.candidateId.value),
      "physical_variant_id" -> Json.string(evaluation.physicalVariantId.value),
      "rendered_statement_sha256" -> Json.string(
        evaluation.renderedStatementSha256
      ),
      "stages" -> Json.array(evidence.stages.collect {
        case stage if stage.trace.physicalForm.emitted =>
          Json.obj(
            "stage_id" -> Json.string(stage.trace.id.code),
            "statement_sha256" -> Json.string(
              stage.diagnosticStatementSha256.get
            ),
            "character_count" -> Json.number(
              stage.diagnosticCharacterCount.get
            ),
            "cte_prefix_count" -> Json.number(
              stage.diagnosticCtePrefixCount.get
            )
          )
      })
    )
  }

  private def countStageCostRenderedEvaluationJson(
      evaluation: PostgreSqlCandidateEvaluation
  ): Json.Value = {
    val evidence = evaluation.countStageCostEvidence.getOrElse(
      throw new IllegalStateException(
        "COUNT stage-cost evaluation has no stage evidence"
      )
    )
    Json.obj(
      "rank" -> Json.number(evaluation.structuralRank),
      "candidate_id" -> Json.string(evaluation.candidateId.value),
      "physical_variant_id" -> Json.string(evaluation.physicalVariantId.value),
      "rendered_statement_sha256" -> Json.string(
        evaluation.renderedStatementSha256
      ),
      "stages" -> Json.array(evidence.stages.collect {
        case stage if stage.trace.physicalForm.emitted =>
          Json.obj(
            "stage_id" -> Json.string(stage.trace.id.code),
            "statement_sha256" -> Json.string(
              stage.diagnosticStatementSha256.get
            ),
            "character_count" -> Json.number(
              stage.diagnosticCharacterCount.get
            ),
            "cte_prefix_count" -> Json.number(
              stage.diagnosticCtePrefixCount.get
            )
          )
      })
    )
  }

  private def factorAwareRenderedCandidateJson(
      candidate: PostgreSqlFactorAwareRenderedCandidateIdentity
  ): Json.Value =
    Json.obj(
      "rank" -> Json.number(candidate.structuralRank),
      "candidate_id" -> Json.string(candidate.candidateId.value),
      "physical_variant_id" -> Json.string(candidate.physicalVariantId.value),
      "rendered_statement_sha256" -> Json.string(
        candidate.renderedStatementSha256
      ),
      "stages" -> Json.array(candidate.stages.map(stage =>
        Json.obj(
          "stage_id" -> Json.string(stage.stageId.code),
          "statement_sha256" -> Json.string(stage.statementSha256),
          "character_count" -> Json.number(stage.characterCount),
          "cte_prefix_count" -> Json.number(stage.ctePrefixCount)
        )
      ))
    )

  private def factorAwareObservationJson(
      observation: PostgreSqlFactorAwareExplainObservation
  ): Json.Value =
    Json.obj(
      "physical_variant_id" -> Json.string(observation.physicalVariantId.value),
      "stage_id" -> Json.string(observation.stageId.code),
      "statement_sha256" -> Json.string(observation.statementSha256),
      "optimizer_estimate" -> explainEstimateJson(observation.estimate)
    )

  private def explainSourceJson(
      source: PostgreSqlExplainSourceEvaluation
  ): Json.Value =
    Json.obj(
      "source_statement_sha256" -> Json.string(source.sourceStatementSha256),
      "optimizer_estimate" -> explainEstimateJson(source.estimate)
    )

  private def explainRowGuardJson(
      evidence: PostgreSqlExplainRowGuardEvidence
  ): Json.Value =
    Json.obj(
      "factor" -> Json.number(evidence.factor),
      "ratio_numerator" -> Json.number(evidence.ratioNumerator),
      "ratio_denominator" -> Json.number(evidence.ratioDenominator),
      "within_guard" -> Json.bool(evidence.withinGuard)
    )

  private def explainEstimateJson(
      estimate: PostgreSqlExplainCostEstimate
  ): Json.Value =
    Json.obj(
      "source" -> Json.string(PostgreSqlExplainCostEstimate.Source),
      "metric" -> Json.string(PostgreSqlExplainCostEstimate.Metric),
      "units" -> Json.string(PostgreSqlExplainCostEstimate.Units),
      "explain_format" -> Json.string(
        PostgreSqlExplainCostEstimate.ExplainFormat
      ),
      "analyze" -> Json.bool(PostgreSqlExplainCostEstimate.Analyze),
      "settings" -> Json.bool(PostgreSqlExplainCostEstimate.Settings),
      "estimated" -> Json.bool(PostgreSqlExplainCostEstimate.Estimated),
      "runtime_measured" -> Json.bool(
        PostgreSqlExplainCostEstimate.RuntimeMeasured
      ),
      "startup_cost" -> Json.number(estimate.startupCost),
      "plan_total_cost" -> Json.number(estimate.planTotalCost),
      "plan_rows" -> Json.number(estimate.planRows),
      "plan_width" -> Json.number(estimate.planWidth)
    )

  private def explainProvenanceJson(
      provenance: PostgreSqlExplainCostProvenance
  ): Json.Value =
    Json.obj(
      "server_version_num" -> Json.number(provenance.serverVersionNum),
      "planner_settings_fingerprint_sha256" -> Json.string(
        provenance.plannerSettingsFingerprintSha256
      ),
      "planner_settings_fingerprint_scheme" -> Json.string(
        provenance.plannerSettingsFingerprintScheme
      ),
      "planner_setting_count" -> Json.number(provenance.plannerSettingCount)
    )

  private def physicalScoreJson(
      score: PostgreSqlStaticShapeScore
  ): Json.Value =
    Json.obj(
      "expanded_scan_count" -> Json.number(score.expandedScanCount),
      "materialized_distinct_support_count" -> Json.number(
        score.materializedDistinctSupportCount
      ),
      "cte_count" -> Json.number(score.cteCount),
      "rendered_term_count" -> Json.number(score.renderedTermCount),
      "sql_character_count" -> Json.number(score.sqlCharacterCount),
      "structural_rank" -> Json.number(score.structuralRank)
    )

  private def decompositionJson(
      decomposition: CoveredDecomposition
  ): Json.Value =
    Json.obj(
      "root_bag_id" -> Json.number(decomposition.root.value),
      "width" -> Json.number(decomposition.width),
      "bags" -> Json.array(
        decomposition.bags.sortBy(_.id.value).map { bag =>
          Json.obj(
            "id" -> Json.number(bag.id.value),
            "vertices" -> Json.array(
              bag.vertices.sortBy(_.value).map(id => Json.number(id.value))
            ),
            "cover_edge_ids" -> Json.array(
              bag.cover.sortBy(_.value).map(id => Json.number(id.value))
            )
          )
        }
      ),
      "tree_edges" -> Json.array(
        decomposition.treeEdges
          .sortBy(edge => (edge.first.value, edge.second.value))
          .map { edge =>
            Json.obj(
              "first" -> Json.number(edge.first.value),
              "second" -> Json.number(edge.second.value)
            )
          }
      )
    )

  private def bagFactorSummary(plan: BagFactorPlan): Json.Value = {
    Json.obj(
      "root_bag_id" -> Json.number(plan.root.value),
      "width_bound" -> Json.number(plan.sourceWidthBound),
      "require_connected_cover" -> Json.bool(plan.requireConnectedCover),
      "factor_count" -> Json.number(plan.factors.size),
      "ownership_count" -> Json.number(plan.ownership.size),
      "arc_count" -> Json.number(plan.arcs.size),
      "bottom_up_step_count" -> Json.number(plan.schedule.bottomUp.size),
      "top_down_step_count" -> Json.number(plan.schedule.topDown.size)
    )
  }

  private def renderingSummary(
      artifact: PostgreSqlRewriteArtifact
  ): Json.Value = {
    val rendered = artifact.renderedQuery
    val statistics = rendered.statistics
    val reduction = rendered.trace.reduction
    Json.obj(
      "output_column_count" -> Json.number(rendered.output.size),
      "expanded_scan_count" -> Json.number(statistics.expandedScanCount),
      "materialized_distinct_support_count" -> Json.number(
        rendered.trace.bags.count(
          _.supportStrategy ==
            PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
        )
      ),
      "rendered_term_count" -> Json.number(statistics.renderedTermCount),
      "cte_count" -> Json.number(statistics.cteCount),
      "sql_character_count" -> Json.number(statistics.sqlCharacterCount),
      "reduction_schedule" -> Json.string(reduction.mode.id),
      "reduction_order" -> Json.string(reduction.reductionOrder),
      "canonical_bottom_up_step_count" -> Json.number(
        reduction.canonicalBottomUpStepCount
      ),
      "canonical_top_down_step_count" -> Json.number(
        reduction.canonicalTopDownStepCount
      ),
      "selected_bottom_up_step_count" -> Json.number(
        reduction.selectedBottomUpStepCount
      ),
      "selected_top_down_step_count" -> Json.number(
        reduction.selectedTopDownStepCount
      ),
      "physical_reduction_cte_count" -> Json.number(
        reduction.physicalReductionCteCount
      )
    )
  }

  private def outcomeStatus(outcome: RewriteOutcome): String = outcome match {
    case _: RewriteOutcome.Rewritten     => "rewritten"
    case _: RewriteOutcome.NotApplicable => "not_applicable"
    case _: RewriteOutcome.InvalidQuery  => "invalid_query"
    case _: RewriteOutcome.Failed        => "failed"
  }

  private def optional[A](value: Option[A])(
      renderValue: A => Json.Value
  ): Json.Value = value match {
    case Some(supplied) => renderValue(supplied)
    case None           => Json.Null
  }

  private def canonicalBigIntJson(value: BigInt): Json.Value = {
    require(value != null && value.signum >= 0)
    Json.string(value.toString)
  }

  private def sha256(value: String): String = {
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
    val alphabet = "0123456789abcdef"
    val result = new StringBuilder(bytes.length * 2)
    bytes.foreach { byte =>
      val unsigned = byte & 0xff
      result.append(alphabet.charAt(unsigned >>> 4))
      result.append(alphabet.charAt(unsigned & 0x0f))
    }
    result.result()
  }

  private val SensitiveContextFragments: Vector[String] = Vector(
    "password",
    "credential",
    "secret",
    "token",
    "exception",
    "throwable",
    "url",
    "user",
    "jdbc_url",
    "jdbcurl",
    "database_url",
    "datasource",
    "data_source",
    "connection_string",
    "hostname",
    "host_name",
    "database_user",
    "db_user",
    "username",
    "file_path",
    "filesystem_path",
    "absolute_path"
  )

  private def safeContext(
      context: Map[String, String]
  ): Vector[(String, String)] =
    context.iterator
      .filterNot { case (key, _) =>
        val normalized = key.toLowerCase(java.util.Locale.ROOT)
        normalized == "path" || normalized.endsWith("_path") ||
        SensitiveContextFragments.exists(normalized.contains)
      }
      .toVector
      .sortBy(_._1)

  private object Json {
    sealed trait Value
    case object Null extends Value
    final case class StringValue(value: String) extends Value
    final case class NumberValue(value: String) extends Value
    final case class BooleanValue(value: Boolean) extends Value
    final case class ArrayValue(values: Vector[Value]) extends Value
    final case class ObjectValue(fields: Vector[(String, Value)]) extends Value

    def string(value: String): Value = StringValue(value)
    def number(value: Int): Value = NumberValue(value.toString)
    def number(value: Long): Value = NumberValue(value.toString)
    def number(value: BigInt): Value = NumberValue(value.toString)
    def number(value: BigDecimal): Value =
      NumberValue(if (value.signum == 0) "0" else value.bigDecimal.toString)
    def bool(value: Boolean): Value = BooleanValue(value)
    def array(values: Vector[Value]): Value = ArrayValue(values)
    def obj(fields: (String, Value)*): Value = ObjectValue(fields.toVector)
    def obj(fields: Vector[(String, Value)]): Value = ObjectValue(fields)

    def render(value: Value): String = {
      val output = new StringBuilder
      append(value, output)
      output.result()
    }

    private def append(value: Value, output: StringBuilder): Unit = value match {
      case Null => output.append("null")
      case StringValue(text) => appendString(text, output)
      case NumberValue(number) => output.append(number)
      case BooleanValue(flag) => output.append(if (flag) "true" else "false")
      case ArrayValue(values) =>
        output.append('[')
        values.zipWithIndex.foreach { case (element, index) =>
          if (index > 0) output.append(',')
          append(element, output)
        }
        output.append(']')
      case ObjectValue(fields) =>
        output.append('{')
        fields.zipWithIndex.foreach { case ((key, fieldValue), index) =>
          if (index > 0) output.append(',')
          appendString(key, output)
          output.append(':')
          append(fieldValue, output)
        }
        output.append('}')
    }

    private def appendString(value: String, output: StringBuilder): Unit = {
      output.append('"')
      var index = 0
      while (index < value.length) {
        val character = value.charAt(index)
        character match {
          case '"' => output.append("\\\"")
          case '\\' => output.append("\\\\")
          case '\b' => output.append("\\b")
          case '\f' => output.append("\\f")
          case '\n' => output.append("\\n")
          case '\r' => output.append("\\r")
          case '\t' => output.append("\\t")
          case control if control < ' ' => appendUnicode(control, output)
          case '\u2028' | '\u2029' => appendUnicode(character, output)
          case high if Character.isHighSurrogate(high) =>
            if (
              index + 1 < value.length &&
              Character.isLowSurrogate(value.charAt(index + 1))
            ) {
              output.append(high)
              index += 1
              output.append(value.charAt(index))
            } else appendUnicode(high, output)
          case low if Character.isLowSurrogate(low) => appendUnicode(low, output)
          case other => output.append(other)
        }
        index += 1
      }
      output.append('"')
    }

    private def appendUnicode(value: Char, output: StringBuilder): Unit = {
      val alphabet = "0123456789abcdef"
      output.append("\\u")
      output.append(alphabet.charAt((value.toInt >>> 12) & 0x0f))
      output.append(alphabet.charAt((value.toInt >>> 8) & 0x0f))
      output.append(alphabet.charAt((value.toInt >>> 4) & 0x0f))
      output.append(alphabet.charAt(value.toInt & 0x0f))
    }
  }
}
