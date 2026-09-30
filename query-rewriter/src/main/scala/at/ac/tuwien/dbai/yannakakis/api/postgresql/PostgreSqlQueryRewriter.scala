package at.ac.tuwien.dbai.yannakakis.api.postgresql

import javax.sql.DataSource

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateInputError,
  CandidatePlanDecision,
  CandidateResourceLimits,
  CandidateSearchInput,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlanningError,
  BagFactorPlanningLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlEnumerationRenderingProfile,
  PostgreSqlRenderingError,
  PostgreSqlRenderingLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{
  SoftBagGenerator,
  SoftResourceLimits
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  DiagnosticSeverity,
  QueryRewriter,
  RewriteDiagnostic,
  RewriteOutcome,
  RewriteReasonCode,
  RewriteRequest,
  TargetDialect
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteQueryAnalyzer,
  CalciteSqlPlannedQuery,
  CalciteSqlPlanner,
  CalciteSqlPlanningError,
  CalciteSqlPlanningStage,
  QueryHypergraphConverter,
  QueryResultShape
}
import org.apache.calcite.schema.SchemaPlus
import org.apache.calcite.sql.parser.SqlParser
import org.apache.calcite.tools.FrameworkConfig

import scala.util.control.NonFatal

/**
  * End-to-end Calcite/CandidateTD/PostgreSQL rewriter.
  *
  * Selection is explicit and versioned. The default remains structural rank
  * one; explicit rank/id selection and the static PostgreSQL shape heuristic
  * never fall back to another or a partially evaluated candidate set.
  */
final class PostgreSqlQueryRewriter private (
    sqlPlanner: CalciteSqlPlanner,
    softLimits: SoftResourceLimits,
    candidateLimits: CandidateResourceLimits,
    bagFactorLimits: BagFactorPlanningLimits,
    renderingLimits: PostgreSqlRenderingLimits,
    selectionLimits: PostgreSqlCandidateSelectionLimits,
    explainCostEvaluator: Option[PostgreSqlExplainCostEvaluator],
    explainLimits: PostgreSqlExplainSelectionLimits,
    factorAwareLimits: PostgreSqlFactorAwareSelectionLimits,
    physicalPortfolioLimits: PostgreSqlPhysicalPortfolioLimits,
    physicalPortfolioEvaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits
) extends QueryRewriter {

  /** Runs the same rewrite as [[rewrite]] while retaining stage evidence. */
  def rewriteDetailed(request: RewriteRequest): PostgreSqlRewriteResult = {
    require(request != null, "request must not be null")
    val detailed = run(request)
    if (detailed.outcome.originalSql != request.originalSql) {
      throw new IllegalStateException(
        "detailed rewriter outcome did not preserve the request's original SQL"
      )
    }
    detailed
  }

  /**
    * Additive, policy-neutral acquisition boundary for the enumeration
    * campaign. It performs one SQL planning/analysis/search pass and then
    * compiles every exact retained rank once under full two-pass reduction
    * with planner-controlled CTEs. It never selects a winner and does not
    * alter the public rewrite/JSON contracts.
    */
  private[yannakakis] def produceEnumerationBatch(
      request: PostgreSqlEnumerationBatchRequest
  ): PostgreSqlEnumerationBatchResult =
    produceEnumerationBatchWithProfile(
      request,
      PostgreSqlEnumerationRenderingProfile.default
    )

  /** Frozen campaign-v2 compatibility boundary; never a v3 timing input. */
  private[yannakakis] def produceEnumerationBatchLegacyV2(
      request: PostgreSqlEnumerationBatchRequest
  ): PostgreSqlEnumerationBatchResult =
    produceEnumerationBatchWithProfile(
      request,
      PostgreSqlEnumerationRenderingProfile.LegacyV1
    )

  private def produceEnumerationBatchWithProfile(
      request: PostgreSqlEnumerationBatchRequest,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): PostgreSqlEnumerationBatchResult = {
    require(request != null, "enumeration batch request must not be null")
    require(
      enumerationRenderingProfile != null,
      "enumeration rendering profile must not be null"
    )
    val originalSql = request.originalSql
    val diagnosticLimits =
      PostgreSqlRetainedCandidateBatchCompiler.DefaultDiagnosticLimits
    val producerSearchContract =
      PostgreSqlEnumerationProducerSearchContract.capture(
        request,
        softLimits,
        candidateLimits,
        bagFactorLimits,
        renderingLimits,
        diagnosticLimits
      )
    var report = PostgreSqlRewriteReport(
      completedPhase = PostgreSqlRewritePhase.RequestAccepted,
      analysis = None,
      hypergraph = None,
      softGeneration = None,
      admissibleGeneration = None,
      candidateInput = None,
      candidateDecision = None,
      candidateSelectionDecision = None,
      bagFactorPlan = None,
      artifact = None
    )

    def failure(outcome: RewriteOutcome): PostgreSqlEnumerationBatchResult =
      PostgreSqlEnumerationBatchResult.fromRewriteFailure(
        request,
        PostgreSqlRewriteResult(outcome, report),
        producerSearchContract
      )

    try {
      if (originalSql.trim.isEmpty) {
        return failure(
          RewriteOutcome.InvalidQuery(
            originalSql,
            Vector(
              RewriteDiagnostic(
                RewriteReasonCode.InvalidSql,
                "SQL statement must not be blank",
                context = Map("stage" -> CalciteSqlPlanningStage.Parsing.id)
              )
            )
          )
        )
      }

      val planned = invokeSqlPlanner(originalSql) match {
        case Left(problem) =>
          val diagnostic = planningDiagnostic(problem)
          val outcome =
            if (problem.stage.invalidatesSql)
              RewriteOutcome.InvalidQuery(originalSql, Vector(diagnostic))
            else RewriteOutcome.Failed(originalSql, Vector(diagnostic))
          return failure(outcome)
        case Right(value) => value
      }
      report = report.copy(completedPhase = PostgreSqlRewritePhase.SqlPlanned)

      val analysis = CalciteQueryAnalyzer.analyze(planned)
      report = report.copy(
        completedPhase = PostgreSqlRewritePhase.QueryAnalyzed,
        analysis = Some(analysis)
      )
      if (analysis.diagnostics.nonEmpty) {
        val outcome =
          if (
            analysis.diagnostics.exists(
              _.severity == DiagnosticSeverity.Error
            )
          ) RewriteOutcome.Failed(originalSql, analysis.diagnostics)
          else RewriteOutcome.NotApplicable(originalSql, analysis.diagnostics)
        return failure(outcome)
      }
      if (analysis.query.resultShape != QueryResultShape.EnumerationProjection) {
        return failure(
          RewriteOutcome.NotApplicable(
            originalSql,
            Vector(
              RewriteDiagnostic(
                RewriteReasonCode.UnsupportedQueryShape,
                "The enumeration campaign producer accepts enumeration projection only",
                DiagnosticSeverity.Warning,
                Map(
                  "stage" -> "enumeration-batch-applicability",
                  "resultShape" -> analysis.query.resultShape.id
                )
              )
            )
          )
        )
      }

      val hypergraph = QueryHypergraphConverter.convert(analysis.query) match {
        case Left(diagnostics) =>
          val outcome =
            if (diagnostics.exists(_.severity == DiagnosticSeverity.Error))
              RewriteOutcome.Failed(originalSql, diagnostics)
            else RewriteOutcome.NotApplicable(originalSql, diagnostics)
          return failure(outcome)
        case Right(value) => value
      }
      report = report.copy(
        completedPhase = PostgreSqlRewritePhase.HypergraphBuilt,
        hypergraph = Some(hypergraph)
      )

      if (
        hypergraph.variables.isEmpty ||
        hypergraph.edges.exists(_.vertices.isEmpty)
      ) {
        return failure(
          RewriteOutcome.NotApplicable(
            originalSql,
            Vector(
              RewriteDiagnostic(
                RewriteReasonCode.UnsupportedQueryShape,
                "CandidateTD rewriting requires every relation occurrence to participate in an inner equality",
                DiagnosticSeverity.Warning,
                Map(
                  "relationOccurrences" -> hypergraph.edges.size.toString,
                  "joinVariables" -> hypergraph.variables.size.toString
                )
              )
            )
          )
        )
      }

      val softGeneration = SoftBagGenerator.generate(
        hypergraph,
        request.widthBound,
        softLimits
      ) match {
        case Left(problem) =>
          return failure(
            failed(
              originalSql,
              RewriteReasonCode.PlanningFailed,
              "Soft-bag generation failed",
              "soft-bag-generation",
              problem.code,
              problem.message
            )
          )
        case Right(value) => value
      }
      val admissible = softGeneration.admissible(
        request.requireConnectedCover
      )
      report = report.copy(
        completedPhase = PostgreSqlRewritePhase.SoftBagsGenerated,
        softGeneration = Some(softGeneration),
        admissibleGeneration = Some(admissible)
      )

      if (request.requireConnectedCover && admissible.candidates.isEmpty) {
        return failure(
          RewriteOutcome.NotApplicable(
            originalSql,
            Vector(
              RewriteDiagnostic(
                RewriteReasonCode.NoConnectedCover,
                "No soft bag has an admissible connected cover",
                DiagnosticSeverity.Warning,
                Map(
                  "widthBound" -> request.widthBound.toString,
                  "generatedCandidateCount" ->
                    softGeneration.candidates.size.toString
                )
              )
            )
          )
        )
      }

      val candidateInput = CandidateSearchInput
        .create(hypergraph, request.widthBound, admissible) match {
        case Left(problems) =>
          return failure(
            RewriteOutcome.Failed(
              originalSql,
              candidateInputDiagnostics(problems)
            )
          )
        case Right(value) => value
      }
      report = report.copy(
        completedPhase = PostgreSqlRewritePhase.CandidateInputBuilt,
        candidateInput = Some(candidateInput)
      )

      val decision = TopKCandidateTD.enumerate(
        candidateInput,
        request.planLimit,
        candidateLimits
      ) match {
        case Left(problem) =>
          return failure(
            failed(
              originalSql,
              RewriteReasonCode.PlanningFailed,
              "CandidateTD search failed",
              "candidate-search",
              problem.code,
              problem.message
            )
          )
        case Right(value) => value
      }
      report = report.copy(
        completedPhase = PostgreSqlRewritePhase.CandidateSearchCompleted,
        candidateDecision = Some(decision)
      )

      val found = decision match {
        case rejected: CandidatePlanDecision.Rejected =>
          return failure(
            RewriteOutcome.NotApplicable(
              originalSql,
              Vector(
                RewriteDiagnostic(
                  RewriteReasonCode.NoDecomposition,
                  "No CandidateTD exists for the requested fixed width",
                  DiagnosticSeverity.Warning,
                  Map(
                    "widthBound" -> request.widthBound.toString,
                    "planLimit" -> rejected.requestedLimit.toString,
                    "candidateBagCount" ->
                      rejected.statistics.base.candidateBagCount.toString
                  )
                )
              )
            )
          )
        case value: CandidatePlanDecision.Found => value
      }

      val batch = PostgreSqlRetainedCandidateBatchCompiler
        .compileWithEnumerationProfile(
          analysis.query,
          candidateInput,
          found,
          bagFactorLimits,
          renderingLimits,
          diagnosticLimits,
          enumerationRenderingProfile
        ) match {
        case Left(
              PostgreSqlRetainedCandidateBatchFailure.InvalidCandidateSet(
                detail
              )
            ) =>
          return failure(
            internalFailure(originalSql, detail, "enumeration-batch")
          )
        case Left(
              PostgreSqlRetainedCandidateBatchFailure.InapplicableResultShape(
                resultShape
              )
            ) =>
          return failure(
            RewriteOutcome.NotApplicable(
              originalSql,
              Vector(
                RewriteDiagnostic(
                  RewriteReasonCode.UnsupportedQueryShape,
                  "The enumeration batch compiler rejected the analyzed result shape",
                  DiagnosticSeverity.Warning,
                  Map(
                    "stage" -> "enumeration-batch",
                    "resultShape" -> resultShape
                  )
                )
              )
            )
          )
        case Left(
              PostgreSqlRetainedCandidateBatchFailure.SqlTransportFailed(
                rank,
                candidateId,
                assetId,
                reason
              )
            ) =>
          return failure(
            RewriteOutcome.Failed(
              originalSql,
              Vector(
                RewriteDiagnostic(
                  RewriteReasonCode.RenderingFailed,
                  "A retained candidate produced SQL outside the campaign transport envelope",
                  context = Map(
                    "stage" -> "enumeration-batch-transport",
                    "candidateRank" -> rank.toString,
                    "candidateId" -> candidateId.value,
                    "assetId" -> assetId,
                    "reason" -> reason.code
                  )
                )
              )
            )
          )
        case Left(
              PostgreSqlRetainedCandidateBatchFailure.CompilationFailed(
                rank,
                candidateId,
                cause
              )
            ) =>
          val context = Map(
            "stage" -> "enumeration-batch",
            "candidateRank" -> rank.toString,
            "candidateId" -> candidateId.value
          )
          cause match {
            case PostgreSqlCandidateCompiler.Failure.BagFactorPlanningFailed(
                  _,
                  problems
                ) =>
              return failure(
                RewriteOutcome.Failed(
                  originalSql,
                  bagFactorDiagnostics(problems).map(diagnostic =>
                    diagnostic.copy(context = diagnostic.context ++ context)
                  )
                )
              )
            case PostgreSqlCandidateCompiler.Failure.Inapplicable(
                  _,
                  bagFactorPlan,
                  reason
                ) =>
              report = report.copy(
                completedPhase = PostgreSqlRewritePhase.BagFactorPlanned,
                bagFactorPlan = Some(bagFactorPlan)
              )
              return failure(
                RewriteOutcome.NotApplicable(
                  originalSql,
                  Vector(
                    RewriteDiagnostic(
                      RewriteReasonCode.UnsupportedQueryShape,
                      "The retained candidate is outside the enumeration compiler fragment",
                      DiagnosticSeverity.Warning,
                      context + ("reason" -> reason)
                    )
                  )
                )
              )
            case PostgreSqlCandidateCompiler.Failure.RenderingFailed(
                  _,
                  bagFactorPlan,
                  problems
                ) =>
              report = report.copy(
                completedPhase = PostgreSqlRewritePhase.BagFactorPlanned,
                bagFactorPlan = Some(bagFactorPlan)
              )
              return failure(
                renderingOutcome(originalSql, problems, context)
              )
          }
        case Right(value) => value
      }

      val artifact = PostgreSqlEnumerationBatchArtifact(
        request,
        analysis,
        hypergraph,
        softGeneration,
        admissible,
        candidateInput,
        batch,
        producerSearchContract
      )
      report = report.copy(completedPhase = PostgreSqlRewritePhase.Rendered)
      PostgreSqlEnumerationBatchResult(
        request,
        PostgreSqlEnumerationBatchStatus.Produced,
        PostgreSqlRewritePhase.Rendered,
        Vector.empty,
        report,
        producerSearchContract,
        Some(artifact)
      )
    } catch {
      case NonFatal(problem) =>
        failure(
          RewriteOutcome.Failed(
            originalSql,
            Vector(
              RewriteDiagnostic(
                RewriteReasonCode.InternalError,
                "An unexpected internal enumeration-batch failure occurred",
                context = Map(
                  "completedPhase" -> report.completedPhase.id,
                  "exceptionClass" -> problem.getClass.getName
                )
              )
            )
          )
        )
    }
  }

  override protected def doRewrite(request: RewriteRequest): RewriteOutcome =
    run(request).outcome

  private def run(request: RewriteRequest): PostgreSqlRewriteResult = {
    val originalSql = request.originalSql
    var report = PostgreSqlRewriteReport(
      completedPhase = PostgreSqlRewritePhase.RequestAccepted,
      analysis = None,
      hypergraph = None,
      softGeneration = None,
      admissibleGeneration = None,
      candidateInput = None,
      candidateDecision = None,
      candidateSelectionDecision = None,
      bagFactorPlan = None,
      artifact = None
    )

    def result(outcome: RewriteOutcome): PostgreSqlRewriteResult =
      PostgreSqlRewriteResult(outcome, report)

    try {
      if (originalSql.trim.isEmpty) {
        return result(
          RewriteOutcome.InvalidQuery(
            originalSql,
            Vector(
              RewriteDiagnostic(
                RewriteReasonCode.InvalidSql,
                "SQL statement must not be blank",
                context = Map("stage" -> CalciteSqlPlanningStage.Parsing.id)
              )
            )
          )
        )
      }

      if (request.targetDialect != TargetDialect.PostgreSql) {
        return result(
          RewriteOutcome.NotApplicable(
            originalSql,
            Vector(
              RewriteDiagnostic(
                RewriteReasonCode.UnsupportedQueryShape,
                "The requested target dialect has no PostgreSQL renderer",
                DiagnosticSeverity.Warning,
                Map("targetDialect" -> request.targetDialect.id)
              )
            )
          )
        )
      }

      val planning = invokeSqlPlanner(originalSql)
      planning match {
        case Left(problem) =>
          val diagnostic = planningDiagnostic(problem)
          val outcome =
            if (problem.stage.invalidatesSql)
              RewriteOutcome.InvalidQuery(originalSql, Vector(diagnostic))
            else RewriteOutcome.Failed(originalSql, Vector(diagnostic))
          return result(outcome)
        case Right(planned) =>
          report = report.copy(
            completedPhase = PostgreSqlRewritePhase.SqlPlanned
          )

          val analysis = CalciteQueryAnalyzer.analyze(planned)
          report = report.copy(
            completedPhase = PostgreSqlRewritePhase.QueryAnalyzed,
            analysis = Some(analysis)
          )
          if (analysis.diagnostics.nonEmpty) {
            val outcome =
              if (analysis.diagnostics.exists(
                    _.severity == DiagnosticSeverity.Error
                  ))
                RewriteOutcome.Failed(originalSql, analysis.diagnostics)
              else
                RewriteOutcome.NotApplicable(originalSql, analysis.diagnostics)
            return result(outcome)
          }

          PostgreSqlCandidateSelector
            .aggregateSelectionInapplicability(
              analysis.query,
              request.options.candidateSelection,
              request.options.reductionMode
            )
            .foreach { reason =>
              return result(
                selectionInapplicableOutcome(
                  originalSql,
                  PostgreSqlCandidateSelectionFailure.Inapplicable(
                    request.options.candidateSelection,
                    analysis.query.resultShape.id,
                    reason
                  )
                )
              )
            }

          val hypergraph = QueryHypergraphConverter.convert(analysis.query) match {
            case Left(diagnostics) =>
              val outcome =
                if (diagnostics.exists(_.severity == DiagnosticSeverity.Error))
                  RewriteOutcome.Failed(originalSql, diagnostics)
                else RewriteOutcome.NotApplicable(originalSql, diagnostics)
              return result(outcome)
            case Right(value) => value
          }
          report = report.copy(
            completedPhase = PostgreSqlRewritePhase.HypergraphBuilt,
            hypergraph = Some(hypergraph)
          )

          if (
            hypergraph.variables.isEmpty ||
            hypergraph.edges.exists(_.vertices.isEmpty)
          ) {
            return result(
              RewriteOutcome.NotApplicable(
                originalSql,
                Vector(
                  RewriteDiagnostic(
                    RewriteReasonCode.UnsupportedQueryShape,
                    "CandidateTD rewriting requires every relation occurrence to participate in an inner equality",
                    DiagnosticSeverity.Warning,
                    Map(
                      "relationOccurrences" -> hypergraph.edges.size.toString,
                      "joinVariables" -> hypergraph.variables.size.toString
                    )
                  )
                )
              )
            )
          }

          val softGeneration = SoftBagGenerator.generate(
            hypergraph,
            request.options.widthBound,
            softLimits
          ) match {
            case Left(problem) =>
              return result(
                failed(
                  originalSql,
                  RewriteReasonCode.PlanningFailed,
                  "Soft-bag generation failed",
                  "soft-bag-generation",
                  problem.code,
                  problem.message
                )
              )
            case Right(value) => value
          }
          val admissible = softGeneration.admissible(
            request.options.requireConnectedCover
          )
          report = report.copy(
            completedPhase = PostgreSqlRewritePhase.SoftBagsGenerated,
            softGeneration = Some(softGeneration),
            admissibleGeneration = Some(admissible)
          )

          if (
            request.options.requireConnectedCover &&
            admissible.candidates.isEmpty
          ) {
            return result(
              RewriteOutcome.NotApplicable(
                originalSql,
                Vector(
                  RewriteDiagnostic(
                    RewriteReasonCode.NoConnectedCover,
                    "No soft bag has an admissible connected cover",
                    DiagnosticSeverity.Warning,
                    Map(
                      "widthBound" -> request.options.widthBound.toString,
                      "generatedCandidateCount" ->
                        softGeneration.candidates.size.toString
                    )
                  )
                )
              )
            )
          }

          val candidateInput = CandidateSearchInput
            .create(hypergraph, request.options.widthBound, admissible) match {
            case Left(problems) =>
              return result(
                RewriteOutcome.Failed(
                  originalSql,
                  candidateInputDiagnostics(problems)
                )
              )
            case Right(value) => value
          }
          report = report.copy(
            completedPhase = PostgreSqlRewritePhase.CandidateInputBuilt,
            candidateInput = Some(candidateInput)
          )

          val decision = TopKCandidateTD.enumerate(
            candidateInput,
            request.options.planLimit,
            candidateLimits
          ) match {
            case Left(problem) =>
              return result(
                failed(
                  originalSql,
                  RewriteReasonCode.PlanningFailed,
                  "CandidateTD search failed",
                  "candidate-search",
                  problem.code,
                  problem.message
                )
              )
            case Right(value) => value
          }
          report = report.copy(
            completedPhase = PostgreSqlRewritePhase.CandidateSearchCompleted,
            candidateDecision = Some(decision)
          )

          decision match {
            case rejected: CandidatePlanDecision.Rejected =>
              return result(
                RewriteOutcome.NotApplicable(
                  originalSql,
                  Vector(
                    RewriteDiagnostic(
                      RewriteReasonCode.NoDecomposition,
                      "No CandidateTD exists for the requested fixed width",
                      DiagnosticSeverity.Warning,
                      Map(
                        "widthBound" -> request.options.widthBound.toString,
                        "planLimit" -> rejected.requestedLimit.toString,
                        "candidateBagCount" ->
                          rejected.statistics.base.candidateBagCount.toString
                      )
                    )
                  )
                )
              )

            case found: CandidatePlanDecision.Found =>
              if (
                request.options.candidateSelection ==
                  CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
              ) {
                val selected = PostgreSqlPhysicalPortfolioSelector.select(
                  analysis.query,
                  candidateInput,
                  found,
                  originalSql,
                  explainCostEvaluator,
                  candidateLimits,
                  physicalPortfolioLimits,
                  physicalPortfolioEvaluationLimits,
                  bagFactorLimits,
                  renderingLimits
                )

                def physicalAttempt(
                    physicalDecision: PostgreSqlPhysicalPortfolioDecision
                ): PostgreSqlPhysicalPortfolioAttempt =
                  PostgreSqlPhysicalPortfolioAttempt(
                    originalSql,
                    analysis.query,
                    hypergraph,
                    softGeneration,
                    admissible,
                    candidateInput,
                    found,
                    candidateLimits,
                    physicalPortfolioLimits,
                    renderingLimits,
                    bagFactorLimits,
                    physicalPortfolioEvaluationLimits,
                    found.plans.size < found.requestedLimit,
                    physicalDecision
                  )

                selected match {
                  case Left(physicalDecision) =>
                    val attempt = physicalAttempt(physicalDecision)
                    report = report.copy(
                      completedPhase =
                        PostgreSqlRewritePhase.CandidateSelectionCompleted,
                      physicalPortfolioAttempt = Some(attempt)
                    )
                    return result(
                      PostgreSqlPhysicalPortfolioOutcome.build(
                        originalSql,
                        physicalDecision,
                        None
                      )
                    )
                  case Right(value) =>
                    val attempt = physicalAttempt(value.decision)
                    val artifact = PostgreSqlPhysicalPortfolioArtifact(
                      attempt,
                      value.plan,
                      value.configuredPlan,
                      value.renderedQuery
                    )
                    report = report.copy(
                      completedPhase = PostgreSqlRewritePhase.Rendered,
                      physicalPortfolioAttempt = Some(attempt),
                      physicalPortfolioArtifact = Some(artifact)
                    )
                    return result(
                      PostgreSqlPhysicalPortfolioOutcome.build(
                        originalSql,
                        value.decision,
                        Some(value.renderedQuery.sql)
                      )
                    )
                }
              }
              val selected = PostgreSqlCandidateSelector.select(
                analysis.query,
                candidateInput,
                found,
                request.options.candidateSelection,
                request.options.reductionMode,
                selectionLimits,
                bagFactorLimits,
                renderingLimits,
                explainCostEvaluator,
                explainLimits,
                Some(originalSql),
                candidateLimits,
                factorAwareLimits
              ) match {
                case Left(problem) =>
                  report = report.copy(
                    candidateSelectionDecision = problem.decision
                  )
                  problem match {
                    case failure: PostgreSqlCandidateSelectionFailure.InvalidCandidateSet =>
                      return result(
                        internalFailure(
                          originalSql,
                          failure.detail,
                          "candidate-selection"
                        )
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.Inapplicable =>
                      return result(
                        selectionInapplicableOutcome(originalSql, failure)
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.Unavailable =>
                      report = report.copy(
                        completedPhase =
                          PostgreSqlRewritePhase.CandidateSelectionCompleted
                      )
                      return result(
                        selectionUnavailableOutcome(originalSql, failure.report)
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.BagFactorPlanningFailed =>
                      return result(
                        RewriteOutcome.Failed(
                          originalSql,
                          withCandidateContext(
                            bagFactorDiagnostics(failure.problems),
                            failure.plan,
                            failure.report.policy.id
                          )
                        )
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.RenderingFailed =>
                      report = report.copy(
                        completedPhase = PostgreSqlRewritePhase.BagFactorPlanned,
                        bagFactorPlan = Some(failure.bagFactorPlan)
                      )
                      return result(
                        renderingOutcome(
                          originalSql,
                          failure.problems,
                          candidateContext(
                            failure.plan,
                            failure.report.policy.id
                          )
                        )
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.ResourceLimitExceeded =>
                      return result(
                        selectionResourceOutcome(originalSql, failure)
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed =>
                      return result(
                        explainCostFailureOutcome(originalSql, failure)
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.NoAdmissibleEstimate =>
                      report = report.copy(
                        completedPhase =
                          PostgreSqlRewritePhase.CandidateSelectionCompleted
                      )
                      return result(
                        noAdmissibleEstimateOutcome(originalSql, failure)
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed =>
                      return result(
                        factorAwareFailureOutcome(originalSql, failure)
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.FactorAwarePlanningFailed =>
                      return result(
                        RewriteOutcome.Failed(
                          originalSql,
                          withCandidateContext(
                            bagFactorDiagnostics(failure.problems),
                            failure.plan,
                            failure.report.policy.id
                          )
                        )
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.FactorAwareRenderingFailed =>
                      report = report.copy(
                        completedPhase = PostgreSqlRewritePhase.BagFactorPlanned,
                        bagFactorPlan = Some(failure.bagFactorPlan)
                      )
                      return result(
                        renderingOutcome(
                          originalSql,
                          failure.problems,
                          candidateContext(
                            failure.plan,
                            failure.report.policy.id
                          )
                        )
                      )

                    case failure: PostgreSqlCandidateSelectionFailure.FactorAwareResourceLimitExceeded =>
                      return result(
                        factorAwareResourceOutcome(originalSql, failure)
                      )
                  }
                case Right(value) => value
              }

              report = report.copy(
                completedPhase =
                  PostgreSqlRewritePhase.CandidateSelectionCompleted,
                candidateSelectionDecision = Some(selected.decision)
              )
              report = report.copy(
                completedPhase = PostgreSqlRewritePhase.BagFactorPlanned,
                bagFactorPlan = Some(selected.bagFactorPlan)
              )

              val artifact = PostgreSqlRewriteArtifact(
                query = analysis.query,
                hypergraph = hypergraph,
                softGeneration = softGeneration,
                admissibleGeneration = admissible,
                candidateInput = candidateInput,
                candidateSearch = found,
                candidateSelection = selected.decision,
                selectedPlan = selected.plan,
                bagFactorPlan = selected.bagFactorPlan,
                renderedQuery = selected.renderedQuery
              )
              report = report.copy(
                completedPhase = PostgreSqlRewritePhase.Rendered,
                artifact = Some(artifact)
              )
              result(
                RewriteOutcome.Rewritten(
                  originalSql,
                  Vector(selected.renderedQuery.sql)
                )
              )
          }
      }
    } catch {
      case NonFatal(problem) =>
        result(
          RewriteOutcome.Failed(
            originalSql,
            Vector(
              RewriteDiagnostic(
                RewriteReasonCode.InternalError,
                "An unexpected internal rewrite failure occurred",
                context = Map(
                  "completedPhase" -> report.completedPhase.id,
                  "exceptionClass" -> problem.getClass.getName
                )
              )
            )
          )
        )
    }
  }

  private def invokeSqlPlanner(
      originalSql: String
  ): Either[CalciteSqlPlanningError, CalciteSqlPlannedQuery] = {
    val planned =
      try CalciteSqlPlanner.planWithSourceEvidence(sqlPlanner, originalSql)
      catch {
        case NonFatal(problem) =>
          return Left(
            CalciteSqlPlanningError(
              CalciteSqlPlanningStage.Boundary,
              "Injected Calcite planning boundary failed",
              Some(problem.getClass.getName)
            )
          )
      }

    if (planned eq null) {
      Left(
        CalciteSqlPlanningError(
          CalciteSqlPlanningStage.Boundary,
          "Injected Calcite planning boundary returned null"
        )
      )
    } else
      planned match {
        case Left(null) =>
          Left(
            CalciteSqlPlanningError(
              CalciteSqlPlanningStage.Boundary,
              "Injected Calcite planning boundary returned a null error"
            )
          )
        case Right(null) =>
          Left(
            CalciteSqlPlanningError(
              CalciteSqlPlanningStage.Boundary,
              "Injected Calcite planning boundary returned a null planned query"
            )
          )
        case other => other
      }
  }

  private def selectionInapplicableOutcome(
      originalSql: String,
      failure: PostgreSqlCandidateSelectionFailure.Inapplicable
  ): RewriteOutcome.NotApplicable =
    RewriteOutcome.NotApplicable(
      originalSql,
      Vector(
        RewriteDiagnostic(
          RewriteReasonCode.UnsupportedQueryShape,
          "The requested PostgreSQL candidate selector is not applicable to the analyzed result shape",
          DiagnosticSeverity.Warning,
          Map(
            "selectionPolicy" -> failure.policy.id,
            "resultShape" -> failure.resultShape,
            "reason" -> failure.reason
          )
        )
      )
    )

  private def planningDiagnostic(
      problem: CalciteSqlPlanningError
  ): RewriteDiagnostic = {
    val code =
      if (problem.stage.invalidatesSql) RewriteReasonCode.InvalidSql
      else RewriteReasonCode.PlanningFailed
    val message = problem.stage match {
      case CalciteSqlPlanningStage.Parsing =>
        "Calcite could not parse the SQL statement"
      case CalciteSqlPlanningStage.Validation =>
        "Calcite could not validate the SQL statement against the catalog"
      case _ => "Calcite SQL planning failed"
    }
    RewriteDiagnostic(
      code,
      message,
      context = Map("stage" -> problem.stage.id) ++
        safeExceptionClass(problem.exceptionClass).map("exceptionClass" -> _)
    )
  }

  private def safeExceptionClass(value: Option[String]): Option[String] =
    value.filter(
      _.matches(
        "[A-Za-z_$][A-Za-z0-9_$]*(\\.[A-Za-z_$][A-Za-z0-9_$]*)*"
      )
    )

  private def candidateInputDiagnostics(
      problems: Vector[CandidateInputError]
  ): Vector[RewriteDiagnostic] =
    problems.map(problem =>
      algorithmDiagnostic(
        RewriteReasonCode.PlanningFailed,
        "CandidateTD input validation failed",
        "candidate-input",
        problem.code,
        problem.message
      )
    )

  private def bagFactorDiagnostics(
      problems: Vector[BagFactorPlanningError]
  ): Vector[RewriteDiagnostic] =
    problems.map(problem =>
      algorithmDiagnostic(
        RewriteReasonCode.PlanningFailed,
        "Bag-factor planning failed",
        "bag-factor-planning",
        problem.code,
        problem.message
      )
    )

  private def renderingDiagnostics(
      problems: Vector[PostgreSqlRenderingError]
  ): Vector[RewriteDiagnostic] =
    problems.map { problem =>
      renderingLimitationReason(problem) match {
        case Some(reason) =>
          algorithmDiagnostic(
            reason,
            "Query is outside the supported PostgreSQL renderer fragment",
            "postgresql-rendering",
            problem.code,
            problem.message,
            severity = DiagnosticSeverity.Warning
          )
        case None =>
          algorithmDiagnostic(
            RewriteReasonCode.RenderingFailed,
            "PostgreSQL rendering failed",
            "postgresql-rendering",
            problem.code,
            problem.message
          )
      }
    }

  private def renderingOutcome(
      originalSql: String,
      problems: Vector[PostgreSqlRenderingError],
      additionalContext: Map[String, String] = Map.empty
  ): RewriteOutcome = {
    val diagnostics = renderingDiagnostics(problems).map(diagnostic =>
      diagnostic.copy(context = diagnostic.context ++ additionalContext)
    )
    if (diagnostics.isEmpty) {
      internalFailure(
        originalSql,
        "PostgreSQL renderer returned no query and no diagnostic",
        "postgresql-rendering"
      )
    } else if (diagnostics.exists(_.severity == DiagnosticSeverity.Error)) {
      RewriteOutcome.Failed(originalSql, diagnostics)
    } else {
      RewriteOutcome.NotApplicable(originalSql, diagnostics)
    }
  }

  private def selectionUnavailableOutcome(
      originalSql: String,
      decision: PostgreSqlCandidateSelectionDecision.Unavailable
  ): RewriteOutcome.Failed = {
    val requested = decision.policy match {
      case at.ac.tuwien.dbai.yannakakis.api.CandidatePlanSelection.StructuralRank(rank) =>
        Map("requestedRank" -> rank.toString)
      case at.ac.tuwien.dbai.yannakakis.api.CandidatePlanSelection.ExactCandidateId(id) =>
        Map("requestedCandidateId" -> id.value)
      case _ => Map.empty[String, String]
    }
    RewriteOutcome.Failed(
      originalSql,
      Vector(
        RewriteDiagnostic(
          RewriteReasonCode.CandidateSelectionFailed,
          "The requested candidate is not present in the retained structural top-k set",
          context = Map(
            "stage" -> "candidate-selection",
            "algorithmCode" -> "selection.candidate-not-retained",
            "selectionPolicy" -> decision.policy.id,
            "retainedCandidateCount" -> decision.retainedCandidateCount.toString
          ) ++ requested
        )
      )
    )
  }

  private def selectionResourceOutcome(
      originalSql: String,
      failure: PostgreSqlCandidateSelectionFailure.ResourceLimitExceeded
  ): RewriteOutcome.Failed =
    RewriteOutcome.Failed(
      originalSql,
      Vector(
        RewriteDiagnostic(
          RewriteReasonCode.CandidateSelectionFailed,
          "Candidate evaluation exceeded its cumulative resource envelope",
          context = candidateContext(failure.plan, failure.report.policy.id) ++
            Map(
              "stage" -> "candidate-selection",
              "algorithmCode" -> "selection.resource-limit.exceeded",
              "resource" -> failure.resource.code,
              "required" -> failure.required.toString,
              "limit" -> failure.limit.toString
            )
        )
      )
    )

  private def explainCostFailureOutcome(
      originalSql: String,
      failure: PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed
  ): RewriteOutcome.Failed =
    RewriteOutcome.Failed(
      originalSql,
      Vector(
        RewriteDiagnostic(
          RewriteReasonCode.CandidateSelectionFailed,
          "PostgreSQL optimizer cost evaluation failed",
          context = failure.plan
            .map(candidateContext(_, failure.report.policy.id))
            .getOrElse(
              Map("selectionPolicy" -> failure.report.policy.id)
            ) ++
            Map(
              "stage" -> "candidate-selection",
              "algorithmCode" -> failure.code.id,
              "explainStage" -> failure.report.stage.id
            )
        )
      )
    )

  private def noAdmissibleEstimateOutcome(
      originalSql: String,
      failure: PostgreSqlCandidateSelectionFailure.NoAdmissibleEstimate
  ): RewriteOutcome.Failed =
    RewriteOutcome.Failed(
      originalSql,
      Vector(
        RewriteDiagnostic(
          RewriteReasonCode.CandidateSelectionFailed,
          "No retained candidate passed the PostgreSQL row-estimate guard",
          context = Map(
            "stage" -> "candidate-selection",
            "algorithmCode" -> "selection.explain.no-admissible-estimate",
            "selectionPolicy" -> failure.report.policy.id,
            "evaluatedCandidateCount" ->
              failure.report.evaluations.size.toString,
            "rowGuardFactor" ->
              PostgreSqlExplainRowGuardEvidence.Factor.toString
          )
        )
      )
    )

  private def factorAwareFailureOutcome(
      originalSql: String,
      failure: PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed
  ): RewriteOutcome.Failed =
    RewriteOutcome.Failed(
      originalSql,
      Vector(
        RewriteDiagnostic(
          RewriteReasonCode.CandidateSelectionFailed,
          "PostgreSQL factor-aware optimizer evaluation failed",
          context = failure.plan
            .map(candidateContext(_, failure.report.policy.id))
            .getOrElse(Map("selectionPolicy" -> failure.report.policy.id)) ++
            Map(
              "stage" -> "candidate-selection",
              "algorithmCode" -> failure.code.id,
              "explainStage" -> failure.report.stage.id
            ) ++ failure.report.failedStageId.map(stage =>
              "factorStageId" -> stage.code
            )
        )
      )
    )

  private def factorAwareResourceOutcome(
      originalSql: String,
      failure: PostgreSqlCandidateSelectionFailure.FactorAwareResourceLimitExceeded
  ): RewriteOutcome.Failed =
    RewriteOutcome.Failed(
      originalSql,
      Vector(
        RewriteDiagnostic(
          RewriteReasonCode.CandidateSelectionFailed,
          "Factor-aware candidate evaluation exceeded its resource envelope",
          context = candidateContext(failure.plan, failure.report.policy.id) ++
            Map(
              "stage" -> "candidate-selection",
              "algorithmCode" -> failure.report.stage.id,
              "resource" -> failure.resource.code,
              "required" -> failure.required.toString,
              "limit" -> failure.limit.toString
            )
        )
      )
    )

  private def candidateContext(
      plan: at.ac.tuwien.dbai.yannakakis.algorithm.candidate.RankedCandidatePlan,
      policyId: String
  ): Map[String, String] =
    Map(
      "selectionPolicy" -> policyId,
      "candidateRank" -> plan.rank.toString,
      "candidateId" -> plan.candidateId.value
    )

  private def withCandidateContext(
      diagnostics: Vector[RewriteDiagnostic],
      plan: at.ac.tuwien.dbai.yannakakis.algorithm.candidate.RankedCandidatePlan,
      policyId: String
  ): Vector[RewriteDiagnostic] = {
    val additional = candidateContext(plan, policyId)
    diagnostics.map(diagnostic =>
      diagnostic.copy(context = diagnostic.context ++ additional)
    )
  }

  private def renderingLimitationReason(
      problem: PostgreSqlRenderingError
  ): Option[RewriteReasonCode] = problem match {
    case _: PostgreSqlRenderingError.NonUniformVariableTypes =>
      Some(RewriteReasonCode.UnsupportedPredicate)
    case _: PostgreSqlRenderingError.UnsupportedStructuralType |
        _: PostgreSqlRenderingError.UnsupportedScalar |
        _: PostgreSqlRenderingError.UnsupportedGroupedCountKey |
        _: PostgreSqlRenderingError.UnsupportedDistributedGroupedCountKey |
        _: PostgreSqlRenderingError.InvalidQualifiedName |
        _: PostgreSqlRenderingError.GeneratedNameCollision =>
      Some(RewriteReasonCode.UnsupportedQueryShape)
    case _ => None
  }

  private def failed(
      originalSql: String,
      reason: RewriteReasonCode,
      message: String,
      stage: String,
      algorithmCode: String,
      detail: String
  ): RewriteOutcome.Failed =
    RewriteOutcome.Failed(
      originalSql,
      Vector(
        algorithmDiagnostic(reason, message, stage, algorithmCode, detail)
      )
    )

  private def algorithmDiagnostic(
      reason: RewriteReasonCode,
      message: String,
      stage: String,
      algorithmCode: String,
      detail: String,
      severity: DiagnosticSeverity = DiagnosticSeverity.Error
  ): RewriteDiagnostic =
    RewriteDiagnostic(
      reason,
      message,
      severity = severity,
      context = Map(
        "stage" -> stage,
        "algorithmCode" -> algorithmCode,
        "detail" -> detail
      )
    )

  private def internalFailure(
      originalSql: String,
      message: String,
      stage: String
  ): RewriteOutcome.Failed =
    RewriteOutcome.Failed(
      originalSql,
      Vector(
        RewriteDiagnostic(
          RewriteReasonCode.InternalError,
          message,
          context = Map("stage" -> stage)
        )
      )
    )
}

object PostgreSqlQueryRewriter {
  /** Non-throwing construction around an injected catalog/planning boundary. */
  def create(
      sqlPlanner: CalciteSqlPlanner,
      softLimits: SoftResourceLimits = SoftResourceLimits.default,
      candidateLimits: CandidateResourceLimits = CandidateResourceLimits.default,
      bagFactorLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default,
      renderingLimits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default,
      selectionLimits: PostgreSqlCandidateSelectionLimits =
        PostgreSqlCandidateSelectionLimits.default,
      explainCostEvaluator: Option[PostgreSqlExplainCostEvaluator] = None,
      explainLimits: PostgreSqlExplainSelectionLimits =
        PostgreSqlExplainSelectionLimits.default,
      factorAwareLimits: PostgreSqlFactorAwareSelectionLimits =
        PostgreSqlFactorAwareSelectionLimits.default,
      physicalPortfolioLimits: PostgreSqlPhysicalPortfolioLimits =
        PostgreSqlPhysicalPortfolioLimits.default,
      physicalPortfolioEvaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits =
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
  ): Either[PostgreSqlRewriterConfigurationError, PostgreSqlQueryRewriter] = {
    val missing = Vector.newBuilder[String]
    if (sqlPlanner eq null) missing += "Calcite SQL planner"
    if (softLimits eq null) missing += "soft-bag resource limits"
    if (candidateLimits eq null) missing += "candidate resource limits"
    if (bagFactorLimits eq null) missing += "bag-factor resource limits"
    if (renderingLimits eq null) missing += "PostgreSQL rendering limits"
    if (selectionLimits eq null) missing += "candidate selection resource limits"
    if (
      explainCostEvaluator == null || explainCostEvaluator.exists(_ == null)
    ) missing += "PostgreSQL EXPLAIN cost evaluator option"
    if (explainLimits eq null) missing += "PostgreSQL EXPLAIN resource limits"
    if (factorAwareLimits eq null)
      missing += "PostgreSQL factor-aware resource limits"
    if (
      physicalPortfolioLimits == null ||
      PostgreSqlPhysicalPortfolioLimits.validate(physicalPortfolioLimits).nonEmpty
    ) missing += "PostgreSQL physical portfolio limits"
    if (
      physicalPortfolioEvaluationLimits == null ||
      PostgreSqlPhysicalPortfolioEvaluationLimits
        .validate(physicalPortfolioEvaluationLimits)
        .nonEmpty
    ) missing += "PostgreSQL physical portfolio evaluation limits"

    val fields = missing.result()
    if (fields.nonEmpty) {
      Left(PostgreSqlRewriterConfigurationError.MissingValues(fields))
    } else {
      Right(
        new PostgreSqlQueryRewriter(
          sqlPlanner,
          softLimits,
          candidateLimits,
          bagFactorLimits,
          renderingLimits,
          selectionLimits,
          explainCostEvaluator,
          explainLimits,
          factorAwareLimits,
          physicalPortfolioLimits,
          physicalPortfolioEvaluationLimits
        )
      )
    }
  }

  def fromFrameworkConfig(
      config: FrameworkConfig
  ): Either[PostgreSqlRewriterConfigurationError, PostgreSqlQueryRewriter] =
    fromPlanner(CalciteSqlPlanner.fromFrameworkConfig(config))

  def forSchema(
      defaultSchema: SchemaPlus,
      parserConfig: SqlParser.Config = CalciteSqlPlanner.DefaultParserConfig
  ): Either[PostgreSqlRewriterConfigurationError, PostgreSqlQueryRewriter] =
    fromPlanner(CalciteSqlPlanner.forSchema(defaultSchema, parserConfig))

  /**
    * Uses but never closes the caller-owned data source. An absent
    * `jdbcSchema` binds the exact physical schema named by `schemaName`;
    * `Some(value)` explicitly overrides that physical name.
    */
  def forJdbcDataSource(
      dataSource: DataSource,
      schemaName: String,
      jdbcCatalog: Option[String] = None,
      jdbcSchema: Option[String] = None,
      parserConfig: SqlParser.Config = CalciteSqlPlanner.PostgreSqlParserConfig,
      explainLimits: PostgreSqlExplainSelectionLimits =
        PostgreSqlExplainSelectionLimits.default,
      factorAwareLimits: PostgreSqlFactorAwareSelectionLimits =
        PostgreSqlFactorAwareSelectionLimits.default,
      physicalPortfolioLimits: PostgreSqlPhysicalPortfolioLimits =
        PostgreSqlPhysicalPortfolioLimits.default,
      physicalPortfolioEvaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits =
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
  ): Either[PostgreSqlRewriterConfigurationError, PostgreSqlQueryRewriter] = {
    val planner =
      CalciteSqlPlanner.forJdbcDataSource(
        dataSource,
        schemaName,
        jdbcCatalog,
        jdbcSchema,
        parserConfig
      )
    finishJdbcConstruction(
      planner,
      dataSource,
      schemaName,
      jdbcSchema,
      explainLimits,
      factorAwareLimits,
      physicalPortfolioLimits,
      physicalPortfolioEvaluationLimits
    )
  }

  /**
    * Explicit C03-G0 v2 capability. Construction first verifies the exact
    * PostgreSQL-18 `synthetic_triangle_e0(payload TEXT, row_id BIGINT)` catalog
    * binding and deterministic payload collation in a read-only
    * repeatable-read transaction. All ordinary factories retain
    * `CalciteAggregateLanguageProfile.LegacyV1` and continue to reject C03;
    * this is independent of the enumeration SQL rendering profile.
    */
  def forJdbcDataSourceWithC03G0V2(
      dataSource: DataSource,
      schemaName: String,
      jdbcCatalog: Option[String] = None,
      jdbcSchema: Option[String] = None,
      parserConfig: SqlParser.Config = CalciteSqlPlanner.PostgreSqlParserConfig,
      explainLimits: PostgreSqlExplainSelectionLimits =
        PostgreSqlExplainSelectionLimits.default,
      factorAwareLimits: PostgreSqlFactorAwareSelectionLimits =
        PostgreSqlFactorAwareSelectionLimits.default,
      physicalPortfolioLimits: PostgreSqlPhysicalPortfolioLimits =
        PostgreSqlPhysicalPortfolioLimits.default,
      physicalPortfolioEvaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits =
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
  ): Either[PostgreSqlRewriterConfigurationError, PostgreSqlQueryRewriter] = {
    // Preserve the established validation order without constructing and
    // discarding a legacy JdbcSchema before the catalog receipt transaction.
    CalciteSqlPlanner.validateJdbcConfiguration(
      dataSource,
      schemaName,
      jdbcCatalog,
      jdbcSchema,
      parserConfig
    ) match {
      case Left(problem) => configurationFailure(problem)
      case Right(_) =>
        val physicalSchemaName = jdbcSchema.getOrElse(schemaName)
        val planner = PostgreSqlC03G0CatalogCapability
          .verify(dataSource, physicalSchemaName)
          .flatMap(binding =>
            CalciteSqlPlanner.forJdbcDataSourceWithC03G0V2(
              dataSource,
              schemaName,
              jdbcCatalog,
              jdbcSchema,
              parserConfig,
              binding
            )
          )
        finishJdbcConstruction(
          planner,
          dataSource,
          schemaName,
          jdbcSchema,
          explainLimits,
          factorAwareLimits,
          physicalPortfolioLimits,
          physicalPortfolioEvaluationLimits
        )
    }
  }

  /** Direct PostgreSQL URL convenience; credentials never enter diagnostics. */
  def forPostgreSqlJdbc(
      jdbcUrl: String,
      user: String,
      password: String,
      schemaName: String,
      parserConfig: SqlParser.Config = CalciteSqlPlanner.PostgreSqlParserConfig,
      explainLimits: PostgreSqlExplainSelectionLimits =
        PostgreSqlExplainSelectionLimits.default,
      factorAwareLimits: PostgreSqlFactorAwareSelectionLimits =
        PostgreSqlFactorAwareSelectionLimits.default,
      physicalPortfolioLimits: PostgreSqlPhysicalPortfolioLimits =
        PostgreSqlPhysicalPortfolioLimits.default,
      physicalPortfolioEvaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits =
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
  ): Either[PostgreSqlRewriterConfigurationError, PostgreSqlQueryRewriter] = {
    val planner =
      CalciteSqlPlanner.forPostgreSqlJdbc(
        jdbcUrl,
        user,
        password,
        schemaName,
        parserConfig
      )
    planner match {
      case Left(problem) => configurationFailure(problem)
      case Right(value) =>
        create(
          value,
          explainCostEvaluator = Some(
            PostgreSqlExplainCostEvaluator.forPostgreSqlJdbc(
              jdbcUrl,
              user,
              password,
              schemaName
            )
          ),
          explainLimits = explainLimits,
          factorAwareLimits = factorAwareLimits,
          physicalPortfolioLimits = physicalPortfolioLimits,
          physicalPortfolioEvaluationLimits = physicalPortfolioEvaluationLimits
        )
    }
  }

  private def fromPlanner(
      planner: Either[CalciteSqlPlanningError, CalciteSqlPlanner]
  ): Either[PostgreSqlRewriterConfigurationError, PostgreSqlQueryRewriter] =
    planner match {
      case Left(problem) => configurationFailure(problem)
      case Right(value)  => create(value)
    }

  private def finishJdbcConstruction(
      planner: Either[CalciteSqlPlanningError, CalciteSqlPlanner],
      dataSource: DataSource,
      schemaName: String,
      jdbcSchema: Option[String],
      explainLimits: PostgreSqlExplainSelectionLimits,
      factorAwareLimits: PostgreSqlFactorAwareSelectionLimits,
      physicalPortfolioLimits: PostgreSqlPhysicalPortfolioLimits,
      physicalPortfolioEvaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits
  ): Either[PostgreSqlRewriterConfigurationError, PostgreSqlQueryRewriter] =
    planner match {
      case Left(problem) => configurationFailure(problem)
      case Right(value) =>
        PostgreSqlExplainCostEvaluator.forDataSource(
          dataSource,
          jdbcSchema.getOrElse(schemaName)
        ) match {
          case Left(_) =>
            Left(
              PostgreSqlRewriterConfigurationError.MissingValues(
                Vector("PostgreSQL EXPLAIN data source")
              )
            )
          case Right(evaluator) =>
            create(
              value,
              explainCostEvaluator = Some(evaluator),
              explainLimits = explainLimits,
              factorAwareLimits = factorAwareLimits,
              physicalPortfolioLimits = physicalPortfolioLimits,
              physicalPortfolioEvaluationLimits =
                physicalPortfolioEvaluationLimits
            )
        }
    }

  private def configurationFailure(
      problem: CalciteSqlPlanningError
  ): Left[PostgreSqlRewriterConfigurationError, Nothing] =
    Left(
      PostgreSqlRewriterConfigurationError.CalciteConfigurationFailure(
        problem.stage,
        problem.exceptionClass
      )
    )
}
