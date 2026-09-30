package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanId,
  CandidatePlanDecision,
  CandidateSearchInput,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlan
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlRenderedQuery
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{
  AdmissibleSoftBagGeneration,
  SoftBagGeneration
}
import at.ac.tuwien.dbai.yannakakis.api.{CandidatePlanSelection, RewriteOutcome}
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  QueryIR,
  QueryResultShape
}

/** Last successfully completed phase of one detailed rewrite attempt. */
sealed trait PostgreSqlRewritePhase extends Product with Serializable {
  def id: String
}

object PostgreSqlRewritePhase {
  case object RequestAccepted extends PostgreSqlRewritePhase {
    override val id: String = "request-accepted"
  }
  case object SqlPlanned extends PostgreSqlRewritePhase {
    override val id: String = "sql-planned"
  }
  case object QueryAnalyzed extends PostgreSqlRewritePhase {
    override val id: String = "query-analyzed"
  }
  case object HypergraphBuilt extends PostgreSqlRewritePhase {
    override val id: String = "hypergraph-built"
  }
  case object SoftBagsGenerated extends PostgreSqlRewritePhase {
    override val id: String = "soft-bags-generated"
  }
  case object CandidateInputBuilt extends PostgreSqlRewritePhase {
    override val id: String = "candidate-input-built"
  }
  case object CandidateSearchCompleted extends PostgreSqlRewritePhase {
    override val id: String = "candidate-search-completed"
  }
  case object CandidateSelectionCompleted extends PostgreSqlRewritePhase {
    override val id: String = "candidate-selection-completed"
  }
  case object BagFactorPlanned extends PostgreSqlRewritePhase {
    override val id: String = "bag-factor-planned"
  }
  case object Rendered extends PostgreSqlRewritePhase {
    override val id: String = "rendered"
  }

  val values: Vector[PostgreSqlRewritePhase] = Vector(
    RequestAccepted,
    SqlPlanned,
    QueryAnalyzed,
    HypergraphBuilt,
    SoftBagsGenerated,
    CandidateInputBuilt,
    CandidateSearchCompleted,
    CandidateSelectionCompleted,
    BagFactorPlanned,
    Rendered
  )
}

/**
  * Inspectable success certificate for the deterministic selected rewrite.
  *
  * `candidateSearch` retains the complete exact top-k decision and statistics;
  * `candidateSelection` proves which retained plan was selected and why.
  */
final case class PostgreSqlRewriteArtifact private[postgresql] (
    query: QueryIR,
    hypergraph: QueryHypergraph,
    softGeneration: SoftBagGeneration,
    admissibleGeneration: AdmissibleSoftBagGeneration,
    candidateInput: CandidateSearchInput,
    candidateSearch: CandidatePlanDecision.Found,
    candidateSelection: PostgreSqlCandidateSelectionDecision.Selected,
    selectedPlan: RankedCandidatePlan,
    bagFactorPlan: BagFactorPlan,
    renderedQuery: PostgreSqlRenderedQuery
) extends Serializable {
  require(query != null, "artifact query must not be null")
  require(hypergraph != null, "artifact hypergraph must not be null")
  require(softGeneration != null, "artifact soft generation must not be null")
  require(
    admissibleGeneration != null,
    "artifact admissible generation must not be null"
  )
  require(candidateInput != null, "artifact candidate input must not be null")
  require(candidateSearch != null, "artifact candidate search must not be null")
  require(
    candidateSelection != null,
    "artifact candidate selection must not be null"
  )
  require(selectedPlan != null, "artifact selected plan must not be null")
  require(bagFactorPlan != null, "artifact bag-factor plan must not be null")
  require(renderedQuery != null, "artifact rendered query must not be null")
  require(
    candidateSearch.plans != null && candidateSearch.plans.nonEmpty &&
      candidateSearch.plans.forall(plan =>
        plan != null && plan.candidateId != null &&
          plan.decomposition != null && plan.certificate != null
      ),
    "artifact candidate search must contain complete retained plans"
  )
  require(
    candidateSearch.plans.map(_.rank) ==
      (1 to candidateSearch.plans.size).toVector,
    "artifact retained candidate ranks must be contiguous and increasing"
  )
  require(
    candidateSearch.plans.map(_.candidateId).distinct.size ==
      candidateSearch.plans.size,
    "artifact retained candidate ids must be unique"
  )
  require(
    candidateSearch.plans.forall(plan =>
      plan.candidateId == CandidatePlanId.from(plan.decomposition)
    ),
    "artifact retained candidate ids must match their decompositions"
  )
  require(
    candidateInput.hypergraph == hypergraph,
    "artifact candidate input must be bound to the artifact hypergraph"
  )
  require(
    softGeneration.sourceHypergraph == hypergraph &&
      admissibleGeneration.sourceHypergraph == hypergraph,
    "artifact soft-bag evidence must be bound to the artifact hypergraph"
  )
  require(
    candidateSearch.plans.count(_ == selectedPlan) == 1,
    "artifact selected plan must occur exactly once in candidate search"
  )
  private val retainedIdentities = candidateSearch.plans.map(plan =>
    plan.rank -> plan.candidateId
  )
  private val evaluatedIdentities = candidateSelection.evaluations.map(
    evaluation => evaluation.structuralRank -> evaluation.candidateId
  )
  private val expectedEvaluatedIdentities = candidateSelection.policy match {
    case CandidatePlanSelection.StructuralHead =>
      Vector(retainedIdentities.head)
    case CandidatePlanSelection.StructuralRank(rank) =>
      retainedIdentities.filter(_._1 == rank)
    case CandidatePlanSelection.ExactCandidateId(candidateId) =>
      retainedIdentities.filter(_._2 == candidateId)
    case CandidatePlanSelection.PostgreSqlStaticShapeV1 =>
      retainedIdentities
    case CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 =>
      retainedIdentities
    case CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 =>
      retainedIdentities
    case CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 =>
      retainedIdentities
    case CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 =>
      throw new IllegalArgumentException(
        "physical portfolio policy cannot inhabit the legacy artifact"
      )
  }
  require(
    evaluatedIdentities == expectedEvaluatedIdentities,
    "artifact evaluations must match the candidates required by the policy"
  )
  require(
    selectedPlan.candidateId == candidateSelection.selectedCandidateId &&
      selectedPlan.rank == candidateSelection.selectedStructuralRank &&
      candidateSelection.selectedPhysicalVariantId ==
        PostgreSqlPhysicalVariantId.from(
          selectedPlan.candidateId,
          candidateSelection.reductionMode
        ),
    "artifact selected plan must match candidate selection evidence"
  )
  private val selectedEvaluation = candidateSelection.evaluations
    .find(evaluation =>
      evaluation.candidateId == selectedPlan.candidateId &&
        evaluation.structuralRank == selectedPlan.rank &&
        evaluation.physicalVariantId ==
          candidateSelection.selectedPhysicalVariantId
    )
    .getOrElse(
      throw new IllegalArgumentException(
        "artifact selected plan must have a completed physical evaluation"
      )
    )
  candidateSelection.policy match {
    case CandidatePlanSelection.PostgreSqlStaticShapeV1 =>
      require(
        selectedEvaluation == candidateSelection.evaluations.minBy(_.score)(
          PostgreSqlStaticShapeScore.ordering
        ),
        "artifact static-shape selection must choose the minimum exact score"
      )
    case CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 =>
      require(
        selectedEvaluation == candidateSelection.evaluations
          .filter(_.rowGuardEvidence.exists(_.withinGuard))
          .min(PostgreSqlCandidateSelectionDecision.explainOrdering),
        "artifact guarded EXPLAIN selection must choose minimum admissible cost and stable ties"
      )
    case CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 =>
      require(
        PostgreSqlFactorAwareSelectionReplay.verify(
          candidateSearch.plans,
          candidateSelection.evaluations,
          candidateSelection.sourceEvaluation.getOrElse(
            throw new IllegalArgumentException(
              "artifact factor-aware selection requires a source estimate"
            )
          ),
          candidateSelection.factorAwareCheckpoints,
          candidateSelection.factorAwareSelectionLimits.getOrElse(
            throw new IllegalArgumentException(
              "artifact factor-aware selection requires bounded limits"
            )
          ),
          None
        ),
        "artifact factor-aware checkpoints must replay against retained plans and estimates"
      )
      require(
        selectedEvaluation == candidateSelection.evaluations
          .filter(_.rowGuardEvidence.exists(_.withinGuard))
          .min(PostgreSqlCandidateSelectionDecision.factorAwareOrdering),
        "artifact factor-aware selection must choose the exact admissible lexicographic minimum"
      )
      require(
        candidateSelection.factorAwareCheckpoints.lastOption.exists(
          _.winnerCandidateId.contains(selectedPlan.candidateId)
        ),
        "artifact factor-aware selection must match the verified K=100 checkpoint"
      )
      val factorEvidence = selectedEvaluation.factorAwareEvidence
        .getOrElse(
          throw new IllegalArgumentException(
            "artifact factor-aware selection requires stage evidence"
          )
        )
      require(
        factorEvidence.physicalVariantId ==
          candidateSelection.selectedPhysicalVariantId &&
          selectedEvaluation.reductionMode == candidateSelection.reductionMode,
        "artifact factor evidence must match the selected physical mode"
      )
      require(
        factorEvidence.stages.map(_.trace) == renderedQuery.trace.stages,
        "artifact factor evidence must bind the exact renderer stage trace"
      )
      val terminalFactorStage = factorEvidence.stages.last
      require(
        terminalFactorStage.diagnosticStatementSha256.contains(
          PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
            renderedQuery.sql
          )
        ) &&
          terminalFactorStage.diagnosticCharacterCount.contains(
            renderedQuery.sql.length.toLong
          ) &&
          selectedEvaluation.renderedStatementSha256 ==
            terminalFactorStage.diagnosticStatementSha256.get,
        "artifact terminal factor evidence must bind the emitted SQL bytes"
      )
    case CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 =>
      require(
        query.resultShape.isInstanceOf[QueryResultShape.GlobalCountStar] &&
          candidateSearch.requestedLimit ==
            PostgreSqlCountStageCostProgressiveV1.RequiredPlanLimit &&
          candidateSelection.reductionMode ==
            at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode.FullTwoPass,
        "v7 selection requires certified global COUNT(*), exact K=100, and FullTwoPass"
      )
      require(
        PostgreSqlCountStageCostSelectionReplay.verify(
          candidateSearch.plans,
          candidateSelection.evaluations,
          candidateSelection.sourceEvaluation.getOrElse(
            throw new IllegalArgumentException(
              "artifact COUNT stage-cost selection requires a source estimate"
            )
          ),
          candidateSelection.countStageCostCheckpoints,
          candidateSelection.countStageCostSelectionLimits.getOrElse(
            throw new IllegalArgumentException(
              "artifact COUNT stage-cost selection requires bounded limits"
            )
          ),
          None
        ),
        "artifact COUNT stage-cost checkpoints must replay against retained plans and estimates"
      )
      require(
        selectedEvaluation == candidateSelection.evaluations
          .filter(_.rowGuardEvidence.exists(_.withinGuard))
          .min(PostgreSqlCandidateSelectionDecision.countStageCostOrdering),
        "artifact COUNT stage-cost selection must choose the exact admissible lexicographic minimum"
      )
      require(
        candidateSelection.countStageCostCheckpoints.lastOption.exists(
          _.winnerCandidateId.contains(selectedPlan.candidateId)
        ),
        "artifact COUNT stage-cost selection must match the verified K=100 checkpoint"
      )
      val countEvidence = selectedEvaluation.countStageCostEvidence.getOrElse(
        throw new IllegalArgumentException(
          "artifact COUNT stage-cost selection requires stage evidence"
        )
      )
      require(
        countEvidence.physicalVariantId ==
          candidateSelection.selectedPhysicalVariantId &&
          selectedEvaluation.reductionMode == candidateSelection.reductionMode,
        "artifact COUNT stage-cost evidence must match the selected physical mode"
      )
      require(
        countEvidence.stages.map(_.trace) == renderedQuery.trace.stages,
        "artifact COUNT stage-cost evidence must bind the exact renderer stage trace"
      )
      val terminalCountStage = countEvidence.stages.last
      require(
        terminalCountStage.diagnosticStatementSha256.contains(
          PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
            renderedQuery.sql
          )
        ) &&
          terminalCountStage.diagnosticCharacterCount.contains(
            renderedQuery.sql.length.toLong
          ) &&
          selectedEvaluation.renderedStatementSha256 ==
            terminalCountStage.diagnosticStatementSha256.get,
        "artifact terminal COUNT stage-cost evidence must bind the emitted SQL bytes"
      )
    case _ => ()
  }
  require(
    bagFactorPlan.decomposition == selectedPlan.decomposition,
    "artifact bag-factor plan must use the selected decomposition"
  )
  require(
    bagFactorPlan.certificate == selectedPlan.certificate,
    "artifact bag-factor plan must use the selected certificate"
  )
  require(
    bagFactorPlan.sourceHypergraph == hypergraph,
    "artifact bag-factor plan must be bound to the artifact hypergraph"
  )
  require(
    bagFactorPlan.sourceWidthBound == candidateInput.widthBound &&
      bagFactorPlan.requireConnectedCover ==
        candidateInput.requireConnectedCover,
    "artifact bag-factor plan must use the candidate-search options"
  )
  require(
    renderedQuery.sourceQuery == query,
    "artifact rendered query must use the analyzed QueryIR"
  )
  require(
    renderedQuery.bagFactorPlan == bagFactorPlan,
    "artifact rendered query must use the selected bag-factor plan"
  )
  require(
    renderedQuery.trace.reduction.mode == candidateSelection.reductionMode &&
      renderedQuery.trace.reduction.reductionOrder ==
        candidateSelection.reductionOrder,
    "artifact rendered query must use the selected reduction schedule"
  )
  require(
    selectedEvaluation.score ==
      PostgreSqlStaticShapeScore.from(renderedQuery, selectedPlan.rank),
    "artifact selected score must describe the rendered query"
  )
  require(
    selectedEvaluation.renderedStatementSha256 ==
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
        renderedQuery.sql
      ),
    "artifact selected digest must describe the rendered SQL"
  )
}

/**
  * Partial, data-only evidence retained even when rewriting is not applicable
  * or a bounded stage fails. In particular, rejected CandidateTD searches keep
  * their exact search statistics in `candidateDecision`.
  */
final case class PostgreSqlRewriteReport private[postgresql] (
    completedPhase: PostgreSqlRewritePhase,
    analysis: Option[AnalysisReport],
    hypergraph: Option[QueryHypergraph],
    softGeneration: Option[SoftBagGeneration],
    admissibleGeneration: Option[AdmissibleSoftBagGeneration],
    candidateInput: Option[CandidateSearchInput],
    candidateDecision: Option[CandidatePlanDecision],
    candidateSelectionDecision: Option[PostgreSqlCandidateSelectionDecision],
    bagFactorPlan: Option[BagFactorPlan],
    artifact: Option[PostgreSqlRewriteArtifact],
    physicalPortfolioAttempt: Option[
      PostgreSqlPhysicalPortfolioAttempt
    ] = None,
    physicalPortfolioArtifact: Option[
      PostgreSqlPhysicalPortfolioArtifact
    ] = None
) extends Serializable {
  require(completedPhase != null, "completed phase must not be null")
  require(analysis != null, "analysis option must not be null")
  require(hypergraph != null, "hypergraph option must not be null")
  require(softGeneration != null, "soft-generation option must not be null")
  require(
    admissibleGeneration != null,
    "admissible-generation option must not be null"
  )
  require(candidateInput != null, "candidate-input option must not be null")
  require(candidateDecision != null, "candidate-decision option must not be null")
  require(
    candidateSelectionDecision != null,
    "candidate-selection-decision option must not be null"
  )
  require(bagFactorPlan != null, "bag-factor-plan option must not be null")
  require(artifact != null, "artifact option must not be null")
  require(
    physicalPortfolioAttempt != null && physicalPortfolioAttempt.forall(_ != null),
    "physical-portfolio attempt option must not be null or contain null"
  )
  require(
    physicalPortfolioArtifact != null && physicalPortfolioArtifact.forall(_ != null),
    "physical-portfolio artifact option must not be null or contain null"
  )
  require(
    physicalPortfolioAttempt.isEmpty ||
      (candidateSelectionDecision.isEmpty && bagFactorPlan.isEmpty &&
        artifact.isEmpty),
    "physical portfolio and legacy selection evidence are mutually exclusive"
  )
  physicalPortfolioAttempt.foreach { attempt =>
    require(
      analysis.exists(_.query == attempt.query) &&
        hypergraph.contains(attempt.hypergraph) &&
        softGeneration.contains(attempt.softGeneration) &&
        admissibleGeneration.contains(attempt.admissibleGeneration) &&
        candidateInput.contains(attempt.candidateInput) &&
        candidateDecision.contains(attempt.candidateSearch),
      "physical portfolio report must bind its exact completed pipeline"
    )
    attempt.decision match {
      case _: PostgreSqlPhysicalPortfolioDecision.Selected =>
        require(
          completedPhase == PostgreSqlRewritePhase.Rendered &&
            physicalPortfolioArtifact.exists(_.attempt == attempt),
          "selected physical portfolio report requires its exact artifact"
        )
      case _: PostgreSqlPhysicalPortfolioDecision.NoAdmissible |
          _: PostgreSqlPhysicalPortfolioDecision.Failed =>
        require(
          completedPhase == PostgreSqlRewritePhase.CandidateSelectionCompleted &&
            physicalPortfolioArtifact.isEmpty,
          "non-selected physical portfolio report ends at candidate selection"
        )
    }
  }
  require(
    physicalPortfolioArtifact.isEmpty || physicalPortfolioAttempt.nonEmpty,
    "physical portfolio artifact requires its replay attempt"
  )
}

/** Public detailed result; `outcome` remains the stable execution decision. */
final case class PostgreSqlRewriteResult private[postgresql] (
    outcome: RewriteOutcome,
    report: PostgreSqlRewriteReport
) extends Serializable {
  require(outcome != null, "rewrite outcome must not be null")
  require(report != null, "rewrite report must not be null")
}
