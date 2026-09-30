package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanId,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlStageDiagnosticStatement,
  PostgreSqlStageId,
  PostgreSqlStageKind,
  PostgreSqlStagePhysicalForm
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  YannakakisReductionMode
}

/** Frozen boundary for the opt-in global COUNT(*) stage-cost policy. */
object PostgreSqlCountStageCostProgressiveV1 {
  val RequiredPlanLimit: Int = RewriteOptions.CountStageCostProgressivePlanLimit
  val Checkpoints: Vector[Int] = Vector(10, 25, 50, 100)
  val Scope: String =
    "certified_global_count_star_retained_structural_top_k_full_two_pass_v1"
  val ScoreOrder: Vector[String] = Vector(
    "sum_named_stage_plan_total_cost",
    "max_named_stage_plan_rows_times_plan_width",
    "sum_named_stage_plan_rows_times_plan_width",
    "final_plan_total_cost",
    "structural_rank",
    "candidate_id"
  )

  require(Checkpoints == Checkpoints.sorted.distinct)
  require(Checkpoints.last == RequiredPlanLimit)
}

/**
  * Exact lexicographic COUNT stage-cost score. Decimal inputs are added with
  * java.math.BigDecimal and are never rounded or converted through binary
  * floating point.
  */
final case class PostgreSqlCountStageCostScore(
    sumNamedStagePlanTotalCost: BigDecimal,
    maxNamedStagePlanRowsTimesPlanWidth: BigDecimal,
    sumNamedStagePlanRowsTimesPlanWidth: BigDecimal,
    finalPlanTotalCost: BigDecimal,
    structuralRank: Int,
    candidateId: CandidatePlanId
) extends Serializable {
  require(
    sumNamedStagePlanTotalCost != null &&
      sumNamedStagePlanTotalCost.signum >= 0,
    "cumulative named-stage plan cost must be nonnegative"
  )
  require(
    maxNamedStagePlanRowsTimesPlanWidth != null &&
      maxNamedStagePlanRowsTimesPlanWidth.signum >= 0,
    "maximum named-stage estimated bytes must be nonnegative"
  )
  require(
    sumNamedStagePlanRowsTimesPlanWidth != null &&
      sumNamedStagePlanRowsTimesPlanWidth.signum >= 0,
    "cumulative named-stage estimated bytes must be nonnegative"
  )
  require(
    finalPlanTotalCost != null && finalPlanTotalCost.signum >= 0,
    "final plan total cost must be nonnegative"
  )
  require(structuralRank > 0, "structural rank must be positive")
  require(candidateId != null, "candidate id must not be null")
  require(
    PostgreSqlFactorAwareNumericEnvelope.acceptsDerived(
      sumNamedStagePlanTotalCost
    ) && PostgreSqlFactorAwareNumericEnvelope.acceptsDerived(
      maxNamedStagePlanRowsTimesPlanWidth
    ) && PostgreSqlFactorAwareNumericEnvelope.acceptsDerived(
      sumNamedStagePlanRowsTimesPlanWidth
    ) && PostgreSqlFactorAwareNumericEnvelope.accepts(finalPlanTotalCost),
    "COUNT stage-cost score exceeds the frozen numeric envelope"
  )
}

object PostgreSqlCountStageCostScore {
  private[postgresql] def exactNamedStageCost(
      stages: Vector[PostgreSqlFactorAwareStageEvaluation]
  ): BigDecimal = {
    require(stages != null && stages.size <= 256 && stages.forall(_ != null))
    PostgreSqlFactorAwareScore.exactSum(stages.collect {
      case stage if stage.trace.physicalForm.namedCte =>
        stage.estimate.get.planTotalCost
    })
  }

  private[postgresql] def from(
      stages: Vector[PostgreSqlFactorAwareStageEvaluation],
      structuralRank: Int,
      candidateId: CandidatePlanId
  ): PostgreSqlCountStageCostScore = {
    val namedBytes = stages.collect {
      case stage if stage.trace.physicalForm.namedCte =>
        val estimate = stage.estimate.get
        PostgreSqlFactorAwareScore.exactEstimatedBytes(
          estimate.planRows,
          estimate.planWidth
        )
    }
    PostgreSqlCountStageCostScore(
      exactNamedStageCost(stages),
      namedBytes.foldLeft(BigDecimal(0))(_.max(_)),
      PostgreSqlFactorAwareScore.exactSum(namedBytes),
      stages.last.estimate.get.planTotalCost,
      structuralRank,
      candidateId
    )
  }

  implicit val ordering: Ordering[PostgreSqlCountStageCostScore] =
    new Ordering[PostgreSqlCountStageCostScore] {
      override def compare(
          left: PostgreSqlCountStageCostScore,
          right: PostgreSqlCountStageCostScore
      ): Int = {
        val stageCost = left.sumNamedStagePlanTotalCost.compare(
          right.sumNamedStagePlanTotalCost
        )
        if (stageCost != 0) return stageCost
        val peak = left.maxNamedStagePlanRowsTimesPlanWidth.compare(
          right.maxNamedStagePlanRowsTimesPlanWidth
        )
        if (peak != 0) return peak
        val sum = left.sumNamedStagePlanRowsTimesPlanWidth.compare(
          right.sumNamedStagePlanRowsTimesPlanWidth
        )
        if (sum != 0) return sum
        val finalCost = left.finalPlanTotalCost.compare(right.finalPlanTotalCost)
        if (finalCost != 0) return finalCost
        val rank = Integer.compare(left.structuralRank, right.structuralRank)
        if (rank != 0) return rank
        left.candidateId.value.compareTo(right.candidateId.value)
      }
    }
}

/** Complete, separately versioned COUNT stage-cost evidence for one plan. */
final case class PostgreSqlCountStageCostCandidateEvidence private[postgresql] (
    physicalVariantId: PostgreSqlPhysicalVariantId,
    stages: Vector[PostgreSqlFactorAwareStageEvaluation],
    score: PostgreSqlCountStageCostScore,
    stageIdScheme: String,
    stageOrderScheme: String,
    statementHashScheme: String,
    characterCountScheme: String
) extends Serializable {
  require(physicalVariantId != null)
  require(stages != null && stages.nonEmpty && stages.forall(_ != null))
  require(
    stages.count(_.trace.physicalForm.emitted) <=
      PostgreSqlFactorAwareSelectionLimits.default.maxStageProbesPerCandidate
  )
  require(stages.map(_.trace.id).distinct.size == stages.size)
  require(score != null)
  require(
    stageIdScheme == PostgreSqlCountStageCostCandidateEvidence.StageIdScheme &&
      stageOrderScheme ==
        PostgreSqlCountStageCostCandidateEvidence.StageOrderScheme &&
      statementHashScheme ==
        PostgreSqlCountStageCostCandidateEvidence.StatementHashScheme &&
      characterCountScheme ==
        PostgreSqlCountStageCostCandidateEvidence.CharacterCountScheme,
    "COUNT stage-cost evidence schemes must match the frozen renderer contract"
  )
  require(
    stages.last.trace.id == PostgreSqlStageId.FinalReconstruction &&
      stages.last.trace.kind == PostgreSqlStageKind.FinalReconstruction &&
      stages.last.trace.physicalForm ==
        PostgreSqlStagePhysicalForm.TerminalSelect &&
      stages.last.estimate.nonEmpty,
    "COUNT stage-cost evidence must end in the estimated reconstruction"
  )
  require(
    score == PostgreSqlCountStageCostScore.from(
      stages,
      score.structuralRank,
      score.candidateId
    ),
    "COUNT stage-cost score must be exactly recomputable from stage evidence"
  )
}

object PostgreSqlCountStageCostCandidateEvidence {
  val StageIdScheme: String = PostgreSqlStageId.Scheme
  val StageOrderScheme: String =
    at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageTrace.OrderScheme
  val StatementHashScheme: String =
    PostgreSqlStageDiagnosticStatement.HashScheme
  val CharacterCountScheme: String =
    PostgreSqlStageDiagnosticStatement.CharacterCountScheme
}

/** Winner (or absence of one) at one COUNT stage-cost prefix. */
final case class PostgreSqlCountStageCostCheckpoint(
    requestedPrefix: Int,
    effectiveCandidateCount: Int,
    truncatedBySearchExhaustion: Boolean,
    winnerStructuralRank: Option[Int],
    winnerCandidateId: Option[CandidatePlanId],
    winnerPhysicalVariantId: Option[PostgreSqlPhysicalVariantId],
    winnerScore: Option[PostgreSqlCountStageCostScore],
    verifiedPrefixSha256: String,
    prefixProofScheme: String
) extends Serializable {
  require(winnerStructuralRank != null)
  require(winnerCandidateId != null && winnerCandidateId.forall(_ != null))
  require(
    winnerPhysicalVariantId != null && winnerPhysicalVariantId.forall(_ != null)
  )
  require(winnerScore != null && winnerScore.forall(_ != null))
  require(
    PostgreSqlCountStageCostProgressiveV1.Checkpoints.contains(requestedPrefix)
  )
  require(effectiveCandidateCount > 0 && effectiveCandidateCount <= requestedPrefix)
  require(truncatedBySearchExhaustion == (effectiveCandidateCount < requestedPrefix))
  require(
    verifiedPrefixSha256 != null && verifiedPrefixSha256.matches("[0-9a-f]{64}")
  )
  require(prefixProofScheme == PostgreSqlFactorAwarePrefixProof.Scheme)
  require(
    Vector(
      winnerStructuralRank.nonEmpty,
      winnerCandidateId.nonEmpty,
      winnerPhysicalVariantId.nonEmpty,
      winnerScore.nonEmpty
    ).distinct.size == 1,
    "checkpoint winner identity and score must be wholly present or absent"
  )
  require(
    winnerStructuralRank.forall(rank =>
      rank > 0 && rank <= effectiveCandidateCount
    )
  )
  require(
    winnerStructuralRank.zip(winnerScore).forall { case (rank, score) =>
      rank == score.structuralRank
    }
  )
  require(
    winnerCandidateId.zip(winnerScore).forall { case (id, score) =>
      id == score.candidateId
    }
  )
}

/** Resource replay for v7 evidence without widening the frozen v5 model. */
private[postgresql] object PostgreSqlCountStageCostDiagnosticReplay {
  def evidenceUsage(
      evidence: PostgreSqlCountStageCostCandidateEvidence,
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Option[PostgreSqlFactorAwareDiagnosticReplay.Usage] =
    try {
      if (
        evidence == null || limits == null || evidence.stages == null ||
        evidence.stages.isEmpty
      ) return None
      var index = 0
      var namedCount = 0L
      var emittedCount = BigInt(0)
      var characterCount = BigInt(0)
      var terminalSeen = false
      while (index < evidence.stages.size) {
        val stage = evidence.stages(index)
        if (stage == null || stage.trace == null || terminalSeen) return None
        stage.trace.physicalForm match {
          case PostgreSqlStagePhysicalForm.Elided =>
            if (
              stage.diagnosticStatementSha256 == null ||
              stage.diagnosticCharacterCount == null ||
              stage.diagnosticCtePrefixCount == null || stage.estimate == null ||
              stage.diagnosticStatementSha256.nonEmpty ||
              stage.diagnosticCharacterCount.nonEmpty ||
              stage.diagnosticCtePrefixCount.nonEmpty || stage.estimate.nonEmpty
            ) return None
          case PostgreSqlStagePhysicalForm.PlannerControlledCte =>
            namedCount += 1L
            if (
              stage.trace.id == PostgreSqlStageId.FinalReconstruction ||
              !stage.diagnosticStatementSha256.exists(
                _.matches("[0-9a-f]{64}")
              ) || !stage.diagnosticCharacterCount.exists(value =>
                value > 0L && value <= limits.maxCharactersPerProbe
              ) || !stage.diagnosticCtePrefixCount.contains(namedCount) ||
              stage.estimate.isEmpty
            ) return None
            emittedCount += 1
            characterCount += BigInt(stage.diagnosticCharacterCount.get)
          case PostgreSqlStagePhysicalForm.MaterializedCte |
              PostgreSqlStagePhysicalForm.NotMaterializedCte =>
            return None
          case PostgreSqlStagePhysicalForm.TerminalSelect =>
            if (
              index != evidence.stages.size - 1 ||
              stage.trace.id != PostgreSqlStageId.FinalReconstruction ||
              !stage.diagnosticStatementSha256.exists(
                _.matches("[0-9a-f]{64}")
              ) || !stage.diagnosticCharacterCount.exists(value =>
                value > 0L && value <= limits.maxCharactersPerProbe
              ) || !stage.diagnosticCtePrefixCount.contains(namedCount) ||
              stage.estimate.isEmpty
            ) return None
            terminalSeen = true
            emittedCount += 1
            characterCount += BigInt(stage.diagnosticCharacterCount.get)
        }
        if (
          emittedCount > BigInt(limits.maxStageProbesPerCandidate) ||
          characterCount > BigInt(limits.maxDiagnosticCharactersPerCandidate)
        ) return None
        index += 1
      }
      if (!terminalSeen) None
      else
        Some(
          PostgreSqlFactorAwareDiagnosticReplay.Usage(
            emittedCount,
            characterCount
          )
        )
    } catch { case scala.util.control.NonFatal(_) => None }
}

/** Total, side-effect-free replay of the separately versioned v7 evidence. */
private[yannakakis] object PostgreSqlCountStageCostSelectionReplay {
  def verify(
      plans: Vector[RankedCandidatePlan],
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      source: PostgreSqlExplainSourceEvaluation,
      checkpoints: Vector[PostgreSqlCountStageCostCheckpoint],
      limits: PostgreSqlFactorAwareSelectionLimits,
      expectedSourceStatementSha256: Option[String]
  ): Boolean =
    try {
      if (
        limits == null || plans == null || plans.isEmpty ||
        plans.size > PostgreSqlCountStageCostProgressiveV1.RequiredPlanLimit ||
        plans.exists(_ == null) || evaluations == null ||
        evaluations.size != plans.size || evaluations.exists(_ == null) ||
        evaluations.exists(
          _.reductionMode != YannakakisReductionMode.FullTwoPass
        ) || source == null || source.estimate == null ||
        expectedSourceStatementSha256 == null ||
        expectedSourceStatementSha256.exists(value =>
          value == null || !value.matches("[0-9a-f]{64}") ||
            value != source.sourceStatementSha256
        ) || !PostgreSqlFactorAwareNumericEnvelope.accepts(source.estimate) ||
        checkpoints == null || checkpoints.exists(_ == null) ||
        checkpoints.map(_.requestedPrefix) !=
          PostgreSqlCountStageCostProgressiveV1.Checkpoints
      ) return false

      val diagnosticUsages = evaluations.map(evaluation =>
        evaluation.countStageCostEvidence.flatMap(evidence =>
          PostgreSqlCountStageCostDiagnosticReplay.evidenceUsage(
            evidence,
            limits
          )
        )
      )
      if (
        diagnosticUsages.exists(_.isEmpty) ||
        !PostgreSqlFactorAwareDiagnosticReplay.withinSession(
          diagnosticUsages.flatten,
          limits
        )
      ) return false
      if (
        plans.map(_.rank) != (1 to plans.size).toVector ||
        plans.exists(plan =>
          plan.candidateId == null || plan.decomposition == null ||
            plan.candidateId != CandidatePlanId.from(plan.decomposition)
        ) || plans.map(_.candidateId).distinct.size != plans.size
      ) return false

      val evaluationsValid = plans.zip(evaluations).forall {
        case (plan, evaluation) =>
          val evidence = evaluation.countStageCostEvidence.orNull
          val terminal =
            if (
              evidence == null || evidence.stages == null ||
              evidence.stages.isEmpty
            ) null
            else evidence.stages.last
          val recomputed =
            if (evidence == null || evidence.stages == null) null
            else
              PostgreSqlCountStageCostScore.from(
                evidence.stages,
                evaluation.structuralRank,
                evaluation.candidateId
              )
          plan.rank == evaluation.structuralRank &&
          plan.candidateId == evaluation.candidateId &&
          evaluation.factorAwareEvidence.isEmpty && evidence != null &&
          evaluation.physicalVariantId == PostgreSqlPhysicalVariantId.from(
            plan.candidateId,
            YannakakisReductionMode.FullTwoPass
          ) && evidence.physicalVariantId == evaluation.physicalVariantId &&
          terminal != null && terminal.estimate.nonEmpty &&
          terminal.diagnosticStatementSha256.contains(
            evaluation.renderedStatementSha256
          ) && evaluation.explainCostEstimate.contains(terminal.estimate.get) &&
          evidence.score == recomputed &&
          terminal.estimate.get.provenance == source.estimate.provenance &&
          evaluation.rowGuardEvidence.contains(
            PostgreSqlExplainRowGuardEvidence.from(
              source.estimate.planRows,
              terminal.estimate.get.planRows
            )
          ) && evidence.stages.forall(stage =>
            stage != null && stage.estimate.forall(estimate =>
              PostgreSqlFactorAwareNumericEnvelope.accepts(estimate) &&
                estimate.provenance == source.estimate.provenance
            )
          )
      }
      if (!evaluationsValid) return false

      checkpoints.forall { checkpoint =>
        val effective = math.min(checkpoint.requestedPrefix, plans.size)
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
        checkpoint.effectiveCandidateCount == effective &&
        checkpoint.truncatedBySearchExhaustion ==
          (effective < checkpoint.requestedPrefix) &&
        checkpoint.prefixProofScheme == PostgreSqlFactorAwarePrefixProof.Scheme &&
        checkpoint.verifiedPrefixSha256 ==
          PostgreSqlFactorAwarePrefixProof.digest(plans.take(effective)) &&
        checkpoint.winnerStructuralRank == winner.map(_.structuralRank) &&
        checkpoint.winnerCandidateId == winner.map(_.candidateId) &&
        checkpoint.winnerPhysicalVariantId == winner.map(_.physicalVariantId) &&
        checkpoint.winnerScore ==
          winner.flatMap(_.countStageCostEvidence.map(_.score))
      }
    } catch { case scala.util.control.NonFatal(_) => false }
}

/** Separate public replay name for v7 failure artifacts. */
private[yannakakis] object PostgreSqlCountStageCostFailureReplay {
  def verify(
      plans: Vector[RankedCandidatePlan],
      failure: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed,
      expectedSourceStatementSha256: Option[String]
  ): Boolean =
    PostgreSqlFactorAwareFailureReplay.verifyCountStageCost(
      plans,
      failure,
      expectedSourceStatementSha256
    )
}
