package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlBagSupportStrategy,
  PostgreSqlRenderedQuery
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  YannakakisReductionMode
}

/** Exact emitted-shape score used by PostgreSqlStaticShapeV1.
  *
  * This is deliberately not a cardinality, EXPLAIN, learned, or execution-cost
  * estimate. The ordering is part of the versioned public policy contract.
  */
final case class PostgreSqlStaticShapeScore(
    expandedScanCount: Long,
    materializedDistinctSupportCount: Long,
    cteCount: Long,
    renderedTermCount: Long,
    sqlCharacterCount: Long,
    structuralRank: Int
) extends Serializable {
  require(expandedScanCount >= 0L, "expanded scan count must not be negative")
  require(
    materializedDistinctSupportCount >= 0L,
    "materialized DISTINCT support count must not be negative"
  )
  require(cteCount >= 0L, "CTE count must not be negative")
  require(renderedTermCount >= 0L, "rendered term count must not be negative")
  require(sqlCharacterCount >= 0L, "SQL character count must not be negative")
  require(structuralRank > 0, "structural rank must be positive")
}

object PostgreSqlStaticShapeScore {
  val OrderedFields: Vector[String] = Vector(
    "expanded_scan_count",
    "materialized_distinct_support_count",
    "cte_count",
    "rendered_term_count",
    "sql_character_count",
    "structural_rank"
  )

  implicit val ordering: Ordering[PostgreSqlStaticShapeScore] =
    new Ordering[PostgreSqlStaticShapeScore] {
      override def compare(
          left: PostgreSqlStaticShapeScore,
          right: PostgreSqlStaticShapeScore
      ): Int = {
        val expanded = java.lang.Long.compare(
          left.expandedScanCount,
          right.expandedScanCount
        )
        if (expanded != 0) return expanded
        val support = java.lang.Long.compare(
          left.materializedDistinctSupportCount,
          right.materializedDistinctSupportCount
        )
        if (support != 0) return support
        val ctes = java.lang.Long.compare(left.cteCount, right.cteCount)
        if (ctes != 0) return ctes
        val terms = java.lang.Long.compare(
          left.renderedTermCount,
          right.renderedTermCount
        )
        if (terms != 0) return terms
        val characters = java.lang.Long.compare(
          left.sqlCharacterCount,
          right.sqlCharacterCount
        )
        if (characters != 0) return characters
        Integer.compare(left.structuralRank, right.structuralRank)
      }
    }

  private[postgresql] def from(
      rendered: PostgreSqlRenderedQuery,
      structuralRank: Int
  ): PostgreSqlStaticShapeScore = {
    require(rendered != null, "rendered query must not be null")
    val statistics = rendered.statistics
    PostgreSqlStaticShapeScore(
      expandedScanCount = statistics.expandedScanCount,
      materializedDistinctSupportCount = rendered.trace.bags.count(
        _.supportStrategy ==
          PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
      ).toLong,
      cteCount = statistics.cteCount,
      renderedTermCount = statistics.renderedTermCount,
      sqlCharacterCount = statistics.sqlCharacterCount,
      structuralRank = structuralRank
    )
  }
}

/** Immutable public evidence for one completely rendered candidate. */
final case class PostgreSqlCandidateEvaluation private[postgresql] (
    candidateId: CandidatePlanId,
    structuralRank: Int,
    reductionMode: YannakakisReductionMode,
    reductionOrder: String,
    physicalVariantId: PostgreSqlPhysicalVariantId,
    score: PostgreSqlStaticShapeScore,
    renderedStatementSha256: String,
    explainCostEstimate: Option[PostgreSqlExplainCostEstimate] = None,
    rowGuardEvidence: Option[PostgreSqlExplainRowGuardEvidence] = None,
    factorAwareEvidence: Option[PostgreSqlFactorAwareCandidateEvidence] = None,
    countStageCostEvidence: Option[
      PostgreSqlCountStageCostCandidateEvidence
    ] = None
) extends Serializable {
  require(candidateId != null, "candidate id must not be null")
  require(structuralRank > 0, "candidate structural rank must be positive")
  require(reductionMode != null, "reduction mode must not be null")
  require(
    reductionOrder == PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
    "reduction order must be the canonical plan schedule"
  )
  require(physicalVariantId != null, "physical variant id must not be null")
  require(
    physicalVariantId ==
      PostgreSqlPhysicalVariantId.from(candidateId, reductionMode),
    "physical variant id must match candidate and reduction mode"
  )
  require(score != null, "candidate score must not be null")
  require(
    score.structuralRank == structuralRank,
    "candidate score rank must equal candidate structural rank"
  )
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}"),
    "rendered statement digest must be lowercase SHA-256"
  )
  require(explainCostEstimate != null, "EXPLAIN cost estimate option must not be null")
  require(
    explainCostEstimate.forall(_ != null),
    "EXPLAIN cost estimate option must not contain null"
  )
  require(
    rowGuardEvidence != null && rowGuardEvidence.forall(_ != null),
    "row guard evidence option must not be null or contain null"
  )
  require(
    explainCostEstimate.nonEmpty == rowGuardEvidence.nonEmpty,
    "EXPLAIN cost and row guard evidence must occur together"
  )
  require(
    factorAwareEvidence != null && factorAwareEvidence.forall(_ != null),
    "factor-aware evidence option must not be null or contain null"
  )
  factorAwareEvidence.foreach { evidence =>
    require(
      evidence.physicalVariantId == physicalVariantId,
      "factor-aware evidence must match the physical variant"
    )
    require(
      evidence.score.structuralRank == structuralRank &&
        evidence.score.candidateId == candidateId,
      "factor-aware score must match the candidate identity"
    )
    require(
      explainCostEstimate.contains(evidence.stages.last.estimate.get),
      "factor-aware terminal estimate must be the candidate root estimate"
    )
    require(
      evidence.stages.last.diagnosticStatementSha256.contains(
        renderedStatementSha256
      ),
      "factor-aware terminal diagnostic digest must match the rendered statement"
    )
  }
  require(
    countStageCostEvidence != null && countStageCostEvidence.forall(_ != null),
    "COUNT stage-cost evidence option must not be null or contain null"
  )
  countStageCostEvidence.foreach { evidence =>
    require(
      evidence.physicalVariantId == physicalVariantId,
      "COUNT stage-cost evidence must match the physical variant"
    )
    require(
      evidence.score.structuralRank == structuralRank &&
        evidence.score.candidateId == candidateId,
      "COUNT stage-cost score must match the candidate identity"
    )
    require(
      explainCostEstimate.contains(evidence.stages.last.estimate.get),
      "COUNT stage-cost terminal estimate must be the candidate root estimate"
    )
    require(
      evidence.stages.last.diagnosticStatementSha256.contains(
        renderedStatementSha256
      ),
      "COUNT stage-cost terminal digest must match the rendered statement"
    )
  }
  require(
    factorAwareEvidence.isEmpty || countStageCostEvidence.isEmpty,
    "v5 factor evidence and v7 COUNT stage-cost evidence are mutually exclusive"
  )
}

private[postgresql] object PostgreSqlCandidateEvaluationEvidence {
  def renderedStatementSha256(sql: String): String = {
    require(sql != null, "rendered SQL must not be null")
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(sql.getBytes(StandardCharsets.UTF_8))
    val alphabet = "0123456789abcdef"
    val result = new StringBuilder(bytes.length * 2)
    bytes.foreach { byte =>
      val unsigned = byte & 0xff
      result.append(alphabet.charAt(unsigned >>> 4))
      result.append(alphabet.charAt(unsigned & 0x0f))
    }
    result.result()
  }
}

sealed trait PostgreSqlCandidateSelectionDecision
    extends Product
    with Serializable {
  def policy: CandidatePlanSelection
  def scope: String
  def reductionMode: YannakakisReductionMode
  def evaluations: Vector[PostgreSqlCandidateEvaluation]
  def sourceEvaluation: Option[PostgreSqlExplainSourceEvaluation]
  def factorAwareCheckpoints: Vector[PostgreSqlFactorAwareCheckpoint] =
    Vector.empty
  def factorAwareSelectionLimits: Option[PostgreSqlFactorAwareSelectionLimits] =
    None
  def countStageCostCheckpoints: Vector[PostgreSqlCountStageCostCheckpoint] =
    Vector.empty
  def countStageCostSelectionLimits
      : Option[PostgreSqlFactorAwareSelectionLimits] = None
  final def reductionOrder: String =
    PostgreSqlPhysicalVariantId.CanonicalReductionOrder

  final def costSelected: Boolean =
    policy == CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 ||
      policy ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ||
      policy ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
  final def estimatedCostSelected: Boolean = costSelected
  final def measuredRuntimeSelected: Boolean = false
  final def usesCatalogStatistics: Boolean = costSelected
  final def usesExecutionFeedback: Boolean = false
  final def explainCostProvenance: Option[PostgreSqlExplainCostProvenance] =
    sourceEvaluation.map(_.estimate.provenance).orElse(
      evaluations.headOption.flatMap(_.explainCostEstimate.map(_.provenance))
    )
}

object PostgreSqlCandidateSelectionDecision {
  val Scope: String =
    "retained_structural_top_k_first_canonical_cover_v1"

  private def scopeFor(policy: CandidatePlanSelection): String =
    if (
      policy ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
    ) PostgreSqlCountStageCostProgressiveV1.Scope
    else Scope

  private[postgresql] val explainOrdering
      : Ordering[PostgreSqlCandidateEvaluation] =
    new Ordering[PostgreSqlCandidateEvaluation] {
      override def compare(
          left: PostgreSqlCandidateEvaluation,
          right: PostgreSqlCandidateEvaluation
      ): Int = {
        val cost = left.explainCostEstimate.get.planTotalCost.compare(
          right.explainCostEstimate.get.planTotalCost
        )
        if (cost != 0) return cost
        val rank = Integer.compare(left.structuralRank, right.structuralRank)
        if (rank != 0) return rank
        left.candidateId.value.compareTo(right.candidateId.value)
      }
    }

  private[postgresql] val factorAwareOrdering
      : Ordering[PostgreSqlCandidateEvaluation] =
    new Ordering[PostgreSqlCandidateEvaluation] {
      override def compare(
          left: PostgreSqlCandidateEvaluation,
          right: PostgreSqlCandidateEvaluation
      ): Int =
        PostgreSqlFactorAwareScore.ordering.compare(
          left.factorAwareEvidence.get.score,
          right.factorAwareEvidence.get.score
        )
    }

  private[postgresql] val countStageCostOrdering
      : Ordering[PostgreSqlCandidateEvaluation] =
    new Ordering[PostgreSqlCandidateEvaluation] {
      override def compare(
          left: PostgreSqlCandidateEvaluation,
          right: PostgreSqlCandidateEvaluation
      ): Int =
        PostgreSqlCountStageCostScore.ordering.compare(
          left.countStageCostEvidence.get.score,
          right.countStageCostEvidence.get.score
        )
    }

  private def requireEvidenceMatchesPolicy(
      policy: CandidatePlanSelection,
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      sourceEvaluation: Option[PostgreSqlExplainSourceEvaluation]
  ): Unit = {
    require(
      policy !=
        CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1,
      "physical portfolio evidence must use its separate v6 decision model"
    )
    val expectsFactor =
      policy ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
    val expectsCountStageCost =
      policy ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
    val expectsExplain =
      policy == CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 ||
        expectsFactor || expectsCountStageCost
    require(
      evaluations.forall(_.explainCostEstimate.nonEmpty == expectsExplain),
      "candidate estimates must match the declared selection policy"
    )
    require(
      evaluations.forall(_.rowGuardEvidence.nonEmpty == expectsExplain),
      "candidate row guards must match the declared selection policy"
    )
    require(
      evaluations.forall(_.factorAwareEvidence.nonEmpty == expectsFactor),
      "candidate factor evidence must match the declared selection policy"
    )
    require(
      evaluations.forall(
        _.countStageCostEvidence.nonEmpty == expectsCountStageCost
      ),
      "candidate COUNT stage-cost evidence must match the declared selection policy"
    )
    require(
      !expectsExplain || evaluations.isEmpty || sourceEvaluation.nonEmpty,
      "candidate optimizer estimates require a source baseline"
    )
    require(
      expectsExplain || sourceEvaluation.isEmpty,
      "non-EXPLAIN policies must not contain a source optimizer estimate"
    )
    require(
      (sourceEvaluation.map(_.estimate.provenance).toVector ++
        evaluations.flatMap(_.explainCostEstimate.map(_.provenance)) ++
        evaluations.flatMap(_.factorAwareEvidence.toVector.flatMap(
          _.stages.flatMap(_.estimate.map(_.provenance))
        )) ++
        evaluations.flatMap(_.countStageCostEvidence.toVector.flatMap(
          _.stages.flatMap(_.estimate.map(_.provenance))
        ))).distinct.size <= 1,
      "source and candidate estimates must share one EXPLAIN provenance value"
    )
    require(
      !(expectsFactor || expectsCountStageCost) ||
        (sourceEvaluation.forall(source =>
          source != null && source.estimate != null &&
            PostgreSqlFactorAwareNumericEnvelope.accepts(source.estimate)
        ) && evaluations.forall(evaluation =>
          evaluation.explainCostEstimate.exists(estimate =>
            PostgreSqlFactorAwareNumericEnvelope.accepts(estimate)
          )
        )),
      "factor-aware estimates must stay inside the frozen numeric envelope"
    )
    sourceEvaluation.foreach { source =>
      evaluations.foreach { evaluation =>
        require(
          evaluation.rowGuardEvidence.contains(
            PostgreSqlExplainRowGuardEvidence.from(
              source.estimate.planRows,
              evaluation.explainCostEstimate.get.planRows
            )
          ),
          "candidate row guard must match the source and candidate estimates"
        )
      }
    }
  }

  final case class Selected private[postgresql] (
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      selectedCandidateId: CandidatePlanId,
      selectedStructuralRank: Int,
      selectedPhysicalVariantId: PostgreSqlPhysicalVariantId,
      sourceEvaluation: Option[PostgreSqlExplainSourceEvaluation] = None,
      override val factorAwareCheckpoints
          : Vector[PostgreSqlFactorAwareCheckpoint] = Vector.empty,
      override val factorAwareSelectionLimits
          : Option[PostgreSqlFactorAwareSelectionLimits] = None,
      override val countStageCostCheckpoints
          : Vector[PostgreSqlCountStageCostCheckpoint] = Vector.empty,
      override val countStageCostSelectionLimits
          : Option[PostgreSqlFactorAwareSelectionLimits] = None
  ) extends PostgreSqlCandidateSelectionDecision {
    override val scope: String = scopeFor(policy)
    require(policy != null, "candidate selection policy must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    require(evaluations != null, "candidate evaluations must not be null")
    require(evaluations.nonEmpty, "selected decision requires an evaluation")
    require(
      evaluations.forall(_ != null),
      "candidate evaluations must not contain null"
    )
    require(
      evaluations.forall(evaluation =>
        evaluation.reductionMode == reductionMode &&
          evaluation.reductionOrder == reductionOrder
      ),
      "candidate evaluations must use the request-wide reduction schedule"
    )
    require(
      evaluations.map(_.structuralRank) ==
        evaluations.map(_.structuralRank).sorted.distinct,
      "candidate evaluations must have unique increasing ranks"
    )
    requireEvidenceMatchesPolicy(policy, evaluations, sourceEvaluation)
    private val factorAware =
      policy ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
    private val countStageCost =
      policy ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
    require(
      factorAwareCheckpoints != null && factorAwareCheckpoints.forall(_ != null)
    )
    require(
      factorAwareCheckpoints.nonEmpty == factorAware,
      "progressive checkpoints must match the declared policy"
    )
    require(
      factorAwareSelectionLimits != null &&
        factorAwareSelectionLimits.nonEmpty == factorAware,
      "factor-aware limits must match the declared policy"
    )
    require(
      countStageCostCheckpoints != null &&
        countStageCostCheckpoints.forall(_ != null)
    )
    require(
      countStageCostCheckpoints.nonEmpty == countStageCost,
      "COUNT stage-cost checkpoints must match the declared policy"
    )
    require(
      countStageCostSelectionLimits != null &&
        countStageCostSelectionLimits.nonEmpty == countStageCost,
      "COUNT stage-cost limits must match the declared policy"
    )
    require(
      !countStageCost ||
        countStageCostCheckpoints.map(_.requestedPrefix) ==
          PostgreSqlCountStageCostProgressiveV1.Checkpoints,
      "COUNT stage-cost checkpoints must use the frozen prefix schedule"
    )
    require(
      !factorAware ||
        factorAwareCheckpoints.map(_.requestedPrefix) ==
          PostgreSqlFactorAwareProgressiveV1.Checkpoints,
      "factor-aware checkpoints must use the frozen prefix schedule"
    )
    require(selectedCandidateId != null, "selected candidate id must not be null")
    require(selectedStructuralRank > 0, "selected structural rank must be positive")
    require(
      selectedPhysicalVariantId != null,
      "selected physical variant id must not be null"
    )
    require(
      selectedPhysicalVariantId ==
        PostgreSqlPhysicalVariantId.from(selectedCandidateId, reductionMode),
      "selected physical variant id must match candidate and reduction mode"
    )
    require(
      evaluations.exists(evaluation =>
        evaluation.candidateId == selectedCandidateId &&
          evaluation.structuralRank == selectedStructuralRank &&
          evaluation.physicalVariantId == selectedPhysicalVariantId
      ),
      "selected candidate must occur in evaluations"
    )
    require(
      !costSelected ||
        evaluations.exists(evaluation =>
          evaluation.candidateId == selectedCandidateId &&
            evaluation.rowGuardEvidence.exists(_.withinGuard)
        ),
      "guarded EXPLAIN selection must select an admissible candidate"
    )
    require(
      !factorAware || factorAwareCheckpoints.lastOption.exists(checkpoint =>
        checkpoint.winnerCandidateId.contains(selectedCandidateId) &&
          checkpoint.winnerStructuralRank.contains(selectedStructuralRank) &&
          checkpoint.winnerPhysicalVariantId.contains(
            selectedPhysicalVariantId
          )
      ),
      "factor-aware selection must equal the final checkpoint winner"
    )
    require(
      !countStageCost || countStageCostCheckpoints.lastOption.exists(checkpoint =>
        checkpoint.winnerCandidateId.contains(selectedCandidateId) &&
          checkpoint.winnerStructuralRank.contains(selectedStructuralRank) &&
          checkpoint.winnerPhysicalVariantId.contains(
            selectedPhysicalVariantId
          )
      ),
      "COUNT stage-cost selection must equal the final checkpoint winner"
    )
  }

  final case class Unavailable private[postgresql] (
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      retainedCandidateCount: Int
  ) extends PostgreSqlCandidateSelectionDecision {
    override val scope: String = Scope
    override val evaluations: Vector[PostgreSqlCandidateEvaluation] =
      Vector.empty
    override val sourceEvaluation: Option[PostgreSqlExplainSourceEvaluation] =
      None
    require(policy != null, "candidate selection policy must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    require(
      retainedCandidateCount >= 0,
      "retained candidate count must not be negative"
    )
    requireEvidenceMatchesPolicy(policy, evaluations, sourceEvaluation)
  }

  final case class EvaluationFailed private[postgresql] (
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      failedCandidateId: CandidatePlanId,
      failedStructuralRank: Int,
      failedPhysicalVariantId: PostgreSqlPhysicalVariantId
  ) extends PostgreSqlCandidateSelectionDecision {
    override val scope: String = Scope
    override val sourceEvaluation: Option[PostgreSqlExplainSourceEvaluation] =
      None
    require(policy != null, "candidate selection policy must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    require(evaluations != null, "candidate evaluations must not be null")
    require(
      evaluations.forall(_ != null),
      "candidate evaluations must not contain null"
    )
    require(
      evaluations.forall(evaluation =>
        evaluation.reductionMode == reductionMode &&
          evaluation.reductionOrder == reductionOrder
      ),
      "candidate evaluations must use the request-wide reduction schedule"
    )
    require(
      evaluations.map(_.structuralRank) ==
        evaluations.map(_.structuralRank).sorted.distinct,
      "candidate evaluations must have unique increasing ranks"
    )
    requireEvidenceMatchesPolicy(policy, evaluations, sourceEvaluation)
    require(failedCandidateId != null, "failed candidate id must not be null")
    require(failedStructuralRank > 0, "failed structural rank must be positive")
    require(
      failedPhysicalVariantId != null,
      "failed physical variant id must not be null"
    )
    require(
      failedPhysicalVariantId ==
        PostgreSqlPhysicalVariantId.from(failedCandidateId, reductionMode),
      "failed physical variant id must match candidate and reduction mode"
    )
    require(
      !evaluations.exists(_.structuralRank == failedStructuralRank),
      "failed candidate must not also be a completed evaluation"
    )
  }

  final case class ExplainEvaluationFailed private[postgresql] (
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      sourceEvaluation: Option[PostgreSqlExplainSourceEvaluation],
      stage: PostgreSqlExplainEvaluationStage,
      failedCandidateId: Option[CandidatePlanId],
      failedStructuralRank: Option[Int],
      failedPhysicalVariantId: Option[PostgreSqlPhysicalVariantId]
  ) extends PostgreSqlCandidateSelectionDecision {
    override val scope: String = Scope
    require(
      policy == CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1,
      "EXPLAIN failure must use the guarded EXPLAIN policy"
    )
    require(reductionMode != null, "reduction mode must not be null")
    require(evaluations != null && evaluations.forall(_ != null))
    require(sourceEvaluation != null && sourceEvaluation.forall(_ != null))
    require(stage != null, "EXPLAIN failure stage must not be null")
    require(
      failedCandidateId != null && failedStructuralRank != null &&
        failedPhysicalVariantId != null,
      "failed candidate options must not be null"
    )
    require(
      Vector(
        failedCandidateId.nonEmpty,
        failedStructuralRank.nonEmpty,
        failedPhysicalVariantId.nonEmpty
      ).distinct.size == 1,
      "failed candidate identity must be wholly present or absent"
    )
    require(
      (stage == PostgreSqlExplainEvaluationStage.CandidateExplain) ==
        failedCandidateId.nonEmpty,
      "only candidate EXPLAIN failures identify a failed candidate"
    )
    require(
      stage != PostgreSqlExplainEvaluationStage.CandidateExplain ||
        sourceEvaluation.nonEmpty,
      "candidate EXPLAIN failures require a completed source estimate"
    )
    require(
      stage != PostgreSqlExplainEvaluationStage.CandidateExplain ||
        failedStructuralRank.exists(rank =>
          evaluations.map(_.structuralRank) == (1 until rank).toVector
        ),
      "candidate EXPLAIN failures require the exact completed rank prefix"
    )
    require(
      stage != PostgreSqlExplainEvaluationStage.SourceExplain ||
        (sourceEvaluation.isEmpty && evaluations.isEmpty),
      "source EXPLAIN failures cannot contain completed estimate evidence"
    )
    require(
      failedStructuralRank.forall(_ > 0),
      "failed structural rank must be positive"
    )
    require(
      failedCandidateId.zip(failedPhysicalVariantId).forall {
        case (candidateId, physicalId) =>
          physicalId ==
            PostgreSqlPhysicalVariantId.from(candidateId, reductionMode)
      },
      "failed physical variant must match candidate and reduction mode"
    )
    require(
      evaluations.forall(evaluation =>
        evaluation.reductionMode == reductionMode &&
          evaluation.reductionOrder == reductionOrder
      ),
      "candidate evaluations must use the request-wide reduction schedule"
    )
    require(
      evaluations.map(_.structuralRank) ==
        evaluations.map(_.structuralRank).sorted.distinct,
      "candidate evaluations must have unique increasing ranks"
    )
    requireEvidenceMatchesPolicy(policy, evaluations, sourceEvaluation)
  }

  final case class FactorAwareEvaluationFailed private[postgresql] (
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      sourceEvaluation: Option[PostgreSqlExplainSourceEvaluation],
      stage: PostgreSqlExplainEvaluationStage,
      failureCode: PostgreSqlExplainCostFailureCode,
      failedCandidateId: Option[CandidatePlanId],
      failedStructuralRank: Option[Int],
      failedPhysicalVariantId: Option[PostgreSqlPhysicalVariantId],
      failedStageId: Option[
        at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId
      ],
      failedStageObservationIndex: Option[Int],
      completedStageObservations: Vector[
        PostgreSqlFactorAwareExplainObservation
      ],
      renderedCandidates: Vector[
        PostgreSqlFactorAwareRenderedCandidateIdentity
      ],
      override val factorAwareSelectionLimits
          : Option[PostgreSqlFactorAwareSelectionLimits]
  ) extends PostgreSqlCandidateSelectionDecision {
    override val scope: String = scopeFor(policy)
    require(
      policy ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ||
        policy ==
          CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
    )
    private val countStageCost =
      policy ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
    require(
      !countStageCost || reductionMode == YannakakisReductionMode.FullTwoPass,
      "COUNT stage-cost failure must use FullTwoPass"
    )
    require(reductionMode != null)
    require(evaluations != null && evaluations.forall(_ != null))
    require(sourceEvaluation != null && sourceEvaluation.forall(_ != null))
    require(stage != null)
    require(failureCode != null)
    require(
      PostgreSqlFactorAwareFailureMatrix.accepts(failureCode, stage),
      "factor failure code must match its evaluation stage"
    )
    require(failedCandidateId != null)
    require(failedStructuralRank != null)
    require(failedPhysicalVariantId != null)
    require(failedStageId != null)
    require(
      failedStageObservationIndex != null &&
        failedStageObservationIndex.forall(_ >= 0)
    )
    require(
      completedStageObservations != null &&
        completedStageObservations.forall(_ != null)
    )
    require(
      factorAwareSelectionLimits != null && factorAwareSelectionLimits.nonEmpty
    )
    private val factorLimits = factorAwareSelectionLimits.get
    private def completedEvidenceStages(
        evaluation: PostgreSqlCandidateEvaluation
    ): Option[Vector[PostgreSqlFactorAwareStageEvaluation]] =
      if (countStageCost)
        evaluation.countStageCostEvidence.map(_.stages)
      else evaluation.factorAwareEvidence.map(_.stages)
    require(
      evaluations.size <= factorLimits.maxCandidates &&
        evaluations.forall(evaluation =>
          completedEvidenceStages(evaluation).exists(stages =>
            stages.count(_.trace.physicalForm.emitted) <=
              factorLimits.maxStageProbesPerCandidate
          )
        ),
      "completed factor evaluations must stay within the factor envelope"
    )
    require(
      renderedCandidates != null && renderedCandidates.forall(_ != null) &&
        renderedCandidates.size <= factorLimits.maxCandidates &&
        renderedCandidates.forall(candidate =>
          candidate.stages != null && candidate.stages.nonEmpty &&
            candidate.stages.size <= factorLimits.maxStageProbesPerCandidate
        ) &&
        renderedCandidates.map(_.structuralRank) ==
          (1 to renderedCandidates.size).toVector
    )
    private val renderedStatementCount = renderedCandidates.foldLeft(BigInt(0))(
      _ + _.stages.size
    )
    require(
      renderedStatementCount <=
        BigInt(factorLimits.maxExplainStatementsPerSession - 1L) &&
        renderedStatementCount <= BigInt(4095),
      "rendered factor failure evidence must stay within the session envelope"
    )
    require(
      renderedCandidates.forall(candidate =>
        candidate.physicalVariantId ==
          PostgreSqlPhysicalVariantId.from(
            candidate.candidateId,
            reductionMode
          )
      ),
      "rendered factor identities must match the decision reduction mode"
    )
    private val flattenedRenderedStages = renderedCandidates.flatMap {
      candidate =>
        candidate.stages.map(stage =>
          (candidate.physicalVariantId, candidate.structuralRank, candidate.candidateId, stage)
        )
    }
    require(
      completedStageObservations.size <= flattenedRenderedStages.size &&
        completedStageObservations.zip(flattenedRenderedStages).forall {
          case (observation, (physicalId, _, _, renderedStage)) =>
            observation.physicalVariantId == physicalId &&
              observation.stageId == renderedStage.stageId &&
              observation.statementSha256 == renderedStage.statementSha256
        },
      "completed factor observations must be the exact rendered statement prefix"
    )
    require(
      Vector(
        failedCandidateId.nonEmpty,
        failedStructuralRank.nonEmpty,
        failedPhysicalVariantId.nonEmpty
      ).distinct.size == 1
    )
    require(
      (stage == PostgreSqlExplainEvaluationStage.StageExplain) ==
        failedStageId.nonEmpty
    )
    require(
      failedStageId.nonEmpty == failedStageObservationIndex.nonEmpty &&
        failedStageObservationIndex.forall(_ == completedStageObservations.size)
    )
    require(
      failedCandidateId.nonEmpty == Set(
        PostgreSqlExplainEvaluationStage.CandidatePlanning,
        PostgreSqlExplainEvaluationStage.StageDiagnosticRendering,
        PostgreSqlExplainEvaluationStage.ResourceValidation,
        PostgreSqlExplainEvaluationStage.StageExplain
      ).contains(stage),
      "failed candidate identity must match the factor failure stage"
    )
    require(
      failedStructuralRank.forall(_ > 0) &&
        failedCandidateId.zip(failedPhysicalVariantId).forall {
          case (candidateId, physicalId) =>
            physicalId ==
              PostgreSqlPhysicalVariantId.from(candidateId, reductionMode)
        }
    )
    require(
      failedStructuralRank.forall { rank =>
        stage match {
          case PostgreSqlExplainEvaluationStage.CandidatePlanning |
              PostgreSqlExplainEvaluationStage.StageDiagnosticRendering =>
            renderedCandidates.size == rank - 1
          case PostgreSqlExplainEvaluationStage.ResourceValidation =>
            renderedCandidates.size == rank - 1 ||
              renderedCandidates.size == rank
          case PostgreSqlExplainEvaluationStage.StageExplain =>
            renderedCandidates.exists(candidate =>
              candidate.structuralRank == rank &&
                failedCandidateId.contains(candidate.candidateId)
            )
          case _ => false
        }
      },
      "failed factor rank must match the rendered prefix"
    )
    require(
      stage != PostgreSqlExplainEvaluationStage.PrefixVerification ||
        (renderedCandidates.isEmpty && sourceEvaluation.isEmpty &&
          completedStageObservations.isEmpty),
      "prefix verification failures cannot claim rendered or optimizer evidence"
    )
    require(
      stage != PostgreSqlExplainEvaluationStage.SourceExplain ||
        (sourceEvaluation.isEmpty && completedStageObservations.isEmpty),
      "source EXPLAIN failures cannot claim completed optimizer evidence"
    )
    require(
      stage != PostgreSqlExplainEvaluationStage.Provenance ||
        ((sourceEvaluation.isEmpty && completedStageObservations.isEmpty) ||
          (sourceEvaluation.nonEmpty &&
            completedStageObservations.size == flattenedRenderedStages.size)),
      "provenance failures must occur before or after the complete EXPLAIN batch"
    )
    require(
      stage != PostgreSqlExplainEvaluationStage.Session ||
        (if (failureCode == PostgreSqlExplainCostFailureCode.CleanupFailed)
           sourceEvaluation.nonEmpty || completedStageObservations.isEmpty
         else sourceEvaluation.isEmpty && completedStageObservations.isEmpty),
      "session evidence is retained only for cleanup failure prefixes"
    )
    require(
      failedStageObservationIndex.forall { index =>
        index < flattenedRenderedStages.size && {
          val (physicalId, rank, candidateId, renderedStage) =
            flattenedRenderedStages(index)
          failedPhysicalVariantId.contains(physicalId) &&
          failedStructuralRank.contains(rank) &&
          failedCandidateId.contains(candidateId) &&
          failedStageId.contains(renderedStage.stageId)
        }
      },
      "failed factor stage must identify the next rendered statement"
    )
    private val completedRenderedPrefix = {
      var candidateCount = 0
      var stageCount = 0
      while (
        candidateCount < renderedCandidates.size &&
        stageCount + renderedCandidates(candidateCount).stages.size <=
          completedStageObservations.size
      ) {
        stageCount += renderedCandidates(candidateCount).stages.size
        candidateCount += 1
      }
      candidateCount -> stageCount
    }
    private val completedRenderedCandidateCount = completedRenderedPrefix._1
    require(
      evaluations.map(evaluation =>
        (evaluation.structuralRank, evaluation.candidateId, evaluation.physicalVariantId)
      ) == renderedCandidates.take(completedRenderedCandidateCount).map(
        candidate =>
          (candidate.structuralRank, candidate.candidateId, candidate.physicalVariantId)
      ),
      "completed factor evaluations must equal the whole-candidate observation prefix"
    )
    require(
      {
        var offset = 0
        val valid = evaluations.forall { evaluation =>
          val emittedEvidence = completedEvidenceStages(evaluation).toVector
            .flatMap(_.filter(_.trace.physicalForm.emitted))
          val observed = completedStageObservations.slice(
            offset,
            offset + emittedEvidence.size
          )
          offset += emittedEvidence.size
          emittedEvidence.size == observed.size &&
          emittedEvidence.zip(observed).forall { case (evidence, observation) =>
            evidence.trace.id == observation.stageId &&
            evidence.diagnosticStatementSha256.contains(
              observation.statementSha256
            ) && evidence.estimate.contains(observation.estimate)
          }
        }
        valid && offset == completedRenderedPrefix._2
      },
      "completed factor evaluations must replay the exact observation prefix"
    )
    require(
      evaluations.map(_.structuralRank) ==
        evaluations.map(_.structuralRank).sorted.distinct
    )
    requireEvidenceMatchesPolicy(policy, evaluations, sourceEvaluation)
    require(
      sourceEvaluation.nonEmpty || completedStageObservations.isEmpty,
      "stage observation prefix requires a source baseline"
    )
    require(
      completedStageObservations.forall(observation =>
        sourceEvaluation.exists(
          _.estimate.provenance == observation.estimate.provenance
        )
      ),
      "stage observation prefix must share source provenance"
    )
  }

  final case class NoAdmissibleEstimate private[postgresql] (
      policy: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode,
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      source: PostgreSqlExplainSourceEvaluation,
      override val factorAwareCheckpoints
          : Vector[PostgreSqlFactorAwareCheckpoint] = Vector.empty,
      override val factorAwareSelectionLimits
          : Option[PostgreSqlFactorAwareSelectionLimits] = None,
      override val countStageCostCheckpoints
          : Vector[PostgreSqlCountStageCostCheckpoint] = Vector.empty,
      override val countStageCostSelectionLimits
          : Option[PostgreSqlFactorAwareSelectionLimits] = None
  ) extends PostgreSqlCandidateSelectionDecision {
    override val scope: String = scopeFor(policy)
    override val sourceEvaluation: Option[PostgreSqlExplainSourceEvaluation] =
      Some(source)
    require(
      policy == CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 ||
        policy ==
          CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ||
        policy ==
          CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1,
      "no-admissible decision must use an EXPLAIN policy"
    )
    require(reductionMode != null, "reduction mode must not be null")
    require(evaluations != null && evaluations.nonEmpty)
    require(evaluations.forall(_ != null))
    require(source != null, "source optimizer estimate must not be null")
    require(
      evaluations.forall(evaluation =>
        evaluation.reductionMode == reductionMode &&
          evaluation.reductionOrder == reductionOrder
      ),
      "candidate evaluations must use the request-wide reduction schedule"
    )
    require(
      evaluations.map(_.structuralRank) ==
        evaluations.map(_.structuralRank).sorted.distinct,
      "candidate evaluations must have unique increasing ranks"
    )
    requireEvidenceMatchesPolicy(policy, evaluations, sourceEvaluation)
    private val factorAware =
      policy ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
    private val countStageCost =
      policy ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
    require(
      factorAwareCheckpoints != null && factorAwareCheckpoints.forall(_ != null)
    )
    require(
      factorAwareCheckpoints.nonEmpty == factorAware &&
        (!factorAware ||
          factorAwareCheckpoints.map(_.requestedPrefix) ==
            PostgreSqlFactorAwareProgressiveV1.Checkpoints)
    )
    require(
      factorAwareSelectionLimits != null &&
        factorAwareSelectionLimits.nonEmpty == factorAware
    )
    require(
      countStageCostCheckpoints != null &&
        countStageCostCheckpoints.forall(_ != null) &&
        countStageCostCheckpoints.nonEmpty == countStageCost &&
        (!countStageCost ||
          countStageCostCheckpoints.map(_.requestedPrefix) ==
            PostgreSqlCountStageCostProgressiveV1.Checkpoints)
    )
    require(
      countStageCostSelectionLimits != null &&
        countStageCostSelectionLimits.nonEmpty == countStageCost
    )
    require(
      !factorAware || factorAwareCheckpoints.forall(_.winnerScore.isEmpty),
      "no-admissible factor checkpoints cannot contain winners"
    )
    require(
      !countStageCost || countStageCostCheckpoints.forall(_.winnerScore.isEmpty),
      "no-admissible COUNT stage-cost checkpoints cannot contain winners"
    )
    require(
      evaluations.forall(_.rowGuardEvidence.exists(!_.withinGuard)),
      "no-admissible decision requires every estimate to fail the row guard"
    )
  }
}
