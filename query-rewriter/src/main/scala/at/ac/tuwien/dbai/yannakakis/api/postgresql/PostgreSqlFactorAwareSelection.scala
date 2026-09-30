package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidatePlanId,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlStageDiagnosticLimits,
  PostgreSqlStageDiagnosticStatement,
  PostgreSqlStageId,
  PostgreSqlStageKind,
  PostgreSqlStagePhysicalForm,
  PostgreSqlStageTrace
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  YannakakisReductionMode
}

/** Frozen constants for factor-aware progressive selection v1. */
object PostgreSqlFactorAwareProgressiveV1 {
  val RequiredPlanLimit: Int = RewriteOptions.FactorAwareProgressivePlanLimit
  val Checkpoints: Vector[Int] = Vector(10, 25, 50, 100)
  val Scope: String =
    "retained_structural_top_k_first_canonical_cover_v1"
  val ScoreOrder: Vector[String] = Vector(
    "max_named_stage_plan_rows_times_plan_width",
    "sum_named_stage_plan_rows_times_plan_width",
    "final_plan_total_cost",
    "structural_rank",
    "candidate_id"
  )

  require(Checkpoints == Checkpoints.sorted.distinct)
  require(Checkpoints.last == RequiredPlanLimit)
}

/** Fixed arithmetic envelope applied only to factor-aware replay evidence. */
object PostgreSqlFactorAwareNumericEnvelope {
  val Scheme: String = "postgresql_factor_numeric_precision_scale_v1"
  val MaxInputPrecision: Int = 64
  val MaxAbsoluteScale: Int = 1024
  val MaxDerivedPrecision: Int = 128

  def accepts(value: BigDecimal): Boolean =
    value != null && value.precision <= MaxInputPrecision &&
      math.abs(value.scale.toLong) <= MaxAbsoluteScale.toLong

  def accepts(estimate: PostgreSqlExplainCostEstimate): Boolean =
    estimate != null && accepts(estimate.startupCost) &&
      accepts(estimate.planTotalCost) && accepts(estimate.planRows)

  private[postgresql] def acceptsDerived(value: BigDecimal): Boolean =
    value != null && value.precision <= MaxDerivedPrecision &&
      math.abs(value.scale.toLong) <= MaxAbsoluteScale.toLong
}

/** Frozen failure-code/stage compatibility for factor-aware v1. */
object PostgreSqlFactorAwareFailureMatrix {
  def accepts(
      code: PostgreSqlExplainCostFailureCode,
      stage: PostgreSqlExplainEvaluationStage
  ): Boolean =
    if (code == null || stage == null) false
    else
      stage match {
        case PostgreSqlExplainEvaluationStage.Session =>
          Set[PostgreSqlExplainCostFailureCode](
            PostgreSqlExplainCostFailureCode.EvaluatorUnavailable,
            PostgreSqlExplainCostFailureCode.ConnectionUnavailable,
            PostgreSqlExplainCostFailureCode.SessionConfigurationFailed,
            PostgreSqlExplainCostFailureCode.StatementTimedOut,
            PostgreSqlExplainCostFailureCode.CleanupFailed,
            PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
            PostgreSqlExplainCostFailureCode.SessionResourceLimitExceeded
          ).contains(code)
        case PostgreSqlExplainEvaluationStage.Provenance =>
          Set[PostgreSqlExplainCostFailureCode](
            PostgreSqlExplainCostFailureCode.ProvenanceUnavailable,
            PostgreSqlExplainCostFailureCode.ProvenanceChanged,
            PostgreSqlExplainCostFailureCode.StatementTimedOut
          ).contains(code)
        case PostgreSqlExplainEvaluationStage.SourceExplain |
            PostgreSqlExplainEvaluationStage.StageExplain =>
          Set[PostgreSqlExplainCostFailureCode](
            PostgreSqlExplainCostFailureCode.UnsafeRenderedStatement,
            PostgreSqlExplainCostFailureCode.StatementTimedOut,
            PostgreSqlExplainCostFailureCode.StatementFailed,
            PostgreSqlExplainCostFailureCode.PlanJsonOversized,
            PostgreSqlExplainCostFailureCode.PlanJsonMalformed
          ).contains(code)
        case PostgreSqlExplainEvaluationStage.PrefixVerification =>
          code == PostgreSqlExplainCostFailureCode.PrefixVerificationFailed
        case PostgreSqlExplainEvaluationStage.CandidatePlanning =>
          code == PostgreSqlExplainCostFailureCode.FactorCandidatePlanningFailed
        case PostgreSqlExplainEvaluationStage.StageDiagnosticRendering =>
          code ==
            PostgreSqlExplainCostFailureCode.FactorStageDiagnosticRenderingFailed
        case PostgreSqlExplainEvaluationStage.ResourceValidation =>
          code == PostgreSqlExplainCostFailureCode.SessionResourceLimitExceeded
        case PostgreSqlExplainEvaluationStage.CandidateExplain => false
      }
}

sealed trait PostgreSqlFactorAwareSelectionResource
    extends Product
    with Serializable {
  def code: String
}

object PostgreSqlFactorAwareSelectionResource {
  case object Candidates extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "candidates"
  }
  case object StageProbesPerCandidate
      extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "stage-probes-per-candidate"
  }
  case object CharactersPerProbe
      extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "characters-per-probe"
  }
  case object DiagnosticCharactersPerCandidate
      extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "diagnostic-characters-per-candidate"
  }
  case object ExplainStatementsPerSession
      extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "explain-statements-per-session"
  }
  case object DiagnosticCharactersPerSession
      extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "diagnostic-characters-per-session"
  }
  case object PlanJsonCharacters
      extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "plan-json-characters"
  }
  case object CumulativePlanJsonCharacters
      extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "cumulative-plan-json-characters"
  }
  case object StatementTimeoutSeconds
      extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "statement-timeout-seconds"
  }
  case object ExplainStatementBudgetSeconds
      extends PostgreSqlFactorAwareSelectionResource {
    override val code: String = "explain-statement-budget-seconds"
  }
}

sealed trait PostgreSqlFactorAwareSelectionLimitError
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object PostgreSqlFactorAwareSelectionLimitError {
  final case class InvalidResourceLimit(
      resource: PostgreSqlFactorAwareSelectionResource,
      value: Long
  ) extends PostgreSqlFactorAwareSelectionLimitError {
    override val code: String = s"factor-aware-limit.invalid.${resource.code}"
    override val message: String =
      s"Factor-aware selection limit ${resource.code} must be positive, received $value"
  }

  final case class InconsistentResourceLimits(
      smaller: PostgreSqlFactorAwareSelectionResource,
      larger: PostgreSqlFactorAwareSelectionResource
  ) extends PostgreSqlFactorAwareSelectionLimitError {
    override val code: String =
      s"factor-aware-limit.inconsistent.${smaller.code}.${larger.code}"
    override val message: String =
      s"Factor-aware limit ${larger.code} must cover ${smaller.code}"
  }

  final case class IntegerMaterializationLimit(
      resource: PostgreSqlFactorAwareSelectionResource,
      value: Long
  ) extends PostgreSqlFactorAwareSelectionLimitError {
    override val code: String =
      s"factor-aware-limit.integer-materialization.${resource.code}"
    override val message: String =
      s"Factor-aware limit ${resource.code} must not exceed ${Int.MaxValue}, received $value"
  }

  final case class FrozenContractLimit(
      resource: PostgreSqlFactorAwareSelectionResource,
      value: Long,
      required: Long
  ) extends PostgreSqlFactorAwareSelectionLimitError {
    override val code: String =
      s"factor-aware-limit.frozen-contract.${resource.code}"
    override val message: String =
      s"Factor-aware v1 requires ${resource.code}=$required, received $value"
  }

  final case class MaximumResourceLimit(
      resource: PostgreSqlFactorAwareSelectionResource,
      value: Long,
      maximum: Long
  ) extends PostgreSqlFactorAwareSelectionLimitError {
    override val code: String =
      s"factor-aware-limit.maximum.${resource.code}"
    override val message: String =
      s"Factor-aware limit ${resource.code} must not exceed $maximum, received $value"
  }
}

/** Explicit bounded envelope for the opt-in factor-aware selector. */
final class PostgreSqlFactorAwareSelectionLimits private (
    val maxCandidates: Long,
    val maxStageProbesPerCandidate: Long,
    val maxCharactersPerProbe: Long,
    val maxDiagnosticCharactersPerCandidate: Long,
    val maxExplainStatementsPerSession: Long,
    val maxDiagnosticCharactersPerSession: Long,
    val maxPlanJsonCharacters: Long,
    val maxCumulativePlanJsonCharacters: Long,
    val statementTimeoutSeconds: Int,
    val explainStatementBudgetSeconds: Int
) extends Serializable {
  private[postgresql] def stageDiagnosticLimits
      : PostgreSqlStageDiagnosticLimits =
    PostgreSqlStageDiagnosticLimits
      .create(
        maxStageProbesPerCandidate,
        maxCharactersPerProbe,
        maxDiagnosticCharactersPerCandidate
      )
      .fold(
        errors =>
          throw new IllegalStateException(
            s"validated factor-aware limits produced invalid diagnostic limits: ${errors.map(_.code).mkString(",")}"
          ),
        identity
      )

  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlFactorAwareSelectionLimits =>
      maxCandidates == that.maxCandidates &&
        maxStageProbesPerCandidate == that.maxStageProbesPerCandidate &&
        maxCharactersPerProbe == that.maxCharactersPerProbe &&
        maxDiagnosticCharactersPerCandidate ==
          that.maxDiagnosticCharactersPerCandidate &&
        maxExplainStatementsPerSession == that.maxExplainStatementsPerSession &&
        maxDiagnosticCharactersPerSession ==
          that.maxDiagnosticCharactersPerSession &&
        maxPlanJsonCharacters == that.maxPlanJsonCharacters &&
        maxCumulativePlanJsonCharacters ==
          that.maxCumulativePlanJsonCharacters &&
        statementTimeoutSeconds == that.statementTimeoutSeconds &&
        explainStatementBudgetSeconds == that.explainStatementBudgetSeconds
    case _ => false
  }

  override def hashCode(): Int =
    (
      maxCandidates,
      maxStageProbesPerCandidate,
      maxCharactersPerProbe,
      maxDiagnosticCharactersPerCandidate,
      maxExplainStatementsPerSession,
      maxDiagnosticCharactersPerSession,
      maxPlanJsonCharacters,
      maxCumulativePlanJsonCharacters,
      statementTimeoutSeconds,
      explainStatementBudgetSeconds
    ).hashCode()
}

object PostgreSqlFactorAwareSelectionLimits {
  private val Mi: Long = 1024L * 1024L

  val default: PostgreSqlFactorAwareSelectionLimits =
    new PostgreSqlFactorAwareSelectionLimits(
      maxCandidates = 100L,
      maxStageProbesPerCandidate = 256L,
      maxCharactersPerProbe = 16L * Mi,
      maxDiagnosticCharactersPerCandidate = 64L * Mi,
      maxExplainStatementsPerSession = 4096L,
      maxDiagnosticCharactersPerSession = 256L * Mi,
      maxPlanJsonCharacters = 1L * Mi,
      maxCumulativePlanJsonCharacters = 128L * Mi,
      statementTimeoutSeconds = 30,
      explainStatementBudgetSeconds = 600
    )

  def create(
      maxCandidates: Long = default.maxCandidates,
      maxStageProbesPerCandidate: Long = default.maxStageProbesPerCandidate,
      maxCharactersPerProbe: Long = default.maxCharactersPerProbe,
      maxDiagnosticCharactersPerCandidate: Long =
        default.maxDiagnosticCharactersPerCandidate,
      maxExplainStatementsPerSession: Long =
        default.maxExplainStatementsPerSession,
      maxDiagnosticCharactersPerSession: Long =
        default.maxDiagnosticCharactersPerSession,
      maxPlanJsonCharacters: Long = default.maxPlanJsonCharacters,
      maxCumulativePlanJsonCharacters: Long =
        default.maxCumulativePlanJsonCharacters,
      statementTimeoutSeconds: Int = default.statementTimeoutSeconds,
      explainStatementBudgetSeconds: Int =
        default.explainStatementBudgetSeconds
  ): Either[
    Vector[PostgreSqlFactorAwareSelectionLimitError],
    PostgreSqlFactorAwareSelectionLimits
  ] = {
    import PostgreSqlFactorAwareSelectionLimitError._
    import PostgreSqlFactorAwareSelectionResource._

    val supplied = Vector(
      Candidates -> maxCandidates,
      StageProbesPerCandidate -> maxStageProbesPerCandidate,
      CharactersPerProbe -> maxCharactersPerProbe,
      DiagnosticCharactersPerCandidate -> maxDiagnosticCharactersPerCandidate,
      ExplainStatementsPerSession -> maxExplainStatementsPerSession,
      DiagnosticCharactersPerSession -> maxDiagnosticCharactersPerSession,
      PlanJsonCharacters -> maxPlanJsonCharacters,
      CumulativePlanJsonCharacters -> maxCumulativePlanJsonCharacters,
      StatementTimeoutSeconds -> statementTimeoutSeconds.toLong,
      ExplainStatementBudgetSeconds -> explainStatementBudgetSeconds.toLong
    )
    val invalid = supplied.collect {
      case (resource, value) if value <= 0L =>
        InvalidResourceLimit(resource, value)
    }
    val integerLimits = Vector(
      CharactersPerProbe -> maxCharactersPerProbe,
      PlanJsonCharacters -> maxPlanJsonCharacters,
      StatementTimeoutSeconds -> statementTimeoutSeconds.toLong,
      ExplainStatementBudgetSeconds -> explainStatementBudgetSeconds.toLong
    ).collect {
      case (resource, value) if value > Int.MaxValue.toLong =>
        IntegerMaterializationLimit(resource, value)
    }
    val inconsistent = Vector.newBuilder[PostgreSqlFactorAwareSelectionLimitError]
    val frozen =
      if (
        maxCandidates > 0L &&
        maxCandidates != PostgreSqlFactorAwareProgressiveV1.RequiredPlanLimit.toLong
      )
        Vector(
          FrozenContractLimit(
            Candidates,
            maxCandidates,
            PostgreSqlFactorAwareProgressiveV1.RequiredPlanLimit.toLong
          )
        )
      else Vector.empty
    val frozenMaximum = Vector(
      StageProbesPerCandidate ->
        (maxStageProbesPerCandidate -> default.maxStageProbesPerCandidate),
      CharactersPerProbe ->
        (maxCharactersPerProbe -> default.maxCharactersPerProbe),
      DiagnosticCharactersPerCandidate ->
        (maxDiagnosticCharactersPerCandidate ->
          default.maxDiagnosticCharactersPerCandidate),
      ExplainStatementsPerSession ->
        (maxExplainStatementsPerSession ->
          default.maxExplainStatementsPerSession),
      DiagnosticCharactersPerSession ->
        (maxDiagnosticCharactersPerSession ->
          default.maxDiagnosticCharactersPerSession),
      PlanJsonCharacters ->
        (maxPlanJsonCharacters -> default.maxPlanJsonCharacters),
      CumulativePlanJsonCharacters ->
        (maxCumulativePlanJsonCharacters ->
          default.maxCumulativePlanJsonCharacters),
      StatementTimeoutSeconds ->
        (statementTimeoutSeconds.toLong ->
          default.statementTimeoutSeconds.toLong),
      ExplainStatementBudgetSeconds ->
        (explainStatementBudgetSeconds.toLong ->
          default.explainStatementBudgetSeconds.toLong)
    ).collect {
      case (resource, (value, maximum)) if value > maximum =>
        MaximumResourceLimit(resource, value, maximum)
    }
    if (
      maxCharactersPerProbe > 0L &&
      maxDiagnosticCharactersPerCandidate > 0L &&
      maxCharactersPerProbe > maxDiagnosticCharactersPerCandidate
    ) inconsistent += InconsistentResourceLimits(
      CharactersPerProbe,
      DiagnosticCharactersPerCandidate
    )
    if (
      maxDiagnosticCharactersPerCandidate > 0L &&
      maxDiagnosticCharactersPerSession > 0L &&
      maxDiagnosticCharactersPerCandidate > maxDiagnosticCharactersPerSession
    ) inconsistent += InconsistentResourceLimits(
      DiagnosticCharactersPerCandidate,
      DiagnosticCharactersPerSession
    )
    if (
      maxPlanJsonCharacters > 0L &&
      maxCumulativePlanJsonCharacters > 0L &&
      maxPlanJsonCharacters > maxCumulativePlanJsonCharacters
    ) inconsistent += InconsistentResourceLimits(
      PlanJsonCharacters,
      CumulativePlanJsonCharacters
    )
    if (
      statementTimeoutSeconds > 0 && explainStatementBudgetSeconds > 0 &&
      statementTimeoutSeconds > explainStatementBudgetSeconds
    ) inconsistent += InconsistentResourceLimits(
      StatementTimeoutSeconds,
      ExplainStatementBudgetSeconds
    )
    if (maxExplainStatementsPerSession == 1L)
      inconsistent += InconsistentResourceLimits(
        StageProbesPerCandidate,
        ExplainStatementsPerSession
      )

    val errors =
      invalid ++ frozen ++ frozenMaximum ++ integerLimits ++ inconsistent.result()
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new PostgreSqlFactorAwareSelectionLimits(
          maxCandidates,
          maxStageProbesPerCandidate,
          maxCharactersPerProbe,
          maxDiagnosticCharactersPerCandidate,
          maxExplainStatementsPerSession,
          maxDiagnosticCharactersPerSession,
          maxPlanJsonCharacters,
          maxCumulativePlanJsonCharacters,
          statementTimeoutSeconds,
          explainStatementBudgetSeconds
        )
      )
  }
}

/** Exact lexicographic factor score. All arithmetic is decimal and unrounded. */
final case class PostgreSqlFactorAwareScore(
    maxNamedStagePlanRowsTimesPlanWidth: BigDecimal,
    sumNamedStagePlanRowsTimesPlanWidth: BigDecimal,
    finalPlanTotalCost: BigDecimal,
    structuralRank: Int,
    candidateId: CandidatePlanId
) extends Serializable {
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
      maxNamedStagePlanRowsTimesPlanWidth
    ) && PostgreSqlFactorAwareNumericEnvelope.acceptsDerived(
      sumNamedStagePlanRowsTimesPlanWidth
    ) && PostgreSqlFactorAwareNumericEnvelope.accepts(finalPlanTotalCost),
    "factor-aware score exceeds the frozen numeric envelope"
  )
}

object PostgreSqlFactorAwareScore {
  private[postgresql] def exactEstimatedBytes(
      rows: BigDecimal,
      width: Long
  ): BigDecimal = {
    require(rows != null && rows.signum >= 0)
    require(width >= 0L)
    require(
      PostgreSqlFactorAwareNumericEnvelope.accepts(rows),
      "plan rows exceed the factor numeric envelope"
    )
    val value = BigDecimal(
      rows.bigDecimal.multiply(java.math.BigDecimal.valueOf(width))
    )
    require(
      PostgreSqlFactorAwareNumericEnvelope.acceptsDerived(value),
      "estimated byte product exceeds the factor numeric envelope"
    )
    value
  }

  private[postgresql] def exactSum(values: Vector[BigDecimal]): BigDecimal = {
    require(values != null && values.size <= 256)
    require(values.forall(PostgreSqlFactorAwareNumericEnvelope.acceptsDerived))
    val value = BigDecimal(
      values.foldLeft(java.math.BigDecimal.ZERO) { (total, value) =>
        total.add(value.bigDecimal)
      }
    )
    require(PostgreSqlFactorAwareNumericEnvelope.acceptsDerived(value))
    value
  }

  implicit val ordering: Ordering[PostgreSqlFactorAwareScore] =
    new Ordering[PostgreSqlFactorAwareScore] {
      override def compare(
          left: PostgreSqlFactorAwareScore,
          right: PostgreSqlFactorAwareScore
      ): Int = {
        val peak = left.maxNamedStagePlanRowsTimesPlanWidth.compare(
          right.maxNamedStagePlanRowsTimesPlanWidth
        )
        if (peak != 0) return peak
        val sum = left.sumNamedStagePlanRowsTimesPlanWidth.compare(
          right.sumNamedStagePlanRowsTimesPlanWidth
        )
        if (sum != 0) return sum
        val cost = left.finalPlanTotalCost.compare(right.finalPlanTotalCost)
        if (cost != 0) return cost
        val rank = Integer.compare(left.structuralRank, right.structuralRank)
        if (rank != 0) return rank
        left.candidateId.value.compareTo(right.candidateId.value)
      }
    }
}

/** Trace-bound evidence for an emitted stage or an explicitly elided support. */
final case class PostgreSqlFactorAwareStageEvaluation private[postgresql] (
    trace: PostgreSqlStageTrace,
    diagnosticStatementSha256: Option[String],
    diagnosticCharacterCount: Option[Long],
    diagnosticCtePrefixCount: Option[Long],
    estimate: Option[PostgreSqlExplainCostEstimate]
) extends Serializable {
  require(trace != null, "stage trace must not be null")
  require(diagnosticStatementSha256 != null)
  require(diagnosticCharacterCount != null)
  require(diagnosticCtePrefixCount != null)
  require(estimate != null)
  require(
    Vector(
      diagnosticStatementSha256.nonEmpty,
      diagnosticCharacterCount.nonEmpty,
      diagnosticCtePrefixCount.nonEmpty,
      estimate.nonEmpty
    ).distinct.size == 1,
    "stage diagnostic identity and estimate must be wholly present or absent"
  )
  require(
    trace.physicalForm.emitted == estimate.nonEmpty,
    "only emitted stages may have optimizer evidence"
  )
  require(
    diagnosticStatementSha256.forall(_.matches("[0-9a-f]{64}")),
    "diagnostic statement digest must be lowercase SHA-256"
  )
  require(diagnosticCharacterCount.forall(_ >= 0L))
  require(diagnosticCtePrefixCount.forall(_ >= 0L))
  require(
    estimate.forall(PostgreSqlFactorAwareNumericEnvelope.accepts),
    "stage estimate exceeds the frozen factor numeric envelope"
  )
}

/** Complete factor-aware optimizer evidence for one retained candidate. */
final case class PostgreSqlFactorAwareCandidateEvidence private[postgresql] (
    physicalVariantId: PostgreSqlPhysicalVariantId,
    stages: Vector[PostgreSqlFactorAwareStageEvaluation],
    score: PostgreSqlFactorAwareScore,
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
    stageIdScheme == PostgreSqlFactorAwareCandidateEvidence.StageIdScheme &&
      stageOrderScheme == PostgreSqlFactorAwareCandidateEvidence.StageOrderScheme &&
      statementHashScheme ==
        PostgreSqlFactorAwareCandidateEvidence.StatementHashScheme &&
      characterCountScheme ==
        PostgreSqlFactorAwareCandidateEvidence.CharacterCountScheme,
    "factor-aware evidence schemes must match the frozen renderer contract"
  )
  require(
    stages.last.trace.id == PostgreSqlStageId.FinalReconstruction &&
      stages.last.trace.kind == PostgreSqlStageKind.FinalReconstruction &&
      stages.last.trace.physicalForm == PostgreSqlStagePhysicalForm.TerminalSelect &&
      stages.last.estimate.nonEmpty,
    "factor-aware evidence must end in the estimated reconstruction"
  )

  private val namedValues = stages.collect {
    case stage if stage.trace.physicalForm.namedCte =>
      val estimate = stage.estimate.get
      PostgreSqlFactorAwareScore.exactEstimatedBytes(
        estimate.planRows,
        estimate.planWidth
      )
  }
  private val expectedMaximum =
    namedValues.foldLeft(BigDecimal(0))(_.max(_))
  private val expectedSum = PostgreSqlFactorAwareScore.exactSum(namedValues)
  require(
    score.maxNamedStagePlanRowsTimesPlanWidth == expectedMaximum &&
      score.sumNamedStagePlanRowsTimesPlanWidth == expectedSum &&
      score.finalPlanTotalCost == stages.last.estimate.get.planTotalCost,
    "factor-aware score must be exactly recomputable from stage evidence"
  )
}

object PostgreSqlFactorAwareCandidateEvidence {
  val StageIdScheme: String = PostgreSqlStageId.Scheme
  val StageOrderScheme: String =
    at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageTrace.OrderScheme
  val StatementHashScheme: String = PostgreSqlStageDiagnosticStatement.HashScheme
  val CharacterCountScheme: String =
    PostgreSqlStageDiagnosticStatement.CharacterCountScheme
}

/** Winner (or absence of an admissible winner) at one verified prefix. */
final case class PostgreSqlFactorAwareCheckpoint(
    requestedPrefix: Int,
    effectiveCandidateCount: Int,
    truncatedBySearchExhaustion: Boolean,
    winnerStructuralRank: Option[Int],
    winnerCandidateId: Option[CandidatePlanId],
    winnerPhysicalVariantId: Option[PostgreSqlPhysicalVariantId],
    winnerScore: Option[PostgreSqlFactorAwareScore],
    verifiedPrefixSha256: String,
    prefixProofScheme: String
) extends Serializable {
  require(winnerStructuralRank != null)
  require(winnerCandidateId != null && winnerCandidateId.forall(_ != null))
  require(
    winnerPhysicalVariantId != null &&
      winnerPhysicalVariantId.forall(_ != null)
  )
  require(winnerScore != null && winnerScore.forall(_ != null))
  require(PostgreSqlFactorAwareProgressiveV1.Checkpoints.contains(requestedPrefix))
  require(effectiveCandidateCount > 0 && effectiveCandidateCount <= requestedPrefix)
  require(truncatedBySearchExhaustion == (effectiveCandidateCount < requestedPrefix))
  require(verifiedPrefixSha256 != null && verifiedPrefixSha256.matches("[0-9a-f]{64}"))
  require(
    prefixProofScheme == PostgreSqlFactorAwarePrefixProof.Scheme,
    "checkpoint prefix proof must use the frozen v1 scheme"
  )
  require(
    Vector(
      winnerStructuralRank.nonEmpty,
      winnerCandidateId.nonEmpty,
      winnerPhysicalVariantId.nonEmpty,
      winnerScore.nonEmpty
    ).distinct.size == 1,
    "checkpoint winner identity and score must be wholly present or absent"
  )
  require(winnerStructuralRank.forall(rank => rank > 0 && rank <= effectiveCandidateCount))
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

/** SQL-free hash identity retained before the optimizer batch starts. */
final case class PostgreSqlFactorAwareRenderedStageIdentity private[postgresql] (
    stageId: PostgreSqlStageId,
    statementSha256: String,
    characterCount: Long,
    ctePrefixCount: Long
) extends Serializable {
  require(stageId != null)
  require(statementSha256 != null && statementSha256.matches("[0-9a-f]{64}"))
  require(characterCount >= 0L)
  require(ctePrefixCount >= 0L)
}

final case class PostgreSqlFactorAwareRenderedCandidateIdentity private[postgresql] (
    candidateId: CandidatePlanId,
    structuralRank: Int,
    physicalVariantId: PostgreSqlPhysicalVariantId,
    renderedStatementSha256: String,
    stages: Vector[PostgreSqlFactorAwareRenderedStageIdentity]
) extends Serializable {
  require(candidateId != null)
  require(structuralRank > 0)
  require(physicalVariantId != null)
  require(renderedStatementSha256 != null && renderedStatementSha256.matches("[0-9a-f]{64}"))
  require(stages != null && stages.nonEmpty && stages.forall(_ != null))
  require(
    stages.size <=
      PostgreSqlFactorAwareSelectionLimits.default.maxStageProbesPerCandidate
  )
  require(stages.map(_.stageId).distinct.size == stages.size)
  require(stages.last.stageId == PostgreSqlStageId.FinalReconstruction)
  require(
    renderedStatementSha256 == stages.last.statementSha256,
    "terminal stage digest must equal the rendered statement digest"
  )
}

/** One emitted stage statement in a single-session factor EXPLAIN request. */
final case class PostgreSqlFactorAwareExplainStageStatement private[postgresql] (
    stageId: PostgreSqlStageId,
    statementSha256: String,
    sql: String
) {
  require(stageId != null)
  require(statementSha256 != null && statementSha256.matches("[0-9a-f]{64}"))
  require(sql != null && sql.nonEmpty)
  require(
    statementSha256 ==
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql),
    "factor stage digest must match its SQL"
  )
}

final case class PostgreSqlFactorAwareExplainCandidate private[postgresql] (
    candidateId: CandidatePlanId,
    structuralRank: Int,
    reductionMode: YannakakisReductionMode,
    physicalVariantId: PostgreSqlPhysicalVariantId,
    stages: Vector[PostgreSqlFactorAwareExplainStageStatement]
) {
  require(candidateId != null)
  require(structuralRank > 0)
  require(reductionMode != null)
  require(
    physicalVariantId == PostgreSqlPhysicalVariantId.from(candidateId, reductionMode)
  )
  require(stages != null && stages.nonEmpty && stages.forall(_ != null))
  require(
    stages.size <=
      PostgreSqlFactorAwareSelectionLimits.default.maxStageProbesPerCandidate
  )
  require(stages.map(_.stageId).distinct.size == stages.size)
  require(stages.last.stageId == PostgreSqlStageId.FinalReconstruction)
}

final case class PostgreSqlFactorAwareExplainBatchRequest private[postgresql] (
    source: PostgreSqlExplainSourceStatement,
    candidates: Vector[PostgreSqlFactorAwareExplainCandidate]
) {
  require(source != null)
  require(candidates != null && candidates.nonEmpty && candidates.forall(_ != null))
  require(candidates.map(_.structuralRank) == (1 to candidates.size).toVector)
  require(candidates.map(_.candidateId).distinct.size == candidates.size)
  require(candidates.map(_.physicalVariantId).distinct.size == candidates.size)
  require(candidates.map(_.reductionMode).distinct.size == 1)

  private[postgresql] val flattened
      : Vector[(PostgreSqlFactorAwareExplainCandidate, PostgreSqlFactorAwareExplainStageStatement)] =
    candidates.flatMap(candidate => candidate.stages.map(candidate -> _))
}

final case class PostgreSqlFactorAwareExplainObservation(
    physicalVariantId: PostgreSqlPhysicalVariantId,
    stageId: PostgreSqlStageId,
    statementSha256: String,
    estimate: PostgreSqlExplainCostEstimate
) {
  require(physicalVariantId != null)
  require(stageId != null)
  require(statementSha256 != null && statementSha256.matches("[0-9a-f]{64}"))
  require(estimate != null)
  require(
    PostgreSqlFactorAwareNumericEnvelope.accepts(estimate),
    "factor observation exceeds the frozen numeric envelope"
  )
}

final case class PostgreSqlFactorAwareExplainBatch(
    provenance: PostgreSqlExplainCostProvenance,
    source: PostgreSqlExplainSourceEvaluation,
    observations: Vector[PostgreSqlFactorAwareExplainObservation]
) {
  require(provenance != null)
  require(source != null && source.estimate.provenance == provenance)
  require(PostgreSqlFactorAwareNumericEnvelope.accepts(source.estimate))
  require(observations != null && observations.forall(_ != null))
  require(observations.forall(_.estimate.provenance == provenance))
}

final case class PostgreSqlFactorAwareExplainFailure(
    code: PostgreSqlExplainCostFailureCode,
    stage: PostgreSqlExplainEvaluationStage,
    failedStatementIndex: Option[Int],
    source: Option[PostgreSqlExplainSourceEvaluation],
    completed: Vector[PostgreSqlFactorAwareExplainObservation]
) {
  require(code != null)
  require(stage != null)
  require(
    PostgreSqlFactorAwareFailureMatrix.accepts(code, stage),
    "factor failure code must match its evaluation stage"
  )
  require(failedStatementIndex != null && failedStatementIndex.forall(_ >= 0))
  require(source != null && source.forall(_ != null))
  require(completed != null && completed.forall(_ != null))
  require(
    (stage == PostgreSqlExplainEvaluationStage.StageExplain) ==
      failedStatementIndex.nonEmpty,
    "only stage EXPLAIN failures identify a failed statement"
  )
  require(failedStatementIndex.forall(_ == completed.size))
  require(failedStatementIndex.isEmpty || source.nonEmpty)
  require(source.nonEmpty || completed.isEmpty)
  require(
    source.forall(value =>
      PostgreSqlFactorAwareNumericEnvelope.accepts(value.estimate)
    )
  )
  require(
    stage != PostgreSqlExplainEvaluationStage.SourceExplain ||
      (failedStatementIndex.isEmpty && source.isEmpty && completed.isEmpty),
    "source EXPLAIN failure cannot contain completed evidence"
  )
  require(
    stage != PostgreSqlExplainEvaluationStage.StageExplain ||
      (failedStatementIndex.contains(completed.size) && source.nonEmpty),
    "stage EXPLAIN failure must identify the exact completed prefix"
  )
  require(
    stage != PostgreSqlExplainEvaluationStage.PrefixVerification,
    "prefix verification is outside the evaluator boundary"
  )
}

private[postgresql] final case class PostgreSqlFactorAwareVerifiedBatch(
    source: PostgreSqlExplainSourceEvaluation,
    observationsByCandidate: Vector[Vector[PostgreSqlFactorAwareExplainObservation]]
)

/** Pure, side-effect-free verifier for injected or persisted evaluator output. */
object PostgreSqlFactorAwareReplay {
  private[postgresql] def verify(
      request: PostgreSqlFactorAwareExplainBatchRequest,
      batch: PostgreSqlFactorAwareExplainBatch
  ): Option[PostgreSqlFactorAwareVerifiedBatch] = {
    if (
      request == null || batch == null || batch.provenance == null ||
      batch.source == null || batch.source.estimate == null ||
      !PostgreSqlFactorAwareNumericEnvelope.accepts(batch.source.estimate) ||
      batch.source.sourceStatementSha256 != request.source.sourceStatementSha256 ||
      batch.source.estimate.provenance != batch.provenance ||
      batch.observations == null ||
      batch.observations.size != request.flattened.size
    ) return None

    val verified = request.flattened.zip(batch.observations).forall {
      case ((candidate, statement), observation) =>
        observation != null && observation.estimate != null &&
          PostgreSqlFactorAwareNumericEnvelope.accepts(observation.estimate) &&
          observation.physicalVariantId == candidate.physicalVariantId &&
          observation.stageId == statement.stageId &&
          observation.statementSha256 == statement.statementSha256 &&
          observation.estimate.provenance == batch.provenance
    }
    if (!verified) None
    else {
      var offset = 0
      val grouped = request.candidates.map { candidate =>
        val values = batch.observations.slice(offset, offset + candidate.stages.size)
        offset += candidate.stages.size
        values
      }
      Some(PostgreSqlFactorAwareVerifiedBatch(batch.source, grouped))
    }
  }

  private[postgresql] def verifyFailurePrefix(
      request: PostgreSqlFactorAwareExplainBatchRequest,
      failure: PostgreSqlFactorAwareExplainFailure
  ): Boolean = {
    if (
      request == null || failure == null || failure.code == null ||
      failure.stage == null ||
      !PostgreSqlFactorAwareFailureMatrix.accepts(
        failure.code,
        failure.stage
      ) || failure.completed == null ||
      failure.failedStatementIndex.exists(index =>
        index < 0 || index >= request.flattened.size
      ) ||
      failure.source.exists(source =>
        source == null || source.estimate == null ||
          !PostgreSqlFactorAwareNumericEnvelope.accepts(source.estimate) ||
          source.sourceStatementSha256 != request.source.sourceStatementSha256
      ) || failure.completed.exists(observation =>
        observation == null || observation.estimate == null ||
          !PostgreSqlFactorAwareNumericEnvelope.accepts(observation.estimate)
      )
    ) return false

    val stageShapeValid = failure.stage match {
      case PostgreSqlExplainEvaluationStage.StageExplain =>
        failure.failedStatementIndex.contains(failure.completed.size) &&
          failure.completed.size < request.flattened.size &&
          failure.source.nonEmpty
      case PostgreSqlExplainEvaluationStage.SourceExplain =>
        failure.failedStatementIndex.isEmpty && failure.source.isEmpty &&
          failure.completed.isEmpty
      case PostgreSqlExplainEvaluationStage.Provenance =>
        failure.failedStatementIndex.isEmpty &&
          ((failure.source.isEmpty && failure.completed.isEmpty) ||
            (failure.source.nonEmpty &&
              failure.completed.size == request.flattened.size))
      case PostgreSqlExplainEvaluationStage.Session =>
        failure.failedStatementIndex.isEmpty &&
          (failure.code == PostgreSqlExplainCostFailureCode.CleanupFailed ||
            (failure.source.isEmpty && failure.completed.isEmpty))
      case _ => false
    }
    val provenance = failure.source.map(_.estimate.provenance)
    stageShapeValid && failure.completed.size <= request.flattened.size &&
    failure.completed.zip(request.flattened).forall {
      case (observation, (candidate, statement)) =>
        observation != null && observation.estimate != null &&
          provenance.contains(observation.estimate.provenance) &&
          observation.physicalVariantId == candidate.physicalVariantId &&
          observation.stageId == statement.stageId &&
          observation.statementSha256 == statement.statementSha256
    }
  }
}

private[postgresql] final case class PostgreSqlFactorAwareVerifiedPrefix(
    requestedPrefix: Int,
    effectiveCandidateCount: Int,
    truncatedBySearchExhaustion: Boolean,
    verifiedPrefixSha256: String
)

object PostgreSqlFactorAwarePrefixProof {
  val Scheme: String =
    "postgresql_factor_prefix_rank_id_score_decomposition_certificate_length_prefixed_v1"

  private[postgresql] def digest(plans: Vector[RankedCandidatePlan]): String = {
    require(plans != null && plans.forall(_ != null))
    val digest = MessageDigest.getInstance("SHA-256")

    def append(value: String): Unit = {
      val text = if (value == null) "" else value
      val bytes = text.getBytes(StandardCharsets.UTF_8)
      digest.update(bytes.length.toString.getBytes(StandardCharsets.UTF_8))
      digest.update(':'.toByte)
      digest.update(bytes)
    }

    append(Scheme)
    append(CandidatePlanId.Scheme)
    append(plans.size.toString)
    plans.foreach { plan =>
      append(plan.rank.toString)
      append(plan.candidateId.value)
      append(plan.score.nodeCount.toString)
      append(plan.decomposition.root.value.toString)
      append(plan.decomposition.width.toString)
      val bags = plan.decomposition.bags.sortBy(_.id.value)
      append(bags.size.toString)
      bags.foreach { bag =>
        append(bag.id.value.toString)
        append(bag.vertices.size.toString)
        bag.vertices.sortBy(_.value).foreach(value => append(value.value.toString))
        append(bag.cover.size.toString)
        bag.cover.sortBy(_.value).foreach(value => append(value.value.toString))
      }
      val treeEdges = plan.decomposition.treeEdges.sortBy(edge =>
        (edge.first.value, edge.second.value)
      )
      append(treeEdges.size.toString)
      treeEdges.foreach { edge =>
        append(edge.first.value.toString)
        append(edge.second.value.toString)
      }
      append(plan.certificate.bagCount.toString)
      append(plan.certificate.requireConnectedCover.toString)
      val arcs = plan.certificate.componentArcs.sortBy(arc =>
        (arc.parent.value, arc.child.value)
      )
      append(arcs.size.toString)
      arcs.foreach { arc =>
        append(arc.parent.value.toString)
        append(arc.child.value.toString)
        append(arc.component.size.toString)
        arc.component.foreach(variable => append(variable.value.toString))
      }
    }
    val bytes = digest.digest()
    val alphabet = "0123456789abcdef"
    val result = new java.lang.StringBuilder(bytes.length * 2)
    bytes.foreach { byte =>
      val unsigned = byte & 0xff
      result.append(alphabet.charAt(unsigned >>> 4))
      result.append(alphabet.charAt(unsigned & 0x0f))
    }
    result.toString
  }
}

/** Pure verifier for independently enumerated K=10/25/50/100 results. */
private[postgresql] object PostgreSqlFactorAwarePrefixVerifier {
  private def validPlanShape(plan: RankedCandidatePlan): Boolean =
    try {
      plan != null && plan.candidateId != null && plan.score != null &&
      plan.score.nodeCount != null && plan.decomposition != null &&
      plan.decomposition.root != null && plan.decomposition.bags != null &&
      plan.decomposition.treeEdges != null &&
      plan.decomposition.bags.forall(bag =>
        bag != null && bag.id != null && bag.vertices != null &&
          bag.vertices.forall(_ != null) && bag.cover != null &&
          bag.cover.forall(_ != null)
      ) &&
      plan.decomposition.treeEdges.forall(edge =>
        edge != null && edge.first != null && edge.second != null
      ) &&
      plan.certificate != null && plan.certificate.componentArcs != null &&
      plan.certificate.componentArcs.forall(arc =>
        arc != null && arc.parent != null && arc.child != null &&
          arc.component != null && arc.component.forall(_ != null)
      ) &&
      plan.candidateId == CandidatePlanId.from(plan.decomposition)
    } catch {
      case scala.util.control.NonFatal(_) => false
    }

  def verify(
      full: CandidatePlanDecision.Found,
      reruns: Vector[(Int, CandidatePlanDecision)]
  ): Option[Vector[PostgreSqlFactorAwareVerifiedPrefix]] = {
    if (
      full == null || full.plans == null || full.plans.isEmpty ||
      full.plans.exists(plan => !validPlanShape(plan)) ||
      full.requestedLimit != PostgreSqlFactorAwareProgressiveV1.RequiredPlanLimit ||
      full.plans.size > PostgreSqlFactorAwareProgressiveV1.RequiredPlanLimit ||
      reruns == null || reruns.exists(_ == null) || reruns.map(_._1) !=
        PostgreSqlFactorAwareProgressiveV1.Checkpoints
    ) return None

    val fullIdentities = full.plans.map(plan => plan.rank -> plan.candidateId)
    if (
      fullIdentities.map(_._1) != (1 to full.plans.size).toVector ||
      fullIdentities.map(_._2).distinct.size != fullIdentities.size
    ) return None

    val verified = reruns.map {
      case (requested, found: CandidatePlanDecision.Found)
          if found != null && found.plans != null &&
            !found.plans.exists(plan => !validPlanShape(plan)) &&
            found.requestedLimit == requested =>
        val expectedSize = math.min(requested, full.plans.size)
        val expectedPlans = full.plans.take(expectedSize)
        val exact = found.plans.zip(expectedPlans).forall {
          case (observed, expected) =>
            observed.rank == expected.rank &&
              observed.candidateId == expected.candidateId &&
              observed.score == expected.score &&
              observed.decomposition == expected.decomposition &&
              observed.certificate == expected.certificate
        }
        if (found.plans.size == expectedSize && exact)
          Some(
            PostgreSqlFactorAwareVerifiedPrefix(
              requested,
              expectedSize,
              expectedSize < requested,
              PostgreSqlFactorAwarePrefixProof.digest(expectedPlans)
            )
          )
        else None
      case _ => None
    }
    if (verified.forall(_.nonEmpty)) Some(verified.flatten) else None
  }
}

/** Bounded resource replay for SQL-free renderer diagnostic evidence. */
private[postgresql] object PostgreSqlFactorAwareDiagnosticReplay {
  final case class Usage(statementCount: BigInt, characterCount: BigInt)

  def evidenceUsage(
      evidence: PostgreSqlFactorAwareCandidateEvidence,
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Option[Usage] =
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
              stage.diagnosticStatementSha256 == null ||
              stage.diagnosticCharacterCount == null ||
              stage.diagnosticCtePrefixCount == null || stage.estimate == null ||
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
            // Explicit materialization belongs exclusively to portfolio v6.
            // Legacy v5 replay must fail closed without widening its enum.
            return None
          case PostgreSqlStagePhysicalForm.TerminalSelect =>
            if (
              index != evidence.stages.size - 1 ||
              stage.trace.id != PostgreSqlStageId.FinalReconstruction ||
              stage.diagnosticStatementSha256 == null ||
              stage.diagnosticCharacterCount == null ||
              stage.diagnosticCtePrefixCount == null || stage.estimate == null ||
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
      else Some(Usage(emittedCount, characterCount))
    } catch { case scala.util.control.NonFatal(_) => None }

  def renderedUsage(
      candidate: PostgreSqlFactorAwareRenderedCandidateIdentity,
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Option[Usage] =
    try {
      if (
        candidate == null || limits == null || candidate.stages == null ||
        candidate.stages.isEmpty ||
        BigInt(candidate.stages.size) >
          BigInt(limits.maxStageProbesPerCandidate)
      ) return None
      var index = 0
      var characterCount = BigInt(0)
      val namedCount = candidate.stages.size - 1
      while (index < candidate.stages.size) {
        val stage = candidate.stages(index)
        val terminal = index == namedCount
        val expectedPrefix = if (terminal) namedCount.toLong else index.toLong + 1L
        if (
          stage == null || stage.stageId == null ||
          stage.statementSha256 == null ||
          !stage.statementSha256.matches("[0-9a-f]{64}") ||
          stage.characterCount <= 0L ||
          stage.characterCount > limits.maxCharactersPerProbe ||
          stage.ctePrefixCount != expectedPrefix ||
          (terminal != (stage.stageId == PostgreSqlStageId.FinalReconstruction))
        ) return None
        characterCount += BigInt(stage.characterCount)
        if (
          characterCount > BigInt(limits.maxDiagnosticCharactersPerCandidate)
        ) return None
        index += 1
      }
      if (candidate.stages.map(_.stageId).distinct.size != candidate.stages.size)
        None
      else Some(Usage(BigInt(candidate.stages.size), characterCount))
    } catch { case scala.util.control.NonFatal(_) => None }

  def withinSession(
      usages: Vector[Usage],
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Boolean =
    try {
      usages != null && limits != null && usages.forall(_ != null) && {
        val statementCount = usages.foldLeft(BigInt(1))(
          _ + _.statementCount
        )
        val characterCount = usages.foldLeft(BigInt(0))(
          _ + _.characterCount
        )
        statementCount <= BigInt(limits.maxExplainStatementsPerSession) &&
        statementCount <= BigInt(4096) &&
        characterCount <= BigInt(limits.maxDiagnosticCharactersPerSession)
      }
    } catch { case scala.util.control.NonFatal(_) => false }
}

/** Total replay of the factor-aware selection evidence retained in v5. */
private[yannakakis] object PostgreSqlFactorAwareSelectionReplay {
  def verify(
      plans: Vector[RankedCandidatePlan],
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      source: PostgreSqlExplainSourceEvaluation,
      checkpoints: Vector[PostgreSqlFactorAwareCheckpoint],
      limits: PostgreSqlFactorAwareSelectionLimits,
      expectedSourceStatementSha256: Option[String]
  ): Boolean =
    try {
      if (
        limits == null || plans == null || plans.isEmpty ||
        plans.size > PostgreSqlFactorAwareProgressiveV1.RequiredPlanLimit ||
        plans.exists(_ == null) || evaluations == null ||
        evaluations.size != plans.size || evaluations.exists(_ == null) ||
        source == null || source.estimate == null ||
        expectedSourceStatementSha256 == null ||
        expectedSourceStatementSha256.exists(value =>
          value == null || !value.matches("[0-9a-f]{64}") ||
            value != source.sourceStatementSha256
        ) ||
        !PostgreSqlFactorAwareNumericEnvelope.accepts(source.estimate) ||
        checkpoints == null || checkpoints.exists(_ == null) ||
        checkpoints.map(_.requestedPrefix) !=
          PostgreSqlFactorAwareProgressiveV1.Checkpoints
      ) return false

      val diagnosticUsages = evaluations.map { evaluation =>
        evaluation.factorAwareEvidence.flatMap(evidence =>
          PostgreSqlFactorAwareDiagnosticReplay.evidenceUsage(evidence, limits)
        )
      }
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
          val evidence = evaluation.factorAwareEvidence.orNull
          val terminal =
            if (evidence == null || evidence.stages == null || evidence.stages.isEmpty)
              null
            else evidence.stages.last
          val namedValues =
            if (evidence == null || evidence.stages == null) Vector.empty
            else
              evidence.stages.collect {
                case stage if stage.trace.physicalForm.namedCte =>
                  val estimate = stage.estimate.get
                  PostgreSqlFactorAwareScore.exactEstimatedBytes(
                    estimate.planRows,
                    estimate.planWidth
                  )
              }
          val recomputedMaximum = namedValues.foldLeft(BigDecimal(0))(_.max(_))
          val recomputedSum = PostgreSqlFactorAwareScore.exactSum(namedValues)
          plan.rank == evaluation.structuralRank &&
          plan.candidateId == evaluation.candidateId &&
          evaluation.physicalVariantId ==
            PostgreSqlPhysicalVariantId.from(
              plan.candidateId,
              evaluation.reductionMode
            ) && evidence != null &&
          evidence.physicalVariantId == evaluation.physicalVariantId &&
          terminal != null && terminal.estimate.nonEmpty &&
          terminal.diagnosticStatementSha256.contains(
            evaluation.renderedStatementSha256
          ) && evaluation.explainCostEstimate.contains(terminal.estimate.get) &&
          evidence.score.maxNamedStagePlanRowsTimesPlanWidth ==
            recomputedMaximum &&
          evidence.score.sumNamedStagePlanRowsTimesPlanWidth == recomputedSum &&
          evidence.score.finalPlanTotalCost ==
            terminal.estimate.get.planTotalCost &&
          evidence.score.structuralRank == evaluation.structuralRank &&
          evidence.score.candidateId == evaluation.candidateId &&
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
      if (!evaluationsValid || evaluations.map(_.reductionMode).distinct.size != 1)
        return false

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
                PostgreSqlCandidateSelectionDecision.factorAwareOrdering
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
          winner.flatMap(_.factorAwareEvidence.map(_.score))
      }
    } catch { case scala.util.control.NonFatal(_) => false }
}

/** Total replay of SQL-free evidence retained for a factor-aware failure. */
private[yannakakis] object PostgreSqlFactorAwareFailureReplay {
  def verify(
      plans: Vector[RankedCandidatePlan],
      failure: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed,
      expectedSourceStatementSha256: Option[String]
  ): Boolean =
    failure != null &&
      failure.policy ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 &&
      verifyInternal(plans, failure, expectedSourceStatementSha256)

  private[postgresql] def verifyCountStageCost(
      plans: Vector[RankedCandidatePlan],
      failure: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed,
      expectedSourceStatementSha256: Option[String]
  ): Boolean =
    failure != null &&
      failure.policy ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 &&
      failure.reductionMode == YannakakisReductionMode.FullTwoPass &&
      verifyInternal(plans, failure, expectedSourceStatementSha256)

  private def verifyInternal(
      plans: Vector[RankedCandidatePlan],
      failure: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed,
      expectedSourceStatementSha256: Option[String]
  ): Boolean =
    try {
      if (failure == null) return false
      val limits = failure.factorAwareSelectionLimits.orNull
      if (
        limits == null || plans == null || plans.isEmpty ||
        plans.size > PostgreSqlFactorAwareProgressiveV1.RequiredPlanLimit ||
        plans.exists(_ == null) || failure.failureCode == null ||
        failure.stage == null ||
        !PostgreSqlFactorAwareFailureMatrix.accepts(
          failure.failureCode,
          failure.stage
        ) ||
        failure.renderedCandidates == null ||
        failure.renderedCandidates.size > limits.maxCandidates ||
        failure.renderedCandidates.size > plans.size ||
        failure.renderedCandidates.exists(_ == null) ||
        failure.completedStageObservations == null ||
        failure.completedStageObservations.exists(_ == null) ||
        failure.evaluations == null ||
        failure.evaluations.size > limits.maxCandidates ||
        failure.evaluations.exists(_ == null) ||
        failure.sourceEvaluation == null ||
        expectedSourceStatementSha256 == null ||
        expectedSourceStatementSha256.exists(value =>
          value == null || !value.matches("[0-9a-f]{64}") ||
            failure.sourceEvaluation.exists(
              _.sourceStatementSha256 != value
            )
        ) ||
        failure.sourceEvaluation.exists(source =>
          source == null || source.estimate == null ||
            !PostgreSqlFactorAwareNumericEnvelope.accepts(source.estimate)
        )
      ) return false

      if (
        plans.map(_.rank) != (1 to plans.size).toVector ||
        plans.exists(plan =>
          plan.candidateId == null || plan.decomposition == null ||
            plan.candidateId != CandidatePlanId.from(plan.decomposition)
        ) || plans.map(_.candidateId).distinct.size != plans.size ||
        failure.renderedCandidates.exists(candidate =>
          candidate.stages == null || candidate.stages.isEmpty ||
            BigInt(candidate.stages.size) >
              BigInt(limits.maxStageProbesPerCandidate) ||
            candidate.stages.exists(_ == null)
        )
      ) return false
      val renderedUsages = failure.renderedCandidates.map(candidate =>
        PostgreSqlFactorAwareDiagnosticReplay.renderedUsage(candidate, limits)
      )
      val countStageCost =
        failure.policy ==
          CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
      val evaluationUsages = failure.evaluations.map { evaluation =>
        if (countStageCost)
          evaluation.countStageCostEvidence.flatMap(evidence =>
            PostgreSqlCountStageCostDiagnosticReplay.evidenceUsage(
              evidence,
              limits
            )
          )
        else
          evaluation.factorAwareEvidence.flatMap(evidence =>
            PostgreSqlFactorAwareDiagnosticReplay.evidenceUsage(
              evidence,
              limits
            )
          )
      }
      if (
        renderedUsages.exists(_.isEmpty) || evaluationUsages.exists(_.isEmpty) ||
        !PostgreSqlFactorAwareDiagnosticReplay.withinSession(
          renderedUsages.flatten,
          limits
        ) || !PostgreSqlFactorAwareDiagnosticReplay.withinSession(
          evaluationUsages.flatten,
          limits
        )
      ) return false
      val renderedStatementCount = failure.renderedCandidates.foldLeft(BigInt(0))(
        _ + _.stages.size
      )
      if (
        renderedStatementCount >
          BigInt(limits.maxExplainStatementsPerSession - 1L) ||
        renderedStatementCount > BigInt(4095) ||
        BigInt(failure.completedStageObservations.size) > renderedStatementCount
      ) return false
      if (
        failure.renderedCandidates.zip(plans).exists {
          case (candidate, plan) =>
            candidate.structuralRank != plan.rank ||
            candidate.candidateId != plan.candidateId ||
            candidate.physicalVariantId != PostgreSqlPhysicalVariantId.from(
              plan.candidateId,
              failure.reductionMode
            )
        } || failure.failedStructuralRank.exists(rank =>
          rank > plans.size ||
            !failure.failedCandidateId.contains(plans(rank - 1).candidateId) ||
            !failure.failedPhysicalVariantId.contains(
              PostgreSqlPhysicalVariantId.from(
                plans(rank - 1).candidateId,
                failure.reductionMode
              )
            )
        )
      ) return false

      val flattened = failure.renderedCandidates.flatMap { candidate =>
        candidate.stages.map(stage => candidate -> stage)
      }
      if (
        failure.completedStageObservations.size > flattened.size ||
        !failure.completedStageObservations.zip(flattened).forall {
          case (observation, (candidate, stage)) =>
            observation.estimate != null &&
            PostgreSqlFactorAwareNumericEnvelope.accepts(observation.estimate) &&
            observation.physicalVariantId == candidate.physicalVariantId &&
            observation.stageId == stage.stageId &&
            observation.statementSha256 == stage.statementSha256 &&
            failure.sourceEvaluation.exists(
              _.estimate.provenance == observation.estimate.provenance
            )
        }
      ) return false

      val failedStageValid = failure.failedStageObservationIndex match {
        case Some(index) =>
          index == failure.completedStageObservations.size &&
          index >= 0 && index < flattened.size && {
            val (candidate, stage) = flattened(index)
            failure.stage == PostgreSqlExplainEvaluationStage.StageExplain &&
            failure.failedCandidateId.contains(candidate.candidateId) &&
            failure.failedStructuralRank.contains(candidate.structuralRank) &&
            failure.failedPhysicalVariantId.contains(candidate.physicalVariantId) &&
            failure.failedStageId.contains(stage.stageId)
          }
        case None =>
          failure.stage != PostgreSqlExplainEvaluationStage.StageExplain &&
            failure.failedStageId.isEmpty
      }
      if (!failedStageValid) return false

      var completeCandidates = 0
      var completeStatements = 0
      while (
        completeCandidates < failure.renderedCandidates.size &&
        completeStatements +
          failure.renderedCandidates(completeCandidates).stages.size <=
          failure.completedStageObservations.size
      ) {
        completeStatements +=
          failure.renderedCandidates(completeCandidates).stages.size
        completeCandidates += 1
      }
      if (
        failure.evaluations.map(evaluation =>
          (evaluation.structuralRank, evaluation.candidateId, evaluation.physicalVariantId)
        ) != failure.renderedCandidates.take(completeCandidates).map(candidate =>
          (candidate.structuralRank, candidate.candidateId, candidate.physicalVariantId)
        )
      ) return false

      var offset = 0
      val evaluationsValid = failure.evaluations.zip(
        failure.renderedCandidates.take(completeCandidates)
      ).forall { case (evaluation, renderedCandidate) =>
        val emitted =
          (if (countStageCost)
             evaluation.countStageCostEvidence.toVector.map(_.stages)
           else evaluation.factorAwareEvidence.toVector.map(_.stages))
            .flatten
            .filter(_.trace.physicalForm.emitted)
        val observed = failure.completedStageObservations.slice(
          offset,
          offset + emitted.size
        )
        offset += emitted.size
        emitted.size == renderedCandidate.stages.size &&
        emitted.size == observed.size &&
        emitted.zip(renderedCandidate.stages).zip(observed).forall {
          case ((stage, renderedStage), observation) =>
            stage.trace.id == observation.stageId &&
            stage.trace.id == renderedStage.stageId &&
            stage.diagnosticStatementSha256.contains(
              observation.statementSha256
            ) && stage.diagnosticStatementSha256.contains(
              renderedStage.statementSha256
            ) && stage.diagnosticCharacterCount.contains(
              renderedStage.characterCount
            ) && stage.diagnosticCtePrefixCount.contains(
              renderedStage.ctePrefixCount
            ) && stage.estimate.contains(observation.estimate)
        }
      }
      evaluationsValid && offset == completeStatements
    } catch { case scala.util.control.NonFatal(_) => false }
}
