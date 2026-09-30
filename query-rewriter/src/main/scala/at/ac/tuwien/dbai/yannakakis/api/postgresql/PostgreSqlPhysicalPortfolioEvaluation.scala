package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlStageDiagnosticLimits,
  PostgreSqlStageId,
  PostgreSqlStageKind,
  PostgreSqlStagePhysicalForm,
  PostgreSqlStageTrace
}
import at.ac.tuwien.dbai.yannakakis.core.BagId

import scala.collection.mutable

/** Frozen score and final-frontier conventions for portfolio v1. */
object PostgreSqlPhysicalPortfolioEvaluationV1 {
  val ScoreScheme: String =
    "postgresql_physical_portfolio_final_frontier_score_v1"
  val FinalFrontierScheme: String =
    "postgresql_emitted_support_plus_latest_factor_per_bag_v1"
  val ScoreOrder: Vector[String] = Vector(
    "max_all_named_stage_plan_rows_times_plan_width",
    "sum_final_frontier_plan_rows_times_plan_width",
    "final_plan_total_cost",
    "structural_rank",
    "candidate_id",
    "representative_physical_variant_id"
  )
}

sealed trait PostgreSqlPhysicalPortfolioEvaluationResource
    extends Product
    with Serializable {
  def code: String
}

object PostgreSqlPhysicalPortfolioEvaluationResource {
  case object Workloads extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "workloads"
  }
  case object StageProbesPerWorkload
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "stage-probes-per-workload"
  }
  case object CharactersPerProbe
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "characters-per-probe"
  }
  case object DiagnosticCharactersPerWorkload
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "diagnostic-characters-per-workload"
  }
  case object RawDiagnosticCharactersRetained
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "raw-diagnostic-characters-retained"
  }
  case object ExplainStatementsPerSession
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "explain-statements-per-session"
  }
  case object DiagnosticCharactersPerSession
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "diagnostic-characters-per-session"
  }
  case object PlanJsonCharacters
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "plan-json-characters"
  }
  case object CumulativePlanJsonCharacters
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "cumulative-plan-json-characters"
  }
  case object StatementTimeoutSeconds
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "statement-timeout-seconds"
  }
  case object ExplainStatementBudgetSeconds
      extends PostgreSqlPhysicalPortfolioEvaluationResource {
    override val code: String = "explain-statement-budget-seconds"
  }
}

sealed trait PostgreSqlPhysicalPortfolioEvaluationLimitError
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object PostgreSqlPhysicalPortfolioEvaluationLimitError {
  final case class InvalidResourceLimit(
      resource: PostgreSqlPhysicalPortfolioEvaluationResource,
      value: Long
  ) extends PostgreSqlPhysicalPortfolioEvaluationLimitError {
    override val code: String =
      s"physical-portfolio-evaluation.limit.invalid.${resource.code}"
    override val message: String =
      s"Physical portfolio evaluation limit ${resource.code} must be positive, received $value"
  }

  final case class FrozenContractLimit(
      resource: PostgreSqlPhysicalPortfolioEvaluationResource,
      value: Long,
      required: Long
  ) extends PostgreSqlPhysicalPortfolioEvaluationLimitError {
    override val code: String =
      s"physical-portfolio-evaluation.limit.frozen.${resource.code}"
    override val message: String =
      s"Physical portfolio v1 requires ${resource.code}=$required, received $value"
  }

  final case class MaximumResourceLimit(
      resource: PostgreSqlPhysicalPortfolioEvaluationResource,
      value: Long,
      maximum: Long
  ) extends PostgreSqlPhysicalPortfolioEvaluationLimitError {
    override val code: String =
      s"physical-portfolio-evaluation.limit.maximum.${resource.code}"
    override val message: String =
      s"Physical portfolio evaluation limit ${resource.code} must not exceed $maximum, received $value"
  }

  final case class IntegerMaterializationLimit(
      resource: PostgreSqlPhysicalPortfolioEvaluationResource,
      value: Long
  ) extends PostgreSqlPhysicalPortfolioEvaluationLimitError {
    override val code: String =
      s"physical-portfolio-evaluation.limit.integer.${resource.code}"
    override val message: String =
      s"Physical portfolio evaluation limit ${resource.code} must not exceed ${Int.MaxValue}, received $value"
  }

  final case class InconsistentResourceLimits(
      smaller: PostgreSqlPhysicalPortfolioEvaluationResource,
      larger: PostgreSqlPhysicalPortfolioEvaluationResource
  ) extends PostgreSqlPhysicalPortfolioEvaluationLimitError {
    override val code: String =
      s"physical-portfolio-evaluation.limit.inconsistent.${smaller.code}.${larger.code}"
    override val message: String =
      s"Physical portfolio evaluation limit ${larger.code} must cover ${smaller.code}"
  }
}

/** Separate v6 evaluation envelope; the legacy v5 K=100 limit is untouched. */
final class PostgreSqlPhysicalPortfolioEvaluationLimits private (
    val maxWorkloads: Long,
    val maxStageProbesPerWorkload: Long,
    val maxCharactersPerProbe: Long,
    val maxDiagnosticCharactersPerWorkload: Long,
    val maxRawDiagnosticCharactersRetained: Long,
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
        maxStageProbesPerWorkload,
        maxCharactersPerProbe,
        maxDiagnosticCharactersPerWorkload
      )
      .fold(
        errors =>
          throw new IllegalStateException(
            s"validated portfolio limits produced invalid diagnostic limits: ${errors.map(_.code).mkString(",") }"
          ),
        identity
      )

  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlPhysicalPortfolioEvaluationLimits =>
      maxWorkloads == that.maxWorkloads &&
        maxStageProbesPerWorkload == that.maxStageProbesPerWorkload &&
        maxCharactersPerProbe == that.maxCharactersPerProbe &&
        maxDiagnosticCharactersPerWorkload ==
          that.maxDiagnosticCharactersPerWorkload &&
        maxRawDiagnosticCharactersRetained ==
          that.maxRawDiagnosticCharactersRetained &&
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
      maxWorkloads,
      maxStageProbesPerWorkload,
      maxCharactersPerProbe,
      maxDiagnosticCharactersPerWorkload,
      maxRawDiagnosticCharactersRetained,
      maxExplainStatementsPerSession,
      maxDiagnosticCharactersPerSession,
      maxPlanJsonCharacters,
      maxCumulativePlanJsonCharacters,
      statementTimeoutSeconds,
      explainStatementBudgetSeconds
    ).hashCode()

  override def toString: String =
    s"PostgreSqlPhysicalPortfolioEvaluationLimits($maxWorkloads," +
      s"$maxStageProbesPerWorkload,$maxCharactersPerProbe," +
      s"$maxDiagnosticCharactersPerWorkload," +
      s"$maxRawDiagnosticCharactersRetained,$maxExplainStatementsPerSession," +
      s"$maxDiagnosticCharactersPerSession,$maxPlanJsonCharacters," +
      s"$maxCumulativePlanJsonCharacters,$statementTimeoutSeconds," +
      s"$explainStatementBudgetSeconds)"
}

object PostgreSqlPhysicalPortfolioEvaluationLimits {
  import PostgreSqlPhysicalPortfolioEvaluationLimitError._
  import PostgreSqlPhysicalPortfolioEvaluationResource._

  private val Mi: Long = 1024L * 1024L

  val default: PostgreSqlPhysicalPortfolioEvaluationLimits =
    new PostgreSqlPhysicalPortfolioEvaluationLimits(
      maxWorkloads = 256L,
      maxStageProbesPerWorkload = 256L,
      maxCharactersPerProbe = 16L * Mi,
      maxDiagnosticCharactersPerWorkload = 64L * Mi,
      maxRawDiagnosticCharactersRetained = 256L * Mi,
      maxExplainStatementsPerSession = 4096L,
      maxDiagnosticCharactersPerSession = 256L * Mi,
      maxPlanJsonCharacters = 1L * Mi,
      maxCumulativePlanJsonCharacters = 128L * Mi,
      statementTimeoutSeconds = 30,
      explainStatementBudgetSeconds = 600
    )

  def create(
      maxWorkloads: Long = default.maxWorkloads,
      maxStageProbesPerWorkload: Long = default.maxStageProbesPerWorkload,
      maxCharactersPerProbe: Long = default.maxCharactersPerProbe,
      maxDiagnosticCharactersPerWorkload: Long =
        default.maxDiagnosticCharactersPerWorkload,
      maxRawDiagnosticCharactersRetained: Long =
        default.maxRawDiagnosticCharactersRetained,
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
    Vector[PostgreSqlPhysicalPortfolioEvaluationLimitError],
    PostgreSqlPhysicalPortfolioEvaluationLimits
  ] = {
    val supplied = Vector(
      Workloads -> maxWorkloads,
      StageProbesPerWorkload -> maxStageProbesPerWorkload,
      CharactersPerProbe -> maxCharactersPerProbe,
      DiagnosticCharactersPerWorkload -> maxDiagnosticCharactersPerWorkload,
      RawDiagnosticCharactersRetained -> maxRawDiagnosticCharactersRetained,
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
    val frozen =
      if (maxWorkloads > 0L && maxWorkloads != default.maxWorkloads)
        Vector(FrozenContractLimit(Workloads, maxWorkloads, default.maxWorkloads))
      else Vector.empty
    val maxima = supplied.collect {
      case (resource, value)
          if resource != Workloads && value > maximum(resource) =>
        MaximumResourceLimit(resource, value, maximum(resource))
    }
    val integers = Vector(
      CharactersPerProbe -> maxCharactersPerProbe,
      PlanJsonCharacters -> maxPlanJsonCharacters,
      StatementTimeoutSeconds -> statementTimeoutSeconds.toLong,
      ExplainStatementBudgetSeconds -> explainStatementBudgetSeconds.toLong
    ).collect {
      case (resource, value) if value > Int.MaxValue.toLong =>
        IntegerMaterializationLimit(resource, value)
    }
    val inconsistent = Vector.newBuilder[
      PostgreSqlPhysicalPortfolioEvaluationLimitError
    ]
    if (
      maxCharactersPerProbe > 0L &&
      maxDiagnosticCharactersPerWorkload > 0L &&
      maxCharactersPerProbe > maxDiagnosticCharactersPerWorkload
    ) inconsistent += InconsistentResourceLimits(
      CharactersPerProbe,
      DiagnosticCharactersPerWorkload
    )
    if (
      maxDiagnosticCharactersPerWorkload > 0L &&
      maxRawDiagnosticCharactersRetained > 0L &&
      maxDiagnosticCharactersPerWorkload > maxRawDiagnosticCharactersRetained
    ) inconsistent += InconsistentResourceLimits(
      DiagnosticCharactersPerWorkload,
      RawDiagnosticCharactersRetained
    )
    if (
      maxDiagnosticCharactersPerWorkload > 0L &&
      maxDiagnosticCharactersPerSession > 0L &&
      maxDiagnosticCharactersPerWorkload > maxDiagnosticCharactersPerSession
    ) inconsistent += InconsistentResourceLimits(
      DiagnosticCharactersPerWorkload,
      DiagnosticCharactersPerSession
    )
    if (
      maxPlanJsonCharacters > 0L && maxCumulativePlanJsonCharacters > 0L &&
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
        StageProbesPerWorkload,
        ExplainStatementsPerSession
      )

    val errors =
      invalid ++ frozen ++ maxima ++ integers ++ inconsistent.result()
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new PostgreSqlPhysicalPortfolioEvaluationLimits(
          maxWorkloads,
          maxStageProbesPerWorkload,
          maxCharactersPerProbe,
          maxDiagnosticCharactersPerWorkload,
          maxRawDiagnosticCharactersRetained,
          maxExplainStatementsPerSession,
          maxDiagnosticCharactersPerSession,
          maxPlanJsonCharacters,
          maxCumulativePlanJsonCharacters,
          statementTimeoutSeconds,
          explainStatementBudgetSeconds
        )
      )
  }

  private[postgresql] def validate(
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits
  ): Vector[PostgreSqlPhysicalPortfolioEvaluationLimitError] =
    if (limits == null)
      Vector(InvalidResourceLimit(Workloads, 0L))
    else
      create(
        limits.maxWorkloads,
        limits.maxStageProbesPerWorkload,
        limits.maxCharactersPerProbe,
        limits.maxDiagnosticCharactersPerWorkload,
        limits.maxRawDiagnosticCharactersRetained,
        limits.maxExplainStatementsPerSession,
        limits.maxDiagnosticCharactersPerSession,
        limits.maxPlanJsonCharacters,
        limits.maxCumulativePlanJsonCharacters,
        limits.statementTimeoutSeconds,
        limits.explainStatementBudgetSeconds
      ).left.getOrElse(Vector.empty)

  private def maximum(
      resource: PostgreSqlPhysicalPortfolioEvaluationResource
  ): Long = resource match {
    case Workloads                       => default.maxWorkloads
    case StageProbesPerWorkload          => default.maxStageProbesPerWorkload
    case CharactersPerProbe              => default.maxCharactersPerProbe
    case DiagnosticCharactersPerWorkload => default.maxDiagnosticCharactersPerWorkload
    case RawDiagnosticCharactersRetained => default.maxRawDiagnosticCharactersRetained
    case ExplainStatementsPerSession     => default.maxExplainStatementsPerSession
    case DiagnosticCharactersPerSession  => default.maxDiagnosticCharactersPerSession
    case PlanJsonCharacters              => default.maxPlanJsonCharacters
    case CumulativePlanJsonCharacters    => default.maxCumulativePlanJsonCharacters
    case StatementTimeoutSeconds         => default.statementTimeoutSeconds.toLong
    case ExplainStatementBudgetSeconds   => default.explainStatementBudgetSeconds.toLong
  }
}

/** Exact lexicographic score used only by the physical portfolio policy. */
final case class PostgreSqlPhysicalPortfolioScore(
    maxAllNamedStagePlanRowsTimesPlanWidth: BigDecimal,
    sumFinalFrontierPlanRowsTimesPlanWidth: BigDecimal,
    finalPlanTotalCost: BigDecimal,
    structuralRank: Int,
    candidateId: CandidatePlanId,
    representativePhysicalVariantId: PostgreSqlPhysicalVariantId
) extends Serializable {
  require(maxAllNamedStagePlanRowsTimesPlanWidth != null)
  require(maxAllNamedStagePlanRowsTimesPlanWidth.signum >= 0)
  require(sumFinalFrontierPlanRowsTimesPlanWidth != null)
  require(sumFinalFrontierPlanRowsTimesPlanWidth.signum >= 0)
  require(finalPlanTotalCost != null && finalPlanTotalCost.signum >= 0)
  require(structuralRank > 0)
  require(candidateId != null)
  require(representativePhysicalVariantId != null)
  require(
    PostgreSqlFactorAwareNumericEnvelope.acceptsDerived(
      maxAllNamedStagePlanRowsTimesPlanWidth
    ) && PostgreSqlFactorAwareNumericEnvelope.acceptsDerived(
      sumFinalFrontierPlanRowsTimesPlanWidth
    ) && PostgreSqlFactorAwareNumericEnvelope.accepts(finalPlanTotalCost),
    "physical portfolio score exceeds the frozen numeric envelope"
  )
}

object PostgreSqlPhysicalPortfolioScore {
  implicit val ordering: Ordering[PostgreSqlPhysicalPortfolioScore] =
    new Ordering[PostgreSqlPhysicalPortfolioScore] {
      override def compare(
          left: PostgreSqlPhysicalPortfolioScore,
          right: PostgreSqlPhysicalPortfolioScore
      ): Int = {
        val peak = left.maxAllNamedStagePlanRowsTimesPlanWidth.compare(
          right.maxAllNamedStagePlanRowsTimesPlanWidth
        )
        if (peak != 0) return peak
        val frontier = left.sumFinalFrontierPlanRowsTimesPlanWidth.compare(
          right.sumFinalFrontierPlanRowsTimesPlanWidth
        )
        if (frontier != 0) return frontier
        val cost = left.finalPlanTotalCost.compare(right.finalPlanTotalCost)
        if (cost != 0) return cost
        val rank = Integer.compare(left.structuralRank, right.structuralRank)
        if (rank != 0) return rank
        val candidate = left.candidateId.value.compareTo(right.candidateId.value)
        if (candidate != 0) return candidate
        left.representativePhysicalVariantId.value.compareTo(
          right.representativePhysicalVariantId.value
        )
      }
    }
}

/** Trace-bound optimizer evidence for one emitted or elided renderer stage. */
final case class PostgreSqlPhysicalPortfolioStageEvaluation private[postgresql] (
    trace: PostgreSqlStageTrace,
    diagnosticStatementSha256: Option[String],
    diagnosticCharacterCount: Option[Long],
    diagnosticCtePrefixCount: Option[Long],
    estimate: Option[PostgreSqlExplainCostEstimate],
    finalFrontier: Boolean
) extends Serializable {
  require(trace != null)
  require(diagnosticStatementSha256 != null)
  require(diagnosticCharacterCount != null)
  require(diagnosticCtePrefixCount != null)
  require(estimate != null)
  private val present = Vector(
    diagnosticStatementSha256.nonEmpty,
    diagnosticCharacterCount.nonEmpty,
    diagnosticCtePrefixCount.nonEmpty,
    estimate.nonEmpty
  )
  require(present.distinct.size == 1)
  require(trace.physicalForm.emitted == estimate.nonEmpty)
  require(diagnosticStatementSha256.forall(_.matches("[0-9a-f]{64}")))
  require(diagnosticCharacterCount.forall(_ >= 0L))
  require(diagnosticCtePrefixCount.forall(_ >= 0L))
  require(estimate.forall(PostgreSqlFactorAwareNumericEnvelope.accepts))
  require(!finalFrontier || trace.physicalForm.namedCte)
}

/** Independently derived final live named-stage set. */
object PostgreSqlPhysicalPortfolioFinalFrontier {
  def derive(
      stages: Vector[PostgreSqlStageTrace]
  ): Either[String, Vector[PostgreSqlStageId]] = {
    if (stages == null || stages.isEmpty || stages.exists(_ == null))
      return Left("physical portfolio stage trace is missing")
    if (
      stages.map(_.id).distinct.size != stages.size ||
      stages.last.id != PostgreSqlStageId.FinalReconstruction ||
      stages.last.kind != PostgreSqlStageKind.FinalReconstruction ||
      stages.last.physicalForm != PostgreSqlStagePhysicalForm.TerminalSelect
    ) return Left("physical portfolio terminal trace is invalid")

    val supports = Vector.newBuilder[PostgreSqlStageId]
    val latest = mutable.Map.empty[BagId, (Int, PostgreSqlStageId)]
    var index = 0
    while (index < stages.size - 1) {
      val stage = stages(index)
      stage.kind match {
        case PostgreSqlStageKind.Support =>
          stage.id match {
            case PostgreSqlStageId.BagSupport(bagId)
                if stage.logicalBagIds == Vector(bagId) =>
              if (stage.physicalForm.namedCte) supports += stage.id
              else if (stage.physicalForm != PostgreSqlStagePhysicalForm.Elided)
                return Left("support stage is neither named nor elided")
            case _ => return Left("support stage identity is not canonical")
          }
        case PostgreSqlStageKind.InitialFactor =>
          stage.id match {
            case PostgreSqlStageId.BagFactorVersion(bagId, 0)
                if stage.logicalBagIds == Vector(bagId) &&
                  stage.physicalForm.namedCte && !latest.contains(bagId) =>
              latest.put(bagId, 0 -> stage.id)
            case _ => return Left("initial factor stage identity is not canonical")
          }
        case PostgreSqlStageKind.FusedReduction =>
          stage.id match {
            case PostgreSqlStageId.BagFactorVersion(bagId, version)
                if version > 0 && stage.logicalBagIds.headOption.contains(bagId) &&
                  stage.physicalForm.namedCte &&
                  latest.get(bagId).exists(_._1 + 1 == version) =>
              latest.put(bagId, version -> stage.id)
            case _ => return Left("fused reduction version is not contiguous")
          }
        case PostgreSqlStageKind.FinalReconstruction =>
          return Left("terminal reconstruction must occur exactly once at the end")
      }
      index += 1
    }
    if (latest.isEmpty) return Left("physical portfolio trace has no factors")
    val terminal = stages.last
    if (
      terminal.logicalBagIds.size != latest.size ||
      terminal.logicalBagIds.distinct.size != latest.size ||
      terminal.logicalBagIds.toSet != latest.keySet.toSet
    ) return Left("terminal inputs do not equal the latest factor frontier")
    val latestIdsInRendererOrder = terminal.logicalBagIds.map(bagId =>
      latest.getOrElse(
        bagId,
        return Left("terminal reads a bag without a latest factor")
      )._2
    )
    if (terminal.inputStageIds != latestIdsInRendererOrder)
      return Left("terminal latest-factor inputs are not in exact renderer order")
    Right(supports.result() ++ latestIdsInRendererOrder)
  }
}

/** Complete score evidence for one representative physical workload. */
final case class PostgreSqlPhysicalPortfolioEvaluationEvidence private[postgresql] (
    workload: PostgreSqlPhysicalPortfolioWorkloadGroupEvidence,
    source: PostgreSqlExplainSourceEvaluation,
    stages: Vector[PostgreSqlPhysicalPortfolioStageEvaluation],
    finalFrontierStageIds: Vector[PostgreSqlStageId],
    rowGuard: PostgreSqlExplainRowGuardEvidence,
    score: PostgreSqlPhysicalPortfolioScore,
    scoreScheme: String,
    finalFrontierScheme: String,
    numericEnvelopeScheme: String
) extends Serializable {
  require(workload != null)
  require(source != null)
  require(PostgreSqlFactorAwareNumericEnvelope.accepts(source.estimate))
  require(stages != null && stages.nonEmpty && stages.forall(_ != null))
  require(stages.map(_.trace.id).distinct.size == stages.size)
  require(finalFrontierStageIds != null && finalFrontierStageIds.nonEmpty)
  require(finalFrontierStageIds.distinct.size == finalFrontierStageIds.size)
  require(rowGuard != null)
  require(score != null)
  require(
    scoreScheme == PostgreSqlPhysicalPortfolioEvaluationV1.ScoreScheme &&
      finalFrontierScheme ==
        PostgreSqlPhysicalPortfolioEvaluationV1.FinalFrontierScheme &&
      numericEnvelopeScheme == PostgreSqlFactorAwareNumericEnvelope.Scheme
  )
  require(
    score.structuralRank == workload.representative.structuralRank &&
      score.candidateId == workload.representative.candidateId &&
      score.representativePhysicalVariantId ==
        workload.representative.physicalVariantId
  )
  require(
    stages.filter(_.trace.physicalForm.namedCte).forall(stage =>
      PostgreSqlPhysicalPortfolioWorkloadDescriptor
        .expectedPhysicalForm(workload.descriptor.materialization)
        .contains(stage.trace.physicalForm)
    ),
    "portfolio evaluation stage forms must match the workload directive"
  )
  require(
    stages.flatMap(_.estimate).forall(_.provenance == source.estimate.provenance),
    "portfolio source and stage estimates must share exact provenance"
  )
  private val emitted = stages.filter(_.trace.physicalForm.emitted)
  require(
    emitted.map(stage =>
      PostgreSqlPhysicalPortfolioStageWorkloadIdentity(
        stage.trace.id,
        stage.diagnosticStatementSha256.get,
        stage.diagnosticCharacterCount.get,
        stage.diagnosticCtePrefixCount.get
      )
    ) == workload.descriptor.stages,
    "portfolio evaluation must bind the exact deduplicated workload"
  )
  private val derivedFrontier =
    PostgreSqlPhysicalPortfolioFinalFrontier
      .derive(stages.map(_.trace))
      .fold(message => throw new IllegalArgumentException(message), identity)
  require(derivedFrontier == finalFrontierStageIds)
  require(
    stages.count(_.finalFrontier) == finalFrontierStageIds.size &&
      stages.filter(_.finalFrontier).map(_.trace.id).toSet ==
        finalFrontierStageIds.toSet,
    "portfolio final-frontier flags must match the derived stage set"
  )
  private val namedValues = stages.collect {
    case stage if stage.trace.physicalForm.namedCte =>
      PostgreSqlFactorAwareScore.exactEstimatedBytes(
        stage.estimate.get.planRows,
        stage.estimate.get.planWidth
      )
  }
  private val frontierValues = stages.collect {
    case stage if stage.finalFrontier =>
      PostgreSqlFactorAwareScore.exactEstimatedBytes(
        stage.estimate.get.planRows,
        stage.estimate.get.planWidth
      )
  }
  private val expectedPeak = namedValues.foldLeft(BigDecimal(0))(_.max(_))
  private val expectedFrontier =
    PostgreSqlFactorAwareScore.exactSum(frontierValues)
  require(
    stages.last.trace.id == PostgreSqlStageId.FinalReconstruction &&
      stages.last.estimate.nonEmpty &&
      score.maxAllNamedStagePlanRowsTimesPlanWidth == expectedPeak &&
      score.sumFinalFrontierPlanRowsTimesPlanWidth == expectedFrontier &&
      score.finalPlanTotalCost == stages.last.estimate.get.planTotalCost,
    "portfolio score must be exactly recomputable from stage evidence"
  )
  require(
    rowGuard == PostgreSqlExplainRowGuardEvidence.from(
      source.estimate.planRows,
      stages.last.estimate.get.planRows
    ),
    "portfolio row guard must bind source and terminal estimates exactly"
  )
}
