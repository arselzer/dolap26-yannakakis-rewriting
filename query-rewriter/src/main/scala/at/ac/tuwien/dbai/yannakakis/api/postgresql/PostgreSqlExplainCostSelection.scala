package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

/** Resource whose exact bound is enforced while obtaining EXPLAIN costs. */
sealed trait PostgreSqlExplainSelectionResource
    extends Product
    with Serializable {
  def code: String
}

object PostgreSqlExplainSelectionResource {
  case object StatementTimeoutSeconds
      extends PostgreSqlExplainSelectionResource {
    override val code: String = "statement-timeout-seconds"
  }
  case object PlanJsonCharacters extends PostgreSqlExplainSelectionResource {
    override val code: String = "plan-json-characters"
  }
  case object CumulativePlanJsonCharacters
      extends PostgreSqlExplainSelectionResource {
    override val code: String = "cumulative-plan-json-characters"
  }
}

sealed trait PostgreSqlExplainSelectionLimitError
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object PostgreSqlExplainSelectionLimitError {
  final case class InvalidResourceLimit(
      resource: PostgreSqlExplainSelectionResource,
      value: Long
  ) extends PostgreSqlExplainSelectionLimitError {
    override val code: String = s"explain-limit.invalid.${resource.code}"
    override val message: String =
      s"PostgreSQL EXPLAIN limit ${resource.code} must be positive, received $value"
  }

  final case class CumulativeLimitBelowPerPlan(
      perPlan: Long,
      cumulative: Long
  ) extends PostgreSqlExplainSelectionLimitError {
    override val code: String = "explain-limit.inconsistent.plan-json-characters"
    override val message: String =
      "Cumulative PostgreSQL EXPLAIN JSON characters must cover one plan document"
  }
}

/** Explicit resource and timeout envelope for optimizer-cost evaluation. */
final class PostgreSqlExplainSelectionLimits private (
    val statementTimeoutSeconds: Int,
    val maxPlanJsonCharacters: Long,
    val maxCumulativePlanJsonCharacters: Long
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlExplainSelectionLimits =>
      statementTimeoutSeconds == that.statementTimeoutSeconds &&
        maxPlanJsonCharacters == that.maxPlanJsonCharacters &&
        maxCumulativePlanJsonCharacters ==
          that.maxCumulativePlanJsonCharacters
    case _ => false
  }

  override def hashCode(): Int =
    (
      statementTimeoutSeconds,
      maxPlanJsonCharacters,
      maxCumulativePlanJsonCharacters
    ).hashCode()

  override def toString: String =
    s"PostgreSqlExplainSelectionLimits($statementTimeoutSeconds," +
      s"$maxPlanJsonCharacters,$maxCumulativePlanJsonCharacters)"
}

object PostgreSqlExplainSelectionLimits {
  val default: PostgreSqlExplainSelectionLimits =
    new PostgreSqlExplainSelectionLimits(
      statementTimeoutSeconds = 30,
      maxPlanJsonCharacters = 1024L * 1024L,
      maxCumulativePlanJsonCharacters = 16L * 1024L * 1024L
    )

  def create(
      statementTimeoutSeconds: Int = default.statementTimeoutSeconds,
      maxPlanJsonCharacters: Long = default.maxPlanJsonCharacters,
      maxCumulativePlanJsonCharacters: Long =
        default.maxCumulativePlanJsonCharacters
  ): Either[Vector[PostgreSqlExplainSelectionLimitError], PostgreSqlExplainSelectionLimits] = {
    import PostgreSqlExplainSelectionResource._
    val supplied = Vector(
      StatementTimeoutSeconds -> statementTimeoutSeconds.toLong,
      PlanJsonCharacters -> maxPlanJsonCharacters,
      CumulativePlanJsonCharacters -> maxCumulativePlanJsonCharacters
    )
    val invalid = supplied.collect {
      case (resource, value) if value <= 0L =>
        PostgreSqlExplainSelectionLimitError.InvalidResourceLimit(
          resource,
          value
        )
    }
    val inconsistent =
      if (
        maxPlanJsonCharacters > 0L &&
        maxCumulativePlanJsonCharacters > 0L &&
        maxCumulativePlanJsonCharacters < maxPlanJsonCharacters
      )
        Vector(
          PostgreSqlExplainSelectionLimitError.CumulativeLimitBelowPerPlan(
            maxPlanJsonCharacters,
            maxCumulativePlanJsonCharacters
          )
        )
      else Vector.empty
    val errors = invalid ++ inconsistent
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new PostgreSqlExplainSelectionLimits(
          statementTimeoutSeconds,
          maxPlanJsonCharacters,
          maxCumulativePlanJsonCharacters
        )
      )
  }
}

/** Sanitized, non-identifying optimizer environment evidence for one batch. */
final case class PostgreSqlExplainCostProvenance(
    serverVersionNum: Int,
    plannerSettingsFingerprintSha256: String,
    plannerSettingsFingerprintScheme: String,
    plannerSettingCount: Int
) extends Serializable {
  require(serverVersionNum > 0, "server version number must be positive")
  require(
    plannerSettingsFingerprintSha256 != null &&
      plannerSettingsFingerprintSha256.matches("[0-9a-f]{64}"),
    "planner settings fingerprint must be lowercase SHA-256"
  )
  require(
    plannerSettingsFingerprintScheme ==
      PostgreSqlExplainCostProvenance.PlannerSettingsFingerprintScheme,
    "planner settings fingerprint scheme must be the frozen v1 scheme"
  )
  require(
    plannerSettingCount ==
      PostgreSqlExplainCostProvenance.PlannerSettingNames.size,
    "planner setting count must match the frozen v1 setting vector"
  )
}

object PostgreSqlExplainCostProvenance {
  val PlannerSettingsFingerprintScheme: String =
    "postgresql_planner_settings_utf8_length_prefixed_v1"

  /**
    * Fixed, sorted, non-identifying planner-setting vector fingerprinted by
    * the v1 evaluator. Missing version-specific settings use an explicit null
    * marker in the fingerprint rather than changing this vector.
    */
  val PlannerSettingNames: Vector[String] = Vector(
    "constraint_exclusion",
    "cpu_index_tuple_cost",
    "cpu_operator_cost",
    "cpu_tuple_cost",
    "cursor_tuple_fraction",
    "default_statistics_target",
    "effective_cache_size",
    "effective_io_concurrency",
    "enable_async_append",
    "enable_bitmapscan",
    "enable_distinct_reordering",
    "enable_gathermerge",
    "enable_group_by_reordering",
    "enable_hashagg",
    "enable_hashjoin",
    "enable_incremental_sort",
    "enable_indexonlyscan",
    "enable_indexscan",
    "enable_material",
    "enable_memoize",
    "enable_mergejoin",
    "enable_nestloop",
    "enable_parallel_append",
    "enable_parallel_hash",
    "enable_partition_pruning",
    "enable_partitionwise_aggregate",
    "enable_partitionwise_join",
    "enable_presorted_aggregate",
    "enable_self_join_elimination",
    "enable_seqscan",
    "enable_sort",
    "enable_tidscan",
    "from_collapse_limit",
    "geqo",
    "geqo_effort",
    "geqo_generations",
    "geqo_pool_size",
    "geqo_seed",
    "geqo_selection_bias",
    "geqo_threshold",
    "hash_mem_multiplier",
    "jit",
    "join_collapse_limit",
    "max_parallel_workers_per_gather",
    "min_parallel_index_scan_size",
    "min_parallel_table_scan_size",
    "parallel_leader_participation",
    "parallel_setup_cost",
    "parallel_tuple_cost",
    "plan_cache_mode",
    "random_page_cost",
    "seq_page_cost",
    "work_mem"
  )

  require(
    PlannerSettingNames == PlannerSettingNames.sorted.distinct,
    "planner setting names must be sorted and unique"
  )
}

/** One optimizer estimate; it is explicitly not an observed runtime. */
final case class PostgreSqlExplainCostEstimate(
    startupCost: BigDecimal,
    planTotalCost: BigDecimal,
    planRows: BigDecimal,
    planWidth: Long,
    provenance: PostgreSqlExplainCostProvenance
) extends Serializable {
  require(startupCost != null, "startup cost must not be null")
  require(startupCost.signum >= 0, "startup cost must not be negative")
  require(planTotalCost != null, "plan total cost must not be null")
  require(planTotalCost.signum >= 0, "plan total cost must not be negative")
  require(planRows != null, "plan rows must not be null")
  require(planRows.signum >= 0, "plan rows must not be negative")
  require(planWidth >= 0L, "plan width must not be negative")
  require(provenance != null, "cost provenance must not be null")

  def canonicalStartupCost: String = canonical(startupCost)
  def canonicalPlanTotalCost: String = canonical(planTotalCost)
  def canonicalPlanRows: String = canonical(planRows)

  private def canonical(value: BigDecimal): String =
    if (value.signum == 0) "0" else value.bigDecimal.toString
}

object PostgreSqlExplainCostEstimate {
  val Source: String = "postgresql_explain_format_json_analyze_false_v1"
  val Metric: String = "plan_total_cost"
  val Units: String = "postgresql_cost_units"
  val ExplainFormat: String = "json"
  val Analyze: Boolean = false
  val Settings: Boolean = true
  val Estimated: Boolean = true
  val RuntimeMeasured: Boolean = false

  val OrderedFields: Vector[String] =
    Vector("plan_total_cost", "structural_rank", "candidate_id")
}

/** Exact rational root-row consistency evidence; no rounded ratio is stored. */
final case class PostgreSqlExplainRowGuardEvidence private[postgresql] (
    factor: Int,
    ratioNumerator: BigDecimal,
    ratioDenominator: BigDecimal,
    withinGuard: Boolean
) extends Serializable {
  require(
    factor == PostgreSqlExplainRowGuardEvidence.Factor,
    "row guard factor must be the frozen v1 factor"
  )
  require(
    ratioNumerator != null && ratioNumerator.signum >= 0,
    "row ratio numerator must be nonnegative"
  )
  require(
    ratioDenominator != null && ratioDenominator.signum > 0,
    "row ratio denominator must be positive"
  )
  require(
    withinGuard ==
      PostgreSqlExplainRowGuardEvidence.exactWithin(
        ratioNumerator,
        ratioDenominator,
        factor
      ),
    "row guard decision must match exact cross multiplication"
  )
}

object PostgreSqlExplainRowGuardEvidence {
  val Factor: Int = 10

  private def exactWithin(
      numerator: BigDecimal,
      denominator: BigDecimal,
      factor: Int
  ): Boolean = {
    val multiplier = java.math.BigDecimal.valueOf(factor.toLong)
    numerator.bigDecimal.multiply(multiplier).compareTo(
      denominator.bigDecimal
    ) >= 0 && numerator.bigDecimal.compareTo(
      denominator.bigDecimal.multiply(multiplier)
    ) <= 0
  }

  private[postgresql] def from(
      sourceRows: BigDecimal,
      candidateRows: BigDecimal
  ): PostgreSqlExplainRowGuardEvidence = {
    require(sourceRows != null && sourceRows.signum >= 0)
    require(candidateRows != null && candidateRows.signum >= 0)
    val denominator = sourceRows.max(BigDecimal(1))
    val numerator = candidateRows
    val within = exactWithin(numerator, denominator, Factor)
    PostgreSqlExplainRowGuardEvidence(
      Factor,
      numerator,
      denominator,
      within
    )
  }
}

/** Sanitized baseline estimate for the original validated SELECT statement. */
final case class PostgreSqlExplainSourceEvaluation(
    sourceStatementSha256: String,
    estimate: PostgreSqlExplainCostEstimate
) extends Serializable {
  require(
    sourceStatementSha256 != null &&
      sourceStatementSha256.matches("[0-9a-f]{64}"),
    "source statement digest must be lowercase SHA-256"
  )
  require(estimate != null, "source optimizer estimate must not be null")
}

final case class PostgreSqlExplainSourceStatement private[postgresql] (
    sourceStatementSha256: String,
    sql: String
) {
  require(
    sourceStatementSha256 != null &&
      sourceStatementSha256.matches("[0-9a-f]{64}"),
    "source statement digest must be lowercase SHA-256"
  )
  require(sql != null && sql.nonEmpty, "source SQL must not be empty")
  require(
    sourceStatementSha256 ==
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql),
    "source statement digest must match source SQL"
  )
}

/** Immutable internal request for one already rendered physical variant. */
final case class PostgreSqlExplainCandidateStatement private[postgresql] (
    candidateId: CandidatePlanId,
    structuralRank: Int,
    reductionMode: YannakakisReductionMode,
    physicalVariantId: PostgreSqlPhysicalVariantId,
    renderedStatementSha256: String,
    sql: String
) {
  require(candidateId != null, "candidate id must not be null")
  require(structuralRank > 0, "candidate structural rank must be positive")
  require(reductionMode != null, "reduction mode must not be null")
  require(physicalVariantId != null, "physical variant id must not be null")
  require(
    physicalVariantId ==
      PostgreSqlPhysicalVariantId.from(candidateId, reductionMode),
    "physical variant id must match candidate and reduction mode"
  )
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}"),
    "rendered statement digest must be lowercase SHA-256"
  )
  require(sql != null && sql.nonEmpty, "rendered SQL must not be empty")
  require(
    renderedStatementSha256 ==
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql),
    "rendered statement digest must match rendered SQL"
  )
}

final case class PostgreSqlExplainCostBatchRequest private[postgresql] (
    source: PostgreSqlExplainSourceStatement,
    statements: Vector[PostgreSqlExplainCandidateStatement]
) {
  require(source != null, "EXPLAIN source statement must not be null")
  require(statements != null && statements.nonEmpty, "EXPLAIN batch must not be empty")
  require(statements.forall(_ != null), "EXPLAIN batch must not contain null")
  require(
    statements.map(_.structuralRank) ==
      statements.map(_.structuralRank).sorted.distinct,
    "EXPLAIN batch ranks must be unique and increasing"
  )
  require(
    statements.map(_.candidateId).distinct.size == statements.size,
    "EXPLAIN batch candidate ids must be unique"
  )
  require(
    statements.map(_.reductionMode).distinct.size == 1,
    "EXPLAIN batch must use one request-wide reduction mode"
  )
}

final case class PostgreSqlExplainCostObservation(
    candidateId: CandidatePlanId,
    structuralRank: Int,
    physicalVariantId: PostgreSqlPhysicalVariantId,
    renderedStatementSha256: String,
    estimate: PostgreSqlExplainCostEstimate
) {
  require(candidateId != null, "observed candidate id must not be null")
  require(structuralRank > 0, "observed structural rank must be positive")
  require(physicalVariantId != null, "observed physical id must not be null")
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}"),
    "observed statement digest must be lowercase SHA-256"
  )
  require(estimate != null, "observed estimate must not be null")
}

final case class PostgreSqlExplainCostBatch(
    provenance: PostgreSqlExplainCostProvenance,
    source: PostgreSqlExplainSourceEvaluation,
    observations: Vector[PostgreSqlExplainCostObservation]
) {
  require(provenance != null, "EXPLAIN batch provenance must not be null")
  require(source != null, "EXPLAIN source estimate must not be null")
  require(observations != null, "EXPLAIN observations must not be null")
  require(observations.forall(_ != null), "EXPLAIN observations must not contain null")
  require(
    source.estimate.provenance == provenance &&
      observations.forall(_.estimate.provenance == provenance),
    "EXPLAIN source and candidate observations must share one provenance value"
  )
}

sealed trait PostgreSqlExplainEvaluationStage
    extends Product
    with Serializable {
  def id: String
}

object PostgreSqlExplainEvaluationStage {
  case object Session extends PostgreSqlExplainEvaluationStage {
    override val id: String = "session"
  }
  case object Provenance extends PostgreSqlExplainEvaluationStage {
    override val id: String = "provenance"
  }
  case object SourceExplain extends PostgreSqlExplainEvaluationStage {
    override val id: String = "source-explain"
  }
  case object CandidateExplain extends PostgreSqlExplainEvaluationStage {
    override val id: String = "candidate-explain"
  }
  case object StageExplain extends PostgreSqlExplainEvaluationStage {
    override val id: String = "stage-explain"
  }
  case object PrefixVerification extends PostgreSqlExplainEvaluationStage {
    override val id: String = "prefix-verification"
  }
  case object CandidatePlanning extends PostgreSqlExplainEvaluationStage {
    override val id: String = "candidate-planning"
  }
  case object StageDiagnosticRendering extends PostgreSqlExplainEvaluationStage {
    override val id: String = "stage-diagnostic-rendering"
  }
  case object ResourceValidation extends PostgreSqlExplainEvaluationStage {
    override val id: String = "resource-validation"
  }
}

sealed trait PostgreSqlExplainCostFailureCode
    extends Product
    with Serializable {
  def id: String
}

object PostgreSqlExplainCostFailureCode {
  case object EvaluatorUnavailable extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.evaluator-unavailable"
  }
  case object ConnectionUnavailable extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.connection-unavailable"
  }
  case object UnsafeRenderedStatement extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.unsafe-rendered-statement"
  }
  case object SessionConfigurationFailed
      extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.session-configuration-failed"
  }
  case object ProvenanceUnavailable extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.provenance-unavailable"
  }
  case object StatementTimedOut extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.statement-timeout"
  }
  case object StatementFailed extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.statement-failed"
  }
  case object PlanJsonOversized extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.plan-json-oversized"
  }
  case object PlanJsonMalformed extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.plan-json-malformed"
  }
  case object ProvenanceChanged extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.provenance-changed"
  }
  case object CleanupFailed extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.cleanup-failed"
  }
  case object EvaluatorContractViolation
      extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.evaluator-contract-violation"
  }
  case object PrefixVerificationFailed
      extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.factor-aware-prefix-verification-failed"
  }
  case object SessionResourceLimitExceeded
      extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.session-resource-limit-exceeded"
  }
  case object FactorCandidatePlanningFailed
      extends PostgreSqlExplainCostFailureCode {
    override val id: String = "explain.factor-aware-candidate-planning-failed"
  }
  case object FactorStageDiagnosticRenderingFailed
      extends PostgreSqlExplainCostFailureCode {
    override val id: String =
      "explain.factor-aware-stage-diagnostic-rendering-failed"
  }
}

final case class PostgreSqlExplainCostEvaluationFailure(
    code: PostgreSqlExplainCostFailureCode,
    stage: PostgreSqlExplainEvaluationStage,
    failedCandidateIndex: Option[Int],
    source: Option[PostgreSqlExplainSourceEvaluation],
    completed: Vector[PostgreSqlExplainCostObservation]
) {
  require(code != null, "EXPLAIN failure code must not be null")
  require(stage != null, "EXPLAIN failure stage must not be null")
  require(
    failedCandidateIndex != null && failedCandidateIndex.forall(_ >= 0),
    "failed candidate index option must be nonnegative"
  )
  require(source != null && source.forall(_ != null), "source evidence option must be valid")
  require(completed != null, "completed EXPLAIN prefix must not be null")
  require(completed.forall(_ != null), "completed EXPLAIN prefix must not contain null")
  require(
    stage == PostgreSqlExplainEvaluationStage.CandidateExplain ==
      failedCandidateIndex.nonEmpty,
    "only candidate EXPLAIN failures may identify a failed candidate"
  )
  require(
    failedCandidateIndex.forall(completed.size == _),
    "completed EXPLAIN observations must be the exact prefix before the failed candidate"
  )
  require(
    failedCandidateIndex.isEmpty || source.nonEmpty,
    "candidate EXPLAIN failure requires a completed source estimate"
  )
  require(
    source.nonEmpty || completed.isEmpty,
    "candidate observations require a completed source estimate"
  )
}
