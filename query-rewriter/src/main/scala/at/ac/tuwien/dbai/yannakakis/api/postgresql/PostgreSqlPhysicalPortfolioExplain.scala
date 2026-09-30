package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId

/** One exact emitted diagnostic statement in a portfolio evaluator request. */
final case class PostgreSqlPhysicalPortfolioExplainStageStatement private[postgresql] (
    stageId: PostgreSqlStageId,
    statementSha256: String,
    characterCount: Long,
    ctePrefixCount: Long,
    sql: String
) extends Serializable {
  require(stageId != null)
  require(statementSha256 != null && statementSha256.matches("[0-9a-f]{64}"))
  require(characterCount >= 0L)
  require(ctePrefixCount >= 0L)
  require(sql != null && sql.nonEmpty)
  require(characterCount == sql.length.toLong)
  require(
    statementSha256 ==
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql)
  )

  private[postgresql] def identity
      : PostgreSqlPhysicalPortfolioStageWorkloadIdentity =
    PostgreSqlPhysicalPortfolioStageWorkloadIdentity(
      stageId,
      statementSha256,
      characterCount,
      ctePrefixCount
    )
}

/** One unique, byte-verified workload and its minimum-id representative. */
final case class PostgreSqlPhysicalPortfolioExplainWorkload private[postgresql] (
    group: PostgreSqlPhysicalPortfolioWorkloadGroupEvidence,
    stages: Vector[PostgreSqlPhysicalPortfolioExplainStageStatement]
) extends Serializable {
  require(group != null)
  require(stages != null && stages.nonEmpty && stages.forall(_ != null))
  require(stages.map(_.stageId).distinct.size == stages.size)
  require(stages.map(_.identity) == group.descriptor.stages)
  require(stages.last.stageId == PostgreSqlStageId.FinalReconstruction)
}

/** Immutable request for one same-session source plus all unique workloads. */
final case class PostgreSqlPhysicalPortfolioExplainBatchRequest private[postgresql] (
    source: PostgreSqlExplainSourceStatement,
    workloads: Vector[PostgreSqlPhysicalPortfolioExplainWorkload],
    minimumServerVersionNum: Int,
    materializationScope: String
) extends Serializable {
  require(source != null)
  require(workloads != null && workloads.nonEmpty && workloads.forall(_ != null))
  require(
    workloads.size <=
      PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxWorkloads
  )
  require(
    workloads.map(_.group.descriptor.workloadId).distinct.size == workloads.size
  )
  require(
    workloads.map(_.group.representative.physicalVariantId).distinct.size ==
      workloads.size
  )
  require(
    workloads.map(_.group.earliestEnumerationOrdinal) ==
      workloads.map(_.group.earliestEnumerationOrdinal).sorted
  )
  require(
    minimumServerVersionNum ==
      PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum
  )
  require(materializationScope == PostgreSqlCteMaterializationDirective.Scope)

  private[postgresql] val flattened: Vector[
    (
        PostgreSqlPhysicalPortfolioExplainWorkload,
        PostgreSqlPhysicalPortfolioExplainStageStatement
    )
  ] = workloads.flatMap(workload => workload.stages.map(workload -> _))
}

/** Exact BigInt resource usage before the first optimizer observation. */
final case class PostgreSqlPhysicalPortfolioEvaluationUsage private[postgresql] (
    workloadCount: BigInt,
    explainStatementCount: BigInt,
    diagnosticCharacterCount: BigInt
) extends Serializable {
  require(workloadCount > 0)
  require(explainStatementCount > 1)
  require(diagnosticCharacterCount > 0)
}

final case class PostgreSqlPhysicalPortfolioEvaluationResourceExceeded private[postgresql] (
    resource: PostgreSqlPhysicalPortfolioEvaluationResource,
    workloadId: Option[PostgreSqlPhysicalPortfolioWorkloadId],
    required: BigInt,
    limit: Long
) extends Serializable {
  require(resource != null)
  require(workloadId != null && workloadId.forall(_ != null))
  require(required > BigInt(limit))
  require(limit > 0L)
}

/** Pure, overflow-free request-envelope validation shared by replay/evaluator. */
object PostgreSqlPhysicalPortfolioEvaluationEnvelope {
  def validate(
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits
  ): Either[
    PostgreSqlPhysicalPortfolioEvaluationResourceExceeded,
    PostgreSqlPhysicalPortfolioEvaluationUsage
  ] = {
    import PostgreSqlPhysicalPortfolioEvaluationResource._
    require(request != null)
    require(limits != null)
    val limitProblems = PostgreSqlPhysicalPortfolioEvaluationLimits.validate(limits)
    require(limitProblems.isEmpty, "portfolio evaluation limits must be valid")

    def exceeded(
        resource: PostgreSqlPhysicalPortfolioEvaluationResource,
        workloadId: Option[PostgreSqlPhysicalPortfolioWorkloadId],
        required: BigInt,
        limit: Long
    ): Left[
      PostgreSqlPhysicalPortfolioEvaluationResourceExceeded,
      PostgreSqlPhysicalPortfolioEvaluationUsage
    ] = Left(
      PostgreSqlPhysicalPortfolioEvaluationResourceExceeded(
        resource,
        workloadId,
        required,
        limit
      )
    )

    val workloadCount = BigInt(request.workloads.size)
    if (workloadCount > BigInt(limits.maxWorkloads))
      return exceeded(Workloads, None, workloadCount, limits.maxWorkloads)

    var totalStatements = BigInt(1)
    var totalCharacters = BigInt(0)
    request.workloads.foreach { workload =>
      val workloadId = Some(workload.group.descriptor.workloadId)
      val statements = BigInt(workload.stages.size)
      if (statements > BigInt(limits.maxStageProbesPerWorkload))
        return exceeded(
          StageProbesPerWorkload,
          workloadId,
          statements,
          limits.maxStageProbesPerWorkload
        )
      var workloadCharacters = BigInt(0)
      workload.stages.foreach { statement =>
        val characters = BigInt(statement.sql.length)
        if (characters > BigInt(limits.maxCharactersPerProbe))
          return exceeded(
            CharactersPerProbe,
            workloadId,
            characters,
            limits.maxCharactersPerProbe
          )
        workloadCharacters += characters
      }
      if (
        workloadCharacters > BigInt(limits.maxDiagnosticCharactersPerWorkload)
      )
        return exceeded(
          DiagnosticCharactersPerWorkload,
          workloadId,
          workloadCharacters,
          limits.maxDiagnosticCharactersPerWorkload
        )
      totalStatements += statements
      totalCharacters += workloadCharacters
      if (totalStatements > BigInt(limits.maxExplainStatementsPerSession))
        return exceeded(
          ExplainStatementsPerSession,
          workloadId,
          totalStatements,
          limits.maxExplainStatementsPerSession
        )
      if (totalCharacters > BigInt(limits.maxDiagnosticCharactersPerSession))
        return exceeded(
          DiagnosticCharactersPerSession,
          workloadId,
          totalCharacters,
          limits.maxDiagnosticCharactersPerSession
        )
    }
    Right(
      PostgreSqlPhysicalPortfolioEvaluationUsage(
        workloadCount,
        totalStatements,
        totalCharacters
      )
    )
  }
}

sealed trait PostgreSqlPhysicalPortfolioExplainFailureCode
    extends Product
    with Serializable {
  def id: String
}

object PostgreSqlPhysicalPortfolioExplainFailureCode {
  case object EvaluatorUnavailable
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.evaluator-unavailable"
  }
  case object ConnectionUnavailable
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.connection-unavailable"
  }
  case object UnsafeRenderedStatement
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.unsafe-rendered-statement"
  }
  case object SessionConfigurationFailed
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.session-configuration-failed"
  }
  case object ProvenanceUnavailable
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.provenance-unavailable"
  }
  case object UnsupportedServerVersion
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.physical-portfolio-unsupported-server-version"
  }
  case object StatementTimedOut
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.statement-timeout"
  }
  case object StatementFailed
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.statement-failed"
  }
  case object PlanJsonOversized
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.plan-json-oversized"
  }
  case object PlanJsonMalformed
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.plan-json-malformed"
  }
  case object ProvenanceChanged
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.provenance-changed"
  }
  case object CleanupFailed
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.cleanup-failed"
  }
  case object EvaluatorContractViolation
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.evaluator-contract-violation"
  }
  case object SessionResourceLimitExceeded
      extends PostgreSqlPhysicalPortfolioExplainFailureCode {
    override val id: String = "explain.session-resource-limit-exceeded"
  }

  private[postgresql] def fromLegacy(
      code: PostgreSqlExplainCostFailureCode
  ): PostgreSqlPhysicalPortfolioExplainFailureCode = code match {
    case PostgreSqlExplainCostFailureCode.EvaluatorUnavailable => EvaluatorUnavailable
    case PostgreSqlExplainCostFailureCode.ConnectionUnavailable => ConnectionUnavailable
    case PostgreSqlExplainCostFailureCode.UnsafeRenderedStatement => UnsafeRenderedStatement
    case PostgreSqlExplainCostFailureCode.SessionConfigurationFailed =>
      SessionConfigurationFailed
    case PostgreSqlExplainCostFailureCode.ProvenanceUnavailable => ProvenanceUnavailable
    case PostgreSqlExplainCostFailureCode.StatementTimedOut => StatementTimedOut
    case PostgreSqlExplainCostFailureCode.StatementFailed => StatementFailed
    case PostgreSqlExplainCostFailureCode.PlanJsonOversized => PlanJsonOversized
    case PostgreSqlExplainCostFailureCode.PlanJsonMalformed => PlanJsonMalformed
    case PostgreSqlExplainCostFailureCode.ProvenanceChanged => ProvenanceChanged
    case PostgreSqlExplainCostFailureCode.CleanupFailed => CleanupFailed
    case PostgreSqlExplainCostFailureCode.EvaluatorContractViolation =>
      EvaluatorContractViolation
    case PostgreSqlExplainCostFailureCode.SessionResourceLimitExceeded =>
      SessionResourceLimitExceeded
    case PostgreSqlExplainCostFailureCode.PrefixVerificationFailed |
        PostgreSqlExplainCostFailureCode.FactorCandidatePlanningFailed |
        PostgreSqlExplainCostFailureCode.FactorStageDiagnosticRenderingFailed =>
      EvaluatorContractViolation
  }
}

sealed trait PostgreSqlPhysicalPortfolioExplainStage
    extends Product
    with Serializable {
  def id: String
}

object PostgreSqlPhysicalPortfolioExplainStage {
  case object Session extends PostgreSqlPhysicalPortfolioExplainStage {
    override val id: String = "session"
  }
  case object VersionPreflight extends PostgreSqlPhysicalPortfolioExplainStage {
    override val id: String = "version-preflight"
  }
  case object SourceExplain extends PostgreSqlPhysicalPortfolioExplainStage {
    override val id: String = "source-explain"
  }
  case object StageExplain extends PostgreSqlPhysicalPortfolioExplainStage {
    override val id: String = "stage-explain"
  }
  case object FinalProvenance extends PostgreSqlPhysicalPortfolioExplainStage {
    override val id: String = "final-provenance"
  }
}

object PostgreSqlPhysicalPortfolioExplainFailureMatrix {
  import PostgreSqlPhysicalPortfolioExplainFailureCode._
  import PostgreSqlPhysicalPortfolioExplainStage._

  def accepts(
      code: PostgreSqlPhysicalPortfolioExplainFailureCode,
      stage: PostgreSqlPhysicalPortfolioExplainStage
  ): Boolean =
    if (code == null || stage == null) false
    else
      stage match {
        case Session =>
          Set[PostgreSqlPhysicalPortfolioExplainFailureCode](
            EvaluatorUnavailable,
            ConnectionUnavailable,
            SessionConfigurationFailed,
            StatementTimedOut,
            CleanupFailed,
            EvaluatorContractViolation,
            SessionResourceLimitExceeded
          )(code)
        case VersionPreflight =>
          Set[PostgreSqlPhysicalPortfolioExplainFailureCode](
            ProvenanceUnavailable,
            UnsupportedServerVersion,
            StatementTimedOut,
            CleanupFailed
          )(code)
        case SourceExplain | StageExplain =>
          Set[PostgreSqlPhysicalPortfolioExplainFailureCode](
            UnsafeRenderedStatement,
            StatementTimedOut,
            StatementFailed,
            PlanJsonOversized,
            PlanJsonMalformed,
            CleanupFailed
          )(code)
        case FinalProvenance =>
          Set[PostgreSqlPhysicalPortfolioExplainFailureCode](
            ProvenanceUnavailable,
            UnsupportedServerVersion,
            ProvenanceChanged,
            StatementTimedOut,
            CleanupFailed
          )(code)
      }
}

final case class PostgreSqlPhysicalPortfolioExplainObservation(
    workloadId: PostgreSqlPhysicalPortfolioWorkloadId,
    representativePhysicalVariantId: PostgreSqlPhysicalVariantId,
    stageId: PostgreSqlStageId,
    statementSha256: String,
    estimate: PostgreSqlExplainCostEstimate
) extends Serializable {
  require(workloadId != null)
  require(representativePhysicalVariantId != null)
  require(stageId != null)
  require(statementSha256 != null && statementSha256.matches("[0-9a-f]{64}"))
  require(estimate != null)
  require(PostgreSqlFactorAwareNumericEnvelope.accepts(estimate))
}

final case class PostgreSqlPhysicalPortfolioExplainBatch(
    provenance: PostgreSqlExplainCostProvenance,
    source: PostgreSqlExplainSourceEvaluation,
    observations: Vector[PostgreSqlPhysicalPortfolioExplainObservation]
) extends Serializable {
  require(provenance != null)
  require(
    provenance.serverVersionNum >=
      PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum
  )
  require(source != null && source.estimate.provenance == provenance)
  require(PostgreSqlFactorAwareNumericEnvelope.accepts(source.estimate))
  require(observations != null && observations.forall(_ != null))
  require(observations.forall(_.estimate.provenance == provenance))
}

final case class PostgreSqlPhysicalPortfolioExplainFailure(
    code: PostgreSqlPhysicalPortfolioExplainFailureCode,
    stage: PostgreSqlPhysicalPortfolioExplainStage,
    failedStatementIndex: Option[Int],
    source: Option[PostgreSqlExplainSourceEvaluation],
    completed: Vector[PostgreSqlPhysicalPortfolioExplainObservation],
    requestedStageStatementCount: Int,
    observedServerVersionNum: Option[Int],
    requiredServerVersionNum: Option[Int]
) extends Serializable {
  import PostgreSqlPhysicalPortfolioExplainFailureCode.UnsupportedServerVersion
  import PostgreSqlPhysicalPortfolioExplainStage._

  require(code != null)
  require(stage != null)
  require(PostgreSqlPhysicalPortfolioExplainFailureMatrix.accepts(code, stage))
  require(failedStatementIndex != null && failedStatementIndex.forall(_ >= 0))
  require(source != null && source.forall(_ != null))
  require(completed != null && completed.forall(_ != null))
  require(requestedStageStatementCount > 0)
  require(observedServerVersionNum != null)
  require(requiredServerVersionNum != null)
  require((stage == StageExplain) == failedStatementIndex.nonEmpty)
  require(failedStatementIndex.forall(_ == completed.size))
  require(failedStatementIndex.forall(_ < requestedStageStatementCount))
  require(failedStatementIndex.isEmpty || source.nonEmpty)
  require(source.nonEmpty || completed.isEmpty)
  require(source.forall(value => PostgreSqlFactorAwareNumericEnvelope.accepts(value.estimate)))
  require(completed.forall(value => PostgreSqlFactorAwareNumericEnvelope.accepts(value.estimate)))
  require(
    source.forall(value =>
      completed.forall(_.estimate.provenance == value.estimate.provenance)
    )
  )
  require(
    stage != SourceExplain ||
      (failedStatementIndex.isEmpty && source.isEmpty && completed.isEmpty)
  )
  require(
    stage != VersionPreflight ||
      (failedStatementIndex.isEmpty && source.isEmpty && completed.isEmpty)
  )
  require(
    stage != FinalProvenance ||
      (failedStatementIndex.isEmpty && source.nonEmpty &&
        completed.size == requestedStageStatementCount)
  )
  require(
    stage != Session || code ==
      PostgreSqlPhysicalPortfolioExplainFailureCode.CleanupFailed ||
      (failedStatementIndex.isEmpty && source.isEmpty && completed.isEmpty)
  )
  require(
    observedServerVersionNum.isDefined == requiredServerVersionNum.isDefined,
    "portfolio version evidence must be wholly present or absent"
  )
  require(
    (code == UnsupportedServerVersion) ==
      (observedServerVersionNum.nonEmpty && requiredServerVersionNum.nonEmpty)
  )
  require(
    observedServerVersionNum.zip(requiredServerVersionNum).forall {
      case (observed, required) =>
        observed > 0 &&
          required ==
            PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum &&
          observed < required && (stage == VersionPreflight || stage == FinalProvenance)
    }
  )
}

private[postgresql] final case class PostgreSqlPhysicalPortfolioVerifiedBatch(
    source: PostgreSqlExplainSourceEvaluation,
    observationsByWorkload: Vector[
      Vector[PostgreSqlPhysicalPortfolioExplainObservation]
    ]
)

/** Pure one-session request/order/provenance replay for v6 evidence. */
object PostgreSqlPhysicalPortfolioExplainReplay {
  def verify(
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      batch: PostgreSqlPhysicalPortfolioExplainBatch
  ): Option[PostgreSqlPhysicalPortfolioVerifiedBatch] = {
    if (request == null || batch == null) return None
    if (
      batch.provenance.serverVersionNum < request.minimumServerVersionNum ||
      batch.source.sourceStatementSha256 != request.source.sourceStatementSha256 ||
      batch.source.estimate.provenance != batch.provenance ||
      !PostgreSqlFactorAwareNumericEnvelope.accepts(batch.source.estimate) ||
      batch.observations.size != request.flattened.size
    ) return None
    val matches = request.flattened.zip(batch.observations).forall {
      case ((workload, stage), observation) =>
        observation.workloadId == workload.group.descriptor.workloadId &&
          observation.representativePhysicalVariantId ==
            workload.group.representative.physicalVariantId &&
          observation.stageId == stage.stageId &&
          observation.statementSha256 == stage.statementSha256 &&
          observation.estimate.provenance == batch.provenance &&
          PostgreSqlFactorAwareNumericEnvelope.accepts(observation.estimate)
    }
    if (!matches) None
    else {
      var offset = 0
      val grouped = request.workloads.map { workload =>
        val next = batch.observations.slice(offset, offset + workload.stages.size)
        offset += workload.stages.size
        next
      }
      Some(PostgreSqlPhysicalPortfolioVerifiedBatch(batch.source, grouped))
    }
  }

  def verifyFailurePrefix(
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      failure: PostgreSqlPhysicalPortfolioExplainFailure
  ): Boolean = {
    if (request == null || failure == null) return false
    if (
      failure.requestedStageStatementCount != request.flattened.size ||
      failure.source.exists(
        _.sourceStatementSha256 != request.source.sourceStatementSha256
      ) || failure.completed.size > request.flattened.size
    ) return false
    val provenance = failure.source.map(_.estimate.provenance)
    if (
      provenance.exists(_.serverVersionNum < request.minimumServerVersionNum) ||
      failure.completed.exists(observation =>
        provenance.forall(_ != observation.estimate.provenance)
      )
    ) return false
    val prefixMatches = request.flattened
      .take(failure.completed.size)
      .zip(failure.completed)
      .forall { case ((workload, stage), observation) =>
        observation.workloadId == workload.group.descriptor.workloadId &&
          observation.representativePhysicalVariantId ==
            workload.group.representative.physicalVariantId &&
          observation.stageId == stage.stageId &&
          observation.statementSha256 == stage.statementSha256 &&
          PostgreSqlFactorAwareNumericEnvelope.accepts(observation.estimate)
      }
    val stageShape = failure.stage match {
      case PostgreSqlPhysicalPortfolioExplainStage.VersionPreflight =>
        failure.source.isEmpty && failure.completed.isEmpty
      case PostgreSqlPhysicalPortfolioExplainStage.SourceExplain =>
        failure.source.isEmpty && failure.completed.isEmpty
      case PostgreSqlPhysicalPortfolioExplainStage.StageExplain =>
        failure.source.nonEmpty &&
          failure.failedStatementIndex.contains(failure.completed.size)
      case PostgreSqlPhysicalPortfolioExplainStage.FinalProvenance =>
        failure.source.nonEmpty &&
          failure.completed.size == request.flattened.size
      case PostgreSqlPhysicalPortfolioExplainStage.Session => true
    }
    prefixMatches && stageShape
  }
}
