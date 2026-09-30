package at.ac.tuwien.dbai.yannakakis.cli

import scala.util.control.NonFatal

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateDecompositionValidator,
  CandidatePlanDecision,
  CandidatePlanId,
  CandidateSearchInput,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.api.{
  DiagnosticSeverity,
  RewriteDiagnostic,
  RewriteReasonCode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchResult,
  PostgreSqlEnumerationBatchStatus,
  PostgreSqlRewritePhase
}

/**
  * Additive, outcome-free disposition of one exact campaign-v2 producer
  * result. Eligible values are the existing frozen packageability ADT; this
  * boundary adds only typed terminal failures.
  */
private[cli] sealed trait PostgreSqlEnumerationAcquisitionDisposition
    extends Product
    with Serializable

private[cli] object PostgreSqlEnumerationAcquisitionDisposition {
  final case class Eligible(
      value: PostgreSqlEnumerationCampaignV2EligibleResult
  ) extends PostgreSqlEnumerationAcquisitionDisposition {
    require(value != null, "eligible acquisition value must not be null")
  }

  final case class Unpackageable private[cli] (
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult,
      terminalClass: PostgreSqlEnumerationAcquisitionTerminalClass,
      diagnostics: Vector[PostgreSqlEnumerationAcquisitionTerminalDiagnostic],
      resourceOrigins: Vector[PostgreSqlEnumerationAcquisitionResourceOrigin],
      producerSearchContract: PostgreSqlEnumerationCampaignV2JsonDocument
  ) extends PostgreSqlEnumerationAcquisitionDisposition {
    require(authoring != null, "terminal authoring request must not be null")
    require(result != null, "terminal producer result must not be null")
    require(terminalClass != null, "terminal class must not be null")
    require(
      diagnostics != null && diagnostics.nonEmpty && diagnostics.forall(_ != null),
      "terminal diagnostics must be nonempty and contain no null"
    )
    require(
      resourceOrigins != null && resourceOrigins.forall(_ != null) &&
        resourceOrigins.distinct == resourceOrigins &&
        resourceOrigins.sortBy(_.id) == resourceOrigins,
      "terminal resource origins must be unique and canonical"
    )
    require(
      producerSearchContract != null,
      "terminal producer search contract must not be null"
    )
    require(
      result.request == authoring.batchRequest && result.artifact.isEmpty,
      "terminal result must retain the exact authoring request and no artifact"
    )
    require(
      result.status == terminalClass.producerStatus,
      "terminal class must agree with the producer status"
    )
    require(
      (terminalClass == PostgreSqlEnumerationAcquisitionTerminalClass.ResourceLimited) ==
        resourceOrigins.nonEmpty,
      "only resource-limited dispositions may retain resource origins"
    )
  }
}

/** Closed terminal classes admitted by the disposition protocol. */
private[cli] sealed trait PostgreSqlEnumerationAcquisitionTerminalClass
    extends Product
    with Serializable {
  def id: String
  def producerStatus: PostgreSqlEnumerationBatchStatus
}

private[cli] object PostgreSqlEnumerationAcquisitionTerminalClass {
  case object InvalidQuery
      extends PostgreSqlEnumerationAcquisitionTerminalClass {
    override val id: String = "invalid-query"
    override val producerStatus: PostgreSqlEnumerationBatchStatus =
      PostgreSqlEnumerationBatchStatus.InvalidQuery
  }

  case object Unsupported
      extends PostgreSqlEnumerationAcquisitionTerminalClass {
    override val id: String = "unsupported"
    override val producerStatus: PostgreSqlEnumerationBatchStatus =
      PostgreSqlEnumerationBatchStatus.NotApplicable
  }

  case object OtherNotApplicable
      extends PostgreSqlEnumerationAcquisitionTerminalClass {
    override val id: String = "other-not-applicable"
    override val producerStatus: PostgreSqlEnumerationBatchStatus =
      PostgreSqlEnumerationBatchStatus.NotApplicable
  }

  case object ResourceLimited
      extends PostgreSqlEnumerationAcquisitionTerminalClass {
    override val id: String = "resource-limited"
    override val producerStatus: PostgreSqlEnumerationBatchStatus =
      PostgreSqlEnumerationBatchStatus.Failed
  }

  case object OtherFailed
      extends PostgreSqlEnumerationAcquisitionTerminalClass {
    override val id: String = "other-failed"
    override val producerStatus: PostgreSqlEnumerationBatchStatus =
      PostgreSqlEnumerationBatchStatus.Failed
  }
}

/** Message- and context-free terminal diagnostic projection. */
private[cli] final case class PostgreSqlEnumerationAcquisitionTerminalDiagnostic(
    code: RewriteReasonCode,
    severity: DiagnosticSeverity
) extends Serializable {
  require(code != null, "terminal diagnostic code must not be null")
  require(severity != null, "terminal diagnostic severity must not be null")
}

/**
  * Closed origin labels backed by exact producer phase/code/severity, context
  * shape, and stable discriminants. Free-text `detail` is presence-only and
  * never becomes a classifier discriminator or receipt field.
  */
private[cli] sealed trait PostgreSqlEnumerationAcquisitionResourceOrigin
    extends Product
    with Serializable {
  def id: String
}

private[cli] object PostgreSqlEnumerationAcquisitionResourceOrigin {
  case object SoftBagGeneration
      extends PostgreSqlEnumerationAcquisitionResourceOrigin {
    override val id: String = "soft-bag-generation"
  }

  case object CandidateSearch
      extends PostgreSqlEnumerationAcquisitionResourceOrigin {
    override val id: String = "candidate-search"
  }

  case object BagFactorPlanning
      extends PostgreSqlEnumerationAcquisitionResourceOrigin {
    override val id: String = "bag-factor-planning"
  }

  case object PostgreSqlRendering
      extends PostgreSqlEnumerationAcquisitionResourceOrigin {
    override val id: String = "postgresql-rendering"
  }

  case object SqlTransport
      extends PostgreSqlEnumerationAcquisitionResourceOrigin {
    override val id: String = "sql-transport"
  }
}

/**
  * A rejected state is not a terminal disposition. In particular, malformed
  * native fallbacks and rejected Produced values block receipt sealing.
  */
private[cli] final case class PostgreSqlEnumerationAcquisitionProducerContractFailure(
    violation: PostgreSqlEnumerationAcquisitionProducerContractViolation,
    completedPhase: String
) extends Product
    with Serializable {
  require(violation != null, "producer-contract violation must not be null")
  require(
    completedPhase != null && completedPhase.nonEmpty,
    "producer-contract failure phase must not be empty"
  )

  val code: String =
    "enumeration-acquisition-disposition.producer-contract-failure"
}

private[cli] sealed trait PostgreSqlEnumerationAcquisitionProducerContractViolation
    extends Product
    with Serializable {
  def id: String
}

private[cli] object PostgreSqlEnumerationAcquisitionProducerContractViolation {
  case object AuthoringEntrypointMismatch
      extends PostgreSqlEnumerationAcquisitionProducerContractViolation {
    override val id: String = "authoring-entrypoint-mismatch"
  }

  case object AuthoringOrSearchContractMismatch
      extends PostgreSqlEnumerationAcquisitionProducerContractViolation {
    override val id: String = "authoring-or-search-contract-mismatch"
  }

  case object RejectedProduced
      extends PostgreSqlEnumerationAcquisitionProducerContractViolation {
    override val id: String = "rejected-produced"
  }

  case object RejectedNativeFallback
      extends PostgreSqlEnumerationAcquisitionProducerContractViolation {
    override val id: String = "rejected-native-fallback"
  }

  case object MalformedTerminalResult
      extends PostgreSqlEnumerationAcquisitionProducerContractViolation {
    override val id: String = "malformed-terminal-result"
  }

  case object ExistingBoundaryRejected
      extends PostgreSqlEnumerationAcquisitionProducerContractViolation {
    override val id: String = "existing-boundary-rejected"
  }
}

/** One pure, exact packageability/disposition decision for a producer result. */
private[cli] object PostgreSqlEnumerationAcquisitionDispositionClassifier {
  import PostgreSqlEnumerationAcquisitionDisposition._
  import PostgreSqlEnumerationAcquisitionProducerContractViolation._
  import PostgreSqlEnumerationAcquisitionResourceOrigin._
  import PostgreSqlEnumerationAcquisitionTerminalClass._

  private val UnsupportedCodes: Set[RewriteReasonCode] = Set(
    RewriteReasonCode.UnsupportedQueryShape,
    RewriteReasonCode.UnsupportedJoinType,
    RewriteReasonCode.UnsupportedPredicate,
    RewriteReasonCode.UnsupportedAggregate,
    RewriteReasonCode.UnresolvedLineage,
    RewriteReasonCode.UnsafeBagSemantics
  )

  private val FallbackCodes: Set[RewriteReasonCode] = Set(
    RewriteReasonCode.NoConnectedCover,
    RewriteReasonCode.NoDecomposition
  )

  private val AlgorithmContextKeys: Set[String] =
    Set("stage", "algorithmCode", "detail")
  private val RetainedCandidateContextKeys: Set[String] =
    AlgorithmContextKeys ++ Set("candidateRank", "candidateId")
  private val TransportContextKeys: Set[String] =
    Set("stage", "candidateRank", "candidateId", "assetId", "reason")
  private val CandidateIdPattern = "sha256:[0-9a-f]{64}".r
  private val PositiveDecimalPattern = "[1-9][0-9]*".r
  private val SafeExceptionClassPattern =
    "[A-Za-z_$][A-Za-z0-9_$]*(\\.[A-Za-z_$][A-Za-z0-9_$]*)*".r
  private val StageAssetPattern =
    "stage:(?:support:b(?:0|[1-9][0-9]*)|factor:b(?:0|[1-9][0-9]*):v(?:0|[1-9][0-9]*)|final-reconstruction)".r

  def classify(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult
  ): Either[
    PostgreSqlEnumerationAcquisitionProducerContractFailure,
    PostgreSqlEnumerationAcquisitionDisposition
  ] = {
    require(authoring != null, "disposition authoring request must not be null")
    require(result != null, "disposition producer result must not be null")

    val phase = result.completedPhase.id
    if (
      !authoring.hasProducerEntrypoint(
        PostgreSqlEnumerationSearchReplayV2AcquisitionCli.Entrypoint
      )
    ) Left(contractFailure(AuthoringEntrypointMismatch, phase))
    else {
      val producerContract =
        try {
          Right(
            PostgreSqlEnumerationCampaignV2Json
              .producerSearchContractDocumentForAcquisition(
                result.producerSearchContract,
                result.request.originalSqlSha256,
                result.request,
                authoring
              )
          )
        } catch {
          case _: IllegalArgumentException =>
            Left(contractFailure(AuthoringOrSearchContractMismatch, phase))
        }

      producerContract.flatMap { contract =>
        val existing =
          try {
            Right(
              PostgreSqlEnumerationCampaignV2Packageability
                .classify(authoring, result)
            )
          } catch {
            case _: IllegalArgumentException =>
              Left(contractFailure(AuthoringOrSearchContractMismatch, phase))
          }

        existing.flatMap {
          case Right(value) => Right(Eligible(value))
          case Left(
                _: PostgreSqlEnumerationCampaignV2AcquisitionFailure.AuthoringMismatch
              ) =>
            Left(contractFailure(AuthoringOrSearchContractMismatch, phase))
          case Left(
                _: PostgreSqlEnumerationCampaignV2AcquisitionFailure.UnpackageableResult
              ) if result.status == PostgreSqlEnumerationBatchStatus.Produced =>
            Left(contractFailure(RejectedProduced, phase))
          case Left(
                _: PostgreSqlEnumerationCampaignV2AcquisitionFailure.UnpackageableResult
              ) if result.diagnostics.exists(diagnostic =>
                FallbackCodes.contains(diagnostic.code)
              ) =>
            Left(contractFailure(RejectedNativeFallback, phase))
          case Left(
                _: PostgreSqlEnumerationCampaignV2AcquisitionFailure.UnpackageableResult
              ) =>
            classifyTerminal(authoring, result, contract)
          case Left(_) =>
            Left(contractFailure(ExistingBoundaryRejected, phase))
        }
      }
    }
  }

  private def classifyTerminal(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult,
      producerContract: PostgreSqlEnumerationCampaignV2JsonDocument
  ): Either[
    PostgreSqlEnumerationAcquisitionProducerContractFailure,
    PostgreSqlEnumerationAcquisitionDisposition
  ] = {
    val phase = result.completedPhase.id
    if (
      result.request != authoring.batchRequest ||
      result.artifact.nonEmpty ||
      result.diagnostics.isEmpty ||
      !reachableTerminalPrefix(result) ||
      !reachableStatusPhase(result)
    ) Left(contractFailure(MalformedTerminalResult, phase))
    else {
      val terminal = result.status match {
        case PostgreSqlEnumerationBatchStatus.InvalidQuery
            if exactInvalidQuery(result) =>
          Some(InvalidQuery -> Vector.empty[PostgreSqlEnumerationAcquisitionResourceOrigin])
        case PostgreSqlEnumerationBatchStatus.NotApplicable
            if result.diagnostics.forall(
              _.severity != DiagnosticSeverity.Error
            ) =>
          val unsupported = result.diagnostics.forall(diagnostic =>
            diagnostic.severity == DiagnosticSeverity.Warning &&
              UnsupportedCodes.contains(diagnostic.code)
          )
          Some(
            (if (unsupported) Unsupported else OtherNotApplicable) ->
              Vector.empty[PostgreSqlEnumerationAcquisitionResourceOrigin]
          )
        case PostgreSqlEnumerationBatchStatus.Failed
            if result.diagnostics.exists(
              _.severity == DiagnosticSeverity.Error
            ) =>
          val origins = resourceOrigins(authoring, result)
          Some((if (origins.nonEmpty) ResourceLimited else OtherFailed) -> origins)
        case _ => None
      }

      terminal match {
        case None =>
          Left(contractFailure(MalformedTerminalResult, phase))
        case Some((terminalClass, origins)) =>
          val diagnostics = result.diagnostics.map(diagnostic =>
            PostgreSqlEnumerationAcquisitionTerminalDiagnostic(
              diagnostic.code,
              diagnostic.severity
            )
          )
          Right(
            Unpackageable(
              authoring,
              result,
              terminalClass,
              diagnostics,
              origins,
              producerContract
            )
          )
      }
    }
  }

  private def exactInvalidQuery(
      result: PostgreSqlEnumerationBatchResult
  ): Boolean = {
    val allowedContextKeys = Set("stage", "exceptionClass")
    result.completedPhase == PostgreSqlRewritePhase.RequestAccepted &&
    result.diagnostics.size == 1 && {
      val diagnostic = result.diagnostics.head
      diagnostic.code == RewriteReasonCode.InvalidSql &&
      diagnostic.severity == DiagnosticSeverity.Error &&
      diagnostic.context.keySet.subsetOf(allowedContextKeys) &&
      diagnostic.context.get("stage").exists(stage =>
        stage == "parsing" || stage == "validation"
      ) &&
      diagnostic.context.get("exceptionClass").forall(value =>
        SafeExceptionClassPattern.pattern.matcher(value).matches()
      )
    }
  }

  /** Exact option-presence prefixes reachable from produceEnumerationBatch. */
  private def reachableTerminalPrefix(
      result: PostgreSqlEnumerationBatchResult
  ): Boolean = {
    val report = result.pipelineEvidence
    val enumerationOnly =
      report.completedPhase == result.completedPhase &&
        report.candidateSelectionDecision.isEmpty &&
        report.artifact.isEmpty &&
        report.physicalPortfolioAttempt.isEmpty &&
        report.physicalPortfolioArtifact.isEmpty
    if (!enumerationOnly) false
    else {
      val presence = (
        report.analysis.nonEmpty,
        report.hypergraph.nonEmpty,
        report.softGeneration.nonEmpty,
        report.admissibleGeneration.nonEmpty,
        report.candidateInput.nonEmpty,
        report.candidateDecision.nonEmpty,
        report.bagFactorPlan.nonEmpty
      )
      result.completedPhase match {
        case PostgreSqlRewritePhase.RequestAccepted |
            PostgreSqlRewritePhase.SqlPlanned =>
          presence == ((false, false, false, false, false, false, false))
        case PostgreSqlRewritePhase.QueryAnalyzed =>
          presence == ((true, false, false, false, false, false, false))
        case PostgreSqlRewritePhase.HypergraphBuilt =>
          presence == ((true, true, false, false, false, false, false))
        case PostgreSqlRewritePhase.SoftBagsGenerated =>
          presence == ((true, true, true, true, false, false, false))
        case PostgreSqlRewritePhase.CandidateInputBuilt =>
          presence == ((true, true, true, true, true, false, false))
        case PostgreSqlRewritePhase.CandidateSearchCompleted =>
          presence == ((true, true, true, true, true, true, false))
        case PostgreSqlRewritePhase.BagFactorPlanned =>
          presence == ((true, true, true, true, true, true, true))
        case PostgreSqlRewritePhase.Rendered =>
          presence == ((true, true, true, true, true, true, false))
        case PostgreSqlRewritePhase.CandidateSelectionCompleted => false
      }
    }
  }

  private def reachableStatusPhase(
      result: PostgreSqlEnumerationBatchResult
  ): Boolean =
    result.status match {
      case PostgreSqlEnumerationBatchStatus.InvalidQuery =>
        result.completedPhase == PostgreSqlRewritePhase.RequestAccepted
      case PostgreSqlEnumerationBatchStatus.NotApplicable =>
        result.completedPhase == PostgreSqlRewritePhase.QueryAnalyzed ||
          result.completedPhase == PostgreSqlRewritePhase.HypergraphBuilt ||
          result.completedPhase ==
            PostgreSqlRewritePhase.CandidateSearchCompleted ||
          result.completedPhase == PostgreSqlRewritePhase.BagFactorPlanned
      case PostgreSqlEnumerationBatchStatus.Failed =>
        result.completedPhase !=
          PostgreSqlRewritePhase.CandidateSelectionCompleted
      case PostgreSqlEnumerationBatchStatus.Produced => false
    }

  /**
    * Resource-limited is intentionally all-or-nothing. Every diagnostic must
    * match the same reachable phase/code/severity, exact context shape, and
    * stable classifier bindings. Free-text detail is presence-only; mixed,
    * missing, or lookalike classifier evidence remains OtherFailed.
    */
  private def resourceOrigins(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult
  ): Vector[PostgreSqlEnumerationAcquisitionResourceOrigin] = {
    if (result.status != PostgreSqlEnumerationBatchStatus.Failed) Vector.empty
    else {
      val matching = Vector(
        SoftBagGeneration,
        CandidateSearch,
        BagFactorPlanning,
        PostgreSqlRendering,
        SqlTransport
      ).filter(origin => matchesResourceResult(authoring, result, origin))
      if (matching.size == 1) matching.sortBy(_.id) else Vector.empty
    }
  }

  private def matchesResourceResult(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult,
      origin: PostgreSqlEnumerationAcquisitionResourceOrigin
  ): Boolean = {
    val diagnostics = result.diagnostics
    val exactDiagnostics =
      diagnostics.nonEmpty && diagnostics.distinct == diagnostics &&
        diagnostics.forall(diagnostic =>
          matchesResourceTuple(result, diagnostic, origin)
        )
    val exactStructuralPrefix = origin match {
      case SoftBagGeneration =>
        PostgreSqlEnumerationCampaignV2Packageability
          .hasExactBoundHypergraph(authoring, result)
      case CandidateSearch | BagFactorPlanning | PostgreSqlRendering |
          SqlTransport =>
        PostgreSqlEnumerationCampaignV2Packageability
          .hasExactBoundCandidateInput(authoring, result)
    }
    if (!exactDiagnostics || !exactStructuralPrefix) false
    else
      origin match {
        case SoftBagGeneration | CandidateSearch | SqlTransport =>
          diagnostics.size == 1 &&
            (origin != SqlTransport || retainedCandidateBound(result))
        case BagFactorPlanning | PostgreSqlRendering =>
          retainedCandidateBound(result)
      }
  }

  private def retainedCandidateBound(
      result: PostgreSqlEnumerationBatchResult
  ): Boolean = {
    val pairs = result.diagnostics.flatMap(diagnostic =>
      canonicalCandidatePair(diagnostic.context, result.request.planLimit)
    )
    pairs.size == result.diagnostics.size && pairs.distinct.size == 1 &&
    result.pipelineEvidence.candidateDecision.exists {
      case found: CandidatePlanDecision.Found =>
        Option(found.plans).exists { plans =>
          result.pipelineEvidence.candidateInput.exists { candidateInput =>
            val (rank, candidateId) = pairs.head
            val matching = plans.filter(plan =>
              plan != null && plan.candidateId != null &&
                plan.rank == rank && plan.candidateId.value == candidateId
            )
            found.requestedLimit == result.request.planLimit &&
            plans.nonEmpty && plans.size <= found.requestedLimit &&
            plans.forall(plan =>
              exactRetainedPlan(result, candidateInput, plan)
            ) &&
            plans.map(_.rank) == (1 to plans.size).toVector &&
            plans.map(_.candidateId).distinct.size == plans.size &&
            matching.size == 1 &&
            result.pipelineEvidence.bagFactorPlan.forall { bagFactorPlan =>
              val retained = matching.head
              bagFactorPlan != null &&
              bagFactorPlan.decomposition == retained.decomposition &&
              bagFactorPlan.certificate == retained.certificate &&
              result.pipelineEvidence.hypergraph.contains(
                bagFactorPlan.sourceHypergraph
              ) &&
              bagFactorPlan.sourceWidthBound == result.request.widthBound &&
              bagFactorPlan.requireConnectedCover ==
                result.request.requireConnectedCover
            }
          }
        }
      case _ => false
    }
  }

  private def exactRetainedPlan(
      result: PostgreSqlEnumerationBatchResult,
      candidateInput: CandidateSearchInput,
      plan: RankedCandidatePlan
  ): Boolean =
    if (
      candidateInput == null || plan == null || plan.candidateId == null ||
      plan.score == null || plan.witness == null || plan.decomposition == null ||
      plan.certificate == null
    ) false
    else
      try {
        plan.candidateId == CandidatePlanId.from(plan.decomposition) &&
        plan.score.nodeCount == plan.witness.nodeCount &&
        plan.certificate.requireConnectedCover ==
          result.request.requireConnectedCover &&
        CandidateDecompositionValidator
          .validate(candidateInput, plan.decomposition)
          .toOption
          .contains(plan.certificate)
      } catch {
        case NonFatal(_) => false
      }

  private def matchesResourceTuple(
      result: PostgreSqlEnumerationBatchResult,
      diagnostic: RewriteDiagnostic,
      origin: PostgreSqlEnumerationAcquisitionResourceOrigin
  ): Boolean = {
    if (diagnostic.severity != DiagnosticSeverity.Error) false
    else
      origin match {
        case SoftBagGeneration =>
          result.completedPhase == PostgreSqlRewritePhase.HypergraphBuilt &&
            diagnostic.code == RewriteReasonCode.PlanningFailed &&
            exactAlgorithmContext(
              diagnostic.context,
              "soft-bag-generation",
              "resource-limit-exceeded",
              retainedCandidate = false,
              result.request.planLimit
            )
        case CandidateSearch =>
          result.completedPhase == PostgreSqlRewritePhase.CandidateInputBuilt &&
            diagnostic.code == RewriteReasonCode.PlanningFailed &&
            exactAlgorithmContext(
              diagnostic.context,
              "candidate-search",
              "candidate.failure.resource-limit.exceeded",
              retainedCandidate = false,
              result.request.planLimit
            )
        case BagFactorPlanning =>
          result.completedPhase ==
            PostgreSqlRewritePhase.CandidateSearchCompleted &&
            diagnostic.code == RewriteReasonCode.PlanningFailed &&
            exactAlgorithmContext(
              diagnostic.context,
              "enumeration-batch",
              "lowering.resource.exceeded",
              retainedCandidate = true,
              result.request.planLimit
            )
        case PostgreSqlRendering =>
          result.completedPhase == PostgreSqlRewritePhase.BagFactorPlanned &&
            diagnostic.code == RewriteReasonCode.RenderingFailed &&
            exactAlgorithmContext(
              diagnostic.context,
              "enumeration-batch",
              "postgresql.rendering.resource.exceeded",
              retainedCandidate = true,
              result.request.planLimit
            )
        case SqlTransport =>
          result.completedPhase ==
            PostgreSqlRewritePhase.CandidateSearchCompleted &&
            diagnostic.code == RewriteReasonCode.RenderingFailed &&
            exactTransportContext(
              diagnostic.context,
              result.request.planLimit
            )
      }
  }

  private def exactAlgorithmContext(
      context: Map[String, String],
      stage: String,
      algorithmCode: String,
      retainedCandidate: Boolean,
      planLimit: Int
  ): Boolean = {
    val expectedKeys =
      if (retainedCandidate) RetainedCandidateContextKeys
      else AlgorithmContextKeys
    context.keySet == expectedKeys &&
    context.get("stage").contains(stage) &&
    context.get("algorithmCode").contains(algorithmCode) &&
    context.get("detail").exists(_.nonEmpty) &&
    (!retainedCandidate || exactCandidateContext(context, planLimit))
  }

  private def exactTransportContext(
      context: Map[String, String],
      planLimit: Int
  ): Boolean =
    context.keySet == TransportContextKeys &&
      context.get("stage").contains("enumeration-batch-transport") &&
      context.get("reason").contains("utf8-byte-limit-exceeded") &&
      exactCandidateContext(context, planLimit) &&
      context.get("assetId").exists(exactTransportAssetId)

  private def exactCandidateContext(
      context: Map[String, String],
      planLimit: Int
  ): Boolean = canonicalCandidatePair(context, planLimit).nonEmpty

  private def canonicalCandidatePair(
      context: Map[String, String],
      planLimit: Int
  ): Option[(Int, String)] =
    for {
      rankText <- context.get("candidateRank")
      if PositiveDecimalPattern.pattern.matcher(rankText).matches()
      rankValue <- rankText.toIntOption
      if rankValue > 0 && rankValue <= planLimit && rankText == rankValue.toString
      candidateId <- context.get("candidateId")
      if CandidateIdPattern.pattern.matcher(candidateId).matches()
    } yield rankValue -> candidateId

  private def exactTransportAssetId(value: String): Boolean =
    value == "rendered-sql" ||
      (value != null && value.length <= 96 &&
        StageAssetPattern.pattern.matcher(value).matches())

  private def contractFailure(
      violation: PostgreSqlEnumerationAcquisitionProducerContractViolation,
      phase: String
  ): PostgreSqlEnumerationAcquisitionProducerContractFailure =
    PostgreSqlEnumerationAcquisitionProducerContractFailure(violation, phase)
}
