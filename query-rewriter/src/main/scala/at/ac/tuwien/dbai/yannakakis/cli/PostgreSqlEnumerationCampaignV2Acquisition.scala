package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidateSearchInput
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{
  AdmissibleSoftBagGeneration,
  SoftBagGeneration
}
import at.ac.tuwien.dbai.yannakakis.api.RewriteReasonCode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchArtifact,
  PostgreSqlEnumerationBatchResult,
  PostgreSqlEnumerationBatchStatus,
  PostgreSqlEnumerationCampaignV2ProjectionView,
  PostgreSqlRewritePhase
}
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  QueryResultShape
}

/** Closed packageability failures; diagnostic prose is never retained. */
private[yannakakis] sealed trait PostgreSqlEnumerationCampaignV2AcquisitionFailure
    extends Product
    with Serializable {
  def code: String
  def completedPhase: String
}

private[yannakakis] object PostgreSqlEnumerationCampaignV2AcquisitionFailure {
  final case class AuthoringMismatch(completedPhase: String)
      extends PostgreSqlEnumerationCampaignV2AcquisitionFailure {
    override val code: String =
      "enumeration-campaign-v2.acquisition.authoring-mismatch"
  }

  final case class UnpackageableResult(completedPhase: String)
      extends PostgreSqlEnumerationCampaignV2AcquisitionFailure {
    override val code: String =
      "enumeration-campaign-v2.acquisition.unpackageable-result"
  }

  final case class ProjectionRejected(completedPhase: String)
      extends PostgreSqlEnumerationCampaignV2AcquisitionFailure {
    override val code: String =
      "enumeration-campaign-v2.acquisition.projection-rejected"
  }

  final case class AssemblyRejected(completedPhase: String)
      extends PostgreSqlEnumerationCampaignV2AcquisitionFailure {
    override val code: String =
      "enumeration-campaign-v2.acquisition.assembly-rejected"
  }
}

/** Exact typed producer states admitted by the offline package boundary. */
private[cli] sealed trait PostgreSqlEnumerationCampaignV2EligibleResult
    extends Product
    with Serializable {
  def completedPhase: String
}

private[cli] object PostgreSqlEnumerationCampaignV2EligibleResult {
  final case class Found(artifact: PostgreSqlEnumerationBatchArtifact)
      extends PostgreSqlEnumerationCampaignV2EligibleResult {
    require(artifact != null, "eligible found artifact must not be null")
    override val completedPhase: String = PostgreSqlRewritePhase.Rendered.id
  }

  final case class NativeFallback(
      result: PostgreSqlEnumerationBatchResult,
      reason: PostgreSqlEnumerationCampaignV2NativeFallbackReason,
      softCounters: PostgreSqlEnumerationCampaignV2ProjectionView.SoftGenerationCounters,
      candidateCounters: Option[
        PostgreSqlEnumerationCampaignV2ProjectionView.CandidateSearchCounters
      ],
      detailPreimage: PostgreSqlEnumerationCampaignV2JsonDocument
  ) extends PostgreSqlEnumerationCampaignV2EligibleResult {
    require(result != null, "eligible fallback result must not be null")
    require(reason != null, "eligible fallback reason must not be null")
    require(softCounters != null, "eligible fallback soft counters must not be null")
    require(
      candidateCounters != null && candidateCounters.forall(_ != null),
      "eligible fallback candidate counters must not be null"
    )
    require(detailPreimage != null, "fallback detail preimage must not be null")
    require(
      reason.requiresCandidateSearch == candidateCounters.nonEmpty,
      "fallback candidate evidence must match its exact reason"
    )
    override val completedPhase: String = result.completedPhase.id
    val detailSha256: String = detailPreimage.sha256
  }
}

/** The only two native fallbacks executable under the frozen Python matrix. */
private[cli] sealed trait PostgreSqlEnumerationCampaignV2NativeFallbackReason
    extends Product
    with Serializable {
  def code: String
  def phase: String
  def completedPhase: PostgreSqlRewritePhase
  def diagnosticCode: RewriteReasonCode
  def requiresCandidateSearch: Boolean
}

private[cli] object PostgreSqlEnumerationCampaignV2NativeFallbackReason {
  case object NoConnectedCover
      extends PostgreSqlEnumerationCampaignV2NativeFallbackReason {
    override val code: String = "no-connected-cover"
    override val phase: String = "soft-generation"
    override val completedPhase: PostgreSqlRewritePhase =
      PostgreSqlRewritePhase.SoftBagsGenerated
    override val diagnosticCode: RewriteReasonCode =
      RewriteReasonCode.NoConnectedCover
    override val requiresCandidateSearch: Boolean = false
  }

  case object NoDecomposition
      extends PostgreSqlEnumerationCampaignV2NativeFallbackReason {
    override val code: String = "no-decomposition"
    override val phase: String = "candidate-search"
    override val completedPhase: PostgreSqlRewritePhase =
      PostgreSqlRewritePhase.CandidateSearchCompleted
    override val diagnosticCode: RewriteReasonCode =
      RewriteReasonCode.NoDecomposition
    override val requiresCandidateSearch: Boolean = true
  }
}

/** Pure typed classification before any package document is constructed. */
private[cli] object PostgreSqlEnumerationCampaignV2Packageability {
  import PostgreSqlEnumerationCampaignV2AcquisitionFailure._
  import PostgreSqlEnumerationCampaignV2EligibleResult._
  import PostgreSqlEnumerationCampaignV2NativeFallbackReason._

  private final case class BoundStructuralEvidence(
      analysis: AnalysisReport,
      hypergraph: QueryHypergraph,
      softGeneration: SoftBagGeneration,
      admissibleGeneration: AdmissibleSoftBagGeneration
  )

  private final case class BoundHypergraphEvidence(
      analysis: AnalysisReport,
      hypergraph: QueryHypergraph
  )

  def classify(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult
  ): Either[
    PostgreSqlEnumerationCampaignV2AcquisitionFailure,
    PostgreSqlEnumerationCampaignV2EligibleResult
  ] = {
    require(authoring != null, "authoring request must not be null")
    require(result != null, "enumeration result must not be null")
    val phase = result.completedPhase.id
    if (
      result.request != authoring.batchRequest ||
      result.producerSearchContract.widthBound !=
        authoring.batchRequest.widthBound ||
      result.producerSearchContract.requireConnectedCover !=
        authoring.batchRequest.requireConnectedCover
    ) Left(AuthoringMismatch(phase))
    else
      result.status match {
        case PostgreSqlEnumerationBatchStatus.Produced =>
          result.artifact match {
            case Some(artifact)
                if result.completedPhase == PostgreSqlRewritePhase.Rendered &&
                  result.diagnostics.isEmpty &&
                  exactFound(authoring, result, artifact) =>
              Right(Found(artifact))
            case _ => Left(UnpackageableResult(phase))
          }
        case PostgreSqlEnumerationBatchStatus.NotApplicable =>
          exactFallback(authoring, result, NoConnectedCover)
            .orElse(exactFallback(authoring, result, NoDecomposition))
            .toRight(UnpackageableResult(phase))
        case PostgreSqlEnumerationBatchStatus.InvalidQuery |
            PostgreSqlEnumerationBatchStatus.Failed =>
          Left(UnpackageableResult(phase))
      }
  }

  private def exactFallback(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult,
      reason: PostgreSqlEnumerationCampaignV2NativeFallbackReason
  ): Option[NativeFallback] = {
    val report = result.pipelineEvidence
    val structuralEvidence = boundStructuralEvidence(authoring, result)
    val common =
      result.artifact.isEmpty &&
        result.completedPhase == reason.completedPhase &&
        result.diagnostics.size == 1 &&
        result.diagnostics.head.code == reason.diagnosticCode &&
        structuralEvidence.nonEmpty &&
        report.candidateSelectionDecision.isEmpty &&
        report.bagFactorPlan.isEmpty &&
        report.artifact.isEmpty &&
        report.physicalPortfolioAttempt.isEmpty &&
        report.physicalPortfolioArtifact.isEmpty
    if (!common) None
    else {
      try {
        val soft = PostgreSqlEnumerationCampaignV2ProjectionView
          .softGenerationCountersFromResult(result)
        val candidate = reason match {
          case NoConnectedCover
              if result.request.requireConnectedCover &&
                report.admissibleGeneration.exists(
                  _.requireConnectedCover
                ) &&
                report.candidateInput.isEmpty &&
                report.candidateDecision.isEmpty &&
                soft.admissibleCandidateBagCount == 0 &&
                soft.admissibleCandidateCoverAlternativeCount == 0 =>
            Some(None)
          case NoDecomposition
              if structuralEvidence.exists(evidence =>
                report.candidateInput.exists(input =>
                  exactCandidateInput(result, evidence, input)
                )
              ) &&
                report.candidateDecision.exists {
                  case rejected: CandidatePlanDecision.Rejected =>
                    rejected.requestedLimit == result.request.planLimit
                  case _ => false
                } =>
            Some(
              Some(
                PostgreSqlEnumerationCampaignV2ProjectionView
                  .rejectedCandidateSearchCountersFromResult(result)
              )
            )
          case _ => None
        }
        candidate.map { exactCandidate =>
          val contract = PostgreSqlEnumerationCampaignV2Json
            .producerSearchContractDocumentForAcquisition(
              result.producerSearchContract,
              result.request.originalSqlSha256,
              result.request,
              authoring
            )
          val detail = PostgreSqlEnumerationCampaignV2ReasonDetails
            .fallbackDetailPreimage(
              authoring,
              contract,
              reason,
              soft,
              exactCandidate
            )
          NativeFallback(result, reason, soft, exactCandidate, detail)
        }
      } catch {
        case _: IllegalArgumentException => None
      }
    }
  }

  private def exactFound(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult,
      artifact: PostgreSqlEnumerationBatchArtifact
  ): Boolean = {
    val report = result.pipelineEvidence
    boundStructuralEvidence(authoring, result).exists { evidence =>
      artifact.request == result.request &&
      artifact.producerSearchContract == result.producerSearchContract &&
      artifact.analysis == evidence.analysis &&
      artifact.hypergraph == evidence.hypergraph &&
      artifact.softGeneration == evidence.softGeneration &&
      artifact.admissibleGeneration == evidence.admissibleGeneration &&
      report.candidateInput.exists(input =>
        (artifact.candidateInput eq input) &&
          exactCandidateInput(result, evidence, input)
      ) &&
      report.candidateDecision.contains(artifact.batch.search) &&
      report.candidateSelectionDecision.isEmpty &&
      report.bagFactorPlan.isEmpty &&
      report.artifact.isEmpty &&
      report.physicalPortfolioAttempt.isEmpty &&
      report.physicalPortfolioArtifact.isEmpty
    }
  }

  /**
    * Replays every structural hand-off available at this boundary. The
    * authoring request binds the source bytes and search settings; analysis is
    * then independently converted back into the hypergraph, soft
    * admissibility is recomputed, and candidate input is reconstructed below.
    */
  private def boundStructuralEvidence(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult
  ): Option[BoundStructuralEvidence] = {
    val request = result.request
    val report = result.pipelineEvidence
    for {
      structural <- boundHypergraphEvidence(authoring, result)
      soft <- report.softGeneration
      admissible <- report.admissibleGeneration
      if soft.sourceHypergraph == structural.hypergraph
      if soft.widthBound == request.widthBound
      if admissible.sourceHypergraph == structural.hypergraph
      if admissible.widthBound == request.widthBound
      if admissible.requireConnectedCover == request.requireConnectedCover
      if admissible.definitionCovers == soft.enumeratedCovers
      if soft.admissible(request.requireConnectedCover) == admissible
    } yield BoundStructuralEvidence(
      structural.analysis,
      structural.hypergraph,
      soft,
      admissible
    )
  }

  /** Exact analysis-to-hypergraph hand-off shared by terminal resource checks. */
  private[cli] def hasExactBoundHypergraph(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult
  ): Boolean =
    boundHypergraphEvidence(authoring, result).nonEmpty

  /** Exact structural and candidate-input hand-off for retained resources. */
  private[cli] def hasExactBoundCandidateInput(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult
  ): Boolean =
    boundStructuralEvidence(authoring, result).exists { evidence =>
      result.pipelineEvidence.candidateInput.exists(input =>
        exactCandidateInput(result, evidence, input)
      )
    }

  private def boundHypergraphEvidence(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult
  ): Option[BoundHypergraphEvidence] = {
    val request = result.request
    val report = result.pipelineEvidence
    for {
      analysis <- report.analysis
      hypergraph <- report.hypergraph
      recomputedHypergraph <- analysis.hypergraph.toOption
      if request == authoring.batchRequest
      if request.originalSqlSha256 == authoring.batchRequest.originalSqlSha256
      if result.producerSearchContract.widthBound == request.widthBound
      if result.producerSearchContract.requireConnectedCover ==
        request.requireConnectedCover
      if BigInt(request.planLimit) <=
        BigInt(result.producerSearchContract.candidate.maxPlanLimit)
      if analysis.diagnostics.isEmpty
      if analysis.query.resultShape == QueryResultShape.EnumerationProjection
      if recomputedHypergraph == hypergraph
      if hypergraph.nonEmpty && hypergraph.variables.nonEmpty
      if hypergraph.edges.forall(_.vertices.nonEmpty)
    } yield BoundHypergraphEvidence(analysis, hypergraph)
  }

  private def exactCandidateInput(
      result: PostgreSqlEnumerationBatchResult,
      evidence: BoundStructuralEvidence,
      supplied: CandidateSearchInput
  ): Boolean =
    CandidateSearchInput
      .create(
        evidence.hypergraph,
        result.request.widthBound,
        evidence.admissibleGeneration
      )
      .toOption
      .exists { recomputed =>
        supplied.hypergraph == evidence.hypergraph &&
        supplied.widthBound == result.request.widthBound &&
        supplied.requireConnectedCover ==
          result.request.requireConnectedCover &&
        supplied.hypergraph == recomputed.hypergraph &&
        supplied.widthBound == recomputed.widthBound &&
        supplied.requireConnectedCover ==
          recomputed.requireConnectedCover &&
        supplied.candidateBags == recomputed.candidateBags
      }
}

/** Scala-producer convention for replayable, diagnostic-text-free reasons. */
private[cli] object PostgreSqlEnumerationCampaignV2ReasonDetails {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._
  import PostgreSqlEnumerationCampaignV2ProjectionView.{
    CandidateSearchCounters,
    SoftGenerationCounters
  }

  val FallbackDetailSchema: String =
    "yrw-enumeration-native-fallback-detail-preimage/v2"
  val PlanNotRequestedDetailSchema: String =
    "yrw-enumeration-postgresql-plan-reason-detail-preimage/v2"

  def fallbackDetailPreimage(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      producerSearchContract: PostgreSqlEnumerationCampaignV2JsonDocument,
      reason: PostgreSqlEnumerationCampaignV2NativeFallbackReason,
      soft: SoftGenerationCounters,
      candidate: Option[CandidateSearchCounters]
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    require(authoring != null, "fallback authoring request must not be null")
    require(producerSearchContract != null, "fallback contract must not be null")
    require(reason != null, "fallback reason must not be null")
    require(soft != null, "fallback soft counters must not be null")
    require(candidate != null, "fallback candidate counters must not be null")
    require(
      reason.requiresCandidateSearch == candidate.nonEmpty,
      "fallback detail evidence must match its reason"
    )
    document(
      obj(
        "authoring_request_sha256" -> string(authoring.sha256),
        "candidate_search" -> candidateSearchValue(candidate),
        "code" -> string(reason.code),
        "completed_phase" -> string(reason.completedPhase.id),
        "phase" -> string(reason.phase),
        "producer_search_contract_sha256" -> string(
          producerSearchContract.sha256
        ),
        "schema_version" -> string(FallbackDetailSchema),
        "soft_generation" -> softGenerationValue(soft)
      )
    )
  }

  def planNotRequestedDetailPreimage(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      actionId: String
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    require(authoring != null, "plan authoring request must not be null")
    PostgreSqlEnumerationCampaignV2AuthoringValidation.requireIdentifier(
      actionId,
      "plan reason action ID"
    )
    document(
      obj(
        "action_id" -> string(actionId),
        "authoring_request_sha256" -> string(authoring.sha256),
        "capture_state" -> string("not-requested"),
        "include_whole_plan" -> bool(false),
        "schema_version" -> string(PlanNotRequestedDetailSchema)
      )
    )
  }

  private[cli] def softGenerationValue(
      soft: SoftGenerationCounters
  ): Value =
    obj(
      "counters" -> obj(
        "admissible_candidate_bag_count" -> number(
          soft.admissibleCandidateBagCount
        ),
        "admissible_candidate_cover_alternative_count" -> number(
          soft.admissibleCandidateCoverAlternativeCount
        ),
        "candidate_cover_check_count" -> number(
          soft.candidateCoverCheckCount
        ),
        "component_count" -> number(soft.componentCount),
        "cover_count" -> number(soft.coverCount),
        "cover_pair_count" -> number(soft.coverPairCount),
        "derivation_count" -> number(soft.derivationCount),
        "generated_candidate_bag_count" -> number(
          soft.generatedCandidateBagCount
        ),
        "generated_candidate_cover_alternative_count" -> number(
          soft.generatedCandidateCoverAlternativeCount
        )
      ),
      "state" -> string("completed")
    )

  private[cli] def candidateSearchValue(
      candidate: Option[CandidateSearchCounters]
  ): Value = candidate match {
    case None => obj("state" -> string("not-run"), "statistics" -> NullValue)
    case Some(search) =>
      obj(
        "state" -> string("rejected"),
        "statistics" -> obj(
          "basis_alternative_count" -> number(search.basisAlternativeCount),
          "basis_check_count" -> number(search.basisCheckCount),
          "block_count" -> number(search.blockCount),
          "candidate_bag_count" -> number(search.candidateBagCount),
          "combination_state_count" -> number(
            search.combinationStateCount
          ),
          "maximum_witness_node_count" -> number(
            search.maximumWitnessNodeCount
          ),
          "retained_partial_plan_count" -> number(
            search.retainedPartialPlanCount
          ),
          "satisfied_block_count" -> number(search.satisfiedBlockCount)
        )
      )
  }

  private[cli] def sha256DomainSeparated(
      asciiDomain: String,
      document: PostgreSqlEnumerationCampaignV2JsonDocument
  ): String = {
    require(asciiDomain != null && asciiDomain.nonEmpty, "hash domain is empty")
    require(
      asciiDomain.forall(_ <= '\u007f'),
      "hash domain must contain only ASCII"
    )
    require(document != null, "domain-separated document must not be null")
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(asciiDomain.getBytes(StandardCharsets.US_ASCII))
    document.updateDigest(digest)
    PostgreSqlEnumerationCampaignV2Json.sha256Digest(digest.digest())
  }
}

/** One non-SQL role keeps payload and wrapper identities impossible to mix. */
private[cli] final case class PostgreSqlEnumerationCampaignV2NonSqlAsset(
    actionId: String,
    role: String,
    contentSchema: String,
    payload: PostgreSqlEnumerationCampaignV2JsonDocument,
    wrapper: PostgreSqlEnumerationCampaignV2JsonDocument
) {
  require(actionId != null && actionId.nonEmpty, "asset action ID is empty")
  require(role != null && role.nonEmpty, "asset role is empty")
  require(contentSchema != null && contentSchema.nonEmpty, "asset schema is empty")
  require(payload != null, "asset payload must not be null")
  require(wrapper != null, "asset wrapper must not be null")
  val assetId: String = s"$actionId:$role"
  val payloadSha256: String = payload.sha256
}

/** Complete Scala-native pre-seal handoff accepted by the Python composer. */
private[yannakakis] final case class PostgreSqlEnumerationCampaignV2Assembly(
    document: PostgreSqlEnumerationCampaignV2JsonDocument,
    projection: PostgreSqlEnumerationCampaignV2Projection,
    actionRegistry: PostgreSqlEnumerationCampaignV2JsonDocument,
    nonSqlAssets: Vector[PostgreSqlEnumerationCampaignV2NonSqlAsset],
    reasonDetailPreimages: Vector[
      PostgreSqlEnumerationCampaignV2JsonDocument
    ],
    cumulativeEvidence: PostgreSqlEnumerationCumulativeBudgetEvidence
) {
  require(document != null, "acquisition assembly must not be null")
  require(projection != null, "acquisition projection must not be null")
  require(actionRegistry != null, "action registry must not be null")
  require(
    nonSqlAssets != null && nonSqlAssets.forall(_ != null),
    "non-SQL assets must not be null or contain null"
  )
  require(
    nonSqlAssets.map(_.assetId) == nonSqlAssets.map(_.assetId).sorted.distinct,
    "non-SQL assets must be sorted by unique asset ID"
  )
  require(
    reasonDetailPreimages != null && reasonDetailPreimages.forall(_ != null),
    "reason detail preimages must not be null or contain null"
  )
  require(cumulativeEvidence != null, "cumulative evidence must not be null")
}

/** Builds only outcome-free found or exact typed-native-fallback assemblies. */
private[yannakakis] object PostgreSqlEnumerationCampaignV2Acquisition {
  import PostgreSqlEnumerationCampaignV2AcquisitionFailure._
  import PostgreSqlEnumerationCampaignV2CanonicalJson._
  import PostgreSqlEnumerationCampaignV2EligibleResult._

  val AssemblySchema: String =
    "yrw-enumeration-offline-acquisition-assembly/v2"
  val ActionRegistrySchema: String =
    "yrw-enumeration-action-registry/v2"
  val NonSqlAssetBytesSchema: String =
    "yrw-enumeration-non-sql-asset-bytes/v2"
  val PostgreSqlPlanPayloadSchema: String =
    "yrw-enumeration-postgresql-plan-payload/v2"
  val PreexecutionFeaturesPayloadSchema: String =
    "yrw-enumeration-preexecution-features-payload/v2"
  val ProducerArtifactPayloadSchema: String =
    "yrw-enumeration-producer-artifact-payload/v2"
  val SemanticCertificatePayloadSchema: String =
    "yrw-enumeration-semantic-certificate-payload/v2"

  private val ActionRegistrySealDomain =
    "yrw-enumeration-action-registry-seal/v2\u0000"

  private val RoleSchemas: Map[String, String] = Map(
    "postgresql-plan" -> PostgreSqlPlanPayloadSchema,
    "preexecution-features" -> PreexecutionFeaturesPayloadSchema,
    "producer-artifact" -> ProducerArtifactPayloadSchema,
    "semantic-certificate" -> SemanticCertificatePayloadSchema
  )

  def assemble(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult,
      cumulativeLimits: PostgreSqlEnumerationCumulativeByteLimits =
        PostgreSqlEnumerationCumulativeByteLimits.default
  ): Either[
    PostgreSqlEnumerationCampaignV2AcquisitionFailure,
    PostgreSqlEnumerationCampaignV2Assembly
  ] = {
    require(authoring != null, "authoring request must not be null")
    require(result != null, "enumeration result must not be null")
    require(cumulativeLimits != null, "cumulative limits must not be null")
    PostgreSqlEnumerationCampaignV2Packageability
      .classify(authoring, result)
      .flatMap(assembleEligible(authoring, _, cumulativeLimits))
  }

  /**
    * Builds from an already-classified result without consulting packageability
    * again. The additive search-replay bundle uses this seam so one exact
    * `Found` value feeds both projections after a single classification.
    */
  private[cli] def assembleEligible(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      eligible: PostgreSqlEnumerationCampaignV2EligibleResult,
      cumulativeLimits: PostgreSqlEnumerationCumulativeByteLimits
  ): Either[
    PostgreSqlEnumerationCampaignV2AcquisitionFailure,
    PostgreSqlEnumerationCampaignV2Assembly
  ] = {
    require(authoring != null, "authoring request must not be null")
    require(eligible != null, "eligible result must not be null")
    require(cumulativeLimits != null, "cumulative limits must not be null")
    val projected: Either[
      PostgreSqlEnumerationCampaignV2AcquisitionFailure,
      PostgreSqlEnumerationCampaignV2Projection
    ] =
      try {
        Right(
          eligible match {
            case Found(artifact) =>
              PostgreSqlEnumerationCampaignV2Json.project(artifact, authoring)
            case fallback: NativeFallback =>
              PostgreSqlEnumerationCampaignV2Json
                .projectNativeFallback(authoring, fallback)
          }
        )
      } catch {
        case _: IllegalArgumentException =>
          Left(ProjectionRejected(eligible.completedPhase))
      }
    projected.flatMap { projection =>
      try Right(build(authoring, eligible, projection, cumulativeLimits))
      catch {
        case _: IllegalArgumentException =>
          Left(AssemblyRejected(eligible.completedPhase))
      }
    }
  }

  private def build(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      eligible: PostgreSqlEnumerationCampaignV2EligibleResult,
      projection: PostgreSqlEnumerationCampaignV2Projection,
      cumulativeLimits: PostgreSqlEnumerationCumulativeByteLimits
  ): PostgreSqlEnumerationCampaignV2Assembly = {
    val payloads = Vector.newBuilder[
      (String, String, PostgreSqlEnumerationCampaignV2JsonDocument)
    ]
    val reasonPreimages = Vector.newBuilder[
      PostgreSqlEnumerationCampaignV2JsonDocument
    ]
    val fallback = eligible match {
      case value: NativeFallback => Some(value)
      case _: Found              => None
    }

    projection.actions.zip(projection.featureValueBundles).foreach {
      case (action, bundle) =>
        val plan = fallback match {
          case Some(value) => fallbackPlanPayload(action, value)
          case None =>
            val detail = PostgreSqlEnumerationCampaignV2ReasonDetails
              .planNotRequestedDetailPreimage(authoring, action.actionId)
            reasonPreimages += detail
            notRequestedPlanPayload(action, detail.sha256)
        }
        payloads += ((action.actionId, "postgresql-plan", plan))
        payloads += ((
          action.actionId,
          "preexecution-features",
          preexecutionFeaturesPayload(
            action,
            bundle,
            projection.featureRegistry.sha256
          )
        ))
        if (action.actionKind == "retained_candidate") {
          payloads += ((
            action.actionId,
            "producer-artifact",
            producerArtifactPayload(action, projection.semanticSearchCore.sha256)
          ))
          payloads += ((
            action.actionId,
            "semantic-certificate",
            semanticCertificatePayload(
              authoring,
              action,
              projection.semanticSearchCore.sha256,
              projection.actions.head.renderedSqlBytesSha256
            )
          ))
        }
    }

    val nonSqlAssets = payloads.result().map { case (actionId, role, payload) =>
      wrapNonSqlAsset(actionId, role, payload)
    }.sortBy(_.assetId)
    val byActionRole = nonSqlAssets.iterator
      .map(asset => (asset.actionId, asset.role) -> asset)
      .toMap
    require(
      byActionRole.size == nonSqlAssets.size,
      "non-SQL asset action/role identities must be unique"
    )
    val (unsignedRegistry, actionRegistry) = actionRegistryDocuments(
      authoring,
      projection,
      byActionRole
    )
    val assembly = assemblyDocument(
      authoring,
      projection,
      actionRegistry,
      nonSqlAssets
    )
    val details = fallback.toVector.map(_.detailPreimage) ++
      reasonPreimages.result()
    val logicalDocuments = documentInventory(
      authoring,
      projection,
      details,
      nonSqlAssets,
      unsignedRegistry,
      actionRegistry,
      assembly
    )
    val cumulative = eligible match {
      case Found(artifact) =>
        PostgreSqlEnumerationCampaignV2CumulativeBudget
          .accountLogicalDocuments(artifact, logicalDocuments, cumulativeLimits)
      case value: NativeFallback =>
        PostgreSqlEnumerationCampaignV2CumulativeBudget
          .accountNativeFallbackLogicalDocuments(
            value.result,
            logicalDocuments,
            cumulativeLimits
          )
    }
    PostgreSqlEnumerationCampaignV2Assembly(
      assembly,
      projection,
      actionRegistry,
      nonSqlAssets,
      details,
      cumulative.fold(
        _ =>
          throw new IllegalArgumentException(
            "cumulative acquisition byte boundary rejected the assembly"
          ),
        identity
      )
    )
  }

  private def notRequestedPlanPayload(
      action: PostgreSqlEnumerationCampaignV2ActionDocuments,
      detailSha256: String
  ): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(
      obj(
        "action_id" -> string(action.actionId),
        "action_kind" -> string(action.actionKind),
        "capture_state" -> string("not-requested"),
        "explain" -> explainContractValue,
        "plan_json" -> NullValue,
        "reason" -> typedReasonValue(
          "whole-plan-capture-not-requested",
          "preexecution",
          detailSha256
        ),
        "schema_version" -> string(PostgreSqlPlanPayloadSchema)
      )
    )

  private def fallbackPlanPayload(
      action: PostgreSqlEnumerationCampaignV2ActionDocuments,
      fallback: NativeFallback
  ): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(
      obj(
        "action_id" -> string(action.actionId),
        "action_kind" -> string(action.actionKind),
        "capture_state" -> string("native-fallback"),
        "explain" -> explainContractValue,
        "plan_json" -> NullValue,
        "reason" -> typedReasonValue(
          fallback.reason.code,
          fallback.reason.phase,
          fallback.detailSha256
        ),
        "schema_version" -> string(PostgreSqlPlanPayloadSchema)
      )
    )

  private def explainContractValue: Value =
    obj(
      "analyze" -> bool(false),
      "format" -> string("json"),
      "same_session" -> bool(true)
    )

  private def typedReasonValue(
      code: String,
      phase: String,
      detailSha256: String
  ): Value = {
    PostgreSqlEnumerationCampaignV2AuthoringValidation.requireIdentifier(
      code,
      "typed reason code"
    )
    PostgreSqlEnumerationCampaignV2AuthoringValidation.requireIdentifier(
      phase,
      "typed reason phase"
    )
    PostgreSqlEnumerationCampaignV2AuthoringValidation.requireSha256(
      detailSha256,
      "typed reason detail SHA-256"
    )
    obj(
      "code" -> string(code),
      "detail_sha256" -> string(detailSha256),
      "phase" -> string(phase)
    )
  }

  private def preexecutionFeaturesPayload(
      action: PostgreSqlEnumerationCampaignV2ActionDocuments,
      bundle: PostgreSqlEnumerationCampaignV2FeatureBundleDocument,
      featureRegistrySha256: String
  ): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(
      obj(
        "action_id" -> string(action.actionId),
        "action_kind" -> string(action.actionKind),
        "capture_state" -> string("complete-explicit-states"),
        "feature_registry_sha256" -> string(featureRegistrySha256),
        "feature_value_bundle_sha256" -> string(bundle.document.sha256),
        "schema_version" -> string(PreexecutionFeaturesPayloadSchema)
      )
    )

  private def producerArtifactPayload(
      action: PostgreSqlEnumerationCampaignV2ActionDocuments,
      semanticSearchCoreSha256: String
  ): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(
      obj(
        "action_id" -> string(action.actionId),
        "candidate_id" -> string(action.candidateId.get),
        "candidate_rank" -> number(BigInt(action.candidateRank.get)),
        "ordered_stages_sha256" -> string(action.orderedStages.sha256),
        "physical_variant_id" -> string(action.physicalVariantId.get),
        "rendered_sql_sha256" -> string(action.renderedSqlBytesSha256),
        "schema_version" -> string(ProducerArtifactPayloadSchema),
        "semantic_search_core_sha256" -> string(
          semanticSearchCoreSha256
        )
      )
    )

  private def semanticCertificatePayload(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      action: PostgreSqlEnumerationCampaignV2ActionDocuments,
      semanticSearchCoreSha256: String,
      sourceSqlSha256: String
  ): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(
      obj(
        "action_id" -> string(action.actionId),
        "authoring_request_sha256" -> string(authoring.sha256),
        "candidate_id" -> string(action.candidateId.get),
        "candidate_rank" -> number(BigInt(action.candidateRank.get)),
        "certificate_status" -> string(
          "producer-certified-no-independent-proof-claim"
        ),
        "generated_sql_sha256" -> string(action.renderedSqlBytesSha256),
        "schema_version" -> string(SemanticCertificatePayloadSchema),
        "semantic_search_core_sha256" -> string(
          semanticSearchCoreSha256
        ),
        "source_sql_sha256" -> string(sourceSqlSha256)
      )
    )

  private def wrapNonSqlAsset(
      actionId: String,
      role: String,
      payload: PostgreSqlEnumerationCampaignV2JsonDocument
  ): PostgreSqlEnumerationCampaignV2NonSqlAsset = {
    val contentSchema = RoleSchemas.getOrElse(
      role,
      throw new IllegalArgumentException("unsupported non-SQL asset role")
    )
    val wrapper = document(
      obj(
        "action_id" -> string(actionId),
        "asset_id" -> string(s"$actionId:$role"),
        "byte_length" -> number(BigInt(payload.byteLength)),
        "content_schema" -> string(contentSchema),
        "encoding" -> string("utf-8"),
        "media_type" -> string("application/json"),
        "role" -> string(role),
        "schema_version" -> string(NonSqlAssetBytesSchema),
        "sha256" -> string(payload.sha256),
        "utf8_hex" -> utf8HexCanonicalDocument(payload)
      )
    )
    PostgreSqlEnumerationCampaignV2NonSqlAsset(
      actionId,
      role,
      contentSchema,
      payload,
      wrapper
    )
  }

  private def actionRegistryDocuments(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      projection: PostgreSqlEnumerationCampaignV2Projection,
      byActionRole: Map[
        (String, String),
        PostgreSqlEnumerationCampaignV2NonSqlAsset
      ]
  ): (
    PostgreSqlEnumerationCampaignV2JsonDocument,
    PostgreSqlEnumerationCampaignV2JsonDocument
  ) = {
    val actions = projection.actions.map(action =>
      actionValue(action, byActionRole)
    )
    val bindings = registryBindingsValue(authoring, projection)
    val unsigned = document(
      obj(
        "actions" -> array(actions),
        "bindings" -> bindings,
        "protocol_version" -> string(
          PostgreSqlEnumerationCampaignV2Json.ProtocolVersion
        ),
        "schema_version" -> string(ActionRegistrySchema)
      )
    )
    val seal = PostgreSqlEnumerationCampaignV2ReasonDetails
      .sha256DomainSeparated(ActionRegistrySealDomain, unsigned)
    val registry = document(
      obj(
        "action_registry_seal_sha256" -> string(seal),
        "actions" -> array(actions),
        "bindings" -> registryBindingsValue(authoring, projection),
        "protocol_version" -> string(
          PostgreSqlEnumerationCampaignV2Json.ProtocolVersion
        ),
        "schema_version" -> string(ActionRegistrySchema)
      )
    )
    (unsigned, registry)
  }

  private def actionValue(
      action: PostgreSqlEnumerationCampaignV2ActionDocuments,
      byActionRole: Map[
        (String, String),
        PostgreSqlEnumerationCampaignV2NonSqlAsset
      ]
  ): Value = {
    val roles =
      if (action.actionKind == "native")
        Vector("postgresql-plan", "preexecution-features", "source-sql")
      else
        Vector(
          "generated-sql",
          "postgresql-plan",
          "preexecution-features",
          "producer-artifact",
          "semantic-certificate"
        )
    val assets = roles.map { role =>
      val sha256 = role match {
        case "source-sql" | "generated-sql" =>
          action.renderedSqlBytesSha256
        case _ =>
          byActionRole
            .getOrElse(
              action.actionId -> role,
              throw new IllegalArgumentException(
                "action is missing a required non-SQL payload"
              )
            )
            .payloadSha256
      }
      obj("role" -> string(role), "sha256" -> string(sha256))
    }
    val retained = action.actionKind == "retained_candidate"
    obj(
      "action_id" -> string(action.actionId),
      "assets" -> array(assets),
      "candidate_id" -> action.candidateId.fold[Value](NullValue)(string),
      "candidate_rank" -> action.candidateRank.fold[Value](NullValue)(rank =>
        number(BigInt(rank))
      ),
      "kind" -> string(action.actionKind),
      "ordered_stages" -> embeddedDocument(action.orderedStages),
      "physical_variant_id" ->
        action.physicalVariantId.fold[Value](NullValue)(string),
      "physical_variant_id_scheme" ->
        (if (retained)
           string("postgresql_candidate_reduction_variant_v1")
         else NullValue),
      "rendered_sql" -> embeddedDocument(action.renderedSql),
      "stage_id_scheme" ->
        (if (retained) string("postgresql_renderer_stage_id_v1")
         else NullValue),
      "stage_order_scheme" ->
        (if (retained)
           string("postgresql_cte_emission_then_terminal_v1")
         else NullValue)
    )
  }

  private def registryBindingsValue(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      projection: PostgreSqlEnumerationCampaignV2Projection
  ): Value =
    obj(
      "authoring_request_sha256" -> string(authoring.sha256),
      "data_contract_sha256" -> string(
        authoring.dataContractDocument.sha256
      ),
      "feature_registry_sha256" -> string(
        authoring.featureRegistryDocument.sha256
      ),
      "fixed_physical_config_sha256" -> string(
        authoring.fixedPhysicalConfigDocument.sha256
      ),
      "lineage_id" -> string(authoring.lineageId),
      "semantic_search_core_sha256" -> string(
        projection.semanticSearchCore.sha256
      ),
      "settings_bundle_sha256" -> string(
        authoring.settingsBundleDocument.sha256
      ),
      "source_sql_sha256" -> string(
        projection.actions.head.renderedSqlBytesSha256
      ),
      "toolchain_bundle_sha256" -> string(
        authoring.toolchainBundleDocument.sha256
      ),
      "workload_id" -> string(authoring.workloadId)
    )

  private def assemblyDocument(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      projection: PostgreSqlEnumerationCampaignV2Projection,
      actionRegistry: PostgreSqlEnumerationCampaignV2JsonDocument,
      assets: Vector[PostgreSqlEnumerationCampaignV2NonSqlAsset]
  ): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(
      obj(
        "action_registry" -> embeddedDocument(actionRegistry),
        "authoring_request" -> embeddedDocument(authoring.document),
        "canonical_decompositions" -> array(
          projection.canonicalDecompositions.map(embeddedDocument)
        ),
        "feature_value_bundles" -> array(
          projection.featureValueBundles.map(bundle =>
            embeddedDocument(bundle.document)
          )
        ),
        "non_sql_assets" -> array(
          assets.map(asset => embeddedDocument(asset.wrapper))
        ),
        "producer_search_contract" -> embeddedDocument(
          projection.producerSearchContract
        ),
        "producer_search_transcript" -> embeddedDocument(
          projection.producerSearchTranscript
        ),
        "protocol_version" -> string(
          PostgreSqlEnumerationCampaignV2Json.ProtocolVersion
        ),
        "schema_version" -> string(AssemblySchema),
        "semantic_search_core" -> embeddedDocument(
          projection.semanticSearchCore
        )
      )
    )

  private def documentInventory(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      projection: PostgreSqlEnumerationCampaignV2Projection,
      detailPreimages: Vector[PostgreSqlEnumerationCampaignV2JsonDocument],
      assets: Vector[PostgreSqlEnumerationCampaignV2NonSqlAsset],
      unsignedRegistry: PostgreSqlEnumerationCampaignV2JsonDocument,
      actionRegistry: PostgreSqlEnumerationCampaignV2JsonDocument,
      assembly: PostgreSqlEnumerationCampaignV2JsonDocument
  ): Vector[(String, PostgreSqlEnumerationCampaignV2JsonDocument)] = {
    val result = Vector.newBuilder[
      (String, PostgreSqlEnumerationCampaignV2JsonDocument)
    ]
    result += "authoring-request" -> authoring.document
    result += "authoring/source-sql" -> authoring.sourceSqlDocument
    result += "authoring/data-contract" -> authoring.dataContractDocument
    result += "authoring/feature-registry" -> authoring.featureRegistryDocument
    result += "authoring/settings-bundle" -> authoring.settingsBundleDocument
    result += "authoring/fixed-physical" -> authoring.fixedPhysicalConfigDocument
    result += "authoring/toolchain-bundle" -> authoring.toolchainBundleDocument
    result += "projection/source-sql" -> projection.sourceSql
    result += "semantic-search-core" -> projection.semanticSearchCore
    result += "producer-search-contract" -> projection.producerSearchContract
    result += "producer-search-transcript" -> projection.producerSearchTranscript
    projection.canonicalDecompositions.zipWithIndex.foreach {
      case (value, index) =>
        result += f"canonical-decomposition/rewrite-${index + 1}%04d" -> value
    }
    result += "projection/feature-registry" -> projection.featureRegistry
    projection.featureValueBundles.foreach(bundle =>
      result += s"feature-values/${bundle.actionId}" -> bundle.document
    )
    projection.actions.foreach { action =>
      result += s"action-sql/${action.actionId}" -> action.renderedSql
      result += s"ordered-stages/${action.actionId}" -> action.orderedStages
    }
    detailPreimages.zipWithIndex.foreach { case (detail, index) =>
      result += f"reason-detail/${index + 1}%04d" -> detail
    }
    assets.foreach { asset =>
      result += s"non-sql-payload/${asset.assetId}" -> asset.payload
      result += s"non-sql-wrapper/${asset.assetId}" -> asset.wrapper
    }
    result += "action-registry-unsigned" -> unsignedRegistry
    result += "action-registry" -> actionRegistry
    result += "pre-seal-assembly" -> assembly
    result.result()
  }
}
