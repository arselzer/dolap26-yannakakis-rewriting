package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidateSearchInput
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchArtifact,
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationBatchResult,
  PostgreSqlEnumerationBatchStatus,
  PostgreSqlEnumerationProducerSearchContract
}
import at.ac.tuwien.dbai.yannakakis.core.{Hyperedge, QueryHypergraph}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  QueryResultShape
}

/**
  * Output-free structural-search input for every already-classified v2 result.
  *
  * The projection validates the structural hand-off retained by either a
  * produced artifact or an exact typed native fallback, but serializes only
  * the common pre-soft/search input. No producer status, reason, counter,
  * candidate, decomposition, termination, or outcome enters the document.
  */
private[cli] object PostgreSqlEnumerationSearchReplayV2Json {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._
  import PostgreSqlEnumerationCampaignV2EligibleResult._
  import PostgreSqlEnumerationCampaignV2NativeFallbackReason._

  val SchemaVersion: String = "yrw-enumeration-search-input/v2"
  val ProtocolVersion: String =
    "yrw-enumeration-search-replay-protocol/v2"
  val CaptureOrigin: String =
    "scala-one-pass-enumeration-eligible-result-v2"
  val ClaimsIndependentSqlFrontend: Boolean = false
  val MaximumCanonicalJsonBytes: Int =
    PostgreSqlEnumerationCampaignV2Json.MaximumCanonicalJsonBytes

  private val SealDomain =
    "yrw-enumeration-search-input-seal/v2\u0000"
      .getBytes(StandardCharsets.UTF_8)

  private final case class Capture(
      request: PostgreSqlEnumerationBatchRequest,
      contract: PostgreSqlEnumerationProducerSearchContract,
      hypergraph: QueryHypergraph
  ) {
    require(request != null, "v2 search capture request must not be null")
    require(contract != null, "v2 search capture contract must not be null")
    require(hypergraph != null, "v2 search capture hypergraph must not be null")
  }

  /** Projects exactly one value returned by the closed packageability gate. */
  def project(
      eligible: PostgreSqlEnumerationCampaignV2EligibleResult
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    require(eligible != null, "v2 search eligible result must not be null")
    val capture = eligible match {
      case Found(artifact)          => captureFound(artifact)
      case fallback: NativeFallback => captureFallback(fallback)
    }

    val request = capture.request
    val contract = capture.contract
    val unsignedFields = Vector[(String, Value)](
      "candidate_limits" -> candidateLimits(contract),
      "capture_origin" -> string(CaptureOrigin),
      "claims_independent_sql_frontend" -> bool(
        ClaimsIndependentSqlFrontend
      ),
      "edges" -> edges(capture.hypergraph),
      "protocol_version" -> string(ProtocolVersion),
      "requested_k" -> number(BigInt(request.planLimit)),
      "require_connected_cover" -> bool(request.requireConnectedCover),
      "schema_version" -> string(SchemaVersion),
      "soft_limits" -> softLimits(contract),
      "source_sql_sha256" -> string(request.originalSqlSha256),
      "width_bound" -> number(BigInt(request.widthBound))
    )
    val unsigned = document(obj(unsignedFields))
    val signed = document(
      obj(
        unsignedFields :+
          ("search_input_seal_sha256" -> string(
            sha256(SealDomain, unsigned.bytes)
          ))
      )
    )
    require(
      signed.byteLength <= MaximumCanonicalJsonBytes,
      "v2 search-input document exceeds its exact canonical UTF-8 byte limit"
    )
    signed
  }

  private def captureFound(
      artifact: PostgreSqlEnumerationBatchArtifact
  ): Capture = {
    require(artifact != null, "v2 found artifact must not be null")
    val request = artifact.request
    val contract = artifact.producerSearchContract
    requireCommonAnalysis(
      artifact.analysis,
      artifact.hypergraph,
      request,
      contract
    )
    require(
      artifact.softGeneration.sourceHypergraph == artifact.hypergraph &&
        artifact.admissibleGeneration.sourceHypergraph == artifact.hypergraph &&
        artifact.candidateInput.hypergraph == artifact.hypergraph &&
        artifact.batch.candidateInput == artifact.candidateInput,
      "v2 found result must bind one exact hypergraph through every phase"
    )
    require(
      artifact.softGeneration.widthBound == request.widthBound &&
        artifact.admissibleGeneration.widthBound == request.widthBound &&
        artifact.candidateInput.widthBound == request.widthBound,
      "v2 found result must bind one exact width setting"
    )
    require(
      artifact.admissibleGeneration.requireConnectedCover ==
        request.requireConnectedCover &&
        artifact.candidateInput.requireConnectedCover ==
          request.requireConnectedCover,
      "v2 found result must bind one exact connected-cover setting"
    )
    require(
      artifact.softGeneration.admissible(request.requireConnectedCover) ==
        artifact.admissibleGeneration,
      "v2 found result must retain the exact admissibility projection"
    )
    requireExactCandidateInput(
      artifact.hypergraph,
      request,
      artifact.admissibleGeneration,
      artifact.candidateInput
    )
    require(
      artifact.batch.search.isInstanceOf[CandidatePlanDecision.Found] &&
        artifact.batch.requestedLimit == request.planLimit &&
        artifact.batch.search.requestedLimit == request.planLimit,
      "v2 found result must retain one exact completed search"
    )
    Capture(request, contract, artifact.hypergraph)
  }

  private def captureFallback(fallback: NativeFallback): Capture = {
    require(fallback != null, "v2 native fallback must not be null")
    val result = fallback.result
    val request = result.request
    val contract = result.producerSearchContract
    val report = result.pipelineEvidence
    require(
      result.status == PostgreSqlEnumerationBatchStatus.NotApplicable &&
        result.artifact.isEmpty &&
        result.diagnostics.size == 1 &&
        result.completedPhase == fallback.reason.completedPhase &&
        result.diagnostics.head.code == fallback.reason.diagnosticCode,
      "v2 fallback must retain one exact typed not-applicable result"
    )
    require(
      report.candidateSelectionDecision.isEmpty &&
        report.bagFactorPlan.isEmpty &&
        report.artifact.isEmpty &&
        report.physicalPortfolioAttempt.isEmpty &&
        report.physicalPortfolioArtifact.isEmpty,
      "v2 fallback must stop before selection or lowering"
    )
    val analysis = report.analysis.getOrElse(
      throw new IllegalArgumentException(
        "v2 fallback must retain its exact analysis"
      )
    )
    val hypergraph = report.hypergraph.getOrElse(
      throw new IllegalArgumentException(
        "v2 fallback must retain its exact hypergraph"
      )
    )
    val soft = report.softGeneration.getOrElse(
      throw new IllegalArgumentException(
        "v2 fallback must retain completed soft generation"
      )
    )
    val admissible = report.admissibleGeneration.getOrElse(
      throw new IllegalArgumentException(
        "v2 fallback must retain completed admissibility"
      )
    )
    requireCommonAnalysis(analysis, hypergraph, request, contract)
    require(
      soft.sourceHypergraph == hypergraph &&
        admissible.sourceHypergraph == hypergraph &&
        soft.widthBound == request.widthBound &&
        admissible.widthBound == request.widthBound &&
        admissible.requireConnectedCover == request.requireConnectedCover &&
        soft.admissible(request.requireConnectedCover) == admissible,
      "v2 fallback must retain the exact soft/admissible hand-off"
    )

    fallback.reason match {
      case NoConnectedCover =>
        require(
          request.requireConnectedCover &&
            admissible.candidates.isEmpty &&
            report.candidateInput.isEmpty &&
            report.candidateDecision.isEmpty &&
            fallback.candidateCounters.isEmpty,
          "v2 no-connected-cover fallback must stop before candidate search"
        )
      case NoDecomposition =>
        val candidateInput = report.candidateInput.getOrElse(
          throw new IllegalArgumentException(
            "v2 no-decomposition fallback must retain candidate input"
          )
        )
        requireExactCandidateInput(
          hypergraph,
          request,
          admissible,
          candidateInput
        )
        require(
          report.candidateDecision.exists {
            case rejected: CandidatePlanDecision.Rejected =>
              rejected.requestedLimit == request.planLimit
            case _: CandidatePlanDecision.Found => false
          } && fallback.candidateCounters.nonEmpty,
          "v2 no-decomposition fallback must retain one exact rejected search"
        )
    }
    Capture(request, contract, hypergraph)
  }

  private def requireCommonAnalysis(
      analysis: AnalysisReport,
      hypergraph: QueryHypergraph,
      request: PostgreSqlEnumerationBatchRequest,
      contract: PostgreSqlEnumerationProducerSearchContract
  ): Unit = {
    require(
      analysis != null && hypergraph != null && request != null &&
        contract != null,
      "v2 search capture structural evidence must not be null"
    )
    require(
      analysis.diagnostics.isEmpty &&
        analysis.query.resultShape == QueryResultShape.EnumerationProjection,
      "v2 search capture must retain a supported enumeration analysis"
    )
    val reconstructedHypergraph = analysis.hypergraph.fold(
      _ =>
        throw new IllegalArgumentException(
          "v2 search analysis must reconstruct its hypergraph"
        ),
      identity
    )
    require(
      reconstructedHypergraph == hypergraph &&
        analysis.query.relations.map(_.id) == hypergraph.relationOccurrenceIds,
      "v2 search hypergraph must equal the exact analysis-derived inventory"
    )
    require(
      hypergraph.nonEmpty && hypergraph.variables.nonEmpty &&
        hypergraph.edges.forall(_.vertices.nonEmpty) &&
        hypergraph.edges == hypergraph.edges.sorted,
      "v2 search hypergraph must retain the supported canonical core"
    )
    hypergraph.edges.foreach(requireCanonicalEdge)
    require(
      contract.widthBound == request.widthBound &&
        contract.requireConnectedCover == request.requireConnectedCover &&
        BigInt(request.planLimit) <= BigInt(contract.candidate.maxPlanLimit),
      "v2 search contract must bind the exact request settings"
    )
    require(
      contract.algorithm ==
        PostgreSqlEnumerationProducerSearchContract.Algorithm &&
        contract.structuralUniverse ==
          PostgreSqlEnumerationProducerSearchContract.StructuralUniverse &&
        contract.ranking ==
          PostgreSqlEnumerationProducerSearchContract.Ranking &&
        contract.physicalCoverPolicy ==
          PostgreSqlEnumerationProducerSearchContract.PhysicalCoverPolicy &&
        contract.exhaustionScope ==
          PostgreSqlEnumerationProducerSearchContract.ExhaustionScope &&
        !contract.coverDistinctAssignmentsEnumerated,
      "v2 search capture must retain the frozen structural replay contract"
    )
  }

  private def requireExactCandidateInput(
      hypergraph: QueryHypergraph,
      request: PostgreSqlEnumerationBatchRequest,
      admissible: at.ac.tuwien.dbai.yannakakis.algorithm.soft.AdmissibleSoftBagGeneration,
      supplied: CandidateSearchInput
  ): Unit = {
    val reconstructed = CandidateSearchInput
      .create(hypergraph, request.widthBound, admissible)
      .fold(
        _ =>
          throw new IllegalArgumentException(
            "v2 search capture must reconstruct its candidate input"
          ),
        identity
      )
    require(
      supplied.hypergraph == reconstructed.hypergraph &&
        supplied.widthBound == reconstructed.widthBound &&
        supplied.requireConnectedCover ==
          reconstructed.requireConnectedCover &&
        supplied.candidateBags == reconstructed.candidateBags,
      "v2 search capture must retain the exact reconstructed candidate input"
    )
  }

  private def edges(hypergraph: QueryHypergraph): Value =
    array(
      hypergraph.edges.map { edge =>
        obj(
          "bindings" -> array(
            edge.bindings.map { case (columnId, variableId) =>
              obj(
                "column_ordinal" -> number(BigInt(columnId.ordinal)),
                "variable_id" -> number(BigInt(variableId.value))
              )
            }
          ),
          "edge_id" -> number(BigInt(edge.id.value)),
          "relation_occurrence_id" -> number(
            BigInt(edge.relationOccurrenceId.value)
          )
        )
      }
    )

  private def candidateLimits(
      contract: PostgreSqlEnumerationProducerSearchContract
  ): Value = {
    val limits = contract.candidate
    obj(
      "max_basis_alternatives" -> number(BigInt(limits.maxBasisAlternatives)),
      "max_basis_checks" -> number(BigInt(limits.maxBasisChecks)),
      "max_blocks" -> number(BigInt(limits.maxBlocks)),
      "max_combination_states" -> number(BigInt(limits.maxCombinationStates)),
      "max_plan_limit" -> number(BigInt(limits.maxPlanLimit)),
      "max_retained_partial_plans" -> number(
        BigInt(limits.maxRetainedPartialPlans)
      ),
      "max_witness_nodes" -> number(BigInt(limits.maxWitnessNodes))
    )
  }

  private def softLimits(
      contract: PostgreSqlEnumerationProducerSearchContract
  ): Value = {
    val limits = contract.soft
    obj(
      "max_candidate_cover_checks" -> number(
        BigInt(limits.maxCandidateCoverChecks)
      ),
      "max_candidates" -> number(BigInt(limits.maxCandidates)),
      "max_components" -> number(BigInt(limits.maxComponents)),
      "max_cover_pairs" -> number(BigInt(limits.maxCoverPairs)),
      "max_covers" -> number(BigInt(limits.maxCovers)),
      "max_derivations" -> number(BigInt(limits.maxDerivations))
    )
  }

  private def requireCanonicalEdge(edge: Hyperedge): Unit = {
    require(edge != null, "v2 search hyperedge must not be null")
    require(
      edge.id.value == edge.relationOccurrenceId.value,
      "v2 search edge identity must equal its analyzed occurrence identity"
    )
    val reconstructed = Hyperedge
      .create(edge.id, edge.relationOccurrenceId, edge.bindings)
      .fold(
        _ =>
          throw new IllegalArgumentException(
            "v2 search hyperedge must satisfy the core binding grammar"
          ),
        identity
      )
    require(
      reconstructed == edge,
      "v2 search hyperedge bindings must retain canonical core order"
    )
  }

  private def sha256(prefix: Array[Byte], value: Array[Byte]): String = {
    require(prefix != null && value != null, "v2 search seal bytes are null")
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(prefix)
    digest.update(value)
    digest
      .digest()
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
  }
}
