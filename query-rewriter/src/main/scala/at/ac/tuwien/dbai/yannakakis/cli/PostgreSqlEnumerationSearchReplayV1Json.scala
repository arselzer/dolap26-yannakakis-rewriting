package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchArtifact,
  PostgreSqlEnumerationProducerSearchContract
}
import at.ac.tuwien.dbai.yannakakis.core.Hyperedge
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryResultShape

/**
  * Canonical occurrence-distinct input for an independent structural replay.
  *
  * This projection deliberately stops before soft-bag generation and search
  * output. In particular, it contains no Scala-produced candidate bag,
  * decomposition, candidate identity, rank, status, or outcome. A consumer can
  * therefore derive those values independently, conditional on the captured
  * SQL-to-hypergraph conversion. The latter is not independently certified by
  * this document and is named as an explicit false claim below.
  */
private[yannakakis] object PostgreSqlEnumerationSearchReplayV1Json {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._

  val SchemaVersion: String = "yrw-enumeration-search-input/v1"
  val ProtocolVersion: String =
    "yrw-enumeration-search-replay-protocol/v1"
  val CaptureOrigin: String = "scala-one-pass-enumeration-artifact-v1"
  val ClaimsIndependentSqlFrontend: Boolean = false
  val MaximumCanonicalJsonBytes: Int =
    PostgreSqlEnumerationCampaignV2Json.MaximumCanonicalJsonBytes

  private val SealDomain =
    "yrw-enumeration-search-input-seal/v1\u0000"
      .getBytes(StandardCharsets.UTF_8)

  /** Projects one complete produced artifact without rerunning any phase. */
  def project(
      artifact: PostgreSqlEnumerationBatchArtifact
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    requireExactProducedArtifact(artifact)

    val contract = artifact.producerSearchContract
    val unsignedFields = Vector[(String, Value)](
      "candidate_limits" -> candidateLimits(contract),
      "capture_origin" -> string(CaptureOrigin),
      "claims_independent_sql_frontend" -> bool(
        ClaimsIndependentSqlFrontend
      ),
      "edges" -> edges(artifact),
      "protocol_version" -> string(ProtocolVersion),
      "requested_k" -> number(BigInt(artifact.request.planLimit)),
      "require_connected_cover" -> bool(
        artifact.request.requireConnectedCover
      ),
      "schema_version" -> string(SchemaVersion),
      "soft_limits" -> softLimits(contract),
      "source_sql_sha256" -> string(artifact.request.originalSqlSha256),
      "width_bound" -> number(BigInt(artifact.request.widthBound))
    )
    val unsigned = document(obj(unsignedFields))
    val seal = sha256(SealDomain, unsigned.bytes)
    val signed = document(
      obj(
        unsignedFields :+
          ("search_input_seal_sha256" -> string(seal))
      )
    )
    require(
      signed.byteLength <= MaximumCanonicalJsonBytes,
      "search-input document exceeds its exact canonical UTF-8 byte limit"
    )
    signed
  }

  private def edges(
      artifact: PostgreSqlEnumerationBatchArtifact
  ): Value =
    array(
      artifact.hypergraph.edges.map { edge =>
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

  /**
    * Rechecks every hand-off used by the projection instead of trusting a
    * caller-assembled product value. Most invariants are also enforced at their
    * native constructors; retaining them here keeps this serialization boundary
    * fail closed if those internal representations evolve.
    */
  private def requireExactProducedArtifact(
      artifact: PostgreSqlEnumerationBatchArtifact
  ): Unit = {
    require(artifact != null, "search-input artifact must not be null")
    require(
      artifact.analysis.diagnostics.isEmpty &&
        artifact.analysis.query.resultShape ==
          QueryResultShape.EnumerationProjection,
      "search-input artifact must be a supported enumeration analysis"
    )
    val reconstructedHypergraph = artifact.analysis.hypergraph.fold(
      _ =>
        throw new IllegalArgumentException(
          "search-input analysis must reconstruct its hypergraph"
        ),
      identity
    )
    require(
      reconstructedHypergraph == artifact.hypergraph,
      "search-input hypergraph must equal the analysis-derived hypergraph"
    )
    require(
      artifact.analysis.query.relations.map(_.id) ==
        artifact.hypergraph.relationOccurrenceIds,
      "search-input edges must cover the exact relation-occurrence inventory"
    )
    require(
      artifact.hypergraph.edges == artifact.hypergraph.edges.sorted,
      "search-input hyperedges must retain canonical core order"
    )
    artifact.hypergraph.edges.foreach(requireCanonicalEdge)

    val request = artifact.request
    val contract = artifact.producerSearchContract
    require(
      artifact.softGeneration.sourceHypergraph == artifact.hypergraph &&
        artifact.admissibleGeneration.sourceHypergraph == artifact.hypergraph &&
        artifact.candidateInput.hypergraph == artifact.hypergraph &&
        artifact.batch.candidateInput == artifact.candidateInput,
      "search-input artifact must bind one exact hypergraph through every phase"
    )
    require(
      artifact.softGeneration.widthBound == request.widthBound &&
        artifact.admissibleGeneration.widthBound == request.widthBound &&
        artifact.candidateInput.widthBound == request.widthBound &&
        contract.widthBound == request.widthBound,
      "search-input artifact must bind one exact width setting"
    )
    require(
      artifact.admissibleGeneration.requireConnectedCover ==
        request.requireConnectedCover &&
        artifact.candidateInput.requireConnectedCover ==
          request.requireConnectedCover &&
        contract.requireConnectedCover == request.requireConnectedCover,
      "search-input artifact must bind one exact connected-cover setting"
    )
    require(
      artifact.softGeneration.admissible(request.requireConnectedCover) ==
        artifact.admissibleGeneration,
      "search-input artifact must retain the exact admissibility projection"
    )
    val reconstructedInput = CandidateSearchInput
      .create(
        artifact.hypergraph,
        request.widthBound,
        artifact.admissibleGeneration
      )
      .fold(
        _ =>
          throw new IllegalArgumentException(
            "search-input artifact must reconstruct its candidate input"
          ),
        identity
      )
    require(
      reconstructedInput.hypergraph == artifact.candidateInput.hypergraph &&
        reconstructedInput.widthBound == artifact.candidateInput.widthBound &&
        reconstructedInput.requireConnectedCover ==
          artifact.candidateInput.requireConnectedCover &&
        reconstructedInput.candidateBags == artifact.candidateInput.candidateBags,
      "search-input artifact must retain the exact reconstructed candidate input"
    )
    require(
      artifact.batch.requestedLimit == request.planLimit &&
        artifact.batch.search.requestedLimit == request.planLimit &&
        BigInt(request.planLimit) <= BigInt(contract.candidate.maxPlanLimit),
      "search-input artifact must bind one exact requested K"
    )
    require(
      contract.algorithm == PostgreSqlEnumerationProducerSearchContract.Algorithm &&
        contract.structuralUniverse ==
          PostgreSqlEnumerationProducerSearchContract.StructuralUniverse &&
        contract.ranking == PostgreSqlEnumerationProducerSearchContract.Ranking &&
        contract.physicalCoverPolicy ==
          PostgreSqlEnumerationProducerSearchContract.PhysicalCoverPolicy &&
        contract.exhaustionScope ==
          PostgreSqlEnumerationProducerSearchContract.ExhaustionScope &&
        !contract.coverDistinctAssignmentsEnumerated,
      "search-input artifact must retain the frozen structural replay contract"
    )
  }

  private def requireCanonicalEdge(edge: Hyperedge): Unit = {
    require(edge != null, "search-input hyperedge must not be null")
    require(
      edge.id.value == edge.relationOccurrenceId.value,
      "search-input edge identity must equal its analyzed occurrence identity"
    )
    val reconstructed = Hyperedge
      .create(edge.id, edge.relationOccurrenceId, edge.bindings)
      .fold(
        _ =>
          throw new IllegalArgumentException(
            "search-input hyperedge must satisfy the core binding grammar"
          ),
        identity
      )
    require(
      reconstructed == edge,
      "search-input hyperedge bindings must retain canonical core order"
    )
  }

  private def sha256(prefix: Array[Byte], value: Array[Byte]): String = {
    require(prefix != null && value != null, "search-input seal bytes are null")
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
