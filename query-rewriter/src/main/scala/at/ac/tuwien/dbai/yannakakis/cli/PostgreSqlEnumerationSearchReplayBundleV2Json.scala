package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Arrays

import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlEnumerationBatchRequest

/** Canonical logical-commit record for one fallback-capable v2 bundle. */
private[cli] object PostgreSqlEnumerationSearchReplayBundleV2Json {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._
  import PostgreSqlEnumerationCampaignV2EligibleResult._

  val SchemaVersion: String =
    "yrw-enumeration-search-replay-acquisition-bundle/v2"
  val ProtocolVersion: String =
    PostgreSqlEnumerationSearchReplayV2Json.ProtocolVersion
  val PackageFileName: String = "package.json"
  val SearchInputFileName: String = "search-input.json"
  val CompleteFileName: String = "complete.json"

  private val SealDomain =
    "yrw-enumeration-search-replay-acquisition-bundle-seal/v2\u0000"
      .getBytes(StandardCharsets.UTF_8)

  def build(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      eligible: PostgreSqlEnumerationCampaignV2EligibleResult,
      producerEntrypoint: String,
      packageEvidence: PostgreSqlEnumerationSearchReplayBundleV1FileEvidence,
      stagedSearchEvidence: PostgreSqlEnumerationSearchReplayBundleV1FileEvidence,
      searchInput: PostgreSqlEnumerationCampaignV2JsonDocument
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    require(authoring != null, "v2 bundle authoring request must not be null")
    require(eligible != null, "v2 bundle eligible result must not be null")
    require(
      producerEntrypoint ==
        PostgreSqlEnumerationSearchReplayV2AcquisitionCli.Entrypoint &&
        authoring.hasProducerEntrypoint(producerEntrypoint),
      "v2 bundle producer entrypoint must equal its fixed authoring authority"
    )
    require(packageEvidence != null, "v2 package evidence must not be null")
    require(
      stagedSearchEvidence != null,
      "v2 staged search evidence must not be null"
    )
    require(searchInput != null, "v2 bundle search input must not be null")

    val request = requestOf(eligible)
    require(
      request == authoring.batchRequest &&
        request.originalSqlSha256 == authoring.batchRequest.originalSqlSha256,
      "v2 bundle package and input must share one authoring request"
    )
    val expectedSearchInput =
      PostgreSqlEnumerationSearchReplayV2Json.project(eligible)
    require(
      Arrays.equals(expectedSearchInput.bytes, searchInput.bytes),
      "v2 bundle search input must be the exact eligible-result projection"
    )
    val searchEvidence =
      PostgreSqlEnumerationSearchReplayBundleV1FileEvidence(
        searchInput.byteLength.toLong,
        searchInput.sha256
      )
    require(
      stagedSearchEvidence == searchEvidence,
      "v2 staged search input must equal the eligible-result projection"
    )

    val unsignedFields = Vector[(String, Value)](
      "acquisition_status" -> string(acquisitionStatus(eligible)),
      "authoring_request_sha256" -> string(authoring.sha256),
      "capture_origin" -> string(
        PostgreSqlEnumerationSearchReplayV2Json.CaptureOrigin
      ),
      "children" -> obj(
        "package" -> child(PackageFileName, packageEvidence),
        "search_input" -> child(SearchInputFileName, searchEvidence)
      ),
      "claims_complete_registered_denominator" -> bool(false),
      "claims_cryptographic_same_result" -> bool(false),
      "claims_same_one_pass_result" -> bool(true),
      "outcomes_in_scope" -> bool(false),
      "producer_entrypoint" -> string(producerEntrypoint),
      "protocol_version" -> string(ProtocolVersion),
      "reportable" -> bool(false),
      "requested_k" -> number(BigInt(request.planLimit)),
      "require_connected_cover" -> bool(request.requireConnectedCover),
      "schema_version" -> string(SchemaVersion),
      "source_sql_sha256" -> string(request.originalSqlSha256),
      "width_bound" -> number(BigInt(request.widthBound))
    )
    val unsigned = document(obj(unsignedFields))
    document(
      obj(
        unsignedFields :+
          ("bundle_seal_sha256" -> string(
            sha256(SealDomain, unsigned.bytes)
          ))
      )
    )
  }

  private def requestOf(
      eligible: PostgreSqlEnumerationCampaignV2EligibleResult
  ): PostgreSqlEnumerationBatchRequest = eligible match {
    case Found(artifact)          => artifact.request
    case fallback: NativeFallback => fallback.result.request
  }

  private def acquisitionStatus(
      eligible: PostgreSqlEnumerationCampaignV2EligibleResult
  ): String = eligible match {
    case _: Found => PostgreSqlEnumerationSearchReplayBundleV2Pair.FoundStatus
    case _: NativeFallback =>
      PostgreSqlEnumerationSearchReplayBundleV2Pair.FallbackStatus
  }

  private def child(
      fileName: String,
      evidence: PostgreSqlEnumerationSearchReplayBundleV1FileEvidence
  ): Value =
    obj(
      "byte_length" -> number(BigInt(evidence.byteLength)),
      "file_name" -> string(fileName),
      "sha256" -> string(evidence.sha256)
    )

  private def sha256(prefix: Array[Byte], value: Array[Byte]): String = {
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
