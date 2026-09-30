package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Arrays

import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlEnumerationBatchArtifact

/** Exact immutable-byte evidence for one staged bundle child. */
private[cli] final case class PostgreSqlEnumerationSearchReplayBundleV1FileEvidence(
    byteLength: Long,
    sha256: String
) {
  require(
    byteLength > 0L &&
      byteLength <= PostgreSqlEnumerationCampaignV2Json.MaximumCanonicalJsonBytes.toLong,
    "bundle child must fit the canonical document envelope"
  )
  require(
    sha256 != null && sha256.matches("[0-9a-f]{64}"),
    "bundle child SHA-256 must be canonical"
  )
}

/**
  * Canonical logical-commit record for one same-artifact package/input pair.
  *
  * The two child hashes bind the validated v2 package and captured search
  * input, including their embedded seals and producer limits. The record is
  * linked into the final bundle only after both data names are durable. Its
  * same-artifact statement is an implementation claim of the dedicated
  * one-call producer, not an independently cryptographic proof.
  */
private[cli] object PostgreSqlEnumerationSearchReplayBundleV1Json {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._

  val SchemaVersion: String =
    "yrw-enumeration-search-replay-acquisition-bundle/v1"
  val ProtocolVersion: String =
    PostgreSqlEnumerationSearchReplayV1Json.ProtocolVersion
  val PackageFileName: String = "package.json"
  val SearchInputFileName: String = "search-input.json"
  val CompleteFileName: String = "complete.json"

  private val SealDomain =
    "yrw-enumeration-search-replay-acquisition-bundle-seal/v1\u0000"
      .getBytes(StandardCharsets.UTF_8)

  def build(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      artifact: PostgreSqlEnumerationBatchArtifact,
      producerEntrypoint: String,
      packageEvidence: PostgreSqlEnumerationSearchReplayBundleV1FileEvidence,
      stagedSearchEvidence: PostgreSqlEnumerationSearchReplayBundleV1FileEvidence,
      searchInput: PostgreSqlEnumerationCampaignV2JsonDocument
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    require(authoring != null, "bundle authoring request must not be null")
    require(artifact != null, "bundle artifact must not be null")
    require(
      producerEntrypoint != null && producerEntrypoint.nonEmpty &&
        authoring.hasProducerEntrypoint(producerEntrypoint),
      "bundle producer entrypoint must equal its authoring authority"
    )
    require(packageEvidence != null, "bundle package evidence must not be null")
    require(
      stagedSearchEvidence != null,
      "staged bundle search evidence must not be null"
    )
    require(searchInput != null, "bundle search input must not be null")
    require(
      artifact.request == authoring.batchRequest &&
        artifact.request.originalSqlSha256 ==
          authoring.batchRequest.originalSqlSha256,
      "bundle package and search input must share one authoring request"
    )
    val expectedSearchInput =
      PostgreSqlEnumerationSearchReplayV1Json.project(artifact)
    require(
      Arrays.equals(expectedSearchInput.bytes, searchInput.bytes),
      "bundle search input must be the exact projection of its artifact"
    )

    val searchEvidence =
      PostgreSqlEnumerationSearchReplayBundleV1FileEvidence(
        searchInput.byteLength.toLong,
        searchInput.sha256
      )
    require(
      stagedSearchEvidence == searchEvidence,
      "staged search input must equal the same-artifact projection"
    )
    val unsignedFields = Vector[(String, Value)](
      "authoring_request_sha256" -> string(authoring.sha256),
      "capture_origin" -> string(
        PostgreSqlEnumerationSearchReplayV1Json.CaptureOrigin
      ),
      "children" -> obj(
        "package" -> child(PackageFileName, packageEvidence),
        "search_input" -> child(SearchInputFileName, searchEvidence)
      ),
      "claims_cryptographic_same_artifact" -> bool(false),
      "claims_same_one_pass_artifact" -> bool(true),
      "outcomes_in_scope" -> bool(false),
      "producer_entrypoint" -> string(producerEntrypoint),
      "protocol_version" -> string(ProtocolVersion),
      "reportable" -> bool(false),
      "requested_k" -> number(BigInt(artifact.request.planLimit)),
      "require_connected_cover" -> bool(
        artifact.request.requireConnectedCover
      ),
      "schema_version" -> string(SchemaVersion),
      "source_sql_sha256" -> string(artifact.request.originalSqlSha256),
      "width_bound" -> number(BigInt(artifact.request.widthBound))
    )
    val unsigned = document(obj(unsignedFields))
    document(
      obj(
        unsignedFields :+
          ("bundle_seal_sha256" -> string(sha256(SealDomain, unsigned.bytes)))
      )
    )
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
