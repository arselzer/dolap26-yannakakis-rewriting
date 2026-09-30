package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/**
  * Canonical, outcome-free receipt for one accepted terminal disposition.
  *
  * Diagnostic prose/context, SQL bytes, execution outcomes, plans, paths, and
  * timing never cross this serializer boundary. The exact producer contract
  * is safe to embed: it contains only algorithms, settings, limits, and hashes.
  */
private[cli] object PostgreSqlEnumerationAcquisitionTerminalReceiptJson {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._

  val SchemaVersion: String =
    "yrw-enumeration-acquisition-terminal-receipt/v1"
  val ProtocolVersion: String =
    "yrw-enumeration-acquisition-disposition/v1"
  val SealDomain: String =
    "yrw-enumeration-acquisition-terminal-receipt-seal/v1"
  val SealScheme: String =
    "sha256-domain-nul-canonical-json-with-final-lf-v1"
  val ResourceOriginScheme: String =
    "exact-producer-phase-code-severity-context-shape-and-classifier-bindings-v1"

  def build(
      terminal: PostgreSqlEnumerationAcquisitionDisposition.Unpackageable
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    require(terminal != null, "terminal disposition must not be null")
    val authoring = terminal.authoring
    val result = terminal.result
    require(
      authoring.hasProducerEntrypoint(
        PostgreSqlEnumerationSearchReplayV2AcquisitionCli.Entrypoint
      ),
      "terminal receipt requires the exact replay-v2 producer entrypoint"
    )
    require(
      result.request == authoring.batchRequest &&
        result.request.originalSqlSha256 ==
          authoring.batchRequest.originalSqlSha256,
      "terminal receipt must retain the exact authoring request"
    )

    val diagnosticValues = terminal.diagnostics.map(diagnostic =>
      obj(
        "code" -> string(diagnostic.code.id),
        "severity" -> string(diagnostic.severity.id)
      )
    )
    val originValues = terminal.resourceOrigins.map(origin => string(origin.id))
    val fields = Vector[(String, Value)](
      "authoring_request_sha256" -> string(authoring.sha256),
      "claims_complete_registered_denominator" -> bool(false),
      "claims_cryptographic_same_result" -> bool(false),
      "claims_external_durable_chronology" -> bool(false),
      "claims_no_unrecorded_acquisition_attempts" -> bool(false),
      "claims_same_one_pass_result" -> bool(true),
      "classification" -> string(
        "inactive-outcome-free-acquisition-terminal"
      ),
      "completed_phase" -> string(result.completedPhase.id),
      "data_contract_sha256" -> string(
        authoring.dataContractDocument.sha256
      ),
      "diagnostics" -> array(diagnosticValues),
      "diagnostic_detail_in_scope" -> bool(false),
      "disposition" -> string("terminal-unpackageable"),
      "execution_outcomes_in_scope" -> bool(false),
      "feature_registry_sha256" -> string(
        authoring.featureRegistryDocument.sha256
      ),
      "fixed_physical_config_sha256" -> string(
        authoring.fixedPhysicalConfigDocument.sha256
      ),
      "lineage_id" -> string(authoring.lineageId),
      "producer_entrypoint" -> string(
        PostgreSqlEnumerationSearchReplayV2AcquisitionCli.Entrypoint
      ),
      "producer_search_contract" -> embeddedDocument(
        terminal.producerSearchContract
      ),
      "producer_search_contract_sha256" -> string(
        terminal.producerSearchContract.sha256
      ),
      "producer_status" -> string(result.status.id),
      "production_policy" -> bool(false),
      "protocol_version" -> string(ProtocolVersion),
      "receipt_seal_domain" -> string(SealDomain),
      "receipt_seal_scheme" -> string(SealScheme),
      "reportable" -> bool(false),
      "resource_origin_scheme" -> string(ResourceOriginScheme),
      "resource_origins" -> array(originValues),
      "schema_version" -> string(SchemaVersion),
      "settings_bundle_sha256" -> string(
        authoring.settingsBundleDocument.sha256
      ),
      "source_sql_document_sha256" -> string(
        authoring.sourceSqlDocument.sha256
      ),
      "source_sql_sha256" -> string(result.request.originalSqlSha256),
      "sql_bytes_in_scope" -> bool(false),
      "terminal_class" -> string(terminal.terminalClass.id),
      "timing_measurements_in_scope" -> bool(false),
      "toolchain_bundle_sha256" -> string(
        authoring.toolchainBundleDocument.sha256
      ),
      "workload_id" -> string(authoring.workloadId)
    )
    val body = document(obj(fields))
    val seal = sealSha256(body)
    document(obj(fields :+ ("receipt_seal_sha256" -> string(seal))))
  }

  /** Test-visible exact domain-separated preimage implementation. */
  private[cli] def sealSha256(
      body: PostgreSqlEnumerationCampaignV2JsonDocument
  ): String = {
    require(body != null, "terminal receipt seal body must not be null")
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(SealDomain.getBytes(StandardCharsets.UTF_8))
    digest.update(0.toByte)
    body.updateDigest(digest)
    lowerHex(digest.digest())
  }

  private def lowerHex(bytes: Array[Byte]): String = {
    val alphabet = "0123456789abcdef"
    val result = new StringBuilder(bytes.length * 2)
    bytes.foreach { byte =>
      val unsigned = byte & 0xff
      result.append(alphabet.charAt(unsigned >>> 4))
      result.append(alphabet.charAt(unsigned & 0x0f))
    }
    result.result()
  }
}
