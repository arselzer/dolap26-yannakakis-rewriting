package at.ac.tuwien.dbai.yannakakis.cli

import java.util.Arrays

import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlEnumerationBatchRequest

/** Exact loaded-data identity committed before enumeration acquisition. */
private[yannakakis] final case class PostgreSqlEnumerationCampaignV2DataContract(
    datasetId: String,
    datasetManifestSha256: String,
    loadReceiptSha256: String,
    dataSemanticsSha256: String
) {
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireIdentifier(
    datasetId,
    "dataset ID"
  )
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireSha256(
    datasetManifestSha256,
    "dataset manifest SHA-256"
  )
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireSha256(
    loadReceiptSha256,
    "load receipt SHA-256"
  )
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireSha256(
    dataSemanticsSha256,
    "data semantics SHA-256"
  )
}

/** One caller-declared toolchain component, sorted by canonical name. */
private[yannakakis] final case class PostgreSqlEnumerationCampaignV2ToolchainComponent(
    name: String,
    version: String,
    artifactSha256: String
) {
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireIdentifier(
    name,
    "toolchain component name"
  )
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireBoundedText(
    version,
    256,
    "toolchain component version"
  )
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireSha256(
    artifactSha256,
    "toolchain component artifact SHA-256"
  )
}

/** Typed inputs whose canonical bytes become the sole producer request. */
private[yannakakis] final case class PostgreSqlEnumerationCampaignV2AuthoringInput(
    sourceSql: String,
    workloadId: String,
    lineageId: String,
    widthBound: Int,
    requestedK: Int,
    requireConnectedCover: Boolean,
    dataContract: PostgreSqlEnumerationCampaignV2DataContract,
    producerEntrypoint: String,
    toolchainComponents: Vector[
      PostgreSqlEnumerationCampaignV2ToolchainComponent
    ]
) {
  require(sourceSql != null, "authoring source SQL must not be null")
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireIdentifier(
    workloadId,
    "workload ID"
  )
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireIdentifier(
    lineageId,
    "lineage ID"
  )
  require(widthBound > 0, "authoring width bound must be positive")
  require(
    requestedK > 0 && requestedK <= 10,
    "authoring requested K must be in 1..10"
  )
  require(dataContract != null, "authoring data contract must not be null")
  PostgreSqlEnumerationCampaignV2AuthoringValidation.requireBoundedText(
    producerEntrypoint,
    512,
    "producer entrypoint"
  )
  require(
    toolchainComponents != null &&
      toolchainComponents.nonEmpty &&
      toolchainComponents.size <= 128 &&
      toolchainComponents.forall(_ != null),
    "toolchain components must be bounded, nonempty, and contain no null"
  )
  require(
    toolchainComponents.map(_.name) ==
      toolchainComponents.map(_.name).sorted.distinct,
    "toolchain components must be sorted by unique canonical name"
  )
}

/**
  * Immutable canonical authoring request and the batch request derived from it.
  *
  * Acquisition code receives this object, never a naked authoring digest or a
  * second copy of SQL/K/width/ConCov settings.
  */
private[yannakakis] final class PostgreSqlEnumerationCampaignV2AuthoringRequest private[cli] (
    val batchRequest: PostgreSqlEnumerationBatchRequest,
    val workloadId: String,
    val lineageId: String,
    private val producerEntrypoint: String,
    val document: PostgreSqlEnumerationCampaignV2JsonDocument,
    val sourceSqlDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
    val dataContractDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
    val featureRegistryDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
    val settingsBundleDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
    val fixedPhysicalConfigDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
    val toolchainBundleDocument: PostgreSqlEnumerationCampaignV2JsonDocument
) {
  require(batchRequest != null, "authoring batch request must not be null")
  require(document != null, "authoring document must not be null")
  require(sourceSqlDocument != null, "source SQL document must not be null")
  require(dataContractDocument != null, "data contract document must not be null")
  require(featureRegistryDocument != null, "feature registry document must not be null")
  require(settingsBundleDocument != null, "settings bundle document must not be null")
  require(fixedPhysicalConfigDocument != null, "fixed physical document must not be null")
  require(toolchainBundleDocument != null, "toolchain bundle document must not be null")

  val sha256: String = document.sha256

  private[cli] def hasProducerEntrypoint(expected: String): Boolean =
    expected != null && producerEntrypoint == expected

  private[cli] def hasExactCanonicalBytes(bytes: Array[Byte]): Boolean =
    bytes != null && Arrays.equals(document.bytes, bytes)
}

private[yannakakis] object PostgreSqlEnumerationCampaignV2AuthoringRequest {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._

  val SchemaVersion: String =
    "yrw-enumeration-registry-authoring-request/v2"
  val DataContractSchema: String = "yrw-enumeration-data-contract/v2"
  val SettingsBundleSchema: String = "yrw-enumeration-settings-bundle/v2"
  val ToolchainBundleSchema: String = "yrw-enumeration-toolchain-bundle/v2"
  val Admissibility: String = "soft-width-bound-v1"

  /**
    * This first handoff slice is deliberately offline-only. Live catalog and
    * whole-plan capture require a later contract and cannot be requested here.
    */
  val IncludeCatalog: Boolean = false
  val IncludeWholePlan: Boolean = false

  private val FrozenSessionSettings: Vector[(String, String)] = Vector(
    "datestyle" -> "ISO, MDY",
    "extra_float_digits" -> "3",
    "standard_conforming_strings" -> "on",
    "timezone" -> "UTC"
  )

  def create(
      input: PostgreSqlEnumerationCampaignV2AuthoringInput
  ): Either[String, PostgreSqlEnumerationCampaignV2AuthoringRequest] = {
    if (input == null) return Left("authoring-input-null")

    PostgreSqlEnumerationBatchRequest
      .create(
        input.sourceSql,
        input.widthBound,
        input.requestedK,
        input.requireConnectedCover
      )
      .left
      .map(_ => "authoring-batch-request-invalid")
      .flatMap { batchRequest =>
        if (input.sourceSql.isEmpty) Left("authoring-source-sql-empty")
        else {
          try Right(build(input, batchRequest))
          catch {
            case _: IllegalArgumentException =>
              Left("authoring-canonicalization-invalid")
          }
        }
      }
  }

  private def build(
      input: PostgreSqlEnumerationCampaignV2AuthoringInput,
      batchRequest: PostgreSqlEnumerationBatchRequest
  ): PostgreSqlEnumerationCampaignV2AuthoringRequest = {
    val source = PostgreSqlEnumerationCampaignV2Json.sourceSqlDocument(
      input.sourceSql
    )
    val featureRegistry =
      PostgreSqlEnumerationCampaignV2Json.structuralFeatureRegistryDocument()
    val dataContract = dataContractDocument(input.dataContract)
    val fixedPhysical = fixedPhysicalConfigDocument()
    val settings = settingsBundleDocument(input, fixedPhysical)
    val toolchain = toolchainBundleDocument(input)
    val request = document(
      obj(
        "data_contract" -> embeddedDocument(dataContract),
        "feature_registry" -> embeddedDocument(featureRegistry),
        "lineage_id" -> string(input.lineageId),
        "protocol_version" -> string(
          PostgreSqlEnumerationCampaignV2Json.ProtocolVersion
        ),
        "schema_version" -> string(SchemaVersion),
        "settings_bundle" -> embeddedDocument(settings),
        "source_sql" -> embeddedDocument(source),
        "toolchain_bundle" -> embeddedDocument(toolchain),
        "workload_id" -> string(input.workloadId)
      )
    )
    new PostgreSqlEnumerationCampaignV2AuthoringRequest(
      batchRequest,
      input.workloadId,
      input.lineageId,
      input.producerEntrypoint,
      request,
      source,
      dataContract,
      featureRegistry,
      settings,
      fixedPhysical,
      toolchain
    )
  }

  private def dataContractDocument(
      value: PostgreSqlEnumerationCampaignV2DataContract
  ): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(
      obj(
        "data_semantics_sha256" -> string(value.dataSemanticsSha256),
        "dataset_id" -> string(value.datasetId),
        "dataset_manifest_sha256" -> string(
          value.datasetManifestSha256
        ),
        "load_receipt_sha256" -> string(value.loadReceiptSha256),
        "schema_version" -> string(DataContractSchema)
      )
    )

  private def fixedPhysicalConfigDocument()
      : PostgreSqlEnumerationCampaignV2JsonDocument =
    document(
      obj(
        "cte_materialization" -> string("planner-controlled"),
        "cte_materialization_scope" -> string("all-emitted-ctes-v1"),
        "physical_variant_id_scheme" -> string(
          "postgresql_candidate_reduction_variant_v1"
        ),
        "reduction_mode" -> string("full"),
        "reduction_order" -> string("canonical_plan_schedule_v1"),
        "sql_shape" -> string("one-statement-cte")
      )
    )

  private def settingsBundleDocument(
      input: PostgreSqlEnumerationCampaignV2AuthoringInput,
      fixedPhysical: PostgreSqlEnumerationCampaignV2JsonDocument
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    val session = FrozenSessionSettings.map { case (name, value) =>
      obj("name" -> string(name), "value" -> string(value))
    }
    document(
      obj(
        "feature_capture" -> obj(
          "analyze" -> bool(false),
          "failure_policy" -> string("record-explicit-state"),
          "include_catalog" -> bool(IncludeCatalog),
          "include_whole_plan" -> bool(IncludeWholePlan),
          "same_session" -> bool(true)
        ),
        "fixed_physical_config" -> embeddedDocument(fixedPhysical),
        "postgres_session" -> array(session),
        "schema_version" -> string(SettingsBundleSchema),
        "search" -> obj(
          "admissibility" -> string(Admissibility),
          "algorithm" -> string("top-k-candidate-td-v1"),
          "requested_k" -> number(BigInt(input.requestedK)),
          "require_connected_cover" -> bool(
            input.requireConnectedCover
          ),
          "width_bound" -> number(BigInt(input.widthBound))
        )
      )
    )
  }

  private def toolchainBundleDocument(
      input: PostgreSqlEnumerationCampaignV2AuthoringInput
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    val components = input.toolchainComponents.map { component =>
      obj(
        "artifact_sha256" -> string(component.artifactSha256),
        "name" -> string(component.name),
        "version" -> string(component.version)
      )
    }
    document(
      obj(
        "components" -> array(components),
        "producer_entrypoint" -> string(input.producerEntrypoint),
        "schema_version" -> string(ToolchainBundleSchema)
      )
    )
  }
}

private[cli] object PostgreSqlEnumerationCampaignV2AuthoringValidation {
  private val IdentifierPattern = "[a-z][a-z0-9._:-]{0,191}".r
  private val Sha256Pattern = "[0-9a-f]{64}".r

  def requireIdentifier(value: String, where: String): Unit =
    require(
      value != null && IdentifierPattern.pattern.matcher(value).matches(),
      s"$where must be a canonical identifier"
    )

  def requireSha256(value: String, where: String): Unit =
    require(
      value != null && Sha256Pattern.pattern.matcher(value).matches(),
      s"$where must be a lowercase SHA-256"
    )

  def requireBoundedText(
      value: String,
      maximumCharacters: Int,
      where: String
  ): Unit = {
    require(maximumCharacters > 0, "text bound must be positive")
    require(
      value != null && value.nonEmpty &&
        value.length <= maximumCharacters &&
        value.indexOf('\u0000') < 0,
      s"$where must be bounded nonempty text"
    )
    PostgreSqlEnumerationCampaignV2CanonicalJson.requireValidUnicode(
      value,
      where
    )
  }
}
