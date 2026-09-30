package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.security.MessageDigest
import java.util.Arrays

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

import com.fasterxml.jackson.core.{JsonFactory, JsonParser}
import com.fasterxml.jackson.databind.{DeserializationFeature, JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.ObjectNode

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationCampaignV2PlanAction,
  PostgreSqlEnumerationCampaignV2WholePlanBatch,
  PostgreSqlEnumerationCampaignV2WholePlanCapture,
  PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits,
  PostgreSqlEnumerationCampaignV2WholePlanRequest
}

/** Stable, deliberately non-diagnostic failures at the exact request boundary. */
private[yannakakis] sealed trait PostgreSqlEnumerationPlanCaptureV1ProtocolFailure
    extends Product
    with Serializable {
  def code: String
}

private[yannakakis] object PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
  case object MissingRequest
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.request-missing"
  }
  case object RequestTooLarge
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.request-oversized"
  }
  case object InvalidUtf8
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.request-utf8-invalid"
  }
  case object InvalidJson
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.request-json-invalid"
  }
  case object NonCanonicalRequest
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.request-noncanonical"
  }
  case object RequestContractMismatch
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.request-contract-mismatch"
  }
  case object RequestBindingMismatch
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.request-binding-mismatch"
  }
  case object CaptureMismatch
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.capture-mismatch"
  }
  case object AssemblyTooLarge
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.assembly-oversized"
  }
  case object AssemblyFailed
      extends PostgreSqlEnumerationPlanCaptureV1ProtocolFailure {
    override val code: String = "plan-capture-v1.assembly-failed"
  }
}

/** Immutable capture authority admitted from one exact canonical request. */
private[yannakakis] final class PostgreSqlEnumerationPlanCaptureV1Request private[cli] (
    val document: PostgreSqlEnumerationCampaignV2JsonDocument,
    val captureRequestSha256: String,
    val wholePlanRequest: PostgreSqlEnumerationCampaignV2WholePlanRequest,
    val acquisitionPackageSha256: String,
    val actionRegistrySha256: String,
    val authoringRequestSha256: String,
    val dataContractSha256: String,
    val limitsSha256: String,
    val sessionContractSha256: String,
    val sourceSqlSha256: String
) {
  require(document != null, "capture request document is null")
  require(wholePlanRequest != null, "whole-plan request is null")
  private val hashes = Vector(
    captureRequestSha256,
    acquisitionPackageSha256,
    actionRegistrySha256,
    authoringRequestSha256,
    dataContractSha256,
    limitsSha256,
    sessionContractSha256,
    sourceSqlSha256
  )
  require(hashes.forall(value => value != null && value.matches("[0-9a-f]{64}")))
}

/**
  * Native admission for the capture-affecting portion of plan-capture request
  * v1. The complete embedded v2 package is preserved and hash/seal bound, but
  * this deliberately does not replace the full Python v2 semantic validator.
  * Python `validate_raw_capture_assembly` reruns that authority before a
  * sidecar can be accepted.
  */
private[yannakakis] object PostgreSqlEnumerationPlanCaptureV1RequestParser {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._
  import PostgreSqlEnumerationPlanCaptureV1ProtocolFailure._

  val MaximumRequestBytes: Int = 16 * 1024 * 1024
  /**
    * Machine-visible trust-boundary flags. Admission authorizes only bounded
    * read-only capture of the exact sealed SQL; it is not evidence acceptance.
    */
  val ClaimsFullV2SemanticValidity: Boolean = false
  val RequiresPinnedPythonValidationBeforeEvidenceUse: Boolean = true
  private val MaximumJsonDepth = 128
  private val MaximumJsonValues = 200000
  private val RequestSealDomain =
    "yrw-enumeration-plan-capture-request-seal/v1\u0000"
      .getBytes(StandardCharsets.UTF_8)
  private val PackageSealDomain =
    "yrw-enumeration-offline-acquisition-package-seal/v2\u0000"
      .getBytes(StandardCharsets.UTF_8)
  private val RegistrySealDomain =
    "yrw-enumeration-action-registry-seal/v2\u0000"
      .getBytes(StandardCharsets.UTF_8)

  private val ProtocolVersion = "yrw-enumeration-plan-capture-protocol/v1"
  private val RequestSchema = "yrw-enumeration-plan-capture-request/v1"
  private val V2ProtocolVersion = "yrw-enumeration-campaign-protocol/v2"
  private val V2PackageSchema = "yrw-enumeration-offline-acquisition-package/v2"
  private val V2RegistrySchema = "yrw-enumeration-action-registry/v2"
  private val V2AuthoringSchema =
    "yrw-enumeration-registry-authoring-request/v2"

  private val ValidatorHashes = Map(
    "v2_protocol_source_sha256" ->
      "ee236dde042963f4d74434517882e56185c97b91459d7cc2a0663393e32a66cd",
    "v2_producer_evidence_source_sha256" ->
      "04aac0d9e0e8b531b03b465111298dc6dedf264011aee7e93dee5c8861178938",
    "v2_package_contract_source_sha256" ->
      "13581731c365ba0af8c7659127fb7c3173dfd372dcbf6042fe16fa3cf652ad95"
  )

  private val RootKeys = Set(
    "actions",
    "capture_request_seal_sha256",
    "classification",
    "data_contract",
    "limits",
    "outcomes_in_scope",
    "production_policy",
    "protocol_version",
    "reportable",
    "schema_version",
    "session_contract",
    "source_acquisition_package",
    "source_package_bindings",
    "source_sql"
  )
  private val PackageKeys = Set(
    "acquisition_package_seal_sha256",
    "acquisition_status",
    "action_registry",
    "authoring_request",
    "bindings",
    "canonical_decompositions",
    "classification",
    "feature_value_bundles",
    "non_sql_assets",
    "outcomes_in_scope",
    "producer_search_contract",
    "producer_search_transcript",
    "production_policy",
    "protocol_version",
    "reportable",
    "resource_statistics_certificate",
    "schema_version",
    "semantic_search_core"
  )
  private val RegistryKeys = Set(
    "action_registry_seal_sha256",
    "actions",
    "bindings",
    "protocol_version",
    "schema_version"
  )
  private val RegistryActionKeys = Set(
    "action_id",
    "assets",
    "candidate_id",
    "candidate_rank",
    "kind",
    "ordered_stages",
    "physical_variant_id",
    "physical_variant_id_scheme",
    "rendered_sql",
    "stage_id_scheme",
    "stage_order_scheme"
  )
  private val CaptureActionKeys = Set(
    "action_id",
    "action_kind",
    "action_position",
    "candidate_id",
    "candidate_rank",
    "rendered_sql"
  )
  private val ActionSqlKeys = Set(
    "byte_length",
    "encoding",
    "hash_scheme",
    "schema_version",
    "sha256",
    "utf8_hex"
  )
  private val SourceSqlKeys = Set(
    "byte_length",
    "encoding",
    "schema_version",
    "sha256",
    "utf8_hex"
  )
  private val DataKeys = Set(
    "data_semantics_sha256",
    "dataset_id",
    "dataset_manifest_sha256",
    "load_receipt_sha256",
    "schema_version"
  )
  private val BindingKeys = Set(
    "acquisition_package_schema",
    "acquisition_package_seal_sha256",
    "acquisition_package_sha256",
    "action_registry_schema",
    "action_registry_seal_sha256",
    "action_registry_sha256",
    "authoring_request_schema",
    "authoring_request_sha256",
    "data_contract_sha256",
    "settings_bundle_sha256",
    "source_sql_sha256",
    "v2_package_contract_source_sha256",
    "v2_producer_evidence_source_sha256",
    "v2_protocol_source_sha256"
  )
  private val AuthoringKeys = Set(
    "data_contract",
    "feature_registry",
    "lineage_id",
    "protocol_version",
    "schema_version",
    "settings_bundle",
    "source_sql",
    "toolchain_bundle",
    "workload_id"
  )
  private val SettingsKeys = Set(
    "feature_capture",
    "fixed_physical_config",
    "postgres_session",
    "schema_version",
    "search"
  )
  private val FeatureCaptureKeys = Set(
    "analyze",
    "failure_policy",
    "include_catalog",
    "include_whole_plan",
    "same_session"
  )

  private val Mapper: ObjectMapper = {
    val factory = new JsonFactory()
    factory.setStreamReadConstraints(
      com.fasterxml.jackson.core.StreamReadConstraints
        .builder()
        .maxNestingDepth(MaximumJsonDepth)
        .maxNumberLength(64)
        .maxStringLength(MaximumRequestBytes)
        .build()
    )
    factory.enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
    factory.disable(JsonParser.Feature.ALLOW_NON_NUMERIC_NUMBERS)
    new ObjectMapper(factory)
      .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS)
  }

  def parse(
      bytes: Array[Byte]
  ): Either[
    PostgreSqlEnumerationPlanCaptureV1ProtocolFailure,
    PostgreSqlEnumerationPlanCaptureV1Request
  ] = {
    if (bytes == null || bytes.isEmpty) return Left(MissingRequest)
    if (bytes.length > MaximumRequestBytes) return Left(RequestTooLarge)
    val snapshot = bytes.clone()
    if (!strictUtf8(snapshot)) return Left(InvalidUtf8)
    val root =
      try Mapper.readTree(snapshot)
      catch { case NonFatal(_) => return Left(InvalidJson) }
    if (root == null || !withinStructuralBounds(root)) return Left(InvalidJson)
    val canonical =
      try canonicalDocument(root)
      catch { case NonFatal(_) => return Left(InvalidJson) }
    if (!Arrays.equals(snapshot, canonical.bytes)) return Left(NonCanonicalRequest)
    validateCanonical(root, canonical)
  }

  private def validateCanonical(
      root: JsonNode,
      exactDocument: PostgreSqlEnumerationCampaignV2JsonDocument
  ): Either[
    PostgreSqlEnumerationPlanCaptureV1ProtocolFailure,
    PostgreSqlEnumerationPlanCaptureV1Request
  ] = {
    if (!closed(root, RootKeys)) return Left(RequestContractMismatch)
    if (
      !textEquals(root, "schema_version", RequestSchema) ||
      !textEquals(root, "protocol_version", ProtocolVersion) ||
      !textEquals(root, "classification", "inactive-preexecution-plan-capture") ||
      !booleanEquals(root, "outcomes_in_scope", false) ||
      !booleanEquals(root, "production_policy", false) ||
      !booleanEquals(root, "reportable", false)
    ) return Left(RequestContractMismatch)

    val sourcePackage = objectField(root, "source_acquisition_package")
      .filter(closed(_, PackageKeys))
      .getOrElse(return Left(RequestContractMismatch))
    if (
      !textEquals(sourcePackage, "schema_version", V2PackageSchema) ||
      !textEquals(sourcePackage, "protocol_version", V2ProtocolVersion) ||
      !textEquals(sourcePackage, "classification", "offline-outcome-free-acquisition") ||
      !booleanEquals(sourcePackage, "outcomes_in_scope", false) ||
      !booleanEquals(sourcePackage, "production_policy", false) ||
      !booleanEquals(sourcePackage, "reportable", false) ||
      !Set("found-actions", "typed-native-fallback").contains(
        text(sourcePackage, "acquisition_status").getOrElse("")
      )
    ) return Left(RequestContractMismatch)

    val authoringNode = objectField(sourcePackage, "authoring_request")
      .getOrElse(return Left(RequestContractMismatch))
    if (
      !closed(authoringNode, AuthoringKeys) ||
      !textEquals(authoringNode, "schema_version", V2AuthoringSchema) ||
      !textEquals(authoringNode, "protocol_version", V2ProtocolVersion) ||
      !text(authoringNode, "workload_id").exists(_.matches("[a-z][a-z0-9._:-]{0,191}")) ||
      !text(authoringNode, "lineage_id").exists(_.matches("[a-z][a-z0-9._:-]{0,191}"))
    ) return Left(RequestContractMismatch)
    val authoringDocument =
      try canonicalDocument(authoringNode)
      catch { case NonFatal(_) => return Left(RequestContractMismatch) }
    val settingsNode = objectField(authoringNode, "settings_bundle")
      .getOrElse(return Left(RequestContractMismatch))
    val sourceNode = objectField(authoringNode, "source_sql")
      .getOrElse(return Left(RequestContractMismatch))
    val dataNode = objectField(authoringNode, "data_contract")
      .getOrElse(return Left(RequestContractMismatch))
    if (!validateSourceSql(sourceNode) || !validateData(dataNode))
      return Left(RequestContractMismatch)
    if (
      !closed(settingsNode, SettingsKeys) ||
      !textEquals(settingsNode, "schema_version", "yrw-enumeration-settings-bundle/v2")
    ) return Left(RequestContractMismatch)
    val featureCapture = objectField(settingsNode, "feature_capture")
      .getOrElse(return Left(RequestContractMismatch))
    if (
      !closed(featureCapture, FeatureCaptureKeys) ||
      !booleanEquals(featureCapture, "analyze", false) ||
      !textEquals(featureCapture, "failure_policy", "record-explicit-state") ||
      !isBoolean(featureCapture, "include_catalog") ||
      !booleanEquals(featureCapture, "include_whole_plan", false) ||
      !booleanEquals(featureCapture, "same_session", true)
    ) return Left(RequestContractMismatch)

    val registry = objectField(sourcePackage, "action_registry")
      .filter(closed(_, RegistryKeys))
      .getOrElse(return Left(RequestContractMismatch))
    if (
      !textEquals(registry, "schema_version", V2RegistrySchema) ||
      !textEquals(registry, "protocol_version", V2ProtocolVersion)
    ) return Left(RequestContractMismatch)

    val packageSeal = text(sourcePackage, "acquisition_package_seal_sha256")
      .filter(isSha256)
      .getOrElse(return Left(RequestContractMismatch))
    val registrySeal = text(registry, "action_registry_seal_sha256")
      .filter(isSha256)
      .getOrElse(return Left(RequestContractMismatch))
    if (
      packageSeal != seal(sourcePackage, "acquisition_package_seal_sha256", PackageSealDomain) ||
      registrySeal != seal(registry, "action_registry_seal_sha256", RegistrySealDomain)
    ) return Left(RequestBindingMismatch)

    val packageHash = canonicalDocument(sourcePackage).sha256
    val registryHash = canonicalDocument(registry).sha256
    val authoringHash = authoringDocument.sha256
    val dataHash = canonicalDocument(dataNode).sha256
    val settingsHash = canonicalDocument(settingsNode).sha256
    val sourceHash = text(sourceNode, "sha256").get

    if (!validateInternalBindings(
          sourcePackage,
          registry,
          authoringNode,
          authoringHash,
          registryHash,
          dataHash,
          settingsHash,
          sourceHash
        )) return Left(RequestBindingMismatch)

    val registryActions = arrayField(registry, "actions")
      .filter(values => values.nonEmpty && values.size <= 11)
      .getOrElse(return Left(RequestContractMismatch))
    val requestActions = arrayField(root, "actions")
      .filter(_.size == registryActions.size)
      .getOrElse(return Left(RequestContractMismatch))
    val typedActions = Vector.newBuilder[PostgreSqlEnumerationCampaignV2PlanAction]
    registryActions.zip(requestActions).zipWithIndex.foreach {
      case ((registryAction, requestAction), position) =>
        if (
          !closed(registryAction, RegistryActionKeys) ||
          !closed(requestAction, CaptureActionKeys)
        ) return Left(RequestContractMismatch)
        val actionId = text(registryAction, "action_id")
          .getOrElse(return Left(RequestContractMismatch))
        val kind = text(registryAction, "kind")
          .getOrElse(return Left(RequestContractMismatch))
        val expectedId = if (position == 0) "native" else f"rewrite-$position%04d"
        val expectedKind = if (position == 0) "native" else "retained_candidate"
        if (actionId != expectedId || kind != expectedKind)
          return Left(RequestContractMismatch)
        if (
          !textEquals(requestAction, "action_id", actionId) ||
          !textEquals(requestAction, "action_kind", kind) ||
          integer(requestAction, "action_position") != Some(BigInt(position)) ||
          requestAction.get("candidate_id") != registryAction.get("candidate_id") ||
          requestAction.get("candidate_rank") != registryAction.get("candidate_rank") ||
          requestAction.get("rendered_sql") != registryAction.get("rendered_sql")
        ) return Left(RequestBindingMismatch)
        val candidateRank = optionInteger(registryAction.get("candidate_rank"))
        val candidateId = optionText(registryAction.get("candidate_id"))
        if (
          (position == 0 && (candidateRank.nonEmpty || candidateId.nonEmpty)) ||
          (position > 0 &&
            (candidateRank != Some(BigInt(position)) ||
              !candidateId.exists(_.startsWith("sha256:"))))
        ) return Left(RequestContractMismatch)
        val rendered = objectField(registryAction, "rendered_sql")
          .getOrElse(return Left(RequestContractMismatch))
        if (position == 0 && !sameEncodedSql(rendered, sourceNode))
          return Left(RequestBindingMismatch)
        val sql = validateActionSql(rendered)
          .getOrElse(return Left(RequestContractMismatch))
        val action = PostgreSqlEnumerationCampaignV2PlanAction
          .create(actionId, kind, sql)
          .fold(_ => return Left(RequestContractMismatch), identity)
        typedActions += action
    }
    val wholePlanRequest = PostgreSqlEnumerationCampaignV2WholePlanRequest
      .create(typedActions.result())
      .fold(_ => return Left(RequestContractMismatch), identity)

    if (root.get("source_sql") != sourceNode || root.get("data_contract") != dataNode)
      return Left(RequestBindingMismatch)
    if (!validateLimits(root.get("limits"))) return Left(RequestContractMismatch)
    if (!validateSession(root.get("session_contract"), settingsNode))
      return Left(RequestBindingMismatch)

    val bindings = objectField(root, "source_package_bindings")
      .filter(closed(_, BindingKeys))
      .getOrElse(return Left(RequestContractMismatch))
    val expectedBindings = Map(
      "acquisition_package_schema" -> V2PackageSchema,
      "acquisition_package_seal_sha256" -> packageSeal,
      "acquisition_package_sha256" -> packageHash,
      "action_registry_schema" -> V2RegistrySchema,
      "action_registry_seal_sha256" -> registrySeal,
      "action_registry_sha256" -> registryHash,
      "authoring_request_schema" -> V2AuthoringSchema,
      "authoring_request_sha256" -> authoringHash,
      "data_contract_sha256" -> dataHash,
      "settings_bundle_sha256" -> settingsHash,
      "source_sql_sha256" -> sourceHash
    ) ++ ValidatorHashes
    if (!expectedBindings.forall { case (name, value) => textEquals(bindings, name, value) })
      return Left(RequestBindingMismatch)

    val requestSeal = text(root, "capture_request_seal_sha256")
      .filter(isSha256)
      .getOrElse(return Left(RequestContractMismatch))
    if (requestSeal != seal(root, "capture_request_seal_sha256", RequestSealDomain))
      return Left(RequestBindingMismatch)

    Right(
      new PostgreSqlEnumerationPlanCaptureV1Request(
        exactDocument,
        exactDocument.sha256,
        wholePlanRequest,
        packageHash,
        registryHash,
        authoringHash,
        dataHash,
        canonicalDocument(root.get("limits")).sha256,
        canonicalDocument(root.get("session_contract")).sha256,
        sourceHash
      )
    )
  }

  private def validateInternalBindings(
      sourcePackage: JsonNode,
      registry: JsonNode,
      authoring: JsonNode,
      authoringHash: String,
      registryHash: String,
      dataHash: String,
      settingsHash: String,
      sourceHash: String
  ): Boolean = {
    val registryBindings = objectField(registry, "bindings").getOrElse(return false)
    val packageBindings = objectField(sourcePackage, "bindings").getOrElse(return false)
    val workload = text(authoring, "workload_id").getOrElse(return false)
    val lineage = text(authoring, "lineage_id").getOrElse(return false)
    val registryExpected = Map(
      "authoring_request_sha256" -> authoringHash,
      "data_contract_sha256" -> dataHash,
      "settings_bundle_sha256" -> settingsHash,
      "source_sql_sha256" -> sourceHash,
      "workload_id" -> workload,
      "lineage_id" -> lineage
    )
    val packageExpected = Map(
      "authoring_request_sha256" -> authoringHash,
      "action_registry_sha256" -> registryHash,
      "source_sql_sha256" -> sourceHash,
      "workload_id" -> workload,
      "lineage_id" -> lineage
    )
    registryExpected.forall { case (name, value) => textEquals(registryBindings, name, value) } &&
    packageExpected.forall { case (name, value) => textEquals(packageBindings, name, value) }
  }

  private def validateSourceSql(node: JsonNode): Boolean =
    closed(node, SourceSqlKeys) &&
      textEquals(node, "schema_version", "yrw-enumeration-source-sql/v2") &&
      textEquals(node, "encoding", "utf-8") &&
      validateEncodedSql(node).nonEmpty

  private def validateActionSql(node: JsonNode): Option[String] =
    if (
      closed(node, ActionSqlKeys) &&
      textEquals(node, "schema_version", "yrw-enumeration-action-sql/v2") &&
      textEquals(node, "encoding", "utf-8") &&
      textEquals(node, "hash_scheme", "sha256_utf8_v1")
    ) validateEncodedSql(node)
    else None

  private def validateEncodedSql(node: JsonNode): Option[String] = {
    val hex = text(node, "utf8_hex").getOrElse(return None)
    val raw = decodeLowerHex(hex, 16 * 1024 * 1024).getOrElse(return None)
    val sql = decodeStrict(raw).getOrElse(return None)
    if (sql.isEmpty || sql.indexOf('\u0000') >= 0) return None
    if (
      integer(node, "byte_length") != Some(BigInt(raw.length)) ||
      !textEquals(node, "sha256", sha256(raw))
    ) None
    else Some(sql)
  }

  private def sameEncodedSql(action: JsonNode, source: JsonNode): Boolean =
    text(action, "encoding") == text(source, "encoding") &&
      integer(action, "byte_length") == integer(source, "byte_length") &&
      text(action, "sha256") == text(source, "sha256") &&
      text(action, "utf8_hex") == text(source, "utf8_hex")

  private def validateData(node: JsonNode): Boolean =
    closed(node, DataKeys) &&
      textEquals(node, "schema_version", "yrw-enumeration-data-contract/v2") &&
      text(node, "dataset_id").exists(_.matches("[a-z][a-z0-9._:-]{0,191}")) &&
      Vector(
        "data_semantics_sha256",
        "dataset_manifest_sha256",
        "load_receipt_sha256"
      ).forall(name => text(node, name).exists(isSha256))

  private def validateLimits(node: JsonNode): Boolean = {
    val expected = Map(
      "max_action_count" -> BigInt(11),
      "max_canonical_decimal_chars" -> BigInt(1024),
      "max_canonical_document_bytes" -> BigInt(64 * 1024 * 1024),
      "max_capture_request_bytes" -> BigInt(16 * 1024 * 1024),
      "max_cumulative_raw_plan_bytes" -> BigInt(16 * 1024 * 1024),
      "max_derived_decimal_chars" -> BigInt(4096),
      "max_json_depth" -> BigInt(128),
      "max_json_values" -> BigInt(200000),
      "max_node_type_chars" -> BigInt(128),
      "max_numeric_token_chars" -> BigInt(256),
      "max_plan_depth" -> BigInt(128),
      "max_plan_nodes" -> BigInt(10000),
      "max_raw_plan_bytes" -> BigInt(4 * 1024 * 1024),
      "postgresql_server_major" -> BigInt(18),
      "statement_timeout_seconds" -> BigInt(10)
    )
    node != null && node.isObject &&
      closed(node, expected.keySet + "schema_version") &&
      textEquals(node, "schema_version", "yrw-enumeration-plan-capture-limits/v1") &&
      expected.forall { case (name, value) => integer(node, name).contains(value) }
  }

  private def validateSession(node: JsonNode, settings: JsonNode): Boolean = {
    val keys = Set(
      "action_order",
      "explain",
      "postgres_session",
      "postgres_session_fingerprint_scheme",
      "postgres_session_fingerprint_sha256",
      "plan_grammar",
      "same_session",
      "schema_version",
      "statement_timeout",
      "transaction"
    )
    if (node == null || !closed(node, keys)) return false
    val committed = settings.get("postgres_session")
    if (committed == null || node.get("postgres_session") != committed) return false
    val settingValues = arrayField(node, "postgres_session").getOrElse(return false)
    if (settingValues.isEmpty || settingValues.size > 256) return false
    val settingNames = settingValues.map(value => text(value, "name").getOrElse(return false))
    if (
      settingNames != settingNames.sorted.distinct ||
      !settingNames.forall(_.matches("[a-z][a-z0-9._:-]{0,191}"))
    ) return false
    if (!settingValues.forall(value =>
          closed(value, Set("name", "value")) &&
            text(value, "value").exists(text =>
              text.nonEmpty && text.indexOf('\u0000') < 0 &&
                text.getBytes(StandardCharsets.UTF_8).length <= 1024
            )
        ))
      return false
    val expectedCampaign = Map(
      "datestyle" -> "ISO, MDY",
      "extra_float_digits" -> "3",
      "standard_conforming_strings" -> "on",
      "timezone" -> "UTC"
    )
    val exactCampaign = settingValues.map(value =>
      text(value, "name").get -> text(value, "value").get
    )
    if (exactCampaign != expectedCampaign.toVector.sortBy(_._1)) return false
    val explain = objectField(node, "explain").getOrElse(return false)
    val grammar = objectField(node, "plan_grammar").getOrElse(return false)
    val timeout = objectField(node, "statement_timeout").getOrElse(return false)
    val transaction = objectField(node, "transaction").getOrElse(return false)
    textEquals(node, "schema_version", "yrw-enumeration-plan-capture-session-contract/v1") &&
    textEquals(node, "action_order", "native-first-exact-v2-registry-order-v1") &&
    booleanEquals(node, "same_session", true) &&
    closed(explain, Set("analyze", "costs", "format", "settings", "summary", "verbose")) &&
    booleanEquals(explain, "analyze", false) && booleanEquals(explain, "costs", true) &&
    textEquals(explain, "format", "json") && booleanEquals(explain, "settings", true) &&
    booleanEquals(explain, "summary", false) && booleanEquals(explain, "verbose", false) &&
    closed(grammar, Set("grammar_id", "postgresql_server_major", "source_commit", "source_path", "source_sha256")) &&
    textEquals(grammar, "grammar_id", PostgreSqlEnumerationCampaignV2WholePlanCapture.PlanGrammarId) &&
    integer(grammar, "postgresql_server_major").contains(BigInt(18)) &&
    textEquals(grammar, "source_commit", PostgreSqlEnumerationCampaignV2WholePlanCapture.PlanGrammarSourceCommit) &&
    textEquals(grammar, "source_path", PostgreSqlEnumerationCampaignV2WholePlanCapture.PlanGrammarSourcePath) &&
    textEquals(grammar, "source_sha256", PostgreSqlEnumerationCampaignV2WholePlanCapture.PlanGrammarSourceSha256) &&
    closed(timeout, Set("scope", "value")) && textEquals(timeout, "scope", "transaction-local") &&
    textEquals(timeout, "value", "10s") &&
    closed(transaction, Set("access_mode", "isolation_level")) &&
    textEquals(transaction, "access_mode", "read-only") &&
    textEquals(transaction, "isolation_level", "repeatable-read") &&
    textEquals(node, "postgres_session_fingerprint_scheme", "length-prefixed-utf8-sorted-name-value-v1") &&
    textEquals(node, "postgres_session_fingerprint_sha256", settingsFingerprint(settingValues))
  }

  private def settingsFingerprint(settings: Vector[JsonNode]): String = {
    val output = new java.io.ByteArrayOutputStream()
    output.write("yrw-enumeration-plan-capture-session-settings/v1\u0000".getBytes(StandardCharsets.UTF_8))
    settings.foreach { setting =>
      val name = text(setting, "name").get.getBytes(StandardCharsets.UTF_8)
      val value = text(setting, "value").get.getBytes(StandardCharsets.UTF_8)
      output.write(name.length.toString.getBytes(StandardCharsets.US_ASCII))
      output.write(':'); output.write(name); output.write('=')
      output.write(value.length.toString.getBytes(StandardCharsets.US_ASCII))
      output.write(':'); output.write(value); output.write('\n')
    }
    sha256(output.toByteArray)
  }

  private def seal(node: JsonNode, sealField: String, domain: Array[Byte]): String = {
    val unsigned = node.deepCopy[ObjectNode]()
    unsigned.remove(sealField)
    val document = canonicalDocument(unsigned)
    sha256(domain ++ document.bytes)
  }

  private[cli] def canonicalDocument(
      node: JsonNode
  ): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(toCanonical(node))

  private def toCanonical(node: JsonNode): Value = {
    if (node == null || node.isNull) NullValue
    else if (node.isTextual) string(node.textValue())
    else if (node.isBoolean) bool(node.booleanValue())
    else if (node.isIntegralNumber) number(BigInt(node.bigIntegerValue()))
    else if (node.isArray) array(node.elements().asScala.toVector.map(toCanonical))
    else if (node.isObject)
      obj(node.fields().asScala.toVector.map(field => field.getKey -> toCanonical(field.getValue)))
    else throw new IllegalArgumentException("non-integral JSON value")
  }

  private def withinStructuralBounds(root: JsonNode): Boolean = {
    var count = 0
    val stack = scala.collection.mutable.ArrayDeque[(JsonNode, Int)]((root, 1))
    while (stack.nonEmpty) {
      val (node, depth) = stack.removeLast()
      count += 1
      if (count > MaximumJsonValues || depth > MaximumJsonDepth) return false
      if (node.isArray)
        node.elements().asScala.foreach(child =>
          stack.append((child, if (child.isContainerNode) depth + 1 else depth))
        )
      else if (node.isObject)
        node.elements().asScala.foreach(child =>
          stack.append((child, if (child.isContainerNode) depth + 1 else depth))
        )
    }
    true
  }

  private def strictUtf8(bytes: Array[Byte]): Boolean = decodeStrict(bytes).nonEmpty
  private def decodeStrict(bytes: Array[Byte]): Option[String] =
    try {
      Some(
        StandardCharsets.UTF_8
          .newDecoder()
          .onMalformedInput(CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)
          .decode(ByteBuffer.wrap(bytes))
          .toString
      )
    } catch { case NonFatal(_) => None }

  private[cli] def decodeLowerHex(
      value: String,
      maximumBytes: Int
  ): Option[Array[Byte]] = {
    if (value == null || value.isEmpty || value.length % 2 != 0 || value.length > maximumBytes * 2)
      return None
    val output = new Array[Byte](value.length / 2)
    var index = 0
    while (index < output.length) {
      val high = asciiHex(value.charAt(index * 2))
      val low = asciiHex(value.charAt(index * 2 + 1))
      if (high < 0 || low < 0) return None
      output(index) = ((high << 4) | low).toByte
      index += 1
    }
    Some(output)
  }

  private def asciiHex(value: Char): Int =
    if (value >= '0' && value <= '9') value - '0'
    else if (value >= 'a' && value <= 'f') value - 'a' + 10
    else -1

  private def closed(node: JsonNode, keys: Set[String]): Boolean =
    node != null && node.isObject && node.fieldNames().asScala.toSet == keys
  private def objectField(node: JsonNode, name: String): Option[JsonNode] =
    Option(node).flatMap(value => Option(value.get(name))).filter(_.isObject)
  private def arrayField(node: JsonNode, name: String): Option[Vector[JsonNode]] =
    Option(node).flatMap(value => Option(value.get(name))).filter(_.isArray)
      .map(_.elements().asScala.toVector)
  private def text(node: JsonNode, name: String): Option[String] =
    Option(node).flatMap(value => Option(value.get(name))).filter(_.isTextual).map(_.textValue())
  private def textEquals(node: JsonNode, name: String, expected: String): Boolean =
    text(node, name).contains(expected)
  private def booleanEquals(node: JsonNode, name: String, expected: Boolean): Boolean =
    Option(node).flatMap(value => Option(value.get(name))).exists(value => value.isBoolean && value.booleanValue() == expected)
  private def isBoolean(node: JsonNode, name: String): Boolean =
    Option(node).flatMap(value => Option(value.get(name))).exists(_.isBoolean)
  private def integer(node: JsonNode, name: String): Option[BigInt] =
    Option(node).flatMap(value => Option(value.get(name))).filter(_.isIntegralNumber)
      .map(value => BigInt(value.bigIntegerValue()))
  private def optionInteger(node: JsonNode): Option[BigInt] =
    Option(node).filterNot(_.isNull).filter(_.isIntegralNumber).map(value => BigInt(value.bigIntegerValue()))
  private def optionText(node: JsonNode): Option[String] =
    Option(node).filterNot(_.isNull).filter(_.isTextual).map(_.textValue())
  private def isSha256(value: String): Boolean = value != null && value.matches("[0-9a-f]{64}")
  private[cli] def sha256(bytes: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
    digest.map(byte => f"${byte & 0xff}%02x").mkString
  }
}

/** Exact Python RAW_ASSEMBLY_SCHEMA serializer; walker receipts stay Scala-local. */
private[yannakakis] object PostgreSqlEnumerationPlanCaptureV1Assembly {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._
  import PostgreSqlEnumerationPlanCaptureV1ProtocolFailure._

  private val RawAssemblySchema =
    "yrw-enumeration-plan-raw-capture-assembly/v1"
  private val RawPlanSchema =
    "yrw-enumeration-raw-postgresql-plan-bytes/v1"
  private val ProvenanceSchema =
    "yrw-enumeration-plan-capture-provenance/v1"
  private val ProtocolVersion =
    "yrw-enumeration-plan-capture-protocol/v1"

  def render(
      request: PostgreSqlEnumerationPlanCaptureV1Request,
      batch: PostgreSqlEnumerationCampaignV2WholePlanBatch
  ): Either[
    PostgreSqlEnumerationPlanCaptureV1ProtocolFailure,
    PostgreSqlEnumerationCampaignV2JsonDocument
  ] = {
    if (request == null || batch == null) return Left(CaptureMismatch)
    val expected = request.wholePlanRequest.actions
    if (
      batch.limits != PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.default ||
      batch.observations.size != expected.size ||
      batch.totalPlanUtf8Bytes > 16L * 1024L * 1024L
    ) return Left(CaptureMismatch)
    val capturedSettings = batch.provenance.settings
    if (capturedSettings.exists(_.value.isEmpty)) return Left(CaptureMismatch)
    val capturedByName = capturedSettings.map(setting => setting.name -> setting.value.get).toMap

    val captures = Vector.newBuilder[Value]
    var total = BigInt(0)
    batch.observations.zip(expected).zipWithIndex.foreach {
      case ((observation, action), position) =>
        if (
          observation.actionId != action.actionId ||
          observation.actionKind != action.actionKind ||
          observation.renderedStatementSha256 != action.renderedStatementSha256
        ) return Left(CaptureMismatch)
        val raw = observation.document.rawUtf8Bytes
        val digest = PostgreSqlEnumerationPlanCaptureV1RequestParser.sha256(raw)
        if (
          raw.isEmpty || raw.length > 4 * 1024 * 1024 ||
          observation.document.utf8ByteLength != raw.length.toLong ||
          observation.document.rawUtf8Sha256 != digest ||
          !Arrays.equals(raw, observation.document.rawUtf8Bytes) ||
          !observation.document.settings.forall(setting =>
            capturedByName.get(setting.name).contains(setting.value)
          ) ||
          !observation.document.settings.exists(_.name == "search_path")
        ) return Left(CaptureMismatch)
        total += raw.length
        captures += obj(
          "action_id" -> string(action.actionId),
          "action_kind" -> string(action.actionKind.id),
          "action_position" -> number(BigInt(position)),
          "raw_plan" -> obj(
            "byte_length" -> number(BigInt(raw.length)),
            "encoding" -> string("utf-8"),
            "media_type" -> string("application/vnd.postgresql.explain+json"),
            "schema_version" -> string(RawPlanSchema),
            "sha256" -> string(digest),
            "utf8_hex" -> utf8HexString(observation.document.rawUtf8, raw.length.toLong)
          ),
          "rendered_statement_sha256" -> string(action.renderedStatementSha256)
        )
    }
    if (total != batch.totalPlanUtf8Bytes || total > 16 * 1024 * 1024)
      return Left(CaptureMismatch)

    val settings = capturedSettings.map { setting =>
      obj(
        "name" -> string(setting.name),
        "value" -> string(setting.value.get)
      )
    }
    val bindings = obj(
      "acquisition_package_sha256" -> string(request.acquisitionPackageSha256),
      "action_registry_sha256" -> string(request.actionRegistrySha256),
      "authoring_request_sha256" -> string(request.authoringRequestSha256),
      "capture_request_sha256" -> string(request.captureRequestSha256),
      "data_contract_sha256" -> string(request.dataContractSha256),
      "limits_sha256" -> string(request.limitsSha256),
      "session_contract_sha256" -> string(request.sessionContractSha256),
      "source_sql_sha256" -> string(request.sourceSqlSha256)
    )
    val explain = obj(
      "analyze" -> bool(false),
      "costs" -> bool(true),
      "format" -> string("json"),
      "settings" -> bool(true),
      "summary" -> bool(false),
      "verbose" -> bool(false)
    )
    val transaction = obj(
      "access_mode" -> string("read-only"),
      "isolation_level" -> string("repeatable-read")
    )
    val provenance = obj(
      "action_count" -> number(BigInt(expected.size)),
      "bindings" -> bindings,
      "database_encoding" -> string(batch.provenance.databaseEncoding),
      "explain" -> explain,
      "postgresql_server_major" -> number(BigInt(batch.postgresqlServerMajor)),
      "same_session" -> bool(true),
      "schema_version" -> string(ProvenanceSchema),
      "server_version_num" -> number(BigInt(batch.provenance.serverVersionNum)),
      "setting_count" -> number(BigInt(batch.provenance.settingCount)),
      "settings" -> array(settings),
      "settings_fingerprint_scheme" -> string(batch.provenance.settingsFingerprintScheme),
      "settings_fingerprint_sha256" -> string(batch.provenance.settingsFingerprintSha256),
      "statement_timeout_seconds" -> number(BigInt(10)),
      "transaction" -> transaction
    )
    try {
      Right(
        document(
          obj(
            "capture_request" -> embeddedDocument(request.document),
            "capture_request_sha256" -> string(request.captureRequestSha256),
            "captures" -> array(captures.result()),
            "protocol_version" -> string(ProtocolVersion),
            "provenance" -> provenance,
            "schema_version" -> string(RawAssemblySchema)
          )
        )
      )
    } catch {
      case problem: IllegalArgumentException
          if Option(problem.getMessage).exists(_.contains("exceeds")) =>
        Left(AssemblyTooLarge)
      case NonFatal(_) => Left(AssemblyFailed)
    }
  }
}
