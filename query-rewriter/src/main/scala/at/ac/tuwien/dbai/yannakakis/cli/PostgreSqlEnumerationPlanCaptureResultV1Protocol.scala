package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Arrays

import scala.util.control.NonFatal

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationCampaignV2WholePlanFailure,
  PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits,
  PostgreSqlEnumerationCampaignV2WholePlanStage
}

private[cli] sealed trait PostgreSqlEnumerationPlanCaptureResultV1Failure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlEnumerationPlanCaptureResultV1Failure {
  case object CaptureMismatch
      extends PostgreSqlEnumerationPlanCaptureResultV1Failure {
    override val code: String = "plan-capture-result-v1.capture-mismatch"
  }
  case object ReceiptFailed
      extends PostgreSqlEnumerationPlanCaptureResultV1Failure {
    override val code: String = "plan-capture-result-v1.receipt-failed"
  }
}

/**
  * Canonical typed terminal record for a backend WholePlanFailure.
  *
  * This is deliberately additive to plan-capture v1. Successful captures still
  * use the byte-identical v1 raw assembly serializer, while this serializer
  * records only the bounded typed failure and the already completed exact plan
  * prefix's action/length/hash commitments. It is not an independent
  * classifier and contains no completed raw plans, diagnostic exception text,
  * runtime measurement, or outcome.
  */
private[cli] object PostgreSqlEnumerationPlanCaptureFailureReceiptV1 {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._
  import PostgreSqlEnumerationPlanCaptureResultV1Failure._

  val SchemaVersion: String =
    "yrw-enumeration-plan-capture-failure-receipt/v1"
  val ProtocolVersion: String =
    "yrw-enumeration-plan-capture-result-protocol/v1"
  val Status: String = "capture-failed"
  val Classification: String =
    "inactive-preexecution-plan-capture-failure"
  val SealDomainUtf8: String =
    "yrw-enumeration-plan-capture-failure-receipt-seal/v1\u0000"

  // Exact backend vocabulary. EvaluatorContractViolation is intentionally not
  // receipt-admissible below: with a parsed request and the fixed live limits,
  // it denotes a producer contract fault rather than an acquisition result.
  val AllowedFailureCodes: Vector[String] = Vector(
    "plan-capture.evaluator-contract-violation",
    "plan-capture.connection-unavailable",
    "plan-capture.session-configuration-failed",
    "plan-capture.provenance-unavailable",
    "plan-capture.unsafe-statement",
    "plan-capture.statement-timeout",
    "plan-capture.statement-failed",
    "plan-capture.plan-document-oversized",
    "plan-capture.plan-utf8-invalid",
    "plan-capture.plan-json-malformed",
    "plan-capture.plan-depth-exceeded",
    "plan-capture.plan-node-limit-exceeded",
    "plan-capture.json-value-limit-exceeded",
    "plan-capture.execution-evidence-present",
    "plan-capture.numeric-evidence-unsafe",
    "plan-capture.provenance-changed",
    "plan-capture.cleanup-failed"
  )
  val AllowedStages: Vector[String] = Vector(
    "session",
    "initial-provenance",
    "action-explain",
    "final-provenance",
    "cleanup"
  )
  private val ReceiptStagesByFailureCode: Map[String, Set[String]] = Map(
    "plan-capture.connection-unavailable" -> Set("session"),
    "plan-capture.session-configuration-failed" -> Set("session"),
    "plan-capture.provenance-unavailable" ->
      Set("initial-provenance", "final-provenance"),
    "plan-capture.unsafe-statement" -> Set("action-explain"),
    "plan-capture.statement-timeout" ->
      Set("session", "initial-provenance", "action-explain", "final-provenance"),
    "plan-capture.statement-failed" -> Set("action-explain"),
    "plan-capture.plan-document-oversized" -> Set("action-explain"),
    "plan-capture.plan-utf8-invalid" -> Set("action-explain"),
    "plan-capture.plan-json-malformed" -> Set("action-explain"),
    "plan-capture.plan-depth-exceeded" -> Set("action-explain"),
    "plan-capture.plan-node-limit-exceeded" -> Set("action-explain"),
    "plan-capture.json-value-limit-exceeded" -> Set("action-explain"),
    "plan-capture.execution-evidence-present" -> Set("action-explain"),
    "plan-capture.numeric-evidence-unsafe" -> Set("action-explain"),
    "plan-capture.provenance-changed" ->
      Set("action-explain", "final-provenance"),
    "plan-capture.cleanup-failed" -> AllowedStages.toSet
  )
  require(
    AllowedFailureCodes.distinct.size == AllowedFailureCodes.size &&
      AllowedStages.distinct.size == AllowedStages.size &&
      ReceiptStagesByFailureCode.keySet ==
        AllowedFailureCodes.toSet -
          "plan-capture.evaluator-contract-violation" &&
      ReceiptStagesByFailureCode.values.forall(_.subsetOf(AllowedStages.toSet)),
    "plan-capture failure vocabulary is not unique"
  )

  private val Mapper = new ObjectMapper()

  def render(
      request: PostgreSqlEnumerationPlanCaptureV1Request,
      failure: PostgreSqlEnumerationCampaignV2WholePlanFailure
  ): Either[
    PostgreSqlEnumerationPlanCaptureResultV1Failure,
    PostgreSqlEnumerationCampaignV2JsonDocument
  ] = {
    if (request == null || failure == null) return Left(CaptureMismatch)
    try {
      val actions = request.wholePlanRequest.actions
      if (
        !AllowedFailureCodes.contains(failure.code.id) ||
        !AllowedStages.contains(failure.stage.id) ||
        !ReceiptStagesByFailureCode
          .get(failure.code.id)
          .exists(_.contains(failure.stage.id)) ||
        failure.completed.size > actions.size ||
        failure.failedActionIndex.exists(index => index >= actions.size)
      ) return Left(CaptureMismatch)

      val prefixSize = failure.completed.size
      val stageHasValidPrefix = failure.stage match {
        case PostgreSqlEnumerationCampaignV2WholePlanStage.Session |
            PostgreSqlEnumerationCampaignV2WholePlanStage.InitialProvenance =>
          prefixSize == 0
        case PostgreSqlEnumerationCampaignV2WholePlanStage.ActionExplain =>
          failure.failedActionIndex.contains(prefixSize) &&
            prefixSize < actions.size
        case PostgreSqlEnumerationCampaignV2WholePlanStage.FinalProvenance =>
          prefixSize == actions.size
        case PostgreSqlEnumerationCampaignV2WholePlanStage.Cleanup =>
          prefixSize <= actions.size
      }
      if (!stageHasValidPrefix) return Left(CaptureMismatch)
      if (
        !withinCumulativePlanByteLimit(
          failure.completed.map(_.document.utf8ByteLength)
        )
      ) return Left(CaptureMismatch)

      val prefix = failure.completed.zipWithIndex.map {
        case (observation, index) =>
          val action = actions(index)
          val raw = observation.document.rawUtf8Bytes
          val rawSha256 = sha256(raw)
          if (
            observation.actionId != action.actionId ||
            observation.actionKind != action.actionKind ||
            observation.renderedStatementSha256 !=
              action.renderedStatementSha256 ||
            observation.document.utf8ByteLength != raw.length.toLong ||
            observation.document.rawUtf8Sha256 != rawSha256 ||
            raw.isEmpty ||
            !Arrays.equals(raw, observation.document.rawUtf8Bytes)
          ) return Left(CaptureMismatch)
          obj(
            "action_id" -> string(action.actionId),
            "action_index" -> number(BigInt(index)),
            "raw_plan_byte_length" -> number(BigInt(raw.length)),
            "raw_plan_sha256" -> string(rawSha256),
            "rendered_statement_sha256" ->
              string(action.renderedStatementSha256)
          )
      }

      val actionFailure =
        failure.stage == PostgreSqlEnumerationCampaignV2WholePlanStage.ActionExplain
      if (actionFailure != failure.failedActionIndex.nonEmpty)
        return Left(CaptureMismatch)
      val failedAction = failure.failedActionIndex.map(actions)
      if (
        failure.failedActionIndex.exists(_ != prefix.size) ||
        (!actionFailure && failedAction.nonEmpty)
      ) return Left(CaptureMismatch)

      val root = Mapper.readTree(request.document.bytes)
      val requestSeal = requiredHash(root, "capture_request_seal_sha256")
        .getOrElse(return Left(CaptureMismatch))
      val source = Option(root.get("source_package_bindings"))
        .filter(_.isObject)
        .getOrElse(return Left(CaptureMismatch))
      val acquisitionPackageSha256 =
        requiredHash(source, "acquisition_package_sha256")
          .getOrElse(return Left(CaptureMismatch))
      val actionRegistrySha256 = requiredHash(source, "action_registry_sha256")
        .getOrElse(return Left(CaptureMismatch))
      val authoringRequestSha256 = requiredHash(source, "authoring_request_sha256")
        .getOrElse(return Left(CaptureMismatch))
      val dataContractSha256 = requiredHash(source, "data_contract_sha256")
        .getOrElse(return Left(CaptureMismatch))
      val settingsBundleSha256 = requiredHash(source, "settings_bundle_sha256")
        .getOrElse(return Left(CaptureMismatch))
      val sourceSqlSha256 = requiredHash(source, "source_sql_sha256")
        .getOrElse(return Left(CaptureMismatch))
      val sourceBindings = obj(
        "acquisition_package_sha256" ->
          string(acquisitionPackageSha256),
        "action_registry_sha256" ->
          string(actionRegistrySha256),
        "authoring_request_sha256" ->
          string(authoringRequestSha256),
        "data_contract_sha256" ->
          string(dataContractSha256),
        "settings_bundle_sha256" ->
          string(settingsBundleSha256),
        "source_sql_sha256" ->
          string(sourceSqlSha256)
      )
      if (
        request.acquisitionPackageSha256 != acquisitionPackageSha256 ||
        request.actionRegistrySha256 != actionRegistrySha256 ||
        request.authoringRequestSha256 != authoringRequestSha256 ||
        request.dataContractSha256 != dataContractSha256 ||
        request.sourceSqlSha256 != sourceSqlSha256
      ) return Left(CaptureMismatch)

      val failedIndex = failure.failedActionIndex
        .map(value => number(BigInt(value)))
        .getOrElse(NullValue)
      val failedId = failedAction
        .map(value => string(value.actionId))
        .getOrElse(NullValue)
      val failureValue = obj(
        "code" -> string(failure.code.id),
        "completed_prefix" -> array(prefix),
        "failed_action_id" -> failedId,
        "failed_action_index" -> failedIndex,
        "stage" -> string(failure.stage.id)
      )
      val unsignedFields = Vector[(String, Value)](
        "action_count" -> number(BigInt(actions.size)),
        "capture_request" -> embeddedDocument(request.document),
        "capture_request_seal_sha256" -> string(requestSeal),
        "capture_request_sha256" -> string(request.captureRequestSha256),
        "claims_external_chronology" -> bool(false),
        "claims_independent_failure_classification" -> bool(false),
        "claims_outcomes" -> bool(false),
        "claims_reportable" -> bool(false),
        "claims_runtime_timing" -> bool(false),
        "classification" -> string(Classification),
        "failure" -> failureValue,
        "outcomes_in_scope" -> bool(false),
        "production_policy" -> bool(false),
        "protocol_version" -> string(ProtocolVersion),
        "reportable" -> bool(false),
        "schema_version" -> string(SchemaVersion),
        "source_bindings" -> sourceBindings,
        "status" -> string(Status)
      )
      val unsigned = document(obj(unsignedFields))
      val seal = sha256(
        SealDomainUtf8.getBytes(StandardCharsets.UTF_8) ++ unsigned.bytes
      )
      Right(
        document(
          obj(unsignedFields :+ ("failure_receipt_seal_sha256" -> string(seal)))
        )
      )
    } catch {
      case NonFatal(_) => Left(ReceiptFailed)
    }
  }

  private def requiredHash(node: JsonNode, field: String): Option[String] =
    Option(node)
      .flatMap(value => Option(value.get(field)))
      .filter(_.isTextual)
      .map(_.textValue())
      .filter(_.matches("[0-9a-f]{64}"))

  private[cli] def withinCumulativePlanByteLimit(
      lengths: Vector[Long]
  ): Boolean =
    lengths != null && lengths.forall(_ > 0L) &&
      lengths.foldLeft(BigInt(0))((total, length) => total + BigInt(length)) <=
        BigInt(
          PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
            .MaximumCumulativePlanUtf8Bytes
        )

  private[cli] def sha256(bytes: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
    digest.map(byte => f"${byte & 0xff}%02x").mkString
  }
}
