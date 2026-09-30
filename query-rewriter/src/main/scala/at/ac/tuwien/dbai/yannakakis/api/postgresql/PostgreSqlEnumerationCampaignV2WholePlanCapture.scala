package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.sql.{Connection, ResultSet, SQLException, SQLTimeoutException, Statement}
import javax.sql.DataSource

import scala.util.control.NonFatal

sealed trait PostgreSqlEnumerationCampaignV2PlanActionKind
    extends Product
    with Serializable {
  def id: String
}

object PostgreSqlEnumerationCampaignV2PlanActionKind {
  case object Native extends PostgreSqlEnumerationCampaignV2PlanActionKind {
    override val id: String = "native"
  }
  case object RetainedCandidate
      extends PostgreSqlEnumerationCampaignV2PlanActionKind {
    override val id: String = "retained_candidate"
  }

  val values: Vector[PostgreSqlEnumerationCampaignV2PlanActionKind] =
    Vector(Native, RetainedCandidate)

  def parse(
      value: String
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanRequestError,
    PostgreSqlEnumerationCampaignV2PlanActionKind
  ] =
    values
      .find(_.id == value)
      .toRight(
        PostgreSqlEnumerationCampaignV2WholePlanRequestError.InvalidActionKind
      )
}

sealed trait PostgreSqlEnumerationCampaignV2WholePlanRequestError
    extends Product
    with Serializable {
  def code: String
}

object PostgreSqlEnumerationCampaignV2WholePlanRequestError {
  case object InvalidActionId
      extends PostgreSqlEnumerationCampaignV2WholePlanRequestError {
    override val code: String = "plan-capture-request.action-id"
  }
  case object InvalidActionKind
      extends PostgreSqlEnumerationCampaignV2WholePlanRequestError {
    override val code: String = "plan-capture-request.action-kind"
  }
  case object InvalidSql
      extends PostgreSqlEnumerationCampaignV2WholePlanRequestError {
    override val code: String = "plan-capture-request.sql"
  }
  case object EmptyActions
      extends PostgreSqlEnumerationCampaignV2WholePlanRequestError {
    override val code: String = "plan-capture-request.actions-empty"
  }
  case object NativeNotFirst
      extends PostgreSqlEnumerationCampaignV2WholePlanRequestError {
    override val code: String = "plan-capture-request.native-not-first"
  }
  case object DuplicateActionId
      extends PostgreSqlEnumerationCampaignV2WholePlanRequestError {
    override val code: String = "plan-capture-request.action-id-duplicate"
  }
  case object TooManyActions
      extends PostgreSqlEnumerationCampaignV2WholePlanRequestError {
    override val code: String = "plan-capture-request.actions-oversized"
  }
}

/** One exact renderer-owned action. The digest is derived, never supplied. */
final case class PostgreSqlEnumerationCampaignV2PlanAction private[postgresql] (
    actionId: String,
    actionKind: PostgreSqlEnumerationCampaignV2PlanActionKind,
    sql: String,
    renderedStatementSha256: String
) extends Serializable {
  require(
    actionId != null &&
      PostgreSqlEnumerationCampaignV2PlanAction.ActionIdPattern.pattern
        .matcher(actionId)
        .matches() &&
      PostgreSqlEnumerationCampaignV2PlanAction.hasValidUnicode(actionId),
    "plan action ID is invalid"
  )
  require(actionKind != null, "plan action kind is null")
  require(
    (actionKind == PostgreSqlEnumerationCampaignV2PlanActionKind.Native &&
      actionId == "native") ||
      (actionKind ==
        PostgreSqlEnumerationCampaignV2PlanActionKind.RetainedCandidate &&
        PostgreSqlEnumerationCampaignV2PlanAction.RetainedActionIdPattern.pattern
          .matcher(actionId)
          .matches()),
    "plan action identity differs"
  )
  require(
    sql != null && sql.nonEmpty &&
      sql.length.toLong <=
        PostgreSqlEnumerationCampaignV2PlanAction.MaximumSqlUtf8Bytes &&
      sql.indexOf('\u0000') < 0 &&
      PostgreSqlEnumerationCampaignV2PlanAction.hasValidUnicode(sql) &&
      PostgreSqlEnumerationCampaignV2PlanAction.utf8LengthAtMost(
        sql,
        PostgreSqlEnumerationCampaignV2PlanAction.MaximumSqlUtf8Bytes
      ),
    "plan action SQL is invalid"
  )
  val sqlUtf8ByteLength: Long = sql.getBytes(StandardCharsets.UTF_8).length.toLong
  require(
    sqlUtf8ByteLength <=
      PostgreSqlEnumerationCampaignV2PlanAction.MaximumSqlUtf8Bytes,
    "plan action SQL is oversized"
  )
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}"),
    "plan action SQL digest is invalid"
  )
  require(
    renderedStatementSha256 ==
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql),
    "plan action SQL digest differs"
  )
}

object PostgreSqlEnumerationCampaignV2PlanAction {
  private[postgresql] val ActionIdPattern = "[a-z][a-z0-9._:-]{0,191}".r
  private[postgresql] val RetainedActionIdPattern = "rewrite-[0-9]{4}".r
  val MaximumSqlUtf8Bytes: Long = 16L * 1024L * 1024L

  def create(
      actionId: String,
      actionKind: PostgreSqlEnumerationCampaignV2PlanActionKind,
      sql: String
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanRequestError,
    PostgreSqlEnumerationCampaignV2PlanAction
  ] = {
    if (
      actionId == null || !ActionIdPattern.pattern.matcher(actionId).matches() ||
      !hasValidUnicode(actionId)
    ) Left(PostgreSqlEnumerationCampaignV2WholePlanRequestError.InvalidActionId)
    else if (actionKind == null)
      Left(PostgreSqlEnumerationCampaignV2WholePlanRequestError.InvalidActionKind)
    else if (
      (actionKind == PostgreSqlEnumerationCampaignV2PlanActionKind.Native &&
        actionId != "native") ||
      (actionKind ==
        PostgreSqlEnumerationCampaignV2PlanActionKind.RetainedCandidate &&
        !RetainedActionIdPattern.pattern.matcher(actionId).matches())
    ) Left(PostgreSqlEnumerationCampaignV2WholePlanRequestError.InvalidActionId)
    else if (
      sql == null || sql.isEmpty || sql.length.toLong > MaximumSqlUtf8Bytes ||
      sql.indexOf('\u0000') >= 0 ||
      !hasValidUnicode(sql) || !utf8LengthAtMost(sql, MaximumSqlUtf8Bytes)
    )
      Left(PostgreSqlEnumerationCampaignV2WholePlanRequestError.InvalidSql)
    else
      Right(
        PostgreSqlEnumerationCampaignV2PlanAction(
          actionId,
          actionKind,
          sql,
          PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql)
        )
      )
  }

  def create(
      actionId: String,
      actionKind: String,
      sql: String
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanRequestError,
    PostgreSqlEnumerationCampaignV2PlanAction
  ] =
    PostgreSqlEnumerationCampaignV2PlanActionKind
      .parse(actionKind)
      .flatMap(create(actionId, _, sql))

  private[postgresql] def hasValidUnicode(value: String): Boolean = {
    var index = 0
    while (index < value.length) {
      val character = value.charAt(index)
      if (Character.isHighSurrogate(character)) {
        if (
          index + 1 >= value.length ||
          !Character.isLowSurrogate(value.charAt(index + 1))
        ) return false
        index += 2
      } else if (Character.isLowSurrogate(character)) return false
      else index += 1
    }
    true
  }

  private[postgresql] def utf8LengthAtMost(
      value: String,
      maximum: Long
  ): Boolean = {
    var length = 0L
    var index = 0
    while (index < value.length) {
      val character = value.charAt(index)
      val increment =
        if (character <= 0x7f) 1L
        else if (character <= 0x7ff) 2L
        else if (Character.isHighSurrogate(character)) {
          if (
            index + 1 >= value.length ||
            !Character.isLowSurrogate(value.charAt(index + 1))
          ) return false
          index += 1
          4L
        } else if (Character.isLowSurrogate(character)) return false
        else 3L
      length += increment
      if (length > maximum) return false
      index += 1
    }
    true
  }
}

final case class PostgreSqlEnumerationCampaignV2WholePlanRequest private[postgresql] (
    actions: Vector[PostgreSqlEnumerationCampaignV2PlanAction]
) extends Serializable {
  require(actions != null && actions.nonEmpty, "plan-capture actions are empty")
  require(
    actions.size <= PostgreSqlEnumerationCampaignV2WholePlanRequest.MaximumActions,
    "plan-capture action count is oversized"
  )
  require(actions.forall(_ != null), "plan-capture action is null")
  require(
    actions.head.actionKind == PostgreSqlEnumerationCampaignV2PlanActionKind.Native,
    "native action is not first"
  )
  require(
    actions.tail.forall(
      _.actionKind ==
        PostgreSqlEnumerationCampaignV2PlanActionKind.RetainedCandidate
    ),
    "non-head native action is present"
  )
  require(
    actions.map(_.actionId).distinct.size == actions.size,
    "plan-capture action IDs are not unique"
  )
  require(
    actions.head.actionId == "native" && actions.tail.zipWithIndex.forall {
      case (action, index) => action.actionId == f"rewrite-${index + 1}%04d"
    },
    "plan-capture action IDs are not in exact registry order"
  )
}

object PostgreSqlEnumerationCampaignV2WholePlanRequest {
  val MaximumActions: Int = 11

  def create(
      actions: Vector[PostgreSqlEnumerationCampaignV2PlanAction]
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanRequestError,
    PostgreSqlEnumerationCampaignV2WholePlanRequest
  ] = {
    if (actions == null || actions.isEmpty || actions.exists(_ == null))
      Left(PostgreSqlEnumerationCampaignV2WholePlanRequestError.EmptyActions)
    else if (actions.size > MaximumActions)
      Left(PostgreSqlEnumerationCampaignV2WholePlanRequestError.TooManyActions)
    else if (
      actions.head.actionKind !=
        PostgreSqlEnumerationCampaignV2PlanActionKind.Native ||
      actions.tail.exists(
        _.actionKind !=
          PostgreSqlEnumerationCampaignV2PlanActionKind.RetainedCandidate
      )
    ) Left(PostgreSqlEnumerationCampaignV2WholePlanRequestError.NativeNotFirst)
    else if (actions.map(_.actionId).distinct.size != actions.size)
      Left(PostgreSqlEnumerationCampaignV2WholePlanRequestError.DuplicateActionId)
    else if (
      actions.head.actionId != "native" || actions.tail.zipWithIndex.exists {
        case (action, index) => action.actionId != f"rewrite-${index + 1}%04d"
      }
    ) Left(PostgreSqlEnumerationCampaignV2WholePlanRequestError.InvalidActionId)
    else Right(PostgreSqlEnumerationCampaignV2WholePlanRequest(actions))
  }
}

sealed trait PostgreSqlEnumerationCampaignV2WholePlanLimitError
    extends Product
    with Serializable {
  def code: String
}

object PostgreSqlEnumerationCampaignV2WholePlanLimitError {
  case object StatementTimeout
      extends PostgreSqlEnumerationCampaignV2WholePlanLimitError {
    override val code: String = "plan-capture-limit.statement-timeout"
  }
  case object PerPlanBytes
      extends PostgreSqlEnumerationCampaignV2WholePlanLimitError {
    override val code: String = "plan-capture-limit.per-plan-bytes"
  }
  case object CumulativeBytes
      extends PostgreSqlEnumerationCampaignV2WholePlanLimitError {
    override val code: String = "plan-capture-limit.cumulative-bytes"
  }
  case object JsonDepth
      extends PostgreSqlEnumerationCampaignV2WholePlanLimitError {
    override val code: String = "plan-capture-limit.json-depth"
  }
  case object JsonValues
      extends PostgreSqlEnumerationCampaignV2WholePlanLimitError {
    override val code: String = "plan-capture-limit.json-values"
  }
  case object PlanDepth
      extends PostgreSqlEnumerationCampaignV2WholePlanLimitError {
    override val code: String = "plan-capture-limit.plan-depth"
  }
  case object PlanNodes
      extends PostgreSqlEnumerationCampaignV2WholePlanLimitError {
    override val code: String = "plan-capture-limit.plan-nodes"
  }
  case object NumericToken
      extends PostgreSqlEnumerationCampaignV2WholePlanLimitError {
    override val code: String = "plan-capture-limit.numeric-token"
  }
}

final case class PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits private (
    statementTimeoutSeconds: Int,
    maxPlanUtf8Bytes: Long,
    maxCumulativePlanUtf8Bytes: Long,
    maxJsonNestingDepth: Int,
    maxJsonValues: Int,
    maxPlanNodeDepth: Int,
    maxPlanNodes: Int,
    maxNumericTokenCharacters: Int
) extends Serializable {
  require(
    statementTimeoutSeconds ==
      PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.StatementTimeoutSeconds,
    "plan-capture statement timeout differs"
  )
  require(
    maxPlanUtf8Bytes > 0L &&
      maxPlanUtf8Bytes <=
        PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.MaximumPlanUtf8Bytes,
    "plan-capture per-plan byte limit is invalid"
  )
  require(
    maxCumulativePlanUtf8Bytes >= maxPlanUtf8Bytes &&
      maxCumulativePlanUtf8Bytes <=
        PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
          .MaximumCumulativePlanUtf8Bytes,
    "plan-capture cumulative byte limit is invalid"
  )
  require(
    maxJsonNestingDepth >= 4 &&
      maxJsonNestingDepth <=
        PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
          .MaximumJsonNestingDepth,
    "plan-capture JSON depth limit is invalid"
  )
  require(
    maxJsonValues > 0 &&
      maxJsonValues <=
        PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.MaximumJsonValues,
    "plan-capture JSON value limit is invalid"
  )
  require(
    maxPlanNodeDepth >= 0 &&
      maxPlanNodeDepth <=
        PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
          .MaximumPlanNodeDepth,
    "plan-capture node depth limit is invalid"
  )
  require(
    maxPlanNodes > 0 &&
      maxPlanNodes <=
        PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.MaximumPlanNodes,
    "plan-capture node count limit is invalid"
  )
  require(
    maxNumericTokenCharacters > 0 &&
      maxNumericTokenCharacters <=
        PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
          .MaximumNumericTokenCharacters,
    "plan-capture numeric-token limit is invalid"
  )
  val maxCanonicalDecimalCharacters: Int =
    PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
      .MaximumCanonicalDecimalCharacters
  val maxNodeTypeCharacters: Int =
    PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
      .MaximumNodeTypeCharacters
}

object PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits {
  private val MiB = 1024L * 1024L

  val StatementTimeoutSeconds: Int = 10
  val MaximumPlanUtf8Bytes: Long = 4L * MiB
  val MaximumCumulativePlanUtf8Bytes: Long = 16L * MiB
  val MaximumJsonNestingDepth: Int = 128
  val MaximumJsonValues: Int = 200000
  val MaximumPlanNodeDepth: Int = 128
  val MaximumPlanNodes: Int = 10000
  val MaximumNumericTokenCharacters: Int = 256
  val MaximumCanonicalDecimalCharacters: Int = 1024
  val MaximumNodeTypeCharacters: Int = 128
  val MaximumPlanTextUtf8Bytes: Int = 64 * 1024
  val MaximumPlanStringListItems: Int = 10000
  val MaximumSettings: Int = 256
  val MaximumSettingValueUtf8Bytes: Int = 4096

  val default: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits =
    new PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits(
      statementTimeoutSeconds = StatementTimeoutSeconds,
      maxPlanUtf8Bytes = MaximumPlanUtf8Bytes,
      maxCumulativePlanUtf8Bytes = MaximumCumulativePlanUtf8Bytes,
      maxJsonNestingDepth = MaximumJsonNestingDepth,
      maxJsonValues = MaximumJsonValues,
      maxPlanNodeDepth = MaximumPlanNodeDepth,
      maxPlanNodes = MaximumPlanNodes,
      maxNumericTokenCharacters = MaximumNumericTokenCharacters
    )

  def create(
      statementTimeoutSeconds: Int = default.statementTimeoutSeconds,
      maxPlanUtf8Bytes: Long = default.maxPlanUtf8Bytes,
      maxCumulativePlanUtf8Bytes: Long = default.maxCumulativePlanUtf8Bytes,
      maxJsonNestingDepth: Int = default.maxJsonNestingDepth,
      maxJsonValues: Int = default.maxJsonValues,
      maxPlanNodeDepth: Int = default.maxPlanNodeDepth,
      maxPlanNodes: Int = default.maxPlanNodes,
      maxNumericTokenCharacters: Int = default.maxNumericTokenCharacters
  ): Either[
    Vector[PostgreSqlEnumerationCampaignV2WholePlanLimitError],
    PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ] = {
    val errors = Vector.newBuilder[PostgreSqlEnumerationCampaignV2WholePlanLimitError]
    if (statementTimeoutSeconds != StatementTimeoutSeconds)
      errors += PostgreSqlEnumerationCampaignV2WholePlanLimitError.StatementTimeout
    if (maxPlanUtf8Bytes <= 0L || maxPlanUtf8Bytes > MaximumPlanUtf8Bytes)
      errors += PostgreSqlEnumerationCampaignV2WholePlanLimitError.PerPlanBytes
    if (
      maxCumulativePlanUtf8Bytes <= 0L ||
      maxCumulativePlanUtf8Bytes < maxPlanUtf8Bytes ||
      maxCumulativePlanUtf8Bytes > MaximumCumulativePlanUtf8Bytes
    ) errors += PostgreSqlEnumerationCampaignV2WholePlanLimitError.CumulativeBytes
    if (
      maxJsonNestingDepth < 4 ||
      maxJsonNestingDepth > MaximumJsonNestingDepth
    )
      errors += PostgreSqlEnumerationCampaignV2WholePlanLimitError.JsonDepth
    if (maxJsonValues <= 0 || maxJsonValues > MaximumJsonValues)
      errors += PostgreSqlEnumerationCampaignV2WholePlanLimitError.JsonValues
    if (maxPlanNodeDepth < 0 || maxPlanNodeDepth > MaximumPlanNodeDepth)
      errors += PostgreSqlEnumerationCampaignV2WholePlanLimitError.PlanDepth
    if (maxPlanNodes <= 0 || maxPlanNodes > MaximumPlanNodes)
      errors += PostgreSqlEnumerationCampaignV2WholePlanLimitError.PlanNodes
    if (
      maxNumericTokenCharacters <= 0 ||
      maxNumericTokenCharacters > MaximumNumericTokenCharacters
    ) errors += PostgreSqlEnumerationCampaignV2WholePlanLimitError.NumericToken
    val result = errors.result().distinct
    if (result.nonEmpty) Left(result)
    else
      Right(
        new PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits(
          statementTimeoutSeconds,
          maxPlanUtf8Bytes,
          maxCumulativePlanUtf8Bytes,
          maxJsonNestingDepth,
          maxJsonValues,
          maxPlanNodeDepth,
          maxPlanNodes,
          maxNumericTokenCharacters
        )
      )
  }
}

sealed trait PostgreSqlEnumerationCampaignV2WholePlanFailureCode
    extends Product
    with Serializable {
  def id: String
}

object PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
  case object EvaluatorContractViolation
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.evaluator-contract-violation"
  }
  case object ConnectionUnavailable
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.connection-unavailable"
  }
  case object SessionConfigurationFailed
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.session-configuration-failed"
  }
  case object ProvenanceUnavailable
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.provenance-unavailable"
  }
  case object UnsafeStatement
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.unsafe-statement"
  }
  case object StatementTimedOut
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.statement-timeout"
  }
  case object StatementFailed
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.statement-failed"
  }
  case object PlanDocumentOversized
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.plan-document-oversized"
  }
  case object PlanUtf8Invalid
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.plan-utf8-invalid"
  }
  case object PlanJsonMalformed
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.plan-json-malformed"
  }
  case object PlanDepthExceeded
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.plan-depth-exceeded"
  }
  case object PlanNodeLimitExceeded
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.plan-node-limit-exceeded"
  }
  case object JsonValueLimitExceeded
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.json-value-limit-exceeded"
  }
  case object ExecutionEvidencePresent
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.execution-evidence-present"
  }
  case object NumericEvidenceUnsafe
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.numeric-evidence-unsafe"
  }
  case object ProvenanceChanged
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.provenance-changed"
  }
  case object CleanupFailed
      extends PostgreSqlEnumerationCampaignV2WholePlanFailureCode {
    override val id: String = "plan-capture.cleanup-failed"
  }
}

sealed trait PostgreSqlEnumerationCampaignV2WholePlanStage
    extends Product
    with Serializable {
  def id: String
}

object PostgreSqlEnumerationCampaignV2WholePlanStage {
  case object Session extends PostgreSqlEnumerationCampaignV2WholePlanStage {
    override val id: String = "session"
  }
  case object InitialProvenance
      extends PostgreSqlEnumerationCampaignV2WholePlanStage {
    override val id: String = "initial-provenance"
  }
  case object ActionExplain
      extends PostgreSqlEnumerationCampaignV2WholePlanStage {
    override val id: String = "action-explain"
  }
  case object FinalProvenance
      extends PostgreSqlEnumerationCampaignV2WholePlanStage {
    override val id: String = "final-provenance"
  }
  case object Cleanup extends PostgreSqlEnumerationCampaignV2WholePlanStage {
    override val id: String = "cleanup"
  }
}

final case class PostgreSqlEnumerationCampaignV2WholePlanProvenance(
    serverVersionNum: Int,
    databaseEncoding: String,
    clientEncoding: String,
    settings: Vector[PostgreSqlEnumerationCampaignV2ObservedSetting],
    settingsFingerprintSha256: String,
    settingsFingerprintScheme: String,
    settingCount: Int
) extends Serializable {
  require(
    serverVersionNum >=
      PostgreSqlEnumerationCampaignV2WholePlanCapture.MinimumServerVersionNum &&
      serverVersionNum <
        PostgreSqlEnumerationCampaignV2WholePlanCapture.MaximumServerVersionNumExclusive,
    "plan-capture server version is not PostgreSQL major 18"
  )
  val postgresqlServerMajor: Int =
    PostgreSqlEnumerationCampaignV2WholePlanCapture.PostgreSqlServerMajor
  val planGrammarId: String =
    PostgreSqlEnumerationCampaignV2WholePlanCapture.PlanGrammarId
  val planGrammarSourceCommit: String =
    PostgreSqlEnumerationCampaignV2WholePlanCapture.PlanGrammarSourceCommit
  val planGrammarSourcePath: String =
    PostgreSqlEnumerationCampaignV2WholePlanCapture.PlanGrammarSourcePath
  val planGrammarSourceSha256: String =
    PostgreSqlEnumerationCampaignV2WholePlanCapture.PlanGrammarSourceSha256
  require(databaseEncoding == "UTF8", "plan-capture database encoding is not UTF8")
  require(clientEncoding == "UTF8", "plan-capture client encoding is not UTF8")
  require(
    settings != null && settings.map(_.name) ==
      PostgreSqlEnumerationCampaignV2WholePlanProvenance.SettingNames,
    "plan-capture settings are not the exact sorted vector"
  )
  require(
    settings.forall(_.value.exists(_.nonEmpty)),
    "PostgreSQL 18 plan-capture setting evidence is incomplete"
  )
  require(
    settings
      .find(_.name == "search_path")
      .flatMap(_.value)
      .exists(_.nonEmpty),
    "plan-capture search_path evidence is absent"
  )
  require(
    settings
      .find(_.name == "transaction_isolation")
      .flatMap(_.value)
      .contains("repeatable read"),
    "plan-capture isolation evidence differs"
  )
  require(
    settings
      .find(_.name == "transaction_read_only")
      .flatMap(_.value)
      .contains("on"),
    "plan-capture read-only evidence differs"
  )
  require(
    PostgreSqlEnumerationCampaignV2WholePlanCapture.CampaignSessionSettings
      .forall { case (name, expected) =>
        settings.find(_.name == name).flatMap(_.value).contains(expected)
      },
    "plan-capture frozen session evidence differs"
  )
  require(
    settings
      .find(
        _.name ==
          PostgreSqlEnumerationCampaignV2WholePlanCapture.StatementTimeoutSettingName
      )
      .flatMap(_.value)
      .contains(
        PostgreSqlEnumerationCampaignV2WholePlanCapture.StatementTimeoutSettingValue
      ),
    "plan-capture statement timeout evidence differs"
  )
  require(
    settingsFingerprintSha256 != null &&
      settingsFingerprintSha256.matches("[0-9a-f]{64}"),
    "plan-capture settings fingerprint is invalid"
  )
  require(
    settingsFingerprintScheme ==
      PostgreSqlEnumerationCampaignV2WholePlanProvenance.FingerprintScheme,
    "plan-capture settings fingerprint scheme is invalid"
  )
  require(
    settingCount ==
      PostgreSqlEnumerationCampaignV2WholePlanProvenance.SettingNames.size,
    "plan-capture settings count differs"
  )
  require(
    settingsFingerprintSha256 ==
      PostgreSqlEnumerationCampaignV2WholePlanProvenance
        .fingerprint(settings),
    "plan-capture settings fingerprint differs"
  )
}

final case class PostgreSqlEnumerationCampaignV2ObservedSetting(
    name: String,
    value: Option[String]
) extends Serializable {
  require(name != null && name.nonEmpty, "observed setting name is empty")
  require(
    value != null && value.forall(text =>
      text != null &&
        text.length <=
          PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
            .MaximumSettingValueUtf8Bytes &&
        text.indexOf('\u0000') < 0 &&
        PostgreSqlEnumerationCampaignV2ObservedSetting.hasValidUnicode(text) &&
        text.getBytes(StandardCharsets.UTF_8).length <=
          PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
            .MaximumSettingValueUtf8Bytes
    ),
    "observed setting value is invalid"
  )
}

object PostgreSqlEnumerationCampaignV2ObservedSetting {
  private def hasValidUnicode(value: String): Boolean = {
    var index = 0
    while (index < value.length) {
      val character = value.charAt(index)
      if (Character.isHighSurrogate(character)) {
        if (
          index + 1 >= value.length ||
          !Character.isLowSurrogate(value.charAt(index + 1))
        ) return false
        index += 2
      } else if (Character.isLowSurrogate(character)) return false
      else index += 1
    }
    true
  }
}

object PostgreSqlEnumerationCampaignV2WholePlanProvenance {
  val FingerprintScheme: String =
    "length-prefixed-utf8-sorted-name-value-v1"
  val FingerprintDomainUtf8: String =
    "yrw-enumeration-plan-capture-session-settings/v1\u0000"

  val SettingNames: Vector[String] =
    (
      PostgreSqlEnumerationCampaignV2WholePlanCapture.PlannerSettingNames ++
        PostgreSqlEnumerationCampaignV2WholePlanCapture.CampaignSessionSettings
          .map(_._1) ++ Vector(
          "search_path",
          "client_encoding",
          "server_encoding",
          PostgreSqlEnumerationCampaignV2WholePlanCapture.StatementTimeoutSettingName,
          "transaction_isolation",
          "transaction_read_only"
        )
    ).sorted.distinct

  private[postgresql] def fingerprint(
      settings: Vector[PostgreSqlEnumerationCampaignV2ObservedSetting]
  ): String = {
    val canonical = new java.io.ByteArrayOutputStream()
    canonical.write(FingerprintDomainUtf8.getBytes(StandardCharsets.UTF_8))
    settings.foreach { setting =>
      val name = setting.name.getBytes(StandardCharsets.UTF_8)
      canonical.write(name.length.toString.getBytes(StandardCharsets.US_ASCII))
      canonical.write(':')
      canonical.write(name)
      canonical.write('=')
      setting.value match {
        case None => canonical.write("-1:".getBytes(StandardCharsets.US_ASCII))
        case Some(text) =>
          val value = text.getBytes(StandardCharsets.UTF_8)
          canonical.write(value.length.toString.getBytes(StandardCharsets.US_ASCII))
          canonical.write(':')
          canonical.write(value)
      }
      canonical.write('\n')
    }
    sha256(canonical.toByteArray)
  }

  private def sha256(bytes: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
    val result = new StringBuilder(digest.length * 2)
    digest.foreach { byte =>
      val unsigned = byte & 0xff
      result.append("0123456789abcdef".charAt(unsigned >>> 4))
      result.append("0123456789abcdef".charAt(unsigned & 0x0f))
    }
    result.result()
  }
}

final case class PostgreSqlEnumerationCampaignV2WholePlanObservation(
    actionId: String,
    actionKind: PostgreSqlEnumerationCampaignV2PlanActionKind,
    renderedStatementSha256: String,
    document: PostgreSqlEnumerationCampaignV2WholePlanDocument
) extends Serializable {
  require(actionId != null && actionId.nonEmpty, "observed action ID is empty")
  require(actionKind != null, "observed action kind is null")
  require(
    (actionKind == PostgreSqlEnumerationCampaignV2PlanActionKind.Native &&
      actionId == "native") ||
      (actionKind ==
        PostgreSqlEnumerationCampaignV2PlanActionKind.RetainedCandidate &&
        actionId.matches("rewrite-[0-9]{4}")),
    "observed action identity differs"
  )
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}"),
    "observed SQL digest is invalid"
  )
  require(document != null, "observed plan document is null")
}

final case class PostgreSqlEnumerationCampaignV2WholePlanBatch(
    provenance: PostgreSqlEnumerationCampaignV2WholePlanProvenance,
    observations: Vector[PostgreSqlEnumerationCampaignV2WholePlanObservation],
    totalPlanUtf8Bytes: Long,
    limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
) extends Serializable {
  require(provenance != null, "plan-capture provenance is null")
  require(observations != null && observations.nonEmpty, "plan capture is empty")
  require(observations.forall(_ != null), "plan-capture observation is null")
  require(limits != null, "plan-capture limits are null")
  require(
    observations.size <=
      PostgreSqlEnumerationCampaignV2WholePlanRequest.MaximumActions,
    "plan-capture observation count is oversized"
  )
  require(
    observations.head.actionKind ==
      PostgreSqlEnumerationCampaignV2PlanActionKind.Native &&
      observations.tail.forall(
        _.actionKind ==
          PostgreSqlEnumerationCampaignV2PlanActionKind.RetainedCandidate
      ),
    "plan-capture observations are not source-first"
  )
  require(
    observations.zipWithIndex.forall { case (observation, index) =>
      observation.actionId ==
        (if (index == 0) "native" else f"rewrite-$index%04d")
    },
    "plan-capture observations are not in exact registry order"
  )
  require(
    observations.forall(_.document.utf8ByteLength <= limits.maxPlanUtf8Bytes),
    "plan-capture observation exceeds its per-plan byte limit"
  )
  require(
    totalPlanUtf8Bytes >= 0L &&
      totalPlanUtf8Bytes <= limits.maxCumulativePlanUtf8Bytes,
    "plan-capture total exceeds its cumulative byte limit"
  )
  require(
    BigInt(totalPlanUtf8Bytes) ==
      observations.map(value => BigInt(value.document.utf8ByteLength)).sum,
    "plan-capture cumulative bytes differ"
  )
  val postgresqlServerMajor: Int = provenance.postgresqlServerMajor
  val planGrammarId: String = provenance.planGrammarId
  val planGrammarSourceCommit: String = provenance.planGrammarSourceCommit
  val planGrammarSourcePath: String = provenance.planGrammarSourcePath
  val planGrammarSourceSha256: String = provenance.planGrammarSourceSha256
}

final case class PostgreSqlEnumerationCampaignV2WholePlanFailure(
    code: PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
    stage: PostgreSqlEnumerationCampaignV2WholePlanStage,
    failedActionIndex: Option[Int],
    completed: Vector[PostgreSqlEnumerationCampaignV2WholePlanObservation]
) extends Serializable {
  require(code != null, "plan-capture failure code is null")
  require(stage != null, "plan-capture failure stage is null")
  require(
    failedActionIndex != null && failedActionIndex.forall(_ >= 0),
    "plan-capture failed action index is invalid"
  )
  require(completed != null, "plan-capture completed prefix is null")
  require(completed.forall(_ != null), "plan-capture completed observation is null")
  require(
    completed.size <=
      PostgreSqlEnumerationCampaignV2WholePlanRequest.MaximumActions &&
      completed.zipWithIndex.forall { case (observation, index) =>
        observation.actionId ==
          (if (index == 0) "native" else f"rewrite-$index%04d")
      },
    "plan-capture completed observations are not an exact registry prefix"
  )
  require(
    (stage == PostgreSqlEnumerationCampaignV2WholePlanStage.ActionExplain) ==
      failedActionIndex.nonEmpty,
    "only an action EXPLAIN failure may identify an action"
  )
  require(
    failedActionIndex.forall(index =>
      index == completed.size &&
        index < PostgreSqlEnumerationCampaignV2WholePlanRequest.MaximumActions
    ),
    "failed action does not immediately follow the completed prefix"
  )
}

sealed trait PostgreSqlEnumerationCampaignV2WholePlanConfigurationError
    extends Product
    with Serializable {
  def code: String
}

object PostgreSqlEnumerationCampaignV2WholePlanConfigurationError {
  case object MissingDataSource
      extends PostgreSqlEnumerationCampaignV2WholePlanConfigurationError {
    override val code: String = "plan-capture-configuration.data-source"
  }
  case object MissingPhysicalSchema
      extends PostgreSqlEnumerationCampaignV2WholePlanConfigurationError {
    override val code: String = "plan-capture-configuration.schema"
  }
}

trait PostgreSqlEnumerationCampaignV2WholePlanCapture {
  def capture(
      request: PostgreSqlEnumerationCampaignV2WholePlanRequest,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailure,
    PostgreSqlEnumerationCampaignV2WholePlanBatch
  ]
}

/**
  * Campaign-v2-specific, source-first ANALYZE-FALSE whole-plan capture.
  *
  * Every batch uses exactly one caller-supplied JDBC connection, an explicit
  * repeatable-read/read-only transaction, a transaction-local schema-bound
  * search path, a transaction-local ten-second PostgreSQL statement timeout,
  * and the four frozen authoring-session settings. It emits the exact
  * validated UTF-8 bytes returned for each JDBC result value and a typed
  * walker receipt only; it does not claim wire bytes and does not emit or
  * enable the dormant v2 `postgresql-plan` package payload. The timeout bounds
  * PostgreSQL statements, not connection acquisition, network fetch, or
  * cleanup wall time.
  */
object PostgreSqlEnumerationCampaignV2WholePlanCapture {
  val PostgreSqlServerMajor: Int = 18
  val MinimumServerVersionNum: Int = 180000
  val MaximumServerVersionNumExclusive: Int = 190000
  val PlanGrammarId: String =
    "postgresql-18-core-nonverbose-analyze-false-select-json-v1"
  val PlanGrammarSourceCommit: String =
    "739f5a947d57845ec60d998e8b0e357e8b906049"
  val PlanGrammarSourcePath: String = "src/backend/commands/explain.c"
  val PlanGrammarSourceSha256: String =
    "34c86d6070224a0e981efef51f79101d6d505e5874f1684ace183034bab14bb4"
  val PlannerSettingNames: Vector[String] =
    PostgreSqlExplainCostProvenance.PlannerSettingNames
  val PlannerSettingNamesLfSha256: String =
    "11064c3c67b3e7471760a4b55dc952bec89664c10b98057384cae038c3108c97"
  require(
    PlannerSettingNames.size == 53 &&
      PlannerSettingNames == PlannerSettingNames.sorted.distinct &&
      sha256((PlannerSettingNames.mkString("\n") + "\n").getBytes(StandardCharsets.UTF_8)) ==
        PlannerSettingNamesLfSha256,
    "PostgreSQL 18 planner-setting vector differs from the frozen contract"
  )
  val AllowedPlanNodeTypes: Vector[String] = Vector(
    "Aggregate",
    "Append",
    "Bitmap Heap Scan",
    "Bitmap Index Scan",
    "BitmapAnd",
    "BitmapOr",
    "CTE Scan",
    "Function Scan",
    "Gather",
    "Gather Merge",
    "Group",
    "Hash",
    "Hash Join",
    "Incremental Sort",
    "Index Only Scan",
    "Index Scan",
    "Limit",
    "Materialize",
    "Memoize",
    "Merge Append",
    "Merge Join",
    "Named Tuplestore Scan",
    "Nested Loop",
    "ProjectSet",
    "Recursive Union",
    "Result",
    "Sample Scan",
    "Seq Scan",
    "SetOp",
    "Sort",
    "Subquery Scan",
    "Table Function Scan",
    "Tid Range Scan",
    "Tid Scan",
    "Unique",
    "Values Scan",
    "WindowAgg",
    "WorkTable Scan"
  )
  require(
    AllowedPlanNodeTypes == AllowedPlanNodeTypes.sorted.distinct,
    "PostgreSQL 18 allowed plan-node labels are not sorted and unique"
  )
  val MaximumPhysicalSchemaCharacters: Int = 63
  val MaximumPhysicalSchemaUtf8Bytes: Int = 63

  val ExplainPrefix: String =
    "EXPLAIN (FORMAT JSON, ANALYZE FALSE, VERBOSE FALSE, COSTS TRUE, " +
      "SETTINGS TRUE, SUMMARY FALSE) "
  val TransactionStatement: String =
    "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY"
  val SearchPathStatement: String =
    "SELECT pg_catalog.set_config(" +
      "'search_path', 'pg_catalog, ' || pg_catalog.quote_ident(?) || ', pg_temp', true)"
  val SessionSettingStatement: String =
    "SELECT pg_catalog.set_config(?, ?, true)"
  val StatementTimeoutSettingName: String = "statement_timeout"
  val StatementTimeoutSettingValue: String =
    PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.StatementTimeoutSeconds.toString +
      "s"

  /** Exact settings frozen by the campaign-v2 authoring request. */
  val CampaignSessionSettings: Vector[(String, String)] = Vector(
    "datestyle" -> "ISO, MDY",
    "extra_float_digits" -> "3",
    "standard_conforming_strings" -> "on",
    "timezone" -> "UTC"
  )

  def forDataSource(
      dataSource: DataSource,
      physicalSchemaName: String
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanConfigurationError,
    PostgreSqlEnumerationCampaignV2WholePlanCapture
  ] = {
    if (dataSource eq null)
      Left(
        PostgreSqlEnumerationCampaignV2WholePlanConfigurationError.MissingDataSource
      )
    else if (
      physicalSchemaName == null || physicalSchemaName.isEmpty ||
      physicalSchemaName.length > MaximumPhysicalSchemaUtf8Bytes ||
      physicalSchemaName.trim.isEmpty ||
      !hasValidUnicode(physicalSchemaName) ||
      physicalSchemaName.codePointCount(0, physicalSchemaName.length) >
        MaximumPhysicalSchemaCharacters ||
      physicalSchemaName.getBytes(StandardCharsets.UTF_8).length >
        MaximumPhysicalSchemaUtf8Bytes ||
      physicalSchemaName.indexOf('\u0000') >= 0
    ) Left(
      PostgreSqlEnumerationCampaignV2WholePlanConfigurationError.MissingPhysicalSchema
    )
    else
      Right(
        new ManagedCapture(() => dataSource.getConnection, physicalSchemaName)
      )
  }

  private def hasValidUnicode(value: String): Boolean = {
    if (value == null) return false
    var index = 0
    while (index < value.length) {
      val character = value.charAt(index)
      if (Character.isHighSurrogate(character)) {
        if (
          index + 1 >= value.length ||
          !Character.isLowSurrogate(value.charAt(index + 1))
        ) return false
        index += 2
      } else if (Character.isLowSurrogate(character)) return false
      else index += 1
    }
    true
  }

  private final class ManagedCapture(
      openConnection: () => Connection,
      physicalSchemaName: String
  ) extends PostgreSqlEnumerationCampaignV2WholePlanCapture {
    override def capture(
        request: PostgreSqlEnumerationCampaignV2WholePlanRequest,
        limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
    ): Either[
      PostgreSqlEnumerationCampaignV2WholePlanFailure,
      PostgreSqlEnumerationCampaignV2WholePlanBatch
    ] = {
      if (request == null || limits == null)
        return Left(
          failure(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.EvaluatorContractViolation,
            PostgreSqlEnumerationCampaignV2WholePlanStage.Session
          )
        )
      val connection =
        try openConnection()
        catch {
          case NonFatal(_) =>
            return Left(
              failure(
                PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ConnectionUnavailable,
                PostgreSqlEnumerationCampaignV2WholePlanStage.Session
              )
            )
        }
      if (connection eq null)
        Left(
          failure(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ConnectionUnavailable,
            PostgreSqlEnumerationCampaignV2WholePlanStage.Session
          )
        )
      else captureAndClose(connection, physicalSchemaName, request, limits)
    }
  }

  private def captureAndClose(
      connection: Connection,
      physicalSchemaName: String,
      request: PostgreSqlEnumerationCampaignV2WholePlanRequest,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailure,
    PostgreSqlEnumerationCampaignV2WholePlanBatch
  ] = {
    var autoCommitDisabled = false
    var cleanupFailed = false
    var result: Either[
      PostgreSqlEnumerationCampaignV2WholePlanFailure,
      PostgreSqlEnumerationCampaignV2WholePlanBatch
    ] = Left(
      failure(
        PostgreSqlEnumerationCampaignV2WholePlanFailureCode.SessionConfigurationFailed,
        PostgreSqlEnumerationCampaignV2WholePlanStage.Session
      )
    )

    try {
      if (connection.isClosed || !connection.getAutoCommit) ()
      else {
        connection.setAutoCommit(false)
        autoCommitDisabled = true
        executeTransactionStatement(connection, limits) match {
          case Left(code) =>
            result = Left(
              failure(code, PostgreSqlEnumerationCampaignV2WholePlanStage.Session)
            )
          case Right(_) =>
            configureSearchPath(connection, physicalSchemaName, limits) match {
              case Left(code) =>
                result = Left(
                  failure(code, PostgreSqlEnumerationCampaignV2WholePlanStage.Session)
                )
              case Right(_) =>
                configureStatementTimeout(connection, limits) match {
                  case Left(code) =>
                    result = Left(
                      failure(code, PostgreSqlEnumerationCampaignV2WholePlanStage.Session)
                    )
                  case Right(_) =>
                    configureCampaignSettings(connection, limits) match {
                      case Left(code) =>
                        result = Left(
                          failure(
                            code,
                            PostgreSqlEnumerationCampaignV2WholePlanStage.Session
                          )
                        )
                      case Right(_) =>
                        result = captureTransaction(connection, request, limits)
                    }
                }
            }
        }
      }
    } catch {
      case NonFatal(problem) =>
        result = Left(
          failure(
            sqlFailureCode(
              problem,
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.SessionConfigurationFailed
            ),
            PostgreSqlEnumerationCampaignV2WholePlanStage.Session
          )
        )
    } finally {
      if (autoCommitDisabled) {
        try connection.rollback()
        catch { case NonFatal(_) => cleanupFailed = true }
        try connection.setAutoCommit(true)
        catch { case NonFatal(_) => cleanupFailed = true }
      }
      try connection.close()
      catch { case NonFatal(_) => cleanupFailed = true }
    }

    if (cleanupFailed) {
      val completed = result.fold(_.completed, _.observations)
      Left(
        failure(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.CleanupFailed,
          PostgreSqlEnumerationCampaignV2WholePlanStage.Cleanup,
          completed = completed
        )
      )
    } else result
  }

  private def captureTransaction(
      connection: Connection,
      request: PostgreSqlEnumerationCampaignV2WholePlanRequest,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailure,
    PostgreSqlEnumerationCampaignV2WholePlanBatch
  ] = {
    val initial = readProvenance(connection, limits) match {
      case Left(code) =>
        return Left(
          failure(
            code,
            PostgreSqlEnumerationCampaignV2WholePlanStage.InitialProvenance
          )
        )
      case Right(value) => value
    }

    var completed = Vector.empty[PostgreSqlEnumerationCampaignV2WholePlanObservation]
    var cumulativeBytes = 0L
    val expectedSettings = initial.settings.iterator.map { setting =>
      setting.name -> setting.value.get
    }.toMap
    var index = 0
    while (index < request.actions.size) {
      val action = request.actions(index)
      runExplain(
        connection,
        action,
        limits,
        cumulativeBytes,
        expectedSettings
      ) match {
        case Left(code) =>
          return Left(
            failure(
              code,
              PostgreSqlEnumerationCampaignV2WholePlanStage.ActionExplain,
              failedActionIndex = Some(index),
              completed = completed
            )
          )
        case Right(observation) =>
          cumulativeBytes += observation.document.utf8ByteLength
          completed :+= observation
      }
      index += 1
    }

    readProvenance(connection, limits) match {
      case Left(code) =>
        Left(
          failure(
            code,
            PostgreSqlEnumerationCampaignV2WholePlanStage.FinalProvenance,
            completed = completed
          )
        )
      case Right(finalValue) if finalValue != initial =>
        Left(
          failure(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceChanged,
            PostgreSqlEnumerationCampaignV2WholePlanStage.FinalProvenance,
            completed = completed
          )
        )
      case Right(_) =>
        Right(
          PostgreSqlEnumerationCampaignV2WholePlanBatch(
            initial,
            completed,
            cumulativeBytes,
            limits
          )
        )
    }
  }

  private def executeTransactionStatement(
      connection: Connection,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Unit] = {
    var statement: Statement = null
    var closeFailed = false
    val result =
      try {
        statement = connection.createStatement()
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.execute(TransactionStatement)
        Right(())
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.SessionConfigurationFailed
            )
          )
      } finally {
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed)
      Left(PostgreSqlEnumerationCampaignV2WholePlanFailureCode.CleanupFailed)
    else result
  }

  private def configureSearchPath(
      connection: Connection,
      physicalSchemaName: String,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Unit] = {
    var statement: java.sql.PreparedStatement = null
    var resultSet: ResultSet = null
    var closeFailed = false
    val result =
      try {
        statement = connection.prepareStatement(SearchPathStatement)
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.setMaxRows(2)
        statement.setString(1, physicalSchemaName)
        resultSet = statement.executeQuery()
        if (
          resultSet.getMetaData.getColumnCount != 1 || !resultSet.next() ||
          resultSet.getString(1) == null || resultSet.next()
        ) Left(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.SessionConfigurationFailed
        )
        else Right(())
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.SessionConfigurationFailed
            )
          )
      } finally {
        if (resultSet != null) {
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed)
      Left(PostgreSqlEnumerationCampaignV2WholePlanFailureCode.CleanupFailed)
    else result
  }

  private def configureCampaignSettings(
      connection: Connection,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Unit] =
    configureSettings(connection, CampaignSessionSettings, limits)

  private def configureStatementTimeout(
      connection: Connection,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Unit] =
    configureSettings(
      connection,
      Vector(StatementTimeoutSettingName -> StatementTimeoutSettingValue),
      limits
    )

  private def configureSettings(
      connection: Connection,
      frozenSettings: Vector[(String, String)],
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Unit] = {
    var statement: java.sql.PreparedStatement = null
    var resultSet: ResultSet = null
    var closeFailed = false
    val result =
      try {
        statement = connection.prepareStatement(SessionSettingStatement)
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.setMaxRows(2)
        var failureCode: Option[
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode
        ] = None
        val settings = frozenSettings.iterator
        while (settings.hasNext && failureCode.isEmpty) {
          val (name, expected) = settings.next()
          statement.setString(1, name)
          statement.setString(2, expected)
          resultSet = statement.executeQuery()
          if (
            resultSet.getMetaData.getColumnCount != 1 || !resultSet.next() ||
            resultSet.getString(1) != expected || resultSet.next()
          ) failureCode = Some(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.SessionConfigurationFailed
          )
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
          resultSet = null
        }
        failureCode.toLeft(())
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.SessionConfigurationFailed
            )
          )
      } finally {
        if (resultSet != null) {
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed)
      Left(PostgreSqlEnumerationCampaignV2WholePlanFailureCode.CleanupFailed)
    else result
  }

  private def readProvenance(
      connection: Connection,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
    PostgreSqlEnumerationCampaignV2WholePlanProvenance
  ] = {
    for {
      version <- readServerVersion(connection, limits)
      settings <- readSettings(connection, limits)
      observed <-
        try
          Right(
            settings.map { case (name, value) =>
              PostgreSqlEnumerationCampaignV2ObservedSetting(name, value)
            }
          )
        catch {
          case NonFatal(_) =>
            Left(
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceUnavailable
            )
        }
      databaseEncoding <- observed
        .find(_.name == "server_encoding")
        .flatMap(_.value)
        .filter(_ == "UTF8")
        .toRight(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceUnavailable
        )
      clientEncoding <- observed
        .find(_.name == "client_encoding")
        .flatMap(_.value)
        .filter(_ == "UTF8")
        .toRight(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceUnavailable
        )
      provenance <-
        try
          Right(
            PostgreSqlEnumerationCampaignV2WholePlanProvenance(
              version,
              databaseEncoding,
              clientEncoding,
              observed,
              PostgreSqlEnumerationCampaignV2WholePlanProvenance.fingerprint(
                observed
              ),
              PostgreSqlEnumerationCampaignV2WholePlanProvenance.FingerprintScheme,
              settings.size
            )
          )
        catch {
          case NonFatal(_) =>
            Left(
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceUnavailable
            )
        }
    } yield provenance
  }

  private def readServerVersion(
      connection: Connection,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Int] = {
    var statement: Statement = null
    var resultSet: ResultSet = null
    var closeFailed = false
    val result =
      try {
        statement = connection.createStatement()
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.setMaxRows(2)
        resultSet = statement.executeQuery(
          "SELECT current_setting('server_version_num')"
        )
        if (resultSet.getMetaData.getColumnCount != 1 || !resultSet.next())
          Left(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceUnavailable
          )
        else {
          val parsed = Option(resultSet.getString(1)).flatMap(_.toIntOption)
          if (
            resultSet.next() ||
            parsed.forall(value =>
              value < MinimumServerVersionNum ||
                value >= MaximumServerVersionNumExclusive
            )
          )
            Left(
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceUnavailable
            )
          else Right(parsed.get)
        }
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceUnavailable
            )
          )
      } finally {
        if (resultSet != null) {
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed)
      Left(PostgreSqlEnumerationCampaignV2WholePlanFailureCode.CleanupFailed)
    else result
  }

  private def readSettings(
      connection: Connection,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
    Vector[(String, Option[String])]
  ] = {
    val names = PostgreSqlEnumerationCampaignV2WholePlanProvenance.SettingNames
    val values = names.indices.map(index => s"(${index + 1},?)").mkString(",")
    val sql =
      s"SELECT requested.name, current_setting(requested.name, true) " +
        s"FROM (VALUES $values) AS requested(ordinal, name) " +
        s"ORDER BY requested.ordinal"
    var statement: java.sql.PreparedStatement = null
    var resultSet: ResultSet = null
    var closeFailed = false
    val result =
      try {
        statement = connection.prepareStatement(sql)
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.setMaxRows(names.size + 1)
        names.zipWithIndex.foreach { case (name, index) =>
          statement.setString(index + 1, name)
        }
        resultSet = statement.executeQuery()
        val observed = Vector.newBuilder[(String, Option[String])]
        while (resultSet.next())
          observed += resultSet.getString(1) -> Option(resultSet.getString(2))
        val rows = observed.result()
        if (rows.map(_._1) != names)
          Left(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceUnavailable
          )
        else Right(rows)
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceUnavailable
            )
          )
      } finally {
        if (resultSet != null) {
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed)
      Left(PostgreSqlEnumerationCampaignV2WholePlanFailureCode.CleanupFailed)
    else result
  }

  private def runExplain(
      connection: Connection,
      action: PostgreSqlEnumerationCampaignV2PlanAction,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits,
      cumulativeBytes: Long,
      expectedSettings: Map[String, String]
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
    PostgreSqlEnumerationCampaignV2WholePlanObservation
  ] = {
    if (!PostgreSqlExplainSafeStatement.isReadOnlySelect(action.sql))
      return Left(
        PostgreSqlEnumerationCampaignV2WholePlanFailureCode.UnsafeStatement
      )
    var statement: Statement = null
    var resultSet: ResultSet = null
    var input: InputStream = null
    var closeFailed = false
    val result =
      try {
        statement = connection.createStatement()
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.setMaxRows(2)
        resultSet = statement.executeQuery(ExplainPrefix + action.sql)
        if (resultSet.getMetaData.getColumnCount != 1 || !resultSet.next())
          Left(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
          )
        else {
          val remaining = limits.maxCumulativePlanUtf8Bytes - cumulativeBytes
          if (remaining <= 0L)
            Left(
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanDocumentOversized
            )
          else {
            input = resultSet.getBinaryStream(1)
            readBounded(input, math.min(limits.maxPlanUtf8Bytes, remaining))
              .flatMap { bytes =>
                if (resultSet.next())
                  Left(
                    PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
                  )
                else
                  PostgreSqlEnumerationCampaignV2PlanWalker
                    .validate(bytes, limits)
                    .flatMap { document =>
                      val capturedSettings =
                        document.settings.iterator.map(setting =>
                          setting.name -> setting.value
                        ).toMap
                      if (
                        !expectedSettings
                          .get("search_path")
                          .exists(expected =>
                            capturedSettings.get("search_path").contains(expected)
                          ) ||
                        capturedSettings.exists { case (name, value) =>
                          expectedSettings.get(name).forall(_ != value)
                        }
                      )
                        Left(
                          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ProvenanceChanged
                        )
                      else
                        Right(
                          PostgreSqlEnumerationCampaignV2WholePlanObservation(
                            action.actionId,
                            action.actionKind,
                            action.renderedStatementSha256,
                            document
                          )
                        )
                    }
              }
          }
        }
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.StatementFailed
            )
          )
      } finally {
        if (input != null) {
          try input.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (resultSet != null) {
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed)
      Left(PostgreSqlEnumerationCampaignV2WholePlanFailureCode.CleanupFailed)
    else result
  }

  private def readBounded(
      input: InputStream,
      maximumBytes: Long
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Array[Byte]] = {
    if (input == null)
      return Left(
        PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
      )
    if (maximumBytes <= 0L)
      return Left(
        PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanDocumentOversized
      )
    val output =
      new ByteArrayOutputStream(math.min(maximumBytes, 8192L).toInt)
    val buffer = new Array[Byte](8192)
    var total = 0L
    var read = input.read(buffer)
    while (read != -1) {
      if (read == 0) {
        val one = input.read()
        if (one == -1) read = -1
        else {
          if (total == maximumBytes)
            return Left(
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanDocumentOversized
            )
          output.write(one)
          total += 1L
          read = input.read(buffer)
        }
      } else {
        if (read.toLong > maximumBytes - total)
          return Left(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanDocumentOversized
          )
        output.write(buffer, 0, read)
        total += read.toLong
        read = input.read(buffer)
      }
    }
    Right(output.toByteArray)
  }

  private def sha256(bytes: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
    val result = new StringBuilder(digest.length * 2)
    digest.foreach { byte =>
      val unsigned = byte & 0xff
      result.append("0123456789abcdef".charAt(unsigned >>> 4))
      result.append("0123456789abcdef".charAt(unsigned & 0x0f))
    }
    result.result()
  }

  private def sqlFailureCode(
      problem: Throwable,
      otherwise: PostgreSqlEnumerationCampaignV2WholePlanFailureCode
  ): PostgreSqlEnumerationCampaignV2WholePlanFailureCode = problem match {
    case _: SQLTimeoutException =>
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode.StatementTimedOut
    case sql: SQLException if sql.getSQLState == "57014" =>
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode.StatementTimedOut
    case _ => otherwise
  }

  private def failure(
      code: PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
      stage: PostgreSqlEnumerationCampaignV2WholePlanStage,
      failedActionIndex: Option[Int] = None,
      completed: Vector[PostgreSqlEnumerationCampaignV2WholePlanObservation] =
        Vector.empty
  ): PostgreSqlEnumerationCampaignV2WholePlanFailure =
    PostgreSqlEnumerationCampaignV2WholePlanFailure(
      code,
      stage,
      failedActionIndex,
      completed
    )
}
