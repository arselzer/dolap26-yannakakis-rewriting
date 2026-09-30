package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermissions}
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.security.MessageDigest
import java.util.Arrays

import scala.jdk.CollectionConverters._
import scala.util.Using
import scala.util.control.NonFatal

import com.fasterxml.jackson.core.{JsonFactory, JsonParser}
import com.fasterxml.jackson.databind.{DeserializationFeature, JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.ObjectNode

import at.ac.tuwien.dbai.yannakakis.api.postgresql._

private[cli] sealed trait PostgreSqlEnumerationLiveDataV1ProtocolFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlEnumerationLiveDataV1ProtocolFailure {
  case object BundleInvalid extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.request-bundle-invalid"
  }
  case object RequestTooLarge extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.request-oversized"
  }
  case object RequestInvalid extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.request-invalid"
  }
  case object RequestNonCanonical extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.request-noncanonical"
  }
  case object RequestContractMismatch extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.request-contract-mismatch"
  }
  case object RequestSealMismatch extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.request-seal-mismatch"
  }
  case object RequestChanged extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.request-bundle-changed"
  }
  case object CaptureMismatch extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.capture-mismatch"
  }
  case object AssemblyFailed extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.assembly-failed"
  }
  case object AssemblyTooLarge extends PostgreSqlEnumerationLiveDataV1ProtocolFailure {
    val code = "live-data-v1.assembly-oversized"
  }
}

private[cli] final case class PostgreSqlEnumerationLiveDataV1AdmittedTable(
    request: PostgreSqlEnumerationLiveDataV1TableRequest,
    document: JsonNode
)

private[cli] final case class PostgreSqlEnumerationLiveDataV1AssemblyBinding(
    byteLength: Long,
    sha256: String
) {
  require(byteLength > 0L)
  require(sha256 != null && sha256.matches("[0-9a-f]{64}"))
}

/** Exact immutable request and completion bytes admitted from one private bundle. */
private[cli] final class PostgreSqlEnumerationLiveDataV1AdmittedRequest private[cli] (
    val bundleRoot: Path,
    val requestDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
    val requestNode: JsonNode,
    val requestSha256: String,
    val completeDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
    val completeSha256: String,
    val workloadId: String,
    val schemaName: String,
    val totalCopyBytes: Long,
    val tables: Vector[PostgreSqlEnumerationLiveDataV1AdmittedTable],
    val assemblyBinding: PostgreSqlEnumerationLiveDataV1AssemblyBinding,
    val captureRequest: PostgreSqlEnumerationLiveDataV1Request
) {
  require(bundleRoot != null && bundleRoot.isAbsolute)
  require(requestDocument != null && requestNode != null && completeDocument != null)
  require(requestSha256.matches("[0-9a-f]{64}") && completeSha256.matches("[0-9a-f]{64}"))
  require(workloadId != null && workloadId.nonEmpty)
  require(schemaName != null && schemaName.nonEmpty)
  require(totalCopyBytes >= 0L)
  require(
    tables != null && tables.nonEmpty && assemblyBinding != null && captureRequest != null
  )
}

/** Strict Scala admission for the Python-authored private request bundle. */
private[cli] object PostgreSqlEnumerationLiveDataV1RequestParser {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._
  import PostgreSqlEnumerationLiveDataV1ProtocolFailure._

  val ProtocolVersion = "yrw-enumeration-live-data-acquisition-protocol/v1"
  val RequestSchema = "yrw-enumeration-live-acquisition-request/v1"
  val RequestCompleteSchema =
    "yrw-enumeration-live-acquisition-request-complete/v1"
  val MaximumDocumentBytes = 64 * 1024 * 1024
  val MaximumCopyBytesPerDataset = 512L * 1024L * 1024L
  val MaximumScalaAssemblyBytes = 256L * 1024L * 1024L
  val StatementTimeoutMs = "300000"
  val LockTimeoutMs = "30000"
  val JdbcVersion = "42.7.2"
  val CliFqcn =
    "at.ac.tuwien.dbai.yannakakis.cli.PostgreSqlEnumerationLiveDataV1Cli"

  private val RequestSealDomain =
    "yrw-enumeration-live-acquisition-request-seal/v1\u0000".getBytes(StandardCharsets.UTF_8)
  private val CopyRootDomain =
    "yrw-enumeration-live-acquisition-copy-root/v1\u0000".getBytes(StandardCharsets.UTF_8)
  private val CompleteSealDomain =
    "yrw-enumeration-live-acquisition-request-complete-seal/v1\u0000"
      .getBytes(StandardCharsets.UTF_8)
  private val DatasetSealDomain =
    "yrw-enumeration-live-dataset-manifest-seal/v1\u0000".getBytes(StandardCharsets.UTF_8)

  private val RootKeys = Set(
    "artifact_status", "attempt_id", "campaign_id", "claims", "classification",
    "data_semantics", "dataset_id", "dataset_manifest", "lineage_group_id",
    "load_contract_sha256", "partition", "postgresql_contract", "protocol_version",
    "schema_name", "schema_version", "seal_sha256", "source_bindings", "tables",
    "toolchain", "total_copy_bytes", "workload_id"
  )
  private val CompleteKeys = Set(
    "artifact_status", "claims", "copy_root_sha256", "protocol_version", "request",
    "schema_version", "seal_sha256", "table_count", "total_copy_bytes", "workload_id"
  )
  private val TableKeys = Set(
    "analyze_sql", "content", "copy_file", "copy_from_stdin_sql", "copy_to_stdout_sql",
    "create_table_sql", "statistics_target_sql", "table_definition",
    "table_definition_sha256", "table_name"
  )
  private val FalseClaims = Map(
    "coherent_atomic_snapshot" -> false,
    "external_durable_chronology" -> false,
    "independent_live_postgresql_observation" -> false,
    "live_loaded_state_truth" -> false,
    "no_unrecorded_attempts" -> false,
    "outcomes_in_scope" -> false,
    "publication_history" -> false,
    "production_policy" -> false,
    "reportable" -> false,
    "expected_copy_bytes_materialized" -> true,
    "request_complete_for_bound_workload" -> true
  )
  private val SafeId = "[A-Za-z0-9][A-Za-z0-9._:-]{0,191}"
  private val Identifier = "[a-z][a-z0-9_]{0,62}"
  private val Sha256 = "[0-9a-f]{64}"
  private val DirectoryPermissions = PosixFilePermissions.fromString("rwx------")
  private val FilePermissions = PosixFilePermissions.fromString("rw-------")
  private lazy val CurrentUid = new com.sun.security.auth.module.UnixSystem().getUid

  private val Mapper = {
    val factory = new JsonFactory()
      .enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
    val mapper = new ObjectMapper(factory)
    mapper.enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS)
    mapper.enable(DeserializationFeature.FAIL_ON_READING_DUP_TREE_KEY)
    mapper
  }

  def load(
      bundle: Path
  ): Either[PostgreSqlEnumerationLiveDataV1ProtocolFailure, PostgreSqlEnumerationLiveDataV1AdmittedRequest] = {
    val root = validateBundleRoot(bundle).getOrElse(return Left(BundleInvalid))
    val requestBytes = readPrivateDocument(root.resolve("request.json")) match {
      case Left(problem) => return Left(problem)
      case Right(bytes)   => bytes
    }
    val completeBytes = readPrivateDocument(root.resolve("complete.json")) match {
      case Left(problem) => return Left(problem)
      case Right(bytes)   => bytes
    }
    val requestNode = parseCanonical(requestBytes).getOrElse(return Left(RequestInvalid))
    val completeNode = parseCanonical(completeBytes).getOrElse(return Left(RequestInvalid))
    val requestDocument = canonicalDocument(requestNode)
    val completeDocument = canonicalDocument(completeNode)
    if (
      !Arrays.equals(requestBytes, requestDocument.bytes) ||
      !Arrays.equals(completeBytes, completeDocument.bytes)
    ) return Left(RequestNonCanonical)
    validateRequest(root, requestNode, requestDocument, completeNode, completeDocument)
  }

  def unchanged(
      request: PostgreSqlEnumerationLiveDataV1AdmittedRequest
  ): Boolean = {
    if (request == null) return false
    load(request.bundleRoot).exists { current =>
      current.requestSha256 == request.requestSha256 &&
      current.completeSha256 == request.completeSha256 &&
      Arrays.equals(current.requestDocument.bytes, request.requestDocument.bytes) &&
      Arrays.equals(current.completeDocument.bytes, request.completeDocument.bytes)
    }
  }

  private def validateRequest(
      root: Path,
      request: JsonNode,
      requestDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
      complete: JsonNode,
      completeDocument: PostgreSqlEnumerationCampaignV2JsonDocument
  ): Either[PostgreSqlEnumerationLiveDataV1ProtocolFailure, PostgreSqlEnumerationLiveDataV1AdmittedRequest] = {
    if (!closed(request, RootKeys) || !closed(complete, CompleteKeys))
      return Left(RequestContractMismatch)
    if (
      !textEquals(request, "schema_version", RequestSchema) ||
      !textEquals(request, "protocol_version", ProtocolVersion) ||
      !textEquals(request, "artifact_status", "private-expected-copy-load-request") ||
      !textEquals(request, "classification", "development") ||
      !claimsEqual(request.get("claims"), FalseClaims) ||
      !isSafeId(text(request, "attempt_id").orNull) ||
      !isSafeId(text(request, "workload_id").orNull)
    ) return Left(RequestContractMismatch)
    if (seal(request, "seal_sha256", RequestSealDomain) != text(request, "seal_sha256").orNull)
      return Left(RequestSealMismatch)

    val schema = text(request, "schema_name").filter(_.matches(Identifier))
      .getOrElse(return Left(RequestContractMismatch))
    val manifest = objectField(request, "dataset_manifest")
      .getOrElse(return Left(RequestContractMismatch))
    if (seal(manifest, "seal_sha256", DatasetSealDomain) != text(manifest, "seal_sha256").orNull)
      return Left(RequestSealMismatch)
    if (!text(manifest, "schema_name").contains(schema)) return Left(RequestContractMismatch)
    val manifestTables = arrayField(manifest, "tables").getOrElse(return Left(RequestContractMismatch))
    val requestTables = arrayField(request, "tables").getOrElse(return Left(RequestContractMismatch))
    if (requestTables.isEmpty || requestTables.size != manifestTables.size)
      return Left(RequestContractMismatch)

    val contract = objectField(request, "postgresql_contract")
      .getOrElse(return Left(RequestContractMismatch))
    if (!validPostgreSqlContract(contract, schema)) return Left(RequestContractMismatch)
    val copyDirectory = root.resolve("copy")
    val expectedCopyNames = requestTables.map(table => text(table, "table_name").orNull + ".copy")
    if (!exactPrivateDirectory(copyDirectory, expectedCopyNames)) return Left(BundleInvalid)

    var total = 0L
    val admitted = Vector.newBuilder[PostgreSqlEnumerationLiveDataV1AdmittedTable]
    requestTables.zip(manifestTables).foreach { case (table, manifestTable) =>
      if (!closed(table, TableKeys)) return Left(RequestContractMismatch)
      val name = text(table, "table_name").filter(_.matches(Identifier))
        .getOrElse(return Left(RequestContractMismatch))
      val withoutCopy = table.deepCopy[ObjectNode]()
      withoutCopy.remove("copy_file")
      if (canonicalDocument(withoutCopy) != canonicalDocument(manifestTable))
        return Left(RequestContractMismatch)
      val definition = objectField(table, "table_definition")
        .getOrElse(return Left(RequestContractMismatch))
      val columns = validateDefinition(definition, name).getOrElse(return Left(RequestContractMismatch))
      if (text(table, "table_definition_sha256").orNull != canonicalDocument(definition).sha256)
        return Left(RequestContractMismatch)
      if (!validExecutableSql(table, schema, name, columns))
        return Left(RequestContractMismatch)
      val contentNode = objectField(table, "content").getOrElse(return Left(RequestContractMismatch))
      val content = parseContent(contentNode).getOrElse(return Left(RequestContractMismatch))
      val copy = objectField(table, "copy_file").getOrElse(return Left(RequestContractMismatch))
      if (
        !closed(copy, Set("byte_length", "relative_path", "sha256")) ||
        integer(copy, "byte_length").forall(_ != BigInt(content.byteLength)) ||
        !text(copy, "relative_path").contains(s"copy/$name.copy") ||
        !text(copy, "sha256").contains(content.sha256) ||
        !privateSingleLinkFile(copyDirectory.resolve(name + ".copy"), content.byteLength) ||
        !hashCopyFile(copyDirectory.resolve(name + ".copy"), content)
      ) return Left(BundleInvalid)
      total = Math.addExact(total, content.byteLength)
      if (total > MaximumCopyBytesPerDataset) return Left(RequestContractMismatch)
      admitted += PostgreSqlEnumerationLiveDataV1AdmittedTable(
        PostgreSqlEnumerationLiveDataV1TableRequest(
          name,
          text(table, "table_definition_sha256").get,
          text(table, "create_table_sql").get,
          text(table, "copy_from_stdin_sql").get,
          text(table, "copy_to_stdout_sql").get,
          arrayField(table, "statistics_target_sql").get.map(_.textValue()),
          text(table, "analyze_sql").get,
          copyDirectory.resolve(name + ".copy"),
          content
        ),
        table
      )
    }
    val tables = admitted.result()
    if (
      integer(request, "total_copy_bytes").forall(_ != BigInt(total)) ||
      tables.map(_.request.tableName).distinct.size != requestTables.size
    ) return Left(RequestContractMismatch)
    val assemblyBinding = validBindings(request, requestDocument, manifest)
      .getOrElse(return Left(RequestContractMismatch))
    if (!validCompletion(request, requestDocument, complete, completeDocument, tables, total))
      return Left(RequestSealMismatch)
    val queries = PostgreSqlEnumerationLiveDataV1RawQueryContract.forRequest(schema, tables)
    val capture = PostgreSqlEnumerationLiveDataV1Request(
      schema,
      createSchemaSql(schema),
      tables.map(_.request),
      queries
    )
    if (!validateBundleRoot(root).contains(root) ||
      !exactPrivateDirectory(copyDirectory, expectedCopyNames)) return Left(RequestChanged)
    Right(new PostgreSqlEnumerationLiveDataV1AdmittedRequest(
      root,
      requestDocument,
      request,
      requestDocument.sha256,
      completeDocument,
      completeDocument.sha256,
      text(request, "workload_id").get,
      schema,
      total,
      tables,
      assemblyBinding,
      capture
    ))
  }

  private final case class Column(name: String, nullable: Boolean, ordinal: Int, dataType: String)

  private def validateDefinition(
      definition: JsonNode,
      tableName: String
  ): Option[Vector[Column]] = {
    if (!closed(definition, Set("columns", "occurrence_ids", "table_name")) ||
      !text(definition, "table_name").contains(tableName)) return None
    val columns = arrayField(definition, "columns").getOrElse(return None)
    if (columns.isEmpty) return None
    val parsed = columns.zipWithIndex.map { case (column, index) =>
      if (!closed(column, Set("name", "nullable", "ordinal", "postgresql_type"))) return None
      val name = text(column, "name").filter(_.matches(Identifier)).getOrElse(return None)
      val nullable = boolean(column, "nullable").getOrElse(return None)
      val ordinal = integer(column, "ordinal").filter(_ == BigInt(index)).map(_.toInt)
        .getOrElse(return None)
      val dataType = text(column, "postgresql_type")
        .filter(value => value == "BIGINT" || value == "SMALLINT").getOrElse(return None)
      Column(name, nullable, ordinal, dataType)
    }
    if (parsed.map(_.name).distinct.size != parsed.size) None else Some(parsed)
  }

  private def validExecutableSql(
      table: JsonNode,
      schema: String,
      name: String,
      columns: Vector[Column]
  ): Boolean = {
    val qualified = quote(schema) + "." + quote(name)
    val definitions = columns.map { column =>
      quote(column.name) + " " + column.dataType + (if (column.nullable) "" else " NOT NULL")
    }
    val names = columns.map(column => quote(column.name))
    val order = columns.map(column => quote(column.name) + (if (column.nullable) " NULLS FIRST" else ""))
    val expectedCreate = s"CREATE TABLE $qualified (${definitions.mkString(", ")})"
    val expectedFrom = s"COPY $qualified (${names.mkString(", ")}) FROM STDIN WITH " +
      "(FORMAT text, DELIMITER E'\\t', NULL E'\\\\N', ENCODING 'UTF8')"
    val expectedTo = s"COPY (SELECT ${names.mkString(", ")} FROM $qualified ORDER BY " +
      s"${order.mkString(", ")}) TO STDOUT WITH (FORMAT text, DELIMITER E'\\t', " +
      "NULL E'\\\\N', ENCODING 'UTF8')"
    val expectedStatistics = columns.map(column =>
      s"ALTER TABLE $qualified ALTER COLUMN ${quote(column.name)} SET STATISTICS 2000"
    )
    text(table, "create_table_sql").contains(expectedCreate) &&
      text(table, "copy_from_stdin_sql").contains(expectedFrom) &&
      text(table, "copy_to_stdout_sql").contains(expectedTo) &&
      text(table, "analyze_sql").contains("ANALYZE " + qualified) &&
      arrayField(table, "statistics_target_sql").exists(values =>
        values.forall(_.isTextual) && values.map(_.textValue()) == expectedStatistics
      )
  }

  private def parseContent(node: JsonNode): Option[PostgreSqlEnumerationLiveDataV1Content] = {
    if (!closed(node, Set("byte_length", "encoding", "row_count", "sha256"))) return None
    for {
      bytes <- integer(node, "byte_length").filter(value => value >= 0 && value <= 256L * 1024L * 1024L)
      rows <- integer(node, "row_count").filter(_ >= 0)
      digest <- text(node, "sha256").filter(_.matches(Sha256))
      encoding <- text(node, "encoding").filter(_ == PostgreSqlEnumerationLiveDataV1Capture.CopyEncoding)
      if bytes.isValidLong && rows.isValidLong
    } yield PostgreSqlEnumerationLiveDataV1Content(bytes.toLong, rows.toLong, digest, encoding)
  }

  private def validPostgreSqlContract(node: JsonNode, schema: String): Boolean = {
    if (!closed(node, Set("capture", "load"))) return false
    val capture = objectField(node, "capture").orNull
    val load = objectField(node, "load").orNull
    closed(capture, Set("client_encoding", "copy_to_stdout_bytes_retained", "date_style",
      "extra_float_digits", "lock_timeout_ms", "search_path", "server_encoding",
      "server_version_num", "statement_timeout_ms", "standard_conforming_strings",
      "time_zone", "transaction_isolation", "transaction_read_only")) &&
      textEquals(capture, "client_encoding", "UTF8") &&
      boolean(capture, "copy_to_stdout_bytes_retained").contains(true) &&
      textEquals(capture, "date_style", "ISO, MDY") &&
      textEquals(capture, "extra_float_digits", "3") &&
      textEquals(capture, "lock_timeout_ms", LockTimeoutMs) &&
      textEquals(capture, "search_path", "pg_catalog, pg_temp") &&
      textEquals(capture, "server_encoding", "UTF8") &&
      textEquals(capture, "server_version_num", "180004") &&
      textEquals(capture, "statement_timeout_ms", StatementTimeoutMs) &&
      textEquals(capture, "standard_conforming_strings", "on") &&
      textEquals(capture, "time_zone", "UTC") &&
      textEquals(capture, "transaction_isolation", "repeatable read") &&
      boolean(capture, "transaction_read_only").contains(true) &&
      closed(load, Set("create_schema_sql", "existing_schema_is_error", "load_transaction",
        "lock_timeout_ms", "search_path", "statement_timeout_ms", "transaction_isolation",
        "transaction_read_only")) &&
      text(load, "create_schema_sql").contains(createSchemaSql(schema)) &&
      boolean(load, "existing_schema_is_error").contains(true) &&
      textEquals(load, "load_transaction", "one-read-write-transaction-all-tables/v1") &&
      textEquals(load, "lock_timeout_ms", LockTimeoutMs) &&
      textEquals(load, "search_path", "pg_catalog, pg_temp") &&
      textEquals(load, "statement_timeout_ms", StatementTimeoutMs) &&
      textEquals(load, "transaction_isolation", "read committed") &&
      boolean(load, "transaction_read_only").contains(false)
  }

  private def validBindings(
      request: JsonNode,
      requestDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
      manifest: JsonNode
  ): Option[PostgreSqlEnumerationLiveDataV1AssemblyBinding] = {
    val sources = objectField(request, "source_bindings").orNull
    val toolchain = objectField(request, "toolchain").orNull
    if (!closed(sources, Set("acquisition_protocol_source", "data_semantics", "dataset_manifest",
      "live_data_protocol_source")) ||
      !closed(toolchain, Set("assembly", "cli_fqcn", "postgresql_jdbc_version"))) return None
    val dataSemantics = request.get("data_semantics")
    def binding(node: JsonNode, expected: Option[PostgreSqlEnumerationCampaignV2JsonDocument]): Boolean =
      closed(node, Set("byte_length", "sha256")) && integer(node, "byte_length").exists(_ >= 0) &&
        text(node, "sha256").exists(_.matches(Sha256)) && expected.forall(document =>
          integer(node, "byte_length").contains(BigInt(document.byteLength)) &&
            text(node, "sha256").contains(document.sha256)
        )
    val assembly = toolchain.get("assembly")
    val assemblyBinding = for {
      byteLength <- integer(assembly, "byte_length")
        .filter(length => length > 0 && length <= BigInt(MaximumScalaAssemblyBytes))
        .filter(_.isValidLong)
      sha256 <- text(assembly, "sha256").filter(_.matches(Sha256))
    } yield PostgreSqlEnumerationLiveDataV1AssemblyBinding(byteLength.longValue, sha256)
    val valid = binding(sources.get("data_semantics"), Some(canonicalDocument(dataSemantics))) &&
      binding(sources.get("dataset_manifest"), Some(canonicalDocument(manifest))) &&
      binding(sources.get("acquisition_protocol_source"), None) &&
      binding(sources.get("live_data_protocol_source"), None) &&
      binding(assembly, None) && assemblyBinding.nonEmpty &&
      text(toolchain, "cli_fqcn").contains(CliFqcn) &&
      text(toolchain, "postgresql_jdbc_version").contains(JdbcVersion) &&
      text(request, "load_contract_sha256").contains(canonicalDocument(manifest.get("load_contract")).sha256)
    if (valid) assemblyBinding else None
  }

  private def validCompletion(
      request: JsonNode,
      requestDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
      complete: JsonNode,
      completeDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
      tables: Vector[PostgreSqlEnumerationLiveDataV1AdmittedTable],
      total: Long
  ): Boolean = {
    if (
      !textEquals(complete, "artifact_status", "complete-private-expected-copy-request") ||
      !textEquals(complete, "schema_version", RequestCompleteSchema) ||
      !textEquals(complete, "protocol_version", ProtocolVersion) ||
      !claimsEqual(complete.get("claims"), FalseClaims) ||
      !text(complete, "workload_id").contains(text(request, "workload_id").orNull) ||
      integer(complete, "table_count").forall(_ != tables.size) ||
      integer(complete, "total_copy_bytes").forall(_ != BigInt(total)) ||
      !fileBindingEquals(complete.get("request"), requestDocument)
    ) return false
    val rows = tables.map { admitted =>
      obj(
        "copy_file" -> jsonValue(admitted.document.get("copy_file")),
        "table_name" -> string(admitted.request.tableName)
      )
    }
    val root = sha256(CopyRootDomain ++ document(array(rows)).bytes)
    text(complete, "copy_root_sha256").contains(root) &&
      text(complete, "seal_sha256").contains(seal(complete, "seal_sha256", CompleteSealDomain))
  }

  private def fileBindingEquals(node: JsonNode, documentValue: PostgreSqlEnumerationCampaignV2JsonDocument): Boolean =
    closed(node, Set("byte_length", "sha256")) &&
      integer(node, "byte_length").contains(BigInt(documentValue.byteLength)) &&
      text(node, "sha256").contains(documentValue.sha256)

  private def validateBundleRoot(bundle: Path): Option[Path] = {
    if (bundle == null || !bundle.isAbsolute || bundle != bundle.normalize()) return None
    if (!exactPrivateDirectory(bundle, Vector("complete.json", "copy", "request.json"))) return None
    val real = try bundle.toRealPath() catch { case NonFatal(_) => return None }
    if (real != bundle) None else Some(bundle)
  }

  private def exactPrivateDirectory(path: Path, names: Seq[String]): Boolean =
    try {
      if (!privateDirectory(path)) return false
      val observed = Using.resource(Files.list(path))(
        _.iterator().asScala.map(_.getFileName.toString).toVector.sorted
      )
      observed == names.sorted
    } catch { case NonFatal(_) => false }

  private def privateDirectory(path: Path): Boolean =
    Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) && !Files.isSymbolicLink(path) &&
      Files.getPosixFilePermissions(path) == DirectoryPermissions && unixUid(path) == CurrentUid

  private def privateSingleLinkFile(path: Path, expectedSize: Long): Boolean =
    try {
      val attributes = Files.readAttributes(path, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
      val links = Files.getAttribute(path, "unix:nlink", LinkOption.NOFOLLOW_LINKS)
        .asInstanceOf[java.lang.Number].longValue()
      attributes.isRegularFile && !attributes.isSymbolicLink && attributes.size() == expectedSize &&
        links == 1L && unixUid(path) == CurrentUid &&
        Files.getPosixFilePermissions(path) == FilePermissions
    } catch { case NonFatal(_) => false }

  private def readPrivateDocument(
      path: Path
  ): Either[PostgreSqlEnumerationLiveDataV1ProtocolFailure, Array[Byte]] = {
    val attributes = try Files.readAttributes(path, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
      catch { case NonFatal(_) => return Left(BundleInvalid) }
    if (attributes.size() > MaximumDocumentBytes) return Left(RequestTooLarge)
    if (attributes.size() <= 0L || !privateSingleLinkFile(path, attributes.size()))
      return Left(BundleInvalid)
    val bytes = try Files.readAllBytes(path) catch { case NonFatal(_) => return Left(BundleInvalid) }
    if (bytes.length != attributes.size() || !privateSingleLinkFile(path, attributes.size()))
      Left(BundleInvalid)
    else Right(bytes)
  }

  private def hashCopyFile(
      path: Path,
      expected: PostgreSqlEnumerationLiveDataV1Content
  ): Boolean = {
    var channel: FileChannel = null
    try {
      channel = FileChannel.open(path, StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)
      val digest = MessageDigest.getInstance("SHA-256")
      val buffer = ByteBuffer.allocate(64 * 1024)
      var total = 0L
      var rows = 0L
      var last = -1
      var zeroReads = 0
      var read = channel.read(buffer)
      while (read >= 0 && zeroReads < 16) {
        if (read == 0) zeroReads += 1
        else {
          zeroReads = 0
          total += read.toLong
          if (total > expected.byteLength) return false
          buffer.flip()
          val chunk = new Array[Byte](read)
          buffer.get(chunk)
          digest.update(chunk)
          chunk.foreach { byte =>
            if (byte == '\n'.toByte) rows += 1L
            last = byte & 0xff
          }
          buffer.clear()
        }
        read = channel.read(buffer)
      }
      read < 0 && total == expected.byteLength && rows == expected.rowCount &&
        last == '\n'.toInt &&
        digest.digest().map(byte => f"${byte & 0xff}%02x").mkString == expected.sha256 &&
        privateSingleLinkFile(path, expected.byteLength)
    } catch { case NonFatal(_) => false }
    finally if (channel != null) try channel.close() catch { case NonFatal(_) => () }
  }

  private def parseCanonical(bytes: Array[Byte]): Option[JsonNode] =
    try {
      if (bytes.isEmpty || bytes.last != '\n'.toByte || bytes.count(_ == '\n'.toByte) != 1) return None
      StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(bytes))
      val node = Mapper.readTree(bytes)
      if (node == null || !node.isObject || !structurallyBound(node)) None else Some(node)
    } catch { case NonFatal(_) => None }

  private def structurallyBound(root: JsonNode): Boolean = {
    var count = 0
    val stack = scala.collection.mutable.ArrayDeque[(JsonNode, Int)]((root, 1))
    while (stack.nonEmpty) {
      val (node, depth) = stack.removeLast(); count += 1
      if (count > 400000 || depth > 128) return false
      if (node.isNumber && !node.isIntegralNumber) return false
      if (node.isContainerNode) node.elements().asScala.foreach(child =>
        stack.append((child, if (child.isContainerNode) depth + 1 else depth)))
    }
    true
  }

  private[cli] def canonicalDocument(node: JsonNode): PostgreSqlEnumerationCampaignV2JsonDocument =
    document(jsonValue(node))
  private[cli] def jsonValue(node: JsonNode): Value = {
    if (node == null || node.isNull) NullValue
    else if (node.isTextual) string(node.textValue())
    else if (node.isBoolean) bool(node.booleanValue())
    else if (node.isIntegralNumber) number(BigInt(node.bigIntegerValue()))
    else if (node.isArray) array(node.elements().asScala.toVector.map(jsonValue))
    else if (node.isObject) obj(node.fields().asScala.toVector.map(field => field.getKey -> jsonValue(field.getValue)))
    else throw new IllegalArgumentException("non-integral JSON value")
  }
  private def seal(node: JsonNode, field: String, domain: Array[Byte]): String = {
    val unsigned = node.deepCopy[ObjectNode](); unsigned.remove(field)
    sha256(domain ++ canonicalDocument(unsigned).bytes)
  }
  private[cli] def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(byte => f"${byte & 0xff}%02x").mkString
  private def quote(value: String): String = "\"" + value + "\""
  private def createSchemaSql(schema: String): String = "CREATE SCHEMA " + quote(schema)
  private def unixUid(path: Path): Long = Files.getAttribute(path, "unix:uid", LinkOption.NOFOLLOW_LINKS)
    .asInstanceOf[java.lang.Number].longValue()
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
  private def boolean(node: JsonNode, name: String): Option[Boolean] =
    Option(node).flatMap(value => Option(value.get(name))).filter(_.isBoolean).map(_.booleanValue())
  private def integer(node: JsonNode, name: String): Option[BigInt] =
    Option(node).flatMap(value => Option(value.get(name))).filter(_.isIntegralNumber)
      .map(value => BigInt(value.bigIntegerValue()))
  private def isSafeId(value: String): Boolean = value != null && value.matches(SafeId)
  private def claimsEqual(node: JsonNode, claims: Map[String, Boolean]): Boolean =
    closed(node, claims.keySet) && claims.forall { case (key, expected) => boolean(node, key).contains(expected) }
}

/** Exact pg_catalog query registry for the raw projection adapter. */
private[cli] object PostgreSqlEnumerationLiveDataV1RawQueryContract {
  private val ProvenanceColumns = Vector(
    "client_encoding", "database_name", "date_style", "extra_float_digits",
    "lock_timeout_ms", "search_path", "server_encoding", "server_version_num",
    "standard_conforming_strings", "statement_timeout_ms", "time_zone",
    "transaction_isolation", "transaction_read_only"
  )

  def forRequest(
      schema: String,
      tables: Vector[PostgreSqlEnumerationLiveDataV1AdmittedTable]
  ): PostgreSqlEnumerationLiveDataV1RawQueries = {
    require(schema != null && schema.matches("[a-z][a-z0-9_]{0,62}"))
    require(tables != null && tables.nonEmpty)
    val schemaLiteral = literal(schema)
    val provenance = PostgreSqlEnumerationLiveDataV1RawQuery(
      "SELECT " +
        "pg_catalog.current_setting('client_encoding') AS client_encoding, " +
        "pg_catalog.current_database() AS database_name, " +
        "pg_catalog.current_setting('DateStyle') AS date_style, " +
        "pg_catalog.current_setting('extra_float_digits') AS extra_float_digits, " +
        "(SELECT setting FROM pg_catalog.pg_settings WHERE name = 'lock_timeout') AS lock_timeout_ms, " +
        "pg_catalog.current_setting('search_path') AS search_path, " +
        "pg_catalog.current_setting('server_encoding') AS server_encoding, " +
        "pg_catalog.current_setting('server_version_num') AS server_version_num, " +
        "pg_catalog.current_setting('standard_conforming_strings') AS standard_conforming_strings, " +
        "(SELECT setting FROM pg_catalog.pg_settings WHERE name = 'statement_timeout') AS statement_timeout_ms, " +
        "pg_catalog.current_setting('TimeZone') AS time_zone, " +
        "pg_catalog.current_setting('transaction_isolation') AS transaction_isolation, " +
        "pg_catalog.current_setting('transaction_read_only') AS transaction_read_only",
      ProvenanceColumns,
      1
    )
    val server = PostgreSqlEnumerationLiveDataV1RawQuery(
      "SELECT control.system_identifier::pg_catalog.text AS system_identifier " +
        "FROM pg_catalog.pg_control_system() AS control",
      Vector("system_identifier"),
      1
    )
    val database = PostgreSqlEnumerationLiveDataV1RawQuery(
      "SELECT database.datname AS database_name, database.oid::pg_catalog.text AS database_oid, " +
        "owner.rolname AS owner_name, database.datdba::pg_catalog.text AS owner_oid " +
        "FROM pg_catalog.pg_database AS database " +
        "JOIN pg_catalog.pg_roles AS owner ON owner.oid = database.datdba " +
        "WHERE database.datname = pg_catalog.current_database()",
      Vector("database_name", "database_oid", "owner_name", "owner_oid"),
      1
    )
    val snapshot = PostgreSqlEnumerationLiveDataV1RawQuery(
      "SELECT pg_catalog.pg_current_snapshot()::pg_catalog.text AS snapshot",
      Vector("snapshot"),
      1
    )
    val owner = PostgreSqlEnumerationLiveDataV1RawQuery(
      "SELECT owner.rolname AS owner_name, namespace.nspowner::pg_catalog.text AS owner_oid, " +
        "namespace.oid::pg_catalog.text AS schema_oid " +
        "FROM pg_catalog.pg_namespace AS namespace " +
        "JOIN pg_catalog.pg_roles AS owner ON owner.oid = namespace.nspowner " +
        s"WHERE namespace.nspname = $schemaLiteral",
      Vector("owner_name", "owner_oid", "schema_oid"),
      1
    )
    val relationNames = PostgreSqlEnumerationLiveDataV1RawQuery(
      "SELECT relation.relname AS name FROM pg_catalog.pg_class AS relation " +
        "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace " +
        s"WHERE namespace.nspname = $schemaLiteral " +
        "ORDER BY relation.relname COLLATE \"C\"",
      Vector("name"),
      math.max(1024, tables.size + 1)
    )
    val tableQueries = tables.map(table => queriesForTable(schema, table))
    PostgreSqlEnumerationLiveDataV1RawQueries(
      provenance,
      server,
      database,
      snapshot,
      relationNames,
      owner,
      relationNames,
      tableQueries
    )
  }

  private def queriesForTable(
      schema: String,
      admitted: PostgreSqlEnumerationLiveDataV1AdmittedTable
  ): PostgreSqlEnumerationLiveDataV1TableQueries = {
    val table = admitted.request.tableName
    val schemaLiteral = literal(schema)
    val tableLiteral = literal(table)
    val relationFrom =
      " FROM pg_catalog.pg_class AS relation " +
        "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace " +
        "LEFT JOIN pg_catalog.pg_am AS access_method ON access_method.oid = relation.relam " +
        s"WHERE namespace.nspname = $schemaLiteral AND relation.relname = $tableLiteral"
    val relation = PostgreSqlEnumerationLiveDataV1RawQuery(
      "SELECT access_method.amname AS access_method, " +
        "relation.relforcerowsecurity::pg_catalog.text AS force_row_security, " +
        "relation.relname AS name, relation.relowner::pg_catalog.text AS owner_oid, " +
        "relation.relpersistence::pg_catalog.text AS persistence, " +
        "relation.relkind::pg_catalog.text AS relkind, " +
        "relation.relreplident::pg_catalog.text AS replica_identity, " +
        "relation.relrowsecurity::pg_catalog.text AS row_security, " +
        "relation.reltablespace::pg_catalog.text AS tablespace_oid, " +
        "relation.reltuples::pg_catalog.text AS reltuples, " +
        "relation.relpages::pg_catalog.text AS relpages, " +
        "relation.relallvisible::pg_catalog.text AS relallvisible, " +
        "relation.relallfrozen::pg_catalog.text AS relallfrozen, " +
        "relation.relnatts::pg_catalog.text AS relnatts, " +
        "relation.relchecks::pg_catalog.text AS relchecks, " +
        "relation.relhasindex::pg_catalog.text AS relhasindex, " +
        "relation.relhasrules::pg_catalog.text AS relhasrules, " +
        "relation.relhastriggers::pg_catalog.text AS relhastriggers, " +
        "relation.relhassubclass::pg_catalog.text AS relhassubclass, " +
        "relation.relispartition::pg_catalog.text AS relispartition, " +
        "relation.reloptions::pg_catalog.text AS reloptions" + relationFrom,
      Vector(
        "access_method", "force_row_security", "name", "owner_oid", "persistence",
        "relkind", "replica_identity", "row_security", "tablespace_oid", "reltuples",
        "relpages", "relallvisible", "relallfrozen", "relnatts", "relchecks",
        "relhasindex", "relhasrules", "relhastriggers", "relhassubclass",
        "relispartition", "reloptions"
      ),
      1
    )
    val columns = PostgreSqlEnumerationLiveDataV1RawQuery(
      "SELECT attribute.attoptions::pg_catalog.text AS attribute_options, " +
        "attribute.attcollation::pg_catalog.text AS collation_oid, " +
        "attribute.attcompression::pg_catalog.text AS compression_method, " +
        "pg_catalog.pg_get_expr(default_value.adbin, default_value.adrelid) AS default_expression, " +
        "attribute.attgenerated::pg_catalog.text AS generated, " +
        "attribute.atthasmissing::pg_catalog.text AS has_missing_value, " +
        "attribute.attidentity::pg_catalog.text AS identity, " +
        "attribute.attinhcount::pg_catalog.text AS inheritance_count, " +
        "attribute.attislocal::pg_catalog.text AS is_local, " +
        "attribute.attmissingval::pg_catalog.text AS missing_value, " +
        "attribute.attname AS name, " +
        "(NOT attribute.attnotnull)::pg_catalog.text AS nullable, " +
        "attribute.attnum::pg_catalog.text AS ordinal, " +
        "pg_catalog.upper(pg_catalog.format_type(attribute.atttypid, attribute.atttypmod)) AS postgresql_type, " +
        "attribute.attstattarget::pg_catalog.text AS statistics_target, " +
        "attribute.attstorage::pg_catalog.text AS storage_strategy, " +
        "attribute.atttypid::pg_catalog.text AS type_oid " +
        "FROM pg_catalog.pg_attribute AS attribute " +
        "JOIN pg_catalog.pg_class AS relation ON relation.oid = attribute.attrelid " +
        "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace " +
        "LEFT JOIN pg_catalog.pg_attrdef AS default_value ON default_value.adrelid = attribute.attrelid " +
        "AND default_value.adnum = attribute.attnum " +
        s"WHERE namespace.nspname = $schemaLiteral AND relation.relname = $tableLiteral " +
        "AND attribute.attnum > 0 AND NOT attribute.attisdropped ORDER BY attribute.attnum",
      Vector(
        "attribute_options", "collation_oid", "compression_method", "default_expression",
        "generated", "has_missing_value", "identity", "inheritance_count", "is_local",
        "missing_value", "name", "nullable", "ordinal", "postgresql_type",
        "statistics_target", "storage_strategy", "type_oid"
      ),
      128
    )
    def definitions(sql: String): PostgreSqlEnumerationLiveDataV1RawQuery =
      PostgreSqlEnumerationLiveDataV1RawQuery(sql, Vector("definition"), 1024)
    val constraints = definitions(
      "SELECT pg_catalog.pg_get_constraintdef(constraint_row.oid, true) AS definition " +
        "FROM pg_catalog.pg_constraint AS constraint_row " +
        "JOIN pg_catalog.pg_class AS relation ON relation.oid = constraint_row.conrelid " +
        "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace " +
        s"WHERE namespace.nspname = $schemaLiteral AND relation.relname = $tableLiteral " +
        "AND constraint_row.contype <> 'n' ORDER BY constraint_row.oid"
    )
    val indexes = definitions(
      "SELECT pg_catalog.pg_get_indexdef(index_row.indexrelid) AS definition " +
        "FROM pg_catalog.pg_index AS index_row " +
        "JOIN pg_catalog.pg_class AS relation ON relation.oid = index_row.indrelid " +
        "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace " +
        s"WHERE namespace.nspname = $schemaLiteral AND relation.relname = $tableLiteral " +
        "ORDER BY index_row.indexrelid"
    )
    val inheritance = definitions(
      "SELECT child_namespace.nspname || '.' || child.relname || '->' || " +
        "parent_namespace.nspname || '.' || parent.relname || ':' || " +
        "inheritance.inhseqno::pg_catalog.text AS definition " +
        "FROM pg_catalog.pg_inherits AS inheritance " +
        "JOIN pg_catalog.pg_class AS child ON child.oid = inheritance.inhrelid " +
        "JOIN pg_catalog.pg_namespace AS child_namespace ON child_namespace.oid = child.relnamespace " +
        "JOIN pg_catalog.pg_class AS parent ON parent.oid = inheritance.inhparent " +
        "JOIN pg_catalog.pg_namespace AS parent_namespace ON parent_namespace.oid = parent.relnamespace " +
        s"WHERE (child_namespace.nspname = $schemaLiteral AND child.relname = $tableLiteral) " +
        s"OR (parent_namespace.nspname = $schemaLiteral AND parent.relname = $tableLiteral) " +
        "ORDER BY child_namespace.nspname COLLATE \"C\", child.relname COLLATE \"C\", " +
        "parent_namespace.nspname COLLATE \"C\", parent.relname COLLATE \"C\", " +
        "inheritance.inhseqno"
    )
    val policies = definitions(
      "SELECT policy.polname AS definition FROM pg_catalog.pg_policy AS policy " +
        "JOIN pg_catalog.pg_class AS relation ON relation.oid = policy.polrelid " +
        "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace " +
        s"WHERE namespace.nspname = $schemaLiteral AND relation.relname = $tableLiteral " +
        "ORDER BY policy.polname COLLATE \"C\", policy.oid"
    )
    val rules = definitions(
      "SELECT pg_catalog.pg_get_ruledef(rule.oid, true) AS definition " +
        "FROM pg_catalog.pg_rewrite AS rule " +
        "JOIN pg_catalog.pg_class AS relation ON relation.oid = rule.ev_class " +
        "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace " +
        s"WHERE namespace.nspname = $schemaLiteral AND relation.relname = $tableLiteral " +
        "ORDER BY rule.rulename COLLATE \"C\", rule.oid"
    )
    val triggers = definitions(
      "SELECT pg_catalog.pg_get_triggerdef(trigger_row.oid, true) AS definition " +
        "FROM pg_catalog.pg_trigger AS trigger_row " +
        "JOIN pg_catalog.pg_class AS relation ON relation.oid = trigger_row.tgrelid " +
        "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace " +
        s"WHERE namespace.nspname = $schemaLiteral AND relation.relname = $tableLiteral " +
        "AND NOT trigger_row.tgisinternal ORDER BY trigger_row.tgname COLLATE \"C\", trigger_row.oid"
    )
    val statsFields = Vector(
      "avg_width", "correlation", "elem_count_histogram", "histogram_bounds",
      "most_common_elem_freqs", "most_common_elems", "most_common_freqs",
      "most_common_vals", "n_distinct", "null_frac", "range_bounds_histogram",
      "range_empty_frac", "range_length_histogram"
    )
    val stats = PostgreSqlEnumerationLiveDataV1RawQuery(
      "SELECT statistics.attname AS column_name, statistics.inherited::pg_catalog.text AS inherited, " +
        statsFields.map(field => s"statistics.$field::pg_catalog.text AS $field").mkString(", ") + " " +
        "FROM pg_catalog.pg_stats AS statistics " +
        "JOIN pg_catalog.pg_attribute AS attribute ON attribute.attname = statistics.attname " +
        "JOIN pg_catalog.pg_class AS relation ON relation.oid = attribute.attrelid " +
        "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace " +
        s"WHERE statistics.schemaname = $schemaLiteral AND statistics.tablename = $tableLiteral " +
        "AND namespace.nspname = statistics.schemaname AND relation.relname = statistics.tablename " +
        "AND attribute.attnum > 0 AND NOT attribute.attisdropped ORDER BY attribute.attnum",
      Vector("column_name", "inherited") ++ statsFields,
      128
    )
    val extended = definitions(
      "SELECT statistics_namespace.nspname || '.' || statistics.stxname || ':' || " +
        "pg_catalog.pg_get_statisticsobjdef(statistics.oid) AS definition " +
        "FROM pg_catalog.pg_statistic_ext AS statistics " +
        "JOIN pg_catalog.pg_namespace AS statistics_namespace " +
        "ON statistics_namespace.oid = statistics.stxnamespace " +
        "WHERE statistics.stxrelid = " +
        "(SELECT relation.oid FROM pg_catalog.pg_class AS relation " +
        "JOIN pg_catalog.pg_namespace AS relation_namespace " +
        "ON relation_namespace.oid = relation.relnamespace " +
        s"WHERE relation_namespace.nspname = $schemaLiteral AND relation.relname = $tableLiteral) " +
        "ORDER BY statistics_namespace.nspname COLLATE \"C\", " +
        "statistics.stxname COLLATE \"C\", statistics.oid"
    )
    PostgreSqlEnumerationLiveDataV1TableQueries(
      table,
      relation,
      columns,
      constraints,
      indexes,
      inheritance,
      policies,
      rules,
      triggers,
      stats,
      extended
    )
  }

  private def literal(value: String): String = "'" + value + "'"
}

private[cli] final case class PostgreSqlEnumerationLiveDataV1RawDocuments(
    assembly: PostgreSqlEnumerationCampaignV2JsonDocument,
    complete: PostgreSqlEnumerationCampaignV2JsonDocument,
    tableNames: Vector[String]
)

/** Exact serializer for Python RAW_ASSEMBLY_KEYS and RAW_COMPLETE_KEYS. */
private[cli] object PostgreSqlEnumerationLiveDataV1Assembly {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._
  import PostgreSqlEnumerationLiveDataV1ProtocolFailure._

  private val RawAssemblySchema = "yrw-enumeration-live-raw-assembly/v1"
  private val RawCompleteSchema = "yrw-enumeration-live-raw-assembly-complete/v1"
  private val ProtocolVersion =
    PostgreSqlEnumerationLiveDataV1RequestParser.ProtocolVersion
  private val AssemblySealDomain =
    "yrw-enumeration-live-raw-assembly-seal/v1\u0000".getBytes(StandardCharsets.UTF_8)
  private val CopyRootDomain =
    "yrw-enumeration-live-raw-copy-root/v1\u0000".getBytes(StandardCharsets.UTF_8)
  private val CompleteSealDomain =
    "yrw-enumeration-live-raw-assembly-complete-seal/v1\u0000"
      .getBytes(StandardCharsets.UTF_8)

  def render(
      request: PostgreSqlEnumerationLiveDataV1AdmittedRequest,
      batch: PostgreSqlEnumerationLiveDataV1Batch
  ): Either[
    PostgreSqlEnumerationLiveDataV1ProtocolFailure,
    PostgreSqlEnumerationLiveDataV1RawDocuments
  ] = {
    if (request == null || batch == null) return Left(CaptureMismatch)
    try {
      val tableNames = request.tables.map(_.request.tableName)
      if (
        batch.load.tables.map(_.tableName) != tableNames ||
        batch.capture.tables.map(_.tableName) != tableNames ||
        !batch.load.allTablesOneTransaction || !batch.load.committed ||
        batch.load.existingSchemaObserved || !batch.capture.sameSession ||
        batch.capture.transactionIsolation != "repeatable read" ||
        !batch.capture.transactionReadOnly
      ) return Left(CaptureMismatch)

      val loadPostgreSql = loadPostgreSqlValue(batch.load.finalPostgreSql)
        .getOrElse(return Left(CaptureMismatch))
      if (batch.load.initialPostgreSql != batch.load.finalPostgreSql)
        return Left(CaptureMismatch)
      val loadTables = batch.load.tables.zip(request.tables).map { case (observed, expected) =>
        if (!copyMatches(observed.copyIn, expected.request.content) ||
          observed.statisticsTargetCommandCount != expected.request.statisticsTargetSql.size ||
          !observed.analyzeExecuted) return Left(CaptureMismatch)
        obj(
          "analyze_executed" -> bool(true),
          "copy_in" -> content(expected.request.content),
          "statistics_target_command_count" -> number(BigInt(observed.statisticsTargetCommandCount)),
          "table_name" -> string(observed.tableName)
        )
      }
      val load = obj(
        "all_tables_one_transaction" -> bool(true),
        "committed" -> bool(true),
        "existing_schema_observed" -> bool(false),
        "postgresql" -> loadPostgreSql,
        "schema_name" -> string(request.schemaName),
        "tables" -> array(loadTables)
      )

      val initial = provenance(batch.capture.initialProvenance)
        .getOrElse(return Left(CaptureMismatch))
      val finalValue = provenance(batch.capture.finalProvenance)
        .getOrElse(return Left(CaptureMismatch))
      if (batch.capture.initialProvenance != batch.capture.finalProvenance)
        return Left(CaptureMismatch)
      val server = exactObject(batch.capture.serverIdentity, Vector("system_identifier"))
        .getOrElse(return Left(CaptureMismatch))
      val database = exactObject(
        batch.capture.databaseIdentity,
        Vector("database_name", "database_oid", "owner_name", "owner_oid")
      ).getOrElse(return Left(CaptureMismatch))
      val snapshot = exactObject(batch.capture.snapshotIdentity, Vector("snapshot"))
        .getOrElse(return Left(CaptureMismatch))
      val owner = exactObject(
        batch.capture.schemaOwner,
        Vector("owner_name", "owner_oid", "schema_oid")
      ).getOrElse(return Left(CaptureMismatch))
      val observedRelationNames = oneColumn(batch.capture.relationSet, "name")
        .getOrElse(return Left(CaptureMismatch))
      if (observedRelationNames != tableNames) return Left(CaptureMismatch)

      val captureTables = batch.capture.tables.zip(request.tables).map {
        case (observed, expected) =>
          val relation = relationValue(observed.relation, expected.request)
            .getOrElse(return Left(CaptureMismatch))
          val columns = columnValues(observed.columns)
            .getOrElse(return Left(CaptureMismatch))
          val constraints = definitions(observed.constraintDefinitions)
            .getOrElse(return Left(CaptureMismatch))
          val indexes = definitions(observed.indexDefinitions)
            .getOrElse(return Left(CaptureMismatch))
          val inheritance = definitions(observed.inheritanceDefinitions)
            .getOrElse(return Left(CaptureMismatch))
          val policies = definitions(observed.policyDefinitions)
            .getOrElse(return Left(CaptureMismatch))
          val rules = definitions(observed.ruleDefinitions)
            .getOrElse(return Left(CaptureMismatch))
          val triggers = definitions(observed.triggerDefinitions)
            .getOrElse(return Left(CaptureMismatch))
          val statistics = statisticsValue(
            observed.plannerStatisticsProjection,
            observed.extendedStatisticsDefinitions
          ).getOrElse(return Left(CaptureMismatch))
          if (!copyMatches(observed.copyOut, expected.request.content) ||
            !observed.copyOut.relativePath.contains(s"copy-out/${observed.tableName}.copy"))
            return Left(CaptureMismatch)
          val catalog = obj(
            "columns" -> array(columns),
            "index_definitions" -> array(indexes.map(string)),
            "bidirectional_inheritance_definitions" -> array(inheritance.map(string)),
            "non_not_null_constraint_definitions" -> array(constraints.map(string)),
            "policy_definitions" -> array(policies.map(string)),
            "relation" -> relation,
            "rule_definitions" -> array(rules.map(string)),
            "trigger_definitions" -> array(triggers.map(string))
          )
          obj(
            "analyze_observed" -> bool(true),
            "catalog_projection" -> catalog,
            "content" -> content(expected.request.content),
            "copy_out_file" -> obj(
              "byte_length" -> number(BigInt(observed.copyOut.byteLength)),
              "relative_path" -> string(observed.copyOut.relativePath.get),
              "sha256" -> string(observed.copyOut.sha256)
            ),
            "planner_statistics_projection" -> statistics,
            "table_definition_sha256" -> string(expected.request.tableDefinitionSha256),
            "table_name" -> string(expected.request.tableName)
          ) -> relation
      }
      val schemaCatalog = obj(
        "all_pg_class_relation_names" -> array(observedRelationNames.map(string)),
        "relation_set_scope" -> string("all-pg-class-kinds-in-schema"),
        "relations" -> array(captureTables.map(_._2)),
        "schema_name" -> string(request.schemaName)
      )
      val schemaObservation = obj(
        "exact_relation_set" -> bool(true),
        "schema_catalog" -> schemaCatalog,
        "schema_name" -> string(request.schemaName),
        "schema_owner" -> owner,
        "tables" -> array(captureTables.map(_._1))
      )
      val capture = obj(
        "database_identity" -> database,
        "final_provenance" -> finalValue,
        "initial_provenance" -> initial,
        "same_session" -> bool(true),
        "schema_observation" -> schemaObservation,
        "server_identity" -> server,
        "snapshot_identity" -> snapshot,
        "transaction" -> obj(
          "access_mode" -> string("read-only"),
          "isolation_level" -> string("repeatable-read"),
          "search_path" -> string("pg_catalog, pg_temp")
        )
      )
      val claims = rawClaims
      val runtime = obj(
        "java_runtime_version" -> string(nonemptySystemProperty("java.runtime.version")),
        "java_vm_name" -> string(nonemptySystemProperty("java.vm.name")),
        "java_vm_version" -> string(nonemptySystemProperty("java.vm.version")),
        "postgresql_jdbc_version" -> string("42.7.2")
      )
      val unsignedFields = Vector(
        "artifact_status" -> string("scala-jdbc-raw-postgresql-load-capture"),
        "capture" -> capture,
        "claims" -> claims,
        "load" -> load,
        "load_request" -> embeddedDocument(request.requestDocument),
        "load_request_sha256" -> string(request.requestSha256),
        "protocol_version" -> string(ProtocolVersion),
        "request_bundle_complete_sha256" -> string(request.completeSha256),
        "runtime" -> runtime,
        "schema_version" -> string(RawAssemblySchema)
      )
      val assemblySeal = digest(AssemblySealDomain, obj(unsignedFields))
      val assembly = document(obj(unsignedFields :+ "assembly_seal_sha256" -> string(assemblySeal)))
      val rootRows = batch.capture.tables.zip(request.tables).map { case (observed, expected) =>
        obj(
          "content" -> content(expected.request.content),
          "copy_out_file" -> obj(
            "byte_length" -> number(BigInt(observed.copyOut.byteLength)),
            "relative_path" -> string(observed.copyOut.relativePath.get),
            "sha256" -> string(observed.copyOut.sha256)
          ),
          "table_name" -> string(observed.tableName)
        )
      }
      val copyRoot = digest(CopyRootDomain, array(rootRows))
      val completeUnsigned = Vector(
        "artifact_status" -> string("complete-private-raw-postgresql-load-capture"),
        "assembly" -> obj(
          "byte_length" -> number(BigInt(assembly.byteLength)),
          "sha256" -> string(assembly.sha256)
        ),
        "claims" -> claims,
        "copy_out_root_sha256" -> string(copyRoot),
        "protocol_version" -> string(ProtocolVersion),
        "request_bundle_complete_sha256" -> string(request.completeSha256),
        "schema_version" -> string(RawCompleteSchema),
        "table_count" -> number(BigInt(tableNames.size)),
        "total_copy_bytes" -> number(BigInt(request.totalCopyBytes)),
        "workload_id" -> string(request.workloadId)
      )
      val completeSeal = digest(CompleteSealDomain, obj(completeUnsigned))
      val complete = document(obj(completeUnsigned :+ "seal_sha256" -> string(completeSeal)))
      Right(PostgreSqlEnumerationLiveDataV1RawDocuments(assembly, complete, tableNames))
    } catch {
      case problem: IllegalArgumentException
          if Option(problem.getMessage).exists(_.contains("exceeds")) =>
        Left(AssemblyTooLarge)
      case NonFatal(_) => Left(AssemblyFailed)
    }
  }

  private def rawClaims: Value = obj(
    "coherent_atomic_snapshot" -> bool(false),
    "external_durable_chronology" -> bool(false),
    "independent_live_postgresql_observation" -> bool(false),
    "live_loaded_state_truth" -> bool(false),
    "no_unrecorded_attempts" -> bool(false),
    "opaque_pg_stats_arrays_are_not_parsed_feature_evidence" -> bool(true),
    "outcomes_in_scope" -> bool(false),
    "publication_history" -> bool(false),
    "production_policy" -> bool(false),
    "raw_copy_catalog_and_statistics_projection_retained" -> bool(true),
    "reportable" -> bool(false),
    "validation_is_conditional_on_pinned_scala_capture" -> bool(true)
  )

  private def content(value: PostgreSqlEnumerationLiveDataV1Content): Value = obj(
    "byte_length" -> number(BigInt(value.byteLength)),
    "encoding" -> string(value.encoding),
    "row_count" -> number(BigInt(value.rowCount)),
    "sha256" -> string(value.sha256)
  )

  private def loadPostgreSqlValue(
      result: PostgreSqlEnumerationLiveDataV1RawResult
  ): Option[Value] =
    single(result).flatMap { row =>
      val expected = Map(
        "client_encoding" -> "UTF8",
        "lock_timeout_ms" -> PostgreSqlEnumerationLiveDataV1RequestParser.LockTimeoutMs,
        "search_path" -> "pg_catalog, pg_temp",
        "server_encoding" -> "UTF8",
        "server_version_num" -> "180004",
        "statement_timeout_ms" -> PostgreSqlEnumerationLiveDataV1RequestParser.StatementTimeoutMs,
        "transaction_isolation" -> "read committed",
        "transaction_read_only" -> "off"
      )
      if (!expected.forall { case (key, value) => row.get(key).flatten.contains(value) }) None
      else for {
        database <- row.get("database_name").flatten.filter(_.nonEmpty)
        system <- row.get("system_identifier").flatten.filter(_.matches("0|[1-9][0-9]*"))
      } yield obj(
        "client_encoding" -> string("UTF8"),
        "database_name" -> string(database),
        "lock_timeout_ms" -> string(PostgreSqlEnumerationLiveDataV1RequestParser.LockTimeoutMs),
        "observation_scope" -> string("load-session-before-commit"),
        "search_path" -> string("pg_catalog, pg_temp"),
        "server_encoding" -> string("UTF8"),
        "server_version_num" -> string("180004"),
        "statement_timeout_ms" -> string(PostgreSqlEnumerationLiveDataV1RequestParser.StatementTimeoutMs),
        "system_identifier" -> string(system),
        "transaction_isolation" -> string("read committed"),
        "transaction_read_only" -> bool(false)
      )
    }

  private def provenance(result: PostgreSqlEnumerationLiveDataV1RawResult): Option[Value] =
    single(result).flatMap { row =>
      val expected = Map(
        "client_encoding" -> "UTF8",
        "date_style" -> "ISO, MDY",
        "extra_float_digits" -> "3",
        "lock_timeout_ms" -> PostgreSqlEnumerationLiveDataV1RequestParser.LockTimeoutMs,
        "search_path" -> "pg_catalog, pg_temp",
        "server_encoding" -> "UTF8",
        "server_version_num" -> "180004",
        "standard_conforming_strings" -> "on",
        "statement_timeout_ms" -> PostgreSqlEnumerationLiveDataV1RequestParser.StatementTimeoutMs,
        "time_zone" -> "UTC",
        "transaction_isolation" -> "repeatable read",
        "transaction_read_only" -> "on"
      )
      if (!expected.forall { case (key, value) => row.get(key).flatten.contains(value) }) None
      else row.get("database_name").flatten.filter(_.nonEmpty).map { database =>
        obj(
          "client_encoding" -> string("UTF8"),
          "database_name" -> string(database),
          "date_style" -> string("ISO, MDY"),
          "extra_float_digits" -> string("3"),
          "lock_timeout_ms" -> string(PostgreSqlEnumerationLiveDataV1RequestParser.LockTimeoutMs),
          "search_path" -> string("pg_catalog, pg_temp"),
          "server_encoding" -> string("UTF8"),
          "server_version_num" -> string("180004"),
          "standard_conforming_strings" -> string("on"),
          "statement_timeout_ms" -> string(PostgreSqlEnumerationLiveDataV1RequestParser.StatementTimeoutMs),
          "time_zone" -> string("UTC"),
          "transaction_isolation" -> string("repeatable read"),
          "transaction_read_only" -> bool(true)
        )
      }
    }

  private def exactObject(
      result: PostgreSqlEnumerationLiveDataV1RawResult,
      columns: Vector[String]
  ): Option[Value] =
    if (result.columns != columns) None
    else single(result).flatMap { row =>
      val fields = columns.map(column => row.get(column).flatten.map(value => column -> string(value)))
      if (fields.exists(_.isEmpty)) None else Some(obj(fields.flatten))
    }

  private def relationValue(
      result: PostgreSqlEnumerationLiveDataV1RawResult,
      request: PostgreSqlEnumerationLiveDataV1TableRequest
  ): Option[Value] =
    single(result).flatMap { row =>
      val booleans = Vector(
        "force_row_security", "relhasindex", "relhasrules", "relhastriggers",
        "relhassubclass", "relispartition", "row_security"
      )
      val texts = Vector(
        "access_method", "name", "owner_oid", "persistence", "relallfrozen",
        "relallvisible", "relchecks", "relkind", "relnatts", "relpages",
        "replica_identity", "reltuples", "tablespace_oid"
      )
      val boolValues = booleans.map(key => row.get(key).flatten.flatMap(parseBoolean).map(key -> _))
      val textValues = texts.map(key => row.get(key).flatten.map(key -> _))
      val reloptions = row.get("reloptions")
      if (boolValues.exists(_.isEmpty) || textValues.exists(_.isEmpty) || reloptions.isEmpty ||
        !row.get("name").flatten.contains(request.tableName)) None
      else {
        val fields = (
          textValues.flatten.map { case (key, value) => key -> string(value) } ++
            boolValues.flatten.map { case (key, value) => key -> bool(value) }
        ) :+ ("reloptions" -> reloptions.flatten.fold[Value](NullValue)(string))
        Some(obj(fields))
      }
    }

  private def columnValues(
      result: PostgreSqlEnumerationLiveDataV1RawResult
  ): Option[Vector[Value]] = {
    val expected = Vector(
      "attribute_options", "collation_oid", "compression_method", "default_expression",
      "generated", "has_missing_value", "identity", "inheritance_count", "is_local",
      "missing_value", "name", "nullable", "ordinal", "postgresql_type",
      "statistics_target", "storage_strategy", "type_oid"
    )
    if (result.columns != expected) return None
    val rows = result.rows.map { values =>
      val row = expected.zip(values).toMap
      for {
        collationOid <- row("collation_oid")
        compression <- row("compression_method")
        generated <- row("generated")
        hasMissingText <- row("has_missing_value")
        hasMissing <- parseBoolean(hasMissingText)
        identity <- row("identity")
        inheritanceCount <- row("inheritance_count")
        isLocalText <- row("is_local")
        isLocal <- parseBoolean(isLocalText)
        name <- row("name")
        nullableText <- row("nullable")
        nullable <- parseBoolean(nullableText)
        ordinal <- row("ordinal")
        dataType <- row("postgresql_type")
        statistics <- row("statistics_target")
        storage <- row("storage_strategy")
        typeOid <- row("type_oid")
      } yield obj(
        "attribute_options" -> row("attribute_options").fold[Value](NullValue)(string),
        "collation_oid" -> string(collationOid),
        "compression_method" -> string(compression),
        "default_expression" -> row("default_expression").fold[Value](NullValue)(string),
        "generated" -> string(generated),
        "has_missing_value" -> bool(hasMissing),
        "identity" -> string(identity),
        "inheritance_count" -> string(inheritanceCount),
        "is_local" -> bool(isLocal),
        "missing_value" -> row("missing_value").fold[Value](NullValue)(string),
        "name" -> string(name),
        "nullable" -> bool(nullable),
        "ordinal" -> string(ordinal),
        "postgresql_type" -> string(dataType),
        "statistics_target" -> string(statistics),
        "storage_strategy" -> string(storage),
        "type_oid" -> string(typeOid)
      )
    }
    if (rows.exists(_.isEmpty)) None else Some(rows.flatten)
  }

  private def statisticsValue(
      result: PostgreSqlEnumerationLiveDataV1RawResult,
      extended: PostgreSqlEnumerationLiveDataV1RawResult
  ): Option[Value] = {
    val fields = Vector(
      "avg_width", "correlation", "elem_count_histogram", "histogram_bounds",
      "most_common_elem_freqs", "most_common_elems", "most_common_freqs",
      "most_common_vals", "n_distinct", "null_frac", "range_bounds_histogram",
      "range_empty_frac", "range_length_histogram"
    )
    val expected = Vector("column_name", "inherited") ++ fields
    if (result.columns != expected) return None
    val rows = result.rows.map { values =>
      val row = expected.zip(values).toMap
      for {
        name <- row("column_name")
        inheritedText <- row("inherited")
        inherited <- parseBoolean(inheritedText)
      } yield obj(
        "column_name" -> string(name),
        "fields" -> obj(fields.map(field =>
          field -> row(field).fold[Value](NullValue)(string)
        )),
        "inherited" -> bool(inherited)
      )
    }
    if (rows.exists(_.isEmpty)) None
    else definitions(extended).map(values => obj(
      "columns" -> array(rows.flatten),
      "extended_statistics_definitions" -> array(values.map(string))
    ))
  }

  private def definitions(
      result: PostgreSqlEnumerationLiveDataV1RawResult
  ): Option[Vector[String]] =
    if (result.columns != Vector("definition")) None
    else {
      val values = result.rows.map(_.headOption.flatten)
      if (values.exists(_.isEmpty)) None else Some(values.flatten)
    }

  private def oneColumn(
      result: PostgreSqlEnumerationLiveDataV1RawResult,
      name: String
  ): Option[Vector[String]] =
    if (result.columns != Vector(name)) None
    else {
      val values = result.rows.map(_.headOption.flatten)
      if (values.exists(_.isEmpty)) None else Some(values.flatten)
    }

  private def single(
      result: PostgreSqlEnumerationLiveDataV1RawResult
  ): Option[Map[String, Option[String]]] =
    if (result == null || result.rows.size != 1) None
    else Some(result.columns.zip(result.rows.head).toMap)

  private def parseBoolean(value: String): Option[Boolean] = value match {
    case "true"  => Some(true)
    case "false" => Some(false)
    case _       => None
  }

  private def copyMatches(
      observed: PostgreSqlEnumerationLiveDataV1CopyObservation,
      expected: PostgreSqlEnumerationLiveDataV1Content
  ): Boolean =
    observed != null && observed.byteLength == expected.byteLength &&
      observed.rowCount == expected.rowCount && observed.sha256 == expected.sha256

  private def nonemptySystemProperty(name: String): String = {
    val value = System.getProperty(name)
    if (
      value == null || value.isEmpty || value.indexOf('\u0000') >= 0 ||
      value.getBytes(StandardCharsets.UTF_8).length > 4096
    ) throw new IllegalStateException("runtime property absent or invalid")
    value
  }

  private def digest(domain: Array[Byte], value: Value): String =
    PostgreSqlEnumerationLiveDataV1RequestParser.sha256(domain ++ document(value).bytes)
}
