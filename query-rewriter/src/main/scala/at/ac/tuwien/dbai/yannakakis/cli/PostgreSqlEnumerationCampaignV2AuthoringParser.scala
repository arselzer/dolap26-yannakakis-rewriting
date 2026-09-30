package at.ac.tuwien.dbai.yannakakis.cli

import java.io.{ByteArrayOutputStream, IOException, InputStream}
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.security.MessageDigest

import scala.jdk.CollectionConverters._

import com.fasterxml.jackson.core.exc.StreamConstraintsException
import com.fasterxml.jackson.core.{JsonFactory, JsonParseException, JsonParser}
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{DeserializationFeature, JsonNode, ObjectMapper}

/** Stable, non-diagnostic failures from the bounded authoring-request loader. */
private[yannakakis] sealed trait PostgreSqlEnumerationCampaignV2AuthoringParseFailure
    extends Product
    with Serializable {
  def code: String
}

private[yannakakis] object PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
  case object MissingInput
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.missing-input"
  }

  case object InputReadFailed
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.input-read-failed"
  }

  case object DocumentTooLarge
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.document-too-large"
  }

  case object InvalidCanonicalTerminator
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String =
      "authoring-request.invalid-canonical-terminator"
  }

  case object InvalidUtf8
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.invalid-utf8"
  }

  case object ExcessiveJsonDepth
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.json-depth"
  }

  case object DuplicateJsonKey
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.duplicate-key"
  }

  case object InvalidJson
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.invalid-json"
  }

  case object InvalidShape
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.invalid-shape"
  }

  case object InvalidSourceSql
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.invalid-source-sql"
  }

  case object FrozenContractMismatch
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.frozen-contract-mismatch"
  }

  case object InvalidContent
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.invalid-content"
  }

  case object NonCanonicalDocument
      extends PostgreSqlEnumerationCampaignV2AuthoringParseFailure {
    override val code: String = "authoring-request.noncanonical-document"
  }
}

/**
  * Strict parser for the sole canonical campaign-v2 authoring request.
  *
  * Parsed JSON is never trusted as a request object. Only the primitive caller
  * commitments are extracted; the frozen request is rebuilt through
  * [[PostgreSqlEnumerationCampaignV2AuthoringRequest.create]], then its exact
  * canonical bytes (including the final LF) must equal the immutable input
  * snapshot.
  */
private[yannakakis] object PostgreSqlEnumerationCampaignV2AuthoringParser {
  import PostgreSqlEnumerationCampaignV2AuthoringParseFailure._

  private val MaximumDocumentBytes: Int =
    PostgreSqlEnumerationCampaignV2Json.MaximumCanonicalJsonBytes
  private val MaximumSourceSqlBytes: Int = 16 * 1024 * 1024
  private val MaximumJsonDepth: Int = 128
  private val ReadBufferBytes: Int = 8192

  private val RootKeys = Set(
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
  private val SourceSqlKeys = Set(
    "byte_length",
    "encoding",
    "schema_version",
    "sha256",
    "utf8_hex"
  )
  private val DataContractKeys = Set(
    "data_semantics_sha256",
    "dataset_id",
    "dataset_manifest_sha256",
    "load_receipt_sha256",
    "schema_version"
  )
  private val SettingsKeys = Set(
    "feature_capture",
    "fixed_physical_config",
    "postgres_session",
    "schema_version",
    "search"
  )
  private val SearchKeys = Set(
    "admissibility",
    "algorithm",
    "requested_k",
    "require_connected_cover",
    "width_bound"
  )
  private val FeatureCaptureKeys = Set(
    "analyze",
    "failure_policy",
    "include_catalog",
    "include_whole_plan",
    "same_session"
  )
  private val FixedPhysicalKeys = Set(
    "cte_materialization",
    "cte_materialization_scope",
    "physical_variant_id_scheme",
    "reduction_mode",
    "reduction_order",
    "sql_shape"
  )
  private val ToolchainKeys = Set(
    "components",
    "producer_entrypoint",
    "schema_version"
  )
  private val ToolchainComponentKeys = Set(
    "artifact_sha256",
    "name",
    "version"
  )
  private val SessionSettingKeys = Set("name", "value")

  private val FrozenSessionSettings = Vector(
    "datestyle" -> "ISO, MDY",
    "extra_float_digits" -> "3",
    "standard_conforming_strings" -> "on",
    "timezone" -> "UTC"
  )

  private val Mapper: ObjectMapper = {
    val factory = new JsonFactory()
    factory.setStreamReadConstraints(
      com.fasterxml.jackson.core.StreamReadConstraints
        .builder()
        .maxNestingDepth(MaximumJsonDepth)
        .maxNumberLength(64)
        .maxStringLength(MaximumDocumentBytes)
        .build()
    )
    factory.enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
    factory.disable(JsonParser.Feature.ALLOW_NON_NUMERIC_NUMBERS)
    new ObjectMapper(factory)
      .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS)
  }

  private lazy val FrozenFeatureRegistry: JsonNode =
    Mapper.readTree(
      PostgreSqlEnumerationCampaignV2Json
        .structuralFeatureRegistryDocument()
        .bytes
    )

  def parse(
      bytes: Array[Byte]
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    PostgreSqlEnumerationCampaignV2AuthoringRequest
  ] =
    parseWithMaximumBytes(bytes, MaximumDocumentBytes)

  /**
    * Load from a caller-owned stream, retaining at most the bounded bytes and
    * reading at most one additional byte to prove an overflow.
    */
  def load(
      input: InputStream
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    PostgreSqlEnumerationCampaignV2AuthoringRequest
  ] =
    loadWithMaximumBytes(input, MaximumDocumentBytes)

  /** Test seam for exact size-bound behavior; production always uses 64 MiB. */
  private[cli] def parseWithMaximumBytes(
      bytes: Array[Byte],
      maximumBytes: Int
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    PostgreSqlEnumerationCampaignV2AuthoringRequest
  ] = {
    require(
      maximumBytes > 0 && maximumBytes <= MaximumDocumentBytes,
      "authoring parser byte bound must be within the production envelope"
    )
    if (bytes == null) Left(MissingInput)
    else if (bytes.length > maximumBytes) Left(DocumentTooLarge)
    else parseSnapshot(bytes.clone())
  }

  /** Test seam for the bounded stream loader. The stream remains caller-owned. */
  private[cli] def loadWithMaximumBytes(
      input: InputStream,
      maximumBytes: Int
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    PostgreSqlEnumerationCampaignV2AuthoringRequest
  ] = {
    require(
      maximumBytes > 0 && maximumBytes <= MaximumDocumentBytes,
      "authoring loader byte bound must be within the production envelope"
    )
    if (input == null) Left(MissingInput)
    else {
      val output = new ByteArrayOutputStream(
        math.min(ReadBufferBytes, maximumBytes)
      )
      val buffer = new Array[Byte](math.min(ReadBufferBytes, maximumBytes + 1))
      var total = 0
      try {
        var done = false
        while (!done) {
          val remainingPlusOne = maximumBytes - total + 1
          val requested = math.min(buffer.length, remainingPlusOne)
          val read = input.read(buffer, 0, requested)
          if (read < 0) done = true
          else if (read == 0) {
            val one = input.read()
            if (one < 0) done = true
            else if (total == maximumBytes) return Left(DocumentTooLarge)
            else {
              output.write(one)
              total += 1
            }
          } else if (read > maximumBytes - total)
            return Left(DocumentTooLarge)
          else {
            output.write(buffer, 0, read)
            total += read
          }
        }
        parseSnapshot(output.toByteArray)
      } catch {
        case _: IOException => Left(InputReadFailed)
      }
    }
  }

  private def parseSnapshot(
      bytes: Array[Byte]
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    PostgreSqlEnumerationCampaignV2AuthoringRequest
  ] = {
    if (!hasSoleFinalLf(bytes)) Left(InvalidCanonicalTerminator)
    else if (!isStrictUtf8(bytes)) Left(InvalidUtf8)
    else {
      parseJson(bytes).flatMap(parseDocument(_, bytes))
    }
  }

  private def parseJson(
      bytes: Array[Byte]
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, JsonNode] =
    try {
      val value = Mapper.readTree(bytes)
      if (value == null) Left(InvalidJson) else Right(value)
    } catch {
      case failure: StreamConstraintsException =>
        if (Option(failure.getMessage).exists(_.toLowerCase.contains("depth")))
          Left(ExcessiveJsonDepth)
        else Left(InvalidJson)
      case failure: JsonParseException =>
        if (
          Option(failure.getOriginalMessage).exists(
            _.toLowerCase.contains("duplicate field")
          )
        ) Left(DuplicateJsonKey)
        else Left(InvalidJson)
      case _: IOException => Left(InvalidJson)
    }

  private def parseDocument(
      rootValue: JsonNode,
      exactBytes: Array[Byte]
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    PostgreSqlEnumerationCampaignV2AuthoringRequest
  ] =
    for {
      root <- closedObject(rootValue, RootKeys)
      schema <- text(root, "schema_version")
      _ <- exact(
        schema,
        PostgreSqlEnumerationCampaignV2AuthoringRequest.SchemaVersion
      )
      protocol <- text(root, "protocol_version")
      _ <- exact(protocol, PostgreSqlEnumerationCampaignV2Json.ProtocolVersion)
      workloadId <- text(root, "workload_id")
      lineageId <- text(root, "lineage_id")
      sourceSql <- parseSourceSql(root.get("source_sql"))
      dataContract <- parseDataContract(root.get("data_contract"))
      _ <- validateFeatureRegistry(root.get("feature_registry"))
      settings <- parseSettings(root.get("settings_bundle"))
      toolchain <- parseToolchain(root.get("toolchain_bundle"))
      input <- construct(
        PostgreSqlEnumerationCampaignV2AuthoringInput(
          sourceSql = sourceSql,
          workloadId = workloadId,
          lineageId = lineageId,
          widthBound = settings.widthBound,
          requestedK = settings.requestedK,
          requireConnectedCover = settings.requireConnectedCover,
          dataContract = dataContract,
          producerEntrypoint = toolchain.producerEntrypoint,
          toolchainComponents = toolchain.components
        )
      )
      request <- PostgreSqlEnumerationCampaignV2AuthoringRequest
        .create(input)
        .left
        .map(
          _ =>
            InvalidContent: PostgreSqlEnumerationCampaignV2AuthoringParseFailure
        )
      _ <-
        if (request.hasExactCanonicalBytes(exactBytes)) Right(())
        else Left(NonCanonicalDocument)
    } yield request

  private final case class ParsedSettings(
      widthBound: Int,
      requestedK: Int,
      requireConnectedCover: Boolean
  )

  private final case class ParsedToolchain(
      producerEntrypoint: String,
      components: Vector[PostgreSqlEnumerationCampaignV2ToolchainComponent]
  )

  private def parseSourceSql(
      value: JsonNode
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, String] =
    for {
      document <- closedObject(value, SourceSqlKeys)
      schema <- text(document, "schema_version")
      _ <- exact(schema, PostgreSqlEnumerationCampaignV2Json.SourceSqlSchema)
      encoding <- text(document, "encoding")
      _ <- exact(encoding, "utf-8")
      byteLength <- boundedInt(
        document,
        "byte_length",
        minimum = 1,
        maximum = MaximumSourceSqlBytes,
        InvalidSourceSql
      )
      encoded <- text(document, "utf8_hex", InvalidSourceSql)
      digest <- text(document, "sha256", InvalidSourceSql)
      raw <- decodeLowerHex(encoded, byteLength)
      _ <-
        if (sha256(raw) == digest) Right(()) else Left(InvalidSourceSql)
      sql <- decodeStrictUtf8(raw).toRight(InvalidSourceSql)
      _ <- if (sql.indexOf('\u0000') < 0) Right(()) else Left(InvalidSourceSql)
    } yield sql

  private def parseDataContract(
      value: JsonNode
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    PostgreSqlEnumerationCampaignV2DataContract
  ] =
    for {
      document <- closedObject(value, DataContractKeys)
      schema <- text(document, "schema_version")
      _ <- exact(
        schema,
        PostgreSqlEnumerationCampaignV2AuthoringRequest.DataContractSchema
      )
      datasetId <- text(document, "dataset_id")
      manifest <- text(document, "dataset_manifest_sha256")
      receipt <- text(document, "load_receipt_sha256")
      semantics <- text(document, "data_semantics_sha256")
      result <- construct(
        PostgreSqlEnumerationCampaignV2DataContract(
          datasetId,
          manifest,
          receipt,
          semantics
        )
      )
    } yield result

  private def parseSettings(
      value: JsonNode
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    ParsedSettings
  ] =
    for {
      document <- closedObject(value, SettingsKeys)
      schema <- text(document, "schema_version")
      _ <- exact(
        schema,
        PostgreSqlEnumerationCampaignV2AuthoringRequest.SettingsBundleSchema
      )
      search <- closedObject(document.get("search"), SearchKeys)
      algorithm <- text(search, "algorithm")
      _ <- exact(algorithm, "top-k-candidate-td-v1")
      admissibility <- text(search, "admissibility")
      _ <- exact(
        admissibility,
        PostgreSqlEnumerationCampaignV2AuthoringRequest.Admissibility
      )
      requestedK <- boundedInt(search, "requested_k", 1, 10, InvalidContent)
      widthBound <- boundedInt(
        search,
        "width_bound",
        1,
        Int.MaxValue,
        InvalidContent
      )
      requireConnectedCover <- boolean(search, "require_connected_cover")
      _ <- validateFeatureCapture(document.get("feature_capture"))
      _ <- validateFixedPhysical(document.get("fixed_physical_config"))
      _ <- validateSession(document.get("postgres_session"))
    } yield ParsedSettings(widthBound, requestedK, requireConnectedCover)

  private def validateFeatureCapture(
      value: JsonNode
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, Unit] =
    for {
      capture <- closedObject(value, FeatureCaptureKeys)
      analyze <- boolean(capture, "analyze")
      sameSession <- boolean(capture, "same_session")
      includeCatalog <- boolean(capture, "include_catalog")
      includeWholePlan <- boolean(capture, "include_whole_plan")
      failurePolicy <- text(capture, "failure_policy")
      _ <- exact(analyze, false)
      _ <- exact(sameSession, true)
      _ <- exact(
        includeCatalog,
        PostgreSqlEnumerationCampaignV2AuthoringRequest.IncludeCatalog
      )
      _ <- exact(
        includeWholePlan,
        PostgreSqlEnumerationCampaignV2AuthoringRequest.IncludeWholePlan
      )
      _ <- exact(failurePolicy, "record-explicit-state")
    } yield ()

  private def validateFixedPhysical(
      value: JsonNode
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, Unit] =
    for {
      physical <- closedObject(value, FixedPhysicalKeys)
      cte <- text(physical, "cte_materialization")
      _ <- exact(cte, "planner-controlled")
      scope <- text(physical, "cte_materialization_scope")
      _ <- exact(scope, "all-emitted-ctes-v1")
      idScheme <- text(physical, "physical_variant_id_scheme")
      _ <- exact(idScheme, "postgresql_candidate_reduction_variant_v1")
      mode <- text(physical, "reduction_mode")
      _ <- exact(mode, "full")
      order <- text(physical, "reduction_order")
      _ <- exact(order, "canonical_plan_schedule_v1")
      shape <- text(physical, "sql_shape")
      _ <- exact(shape, "one-statement-cte")
    } yield ()

  private def validateSession(
      value: JsonNode
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, Unit] = {
    if (
      value == null || !value.isArray ||
      value.size() != FrozenSessionSettings.size
    )
      Left(InvalidShape)
    else {
      val parsed = value.elements().asScala.toVector.map { settingValue =>
        for {
          setting <- closedObject(settingValue, SessionSettingKeys)
          name <- text(setting, "name")
          settingValue <- text(setting, "value")
        } yield name -> settingValue
      }
      sequence(parsed).flatMap { settings =>
        if (settings == FrozenSessionSettings) Right(())
        else Left(FrozenContractMismatch)
      }
    }
  }

  private def validateFeatureRegistry(
      value: JsonNode
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, Unit] = {
    if (value == null || !value.isObject) Left(InvalidShape)
    else if (value == FrozenFeatureRegistry) Right(())
    else Left(FrozenContractMismatch)
  }

  private def parseToolchain(
      value: JsonNode
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    ParsedToolchain
  ] =
    for {
      document <- closedObject(value, ToolchainKeys)
      schema <- text(document, "schema_version")
      _ <- exact(
        schema,
        PostgreSqlEnumerationCampaignV2AuthoringRequest.ToolchainBundleSchema
      )
      producerEntrypoint <- text(document, "producer_entrypoint")
      componentsNode <-
        if (
          document.get("components") != null &&
          document.get("components").isArray &&
          document.get("components").size() >= 1 &&
          document.get("components").size() <= 128
        ) Right(document.get("components"))
        else Left(InvalidShape)
      components <- sequence(
        componentsNode.elements().asScala.toVector.map(parseToolchainComponent)
      )
    } yield ParsedToolchain(producerEntrypoint, components)

  private def parseToolchainComponent(
      value: JsonNode
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    PostgreSqlEnumerationCampaignV2ToolchainComponent
  ] =
    for {
      component <- closedObject(value, ToolchainComponentKeys)
      name <- text(component, "name")
      version <- text(component, "version")
      digest <- text(component, "artifact_sha256")
      result <- construct(
        PostgreSqlEnumerationCampaignV2ToolchainComponent(
          name,
          version,
          digest
        )
      )
    } yield result

  private def closedObject(
      value: JsonNode,
      keys: Set[String]
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, ObjectNode] = {
    if (value == null || !value.isObject) Left(InvalidShape)
    else {
      val result = value.asInstanceOf[ObjectNode]
      val actual = result.fieldNames().asScala.toSet
      if (actual == keys && result.size() == keys.size) Right(result)
      else Left(InvalidShape)
    }
  }

  private def text(
      objectNode: ObjectNode,
      field: String,
      failure: PostgreSqlEnumerationCampaignV2AuthoringParseFailure = InvalidShape
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, String] = {
    val value = objectNode.get(field)
    if (value != null && value.isTextual) Right(value.textValue())
    else Left(failure)
  }

  private def boolean(
      objectNode: ObjectNode,
      field: String
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, Boolean] = {
    val value = objectNode.get(field)
    if (value != null && value.isBoolean) Right(value.booleanValue())
    else Left(InvalidShape)
  }

  private def boundedInt(
      objectNode: ObjectNode,
      field: String,
      minimum: Int,
      maximum: Int,
      failure: PostgreSqlEnumerationCampaignV2AuthoringParseFailure
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, Int] = {
    val value = objectNode.get(field)
    if (
      value != null && value.isIntegralNumber && value.canConvertToInt &&
      value.intValue() >= minimum && value.intValue() <= maximum
    ) Right(value.intValue())
    else Left(failure)
  }

  private def exact[A](
      actual: A,
      expected: A
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, Unit] =
    if (actual == expected) Right(()) else Left(FrozenContractMismatch)

  private def construct[A](
      value: => A
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, A] =
    try Right(value)
    catch {
      case _: IllegalArgumentException => Left(InvalidContent)
    }

  private def sequence[A](
      values: Vector[
        Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, A]
      ]
  ): Either[PostgreSqlEnumerationCampaignV2AuthoringParseFailure, Vector[A]] =
    values.foldLeft(
      Right(Vector.empty): Either[
        PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
        Vector[A]
      ]
    ) { (accumulated, next) =>
      for {
        prefix <- accumulated
        value <- next
      } yield prefix :+ value
    }

  private def decodeLowerHex(
      value: String,
      expectedBytes: Int
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    Array[Byte]
  ] = {
    if (
      value == null || value.length != expectedBytes * 2 ||
      value.length > MaximumSourceSqlBytes * 2
    ) Left(InvalidSourceSql)
    else {
      val output = new Array[Byte](expectedBytes)
      var index = 0
      while (index < expectedBytes) {
        val high = lowerHexNibble(value.charAt(index * 2))
        val low = lowerHexNibble(value.charAt(index * 2 + 1))
        if (high < 0 || low < 0) return Left(InvalidSourceSql)
        output(index) = ((high << 4) | low).toByte
        index += 1
      }
      Right(output)
    }
  }

  private def lowerHexNibble(value: Char): Int =
    if (value >= '0' && value <= '9') value - '0'
    else if (value >= 'a' && value <= 'f') value - 'a' + 10
    else -1

  private def decodeStrictUtf8(bytes: Array[Byte]): Option[String] = {
    val decoder = StandardCharsets.UTF_8
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    try Some(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch {
      case _: java.nio.charset.CharacterCodingException => None
    }
  }

  private def isStrictUtf8(bytes: Array[Byte]): Boolean = {
    val decoder = StandardCharsets.UTF_8
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    val input = ByteBuffer.wrap(bytes)
    val output = CharBuffer.allocate(ReadBufferBytes)
    var decoding = true
    while (decoding) {
      val result = decoder.decode(input, output, true)
      if (result.isError) return false
      if (result.isOverflow) output.clear()
      else decoding = false
    }
    output.clear()
    var flushing = true
    while (flushing) {
      val result = decoder.flush(output)
      if (result.isError) return false
      if (result.isOverflow) output.clear()
      else flushing = false
    }
    true
  }

  private def hasSoleFinalLf(bytes: Array[Byte]): Boolean = {
    if (bytes.isEmpty || bytes.last != '\n'.toByte) false
    else {
      var lineFeeds = 0
      var index = 0
      while (index < bytes.length) {
        if (bytes(index) == '\n'.toByte) lineFeeds += 1
        if (bytes(index) == '\r'.toByte) return false
        index += 1
      }
      lineFeeds == 1
    }
  }

  private def sha256(bytes: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
    val alphabet = "0123456789abcdef"
    val output = new StringBuilder(digest.length * 2)
    digest.foreach { byte =>
      val unsigned = byte & 0xff
      output.append(alphabet.charAt(unsigned >>> 4))
      output.append(alphabet.charAt(unsigned & 0x0f))
    }
    output.result()
  }
}
