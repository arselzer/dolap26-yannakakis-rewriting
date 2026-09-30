package at.ac.tuwien.dbai.yannakakis.cli

import java.io.{ByteArrayInputStream, IOException, InputStream}
import java.nio.charset.StandardCharsets

class PostgreSqlEnumerationCampaignV2AuthoringParserSuite
    extends munit.FunSuite {
  import PostgreSqlEnumerationCampaignV2AuthoringParseFailure._

  private val sourceSql = "SELECT 'é' AS value\n"

  private def canonicalRequest()
      : PostgreSqlEnumerationCampaignV2AuthoringRequest =
    PostgreSqlEnumerationCampaignV2AuthoringRequest
      .create(
        PostgreSqlEnumerationCampaignV2AuthoringInput(
          sourceSql = sourceSql,
          workloadId = "c01.sparse",
          lineageId = "lineage:2026-08-22",
          widthBound = 2,
          requestedK = 7,
          requireConnectedCover = true,
          dataContract = PostgreSqlEnumerationCampaignV2DataContract(
            datasetId = "c01.sparse.data",
            datasetManifestSha256 = "a" * 64,
            loadReceiptSha256 = "b" * 64,
            dataSemanticsSha256 = "c" * 64
          ),
          producerEntrypoint =
            "at.ac.tuwien.dbai.yannakakis.cli.EnumerationAcquisition",
          toolchainComponents = Vector(
            PostgreSqlEnumerationCampaignV2ToolchainComponent(
              "java",
              "25.0.3",
              "d" * 64
            ),
            PostgreSqlEnumerationCampaignV2ToolchainComponent(
              "query-rewriter",
              "41cca0d",
              "e" * 64
            )
          )
        )
      )
      .fold(problem => fail(problem), identity)

  test("strict parser reconstructs the typed request from exact canonical bytes") {
    val expected = canonicalRequest()
    val parsed = PostgreSqlEnumerationCampaignV2AuthoringParser
      .parse(expected.document.bytes)
      .fold(problem => fail(problem.code), identity)

    assertEquals(parsed.document, expected.document)
    assertEquals(parsed.sha256, expected.sha256)
    assertEquals(parsed.workloadId, "c01.sparse")
    assertEquals(parsed.lineageId, "lineage:2026-08-22")
    assertEquals(parsed.batchRequest.originalSql, sourceSql)
    assertEquals(parsed.batchRequest.widthBound, 2)
    assertEquals(parsed.batchRequest.planLimit, 7)
    assert(parsed.batchRequest.requireConnectedCover)
  }

  test("parser and loader reject missing input as a typed failure") {
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.parse(null),
      Left(MissingInput)
    )
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.load(null),
      Left(MissingInput)
    )
  }

  test("bounded stream loader accepts the exact request and leaves the stream open") {
    val expected = canonicalRequest()
    final class TrackedInput(bytes: Array[Byte])
        extends ByteArrayInputStream(bytes) {
      var closed: Boolean = false
      override def close(): Unit = closed = true
    }
    val input = new TrackedInput(expected.document.bytes)

    val parsed = PostgreSqlEnumerationCampaignV2AuthoringParser
      .load(input)
      .fold(problem => fail(problem.code), identity)

    assertEquals(parsed.sha256, expected.sha256)
    assert(!input.closed)
  }

  test("byte-array and stream entry points enforce their exact size bound") {
    val bytes = canonicalRequest().document.bytes
    val maximum = bytes.length - 1

    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser
        .parseWithMaximumBytes(bytes, maximum),
      Left(DocumentTooLarge)
    )
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser
        .loadWithMaximumBytes(new ByteArrayInputStream(bytes), maximum),
      Left(DocumentTooLarge)
    )
  }

  test("loader maps I/O failures without exposing exception text") {
    val failing = new InputStream {
      override def read(): Int = throw new IOException("private path")
    }
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.load(failing),
      Left(InputReadFailed)
    )
  }

  test("parser rejects missing, repeated, and non-final LF terminators") {
    val bytes = canonicalRequest().document.bytes
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.parse(bytes.dropRight(1)),
      Left(InvalidCanonicalTerminator)
    )
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.parse(
        bytes ++ Array('\n'.toByte)
      ),
      Left(InvalidCanonicalTerminator)
    )
    val embeddedLf = bytes.clone()
    embeddedLf(embeddedLf.length / 2) = '\n'.toByte
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.parse(embeddedLf),
      Left(InvalidCanonicalTerminator)
    )
  }

  test("parser rejects malformed UTF-8 before JSON interpretation") {
    val bytes = Array[Byte](
      0x7b,
      0x22,
      0x78,
      0x22,
      0x3a,
      0x22,
      0xc3.toByte,
      0x28,
      0x22,
      0x7d,
      0x0a
    )
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.parse(bytes),
      Left(InvalidUtf8)
    )
  }

  test("parser rejects duplicate object keys and nesting beyond 128") {
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.parse(
        "{\"a\":1,\"a\":2}\n".getBytes(StandardCharsets.UTF_8)
      ),
      Left(DuplicateJsonKey)
    )

    def nested(depth: Int): Array[Byte] =
      (Vector.fill(depth)("[").mkString + "0" +
        Vector.fill(depth)("]").mkString + "\n").getBytes(StandardCharsets.UTF_8)
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.parse(nested(128)),
      Left(InvalidShape)
    )
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.parse(nested(129)),
      Left(ExcessiveJsonDepth)
    )
  }

  test("parser rejects valid JSON that is not the exact canonical byte document") {
    val canonical = canonicalRequest().document.utf8
    val withWhitespace = canonical.dropRight(1) + " \n"
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringParser.parse(
        withWhitespace.getBytes(StandardCharsets.UTF_8)
      ),
      Left(NonCanonicalDocument)
    )
  }

  test("parser closes the root shape and the frozen settings and registry") {
    val canonical = canonicalRequest().document.utf8
    val extraRootKey = "{\"unexpected\":0," + canonical.drop(1)
    assertEquals(
      parseUtf8(extraRootKey),
      Left(InvalidShape)
    )

    val changedCapture = canonical.replace(
      "\"include_catalog\":false",
      "\"include_catalog\":true"
    )
    assertNotEquals(changedCapture, canonical)
    assertEquals(
      parseUtf8(changedCapture),
      Left(FrozenContractMismatch)
    )

    val changedRegistry = canonical.replace(
      "postgresql_enumeration_structural_features/v1",
      "postgresql_enumeration_structural_features/v2"
    )
    assertNotEquals(changedRegistry, canonical)
    assertEquals(
      parseUtf8(changedRegistry),
      Left(FrozenContractMismatch)
    )
  }

  test("parser verifies the source-SQL digest before request reconstruction") {
    val expected = canonicalRequest()
    val changed = expected.document.utf8.replace(
      expected.batchRequest.originalSqlSha256,
      "f" * 64
    )
    assertNotEquals(changed, expected.document.utf8)
    assertEquals(
      parseUtf8(changed),
      Left(InvalidSourceSql)
    )
  }

  private def parseUtf8(
      value: String
  ): Either[
    PostgreSqlEnumerationCampaignV2AuthoringParseFailure,
    PostgreSqlEnumerationCampaignV2AuthoringRequest
  ] =
    PostgreSqlEnumerationCampaignV2AuthoringParser.parse(
      value.getBytes(StandardCharsets.UTF_8)
    )
}
