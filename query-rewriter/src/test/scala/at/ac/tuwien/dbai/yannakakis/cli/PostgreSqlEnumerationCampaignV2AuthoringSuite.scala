package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets

class PostgreSqlEnumerationCampaignV2AuthoringSuite extends munit.FunSuite {
  private val hashes = Vector.tabulate(6)(index =>
    (('a'.toInt + index).toChar.toString) * 64
  )

  test("typed authoring input derives the complete canonical producer request") {
    val request = authoring("SELECT 'é'\n", requestedK = 3)
    val parsed = ujson.read(request.document.utf8)

    assertEquals(request.batchRequest.originalSql, "SELECT 'é'\n")
    assertEquals(request.batchRequest.widthBound, 2)
    assertEquals(request.batchRequest.planLimit, 3)
    assertEquals(request.batchRequest.requireConnectedCover, true)
    assertEquals(parsed.obj.keySet, Set(
      "data_contract",
      "feature_registry",
      "lineage_id",
      "protocol_version",
      "schema_version",
      "settings_bundle",
      "source_sql",
      "toolchain_bundle",
      "workload_id"
    ))
    assertEquals(
      parsed("source_sql")("utf8_hex").str,
      "SELECT 'é'\n".getBytes(StandardCharsets.UTF_8).iterator
        .map(byte => f"${byte & 0xff}%02x")
        .mkString
    )
    assertEquals(
      request.sourceSqlDocument,
      PostgreSqlEnumerationCampaignV2Json.sourceSqlDocument("SELECT 'é'\n")
    )
    assertEquals(
      request.featureRegistryDocument.sha256,
      "d945eb526d7d29e2ebba1cd1038f7f737d9834957224a204d997395f9b5f208e"
    )
    assertEquals(
      parsed("settings_bundle")("feature_capture")("include_catalog").bool,
      false
    )
    assertEquals(
      parsed("settings_bundle")("feature_capture")("include_whole_plan").bool,
      false
    )
    assertEquals(
      parsed("settings_bundle")("postgres_session").arr.map(_("name").str).toVector,
      Vector(
        "datestyle",
        "extra_float_digits",
        "standard_conforming_strings",
        "timezone"
      )
    )
    assertEquals(
      parsed("settings_bundle")("fixed_physical_config").obj.toMap,
      ujson.read(request.fixedPhysicalConfigDocument.utf8).obj.toMap
    )
    assertEquals(request.document.utf8.count(_ == '\n'), 1)
    assertEquals(request.document.bytes.last, '\n'.toByte)
    assert(request.hasExactCanonicalBytes(request.document.bytes))
    val hostile = request.document.bytes
    hostile(0) = '['.toByte
    assert(!request.hasExactCanonicalBytes(hostile))
  }

  test("authoring is deterministic and binds every typed input") {
    val first = authoring("SELECT 1", requestedK = 2)
    val second = authoring("SELECT 1", requestedK = 2)
    val changedSql = authoring("SELECT 2", requestedK = 2)
    val changedK = authoring("SELECT 1", requestedK = 3)

    assertEquals(first.document, second.document)
    assertEquals(first.sha256, second.sha256)
    assertNotEquals(first.sha256, changedSql.sha256)
    assertNotEquals(first.sha256, changedK.sha256)
    assertEquals(first.sha256.length, 64)
    assert(first.sha256.forall(character =>
      character.isDigit || (character >= 'a' && character <= 'f')
    ))
  }

  test("canonical algebra embeds documents and streams their exact UTF-8 hex") {
    import PostgreSqlEnumerationCampaignV2CanonicalJson._
    val inner = document(obj("z" -> string("é"), "a" -> number(BigInt(1))))
    val outer = document(
      obj(
        "embedded" -> embeddedDocument(inner),
        "hex" -> utf8HexDocument(inner)
      )
    )
    val expectedValue = inner.utf8.stripSuffix("\n")
    val expectedHex = expectedValue.getBytes(StandardCharsets.UTF_8).iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

    assertEquals(
      outer.utf8,
      s"{\"embedded\":$expectedValue,\"hex\":\"$expectedHex\"}\n"
    )
    assertEquals(outer.utf8.count(_ == '\n'), 1)
  }

  test("authoring rejects empty or transport-invalid source SQL") {
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringRequest.create(input("")),
      Left("authoring-source-sql-empty")
    )
    assertEquals(
      PostgreSqlEnumerationCampaignV2AuthoringRequest.create(
        input("SELECT '\u0000'")
      ),
      Left("authoring-batch-request-invalid")
    )
  }

  private def authoring(
      sql: String,
      requestedK: Int
  ): PostgreSqlEnumerationCampaignV2AuthoringRequest =
    PostgreSqlEnumerationCampaignV2AuthoringRequest
      .create(input(sql, requestedK))
      .fold(problem => fail(problem), identity)

  private def input(
      sql: String,
      requestedK: Int = 2
  ): PostgreSqlEnumerationCampaignV2AuthoringInput =
    PostgreSqlEnumerationCampaignV2AuthoringInput(
      sourceSql = sql,
      workloadId = "triangle-cell-01",
      lineageId = "triangle-lineage-v2",
      widthBound = 2,
      requestedK = requestedK,
      requireConnectedCover = true,
      dataContract = PostgreSqlEnumerationCampaignV2DataContract(
        datasetId = "triangle-fixture-v2",
        datasetManifestSha256 = hashes(0),
        loadReceiptSha256 = hashes(1),
        dataSemanticsSha256 = hashes(2)
      ),
      producerEntrypoint =
        "at.ac.tuwien.dbai.yannakakis.cli.PostgreSqlEnumerationCampaignV2AcquisitionCli",
      toolchainComponents = Vector(
        PostgreSqlEnumerationCampaignV2ToolchainComponent(
          "calcite",
          "1.40.0",
          hashes(3)
        ),
        PostgreSqlEnumerationCampaignV2ToolchainComponent(
          "query-rewriter",
          "commit-fixture",
          hashes(4)
        ),
        PostgreSqlEnumerationCampaignV2ToolchainComponent(
          "scala",
          "2.13.16",
          hashes(5)
        )
      )
    )
}
