package at.ac.tuwien.dbai.yannakakis.cli

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path, Paths}

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationBatchResult,
  PostgreSqlEnumerationCampaignV2HostileEvidence,
  PostgreSqlQueryRewriter
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

class PostgreSqlEnumerationCampaignV2AcquisitionSuite
    extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  private val chainSql =
    """SELECT a.id, b.c_id
      |FROM chain_a AS a
      |JOIN chain_b AS b ON a.b_id = b.id""".stripMargin

  test("found retained prefix assembles and composes across the Python boundary") {
    val result = produce(triangleSql, width = 2, k = 1)
    val authoring = requestFor(result)
    val assembly = PostgreSqlEnumerationCampaignV2Acquisition
      .assemble(authoring, result)
      .fold(problem => fail(problem.toString), identity)
    val parsed = ujson.read(assembly.document.utf8)
    assertEquals(
      Vector(
        authoring.sha256,
        assembly.projection.semanticSearchCore.sha256,
        assembly.projection.producerSearchContract.sha256,
        assembly.projection.producerSearchTranscript.sha256,
        assembly.actionRegistry.sha256,
        assembly.document.sha256
      ),
      Vector(
        "ffba3ca42a90de0af73c2f2ce3734d8089f0afb51f01161136abe8ddcfc2e431",
        "ef36076ec029c65da08e402f3fcf6fc87f3d9d193fb391b685ede2c4ab2a9c59",
        "050e06c8cb75080f04e8b10936c094bcbf3f3afc59256b664e6c49134c9c5e50",
        "69ec4380e7e5f20ce66e311ebb6b763df72369410f66528357ae0df08997bc80",
        "cd72814f38399f40b1736a81c27b1b3fbe96bda1d49c25e22282efe630568d26",
        "bd0789e378b98b10834232ff91efcf45fc1248e2ddae0aca0ddc9ea2e5838946"
      )
    )

    assertEquals(
      parsed("schema_version").str,
      "yrw-enumeration-offline-acquisition-assembly/v2"
    )
    assertEquals(parsed("semantic_search_core")("status").str, "found")
    assertEquals(parsed("action_registry")("actions").arr.size, 2)
    assertEquals(parsed("feature_value_bundles").arr.size, 2)
    assertEquals(parsed("canonical_decompositions").arr.size, 1)
    assertEquals(parsed("non_sql_assets").arr.size, 6)
    assertEquals(assembly.reasonDetailPreimages.size, 2)
    assertEquals(assembly.nonSqlAssets.size, 6)
    assert(
      assembly.nonSqlAssets.forall(asset =>
        asset.payloadSha256 ==
          ujson.read(asset.wrapper.utf8)("sha256").str
      )
    )
    assert(
      assembly.nonSqlAssets.exists(asset =>
        asset.payloadSha256 != asset.wrapper.sha256
      )
    )
    assertEquals(
      assembly.nonSqlAssets.map(_.assetId),
      assembly.nonSqlAssets.map(_.assetId).sorted
    )
    assertEquals(
      assembly.cumulativeEvidence.usage.sourceSqlItemCount,
      1
    )
    assert(assembly.cumulativeEvidence.usage.generatedSqlItemCount > 0)
    assert(assembly.cumulativeEvidence.usage.stageSqlItemCount > 0)

    val packaged = composeAndValidate(assembly.document)
    assertEquals(
      packaged("acquisition_package_seal_sha256").str,
      "eda89027acd8f6af96313f8b07381b7f672983d66e6aee730cbad09231221442"
    )
    assertEquals(packaged("acquisition_status").str, "found-actions")
    assertEquals(packaged("reportable").bool, false)
    assertEquals(packaged("outcomes_in_scope").bool, false)
  }

  test("exact rejected search becomes the only no-decomposition native fallback") {
    val result = produce(triangleSql, width = 1, k = 3)
    val authoring = requestFor(result)
    val assembly = PostgreSqlEnumerationCampaignV2Acquisition
      .assemble(authoring, result)
      .fold(problem => fail(problem.toString), identity)
    val parsed = ujson.read(assembly.document.utf8)
    assertEquals(
      Vector(
        authoring.sha256,
        assembly.reasonDetailPreimages.head.sha256,
        assembly.projection.semanticSearchCore.sha256,
        assembly.projection.producerSearchContract.sha256,
        assembly.projection.producerSearchTranscript.sha256,
        assembly.actionRegistry.sha256,
        assembly.document.sha256
      ),
      Vector(
        "f9adced75b888ac361403b38f3b3cd7a06a67fd37c8b826b1ebb577eca27007e",
        "44f80d8cb544956091f37eae121ea7409f2d1ee2362cdfa4e2c2f5703be11e19",
        "638c8b2c5b7e16152f6c4030351dd0a78719c68a5990a59afdee5d395c207bda",
        "07d0bacdc51dff4e6278afbfb12470f8c023c59d311c7e0fdf6279dc717afce9",
        "5f8352bc4d520602a9c262df4d6b1b9aa4f00eb9d914492ac8af1e7d6b2ac568",
        "d2c17d907dfc0ae90a1e21648ab129142afdc6aac1abf04a8c32c71db4fc6cf9",
        "7e7fd8c11ca3efef70b55e84148fb2d471cfed8d7cb8a90c1d55f56bac995e37"
      )
    )
    val core = parsed("semantic_search_core")

    assertEquals(core("status").str, "native-fallback")
    assertEquals(core("termination").str, "native-fallback")
    assertEquals(core("rejection")("code").str, "no-decomposition")
    assertEquals(core("rejection")("phase").str, "candidate-search")
    assertEquals(core("candidates").arr.size, 0)
    assertEquals(
      parsed("producer_search_transcript")("candidate_search")("state").str,
      "rejected"
    )
    assertEquals(
      parsed("producer_search_transcript")("completed_phase").str,
      "candidate-search-completed"
    )
    assertEquals(parsed("action_registry")("actions").arr.size, 1)
    assertEquals(parsed("feature_value_bundles").arr.size, 1)
    assertEquals(parsed("canonical_decompositions").arr.size, 0)
    assertEquals(parsed("non_sql_assets").arr.size, 2)
    assertEquals(assembly.reasonDetailPreimages.size, 1)
    assertEquals(
      ujson.read(assembly.nonSqlAssets.head.payload.utf8)("capture_state").str,
      "native-fallback"
    )

    val packaged = composeAndValidate(assembly.document)
    assertEquals(
      packaged("acquisition_package_seal_sha256").str,
      "e2c5ce562e7b1c34eb92c1803cee5fa7925d98889a8edb78e5dc2b241b29139b"
    )
    assertEquals(
      packaged("acquisition_status").str,
      "typed-native-fallback"
    )
    assertEquals(
      packaged("resource_statistics_certificate")("fallback_reason")("code").str,
      "no-decomposition"
    )
  }

  test("invalid and unsupported inputs remain unpackageable") {
    val cases = Vector(
      produce("   ", width = 2, k = 1),
      produce("SELECT r.a FROM triangle_r AS r", width = 2, k = 1)
    )
    cases.foreach { result =>
      val failure = PostgreSqlEnumerationCampaignV2Acquisition
        .assemble(requestFor(result), result)
        .left
        .toOption
        .getOrElse(fail("unpackageable result produced an assembly"))
      assertEquals(
        failure.code,
        "enumeration-campaign-v2.acquisition.unpackageable-result"
      )
    }
  }

  test("typed authoring mismatch fails before projection") {
    val result = produce(triangleSql, width = 2, k = 1)
    val mismatched = authoring(
      sourceSql = triangleSql,
      width = 2,
      k = 2,
      connected = true
    )
    val failure = PostgreSqlEnumerationCampaignV2Acquisition
      .assemble(mismatched, result)
      .left
      .toOption
      .getOrElse(fail("mismatched authoring request was accepted"))
    assertEquals(
      failure.code,
      "enumeration-campaign-v2.acquisition.authoring-mismatch"
    )
  }

  test("packageability rejects mismatched typed structural evidence") {
    val rejected = produce(triangleSql, width = 1, k = 3)
    val differentQuery = produce(chainSql, width = 1, k = 3)
    val differentWidth = produce(triangleSql, width = 2, k = 3)
    val differentConnectedMode =
      produce(triangleSql, width = 1, k = 3, connected = false)
    assert(
      rejected.pipelineEvidence.analysis.nonEmpty &&
        differentQuery.pipelineEvidence.analysis.nonEmpty
    )
    assertNotEquals(
      rejected.pipelineEvidence.hypergraph,
      differentQuery.pipelineEvidence.hypergraph
    )
    assertEquals(
      Vector(
        rejected.pipelineEvidence.softGeneration.map(_.widthBound),
        differentWidth.pipelineEvidence.softGeneration.map(_.widthBound)
      ),
      Vector(Some(1), Some(2))
    )
    assertEquals(
      Vector(
        rejected.pipelineEvidence.admissibleGeneration
          .map(_.requireConnectedCover),
        differentConnectedMode.pipelineEvidence.admissibleGeneration
          .map(_.requireConnectedCover)
      ),
      Vector(Some(true), Some(false))
    )
    assertEquals(
      Vector(
        rejected.pipelineEvidence.candidateInput
          .map(_.requireConnectedCover),
        differentConnectedMode.pipelineEvidence.candidateInput
          .map(_.requireConnectedCover)
      ),
      Vector(Some(true), Some(false))
    )
    val hostile = Vector(
      "analysis" -> PostgreSqlEnumerationCampaignV2HostileEvidence
        .withAnalysis(rejected, differentQuery),
      "hypergraph" -> PostgreSqlEnumerationCampaignV2HostileEvidence
        .withHypergraph(rejected, differentQuery),
      "soft width/source" -> PostgreSqlEnumerationCampaignV2HostileEvidence
        .withSoftGeneration(rejected, differentWidth),
      "admissible ConCov" -> PostgreSqlEnumerationCampaignV2HostileEvidence
        .withAdmissibleGeneration(rejected, differentConnectedMode),
      "candidate input ConCov" ->
        PostgreSqlEnumerationCampaignV2HostileEvidence
          .withCandidateInput(rejected, differentConnectedMode)
    )

    hostile.foreach { case (label, result) =>
      val failure = PostgreSqlEnumerationCampaignV2Packageability
        .classify(requestFor(rejected), result)
        .left
        .toOption
        .getOrElse(fail(s"mismatched $label evidence was accepted"))
      assertEquals(
        failure.code,
        "enumeration-campaign-v2.acquisition.unpackageable-result",
        label
      )
    }
  }

  private def produce(
      sql: String,
      width: Int,
      k: Int,
      connected: Boolean = true
  ): PostgreSqlEnumerationBatchResult = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(value => Right(fixture.plan(value).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
    val request = PostgreSqlEnumerationBatchRequest
      .create(
        sql,
        widthBound = width,
        planLimit = k,
        requireConnectedCover = connected
      )
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    rewriter.produceEnumerationBatchLegacyV2(request)
  }

  private def requestFor(
      result: PostgreSqlEnumerationBatchResult
  ): PostgreSqlEnumerationCampaignV2AuthoringRequest =
    authoring(
      result.request.originalSql,
      result.request.widthBound,
      result.request.planLimit,
      result.request.requireConnectedCover
    )

  private def authoring(
      sourceSql: String,
      width: Int,
      k: Int,
      connected: Boolean
  ): PostgreSqlEnumerationCampaignV2AuthoringRequest =
    PostgreSqlEnumerationCampaignV2AuthoringRequest
      .create(
        PostgreSqlEnumerationCampaignV2AuthoringInput(
          sourceSql = sourceSql,
          workloadId = "triangle-cell-01",
          lineageId = "triangle-lineage-v2",
          widthBound = width,
          requestedK = k,
          requireConnectedCover = connected,
          dataContract = PostgreSqlEnumerationCampaignV2DataContract(
            "triangle-fixture-v2",
            "a" * 64,
            "b" * 64,
            "c" * 64
          ),
          producerEntrypoint =
            "at.ac.tuwien.dbai.yannakakis.cli.PostgreSqlEnumerationCampaignV2AcquisitionCli",
          toolchainComponents = Vector(
            PostgreSqlEnumerationCampaignV2ToolchainComponent(
              "calcite",
              "1.36.0",
              "d" * 64
            ),
            PostgreSqlEnumerationCampaignV2ToolchainComponent(
              "query-rewriter",
              "commit-fixture",
              "e" * 64
            ),
            PostgreSqlEnumerationCampaignV2ToolchainComponent(
              "scala",
              "2.13.13",
              "f" * 64
            )
          )
        )
      )
      .fold(problem => fail(problem), identity)

  private def composeAndValidate(
      assembly: PostgreSqlEnumerationCampaignV2JsonDocument
  ): ujson.Value = {
    val directory = Files.createTempDirectory("yrw-campaign-v2-package-")
    val input = directory.resolve("assembly.json")
    val output = directory.resolve("package.json")
    try {
      Files.write(input, assembly.bytes)
      val cli = campaignRoot.resolve("package_cli.py")
      val assembled = runPython(
        Vector(
          "python3",
          "-B",
          cli.toString,
          "assemble",
          "--input",
          input.toString,
          "--output",
          output.toString
        )
      )
      assertEquals(assembled._1, 0, assembled._3)
      assertEquals(assembled._2, "")
      assertEquals(assembled._3, "")
      val validated = runPython(
        Vector("python3", "-B", cli.toString, "validate", output.toString)
      )
      assertEquals(validated._1, 0, validated._3)
      assertEquals(validated._2, "")
      assertEquals(validated._3, "")
      ujson.read(Files.readString(output))
    } finally {
      Files.deleteIfExists(output)
      Files.deleteIfExists(input)
      Files.deleteIfExists(directory)
    }
  }

  private def campaignRoot: Path =
    Paths
      .get("..", "benchmarks", "cyclic-postgresql", "enumeration-campaign-v2")
      .toAbsolutePath
      .normalize()

  private def runPython(command: Vector[String]): (Int, String, String) = {
    val process = new ProcessBuilder(command: _*)
      .redirectInput(ProcessBuilder.Redirect.PIPE)
      .start()
    process.getOutputStream.close()
    val stdout = new ByteArrayOutputStream()
    val stderr = new ByteArrayOutputStream()
    val stdoutThread = new Thread(() => process.getInputStream.transferTo(stdout))
    val stderrThread = new Thread(() => process.getErrorStream.transferTo(stderr))
    stdoutThread.start()
    stderrThread.start()
    val exit = process.waitFor()
    stdoutThread.join()
    stderrThread.join()
    (
      exit,
      stdout.toString("UTF-8"),
      stderr.toString("UTF-8")
    )
  }
}
