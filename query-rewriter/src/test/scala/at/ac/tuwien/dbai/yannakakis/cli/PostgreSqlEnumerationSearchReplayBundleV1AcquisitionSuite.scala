package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationBatchResult,
  PostgreSqlQueryRewriter
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

import munit.FunSuite

final class PostgreSqlEnumerationSearchReplayBundleV1AcquisitionSuite
    extends FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("one found artifact yields the legacy package assembly and exact search input") {
    val result = produce(triangleSql, width = 2, k = 4)
    val authoring = requestFor(result)
    val pair = PostgreSqlEnumerationSearchReplayBundleV1Acquisition
      .assembleFoundPair(authoring, result)
      .fold(problem => fail(problem.toString), identity)
    val legacy = PostgreSqlEnumerationCampaignV2Acquisition
      .assemble(authoring, result)
      .fold(problem => fail(problem.toString), identity)
    val artifact = result.artifact.getOrElse(fail("expected produced artifact"))
    val directSearch = PostgreSqlEnumerationSearchReplayV1Json.project(artifact)

    assert(pair.artifact eq artifact)
    assertEquals(pair.packageAssembly.document.bytes.toVector, legacy.document.bytes.toVector)
    assertEquals(pair.searchInput.bytes.toVector, directSearch.bytes.toVector)
    assertEquals(
      pair.packageAssembly.projection.actions.size,
      artifact.batch.candidates.size + 1
    )

    val search = ujson.read(pair.searchInput.utf8)
    val contract = ujson.read(
      pair.packageAssembly.projection.producerSearchContract.utf8
    )
    assertEquals(
      search("source_sql_sha256").str,
      contract("source_sql_sha256").str
    )
    assertEquals(
      search("requested_k").num.toInt,
      artifact.request.planLimit
    )
    assertEquals(
      search("width_bound").num.toInt,
      contract("search")("width_bound").num.toInt
    )
    assertEquals(
      search("require_connected_cover").bool,
      contract("search")("require_connected_cover").bool
    )
    assertEquals(search("candidate_limits"), contract("limits")("candidate"))
    assertEquals(search("soft_limits"), contract("limits")("soft"))
  }

  test("typed native fallback is intentionally outside the paired boundary") {
    val result = produce(triangleSql, width = 1, k = 3)
    assertEquals(result.artifact, None)
    val failure = PostgreSqlEnumerationSearchReplayBundleV1Acquisition
      .assembleFoundPair(requestFor(result), result)
      .left
      .toOption
      .getOrElse(fail("fallback unexpectedly produced a search pair"))
    assertEquals(
      failure.code,
      "enumeration-campaign-v2.acquisition.unpackageable-result"
    )
  }

  test("authoring mismatch fails before either paired projection") {
    val result = produce(triangleSql, width = 2, k = 2)
    val mismatched = authoring(
      result.request.originalSql,
      width = 2,
      k = 1,
      connected = true
    )
    val failure = PostgreSqlEnumerationSearchReplayBundleV1Acquisition
      .assembleFoundPair(mismatched, result)
      .left
      .toOption
      .getOrElse(fail("mismatched authoring unexpectedly produced a pair"))
    assertEquals(
      failure.code,
      "enumeration-campaign-v2.acquisition.authoring-mismatch"
    )
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
            PostgreSqlEnumerationSearchReplayV1AcquisitionCli.Entrypoint,
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

  private def produce(
      sql: String,
      width: Int,
      k: Int
  ): PostgreSqlEnumerationBatchResult = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(value => Right(fixture.plan(value).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
    val request = PostgreSqlEnumerationBatchRequest
      .create(sql, widthBound = width, planLimit = k)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    rewriter.produceEnumerationBatch(request)
  }
}
