package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateResourceLimits
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

private[cli] object PostgreSqlEnumerationSearchReplayV2TestSupport {
  val TriangleSql: String =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  val UnsupportedSql: String = "SELECT r.a FROM triangle_r AS r"

  def produce(
      sql: String = TriangleSql,
      width: Int = 2,
      k: Int = 2,
      candidateLimits: CandidateResourceLimits = CandidateResourceLimits.default
  ): PostgreSqlEnumerationBatchResult = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(value => Right(fixture.plan(value).root))
      .fold(problem => throw new AssertionError(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner, candidateLimits = candidateLimits)
      .fold(problem => throw new AssertionError(problem.toString), identity)
    val request = PostgreSqlEnumerationBatchRequest
      .create(sql, widthBound = width, planLimit = k)
      .fold(
        problems =>
          throw new AssertionError(problems.map(_.code).mkString("; ")),
        identity
      )
    rewriter.produceEnumerationBatch(request)
  }

  def authoring(
      result: PostgreSqlEnumerationBatchResult,
      producerEntrypoint: String =
        PostgreSqlEnumerationSearchReplayV2AcquisitionCli.Entrypoint
  ): PostgreSqlEnumerationCampaignV2AuthoringRequest =
    PostgreSqlEnumerationCampaignV2AuthoringRequest
      .create(
        PostgreSqlEnumerationCampaignV2AuthoringInput(
          sourceSql = result.request.originalSql,
          workloadId = "triangle-cell-v2",
          lineageId = "triangle-lineage-v2",
          widthBound = result.request.widthBound,
          requestedK = result.request.planLimit,
          requireConnectedCover = result.request.requireConnectedCover,
          dataContract = PostgreSqlEnumerationCampaignV2DataContract(
            "triangle-fixture-v2",
            "a" * 64,
            "b" * 64,
            "c" * 64
          ),
          producerEntrypoint = producerEntrypoint,
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
      .fold(problem => throw new AssertionError(problem), identity)

  def classify(
      result: PostgreSqlEnumerationBatchResult,
      request: PostgreSqlEnumerationCampaignV2AuthoringRequest = null
  ): PostgreSqlEnumerationCampaignV2EligibleResult = {
    val exact = if (request == null) authoring(result) else request
    PostgreSqlEnumerationCampaignV2Packageability
      .classify(exact, result)
      .fold(problem => throw new AssertionError(problem.toString), identity)
  }
}
