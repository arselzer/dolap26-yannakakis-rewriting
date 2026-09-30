package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.{
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteSqlPlanner,
  CalciteSqlPlanningError,
  CalciteSqlPlanningStage
}

class PostgreSqlRewriteJsonSuite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("success JSON is byte-deterministic and exposes ranked structural evidence") {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
    val request = RewriteRequest(
      triangleSql,
      options = RewriteOptions(widthBound = 2, planLimit = 4)
    )
    val detailed = rewriter.rewriteDetailed(request)

    assert(detailed.outcome.isInstanceOf[RewriteOutcome.Rewritten])
    val first = PostgreSqlRewriteJson.render(request, detailed)
    val second = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )
    assertEquals(second, first)
    assert(!first.contains('\n'))
    assert(!first.contains('\r'))

    val document = ujson.read(first)
    assertEquals(
      document("schema_version").str,
      PostgreSqlRewriteJson.SchemaVersion
    )
    assertEquals(document("status").str, "rewritten")
    assertEquals(document("exit_code").num.toInt, 0)
    assertEquals(document("request")("original_sql").str, triangleSql)
    assertEquals(
      document("request")("options")("reduction_schedule").str,
      YannakakisReductionMode.Default.id
    )
    assertEquals(
      document("report")("selected_plan")("selection_policy").str,
      "structural_top_k_head_v1"
    )
    assertEquals(
      document("report")("selected_plan")("cost_selected").bool,
      false
    )
    assertEquals(document("report")("selected_plan")("rank").num.toInt, 1)

    val search = document("report")("candidate_search")
    assertEquals(search("decision").str, "found")
    assertEquals(search("requested_limit").num.toInt, 4)
    assert(search("plans").arr.nonEmpty)
    assertEquals(
      search("plans")(0)("candidate_id").str,
      document("report")("selected_plan")("candidate_id").str
    )
    assert(
      search("plans")(0)("candidate_id").str.matches("sha256:[0-9a-f]{64}")
    )

    val decomposition = document("report")("selected_plan")("decomposition")
    val bagIds = decomposition("bags").arr.map(_("id").num.toInt)
    assertEquals(bagIds, bagIds.sorted)
    decomposition("bags").arr.foreach { bag =>
      val vertices = bag("vertices").arr.map(_.num.toInt)
      val cover = bag("cover_edge_ids").arr.map(_.num.toInt)
      assertEquals(vertices, vertices.sorted)
      assertEquals(cover, cover.sorted)
    }
    assert(document("report")("bag_factor_plan") != ujson.Null)
    assert(document("report")("rendering")("cte_count").num > 0)
    val selection = document("report")("candidate_selection")
    val selected = document("report")("selected_plan")
    val rendering = document("report")("rendering")
    assertEquals(selection("reduction_schedule").str, "full")
    assertEquals(selection("reduction_order").str, "canonical_plan_schedule_v1")
    assert(selection("selected_physical_variant_id").str.matches("sha256:[0-9a-f]{64}"))
    assertEquals(
      selection("selected_physical_variant_id").str,
      selected("physical_variant_id").str
    )
    assertEquals(rendering("reduction_schedule").str, "full")
    assertEquals(
      rendering("selected_bottom_up_step_count").num.toInt,
      rendering("canonical_bottom_up_step_count").num.toInt
    )
    assertEquals(
      rendering("selected_top_down_step_count").num.toInt,
      rendering("canonical_top_down_step_count").num.toInt
    )
  }

  test("invalid-query JSON preserves exact SQL and a partial report") {
    val rewriter = rewriterWithPlanningFailure(
      CalciteSqlPlanningStage.Parsing,
      None
    )
    val original = " \r\n\t "
    val request = RewriteRequest(original)
    val rendered = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )
    val document = ujson.read(rendered)

    assertEquals(document("status").str, "invalid_query")
    assertEquals(document("exit_code").num.toInt, 3)
    assertEquals(document("request")("original_sql").str, original)
    assertEquals(
      document("report")("completed_phase").str,
      "request-accepted"
    )
    assertEquals(document("report")("analysis"), ujson.Null)
    assert(!rendered.contains('\n'))
    assert(!rendered.contains('\r'))
  }

  test("diagnostic context order is canonical") {
    val failure = PostgreSqlRewriteCliFailure.invocation(
      "bad input",
      Map("z" -> "last", "a" -> "first")
    )
    val rendered = PostgreSqlRewriteJson.renderFailure(failure)

    assertEquals(
      rendered,
      "{" +
        "\"schema_version\":\"yannakakis-postgresql-rewrite/v4\"," +
        "\"status\":\"invalid_invocation\",\"exit_code\":64," +
        "\"request\":null,\"outcome\":{" +
        "\"rewritten_statements\":[]," +
        "\"rewritten_statement_sha256\":[]," +
        "\"diagnostics\":[{" +
        "\"code\":\"CLI_INVALID_INVOCATION\"," +
        "\"severity\":\"error\",\"message\":\"bad input\"," +
        "\"context\":{\"a\":\"first\",\"z\":\"last\"}}]}," +
        "\"report\":null}"
    )
  }

  test("connection, path, credential, and Throwable metadata are omitted") {
    val secrets = Vector(
      "jdbc:postgresql://db.internal/private?password=hunter2",
      "database-user-secret",
      "database-password-secret",
      "/private/work/query.sql",
      "private.DriverException"
    )
    val failure = PostgreSqlRewriteCliFailure.invocation(
      "Safe public message",
      Map(
        "jdbcUrl" -> secrets(0),
        "databaseUser" -> secrets(1),
        "password" -> secrets(2),
        "path" -> secrets(3),
        "exceptionClass" -> secrets(4),
        "stage" -> "configuration"
      )
    )
    val failureJson = PostgreSqlRewriteJson.renderFailure(failure)
    secrets.foreach(secret => assert(!failureJson.contains(secret)))
    assertEquals(
      ujson.read(failureJson)("outcome")("diagnostics")(0)("context")("stage").str,
      "configuration"
    )

    val rewriter = rewriterWithPlanningFailure(
      CalciteSqlPlanningStage.PlannerCreation,
      Some(secrets(4))
    )
    val request = RewriteRequest("SELECT 1")
    val detailedJson = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )
    assert(!detailedJson.contains(secrets(4)))
    assert(!detailedJson.contains("exceptionClass"))
  }

  test("JSON string encoding is compact and lossless for SQL characters") {
    val original = "SELECT '\"\\line\n" + 1.toChar + 0xd800.toChar + "'"
    val rewriter = rewriterWithPlanningFailure(
      CalciteSqlPlanningStage.Parsing,
      None
    )
    val request = RewriteRequest(original)
    val rendered = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )

    assert(!rendered.contains('\n'))
    assert(rendered.contains("\\n"))
    assert(rendered.contains("\\u0001"))
    assert(rendered.contains("\\ud800"))
  }

  private def rewriterWithPlanningFailure(
      stage: CalciteSqlPlanningStage,
      exceptionClass: Option[String]
  ): PostgreSqlQueryRewriter = {
    val planner = CalciteSqlPlanner
      .fromFunction(_ =>
        Left(
          CalciteSqlPlanningError(
            stage,
            "A deliberately ignored raw detail",
            exceptionClass
          )
        )
      )
      .fold(problem => fail(problem.message), identity)
    PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
  }
}
