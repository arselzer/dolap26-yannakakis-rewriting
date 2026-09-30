package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.{Files, Path, Paths}

import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlExplainCostBatch,
  PostgreSqlExplainCostBatchRequest,
  PostgreSqlExplainCostEstimate,
  PostgreSqlExplainCostEvaluationFailure,
  PostgreSqlExplainCostEvaluator,
  PostgreSqlExplainCostFailureCode,
  PostgreSqlExplainCostObservation,
  PostgreSqlExplainCostProvenance,
  PostgreSqlExplainEvaluationStage,
  PostgreSqlExplainSelectionLimits,
  PostgreSqlExplainSourceEvaluation,
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteResult
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import com.fasterxml.jackson.core.{JsonFactory, JsonParser}

class PostgreSqlExplainGuardedJsonSuite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  private val provenance = PostgreSqlExplainCostProvenance(
    serverVersionNum = 180004,
    plannerSettingsFingerprintSha256 = "c" * 64,
    plannerSettingsFingerprintScheme =
      PostgreSqlExplainCostProvenance.PlannerSettingsFingerprintScheme,
    plannerSettingCount = PostgreSqlExplainCostProvenance.PlannerSettingNames.size
  )

  test("v4 guarded selection serializes exact source, candidate, guard, and provenance evidence") {
    val request = guardedRequest()
    val result = rewriter(success(
      sourceRows = BigDecimal(100),
      costs = Vector("0.01", "5", "5.0", "8").map(BigDecimal(_)),
      rows = Vector("1001", "200", "200", "9.99").map(BigDecimal(_))
    )).rewriteDetailed(request)
    assert(result.outcome.isInstanceOf[RewriteOutcome.Rewritten], clues(result))
    val rendered = PostgreSqlRewriteJson.render(request, result)
    assertV4(rendered)
    assertNoDuplicateKeys(rendered)

    val document = ujson.read(rendered)
    val selection = document("report")("candidate_selection")
    val selected = document("report")("selected_plan")
    assertEquals(document("schema_version").str, "yannakakis-postgresql-rewrite/v4")
    assertEquals(
      document("request")("options")("candidate_selection")("policy").str,
      "postgresql_explain_row_guarded_total_cost_v1"
    )
    assertEquals(selection("decision").str, "selected")
    assertEquals(selection("cost_selected").bool, true)
    assertEquals(selection("estimated_cost_selected").bool, true)
    assertEquals(selection("measured_runtime_selected").bool, false)
    assertEquals(selection("uses_catalog_statistics").bool, true)
    assertEquals(selection("uses_execution_feedback").bool, false)
    assertEquals(
      selection("cost_order").arr.map(_.str).toVector,
      Vector("plan_total_cost", "structural_rank", "candidate_id")
    )
    assertEquals(selection("explain_failure_stage"), ujson.Null)
    assertEquals(selection("source_optimizer_estimate")("optimizer_estimate")("plan_rows").num, 100.0)
    assert(!selection("source_optimizer_estimate").obj.contains("sql"))
    assertEquals(selection("evaluations").arr.size, 4)
    assertEquals(
      selection("evaluations").arr.map(_("row_guard")("within_guard").bool).toVector,
      Vector(false, true, true, false)
    )
    assertEquals(
      selection("evaluations")(1)("row_guard")("ratio_numerator").num,
      200.0
    )
    assertEquals(
      selection("evaluations")(1)("row_guard")("ratio_denominator").num,
      100.0
    )
    assertEquals(selection("selected_rank").num.toInt, 2)
    assertEquals(selected("rank").num.toInt, 2)
    assertEquals(selected("row_guard")("within_guard").bool, true)
    assertEquals(
      selected("source_optimizer_estimate"),
      selection("source_optimizer_estimate")
    )

    val allFalse = ujson.read(rendered)
    allFalse("report")("candidate_selection")("evaluations").arr.foreach(
      _("row_guard")("within_guard") = false
    )
    assert(TestJsonSchemaValidator.validate(allFalse, v4Schema).nonEmpty)

    val selectedPlanFalse = ujson.read(rendered)
    selectedPlanFalse("report")("selected_plan")("row_guard")("within_guard") = false
    assert(TestJsonSchemaValidator.validate(selectedPlanFalse, v4Schema).nonEmpty)

    val contradictoryDecision = ujson.read(rendered)
    contradictoryDecision("report")("candidate_selection")("decision") =
      "no_admissible_estimate"
    assert(TestJsonSchemaValidator.validate(contradictoryDecision, v4Schema).nonEmpty)
  }

  test("v4 no-admissible result retains all false guards and exposes no winner") {
    val request = guardedRequest()
    val result = rewriter(success(
      sourceRows = BigDecimal(100),
      costs = Vector(1, 2, 3, 4).map(BigDecimal(_)),
      rows = Vector("1001", "2000", "0", "9.99").map(BigDecimal(_))
    )).rewriteDetailed(request)
    assert(result.outcome.isInstanceOf[RewriteOutcome.Failed], clues(result))
    assertEquals(result.report.artifact, None)
    val rendered = PostgreSqlRewriteJson.render(request, result)
    assertV4(rendered)
    assertNoDuplicateKeys(rendered)

    val document = ujson.read(rendered)
    val selection = document("report")("candidate_selection")
    assertEquals(document("status").str, "failed")
    assertEquals(selection("decision").str, "no_admissible_estimate")
    assertEquals(selection("evaluated_candidate_count").num.toInt, 4)
    assert(selection("evaluations").arr.forall(value =>
      !value("row_guard")("within_guard").bool
    ))
    assert(selection("source_optimizer_estimate") != ujson.Null)
    assertEquals(selection("selected_rank"), ujson.Null)
    assertEquals(selection("selected_candidate_id"), ujson.Null)
    assertEquals(selection("failed_rank"), ujson.Null)
    assertEquals(document("report")("selected_plan"), ujson.Null)
    assertEquals(document("report")("bag_factor_plan"), ujson.Null)
    assertEquals(document("report")("rendering"), ujson.Null)
  }

  test("v4 source and candidate EXPLAIN failures have strict stage-specific evidence") {
    val request = guardedRequest()
    val sourceFailed = rewriter(new FunctionEvaluator(_ =>
      Left(
        PostgreSqlExplainCostEvaluationFailure(
          PostgreSqlExplainCostFailureCode.StatementTimedOut,
          PostgreSqlExplainEvaluationStage.SourceExplain,
          None,
          None,
          Vector.empty
        )
      )
    )).rewriteDetailed(request)
    val sourceJson = PostgreSqlRewriteJson.render(request, sourceFailed)
    assertV4(sourceJson)
    assertNoDuplicateKeys(sourceJson)
    val sourceSelection = ujson.read(sourceJson)("report")("candidate_selection")
    assertEquals(sourceSelection("decision").str, "explain_evaluation_failed")
    assertEquals(sourceSelection("explain_failure_stage").str, "source-explain")
    assertEquals(sourceSelection("source_optimizer_estimate"), ujson.Null)
    assertEquals(sourceSelection("evaluations").arr.size, 0)
    assertEquals(sourceSelection("failed_rank"), ujson.Null)

    val candidateFailed = rewriter(new FunctionEvaluator(batch => {
      val source = sourceEvaluation(batch, BigDecimal(100))
      Left(
        PostgreSqlExplainCostEvaluationFailure(
          PostgreSqlExplainCostFailureCode.StatementFailed,
          PostgreSqlExplainEvaluationStage.CandidateExplain,
          Some(1),
          Some(source),
          Vector(
            observation(
              batch,
              index = 0,
              estimate(total = BigDecimal(2), rows = BigDecimal(100))
            )
          )
        )
      )
    })).rewriteDetailed(request)
    val candidateJson = PostgreSqlRewriteJson.render(request, candidateFailed)
    assertV4(candidateJson)
    assertNoDuplicateKeys(candidateJson)
    val candidateSelection =
      ujson.read(candidateJson)("report")("candidate_selection")
    assertEquals(
      candidateSelection("decision").str,
      "explain_evaluation_failed"
    )
    assertEquals(
      candidateSelection("explain_failure_stage").str,
      "candidate-explain"
    )
    assert(candidateSelection("source_optimizer_estimate") != ujson.Null)
    assertEquals(candidateSelection("evaluations").arr.size, 1)
    assertEquals(candidateSelection("evaluations")(0)("rank").num.toInt, 1)
    assertEquals(candidateSelection("failed_rank").num.toInt, 2)
    assertEquals(candidateSelection("selected_rank"), ujson.Null)
    assertEquals(ujson.read(candidateJson)("report")("selected_plan"), ujson.Null)

    val wrongStage = ujson.read(candidateJson)
    wrongStage("report")("candidate_selection")("explain_failure_stage") =
      "source-explain"
    assert(TestJsonSchemaValidator.validate(wrongStage, v4Schema).nonEmpty)
  }

  test("v4 renders huge exact exponents compactly without decimal expansion") {
    val request = guardedRequest()
    val largeRows = BigDecimal("9007199254740993.125")
    val scaleBoundary = BigDecimal("100e2147483647")
    val result = rewriter(success(
      sourceRows = largeRows,
      costs = Vector(
        scaleBoundary,
        BigDecimal("1e400"),
        BigDecimal("2"),
        BigDecimal("3")
      ),
      rows = Vector.fill(4)(largeRows)
    )).rewriteDetailed(request)
    val rendered = PostgreSqlRewriteJson.render(request, result)

    assertEquals(scaleBoundary.bigDecimal.toString, "1.00E+2147483649")
    assert(rendered.contains("\"plan_total_cost\":1.00E+2147483649"), rendered)
    assert(rendered.contains("\"plan_total_cost\":1E+400"), rendered)
    assert(
      rendered.contains("\"plan_rows\":9007199254740993.125"),
      rendered
    )
    assert(rendered.length < 1000000, clues(rendered.length))
    assertNoDuplicateKeys(rendered)
  }

  test("v4 schema and emitted documents contain no duplicate object keys") {
    assertNoDuplicateKeys(Files.readString(v4SchemaPath))
    val failure = PostgreSqlRewriteJson.renderFailure(
      PostgreSqlRewriteCliFailure.invocation("safe failure")
    )
    assertV4(failure)
    assertNoDuplicateKeys(failure)

    intercept[com.fasterxml.jackson.core.JsonParseException] {
      assertNoDuplicateKeys("{\"duplicate\":1,\"duplicate\":2}")
    }
  }

  private final class FunctionEvaluator(
      response: PostgreSqlExplainCostBatchRequest => Either[
        PostgreSqlExplainCostEvaluationFailure,
        PostgreSqlExplainCostBatch
      ]
  ) extends PostgreSqlExplainCostEvaluator {
    override def evaluate(
        request: PostgreSqlExplainCostBatchRequest,
        limits: PostgreSqlExplainSelectionLimits
    ): Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch] =
      response(request)
  }

  private def success(
      sourceRows: BigDecimal,
      costs: Vector[BigDecimal],
      rows: Vector[BigDecimal]
  ): PostgreSqlExplainCostEvaluator =
    new FunctionEvaluator(batch => {
      assertEquals(batch.statements.map(_.structuralRank), Vector(1, 2, 3, 4))
      assertEquals(costs.size, batch.statements.size)
      assertEquals(rows.size, batch.statements.size)
      Right(
        PostgreSqlExplainCostBatch(
          provenance,
          sourceEvaluation(batch, sourceRows),
          batch.statements.indices.toVector.map(index =>
            observation(batch, index, estimate(costs(index), rows(index)))
          )
        )
      )
    })

  private def sourceEvaluation(
      batch: PostgreSqlExplainCostBatchRequest,
      rows: BigDecimal
  ): PostgreSqlExplainSourceEvaluation =
    PostgreSqlExplainSourceEvaluation(
      batch.source.sourceStatementSha256,
      estimate(total = BigDecimal(10), rows = rows)
    )

  private def observation(
      batch: PostgreSqlExplainCostBatchRequest,
      index: Int,
      value: PostgreSqlExplainCostEstimate
  ): PostgreSqlExplainCostObservation = {
    val statement = batch.statements(index)
    PostgreSqlExplainCostObservation(
      statement.candidateId,
      statement.structuralRank,
      statement.physicalVariantId,
      statement.renderedStatementSha256,
      value
    )
  }

  private def estimate(
      total: BigDecimal,
      rows: BigDecimal
  ): PostgreSqlExplainCostEstimate =
    PostgreSqlExplainCostEstimate(
      startupCost = BigDecimal("0.125"),
      planTotalCost = total,
      planRows = rows,
      planWidth = 32,
      provenance = provenance
    )

  private def guardedRequest(): RewriteRequest =
    RewriteRequest(
      triangleSql,
      options = RewriteOptions(
        widthBound = 2,
        planLimit = 10,
        candidateSelection =
          CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1,
        reductionMode = YannakakisReductionMode.NoReduction
      )
    )

  private def rewriter(
      evaluator: PostgreSqlExplainCostEvaluator
  ): PostgreSqlQueryRewriter = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = Some(evaluator))
      .fold(problem => fail(problem.message), identity)
  }

  private lazy val v4SchemaPath: Path = repositoryFile(
    "docs/journal-extension/schemas/postgresql-rewrite-artifact-v4.schema.json"
  )

  private lazy val v4Schema: ujson.Value =
    ujson.read(Files.readString(v4SchemaPath))

  private def assertV4(rendered: String): Unit = {
    val errors = TestJsonSchemaValidator.validate(ujson.read(rendered), v4Schema)
    assert(
      errors.isEmpty,
      errors.map(error => s"${error.path}: ${error.message}").mkString("\n")
    )
  }

  private def assertNoDuplicateKeys(json: String): Unit = {
    val parser = new JsonFactory()
      .enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
      .createParser(json)
    try while (parser.nextToken() != null) ()
    finally parser.close()
  }

  private def repositoryFile(relative: String): Path = {
    var current = Paths.get("").toAbsolutePath.normalize()
    while (current != null) {
      val candidate = current.resolve(relative)
      if (Files.isRegularFile(candidate)) return candidate
      current = current.getParent
    }
    fail(s"could not locate repository file $relative")
  }
}
