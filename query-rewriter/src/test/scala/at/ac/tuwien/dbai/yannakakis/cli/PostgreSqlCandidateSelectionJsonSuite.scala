package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.{Files, Paths}

import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCandidateSelectionDecision,
  PostgreSqlPhysicalVariantId,
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteArtifact,
  PostgreSqlRewriteResult,
  PostgreSqlStaticShapeScore
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlRenderingLimits
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

class PostgreSqlCandidateSelectionJsonSuite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("v4 default-head JSON is byte deterministic and explicit about selection") {
    val rewriter = standardRewriter()
    val request = RewriteRequest(
      triangleSql,
      options = RewriteOptions(widthBound = 2, planLimit = 10)
    )
    val first = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )
    val second = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )

    assertEquals(second, first)
    assert(!first.contains('\n'))
    assert(!first.contains('\r'))
    assertV4Schema(first)
    val document = ujson.read(first)
    assertEquals(document("schema_version").str, "yannakakis-postgresql-rewrite/v4")
    val requested = document("request")("options")("candidate_selection")
    assertEquals(requested("policy").str, "structural_top_k_head_v1")
    assertEquals(requested("requested_rank"), ujson.Null)
    assertEquals(requested("requested_candidate_id"), ujson.Null)

    val search = document("report")("candidate_search")
    val selection = document("report")("candidate_selection")
    val selected = document("report")("selected_plan")
    assertEquals(
      search("candidate_id_scheme").str,
      "covered_decomposition_json_v1"
    )
    assertEquals(selection("decision").str, "selected")
    assertEquals(selection("policy").str, "structural_top_k_head_v1")
    assertEquals(selection("evaluated_candidate_count").num.toInt, 1)
    assertEquals(selection("evaluations").arr.size, 1)
    assertEquals(selection("selected_rank").num.toInt, 1)
    assertEquals(
      selection("selected_candidate_id").str,
      search("plans")(0)("candidate_id").str
    )
    assertEquals(selected("rank").num.toInt, 1)
    assertEquals(selected("candidate_id").str, search("plans")(0)("candidate_id").str)
    assertEquals(selected("decomposition"), search("plans")(0)("decomposition"))
    assertEquals(selected("physical_shape_score"), selection("evaluations")(0)("physical_shape_score"))
    assertEquals(document("request")("options")("reduction_schedule").str, "full")
    assertEquals(selection("reduction_schedule").str, "full")
    assertEquals(selection("reduction_order").str, "canonical_plan_schedule_v1")
    assertEquals(
      selection("selected_physical_variant_id").str,
      selected("physical_variant_id").str
    )
    assertEquals(
      selection("selected_physical_variant_id").str,
      selection("evaluations")(0)("physical_variant_id").str
    )
    assertNonCostSelection(selection)
    assertNonCostSelection(selected)

    val invalid = ujson.read(first)
    invalid("report")("selected_plan")("rank") = 0
    assert(TestJsonSchemaValidator.validate(invalid, v4Schema).nonEmpty)

    val unknown = ujson.read(first)
    unknown("report")("rendering")("unexpected") = true
    assert(TestJsonSchemaValidator.validate(unknown, v4Schema).nonEmpty)

    val wrongMode = ujson.read(first)
    wrongMode("report")("candidate_selection")("reduction_schedule") = "FULL"
    assert(TestJsonSchemaValidator.validate(wrongMode, v4Schema).nonEmpty)

    val missingPhysicalId = ujson.read(first)
    missingPhysicalId("report")("selected_plan").obj.remove("physical_variant_id")
    assert(TestJsonSchemaValidator.validate(missingPhysicalId, v4Schema).nonEmpty)
  }

  test("v4 success is deterministic and internally consistent for every reduction mode") {
    val rewriter = standardRewriter()
    YannakakisReductionMode.Values.foreach { mode =>
      val request = RewriteRequest(
        triangleSql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = 10,
          reductionMode = mode
        )
      )
      val firstDetailed = rewriter.rewriteDetailed(request)
      val selectedArtifact = successfulArtifact(firstDetailed)
      val first = PostgreSqlRewriteJson.render(request, firstDetailed)
      val repeated = PostgreSqlRewriteJson.render(
        request,
        rewriter.rewriteDetailed(request)
      )
      assertEquals(repeated, first)
      assertV4Schema(first)

      val document = ujson.read(first)
      val selection = document("report")("candidate_selection")
      val evaluation = selection("evaluations")(0)
      val selected = document("report")("selected_plan")
      val rendering = document("report")("rendering")
      val physicalId = PostgreSqlPhysicalVariantId
        .from(selectedArtifact.selectedPlan.candidateId, mode)
        .value

      assertEquals(document("request")("options")("reduction_schedule").str, mode.id)
      assertEquals(selection("reduction_schedule").str, mode.id)
      assertEquals(evaluation("reduction_schedule").str, mode.id)
      assertEquals(selected("reduction_schedule").str, mode.id)
      assertEquals(rendering("reduction_schedule").str, mode.id)
      assertEquals(selection("selected_physical_variant_id").str, physicalId)
      assertEquals(evaluation("physical_variant_id").str, physicalId)
      assertEquals(selected("physical_variant_id").str, physicalId)
      assertEquals(selection("reduction_order").str, "canonical_plan_schedule_v1")
      assertEquals(rendering("reduction_order").str, "canonical_plan_schedule_v1")
      mode match {
        case YannakakisReductionMode.FullTwoPass =>
          assertEquals(
            rendering("selected_top_down_step_count").num.toInt,
            rendering("canonical_top_down_step_count").num.toInt
          )
        case YannakakisReductionMode.BottomUpOnly =>
          assertEquals(rendering("selected_top_down_step_count").num.toInt, 0)
          assertEquals(
            rendering("selected_bottom_up_step_count").num.toInt,
            rendering("canonical_bottom_up_step_count").num.toInt
          )
        case YannakakisReductionMode.NoReduction =>
          assertEquals(rendering("selected_bottom_up_step_count").num.toInt, 0)
          assertEquals(rendering("selected_top_down_step_count").num.toInt, 0)
          assertEquals(rendering("physical_reduction_cte_count").num.toInt, 0)
      }
    }
  }

  test("serializer takes a retained non-head selection from the artifact") {
    val rewriter = standardRewriter()
    val request = RewriteRequest(
      triangleSql,
      options = RewriteOptions(
        widthBound = 2,
        planLimit = 10,
        candidateSelection = CandidatePlanSelection.StructuralRank(4)
      )
    )
    val detailed = rewriter.rewriteDetailed(request)
    val artifact = successfulArtifact(detailed)
    assertEquals(artifact.selectedPlan.rank, 4)
    val rendered = PostgreSqlRewriteJson.render(request, detailed)
    val repeated = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )
    assertEquals(repeated, rendered)
    assertV4Schema(rendered)

    val document = ujson.read(rendered)
    val requested = document("request")("options")("candidate_selection")
    assertEquals(requested("policy").str, "explicit_structural_rank_v1")
    assertEquals(requested("requested_rank").num.toInt, 4)
    assertEquals(requested("requested_candidate_id"), ujson.Null)

    val search = document("report")("candidate_search")
    val plan = search("plans")(3)
    val selection = document("report")("candidate_selection")
    val selected = document("report")("selected_plan")
    assertEquals(selection("policy").str, "explicit_structural_rank_v1")
    assertEquals(selection("evaluated_candidate_count").num.toInt, 1)
    assertEquals(selection("evaluations")(0)("rank").num.toInt, 4)
    assertEquals(selection("selected_rank").num.toInt, 4)
    assertEquals(selection("selected_candidate_id").str, plan("candidate_id").str)
    assertEquals(selected("selection_policy").str, "explicit_structural_rank_v1")
    assertEquals(selected("rank").num.toInt, 4)
    assertEquals(selected("candidate_id").str, plan("candidate_id").str)
    assertEquals(selected("decomposition"), plan("decomposition"))
    assertEquals(
      selected("physical_shape_score"),
      selection("evaluations")(0)("physical_shape_score")
    )
    assertEquals(
      selection("evaluations")(0)("rendered_statement_sha256").str,
      document("outcome")("rewritten_statement_sha256")(0).str
    )
  }

  test("static-shape JSON preserves all evaluations in structural rank order") {
    val rewriter = standardRewriter()
    val request = RewriteRequest(
      triangleSql,
      options = RewriteOptions(
        planLimit = 10,
        candidateSelection = CandidatePlanSelection.PostgreSqlStaticShapeV1
      )
    )
    val detailed = rewriter.rewriteDetailed(request)
    val artifact = successfulArtifact(detailed)
    val rendered = PostgreSqlRewriteJson.render(request, detailed)
    val repeated = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )
    assertEquals(repeated, rendered)
    assertV4Schema(rendered)
    val document = ujson.read(rendered)
    val selection = document("report")("candidate_selection")
    val evaluations = selection("evaluations").arr

    assertEquals(
      selection("policy").str,
      "postgresql_static_shape_lexicographic_v1"
    )
    assertEquals(
      selection("score_order").arr.map(_.str).toVector,
      PostgreSqlStaticShapeScore.OrderedFields
    )
    assertEquals(evaluations.size, artifact.candidateSearch.plans.size)
    assertEquals(
      evaluations.map(_("rank").num.toInt).toVector,
      artifact.candidateSearch.plans.map(_.rank)
    )
    assertEquals(
      evaluations.map(_("candidate_id").str).toVector,
      artifact.candidateSearch.plans.map(_.candidateId.value)
    )
    assertEquals(
      selection("selected_candidate_id").str,
      artifact.selectedPlan.candidateId.value
    )
    assertEquals(
      document("report")("selected_plan")("candidate_id").str,
      artifact.selectedPlan.candidateId.value
    )
    assertNonCostSelection(selection)
  }

  test("unavailable exact selection serializes failure evidence and no winner") {
    val rewriter = standardRewriter()
    val request = RewriteRequest(
      triangleSql,
      options = RewriteOptions(
        planLimit = 10,
        candidateSelection = CandidatePlanSelection.StructuralRank(5),
        reductionMode = YannakakisReductionMode.BottomUpOnly
      )
    )
    val detailed = rewriter.rewriteDetailed(request)
    assert(detailed.outcome.isInstanceOf[RewriteOutcome.Failed])
    detailed.report.candidateSelectionDecision match {
      case Some(_: PostgreSqlCandidateSelectionDecision.Unavailable) => ()
      case other => fail(s"expected unavailable selection, received $other")
    }
    val rendered = PostgreSqlRewriteJson.render(request, detailed)
    val repeatedUnavailable = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )
    assertEquals(repeatedUnavailable, rendered)
    assertV4Schema(rendered)
    val document = ujson.read(rendered)
    val requested = document("request")("options")("candidate_selection")
    val selection = document("report")("candidate_selection")

    assertEquals(document("schema_version").str, "yannakakis-postgresql-rewrite/v4")
    assertEquals(document("status").str, "failed")
    assertEquals(requested("requested_rank").num.toInt, 5)
    assertEquals(selection("decision").str, "unavailable")
    assertEquals(selection("evaluated_candidate_count").num.toInt, 0)
    assertEquals(selection("evaluations").arr.toVector, Vector.empty)
    assertEquals(selection("selected_rank"), ujson.Null)
    assertEquals(selection("selected_candidate_id"), ujson.Null)
    assertEquals(selection("retained_candidate_count").num.toInt, 4)
    assertEquals(selection("reduction_schedule").str, "bottom-up")
    assertEquals(selection("selected_physical_variant_id"), ujson.Null)
    assertEquals(selection("failed_physical_variant_id"), ujson.Null)
    assertEquals(document("report")("selected_plan"), ujson.Null)
    assertEquals(document("report")("bag_factor_plan"), ujson.Null)
    assertEquals(document("report")("rendering"), ujson.Null)
  }

  test("evaluation failure JSON retains completed evaluations but no winner") {
    val renderingLimits = PostgreSqlRenderingLimits
      .create(maxExpandedScans = 3L)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val rewriter = standardRewriter(renderingLimits)
    val request = RewriteRequest(
      triangleSql,
      options = RewriteOptions(
        planLimit = 10,
        candidateSelection = CandidatePlanSelection.PostgreSqlStaticShapeV1,
        reductionMode = YannakakisReductionMode.NoReduction
      )
    )
    val detailed = rewriter.rewriteDetailed(request)
    assert(detailed.outcome.isInstanceOf[RewriteOutcome.Failed])
    detailed.report.candidateSelectionDecision match {
      case Some(_: PostgreSqlCandidateSelectionDecision.EvaluationFailed) => ()
      case other => fail(s"expected evaluation failure, received $other")
    }
    val rendered = PostgreSqlRewriteJson.render(request, detailed)
    val repeatedFailure = PostgreSqlRewriteJson.render(
      request,
      rewriter.rewriteDetailed(request)
    )
    assertEquals(repeatedFailure, rendered)
    assertV4Schema(rendered)
    val document = ujson.read(rendered)
    val search = document("report")("candidate_search")
    val selection = document("report")("candidate_selection")

    assertEquals(selection("decision").str, "evaluation_failed")
    assertEquals(
      selection("policy").str,
      "postgresql_static_shape_lexicographic_v1"
    )
    assertEquals(selection("evaluated_candidate_count").num.toInt, 1)
    assertEquals(selection("evaluations").arr.size, 1)
    assertEquals(selection("evaluations")(0)("rank").num.toInt, 1)
    assertEquals(
      selection("evaluations")(0)("candidate_id").str,
      search("plans")(0)("candidate_id").str
    )
    assertEquals(selection("failed_rank").num.toInt, 2)
    assertEquals(
      selection("failed_candidate_id").str,
      search("plans")(1)("candidate_id").str
    )
    assertEquals(selection("reduction_schedule").str, "none")
    assertEquals(
      selection("failed_physical_variant_id").str,
      PostgreSqlPhysicalVariantId
        .from(
          artifactCandidateId(search("plans")(1)("candidate_id").str),
          YannakakisReductionMode.NoReduction
        )
        .value
    )
    assert(selection("evaluations").arr.forall(_("reduction_schedule").str == "none"))
    assertEquals(selection("selected_rank"), ujson.Null)
    assertEquals(selection("selected_candidate_id"), ujson.Null)
    assertEquals(selection("retained_candidate_count"), ujson.Null)
    assertEquals(document("report")("selected_plan"), ujson.Null)
    assertEquals(document("report")("rendering"), ujson.Null)
    assertNonCostSelection(selection)
  }

  private def assertNonCostSelection(value: ujson.Value): Unit = {
    assertEquals(value("cost_selected").bool, false)
    assertEquals(value("estimated_cost_selected").bool, false)
    assertEquals(value("uses_catalog_statistics").bool, false)
    assertEquals(value("uses_execution_feedback").bool, false)
    assertEquals(
      value("selection_scope").str,
      "retained_structural_top_k_first_canonical_cover_v1"
    )
  }

  private lazy val v4Schema: ujson.Value = {
    val relative =
      "docs/journal-extension/schemas/postgresql-rewrite-artifact-v4.schema.json"
    var current = Paths.get("").toAbsolutePath.normalize()
    var found: Option[java.nio.file.Path] = None
    while (current != null && found.isEmpty) {
      val candidate = current.resolve(relative)
      if (Files.isRegularFile(candidate)) found = Some(candidate)
      current = current.getParent
    }
    val path = found.getOrElse(fail(s"could not locate repository file $relative"))
    ujson.read(Files.readString(path))
  }

  private def assertV4Schema(rendered: String): Unit = {
    val errors = TestJsonSchemaValidator.validate(ujson.read(rendered), v4Schema)
    assert(
      errors.isEmpty,
      errors.map(error => s"${error.path}: ${error.message}").mkString("\n")
    )
  }

  private def artifactCandidateId(value: String) =
    at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
      .parse(value)
      .fold(problem => fail(problem.message), identity)

  private def standardRewriter(
      renderingLimits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default
  ): PostgreSqlQueryRewriter = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    PostgreSqlQueryRewriter
      .create(planner, renderingLimits = renderingLimits)
      .fold(problem => fail(problem.message), identity)
  }

  private def successfulArtifact(
      detailed: PostgreSqlRewriteResult
  ): PostgreSqlRewriteArtifact = {
    assert(detailed.outcome.isInstanceOf[RewriteOutcome.Rewritten])
    detailed.report.artifact.getOrElse(fail("successful rewrite has no artifact"))
  }
}
