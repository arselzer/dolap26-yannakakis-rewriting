package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlExplainCostEvaluator,
  PostgreSqlExplainCostFailureCode,
  PostgreSqlExplainEvaluationStage,
  PostgreSqlFactorAwareContractTestSupport,
  PostgreSqlFactorAwareExplainFailure,
  PostgreSqlFactorAwareSelectionReplay,
  PostgreSqlQueryRewriter
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import com.fasterxml.jackson.core.{JsonFactory, JsonParser}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlFactorAwareV5JsonContractSuite extends munit.FunSuite {
  import PostgreSqlFactorAwareContractTestSupport._

  private val policy =
    CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1

  test("the v5 test validator honors the Draft 2020-12 composition keywords in use") {
    val referenceSibling = ujson.read(
      """{
        |  "$defs":{"bounded":{"type":"integer","minimum":1}},
        |  "$ref":"#/$defs/bounded",
        |  "maximum":2
        |}""".stripMargin
    )
    assertValid(ujson.Num(2), referenceSibling)
    assertInvalid(ujson.Num(0), referenceSibling)
    assertInvalid(ujson.Num(3), referenceSibling)

    val arrayComposition = ujson.read(
      """{
        |  "type":"array",
        |  "prefixItems":[{"const":1}],
        |  "items":false,
        |  "contains":{"const":1},
        |  "minContains":1
        |}""".stripMargin
    )
    assertValid(ujson.Arr(1), arrayComposition)
    assertInvalid(ujson.Arr(2), arrayComposition)
    assertInvalid(ujson.Arr(1, 2), arrayComposition)

    val conditional = ujson.read(
      """{
        |  "type":"object",
        |  "required":["flag","value"],
        |  "if":{"properties":{"flag":{"const":true}}},
        |  "then":{"properties":{"value":{"const":1}}},
        |  "else":{"properties":{"value":{"const":2}}},
        |  "not":{"properties":{"value":{"const":3}}}
        |}""".stripMargin
    )
    assertValid(ujson.Obj("flag" -> true, "value" -> 1), conditional)
    assertValid(ujson.Obj("flag" -> false, "value" -> 2), conditional)
    assertInvalid(ujson.Obj("flag" -> true, "value" -> 2), conditional)
    assertInvalid(ujson.Obj("flag" -> false, "value" -> 3), conditional)

    val namedProperties = ujson.read(
      """{
        |  "type":"object",
        |  "propertyNames":{"pattern":"\\S"},
        |  "additionalProperties":{"type":"string"}
        |}""".stripMargin
    )
    assertValid(ujson.Obj("detail" -> "value"), namedProperties)
    assertInvalid(ujson.Obj(" " -> "value"), namedProperties)
  }

  test("v5 early analysis and rejected-search states expose only completed phase evidence") {
    val analysis = parseV5(documents.analysisFailure)
    assertEquals(analysis("status").str, "not_applicable")
    assertEquals(analysis("report")("completed_phase").str, "query-analyzed")
    assert(analysis("report")("analysis") != ujson.Null)
    Vector(
      "hypergraph",
      "soft_generation",
      "admissible_generation",
      "candidate_input",
      "candidate_search",
      "candidate_selection",
      "selected_plan",
      "bag_factor_plan",
      "rendering"
    ).foreach(field => assertEquals(analysis("report")(field), ujson.Null))

    val leakedSearch = parseV5(documents.analysisFailure)
    leakedSearch("report")("candidate_search") =
      parseV5(documents.searchFailure)("report")("candidate_search")
    assertInvalidV5(leakedSearch)

    val unexpected = parseV5(documents.analysisFailure)
    unexpected("report")("unexpected") = true
    assertInvalidV5(unexpected)

    val missingRequired = parseV5(documents.analysisFailure)
    missingRequired("report").obj.remove("candidate_selection")
    assertInvalidV5(missingRequired)

    val search = parseV5(documents.searchFailure)
    assertEquals(search("status").str, "not_applicable")
    assertEquals(
      search("report")("completed_phase").str,
      "candidate-search-completed"
    )
    assertEquals(search("report")("candidate_search")("decision").str, "rejected")
    assertEquals(search("report")("candidate_selection"), ujson.Null)
    assertFinalArtifactsNull(search)

    val leakedSelection = parseV5(documents.searchFailure)
    leakedSelection("report")("candidate_selection") =
      parseV5(documents.missingEvaluator)("report")("candidate_selection")
    assertInvalidV5(leakedSelection)
  }

  test("v5 pre-evaluator and mid-stage failures have strict stage-specific prefixes") {
    val unavailable = parseV5(documents.missingEvaluator)
    assertEquals(unavailable("status").str, "failed")
    assertEquals(
      unavailable("report")("completed_phase").str,
      "candidate-search-completed"
    )
    val unavailableSelection = unavailable("report")("candidate_selection")
    val unavailableFactor = unavailableSelection("factor_aware")
    assertEquals(
      unavailableSelection("decision").str,
      "factor_aware_evaluation_failed"
    )
    assertEquals(unavailableSelection("explain_failure_stage").str, "session")
    assertEquals(unavailableSelection("cost_provenance"), ujson.Null)
    assertEquals(unavailableSelection("source_optimizer_estimate"), ujson.Null)
    assertEquals(unavailableSelection("evaluated_candidate_count").num.toInt, 0)
    assertEquals(unavailableSelection("evaluations").arr.size, 0)
    assertEquals(
      unavailableFactor("failure_code").str,
      PostgreSqlExplainCostFailureCode.EvaluatorUnavailable.id
    )
    assertEquals(unavailableFactor("terminal_effective_candidate_count"), ujson.Null)
    assertEquals(unavailableFactor("search_exhausted_before_limit"), ujson.Null)
    assertEquals(unavailableFactor("checkpoints").arr.size, 0)
    assertEquals(unavailableFactor("rendered_candidates").arr.size, 4)
    assertEquals(unavailableFactor("completed_stage_observations").arr.size, 0)
    assertEquals(unavailableFactor("failed_stage_id"), ujson.Null)
    assertEquals(unavailableFactor("failed_stage_observation_index"), ujson.Null)
    assertFinalArtifactsNull(unavailable)

    val wrongSessionCode = parseV5(documents.missingEvaluator)
    wrongSessionCode("report")("candidate_selection")("factor_aware")(
      "failure_code"
    ) = PostgreSqlExplainCostFailureCode.StatementFailed.id
    assertInvalidV5(wrongSessionCode)

    val leakedSource = parseV5(documents.missingEvaluator)
    leakedSource("report")("candidate_selection")("source_optimizer_estimate") =
      parseV5(documents.midStageFailure)("report")("candidate_selection")(
        "source_optimizer_estimate"
      )
    assertInvalidV5(leakedSource)

    val leakedTerminal = parseV5(documents.missingEvaluator)
    leakedTerminal("report")("candidate_selection")("factor_aware")(
      "terminal_effective_candidate_count"
    ) = 4
    assertInvalidV5(leakedTerminal)

    val leakedEvaluation = parseV5(documents.missingEvaluator)
    val selectedEvaluation = parseV5(documents.selected)("report")(
      "candidate_selection"
    )("evaluations")(0)
    leakedEvaluation("report")("candidate_selection")("evaluated_candidate_count") =
      1
    leakedEvaluation("report")("candidate_selection")("evaluations").arr +=
      selectedEvaluation
    assertInvalidV5(leakedEvaluation)

    val leakedArtifacts = parseV5(documents.missingEvaluator)
    leakedArtifacts("report")("bag_factor_plan") =
      parseV5(documents.selected)("report")("bag_factor_plan")
    assertInvalidV5(leakedArtifacts)

    val mid = parseV5(documents.midStageFailure)
    val midSelection = mid("report")("candidate_selection")
    val midFactor = midSelection("factor_aware")
    assertEquals(mid("status").str, "failed")
    assertEquals(midSelection("explain_failure_stage").str, "stage-explain")
    assert(midSelection("cost_provenance") != ujson.Null)
    assert(midSelection("source_optimizer_estimate") != ujson.Null)
    assertEquals(midSelection("evaluated_candidate_count").num.toInt, 1)
    assertEquals(midSelection("evaluations").arr.size, 1)
    assertEquals(midSelection("failed_rank").num.toInt, 2)
    assertEquals(
      midFactor("failure_code").str,
      PostgreSqlExplainCostFailureCode.StatementFailed.id
    )
    assert(midFactor("failed_stage_id").str.nonEmpty)
    assertEquals(
      midFactor("failed_stage_observation_index").num.toInt,
      midFactor("completed_stage_observations").arr.size
    )
    assert(midFactor("completed_stage_observations").arr.nonEmpty)
    assertEquals(midFactor("rendered_candidates").arr.size, 4)
    assertEquals(midFactor("terminal_effective_candidate_count"), ujson.Null)
    assertEquals(midFactor("search_exhausted_before_limit"), ujson.Null)
    assertEquals(midFactor("checkpoints").arr.size, 0)
    assertFinalArtifactsNull(mid)

    val wrongStageCode = parseV5(documents.midStageFailure)
    wrongStageCode("report")("candidate_selection")("factor_aware")(
      "failure_code"
    ) = PostgreSqlExplainCostFailureCode.EvaluatorUnavailable.id
    assertInvalidV5(wrongStageCode)

    val missingStageId = parseV5(documents.midStageFailure)
    missingStageId("report")("candidate_selection")("factor_aware")(
      "failed_stage_id"
    ) = ujson.Null
    assertInvalidV5(missingStageId)

    val missingStageIndex = parseV5(documents.midStageFailure)
    missingStageIndex("report")("candidate_selection")("factor_aware")(
      "failed_stage_observation_index"
    ) = ujson.Null
    assertInvalidV5(missingStageIndex)

    val oversizedStageIndex = parseV5(documents.midStageFailure)
    oversizedStageIndex("report")("candidate_selection")("factor_aware")(
      "failed_stage_observation_index"
    ) = 4095
    assertInvalidV5(oversizedStageIndex)

    val missingSource = parseV5(documents.midStageFailure)
    missingSource("report")("candidate_selection")(
      "source_optimizer_estimate"
    ) = ujson.Null
    assertInvalidV5(missingSource)
  }

  test("v5 selected success binds terminal rendering, exact checkpoints, and stage discriminators") {
    val selected = parseV5(documents.selected)
    assertEquals(selected("status").str, "rewritten")
    assertEquals(selected("report")("completed_phase").str, "rendered")
    assert(selected("report")("selected_plan") != ujson.Null)
    assert(selected("report")("bag_factor_plan") != ujson.Null)
    assert(selected("report")("rendering") != ujson.Null)

    val selection = selected("report")("candidate_selection")
    val factor = selection("factor_aware")
    assertEquals(selection("decision").str, "selected")
    assert(selection("cost_provenance") != ujson.Null)
    assert(selection("source_optimizer_estimate") != ujson.Null)
    assertEquals(selection("evaluated_candidate_count").num.toInt, 4)
    assert(selection("evaluations").arr.exists(
      _("row_guard")("within_guard").bool
    ))
    assertEquals(factor("requested_plan_limit").num.toInt, 100)
    assertEquals(factor("terminal_effective_candidate_count").num.toInt, 4)
    assertEquals(factor("search_exhausted_before_limit").bool, true)
    assertNumericEnvelope(factor("numeric_envelope"))
    assertLimits(factor("limits"))
    assertEquals(
      factor("checkpoints").arr.map(_("requested_prefix").num.toInt).toVector,
      Vector(10, 25, 50, 100)
    )
    assert(factor("checkpoints").arr.forall(
      _("truncated_by_search_exhaustion").bool
    ))
    assert(factor("checkpoints").arr.last("winner_score") != ujson.Null)
    assertEquals(factor("rendered_candidates").arr.size, 4)
    assertEquals(factor("completed_stage_observations").arr.size, 0)
    assertEquals(factor("failure_code"), ujson.Null)
    assertEquals(factor("failed_stage_id"), ujson.Null)
    assertEquals(factor("failed_stage_observation_index"), ujson.Null)

    selection("evaluations").arr.zip(factor("rendered_candidates").arr)
      .foreach { case (evaluation, rendered) =>
        val terminal = evaluation("factor_aware_evidence")("stages").arr.last
        assertEquals(
          terminal("diagnostic_statement_sha256").str,
          evaluation("rendered_statement_sha256").str
        )
        assertEquals(
          terminal("diagnostic_statement_sha256").str,
          rendered("rendered_statement_sha256").str
        )
        assertEquals(
          rendered("stages").arr.last("statement_sha256").str,
          rendered("rendered_statement_sha256").str
        )
      }

    val allStages = selection("evaluations").arr.flatMap(
      _("factor_aware_evidence")("stages").arr
    )
    val initial = allStages.find(stage => stage("stage_id").str.matches(
      "^factor:b(0|[1-9][0-9]*):v0$"
    )).getOrElse(fail("selected factor evidence has no initial v0 stage"))
    assertEquals(initial("kind").str, "initial-factor")
    val fused = allStages.find(stage => stage("stage_id").str.matches(
      "^factor:b(0|[1-9][0-9]*):v[1-9][0-9]*$"
    )).getOrElse(fail("selected factor evidence has no fused v1+ stage"))
    assertEquals(fused("kind").str, "fused-reduction")

    val wrongInitialKind = parseV5(documents.selected)
    findStage(wrongInitialKind, initial("stage_id").str)("kind") =
      "fused-reduction"
    assertInvalidV5(wrongInitialKind)

    val wrongFusedKind = parseV5(documents.selected)
    findStage(wrongFusedKind, fused("stage_id").str)("kind") =
      "initial-factor"
    assertInvalidV5(wrongFusedKind)

    val missingInitialStage = parseV5(documents.selected)
    val firstEvidenceStages = missingInitialStage("report")(
      "candidate_selection"
    )("evaluations")(0)("factor_aware_evidence")("stages").arr
    val withoutInitial = firstEvidenceStages.filterNot(
      _("kind").str == "initial-factor"
    )
    assert(withoutInitial.size < firstEvidenceStages.size)
    firstEvidenceStages.clear()
    firstEvidenceStages ++= withoutInitial
    assertInvalidV5(missingInitialStage)

    val missingFinalStage = parseV5(documents.selected)
    val evidenceStages = missingFinalStage("report")("candidate_selection")(
      "evaluations"
    )(0)("factor_aware_evidence")("stages").arr
    assertEquals(evidenceStages.last("stage_id").str, "final-reconstruction")
    evidenceStages.remove(evidenceStages.size - 1)
    assertInvalidV5(missingFinalStage)

    val missingRenderedFinal = parseV5(documents.selected)
    val renderedStages = missingRenderedFinal("report")("candidate_selection")(
      "factor_aware"
    )("rendered_candidates")(0)("stages").arr
    assertEquals(renderedStages.last("stage_id").str, "final-reconstruction")
    renderedStages.remove(renderedStages.size - 1)
    assertInvalidV5(missingRenderedFinal)

    val allFalse = parseV5(documents.selected)
    allFalse("report")("candidate_selection")("evaluations").arr.foreach(
      _("row_guard")("within_guard") = false
    )
    assertInvalidV5(allFalse)

    val swappedCheckpoints = parseV5(documents.selected)
    val checkpoints = swappedCheckpoints("report")("candidate_selection")(
      "factor_aware"
    )("checkpoints").arr
    val first = checkpoints(0)
    checkpoints(0) = checkpoints(1)
    checkpoints(1) = first
    assertInvalidV5(swappedCheckpoints)

    val nonMonotoneExhaustion = parseV5(documents.selected)
    val secondCheckpoint = nonMonotoneExhaustion("report")(
      "candidate_selection"
    )("factor_aware")("checkpoints")(1)
    secondCheckpoint("truncated_by_search_exhaustion") = false
    secondCheckpoint("effective_candidate_count") = 25
    assertInvalidV5(nonMonotoneExhaustion)

    val missingWinner = parseV5(documents.selected)
    val finalCheckpoint = missingWinner("report")("candidate_selection")(
      "factor_aware"
    )("checkpoints").arr.last
    Vector(
      "winner_rank",
      "winner_candidate_id",
      "winner_physical_variant_id",
      "winner_score"
    ).foreach(field => finalCheckpoint(field) = ujson.Null)
    assertInvalidV5(missingWinner)

    val terminalUnknown = parseV5(documents.selected)
    terminalUnknown("report")("candidate_selection")("factor_aware")(
      "terminal_effective_candidate_count"
    ) = ujson.Null
    assertInvalidV5(terminalUnknown)

    val inconsistentExhaustion = parseV5(documents.selected)
    inconsistentExhaustion("report")("candidate_selection")("factor_aware")(
      "search_exhausted_before_limit"
    ) = false
    assertInvalidV5(inconsistentExhaustion)

    val zeroEvaluatedCount = parseV5(documents.selected)
    zeroEvaluatedCount("report")("candidate_selection")(
      "evaluated_candidate_count"
    ) = 0
    assertInvalidV5(zeroEvaluatedCount)

    val oversizedRank = parseV5(documents.selected)
    oversizedRank("report")("candidate_selection")("evaluations")(0)("rank") =
      101
    assertInvalidV5(oversizedRank)

    val oversizedRenderedStage = parseV5(documents.selected)
    oversizedRenderedStage("report")("candidate_selection")("factor_aware")(
      "rendered_candidates"
    )(0)("stages")(0)("character_count") = 16777217
    assertInvalidV5(oversizedRenderedStage)

    val failedSuccess = parseV5(documents.selected)
    failedSuccess("status") = "failed"
    failedSuccess("exit_code") = 4
    assertInvalidV5(failedSuccess)

    val missingFinalArtifact = parseV5(documents.selected)
    missingFinalArtifact("report")("selected_plan") = ujson.Null
    assertInvalidV5(missingFinalArtifact)

    val unexpectedFactorField = parseV5(documents.selected)
    unexpectedFactorField("report")("candidate_selection")("factor_aware")(
      "unexpected"
    ) = true
    assertInvalidV5(unexpectedFactorField)
  }

  test("certified full-two-pass COUNT emits a schema-valid replay-bound v5 success") {
    val sql =
      "SELECT COUNT(*) AS cycle_rows " +
        "FROM count_e0 AS e0 " +
        "JOIN count_e1 AS e1 ON e0.right_key = e1.left_key " +
        "JOIN count_e2 AS e2 ON e1.right_key = e2.left_key " +
        "JOIN count_e3 AS e3 ON e2.right_key = e3.left_key " +
        "AND e3.right_key = e0.left_key"
    val evaluator = new RecordingFactorEvaluator((request, _) =>
      Right(
        successfulBatch(request, sourceRows = BigDecimal(1)) {
          (candidate, statement, index) =>
            if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
              estimate(
                totalCost = BigDecimal(candidate.structuralRank),
                rows = BigDecimal(1),
                width = 8L
              )
            else
              estimate(
                totalCost = BigDecimal(index + 1),
                rows = BigDecimal(candidate.structuralRank),
                width = 8L
              )
        }
      )
    )
    val request = factorRequest(sql)
    val schema = Frameworks.createRootSchema(true)
    (0 until 4).foreach { index =>
      schema.add(
        s"count_e$index",
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("left_key", SqlTypeName.INTEGER)
              .add("right_key", SqlTypeName.INTEGER)
              .build()
        }
      )
    }
    val planner = CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.message), identity)
    val countRewriter = PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = Some(evaluator))
      .fold(problem => fail(problem.message), identity)
    val result = countRewriter.rewriteDetailed(request)
    assert(
      result.outcome.isInstanceOf[RewriteOutcome.Rewritten],
      clues(result.outcome, result.report.completedPhase)
    )
    val artifact = result.report.artifact.getOrElse(
      fail("certified COUNT success did not retain its v5 artifact")
    )
    val selection = artifact.candidateSelection

    assertEquals(selection.policy, policy)
    assertEquals(selection.reductionMode, YannakakisReductionMode.FullTwoPass)
    assertEquals(evaluator.factorCalls, 1)
    assert(
      evaluator.factorRequests.head.candidates.forall(
        _.reductionMode == YannakakisReductionMode.FullTwoPass
      )
    )
    assert(
      artifact.renderedQuery.sql.contains(
        "CAST(COUNT(*) AS NUMERIC) AS \"yr_freq\""
      ),
      clues(artifact.renderedQuery.sql)
    )

    val rendered = PostgreSqlRewriteJson.render(request, result)
    assertNoDuplicateKeys(rendered)
    val document = parseV5(rendered)
    assertEquals(document("status").str, "rewritten")
    assertEquals(document("report")("completed_phase").str, "rendered")
    assertEquals(
      document("outcome")("rewritten_statements")(0).str,
      artifact.renderedQuery.sql
    )

    val source = selection.sourceEvaluation.getOrElse(
      fail("factor-aware COUNT selection has no source estimate")
    )
    val limits = selection.factorAwareSelectionLimits.getOrElse(
      fail("factor-aware COUNT selection has no replay limits")
    )
    def replays(checkpoints: Vector[
        at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlFactorAwareCheckpoint
    ]): Boolean =
      PostgreSqlFactorAwareSelectionReplay.verify(
        artifact.candidateSearch.plans,
        selection.evaluations,
        source,
        checkpoints,
        limits,
        Some(source.sourceStatementSha256)
      )

    assert(replays(selection.factorAwareCheckpoints))
    val firstCheckpoint = selection.factorAwareCheckpoints.head
    val tampered = selection.factorAwareCheckpoints.updated(
      0,
      firstCheckpoint.copy(verifiedPrefixSha256 = "0" * 64)
    )
    assert(!replays(tampered))
  }

  test("v5 no-admissible state retains all false guards and has no winner or final artifact") {
    val noAdmissible = parseV5(documents.noAdmissible)
    assertEquals(noAdmissible("status").str, "failed")
    assertEquals(
      noAdmissible("report")("completed_phase").str,
      "candidate-selection-completed"
    )
    val selection = noAdmissible("report")("candidate_selection")
    val factor = selection("factor_aware")
    assertEquals(selection("decision").str, "no_admissible_estimate")
    assert(selection("cost_provenance") != ujson.Null)
    assert(selection("source_optimizer_estimate") != ujson.Null)
    assertEquals(selection("evaluated_candidate_count").num.toInt, 4)
    assert(selection("evaluations").arr.forall(
      !_("row_guard")("within_guard").bool
    ))
    assertEquals(factor("terminal_effective_candidate_count").num.toInt, 4)
    assertEquals(factor("search_exhausted_before_limit").bool, true)
    assertEquals(
      factor("checkpoints").arr.map(_("requested_prefix").num.toInt).toVector,
      Vector(10, 25, 50, 100)
    )
    assert(factor("checkpoints").arr.forall(checkpoint =>
      checkpoint("winner_rank") == ujson.Null &&
        checkpoint("winner_candidate_id") == ujson.Null &&
        checkpoint("winner_physical_variant_id") == ujson.Null &&
        checkpoint("winner_score") == ujson.Null
    ))
    assertEquals(factor("rendered_candidates").arr.size, 4)
    assertEquals(factor("completed_stage_observations").arr.size, 0)
    assertEquals(factor("failure_code"), ujson.Null)
    assertFinalArtifactsNull(noAdmissible)

    val trueGuard = parseV5(documents.noAdmissible)
    trueGuard("report")("candidate_selection")("evaluations")(0)(
      "row_guard"
    )("within_guard") = true
    assertInvalidV5(trueGuard)

    val winner = parseV5(documents.noAdmissible)
    winner("report")("candidate_selection")("factor_aware")("checkpoints")(
      0
    ) = parseV5(documents.selected)("report")("candidate_selection")(
      "factor_aware"
    )("checkpoints")(0)
    assertInvalidV5(winner)

    val missingTerminal = parseV5(documents.noAdmissible)
    missingTerminal("report")("candidate_selection")("factor_aware")(
      "terminal_effective_candidate_count"
    ) = ujson.Null
    assertInvalidV5(missingTerminal)

    val leakedSelectedPlan = parseV5(documents.noAdmissible)
    leakedSelectedPlan("report")("selected_plan") =
      parseV5(documents.selected)("report")("selected_plan")
    assertInvalidV5(leakedSelectedPlan)

    val leakedRendering = parseV5(documents.noAdmissible)
    leakedRendering("report")("rendering") =
      parseV5(documents.selected)("report")("rendering")
    assertInvalidV5(leakedRendering)
  }

  test("the v5 schema and every emitted state are strict and duplicate-free") {
    assertEquals(
      sha256(Files.readAllBytes(v5SchemaPath)),
      "edf059d678c1644b60beeb74e77018036211dbf2b2b45beafdeced72ad0e8ee8"
    )
    assertEquals(
      v5Schema("$schema").str,
      "https://json-schema.org/draft/2020-12/schema"
    )
    assertEquals(
      v5Schema("properties")("schema_version")("const").str,
      PostgreSqlRewriteJson.FactorAwareSchemaVersion
    )
    assertNoDuplicateKeys(v5SchemaText)
    assert(!v5SchemaText.contains("\"reached_plan_limit\""))
    assert(!v5SchemaText.contains("\"required_plan_limit\""))

    documents.all.foreach { rendered =>
      assertNoDuplicateKeys(rendered)
      assertV5(rendered)
    }

    val unknownDiagnosticCode = parseV5(documents.analysisFailure)
    unknownDiagnosticCode("outcome")("diagnostics")(0)("code") = "UNKNOWN"
    assertInvalidV5(unknownDiagnosticCode)

    val blankDiagnosticMessage = parseV5(documents.analysisFailure)
    blankDiagnosticMessage("outcome")("diagnostics")(0)("message") = " "
    assertInvalidV5(blankDiagnosticMessage)

    val blankDiagnosticContextKey = parseV5(documents.analysisFailure)
    blankDiagnosticContextKey("outcome")("diagnostics")(0)("context")(
      " "
    ) = "value"
    assertInvalidV5(blankDiagnosticContextKey)

    val blankRewrittenStatement = parseV5(documents.selected)
    blankRewrittenStatement("outcome")("rewritten_statements")(0) = " "
    assertInvalidV5(blankRewrittenStatement)
  }

  private final case class Documents(
      analysisFailure: String,
      searchFailure: String,
      missingEvaluator: String,
      midStageFailure: String,
      selected: String,
      noAdmissible: String
  ) {
    val all: Vector[String] = Vector(
      analysisFailure,
      searchFailure,
      missingEvaluator,
      midStageFailure,
      selected,
      noAdmissible
    )
  }

  private lazy val documents: Documents = {
    val analysisRequest = factorRequest(
      "SELECT department_id, COUNT(*) AS n FROM fact_sale GROUP BY department_id"
    )
    val analysisResult = rewriter(None).rewriteDetailed(analysisRequest)
    assert(analysisResult.outcome.isInstanceOf[RewriteOutcome.NotApplicable])

    val searchRequest = factorRequest(triangleSql, widthBound = 1)
    val searchResult = rewriter(None).rewriteDetailed(searchRequest)
    assert(searchResult.outcome.isInstanceOf[RewriteOutcome.NotApplicable])

    val missingRequest = factorRequest(triangleSql)
    val missingResult = rewriter(None).rewriteDetailed(missingRequest)
    assert(missingResult.outcome.isInstanceOf[RewriteOutcome.Failed])

    val midEvaluator = new RecordingFactorEvaluator((request, _) => {
      val batch = successfulBatch(request, sourceRows = BigDecimal(100))()
      val failedIndex = request.candidates.head.stages.size
      Left(
        PostgreSqlFactorAwareExplainFailure(
          PostgreSqlExplainCostFailureCode.StatementFailed,
          PostgreSqlExplainEvaluationStage.StageExplain,
          Some(failedIndex),
          Some(batch.source),
          batch.observations.take(failedIndex)
        )
      )
    })
    val midRequest = factorRequest(triangleSql)
    val midResult = rewriter(Some(midEvaluator)).rewriteDetailed(midRequest)
    assert(midResult.outcome.isInstanceOf[RewriteOutcome.Failed])

    val selectedEvaluator = new RecordingFactorEvaluator((request, _) =>
      Right(
        successfulBatch(request, sourceRows = BigDecimal(100)) {
          (candidate, statement, index) =>
            if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
              estimate(
                totalCost = BigDecimal(candidate.structuralRank),
                rows = BigDecimal(100),
                width = 8
              )
            else
              estimate(
                totalCost = BigDecimal(index + 1),
                rows = BigDecimal(candidate.structuralRank),
                width = 8
              )
        }
      )
    )
    val selectedRequest = factorRequest(triangleSql)
    val selectedResult =
      rewriter(Some(selectedEvaluator)).rewriteDetailed(selectedRequest)
    assert(selectedResult.outcome.isInstanceOf[RewriteOutcome.Rewritten])

    val noAdmissibleEvaluator = new RecordingFactorEvaluator((request, _) =>
      Right(
        successfulBatch(request, sourceRows = BigDecimal(100)) {
          (candidate, statement, index) =>
            if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
              estimate(
                totalCost = BigDecimal(candidate.structuralRank),
                rows = BigDecimal(0),
                width = 8
              )
            else
              estimate(
                totalCost = BigDecimal(index + 1),
                rows = BigDecimal(candidate.structuralRank),
                width = 8
              )
        }
      )
    )
    val noAdmissibleRequest = factorRequest(triangleSql)
    val noAdmissibleResult =
      rewriter(Some(noAdmissibleEvaluator)).rewriteDetailed(noAdmissibleRequest)
    assert(noAdmissibleResult.outcome.isInstanceOf[RewriteOutcome.Failed])

    Documents(
      PostgreSqlRewriteJson.render(analysisRequest, analysisResult),
      PostgreSqlRewriteJson.render(searchRequest, searchResult),
      PostgreSqlRewriteJson.render(missingRequest, missingResult),
      PostgreSqlRewriteJson.render(midRequest, midResult),
      PostgreSqlRewriteJson.render(selectedRequest, selectedResult),
      PostgreSqlRewriteJson.render(noAdmissibleRequest, noAdmissibleResult)
    )
  }

  private def factorRequest(
      sql: String,
      widthBound: Int = 2
  ): RewriteRequest =
    RewriteRequest(
      sql,
      options = RewriteOptions(
        widthBound = widthBound,
        planLimit = 100,
        candidateSelection = policy,
        reductionMode = YannakakisReductionMode.FullTwoPass
      )
    )

  private def rewriter(
      evaluator: Option[PostgreSqlExplainCostEvaluator]
  ): PostgreSqlQueryRewriter = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = evaluator)
      .fold(problem => fail(problem.message), identity)
  }

  private def findStage(document: ujson.Value, stageId: String): ujson.Value =
    document("report")("candidate_selection")("evaluations").arr
      .flatMap(_("factor_aware_evidence")("stages").arr)
      .find(_("stage_id").str == stageId)
      .getOrElse(fail(s"could not find factor stage $stageId"))

  private def assertNumericEnvelope(value: ujson.Value): Unit = {
    assertEquals(
      value("scheme").str,
      "postgresql_factor_numeric_precision_scale_v1"
    )
    assertEquals(value("max_input_precision").num.toInt, 64)
    assertEquals(value("max_derived_precision").num.toInt, 128)
    assertEquals(value("max_absolute_scale").num.toInt, 1024)
  }

  private def assertLimits(value: ujson.Value): Unit = {
    assertEquals(value("max_candidates").num.toInt, 100)
    assertEquals(value("max_stage_probes_per_candidate").num.toInt, 256)
    assertEquals(value("max_explain_statements_per_session").num.toInt, 4096)
    assertEquals(value("statement_timeout_seconds").num.toInt, 30)
    assertEquals(value("explain_statement_budget_seconds").num.toInt, 600)
  }

  private def assertFinalArtifactsNull(document: ujson.Value): Unit =
    Vector("selected_plan", "bag_factor_plan", "rendering").foreach(field =>
      assertEquals(document("report")(field), ujson.Null)
    )

  private lazy val v5SchemaPath: Path = repositoryFile(
    "docs/journal-extension/schemas/postgresql-rewrite-artifact-v5.schema.json"
  )
  private lazy val v5SchemaText: String = Files.readString(v5SchemaPath)
  private lazy val v5Schema: ujson.Value = ujson.read(v5SchemaText)

  private def parseV5(rendered: String): ujson.Value = {
    assertV5(rendered)
    ujson.read(rendered)
  }

  private def assertV5(rendered: String): Unit = {
    val document = ujson.read(rendered)
    assertEquals(
      document("schema_version").str,
      PostgreSqlRewriteJson.FactorAwareSchemaVersion
    )
    val errors =
      PostgreSqlFactorAwareV5SchemaTestValidator.validate(document, v5Schema)
    assert(
      errors.isEmpty,
      errors.map(error => s"${error.path}: ${error.message}").mkString("\n")
    )
  }

  private def assertInvalidV5(document: ujson.Value): Unit =
    assert(
      PostgreSqlFactorAwareV5SchemaTestValidator
        .validate(document, v5Schema)
        .nonEmpty
    )

  private def assertValid(
      instance: ujson.Value,
      schema: ujson.Value
  ): Unit =
    assertEquals(
      PostgreSqlFactorAwareV5SchemaTestValidator.validate(instance, schema),
      Vector.empty
    )

  private def assertInvalid(
      instance: ujson.Value,
      schema: ujson.Value
  ): Unit =
    assert(
      PostgreSqlFactorAwareV5SchemaTestValidator
        .validate(instance, schema)
        .nonEmpty
    )

  private def assertNoDuplicateKeys(json: String): Unit = {
    val parser = new JsonFactory()
      .enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
      .createParser(json)
    try while (parser.nextToken() != null) ()
    finally parser.close()
  }

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

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
