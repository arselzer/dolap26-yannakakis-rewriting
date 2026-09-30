package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.IdentityHashMap

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlRenderingLimits,
  PostgreSqlStageId
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  DiagnosticSeverity,
  RewriteDiagnostic,
  RewriteOptions,
  RewriteOutcome,
  RewriteReasonCode,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql._
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import com.fasterxml.jackson.core.{JsonFactory, JsonParser}

/** Emitted-state and hostile-copy contract for the isolated v6 serializer. */
class PostgreSqlPhysicalPortfolioV6JsonContractSuite
    extends munit.FunSuite {
  private val policy =
    CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
  private val triangleSql =
    "SELECT r.a, r.b, s.c FROM triangle_r AS r " +
      "JOIN triangle_s AS s ON r.b = s.b " +
      "JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a"

  test("the v6 test validator covers every Draft 2020-12 keyword used by the schema") {
    val schema = ujson.read(
      """{
        |  "type":"object",
        |  "propertyNames":{"pattern":"\\S"},
        |  "required":["values"],
        |  "properties":{
        |    "values":{
        |      "type":"array",
        |      "prefixItems":[{"const":"first"}],
        |      "items":{"type":"string","minLength":1,"maxLength":3},
        |      "contains":{"const":"hit"},
        |      "minContains":1,
        |      "uniqueItems":true
        |    }
        |  },
        |  "additionalProperties":false
        |}""".stripMargin
    )
    assertValid(ujson.Obj("values" -> ujson.Arr("first", "hit")), schema)
    assertInvalid(ujson.Obj("values" -> ujson.Arr("first", "long")), schema)
    assertInvalid(ujson.Obj("values" -> ujson.Arr("first", "miss")), schema)
    assertInvalid(ujson.Obj("values" -> ujson.Arr("first", "hit", "hit")), schema)
    assertInvalid(ujson.Obj(" " -> ujson.Arr("first", "hit")), schema)

    val exactOne = ujson.read(
      """{
        |  "oneOf":[
        |    {"type":"string"},
        |    {"const":"two-matches"}
        |  ]
        |}""".stripMargin
    )
    assertValid(ujson.Str("one-match"), exactOne)
    assertInvalid(ujson.Num(1), exactOne)
    assertInvalid(ujson.Str("two-matches"), exactOne)

    val hiddenNestedViolation = ujson.read(
      """{
        |  "oneOf":[
        |    {
        |      "properties":{
        |        "kind":{"const":"selected"},
        |        "payload":{
        |          "type":"object",
        |          "required":["required_value"],
        |          "properties":{"required_value":{"type":"string"}}
        |        }
        |      }
        |    },
        |    {"properties":{"kind":{"const":"other"}}}
        |  ]
        |}""".stripMargin
    )
    assertInvalid(
      ujson.Obj("kind" -> "selected", "payload" -> ujson.Obj()),
      hiddenNestedViolation
    )
  }

  test("v6 absent-attempt states stop at the exact candidate-search boundary") {
    val invalid = parseV6(documents.invalidQuery.rendered)
    assertEquals(invalid("status").str, "invalid_query")
    assertEquals(invalid("request")("original_sql").str, "")
    assertEquals(invalid("report")("completed_phase").str, "request-accepted")
    assertEquals(invalid("report")("candidate_search"), ujson.Null)
    assertEquals(invalid("report")("physical_portfolio"), ujson.Null)

    val analysis = parseV6(documents.analysisFailure.rendered)
    assertEquals(analysis("status").str, "not_applicable")
    assertEquals(analysis("report")("completed_phase").str, "query-analyzed")
    assertEquals(analysis("report")("candidate_search"), ujson.Null)
    assertEquals(analysis("report")("physical_portfolio"), ujson.Null)

    val rejected = parseV6(documents.searchRejected.rendered)
    assertEquals(
      rejected("report")("completed_phase").str,
      "candidate-search-completed"
    )
    assertEquals(
      rejected("report")("candidate_search")("decision").str,
      "rejected"
    )
    assertEquals(rejected("report")("physical_portfolio"), ujson.Null)

    val earlyRejected =
      PostgreSqlPhysicalPortfolioJsonTestMutations.withoutAttemptAt(
        documents.analysisFailure.result,
        PostgreSqlRewritePhase.QueryAnalyzed,
        documents.searchRejected.result.report.candidateDecision
      )
    assertRenderRejected(documents.analysisFailure.request, earlyRejected)

    val missingSearchDecision =
      PostgreSqlPhysicalPortfolioJsonTestMutations.withoutAttemptAt(
        documents.searchRejected.result,
        PostgreSqlRewritePhase.CandidateSearchCompleted,
        None
      )
    assertRenderRejected(documents.searchRejected.request, missingSearchDecision)

    val foundWithoutAttempt =
      PostgreSqlPhysicalPortfolioJsonTestMutations.withoutAttemptAt(
        documents.missingEvaluator.result,
        PostgreSqlRewritePhase.CandidateSearchCompleted,
        documents.missingEvaluator.result.report.candidateDecision
      )
    assertRenderRejected(documents.missingEvaluator.request, foundWithoutAttempt)

    Vector(
      PostgreSqlRewritePhase.CandidateSelectionCompleted,
      PostgreSqlRewritePhase.BagFactorPlanned,
      PostgreSqlRewritePhase.Rendered
    ).foreach { phase =>
      val late = PostgreSqlPhysicalPortfolioJsonTestMutations.withoutAttemptAt(
        documents.analysisFailure.result,
        phase,
        None
      )
      assertRenderRejected(documents.analysisFailure.request, late)
    }
  }

  test("v6 terminal outcomes are the exact projection of their physical decisions") {
    val selectedResult = documents.selected.result
    val selected = selectedResult.outcome match {
      case value: RewriteOutcome.Rewritten => value
      case other => fail(s"expected rewritten fixture, received $other")
    }
    val warning = RewriteDiagnostic(
      RewriteReasonCode.RewriteNotBeneficial,
      "hostile added warning",
      DiagnosticSeverity.Warning,
      Map("source" -> "test")
    )
    assertRenderRejected(
      documents.selected.request,
      PostgreSqlPhysicalPortfolioJsonTestMutations.withOutcome(
        selectedResult,
        selected.copy(diagnostics = Vector(warning))
      )
    )
    assertRenderRejected(
      documents.selected.request,
      PostgreSqlPhysicalPortfolioJsonTestMutations.withOutcome(
        selectedResult,
        selected.copy(
          rewrittenStatements = Vector(selected.rewrittenStatements.head + " ")
        )
      )
    )

    Vector(documents.noAdmissible, documents.missingEvaluator).foreach {
      emitted =>
        val failed = emitted.result.outcome match {
          case value: RewriteOutcome.Failed => value
          case other => fail(s"expected failed fixture, received $other")
        }
        val original = failed.diagnostics.head
        val additional = original.copy(message = original.message + " (extra)")
        val mutations = Vector(
          failed.copy(diagnostics = failed.diagnostics :+ additional),
          failed.copy(diagnostics = Vector(additional, original)),
          failed.copy(diagnostics = Vector(original.copy(
            code = RewriteReasonCode.InternalError
          ))),
          failed.copy(diagnostics = Vector(original.copy(
            message = original.message + "!"
          ))),
          failed.copy(diagnostics = Vector(original.copy(
            context = original.context.updated("hostile", "copy")
          )))
        )
        mutations.foreach { mutation =>
          assertRenderRejected(
            emitted.request,
            PostgreSqlPhysicalPortfolioJsonTestMutations.withOutcome(
              emitted.result,
              mutation
            )
          )
        }
    }
  }

  test("v6 selected and no-admissible states have strict final artifact shapes") {
    val selected = parseV6(documents.selected.rendered)
    assertEquals(selected("status").str, "rewritten")
    assertEquals(selected("report")("completed_phase").str, "rendered")
    assertEquals(
      selected("report")("physical_portfolio")("decision")("status").str,
      "selected"
    )
    Vector("selected_plan", "bag_factor_plan", "rendering").foreach(field =>
      assert(selected("report")(field) != ujson.Null, clues(field))
    )
    assertEquals(
      selected("outcome")("rewritten_statements").arr.toVector,
      Vector(ujson.Str(
        documents.selected.result.report.physicalPortfolioArtifact.get
          .renderedQuery.sql
      ))
    )

    val noAdmissible = parseV6(documents.noAdmissible.rendered)
    assertEquals(noAdmissible("status").str, "failed")
    assertEquals(
      noAdmissible("report")("completed_phase").str,
      "candidate-selection-completed"
    )
    assertEquals(
      noAdmissible("report")("physical_portfolio")("decision")("status").str,
      "no_admissible"
    )
    assert(
      noAdmissible("report")("physical_portfolio")("decision")("evaluations")
        .arr
        .forall(!_("row_guard")("within_guard").bool)
    )
    assertFinalArtifactsNull(noAdmissible)
  }

  test("v6 emitted failure states preserve the stage-specific replay prefix matrix") {
    val expected = Vector(
      documents.enumerationFailure ->
        "physical-portfolio.enumeration-failed",
      documents.planningFailure ->
        "physical-portfolio.candidate-planning-failed",
      documents.renderingFailure ->
        "physical-portfolio.stage-diagnostic-rendering-failed",
      documents.rawResourceFailure ->
        "physical-portfolio.raw-rendering-resource-limit-exceeded",
      documents.envelopeFailure ->
        "physical-portfolio.evaluation-envelope-limit-exceeded",
      documents.missingEvaluator -> "physical-portfolio.evaluator-failed",
      documents.midStageFailure -> "physical-portfolio.evaluator-failed",
      documents.unsupportedServer -> "physical-portfolio.evaluator-failed",
      documents.evidenceInvalid -> "physical-portfolio.evidence-invalid"
    )

    expected.foreach { case (emitted, failureKind) =>
      val document = parseV6(emitted.rendered)
      val decision = document("report")("physical_portfolio")("decision")
      assertEquals(document("status").str, "failed", clues(failureKind))
      assertEquals(decision("status").str, "failed", clues(failureKind))
      assertEquals(decision("failure_kind").str, failureKind)
      assertEquals(
        document("report")("completed_phase").str,
        "candidate-selection-completed"
      )
      assertFinalArtifactsNull(document)
      assert(!decision.obj.contains("collision_witness"), clues(failureKind))
    }

    val enumeration = decision(documents.enumerationFailure)
    assert(enumeration("enumeration_failure") != ujson.Null)
    assertEquals(enumeration("enumeration"), ujson.Null)
    assertEquals(enumeration("raw_render_prefix").arr.size, 0)

    val planning = decision(documents.planningFailure)
    assert(planning("enumeration") != ujson.Null)
    assert(planning("planning_problems").arr.nonEmpty)
    assertEquals(planning("deduplication"), ujson.Null)

    val rendering = decision(documents.renderingFailure)
    assert(rendering("enumeration") != ujson.Null)
    assert(rendering("rendering_problems").arr.nonEmpty)
    assertEquals(rendering("deduplication"), ujson.Null)

    val raw = decision(documents.rawResourceFailure)
    assert(raw("raw_render_prefix").arr.size >= 2)
    assertEquals(
      raw("resource_exceeded")("resource").str,
      "raw-diagnostic-characters-retained"
    )
    assertEquals(raw("deduplication"), ujson.Null)
    assertEquals(raw("explain_request"), ujson.Null)

    val envelope = decision(documents.envelopeFailure)
    assert(envelope("deduplication") != ujson.Null)
    assert(envelope("explain_request") != ujson.Null)
    assertEquals(envelope("evaluation_usage"), ujson.Null)
    assertEquals(
      envelope("resource_exceeded")("resource").str,
      "explain-statements-per-session"
    )

    val unavailable = decision(documents.missingEvaluator)
    assert(unavailable("deduplication") != ujson.Null)
    assert(unavailable("explain_request") != ujson.Null)
    assert(unavailable("evaluation_usage") != ujson.Null)
    assertEquals(
      unavailable("explain_failure")("code").str,
      "explain.evaluator-unavailable"
    )
    assertEquals(unavailable("explain_failure")("completed").arr.size, 0)

    val unsupported = decision(documents.unsupportedServer)
    assertEquals(
      unsupported("explain_failure")("stage").str,
      "version-preflight"
    )
    assertEquals(unsupported("explain_failure")("completed").arr.size, 0)
    assertEquals(unsupported("completed_evaluations").arr.size, 0)

    val mid = decision(documents.midStageFailure)
    assertEquals(mid("explain_failure")("stage").str, "stage-explain")
    assert(mid("explain_failure")("completed").arr.nonEmpty)
    assertEquals(mid("completed_evaluations").arr.size, 0)

    val evidence = decision(documents.evidenceInvalid)
    assertEquals(evidence("explain_failure"), ujson.Null)
    assert(evidence("explain_batch") != ujson.Null)
    assert(evidence("evaluation_usage") != ujson.Null)
    assertEquals(evidence("completed_evaluations").arr.size, 0)
  }

  test("v6 freezes axis order, canonical BigInt strings, and the normal SQL allowlist") {
    val all = documents.all.map(value => parseV6(value.rendered))
    all.foreach { document =>
      val options = document("request")("options")
      assertEquals(
        options("portfolio_reduction_schedule_order").arr.map(_.str).toVector,
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.ReductionModeOrder
          .map(_.id)
      )
      assertEquals(
        options("portfolio_materialization_directive_order").arr
          .map(_.str)
          .toVector,
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.MaterializationDirectiveOrder
          .map(_.id)
      )
      assertEquals(options("neutral_reduction_schedule").str, "full")

      collectObjectFields(document).foreach { case (path, name, value) =>
        assert(name != "sql", clues(path, value))
        assert(name != "ordered_stage_sql", clues(path, value))
        if (isCanonicalBigIntField(path, name)) {
          assert(value.isInstanceOf[ujson.Str], clues(path, value))
          assert(value.str.matches("0|[1-9][0-9]*"), clues(path, value))
        }
      }
    }

    val selected = parseV6(documents.selected.rendered)
    val candidates = selected("report")("physical_portfolio")("decision")(
      "enumeration"
    )("candidates").arr
    candidates.foreach { candidate =>
      val actual = candidate("applicable_axes").arr.map(_.str).toVector
      assertEquals(
        actual,
        PostgreSqlPhysicalPortfolioAxis.Values.filter(actual.toSet)
      )
    }
  }

  test("v6 schema rejects SQL leakage, reordered axes, and noncanonical BigInt fields") {
    val reversedModes = parseV6(documents.selected.rendered)
    reverseArray(reversedModes("request")("options")(
      "portfolio_reduction_schedule_order"
    ))
    assertInvalidV6(reversedModes)

    val reversedDirectives = parseV6(documents.selected.rendered)
    reverseArray(reversedDirectives("request")("options")(
      "portfolio_materialization_directive_order"
    ))
    assertInvalidV6(reversedDirectives)

    val sqlLeaks = Vector[(ujson.Value, ujson.Value => Unit)](
      parseV6(documents.selected.rendered) -> { document =>
        selectedDecision(document)("explain_request")("source")("sql") =
          "SELECT secret_source"
      },
      parseV6(documents.selected.rendered) -> { document =>
        selectedDecision(document)("explain_request")("workloads")(0)(
          "stages"
        )(0)("sql") = "SELECT secret_probe"
      },
      parseV6(documents.selected.rendered) -> { document =>
        selectedDecision(document)("explain_batch")("source")("sql") =
          "SELECT secret_source"
      },
      parseV6(documents.selected.rendered) -> { document =>
        selectedDecision(document)("explain_batch")("observations")(0)(
          "sql"
        ) = "SELECT secret_probe"
      },
      parseV6(documents.selected.rendered) -> { document =>
        selectedDecision(document)("raw_renderings")(0)("workload")(
          "ordered_stage_sql"
        ) = ujson.Arr("SELECT secret_probe")
      }
    )
    sqlLeaks.foreach { case (document, mutate) =>
      mutate(document)
      assertInvalidV6(document)
    }

    val selected = parseV6(documents.selected.rendered)
    val globalExact = selectedDecision(selected)("enumeration")(
      "exact_logical_variant_count"
    )
    assert(globalExact.isInstanceOf[ujson.Str])
    val numericGlobal = parseV6(documents.selected.rendered)
    selectedDecision(numericGlobal)("enumeration")(
      "exact_logical_variant_count"
    ) = ujson.Num(globalExact.str.toDouble)
    assertInvalidV6(numericGlobal)

    val leadingZero = parseV6(documents.selected.rendered)
    selectedDecision(leadingZero)("enumeration")("candidates")(0)(
      "exact_root_count"
    ) = "01"
    assertInvalidV6(leadingZero)

    val signed = parseV6(documents.selected.rendered)
    selectedDecision(signed)("enumeration")("entries")(0)("heap_key")(
      "deviation_magnitude"
    ) = "+1"
    assertInvalidV6(signed)

    val numericResource = parseV6(documents.rawResourceFailure.rendered)
    decision(numericResource)("resource_exceeded")("required") = 1
    assertInvalidV6(numericResource)
  }

  test("v6 canonical BigInt strings retain exact values beyond IEEE-754 range") {
    val huge = "9007199254740993"
    val mutations = Vector[(ujson.Value, ujson.Value => Unit)](
      parseV6(documents.selected.rendered) -> { document =>
        selectedDecision(document)("enumeration")(
          "exact_logical_variant_count"
        ) = huge
      },
      parseV6(documents.selected.rendered) -> { document =>
        selectedDecision(document)("enumeration")("candidates")(0)(
          "exact_root_count"
        ) = huge
      },
      parseV6(documents.selected.rendered) -> { document =>
        selectedDecision(document)("enumeration")("entries")(0)("heap_key")(
          "deviation_magnitude"
        ) = huge
      },
      parseV6(documents.rawResourceFailure.rendered) -> { document =>
        decision(document)("resource_exceeded")("required") = huge
      }
    )
    mutations.foreach { case (document, mutate) =>
      mutate(document)
      assertValidV6(document)
      assert(
        collectObjectFields(document).exists { case (_, _, value) =>
          value == ujson.Str(huge)
        }
      )
    }
  }

  test("v6 collision SQL is present only in the exact bounded collision witness") {
    val rawDecision = decision(documents.missingEvaluator)
    val rawPrefix = rawDecision("raw_render_prefix").arr
    assert(rawPrefix.size >= 2)
    val prior = collisionVector(rawPrefix.head)
    val offending = collisionVector(rawPrefix.last)
    offending("descriptor")("workload_id") =
      prior("descriptor")("workload_id")
    offending("descriptor")("rendered_character_count") =
      ujson.Num(
        offending("descriptor")("rendered_character_count").num + 1.0
      )
    val collision = ujson.Obj(
      "prior" -> prior,
      "offending" -> offending
    )
    assertValid(collision, schemaReference("collisionWitness"))

    val missingVector = ujson.read(collision.render())
    missingVector.obj.remove("offending")
    assertInvalid(missingVector, schemaReference("collisionWitness"))

    val extraVector = ujson.read(collision.render())
    extraVector("third") = collision("prior")
    assertInvalid(extraVector, schemaReference("collisionWitness"))

    val blankSql = ujson.read(collision.render())
    blankSql("prior")("ordered_stage_sql")(0) = " "
    assertInvalid(blankSql, schemaReference("collisionWitness"))

    val oversizedSql = ujson.read(collision.render())
    oversizedSql("prior")("ordered_stage_sql")(0) =
      "x" * (PostgreSqlRenderingLimits.default.maxSqlCharacters.toInt + 1)
    assertInvalid(oversizedSql, schemaReference("collisionWitness"))

    val collisionDecision = ujson.read(rawDecision.render())
    collisionDecision("failure_kind") =
      "physical-portfolio.workload-identity-collision"
    collisionDecision("failure_stage") = "workload-deduplication"
    collisionDecision("detail_code") =
      "physical-portfolio.workload-identity-collision"
    collisionDecision("failed_enumeration_ordinal") =
      collision("offending")("enumeration_ordinal")
    collisionDecision("deduplication") = ujson.Null
    collisionDecision("explain_request") = ujson.Null
    collisionDecision("evaluation_usage") = ujson.Null
    collisionDecision("resource_exceeded") = ujson.Null
    collisionDecision("explain_failure") = ujson.Null
    collisionDecision("collision_witness") = collision
    assertValid(collisionDecision, schemaReference("failedDecision"))

    val collisionDocument = constructedCollisionDocument(collision)
    assertValidV6(collisionDocument)
    assertEquals(
      decision(collisionDocument)("collision_witness")("prior")(
        "ordered_stage_sql"
      ).arr.size,
      decision(collisionDocument)("collision_witness")("prior")(
        "descriptor"
      )("stages").arr.size
    )

    documents.all.filter(_.result.report.physicalPortfolioAttempt.nonEmpty)
      .foreach { emitted =>
        assertEquals(
          PostgreSqlPhysicalPortfolioCollisionProjection.from(
            emitted.result.report.physicalPortfolioAttempt.get.decision
          ),
          None
        )
        val nonCollision = parseV6(emitted.rendered)
        val physicalDecision = decision(nonCollision)
        physicalDecision("collision_witness") = collision
        assertInvalidV6(nonCollision)
      }
  }

  test("v6 schema couples top-level state and all eight decision failure arms") {
    val selectedAsFailed = parseV6(documents.selected.rendered)
    selectedAsFailed("status") = "failed"
    selectedAsFailed("exit_code") = 4
    assertInvalidV6(selectedAsFailed)

    val leakedArtifact = parseV6(documents.noAdmissible.rendered)
    leakedArtifact("report")("selected_plan") =
      parseV6(documents.selected.rendered)("report")("selected_plan")
    assertInvalidV6(leakedArtifact)

    val wrongFailureStage = parseV6(documents.enumerationFailure.rendered)
    decision(wrongFailureStage)("failure_stage") = "evaluator"
    assertInvalidV6(wrongFailureStage)

    val evidenceInvalid = parseV6(documents.evidenceInvalid.rendered)
    val missingEvidenceBatch = ujson.read(evidenceInvalid.render())
    decision(missingEvidenceBatch)("explain_batch") = ujson.Null
    assertInvalidV6(missingEvidenceBatch)

    val leakedEvidenceFailure = ujson.read(evidenceInvalid.render())
    decision(leakedEvidenceFailure)("explain_failure") =
      decision(parseV6(documents.missingEvaluator.rendered))("explain_failure")
    assertInvalidV6(leakedEvidenceFailure)

    val leakedEvidenceCollision = ujson.read(evidenceInvalid.render())
    decision(leakedEvidenceCollision)("collision_witness") =
      constructedCollisionDocument(
        ujson.Obj(
          "prior" -> collisionVector(
            decision(documents.rawResourceFailure)("raw_render_prefix")(0)
          ),
          "offending" -> collisionVector(
            decision(documents.rawResourceFailure)("raw_render_prefix")(1)
          )
        )
      )("report")("physical_portfolio")("decision")("collision_witness")
    assertInvalidV6(leakedEvidenceCollision)

    val emittedKinds = Vector(
      documents.enumerationFailure,
      documents.planningFailure,
      documents.renderingFailure,
      documents.rawResourceFailure,
      documents.envelopeFailure,
      documents.missingEvaluator,
      documents.evidenceInvalid
    ).map(value => decision(value)("failure_kind").str).toSet
    assertEquals(
      emittedKinds + collisionDecisionKind,
      AllFailureKinds
    )
  }

  test("v6 remains opt-in while every published v1-v5 schema byte stays frozen") {
    val expected = Vector(
      1 -> "cb490fd238d0a6044e5fe03f403a9a1e5257adddd76cee9635f53163662d3283",
      2 -> "b0bbff746dc349182f3fd55ae8d87acaf6bd1157c736aea89b2eabbb837f2adb",
      3 -> "c87261872c356391365c92955cf43480074fd5a4a77c9309e10ad84bcf21fdd0",
      4 -> "8bb50a5d0458797695207972d35b08c171006b8e0ce6e763f5fbdb3f439d0f8b",
      5 -> "edf059d678c1644b60beeb74e77018036211dbf2b2b45beafdeced72ad0e8ee8"
    )
    expected.foreach { case (version, digest) =>
      val bytes = Files.readAllBytes(repositoryFile(
        s"docs/journal-extension/schemas/postgresql-rewrite-artifact-v$version.schema.json"
      ))
      assertEquals(sha256(bytes), digest, clues(version))
      assertEquals(
        ujson.read(new String(bytes, StandardCharsets.UTF_8))("properties")(
          "schema_version"
        )("const").str,
        s"yannakakis-postgresql-rewrite/v$version"
      )
    }

    documents.all.foreach { emitted =>
      assertEquals(
        ujson.read(emitted.rendered)("schema_version").str,
        PostgreSqlRewriteJson.PhysicalPortfolioSchemaVersion
      )
      assertNoDuplicateKeys(emitted.rendered)
    }
    assertNoDuplicateKeys(v6SchemaText)
    assertEquals(
      v6Schema("$schema").str,
      "https://json-schema.org/draft/2020-12/schema"
    )
    assertEquals(
      v6Schema("properties")("schema_version")("const").str,
      PostgreSqlRewriteJson.PhysicalPortfolioSchemaVersion
    )
    assertEquals(
      sha256(Files.readAllBytes(v6SchemaPath)),
      "ae3df2a46439129c74b41b965198d63737a29f12150b48634a03c592ad88f0ae"
    )
    assertEquals(
      ujson.read(PostgreSqlRewriteJson.renderFailure(
        PostgreSqlRewriteCliFailure.invocation("safe failure")
      ))("schema_version").str,
      PostgreSqlRewriteJson.SchemaVersion
    )
  }

  private final case class Emitted(
      request: RewriteRequest,
      result: PostgreSqlRewriteResult
  ) {
    lazy val rendered: String = PostgreSqlRewriteJson.render(request, result)
  }

  private final case class Documents(
      invalidQuery: Emitted,
      analysisFailure: Emitted,
      searchRejected: Emitted,
      selected: Emitted,
      noAdmissible: Emitted,
      missingEvaluator: Emitted,
      midStageFailure: Emitted,
      unsupportedServer: Emitted,
      evidenceInvalid: Emitted,
      enumerationFailure: Emitted,
      planningFailure: Emitted,
      renderingFailure: Emitted,
      rawResourceFailure: Emitted,
      envelopeFailure: Emitted
  ) {
    val all: Vector[Emitted] = Vector(
      invalidQuery,
      analysisFailure,
      searchRejected,
      selected,
      noAdmissible,
      missingEvaluator,
      midStageFailure,
      unsupportedServer,
      evidenceInvalid,
      enumerationFailure,
      planningFailure,
      renderingFailure,
      rawResourceFailure,
      envelopeFailure
    )
  }

  private lazy val documents: Documents = {
    val invalidQuery = emit(request(""), rewriter(None))
    val analysisFailure = emit(
      request(
        "SELECT department_id, COUNT(*) AS n FROM fact_sale GROUP BY department_id"
      ),
      rewriter(None)
    )
    val searchRejected = emit(
      request(triangleSql, widthBound = 1),
      rewriter(None)
    )
    val selected = emit(
      request(triangleSql),
      rewriter(Some(new PortfolioEvaluator(request =>
        Right(successfulBatch(request, terminalRows = BigDecimal(100)))
      )))
    )
    val noAdmissible = emit(
      request(triangleSql),
      rewriter(Some(new PortfolioEvaluator(request =>
        Right(successfulBatch(request, terminalRows = BigDecimal(1001)))
      )))
    )
    val missingEvaluator = emit(request(triangleSql), rewriter(None))
    val midStageFailure = emit(
      request(triangleSql),
      rewriter(Some(new PortfolioEvaluator(request => {
        val batch = successfulBatch(request, terminalRows = BigDecimal(100))
        val failedIndex = request.workloads.head.stages.size
        Left(
          PostgreSqlPhysicalPortfolioExplainFailure(
            PostgreSqlPhysicalPortfolioExplainFailureCode.StatementFailed,
            PostgreSqlPhysicalPortfolioExplainStage.StageExplain,
            Some(failedIndex),
            Some(batch.source),
            batch.observations.take(failedIndex),
            flattened(request).size,
            None,
            None
          )
        )
      })))
    )
    val unsupportedServer = emit(
      request(triangleSql),
      rewriter(Some(new PortfolioEvaluator(request =>
        Left(
          PostgreSqlPhysicalPortfolioExplainFailure(
            PostgreSqlPhysicalPortfolioExplainFailureCode.UnsupportedServerVersion,
            PostgreSqlPhysicalPortfolioExplainStage.VersionPreflight,
            None,
            None,
            Vector.empty,
            flattened(request).size,
            Some(110000),
            Some(
              PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum
            )
          )
        )
      )))
    )
    val evidenceInvalid = Emitted(
      selected.request,
      PostgreSqlPhysicalPortfolioJsonTestMutations.asEvidenceInvalid(
        selected.result
      )
    )

    val enumerationLimits = PostgreSqlPhysicalPortfolioLimits
      .create(maxRootsPerCandidate = 1)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val enumerationFailure = emit(
      request(triangleSql),
      rewriter(None, physicalLimits = enumerationLimits)
    )
    val planningLimits = BagFactorPlanningLimits
      .create(maxBags = 1)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val planningFailure = emit(
      request(triangleSql),
      rewriter(None, bagFactorLimits = planningLimits)
    )
    val renderingLimits = PostgreSqlRenderingLimits
      .create(maxSqlCharacters = 1)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val renderingFailure = emit(
      request(triangleSql),
      rewriter(None, renderingLimits = renderingLimits)
    )

    val rawDescriptors = selected.result.report.physicalPortfolioAttempt
      .toVector
      .flatMap(_.decision match {
        case value: PostgreSqlPhysicalPortfolioDecision.Selected =>
          value.rawRenderings.map(_.workload)
        case _ => Vector.empty
      })
    assert(rawDescriptors.size >= 2)
    val maximumProbeCharacters = rawDescriptors
      .flatMap(_.stages.map(_.characterCount))
      .max
    val maximumWorkloadCharacters = rawDescriptors
      .map(_.stages.map(_.characterCount).sum)
      .max
    val rawLimits = PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(
        maxCharactersPerProbe = maximumProbeCharacters,
        maxDiagnosticCharactersPerWorkload = maximumWorkloadCharacters,
        maxRawDiagnosticCharactersRetained = maximumWorkloadCharacters
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val rawResourceFailure = emit(
      request(triangleSql),
      rewriter(None, evaluationLimits = rawLimits)
    )

    val envelopeLimits = PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(maxExplainStatementsPerSession = 2)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val envelopeFailure = emit(
      request(triangleSql),
      rewriter(None, evaluationLimits = envelopeLimits)
    )

    Documents(
      invalidQuery,
      analysisFailure,
      searchRejected,
      selected,
      noAdmissible,
      missingEvaluator,
      midStageFailure,
      unsupportedServer,
      evidenceInvalid,
      enumerationFailure,
      planningFailure,
      renderingFailure,
      rawResourceFailure,
      envelopeFailure
    )
  }

  private def emit(
      activeRequest: RewriteRequest,
      activeRewriter: PostgreSqlQueryRewriter
  ): Emitted =
    Emitted(activeRequest, activeRewriter.rewriteDetailed(activeRequest))

  private def request(sql: String, widthBound: Int = 2): RewriteRequest =
    RewriteRequest(
      sql,
      options = RewriteOptions(
        widthBound = widthBound,
        planLimit = 100,
        candidateSelection = policy,
        reductionMode = YannakakisReductionMode.FullTwoPass
      )
    )

  private lazy val planner: CalciteSqlPlanner = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
  }

  private def rewriter(
      evaluator: Option[PostgreSqlExplainCostEvaluator],
      physicalLimits: PostgreSqlPhysicalPortfolioLimits =
        PostgreSqlPhysicalPortfolioLimits.default,
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits =
        PostgreSqlPhysicalPortfolioEvaluationLimits.default,
      bagFactorLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default,
      renderingLimits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default
  ): PostgreSqlQueryRewriter =
    PostgreSqlQueryRewriter
      .create(
        planner,
        bagFactorLimits = bagFactorLimits,
        renderingLimits = renderingLimits,
        explainCostEvaluator = evaluator,
        physicalPortfolioLimits = physicalLimits,
        physicalPortfolioEvaluationLimits = evaluationLimits
      )
      .fold(problem => fail(problem.message), identity)

  private def successfulBatch(
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      terminalRows: BigDecimal
  ): PostgreSqlPhysicalPortfolioExplainBatch = {
    val source = PostgreSqlExplainSourceEvaluation(
      request.source.sourceStatementSha256,
      estimate(totalCost = BigDecimal(10), rows = BigDecimal(100))
    )
    val observations = flattened(request).zipWithIndex.map {
      case ((workload, statement), index) =>
        val directiveCost = workload.group.descriptor.materialization match {
          case PostgreSqlCteMaterializationDirective.Materialized =>
            BigDecimal(1)
          case PostgreSqlCteMaterializationDirective.NotMaterialized =>
            BigDecimal(2)
          case PostgreSqlCteMaterializationDirective.PlannerControlled =>
            BigDecimal(3)
        }
        val rows =
          if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
            terminalRows
          else directiveCost
        PostgreSqlPhysicalPortfolioExplainObservation(
          workload.group.descriptor.workloadId,
          workload.group.representative.physicalVariantId,
          statement.stageId,
          statement.statementSha256,
          estimate(
            totalCost = directiveCost + BigDecimal(index) / BigDecimal(1000),
            rows = rows,
            width = 1L
          )
        )
    }
    PostgreSqlPhysicalPortfolioExplainBatch(provenance, source, observations)
  }

  private def flattened(
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest
  ): Vector[(
      PostgreSqlPhysicalPortfolioExplainWorkload,
      PostgreSqlPhysicalPortfolioExplainStageStatement
  )] =
    request.workloads.flatMap(workload => workload.stages.map(workload -> _))

  private val provenance = PostgreSqlExplainCostProvenance(
    serverVersionNum = 180004,
    plannerSettingsFingerprintSha256 = "d" * 64,
    plannerSettingsFingerprintScheme =
      PostgreSqlExplainCostProvenance.PlannerSettingsFingerprintScheme,
    plannerSettingCount = PostgreSqlExplainCostProvenance.PlannerSettingNames.size
  )

  private def estimate(
      totalCost: BigDecimal,
      rows: BigDecimal,
      width: Long = 8L
  ): PostgreSqlExplainCostEstimate =
    PostgreSqlExplainCostEstimate(
      BigDecimal("0.125"),
      totalCost,
      rows,
      width,
      provenance
    )

  private final class PortfolioEvaluator(
      response: PostgreSqlPhysicalPortfolioExplainBatchRequest => Either[
        PostgreSqlPhysicalPortfolioExplainFailure,
        PostgreSqlPhysicalPortfolioExplainBatch
      ]
  ) extends PostgreSqlExplainCostEvaluator {
    override def evaluate(
        request: PostgreSqlExplainCostBatchRequest,
        limits: PostgreSqlExplainSelectionLimits
    ): Either[
      PostgreSqlExplainCostEvaluationFailure,
      PostgreSqlExplainCostBatch
    ] =
      Left(
        PostgreSqlExplainCostEvaluationFailure(
          PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
          PostgreSqlExplainEvaluationStage.Session,
          None,
          None,
          Vector.empty
        )
      )

    override def evaluatePhysicalPortfolio(
        request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
        limits: PostgreSqlPhysicalPortfolioEvaluationLimits
    ): Either[
      PostgreSqlPhysicalPortfolioExplainFailure,
      PostgreSqlPhysicalPortfolioExplainBatch
    ] = response(request)
  }

  private def parseV6(rendered: String): ujson.Value = {
    val document = ujson.read(rendered)
    assertEquals(
      document("schema_version").str,
      PostgreSqlRewriteJson.PhysicalPortfolioSchemaVersion
    )
    if (!schemaValidatedRenderings.containsKey(rendered)) {
      assertValidV6(document)
      schemaValidatedRenderings.put(rendered, java.lang.Boolean.TRUE)
    }
    document
  }

  private def decision(emitted: Emitted): ujson.Value =
    parseV6(emitted.rendered)("report")("physical_portfolio")("decision")

  private def decision(document: ujson.Value): ujson.Value =
    document("report")("physical_portfolio")("decision")

  private def selectedDecision(document: ujson.Value): ujson.Value =
    decision(document)

  private def assertRenderRejected(
      request: RewriteRequest,
      result: PostgreSqlRewriteResult
  ): Unit =
    intercept[IllegalArgumentException] {
      PostgreSqlRewriteJson.render(request, result)
    }

  private def assertFinalArtifactsNull(document: ujson.Value): Unit =
    Vector("selected_plan", "bag_factor_plan", "rendering").foreach(field =>
      assertEquals(document("report")(field), ujson.Null)
    )

  private def collectObjectFields(
      value: ujson.Value,
      path: String = "$"
  ): Vector[(String, String, ujson.Value)] = value match {
    case objectValue: ujson.Obj =>
      objectValue.obj.toVector.flatMap { case (name, child) =>
        val childPath = s"$path.$name"
        Vector((childPath, name, child)) ++ collectObjectFields(child, childPath)
      }
    case arrayValue: ujson.Arr =>
      arrayValue.arr.zipWithIndex.toVector.flatMap { case (child, index) =>
        collectObjectFields(child, s"$path[$index]")
      }
    case _ => Vector.empty
  }

  private def reverseArray(value: ujson.Value): Unit = {
    val reversed = value.arr.reverse.toVector
    value.arr.clear()
    value.arr ++= reversed
  }

  private val CanonicalBigIntFields: Set[String] = Set(
    "witness_count",
    "physical_cover_count",
    "node_count",
    "exact_logical_variant_count",
    "exact_root_count",
    "exact_cover_assignment_count",
    "deviation_magnitude",
    "workload_count",
    "explain_statement_count",
    "required"
  )

  private def isCanonicalBigIntField(path: String, name: String): Boolean =
    CanonicalBigIntFields(name) ||
      (name == "diagnostic_character_count" &&
        path.endsWith(".evaluation_usage.diagnostic_character_count"))

  private val collisionDecisionKind =
    "physical-portfolio.workload-identity-collision"

  private val AllFailureKinds: Set[String] = Set(
    "physical-portfolio.enumeration-failed",
    "physical-portfolio.candidate-planning-failed",
    "physical-portfolio.stage-diagnostic-rendering-failed",
    collisionDecisionKind,
    "physical-portfolio.raw-rendering-resource-limit-exceeded",
    "physical-portfolio.evaluation-envelope-limit-exceeded",
    "physical-portfolio.evaluator-failed",
    "physical-portfolio.evidence-invalid"
  )

  private def collisionVector(raw: ujson.Value): ujson.Value = {
    val descriptor = raw("workload")
    val stageCount = descriptor("stages").arr.size
    ujson.Obj(
      "enumeration_ordinal" -> raw("entry")("enumeration_ordinal"),
      "descriptor" -> ujson.read(descriptor.render()),
      "ordered_stage_sql" -> ujson.Arr(
        Vector.fill(stageCount)(ujson.Str("SELECT 1")): _*
      )
    )
  }

  /** Schema-positive collision state. End-to-end emission would require a
    * real digest collision, which the production replay correctly refuses to
    * synthesize.
    */
  private def constructedCollisionDocument(
      collision: ujson.Value
  ): ujson.Value = {
    val document = parseV6(documents.missingEvaluator.rendered)
    val physicalDecision = decision(document)
    physicalDecision("failure_kind") = collisionDecisionKind
    physicalDecision("failure_stage") = "workload-deduplication"
    physicalDecision("detail_code") = collisionDecisionKind
    physicalDecision("failed_enumeration_ordinal") =
      collision("offending")("enumeration_ordinal")
    physicalDecision("deduplication") = ujson.Null
    physicalDecision("explain_request") = ujson.Null
    physicalDecision("evaluation_usage") = ujson.Null
    physicalDecision("resource_exceeded") = ujson.Null
    physicalDecision("explain_failure") = ujson.Null
    physicalDecision("collision_witness") = ujson.read(collision.render())
    val context = document("outcome")("diagnostics")(0)("context")
    context("algorithmCode") = collisionDecisionKind
    context("portfolioStage") = "workload-deduplication"
    context("failureKind") = collisionDecisionKind
    document
  }

  private lazy val v6SchemaPath: Path = repositoryFile(
    "docs/journal-extension/schemas/postgresql-rewrite-artifact-v6.schema.json"
  )
  private lazy val v6SchemaText: String = Files.readString(v6SchemaPath)
  private lazy val v6Schema: ujson.Value = ujson.read(v6SchemaText)

  // The same immutable emitted String is parsed repeatedly for independent
  // hostile copies. Validate its full schema once per object identity; each
  // mutated copy is still validated explicitly after mutation.
  private val schemaValidatedRenderings =
    new IdentityHashMap[String, java.lang.Boolean]()

  private def schemaReference(definition: String): ujson.Value =
    ujson.Obj(
      "$defs" -> v6Schema("$defs"),
      "$ref" -> ujson.Str(s"#/$$defs/$definition")
    )

  private def assertValidV6(document: ujson.Value): Unit =
    assertValid(document, v6Schema)

  private def assertInvalidV6(document: ujson.Value): Unit =
    assertInvalid(document, v6Schema)

  private def assertValid(
      instance: ujson.Value,
      schema: ujson.Value
  ): Unit = {
    val violations =
      PostgreSqlPhysicalPortfolioV6SchemaTestValidator.validate(instance, schema)
    assert(
      violations.isEmpty,
      violations.map(value => s"${value.path}: ${value.message}").mkString("\n")
    )
  }

  private def assertInvalid(
      instance: ujson.Value,
      schema: ujson.Value
  ): Unit =
    assert(
      PostgreSqlPhysicalPortfolioV6SchemaTestValidator
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
