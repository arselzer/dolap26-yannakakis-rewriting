package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
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
  PostgreSqlCountStageCostContractTestSupport,
  PostgreSqlExplainCostFailureCode,
  PostgreSqlExplainEvaluationStage,
  PostgreSqlFactorAwareContractTestSupport,
  PostgreSqlFactorAwareExplainFailure
}

class PostgreSqlCountStageCostV7JsonContractSuite extends munit.FunSuite {
  import PostgreSqlCountStageCostContractTestSupport._
  import PostgreSqlFactorAwareContractTestSupport.{
    RecordingFactorEvaluator,
    estimate,
    successfulBatch
  }

  private lazy val emitted = {
    val (rewriteRequest, result) = successfulDocument()
    rewriteRequest -> PostgreSqlRewriteJson.render(rewriteRequest, result)
  }
  private lazy val schemaPath = repositoryFile(
    "docs/journal-extension/schemas/postgresql-rewrite-artifact-v7.schema.json"
  )
  private lazy val schema = ujson.read(
    new String(Files.readAllBytes(schemaPath), StandardCharsets.UTF_8)
  )

  test("CLI exposes one exact opt-in spelling and actionable K/mode constraints") {
    val environment = Map(
      PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
        "jdbc:postgresql://localhost/database",
      PostgreSqlRewriteCliConfig.UserEnvironment -> "user",
      PostgreSqlRewriteCliConfig.PasswordEnvironment -> "password",
      PostgreSqlRewriteCliConfig.SchemaEnvironment -> "public"
    )
    def parse(extra: Vector[String]) =
      PostgreSqlRewriteCliConfig.parse(
        (Vector("rewrite", "--stdin") ++ extra).toArray,
        environment
      )

    parse(Vector(
      "--selection",
      "postgresql-explain-count-stage-cost-progressive-v1",
      "--plan-limit",
      "100",
      "--reduction-schedule",
      "full",
      "--explain-timeout-seconds",
      "7"
    )) match {
      case Right(PostgreSqlRewriteCliCommand.Rewrite(config)) =>
        assertEquals(config.options.candidateSelection, policy)
        assertEquals(config.options.planLimit, 100)
        assertEquals(
          config.options.reductionMode,
          YannakakisReductionMode.FullTwoPass
        )
        assertEquals(config.factorAwareLimits.statementTimeoutSeconds, 7)
      case other => fail(s"v7 CLI did not parse: $other")
    }

    val missingK = parse(Vector(
      "--selection",
      "postgresql-explain-count-stage-cost-progressive-v1"
    )).swap.getOrElse(fail("default K=10 must be rejected"))
    assertEquals(missingK.context("option"), "--plan-limit")
    assertEquals(missingK.context("valueSource"), "default")
    assertEquals(missingK.context("required"), "100")

    val wrongMode = parse(Vector(
      "--selection",
      "postgresql-explain-count-stage-cost-progressive-v1",
      "--plan-limit",
      "100",
      "--reduction-schedule",
      "bottom-up"
    )).swap.getOrElse(fail("non-full schedule must be rejected"))
    assertEquals(wrongMode.context("option"), "--reduction-schedule")
    assertEquals(wrongMode.context("required"), "full")

    assert(
      parse(Vector(
        "--selection",
        "postgresql_explain_count_stage_cost_progressive_v1",
        "--plan-limit",
        "100"
      )).isLeft
    )
    assert(
      PostgreSqlRewriteCliConfig.Usage.contains(
        "postgresql-explain-count-stage-cost-progressive-v1"
      )
    )
  }

  test("v7 serializer emits a distinct strict schema and exact score key") {
    val document = ujson.read(emitted._2)
    assertEquals(
      document("schema_version").str,
      PostgreSqlRewriteJson.CountStageCostSchemaVersion
    )
    assertEquals(
      document("request")("options")("candidate_selection")("policy").str,
      policy.id
    )
    assertEquals(
      document("request")("options")("reduction_schedule").str,
      "full"
    )
    val selection = document("report")("candidate_selection")
    assertEquals(
      selection("cost_order").arr.map(_.str).toVector,
      at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCountStageCostProgressiveV1.ScoreOrder
    )
    assert(selection.obj.contains("count_stage_cost"))
    assert(!selection.obj.contains("factor_aware"))
    val evidence = selection("evaluations")(0)("count_stage_cost_evidence")
    assert(evidence("score").obj.contains("sum_named_stage_plan_total_cost"))
    assertValid(document)
    assertEquals(
      sha256(Files.readAllBytes(schemaPath)),
      "98ed10cdd27c92809d902cbaf9e904685172e8987bf3d58f4ffbdbe08ea1ff28"
    )
  }

  test("v7 schema rejects score, policy, mode, ordering, and extension mutations") {
    def mutate(body: ujson.Value => Unit): ujson.Value = {
      val value = ujson.read(emitted._2)
      body(value)
      value
    }

    val missingLeadingScore = mutate { value =>
      value("report")("candidate_selection")("evaluations")(0)(
        "count_stage_cost_evidence"
      )("score").obj.remove("sum_named_stage_plan_total_cost")
    }
    val negativeLeadingScore = mutate { value =>
      value("report")("candidate_selection")("evaluations")(0)(
        "count_stage_cost_evidence"
      )("score")("sum_named_stage_plan_total_cost") = -1
    }
    val wrongPolicy = mutate { value =>
      value("request")("options")("candidate_selection")("policy") =
        "postgresql_explain_factor_aware_progressive_v1"
    }
    val wrongMode = mutate { value =>
      value("request")("options")("reduction_schedule") = "none"
    }
    val wrongOrder = mutate { value =>
      value("report")("candidate_selection")("cost_order") = ujson.Arr(
        "max_named_stage_plan_rows_times_plan_width",
        "sum_named_stage_plan_total_cost",
        "sum_named_stage_plan_rows_times_plan_width",
        "final_plan_total_cost",
        "structural_rank",
        "candidate_id"
      )
    }
    val extension = mutate { value =>
      value("report")("candidate_selection")("unversioned_extension") = true
    }
    val swappedCheckpoints = mutate { value =>
      val checkpoints = value("report")("candidate_selection")(
        "count_stage_cost"
      )("checkpoints").arr
      val first = checkpoints(0)
      checkpoints(0) = checkpoints(1)
      checkpoints(1) = first
    }
    val hostileEnvelope = mutate { value =>
      value("report")("candidate_selection")("count_stage_cost")(
        "numeric_envelope"
      )("max_absolute_scale") = 1025
    }

    Vector(
      missingLeadingScore,
      negativeLeadingScore,
      wrongPolicy,
      wrongMode,
      wrongOrder,
      extension,
      swappedCheckpoints,
      hostileEnvelope
    ).foreach(assertInvalid)
  }

  test(
    "v7 schema accepts the exact early, rejected, failure, no-admissible, and selected state matrix"
  ) {
    val earlyEvaluator = calibratedEvaluator()
    val earlyRequest = request(groupedCountSql)
    val earlyResult = rewrite(earlyRequest, earlyEvaluator)
    assert(earlyResult.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
    assertEquals(earlyEvaluator.factorCalls, 0)

    val rejectedEvaluator = calibratedEvaluator()
    val rejectedRequest = request().copy(
      options = request().options.copy(widthBound = 1)
    )
    val rejectedResult = rewrite(rejectedRequest, rejectedEvaluator)
    assert(rejectedResult.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
    assertEquals(rejectedEvaluator.factorCalls, 0)

    val unavailableRequest = request()
    val unavailableResult = rewriteWithoutEvaluator(unavailableRequest)
    assert(unavailableResult.outcome.isInstanceOf[RewriteOutcome.Failed])

    val midStageEvaluator = new RecordingFactorEvaluator((probe, _) => {
      val batch = successfulBatch(probe, sourceRows = BigDecimal(1))()
      val failedIndex = probe.candidates.head.stages.size
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
    val midStageRequest = request()
    val midStageResult = rewrite(midStageRequest, midStageEvaluator)
    assert(midStageResult.outcome.isInstanceOf[RewriteOutcome.Failed])

    val noAdmissibleEvaluator = new RecordingFactorEvaluator((probe, _) =>
      Right(
        successfulBatch(probe, sourceRows = BigDecimal(100)) {
          (candidate, statement, index) =>
            if (
              statement.stageId == PostgreSqlStageId.FinalReconstruction
            )
              estimate(
                totalCost = BigDecimal(candidate.structuralRank),
                rows = BigDecimal(0),
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
    val noAdmissibleRequest = request()
    val noAdmissibleResult = rewrite(
      noAdmissibleRequest,
      noAdmissibleEvaluator
    )
    assert(noAdmissibleResult.outcome.isInstanceOf[RewriteOutcome.Failed])

    val (selectedRequest, selectedResult) = successfulDocument()
    val states = Vector(
      "early" -> PostgreSqlRewriteJson.render(earlyRequest, earlyResult),
      "rejected" -> PostgreSqlRewriteJson.render(
        rejectedRequest,
        rejectedResult
      ),
      "unavailable" -> PostgreSqlRewriteJson.render(
        unavailableRequest,
        unavailableResult
      ),
      "mid-stage" -> PostgreSqlRewriteJson.render(
        midStageRequest,
        midStageResult
      ),
      "no-admissible" -> PostgreSqlRewriteJson.render(
        noAdmissibleRequest,
        noAdmissibleResult
      ),
      "selected" -> PostgreSqlRewriteJson.render(
        selectedRequest,
        selectedResult
      )
    )

    states.foreach { case (name, rendered) =>
      val document = ujson.read(rendered)
      assertEquals(
        document("schema_version").str,
        PostgreSqlRewriteJson.CountStageCostSchemaVersion,
        clues(name)
      )
      assertValid(document)
    }

    assertEquals(
      ujson.read(states(0)._2)("report")("candidate_selection"),
      ujson.Null
    )
    assertEquals(
      ujson.read(states(1)._2)("report")("candidate_search")("decision").str,
      "rejected"
    )
    assertEquals(
      ujson.read(states(2)._2)("report")("candidate_selection")("decision").str,
      "count_stage_cost_evaluation_failed"
    )
    assertEquals(
      ujson.read(states(3)._2)("report")("candidate_selection")(
        "explain_failure_stage"
      ).str,
      "stage-explain"
    )
    assertEquals(
      ujson.read(states(4)._2)("report")("candidate_selection")("decision").str,
      "no_admissible_estimate"
    )
    assertEquals(
      ujson.read(states(5)._2)("report")("candidate_selection")("decision").str,
      "selected"
    )
  }

  test("v7 canonicalizes every noncanonical decimal zero in score evidence") {
    val evaluator = new RecordingFactorEvaluator((probe, _) =>
      Right(
        successfulBatch(probe, sourceRows = BigDecimal(1)) {
          (_, _, _) =>
            estimate(
              totalCost = BigDecimal("0E+100"),
              rows = BigDecimal(1),
              width = 0L
            )
        }
      )
    )
    val rewriteRequest = request()
    val result = rewrite(rewriteRequest, evaluator)
    assert(result.outcome.isInstanceOf[RewriteOutcome.Rewritten])

    val rendered = PostgreSqlRewriteJson.render(rewriteRequest, result)
    assert(!rendered.contains("0E+"), clues(rendered))
    val document = ujson.read(rendered)
    val evaluations = document("report")("candidate_selection")(
      "evaluations"
    ).arr
    evaluations.foreach { evaluation =>
      val score = evaluation("count_stage_cost_evidence")("score")
      assertEquals(score("sum_named_stage_plan_total_cost").num, 0.0)
      assertEquals(
        score("max_named_stage_plan_rows_times_plan_width").num,
        0.0
      )
      assertEquals(
        score("sum_named_stage_plan_rows_times_plan_width").num,
        0.0
      )
      assertEquals(score("final_plan_total_cost").num, 0.0)
    }
    assertValid(document)
  }

  test("v7 leaves all v1-v6 schema bytes and a canonical v5 document frozen") {
    val expectedSchemas = Vector(
      1 -> "cb490fd238d0a6044e5fe03f403a9a1e5257adddd76cee9635f53163662d3283",
      2 -> "b0bbff746dc349182f3fd55ae8d87acaf6bd1157c736aea89b2eabbb837f2adb",
      3 -> "c87261872c356391365c92955cf43480074fd5a4a77c9309e10ad84bcf21fdd0",
      4 -> "8bb50a5d0458797695207972d35b08c171006b8e0ce6e763f5fbdb3f439d0f8b",
      5 -> "edf059d678c1644b60beeb74e77018036211dbf2b2b45beafdeced72ad0e8ee8",
      6 -> "ae3df2a46439129c74b41b965198d63737a29f12150b48634a03c592ad88f0ae"
    )
    expectedSchemas.foreach { case (version, digest) =>
      val bytes = Files.readAllBytes(repositoryFile(
        s"docs/journal-extension/schemas/postgresql-rewrite-artifact-v$version.schema.json"
      ))
      assertEquals(sha256(bytes), digest, clues(version))
    }

    val v5Request = RewriteRequest(
      countSql,
      options = RewriteOptions(
        planLimit = RewriteOptions.FactorAwareProgressivePlanLimit,
        candidateSelection =
          CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1,
        reductionMode = YannakakisReductionMode.FullTwoPass
      )
    )
    val rendered = PostgreSqlRewriteJson.render(
      v5Request,
      rewrite(v5Request, calibratedEvaluator())
    )
    assertEquals(
      ujson.read(rendered)("schema_version").str,
      PostgreSqlRewriteJson.FactorAwareSchemaVersion
    )
    assert(!rendered.contains("count_stage_cost"))
    assertEquals(
      sha256(rendered.getBytes(StandardCharsets.UTF_8)),
      "62149bfb44a4af49c48c3112db2ad4e009ef90ec38fc345ffcd08e086bf235f2"
    )

    val blankSql = " \n\t "
    val legacyRequest = RewriteRequest(blankSql)
    val legacyRendered = PostgreSqlRewriteJson.render(
      legacyRequest,
      rewriteWithoutEvaluator(legacyRequest)
    )
    assertEquals(
      ujson.read(legacyRendered)("schema_version").str,
      PostgreSqlRewriteJson.SchemaVersion
    )
    assert(!legacyRendered.contains("count_stage_cost"))
    assertEquals(
      sha256(legacyRendered.getBytes(StandardCharsets.UTF_8)),
      "d7a4e739aa285aa75c34ee694da81070778cac1b115d4b99b9fe2607ed8d2e60"
    )

    val portfolioRequest = RewriteRequest(
      blankSql,
      options = RewriteOptions(
        planLimit = RewriteOptions.PhysicalPortfolioPlanLimit,
        candidateSelection =
          CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1,
        reductionMode = YannakakisReductionMode.FullTwoPass
      )
    )
    val portfolioRendered = PostgreSqlRewriteJson.render(
      portfolioRequest,
      rewriteWithoutEvaluator(portfolioRequest)
    )
    assertEquals(
      ujson.read(portfolioRendered)("schema_version").str,
      PostgreSqlRewriteJson.PhysicalPortfolioSchemaVersion
    )
    assert(!portfolioRendered.contains("count_stage_cost"))
    assertEquals(
      sha256(portfolioRendered.getBytes(StandardCharsets.UTF_8)),
      "9f25833d0975aef0f2d0f6dfae13852dd3784b18dfd424bf513e470ddee87b23"
    )
  }

  private def assertValid(value: ujson.Value): Unit = {
    val errors = PostgreSqlFactorAwareV5SchemaTestValidator.validate(value, schema)
    assertEquals(errors, Vector.empty)
  }

  private def assertInvalid(value: ujson.Value): Unit =
    assert(
      PostgreSqlFactorAwareV5SchemaTestValidator.validate(value, schema).nonEmpty
    )

  private def repositoryFile(relative: String): Path = {
    var current = Paths.get("").toAbsolutePath.normalize()
    while (current != null) {
      val candidate = current.resolve(relative)
      if (Files.isRegularFile(candidate)) return candidate
      current = current.getParent
    }
    fail(s"could not locate repository file $relative")
  }

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
