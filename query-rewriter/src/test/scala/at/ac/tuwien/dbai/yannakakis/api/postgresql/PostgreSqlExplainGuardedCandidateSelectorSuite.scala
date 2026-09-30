package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidatePlanId,
  CandidateSearchInput,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlRenderingLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  TestColumn,
  TestTable
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteQueryAnalyzer,
  QueryIR
}

class PostgreSqlExplainGuardedCandidateSelectorSuite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  private val triangleTables = Vector(
    TestTable(
      "triangle_r",
      Vector(TestColumn("row_id"), TestColumn("a"), TestColumn("b"))
    ),
    TestTable(
      "triangle_s",
      Vector(TestColumn("row_id"), TestColumn("b"), TestColumn("c"))
    ),
    TestTable(
      "triangle_t",
      Vector(TestColumn("row_id"), TestColumn("c"), TestColumn("a"))
    )
  )

  private val provenance = PostgreSqlExplainCostProvenance(
    serverVersionNum = 180004,
    plannerSettingsFingerprintSha256 = "a" * 64,
    plannerSettingsFingerprintScheme =
      PostgreSqlExplainCostProvenance.PlannerSettingsFingerprintScheme,
    plannerSettingCount = PostgreSqlExplainCostProvenance.PlannerSettingNames.size
  )

  test("row guard is exact, inclusive at both 10x boundaries, and uses max(source, 1)") {
    val cases = Vector(
      (BigDecimal("10"), BigDecimal("1"), BigDecimal("10"), true),
      (BigDecimal("10"), BigDecimal("100"), BigDecimal("10"), true),
      (BigDecimal("10"), BigDecimal("0.999999999999999999"), BigDecimal("10"), false),
      (BigDecimal("10"), BigDecimal("100.000000000000000001"), BigDecimal("10"), false),
      (BigDecimal("0"), BigDecimal("0"), BigDecimal("1"), false),
      (BigDecimal("0"), BigDecimal("0.1"), BigDecimal("1"), true),
      (BigDecimal("0"), BigDecimal("10"), BigDecimal("1"), true),
      (BigDecimal("0"), BigDecimal("10.000000000000000001"), BigDecimal("1"), false),
      (BigDecimal("0.5"), BigDecimal("0.1"), BigDecimal("1"), true)
    )

    cases.foreach { case (sourceRows, candidateRows, denominator, within) =>
      val evidence = PostgreSqlExplainRowGuardEvidence.from(
        sourceRows,
        candidateRows
      )
      assertEquals(evidence.factor, 10)
      assertEquals(evidence.ratioNumerator, candidateRows)
      assertEquals(evidence.ratioDenominator, denominator)
      assertEquals(evidence.withinGuard, within, clues(sourceRows, candidateRows))
    }
  }

  test("guarded selection rejects an implausibly cheap estimate and orders admissible ties by rank") {
    val fixture = preparedTriangle()
    val evaluator = successEvaluator(
      sourceRows = BigDecimal("100"),
      candidateCosts = Vector("0.01", "5", "5.0", "8").map(BigDecimal(_)),
      candidateRows = Vector("1000.0001", "200", "200", "9.9999").map(BigDecimal(_))
    )

    val selected = guarded(fixture, Some(evaluator))
      .fold(problem => fail(problem.toString), identity)

    assertEquals(evaluator.calls, 1)
    assertEquals(selected.plan.rank, 2)
    assertEquals(
      selected.decision.policy,
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1
    )
    assertEquals(selected.decision.sourceEvaluation.map(_.estimate.planRows), Some(BigDecimal(100)))
    assertEquals(
      selected.decision.evaluations.map(_.rowGuardEvidence.map(_.withinGuard)),
      Vector(Some(false), Some(true), Some(true), Some(false))
    )
    assertEquals(
      selected.decision.evaluations.map(_.explainCostEstimate.map(_.planTotalCost)),
      Vector("0.01", "5", "5.0", "8").map(value => Some(BigDecimal(value)))
    )
    assertEquals(selected.decision.costSelected, true)
    assertEquals(selected.decision.estimatedCostSelected, true)
    assertEquals(selected.decision.measuredRuntimeSelected, false)
    assertEquals(selected.decision.usesCatalogStatistics, true)
    assertEquals(selected.decision.usesExecutionFeedback, false)
  }

  test("guarded total-cost ordering ends with candidate id after cost and rank") {
    val lowerCost = syntheticEvaluation(candidateId("f"), 9, "0.99")
    val higherCost = syntheticEvaluation(candidateId("0"), 1, "1")
    assert(PostgreSqlCandidateSelectionDecision.explainOrdering.lt(lowerCost, higherCost))

    val lowerRank = syntheticEvaluation(candidateId("f"), 1, "4")
    val higherRank = syntheticEvaluation(candidateId("0"), 2, "4.0")
    assert(PostgreSqlCandidateSelectionDecision.explainOrdering.lt(lowerRank, higherRank))

    val lowerId = syntheticEvaluation(candidateId("0"), 3, "8")
    val higherId = syntheticEvaluation(candidateId("f"), 3, "8.00")
    assert(PostgreSqlCandidateSelectionDecision.explainOrdering.lt(lowerId, higherId))
    assertEquals(
      PostgreSqlExplainCostEstimate.OrderedFields,
      Vector("plan_total_cost", "structural_rank", "candidate_id")
    )
  }

  test("no admissible estimate retains every evaluation and source without fallback") {
    val fixture = preparedTriangle()
    val evaluator = successEvaluator(
      sourceRows = BigDecimal(100),
      candidateCosts = Vector("1", "2", "3", "4").map(BigDecimal(_)),
      candidateRows = Vector("1001", "2000", "0", "9.99").map(BigDecimal(_))
    )

    guarded(fixture, Some(evaluator)) match {
      case Left(failure: PostgreSqlCandidateSelectionFailure.NoAdmissibleEstimate) =>
        val report = failure.report
        assertEquals(report.evaluations.map(_.structuralRank), Vector(1, 2, 3, 4))
        assert(report.evaluations.forall(
          _.rowGuardEvidence.exists(evidence => !evidence.withinGuard)
        ))
        assertEquals(report.sourceEvaluation.map(_.estimate.planRows), Some(BigDecimal(100)))
        assertEquals(report.explainCostProvenance, Some(provenance))
      case other => fail(s"expected typed no-admissible decision, received $other")
    }
  }

  test("source failure has no source or candidate identity and candidate failure retains exact prefix") {
    val fixture = preparedTriangle()
    val sourceFailure = new RecordingEvaluator(_ =>
      Left(
        PostgreSqlExplainCostEvaluationFailure(
          PostgreSqlExplainCostFailureCode.StatementTimedOut,
          PostgreSqlExplainEvaluationStage.SourceExplain,
          failedCandidateIndex = None,
          source = None,
          completed = Vector.empty
        )
      )
    )
    guarded(fixture, Some(sourceFailure)) match {
      case Left(failure: PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed) =>
        assertEquals(failure.code, PostgreSqlExplainCostFailureCode.StatementTimedOut)
        assertEquals(failure.plan, None)
        assertEquals(failure.report.stage, PostgreSqlExplainEvaluationStage.SourceExplain)
        assertEquals(failure.report.sourceEvaluation, None)
        assertEquals(failure.report.evaluations, Vector.empty)
        assertEquals(failure.report.failedCandidateId, None)
      case other => fail(s"expected source EXPLAIN failure, received $other")
    }

    val candidateFailure = new RecordingEvaluator(request => {
      val source = sourceEvaluation(request.source, planRows = BigDecimal(100))
      Left(
        PostgreSqlExplainCostEvaluationFailure(
          PostgreSqlExplainCostFailureCode.PlanJsonMalformed,
          PostgreSqlExplainEvaluationStage.CandidateExplain,
          failedCandidateIndex = Some(1),
          source = Some(source),
          completed = Vector(
            observation(
              request.statements.head,
              estimate(totalCost = BigDecimal(2), planRows = BigDecimal(100))
            )
          )
        )
      )
    })
    guarded(fixture, Some(candidateFailure)) match {
      case Left(failure: PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed) =>
        assertEquals(failure.code, PostgreSqlExplainCostFailureCode.PlanJsonMalformed)
        assertEquals(failure.plan.map(_.rank), Some(2))
        assertEquals(failure.report.stage, PostgreSqlExplainEvaluationStage.CandidateExplain)
        assertEquals(failure.report.failedStructuralRank, Some(2))
        assertEquals(failure.report.evaluations.map(_.structuralRank), Vector(1))
        assertEquals(
          failure.report.evaluations.head.rowGuardEvidence.map(_.withinGuard),
          Some(true)
        )
        assertEquals(failure.report.sourceEvaluation.map(_.estimate.planRows), Some(BigDecimal(100)))
      case other => fail(s"expected exact candidate prefix failure, received $other")
    }
  }

  test("missing evaluator and hostile evaluator responses fail as typed contracts without fallback") {
    val fixture = preparedTriangle()
    guarded(fixture, None) match {
      case Left(failure: PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed) =>
        assertEquals(failure.code, PostgreSqlExplainCostFailureCode.EvaluatorUnavailable)
        assertEquals(failure.plan, None)
        assertEquals(failure.report.stage, PostgreSqlExplainEvaluationStage.Session)
        assertEquals(failure.report.evaluations, Vector.empty)
      case other => fail(s"expected evaluator-unavailable, received $other")
    }

    val secret = "never-report-this-password"
    val throwing = new RecordingEvaluator(_ =>
      throw new RuntimeException(s"$secret SELECT payload FROM private_table")
    )
    val failure = guarded(fixture, Some(throwing)) match {
      case Left(value: PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed) =>
        value
      case other => fail(s"expected evaluator contract violation, received $other")
    }
    assertEquals(
      failure.code,
      PostgreSqlExplainCostFailureCode.EvaluatorContractViolation
    )
    assert(!failure.toString.contains(secret))
    assert(!failure.toString.contains("private_table"))
    assert(!failure.toString.contains(classOf[RuntimeException].getName))
  }

  test("selector rejects wrong source digest, count, order, candidate digest, and failure prefix") {
    val fixture = preparedTriangle()
    val mutations = Vector[(String, PostgreSqlExplainCostBatchRequest => Either[
      PostgreSqlExplainCostEvaluationFailure,
      PostgreSqlExplainCostBatch
    ])](
      "wrong-source-digest" -> { request =>
        val valid = successfulBatch(
          request,
          BigDecimal(100),
          Vector.fill(4)(BigDecimal(100)),
          Vector(1, 2, 3, 4).map(BigDecimal(_))
        )
        Right(
          valid.copy(
            source = valid.source.copy(sourceStatementSha256 = "0" * 64)
          )
        )
      },
      "missing-candidate" -> { request =>
        val valid = successfulBatch(
          request,
          BigDecimal(100),
          Vector.fill(4)(BigDecimal(100)),
          Vector(1, 2, 3, 4).map(BigDecimal(_))
        )
        Right(valid.copy(observations = valid.observations.dropRight(1)))
      },
      "reordered-candidates" -> { request =>
        val valid = successfulBatch(
          request,
          BigDecimal(100),
          Vector.fill(4)(BigDecimal(100)),
          Vector(1, 2, 3, 4).map(BigDecimal(_))
        )
        Right(valid.copy(observations = valid.observations.reverse))
      },
      "wrong-candidate-digest" -> { request =>
        val valid = successfulBatch(
          request,
          BigDecimal(100),
          Vector.fill(4)(BigDecimal(100)),
          Vector(1, 2, 3, 4).map(BigDecimal(_))
        )
        val first = valid.observations.head.copy(renderedStatementSha256 = "0" * 64)
        Right(valid.copy(observations = first +: valid.observations.tail))
      },
      "non-prefix-failure" -> { request =>
        val source = sourceEvaluation(request.source, BigDecimal(100))
        Left(
          PostgreSqlExplainCostEvaluationFailure(
            PostgreSqlExplainCostFailureCode.StatementFailed,
            PostgreSqlExplainEvaluationStage.Provenance,
            failedCandidateIndex = None,
            source = Some(source),
            completed = Vector(
              observation(
                request.statements(1),
                estimate(planRows = BigDecimal(100))
              )
            )
          )
        )
      }
    )

    mutations.foreach { case (label, response) =>
      guarded(fixture, Some(new RecordingEvaluator(response))) match {
        case Left(failure: PostgreSqlCandidateSelectionFailure.ExplainCostEvaluationFailed) =>
          assertEquals(
            failure.code,
            PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
            label
          )
          assertEquals(failure.report.evaluations, Vector.empty, label)
          assertEquals(failure.report.sourceEvaluation, None, label)
        case other => fail(s"$label should fail the evaluator contract, received $other")
      }
    }
  }

  test("structural and static policies remain byte-identical and never invoke EXPLAIN") {
    val fixture = preparedTriangle()
    val exploding = new RecordingEvaluator(_ =>
      throw new AssertionError("non-cost policy invoked EXPLAIN")
    )
    val policies = Vector[CandidatePlanSelection](
      CandidatePlanSelection.StructuralHead,
      CandidatePlanSelection.StructuralRank(3),
      CandidatePlanSelection.ExactCandidateId(fixture.found.plans(1).candidateId),
      CandidatePlanSelection.PostgreSqlStaticShapeV1
    )

    policies.foreach { policy =>
      val baseline = select(fixture, policy, None, None)
        .fold(problem => fail(problem.toString), identity)
      val withCapability = select(
        fixture,
        policy,
        Some(exploding),
        Some(triangleSql)
      ).fold(problem => fail(problem.toString), identity)
      assertEquals(withCapability.plan, baseline.plan)
      assertEquals(withCapability.bagFactorPlan, baseline.bagFactorPlan)
      assertEquals(withCapability.renderedQuery, baseline.renderedQuery)
      assertEquals(withCapability.decision, baseline.decision)
      assert(withCapability.decision.evaluations.forall(
        _.explainCostEstimate.isEmpty
      ))
      assert(withCapability.decision.evaluations.forall(
        _.rowGuardEvidence.isEmpty
      ))
    }
    assertEquals(exploding.calls, 0)
  }

  private final class RecordingEvaluator(
      response: PostgreSqlExplainCostBatchRequest => Either[
        PostgreSqlExplainCostEvaluationFailure,
        PostgreSqlExplainCostBatch
      ]
  ) extends PostgreSqlExplainCostEvaluator {
    var calls: Int = 0
    var requests: Vector[PostgreSqlExplainCostBatchRequest] = Vector.empty

    override def evaluate(
        request: PostgreSqlExplainCostBatchRequest,
        limits: PostgreSqlExplainSelectionLimits
    ): Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch] = {
      calls += 1
      requests :+= request
      assertEquals(limits, PostgreSqlExplainSelectionLimits.default)
      assertEquals(request.statements.map(_.structuralRank), Vector(1, 2, 3, 4))
      assertEquals(request.source.sql, triangleSql)
      response(request)
    }
  }

  private final case class Prepared(
      query: QueryIR,
      input: CandidateSearchInput,
      found: CandidatePlanDecision.Found
  )

  private def preparedTriangle(): Prepared = {
    val planner = CalcitePlannerFixture(triangleTables)
    val analysis = CalciteQueryAnalyzer.analyze(planner.plan(triangleSql).root)
    assert(analysis.isSupported, clues(analysis.diagnostics))
    val hypergraph = analysis.hypergraph
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val generated = SoftBagGenerator
      .generate(hypergraph, widthBound = 2)
      .fold(problem => fail(problem.message), identity)
    val input = CandidateSearchInput
      .create(
        hypergraph,
        widthBound = 2,
        generated.admissible(requireConnectedCover = true)
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val found = TopKCandidateTD
      .enumerate(input, planLimit = 10)
      .fold(problem => fail(problem.message), identity) match {
      case value: CandidatePlanDecision.Found => value
      case rejected: CandidatePlanDecision.Rejected =>
        fail(s"triangle unexpectedly rejected: $rejected")
    }
    assertEquals(found.plans.size, 4)
    Prepared(analysis.query, input, found)
  }

  private def guarded(
      fixture: Prepared,
      evaluator: Option[PostgreSqlExplainCostEvaluator]
  ): Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate] =
    select(
      fixture,
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1,
      evaluator,
      Some(triangleSql)
    )

  private def select(
      fixture: Prepared,
      policy: CandidatePlanSelection,
      evaluator: Option[PostgreSqlExplainCostEvaluator],
      sourceSql: Option[String]
  ): Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate] =
    PostgreSqlCandidateSelector.select(
      fixture.query,
      fixture.input,
      fixture.found,
      policy,
      YannakakisReductionMode.NoReduction,
      PostgreSqlCandidateSelectionLimits.default,
      BagFactorPlanningLimits.default,
      PostgreSqlRenderingLimits.default,
      evaluator,
      PostgreSqlExplainSelectionLimits.default,
      sourceSql
    )

  private def successEvaluator(
      sourceRows: BigDecimal,
      candidateCosts: Vector[BigDecimal],
      candidateRows: Vector[BigDecimal]
  ): RecordingEvaluator =
    new RecordingEvaluator(request =>
      Right(
        successfulBatch(
          request,
          sourceRows,
          candidateRows,
          candidateCosts
        )
      )
    )

  private def successfulBatch(
      request: PostgreSqlExplainCostBatchRequest,
      sourceRows: BigDecimal,
      candidateRows: Vector[BigDecimal],
      candidateCosts: Vector[BigDecimal]
  ): PostgreSqlExplainCostBatch = {
    assertEquals(candidateRows.size, request.statements.size)
    assertEquals(candidateCosts.size, request.statements.size)
    PostgreSqlExplainCostBatch(
      provenance,
      sourceEvaluation(request.source, sourceRows),
      request.statements.zip(candidateRows.zip(candidateCosts)).map {
        case (statement, (rows, cost)) =>
          observation(statement, estimate(totalCost = cost, planRows = rows))
      }
    )
  }

  private def sourceEvaluation(
      statement: PostgreSqlExplainSourceStatement,
      planRows: BigDecimal
  ): PostgreSqlExplainSourceEvaluation =
    PostgreSqlExplainSourceEvaluation(
      statement.sourceStatementSha256,
      estimate(totalCost = BigDecimal(10), planRows = planRows)
    )

  private def observation(
      statement: PostgreSqlExplainCandidateStatement,
      value: PostgreSqlExplainCostEstimate
  ): PostgreSqlExplainCostObservation =
    PostgreSqlExplainCostObservation(
      statement.candidateId,
      statement.structuralRank,
      statement.physicalVariantId,
      statement.renderedStatementSha256,
      value
    )

  private def estimate(
      totalCost: BigDecimal = BigDecimal(1),
      planRows: BigDecimal = BigDecimal(1)
  ): PostgreSqlExplainCostEstimate =
    PostgreSqlExplainCostEstimate(
      startupCost = BigDecimal("0.125"),
      planTotalCost = totalCost,
      planRows = planRows,
      planWidth = 32,
      provenance = provenance
    )

  private def syntheticEvaluation(
      id: CandidatePlanId,
      structuralRank: Int,
      totalCost: String
  ): PostgreSqlCandidateEvaluation = {
    val mode = YannakakisReductionMode.NoReduction
    val value = estimate(
      totalCost = BigDecimal(totalCost),
      planRows = BigDecimal(10)
    )
    PostgreSqlCandidateEvaluation(
      candidateId = id,
      structuralRank = structuralRank,
      reductionMode = mode,
      reductionOrder = PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
      physicalVariantId = PostgreSqlPhysicalVariantId.from(id, mode),
      score = PostgreSqlStaticShapeScore(0, 0, 0, 0, 0, structuralRank),
      renderedStatementSha256 = "0" * 64,
      explainCostEstimate = Some(value),
      rowGuardEvidence = Some(
        PostgreSqlExplainRowGuardEvidence.from(BigDecimal(10), value.planRows)
      )
    )
  }

  private def candidateId(nibble: String): CandidatePlanId =
    CandidatePlanId
      .parse("sha256:" + nibble * 64)
      .fold(problem => fail(problem.message), identity)
}
