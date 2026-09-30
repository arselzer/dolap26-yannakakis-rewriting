package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

private[yannakakis] object PostgreSqlCountStageCostContractTestSupport {
  import PostgreSqlFactorAwareContractTestSupport._

  val policy: CandidatePlanSelection =
    CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1

  val countSql: String =
    "SELECT COUNT(*) AS cycle_count " +
      "FROM count_cost_e0 AS e0 " +
      "JOIN count_cost_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN count_cost_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN count_cost_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key"

  val groupedCountSql: String =
    "SELECT e0.left_key, COUNT(*) AS cycle_count " +
      "FROM count_cost_e0 AS e0 " +
      "JOIN count_cost_e1 AS e1 ON e0.right_key = e1.left_key " +
      "GROUP BY e0.left_key"

  val nonAggregateSql: String =
    "SELECT e0.left_key " +
      "FROM count_cost_e0 AS e0 " +
      "JOIN count_cost_e1 AS e1 ON e0.right_key = e1.left_key"

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    (0 until 4).foreach { index =>
      schema.add(
        s"count_cost_e$index",
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
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => throw new AssertionError(problem.toString), identity)
  }

  def request(sql: String = countSql): RewriteRequest =
    RewriteRequest(
      sql,
      options = RewriteOptions(
        widthBound = 2,
        planLimit = RewriteOptions.CountStageCostProgressivePlanLimit,
        requireConnectedCover = true,
        candidateSelection = policy,
        reductionMode = YannakakisReductionMode.FullTwoPass
      )
    )

  def calibratedEvaluator(): RecordingFactorEvaluator =
    new RecordingFactorEvaluator((probe, _) =>
      Right(
        successfulBatch(probe, sourceRows = BigDecimal(1)) {
          (candidate, statement, _) =>
            if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
              estimate(
                totalCost = BigDecimal(candidate.structuralRank),
                rows = BigDecimal(1),
                width = 8L
              )
            else if (candidate.structuralRank == 2)
              estimate(
                totalCost = BigDecimal("0.00000000000000000001"),
                rows = BigDecimal("1000000000"),
                width = 1024L
              )
            else
              estimate(
                totalCost = BigDecimal("1000000000000000000.00000000000000000001"),
                rows = BigDecimal(1),
                width = 1L
              )
        }
      )
    )

  def rewrite(
      rewriteRequest: RewriteRequest,
      evaluator: PostgreSqlExplainCostEvaluator
  ): PostgreSqlRewriteResult =
    PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = Some(evaluator))
      .fold(problem => throw new AssertionError(problem.message), identity)
      .rewriteDetailed(rewriteRequest)

  def rewriteWithoutEvaluator(
      rewriteRequest: RewriteRequest
  ): PostgreSqlRewriteResult =
    PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => throw new AssertionError(problem.message), identity)
      .rewriteDetailed(rewriteRequest)

  def successfulDocument(): (RewriteRequest, PostgreSqlRewriteResult) = {
    val value = request()
    value -> rewrite(value, calibratedEvaluator())
  }
}

class PostgreSqlCountStageCostSelectorContractSuite extends munit.FunSuite {
  import PostgreSqlCountStageCostContractTestSupport._
  import PostgreSqlFactorAwareContractTestSupport._

  test("exact score puts cumulative named-stage plan cost before every frozen v5 key") {
    val rankOne = candidateId(1)
    val rankTwo = candidateId(2)
    val lowerStageCost = PostgreSqlCountStageCostScore(
      sumNamedStagePlanTotalCost = BigDecimal("1.00000000000000000001"),
      maxNamedStagePlanRowsTimesPlanWidth = BigDecimal("99999999999999999999"),
      sumNamedStagePlanRowsTimesPlanWidth = BigDecimal("99999999999999999999"),
      finalPlanTotalCost = BigDecimal(999),
      structuralRank = 2,
      candidateId = rankTwo
    )
    val betterLegacyKeys = PostgreSqlCountStageCostScore(
      sumNamedStagePlanTotalCost = BigDecimal("1.00000000000000000002"),
      maxNamedStagePlanRowsTimesPlanWidth = BigDecimal(0),
      sumNamedStagePlanRowsTimesPlanWidth = BigDecimal(0),
      finalPlanTotalCost = BigDecimal(0),
      structuralRank = 1,
      candidateId = rankOne
    )

    assert(
      PostgreSqlCountStageCostScore.ordering.lt(
        lowerStageCost,
        betterLegacyKeys
      )
    )
    assertEquals(
      PostgreSqlCountStageCostProgressiveV1.ScoreOrder,
      Vector(
        "sum_named_stage_plan_total_cost",
        "max_named_stage_plan_rows_times_plan_width",
        "sum_named_stage_plan_rows_times_plan_width",
        "final_plan_total_cost",
        "structural_rank",
        "candidate_id"
      )
    )
  }

  test("exact score applies every frozen key in its declared order") {
    val base = PostgreSqlCountStageCostScore(
      sumNamedStagePlanTotalCost = BigDecimal("1.00000000000000000001"),
      maxNamedStagePlanRowsTimesPlanWidth = BigDecimal(2),
      sumNamedStagePlanRowsTimesPlanWidth = BigDecimal(3),
      finalPlanTotalCost = BigDecimal(4),
      structuralRank = 5,
      candidateId = candidateId(5)
    )
    val worseAtEachKey = Vector(
      base.copy(
        sumNamedStagePlanTotalCost = BigDecimal("1.00000000000000000002")
      ),
      base.copy(maxNamedStagePlanRowsTimesPlanWidth = BigDecimal(3)),
      base.copy(sumNamedStagePlanRowsTimesPlanWidth = BigDecimal(4)),
      base.copy(finalPlanTotalCost = BigDecimal(5)),
      base.copy(structuralRank = 6),
      base.copy(candidateId = candidateId(6))
    )

    worseAtEachKey.foreach(score =>
      assert(PostgreSqlCountStageCostScore.ordering.lt(base, score))
    )
  }

  test("certified global COUNT reuses one probe batch but retains only v7 evidence") {
    val evaluator = calibratedEvaluator()
    val rewriteRequest = request()
    val result = rewrite(rewriteRequest, evaluator)
    val artifact = result.report.artifact.getOrElse(
      fail(s"missing COUNT stage-cost artifact: ${result.outcome}")
    )
    val decision = artifact.candidateSelection

    assert(result.outcome.isInstanceOf[RewriteOutcome.Rewritten])
    assertEquals(evaluator.legacyCalls, 0)
    assertEquals(evaluator.factorCalls, 1)
    assertEquals(artifact.candidateSearch.requestedLimit, 100)
    assertEquals(artifact.selectedPlan.rank, 2)
    assertEquals(decision.reductionMode, YannakakisReductionMode.FullTwoPass)
    assert(decision.evaluations.forall(_.factorAwareEvidence.isEmpty))
    assert(decision.evaluations.forall(_.countStageCostEvidence.nonEmpty))
    assertEquals(decision.factorAwareCheckpoints, Vector.empty)
    assertEquals(decision.factorAwareSelectionLimits, None)
    assertEquals(
      decision.countStageCostCheckpoints.map(_.requestedPrefix),
      Vector(10, 25, 50, 100)
    )
    assertEquals(
      decision.countStageCostCheckpoints.last.winnerStructuralRank,
      Some(2)
    )

    val rankTwoEvidence = decision.evaluations(1).countStageCostEvidence.get
    assertEquals(
      rankTwoEvidence.score.sumNamedStagePlanTotalCost,
      PostgreSqlFactorAwareScore.exactSum(
        rankTwoEvidence.stages.collect {
          case stage if stage.trace.physicalForm.namedCte =>
            stage.estimate.get.planTotalCost
        }
      )
    )
    assert(
      PostgreSqlCountStageCostSelectionReplay.verify(
        artifact.candidateSearch.plans,
        decision.evaluations,
        decision.sourceEvaluation.get,
        decision.countStageCostCheckpoints,
        decision.countStageCostSelectionLimits.get,
        Some(
          PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
            countSql
          )
        )
      )
    )

    val first = decision.countStageCostCheckpoints.head
    val wrong = first.copy(
      winnerScore = first.winnerScore.map(score =>
        score.copy(
          sumNamedStagePlanTotalCost = score.sumNamedStagePlanTotalCost + 1
        )
      )
    )
    assert(
      !PostgreSqlCountStageCostSelectionReplay.verify(
        artifact.candidateSearch.plans,
        decision.evaluations,
        decision.sourceEvaluation.get,
        decision.countStageCostCheckpoints.updated(0, wrong),
        decision.countStageCostSelectionLimits.get,
        Some(
          PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
            countSql
          )
        )
      )
    )
  }

  test("grouped COUNT is rejected before candidate search or probe evaluation") {
    val evaluator = calibratedEvaluator()
    val result = rewrite(request(groupedCountSql), evaluator)

    assert(result.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
    assertEquals(result.report.completedPhase, PostgreSqlRewritePhase.QueryAnalyzed)
    assertEquals(result.report.candidateDecision, None)
    assertEquals(evaluator.factorCalls, 0)
    assertEquals(
      result.outcome.diagnostics.head.context("reason"),
      PostgreSqlCandidateSelector.CountStageCostRequiresGlobalCountStarReason
    )
  }

  test(
    "every non-global shape is rejected before candidate search or probe evaluation"
  ) {
    val evaluator = calibratedEvaluator()
    val result = rewrite(request(nonAggregateSql), evaluator)

    assert(result.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
    assertEquals(result.report.completedPhase, PostgreSqlRewritePhase.QueryAnalyzed)
    assertEquals(result.report.candidateDecision, None)
    assertEquals(result.report.candidateSelectionDecision, None)
    assertEquals(evaluator.legacyCalls, 0)
    assertEquals(evaluator.factorCalls, 0)
    assertEquals(
      result.outcome.diagnostics.head.context("reason"),
      PostgreSqlCandidateSelector.CountStageCostRequiresGlobalCountStarReason
    )
  }

  test("programmatic options freeze exact K=100 and FullTwoPass") {
    intercept[IllegalArgumentException] {
      RewriteOptions(
        planLimit = 99,
        candidateSelection = policy,
        reductionMode = YannakakisReductionMode.FullTwoPass
      )
    }
    intercept[IllegalArgumentException] {
      RewriteOptions(
        planLimit = RewriteOptions.CountStageCostProgressivePlanLimit,
        candidateSelection = policy,
        reductionMode = YannakakisReductionMode.BottomUpOnly
      )
    }

    val invalidK = RewriteOptions.validated(
      planLimit = 99,
      candidateSelection = policy,
      reductionMode = YannakakisReductionMode.FullTwoPass
    ).swap.getOrElse(fail("K=99 must be rejected"))
    assertEquals(invalidK.map(_.context("option")), Vector("planLimit"))

    val invalidMode = RewriteOptions.validated(
      planLimit = RewriteOptions.CountStageCostProgressivePlanLimit,
      candidateSelection = policy,
      reductionMode = YannakakisReductionMode.BottomUpOnly
    ).swap.getOrElse(fail("BottomUpOnly must be rejected"))
    assertEquals(invalidMode.map(_.context("option")), Vector("reductionMode"))
  }

  test(
    "v7 replay fails closed on score, checkpoint, source, and hostile numeric mutations"
  ) {
    val (rewriteRequest, result) = successfulDocument()
    val artifact = result.report.artifact.getOrElse(
      fail(s"missing COUNT stage-cost artifact: ${result.outcome}")
    )
    val decision = artifact.candidateSelection
    val plans = artifact.candidateSearch.plans
    val source = decision.sourceEvaluation.getOrElse(
      fail("COUNT stage-cost decision has no source estimate")
    )
    val checkpoints = decision.countStageCostCheckpoints
    val limits = decision.countStageCostSelectionLimits.getOrElse(
      fail("COUNT stage-cost decision has no limits")
    )
    val expectedSourceHash = Some(
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
        rewriteRequest.originalSql
      )
    )

    def verifies(
        candidateEvaluations: Vector[PostgreSqlCandidateEvaluation] =
          decision.evaluations,
        sourceEvaluation: PostgreSqlExplainSourceEvaluation = source,
        candidateCheckpoints: Vector[PostgreSqlCountStageCostCheckpoint] =
          checkpoints,
        expectedHash: Option[String] = expectedSourceHash
    ): Boolean =
      PostgreSqlCountStageCostSelectionReplay.verify(
        plans,
        candidateEvaluations,
        sourceEvaluation,
        candidateCheckpoints,
        limits,
        expectedHash
      )

    assert(verifies())

    val firstWinner = checkpoints.head.winnerScore.getOrElse(
      fail("first checkpoint has no winner")
    )
    val wrongScore = checkpoints.head.copy(
      winnerScore = Some(
        firstWinner.copy(
          sumNamedStagePlanTotalCost =
            firstWinner.sumNamedStagePlanTotalCost +
              BigDecimal("0.00000000000000000001")
        )
      )
    )
    assert(!verifies(candidateCheckpoints = checkpoints.updated(0, wrongScore)))

    val wrongPhysical = checkpoints.head.copy(
      winnerPhysicalVariantId = Some(
        PostgreSqlPhysicalVariantId.from(
          candidateId(999),
          YannakakisReductionMode.FullTwoPass
        )
      )
    )
    assert(
      !verifies(candidateCheckpoints = checkpoints.updated(0, wrongPhysical))
    )

    val wrongProof = checkpoints.head.copy(verifiedPrefixSha256 = "0" * 64)
    assert(!verifies(candidateCheckpoints = checkpoints.updated(0, wrongProof)))
    assert(!verifies(candidateCheckpoints = checkpoints.reverse))
    assert(!verifies(expectedHash = Some("0" * 64)))

    val hostileValues = Vector(
      BigDecimal(
        "12345678901234567890123456789012345678901234567890123456789012345"
      ),
      BigDecimal("1e-1025"),
      BigDecimal("1e1025")
    )
    hostileValues.foreach { hostile =>
      val hostileSource = source.copy(
        estimate = source.estimate.copy(startupCost = hostile)
      )
      assert(!verifies(sourceEvaluation = hostileSource), clues(hostile))
    }

    val reordered = decision.evaluations.reverse
    assert(!verifies(candidateEvaluations = reordered))
  }
}
