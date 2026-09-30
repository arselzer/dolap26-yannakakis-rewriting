package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteReasonCode,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteSqlPlanner,
  QueryResultShape
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlDistributedGroupedCountStarRewriteSuite extends munit.FunSuite {
  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    (0 until 4).foreach { index =>
      schema.add(
        s"distributed_e$index",
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("left_key", SqlTypeName.INTEGER)
              .nullable(true)
              .add("right_key", SqlTypeName.INTEGER)
              .nullable(true)
              .add("payload", SqlTypeName.INTEGER)
              .nullable(true)
              .build()
        }
      )
    }
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  private val sql =
    "SELECT e0.left_key AS \"First\", e2.left_key AS \"Second\", " +
      "COUNT(*) AS \"Rows\" " +
      "FROM distributed_e0 AS e0 " +
      "JOIN distributed_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN distributed_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN distributed_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key " +
      "GROUP BY e0.left_key, e2.left_key"

  private val rewriter = PostgreSqlQueryRewriter
    .create(planner)
    .fold(problem => fail(problem.message), identity)

  test("structural and static policies emit the exact distributed result shape") {
    Vector(
      CandidatePlanSelection.StructuralHead,
      CandidatePlanSelection.StructuralRank(1),
      CandidatePlanSelection.PostgreSqlStaticShapeV1
    ).foreach { policy =>
      val result = rewriter.rewriteDetailed(
        RewriteRequest(
          sql,
          options = RewriteOptions(
            widthBound = 2,
            planLimit = 10,
            requireConnectedCover = true,
            reductionMode = YannakakisReductionMode.FullTwoPass,
            candidateSelection = policy
          )
        )
      )
      val artifact = result.outcome match {
        case _: RewriteOutcome.Rewritten =>
          result.report.artifact.getOrElse(fail("missing rewrite artifact"))
        case other => fail(s"expected distributed rewrite for $policy, found $other")
      }
      assert(
        artifact.query.resultShape
          .isInstanceOf[QueryResultShape.DistributedGroupedCountStar]
      )
      assertEquals(
        artifact.renderedQuery.output.map(column =>
          (column.ordinal, column.name, column.dataType, column.nullable)
        ),
        Vector(
          (0, "First", "INTEGER", true),
          (1, "Second", "INTEGER", true),
          (2, "Rows", "BIGINT NOT NULL", false)
        )
      )
      assert(artifact.renderedQuery.sql.contains("CAST(COALESCE(SUM("))
      assert(artifact.renderedQuery.sql.contains("\nGROUP BY\n"))
    }

    val head = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = 10,
          requireConnectedCover = true,
          reductionMode = YannakakisReductionMode.FullTwoPass,
          candidateSelection = CandidatePlanSelection.StructuralHead
        )
      )
    )
    val selected = head.report.artifact
      .getOrElse(fail("missing structural-head artifact"))
      .selectedPlan
    val exact = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = 10,
          requireConnectedCover = true,
          reductionMode = YannakakisReductionMode.FullTwoPass,
          candidateSelection =
            CandidatePlanSelection.ExactCandidateId(selected.candidateId)
        )
      )
    )
    val exactArtifact = exact.report.artifact.getOrElse(
      fail(s"exact-ID distributed rewrite failed: ${exact.outcome}")
    )
    assertEquals(exactArtifact.selectedPlan, selected)
  }

  test("partial modes reject before candidate search") {
    Vector(
      YannakakisReductionMode.BottomUpOnly,
      YannakakisReductionMode.NoReduction
    ).foreach { mode =>
      val result = rewriter.rewriteDetailed(
        RewriteRequest(
          sql,
          options = RewriteOptions(
            widthBound = 2,
            planLimit = 10,
            requireConnectedCover = true,
            reductionMode = mode
          )
        )
      )
      assert(result.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
      assertEquals(result.report.completedPhase, PostgreSqlRewritePhase.QueryAnalyzed)
      assertEquals(result.report.hypergraph, None)
      assertEquals(result.report.candidateDecision, None)
      assertEquals(
        result.outcome.diagnostics.head.context.get("reason"),
        Some(
          PostgreSqlCandidateSelector
            .DistributedGroupedCountRequiresFullTwoPassReason
        )
      )
    }
  }

  test("all EXPLAIN policies reject before evaluator access") {
    val spy = new ExplainSpy
    val guarded = PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = Some(spy))
      .fold(problem => fail(problem.message), identity)
    val policies = Vector(
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 -> 10,
      CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ->
        RewriteOptions.FactorAwareProgressivePlanLimit,
      CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 ->
        RewriteOptions.CountStageCostProgressivePlanLimit,
      CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 ->
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit
    )

    policies.foreach { case (policy, planLimit) =>
      val result = guarded.rewriteDetailed(
        RewriteRequest(
          sql,
          options = RewriteOptions(
            widthBound = 2,
            planLimit = planLimit,
            requireConnectedCover = true,
            reductionMode = YannakakisReductionMode.FullTwoPass,
            candidateSelection = policy
          )
        )
      )
      assert(result.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
      assertEquals(result.outcome.diagnostics.map(_.code), Vector(
        RewriteReasonCode.UnsupportedQueryShape
      ))
      assertEquals(
        result.outcome.diagnostics.head.context,
        Map(
          "selectionPolicy" -> policy.id,
          "resultShape" -> "distributed-grouped-count-star",
          "reason" -> PostgreSqlCandidateSelector
            .DistributedGroupedCountOptimizerPolicyUnsupportedReason
        )
      )
      assertEquals(result.report.completedPhase, PostgreSqlRewritePhase.QueryAnalyzed)
      assertEquals(result.report.hypergraph, None)
      assertEquals(result.report.candidateDecision, None)
      assertEquals(result.report.artifact, None)
    }

    assertEquals(spy.standardCalls, 0)
    assertEquals(spy.factorAwareCalls, 0)
    assertEquals(spy.physicalPortfolioCalls, 0)
  }

  test("renderer-level grouped-key exclusions remain typed inapplicability") {
    val from =
      " FROM distributed_e0 AS e0 " +
        "JOIN distributed_e1 AS e1 ON e0.right_key = e1.left_key " +
        "JOIN distributed_e2 AS e2 ON e1.right_key = e2.left_key " +
        "JOIN distributed_e3 AS e3 ON e2.right_key = e3.left_key " +
        "AND e3.right_key = e0.left_key "
    val excluded = Vector(
      "SELECT e0.payload, COUNT(*)" + from + "GROUP BY e0.payload",
      "SELECT e0.left_key, e3.right_key, COUNT(*)" + from +
        "GROUP BY e0.left_key, e3.right_key"
    )

    excluded.foreach { query =>
      val result = rewriter.rewriteDetailed(
        RewriteRequest(
          query,
          options = RewriteOptions(
            widthBound = 2,
            planLimit = 10,
            requireConnectedCover = true,
            reductionMode = YannakakisReductionMode.FullTwoPass
          )
        )
      )

      assert(result.outcome.isInstanceOf[RewriteOutcome.NotApplicable], clues(query))
      assertEquals(
        result.outcome.diagnostics.map(_.code),
        Vector(RewriteReasonCode.UnsupportedQueryShape),
        clues(query, result.outcome.diagnostics)
      )
      assert(
        result.outcome.diagnostics.head.context
          .get("algorithmCode")
          .exists(code =>
            code == "postgresql.rendering.aggregate.grouped-count-key" ||
              code == "postgresql.rendering.aggregate.distributed-grouped-count-key"
          ),
        clues(query, result.outcome.diagnostics)
      )
    }
  }

  private final class ExplainSpy extends PostgreSqlExplainCostEvaluator {
    var standardCalls = 0
    var factorAwareCalls = 0
    var physicalPortfolioCalls = 0

    override def evaluate(
        request: PostgreSqlExplainCostBatchRequest,
        limits: PostgreSqlExplainSelectionLimits
    ): Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch] = {
      standardCalls += 1
      throw new AssertionError("standard evaluator must not be called")
    }

    override def evaluateFactorAware(
        request: PostgreSqlFactorAwareExplainBatchRequest,
        limits: PostgreSqlFactorAwareSelectionLimits
    ): Either[
      PostgreSqlFactorAwareExplainFailure,
      PostgreSqlFactorAwareExplainBatch
    ] = {
      factorAwareCalls += 1
      throw new AssertionError("factor-aware evaluator must not be called")
    }

    override def evaluatePhysicalPortfolio(
        request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
        limits: PostgreSqlPhysicalPortfolioEvaluationLimits
    ): Either[
      PostgreSqlPhysicalPortfolioExplainFailure,
      PostgreSqlPhysicalPortfolioExplainBatch
    ] = {
      physicalPortfolioCalls += 1
      throw new AssertionError("physical evaluator must not be called")
    }
  }
}
