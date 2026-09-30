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

class PostgreSqlGlobalGuardedExtremumRewriteSuite extends munit.FunSuite {
  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    (0 until 4).foreach { index =>
      schema.add(
        s"extremum_e$index",
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("left_key", SqlTypeName.INTEGER)
              .add("right_key", SqlTypeName.INTEGER)
              .add("payload", SqlTypeName.INTEGER)
              .build()
        }
      )
    }
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  private val cycleFrom =
    "FROM extremum_e0 AS e0 " +
      "JOIN extremum_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN extremum_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN extremum_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key"

  private val rewriter = PostgreSqlQueryRewriter
    .create(planner)
    .fold(problem => fail(problem.message), identity)

  test("structural selection preserves plan identity and the exact nullable result") {
    val sql = s"SELECT MAX(e0.payload) AS \"Top\" $cycleFrom"
    val result = rewritten(sql, YannakakisReductionMode.FullTwoPass)
    val value = artifact(result)

    assertEquals(value.query.resultShape.id, "global-guarded-extremum")
    assertEquals(
      value.renderedQuery.output.map(column =>
        (column.ordinal, column.name, column.dataType, column.nullable)
      ),
      Vector((0, "Top", "INTEGER", true))
    )
    assertEquals(
      value.candidateSelection.selectedCandidateId,
      value.selectedPlan.candidateId
    )
    assertEquals(
      value.candidateSelection.selectedPhysicalVariantId,
      PostgreSqlPhysicalVariantId.from(
        value.selectedPlan.candidateId,
        YannakakisReductionMode.FullTwoPass
      )
    )
    assert(value.renderedQuery.sql.contains("SELECT\n  MAX("))
    assert(!value.renderedQuery.sql.contains("COUNT(*)"))
  }

  test("direct guard-row semijoin is exact across reductions without rerooting") {
    val full = (0 until 4).map { guardIndex =>
      val sql =
        s"SELECT MIN(e$guardIndex.payload) AS \"result\" $cycleFrom"
      (guardIndex, sql, artifact(rewritten(sql, YannakakisReductionMode.FullTwoPass)))
    }.toVector
    val rootGuard = full.find { case (_, _, value) =>
      guardOwner(value) == value.bagFactorPlan.root
    }.getOrElse(fail("cycle plan has no root-owned guard"))
    val nonRootGuard = full.find { case (_, _, value) =>
      guardOwner(value) != value.bagFactorPlan.root
    }.getOrElse(fail("cycle plan has no non-root guard"))

    val bottomUp = rewritten(nonRootGuard._2, YannakakisReductionMode.BottomUpOnly)
    assertEquals(
      artifact(bottomUp).selectedPlan.candidateId,
      nonRootGuard._3.selectedPlan.candidateId
    )
    assert(artifact(bottomUp).renderedQuery.sql.contains("SELECT\n  MIN("))
    assert(artifact(bottomUp).renderedQuery.sql.contains("\nWHERE\n  EXISTS ("))

    val noReduction = rewritten(
      rootGuard._2,
      YannakakisReductionMode.NoReduction
    )
    assertEquals(
      artifact(noReduction).selectedPlan.candidateId,
      rootGuard._3.selectedPlan.candidateId
    )
    val nonRootNoReduction = rewritten(
      nonRootGuard._2,
      YannakakisReductionMode.NoReduction
    )
    assertEquals(
      artifact(nonRootNoReduction).selectedPlan.candidateId,
      nonRootGuard._3.selectedPlan.candidateId
    )
    assert(
      artifact(nonRootNoReduction).renderedQuery.sql.contains(
        "\nWHERE\n  EXISTS ("
      )
    )
  }

  test("all EXPLAIN v1 policies reject the aggregate row guard before evaluators") {
    val spy = new ExplainSpy
    val guardedRewriter = PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = Some(spy))
      .fold(problem => fail(problem.message), identity)
    val sql = s"SELECT MIN(e0.payload) AS \"result\" $cycleFrom"
    val policies = Vector(
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 -> 10,
      CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ->
        RewriteOptions.FactorAwareProgressivePlanLimit,
      CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 ->
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit
    )

    policies.foreach { case (policy, planLimit) =>
      val result = guardedRewriter.rewriteDetailed(
        RewriteRequest(
          sql,
          options = RewriteOptions(
            planLimit = planLimit,
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
          "resultShape" -> "global-guarded-extremum",
          "reason" -> "aggregate-root-row-guard-degenerate"
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
      throw new AssertionError("physical-portfolio evaluator must not be called")
    }
  }

  private def rewrite(
      sql: String,
      mode: YannakakisReductionMode
  ): PostgreSqlRewriteResult =
    rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = 2,
          requireConnectedCover = true,
          reductionMode = mode
        )
      )
    )

  private def rewritten(
      sql: String,
      mode: YannakakisReductionMode
  ): PostgreSqlRewriteResult = {
    val result = rewrite(sql, mode)
    result.outcome match {
      case _: RewriteOutcome.Rewritten => result
      case other => fail(s"expected rewritten outcome, found $other")
    }
  }

  private def artifact(result: PostgreSqlRewriteResult): PostgreSqlRewriteArtifact =
    result.report.artifact.getOrElse(fail("missing rewrite artifact"))

  private def guardOwner(artifact: PostgreSqlRewriteArtifact) = {
    val shape = artifact.query.resultShape
      .asInstanceOf[QueryResultShape.GlobalGuardedExtremum]
    val edge = artifact.hypergraph.edges
      .find(
        _.relationOccurrenceId == shape.guardColumn.relationOccurrenceId
      )
      .getOrElse(fail("missing guard edge"))
    artifact.bagFactorPlan.ownership
      .find(_.edgeId == edge.id)
      .map(_.owner)
      .getOrElse(fail("missing guard owner"))
  }

}
