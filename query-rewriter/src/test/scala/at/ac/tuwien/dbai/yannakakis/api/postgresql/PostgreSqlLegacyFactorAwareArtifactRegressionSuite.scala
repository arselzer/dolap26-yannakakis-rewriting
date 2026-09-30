package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

/** Regression for the legacy v5 success artifact's terminal digest binding. */
class PostgreSqlLegacyFactorAwareArtifactRegressionSuite
    extends munit.FunSuite {
  import PostgreSqlFactorAwareContractTestSupport._

  test("factor-aware success constructs an artifact whose selected digest equals the terminal diagnostic digest") {
    val evaluator = new RecordingFactorEvaluator((request, _) =>
      Right(
        successfulBatch(request, sourceRows = BigDecimal(100)) {
          (candidate, statement, index) =>
            if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
              estimate(
                totalCost = BigDecimal(candidate.structuralRank),
                rows = BigDecimal(100),
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
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = Some(evaluator))
      .fold(problem => fail(problem.message), identity)

    val result = rewriter.rewriteDetailed(
      RewriteRequest(
        triangleSql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = 100,
          candidateSelection =
            CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1,
          reductionMode = YannakakisReductionMode.FullTwoPass
        )
      )
    )
    assert(result.outcome.isInstanceOf[RewriteOutcome.Rewritten])
    val artifact = result.report.artifact.getOrElse(
      fail("factor-aware success did not retain its legacy v5 artifact")
    )
    val selectedEvaluation = artifact.candidateSelection.evaluations
      .find(_.candidateId == artifact.selectedPlan.candidateId)
      .getOrElse(fail("selected plan has no evaluation"))
    val terminalDigest = selectedEvaluation.factorAwareEvidence
      .getOrElse(fail("selected evaluation has no factor-aware evidence"))
      .stages
      .last
      .diagnosticStatementSha256
      .getOrElse(fail("terminal diagnostic has no digest"))

    assertEquals(selectedEvaluation.renderedStatementSha256, terminalDigest)
    assertEquals(
      terminalDigest,
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
        artifact.renderedQuery.sql
      )
    )
  }
}
