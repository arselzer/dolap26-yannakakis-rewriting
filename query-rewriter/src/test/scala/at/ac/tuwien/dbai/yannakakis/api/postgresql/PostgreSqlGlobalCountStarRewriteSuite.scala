package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlRenderingLimits,
  PostgreSqlStageId
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteReasonCode,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlGlobalCountStarRewriteSuite extends munit.FunSuite {
  private val joined =
    "FROM count_r AS r JOIN count_s AS s ON r.k = s.k"
  private val implicitCountSql = "SELECT COUNT(*) " + joined
  private val explicitCountSql = "SELECT COUNT(*) AS \"Total\" " + joined
  private val enumerationSql =
    "SELECT r.payload AS left_payload, s.payload AS right_payload " + joined
  private val cycleCountSql =
    "SELECT COUNT(*) AS cycle_count " +
      "FROM count_e0 AS e0 " +
      "JOIN count_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN count_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN count_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key"

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    Vector("count_r", "count_s").foreach { name =>
      schema.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("k", SqlTypeName.INTEGER)
              .add("payload", SqlTypeName.INTEGER)
              .build()
        }
      )
    }
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
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  test("structural and static policies rewrite COUNT(*) without changing plan identity") {
    val rewriter = newRewriter()
    val enumeration = rewrite(rewriter, enumerationSql)
    val implicitCount = rewrite(rewriter, implicitCountSql)
    val explicitCount = rewrite(rewriter, explicitCountSql)
    val staticCount = rewrite(
      rewriter,
      explicitCountSql,
      CandidatePlanSelection.PostgreSqlStaticShapeV1
    )

    val enumerationArtifact = artifact(enumeration)
    val implicitArtifact = artifact(implicitCount)
    val explicitArtifact = artifact(explicitCount)
    val staticArtifact = artifact(staticCount)

    assertEquals(
      implicitArtifact.candidateSearch.plans.map(_.candidateId),
      enumerationArtifact.candidateSearch.plans.map(_.candidateId)
    )
    assertEquals(
      explicitArtifact.selectedPlan.candidateId,
      enumerationArtifact.selectedPlan.candidateId
    )
    assertEquals(
      explicitArtifact.candidateSelection.selectedPhysicalVariantId,
      enumerationArtifact.candidateSelection.selectedPhysicalVariantId
    )
    assertEquals(
      explicitArtifact.candidateSelection.selectedPhysicalVariantId,
      PostgreSqlPhysicalVariantId.from(
        explicitArtifact.selectedPlan.candidateId,
        explicitArtifact.candidateSelection.reductionMode
      )
    )
    assertNotEquals(
      selectedHash(explicitArtifact),
      selectedHash(enumerationArtifact)
    )
    assertNotEquals(selectedHash(explicitArtifact), selectedHash(implicitArtifact))

    assertEquals(
      implicitArtifact.renderedQuery.output.map(column =>
        (column.ordinal, column.name, column.dataType, column.nullable)
      ),
      Vector((0, "count", "BIGINT NOT NULL", false))
    )
    assertEquals(explicitArtifact.renderedQuery.output.map(_.name), Vector("Total"))
    assert(
      explicitArtifact.renderedQuery.sql.contains(
        "CAST(COALESCE(SUM(\"yr_cf_b0\".\"yr_freq\"), " +
          "CAST(0 AS NUMERIC)) AS BIGINT) AS \"Total\""
      ),
      clues(explicitArtifact.renderedQuery.sql)
    )
    assert(
      explicitArtifact.renderedQuery.sql.contains(
        "CAST(COUNT(*) AS NUMERIC) AS \"yr_freq\""
      ),
      clues(explicitArtifact.renderedQuery.sql)
    )
    assertEquals(
      staticArtifact.candidateSelection.policy,
      CandidatePlanSelection.PostgreSqlStaticShapeV1
    )
  }

  test("row-only EXPLAIN policies stop after analysis without evaluator calls") {
    val spy = new ExplainSpy
    val rewriter = newRewriter(Some(spy))
    val policies = Vector(
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 -> 10,
      CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 ->
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit
    )

    policies.foreach { case (policy, planLimit) =>
      val result = rewriter.rewriteDetailed(
        RewriteRequest(
          explicitCountSql,
          options = RewriteOptions(
            planLimit = planLimit,
            candidateSelection = policy
          )
        )
      )
      assert(result.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
      assertEquals(
        result.outcome.diagnostics.map(_.code),
        Vector(RewriteReasonCode.UnsupportedQueryShape)
      )
      assertEquals(
        result.outcome.diagnostics.head.context,
        Map(
          "selectionPolicy" -> policy.id,
          "resultShape" -> "global-count-star",
          "reason" -> "aggregate-root-row-guard-degenerate"
        )
      )
      assertEquals(result.report.completedPhase, PostgreSqlRewritePhase.QueryAnalyzed)
      assert(result.report.analysis.exists(_.isSupported))
      assertEquals(result.report.hypergraph, None)
      assertEquals(result.report.candidateDecision, None)
      assertEquals(result.report.candidateSelectionDecision, None)
      assertEquals(result.report.artifact, None)
    }

    assertEquals(spy.standardCalls, 0)
    assertEquals(spy.factorAwareCalls, 0)
    assertEquals(spy.physicalPortfolioCalls, 0)
  }

  test("direct row-only selector guard is fail-closed before evaluator boundaries") {
    val structural = rewrite(newRewriter(), explicitCountSql)
    val value = artifact(structural)
    val spy = new ExplainSpy

    Vector(
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1,
      CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
    ).foreach { policy =>
      PostgreSqlCandidateSelector.select(
        query = value.query,
        input = value.candidateInput,
        found = value.candidateSearch,
        policy = policy,
        reductionMode = YannakakisReductionMode.Default,
        limits = PostgreSqlCandidateSelectionLimits.default,
        bagFactorLimits = BagFactorPlanningLimits.default,
        renderingLimits = PostgreSqlRenderingLimits.default,
        explainCostEvaluator = Some(spy),
        sourceSql = Some(explicitCountSql)
      ) match {
        case Left(failure: PostgreSqlCandidateSelectionFailure.Inapplicable) =>
          assertEquals(failure.policy, policy)
          assertEquals(failure.resultShape, "global-count-star")
          assertEquals(failure.reason, "aggregate-root-row-guard-degenerate")
          assertEquals(failure.decision, None)
        case other => fail(s"expected selector inapplicability, found $other")
      }
    }

    assertEquals(spy.standardCalls, 0)
    assertEquals(spy.factorAwareCalls, 0)
    assertEquals(spy.physicalPortfolioCalls, 0)
  }

  test("factor-aware COUNT requires full two-pass before candidate search") {
    val spy = new ExplainSpy
    val result = newRewriter(Some(spy)).rewriteDetailed(
      RewriteRequest(
        explicitCountSql,
        options = RewriteOptions(
          planLimit = RewriteOptions.FactorAwareProgressivePlanLimit,
          candidateSelection =
            CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1,
          reductionMode = YannakakisReductionMode.BottomUpOnly
        )
      )
    )

    assert(result.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
    assertEquals(
      result.outcome.diagnostics.head.context,
      Map(
        "selectionPolicy" ->
          CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1.id,
        "resultShape" -> "global-count-star",
        "reason" ->
          "aggregate-count-factor-aware-requires-full-two-pass"
      )
    )
    assertEquals(result.report.completedPhase, PostgreSqlRewritePhase.QueryAnalyzed)
    assertEquals(result.report.candidateDecision, None)
    assertEquals(spy.factorAwareCalls, 0)
  }

  test("factor-aware COUNT selects by named-stage estimates with scalar roots") {
    import PostgreSqlFactorAwareContractTestSupport._

    val evaluator = new RecordingFactorEvaluator((request, _) =>
      Right(
        successfulBatch(request, sourceRows = BigDecimal(1)) {
          (candidate, statement, _) =>
            if (
              statement.stageId ==
                PostgreSqlStageId.FinalReconstruction
            )
              estimate(
                totalCost = BigDecimal(candidate.structuralRank),
                rows = BigDecimal(1),
                width = 8L
              )
            else
              estimate(
                totalCost = BigDecimal(1),
                rows = BigDecimal(
                  if (candidate.structuralRank == 1) 1000000
                  else candidate.structuralRank
                ),
                width = 8L
              )
        }
      )
    )
    val result = newRewriter(Some(evaluator)).rewriteDetailed(
      RewriteRequest(
        cycleCountSql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = RewriteOptions.FactorAwareProgressivePlanLimit,
          requireConnectedCover = true,
          candidateSelection =
            CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1,
          reductionMode = YannakakisReductionMode.FullTwoPass
        )
      )
    )
    val value = artifact(result)

    assertEquals(evaluator.legacyCalls, 0)
    assertEquals(evaluator.factorCalls, 1)
    assert(value.candidateSelection.evaluations.size >= 2)
    assertEquals(value.selectedPlan.rank, 2)
    assert(
      value.candidateSelection.evaluations.forall(
        _.rowGuardEvidence.exists(_.withinGuard)
      )
    )
    val rankOne = value.candidateSelection.evaluations.head
      .factorAwareEvidence
      .getOrElse(fail("rank one lacks factor-aware evidence"))
    val rankTwo = value.candidateSelection.evaluations(1)
      .factorAwareEvidence
      .getOrElse(fail("rank two lacks factor-aware evidence"))
    assert(
      rankOne.score.maxNamedStagePlanRowsTimesPlanWidth >
        rankTwo.score.maxNamedStagePlanRowsTimesPlanWidth
    )
    assertEquals(
      value.candidateSelection.factorAwareCheckpoints.last.winnerStructuralRank,
      Some(2)
    )
  }

  private final class ExplainSpy extends PostgreSqlExplainCostEvaluator {
    var standardCalls: Int = 0
    var factorAwareCalls: Int = 0
    var physicalPortfolioCalls: Int = 0

    override def evaluate(
        request: PostgreSqlExplainCostBatchRequest,
        limits: PostgreSqlExplainSelectionLimits
    ): Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch] = {
      standardCalls += 1
      throw new AssertionError("standard EXPLAIN evaluator must not be called")
    }

    override def evaluateFactorAware(
        request: PostgreSqlFactorAwareExplainBatchRequest,
        limits: PostgreSqlFactorAwareSelectionLimits
    ): Either[
      PostgreSqlFactorAwareExplainFailure,
      PostgreSqlFactorAwareExplainBatch
    ] = {
      factorAwareCalls += 1
      throw new AssertionError("factor-aware EXPLAIN evaluator must not be called")
    }

    override def evaluatePhysicalPortfolio(
        request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
        limits: PostgreSqlPhysicalPortfolioEvaluationLimits
    ): Either[
      PostgreSqlPhysicalPortfolioExplainFailure,
      PostgreSqlPhysicalPortfolioExplainBatch
    ] = {
      physicalPortfolioCalls += 1
      throw new AssertionError(
        "physical-portfolio EXPLAIN evaluator must not be called"
      )
    }
  }

  private def newRewriter(
      evaluator: Option[PostgreSqlExplainCostEvaluator] = None
  ): PostgreSqlQueryRewriter =
    PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = evaluator)
      .fold(problem => fail(problem.message), identity)

  private def rewrite(
      rewriter: PostgreSqlQueryRewriter,
      sql: String,
      policy: CandidatePlanSelection = CandidatePlanSelection.StructuralHead
  ): PostgreSqlRewriteResult = {
    val result = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(candidateSelection = policy)
      )
    )
    result.outcome match {
      case _: RewriteOutcome.Rewritten => result
      case other => fail(s"expected rewritten outcome, found $other")
    }
  }

  private def artifact(
      result: PostgreSqlRewriteResult
  ): PostgreSqlRewriteArtifact =
    result.report.artifact.getOrElse(fail("missing rewrite artifact"))

  private def selectedHash(artifact: PostgreSqlRewriteArtifact): String =
    artifact.candidateSelection.evaluations
      .find(_.candidateId == artifact.candidateSelection.selectedCandidateId)
      .map(_.renderedStatementSha256)
      .getOrElse(fail("selected evaluation is missing"))
}
