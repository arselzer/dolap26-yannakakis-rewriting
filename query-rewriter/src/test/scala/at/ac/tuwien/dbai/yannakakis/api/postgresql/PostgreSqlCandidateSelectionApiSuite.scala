package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteReasonCode,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

class PostgreSqlCandidateSelectionApiSuite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("default selection is exact structural-head behavior") {
    val rewriter = standardRewriter()
    val defaultRequest = RewriteRequest(
      triangleSql,
      options = RewriteOptions(widthBound = 2, planLimit = 10)
    )
    val explicitRequest = defaultRequest.copy(
      options = defaultRequest.options.copy(
        candidateSelection = CandidatePlanSelection.StructuralHead
      )
    )
    val default = rewriter.rewriteDetailed(defaultRequest)
    val explicit = rewriter.rewriteDetailed(explicitRequest)

    assertEquals(default.outcome, explicit.outcome)
    val defaultArtifact = artifact(default)
    val explicitArtifact = artifact(explicit)
    assertEquals(
      defaultArtifact.candidateSearch,
      explicitArtifact.candidateSearch
    )
    assertEquals(
      defaultArtifact.candidateSelection,
      explicitArtifact.candidateSelection
    )
    assertEquals(defaultArtifact.selectedPlan, explicitArtifact.selectedPlan)
    assertEquals(defaultArtifact.bagFactorPlan, explicitArtifact.bagFactorPlan)
    assertEquals(
      defaultArtifact.renderedQuery.sql,
      explicitArtifact.renderedQuery.sql
    )
    assertEquals(defaultArtifact.selectedPlan.rank, 1)
    assertEquals(
      defaultArtifact.selectedPlan,
      defaultArtifact.candidateSearch.plans.head
    )
    assertEquals(
      defaultArtifact.candidateSelection.policy,
      CandidatePlanSelection.StructuralHead
    )
    assertEquals(
      defaultArtifact.candidateSelection.evaluations.map(_.structuralRank),
      Vector(1)
    )
  }

  test("public API selects retained non-head plans by exact rank and id") {
    val rewriter = standardRewriter()
    val baseline = rewriter.rewriteDetailed(
      RewriteRequest(
        triangleSql,
        options = RewriteOptions(widthBound = 2, planLimit = 10)
      )
    )
    val baselineArtifact = artifact(baseline)
    assertEquals(baselineArtifact.candidateSearch.plans.size, 4)

    val targets = Vector(
      4 -> CandidatePlanSelection.StructuralRank(4),
      3 -> CandidatePlanSelection.ExactCandidateId(
        baselineArtifact.candidateSearch.plans(2).candidateId
      )
    )
    targets.foreach { case (rank, policy) =>
      val detailed = rewriter.rewriteDetailed(
        RewriteRequest(
          triangleSql,
          options = RewriteOptions(
            widthBound = 2,
            planLimit = 10,
            candidateSelection = policy
          )
        )
      )
      val selected = artifact(detailed)
      val expected = selected.candidateSearch.plans(rank - 1)

      assertEquals(selected.selectedPlan, expected)
      assertEquals(selected.selectedPlan.rank, rank)
      assertEquals(selected.candidateSelection.policy, policy)
      assertEquals(selected.candidateSelection.selectedCandidateId, expected.candidateId)
      assertEquals(selected.candidateSelection.selectedStructuralRank, rank)
      assertEquals(
        selected.candidateSelection.evaluations.map(evaluation =>
          evaluation.structuralRank -> evaluation.candidateId
        ),
        Vector(rank -> expected.candidateId)
      )
      assertEquals(selected.bagFactorPlan.decomposition, expected.decomposition)
      assertEquals(
        detailed.outcome,
        rewriter.rewrite(
          RewriteRequest(
            triangleSql,
            options = RewriteOptions(
              widthBound = 2,
              planLimit = 10,
              candidateSelection = policy
            )
          )
        )
      )
    }
    assertNotEquals(
      rewrittenStatement(baseline.outcome),
      rewrittenStatement(
        rewriter.rewrite(
          RewriteRequest(
            triangleSql,
            options = RewriteOptions(
              planLimit = 10,
              candidateSelection = CandidatePlanSelection.StructuralRank(4)
            )
          )
        )
      )
    )
  }

  test("unretained rank and id are typed failures with no fallback artifact") {
    val rewriter = standardRewriter()
    val missingId = CandidatePlanId
      .parse("sha256:" + "0" * 64)
      .fold(problem => fail(problem.message), identity)
    val policies = Vector[CandidatePlanSelection](
      CandidatePlanSelection.StructuralRank(5),
      CandidatePlanSelection.ExactCandidateId(missingId)
    )

    policies.foreach { policy =>
      val detailed = rewriter.rewriteDetailed(
        RewriteRequest(
          triangleSql,
          options = RewriteOptions(
            planLimit = 10,
            candidateSelection = policy
          )
        )
      )

      assert(detailed.outcome.isInstanceOf[RewriteOutcome.Failed], clues(policy))
      assertEquals(
        detailed.outcome.diagnostics.map(_.code),
        Vector(RewriteReasonCode.CandidateSelectionFailed)
      )
      assertEquals(
        detailed.outcome.diagnostics.head.context("algorithmCode"),
        "selection.candidate-not-retained"
      )
      assertEquals(
        detailed.report.completedPhase,
        PostgreSqlRewritePhase.CandidateSelectionCompleted
      )
      detailed.report.candidateSelectionDecision match {
        case Some(unavailable: PostgreSqlCandidateSelectionDecision.Unavailable) =>
          assertEquals(unavailable.policy, policy)
          assertEquals(unavailable.retainedCandidateCount, 4)
          assertEquals(unavailable.evaluations, Vector.empty)
        case other => fail(s"expected unavailable decision, received $other")
      }
      assertEquals(detailed.report.bagFactorPlan, None)
      assertEquals(detailed.report.artifact, None)
    }
  }

  test("cumulative preflight exhaustion fails before accepting a partial winner") {
    val limits = PostgreSqlCandidateSelectionLimits
      .create(maxEvaluatedCandidates = 3L)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val rewriter = standardRewriter(selectionLimits = limits)
    val detailed = rewriter.rewriteDetailed(
      RewriteRequest(
        triangleSql,
        options = RewriteOptions(
          planLimit = 10,
          candidateSelection = CandidatePlanSelection.PostgreSqlStaticShapeV1
        )
      )
    )

    assert(detailed.outcome.isInstanceOf[RewriteOutcome.Failed])
    assertEquals(
      detailed.outcome.diagnostics.map(_.code),
      Vector(RewriteReasonCode.CandidateSelectionFailed)
    )
    val context = detailed.outcome.diagnostics.head.context
    assertEquals(context("algorithmCode"), "selection.resource-limit.exceeded")
    assertEquals(context("resource"), "evaluated-candidates")
    assertEquals(context("required"), "4")
    assertEquals(context("limit"), "3")
    assertEquals(
      detailed.report.completedPhase,
      PostgreSqlRewritePhase.CandidateSearchCompleted
    )
    detailed.report.candidateSelectionDecision match {
      case Some(failed: PostgreSqlCandidateSelectionDecision.EvaluationFailed) =>
        assertEquals(failed.policy, CandidatePlanSelection.PostgreSqlStaticShapeV1)
        assertEquals(failed.evaluations, Vector.empty)
        assertEquals(failed.failedStructuralRank, 1)
      case other => fail(s"expected failed selection evidence, received $other")
    }
    assertEquals(detailed.report.bagFactorPlan, None)
    assertEquals(detailed.report.artifact, None)
  }

  test("rewriter configuration rejects a missing selection limit envelope") {
    val planner = standardPlanner()
    val failure = PostgreSqlQueryRewriter
      .create(planner, selectionLimits = null)
      .left
      .toOption
      .getOrElse(fail("null selection limits were accepted"))
    assert(failure.message.contains("candidate selection resource limits"))
  }

  test("public outcomes bind one reduction mode through selection, artifact, and rendering") {
    val rewriter = standardRewriter()
    val results = YannakakisReductionMode.Values.map { mode =>
      val request = RewriteRequest(
        triangleSql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = 10,
          reductionMode = mode
        )
      )
      val detailed = rewriter.rewriteDetailed(request)
      val selected = artifact(detailed)
      val reduction = selected.renderedQuery.trace.reduction

      assert(detailed.outcome.isInstanceOf[RewriteOutcome.Rewritten])
      assertEquals(selected.candidateSelection.reductionMode, mode)
      assertEquals(
        selected.candidateSelection.reductionOrder,
        PostgreSqlPhysicalVariantId.CanonicalReductionOrder
      )
      assertEquals(
        selected.candidateSelection.selectedPhysicalVariantId,
        PostgreSqlPhysicalVariantId.from(selected.selectedPlan.candidateId, mode)
      )
      assert(selected.candidateSelection.evaluations.forall(_.reductionMode == mode))
      assertEquals(reduction.mode, mode)
      assertEquals(reduction.reductionOrder, selected.candidateSelection.reductionOrder)
      assertEquals(
        reduction.canonicalBottomUpStepCount,
        selected.bagFactorPlan.schedule.bottomUp.size
      )
      assertEquals(
        reduction.canonicalTopDownStepCount,
        selected.bagFactorPlan.schedule.topDown.size
      )
      mode match {
        case YannakakisReductionMode.FullTwoPass =>
          assertEquals(
            reduction.selectedBottomUpStepCount,
            reduction.canonicalBottomUpStepCount
          )
          assertEquals(
            reduction.selectedTopDownStepCount,
            reduction.canonicalTopDownStepCount
          )
        case YannakakisReductionMode.BottomUpOnly =>
          assertEquals(
            reduction.selectedBottomUpStepCount,
            reduction.canonicalBottomUpStepCount
          )
          assertEquals(reduction.selectedTopDownStepCount, 0)
        case YannakakisReductionMode.NoReduction =>
          assertEquals(reduction.selectedBottomUpStepCount, 0)
          assertEquals(reduction.selectedTopDownStepCount, 0)
          assertEquals(reduction.physicalReductionCteCount, 0)
      }
      mode -> selected
    }

    assertEquals(
      results.map(_._2.selectedPlan.candidateId).distinct.size,
      1,
      "reduction mode must not change logical candidate identity"
    )
    assertEquals(
      results.map(_._2.candidateSelection.selectedPhysicalVariantId).distinct.size,
      YannakakisReductionMode.Values.size
    )

    val default = rewriter.rewriteDetailed(
      RewriteRequest(
        triangleSql,
        options = RewriteOptions(widthBound = 2, planLimit = 10)
      )
    )
    val explicitFull = rewriter.rewriteDetailed(
      RewriteRequest(
        triangleSql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = 10,
          reductionMode = YannakakisReductionMode.FullTwoPass
        )
      )
    )
    assertEquals(default.outcome, explicitFull.outcome)
    val defaultArtifact = artifact(default)
    val explicitArtifact = artifact(explicitFull)
    assertEquals(defaultArtifact.candidateSearch, explicitArtifact.candidateSearch)
    assertEquals(defaultArtifact.candidateSelection, explicitArtifact.candidateSelection)
    assertEquals(defaultArtifact.selectedPlan, explicitArtifact.selectedPlan)
    assertEquals(defaultArtifact.bagFactorPlan, explicitArtifact.bagFactorPlan)
    assertEquals(
      defaultArtifact.renderedQuery.sql,
      explicitArtifact.renderedQuery.sql
    )
    assertEquals(
      defaultArtifact.renderedQuery.trace,
      explicitArtifact.renderedQuery.trace
    )
    assertEquals(
      defaultArtifact.renderedQuery.statistics,
      explicitArtifact.renderedQuery.statistics
    )
  }

  test("unavailable public selection retains the requested reduction mode") {
    val rewriter = standardRewriter()
    val detailed = rewriter.rewriteDetailed(
      RewriteRequest(
        triangleSql,
        options = RewriteOptions(
          planLimit = 10,
          candidateSelection = CandidatePlanSelection.StructuralRank(5),
          reductionMode = YannakakisReductionMode.BottomUpOnly
        )
      )
    )

    detailed.report.candidateSelectionDecision match {
      case Some(value: PostgreSqlCandidateSelectionDecision.Unavailable) =>
        assertEquals(value.reductionMode, YannakakisReductionMode.BottomUpOnly)
        assertEquals(
          value.reductionOrder,
          PostgreSqlPhysicalVariantId.CanonicalReductionOrder
        )
        assertEquals(value.evaluations, Vector.empty)
      case other => fail(s"expected unavailable selection, received $other")
    }
    assert(detailed.outcome.isInstanceOf[RewriteOutcome.Failed])
    assertEquals(detailed.report.artifact, None)
  }

  private def standardRewriter(
      selectionLimits: PostgreSqlCandidateSelectionLimits =
        PostgreSqlCandidateSelectionLimits.default
  ): PostgreSqlQueryRewriter =
    PostgreSqlQueryRewriter
      .create(standardPlanner(), selectionLimits = selectionLimits)
      .fold(problem => fail(problem.message), identity)

  private def standardPlanner(): CalciteSqlPlanner = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
  }

  private def artifact(
      detailed: PostgreSqlRewriteResult
  ): PostgreSqlRewriteArtifact = {
    assert(detailed.outcome.isInstanceOf[RewriteOutcome.Rewritten])
    detailed.report.artifact.getOrElse(fail("successful rewrite has no artifact"))
  }

  private def rewrittenStatement(outcome: RewriteOutcome): String = outcome match {
    case value: RewriteOutcome.Rewritten =>
      value.rewrittenStatements.headOption.getOrElse(
        fail("rewritten outcome contains no statement")
      )
    case other => fail(s"expected rewritten outcome, received $other")
  }
}
