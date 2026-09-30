package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidatePlanId,
  CandidateSearchInput,
  RankedCandidatePlan,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlanner,
  BagFactorPlanningLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlBagFactorRenderer,
  PostgreSqlBagSupportStrategy,
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingLimits
}
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

class PostgreSqlCandidateSelectorSuite extends munit.FunSuite {
  import PostgreSqlCandidateSelectionFailure._

  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  private val triangleTablesWithDeadIds = Vector(
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

  private val frozenTriangleCandidateIds = Vector(
    "sha256:b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d",
    "sha256:3db4f5752c2a2a6de0c2f6c71b80fd0d39af82a9b347750026f29f46b8572d43",
    "sha256:ebb724027f175a60cbd89ed4e819e5fef5131045ccfb81eb42635bba762b87a7",
    "sha256:d9710bb95f1744f317237069e3ca63f1f6a4294dd5a1449dfdaef11d2dc62087"
  )

  test("structural head is byte-identical to the former direct rank-one path") {
    val fixture = preparedTriangle()
    val expected = render(fixture, fixture.found.plans.head)
    val selected = select(
      fixture,
      CandidatePlanSelection.StructuralHead
    ).fold(problem => fail(problem.toString), identity)

    assertEquals(selected.plan, fixture.found.plans.head)
    assertEquals(selected.renderedQuery, expected)
    assertEquals(selected.decision.policy, CandidatePlanSelection.StructuralHead)
    assertEquals(selected.decision.evaluations.map(_.structuralRank), Vector(1))
    assertEquals(
      selected.decision.evaluations.map(_.candidateId),
      Vector(fixture.found.plans.head.candidateId)
    )
    assertEquals(selected.decision.costSelected, false)
    assertEquals(selected.decision.usesCatalogStatistics, false)
    assertEquals(selected.decision.usesExecutionFeedback, false)
  }

  test("explicit rank and id select exactly one retained candidate") {
    val fixture = preparedTriangle()

    fixture.found.plans.foreach { expected =>
      val byRank = select(
        fixture,
        CandidatePlanSelection.StructuralRank(expected.rank)
      ).fold(problem => fail(problem.toString), identity)
      val byId = select(
        fixture,
        CandidatePlanSelection.ExactCandidateId(expected.candidateId)
      ).fold(problem => fail(problem.toString), identity)

      Vector(byRank, byId).foreach { selected =>
        assertEquals(selected.plan, expected)
        assertEquals(selected.renderedQuery, render(fixture, expected))
        assertEquals(selected.decision.selectedStructuralRank, expected.rank)
        assertEquals(selected.decision.selectedCandidateId, expected.candidateId)
        assertEquals(
          selected.decision.evaluations.map(evaluation =>
            evaluation.structuralRank -> evaluation.candidateId
          ),
          Vector(expected.rank -> expected.candidateId)
        )
      }
    }
  }

  test("unavailable rank and id fail without selecting the structural head") {
    val fixture = preparedTriangle()
    val missingId = CandidatePlanId
      .parse("sha256:" + "0" * 64)
      .fold(problem => fail(problem.message), identity)
    assert(!fixture.found.plans.exists(_.candidateId == missingId))

    val policies = Vector[CandidatePlanSelection](
      CandidatePlanSelection.StructuralRank(fixture.found.plans.size + 1),
      CandidatePlanSelection.ExactCandidateId(missingId)
    )
    policies.foreach { policy =>
      select(fixture, policy) match {
        case Left(Unavailable(report)) =>
          assertEquals(report.policy, policy)
          assertEquals(report.retainedCandidateCount, fixture.found.plans.size)
          assertEquals(report.evaluations, Vector.empty)
        case other => fail(s"expected unavailable selection for $policy, received $other")
      }
    }
  }

  test("static-shape selection evaluates every plan and uses the frozen tuple") {
    val fixture = preparedTriangle()
    val selected = select(
      fixture,
      CandidatePlanSelection.PostgreSqlStaticShapeV1
    ).fold(problem => fail(problem.toString), identity)
    val independentlyRendered = fixture.found.plans.map(plan =>
      plan -> render(fixture, plan)
    )
    val expectedScores = independentlyRendered.map { case (plan, rendered) =>
      plan -> independentScore(plan, rendered)
    }

    assertEquals(
      fixture.found.plans.map(_.candidateId.value),
      frozenTriangleCandidateIds
    )
    independentlyRendered.foreach { case (_, rendered) =>
      (0 to 2).foreach { edge =>
        assert(
          !rendered.sql.contains(s"\"yr_e${edge}_c0\""),
          clues(edge, rendered.sql)
        )
      }
    }

    assertEquals(
      selected.decision.evaluations.map(_.structuralRank),
      fixture.found.plans.map(_.rank)
    )
    selected.decision.evaluations.zip(independentlyRendered).foreach {
      case (evaluation, (plan, rendered)) =>
        assertEquals(evaluation.candidateId, plan.candidateId)
        assertEquals(evaluation.structuralRank, plan.rank)
        assertEquals(evaluation.score, independentScore(plan, rendered))
        assertEquals(evaluation.renderedStatementSha256, sha256(rendered.sql))
    }

    val expected = expectedScores.minBy { case (_, score) =>
      (
        score.expandedScanCount,
        score.materializedDistinctSupportCount,
        score.cteCount,
        score.renderedTermCount,
        score.sqlCharacterCount,
        score.structuralRank
      )
    }
    assertEquals(selected.plan, expected._1)
    assertEquals(selected.plan.rank, 1)
    assertEquals(selected.plan.candidateId.value, frozenTriangleCandidateIds.head)
    assertEquals(selected.decision.selectedCandidateId, expected._1.candidateId)
    assertEquals(selected.decision.selectedStructuralRank, expected._1.rank)
  }

  test("all cumulative selection resources accept exact totals and fail one below") {
    val fixture = preparedTriangle()
    val baseline = select(
      fixture,
      CandidatePlanSelection.PostgreSqlStaticShapeV1
    ).fold(problem => fail(problem.toString), identity)
    val evaluations = baseline.decision.evaluations
    val totals = Totals(
      evaluated = evaluations.size.toLong,
      scans = evaluations.map(_.score.expandedScanCount).sum,
      ctes = evaluations.map(_.score.cteCount).sum,
      terms = evaluations.map(_.score.renderedTermCount).sum,
      characters = evaluations.map(_.score.sqlCharacterCount).sum
    )
    assert(totals.values.forall(_ > 1L), clues(totals))

    val atBoundary = select(
      fixture,
      CandidatePlanSelection.PostgreSqlStaticShapeV1,
      selectionLimits(totals)
    ).fold(problem => fail(problem.toString), identity)
    assertEquals(atBoundary.decision, baseline.decision)
    assertEquals(atBoundary.plan, baseline.plan)

    val cases = Vector(
      (
        PostgreSqlCandidateSelectionResource.CumulativeExpandedScans,
        totals.copy(scans = totals.scans - 1L),
        totals.scans
      ),
      (
        PostgreSqlCandidateSelectionResource.CumulativeCtes,
        totals.copy(ctes = totals.ctes - 1L),
        totals.ctes
      ),
      (
        PostgreSqlCandidateSelectionResource.CumulativeRenderedTerms,
        totals.copy(terms = totals.terms - 1L),
        totals.terms
      ),
      (
        PostgreSqlCandidateSelectionResource.CumulativeSqlCharacters,
        totals.copy(characters = totals.characters - 1L),
        totals.characters
      )
    )
    cases.foreach { case (resource, limits, required) =>
      select(
        fixture,
        CandidatePlanSelection.PostgreSqlStaticShapeV1,
        selectionLimits(limits)
      ) match {
        case Left(failure: ResourceLimitExceeded) =>
          assertEquals(failure.resource, resource)
          assertEquals(failure.required, BigInt(required))
          assertEquals(failure.limit, required - 1L)
          assertEquals(failure.plan.rank, fixture.found.plans.last.rank)
          assertEquals(
            failure.report.evaluations.map(_.structuralRank),
            fixture.found.plans.dropRight(1).map(_.rank)
          )
        case other => fail(s"expected $resource exhaustion, received $other")
      }
    }

    select(
      fixture,
      CandidatePlanSelection.PostgreSqlStaticShapeV1,
      selectionLimits(totals.copy(evaluated = totals.evaluated - 1L))
    ) match {
      case Left(failure: ResourceLimitExceeded) =>
        assertEquals(
          failure.resource,
          PostgreSqlCandidateSelectionResource.EvaluatedCandidates
        )
        assertEquals(failure.required, BigInt(totals.evaluated))
        assertEquals(failure.limit, totals.evaluated - 1L)
        assertEquals(failure.report.evaluations, Vector.empty)
      case other => fail(s"expected evaluated-candidate exhaustion, received $other")
    }
  }

  test("multi-candidate rendering failure aborts after a viable earlier plan") {
    val fixture = preparedTriangle()
    val rankOneScans = render(fixture, fixture.found.plans.head)
      .statistics
      .expandedScanCount
    val renderingLimits = PostgreSqlRenderingLimits
      .create(maxExpandedScans = rankOneScans)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

    val head = select(
      fixture,
      CandidatePlanSelection.StructuralHead,
      renderingLimits = renderingLimits
    ).fold(problem => fail(problem.toString), identity)
    assertEquals(head.plan.rank, 1)

    select(
      fixture,
      CandidatePlanSelection.PostgreSqlStaticShapeV1,
      renderingLimits = renderingLimits
    ) match {
      case Left(failure: RenderingFailed) =>
        assertEquals(failure.plan.rank, 2)
        assertEquals(failure.report.evaluations.map(_.structuralRank), Vector(1))
      case other => fail(s"expected fail-closed static evaluation, received $other")
    }

    select(
      fixture,
      CandidatePlanSelection.StructuralRank(2),
      renderingLimits = renderingLimits
    ) match {
      case Left(failure: RenderingFailed) =>
        assertEquals(failure.plan.rank, 2)
        assertEquals(failure.report.evaluations, Vector.empty)
      case other => fail(s"expected exact rank failure without fallback, received $other")
    }
  }

  test("every policy evaluates one request-wide reduction mode without changing candidate IDs") {
    val fixture = preparedTriangle()
    val staticSelections = YannakakisReductionMode.Values.map { mode =>
      val selected = select(
        fixture,
        CandidatePlanSelection.PostgreSqlStaticShapeV1,
        reductionMode = mode
      ).fold(problem => fail(problem.toString), identity)

      assertEquals(selected.decision.reductionMode, mode)
      assertEquals(
        selected.decision.reductionOrder,
        PostgreSqlPhysicalVariantId.CanonicalReductionOrder
      )
      assertEquals(selected.decision.evaluations.size, fixture.found.plans.size)
      assertEquals(
        selected.decision.evaluations.map(_.candidateId.value),
        frozenTriangleCandidateIds
      )
      selected.decision.evaluations.zip(fixture.found.plans).foreach {
        case (evaluation, plan) =>
          val direct = render(fixture, plan, mode)
          assertEquals(evaluation.structuralRank, plan.rank)
          assertEquals(evaluation.reductionMode, mode)
          assertEquals(
            evaluation.reductionOrder,
            PostgreSqlPhysicalVariantId.CanonicalReductionOrder
          )
          assertEquals(
            evaluation.physicalVariantId,
            PostgreSqlPhysicalVariantId.from(plan.candidateId, mode)
          )
          assertEquals(evaluation.score, independentScore(plan, direct))
          assertEquals(evaluation.renderedStatementSha256, sha256(direct.sql))
      }
      assertEquals(selected.renderedQuery.trace.reduction.mode, mode)
      mode -> selected
    }

    assertEquals(
      staticSelections.map(_._2.decision.evaluations.map(_.candidateId)),
      Vector.fill(YannakakisReductionMode.Values.size)(fixture.found.plans.map(_.candidateId))
    )
    fixture.found.plans.indices.foreach { index =>
      val ids = staticSelections.map(_._2.decision.evaluations(index).physicalVariantId)
      assertEquals(ids.distinct.size, YannakakisReductionMode.Values.size)
    }

    val exactPolicies = Vector[CandidatePlanSelection](
      CandidatePlanSelection.StructuralHead,
      CandidatePlanSelection.StructuralRank(4),
      CandidatePlanSelection.ExactCandidateId(fixture.found.plans(2).candidateId)
    )
    exactPolicies.foreach { policy =>
      YannakakisReductionMode.Values.foreach { mode =>
        val selected = select(fixture, policy, reductionMode = mode)
          .fold(problem => fail(problem.toString), identity)
        assertEquals(selected.decision.reductionMode, mode)
        assertEquals(selected.decision.evaluations.size, 1)
        assertEquals(selected.renderedQuery.trace.reduction.mode, mode)
      }
    }
  }

  test("default selection is explicit-full and failure evidence retains its physical variant") {
    val fixture = preparedTriangle()
    val default = select(fixture, CandidatePlanSelection.StructuralHead)
    val explicit = select(
      fixture,
      CandidatePlanSelection.StructuralHead,
      reductionMode = YannakakisReductionMode.FullTwoPass
    )
    assertEquals(default, explicit)

    select(
      fixture,
      CandidatePlanSelection.StructuralRank(5),
      reductionMode = YannakakisReductionMode.BottomUpOnly
    ) match {
      case Left(Unavailable(report)) =>
        assertEquals(report.reductionMode, YannakakisReductionMode.BottomUpOnly)
        assertEquals(report.evaluations, Vector.empty)
      case other => fail(s"expected unavailable bottom-up selection, received $other")
    }

    val rankOneScans = render(
      fixture,
      fixture.found.plans.head,
      YannakakisReductionMode.NoReduction
    ).statistics.expandedScanCount
    val renderingLimits = PostgreSqlRenderingLimits
      .create(maxExpandedScans = rankOneScans)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    select(
      fixture,
      CandidatePlanSelection.PostgreSqlStaticShapeV1,
      reductionMode = YannakakisReductionMode.NoReduction,
      renderingLimits = renderingLimits
    ) match {
      case Left(failure: RenderingFailed) =>
        assertEquals(failure.report.reductionMode, YannakakisReductionMode.NoReduction)
        assertEquals(failure.report.failedStructuralRank, 2)
        assertEquals(
          failure.report.failedPhysicalVariantId,
          PostgreSqlPhysicalVariantId.from(
            failure.report.failedCandidateId,
            YannakakisReductionMode.NoReduction
          )
        )
        assert(failure.report.evaluations.forall(
          _.reductionMode == YannakakisReductionMode.NoReduction
        ))
      case other => fail(s"expected fail-closed no-reduction evaluation, received $other")
    }
  }

  test("selection evidence rejects cross-mode or mismatched physical identities") {
    val fixture = preparedTriangle()
    val selected = select(
      fixture,
      CandidatePlanSelection.PostgreSqlStaticShapeV1,
      reductionMode = YannakakisReductionMode.FullTwoPass
    ).fold(problem => fail(problem.toString), identity)
    val decision = selected.decision
    val bottomUpId = PostgreSqlPhysicalVariantId.from(
      decision.selectedCandidateId,
      YannakakisReductionMode.BottomUpOnly
    )

    intercept[IllegalArgumentException] {
      decision.copy(reductionMode = YannakakisReductionMode.BottomUpOnly)
    }
    intercept[IllegalArgumentException] {
      decision.copy(selectedPhysicalVariantId = bottomUpId)
    }
    intercept[IllegalArgumentException] {
      decision.evaluations.head.copy(
        reductionMode = YannakakisReductionMode.BottomUpOnly
      )
    }
    intercept[IllegalArgumentException] {
      decision.evaluations.head.copy(physicalVariantId = bottomUpId)
    }
  }

  private final case class Prepared(
      query: QueryIR,
      input: CandidateSearchInput,
      found: CandidatePlanDecision.Found
  )

  private final case class Totals(
      evaluated: Long,
      scans: Long,
      ctes: Long,
      terms: Long,
      characters: Long
  ) {
    val values: Vector[Long] =
      Vector(evaluated, scans, ctes, terms, characters)
  }

  private def preparedTriangle(): Prepared = {
    val planner = CalcitePlannerFixture(triangleTablesWithDeadIds)
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
      case value: CandidatePlanDecision.Rejected =>
        fail(s"triangle unexpectedly rejected: $value")
    }
    assertEquals(found.plans.size, 4)
    assertEquals(found.plans.map(_.rank), Vector(1, 2, 3, 4))
    assertEquals(found.plans.map(_.candidateId.value), frozenTriangleCandidateIds)
    Prepared(analysis.query, input, found)
  }

  private def select(
      fixture: Prepared,
      policy: CandidatePlanSelection,
      limits: PostgreSqlCandidateSelectionLimits =
        PostgreSqlCandidateSelectionLimits.default,
      bagFactorLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default,
      renderingLimits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default,
      reductionMode: YannakakisReductionMode = YannakakisReductionMode.Default
  ): Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate] =
    PostgreSqlCandidateSelector.select(
      fixture.query,
      fixture.input,
      fixture.found,
      policy,
      reductionMode,
      limits,
      bagFactorLimits,
      renderingLimits
    )

  private def render(
      fixture: Prepared,
      plan: RankedCandidatePlan,
      reductionMode: YannakakisReductionMode = YannakakisReductionMode.Default
  ): PostgreSqlRenderedQuery = {
    val bagFactor = BagFactorPlanner
      .plan(fixture.input, plan.decomposition)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    PostgreSqlBagFactorRenderer
      .renderWithReductionMode(
        fixture.query,
        fixture.input,
        bagFactor,
        reductionMode
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
  }

  private def independentScore(
      plan: RankedCandidatePlan,
      rendered: PostgreSqlRenderedQuery
  ): PostgreSqlStaticShapeScore = {
    val statistics = rendered.statistics
    PostgreSqlStaticShapeScore(
      statistics.expandedScanCount,
      rendered.trace.bags.count(
        _.supportStrategy ==
          PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
      ).toLong,
      statistics.cteCount,
      statistics.renderedTermCount,
      statistics.sqlCharacterCount,
      plan.rank
    )
  }

  private def selectionLimits(
      totals: Totals
  ): PostgreSqlCandidateSelectionLimits =
    PostgreSqlCandidateSelectionLimits
      .create(
        maxEvaluatedCandidates = totals.evaluated,
        maxCumulativeExpandedScans = totals.scans,
        maxCumulativeCtes = totals.ctes,
        maxCumulativeRenderedTerms = totals.terms,
        maxCumulativeSqlCharacters = totals.characters
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def sha256(value: String): String = {
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
    bytes.iterator.map(byte => f"${byte & 0xff}%02x").mkString
  }
}
