package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidateResourceLimits,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlEnumerationRenderingProfile,
  PostgreSqlRenderingLimits,
  PostgreSqlStageDiagnosticLimits,
  PostgreSqlStageId
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  YannakakisReductionMode
}

class PostgreSqlRetainedCandidateBatchSuite extends munit.FunSuite {
  import PostgreSqlFactorAwareContractTestSupport.{q5, triangle}

  test("batch compiles the exact K=10 prefix once in retained-rank order") {
    val found = enumerate(q5.input, 10)
    assertEquals(found.plans.size, 10)

    val batch = compile(q5.query, q5.input, found)
    assertEquals(batch.requestedLimit, 10)
    assertEquals(batch.retainedCount, 10)
    assertEquals(batch.exhaustedBeforeK, false)
    assertEquals(batch.candidates.map(_.plan), found.plans)
    assertEquals(batch.candidates.map(_.plan.rank), (1 to 10).toVector)
    assertEquals(
      batch.reductionMode,
      YannakakisReductionMode.FullTwoPass
    )
    assertEquals(
      batch.materializationDirective,
      PostgreSqlCteMaterializationDirective.PlannerControlled
    )

    batch.candidates.foreach { candidate =>
      assertEquals(
        candidate.stageDiagnostics.statements.last.stageId,
        PostgreSqlStageId.FinalReconstruction
      )
      assertEquals(
        candidate.stageDiagnostics.statements.last.statementSha256,
        candidate.renderedStatementSha256
      )
      assertEquals(
        candidate.physicalVariantId,
        PostgreSqlPhysicalVariantId.from(
          candidate.plan.candidateId,
          YannakakisReductionMode.FullTwoPass
        )
      )
    }
  }

  test("batch SQL and bag-factor plans equal current exact-rank compilation") {
    val found = enumerate(triangle.input, 10)
    val batch = compile(triangle.query, triangle.input, found)

    batch.candidates.foreach { candidate =>
      val selected = PostgreSqlCandidateSelector
        .select(
          triangle.query,
          triangle.input,
          found,
          CandidatePlanSelection.StructuralRank(candidate.plan.rank),
          YannakakisReductionMode.FullTwoPass,
          PostgreSqlCandidateSelectionLimits.default,
          BagFactorPlanningLimits.default,
          PostgreSqlRenderingLimits.default
        )
        .fold(problem => fail(s"exact-rank compilation failed: $problem"), identity)

      assertEquals(selected.plan, candidate.plan)
      assertEquals(selected.bagFactorPlan, candidate.bagFactorPlan)
      assertEquals(selected.renderedQuery.sql, candidate.renderedQuery.sql)
      assertEquals(
        selected.renderedQuery.statistics,
        candidate.renderedQuery.statistics
      )
      assertEquals(selected.renderedQuery.trace, candidate.renderedQuery.trace)
    }
  }

  test("frozen legacy batch is explicit and hashes its distinct SQL stages") {
    val found = enumerate(triangle.input, 10)
    val current = compile(triangle.query, triangle.input, found)
    val legacy = PostgreSqlRetainedCandidateBatchCompiler
      .compileWithEnumerationProfile(
        triangle.query,
        triangle.input,
        found,
        BagFactorPlanningLimits.default,
        PostgreSqlRenderingLimits.default,
        diagnosticLimits,
        PostgreSqlEnumerationRenderingProfile.LegacyV1
      )
      .fold(problem => fail(s"legacy batch compilation failed: $problem"), identity)

    assertEquals(current.candidates.map(_.plan), legacy.candidates.map(_.plan))
    current.candidates.zip(legacy.candidates).foreach {
      case (safeCandidate, legacyCandidate) =>
        assertNotEquals(
          safeCandidate.renderedQuery.sql,
          legacyCandidate.renderedQuery.sql
        )
        assertNotEquals(
          safeCandidate.renderedStatementSha256,
          legacyCandidate.renderedStatementSha256
        )
        assertEquals(
          safeCandidate.stageDiagnostics.statements.map(_.stageId),
          legacyCandidate.stageDiagnostics.statements.map(_.stageId)
        )
        assertEquals(
          legacyCandidate.stageDiagnostics.statements.last.statementSha256,
          legacyCandidate.renderedStatementSha256
        )
    }
  }

  test("exhausted-before-K is derived only from the exact search decision") {
    val limited = enumerate(q5.input, 1)
    val atLimit = compile(q5.query, q5.input, limited)
    assertEquals(atLimit.retainedCount, 1)
    assertEquals(atLimit.exhaustedBeforeK, false)

    val exhausted = enumerate(triangle.input, 10)
    assert(exhausted.plans.size < exhausted.requestedLimit)
    val short = compile(triangle.query, triangle.input, exhausted)
    assertEquals(short.retainedCount, exhausted.plans.size)
    assertEquals(short.exhaustedBeforeK, true)
  }

  test("batch rejects reordered, gapped, and over-envelope search decisions") {
    val found = enumerate(triangle.input, 10)
    val reordered = found.copy(plans = found.plans.reverse)
    val gapped = found.copy(
      plans = found.plans.updated(1, found.plans(1).copy(rank = 3))
    )
    val overEnvelope = found.copy(requestedLimit = 11)

    Vector(reordered, gapped, overEnvelope).foreach { hostile =>
      val failure = PostgreSqlRetainedCandidateBatchCompiler
        .compile(
          triangle.query,
          triangle.input,
          hostile,
          BagFactorPlanningLimits.default,
          PostgreSqlRenderingLimits.default,
          diagnosticLimits
        )
        .swap
        .toOption
        .getOrElse(fail("hostile search decision was accepted"))
      assert(
        failure.isInstanceOf[
          PostgreSqlRetainedCandidateBatchFailure.InvalidCandidateSet
        ]
      )
    }
  }

  test("batch fails the whole prefix when one retained render exceeds limits") {
    val found = enumerate(triangle.input, 10)
    val tight = PostgreSqlRenderingLimits
      .create(maxSqlCharacters = 1L)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

    val failure = PostgreSqlRetainedCandidateBatchCompiler
      .compile(
        triangle.query,
        triangle.input,
        found,
        BagFactorPlanningLimits.default,
        tight,
        diagnosticLimits
      )
      .swap
      .toOption
      .getOrElse(fail("render-limit failure was accepted"))

    failure match {
      case PostgreSqlRetainedCandidateBatchFailure.CompilationFailed(
            rank,
            candidateId,
            _: PostgreSqlCandidateCompiler.Failure.RenderingFailed
          ) =>
        assertEquals(rank, 1)
        assertEquals(candidateId, found.plans.head.candidateId)
      case other => fail(s"unexpected batch failure: $other")
    }
  }

  private def compile(
      query: at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryIR,
      input: at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput,
      found: CandidatePlanDecision.Found
  ): PostgreSqlRetainedCandidateBatch =
    PostgreSqlRetainedCandidateBatchCompiler
      .compile(
        query,
        input,
        found,
        BagFactorPlanningLimits.default,
        PostgreSqlRenderingLimits.default,
        diagnosticLimits
      )
      .fold(problem => fail(s"batch compilation failed: $problem"), identity)

  private def enumerate(
      input: at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput,
      limit: Int
  ): CandidatePlanDecision.Found =
    TopKCandidateTD
      .enumerate(input, limit, CandidateResourceLimits.default)
      .fold(problem => fail(problem.message), identity) match {
      case value: CandidatePlanDecision.Found => value
      case other => fail(s"expected found search decision, received $other")
    }

  private val diagnosticLimits: PostgreSqlStageDiagnosticLimits =
    PostgreSqlStageDiagnosticLimits
      .create(
        maxStatements = 256L,
        maxCharactersPerStatement = 16L * 1024L * 1024L,
        maxTotalCharacters = 64L * 1024L * 1024L
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
}
