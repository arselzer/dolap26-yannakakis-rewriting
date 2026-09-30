package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}

class PostgreSqlEnumerationBatchApiSuite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("producer plans the source once and emits every retained rank without a winner") {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    var plannerCalls = 0
    val planner = CalciteSqlPlanner
      .fromFunction { sql =>
        plannerCalls += 1
        Right(fixture.plan(sql).root)
      }
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
    val request = PostgreSqlEnumerationBatchRequest
      .create(triangleSql, planLimit = 10)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)

    val result = rewriter.produceEnumerationBatch(request)

    assertEquals(plannerCalls, 1)
    assertEquals(result.status, PostgreSqlEnumerationBatchStatus.Produced)
    assertEquals(result.completedPhase, PostgreSqlRewritePhase.Rendered)
    assertEquals(result.diagnostics, Vector.empty)
    val artifact = result.artifact.getOrElse(fail("produced batch has no artifact"))
    assert(artifact.producerSearchContract eq result.producerSearchContract)
    assert(artifact.batch.query eq artifact.analysis.query)
    assertEquals(artifact.batch.candidates.map(_.plan.rank), Vector(1, 2, 3, 4))
    assertEquals(artifact.batch.requestedLimit, 10)
    assertEquals(artifact.batch.exhaustedBeforeK, true)
    assertEquals(
      artifact.batch.candidates.map(_.plan),
      artifact.batch.search.plans
    )
  }

  test("K=1 is a retained-limit result rather than false search exhaustion") {
    val rewriter = standardRewriter()
    val request = PostgreSqlEnumerationBatchRequest
      .create(triangleSql, planLimit = 1)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    val result = rewriter.produceEnumerationBatch(request)
    val batch = result.artifact
      .getOrElse(fail(s"K=1 producer failed: ${result.diagnostics}"))
      .batch

    assertEquals(batch.retainedCount, 1)
    assertEquals(batch.requestedLimit, 1)
    assertEquals(batch.exhaustedBeforeK, false)
  }

  test("artifact rejects width or ConCov drift from the captured producer contract") {
    val result = standardRewriter().produceEnumerationBatch(
      PostgreSqlEnumerationBatchRequest
        .create(triangleSql, planLimit = 1)
        .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    )
    val artifact = result.artifact.getOrElse(fail("producer did not succeed"))

    intercept[IllegalArgumentException] {
      artifact.copy(
        producerSearchContract = artifact.producerSearchContract.copy(
          widthBound = artifact.request.widthBound + 1
        )
      )
    }
    intercept[IllegalArgumentException] {
      artifact.copy(
        producerSearchContract = artifact.producerSearchContract.copy(
          requireConnectedCover = !artifact.request.requireConnectedCover
        )
      )
    }
  }

  test("one-pass SQL is byte-identical to every public exact-rank rewrite") {
    val rewriter = standardRewriter()
    val acquisitionRequest = PostgreSqlEnumerationBatchRequest
      .create(triangleSql, planLimit = 10)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    val batch = rewriter
      .produceEnumerationBatch(acquisitionRequest)
      .artifact
      .getOrElse(fail("one-pass producer did not produce an artifact"))
      .batch

    batch.candidates.foreach { candidate =>
      val detailed = rewriter.rewriteDetailed(
        RewriteRequest(
          triangleSql,
          options = RewriteOptions(
            widthBound = acquisitionRequest.widthBound,
            planLimit = acquisitionRequest.planLimit,
            requireConnectedCover = acquisitionRequest.requireConnectedCover,
            candidateSelection =
              CandidatePlanSelection.StructuralRank(candidate.plan.rank),
            reductionMode = YannakakisReductionMode.FullTwoPass
          )
        )
      )
      val exactRank = detailed.report.artifact.getOrElse(
        fail(s"exact-rank ${candidate.plan.rank} rewrite failed")
      )
      assert(detailed.outcome.isInstanceOf[RewriteOutcome.Rewritten])
      assertEquals(exactRank.selectedPlan, candidate.plan)
      assertEquals(exactRank.bagFactorPlan, candidate.bagFactorPlan)
      assertEquals(exactRank.renderedQuery.sql, candidate.renderedQuery.sql)
    }
  }

  test("producer retains typed non-produced states with no partial batch") {
    val rewriter = standardRewriter()
    val requests = Vector(
      PostgreSqlEnumerationBatchRequest.create("   "),
      PostgreSqlEnumerationBatchRequest.create(
        "SELECT r.a FROM triangle_r AS r"
      )
    ).map(_.fold(problems => fail(problems.map(_.code).mkString("; ")), identity))

    val results = requests.map(rewriter.produceEnumerationBatch)
    assertEquals(
      results.map(_.status),
      Vector(
        PostgreSqlEnumerationBatchStatus.InvalidQuery,
        PostgreSqlEnumerationBatchStatus.NotApplicable
      )
    )
    assert(results.forall(_.artifact.isEmpty))
    assert(results.forall(_.producerSearchContract != null))
    assert(
      results.forall(result =>
        result.producerSearchContract.widthBound == result.request.widthBound &&
          result.producerSearchContract.requireConnectedCover ==
            result.request.requireConnectedCover
      )
    )
    assert(results.forall(_.diagnostics.nonEmpty))
    assertEquals(
      results.map(_.pipelineEvidence.completedPhase),
      results.map(_.completedPhase)
    )
    assertEquals(results.head.pipelineEvidence.candidateDecision, None)
    assert(results(1).pipelineEvidence.analysis.nonEmpty)
  }

  test("failure transcript distinguishes exact search rejection from post-search compilation failure") {
    val rejectedRequest = PostgreSqlEnumerationBatchRequest
      .create(triangleSql, widthBound = 1, planLimit = 3)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    val rejected = standardRewriter().produceEnumerationBatch(rejectedRequest)

    assertEquals(rejected.status, PostgreSqlEnumerationBatchStatus.NotApplicable)
    assertEquals(
      rejected.completedPhase,
      PostgreSqlRewritePhase.CandidateSearchCompleted
    )
    rejected.pipelineEvidence.candidateDecision match {
      case Some(decision: CandidatePlanDecision.Rejected) =>
        assertEquals(decision.requestedLimit, 3)
        assert(decision.statistics.base.candidateBagCount > 0)
      case other => fail(s"expected exact rejected search evidence, received $other")
    }

    val restrictiveBagLimits = BagFactorPlanningLimits
      .create(maxOwnershipChecks = 1L)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val failed = standardRewriter(restrictiveBagLimits)
      .produceEnumerationBatch(
        PostgreSqlEnumerationBatchRequest
          .create(triangleSql, planLimit = 4)
          .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
      )

    assertEquals(failed.status, PostgreSqlEnumerationBatchStatus.Failed)
    assertEquals(
      failed.completedPhase,
      PostgreSqlRewritePhase.CandidateSearchCompleted
    )
    failed.pipelineEvidence.candidateDecision match {
      case Some(decision: CandidatePlanDecision.Found) =>
        assertEquals(decision.requestedLimit, 4)
        assertEquals(decision.plans.map(_.rank), Vector(1, 2, 3, 4))
      case other => fail(s"expected retained found search evidence, received $other")
    }
    assertEquals(failed.artifact, None)
  }

  test("request validation enforces the frozen primary K envelope") {
    val invalid = Vector(
      PostgreSqlEnumerationBatchRequest.create(triangleSql, widthBound = 0),
      PostgreSqlEnumerationBatchRequest.create(triangleSql, planLimit = 0),
      PostgreSqlEnumerationBatchRequest.create(triangleSql, planLimit = 11),
      PostgreSqlEnumerationBatchRequest.create(null)
    )
    assert(invalid.forall(_.isLeft))
  }

  test("request rejects lossy or oversized source SQL before planning and hashing") {
    val unpairedSurrogate = new String(Array('\udc00'))
    val invalidEncoding = Vector(
      PostgreSqlEnumerationBatchRequest.create("SELECT '\u0000'"),
      PostgreSqlEnumerationBatchRequest.create(unpairedSurrogate)
    )
    assert(
      invalidEncoding.forall(
        _.left.toOption.exists(
          _.contains(
            PostgreSqlEnumerationBatchRequest.ValidationError.InvalidSourceSqlEncoding
          )
        )
      )
    )

    val exact = "a" * PostgreSqlEnumerationBatchRequest.MaximumSourceSqlBytes
    assert(PostgreSqlEnumerationBatchRequest.create(exact).isRight)
    val oversized = PostgreSqlEnumerationBatchRequest.create(exact + "a")
    assert(
      oversized.left.toOption.exists(_.exists {
        case PostgreSqlEnumerationBatchRequest.ValidationError.SourceSqlTooLarge(
              byteLength
            ) =>
          byteLength ==
            PostgreSqlEnumerationBatchRequest.MaximumSourceSqlBytes + 1
        case _ => false
      })
    )
  }

  test("request copy cannot bypass the exact source SQL transport boundary") {
    val request = PostgreSqlEnumerationBatchRequest
      .create(triangleSql)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)

    intercept[IllegalArgumentException] {
      request.copy(originalSql = "SELECT '\u0000'")
    }
    intercept[IllegalArgumentException] {
      request.copy(originalSql = new String(Array('\ud800')))
    }
  }

  test("shared SQL transport check rejects malformed generated or stage bytes") {
    assertEquals(
      PostgreSqlEnumerationSqlTransport.validate(
        "SELECT 'é'",
        maximumBytes = 11,
        allowEmpty = false
      ),
      Right(11L)
    )
    assertEquals(
      PostgreSqlEnumerationSqlTransport.validate(
        "SELECT 'é'",
        maximumBytes = 10,
        allowEmpty = false
      ),
      Left(PostgreSqlEnumerationSqlTransportFailure.TooLarge(11))
    )
    assertEquals(
      PostgreSqlEnumerationSqlTransport.validate(
        "",
        maximumBytes = 1,
        allowEmpty = false
      ),
      Left(PostgreSqlEnumerationSqlTransportFailure.Empty)
    )
    assertEquals(
      PostgreSqlEnumerationSqlTransport.validate(
        "x\u0000y",
        maximumBytes = 3,
        allowEmpty = false
      ),
      Left(PostgreSqlEnumerationSqlTransportFailure.ContainsNul)
    )
    assertEquals(
      PostgreSqlEnumerationSqlTransport.validate(
        new String(Array('\ud800')),
        maximumBytes = 4,
        allowEmpty = false
      ),
      Left(PostgreSqlEnumerationSqlTransportFailure.MalformedUtf16)
    )
    assertEquals(
      PostgreSqlEnumerationSqlTransport.validate(
        "Aé€😀",
        maximumBytes = 10,
        allowEmpty = false
      ),
      Right(10L)
    )
  }

  private def standardRewriter(
      bagFactorLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default
  ): PostgreSqlQueryRewriter = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    PostgreSqlQueryRewriter
      .create(planner, bagFactorLimits = bagFactorLimits)
      .fold(problem => fail(problem.message), identity)
  }
}
