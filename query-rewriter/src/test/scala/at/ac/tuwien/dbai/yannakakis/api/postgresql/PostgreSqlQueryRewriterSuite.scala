package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidateResourceLimits
}
import at.ac.tuwien.dbai.yannakakis.api.{
  DiagnosticSeverity,
  RewriteOptions,
  RewriteOutcome,
  RewriteReasonCode,
  RewriteRequest
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlRenderingLimits
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas,
  TestColumn,
  TestTable
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteSqlPlanner,
  CalciteSqlPlanningError,
  CalciteSqlPlanningStage
}
import org.apache.calcite.rel.RelRoot
import org.apache.calcite.sql.`type`.SqlTypeName

class PostgreSqlQueryRewriterSuite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("end-to-end rewrite preserves SQL and exposes a complete rank-one artifact") {
    val originalSql = s"  /* exact input */\n$triangleSql \n"
    val rewriter = rewriterFor(CalciteTestSchemas.standard.tables)
    val request = RewriteRequest(
      originalSql,
      options = RewriteOptions(widthBound = 2, planLimit = 4)
    )

    val first = rewriter.rewriteDetailed(request)
    val second = rewriter.rewriteDetailed(request)

    assertEquals(first.outcome.originalSql, originalSql)
    val rewritten = first.outcome match {
      case value: RewriteOutcome.Rewritten => value
      case other => fail(s"expected a rewrite, received $other")
    }
    assertEquals(rewritten.rewrittenStatements.size, 1)
    assert(rewritten.rewrittenStatements.head.startsWith("WITH"))
    assertEquals(first.report.completedPhase, PostgreSqlRewritePhase.Rendered)

    val artifact = first.report.artifact.getOrElse(fail("missing artifact"))
    val found = first.report.candidateDecision match {
      case Some(value: CandidatePlanDecision.Found) => value
      case other => fail(s"missing found decision: $other")
    }
    assertEquals(artifact.candidateSearch, found)
    assertEquals(found.requestedLimit, 4)
    assert(found.plans.nonEmpty)
    assertEquals(artifact.selectedPlan, found.plans.head)
    assertEquals(artifact.selectedPlan.rank, 1)
    assertEquals(artifact.renderedQuery.sql, rewritten.rewrittenStatements.head)
    assertEquals(artifact.renderedQuery.sourceQuery, artifact.query)
    assertEquals(artifact.candidateInput.hypergraph, artifact.hypergraph)
    assertEquals(artifact.bagFactorPlan.decomposition, artifact.selectedPlan.decomposition)
    assertEquals(first.report.bagFactorPlan, Some(artifact.bagFactorPlan))

    val secondArtifact = second.report.artifact.getOrElse(fail("missing artifact"))
    assertEquals(second.outcome, first.outcome)
    assertEquals(secondArtifact.candidateSearch, artifact.candidateSearch)
    assertEquals(secondArtifact.selectedPlan, artifact.selectedPlan)
  }

  test("ordinary rewrite and detailed rewrite return the same exact SQL") {
    val rewriter = rewriterFor(CalciteTestSchemas.standard.tables)
    val request = RewriteRequest(triangleSql)

    assertEquals(
      rewriter.rewrite(request),
      rewriter.rewriteDetailed(request).outcome
    )
  }

  test("blank SQL is InvalidQuery without consulting Calcite") {
    var invoked = false
    val planner = plannerFromFunction { _ =>
      invoked = true
      fail("planner must not be invoked for blank SQL")
    }
    val originalSql = " \n\t "
    val detailed = rewriterFromPlanner(planner).rewriteDetailed(
      RewriteRequest(originalSql)
    )

    assert(!invoked)
    assert(detailed.outcome.isInstanceOf[RewriteOutcome.InvalidQuery])
    assertEquals(detailed.outcome.originalSql, originalSql)
    assertEquals(
      detailed.outcome.diagnostics.map(_.code),
      Vector(RewriteReasonCode.InvalidSql)
    )
    assertEquals(
      detailed.report.completedPhase,
      PostgreSqlRewritePhase.RequestAccepted
    )
  }

  test("unsupported aggregate is NotApplicable and retains analyzer evidence") {
    val sql =
      "SELECT department_id, COUNT(*) AS n FROM fact_sale GROUP BY department_id"
    val detailed = rewriterFor(CalciteTestSchemas.standard.tables).rewriteDetailed(
      RewriteRequest(sql)
    )

    assert(detailed.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
    assert(
      detailed.outcome.diagnostics.exists(
        _.code == RewriteReasonCode.UnsupportedAggregate
      )
    )
    assertEquals(
      detailed.report.completedPhase,
      PostgreSqlRewritePhase.QueryAnalyzed
    )
    assert(detailed.report.analysis.nonEmpty)
    assertEquals(detailed.report.artifact, None)
  }

  test("a relation without an inter-occurrence equality is NotApplicable") {
    val sql = "SELECT amount FROM fact_sale"
    val detailed = rewriterFor(CalciteTestSchemas.standard.tables).rewriteDetailed(
      RewriteRequest(sql)
    )

    assert(detailed.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
    assertEquals(
      detailed.outcome.diagnostics.map(_.code),
      Vector(RewriteReasonCode.UnsupportedQueryShape)
    )
    assertEquals(
      detailed.report.completedPhase,
      PostgreSqlRewritePhase.HypergraphBuilt
    )
    assert(detailed.report.hypergraph.exists(_.variables.isEmpty))
  }

  test("width-one triangle is a typed rejected CandidateTD with exact statistics") {
    val request = RewriteRequest(
      triangleSql,
      options = RewriteOptions(widthBound = 1, planLimit = 3)
    )
    val detailed = rewriterFor(CalciteTestSchemas.standard.tables)
      .rewriteDetailed(request)

    assert(detailed.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
    assertEquals(
      detailed.outcome.diagnostics.map(_.code),
      Vector(RewriteReasonCode.NoDecomposition)
    )
    assertEquals(
      detailed.report.completedPhase,
      PostgreSqlRewritePhase.CandidateSearchCompleted
    )
    detailed.report.candidateDecision match {
      case Some(rejected: CandidatePlanDecision.Rejected) =>
        assertEquals(rejected.requestedLimit, 3)
        assert(rejected.statistics.base.candidateBagCount > 0)
      case other => fail(s"expected rejected decision, received $other")
    }
    assertEquals(detailed.report.artifact, None)
  }

  test("candidate resource exhaustion maps to PlanningFailed without truncation") {
    val limits = CandidateResourceLimits
      .create(maxPlanLimit = 1)
      .fold(errors => fail(errors.mkString("; ")), identity)
    val planner = plannerFor(CalciteTestSchemas.standard.tables)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner, candidateLimits = limits)
      .fold(problem => fail(problem.message), identity)
    val detailed = rewriter.rewriteDetailed(
      RewriteRequest(
        triangleSql,
        options = RewriteOptions(widthBound = 2, planLimit = 2)
      )
    )

    assert(detailed.outcome.isInstanceOf[RewriteOutcome.Failed])
    assertEquals(
      detailed.outcome.diagnostics.map(_.code),
      Vector(RewriteReasonCode.PlanningFailed)
    )
    assert(
      detailed.outcome.diagnostics.head.context("algorithmCode").contains(
        "resource-limit.exceeded"
      )
    )
    assertEquals(
      detailed.report.completedPhase,
      PostgreSqlRewritePhase.CandidateInputBuilt
    )
    assertEquals(detailed.report.candidateDecision, None)
  }

  test("unsupported structural key type is NotApplicable") {
    val tables = Vector(
      TestTable(
        "V_R",
        Vector(
          TestColumn("K", SqlTypeName.VARCHAR),
          TestColumn("PAYLOAD", SqlTypeName.INTEGER)
        )
      ),
      TestTable("V_S", Vector(TestColumn("K", SqlTypeName.VARCHAR)))
    )
    val sql = "SELECT r.payload FROM v_r r JOIN v_s s ON r.k = s.k"
    val detailed = rewriterFor(tables).rewriteDetailed(RewriteRequest(sql))

    assert(detailed.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
    assertEquals(
      detailed.outcome.diagnostics.map(_.code),
      Vector(RewriteReasonCode.UnsupportedQueryShape)
    )
    assertEquals(
      detailed.outcome.diagnostics.map(_.severity),
      Vector(DiagnosticSeverity.Warning)
    )
    assert(
      detailed.outcome.diagnostics.head.context("algorithmCode").contains(
        "type.unsupported"
      )
    )
    assertEquals(
      detailed.report.completedPhase,
      PostgreSqlRewritePhase.BagFactorPlanned
    )
    assert(detailed.report.bagFactorPlan.nonEmpty)
    assertEquals(detailed.report.artifact, None)
  }

  test("renderer resource exhaustion remains a typed failure") {
    val limits = PostgreSqlRenderingLimits
      .create(maxSqlCharacters = 1L)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(
        plannerFor(CalciteTestSchemas.standard.tables),
        renderingLimits = limits
      )
      .fold(problem => fail(problem.message), identity)
    val detailed = rewriter.rewriteDetailed(RewriteRequest(triangleSql))

    assert(detailed.outcome.isInstanceOf[RewriteOutcome.Failed])
    assertEquals(
      detailed.outcome.diagnostics.map(_.code),
      Vector(RewriteReasonCode.RenderingFailed)
    )
    assertEquals(
      detailed.outcome.diagnostics.map(_.severity),
      Vector(DiagnosticSeverity.Error)
    )
    assert(
      detailed.outcome.diagnostics.head.context("algorithmCode").contains(
        "resource.exceeded"
      )
    )
    assertEquals(
      detailed.report.completedPhase,
      PostgreSqlRewritePhase.BagFactorPlanned
    )
    assert(detailed.report.bagFactorPlan.nonEmpty)
    assertEquals(detailed.report.artifact, None)
  }

  test("injected planner throw and malformed null results map to typed failures") {
    val secret = "jdbc:postgresql://user:pass@host/db?password=hunter2"
    val planners = Vector[
      (String, String => Either[CalciteSqlPlanningError, RelRoot])
    ](
      "throw" -> (_ => throw new IllegalStateException(secret)),
      "null-either" -> (_ => null),
      "null-left" -> (_ => Left(null)),
      "null-right" -> (_ => Right(null))
    )

    planners.foreach { case (label, function) =>
      val detailed = rewriterFromPlanner(plannerFromFunction(function))
        .rewriteDetailed(RewriteRequest("SELECT 1"))
      assert(
        detailed.outcome.isInstanceOf[RewriteOutcome.Failed],
        clues(label, detailed.outcome)
      )
      assertEquals(
        detailed.outcome.diagnostics.map(_.code),
        Vector(RewriteReasonCode.PlanningFailed),
        label
      )
      assertNoSecret(detailed.outcome, secret, label)
      assertEquals(
        detailed.report.completedPhase,
        PostgreSqlRewritePhase.RequestAccepted,
        label
      )
    }
  }

  test("raw secret text is redacted for every injected Calcite planning stage") {
    val secrets = Vector(
      "jdbc:postgresql://user:pass@host/db?password=hunter2",
      "password=correct horse battery staple",
      "SELECT * FROM private_table WHERE token = 'classified'"
    )

    CalciteSqlPlanningStage.values.zipWithIndex.foreach { case (stage, index) =>
      val secret = secrets(index % secrets.size)
      val planner = plannerFromFunction { _ =>
        Left(
          CalciteSqlPlanningError(
            stage,
            secret,
            Some(if (index % 2 == 0) "example.DriverException" else secret)
          )
        )
      }
      val detailed = rewriterFromPlanner(planner).rewriteDetailed(
        RewriteRequest("SELECT 1")
      )

      assertNoSecret(detailed.outcome, secret, stage.id)
      val expectedClass =
        if (stage.invalidatesSql) classOf[RewriteOutcome.InvalidQuery]
        else classOf[RewriteOutcome.Failed]
      assert(
        expectedClass.isInstance(detailed.outcome),
        clues(stage.id, detailed.outcome)
      )
    }
  }

  test("configuration factories reject nulls without throwing") {
    PostgreSqlQueryRewriter.create(null) match {
      case Left(problem: PostgreSqlRewriterConfigurationError.MissingValues) =>
        assert(problem.fields.contains("Calcite SQL planner"))
      case other => fail(s"unexpected construction result: $other")
    }

    PostgreSqlQueryRewriter.forSchema(null) match {
      case Left(
            problem: PostgreSqlRewriterConfigurationError.CalciteConfigurationFailure
          ) =>
        assertEquals(problem.stage, CalciteSqlPlanningStage.Configuration)
        assertEquals(problem.exceptionClass, None)
      case other => fail(s"unexpected schema construction result: $other")
    }

    PostgreSqlQueryRewriter.forPostgreSqlJdbc(
      null,
      null,
      null,
      null
    ) match {
      case Left(
            problem: PostgreSqlRewriterConfigurationError.CalciteConfigurationFailure
          ) =>
        assertEquals(problem.stage, CalciteSqlPlanningStage.Configuration)
        assert(!problem.message.toLowerCase.contains("password="))
      case other => fail(s"unexpected JDBC construction result: $other")
    }
  }

  private def rewriterFor(tables: Iterable[TestTable]): PostgreSqlQueryRewriter =
    rewriterFromPlanner(plannerFor(tables))

  private def plannerFor(tables: Iterable[TestTable]): CalciteSqlPlanner = {
    val fixture = CalcitePlannerFixture(tables)
    plannerFromFunction(sql => Right(fixture.plan(sql).root))
  }

  private def plannerFromFunction(
      function: String => Either[CalciteSqlPlanningError, RelRoot]
  ): CalciteSqlPlanner =
    CalciteSqlPlanner
      .fromFunction(function)
      .fold(problem => fail(problem.message), identity)

  private def rewriterFromPlanner(
      planner: CalciteSqlPlanner
  ): PostgreSqlQueryRewriter =
    PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)

  private def assertNoSecret(
      outcome: RewriteOutcome,
      secret: String,
      clue: String
  ): Unit = {
    val publicText = outcome.diagnostics.iterator
      .flatMap(diagnostic =>
        Iterator(diagnostic.message) ++
          diagnostic.context.iterator.flatMap { case (key, value) =>
            Iterator(key, value)
          }
      )
      .mkString("\n")
    assert(!publicText.contains(secret), clues(clue, publicText))
  }
}
