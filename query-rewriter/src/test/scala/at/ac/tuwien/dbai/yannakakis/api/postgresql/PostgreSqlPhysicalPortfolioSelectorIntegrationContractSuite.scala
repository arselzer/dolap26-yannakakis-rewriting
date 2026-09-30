package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.util.Locale

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateResourceLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlRenderingLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  DiagnosticSeverity,
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
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.tools.Frameworks

/** End-to-end and hostile-copy checks for the isolated physical selector. */
class PostgreSqlPhysicalPortfolioSelectorIntegrationContractSuite
    extends munit.FunSuite {
  import PostgreSqlFactorAwareContractTestSupport.{q5, triangle, triangleSql}
  import PostgreSqlPhysicalPortfolioContractTestSupport.{estimate, provenance}

  private val policy =
    CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1

  test("selected portfolio retains a replayable exhausted attempt and exact materialized artifact") {
    val evaluator = new RecordingPortfolioEvaluator((request, _) =>
      Right(successfulBatch(request, terminalRows = _ => BigDecimal(100)))
    )
    val result = rewrite(triangleSql, Some(evaluator))

    assert(result.outcome.isInstanceOf[RewriteOutcome.Rewritten])
    assertEquals(evaluator.physicalCalls, 1)
    assertEquals(evaluator.legacyCalls, 0)
    val attempt = result.report.physicalPortfolioAttempt.getOrElse(
      fail("selected portfolio did not retain its common attempt")
    )
    val artifact = result.report.physicalPortfolioArtifact.getOrElse(
      fail("selected portfolio did not retain its success artifact")
    )
    val selected = attempt.decision match {
      case value: PostgreSqlPhysicalPortfolioDecision.Selected => value
      case other => fail(s"expected selected decision, received $other")
    }
    val selectedEntry = selected.enumeration.entries
      .find(
        _.variant.physicalVariantId == selected.selectedPhysicalVariantId
      )
      .getOrElse(fail("selected physical variant was not enumerated"))

    assert(attempt.structuralSearchExhaustedBeforeLimit)
    assertEquals(attempt.candidateSearch.requestedLimit, 100)
    assert(attempt.candidateSearch.plans.size < 100)
    assert(PostgreSqlPhysicalPortfolioSelectionReplay.verifyAttempt(attempt))
    assertEquals(artifact.attempt, attempt)
    assertEquals(
      selectedEntry.variant.materialization,
      PostgreSqlCteMaterializationDirective.Materialized
    )
    assertEquals(
      artifact.configuredPlan.configuration,
      selectedEntry.variant.configuration
    )
    assertEquals(
      result.outcome.asInstanceOf[RewriteOutcome.Rewritten].rewrittenStatements,
      Vector(artifact.renderedQuery.sql)
    )
  }

  test("no-admissible portfolio retains complete evidence without a success artifact") {
    val evaluator = new RecordingPortfolioEvaluator((request, _) =>
      Right(successfulBatch(request, terminalRows = _ => BigDecimal(1001)))
    )
    val result = rewrite(triangleSql, Some(evaluator))

    assert(result.outcome.isInstanceOf[RewriteOutcome.Failed])
    assertEquals(evaluator.physicalCalls, 1)
    assertEquals(result.report.physicalPortfolioArtifact, None)
    assertEquals(
      result.report.completedPhase,
      PostgreSqlRewritePhase.CandidateSelectionCompleted
    )
    val attempt = result.report.physicalPortfolioAttempt.getOrElse(
      fail("no-admissible portfolio did not retain its common attempt")
    )
    val decision = attempt.decision match {
      case value: PostgreSqlPhysicalPortfolioDecision.NoAdmissible => value
      case other => fail(s"expected no-admissible decision, received $other")
    }
    assert(decision.evaluations.nonEmpty)
    assert(decision.evaluations.forall(!_.rowGuard.withinGuard))
    assert(PostgreSqlPhysicalPortfolioSelectionReplay.verifyAttempt(attempt))
  }

  test("absent and preflight-rejecting evaluators fail closed with exact zero prefixes") {
    val absent = rewrite(triangleSql, None)
    val absentFailure = failedDecision(absent)
    assertEquals(
      absentFailure.explainFailure.map(_.code),
      Some(PostgreSqlPhysicalPortfolioExplainFailureCode.EvaluatorUnavailable)
    )
    assertEquals(absentFailure.completedEvaluations, Vector.empty)
    assertEquals(absent.report.physicalPortfolioArtifact, None)

    val unsupported = new RecordingPortfolioEvaluator((request, _) =>
      Left(
        PostgreSqlPhysicalPortfolioExplainFailure(
          PostgreSqlPhysicalPortfolioExplainFailureCode.UnsupportedServerVersion,
          PostgreSqlPhysicalPortfolioExplainStage.VersionPreflight,
          None,
          None,
          Vector.empty,
          request.flattened.size,
          Some(110000),
          Some(
            PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum
          )
        )
      )
    )
    val preflight = rewrite(triangleSql, Some(unsupported))
    val preflightFailure = failedDecision(preflight)
    val explainFailure = preflightFailure.explainFailure.getOrElse(
      fail("unsupported server version was not retained")
    )
    assertEquals(unsupported.physicalCalls, 1)
    assertEquals(explainFailure.source, None)
    assertEquals(explainFailure.completed, Vector.empty)
    assertEquals(explainFailure.failedStatementIndex, None)
    assert(
      PostgreSqlPhysicalPortfolioSelectionReplay.verifyAttempt(
        preflight.report.physicalPortfolioAttempt.get
      )
    )
  }

  test("exact K=100 Q5 portfolio reaches evaluation without claiming structural exhaustion") {
    assertEquals(q5.found.requestedLimit, 100)
    assertEquals(q5.found.plans.size, 100)
    val evaluator = new RecordingPortfolioEvaluator((request, _) =>
      Left(
        PostgreSqlPhysicalPortfolioExplainFailure(
          PostgreSqlPhysicalPortfolioExplainFailureCode.UnsupportedServerVersion,
          PostgreSqlPhysicalPortfolioExplainStage.VersionPreflight,
          None,
          None,
          Vector.empty,
          request.flattened.size,
          Some(110000),
          Some(
            PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum
          )
        )
      )
    )
    val selected = PostgreSqlPhysicalPortfolioSelector.select(
      q5.query,
      q5.input,
      q5.found,
      q5.sql,
      Some(evaluator),
      CandidateResourceLimits.default,
      PostgreSqlPhysicalPortfolioLimits.default,
      PostgreSqlPhysicalPortfolioEvaluationLimits.default,
      BagFactorPlanningLimits.default,
      PostgreSqlRenderingLimits.default
    )
    val failure = selected.left.toOption
      .collect { case value: PostgreSqlPhysicalPortfolioDecision.Failed => value }
      .getOrElse(fail(s"expected typed evaluator failure, received $selected"))

    assertEquals(evaluator.physicalCalls, 1)
    assertEquals(failure.failureKind,
      PostgreSqlPhysicalPortfolioSelectionFailureKind.EvaluatorFailed)
    assertEquals(failure.enumeration.get.candidates.size, 100)
    assertEquals(
      failure.rawRenderPrefix.size,
      failure.enumeration.get.entries.size
    )
    assertEquals(
      failure.explainRequest.map(_.workloads.size),
      failure.deduplication.map(_.uniqueWorkloadCount)
    )
  }

  test("global COUNT is rejected before enumeration and evaluator at both public and direct boundaries") {
    val evaluator = new RecordingPortfolioEvaluator((request, _) =>
      Right(successfulBatch(request, terminalRows = _ => BigDecimal(100)))
    )
    val countSql =
      "SELECT COUNT(*) FROM triangle_r AS r " +
        "JOIN triangle_s AS s ON r.b = s.b " +
        "JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a"
    val publicResult = rewriteCertifiedCount(countSql, Some(evaluator))

    assert(
      publicResult.outcome.isInstanceOf[RewriteOutcome.NotApplicable],
      clues(publicResult.outcome)
    )
    assertEquals(
      publicResult.outcome.diagnostics.map(_.code),
      Vector(RewriteReasonCode.UnsupportedQueryShape)
    )
    assertEquals(
      publicResult.outcome.diagnostics.map(_.severity),
      Vector(DiagnosticSeverity.Warning)
    )
    assertEquals(
      publicResult.outcome.diagnostics.head.context,
      Map(
        "selectionPolicy" -> policy.id,
        "resultShape" -> "global-count-star",
        "reason" -> "aggregate-root-row-guard-degenerate"
      )
    )
    assertEquals(
      publicResult.report.completedPhase,
      PostgreSqlRewritePhase.QueryAnalyzed
    )
    assert(publicResult.report.analysis.exists(_.isSupported))
    assertEquals(publicResult.report.candidateDecision, None)
    assertEquals(publicResult.report.physicalPortfolioAttempt, None)
    assertEquals(evaluator.physicalCalls, 0)

    val countQuery = publicResult.report.analysis
      .map(_.query)
      .getOrElse(fail("COUNT query was not analyzed"))
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalPortfolioSelector.select(
        countQuery,
        triangle.input,
        triangle.found,
        countSql,
        Some(evaluator),
        CandidateResourceLimits.default,
        PostgreSqlPhysicalPortfolioLimits.default,
        PostgreSqlPhysicalPortfolioEvaluationLimits.default,
        BagFactorPlanningLimits.default,
        PostgreSqlRenderingLimits.default
      )
    }
    assertEquals(evaluator.physicalCalls, 0)
  }

  test("direct selector rejects a non-K100 Found payload before enumeration and evaluation") {
    val evaluator = new RecordingPortfolioEvaluator((request, _) =>
      Right(successfulBatch(request, terminalRows = _ => BigDecimal(100)))
    )
    val nonContractSearch = triangle.found.copy(requestedLimit = 99)

    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalPortfolioSelector.select(
        triangle.query,
        triangle.input,
        nonContractSearch,
        triangle.sql,
        Some(evaluator),
        CandidateResourceLimits.default,
        PostgreSqlPhysicalPortfolioLimits.default,
        PostgreSqlPhysicalPortfolioEvaluationLimits.default,
        BagFactorPlanningLimits.default,
        PostgreSqlRenderingLimits.default
      )
    }
    assertEquals(evaluator.physicalCalls, 0)
    assertEquals(evaluator.legacyCalls, 0)
  }

  test("attempt replay rejects QueryIR-hypergraph drift and same-code enumeration payload drift") {
    val limits = PostgreSqlPhysicalPortfolioLimits
      .create(maxRootsPerCandidate = 1)
      .fold(problems => fail(problems.mkString("; ")), identity)
    val selected = PostgreSqlPhysicalPortfolioSelector.select(
      triangle.query,
      triangle.input,
      triangle.found,
      triangle.sql,
      None,
      CandidateResourceLimits.default,
      limits,
      PostgreSqlPhysicalPortfolioEvaluationLimits.default,
      BagFactorPlanningLimits.default,
      PostgreSqlRenderingLimits.default
    )
    val decision = selected.left.toOption
      .collect { case value: PostgreSqlPhysicalPortfolioDecision.Failed => value }
      .getOrElse(fail(s"expected bounded enumeration failure, received $selected"))
    assertEquals(
      decision.failureKind,
      PostgreSqlPhysicalPortfolioSelectionFailureKind.EnumerationFailed
    )
    val generated = SoftBagGenerator
      .generate(triangle.input.hypergraph, triangle.input.widthBound)
      .fold(problem => fail(problem.message), identity)
    val admissible = generated.admissible(triangle.input.requireConnectedCover)
    val attempt = PostgreSqlPhysicalPortfolioAttempt(
      triangle.sql,
      triangle.query,
      triangle.input.hypergraph,
      generated,
      admissible,
      triangle.input,
      triangle.found,
      CandidateResourceLimits.default,
      limits,
      PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default,
      PostgreSqlPhysicalPortfolioEvaluationLimits.default,
      triangle.found.plans.size < triangle.found.requestedLimit,
      decision
    )

    intercept[IllegalArgumentException] {
      attempt.copy(query = q5.query)
    }

    val originalFailure = decision.enumerationFailure.getOrElse(
      fail("enumeration failure payload was not retained")
    )
    val mutatedFailure = originalFailure match {
      case value: PostgreSqlPhysicalPortfolioEnumerationFailure.ResourceLimitExceeded =>
        value.copy(required = value.required + 1)
      case other => fail(s"expected resource enumeration failure, received $other")
    }
    val mutatedDecision = decision.copy(
      enumerationFailure = Some(mutatedFailure)
    )
    assertEquals(mutatedDecision.detailCode, decision.detailCode)
    intercept[IllegalArgumentException] {
      attempt.copy(decision = mutatedDecision)
    }
  }

  private def rewrite(
      sql: String,
      evaluator: Option[PostgreSqlExplainCostEvaluator]
  ): PostgreSqlRewriteResult = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(statement => Right(fixture.plan(statement).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = evaluator)
      .fold(problem => fail(problem.message), identity)
    rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = 100,
          candidateSelection = policy,
          reductionMode = YannakakisReductionMode.FullTwoPass
        )
      )
    )
  }

  /** Uses the framework-backed boundary so exact COUNT(*) source evidence is
    * certified instead of being intentionally downgraded to Uncertified by
    * the generic function-backed fixture used by the other portfolio tests.
    */
  private def rewriteCertifiedCount(
      sql: String,
      evaluator: Option[PostgreSqlExplainCostEvaluator]
  ): PostgreSqlRewriteResult = {
    val schema = Frameworks.createRootSchema(true)
    CalciteTestSchemas.triangle.foreach { tableDefinition =>
      schema.add(
        tableDefinition.name.toLowerCase(Locale.ROOT),
        new AbstractTable {
          override def getRowType(
              factory: RelDataTypeFactory
          ): RelDataType = {
            val builder = factory.builder()
            tableDefinition.columns.foreach { column =>
              builder.add(
                column.name.toLowerCase(Locale.ROOT),
                factory.createTypeWithNullability(
                  factory.createSqlType(column.sqlType),
                  column.nullable
                )
              )
            }
            builder.build()
          }
        }
      )
    }
    val planner = CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = evaluator)
      .fold(problem => fail(problem.message), identity)
    rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = 100,
          candidateSelection = policy,
          reductionMode = YannakakisReductionMode.FullTwoPass
        )
      )
    )
  }

  private def failedDecision(
      result: PostgreSqlRewriteResult
  ): PostgreSqlPhysicalPortfolioDecision.Failed =
    result.report.physicalPortfolioAttempt
      .map(_.decision)
      .collect { case value: PostgreSqlPhysicalPortfolioDecision.Failed => value }
      .getOrElse(fail("result did not retain a failed portfolio attempt"))

  private def successfulBatch(
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      terminalRows: PostgreSqlCteMaterializationDirective => BigDecimal
  ): PostgreSqlPhysicalPortfolioExplainBatch = {
    val source = PostgreSqlExplainSourceEvaluation(
      request.source.sourceStatementSha256,
      estimate(totalCost = BigDecimal(10), rows = BigDecimal(100))
    )
    val observations = request.flattened.zipWithIndex.map {
      case ((workload, statement), index) =>
        val directive = workload.group.descriptor.materialization
        val preferredRows = directive match {
          case PostgreSqlCteMaterializationDirective.Materialized => BigDecimal(1)
          case PostgreSqlCteMaterializationDirective.NotMaterialized => BigDecimal(2)
          case PostgreSqlCteMaterializationDirective.PlannerControlled => BigDecimal(3)
        }
        val rows =
          if (
            statement.stageId ==
              at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId.FinalReconstruction
          ) terminalRows(directive)
          else preferredRows
        PostgreSqlPhysicalPortfolioExplainObservation(
          workload.group.descriptor.workloadId,
          workload.group.representative.physicalVariantId,
          statement.stageId,
          statement.statementSha256,
          estimate(
            totalCost = preferredRows + BigDecimal(index) / BigDecimal(1000),
            rows = rows,
            width = 1L
          )
        )
    }
    PostgreSqlPhysicalPortfolioExplainBatch(
      provenance,
      source,
      observations
    )
  }

  private final class RecordingPortfolioEvaluator(
      response: (
          PostgreSqlPhysicalPortfolioExplainBatchRequest,
          PostgreSqlPhysicalPortfolioEvaluationLimits
      ) => Either[
        PostgreSqlPhysicalPortfolioExplainFailure,
        PostgreSqlPhysicalPortfolioExplainBatch
      ]
  ) extends PostgreSqlExplainCostEvaluator {
    var legacyCalls: Int = 0
    var physicalCalls: Int = 0

    override def evaluate(
        request: PostgreSqlExplainCostBatchRequest,
        limits: PostgreSqlExplainSelectionLimits
    ): Either[
      PostgreSqlExplainCostEvaluationFailure,
      PostgreSqlExplainCostBatch
    ] = {
      legacyCalls += 1
      Left(
        PostgreSqlExplainCostEvaluationFailure(
          PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
          PostgreSqlExplainEvaluationStage.Session,
          None,
          None,
          Vector.empty
        )
      )
    }

    override def evaluatePhysicalPortfolio(
        request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
        limits: PostgreSqlPhysicalPortfolioEvaluationLimits
    ): Either[
      PostgreSqlPhysicalPortfolioExplainFailure,
      PostgreSqlPhysicalPortfolioExplainBatch
    ] = {
      physicalCalls += 1
      response(request, limits)
    }
  }
}
