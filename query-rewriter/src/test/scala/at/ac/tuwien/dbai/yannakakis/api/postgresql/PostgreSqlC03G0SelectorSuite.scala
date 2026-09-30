package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteReasonCode,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  C03G0CatalogBinding,
  CalciteSqlPlanner
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.{AbstractSchema, AbstractTable}
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlC03G0SelectorSuite extends munit.FunSuite {
  private val schemaName = "c03_selector"
  private val binding = C03G0CatalogBinding(
    databaseName = "c03_selector_database",
    databaseOid = 10L,
    physicalSchemaName = schemaName,
    relationName = "synthetic_triangle_e0",
    groupColumnName = "payload",
    measureColumnName = "row_id",
    serverMajorVersion = 18,
    namespaceOid = 11L,
    relationOid = 12L,
    groupAttnum = 4,
    groupTypeOid = 25L,
    measureAttnum = 1,
    measureTypeOid = 20L,
    groupCollationOid = 13L,
    groupCollationName = "C",
    groupCollationProvider = "c",
    groupCollationDeterministic = true
  )
  private val source = Files.readString(sourcePath, StandardCharsets.UTF_8)
  private val evaluator = new ExplainSpy
  private val rewriter = {
    val root = Frameworks.createRootSchema(true)
    val schema = root.add(schemaName, new AbstractSchema {})
    Vector(
      "synthetic_triangle_e0",
      "synthetic_triangle_e1",
      "synthetic_triangle_e2"
    ).foreach { tableName =>
      schema.add(
        tableName,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("row_id", SqlTypeName.BIGINT)
              .nullable(false)
              .add("left_key", SqlTypeName.BIGINT)
              .nullable(true)
              .add("right_key", SqlTypeName.BIGINT)
              .nullable(true)
              .add("payload", SqlTypeName.VARCHAR)
              .nullable(true)
              .build()
        }
      )
    }
    val planner = CalciteSqlPlanner
      .forSchemaWithC03G0V2(
        schema,
        binding,
        CalciteSqlPlanner.PostgreSqlParserConfig
      )
      .fold(problem => fail(problem.toString), identity)
    PostgreSqlQueryRewriter
      .create(planner, explainCostEvaluator = Some(evaluator))
      .fold(problem => fail(problem.toString), identity)
  }

  test("closed structural, exact, and static selector set rewrites C03") {
    val head = rewritten(CandidatePlanSelection.StructuralHead)
    val plans = head.report.artifact
      .getOrElse(fail("C03 structural head has no artifact"))
      .candidateSearch
      .plans
    assert(plans.nonEmpty)

    val allowed =
      Vector(CandidatePlanSelection.PostgreSqlStaticShapeV1) ++
        plans.flatMap(plan =>
          Vector(
            CandidatePlanSelection.StructuralRank(plan.rank),
            CandidatePlanSelection.ExactCandidateId(plan.candidateId)
          )
        )
    allowed.foreach(rewritten)
    assertEquals(evaluator.calls, 0)
  }

  test("every optimizer-backed policy rejects C03 before evaluator access") {
    val optimizerPolicies = Vector(
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1,
      CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1,
      CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1,
      CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
    )
    optimizerPolicies.foreach { policy =>
      val result = rewrite(policy, YannakakisReductionMode.FullTwoPass)
      assert(result.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
      assertEquals(
        result.outcome.diagnostics.map(_.code),
        Vector(RewriteReasonCode.UnsupportedQueryShape),
        clues(policy.id)
      )
      assertEquals(
        result.outcome.diagnostics.head.context.get("reason"),
        Some(PostgreSqlCandidateSelector.C03G0SelectionPolicyUnsupportedReason),
        clues(policy.id)
      )
      assertEquals(
        result.report.completedPhase,
        PostgreSqlRewritePhase.QueryAnalyzed,
        clues(policy.id)
      )
      assertEquals(result.report.hypergraph, None, clues(policy.id))
      assertEquals(result.report.candidateDecision, None, clues(policy.id))
    }
    assertEquals(evaluator.calls, 0)
  }

  test("both partial reduction modes reject C03 before candidate search") {
    Vector(
      YannakakisReductionMode.BottomUpOnly,
      YannakakisReductionMode.NoReduction
    ).foreach { mode =>
      val result = rewrite(CandidatePlanSelection.StructuralHead, mode)
      assert(result.outcome.isInstanceOf[RewriteOutcome.NotApplicable])
      assertEquals(
        result.outcome.diagnostics.map(_.code),
        Vector(RewriteReasonCode.UnsupportedQueryShape),
        clues(mode.id)
      )
      assertEquals(
        result.outcome.diagnostics.head.context.get("reason"),
        Some(PostgreSqlCandidateSelector.C03G0RequiresFullTwoPassReason),
        clues(mode.id)
      )
      assertEquals(
        result.report.completedPhase,
        PostgreSqlRewritePhase.QueryAnalyzed,
        clues(mode.id)
      )
      assertEquals(result.report.hypergraph, None, clues(mode.id))
    }
    assertEquals(evaluator.calls, 0)
  }

  private def rewritten(policy: CandidatePlanSelection): PostgreSqlRewriteResult = {
    val result = rewrite(policy, YannakakisReductionMode.FullTwoPass)
    assert(
      result.outcome.isInstanceOf[RewriteOutcome.Rewritten],
      clues(policy.id, result.outcome)
    )
    result
  }

  private def rewrite(
      policy: CandidatePlanSelection,
      mode: YannakakisReductionMode
  ): PostgreSqlRewriteResult = {
    val planLimit = policy match {
      case CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 |
          CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 |
          CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 =>
        100
      case _ => 10
    }
    rewriter.rewriteDetailed(
      RewriteRequest(
        source,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = planLimit,
          requireConnectedCover = true,
          candidateSelection = policy,
          reductionMode = mode
        )
      )
    )
  }

  private final class ExplainSpy extends PostgreSqlExplainCostEvaluator {
    var calls: Int = 0

    override def evaluate(
        request: PostgreSqlExplainCostBatchRequest,
        limits: PostgreSqlExplainSelectionLimits
    ): Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch] = {
      calls += 1
      throw new AssertionError("C03 must reject before EXPLAIN evaluator access")
    }
  }

  private def sourcePath: Path = {
    val relative = Paths.get(
      "benchmarks",
      "cyclic-postgresql",
      "starter-corpus",
      "queries",
      "c03-triangle-guarded-multiagg.sql"
    )
    Vector(relative, Paths.get("..").resolve(relative))
      .map(_.toAbsolutePath.normalize())
      .find(Files.isRegularFile(_))
      .getOrElse(fail("could not locate frozen C03 source SQL"))
  }
}
