package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidatePlanId,
  CandidateSearchInput,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  TestColumn,
  TestTable
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteQueryAnalyzer,
  QueryIR
}
import org.apache.calcite.sql.`type`.SqlTypeName

/** Shared, data-free fixtures for independent factor-aware contract tests. */
private[yannakakis] object PostgreSqlFactorAwareContractTestSupport {
  final case class Prepared(
      sql: String,
      query: QueryIR,
      input: CandidateSearchInput,
      found: CandidatePlanDecision.Found
  )

  val provenance: PostgreSqlExplainCostProvenance =
    PostgreSqlExplainCostProvenance(
      serverVersionNum = 180004,
      plannerSettingsFingerprintSha256 = "d" * 64,
      plannerSettingsFingerprintScheme =
        PostgreSqlExplainCostProvenance.PlannerSettingsFingerprintScheme,
      plannerSettingCount =
        PostgreSqlExplainCostProvenance.PlannerSettingNames.size
    )

  final class RecordingFactorEvaluator(
      response: (
          PostgreSqlFactorAwareExplainBatchRequest,
          PostgreSqlFactorAwareSelectionLimits
      ) => Either[
        PostgreSqlFactorAwareExplainFailure,
        PostgreSqlFactorAwareExplainBatch
      ]
  ) extends PostgreSqlExplainCostEvaluator {
    var legacyCalls: Int = 0
    var factorCalls: Int = 0
    var factorRequests: Vector[PostgreSqlFactorAwareExplainBatchRequest] =
      Vector.empty

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

    override def evaluateFactorAware(
        request: PostgreSqlFactorAwareExplainBatchRequest,
        limits: PostgreSqlFactorAwareSelectionLimits
    ): Either[
      PostgreSqlFactorAwareExplainFailure,
      PostgreSqlFactorAwareExplainBatch
    ] = {
      factorCalls += 1
      factorRequests :+= request
      response(request, limits)
    }
  }

  lazy val q5: Prepared =
    prepare(q5Sql, q5Tables, widthBound = 2, connectedCover = true, limit = 100)

  lazy val q5PrefixReruns: Vector[(Int, CandidatePlanDecision)] =
    PostgreSqlFactorAwareProgressiveV1.Checkpoints.map { limit =>
      limit -> enumerate(q5.input, limit)
    }

  lazy val triangle: Prepared =
    prepare(
      triangleSql,
      triangleTables,
      widthBound = 2,
      connectedCover = true,
      limit = 100
    )

  lazy val trianglePrefixReruns: Vector[(Int, CandidatePlanDecision)] =
    PostgreSqlFactorAwareProgressiveV1.Checkpoints.map { limit =>
      limit -> enumerate(triangle.input, limit)
    }

  def factorRequest(count: Int): PostgreSqlFactorAwareExplainBatchRequest = {
    require(count > 0)
    val sourceSql = "SELECT 42 AS source_value"
    val source = PostgreSqlExplainSourceStatement(
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sourceSql),
      sourceSql
    )
    val candidates = (1 to count).toVector.map { rank =>
      val id = candidateId(rank)
      val mode = YannakakisReductionMode.NoReduction
      val sql = s"SELECT $rank AS factor_stage"
      val statement = PostgreSqlFactorAwareExplainStageStatement(
        at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId.FinalReconstruction,
        PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql),
        sql
      )
      PostgreSqlFactorAwareExplainCandidate(
        id,
        rank,
        mode,
        PostgreSqlPhysicalVariantId.from(id, mode),
        Vector(statement)
      )
    }
    PostgreSqlFactorAwareExplainBatchRequest(source, candidates)
  }

  def successfulBatch(
      request: PostgreSqlFactorAwareExplainBatchRequest,
      sourceRows: BigDecimal = BigDecimal(100)
  )(
      stageEstimate: (
          PostgreSqlFactorAwareExplainCandidate,
          PostgreSqlFactorAwareExplainStageStatement,
          Int
      ) => PostgreSqlExplainCostEstimate = (_, _, _) => estimate()
  ): PostgreSqlFactorAwareExplainBatch = {
    val source = PostgreSqlExplainSourceEvaluation(
      request.source.sourceStatementSha256,
      estimate(totalCost = BigDecimal(10), rows = sourceRows)
    )
    val observations = request.flattened.zipWithIndex.map {
      case ((candidate, stage), index) =>
        PostgreSqlFactorAwareExplainObservation(
          candidate.physicalVariantId,
          stage.stageId,
          stage.statementSha256,
          stageEstimate(candidate, stage, index)
        )
    }
    PostgreSqlFactorAwareExplainBatch(provenance, source, observations)
  }

  def estimate(
      totalCost: BigDecimal = BigDecimal(1),
      rows: BigDecimal = BigDecimal(1),
      width: Long = 8L,
      observedProvenance: PostgreSqlExplainCostProvenance = provenance
  ): PostgreSqlExplainCostEstimate =
    PostgreSqlExplainCostEstimate(
      startupCost = BigDecimal("0.125"),
      planTotalCost = totalCost,
      planRows = rows,
      planWidth = width,
      provenance = observedProvenance
    )

  def candidateId(rank: Int): CandidatePlanId =
    CandidatePlanId
      .parse("sha256:" + f"$rank%064x")
      .fold(problem => throw new AssertionError(problem.message), identity)

  private def prepare(
      sql: String,
      tables: Vector[TestTable],
      widthBound: Int,
      connectedCover: Boolean,
      limit: Int
  ): Prepared = {
    val planner = CalcitePlannerFixture(tables)
    val analysis = CalciteQueryAnalyzer.analyze(planner.plan(sql).root)
    if (!analysis.isSupported)
      throw new AssertionError(analysis.diagnostics.mkString("; "))
    val hypergraph = analysis.hypergraph.fold(
      problems => throw new AssertionError(problems.map(_.message).mkString("; ")),
      identity
    )
    val generated = SoftBagGenerator
      .generate(hypergraph, widthBound)
      .fold(problem => throw new AssertionError(problem.message), identity)
    val input = CandidateSearchInput
      .create(
        hypergraph,
        widthBound,
        generated.admissible(requireConnectedCover = connectedCover)
      )
      .fold(
        problems =>
          throw new AssertionError(problems.map(_.message).mkString("; ")),
        identity
      )
    enumerate(input, limit) match {
      case found: CandidatePlanDecision.Found =>
        Prepared(sql, analysis.query, input, found)
      case rejected: CandidatePlanDecision.Rejected =>
        throw new AssertionError(s"fixture unexpectedly rejected: $rejected")
    }
  }

  private def enumerate(
      input: CandidateSearchInput,
      limit: Int
  ): CandidatePlanDecision =
    TopKCandidateTD
      .enumerate(input, limit)
      .fold(problem => throw new AssertionError(problem.message), identity)

  private def typedTable(
      name: String,
      first: String,
      second: String
  ): TestTable =
    TestTable(
      name,
      Vector(
        TestColumn("row_id", SqlTypeName.BIGINT, nullable = false),
        TestColumn(first, SqlTypeName.BIGINT, nullable = true),
        TestColumn(second, SqlTypeName.BIGINT, nullable = true)
      )
    )

  val triangleSql: String =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  private val triangleTables = Vector(
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

  val q5Sql: String =
    """SELECT
      |  c.customer_id AS "customer_id",
      |  o.order_id AS "order_id",
      |  l.supplier_id AS "supplier_id",
      |  c.nation_id AS "nation_id",
      |  n.region_id AS "region_id"
      |FROM q5d_customer AS c
      |JOIN q5d_orders AS o ON c.customer_id = o.customer_id
      |JOIN q5d_lineitem AS l ON o.order_id = l.order_id
      |JOIN q5d_supplier AS s
      |  ON l.supplier_id = s.supplier_id AND s.nation_id = c.nation_id
      |JOIN q5d_nation AS n ON s.nation_id = n.nation_id
      |JOIN q5d_region AS r ON n.region_id = r.region_id
      |""".stripMargin

  private val q5Tables = Vector(
    typedTable("q5d_customer", "customer_id", "nation_id"),
    typedTable("q5d_orders", "customer_id", "order_id"),
    typedTable("q5d_lineitem", "order_id", "supplier_id"),
    typedTable("q5d_supplier", "supplier_id", "nation_id"),
    typedTable("q5d_nation", "nation_id", "region_id"),
    TestTable(
      "q5d_region",
      Vector(
        TestColumn("row_id", SqlTypeName.BIGINT, nullable = false),
        TestColumn("region_id", SqlTypeName.BIGINT, nullable = true)
      )
    )
  )
}
