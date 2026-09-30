package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  CalciteQueryAnalyzer,
  CalciteSqlPlanner
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlCompactReductionFinalizationCompilerSuite
    extends munit.FunSuite {
  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    (0 until 4).foreach { index =>
      schema.add(
        s"compact_e$index",
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

  private val compactProfile = PostgreSqlAggregateRenderingProfile
    .create(
      PostgreSqlAggregateOneBagTerminalProfile.LegacyNestedV1,
      PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2,
      PostgreSqlReductionSupportProfile.DistinctSeparatorSupportV1
    )
    .fold(problems => fail(problems.mkString("; ")), identity)

  private val collapsedProfile = PostgreSqlAggregateRenderingProfile
    .create(
      PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2,
      PostgreSqlAggregateReductionFinalizationProfile.LegacyFullVerticesV1,
      PostgreSqlReductionSupportProfile.DistinctSeparatorSupportV1
    )
    .fold(problems => fail(problems.mkString("; ")), identity)

  test("compact COUNT finalizations use each propagation bag's exact local grouping") {
    val prepared = prepare(
      "SELECT COUNT(*) AS \"result\" " +
        "FROM compact_e0 AS e0 " +
        "JOIN compact_e1 AS e1 ON e0.right_key = e1.left_key " +
        "JOIN compact_e2 AS e2 ON e1.right_key = e2.left_key " +
        "JOIN compact_e3 AS e3 ON e2.right_key = e3.left_key " +
        "AND e3.right_key = e0.left_key"
    )
    val compiled = compile(prepared, compactProfile)
    assert(compiled.plan.factors.size > 1)

    val frequencyPlan = compiled.terminal match {
      case CompiledPostgreSqlTerminal.GlobalCountStar(
            _,
            CompiledPostgreSqlTerminal.GlobalCountStrategy
              .FrequencyPropagation(plan)
          ) => plan
      case other => fail(s"expected frequency COUNT terminal, found $other")
    }
    val expected = frequencyPlan.bags.iterator.map { bag =>
      bag.bagId -> CompiledPostgreSqlReductionFinalization.CompactFrequency(
        bag.localGrouping,
        suppressEmptyGroup = bag.localGrouping.isEmpty
      )
    }.toMap
    val actual = compiled.reductionCtes.iterator.collect {
      case reduction if reduction.finalizesCountFrequency =>
        reduction.target -> reduction.finalization
    }.toMap

    assertEquals(actual, expected)
    assertEquals(compiled.aggregateRenderingProfile, compactProfile)
  }

  test("collapsed one-bag COUNT charges no redundant local propagation terms") {
    val prepared = prepare(
      "SELECT COUNT(*) AS \"result\" " +
        "FROM compact_e0 AS e0 JOIN compact_e1 AS e1 " +
        "ON e0.left_key = e1.left_key"
    )
    val legacy = compile(prepared, PostgreSqlAggregateRenderingProfile.LegacyV1)
    val collapsed = compile(prepared, collapsedProfile)
    assertEquals(legacy.plan.factors.size, 1)
    assertEquals(collapsed.reductionCtes, Vector.empty)

    val additionalTerms = legacy.terminal match {
      case CompiledPostgreSqlTerminal.GlobalCountStar(
            _,
            CompiledPostgreSqlTerminal.GlobalCountStrategy
              .FrequencyPropagation(plan)
          ) => plan.resources.additionalRenderedTerms.toLong
      case other => fail(s"expected frequency COUNT terminal, found $other")
    }
    assertEquals(
      legacy.statistics.renderedTermCount -
        collapsed.statistics.renderedTermCount,
      additionalTerms
    )
  }

  test("compact scalar state requires its empty-input suppression guard") {
    intercept[IllegalArgumentException] {
      CompiledPostgreSqlReductionFinalization.CompactFrequency(
        Vector.empty,
        suppressEmptyGroup = false
      )
    }
    val guarded = CompiledPostgreSqlReductionFinalization.CompactFrequency(
      Vector.empty,
      suppressEmptyGroup = true
    )
    assert(guarded.suppressEmptyGroup)
  }

  private def prepare(
      sql: String
  ): PostgreSqlRendererTestSupport.PreparedQuery = {
    val report = analyzed(sql)
    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    PostgreSqlRendererTestSupport.prepareQuery(
      report,
      hypergraph,
      widthBound = 2,
      requireConnectedCover = true
    )
  }

  private def compile(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      profile: PostgreSqlAggregateRenderingProfile
  ): CompiledPostgreSqlRendering =
    PostgreSqlRenderingCompiler
      .compile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlCteMaterializationDirective.PlannerControlled,
        profile
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def analyzed(sql: String): AnalysisReport = {
    val planned = CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)
    val report = CalciteQueryAnalyzer.analyze(planned)
    assert(report.isSupported, clues(sql, report.diagnostics))
    report
  }
}
