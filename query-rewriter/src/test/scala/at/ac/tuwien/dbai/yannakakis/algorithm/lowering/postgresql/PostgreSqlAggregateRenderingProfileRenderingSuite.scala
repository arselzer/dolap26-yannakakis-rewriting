package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  CalciteQueryAnalyzer,
  CalciteSqlPlanner
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlAggregateRenderingProfileRenderingSuite
    extends munit.FunSuite {
  import PostgreSqlAggregateOneBagTerminalProfile._
  import PostgreSqlAggregateReductionFinalizationProfile._
  import PostgreSqlReductionSupportProfile._

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    Vector("profile_e0", "profile_e1", "profile_e2", "profile_e3")
      .foreach { name =>
        schema.add(
          name,
          new AbstractTable {
            override def getRowType(factory: RelDataTypeFactory): RelDataType =
              factory
                .builder()
                .add("left_key", SqlTypeName.INTEGER)
                .nullable(true)
                .add("right_key", SqlTypeName.INTEGER)
                .nullable(true)
                .add("payload", SqlTypeName.INTEGER)
                .nullable(true)
                .build()
          }
        )
      }
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  private val cycle =
    "FROM profile_e0 AS e0 " +
      "JOIN profile_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN profile_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN profile_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key"

  private val oneBagJoin =
    "FROM profile_e0 AS e0 " +
      "JOIN profile_e1 AS e1 " +
      "ON e0.left_key = e1.left_key " +
      "AND e0.right_key = e1.right_key"

  test("the explicit legacy profile preserves the ordinary renderer byte for byte") {
    val fixtures = Vector(
      prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2),
      prepare(
        s"SELECT e0.left_key AS g, COUNT(*) AS n $cycle GROUP BY e0.left_key",
        widthBound = 2
      ),
      prepare(
        "SELECT e0.left_key AS g0, e2.left_key AS g1, COUNT(*) AS n " +
          cycle + " GROUP BY e0.left_key, e2.left_key",
        widthBound = 2
      )
    )

    fixtures.foreach { prepared =>
      val ordinary = compileOrdinary(prepared)
      val explicit = compile(prepared, PostgreSqlAggregateRenderingProfile.LegacyV1)

      assertEquals(explicit.compiled, ordinary.compiled)
      assertEquals(explicit.rendered, ordinary.rendered)
      assertEquals(explicit.rendered.sql, ordinary.rendered.sql)
      assert(
        explicit.compiled.aggregateRenderingProfile eq
          PostgreSqlAggregateRenderingProfile.LegacyV1
      )
    }
  }

  test("direct EXISTS removes exactly the staged separator-support projections") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val legacy = compile(prepared, PostgreSqlAggregateRenderingProfile.LegacyV1)
    val direct = compile(
      prepared,
      profile(LegacyNestedV1, LegacyFullVerticesV1, DirectExistsV2)
    )
    val stagedMembers = legacy.compiled.reductionCtes
      .filter(_.finalizesCountFrequency)
      .flatMap(_.members)
      .filter(_.step.separator.nonEmpty)
    val removedTerms = stagedMembers.iterator
      .map(_.step.separator.size.toLong)
      .sum

    assert(stagedMembers.nonEmpty, clues(legacy.rendered.sql))
    assertEquals(
      occurrences(legacy.rendered.sql, "SELECT DISTINCT\n          \"yr_cd_"),
      stagedMembers.size
    )
    assertEquals(
      occurrences(direct.rendered.sql, "SELECT DISTINCT\n          \"yr_cd_"),
      0,
      clues(direct.rendered.sql)
    )
    assertEquals(
      occurrences(legacy.rendered.sql, "SELECT DISTINCT") -
        occurrences(direct.rendered.sql, "SELECT DISTINCT"),
      stagedMembers.size
    )
    stagedMembers.foreach { member =>
      val sourceAlias = PostgreSqlNames.semijoinSource(member.step)
      assert(
        direct.rendered.sql.contains(
          s"FROM \"${member.trace.sourceInputCte}\" AS \"$sourceAlias\""
        ),
        clues(member, direct.rendered.sql)
      )
    }

    assertEquals(direct.compiled.plan, legacy.compiled.plan)
    assertEquals(direct.compiled.terminal, legacy.compiled.terminal)
    assertEquals(direct.compiled.bags, legacy.compiled.bags)
    assertEquals(direct.compiled.reductionCtes, legacy.compiled.reductionCtes)
    assertEquals(direct.compiled.trace, legacy.compiled.trace)
    assertEquals(direct.compiled.output, legacy.compiled.output)
    assertEquals(
      direct.rendered.statistics.renderedTermCount,
      legacy.rendered.statistics.renderedTermCount - removedTerms
    )
    assertEquals(
      direct.rendered.statistics.cteCount,
      legacy.rendered.statistics.cteCount
    )
    assertEquals(
      direct.rendered.statistics.expandedScanCount,
      legacy.rendered.statistics.expandedScanCount
    )
    assert(direct.rendered.sql.length < legacy.rendered.sql.length)
  }

  test("compact finalization installs exact local schemas and removes local reaggregation") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val legacy = compile(prepared, PostgreSqlAggregateRenderingProfile.LegacyV1)
    val compact = compile(
      prepared,
      profile(LegacyNestedV1, CompactLocalStateV2, DistinctSeparatorSupportV1)
    )
    val terminal = compact.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.GlobalCountStar]
    val frequency = terminal.strategy
      .asInstanceOf[
        CompiledPostgreSqlTerminal.GlobalCountStrategy.FrequencyPropagation
      ]
      .plan
    val compactFinalizations = compact.compiled.reductionCtes
      .filter(_.finalizesCountFrequency)
      .map(reduction => reduction.target -> reduction.finalization)
      .toMap
    val expected = frequency.bags.iterator.map { bag =>
      bag.bagId -> CompiledPostgreSqlReductionFinalization.CompactFrequency(
        bag.localGrouping,
        suppressEmptyGroup = bag.localGrouping.isEmpty
      )
    }.toMap

    assertEquals(compactFinalizations, expected)
    assertEquals(compactFinalizations.keySet, compact.compiled.plan.factors.map(_.bagId).toSet)
    assert(
      compactFinalizations.values.forall {
        case CompiledPostgreSqlReductionFinalization.CompactFrequency(
              grouping,
              suppressEmptyGroup
            ) => grouping.nonEmpty && !suppressEmptyGroup
        case _ => false
      },
      clues(compactFinalizations)
    )
    assert(!compact.rendered.sql.contains("HAVING COUNT(*) > 0"))

    compact.compiled.reductionCtes
      .filter(_.finalizesCountFrequency)
      .foreach { reduction =>
        val grouping = reduction.finalization
          .asInstanceOf[
            CompiledPostgreSqlReductionFinalization.CompactFrequency
          ]
          .groupingVariables
        val body = cteBody(compact.rendered.sql, reduction.outputCte)
        val groupBy = body.substring(body.indexOf("\n    GROUP BY\n"))
        grouping.foreach { variable =>
          assert(
            groupBy.contains(s"\"${PostgreSqlNames.variable(variable)}\""),
            clues(reduction, groupBy)
          )
        }
        val unexpected = compact.compiled.plan.factors
          .find(_.bagId == reduction.target)
          .toVector
          .flatMap(_.vertices)
          .filterNot(grouping.contains)
        unexpected.foreach { variable =>
          assert(
            !groupBy.contains(s"\"${PostgreSqlNames.variable(variable)}\""),
            clues(reduction, variable, groupBy)
          )
        }
      }

    assert(
      occurrences(legacy.rendered.sql, "COALESCE(SUM(\"yr_ci_b") > 0,
      clues(legacy.rendered.sql)
    )
    assertEquals(
      occurrences(compact.rendered.sql, "COALESCE(SUM(\"yr_ci_b"),
      0,
      clues(compact.rendered.sql)
    )

    val factorById = compact.compiled.plan.factors.iterator
      .map(factor => factor.bagId -> factor)
      .toMap
    val narrowedReductionTerms = frequency.bags.iterator.map { bag =>
      2L * (factorById(bag.bagId).vertices.size - bag.localGrouping.size)
    }.sum
    val localTerms = frequency.resources.localAggregateTerms +
      frequency.resources.localKeyProjectionTerms +
      frequency.resources.localKeyGroupingTerms
    assertEquals(
      compact.rendered.statistics.renderedTermCount,
      legacy.rendered.statistics.renderedTermCount -
        narrowedReductionTerms - localTerms.toLong
    )
    assertEquals(
      compact.rendered.statistics.cteCount,
      legacy.rendered.statistics.cteCount
    )
    assertEquals(
      compact.rendered.statistics.expandedScanCount,
      legacy.rendered.statistics.expandedScanCount
    )
    assert(compact.rendered.sql.length < legacy.rendered.sql.length)
  }

  test("collapsed one-bag terminals emit one direct aggregate for global grouped and U1 COUNT") {
    val cases = Vector(
      new OneBagCase(
        s"SELECT COUNT(*) AS n $oneBagJoin",
        expectedSelect = "SELECT\n  COUNT(*) AS \"n\"",
        expectedGroupTerms = Vector.empty
      ),
      new OneBagCase(
        s"SELECT e0.left_key AS g, COUNT(*) AS n $oneBagJoin GROUP BY e0.left_key",
        expectedSelect = "COUNT(*) AS \"n\"",
        expectedGroupTerms = Vector("g")
      ),
      new OneBagCase(
        "SELECT e0.left_key AS g0, e1.right_key AS g1, COUNT(*) AS n " +
          oneBagJoin + " GROUP BY e0.left_key, e1.right_key",
        expectedSelect = "COUNT(*) AS \"n\"",
        expectedGroupTerms = Vector("g0", "g1")
      )
    )
    val collapsedProfile = profile(
      CollapsedDirectV2,
      LegacyFullVerticesV1,
      DistinctSeparatorSupportV1
    )

    cases.foreach { oneBagCase =>
      val prepared = prepare(oneBagCase.sql, widthBound = 1)
      val legacy = compile(prepared, PostgreSqlAggregateRenderingProfile.LegacyV1)
      val compactWithoutCollapse = compile(
        prepared,
        profile(LegacyNestedV1, CompactLocalStateV2, DirectExistsV2)
      )
      val collapsed = compile(prepared, collapsedProfile)

      assertEquals(collapsed.compiled.plan.factors.size, 1, clues(oneBagCase.sql))
      assertEquals(collapsed.compiled.reductionCtes, Vector.empty)
      assertEquals(compactWithoutCollapse.rendered.sql, legacy.rendered.sql)
      assertEquals(
        compactWithoutCollapse.rendered.statistics,
        legacy.rendered.statistics
      )
      assert(
        collapsed.rendered.sql.contains(oneBagCase.expectedSelect),
        clues(collapsed.rendered.sql)
      )
      assert(
        !collapsed.rendered.sql.contains("CAST(COUNT(*) AS NUMERIC)"),
        clues(collapsed.rendered.sql)
      )
      assert(!collapsed.rendered.sql.contains("COALESCE(SUM("), clues(collapsed.rendered.sql))
      assertEquals(
        occurrences(collapsed.rendered.sql, "COUNT(*)"),
        1,
        clues(collapsed.rendered.sql)
      )
      assertEquals(
        occurrences(collapsed.rendered.sql, "\nGROUP BY\n"),
        if (oneBagCase.expectedGroupTerms.isEmpty) 0 else 1,
        clues(collapsed.rendered.sql)
      )
      oneBagCase.expectedGroupTerms.foreach { alias =>
        assert(collapsed.rendered.sql.contains(s" AS \"$alias\""))
      }
      assertEquals(collapsed.compiled.plan, legacy.compiled.plan)
      assertEquals(collapsed.compiled.terminal, legacy.compiled.terminal)
      assertEquals(collapsed.compiled.trace, legacy.compiled.trace)
      assertEquals(
        collapsed.rendered.statistics.cteCount,
        legacy.rendered.statistics.cteCount
      )
      assertEquals(
        collapsed.rendered.statistics.expandedScanCount,
        legacy.rendered.statistics.expandedScanCount
      )
      assert(
        collapsed.rendered.statistics.renderedTermCount <
          legacy.rendered.statistics.renderedTermCount,
        clues(
          oneBagCase.sql,
          collapsed.rendered.statistics,
          legacy.rendered.statistics
        )
      )
      assert(collapsed.rendered.sql.length < legacy.rendered.sql.length)
    }
  }

  test("the development renderer rejects a null aggregate profile before compilation") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val result = PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfile(
      prepared.query,
      prepared.input,
      prepared.plan,
      PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default,
      YannakakisReductionMode.FullTwoPass,
      null
    )

    result match {
      case Left(Vector(PostgreSqlRenderingError.MissingValue(label))) =>
        assertEquals(label, "PostgreSQL aggregate rendering profile")
      case other => fail(s"expected a typed missing-profile rejection, found $other")
    }
  }

  test("collapsed one-bag global COUNT is exact in every compatibility mode") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $oneBagJoin", widthBound = 1)
    val collapsedProfile = profile(
      CollapsedDirectV2,
      CompactLocalStateV2,
      DirectExistsV2
    )
    YannakakisReductionMode.Values.foreach { mode =>
      val legacy = compile(
        prepared,
        PostgreSqlAggregateRenderingProfile.LegacyV1,
        mode
      )
      val collapsed = compile(prepared, collapsedProfile, mode)
      assertEquals(collapsed.compiled.reductionCtes, Vector.empty)
      assertEquals(occurrences(collapsed.rendered.sql, "COUNT(*)"), 1)
      assert(!collapsed.rendered.sql.contains("COALESCE(SUM("))
      assertEquals(
        collapsed.rendered.statistics.sqlCharacterCount,
        collapsed.rendered.sql.length.toLong
      )
      assert(
        collapsed.rendered.statistics.renderedTermCount <=
          legacy.rendered.statistics.renderedTermCount,
        clues(mode, collapsed.rendered.statistics, legacy.rendered.statistics)
      )
    }
  }

  private final class CompiledFixture(
      val compiled: CompiledPostgreSqlRendering,
      val rendered: PostgreSqlRenderedQuery
  )

  private final class OneBagCase(
      val sql: String,
      val expectedSelect: String,
      val expectedGroupTerms: Vector[String]
  )

  private def prepare(
      sql: String,
      widthBound: Int
  ): PostgreSqlRendererTestSupport.PreparedQuery = {
    val report = analyzed(sql)
    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    PostgreSqlRendererTestSupport.prepareQuery(
      report,
      hypergraph,
      widthBound,
      requireConnectedCover = true
    )
  }

  private def analyzed(sql: String): AnalysisReport = {
    val planned = CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)
    val report = CalciteQueryAnalyzer.analyze(planned)
    assert(report.isSupported, clues(report.diagnostics))
    report
  }

  private def compileOrdinary(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery
  ): CompiledFixture = {
    val (compiled, rendered) = PostgreSqlBagFactorRenderer
      .compileAndRender(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    new CompiledFixture(compiled, rendered)
  }

  private def compile(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      aggregateProfile: PostgreSqlAggregateRenderingProfile,
      reductionMode: YannakakisReductionMode =
        YannakakisReductionMode.FullTwoPass
  ): CompiledFixture = {
    val (compiled, rendered) = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        reductionMode,
        aggregateProfile
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    new CompiledFixture(compiled, rendered)
  }

  private def profile(
      oneBag: PostgreSqlAggregateOneBagTerminalProfile,
      finalization: PostgreSqlAggregateReductionFinalizationProfile,
      support: PostgreSqlReductionSupportProfile
  ): PostgreSqlAggregateRenderingProfile =
    PostgreSqlAggregateRenderingProfile
      .create(oneBag, finalization, support)
      .fold(
        problems => fail(problems.map(_.code).mkString("; ")),
        identity
      )

  private def cteBody(sql: String, cteName: String): String = {
    val startMarker = s"  \"$cteName\" AS ("
    val start = sql.indexOf(startMarker)
    assert(start >= 0, clues(cteName, sql))
    val nextCte = sql.indexOf("\n  \"", start + startMarker.length)
    val terminal = sql.indexOf("\n)\nSELECT", start + startMarker.length)
    val end = Vector(nextCte, terminal).filter(_ >= 0).min
    sql.substring(start, end)
  }

  private def occurrences(haystack: String, needle: String): Int = {
    require(needle.nonEmpty, "needle must not be empty")
    var count = 0
    var from = 0
    var next = haystack.indexOf(needle, from)
    while (next >= 0) {
      count += 1
      from = next + needle.length
      next = haystack.indexOf(needle, from)
    }
    count
  }
}
