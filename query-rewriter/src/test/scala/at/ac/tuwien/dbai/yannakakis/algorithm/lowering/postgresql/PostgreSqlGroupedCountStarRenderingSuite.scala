package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  CalciteQueryAnalyzer,
  CalciteSqlPlanner,
  QueryResultShape
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlGroupedCountStarRenderingSuite extends munit.FunSuite {
  private val tables = Vector("group_r", "group_s", "group_t", "group_u")
  private val from =
    "FROM group_r AS r " +
      "JOIN group_s AS s ON r.k = s.k " +
      "JOIN group_t AS t ON s.payload = t.k " +
      "JOIN group_u AS u ON t.payload = u.k"

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    tables.foreach { name =>
      schema.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("k", SqlTypeName.INTEGER)
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

  test("grouped COUNT reroots terminal messages at the structural key owner") {
    val candidates = Vector("r.k", "s.payload", "t.payload", "u.k").map {
      key => compile(groupedSql(key))
    }
    val fixture = candidates.find { candidate =>
      val terminal = candidate.compiled.terminal
        .asInstanceOf[CompiledPostgreSqlTerminal.GroupedCountStar]
      terminal.aggregateBagId != candidate.compiled.plan.root
    }.getOrElse(
      fail("expected at least one structural grouping occurrence with a non-root owner")
    )
    val terminal = fixture.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.GroupedCountStar]
    val frequency = terminal.frequencyPlan

    assertEquals(frequency.root, terminal.aggregateBagId)
    assertNotEquals(frequency.root, fixture.compiled.plan.root)
    assertEquals(
      frequency.finalization.groupingVariables,
      Vector(terminal.groupVariable)
    )
    assert(
      frequency.bagById(frequency.root).localGrouping
        .contains(terminal.groupVariable)
    )
    assertEquals(
      terminal.inputBagIds(fixture.compiled.plan).toSet,
      fixture.compiled.plan.factors.map(_.bagId).toSet
    )

    assertEquals(
      fixture.rendered.output.map(column =>
        (column.ordinal, column.name, column.dataType, column.nullable)
      ),
      Vector(
        (0, "Group Key", "INTEGER", true),
        (1, "Rows", "BIGINT NOT NULL", false)
      )
    )
    assert(
      fixture.rendered.sql.contains(
        s"\"${PostgreSqlNames.variable(terminal.groupVariable)}\" AS \"Group Key\""
      ),
      clues(fixture.rendered.sql)
    )
    assert(
      fixture.rendered.sql.contains(
        "), CAST(0 AS NUMERIC)) AS BIGINT) AS \"Rows\""
      ),
      clues(fixture.rendered.sql)
    )
    assert(
      fixture.rendered.sql.endsWith(
        s"GROUP BY\n  \"${PostgreSqlNames.countFactor(frequency.root)}\"." +
          s"\"${PostgreSqlNames.variable(terminal.groupVariable)}\";"
      ),
      clues(fixture.rendered.sql)
    )
    assertEquals(terminal.renderedTermCount, 3)
    assert(terminal.groupOutput.nullable)
    assertEquals(carriedSourceNames(fixture.compiled), Vector.empty)
    assertEquals(
      fixture.rendered.statistics.sqlCharacterCount,
      fixture.rendered.sql.length.toLong
    )

    val terminalStage = fixture.rendered.trace.stages.last
    assertEquals(terminalStage.id, PostgreSqlStageId.FinalReconstruction)
    assertEquals(terminalStage.logicalBagIds, terminal.inputBagIds(fixture.compiled.plan))
    assertEquals(terminalStage.logicalEdgeIds, Vector.empty)
    assertEquals(terminalStage.inputStageIds.size, fixture.compiled.plan.factors.size)
  }

  test("empty-input semantics are grouped and per-group overflow is checked") {
    val fixture = compile(groupedSql("u.k"))
    val terminal = fixture.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.GroupedCountStar]

    assert(
      fixture.rendered.sql.startsWith("WITH\n"),
      clues(fixture.rendered.sql)
    )
    assert(
      fixture.rendered.sql.contains("SELECT\n  \"yr_cf_b"),
      clues(fixture.rendered.sql)
    )
    assert(
      fixture.rendered.sql.contains("CAST(COALESCE(SUM("),
      clues(fixture.rendered.sql)
    )
    assert(
      fixture.rendered.sql.contains("AS BIGINT) AS \"Rows\""),
      clues(fixture.rendered.sql)
    )
    assert(
      fixture.rendered.sql.contains("\nGROUP BY\n  "),
      clues(fixture.rendered.sql)
    )
    assertEquals(
      terminal.frequencyPlan.arithmetic,
      PostgreSqlFrequencyPropagation.ExactCountArithmetic
    )
    assertEquals(
      terminal.frequencyPlan.arithmetic.emptyInputCount,
      BigInt(0)
    )
    // GROUP BY over an empty root factor emits no row; unlike scalar COUNT,
    // there is deliberately no scalar SELECT without a grouping clause.
    assertEquals(terminal.frequencyPlan.finalization.groupingVariables.size, 1)
  }

  test("one factor preserves duplicate multiplicity across multiple occurrences") {
    val sql =
      "SELECT r.k AS \"Group Key\", COUNT(*) AS \"Rows\" " +
        "FROM group_r AS r " +
        "JOIN group_s AS s ON r.k = s.k AND r.payload = s.payload " +
        "GROUP BY r.k"
    val fixture = compile(sql)
    val terminal = fixture.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.GroupedCountStar]

    assertEquals(fixture.compiled.plan.factors.size, 1)
    assertEquals(fixture.compiled.bags.size, 1)
    assertEquals(fixture.compiled.bags.head.ownedOccurrences.size, 2)
    assertEquals(
      fixture.compiled.bags.head.ownedOccurrences
        .map(_.relation.qualifiedName.last)
        .toSet,
      Set("group_r", "group_s")
    )
    assertEquals(terminal.frequencyPlan.messages, Vector.empty)
    assertEquals(terminal.frequencyPlan.root, fixture.compiled.plan.root)
    assertEquals(
      terminal.frequencyPlan.finalization.groupingVariables,
      Vector(terminal.groupVariable)
    )
    assertEquals(fixture.rendered.statistics.expandedScanCount, 2L)
    fixture.compiled.bags.head.ownedOccurrences.foreach { occurrence =>
      val table = occurrence.relation.qualifiedName.last
      val alias = PostgreSqlNames.factorScan(
        fixture.compiled.bags.head.factor.bagId,
        occurrence.edge.id
      )
      assert(
        fixture.rendered.sql.contains(s"\"$table\" AS \"$alias\""),
        clues(table, alias, fixture.rendered.sql)
      )
    }
    assert(
      fixture.rendered.sql.endsWith(
        s"GROUP BY\n  \"${PostgreSqlNames.countFactor(terminal.aggregateBagId)}\"." +
          s"\"${PostgreSqlNames.variable(terminal.groupVariable)}\";"
      ),
      clues(fixture.rendered.sql)
    )
  }

  test("bottom-up and no-reduction grouped COUNT fail with a typed boundary") {
    val prepared = prepare(groupedSql("u.k"))
    Vector(
      YannakakisReductionMode.BottomUpOnly,
      YannakakisReductionMode.NoReduction
    ).foreach { mode =>
      val result = PostgreSqlBagFactorRenderer.compileAndRender(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        mode
      )
      result match {
        case Left(Vector(problem: PostgreSqlRenderingError.GroupedCountRequiresFullTwoPass)) =>
          assertEquals(problem.mode, mode)
        case other => fail(s"expected typed grouped COUNT mode rejection, found $other")
      }
    }
  }

  test("a direct payload grouping key remains frontend-valid but lowering-inapplicable") {
    val prepared = prepare(groupedSql("r.payload"))
    PostgreSqlBagFactorRenderer.compileAndRender(
      prepared.query,
      prepared.input,
      prepared.plan,
      PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default,
      YannakakisReductionMode.FullTwoPass
    ) match {
      case Left(problems) =>
        assert(
          problems.exists(
            _.isInstanceOf[PostgreSqlRenderingError.UnsupportedGroupedCountKey]
          ),
          clues(problems)
        )
      case Right(value) => fail(s"payload key unexpectedly rendered: ${value._2.sql}")
    }
  }

  private final class CompiledFixture(
      val compiled: CompiledPostgreSqlRendering,
      val rendered: PostgreSqlRenderedQuery
  )

  private def groupedSql(key: String): String =
    s"SELECT $key AS \"Group Key\", COUNT(*) AS \"Rows\" $from GROUP BY $key"

  private def compile(sql: String): CompiledFixture = {
    val prepared = prepare(sql)
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
    assert(
      prepared.query.resultShape.isInstanceOf[QueryResultShape.GroupedCountStar]
    )
    new CompiledFixture(compiled, rendered)
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
      widthBound = 1,
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

  private def carriedSourceNames(
      compiled: CompiledPostgreSqlRendering
  ): Vector[String] =
    compiled.bags.flatMap(
      _.ownedOccurrences.flatMap(_.carriedColumns.map(_.sourceName))
    )
}
