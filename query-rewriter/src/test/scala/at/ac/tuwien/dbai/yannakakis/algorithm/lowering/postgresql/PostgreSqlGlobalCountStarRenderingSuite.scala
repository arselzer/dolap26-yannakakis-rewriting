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

class PostgreSqlGlobalCountStarRenderingSuite extends munit.FunSuite {
  private val joined =
    "FROM count_r AS r JOIN count_s AS s ON r.k = s.k"

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    val tables =
      Vector("count_r", "count_s", "count_t", "count_u", "count_v")
    tables.foreach { name =>
      schema.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("k", SqlTypeName.INTEGER)
              .add("payload", SqlTypeName.INTEGER)
              .build()
        }
      )
    }
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  test("global COUNT(*) terminal has exact output and no payload liveness") {
    val count = compile("SELECT COUNT(*) AS \"Total\" " + joined)
    val projection = compile(
      "SELECT r.payload AS left_payload, s.payload AS right_payload " + joined
    )

    count.compiled.terminal match {
      case terminal: CompiledPostgreSqlTerminal.GlobalCountStar =>
        assertEquals(terminal.output, count.rendered.output.head)
        assertEquals(terminal.outputScalars, Vector.empty)
        assertEquals(terminal.renderedTermCount, 1)
        terminal.strategy match {
          case CompiledPostgreSqlTerminal.GlobalCountStrategy
                .FrequencyPropagation(plan) =>
            assertEquals(plan.root, count.compiled.plan.root)
          case other => fail(s"expected frequency propagation, found $other")
        }
      case other => fail(s"expected global COUNT(*) terminal, found $other")
    }
    assert(
      projection.compiled.terminal
        .isInstanceOf[CompiledPostgreSqlTerminal.Projection]
    )

    assertEquals(
      count.rendered.output.map(column =>
        (column.ordinal, column.name, column.dataType, column.nullable)
      ),
      Vector((0, "Total", "BIGINT NOT NULL", false))
    )
    assertEquals(
      count.prepared.query.resultShape.id,
      QueryResultShape.GlobalCountStar(
        count.prepared.query.output.head,
        count.prepared.query.root
          .asInstanceOf[
            at.ac.tuwien.dbai.yannakakis.frontend.calcite.GlobalCountStarIR
          ]
          .sourceCertificate
          .aliasMode
      ).id
    )
    assert(
      count.rendered.sql.contains(
        "SELECT\n  CAST(COALESCE(SUM(\"yr_cf_b0\".\"yr_freq\"), " +
          "CAST(0 AS NUMERIC)) AS BIGINT) AS \"Total\"\nFROM (\n" +
          "SELECT\n  CAST(COUNT(*) AS NUMERIC) AS \"yr_freq\""
      ),
      clues(count.rendered.sql)
    )
    assertEquals(
      occurrences(count.rendered.sql, "CAST(COUNT(*) AS NUMERIC)"),
      1
    )
    assert(!count.rendered.sql.contains("AS \"left_payload\""))
    assertEquals(
      count.rendered.statistics.sqlCharacterCount,
      count.rendered.sql.length.toLong
    )

    val countCarried = carriedSourceNames(count.compiled)
    val projectionCarried = carriedSourceNames(projection.compiled)
    assert(!countCarried.contains("payload"), clues(countCarried))
    assertEquals(countCarried, Vector.empty)
    assert(projectionCarried.contains("payload"), clues(projectionCarried))
    assert(
      count.rendered.statistics.renderedTermCount <
        projection.rendered.statistics.renderedTermCount,
      clues(
        count.rendered.statistics.renderedTermCount,
        projection.rendered.statistics.renderedTermCount
      )
    )
  }

  test("only full reduction uses frequency propagation; other modes reconstruct") {
    val sql = "SELECT COUNT(*) AS \"Total\" " + joined
    val full = compile(sql, YannakakisReductionMode.FullTwoPass)
    val fallbacks = Vector(
      YannakakisReductionMode.BottomUpOnly,
      YannakakisReductionMode.NoReduction
    ).map(mode => mode -> compile(sql, mode))

    assert(
      full.compiled.terminal
        .asInstanceOf[CompiledPostgreSqlTerminal.GlobalCountStar]
        .strategy
        .isInstanceOf[
          CompiledPostgreSqlTerminal.GlobalCountStrategy.FrequencyPropagation
        ]
    )
    assert(full.rendered.sql.contains("COALESCE(SUM("), clues(full.rendered.sql))
    assert(
      full.rendered.sql.contains("CAST(COUNT(*) AS NUMERIC)"),
      clues(full.rendered.sql)
    )
    assertEquals(carriedSourceNames(full.compiled), Vector.empty)

    fallbacks.foreach { case (mode, fallback) =>
      assertEquals(
        fallback.compiled.terminal
          .asInstanceOf[CompiledPostgreSqlTerminal.GlobalCountStar]
          .strategy,
        CompiledPostgreSqlTerminal.GlobalCountStrategy.Reconstruction,
        clues(mode)
      )
      assert(
        fallback.rendered.sql.contains(
          "SELECT\n  COUNT(*) AS \"Total\"\nFROM "
        ),
        clues(mode, fallback.rendered.sql)
      )
      assert(carriedSourceNames(fallback.compiled).contains("k"), clues(mode))
      assertEquals(fallback.rendered.output, full.rendered.output, clues(mode))
    }
  }

  test("local aggregation regroups only when neighboring separators differ") {
    val chain = compile(
      "SELECT COUNT(*) AS \"Total\" " +
        "FROM count_r AS r " +
        "JOIN count_s AS s ON r.k = s.k " +
        "JOIN count_t AS t ON s.payload = t.k " +
        "JOIN count_u AS u ON t.payload = u.k " +
        "JOIN count_v AS v ON u.payload = v.k"
    )
    val frequency = chain.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.GlobalCountStar]
      .strategy
      .asInstanceOf[
        CompiledPostgreSqlTerminal.GlobalCountStrategy.FrequencyPropagation
      ]
      .plan

    assert(
      frequency.messages.exists(_.requiresRegrouping),
      clues(frequency.messages, chain.rendered.sql)
    )
    assertEquals(
      occurrences(chain.rendered.sql, "CAST(COUNT(*) AS NUMERIC)"),
      frequency.bags.size
    )
    assertEquals(
      occurrences(chain.rendered.sql, "COALESCE(SUM("),
      1 + frequency.bags.size +
        frequency.messages.count(_.requiresRegrouping)
    )
  }

  test("enumeration terminal retains the canonical counting/materializing byte stream") {
    val projection = compile(
      "SELECT r.payload AS left_payload, s.payload AS right_payload " + joined
    )
    val counted = PostgreSqlEmitter.characterCount(projection.compiled)
    val rematerialized = PostgreSqlEmitter.render(
      projection.compiled,
      counted.toInt
    )

    assertEquals(counted, BigInt(projection.rendered.sql.length))
    assertEquals(rematerialized, projection.rendered.sql)
    assert(!rematerialized.contains("COUNT(*)"), clues(rematerialized))
    assert(!rematerialized.contains("MATERIALIZED"), clues(rematerialized))
  }

  private final class CompiledFixture(
      val prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      val compiled: CompiledPostgreSqlRendering,
      val rendered: PostgreSqlRenderedQuery
  )

  private def compile(
      sql: String,
      reductionMode: YannakakisReductionMode = YannakakisReductionMode.Default
  ): CompiledFixture = {
    val report = analyzed(sql)
    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    val prepared = PostgreSqlRendererTestSupport.prepareQuery(
      report,
      hypergraph,
      widthBound = 1,
      requireConnectedCover = true
    )
    val (compiled, rendered) = PostgreSqlBagFactorRenderer
      .compileAndRender(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        reductionMode
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    new CompiledFixture(prepared, compiled, rendered)
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

  private def occurrences(value: String, needle: String): Int = {
    var count = 0
    var from = 0
    var next = value.indexOf(needle, from)
    while (next >= 0) {
      count += 1
      from = next + needle.length
      next = value.indexOf(needle, from)
    }
    count
  }
}
