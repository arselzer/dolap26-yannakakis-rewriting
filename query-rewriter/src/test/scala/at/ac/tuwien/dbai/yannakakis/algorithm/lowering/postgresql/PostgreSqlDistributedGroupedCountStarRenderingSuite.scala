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

class PostgreSqlDistributedGroupedCountStarRenderingSuite
    extends munit.FunSuite {
  private val schema = {
    val root = Frameworks.createRootSchema(true)
    Vector("dist_e0", "dist_e1", "dist_e2", "dist_e3").foreach { name =>
      root.add(
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
    root
  }

  private val planner = CalciteSqlPlanner
    .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
    .fold(problem => fail(problem.toString), identity)

  private val cycle =
    "FROM dist_e0 AS e0 " +
      "JOIN dist_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN dist_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN dist_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key"

  private val distributedSql =
    "SELECT e0.left_key AS \"First Key\", " +
      "e2.left_key AS \"Second Key\", COUNT(*) AS \"Rows\" " +
      cycle + " GROUP BY e0.left_key, e2.left_key"

  private val carriedChainSql =
    "SELECT e0.right_key AS \"First Key\", " +
      "e2.right_key AS \"Second Key\", COUNT(*) AS \"Rows\" " +
      "FROM dist_e0 AS e0 " +
      "JOIN dist_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN dist_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN dist_e3 AS e3 ON e2.right_key = e3.left_key " +
      "GROUP BY e0.right_key, e2.right_key"

  test("three-key opt-in uses the general propagation plan and exact resource accounting") {
    val sql = "SELECT e2.left_key AS x, e0.left_key AS x, e1.left_key AS x, COUNT(*) " +
      cycle + " GROUP BY e2.left_key, e0.left_key, e1.left_key"
    val prepared = prepare(sql, threeKeys = true)
    val (compiled, rendered) = PostgreSqlBagFactorRenderer.compileAndRender(
      prepared.query, prepared.input, prepared.plan, PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default, YannakakisReductionMode.FullTwoPass
    ).fold(problems => fail(problems.mkString("; ")), identity)
    val terminal = compiled.terminal.asInstanceOf[CompiledPostgreSqlTerminal.DistributedGroupedCountStar]
    assertEquals(terminal.groupVariables.distinct.size, 3)
    assertEquals(terminal.propagationPlan.finalization.groupKeys.size, 3)
    assertEquals(terminal.renderedTermCount, 4)
    assertEquals(rendered.output.map(_.name), Vector("x", "x", "x", "count"))
    assertEquals(rendered.statistics.renderedTermCount, exactRenderedTerms(compiled))
    for (mode <- Vector(YannakakisReductionMode.BottomUpOnly, YannakakisReductionMode.NoReduction)) {
      assert(PostgreSqlBagFactorRenderer.compileAndRender(prepared.query, prepared.input,
        prepared.plan, PostgreSqlRenderingLimits.default, BagFactorPlanningLimits.default, mode).isLeft)
    }
    Vector("e1.payload", "e1.right_key").foreach { key =>
      val rejected = s"SELECT e2.left_key,e0.left_key,$key,COUNT(*) " +
        cycle + s" GROUP BY e2.left_key,e0.left_key,$key"
      val p = prepare(rejected, threeKeys = true)
      assert(PostgreSqlBagFactorRenderer.compileAndRender(p.query,p.input,p.plan,
        PostgreSqlRenderingLimits.default,BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass).isLeft)
    }
  }

  test("wider grouping admission leaves existing two-key SQL byte-identical") {
    val legacy = compile(distributedSql)
    val prepared = prepare(distributedSql, threeKeys = true)
    val (_, wider) = PostgreSqlBagFactorRenderer.compileAndRender(
      prepared.query, prepared.input, prepared.plan, PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default, YannakakisReductionMode.FullTwoPass
    ).fold(problems => fail(problems.mkString("; ")), identity)
    assertEquals(wider.sql, legacy.rendered.sql)
    assertEquals(wider.output, legacy.rendered.output)
    assertEquals(wider.statistics, legacy.rendered.statistics)
  }

  test("two structural keys propagate independently to the terminal") {
    val fixture = compile(carriedChainSql, widthBound = 1)
    val terminal = fixture.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.DistributedGroupedCountStar]
    val propagation = terminal.propagationPlan

    assertEquals(terminal.groupVariables.distinct.size, 2)
    assertEquals(
      propagation.finalization.groupKeys,
      terminal.groupVariables.map(
        PostgreSqlAggregatePropagation.AggregateKey.Structural
      )
    )
    assertEquals(propagation.root, terminal.aggregateBagId)
    assertEquals(propagation.laneBindings, Vector.empty)
    assertEquals(propagation.maximumCarriedKeys, 1)
    assert(propagation.messages.exists(_.carriedKeys.nonEmpty))
    propagation.messages.foreach { message =>
      message.carriedKeys.foreach {
        case PostgreSqlAggregatePropagation.AggregateKey.Structural(variable) =>
          val forbidden =
            s"= \"${PostgreSqlNames.countMessage(message.source)}\"." +
              s"\"${PostgreSqlNames.variable(variable)}\""
          assert(
            !fixture.rendered.sql.contains(forbidden),
            clues(message, forbidden, fixture.rendered.sql)
          )
        case other => fail(s"U1 emitted nonstructural carried key $other")
      }
    }
    assertEquals(
      PostgreSqlAggregatePropagation.validate(
        fixture.compiled.plan,
        terminal.aggregateBagId,
        PostgreSqlAggregatePropagation.AggregateRequest.countStarGroupedBy(
          terminal.groupVariables.map(
            PostgreSqlAggregatePropagation.AggregateKey.Structural
          ): _*
        ),
        propagation
      ),
      Right(propagation)
    )

    assertEquals(
      fixture.rendered.output.map(column =>
        (column.ordinal, column.name, column.dataType, column.nullable)
      ),
      Vector(
        (0, "First Key", "INTEGER", true),
        (1, "Second Key", "INTEGER", true),
        (2, "Rows", "BIGINT NOT NULL", false)
      )
    )
    assertEquals(terminal.renderedTermCount, 3)
    assertEquals(carriedSourceNames(fixture.compiled), Vector.empty)
    terminal.groupVariables.foreach { variable =>
      assert(
        fixture.rendered.sql.contains(
          s"\"${PostgreSqlNames.variable(variable)}\""
        ),
        clues(variable, fixture.rendered.sql)
      )
    }
    assert(
      fixture.rendered.sql.contains("CAST(COALESCE(SUM("),
      clues(fixture.rendered.sql)
    )
    assertEquals(
      fixture.rendered.sql.split("\nGROUP BY\n").last.count(_ == ','),
      1,
      clues(fixture.rendered.sql)
    )

    val terminalStage = fixture.rendered.trace.stages.last
    assertEquals(terminalStage.logicalBagIds, terminal.inputBagIds(fixture.compiled.plan))
    assertEquals(terminalStage.logicalEdgeIds, Vector.empty)
    assertEquals(terminalStage.inputStageIds.size, fixture.compiled.plan.factors.size)
    assertEquals(
      fixture.rendered.statistics.sqlCharacterCount,
      fixture.rendered.sql.length.toLong
    )

    val independentlyCounted =
      PostgreSqlAggregateStructuralResourceCounter.count(propagation)
    assertEquals(independentlyCounted.localLaneConversionTerms, BigInt(0))
    assertEquals(independentlyCounted.terminalGroupingTerms, BigInt(2))
    assertEquals(
      independentlyCounted.propagatedMessageAggregateTerms +
        independentlyCounted.propagatedMessageProjectionTerms,
      BigInt(propagation.messages.size)
    )
    assertEquals(
      fixture.rendered.statistics.renderedTermCount,
      exactRenderedTerms(fixture.compiled)
    )
  }

  test("one factor retains both keys and duplicate multiplicity") {
    val sql =
      "SELECT e0.left_key AS g0, e1.right_key AS g1, COUNT(*) AS n " +
        "FROM dist_e0 AS e0 JOIN dist_e1 AS e1 " +
        "ON e0.left_key = e1.left_key AND e0.right_key = e1.right_key " +
        "GROUP BY e0.left_key, e1.right_key"
    val fixture = compile(sql, widthBound = 1)
    val terminal = fixture.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.DistributedGroupedCountStar]

    assertEquals(fixture.compiled.plan.factors.size, 1)
    assertEquals(terminal.propagationPlan.messages, Vector.empty)
    assertEquals(terminal.propagationPlan.maximumMessageNestingDepth, 0)
    assertEquals(fixture.rendered.statistics.expandedScanCount, 2L)
    assert(fixture.rendered.sql.contains("CAST(COUNT(*) AS NUMERIC)"))
    assert(fixture.rendered.sql.contains("AS \"g0\""))
    assert(fixture.rendered.sql.contains("AS \"g1\""))
  }

  test("two-key SQL keeps exact frequencies NUMERIC until one per-group BIGINT cast") {
    val fixture = compile(carriedChainSql, widthBound = 1)
    val sql = fixture.rendered.sql
    val terminal = fixture.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.DistributedGroupedCountStar]

    assertEquals(terminal.propagationPlan.laneBindings, Vector.empty)
    assert(sql.contains("CAST(COUNT(*) AS NUMERIC)"), clues(sql))
    assert(sql.contains("CAST(COALESCE(SUM("), clues(sql))
    assertEquals(
      occurrences(sql, "AS BIGINT) AS \"Rows\""),
      1,
      clues(sql)
    )
    assertEquals(occurrences(sql, " AS BIGINT)"), 1, clues(sql))
    assert(!sql.contains("AS BIGINT) AS \"yr_freq\""), clues(sql))
    assertEquals(
      fixture.rendered.output.map(column => column.dataType -> column.nullable),
      Vector(
        "INTEGER" -> true,
        "INTEGER" -> true,
        "BIGINT NOT NULL" -> false
      )
    )
  }

  test("the opposite-key C4 and exact resource boundary render canonically") {
    val fixture = compile(distributedSql)
    val terminal = fixture.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.DistributedGroupedCountStar]
    assertEquals(terminal.groupVariables.distinct.size, 2)
    assertEquals(terminal.propagationPlan.finalization.groupKeys.size, 2)
    assertEquals(
      fixture.rendered.statistics.renderedTermCount,
      exactRenderedTerms(fixture.compiled)
    )

    val exact = limitsWithRenderedTerms(
      fixture.rendered.statistics.renderedTermCount
    )
    assert(
      PostgreSqlBagFactorRenderer
        .compileAndRender(
          fixture.compiled.query,
          fixture.compiled.input,
          fixture.compiled.plan,
          exact,
          BagFactorPlanningLimits.default,
          YannakakisReductionMode.FullTwoPass
        )
        .isRight
    )
    val below = limitsWithRenderedTerms(
      fixture.rendered.statistics.renderedTermCount - 1
    )
    val problems = PostgreSqlBagFactorRenderer
      .compileAndRender(
        fixture.compiled.query,
        fixture.compiled.input,
        fixture.compiled.plan,
        below,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass
      )
      .fold(identity, value => fail(s"below-exact term limit rendered: $value"))
    assert(
      problems.exists {
        case PostgreSqlRenderingError.ResourceLimitExceeded(
              PostgreSqlRenderingResource.RenderedTerms,
              required,
              limit
            ) =>
          required == BigInt(fixture.rendered.statistics.renderedTermCount) &&
            limit == fixture.rendered.statistics.renderedTermCount - 1
        case _ => false
      },
      clues(problems)
    )

    val hostile = fixture.compiled.copy(
      terminal = terminal.copy(
        propagationPlan = terminal.propagationPlan.copy(
          resources = terminal.propagationPlan.resources.copy(
            terminalGroupingTerms = BigInt(1)
          )
        )
      )
    )
    intercept[IllegalArgumentException] {
      PostgreSqlEmitter.characterCount(hostile)
    }
  }

  test("partial reduction modes and duplicate structural keys fail closed") {
    val prepared = prepare(distributedSql)
    Vector(
      YannakakisReductionMode.BottomUpOnly,
      YannakakisReductionMode.NoReduction
    ).foreach { mode =>
      PostgreSqlBagFactorRenderer.compileAndRender(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        mode
      ) match {
        case Left(
              Vector(
                problem: PostgreSqlRenderingError
                  .DistributedGroupedCountRequiresFullTwoPass
              )
            ) => assertEquals(problem.mode, mode)
        case other => fail(s"expected distributed grouped mode rejection, found $other")
      }
    }

    val sameVariable =
      "SELECT e0.right_key, e1.left_key, COUNT(*) " + cycle +
        " GROUP BY e0.right_key, e1.left_key"
    val duplicate = prepare(sameVariable)
    PostgreSqlBagFactorRenderer.compileAndRender(
      duplicate.query,
      duplicate.input,
      duplicate.plan,
      PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default,
      YannakakisReductionMode.FullTwoPass
    ) match {
      case Left(problems) =>
        assert(
          problems.exists(
            _.isInstanceOf[
              PostgreSqlRenderingError.UnsupportedDistributedGroupedCountKey
            ]
          ),
          clues(problems)
        )
      case Right(value) =>
        fail(s"duplicate structural grouping variable rendered: ${value._2.sql}")
    }


    val payload = prepare(
      "SELECT e0.left_key, e2.payload, COUNT(*) " + cycle +
        " GROUP BY e0.left_key, e2.payload"
    )
    PostgreSqlBagFactorRenderer.compileAndRender(
      payload.query,
      payload.input,
      payload.plan,
      PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default,
      YannakakisReductionMode.FullTwoPass
    ) match {
      case Left(problems) =>
        assert(
          problems.exists(
            _.isInstanceOf[
              PostgreSqlRenderingError.UnsupportedDistributedGroupedCountKey
            ]
          ),
          clues(problems)
        )
      case Right(value) =>
        fail(s"payload distributed grouping key rendered: ${value._2.sql}")
    }
  }

  private final class CompiledFixture(
      val compiled: CompiledPostgreSqlRendering,
      val rendered: PostgreSqlRenderedQuery
  )

  private def compile(
      sql: String,
      widthBound: Int = 2
  ): CompiledFixture = {
    val prepared = prepare(sql, widthBound)
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
      prepared.query.resultShape
        .isInstanceOf[QueryResultShape.DistributedGroupedCountStar]
    )
    new CompiledFixture(compiled, rendered)
  }

  private def prepare(
      sql: String,
      widthBound: Int = 2,
      threeKeys: Boolean = false
  ): PostgreSqlRendererTestSupport.PreparedQuery = {
    val report = analyzed(sql, threeKeys)
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

  private def analyzed(sql: String, threeKeys: Boolean = false): AnalysisReport = {
    val selectedPlanner = if (threeKeys) CalciteSqlPlanner.forSchemaWithDistributedCountGrouping(schema, maximumGroupKeys = 3)
      .fold(problem => fail(problem.toString), identity) else planner
    val planned = CalciteSqlPlanner
      .planWithSourceEvidence(selectedPlanner, sql)
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

  private def limitsWithRenderedTerms(value: Long): PostgreSqlRenderingLimits =
    PostgreSqlRenderingLimits
      .create(
        PostgreSqlRenderingLimits.default.maxExpandedScans,
        value,
        PostgreSqlRenderingLimits.default.maxCtes,
        PostgreSqlRenderingLimits.default.maxSqlCharacters
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def exactRenderedTerms(compiled: CompiledPostgreSqlRendering): Long = {
    var terms = BigInt(compiled.terminal.renderedTermCount)
    compiled.bags.foreach { bag =>
      bag.supportStrategy match {
        case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
          terms += bag.factor.vertices.size + bag.supportEqualities.size
          bag.supportEdges.foreach(edge =>
            terms += compiled.localPredicatesByOccurrence
              .getOrElse(edge.relationOccurrenceId, Vector.empty)
              .size
          )
          terms += bag.factor.vertices.size
          bag.ownedOccurrences.foreach { occurrence =>
            terms += occurrence.edge.bindings.size
            terms += occurrence.carriedColumns.size
            terms += compiled.localPredicatesByOccurrence
              .getOrElse(occurrence.relation.id, Vector.empty)
              .size
          }
        case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
          terms += bag.factor.vertices.size + bag.ownedEqualities.size
          bag.ownedOccurrences.foreach { occurrence =>
            terms += occurrence.carriedColumns.size
            terms += compiled.localPredicatesByOccurrence
              .getOrElse(occurrence.relation.id, Vector.empty)
              .size
          }
      }
    }
    val factorById = compiled.plan.factors.iterator
      .map(factor => factor.bagId -> factor)
      .toMap
    compiled.reductionCtes.foreach { reduction =>
      if (reduction.finalizesCountFrequency) {
        val vertices = factorById(reduction.target).vertices.size
        terms += (BigInt(2) * vertices) + 1
      } else terms += 1
      reduction.members.foreach { member =>
        terms += 1 + member.step.separator.size
        if (member.step.separator.nonEmpty)
          terms += member.step.separator.size
      }
    }
    val propagation = compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.DistributedGroupedCountStar]
      .propagationPlan
    terms += PostgreSqlAggregateStructuralResourceCounter
      .count(propagation)
      .additionalRenderedTerms
    assert(terms.isValidLong, clues(terms))
    terms.toLong
  }
}
