package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlanningLimits,
  SemijoinPass
}
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

class PostgreSqlGlobalAggregateFastPathRenderingSuite extends munit.FunSuite {
  private val tableNames = (0 until 4).map(index => s"aggregate_e$index").toVector

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    tableNames.foreach { name =>
      schema.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("left_key", SqlTypeName.INTEGER)
              .add("right_key", SqlTypeName.INTEGER)
              .add("payload", SqlTypeName.INTEGER)
              .build()
        }
      )
    }
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  private val cycleFrom =
    "FROM aggregate_e0 AS e0 " +
      "JOIN aggregate_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN aggregate_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN aggregate_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key"

  test("full reduction reads the latest root or non-root guard-owner factor") {
    val fixtures = (0 until 4).map { guardIndex =>
      val function = if (guardIndex % 2 == 0) "MIN" else "MAX"
      compileOk(
        s"SELECT $function(e$guardIndex.payload) AS \"result\" $cycleFrom",
        YannakakisReductionMode.FullTwoPass
      )
    }.toVector
    val rootOwned = fixtures.filter(fixture =>
      extremum(fixture).aggregateBagId == fixture.compiled.plan.root
    )
    val nonRootOwned = fixtures.filterNot(rootOwned.contains)

    assert(fixtures.head.compiled.plan.factors.size > 1)
    assert(rootOwned.nonEmpty, clues(fixtures.map(extremum(_).aggregateBagId)))
    assert(nonRootOwned.nonEmpty, clues(fixtures.map(extremum(_).aggregateBagId)))

    fixtures.foreach { fixture =>
      val terminal = extremum(fixture)
      val edge = fixture.compiled.edgeByOccurrence(
        terminal.argument.ref.relationOccurrenceId
      )
      val factorTrace = fixture.compiled.trace.bags
        .find(_.bagId == terminal.aggregateBagId)
        .getOrElse(fail("missing guard-owner factor trace"))
      val factorAlias = PostgreSqlNames.finalFactor(terminal.aggregateBagId)
      val occurrenceColumn = PostgreSqlNames.occurrenceColumn(
        edge.id,
        terminal.argument.ref.id
      )
      val expected =
        s"SELECT\n  ${terminal.function.sqlName}(\"$factorAlias\".\"$occurrenceColumn\") " +
          s"AS \"result\"\nFROM \"${factorTrace.finalFactorCte}\" AS \"$factorAlias\";"
      val finalFactorStage = fixture.rendered.trace.stages
        .find(_.physicalCteName.contains(factorTrace.finalFactorCte))
        .getOrElse(fail("missing latest guard-owner factor stage"))

      assert(terminal.usesFullyReducedGuardFactor)
      assertEquals(
        terminalSql(fixture.rendered.sql),
        expected,
        clues(fixture.compiled.plan.root, terminal.aggregateBagId)
      )
      assert(!fixture.rendered.sql.contains("\"yr_g_s0\""))
      assertEquals(
        fixture.rendered.output.map(column =>
          (column.ordinal, column.name, column.dataType, column.nullable)
        ),
        Vector((0, "result", terminal.output.dataType, true))
      )
      assertEquals(
        fixture.rendered.trace.stages.last.logicalBagIds,
        Vector(terminal.aggregateBagId)
      )
      assertEquals(fixture.rendered.trace.stages.last.logicalEdgeIds, Vector.empty)
      assertEquals(
        fixture.rendered.trace.stages.last.inputStageIds,
        Vector(finalFactorStage.id)
      )
      assertEquals(
        fixture.compiled.bags.flatMap(
          _.ownedOccurrences.flatMap(_.carriedColumns.map(_.ref))
        ),
        Vector(terminal.argument.ref)
      )
      assertEquals(
        fixture.rendered.trace.bags.flatMap(_.occurrenceColumns).map(_.columnId),
        Vector(terminal.argument.ref.id)
      )
    }
  }

  test("partial reduction retains direct extremum semijoin for every guard owner") {
    val full = (0 until 4).map { guardIndex =>
      guardIndex -> compileOk(
        s"SELECT MIN(e$guardIndex.payload) AS \"result\" $cycleFrom",
        YannakakisReductionMode.FullTwoPass
      )
    }.toVector
    val rootGuard = full.find { case (_, fixture) =>
      extremum(fixture).aggregateBagId == fixture.compiled.plan.root
    }.getOrElse(fail("cycle plan has no root-owned guard"))
    val nonRootGuard = full.find { case (_, fixture) =>
      extremum(fixture).aggregateBagId != fixture.compiled.plan.root
    }.getOrElse(fail("cycle plan has no non-root guard"))

    val rootBottomUp = compileOk(
      s"SELECT MIN(e${rootGuard._1}.payload) AS \"result\" $cycleFrom",
      YannakakisReductionMode.BottomUpOnly
    )
    assertEquals(
      extremum(rootBottomUp).aggregateBagId,
      rootBottomUp.compiled.plan.root
    )

    val nonRootBottomUp = compileOk(
      s"SELECT MIN(e${nonRootGuard._1}.payload) AS \"result\" $cycleFrom",
      YannakakisReductionMode.BottomUpOnly
    )
    assertNotEquals(
      extremum(nonRootBottomUp).aggregateBagId,
      nonRootBottomUp.compiled.plan.root
    )

    val multiBagNone = compileOk(
      s"SELECT MIN(e${nonRootGuard._1}.payload) AS \"result\" $cycleFrom",
      YannakakisReductionMode.NoReduction
    )
    assert(multiBagNone.compiled.plan.factors.size > 1)

    Vector(rootBottomUp, nonRootBottomUp, multiBagNone).foreach { fixture =>
      assert(!extremum(fixture).usesFullyReducedGuardFactor)
      val sql = terminalSql(fixture.rendered.sql)
      assert(sql.startsWith("SELECT\n  MIN("), clues(sql))
      assert(sql.contains("\n  EXISTS ("), clues(sql))
      assert(!sql.contains("IS NOT DISTINCT FROM"), clues(sql))
      assertEquals(
        fixture.rendered.trace.stages.last.inputStageIds,
        Vector.empty
      )
    }

    val oneBag = compileOk(
      "SELECT MAX(e0.payload) AS \"result\" " +
        "FROM aggregate_e0 AS e0 JOIN aggregate_e1 AS e1 " +
        "ON e0.left_key = e1.left_key",
      YannakakisReductionMode.NoReduction
    )
    assertEquals(oneBag.compiled.plan.factors.size, 1)
    assertEquals(extremum(oneBag).aggregateBagId, oneBag.compiled.plan.root)
    assert(terminalSql(oneBag.rendered.sql).startsWith("SELECT\n  MAX("))
    assert(terminalSql(oneBag.rendered.sql).contains("\n  EXISTS ("))
  }

  test("full COUNT emits one nested message tree with exact resources and narrow factors") {
    val fixture = compileOk(
      s"SELECT COUNT(*) AS \"result\" $cycleFrom",
      YannakakisReductionMode.FullTwoPass
    )
    val terminal = fixture.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.GlobalCountStar]
    val frequency = terminal.strategy match {
      case CompiledPostgreSqlTerminal.GlobalCountStrategy
            .FrequencyPropagation(plan) => plan
      case other => fail(s"expected frequency propagation, found $other")
    }
    val marker = "\nSELECT\n  CAST(COALESCE(SUM("
    val start = fixture.rendered.sql.indexOf(marker)
    assert(start >= 0, clues(fixture.rendered.sql))
    val sql = fixture.rendered.sql.substring(start + 1)

    assert(frequency.messages.nonEmpty)
    assert(sql.startsWith("SELECT\n  CAST(COALESCE(SUM("), clues(sql))
    assert(sql.contains("COALESCE(SUM("), clues(sql))
    assert(sql.contains("GROUP BY\n"), clues(sql))
    assert(sql.contains(" = "), clues(sql))
    assertEquals(
      occurrences(fixture.rendered.sql, "CAST(COUNT(*) AS NUMERIC)"),
      frequency.bags.size,
      clues(fixture.rendered.sql)
    )
    assertEquals(
      occurrences(sql, "CAST(COUNT(*) AS NUMERIC)"),
      0,
      clues(sql)
    )
    assertEquals(
      occurrences(sql, "COALESCE(SUM("),
      1 + frequency.bags.size +
        frequency.messages.count(_.requiresRegrouping),
      clues(sql)
    )
    frequency.bags.foreach { bag =>
      assert(
        sql.contains(s" AS \"${PostgreSqlNames.countFactorInput(bag.bagId)}\""),
        clues(bag, sql)
      )
    }
    assert(
      frequency.messages.exists(message => !message.requiresRegrouping),
      clues(frequency.messages)
    )
    assertEquals(
      fixture.compiled.reductionCtes.count(_.finalizesCountFrequency),
      frequency.bags.size
    )
    fixture.compiled.reductionCtes
      .filter(_.finalizesCountFrequency)
      .foreach { reduction =>
        val factor = fixture.compiled.plan.factors.find(
          _.bagId == reduction.target
        ).get
        val cteStart = fixture.rendered.sql.indexOf(s"  \"${reduction.outputCte}\" AS (")
        val nextCte = fixture.rendered.sql.indexOf("\n  \"", cteStart + 4)
        val body = fixture.rendered.sql.substring(
          cteStart,
          if (nextCte < 0) fixture.rendered.sql.length else nextCte
        )
        assert(body.contains("CAST(COUNT(*) AS NUMERIC) AS \"yr_freq\""), clues(body))
        factor.vertices.foreach(variableId =>
          assert(
            body.contains(s"GROUP BY") &&
              body.contains(s"\"${PostgreSqlNames.variable(variableId)}\""),
            clues(reduction, variableId, body)
          )
        )
      }
    fixture.compiled.reductionCtes.foreach { reduction =>
      reduction.members.filter(_.step.separator.nonEmpty).foreach { member =>
        val sourceAlias = PostgreSqlNames.countReductionSource(member.step)
        assert(
          fixture.rendered.sql.contains(
            s"SELECT DISTINCT\n          \"$sourceAlias\"."
          ),
          clues(member, fixture.rendered.sql)
        )
      }
    }
    fixture.compiled.trace.bags.foreach { bag =>
      assertEquals(
        occurrences(sql, s"\"${bag.finalFactorCte}\""),
        1,
        clues(bag.bagId, sql)
      )
    }
    assertEquals(
      fixture.compiled.bags.flatMap(
        _.ownedOccurrences.flatMap(_.carriedColumns)
      ),
      Vector.empty
    )
    assertEquals(fixture.rendered.statistics.renderedTermCount, exactTerms(fixture))
  }

  test("staged COUNT finalization is canonical, trace-truthful, and fail-closed") {
    val fixture = compileOk(
      s"SELECT COUNT(*) AS \"result\" $cycleFrom",
      YannakakisReductionMode.FullTwoPass
    )
    val compiled = fixture.compiled
    val finalizers = compiled.reductionCtes.filter(_.finalizesCountFrequency)
    val factorById = compiled.plan.factors.iterator
      .map(factor => factor.bagId -> factor)
      .toMap

    assertEquals(finalizers.map(_.target).toSet, factorById.keySet)
    finalizers.foreach { reduction =>
      val factor = factorById(reduction.target)
      if (reduction.target == compiled.plan.root) {
        assertEquals(reduction.pass, SemijoinPass.BottomUp)
        assertEquals(factor.parent, None)
      } else {
        assertEquals(reduction.pass, SemijoinPass.TopDown)
        assertEquals(reduction.members.size, 1)
        assertEquals(factor.parent, Some(reduction.members.head.step.source))
      }
    }

    val latestStageByBag = compiled.trace.stages.collect {
      case stage
          if stage.id.isInstanceOf[PostgreSqlStageId.BagFactorVersion] =>
        val id = stage.id.asInstanceOf[PostgreSqlStageId.BagFactorVersion]
        id.bagId -> (id.version -> stage.id)
    }.groupBy(_._1).iterator.map { case (bagId, values) =>
      bagId -> values.map(_._2).maxBy(_._1)._2
    }.toMap
    val terminal = compiled.trace.stages.last
    assertEquals(
      terminal.inputStageIds,
      terminal.logicalBagIds.map(latestStageByBag)
    )

    val schedule = CompiledPostgreSqlReductionSchedule(
      YannakakisReductionMode.FullTwoPass,
      compiled.plan.schedule.bottomUp,
      compiled.plan.schedule.topDown
    )
    val finalizationsByBag = finalizers.iterator
      .map(reduction => reduction.target -> reduction.finalization)
      .toMap
    assertEquals(
      PostgreSqlRenderingCompiler.validateReductionCtes(
        compiled.plan,
        schedule,
        compiled.trace.bags,
        compiled.trace.semijoins,
        compiled.reductionCtes,
        finalizationsByBag
      ),
      Vector.empty
    )
    val corrupted = compiled.reductionCtes.updated(
      compiled.reductionCtes.indexWhere(_.finalizesCountFrequency),
      finalizers.head.copy(
        finalization =
          CompiledPostgreSqlReductionFinalization.RetainRawRows
      )
    )
    assert(
      PostgreSqlRenderingCompiler
        .validateReductionCtes(
          compiled.plan,
          schedule,
          compiled.trace.bags,
          compiled.trace.semijoins,
          corrupted,
          finalizationsByBag
        )
        .exists(_.message.contains("reduction-finalization"))
    )
  }

  test("one-bag COUNT remains a direct globally safe local count") {
    val fixture = compileOk(
      "SELECT COUNT(*) AS \"result\" " +
        "FROM aggregate_e0 AS e0 JOIN aggregate_e1 AS e1 " +
        "ON e0.left_key = e1.left_key",
      YannakakisReductionMode.FullTwoPass
    )
    assertEquals(fixture.compiled.plan.factors.size, 1)
    assertEquals(fixture.compiled.reductionCtes, Vector.empty)
    assertEquals(
      occurrences(fixture.rendered.sql, "CAST(COUNT(*) AS NUMERIC)"),
      1
    )
    assert(!fixture.rendered.sql.contains("SELECT DISTINCT\n          \"yr_cd_"))
  }

  private final class CompiledFixture(
      val compiled: CompiledPostgreSqlRendering,
      val rendered: PostgreSqlRenderedQuery
  )

  private def compileOk(
      sql: String,
      mode: YannakakisReductionMode
  ): CompiledFixture =
    compile(sql, mode).fold(
      problems => fail(problems.map(_.message).mkString("; ")),
      identity
    )

  private def compile(
      sql: String,
      mode: YannakakisReductionMode
  ): Either[Vector[PostgreSqlRenderingError], CompiledFixture] = {
    val report = analyzed(sql)
    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    val prepared = PostgreSqlRendererTestSupport.prepareQuery(
      report,
      hypergraph,
      widthBound = 2,
      requireConnectedCover = true
    )
    PostgreSqlBagFactorRenderer
      .compileAndRender(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        mode
      )
      .map { case (compiled, rendered) =>
        new CompiledFixture(compiled, rendered)
      }
  }

  private def analyzed(sql: String): AnalysisReport = {
    val planned = CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)
    val report = CalciteQueryAnalyzer.analyze(planned)
    assert(report.isSupported, clues(sql, report.diagnostics))
    report
  }

  private def extremum(
      fixture: CompiledFixture
  ): CompiledPostgreSqlTerminal.GlobalGuardedExtremum =
    fixture.compiled.terminal match {
      case value: CompiledPostgreSqlTerminal.GlobalGuardedExtremum => value
      case other => fail(s"expected guarded extremum terminal, found $other")
    }

  private def terminalSql(sql: String): String = {
    val start = sql.lastIndexOf("\nSELECT\n")
    assert(start >= 0, clues(sql))
    sql.substring(start + 1)
  }

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

  /** Independent exact term oracle for the aggregate rendering branches. */
  private def exactTerms(fixture: CompiledFixture): Long = {
    val compiled = fixture.compiled
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
        val vertexCount = factorById(reduction.target).vertices.size
        terms += (BigInt(2) * vertexCount) + 1
      } else terms += 1
      reduction.members.foreach { member =>
        terms += 1 + member.step.separator.size
        if (member.step.separator.nonEmpty)
          terms += member.step.separator.size
      }
    }
    val frequency = compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.GlobalCountStar]
      .strategy
      .asInstanceOf[
        CompiledPostgreSqlTerminal.GlobalCountStrategy.FrequencyPropagation
      ]
      .plan
    terms += frequency.resources.additionalRenderedTerms
    assert(terms.isValidLong, clues(terms))
    terms.toLong
  }
}
