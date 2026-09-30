package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  TopKCandidateTD
}
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

class PostgreSqlGuardedExtremumDirectSemijoinSuite extends munit.FunSuite {
  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    ((0 until 7).map(index => s"direct_e$index") :+ "yr_g_s0").foreach {
      name =>
      schema.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("left_key", SqlTypeName.INTEGER)
              .add("right_key", SqlTypeName.INTEGER)
              .add("payload", SqlTypeName.INTEGER)
              .add("enabled", SqlTypeName.BOOLEAN)
              .build()
        }
      )
    }
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  private val filteredCycle =
    "FROM direct_e0 AS e0 " +
      "JOIN direct_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN direct_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN direct_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key " +
      "WHERE e0.enabled AND e2.enabled"

  private val twoDiamond =
    "FROM direct_e0 AS e0 " +
      "JOIN direct_e1 AS e1 ON e0.left_key = e1.left_key " +
      "JOIN direct_e2 AS e2 ON e0.right_key = e2.left_key " +
      "JOIN direct_e3 AS e3 ON e1.right_key = e3.left_key " +
      "AND e2.right_key = e3.right_key " +
      "JOIN direct_e4 AS e4 ON e3.right_key = e4.left_key " +
      "JOIN direct_e5 AS e5 ON e4.left_key = e5.left_key " +
      "JOIN direct_e6 AS e6 ON e4.right_key = e6.left_key " +
      "AND e5.right_key = e6.right_key"

  private val oneMaterializedChild =
    "FROM direct_e0 AS e0 " +
      "JOIN direct_e1 AS e1 ON e0.left_key = e1.left_key " +
      "JOIN direct_e2 AS e2 ON e1.right_key = e2.left_key " +
      "AND e0.left_key = e2.right_key " +
      "JOIN direct_e3 AS e3 ON e0.right_key = e3.left_key " +
      "JOIN direct_e4 AS e4 ON e3.right_key = e4.left_key " +
      "JOIN direct_e5 AS e5 ON e3.right_key = e5.left_key " +
      "AND e4.right_key = e5.right_key"

  test("terminal scans each source occurrence once and retains every predicate") {
    val fixture = compileOk(
      s"SELECT MIN(e0.payload) AS \"result\" $filteredCycle",
      YannakakisReductionMode.NoReduction
    )
    val terminal = extremum(fixture)
    val sql = terminalSql(fixture.rendered.sql)

    assert(fixture.compiled.plan.factors.size > 1)
    assert(sql.startsWith("SELECT\n  MIN("), clues(sql))
    assert(sql.contains("EXISTS ("), clues(sql))
    assert(!sql.contains("IS NOT DISTINCT FROM"), clues(sql))

    terminal.semijoinPlan.sourceEdgeIds.foreach { edgeId =>
      val edge = fixture.compiled.edgeById(edgeId)
      val relation = fixture.compiled.relationByOccurrence(
        edge.relationOccurrenceId
      )
      assertEquals(
        occurrences(
          sql,
          relation.qualifiedName.map(name => s"\"$name\"").mkString(".") +
            " AS "
        ),
        1,
        clues(edgeId, sql)
      )
    }

    val existsAt = sql.indexOf("EXISTS (")
    assert(existsAt > 0, clues(sql))
    val outer = sql.substring(0, existsAt)
    val witness = sql.substring(existsAt)
    val guardAlias = PostgreSqlNames.extremumScan(
      terminal.semijoinPlan.guardEdgeId
    )
    val filteredWitnessEdge = fixture.compiled.edgeByOccurrence
      .find { case (occurrenceId, _) =>
        fixture.compiled.relationByOccurrence(occurrenceId).qualifiedName.last ==
          "direct_e2"
      }
      .map(_._2)
      .getOrElse(fail("missing filtered witness edge"))
    val witnessRelation = fixture.compiled.relationByOccurrence(
      filteredWitnessEdge.relationOccurrenceId
    )
    assert(outer.contains(s"\"$guardAlias\".\"enabled\""), clues(outer))
    assert(
      !outer.contains(
        witnessRelation.qualifiedName.map(name => s"\"$name\"").mkString(".")
      ),
      clues(outer)
    )
    assert(witness.contains(".\"enabled\""), clues(witness))

    fixture.compiled.trace.bags.foreach(trace =>
      assert(!sql.contains(s"\"${trace.finalFactorCte}\""), clues(trace, sql))
    )
    val terminalStage = fixture.rendered.trace.stages.last
    assertEquals(terminalStage.logicalBagIds, Vector(terminal.aggregateBagId))
    assertEquals(
      terminalStage.logicalEdgeIds,
      terminal.semijoinPlan.sourceEdgeIds
    )
    assertEquals(terminalStage.inputStageIds, Vector.empty)

    val legacyScanCount = fixture.compiled.bags.iterator.map { bag =>
      bag.supportStrategy match {
        case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
          bag.supportEdges.size + bag.ownedOccurrences.size
        case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
          bag.ownedOccurrences.size
      }
    }.sum
    assertEquals(
      fixture.rendered.statistics.expandedScanCount,
      legacyScanCount.toLong + terminal.semijoinPlan.sourceEdgeIds.size
    )
  }

  test("semijoin provenance validation rejects a changed terminal program") {
    val fixture = compileOk(
      s"SELECT MAX(e1.payload) AS \"result\" $filteredCycle",
      YannakakisReductionMode.FullTwoPass
    )
    val terminal = extremum(fixture)
    val plan = terminal.semijoinPlan
    val corrupted = plan.copy(
      terminalEqualities = plan.terminalEqualities.reverse
    )
    assertNotEquals(corrupted, plan)

    val problems = PostgreSqlGuardedExtremumSemijoin
      .validate(
        fixture.compiled.plan,
        terminal.argument.ref,
        terminal.aggregateBagId,
        corrupted
      )
      .fold(identity, _ => fail("corrupted semijoin provenance was accepted"))
    assertEquals(
      problems.map(_.code),
      Vector(PostgreSqlGuardedExtremumSemijoin.ErrorCode.CandidateMismatch)
    )
  }

  test("q_hto-shaped lowering uses the full factor and stages partial modes") {
    val fixtures = YannakakisReductionMode.Values.map { mode =>
      mode -> compileOk(
        s"SELECT MIN(e0.left_key) AS \"result\" $twoDiamond",
        mode
      )
    }
    val directFixtures = fixtures.filterNot { case (_, fixture) =>
      extremum(fixture).usesFullyReducedGuardFactor
    }
    val terminalPrograms = directFixtures.map { case (_, fixture) =>
      terminalSql(fixture.rendered.sql)
    }
    assertEquals(terminalPrograms.distinct.size, 1)
    val supportPrograms = directFixtures.map { case (_, fixture) =>
      extremumProgramSql(fixture.rendered.sql, extremum(fixture).semijoinPlan)
    }
    assertEquals(supportPrograms.distinct.size, 1)

    fixtures.foreach { case (mode, fixture) =>
      val terminal = extremum(fixture)
      val plan = terminal.semijoinPlan
      val sql = fixture.rendered.sql
      val terminalProgram = terminalSql(sql)
      assertEquals(plan.stages.size, 2, clues(mode, plan, sql))
      if (terminal.usesFullyReducedGuardFactor) {
        assertEquals(mode, YannakakisReductionMode.FullTwoPass)
        assertEquals(occurrences(sql, " AS MATERIALIZED ("), 0, clues(sql))
        plan.stages.foreach(stage =>
          assert(!sql.contains(PostgreSqlNames.extremumStage(stage.ordinal)))
        )
        assert(!terminalProgram.contains("EXISTS ("), clues(terminalProgram))
        val factorTrace = fixture.compiled.trace.bags
          .find(_.bagId == terminal.aggregateBagId)
          .getOrElse(fail("missing full guard-owner factor"))
        val factorStage = fixture.rendered.trace.stages
          .find(_.physicalCteName.contains(factorTrace.finalFactorCte))
          .getOrElse(fail("missing latest full guard-owner factor stage"))
        assertEquals(
          fixture.rendered.trace.stages.last.logicalEdgeIds,
          Vector.empty
        )
        assertEquals(
          fixture.rendered.trace.stages.last.inputStageIds,
          Vector(factorStage.id)
        )
        assert(
          terminalProgram.contains(s"FROM \"${factorTrace.finalFactorCte}\""),
          clues(terminalProgram)
        )
      } else {
        val program = extremumProgramSql(sql, plan)
        assertNotEquals(mode, YannakakisReductionMode.FullTwoPass)
        assertEquals(
          occurrences(program, "SELECT DISTINCT"),
          2,
          clues(mode, program)
        )
        assertEquals(
          occurrences(sql, " AS MATERIALIZED ("),
          2,
          clues(mode, sql)
        )
        plan.stages.foreach { stage =>
          val name = PostgreSqlNames.extremumStage(stage.ordinal)
          assertEquals(
            occurrences(sql, s"\"$name\" AS MATERIALIZED ("),
            1,
            clues(mode, stage, sql)
          )
          // One definition and one consumer prove a flat, topological support
          // chain rather than a recursively duplicated derived table.
          assertEquals(occurrences(sql, s"\"$name\""), 2, clues(mode, stage, sql))
        }
        assert(
          !terminalProgram.contains("SELECT DISTINCT"),
          clues(mode, terminalProgram)
        )
        assert(!sql.contains("CROSS JOIN (\n      SELECT DISTINCT"), clues(mode, sql))
        plan.sourceEdgeIds.foreach { edgeId =>
          val relation = fixture.compiled.relationByOccurrence(
            fixture.compiled.edgeById(edgeId).relationOccurrenceId
          )
          assertEquals(
            occurrences(
              program,
              relation.qualifiedName.map(name => s"\"$name\"").mkString(".") +
                " AS "
            ),
            1,
            clues(mode, edgeId, program)
          )
        }
        assertEquals(
          fixture.rendered.trace.stages.last.logicalEdgeIds,
          plan.sourceEdgeIds
        )
        assertEquals(
          fixture.rendered.trace.stages.last.inputStageIds,
          Vector.empty
        )
      }
      assertEquals(
        plan.stages.map(_.existentialInputs.size),
        Vector(2, 1),
        clues(mode, plan)
      )
      assertEquals(
        plan.stages.map(_.output.consumedEdgeIds.size),
        Vector(3, 4),
        clues(mode, plan)
      )
      assertEquals(
        plan.terminalHandles.map(_.consumedEdgeIds.size),
        Vector(1, 1, 1, 4),
        clues(mode, plan)
      )
      assertEquals(
        plan.stages.map(stage =>
          fixture.compiled
            .relationByOccurrence(
              fixture.compiled.edgeById(stage.output.anchorEdgeId).relationOccurrenceId
            )
            .qualifiedName
            .last
        ),
        Vector("direct_e4", "direct_e3"),
        clues(mode, plan, sql)
      )
      assert(!sql.contains("IS NOT DISTINCT FROM"), clues(mode, sql))

      val compatibilityCtes = fixture.compiled.bags.map { bag =>
        bag.supportStrategy match {
          case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport => 2L
          case PostgreSqlBagSupportStrategy.ElidedByOwnedCover           => 1L
        }
      }.sum + fixture.compiled.reductionCtes.size
      assertEquals(
        fixture.rendered.statistics.cteCount,
        compatibilityCtes +
          (if (terminal.usesFullyReducedGuardFactor) 0L else plan.stages.size),
        clues(mode, fixture.rendered.statistics, plan)
      )
      val tracedNamedCtes = fixture.rendered.trace.stages.count(
        _.physicalForm.namedCte
      )
      assertEquals(tracedNamedCtes.toLong, compatibilityCtes)
    }
  }

  test("one-stage and zero-stage extrema materialize exactly their support count") {
    val oneStage = compileOk(
      s"SELECT MIN(e0.left_key) AS \"result\" $oneMaterializedChild",
      YannakakisReductionMode.BottomUpOnly
    )
    val zeroStage = compileOk(
      "SELECT MAX(e0.payload) AS \"result\" " +
        "FROM direct_e0 AS e0 JOIN direct_e1 AS e1 " +
        "ON e0.left_key = e1.left_key",
      YannakakisReductionMode.BottomUpOnly
    )

    assertEquals(
      extremum(oneStage).semijoinPlan.stages.size,
      1,
      clues(
        oneStage.compiled.plan.decomposition,
        oneStage.compiled.plan.ownership,
        extremum(oneStage).semijoinPlan
      )
    )
    assertEquals(
      occurrences(oneStage.rendered.sql, " AS MATERIALIZED ("),
      1
    )
    assert(oneStage.rendered.sql.contains("\"yr_g_s0\" AS MATERIALIZED ("))
    assertEquals(extremum(zeroStage).semijoinPlan.stages.size, 0)
    assert(!zeroStage.rendered.sql.contains(" AS MATERIALIZED ("))
    assert(!zeroStage.rendered.sql.contains("\"yr_g_s0\""))
  }

  test("q_hto structural rank 2 reads its exact fully reduced guard factor") {
    val fixture = compileRankOk(
      s"SELECT MIN(e0.left_key) AS \"result\" $twoDiamond",
      YannakakisReductionMode.FullTwoPass,
      rank = 2
    )
    val terminal = extremum(fixture)
    val rootFactor = fixture.compiled.plan.factors
      .find(_.bagId == fixture.compiled.plan.root)
      .getOrElse(fail("rank-2 q_hto shape has no root factor"))
    val factorTrace = fixture.rendered.trace.bags
      .find(_.bagId == terminal.aggregateBagId)
      .getOrElse(fail("rank-2 q_hto shape has no guard-owner trace"))

    assertEquals(fixture.compiled.plan.root.value, 0)
    assertEquals(rootFactor.supportCover.map(_.value), Vector(0, 2))
    assertEquals(terminal.aggregateBagId, fixture.compiled.plan.root)
    assert(terminal.usesFullyReducedGuardFactor)
    assertEquals(terminal.semijoinPlan.stages.size, 2)
    assertEquals(factorTrace.finalFactorCte, "yr_f_b0_bu1")
    assertEquals(
      terminalSql(fixture.rendered.sql),
      "SELECT\n  MIN(\"yr_o_b0\".\"yr_e0_c0\") AS \"result\"\n" +
        "FROM \"yr_f_b0_bu1\" AS \"yr_o_b0\";"
    )
    assert(!fixture.rendered.sql.contains("\"yr_g_s0\""))
    assertEquals(
      fixture.rendered.trace.stages.last.inputStageIds,
      Vector(
        fixture.rendered.trace.stages
          .find(_.physicalCteName.contains(factorTrace.finalFactorCte))
          .getOrElse(fail("rank-2 q_hto latest factor is not traced"))
          .id
      )
    )
  }

  test("terminal support CTEs are resource-bounded and collision-checked") {
    val sql = s"SELECT MIN(e0.left_key) AS \"result\" $twoDiamond"
    val fixture = compileOk(sql, YannakakisReductionMode.BottomUpOnly)
    val required = fixture.rendered.statistics.cteCount
    assert(required > extremum(fixture).semijoinPlan.stages.size)
    val limits = PostgreSqlRenderingLimits
      .create(maxCtes = required - 1L)
      .fold(problems => fail(problems.mkString("; ")), identity)
    val resourceProblems = compile(
      sql,
      YannakakisReductionMode.BottomUpOnly,
      limits
    ).fold(identity, _ => fail("under-counted terminal support CTEs"))
    assertEquals(
      resourceProblems.collect {
        case problem @ PostgreSqlRenderingError.ResourceLimitExceeded(
              PostgreSqlRenderingResource.Ctes,
              _,
              _
            ) => problem.required
      },
      Vector(BigInt(required))
    )

    val colliding = twoDiamond.replace("direct_e4 AS e4", "yr_g_s0 AS e4")
    val collisionProblems = compile(
      s"SELECT MIN(e0.left_key) AS \"result\" $colliding",
      YannakakisReductionMode.BottomUpOnly,
      PostgreSqlRenderingLimits.default
    ).fold(identity, _ => fail("generated terminal CTE collision was accepted"))
    assert(
      collisionProblems.exists {
        case PostgreSqlRenderingError.GeneratedNameCollision(name, _) =>
          name == PostgreSqlNames.extremumStage(0)
        case _ => false
      },
      clues(collisionProblems)
    )
  }

  test("guard-row existence identity preserves duplicates, NULLs, and emptiness") {
    val scenarios = Vector(
      Scenario(
        guards = Vector(
          GuardRow(Some(1), Some(9), enabled = true),
          GuardRow(Some(1), Some(3), enabled = true),
          GuardRow(Some(1), Some(3), enabled = true),
          GuardRow(Some(2), None, enabled = true)
        ),
        middles = Vector(
          MiddleRow(Some(1), Some(10), enabled = true),
          MiddleRow(Some(1), Some(10), enabled = true),
          MiddleRow(Some(2), Some(20), enabled = true)
        ),
        leaves = Vector(
          LeafRow(Some(10), enabled = true),
          LeafRow(Some(10), enabled = true),
          LeafRow(Some(20), enabled = true)
        )
      ),
      Scenario(
        guards = Vector(
          GuardRow(None, Some(-100), enabled = true),
          GuardRow(Some(1), None, enabled = true),
          GuardRow(Some(2), Some(7), enabled = true)
        ),
        middles = Vector(
          MiddleRow(None, Some(30), enabled = true),
          MiddleRow(Some(1), None, enabled = true),
          MiddleRow(Some(2), Some(40), enabled = true)
        ),
        leaves = Vector(
          LeafRow(None, enabled = true),
          LeafRow(Some(40), enabled = true)
        )
      ),
      Scenario(
        guards = Vector(GuardRow(Some(1), Some(5), enabled = true)),
        middles = Vector.empty,
        leaves = Vector(LeafRow(Some(1), enabled = true))
      ),
      Scenario(
        guards = Vector(
          GuardRow(Some(1), Some(1), enabled = false),
          GuardRow(Some(2), Some(8), enabled = true)
        ),
        middles = Vector(
          MiddleRow(Some(1), Some(10), enabled = true),
          MiddleRow(Some(2), Some(20), enabled = false)
        ),
        leaves = Vector(
          LeafRow(Some(10), enabled = true),
          LeafRow(Some(20), enabled = true)
        )
      )
    )

    scenarios.foreach { scenario =>
      val native = nativeWitnessValues(scenario)
      val semijoin = guardSemijoinValues(scenario)
      assertEquals(extremumValue(native, minimum = true), extremumValue(semijoin, minimum = true))
      assertEquals(extremumValue(native, minimum = false), extremumValue(semijoin, minimum = false))
    }
  }

  test("staged and fully reduced two-diamond identities preserve extrema") {
    val populated = TwoDiamondRows(
      r0 = Vector(
        Pair(Some(-5), Some(-50)),
        Pair(Some(1), Some(10)),
        Pair(Some(1), Some(10)),
        Pair(Some(9), Some(90)),
        Pair(Some(100), Some(777)),
        Pair(None, Some(10))
      ),
      r1 = Vector(
        Pair(Some(1), Some(20)),
        Pair(Some(9), Some(120)),
        Pair(None, Some(20))
      ),
      r2 = Vector(
        Pair(Some(10), Some(30)),
        Pair(Some(90), Some(130)),
        Pair(None, Some(30))
      ),
      r3 = Vector(
        Pair(Some(20), Some(30)),
        Pair(Some(120), Some(130)),
        Pair(None, Some(30))
      ),
      r4 = Vector(
        Pair(Some(30), Some(40)),
        Pair(Some(130), Some(140)),
        Pair(None, Some(40))
      ),
      r5 = Vector(
        Pair(Some(30), Some(50)),
        Pair(Some(30), Some(50)),
        Pair(Some(130), Some(150))
      ),
      r6 = Vector(
        Pair(Some(40), Some(50)),
        Pair(Some(40), Some(50)),
        Pair(Some(140), Some(150)),
        Pair(None, Some(50))
      )
    )
    Vector(populated, populated.copy(r6 = Vector.empty)).foreach { rows =>
      val native = nativeTwoDiamondGuardValues(rows)
      val staged = stagedTwoDiamondGuardValues(rows)
      val fullyReducedFactor = fullyReducedRankTwoGuardFactorValues(rows)
      assertEquals(
        extremumValue(native, minimum = true),
        extremumValue(staged, minimum = true)
      )
      assertEquals(
        extremumValue(native, minimum = false),
        extremumValue(staged, minimum = false)
      )
      assertEquals(
        extremumValue(native, minimum = true),
        extremumValue(fullyReducedFactor, minimum = true)
      )
      assertEquals(
        extremumValue(native, minimum = false),
        extremumValue(fullyReducedFactor, minimum = false)
      )
    }
  }

  private final case class CompiledFixture(
      compiled: CompiledPostgreSqlRendering,
      rendered: PostgreSqlRenderedQuery
  )

  private def compileOk(
      sql: String,
      mode: YannakakisReductionMode
  ): CompiledFixture =
    compile(sql, mode, PostgreSqlRenderingLimits.default).fold(
      problems => fail(problems.map(_.message).mkString("; ")),
      identity
    )

  private def compileRankOk(
      sql: String,
      mode: YannakakisReductionMode,
      rank: Int
  ): CompiledFixture = {
    val report = analyzed(sql)
    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    val topOne = PostgreSqlRendererTestSupport.prepareQuery(
      report,
      hypergraph,
      widthBound = 2,
      requireConnectedCover = true
    )
    val decomposition = TopKCandidateTD
      .enumerate(topOne.input, planLimit = rank)
      .fold(error => fail(error.message), {
        case found: CandidatePlanDecision.Found =>
          found.plans.lift(rank - 1).map(_.decomposition).getOrElse(
            fail(s"candidate search returned fewer than $rank plans")
          )
        case rejected: CandidatePlanDecision.Rejected =>
          fail(s"candidate search rejected q_hto shape: ${rejected.statistics}")
      })
    val prepared = PostgreSqlRendererTestSupport.withDecomposition(
      report,
      hypergraph,
      topOne.input,
      decomposition
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
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        result => CompiledFixture(result._1, result._2)
      )
  }

  private def compile(
      sql: String,
      mode: YannakakisReductionMode,
      limits: PostgreSqlRenderingLimits
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
        limits,
        BagFactorPlanningLimits.default,
        mode
      )
      .map(result => CompiledFixture(result._1, result._2))
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

  private def extremumProgramSql(
      sql: String,
      plan: PostgreSqlGuardedExtremumSemijoin.Plan
  ): String =
    plan.stages.headOption match {
      case Some(stage) =>
        val marker =
          s"  \"${PostgreSqlNames.extremumStage(stage.ordinal)}\" AS MATERIALIZED ("
        val start = sql.indexOf(marker)
        assert(start >= 0, clues(marker, sql))
        sql.substring(start)
      case None => terminalSql(sql)
    }

  private def occurrences(value: String, needle: String): Int = {
    var result = 0
    var offset = 0
    var next = value.indexOf(needle, offset)
    while (next >= 0) {
      result += 1
      offset = next + needle.length
      next = value.indexOf(needle, offset)
    }
    result
  }

  private final case class GuardRow(
      joinKey: Option[Int],
      value: Option[Int],
      enabled: Boolean
  )
  private final case class MiddleRow(
      joinKey: Option[Int],
      leafKey: Option[Int],
      enabled: Boolean
  )
  private final case class LeafRow(key: Option[Int], enabled: Boolean)
  private final case class Scenario(
      guards: Vector[GuardRow],
      middles: Vector[MiddleRow],
      leaves: Vector[LeafRow]
  )
  private final case class Pair(s: Option[Int], d: Option[Int])
  private final case class TwoDiamondRows(
      r0: Vector[Pair],
      r1: Vector[Pair],
      r2: Vector[Pair],
      r3: Vector[Pair],
      r4: Vector[Pair],
      r5: Vector[Pair],
      r6: Vector[Pair]
  )

  private def sqlEqual(left: Option[Int], right: Option[Int]): Boolean =
    left.nonEmpty && left == right

  private def nativeWitnessValues(scenario: Scenario): Vector[Option[Int]] =
    for {
      guard <- scenario.guards
      if guard.enabled
      middle <- scenario.middles
      if middle.enabled && sqlEqual(guard.joinKey, middle.joinKey)
      leaf <- scenario.leaves
      if leaf.enabled && sqlEqual(middle.leafKey, leaf.key)
    } yield guard.value

  private def guardSemijoinValues(
      scenario: Scenario
  ): Vector[Option[Int]] =
    scenario.guards.collect {
      case guard
          if guard.enabled && scenario.middles.exists(middle =>
            middle.enabled &&
              sqlEqual(guard.joinKey, middle.joinKey) &&
              scenario.leaves.exists(leaf =>
                leaf.enabled && sqlEqual(middle.leafKey, leaf.key)
              )
          ) => guard.value
    }

  private def nativeTwoDiamondGuardValues(
      rows: TwoDiamondRows
  ): Vector[Option[Int]] =
    for {
      r0 <- rows.r0
      r1 <- rows.r1
      if sqlEqual(r0.s, r1.s)
      r2 <- rows.r2
      if sqlEqual(r0.d, r2.s)
      r3 <- rows.r3
      if sqlEqual(r1.d, r3.s) && sqlEqual(r2.d, r3.d)
      r4 <- rows.r4
      if sqlEqual(r3.d, r4.s)
      r5 <- rows.r5
      if sqlEqual(r4.s, r5.s)
      r6 <- rows.r6
      if sqlEqual(r4.d, r6.s) && sqlEqual(r5.d, r6.d)
    } yield r0.s

  private def stagedTwoDiamondGuardValues(
      rows: TwoDiamondRows
  ): Vector[Option[Int]] = {
    val supportedR4 = rows.r4.filter(r4 =>
      rows.r5.exists(r5 =>
        sqlEqual(r4.s, r5.s) && rows.r6.exists(r6 =>
          sqlEqual(r4.d, r6.s) && sqlEqual(r5.d, r6.d)
        )
      )
    ).distinct
    val supportedR3 = rows.r3.filter(r3 =>
      supportedR4.exists(r4 => sqlEqual(r3.d, r4.s))
    ).distinct
    rows.r0.collect {
      case r0
          if rows.r1.exists(r1 =>
            sqlEqual(r0.s, r1.s) && rows.r2.exists(r2 =>
              sqlEqual(r0.d, r2.s) && supportedR3.exists(r3 =>
                sqlEqual(r1.d, r3.s) && sqlEqual(r2.d, r3.d)
              )
            )
          ) => r0.s
    }
  }

  /** Pure oracle for q_hto rank 2's h0+h2 root factor after both passes. */
  private def fullyReducedRankTwoGuardFactorValues(
      rows: TwoDiamondRows
  ): Vector[Option[Int]] =
    for {
      r0 <- rows.r0
      r2 <- rows.r2
      if sqlEqual(r0.d, r2.s)
      if rows.r1.exists(r1 =>
        sqlEqual(r0.s, r1.s) && rows.r3.exists(r3 =>
          sqlEqual(r1.d, r3.s) && sqlEqual(r2.d, r3.d)
        )
      )
      if rows.r4.exists(r4 =>
        sqlEqual(r2.d, r4.s) && rows.r5.exists(r5 =>
          sqlEqual(r4.s, r5.s) && rows.r6.exists(r6 =>
            sqlEqual(r4.d, r6.s) && sqlEqual(r5.d, r6.d)
          )
        )
      )
    } yield r0.s

  private def extremumValue(
      values: Vector[Option[Int]],
      minimum: Boolean
  ): Option[Int] =
    values.flatten.reduceOption { (left, right) =>
      if (minimum) math.min(left, right) else math.max(left, right)
    }
}
