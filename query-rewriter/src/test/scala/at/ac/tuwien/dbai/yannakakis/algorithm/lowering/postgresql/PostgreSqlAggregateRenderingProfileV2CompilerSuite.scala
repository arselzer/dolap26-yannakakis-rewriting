package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  CalciteQueryAnalyzer,
  CalciteSqlPlanner
}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlAggregateRenderingProfileV2CompilerSuite
    extends munit.FunSuite {
  import PostgreSqlAggregateLeafMessageProfile._
  import PostgreSqlAggregateOneBagTerminalProfile._
  import PostgreSqlAggregateReductionFinalizationProfile._
  import PostgreSqlFactorBoundaryProjectionProfile._
  import PostgreSqlReductionSupportProfile._

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    Vector("v2_e0", "v2_e1", "v2_e2", "v2_e3").foreach { name =>
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
    "FROM v2_e0 AS e0 " +
      "JOIN v2_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN v2_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN v2_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key"

  private val acyclicChain =
    "FROM v2_e0 AS e0 " +
      "JOIN v2_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN v2_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN v2_e3 AS e3 ON e2.right_key = e3.left_key"

  private val oneBag =
    "FROM v2_e0 AS e0 JOIN v2_e1 AS e1 " +
      "ON e0.left_key = e1.left_key AND e0.right_key = e1.right_key"

  private val compactBase = PostgreSqlAggregateRenderingProfile
    .create(LegacyNestedV1, CompactLocalStateV2, DirectExistsV2)
    .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)

  test("the v2 baseline wrapper preserves frozen v1 SQL trace and counters") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val legacyProfile = PostgreSqlAggregateRenderingProfile.LegacyV1
    val legacy = compileV1(prepared, legacyProfile)
    val baseline = compileV2(
      prepared,
      PostgreSqlAggregateRenderingProfileV2
        .baseline(legacyProfile)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )

    assertEquals(baseline.rendered, legacy.rendered)
    assertEquals(baseline.rendered.sql, legacy.rendered.sql)
    assertEquals(baseline.compiled.trace, legacy.compiled.trace)
    assertEquals(baseline.compiled.statistics, legacy.compiled.statistics)
    assertEquals(
      baseline.compiled.bags.map(_.physicalProjection),
      legacy.compiled.bags.map(_.physicalProjection)
    )
    assert(baseline.compiled.aggregateRendererV2Trace.requestedProfile.isBaseline)
    assertEquals(
      baseline.compiled.aggregateRendererV2Trace.elidedLogicalMessageOrdinals,
      Set.empty[Int]
    )
    assertEquals(
      baseline.compiled.aggregateRendererV2Trace.factorProjections.map(
        _.structuralVariables
      ),
      baseline.compiled.bags.map(_.factor.vertices)
    )
    assertEquals(legacy.rendered.sql.length, 3409)
    assertEquals(
      sha256(legacy.rendered.sql),
      "a686c570dacc82bd3517a0f9dc79886863f3bf8835633955ea453d860dacdbdd"
    )
  }

  test("independent factor-demand oracle covers global, G1, and U1 terminals") {
    Vector(
      "global" -> s"SELECT COUNT(*) AS n $cycle",
      "G1" ->
        (s"SELECT e0.left_key AS g, COUNT(*) AS n $cycle " +
          "GROUP BY e0.left_key"),
      "U1" ->
        (s"SELECT e0.left_key AS g0, e2.left_key AS g1, COUNT(*) AS n $cycle " +
          "GROUP BY e0.left_key, e2.left_key")
    ).foreach { case (label, sql) =>
      val prepared = prepare(sql, widthBound = 2)
      val fixture = compileV2(
        prepared,
        profile(DemandedStructuralColumnsV2, DerivedSelectV1)
      )
      assertIndependentDemand(
        prepared,
        fixture,
        profile(DemandedStructuralColumnsV2, DerivedSelectV1),
        clues(label)
      )
    }
  }

  test("demanded structural columns are omitted from the emitted initial-factor SELECT") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val full = compileV2(
      prepared,
      profile(FullFactorVerticesV1, DerivedSelectV1)
    )
    val projected = compileV2(
      prepared,
      profile(DemandedStructuralColumnsV2, DerivedSelectV1)
    )
    val fullByBag = full.compiled.bags.iterator
      .map(bag => bag.factor.bagId -> bag)
      .toMap
    val fullTraceByBag = full.compiled.trace.bags.iterator
      .map(trace => trace.bagId -> trace)
      .toMap
    val projectedTraceByBag = projected.rendered.trace.bags.iterator
      .map(trace => trace.bagId -> trace)
      .toMap
    val removed = projected.compiled.bags.flatMap { bag =>
      bag.factor.vertices.filterNot(
        bag.physicalProjection.structuralVariables.contains
      ).map(bag.factor.bagId -> _)
    }

    assert(removed.nonEmpty, clues(projected.compiled.bags))
    removed.foreach { case (bagId, variableId) =>
      val projectedBody = initialFactorDefinition(
        projected.rendered.sql,
        projectedTraceByBag(bagId).initialFactorCte
      )
      val fullBody = initialFactorDefinition(
        full.rendered.sql,
        fullTraceByBag(bagId).initialFactorCte
      )
      val alias = s" AS \"${PostgreSqlNames.variable(variableId)}\""
      assert(
        fullBody.contains(alias),
        clues(bagId, variableId, fullByBag(bagId), fullBody)
      )
      assert(
        !projectedBody.contains(alias),
        clues(bagId, variableId, projectedBody)
      )
    }
  }

  test("width-1 acyclic global G1 and U1 retain multi-factor frequency paths") {
    Vector(
      "global" -> s"SELECT COUNT(*) AS n $acyclicChain",
      "G1" ->
        (s"SELECT e0.right_key AS g, COUNT(*) AS n $acyclicChain " +
          "GROUP BY e0.right_key"),
      "U1" ->
        (s"SELECT e0.right_key AS g0, e2.right_key AS g1, COUNT(*) AS n " +
          acyclicChain + " GROUP BY e0.right_key, e2.right_key")
    ).foreach { case (label, sql) =>
      val prepared = prepare(sql, widthBound = 1)
      assert(prepared.plan.factors.size > 1, clues(label, prepared.plan))
      assert(prepared.plan.arcs.nonEmpty, clues(label, prepared.plan))

      val projected = compileV2(
        prepared,
        profile(DemandedStructuralColumnsV2, DerivedSelectV1)
      )
      val direct = compileV2(
        prepared,
        profile(DemandedStructuralColumnsV2, ExactDirectFactorV2)
      )
      assertIndependentDemand(
        prepared,
        projected,
        profile(DemandedStructuralColumnsV2, DerivedSelectV1),
        clues(label)
      )
      assertIndependentDemand(
        prepared,
        direct,
        profile(DemandedStructuralColumnsV2, ExactDirectFactorV2),
        clues(label)
      )
      assert(
        direct.compiled.aggregateRendererV2Trace.elidedLogicalMessageOrdinals.nonEmpty,
        clues(label, direct.compiled.terminal, direct.rendered.sql)
      )
      assert(
        direct.rendered.sql.length < projected.rendered.sql.length,
        clues(label)
      )
    }
  }

  test("eligible frequency leaves directly reference exact compact factors") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val projected = compileV2(
      prepared,
      profile(DemandedStructuralColumnsV2, DerivedSelectV1)
    )
    val direct = compileV2(
      prepared,
      profile(DemandedStructuralColumnsV2, ExactDirectFactorV2)
    )
    val frequency = direct.compiled.terminal match {
      case CompiledPostgreSqlTerminal.GlobalCountStar(
            _,
            CompiledPostgreSqlTerminal.GlobalCountStrategy
              .FrequencyPropagation(plan)
          ) => plan
      case other => fail(s"expected frequency terminal, found $other")
    }
    val messageByOrdinal = frequency.messages.iterator
      .map(message => message.ordinal -> message)
      .toMap
    val traceByBag = direct.compiled.trace.bags.iterator
      .map(trace => trace.bagId -> trace)
      .toMap
    val elided = direct.compiled.aggregateRendererV2Trace
      .elidedLogicalMessageOrdinals

    assert(elided.nonEmpty, clues(frequency.messages, direct.rendered.sql))
    assert(elided.subsetOf(messageByOrdinal.keySet))
    elided.foreach { ordinal =>
      val message = messageByOrdinal(ordinal)
      assertEquals(message.inputMessageOrdinals, Vector.empty)
      assert(!message.requiresRegrouping)
      val cte = traceByBag(message.source).finalFactorCte
      val alias = PostgreSqlNames.countMessage(message.source)
      assert(
        direct.rendered.sql.contains(s"\"$cte\" AS \"$alias\""),
        clues(message, direct.rendered.sql)
      )
      assert(
        !direct.rendered.sql.contains(
          s"FROM \"$cte\" AS \"${PostgreSqlNames.countFactor(message.source)}\""
        ),
        clues(message, direct.rendered.sql)
      )
    }
    val removedTerms = elided.iterator.map { ordinal =>
      messageByOrdinal(ordinal).separator.size.toLong + 1L
    }.sum
    assertEquals(
      projected.rendered.statistics.renderedTermCount -
        direct.rendered.statistics.renderedTermCount,
      removedTerms
    )
    assertEquals(
      direct.rendered.statistics.cteCount,
      projected.rendered.statistics.cteCount
    )
    assertEquals(direct.compiled.terminal, projected.compiled.terminal)
    assert(direct.rendered.sql.length < projected.rendered.sql.length)
  }

  test("U1 leaf elision never removes a regrouping or internal message") {
    val prepared = prepare(
      "SELECT e0.left_key AS g0, e2.left_key AS g1, COUNT(*) AS n " +
        cycle + " GROUP BY e0.left_key, e2.left_key",
      widthBound = 2
    )
    val projected = compileV2(
      prepared,
      profile(DemandedStructuralColumnsV2, DerivedSelectV1)
    )
    val fixture = compileV2(
      prepared,
      profile(DemandedStructuralColumnsV2, ExactDirectFactorV2)
    )
    val terminal = fixture.compiled.terminal
      .asInstanceOf[CompiledPostgreSqlTerminal.DistributedGroupedCountStar]
    val byOrdinal = terminal.propagationPlan.messages.iterator
      .map(message => message.ordinal -> message)
      .toMap

    assert(
      fixture.compiled.aggregateRendererV2Trace.elidedLogicalMessageOrdinals.nonEmpty,
      clues(terminal.propagationPlan.messages, fixture.rendered.sql)
    )
    fixture.compiled.aggregateRendererV2Trace.elidedLogicalMessageOrdinals
      .foreach { ordinal =>
        val message = byOrdinal(ordinal)
        assertEquals(message.inputMessageOrdinals, Vector.empty)
        assert(!message.requiresRegrouping)
        assert(message.carriedKeys.forall(
          _.isInstanceOf[PostgreSqlAggregatePropagation.AggregateKey.Structural]
        ))
      }
    terminal.propagationPlan.messages
      .filter(message =>
        message.requiresRegrouping || message.inputMessageOrdinals.nonEmpty
      )
      .foreach(message =>
        assert(
          !fixture.compiled.aggregateRendererV2Trace.elidedLogicalMessageOrdinals(
            message.ordinal
          )
        )
      )
    val removedTerms = fixture.compiled.aggregateRendererV2Trace
      .elidedLogicalMessageOrdinals.iterator
      .map { ordinal =>
        val message = byOrdinal(ordinal)
        message.separator.size.toLong + message.carriedKeys.size.toLong + 1L
      }
      .sum
    assertEquals(
      projected.rendered.statistics.renderedTermCount -
        fixture.rendered.statistics.renderedTermCount,
      removedTerms
    )

    val required = fixture.rendered.statistics.renderedTermCount
    val atBoundary = compileV2(
      prepared,
      profile(DemandedStructuralColumnsV2, ExactDirectFactorV2),
      renderingLimits(required)
    )
    assertEquals(atBoundary.rendered.statistics.renderedTermCount, required)
    val rejected = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV2(
        prepared.query,
        prepared.input,
        prepared.plan,
        renderingLimits(required - 1L),
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        profile(DemandedStructuralColumnsV2, ExactDirectFactorV2)
      )
    rejected match {
      case Left(problems) =>
        assert(problems.exists {
          case PostgreSqlRenderingError.ResourceLimitExceeded(
                PostgreSqlRenderingResource.RenderedTerms,
                actual,
                limit
              ) => actual == BigInt(required) && limit == required - 1L
          case _ => false
        }, clues(problems))
      case Right(_) => fail("expected one-below U1 rendered-term rejection")
    }
  }

  test("a zero-demand one-bag factor emits one private multiplicity marker") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $oneBag", widthBound = 1)
    val fixture = compileV2(
      prepared,
      profile(DemandedStructuralColumnsV2, DerivedSelectV1)
    )
    val bag = fixture.compiled.bags.head
    val marker = PostgreSqlNames.factorUnit(bag.factor.bagId)

    assertEquals(bag.physicalProjection.structuralVariables, Vector.empty)
    assertEquals(bag.physicalProjection.occurrenceColumns, Vector.empty)
    assertEquals(bag.physicalProjection.unitColumn, Some(marker))
    assertEquals(fixture.rendered.trace.bags.head.variableColumns, Vector.empty)
    assertEquals(
      fixture.compiled.aggregateRendererV2Trace.factorProjections.head.unitColumn,
      Some(marker)
    )
    assertEquals(occurrences(fixture.rendered.sql, s"AS \"$marker\""), 1)
    assert(
      fixture.rendered.sql.contains(
        s"CAST(1 AS SMALLINT) AS \"$marker\""
      ),
      clues(fixture.rendered.sql)
    )
  }

  test("renderer-v2 resource accounting passes exactly at term and SQL limits") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val selectedProfile = profile(
      DemandedStructuralColumnsV2,
      ExactDirectFactorV2
    )
    val ordinary = compileV2(prepared, selectedProfile)
    val required = ordinary.rendered.statistics.renderedTermCount
    assert(required > 1L)

    val exact = renderingLimits(required)
    val atBoundary = compileV2(prepared, selectedProfile, exact)
    assertEquals(atBoundary.rendered.statistics.renderedTermCount, required)

    val rejected = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV2(
        prepared.query,
        prepared.input,
        prepared.plan,
        renderingLimits(required - 1L),
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        selectedProfile
      )
    rejected match {
      case Left(problems) =>
        assert(problems.exists {
          case PostgreSqlRenderingError.ResourceLimitExceeded(
                PostgreSqlRenderingResource.RenderedTerms,
                actual,
                limit
              ) => actual == BigInt(required) && limit == required - 1L
          case _ => false
        }, clues(problems))
      case Right(_) => fail("expected one-below rendered-term rejection")
    }

    val characters = ordinary.rendered.statistics.sqlCharacterCount
    val exactCharacters = compileV2(
      prepared,
      selectedProfile,
      renderingLimits(maxSqlCharacters = characters)
    )
    assertEquals(
      exactCharacters.rendered.statistics.sqlCharacterCount,
      characters
    )
    val rejectedCharacters = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV2(
        prepared.query,
        prepared.input,
        prepared.plan,
        renderingLimits(maxSqlCharacters = characters - 1L),
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        selectedProfile
      )
    rejectedCharacters match {
      case Left(problems) =>
        assert(problems.exists {
          case PostgreSqlRenderingError.ResourceLimitExceeded(
                PostgreSqlRenderingResource.SqlCharacters,
                actual,
                limit
              ) => actual == BigInt(characters) && limit == characters - 1L
          case _ => false
        }, clues(problems))
      case Right(_) => fail("expected one-below SQL-character rejection")
    }
  }

  test("optimized axes fail closed outside the compact full aggregate scope") {
    val count = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val optimized = profile(DemandedStructuralColumnsV2, DerivedSelectV1)
    val bottomUp = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV2(
        count.query,
        count.input,
        count.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.BottomUpOnly,
        optimized
      )
    assert(bottomUp.isLeft, clues(bottomUp))

    val legacyEnumerationSql = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV2(
        count.query,
        count.input,
        count.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        optimized,
        PostgreSqlEnumerationRenderingProfile.LegacyV1
      )
    assert(legacyEnumerationSql.isLeft, clues(legacyEnumerationSql))

    val enumeration = prepare(
      s"SELECT e0.payload AS p $cycle",
      widthBound = 2
    )
    val projectedEnumeration = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV2(
        enumeration.query,
        enumeration.input,
        enumeration.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        optimized
      )
    assert(projectedEnumeration.isLeft, clues(projectedEnumeration))
  }

  private final case class RenderedFixture(
      compiled: CompiledPostgreSqlRendering,
      rendered: PostgreSqlRenderedQuery
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
    assert(report.isSupported, clues(sql, report.diagnostics))
    report
  }

  private def compileV1(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      profile: PostgreSqlAggregateRenderingProfile
  ): RenderedFixture = {
    val result = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        profile
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    RenderedFixture(result._1, result._2)
  }

  /**
    * Independent of the v2 demand pass: this uses only the selected logical
    * tree and the terminal annotation model. Full reduction consumes every
    * incident tree separator; compact finalization consumes the same local
    * grouping carried by the terminal model.
    */
  private def assertIndependentDemand(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      fixture: RenderedFixture,
      expectedProfile: PostgreSqlAggregateRenderingProfileV2,
      clue: munit.Clue[_]
  ): Unit = {
    val localGroupingByBag: Map[BagId, Set[VariableId]] =
      fixture.compiled.terminal match {
        case CompiledPostgreSqlTerminal.GlobalCountStar(
              _,
              CompiledPostgreSqlTerminal.GlobalCountStrategy
                .FrequencyPropagation(plan)
            ) =>
          plan.bags.iterator
            .map(bag => bag.bagId -> bag.localGrouping.toSet)
            .toMap
        case count: CompiledPostgreSqlTerminal.GroupedCountStar =>
          count.frequencyPlan.bags.iterator
            .map(bag => bag.bagId -> bag.localGrouping.toSet)
            .toMap
        case count: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
          count.propagationPlan.bags.iterator
            .map(bag => bag.bagId -> bag.localGroupingVariables.toSet)
            .toMap
        case other => fail(s"unsupported demand-oracle terminal: $other")
      }
    val publicTraceByBag = fixture.rendered.trace.bags.iterator
      .map(trace => trace.bagId -> trace)
      .toMap
    val privateTraceByBag = fixture.compiled.aggregateRendererV2Trace
      .factorProjections.iterator
      .map(trace => trace.bagId -> trace)
      .toMap

    fixture.compiled.bags.foreach { bag =>
      val bagId = bag.factor.bagId
      val separatorDemand = prepared.plan.arcs.iterator
        .filter(arc => arc.parent == bagId || arc.child == bagId)
        .flatMap(_.separator.iterator)
        .toSet
      val expectedSet = separatorDemand ++
        localGroupingByBag.getOrElse(bagId, Set.empty)
      val expected = bag.factor.vertices.filter(expectedSet)

      assertEquals(
        bag.physicalProjection.structuralVariables,
        expected,
        clues(clue, bagId, separatorDemand, localGroupingByBag)
      )
      assertEquals(
        publicTraceByBag(bagId).variableColumns.map(_.variableId),
        expected,
        clue
      )
      val privateTrace = privateTraceByBag(bagId)
      assertEquals(privateTrace.structuralVariables, expected, clue)
      assertEquals(
        privateTrace.occurrenceColumns,
        publicTraceByBag(bagId).occurrenceColumns,
        clue
      )
      assertEquals(
        privateTrace.unitColumn,
        bag.physicalProjection.unitColumn,
        clue
      )
    }
    assertEquals(
      fixture.compiled.aggregateRendererV2Trace.requestedProfile,
      expectedProfile,
      clue
    )
  }

  private def compileV2(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      profile: PostgreSqlAggregateRenderingProfileV2,
      limits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default
  ): RenderedFixture = {
    val result = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV2(
        prepared.query,
        prepared.input,
        prepared.plan,
        limits,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        profile
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    RenderedFixture(result._1, result._2)
  }

  private def profile(
      factorBoundary: PostgreSqlFactorBoundaryProjectionProfile,
      leafMessage: PostgreSqlAggregateLeafMessageProfile
  ): PostgreSqlAggregateRenderingProfileV2 =
    PostgreSqlAggregateRenderingProfileV2
      .create(compactBase, factorBoundary, leafMessage)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)

  private def renderingLimits(
      maxRenderedTerms: Long = PostgreSqlRenderingLimits.default.maxRenderedTerms,
      maxSqlCharacters: Long = PostgreSqlRenderingLimits.default.maxSqlCharacters
  ): PostgreSqlRenderingLimits =
    PostgreSqlRenderingLimits
      .create(
        maxRenderedTerms = maxRenderedTerms,
        maxSqlCharacters = maxSqlCharacters
      )
      .fold(problems => fail(problems.mkString("; ")), identity)

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

  private def initialFactorDefinition(sql: String, cteName: String): String = {
    val start = sql.indexOf(s"\"$cteName\"")
    assert(start >= 0, clues(cteName, sql))
    val select = sql.indexOf("    SELECT\n", start)
    assert(select >= 0, clues(cteName, sql))
    val end = sql.indexOf("\n  )", select)
    assert(end >= 0, clues(cteName, sql))
    sql.substring(select, end)
  }

  private def sha256(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
