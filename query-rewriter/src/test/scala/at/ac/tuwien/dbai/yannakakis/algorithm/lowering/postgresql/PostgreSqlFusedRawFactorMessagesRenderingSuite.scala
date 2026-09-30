package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
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

/**
  * Contract tests for the additive, development-only raw-factor COUNT message
  * realization. The public/default and renderer-v1/v2 paths remain controls.
  */
class PostgreSqlFusedRawFactorMessagesRenderingSuite extends munit.FunSuite {
  import PostgreSqlGlobalCountMessageRealizationProfile._

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    Vector("v2_e0", "v2_e1", "v2_e2", "v2_e3").foreach { name =>
      schema.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("row_id", SqlTypeName.INTEGER)
              .nullable(false)
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
    Vector(
      "branch_l" -> Vector("a"),
      "branch_x" -> Vector("a", "b"),
      "branch_y" -> Vector("b", "c"),
      "branch_d" -> Vector("c", "d"),
      "branch_dl" -> Vector("d"),
      "branch_e" -> Vector("c", "e"),
      "branch_el" -> Vector("e")
    ).foreach { case (name, columns) =>
      schema.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType = {
            val builder = factory.builder()
            columns.foreach(column =>
              builder
                .add(column, SqlTypeName.INTEGER)
                .nullable(true)
            )
            builder.build()
          }
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

  private val chain =
    "FROM v2_e0 AS e0 " +
      "JOIN v2_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN v2_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN v2_e3 AS e3 ON e2.right_key = e3.left_key"

  private val oneBag =
    "FROM v2_e0 AS e0 JOIN v2_e1 AS e1 " +
      "ON e0.left_key = e1.left_key AND e0.right_key = e1.right_key"

  // The two depth-two arms force an actual branching width-one join tree.
  private val branch =
    "FROM branch_l AS l " +
      "JOIN branch_x AS x ON l.a = x.a " +
      "JOIN branch_y AS y ON x.b = y.b " +
      "JOIN branch_d AS d ON y.c = d.c " +
      "JOIN branch_dl AS dl ON d.d = dl.d " +
      "JOIN branch_e AS e ON y.c = e.c " +
      "JOIN branch_el AS el ON e.e = el.e"

  private val legacyProfile = PostgreSqlAggregateRenderingProfile.LegacyV1
  private val rendererV2Baseline = PostgreSqlAggregateRenderingProfileV2
    .baseline(legacyProfile)
    .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
  private val rendererV3Baseline = PostgreSqlAggregateRenderingProfileV3
    .baseline(rendererV2Baseline)
    .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
  private val fusedProfile = PostgreSqlAggregateRenderingProfileV3
    .fusedRawFactorMessages(rendererV2Baseline)
    .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)

  test("the fused v3 profile has one frozen canonical identity") {
    val expectedPreimage =
      "41:postgresql_aggregate_rendering_profile_v3" +
        "41:postgresql_aggregate_rendering_profile_v2" +
        "71:sha256:5ed7f68dde4c769158abba73bb012fb80b21fd7f52653a08bba97b6e84fb1400" +
        "23:full-factor-vertices-v1" +
        "17:derived-select-v1" +
        "28:fused-raw-factor-messages-v1"

    assertEquals(
      rendererV2Baseline.id,
      "sha256:5ed7f68dde4c769158abba73bb012fb80b21fd7f52653a08bba97b6e84fb1400"
    )
    assertEquals(
      PostgreSqlAggregateRenderingProfileV3.canonicalPreimage(fusedProfile),
      expectedPreimage
    )
    assertEquals(
      fusedProfile.id,
      "sha256:5dc5ea030ff0160b630948547ef5af2d1071aea65febc20a90781a10ce5d0835"
    )
  }

  test("default v1 v2 and v3-baseline routes preserve frozen SQL bytes") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)

    YannakakisReductionMode.Values.foreach { mode =>
      val ordinary = compileDefault(prepared, mode)
      val v1 = compileV1(prepared, mode)
      val v2 = compileV2(prepared, mode)
      val v3 = compileV3(prepared, mode, rendererV3Baseline)

      Vector(v1, v2, v3).foreach { control =>
        assertEquals(control.rendered, ordinary.rendered, clues(mode))
        assertEquals(control.compiled.trace, ordinary.compiled.trace, clues(mode))
        assertEquals(
          control.compiled.statistics,
          ordinary.compiled.statistics,
          clues(mode)
        )
        assertEquals(control.compiled.terminal, ordinary.compiled.terminal, clues(mode))
      }
      assertEquals(v2.compiled.aggregateRenderingProfileV3, rendererV3Baseline)
      assertEquals(v3.compiled.aggregateRenderingProfileV3, rendererV3Baseline)
    }

    val frozenFull = compileV1(
      prepared,
      YannakakisReductionMode.FullTwoPass
    ).rendered.sql
    assertEquals(frozenFull.length, 3409)
    assertEquals(
      sha256(frozenFull),
      "a686c570dacc82bd3517a0f9dc79886863f3bf8835633955ea453d860dacdbdd"
    )
  }

  test("fused raw messages are a distinct NoReduction frequency realization") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val fixture = compileV3(
      prepared,
      YannakakisReductionMode.NoReduction,
      fusedProfile
    )
    val frequency = frequencyPlan(fixture.compiled)
    val reduction = fixture.rendered.trace.reduction

    assertEquals(fusedProfile.axisIds, Vector(FusedRawFactorMessagesV1.id))
    assert(fusedProfile.usesFusedRawFactorMessages)
    assert(!rendererV3Baseline.usesFusedRawFactorMessages)
    assertNotEquals(fusedProfile.id, rendererV3Baseline.id)
    assertEquals(fixture.compiled.aggregateRenderingProfileV3, fusedProfile)
    assertEquals(frequency.messages.size, prepared.plan.arcs.size)
    assertEquals(fixture.compiled.reductionCtes, Vector.empty)
    assertEquals(reduction.mode, YannakakisReductionMode.NoReduction)
    assertEquals(reduction.selectedBottomUpStepCount, 0)
    assertEquals(reduction.selectedTopDownStepCount, 0)
    assertEquals(reduction.physicalReductionCteCount, 0)
    assertEquals(
      fixture.rendered.output.map(column =>
        (column.ordinal, column.name, column.dataType, column.nullable)
      ),
      Vector((0, "n", "BIGINT NOT NULL", false))
    )
    assert(!fixture.rendered.sql.matches("(?s).*yr_f_b[0-9]+_(bu|td)[0-9]+.*"))
    assert(!fixture.rendered.sql.contains("\"yr_ci_b"), clues(fixture.rendered.sql))
  }

  test("fused C4 resource accounting is exact at both hard limits") {
    val prepared = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    val ordinary = compileV3(
      prepared,
      YannakakisReductionMode.NoReduction,
      fusedProfile
    )
    val frequency = frequencyPlan(ordinary.compiled)
    val separatorTerms = frequency.messages.iterator
      .map(_.separator.size.toLong)
      .sum

    assertEquals(frequency.messages.size, 1)
    assertEquals(separatorTerms, 2L)
    assertEquals(ordinary.rendered.statistics.expandedScanCount, 4L)
    assertEquals(ordinary.rendered.statistics.renderedTermCount, 22L)
    assertEquals(ordinary.rendered.statistics.cteCount, 2L)
    assertEquals(
      ordinary.rendered.statistics.sqlCharacterCount,
      ordinary.rendered.sql.length.toLong
    )

    val terms = ordinary.rendered.statistics.renderedTermCount
    val exactTerms = compileV3(
      prepared,
      YannakakisReductionMode.NoReduction,
      fusedProfile,
      renderingLimits(maxRenderedTerms = terms)
    )
    assertEquals(exactTerms.rendered.statistics.renderedTermCount, terms)
    assertResourceRejection(
      compileV3Attempt(
        prepared,
        YannakakisReductionMode.NoReduction,
        fusedProfile,
        renderingLimits(maxRenderedTerms = terms - 1L)
      ),
      PostgreSqlRenderingResource.RenderedTerms,
      terms,
      terms - 1L
    )

    val characters = ordinary.rendered.statistics.sqlCharacterCount
    val exactCharacters = compileV3(
      prepared,
      YannakakisReductionMode.NoReduction,
      fusedProfile,
      renderingLimits(maxSqlCharacters = characters)
    )
    assertEquals(
      exactCharacters.rendered.statistics.sqlCharacterCount,
      characters
    )
    assertResourceRejection(
      compileV3Attempt(
        prepared,
        YannakakisReductionMode.NoReduction,
        fusedProfile,
        renderingLimits(maxSqlCharacters = characters - 1L)
      ),
      PostgreSqlRenderingResource.SqlCharacters,
      characters,
      characters - 1L
    )
  }

  test("one bag uses the raw factor directly without messages or reductions") {
    val fixture = compileFused(
      s"SELECT COUNT(*) AS n $oneBag",
      widthBound = 1
    )
    val frequency = frequencyPlan(fixture.compiled)
    val trace = fixture.rendered.trace.bags.head
    val rawSource =
      s"FROM \"${trace.initialFactorCte}\" AS \"${PostgreSqlNames.countFactor(trace.bagId)}\""

    assertEquals(fixture.compiled.plan.factors.size, 1)
    assertEquals(frequency.bags.size, 1)
    assertEquals(frequency.messages, Vector.empty)
    assertEquals(fixture.compiled.reductionCtes, Vector.empty)
    assertEquals(occurrences(fixture.rendered.sql, rawSource), 1)
    assert(fixture.rendered.sql.contains("COUNT(*) AS \"n\""))
    assert(!fixture.rendered.sql.contains("SUM("), clues(fixture.rendered.sql))
    assert(!fixture.rendered.sql.contains("yr_cm_b"), clues(fixture.rendered.sql))
  }

  test("chain branch and C4 join raw factors to child messages before grouping") {
    val fixtures = Vector(
      "chain" -> compileFused(
        s"SELECT COUNT(*) AS n $chain",
        widthBound = 1
      ),
      "branch" -> compileFused(
        s"SELECT COUNT(*) AS n $branch",
        widthBound = 1
      ),
      "C4" -> compileFused(
        s"SELECT COUNT(*) AS n $cycle",
        widthBound = 2
      )
    )

    val chainDegrees = degrees(fixtures(0)._2.compiled)
    val branchDegrees = degrees(fixtures(1)._2.compiled)
    assert(chainDegrees.values.forall(_ <= 2), clues(chainDegrees))
    assert(branchDegrees.values.exists(_ >= 3), clues(branchDegrees))

    var observedInternalGrouping = false
    fixtures.foreach { case (label, fixture) =>
      val frequency = frequencyPlan(fixture.compiled)
      val terminal = fixture.rendered.sql
      val traceByBag = fixture.rendered.trace.bags.iterator
        .map(trace => trace.bagId -> trace)
        .toMap

      assertEquals(fixture.compiled.reductionCtes, Vector.empty, clues(label))
      assertEquals(frequency.messages.size, fixture.compiled.plan.arcs.size)
      traceByBag.values.foreach { trace =>
        val rawSource =
          s"FROM \"${trace.initialFactorCte}\" AS \"${PostgreSqlNames.countFactor(trace.bagId)}\""
        assertEquals(
          occurrences(terminal, rawSource),
          1,
          clues(label, trace.bagId, terminal)
        )
      }

      val messageByOrdinal = frequency.messages.iterator
        .map(message => message.ordinal -> message)
        .toMap
      frequency.messages
        .filter(message =>
          message.inputMessageOrdinals.nonEmpty &&
            message.separator.nonEmpty
        )
        .foreach { message =>
          observedInternalGrouping = true
          val factorAlias = PostgreSqlNames.countFactor(message.source)
          val factorMarker =
            s"FROM \"${traceByBag(message.source).initialFactorCte}\" AS \"$factorAlias\""
          val factorPosition = terminal.indexOf(factorMarker)
          val groupPosition = terminal.indexOf(
            s"\nGROUP BY\n  \"$factorAlias\".",
            factorPosition
          )
          assert(factorPosition >= 0, clues(label, message, terminal))
          assert(groupPosition > factorPosition, clues(label, message, terminal))

          message.inputMessageOrdinals.foreach { ordinal =>
            val child = messageByOrdinal(ordinal)
            val childAlias = PostgreSqlNames.countMessage(child.source)
            val childJoinPosition = terminal.indexOf(
              s") AS \"$childAlias\"",
              factorPosition
            )
            assert(
              childJoinPosition > factorPosition &&
                childJoinPosition < groupPosition,
              clues(label, message, child, terminal)
            )
          }
        }
    }
    assert(observedInternalGrouping, clues(fixtures.map(_._1)))
  }

  test("the fused action fails closed outside global COUNT NoReduction") {
    val global = prepare(s"SELECT COUNT(*) AS n $cycle", widthBound = 2)
    Vector(
      YannakakisReductionMode.FullTwoPass,
      YannakakisReductionMode.BottomUpOnly
    ).foreach { mode =>
      assert(
        compileV3Attempt(global, mode, fusedProfile).isLeft,
        clues(mode)
      )
    }

    val projection = prepare(
      s"SELECT e0.payload AS p $cycle",
      widthBound = 2
    )
    assert(
      compileV3Attempt(
        projection,
        YannakakisReductionMode.NoReduction,
        fusedProfile
      ).isLeft
    )

    val legacyEnumeration = compileV3Attempt(
      global,
      YannakakisReductionMode.NoReduction,
      fusedProfile,
      enumerationProfile = PostgreSqlEnumerationRenderingProfile.LegacyV1
    )
    assertModelRejection(
      legacyEnumeration,
      "fused raw-factor COUNT messages require statistics-safe-v2 enumeration SQL"
    )

    val compactBase = PostgreSqlAggregateRenderingProfile
      .create(
        PostgreSqlAggregateOneBagTerminalProfile.LegacyNestedV1,
        PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2,
        PostgreSqlReductionSupportProfile.DistinctSeparatorSupportV1
      )
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    val optimizedBases = PostgreSqlAggregateRenderingProfileV2
      .valuesFor(compactBase)
      .filter(_.usesOptimizedAxes)
    assert(optimizedBases.nonEmpty)
    optimizedBases.foreach { optimized =>
      val rejected = PostgreSqlAggregateRenderingProfileV3
        .fusedRawFactorMessages(optimized)
      assertEquals(
        rejected.left.toOption.map(_.map(_.code)),
        Some(
          Vector(
            "aggregate-rendering-profile-v3.invalid.fused-raw-factor-compatibility"
          )
        ),
        clues(optimized)
      )
    }
  }

  private final case class RenderedFixture(
      compiled: CompiledPostgreSqlRendering,
      rendered: PostgreSqlRenderedQuery
  )

  private def compileFused(sql: String, widthBound: Int): RenderedFixture =
    compileV3(
      prepare(sql, widthBound),
      YannakakisReductionMode.NoReduction,
      fusedProfile
    )

  private def compileDefault(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      mode: YannakakisReductionMode
  ): RenderedFixture =
    result(
      PostgreSqlBagFactorRenderer.compileAndRender(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        mode
      )
    )

  private def compileV1(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      mode: YannakakisReductionMode
  ): RenderedFixture =
    result(
      PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        mode,
        legacyProfile
      )
    )

  private def compileV2(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      mode: YannakakisReductionMode
  ): RenderedFixture =
    result(
      PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfileV2(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        mode,
        rendererV2Baseline
      )
    )

  private def compileV3(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      mode: YannakakisReductionMode,
      profile: PostgreSqlAggregateRenderingProfileV3,
      limits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default,
      enumerationProfile: PostgreSqlEnumerationRenderingProfile =
        PostgreSqlEnumerationRenderingProfile.default
  ): RenderedFixture =
    result(compileV3Attempt(prepared, mode, profile, limits, enumerationProfile))

  private def compileV3Attempt(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      mode: YannakakisReductionMode,
      profile: PostgreSqlAggregateRenderingProfileV3,
      limits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default,
      enumerationProfile: PostgreSqlEnumerationRenderingProfile =
        PostgreSqlEnumerationRenderingProfile.default
  ) =
    PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfileV3(
      prepared.query,
      prepared.input,
      prepared.plan,
      limits,
      BagFactorPlanningLimits.default,
      mode,
      profile,
      enumerationProfile
    )

  private def assertResourceRejection(
      value: Either[
        Vector[PostgreSqlRenderingError],
        (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
      ],
      resource: PostgreSqlRenderingResource,
      actual: Long,
      limit: Long
  ): Unit =
    value match {
      case Left(problems) =>
        assert(
          problems.exists {
            case PostgreSqlRenderingError.ResourceLimitExceeded(
                  observedResource,
                  observedActual,
                  observedLimit
                ) =>
              observedResource == resource &&
                observedActual == BigInt(actual) &&
                observedLimit == limit
            case _ => false
          },
          clues(problems)
        )
      case Right(_) => fail(s"expected a one-below $resource rejection")
    }

  private def assertModelRejection(
      value: Either[
        Vector[PostgreSqlRenderingError],
        (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
      ],
      detail: String
  ): Unit =
    value match {
      case Left(problems) =>
        assert(
          problems.exists {
            case PostgreSqlRenderingError.ModelInvariantViolation(message) =>
              message == detail
            case _ => false
          },
          clues(problems)
        )
      case Right(_) => fail(s"expected model rejection: $detail")
    }

  private def result(
      value: Either[
        Vector[PostgreSqlRenderingError],
        (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
      ]
  ): RenderedFixture =
    value
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        pair => RenderedFixture(pair._1, pair._2)
      )

  private def frequencyPlan(
      compiled: CompiledPostgreSqlRendering
  ): PostgreSqlFrequencyPropagation.Plan =
    compiled.terminal match {
      case CompiledPostgreSqlTerminal.GlobalCountStar(
            _,
            CompiledPostgreSqlTerminal.GlobalCountStrategy
              .RawFrequencyPropagation(plan)
          ) => plan
      case other => fail(s"expected frequency-propagated global COUNT, found $other")
    }

  private def degrees(
      compiled: CompiledPostgreSqlRendering
  ): Map[at.ac.tuwien.dbai.yannakakis.core.BagId, Int] = {
    val zero = compiled.plan.factors.iterator
      .map(_.bagId -> 0)
      .toMap
    compiled.plan.arcs.foldLeft(zero) { case (current, arc) =>
      current
        .updated(arc.parent, current(arc.parent) + 1)
        .updated(arc.child, current(arc.child) + 1)
    }
  }

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

  private def sha256(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
