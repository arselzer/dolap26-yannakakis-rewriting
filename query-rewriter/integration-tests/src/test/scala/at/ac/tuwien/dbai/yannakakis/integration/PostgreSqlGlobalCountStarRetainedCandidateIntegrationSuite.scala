package at.ac.tuwien.dbai.yannakakis.integration

import at.ac.tuwien.dbai.yannakakis.integration.{
  ExactResultMultisetV2 => ExactResultMultiset
}
import java.sql.{Connection, DriverManager, Types}

import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCandidateSelectionDecision,
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteArtifact,
  PostgreSqlRewriteResult
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlAggregateOneBagTerminalProfile,
  PostgreSqlAggregateLeafMessageProfile,
  PostgreSqlAggregateReductionFinalizationProfile,
  PostgreSqlAggregateRenderingProfile,
  PostgreSqlAggregateRenderingProfileV2,
  PostgreSqlBagFactorRenderer,
  PostgreSqlFactorBoundaryProjectionProfile,
  PostgreSqlReductionSupportProfile,
  PostgreSqlRenderingLimits
}
import at.ac.tuwien.dbai.yannakakis.integration.SyntheticCycleMultisetFixture.{
  Dataset,
  SparseC4,
  SparseC6
}
import org.testcontainers.postgresql.PostgreSQLContainer

private object PostgreSqlGlobalCountStarRetainedCandidateIntegrationSuite {
  final case class Database(
      jdbcUrl: String,
      username: String,
      password: String
  )

  final case class Scenario(
      id: String,
      dataset: Dataset,
      widthBound: Int,
      requireConnectedCover: Boolean,
      finalPredicate: Option[String],
      expectedCount: Long
  )
}

/**
  * PostgreSQL execution proof for the certified global COUNT(*) terminal.
  *
  * Every retained structural candidate is rendered in every reduction mode.
  * The sparse C4/C6 inputs deliberately contain duplicate rows, NULL join
  * keys, and nullable payloads; the empty scenario exercises the aggregate's
  * mandatory zero row without changing the cyclic decomposition.
  */
class PostgreSqlGlobalCountStarRetainedCandidateIntegrationSuite
    extends munit.FunSuite {
  import PostgreSqlGlobalCountStarRetainedCandidateIntegrationSuite.{
    Database,
    Scenario
  }

  private val ResultAlias = "result_count"
  private val PlanLimit = 10

  private val C4 = Scenario(
    id = "sparse-c4",
    dataset = SparseC4,
    widthBound = 2,
    requireConnectedCover = true,
    finalPredicate = None,
    expectedCount = SparseC4.expectedWitnessMultiplicity
  )
  private val C6 = Scenario(
    id = "sparse-c6",
    dataset = SparseC6,
    widthBound = 3,
    requireConnectedCover = true,
    finalPredicate = None,
    expectedCount = SparseC6.expectedWitnessMultiplicity
  )
  private val EmptyC4 = C4.copy(
    id = "empty-sparse-c4",
    finalPredicate = Some("\"e0\".\"row_id\" = \"e0\".\"left_key\""),
    expectedCount = 0L
  )

  private var postgres: PostgreSQLContainer = _
  private var connection: Connection = _
  private var rewriter: PostgreSqlQueryRewriter = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val database = externalDatabase.getOrElse {
      postgres = new PostgreSQLContainer("postgres:18.4-alpine")
        .withDatabaseName("global_count_star")
        .withUsername("global_count")
        .withPassword("global_count")
      postgres.start()
      Database(postgres.getJdbcUrl, postgres.getUsername, postgres.getPassword)
    }
    connection = DriverManager.getConnection(
      database.jdbcUrl,
      database.username,
      database.password
    )
    SyntheticCycleMultisetFixture.install(connection)
    rewriter = PostgreSqlQueryRewriter
      .forSchema(SyntheticCycleCalciteFixture.rootSchema())
      .fold(
        problem =>
          throw new IllegalStateException(s"${problem.code}: ${problem.message}"),
        identity
      )
  }

  override def afterAll(): Unit =
    try if (connection != null) connection.close()
    finally {
      try if (postgres != null) postgres.stop()
      finally super.afterAll()
    }

  test("C4 and C6 fixtures expose duplicates, NULL keys, and nullable payloads") {
    Vector(SparseC4, SparseC6).foreach { dataset =>
      assert(
        dataset.expectedWitnessMultiplicity > dataset.expectedDistinctWitnesses,
        clues(dataset.id)
      )
      (0 until dataset.topology.edgeCount).foreach { edgeIndex =>
        val table = SyntheticCycleMultisetFixture.tableName(dataset, edgeIndex)
        val rows = execute(s"SELECT * FROM \"$table\"")
        val nullKeys = execute(
          s"SELECT left_key, right_key FROM \"$table\" " +
            "WHERE left_key IS NULL OR right_key IS NULL"
        )
        val nullPayloads = execute(
          s"SELECT payload FROM \"$table\" WHERE payload IS NULL"
        )

        assertEquals(rows.totalMultiplicity, dataset.expectedRowsPerRelation)
        assert(rows.containsDuplicate, clues(dataset.id, edgeIndex))
        assert(!nullKeys.isEmpty, clues(dataset.id, edgeIndex))
        assert(nullKeys.containsNull, clues(dataset.id, edgeIndex))
        assert(!nullPayloads.isEmpty, clues(dataset.id, edgeIndex))
        assert(nullPayloads.containsNull, clues(dataset.id, edgeIndex))
      }
    }
  }

  test("native and every retained C4/C6 candidate agree in all reduction modes") {
    Vector(C4, C6).foreach(assertEveryRetainedCandidate)
  }

  test("empty aggregate input yields the exact zero row for every candidate and mode") {
    assertEveryRetainedCandidate(EmptyC4)
  }

  private def assertEveryRetainedCandidate(scenario: Scenario): Unit = {
    val source = countCore(scenario)
    val native = execute(source)
    assertExactCount(native, scenario.expectedCount, clues(scenario.id, "native"))

    val baseline = detailed(
      source,
      CandidatePlanSelection.StructuralHead,
      scenario,
      YannakakisReductionMode.FullTwoPass
    )
    val retained = artifact(baseline).candidateSearch.plans
    assert(retained.size > 1, clues(scenario.id, retained.map(_.rank)))
    assertEquals(retained.map(_.rank), (1 to retained.size).toVector)
    assertEquals(retained.map(_.candidateId).distinct.size, retained.size)

    retained.foreach { expectedPlan =>
      YannakakisReductionMode.Values.foreach { mode =>
        val result = detailed(
          source,
          CandidatePlanSelection.ExactCandidateId(expectedPlan.candidateId),
          scenario,
          mode
        )
        val selected = artifact(result)
        val rendered = selected.renderedQuery
        val clue = clues(scenario.id, expectedPlan.rank, expectedPlan.candidateId, mode)

        assertEquals(selected.selectedPlan, expectedPlan, clue)
        assertEquals(selected.candidateSelection.reductionMode, mode, clue)
        assertEquals(rendered.trace.reduction.mode, mode, clue)
        assertEquals(
          rendered.output.map(column =>
            (column.ordinal, column.name, column.dataType, column.nullable)
          ),
          Vector((0, ResultAlias, "BIGINT NOT NULL", false)),
          clue
        )
        assertEquals(selected.query.resultShape.id, "global-count-star", clue)
        mode match {
          case YannakakisReductionMode.FullTwoPass =>
            assert(rendered.sql.contains("COALESCE(SUM("), clue)
            assert(rendered.sql.contains(" AS BIGINT)"), clue)
            assert(
              rendered.sql.contains("CAST(COUNT(*) AS NUMERIC)"),
              clue
            )
          case YannakakisReductionMode.BottomUpOnly |
              YannakakisReductionMode.NoReduction =>
            assert(rendered.sql.contains("COUNT(*)"), clue)
            assert(!rendered.sql.contains("COALESCE(SUM("), clue)
        }
        assert(!rendered.sql.contains("\"payload\""), clue)

        val actual = execute(rewrittenStatement(result))
        assertExactCount(actual, scenario.expectedCount, clue)
        val difference = native.difference(actual)
        assert(difference.isEmpty, s"$clue failed:\n${difference.render}")
        if (mode == YannakakisReductionMode.FullTwoPass) {
          assertOptimizedExactCount(
            native,
            selected,
            legacyDirectProfile,
            scenario.expectedCount,
            clues(clue, "legacy-direct")
          )
          assertOptimizedExactCount(
            native,
            selected,
            compactDirectProfile,
            scenario.expectedCount,
            clues(clue, "compact-direct")
          )
          assertOptimizedExactCount(
            native,
            selected,
            compactDistinctProfile,
            scenario.expectedCount,
            clues(clue, "compact-distinct")
          )
          assertOptimizedExactCountV2(
            native,
            selected,
            rendererV2Profile,
            scenario.expectedCount,
            clues(clue, "renderer-v2")
          )
          if (selected.bagFactorPlan.factors.size == 1)
            assertOptimizedExactCount(
              native,
              selected,
              collapsedProfile,
              scenario.expectedCount,
              clues(clue, "collapsed")
            )
        }
      }
    }
  }

  private def assertOptimizedExactCount(
      native: ExactResultMultiset.ResultMultiset,
      source: PostgreSqlRewriteArtifact,
      profile: PostgreSqlAggregateRenderingProfile,
      expected: Long,
      clue: munit.Clue[_]
  ): Unit = {
    val sql = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfile(
        source.query,
        source.candidateInput,
        source.bagFactorPlan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        profile
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        _._2.sql
      )
    val actual = execute(sql)
    assertExactCount(actual, expected, clue)
    val difference = native.difference(actual)
    assert(difference.isEmpty, s"$clue failed:\n${difference.render}")
  }

  private def assertOptimizedExactCountV2(
      native: ExactResultMultiset.ResultMultiset,
      source: PostgreSqlRewriteArtifact,
      profile: PostgreSqlAggregateRenderingProfileV2,
      expected: Long,
      clue: munit.Clue[_]
  ): Unit = {
    val sql = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV2(
        source.query,
        source.candidateInput,
        source.bagFactorPlan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        profile
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        _._2.sql
      )
    val actual = execute(sql)
    assertExactCount(actual, expected, clue)
    val difference = native.difference(actual)
    assert(difference.isEmpty, s"$clue failed:\n${difference.render}")
  }

  private val compactDirectProfile = aggregateProfile(
    PostgreSqlAggregateOneBagTerminalProfile.LegacyNestedV1,
    PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2,
    PostgreSqlReductionSupportProfile.DirectExistsV2
  )

  private val legacyDirectProfile = aggregateProfile(
    PostgreSqlAggregateOneBagTerminalProfile.LegacyNestedV1,
    PostgreSqlAggregateReductionFinalizationProfile.LegacyFullVerticesV1,
    PostgreSqlReductionSupportProfile.DirectExistsV2
  )

  private val compactDistinctProfile = aggregateProfile(
    PostgreSqlAggregateOneBagTerminalProfile.LegacyNestedV1,
    PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2,
    PostgreSqlReductionSupportProfile.DistinctSeparatorSupportV1
  )

  private val collapsedProfile = aggregateProfile(
    PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2,
    PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2,
    PostgreSqlReductionSupportProfile.DirectExistsV2
  )

  private val rendererV2Profile = PostgreSqlAggregateRenderingProfileV2
    .create(
      compactDistinctProfile,
      PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2,
      PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2
    )
    .fold(
      problems => throw new IllegalStateException(
        problems.map(_.code).mkString("; ")
      ),
      identity
    )

  private def aggregateProfile(
      terminal: PostgreSqlAggregateOneBagTerminalProfile,
      finalization: PostgreSqlAggregateReductionFinalizationProfile,
      support: PostgreSqlReductionSupportProfile
  ): PostgreSqlAggregateRenderingProfile =
    PostgreSqlAggregateRenderingProfile
      .create(terminal, finalization, support)
      .fold(
        problems => throw new IllegalStateException(
          problems.map(_.code).mkString("; ")
        ),
        identity
      )

  private def countCore(scenario: Scenario): String = {
    val enumeration =
      SyntheticCycleMultisetFixture.enumerationCore(scenario.dataset)
    val fromOffset = enumeration.indexOf("\nFROM ")
    require(fromOffset >= 0, s"${scenario.id} enumeration core has no FROM")
    val fromAndJoins = enumeration.substring(fromOffset + 1).trim
    val predicate = scenario.finalPredicate.fold("")(value => s"\nWHERE $value")
    s"SELECT COUNT(*) AS \"$ResultAlias\"\n$fromAndJoins$predicate"
  }

  private def assertExactCount(
      result: ExactResultMultiset.ResultMultiset,
      expected: Long,
      clue: munit.Clue[_]
  ): Unit = {
    assertEquals(result.schema.size, 1, clue)
    assertEquals(result.schema.head.columnLabel, ResultAlias, clue)
    assertEquals(result.schema.head.jdbcType, Types.BIGINT, clue)
    assertEquals(result.schema.head.databaseType, "int8", clue)
    assertEquals(result.totalMultiplicity, 1L, clue)
    assertEquals(result.distinctRowCount, 1, clue)
    assert(!result.containsNull, clue)

    val expectedResult = execute(
      s"SELECT CAST($expected AS BIGINT) AS \"$ResultAlias\""
    )
    val difference =
      expectedResult.differenceIgnoringNullabilityAndOrigin(result)
    assert(difference.isEmpty, s"$clue failed:\n${difference.render}")
  }

  private def detailed(
      sql: String,
      selection: CandidatePlanSelection,
      scenario: Scenario,
      reductionMode: YannakakisReductionMode
  ): PostgreSqlRewriteResult = {
    val result = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = scenario.widthBound,
          planLimit = PlanLimit,
          requireConnectedCover = scenario.requireConnectedCover,
          candidateSelection = selection,
          reductionMode = reductionMode
        )
      )
    )
    result.outcome match {
      case _: RewriteOutcome.Rewritten => result
      case other =>
        fail(
          s"${scenario.id} selection ${selection.id} did not rewrite: " +
            other.diagnostics
              .map(diagnostic =>
                s"${diagnostic.code.id}: ${diagnostic.message}"
              )
              .mkString("; ")
        )
    }
  }

  private def artifact(
      result: PostgreSqlRewriteResult
  ): PostgreSqlRewriteArtifact = {
    val value = result.report.artifact.getOrElse(
      fail(s"successful rewrite has no artifact: ${result.report}")
    )
    value.candidateSelection match {
      case _: PostgreSqlCandidateSelectionDecision.Selected => value
    }
  }

  private def rewrittenStatement(result: PostgreSqlRewriteResult): String =
    result.outcome match {
      case rewritten: RewriteOutcome.Rewritten =>
        rewritten.rewrittenStatements match {
          case Vector(statement) => statement
          case other => fail(s"expected one rewritten statement, received $other")
        }
      case other => fail(s"expected rewritten outcome, received $other")
    }

  private def execute(sql: String): ExactResultMultiset.ResultMultiset =
    ExactResultMultiset.execute(connection, sql)

  private def externalDatabase: Option[Database] = {
    val urlName = "YANNAKAKIS_TEST_JDBC_URL"
    val userName = "YANNAKAKIS_TEST_DB_USER"
    val passwordName = "YANNAKAKIS_TEST_DB_PASSWORD"
    val names = Vector(urlName, userName, passwordName)
    val present = names.filter(sys.env.contains)

    if (present.isEmpty) None
    else if (present.size != names.size) {
      throw new IllegalArgumentException(
        "External PostgreSQL test configuration is incomplete; missing: " +
          names.filterNot(sys.env.contains).mkString(", ")
      )
    } else {
      if (sys.env(urlName).trim.isEmpty || sys.env(userName).trim.isEmpty)
        throw new IllegalArgumentException(
          "External PostgreSQL URL and user must not be blank"
        )
      Some(Database(sys.env(urlName), sys.env(userName), sys.env(passwordName)))
    }
  }
}
