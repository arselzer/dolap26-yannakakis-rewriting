package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.sql.{Connection, DriverManager}

import scala.util.Using

import at.ac.tuwien.dbai.yannakakis.integration.ExactResultMultisetV2
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{ConnectedCover, EdgeCover}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteArtifact,
  PostgreSqlRewriteResult
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.{AbstractSchema, AbstractTable}
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks
import org.testcontainers.postgresql.PostgreSQLContainer

private object PostgreSqlFusedRawFactorApplicabilitySuite {
  final case class Database(jdbcUrl: String, username: String, password: String)

  val TableName = "v3_scope_edge"
  val CountAlias = "exact_count"

  val Cycle5: String =
    s"""FROM "pg_temp"."$TableName" AS "e0"
       |JOIN "pg_temp"."$TableName" AS "e1"
       |  ON "e0"."right_key" = "e1"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e2"
       |  ON "e1"."right_key" = "e2"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e3"
       |  ON "e2"."right_key" = "e3"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e4"
       |  ON "e3"."right_key" = "e4"."left_key"
       | AND "e4"."right_key" = "e0"."left_key"""".stripMargin

  // e0 binds both of its key columns to the same structural variable. Every
  // source occurrence is nevertheless the same physical relation, so this
  // combines occurrence-safe self joins with a repeated within-edge binding.
  val RepeatedBindingCycle: String =
    s"""FROM "pg_temp"."$TableName" AS "e0"
       |JOIN "pg_temp"."$TableName" AS "e1"
       |  ON "e0"."left_key" = "e1"."left_key"
       | AND "e0"."right_key" = "e1"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e2"
       |  ON "e1"."right_key" = "e2"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e3"
       |  ON "e2"."right_key" = "e3"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e4"
       |  ON "e3"."right_key" = "e4"."left_key"
       | AND "e4"."right_key" = "e0"."left_key"""".stripMargin

  val OneBag: String =
    s"""FROM "pg_temp"."$TableName" AS "e0"
       |JOIN "pg_temp"."$TableName" AS "e1"
       |  ON "e0"."left_key" = "e1"."left_key"
       | AND "e0"."right_key" = "e1"."right_key"""".stripMargin

  def count(from: String): String =
    s"SELECT COUNT(*) AS \"$CountAlias\"\n$from"

  def rootSchema() = {
    val root = Frameworks.createRootSchema(true)
    val temporary = root.add("pg_temp", new AbstractSchema)
    temporary.add(
      TableName,
      new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType =
          factory
            .builder()
            .add("row_id", SqlTypeName.BIGINT)
            .nullable(false)
            .add("left_key", SqlTypeName.BIGINT)
            .nullable(true)
            .add("right_key", SqlTypeName.BIGINT)
            .nullable(true)
            .build()
      }
    )
    root
  }

  def install(connection: Connection): Unit =
    Using.resource(connection.createStatement()) { statement =>
      statement.execute(
        s"""CREATE TEMPORARY TABLE "$TableName" (
           |  "row_id" BIGINT NOT NULL,
           |  "left_key" BIGINT,
           |  "right_key" BIGINT
           |)""".stripMargin
      )
      statement.execute(
        s"""INSERT INTO "$TableName" ("row_id", "left_key", "right_key")
           |VALUES
           |  (1, 1, 2),
           |  (1, 1, 2),
           |  (2, 2, 3),
           |  (3, 3, 4),
           |  (4, 4, 5),
           |  (5, 5, 1),
           |  (6, 9, 9),
           |  (6, 9, 9),
           |  (7, NULL, 1),
           |  (8, 1, NULL),
           |  (9, 100, 101)""".stripMargin
      )
    }
}

/**
  * Live coverage for V3 cases deliberately kept out of the original focused
  * suite: retained ranks beyond one, ConCov-off width three, occurrence-safe
  * self joins with repeated bindings, and the one-bag non-action boundary.
  */
class PostgreSqlFusedRawFactorApplicabilitySuite extends munit.FunSuite {
  import PostgreSqlFusedRawFactorApplicabilitySuite._

  private val PlanLimit = 20

  private val rendererV2Baseline = PostgreSqlAggregateRenderingProfileV2
    .baseline(PostgreSqlAggregateRenderingProfile.LegacyV1)
    .fold(
      problems => throw new IllegalStateException(problems.map(_.code).mkString("; ")),
      identity
    )
  private val rendererV3Baseline = PostgreSqlAggregateRenderingProfileV3
    .baseline(rendererV2Baseline)
    .fold(
      problems => throw new IllegalStateException(problems.map(_.code).mkString("; ")),
      identity
    )
  private val fusedProfile = PostgreSqlAggregateRenderingProfileV3
    .fusedRawFactorMessages(rendererV2Baseline)
    .fold(
      problems => throw new IllegalStateException(problems.map(_.code).mkString("; ")),
      identity
    )

  private var postgres: PostgreSQLContainer = _
  private var connection: Connection = _
  private var rewriter: PostgreSqlQueryRewriter = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val database = externalDatabase.getOrElse {
      postgres = new PostgreSQLContainer("postgres:18.4-alpine")
        .withDatabaseName("v3_applicability")
        .withUsername("v3_applicability")
        .withPassword("v3_applicability")
      postgres.start()
      Database(postgres.getJdbcUrl, postgres.getUsername, postgres.getPassword)
    }
    connection = DriverManager.getConnection(
      database.jdbcUrl,
      database.username,
      database.password
    )
    install(connection)
    rewriter = PostgreSqlQueryRewriter
      .forSchema(rootSchema())
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

  test("ConCov-off width-three V3 is exact for multiple retained ranks") {
    val source = count(Cycle5)
    val native = execute(source)
    val head = artifact(
      rewrite(
        source,
        CandidatePlanSelection.StructuralHead,
        widthBound = 3,
        requireConnectedCover = false
      )
    )
    val plans = head.candidateSearch.plans
    assert(plans.size > 2, clues(plans.map(_.rank)))
    assertEquals(plans.map(_.rank), (1 to plans.size).toVector)
    assertEquals(plans.map(_.candidateId).distinct.size, plans.size)
    assertEquals(head.candidateInput.widthBound, 3)
    assertEquals(head.candidateInput.requireConnectedCover, false)

    val multiBag = plans.filter(_.decomposition.bags.size > 1)
    assert(multiBag.size >= 2, clues(plans.map(p => p.rank -> p.decomposition.bags.size)))
    val disconnected = multiBag.find(plan =>
      plan.decomposition.bags.exists(bag =>
        EdgeCover
          .create(bag.cover)
          .flatMap(cover => ConnectedCover.isConnected(head.hypergraph, cover))
          .contains(false)
      )
    )
    assert(disconnected.nonEmpty, clues(multiBag.map(_.rank)))

    val disconnectedPlan = disconnected.get
    val otherPlan = multiBag
      .find(_.candidateId != disconnectedPlan.candidateId)
      .getOrElse(fail("expected a second distinct multi-bag retained candidate"))
    val selectedPlans = Vector(disconnectedPlan, otherPlan)
    assertEquals(selectedPlans.map(_.candidateId).distinct.size, 2)
    assert(selectedPlans.exists(_.rank > 1), clues(selectedPlans.map(_.rank)))
    selectedPlans.foreach { ranked =>
      val selected = artifact(
        rewrite(
          source,
          CandidatePlanSelection.StructuralRank(ranked.rank),
          widthBound = 3,
          requireConnectedCover = false
        )
      )
      assertEquals(selected.selectedPlan, ranked)
      assertEquals(selected.candidateInput.requireConnectedCover, false)
      assertExact(native, renderFused(selected), clues(ranked.rank, ranked.candidateId))
    }
  }

  test("V3 preserves self-join occurrences and repeated within-edge bindings") {
    val source = count(RepeatedBindingCycle)
    val native = execute(source)
    val head = artifact(
      rewrite(
        source,
        CandidatePlanSelection.StructuralHead,
        widthBound = 2,
        requireConnectedCover = false
      )
    )
    val multiBag = head.candidateSearch.plans
      .find(_.decomposition.bags.size > 1)
      .getOrElse(
        fail(
          s"top-$PlanLimit repeated-binding candidates contain no multi-bag plan"
        )
      )
    val selected = artifact(
      rewrite(
        source,
        CandidatePlanSelection.StructuralRank(multiBag.rank),
        widthBound = 2,
        requireConnectedCover = false
      )
    )

    assertEquals(
      selected.query.relations.map(_.qualifiedName.last).distinct,
      Vector(TableName)
    )
    assertEquals(selected.query.relations.map(_.id.value), (0 until 5).toVector)
    assert(
      selected.hypergraph.edges.exists(edge =>
        edge.bindings.map(_._2).distinct.size < edge.bindings.size
      ),
      clues(selected.hypergraph.edges.map(_.bindings))
    )
    assert(selected.bagFactorPlan.factors.size > 1)
    assertExact(native, renderFused(selected), clues(selected.selectedPlan))
  }

  test("one-bag V3 stays direct while the profile identity remains global") {
    val source = count(OneBag)
    val native = execute(source)
    val selected = artifact(
      rewrite(
        source,
        CandidatePlanSelection.StructuralHead,
        widthBound = 1,
        requireConnectedCover = true
      )
    )
    val (compiled, rendered) = renderFused(selected)

    assertEquals(selected.bagFactorPlan.factors.size, 1)
    assertEquals(
      fusedProfile.id,
      "sha256:5dc5ea030ff0160b630948547ef5af2d1071aea65febc20a90781a10ce5d0835"
    )
    assertEquals(
      rendererV3Baseline.id,
      "sha256:9ab89d3e6281b4e2c3d1fdb2f3ae96cef553ac3d8bfd3cf68ffce0d839210739"
    )
    assertNotEquals(fusedProfile.id, rendererV3Baseline.id)
    assertEquals(compiled.aggregateRenderingProfileV3.id, fusedProfile.id)
    assertEquals(rawPlan(compiled).messages, Vector.empty)
    assert(!rendered.sql.contains("yr_cm_b"), clues(rendered.sql))
    assert(!rendered.sql.contains("SUM("), clues(rendered.sql))
    assertExact(native, compiled -> rendered, clues("one-bag direct COUNT"))
  }

  private def rewrite(
      sql: String,
      selection: CandidatePlanSelection,
      widthBound: Int,
      requireConnectedCover: Boolean
  ): PostgreSqlRewriteResult = {
    val result = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = widthBound,
          planLimit = PlanLimit,
          requireConnectedCover = requireConnectedCover,
          candidateSelection = selection,
          reductionMode = YannakakisReductionMode.NoReduction
        )
      )
    )
    result.outcome match {
      case _: RewriteOutcome.Rewritten => result
      case other =>
        fail(
          s"${selection.id} did not rewrite: " +
            other.diagnostics.map(d => s"${d.code.id}: ${d.message}").mkString("; ")
        )
    }
  }

  private def artifact(result: PostgreSqlRewriteResult): PostgreSqlRewriteArtifact =
    result.report.artifact.getOrElse(
      fail(s"successful rewrite has no artifact: ${result.report}")
    )

  private def renderFused(
      source: PostgreSqlRewriteArtifact
  ): (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery) =
    PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV3(
        source.query,
        source.candidateInput,
        source.bagFactorPlan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.NoReduction,
        fusedProfile
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        identity
      )

  private def rawPlan(
      compiled: CompiledPostgreSqlRendering
  ) = compiled.terminal match {
    case CompiledPostgreSqlTerminal.GlobalCountStar(
          _,
          CompiledPostgreSqlTerminal.GlobalCountStrategy
            .RawFrequencyPropagation(plan)
        ) => plan
    case other => fail(s"expected raw frequency propagation, found $other")
  }

  private def assertExact(
      expected: ExactResultMultisetV2.ResultMultiset,
      actual: (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery),
      clue: munit.Clue[_]
  ): Unit = {
    val (_, rendered) = actual
    val difference = expected.difference(execute(rendered.sql))
    assert(difference.isEmpty, s"$clue V3 mismatch:\n${difference.render}")
  }

  private def execute(sql: String): ExactResultMultisetV2.ResultMultiset =
    ExactResultMultisetV2.execute(connection, sql)

  private def externalDatabase: Option[Database] = {
    val url = sys.env.get("YANNAKAKIS_TEST_JDBC_URL")
    val user = sys.env.get("YANNAKAKIS_TEST_DB_USER")
    val password = sys.env.get("YANNAKAKIS_TEST_DB_PASSWORD")
    val supplied = Vector(url, user, password).count(_.nonEmpty)
    if (supplied == 0) None
    else if (supplied != 3)
      throw new IllegalArgumentException(
        "External PostgreSQL test configuration must supply URL, user, and password"
      )
    else Some(Database(url.get, user.get, password.get))
  }
}
