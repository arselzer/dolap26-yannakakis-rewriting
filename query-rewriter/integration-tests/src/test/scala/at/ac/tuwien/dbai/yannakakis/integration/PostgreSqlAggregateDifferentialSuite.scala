package at.ac.tuwien.dbai.yannakakis.integration

import at.ac.tuwien.dbai.yannakakis.integration.{
  ExactResultMultisetV2 => ExactResultMultiset
}
import java.sql.{Connection, DriverManager, Types}

import scala.util.Using

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
  PostgreSqlAggregateRenderingProfileV3,
  PostgreSqlBagFactorRenderer,
  PostgreSqlFactorBoundaryProjectionProfile,
  PostgreSqlReductionSupportProfile,
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingLimits
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryResultShape
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.{AbstractSchema, AbstractTable}
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks
import org.testcontainers.postgresql.PostgreSQLContainer

private object PostgreSqlAggregateDifferentialSuite {
  final case class Database(
      jdbcUrl: String,
      username: String,
      password: String
  )

  val TableName = "aggregate_self_edge"
  val GroupAlias = "Group Key"
  val FirstGroupAlias = "First Key"
  val SecondGroupAlias = "Second Key"
  val CountAlias = "Witness Count"

  val CycleFrom: String =
    s"""FROM "pg_temp"."$TableName" AS "e0"
       |JOIN "pg_temp"."$TableName" AS "e1"
       |  ON "e0"."right_key" = "e1"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e2"
       |  ON "e1"."right_key" = "e2"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e3"
       |  ON "e2"."right_key" = "e3"."left_key"
       | AND "e3"."right_key" = "e0"."left_key"""".stripMargin

  val ChainFrom: String =
    s"""FROM "pg_temp"."$TableName" AS "e0"
       |JOIN "pg_temp"."$TableName" AS "e1"
       |  ON "e0"."right_key" = "e1"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e2"
       |  ON "e1"."right_key" = "e2"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e3"
       |  ON "e2"."right_key" = "e3"."left_key"""".stripMargin

  val BranchFrom: String =
    s"""FROM "pg_temp"."$TableName" AS "l"
       |JOIN "pg_temp"."$TableName" AS "x"
       |  ON "l"."left_key" = "x"."left_key"
       |JOIN "pg_temp"."$TableName" AS "y"
       |  ON "x"."right_key" = "y"."left_key"
       |JOIN "pg_temp"."$TableName" AS "d"
       |  ON "y"."right_key" = "d"."left_key"
       |JOIN "pg_temp"."$TableName" AS "dl"
       |  ON "d"."right_key" = "dl"."left_key"
       |JOIN "pg_temp"."$TableName" AS "e"
       |  ON "y"."right_key" = "e"."left_key"
       |JOIN "pg_temp"."$TableName" AS "el"
       |  ON "e"."right_key" = "el"."left_key"""".stripMargin

  def globalCount(
      from: String,
      predicate: Option[String] = None
  ): String =
    s"SELECT COUNT(*) AS \"$CountAlias\"\n$from${where(predicate)}"

  def groupedCount(predicate: Option[String] = None): String =
    s"""SELECT
       |  "e0"."left_key" AS "$GroupAlias",
       |  COUNT(*) AS "$CountAlias"
       |$CycleFrom${where(predicate)}
       |GROUP BY "e0"."left_key"
       |""".stripMargin.trim

  def distributedGroupedCount(predicate: Option[String] = None): String =
    s"""SELECT
       |  "e0"."left_key" AS "$FirstGroupAlias",
       |  "e2"."left_key" AS "$SecondGroupAlias",
       |  COUNT(*) AS "$CountAlias"
       |$CycleFrom${where(predicate)}
       |GROUP BY "e0"."left_key", "e2"."left_key"
       |""".stripMargin.trim

  def distributedChainCount: String =
    s"""SELECT
       |  "e0"."right_key" AS "$FirstGroupAlias",
       |  "e2"."right_key" AS "$SecondGroupAlias",
       |  COUNT(*) AS "$CountAlias"
       |$ChainFrom
       |GROUP BY "e0"."right_key", "e2"."right_key"
       |""".stripMargin.trim

  def distributedOneFactorCount: String =
    s"""SELECT
       |  "e0"."left_key" AS "$FirstGroupAlias",
       |  "e1"."right_key" AS "$SecondGroupAlias",
       |  COUNT(*) AS "$CountAlias"
       |FROM "pg_temp"."$TableName" AS "e0"
       |JOIN "pg_temp"."$TableName" AS "e1"
       |  ON "e0"."left_key" = "e1"."left_key"
       | AND "e0"."right_key" = "e1"."right_key"
       |GROUP BY "e0"."left_key", "e1"."right_key"
       |""".stripMargin.trim

  def extremum(
      function: String,
      guardIndex: Int,
      alias: String,
      predicate: Option[String] = None
  ): String =
    s"""SELECT $function("e$guardIndex"."payload") AS "$alias"
       |$CycleFrom${where(predicate)}""".stripMargin

  def rootSchema() = {
    val root = Frameworks.createRootSchema(true)
    val temporary = root.add("pg_temp", new AbstractSchema)
    temporary.add(
      TableName,
      new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType = {
          val builder = factory.builder()
          def add(name: String, nullable: Boolean): Unit = {
            val base = factory.createSqlType(SqlTypeName.BIGINT)
            builder.add(
              name,
              factory.createTypeWithNullability(base, nullable)
            )
          }
          add("row_id", nullable = false)
          add("left_key", nullable = true)
          add("right_key", nullable = true)
          add("payload", nullable = true)
          builder.build()
        }
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
           |  "right_key" BIGINT,
           |  "payload" BIGINT
           |)""".stripMargin
      )
      statement.execute(
        s"""INSERT INTO "$TableName"
           |  ("row_id", "left_key", "right_key", "payload")
           |VALUES
           |  (101, 1, 2, 90),
           |  (101, 1, 2, 90),
           |  (102, 2, 3, 10),
           |  (3, 3, 4, NULL),
           |  (3, 3, 4, NULL),
           |  (104, 4, 1, 40),
           |  (105, 9, 9, -5),
           |  (105, 9, 9, -5),
           |  (105, 9, 9, -5),
           |  (106, NULL, 2, 100),
           |  (107, 1, NULL, NULL),
           |  (108, 100, 101, 500)""".stripMargin
      )
    }

  private def where(predicate: Option[String]): String =
    predicate.fold("")(value => s"\nWHERE $value")
}

/**
  * Live PostgreSQL differential gate for the two retained aggregate families.
  *
  * The single physical relation is used four times. Its productive rows form
  * one directed C4 plus a self-loop, while duplicate rows amplify witnesses to
  * 4 per C4 start and 81 for the self-loop. NULL-key and dangling rows cannot
  * join under ordinary equality. This keeps the oracle small while exercising
  * occurrence identity, bag multiplicity, NULL rejection, all-NULL extrema,
  * and empty aggregate inputs.
  */
class PostgreSqlAggregateDifferentialSuite extends munit.FunSuite {
  import PostgreSqlAggregateDifferentialSuite._

  private val WidthBound = 2
  private val PlanLimit = 10

  private var postgres: PostgreSQLContainer = _
  private var connection: Connection = _
  private var rewriter: PostgreSqlQueryRewriter = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val database = externalDatabase.getOrElse {
      postgres = new PostgreSQLContainer("postgres:18.4-alpine")
        .withDatabaseName("aggregate_differential")
        .withUsername("aggregate_differential")
        .withPassword("aggregate_differential")
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

  test("self-join fixture pins duplicate, NULL, and adversarial native semantics") {
    val rows = execute(
      s"SELECT row_id, left_key, right_key, payload FROM \"$TableName\""
    )
    val nullKeys = execute(
      s"SELECT left_key, right_key FROM \"$TableName\" " +
        "WHERE left_key IS NULL OR right_key IS NULL"
    )
    val nullPayloads = execute(
      s"SELECT payload FROM \"$TableName\" WHERE payload IS NULL"
    )

    assertEquals(rows.totalMultiplicity, 12L)
    assert(rows.containsDuplicate)
    assert(rows.containsNull)
    assertEquals(nullKeys.totalMultiplicity, 2L)
    assert(nullKeys.containsNull)
    assertEquals(nullPayloads.totalMultiplicity, 3L)
    assert(nullPayloads.containsNull)

    assertMatchesExpected(
      groupedCount(),
      expectedGroupedRows,
      clues("native grouped counts")
    )
    assertMatchesExpected(
      extremum("MIN", 0, "minimum_payload"),
      expectedScalar("minimum_payload", Some(-5L)),
      clues("native MIN")
    )
    assertMatchesExpected(
      extremum("MAX", 0, "maximum_payload"),
      expectedScalar("maximum_payload", Some(90L)),
      clues("native MAX")
    )
    val allNullPredicate = Some(
      "\"e0\".\"row_id\" = \"e0\".\"left_key\""
    )
    assertMatchesExpected(
      s"SELECT COUNT(*) AS \"witnesses\"\n$CycleFrom${whereSql(allNullPredicate)}",
      expectedScalar("witnesses", Some(4L)),
      clues("nonempty all-NULL guard")
    )
  }

  test("one-key grouped COUNT is exact for structural head and explicit rank") {
    val source = groupedCount()
    val native = execute(source)
    assertMatchesExpected(source, expectedGroupedRows, clues("native"))
    assertEquals(
      native.schema.map(column =>
        (
          column.columnLabel,
          column.jdbcType,
          column.databaseType,
          column.precision,
          column.scale
        )
      ),
      Vector(
        (GroupAlias, Types.BIGINT, "int8", 19, 0),
        (CountAlias, Types.BIGINT, "int8", 19, 0)
      )
    )
    assertEquals(native.totalMultiplicity, 5L)
    assertEquals(native.distinctRowCount, 5)
    assert(!native.containsNull)

    val results = structuralHeadAndLastRank(
      source,
      YannakakisReductionMode.FullTwoPass
    )
    results.foreach { case (selection, expectedPlan, result) =>
      val value = artifact(result)
      val clue = clues(selection.id, expectedPlan.rank, expectedPlan.candidateId)

      assertEquals(value.selectedPlan, expectedPlan, clue)
      assertEquals(value.query.resultShape.id, "grouped-count-star", clue)
      assertEquals(value.query.relations.size, 4, clue)
      assertEquals(
        value.query.relations.map(_.qualifiedName.last),
        Vector.fill(4)(TableName),
        clue
      )
      assertEquals(
        value.query.relations.map(_.id.value),
        (0 until 4).toVector,
        clue
      )
      assert(value.bagFactorPlan.factors.size > 1, clue)
      assertEquals(
        value.candidateSelection.reductionMode,
        YannakakisReductionMode.FullTwoPass,
        clue
      )
      assertEquals(
        value.renderedQuery.output.map(column =>
          (column.ordinal, column.name, column.dataType, column.nullable)
        ),
        Vector(
          (0, GroupAlias, "BIGINT", true),
          (1, CountAlias, "BIGINT NOT NULL", false)
        ),
        clue
      )
      assert(value.renderedQuery.sql.contains("CAST(COALESCE(SUM("), clue)
      assert(value.renderedQuery.sql.contains("\nGROUP BY\n"), clue)

      assertDifferential(native, result, clue)
      assertOptimizedDifferential(native, value, legacyDirectProfile, clue)
      assertOptimizedDifferential(native, value, compactDirectProfile, clue)
      assertOptimizedDifferential(native, value, compactDistinctProfile, clue)
      assertRendererV2Differential(native, value, clue)
    }
  }

  test("grouped COUNT emits no row on empty input for head and explicit rank") {
    val source = groupedCount(
      Some("\"e0\".\"row_id\" = \"e0\".\"right_key\"")
    )
    val native = execute(source)
    val expected = expectedEmptyGroupedRows
    val nativeDifference =
      expected.differenceIgnoringNullabilityAndOrigin(native)
    assert(nativeDifference.isEmpty, nativeDifference.render)
    assert(native.isEmpty)

    structuralHeadAndLastRank(
      source,
      YannakakisReductionMode.FullTwoPass
    ).foreach { case (selection, expectedPlan, result) =>
      val value = artifact(result)
      val clue = clues(selection.id, expectedPlan.rank, expectedPlan.candidateId)
      assertEquals(value.selectedPlan, expectedPlan, clue)
      assertEquals(
        value.renderedQuery.output.map(_.nullable),
        Vector(true, false),
        clue
      )
      assertDifferential(native, result, clue)
      assertOptimizedDifferential(native, value, legacyDirectProfile, clue)
      assertOptimizedDifferential(native, value, compactDirectProfile, clue)
      assertRendererV2Differential(native, value, clue)
    }
  }

  test("renderer-v2 axes independently fire and remain exact") {
    Vector(
      "global" -> s"SELECT COUNT(*) AS \"$CountAlias\"\n$CycleFrom",
      "G1" -> groupedCount(),
      "U1" -> distributedGroupedCount()
    ).foreach { case (label, source) =>
      val native = execute(source)
      val selected = artifact(
        detailed(
          source,
          CandidatePlanSelection.StructuralHead,
          YannakakisReductionMode.FullTwoPass
        )
      )
      assert(selected.bagFactorPlan.factors.size > 1, clues(label))

      val demandOnly = renderV2(selected, demandOnlyProfile)
      assert(strictDemandProjection(selected, demandOnly), clues(label))
      assert(!containsDirectLeafReference(demandOnly), clues(label))
      assertExact(native, demandOnly.sql, clues(label, "demand-only"))

      val leafOnly = renderV2(selected, leafOnlyProfile)
      assert(!strictDemandProjection(selected, leafOnly), clues(label))
      assert(containsDirectLeafReference(leafOnly), clues(label))
      assertExact(native, leafOnly.sql, clues(label, "leaf-only"))

      val combined = renderV2(selected, rendererV2Profile)
      assert(strictDemandProjection(selected, combined), clues(label))
      assert(containsDirectLeafReference(combined), clues(label))
      assertExact(native, combined.sql, clues(label, "combined"))
    }
  }

  test("renderer-v2 live zero-demand factor preserves multiplicity with a unit marker") {
    val source = s"SELECT COUNT(*) AS \"$CountAlias\"\n$CycleFrom"
    val native = execute(source)
    val selected = artifact(
      detailed(
        source,
        CandidatePlanSelection.StructuralHead,
        YannakakisReductionMode.FullTwoPass,
        widthBound = 3
      )
    )
    assertEquals(selected.bagFactorPlan.factors.size, 1)
    val rendered = renderV2(selected, demandOnlyProfile)
    val factor = rendered.trace.bags.head
    assertEquals(factor.variableColumns, Vector.empty)
    assertEquals(factor.occurrenceColumns, Vector.empty)
    assert(
      rendered.sql.contains(s"\"yr_unit_b${factor.bagId.value}\""),
      clues(rendered.sql)
    )
    assertExact(native, rendered.sql, clues("unit-only factor"))
  }

  test("fused raw-factor global COUNT is exact on C4 chain branch and empty input") {
    val emptyPredicate = Some(
      "\"e0\".\"row_id\" = \"e0\".\"right_key\""
    )
    val scenarios = Vector(
      ("C4-duplicates-and-NULLs", globalCount(CycleFrom), 2),
      ("C4-empty", globalCount(CycleFrom, emptyPredicate), 2),
      ("chain", globalCount(ChainFrom), 1),
      ("branch", globalCount(BranchFrom), 1)
    )

    assertMatchesExpected(
      scenarios.head._2,
      expectedScalar(CountAlias, Some(97L)),
      clues("native C4 duplicate/NULL semantics")
    )
    assertMatchesExpected(
      scenarios(1)._2,
      expectedScalar(CountAlias, Some(0L)),
      clues("native empty global COUNT")
    )

    scenarios.foreach { case (label, source, widthBound) =>
      val native = execute(source)
      val selected = artifact(
        detailed(
          source,
          CandidatePlanSelection.StructuralHead,
          YannakakisReductionMode.NoReduction,
          widthBound
        )
      )
      val rendered = renderFusedRawCount(selected)
      val reduction = rendered.trace.reduction
      val clue = clues(label, selected.selectedPlan)

      assert(selected.bagFactorPlan.factors.size > 1, clue)
      if (label == "branch") {
        assert(
          selected.bagFactorPlan.factors.exists(_.children.size >= 2),
          clues(label, selected.bagFactorPlan.factors)
        )
      }
      assertEquals(
        selected.candidateSelection.reductionMode,
        YannakakisReductionMode.NoReduction,
        clue
      )
      assertEquals(reduction.mode, YannakakisReductionMode.NoReduction, clue)
      assertEquals(reduction.selectedBottomUpStepCount, 0, clue)
      assertEquals(reduction.selectedTopDownStepCount, 0, clue)
      assertEquals(reduction.physicalReductionCteCount, 0, clue)
      assert(
        !rendered.sql.matches("(?s).*yr_f_b[0-9]+_(bu|td)[0-9]+.*"),
        clue
      )
      assertEquals(
        rendered.output.map(column =>
          (column.ordinal, column.name, column.dataType, column.nullable)
        ),
        Vector((0, CountAlias, "BIGINT NOT NULL", false)),
        clue
      )
      assertExact(native, rendered.sql, clue)
    }
  }

  test("distributed two-key COUNT is exact for head, rank, duplicates, and empty input") {
    val source = distributedGroupedCount()
    val native = execute(source)
    assertMatchesExpected(source, expectedDistributedRows, clues("native"))
    assertEquals(
      native.schema.map(column =>
        (
          column.columnLabel,
          column.jdbcType,
          column.databaseType,
          column.precision,
          column.scale
        )
      ),
      Vector(
        (
          FirstGroupAlias,
          Types.BIGINT,
          "int8",
          19,
          0
        ),
        (
          SecondGroupAlias,
          Types.BIGINT,
          "int8",
          19,
          0
        ),
        (CountAlias, Types.BIGINT, "int8", 19, 0)
      )
    )
    assertEquals(native.totalMultiplicity, 5L)
    assertEquals(native.distinctRowCount, 5)

    structuralHeadAndLastRank(
      source,
      YannakakisReductionMode.FullTwoPass
    ).foreach { case (selection, expectedPlan, result) =>
      val value = artifact(result)
      val clue = clues(selection.id, expectedPlan.rank, expectedPlan.candidateId)
      assertEquals(value.selectedPlan, expectedPlan, clue)
      assertEquals(
        value.query.resultShape.id,
        "distributed-grouped-count-star",
        clue
      )
      assertEquals(
        value.renderedQuery.output.map(column =>
          (column.ordinal, column.name, column.dataType, column.nullable)
        ),
        Vector(
          (0, FirstGroupAlias, "BIGINT", true),
          (1, SecondGroupAlias, "BIGINT", true),
          (2, CountAlias, "BIGINT NOT NULL", false)
        ),
        clue
      )
      assert(value.renderedQuery.sql.contains("CAST(COALESCE(SUM("), clue)
      assertDifferential(native, result, clue)
      assertOptimizedDifferential(native, value, legacyDirectProfile, clue)
      assertOptimizedDifferential(native, value, compactDirectProfile, clue)
      assertOptimizedDifferential(native, value, compactDistinctProfile, clue)
      assertRendererV2Differential(native, value, clue)
    }

    val emptySource = distributedGroupedCount(
      Some("\"e0\".\"row_id\" = \"e0\".\"right_key\"")
    )
    val emptyNative = execute(emptySource)
    assert(emptyNative.isEmpty)
    structuralHeadAndLastRank(
      emptySource,
      YannakakisReductionMode.FullTwoPass
    ).foreach { case (selection, expectedPlan, result) =>
      val value = artifact(result)
      val clue = clues(selection.id, expectedPlan.rank, "empty")
      assertDifferential(
        emptyNative,
        result,
        clue
      )
      assertOptimizedDifferential(
        emptyNative,
        value,
        legacyDirectProfile,
        clue
      )
      assertOptimizedDifferential(
        emptyNative,
        value,
        compactDirectProfile,
        clue
      )
      assertRendererV2Differential(emptyNative, value, clue)
    }

    // Unlike the C4 plans above, this path forces one output key to travel
    // across a decomposition message. It is the live counterpart of the
    // renderer suite's carried-key shape, including an otherwise dangling row
    // whose ungrouped endpoint is NULL.
    val chainSource = distributedChainCount
    val chainNative = execute(chainSource)
    assertMatchesExpected(
      chainSource,
      expectedDistributedChainRows,
      clues("native carried-key chain")
    )
    structuralHeadAndLastRank(
      chainSource,
      YannakakisReductionMode.FullTwoPass
    ).foreach { case (selection, expectedPlan, result) =>
      val value = artifact(result)
      val clue = clues(selection.id, expectedPlan.rank, "carried-key chain")
      assertDifferential(
        chainNative,
        result,
        clue
      )
      assertOptimizedDifferential(
        chainNative,
        value,
        legacyDirectProfile,
        clue
      )
      assertOptimizedDifferential(
        chainNative,
        value,
        compactDirectProfile,
        clue
      )
      assertRendererV2Differential(chainNative, value, clue)
    }

    val oneFactorSource = distributedOneFactorCount
    val oneFactorNative = execute(oneFactorSource)
    assertMatchesExpected(
      oneFactorSource,
      expectedDistributedOneFactorRows,
      clues("native one-factor")
    )
    val oneFactor = detailed(
      oneFactorSource,
      CandidatePlanSelection.StructuralHead,
      YannakakisReductionMode.FullTwoPass
    )
    assertEquals(artifact(oneFactor).bagFactorPlan.factors.size, 1)
    assertDifferential(
      oneFactorNative,
      oneFactor,
      clues("one-factor structural head")
    )
    assertOptimizedDifferential(
      oneFactorNative,
      artifact(oneFactor),
      collapsedProfile,
      clues("one-factor collapsed")
    )
    assertRendererV2Differential(
      oneFactorNative,
      artifact(oneFactor),
      clues("one-factor renderer-v2")
    )
  }

  test("two-key NUMERIC terminal checks BIGINT at each group without huge rows") {
    val maximum = Long.MaxValue.toString
    val representable = execute(
      s"""WITH "weighted"("first_key", "second_key", "yr_freq") AS (
         |  VALUES
         |    (CAST(1 AS BIGINT), CAST(10 AS BIGINT), CAST($maximum AS NUMERIC)),
         |    (CAST(2 AS BIGINT), CAST(20 AS BIGINT), CAST($maximum AS NUMERIC))
         |)
         |SELECT
         |  "first_key" AS "$FirstGroupAlias",
         |  "second_key" AS "$SecondGroupAlias",
         |  CAST(
         |    COALESCE(SUM("yr_freq"), CAST(0 AS NUMERIC)) AS BIGINT
         |  ) AS "$CountAlias"
         |FROM "weighted"
         |GROUP BY "first_key", "second_key"""".stripMargin
    )
    val expected = execute(
      s"""SELECT
         |  "first_key" AS "$FirstGroupAlias",
         |  "second_key" AS "$SecondGroupAlias",
         |  "witness_count" AS "$CountAlias"
         |FROM (VALUES
         |  (CAST(1 AS BIGINT), CAST(10 AS BIGINT), CAST($maximum AS BIGINT)),
         |  (CAST(2 AS BIGINT), CAST(20 AS BIGINT), CAST($maximum AS BIGINT))
         |) AS "expected"("first_key", "second_key", "witness_count")""".stripMargin
    )
    val difference =
      expected.differenceIgnoringNullabilityAndOrigin(representable)
    assert(difference.isEmpty, difference.render)
    assertEquals(representable.totalMultiplicity, 2L)

    val overflow = intercept[java.sql.SQLException] {
      execute(
        s"""WITH "weighted"("first_key", "second_key", "yr_freq") AS (
           |  VALUES
           |    (CAST(1 AS BIGINT), CAST(10 AS BIGINT), CAST($maximum AS NUMERIC)),
           |    (CAST(1 AS BIGINT), CAST(10 AS BIGINT), CAST(1 AS NUMERIC)),
           |    (CAST(2 AS BIGINT), CAST(20 AS BIGINT), CAST($maximum AS NUMERIC))
           |)
           |SELECT
           |  "first_key",
           |  "second_key",
           |  CAST(
           |    COALESCE(SUM("yr_freq"), CAST(0 AS NUMERIC)) AS BIGINT
           |  ) AS "witness_count"
           |FROM "weighted"
           |GROUP BY "first_key", "second_key"""".stripMargin
      )
    }
    assertEquals(overflow.getSQLState, "22003")
  }

  test("guarded MIN and MAX are exact for root and non-root guards in every mode") {
    val probes = (0 until 4).map { guardIndex =>
      val source = extremum("MIN", guardIndex, s"probe_$guardIndex")
      val result = detailed(
        source,
        CandidatePlanSelection.StructuralHead,
        YannakakisReductionMode.FullTwoPass
      )
      (guardIndex, artifact(result))
    }.toVector
    val rootGuard = probes.find { case (_, value) =>
      guardOwner(value) == value.bagFactorPlan.root
    }.map(_._1).getOrElse(fail("retained C4 head has no root-owned guard"))
    val nonRootGuard = probes.find { case (_, value) =>
      guardOwner(value) != value.bagFactorPlan.root
    }.map(_._1).getOrElse(fail("retained C4 head has no non-root guard"))

    val scenarios = Vector(
      ("MIN", rootGuard, "Root MIN", -5L),
      ("MAX", nonRootGuard, "Non-root MAX", 90L)
    )
    val observedOwnership = scala.collection.mutable.Set.empty[Boolean]

    scenarios.foreach { case (function, guardIndex, alias, expectedValue) =>
      val source = extremum(function, guardIndex, alias)
      val native = execute(source)
      assertMatchesExpected(
        source,
        expectedScalar(alias, Some(expectedValue)),
        clues(function, guardIndex, "native")
      )

      YannakakisReductionMode.Values.foreach { mode =>
        structuralHeadAndLastRank(source, mode).foreach {
          case (selection, expectedPlan, result) =>
            val value = artifact(result)
            val clue = clues(
              function,
              guardIndex,
              selection.id,
              expectedPlan.rank,
              mode
            )
            observedOwnership += (guardOwner(value) == value.bagFactorPlan.root)

            assertEquals(value.selectedPlan, expectedPlan, clue)
            assertEquals(value.query.resultShape.id, "global-guarded-extremum", clue)
            assertEquals(value.candidateSelection.reductionMode, mode, clue)
            assertEquals(value.renderedQuery.trace.reduction.mode, mode, clue)
            assertEquals(
              value.renderedQuery.output.map(column =>
                (column.ordinal, column.name, column.dataType, column.nullable)
              ),
              Vector((0, alias, "BIGINT", true)),
              clue
            )
            assertEquals(value.query.relations.map(_.id.value), (0 until 4).toVector)
            assertEquals(
              value.query.relations.map(_.qualifiedName.last),
              Vector.fill(4)(TableName)
            )
            mode match {
              case YannakakisReductionMode.FullTwoPass =>
                assert(!terminalSql(value.renderedQuery.sql).contains("WHERE"), clue)
              case YannakakisReductionMode.BottomUpOnly |
                  YannakakisReductionMode.NoReduction =>
                assert(
                  terminalSql(value.renderedQuery.sql).contains("WHERE\n  EXISTS ("),
                  clue
                )
            }

            assertDifferential(native, result, clue)
        }
      }
    }

    assertEquals(observedOwnership.toSet, Set(false, true))
  }

  test("guarded extrema preserve all-NULL and empty scalar rows in every mode") {
    val scenarios = Vector(
      (
        extremum(
          "MIN",
          0,
          "all_null_min",
          Some("\"e0\".\"row_id\" = \"e0\".\"left_key\"")
        ),
        "all_null_min"
      ),
      (
        extremum(
          "MAX",
          0,
          "empty_max",
          Some("\"e0\".\"row_id\" = \"e0\".\"right_key\"")
        ),
        "empty_max"
      )
    )

    scenarios.foreach { case (source, alias) =>
      val native = execute(source)
      val expected = expectedScalar(alias, None)
      val nativeDifference =
        expected.differenceIgnoringNullabilityAndOrigin(native)
      assert(nativeDifference.isEmpty, nativeDifference.render)
      assertEquals(native.totalMultiplicity, 1L)
      assertEquals(native.distinctRowCount, 1)
      assert(native.containsNull)

      YannakakisReductionMode.Values.foreach { mode =>
        val result = detailed(
          source,
          CandidatePlanSelection.StructuralHead,
          mode
        )
        val value = artifact(result)
        val clue = clues(alias, mode, value.selectedPlan)
        assertEquals(
          value.renderedQuery.output.map(column =>
            (column.name, column.dataType, column.nullable)
          ),
          Vector((alias, "BIGINT", true)),
          clue
        )
        assertDifferential(native, result, clue)
      }
    }
  }

  private def structuralHeadAndLastRank(
      sql: String,
      mode: YannakakisReductionMode
  ) = {
    val head = detailed(sql, CandidatePlanSelection.StructuralHead, mode)
    val plans = artifact(head).candidateSearch.plans
    assert(plans.size > 1, clues(plans.map(_.rank)))
    assertEquals(plans.map(_.rank), (1 to plans.size).toVector)
    assertEquals(plans.map(_.candidateId).distinct.size, plans.size)
    val last = plans.last
    Vector(
      (CandidatePlanSelection.StructuralHead, plans.head, head),
      (
        CandidatePlanSelection.StructuralRank(last.rank),
        last,
        detailed(
          sql,
          CandidatePlanSelection.StructuralRank(last.rank),
          mode
        )
      )
    )
  }

  private def detailed(
      sql: String,
      selection: CandidatePlanSelection,
      mode: YannakakisReductionMode,
      widthBound: Int = WidthBound
  ): PostgreSqlRewriteResult = {
    val result = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = widthBound,
          planLimit = PlanLimit,
          requireConnectedCover = true,
          candidateSelection = selection,
          reductionMode = mode
        )
      )
    )
    result.outcome match {
      case _: RewriteOutcome.Rewritten => result
      case other =>
        fail(
          s"selection ${selection.id} in $mode did not rewrite: " +
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

  private def assertDifferential(
      expected: ExactResultMultiset.ResultMultiset,
      result: PostgreSqlRewriteResult,
      clue: munit.Clue[_]
  ): Unit = {
    val actual = execute(rewrittenStatement(result))
    val difference = expected.difference(actual)
    assert(difference.isEmpty, s"$clue failed:\n${difference.render}")
  }

  private def assertOptimizedDifferential(
      expected: ExactResultMultiset.ResultMultiset,
      source: PostgreSqlRewriteArtifact,
      profile: PostgreSqlAggregateRenderingProfile,
      clue: munit.Clue[_]
  ): Unit = {
    val generated = PostgreSqlBagFactorRenderer
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
    val actual = execute(generated)
    val difference = expected.difference(actual)
    assert(difference.isEmpty, s"$clue optimized failed:\n${difference.render}")
  }

  private def assertRendererV2Differential(
      expected: ExactResultMultiset.ResultMultiset,
      source: PostgreSqlRewriteArtifact,
      clue: munit.Clue[_]
  ): Unit = {
    val generated = renderV2(source, rendererV2Profile).sql
    assertExact(expected, generated, clue)
  }

  private def renderV2(
      source: PostgreSqlRewriteArtifact,
      profile: PostgreSqlAggregateRenderingProfileV2
  ): PostgreSqlRenderedQuery =
    PostgreSqlBagFactorRenderer
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
        _._2
      )

  private def renderFusedRawCount(
      source: PostgreSqlRewriteArtifact
  ): PostgreSqlRenderedQuery =
    PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfileV3(
        source.query,
        source.candidateInput,
        source.bagFactorPlan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.NoReduction,
        fusedRawCountProfile
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        _._2
      )

  private def assertExact(
      expected: ExactResultMultiset.ResultMultiset,
      sql: String,
      clue: munit.Clue[_]
  ): Unit = {
    val actual = execute(sql)
    val difference = expected.difference(actual)
    assert(difference.isEmpty, s"$clue renderer-v2 failed:\n${difference.render}")
  }

  private def strictDemandProjection(
      source: PostgreSqlRewriteArtifact,
      rendered: PostgreSqlRenderedQuery
  ): Boolean = {
    val verticesByBag = source.bagFactorPlan.factors.iterator
      .map(factor => factor.bagId -> factor.vertices)
      .toMap
    rendered.trace.bags.exists(trace =>
      trace.variableColumns.map(_.variableId) != verticesByBag(trace.bagId)
    )
  }

  private def containsDirectLeafReference(
      rendered: PostgreSqlRenderedQuery
  ): Boolean =
    rendered.trace.bags.exists(trace =>
      rendered.sql.contains(
        s"\"${trace.finalFactorCte}\" AS \"yr_cm_b${trace.bagId.value}\""
      )
    )

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

  private val demandOnlyProfile = PostgreSqlAggregateRenderingProfileV2
    .create(
      compactDistinctProfile,
      PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2,
      PostgreSqlAggregateLeafMessageProfile.DerivedSelectV1
    )
    .fold(
      problems => throw new IllegalStateException(
        problems.map(_.code).mkString("; ")
      ),
      identity
    )

  private val leafOnlyProfile = PostgreSqlAggregateRenderingProfileV2
    .create(
      compactDistinctProfile,
      PostgreSqlFactorBoundaryProjectionProfile.FullFactorVerticesV1,
      PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2
    )
    .fold(
      problems => throw new IllegalStateException(
        problems.map(_.code).mkString("; ")
      ),
      identity
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

  private val fusedRawCountProfile = {
    val baseline = PostgreSqlAggregateRenderingProfileV2
      .baseline(PostgreSqlAggregateRenderingProfile.LegacyV1)
      .fold(
        problems => throw new IllegalStateException(
          problems.map(_.code).mkString("; ")
        ),
        identity
      )
    PostgreSqlAggregateRenderingProfileV3
      .fusedRawFactorMessages(baseline)
      .fold(
        problems => throw new IllegalStateException(
          problems.map(_.code).mkString("; ")
        ),
        identity
      )
  }

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

  private def assertMatchesExpected(
      sourceSql: String,
      expected: ExactResultMultiset.ResultMultiset,
      clue: munit.Clue[_]
  ): Unit = {
    val actual = execute(sourceSql)
    val difference = expected.differenceIgnoringNullabilityAndOrigin(actual)
    assert(difference.isEmpty, s"$clue failed:\n${difference.render}")
  }

  private def expectedGroupedRows: ExactResultMultiset.ResultMultiset =
    execute(
      s"""SELECT "group_key" AS "$GroupAlias", "witness_count" AS "$CountAlias"
         |FROM (VALUES
         |  (CAST(1 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(2 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(3 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(4 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(9 AS BIGINT), CAST(81 AS BIGINT))
         |) AS "expected"("group_key", "witness_count")""".stripMargin
    )

  private def expectedEmptyGroupedRows: ExactResultMultiset.ResultMultiset =
    execute(
      s"""SELECT
         |  CAST(NULL AS BIGINT) AS "$GroupAlias",
         |  CAST(NULL AS BIGINT) AS "$CountAlias"
         |WHERE FALSE""".stripMargin
    )

  private def expectedDistributedRows: ExactResultMultiset.ResultMultiset =
    execute(
      s"""SELECT
         |  "first_key" AS "$FirstGroupAlias",
         |  "second_key" AS "$SecondGroupAlias",
         |  "witness_count" AS "$CountAlias"
         |FROM (VALUES
         |  (CAST(1 AS BIGINT), CAST(3 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(2 AS BIGINT), CAST(4 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(3 AS BIGINT), CAST(1 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(4 AS BIGINT), CAST(2 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(9 AS BIGINT), CAST(9 AS BIGINT), CAST(81 AS BIGINT))
         |) AS "expected"("first_key", "second_key", "witness_count")""".stripMargin
    )

  private def expectedDistributedChainRows: ExactResultMultiset.ResultMultiset =
    execute(
      s"""SELECT
         |  "first_key" AS "$FirstGroupAlias",
         |  "second_key" AS "$SecondGroupAlias",
         |  "witness_count" AS "$CountAlias"
         |FROM (VALUES
         |  (CAST(1 AS BIGINT), CAST(3 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(2 AS BIGINT), CAST(4 AS BIGINT), CAST(6 AS BIGINT)),
         |  (CAST(3 AS BIGINT), CAST(1 AS BIGINT), CAST(6 AS BIGINT)),
         |  (CAST(4 AS BIGINT), CAST(2 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(9 AS BIGINT), CAST(9 AS BIGINT), CAST(81 AS BIGINT))
         |) AS "expected"("first_key", "second_key", "witness_count")""".stripMargin
    )

  private def expectedDistributedOneFactorRows
      : ExactResultMultiset.ResultMultiset =
    execute(
      s"""SELECT
         |  "first_key" AS "$FirstGroupAlias",
         |  "second_key" AS "$SecondGroupAlias",
         |  "witness_count" AS "$CountAlias"
         |FROM (VALUES
         |  (CAST(1 AS BIGINT), CAST(2 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(2 AS BIGINT), CAST(3 AS BIGINT), CAST(1 AS BIGINT)),
         |  (CAST(3 AS BIGINT), CAST(4 AS BIGINT), CAST(4 AS BIGINT)),
         |  (CAST(4 AS BIGINT), CAST(1 AS BIGINT), CAST(1 AS BIGINT)),
         |  (CAST(9 AS BIGINT), CAST(9 AS BIGINT), CAST(9 AS BIGINT)),
         |  (CAST(100 AS BIGINT), CAST(101 AS BIGINT), CAST(1 AS BIGINT))
         |) AS "expected"("first_key", "second_key", "witness_count")""".stripMargin
    )

  private def expectedScalar(
      alias: String,
      value: Option[Long]
  ): ExactResultMultiset.ResultMultiset = {
    val rendered = value.fold("NULL")(_.toString)
    execute(s"SELECT CAST($rendered AS BIGINT) AS \"$alias\"")
  }

  private def execute(sql: String): ExactResultMultiset.ResultMultiset =
    ExactResultMultiset.execute(connection, sql)

  private def guardOwner(artifact: PostgreSqlRewriteArtifact) = {
    val shape = artifact.query.resultShape
      .asInstanceOf[QueryResultShape.GlobalGuardedExtremum]
    val edge = artifact.hypergraph.edges
      .find(_.relationOccurrenceId == shape.guardColumn.relationOccurrenceId)
      .getOrElse(fail("missing guard edge"))
    artifact.bagFactorPlan.ownership
      .find(_.edgeId == edge.id)
      .map(_.owner)
      .getOrElse(fail("missing guard owner"))
  }

  private def terminalSql(sql: String): String = {
    val marker = "\nSELECT\n"
    val start = sql.lastIndexOf(marker)
    if (start < 0) sql else sql.substring(start + 1)
  }

  private def whereSql(predicate: Option[String]): String =
    predicate.fold("")(value => s"\nWHERE $value")

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
