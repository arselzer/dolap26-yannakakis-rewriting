package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.calcite.{TestColumn, TestTable}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteQueryAnalyzer,
  CalciteSqlPlanner
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.tools.Frameworks

/** SQL-shape and resource contracts for the additive one-CTE renderer. */
final class PostgreSqlRelevanceLimitedCountRendererSuite
    extends munit.FunSuite {
  private val tables = Vector(
    TestTable(
      "v4_link",
      Vector(
        TestColumn("left_key", nullable = true),
        TestColumn("right_key", nullable = true)
      )
    ),
    TestTable(
      "v4_fact",
      Vector(
        TestColumn("movie_id", nullable = true),
        TestColumn("dim_id", nullable = true)
      )
    ),
    TestTable(
      "v4_dim",
      Vector(TestColumn("id", nullable = true))
    ),
    TestTable(
      "yr_v4_shared_wing",
      Vector(
        TestColumn("left_key", nullable = true),
        TestColumn("right_key", nullable = true)
      )
    ),
    TestTable(
      "yr_v4_shared_wing_1",
      Vector(
        TestColumn("movie_id", nullable = true),
        TestColumn("dim_id", nullable = true)
      )
    )
  )

  private val sourceSql =
    """SELECT COUNT(*) AS exact_count
      |FROM v4_link AS l
      |JOIN v4_fact AS f1 ON f1.movie_id = l.left_key
      |JOIN v4_dim AS d1 ON d1.id = f1.dim_id
      |JOIN v4_fact AS f2 ON f2.movie_id = l.right_key
      |JOIN v4_dim AS d2 ON d2.id = f2.dim_id""".stripMargin

  private val collisionSql =
    """SELECT COUNT(*) AS exact_count
      |FROM yr_v4_shared_wing AS l
      |JOIN yr_v4_shared_wing_1 AS f1 ON f1.movie_id = l.left_key
      |JOIN v4_dim AS d1 ON d1.id = f1.dim_id
      |JOIN yr_v4_shared_wing_1 AS f2 ON f2.movie_id = l.right_key
      |JOIN v4_dim AS d2 ON d2.id = f2.dim_id""".stripMargin

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    tables.foreach { table =>
      schema.add(
        table.name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType = {
            val builder = factory.builder()
            table.columns.foreach { column =>
              val base = factory.createSqlType(column.sqlType)
              builder.add(
                column.name,
                factory.createTypeWithNullability(base, column.nullable)
              )
            }
            builder.build()
          }
        }
      )
    }
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  private lazy val prepared = prepare(sourceSql)

  private def prepare(sql: String) = {
    val planned = CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)
    val report = CalciteQueryAnalyzer.analyze(planned)
    assert(report.isSupported, clues(report.diagnostics))
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

  private lazy val evidence = baseTableEvidence(
    Vector("v4_link", "v4_fact", "v4_dim")
  )

  test("renders one materialized relevance-limited shared frequency CTE") {
    val value = rendered()
    val sql = value.sql

    assertEquals(value.sourceQuery, prepared.query)
    assertEquals(value.candidateInput, prepared.input)
    assertEquals(value.provenancePlan, prepared.plan)
    assertEquals(value.trace.physicalCteCount, 1)
    assertEquals(value.trace.members.size, 2)
    assertEquals(value.trace.eliminatedWingScanCount, 2)
    assertEquals(value.trace.anchorDemandScanCount, 2)
    assertEquals(value.trace.originalSourceScanCount, 5)
    assertEquals(value.trace.netSourceScanDelta, 0)
    assertEquals(value.statistics.cteCount, 1L)
    assertEquals(value.statistics.expandedScanCount, 5L)
    assertEquals(value.statistics.sqlCharacterCount, sql.length.toLong)
    assertEquals(
      value.output.map(column =>
        (column.ordinal, column.name, column.dataType, column.nullable)
      ),
      Vector((0, "exact_count", "BIGINT NOT NULL", false))
    )

    assert(
      sql.startsWith("WITH \"yr_v4_shared_wing\" AS MATERIALIZED (\n"),
      clues(sql)
    )
    assertEquals(occurrences(sql, " AS MATERIALIZED ("), 1)
    assertEquals(occurrences(sql, "\"yr_v4_shared_wing\""), 3)
    assert(sql.contains("\n    UNION\n"), clues(sql))
    assert(!sql.contains("UNION ALL"), clues(sql))
    assert(sql.contains("SUM(CAST(1 AS NUMERIC)) AS \"yr_freq\""), clues(sql))
    assert(sql.contains("GROUP BY \"yr_v4_demand\".\"yr_key\""), clues(sql))
    assert(
      sql.contains(
        "SUM(\"yr_v4_m0\".\"yr_freq\" * \"yr_v4_m1\".\"yr_freq\")"
      ),
      clues(sql)
    )
    assert(sql.contains("CAST(0 AS NUMERIC)"), clues(sql))
    assert(sql.contains(") AS BIGINT"), clues(sql))
    assertEquals(occurrences(sql, "\"v4_link\""), 3)
    assertEquals(occurrences(sql, "\"v4_fact\""), 1)
    assertEquals(occurrences(sql, "\"v4_dim\""), 1)
    assert(!sql.contains("IS NOT DISTINCT FROM"), clues(sql))
    assert(sql.contains("OPERATOR(pg_catalog.=)"), clues(sql))
    Vector("yr_s_b", "yr_f_b", "yr_cm_b", "_bu", "_td").foreach(
      marker => assert(!sql.contains(marker), clues(marker, sql))
    )
  }

  test("chooses a deterministic fresh CTE suffix after source-name collisions") {
    val collisionPrepared = prepare(collisionSql)
    val collisionEvidence = baseTableEvidence(
      Vector("yr_v4_shared_wing", "yr_v4_shared_wing_1", "v4_dim")
    )
    val first = renderedFor(collisionPrepared, collisionEvidence)
    val repeated = renderedFor(collisionPrepared, collisionEvidence)

    assertEquals(first, repeated)
    assertEquals(first.trace.sharedCteName, "yr_v4_shared_wing_2")
    assert(
      first.sql.startsWith(
        "WITH \"yr_v4_shared_wing_2\" AS MATERIALIZED (\n"
      ),
      clues(first.sql)
    )
    assertEquals(occurrences(first.sql, "\"yr_v4_shared_wing_2\""), 3)
    assertEquals(occurrences(first.sql, "\"yr_v4_shared_wing\""), 3)
    assertEquals(occurrences(first.sql, "\"yr_v4_shared_wing_1\""), 1)
  }

  test("requires purity evidence for the anchor and selected wing relations") {
    val incomplete = baseTableEvidence(Vector("v4_link", "v4_fact"))
    val failure = PostgreSqlRelevanceLimitedCountRenderer
      .render(
        prepared.query,
        prepared.input,
        prepared.plan,
        incomplete
      )
      .left
      .getOrElse(fail("expected incomplete-evidence rejection"))

    assertEquals(
      failure.failureClass,
      PostgreSqlRelevanceLimitedCountFailureClass.NotApplicable
    )
    assertEquals(
      failure.code,
      "relevance-limited-count.not-applicable.base-table-evidence"
    )
    assert(failure.detail.contains("v4_dim"), clues(failure))
  }

  test("enforces exact custom SQL resource accounting") {
    val ordinary = rendered()
    val statistics = ordinary.statistics
    val exact = limits(
      maxExpandedScans = statistics.expandedScanCount,
      maxRenderedTerms = statistics.renderedTermCount,
      maxSqlCharacters = statistics.sqlCharacterCount
    )
    assert(
      PostgreSqlRelevanceLimitedCountRenderer
        .render(
          prepared.query,
          prepared.input,
          prepared.plan,
          evidence,
          exact
        )
        .isRight
    )

    Vector(
      "expanded-scans" -> limits(
        maxExpandedScans = statistics.expandedScanCount - 1L
      ),
      "rendered-terms" -> limits(
        maxRenderedTerms = statistics.renderedTermCount - 1L
      ),
      "sql-characters" -> limits(
        maxSqlCharacters = statistics.sqlCharacterCount - 1L
      )
    ).foreach { case (suffix, constrained) =>
      val failure = PostgreSqlRelevanceLimitedCountRenderer
        .render(
          prepared.query,
          prepared.input,
          prepared.plan,
          evidence,
          constrained
        )
        .left
        .getOrElse(fail(s"expected one-below $suffix rejection"))
      assertEquals(
        failure.failureClass,
        PostgreSqlRelevanceLimitedCountFailureClass.ResourceLimited
      )
      assertEquals(failure.code, s"relevance-limited-count.resource.$suffix")
    }
  }

  test("the additive renderer does not change established no-reduction bytes") {
    val before = legacyNoReduction()
    rendered()
    val after = legacyNoReduction()

    assertEquals(after, before)
    assertEquals(before.sql.length, 1576)
    assertEquals(
      sha256(before.sql),
      "ab101aaeb191de009d00f616fbc560dd16e4f1c01153f5750284687fbbbed027"
    )
  }

  private def rendered(
      customLimits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default
  ): PostgreSqlRelevanceLimitedCountRenderedQuery =
    renderedFor(prepared, evidence, customLimits)

  private def renderedFor(
      query: PostgreSqlRendererTestSupport.PreparedQuery,
      tableEvidence: PostgreSqlRelevanceLimitedBaseTableEvidence,
      customLimits: PostgreSqlRenderingLimits = PostgreSqlRenderingLimits.default
  ): PostgreSqlRelevanceLimitedCountRenderedQuery =
    PostgreSqlRelevanceLimitedCountRenderer
      .render(
        query.query,
        query.input,
        query.plan,
        tableEvidence,
        customLimits
      )
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def legacyNoReduction(): PostgreSqlRenderedQuery =
    PostgreSqlBagFactorRenderer
      .compileAndRender(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.NoReduction
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), _._2)

  private def baseTableEvidence(
      names: Vector[String]
  ): PostgreSqlRelevanceLimitedBaseTableEvidence = {
    val relations = names.zipWithIndex.map { case (name, ordinal) =>
      val columns = tables
        .find(_.name == name)
        .getOrElse(fail(s"missing unit table $name"))
        .columns
        .zipWithIndex
        .map { case (column, columnOrdinal) =>
          PostgreSqlRelevanceLimitedBaseColumn(
            column.name,
            attributeNumber = columnOrdinal + 1,
            typeOid = PostgreSqlRelevanceLimitedBaseColumn.IntegerOid,
            structural = true
          )
        }
      PostgreSqlRelevanceLimitedBaseRelation(
        Vector(name),
        databaseOid = 100L,
        namespaceOid = 200L,
        relationOid = 300L + ordinal,
        relkind = "r",
        hasSubclass = false,
        rowSecurity = false,
        forceRowSecurity = false,
        columns = columns
      )
    }
    PostgreSqlRelevanceLimitedBaseTableEvidence
      .create("unit_v4", 180004, relations)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
  }

  private def limits(
      maxExpandedScans: Long = PostgreSqlRenderingLimits.default.maxExpandedScans,
      maxRenderedTerms: Long = PostgreSqlRenderingLimits.default.maxRenderedTerms,
      maxSqlCharacters: Long = PostgreSqlRenderingLimits.default.maxSqlCharacters
  ): PostgreSqlRenderingLimits =
    PostgreSqlRenderingLimits
      .create(
        maxExpandedScans = maxExpandedScans,
        maxRenderedTerms = maxRenderedTerms,
        maxSqlCharacters = maxSqlCharacters
      )
      .fold(problems => fail(problems.mkString("; ")), identity)

  private def occurrences(haystack: String, needle: String): Int = {
    require(needle.nonEmpty)
    var count = 0
    var position = haystack.indexOf(needle)
    while (position >= 0) {
      count += 1
      position = haystack.indexOf(needle, position + needle.length)
    }
    count
  }

  private def sha256(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
