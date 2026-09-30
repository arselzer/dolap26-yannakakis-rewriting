package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.RewriteReasonCode
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class CalcitePostgreSqlOutputLabelsSuite extends munit.FunSuite {
  private val schema = {
    val root = Frameworks.createRootSchema(true)
    Vector(
      "r" -> Vector("a", "b", "payload"),
      "s" -> Vector("b", "c", "payload"),
      "t" -> Vector("c", "a", "payload")
    ).foreach { case (tableName, columns) =>
      root.add(
        tableName,
        new AbstractTable {
          override def getRowType(
              typeFactory: RelDataTypeFactory
          ): RelDataType = {
            val builder = typeFactory.builder()
            columns.foreach(column =>
              builder.add(
                column,
                typeFactory.createTypeWithNullability(
                  typeFactory.createSqlType(SqlTypeName.INTEGER),
                  false
                )
              )
            )
            builder.build()
          }
        }
      )
    }
    root
  }

  private val planner = CalciteSqlPlanner
    .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
    .fold(problem => throw new IllegalStateException(problem.message), identity)

  private def plan(sql: String): CalciteSqlPlannedQuery =
    CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(s"${problem.stage.id}: ${problem.message}"), identity)

  private def analyze(sql: String): AnalysisReport =
    CalciteQueryAnalyzer.analyze(plan(sql))

  private val from =
    "FROM r JOIN s ON r.b = s.b JOIN t ON s.c = t.c AND t.a = r.a"

  test("join star expansion preserves PostgreSQL duplicate source labels") {
    val report = analyze(s"SELECT * $from")

    assert(report.isSupported, clues(report.diagnostics))
    assertEquals(
      report.query.output.map(_.name),
      Vector("a", "b", "payload", "b", "c", "payload", "c", "a", "payload")
    )
  }

  test("qualified stars and fixed items retain positional PostgreSQL labels") {
    val report = analyze(
      s"SELECT r.*, s.c, t.*, r.a = t.a, r.a AS \"EXPR$$0\" $from"
    )

    assert(report.isSupported, clues(report.diagnostics))
    assertEquals(
      report.query.output.map(_.name),
      Vector(
        "a",
        "b",
        "payload",
        "c",
        "c",
        "a",
        "payload",
        "?column?",
        "EXPR$0"
      )
    )
  }

  test("unaliased retained equality uses PostgreSQL question-column label") {
    val report = analyze(s"SELECT r.a = t.a $from")

    assert(report.isSupported, clues(report.diagnostics))
    assertEquals(report.query.output.map(_.name), Vector("?column?"))
  }

  test("derived-table star fails closed instead of certifying Calcite labels") {
    val report = analyze(
      s"SELECT d.* FROM (SELECT r.a, r.b, s.c, r.a = t.a $from) AS d"
    )

    assert(!report.isSupported, clues(report.diagnostics))
    assert(
      report.diagnostics.exists(diagnostic =>
        diagnostic.code == RewriteReasonCode.UnsupportedQueryShape &&
          diagnostic.context.get("sourceBoundary").contains("result-labels") &&
          diagnostic.context
            .get("reason")
            .exists(_.contains("nested query blocks"))
      ),
      clues(report.diagnostics)
    )
  }

  test("derived-table aggregate cannot bypass the single-query-block boundary") {
    val sql = s"SELECT COUNT(*) FROM (SELECT r.a, r.b, s.c $from) AS d"
    val planned = plan(sql)
    val report = CalciteQueryAnalyzer.analyze(planned)

    assertEquals(
      planned.sourceEvidence,
      CalciteSqlSourceEvidence.RejectedAggregate(
        CalciteRejectedAggregateReason.UnsupportedTopLevelAggregateShape
      )
    )
    assert(!report.isSupported, clues(report.diagnostics))
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate),
      clues(report.diagnostics)
    )
  }
}
