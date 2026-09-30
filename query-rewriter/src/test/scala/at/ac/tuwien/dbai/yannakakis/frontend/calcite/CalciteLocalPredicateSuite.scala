package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks
import org.apache.calcite.sql.parser.SqlParser
import org.apache.calcite.sql.{SqlIdentifier, SqlSelect}
import scala.jdk.CollectionConverters._
import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter

class CalciteLocalPredicateSuite extends munit.FunSuite {
  private val schema = Frameworks.createRootSchema(true)
  schema.add("edge", new AbstractTable {
    override def getRowType(factory: RelDataTypeFactory): RelDataType = factory.builder()
      .add("a", SqlTypeName.INTEGER).nullable(true)
      .add("b", SqlTypeName.INTEGER).nullable(true)
      .add("n", SqlTypeName.SMALLINT).nullable(true)
      .add("label", SqlTypeName.VARCHAR, 80).nullable(true)
      .add("ts", SqlTypeName.TIMESTAMP).nullable(true)
      .add("date", SqlTypeName.DATE).nullable(true)
      .add("flag", SqlTypeName.BOOLEAN).nullable(true).build()
  })
  private val capable = CalciteSqlPlanner.forSchemaWithLocalPredicates(schema, 3).toOption.get
  private val legacy = CalciteSqlPlanner.forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig).toOption.get
  private val from = " FROM edge e0 JOIN edge e1 ON e0.b=e1.a JOIN edge e2 ON e1.b=e2.a AND e2.b=e0.a"
  private def planned(sql: String, planner: CalciteSqlPlanner = capable): CalciteSqlPlannedQuery =
    CalciteSqlPlanner.planWithSourceEvidence(planner, sql).fold(p => fail(p.toString), identity)
  private def analyze(sql: String): AnalysisReport = CalciteQueryAnalyzer.analyze(planned(sql))

  test("PostgreSQL qualified keywords use Calcite tokens, preserving identifiers and source positions") {
    val metadata = SqlParser.create("", CalciteSqlPlanner.PostgreSqlParserConfig).getMetadata
    val keywords = metadata.getTokens.asScala.filter(metadata.isKeyword)
    assert(keywords.size > 100)
    keywords.foreach { keyword =>
      val select = SqlParser.create("SELECT e." + keyword + " FROM edge e",
        CalciteSqlPlanner.PostgreSqlQualifiedNameParserConfig).parseQuery().asInstanceOf[SqlSelect]
      val id = select.getSelectList.get(0).asInstanceOf[SqlIdentifier]
      assertEquals(id.names.asScala.toVector, Vector("e", keyword.toLowerCase(java.util.Locale.ROOT)))
      assertEquals(id.getComponentParserPosition(1).getColumnNum, 10)
    }
    Vector("e. /* comment with . SELECT */ Date", "e.\n-- Date is a column\nDATE", "e.\"date\"").foreach { ref =>
      val report = analyze("SELECT COUNT(*)" + from + " WHERE " + ref.replace("e.", "e0.") +
        " >= CAST('2010-07-19 00:00:00' AS TIMESTAMP)")
      assert(report.isSupported, clues(ref, report.diagnostics))
    }
    assert(CalciteSqlPlanner.planWithSourceEvidence(capable,
      "SELECT COUNT(*)" + from + " WHERE e0.\"Date\" IS NULL").isLeft)
    val reusable = SqlParser.create("SELECT e.Date FROM edge e",
      CalciteSqlPlanner.PostgreSqlQualifiedNameParserConfig)
    assert(reusable.parseQuery().isInstanceOf[SqlSelect])
    assert(reusable.parseQuery("SELECT e.SELECT FROM edge e").isInstanceOf[SqlSelect])
  }

  Vector(
    "e0.n >= -2", "e0.n BETWEEN -2 AND 4", "e0.label = 'character'",
    "e0.label IN ('actress','actor')", "e0.label < 'z'",
    "e0.label IS NOT NULL", "e0.label IS NULL", "e0.flag",
    "(e0.label LIKE '%An%' OR e0.label LIKE 'O''Brien%')",
    "e0.ts >= CAST('2010-07-19 00:00:00' AS TIMESTAMP)",
    "e0.Date >= CAST('2010-07-19 00:00:00' AS TIMESTAMP)",
    "e0.n >= -2 AND e1.n < 8 AND (e2.label='a' OR e2.label='b')"
  ).foreach { predicate =>
    test(s"source-certified local predicate: $predicate") {
      val sql = "SELECT COUNT(*)" + from + " WHERE " + predicate
      val report = analyze(sql)
      assert(report.isSupported, clues(report.diagnostics, report.query.predicates))
      assert(report.query.sourceProof.isBoundTo(report.query))
      assert(CalciteLocalPredicates.concordant(report.query.sourceProof.localPredicateCertificate.get, report.query))
      val result = PostgreSqlQueryRewriter.create(capable).toOption.get.rewriteDetailed(
        RewriteRequest(sql, options = RewriteOptions(widthBound=2, planLimit=1)))
      assert(result.report.artifact.nonEmpty, clues(result.outcome))
    }
  }

  test("legacy rejection and generic three-key grouping remain separate capabilities") {
    val sql = "SELECT e0.a,e1.a,e2.a,COUNT(*)" + from +
      " WHERE e0.n > -4 GROUP BY e0.a,e1.a,e2.a"
    assert(analyze(sql).isSupported)
    assert(!CalciteQueryAnalyzer.analyze(planned(sql, legacy)).isSupported)
  }

  test("filters retain self-join occurrences and also work for enumeration") {
    val report = analyze("SELECT e0.a,e1.a" + from + " WHERE e0.label='a' AND e1.label='b'")
    assert(report.isSupported, clues(report.diagnostics))
    assertEquals(report.query.predicates.collect {
      case p if p.expression.isInstanceOf[ScalarExpression.LocalPredicate] =>
        p.expression.columnRefs.head.relationOccurrenceId.value
    }, Vector(0, 1))
  }

  test("source-only casts, subqueries, cross-occurrence OR and unsupported literals fail closed") {
    Vector("CAST(e0.n AS INTEGER)>1", "e0.n IN (SELECT a FROM edge)",
      "e0.n>1 OR e1.n>1", "e0.label LIKE 'a\\_%'", "e0.n IN (1,NULL)",
      "e0.n + 1 > 2", "e0.ts > CAST('2010-01-01 00:00:00.1' AS TIMESTAMP)",
      "e0.ts > CAST('2010-01-01 00:00:00' AS TIMESTAMP(0))").foreach { p =>
      val report = analyze("SELECT COUNT(*)" + from + " WHERE " + p)
      assert(!report.isSupported, clues(p, report))
    }
  }

  test("source-certificate and QueryIR proof transplantation fail closed") {
    val a = planned("SELECT COUNT(*)" + from + " WHERE e0.label='a'")
    val b = planned("SELECT COUNT(*)" + from + " WHERE e1.label='a'")
    assert(!CalciteQueryAnalyzer.analyze(b.copy(localPredicateCertificate=a.localPredicateCertificate)).isSupported)
    val qa = CalciteQueryAnalyzer.analyze(a).query
    val qb = CalciteQueryAnalyzer.analyze(planned("SELECT COUNT(*)" + from + " WHERE e0.label='a'")).query
    assert(!qa.sourceProof.isBoundTo(qb))
    assert(qa.sourceProof.isBoundTo(qa.copy()))
  }
}
