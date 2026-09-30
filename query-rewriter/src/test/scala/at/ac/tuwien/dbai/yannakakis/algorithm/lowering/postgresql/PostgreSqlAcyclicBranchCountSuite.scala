package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest, YannakakisReductionMode}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

final class PostgreSqlAcyclicBranchCountSuite extends munit.FunSuite {
  private def b(n: Int) = BagId.from(n).toOption.get
  private def v(n: Int) = VariableId.from(n).toOption.get
  private val star = Vector(b(0) -> Vector(v(0), v(1)),
    b(1) -> Vector(v(0), v(2)), b(2) -> Vector(v(1), v(3)))

  test("certifies a factor star and preserves positional key providers") {
    val shape = PostgreSqlAcyclicBranchCountRenderer.certify(star, Vector(v(3), v(2))).get
    assertEquals(shape.anchor, b(0))
    assertEquals(shape.keyProviders, Vector(b(2), b(1)))
    assertEquals(shape.shared, Vector(v(0), v(1)))
    assertEquals(shape.boundaries(b(2)), Vector(v(1)))
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.certify(star.reverse, Vector(v(3), v(2))), Some(shape))
  }

  test("declines shared output keys, duplicate keys, disconnected and non-star factors") {
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.certify(star, Vector(v(0), v(2))), None)
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.certify(star, Vector(v(2), v(2))), None)
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.certify(star :+ (b(3) -> Vector(v(4))), Vector(v(2), v(3))), None)
    val path = Vector(b(0) -> Vector(v(0), v(2)), b(1) -> Vector(v(0), v(1)),
      b(2) -> Vector(v(1), v(4)), b(3) -> Vector(v(4), v(3)))
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.certify(path, Vector(v(2), v(3))), None)
  }

  test("permits two keys in one branch without squaring its multiplicity") {
    val factors = Vector(b(0) -> Vector(v(0)), b(1) -> Vector(v(0), v(1), v(2)),
      b(2) -> Vector(v(0), v(3)))
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.certify(factors, Vector(v(1), v(2))).get.keyProviders,
      Vector(b(1), b(1)))
  }

  test("bounds the shape and declines duplicate or malformed factor identities") {
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.certify(star :+ star.head, Vector(v(2), v(3))), None)
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.certify(star.updated(0, b(0) -> Vector(v(0), v(0))), Vector(v(2), v(3))), None)
    val large = star ++ (3 until 33).map(i => b(i) -> Vector(v(0), v(100 + i)))
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.certify(large, Vector(v(2), v(3))), None)
  }

  private val rewriter = {
    val schema = Frameworks.createRootSchema(true)
    Vector("r" -> Vector("x", "y"), "s" -> Vector("x", "a"), "t" -> Vector("y", "b"),
      "u" -> Vector("a"), "w" -> Vector("b")).foreach { case (name, columns) =>
      schema.add(name, new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType = {
          val builder = factory.builder()
          columns.foreach(c => builder.add(c, SqlTypeName.INTEGER).nullable(true))
          builder.build()
        }
      })
    }
    PostgreSqlQueryRewriter.create(CalciteSqlPlanner.forSchema(schema,
      CalciteSqlPlanner.PostgreSqlParserConfig).fold(p => fail(p.toString), identity))
      .fold(p => fail(p.message), identity)
  }
  private val sql = "SELECT s.a AS g1, t.b AS g2, COUNT(*) AS n " +
    "FROM r JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b " +
    "GROUP BY s.a,t.b"
  private def artifact(text: String = sql, width: Int = 1) = rewriter.rewriteDetailed(
    RewriteRequest(text, options = RewriteOptions(widthBound = width, planLimit = 1,
      requireConnectedCover = true, reductionMode = YannakakisReductionMode.FullTwoPass)))
    .report.artifact.getOrElse(fail("missing admitted artifact"))

  test("renders NUMERIC branch states with late key branches without changing default bytes") {
    val a = artifact()
    val result = PostgreSqlAcyclicBranchCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan)
      .fold(p => fail(p.map(_.message).mkString("; ")), identity)
    assert(result.sql.contains("SUM(CAST(1 AS NUMERIC))"))
    assert(result.sql.contains("AS NOT MATERIALIZED"))
    assert(result.sql.contains("WHERE EXISTS"))
    assert(result.sql.contains("CAST(SUM("))
    assert(!result.sql.contains("_td"))
    assert(!result.sql.contains("_bu"))
    assertEquals(result.factorCount, 3)
    assertEquals(result, PostgreSqlAcyclicBranchCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan).toOption.get)
    val again = artifact()
    assertEquals(a.renderedQuery.sql, again.renderedQuery.sql)
  }

  test("rejects a wider plan and non-U1 outputs") {
    val wide = artifact(width = 2)
    assert(PostgreSqlAcyclicBranchCountRenderer.render(wide.query, wide.candidateInput, wide.bagFactorPlan).isLeft)
    val scalar = artifact(sql.replace("s.a AS g1, t.b AS g2, ", "").replace(" GROUP BY s.a,t.b", ""))
    assert(PostgreSqlAcyclicBranchCountRenderer.render(scalar.query, scalar.candidateInput, scalar.bagFactorPlan).isLeft)
    assert(PostgreSqlAcyclicBranchCountRenderer.render(null, null, null).isLeft)
  }

  test("revalidates ownership rather than trusting a caller-modified plan") {
    val a = artifact()
    val first = a.bagFactorPlan.factors.head
    val changed = a.bagFactorPlan.copy(factors = a.bagFactorPlan.factors.updated(0,
      first.copy(ownedEdgeIds = Vector.empty)))
    assert(PostgreSqlAcyclicBranchCountRenderer.render(a.query, a.candidateInput, changed).isLeft)
  }
}
