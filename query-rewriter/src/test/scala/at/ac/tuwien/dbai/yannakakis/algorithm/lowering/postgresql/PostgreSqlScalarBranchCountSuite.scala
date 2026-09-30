package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest, YannakakisReductionMode}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

final class PostgreSqlScalarBranchCountSuite extends munit.FunSuite {
  import PostgreSqlScalarBranchCountRenderer.{Options, Strategy}
  private def b(i: Int) = BagId.from(i).toOption.get
  private def v(i: Int) = VariableId.from(i).toOption.get
  private val factors = Vector(b(0) -> Vector(v(0), v(1)),
    b(1) -> Vector(v(0), v(2)), b(2) -> Vector(v(1), v(3)))
  private val rewriter = {
    val schema = Frameworks.createRootSchema(true)
    Vector("r" -> Vector("x", "y"), "s" -> Vector("x", "a"), "t" -> Vector("y", "b"),
      "u" -> Vector("a"), "w" -> Vector("b"), "z" -> Vector("x", "c"),
      "q" -> Vector("c")).foreach { case (name, columns) =>
      schema.add(name, new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType = {
          val builder = factory.builder()
          columns.foreach(c => builder.add(c, SqlTypeName.INTEGER).nullable(true))
          builder.build()
        }
      })
    }
    PostgreSqlQueryRewriter.create(CalciteSqlPlanner.forSchema(schema,
      CalciteSqlPlanner.PostgreSqlParserConfig).toOption.get).toOption.get
  }
  private val from = " FROM r JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b"
  private val sql = "SELECT COUNT(*) AS \"total\"\"rows\"" + from
  private def artifact(text: String = sql, width: Int = 1) = rewriter.rewriteDetailed(
    RewriteRequest(text, options = RewriteOptions(widthBound = width, planLimit = 1,
      reductionMode = YannakakisReductionMode.FullTwoPass)))
    .report.artifact.getOrElse(fail("no admitted artifact"))
  private def render(text: String = sql, options: Options = Options(Strategy.PerKey)) = {
    val a = artifact(text)
    PostgreSqlScalarBranchCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan, options)
  }

  test("shared certificate finds exactly the anchors containing every shared variable") {
    val shapes = PostgreSqlFactorStar.certify(factors)
    assertEquals(shapes.map(_.anchor), Vector(b(0)))
    assertEquals(shapes.head.shared, Vector(v(0), v(1)))
    assertEquals(shapes.head.boundaries(b(1)), Vector(v(0)))
    assertEquals(PostgreSqlFactorStar.certify(factors.reverse), shapes)
    val oneKey = (0 until 3).map(i => b(i) -> Vector(v(0))).toVector
    assertEquals(PostgreSqlFactorStar.certify(oneKey).map(_.anchor), Vector(b(0), b(1), b(2)))
  }

  test("certificate fails closed for null, duplicate, disconnected, non-star and oversized input") {
    val invalid = Vector(null, Vector.empty, factors.take(2), factors :+ factors.head,
      factors.updated(0, b(0) -> Vector(v(0), v(0))), factors :+ (b(3) -> Vector(v(8))),
      Vector(b(0) -> Vector(v(0), v(1)), b(1) -> Vector(v(1), v(2)), b(2) -> Vector(v(2), v(0))),
      (0 until 33).map(i => b(i) -> Vector(v(0))).toVector)
    invalid.foreach(x => assertEquals(PostgreSqlFactorStar.certify(x), Vector.empty))
  }

  test("proper-subset boundaries deduplicate their domain and reattach anchor weights") {
    val r = render().toOption.get
    assert(r.sql.contains("SELECT DISTINCT"))
    assertEquals("CROSS JOIN LATERAL".r.findAllIn(r.sql).length, 2)
    assert(r.sql.contains("a.yr_freq * b.yr_freq"))
    assert(r.sql.contains("SUM(CAST(1 AS NUMERIC))"))
    assert(!r.sql.contains("COUNT(*)"))
    assert(r.sql.contains("COALESCE(SUM(yr_freq), CAST(0 AS NUMERIC))"))
    assert(r.sql.contains("AS \"total\"\"rows\""))
    assert(!r.sql.contains("_td") && !r.sql.contains("_bu"))
  }

  test("full shared boundary uses the unique seed directly without an extra count CTE") {
    val r = render(sql.replace("r.y=t.y", "r.x=t.y") +
      " JOIN z ON z.x=r.x JOIN q ON q.c=z.c").toOption.get
    assert(!r.sql.contains("SELECT DISTINCT"))
    assert(!r.sql.contains("yr_sc_count_"))
    assert(r.sql.contains("AS a CROSS JOIN LATERAL"))
  }

  test("bulk restricted strategy shares the same shape and preserves numeric weights") {
    val r = render(options = Options(Strategy.RestrictedGroup)).toOption.get
    assert(r.sql.contains("WHERE EXISTS"))
    assert(!r.sql.contains("LATERAL"))
    assert(r.sql.contains("SUM(CAST(1 AS NUMERIC))"))
    assertNotEquals(r.actionId, render().toOption.get.actionId)
  }

  test("physical order is explicit, deterministic and bound into the action identity") {
    val first = render().toOption.get
    assertEquals(first, render().toOption.get)
    val reversed = render(options = Options(Strategy.PerKey, branchOrder = first.branchOrder.reverse)).toOption.get
    assertNotEquals(first.sql, reversed.sql)
    assertNotEquals(first.actionId, reversed.actionId)
    Vector(first.branchOrder.drop(1), first.branchOrder :+ first.branchOrder.head,
      Vector(first.anchor) ++ first.branchOrder.drop(1)).foreach { order =>
      assert(render(options = Options(Strategy.PerKey, branchOrder = order)).isLeft)
    }
    assert(render(options = Options(Strategy.PerKey, anchor = Some(b(99)))).isLeft)
  }

  test("exact SQL-character boundary passes, one below fails, invalid envelopes decline") {
    val first = render().toOption.get
    assertEquals(render(options = Options(Strategy.PerKey, maximumCharacters = first.sql.length)).toOption.get.sql, first.sql)
    assert(render(options = Options(Strategy.PerKey, maximumCharacters = first.sql.length - 1)).isLeft)
    assert(render(options = Options(Strategy.PerKey, maximumCharacters = 0)).isLeft)
    assert(render(options = Options(Strategy.PerKey, maximumCharacters = 1048577)).isLeft)
  }

  test("revalidates ownership, rejects wrong terminals and keeps default SQL unchanged") {
    val a = artifact()
    val modified = a.bagFactorPlan.copy(factors = a.bagFactorPlan.factors.updated(0,
      a.bagFactorPlan.factors.head.copy(ownedEdgeIds = Vector.empty)))
    assert(PostgreSqlScalarBranchCountRenderer.render(a.query, a.candidateInput, modified, Options(Strategy.PerKey)).isLeft)
    assert(render("SELECT s.a, t.b, COUNT(*) AS n" + from + " GROUP BY s.a,t.b").isLeft)
    val wide = artifact(width = 2)
    assert(PostgreSqlScalarBranchCountRenderer.render(wide.query, wide.candidateInput, wide.bagFactorPlan, Options(Strategy.PerKey)).isLeft)
    assert(PostgreSqlScalarBranchCountRenderer.render(null, null, null, null).isLeft)
    assert(render(options = Options(null)).isLeft)
    assert(render(options = Options(Strategy.PerKey, branchOrder = null)).isLeft)
    assertEquals(artifact().renderedQuery.sql, a.renderedQuery.sql)
  }

  test("independent bag oracle verifies star compression with shared subset boundaries and NULLs") {
    val random = new scala.util.Random(330909)
    type Row = Map[Int, Option[Int]]
    for (_ <- 0 until 200) {
      val columns = Vector(Vector(0, 1), Vector(0, 2), Vector(1, 3))
      val bags = columns.map(vs => Vector.fill(random.nextInt(5))(
        vs.map(v => v -> (if (random.nextInt(4) == 0) None else Some(random.nextInt(3)))).toMap))
      def compatible(a: Row, b: Row): Boolean = (a.keySet intersect b.keySet).forall(k =>
        a(k).nonEmpty && a(k) == b(k))
      val expected = bags.foldLeft(Vector(Map.empty[Int, Option[Int]])) { (rows, next) =>
        for (a <- rows; b <- next if compatible(a, b)) yield a ++ b
      }.size
      for (order <- Vector(Vector(1, 2), Vector(2, 1))) {
        val initial = bags.head.filter(r => r(0).nonEmpty && r(1).nonEmpty)
          .groupMapReduce(identity)(_ => BigInt(1))(_ + _)
        val weighted = order.foldLeft(initial) { (states, branch) =>
          states.flatMap { case (a, weight) =>
            val count = bags(branch).count(compatible(a, _))
            if (count == 0) None else Some(a -> (weight * count))
          }
        }
        assertEquals(weighted.values.sum, BigInt(expected))
      }
    }
  }

  test("prepared automatic pool preserves explicit SQL and identities without reanalysis") {
    val a = artifact()
    val prepared = PostgreSqlScalarBranchCountRenderer.prepare(a.query, a.candidateInput, a.bagFactorPlan).toOption.get
    val pool = prepared.candidates.toOption.get
    assertEquals(pool.size, 4)
    pool.foreach { candidate =>
      val r = candidate.rendered
      val direct = render(options = Options(r.strategy, Some(r.anchor), r.branchOrder)).toOption.get
      assertEquals(r, direct)
      assertEquals(candidate.stages.map(_.bagId), r.anchor +: r.branchOrder)
      assert(candidate.stages.forall(p => p.sql.startsWith("WITH\n") && p.sql.length <= 1048576))
    }
    assertEquals(prepared.candidates.toOption.get, pool)
    assertEquals(artifact().renderedQuery.sql, a.renderedQuery.sql)
  }

  test("typed branch probes preserve local factor definitions and expose source binding roles") {
    val a = artifact()
    val p = PostgreSqlScalarBranchCountRenderer.prepare(a.query, a.candidateInput, a.bagFactorPlan).toOption.get
    val probes = p.probes.toOption.get
    assertEquals(probes.count(_.kind == "raw"), a.bagFactorPlan.factors.size)
    assertEquals(probes.count(_.kind == "keys"), a.bagFactorPlan.factors.size)
    assertEquals(probes.count(_.kind == "survival"), a.bagFactorPlan.factors.size - 1)
    assert(probes.filter(_.kind == "survival").forall(_.sql.contains("EXISTS")))
    assert(p.branches.forall(b => b.boundaryColumns.nonEmpty && b.boundarySize <= b.sharedSize))
    assert(p.branches.flatMap(_.boundaryColumns).forall(_.relation.nonEmpty))
  }

  test("probe-free acquisition preserves every legacy action and returns no diagnostics") {
    val a = artifact()
    val p = PostgreSqlScalarBranchCountRenderer.prepare(a.query, a.candidateInput, a.bagFactorPlan).toOption.get
    assertEquals(p.candidatesWithoutProbes.toOption.get, p.candidates.toOption.get.map(_.copy(stages = Vector.empty)))
  }

  test("terminal collapse removes exactly the last weighted materialization in both boundary shapes") {
    for (text <- Vector(sql, sql.replace("r.y=t.y", "r.x=t.y") + " JOIN z ON z.x=r.x JOIN q ON q.c=z.c")) {
      val a = artifact(text)
      val p = PostgreSqlScalarBranchCountRenderer.prepare(a.query, a.candidateInput, a.bagFactorPlan).toOption.get
      val legacy = p.candidatesWithoutProbes.toOption.get
      p.terminalCollapsedCandidates.toOption.get.zip(legacy).foreach { case (candidate, old) =>
        val r = candidate.rendered
        val options = Options(r.strategy, Some(r.anchor), r.branchOrder)
        assertEquals(r, p.renderTerminalCollapsed(options).toOption.get)
        assertEquals(r.output, old.rendered.output)
        assertNotEquals(r.actionId, old.rendered.actionId)
        assert(r.sql.contains("SUM(a.yr_freq * b.yr_freq)"))
        assert(!r.sql.contains(s"\"yr_sc_step_${r.branchOrder.last.value}\" AS MATERIALIZED"))
        assertEquals(" AS MATERIALIZED".r.findAllIn(r.sql).length,
          " AS MATERIALIZED".r.findAllIn(old.rendered.sql).length - 1)
        assertEquals(p.renderTerminalCollapsed(options.copy(maximumCharacters = r.sql.length)).toOption.get, r)
        assert(p.renderTerminalCollapsed(options.copy(maximumCharacters = r.sql.length - 1)).isLeft)
        assert(candidate.stages.isEmpty)
      }
      assertEquals(p.candidatesWithoutProbes.toOption.get, legacy)
    }
  }
}
