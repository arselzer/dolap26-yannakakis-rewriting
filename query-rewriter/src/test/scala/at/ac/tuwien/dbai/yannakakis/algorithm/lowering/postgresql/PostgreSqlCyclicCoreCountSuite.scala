package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.core.{EdgeId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

final class PostgreSqlCyclicCoreCountSuite extends munit.FunSuite {
  private def e(i: Int) = EdgeId.from(i).toOption.get
  private def v(i: Int) = VariableId.from(i).toOption.get
  private val graph = Vector(Vector(0,1), Vector(1,2), Vector(2,0), Vector(0,3), Vector(3), Vector(2,4), Vector(4))
    .zipWithIndex.map { case (vs, i) => e(i) -> vs.map(v) }
  private val schema = Frameworks.createRootSchema(true)
  Vector("r" -> Vector("x","z"), "yr_cc_0" -> Vector("x","z"), "q" -> Vector("z","y"), "p" -> Vector("y","x"),
    "s" -> Vector("x","a"), "u" -> Vector("a"), "t" -> Vector("y","b"), "w" -> Vector("b")).foreach {
    case (name, cols) => schema.add(name, new AbstractTable {
      override def getRowType(f: RelDataTypeFactory): RelDataType = {
        val b = f.builder(); cols.foreach(c => b.add(c, SqlTypeName.INTEGER).nullable(true)); b.build()
      }
    })
  }
  private val rewriter = PostgreSqlQueryRewriter.create(
    CalciteSqlPlanner.forSchemaWithDistributedCountGrouping(schema, 4).toOption.get).toOption.get
  private val from = " FROM r JOIN q ON r.z=q.z JOIN p ON p.y=q.y AND p.x=r.x" +
    " JOIN s ON s.x=r.x JOIN u ON u.a=s.a JOIN t ON t.y=p.y JOIN w ON w.b=t.b"
  private def source(keys: Vector[String], suffix: String = "") = "SELECT " +
    keys.map(_ + " AS same").mkString(",") + ", COUNT(*) AS \"n\"\"rows\"" + from + suffix + " GROUP BY " + keys.mkString(",")
  private def artifact(sql: String) = {
    val a = rewriter.rewriteDetailed(RewriteRequest(sql,
      options = RewriteOptions(widthBound = 2, requireConnectedCover = false, planLimit = 1)))
    a.report.artifact.getOrElse(fail(a.outcome.toString))
  }
  private def render(sql: String) = {
    val a = artifact(sql)
    PostgreSqlCyclicCoreCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan)
      .fold(es => fail(es.map(_.message).mkString("; ")), identity)
  }

  test("recognizes maximal independent branches and is permutation invariant") {
    val shape = PostgreSqlCyclicCoreCountRenderer.recognize(graph, Vector(v(3), v(4))).get
    assertEquals(shape.core, Vector(e(0), e(1), e(2)))
    assertEquals(shape.branches.map(_.edges), Vector(Vector(e(3), e(4)), Vector(e(5), e(6))))
    assertEquals(PostgreSqlCyclicCoreCountRenderer.recognize(graph.reverse.map { case (id, vs) => id -> vs.reverse },
      Vector(v(4), v(3))), Some(shape))
    val renamed = graph.map { case (id, vs) => e(id.value + 20) -> vs.map(x => v(x.value + 10)) }
    val renamedShape = PostgreSqlCyclicCoreCountRenderer.recognize(renamed, Vector(v(13), v(14))).get
    assertEquals(renamedShape.core.map(_.value), shape.core.map(_.value + 20))
  }

  test("cross-branch dependencies, absent private keys, acyclic and disconnected graphs decline") {
    val dependent = graph :+ (e(7) -> Vector(v(3), v(4)))
    assert(PostgreSqlCyclicCoreCountRenderer.recognize(dependent, Vector(v(3), v(4))).isEmpty)
    assert(PostgreSqlCyclicCoreCountRenderer.recognize(graph, Vector(v(0), v(1))).isEmpty)
    assert(PostgreSqlCyclicCoreCountRenderer.recognize(graph.filterNot(_._1 == e(2)), Vector(v(3))).isEmpty)
    assert(PostgreSqlCyclicCoreCountRenderer.recognize(graph :+ (e(7) -> Vector(v(99))), Vector(v(3))).isEmpty)
    assert(PostgreSqlCyclicCoreCountRenderer.recognize(graph, Vector(v(3), v(3))).isEmpty)
    assert(PostgreSqlCyclicCoreCountRenderer.recognize(graph ++ graph, Vector(v(3))).isEmpty)
    assert(PostgreSqlCyclicCoreCountRenderer.recognize(null, null).isEmpty)
  }

  test("emission order is connected even when lookup occurrences have the lowest IDs") {
    val shuffled = Vector(e(0) -> Vector(v(8)), e(1) -> Vector(v(9)), e(2) -> Vector(v(7)),
      e(10) -> Vector(v(0),v(8)), e(11) -> Vector(v(1),v(9)),
      e(12) -> Vector(v(0),v(1),v(7)))
    val order = PostgreSqlCyclicCoreCountRenderer.connectedOrder(shuffled, Vector(v(0),v(1)))
    assertEquals(order.head, e(12))
    assertEquals(order.toSet, shuffled.map(_._1).toSet)
    assertEquals(PostgreSqlCyclicCoreCountRenderer.connectedOrder(shuffled.reverse, Vector(v(1),v(0))), order)
    order.indices.drop(1).foreach { i =>
      val seen = order.take(i).flatMap(shuffled.toMap).toSet
      assert(shuffled.toMap.apply(order(i)).exists(seen))
    }
  }

  test("one through four keys and boundary-origin routing use three or two counted CTEs") {
    for (keys <- Vector(Vector("u.a"), Vector("s.a", "w.b"), Vector("u.a", "w.b", "q.z"),
        Vector("s.x", "u.a", "w.b"), Vector("u.a", "w.b", "q.z", "t.y"))) {
      for (order <- Vector(keys, keys.reverse)) {
        val r = render(source(order))
        assertEquals(r.output.map(_.name), Vector.fill(keys.size)("same") :+ "n\"rows")
        assertEquals(r.sql.sliding("AS MATERIALIZED".length).count(_ == "AS MATERIALIZED"), 1 + r.shape.branches.size)
        assert(r.sql.contains("SUM(CAST(1 AS NUMERIC))"))
        assert(r.sql.contains("CAST(SUM("))
        assert(r.sql.contains("EXISTS (SELECT 1 FROM"))
        assert(!r.sql.contains("_bu") && !r.sql.contains("_td") && !r.sql.contains("DISTINCT"))
        assertEquals(render(source(order)), r)
      }
    }
  }

  test("character bound is exact, malformed ownership and unsupported terminals fail closed") {
    val a = artifact(source(Vector("s.a", "w.b")))
    val r = render(source(Vector("s.a", "w.b")))
    assertEquals(PostgreSqlCyclicCoreCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan, r.sql.length).toOption.get, r)
    assert(PostgreSqlCyclicCoreCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan, r.sql.length - 1).isLeft)
    val bad = a.bagFactorPlan.copy(factors = a.bagFactorPlan.factors.updated(0,
      a.bagFactorPlan.factors.head.copy(ownedEdgeIds = Vector.empty)))
    assert(PostgreSqlCyclicCoreCountRenderer.render(a.query, a.candidateInput, bad).isLeft)
    val scalar = artifact("SELECT COUNT(*)" + from)
    assert(PostgreSqlCyclicCoreCountRenderer.render(scalar.query, scalar.candidateInput, scalar.bagFactorPlan).isLeft)
    assert(PostgreSqlCyclicCoreCountRenderer.render(null, null, null).isLeft)
  }

  test("unqualified source names cannot be captured by the new CTE namespace") {
    val a = artifact(source(Vector("s.a", "w.b")).replace("FROM r JOIN", "FROM yr_cc_0 r JOIN"))
    val result = PostgreSqlCyclicCoreCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan)
    assert(result.left.toOption.exists(_.exists(_.message.contains("captured"))))
  }

  test("pool v2 preserves v1 action bytes and adds bounded deterministic alternatives, not a selected winner") {
    val a = artifact(source(Vector("u.a", "w.b", "q.z")))
    val old = PostgreSqlAggregateCandidatePool.generate(a.query, a.candidateInput, a.bagFactorPlan).toOption.get
    val next = PostgreSqlAggregateCandidatePoolV2.generate(a.query, a.candidateInput, a.bagFactorPlan).toOption.get
    assertEquals(next.actions.take(old.actions.size), old.actions)
    assert(next.uniqueActions.exists(_.name == "cyclic-core-count"))
    assert(next.declines.exists(d => d.name.startsWith("partial-inline-b") && d.errors.exists(_.message.contains("origin"))))
    val canonical = artifact(source(Vector("s.a", "t.b", "r.z")))
    val safe = PostgreSqlAggregateCandidatePoolV2.generate(canonical.query, canonical.candidateInput, canonical.bagFactorPlan).toOption.get
    assert(safe.actions.exists(_.name.startsWith("partial-inline-b")))
    assert(next.actions.size <= PostgreSqlAggregateCandidatePoolV2.MaximumRewrites)
    assertEquals(next.uniqueActions.map(_.sql).distinct.size, next.uniqueActions.size)
    assertEquals(PostgreSqlAggregateCandidatePoolV2.generate(a.query, a.candidateInput, a.bagFactorPlan).toOption.get, next)
    assertEquals(artifact(source(Vector("u.a", "w.b", "q.z"))).renderedQuery.sql, a.renderedQuery.sql)
    assert(PostgreSqlAggregateCandidatePoolV2.generate(null, null, null).isLeft)
  }

  test("independent bag algebra agrees for random duplicates, NULLs, empty branches and merged groups") {
    type Row = Map[VariableId, Option[Int]]
    val random = new scala.util.Random(9102026)
    def compatible(a: Row, b: Row) = (a.keySet intersect b.keySet).forall(k => a(k).nonEmpty && a(k) == b(k))
    for (_ <- 0 until 200) {
      val data = graph.map { case (id, vs) => id -> (Vector(vs.map(_ -> Some(1)).toMap) ++
        Vector.fill(random.nextInt(5))(vs.map(k => k -> (if (random.nextInt(4) == 0) None else Some(random.nextInt(3)))).toMap))
      }.toMap
      def join(ids: Vector[EdgeId]) = ids.foldLeft(Vector(Map.empty[VariableId, Option[Int]])) { (rows, id) =>
        for (a <- rows; b <- data(id) if compatible(a, b)) yield a ++ b
      }
      for (keys <- Vector(Vector(v(3)), Vector(v(3), v(4)), Vector(v(0), v(3), v(4)), Vector(v(1), v(3), v(4)))) {
        val shape = PostgreSqlCyclicCoreCountRenderer.recognize(graph, keys).get
        val expected = join(graph.map(_._1)).groupMapReduce(r => keys.map(r))(_ => BigInt(1))(_ + _)
        val parts = Vector(shape.core) ++ shape.branches.map(_.edges)
        val counted = parts.zipWithIndex.map { case (ids, i) =>
          val available = ids.flatMap(graph.toMap).toSet
          val boundary = if (i == 0) shape.branches.map(_.boundary) else Vector(shape.branches(i - 1).boundary)
          val live = (boundary ++ keys.filter(available)).distinct
          join(ids).filter(_.values.forall(_.nonEmpty)).groupMapReduce(r => live.map(k => k -> r(k)).toMap)(_ => BigInt(1))(_ + _).toVector
        }
        val weighted = counted.tail.foldLeft(counted.head) { (left, right) =>
          for ((a, n) <- left; (b, m) <- right if compatible(a, b)) yield (a ++ b, n * m)
        }
        assertEquals(weighted.groupMapReduce { case (r, _) => keys.map(r) }(_._2)(_ + _), expected)
      }
    }
  }
}
