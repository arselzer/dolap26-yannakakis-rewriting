package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

final class PostgreSqlGroupedBranchCountSuite extends munit.FunSuite {
  import PostgreSqlGroupedBranchCountRenderer.Options
  import PostgreSqlScalarBranchCountRenderer.Strategy
  private def b(i: Int) = BagId.from(i).toOption.get
  private def v(i: Int) = VariableId.from(i).toOption.get
  private val factors = Vector(b(0) -> Vector(v(0),v(1),v(4)),
    b(1) -> Vector(v(0),v(2)), b(2) -> Vector(v(1),v(3)))
  private val schema = Frameworks.createRootSchema(true)
  Vector("r"->Vector("x","y","c"), "p"->Vector("c"), "s"->Vector("x","a","d"),
    "t"->Vector("y","b"), "u"->Vector("a"), "w"->Vector("b"), "z"->Vector("d")).foreach {
    case (name,columns) => schema.add(name,new AbstractTable {
      override def getRowType(f: RelDataTypeFactory): RelDataType = {
        val builder=f.builder(); columns.foreach(c=>builder.add(c,SqlTypeName.INTEGER).nullable(true)); builder.build()
      }
    })
  }
  private val planner = CalciteSqlPlanner.forSchemaWithDistributedCountGrouping(schema,4).toOption.get
  private val rewriter = PostgreSqlQueryRewriter.create(planner).toOption.get
  private val from = " FROM r JOIN p ON r.c=p.c JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b"
  private def sql(keys: Vector[String], suffix: String = "") =
    "SELECT " + keys.map(_ + " AS same").mkString(",") + ", COUNT(*) AS \"n\"\"rows\"" + from + suffix + " GROUP BY " + keys.mkString(",")
  private def artifact(text: String, width: Int = 1) = {
    val a=rewriter.rewriteDetailed(RewriteRequest(text, options=RewriteOptions(widthBound=width,planLimit=1)))
    a.report.artifact.getOrElse(fail(a.outcome.toString))
  }
  private def prepared(text: String) = {
    val a=artifact(text)
    PostgreSqlGroupedBranchCountRenderer.prepare(a.query,a.candidateInput,a.bagFactorPlan)
      .fold(p=>fail(p.map(_.message).mkString(";")),identity)
  }

  test("group placement takes shared values at the anchor and retains local anchor keys") {
    val keys=Vector(v(3),v(0),v(4),v(2))
    val s=PostgreSqlFactorStar.certifyGrouping(factors,keys).head
    assertEquals(s.providers,Vector(b(2),b(0),b(0),b(1)))
    assertEquals(s.anchorKeys,Vector(v(0),v(1),v(4)))
    assertEquals(s.keysFor(b(1)),Vector(v(0),v(2)))
    assertEquals(PostgreSqlFactorStar.certifyGrouping(factors.reverse,keys),Vector(s))
  }

  test("malformed, missing, duplicate keys and cross-branch dependencies fail closed") {
    Vector(null,Vector.empty,Vector(v(99)),Vector(v(2),v(2)),Vector(null)).foreach { keys =>
      assert(PostgreSqlFactorStar.certifyGrouping(factors,keys).isEmpty)
    }
    assert(PostgreSqlFactorStar.certifyGrouping(null,Vector(v(0))).isEmpty)
    val triangle=Vector(b(0)->Vector(v(0),v(1)),b(1)->Vector(v(0),v(2)),b(2)->Vector(v(1),v(2)))
    assert(PostgreSqlFactorStar.certifyGrouping(triangle,Vector(v(2))).isEmpty)
  }

  test("one through four caller-admitted keys, permutations and both strategies render") {
    val keySets=Vector(Vector("r.x"),Vector("p.c"),Vector("s.a"),Vector("s.a","t.b"),
      Vector("r.x","s.a","t.b"),Vector("p.c","s.a","t.b"),Vector("p.c","s.a","t.b","r.y"))
    keySets.foreach { keys =>
      for (ordered <- Vector(keys,keys.reverse)) {
        val p=prepared(sql(ordered)); val candidates=p.candidates.toOption.get
        assertEquals(candidates.size,4)
        candidates.foreach { c =>
          assert(c.rendered.sql.contains("SUM(CAST(1 AS NUMERIC))"))
          assert(c.rendered.sql.contains("CAST(SUM("))
          assert(!c.rendered.sql.contains("COALESCE"))
          assert(!c.rendered.sql.contains("_bu") && !c.rendered.sql.contains("_td"))
          assertEquals(c.rendered.output.map(_.name),Vector.fill(keys.size)("same") :+ "n\"rows")
        }
        assertEquals(p.candidates.toOption.get,candidates)
      }
    }
  }

  test("anchor extra groups and proper-subset boundaries require deduplicated domains") {
    val r=prepared(sql(Vector("p.c","s.a","t.b"))).render(Options(Strategy.PerKey)).toOption.get
    assert(r.sql.contains("SELECT DISTINCT"))
    val shared=prepared(sql(Vector("r.x","s.a","t.b")))
    assert(shared.render(Options(Strategy.RestrictedGroup)).toOption.get.sql.contains("WHERE EXISTS"))
  }

  test("two output keys in one branch contribute that branch only once") {
    val text=sql(Vector("u.a","z.d")," JOIN z ON z.d=s.d")
    val p=prepared(text)
    val r=p.render(Options(Strategy.RestrictedGroup)).toOption.get
    assertEquals(r.sql.split(java.util.regex.Pattern.quote("SUM(CAST(1 AS NUMERIC))"),-1).length-1,3)
    assertEquals(r.output.size,3)
  }

  test("selected keys keep their exact source binding even across equivalent join columns") {
    for (keys <- Vector(Vector("p.c","s.a","t.b"), Vector("s.x","t.b"), Vector("u.a","w.b"))) {
      val a=artifact(sql(keys))
      val c=PostgreSqlBagFactorRenderer.compileValidatedWithEnumerationProfile(a.query,a.candidateInput,a.bagFactorPlan,
        PostgreSqlRenderingLimits.default,at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits.default,
        at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode.FullTwoPass,
        PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2).toOption.get
      val terminal=c.terminal.asInstanceOf[CompiledPostgreSqlTerminal.DistributedGroupedCountStar]
      prepared(sql(keys)).candidates.toOption.get.foreach { candidate =>
        terminal.groupColumns.zip(terminal.groupVariables).zipWithIndex.foreach { case ((column,key),ordinal) =>
          val edge=c.edgeByOccurrence(column.ref.relationOccurrenceId)
          val owner=c.bags.find(_.factor.ownedEdgeIds.contains(edge.id)).get.factor.bagId
          val source=c.metadataByColumn(column.ref.id).sourceName
          val projection="\""+PostgreSqlNames.factorScan(owner,edge.id)+"\".\""+source+"\" AS \""+PostgreSqlNames.variable(key)+"\""
          assert(candidate.rendered.sql.contains(projection),projection)
          val alias=if(owner==candidate.rendered.anchor)"a" else "b"+owner.value
          assert(candidate.rendered.sql.contains(alias+".\""+PostgreSqlNames.variable(key)+"\" AS \"same\""))
        }
      }
    }
  }

  test("physical order, option errors and exact SQL length are validated") {
    val p=prepared(sql(Vector("r.x","s.a","t.b")))
    val r=p.render(Options(Strategy.PerKey)).toOption.get
    assertEquals(p.render(Options(Strategy.PerKey,maximumCharacters=r.sql.length)).toOption.get,r)
    assert(p.render(Options(Strategy.PerKey,maximumCharacters=r.sql.length-1)).isLeft)
    Vector(Options(null),Options(Strategy.PerKey,anchor=Some(b(99))),
      Options(Strategy.PerKey,branchOrder=null),Options(Strategy.PerKey,branchOrder=Vector(p.anchor)),
      Options(Strategy.PerKey,branchOrder=p.branchOrder :+ p.branchOrder.head),
      Options(Strategy.PerKey,maximumCharacters=0),Options(Strategy.PerKey,maximumCharacters=1048577)).foreach(o=>assert(p.render(o).isLeft))
    assert(p.render(null).isLeft)
  }

  test("revalidates ownership and retains the old U1/default paths unchanged") {
    val text=sql(Vector("s.a","t.b")); val a=artifact(text)
    val old=PostgreSqlAcyclicBranchCountRenderer.render(a.query,a.candidateInput,a.bagFactorPlan).toOption.get
    val p=prepared(text); assert(p.candidates.isRight)
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.render(a.query,a.candidateInput,a.bagFactorPlan).toOption.get,old)
    assertEquals(artifact(text).renderedQuery.sql,a.renderedQuery.sql)
    val bad=a.bagFactorPlan.copy(factors=a.bagFactorPlan.factors.updated(0,a.bagFactorPlan.factors.head.copy(ownedEdgeIds=Vector.empty)))
    assert(PostgreSqlGroupedBranchCountRenderer.prepare(a.query,a.candidateInput,bad).isLeft)
    val wide=artifact(text,2)
    assert(PostgreSqlGroupedBranchCountRenderer.prepare(wide.query,wide.candidateInput,wide.bagFactorPlan).isLeft)
    val scalar=artifact("SELECT COUNT(*)"+from)
    assert(PostgreSqlGroupedBranchCountRenderer.prepare(scalar.query,scalar.candidateInput,scalar.bagFactorPlan).isLeft)
    assert(PostgreSqlGroupedBranchCountRenderer.prepare(null,null,null).isLeft)
  }

  test("seeded independent bag oracle covers anchor, shared and same-leaf keys including NULL groups") {
    type Row=Map[VariableId,Option[Int]]
    val random=new scala.util.Random(9102026)
    def compatible(a:Row,b:Row)=(a.keySet intersect b.keySet).forall(k=>a(k).nonEmpty && a(k)==b(k))
    for (_ <- 0 until 150) {
      val bags=factors.map { case (id,vs) => id -> Vector.fill(random.nextInt(5))(
        vs.map(v=>v -> (if(random.nextInt(4)==0) None else Some(random.nextInt(3)))).toMap)
      }.toMap
      val naive=factors.foldLeft(Vector(Map.empty[VariableId,Option[Int]])) { case (rows,(id,_)) =>
        for(a<-rows;b<-bags(id) if compatible(a,b)) yield a++b
      }
      Vector(Vector(v(4)),Vector(v(0),v(2)),Vector(v(4),v(2),v(3)),Vector(v(0),v(1),v(2),v(3))).foreach { keys =>
        val shape=PostgreSqlFactorStar.certifyGrouping(factors,keys).head
        def project(r:Row,ks:Vector[VariableId])=ks.map(k=>k->r(k)).toMap
        val expected=naive.groupMapReduce(r=>keys.map(r))(_=>BigInt(1))(_+_)
        val seed=bags(shape.star.anchor).filter(r=>shape.star.shared.forall(k=>r(k).nonEmpty))
          .groupMapReduce(r=>project(r,shape.anchorKeys))(_=>BigInt(1))(_+_).toVector
        for(order<-Vector(Vector(b(1),b(2)),Vector(b(2),b(1)))) {
          val weighted=order.foldLeft(seed) { case (rows,id) =>
            val counted=bags(id).groupMapReduce(r=>project(r,shape.keysFor(id)))(_=>BigInt(1))(_+_)
            for((a,n)<-rows;(r,m)<-counted.toVector if compatible(a,r)) yield (a++r,n*m)
          }
          val observed=weighted.groupMapReduce { case(r,_)=>keys.map(r) }(_._2)(_+_)
          assertEquals(observed,expected)
        }
      }
    }
  }
}
