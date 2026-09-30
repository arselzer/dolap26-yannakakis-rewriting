package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

final class PostgreSqlAggregateCandidatePoolSuite extends munit.FunSuite {
  private val schema=Frameworks.createRootSchema(true)
  (Vector("r"->Vector("x","y"),"s"->Vector("x","a"),"t"->Vector("y","b"),
    "u"->Vector("a"),"w"->Vector("b")) ++ (0 to 6).map(i=>s"p$i"->Vector("a","b"))).foreach {
    case (name,columns)=>schema.add(name,new AbstractTable {
      override def getRowType(f:RelDataTypeFactory):RelDataType={
        val b=f.builder();columns.foreach(c=>b.add(c,SqlTypeName.INTEGER).nullable(true));b.build()
      }
    })
  }
  private val rewriter=PostgreSqlQueryRewriter.create(
    CalciteSqlPlanner.forSchemaWithDistributedCountGrouping(schema,4).toOption.get).toOption.get
  private val from=" FROM r JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b"
  private def artifact(source:String)=rewriter.rewriteDetailed(RewriteRequest(source,
    options=RewriteOptions(widthBound=1,planLimit=1))).report.artifact.getOrElse(fail(source))
  private def pool(source:String)={val a=artifact(source);PostgreSqlAggregateCandidatePool.generate(a.query,a.candidateInput,a.bagFactorPlan).toOption.get}

  test("scalar and grouped terminals automatically expose the matching bounded family") {
    val scalar=pool("SELECT COUNT(*)"+from)
    assertEquals(scalar.actions.count(_.name.startsWith("scalar-")),4)
    assert(scalar.actions.exists(_.name=="fused-v3"))
    for(keys<-Vector(Vector("s.a"),Vector("s.a","t.b"),Vector("s.a","t.b","r.x"))) {
      val source="SELECT "+keys.mkString(",")+",COUNT(*)"+from+" GROUP BY "+keys.mkString(",")
      val p=pool(source)
      assertEquals(p.actions.count(_.name.startsWith("grouped-")),4)
      assert(p.actions.exists(_.name=="baseline-v2"))
      assert(p.actions.size<=PostgreSqlAggregateCandidatePool.MaximumRewrites)
      assert(p.declines.exists(_.name=="fused-v3"))
    }
  }
  test("specialized decline preserves independently admitted general rewrites") {
    val source="SELECT COUNT(*) FROM p0"+(1 to 6).map(i=>s" JOIN p$i ON p${i-1}.b=p$i.a").mkString
    val p=pool(source)
    assert(p.declines.exists(_.name=="scalar-branch-family"))
    assert(p.uniqueActions.exists(_.name=="baseline-v2"))
    assert(p.uniqueActions.exists(_.name=="fused-v3"))
  }
  test("exact SQL aliases are deterministic and cannot inflate distinct action counts") {
    val source="SELECT COUNT(*)"+from
    val p=pool(source)
    assertEquals(p,pool(source))
    val alias=p.actions.find(_.name=="partial-support").get
    assertEquals(alias.aliasOf,Some("baseline-v2"))
    assertEquals(p.uniqueActions.map(_.sql).distinct.size,p.uniqueActions.size)
    p.actions.filter(_.aliasOf.nonEmpty).foreach(a=>assertEquals(a.sql,p.actions.find(_.name==a.aliasOf.get).get.sql))
  }
  test("grouped candidates keep the dedicated renderer's SQL and identity; default stays unchanged") {
    val source="SELECT s.a,t.b,r.x,COUNT(*)"+from+" GROUP BY s.a,t.b,r.x"
    val a=artifact(source)
    val direct=PostgreSqlGroupedBranchCountRenderer.prepare(a.query,a.candidateInput,a.bagFactorPlan).toOption.get.candidates.toOption.get
    val p=pool(source)
    direct.foreach { d=>
      val action=p.actions.find(_.name=="grouped-"+d.name).get
      assertEquals(action.sql,d.rendered.sql);assertEquals(action.rendererActionId,d.rendered.actionId)
    }
    assertEquals(artifact(source).renderedQuery.sql,a.renderedQuery.sql)
  }
  test("malformed inputs and non-aggregate terminals never emit a candidate") {
    assert(PostgreSqlAggregateCandidatePool.generate(null,null,null).isLeft)
    val projection=artifact("SELECT r.x"+from)
    assert(PostgreSqlAggregateCandidatePool.generate(projection.query,projection.candidateInput,projection.bagFactorPlan).isLeft)
    val a=artifact("SELECT COUNT(*)"+from)
    val bad=a.bagFactorPlan.copy(factors=a.bagFactorPlan.factors.updated(0,a.bagFactorPlan.factors.head.copy(ownedEdgeIds=Vector.empty)))
    val p=PostgreSqlAggregateCandidatePool.generate(a.query,a.candidateInput,bad).toOption.get
    assert(p.actions.isEmpty);assert(p.declines.nonEmpty)
  }
}
