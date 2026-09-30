package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest, YannakakisReductionMode}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{PostgreSqlCteMaterializationDirective, PostgreSqlQueryRewriter}
import at.ac.tuwien.dbai.yannakakis.core.BagId
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

final class PostgreSqlPhysicalOptimizationSuite extends munit.FunSuite {
  private val profile = PostgreSqlAggregateRenderingProfileV2.create(
    PostgreSqlAggregateRenderingProfile.Values.find(p =>
      p.oneBagTerminal == PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2 &&
      p.reductionFinalization == PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2 &&
      p.reductionSupport == PostgreSqlReductionSupportProfile.DirectExistsV2).get,
    PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2,
    PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2).toOption.get
  private val tables = Vector("r" -> Vector("x","y"),"s" -> Vector("x","a"),"t" -> Vector("y","b"),
    "u" -> Vector("a"),"w" -> Vector("b"),"l" -> Vector("x","c"),"ld" -> Vector("c"),
    "m" -> Vector("y","d"),"md" -> Vector("d"),"e" -> Vector("s","d"),"f" -> Vector("s","d"),"g" -> Vector("s","d"))
  private val schema=Frameworks.createRootSchema(true)
  tables.foreach { case (name,cols) => schema.add(name,new AbstractTable {
    override def getRowType(factory:RelDataTypeFactory):RelDataType = {
      val b=factory.builder();cols.foreach(c=>b.add(c,SqlTypeName.INTEGER).nullable(true));b.build()
    }
  }) }
  private val rewriter=PostgreSqlQueryRewriter.create(CalciteSqlPlanner.forSchema(schema,
    CalciteSqlPlanner.PostgreSqlParserConfig).toOption.get).toOption.get
  private val star="SELECT s.a AS g1,t.b AS g2,COUNT(*) AS n FROM r JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b JOIN l ON l.x=r.x JOIN ld ON ld.c=l.c JOIN m ON m.y=r.y JOIN md ON md.d=m.d GROUP BY s.a,t.b"
  private val cyclic="SELECT COUNT(*) AS n FROM e a,e b,e c,e d,f,g h,g i WHERE a.s=b.s AND a.d=c.s AND b.d=d.s AND c.d=d.d AND f.s=c.d AND h.s=f.s AND i.s=f.d AND i.d=h.d"
  private def artifact(sql:String=cyclic,width:Int=3)=rewriter.rewriteDetailed(RewriteRequest(sql,
    options=RewriteOptions(widthBound=width,planLimit=10,requireConnectedCover=true,
      reductionMode=YannakakisReductionMode.FullTwoPass))).report.artifact.getOrElse(fail("missing artifact: "+sql))

  test("empty options preserve exact existing optimized-V2 bytes") {
    val a=artifact()
    val old=PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfileV2(a.query,a.candidateInput,a.bagFactorPlan,
      PostgreSqlRenderingLimits.default,at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits.default,
      YannakakisReductionMode.FullTwoPass,profile,PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2).toOption.get._2
    val result=PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,profile,
      PostgreSqlPhysicalOptimizationCompiler.Options()).toOption.get
    assertEquals(result.sql,old.sql);assertEquals(result.supportChanges,Vector.empty)
  }

  test("partial support preserves owned scans and replaces lost support bindings") {
    val a=artifact()
    val result=PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,profile,
      PostgreSqlPhysicalOptimizationCompiler.Options(partialSupport=true)).toOption.get
    assert(result.supportChanges.nonEmpty)
    result.supportChanges.foreach { change =>
      assert(change.removedEdges.nonEmpty);assert(change.retainedEdges.nonEmpty)
      change.removedEdges.foreach { edge =>
        assert(!result.sql.contains(s"AS \"yr_s_b${change.bagId.value}_e${edge.value}\""))
        assert(result.sql.contains(s"AS \"yr_f_b${change.bagId.value}_e${edge.value}\""))
      }
    }
    assertEquals(result,PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,profile,
      PostgreSqlPhysicalOptimizationCompiler.Options(partialSupport=true)).toOption.get)
  }

  test("stage directive overrides only one initial CTE") {
    val a=artifact();val id=a.bagFactorPlan.factors.head.bagId
    val empty=PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,profile,
      PostgreSqlPhysicalOptimizationCompiler.Options()).toOption.get
    val changed=PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,profile,
      PostgreSqlPhysicalOptimizationCompiler.Options(initialFactorDirectives=Map(id->PostgreSqlCteMaterializationDirective.NotMaterialized))).toOption.get
    assertEquals(changed.sql,empty.sql.replace(s"\"yr_f_b${id.value}_i\" AS (",s"\"yr_f_b${id.value}_i\" AS NOT MATERIALIZED ("))
    assert(changed.actionId!=empty.actionId)
  }

  test("invalid override and tampered ownership fail closed") {
    val a=artifact()
    assert(PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,profile,
      PostgreSqlPhysicalOptimizationCompiler.Options(initialFactorDirectives=Map(BagId.from(999).toOption.get->PostgreSqlCteMaterializationDirective.NotMaterialized))).isLeft)
    val first=a.bagFactorPlan.factors.head
    val bad=a.bagFactorPlan.copy(factors=a.bagFactorPlan.factors.updated(0,first.copy(ownedEdgeIds=Vector.empty)))
    assert(PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,bad,profile,
      PostgreSqlPhysicalOptimizationCompiler.Options(partialSupport=true)).isLeft)
    assert(PostgreSqlPhysicalOptimizationCompiler.render(null,null,null,profile,null).isLeft)
  }

  test("explicit progressive branch moves one multiplier into the seed") {
    val a=artifact(star,1)
    val baseline=PostgreSqlAcyclicBranchCountRenderer.render(a.query,a.candidateInput,a.bagFactorPlan).toOption.get
    val choices=a.bagFactorPlan.factors.map(_.bagId).flatMap { id =>
      PostgreSqlAcyclicBranchCountRenderer.renderWithFirstBranch(a.query,a.candidateInput,a.bagFactorPlan,Some(id)).toOption.map(id->_)
    }
    assert(choices.size>=2)
    choices.foreach { case (id,result) =>
      assert(result.sql.contains("\"yr_bc_seed\" AS MATERIALIZED"))
      assert(result.actionId!=baseline.actionId)
      val core=result.sql.substring(result.sql.indexOf("\"yr_bc_core\" AS"))
      assert(!core.contains(s"b${id.value}.yr_freq"))
    }
    assertEquals(PostgreSqlAcyclicBranchCountRenderer.renderWithFirstBranch(a.query,a.candidateInput,a.bagFactorPlan,None).toOption.get,baseline)
    assert(PostgreSqlAcyclicBranchCountRenderer.renderWithFirstBranch(a.query,a.candidateInput,a.bagFactorPlan,Some(baseline.anchor)).isLeft)
  }
}
