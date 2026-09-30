package at.ac.tuwien.dbai.yannakakis.integration

import org.postgresql.ds.PGSimpleDataSource
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql._
import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest, YannakakisReductionMode}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

/** Exact local-data check. No benchmark endpoint, data or timings are used. */
object PostgreSqlLocalPredicateLiveCheck {
  def main(args: Array[String]): Unit = {
    require(args.length == 4, "JDBC USER PASSWORD_ENV SCRATCH_SCHEMA")
    require(args(3).matches("yrw_local_predicate_test_[a-z0-9]+"))
    val ds = new PGSimpleDataSource()
    ds.setURL(args(0)); ds.setUser(args(1)); ds.setPassword(sys.env(args(2)))
    val c = ds.getConnection
    var checks = 0
    var nonempty = 0
    try {
      val s = c.createStatement()
      val schema = args(3)
      s.execute("SET statement_timeout=10000")
      s.execute(s"CREATE SCHEMA $schema")
      s.execute(s"SET search_path=pg_catalog,$schema")
      s.execute(s"CREATE TABLE $schema.edge(a INTEGER,b INTEGER,n SMALLINT,label VARCHAR(80),ts TIMESTAMP,flag BOOLEAN,date DATE)")
      val planner = CalciteSqlPlanner.forJdbcDataSourceWithLocalPredicates(ds, schema, 3)
        .fold(p => sys.error(p.toString), identity)
      val rewriter = PostgreSqlQueryRewriter.create(planner).fold(p => sys.error(p.toString), identity)
      val chain = " FROM edge e0 JOIN edge e1 ON e0.b=e1.a JOIN edge e2 ON e1.b=e2.a JOIN edge e3 ON e2.b=e3.a"
      val filters = Vector("e0.n >= -2", "e0.n BETWEEN -2 AND 4", "e0.label = 'actor'",
        "e0.label IN ('actor','actress')", "e0.label < 'z'", "e0.label IS NOT NULL",
        "e0.label IS NULL", "e0.flag", "e0.label LIKE '%An%' OR e0.label LIKE 'O''Brien%'",
        "e0.ts >= CAST('2010-07-19 00:00:00' AS TIMESTAMP)",
        "e0.Date >= CAST('2010-07-19 00:00:00' AS TIMESTAMP)",
        "e0.n >= -2 AND e1.n < 8 AND (e2.label='actor' OR e2.label='actress')")
      val profile = PostgreSqlAggregateRenderingProfileV2.create(
        PostgreSqlAggregateRenderingProfile.Values.find(p =>
          p.oneBagTerminal == PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2 &&
          p.reductionFinalization == PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2 &&
          p.reductionSupport == PostgreSqlReductionSupportProfile.DirectExistsV2).get,
        PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2,
        PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2).toOption.get
      val pairs = (for {
        cycle <- Vector(false, true)
        predicate <- filters
        lane <- Vector("global", "grouped", "three-key", "enumeration", "min")
      } yield {
        val (select, group) = lane match {
          case "global" => ("COUNT(*)", "")
          case "grouped" => ("e1.a,COUNT(*)", " GROUP BY e1.a")
          case "three-key" => ("e0.b,e1.b,e2.b,COUNT(*)", " GROUP BY e0.b,e1.b,e2.b")
          case "enumeration" => ("e0.a,e1.a,e2.a", "")
          case "min" => ("MIN(e1.a)", "")
        }
        val source = "SELECT " + select + chain + (if(cycle) " AND e3.b=e0.a" else "") +
          " WHERE (" + predicate + ")" + group
        val result = rewriter.rewriteDetailed(RewriteRequest(source, options=RewriteOptions(
          widthBound=if(cycle) 2 else 1, planLimit=1, reductionMode=YannakakisReductionMode.FullTwoPass)))
        val a = result.report.artifact.getOrElse(sys.error(source + "\n" + result.outcome))
        val optimized = if(Set("global", "grouped", "three-key")(lane)) Vector(
          PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfileV2(
          a.query, a.candidateInput, a.bagFactorPlan, PostgreSqlRenderingLimits.default,
          BagFactorPlanningLimits.default, YannakakisReductionMode.FullTwoPass,
          profile, PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2)
          .fold(p => sys.error(p.mkString("; ")), _._2.sql)) else Vector.empty
        val physical = if(Set("global", "grouped", "three-key")(lane)) Vector(
          PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,profile,
            PostgreSqlPhysicalOptimizationCompiler.Options(partialSupport=true))
            .fold(p => sys.error(p.mkString("; ")), _.sql)) else Vector.empty
        val fused = if(lane == "global") {
          val v3 = PostgreSqlAggregateRenderingProfileV3.fusedRawFactorMessages(
            PostgreSqlAggregateRenderingProfileV2.baseline(PostgreSqlAggregateRenderingProfile.LegacyV1).toOption.get).toOption.get
          Vector(PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfileV3(
            a.query,a.candidateInput,a.bagFactorPlan,PostgreSqlRenderingLimits.default,
            BagFactorPlanningLimits.default,YannakakisReductionMode.NoReduction,v3,
            PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2)
            .fold(p => sys.error(p.mkString("; ")), _._2.sql))
        } else Vector.empty
        (Vector(a.renderedQuery.sql) ++ optimized ++ physical ++ fused).map(source -> _)
      }).flatten.distinct
      val insert = c.prepareStatement(s"INSERT INTO $schema.edge VALUES (?,?,?,?,?,?,?)")
      val random = new scala.util.Random(909260)
      for (trial <- 0 until 12) {
        s.execute(s"DELETE FROM $schema.edge")
        if (trial % 6 != 0) for (row <- 0 until 12) {
          val planted = row < 4
          val values: Vector[AnyRef] = Vector(
            if(planted) Int.box(1) else Vector(null,Int.box(-1),Int.box(0),Int.box(1))(random.nextInt(4)),
            if(planted) Int.box(1) else Vector(null,Int.box(-1),Int.box(0),Int.box(1))(random.nextInt(4)),
            if(planted) Int.box(1) else Vector(null,Int.box(-4),Int.box(-2),Int.box(8))(random.nextInt(4)),
            if(planted) "actor" else Vector(null,"actress","O'Brien; --","Anna")(random.nextInt(4)),
            if(planted) java.sql.Timestamp.valueOf("2011-01-01 00:00:00") else
              Vector(null,java.sql.Timestamp.valueOf("2009-01-01 00:00:00"),
                java.sql.Timestamp.valueOf("2010-07-19 00:00:00"))(random.nextInt(3)),
            if(planted) Boolean.box(true) else Vector(null,Boolean.box(false),Boolean.box(true))(random.nextInt(3)),
            if(planted) java.sql.Date.valueOf("2011-01-01") else Vector(null,
              java.sql.Date.valueOf("2009-01-01"),java.sql.Date.valueOf("2010-07-19"))(random.nextInt(3)))
          values.zipWithIndex.foreach { case(value,index) => insert.setObject(index+1,value) }
          insert.executeUpdate()
        }
        pairs.foreach { case(source,generated) =>
          val expected = ExactResultMultiset.execute(c,source)
          val actual = ExactResultMultiset.execute(c,generated)
          val difference = expected.difference(actual)
          require(difference.isEmpty,s"trial=$trial ${difference.render}\n$source\n$generated")
          checks += 1
          if(!expected.isEmpty) nonempty += 1
        }
      }
      insert.close(); s.close()
      println(s"EXACT checks=$checks nonempty=$nonempty sourceQueries=120 trials=12")
    } finally c.close()
  }
}
