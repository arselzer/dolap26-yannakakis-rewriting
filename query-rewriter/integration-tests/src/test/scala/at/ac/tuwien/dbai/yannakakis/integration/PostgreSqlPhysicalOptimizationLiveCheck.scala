package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.sql.DriverManager
import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest, YannakakisReductionMode}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{PostgreSqlCteMaterializationDirective, PostgreSqlQueryRewriter}

/** Creates isolated scratch schemas; never updates benchmark relations. Retains fixtures on failure. */
object PostgreSqlPhysicalOptimizationLiveCheck {
  def main(args: Array[String]): Unit = {
    require(args.length == 4, "JDBC USER PASSWORD_ENV SCRATCH_PREFIX")
    require(args(3).matches("yrw_physical_test_[a-z0-9]+"))
    Class.forName("org.postgresql.Driver")
    val connection = DriverManager.getConnection(args(0), args(1), sys.env(args(2)))
    val profile = PostgreSqlAggregateRenderingProfileV2.create(
      PostgreSqlAggregateRenderingProfile.Values.find(p =>
        p.oneBagTerminal == PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2 &&
        p.reductionFinalization == PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2 &&
        p.reductionSupport == PostgreSqlReductionSupportProfile.DirectExistsV2).get,
      PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2,
      PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2).toOption.get
    val tables = Vector("r" -> Vector("x","y"), "s" -> Vector("x","a"), "t" -> Vector("y","b"),
      "u" -> Vector("a"), "w" -> Vector("b"), "l" -> Vector("x","c"), "ld" -> Vector("c"),
      "m" -> Vector("y","d"), "md" -> Vector("d"), "e" -> Vector("s","d"),
      "f" -> Vector("s","d"), "g" -> Vector("s","d"))
    val star = "SELECT s.a AS g1,t.b AS g2,COUNT(*) AS n FROM r JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b JOIN l ON l.x=r.x JOIN ld ON ld.c=l.c JOIN m ON m.y=r.y JOIN md ON md.d=m.d GROUP BY s.a,t.b"
    val cyclic = "SELECT COUNT(*) AS n FROM e a,e b,e c,e d,f,g h,g i WHERE a.s=b.s AND a.d=c.s AND b.d=d.s AND c.d=d.d AND f.s=c.d AND h.s=f.s AND i.s=f.d AND i.d=h.d"
    var checks = 0; var nonzero = 0
    try {
      val stmt = connection.createStatement()
      stmt.execute("SET statement_timeout=10000")
      for ((sqlType, typeIndex) <- Vector("SMALLINT", "INTEGER", "BIGINT").zipWithIndex) {
        val schema = args(3) + "_" + typeIndex
        stmt.execute(s"CREATE SCHEMA $schema")
        tables.foreach { case (table, cols) =>
          stmt.execute(s"CREATE TABLE $schema.$table (" + cols.map(_ + " " + sqlType).mkString(",") + ")")
        }
        stmt.execute(s"SET search_path=pg_catalog,$schema")
        val rewriter = PostgreSqlQueryRewriter.forPostgreSqlJdbc(args(0), args(1), sys.env(args(2)), schema)
          .fold(p => throw new IllegalArgumentException(p.toString), identity)
        val pairs = Vector(star -> 1, cyclic -> 3).flatMap { case (source, width) =>
          val acquisition = rewriter.rewriteDetailed(RewriteRequest(source, options = RewriteOptions(
            widthBound=width, planLimit=10, reductionMode=YannakakisReductionMode.FullTwoPass)))
          val a = acquisition.report.artifact.getOrElse(sys.error(acquisition.outcome.toString))
          val variants = if (width == 1) {
            val baseline = PostgreSqlAcyclicBranchCountRenderer.render(a.query,a.candidateInput,a.bagFactorPlan).toOption.get.sql
            val progressive = a.bagFactorPlan.factors.flatMap(f =>
              PostgreSqlAcyclicBranchCountRenderer.renderWithFirstBranch(a.query,a.candidateInput,a.bagFactorPlan,Some(f.bagId)).toOption.map(_.sql))
            require(progressive.size >= 2)
            baseline +: progressive
          } else {
            val partial = PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,
              profile,PostgreSqlPhysicalOptimizationCompiler.Options(partialSupport=true)).toOption.get
            require(partial.supportChanges.nonEmpty)
            val directives = a.bagFactorPlan.factors.flatMap(f =>
              Vector(PostgreSqlCteMaterializationDirective.Materialized,PostgreSqlCteMaterializationDirective.NotMaterialized)
                .map(d => PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,
                  profile,PostgreSqlPhysicalOptimizationCompiler.Options(partialSupport=true,
                    initialFactorDirectives=Map(f.bagId -> d))).toOption.get.sql))
            partial.sql +: directives
          }
          variants.distinct.map(source -> _)
        }
        def read(sql: String) = {
          val rs = stmt.executeQuery(sql); val md = rs.getMetaData
          val descriptor = (1 to md.getColumnCount).map(i =>
            (md.getColumnLabel(i),md.getColumnType(i),md.getPrecision(i),md.getScale(i),md.isNullable(i))).toVector
          val rows = Vector.newBuilder[Vector[Option[String]]]
          while (rs.next()) rows += (1 to md.getColumnCount).map(i => Option(rs.getString(i))).toVector
          rs.close(); (descriptor,rows.result().groupMapReduce(identity)(_ => 1)(_ + _))
        }
        val random = new scala.util.Random(908260 + typeIndex)
        for (trial <- 0 until 30) {
          tables.foreach { case (table, cols) =>
            stmt.execute(s"DELETE FROM $schema.$table")
            val planted = if (trial % 6 == 0) Vector.empty else Vector(Vector.fill(cols.size)("1"))
            var rows = planted ++ Vector.fill(3)(Vector.fill(cols.size)(Vector("NULL","1","2","3")(random.nextInt(4))))
            if (trial % 3 == 0) rows = rows ++ rows.take(1)
            if (trial % 7 == 0 && Set("u","f")(table)) rows = Vector.empty
            rows.foreach(row => stmt.execute(s"INSERT INTO $schema.$table VALUES (" + row.mkString(",") + ")"))
          }
          val expected = pairs.map(_._1).distinct.map(s => s -> read(s)).toMap
          pairs.foreach { case (source, generated) =>
            val native = expected(source)
            require(native == read(generated), s"mismatch in $schema trial $trial: $generated")
            checks += 1
            if (native._2.keys.exists(_.last.exists(_ != "0"))) nonzero += 1
          }
        }
      }
      println(s"EXACT checks=$checks nonzero=$nonzero descriptor=labels-types-precision-scale-nullability")
      stmt.close()
    } finally connection.close()
  }
}
