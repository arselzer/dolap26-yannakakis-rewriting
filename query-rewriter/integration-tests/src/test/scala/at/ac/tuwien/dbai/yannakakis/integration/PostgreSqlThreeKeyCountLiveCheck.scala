package at.ac.tuwien.dbai.yannakakis.integration

import org.postgresql.ds.PGSimpleDataSource
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql._
import at.ac.tuwien.dbai.yannakakis.api.{CandidatePlanSelection, RewriteOptions, RewriteRequest, YannakakisReductionMode}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

/** Seeded differential check in create-only scratch schemas, never benchmark data.
  * Keeps fixtures on failure for reproduction. Credentials are read from an
  * environment variable, not printed or written into an artifact.
  */
object PostgreSqlThreeKeyCountLiveCheck {
  def main(args: Array[String]): Unit = {
    require(args.length == 4, "JDBC USER PASSWORD_ENV SCRATCH_PREFIX")
    require(args(3).matches("yrw_three_key_test_[a-z0-9]+"))
    val ds = new PGSimpleDataSource()
    ds.setURL(args(0)); ds.setUser(args(1)); ds.setPassword(sys.env(args(2)))
    val connection = ds.getConnection
    var checks = 0
    var nonempty = 0
    try {
      val statement = connection.createStatement()
      statement.execute("SET statement_timeout=10000")
      for ((sqlType, typeIndex) <- Vector("SMALLINT", "INTEGER", "BIGINT").zipWithIndex) {
        val schema = args(3) + "_" + typeIndex
        statement.execute(s"CREATE SCHEMA $schema")
        statement.execute(s"SET search_path=pg_catalog,$schema")
        statement.execute(s"CREATE TABLE $schema.edge (a $sqlType, b $sqlType, c $sqlType)")
        val planner = CalciteSqlPlanner.forJdbcDataSourceWithDistributedCountGrouping(ds, schema, maximumGroupKeys = 4)
          .fold(p => sys.error(p.toString), identity)
        val rewriter = PostgreSqlQueryRewriter.create(planner).fold(p => sys.error(p.toString), identity)
        val chain = "FROM edge e0 JOIN edge e1 ON e0.b=e1.a JOIN edge e2 ON e1.b=e2.a JOIN edge e3 ON e2.b=e3.a"
        val keys = "e2.b AS same_name,e0.b AS same_name,e1.b AS same_name,COUNT(*)"
        val grouping = " GROUP BY e2.b,e0.b,e1.b"
        val sourceQueries = Vector(
          (s"SELECT $keys $chain$grouping", 1),
          (s"SELECT $keys $chain AND e3.b=e0.a$grouping", 2),
          ("SELECT e3.a,e1.a,e0.a,e2.a,COUNT(*) " + chain + " AND e3.b=e0.a" +
            " GROUP BY e3.a,e1.a,e0.a,e2.a", 2),
          ("SELECT e0.a,e1.b,e2.c,COUNT(*) FROM edge e0 JOIN edge e1 ON e0.a=e1.a AND e0.b=e1.b AND e0.c=e1.c " +
            "JOIN edge e2 ON e1.a=e2.a AND e1.b=e2.b AND e1.c=e2.c GROUP BY e0.a,e1.b,e2.c", 1)
        )
        val pairs = sourceQueries.flatMap { case (source, width) =>
          Vector(1, 2).flatMap { rank =>
            val result = rewriter.rewriteDetailed(RewriteRequest(source, options = RewriteOptions(
              widthBound=width, planLimit=2, reductionMode=YannakakisReductionMode.FullTwoPass,
              candidateSelection=CandidatePlanSelection.StructuralRank(rank))))
            if (rank == 1) require(result.report.artifact.nonEmpty, result.outcome.toString)
            result.report.artifact.toVector.flatMap { a =>
              val profile = PostgreSqlAggregateRenderingProfileV2.create(
                PostgreSqlAggregateRenderingProfile.Values.find(p =>
                  p.oneBagTerminal == PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2 &&
                  p.reductionFinalization == PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2 &&
                  p.reductionSupport == PostgreSqlReductionSupportProfile.DirectExistsV2).get,
                PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2,
                PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2).toOption.get
              val optimized = PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfileV2(
                a.query, a.candidateInput, a.bagFactorPlan, PostgreSqlRenderingLimits.default,
                BagFactorPlanningLimits.default, YannakakisReductionMode.FullTwoPass,
                profile, PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2)
                .fold(p => sys.error(p.mkString("; ")), _._2.sql)
              Vector(source -> a.renderedQuery.sql, source -> optimized)
            }
          }
        }.distinct
        val random = new scala.util.Random(909260 + typeIndex)
        for (trial <- 0 until 30) {
          statement.execute(s"DELETE FROM $schema.edge")
          val planted = if (trial % 6 == 0) Vector.empty else Vector.fill(2)(Vector("1", "1", "1"))
          val randomRows = if (trial % 6 == 0) Vector.empty else
            Vector.fill(6)(Vector.fill(3)(Vector("NULL", "-1", "0", "1", "2")(random.nextInt(5))))
          (planted ++ randomRows).foreach(row =>
            statement.execute(s"INSERT INTO $schema.edge VALUES (" + row.mkString(",") + ")"))
          pairs.foreach { case (source, generated) =>
            val expected = ExactResultMultiset.execute(connection, source)
            val actual = ExactResultMultiset.execute(connection, generated)
            val difference = expected.difference(actual)
            require(difference.isEmpty, s"$schema trial $trial: ${difference.render}\n$generated")
            checks += 1
            if (!expected.isEmpty) nonempty += 1
          }
        }
      }
      statement.close()
      println(s"EXACT checks=$checks nonempty=$nonempty types=SMALLINT,INTEGER,BIGINT trials=30")
    } finally connection.close()
  }
}
