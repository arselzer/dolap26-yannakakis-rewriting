package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.sql.DriverManager
import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest, YannakakisReductionMode}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter

/** Small isolated scratch schemas only; no benchmark data is modified. */
object PostgreSqlAcyclicBranchCountLiveCheck {
  def main(args: Array[String]): Unit = {
    require(args.length == 4, "JDBC USER PASSWORD_ENV SCRATCH_PREFIX")
    require(args(3).matches("yrw_branch_test_[a-z0-9]+"))
    Class.forName("org.postgresql.Driver")
    val connection = DriverManager.getConnection(args(0), args(1), sys.env(args(2)))
    var checks = 0; var nonempty = 0
    try {
      val stmt = connection.createStatement()
      val tables = Vector("r" -> Vector("x", "y"), "s" -> Vector("x", "a"),
        "t" -> Vector("y", "b"), "u" -> Vector("a"), "w" -> Vector("b"))
      val base = "SELECT s.a AS g1, t.b AS g2, COUNT(*) AS n FROM r JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b GROUP BY s.a,t.b"
      val self = "SELECT s1.a AS g1, s2.a AS g2, COUNT(*) AS n FROM r JOIN s AS s1 ON r.x=s1.x JOIN s AS s2 ON r.y=s2.x JOIN u AS u1 ON u1.a=s1.a JOIN u AS u2 ON u2.a=s2.a GROUP BY s1.a,s2.a"
      for ((sqlType, typeIndex) <- Vector("SMALLINT", "INTEGER", "BIGINT").zipWithIndex) {
        val schema = args(3) + "_" + typeIndex
        stmt.execute(s"CREATE SCHEMA $schema")
        tables.foreach { case (table, cols) => stmt.execute(s"CREATE TABLE $schema.$table (" + cols.map(_ + " " + sqlType).mkString(",") + ")") }
        stmt.execute(s"SET search_path=pg_catalog,$schema")
        stmt.execute("SET statement_timeout=10000")
        val rewriter = PostgreSqlQueryRewriter.forPostgreSqlJdbc(args(0), args(1), sys.env(args(2)), schema)
          .fold(p => throw new IllegalArgumentException(p.toString), identity)
        val swapped = "SELECT t.b AS g1, s.a AS g2, COUNT(*) AS n FROM r JOIN t ON r.y=t.y JOIN s ON r.x=s.x JOIN w ON w.b=t.b JOIN u ON u.a=s.a GROUP BY t.b,s.a"
        val pairs = Vector(base, swapped,
          base.replace("AS g1", "AS same").replace("AS g2", "AS same"), self).map { source =>
          val acquisition = rewriter.rewriteDetailed(RewriteRequest(source, options = RewriteOptions(widthBound = 1,
            planLimit = 1, reductionMode = YannakakisReductionMode.FullTwoPass)))
          val a = acquisition.report.artifact.getOrElse(throw new IllegalArgumentException(
            s"acquisition failed for $schema: $source: ${acquisition.outcome}"))
          val rendered = PostgreSqlAcyclicBranchCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan)
            .fold(p => throw new IllegalArgumentException(p.map(_.message).mkString("; ")), identity)
          source -> rendered.sql
        }
        def read(sql: String) = {
          val rs = stmt.executeQuery(sql)
          val md = rs.getMetaData
          val descriptor = (1 to md.getColumnCount).map(i =>
            (md.getColumnLabel(i), md.getColumnType(i), md.getPrecision(i), md.getScale(i), md.isNullable(i))).toVector
          val rows = Vector.newBuilder[Vector[Option[String]]]
          while (rs.next()) rows += (1 to md.getColumnCount).map(i => Option(rs.getString(i))).toVector
          rs.close()
          (descriptor, rows.result().groupMapReduce(identity)(_ => 1)(_ + _))
        }
        val random = new scala.util.Random(330910 + typeIndex)
        for (trial <- 0 until 24) {
          tables.foreach { case (table, cols) =>
            stmt.execute(s"DELETE FROM $schema.$table")
            val planted = if (trial % 6 == 0) Vector.empty else Vector(Vector.fill(cols.size)("1"))
            var rows = planted ++ Vector.fill(3)(Vector.fill(cols.size)(Vector("NULL", "1", "2", "3")(random.nextInt(4))))
            if (trial % 3 == 0) rows = rows ++ rows.take(1)
            if (trial % 7 == 0 && table == "u") rows = Vector.empty
            rows.foreach(row => stmt.execute(s"INSERT INTO $schema.$table VALUES (" + row.mkString(",") + ")"))
          }
          pairs.foreach { case (source, generated) =>
            val native = read(source); val rewritten = read(generated)
            require(native == rewritten, s"mismatch in $schema trial $trial")
            checks += 1; if (native._2.nonEmpty) nonempty += 1
          }
        }
      }
      println(s"EXACT checks=$checks nonempty=$nonempty descriptor=labels-types-precision-scale-nullability")
      stmt.close()
    } finally connection.close()
  }
}
