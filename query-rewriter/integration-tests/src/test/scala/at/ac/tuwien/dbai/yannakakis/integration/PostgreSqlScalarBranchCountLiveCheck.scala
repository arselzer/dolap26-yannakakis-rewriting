package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.sql.DriverManager
import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest, YannakakisReductionMode}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter

/** Fresh scratch schemas only; scalar bags/descriptors, both strategies/orders. */
object PostgreSqlScalarBranchCountLiveCheck {
  def main(args: Array[String]): Unit = {
    require(args.length == 4 || (args.length == 5 && args(4) == "terminal-collapse"),
      "JDBC USER PASSWORD_ENV FRESH_SCHEMA_PREFIX [terminal-collapse]")
    require(args(3).matches("yrw_scalar_test_[a-z0-9]+"))
    Class.forName("org.postgresql.Driver")
    val connection = DriverManager.getConnection(args(0), args(1), sys.env(args(2)))
    var comparisons = 0
    var nonzero = 0
    try {
      val stmt = connection.createStatement()
      stmt.execute("SET statement_timeout=10000")
      val tables = Vector("r" -> Vector("x", "y"), "s" -> Vector("x", "y", "a"),
        "t" -> Vector("x", "y", "b"), "z" -> Vector("x", "y", "c"),
        "u" -> Vector("a"), "w" -> Vector("b"), "q" -> Vector("c"))
      val projection = "SELECT COUNT(*) AS \"n\"\"count\" "
      val queries = Vector(
        projection + "FROM r JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b",
        projection + "FROM r JOIN s ON r.x=s.x JOIN t ON r.x=t.x JOIN z ON r.x=z.x JOIN u ON u.a=s.a JOIN w ON w.b=t.b JOIN q ON q.c=z.c",
        projection + "FROM r JOIN s ON r.x=s.x AND r.y=s.y JOIN t ON r.x=t.x AND r.y=t.y JOIN z ON r.x=z.x AND r.y=z.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b JOIN q ON q.c=z.c",
        projection + "FROM r JOIN s AS a ON r.x=a.x JOIN s AS b ON r.y=b.x JOIN u AS c ON c.a=a.a JOIN u AS d ON d.a=b.a")
      for ((sqlType, index) <- Vector("SMALLINT", "INTEGER", "BIGINT").zipWithIndex) {
        val schema = args(3) + "_" + index
        stmt.execute(s"CREATE SCHEMA $schema")
        tables.foreach { case (table, cols) =>
          stmt.execute(s"CREATE TABLE $schema.$table (" + cols.map(_ + " " + sqlType).mkString(",") + ")")
        }
        stmt.execute(s"SET search_path=pg_catalog,$schema")
        val rewriter = PostgreSqlQueryRewriter.forPostgreSqlJdbc(args(0), args(1), sys.env(args(2)), schema)
          .fold(p => sys.error(p.toString), identity)
        val pairs = queries.map { source =>
          val acquired = rewriter.rewriteDetailed(RewriteRequest(source, options = RewriteOptions(
            widthBound = 1, planLimit = 1, reductionMode = YannakakisReductionMode.FullTwoPass)))
          val a = acquired.report.artifact.getOrElse(sys.error(acquired.outcome.toString))
          import PostgreSqlScalarBranchCountRenderer.{Options, Strategy}
          val head = PostgreSqlScalarBranchCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan,
            Options(Strategy.PerKey)).fold(p => sys.error(source + ": " + p.toString), identity)
          val sqls = for (strategy <- Vector(Strategy.PerKey, Strategy.RestrictedGroup);
              order <- Vector(head.branchOrder, head.branchOrder.reverse)) yield {
            val prepared = PostgreSqlScalarBranchCountRenderer.prepare(a.query, a.candidateInput, a.bagFactorPlan)
              .fold(p => sys.error(p.toString), identity)
            val options = Options(strategy, branchOrder = order)
            (if (args.length == 5) prepared.renderTerminalCollapsed(options) else prepared.render(options))
              .fold(p => sys.error(p.toString), _.sql)
          }
          source -> sqls
        }
        def read(sql: String) = {
          val rs = stmt.executeQuery(sql)
          try {
            val md = rs.getMetaData
            val descriptor = (1 to md.getColumnCount).map(i =>
              (md.getColumnLabel(i), md.getColumnType(i), md.getPrecision(i), md.getScale(i), md.isNullable(i))).toVector
            require(rs.next(), "scalar COUNT must return one row")
            val value = rs.getString(1)
            require(!rs.next(), "scalar COUNT returned multiple rows")
            descriptor -> value
          } finally rs.close()
        }
        val random = new scala.util.Random(330909 + index)
        for (trial <- 0 until 20) {
          tables.foreach { case (table, cols) =>
            stmt.execute(s"DELETE FROM $schema.$table")
            val planted = if (trial % 5 == 0) Vector.empty else Vector(Vector.fill(cols.size)("1"))
            var rows = planted ++ Vector.fill(3)(Vector.fill(cols.size)(Vector("NULL", "1", "2", "3")(random.nextInt(4))))
            if (trial % 3 == 0) rows = rows ++ rows.take(1)
            if (trial % 7 == 0 && table == "u") rows = Vector.empty
            rows.foreach(row => stmt.execute(s"INSERT INTO $schema.$table VALUES (" + row.mkString(",") + ")"))
          }
          pairs.foreach { case (source, sqls) =>
            val native = read(source)
            sqls.foreach { sql =>
              require(read(sql) == native, s"mismatch $schema trial $trial")
              comparisons += 1
              if (native._2 != "0") nonzero += 1
            }
          }
        }
      }
      println(s"EXACT comparisons=$comparisons nonzero=$nonzero types=3 shapes=4 strategies=2 orders=2")
      stmt.close()
    } finally connection.close()
  }
}
