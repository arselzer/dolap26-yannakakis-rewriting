package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

/** Tiny create-only schemas; not a performance experiment. Checks values,
  * multiplicities and complete JDBC output descriptors against native SQL.
  */
object PostgreSqlCyclicCoreCountLiveCheck {
  def main(args: Array[String]): Unit = {
    require(args.length == 4, "JDBC USER PASSWORD_ENV FRESH_SCHEMA_PREFIX")
    require(args(3).matches("yrw_core_test_[a-z0-9]+"))
    val ds = new org.postgresql.ds.PGSimpleDataSource
    ds.setURL(args(0)); ds.setUser(args(1)); ds.setPassword(sys.env(args(2)))
    val connection = ds.getConnection
    var comparisons = 0; var nonempty = 0
    try {
      val stmt = connection.createStatement()
      stmt.execute("SET statement_timeout=10000"); stmt.execute("SET jit=off")
      val tables = Vector("r" -> Vector("x","z"), "q" -> Vector("z","y"), "p" -> Vector("y","x"),
        "s" -> Vector("x","a"), "u" -> Vector("a"), "t" -> Vector("y","b"), "w" -> Vector("b"))
      val from = " FROM r JOIN q ON r.z=q.z JOIN p ON p.y=q.y AND p.x=r.x" +
        " JOIN s ON s.x=r.x JOIN u ON u.a=s.a JOIN t ON t.y=p.y JOIN w ON w.b=t.b"
      def sql(keys: Vector[String], body: String = from, filter: String = "") = "SELECT " +
        keys.map(_ + " AS same").mkString(",") + ", COUNT(*) AS \"n\"\"rows\"" + body + filter + " GROUP BY " + keys.mkString(",")
      val keySets = Vector(Vector("s.a"), Vector("u.a","w.b"), Vector("q.z","u.a","w.b"),
        Vector("s.x","u.a","w.b"), Vector("q.z","u.a","w.b","t.y"))
      val sources = keySets.flatMap(keys => Vector(sql(keys), sql(keys.reverse))) ++ Vector(
        sql(Vector("s.a","w.b"), filter = " WHERE r.x >= -1 AND s.tag IN ('keep','yes') AND (t.tag IS NULL OR t.tag LIKE 'y%')"),
        sql(Vector("s.a","w.b"), body = from + " JOIN r again ON again.x=s.a AND again.z=s.a"),
        sql(Vector("s.a","w.b"), body = from + " JOIN s other ON other.x=t.y JOIN u ux ON ux.a=other.a"))
      def read(query: String) = {
        val rs = stmt.executeQuery(query)
        try {
          val md = rs.getMetaData
          val descriptor = (1 to md.getColumnCount).map(i => (md.getColumnLabel(i), md.getColumnType(i),
            md.getPrecision(i), md.getScale(i), md.isNullable(i), md.getTableName(i), md.getColumnName(i))).toVector
          val rows = Vector.newBuilder[Vector[Option[String]]]
          while (rs.next()) rows += (1 to md.getColumnCount).map(i => Option(rs.getString(i))).toVector
          descriptor -> rows.result().groupMapReduce(identity)(_ => 1)(_ + _)
        } finally rs.close()
      }
      for ((sqlType, index) <- Vector("SMALLINT", "INTEGER", "BIGINT").zipWithIndex) {
        val schema = args(3) + "_" + index
        stmt.execute(s"CREATE SCHEMA $schema")
        tables.foreach { case (table, cols) => stmt.execute(s"CREATE TABLE $schema.$table (" +
          cols.map(c => c + " " + sqlType + (if (Set("u","w")(table)) " NOT NULL" else "")).mkString(",") + ",tag VARCHAR(20))") }
        stmt.execute(s"SET search_path=pg_catalog,$schema")
        val planner = CalciteSqlPlanner.forJdbcDataSourceWithLocalPredicates(ds, schema, 4).fold(p => sys.error(p.toString), identity)
        val rewriter = PostgreSqlQueryRewriter.create(planner).toOption.get
        val pairs = sources.distinct.map { source =>
          val result = rewriter.rewriteDetailed(RewriteRequest(source,
            options = RewriteOptions(widthBound = 2, requireConnectedCover = false, planLimit = 1)))
          val a = result.report.artifact.getOrElse(sys.error(source + ": " + result.outcome))
          val generated = PostgreSqlCyclicCoreCountRenderer.render(a.query, a.candidateInput, a.bagFactorPlan)
            .fold(p => sys.error(source + ": " + p.toString), identity)
          val pool = PostgreSqlAggregateCandidatePoolV2.generate(a.query, a.candidateInput, a.bagFactorPlan).toOption.get
          source -> (Vector(generated.sql) ++ pool.uniqueActions.filter(_.name.startsWith("partial-inline-")).map(_.sql))
        }
        val random = new scala.util.Random(9102026 + index)
        for (trial <- 0 until 20) {
          tables.foreach { case (table, cols) =>
            stmt.execute(s"DELETE FROM $schema.$table")
            val nullable = !Set("u","w")(table)
            var rows = Vector(Vector.fill(cols.size)("1") :+ "'yes'") ++ Vector.fill(4)(cols.map { _ =>
              val values = if (nullable) Vector("NULL","1","2","3") else Vector("1","2","3")
              values(random.nextInt(values.size))
            } :+ Vector("NULL","'yes'","'no'","'keep'")(random.nextInt(4)))
            if (trial % 3 == 0) rows ++= rows.take(2)
            if (trial % 7 == 0 && table == "p" || trial % 5 == 0 && table == "u") rows = Vector.empty
            rows.foreach(r => stmt.execute(s"INSERT INTO $schema.$table VALUES (" + r.mkString(",") + ")"))
          }
          pairs.foreach { case (source, generated) =>
            val native = read(source)
            generated.foreach { sql =>
              val actual = read(sql)
              require(native == actual, s"Mismatch $sqlType trial=$trial $source; descriptors ${native._1} / ${actual._1}; bagsEqual=${native._2 == actual._2}\n$sql")
              comparisons += 1; if (native._2.nonEmpty) nonempty += 1
            }
          }
        }
      }
      println(s"EXACT comparisons=$comparisons nonempty=$nonempty types=3 trials=20 descriptors=labels-types-precision-scale-nullability-origin")
      stmt.close()
    } finally connection.close()
  }
}
