package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.sql.DriverManager
import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions, RewriteRequest}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

/** Create-only tiny schemas. Covers group placement, NULLs, duplicates and self joins. */
object PostgreSqlGroupedBranchCountLiveCheck {
  def main(args: Array[String]): Unit = {
    require(args.length==4 || (args.length==5 && args(4)=="mixed-nullability"),
      "JDBC USER PASSWORD_ENV FRESH_SCHEMA_PREFIX [mixed-nullability]")
    require(args(3).matches("yrw_grouped_test_[a-z0-9]+"))
    val ds=new org.postgresql.ds.PGSimpleDataSource
    ds.setURL(args(0)); ds.setUser(args(1)); ds.setPassword(sys.env(args(2)))
    val connection=ds.getConnection
    var comparisons=0; var nonempty=0
    try {
      val stmt=connection.createStatement()
      stmt.execute("SET statement_timeout=10000")
      stmt.execute("SET jit=off")
      val tables=Vector("r"->Vector("x","y","c"),"p"->Vector("c"),"s"->Vector("x","a","d"),
        "t"->Vector("y","b"),"u"->Vector("a"),"w"->Vector("b"),"z"->Vector("d"))
      val from=" FROM r JOIN p ON r.c=p.c JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b"
      def sql(keys:Vector[String],body:String=from)=
        "SELECT "+keys.map(_+" AS same").mkString(",")+", COUNT(*) AS \"n\"\"rows\""+body+" GROUP BY "+keys.mkString(",")
      val keys=Vector(Vector("s.a","t.b"),Vector("r.x","s.a","t.b"),Vector("p.c","s.a","t.b"),
        Vector("p.c","s.a","t.b","r.y"),Vector("r.x"),Vector("s.a"),Vector("p.c"),
        Vector("p.c","r.y"),Vector("t.b","r.x","s.a"))
      val sources=keys.map(sql(_)) ++ Vector(sql(Vector("s.x","t.b")),sql(Vector("u.a","z.d"),from+" JOIN z ON z.d=s.d"),
        sql(Vector("s2.a","r.x","s1.a")," FROM r JOIN p ON r.c=p.c JOIN s s1 ON r.x=s1.x JOIN s s2 ON r.y=s2.x JOIN u u1 ON u1.a=s1.a JOIN u u2 ON u2.a=s2.a"))
      def read(sql:String) = {
        val rs=stmt.executeQuery(sql)
        try {
          val md=rs.getMetaData
          val descriptor=(1 to md.getColumnCount).map(i=>(md.getColumnLabel(i),md.getColumnType(i),md.getPrecision(i),md.getScale(i),md.isNullable(i),md.getTableName(i),md.getColumnName(i))).toVector
          val rows=Vector.newBuilder[Vector[Option[String]]]
          while(rs.next()) rows += (1 to md.getColumnCount).map(i=>Option(rs.getString(i))).toVector
          descriptor -> rows.result().groupMapReduce(identity)(_=>1)(_+_)
        } finally rs.close()
      }
      val types=if(args.length==5)Vector("INTEGER") else Vector("SMALLINT","INTEGER","BIGINT")
      for((sqlType,index)<-types.zipWithIndex) {
        val schema=args(3)+"_"+index
        stmt.execute(s"CREATE SCHEMA $schema")
        def notNull(table:String,col:String)=args.length==5 && Set("p.c","u.a","w.b","z.d")(table+"."+col)
        tables.foreach { case (table,cols)=>stmt.execute(s"CREATE TABLE $schema.$table ("+
          cols.map(col=>col+" "+sqlType+(if(notNull(table,col))" NOT NULL" else "")).mkString(",")+")") }
        stmt.execute(s"SET search_path=pg_catalog,$schema")
        val planner=CalciteSqlPlanner.forJdbcDataSourceWithDistributedCountGrouping(ds,schema,4).fold(p=>sys.error(p.toString),identity)
        val rewriter=PostgreSqlQueryRewriter.create(planner).toOption.get
        val pairs=sources.map { source =>
          val acquired=rewriter.rewriteDetailed(RewriteRequest(source,options=RewriteOptions(widthBound=1,planLimit=1)))
          val a=acquired.report.artifact.getOrElse(sys.error(source+": "+acquired.outcome.toString))
          val p=PostgreSqlGroupedBranchCountRenderer.prepare(a.query,a.candidateInput,a.bagFactorPlan).fold(p=>sys.error(p.toString),identity)
          val sqls=p.candidates.fold(p=>sys.error(p.toString),identity).map(_.rendered.sql)
          source -> sqls
        }
        val random=new scala.util.Random(9102026+index)
        for(trial<-0 until (if(args.length==5)3 else 20)) {
          tables.foreach { case (table,cols)=>
            stmt.execute(s"DELETE FROM $schema.$table")
            val planted=if(trial%5==0)Vector.empty else Vector(Vector.fill(cols.size)("1"))
            var rows=planted ++ Vector.fill(4)(cols.map { col =>
              val values=if(notNull(table,col))Vector("1","2","3") else Vector("NULL","1","2","3")
              values(random.nextInt(values.size))
            })
            if(trial%3==0)rows=rows++rows.take(2)
            if(trial%7==0 && table=="u")rows=Vector.empty
            rows.foreach(r=>stmt.execute(s"INSERT INTO $schema.$table VALUES ("+r.mkString(",")+")"))
          }
          pairs.foreach { case (source,sqls)=>
            val native=read(source)
            sqls.foreach { generated=>
              val rewritten=read(generated)
              require(native==rewritten,s"Mismatch $schema trial $trial: $source; descriptors ${native._1} / ${rewritten._1}; bagsEqual=${native._2==rewritten._2}")
              comparisons+=1; if(native._2.nonEmpty)nonempty+=1
            }
          }
        }
      }
      println(s"EXACT comparisons=$comparisons nonempty=$nonempty types=${types.size} shapes=${sources.size} actions=4 labels-types-precision-scale-nullability-origin")
      stmt.close()
    } finally connection.close()
  }
}
