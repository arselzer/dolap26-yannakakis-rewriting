package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions,RewriteRequest}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

/** Integration of unchanged producers; full value bags and positional labels/types. */
object PostgreSqlAggregateCandidatePoolLiveCheck {
  def main(args:Array[String]):Unit={
    require(args.length==4,"JDBC USER PASSWORD_ENV FRESH_SCHEMA")
    require(args(3).matches("yrw_pool_test_[a-z0-9]+"))
    val ds=new org.postgresql.ds.PGSimpleDataSource
    ds.setURL(args(0));ds.setUser(args(1));ds.setPassword(sys.env(args(2)))
    val c=ds.getConnection;var comparisons=0
    try {
      val s=c.createStatement();s.execute("SET statement_timeout=10000");s.execute("SET jit=off")
      val schema=args(3);s.execute(s"CREATE SCHEMA $schema");s.execute(s"SET search_path=pg_catalog,$schema")
      val tables=Vector("r"->Vector("x","y"),"s"->Vector("x","a"),"t"->Vector("y","b"),
        "u"->Vector("a"),"w"->Vector("b")) ++ (0 to 6).map(i=>s"p$i"->Vector("a","b"))
      tables.foreach { case(name,columns)=>
        s.execute(s"CREATE TABLE $schema.$name ("+columns.map(_+" INTEGER").mkString(",")+")")
        for(value<-Vector("1","1","2","NULL"))s.execute(s"INSERT INTO $schema.$name VALUES ("+columns.map(_=>value).mkString(",")+")")
      }
      val planner=CalciteSqlPlanner.forJdbcDataSourceWithDistributedCountGrouping(ds,schema,3).toOption.get
      val rewriter=PostgreSqlQueryRewriter.create(planner).toOption.get
      val from=" FROM r JOIN s ON r.x=s.x JOIN t ON r.y=t.y JOIN u ON u.a=s.a JOIN w ON w.b=t.b"
      val chain=" FROM p0"+(1 to 6).map(i=>s" JOIN p$i ON p${i-1}.b=p$i.a").mkString
      val cycle=" FROM p0 JOIN p1 ON p0.b=p1.a JOIN p2 ON p1.b=p2.a JOIN p3 ON p2.b=p3.a AND p3.b=p0.a"
      val sources=Vector(("SELECT COUNT(*) AS n"+from,1),
        ("SELECT s.a AS g,COUNT(*) AS n"+from+" GROUP BY s.a",1),
        ("SELECT s.a AS g,t.b AS g,COUNT(*) AS n"+from+" GROUP BY s.a,t.b",1),
        ("SELECT s.a AS g,t.b AS g,r.x AS g,COUNT(*) AS n"+from+" GROUP BY s.a,t.b,r.x",1),
        ("SELECT COUNT(*) AS n"+chain,1),("SELECT COUNT(*) AS n"+cycle,3),
        ("SELECT p0.a AS g,p2.a AS g,COUNT(*) AS n"+cycle+" GROUP BY p0.a,p2.a",3))
      def read(sql:String)={
        val rs=s.executeQuery(sql)
        try {
          val m=rs.getMetaData;val descriptor=(1 to m.getColumnCount).map(i=>(m.getColumnLabel(i),m.getColumnType(i))).toVector
          val rows=Vector.newBuilder[Vector[Option[String]]]
          while(rs.next())rows+=(1 to m.getColumnCount).map(i=>Option(rs.getString(i))).toVector
          descriptor -> rows.result().groupMapReduce(identity)(_=>1)(_+_)
        } finally rs.close()
      }
      val programs=sources.map { case(source,width)=>
        val a=rewriter.rewriteDetailed(RewriteRequest(source,options=RewriteOptions(widthBound=width,planLimit=1,
          requireConnectedCover=width==1))).report.artifact.getOrElse(sys.error(source))
        source -> PostgreSqlAggregateCandidatePool.generate(a.query,a.candidateInput,a.bagFactorPlan).toOption.get.uniqueActions
      }
      for(empty<-Vector(false,true)) {
        if(empty)tables.foreach { case(name,_)=>s.execute(s"DELETE FROM $schema.$name") }
        programs.foreach { case(source,actions)=>
          require(actions.nonEmpty)
          val native=read(source)
          actions.foreach { a=>require(native==read(a.sql),s"Mismatch ${a.name}: $source");comparisons+=1 }
        }
      }
      println(s"EXACT comparisons=$comparisons sources=${sources.size} cyclic-and-acyclic scalar-and-grouped nulls-duplicates-empty labels-types-bags")
      s.close()
    } finally c.close()
  }
}
