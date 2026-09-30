package at.ac.tuwien.dbai.yannakakis.integration

import java.nio.file.{Files,Paths}
import org.postgresql.ds.PGSimpleDataSource
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql._
import at.ac.tuwien.dbai.yannakakis.api.{RewriteOptions,RewriteRequest,YannakakisReductionMode}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

/** Bounded local correctness gate, not a performance experiment. */
object PostgreSqlTextKeyLiveCheck {
  def main(args: Array[String]): Unit = {
    require(args.length==5,"JDBC USER PASSWORD_ENV NEW_SCHEMA WORKLOAD_DIRECTORY")
    require(args(3).matches("yrw_text_key_test_[a-z0-9]+"))
    val ds = new PGSimpleDataSource()
    ds.setURL(args(0)); ds.setUser(args(1)); ds.setPassword(sys.env(args(2)))
    val c = ds.getConnection
    var checks = 0; var nonempty = 0; var sourceQueries = 0
    def compare(source:String,generated:String): Unit = {
      val expected = ExactResultMultiset.execute(c,source)
      val actual = ExactResultMultiset.execute(c,generated)
      val difference = expected.difference(actual)
      require(difference.isEmpty,s"${difference.render}\n$source\n$generated")
      checks += 1; if(!expected.isEmpty) nonempty += 1
    }
    try {
      val s = c.createStatement(); val schema = args(3)
      s.execute("SET statement_timeout=10000")
      s.execute(s"CREATE SCHEMA $schema")
      s.execute(s"SET search_path=pg_catalog,$schema")
      val planner = CalciteSqlPlanner.forJdbcDataSourceWithTextKeys(ds,schema,3).toOption.get
      val rewriter = PostgreSqlQueryRewriter.create(planner).toOption.get
      val profile = PostgreSqlAggregateRenderingProfileV2.create(
        PostgreSqlAggregateRenderingProfile.Values.find(p =>
          p.oneBagTerminal == PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2 &&
          p.reductionFinalization == PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2 &&
          p.reductionSupport == PostgreSqlReductionSupportProfile.DirectExistsV2).get,
        PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2,
        PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2).toOption.get
      def rewrites(source:String,width:Int,cycle:Boolean,lane:String,connected:Boolean=true): Vector[String] = {
        val result = rewriter.rewriteDetailed(RewriteRequest(source,options=RewriteOptions(
          widthBound=width,planLimit=1,requireConnectedCover=connected,
          reductionMode=YannakakisReductionMode.FullTwoPass)))
        val a = result.report.artifact.getOrElse(sys.error(source+"\n"+result.outcome))
        require(a.query.textKeyProof.nonEmpty)
        if(cycle && width==2) require(a.bagFactorPlan.factors.size>1,"cyclic fixture must exercise multiple bags")
        val extra = if(lane=="enumeration") Vector(YannakakisReductionMode.BottomUpOnly,YannakakisReductionMode.NoReduction).map { mode =>
          PostgreSqlBagFactorRenderer.render(a.query,a.candidateInput,a.bagFactorPlan,
            PostgreSqlRenderingLimits.default,BagFactorPlanningLimits.default,mode).toOption.get.sql
        } else if(lane=="min") Vector.empty else Vector(
          PostgreSqlPhysicalOptimizationCompiler.render(a.query,a.candidateInput,a.bagFactorPlan,profile,
            PostgreSqlPhysicalOptimizationCompiler.Options(partialSupport=true))
            .fold(p=>sys.error(p.mkString(";")),_.sql))
        val fused = if(lane=="global") {
          val v3 = PostgreSqlAggregateRenderingProfileV3.fusedRawFactorMessages(
            PostgreSqlAggregateRenderingProfileV2.baseline(PostgreSqlAggregateRenderingProfile.LegacyV1).toOption.get).toOption.get
          Vector(PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfileV3(a.query,a.candidateInput,
            a.bagFactorPlan,PostgreSqlRenderingLimits.default,BagFactorPlanningLimits.default,
            YannakakisReductionMode.NoReduction,v3,PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2)
            .fold(p=>sys.error(p.mkString(";")),_._2.sql))
        } else Vector.empty
        val branch = if(!cycle && lane=="global")
          PostgreSqlAcyclicBranchCountRenderer.render(a.query,a.candidateInput,a.bagFactorPlan)
            .toOption.map(_.sql).toVector else Vector.empty
        sourceQueries += 1
        (Vector(a.renderedQuery.sql) ++ extra ++ fused ++ branch).distinct
      }
      val types = Vector("VARCHAR(64)","TEXT","VARCHAR", "VARCHAR(64) COLLATE \"C\"")
      types.zipWithIndex.foreach { case(t,index) =>
        val table = s"edge$index"
        s.execute(s"CREATE TABLE $schema.$table(a $t,b $t,p INTEGER)")
        val chain = s" FROM $table e0 JOIN $table e1 ON e0.b=e1.a JOIN $table e2 ON e1.b=e2.a JOIN $table e3 ON e2.b=e3.a"
        val pairs = (for {
          cycle <- Vector(false,true)
          filtered <- Vector(false,true)
          lane <- Vector("global","g1","u2","u3","enumeration","min")
        } yield {
          val keys = lane match { case "g1"=>Vector("e0.b"); case "u2"=>Vector("e0.b","e2.b")
            case "u3"=>Vector("e0.b","e1.b","e2.b"); case _=>Vector.empty }
          val select = if(keys.nonEmpty) keys.mkString(",")+",COUNT(*)" else lane match {
            case "enumeration"=>"e0.a,e1.a,e2.a,e0.p"; case "min"=>"MIN(e1.a)"; case _=>"COUNT(*)" }
          val source = "SELECT "+select+chain+(if(cycle) " AND e3.b=e0.a" else "")+
            (if(filtered) " WHERE e0.a IN ('A','a','é','') AND e1.p >= -1" else "")+
            (if(keys.nonEmpty) " GROUP BY "+keys.mkString(",") else "")
          rewrites(source,if(cycle) 2 else 1,cycle,lane).map(source->_)
        }).flatten
        val insert = c.prepareStatement(s"INSERT INTO $schema.$table VALUES (?,?,?)")
        val values = Vector(null,"","A","a","a ","é","e\u0301","O'Brien; --","1","01")
        val random = new scala.util.Random(90927+index)
        for(trial <- 0 until 10) {
          s.execute(s"DELETE FROM $schema.$table")
          if(trial%5 != 0) for(row <- 0 until 12) {
            insert.setString(1,if(trial==1) values(row%values.size) else if(row<4) "a" else values(random.nextInt(values.size)))
            insert.setString(2,if(trial==1) values(row%values.size) else if(row<4) "a" else values(random.nextInt(values.size)))
            insert.setObject(3,if(row%3==0) null else Int.box(row-2)); insert.executeUpdate()
          }
          pairs.foreach { case(source,generated) => compare(source,generated) }
        }
        insert.close()
        val star = s"SELECT COUNT(*) FROM $table e0 JOIN $table e1 ON e0.a=e1.a JOIN $table e2 ON e0.a=e2.a JOIN $table e3 ON e0.a=e3.a"
        rewrites(star,1,false,"global").foreach(compare(star,_))
        val repeated = s"SELECT COUNT(*) FROM $table x JOIN $table y ON x.a=y.a AND x.b=y.a"
        rewrites(repeated,1,false,"global").foreach(compare(repeated,_))
      }
      // Adversarial catalog types/collations and changed schemas must fail closed.
      s.execute(s"CREATE DOMAIN $schema.key_domain AS VARCHAR(64)")
      s.execute(s"CREATE TABLE $schema.bad_char(a CHAR(64),b CHAR(64))")
      s.execute(s"CREATE TABLE $schema.bad_domain(a $schema.key_domain,b $schema.key_domain)")
      s.execute(s"CREATE TABLE $schema.bad_collation(a VARCHAR(64) COLLATE \"C\",b VARCHAR(64) COLLATE \"POSIX\")")
      s.execute(s"CREATE TABLE $schema.bad_length(a VARCHAR(32),b VARCHAR(64))")
      s.execute(s"CREATE TABLE $schema.bad_mixed(a TEXT,b VARCHAR(64))")
      s.execute(s"CREATE VIEW $schema.bad_view AS SELECT * FROM $schema.edge0")
      def rejected(table:String): Unit = {
        val sql = s"SELECT COUNT(*) FROM $table x JOIN $table y ON x.b=y.a"
        require(rewriter.rewriteDetailed(RewriteRequest(sql)).report.artifact.isEmpty,table)
      }
      Vector("bad_char","bad_domain","bad_collation","bad_length","bad_mixed","bad_view").foreach(rejected)
      val explicit = "SELECT COUNT(*) FROM edge0 x JOIN edge0 y ON x.b COLLATE \"C\" = y.a"
      require(rewriter.rewriteDetailed(RewriteRequest(explicit)).report.artifact.isEmpty)
      val hasIcu = s.executeQuery("SELECT EXISTS(SELECT 1 FROM pg_collation WHERE collprovider='i')")
      hasIcu.next(); val icu = hasIcu.getBoolean(1); hasIcu.close()
      if(icu) {
        s.execute(s"CREATE COLLATION $schema.insensitive(provider=icu,locale='und-u-ks-level2',deterministic=false)")
        s.execute(s"CREATE TABLE $schema.bad_nondeterministic(a VARCHAR(64) COLLATE $schema.insensitive,b VARCHAR(64) COLLATE $schema.insensitive)")
        rejected("bad_nondeterministic")
      }
      s.execute(s"CREATE TABLE $schema.mutable_key(a VARCHAR(64),b VARCHAR(64))")
      val mutableSql = "SELECT COUNT(*) FROM mutable_key x JOIN mutable_key y ON x.b=y.a"
      require(rewriter.rewriteDetailed(RewriteRequest(mutableSql)).report.artifact.nonEmpty)
      s.execute(s"ALTER TABLE $schema.mutable_key ALTER COLUMN a TYPE CHAR(64), ALTER COLUMN b TYPE CHAR(64)")
      rejected("mutable_key")
      s.execute(s"CREATE TABLE $schema.integer_edge(a INTEGER,b INTEGER)")
      val integerSql = "SELECT COUNT(*) FROM integer_edge x JOIN integer_edge y ON x.b=y.a"
      val legacyPlanner = CalciteSqlPlanner.forJdbcDataSourceWithLocalPredicates(ds,schema,3).toOption.get
      val legacy = PostgreSqlQueryRewriter.create(legacyPlanner).toOption.get
        .rewriteDetailed(RewriteRequest(integerSql)).report.artifact.get
      val extended = rewriter.rewriteDetailed(RewriteRequest(integerSql)).report.artifact.get
      require(legacy.renderedQuery.sql == extended.renderedQuery.sql,"integer SQL changed")
      // Exact eight registered Hetionet texts, original VARCHAR(64) schema.
      Vector("compound","gene","disease").foreach(t => {
        s.execute(s"CREATE TABLE $schema.$t(nid VARCHAR(64) PRIMARY KEY,name TEXT)")
        s.execute(s"INSERT INTO $schema.$t VALUES ('1','one'),('2','two')")
      })
      Vector("binds","interacts","associates").foreach(t => {
        s.execute(s"CREATE TABLE $schema.$t(sid VARCHAR(64),tid VARCHAR(64))")
        s.execute(s"INSERT INTO $schema.$t VALUES ('1','1'),('1','1'),('1','2'),('2','1'),(NULL,'1'),('1',NULL)")
      })
      for(cycle <- Vector(false,true); lane <- Vector("global","g1","u2","u3")) {
        val name = "hetionet-thesis-cbgigigad-"+(if(cycle) "cyclic" else "acyclic")+"-"+lane+"-equality-core.sql"
        val source = Files.readString(Paths.get(args(4),"queries",name)).trim.stripSuffix(";")
        rewrites(source,if(cycle) 3 else 1,cycle,lane,connected= !cycle).foreach(compare(source,_))
      }
      s.close()
      println(s"EXACT checks=$checks nonempty=$nonempty sourceQueries=$sourceQueries hetionetQueries=8 nondeterministicCollationTest=$icu")
    } finally c.close()
  }
}

/** Check every byte-distinct action of the benchmark exporter, not just its head. */
object PostgreSqlTextKeyExportLiveCheck {
  def main(args:Array[String]): Unit = {
    require(args.length==5,"JDBC USER PASSWORD_ENV SCHEMA EXPORT_ROOT")
    require(args(3).matches("yrw_text_key_test_[a-z0-9]+"))
    val ds = new PGSimpleDataSource()
    ds.setURL(args(0)); ds.setUser(args(1)); ds.setPassword(sys.env(args(2)))
    val c = ds.getConnection
    try {
      val statement = c.createStatement()
      statement.execute(s"SET search_path=pg_catalog,${args(3)}")
      statement.execute("SET statement_timeout=10000")
      val folders = Files.list(Paths.get(args(4)))
      var checks = 0
      try {
        import scala.jdk.CollectionConverters._
        folders.iterator().asScala.filter(Files.isDirectory(_)).foreach { folder =>
          val manifest = ujson.read(Files.readString(folder.resolve("manifest.json")))
          val source = Files.readString(folder.resolve("source.sql"))
          val expected = ExactResultMultiset.execute(c,source)
          manifest("actions").arr.filter(_("status").str=="rendered").foreach { action =>
            val sql = Files.readString(folder.resolve(action("name").str+".sql"))
            val diff = expected.difference(ExactResultMultiset.execute(c,sql))
            require(diff.isEmpty,folder.toString+" "+action("name").str+" "+diff.render)
            checks += 1
          }
        }
      } finally folders.close()
      println(s"EXPORT_EXACT checks=$checks")
      statement.close()
    } finally c.close()
  }
}
