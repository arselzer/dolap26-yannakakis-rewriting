package at.ac.tuwien.dbai.yannakakis.integration

import at.ac.tuwien.dbai.yannakakis.integration.{
  ExactResultMultisetV2 => ExactResultMultiset
}
import java.io.PrintWriter
import java.sql.{Connection, DriverManager}
import java.util.UUID
import java.util.logging.Logger
import javax.sql.DataSource

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidateSearchInput,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanner
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlBagSupportStrategy,
  PostgreSqlBagFactorRenderer,
  PostgreSqlRenderedQuery
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCandidateSelectionDecision,
  PostgreSqlExplainCostEstimate,
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteArtifact
}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.PredicateClassification
import org.apache.calcite.adapter.jdbc.JdbcSchema
import org.testcontainers.postgresql.PostgreSQLContainer

private object PostgreSqlTriangleMultisetSuite {
  private final case class ExternalDatabase(
      jdbcUrl: String,
      username: String,
      password: String
  )

  /** Makes accidental ownership transfer observable without changing JDBC. */
  private final class CloseTrackingDataSource(delegate: DataSource)
      extends DataSource
      with AutoCloseable {
    require(delegate != null, "delegate data source must not be null")

    @volatile private var closeCalled = false
    def wasClosed: Boolean = closeCalled

    override def close(): Unit = closeCalled = true
    override def getConnection(): Connection = delegate.getConnection()
    override def getConnection(user: String, password: String): Connection =
      delegate.getConnection(user, password)
    override def getLogWriter(): PrintWriter = delegate.getLogWriter()
    override def setLogWriter(out: PrintWriter): Unit = delegate.setLogWriter(out)
    override def setLoginTimeout(seconds: Int): Unit =
      delegate.setLoginTimeout(seconds)
    override def getLoginTimeout(): Int = delegate.getLoginTimeout()
    override def getParentLogger(): Logger = delegate.getParentLogger()
    override def isWrapperFor(iface: Class[_]): Boolean =
      iface.isInstance(this) || delegate.isWrapperFor(iface)
    override def unwrap[T](iface: Class[T]): T =
      if (iface.isInstance(this)) iface.cast(this) else delegate.unwrap(iface)
  }
}

class PostgreSqlTriangleMultisetSuite extends munit.FunSuite {
  import PostgreSqlTriangleMultisetSuite.{
    CloseTrackingDataSource,
    ExternalDatabase
  }

  private var postgres: PostgreSQLContainer = _
  private var connection: Connection = _
  private var activeDatabase: ExternalDatabase = _
  private var benchmarkRewriter: PostgreSqlQueryRewriter = _
  private var jdbcCatalogRewriter: PostgreSqlQueryRewriter = _
  private var jdbcDataSourceRewriter: PostgreSqlQueryRewriter = _
  private var trackedDataSource: CloseTrackingDataSource = _
  private var jdbcCycleSchema: String = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    activeDatabase = externalDatabase match {
      case Some(database) => database
      case None =>
        postgres = new PostgreSQLContainer("postgres:18.4-alpine")
          .withDatabaseName("triangle_multiset")
          .withUsername("triangle")
          .withPassword("triangle")
        postgres.start()
        ExternalDatabase(
          jdbcUrl = postgres.getJdbcUrl,
          username = postgres.getUsername,
          password = postgres.getPassword
        )
    }
    connection = DriverManager.getConnection(
      activeDatabase.jdbcUrl,
      activeDatabase.username,
      activeDatabase.password
    )
    TriangleMultisetFixture.install(connection)
    BenchmarkDerivedMultisetFixture.install(connection)
    benchmarkRewriter = PostgreSqlQueryRewriter
      .forSchema(BenchmarkDerivedCalciteFixture.rootSchema())
      .fold(
        error =>
          throw new IllegalStateException(s"${error.code}: ${error.message}"),
        identity
      )
    val schemaSuffix = UUID.randomUUID().toString.replace("-", "")
    jdbcCycleSchema = s"YannakakisCycle_$schemaSuffix"
    try {
      JdbcCatalogCycleFixture.install(connection, jdbcCycleSchema)
      jdbcCatalogRewriter = PostgreSqlQueryRewriter
        .forPostgreSqlJdbc(
          jdbcUrl = activeDatabase.jdbcUrl,
          user = activeDatabase.username,
          password = activeDatabase.password,
          schemaName = jdbcCycleSchema
        )
        .fold(
          error =>
            throw new IllegalStateException(s"${error.code}: ${error.message}"),
          identity
        )
      val delegateDataSource = JdbcSchema.dataSource(
        activeDatabase.jdbcUrl,
        "org.postgresql.Driver",
        activeDatabase.username,
        activeDatabase.password
      )
      trackedDataSource = new CloseTrackingDataSource(delegateDataSource)
      jdbcDataSourceRewriter = PostgreSqlQueryRewriter
        .forJdbcDataSource(
          dataSource = trackedDataSource,
          schemaName = jdbcCycleSchema
        )
        .fold(
          error =>
            throw new IllegalStateException(s"${error.code}: ${error.message}"),
          identity
        )
    } catch {
      case setupFailure: Throwable =>
        try JdbcCatalogCycleFixture.drop(connection, jdbcCycleSchema)
        catch {
          case cleanupFailure: Throwable =>
            if (cleanupFailure ne setupFailure)
              setupFailure.addSuppressed(cleanupFailure)
        }
        throw setupFailure
    }
  }

  override def afterAll(): Unit =
    try {
      if (connection != null && jdbcCycleSchema != null)
        JdbcCatalogCycleFixture.drop(connection, jdbcCycleSchema)
    } finally {
      try if (connection != null) connection.close()
      finally {
        try if (postgres != null) postgres.stop()
        finally super.afterAll()
      }
    }

  private def externalDatabase: Option[ExternalDatabase] = {
    val variableNames = Vector(
      "YANNAKAKIS_TEST_JDBC_URL",
      "YANNAKAKIS_TEST_DB_USER",
      "YANNAKAKIS_TEST_DB_PASSWORD"
    )
    val configured = variableNames.flatMap(name => sys.env.get(name).map(name -> _))

    if (configured.isEmpty) None
    else if (configured.size != variableNames.size) {
      val missing = variableNames.filterNot(sys.env.contains)
      throw new IllegalArgumentException(
        s"External PostgreSQL test configuration is incomplete; missing: ${missing.mkString(", ")}"
      )
    } else {
      val requiredNonBlank = Vector(
        "YANNAKAKIS_TEST_JDBC_URL",
        "YANNAKAKIS_TEST_DB_USER"
      )
      val blank = requiredNonBlank.filter(name => sys.env(name).trim.isEmpty)
      if (blank.nonEmpty)
        throw new IllegalArgumentException(
          s"External PostgreSQL test configuration must not be blank: ${blank.mkString(", ")}"
        )

      Some(
        ExternalDatabase(
          jdbcUrl = sys.env("YANNAKAKIS_TEST_JDBC_URL"),
          username = sys.env("YANNAKAKIS_TEST_DB_USER"),
          password = sys.env("YANNAKAKIS_TEST_DB_PASSWORD")
        )
      )
    }
  }

  test("fixture contains exact duplicate full tuples in every base relation") {
    Vector("triangle_r", "triangle_s", "triangle_t").foreach { table =>
      val rows = execute(s"SELECT * FROM $table")
      assertEquals(rows.totalMultiplicity, 7L, table)
      assertEquals(rows.distinctRowCount, 6, table)
      assert(rows.containsDuplicate, s"$table must contain an exact duplicate")
    }
  }

  test(
    "benchmark-derived fixtures contain exact duplicates and NULL join keys"
  ) {
    BenchmarkDerivedMultisetFixture.LsqbQ2Tables.foreach { table =>
      val rows = execute(s"SELECT * FROM $table")
      assertEquals(rows.totalMultiplicity, 6L, table)
      assertEquals(rows.distinctRowCount, 5, table)
      assert(rows.containsDuplicate, s"$table must contain an exact duplicate")
      assert(rows.containsNull, s"$table must contain NULL test data")
    }

    BenchmarkDerivedMultisetFixture.TpchQ5DerivedTables.foreach { table =>
      val rows = execute(s"SELECT * FROM $table")
      assertEquals(rows.totalMultiplicity, 5L, table)
      assertEquals(rows.distinctRowCount, 4, table)
      assert(rows.containsDuplicate, s"$table must contain an exact duplicate")
      assert(rows.containsNull, s"$table must contain NULL test data")
    }

    Vector(
      "lsqb_q2_knows" -> "person1_id IS NULL OR person2_id IS NULL",
      "lsqb_q2_comment" -> "creator_id IS NULL OR reply_post_id IS NULL",
      "lsqb_q2_post" -> "post_id IS NULL OR creator_id IS NULL",
      "q5d_customer" -> "customer_id IS NULL",
      "q5d_orders" -> "customer_id IS NULL",
      "q5d_lineitem" -> "supplier_id IS NULL",
      "q5d_supplier" -> "supplier_id IS NULL",
      "q5d_nation" -> "region_id IS NULL",
      "q5d_region" -> "region_id IS NULL"
    ).foreach { case (table, predicate) =>
      assert(
        !execute(s"SELECT * FROM $table WHERE $predicate").isEmpty,
        s"$table must contain a NULL join key"
      )
    }
  }

  test(
    "public API rewrites the adapted LSQB Q2 cyclic enumeration core exactly"
  ) {
    val source = BenchmarkDerivedMultisetFixture.LsqbQ2EnumerationCore
    val (artifact, generatedSql) = publicGenerated(source)
    val native = execute(source)
    val actual = execute(generatedSql)

    assertEquals(
      native.totalMultiplicity,
      BenchmarkDerivedMultisetFixture.LsqbQ2ExpectedMultiplicity
    )
    assertEquals(native.distinctRowCount, 2)
    assert(native.containsDuplicate, "Q2 fixture must exercise bag semantics")
    assert(native.containsNull, "Q2 fixture must return NULL payloads")
    assertEquals(
      native.schema.map(_.columnLabel),
      Vector(
        "person1_id",
        "person2_id",
        "comment_id",
        "post_id",
        "knows_payload",
        "comment_payload",
        "post_payload"
      )
    )
    assertEquals(
      artifact.renderedQuery.output.map(_.name),
      native.schema.map(_.columnLabel)
    )

    val hypergraph = artifact.hypergraph
    val edgeVertices = hypergraph.edges.map(_.vertices.toSet)
    val pairwiseIntersections =
      for {
        left <- edgeVertices.indices
        right <- 0 until left
      } yield edgeVertices(left).intersect(edgeVertices(right)).size

    assertEquals(hypergraph.edges.size, 3)
    assertEquals(hypergraph.variables.size, 3)
    assertEquals(edgeVertices.map(_.size), Vector(2, 2, 2))
    assertEquals(edgeVertices.distinct.size, 3)
    assertEquals(pairwiseIntersections.toVector, Vector(1, 1, 1))
    assertEquals(hypergraph.incidence.map(_._2.size), Vector(2, 2, 2))

    val decomposition = artifact.selectedPlan.decomposition
    assertEquals(decomposition.width, 2)
    assertEquals(decomposition.bags.size, 1)
    assertEquals(decomposition.treeEdges.size, 0)
    assertEquals(decomposition.bags.head.vertices, hypergraph.variables)
    assertEquals(decomposition.bags.head.coverWidth, 2)
    assertEquals(artifact.bagFactorPlan.sourceWidthBound, 2)
    assertEquals(artifact.bagFactorPlan.factors.size, 1)
    assertEquals(artifact.bagFactorPlan.schedule.bottomUp.size, 0)
    assertEquals(artifact.bagFactorPlan.schedule.topDown.size, 0)
    assertEquals(
      artifact.renderedQuery.trace.bags.map(_.supportStrategy),
      Vector(PostgreSqlBagSupportStrategy.ElidedByOwnedCover)
    )
    assert(artifact.renderedQuery.trace.bags.forall(_.supportCte.isEmpty))
    assertEquals(artifact.renderedQuery.statistics.expandedScanCount, 3L)
    assertEquals(artifact.renderedQuery.statistics.cteCount, 1L)
    assert(!generatedSql.contains("SELECT DISTINCT"), generatedSql)
    assert(!generatedSql.contains("IS NOT DISTINCT FROM"), generatedSql)
    assertEquivalent(native, actual)
  }

  test(
    "public API rewrites the non-comparable TPC-H Q5-derived core exactly in every reduction mode"
  ) {
    val source = BenchmarkDerivedMultisetFixture.TpchQ5DerivedEnumerationCore
    val native = execute(source)
    val generatedByMode = YannakakisReductionMode.Values.map { mode =>
      mode -> publicGenerated(benchmarkRewriter, source, mode)
    }
    generatedByMode.foreach { case (mode, (modeArtifact, modeSql)) =>
      val actual = execute(modeSql)
      assertEquivalent(native, actual)
      assertEquals(modeArtifact.renderedQuery.trace.reduction.mode, mode)
      assertEquals(
        modeArtifact.candidateSelection.reductionMode,
        mode
      )
      val reduction = modeArtifact.renderedQuery.trace.reduction
      mode match {
        case YannakakisReductionMode.FullTwoPass =>
          assertEquals(
            reduction.selectedBottomUpStepCount,
            modeArtifact.bagFactorPlan.schedule.bottomUp.size
          )
          assertEquals(
            reduction.selectedTopDownStepCount,
            modeArtifact.bagFactorPlan.schedule.topDown.size
          )
        case YannakakisReductionMode.BottomUpOnly =>
          assertEquals(
            reduction.selectedBottomUpStepCount,
            modeArtifact.bagFactorPlan.schedule.bottomUp.size
          )
          assertEquals(reduction.selectedTopDownStepCount, 0)
        case YannakakisReductionMode.NoReduction =>
          assertEquals(modeArtifact.renderedQuery.trace.semijoins, Vector.empty)
          assertEquals(reduction.physicalReductionCteCount, 0)
      }
    }
    val (artifact, generatedSql) = generatedByMode.head._2
    val actual = execute(generatedSql)

    assertEquals(
      native.totalMultiplicity,
      BenchmarkDerivedMultisetFixture.TpchQ5DerivedExpectedMultiplicity
    )
    assertEquals(native.distinctRowCount, 2)
    assert(native.containsDuplicate, "Q5-derived fixture must exercise bags")
    assert(native.containsNull, "Q5-derived fixture must return NULL payloads")
    assertEquals(
      native.schema.map(_.columnLabel),
      Vector(
        "customer_id",
        "order_id",
        "supplier_id",
        "nation_id",
        "region_id",
        "customer_payload",
        "orders_payload",
        "lineitem_payload",
        "supplier_payload",
        "nation_payload",
        "region_payload"
      )
    )
    assertEquals(
      artifact.renderedQuery.output.map(_.name),
      native.schema.map(_.columnLabel)
    )

    val hypergraph = artifact.hypergraph
    val edgesByTable = artifact.query.relations.map { relation =>
      relation.qualifiedName.last -> hypergraph
        .edgeFor(relation.id)
        .getOrElse(
          fail(s"missing edge for ${relation.qualifiedName.mkString(".")}")
        )
    }.toMap
    val cycleEdges = Vector(
      "q5d_customer",
      "q5d_orders",
      "q5d_lineitem",
      "q5d_supplier"
    ).map(table => edgesByTable(table))
    val cycleVertices = cycleEdges.flatMap(_.vertices).distinct

    assertEquals(hypergraph.edges.size, 6)
    assertEquals(hypergraph.variables.size, 5)
    assertEquals(
      hypergraph.edges.map(_.vertices.size).sorted,
      Vector(1, 2, 2, 2, 2, 2)
    )
    assertEquals(cycleEdges.map(_.vertices.size), Vector(2, 2, 2, 2))
    assertEquals(cycleEdges.map(_.vertices.toSet).distinct.size, 4)
    assertEquals(cycleVertices.size, 4)
    assertEquals(
      cycleVertices
        .map(variable => cycleEdges.count(_.contains(variable)))
        .sorted,
      Vector(2, 2, 2, 2)
    )

    val decomposition = artifact.selectedPlan.decomposition
    assertEquals(decomposition.width, 2)
    assert(decomposition.bags.size > 1, clues(decomposition))
    assert(decomposition.treeEdges.nonEmpty, clues(decomposition))
    assert(artifact.bagFactorPlan.factors.size > 1)
    assert(artifact.bagFactorPlan.schedule.bottomUp.nonEmpty)
    assert(artifact.bagFactorPlan.schedule.topDown.nonEmpty)
    assertEquals(
      artifact.renderedQuery.trace.semijoins.size,
      artifact.bagFactorPlan.schedule.bottomUp.size +
        artifact.bagFactorPlan.schedule.topDown.size
    )
    val logicalSemijoins = artifact.renderedQuery.trace.semijoins
    val physicalOutputs = logicalSemijoins.map(_.outputCte).distinct
    val fusedOutputs = logicalSemijoins
      .groupBy(_.outputCte)
      .values
      .filter(_.size > 1)
      .toVector
    assertEquals(fusedOutputs.size, 1)
    assertEquals(fusedOutputs.head.size, 2)
    assertEquals(fusedOutputs.head.map(_.pass).distinct.size, 1)
    assertEquals(fusedOutputs.head.map(_.target).distinct.size, 1)
    assertEquals(fusedOutputs.head.map(_.targetInputCte).distinct.size, 1)
    assertEquals(fusedOutputs.head.map(_.source).distinct.size, 2)
    physicalOutputs.foreach { outputCte =>
      assertEquals(
        count(generatedSql, s"  \"$outputCte\" AS ("),
        1,
        clues(outputCte, generatedSql)
      )
    }
    assertEquals(
      count(generatedSql, "WHERE EXISTS ("),
      physicalOutputs.size
    )
    assertEquals(
      count(generatedSql, "AND EXISTS ("),
      logicalSemijoins.size - physicalOutputs.size
    )
    assertEquals(
      artifact.renderedQuery.statistics.cteCount,
      artifact.renderedQuery.trace.bags.size.toLong +
        artifact.renderedQuery.trace.bags.count(_.supportCte.nonEmpty).toLong +
        physicalOutputs.size.toLong
    )
    val q5FactorByBag = artifact.bagFactorPlan.factors.map(factor =>
      factor.bagId -> factor
    ).toMap
    artifact.renderedQuery.trace.bags.foreach { trace =>
      val factor = q5FactorByBag(trace.bagId)
      val eligible = factor.supportCover.forall(factor.ownedEdgeIds.toSet)
      assertEquals(
        trace.supportStrategy ==
          PostgreSqlBagSupportStrategy.ElidedByOwnedCover,
        eligible
      )
      assertEquals(trace.supportCte.isEmpty, eligible)
    }
    assert(
      artifact.renderedQuery.trace.bags.exists(
        _.supportStrategy == PostgreSqlBagSupportStrategy.ElidedByOwnedCover
      ),
      clues(artifact.bagFactorPlan.factors)
    )
    assertEquals(artifact.bagFactorPlan.sourceWidthBound, 2)
    assert(!generatedSql.contains("IS NOT DISTINCT FROM"), generatedSql)
    assertEquivalent(native, actual)
  }

  test(
    "PostgreSQL JDBC catalog factory rewrites and executes a cyclic query"
  ) {
    val source = JdbcCatalogCycleFixture.UnqualifiedEnumerationCore
    val (artifact, generatedSql) =
      publicGenerated(jdbcCatalogRewriter, source)
    val qualifiedSource =
      JdbcCatalogCycleFixture.qualifiedEnumerationCore(jdbcCycleSchema)
    val (qualifiedArtifact, qualifiedGeneratedSql) =
      publicGenerated(jdbcCatalogRewriter, qualifiedSource)
    val (dataSourceArtifact, dataSourceGeneratedSql) =
      publicGenerated(jdbcDataSourceRewriter, source)
    val (unquotedArtifact, unquotedGeneratedSql) = publicGenerated(
      jdbcCatalogRewriter,
      JdbcCatalogCycleFixture.UnquotedEnumerationCore
    )
    val native = execute(qualifiedSource)
    val expectedRelations = JdbcCatalogCycleFixture.Tables.map(table =>
      Vector(jdbcCycleSchema, table)
    )
    val actual = execute(generatedSql)
    val dataSourceActual = execute(dataSourceGeneratedSql)
    val unquotedActual = execute(unquotedGeneratedSql)

    assertEquals(
      native.totalMultiplicity,
      JdbcCatalogCycleFixture.ExpectedMultiplicity
    )
    assertEquals(native.distinctRowCount, 2)
    assert(native.containsDuplicate, "JDBC fixture must exercise bag semantics")
    assert(native.containsNull, "JDBC fixture must return NULL payloads")
    assertEquals(
      artifact.renderedQuery.output.map(_.name),
      native.schema.map(_.columnLabel)
    )
    assertEquals(
      artifact.query.relations.map(_.qualifiedName.takeRight(2)),
      expectedRelations
    )
    assertEquals(
      qualifiedArtifact.query.relations.map(_.qualifiedName.takeRight(2)),
      expectedRelations
    )
    assertEquals(
      dataSourceArtifact.query.relations.map(_.qualifiedName.takeRight(2)),
      expectedRelations
    )
    assertEquals(
      unquotedArtifact.query.relations.map(_.qualifiedName.takeRight(2)),
      expectedRelations
    )
    assert(qualifiedGeneratedSql.trim.nonEmpty)
    assert(!trackedDataSource.wasClosed, "rewriter must not close caller DataSource")

    val hypergraph = artifact.hypergraph
    val edgeVertices = hypergraph.edges.map(_.vertices.toSet)
    assertEquals(hypergraph.edges.size, 3)
    assertEquals(hypergraph.variables.size, 3)
    assertEquals(edgeVertices.map(_.size), Vector(2, 2, 2))
    assertEquals(edgeVertices.distinct.size, 3)
    assertEquals(hypergraph.incidence.map(_._2.size), Vector(2, 2, 2))
    assertEquals(artifact.selectedPlan.decomposition.width, 2)
    assertEquals(artifact.selectedPlan.decomposition.bags.size, 1)
    assert(!generatedSql.contains("IS NOT DISTINCT FROM"), generatedSql)
    assertEquivalent(native, actual)
    assertEquivalent(native, dataSourceActual)
    assertEquivalent(native, unquotedActual)
  }

  test(
    "guarded EXPLAIN uses the configured mixed-case schema and independently minimal admissible estimate"
  ) {
    val source =
      "/* guarded source baseline with an unqualified mixed-case catalog */\n" +
        JdbcCatalogCycleFixture.UnqualifiedEnumerationCore
    val request = RewriteRequest(
      source,
      options = RewriteOptions(
        widthBound = 2,
        planLimit = 10,
        requireConnectedCover = true,
        candidateSelection =
          CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1,
        reductionMode = YannakakisReductionMode.NoReduction
      )
    )
    val detailed = jdbcDataSourceRewriter.rewriteDetailed(request)
    val selectedSql = detailed.outcome match {
      case RewriteOutcome.Rewritten(_, Vector(statement), _) => statement
      case other => fail(s"guarded EXPLAIN rewrite failed: $other")
    }
    val artifact = detailed.report.artifact.getOrElse(
      fail(s"guarded EXPLAIN success omitted its artifact: ${detailed.report}")
    )
    val decision = artifact.candidateSelection match {
      case value: PostgreSqlCandidateSelectionDecision.Selected => value
    }
    val plans = artifact.candidateSearch.plans

    val candidateSql = plans.map { plan =>
      val exact = jdbcDataSourceRewriter.rewriteDetailed(
        RewriteRequest(
          source,
          options = request.options.copy(
            candidateSelection = CandidatePlanSelection.StructuralRank(plan.rank)
          )
        )
      )
      val exactArtifact = exact.report.artifact.getOrElse(
        fail(s"retained rank ${plan.rank} omitted its artifact: $exact")
      )
      assertEquals(exactArtifact.selectedPlan.candidateId, plan.candidateId)
      exactArtifact.renderedQuery.sql
    }
    val independent = explainInConfiguredSchema(source +: candidateSql)
    val (sourceCost, sourceRows) = independent.head
    val denominator = sourceRows.max(BigDecimal(1))
    val eligible = plans.zip(independent.tail).filter { case (_, (_, rows)) =>
      rows * BigDecimal(10) >= denominator &&
      rows <= denominator * BigDecimal(10)
    }
    assert(eligible.nonEmpty, clues(sourceCost, sourceRows, independent))
    val expected = eligible.minBy { case (plan, (totalCost, _)) =>
      (totalCost, plan.rank, plan.candidateId.value)
    }

    assertEquals(artifact.selectedPlan.candidateId, expected._1.candidateId)
    assertEquals(artifact.selectedPlan.rank, expected._1.rank)
    assertEquals(
      decision.evaluations.map(_.structuralRank),
      plans.map(_.rank)
    )
    assertEquals(
      decision.sourceEvaluation.map(_.estimate.planRows),
      Some(sourceRows)
    )
    decision.evaluations.zip(independent.tail).foreach {
      case (evaluation, (totalCost, rows)) =>
        assertEquals(
          evaluation.explainCostEstimate.map(_.planTotalCost),
          Some(totalCost)
        )
        assertEquals(
          evaluation.explainCostEstimate.map(_.planRows),
          Some(rows)
        )
    }
    assertEquals(PostgreSqlExplainCostEstimate.Analyze, false)
    assertEquals(PostgreSqlExplainCostEstimate.RuntimeMeasured, false)

    val native = execute(
      JdbcCatalogCycleFixture.qualifiedEnumerationCore(jdbcCycleSchema)
    )
    val actual = execute(selectedSql)
    assertEquivalent(native, actual)
    assert(!trackedDataSource.wasClosed, "rewriter must not close caller DataSource")
  }

  test(
    "physical portfolio selection produces an executable exact enumeration multiset"
  ) {
    val source =
      "/* physical-portfolio live execution gate */\n" +
        JdbcCatalogCycleFixture.UnqualifiedEnumerationCore
    val detailed = jdbcDataSourceRewriter.rewriteDetailed(
      RewriteRequest(
        source,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = RewriteOptions.PhysicalPortfolioPlanLimit,
          requireConnectedCover = true,
          candidateSelection =
            CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1,
          reductionMode = YannakakisReductionMode.FullTwoPass
        )
      )
    )
    val selectedSql = detailed.outcome match {
      case RewriteOutcome.Rewritten(_, Vector(statement), _) => statement
      case other => fail(s"physical portfolio rewrite failed: $other")
    }
    val attempt = detailed.report.physicalPortfolioAttempt.getOrElse(
      fail(s"physical portfolio success omitted its attempt: ${detailed.report}")
    )
    val artifact = detailed.report.physicalPortfolioArtifact.getOrElse(
      fail(s"physical portfolio success omitted its artifact: ${detailed.report}")
    )
    val selected = attempt.decision match {
      case value: at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlPhysicalPortfolioDecision.Selected =>
        value
      case other => fail(s"expected selected physical portfolio, received $other")
    }

    assert(
      at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlPhysicalPortfolioSelectionReplay
        .verifyAttempt(attempt)
    )
    assert(selected.evaluations.nonEmpty)
    val selectedEntry = selected.enumeration.entries
      .find(_.variant.physicalVariantId == selected.selectedPhysicalVariantId)
      .getOrElse(fail("selected physical variant was not enumerated"))
    assertEquals(
      artifact.configuredPlan.configuration,
      selectedEntry.variant.configuration
    )
    assertEquals(artifact.renderedQuery.sql, selectedSql)

    val native = execute(
      JdbcCatalogCycleFixture.qualifiedEnumerationCore(jdbcCycleSchema)
    )
    val actual = execute(selectedSql)
    assertEquals(native.totalMultiplicity, JdbcCatalogCycleFixture.ExpectedMultiplicity)
    assertEquivalent(native, actual)
    assert(!trackedDataSource.wasClosed, "rewriter must not close caller DataSource")
  }

  test("PostgreSQL quoted identifiers retain exact case") {
    val correctlyQualified =
      JdbcCatalogCycleFixture.qualifiedEnumerationCore(jdbcCycleSchema)
    val correct = jdbcCatalogRewriter.rewriteDetailed(
      RewriteRequest(correctlyQualified)
    )
    assert(correct.outcome.isInstanceOf[RewriteOutcome.Rewritten])

    val wrongTableCase = JdbcCatalogCycleFixture.UnqualifiedEnumerationCore
      .replace("\"jdbc_cycle_r\"", "\"JDBC_CYCLE_R\"")
    val wrong = jdbcCatalogRewriter.rewriteDetailed(
      RewriteRequest(wrongTableCase)
    )
    assert(
      wrong.outcome.isInstanceOf[RewriteOutcome.InvalidQuery],
      clues(wrong.outcome)
    )
    intercept[java.sql.SQLException] {
      execute(wrongTableCase)
    }
  }

  test("JDBC metadata underscores cannot leak sibling schemas or table columns") {
    val token = UUID.randomUUID().toString.replace("-", "").take(16)
    val targetSchema = s"lk_$token"
    val siblingSchema = s"lkx$token"
    def quoted(value: String): String =
      "\"" + value.replace("\"", "\"\"") + "\""
    val target = quoted(targetSchema)
    val sibling = quoted(siblingSchema)

    val statement = connection.createStatement()
    try {
      Vector(
        s"CREATE SCHEMA $target",
        s"CREATE SCHEMA $sibling",
        s"CREATE TABLE $target.\"r\" (a INTEGER, b INTEGER)",
        s"CREATE TABLE $target.\"s\" (b INTEGER, c INTEGER)",
        s"CREATE TABLE $target.\"t\" (c INTEGER, a INTEGER)",
        s"CREATE TABLE $target.\"p_q\" (a INTEGER, b INTEGER)",
        s"CREATE TABLE $target.\"pAq\" (zz INTEGER)",
        s"CREATE TABLE $sibling.\"q\" (c INTEGER, a INTEGER)"
      ).foreach(statement.execute)

      val rewriter = PostgreSqlQueryRewriter
        .forPostgreSqlJdbc(
          jdbcUrl = activeDatabase.jdbcUrl,
          user = activeDatabase.username,
          password = activeDatabase.password,
          schemaName = targetSchema
        )
        .fold(
          error => fail(s"${error.code}: ${error.message}"),
          identity
        )

      val siblingSchemaLeak =
        """SELECT "r"."a"
          |FROM "r"
          |JOIN "s" ON "r"."b" = "s"."b"
          |JOIN "q" ON "s"."c" = "q"."c" AND "q"."a" = "r"."a"
          |""".stripMargin
      val siblingTableColumnLeak =
        """SELECT "p"."zz"
          |FROM "p_q" AS "p"
          |JOIN "s" ON "p"."b" = "s"."b"
          |JOIN "t" ON "s"."c" = "t"."c" AND "t"."a" = "p"."a"
          |""".stripMargin

      Vector(siblingSchemaLeak, siblingTableColumnLeak).foreach { sql =>
        val detailed = rewriter.rewriteDetailed(RewriteRequest(sql))
        assert(
          !detailed.outcome.isInstanceOf[RewriteOutcome.Rewritten],
          clues(sql, detailed.outcome, detailed.report)
        )
        assertEquals(detailed.report.artifact, None, clues(sql))
      }
    } finally {
      try statement.execute(s"DROP SCHEMA IF EXISTS $target CASCADE")
      finally {
        try statement.execute(s"DROP SCHEMA IF EXISTS $sibling CASCADE")
        finally statement.close()
      }
    }
  }

  test("support filtering preserves the exact triangle enumeration multiset") {
    val native = execute(TriangleMultisetFixture.nativeEnumeration())
    val rewritten = execute(
      TriangleMultisetFixture.supportFilteredEnumeration()
    )

    assertEquals(
      native.totalMultiplicity,
      TriangleMultisetFixture.ExpectedEnumerationMultiplicity
    )
    assert(native.containsDuplicate, "fixture must exercise duplicate outputs")
    assert(native.containsNull, "fixture must return successful rows with NULL payloads")
    assertEquivalent(native, rewritten)
  }

  test("generated k=2 ConCov SQL preserves triangle schema and multiset") {
    val rendered = generated(TriangleMultisetFixture.QualifiedNativeEnumeration)
    val mixedRendered = generated(
      TriangleMultisetFixture.QualifiedNativeEnumeration,
      widthBound = 2,
      decompositionOverride = Some(secondCandidateDecomposition)
    )
    val native = execute(TriangleMultisetFixture.QualifiedNativeEnumeration)
    val actual = execute(rendered.sql)
    val mixedActual = execute(mixedRendered.sql)

    assertEquals(
      native.totalMultiplicity,
      TriangleMultisetFixture.ExpectedEnumerationMultiplicity
    )
    assert(native.containsDuplicate)
    assert(native.containsNull)
    assertEquals(
      native.schema.map(_.columnLabel),
      Vector("x", "y", "z", "r_payload", "s_payload", "t_payload")
    )
    assertEquals(
      actual.schema.map(_.columnLabel),
      native.schema.map(_.columnLabel)
    )
    assertEquals(rendered.output.map(_.name), native.schema.map(_.columnLabel))
    assertEquals(
      rendered.sourceQuery.relations.map(_.qualifiedName.takeRight(2)),
      Vector(
        Vector("pg_temp", "triangle_r"),
        Vector("pg_temp", "triangle_s"),
        Vector("pg_temp", "triangle_t")
      )
    )
    val postFactorColumnIds = (
      rendered.sourceQuery.output.iterator.flatMap(_.expression.columnRefs.iterator) ++
        rendered.sourceQuery.predicates.iterator.collect {
          case predicate
              if predicate.classification
                .isInstanceOf[PredicateClassification.InnerEquality] =>
            predicate.expression
        }.flatMap(_.columnRefs.iterator)
    ).map(_.id).toSet
    val expectedOwnedColumns = rendered.bagFactorPlan.sourceHypergraph.edges
      .map { edge =>
        val relation = rendered.sourceQuery.relations.find(
          _.id == edge.relationOccurrenceId
        ).get
        val carriedIds = edge.bindings.iterator.map(_._1).toSet ++ postFactorColumnIds
        relation.columns.count(metadata => carriedIds(metadata.ref.id))
      }
      .sum
    assertEquals(
      rendered.trace.bags.flatMap(_.occurrenceColumns).size,
      expectedOwnedColumns
    )
    assertEquals(
      rendered.trace.bags.map(_.supportStrategy),
      Vector(PostgreSqlBagSupportStrategy.ElidedByOwnedCover)
    )
    assert(rendered.trace.bags.forall(_.supportCte.isEmpty))
    assertEquals(rendered.statistics.expandedScanCount, 3L)
    assertEquals(rendered.statistics.cteCount, 1L)
    assert(!rendered.sql.contains("SELECT DISTINCT"), rendered.sql)
    assert(!rendered.sql.contains("IS NOT DISTINCT FROM"), rendered.sql)
    assertEquivalent(native, actual)

    assertEquals(mixedRendered.bagFactorPlan.factors.size, 2)
    val mixedFactorByBag = mixedRendered.bagFactorPlan.factors.map(factor =>
      factor.bagId -> factor
    ).toMap
    val mixedMaterialized = mixedRendered.trace.bags.filter(
      _.supportStrategy ==
        PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
    )
    assertEquals(mixedMaterialized.size, 1)
    assert(mixedMaterialized.head.supportCte.nonEmpty)
    val materializedFactor = mixedFactorByBag(mixedMaterialized.head.bagId)
    assert(!materializedFactor.supportCover.forall(
      materializedFactor.ownedEdgeIds.toSet
    ))
    assert(mixedRendered.sql.contains("SELECT DISTINCT"), mixedRendered.sql)
    assertEquivalent(native, mixedActual)
  }

  test(
    "generated SQL preserves PostgreSQL's repeated labels for joined SELECT *"
  ) {
    val source = jdbcCycleProjection("*", qualified = false)
    val native = execute(jdbcCycleProjection("*", qualified = true))
    assertEquals(
      native.schema.map(_.columnLabel),
      Vector(
        "row_id",
        "x",
        "y",
        "payload",
        "row_id",
        "y",
        "z",
        "payload",
        "row_id",
        "z",
        "x",
        "payload"
      )
    )

    YannakakisReductionMode.Values.foreach { mode =>
      val (_, generatedSql) = publicGenerated(jdbcCatalogRewriter, source, mode)
      val actual = execute(generatedSql)
      val difference = native.difference(actual)
      assert(difference.isEmpty, s"$mode failed:\n${difference.render}")
    }
  }

  test(
    "generated SQL preserves PostgreSQL's ?column? label for an unaliased equality"
  ) {
    val projection = "\"r\".\"x\" = \"t\".\"x\""
    val source = jdbcCycleProjection(projection, qualified = false)
    val native = execute(jdbcCycleProjection(projection, qualified = true))
    assertEquals(native.schema.map(_.columnLabel), Vector("?column?"))

    YannakakisReductionMode.Values.foreach { mode =>
      val (_, generatedSql) = publicGenerated(jdbcCatalogRewriter, source, mode)
      val actual = execute(generatedSql)
      val difference = native.difference(actual)
      assert(difference.isEmpty, s"$mode failed:\n${difference.render}")
    }
  }

  test(
    "generated SQL retains nullable WHERE r.a = r.a as a filtering predicate"
  ) {
    val source = TriangleMultisetFixture.QualifiedSelfEqualityEnumeration
    val rendered = generated(source)
    val native = execute(source)
    val actual = execute(rendered.sql)
    val withoutPredicate = execute(
      TriangleMultisetFixture.QualifiedNativeEnumeration
    )

    assert(
      rendered.sourceQuery.predicates.exists(
        _.classification.isInstanceOf[PredicateClassification.LocalFilter]
      ),
      clues(rendered.sourceQuery.predicates)
    )
    assertEquals(
      native.totalMultiplicity,
      TriangleMultisetFixture.ExpectedSelfEqualityMultiplicity
    )
    assertEquals(
      withoutPredicate.totalMultiplicity,
      TriangleMultisetFixture.ExpectedEnumerationMultiplicity
    )
    assert(!native.difference(withoutPredicate).isEmpty)
    assert(!rendered.sql.contains("IS NOT DISTINCT FROM"), rendered.sql)
    assertEquivalent(native, actual)
  }

  test("generated multi-bag chain SQL executes both semijoin passes") {
    val source = TriangleMultisetFixture.QualifiedChainEnumeration
    val rendered = generated(source, widthBound = 1)
    val native = execute(source)
    val actual = execute(rendered.sql)

    assert(rendered.bagFactorPlan.factors.size > 1)
    assert(rendered.bagFactorPlan.schedule.bottomUp.nonEmpty)
    assert(rendered.bagFactorPlan.schedule.topDown.nonEmpty)
    assertEquals(
      rendered.trace.semijoins.size,
      rendered.bagFactorPlan.schedule.bottomUp.size +
        rendered.bagFactorPlan.schedule.topDown.size
    )
    assertEquals(
      native.totalMultiplicity,
      TriangleMultisetFixture.ExpectedChainMultiplicity
    )
    assert(native.containsDuplicate)
    assert(native.containsNull)
    assertEquals(
      native.schema.map(_.columnLabel),
      Vector(
        "a",
        "b",
        "c",
        "d",
        "e",
        "r_payload",
        "s_payload",
        "t_payload",
        "u_payload"
      )
    )
    assertEquivalent(native, actual)
  }

  test("support filtering preserves COUNT(*) over the triangle bag") {
    val native = execute(TriangleMultisetFixture.nativeCount())
    val rewritten = execute(TriangleMultisetFixture.supportFilteredCount())

    assertValueOracle(
      expectedCount(TriangleMultisetFixture.ExpectedEnumerationMultiplicity),
      native
    )
    assertEquivalent(native, rewritten)
  }

  test("support filtering preserves empty enumeration and zero COUNT(*)") {
    val impossiblePredicate = "r.x = -1"
    val nativeEnumeration = execute(
      TriangleMultisetFixture.nativeEnumeration(impossiblePredicate)
    )
    val rewrittenEnumeration = execute(
      TriangleMultisetFixture.supportFilteredEnumeration(impossiblePredicate)
    )

    assert(nativeEnumeration.isEmpty)
    assertEquivalent(nativeEnumeration, rewrittenEnumeration)

    val nativeCount = execute(
      TriangleMultisetFixture.nativeCount(impossiblePredicate)
    )
    val rewrittenCount = execute(
      TriangleMultisetFixture.supportFilteredCount(impossiblePredicate)
    )
    assertValueOracle(expectedCount(0L), nativeCount)
    assertEquivalent(nativeCount, rewrittenCount)
  }

  test("exact comparison detects multiplicity lost by DISTINCT final support") {
    val native = execute(TriangleMultisetFixture.nativeEnumeration())
    val badRewrite = execute(TriangleMultisetFixture.BadSupportAsFinalRelation)
    val difference = native.difference(badRewrite)

    assertEquals(
      native.totalMultiplicity,
      TriangleMultisetFixture.ExpectedEnumerationMultiplicity
    )
    assertEquals(
      badRewrite.totalMultiplicity,
      TriangleMultisetFixture.ExpectedBadSupportMultiplicity
    )
    assert(!difference.isEmpty, "the deliberately bad rewrite must not compare equal")
    assertEquals(difference.missingFromActual.values.sum, 15L)
  }

  test("exact comparison detects NULL keys admitted by NULL-safe equality") {
    val native = execute(TriangleMultisetFixture.nativeEnumeration())
    val badRewrite = execute(TriangleMultisetFixture.BadNullSafeJoin)
    val difference = native.difference(badRewrite)

    assertEquals(
      badRewrite.totalMultiplicity,
      TriangleMultisetFixture.ExpectedNullSafeMultiplicity
    )
    assert(!difference.isEmpty, "NULL-safe equality must change the result bag")
    assertEquals(difference.missingFromActual.values.sum, 0L)
    assertEquals(difference.unexpectedInActual.values.sum, 9L)
  }

  test("exact comparison includes result schema for empty results") {
    val emptyInteger = execute(
      "SELECT CAST(1 AS INTEGER) AS value WHERE FALSE"
    )
    val emptyText = execute(
      "SELECT CAST('one' AS TEXT) AS value WHERE FALSE"
    )
    val difference = emptyInteger.difference(emptyText)

    assert(emptyInteger.isEmpty)
    assert(emptyText.isEmpty)
    assert(!difference.schemaMatches)
    assert(!difference.isEmpty, "different empty result schemas must not compare equal")
  }

  test("exact comparison supports bytes and standard JDBC temporal values") {
    val sql =
      """SELECT
        |  decode('00ff', 'hex') AS bytes_value,
        |  DATE '2026-08-18' AS date_value,
        |  TIME '12:34:56.123456' AS time_value,
        |  TIMESTAMP '2026-08-18 12:34:56.123456' AS timestamp_value""".stripMargin

    val first = execute(sql)
    val second = execute(sql)
    assertEquals(first.totalMultiplicity, 1L)
    assertEquivalent(first, second)
  }

  test("exact comparison fails closed for unsupported JDBC value classes") {
    val failure = intercept[IllegalArgumentException] {
      execute("SELECT '{}'::JSONB AS unsupported_value")
    }

    assert(failure.getMessage.startsWith("Unsupported JDBC result value class:"))
  }

  private def execute(sql: String): ExactResultMultiset.ResultMultiset =
    ExactResultMultiset.execute(connection, sql)

  private def explainInConfiguredSchema(
      statements: Vector[String]
  ): Vector[(BigDecimal, BigDecimal)] = {
    val originalAutoCommit = connection.getAutoCommit
    require(originalAutoCommit, "integration connection must start in auto-commit")
    connection.setAutoCommit(false)
    try {
      val transaction = connection.createStatement()
      try
        transaction.execute(
          "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY"
        )
      finally transaction.close()

      val searchPath = connection.prepareStatement(
        "SELECT pg_catalog.set_config(" +
          "'search_path', 'pg_catalog, ' || pg_catalog.quote_ident(?) || ', pg_temp', true)"
      )
      try {
        searchPath.setString(1, jdbcCycleSchema)
        val result = searchPath.executeQuery()
        try {
          assert(result.next())
          assert(result.getString(1).startsWith("pg_catalog,"))
          assert(!result.next())
        } finally result.close()
      } finally searchPath.close()

      statements.map { sql =>
        val explain = connection.createStatement()
        try {
          val result = explain.executeQuery(
            "EXPLAIN (FORMAT JSON, ANALYZE FALSE) " + sql
          )
          try {
            assert(result.next())
            val document = ujson.read(result.getString(1))
            assert(!result.next())
            val root = document.arr.head("Plan")
            BigDecimal(java.lang.Double.toString(root("Total Cost").num)) ->
              BigDecimal(java.lang.Double.toString(root("Plan Rows").num))
          } finally result.close()
        } finally explain.close()
      }
    } finally {
      try connection.rollback()
      finally {
        connection.setAutoCommit(originalAutoCommit)
      }
    }
  }

  private def generated(
      sql: String,
      widthBound: Int = 2,
      decompositionOverride: Option[
        CandidateSearchInput => CoveredDecomposition
      ] = None
  ): PostgreSqlRenderedQuery = {
    val report = GeneratedRendererCalciteFixture.analyze(sql)
    assert(report.isSupported, clues(report.diagnostics))
    val hypergraph = report.hypergraph.fold(
      diagnostics => fail(diagnostics.map(_.message).mkString("; ")),
      identity
    )
    val generation = SoftBagGenerator
      .generate(hypergraph, widthBound)
      .fold(error => fail(error.message), identity)
    val input = CandidateSearchInput
      .create(
        hypergraph,
        widthBound,
        generation.admissible(requireConnectedCover = true)
      )
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
    val decomposition = decompositionOverride match {
      case Some(build) => build(input)
      case None =>
        TopKCandidateTD
          .enumerate(input, planLimit = 1)
          .fold(
            error => fail(error.message),
            {
              case found: CandidatePlanDecision.Found =>
                found.plans.head.decomposition
              case rejected: CandidatePlanDecision.Rejected =>
                fail(
                  s"triangle candidate search rejected: ${rejected.statistics}"
                )
            }
          )
    }
    val bagFactorPlan = BagFactorPlanner
      .plan(input, decomposition)
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
    PostgreSqlBagFactorRenderer
      .render(report.query, input, bagFactorPlan)
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
  }

  private def jdbcCycleProjection(
      projection: String,
      qualified: Boolean
  ): String = {
    require(
      projection != null && projection.nonEmpty,
      "projection must not be empty"
    )
    val source =
      if (qualified)
        JdbcCatalogCycleFixture.qualifiedEnumerationCore(jdbcCycleSchema)
      else JdbcCatalogCycleFixture.UnqualifiedEnumerationCore
    val fromOffset = source.indexOf("FROM ")
    require(fromOffset >= 0, "JDBC cycle query must contain FROM")
    s"SELECT $projection\n${source.substring(fromOffset)}"
  }

  /** Selects the actual retained rank-2 candidate, never a forged plan. */
  private def secondCandidateDecomposition(
      input: CandidateSearchInput
  ): CoveredDecomposition =
    TopKCandidateTD
      .enumerate(input, planLimit = 2)
      .fold(
        error => fail(error.message),
        {
          case found: CandidatePlanDecision.Found =>
            found.plans.lift(1).map(_.decomposition).getOrElse(
              fail(s"candidate search returned fewer than two plans: $found")
            )
          case rejected: CandidatePlanDecision.Rejected =>
            fail(s"candidate search rejected: ${rejected.statistics}")
        }
      )

  private def publicGenerated(
      sql: String
  ): (PostgreSqlRewriteArtifact, String) =
    publicGenerated(benchmarkRewriter, sql)

  private def publicGenerated(
      rewriter: PostgreSqlQueryRewriter,
      sql: String,
      reductionMode: YannakakisReductionMode = YannakakisReductionMode.Default
  ): (PostgreSqlRewriteArtifact, String) = {
    val detailed = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = 2,
          planLimit = 10,
          requireConnectedCover = true,
          reductionMode = reductionMode
        )
      )
    )
    val generatedSql = detailed.outcome match {
      case RewriteOutcome.Rewritten(originalSql, statements, _) =>
        assertEquals(originalSql, sql)
        assertEquals(statements.size, 1)
        statements.head
      case outcome =>
        fail(
          s"public rewrite did not succeed: ${outcome.getClass.getSimpleName}: " +
            outcome.diagnostics.map(diagnostic =>
              s"${diagnostic.code.id}: ${diagnostic.message}"
            ).mkString("; ")
        )
    }
    val artifact = detailed.report.artifact.getOrElse(
      fail(s"successful rewrite has no artifact: ${detailed.report}")
    )
    assertEquals(artifact.renderedQuery.sql, generatedSql)
    artifact -> generatedSql
  }

  private def expectedCount(value: Long): ExactResultMultiset.ResultMultiset =
    execute(s"SELECT CAST($value AS BIGINT) AS result_count")

  private def assertEquivalent(
      expected: ExactResultMultiset.ResultMultiset,
      actual: ExactResultMultiset.ResultMultiset
  ): Unit = {
    val difference = expected.difference(actual)
    assert(difference.isEmpty, difference.render)
  }

  private def assertValueOracle(
      expected: ExactResultMultiset.ResultMultiset,
      actual: ExactResultMultiset.ResultMultiset
  ): Unit = {
    val difference = expected.differenceIgnoringNullabilityAndOrigin(actual)
    assert(difference.isEmpty, difference.render)
  }

  private def count(value: String, fragment: String): Int = {
    var total = 0
    var offset = 0
    var next = value.indexOf(fragment, offset)
    while (next >= 0) {
      total += 1
      offset = next + fragment.length
      next = value.indexOf(fragment, offset)
    }
    total
  }
}
