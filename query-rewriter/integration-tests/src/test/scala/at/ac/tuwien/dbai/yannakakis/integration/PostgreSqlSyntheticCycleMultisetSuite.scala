package at.ac.tuwien.dbai.yannakakis.integration

import at.ac.tuwien.dbai.yannakakis.integration.{
  ExactResultMultisetV2 => ExactResultMultiset
}
import java.sql.{Connection, DriverManager}

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteReasonCode,
  RewriteRequest
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCandidateSelectionDecision,
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteArtifact,
  PostgreSqlRewritePhase,
  PostgreSqlRewriteResult
}
import org.testcontainers.postgresql.PostgreSQLContainer

private object PostgreSqlSyntheticCycleMultisetSuite {
  final case class Database(
      jdbcUrl: String,
      username: String,
      password: String
  )
}

/** Exact bag-semantics gates for the deterministic sparse/dense corpus. */
class PostgreSqlSyntheticCycleMultisetSuite extends munit.FunSuite {
  import PostgreSqlSyntheticCycleMultisetSuite.Database
  import SyntheticCycleMultisetFixture.{
    Dataset,
    DenseBowTie,
    DenseC4,
    DenseC6,
    SparseBowTie,
    SparseC4,
    SparseC6
  }

  private var postgres: PostgreSQLContainer = _
  private var connection: Connection = _
  private var rewriter: PostgreSqlQueryRewriter = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val database = externalDatabase.getOrElse {
      postgres = new PostgreSQLContainer("postgres:18.4-alpine")
        .withDatabaseName("synthetic_cycle_multiset")
        .withUsername("synthetic_cycle")
        .withPassword("synthetic_cycle")
      postgres.start()
      Database(postgres.getJdbcUrl, postgres.getUsername, postgres.getPassword)
    }
    connection = DriverManager.getConnection(
      database.jdbcUrl,
      database.username,
      database.password
    )
    SyntheticCycleMultisetFixture.install(connection)
    rewriter = PostgreSqlQueryRewriter
      .forSchema(SyntheticCycleCalciteFixture.rootSchema())
      .fold(
        problem =>
          throw new IllegalStateException(s"${problem.code}: ${problem.message}"),
        identity
      )
  }

  override def afterAll(): Unit =
    try if (connection != null) connection.close()
    finally {
      try if (postgres != null) postgres.stop()
      finally super.afterAll()
    }

  test("correctness relations pin duplicate, NULL-key, and nullable-payload data") {
    SyntheticCycleMultisetFixture.All.foreach { dataset =>
      (0 until dataset.topology.edgeCount).foreach { edgeIndex =>
        val rows = ExactResultMultiset.execute(
          connection,
          s"SELECT * FROM \"${SyntheticCycleMultisetFixture.tableName(dataset, edgeIndex)}\""
        )
        assertEquals(
          rows.totalMultiplicity,
          dataset.expectedRowsPerRelation,
          clues(dataset.id, edgeIndex)
        )
        assert(rows.containsDuplicate, clues(dataset.id, edgeIndex))
        assert(rows.containsNull, clues(dataset.id, edgeIndex))
        val nullLeft = ExactResultMultiset.execute(
          connection,
          s"SELECT * FROM \"${SyntheticCycleMultisetFixture.tableName(dataset, edgeIndex)}\" WHERE left_key IS NULL"
        )
        val nullRight = ExactResultMultiset.execute(
          connection,
          s"SELECT * FROM \"${SyntheticCycleMultisetFixture.tableName(dataset, edgeIndex)}\" WHERE right_key IS NULL"
        )
        assert(!nullLeft.isEmpty, clues(dataset.id, edgeIndex))
        assert(!nullRight.isEmpty, clues(dataset.id, edgeIndex))
      }
    }
  }

  test("sparse and dense C4 retain exact multi-bag width-two connected plans") {
    Vector(SparseC4, DenseC4).foreach { dataset =>
      assertRejected(dataset, widthBound = 1, requireConnectedCover = true)
      assertEveryRetainedAlternative(
        dataset,
        widthBound = 2,
        requireConnectedCover = true
      )
    }
  }

  test("sparse and dense C6 pin the connected-cover width boundary exactly") {
    Vector(SparseC6, DenseC6).foreach { dataset =>
      assertRejected(dataset, widthBound = 2, requireConnectedCover = true)
      assertEveryRetainedAlternative(
        dataset,
        widthBound = 2,
        requireConnectedCover = false
      )
      assertEveryRetainedAlternative(
        dataset,
        widthBound = 3,
        requireConnectedCover = true
      )
    }
  }

  test("sparse and dense bow-ties retain exact articulation-aware plans") {
    Vector(SparseBowTie, DenseBowTie).foreach { dataset =>
      assertRejected(dataset, widthBound = 1, requireConnectedCover = true)
      assertEveryRetainedAlternative(
        dataset,
        widthBound = 2,
        requireConnectedCover = true
      )
    }
  }

  private def assertEveryRetainedAlternative(
      dataset: Dataset,
      widthBound: Int,
      requireConnectedCover: Boolean
  ): Unit = {
    val source = SyntheticCycleMultisetFixture.enumerationCore(dataset)
    val native = ExactResultMultiset.execute(connection, source)
    assertEquals(
      native.totalMultiplicity,
      dataset.expectedWitnessMultiplicity,
      clues(dataset.id, widthBound, requireConnectedCover)
    )
    assertEquals(
      native.distinctRowCount,
      dataset.expectedDistinctWitnesses,
      clues(dataset.id, widthBound, requireConnectedCover)
    )
    assert(native.containsDuplicate, clues(dataset.id))
    assert(native.containsNull, clues(dataset.id))

    val baseline = detailed(
      source,
      CandidatePlanSelection.StructuralHead,
      widthBound,
      requireConnectedCover
    )
    val baselineArtifact = artifact(baseline)
    val plans = baselineArtifact.candidateSearch.plans
    assert(plans.size > 1, clues(dataset.id, plans.map(_.rank)))
    assertEquals(plans.map(_.rank), (1 to plans.size).toVector)
    assertEquals(plans.map(_.candidateId).distinct.size, plans.size)
    assertEquals(baselineArtifact.query.relations.size, dataset.topology.edgeCount)
    assertEquals(baselineArtifact.hypergraph.variables.size, dataset.topology.vertexCount)

    plans.foreach { expectedPlan =>
      assert(
        expectedPlan.decomposition.bags.size > 1,
        clues(dataset.id, widthBound, requireConnectedCover, expectedPlan)
      )
      val selectedResult = detailed(
        source,
        CandidatePlanSelection.StructuralRank(expectedPlan.rank),
        widthBound,
        requireConnectedCover
      )
      val selectedArtifact = artifact(selectedResult)
      assertEquals(selectedArtifact.selectedPlan, expectedPlan)
      assert(
        selectedArtifact.bagFactorPlan.factors.size > 1,
        clues(dataset.id, expectedPlan.rank)
      )

      val actual = ExactResultMultiset.execute(
        connection,
        rewrittenStatement(selectedResult)
      )
      val difference = native.difference(actual)
      assert(
        difference.isEmpty,
        s"${dataset.id} rank ${expectedPlan.rank} failed:\n${difference.render}"
      )
    }
  }

  private def assertRejected(
      dataset: Dataset,
      widthBound: Int,
      requireConnectedCover: Boolean
  ): Unit = {
    val result = attempt(
      SyntheticCycleMultisetFixture.enumerationCore(dataset),
      CandidatePlanSelection.StructuralHead,
      widthBound,
      requireConnectedCover
    )
    assert(
      result.outcome.isInstanceOf[RewriteOutcome.NotApplicable],
      clues(dataset.id, widthBound, requireConnectedCover, result.outcome)
    )
    assertEquals(
      result.outcome.diagnostics.map(_.code),
      Vector(RewriteReasonCode.NoDecomposition),
      clues(dataset.id, widthBound, requireConnectedCover)
    )
    assertEquals(
      result.report.completedPhase,
      PostgreSqlRewritePhase.CandidateSearchCompleted
    )
    result.report.candidateDecision match {
      case Some(_: CandidatePlanDecision.Rejected) => ()
      case other =>
        fail(
          s"${dataset.id} width $widthBound connected=$requireConnectedCover " +
            s"expected typed rejection, received $other"
        )
    }
    assertEquals(result.report.artifact, None)
  }

  private def detailed(
      sql: String,
      selection: CandidatePlanSelection,
      widthBound: Int,
      requireConnectedCover: Boolean
  ): PostgreSqlRewriteResult = {
    val result = attempt(
      sql,
      selection,
      widthBound,
      requireConnectedCover
    )
    result.outcome match {
      case _: RewriteOutcome.Rewritten => result
      case other =>
        fail(
          s"selection ${selection.id} did not rewrite: " +
            other.diagnostics
              .map(diagnostic =>
                s"${diagnostic.code.id}: ${diagnostic.message}"
              )
              .mkString("; ")
        )
    }
  }

  private def attempt(
      sql: String,
      selection: CandidatePlanSelection,
      widthBound: Int,
      requireConnectedCover: Boolean
  ): PostgreSqlRewriteResult =
    rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = widthBound,
          planLimit = 10,
          requireConnectedCover = requireConnectedCover,
          candidateSelection = selection
        )
      )
    )

  private def artifact(
      result: PostgreSqlRewriteResult
  ): PostgreSqlRewriteArtifact = {
    val value = result.report.artifact.getOrElse(
      fail(s"successful rewrite has no artifact: ${result.report}")
    )
    value.candidateSelection match {
      case _: PostgreSqlCandidateSelectionDecision.Selected => value
    }
  }

  private def rewrittenStatement(result: PostgreSqlRewriteResult): String =
    result.outcome match {
      case rewritten: RewriteOutcome.Rewritten =>
        rewritten.rewrittenStatements match {
          case Vector(statement) => statement
          case other => fail(s"expected one rewritten statement, received $other")
        }
      case other => fail(s"expected rewritten outcome, received $other")
    }

  private def externalDatabase: Option[Database] = {
    val urlName = "YANNAKAKIS_TEST_JDBC_URL"
    val userName = "YANNAKAKIS_TEST_DB_USER"
    val passwordName = "YANNAKAKIS_TEST_DB_PASSWORD"
    val names = Vector(urlName, userName, passwordName)
    val present = names.filter(sys.env.contains)

    if (present.isEmpty) None
    else if (present.size != names.size) {
      throw new IllegalArgumentException(
        "External PostgreSQL test configuration is incomplete; missing: " +
          names.filterNot(sys.env.contains).mkString(", ")
      )
    } else {
      if (sys.env(urlName).trim.isEmpty || sys.env(userName).trim.isEmpty)
        throw new IllegalArgumentException(
          "External PostgreSQL URL and user must not be blank"
        )
      Some(Database(sys.env(urlName), sys.env(userName), sys.env(passwordName)))
    }
  }
}
