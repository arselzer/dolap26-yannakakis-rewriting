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
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCandidateSelectionDecision,
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteArtifact,
  PostgreSqlRewritePhase,
  PostgreSqlRewriteResult
}
import org.testcontainers.postgresql.PostgreSQLContainer

class PostgreSqlRetainedCandidateMultisetSuite extends munit.FunSuite {
  private final case class Database(
      jdbcUrl: String,
      username: String,
      password: String
  )

  private var postgres: PostgreSQLContainer = _
  private var connection: Connection = _
  private var rewriter: PostgreSqlQueryRewriter = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val database = externalDatabase.getOrElse {
      postgres = new PostgreSQLContainer("postgres:18.4-alpine")
        .withDatabaseName("retained_candidate_multiset")
        .withUsername("candidate")
        .withPassword("candidate")
      postgres.start()
      Database(postgres.getJdbcUrl, postgres.getUsername, postgres.getPassword)
    }
    connection = DriverManager.getConnection(
      database.jdbcUrl,
      database.username,
      database.password
    )
    BenchmarkDerivedMultisetFixture.install(connection)
    rewriter = PostgreSqlQueryRewriter
      .forSchema(BenchmarkDerivedCalciteFixture.rootSchema())
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

  test("every retained LSQB-Q2-derived alternative preserves the exact multiset") {
    assertEveryRetainedAlternative(
      BenchmarkDerivedMultisetFixture.LsqbQ2EnumerationCore,
      expectedMultiplicity = BenchmarkDerivedMultisetFixture.LsqbQ2ExpectedMultiplicity,
      expectedCandidateCount = Some(4),
      deadOccurrenceColumnAliases = Vector("yr_e0_c0")
    )
  }

  test("every retained Q5-derived alternative preserves the exact multiset") {
    assertEveryRetainedAlternative(
      BenchmarkDerivedMultisetFixture.TpchQ5DerivedEnumerationCore,
      expectedMultiplicity =
        BenchmarkDerivedMultisetFixture.TpchQ5DerivedExpectedMultiplicity,
      expectedCandidateCount = Some(10),
      deadOccurrenceColumnAliases = (0 to 5).map(edge => s"yr_e${edge}_c0").toVector
    )
  }

  test("LSQB-Q3-derived self-join is exact in both accepted width regimes") {
    val source = BenchmarkDerivedMultisetFixture.LsqbQ3DerivedEnumerationCore
    val native = ExactResultMultiset.execute(connection, source)
    assertEquals(
      native.totalMultiplicity,
      BenchmarkDerivedMultisetFixture.LsqbQ3DerivedExpectedMultiplicity
    )
    assert(native.containsDuplicate, "fixture must exercise duplicate multiplicity")
    assert(native.containsNull, "fixture must exercise NULL payloads")

    Vector(2, 3).foreach { rejectedWidth =>
      val rejected = attempt(
        source,
        CandidatePlanSelection.StructuralHead,
        widthBound = rejectedWidth,
        planLimit = 10,
        requireConnectedCover = true
      )
      assert(
        rejected.outcome.isInstanceOf[RewriteOutcome.NotApplicable],
        clues(rejectedWidth, rejected.outcome)
      )
      assertEquals(
        rejected.outcome.diagnostics.map(_.code),
        Vector(RewriteReasonCode.NoDecomposition),
        clues(rejectedWidth)
      )
      assertEquals(
        rejected.report.completedPhase,
        PostgreSqlRewritePhase.CandidateSearchCompleted,
        clues(rejectedWidth)
      )
      rejected.report.candidateDecision match {
        case Some(decision: CandidatePlanDecision.Rejected) =>
          assertEquals(decision.requestedLimit, 10, clues(rejectedWidth))
        case other =>
          fail(s"width $rejectedWidth expected typed rejection, received $other")
      }
      assertEquals(
        rejected.report.candidateSelectionDecision,
        None,
        clues(rejectedWidth)
      )
      assertEquals(rejected.report.bagFactorPlan, None, clues(rejectedWidth))
      assertEquals(rejected.report.artifact, None, clues(rejectedWidth))
    }

    val regimes = Vector(
      (2, false),
      (4, true)
    )
    regimes.foreach { case (widthBound, requireConnectedCover) =>
      val result = detailed(
        source,
        CandidatePlanSelection.StructuralHead,
        widthBound = widthBound,
        planLimit = 10,
        requireConnectedCover = requireConnectedCover
      )
      val selected = artifact(result)
      val actual = ExactResultMultiset.execute(connection, rewrittenStatement(result))
      val difference = native.difference(actual)
      val clue = clues(widthBound, requireConnectedCover, selected.selectedPlan)

      assertEquals(selected.query.relations.size, 9, clue)
      assertEquals(
        selected.query.relations.map(_.id.value),
        (0 until 9).toVector,
        clue
      )
      assertEquals(
        selected.query.relations.map(_.qualifiedName.last),
        Vector.fill(3)("lsqb_q3_city") ++
          Vector.fill(3)("lsqb_q3_person") ++
          Vector.fill(3)("lsqb_q3_knows"),
        clue
      )
      assertEquals(selected.hypergraph.variables.size, 7, clue)
      assertEquals(
        selected.hypergraph.edgeIds.map(_.value),
        (0 until 9).toVector,
        clue
      )
      assertEquals(
        selected.hypergraph.relationOccurrenceIds.map(_.value),
        (0 until 9).toVector,
        clue
      )
      assertEquals(
        selected.bagFactorPlan.ownership.map(_.edgeId).sorted,
        selected.hypergraph.edgeIds,
        clue
      )
      assertEquals(
        selected.renderedQuery.trace.bags
          .flatMap(_.occurrenceColumns)
          .map(_.edgeId)
          .distinct
          .sorted,
        selected.hypergraph.edgeIds,
        clue
      )
      assertEquals(selected.candidateSearch.plans.size, 10, clue)
      assert(selected.selectedPlan.decomposition.bags.size > 1, clue)
      assert(selected.bagFactorPlan.factors.size > 1, clue)
      assert(difference.isEmpty, s"$clue failed:\n${difference.render}")
    }
  }

  test("retained multi-bag LSQB-Q2-derived cycle is exact in every reduction mode") {
    val source = BenchmarkDerivedMultisetFixture.LsqbQ2EnumerationCore
    val native = ExactResultMultiset.execute(connection, source)
    assert(native.containsDuplicate)
    assert(native.containsNull)

    YannakakisReductionMode.Values.foreach { mode =>
      val result = detailed(
        source,
        CandidatePlanSelection.StructuralRank(2),
        mode
      )
      val selected = artifact(result)
      val rendered = selected.renderedQuery
      val actual = ExactResultMultiset.execute(connection, rewrittenStatement(result))
      val difference = native.difference(actual)

      assert(selected.bagFactorPlan.factors.size > 1, clues(mode, selected.selectedPlan))
      assert(selected.bagFactorPlan.schedule.bottomUp.nonEmpty, clues(mode))
      assert(selected.bagFactorPlan.schedule.topDown.nonEmpty, clues(mode))
      assert(difference.isEmpty, s"$mode failed:\n${difference.render}")
      assertEquals(rendered.trace.reduction.mode, mode)
      assertEquals(selected.candidateSelection.reductionMode, mode)
      mode match {
        case YannakakisReductionMode.FullTwoPass =>
          assertEquals(
            rendered.trace.semijoins.size,
            selected.bagFactorPlan.schedule.bottomUp.size +
              selected.bagFactorPlan.schedule.topDown.size
          )
        case YannakakisReductionMode.BottomUpOnly =>
          assertEquals(
            rendered.trace.semijoins.size,
            selected.bagFactorPlan.schedule.bottomUp.size
          )
          assert(rendered.trace.semijoins.forall(
            _.pass == at.ac.tuwien.dbai.yannakakis.algorithm.lowering.SemijoinPass.BottomUp
          ))
        case YannakakisReductionMode.NoReduction =>
          assertEquals(rendered.trace.semijoins, Vector.empty)
          assertEquals(rendered.trace.reduction.physicalReductionCteCount, 0)
      }
    }
  }

  private def assertEveryRetainedAlternative(
      sourceSql: String,
      expectedMultiplicity: Long,
      expectedCandidateCount: Option[Int],
      deadOccurrenceColumnAliases: Vector[String]
  ): Unit = {
    val native = ExactResultMultiset.execute(connection, sourceSql)
    assertEquals(native.totalMultiplicity, expectedMultiplicity)
    assert(native.containsDuplicate, "fixture must exercise duplicate multiplicity")
    assert(native.containsNull, "fixture must exercise NULL payloads")

    val baseline = detailed(
      sourceSql,
      CandidatePlanSelection.StructuralHead
    )
    val baselineArtifact = artifact(baseline)
    val plans = baselineArtifact.candidateSearch.plans
    expectedCandidateCount match {
      case Some(expected) => assertEquals(plans.size, expected)
      case None           => assert(plans.size > 1, clues(plans.map(_.rank)))
    }
    assertEquals(plans.map(_.rank), (1 to plans.size).toVector)
    assertEquals(plans.map(_.candidateId).distinct.size, plans.size)

    plans.foreach { expectedPlan =>
      val byRank = detailed(
        sourceSql,
        CandidatePlanSelection.StructuralRank(expectedPlan.rank)
      )
      val byId = detailed(
        sourceSql,
        CandidatePlanSelection.ExactCandidateId(expectedPlan.candidateId)
      )
      val rankArtifact = artifact(byRank)
      val idArtifact = artifact(byId)

      assertEquals(rankArtifact.selectedPlan, expectedPlan)
      assertEquals(idArtifact.selectedPlan, expectedPlan)
      assertEquals(
        rankArtifact.candidateSelection.evaluations.map(_.structuralRank),
        Vector(expectedPlan.rank)
      )
      assertEquals(
        idArtifact.candidateSelection.evaluations.map(_.candidateId),
        Vector(expectedPlan.candidateId)
      )
      val rankStatement = rewrittenStatement(byRank)
      val idStatement = rewrittenStatement(byId)
      assertEquals(
        rankStatement,
        idStatement,
        s"rank and id disagree for retained rank ${expectedPlan.rank}"
      )
      assertGeneratedAliasesAbsent(rankStatement, deadOccurrenceColumnAliases)
      assertGeneratedAliasesAbsent(idStatement, deadOccurrenceColumnAliases)

      val actual = ExactResultMultiset.execute(
        connection,
        rankStatement
      )
      val difference = native.difference(actual)
      assert(
        difference.isEmpty,
        s"retained rank ${expectedPlan.rank} (${expectedPlan.candidateId}) failed:\n${difference.render}"
      )
    }

    val static = detailed(
      sourceSql,
      CandidatePlanSelection.PostgreSqlStaticShapeV1
    )
    val staticArtifact = artifact(static)
    assertEquals(
      staticArtifact.candidateSelection.evaluations.map(_.structuralRank),
      plans.map(_.rank)
    )
    assert(
      plans.exists(_.candidateId == staticArtifact.selectedPlan.candidateId),
      clues(staticArtifact.selectedPlan)
    )
    val staticStatement = rewrittenStatement(static)
    assertGeneratedAliasesAbsent(staticStatement, deadOccurrenceColumnAliases)
    val staticDifference = native.difference(
      ExactResultMultiset.execute(connection, staticStatement)
    )
    assert(staticDifference.isEmpty, staticDifference.render)
  }

  private def assertGeneratedAliasesAbsent(
      sql: String,
      aliases: Vector[String]
  ): Unit = aliases.foreach { alias =>
    val quotedAlias = "\"" + alias + "\""
    assert(!sql.contains(quotedAlias), clues(alias, sql))
  }

  private def detailed(
      sql: String,
      selection: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode = YannakakisReductionMode.Default,
      widthBound: Int = 2,
      planLimit: Int = 10,
      requireConnectedCover: Boolean = true
  ): PostgreSqlRewriteResult = {
    val result = attempt(
      sql,
      selection,
      reductionMode,
      widthBound,
      planLimit,
      requireConnectedCover
    )
    result.outcome match {
      case _: RewriteOutcome.Rewritten => result
      case other =>
        fail(
          s"selection ${selection.id} did not rewrite: " +
            other.diagnostics.map(diagnostic =>
              s"${diagnostic.code.id}: ${diagnostic.message}"
            ).mkString("; ")
        )
    }
  }

  private def attempt(
      sql: String,
      selection: CandidatePlanSelection,
      reductionMode: YannakakisReductionMode = YannakakisReductionMode.Default,
      widthBound: Int = 2,
      planLimit: Int = 10,
      requireConnectedCover: Boolean = true
  ): PostgreSqlRewriteResult =
    rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = widthBound,
          planLimit = planLimit,
          requireConnectedCover = requireConnectedCover,
          candidateSelection = selection,
          reductionMode = reductionMode
        )
      )
    )

  private def artifact(
      result: PostgreSqlRewriteResult
  ): PostgreSqlRewriteArtifact = {
    val artifact = result.report.artifact.getOrElse(
      fail(s"successful rewrite has no artifact: ${result.report}")
    )
    artifact.candidateSelection match {
      case _: PostgreSqlCandidateSelectionDecision.Selected => artifact
    }
  }

  private def rewrittenStatement(result: PostgreSqlRewriteResult): String =
    result.outcome match {
      case value: RewriteOutcome.Rewritten =>
        value.rewrittenStatements match {
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
        s"External PostgreSQL test configuration is incomplete; missing: " +
          names.filterNot(sys.env.contains).mkString(", ")
      )
    } else {
      if (sys.env(urlName).trim.isEmpty || sys.env(userName).trim.isEmpty) {
        throw new IllegalArgumentException(
          "External PostgreSQL URL and user must not be blank"
        )
      }
      Some(Database(sys.env(urlName), sys.env(userName), sys.env(passwordName)))
    }
  }
}
