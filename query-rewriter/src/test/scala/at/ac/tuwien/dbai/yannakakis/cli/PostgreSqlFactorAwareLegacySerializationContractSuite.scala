package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteRequest
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

class PostgreSqlFactorAwareLegacySerializationContractSuite
    extends munit.FunSuite {
  private val schemaDirectory = "docs/journal-extension/schemas"

  test("factor-aware v5 leaves every published v1-v4 schema byte frozen") {
    val expected = Vector(
      1 -> "cb490fd238d0a6044e5fe03f403a9a1e5257adddd76cee9635f53163662d3283",
      2 -> "b0bbff746dc349182f3fd55ae8d87acaf6bd1157c736aea89b2eabbb837f2adb",
      3 -> "c87261872c356391365c92955cf43480074fd5a4a77c9309e10ad84bcf21fdd0",
      4 -> "8bb50a5d0458797695207972d35b08c171006b8e0ce6e763f5fbdb3f439d0f8b"
    )

    expected.foreach { case (version, digest) =>
      val path = repositoryFile(
        s"$schemaDirectory/postgresql-rewrite-artifact-v$version.schema.json"
      )
      val bytes = Files.readAllBytes(path)
      assertEquals(sha256(bytes), digest, clues(version))
      assertEquals(
        ujson.read(new String(bytes, StandardCharsets.UTF_8))("properties")(
          "schema_version"
        )("const").str,
        s"yannakakis-postgresql-rewrite/v$version"
      )
    }
  }

  test("only the opt-in factor-aware request selects v5 serialization") {
    val rewriter = standardRewriter()
    val blankSql = " \n\t "
    val candidateId = CandidatePlanId
      .parse("sha256:" + "0" * 64)
      .fold(problem => fail(problem.message), identity)
    val legacyPolicies = Vector[CandidatePlanSelection](
      CandidatePlanSelection.StructuralHead,
      CandidatePlanSelection.StructuralRank(1),
      CandidatePlanSelection.ExactCandidateId(candidateId),
      CandidatePlanSelection.PostgreSqlStaticShapeV1,
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1
    )

    legacyPolicies.foreach { selection =>
      val request = RewriteRequest(
        blankSql,
        options = RewriteOptions(
          planLimit = 10,
          candidateSelection = selection
        )
      )
      val rendered = PostgreSqlRewriteJson.render(
        request,
        rewriter.rewriteDetailed(request)
      )
      assertEquals(
        ujson.read(rendered)("schema_version").str,
        PostgreSqlRewriteJson.SchemaVersion,
        clues(selection.id)
      )
    }

    val factorRequest = RewriteRequest(
      blankSql,
      options = RewriteOptions(
        planLimit = 100,
        candidateSelection =
          CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
      )
    )
    val factorRendered = PostgreSqlRewriteJson.render(
      factorRequest,
      rewriter.rewriteDetailed(factorRequest)
    )
    assertEquals(
      ujson.read(factorRendered)("schema_version").str,
      PostgreSqlRewriteJson.FactorAwareSchemaVersion
    )

    val invocationFailure = PostgreSqlRewriteJson.renderFailure(
      PostgreSqlRewriteCliFailure.invocation("safe failure")
    )
    assertEquals(
      ujson.read(invocationFailure)("schema_version").str,
      PostgreSqlRewriteJson.SchemaVersion
    )
  }

  test("CLI accepts only the frozen factor-aware spelling with exact plan limit 100") {
    val environment = Map(
      PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
        "jdbc:postgresql://localhost/database",
      PostgreSqlRewriteCliConfig.UserEnvironment -> "user",
      PostgreSqlRewriteCliConfig.PasswordEnvironment -> "password",
      PostgreSqlRewriteCliConfig.SchemaEnvironment -> "public"
    )
    val parsed = PostgreSqlRewriteCliConfig.parse(
      Array(
        "rewrite",
        "--stdin",
        "--selection",
        "postgresql-explain-factor-aware-progressive-v1",
        "--plan-limit",
        "100",
        "--explain-timeout-seconds",
        "7"
      ),
      environment
    )
    parsed match {
      case Right(PostgreSqlRewriteCliCommand.Rewrite(config)) =>
        assertEquals(
          config.options.candidateSelection,
          CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
        )
        assertEquals(config.options.planLimit, 100)
        assertEquals(config.explainLimits.statementTimeoutSeconds, 7)
        assertEquals(config.factorAwareLimits.statementTimeoutSeconds, 7)
        assertEquals(config.factorAwareLimits.explainStatementBudgetSeconds, 600)
      case other => fail(s"factor-aware CLI did not parse: $other")
    }

    val defaultLimit = PostgreSqlRewriteCliConfig.parse(
      Array(
        "rewrite",
        "--stdin",
        "--selection",
        "postgresql-explain-factor-aware-progressive-v1"
      ),
      environment
    )
    assert(defaultLimit.isLeft)

    val internalId = PostgreSqlRewriteCliConfig.parse(
      Array(
        "rewrite",
        "--stdin",
        "--selection",
        "postgresql_explain_factor_aware_progressive_v1",
        "--plan-limit",
        "100"
      ),
      environment
    )
    assert(internalId.isLeft)
    assert(
      PostgreSqlRewriteCliConfig.Usage.contains(
        "postgresql-explain-factor-aware-progressive-v1"
      )
    )
  }

  private def standardRewriter(): PostgreSqlQueryRewriter = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
  }

  private def repositoryFile(relative: String): Path = {
    var current = Paths.get("").toAbsolutePath.normalize()
    while (current != null) {
      val candidate = current.resolve(relative)
      if (Files.isRegularFile(candidate)) return candidate
      current = current.getParent
    }
    fail(s"could not locate repository file $relative")
  }

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
