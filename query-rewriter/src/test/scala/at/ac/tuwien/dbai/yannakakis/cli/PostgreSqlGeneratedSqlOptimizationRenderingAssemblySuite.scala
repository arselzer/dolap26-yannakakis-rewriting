package at.ac.tuwien.dbai.yannakakis.cli

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import munit.FunSuite

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlAggregateRenderingProfile
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlGeneratedSqlOptimizationActionId,
  PostgreSqlPhysicalVariantId
}

final class PostgreSqlGeneratedSqlOptimizationRenderingAssemblySuite
    extends FunSuite {
  private val winSource =
    """SELECT COUNT(*) AS cycle_rows
      |FROM win_r AS r
      |JOIN win_s AS s ON r.x = s.x
      |JOIN win_t AS t ON s.y = t.y
      |JOIN win_u AS u ON t.z = u.z AND u.w = r.w;
      |""".stripMargin

  test("pinned win source retains native bytes and uses one explicit planning transform") {
    val source = PostgreSqlGeneratedSqlOptimizationSource
      .fromBytes(winSource.getBytes(StandardCharsets.UTF_8))
      .toOption
      .get
    assertEquals(source.source.byteLength, 148)
    assertEquals(
      source.source.sha256,
      "f8c5152ab789e946566fbf48a4ecdc4763867b7e6e9398a4e2f8713631963ed1"
    )
    assertEquals(
      source.planningTransform,
      "remove-one-terminal-semicolon-before-final-lf-v1"
    )
    assertEquals(source.planning.byteLength, 147)
    assertEquals(
      source.planning.sha256,
      "32b91fcb7d64e79a10a2b007d101ccf227ff28b5b90e160ad2bb59215f24e289"
    )
    assertEquals(source.planning.text, winSource.dropRight(2) + "\n")
    assertEquals(source.source.text, winSource)
  }

  test("planning transform rejects every other terminal-semicolon layout") {
    assert(
      PostgreSqlGeneratedSqlOptimizationSource
        .fromBytes("SELECT 1; \n".getBytes(StandardCharsets.UTF_8))
        .isLeft
    )
    val eof = PostgreSqlGeneratedSqlOptimizationSource
      .fromBytes("SELECT 1;".getBytes(StandardCharsets.UTF_8))
      .toOption
      .get
    assertEquals(eof.planning.text, "SELECT 1")
    assertEquals(
      eof.planningTransform,
      "remove-one-terminal-semicolon-at-eof-v1"
    )
    val identity = PostgreSqlGeneratedSqlOptimizationSource
      .fromBytes("SELECT 1\n".getBytes(StandardCharsets.UTF_8))
      .toOption
      .get
    assertEquals(identity.planningTransform, "identity-v1")
    assertEquals(identity.planning.text, identity.source.text)
  }

  test("one backend call publishes canonical private create-only assembly") {
    val directory = Files.createTempDirectory("yrw-generated-sql-rendering-")
    val query = directory.resolve("query.sql")
    val output = directory.resolve("assembly.json")
    Files.write(query, "SELECT 1\n".getBytes(StandardCharsets.UTF_8))
    val backend = new FixtureBackend()
    val errors = new ByteArrayOutputStream()
    val status = PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCli.run(
      arguments(query, output),
      environment,
      directory,
      errors,
      backend
    )
    assertEquals(status, 0)
    assertEquals(backend.calls, 1)
    assertEquals(errors.size(), 0)
    assertEquals(
      Files.getPosixFilePermissions(output),
      PosixFilePermissions.fromString("rw-------")
    )
    val json = Files.readString(output, StandardCharsets.UTF_8)
    assert(json.endsWith("\n"))
    assertEquals(json.count(_ == '\n'), 1)
    assert(json.contains(
      "\"schema_version\":\"yrw-generated-sql-optimization-rendering-assembly/v1\""
    ))
    assert(json.contains("\"action_alias\":\"native\""))
    assert(json.contains("\"action_alias\":\"profile_01\""))
    assert(json.contains("\"planning_sql_transform\":\"identity-v1\""))
    assert(json.contains("\"factory_mode\":\"ordinary\""))
    assert(json.contains("\"sql_utf8_hex\":\"53454c45435420310a\""))
    assert(!json.contains("fixture-secret"))
    assert(!json.contains("fixture_schema"))
    assert(!json.contains(query.toString))

    val second = new FixtureBackend()
    val secondErrors = new ByteArrayOutputStream()
    val secondStatus =
      PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCli.run(
        arguments(query, output),
        environment,
        directory,
        secondErrors,
        second
      )
    assertEquals(secondStatus, PostgreSqlRewriteExitCode.InputError)
    assertEquals(second.calls, 0)
    val error = secondErrors.toString(StandardCharsets.UTF_8)
    assertEquals(
      error,
      "generated-sql-rendering-cli error: acquisition-files.output-exists\n"
    )
    assert(!error.contains("fixture-secret"))
    assert(!error.contains(query.toString))
  }

  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
      "jdbc:postgresql://127.0.0.1/fixture",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "fixture-user",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "fixture-secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "fixture_schema"
  )

  private def arguments(query: Path, output: Path): Array[String] = Array(
    "--query-file",
    query.toString,
    "--output-assembly",
    output.toString,
    "--query-id",
    "fixture-query",
    "--family",
    "global-count-star",
    "--cell-id",
    "fixture-cell",
    "--dataset-id",
    "fixture-data",
    "--candidate-rank",
    "1",
    "--profile-ordinals",
    "1",
    "--plan-shape",
    "multi-bag"
  )

  private final class FixtureBackend
      extends PostgreSqlGeneratedSqlOptimizationRenderingBackend {
    var calls = 0

    override def produce(
        config: PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig,
        source: PostgreSqlGeneratedSqlOptimizationSource
    ): Either[
      PostgreSqlGeneratedSqlOptimizationBackendFailure,
      PostgreSqlGeneratedSqlOptimizationRenderingAssembly
    ] = {
      calls += 1
      val candidateId = CandidatePlanId
        .parse("sha256:" + "1" * 64)
        .toOption
        .get
      val base = PostgreSqlPhysicalVariantId.from(
        candidateId,
        YannakakisReductionMode.FullTwoPass
      )
      val profile = PostgreSqlAggregateRenderingProfile.LegacyV1
      val generatedSql = PostgreSqlGeneratedSqlOptimizationSource
        .generatedBinding("SELECT 1\n")
        .toOption
        .get
      val native = PostgreSqlGeneratedSqlOptimizationAction(
        "native",
        "native",
        None,
        "native",
        None,
        None,
        None,
        None,
        None,
        source.source
      )
      val generated = PostgreSqlGeneratedSqlOptimizationAction(
        "profile_01",
        PostgreSqlGeneratedSqlOptimizationActionId
          .from(candidateId, YannakakisReductionMode.FullTwoPass, profile)
          .value,
        Some(base.value),
        "generated",
        Some(profile.oneBagTerminal.id),
        Some(profile.id),
        Some(1),
        Some(profile.reductionFinalization.id),
        Some(profile.reductionSupport.id),
        generatedSql
      )
      Right(
        PostgreSqlGeneratedSqlOptimizationRenderingAssembly(
          config.queryId,
          config.family,
          config.cellId,
          config.datasetId,
          source,
          candidateId.value,
          config.candidateRank,
          10,
          base.value,
          config.planShape,
          Vector(native, generated)
        )
      )
    }
  }
}
