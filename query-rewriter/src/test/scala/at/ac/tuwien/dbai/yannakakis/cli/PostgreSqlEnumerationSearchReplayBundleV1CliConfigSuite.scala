package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.Paths

import munit.FunSuite

final class PostgreSqlEnumerationSearchReplayBundleV1CliConfigSuite
    extends FunSuite {
  private val cwd = Paths.get("/tmp/yrw-search-replay-bundle-config").normalize()
  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment -> "jdbc:postgresql://db/test",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "user",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "fixture"
  )
  private val validArguments = Array(
    "acquire",
    "--authoring-request",
    "request.json",
    "--output-bundle",
    "out/replay-bundle",
    "--python-executable",
    "/usr/bin/python3",
    "--composer-root",
    "/opt/yrw/enumeration-campaign-v2"
  )

  test("bundle invocation derives normalized paths and a backend-only view") {
    val parsed = PostgreSqlEnumerationSearchReplayBundleV1CliConfig
      .parse(validArguments, environment, cwd)
      .fold(problem => fail(problem.toString), identity)
    assertEquals(
      parsed.authoringRequest,
      Paths.get("/tmp/yrw-search-replay-bundle-config/request.json")
    )
    assertEquals(
      parsed.outputBundle,
      Paths.get("/tmp/yrw-search-replay-bundle-config/out/replay-bundle")
    )
    val staged = Paths.get("/tmp/yrw-staging/package.json")
    assertEquals(parsed.backendConfig(staged).outputPackage, staged)
    assertEquals(parsed.schemaName, "fixture")
    assertEquals(parsed.jdbc.password, "secret")
  }

  test("only the dedicated output-bundle surface is admitted") {
    Vector(
      "--output-package",
      "--sql-file",
      "--plan-limit",
      "--width-bound",
      "--connected-cover"
    ).foreach { option =>
      val problem = PostgreSqlEnumerationSearchReplayBundleV1CliConfig
        .parse(validArguments ++ Array(option, "x"), environment, cwd)
        .left
        .toOption
        .getOrElse(fail(s"unexpectedly accepted $option"))
      assertEquals(
        problem,
        PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidArguments
      )
    }
  }

  test("required options are exact, unique, and path-distinct") {
    val cases = Vector(
      validArguments.dropRight(2),
      validArguments ++ Array("--output-bundle", "other"),
      validArguments.updated(4, "request.json"),
      validArguments.updated(2, "../request.json"),
      validArguments.updated(4, "out/../bundle"),
      validArguments.updated(6, "python3"),
      Array("bundle")
    )
    cases.foreach { arguments =>
      assert(
        PostgreSqlEnumerationSearchReplayBundleV1CliConfig
          .parse(arguments, environment, cwd)
          .isLeft
      )
    }
  }

  test("schema override and environment-only JDBC rules match acquisition") {
    val parsed = PostgreSqlEnumerationSearchReplayBundleV1CliConfig
      .parse(validArguments ++ Array("--schema", "campaign"), environment, cwd)
      .fold(problem => fail(problem.toString), identity)
    assertEquals(parsed.schemaName, "campaign")
    assertEquals(
      PostgreSqlEnumerationSearchReplayBundleV1CliConfig
        .parse(
          validArguments,
          environment - PostgreSqlRewriteCliConfig.PasswordEnvironment,
          cwd
        )
        .left
        .toOption
        .get,
      PostgreSqlEnumerationCampaignV2CliConfigFailure.MissingConfiguration
    )
  }
}
