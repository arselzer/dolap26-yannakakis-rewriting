package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.Path

final class PostgreSqlEnumerationSearchReplayBundleV2CliConfigSuite
    extends munit.FunSuite {
  private val workingDirectory = Path.of("/fixture/work").toAbsolutePath.normalize()
  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
      "jdbc:postgresql://fixture.invalid/db",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "fixture-user",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "fixture-secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "fixture_schema"
  )

  test("v2 keeps the closed bundle grammar and owns only its staging package path") {
    val config = PostgreSqlEnumerationSearchReplayBundleV2CliConfig
      .parse(
        Array(
          "acquire",
          "--authoring-request",
          "authoring.json",
          "--output-bundle",
          "bundle",
          "--python-executable",
          "/usr/bin/python3",
          "--composer-root",
          "/fixture/composer"
        ),
        environment,
        workingDirectory
      )
      .fold(problem => fail(problem.toString), identity)
    assertEquals(
      config.authoringRequest,
      workingDirectory.resolve("authoring.json")
    )
    assertEquals(config.outputBundle, workingDirectory.resolve("bundle"))
    val staged = workingDirectory.resolve("staging/package.json")
    assertEquals(config.backendConfig(staged).outputPackage, staged)
  }

  test("v2 rejects SQL/search overrides and malformed invocation") {
    Vector(
      Array.empty[String],
      Array("acquire", "--sql-file", "query.sql"),
      Array(
        "acquire",
        "--authoring-request",
        "authoring.json",
        "--output-bundle",
        "bundle",
        "--python-executable",
        "/usr/bin/python3",
        "--composer-root",
        "/fixture/composer",
        "--plan-limit",
        "1"
      )
    ).foreach { arguments =>
      assert(
        PostgreSqlEnumerationSearchReplayBundleV2CliConfig
          .parse(arguments, environment, workingDirectory)
          .isLeft
      )
    }
  }
}
