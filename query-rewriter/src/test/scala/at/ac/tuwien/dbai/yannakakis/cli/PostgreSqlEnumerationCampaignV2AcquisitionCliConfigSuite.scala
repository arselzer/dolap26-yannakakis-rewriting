package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.Paths

import munit.FunSuite

final class PostgreSqlEnumerationCampaignV2AcquisitionCliConfigSuite
    extends FunSuite {
  private val cwd = Paths.get("/tmp/yrw-campaign-v2-config").normalize()
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
    "--output-package",
    "out/package.json",
    "--python-executable",
    "/usr/bin/python3",
    "--composer-root",
    "/opt/yrw/enumeration-campaign-v2"
  )

  test("the command derives normalized request and output paths") {
    val parsed = PostgreSqlEnumerationCampaignV2CliConfig
      .parse(validArguments, environment, cwd)
      .toOption
      .get
    assertEquals(
      parsed.authoringRequest,
      Paths.get("/tmp/yrw-campaign-v2-config/request.json")
    )
    assertEquals(
      parsed.outputPackage,
      Paths.get("/tmp/yrw-campaign-v2-config/out/package.json")
    )
    assertEquals(parsed.schemaName, "fixture")
    assertEquals(parsed.jdbc.password, "secret")
  }

  test("the schema flag overrides only the schema environment") {
    val parsed = PostgreSqlEnumerationCampaignV2CliConfig
      .parse(validArguments ++ Array("--schema", "campaign"), environment, cwd)
      .toOption
      .get
    assertEquals(parsed.schemaName, "campaign")
  }

  test("SQL and search settings are not CLI options") {
    Vector("--sql-file", "--plan-limit", "--width-bound", "--connected-cover")
      .foreach { option =>
        val problem = PostgreSqlEnumerationCampaignV2CliConfig
          .parse(validArguments ++ Array(option, "1"), environment, cwd)
          .left
          .toOption
          .get
        assertEquals(
          problem,
          PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidArguments
        )
      }
  }

  test("required options are exact and unique") {
    val missing = validArguments.dropRight(2)
    val duplicate = validArguments ++ Array(
      "--output-package",
      "another.json"
    )
    Vector(missing, duplicate, Array("assemble"), null).foreach { arguments =>
      assertEquals(
        PostgreSqlEnumerationCampaignV2CliConfig
          .parse(arguments, environment, cwd)
          .left
          .toOption
          .get,
        PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidArguments
      )
    }
  }

  test("Python and composer spellings must be absolute and normalized") {
    val cases = Vector(
      validArguments.updated(6, "python3"),
      validArguments.updated(8, "/opt/yrw/../composer")
    )
    cases.foreach { arguments =>
      assertEquals(
        PostgreSqlEnumerationCampaignV2CliConfig
          .parse(arguments, environment, cwd)
          .left
          .toOption
          .get,
        PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidPath
      )
    }
  }

  test("request and output cannot resolve to the same path") {
    val arguments = validArguments.updated(4, "request.json")
    assertEquals(
      PostgreSqlEnumerationCampaignV2CliConfig
        .parse(arguments, environment, cwd)
        .left
        .toOption
        .get,
      PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidPath
    )
  }

  test("ambiguous relative request and output spellings are rejected before normalization") {
    Vector(
      validArguments.updated(2, "../request.json"),
      validArguments.updated(4, "out/../package.json"),
      validArguments.updated(2, "./request.json")
    ).foreach { arguments =>
      assertEquals(
        PostgreSqlEnumerationCampaignV2CliConfig
          .parse(arguments, environment, cwd)
          .left
          .toOption
          .get,
        PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidPath
      )
    }
  }

  test("JDBC values and schema remain environment-only configuration") {
    val missingPassword = environment - PostgreSqlRewriteCliConfig.PasswordEnvironment
    val invalidSchema = environment.updated(
      PostgreSqlRewriteCliConfig.SchemaEnvironment,
      "bad\nname"
    )
    assertEquals(
      PostgreSqlEnumerationCampaignV2CliConfig
        .parse(validArguments, missingPassword, cwd)
        .left
        .toOption
        .get,
      PostgreSqlEnumerationCampaignV2CliConfigFailure.MissingConfiguration
    )
    assertEquals(
      PostgreSqlEnumerationCampaignV2CliConfig
        .parse(validArguments, invalidSchema, cwd)
        .left
        .toOption
        .get,
      PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidSchema
    )
  }

  test("configuration failures never retain caller values") {
    val problem = PostgreSqlEnumerationCampaignV2CliConfig
      .parse(
        validArguments.updated(6, "relative-secret-python"),
        environment,
        cwd
      )
      .left
      .toOption
      .get
    assert(!problem.toString.contains("secret"))
    assert(!problem.toString.contains("relative-secret-python"))
  }
}
