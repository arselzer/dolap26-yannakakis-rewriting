package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.{
  RewriteOptions,
  YannakakisReductionMode
}

class PostgreSqlRewriteCliConfigSuite extends munit.FunSuite {
  private val completeEnvironment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
      "jdbc:postgresql://db.internal/research",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "researcher",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "very-secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "benchmark"
  )

  test("stdin command uses documented defaults and environment schema") {
    val config = parsedConfig(Array("rewrite", "--stdin"), completeEnvironment)

    assertEquals(config.sqlSource, SqlSource.StandardInput)
    assertEquals(config.schemaName, "benchmark")
    assertEquals(config.options, RewriteOptions.Default)
    assertEquals(config.jdbc.user, "researcher")
  }

  test("flags override schema and every rewrite option") {
    val config = parsedConfig(
      Array(
        "rewrite",
        "--stdin",
        "--schema",
        "journal",
        "--width-bound",
        "3",
        "--plan-limit",
        "7",
        "--no-connected-cover"
      ),
      completeEnvironment
    )

    assertEquals(config.schemaName, "journal")
    assertEquals(config.options, RewriteOptions(3, 7, false))
  }

  test("SQL file is the only accepted non-stdin source") {
    val config = parsedConfig(
      Array("rewrite", "--sql-file", "queries/triangle.sql"),
      completeEnvironment
    )

    config.sqlSource match {
      case SqlSource.File(path) =>
        assertEquals(path.toString, "queries/triangle.sql")
      case other => fail(s"expected SQL file source, received $other")
    }
  }

  test("help does not require credentials") {
    assertEquals(
      PostgreSqlRewriteCliConfig.parse(Array("--help"), Map.empty),
      Right(PostgreSqlRewriteCliCommand.Help)
    )
    assertEquals(
      PostgreSqlRewriteCliConfig.parse(
        Array("rewrite", "--help"),
        Map.empty
      ),
      Right(PostgreSqlRewriteCliCommand.Help)
    )
  }

  test("SQL input choices are exclusive and mandatory") {
    val both = parseFailure(
      Array("rewrite", "--stdin", "--sql-file", "query.sql"),
      completeEnvironment
    )
    val neither = parseFailure(Array("rewrite"), completeEnvironment)

    assertEquals(both.exitCode, PostgreSqlRewriteExitCode.InvalidInvocation)
    assertEquals(neither.exitCode, PostgreSqlRewriteExitCode.InvalidInvocation)
    assertEquals(both.context("options"), "--sql-file,--stdin")
  }

  test("duplicate and conflicting options fail closed") {
    val duplicate = parseFailure(
      Array("rewrite", "--stdin", "--plan-limit", "2", "--plan-limit", "3"),
      completeEnvironment
    )
    val conflict = parseFailure(
      Array(
        "rewrite",
        "--stdin",
        "--connected-cover",
        "--no-connected-cover"
      ),
      completeEnvironment
    )

    assertEquals(duplicate.context("option"), "--plan-limit")
    assert(conflict.message.contains("mutually exclusive"))
  }

  test("invalid integers and nonpositive options are typed invocation errors") {
    val malformed = parseFailure(
      Array("rewrite", "--stdin", "--width-bound", "many"),
      completeEnvironment
    )
    val nonpositive = parseFailure(
      Array("rewrite", "--stdin", "--plan-limit", "0"),
      completeEnvironment
    )

    assertEquals(malformed.context("option"), "--width-bound")
    assertEquals(nonpositive.context("options"), "planLimit")
  }

  test("schema flag takes precedence over an invalid environment schema") {
    val environment = completeEnvironment.updated(
      PostgreSqlRewriteCliConfig.SchemaEnvironment,
      "   "
    )
    val config = parsedConfig(
      Array("rewrite", "--stdin", "--schema", "public"),
      environment
    )

    assertEquals(config.schemaName, "public")
  }

  test("missing credentials report names only and allow an empty password") {
    val missing = parseFailure(
      Array("rewrite", "--stdin"),
      Map(
        PostgreSqlRewriteCliConfig.SchemaEnvironment -> "public",
        PostgreSqlRewriteCliConfig.PasswordEnvironment -> "do-not-print"
      )
    )
    assert(missing.context("required").contains("YANNAKAKIS_JDBC_URL"))
    assert(missing.context("required").contains("YANNAKAKIS_DB_USER"))
    assert(!missing.toString.contains("do-not-print"))

    val emptyPassword = parsedConfig(
      Array("rewrite", "--stdin"),
      completeEnvironment.updated(
        PostgreSqlRewriteCliConfig.PasswordEnvironment,
        ""
      )
    )
    assertEquals(emptyPassword.jdbc.password, "")
  }

  test("null argv and null environment values never throw") {
    val nullArguments = PostgreSqlRewriteCliConfig.parse(null, completeEnvironment)
    assert(nullArguments.isLeft)

    val nullValueEnvironment = completeEnvironment.updated(
      PostgreSqlRewriteCliConfig.JdbcUrlEnvironment,
      null
    )
    assert(
      PostgreSqlRewriteCliConfig
        .parse(Array("rewrite", "--stdin"), nullValueEnvironment)
        .isLeft
    )

    assert(
      PostgreSqlRewriteCliConfig
        .parse(Array("rewrite", "--schema", null, "--stdin"), completeEnvironment)
        .isLeft
    )
  }

  test("password command-line arguments are rejected without consuming values") {
    val failure = parseFailure(
      Array("rewrite", "--stdin", "--password", "argv-secret"),
      completeEnvironment
    )

    assertEquals(failure.context("argument"), "unsupported")
    assert(!failure.toString.contains("argv-secret"))
  }

  test("blank and invalid SQL file paths are invocation errors") {
    assert(
      PostgreSqlRewriteCliConfig
        .parse(
          Array("rewrite", "--sql-file", ""),
          completeEnvironment
        )
        .isLeft
    )
    assert(
      PostgreSqlRewriteCliConfig
        .parse(
          Array("rewrite", "--sql-file", "bad" + 0.toChar + "path"),
          completeEnvironment
        )
        .isLeft
    )
  }

  test("reduction schedule defaults to full and accepts every exact identifier") {
    assertEquals(
      parsedConfig(Array("rewrite", "--stdin"), completeEnvironment)
        .options
        .reductionMode,
      YannakakisReductionMode.FullTwoPass
    )
    YannakakisReductionMode.Values.foreach { mode =>
      val config = parsedConfig(
        Array("rewrite", "--stdin", "--reduction-schedule", mode.id),
        completeEnvironment
      )
      assertEquals(config.options.reductionMode, mode)
    }
  }

  test("reduction schedule parsing is strict, non-null, and duplicate-safe") {
    Vector("", "FULL", " full", "full ", "bottom_up", "unknown").foreach {
      value =>
        val failure = parseFailure(
          Array("rewrite", "--stdin", "--reduction-schedule", value),
          completeEnvironment
        )
        assertEquals(failure.exitCode, PostgreSqlRewriteExitCode.InvalidInvocation)
        assertEquals(failure.context("option"), "--reduction-schedule")
    }

    val missing = parseFailure(
      Array("rewrite", "--stdin", "--reduction-schedule"),
      completeEnvironment
    )
    assertEquals(missing.context("option"), "--reduction-schedule")

    val nullValue = parseFailure(
      Array("rewrite", "--stdin", "--reduction-schedule", null),
      completeEnvironment
    )
    assertEquals(nullValue.context("option"), "--reduction-schedule")

    val duplicate = parseFailure(
      Array(
        "rewrite",
        "--stdin",
        "--reduction-schedule",
        "full",
        "--reduction-schedule",
        "none"
      ),
      completeEnvironment
    )
    assertEquals(duplicate.context("option"), "--reduction-schedule")
  }

  test("invalid reduction schedule is diagnosed before JDBC environment requirements") {
    val failure = parseFailure(
      Array("rewrite", "--stdin", "--reduction-schedule", "FULL"),
      Map.empty
    )

    assertEquals(failure.exitCode, PostgreSqlRewriteExitCode.InvalidInvocation)
    assertEquals(failure.context, Map("option" -> "--reduction-schedule"))
    assert(!failure.context.contains("required"))
  }

  private def parsedConfig(
      arguments: Array[String],
      environment: Map[String, String]
  ): PostgreSqlRewriteCliConfig =
    PostgreSqlRewriteCliConfig.parse(arguments, environment) match {
      case Right(PostgreSqlRewriteCliCommand.Rewrite(config)) => config
      case other => fail(s"expected rewrite configuration, received $other")
    }

  private def parseFailure(
      arguments: Array[String],
      environment: Map[String, String]
  ): PostgreSqlRewriteCliFailure =
    PostgreSqlRewriteCliConfig.parse(arguments, environment) match {
      case Left(problem) => problem
      case other         => fail(s"expected CLI failure, received $other")
    }
}
