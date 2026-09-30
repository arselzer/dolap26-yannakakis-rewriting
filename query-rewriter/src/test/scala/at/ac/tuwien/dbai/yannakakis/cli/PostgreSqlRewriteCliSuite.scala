package at.ac.tuwien.dbai.yannakakis.cli

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  PrintStream
}
import java.nio.charset.StandardCharsets

import at.ac.tuwien.dbai.yannakakis.api.{
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteResult
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteSqlPlanner,
  CalciteSqlPlanningError,
  CalciteSqlPlanningStage
}

class PostgreSqlRewriteCliSuite extends munit.FunSuite {
  private val jdbcUrl =
    "jdbc:postgresql://secret-host/private?user=url-user&password=url-password"
  private val databaseUser = "environment-user"
  private val databasePassword = "environment-password"
  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment -> jdbcUrl,
    PostgreSqlRewriteCliConfig.UserEnvironment -> databaseUser,
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> databasePassword,
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "public"
  )

  test("one invocation emits exactly one compact JSON line and forwards options") {
    val probe = new RecordingProbe(Right(()))
    val backend = new PlanningFailureBackend
    val sql = "SELECT 1\r\n"
    val invocation = run(
      Array(
        "rewrite",
        "--stdin",
        "--schema",
        "journal",
        "--width-bound",
        "3",
        "--plan-limit",
        "6",
        "--reduction-schedule",
        "bottom-up",
        "--no-connected-cover"
      ),
      sql.getBytes(StandardCharsets.UTF_8),
      environment,
      probe,
      backend
    )

    assertEquals(invocation.exitCode, PostgreSqlRewriteExitCode.InvalidQuery)
    val document = oneJsonLine(invocation.output)
    assertEquals(document("status").str, "invalid_query")
    assertEquals(document("request")("original_sql").str, sql)
    assertEquals(document("request")("options")("width_bound").num.toInt, 3)
    assertEquals(document("request")("options")("plan_limit").num.toInt, 6)
    assertEquals(
      document("request")("options")("require_connected_cover").bool,
      false
    )
    assertEquals(
      document("request")("options")("reduction_schedule").str,
      "bottom-up"
    )
    assertEquals(probe.seenSchema, Some("journal"))
    assertEquals(backend.seenRequest.map(_.originalSql), Some(sql))
    assertEquals(
      backend.seenRequest.map(_.options.reductionMode),
      Some(YannakakisReductionMode.BottomUpOnly)
    )
    assert(!invocation.output.contains(jdbcUrl))
    assert(!invocation.output.contains(databaseUser))
    assert(!invocation.output.contains(databasePassword))
    assert(!invocation.output.contains("journal"))
  }

  test("connectivity and schema preflight failures use exit 69") {
    val failure = PostgreSqlRewriteCliFailure.unavailable(
      "CLI_POSTGRESQL_UNAVAILABLE",
      "PostgreSQL connectivity preflight failed"
    )
    val probe = new RecordingProbe(Left(failure))
    val backend = new PlanningFailureBackend
    val invocation = run(
      Array("rewrite", "--stdin"),
      "SELECT 1".getBytes(StandardCharsets.UTF_8),
      environment,
      probe,
      backend
    )

    assertEquals(
      invocation.exitCode,
      PostgreSqlRewriteExitCode.ServiceUnavailable
    )
    val document = oneJsonLine(invocation.output)
    assertEquals(document("status").str, "service_unavailable")
    assertEquals(document("exit_code").num.toInt, 69)
    assertEquals(backend.seenRequest, None)
    Vector(jdbcUrl, databaseUser, databasePassword, "secret-host").foreach {
      secret => assert(!invocation.output.contains(secret))
    }
  }

  test("invalid reduction schedule is rejected before connectivity probing") {
    val probe = new RecordingProbe(Right(()))
    val backend = new PlanningFailureBackend
    val invocation = run(
      Array("rewrite", "--stdin", "--reduction-schedule", "FULL"),
      "SELECT 1".getBytes(StandardCharsets.UTF_8),
      Map.empty,
      probe,
      backend
    )

    assertEquals(invocation.exitCode, PostgreSqlRewriteExitCode.InvalidInvocation)
    val document = oneJsonLine(invocation.output)
    assertEquals(document("status").str, "invalid_invocation")
    assertEquals(document("request"), ujson.Null)
    assertEquals(probe.seenSchema, None)
    assertEquals(backend.seenRequest, None)
  }

  test("live preflight explicitly loads the pinned JDBC driver") {
    assertEquals(
      LivePostgreSqlConnectivityProbe.PostgreSqlDriverClass,
      "org.postgresql.Driver"
    )
    assert(
      LivePostgreSqlConnectivityProbe
        .loadDriverClass(LivePostgreSqlConnectivityProbe.PostgreSqlDriverClass)
        .isRight
    )

    val secretClass = "private.credentials.SecretDriver"
    val failure = LivePostgreSqlConnectivityProbe
      .loadDriverClass(secretClass)
      .left
      .toOption
      .getOrElse(fail("missing driver class unexpectedly loaded"))
    val rendered = PostgreSqlRewriteJson.renderFailure(failure)
    assertEquals(failure.exitCode, PostgreSqlRewriteExitCode.ServiceUnavailable)
    assert(!rendered.contains(secretClass))
    assert(!rendered.contains("ClassNotFoundException"))
  }

  test("process boundary diverts incidental stdout and restores it") {
    val original = System.out
    val redirectedBytes = new ByteArrayOutputStream
    val redirected = new PrintStream(
      redirectedBytes,
      true,
      StandardCharsets.UTF_8.name()
    )
    var observed: PrintStream = null

    val result = PostgreSqlRewriteCli.withThirdPartyStandardOutputRedirected(
      redirected
    ) {
      observed = System.out
      42
    }

    assertEquals(result, 42)
    assert(observed eq redirected)
    assert(System.out eq original)
    redirected.close()
  }

  test("strict UTF-8 rejection occurs before database access") {
    val probe = new RecordingProbe(Right(()))
    val backend = new PlanningFailureBackend
    val invocation = run(
      Array("rewrite", "--stdin"),
      Array[Byte](0xc3.toByte, 0x28.toByte),
      environment,
      probe,
      backend
    )

    assertEquals(invocation.exitCode, PostgreSqlRewriteExitCode.InputError)
    val document = oneJsonLine(invocation.output)
    assertEquals(document("status").str, "input_error")
    assertEquals(probe.seenSchema, None)
    assertEquals(backend.seenRequest, None)
  }

  test("blank SQL is InvalidQuery without a connectivity preflight") {
    val unavailable = PostgreSqlRewriteCliFailure.unavailable(
      "CLI_POSTGRESQL_UNAVAILABLE",
      "PostgreSQL connectivity preflight failed"
    )
    val probe = new RecordingProbe(Left(unavailable))
    val backend = new PlanningFailureBackend
    val original = " \r\n\t "
    val invocation = run(
      Array("rewrite", "--stdin"),
      original.getBytes(StandardCharsets.UTF_8),
      environment,
      probe,
      backend
    )

    assertEquals(invocation.exitCode, PostgreSqlRewriteExitCode.InvalidQuery)
    val document = oneJsonLine(invocation.output)
    assertEquals(document("status").str, "invalid_query")
    assertEquals(document("request")("original_sql").str, original)
    assertEquals(probe.seenSchema, None)
    assertEquals(backend.seenRequest.map(_.originalSql), Some(original))
  }

  test("SQL reader accepts the exact byte bound and rejects one byte more") {
    val exact = StrictUtf8SqlReader.read(
      SqlSource.StandardInput,
      new ByteArrayInputStream("abc".getBytes(StandardCharsets.UTF_8)),
      3
    )
    val excessive = StrictUtf8SqlReader.read(
      SqlSource.StandardInput,
      new ByteArrayInputStream("abcd".getBytes(StandardCharsets.UTF_8)),
      3
    )

    assertEquals(exact, Right("abc"))
    assertEquals(
      excessive.left.toOption.map(_.exitCode),
      Some(PostgreSqlRewriteExitCode.InputError)
    )
  }

  test("unreadable SQL file does not disclose its path") {
    val secretPath = "/private/operator/credentials/hidden-query.sql"
    val invocation = run(
      Array("rewrite", "--sql-file", secretPath),
      Array.emptyByteArray,
      environment,
      new RecordingProbe(Right(())),
      new PlanningFailureBackend
    )

    assertEquals(invocation.exitCode, PostgreSqlRewriteExitCode.InputError)
    oneJsonLine(invocation.output)
    assert(!invocation.output.contains(secretPath))
    assert(!invocation.output.contains("/private/operator"))
  }

  test("missing and null argv or environment data return one typed JSON line") {
    val cases: Vector[(Array[String], Map[String, String])] = Vector(
      ((null: Array[String]), environment),
      (Array("rewrite", "--schema", null, "--stdin"), environment),
      (
        Array("rewrite", "--stdin"),
        environment.updated(PostgreSqlRewriteCliConfig.JdbcUrlEnvironment, null)
      ),
      (Array("rewrite", "--stdin"), null: Map[String, String])
    )

    cases.foreach { case (arguments, suppliedEnvironment) =>
      val invocation = run(
        arguments,
        Array.emptyByteArray,
        suppliedEnvironment,
        new RecordingProbe(Right(())),
        new PlanningFailureBackend
      )
      assert(
        invocation.exitCode == PostgreSqlRewriteExitCode.InvalidInvocation ||
          invocation.exitCode == PostgreSqlRewriteExitCode.InternalError
      )
      oneJsonLine(invocation.output)
    }
  }

  test("backend exceptions become secret-free internal failures") {
    val backend = new PostgreSqlRewriteBackend {
      override def rewrite(
          config: PostgreSqlRewriteCliConfig,
          request: RewriteRequest
      ): Either[PostgreSqlRewriteCliFailure, PostgreSqlRewriteResult] =
        throw new IllegalStateException(
          s"$jdbcUrl $databaseUser $databasePassword private.Exception"
        )
    }
    val invocation = run(
      Array("rewrite", "--stdin"),
      "SELECT 1".getBytes(StandardCharsets.UTF_8),
      environment,
      new RecordingProbe(Right(())),
      backend
    )

    assertEquals(invocation.exitCode, PostgreSqlRewriteExitCode.InternalError)
    val document = oneJsonLine(invocation.output)
    assertEquals(document("status").str, "internal_error")
    Vector(jdbcUrl, databaseUser, databasePassword, "private.Exception").foreach {
      secret => assert(!invocation.output.contains(secret))
    }
  }

  test("help is the only non-JSON stdout mode and needs no environment") {
    val invocation = run(
      Array("rewrite", "--help"),
      Array.emptyByteArray,
      Map.empty,
      new RecordingProbe(Right(())),
      new PlanningFailureBackend
    )

    assertEquals(invocation.exitCode, 0)
    assert(invocation.output.startsWith("Usage:\n"))
    assert(invocation.output.endsWith("\n"))
    assert(!invocation.output.startsWith("{"))
  }

  private def run(
      arguments: Array[String],
      input: Array[Byte],
      suppliedEnvironment: Map[String, String],
      probe: PostgreSqlConnectivityProbe,
      backend: PostgreSqlRewriteBackend
  ): Invocation = {
    val output = new ByteArrayOutputStream
    val exitCode = PostgreSqlRewriteCli.run(
      arguments,
      suppliedEnvironment,
      new ByteArrayInputStream(input),
      output,
      probe,
      backend
    )
    Invocation(exitCode, output.toString(StandardCharsets.UTF_8.name()))
  }

  private def oneJsonLine(output: String): ujson.Value = {
    assert(output.endsWith("\n"), clues(output))
    val content = output.dropRight(1)
    assert(!content.contains('\n'), clues(content))
    assert(!content.contains('\r'), clues(content))
    ujson.read(content)
  }

  private final case class Invocation(exitCode: Int, output: String)

  private final class RecordingProbe(
      result: Either[PostgreSqlRewriteCliFailure, Unit]
  ) extends PostgreSqlConnectivityProbe {
    var seenSchema: Option[String] = None

    override def verify(
        jdbc: JdbcEnvironment,
        schemaName: String
    ): Either[PostgreSqlRewriteCliFailure, Unit] = {
      seenSchema = Some(schemaName)
      result
    }
  }

  private final class PlanningFailureBackend extends PostgreSqlRewriteBackend {
    var seenRequest: Option[RewriteRequest] = None

    private val planner = CalciteSqlPlanner
      .fromFunction(_ =>
        Left(
          CalciteSqlPlanningError(
            CalciteSqlPlanningStage.Parsing,
            "ignored parser detail"
          )
        )
      )
      .fold(problem => throw new AssertionError(problem.message), identity)
    private val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => throw new AssertionError(problem.message), identity)

    override def rewrite(
        config: PostgreSqlRewriteCliConfig,
        request: RewriteRequest
    ): Either[PostgreSqlRewriteCliFailure, PostgreSqlRewriteResult] = {
      seenRequest = Some(request)
      Right(rewriter.rewriteDetailed(request))
    }
  }
}
