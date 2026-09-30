package at.ac.tuwien.dbai.yannakakis.cli

import java.io.{ByteArrayOutputStream, InputStream, OutputStream, PrintStream}
import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.Files
import java.sql.{Connection, DriverManager, PreparedStatement, ResultSet}

import at.ac.tuwien.dbai.yannakakis.api.{
  RewriteRequest,
  TargetDialect
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteResult
}

import scala.util.control.NonFatal

/** Deterministic command-line boundary for the PostgreSQL external rewriter. */
object PostgreSqlRewriteCli {
  private[cli] val MaxSqlInputBytes: Int = 16 * 1024 * 1024

  def main(arguments: Array[String]): Unit = {
    // Some transitive Calcite/logging dependencies write startup warnings to
    // System.out. Preserve stdout as the machine-readable channel by keeping a
    // direct reference to it and diverting incidental process output to stderr
    // for the duration of the rewrite.
    val machineOutput = System.out
    val exitCode = withThirdPartyStandardOutputRedirected(System.err) {
      run(
        arguments = arguments,
        environment = sys.env,
        standardInput = System.in,
        standardOutput = machineOutput,
        connectivityProbe = LivePostgreSqlConnectivityProbe,
        backend = LivePostgreSqlRewriteBackend
      )
    }
    if (exitCode != PostgreSqlRewriteExitCode.Rewritten) {
      System.exit(exitCode)
    }
  }

  private[cli] def withThirdPartyStandardOutputRedirected[A](
      standardError: PrintStream
  )(body: => A): A = {
    require(standardError != null, "standard error must not be null")
    this.synchronized {
      val previous = System.out
      try {
        System.setOut(standardError)
        body
      } finally System.setOut(previous)
    }
  }

  private[cli] def run(
      arguments: Array[String],
      environment: Map[String, String],
      standardInput: InputStream,
      standardOutput: OutputStream,
      connectivityProbe: PostgreSqlConnectivityProbe,
      backend: PostgreSqlRewriteBackend
  ): Int = {
    val emission =
      try {
        execute(
          arguments,
          environment,
          standardInput,
          connectivityProbe,
          backend
        )
      } catch {
        case NonFatal(_) => failureEmission(PostgreSqlRewriteCliFailure.internal)
      }

    writeUtf8Line(standardOutput, emission.content)
    emission.exitCode
  }

  private def execute(
      arguments: Array[String],
      environment: Map[String, String],
      standardInput: InputStream,
      connectivityProbe: PostgreSqlConnectivityProbe,
      backend: PostgreSqlRewriteBackend
  ): CliEmission =
    PostgreSqlRewriteCliConfig.parse(arguments, environment) match {
      case Left(problem) => failureEmission(problem)
      case Right(PostgreSqlRewriteCliCommand.Help) =>
        CliEmission(
          PostgreSqlRewriteExitCode.Rewritten,
          PostgreSqlRewriteCliConfig.Usage.stripSuffix("\n")
        )
      case Right(PostgreSqlRewriteCliCommand.Rewrite(config)) =>
        StrictUtf8SqlReader.read(
          config.sqlSource,
          standardInput,
          MaxSqlInputBytes
        ) match {
          case Left(problem) => failureEmission(problem)
          case Right(sql) =>
            val request = RewriteRequest(
              originalSql = sql,
              targetDialect = TargetDialect.PostgreSql,
              options = config.options
            )

            def rewrite(): CliEmission =
              backend.rewrite(config, request) match {
                case Left(problem) => failureEmission(problem)
                case Right(result) =>
                  val exitCode = PostgreSqlRewriteExitCode.forOutcome(
                    result.outcome
                  )
                  CliEmission(
                    exitCode,
                    PostgreSqlRewriteJson.render(request, result)
                  )
              }

            // Blank SQL is catalog-independent. Preserve the public API's
            // InvalidQuery classification even when PostgreSQL is unavailable.
            if (sql.trim.isEmpty) rewrite()
            else
              connectivityProbe.verify(config.jdbc, config.schemaName) match {
                case Left(problem) => failureEmission(problem)
                case Right(_)      => rewrite()
              }
        }
    }

  private def failureEmission(
      problem: PostgreSqlRewriteCliFailure
  ): CliEmission =
    CliEmission(problem.exitCode, PostgreSqlRewriteJson.renderFailure(problem))

  private def writeUtf8Line(output: OutputStream, content: String): Unit = {
    require(output != null, "standard output must not be null")
    output.write(content.getBytes(StandardCharsets.UTF_8))
    output.write('\n')
    output.flush()
  }

  private final case class CliEmission(exitCode: Int, content: String)
}

private[cli] trait PostgreSqlRewriteBackend {
  def rewrite(
      config: PostgreSqlRewriteCliConfig,
      request: RewriteRequest
  ): Either[PostgreSqlRewriteCliFailure, PostgreSqlRewriteResult]
}

private[cli] object LivePostgreSqlRewriteBackend
    extends PostgreSqlRewriteBackend {
  override def rewrite(
      config: PostgreSqlRewriteCliConfig,
      request: RewriteRequest
  ): Either[PostgreSqlRewriteCliFailure, PostgreSqlRewriteResult] =
    PostgreSqlQueryRewriter
      .forPostgreSqlJdbc(
        jdbcUrl = config.jdbc.jdbcUrl,
        user = config.jdbc.user,
        password = config.jdbc.password,
        schemaName = config.schemaName,
        explainLimits = config.explainLimits,
        factorAwareLimits = config.factorAwareLimits,
        physicalPortfolioLimits = config.physicalPortfolioLimits,
        physicalPortfolioEvaluationLimits =
          config.physicalPortfolioEvaluationLimits
      )
      .left
      .map(_ =>
        PostgreSqlRewriteCliFailure.unavailable(
          code = "CLI_REWRITER_CONFIGURATION_FAILED",
          message = "PostgreSQL rewriter configuration failed"
        )
      )
      .map(_.rewriteDetailed(request))
}

private[cli] trait PostgreSqlConnectivityProbe {
  def verify(
      jdbc: JdbcEnvironment,
      schemaName: String
  ): Either[PostgreSqlRewriteCliFailure, Unit]
}

/** Verifies connectivity and exact PostgreSQL schema existence before Calcite.
  *
  * Without this typed preflight, a catalog outage can surface during Calcite
  * validation and become indistinguishable from invalid SQL. The fixed query
  * uses a bound parameter and does not interpolate the schema name.
  */
private[cli] object LivePostgreSqlConnectivityProbe
    extends PostgreSqlConnectivityProbe {
  private[cli] val PostgreSqlDriverClass: String = "org.postgresql.Driver"
  private val SchemaQuery =
    "SELECT 1 FROM pg_catalog.pg_namespace WHERE nspname = ?"

  override def verify(
      jdbc: JdbcEnvironment,
      schemaName: String
  ): Either[PostgreSqlRewriteCliFailure, Unit] =
    loadDriverClass(PostgreSqlDriverClass).flatMap(_ =>
      verifyWithLoadedDriver(jdbc, schemaName)
    )

  /** Explicit loading is required because assembly discards META-INF services. */
  private[cli] def loadDriverClass(
      driverClass: String
  ): Either[PostgreSqlRewriteCliFailure, Unit] =
    try {
      Class.forName(driverClass)
      Right(())
    } catch {
      case NonFatal(_) =>
        Left(
          PostgreSqlRewriteCliFailure.unavailable(
            code = "CLI_POSTGRESQL_DRIVER_UNAVAILABLE",
            message = "The PostgreSQL JDBC driver is unavailable"
          )
        )
    }

  private def verifyWithLoadedDriver(
      jdbc: JdbcEnvironment,
      schemaName: String
  ): Either[PostgreSqlRewriteCliFailure, Unit] = {
    var connection: Connection = null
    var statement: PreparedStatement = null
    var result: ResultSet = null
    try {
      connection = DriverManager.getConnection(
        jdbc.jdbcUrl,
        jdbc.user,
        jdbc.password
      )
      statement = connection.prepareStatement(SchemaQuery)
      statement.setString(1, schemaName)
      result = statement.executeQuery()
      if (result.next()) Right(())
      else
        Left(
          PostgreSqlRewriteCliFailure.unavailable(
            code = "CLI_POSTGRESQL_SCHEMA_UNAVAILABLE",
            message = "The configured PostgreSQL schema is unavailable"
          )
        )
    } catch {
      case NonFatal(_) =>
        Left(
          PostgreSqlRewriteCliFailure.unavailable(
            code = "CLI_POSTGRESQL_UNAVAILABLE",
            message = "PostgreSQL connectivity preflight failed"
          )
        )
    } finally {
      closeQuietly(result)
      closeQuietly(statement)
      closeQuietly(connection)
    }
  }

  private def closeQuietly(resource: AutoCloseable): Unit =
    if (resource != null) {
      try resource.close()
      catch { case NonFatal(_) => () }
    }
}

private[cli] object StrictUtf8SqlReader {
  def read(
      source: SqlSource,
      standardInput: InputStream,
      maximumBytes: Int
  ): Either[PostgreSqlRewriteCliFailure, String] = {
    require(source != null, "SQL source must not be null")
    require(standardInput != null, "standard input must not be null")
    require(maximumBytes >= 0, "maximum SQL input size must not be negative")

    val sourceName = source match {
      case SqlSource.StandardInput => "stdin"
      case _: SqlSource.File       => "sql-file"
    }

    val bytes = source match {
      case SqlSource.StandardInput =>
        readBounded(standardInput, maximumBytes, sourceName)
      case SqlSource.File(path) =>
        try {
          if (Files.size(path) > maximumBytes) {
            Left(tooLarge(sourceName, maximumBytes))
          } else {
            val input = Files.newInputStream(path)
            try readBounded(input, maximumBytes, sourceName)
            finally input.close()
          }
        } catch {
          case NonFatal(_) =>
            Left(
              PostgreSqlRewriteCliFailure.input(
                "SQL input could not be read",
                sourceName
              )
            )
        }
    }

    bytes.flatMap { supplied =>
      try {
        val decoder = StandardCharsets.UTF_8
          .newDecoder()
          .onMalformedInput(CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)
        Right(decoder.decode(ByteBuffer.wrap(supplied)).toString)
      } catch {
        case NonFatal(_) =>
          Left(
            PostgreSqlRewriteCliFailure.input(
              "SQL input is not valid UTF-8",
              sourceName
            )
          )
      }
    }
  }

  private def readBounded(
      input: InputStream,
      maximumBytes: Int,
      sourceName: String
  ): Either[PostgreSqlRewriteCliFailure, Array[Byte]] = {
    val output = new ByteArrayOutputStream(math.min(maximumBytes, 8192))
    val buffer = new Array[Byte](8192)
    var total = 0
    try {
      var read = input.read(buffer)
      while (read != -1) {
        if (read == 0) {
          val single = input.read()
          if (single == -1) read = -1
          else {
            if (total == maximumBytes) {
              return Left(tooLarge(sourceName, maximumBytes))
            }
            output.write(single)
            total += 1
            read = input.read(buffer)
          }
        } else {
          if (read > maximumBytes - total) {
            return Left(tooLarge(sourceName, maximumBytes))
          }
          output.write(buffer, 0, read)
          total += read
          read = input.read(buffer)
        }
      }
      Right(output.toByteArray)
    } catch {
      case NonFatal(_) =>
        Left(
          PostgreSqlRewriteCliFailure.input(
            "SQL input could not be read",
            sourceName
          )
        )
    }
  }

  private def tooLarge(
      sourceName: String,
      maximumBytes: Int
  ): PostgreSqlRewriteCliFailure =
    PostgreSqlRewriteCliFailure.input(
      s"SQL input exceeds the $maximumBytes-byte limit",
      sourceName
    )
}
