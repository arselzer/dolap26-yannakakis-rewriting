package at.ac.tuwien.dbai.yannakakis.cli

import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{InvalidPathException, Path, Paths}
import java.util.Arrays

import scala.util.control.NonFatal

import org.postgresql.ds.PGSimpleDataSource

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationCampaignV2WholePlanBatch,
  PostgreSqlEnumerationCampaignV2WholePlanCapture,
  PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits,
  PostgreSqlEnumerationCampaignV2WholePlanFailure,
  PostgreSqlEnumerationCampaignV2WholePlanRequest
}

private[cli] sealed trait PostgreSqlEnumerationPlanCaptureV1CliConfigFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlEnumerationPlanCaptureV1CliConfigFailure {
  case object InvalidArguments
      extends PostgreSqlEnumerationPlanCaptureV1CliConfigFailure {
    override val code: String = "plan-capture-v1-cli.invalid-arguments"
  }
  case object MissingConfiguration
      extends PostgreSqlEnumerationPlanCaptureV1CliConfigFailure {
    override val code: String = "plan-capture-v1-cli.missing-configuration"
  }
  case object InvalidPath
      extends PostgreSqlEnumerationPlanCaptureV1CliConfigFailure {
    override val code: String = "plan-capture-v1-cli.invalid-path"
  }
  case object InvalidSchema
      extends PostgreSqlEnumerationPlanCaptureV1CliConfigFailure {
    override val code: String = "plan-capture-v1-cli.invalid-schema"
  }
}

private[cli] final case class PostgreSqlEnumerationPlanCaptureV1CliConfig(
    captureRequest: Path,
    outputAssembly: Path,
    schemaName: String,
    jdbc: JdbcEnvironment
) {
  require(captureRequest != null && outputAssembly != null)
  require(schemaName != null && schemaName.nonEmpty)
  require(jdbc != null)
}

private[cli] object PostgreSqlEnumerationPlanCaptureV1CliConfig {
  import PostgreSqlEnumerationPlanCaptureV1CliConfigFailure._

  private val Options = Set(
    "--capture-request",
    "--output-assembly",
    "--schema"
  )

  def parse(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path
  ): Either[
    PostgreSqlEnumerationPlanCaptureV1CliConfigFailure,
    PostgreSqlEnumerationPlanCaptureV1CliConfig
  ] = {
    if (
      arguments == null || environment == null || workingDirectory == null ||
      !workingDirectory.isAbsolute || workingDirectory != workingDirectory.normalize()
    ) return Left(InvalidArguments)
    val args = arguments.toVector
    var values = Map.empty[String, String]
    var index = 0
    while (index < args.size) {
      val option = args(index)
      if (
        option == null || !Options(option) || values.contains(option) ||
        index + 1 >= args.size
      ) return Left(InvalidArguments)
      val value = args(index + 1)
      if (value == null || value.isEmpty || value.startsWith("--"))
        return Left(InvalidArguments)
      values += option -> value
      index += 2
    }
    if (!Vector("--capture-request", "--output-assembly").forall(values.contains))
      return Left(InvalidArguments)

    val schema = values
      .get("--schema")
      .orElse(environment.get(PostgreSqlRewriteCliConfig.SchemaEnvironment))
      .getOrElse(return Left(MissingConfiguration))
    if (!validSchema(schema)) return Left(InvalidSchema)
    val jdbcNames = Vector(
      PostgreSqlRewriteCliConfig.JdbcUrlEnvironment,
      PostgreSqlRewriteCliConfig.UserEnvironment,
      PostgreSqlRewriteCliConfig.PasswordEnvironment
    )
    if (!jdbcNames.forall(environment.contains)) return Left(MissingConfiguration)
    val jdbcUrl = environment(PostgreSqlRewriteCliConfig.JdbcUrlEnvironment)
    val user = environment(PostgreSqlRewriteCliConfig.UserEnvironment)
    val password = environment(PostgreSqlRewriteCliConfig.PasswordEnvironment)
    if (
      jdbcUrl == null || jdbcUrl.trim.isEmpty || user == null || user.trim.isEmpty ||
      password == null
    ) return Left(MissingConfiguration)

    for {
      request <- resolve(values("--capture-request"), workingDirectory)
      output <- resolve(values("--output-assembly"), workingDirectory)
      _ <- if (request != output) Right(()) else Left(InvalidPath)
    } yield PostgreSqlEnumerationPlanCaptureV1CliConfig(
      request,
      output,
      schema,
      JdbcEnvironment(jdbcUrl, user, password)
    )
  }

  private def resolve(
      spelling: String,
      workingDirectory: Path
  ): Either[PostgreSqlEnumerationPlanCaptureV1CliConfigFailure, Path] =
    try {
      val parsed = Paths.get(spelling)
      val segments = parsed.iterator()
      var ambiguous = parsed.toString.isEmpty || parsed != parsed.normalize()
      while (segments.hasNext) {
        val value = segments.next().toString
        if (value == "." || value == "..") ambiguous = true
      }
      if (ambiguous) Left(InvalidPath)
      else {
        val absolute =
          if (parsed.isAbsolute) parsed.normalize()
          else workingDirectory.resolve(parsed).normalize()
        if (absolute.isAbsolute && absolute == absolute.normalize()) Right(absolute)
        else Left(InvalidPath)
      }
    } catch { case _: InvalidPathException => Left(InvalidPath) }

  private def validSchema(value: String): Boolean =
    value != null && value.nonEmpty && value.trim.nonEmpty &&
      value.length <=
        PostgreSqlEnumerationCampaignV2WholePlanCapture.MaximumPhysicalSchemaCharacters &&
      value.codePointCount(0, value.length) <=
        PostgreSqlEnumerationCampaignV2WholePlanCapture.MaximumPhysicalSchemaCharacters &&
      value.getBytes(StandardCharsets.UTF_8).length <=
        PostgreSqlEnumerationCampaignV2WholePlanCapture.MaximumPhysicalSchemaUtf8Bytes &&
      value.indexOf('\u0000') < 0 &&
      validUnicode(value) &&
      !value.exists(character => Character.isISOControl(character))

  private def validUnicode(value: String): Boolean = {
    var index = 0
    while (index < value.length) {
      val current = value.charAt(index)
      if (Character.isHighSurrogate(current)) {
        if (index + 1 >= value.length || !Character.isLowSurrogate(value.charAt(index + 1)))
          return false
        index += 2
      } else if (Character.isLowSurrogate(current)) return false
      else index += 1
    }
    true
  }
}

private[cli] trait PostgreSqlEnumerationPlanCaptureV1Backend {
  def capture(
      config: PostgreSqlEnumerationPlanCaptureV1CliConfig,
      request: PostgreSqlEnumerationCampaignV2WholePlanRequest
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailure,
    PostgreSqlEnumerationCampaignV2WholePlanBatch
  ]
}

private[cli] object LivePostgreSqlEnumerationPlanCaptureV1Backend
    extends PostgreSqlEnumerationPlanCaptureV1Backend {
  override def capture(
      config: PostgreSqlEnumerationPlanCaptureV1CliConfig,
      request: PostgreSqlEnumerationCampaignV2WholePlanRequest
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailure,
    PostgreSqlEnumerationCampaignV2WholePlanBatch
  ] = {
    require(config != null && request != null)
    val dataSource = new PGSimpleDataSource()
    dataSource.setURL(config.jdbc.jdbcUrl)
    dataSource.setUser(config.jdbc.user)
    dataSource.setPassword(config.jdbc.password)
    PostgreSqlEnumerationCampaignV2WholePlanCapture
      .forDataSource(dataSource, config.schemaName)
      .fold(
        _ => throw new IllegalArgumentException("capture configuration rejected"),
        _.capture(request, PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.default)
      )
  }
}

private[cli] final case class PostgreSqlEnumerationPlanCaptureV1CliFailure(
    code: String,
    exitCode: Int
) {
  require(code != null && code.nonEmpty)
  require(exitCode != 0)
}

/**
  * Silent one-shot PostgreSQL-18 plan capture. It accepts no SQL, settings,
  * limits, or timeout overrides and publishes exactly one create-only assembly.
  */
object PostgreSqlEnumerationPlanCaptureV1Cli {
  private val FailurePrefix = "enumeration-plan-capture-v1 error: "

  def main(arguments: Array[String]): Unit = {
    val savedError = System.err
    val status = run(
      arguments,
      sys.env,
      Paths.get("").toAbsolutePath.normalize(),
      savedError,
      LivePostgreSqlEnumerationPlanCaptureV1Backend
    )
    if (status != 0) System.exit(status)
  }

  private[cli] def run(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path,
      standardError: OutputStream,
    backend: PostgreSqlEnumerationPlanCaptureV1Backend
  ): Int = {
    val result =
      try {
        execute(arguments, environment, workingDirectory, backend)
      } catch {
        case NonFatal(_) => Left(
            PostgreSqlEnumerationPlanCaptureV1CliFailure(
              "plan-capture-v1-cli.internal-error",
              PostgreSqlRewriteExitCode.InternalError
            )
          )
      }
    result match {
      case Right(()) => 0
      case Left(problem) =>
        writeFailure(standardError, problem)
        problem.exitCode
    }
  }

  private def execute(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path,
      backend: PostgreSqlEnumerationPlanCaptureV1Backend
  ): Either[PostgreSqlEnumerationPlanCaptureV1CliFailure, Unit] = {
    if (backend == null) return Left(internal)
    for {
      config <- PostgreSqlEnumerationPlanCaptureV1CliConfig
        .parse(arguments, environment, workingDirectory)
        .left.map(problem => failure(problem.code, PostgreSqlRewriteExitCode.InvalidInvocation))
      loaded <- PostgreSqlEnumerationCampaignV2AcquisitionFiles
        .readAuthoringWithMaximumBytes(
          config.captureRequest,
          PostgreSqlEnumerationPlanCaptureV1RequestParser.MaximumRequestBytes
        )
        .left.map(fileFailure)
      request <- PostgreSqlEnumerationPlanCaptureV1RequestParser
        .parse(loaded.bytes)
        .left.map(problem => failure(problem.code, PostgreSqlRewriteExitCode.InputError))
      _ <- PostgreSqlEnumerationCampaignV2AcquisitionFiles
        .withMappedWorkspace[
          PostgreSqlEnumerationPlanCaptureV1CliFailure,
          Unit
        ](config.outputAssembly, fileFailure, internal) { workspace =>
          for {
            batch <- backend.capture(config, request.wholePlanRequest)
              .left.map(problem =>
                failure(problem.code.id, PostgreSqlRewriteExitCode.ServiceUnavailable)
              )
            reloaded <- PostgreSqlEnumerationCampaignV2AcquisitionFiles
              .readAuthoringWithMaximumBytes(
                config.captureRequest,
                PostgreSqlEnumerationPlanCaptureV1RequestParser.MaximumRequestBytes
              )
              .left.map(fileFailure)
            _ <-
              if (
                Arrays.equals(loaded.bytes, reloaded.bytes) &&
                PostgreSqlEnumerationPlanCaptureV1RequestParser.sha256(
                  reloaded.bytes
                ) == request.captureRequestSha256
              ) Right(())
              else Left(
                failure(
                  "plan-capture-v1-cli.request-changed",
                  PostgreSqlRewriteExitCode.InputError
                )
              )
            assembly <- PostgreSqlEnumerationPlanCaptureV1Assembly
              .render(request, batch)
              .left.map(problem => failure(problem.code, PostgreSqlRewriteExitCode.RewriteFailed))
            _ <- workspace.writeStagedPackage(assembly.bytes).left.map(fileFailure)
            _ <- workspace.publish().left.map(fileFailure)
          } yield ()
        }
    } yield ()
  }

  private def failure(code: String, exit: Int) =
    PostgreSqlEnumerationPlanCaptureV1CliFailure(code, exit)
  private val internal = failure(
    "plan-capture-v1-cli.internal-error",
    PostgreSqlRewriteExitCode.InternalError
  )
  private def fileFailure(
      problem: PostgreSqlEnumerationCampaignV2FileFailure
  ): PostgreSqlEnumerationPlanCaptureV1CliFailure = {
    import PostgreSqlEnumerationCampaignV2FileFailure._
    val exit = problem match {
      case MissingPath | AmbiguousPath | UnsafePathComponent |
          FileIdentityUnavailable | InputUnavailable | InputNotRegular |
          InputSizeInvalid | InputChanged | InputReadFailed |
          OutputParentUnavailable | OutputExists =>
        PostgreSqlRewriteExitCode.InputError
      case PosixPermissionsUnavailable =>
        PostgreSqlRewriteExitCode.ServiceUnavailable
      case _ => PostgreSqlRewriteExitCode.InternalError
    }
    failure(problem.code, exit)
  }

  private def writeFailure(
      output: OutputStream,
      problem: PostgreSqlEnumerationPlanCaptureV1CliFailure
  ): Unit = {
    if (output == null) return
    try {
      output.write((FailurePrefix + problem.code + "\n").getBytes(StandardCharsets.UTF_8))
      output.flush()
    } catch { case NonFatal(_) => () }
  }
}
