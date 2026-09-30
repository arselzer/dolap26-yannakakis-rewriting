package at.ac.tuwien.dbai.yannakakis.cli

import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{InvalidPathException, Path, Paths}
import java.util.Arrays

import scala.util.control.NonFatal

import at.ac.tuwien.dbai.yannakakis.api.RewriteOptions

private[cli] sealed trait PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
  case object InvalidArguments
      extends PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
    override val code: String = "generated-sql-rendering-cli.invalid-arguments"
  }
  case object MissingConfiguration
      extends PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
    override val code: String = "generated-sql-rendering-cli.missing-configuration"
  }
  case object InvalidPath
      extends PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
    override val code: String = "generated-sql-rendering-cli.invalid-path"
  }
  case object InvalidSchema
      extends PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
    override val code: String = "generated-sql-rendering-cli.invalid-schema"
  }
  case object InvalidIdentity
      extends PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
    override val code: String = "generated-sql-rendering-cli.invalid-identity"
  }
  case object InvalidFamily
      extends PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
    override val code: String = "generated-sql-rendering-cli.invalid-family"
  }
  case object InvalidRank
      extends PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
    override val code: String = "generated-sql-rendering-cli.invalid-rank"
  }
  case object InvalidProfileOrdinals
      extends PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
    override val code: String =
      "generated-sql-rendering-cli.invalid-profile-ordinals"
  }
  case object InvalidPlanShape
      extends PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure {
    override val code: String = "generated-sql-rendering-cli.invalid-plan-shape"
  }
}

private[cli] final case class PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig(
    queryFile: Path,
    outputAssembly: Path,
    schemaName: String,
    queryId: String,
    family: PostgreSqlGeneratedSqlOptimizationFamily,
    cellId: String,
    datasetId: String,
    candidateRank: Int,
    profileOrdinals: Vector[Int],
    planShape: PostgreSqlGeneratedSqlOptimizationPlanShape,
    jdbc: JdbcEnvironment
) {
  require(queryFile != null && outputAssembly != null)
  require(schemaName != null && schemaName.nonEmpty)
  require(queryId != null && cellId != null && datasetId != null)
  require(family != null && planShape != null && jdbc != null)
  require(
    candidateRank > 0 && candidateRank <= RewriteOptions.DefaultPlanLimit
  )
  require(
    profileOrdinals.nonEmpty && profileOrdinals.head == 1 &&
      profileOrdinals == profileOrdinals.sorted.distinct &&
      profileOrdinals.forall(value => value >= 1 && value <= 8)
  )
}

private[cli] object PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig {
  import PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure._

  private val RequiredOptions = Vector(
    "--query-file",
    "--output-assembly",
    "--query-id",
    "--family",
    "--cell-id",
    "--dataset-id",
    "--candidate-rank",
    "--profile-ordinals",
    "--plan-shape"
  )
  private val Options = RequiredOptions.toSet + "--schema"
  private val SafeId = "[a-z][a-z0-9_-]{0,127}"

  def parse(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path
  ): Either[
    PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure,
    PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig
  ] = {
    if (
      arguments == null || environment == null || workingDirectory == null ||
      !workingDirectory.isAbsolute ||
      workingDirectory != workingDirectory.normalize()
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
    if (!RequiredOptions.forall(values.contains)) return Left(InvalidArguments)

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

    val queryId = values("--query-id")
    val cellId = values("--cell-id")
    val datasetId = values("--dataset-id")
    if (!Vector(queryId, cellId, datasetId).forall(_.matches(SafeId)))
      return Left(InvalidIdentity)
    val family = PostgreSqlGeneratedSqlOptimizationFamily
      .parse(values("--family"))
      .getOrElse(return Left(InvalidFamily))
    val rankText = values("--candidate-rank")
    if (!rankText.matches("[1-9][0-9]*")) return Left(InvalidRank)
    val rank =
      try rankText.toInt
      catch { case _: NumberFormatException => return Left(InvalidRank) }
    if (rank > RewriteOptions.DefaultPlanLimit) return Left(InvalidRank)
    val profileText = values("--profile-ordinals")
    if (!profileText.matches("[1-8](,[1-8]){0,7}"))
      return Left(InvalidProfileOrdinals)
    val profiles = profileText.split(",", -1).iterator.map(_.toInt).toVector
    if (
      profiles.headOption != Some(1) || profiles != profiles.sorted.distinct
    ) return Left(InvalidProfileOrdinals)
    val shape = PostgreSqlGeneratedSqlOptimizationPlanShape
      .parse(values("--plan-shape"))
      .getOrElse(return Left(InvalidPlanShape))

    for {
      query <- resolve(values("--query-file"), workingDirectory)
      output <- resolve(values("--output-assembly"), workingDirectory)
      _ <- if (query != output) Right(()) else Left(InvalidPath)
    } yield PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig(
      queryFile = query,
      outputAssembly = output,
      schemaName = schema,
      queryId = queryId,
      family = family,
      cellId = cellId,
      datasetId = datasetId,
      candidateRank = rank,
      profileOrdinals = profiles,
      planShape = shape,
      jdbc = JdbcEnvironment(jdbcUrl, user, password)
    )
  }

  private def resolve(
      spelling: String,
      workingDirectory: Path
  ): Either[PostgreSqlGeneratedSqlOptimizationRenderingCliConfigFailure, Path] =
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
      value.length <= 256 && value.codePointCount(0, value.length) <= 256 &&
      value.getBytes(StandardCharsets.UTF_8).length <= 1024 &&
      value.indexOf('\u0000') < 0 && validUnicode(value) &&
      !value.exists(character => Character.isISOControl(character))

  private def validUnicode(value: String): Boolean = {
    var index = 0
    while (index < value.length) {
      val current = value.charAt(index)
      if (Character.isHighSurrogate(current)) {
        if (
          index + 1 >= value.length ||
          !Character.isLowSurrogate(value.charAt(index + 1))
        ) return false
        index += 2
      } else if (Character.isLowSurrogate(current)) return false
      else index += 1
    }
    true
  }
}

private[cli] final case class PostgreSqlGeneratedSqlOptimizationRenderingCliFailure(
    code: String,
    exitCode: Int
) {
  require(code != null && code.nonEmpty)
  require(exitCode != 0)
}

/**
  * Silent development-only producer for one canonical rendering assembly.
  * Connection credentials are environment-only, and the output is a private
  * mode-0600 create-only file. It neither executes SQL nor chooses a profile.
  */
object PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCli {
  private val FailurePrefix = "generated-sql-rendering-cli error: "

  def main(arguments: Array[String]): Unit = {
    val status = run(
      arguments,
      sys.env,
      Paths.get("").toAbsolutePath.normalize(),
      System.err,
      LivePostgreSqlGeneratedSqlOptimizationRenderingBackend
    )
    if (status != 0) System.exit(status)
  }

  private[cli] def run(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path,
      standardError: OutputStream,
      backend: PostgreSqlGeneratedSqlOptimizationRenderingBackend
  ): Int = {
    val result =
      try execute(arguments, environment, workingDirectory, backend)
      catch { case NonFatal(_) => Left(internal) }
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
      backend: PostgreSqlGeneratedSqlOptimizationRenderingBackend
  ): Either[PostgreSqlGeneratedSqlOptimizationRenderingCliFailure, Unit] = {
    if (backend == null) return Left(internal)
    for {
      config <- PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig
        .parse(arguments, environment, workingDirectory)
        .left.map(problem =>
          failure(problem.code, PostgreSqlRewriteExitCode.InvalidInvocation)
        )
      loaded <- PostgreSqlEnumerationCampaignV2AcquisitionFiles
        .readAuthoringWithMaximumBytes(
          config.queryFile,
          PostgreSqlGeneratedSqlOptimizationSource.MaximumSourceBytes
        )
        .left.map(fileFailure)
      source <- PostgreSqlGeneratedSqlOptimizationSource
        .fromBytes(loaded.bytes)
        .left.map(problem =>
          failure(problem.code, PostgreSqlRewriteExitCode.InputError)
        )
      _ <- PostgreSqlEnumerationCampaignV2AcquisitionFiles
        .withMappedWorkspace[
          PostgreSqlGeneratedSqlOptimizationRenderingCliFailure,
          Unit
        ](config.outputAssembly, fileFailure, internal) { workspace =>
          for {
            assembly <- backend
              .produce(config, source)
              .left.map(backendFailure)
            reloaded <- PostgreSqlEnumerationCampaignV2AcquisitionFiles
              .readAuthoringWithMaximumBytes(
                config.queryFile,
                PostgreSqlGeneratedSqlOptimizationSource.MaximumSourceBytes
              )
              .left.map(fileFailure)
            _ <-
              if (Arrays.equals(loaded.bytes, reloaded.bytes)) Right(())
              else
                Left(
                  failure(
                    "generated-sql-rendering-cli.query-changed",
                    PostgreSqlRewriteExitCode.InputError
                  )
                )
            document =
              PostgreSqlGeneratedSqlOptimizationRenderingAssembly.render(
                assembly
              )
            _ <- workspace
              .writeStagedPackage(document.bytes)
              .left.map(fileFailure)
            _ <- workspace.publish().left.map(fileFailure)
          } yield ()
        }
    } yield ()
  }

  private def backendFailure(
      problem: PostgreSqlGeneratedSqlOptimizationBackendFailure
  ): PostgreSqlGeneratedSqlOptimizationRenderingCliFailure = {
    val exit = problem match {
      case PostgreSqlGeneratedSqlOptimizationBackendFailure.ConfigurationRejected =>
        PostgreSqlRewriteExitCode.ServiceUnavailable
      case PostgreSqlGeneratedSqlOptimizationBackendFailure.RewriteRejected |
          PostgreSqlGeneratedSqlOptimizationBackendFailure.ProfileRenderingFailed |
          PostgreSqlGeneratedSqlOptimizationBackendFailure.GeneratedSqlInvalid =>
        PostgreSqlRewriteExitCode.RewriteFailed
      case PostgreSqlGeneratedSqlOptimizationBackendFailure.IdentityMismatch =>
        PostgreSqlRewriteExitCode.InternalError
    }
    failure(problem.code, exit)
  }

  private def fileFailure(
      problem: PostgreSqlEnumerationCampaignV2FileFailure
  ): PostgreSqlGeneratedSqlOptimizationRenderingCliFailure =
    failure(problem.code, PostgreSqlRewriteExitCode.InputError)

  private def failure(
      code: String,
      exitCode: Int
  ): PostgreSqlGeneratedSqlOptimizationRenderingCliFailure =
    PostgreSqlGeneratedSqlOptimizationRenderingCliFailure(code, exitCode)

  private val internal = failure(
    "generated-sql-rendering-cli.internal-error",
    PostgreSqlRewriteExitCode.InternalError
  )

  private def writeFailure(
      output: OutputStream,
      problem: PostgreSqlGeneratedSqlOptimizationRenderingCliFailure
  ): Unit = {
    if (output == null) return
    try {
      output.write(
        (FailurePrefix + problem.code + "\n").getBytes(StandardCharsets.UTF_8)
      )
      output.flush()
    } catch { case NonFatal(_) => () }
  }
}
