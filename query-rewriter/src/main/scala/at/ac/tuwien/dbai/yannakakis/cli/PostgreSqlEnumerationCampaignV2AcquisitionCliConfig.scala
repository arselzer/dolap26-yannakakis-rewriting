package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.{InvalidPathException, Path, Paths}

/** Closed invocation failures; no value retains a caller path or credential. */
private[cli] sealed trait PostgreSqlEnumerationCampaignV2CliConfigFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlEnumerationCampaignV2CliConfigFailure {
  case object InvalidArguments
      extends PostgreSqlEnumerationCampaignV2CliConfigFailure {
    override val code: String = "acquisition-cli.invalid-arguments"
  }

  case object MissingConfiguration
      extends PostgreSqlEnumerationCampaignV2CliConfigFailure {
    override val code: String = "acquisition-cli.missing-configuration"
  }

  case object InvalidPath
      extends PostgreSqlEnumerationCampaignV2CliConfigFailure {
    override val code: String = "acquisition-cli.invalid-path"
  }

  case object InvalidSchema
      extends PostgreSqlEnumerationCampaignV2CliConfigFailure {
    override val code: String = "acquisition-cli.invalid-schema"
  }
}

/**
  * The acquisition command has no second copy of SQL or search settings.
  * Those values exist only in the canonical authoring request loaded later.
  */
private[cli] final case class PostgreSqlEnumerationCampaignV2CliConfig(
    authoringRequest: Path,
    outputPackage: Path,
    pythonExecutable: Path,
    composerRoot: Path,
    schemaName: String,
    jdbc: JdbcEnvironment
) {
  require(authoringRequest != null, "authoring request path must not be null")
  require(outputPackage != null, "output package path must not be null")
  require(pythonExecutable != null, "Python executable path must not be null")
  require(composerRoot != null, "composer root path must not be null")
  require(schemaName != null && schemaName.nonEmpty, "schema must not be empty")
  require(jdbc != null, "JDBC environment must not be null")
}

private[cli] object PostgreSqlEnumerationCampaignV2CliConfig {
  import PostgreSqlEnumerationCampaignV2CliConfigFailure._

  private val RequiredValueOptions = Set(
    "--authoring-request",
    "--output-package",
    "--python-executable",
    "--composer-root",
    "--schema"
  )

  def parse(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path
  ): Either[
    PostgreSqlEnumerationCampaignV2CliConfigFailure,
    PostgreSqlEnumerationCampaignV2CliConfig
  ] = {
    if (arguments == null || environment == null || workingDirectory == null)
      return Left(InvalidArguments)
    if (!workingDirectory.isAbsolute || workingDirectory != workingDirectory.normalize())
      return Left(InvalidArguments)

    val args = arguments.toVector
    if (args.headOption != Some("acquire")) return Left(InvalidArguments)

    var values = Map.empty[String, String]
    var index = 1
    while (index < args.size) {
      val option = args(index)
      if (
        option == null || !RequiredValueOptions(option) || values.contains(option) ||
        index + 1 >= args.size
      ) return Left(InvalidArguments)
      val value = args(index + 1)
      if (value == null || value.isEmpty || value.startsWith("--"))
        return Left(InvalidArguments)
      values += option -> value
      index += 2
    }

    val required = Vector(
      "--authoring-request",
      "--output-package",
      "--python-executable",
      "--composer-root"
    )
    if (!required.forall(values.contains)) return Left(InvalidArguments)

    val schemaValue = values
      .get("--schema")
      .orElse(environment.get(PostgreSqlRewriteCliConfig.SchemaEnvironment))
      .getOrElse(return Left(MissingConfiguration))
    if (!validSchema(schemaValue)) return Left(InvalidSchema)
    val schema = schemaValue

    val jdbcNames = Vector(
      PostgreSqlRewriteCliConfig.JdbcUrlEnvironment,
      PostgreSqlRewriteCliConfig.UserEnvironment,
      PostgreSqlRewriteCliConfig.PasswordEnvironment
    )
    if (!jdbcNames.forall(environment.contains))
      return Left(MissingConfiguration)
    val jdbcUrl = environment(PostgreSqlRewriteCliConfig.JdbcUrlEnvironment)
    val user = environment(PostgreSqlRewriteCliConfig.UserEnvironment)
    val password = environment(PostgreSqlRewriteCliConfig.PasswordEnvironment)
    if (
      jdbcUrl == null || jdbcUrl.trim.isEmpty || user == null ||
      user.trim.isEmpty || password == null
    ) return Left(MissingConfiguration)

    for {
      authoring <- resolvePath(
        values("--authoring-request"),
        workingDirectory,
        requireAbsoluteSpelling = false
      )
      output <- resolvePath(
        values("--output-package"),
        workingDirectory,
        requireAbsoluteSpelling = false
      )
      python <- resolvePath(
        values("--python-executable"),
        workingDirectory,
        requireAbsoluteSpelling = true
      )
      composer <- resolvePath(
        values("--composer-root"),
        workingDirectory,
        requireAbsoluteSpelling = true
      )
      _ <-
        if (authoring != output) Right(())
        else Left(InvalidPath)
    } yield PostgreSqlEnumerationCampaignV2CliConfig(
      authoringRequest = authoring,
      outputPackage = output,
      pythonExecutable = python,
      composerRoot = composer,
      schemaName = schema,
      jdbc = JdbcEnvironment(jdbcUrl, user, password)
    )
  }

  private def resolvePath(
      spelling: String,
      workingDirectory: Path,
      requireAbsoluteSpelling: Boolean
  ): Either[PostgreSqlEnumerationCampaignV2CliConfigFailure, Path] =
    try {
      val parsed = Paths.get(spelling)
      if (
        parsed.toString.isEmpty ||
        containsDotSegment(parsed) ||
        parsed != parsed.normalize() ||
        (requireAbsoluteSpelling &&
          !parsed.isAbsolute)
      ) Left(InvalidPath)
      else {
        val absolute =
          if (parsed.isAbsolute) parsed.normalize()
          else workingDirectory.resolve(parsed).normalize()
        if (absolute.isAbsolute && absolute == absolute.normalize())
          Right(absolute)
        else Left(InvalidPath)
      }
    } catch {
      case _: InvalidPathException => Left(InvalidPath)
    }

  private def containsDotSegment(path: Path): Boolean = {
    val segments = path.iterator()
    var found = false
    while (segments.hasNext && !found) {
      val segment = segments.next().toString
      found = segment == "." || segment == ".."
    }
    found
  }

  private def validSchema(value: String): Boolean =
    value != null && value.nonEmpty && value.length <= 1024 &&
      value.trim.nonEmpty && value.indexOf('\u0000') < 0 &&
      !value.exists(character => Character.isISOControl(character))
}
