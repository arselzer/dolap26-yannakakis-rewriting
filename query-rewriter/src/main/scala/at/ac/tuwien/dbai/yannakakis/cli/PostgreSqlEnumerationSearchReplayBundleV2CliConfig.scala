package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.Path

/** Closed path/JDBC configuration for the additive replay-v2 publisher. */
private[cli] final case class PostgreSqlEnumerationSearchReplayBundleV2CliConfig(
    authoringRequest: Path,
    outputBundle: Path,
    pythonExecutable: Path,
    composerRoot: Path,
    schemaName: String,
    jdbc: JdbcEnvironment
) {
  require(authoringRequest != null, "v2 authoring path must not be null")
  require(outputBundle != null, "v2 output bundle must not be null")
  require(pythonExecutable != null, "v2 Python path must not be null")
  require(composerRoot != null, "v2 composer root must not be null")
  require(schemaName != null && schemaName.nonEmpty, "v2 schema is empty")
  require(jdbc != null, "v2 JDBC environment must not be null")

  private[cli] def backendConfig(
      stagedPackage: Path
  ): PostgreSqlEnumerationCampaignV2CliConfig = {
    require(
      stagedPackage != null && stagedPackage.isAbsolute &&
        stagedPackage == stagedPackage.normalize(),
      "v2 staged package path must be absolute and normalized"
    )
    PostgreSqlEnumerationCampaignV2CliConfig(
      authoringRequest = authoringRequest,
      outputPackage = stagedPackage,
      pythonExecutable = pythonExecutable,
      composerRoot = composerRoot,
      schemaName = schemaName,
      jdbc = jdbc
    )
  }
}

private[cli] object PostgreSqlEnumerationSearchReplayBundleV2CliConfig {
  /** Reuses the byte/path-stable v1 grammar without changing its contract. */
  def parse(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path
  ): Either[
    PostgreSqlEnumerationCampaignV2CliConfigFailure,
    PostgreSqlEnumerationSearchReplayBundleV2CliConfig
  ] =
    PostgreSqlEnumerationSearchReplayBundleV1CliConfig
      .parse(arguments, environment, workingDirectory)
      .map { value =>
        PostgreSqlEnumerationSearchReplayBundleV2CliConfig(
          value.authoringRequest,
          value.outputBundle,
          value.pythonExecutable,
          value.composerRoot,
          value.schemaName,
          value.jdbc
        )
      }
}
