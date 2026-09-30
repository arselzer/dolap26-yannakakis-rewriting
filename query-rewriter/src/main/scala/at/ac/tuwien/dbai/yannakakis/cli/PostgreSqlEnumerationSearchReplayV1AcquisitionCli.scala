package at.ac.tuwien.dbai.yannakakis.cli

import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

import scala.util.control.NonFatal

/**
  * Silent create-only producer for one found-actions v2 package and its
  * independently replayable structural-search input.
  *
  * This dedicated entrypoint invokes the one-pass backend exactly once. Both
  * documents are staged and the package is composer-validated before a fixed
  * three-child bundle is made visible; `complete.json` is linked last as the
  * logical commit marker.
  */
object PostgreSqlEnumerationSearchReplayV1AcquisitionCli {
  private val FailurePrefix =
    "enumeration-search-replay-v1 acquisition error: "
  private[cli] val Entrypoint: String =
    "at.ac.tuwien.dbai.yannakakis.cli.PostgreSqlEnumerationSearchReplayV1AcquisitionCli"

  def main(arguments: Array[String]): Unit = {
    val terminalError = System.err
    val exitCode = run(
      arguments = arguments,
      environment = sys.env,
      workingDirectory = Paths.get("").toAbsolutePath.normalize(),
      standardError = terminalError,
      backend = LivePostgreSqlEnumerationCampaignV2Backend,
      composer = LivePostgreSqlEnumerationCampaignV2PackageComposer
    )
    if (exitCode != PostgreSqlRewriteExitCode.Rewritten) System.exit(exitCode)
  }

  private[cli] def run(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path,
      standardError: OutputStream,
      backend: PostgreSqlEnumerationCampaignV2Backend,
      composer: PostgreSqlEnumerationCampaignV2PackageComposer
  ): Int = {
    val outcome =
      try {
        PostgreSqlEnumerationCampaignV2AcquisitionCli.withProcessOutputDiscarded {
          execute(arguments, environment, workingDirectory, backend, composer)
        }
      } catch {
        case NonFatal(_) =>
          Left(PostgreSqlEnumerationCampaignV2CliFailure.Internal)
      }
    outcome match {
      case Right(()) => PostgreSqlRewriteExitCode.Rewritten
      case Left(problem) =>
        writeFailure(standardError, problem)
        problem.exitCode
    }
  }

  private def execute(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path,
      backend: PostgreSqlEnumerationCampaignV2Backend,
      composer: PostgreSqlEnumerationCampaignV2PackageComposer
  ): Either[PostgreSqlEnumerationCampaignV2CliFailure, Unit] = {
    if (backend == null || composer == null)
      return Left(PostgreSqlEnumerationCampaignV2CliFailure.Internal)
    for {
      config <- PostgreSqlEnumerationSearchReplayBundleV1CliConfig
        .parse(arguments, environment, workingDirectory)
        .left
        .map(PostgreSqlEnumerationCampaignV2CliFailure.config)
      loaded <- PostgreSqlEnumerationCampaignV2AcquisitionFiles
        .readAuthoring(config.authoringRequest)
        .left
        .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
      authoringRequest <- PostgreSqlEnumerationCampaignV2AuthoringParser
        .parse(loaded.bytes)
        .left
        .map(PostgreSqlEnumerationCampaignV2CliFailure.authoring)
      _ <-
        if (authoringRequest.hasProducerEntrypoint(Entrypoint)) Right(())
        else
          Left(
            PostgreSqlEnumerationCampaignV2CliFailure(
              "acquisition-cli.producer-entrypoint-mismatch",
              PostgreSqlRewriteExitCode.InputError
            )
          )
      _ <- PostgreSqlEnumerationCampaignV2AcquisitionFiles
        .withMappedPairWorkspace[
          PostgreSqlEnumerationCampaignV2CliFailure,
          Unit
        ](
          config.outputBundle,
          PostgreSqlEnumerationCampaignV2CliFailure.files,
          PostgreSqlEnumerationCampaignV2CliFailure.Internal
        ) { workspace =>
          for {
            _ <- composer
              .preflight(config.pythonExecutable, config.composerRoot)
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.composer)
            result <- backend
              .acquire(
                config.backendConfig(workspace.stagedPackagePath),
                authoringRequest.batchRequest
              )
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.backend)
            pair <- PostgreSqlEnumerationSearchReplayBundleV1Acquisition
              .assembleFoundPair(authoringRequest, result)
              .left
              .map(
                PostgreSqlEnumerationCampaignV2CliFailure.acquisition(_, result)
              )
            _ <- workspace
              .writeAssembly(pair.packageAssembly.document.bytes)
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
            _ <- workspace
              .writeSearchInput(pair.searchInput.bytes)
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
            _ <- assemblePackage(
              composer,
              config,
              workspace
            )
            _ <- workspace
              .adoptStagedPackage()
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
            finalPackageBytes <- workspace
              .stagedPackageByteLength()
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
            _ <- PostgreSqlEnumerationCampaignV2CumulativeBudget
              .finalizePackage(
                pair.packageAssembly.cumulativeEvidence,
                finalPackageBytes
              )
              .left
              .map(problem =>
                PostgreSqlEnumerationCampaignV2CliFailure(
                  problem.code,
                  PostgreSqlRewriteExitCode.RewriteFailed
                )
              )
            _ <- composer
              .validate(
                config.pythonExecutable,
                config.composerRoot,
                workspace.stagedPackagePath
              )
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.composer)
            packageEvidence <- workspace
              .stagedPackageEvidence()
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
            searchEvidence <- workspace
              .stagedSearchInputEvidence()
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
            complete <- buildComplete(
              authoringRequest,
              pair,
              packageEvidence,
              searchEvidence
            )
            _ <- PostgreSqlEnumerationCampaignV2CumulativeBudget
              .finalizeSearchReplayBundle(
                pair.packageAssembly.cumulativeEvidence,
                packageEvidence.byteLength,
                searchEvidence.byteLength,
                complete.byteLength.toLong
              )
              .left
              .map(problem =>
                PostgreSqlEnumerationCampaignV2CliFailure(
                  problem.code,
                  PostgreSqlRewriteExitCode.RewriteFailed
                )
              )
            _ <- workspace
              .writeComplete(complete.bytes)
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
            _ <- workspace
              .publish()
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
          } yield ()
        }
    } yield ()
  }

  private def assemblePackage(
      composer: PostgreSqlEnumerationCampaignV2PackageComposer,
      config: PostgreSqlEnumerationSearchReplayBundleV1CliConfig,
      workspace: PostgreSqlEnumerationCampaignV2AcquisitionFiles.PairWorkspace
  ): Either[PostgreSqlEnumerationCampaignV2CliFailure, Unit] =
    composer
      .assemble(
        config.pythonExecutable,
        config.composerRoot,
        workspace.assemblyPath,
        workspace.stagedPackagePath
      ) match {
      case Right(()) => Right(())
      case Left(problem) =>
        workspace
          .recordPartialStagedPackageForCleanup()
          .left
          .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
          .flatMap(_ => Left(PostgreSqlEnumerationCampaignV2CliFailure.composer(problem)))
    }

  private def buildComplete(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      pair: PostgreSqlEnumerationSearchReplayBundleV1Pair,
      packageEvidence: PostgreSqlEnumerationSearchReplayBundleV1FileEvidence,
      searchEvidence: PostgreSqlEnumerationSearchReplayBundleV1FileEvidence
  ): Either[
    PostgreSqlEnumerationCampaignV2CliFailure,
    PostgreSqlEnumerationCampaignV2JsonDocument
  ] =
    try {
      Right(
        PostgreSqlEnumerationSearchReplayBundleV1Json.build(
          authoring,
          pair.artifact,
          Entrypoint,
          packageEvidence,
          searchEvidence,
          pair.searchInput
        )
      )
    } catch {
      case NonFatal(_) =>
        Left(
          PostgreSqlEnumerationCampaignV2CliFailure(
            "enumeration-search-replay-v1.complete-rejected",
            PostgreSqlRewriteExitCode.RewriteFailed
          )
        )
    }

  private def writeFailure(
      output: OutputStream,
      failure: PostgreSqlEnumerationCampaignV2CliFailure
  ): Unit = {
    if (output == null) return
    try {
      output.write(
        (FailurePrefix + failure.code + "\n").getBytes(StandardCharsets.UTF_8)
      )
      output.flush()
    } catch {
      case NonFatal(_) => ()
    }
  }
}
