package at.ac.tuwien.dbai.yannakakis.cli

import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

import scala.util.control.NonFatal

/**
  * Silent create-only publisher for one fallback-capable replay-v2 bundle.
  *
  * The one-pass backend is invoked exactly once. One packageability decision
  * then feeds both the campaign-v2 package assembly and the output-free v2
  * search input before the fixed completion marker is linked last.
  */
object PostgreSqlEnumerationSearchReplayV2AcquisitionCli {
  private val FailurePrefix =
    "enumeration-search-replay-v2 acquisition error: "
  private[cli] val Entrypoint: String =
    "at.ac.tuwien.dbai.yannakakis.cli.PostgreSqlEnumerationSearchReplayV2AcquisitionCli"

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
  ): Either[PostgreSqlEnumerationCampaignV2CliFailure, Unit] =
    if (
      Option(arguments).flatMap(_.headOption).contains("acquire-disposition")
    )
      executeDisposition(
        arguments,
        environment,
        workingDirectory,
        backend,
        composer
      )
    else
      executeLegacy(
        arguments,
        environment,
        workingDirectory,
        backend,
        composer
      )

  /** Frozen legacy path. `acquire` keeps its exact parser and state machine. */
  private def executeLegacy(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path,
      backend: PostgreSqlEnumerationCampaignV2Backend,
      composer: PostgreSqlEnumerationCampaignV2PackageComposer
  ): Either[PostgreSqlEnumerationCampaignV2CliFailure, Unit] = {
    if (backend == null || composer == null)
      return Left(PostgreSqlEnumerationCampaignV2CliFailure.Internal)
    for {
      config <- PostgreSqlEnumerationSearchReplayBundleV2CliConfig
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
            pair <- PostgreSqlEnumerationSearchReplayBundleV2Acquisition
              .assemblePair(authoringRequest, result)
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
            _ <- assemblePackage(composer, config, workspace)
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

  /**
    * Additive complete-disposition path. The backend and the typed classifier
    * are each consumed once. Eligible results reuse the frozen three-child v2
    * publication; accepted terminal results publish only their sealed receipt.
    */
  private def executeDisposition(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path,
      backend: PostgreSqlEnumerationCampaignV2Backend,
      composer: PostgreSqlEnumerationCampaignV2PackageComposer
  ): Either[PostgreSqlEnumerationCampaignV2CliFailure, Unit] = {
    if (backend == null || composer == null)
      return Left(PostgreSqlEnumerationCampaignV2CliFailure.Internal)
    for {
      config <- PostgreSqlEnumerationAcquisitionDispositionCliConfig
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
          config.outputDisposition,
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
            disposition <- PostgreSqlEnumerationAcquisitionDispositionClassifier
              .classify(authoringRequest, result)
              .left
              .map(dispositionContractFailure)
            _ <- disposition match {
              case PostgreSqlEnumerationAcquisitionDisposition.Eligible(
                    eligible
                  ) =>
                publishEligibleDisposition(
                  authoringRequest,
                  eligible,
                  config,
                  workspace,
                  composer
                )
              case terminal: PostgreSqlEnumerationAcquisitionDisposition.Unpackageable =>
                publishTerminalDisposition(config.outputDisposition, terminal)
            }
          } yield ()
        }
    } yield ()
  }

  private def publishEligibleDisposition(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      eligible: PostgreSqlEnumerationCampaignV2EligibleResult,
      config: PostgreSqlEnumerationAcquisitionDispositionCliConfig,
      workspace: PostgreSqlEnumerationCampaignV2AcquisitionFiles.PairWorkspace,
      composer: PostgreSqlEnumerationCampaignV2PackageComposer
  ): Either[PostgreSqlEnumerationCampaignV2CliFailure, Unit] =
    for {
      pair <- PostgreSqlEnumerationSearchReplayBundleV2Acquisition
        .assembleEligiblePair(authoring, eligible)
        .left
        .map(dispositionEligibleFailure)
      _ <- workspace
        .writeAssembly(pair.packageAssembly.document.bytes)
        .left
        .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
      _ <- workspace
        .writeSearchInput(pair.searchInput.bytes)
        .left
        .map(PostgreSqlEnumerationCampaignV2CliFailure.files)
      _ <- assembleDispositionPackage(composer, config, workspace)
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
      complete <- buildComplete(authoring, pair, packageEvidence, searchEvidence)
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

  private def publishTerminalDisposition(
      output: Path,
      terminal: PostgreSqlEnumerationAcquisitionDisposition.Unpackageable
  ): Either[PostgreSqlEnumerationCampaignV2CliFailure, Unit] = {
    val receipt =
      try {
        Right(
          PostgreSqlEnumerationAcquisitionTerminalReceiptJson.build(terminal)
        )
      } catch {
        case _: IllegalArgumentException =>
          Left(
            PostgreSqlEnumerationCampaignV2CliFailure(
              "enumeration-acquisition-disposition.terminal-receipt-rejected",
              PostgreSqlRewriteExitCode.RewriteFailed
            )
          )
      }
    receipt.flatMap(document =>
      PostgreSqlEnumerationAcquisitionTerminalPublisher
        .publish(output, document)
        .left
        .map(dispositionFileFailure)
    )
  }

  private def assembleDispositionPackage(
      composer: PostgreSqlEnumerationCampaignV2PackageComposer,
      config: PostgreSqlEnumerationAcquisitionDispositionCliConfig,
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
          .flatMap(_ =>
            Left(PostgreSqlEnumerationCampaignV2CliFailure.composer(problem))
          )
    }

  private def dispositionContractFailure(
      failure: PostgreSqlEnumerationAcquisitionProducerContractFailure
  ): PostgreSqlEnumerationCampaignV2CliFailure =
    PostgreSqlEnumerationCampaignV2CliFailure(
      failure.code,
      PostgreSqlRewriteExitCode.RewriteFailed
    )

  private def dispositionEligibleFailure(
      failure: PostgreSqlEnumerationCampaignV2AcquisitionFailure
  ): PostgreSqlEnumerationCampaignV2CliFailure =
    PostgreSqlEnumerationCampaignV2CliFailure(
      failure.code,
      PostgreSqlRewriteExitCode.RewriteFailed
    )

  private def dispositionFileFailure(
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): PostgreSqlEnumerationCampaignV2CliFailure = {
    import PostgreSqlEnumerationAcquisitionTerminalFailure._
    val exitCode = failure match {
      case MissingPath | AmbiguousPath | UnsafePathComponent |
          FileIdentityUnavailable | OutputParentUnavailable | OutputExists =>
        PostgreSqlRewriteExitCode.InputError
      case PosixPermissionsUnavailable | DifferentFileSystem =>
        PostgreSqlRewriteExitCode.ServiceUnavailable
      case _ => PostgreSqlRewriteExitCode.InternalError
    }
    PostgreSqlEnumerationCampaignV2CliFailure(failure.code, exitCode)
  }

  private def assemblePackage(
      composer: PostgreSqlEnumerationCampaignV2PackageComposer,
      config: PostgreSqlEnumerationSearchReplayBundleV2CliConfig,
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
          .flatMap(_ =>
            Left(PostgreSqlEnumerationCampaignV2CliFailure.composer(problem))
          )
    }

  private def buildComplete(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      pair: PostgreSqlEnumerationSearchReplayBundleV2Pair,
      packageEvidence: PostgreSqlEnumerationSearchReplayBundleV1FileEvidence,
      searchEvidence: PostgreSqlEnumerationSearchReplayBundleV1FileEvidence
  ): Either[
    PostgreSqlEnumerationCampaignV2CliFailure,
    PostgreSqlEnumerationCampaignV2JsonDocument
  ] =
    try {
      Right(
        PostgreSqlEnumerationSearchReplayBundleV2Json.build(
          authoring,
          pair.eligible,
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
            "enumeration-search-replay-v2.complete-rejected",
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
