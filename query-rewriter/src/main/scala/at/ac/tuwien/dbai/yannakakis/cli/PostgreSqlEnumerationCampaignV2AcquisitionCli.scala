package at.ac.tuwien.dbai.yannakakis.cli

import java.io.{OutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchResult,
  PostgreSqlEnumerationBatchStatus
}

import scala.util.control.NonFatal

/** One stable machine-safe CLI failure line; caller values are never retained. */
private[cli] final case class PostgreSqlEnumerationCampaignV2CliFailure(
    code: String,
    exitCode: Int
) {
  require(code != null && code.nonEmpty, "CLI failure code must not be empty")
  require(
    exitCode != PostgreSqlRewriteExitCode.Rewritten,
    "CLI failure exit must be nonzero"
  )
}

private[cli] object PostgreSqlEnumerationCampaignV2CliFailure {
  import PostgreSqlEnumerationCampaignV2ComposerFailure.PathRole
  import PostgreSqlEnumerationCampaignV2FileFailure._

  val Internal: PostgreSqlEnumerationCampaignV2CliFailure =
    PostgreSqlEnumerationCampaignV2CliFailure(
      "acquisition-cli.internal-error",
      PostgreSqlRewriteExitCode.InternalError
    )

  def config(
      failure: PostgreSqlEnumerationCampaignV2CliConfigFailure
  ): PostgreSqlEnumerationCampaignV2CliFailure =
    PostgreSqlEnumerationCampaignV2CliFailure(
      failure.code,
      PostgreSqlRewriteExitCode.InvalidInvocation
    )

  def authoring(
      failure: PostgreSqlEnumerationCampaignV2AuthoringParseFailure
  ): PostgreSqlEnumerationCampaignV2CliFailure =
    PostgreSqlEnumerationCampaignV2CliFailure(
      failure.code,
      PostgreSqlRewriteExitCode.InputError
    )

  def backend(
      failure: PostgreSqlEnumerationCampaignV2BackendFailure
  ): PostgreSqlEnumerationCampaignV2CliFailure =
    PostgreSqlEnumerationCampaignV2CliFailure(
      failure.code,
      PostgreSqlRewriteExitCode.ServiceUnavailable
    )

  def acquisition(
      failure: PostgreSqlEnumerationCampaignV2AcquisitionFailure,
      result: PostgreSqlEnumerationBatchResult
  ): PostgreSqlEnumerationCampaignV2CliFailure = {
    val exit = failure match {
      case _: PostgreSqlEnumerationCampaignV2AcquisitionFailure.UnpackageableResult =>
        result.status match {
          case PostgreSqlEnumerationBatchStatus.NotApplicable =>
            PostgreSqlRewriteExitCode.NotApplicable
          case PostgreSqlEnumerationBatchStatus.InvalidQuery =>
            PostgreSqlRewriteExitCode.InvalidQuery
          case PostgreSqlEnumerationBatchStatus.Failed |
              PostgreSqlEnumerationBatchStatus.Produced =>
            PostgreSqlRewriteExitCode.RewriteFailed
        }
      case _: PostgreSqlEnumerationCampaignV2AcquisitionFailure.AuthoringMismatch |
          _: PostgreSqlEnumerationCampaignV2AcquisitionFailure.ProjectionRejected |
          _: PostgreSqlEnumerationCampaignV2AcquisitionFailure.AssemblyRejected =>
        PostgreSqlRewriteExitCode.RewriteFailed
    }
    PostgreSqlEnumerationCampaignV2CliFailure(failure.code, exit)
  }

  def files(
      failure: PostgreSqlEnumerationCampaignV2FileFailure
  ): PostgreSqlEnumerationCampaignV2CliFailure = {
    val exit = failure match {
      case MissingPath | AmbiguousPath | UnsafePathComponent |
          FileIdentityUnavailable | InputUnavailable | InputNotRegular |
          InputSizeInvalid | InputChanged | InputReadFailed |
          OutputParentUnavailable | OutputExists =>
        PostgreSqlRewriteExitCode.InputError
      case PosixPermissionsUnavailable =>
        PostgreSqlRewriteExitCode.ServiceUnavailable
      case _ => PostgreSqlRewriteExitCode.InternalError
    }
    PostgreSqlEnumerationCampaignV2CliFailure(failure.code, exit)
  }

  def composer(
      failure: PostgreSqlEnumerationCampaignV2ComposerFailure
  ): PostgreSqlEnumerationCampaignV2CliFailure = {
    import PostgreSqlEnumerationCampaignV2ComposerFailure._
    if (failure == NonZeroExit(2))
      return PostgreSqlEnumerationCampaignV2CliFailure(
        failure.code,
        PostgreSqlRewriteExitCode.RewriteFailed
      )
    val unavailable = failure match {
      case InvalidPath(PathRole.PythonExecutable) |
          InvalidPath(PathRole.ComposerRoot) |
          MissingBundleComponent(_) | InvalidBundleComponent(_) |
          BundleComponentTooLarge(_) | BundleHashMismatch(_) |
          BundleChangedDuringInvocation(_) | ProcessStartFailed |
          StandardInputCloseFailed | TimedOut => true
      case _ => false
    }
    PostgreSqlEnumerationCampaignV2CliFailure(
      failure.code,
      if (unavailable) PostgreSqlRewriteExitCode.ServiceUnavailable
      else PostgreSqlRewriteExitCode.InternalError
    )
  }
}

/** Testable adapter around the frozen Python subprocess boundary. */
private[cli] trait PostgreSqlEnumerationCampaignV2PackageComposer {
  def preflight(
      pythonExecutable: Path,
      composerRoot: Path
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit]

  def assemble(
      pythonExecutable: Path,
      composerRoot: Path,
      assemblyInput: Path,
      stagedOutput: Path
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit]

  def validate(
      pythonExecutable: Path,
      composerRoot: Path,
      stagedPackage: Path
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit]
}

private[cli] object LivePostgreSqlEnumerationCampaignV2PackageComposer
    extends PostgreSqlEnumerationCampaignV2PackageComposer {
  override def preflight(
      pythonExecutable: Path,
      composerRoot: Path
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] =
    PostgreSqlEnumerationCampaignV2ComposerRunner
      .preflight(pythonExecutable, composerRoot)
      .map(_ => ())

  override def assemble(
      pythonExecutable: Path,
      composerRoot: Path,
      assemblyInput: Path,
      stagedOutput: Path
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] =
    PostgreSqlEnumerationCampaignV2ComposerInvocation
      .create(pythonExecutable, composerRoot, assemblyInput, stagedOutput)
      .flatMap(PostgreSqlEnumerationCampaignV2ComposerRunner.assemble)
      .map(_ => ())

  override def validate(
      pythonExecutable: Path,
      composerRoot: Path,
      stagedPackage: Path
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] =
    PostgreSqlEnumerationCampaignV2ComposerValidationInvocation
      .create(pythonExecutable, composerRoot, stagedPackage)
      .flatMap(PostgreSqlEnumerationCampaignV2ComposerRunner.validate)
      .map(_ => ())
}

/**
  * Silent create-only producer for one outcome-free campaign-v2 package.
  *
  * This remains a separate fully-qualified entry point; it does not replace
  * the assembly's legacy default main class or the public rewrite CLI.
  */
object PostgreSqlEnumerationCampaignV2AcquisitionCli {
  import PostgreSqlEnumerationCampaignV2CliFailure._

  private val FailurePrefix = "enumeration-campaign-v2 acquisition error: "
  private[cli] val Entrypoint: String =
    "at.ac.tuwien.dbai.yannakakis.cli.PostgreSqlEnumerationCampaignV2AcquisitionCli"

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
        withProcessOutputDiscarded {
          execute(
            arguments,
            environment,
            workingDirectory,
            backend,
            composer
          )
        }
      } catch {
        case NonFatal(_) => Left(Internal)
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
    if (backend == null || composer == null) return Left(Internal)
    for {
      config <- PostgreSqlEnumerationCampaignV2CliConfig
        .parse(arguments, environment, workingDirectory)
        .left
        .map(PostgreSqlEnumerationCampaignV2CliFailure.config)
      loaded <- PostgreSqlEnumerationCampaignV2AcquisitionFiles
        .readAuthoring(config.authoringRequest)
        .left
        .map(files)
      authoringRequest <- PostgreSqlEnumerationCampaignV2AuthoringParser
        .parse(loaded.bytes)
        .left
        .map(authoring)
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
        .withMappedWorkspace[
          PostgreSqlEnumerationCampaignV2CliFailure,
          Unit
        ](
          config.outputPackage,
          files,
          Internal
        ) { workspace =>
          for {
            _ <- composer
              .preflight(config.pythonExecutable, config.composerRoot)
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.composer)
            result <- backend
              .acquire(config, authoringRequest.batchRequest)
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.backend)
            assembly <- PostgreSqlEnumerationCampaignV2Acquisition
              .assemble(authoringRequest, result)
              .left
              .map(acquisition(_, result))
            _ <- workspace
              .writeAssembly(assembly.document.bytes)
              .left
              .map(files)
            _ <- composer
              .assemble(
                config.pythonExecutable,
                config.composerRoot,
                workspace.assemblyPath,
                workspace.stagedPackagePath
              )
              .left
              .map(PostgreSqlEnumerationCampaignV2CliFailure.composer)
            _ <- workspace
              .adoptStagedPackage()
              .left
              .map(files)
            finalPackageBytes <- workspace
              .stagedPackageByteLength()
              .left
              .map(files)
            _ <- PostgreSqlEnumerationCampaignV2CumulativeBudget
              .finalizePackage(
                assembly.cumulativeEvidence,
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
            _ <- workspace.publish().left.map(files)
          } yield ()
        }
    } yield ()
  }

  private[cli] def withProcessOutputDiscarded[A](body: => A): A =
    withProcessOutputLock {
      val previousOutput = System.out
      val previousError = System.err
      val sink = new PrintStream(
        OutputStream.nullOutputStream(),
        true,
        StandardCharsets.UTF_8
      )
      try {
        System.setOut(sink)
        System.setErr(sink)
        body
      } finally {
        System.setOut(previousOutput)
        System.setErr(previousError)
        sink.close()
      }
    }

  /** Shared reentrant lock for tests that observe JVM-global stream identity. */
  private[cli] def withProcessOutputLock[A](body: => A): A =
    this.synchronized(body)

  private def writeFailure(
      output: OutputStream,
      failure: PostgreSqlEnumerationCampaignV2CliFailure
  ): Unit = {
    if (output == null) return
    try {
      output.write((FailurePrefix + failure.code + "\n").getBytes(StandardCharsets.UTF_8))
      output.flush()
    } catch {
      case NonFatal(_) => ()
    }
  }
}
