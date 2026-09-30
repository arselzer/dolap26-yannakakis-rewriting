package at.ac.tuwien.dbai.yannakakis.cli

import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import java.util.Arrays

import scala.util.control.NonFatal

/**
  * Additive one-shot result publisher for plan-capture v1.
  *
  * A successful backend capture is serialized by the unchanged v1 raw
  * assembly renderer. A typed backend WholePlanFailure is instead a successful
  * terminal publication of a canonical failure receipt. Invocation, input,
  * request-race, rendering, filesystem, and internal failures remain nonzero
  * operator faults and never become receipts.
  */
object PostgreSqlEnumerationPlanCaptureResultV1Cli {
  private val FailurePrefix = "enumeration-plan-capture-result-v1 error: "

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
      backend: PostgreSqlEnumerationPlanCaptureV1Backend
  ): Either[PostgreSqlEnumerationPlanCaptureV1CliFailure, Unit] = {
    if (backend == null) return Left(internal)
    for {
      config <- PostgreSqlEnumerationPlanCaptureV1CliConfig
        .parse(arguments, environment, workingDirectory)
        .left.map(problem =>
          failure(problem.code, PostgreSqlRewriteExitCode.InvalidInvocation)
        )
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
        .withMappedWorkspace[PostgreSqlEnumerationPlanCaptureV1CliFailure, Unit](
          config.outputAssembly,
          fileFailure,
          internal
        ) { workspace =>
          for {
            result <- Right(backend.capture(config, request.wholePlanRequest))
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
                  "plan-capture-result-v1-cli.request-changed",
                  PostgreSqlRewriteExitCode.InputError
                )
              )
            document <- result match {
              case Right(batch) =>
                PostgreSqlEnumerationPlanCaptureV1Assembly
                  .render(request, batch)
                  .left.map(problem =>
                    failure(problem.code, PostgreSqlRewriteExitCode.RewriteFailed)
                  )
              case Left(typedFailure) =>
                PostgreSqlEnumerationPlanCaptureFailureReceiptV1
                  .render(request, typedFailure)
                  .left.map(problem =>
                    failure(problem.code, PostgreSqlRewriteExitCode.RewriteFailed)
                  )
            }
            _ <- workspace.writeStagedPackage(document.bytes).left.map(fileFailure)
            _ <- workspace.publish().left.map(fileFailure)
          } yield ()
        }
    } yield ()
  }

  private def failure(code: String, exit: Int) =
    PostgreSqlEnumerationPlanCaptureV1CliFailure(code, exit)

  private val internal = failure(
    "plan-capture-result-v1-cli.internal-error",
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
