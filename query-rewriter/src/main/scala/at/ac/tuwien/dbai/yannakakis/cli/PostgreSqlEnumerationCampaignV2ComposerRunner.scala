package at.ac.tuwien.dbai.yannakakis.cli

import java.io.{IOException, InputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, LinkOption, OpenOption, Path, StandardOpenOption}
import java.security.MessageDigest
import java.time.Duration
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Callable, Executors, Future, ThreadFactory, TimeUnit, TimeoutException}

import scala.jdk.CollectionConverters._

/** The exact Python modules that constitute the frozen offline composer. */
private[yannakakis] sealed abstract class PostgreSqlEnumerationCampaignV2ComposerComponent(
    val fileName: String
) extends Product
    with Serializable

private[yannakakis] object PostgreSqlEnumerationCampaignV2ComposerComponent {
  case object PackageCli
      extends PostgreSqlEnumerationCampaignV2ComposerComponent("package_cli.py")
  case object PackageContract
      extends PostgreSqlEnumerationCampaignV2ComposerComponent(
        "package_contract.py"
      )
  case object ProducerEvidence
      extends PostgreSqlEnumerationCampaignV2ComposerComponent(
        "producer_evidence.py"
      )
  case object Protocol
      extends PostgreSqlEnumerationCampaignV2ComposerComponent("protocol.py")

  val All: Vector[PostgreSqlEnumerationCampaignV2ComposerComponent] = Vector(
    PackageCli,
    PackageContract,
    ProducerEvidence,
    Protocol
  )
}

/**
  * Exact digest inventory for the four-file offline composer.
  *
  * The closed constructor and sole frozen instance prevent an acquisition CLI
  * from replacing or omitting a component hash at runtime.
  */
private[yannakakis] final class PostgreSqlEnumerationCampaignV2ComposerManifest private (
    private val expected: Map[
      PostgreSqlEnumerationCampaignV2ComposerComponent,
      String
    ]
) extends Serializable {
  private[cli] def sha256(
      component: PostgreSqlEnumerationCampaignV2ComposerComponent
  ): String = expected(component)
}

private[yannakakis] object PostgreSqlEnumerationCampaignV2ComposerManifest {
  import PostgreSqlEnumerationCampaignV2ComposerComponent._

  val Production: PostgreSqlEnumerationCampaignV2ComposerManifest =
    new PostgreSqlEnumerationCampaignV2ComposerManifest(
      Map(
        PackageCli ->
          "35f598123fa7ca94db49f8a46627b8178d08489e32cc6272585abb19085978af",
        PackageContract ->
          "13581731c365ba0af8c7659127fb7c3173dfd372dcbf6042fe16fa3cf652ad95",
        ProducerEvidence ->
          "04aac0d9e0e8b531b03b465111298dc6dedf264011aee7e93dee5c8861178938",
        Protocol ->
          "ee236dde042963f4d74434517882e56185c97b91459d7cc2a0663393e32a66cd"
      )
    )
}

/** Stable runner failures. No branch contains child output or exception text. */
private[yannakakis] sealed trait PostgreSqlEnumerationCampaignV2ComposerFailure
    extends Product
    with Serializable {
  def code: String
}

private[yannakakis] object PostgreSqlEnumerationCampaignV2ComposerFailure {
  sealed trait PathRole extends Product with Serializable
  object PathRole {
    case object PythonExecutable extends PathRole
    case object ComposerRoot extends PathRole
    case object AssemblyInput extends PathRole
    case object StagedOutput extends PathRole
    case object StagedOutputParent extends PathRole
    case object StagedPackage extends PathRole
  }

  sealed trait OutputStreamRole extends Product with Serializable
  object OutputStreamRole {
    case object StandardOutput extends OutputStreamRole
    case object StandardError extends OutputStreamRole
  }

  final case class InvalidPath(role: PathRole)
      extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.invalid-path"
  }

  case object OutputAlreadyExists
      extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.output-already-exists"
  }

  final case class MissingBundleComponent(
      component: PostgreSqlEnumerationCampaignV2ComposerComponent
  ) extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.bundle-component-missing"
  }

  final case class InvalidBundleComponent(
      component: PostgreSqlEnumerationCampaignV2ComposerComponent
  ) extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.bundle-component-invalid"
  }

  final case class BundleComponentTooLarge(
      component: PostgreSqlEnumerationCampaignV2ComposerComponent
  ) extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.bundle-component-too-large"
  }

  final case class BundleHashMismatch(
      component: PostgreSqlEnumerationCampaignV2ComposerComponent
  ) extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.bundle-hash-mismatch"
  }

  final case class BundleChangedDuringInvocation(
      component: PostgreSqlEnumerationCampaignV2ComposerComponent
  ) extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.bundle-changed-during-invocation"
  }

  case object ProcessStartFailed
      extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.process-start-failed"
  }

  case object StandardInputCloseFailed
      extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.stdin-close-failed"
  }

  case object TimedOut extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.timed-out"
  }

  final case class OutputLimitExceeded(stream: OutputStreamRole)
      extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.output-limit-exceeded"
  }

  final case class NonSilentOutput(
      standardOutputPresent: Boolean,
      standardErrorPresent: Boolean
  ) extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    require(
      standardOutputPresent || standardErrorPresent,
      "non-silent output must identify at least one stream"
    )
    override val code: String = "composer.non-silent-output"
  }

  final case class NonZeroExit(exitCode: Int)
      extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.nonzero-exit"
  }

  case object StreamDrainFailed
      extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.stream-drain-failed"
  }

  case object MissingOutput
      extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.missing-output"
  }

  case object InvalidOutput
      extends PostgreSqlEnumerationCampaignV2ComposerFailure {
    override val code: String = "composer.invalid-output"
  }
}

/**
  * Validated paths for one create-only offline composition attempt.
  *
  * Every path is absolute and normalized. Existing inputs and all of their
  * path components are non-symlinks; the output itself is absent and its
  * existing parent chain is non-symlinked. The runner repeats the volatile
  * checks immediately before launch.
  */
private[yannakakis] final class PostgreSqlEnumerationCampaignV2ComposerInvocation private (
    val pythonExecutable: Path,
    val composerRoot: Path,
    val assemblyInput: Path,
    val stagedOutput: Path
)

private[yannakakis] object PostgreSqlEnumerationCampaignV2ComposerInvocation {
  import PostgreSqlEnumerationCampaignV2ComposerFailure._
  import PostgreSqlEnumerationCampaignV2ComposerFailure.PathRole._

  def create(
      pythonExecutable: Path,
      composerRoot: Path,
      assemblyInput: Path,
      stagedOutput: Path
  ): Either[
    PostgreSqlEnumerationCampaignV2ComposerFailure,
    PostgreSqlEnumerationCampaignV2ComposerInvocation
  ] = {
    val outputParent =
      Option(stagedOutput).flatMap(path => Option(path.getParent))
    if (!validRegularExecutable(pythonExecutable))
      Left(InvalidPath(PythonExecutable))
    else if (!validDirectory(composerRoot)) Left(InvalidPath(ComposerRoot))
    else if (!validRegularFile(assemblyInput)) Left(InvalidPath(AssemblyInput))
    else if (!absoluteNormalized(stagedOutput)) Left(InvalidPath(StagedOutput))
    else if (
      Files.exists(stagedOutput, LinkOption.NOFOLLOW_LINKS) ||
      Files.isSymbolicLink(stagedOutput)
    )
      Left(OutputAlreadyExists)
    else if (!outputParent.exists(validDirectory))
      Left(InvalidPath(StagedOutputParent))
    else
      Right(
        new PostgreSqlEnumerationCampaignV2ComposerInvocation(
          pythonExecutable,
          composerRoot,
          assemblyInput,
          stagedOutput
        )
      )
  }

  private[cli] def absoluteNormalized(path: Path): Boolean =
    path != null && path.isAbsolute && path == path.normalize()

  private[cli] def validRegularExecutable(path: Path): Boolean =
    validRegularFile(path) && Files.isExecutable(path)

  private[cli] def validRegularFile(path: Path): Boolean =
    absoluteNormalized(path) &&
      !hasSymbolicLinkComponent(path) &&
      Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)

  private[cli] def validDirectory(path: Path): Boolean =
    absoluteNormalized(path) &&
      !hasSymbolicLinkComponent(path) &&
      Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)

  private def hasSymbolicLinkComponent(path: Path): Boolean = {
    var current: Path = path
    while (current != null) {
      if (Files.isSymbolicLink(current)) return true
      current = current.getParent
    }
    false
  }
}

/** Validated paths for one silent offline package-validation attempt. */
private[yannakakis] final class PostgreSqlEnumerationCampaignV2ComposerValidationInvocation private (
    val pythonExecutable: Path,
    val composerRoot: Path,
    val stagedPackage: Path
)

private[yannakakis] object PostgreSqlEnumerationCampaignV2ComposerValidationInvocation {
  import PostgreSqlEnumerationCampaignV2ComposerFailure._
  import PostgreSqlEnumerationCampaignV2ComposerFailure.PathRole._

  def create(
      pythonExecutable: Path,
      composerRoot: Path,
      stagedPackage: Path
  ): Either[
    PostgreSqlEnumerationCampaignV2ComposerFailure,
    PostgreSqlEnumerationCampaignV2ComposerValidationInvocation
  ] =
    if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validRegularExecutable(pythonExecutable)
    ) Left(InvalidPath(PythonExecutable))
    else if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validDirectory(composerRoot)
    ) Left(InvalidPath(ComposerRoot))
    else if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validRegularFile(stagedPackage)
    ) Left(InvalidPath(StagedPackage))
    else
      Right(
        new PostgreSqlEnumerationCampaignV2ComposerValidationInvocation(
          pythonExecutable,
          composerRoot,
          stagedPackage
        )
      )
}

/** Successful completion means the pinned composer created one silent output. */
private[yannakakis] case object PostgreSqlEnumerationCampaignV2ComposerCompleted

/** Successful validation means the pinned composer silently accepted a package. */
private[yannakakis] case object PostgreSqlEnumerationCampaignV2ComposerValidated

/** Successful preflight means the executable and exact pinned bundle exist. */
private[yannakakis] case object PostgreSqlEnumerationCampaignV2ComposerReady

/**
  * Isolated subprocess boundary for the dependency-free Python composer.
  *
  * Production callers get one fixed process/output envelope. The package-
  * private testing seam changes only those time bounds; argv, environment,
  * hashing, silence, and create-only behavior are identical.
  */
private[yannakakis] object PostgreSqlEnumerationCampaignV2ComposerRunner {
  import PostgreSqlEnumerationCampaignV2ComposerComponent._
  import PostgreSqlEnumerationCampaignV2ComposerFailure._
  import PostgreSqlEnumerationCampaignV2ComposerFailure.OutputStreamRole._

  private val MaximumComponentBytes: Long = 16L * 1024L * 1024L
  private val BufferBytes: Int = 8192
  /**
    * Import only the four absolute, hash-checked source files.
    *
    * `-I` keeps the caller-supplied composer directory and current directory
    * off `sys.path`. Compiling the exact source bytes directly, rather than
    * using Python's source loader, also prevents adjacent cached bytecode from
    * replacing a frozen module. The modules are registered explicitly in
    * dependency order before their source is executed.
    */
  private[cli] val IsolatedBootstrap: String =
    "import sys,types\n" +
      "def _load(name,path):\n" +
      " with open(path,'rb') as source: data=source.read()\n" +
      " code=compile(data,path,'exec',dont_inherit=True)\n" +
      " module=types.ModuleType(name)\n" +
      " module.__file__=path\n" +
      " sys.modules[name]=module\n" +
      " exec(code,module.__dict__)\n" +
      "paths=sys.argv[1:5]\n" +
      "sys.argv=[paths[3]]+sys.argv[5:]\n" +
      "_load('protocol',paths[0])\n" +
      "_load('producer_evidence',paths[1])\n" +
      "_load('package_contract',paths[2])\n" +
      "_load('__main__',paths[3])\n"
  private val DefaultLimits = RunnerLimits(
    processTimeout = Duration.ofSeconds(60),
    terminationTimeout = Duration.ofSeconds(2),
    drainTimeout = Duration.ofSeconds(2),
    maximumOutputBytesPerStream = 4096L
  )

  def preflight(
      pythonExecutable: Path,
      composerRoot: Path
  ): Either[
    PostgreSqlEnumerationCampaignV2ComposerFailure,
    PostgreSqlEnumerationCampaignV2ComposerReady.type
  ] = {
    import PostgreSqlEnumerationCampaignV2ComposerFailure.PathRole._
    if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validRegularExecutable(pythonExecutable)
    ) Left(InvalidPath(PythonExecutable))
    else if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validDirectory(composerRoot)
    ) Left(InvalidPath(ComposerRoot))
    else
      validateBundle(composerRoot, changed = false).map(_ =>
        PostgreSqlEnumerationCampaignV2ComposerReady
      )
  }

  def assemble(
      invocation: PostgreSqlEnumerationCampaignV2ComposerInvocation
  ): Either[
    PostgreSqlEnumerationCampaignV2ComposerFailure,
    PostgreSqlEnumerationCampaignV2ComposerCompleted.type
  ] =
    assembleBounded(invocation, DefaultLimits).map(_ =>
      PostgreSqlEnumerationCampaignV2ComposerCompleted
    )

  def validate(
      invocation: PostgreSqlEnumerationCampaignV2ComposerValidationInvocation
  ): Either[
    PostgreSqlEnumerationCampaignV2ComposerFailure,
    PostgreSqlEnumerationCampaignV2ComposerValidated.type
  ] =
    validateBounded(invocation, DefaultLimits).map(_ =>
      PostgreSqlEnumerationCampaignV2ComposerValidated
    )

  private[cli] def assembleForTesting(
      invocation: PostgreSqlEnumerationCampaignV2ComposerInvocation,
      processTimeout: Duration,
      terminationTimeout: Duration,
      drainTimeout: Duration,
      maximumOutputBytesPerStream: Long
  ): Either[
    PostgreSqlEnumerationCampaignV2ComposerFailure,
    PostgreSqlEnumerationCampaignV2ComposerCompleted.type
  ] =
    assembleBounded(
      invocation,
      RunnerLimits(
        processTimeout,
        terminationTimeout,
        drainTimeout,
        maximumOutputBytesPerStream
      )
    ).map(_ => PostgreSqlEnumerationCampaignV2ComposerCompleted)

  private[cli] def validateForTesting(
      invocation: PostgreSqlEnumerationCampaignV2ComposerValidationInvocation,
      processTimeout: Duration,
      terminationTimeout: Duration,
      drainTimeout: Duration,
      maximumOutputBytesPerStream: Long
  ): Either[
    PostgreSqlEnumerationCampaignV2ComposerFailure,
    PostgreSqlEnumerationCampaignV2ComposerValidated.type
  ] =
    validateBounded(
      invocation,
      RunnerLimits(
        processTimeout,
        terminationTimeout,
        drainTimeout,
        maximumOutputBytesPerStream
      )
    ).map(_ => PostgreSqlEnumerationCampaignV2ComposerValidated)

  private final case class RunnerLimits(
      processTimeout: Duration,
      terminationTimeout: Duration,
      drainTimeout: Duration,
      maximumOutputBytesPerStream: Long
  ) {
    require(
      processTimeout != null && !processTimeout.isNegative &&
        !processTimeout.isZero,
      "composer process timeout must be positive"
    )
    require(
      terminationTimeout != null && !terminationTimeout.isNegative &&
        !terminationTimeout.isZero,
      "composer termination timeout must be positive"
    )
    require(
      drainTimeout != null && !drainTimeout.isNegative &&
        !drainTimeout.isZero,
      "composer drain timeout must be positive"
    )
    require(
      maximumOutputBytesPerStream >= 0L,
      "composer output bound must be non-negative"
    )
  }

  private final case class DrainResult(byteCount: Long, exceeded: Boolean)

  private def assembleBounded(
      invocation: PostgreSqlEnumerationCampaignV2ComposerInvocation,
      limits: RunnerLimits
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] = {
    if (invocation == null) Left(ProcessStartFailed)
    else
      revalidateInvocation(invocation).flatMap { _ =>
        validateBundle(invocation.composerRoot, changed = false).flatMap { _ =>
          invoke(
            invocation.pythonExecutable,
            invocation.composerRoot,
            Vector(
              "assemble",
              "--input",
              invocation.assemblyInput.toString,
              "--output",
              invocation.stagedOutput.toString
            ),
            limits,
            validateCreatedOutput(invocation.stagedOutput)
          )
        }
      }
  }

  private def validateBounded(
      invocation: PostgreSqlEnumerationCampaignV2ComposerValidationInvocation,
      limits: RunnerLimits
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] = {
    if (invocation == null) Left(ProcessStartFailed)
    else
      revalidateValidationInvocation(invocation).flatMap { _ =>
        validateBundle(invocation.composerRoot, changed = false).flatMap { _ =>
          invoke(
            invocation.pythonExecutable,
            invocation.composerRoot,
            Vector("validate", invocation.stagedPackage.toString),
            limits,
            Right(())
          )
        }
      }
  }

  private def revalidateInvocation(
      invocation: PostgreSqlEnumerationCampaignV2ComposerInvocation
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] = {
    import PostgreSqlEnumerationCampaignV2ComposerFailure.PathRole._
    val outputParent = Option(invocation.stagedOutput.getParent)
    if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validRegularExecutable(invocation.pythonExecutable)
    )
      Left(InvalidPath(PythonExecutable))
    else if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validDirectory(invocation.composerRoot)
    )
      Left(InvalidPath(ComposerRoot))
    else if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validRegularFile(invocation.assemblyInput)
    )
      Left(InvalidPath(AssemblyInput))
    else if (
      Files.exists(invocation.stagedOutput, LinkOption.NOFOLLOW_LINKS) ||
      Files.isSymbolicLink(invocation.stagedOutput)
    )
      Left(OutputAlreadyExists)
    else if (
      !outputParent.exists(
        PostgreSqlEnumerationCampaignV2ComposerInvocation.validDirectory
      )
    )
      Left(InvalidPath(StagedOutputParent))
    else Right(())
  }

  private def revalidateValidationInvocation(
      invocation: PostgreSqlEnumerationCampaignV2ComposerValidationInvocation
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] = {
    import PostgreSqlEnumerationCampaignV2ComposerFailure.PathRole._
    if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validRegularExecutable(invocation.pythonExecutable)
    ) Left(InvalidPath(PythonExecutable))
    else if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validDirectory(invocation.composerRoot)
    ) Left(InvalidPath(ComposerRoot))
    else if (
      !PostgreSqlEnumerationCampaignV2ComposerInvocation
        .validRegularFile(invocation.stagedPackage)
    ) Left(InvalidPath(StagedPackage))
    else Right(())
  }

  private def validateBundle(
      root: Path,
      changed: Boolean
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] = {
    All.foldLeft[Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit]](
      Right(())
    ) { (result, component) =>
      result.flatMap { _ =>
        digestComponent(root.resolve(component.fileName), component).flatMap {
          digest =>
            if (
              MessageDigest.isEqual(
                digest,
                hexToBytes(
                  PostgreSqlEnumerationCampaignV2ComposerManifest.Production
                    .sha256(component)
                )
              )
            )
              Right(())
            else if (changed) Left(BundleChangedDuringInvocation(component))
            else Left(BundleHashMismatch(component))
        }
      }
    }
  }

  private def digestComponent(
      path: Path,
      component: PostgreSqlEnumerationCampaignV2ComposerComponent
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Array[Byte]] = {
    if (!Files.exists(path, LinkOption.NOFOLLOW_LINKS))
      Left(MissingBundleComponent(component))
    else if (
      Files.isSymbolicLink(path) ||
      !Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)
    ) Left(InvalidBundleComponent(component))
    else
      try {
        val before = Files.readAttributes(
          path,
          classOf[BasicFileAttributes],
          LinkOption.NOFOLLOW_LINKS
        )
        if (before.size() > MaximumComponentBytes)
          Left(BundleComponentTooLarge(component))
        else {
          val digest = MessageDigest.getInstance("SHA-256")
          val options = Set[OpenOption](
            StandardOpenOption.READ,
            LinkOption.NOFOLLOW_LINKS
          ).asJava
          val channel = FileChannel.open(path, options)
          var total = 0L
          try {
            val buffer = ByteBuffer.allocate(BufferBytes)
            var done = false
            while (!done) {
              buffer.clear()
              val read = channel.read(buffer)
              if (read < 0) done = true
              else if (read > 0) {
                total += read.toLong
                if (total > MaximumComponentBytes)
                  return Left(BundleComponentTooLarge(component))
                buffer.flip()
                digest.update(buffer)
              }
            }
          } finally channel.close()
          val after = Files.readAttributes(
            path,
            classOf[BasicFileAttributes],
            LinkOption.NOFOLLOW_LINKS
          )
          if (
            !after.isRegularFile || before.size() != after.size() ||
            before.lastModifiedTime() != after.lastModifiedTime() ||
            before.fileKey() != after.fileKey()
          ) Left(InvalidBundleComponent(component))
          else Right(digest.digest())
        }
      } catch {
        case _: IOException | _: SecurityException =>
          Left(InvalidBundleComponent(component))
      }
  }

  private def invoke(
      pythonExecutable: Path,
      composerRoot: Path,
      composerArguments: Vector[String],
      limits: RunnerLimits,
      validateSuccess: => Either[
        PostgreSqlEnumerationCampaignV2ComposerFailure,
        Unit
      ]
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] = {
    val protocol = composerRoot.resolve(Protocol.fileName)
    val producerEvidence = composerRoot.resolve(ProducerEvidence.fileName)
    val packageContract = composerRoot.resolve(PackageContract.fileName)
    val packageCli = composerRoot.resolve(PackageCli.fileName)
    val command = Vector(
      pythonExecutable.toString,
      "-I",
      "-E",
      "-s",
      "-S",
      "-B",
      "-X",
      "utf8",
      "-c",
      IsolatedBootstrap,
      protocol.toString,
      producerEvidence.toString,
      packageContract.toString,
      packageCli.toString
    ) ++ composerArguments
    val process =
      try {
        val builder = new ProcessBuilder(command.asJava)
        builder.directory(composerRoot.toFile)
        // The composer needs no ambient environment. Clearing it is stronger
        // than trying to enumerate JDBC, libpq, Python, cloud, and generic
        // credential spellings; the executable and paths are already absolute.
        builder.environment().clear()
        builder.start()
      }
      catch {
        case _: IOException | _: SecurityException =>
          return Left(ProcessStartFailed)
      }

    try process.getOutputStream.close()
    catch {
      case _: IOException =>
        terminate(process, limits.terminationTimeout)
        return Left(StandardInputCloseFailed)
    }

    val standardOutputExceeded = new AtomicBoolean(false)
    val standardErrorExceeded = new AtomicBoolean(false)
    val executor = Executors.newFixedThreadPool(2, daemonThreadFactory)
    val stdout = executor.submit(
      drainTask(
        process.getInputStream,
        limits.maximumOutputBytesPerStream,
        standardOutputExceeded,
        process
      )
    )
    val stderr = executor.submit(
      drainTask(
        process.getErrorStream,
        limits.maximumOutputBytesPerStream,
        standardErrorExceeded,
        process
      )
    )
    executor.shutdown()

    val exited =
      try process.waitFor(limits.processTimeout.toMillis, TimeUnit.MILLISECONDS)
      catch {
        case _: InterruptedException =>
          Thread.currentThread.interrupt()
          false
      }
    if (!exited) {
      terminate(process, limits.terminationTimeout)
      closeQuietly(process.getInputStream)
      closeQuietly(process.getErrorStream)
      cancel(stdout)
      cancel(stderr)
      Left(TimedOut)
    } else if (standardOutputExceeded.get() || standardErrorExceeded.get()) {
      closeQuietly(process.getInputStream)
      closeQuietly(process.getErrorStream)
      cancel(stdout)
      cancel(stderr)
      if (standardOutputExceeded.get())
        Left(OutputLimitExceeded(StandardOutput))
      else Left(OutputLimitExceeded(StandardError))
    } else {
      val drained =
        for {
          standardOutput <- awaitDrain(stdout, limits.drainTimeout)
          standardError <- awaitDrain(stderr, limits.drainTimeout)
        } yield standardOutput -> standardError
      drained.flatMap { case (standardOutput, standardError) =>
        if (standardOutput.exceeded)
          Left(OutputLimitExceeded(StandardOutput))
        else if (standardError.exceeded)
          Left(OutputLimitExceeded(StandardError))
        else if (process.exitValue() != 0) Left(NonZeroExit(process.exitValue()))
        else if (standardOutput.byteCount > 0L || standardError.byteCount > 0L)
          Left(
            NonSilentOutput(
              standardOutputPresent = standardOutput.byteCount > 0L,
              standardErrorPresent = standardError.byteCount > 0L
            )
          )
        else
          validateBundle(composerRoot, changed = true)
            .flatMap { _ => validateSuccess }
      }
    }
  }

  private def drainTask(
      input: InputStream,
      maximumBytes: Long,
      exceededSignal: AtomicBoolean,
      process: Process
  ): Callable[DrainResult] = () => {
    val buffer = new Array[Byte](BufferBytes)
    var total = 0L
    var exceeded = false
    var done = false
    while (!done) {
      val read = input.read(buffer)
      if (read < 0) done = true
      else if (read > 0) {
        total = saturatingAdd(total, read.toLong)
        if (!exceeded && total > maximumBytes) {
          exceeded = true
          exceededSignal.set(true)
          destroyProcessTree(process)
        }
      }
    }
    DrainResult(total, exceeded)
  }

  private def awaitDrain(
      future: Future[DrainResult],
      timeout: Duration
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, DrainResult] =
    try Right(future.get(timeout.toMillis, TimeUnit.MILLISECONDS))
    catch {
      case _: TimeoutException =>
        cancel(future)
        Left(StreamDrainFailed)
      case _: InterruptedException =>
        Thread.currentThread.interrupt()
        cancel(future)
        Left(StreamDrainFailed)
      case _: Exception => Left(StreamDrainFailed)
    }

  private def validateCreatedOutput(
      path: Path
  ): Either[PostgreSqlEnumerationCampaignV2ComposerFailure, Unit] =
    if (!Files.exists(path, LinkOption.NOFOLLOW_LINKS)) Left(MissingOutput)
    else if (
      Files.isSymbolicLink(path) ||
      !Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)
    ) Left(InvalidOutput)
    else Right(())

  private def terminate(process: Process, timeout: Duration): Unit = {
    destroyProcessTree(process)
    try process.waitFor(timeout.toMillis, TimeUnit.MILLISECONDS)
    catch {
      case _: InterruptedException => Thread.currentThread.interrupt()
    }
    ()
  }

  private def destroyProcessTree(process: Process): Unit = {
    try {
      val descendants = process.descendants()
      try
        descendants
          .iterator()
          .asScala
          .toVector
          .reverse
          .foreach(handle => handle.destroyForcibly())
      finally descendants.close()
    } catch {
      case _: SecurityException | _: UnsupportedOperationException => ()
    }
    try process.destroyForcibly()
    catch {
      case _: SecurityException | _: UnsupportedOperationException => ()
    }
    ()
  }

  private def closeQuietly(input: InputStream): Unit =
    try input.close()
    catch { case _: IOException => () }

  private def cancel(future: Future[_]): Unit = {
    future.cancel(true)
    ()
  }

  private def saturatingAdd(left: Long, right: Long): Long =
    if (Long.MaxValue - left < right) Long.MaxValue else left + right

  private def hexToBytes(value: String): Array[Byte] = {
    val bytes = new Array[Byte](value.length / 2)
    var index = 0
    while (index < bytes.length) {
      bytes(index) = Integer.parseInt(
        value.substring(index * 2, index * 2 + 2),
        16
      ).toByte
      index += 1
    }
    bytes
  }

  private val daemonThreadFactory: ThreadFactory = new ThreadFactory {
    private var nextId = 0
    override def newThread(runnable: Runnable): Thread = synchronized {
      nextId += 1
      val thread = new Thread(
        runnable,
        s"enumeration-campaign-v2-composer-drain-$nextId"
      )
      thread.setDaemon(true)
      thread
    }
  }
}
