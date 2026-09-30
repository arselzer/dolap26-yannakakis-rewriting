package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{Files, Path, StandardOpenOption}
import java.security.MessageDigest
import java.time.Duration

import scala.jdk.CollectionConverters._

class PostgreSqlEnumerationCampaignV2ComposerRunnerSuite
    extends munit.FunSuite {
  import PostgreSqlEnumerationCampaignV2ComposerComponent._
  import PostgreSqlEnumerationCampaignV2ComposerFailure._
  import PostgreSqlEnumerationCampaignV2ComposerFailure.OutputStreamRole._

  private final class RunnerFixture(
      val composerRoot: Path,
      val pythonExecutable: Path,
      val assemblyInput: Path,
      val stagedOutput: Path
  )

  private lazy val checkedComposerSource: Path =
    Path
      .of(
        "..",
        "benchmarks",
        "cyclic-postgresql",
        "enumeration-campaign-v2"
      )
      .toAbsolutePath
      .normalize()

  test("production manifest exactly pins the checked four-file composer") {
    val manifest = PostgreSqlEnumerationCampaignV2ComposerManifest.Production
    All.foreach { component =>
      assertEquals(
        sha256(Files.readAllBytes(checkedComposerSource.resolve(component.fileName))),
        manifest.sha256(component)
      )
    }
  }

  test(
    "runner uses exact argv tokens, clears ambient state, and accepts silent create-only success"
  ) {
    withFixture { directory =>
      val hostileSegment = "--literal $(touch injected); quote' space"
      val marker = directory.resolve("injected")
      val assembly = directory.resolve(hostileSegment + ".assembly.json")
      val output = directory.resolve(hostileSegment + ".package.json")
      Files.write(
        assembly,
        "{}\n".getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE_NEW
      )
      val composer = directory.resolve("composer root;literal")
      Files.createDirectory(composer)
      writeBundle(composer)
      val packageCli = composer.resolve(PackageCli.fileName)
      val script = exactSuccessScript(packageCli, assembly, output)
      val python = writeExecutable(
        directory.resolve("--python $() ; literal.sh"),
        script
      )
      val invocation = invocationOf(python, composer, assembly, output)

      val result =
        PostgreSqlEnumerationCampaignV2ComposerRunner.assemble(invocation)

      assertEquals(result, Right(PostgreSqlEnumerationCampaignV2ComposerCompleted))
      assert(Files.isRegularFile(output))
      assert(!Files.exists(marker))
    }
  }

  test(
    "validation uses exact argv tokens, clears ambient state, closes stdin, and stays silent"
  ) {
    withFixture { directory =>
      val hostileSegment = "--package $(touch injected); quote' space"
      val marker = directory.resolve("injected")
      val stagedPackage = directory.resolve(hostileSegment + ".json")
      Files.write(
        stagedPackage,
        "{}\n".getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE_NEW
      )
      val composer = directory.resolve("composer root;literal")
      Files.createDirectory(composer)
      writeBundle(composer)
      val packageCli = composer.resolve(PackageCli.fileName)
      val python = writeExecutable(
        directory.resolve("--python validate $() ; literal.sh"),
        exactValidationScript(packageCli, stagedPackage)
      )
      val invocation = validationInvocationOf(
        python,
        composer,
        stagedPackage
      )

      assertEquals(
        PostgreSqlEnumerationCampaignV2ComposerRunner.validate(invocation),
        Right(PostgreSqlEnumerationCampaignV2ComposerValidated)
      )
      assert(!Files.exists(marker))
    }
  }

  test("validation retains closed nonzero and output failures") {
    withFixture { directory =>
      val composer = directory.resolve("composer")
      Files.createDirectory(composer)
      writeBundle(composer)
      val stagedPackage = directory.resolve("package.json")
      Files.write(
        stagedPackage,
        "{}\n".getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE_NEW
      )
      val silentNonzero = writeExecutable(
        directory.resolve("validate-nonzero"),
        "#!/bin/sh\nexit 23\n"
      )
      assertEquals(
        runValidationWithTestLimits(
          validationInvocationOf(silentNonzero, composer, stagedPackage)
        ),
        Left(NonZeroExit(23))
      )

      val diagnosticNonzero = writeExecutable(
        directory.resolve("validate-diagnostic-nonzero"),
        "#!/bin/sh\nprintf 'private diagnostic' >&2\nexit 2\n"
      )
      assertEquals(
        runValidationWithTestLimits(
          validationInvocationOf(diagnosticNonzero, composer, stagedPackage)
        ),
        Left(NonZeroExit(2))
      )

      val noisy = writeExecutable(
        directory.resolve("validate-noisy"),
        "#!/bin/sh\nprintf private >&2\nexit 0\n"
      )
      assertEquals(
        runValidationWithTestLimits(
          validationInvocationOf(noisy, composer, stagedPackage)
        ),
        Left(
          NonSilentOutput(
            standardOutputPresent = false,
            standardErrorPresent = true
          )
        )
      )
    }
  }

  test("runner rejects a bundle hash mismatch before starting the child") {
    withFixture { directory =>
      val fixture = basicFixture(directory, "exit 93")
      Files.write(
        fixture.composerRoot.resolve(Protocol.fileName),
        "mutation\n".getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.TRUNCATE_EXISTING
      )
      val invocation = invocationOf(
        fixture.pythonExecutable,
        fixture.composerRoot,
        fixture.assemblyInput,
        fixture.stagedOutput
      )

      assertEquals(
        PostgreSqlEnumerationCampaignV2ComposerRunner.assemble(invocation),
        Left(BundleHashMismatch(Protocol))
      )
      assert(!Files.exists(fixture.stagedOutput))
    }
  }

  test("runner rejects symlinked bundle components") {
    withFixture { directory =>
      val fixture = basicFixture(directory, "exit 93")
      val protocol = fixture.composerRoot.resolve(Protocol.fileName)
      val target = directory.resolve("protocol-target.py")
      Files.move(protocol, target)
      Files.createSymbolicLink(protocol, target)
      val invocation = invocationOf(
        fixture.pythonExecutable,
        fixture.composerRoot,
        fixture.assemblyInput,
        fixture.stagedOutput
      )

      assertEquals(
        PostgreSqlEnumerationCampaignV2ComposerRunner.assemble(invocation),
        Left(InvalidBundleComponent(Protocol))
      )
    }
  }

  test("runner detects a composer component changed by the child") {
    withFixture { directory =>
      val composer = directory.resolve("composer")
      Files.createDirectory(composer)
      writeBundle(composer)
      val assembly = directory.resolve("assembly.json")
      Files.write(
        assembly,
        "{}\n".getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE_NEW
      )
      val output = directory.resolve("package.json")
      val protocol = composer.resolve(Protocol.fileName)
      val python = writeExecutable(
        directory.resolve("fake-python"),
        s"""#!/bin/sh
           |printf mutation > ${shellQuote(protocol.toString)}
           |: > "$${18}"
           |""".stripMargin
      )
      val invocation = invocationOf(python, composer, assembly, output)

      assertEquals(
        PostgreSqlEnumerationCampaignV2ComposerRunner.assemble(invocation),
        Left(BundleChangedDuringInvocation(Protocol))
      )
    }
  }

  test("runner reports completely silent nonzero exit without child diagnostics") {
    withFixture { directory =>
      val fixture = basicFixture(directory, "exit 17")
      assertEquals(
        runWithTestLimits(fixture),
        Left(NonZeroExit(17))
      )
    }
  }

  test("runner rejects any stdout or stderr without retaining raw bytes") {
    withFixture { directory =>
      val fixture = basicFixture(
        directory,
        "printf 'private stdout'; printf 'private stderr' >&2; exit 0"
      )
      assertEquals(
        runWithTestLimits(fixture),
        Left(
          NonSilentOutput(
            standardOutputPresent = true,
            standardErrorPresent = true
          )
        )
      )
    }
  }

  test("runner bounds and concurrently drains child output") {
    withFixture { directory =>
      val fixture = basicFixture(
        directory,
        "i=0; while [ \"$i\" -lt 4096 ]; do printf x; printf y >&2; i=$((i + 1)); done; exit 0"
      )
      val result = runWithTestLimits(fixture, maximumOutputBytes = 32L)
      result match {
        case Left(OutputLimitExceeded(StandardOutput | StandardError)) => ()
        case other => fail(s"unexpected bounded-output result: $other")
      }
    }
  }

  test("runner forcibly terminates a child at the fixed process deadline") {
    withFixture { directory =>
      val fixture = basicFixture(directory, "/bin/sleep 30")
      val started = System.nanoTime()
      val result = runWithTestLimits(
        fixture,
        processTimeout = Duration.ofMillis(150),
        terminationTimeout = Duration.ofSeconds(1),
        drainTimeout = Duration.ofSeconds(1)
      )
      val elapsedMillis = (System.nanoTime() - started) / 1000000L

      assertEquals(result, Left(TimedOut))
      assert(elapsedMillis < 5000L)
    }
  }

  test("successful silent exit must create the staged output") {
    withFixture { directory =>
      val fixture = basicFixture(directory, "exit 0")
      assertEquals(runWithTestLimits(fixture), Left(MissingOutput))
    }
  }

  test("validated invocation refuses an existing or dangling-symlink output") {
    withFixture { directory =>
      val fixture = basicFixture(directory, "exit 0")
      Files.write(
        fixture.stagedOutput,
        Array.emptyByteArray,
        StandardOpenOption.CREATE_NEW
      )
      assertEquals(
        PostgreSqlEnumerationCampaignV2ComposerInvocation.create(
          fixture.pythonExecutable,
          fixture.composerRoot,
          fixture.assemblyInput,
          fixture.stagedOutput
        ),
        Left(OutputAlreadyExists)
      )
      Files.delete(fixture.stagedOutput)
      Files.createSymbolicLink(
        fixture.stagedOutput,
        directory.resolve("missing-target")
      )
      assertEquals(
        PostgreSqlEnumerationCampaignV2ComposerInvocation.create(
          fixture.pythonExecutable,
          fixture.composerRoot,
          fixture.assemblyInput,
          fixture.stagedOutput
        ),
        Left(OutputAlreadyExists)
      )
    }
  }

  private def basicFixture(directory: Path, body: String): RunnerFixture = {
    val composer = directory.resolve("composer")
    Files.createDirectory(composer)
    writeBundle(composer)
    val assembly = directory.resolve("assembly.json")
    Files.write(
      assembly,
      "{}\n".getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE_NEW
    )
    val output = directory.resolve("package.json")
    val python = writeExecutable(
      directory.resolve("fake-python"),
      "#!/bin/sh\n" + body + "\n"
    )
    new RunnerFixture(composer, python, assembly, output)
  }

  private def writeBundle(composerRoot: Path): Unit =
    All.foreach { component =>
      Files.copy(
        checkedComposerSource.resolve(component.fileName),
        composerRoot.resolve(component.fileName)
      )
    }

  private def invocationOf(
      pythonExecutable: Path,
      composerRoot: Path,
      assemblyInput: Path,
      stagedOutput: Path
  ): PostgreSqlEnumerationCampaignV2ComposerInvocation =
    PostgreSqlEnumerationCampaignV2ComposerInvocation
      .create(
        pythonExecutable,
        composerRoot,
        assemblyInput,
        stagedOutput
      )
      .fold(problem => fail(problem.code), identity)

  private def validationInvocationOf(
      pythonExecutable: Path,
      composerRoot: Path,
      stagedPackage: Path
  ): PostgreSqlEnumerationCampaignV2ComposerValidationInvocation =
    PostgreSqlEnumerationCampaignV2ComposerValidationInvocation
      .create(
        pythonExecutable,
        composerRoot,
        stagedPackage
      )
      .fold(problem => fail(problem.code), identity)

  private def runWithTestLimits(
      fixture: RunnerFixture,
      processTimeout: Duration = Duration.ofSeconds(3),
      terminationTimeout: Duration = Duration.ofSeconds(1),
      drainTimeout: Duration = Duration.ofSeconds(1),
      maximumOutputBytes: Long = 4096L
  ): Either[
    PostgreSqlEnumerationCampaignV2ComposerFailure,
    PostgreSqlEnumerationCampaignV2ComposerCompleted.type
  ] =
    PostgreSqlEnumerationCampaignV2ComposerRunner.assembleForTesting(
      invocationOf(
        fixture.pythonExecutable,
        fixture.composerRoot,
        fixture.assemblyInput,
        fixture.stagedOutput
      ),
      processTimeout,
      terminationTimeout,
      drainTimeout,
      maximumOutputBytes
    )

  private def runValidationWithTestLimits(
      invocation: PostgreSqlEnumerationCampaignV2ComposerValidationInvocation,
      processTimeout: Duration = Duration.ofSeconds(3),
      terminationTimeout: Duration = Duration.ofSeconds(1),
      drainTimeout: Duration = Duration.ofSeconds(1),
      maximumOutputBytes: Long = 4096L
  ): Either[
    PostgreSqlEnumerationCampaignV2ComposerFailure,
    PostgreSqlEnumerationCampaignV2ComposerValidated.type
  ] =
    PostgreSqlEnumerationCampaignV2ComposerRunner.validateForTesting(
      invocation,
      processTimeout,
      terminationTimeout,
      drainTimeout,
      maximumOutputBytes
    )

  private def exactSuccessScript(
      packageCli: Path,
      assemblyInput: Path,
      stagedOutput: Path
  ): String = {
    val composerRoot = packageCli.getParent
    s"""#!/bin/sh
       |set -eu
       |[ "$$#" -eq 18 ] || exit 81
       |[ "$$1" = '-I' ] || exit 82
       |[ "$$2" = '-E' ] || exit 83
       |[ "$$3" = '-s' ] || exit 84
       |[ "$$4" = '-S' ] || exit 85
       |[ "$$5" = '-B' ] || exit 86
       |[ "$$6" = '-X' ] || exit 87
       |[ "$$7" = 'utf8' ] || exit 88
       |[ "$$8" = '-c' ] || exit 89
       |[ "$$9" = ${shellQuote(PostgreSqlEnumerationCampaignV2ComposerRunner.IsolatedBootstrap)} ] || exit 90
       |[ "$${10}" = ${shellQuote(composerRoot.resolve(Protocol.fileName).toString)} ] || exit 91
       |[ "$${11}" = ${shellQuote(composerRoot.resolve(ProducerEvidence.fileName).toString)} ] || exit 92
       |[ "$${12}" = ${shellQuote(composerRoot.resolve(PackageContract.fileName).toString)} ] || exit 93
       |[ "$${13}" = ${shellQuote(packageCli.toString)} ] || exit 94
       |[ "$${14}" = 'assemble' ] || exit 95
       |[ "$${15}" = '--input' ] || exit 96
       |[ "$${16}" = ${shellQuote(assemblyInput.toString)} ] || exit 97
       |[ "$${17}" = '--output' ] || exit 98
       |[ "$${18}" = ${shellQuote(stagedOutput.toString)} ] || exit 99
       |[ "$${HOME+x}" != x ] || exit 91
       |[ "$${PYTHONPATH+x}" != x ] || exit 92
       |[ "$${PYTHONHOME+x}" != x ] || exit 93
       |[ "$${YANNAKAKIS_JDBC_URL+x}" != x ] || exit 94
       |[ "$${YANNAKAKIS_DB_PASSWORD+x}" != x ] || exit 95
       |[ "$${PGPASSWORD+x}" != x ] || exit 96
       |[ "$${AWS_SECRET_ACCESS_KEY+x}" != x ] || exit 97
       |if IFS= read -r unexpected_stdin; then exit 98; fi
       |: > "$${18}"
       |""".stripMargin
  }

  private def exactValidationScript(
      packageCli: Path,
      stagedPackage: Path
  ): String = {
    val composerRoot = packageCli.getParent
    s"""#!/bin/sh
       |set -eu
       |[ "$$#" -eq 15 ] || exit 81
       |[ "$$1" = '-I' ] || exit 82
       |[ "$$2" = '-E' ] || exit 83
       |[ "$$3" = '-s' ] || exit 84
       |[ "$$4" = '-S' ] || exit 85
       |[ "$$5" = '-B' ] || exit 86
       |[ "$$6" = '-X' ] || exit 87
       |[ "$$7" = 'utf8' ] || exit 88
       |[ "$$8" = '-c' ] || exit 89
       |[ "$$9" = ${shellQuote(PostgreSqlEnumerationCampaignV2ComposerRunner.IsolatedBootstrap)} ] || exit 90
       |[ "$${10}" = ${shellQuote(composerRoot.resolve(Protocol.fileName).toString)} ] || exit 91
       |[ "$${11}" = ${shellQuote(composerRoot.resolve(ProducerEvidence.fileName).toString)} ] || exit 92
       |[ "$${12}" = ${shellQuote(composerRoot.resolve(PackageContract.fileName).toString)} ] || exit 93
       |[ "$${13}" = ${shellQuote(packageCli.toString)} ] || exit 94
       |[ "$${14}" = 'validate' ] || exit 95
       |[ "$${15}" = ${shellQuote(stagedPackage.toString)} ] || exit 96
       |[ "$${HOME+x}" != x ] || exit 88
       |[ "$${PYTHONPATH+x}" != x ] || exit 89
       |[ "$${PYTHONHOME+x}" != x ] || exit 90
       |[ "$${YANNAKAKIS_JDBC_URL+x}" != x ] || exit 91
       |[ "$${YANNAKAKIS_DB_PASSWORD+x}" != x ] || exit 92
       |[ "$${PGPASSWORD+x}" != x ] || exit 93
       |[ "$${AWS_SECRET_ACCESS_KEY+x}" != x ] || exit 94
       |if IFS= read -r unexpected_stdin; then exit 95; fi
       |""".stripMargin
  }

  private def shellQuote(value: String): String =
    "'" + value.replace("'", "'\"'\"'") + "'"

  private def writeExecutable(path: Path, content: String): Path = {
    Files.write(
      path,
      content.getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE_NEW
    )
    val permissions = Files.getPosixFilePermissions(path).asScala.toSet +
      PosixFilePermission.OWNER_EXECUTE
    Files.setPosixFilePermissions(path, permissions.asJava)
    path
  }

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def withFixture(body: Path => Unit): Unit = {
    val directory = Files.createTempDirectory("yrw-composer-runner-")
    try body(directory)
    finally {
      val stream = Files.walk(directory)
      try
        stream.iterator().asScala.toVector
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
      finally stream.close()
    }
  }
}
