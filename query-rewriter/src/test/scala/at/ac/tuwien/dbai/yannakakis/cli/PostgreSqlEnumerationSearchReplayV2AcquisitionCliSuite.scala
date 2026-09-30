package at.ac.tuwien.dbai.yannakakis.cli

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationBatchResult
}

import PostgreSqlEnumerationSearchReplayV2TestSupport._

final class PostgreSqlEnumerationSearchReplayV2AcquisitionCliSuite
    extends munit.FunSuite {

  test("one backend call publishes found and exact fallback v2 bundles") {
    Vector(
      produce(width = 2, k = 1) -> "found-actions",
      produce(width = 1, k = 3) -> "typed-native-fallback"
    ).foreach { case (result, expectedStatus) =>
      withFixture(result) { fixture =>
        val events = ArrayBuffer.empty[String]
        val backend = new FixedBackend(result, events)
        val composer = new RecordingComposer(events)

        val exit = fixture.run(backend, composer)

        assertEquals(exit, PostgreSqlRewriteExitCode.Rewritten)
        assertEquals(backend.calls, 1)
        assertEquals(composer.assembleCalls, 1)
        assertEquals(composer.validateCalls, 1)
        assertEquals(
          events.toVector,
          Vector("preflight", "backend", "assemble", "validate")
        )
        assertEquals(
          directChildren(fixture.bundle),
          Vector("complete.json", "package.json", "search-input.json")
        )
        val complete = ujson.read(
          Files.readString(fixture.bundle.resolve("complete.json"))
        )
        assertEquals(complete("acquisition_status").str, expectedStatus)
        assertEquals(complete("claims_same_one_pass_result").bool, true)
        assertEquals(
          complete("claims_complete_registered_denominator").bool,
          false
        )
        assertEquals(
          complete("producer_entrypoint").str,
          PostgreSqlEnumerationSearchReplayV2AcquisitionCli.Entrypoint
        )
        assertEquals(fixture.standardError.size(), 0)
      }
    }
  }

  test("real fallback bundle passes the independent pinned v2 loader") {
    val result = produce(width = 1, k = 3)
    withFixture(result) { fixture =>
      val events = ArrayBuffer.empty[String]
      val backend = new FixedBackend(result, events)
      val exit = fixture.run(
        backend,
        LivePostgreSqlEnumerationCampaignV2PackageComposer,
        pythonExecutable = systemPython,
        composerRoot = campaignRoot
      )

      assertEquals(exit, PostgreSqlRewriteExitCode.Rewritten)
      assertEquals(backend.calls, 1)
      assertEquals(
        ujson.read(Files.readString(fixture.bundle.resolve("package.json")))(
          "acquisition_status"
        ).str,
        "typed-native-fallback"
      )
      assertEquals(
        ujson.read(Files.readString(fixture.bundle.resolve("search-input.json")))(
          "schema_version"
        ).str,
        "yrw-enumeration-search-input/v2"
      )
      val validated = runProcess(
        Vector(
          systemPython.toString,
          "-I",
          "-S",
          "-B",
          replayRoot.resolve("tool.py").toString,
          "validate-bundle",
          "--bundle",
          fixture.bundle.toString
        )
      )
      assertEquals(validated._1, 0, validated._3)
      assertEquals(validated._3, "")
      val summary = ujson.read(validated._2)
      assertEquals(summary("status").str, "verified")
      assertEquals(summary("acquisition_status").str, "typed-native-fallback")
      assertEquals(summary("claims_same_one_pass_result").bool, true)
      assertEquals(
        summary("claims_complete_registered_denominator").bool,
        false
      )
    }
  }

  test("old producer entrypoints are rejected before preflight or backend work") {
    Vector(
      PostgreSqlEnumerationSearchReplayV1AcquisitionCli.Entrypoint,
      PostgreSqlEnumerationCampaignV2AcquisitionCli.Entrypoint
    ).foreach { oldEntrypoint =>
      val result = produce(width = 2, k = 1)
      withFixture(result, producerEntrypoint = oldEntrypoint) { fixture =>
        val events = ArrayBuffer.empty[String]
        val backend = new FixedBackend(result, events)
        val exit = fixture.run(backend, new RecordingComposer(events))

        assertEquals(exit, PostgreSqlRewriteExitCode.InputError)
        assertEquals(events.toVector, Vector.empty)
        assertEquals(backend.calls, 0)
        assert(!Files.exists(fixture.bundle))
      }
    }
  }

  test("unsupported producer result is rejected after exactly one backend call") {
    val result = produce(sql = UnsupportedSql, width = 2, k = 1)
    withFixture(result) { fixture =>
      val events = ArrayBuffer.empty[String]
      val backend = new FixedBackend(result, events)
      val exit = fixture.run(backend, new RecordingComposer(events))

      assertEquals(exit, PostgreSqlRewriteExitCode.NotApplicable)
      assertEquals(events.toVector, Vector("preflight", "backend"))
      assertEquals(backend.calls, 1)
      assert(!Files.exists(fixture.bundle))
    }
  }

  private final class FixedBackend(
      result: PostgreSqlEnumerationBatchResult,
      events: ArrayBuffer[String]
  ) extends PostgreSqlEnumerationCampaignV2Backend {
    var calls: Int = 0

    override def acquire(
        config: PostgreSqlEnumerationCampaignV2CliConfig,
        request: PostgreSqlEnumerationBatchRequest
    ) = {
      calls += 1
      events += "backend"
      assertEquals(request, result.request)
      assertEquals(
        config.outputPackage.getFileName.toString,
        PostgreSqlEnumerationSearchReplayBundleV2Json.PackageFileName
      )
      Right(result)
    }
  }

  private final class RecordingComposer(events: ArrayBuffer[String])
      extends PostgreSqlEnumerationCampaignV2PackageComposer {
    var assembleCalls: Int = 0
    var validateCalls: Int = 0

    override def preflight(pythonExecutable: Path, composerRoot: Path) = {
      events += "preflight"
      Right(())
    }

    override def assemble(
        pythonExecutable: Path,
        composerRoot: Path,
        assemblyInput: Path,
        stagedOutput: Path
    ) = {
      assembleCalls += 1
      events += "assemble"
      assert(Files.readAllBytes(assemblyInput).last == '\n'.toByte)
      Files.write(
        stagedOutput,
        RecordingComposer.PackageBytes,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE
      )
      Right(())
    }

    override def validate(
        pythonExecutable: Path,
        composerRoot: Path,
        stagedPackage: Path
    ) = {
      validateCalls += 1
      events += "validate"
      assertEquals(
        Files.readAllBytes(stagedPackage).toVector,
        RecordingComposer.PackageBytes.toVector
      )
      Right(())
    }
  }

  private object RecordingComposer {
    val PackageBytes: Array[Byte] =
      "{\"package\":\"fixture\"}\n".getBytes(StandardCharsets.UTF_8)
  }

  private final class CliFixture(
      val directory: Path,
      val authoringPath: Path,
      val bundle: Path,
      val standardError: ByteArrayOutputStream
  ) {
    def run(
        backend: PostgreSqlEnumerationCampaignV2Backend,
        composer: PostgreSqlEnumerationCampaignV2PackageComposer,
        pythonExecutable: Path = Path.of("/fixture/python"),
        composerRoot: Path = Path.of("/fixture/composer")
    ): Int =
      PostgreSqlEnumerationSearchReplayV2AcquisitionCli.run(
        arguments = Array(
          "acquire",
          "--authoring-request",
          authoringPath.toString,
          "--output-bundle",
          bundle.toString,
          "--python-executable",
          pythonExecutable.toString,
          "--composer-root",
          composerRoot.toString
        ),
        environment = Map(
          PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
            "jdbc:postgresql://fixture.invalid/db",
          PostgreSqlRewriteCliConfig.UserEnvironment -> "fixture-user",
          PostgreSqlRewriteCliConfig.PasswordEnvironment -> "fixture-secret",
          PostgreSqlRewriteCliConfig.SchemaEnvironment -> "fixture_schema"
        ),
        workingDirectory = directory,
        standardError = standardError,
        backend = backend,
        composer = composer
      )
  }

  private def withFixture(
      result: PostgreSqlEnumerationBatchResult,
      producerEntrypoint: String =
        PostgreSqlEnumerationSearchReplayV2AcquisitionCli.Entrypoint
  )(body: CliFixture => Unit): Unit = {
    val directory = Files.createTempDirectory("yrw-search-replay-v2-cli-")
    val authoringPath = directory.resolve("authoring.json")
    val bundle = directory.resolve("bundle")
    Files.write(
      authoringPath,
      authoring(result, producerEntrypoint).document.bytes,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )
    try
      body(
        new CliFixture(
          directory,
          authoringPath,
          bundle,
          new ByteArrayOutputStream()
        )
      )
    finally deleteOwnedTree(directory)
  }

  private def directChildren(directory: Path): Vector[String] = {
    val stream = Files.list(directory)
    try stream.iterator().asScala.map(_.getFileName.toString).toVector.sorted
    finally stream.close()
  }

  private def deleteOwnedTree(root: Path): Unit = {
    if (!Files.exists(root)) return
    val stream = Files.walk(root)
    try
      stream.iterator().asScala.toVector.sortBy(_.getNameCount).reverse.foreach {
        path => Files.deleteIfExists(path)
      }
    finally stream.close()
  }

  private def campaignRoot: Path =
    Path
      .of("..", "benchmarks", "cyclic-postgresql", "enumeration-campaign-v2")
      .toAbsolutePath
      .normalize()

  private def replayRoot: Path =
    Path
      .of(
        "..",
        "benchmarks",
        "cyclic-postgresql",
        "enumeration-search-replay-v2"
      )
      .toAbsolutePath
      .normalize()

  private def runProcess(command: Vector[String]): (Int, String, String) = {
    val process = new ProcessBuilder(command: _*)
      .redirectInput(ProcessBuilder.Redirect.PIPE)
      .start()
    process.getOutputStream.close()
    val stdout = new ByteArrayOutputStream()
    val stderr = new ByteArrayOutputStream()
    val stdoutThread = new Thread(() => process.getInputStream.transferTo(stdout))
    val stderrThread = new Thread(() => process.getErrorStream.transferTo(stderr))
    stdoutThread.start()
    stderrThread.start()
    val exit = process.waitFor()
    stdoutThread.join()
    stderrThread.join()
    (
      exit,
      stdout.toString(StandardCharsets.UTF_8),
      stderr.toString(StandardCharsets.UTF_8)
    )
  }

  private lazy val systemPython: Path =
    sys.env
      .get("PATH")
      .toVector
      .flatMap(_.split(java.io.File.pathSeparator).toVector)
      .iterator
      .map(entry => Path.of(entry).toAbsolutePath.normalize().resolve("python3"))
      .find(path => Files.isRegularFile(path) && Files.isExecutable(path))
      .map(_.toRealPath())
      .getOrElse(fail("python3 is required by the v2 composer test"))
}
