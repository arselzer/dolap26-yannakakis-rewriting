package at.ac.tuwien.dbai.yannakakis.cli

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, StandardOpenOption}
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationBatchResult,
  PostgreSqlQueryRewriter
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

import munit.FunSuite

final class PostgreSqlEnumerationSearchReplayV1AcquisitionCliSuite
    extends FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("one backend call publishes one exact committed three-child bundle") {
    PostgreSqlEnumerationCampaignV2AcquisitionCli.withProcessOutputLock {
      withFixture(produce(triangleSql, width = 2, k = 2)) { fixture =>
        val events = ArrayBuffer.empty[String]
        val backend = new FixedBackend(fixture.result, events, emitNoise = true)
        val composer = new RecordingComposer(events)
        val originalOutput = System.out
        val originalError = System.err

        val exit = fixture.run(backend, composer)

        assertEquals(exit, PostgreSqlRewriteExitCode.Rewritten)
        assertEquals(fixture.standardError.toString(StandardCharsets.UTF_8), "")
        assertEquals(
          events.toVector,
          Vector("preflight", "backend", "assemble", "validate")
        )
        assertEquals(backend.calls, 1)
        assertEquals(composer.assembleCalls, 1)
        assertEquals(composer.validateCalls, 1)
        assert(System.out eq originalOutput)
        assert(System.err eq originalError)

        val children = directChildren(fixture.bundle)
        assertEquals(
          children,
          Vector("complete.json", "package.json", "search-input.json")
        )
        val packageBytes =
          Files.readAllBytes(fixture.bundle.resolve("package.json"))
        val searchBytes =
          Files.readAllBytes(fixture.bundle.resolve("search-input.json"))
        val completeBytes =
          Files.readAllBytes(fixture.bundle.resolve("complete.json"))
        val artifact = fixture.result.artifact.getOrElse(fail("expected artifact"))
        assertEquals(
          packageBytes.toVector,
          RecordingComposer.PackageBytes.toVector
        )
        assertEquals(
          searchBytes.toVector,
          PostgreSqlEnumerationSearchReplayV1Json.project(artifact).bytes.toVector
        )
        assert(completeBytes.last == '\n'.toByte)

        val complete = ujson.read(completeBytes)
        assertEquals(
          complete("schema_version").str,
          PostgreSqlEnumerationSearchReplayBundleV1Json.SchemaVersion
        )
        assertEquals(
          complete("producer_entrypoint").str,
          PostgreSqlEnumerationSearchReplayV1AcquisitionCli.Entrypoint
        )
        assertEquals(complete("claims_same_one_pass_artifact").bool, true)
        assertEquals(complete("claims_cryptographic_same_artifact").bool, false)
        assertEquals(complete("outcomes_in_scope").bool, false)
        assertEquals(complete("reportable").bool, false)
        assertEquals(
          complete("children")("package")("byte_length").num.toLong,
          packageBytes.length.toLong
        )
        assertEquals(
          complete("children")("package")("sha256").str,
          sha256(packageBytes)
        )
        assertEquals(
          complete("children")("search_input")("byte_length").num.toLong,
          searchBytes.length.toLong
        )
        assertEquals(
          complete("children")("search_input")("sha256").str,
          sha256(searchBytes)
        )
        assertEquals(
          PosixFilePermissions.toString(
            Files.getPosixFilePermissions(fixture.bundle)
          ),
          "rwx------"
        )
        children.foreach { name =>
          assertEquals(
            PosixFilePermissions.toString(
              Files.getPosixFilePermissions(fixture.bundle.resolve(name))
            ),
            "rw-------"
          )
        }
        assertEquals(stagingChildren(fixture.directory), Vector.empty)
      }
    }
  }

  test("pinned composer output is accepted by the independent bundle loader") {
    withFixture(produce(triangleSql, width = 2, k = 2)) { fixture =>
      val events = ArrayBuffer.empty[String]
      val backend = new FixedBackend(fixture.result, events)
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
        "found-actions"
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
      assertEquals(summary("claims_same_one_pass_artifact").bool, true)
      assertEquals(summary("claims_cryptographic_same_artifact").bool, false)
      assertEquals(summary("reportable").bool, false)
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }
  }

  test("create-only bundle rejection happens before composer or backend work") {
    withFixture(produce(triangleSql, width = 2, k = 1)) { fixture =>
      Files.createDirectory(fixture.bundle)
      Files.writeString(fixture.bundle.resolve("sentinel"), "owned\n")
      val events = ArrayBuffer.empty[String]
      val backend = new FixedBackend(fixture.result, events)

      val exit = fixture.run(backend, new RecordingComposer(events))

      assertEquals(exit, PostgreSqlRewriteExitCode.InputError)
      assertEquals(events.toVector, Vector.empty)
      assertEquals(backend.calls, 0)
      assertEquals(Files.readString(fixture.bundle.resolve("sentinel")), "owned\n")
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }
  }

  test("preflight and producer failures leave no visible bundle") {
    withFixture(produce(triangleSql, width = 2, k = 1)) { fixture =>
      val events = ArrayBuffer.empty[String]
      val backend = new FixedBackend(fixture.result, events)
      val composer = new RecordingComposer(
        events,
        preflightFailure = Some(
          PostgreSqlEnumerationCampaignV2ComposerFailure.BundleHashMismatch(
            PostgreSqlEnumerationCampaignV2ComposerComponent.Protocol
          )
        )
      )
      assertEquals(
        fixture.run(backend, composer),
        PostgreSqlRewriteExitCode.ServiceUnavailable
      )
      assertEquals(events.toVector, Vector("preflight"))
      assertEquals(backend.calls, 0)
      assert(!Files.exists(fixture.bundle))
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }

    withFixture(produce(triangleSql, width = 2, k = 1)) { fixture =>
      val events = ArrayBuffer.empty[String]
      val backend = new FailingBackend(events)
      assertEquals(
        fixture.run(backend, new RecordingComposer(events)),
        PostgreSqlRewriteExitCode.ServiceUnavailable
      )
      assertEquals(events.toVector, Vector("preflight", "backend"))
      assertEquals(backend.calls, 1)
      assert(!Files.exists(fixture.bundle))
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }
  }

  test("partial composer assembly and validation failures never publish a bundle") {
    val failure = PostgreSqlEnumerationCampaignV2ComposerFailure.NonZeroExit(2)
    Vector(true, false).foreach { failDuringAssembly =>
      withFixture(produce(triangleSql, width = 2, k = 1)) { fixture =>
        val events = ArrayBuffer.empty[String]
        val backend = new FixedBackend(fixture.result, events)
        val composer = new RecordingComposer(
          events,
          assemblyFailure = if (failDuringAssembly) Some(failure) else None,
          validationFailure = if (failDuringAssembly) None else Some(failure)
        )

        assertEquals(
          fixture.run(backend, composer),
          PostgreSqlRewriteExitCode.RewriteFailed
        )
        assertEquals(
          events.toVector,
          if (failDuringAssembly)
            Vector("preflight", "backend", "assemble")
          else Vector("preflight", "backend", "assemble", "validate")
        )
        assertEquals(backend.calls, 1)
        assert(!Files.exists(fixture.bundle))
        assertEquals(stagingChildren(fixture.directory), Vector.empty)
      }
    }
  }

  test("fallback and a mismatched entrypoint cannot create a committed bundle") {
    val fallback = produce(triangleSql, width = 1, k = 3)
    withFixture(fallback) { fixture =>
      val events = ArrayBuffer.empty[String]
      assertEquals(
        fixture.run(
          new FixedBackend(fallback, events),
          new RecordingComposer(events)
        ),
        PostgreSqlRewriteExitCode.NotApplicable
      )
      assertEquals(events.toVector, Vector("preflight", "backend"))
      assert(!Files.exists(fixture.bundle))
    }

    withFixture(
      produce(triangleSql, width = 2, k = 1),
      producerEntrypoint = PostgreSqlEnumerationCampaignV2AcquisitionCli.Entrypoint
    ) { fixture =>
      val events = ArrayBuffer.empty[String]
      val backend = new FixedBackend(fixture.result, events)
      assertEquals(
        fixture.run(backend, new RecordingComposer(events)),
        PostgreSqlRewriteExitCode.InputError
      )
      assertEquals(events.toVector, Vector.empty)
      assertEquals(backend.calls, 0)
      assert(!Files.exists(fixture.bundle))
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }
  }

  private final class FixedBackend(
      result: PostgreSqlEnumerationBatchResult,
      events: ArrayBuffer[String],
      emitNoise: Boolean = false
  ) extends PostgreSqlEnumerationCampaignV2Backend {
    var calls = 0
    override def acquire(
        config: PostgreSqlEnumerationCampaignV2CliConfig,
        request: PostgreSqlEnumerationBatchRequest
    ) = {
      calls += 1
      events += "backend"
      if (emitNoise) {
        System.out.print("discarded backend stdout")
        System.err.print("discarded backend stderr")
      }
      assertEquals(request, result.request)
      assertEquals(
        config.outputPackage.getFileName.toString,
        PostgreSqlEnumerationSearchReplayBundleV1Json.PackageFileName
      )
      Right(result)
    }
  }

  private final class FailingBackend(events: ArrayBuffer[String])
      extends PostgreSqlEnumerationCampaignV2Backend {
    var calls = 0
    override def acquire(
        config: PostgreSqlEnumerationCampaignV2CliConfig,
        request: PostgreSqlEnumerationBatchRequest
    ) = {
      calls += 1
      events += "backend"
      Left(
        PostgreSqlEnumerationCampaignV2BackendFailure.PostgreSqlUnavailable
      )
    }
  }

  private final class RecordingComposer(
      events: ArrayBuffer[String],
      preflightFailure: Option[PostgreSqlEnumerationCampaignV2ComposerFailure] = None,
      assemblyFailure: Option[PostgreSqlEnumerationCampaignV2ComposerFailure] = None,
      validationFailure: Option[PostgreSqlEnumerationCampaignV2ComposerFailure] = None
  ) extends PostgreSqlEnumerationCampaignV2PackageComposer {
    var assembleCalls = 0
    var validateCalls = 0

    override def preflight(pythonExecutable: Path, composerRoot: Path) = {
      events += "preflight"
      preflightFailure.toLeft(())
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
      assemblyFailure.toLeft(())
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
      validationFailure.toLeft(())
    }
  }

  private object RecordingComposer {
    val PackageBytes: Array[Byte] =
      "{\"package\":\"fixture\"}\n".getBytes(StandardCharsets.UTF_8)
  }

  private final class CliFixture(
      val directory: Path,
      val authoring: Path,
      val bundle: Path,
      val result: PostgreSqlEnumerationBatchResult,
      val standardError: ByteArrayOutputStream
  ) {
    def run(
        backend: PostgreSqlEnumerationCampaignV2Backend,
        composer: PostgreSqlEnumerationCampaignV2PackageComposer,
        pythonExecutable: Path = Path.of("/fixture/python"),
        composerRoot: Path = Path.of("/fixture/composer")
    ): Int =
      PostgreSqlEnumerationSearchReplayV1AcquisitionCli.run(
        arguments = Array(
          "acquire",
          "--authoring-request",
          authoring.toString,
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
        PostgreSqlEnumerationSearchReplayV1AcquisitionCli.Entrypoint
  )(body: CliFixture => Unit): Unit = {
    val directory = Files.createTempDirectory("yrw-search-replay-cli-")
    val authoringPath = directory.resolve("authoring.json")
    val bundle = directory.resolve("bundle")
    Files.write(
      authoringPath,
      request(result, producerEntrypoint).document.bytes,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )
    try
      body(
        new CliFixture(
          directory,
          authoringPath,
          bundle,
          result,
          new ByteArrayOutputStream()
        )
      )
    finally deleteOwnedTree(directory)
  }

  private def request(
      result: PostgreSqlEnumerationBatchResult,
      producerEntrypoint: String
  ): PostgreSqlEnumerationCampaignV2AuthoringRequest =
    PostgreSqlEnumerationCampaignV2AuthoringRequest
      .create(
        PostgreSqlEnumerationCampaignV2AuthoringInput(
          sourceSql = result.request.originalSql,
          workloadId = "triangle-cell-01",
          lineageId = "triangle-lineage-v2",
          widthBound = result.request.widthBound,
          requestedK = result.request.planLimit,
          requireConnectedCover = result.request.requireConnectedCover,
          dataContract = PostgreSqlEnumerationCampaignV2DataContract(
            "triangle-fixture-v2",
            "a" * 64,
            "b" * 64,
            "c" * 64
          ),
          producerEntrypoint = producerEntrypoint,
          toolchainComponents = Vector(
            PostgreSqlEnumerationCampaignV2ToolchainComponent(
              "calcite",
              "1.36.0",
              "d" * 64
            ),
            PostgreSqlEnumerationCampaignV2ToolchainComponent(
              "query-rewriter",
              "commit-fixture",
              "e" * 64
            ),
            PostgreSqlEnumerationCampaignV2ToolchainComponent(
              "scala",
              "2.13.13",
              "f" * 64
            )
          )
        )
      )
      .fold(problem => fail(problem), identity)

  private def produce(
      sql: String,
      width: Int,
      k: Int
  ): PostgreSqlEnumerationBatchResult = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(value => Right(fixture.plan(value).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
    val request = PostgreSqlEnumerationBatchRequest
      .create(sql, widthBound = width, planLimit = k)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    rewriter.produceEnumerationBatch(request)
  }

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

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
        "enumeration-search-replay-v1"
      )
      .toAbsolutePath
      .normalize()

  private lazy val systemPython: Path =
    sys.env
      .get("PATH")
      .toVector
      .flatMap(_.split(java.io.File.pathSeparator).toVector)
      .iterator
      .map(entry => Path.of(entry).toAbsolutePath.normalize().resolve("python3"))
      .find(path => Files.isRegularFile(path) && Files.isExecutable(path))
      .map(_.toRealPath())
      .getOrElse(fail("python3 is required by the cross-language bundle test"))

  private def directChildren(directory: Path): Vector[String] = {
    val stream = Files.list(directory)
    try stream.iterator().asScala.map(_.getFileName.toString).toVector.sorted
    finally stream.close()
  }

  private def stagingChildren(directory: Path): Vector[String] = {
    val stream = Files.list(directory)
    try
      stream.iterator().asScala
        .map(_.getFileName.toString)
        .filter(_.startsWith(".yrw-enumeration-acquisition-"))
        .toVector
    finally stream.close()
  }

  private def deleteOwnedTree(directory: Path): Unit = {
    val stream = Files.walk(directory)
    val paths = try stream.iterator().asScala.toVector
    finally stream.close()
    paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
  }
}
