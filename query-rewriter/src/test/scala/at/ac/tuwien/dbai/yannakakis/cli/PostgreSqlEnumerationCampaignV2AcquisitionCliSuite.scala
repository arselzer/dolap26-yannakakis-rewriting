package at.ac.tuwien.dbai.yannakakis.cli

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, StandardOpenOption}

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

final class PostgreSqlEnumerationCampaignV2AcquisitionCliSuite
    extends FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("the top-level CLI silently publishes one validated package") {
    PostgreSqlEnumerationCampaignV2AcquisitionCli.withProcessOutputLock {
      withFixture(produce(triangleSql, width = 2, k = 1)) { fixture =>
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
        assertEquals(
          Files.readString(fixture.output, StandardCharsets.UTF_8),
          RecordingComposer.PackageText
        )
        assertEquals(
          PosixFilePermissions.toString(
            Files.getPosixFilePermissions(fixture.output)
          ),
          "rw-------"
        )
        assert(composer.assemblyBytes.exists(_.last == '\n'.toByte))
        assertEquals(stagingChildren(fixture.directory), Vector.empty)
      }
    }
  }

  test(
    "the real pinned composer ignores sibling source and cached-bytecode shadows"
  ) {
    withFixture(produce(triangleSql, width = 2, k = 1)) { fixture =>
      val python = systemPython
      val isolatedComposer = fixture.directory.resolve("composer-with-shadow")
      Files.createDirectory(isolatedComposer)
      PostgreSqlEnumerationCampaignV2ComposerComponent.All.foreach { component =>
        Files.copy(
          campaignRoot.resolve(component.fileName),
          isolatedComposer.resolve(component.fileName)
        )
      }
      Files.writeString(
        isolatedComposer.resolve("json.py"),
        "raise SystemExit(97)\n",
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE_NEW
      )
      Files.writeString(
        isolatedComposer.resolve("argparse.py"),
        "raise SystemExit(98)\n",
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE_NEW
      )
      val maliciousSource = fixture.directory.resolve("package-contract-shadow.py")
      Files.writeString(
        maliciousSource,
        "raise SystemExit(96)\n",
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE_NEW
      )
      val cache = isolatedComposer.resolve("__pycache__")
      Files.createDirectory(cache)
      val bytecodeCompiler = new ProcessBuilder(
        python.toString,
        "-I",
        "-S",
        "-B",
        "-c",
        "import pathlib,py_compile,sys;" +
          "target=pathlib.Path(sys.argv[2])/(sys.argv[3]+'.'+" +
          "sys.implementation.cache_tag+'.pyc');" +
          "py_compile.compile(sys.argv[1],cfile=str(target),doraise=True," +
          "invalidation_mode=py_compile.PycInvalidationMode.UNCHECKED_HASH)",
        maliciousSource.toString,
        cache.toString,
        "package_contract"
      ).start()
      bytecodeCompiler.getOutputStream.close()
      assertEquals(bytecodeCompiler.waitFor(), 0)
      val backend = new FixedBackend(fixture.result, ArrayBuffer.empty)
      val exit = fixture.run(
        backend,
        LivePostgreSqlEnumerationCampaignV2PackageComposer,
        pythonExecutable = python,
        composerRoot = isolatedComposer
      )

      assertEquals(exit, PostgreSqlRewriteExitCode.Rewritten)
      assertEquals(fixture.standardError.toString(StandardCharsets.UTF_8), "")
      val packaged = ujson.read(Files.readString(fixture.output))
      assertEquals(packaged("acquisition_status").str, "found-actions")
      assertEquals(packaged("reportable").bool, false)
      assertEquals(packaged("outcomes_in_scope").bool, false)
      assertEquals(backend.calls, 1)
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }
    withFixture(produce(triangleSql, width = 1, k = 3)) { fixture =>
      val backend = new FixedBackend(fixture.result, ArrayBuffer.empty)
      val exit = fixture.run(
        backend,
        LivePostgreSqlEnumerationCampaignV2PackageComposer,
        pythonExecutable = systemPython,
        composerRoot = campaignRoot
      )

      assertEquals(exit, PostgreSqlRewriteExitCode.Rewritten)
      assertEquals(fixture.standardError.toString(StandardCharsets.UTF_8), "")
      val packaged = ujson.read(Files.readString(fixture.output))
      assertEquals(packaged("acquisition_status").str, "typed-native-fallback")
      assertEquals(
        packaged("resource_statistics_certificate")("fallback_reason")("code").str,
        "no-decomposition"
      )
      assertEquals(backend.calls, 1)
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }
  }

  test("create-only output rejection occurs before composer or backend work") {
    withFixture(produce(triangleSql, width = 2, k = 1)) { fixture =>
      Files.writeString(fixture.output, "existing\n", StandardCharsets.UTF_8)
      val events = ArrayBuffer.empty[String]
      val backend = new FixedBackend(fixture.result, events)
      val composer = new RecordingComposer(events)

      val exit = fixture.run(backend, composer)

      assertEquals(exit, PostgreSqlRewriteExitCode.InputError)
      assertEquals(events.toVector, Vector.empty)
      assertEquals(backend.calls, 0)
      assertEquals(Files.readString(fixture.output), "existing\n")
      assertEquals(
        fixture.standardError.toString(StandardCharsets.UTF_8),
        "enumeration-campaign-v2 acquisition error: acquisition-files.output-exists\n"
      )
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }
  }

  test("composer preflight is before the sole producer call") {
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

      val exit = fixture.run(backend, composer)

      assertEquals(exit, PostgreSqlRewriteExitCode.ServiceUnavailable)
      assertEquals(events.toVector, Vector("preflight"))
      assertEquals(backend.calls, 0)
      assert(!Files.exists(fixture.output))
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }
  }

  test("a noncanonical request and a false producer entrypoint fail before staging") {
    val result = produce(triangleSql, width = 2, k = 1)
    Vector(
      request(result, PostgreSqlEnumerationCampaignV2AcquisitionCli.Entrypoint)
        .document.bytes
        .dropRight(1),
      request(result, "wrong.producer.Entrypoint").document.bytes
    ).foreach { requestBytes =>
      withFixture(result, requestBytes = Some(requestBytes)) { fixture =>
        val events = ArrayBuffer.empty[String]
        val exit = fixture.run(
          new FixedBackend(result, events),
          new RecordingComposer(events)
        )
        assertEquals(exit, PostgreSqlRewriteExitCode.InputError)
        assertEquals(events.toVector, Vector.empty)
        assert(!Files.exists(fixture.output))
        assertEquals(stagingChildren(fixture.directory), Vector.empty)
      }
    }
  }

  test("typed producer outcomes retain the public 2 and 3 exit meanings") {
    val cases = Vector(
      produce("SELECT r.a FROM triangle_r AS r", width = 2, k = 1) ->
        PostgreSqlRewriteExitCode.NotApplicable,
      produce("   ", width = 2, k = 1) ->
        PostgreSqlRewriteExitCode.InvalidQuery
    )
    val notApplicable = cases.head._1
    Vector(
      PostgreSqlEnumerationCampaignV2AcquisitionFailure.AuthoringMismatch(
        notApplicable.completedPhase.id
      ),
      PostgreSqlEnumerationCampaignV2AcquisitionFailure.ProjectionRejected(
        notApplicable.completedPhase.id
      ),
      PostgreSqlEnumerationCampaignV2AcquisitionFailure.AssemblyRejected(
        notApplicable.completedPhase.id
      )
    ).foreach { integrityFailure =>
      assertEquals(
        PostgreSqlEnumerationCampaignV2CliFailure
          .acquisition(integrityFailure, notApplicable)
          .exitCode,
        PostgreSqlRewriteExitCode.RewriteFailed
      )
    }
    assertEquals(
      PostgreSqlEnumerationCampaignV2CliFailure
        .acquisition(
          PostgreSqlEnumerationCampaignV2AcquisitionFailure
            .UnpackageableResult(notApplicable.completedPhase.id),
          notApplicable
        )
        .exitCode,
      PostgreSqlRewriteExitCode.NotApplicable
    )
    cases.foreach { case (result, expectedExit) =>
      withFixture(result) { fixture =>
        val events = ArrayBuffer.empty[String]
        val exit = fixture.run(
          new FixedBackend(result, events),
          new RecordingComposer(events)
        )
        assertEquals(exit, expectedExit)
        assertEquals(events.toVector, Vector("preflight", "backend"))
        assert(!Files.exists(fixture.output))
        assertEquals(stagingChildren(fixture.directory), Vector.empty)
      }
    }
  }

  test("composer validation failure never publishes its staged bytes") {
    withFixture(produce(triangleSql, width = 2, k = 1)) { fixture =>
      val events = ArrayBuffer.empty[String]
      val composer = new RecordingComposer(
        events,
        validationFailure = Some(
          PostgreSqlEnumerationCampaignV2ComposerFailure.NonZeroExit(2)
        )
      )
      val exit = fixture.run(new FixedBackend(fixture.result, events), composer)

      assertEquals(exit, PostgreSqlRewriteExitCode.RewriteFailed)
      assertEquals(
        events.toVector,
        Vector("preflight", "backend", "assemble", "validate")
      )
      assert(!Files.exists(fixture.output))
      assertEquals(stagingChildren(fixture.directory), Vector.empty)
    }
  }

  test("unexpected producer failure is redacted and cleaned") {
    withFixture(produce(triangleSql, width = 2, k = 1)) { fixture =>
      val events = ArrayBuffer.empty[String]
      val backend = new PostgreSqlEnumerationCampaignV2Backend {
        override def acquire(
            config: PostgreSqlEnumerationCampaignV2CliConfig,
            request: PostgreSqlEnumerationBatchRequest
        ) = {
          events += "backend"
          throw new IllegalStateException("secret-path-and-password")
        }
      }
      val exit = fixture.run(backend, new RecordingComposer(events))
      val failure = fixture.standardError.toString(StandardCharsets.UTF_8)

      assertEquals(exit, PostgreSqlRewriteExitCode.InternalError)
      assertEquals(
        failure,
        "enumeration-campaign-v2 acquisition error: acquisition-cli.internal-error\n"
      )
      assert(!failure.contains("secret"))
      assert(!Files.exists(fixture.output))
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
        System.out.print("discarded third-party stdout")
        System.err.print("discarded third-party stderr")
      }
      assertEquals(request, result.request)
      Right(result)
    }
  }

  private final class RecordingComposer(
      events: ArrayBuffer[String],
      preflightFailure: Option[PostgreSqlEnumerationCampaignV2ComposerFailure] = None,
      validationFailure: Option[PostgreSqlEnumerationCampaignV2ComposerFailure] = None
  ) extends PostgreSqlEnumerationCampaignV2PackageComposer {
    var assembleCalls = 0
    var validateCalls = 0
    var assemblyBytes: Option[Array[Byte]] = None

    override def preflight(
        pythonExecutable: Path,
        composerRoot: Path
    ) = {
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
      assemblyBytes = Some(Files.readAllBytes(assemblyInput))
      Files.write(
        stagedOutput,
        RecordingComposer.PackageText.getBytes(StandardCharsets.UTF_8),
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
      validationFailure.toLeft(())
    }
  }

  private object RecordingComposer {
    val PackageText = "{\"package\":\"fixture\"}\n"
  }

  private final class CliFixture(
      val directory: Path,
      val authoring: Path,
      val output: Path,
      val result: PostgreSqlEnumerationBatchResult,
      val standardError: ByteArrayOutputStream
  ) {
    def run(
        backend: PostgreSqlEnumerationCampaignV2Backend,
        composer: PostgreSqlEnumerationCampaignV2PackageComposer,
        pythonExecutable: Path = Path.of("/fixture/python"),
        composerRoot: Path = Path.of("/fixture/composer")
    ): Int =
      PostgreSqlEnumerationCampaignV2AcquisitionCli.run(
        arguments = Array(
          "acquire",
          "--authoring-request",
          authoring.toString,
          "--output-package",
          output.toString,
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
      requestBytes: Option[Array[Byte]] = None
  )(body: CliFixture => Unit): Unit = {
    val directory = Files.createTempDirectory("yrw-campaign-v2-cli-")
    val authoringPath = directory.resolve("authoring.json")
    val output = directory.resolve("package.json")
    val bytes = requestBytes.getOrElse(
      request(
        result,
        PostgreSqlEnumerationCampaignV2AcquisitionCli.Entrypoint
      ).document.bytes
    )
    Files.write(authoringPath, bytes)
    try
      body(
        new CliFixture(
          directory,
          authoringPath,
          output,
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
    rewriter.produceEnumerationBatchLegacyV2(request)
  }

  private def campaignRoot: Path =
    Path
      .of("..", "benchmarks", "cyclic-postgresql", "enumeration-campaign-v2")
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
      .getOrElse(fail("python3 executable is required by the composer integration test"))

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
