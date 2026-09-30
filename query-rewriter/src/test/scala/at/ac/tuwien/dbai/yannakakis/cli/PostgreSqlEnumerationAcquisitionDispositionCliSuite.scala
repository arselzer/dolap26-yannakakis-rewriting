package at.ac.tuwien.dbai.yannakakis.cli

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateResourceLimits
import at.ac.tuwien.dbai.yannakakis.api.{
  DiagnosticSeverity,
  RewriteDiagnostic,
  RewriteReasonCode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationBatchResult
}

import PostgreSqlEnumerationSearchReplayV2TestSupport._

final class PostgreSqlEnumerationAcquisitionDispositionCliSuite
    extends munit.FunSuite {

  test("eligible found and fallback dispositions preserve legacy bundle bytes") {
    Vector(
      produce(width = 2, k = 1),
      produce(width = 1, k = 3)
    ).foreach { result =>
      withFixture(result) { fixture =>
        val legacyEvents = ArrayBuffer.empty[String]
        val dispositionEvents = ArrayBuffer.empty[String]
        val legacyBackend = new FixedBackend(result, legacyEvents)
        val dispositionBackend = new FixedBackend(result, dispositionEvents)
        val legacyComposer = new RecordingComposer(legacyEvents)
        val dispositionComposer = new RecordingComposer(dispositionEvents)

        val legacyExit = fixture.run(
          command = "acquire",
          output = fixture.legacyBundle,
          backend = legacyBackend,
          composer = legacyComposer
        )
        val dispositionExit = fixture.run(
          command = "acquire-disposition",
          output = fixture.disposition,
          backend = dispositionBackend,
          composer = dispositionComposer
        )

        assertEquals(legacyExit, PostgreSqlRewriteExitCode.Rewritten)
        assertEquals(dispositionExit, PostgreSqlRewriteExitCode.Rewritten)
        assertEquals(legacyBackend.calls, 1)
        assertEquals(dispositionBackend.calls, 1)
        assertEquals(legacyComposer.assembleCalls, 1)
        assertEquals(dispositionComposer.assembleCalls, 1)
        assertEquals(legacyComposer.validateCalls, 1)
        assertEquals(dispositionComposer.validateCalls, 1)
        assertEquals(
          legacyEvents.toVector,
          Vector("preflight", "backend", "assemble", "validate")
        )
        assertEquals(dispositionEvents.toVector, legacyEvents.toVector)
        assertEquals(
          directChildren(fixture.disposition),
          Vector("complete.json", "package.json", "search-input.json")
        )
        directChildren(fixture.legacyBundle).foreach { child =>
          assertEquals(
            Files.readAllBytes(fixture.disposition.resolve(child)).toVector,
            Files.readAllBytes(fixture.legacyBundle.resolve(child)).toVector,
            child
          )
        }
      }
    }
  }

  test("all terminal classes publish only their exact terminal receipt") {
    val unsupported = produce(sql = UnsupportedSql, width = 2, k = 1)
    val otherNotApplicable = unsupported.copy(
      diagnostics = Vector(
        RewriteDiagnostic(
          RewriteReasonCode.RewriteNotBeneficial,
          "closed non-applicability fixture",
          DiagnosticSeverity.Warning,
          Map("reason" -> "fixture")
        )
      )
    )
    val resourceLimited = produce(
      width = 2,
      k = 1,
      candidateLimits = CandidateResourceLimits
        .create(maxBlocks = 1L)
        .fold(problems => fail(problems.mkString("; ")), identity)
    )
    val otherFailed = resourceLimited.copy(
      diagnostics = Vector(
        RewriteDiagnostic(
          RewriteReasonCode.InternalError,
          "closed failure fixture",
          DiagnosticSeverity.Error,
          Map("stage" -> "candidate-search")
        )
      )
    )
    Vector(
      (produce(sql = " ", width = 2, k = 1), "invalid-query", Vector.empty),
      (unsupported, "unsupported", Vector.empty),
      (otherNotApplicable, "other-not-applicable", Vector.empty),
      (resourceLimited, "resource-limited", Vector("candidate-search")),
      (otherFailed, "other-failed", Vector.empty)
    ).foreach { case (result, expectedClass, expectedOrigins) =>
      withFixture(result) { fixture =>
        val events = ArrayBuffer.empty[String]
        val backend = new FixedBackend(result, events)
        val composer = new RecordingComposer(events)

        val exit = fixture.run(
          command = "acquire-disposition",
          output = fixture.disposition,
          backend = backend,
          composer = composer
        )

        assertEquals(exit, PostgreSqlRewriteExitCode.Rewritten)
        assertEquals(backend.calls, 1)
        assertEquals(composer.preflightCalls, 1)
        assertEquals(composer.assembleCalls, 0)
        assertEquals(composer.validateCalls, 0)
        assertEquals(events.toVector, Vector("preflight", "backend"))
        assertEquals(directChildren(fixture.disposition), Vector("complete.json"))
        val complete = ujson.read(
          Files.readString(fixture.disposition.resolve("complete.json"))
        )
        assertEquals(
          complete("schema_version").str,
          PostgreSqlEnumerationAcquisitionTerminalReceiptJson.SchemaVersion
        )
        assertEquals(complete("terminal_class").str, expectedClass)
        assertEquals(
          complete("resource_origins").arr.map(_.str).toVector,
          expectedOrigins
        )
        assertEquals(complete("claims_same_one_pass_result").bool, true)
        assertEquals(
          complete("claims_complete_registered_denominator").bool,
          false
        )
        assertEquals(complete("execution_outcomes_in_scope").bool, false)
        assertEquals(fixture.standardError.size(), 0)
      }
    }
  }

  test("mismatched request and malformed fallback fail closed without output") {
    val found = produce(width = 2, k = 1)
    val mismatched = produce(width = 1, k = 3)
    val fallback = produce(width = 1, k = 3)
    val malformedFallback = fallback.copy(
      diagnostics = fallback.diagnostics :+ fallback.diagnostics.head
    )

    Vector(
      mismatched -> found,
      malformedFallback -> fallback
    ).foreach { case (returned, authoredFrom) =>
      withFixture(authoredFrom) { fixture =>
        val events = ArrayBuffer.empty[String]
        val backend = new FixedBackend(
          returned,
          events,
          requireExactRequestedResult = false
        )
        val composer = new RecordingComposer(events)

        val exit = fixture.run(
          command = "acquire-disposition",
          output = fixture.disposition,
          backend = backend,
          composer = composer
        )

        assertEquals(exit, PostgreSqlRewriteExitCode.RewriteFailed)
        assertEquals(backend.calls, 1)
        assertEquals(composer.preflightCalls, 1)
        assertEquals(composer.assembleCalls, 0)
        assertEquals(composer.validateCalls, 0)
        assertEquals(events.toVector, Vector("preflight", "backend"))
        assert(!Files.exists(fixture.disposition))
        assert(
          fixture.standardError
            .toString(StandardCharsets.UTF_8)
            .contains(
              "enumeration-acquisition-disposition.producer-contract-failure"
            )
        )
      }
    }
  }

  test("a preexisting disposition is rejected before preflight or backend") {
    val result = produce(width = 2, k = 1)
    withFixture(result) { fixture =>
      Files.createDirectory(fixture.disposition)
      val events = ArrayBuffer.empty[String]
      val backend = new FixedBackend(result, events)
      val composer = new RecordingComposer(events)

      val exit = fixture.run(
        command = "acquire-disposition",
        output = fixture.disposition,
        backend = backend,
        composer = composer
      )

      assertEquals(exit, PostgreSqlRewriteExitCode.InvalidInvocation)
      assertEquals(backend.calls, 0)
      assertEquals(composer.preflightCalls, 0)
      assertEquals(composer.assembleCalls, 0)
      assertEquals(composer.validateCalls, 0)
      assertEquals(events.toVector, Vector.empty)
      assertEquals(directChildren(fixture.disposition), Vector.empty)
    }
  }

  private final class FixedBackend(
      result: PostgreSqlEnumerationBatchResult,
      events: ArrayBuffer[String],
      requireExactRequestedResult: Boolean = true
  ) extends PostgreSqlEnumerationCampaignV2Backend {
    var calls: Int = 0

    override def acquire(
        config: PostgreSqlEnumerationCampaignV2CliConfig,
        request: PostgreSqlEnumerationBatchRequest
    ) = {
      calls += 1
      events += "backend"
      if (requireExactRequestedResult) assertEquals(request, result.request)
      assertEquals(
        config.outputPackage.getFileName.toString,
        PostgreSqlEnumerationSearchReplayBundleV2Json.PackageFileName
      )
      Right(result)
    }
  }

  private final class RecordingComposer(events: ArrayBuffer[String])
      extends PostgreSqlEnumerationCampaignV2PackageComposer {
    var preflightCalls: Int = 0
    var assembleCalls: Int = 0
    var validateCalls: Int = 0

    override def preflight(pythonExecutable: Path, composerRoot: Path) = {
      preflightCalls += 1
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
      val legacyBundle: Path,
      val disposition: Path,
      val standardError: ByteArrayOutputStream
  ) {
    def run(
        command: String,
        output: Path,
        backend: PostgreSqlEnumerationCampaignV2Backend,
        composer: PostgreSqlEnumerationCampaignV2PackageComposer
    ): Int = {
      standardError.reset()
      val outputOption =
        if (command == "acquire") "--output-bundle"
        else "--output-disposition"
      PostgreSqlEnumerationSearchReplayV2AcquisitionCli.run(
        arguments = Array(
          command,
          "--authoring-request",
          authoringPath.toString,
          outputOption,
          output.toString,
          "--python-executable",
          "/fixture/python",
          "--composer-root",
          "/fixture/composer"
        ),
        environment = Environment,
        workingDirectory = directory,
        standardError = standardError,
        backend = backend,
        composer = composer
      )
    }
  }

  private def withFixture(result: PostgreSqlEnumerationBatchResult)(
      body: CliFixture => Unit
  ): Unit = {
    val directory = Files.createTempDirectory("yrw-disposition-cli-")
    val authoringPath = directory.resolve("authoring.json")
    Files.write(
      authoringPath,
      authoring(result).document.bytes,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )
    try
      body(
        new CliFixture(
          directory = directory,
          authoringPath = authoringPath,
          legacyBundle = directory.resolve("legacy-bundle"),
          disposition = directory.resolve("disposition"),
          standardError = new ByteArrayOutputStream()
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

  private val Environment: Map[String, String] = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
      "jdbc:postgresql://fixture.invalid/db",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "fixture-user",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "fixture-secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "fixture_schema"
  )
}
