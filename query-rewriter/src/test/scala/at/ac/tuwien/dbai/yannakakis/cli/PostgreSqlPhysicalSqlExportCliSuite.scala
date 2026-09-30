package at.ac.tuwien.dbai.yannakakis.cli

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorCoverAssignment,
  BagFactorPhysicalConfiguration
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId}
import com.fasterxml.jackson.databind.ObjectMapper
import munit.FunSuite

import scala.jdk.CollectionConverters._
import scala.util.Using

final class PostgreSqlPhysicalSqlExportCliSuite extends FunSuite {
  private val mapper = new ObjectMapper()
  private val passwordName = "EXPORT_TEST_PASSWORD"
  private val password = "fixture-super-secret"
  private val jdbcUrl = "jdbc:postgresql://invalid.example/fixture"
  private val user = "fixture-user"
  private val sourceSql = "SELECT l.k, r.k FROM l JOIN r ON l.k = r.k\n"

  private final class FixtureBackend extends PostgreSqlPhysicalSqlExportBackend {
    var calls = 0
    var observedPassword: Option[String] = None

    override def generate(
        config: PostgreSqlPhysicalSqlExportCli.Config,
        sql: String
    ): Either[
      PostgreSqlPhysicalSqlExportCli.Failure,
      PostgreSqlPhysicalSqlExportCli.Generated
    ] = {
      calls += 1
      observedPassword = Some(config.password)
      assertEquals(sql, sourceSql)
      val pairs = for {
        reduction <- Vector(
          YannakakisReductionMode.FullTwoPass,
          YannakakisReductionMode.BottomUpOnly,
          YannakakisReductionMode.NoReduction
        )
        directive <- PostgreSqlCteMaterializationDirective.Values
      } yield reduction -> directive
      val actions = pairs.zipWithIndex.map { case ((reduction, directive), index) =>
        // The first two aliases deliberately share exact bytes so publication
        // exercises content-addressed payload deduplication.
        val payloadIndex = if (index == 1) 0 else index
        PostgreSqlPhysicalSqlExportCli.Action(
          ordinal = index + 1,
          physicalVariantId = id("abcdef012".charAt(index)),
          reductionMode = reduction,
          cteDirective = directive,
          sql = s"WITH q AS (SELECT $payloadIndex) SELECT * FROM q"
        )
      }
      Right(
        PostgreSqlPhysicalSqlExportCli.Generated(
          rendererVersion = PostgreSqlPhysicalSqlExportCli.RendererVersion,
          profileId = PostgreSqlPhysicalSqlExportCli.RendererProfile,
          rank = config.rank,
          candidateId = id('a'),
          decompositionId = id('a'),
          configuration = BagFactorPhysicalConfiguration(
            bagId(0),
            Vector(BagFactorCoverAssignment(bagId(0), Vector(edgeId(2))))
          ),
          actions = actions
        )
      )
    }
  }

  test("publishes one private canonical secret-free nine-alias SQL bundle") {
    val parent = Files.createTempDirectory("yrw-physical-export-")
    val sqlFile = parent.resolve("input.sql")
    val output = parent.resolve("export")
    Files.write(sqlFile, sourceSql.getBytes(StandardCharsets.UTF_8))
    val backend = new FixtureBackend()
    val errors = new ByteArrayOutputStream()

    val status = PostgreSqlPhysicalSqlExportCli.run(
      arguments(sqlFile, output),
      Map(passwordName -> password),
      errors,
      backend
    )
    assertEquals(status, PostgreSqlRewriteExitCode.Rewritten)
    assertEquals(errors.size(), 0)
    assertEquals(backend.calls, 1)
    assertEquals(backend.observedPassword, Some(password))
    assertEquals(
      Files.getPosixFilePermissions(output),
      PosixFilePermissions.fromString("rwx------")
    )
    assertEquals(
      Files.getPosixFilePermissions(output.resolve("actions")),
      PosixFilePermissions.fromString("rwx------")
    )

    val topLevel = Using.resource(Files.list(output))(
      _.iterator().asScala.map(_.getFileName.toString).toVector.sorted
    )
    assertEquals(
      topLevel,
      Vector("actions", "manifest.json", "native.sql", "source.sql")
    )
    assertEquals(Files.readString(output.resolve("source.sql")), sourceSql)
    assertEquals(Files.readString(output.resolve("native.sql")), sourceSql)

    val manifestBytes = Files.readAllBytes(output.resolve("manifest.json"))
    val manifest = new String(manifestBytes, StandardCharsets.UTF_8)
    assert(manifest.endsWith("\n"))
    val root = mapper.readTree(manifestBytes)
    assertEquals(
      root.fieldNames().asScala.toVector,
      Vector(
        "actions",
        "candidate",
        "configuration",
        "contains_database_results",
        "contains_execution_outcomes",
        "contains_timing",
        "files",
        "input",
        "renderer",
        "schema_version",
        "status"
      )
    )
    assertEquals(
      root.path("schema_version").asText(),
      PostgreSqlPhysicalSqlExportCli.SchemaVersion
    )
    assertEquals(root.path("status").asText(), "development-only-nonreportable")
    assert(!root.path("contains_database_results").asBoolean())
    assert(!root.path("contains_execution_outcomes").asBoolean())
    assert(!root.path("contains_timing").asBoolean())
    assertEquals(root.path("renderer").path("version").asText(), "current-v3")
    assertEquals(
      root.path("renderer").path("profile").asText(),
      "statistics-safe-v2"
    )
    assertEquals(root.path("candidate").path("rank").asInt(), 2)
    assert(root.path("candidate").path("configuration_id").asText().matches("sha256:[0-9a-f]{64}"))
    assertEquals(root.path("configuration").path("execution_root").asInt(), 0)
    assertEquals(
      root
        .path("configuration")
        .path("cover_assignments")
        .path(0)
        .path("edge_ids")
        .path(0)
        .asInt(),
      2
    )
    assertEquals(root.path("input").path("schema").asText(), "fixture_schema")

    val actions = root.path("actions").elements().asScala.toVector
    assertEquals(actions.size, 9)
    assertEquals(
      actions.map(_.path("alias_id").asText()),
      Vector(
        "full:planner-controlled",
        "full:materialized",
        "full:not-materialized",
        "bottom-up:planner-controlled",
        "bottom-up:materialized",
        "bottom-up:not-materialized",
        "none:planner-controlled",
        "none:materialized",
        "none:not-materialized"
      )
    )
    assertEquals(actions.map(_.path("action_id").asText()).distinct.size, 9)
    assertEquals(
      actions.map(_.path("action_id").asText()),
      actions.map(_.path("physical_variant_id").asText())
    )
    assertEquals(
      actions.map(_.path("physical_variant_id").asText()).distinct.size,
      9
    )
    val payloads = root.path("files").path("payloads").elements().asScala.toVector
    assertEquals(payloads.size, 8)
    assertEquals(
      payloads.map(_.path("payload_id").asText()),
      payloads.map(_.path("payload_id").asText()).sorted
    )
    actions.foreach { action =>
      val reference = action.path("sql")
      val path = output.resolve(reference.path("path").asText())
      val bytes = Files.readAllBytes(path)
      assertEquals(bytes.length.toLong, reference.path("bytes").asLong())
      assertEquals(sha256(bytes), reference.path("sha256").asText())
      assertEquals(
        action.path("payload_id").asText(),
        path.getFileName.toString.stripSuffix(".sql")
      )
    }

    val files = Using.resource(Files.walk(output))(
      _.iterator().asScala.filter(Files.isRegularFile(_)).toVector
    )
    files.foreach { path =>
      assertEquals(
        Files.getPosixFilePermissions(path),
        PosixFilePermissions.fromString("rw-------")
      )
    }
    val retainedText = files
      .map(path => new String(Files.readAllBytes(path), StandardCharsets.UTF_8))
      .mkString("\n")
    Vector(password, passwordName, jdbcUrl, user).foreach(secret =>
      assert(!retainedText.contains(secret))
    )

    val secondBackend = new FixtureBackend()
    val secondErrors = new ByteArrayOutputStream()
    val secondStatus = PostgreSqlPhysicalSqlExportCli.run(
      arguments(sqlFile, output),
      Map(passwordName -> password),
      secondErrors,
      secondBackend
    )
    assertEquals(secondStatus, PostgreSqlRewriteExitCode.InputError)
    assertEquals(secondBackend.calls, 0)
    assertEquals(
      secondErrors.toString(StandardCharsets.UTF_8),
      "postgresql-physical-sql-export error: output-exists\n"
    )
  }

  test("rejects an absent password or rank outside the requested prefix") {
    val parent = Files.createTempDirectory("yrw-physical-export-invalid-")
    val sqlFile = parent.resolve("input.sql")
    Files.write(sqlFile, sourceSql.getBytes(StandardCharsets.UTF_8))
    val backend = new FixtureBackend()
    val missingErrors = new ByteArrayOutputStream()
    val missingStatus = PostgreSqlPhysicalSqlExportCli.run(
      arguments(sqlFile, parent.resolve("missing")),
      Map.empty,
      missingErrors,
      backend
    )
    assertEquals(missingStatus, PostgreSqlRewriteExitCode.InvalidInvocation)
    assertEquals(backend.calls, 0)
    assertEquals(
      missingErrors.toString(StandardCharsets.UTF_8),
      "postgresql-physical-sql-export error: password-environment-unavailable\n"
    )

    val invalid = arguments(sqlFile, parent.resolve("invalid"))
    invalid(invalid.indexOf("--rank") + 1) = "4"
    invalid(invalid.indexOf("--plan-limit") + 1) = "3"
    val invalidErrors = new ByteArrayOutputStream()
    val invalidStatus = PostgreSqlPhysicalSqlExportCli.run(
      invalid,
      Map(passwordName -> password),
      invalidErrors,
      backend
    )
    assertEquals(invalidStatus, PostgreSqlRewriteExitCode.InvalidInvocation)
    assertEquals(backend.calls, 0)
    assert(!Files.exists(parent.resolve("invalid")))
  }

  private def arguments(sqlFile: Path, output: Path): Array[String] = Array(
    "--jdbc-url", jdbcUrl,
    "--user", user,
    "--password-env", passwordName,
    "--schema", "fixture_schema",
    "--sql-file", sqlFile.toString,
    "--width", "3",
    "--require-connected-cover", "true",
    "--plan-limit", "3",
    "--rank", "2",
    "--output-dir", output.toString
  )

  private def id(character: Char): String = "sha256:" + character.toString * 64

  private def bagId(value: Int): BagId =
    BagId.from(value).fold(error => fail(error.message), identity)

  private def edgeId(value: Int): EdgeId =
    EdgeId.from(value).fold(error => fail(error.message), identity)

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
