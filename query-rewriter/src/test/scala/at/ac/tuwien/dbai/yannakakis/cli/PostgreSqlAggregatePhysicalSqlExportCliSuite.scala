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
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlAggregatePhysicalSweepV3,
  PostgreSqlAggregatePhysicalSweepV3Family,
  PostgreSqlCteMaterializationDirective
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId}
import com.fasterxml.jackson.databind.ObjectMapper

import scala.jdk.CollectionConverters._

final class PostgreSqlAggregatePhysicalSqlExportCliSuite extends munit.FunSuite {
  private val mapper = new ObjectMapper()
  private val passwordName = "AGG_EXPORT_PASSWORD"
  private val password = "fixture-secret"
  private val sourceSql = "SELECT COUNT(*) FROM r JOIN s ON r.k = s.k;\n"

  private final class FixtureBackend
      extends PostgreSqlAggregatePhysicalSqlExportBackend {
    var calls = 0
    var planningSql = ""

    override def generate(
        config: PostgreSqlAggregatePhysicalSqlExportCli.Config,
        sql: String
    ): Either[
      PostgreSqlAggregatePhysicalSqlExportCli.Failure,
      PostgreSqlAggregatePhysicalSqlExportCli.Generated
    ] = {
      calls += 1
      planningSql = sql
      val forms =
        if (
          config.widthBound == 3 &&
          (config.family == PostgreSqlAggregatePhysicalSweepV3Family.GlobalCount ||
            config.family == PostgreSqlAggregatePhysicalSweepV3Family.DistributedGroupedCount)
        )
          Vector(
            YannakakisReductionMode.FullTwoPass ->
              PostgreSqlCteMaterializationDirective.PlannerControlled
          )
        else config.family match {
        case PostgreSqlAggregatePhysicalSweepV3Family.GlobalCount |
            PostgreSqlAggregatePhysicalSweepV3Family.DistributedGroupedCount =>
          Vector(
            YannakakisReductionMode.FullTwoPass ->
              PostgreSqlCteMaterializationDirective.PlannerControlled,
            YannakakisReductionMode.FullTwoPass ->
              PostgreSqlCteMaterializationDirective.Materialized
          )
        case PostgreSqlAggregatePhysicalSweepV3Family.GuardedMin |
            PostgreSqlAggregatePhysicalSweepV3Family.GuardedMax =>
          Vector(
            YannakakisReductionMode.FullTwoPass ->
              PostgreSqlCteMaterializationDirective.PlannerControlled,
            YannakakisReductionMode.BottomUpOnly ->
              PostgreSqlCteMaterializationDirective.PlannerControlled
          )
      }
      val resultShape = config.family match {
        case PostgreSqlAggregatePhysicalSweepV3Family.GlobalCount =>
          "global-count-star"
        case PostgreSqlAggregatePhysicalSweepV3Family.DistributedGroupedCount =>
          "distributed-grouped-count-star"
        case PostgreSqlAggregatePhysicalSweepV3Family.GuardedMin |
            PostgreSqlAggregatePhysicalSweepV3Family.GuardedMax =>
          "global-guarded-extremum"
      }
      val actions = (for {
        rank <- config.family.requiredRanks
        (mode, directive) <- forms
      } yield (rank, mode, directive)).zipWithIndex.map {
        case ((rank, mode, directive), index) =>
          val modeId = mode match {
            case YannakakisReductionMode.FullTwoPass => "full"
            case YannakakisReductionMode.BottomUpOnly => "bu"
            case YannakakisReductionMode.NoReduction => "none"
          }
          val directiveId = directive match {
            case PostgreSqlCteMaterializationDirective.PlannerControlled =>
              "pc"
            case PostgreSqlCteMaterializationDirective.Materialized => "mat"
            case PostgreSqlCteMaterializationDirective.NotMaterialized =>
              "not-mat"
          }
          PostgreSqlAggregatePhysicalSqlExportCli.Action(
            index + 1,
            f"${config.family.id}-r$rank%02d-$modeId-$directiveId",
            rank,
            id(('a'.toInt + index).toChar),
            id(('1'.toInt + index).toChar),
            BagFactorPhysicalConfiguration(
              bagId(0),
              Vector(
                BagFactorCoverAssignment(
                  bagId(0),
                  Vector(edgeId(0))
                )
              )
            ),
            mode,
            directive,
            s"WITH q AS ${if (directive.isExplicit) "MATERIALIZED " else ""}(SELECT $rank) SELECT * FROM q"
          )
      }
      Right(
        PostgreSqlAggregatePhysicalSqlExportCli.Generated(
          PostgreSqlAggregatePhysicalSweepV3.ProtocolId,
          config.family,
          resultShape,
          PostgreSqlAggregatePhysicalSweepV3.AggregateProfileId,
          PostgreSqlAggregatePhysicalSweepV3.EnumerationProfileId,
          actions
        )
      )
    }
  }

  test("publishes native plus the exact four private nonreportable actions") {
    val parent = Files.createTempDirectory("yrw-aggregate-export-")
    val input = parent.resolve("input.sql")
    val output = parent.resolve("export")
    Files.writeString(input, sourceSql)
    val backend = new FixtureBackend()
    val errors = new ByteArrayOutputStream()

    val status = PostgreSqlAggregatePhysicalSqlExportCli.run(
      arguments(input, output),
      Map(passwordName -> password),
      errors,
      backend
    )
    assertEquals(status, PostgreSqlRewriteExitCode.Rewritten)
    assertEquals(errors.size(), 0)
    assertEquals(backend.calls, 1)
    assertEquals(
      backend.planningSql,
      "SELECT COUNT(*) FROM r JOIN s ON r.k = s.k\n"
    )
    assertEquals(Files.readString(output.resolve("native.sql")), sourceSql)
    assertEquals(Files.readString(output.resolve("source.sql")), sourceSql)

    val root = mapper.readTree(Files.readAllBytes(output.resolve("manifest.json")))
    assertEquals(root.path("schema_version").asText(), PostgreSqlAggregatePhysicalSqlExportCli.SchemaVersion)
    assertEquals(root.path("family").asText(), "global-count")
    assertEquals(
      root.path("semantic_execution_lane").asText(),
      "global-count-frequency-propagation"
    )
    assertEquals(root.path("group_key_count").asInt(), 0)
    assertEquals(root.path("input").path("width").asInt(), 2)
    assertEquals(root.path("producer_result_shape").asText(), "global-count-star")
    assertEquals(root.path("status").asText(), "development-only-nonreportable")
    assert(!root.path("training_eligible").asBoolean())
    assert(!root.path("contains_timing").asBoolean())
    val actions = root.path("actions").elements().asScala.toVector
    assertEquals(actions.size, 4)
    assertEquals(
      actions.map(_.path("structural_rank").asInt()),
      Vector(1, 1, 5, 5)
    )
    assertEquals(
      actions.map(_.path("alias_id").asText()),
      Vector(
        "global-count-r01-full-pc",
        "global-count-r01-full-mat",
        "global-count-r05-full-pc",
        "global-count-r05-full-mat"
      )
    )
    val fileReferences =
      Vector(
        root.path("files").path("source_sql"),
        root.path("files").path("native_sql")
      ) ++ actions.map(_.path("sql"))
    fileReferences.foreach(reference => {
      val path = output.resolve(reference.path("path").asText())
      val bytes = Files.readAllBytes(path)
      assert(Files.isRegularFile(path))
      assertEquals(bytes.length.toLong, reference.path("bytes").asLong())
      assertEquals(sha256(bytes), reference.path("sha256").asText())
      assertEquals(
        PosixFilePermissions.toString(Files.getPosixFilePermissions(path)),
        "rw-------"
      )
    })
    assertEquals(
      PosixFilePermissions.toString(Files.getPosixFilePermissions(output)),
      "rwx------"
    )
    assertEquals(
      PosixFilePermissions.toString(
        Files.getPosixFilePermissions(output.resolve("actions"))
      ),
      "rwx------"
    )
    val retained = Files
      .walk(output)
      .iterator()
      .asScala
      .filter(Files.isRegularFile(_))
      .map(Files.readString(_))
      .mkString("\n")
    assert(!retained.contains(password))
    assert(!retained.contains("invalid.example"))
  }

  test("publishes guarded MAX with the guarded-extremum routing contract") {
    val parent = Files.createTempDirectory("yrw-aggregate-max-export-")
    val input = parent.resolve("input.sql")
    val output = parent.resolve("export")
    Files.writeString(
      input,
      "SELECT MAX(r.k) FROM r JOIN s ON r.k = s.k;\n"
    )
    val backend = new FixtureBackend()
    val errors = new ByteArrayOutputStream()

    val status = PostgreSqlAggregatePhysicalSqlExportCli.run(
      arguments(
        input,
        output,
        PostgreSqlAggregatePhysicalSweepV3Family.GuardedMax
      ),
      Map(passwordName -> password),
      errors,
      backend
    )
    assertEquals(status, PostgreSqlRewriteExitCode.Rewritten)
    assertEquals(errors.size(), 0)
    assertEquals(backend.calls, 1)

    val root = mapper.readTree(
      Files.readAllBytes(output.resolve("manifest.json"))
    )
    assertEquals(root.path("family").asText(), "guarded-max")
    assertEquals(
      root.path("semantic_execution_lane").asText(),
      "guarded-extremum"
    )
    assertEquals(
      root.path("producer_result_shape").asText(),
      "global-guarded-extremum"
    )
    assertEquals(root.path("group_key_count").asInt(), 0)
    assert(!root.path("training_eligible").asBoolean())
    assertEquals(
      root
        .path("actions")
        .elements()
        .asScala
        .toVector
        .map(_.path("alias_id").asText()),
      Vector(
        "guarded-max-r01-full-pc",
        "guarded-max-r01-bu-pc",
        "guarded-max-r02-full-pc",
        "guarded-max-r02-bu-pc"
      )
    )
  }

  test("publishes an explicit width-3 aggregate arm without changing the default") {
    val parent = Files.createTempDirectory("yrw-aggregate-width3-export-")
    val input = parent.resolve("input.sql")
    val output = parent.resolve("export")
    Files.writeString(input, sourceSql)
    val backend = new FixtureBackend()
    val errors = new ByteArrayOutputStream()

    val status = PostgreSqlAggregatePhysicalSqlExportCli.run(
      arguments(
        input,
        output,
        PostgreSqlAggregatePhysicalSweepV3Family.GuardedMin,
        Some(3)
      ),
      Map(passwordName -> password),
      errors,
      backend
    )
    assertEquals(status, PostgreSqlRewriteExitCode.Rewritten)
    assertEquals(errors.size(), 0)
    assertEquals(backend.calls, 1)

    val root = mapper.readTree(
      Files.readAllBytes(output.resolve("manifest.json"))
    )
    assertEquals(
      root.path("schema_version").asText(),
      PostgreSqlAggregatePhysicalSqlExportCli.Width3SchemaVersion
    )
    assertEquals(root.path("input").path("width").asInt(), 3)
    assertEquals(root.path("family").asText(), "guarded-min")
  }

  test("publishes only planner-controlled ranks for the width-3 frequency panel") {
    for (
      family <- Vector(
        PostgreSqlAggregatePhysicalSweepV3Family.GlobalCount,
        PostgreSqlAggregatePhysicalSweepV3Family.DistributedGroupedCount
      )
    ) {
      val parent = Files.createTempDirectory("yrw-aggregate-frequency-width3-")
      val input = parent.resolve("input.sql")
      val output = parent.resolve("export")
      Files.writeString(input, sourceSql)
      val backend = new FixtureBackend()
      val errors = new ByteArrayOutputStream()

      val status = PostgreSqlAggregatePhysicalSqlExportCli.run(
        arguments(input, output, family, Some(3)),
        Map(passwordName -> password),
        errors,
        backend
      )
      assertEquals(status, PostgreSqlRewriteExitCode.Rewritten)
      assertEquals(errors.size(), 0)
      assertEquals(backend.calls, 1)

      val root = mapper.readTree(
        Files.readAllBytes(output.resolve("manifest.json"))
      )
      assertEquals(
        root.path("schema_version").asText(),
        PostgreSqlAggregatePhysicalSqlExportCli.Width3FrequencySchemaVersion
      )
      assertEquals(root.path("input").path("width").asInt(), 3)
      val actions = root.path("actions").elements().asScala.toVector
      assertEquals(actions.size, 2)
      assertEquals(
        actions.map(_.path("structural_rank").asInt()),
        family.requiredRanks
      )
      assert(
        actions.forall(
          _.path("cte_directive").asText() == "planner-controlled"
        )
      )
    }
  }

  test("rejects unregistered families and existing output before JDBC") {
    val parent = Files.createTempDirectory("yrw-aggregate-export-invalid-")
    val input = parent.resolve("input.sql")
    Files.writeString(input, sourceSql)
    val backend = new FixtureBackend()
    val bad = arguments(input, parent.resolve("bad"))
    bad(bad.indexOf("--family") + 1) = "guarded-median"
    val errors = new ByteArrayOutputStream()
    assertEquals(
      PostgreSqlAggregatePhysicalSqlExportCli.run(
        bad,
        Map(passwordName -> password),
        errors,
        backend
      ),
      PostgreSqlRewriteExitCode.InvalidInvocation
    )
    assertEquals(backend.calls, 0)

    val wrongFamily = arguments(
      input,
      parent.resolve("wrong-width3-family"),
      PostgreSqlAggregatePhysicalSweepV3Family.GuardedMax,
      Some(3)
    )
    val familyErrors = new ByteArrayOutputStream()
    assertEquals(
      PostgreSqlAggregatePhysicalSqlExportCli.run(
        wrongFamily,
        Map(passwordName -> password),
        familyErrors,
        backend
      ),
      PostgreSqlRewriteExitCode.InvalidInvocation
    )
    assertEquals(backend.calls, 0)

    val badWidth = arguments(
      input,
      parent.resolve("bad-width"),
      widthBound = Some(4)
    )
    val widthErrors = new ByteArrayOutputStream()
    assertEquals(
      PostgreSqlAggregatePhysicalSqlExportCli.run(
        badWidth,
        Map(passwordName -> password),
        widthErrors,
        backend
      ),
      PostgreSqlRewriteExitCode.InvalidInvocation
    )
    assertEquals(backend.calls, 0)

    val output = parent.resolve("existing")
    Files.createDirectory(output)
    val existingErrors = new ByteArrayOutputStream()
    assertEquals(
      PostgreSqlAggregatePhysicalSqlExportCli.run(
        arguments(input, output),
        Map(passwordName -> password),
        existingErrors,
        backend
      ),
      PostgreSqlRewriteExitCode.InputError
    )
    assertEquals(backend.calls, 0)
  }

  private def arguments(
      input: Path,
      output: Path,
      family: PostgreSqlAggregatePhysicalSweepV3Family =
        PostgreSqlAggregatePhysicalSweepV3Family.GlobalCount,
      widthBound: Option[Int] = None
  ): Array[String] = {
    val base = Array(
      "--jdbc-url",
      "jdbc:postgresql://invalid.example/fixture",
      "--user",
      "fixture-user",
      "--password-env",
      passwordName,
      "--schema",
      "fixture_schema",
      "--family",
      family.id,
      "--sql-file",
      input.toString,
      "--output-dir",
      output.toString
    )
    widthBound.fold(base)(value => base ++ Array("--width-bound", value.toString))
  }

  private def id(character: Char): String =
    "sha256:" + character.toString * 64

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def bagId(value: Int): BagId =
    BagId.from(value).fold(problem => fail(problem.message), identity)

  private def edgeId(value: Int): EdgeId =
    EdgeId.from(value).fold(problem => fail(problem.message), identity)
}
