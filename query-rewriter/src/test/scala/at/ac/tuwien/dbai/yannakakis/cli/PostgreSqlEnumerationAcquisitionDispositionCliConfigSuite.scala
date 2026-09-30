package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}

import scala.jdk.CollectionConverters._

import munit.FunSuite

final class PostgreSqlEnumerationAcquisitionDispositionCliConfigSuite
    extends FunSuite {
  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment -> "jdbc:postgresql://db/test",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "user",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "fixture"
  )

  test("the additive invocation exposes one absent normalized disposition path") {
    withTestDirectory { cwd =>
      val parsed = PostgreSqlEnumerationAcquisitionDispositionCliConfig
        .parse(validArguments, environment, cwd)
        .fold(problem => fail(problem.toString), identity)

      assertEquals(parsed.authoringRequest, cwd.resolve("request.json"))
      assertEquals(
        parsed.outputDisposition,
        cwd.resolve("out/disposition")
      )
      assert(parsed.outputDisposition.isAbsolute)
      assertEquals(parsed.outputDisposition, parsed.outputDisposition.normalize())
      assert(Files.notExists(parsed.outputDisposition, LinkOption.NOFOLLOW_LINKS))
      assertEquals(parsed.schemaName, "fixture")
      assertEquals(parsed.jdbc.password, "secret")

      val staged = cwd.resolve("private-stage/package.json")
      val backend = parsed.backendConfig(staged)
      assertEquals(backend.authoringRequest, parsed.authoringRequest)
      assertEquals(backend.outputPackage, staged)
      assertEquals(backend.pythonExecutable, parsed.pythonExecutable)
      assertEquals(backend.composerRoot, parsed.composerRoot)
      assertEquals(backend.schemaName, parsed.schemaName)
      assertEquals(backend.jdbc, parsed.jdbc)
    }
  }

  test("the option and command surfaces are closed without widening v2 acquire") {
    withTestDirectory { cwd =>
      Vector(
        "--output-bundle",
        "--output-package",
        "--sql-file",
        "--plan-limit",
        "--width-bound",
        "--connected-cover"
      ).foreach { option =>
        val result = PostgreSqlEnumerationAcquisitionDispositionCliConfig
          .parse(validArguments ++ Array(option, "x"), environment, cwd)
        assertEquals(
          result.left.toOption,
          Some(PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidArguments),
          option
        )
      }

      val legacy = PostgreSqlEnumerationSearchReplayBundleV2CliConfig.parse(
        validArguments,
        environment,
        cwd
      )
      assertEquals(
        legacy.left.toOption,
        Some(PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidArguments)
      )
      assert(
        PostgreSqlEnumerationAcquisitionDispositionCliConfig
          .parse(validArguments.updated(0, "acquire"), environment, cwd)
          .isLeft
      )
    }
  }

  test("required values are exact, unique, normalized, and path-distinct") {
    withTestDirectory { cwd =>
      val cases = Vector(
        validArguments.dropRight(2),
        validArguments ++ Array("--output-disposition", "other"),
        validArguments.updated(4, "request.json"),
        validArguments.updated(2, "../request.json"),
        validArguments.updated(4, "out/../disposition"),
        validArguments.updated(6, "python3"),
        validArguments.updated(8, "composer"),
        Array("acquire-disposition")
      )
      cases.foreach { arguments =>
        assert(
          PostgreSqlEnumerationAcquisitionDispositionCliConfig
            .parse(arguments, environment, cwd)
            .isLeft,
          arguments.mkString(" ")
        )
      }
    }
  }

  test("an existing file, directory, or dangling symlink output is rejected") {
    withTestDirectory { cwd =>
      val output = cwd.resolve("out/disposition")
      Files.createDirectories(output.getParent)
      Files.write(
        output,
        "existing\n".getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE
      )
      assertInvalidPath(validArguments, cwd)
      Files.delete(output)

      Files.createDirectory(output)
      assertInvalidPath(validArguments, cwd)
      Files.delete(output)

      Files.createSymbolicLink(output, cwd.resolve("missing-target"))
      assertInvalidPath(validArguments, cwd)
    }
  }

  test("schema override and environment-only JDBC rules match acquisition") {
    withTestDirectory { cwd =>
      val parsed = PostgreSqlEnumerationAcquisitionDispositionCliConfig
        .parse(
          validArguments ++ Array("--schema", "campaign"),
          environment,
          cwd
        )
        .fold(problem => fail(problem.toString), identity)
      assertEquals(parsed.schemaName, "campaign")

      val missingPassword = PostgreSqlEnumerationAcquisitionDispositionCliConfig
        .parse(
          validArguments,
          environment - PostgreSqlRewriteCliConfig.PasswordEnvironment,
          cwd
        )
      assertEquals(
        missingPassword.left.toOption,
        Some(
          PostgreSqlEnumerationCampaignV2CliConfigFailure.MissingConfiguration
        )
      )
    }
  }

  private val validArguments = Array(
    "acquire-disposition",
    "--authoring-request",
    "request.json",
    "--output-disposition",
    "out/disposition",
    "--python-executable",
    "/usr/bin/python3",
    "--composer-root",
    "/opt/yrw/enumeration-campaign-v2"
  )

  private def assertInvalidPath(arguments: Array[String], cwd: Path): Unit =
    assertEquals(
      PostgreSqlEnumerationAcquisitionDispositionCliConfig
        .parse(arguments, environment, cwd)
        .left
        .toOption,
      Some(PostgreSqlEnumerationCampaignV2CliConfigFailure.InvalidPath)
    )

  private def withTestDirectory[A](operation: Path => A): A = {
    val root = Files.createTempDirectory(
      "yrw-acquisition-disposition-config-",
      PosixFilePermissions.asFileAttribute(
        PosixFilePermissions.fromString("rwx------")
      )
    )
    try operation(root.toAbsolutePath.normalize())
    finally deleteOwnedTree(root)
  }

  private def deleteOwnedTree(root: Path): Unit = {
    if (Files.notExists(root, LinkOption.NOFOLLOW_LINKS)) return
    val stream = Files.walk(root)
    val paths = try stream.iterator().asScala.toVector
    finally stream.close()
    paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
  }
}
