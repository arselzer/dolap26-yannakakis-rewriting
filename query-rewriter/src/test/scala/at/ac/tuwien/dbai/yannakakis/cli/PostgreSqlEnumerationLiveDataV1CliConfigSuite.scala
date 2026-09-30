package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.nio.channels.FileChannel
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, StandardOpenOption}
import java.security.MessageDigest

import scala.jdk.CollectionConverters._
import scala.util.Using

import munit.FunSuite

final class PostgreSqlEnumerationLiveDataV1CliConfigSuite extends FunSuite {
  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment -> "jdbc:postgresql://fixture/db",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "fixture-user",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "fixture-password"
  )

  test("configuration accepts only the two path arguments and JDBC environment") {
    val cwd = Path.of("/").resolve("tmp").toAbsolutePath.normalize()
    val parsed = PostgreSqlEnumerationLiveDataV1CliConfig.parse(
      Array("--request-bundle", "request", "--output-assembly", "raw"),
      environment,
      cwd
    )
    assert(parsed.isRight)
    assertEquals(parsed.toOption.get.requestBundle, cwd.resolve("request"))
    assert(
      PostgreSqlEnumerationLiveDataV1CliConfig.parse(
        Array(
          "--request-bundle", "request",
          "--output-assembly", "raw",
          "--schema", "hostile"
        ),
        environment,
        cwd
      ).isLeft
    )
    assert(
      PostgreSqlEnumerationLiveDataV1CliConfig.parse(
        Array("--request-bundle", "same", "--output-assembly", "same"),
        environment,
        cwd
      ).isLeft
    )
    assert(
      PostgreSqlEnumerationLiveDataV1CliConfig.parse(
        Array("--request-bundle", "request", "--output-assembly", "request/child"),
        environment,
        cwd
      ).isLeft
    )
    assert(
      PostgreSqlEnumerationLiveDataV1CliConfig.parse(
        Array("--request-bundle", "../request", "--output-assembly", "raw"),
        environment,
        cwd
      ).isLeft
    )
    assert(
      PostgreSqlEnumerationLiveDataV1CliConfig.parse(
        Array("--request-bundle", "request", "--output-assembly", "raw"),
        environment - PostgreSqlRewriteCliConfig.PasswordEnvironment,
        cwd
      ).isLeft
    )
  }

  test("publisher exposes only one private completion-last raw envelope") {
    val parent = Files.createTempDirectory("yrw-live-data-publisher-")
    Files.setPosixFilePermissions(parent, PosixFilePermissions.fromString("rwx------"))
    val output = parent.resolve("raw")
    val result = PostgreSqlEnumerationLiveDataV1Publisher.withWorkspace(output) { workspace =>
      assert(Files.isDirectory(output))
      assert(!Files.exists(output.resolve("complete.json")))
      val table = workspace.copyOutDirectory.resolve("edge.copy")
      Files.write(table, "1\t2\n".getBytes(StandardCharsets.UTF_8))
      Files.setPosixFilePermissions(table, PosixFilePermissions.fromString("rw-------"))
      for {
        _ <- workspace.writeAssembly("{}\n".getBytes(StandardCharsets.UTF_8))
        _ <- workspace.writeComplete("{}\n".getBytes(StandardCharsets.UTF_8), Vector("edge"))
        _ <- workspace.publish()
      } yield ()
    }
    assertEquals(result, Right(()))
    assertEquals(
      Files.getPosixFilePermissions(output),
      PosixFilePermissions.fromString("rwx------")
    )
    assertEquals(
      Using.resource(Files.list(output))(
        _.iterator().asScala.map(_.getFileName.toString).toVector.sorted
      ),
      Vector("assembly.json", "complete.json", "copy-out")
    )
    assertEquals(
      Files.getPosixFilePermissions(output.resolve("assembly.json")),
      PosixFilePermissions.fromString("rw-------")
    )
    var entered = false
    val second = PostgreSqlEnumerationLiveDataV1Publisher.withWorkspace(output) { _ =>
      entered = true
      Right(())
    }
    assertEquals(
      second.left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1PublicationFailure.OutputExists)
    )
    assert(!entered)
  }

  test("publisher reserves the final target without replacing an existing directory") {
    val parent = Files.createTempDirectory("yrw-live-data-reservation-")
    Files.setPosixFilePermissions(parent, PosixFilePermissions.fromString("rwx------"))
    val output = parent.resolve("raw")
    Files.createDirectory(output)
    Files.setPosixFilePermissions(output, PosixFilePermissions.fromString("rwx------"))
    val sentinel = output.resolve("sentinel")
    Files.write(sentinel, Array[Byte](1))
    Files.setPosixFilePermissions(sentinel, PosixFilePermissions.fromString("rw-------"))
    var entered = false
    val result = PostgreSqlEnumerationLiveDataV1Publisher.withWorkspace(output) { _ =>
      entered = true
      Right(())
    }
    assertEquals(
      result.left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1PublicationFailure.OutputExists)
    )
    assert(!entered)
    assertEquals(Files.readAllBytes(sentinel).toVector, Vector[Byte](1))
  }

  test("executing archive hashing accepts exact bytes and rejects either binding drift") {
    val parent = Files.createTempDirectory("yrw-live-data-archive-")
    val archive = parent.resolve("rewrite-assembly.jar")
    val bytes = "exact executable archive fixture\n".getBytes(StandardCharsets.UTF_8)
    Files.write(archive, bytes)
    val sha256 = MessageDigest.getInstance("SHA-256").digest(bytes)
      .map(byte => f"${byte & 0xff}%02x").mkString
    val expected = PostgreSqlEnumerationLiveDataV1AssemblyBinding(bytes.length.toLong, sha256)
    assertEquals(
      PostgreSqlEnumerationLiveDataV1ExecutingAssembly.verifyPath(archive, expected),
      Right(())
    )
    assertEquals(
      PostgreSqlEnumerationLiveDataV1ExecutingAssembly.verifyPath(
        archive,
        expected.copy(sha256 = "0" * 64)
      ).left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure.BindingMismatch)
    )
    assertEquals(
      PostgreSqlEnumerationLiveDataV1ExecutingAssembly.verifyPath(
        archive,
        expected.copy(byteLength = expected.byteLength + 1L)
      ).left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure.BindingMismatch)
    )
  }

  test("executing archive hashing rejects indirect, nonregular, and oversized sources") {
    val parent = Files.createTempDirectory("yrw-live-data-archive-hostile-")
    val archive = parent.resolve("rewrite-assembly.jar")
    Files.write(archive, "archive\n".getBytes(StandardCharsets.UTF_8))
    val expected = PostgreSqlEnumerationLiveDataV1AssemblyBinding(8L, "0" * 64)
    val alias = parent.resolve("alias.jar")
    Files.createSymbolicLink(alias, archive.getFileName)
    val directory = parent.resolve("directory.jar")
    Files.createDirectory(directory)
    val oversized = parent.resolve("oversized.jar")
    val channel = FileChannel.open(
      oversized,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )
    try {
      channel.position(PostgreSqlEnumerationLiveDataV1RequestParser.MaximumScalaAssemblyBytes)
      channel.write(java.nio.ByteBuffer.wrap(Array[Byte](1)))
    } finally channel.close()
    Vector(alias, directory, oversized).foreach { path =>
      assertEquals(
        PostgreSqlEnumerationLiveDataV1ExecutingAssembly.verifyPath(path, expected)
          .left.toOption,
        Some(PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure.InvalidArchive)
      )
    }
  }

  test("publisher refuses completion until every exact private COPY child exists") {
    val parent = Files.createTempDirectory("yrw-live-data-incomplete-")
    Files.setPosixFilePermissions(parent, PosixFilePermissions.fromString("rwx------"))
    val output = parent.resolve("raw")
    val result = PostgreSqlEnumerationLiveDataV1Publisher.withWorkspace(output) { workspace =>
      for {
        _ <- workspace.writeAssembly("{}\n".getBytes(StandardCharsets.UTF_8))
        _ <- workspace.writeComplete("{}\n".getBytes(StandardCharsets.UTF_8), Vector("missing"))
      } yield ()
    }
    assertEquals(
      result.left.toOption,
      Some(PostgreSqlEnumerationLiveDataV1PublicationFailure.EnvelopeMismatch)
    )
    assert(!Files.exists(output))
  }
}
