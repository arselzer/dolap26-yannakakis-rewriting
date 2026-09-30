package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}

import scala.jdk.CollectionConverters._

class PostgreSqlEnumerationCampaignV2AcquisitionFilesSuite
    extends munit.FunSuite {
  import PostgreSqlEnumerationCampaignV2AcquisitionFiles._
  import PostgreSqlEnumerationCampaignV2FileFailure._

  private val assemblyBytes = "{\"assembly\":true}\n".getBytes(StandardCharsets.UTF_8)
  private val packageBytes = "{\"package\":true}\n".getBytes(StandardCharsets.UTF_8)

  test("authoring loader returns an immutable bounded regular-file snapshot") {
    withTestDirectory { root =>
      val input = root.resolve("authoring.json")
      val original = "{\"request\":1}\n".getBytes(StandardCharsets.UTF_8)
      writeNew(input, original)

      val loaded = readAuthoring(input).fold(problem => fail(problem.code), identity)
      assertEquals(loaded.byteLength, original.length)
      assertEquals(loaded.bytes.toVector, original.toVector)
      val callerCopy = loaded.bytes
      callerCopy(0) = 'x'.toByte
      assertEquals(loaded.bytes.toVector, original.toVector)

      assertEquals(
        readAuthoringWithMaximumBytes(input, original.length - 1),
        Left(InputSizeInvalid)
      )
      assertEquals(readAuthoring(root), Left(InputNotRegular))
    }
  }

  test("authoring loader rejects symlinks in the final and parent components") {
    withTestDirectory { root =>
      val realParent = Files.createDirectory(root.resolve("real"))
      val target = realParent.resolve("authoring.json")
      writeNew(target, assemblyBytes)

      val finalLink = root.resolve("authoring-link.json")
      Files.createSymbolicLink(finalLink, target)
      assertEquals(readAuthoring(finalLink), Left(UnsafePathComponent))

      val parentLink = root.resolve("parent-link")
      Files.createSymbolicLink(parentLink, realParent)
      assertEquals(
        readAuthoring(parentLink.resolve("authoring.json")),
        Left(UnsafePathComponent)
      )
    }
  }

  test("authoring and output paths reject explicit dot-dot components") {
    withTestDirectory { root =>
      val nested = Files.createDirectory(root.resolve("nested"))
      val input = root.resolve("authoring.json")
      writeNew(input, assemblyBytes)
      val ambiguousInput = nested.resolve("..").resolve("authoring.json")
      val ambiguousOutput = nested.resolve("..").resolve("output.json")

      assertEquals(readAuthoring(ambiguousInput), Left(AmbiguousPath))
      assertEquals(
        withWorkspace(ambiguousOutput)(_ => Right(())),
        Left(AmbiguousPath)
      )
    }
  }

  test("workspace creates 0700 staging, fsynced 0600 files, and a hard-link publication") {
    withTestDirectory { root =>
      val output = root.resolve("result.json")
      var staging: Path = null

      val result = withWorkspace(output) { workspace =>
        staging = workspace.stagingDirectory
        assertEquals(
          Files.getPosixFilePermissions(staging, LinkOption.NOFOLLOW_LINKS),
          PosixFilePermissions.fromString("rwx------")
        )
        for {
          _ <- workspace.writeAssembly(assemblyBytes)
          _ = assertEquals(
            workspace.writeAssembly(assemblyBytes),
            Left(AssemblyExists)
          )
          _ <- workspace.writeStagedPackage(packageBytes)
          _ = assertEquals(
            workspace.writeStagedPackage(packageBytes),
            Left(StagedPackageExists)
          )
          _ = assertEquals(
            Files.getPosixFilePermissions(
              workspace.assemblyPath,
              LinkOption.NOFOLLOW_LINKS
            ),
            PosixFilePermissions.fromString("rw-------")
          )
          _ <- workspace.publish()
          _ = assert(Files.isSameFile(output, workspace.stagedPackagePath))
        } yield ()
      }

      assertEquals(result, Right(()))
      assert(staging != null && Files.notExists(staging, LinkOption.NOFOLLOW_LINKS))
      assertEquals(Files.readAllBytes(output).toVector, packageBytes.toVector)
      assertEquals(
        Files.getPosixFilePermissions(output, LinkOption.NOFOLLOW_LINKS),
        PosixFilePermissions.fromString("rw-------")
      )
    }
  }

  test("workspace adopts and fsyncs a composer-created staged package") {
    withTestDirectory { root =>
      val output = root.resolve("result.json")
      val result = withWorkspace(output) { workspace =>
        for {
          _ <- workspace.writeAssembly(assemblyBytes)
          _ = writeNew(workspace.stagedPackagePath, packageBytes)
          _ <- workspace.adoptStagedPackage()
          _ = assertEquals(
            Files.getPosixFilePermissions(
              workspace.stagedPackagePath,
              LinkOption.NOFOLLOW_LINKS
            ),
            PosixFilePermissions.fromString("rw-------")
          )
          _ <- workspace.publish()
        } yield ()
      }

      assertEquals(result, Right(()))
      assertEquals(Files.readAllBytes(output).toVector, packageBytes.toVector)
    }
  }

  test("workspace rejects an aliased composer package before changing permissions") {
    withTestDirectory { root =>
      val outside = root.resolve("outside.json")
      writeNew(outside, packageBytes)
      val originalPermissions =
        Files.getPosixFilePermissions(outside, LinkOption.NOFOLLOW_LINKS)
      var staging: Path = null

      val result = withWorkspace(root.resolve("result.json")) { workspace =>
        staging = workspace.stagingDirectory
        Files.createLink(workspace.stagedPackagePath, outside)
        assert(Files.isSameFile(workspace.stagedPackagePath, outside))
        workspace.adoptStagedPackage()
      }

      assertEquals(result, Left(StagedPackageInvalid))
      assertEquals(
        Files.getPosixFilePermissions(outside, LinkOption.NOFOLLOW_LINKS),
        originalPermissions
      )
      assertEquals(Files.readAllBytes(outside).toVector, packageBytes.toVector)
      assert(Files.notExists(staging, LinkOption.NOFOLLOW_LINKS))
    }
  }

  test("invalid partial composer output is identity-bound and exactly cleaned") {
    withTestDirectory { root =>
      val output = root.resolve("result.json")
      var staging: Path = null
      val result = withWorkspace(output) { workspace =>
        staging = workspace.stagingDirectory
        writeNew(workspace.stagedPackagePath, packageBytes)
        workspace.adoptStagedPackage(maximumBytes = packageBytes.length - 1)
      }

      assertEquals(result, Left(StagedPackageInvalid))
      assert(Files.notExists(staging, LinkOption.NOFOLLOW_LINKS))
      assert(Files.notExists(output, LinkOption.NOFOLLOW_LINKS))
    }
  }

  test("pre-existing output and dangling output symlink are never replaced") {
    withTestDirectory { root =>
      val existing = root.resolve("existing.json")
      val sentinel = "sentinel".getBytes(StandardCharsets.UTF_8)
      writeNew(existing, sentinel)
      assertEquals(
        withWorkspace(existing)(_ => fail("operation must not run")),
        Left(OutputExists)
      )
      assertEquals(Files.readAllBytes(existing).toVector, sentinel.toVector)

      val dangling = root.resolve("dangling.json")
      Files.createSymbolicLink(dangling, root.resolve("missing-target"))
      assertEquals(
        withWorkspace(dangling)(_ => fail("operation must not run")),
        Left(OutputExists)
      )
      assert(Files.isSymbolicLink(dangling))
    }
  }

  test("output parent symlinks are rejected before staging is created") {
    withTestDirectory { root =>
      val realParent = Files.createDirectory(root.resolve("real-output"))
      val parentLink = root.resolve("output-link")
      Files.createSymbolicLink(parentLink, realParent)

      assertEquals(
        withWorkspace(parentLink.resolve("result.json"))(_ => Right(())),
        Left(UnsafePathComponent)
      )
      assertEquals(listChildren(realParent), Vector.empty)
    }
  }

  test("a publication race preserves the independently created output") {
    withTestDirectory { root =>
      val output = root.resolve("result.json")
      val intruder = "independent".getBytes(StandardCharsets.UTF_8)
      var staging: Path = null
      val result = withWorkspace(output) { workspace =>
        staging = workspace.stagingDirectory
        for {
          _ <- workspace.writeStagedPackage(packageBytes)
          _ = writeNew(output, intruder)
          _ <- workspace.publish()
        } yield ()
      }

      assertEquals(result, Left(OutputExists))
      assertEquals(Files.readAllBytes(output).toVector, intruder.toVector)
      assert(Files.notExists(staging, LinkOption.NOFOLLOW_LINKS))
    }
  }

  test("a post-link replacement is indeterminate, terminal, and never deleted") {
    withTestDirectory { root =>
      val output = root.resolve("result.json")
      val displacedLink = root.resolve("displaced-package.json")
      val independent = "independent".getBytes(StandardCharsets.UTF_8)
      var staging: Path = null
      var secondPublish: Either[
        PostgreSqlEnumerationCampaignV2FileFailure,
        Unit
      ] = null

      val result = withWorkspaceAfterHardLinkForTest(output, linked => {
        Files.move(linked, displacedLink)
        writeNew(linked, independent)
      }) { workspace =>
        staging = workspace.stagingDirectory
        workspace.writeStagedPackage(packageBytes).flatMap { _ =>
          val first = workspace.publish()
          secondPublish = workspace.publish()
          first
        }
      }

      assertEquals(result, Left(PublishIndeterminate))
      assertEquals(secondPublish, Left(OutputExists))
      assertEquals(Files.readAllBytes(output).toVector, independent.toVector)
      assertEquals(
        Files.readAllBytes(displacedLink).toVector,
        packageBytes.toVector
      )
      assert(Files.notExists(staging, LinkOption.NOFOLLOW_LINKS))
    }
  }

  test("a post-link validation exception preserves the exact published name") {
    withTestDirectory { root =>
      val output = root.resolve("result.json")
      var staging: Path = null
      val result = withWorkspaceAfterHardLinkForTest(
        output,
        _ => throw new IllegalStateException("private detail")
      ) { workspace =>
        staging = workspace.stagingDirectory
        workspace.writeStagedPackage(packageBytes).flatMap(_ => workspace.publish())
      }

      assertEquals(result, Left(PublishIndeterminate))
      assertEquals(Files.readAllBytes(output).toVector, packageBytes.toVector)
      assert(Files.notExists(staging, LinkOption.NOFOLLOW_LINKS))
    }
  }

  test("operation failures and nonfatal exceptions still clean only the owned staging directory") {
    withTestDirectory { root =>
      val failedOutput = root.resolve("failed.json")
      var failedStaging: Path = null
      val failed = withWorkspace(failedOutput) { workspace =>
        failedStaging = workspace.stagingDirectory
        workspace.writeAssembly(assemblyBytes).flatMap(_ => Left(OperationFailed))
      }
      assertEquals(failed, Left(OperationFailed))
      assert(Files.notExists(failedStaging, LinkOption.NOFOLLOW_LINKS))
      assert(Files.notExists(failedOutput, LinkOption.NOFOLLOW_LINKS))

      val thrownOutput = root.resolve("thrown.json")
      var thrownStaging: Path = null
      val thrown = withWorkspace(thrownOutput) { workspace =>
        thrownStaging = workspace.stagingDirectory
        throw new IllegalStateException("private path")
      }
      assertEquals(thrown, Left(OperationFailed))
      assert(Files.notExists(thrownStaging, LinkOption.NOFOLLOW_LINKS))
      assert(Files.notExists(thrownOutput, LinkOption.NOFOLLOW_LINKS))
    }
  }

  test("control throwables propagate only after exact staging cleanup") {
    withTestDirectory { root =>
      object Stop extends scala.util.control.ControlThrowable
      var staging: Path = null
      var propagated = false

      try {
        withWorkspace(root.resolve("result.json")) { workspace =>
          staging = workspace.stagingDirectory
          workspace.writeAssembly(assemblyBytes).fold(
            failure => fail(failure.code),
            _ => throw Stop
          )
        }
      } catch {
        case Stop => propagated = true
      }

      assert(propagated)
      assert(staging != null)
      assert(Files.notExists(staging, LinkOption.NOFOLLOW_LINKS))
    }
  }

  test("mapped workspace preserves caller orchestration errors and maps file failures") {
    withTestDirectory { root =>
      sealed trait OrchestrationFailure
      final case class FileBoundary(
          failure: PostgreSqlEnumerationCampaignV2FileFailure
      ) extends OrchestrationFailure
      case object ComposerRejected extends OrchestrationFailure
      case object Unexpected extends OrchestrationFailure

      val output = root.resolve("result.json")
      var staging: Path = null
      val composerFailure = withMappedWorkspace[OrchestrationFailure, Unit](
        output,
        FileBoundary.apply,
        Unexpected
      ) { workspace =>
        staging = workspace.stagingDirectory
        workspace
          .writeAssembly(assemblyBytes)
          .left
          .map(FileBoundary.apply)
          .flatMap(_ => Left(ComposerRejected))
      }
      assertEquals(composerFailure, Left(ComposerRejected))
      assert(Files.notExists(staging, LinkOption.NOFOLLOW_LINKS))

      writeNew(output, packageBytes)
      val fileFailure = withMappedWorkspace[OrchestrationFailure, Unit](
        output,
        FileBoundary.apply,
        Unexpected
      )(_ => Right(()))
      assertEquals(fileFailure, Left(FileBoundary(OutputExists)))
    }
  }

  test("cleanup refuses unknown children instead of recursively deleting them") {
    withTestDirectory { root =>
      val output = root.resolve("result.json")
      var staging: Path = null
      val result = withWorkspace(output) { workspace =>
        staging = workspace.stagingDirectory
        writeNew(staging.resolve("unknown.txt"), Array[Byte](1))
        Right(())
      }

      assertEquals(result, Left(CleanupFailed))
      assert(Files.isDirectory(staging, LinkOption.NOFOLLOW_LINKS))
      assertEquals(
        Files.readAllBytes(staging.resolve("unknown.txt")).toVector,
        Vector[Byte](1)
      )
    }
  }

  test("stable failures contain no exception message or filesystem path") {
    withTestDirectory { root =>
      val missing = root.resolve("private-name.json")
      val failure = readAuthoring(missing).left.toOption.getOrElse(
        fail("missing input unexpectedly loaded")
      )
      assertEquals(failure, InputUnavailable)
      assert(!failure.code.contains(root.toString))
      assert(!failure.toString.contains(root.toString))
      assertEquals(failure.productIterator.toVector, Vector.empty)
    }
  }

  private def writeNew(path: Path, bytes: Array[Byte]): Unit = {
    Files.write(path, bytes, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
    ()
  }

  private def listChildren(path: Path): Vector[Path] = {
    val stream = Files.newDirectoryStream(path)
    try stream.iterator().asScala.toVector
    finally stream.close()
  }

  private def withTestDirectory[A](operation: Path => A): A = {
    val root = Files.createTempDirectory(
      "yrw-acquisition-files-suite-",
      PosixFilePermissions.asFileAttribute(
        PosixFilePermissions.fromString("rwx------")
      )
    )
    try operation(root)
    finally deleteTestTree(root)
  }

  private def deleteTestTree(root: Path): Unit = {
    if (Files.notExists(root, LinkOption.NOFOLLOW_LINKS)) return
    val stream = Files.walk(root)
    val paths =
      try stream.iterator().asScala.toVector.sortBy(_.getNameCount).reverse
      finally stream.close()
    paths.foreach(path => Files.deleteIfExists(path))
  }
}
