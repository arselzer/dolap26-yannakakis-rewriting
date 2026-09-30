package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.security.MessageDigest

import scala.jdk.CollectionConverters._

import munit.FunSuite

final class PostgreSqlEnumerationSearchReplayBundleV1FilesSuite
    extends FunSuite {
  import PostgreSqlEnumerationCampaignV2AcquisitionFiles._
  import PostgreSqlEnumerationCampaignV2FileFailure._

  private val assemblyBytes =
    "{\"assembly\":true}\n".getBytes(StandardCharsets.UTF_8)
  private val packageBytes =
    "{\"package\":true}\n".getBytes(StandardCharsets.UTF_8)
  private val searchBytes =
    "{\"search\":true}\n".getBytes(StandardCharsets.UTF_8)
  private val completeBytes =
    "{\"complete\":true}\n".getBytes(StandardCharsets.UTF_8)

  test("the committed bundle has fixed private children and exact byte evidence") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      var staging: Path = null
      val result = withMappedPairWorkspace[
        PostgreSqlEnumerationCampaignV2FileFailure,
        PostgreSqlEnumerationSearchReplayBundleV1FileEvidence
      ](bundle, identity, OperationFailed) { workspace =>
        staging = workspace.stagingDirectory
        for {
          _ <- workspace.writeAssembly(assemblyBytes)
          _ <- workspace.writeStagedPackage(packageBytes)
          _ <- workspace.writeSearchInput(searchBytes)
          evidence <- workspace.stagedPackageEvidence()
          _ <- workspace.writeComplete(completeBytes)
          _ <- workspace.publish()
        } yield evidence
      }

      val evidence = result.fold(problem => fail(problem.code), identity)
      assertEquals(evidence.byteLength, packageBytes.length.toLong)
      assertEquals(evidence.sha256, sha256(packageBytes))
      assert(Files.notExists(staging, LinkOption.NOFOLLOW_LINKS))
      assertEquals(
        childNames(bundle),
        Vector("complete.json", "package.json", "search-input.json")
      )
      assertEquals(
        PosixFilePermissions.toString(
          Files.getPosixFilePermissions(bundle, LinkOption.NOFOLLOW_LINKS)
        ),
        "rwx------"
      )
      childNames(bundle).foreach { name =>
        assertEquals(
          PosixFilePermissions.toString(
            Files.getPosixFilePermissions(
              bundle.resolve(name),
              LinkOption.NOFOLLOW_LINKS
            )
          ),
          "rw-------"
        )
      }
      assertEquals(
        Files.readAllBytes(bundle.resolve("package.json")).toVector,
        packageBytes.toVector
      )
    }
  }

  test("existing bundle names and symlinked parents reject before work") {
    withTestDirectory { root =>
      val existing = Files.createDirectory(root.resolve("existing"))
      var invoked = false
      assertEquals(
        withMappedPairWorkspace[PostgreSqlEnumerationCampaignV2FileFailure, Unit](
          existing,
          identity,
          OperationFailed
        ) { _ =>
          invoked = true
          Right(())
        },
        Left(OutputExists)
      )
      assert(!invoked)

      val realParent = Files.createDirectory(root.resolve("real"))
      val linkedParent = root.resolve("linked")
      Files.createSymbolicLink(linkedParent, realParent)
      assertEquals(
        withMappedPairWorkspace[PostgreSqlEnumerationCampaignV2FileFailure, Unit](
          linkedParent.resolve("bundle"),
          identity,
          OperationFailed
        )(_ => Right(())),
        Left(UnsafePathComponent)
      )
      assertEquals(childNames(realParent), Vector.empty)
    }
  }

  test("every failure before the commit link leaves an unaccepted bundle") {
    val precommitSteps = Vector(
      "bundle-created",
      "package-linked",
      "search-input-linked",
      "data-directory-forced"
    )
    precommitSteps.foreach { failedStep =>
      withTestDirectory { root =>
        val bundle = root.resolve("bundle")
        val result = withPairWorkspaceAfterStepForTest(
          bundle,
          (step, _) =>
            if (step == failedStep)
              throw new IllegalStateException("private injected failure")
        )(writeReadyBundle)

        assertEquals(result, Left(BundleIncomplete))
        assert(Files.isDirectory(bundle, LinkOption.NOFOLLOW_LINKS))
        assert(
          Files.notExists(
            bundle.resolve("complete.json"),
            LinkOption.NOFOLLOW_LINKS
          )
        )
      }
    }
  }

  test("the final-directory create-to-identity gap is terminal and indeterminate") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      val result = withPairWorkspaceAfterStepForTest(
        bundle,
        (step, _) =>
          if (step == "bundle-path-created")
            throw new IllegalStateException("private injected failure")
      )(writeReadyBundle)

      assertEquals(result, Left(PublishIndeterminate))
      assert(Files.isDirectory(bundle, LinkOption.NOFOLLOW_LINKS))
      assertEquals(childNames(bundle), Vector.empty)
    }
  }

  test("a post-commit failure preserves the complete immutable bundle") {
    Vector("complete-linked", "complete-directory-forced", "parent-forced")
      .foreach { failedStep =>
        withTestDirectory { root =>
          val bundle = root.resolve("bundle")
          val result = withPairWorkspaceAfterStepForTest(
            bundle,
            (step, _) =>
              if (step == failedStep)
                throw new IllegalStateException("private injected failure")
          )(writeReadyBundle)

          assertEquals(result, Left(PublishIndeterminate))
          assertEquals(
            childNames(bundle),
            Vector("complete.json", "package.json", "search-input.json")
          )
          assertEquals(
            Files.readAllBytes(bundle.resolve("complete.json")).toVector,
            completeBytes.toVector
          )
        }
      }
  }

  test("a publication race preserves the independently created directory") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      val sentinel = "independent\n".getBytes(StandardCharsets.UTF_8)
      val result = withMappedPairWorkspace[
        PostgreSqlEnumerationCampaignV2FileFailure,
        Unit
      ](bundle, identity, OperationFailed) { workspace =>
        for {
          _ <- prepare(workspace)
          _ = Files.createDirectory(bundle)
          _ = writeNew(bundle.resolve("sentinel"), sentinel)
          _ <- workspace.publish()
        } yield ()
      }

      assertEquals(result, Left(OutputExists))
      assertEquals(
        Files.readAllBytes(bundle.resolve("sentinel")).toVector,
        sentinel.toVector
      )
      assertEquals(childNames(bundle), Vector("sentinel"))
    }
  }

  test("an ambiguous commit-link attempt is indeterminate") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      val result = withPairWorkspaceAfterStepForTest(
        bundle,
        (step, _) =>
          if (step == "complete-link-attempt")
            throw new IllegalStateException("private injected failure")
      )(writeReadyBundle)

      assertEquals(result, Left(PublishIndeterminate))
      assert(Files.notExists(bundle.resolve("complete.json")))
      assertEquals(
        childNames(bundle),
        Vector("package.json", "search-input.json")
      )
    }
  }

  test("unknown final children fail closed before any commit marker") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      val result = withPairWorkspaceAfterStepForTest(
        bundle,
        (step, path) =>
          if (step == "bundle-created")
            writeNew(path.resolve("unknown"), Array[Byte](1))
      )(writeReadyBundle)

      assertEquals(result, Left(BundleIncomplete))
      assertEquals(childNames(bundle), Vector("unknown"))
      assert(Files.notExists(bundle.resolve("complete.json")))
    }
  }

  test("a replaced final directory is never followed or deleted") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      val displaced = root.resolve("displaced")
      val result = withPairWorkspaceAfterStepForTest(
        bundle,
        (step, path) =>
          if (step == "bundle-created") {
            Files.move(path, displaced)
            Files.createDirectory(path)
            writeNew(path.resolve("independent"), Array[Byte](7))
          }
      )(writeReadyBundle)

      assertEquals(result, Left(BundleIncomplete))
      assertEquals(childNames(bundle), Vector("independent"))
      assertEquals(childNames(displaced), Vector.empty)
    }
  }

  test("unknown staging children are preserved instead of recursively removed") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      var staging: Path = null
      val result = withMappedPairWorkspace[
        PostgreSqlEnumerationCampaignV2FileFailure,
        Unit
      ](bundle, identity, OperationFailed) { workspace =>
        staging = workspace.stagingDirectory
        writeNew(staging.resolve("unknown"), Array[Byte](1))
        Right(())
      }

      assertEquals(result, Left(CleanupFailed))
      assert(Files.isDirectory(staging, LinkOption.NOFOLLOW_LINKS))
      assertEquals(Files.readAllBytes(staging.resolve("unknown")).toVector, Vector[Byte](1))
      // The suite owns this exact temporary tree and removes it in its fixture.
    }
  }

  test("post-link child replacement is detected before the commit marker") {
    Vector("package-linked", "search-input-linked").foreach { replacedStep =>
      withTestDirectory { root =>
        val bundle = root.resolve("bundle")
        val outside = root.resolve("outside")
        writeNew(outside, Array[Byte](9))
        val result = withPairWorkspaceAfterStepForTest(
          bundle,
          (step, path) =>
            if (step == replacedStep) {
              Files.delete(path)
              Files.createSymbolicLink(path, outside)
            }
        )(writeReadyBundle)

        assertEquals(result, Left(BundleIncomplete))
        assert(Files.isSymbolicLink(bundle.resolve(
          if (replacedStep == "package-linked") "package.json"
          else "search-input.json"
        )))
        assert(Files.notExists(bundle.resolve("complete.json")))
      }
    }
  }

  test("post-commit child replacement is indeterminate and never hidden") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      val outside = root.resolve("outside")
      writeNew(outside, Array[Byte](11))
      val result = withPairWorkspaceAfterStepForTest(
        bundle,
        (step, path) =>
          if (step == "complete-linked") {
            Files.delete(path)
            Files.createSymbolicLink(path, outside)
          }
      )(writeReadyBundle)

      assertEquals(result, Left(PublishIndeterminate))
      assert(Files.isSymbolicLink(bundle.resolve("complete.json")))
      assertEquals(
        childNames(bundle),
        Vector("complete.json", "package.json", "search-input.json")
      )
    }
  }

  test("post-link permission drift fails before logical commit") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      val result = withPairWorkspaceAfterStepForTest(
        bundle,
        (step, path) =>
          if (step == "search-input-linked")
            Files.setPosixFilePermissions(
              path,
              PosixFilePermissions.fromString("rw-r--r--")
            )
      )(writeReadyBundle)

      assertEquals(result, Left(BundleIncomplete))
      assert(Files.notExists(bundle.resolve("complete.json")))
    }
  }

  test("bundle-directory permission drift fails before logical commit") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      val result = withPairWorkspaceAfterStepForTest(
        bundle,
        (step, path) =>
          if (step == "bundle-created")
            Files.setPosixFilePermissions(
              path,
              PosixFilePermissions.fromString("rwxr-xr-x")
            )
      )(writeReadyBundle)

      assertEquals(result, Left(BundleIncomplete))
      assert(Files.notExists(bundle.resolve("complete.json")))
    }
  }

  test("cleanup cannot mask a primary post-publication failure") {
    withTestDirectory { root =>
      val bundle = root.resolve("bundle")
      val result = withPairWorkspaceAfterStepForTest(
        bundle,
        (step, _) =>
          if (step == "package-linked")
            throw new IllegalStateException("private injected failure")
      ) { workspace =>
        prepare(workspace).flatMap { _ =>
          writeNew(workspace.stagingDirectory.resolve("unknown"), Array[Byte](1))
          workspace.publish()
        }
      }

      assertEquals(result, Left(BundleIncomplete))
      assert(Files.notExists(bundle.resolve("complete.json")))
    }
  }

  private def writeReadyBundle(
      workspace: PairWorkspace
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
    prepare(workspace).flatMap(_ => workspace.publish())

  private def prepare(
      workspace: PairWorkspace
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
    for {
      _ <- workspace.writeAssembly(assemblyBytes)
      _ <- workspace.writeStagedPackage(packageBytes)
      _ <- workspace.writeSearchInput(searchBytes)
      _ <- workspace.writeComplete(completeBytes)
    } yield ()

  private def writeNew(path: Path, bytes: Array[Byte]): Unit = {
    Files.write(path, bytes, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
    ()
  }

  private def childNames(path: Path): Vector[String] = {
    val stream = Files.list(path)
    try stream.iterator().asScala.map(_.getFileName.toString).toVector.sorted
    finally stream.close()
  }

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def withTestDirectory[A](operation: Path => A): A = {
    val root = Files.createTempDirectory(
      "yrw-search-replay-bundle-files-",
      PosixFilePermissions.asFileAttribute(
        PosixFilePermissions.fromString("rwx------")
      )
    )
    try operation(root)
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
