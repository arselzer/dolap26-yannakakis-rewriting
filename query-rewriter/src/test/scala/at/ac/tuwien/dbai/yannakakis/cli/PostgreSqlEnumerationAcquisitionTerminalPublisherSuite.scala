package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, LinkOption, Path, Paths, StandardOpenOption}

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

import munit.FunSuite

final class PostgreSqlEnumerationAcquisitionTerminalPublisherSuite
    extends FunSuite {
  import PostgreSqlEnumerationAcquisitionTerminalFailure._
  import PostgreSqlEnumerationAcquisitionTerminalPublisher._

  private val completeDocument = {
    val json = PostgreSqlEnumerationCampaignV2CanonicalJson
    json.document(
      json.obj(
        "schema_version" -> json.string(
          "postgresql-enumeration-acquisition-terminal-v1"
        ),
        "terminal" -> json.bool(true)
      )
    )
  }

  test("publication creates one private canonical completion child last") {
    withTestDirectory { root =>
      val output = root.resolve("disposition")
      val observed = ArrayBuffer.empty[(String, Vector[String])]
      val result = publishAfterStepForTest(
        output,
        completeDocument,
        (step, _) => observed += step -> childNames(output)
      )

      assertEquals(result, Right(()))
      assertEquals(childNames(output), Vector(CompleteFileName))
      assertEquals(
        Files.readAllBytes(output.resolve(CompleteFileName)).toVector,
        completeDocument.bytes.toVector
      )
      assertEquals(
        Files.getPosixFilePermissions(output, LinkOption.NOFOLLOW_LINKS),
        PosixFilePermissions.fromString("rwx------")
      )
      assertEquals(
        Files.getPosixFilePermissions(
          output.resolve(CompleteFileName),
          LinkOption.NOFOLLOW_LINKS
        ),
        PosixFilePermissions.fromString("rw-------")
      )
      assertEquals(
        observed.map(_._1).toVector,
        Vector(
          "terminal-path-created",
          "terminal-created",
          "complete-link-attempt",
          "complete-linked",
          "terminal-forced",
          "parent-forced",
          "cleanup-before-staged-delete",
          "cleanup-after-staged-delete"
        )
      )
      assertEquals(observed(0)._2, Vector.empty)
      assertEquals(observed(1)._2, Vector.empty)
      assertEquals(observed(2)._2, Vector.empty)
      assertEquals(observed(3)._2, Vector(CompleteFileName))
      assertEquals(
        childNames(root).filter(_.startsWith(".yrw-enumeration-disposition-")),
        Vector.empty
      )

      assertEquals(publish(output, completeDocument), Left(OutputExists))
      assertEquals(childNames(output), Vector(CompleteFileName))
    }
  }

  test("existing outputs and symlinked parents reject before staging") {
    withTestDirectory { root =>
      val existing = Files.createDirectory(root.resolve("existing"))
      assertEquals(publish(existing, completeDocument), Left(OutputExists))

      val dangling = root.resolve("dangling")
      Files.createSymbolicLink(dangling, root.resolve("missing"))
      assertEquals(publish(dangling, completeDocument), Left(OutputExists))

      val realParent = Files.createDirectory(root.resolve("real"))
      val linkedParent = root.resolve("linked")
      Files.createSymbolicLink(linkedParent, realParent)
      assertEquals(
        publish(linkedParent.resolve("disposition"), completeDocument),
        Left(UnsafePathComponent)
      )
      assertEquals(childNames(realParent), Vector.empty)
    }
  }

  test("publisher requires an absolute normalized path and canonical document") {
    withTestDirectory { root =>
      val nested = Files.createDirectory(root.resolve("nested"))
      assertEquals(
        publish(Paths.get("relative-disposition"), completeDocument),
        Left(AmbiguousPath)
      )
      assertEquals(
        publish(
          nested.resolve("..").resolve("disposition"),
          completeDocument
        ),
        Left(AmbiguousPath)
      )
      assertEquals(
        publish(root.resolve("null-document"), null),
        Left(InvalidDocument)
      )
    }
  }

  test("a create-to-identity failure preserves an indeterminate visible directory") {
    withTestDirectory { root =>
      val output = root.resolve("disposition")
      val result = publishAfterStepForTest(
        output,
        completeDocument,
        (step, _) =>
          if (step == "terminal-path-created")
            throw new IllegalStateException("private injected failure")
      )

      assertEquals(result, Left(PublishIndeterminate))
      assert(Files.isDirectory(output, LinkOption.NOFOLLOW_LINKS))
      assertEquals(childNames(output), Vector.empty)
    }
  }

  test("a pre-completion failure preserves a visible incomplete directory") {
    withTestDirectory { root =>
      val output = root.resolve("disposition")
      val result = publishAfterStepForTest(
        output,
        completeDocument,
        (step, _) =>
          if (step == "terminal-created")
            throw new IllegalStateException("private injected failure")
      )

      assertEquals(result, Left(TerminalIncomplete))
      assert(Files.isDirectory(output, LinkOption.NOFOLLOW_LINKS))
      assertEquals(childNames(output), Vector.empty)
      assertEquals(stagingNames(root), Vector.empty)
    }
  }

  test("unknown final children fail closed and remain visible") {
    withTestDirectory { root =>
      val output = root.resolve("disposition")
      val result = publishAfterStepForTest(
        output,
        completeDocument,
        (step, path) =>
          if (step == "terminal-created")
            writeNew(path.resolve("unknown"), Array[Byte](1))
      )

      assertEquals(result, Left(TerminalIncomplete))
      assertEquals(childNames(output), Vector("unknown"))
      assert(
        Files.notExists(
          output.resolve(CompleteFileName),
          LinkOption.NOFOLLOW_LINKS
        )
      )
    }
  }

  test("a replaced final directory is never followed or removed") {
    withTestDirectory { root =>
      val output = root.resolve("disposition")
      val displaced = root.resolve("displaced")
      val result = publishAfterStepForTest(
        output,
        completeDocument,
        (step, path) =>
          if (step == "terminal-created") {
            Files.move(path, displaced)
            Files.createDirectory(path)
            writeNew(path.resolve("independent"), Array[Byte](7))
          }
      )

      assertEquals(result, Left(TerminalIncomplete))
      assertEquals(childNames(output), Vector("independent"))
      assertEquals(childNames(displaced), Vector.empty)
    }
  }

  test("a completion-link attempt failure is terminal and indeterminate") {
    withTestDirectory { root =>
      val output = root.resolve("disposition")
      val result = publishAfterStepForTest(
        output,
        completeDocument,
        (step, _) =>
          if (step == "complete-link-attempt")
            throw new IllegalStateException("private injected failure")
      )

      assertEquals(result, Left(PublishIndeterminate))
      assertEquals(childNames(output), Vector.empty)
      assertEquals(stagingNames(root), Vector.empty)
    }
  }

  test("post-completion failures preserve the exact visible receipt") {
    Vector("complete-linked", "terminal-forced", "parent-forced").foreach {
      failedStep =>
        withTestDirectory { root =>
          val output = root.resolve("disposition")
          val result = publishAfterStepForTest(
            output,
            completeDocument,
            (step, _) =>
              if (step == failedStep)
                throw new IllegalStateException("private injected failure")
          )

          assertEquals(result, Left(PublishIndeterminate), failedStep)
          assertEquals(childNames(output), Vector(CompleteFileName))
          assertEquals(
            Files.readAllBytes(output.resolve(CompleteFileName)).toVector,
            completeDocument.bytes.toVector
          )
          assertEquals(stagingNames(root), Vector.empty)
        }
    }
  }

  test("a hostile post-link replacement is preserved and detected") {
    withTestDirectory { root =>
      val output = root.resolve("disposition")
      val outside = root.resolve("outside")
      writeNew(outside, "outside\n".getBytes(StandardCharsets.UTF_8))
      val result = publishAfterStepForTest(
        output,
        completeDocument,
        (step, path) =>
          if (step == "complete-linked") {
            Files.delete(path)
            Files.createSymbolicLink(path, outside)
          }
      )

      assertEquals(result, Left(PublishIndeterminate))
      assert(Files.isSymbolicLink(output.resolve(CompleteFileName)))
      assertEquals(childNames(output), Vector(CompleteFileName))
      assertEquals(stagingNames(root), Vector.empty)
    }
  }

  test("cleanup rejects an added hard-link alias before returning success") {
    withTestDirectory { root =>
      val output = root.resolve("disposition")
      val outside = root.resolve("outside-link")
      val result = publishAfterStepForTest(
        output,
        completeDocument,
        (step, path) =>
          if (step == "cleanup-before-staged-delete")
            Files.createLink(outside, path)
      )

      assertEquals(result, Left(CleanupFailed))
      assertEquals(childNames(output), Vector(CompleteFileName))
      assert(Files.isRegularFile(outside, LinkOption.NOFOLLOW_LINKS))
      assertEquals(stagingNames(root), Vector.empty)
    }
  }

  test("cleanup detects same-length receipt mutation with restored metadata") {
    withTestDirectory { root =>
      val output = root.resolve("disposition")
      val mutation = completeDocument.bytes.clone()
      mutation(1) = (mutation(1) ^ 1).toByte
      val result = publishAfterStepForTest(
        output,
        completeDocument,
        (step, path) =>
          if (step == "cleanup-after-staged-delete") {
            val originalModified = Files.getLastModifiedTime(
              path,
              LinkOption.NOFOLLOW_LINKS
            )
            Files.write(
              path,
              mutation,
              StandardOpenOption.WRITE,
              StandardOpenOption.TRUNCATE_EXISTING
            )
            Files.setLastModifiedTime(path, originalModified)
          }
      )

      assertEquals(result, Left(CleanupFailed))
      assertEquals(childNames(output), Vector(CompleteFileName))
      assertEquals(
        Files.readAllBytes(output.resolve(CompleteFileName)).toVector,
        mutation.toVector
      )
      assertEquals(stagingNames(root), Vector.empty)
    }
  }

  private def writeNew(path: Path, bytes: Array[Byte]): Unit = {
    Files.write(
      path,
      bytes,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )
    ()
  }

  private def childNames(path: Path): Vector[String] = {
    if (!Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) return Vector.empty
    val stream = Files.list(path)
    try stream.iterator().asScala.map(_.getFileName.toString).toVector.sorted
    finally stream.close()
  }

  private def stagingNames(root: Path): Vector[String] =
    childNames(root).filter(_.startsWith(".yrw-enumeration-disposition-"))

  private def withTestDirectory[A](operation: Path => A): A = {
    val root = Files.createTempDirectory(
      "yrw-acquisition-terminal-publisher-",
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
