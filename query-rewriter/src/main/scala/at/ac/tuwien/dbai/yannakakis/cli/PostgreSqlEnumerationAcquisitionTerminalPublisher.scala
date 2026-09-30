package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.security.MessageDigest
import java.nio.file.attribute.{
  BasicFileAttributes,
  FileTime,
  PosixFileAttributeView,
  PosixFilePermissions
}
import java.nio.file.{
  FileAlreadyExistsException,
  Files,
  FileStore,
  LinkOption,
  NoSuchFileException,
  OpenOption,
  Path,
  StandardOpenOption
}

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/** Stable failures from the terminal-disposition create-only boundary. */
private[yannakakis] sealed trait PostgreSqlEnumerationAcquisitionTerminalFailure
    extends Product
    with Serializable {
  def code: String
}

private[yannakakis] object PostgreSqlEnumerationAcquisitionTerminalFailure {
  case object MissingPath
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String = "acquisition-disposition-files.missing-path"
  }
  case object AmbiguousPath
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String = "acquisition-disposition-files.ambiguous-path"
  }
  case object UnsafePathComponent
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.unsafe-path-component"
  }
  case object FileIdentityUnavailable
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.identity-unavailable"
  }
  case object OutputParentUnavailable
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.output-parent-unavailable"
  }
  case object OutputExists
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String = "acquisition-disposition-files.output-exists"
  }
  case object PosixPermissionsUnavailable
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.posix-permissions-unavailable"
  }
  case object StagingCreateFailed
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.staging-create-failed"
  }
  case object StagingIdentityUnavailable
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.staging-identity-unavailable"
  }
  case object StagingChanged
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.staging-changed"
  }
  case object InvalidDocument
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.invalid-document"
  }
  case object CompleteWriteFailed
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.complete-write-failed"
  }
  case object CompleteChanged
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.complete-changed"
  }
  case object FileSyncFailed
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.file-sync-failed"
  }
  case object DifferentFileSystem
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.different-filesystem"
  }
  case object TerminalCreateFailed
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.terminal-create-failed"
  }
  case object TerminalIncomplete
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.terminal-incomplete"
  }
  case object PublishFailed
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.publish-failed"
  }
  case object PublishIndeterminate
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.publish-indeterminate"
  }
  case object PublishDurabilityFailed
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.publish-durability-failed"
  }
  case object CleanupFailed
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String = "acquisition-disposition-files.cleanup-failed"
  }
  case object OperationFailed
      extends PostgreSqlEnumerationAcquisitionTerminalFailure {
    override val code: String =
      "acquisition-disposition-files.operation-failed"
  }
}

/**
  * Create-only publication for an unpackageable terminal disposition.
  *
  * One unpredictable private staging directory holds the already-canonical
  * receipt. The final private directory is then created and `complete.json` is
  * hard-linked as its sole and final child. File and both directory entries are
  * forced before success. Once the final pathname becomes visible it is never
  * removed, including on incomplete or indeterminate publication.
  */
private[yannakakis] object PostgreSqlEnumerationAcquisitionTerminalPublisher {
  import PostgreSqlEnumerationAcquisitionTerminalFailure._

  val CompleteFileName: String = "complete.json"

  private val StagingPrefix = ".yrw-enumeration-disposition-"
  private val DirectoryPermissions = PosixFilePermissions.fromString("rwx------")
  private val FilePermissions = PosixFilePermissions.fromString("rw-------")
  private val NoFollow = Array(LinkOption.NOFOLLOW_LINKS)

  def publish(
      outputDisposition: Path,
      completeDocument: PostgreSqlEnumerationCampaignV2JsonDocument
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    publishAfterStep(outputDisposition, completeDocument, (_, _) => ())

  /** Deterministic seam for visible-incomplete and post-link hostile tests. */
  private[cli] def publishAfterStepForTest(
      outputDisposition: Path,
      completeDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
      afterPublicationStep: (String, Path) => Unit
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    publishAfterStep(
      outputDisposition,
      completeDocument,
      afterPublicationStep
    )

  private def publishAfterStep(
      outputDisposition: Path,
      completeDocument: PostgreSqlEnumerationCampaignV2JsonDocument,
      afterPublicationStep: (String, Path) => Unit
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
    if (completeDocument == null || afterPublicationStep == null)
      return Left(InvalidDocument)
    val bytes = completeDocument.bytes
    if (
      bytes.isEmpty ||
      bytes.length > PostgreSqlEnumerationCampaignV2Json.MaximumCanonicalJsonBytes ||
      bytes.last != '\n'.toByte || bytes.count(_ == '\n'.toByte) != 1
    ) return Left(InvalidDocument)

    createPublication(outputDisposition, afterPublicationStep).flatMap {
      publication =>
        var cleanupResult: Either[
          PostgreSqlEnumerationAcquisitionTerminalFailure,
          Unit
        ] = Right(())
        val operationResult =
          try publication.stageAndPublish(bytes)
          catch {
            case NonFatal(_) if publication.publicationStarted =>
              Left(PublishIndeterminate)
            case NonFatal(_) => Left(OperationFailed)
          } finally {
            cleanupResult = publication.cleanup()
          }
        cleanupResult match {
          case Left(_) if publication.publicationStarted && operationResult.isLeft =>
            operationResult
          case Left(_)   => Left(CleanupFailed)
          case Right(()) => operationResult
        }
    }
  }

  private final class Publication(
      val outputDisposition: Path,
      val outputParent: Path,
      val outputParentChain: PathChain,
      val stagingDirectory: Path,
      val stagingIdentity: FileIdentity,
      val afterPublicationStep: (String, Path) => Unit
  ) {
    private val stagedComplete = stagingDirectory.resolve(CompleteFileName)
    private var stagedCompleteIdentity: Option[FileIdentity] = None
    private var stagedCompleteSha256: Option[Vector[Byte]] = None
    private var stagedCompleteReady = false
    private var finalDirectoryIdentity: Option[FileIdentity] = None
    private var finalDirectoryReady = false
    private var publicationAttempted = false
    private var completeLinkAttempted = false
    private var completeLinked = false
    private var closed = false

    def publicationStarted: Boolean = synchronized(publicationAttempted)

    def stageAndPublish(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      synchronized {
        if (closed) Left(OperationFailed)
        else writeComplete(bytes).flatMap(_ => publishStaged())
      }

    private def writeComplete(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
      if (bytes == null || bytes.isEmpty) return Left(InvalidDocument)
      for {
        _ <- ensureStagingStable()
        _ <- requireDirectoryState(stagingDirectory, Set.empty, StagingChanged)
        _ <- ensureAbsent(stagedComplete).left.map {
          case OutputExists => CompleteWriteFailed
          case _            => CompleteWriteFailed
        }
        _ <- writeStagedComplete(bytes.clone())
      } yield ()
    }

    private def writeStagedComplete(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
      val expectedSha256 = sha256(bytes)
      var channel: FileChannel = null
      var createdIdentity: FileIdentity = null
      var failure: PostgreSqlEnumerationAcquisitionTerminalFailure = null
      try {
        channel = FileChannel.open(
          stagedComplete,
          openOptions(
            StandardOpenOption.CREATE_NEW,
            StandardOpenOption.WRITE,
            LinkOption.NOFOLLOW_LINKS
          ),
          PosixFilePermissions.asFileAttribute(FilePermissions)
        )
        readRegularIdentity(stagedComplete, CompleteWriteFailed) match {
          case Left(problem) => failure = problem
          case Right(created) =>
            createdIdentity = created
            stagedCompleteIdentity = Some(created)
            val buffer = ByteBuffer.wrap(bytes)
            var zeroWrites = 0
            while (buffer.hasRemaining && zeroWrites < 16) {
              val written = channel.write(buffer)
              if (written == 0) zeroWrites += 1 else zeroWrites = 0
            }
            if (buffer.hasRemaining) failure = CompleteWriteFailed
            else channel.force(true)
        }
      } catch {
        case _: FileAlreadyExistsException => failure = CompleteWriteFailed
        case NonFatal(_)                    => failure = CompleteWriteFailed
      } finally {
        if (channel != null) {
          try channel.close()
          catch {
            case NonFatal(_) => failure = CompleteWriteFailed
          }
        }
      }

      if (failure != null) Left(failure)
      else
        for {
          after <- readSingleLinkRegularIdentity(
            stagedComplete,
            CompleteWriteFailed
          )
          _ <-
            if (
              createdIdentity != null &&
              createdIdentity.sameObject(after) &&
              after.size == bytes.length.toLong
            ) Right(())
            else Left(CompleteWriteFailed)
          _ <- setAndVerifyFilePermissions(stagedComplete)
          _ <- forceExistingFile(stagedComplete, after, CompleteWriteFailed)
          durable <- readSingleLinkRegularIdentity(
            stagedComplete,
            CompleteWriteFailed
          )
          _ <-
            if (after.sameRegularFile(durable)) Right(())
            else Left(CompleteWriteFailed)
          _ <- forceDirectory(stagingDirectory)
          _ <- requireDirectoryState(
            stagingDirectory,
            Set(CompleteFileName),
            StagingChanged
          )
          _ <- verifyRegularFileSha256(
            stagedComplete,
            durable,
            expectedSha256,
            CompleteWriteFailed
          )
          _ = stagedCompleteIdentity = Some(durable)
          _ = stagedCompleteSha256 = Some(expectedSha256)
          _ = stagedCompleteReady = true
        } yield ()
    }

    private def publishStaged()
        : Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
      if (publicationAttempted) return Left(OutputExists)
      if (!stagedCompleteReady || stagedCompleteIdentity.isEmpty)
        return Left(CompleteChanged)

      val attempt = for {
        _ <- ensureStagingStable()
        _ <- revalidateChain(outputParentChain, StagingChanged)
        _ <- ensureAbsent(outputDisposition)
        current <- exactStagedComplete()
        _ <- requireSameFileStore(stagedComplete, outputParent)
        _ <- createFinalDirectory()
        _ <- runPublicationStep("terminal-created", outputDisposition)
        _ <- requireFinalState(Set.empty, TerminalIncomplete)
        _ <- exactStagedComplete()
        _ <- linkComplete(current)
        _ <- runPublicationStep(
          "complete-linked",
          outputDisposition.resolve(CompleteFileName)
        )
        _ <- requireFinalState(Set(CompleteFileName), PublishIndeterminate)
        _ <- verifyLinkedComplete(current)
        _ <- forceDirectory(outputDisposition).left.map(_ => PublishDurabilityFailed)
        _ <- runPublicationStep("terminal-forced", outputDisposition)
        _ <- requireFinalState(Set(CompleteFileName), PublishIndeterminate)
        _ <- verifyLinkedComplete(current)
        _ <- forceDirectory(outputParent).left.map(_ => PublishDurabilityFailed)
        _ <- runPublicationStep("parent-forced", outputParent)
        _ <- revalidateChain(outputParentChain, PublishDurabilityFailed)
        _ <- ensureFinalDirectoryStable(PublishDurabilityFailed)
        _ <- requireFinalState(Set(CompleteFileName), PublishDurabilityFailed)
        _ <- verifyLinkedComplete(current)
      } yield ()
      attempt.left.map(normalizePublicationFailure)
    }

    private def exactStagedComplete()
        : Either[PostgreSqlEnumerationAcquisitionTerminalFailure, FileIdentity] =
      readSingleLinkRegularIdentity(stagedComplete, CompleteChanged).flatMap {
        current =>
          for {
            expectedSha256 <- stagedCompleteSha256.toRight(CompleteChanged)
            _ <-
              if (stagedCompleteIdentity.exists(_.sameRegularFile(current)))
                Right(())
              else Left(CompleteChanged)
            _ <- verifyRegularFileSha256(
              stagedComplete,
              current,
              expectedSha256,
              CompleteChanged
            )
          } yield current
      }

    private def createFinalDirectory()
        : Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      try {
        Files.createDirectory(
          outputDisposition,
          PosixFilePermissions.asFileAttribute(DirectoryPermissions)
        )
        // Visibility starts here. Even a create-to-first-identity failure must
        // leave the pathname for operator inspection.
        publicationAttempted = true
        runPublicationStep("terminal-path-created", outputDisposition).flatMap {
          _ =>
            readDirectoryIdentity(outputDisposition, PublishIndeterminate)
              .flatMap { identity =>
                finalDirectoryIdentity = Some(identity)
                verifyDirectoryPermissions(
                  outputDisposition,
                  PublishIndeterminate
                ).map { _ =>
                  finalDirectoryReady = true
                  ()
                }
              }
        }
      } catch {
        case _: FileAlreadyExistsException => Left(OutputExists)
        case NonFatal(_) if publicationAttempted => Left(PublishIndeterminate)
        case NonFatal(_)                    => Left(TerminalCreateFailed)
      }

    private def linkComplete(
        expected: FileIdentity
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
      val target = outputDisposition.resolve(CompleteFileName)
      for {
        _ <- ensureFinalDirectoryStable(TerminalIncomplete)
        _ <- ensureAbsent(target).left.map(_ => PublishIndeterminate)
        _ <- createCompleteHardLink(target)
        _ <- verifyLinkedComplete(expected)
      } yield ()
    }

    private def createCompleteHardLink(
        target: Path
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      try {
        completeLinkAttempted = true
        runPublicationStep("complete-link-attempt", target) match {
          case Left(problem) => return Left(problem)
          case Right(())     => ()
        }
        Files.createLink(target, stagedComplete)
        completeLinked = true
        Right(())
      } catch {
        case _: FileAlreadyExistsException => Left(PublishIndeterminate)
        case NonFatal(_)                    => Left(PublishFailed)
      }

    private def verifyLinkedComplete(
        expected: FileIdentity
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
      val target = outputDisposition.resolve(CompleteFileName)
      for {
        expectedSha256 <- stagedCompleteSha256.toRight(PublishIndeterminate)
        _ <- ensureFinalDirectoryStable(PublishIndeterminate)
        staged <- readRegularIdentityWithLinkCount(
          stagedComplete,
          expectedLinkCount = 2L,
          PublishIndeterminate
        )
        published <- readRegularIdentityWithLinkCount(
          target,
          expectedLinkCount = 2L,
          PublishIndeterminate
        )
        _ <-
          if (
            expected.sameRegularFile(staged) &&
            staged.sameRegularFile(published)
          ) Right(())
          else Left(PublishIndeterminate)
        _ <- verifyRegularFileSha256(
          stagedComplete,
          staged,
          expectedSha256,
          PublishIndeterminate
        )
        _ <- verifyRegularFileSha256(
          target,
          published,
          expectedSha256,
          PublishIndeterminate
        )
        _ <- verifyFilePermissions(target, PublishIndeterminate)
      } yield ()
    }

    private def verifyPublishedCompleteAfterCleanup(
        expected: FileIdentity
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
      val target = outputDisposition.resolve(CompleteFileName)
      for {
        expectedSha256 <- stagedCompleteSha256.toRight(CleanupFailed)
        _ <- ensureFinalDirectoryStable(CleanupFailed)
        _ <- requireDirectoryState(
          outputDisposition,
          Set(CompleteFileName),
          CleanupFailed
        )
        published <- readRegularIdentityWithLinkCount(
          target,
          expectedLinkCount = 1L,
          CleanupFailed
        )
        _ <-
          if (expected.sameRegularFile(published)) Right(())
          else Left(CleanupFailed)
        _ <- verifyRegularFileSha256(
          target,
          published,
          expectedSha256,
          CleanupFailed
        )
        _ <- verifyFilePermissions(target, CleanupFailed)
      } yield ()
    }

    private def requireFinalState(
        expectedNames: Set[String],
        failure: PostgreSqlEnumerationAcquisitionTerminalFailure
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      ensureFinalDirectoryStable(failure).flatMap { _ =>
        requireDirectoryState(outputDisposition, expectedNames, failure)
      }

    private def ensureFinalDirectoryStable(
        failure: PostgreSqlEnumerationAcquisitionTerminalFailure
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      finalDirectoryIdentity match {
        case None => Left(failure)
        case Some(expected) =>
          readDirectoryIdentity(outputDisposition, failure).flatMap { current =>
            if (!expected.sameDirectory(current)) Left(failure)
            else verifyDirectoryPermissions(outputDisposition, failure)
          }
      }

    private def ensureStagingStable()
        : Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      readDirectoryIdentity(stagingDirectory, StagingChanged).flatMap {
        current =>
          if (stagingIdentity.sameDirectory(current)) Right(())
          else Left(StagingChanged)
      }

    private def runPublicationStep(
        name: String,
        path: Path
    ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      try {
        afterPublicationStep(name, path)
        Right(())
      } catch {
        case NonFatal(_) => Left(PublishIndeterminate)
      }

    private def normalizePublicationFailure(
        failure: PostgreSqlEnumerationAcquisitionTerminalFailure
    ): PostgreSqlEnumerationAcquisitionTerminalFailure =
      if (!publicationAttempted) failure
      else if (!finalDirectoryReady) PublishIndeterminate
      else if (completeLinkAttempted || completeLinked)
        failure match {
          case PublishDurabilityFailed => PublishDurabilityFailed
          case _                       => PublishIndeterminate
        }
      else TerminalIncomplete

    def cleanup()
        : Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      synchronized {
        if (closed) return Right(())
        closed = true
        if (ensureStagingStable().isLeft) return Left(CleanupFailed)

        var cleanupChanged = false

        val present = listDirectChildren(stagingDirectory, CleanupFailed).fold(
          _ => return Left(CleanupFailed),
          identity
        )
        if (
          !present.forall(_.getFileName.toString == CompleteFileName) ||
          present.size > 1
        ) return Left(CleanupFailed)

        if (stagedCompleteIdentity.nonEmpty && present.isEmpty)
          cleanupChanged = true

        present.foreach { path =>
          val identity = readRegularIdentity(path, CleanupFailed).fold(
            _ => return Left(CleanupFailed),
            value => value
          )
          if (!stagedCompleteIdentity.exists(_.sameRegularFile(identity)))
            return Left(CleanupFailed)

          if (completeLinked) {
            val hookFailed = runPublicationStep(
              "cleanup-before-staged-delete",
              outputDisposition.resolve(CompleteFileName)
            ).isLeft
            val linkChanged = verifyLinkedComplete(identity).isLeft
            if (hookFailed || linkChanged) cleanupChanged = true
          }
          val beforeDelete = readRegularIdentity(path, CleanupFailed).fold(
            _ => return Left(CleanupFailed),
            value => value
          )
          if (!identity.sameRegularFile(beforeDelete))
            return Left(CleanupFailed)
          try Files.delete(path)
          catch {
            case NonFatal(_) => return Left(CleanupFailed)
          }
        }

        if (forceDirectory(stagingDirectory).isLeft)
          return Left(CleanupFailed)
        if (
          listDirectChildren(stagingDirectory, CleanupFailed)
            .fold(_ => true, _.nonEmpty)
        ) return Left(CleanupFailed)
        if (ensureStagingStable().isLeft) return Left(CleanupFailed)
        try Files.delete(stagingDirectory)
        catch {
          case NonFatal(_) => return Left(CleanupFailed)
        }
        if (forceDirectory(outputParent).isLeft) cleanupChanged = true

        if (completeLinked) {
          val expected = stagedCompleteIdentity.getOrElse(
            return Left(CleanupFailed)
          )
          if (
            runPublicationStep(
              "cleanup-after-staged-delete",
              outputDisposition.resolve(CompleteFileName)
            ).isLeft || verifyPublishedCompleteAfterCleanup(expected).isLeft
          ) cleanupChanged = true
        }

        if (cleanupChanged) Left(CleanupFailed) else Right(())
      }
  }

  private final case class FileIdentity(
      key: AnyRef,
      isDirectory: Boolean,
      isRegularFile: Boolean,
      size: Long,
      lastModified: FileTime
  ) {
    def sameObject(other: FileIdentity): Boolean =
      other != null && key == other.key &&
        isDirectory == other.isDirectory &&
        isRegularFile == other.isRegularFile

    def sameDirectory(other: FileIdentity): Boolean =
      sameObject(other) && isDirectory

    def sameRegularFile(other: FileIdentity): Boolean =
      sameObject(other) && isRegularFile && size == other.size &&
        lastModified == other.lastModified
  }

  private final case class PathEntry(path: Path, identity: FileIdentity)
  private final case class PathChain(entries: Vector[PathEntry])

  private def createPublication(
      outputPath: Path,
      afterPublicationStep: (String, Path) => Unit
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Publication] = {
    val prepared = for {
      output <- absoluteNormalized(outputPath)
      parent <- Option(output.getParent).toRight(OutputParentUnavailable)
      parentChain <- inspectDirectoryChain(parent, OutputParentUnavailable)
      _ <- ensureAbsent(output)
      _ <- requirePosix(parent)
    } yield (output, parent, parentChain)

    prepared.flatMap { case (output, parent, parentChain) =>
      createStaging(parent).flatMap { staging =>
        readDirectoryIdentity(staging, StagingCreateFailed) match {
          case Left(_) =>
            // Without a first no-follow identity, safe ownership and cleanup
            // cannot be established. Preserve the path for inspection.
            Left(StagingIdentityUnavailable)
          case Right(stagingIdentity) =>
            val validation = for {
              _ <- verifyDirectoryPermissions(staging, StagingCreateFailed)
              _ <- revalidateChain(parentChain, StagingCreateFailed)
              _ <- forceDirectory(parent)
            } yield new Publication(
              output,
              parent,
              parentChain,
              staging,
              stagingIdentity,
              afterPublicationStep
            )
            validation match {
              case success @ Right(_) => success
              case Left(problem) =>
                cleanupFreshStaging(staging, stagingIdentity, parent) match {
                  case Right(()) => Left(problem)
                  case Left(_)   => Left(CleanupFailed)
                }
            }
        }
      }
    }
  }

  private def absoluteNormalized(
      path: Path
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Path] = {
    if (path == null) Left(MissingPath)
    else if (
      !path.isAbsolute || path.toString.isEmpty || path != path.normalize() ||
      path.iterator().asScala.exists { component =>
        component.toString == "." || component.toString == ".."
      }
    ) Left(AmbiguousPath)
    else Right(path)
  }

  private def inspectDirectoryChain(
      directory: Path,
      unavailable: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, PathChain] = {
    val root = directory.getRoot
    if (root == null) return Left(AmbiguousPath)
    val paths = Vector.newBuilder[Path]
    paths += root
    var current = root
    directory.iterator().asScala.foreach { component =>
      current = current.resolve(component)
      paths += current
    }
    paths.result().foldLeft(
      Right(Vector.empty): Either[
        PostgreSqlEnumerationAcquisitionTerminalFailure,
        Vector[PathEntry]
      ]
    ) { (checked, checkedPath) =>
      checked.flatMap { prefix =>
        readIdentity(checkedPath, unavailable).flatMap { identity =>
          if (identity.isDirectory)
            Right(prefix :+ PathEntry(checkedPath, identity))
          else Left(UnsafePathComponent)
        }
      }
    }.map(PathChain)
  }

  private def revalidateChain(
      chain: PathChain,
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    chain.entries.foldLeft(
      Right(()): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit]
    ) { (checked, entry) =>
      checked.flatMap { _ =>
        readIdentity(entry.path, failure).flatMap { current =>
          if (entry.identity.sameDirectory(current)) Right(())
          else Left(failure)
        }
      }
    }

  private def readIdentity(
      path: Path,
      unavailable: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, FileIdentity] =
    try {
      val attributes = Files.readAttributes(
        path,
        classOf[BasicFileAttributes],
        NoFollow: _*
      )
      if (attributes.isSymbolicLink) Left(UnsafePathComponent)
      else if (attributes.fileKey() == null) Left(FileIdentityUnavailable)
      else
        Right(
          FileIdentity(
            attributes.fileKey(),
            attributes.isDirectory,
            attributes.isRegularFile,
            attributes.size(),
            attributes.lastModifiedTime()
          )
        )
    } catch {
      case _: NoSuchFileException => Left(unavailable)
      case NonFatal(_)            => Left(unavailable)
    }

  private def readDirectoryIdentity(
      path: Path,
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, FileIdentity] =
    readIdentity(path, failure).flatMap { identity =>
      if (identity.isDirectory) Right(identity) else Left(failure)
    }

  private def readRegularIdentity(
      path: Path,
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, FileIdentity] =
    readIdentity(path, failure).flatMap { identity =>
      if (identity.isRegularFile) Right(identity) else Left(failure)
    }

  private def readRegularIdentityWithLinkCount(
      path: Path,
      expectedLinkCount: Long,
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, FileIdentity] =
    for {
      before <- readRegularIdentity(path, failure)
      linkCount <- readUnixLinkCount(path, failure)
      after <- readRegularIdentity(path, failure)
      _ <-
        if (
          before.sameRegularFile(after) && linkCount == expectedLinkCount
        ) Right(())
        else Left(failure)
    } yield after

  private def readSingleLinkRegularIdentity(
      path: Path,
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, FileIdentity] =
    readRegularIdentityWithLinkCount(path, 1L, failure)

  private def sha256(bytes: Array[Byte]): Vector[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bytes).toVector

  /** Bounded no-follow content check around an exact regular-file identity. */
  private def verifyRegularFileSha256(
      path: Path,
      expectedIdentity: FileIdentity,
      expectedSha256: Vector[Byte],
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
    if (
      expectedIdentity == null || expectedSha256 == null ||
      expectedSha256.size != 32 || expectedIdentity.size < 0L ||
      expectedIdentity.size >
        PostgreSqlEnumerationCampaignV2Json.MaximumCanonicalJsonBytes.toLong
    ) return Left(failure)

    readRegularIdentity(path, failure).flatMap { before =>
      if (!expectedIdentity.sameRegularFile(before)) Left(failure)
      else {
        var channel: FileChannel = null
        var digestBytes: Vector[Byte] = Vector.empty
        var readFailed = false
        try {
          channel = FileChannel.open(
            path,
            openOptions(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)
          )
          if (channel.size() != expectedIdentity.size) readFailed = true
          else {
            val digest = MessageDigest.getInstance("SHA-256")
            val buffer = ByteBuffer.allocate(8192)
            var remaining = expectedIdentity.size
            var zeroReads = 0
            while (remaining > 0L && zeroReads < 16) {
              buffer.clear()
              buffer.limit(math.min(buffer.capacity().toLong, remaining).toInt)
              val read = channel.read(buffer)
              if (read < 0) {
                readFailed = true
                remaining = 0L
              } else if (read == 0) zeroReads += 1
              else {
                zeroReads = 0
                remaining -= read.toLong
                buffer.flip()
                digest.update(buffer)
              }
            }
            if (remaining != 0L || channel.size() != expectedIdentity.size)
              readFailed = true
            else digestBytes = digest.digest().toVector
          }
        } catch {
          case NonFatal(_) => readFailed = true
        } finally {
          if (channel != null) {
            try channel.close()
            catch {
              case NonFatal(_) => readFailed = true
            }
          }
        }

        if (readFailed || digestBytes != expectedSha256) Left(failure)
        else
          readRegularIdentity(path, failure).flatMap { after =>
            if (expectedIdentity.sameRegularFile(after)) Right(())
            else Left(failure)
          }
      }
    }
  }

  private def readUnixLinkCount(
      path: Path,
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Long] =
    try {
      Files.getAttribute(path, "unix:nlink", NoFollow: _*) match {
        case value: java.lang.Number if value.longValue() > 0L =>
          Right(value.longValue())
        case _ => Left(failure)
      }
    } catch {
      case NonFatal(_) => Left(failure)
    }

  private def ensureAbsent(
      path: Path
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    try {
      Files.readAttributes(path, classOf[BasicFileAttributes], NoFollow: _*)
      Left(OutputExists)
    } catch {
      case _: NoSuchFileException => Right(())
      case NonFatal(_)            => Left(OutputParentUnavailable)
    }

  private def requirePosix(
      path: Path
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    try {
      if (
        Files.getFileAttributeView(
          path,
          classOf[PosixFileAttributeView],
          NoFollow: _*
        ) == null
      ) Left(PosixPermissionsUnavailable)
      else Right(())
    } catch {
      case NonFatal(_) => Left(PosixPermissionsUnavailable)
    }

  private def verifyDirectoryPermissions(
      path: Path,
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    try {
      if (Files.getPosixFilePermissions(path, NoFollow: _*) == DirectoryPermissions)
        Right(())
      else Left(failure)
    } catch {
      case NonFatal(_) => Left(failure)
    }

  private def setAndVerifyFilePermissions(
      path: Path
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    try {
      Files.setPosixFilePermissions(path, FilePermissions)
      if (Files.getPosixFilePermissions(path, NoFollow: _*) == FilePermissions)
        Right(())
      else Left(FileSyncFailed)
    } catch {
      case NonFatal(_) => Left(PosixPermissionsUnavailable)
    }

  private def verifyFilePermissions(
      path: Path,
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    try {
      if (Files.getPosixFilePermissions(path, NoFollow: _*) == FilePermissions)
        Right(())
      else Left(failure)
    } catch {
      case NonFatal(_) => Left(failure)
    }

  private def forceExistingFile(
      path: Path,
      expected: FileIdentity,
      changedFailure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
    var channel: FileChannel = null
    var result: Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      Right(())
    try {
      channel = FileChannel.open(
        path,
        openOptions(StandardOpenOption.WRITE, LinkOption.NOFOLLOW_LINKS)
      )
      if (channel.size() != expected.size) result = Left(changedFailure)
      else channel.force(true)
    } catch {
      case NonFatal(_) => result = Left(FileSyncFailed)
    } finally {
      if (channel != null) {
        try channel.close()
        catch {
          case NonFatal(_) => result = Left(FileSyncFailed)
        }
      }
    }
    result
  }

  private def forceDirectory(
      path: Path
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] = {
    var channel: FileChannel = null
    var result: Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
      Right(())
    try {
      channel = FileChannel.open(
        path,
        openOptions(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)
      )
      channel.force(true)
    } catch {
      case NonFatal(_) => result = Left(FileSyncFailed)
    } finally {
      if (channel != null) {
        try channel.close()
        catch {
          case NonFatal(_) => result = Left(FileSyncFailed)
        }
      }
    }
    result
  }

  private def requireSameFileStore(
      source: Path,
      targetParent: Path
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    try {
      val sourceStore: FileStore = Files.getFileStore(source)
      val targetStore: FileStore = Files.getFileStore(targetParent)
      if (sourceStore == targetStore) Right(()) else Left(DifferentFileSystem)
    } catch {
      case NonFatal(_) => Left(DifferentFileSystem)
    }

  private def requireDirectoryState(
      directory: Path,
      expectedNames: Set[String],
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    listDirectChildren(directory, failure).flatMap { children =>
      val names = children.map(_.getFileName.toString).toSet
      if (names == expectedNames && children.size == expectedNames.size) Right(())
      else Left(failure)
    }

  private def listDirectChildren(
      directory: Path,
      failure: PostgreSqlEnumerationAcquisitionTerminalFailure
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Vector[Path]] = {
    var stream: java.nio.file.DirectoryStream[Path] = null
    var result: Either[
      PostgreSqlEnumerationAcquisitionTerminalFailure,
      Vector[Path]
    ] = Left(failure)
    try {
      stream = Files.newDirectoryStream(directory)
      result = Right(stream.iterator().asScala.toVector)
    } catch {
      case NonFatal(_) => result = Left(failure)
    } finally {
      if (stream != null) {
        try stream.close()
        catch {
          case NonFatal(_) => result = Left(failure)
        }
      }
    }
    result
  }

  private def createStaging(
      parent: Path
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Path] =
    try {
      Right(
        Files.createTempDirectory(
          parent,
          StagingPrefix,
          PosixFilePermissions.asFileAttribute(DirectoryPermissions)
        )
      )
    } catch {
      case NonFatal(_) => Left(StagingCreateFailed)
    }

  private def cleanupFreshStaging(
      staging: Path,
      expected: FileIdentity,
      parent: Path
  ): Either[PostgreSqlEnumerationAcquisitionTerminalFailure, Unit] =
    readDirectoryIdentity(staging, CleanupFailed).flatMap { current =>
      if (!expected.sameDirectory(current)) Left(CleanupFailed)
      else
        listDirectChildren(staging, CleanupFailed).flatMap { children =>
          if (children.nonEmpty) Left(CleanupFailed)
          else
            try {
              Files.delete(staging)
              forceDirectory(parent).left.map(_ => CleanupFailed)
            } catch {
              case NonFatal(_) => Left(CleanupFailed)
            }
        }
    }

  private def openOptions(options: OpenOption*): java.util.Set[OpenOption] = {
    val result = new java.util.HashSet[OpenOption]()
    options.foreach(result.add)
    result
  }
}
