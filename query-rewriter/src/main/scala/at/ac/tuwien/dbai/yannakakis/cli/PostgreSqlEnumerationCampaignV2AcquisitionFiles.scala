package at.ac.tuwien.dbai.yannakakis.cli

import java.io.IOException
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
  FileStore,
  Files,
  LinkOption,
  NoSuchFileException,
  OpenOption,
  Path,
  StandardOpenOption
}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/** Stable failures from the campaign-v2 create-only filesystem boundary. */
private[yannakakis] sealed trait PostgreSqlEnumerationCampaignV2FileFailure
    extends Product
    with Serializable {
  def code: String
}

private[yannakakis] object PostgreSqlEnumerationCampaignV2FileFailure {
  case object MissingPath extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.missing-path"
  }
  case object AmbiguousPath
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.ambiguous-path"
  }
  case object UnsafePathComponent
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.unsafe-path-component"
  }
  case object FileIdentityUnavailable
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.identity-unavailable"
  }
  case object InputUnavailable
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.input-unavailable"
  }
  case object InputNotRegular
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.input-not-regular"
  }
  case object InputSizeInvalid
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.input-size-invalid"
  }
  case object InputChanged
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.input-changed"
  }
  case object InputReadFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.input-read-failed"
  }
  case object OutputParentUnavailable
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.output-parent-unavailable"
  }
  case object OutputExists
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.output-exists"
  }
  case object PosixPermissionsUnavailable
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.posix-permissions-unavailable"
  }
  case object StagingCreateFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.staging-create-failed"
  }
  case object StagingIdentityUnavailable
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String =
      "acquisition-files.staging-identity-unavailable"
  }
  case object StagingChanged
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.staging-changed"
  }
  case object InvalidDocumentBytes
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.invalid-document-bytes"
  }
  case object AssemblyExists
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.assembly-exists"
  }
  case object AssemblyWriteFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.assembly-write-failed"
  }
  case object StagedPackageExists
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.staged-package-exists"
  }
  case object StagedPackageUnavailable
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.staged-package-unavailable"
  }
  case object StagedPackageInvalid
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.staged-package-invalid"
  }
  case object StagedPackageWriteFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.staged-package-write-failed"
  }
  case object StagedPackageChanged
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.staged-package-changed"
  }
  case object FileSyncFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.file-sync-failed"
  }
  case object DifferentFileSystem
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.different-filesystem"
  }
  case object PublishFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.publish-failed"
  }
  case object PublishIndeterminate
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.publish-indeterminate"
  }
  case object PublishDurabilityFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.publish-durability-failed"
  }
  case object WorkspaceClosed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.workspace-closed"
  }
  case object OperationFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.operation-failed"
  }
  case object CleanupFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.cleanup-failed"
  }
  case object SearchInputExists
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.search-input-exists"
  }
  case object SearchInputUnavailable
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.search-input-unavailable"
  }
  case object SearchInputWriteFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.search-input-write-failed"
  }
  case object SearchInputChanged
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.search-input-changed"
  }
  case object CompleteRecordExists
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.complete-record-exists"
  }
  case object CompleteRecordUnavailable
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String =
      "acquisition-files.complete-record-unavailable"
  }
  case object CompleteRecordWriteFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String =
      "acquisition-files.complete-record-write-failed"
  }
  case object CompleteRecordChanged
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.complete-record-changed"
  }
  case object BundleCreateFailed
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.bundle-create-failed"
  }
  case object BundleChanged
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.bundle-changed"
  }
  case object BundleIncomplete
      extends PostgreSqlEnumerationCampaignV2FileFailure {
    override val code: String = "acquisition-files.bundle-incomplete"
  }
}

/**
  * Path-safe, create-only filesystem mechanics for the acquisition CLI.
  *
  * The utility deliberately has no process invocation or JSON knowledge. Each
  * operation owns one unpredictable 0700 staging directory below the final
  * output parent and verifies staged files at 0600. The legacy path publishes
  * one package through a create-only hard link; the paired path publishes a
  * fixed bundle whose completion record is linked last as a logical commit.
  */
private[yannakakis] object PostgreSqlEnumerationCampaignV2AcquisitionFiles {
  import PostgreSqlEnumerationCampaignV2FileFailure._

  val MaximumDocumentBytes: Int = 64 * 1024 * 1024

  private val StagingPrefix = ".yrw-enumeration-acquisition-"
  private val AssemblyFileName = "assembly.json"
  private val PackageFileName = "package.json"
  private val SearchInputFileName = "search-input.json"
  private val CompleteFileName = "complete.json"
  private val DirectoryPermissions = PosixFilePermissions.fromString("rwx------")
  private val FilePermissions = PosixFilePermissions.fromString("rw-------")
  private val NoFollow = Array(LinkOption.NOFOLLOW_LINKS)

  /** One bounded immutable snapshot after best-effort NIO path revalidation. */
  final class LoadedAuthoring private[cli] (
      private val snapshot: Array[Byte]
  ) {
    val byteLength: Int = snapshot.length
    def bytes: Array[Byte] = snapshot.clone()
  }

  /**
    * Read the complete existing input after checking every path component and
    * comparing file/directory identities before and after the open read.
    * Standard Java NIO does not expose the opened descriptor's file key here;
    * a swap-and-restore attack by any principal able to rename or replace a
    * checked ancestor is therefore outside this pathname-based boundary.
    */
  def readAuthoring(
      input: Path
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, LoadedAuthoring] =
    readAuthoringWithMaximumBytes(input, MaximumDocumentBytes)

  private[cli] def readAuthoringWithMaximumBytes(
      input: Path,
      maximumBytes: Int
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, LoadedAuthoring] = {
    require(
      maximumBytes > 0 && maximumBytes <= MaximumDocumentBytes,
      "authoring input bound must be within the production envelope"
    )
    for {
      absolute <- absoluteUnambiguous(input)
      chain <- inspectInputChain(absolute)
      finalIdentity = chain.entries.last.identity
      _ <-
        if (
          finalIdentity.size > 0L &&
          finalIdentity.size <= maximumBytes.toLong
        ) Right(())
        else Left(InputSizeInvalid)
      bytes <- readExactFile(absolute, chain, finalIdentity.size.toInt)
    } yield new LoadedAuthoring(bytes)
  }

  /**
    * Run one operation inside a fresh owned staging directory and always
    * attempt exact, non-recursive cleanup before returning.
    */
  def withWorkspace[A](
      output: Path
  )(
      operation: Workspace => Either[
        PostgreSqlEnumerationCampaignV2FileFailure,
        A
      ]
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, A] =
    withMappedWorkspace(
      output,
      (failure: PostgreSqlEnumerationCampaignV2FileFailure) => failure,
      OperationFailed
    )(operation)

  /**
    * Orchestration adapter that preserves caller-owned composer/backend errors
    * while mapping only this utility's setup, durability, and cleanup errors.
    */
  def withMappedWorkspace[E, A](
      output: Path,
      mapFileFailure: PostgreSqlEnumerationCampaignV2FileFailure => E,
      unexpectedFailure: E
  )(
      operation: Workspace => Either[E, A]
  ): Either[E, A] =
    withMappedWorkspaceAfterHardLink(
      output,
      mapFileFailure,
      unexpectedFailure,
      _ => ()
    )(operation)

  /**
    * Pair-specific staging and logical-commit boundary. The existing
    * single-package workspace is intentionally not used or changed: a bundle
    * has three fixed final children and is accepted only after an independent
    * consumer fully validates those children and rederives `complete.json`.
    */
  def withMappedPairWorkspace[E, A](
      outputBundle: Path,
      mapFileFailure: PostgreSqlEnumerationCampaignV2FileFailure => E,
      unexpectedFailure: E
  )(
      operation: PairWorkspace => Either[E, A]
  ): Either[E, A] =
    withMappedPairWorkspaceAfterStep(
      outputBundle,
      mapFileFailure,
      unexpectedFailure,
      (_, _) => ()
    )(operation)

  /** Deterministic seam for incomplete/committed bundle race tests. */
  private[cli] def withPairWorkspaceAfterStepForTest[A](
      outputBundle: Path,
      afterPublicationStep: (String, Path) => Unit
  )(
      operation: PairWorkspace => Either[
        PostgreSqlEnumerationCampaignV2FileFailure,
        A
      ]
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, A] =
    withMappedPairWorkspaceAfterStep(
      outputBundle,
      (failure: PostgreSqlEnumerationCampaignV2FileFailure) => failure,
      OperationFailed,
      afterPublicationStep
    )(operation)

  private def withMappedPairWorkspaceAfterStep[E, A](
      outputBundle: Path,
      mapFileFailure: PostgreSqlEnumerationCampaignV2FileFailure => E,
      unexpectedFailure: E,
      afterPublicationStep: (String, Path) => Unit
  )(
      operation: PairWorkspace => Either[E, A]
  ): Either[E, A] = {
    require(mapFileFailure != null, "file-failure mapper must not be null")
    require(
      afterPublicationStep != null,
      "post-publication hook must not be null"
    )
    if (operation == null) return Left(unexpectedFailure)
    createPairWorkspace(outputBundle, afterPublicationStep)
      .left
      .map(mapFileFailure)
      .flatMap { workspace =>
        var cleanupResult: Either[
          PostgreSqlEnumerationCampaignV2FileFailure,
          Unit
        ] = Right(())
        val operationResult =
          try {
            val result = operation(workspace)
            if (result == null) Left(unexpectedFailure) else result
          } catch {
            case NonFatal(_) => Left(unexpectedFailure)
          } finally {
            cleanupResult = workspace.cleanup()
          }
        cleanupResult match {
          case Left(_) if workspace.publicationStarted && operationResult.isLeft =>
            operationResult
          case Left(_)   => Left(mapFileFailure(CleanupFailed))
          case Right(()) => operationResult
        }
      }
  }

  /** Deterministic seam for hostile post-link race tests. */
  private[cli] def withWorkspaceAfterHardLinkForTest[A](
      output: Path,
      afterHardLink: Path => Unit
  )(
      operation: Workspace => Either[
        PostgreSqlEnumerationCampaignV2FileFailure,
        A
      ]
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, A] =
    withMappedWorkspaceAfterHardLink(
      output,
      (failure: PostgreSqlEnumerationCampaignV2FileFailure) => failure,
      OperationFailed,
      afterHardLink
    )(operation)

  private def withMappedWorkspaceAfterHardLink[E, A](
      output: Path,
      mapFileFailure: PostgreSqlEnumerationCampaignV2FileFailure => E,
      unexpectedFailure: E,
      afterHardLink: Path => Unit
  )(
      operation: Workspace => Either[E, A]
  ): Either[E, A] = {
    require(mapFileFailure != null, "file-failure mapper must not be null")
    require(afterHardLink != null, "post-link hook must not be null")
    if (operation == null) return Left(unexpectedFailure)
    createWorkspace(output, afterHardLink).left.map(mapFileFailure).flatMap {
      workspace =>
      var cleanupResult: Either[
        PostgreSqlEnumerationCampaignV2FileFailure,
        Unit
      ] = Right(())
      val operationResult =
        try {
          val result = operation(workspace)
          if (result == null) Left(unexpectedFailure) else result
        }
        catch {
          case NonFatal(_) => Left(unexpectedFailure)
        } finally {
          // This also runs before interruption, ControlThrowable, ThreadDeath,
          // and VM/linkage failures propagate. The staging boundary must make
          // its best exact-target cleanup attempt on every operation exit.
          cleanupResult = workspace.cleanup()
        }
      cleanupResult match {
        case Left(_)   => Left(mapFileFailure(CleanupFailed))
        case Right(()) => operationResult
      }
    }
  }

  /** Owned staging state for a fixed-name logically committed pair bundle. */
  final class PairWorkspace private[cli] (
      private val outputBundle: Path,
      private val outputParent: Path,
      private val outputParentChain: PathChain,
      val stagingDirectory: Path,
      private val stagingIdentity: FileIdentity,
      private val afterPublicationStep: (String, Path) => Unit
  ) {
    val assemblyPath: Path = stagingDirectory.resolve(AssemblyFileName)
    val stagedPackagePath: Path = stagingDirectory.resolve(PackageFileName)
    val stagedSearchInputPath: Path =
      stagingDirectory.resolve(SearchInputFileName)
    val stagedCompletePath: Path = stagingDirectory.resolve(CompleteFileName)

    private var closed = false
    private var assemblyIdentity: Option[FileIdentity] = None
    private var packageIdentity: Option[FileIdentity] = None
    private var searchInputIdentity: Option[FileIdentity] = None
    private var completeIdentity: Option[FileIdentity] = None
    private var packageReady = false
    private var searchInputReady = false
    private var completeReady = false
    private var publicationAttempted = false
    private var finalBundleIdentity: Option[FileIdentity] = None
    private var finalBundleReady = false
    private var completeLinkAttempted = false
    private var completeLinked = false

    private[cli] def publicationStarted: Boolean = synchronized {
      publicationAttempted
    }

    def writeAssembly(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (assemblyIdentity.nonEmpty) Left(AssemblyExists)
        else
          writeDocument(
            assemblyPath,
            bytes,
            AssemblyExists,
            AssemblyWriteFailed,
            identity => assemblyIdentity = Some(identity)
          )
      }

    /** Direct writer retained as a deterministic package-publication seam. */
    def writeStagedPackage(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (packageReady || packageIdentity.nonEmpty) Left(StagedPackageExists)
        else
          writeDocument(
            stagedPackagePath,
            bytes,
            StagedPackageExists,
            StagedPackageWriteFailed,
            identity => {
              packageIdentity = Some(identity)
              packageReady = true
            }
          )
      }

    def writeSearchInput(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (searchInputReady || searchInputIdentity.nonEmpty)
          Left(SearchInputExists)
        else
          writeDocument(
            stagedSearchInputPath,
            bytes,
            SearchInputExists,
            SearchInputWriteFailed,
            identity => {
              searchInputIdentity = Some(identity)
              searchInputReady = true
            }
          )
      }

    def writeComplete(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (completeReady || completeIdentity.nonEmpty)
          Left(CompleteRecordExists)
        else
          writeDocument(
            stagedCompletePath,
            bytes,
            CompleteRecordExists,
            CompleteRecordWriteFailed,
            identity => {
              completeIdentity = Some(identity)
              completeReady = true
            }
          )
      }

    /** Adopt the single-link regular package produced by the pinned composer. */
    def adoptStagedPackage(
        maximumBytes: Int = MaximumDocumentBytes
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        require(
          maximumBytes > 0 && maximumBytes <= MaximumDocumentBytes,
          "staged package bound must be within the production envelope"
        )
        if (closed) Left(WorkspaceClosed)
        else if (packageReady || packageIdentity.nonEmpty)
          Left(StagedPackageExists)
        else
          ensureStagingStable().flatMap { _ =>
            readRegularIdentity(stagedPackagePath, StagedPackageUnavailable)
              .flatMap { observed =>
                readSingleLinkRegularIdentity(
                  stagedPackagePath,
                  StagedPackageInvalid
                ).flatMap { before =>
                  if (observed.sameRegularFile(before)) Right(before)
                  else Left(StagedPackageChanged)
                }
              }
              .flatMap { before =>
                packageIdentity = Some(before)
                if (before.size <= 0L || before.size > maximumBytes.toLong)
                  Left(StagedPackageInvalid)
                else
                  setAndVerifyFilePermissions(stagedPackagePath).flatMap { _ =>
                    forceExistingFile(
                      stagedPackagePath,
                      before,
                      StagedPackageChanged
                    ).flatMap { _ =>
                      readSingleLinkRegularIdentity(
                        stagedPackagePath,
                        StagedPackageChanged
                      ).flatMap { after =>
                        if (!before.sameRegularFile(after))
                          Left(StagedPackageChanged)
                        else
                          forceDirectory(stagingDirectory).map { _ =>
                            packageIdentity = Some(after)
                            packageReady = true
                            ()
                          }
                      }
                    }
                  }
              }
          }
      }

    /**
      * Failure-path ownership capture for a composer-created partial. Only an
      * exact single-link regular file in the still-owned staging directory is
      * recorded; aliases and ambiguous paths remain deliberately undeletable.
      */
    def recordPartialStagedPackageForCleanup()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (closed) Left(WorkspaceClosed)
        else if (packageIdentity.nonEmpty) Right(())
        else
          ensureStagingStable().flatMap { _ =>
            ensureAbsent(stagedPackagePath) match {
              case Right(()) => Right(())
              case Left(OutputExists) =>
                readSingleLinkRegularIdentity(
                  stagedPackagePath,
                  StagedPackageInvalid
                ).flatMap { observed =>
                  ensureStagingStable().map { _ =>
                    packageIdentity = Some(observed)
                    ()
                  }
                }
              case Left(_) => Left(StagedPackageInvalid)
            }
          }
      }

    def stagedPackageByteLength()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Long] =
      stagedPackageEvidence().map(_.byteLength)

    def stagedPackageSha256()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, String] =
      stagedPackageEvidence().map(_.sha256)

    def stagedPackageEvidence(): Either[
      PostgreSqlEnumerationCampaignV2FileFailure,
      PostgreSqlEnumerationSearchReplayBundleV1FileEvidence
    ] = synchronized {
      stagedEvidence(
        stagedPackagePath,
        packageIdentity,
        packageReady,
        StagedPackageUnavailable,
        StagedPackageChanged
      )
    }

    def stagedSearchInputEvidence(): Either[
      PostgreSqlEnumerationCampaignV2FileFailure,
      PostgreSqlEnumerationSearchReplayBundleV1FileEvidence
    ] = synchronized {
      stagedEvidence(
        stagedSearchInputPath,
        searchInputIdentity,
        searchInputReady,
        SearchInputUnavailable,
        SearchInputChanged
      )
    }

    /**
      * Publish data names first and the complete record last. A visible bundle
      * without the final record is deliberately incomplete and must never be
      * accepted by a consumer.
      */
    def publish()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (closed) Left(WorkspaceClosed)
        else if (publicationAttempted) Left(OutputExists)
        else if (!packageReady || packageIdentity.isEmpty)
          Left(StagedPackageUnavailable)
        else if (!searchInputReady || searchInputIdentity.isEmpty)
          Left(SearchInputUnavailable)
        else if (!completeReady || completeIdentity.isEmpty)
          Left(CompleteRecordUnavailable)
        else {
          val attempt = for {
            _ <- ensureStagingStable()
            _ <- revalidateChain(outputParentChain, InputChanged)
            _ <- ensureAbsent(outputBundle)
            currentPackage <- exactStagedIdentity(
              stagedPackagePath,
              packageIdentity,
              StagedPackageChanged
            )
            currentSearch <- exactStagedIdentity(
              stagedSearchInputPath,
              searchInputIdentity,
              SearchInputChanged
            )
            currentComplete <- exactStagedIdentity(
              stagedCompletePath,
              completeIdentity,
              CompleteRecordChanged
            )
            _ <- requireSameFileStore(stagedPackagePath, outputParent)
            _ <- createFinalBundle()
            _ <- runPublicationStep("bundle-created", outputBundle)
            _ <- requireBundleState(Set.empty)
            _ <- linkAndVerify(
              PackageFileName,
              stagedPackagePath,
              currentPackage
            )
            _ <- runPublicationStep(
              "package-linked",
              outputBundle.resolve(PackageFileName)
            )
            _ <- requireBundleState(Set(PackageFileName))
            _ <- verifyLinked(
              PackageFileName,
              stagedPackagePath,
              currentPackage
            )
            _ <- linkAndVerify(
              SearchInputFileName,
              stagedSearchInputPath,
              currentSearch
            )
            _ <- runPublicationStep(
              "search-input-linked",
              outputBundle.resolve(SearchInputFileName)
            )
            _ <- requireBundleState(Set(PackageFileName, SearchInputFileName))
            _ <- verifyDataLinks(currentPackage, currentSearch)
            _ <- forceDirectory(outputBundle).left.map(_ => PublishDurabilityFailed)
            _ <- runPublicationStep("data-directory-forced", outputBundle)
            _ <- requireBundleState(Set(PackageFileName, SearchInputFileName))
            _ <- verifyDataLinks(currentPackage, currentSearch)
            _ <- linkAndVerify(
              CompleteFileName,
              stagedCompletePath,
              currentComplete,
              commitMarker = true
            )
            _ <- runPublicationStep(
              "complete-linked",
              outputBundle.resolve(CompleteFileName)
            )
            _ <- requireBundleState(
              Set(PackageFileName, SearchInputFileName, CompleteFileName)
            )
            _ <- verifyAllLinks(currentPackage, currentSearch, currentComplete)
            _ <- forceDirectory(outputBundle).left.map(_ => PublishDurabilityFailed)
            _ <- runPublicationStep("complete-directory-forced", outputBundle)
            _ <- requireBundleState(
              Set(PackageFileName, SearchInputFileName, CompleteFileName)
            )
            _ <- verifyAllLinks(currentPackage, currentSearch, currentComplete)
            _ <- forceDirectory(outputParent).left.map(_ => PublishDurabilityFailed)
            _ <- runPublicationStep("parent-forced", outputParent)
            _ <- revalidateChain(outputParentChain, PublishDurabilityFailed)
            _ <- ensureFinalBundleStable(PublishDurabilityFailed)
            _ <- verifyAllLinks(currentPackage, currentSearch, currentComplete)
          } yield ()
          attempt.left.map(normalizePublicationFailure)
        }
      }

    private def writeDocument(
        path: Path,
        bytes: Array[Byte],
        existsFailure: PostgreSqlEnumerationCampaignV2FileFailure,
        writeFailure: PostgreSqlEnumerationCampaignV2FileFailure,
        record: FileIdentity => Unit
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] = {
      if (closed) return Left(WorkspaceClosed)
      if (pathExistsNoFollow(path)) return Left(existsFailure)
      if (bytes == null || bytes.isEmpty || bytes.length > MaximumDocumentBytes)
        return Left(InvalidDocumentBytes)
      val snapshot = bytes.clone()
      ensureStagingStable().flatMap { _ =>
        ensureAbsent(path).left.map {
          case OutputExists => existsFailure
          case _            => writeFailure
        }.flatMap { _ =>
          var channel: FileChannel = null
          var createdIdentity: FileIdentity = null
          var failure: PostgreSqlEnumerationCampaignV2FileFailure = null
          try {
            channel = FileChannel.open(
              path,
              openOptions(
                StandardOpenOption.CREATE_NEW,
                StandardOpenOption.WRITE,
                LinkOption.NOFOLLOW_LINKS
              ),
              PosixFilePermissions.asFileAttribute(FilePermissions)
            )
            readRegularIdentity(path, writeFailure) match {
              case Left(problem) => failure = problem
              case Right(created) =>
                createdIdentity = created
                record(created)
                val buffer = ByteBuffer.wrap(snapshot)
                var zeroWrites = 0
                while (buffer.hasRemaining && zeroWrites < 16) {
                  val written = channel.write(buffer)
                  if (written == 0) zeroWrites += 1 else zeroWrites = 0
                }
                if (buffer.hasRemaining) failure = writeFailure
                else channel.force(true)
            }
          } catch {
            case _: FileAlreadyExistsException => failure = existsFailure
            case NonFatal(_)                    => failure = writeFailure
          } finally {
            if (channel != null) {
              try channel.close()
              catch {
                case NonFatal(_) => failure = writeFailure
              }
            }
          }
          if (failure != null) Left(failure)
          else
            for {
              after <- readSingleLinkRegularIdentity(path, writeFailure)
              _ <-
                if (
                  createdIdentity != null &&
                  createdIdentity.sameObject(after) &&
                  after.size == snapshot.length.toLong
                ) Right(())
                else Left(writeFailure)
              _ <- setAndVerifyFilePermissions(path)
              _ <- forceExistingFile(path, after, writeFailure)
              durable <- readSingleLinkRegularIdentity(path, writeFailure)
              _ <-
                if (after.sameRegularFile(durable)) Right(())
                else Left(writeFailure)
              _ <- forceDirectory(stagingDirectory)
              _ = record(durable)
            } yield ()
        }
      }
    }

    private def stagedEvidence(
        path: Path,
        recorded: Option[FileIdentity],
        ready: Boolean,
        unavailable: PostgreSqlEnumerationCampaignV2FileFailure,
        changed: PostgreSqlEnumerationCampaignV2FileFailure
    ): Either[
      PostgreSqlEnumerationCampaignV2FileFailure,
      PostgreSqlEnumerationSearchReplayBundleV1FileEvidence
    ] = {
      if (closed) Left(WorkspaceClosed)
      else if (!ready || recorded.isEmpty) Left(unavailable)
      else
        for {
          _ <- ensureStagingStable()
          current <- exactStagedIdentity(path, recorded, changed)
          chain <- inspectInputChain(path).left.map(_ => changed)
          _ <-
            if (chain.entries.last.identity.sameRegularFile(current)) Right(())
            else Left(changed)
          bytes <- readExactFile(path, chain, current.size.toInt).left.map(_ => changed)
        } yield PostgreSqlEnumerationSearchReplayBundleV1FileEvidence(
          current.size,
          sha256(bytes)
        )
    }

    private def exactStagedIdentity(
        path: Path,
        recorded: Option[FileIdentity],
        changed: PostgreSqlEnumerationCampaignV2FileFailure
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, FileIdentity] =
      readSingleLinkRegularIdentity(path, changed).flatMap { current =>
        if (recorded.exists(_.sameRegularFile(current))) Right(current)
        else Left(changed)
      }

    private def createFinalBundle()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      try {
        Files.createDirectory(
          outputBundle,
          PosixFilePermissions.asFileAttribute(DirectoryPermissions)
        )
        // The pathname is externally visible from this exact point. Even if
        // its first identity/permission read fails, never treat the path as
        // safely rollbackable or reusable.
        publicationAttempted = true
        runPublicationStep("bundle-path-created", outputBundle).flatMap { _ =>
          readDirectoryIdentity(outputBundle, PublishIndeterminate).flatMap {
            identity =>
              finalBundleIdentity = Some(identity)
              verifyDirectoryPermissions(outputBundle)
                .left
                .map(_ => PublishIndeterminate)
                .map { _ =>
                  finalBundleReady = true
                  ()
                }
          }
        }
      } catch {
        case _: FileAlreadyExistsException => Left(OutputExists)
        case NonFatal(_) if publicationAttempted => Left(PublishIndeterminate)
        case NonFatal(_)                    => Left(BundleCreateFailed)
      }

    private def linkAndVerify(
        fileName: String,
        staged: Path,
        expected: FileIdentity,
        commitMarker: Boolean = false
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] = {
      val target = outputBundle.resolve(fileName)
      for {
        _ <- ensureFinalBundleStable(BundleChanged)
        _ <- ensureAbsent(target)
        _ <- createHardLink(target, staged, commitMarker)
        _ <- verifyLinked(fileName, staged, expected)
      } yield ()
    }

    private def createHardLink(
        target: Path,
        staged: Path,
        commitMarker: Boolean
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      try {
        if (commitMarker) {
          completeLinkAttempted = true
          runPublicationStep("complete-link-attempt", target) match {
            case Left(problem) => return Left(problem)
            case Right(())     => ()
          }
        }
        Files.createLink(target, staged)
        if (commitMarker) completeLinked = true
        Right(())
      } catch {
        case _: FileAlreadyExistsException => Left(PublishIndeterminate)
        case NonFatal(_)                    => Left(PublishFailed)
      }

    private def verifyLinked(
        fileName: String,
        staged: Path,
        expected: FileIdentity
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] = {
      val target = outputBundle.resolve(fileName)
      for {
        _ <- ensureFinalBundleStable(PublishIndeterminate)
        stagedLinked <- readRegularIdentityWithLinkCount(
          staged,
          expectedLinkCount = 2L,
          PublishIndeterminate
        )
        targetLinked <- readRegularIdentityWithLinkCount(
          target,
          expectedLinkCount = 2L,
          PublishIndeterminate
        )
        _ <-
          if (
            expected.sameRegularFile(stagedLinked) &&
            stagedLinked.sameRegularFile(targetLinked)
          ) Right(())
          else Left(PublishIndeterminate)
        _ <- verifyFilePermissions(target, PublishIndeterminate)
      } yield ()
    }

    private def verifyDataLinks(
        expectedPackage: FileIdentity,
        expectedSearch: FileIdentity
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      verifyLinked(
        PackageFileName,
        stagedPackagePath,
        expectedPackage
      ).flatMap { _ =>
        verifyLinked(
          SearchInputFileName,
          stagedSearchInputPath,
          expectedSearch
        )
      }

    private def verifyAllLinks(
        expectedPackage: FileIdentity,
        expectedSearch: FileIdentity,
        expectedComplete: FileIdentity
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      verifyDataLinks(expectedPackage, expectedSearch).flatMap { _ =>
        verifyLinked(
          CompleteFileName,
          stagedCompletePath,
          expectedComplete
        )
      }

    private def normalizePublicationFailure(
        failure: PostgreSqlEnumerationCampaignV2FileFailure
    ): PostgreSqlEnumerationCampaignV2FileFailure =
      if (!publicationAttempted) failure
      else if (!finalBundleReady) PublishIndeterminate
      else if (completeLinkAttempted || completeLinked)
        failure match {
          case PublishDurabilityFailed => PublishDurabilityFailed
          case _                       => PublishIndeterminate
        }
      else BundleIncomplete

    private def requireBundleState(
        expectedNames: Set[String]
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      ensureFinalBundleStable(BundleChanged).flatMap { _ =>
        listDirectChildren(outputBundle).flatMap { children =>
          val names = children.map(_.getFileName.toString).toSet
          if (names == expectedNames && children.size == expectedNames.size)
            Right(())
          else Left(BundleChanged)
        }
      }

    private def ensureFinalBundleStable(
        failure: PostgreSqlEnumerationCampaignV2FileFailure
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      finalBundleIdentity match {
        case None => Left(failure)
        case Some(expected) =>
          readDirectoryIdentity(outputBundle, failure).flatMap { current =>
            if (!expected.sameDirectory(current)) Left(failure)
            else
              verifyDirectoryPermissions(outputBundle)
                .left
                .map(_ => failure)
          }
      }

    private def ensureStagingStable()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      readDirectoryIdentity(stagingDirectory, StagingChanged).flatMap {
        current =>
          if (stagingIdentity.sameDirectory(current)) Right(())
          else Left(StagingChanged)
      }

    private def runPublicationStep(
        name: String,
        path: Path
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      try {
        afterPublicationStep(name, path)
        Right(())
      } catch {
        case NonFatal(_) => Left(PublishIndeterminate)
      }

    private[cli] def cleanup()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (closed) return Right(())
        closed = true
        if (ensureStagingStable().isLeft) return Left(CleanupFailed)

        val expectedNames = Set(
          AssemblyFileName,
          PackageFileName,
          SearchInputFileName,
          CompleteFileName
        )
        val present = listDirectChildren(stagingDirectory).fold(
          _ => return Left(CleanupFailed),
          identity
        )
        if (!present.forall(path => expectedNames(path.getFileName.toString)))
          return Left(CleanupFailed)

        present.foreach { path =>
          val identity = readRegularIdentity(path, CleanupFailed).fold(
            _ => return Left(CleanupFailed),
            value => value
          )
          val recorded = path.getFileName.toString match {
            case AssemblyFileName    => assemblyIdentity
            case PackageFileName     => packageIdentity
            case SearchInputFileName => searchInputIdentity
            case CompleteFileName    => completeIdentity
          }
          if (!recorded.exists(_.sameObject(identity)))
            return Left(CleanupFailed)
          try Files.delete(path)
          catch {
            case NonFatal(_) => return Left(CleanupFailed)
          }
        }

        if (forceDirectory(stagingDirectory).isLeft)
          return Left(CleanupFailed)
        if (listDirectChildren(stagingDirectory).fold(_ => true, _.nonEmpty))
          return Left(CleanupFailed)
        if (ensureStagingStable().isLeft) return Left(CleanupFailed)
        try Files.delete(stagingDirectory)
        catch {
          case NonFatal(_) => return Left(CleanupFailed)
        }
        forceDirectory(outputParent).left.map(_ => CleanupFailed)
      }
  }

  final class Workspace private[cli] (
      private val output: Path,
      private val outputParent: Path,
      private val outputParentChain: PathChain,
      val stagingDirectory: Path,
      private val stagingIdentity: FileIdentity,
      private val afterHardLink: Path => Unit
  ) {
    val assemblyPath: Path = stagingDirectory.resolve(AssemblyFileName)
    val stagedPackagePath: Path = stagingDirectory.resolve(PackageFileName)

    private var closed = false
    private var assemblyIdentity: Option[FileIdentity] = None
    private var packageIdentity: Option[FileIdentity] = None
    private var packageReady = false
    private var publicationAttempted = false

    def writeAssembly(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (closed) Left(WorkspaceClosed)
        else if (assemblyIdentity.nonEmpty || pathExistsNoFollow(assemblyPath))
          Left(AssemblyExists)
        else {
          writeNewDocument(
            assemblyPath,
            bytes,
            AssemblyExists,
            AssemblyWriteFailed
          ).map { identity =>
            assemblyIdentity = Some(identity)
            ()
          }
        }
      }

    /** Direct writer used by tests and by non-process callers. */
    def writeStagedPackage(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (closed) Left(WorkspaceClosed)
        else if (packageIdentity.nonEmpty || pathExistsNoFollow(stagedPackagePath))
          Left(StagedPackageExists)
        else {
          writeNewDocument(
            stagedPackagePath,
            bytes,
            StagedPackageExists,
            StagedPackageWriteFailed
          ).map { identity =>
            packageIdentity = Some(identity)
            packageReady = true
            ()
          }
        }
      }

    /**
      * Adopt the package created at the reserved path by the offline composer,
      * force its bytes and metadata, and bind its exact identity for publish.
      */
    def adoptStagedPackage(
        maximumBytes: Int = MaximumDocumentBytes
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        require(
          maximumBytes > 0 && maximumBytes <= MaximumDocumentBytes,
          "staged package bound must be within the production envelope"
        )
        if (closed) Left(WorkspaceClosed)
        else if (packageReady || packageIdentity.nonEmpty)
          Left(StagedPackageExists)
        else {
          ensureStagingStable().flatMap { _ =>
            readRegularIdentity(stagedPackagePath, StagedPackageUnavailable)
              .flatMap { observed =>
                readSingleLinkRegularIdentity(
                  stagedPackagePath,
                  StagedPackageInvalid
                ).flatMap { before =>
                  if (observed.sameRegularFile(before)) Right(before)
                  else Left(StagedPackageChanged)
                }
              }
              .flatMap { before =>
                // The reserved name was absent in the freshly-created staging
                // directory. Once a regular file appears there, retain its
                // identity even when later validation fails so exact cleanup
                // can unlink the partial composer output.
                packageIdentity = Some(before)
                if (before.size <= 0L || before.size > maximumBytes.toLong)
                  Left(StagedPackageInvalid)
                else {
                  setAndVerifyFilePermissions(stagedPackagePath).flatMap { _ =>
                    forceExistingFile(
                      stagedPackagePath,
                      before,
                      StagedPackageChanged
                    ).flatMap { _ =>
                      readSingleLinkRegularIdentity(
                        stagedPackagePath,
                        StagedPackageChanged
                      ).flatMap { after =>
                        if (!before.sameRegularFile(after))
                          Left(StagedPackageChanged)
                        else
                          forceDirectory(stagingDirectory).map { _ =>
                            packageIdentity = Some(after)
                            packageReady = true
                            ()
                          }
                      }
                    }
                  }
                }
              }
          }
        }
      }

    /** Exact, identity-rechecked package length after successful adoption. */
    def stagedPackageByteLength()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Long] =
      synchronized {
        if (closed) Left(WorkspaceClosed)
        else if (!packageReady || packageIdentity.isEmpty)
          Left(StagedPackageUnavailable)
        else
          readSingleLinkRegularIdentity(
            stagedPackagePath,
            StagedPackageChanged
          )
            .flatMap { current =>
              if (packageIdentity.exists(_.sameRegularFile(current)))
                Right(current.size)
              else Left(StagedPackageChanged)
            }
      }

    /** Publish exactly once through a same-filesystem create-only hard link. */
    def publish()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (closed) Left(WorkspaceClosed)
        else if (publicationAttempted) Left(OutputExists)
        else if (!packageReady || packageIdentity.isEmpty)
          Left(StagedPackageUnavailable)
        else {
          for {
            _ <- ensureStagingStable()
            _ <- revalidateChain(outputParentChain, InputChanged)
            _ <- ensureAbsent(output)
            currentPackage <- readSingleLinkRegularIdentity(
              stagedPackagePath,
              StagedPackageChanged
            )
            _ <-
              if (packageIdentity.exists(_.sameRegularFile(currentPackage)))
                Right(())
              else Left(StagedPackageChanged)
            _ <- requireSameFileStore(stagedPackagePath, outputParent)
            _ <- createOutputHardLink()
            // A successful createLink has already created an externally
            // visible name. Make the attempt terminal before any path-based
            // validation: portable Java NIO cannot atomically unlink only if
            // the name still denotes this inode, so a later failure must
            // preserve the name rather than risk deleting a replacement.
            _ = publicationAttempted = true
            _ <- runAfterHardLink()
            linkedPackage <- readRegularIdentityWithLinkCount(
              stagedPackagePath,
              expectedLinkCount = 2L,
              PublishIndeterminate
            )
            publishedIdentity <- readRegularIdentityWithLinkCount(
              output,
              expectedLinkCount = 2L,
              PublishIndeterminate
            )
            _ <-
              if (
                currentPackage.sameRegularFile(linkedPackage) &&
                linkedPackage.sameRegularFile(publishedIdentity)
              ) Right(())
              else Left(PublishIndeterminate)
            _ <- {
              // From this point onward the output name is proven to be our
              // exact hard link. Keep it on a durability failure rather than
              // risk deleting a successfully published immutable package.
              forceDirectory(outputParent).left.map(_ => PublishDurabilityFailed)
            }
            _ <- revalidateChain(
              outputParentChain,
              PublishDurabilityFailed
            )
          } yield ()
        }
      }

    private def runAfterHardLink()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      try {
        afterHardLink(output)
        Right(())
      } catch {
        case NonFatal(_) => Left(PublishIndeterminate)
      }

    private def writeNewDocument(
        path: Path,
        bytes: Array[Byte],
        existsFailure: PostgreSqlEnumerationCampaignV2FileFailure,
        writeFailure: PostgreSqlEnumerationCampaignV2FileFailure
    ): Either[PostgreSqlEnumerationCampaignV2FileFailure, FileIdentity] = {
      if (
        bytes == null || bytes.isEmpty ||
        bytes.length > MaximumDocumentBytes
      ) return Left(InvalidDocumentBytes)
      val snapshot = bytes.clone()
      ensureStagingStable().flatMap { _ =>
        ensureAbsent(path).left.map {
          case OutputExists => existsFailure
          case _            => writeFailure
        }.flatMap { _ =>
          var channel: FileChannel = null
          var createdIdentity: FileIdentity = null
          var failure: PostgreSqlEnumerationCampaignV2FileFailure = null
          try {
            channel = FileChannel.open(
              path,
              openOptions(
                StandardOpenOption.CREATE_NEW,
                StandardOpenOption.WRITE,
                LinkOption.NOFOLLOW_LINKS
              ),
              PosixFilePermissions.asFileAttribute(FilePermissions)
            )
            readRegularIdentity(path, writeFailure) match {
              case Left(problem) => failure = problem
              case Right(created) =>
                createdIdentity = created
                if (path == assemblyPath) assemblyIdentity = Some(created)
                else if (path == stagedPackagePath)
                  packageIdentity = Some(created)
                val buffer = ByteBuffer.wrap(snapshot)
                var zeroWrites = 0
                while (buffer.hasRemaining && zeroWrites < 16) {
                  val written = channel.write(buffer)
                  if (written == 0) zeroWrites += 1 else zeroWrites = 0
                }
                if (buffer.hasRemaining) failure = writeFailure
                else channel.force(true)
            }
          } catch {
            case _: FileAlreadyExistsException => failure = existsFailure
            case NonFatal(_)                    => failure = writeFailure
          } finally {
            if (channel != null) {
              try channel.close()
              catch {
                case NonFatal(_) => failure = writeFailure
              }
            }
          }
          if (failure != null) Left(failure)
          else
            for {
              after <- readSingleLinkRegularIdentity(path, writeFailure)
              _ <-
                if (
                  createdIdentity != null &&
                  createdIdentity.sameObject(after) &&
                  after.size == snapshot.length.toLong
                ) Right(())
                else Left(writeFailure)
              _ <- setAndVerifyFilePermissions(path)
              _ <- forceExistingFile(path, after, writeFailure)
              durable <- readSingleLinkRegularIdentity(path, writeFailure)
              _ <-
                if (after.sameRegularFile(durable)) Right(())
                else Left(writeFailure)
              _ <- forceDirectory(stagingDirectory)
            } yield durable
        }
      }
    }

    private def createOutputHardLink()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      try {
        Files.createLink(output, stagedPackagePath)
        Right(())
      } catch {
        case _: FileAlreadyExistsException => Left(OutputExists)
        case NonFatal(_)                    => Left(PublishFailed)
      }

    private def ensureStagingStable()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      readDirectoryIdentity(stagingDirectory, StagingChanged).flatMap {
        current =>
          if (stagingIdentity.sameDirectory(current)) Right(())
          else Left(StagingChanged)
      }

    private[cli] def cleanup()
        : Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
      synchronized {
        if (closed) return Right(())
        closed = true
        if (ensureStagingStable().isLeft) return Left(CleanupFailed)

        val expectedNames = Set(AssemblyFileName, PackageFileName)
        val present = listDirectChildren(stagingDirectory).fold(
          _ => return Left(CleanupFailed),
          identity
        )
        if (!present.forall(path => expectedNames.contains(path.getFileName.toString)))
          return Left(CleanupFailed)

        present.foreach { path =>
          val identity = readRegularIdentity(path, CleanupFailed).fold(
            _ => return Left(CleanupFailed),
            value => value
          )
          val recorded =
            if (path.getFileName.toString == AssemblyFileName) assemblyIdentity
            else packageIdentity
          if (recorded.exists(!_.sameObject(identity)))
            return Left(CleanupFailed)
          try Files.delete(path)
          catch {
            case NonFatal(_) => return Left(CleanupFailed)
          }
        }

        if (forceDirectory(stagingDirectory).isLeft)
          return Left(CleanupFailed)
        if (listDirectChildren(stagingDirectory).fold(_ => true, _.nonEmpty))
          return Left(CleanupFailed)
        if (ensureStagingStable().isLeft) return Left(CleanupFailed)
        try Files.delete(stagingDirectory)
        catch {
          case NonFatal(_) => return Left(CleanupFailed)
        }
        forceDirectory(outputParent).left.map(_ => CleanupFailed)
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

  private def createWorkspace(
      outputPath: Path,
      afterHardLink: Path => Unit
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Workspace] = {
    val prepared = for {
      output <- absoluteUnambiguous(outputPath)
      parent <- Option(output.getParent).toRight(OutputParentUnavailable)
      parentChain <- inspectDirectoryChain(parent, OutputParentUnavailable)
      _ <- ensureAbsent(output)
      _ <- requirePosix(parent)
    } yield (output, parent, parentChain)

    prepared.flatMap { case (output, parent, parentChain) =>
      createStaging(parent).flatMap { staging =>
        readDirectoryIdentity(staging, StagingCreateFailed) match {
          case Left(_) =>
            // Java NIO directory creation returns a path, not an open handle.
            // If the first no-follow file-key read fails, ownership cannot be
            // distinguished from an immediate replacement. Deleting would be
            // unsafe, so this single pre-identity gap is explicit and leaves
            // the path untouched for operator inspection.
            Left(StagingIdentityUnavailable)
          case Right(stagingIdentity) =>
            val validation = for {
              _ <- verifyDirectoryPermissions(staging)
              _ <- revalidateChain(parentChain, StagingCreateFailed)
              _ <- forceDirectory(parent)
            } yield new Workspace(
              output,
              parent,
              parentChain,
              staging,
              stagingIdentity,
              afterHardLink
            )
            validation match {
              case success @ Right(_) => success
              case Left(problem) =>
                cleanupFreshStaging(
                  staging,
                  stagingIdentity,
                  parent
                ) match {
                  case Right(()) => Left(problem)
                  case Left(_)   => Left(CleanupFailed)
                }
            }
        }
      }
    }
  }

  private def createPairWorkspace(
      outputBundlePath: Path,
      afterPublicationStep: (String, Path) => Unit
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, PairWorkspace] = {
    val prepared = for {
      output <- absoluteUnambiguous(outputBundlePath)
      parent <- Option(output.getParent).toRight(OutputParentUnavailable)
      parentChain <- inspectDirectoryChain(parent, OutputParentUnavailable)
      _ <- ensureAbsent(output)
      _ <- requirePosix(parent)
    } yield (output, parent, parentChain)

    prepared.flatMap { case (output, parent, parentChain) =>
      createStaging(parent).flatMap { staging =>
        readDirectoryIdentity(staging, StagingCreateFailed) match {
          case Left(_) => Left(StagingIdentityUnavailable)
          case Right(stagingIdentity) =>
            val validation = for {
              _ <- verifyDirectoryPermissions(staging)
              _ <- revalidateChain(parentChain, StagingCreateFailed)
              _ <- forceDirectory(parent)
            } yield new PairWorkspace(
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

  private def cleanupFreshStaging(
      staging: Path,
      expected: FileIdentity,
      parent: Path
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
    readDirectoryIdentity(staging, CleanupFailed).flatMap { current =>
      if (!expected.sameDirectory(current)) Left(CleanupFailed)
      else
        listDirectChildren(staging).flatMap { children =>
          if (children.nonEmpty) Left(CleanupFailed)
          else {
            try {
              Files.delete(staging)
              forceDirectory(parent).left.map(_ => CleanupFailed)
            } catch {
              case NonFatal(_) => Left(CleanupFailed)
            }
          }
        }
    }

  private def createStaging(
      parent: Path
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Path] =
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

  private def sha256(bytes: Array[Byte]): String = {
    require(bytes != null, "SHA-256 input must not be null")
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
  }

  private def absoluteUnambiguous(
      path: Path
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Path] = {
    if (path == null) Left(MissingPath)
    else if (
      path.toString.isEmpty ||
      path.iterator().asScala.exists { component =>
        component.toString == "." || component.toString == ".."
      }
    ) Left(AmbiguousPath)
    else {
      try Right(path.toAbsolutePath.normalize())
      catch {
        case NonFatal(_) => Left(AmbiguousPath)
      }
    }
  }

  private def inspectInputChain(
      input: Path
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, PathChain] = {
    val parent = Option(input.getParent).toRight(InputUnavailable)
    parent.flatMap(inspectDirectoryChain(_, InputUnavailable)).flatMap {
      parentChain =>
        readIdentity(input, InputUnavailable).flatMap { identity =>
          if (!identity.isRegularFile) Left(InputNotRegular)
          else Right(PathChain(parentChain.entries :+ PathEntry(input, identity)))
        }
    }
  }

  private def inspectDirectoryChain(
      directory: Path,
      unavailable: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, PathChain] = {
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
        PostgreSqlEnumerationCampaignV2FileFailure,
        Vector[PathEntry]
      ]
    ) { (checked, path) =>
      checked.flatMap { prefix =>
        readIdentity(path, unavailable).flatMap { identity =>
          if (identity.isDirectory)
            Right(prefix :+ PathEntry(path, identity))
          else Left(UnsafePathComponent)
        }
      }
    }.map(PathChain)
  }

  private def revalidateChain(
      chain: PathChain,
      failure: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] = {
    chain.entries.foldLeft(
      Right(()): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit]
    ) { (checked, entry) =>
      checked.flatMap { _ =>
        readIdentity(entry.path, failure).flatMap { current =>
          val same =
            if (entry.identity.isDirectory)
              entry.identity.sameDirectory(current)
            else entry.identity.sameRegularFile(current)
          if (same) Right(()) else Left(failure)
        }
      }
    }
  }

  private def readExactFile(
      path: Path,
      chain: PathChain,
      expectedSize: Int
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Array[Byte]] = {
    var channel: FileChannel = null
    var result: Either[PostgreSqlEnumerationCampaignV2FileFailure, Array[Byte]] =
      Left(InputReadFailed)
    try {
      channel = FileChannel.open(
        path,
        openOptions(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)
      )
      if (channel.size() != expectedSize.toLong) result = Left(InputChanged)
      else {
        val bytes = new Array[Byte](expectedSize)
        val buffer = ByteBuffer.wrap(bytes)
        var zeroReads = 0
        while (buffer.hasRemaining && zeroReads < 16) {
          val read = channel.read(buffer)
          if (read < 0) zeroReads = 16
          else if (read == 0) zeroReads += 1
          else zeroReads = 0
        }
        if (buffer.hasRemaining || channel.size() != expectedSize.toLong)
          result = Left(InputChanged)
        else result = Right(bytes)
      }
    } catch {
      case _: NoSuchFileException => result = Left(InputUnavailable)
      case NonFatal(_)            => result = Left(InputReadFailed)
    } finally {
      if (channel != null) {
        try channel.close()
        catch {
          case NonFatal(_) => result = Left(InputReadFailed)
        }
      }
    }
    result.flatMap { bytes =>
      revalidateChain(chain, InputChanged).map(_ => bytes)
    }
  }

  private def readIdentity(
      path: Path,
      unavailable: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, FileIdentity] =
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
      failure: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, FileIdentity] =
    readIdentity(path, failure).flatMap { identity =>
      if (identity.isDirectory) Right(identity) else Left(failure)
    }

  private def readRegularIdentity(
      path: Path,
      failure: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, FileIdentity] =
    readIdentity(path, failure).flatMap { identity =>
      if (identity.isRegularFile) Right(identity) else Left(failure)
    }

  /**
    * Bind a regular pathname to both one file key and one hard-link count.
    * The two identity reads bracket the provider-specific `unix:nlink` read;
    * fail closed when that Java NIO attribute is unavailable. Campaign
    * package staging is intentionally supported only on this stronger
    * create-only boundary.
    */
  private def readRegularIdentityWithLinkCount(
      path: Path,
      expectedLinkCount: Long,
      failure: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, FileIdentity] =
    for {
      before <- readRegularIdentity(path, failure)
      linkCount <- readUnixLinkCount(path, failure)
      after <- readRegularIdentity(path, failure)
      _ <-
        if (
          before.sameRegularFile(after) &&
          linkCount == expectedLinkCount
        ) Right(())
        else Left(failure)
    } yield after

  private def readSingleLinkRegularIdentity(
      path: Path,
      failure: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, FileIdentity] =
    readRegularIdentityWithLinkCount(
      path,
      expectedLinkCount = 1L,
      failure = failure
    )

  private def readUnixLinkCount(
      path: Path,
      failure: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Long] =
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
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
    try {
      Files.readAttributes(path, classOf[BasicFileAttributes], NoFollow: _*)
      Left(OutputExists)
    } catch {
      case _: NoSuchFileException => Right(())
      case NonFatal(_)            => Left(OutputParentUnavailable)
    }

  private def pathExistsNoFollow(path: Path): Boolean =
    try {
      Files.readAttributes(path, classOf[BasicFileAttributes], NoFollow: _*)
      true
    } catch {
      case _: NoSuchFileException => false
      case NonFatal(_)            => true
    }

  private def requirePosix(
      path: Path
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
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
      path: Path
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
    try {
      val permissions = Files.getPosixFilePermissions(path, NoFollow: _*)
      if (permissions == DirectoryPermissions) Right(())
      else Left(StagingCreateFailed)
    } catch {
      case NonFatal(_) => Left(PosixPermissionsUnavailable)
    }

  private def setAndVerifyFilePermissions(
      path: Path
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
    try {
      Files.setPosixFilePermissions(path, FilePermissions)
      val permissions = Files.getPosixFilePermissions(path, NoFollow: _*)
      if (permissions == FilePermissions) Right(())
      else Left(FileSyncFailed)
    } catch {
      case NonFatal(_) => Left(PosixPermissionsUnavailable)
    }

  private def verifyFilePermissions(
      path: Path,
      failure: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
    try {
      val permissions = Files.getPosixFilePermissions(path, NoFollow: _*)
      if (permissions == FilePermissions) Right(()) else Left(failure)
    } catch {
      case NonFatal(_) => Left(failure)
    }

  private def forceExistingFile(
      path: Path,
      expected: FileIdentity,
      changedFailure: PostgreSqlEnumerationCampaignV2FileFailure
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] = {
    var channel: FileChannel = null
    var result: Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
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
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] = {
    var channel: FileChannel = null
    var result: Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
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
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Unit] =
    try {
      val sourceStore: FileStore = Files.getFileStore(source)
      val targetStore: FileStore = Files.getFileStore(targetParent)
      if (sourceStore == targetStore) Right(()) else Left(DifferentFileSystem)
    } catch {
      case NonFatal(_) => Left(DifferentFileSystem)
    }

  private def listDirectChildren(
      directory: Path
  ): Either[PostgreSqlEnumerationCampaignV2FileFailure, Vector[Path]] = {
    var stream: java.nio.file.DirectoryStream[Path] = null
    var result: Either[PostgreSqlEnumerationCampaignV2FileFailure, Vector[Path]] =
      Left(CleanupFailed)
    try {
      stream = Files.newDirectoryStream(directory)
      result = Right(stream.iterator().asScala.toVector)
    } catch {
      case NonFatal(_) => result = Left(CleanupFailed)
    } finally {
      if (stream != null) {
        try stream.close()
        catch {
          case NonFatal(_) => result = Left(CleanupFailed)
        }
      }
    }
    result
  }

  private def openOptions(options: OpenOption*): java.util.Set[OpenOption] = {
    val result = new java.util.HashSet[OpenOption]()
    options.foreach(result.add)
    result
  }

}
