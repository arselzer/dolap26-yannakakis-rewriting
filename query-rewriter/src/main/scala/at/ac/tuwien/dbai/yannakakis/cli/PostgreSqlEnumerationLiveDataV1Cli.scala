package at.ac.tuwien.dbai.yannakakis.cli

import java.io.{File, OutputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.channels.FileChannel
import java.nio.file.attribute.{BasicFileAttributes, FileTime, PosixFilePermissions}
import java.nio.file.{Files, InvalidPathException, LinkOption, Path, Paths, StandardOpenOption}
import java.security.MessageDigest

import scala.jdk.CollectionConverters._
import scala.util.Using
import scala.util.control.NonFatal

import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.databind.ObjectMapper
import org.postgresql.PGConnection
import org.postgresql.copy.CopyManager
import org.postgresql.ds.PGSimpleDataSource

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationLiveDataV1Batch,
  PostgreSqlEnumerationLiveDataV1Capture,
  PostgreSqlEnumerationLiveDataV1CaptureFailure,
  PostgreSqlEnumerationLiveDataV1Request
}

private[cli] sealed trait PostgreSqlEnumerationLiveDataV1CliConfigFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlEnumerationLiveDataV1CliConfigFailure {
  case object InvalidArguments extends PostgreSqlEnumerationLiveDataV1CliConfigFailure {
    val code = "live-data-v1-cli.invalid-arguments"
  }
  case object MissingConfiguration extends PostgreSqlEnumerationLiveDataV1CliConfigFailure {
    val code = "live-data-v1-cli.missing-configuration"
  }
  case object InvalidPath extends PostgreSqlEnumerationLiveDataV1CliConfigFailure {
    val code = "live-data-v1-cli.invalid-path"
  }
}

private[cli] final case class PostgreSqlEnumerationLiveDataV1CliConfig(
    requestBundle: Path,
    outputAssembly: Path,
    jdbc: JdbcEnvironment
) {
  require(requestBundle != null && requestBundle.isAbsolute)
  require(outputAssembly != null && outputAssembly.isAbsolute)
  require(requestBundle != outputAssembly)
  require(jdbc != null)
}

private[cli] object PostgreSqlEnumerationLiveDataV1CliConfig {
  import PostgreSqlEnumerationLiveDataV1CliConfigFailure._

  private val Options = Set("--request-bundle", "--output-assembly")

  def parse(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path
  ): Either[
    PostgreSqlEnumerationLiveDataV1CliConfigFailure,
    PostgreSqlEnumerationLiveDataV1CliConfig
  ] = {
    if (
      arguments == null || environment == null || workingDirectory == null ||
      !workingDirectory.isAbsolute || workingDirectory != workingDirectory.normalize()
    ) return Left(InvalidArguments)

    val args = arguments.toVector
    var values = Map.empty[String, String]
    var index = 0
    while (index < args.size) {
      val option = args(index)
      if (
        option == null || !Options(option) || values.contains(option) ||
        index + 1 >= args.size
      ) return Left(InvalidArguments)
      val value = args(index + 1)
      if (value == null || value.isEmpty || value.startsWith("--"))
        return Left(InvalidArguments)
      values += option -> value
      index += 2
    }
    if (!Options.forall(values.contains)) return Left(InvalidArguments)

    val jdbcNames = Vector(
      PostgreSqlRewriteCliConfig.JdbcUrlEnvironment,
      PostgreSqlRewriteCliConfig.UserEnvironment,
      PostgreSqlRewriteCliConfig.PasswordEnvironment
    )
    if (!jdbcNames.forall(environment.contains)) return Left(MissingConfiguration)
    val jdbcUrl = environment(PostgreSqlRewriteCliConfig.JdbcUrlEnvironment)
    val user = environment(PostgreSqlRewriteCliConfig.UserEnvironment)
    val password = environment(PostgreSqlRewriteCliConfig.PasswordEnvironment)
    if (
      jdbcUrl == null || jdbcUrl.trim.isEmpty || user == null || user.trim.isEmpty ||
      password == null
    ) return Left(MissingConfiguration)

    for {
      request <- resolve(values("--request-bundle"), workingDirectory)
      output <- resolve(values("--output-assembly"), workingDirectory)
      _ <-
        if (
          request != output && !output.startsWith(request) &&
          !request.startsWith(output)
        ) Right(())
        else Left(InvalidPath)
    } yield PostgreSqlEnumerationLiveDataV1CliConfig(
      request,
      output,
      JdbcEnvironment(jdbcUrl, user, password)
    )
  }

  private def resolve(
      spelling: String,
      workingDirectory: Path
  ): Either[PostgreSqlEnumerationLiveDataV1CliConfigFailure, Path] =
    try {
      val parsed = Paths.get(spelling)
      val segments = parsed.iterator()
      var ambiguous = parsed.toString.isEmpty || parsed != parsed.normalize()
      while (segments.hasNext) {
        val segment = segments.next().toString
        if (segment == "." || segment == "..") ambiguous = true
      }
      if (ambiguous) Left(InvalidPath)
      else {
        val absolute =
          if (parsed.isAbsolute) parsed.normalize()
          else workingDirectory.resolve(parsed).normalize()
        if (absolute.isAbsolute && absolute == absolute.normalize()) Right(absolute)
        else Left(InvalidPath)
      }
    } catch { case _: InvalidPathException => Left(InvalidPath) }
}

private[cli] sealed trait PostgreSqlEnumerationLiveDataV1PublicationFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlEnumerationLiveDataV1PublicationFailure {
  case object InvalidOutput extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.invalid-output"
  }
  case object OutputExists extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.output-exists"
  }
  case object WorkspaceFailed extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.workspace-failed"
  }
  case object AssemblyExists extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.assembly-exists"
  }
  case object AssemblyWriteFailed extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.assembly-write-failed"
  }
  case object CompleteExists extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.complete-exists"
  }
  case object CompleteWriteFailed extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.complete-write-failed"
  }
  case object EnvelopeMismatch extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.envelope-mismatch"
  }
  case object DurabilityFailed extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.durability-failed"
  }
  case object PublicationFailed extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.publish-failed"
  }
  case object CleanupFailed extends PostgreSqlEnumerationLiveDataV1PublicationFailure {
    val code = "live-data-v1-publication.cleanup-failed"
  }
}

/**
  * Private create-only raw-assembly directory. `complete.json` must be written
  * after every COPY output and `assembly.json`; it is the logical marker, not
  * proof of external chronology or an atomic evidence snapshot. The final
  * directory is reserved first with atomic `CREATE_NEW` directory semantics,
  * avoiding Java `ATOMIC_MOVE`'s unspecified existing-target behavior. Until
  * the marker appears, the visible directory is intentionally incomplete; a
  * failed cleanup leaves an incomplete/indeterminate quarantine. The mode-0700
  * parent and identity rechecks still rely on a non-hostile same-owner process
  * and do not establish publication history.
  */
private[cli] object PostgreSqlEnumerationLiveDataV1Publisher {
  import PostgreSqlEnumerationLiveDataV1PublicationFailure._

  private val DirectoryPermissions = PosixFilePermissions.fromString("rwx------")
  private val FilePermissions = PosixFilePermissions.fromString("rw-------")
  private val AssemblyName = "assembly.json"
  private val CompleteName = "complete.json"
  private val CopyOutName = "copy-out"
  private lazy val CurrentUid: Long =
    new com.sun.security.auth.module.UnixSystem().getUid

  private final case class FileIdentity(
      key: AnyRef,
      directory: Boolean,
      regular: Boolean,
      size: Long,
      modified: FileTime
  ) {
    def sameObject(other: FileIdentity): Boolean =
      other != null && key == other.key && directory == other.directory &&
        regular == other.regular
    def sameFile(other: FileIdentity): Boolean =
      sameObject(other) && regular && size == other.size && modified == other.modified
  }

  def withWorkspace[A](
      output: Path
  )(
      operation: Workspace => Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, A]
  ): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, A] =
    withMappedWorkspace(
      output,
      (failure: PostgreSqlEnumerationLiveDataV1PublicationFailure) => failure,
      WorkspaceFailed
    )(operation)

  def withMappedWorkspace[E, A](
      output: Path,
      mapFailure: PostgreSqlEnumerationLiveDataV1PublicationFailure => E,
      unexpected: E
  )(
      operation: Workspace => Either[E, A]
  ): Either[E, A] = {
    if (operation == null || mapFailure == null) return Left(unexpected)
    createWorkspace(output).left.map(mapFailure).flatMap { workspace =>
      var cleanup: Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Unit] = Right(())
      val result =
        try {
          val value = operation(workspace)
          if (value == null) Left(unexpected) else value
        } catch { case NonFatal(_) => Left(unexpected) }
        finally cleanup = workspace.cleanup()
      cleanup match {
        case Left(_) if !workspace.published => Left(mapFailure(CleanupFailed))
        case _                               => result
      }
    }
  }

  private def createWorkspace(
      output: Path
  ): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Workspace] = {
    if (output == null || !output.isAbsolute || output != output.normalize())
      return Left(InvalidOutput)
    val parent = output.getParent
    if (parent == null || !privateSafeDirectory(parent)) return Left(InvalidOutput)
    if (Files.exists(output, LinkOption.NOFOLLOW_LINKS)) return Left(OutputExists)
    var reserved = false
    var copyCreated = false
    try {
      Files.createDirectory(
        output,
        PosixFilePermissions.asFileAttribute(DirectoryPermissions)
      )
      reserved = true
      val copyOut = output.resolve(CopyOutName)
      Files.createDirectory(
        copyOut,
        PosixFilePermissions.asFileAttribute(DirectoryPermissions)
      )
      copyCreated = true
      (readIdentity(output), readIdentity(copyOut)) match {
        case (Some(stagingIdentity), Some(copyIdentity)) =>
          Right(new Workspace(
            output,
            parent,
            output,
            copyOut,
            stagingIdentity,
            copyIdentity
          ))
        case _ =>
          if (cleanupReservation(output, copyCreated = true)) Left(WorkspaceFailed)
          else Left(CleanupFailed)
      }
    } catch {
      case _: java.nio.file.FileAlreadyExistsException if !reserved => Left(OutputExists)
      case NonFatal(_) =>
        if (!reserved || cleanupReservation(output, copyCreated)) Left(WorkspaceFailed)
        else Left(CleanupFailed)
    }
  }

  private def cleanupReservation(output: Path, copyCreated: Boolean): Boolean = {
    var failed = false
    if (copyCreated)
      try Files.deleteIfExists(output.resolve(CopyOutName))
      catch { case NonFatal(_) => failed = true }
    try Files.deleteIfExists(output) catch { case NonFatal(_) => failed = true }
    val absent = try !Files.exists(output, LinkOption.NOFOLLOW_LINKS)
    catch { case NonFatal(_) => false }
    !failed && absent
  }

  final class Workspace private[cli] (
      val output: Path,
      private val parent: Path,
      val stagingDirectory: Path,
      val copyOutDirectory: Path,
      private val stagingIdentity: FileIdentity,
      private val copyOutIdentity: FileIdentity
  ) {
    private var assemblyWritten = false
    private var completeWritten = false
    private var publicationComplete = false
    private var expectedCopyNames = Vector.empty[String]
    private var assemblyIdentity: Option[FileIdentity] = None
    private var completeIdentity: Option[FileIdentity] = None
    private var copyIdentities = Map.empty[String, FileIdentity]

    def published: Boolean = synchronized(publicationComplete)

    def writeAssembly(
        bytes: Array[Byte]
    ): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Unit] = synchronized {
      if (assemblyWritten) Left(AssemblyExists)
      else
        writePrivateDocument(stagingDirectory.resolve(AssemblyName), bytes, AssemblyExists, AssemblyWriteFailed)
          .map { identity =>
            assemblyIdentity = Some(identity)
            assemblyWritten = true
          }
    }

    def writeComplete(
        bytes: Array[Byte],
        tableNames: Vector[String]
    ): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Unit] = synchronized {
      if (completeWritten) return Left(CompleteExists)
      if (
        !assemblyWritten || tableNames == null || tableNames.isEmpty ||
        tableNames.distinct.size != tableNames.size ||
        !tableNames.forall(_.matches("[a-z][a-z0-9_]{0,62}"))
      ) return Left(EnvelopeMismatch)
      validateCopyEnvelope(tableNames).flatMap { _ =>
        writePrivateDocument(stagingDirectory.resolve(CompleteName), bytes, CompleteExists, CompleteWriteFailed)
          .map { identity =>
            completeIdentity = Some(identity)
            expectedCopyNames = tableNames.map(_ + ".copy")
            completeWritten = true
          }
      }
    }

    def publish(): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Unit] = synchronized {
      if (!assemblyWritten || !completeWritten || publicationComplete)
        return Left(EnvelopeMismatch)
      for {
        _ <- validateEnvelope()
        _ <- forceDirectory(copyOutDirectory)
        _ <- forceDirectory(stagingDirectory)
        _ <- forceDirectory(parent)
        _ <- validatePublishedEnvelope()
      } yield publicationComplete = true
    }

    private[cli] def cleanup(): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Unit] = synchronized {
      if (publicationComplete) Right(())
      else {
        var failed = false
        expectedOrObservedCopyNames().foreach { name =>
          try Files.deleteIfExists(copyOutDirectory.resolve(name))
          catch { case NonFatal(_) => failed = true }
        }
        Vector(CompleteName, AssemblyName).foreach { name =>
          try Files.deleteIfExists(stagingDirectory.resolve(name))
          catch { case NonFatal(_) => failed = true }
        }
        try Files.deleteIfExists(copyOutDirectory) catch { case NonFatal(_) => failed = true }
        try Files.deleteIfExists(stagingDirectory) catch { case NonFatal(_) => failed = true }
        if (failed) Left(CleanupFailed) else Right(())
      }
    }

    private def expectedOrObservedCopyNames(): Vector[String] =
      if (expectedCopyNames.nonEmpty) expectedCopyNames
      else
        try Using.resource(Files.list(copyOutDirectory))(
          _.iterator().asScala
            .map(_.getFileName.toString).toVector
            .filter(_.matches("[a-z][a-z0-9_]{0,62}\\.copy"))
        )
        catch { case NonFatal(_) => Vector.empty }

    private def validateCopyEnvelope(
        tableNames: Vector[String]
    ): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Unit] = {
      val expected = tableNames.map(_ + ".copy").sorted
      val observed =
        try Using.resource(Files.list(copyOutDirectory))(
          _.iterator().asScala.map(_.getFileName.toString).toVector.sorted
        )
        catch { case NonFatal(_) => return Left(EnvelopeMismatch) }
      if (observed != expected) return Left(EnvelopeMismatch)
      val identities = observed.map { name =>
        val path = copyOutDirectory.resolve(name)
        if (!privateSingleLinkFile(path)) return Left(EnvelopeMismatch)
        val identity = readIdentity(path).getOrElse(return Left(EnvelopeMismatch))
        name -> identity
      }.toMap
      copyIdentities = identities
      Right(())
    }

    private def validateEnvelope(): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Unit] = {
      val rootNames =
        try Using.resource(Files.list(stagingDirectory))(
          _.iterator().asScala.map(_.getFileName.toString).toVector.sorted
        )
        catch { case NonFatal(_) => return Left(EnvelopeMismatch) }
      if (rootNames != Vector(AssemblyName, CompleteName, CopyOutName).sorted)
        return Left(EnvelopeMismatch)
      if (
        !privateSafeDirectory(stagingDirectory) ||
        !privateSafeDirectory(copyOutDirectory) ||
        !privateSingleLinkFile(stagingDirectory.resolve(AssemblyName)) ||
        !privateSingleLinkFile(stagingDirectory.resolve(CompleteName))
      ) return Left(EnvelopeMismatch)
      validateCopyEnvelope(expectedCopyNames.map(_.stripSuffix(".copy")))
    }

    private def validatePublishedEnvelope(): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Unit] = {
      val currentRoot = readIdentity(output).getOrElse(return Left(PublicationFailed))
      val currentCopy = readIdentity(output.resolve(CopyOutName)).getOrElse(return Left(PublicationFailed))
      if (
        !privateSafeDirectory(output) || !stagingIdentity.sameObject(currentRoot) ||
        !copyOutIdentity.sameObject(currentCopy)
      ) return Left(PublicationFailed)
      val rootNames =
        try Using.resource(Files.list(output))(
          _.iterator().asScala.map(_.getFileName.toString).toVector.sorted
        )
        catch { case NonFatal(_) => return Left(PublicationFailed) }
      if (rootNames != Vector(AssemblyName, CompleteName, CopyOutName).sorted)
        return Left(PublicationFailed)
      val outputAssembly = readIdentity(output.resolve(AssemblyName))
      val outputComplete = readIdentity(output.resolve(CompleteName))
      if (
        !assemblyIdentity.exists(expected => outputAssembly.exists(expected.sameFile)) ||
        !completeIdentity.exists(expected => outputComplete.exists(expected.sameFile))
      ) return Left(PublicationFailed)
      val copyNames = Using.resource(Files.list(output.resolve(CopyOutName)))(
        _.iterator().asScala.map(_.getFileName.toString).toVector.sorted
      )
      if (copyNames != expectedCopyNames.sorted) return Left(PublicationFailed)
      copyNames.foreach { name =>
        val current = readIdentity(output.resolve(CopyOutName).resolve(name))
        if (!copyIdentities.get(name).exists(expected => current.exists(expected.sameFile)))
          return Left(PublicationFailed)
        if (!privateSingleLinkFile(output.resolve(CopyOutName).resolve(name)))
          return Left(PublicationFailed)
      }
      if (
        !privateSingleLinkFile(output.resolve(AssemblyName)) ||
        !privateSingleLinkFile(output.resolve(CompleteName))
      ) Left(PublicationFailed)
      else Right(())
    }
  }

  private def writePrivateDocument(
      path: Path,
      bytes: Array[Byte],
      exists: PostgreSqlEnumerationLiveDataV1PublicationFailure,
      failed: PostgreSqlEnumerationLiveDataV1PublicationFailure
  ): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, FileIdentity] = {
    if (bytes == null || bytes.isEmpty || bytes.last != '\n'.toByte)
      return Left(failed)
    var channel: FileChannel = null
    try {
      val options = new java.util.HashSet[java.nio.file.OpenOption]()
      options.add(StandardOpenOption.CREATE_NEW)
      options.add(StandardOpenOption.WRITE)
      options.add(LinkOption.NOFOLLOW_LINKS)
      channel = FileChannel.open(
        path,
        options,
        PosixFilePermissions.asFileAttribute(FilePermissions)
      )
      val buffer = java.nio.ByteBuffer.wrap(bytes)
      var zeroWrites = 0
      while (buffer.hasRemaining && zeroWrites < 16) {
        val count = channel.write(buffer)
        if (count == 0) zeroWrites += 1 else zeroWrites = 0
      }
      if (buffer.hasRemaining) Left(failed)
      else {
        channel.force(true)
        readIdentity(path).filter(identity =>
          identity.regular && identity.size == bytes.length.toLong &&
            privateSingleLinkFile(path)
        ).toRight(failed)
      }
    } catch {
      case _: java.nio.file.FileAlreadyExistsException => Left(exists)
      case NonFatal(_)                                 => Left(failed)
    } finally if (channel != null) try channel.close() catch { case NonFatal(_) => () }
  }

  private def privateSafeDirectory(path: Path): Boolean =
    try {
      path.isAbsolute && path == path.normalize() && path.toRealPath() == path &&
      Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) &&
      !Files.isSymbolicLink(path) &&
      unixUid(path) == CurrentUid &&
      Files.getPosixFilePermissions(path) == DirectoryPermissions
    } catch { case NonFatal(_) => false }

  private def privateSingleLinkFile(path: Path): Boolean =
    try {
      val attributes = Files.readAttributes(
        path,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      val links = Files.getAttribute(path, "unix:nlink", LinkOption.NOFOLLOW_LINKS)
        .asInstanceOf[java.lang.Number].longValue()
      attributes.isRegularFile && !attributes.isSymbolicLink && links == 1L &&
        unixUid(path) == CurrentUid &&
        Files.getPosixFilePermissions(path) == FilePermissions
    } catch { case NonFatal(_) => false }

  private def forceDirectory(
      path: Path
  ): Either[PostgreSqlEnumerationLiveDataV1PublicationFailure, Unit] = {
    var channel: FileChannel = null
    try {
      channel = FileChannel.open(path, StandardOpenOption.READ)
      channel.force(true)
      Right(())
    } catch { case NonFatal(_) => Left(DurabilityFailed) }
    finally if (channel != null) try channel.close() catch { case NonFatal(_) => () }
  }

  private def unixUid(path: Path): Long =
    Files
      .getAttribute(path, "unix:uid", LinkOption.NOFOLLOW_LINKS)
      .asInstanceOf[java.lang.Number]
      .longValue()

  private def readIdentity(path: Path): Option[FileIdentity] =
    try {
      val attributes = Files.readAttributes(
        path,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      if (attributes.isSymbolicLink || attributes.fileKey() == null) None
      else Some(
        FileIdentity(
          attributes.fileKey(),
          attributes.isDirectory,
          attributes.isRegularFile,
          attributes.size(),
          attributes.lastModifiedTime()
        )
      )
    } catch { case NonFatal(_) => None }
}

private[cli] trait PostgreSqlEnumerationLiveDataV1Backend {
  /**
    * Production overrides this to bind the exact code-source archive before
    * opening JDBC. The default is solely an injection seam for controlled
    * test backends, which do not execute the PostgreSQL path.
    */
  def verifyExecutingAssembly(
      expected: PostgreSqlEnumerationLiveDataV1AssemblyBinding
  ): Either[PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure, Unit] = Right(())

  def capture(
      config: PostgreSqlEnumerationLiveDataV1CliConfig,
      request: PostgreSqlEnumerationLiveDataV1Request,
      copyOutDirectory: Path
  ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, PostgreSqlEnumerationLiveDataV1Batch]
}

private[cli] object LivePostgreSqlEnumerationLiveDataV1Backend
    extends PostgreSqlEnumerationLiveDataV1Backend {
  override def verifyExecutingAssembly(
      expected: PostgreSqlEnumerationLiveDataV1AssemblyBinding
  ): Either[PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure, Unit] =
    PostgreSqlEnumerationLiveDataV1ExecutingAssembly.verifyClasses(
      Vector(
        PostgreSqlEnumerationLiveDataV1Cli.getClass,
        PostgreSqlEnumerationLiveDataV1CliConfig.getClass,
        LivePostgreSqlEnumerationLiveDataV1Backend.getClass,
        PostgreSqlEnumerationLiveDataV1Publisher.getClass,
        PostgreSqlEnumerationLiveDataV1RequestParser.getClass,
        PostgreSqlEnumerationLiveDataV1RawQueryContract.getClass,
        PostgreSqlEnumerationLiveDataV1Assembly.getClass,
        PostgreSqlEnumerationLiveDataV1ExecutingAssembly.getClass,
        PostgreSqlEnumerationCampaignV2CanonicalJson.getClass,
        PostgreSqlRewriteCliConfig.getClass,
        PostgreSqlEnumerationLiveDataV1Capture.getClass,
        classOf[JsonFactory],
        classOf[ObjectMapper],
        classOf[PGConnection],
        classOf[CopyManager],
        classOf[PGSimpleDataSource]
      ),
      expected
    )

  override def capture(
      config: PostgreSqlEnumerationLiveDataV1CliConfig,
      request: PostgreSqlEnumerationLiveDataV1Request,
      copyOutDirectory: Path
  ): Either[PostgreSqlEnumerationLiveDataV1CaptureFailure, PostgreSqlEnumerationLiveDataV1Batch] = {
    require(config != null && request != null && copyOutDirectory != null)
    val dataSource = new PGSimpleDataSource()
    dataSource.setURL(config.jdbc.jdbcUrl)
    dataSource.setUser(config.jdbc.user)
    dataSource.setPassword(config.jdbc.password)
    PostgreSqlEnumerationLiveDataV1Capture
      .forDataSource(dataSource)
      .flatMap(_.capture(request, copyOutDirectory))
  }
}

private[cli] sealed trait PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure {
  case object InvalidArchive
      extends PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure {
    val code = "live-data-v1.executing-assembly-invalid"
  }
  case object BindingMismatch
      extends PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure {
    val code = "live-data-v1.executing-assembly-mismatch"
  }
}

/** Exact, bounded hashing of the archive supplying every live-path class. */
private[cli] object PostgreSqlEnumerationLiveDataV1ExecutingAssembly {
  import PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure._

  private val BufferBytes = 64 * 1024
  private val MaximumZeroReads = 16

  private final case class ArchiveIdentity(
      key: AnyRef,
      size: Long,
      modified: FileTime,
      links: Long
  ) {
    def sameFile(other: ArchiveIdentity): Boolean =
      other != null && key == other.key && size == other.size &&
        modified == other.modified && links == other.links
  }

  def verifyClasses(
      classes: Vector[Class[_]],
      expected: PostgreSqlEnumerationLiveDataV1AssemblyBinding
  ): Either[PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure, Unit] = {
    if (classes == null || classes.isEmpty || expected == null)
      return Left(InvalidArchive)
    val paths = Vector.newBuilder[Path]
    classes.foreach { runtimeClass =>
      paths += codeSourcePath(runtimeClass).getOrElse(return Left(InvalidArchive))
    }
    val resolved = paths.result()
    if (resolved.distinct.size != 1 || !onlyClassPathEntry(resolved.head)) Left(InvalidArchive)
    else verifyPath(resolved.head, expected)
  }

  private def onlyClassPathEntry(archive: Path): Boolean =
    try {
      val raw = Option(System.getProperty("java.class.path")).getOrElse(return false)
      val entries = raw.split(java.util.regex.Pattern.quote(File.pathSeparator), -1).toVector
      if (entries.size != 1 || entries.head.isEmpty) false
      else {
        val spelling = Paths.get(entries.head)
        val absolute =
          if (spelling.isAbsolute) spelling.normalize()
          else Paths.get("").toAbsolutePath.normalize().resolve(spelling).normalize()
        direct(absolute) && absolute.toRealPath() == archive.toRealPath()
      }
    } catch { case NonFatal(_) => false }

  private[cli] def verifyPath(
      path: Path,
      expected: PostgreSqlEnumerationLiveDataV1AssemblyBinding
  ): Either[PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure, Unit] = {
    if (path == null || expected == null || !direct(path)) return Left(InvalidArchive)
    val before = identity(path).getOrElse(return Left(InvalidArchive))
    if (
      before.size <= 0L ||
      before.size > PostgreSqlEnumerationLiveDataV1RequestParser.MaximumScalaAssemblyBytes ||
      before.links != 1L
    ) return Left(InvalidArchive)

    var channel: FileChannel = null
    try {
      channel = FileChannel.open(path, StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)
      val digest = MessageDigest.getInstance("SHA-256")
      val buffer = ByteBuffer.allocate(BufferBytes)
      var total = 0L
      var zeroReads = 0
      var read = channel.read(buffer)
      while (read >= 0 && zeroReads < MaximumZeroReads) {
        if (read == 0) zeroReads += 1
        else {
          zeroReads = 0
          total = Math.addExact(total, read.toLong)
          if (total > PostgreSqlEnumerationLiveDataV1RequestParser.MaximumScalaAssemblyBytes)
            return Left(InvalidArchive)
          buffer.flip()
          digest.update(buffer)
          buffer.clear()
        }
        read = channel.read(buffer)
      }
      val after = identity(path).getOrElse(return Left(InvalidArchive))
      if (
        read >= 0 || total != before.size || !before.sameFile(after) || !direct(path)
      ) return Left(InvalidArchive)
      val actualSha256 = digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
      if (total == expected.byteLength && actualSha256 == expected.sha256) Right(())
      else Left(BindingMismatch)
    } catch { case NonFatal(_) => Left(InvalidArchive) }
    finally if (channel != null) try channel.close() catch { case NonFatal(_) => () }
  }

  private def codeSourcePath(runtimeClass: Class[_]): Option[Path] =
    try {
      if (runtimeClass == null) return None
      val domain = Option(runtimeClass.getProtectionDomain).getOrElse(return None)
      val source = Option(domain.getCodeSource).getOrElse(return None)
      val location = Option(source.getLocation).getOrElse(return None)
      val uri = location.toURI
      if (
        uri.getScheme != "file" || Option(uri.getAuthority).exists(_.nonEmpty) ||
        uri.getQuery != null || uri.getFragment != null
      ) None
      else {
        val path = Paths.get(uri)
        if (direct(path)) Some(path) else None
      }
    } catch { case NonFatal(_) => None }

  private def direct(path: Path): Boolean =
    try {
      path.isAbsolute && path == path.normalize() &&
      path.toRealPath() == path && !Files.isSymbolicLink(path) &&
      Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)
    } catch { case NonFatal(_) => false }

  private def identity(path: Path): Option[ArchiveIdentity] =
    try {
      val attributes = Files.readAttributes(
        path,
        classOf[BasicFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
      val links = Files.getAttribute(path, "unix:nlink", LinkOption.NOFOLLOW_LINKS)
        .asInstanceOf[java.lang.Number].longValue()
      if (
        attributes.fileKey() == null || attributes.isSymbolicLink ||
        !attributes.isRegularFile
      ) None
      else Some(ArchiveIdentity(
        attributes.fileKey(),
        attributes.size(),
        attributes.lastModifiedTime(),
        links
      ))
    } catch { case NonFatal(_) => None }
}

private[cli] final case class PostgreSqlEnumerationLiveDataV1CliFailure(
    code: String,
    exitCode: Int
) {
  require(code != null && code.nonEmpty && exitCode != 0)
}

/** Silent one-shot exact load plus raw-capture publisher; no final receipt. */
object PostgreSqlEnumerationLiveDataV1Cli {
  private val FailurePrefix = "enumeration-live-data-v1 error: "

  def main(arguments: Array[String]): Unit = {
    val status = run(
      arguments,
      sys.env,
      Paths.get("").toAbsolutePath.normalize(),
      System.err,
      LivePostgreSqlEnumerationLiveDataV1Backend
    )
    if (status != 0) System.exit(status)
  }

  private[cli] def run(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path,
      standardError: OutputStream,
      backend: PostgreSqlEnumerationLiveDataV1Backend
  ): Int = {
    val result =
      try execute(arguments, environment, workingDirectory, backend)
      catch { case NonFatal(_) => Left(internal) }
    result match {
      case Right(()) => 0
      case Left(problem) =>
        writeFailure(standardError, problem)
        problem.exitCode
    }
  }

  private def execute(
      arguments: Array[String],
      environment: Map[String, String],
      workingDirectory: Path,
      backend: PostgreSqlEnumerationLiveDataV1Backend
  ): Either[PostgreSqlEnumerationLiveDataV1CliFailure, Unit] = {
    if (backend == null) return Left(internal)
    for {
      config <- PostgreSqlEnumerationLiveDataV1CliConfig
        .parse(arguments, environment, workingDirectory)
        .left.map(problem => failure(problem.code, PostgreSqlRewriteExitCode.InvalidInvocation))
      request <- PostgreSqlEnumerationLiveDataV1RequestParser
        .load(config.requestBundle)
        .left.map(problem => failure(problem.code, PostgreSqlRewriteExitCode.InputError))
      _ <- backend.verifyExecutingAssembly(request.assemblyBinding)
        .left.map(executingAssemblyFailure)
      _ <- PostgreSqlEnumerationLiveDataV1Publisher.withMappedWorkspace(
        config.outputAssembly,
        publicationFailure,
        internal
      ) {
        workspace =>
          for {
            batch <- backend.capture(config, request.captureRequest, workspace.copyOutDirectory)
              .left.map(problem => failure(problem.code, PostgreSqlRewriteExitCode.ServiceUnavailable))
            _ <-
              if (PostgreSqlEnumerationLiveDataV1RequestParser.unchanged(request)) Right(())
              else Left(failure(
                PostgreSqlEnumerationLiveDataV1ProtocolFailure.RequestChanged.code,
                PostgreSqlRewriteExitCode.InputError
              ))
            documents <- PostgreSqlEnumerationLiveDataV1Assembly.render(request, batch)
              .left.map(problem => failure(problem.code, PostgreSqlRewriteExitCode.RewriteFailed))
            _ <- workspace.writeAssembly(documents.assembly.bytes).left.map(publicationFailure)
            _ <- workspace.writeComplete(documents.complete.bytes, documents.tableNames)
              .left.map(publicationFailure)
            _ <- workspace.publish().left.map(publicationFailure)
          } yield ()
      }
    } yield ()
  }

  private def publicationFailure(
      problem: PostgreSqlEnumerationLiveDataV1PublicationFailure
  ): PostgreSqlEnumerationLiveDataV1CliFailure = {
    import PostgreSqlEnumerationLiveDataV1PublicationFailure._
    val exit = problem match {
      case InvalidOutput | OutputExists | EnvelopeMismatch =>
        PostgreSqlRewriteExitCode.InputError
      case _ => PostgreSqlRewriteExitCode.ServiceUnavailable
    }
    failure(problem.code, exit)
  }

  private def executingAssemblyFailure(
      problem: PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure
  ): PostgreSqlEnumerationLiveDataV1CliFailure = {
    val exit = problem match {
      case PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure.BindingMismatch =>
        PostgreSqlRewriteExitCode.InputError
      case PostgreSqlEnumerationLiveDataV1ExecutingAssemblyFailure.InvalidArchive =>
        PostgreSqlRewriteExitCode.ServiceUnavailable
    }
    failure(problem.code, exit)
  }

  private def failure(code: String, exitCode: Int) =
    PostgreSqlEnumerationLiveDataV1CliFailure(code, exitCode)
  private val internal = failure(
    "live-data-v1-cli.internal-error",
    PostgreSqlRewriteExitCode.InternalError
  )

  private def writeFailure(
      output: OutputStream,
      problem: PostgreSqlEnumerationLiveDataV1CliFailure
  ): Unit = {
    if (output == null) return
    try {
      output.write((FailurePrefix + problem.code + "\n").getBytes(StandardCharsets.UTF_8))
      output.flush()
    } catch { case NonFatal(_) => () }
  }
}
