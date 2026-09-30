package at.ac.tuwien.dbai.yannakakis.cli

import java.io.{ByteArrayInputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, InvalidPathException, LinkOption, Path, Paths, StandardOpenOption}
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPhysicalConfiguration
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCteMaterializationDirective,
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationPhysicalFactorialV3,
  PostgreSqlQueryRewriter
}

import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * Development-only SQL exporter for the same-candidate physical factorial.
  *
  * This boundary performs catalog-backed rewrite generation only. It never
  * executes or explains the source/native/action statements, records no
  * timing, and publishes no credentials or connection identifiers.
  */
object PostgreSqlPhysicalSqlExportCli {
  private val Prefix = "postgresql-physical-sql-export"

  def main(arguments: Array[String]): Unit = {
    val exitCode = run(arguments, sys.env, System.err, LiveBackend)
    if (exitCode != PostgreSqlRewriteExitCode.Rewritten) System.exit(exitCode)
  }

  private[cli] def run(
      arguments: Array[String],
      environment: Map[String, String],
      standardError: OutputStream,
      backend: PostgreSqlPhysicalSqlExportBackend
  ): Int = {
    require(standardError != null, "standard error must not be null")
    val result =
      try execute(arguments, environment, backend)
      catch { case NonFatal(_) => Left(Failure.Internal) }
    result match {
      case Right(_) => PostgreSqlRewriteExitCode.Rewritten
      case Left(problem) =>
        val line = s"$Prefix error: ${problem.code}\n"
        standardError.write(line.getBytes(StandardCharsets.UTF_8))
        standardError.flush()
        problem.exitCode
    }
  }

  private def execute(
      arguments: Array[String],
      environment: Map[String, String],
      backend: PostgreSqlPhysicalSqlExportBackend
  ): Either[Failure, Unit] =
    for {
      config <- Config.parse(arguments, environment)
      _ <- ensureOutputAbsent(config.outputDirectory)
      sql <- readSql(config.sqlFile)
      generated <- backend.generate(config, sql)
      _ <- validateGenerated(config, generated)
      _ <- Publisher.publish(config, sql, generated)
    } yield ()

  private def ensureOutputAbsent(path: Path): Either[Failure, Unit] =
    if (Files.exists(path, LinkOption.NOFOLLOW_LINKS)) Left(Failure.OutputExists)
    else Right(())

  private def readSql(path: Path): Either[Failure, String] =
    StrictUtf8SqlReader
      .read(
        SqlSource.File(path),
        new ByteArrayInputStream(Array.emptyByteArray),
        PostgreSqlEnumerationBatchMaximumSqlBytes
      )
      .left
      .map(_ => Failure.Input)

  private val PostgreSqlEnumerationBatchMaximumSqlBytes = 16 * 1024 * 1024

  private def validateGenerated(
      config: Config,
      generated: Generated
  ): Either[Failure, Unit] = {
    val expected = for {
      reduction <- Vector(
        YannakakisReductionMode.FullTwoPass,
        YannakakisReductionMode.BottomUpOnly,
        YannakakisReductionMode.NoReduction
      )
      directive <- PostgreSqlCteMaterializationDirective.Values
    } yield reduction -> directive
    val observed = generated.actions.map(action => action.reductionMode -> action.cteDirective)
    if (generated.rank != config.rank) Left(Failure.BackendContract)
    else if (
      generated.rendererVersion != RendererVersion ||
      generated.profileId != RendererProfile
    ) Left(Failure.BackendContract)
    else if (
      !CanonicalSha256.matches(generated.candidateId) ||
      !CanonicalSha256.matches(generated.decompositionId) ||
      generated.candidateId != generated.decompositionId ||
      generated.configuration == null
    ) Left(Failure.BackendContract)
    else if (observed != expected) Left(Failure.BackendContract)
    else if (
      generated.actions.zipWithIndex.exists { case (action, index) =>
        action.ordinal != index + 1 || action.sql == null || action.sql.isEmpty ||
        action.sql.indexOf('\u0000') >= 0 ||
        !CanonicalSha256.matches(action.physicalVariantId)
      }
    ) Left(Failure.BackendContract)
    else if (generated.actions.map(_.physicalVariantId).distinct.size != 9)
      Left(Failure.BackendContract)
    else Right(())
  }

  private[cli] val SchemaVersion = "postgresql-physical-sql-export/v1"
  private[cli] val RendererVersion = "current-v3"
  private[cli] val RendererProfile = "statistics-safe-v2"
  private val CanonicalSha256 = "sha256:[0-9a-f]{64}".r

  private[cli] final case class Config(
      jdbcUrl: String,
      user: String,
      password: String,
      schema: String,
      sqlFile: Path,
      width: Int,
      requireConnectedCover: Boolean,
      planLimit: Int,
      rank: Int,
      outputDirectory: Path
  ) {
    require(jdbcUrl != null && jdbcUrl.trim.nonEmpty)
    require(user != null && user.trim.nonEmpty)
    require(password != null)
    require(schema != null && schema.trim.nonEmpty)
    require(sqlFile != null && outputDirectory != null)
    require(width > 0 && planLimit > 0 && planLimit <= 10)
    require(rank > 0 && rank <= planLimit)
  }

  private[cli] object Config {
    private val Required = Vector(
      "--jdbc-url",
      "--user",
      "--password-env",
      "--schema",
      "--sql-file",
      "--width",
      "--require-connected-cover",
      "--plan-limit",
      "--rank",
      "--output-dir"
    )
    private val EnvironmentName = "[A-Za-z_][A-Za-z0-9_]*".r

    def parse(
        arguments: Array[String],
        environment: Map[String, String]
    ): Either[Failure, Config] = {
      if (arguments == null || environment == null) return Left(Failure.Invocation)
      val values = mutable.LinkedHashMap.empty[String, String]
      var index = 0
      while (index < arguments.length) {
        val option = arguments(index)
        if (
          option == null || !Required.contains(option) || values.contains(option) ||
          index + 1 >= arguments.length || arguments(index + 1) == null ||
          arguments(index + 1).startsWith("--")
        ) return Left(Failure.Invocation)
        values += option -> arguments(index + 1)
        index += 2
      }
      if (values.keySet.toSet != Required.toSet) return Left(Failure.Invocation)

      def nonBlank(option: String): Either[Failure, String] = {
        val value = values(option)
        if (value.trim.nonEmpty) Right(value) else Left(Failure.Invocation)
      }
      def positiveInteger(option: String): Either[Failure, Int] =
        try {
          val value = values(option).toInt
          if (value > 0) Right(value) else Left(Failure.Invocation)
        } catch { case _: NumberFormatException => Left(Failure.Invocation) }
      def path(option: String): Either[Failure, Path] =
        try {
          val value = values(option)
          if (value.trim.isEmpty) Left(Failure.Invocation)
          else Right(Paths.get(value).toAbsolutePath.normalize())
        } catch { case _: InvalidPathException => Left(Failure.Invocation) }

      for {
        jdbcUrl <- nonBlank("--jdbc-url")
        user <- nonBlank("--user")
        passwordEnvironment <- nonBlank("--password-env")
        _ <-
          if (EnvironmentName.pattern.matcher(passwordEnvironment).matches()) Right(())
          else Left(Failure.Invocation)
        password <- environment.get(passwordEnvironment).toRight(Failure.MissingPassword)
        schema <- nonBlank("--schema")
        sqlFile <- path("--sql-file")
        width <- positiveInteger("--width")
        connectedCover <- values("--require-connected-cover") match {
          case "true"  => Right(true)
          case "false" => Right(false)
          case _       => Left(Failure.Invocation)
        }
        planLimit <- positiveInteger("--plan-limit")
        _ <- if (planLimit <= 10) Right(()) else Left(Failure.Invocation)
        rank <- positiveInteger("--rank")
        _ <- if (rank <= planLimit) Right(()) else Left(Failure.Invocation)
        output <- path("--output-dir")
        _ <- if (sqlFile != output) Right(()) else Left(Failure.Invocation)
      } yield Config(
        jdbcUrl,
        user,
        password,
        schema,
        sqlFile,
        width,
        connectedCover,
        planLimit,
        rank,
        output
      )
    }
  }

  private[cli] final case class Action(
      ordinal: Int,
      physicalVariantId: String,
      reductionMode: YannakakisReductionMode,
      cteDirective: PostgreSqlCteMaterializationDirective,
      sql: String
  )

  private[cli] final case class Generated(
      rendererVersion: String,
      profileId: String,
      rank: Int,
      candidateId: String,
      decompositionId: String,
      configuration: BagFactorPhysicalConfiguration,
      actions: Vector[Action]
  )

  private[cli] sealed trait Failure {
    def code: String
    def exitCode: Int
  }

  private[cli] object Failure {
    case object Invocation extends Failure {
      val code = "invalid-invocation"
      val exitCode = PostgreSqlRewriteExitCode.InvalidInvocation
    }
    case object MissingPassword extends Failure {
      val code = "password-environment-unavailable"
      val exitCode = PostgreSqlRewriteExitCode.InvalidInvocation
    }
    case object Input extends Failure {
      val code = "sql-input-error"
      val exitCode = PostgreSqlRewriteExitCode.InputError
    }
    case object OutputExists extends Failure {
      val code = "output-exists"
      val exitCode = PostgreSqlRewriteExitCode.InputError
    }
    case object BackendUnavailable extends Failure {
      val code = "generation-unavailable"
      val exitCode = PostgreSqlRewriteExitCode.ServiceUnavailable
    }
    case object GenerationFailed extends Failure {
      val code = "generation-failed"
      val exitCode = PostgreSqlRewriteExitCode.RewriteFailed
    }
    case object BackendContract extends Failure {
      val code = "generation-contract-mismatch"
      val exitCode = PostgreSqlRewriteExitCode.RewriteFailed
    }
    case object PublicationFailed extends Failure {
      val code = "publication-incomplete"
      val exitCode = PostgreSqlRewriteExitCode.InputError
    }
    case object Internal extends Failure {
      val code = "internal-error"
      val exitCode = PostgreSqlRewriteExitCode.InternalError
    }
  }

  private object Publisher {
    private val DirectoryPermissions = PosixFilePermissions.fromString("rwx------")
    private val FilePermissions = PosixFilePermissions.fromString("rw-------")
    private val DirectoryAttribute =
      PosixFilePermissions.asFileAttribute(DirectoryPermissions)
    private val FileAttribute = PosixFilePermissions.asFileAttribute(FilePermissions)

    private final case class FileReference(path: String, bytes: Long, sha256: String)
    private final case class Payload(id: String, sql: String, reference: FileReference)

    def publish(
        config: Config,
        sourceSql: String,
        generated: Generated
    ): Either[Failure, Unit] = {
      val sourceBytes = sourceSql.getBytes(StandardCharsets.UTF_8)
      val sourceReference = reference("source.sql", sourceBytes)
      val nativeReference = reference("native.sql", sourceBytes)
      val payloads = buildPayloads(generated.actions)
      val actions = generated.actions.map { action =>
        val payload = payloads.find(_.sql == action.sql).get
        action -> payload
      }
      val manifest = manifestBytes(
        config,
        generated,
        sourceReference,
        nativeReference,
        payloads,
        actions
      )

      try {
        val parent = config.outputDirectory.getParent
        if (
          parent == null || !Files.isDirectory(parent, LinkOption.NOFOLLOW_LINKS) ||
          Files.isSymbolicLink(parent)
        ) return Left(Failure.PublicationFailed)
        Files.createDirectory(config.outputDirectory, DirectoryAttribute)
        verifyDirectory(config.outputDirectory)
        val actionsDirectory = config.outputDirectory.resolve("actions")
        Files.createDirectory(actionsDirectory, DirectoryAttribute)
        verifyDirectory(actionsDirectory)
        write(config.outputDirectory.resolve("source.sql"), sourceBytes)
        write(config.outputDirectory.resolve("native.sql"), sourceBytes)
        payloads.foreach(payload =>
          write(
            config.outputDirectory.resolve(payload.reference.path),
            payload.sql.getBytes(StandardCharsets.UTF_8)
          )
        )
        write(config.outputDirectory.resolve("manifest.json"), manifest)
        Right(())
      } catch {
        case NonFatal(_) => Left(Failure.PublicationFailed)
      }
    }

    private def buildPayloads(actions: Vector[Action]): Vector[Payload] = {
      val bySql = mutable.LinkedHashMap.empty[String, Payload]
      actions.foreach { action =>
        if (!bySql.contains(action.sql)) {
          val bytes = action.sql.getBytes(StandardCharsets.UTF_8)
          val digest = sha256(bytes)
          val id = s"payload-$digest"
          val path = s"actions/$id.sql"
          bySql += action.sql -> Payload(id, action.sql, reference(path, bytes))
        }
      }
      bySql.values.toVector.sortBy(_.id)
    }

    private def manifestBytes(
        config: Config,
        generated: Generated,
        source: FileReference,
        native: FileReference,
        payloads: Vector[Payload],
        actions: Vector[(Action, Payload)]
    ): Array[Byte] = {
      import PostgreSqlEnumerationCampaignV2CanonicalJson._
      def file(value: FileReference): Value = obj(
        "bytes" -> number(value.bytes),
        "path" -> string(value.path),
        "sha256" -> string(value.sha256)
      )
      val configuration = configurationValue(generated.configuration)
      val configurationId = "sha256:" + sha256(document(configuration).bytes)
      val root = obj(
        "actions" -> array(actions.map { case (action, payload) =>
          obj(
            "action_id" -> string(action.physicalVariantId),
            "alias_id" -> string(s"${action.reductionMode.id}:${action.cteDirective.id}"),
            "cte_directive" -> string(action.cteDirective.id),
            "payload_id" -> string(payload.id),
            "physical_variant_id" -> string(action.physicalVariantId),
            "reduction_mode" -> string(action.reductionMode.id),
            "sql" -> file(payload.reference)
          )
        }),
        "candidate" -> obj(
          "candidate_id" -> string(generated.candidateId),
          "configuration_id" -> string(configurationId),
          "decomposition_id" -> string(generated.decompositionId),
          "rank" -> number(generated.rank)
        ),
        "configuration" -> configuration,
        "contains_database_results" -> bool(false),
        "contains_execution_outcomes" -> bool(false),
        "contains_timing" -> bool(false),
        "files" -> obj(
          "native_sql" -> file(native),
          "payloads" -> array(payloads.map(payload =>
            obj(
              "bytes" -> number(payload.reference.bytes),
              "path" -> string(payload.reference.path),
              "payload_id" -> string(payload.id),
              "sha256" -> string(payload.reference.sha256)
            )
          )),
          "source_sql" -> file(source)
        ),
        "input" -> obj(
          "plan_limit" -> number(config.planLimit),
          "requested_rank" -> number(config.rank),
          "require_connected_cover" -> bool(config.requireConnectedCover),
          "schema" -> string(config.schema),
          "width" -> number(config.width)
        ),
        "renderer" -> obj(
          "profile" -> string(generated.profileId),
          "version" -> string(generated.rendererVersion)
        ),
        "schema_version" -> string(SchemaVersion),
        "status" -> string("development-only-nonreportable")
      )
      document(root).bytes
    }

    private def configurationValue(
        configuration: BagFactorPhysicalConfiguration
    ): PostgreSqlEnumerationCampaignV2CanonicalJson.Value = {
      import PostgreSqlEnumerationCampaignV2CanonicalJson._
      obj(
        "cover_assignments" -> array(
          configuration.coverAssignments.map(assignment =>
            obj(
              "bag_id" -> number(assignment.bagId.value),
              "edge_ids" -> array(
                assignment.edgeIds.map(edge => number(edge.value))
              )
            )
          )
        ),
        "execution_root" -> number(configuration.executionRoot.value)
      )
    }

    private def reference(path: String, bytes: Array[Byte]): FileReference =
      FileReference(path, bytes.length.toLong, sha256(bytes))

    private def sha256(bytes: Array[Byte]): String =
      MessageDigest
        .getInstance("SHA-256")
        .digest(bytes)
        .map(byte => f"${byte & 0xff}%02x")
        .mkString

    private def write(path: Path, bytes: Array[Byte]): Unit = {
      Files.createFile(path, FileAttribute)
      val channel = FileChannel.open(path, StandardOpenOption.WRITE)
      try {
        val buffer = ByteBuffer.wrap(bytes)
        while (buffer.hasRemaining) channel.write(buffer)
        channel.force(true)
      } finally channel.close()
      if (
        Files.getPosixFilePermissions(path, LinkOption.NOFOLLOW_LINKS) !=
          FilePermissions
      ) throw new IllegalStateException("published file permissions changed")
    }

    private def verifyDirectory(path: Path): Unit =
      if (
        !Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) ||
        Files.getPosixFilePermissions(path, LinkOption.NOFOLLOW_LINKS) !=
          DirectoryPermissions
      ) throw new IllegalStateException("published directory is not private")
  }

  private object LiveBackend extends PostgreSqlPhysicalSqlExportBackend {
    override def generate(
        config: Config,
        sql: String
    ): Either[Failure, Generated] = {
      try Class.forName(LivePostgreSqlConnectivityProbe.PostgreSqlDriverClass)
      catch { case NonFatal(_) => return Left(Failure.BackendUnavailable) }

      val rewriter = PostgreSqlQueryRewriter
        .forPostgreSqlJdbc(
          jdbcUrl = config.jdbcUrl,
          user = config.user,
          password = config.password,
          schemaName = config.schema
        )
        .fold(_ => return Left(Failure.BackendUnavailable), identity)
      val request = PostgreSqlEnumerationBatchRequest
        .create(
          originalSql = sql,
          widthBound = config.width,
          planLimit = config.planLimit,
          requireConnectedCover = config.requireConnectedCover
        )
        .fold(_ => return Left(Failure.GenerationFailed), identity)
      val factorial = PostgreSqlEnumerationPhysicalFactorialV3
        .produce(rewriter.produceEnumerationBatch(request), config.rank)
        .fold(_ => return Left(Failure.GenerationFailed), identity)
      if (
        factorial.protocolId !=
          PostgreSqlEnumerationPhysicalFactorialV3.ProtocolId ||
        factorial.renderingProfileId != RendererProfile
      ) return Left(Failure.BackendContract)

      Right(
        Generated(
          rendererVersion = RendererVersion,
          profileId = factorial.renderingProfileId,
          rank = factorial.structuralRank,
          candidateId = factorial.candidateId.value,
          decompositionId = factorial.decompositionId.value,
          configuration = factorial.configuration,
          actions = factorial.actions.map(action =>
            Action(
              ordinal = action.ordinal,
              physicalVariantId = action.physicalVariantId.value,
              reductionMode = action.reductionMode,
              cteDirective = action.cteMaterializationDirective,
              sql = action.sql
            )
          )
        )
      )
    }
  }
}

private[cli] trait PostgreSqlPhysicalSqlExportBackend {
  def generate(
      config: PostgreSqlPhysicalSqlExportCli.Config,
      sql: String
  ): Either[PostgreSqlPhysicalSqlExportCli.Failure, PostgreSqlPhysicalSqlExportCli.Generated]
}
