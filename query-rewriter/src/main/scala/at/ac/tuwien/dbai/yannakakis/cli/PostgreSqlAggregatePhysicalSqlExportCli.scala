package at.ac.tuwien.dbai.yannakakis.cli

import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{
  Files,
  InvalidPathException,
  LinkOption,
  Path,
  Paths,
  StandardOpenOption
}
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPhysicalConfiguration
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlAggregatePhysicalSweepV3,
  PostgreSqlAggregatePhysicalSweepV3Family,
  PostgreSqlCteMaterializationDirective,
  PostgreSqlQueryRewriter
}

import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * Development-only JDBC exporter for the closed aggregate physical sweep.
  * It plans and renders SQL but never executes, explains, or times a query.
  */
object PostgreSqlAggregatePhysicalSqlExportCli {
  private val Prefix = "postgresql-aggregate-physical-sql-export"
  private val CanonicalSha256 = "sha256:[0-9a-f]{64}".r

  private[cli] val SchemaVersion =
    "postgresql-aggregate-physical-sql-export/v1"
  private[cli] val Width3SchemaVersion =
    "postgresql-aggregate-physical-sql-export/v2"
  private[cli] val Width3FrequencySchemaVersion =
    "postgresql-aggregate-physical-sql-export/v3"
  private[cli] val RendererVersion = "current-v3"

  def main(arguments: Array[String]): Unit = {
    val status = run(arguments, sys.env, System.err, LiveBackend)
    if (status != PostgreSqlRewriteExitCode.Rewritten) System.exit(status)
  }

  private[cli] def run(
      arguments: Array[String],
      environment: Map[String, String],
      standardError: OutputStream,
      backend: PostgreSqlAggregatePhysicalSqlExportBackend
  ): Int = {
    require(standardError != null, "standard error must not be null")
    val result =
      try execute(arguments, environment, backend)
      catch { case NonFatal(_) => Left(Failure.Internal) }
    result match {
      case Right(_) => PostgreSqlRewriteExitCode.Rewritten
      case Left(problem) =>
        standardError.write(
          s"$Prefix error: ${problem.code}\n".getBytes(StandardCharsets.UTF_8)
        )
        standardError.flush()
        problem.exitCode
    }
  }

  private def execute(
      arguments: Array[String],
      environment: Map[String, String],
      backend: PostgreSqlAggregatePhysicalSqlExportBackend
  ): Either[Failure, Unit] =
    for {
      config <- Config.parse(arguments, environment)
      _ <-
        if (Files.exists(config.outputDirectory, LinkOption.NOFOLLOW_LINKS))
          Left(Failure.OutputExists)
        else Right(())
      source <- readSource(config.sqlFile)
      generated <- backend.generate(config, source.planning.text)
      _ <- validateGenerated(config, generated)
      _ <- Publisher.publish(config, source, generated)
    } yield ()

  private def readSource(
      path: Path
  ): Either[Failure, PostgreSqlGeneratedSqlOptimizationSource] =
    try
      PostgreSqlGeneratedSqlOptimizationSource
        .fromBytes(Files.readAllBytes(path))
        .left
        .map(_ => Failure.Input)
    catch { case NonFatal(_) => Left(Failure.Input) }

  private def validateGenerated(
      config: Config,
      generated: Generated
  ): Either[Failure, Unit] = {
    val expected = for {
      rank <- config.family.requiredRanks
      form <- expectedForms(config)
    } yield (rank, form._1, form._2)
    val observed = generated.actions.map(action =>
      (action.structuralRank, action.reductionMode, action.cteDirective)
    )
    val expectedAliases = expected.map { case (rank, mode, directive) =>
      expectedAlias(config.family, rank, mode, directive)
    }
    if (
      generated.family != config.family ||
      generated.protocolId != PostgreSqlAggregatePhysicalSweepV3.ProtocolId ||
      generated.aggregateProfileId !=
        PostgreSqlAggregatePhysicalSweepV3.AggregateProfileId ||
      generated.enumerationProfileId !=
        PostgreSqlAggregatePhysicalSweepV3.EnumerationProfileId ||
      observed != expected ||
      generated.actions.map(_.aliasId) != expectedAliases ||
      generated.actions.size != expected.size
    ) Left(Failure.BackendContract)
    else if (
      generated.actions.zipWithIndex.exists { case (action, index) =>
        action.ordinal != index + 1 ||
        !action.aliasId.matches("[a-z][a-z0-9_-]{0,127}") ||
        !CanonicalSha256.matches(action.candidateId) ||
        !CanonicalSha256.matches(action.physicalVariantId) ||
        action.configuration == null || action.sql == null ||
        action.sql.isEmpty || action.sql.indexOf('\u0000') >= 0 ||
        action.sql.getBytes(StandardCharsets.UTF_8).length >
          PostgreSqlGeneratedSqlOptimizationSource.MaximumGeneratedSqlBytes
      }
    ) Left(Failure.BackendContract)
    else if (
      generated.actions.map(_.aliasId).distinct.size != expected.size ||
      generated.actions.map(_.physicalVariantId).distinct.size != expected.size
    ) Left(Failure.BackendContract)
    else Right(())
  }

  private def expectedForms(
      config: Config
  ): Vector[
    (YannakakisReductionMode, PostgreSqlCteMaterializationDirective)
  ] =
    if (isWidth3Frequency(config))
      Vector(
        YannakakisReductionMode.FullTwoPass ->
          PostgreSqlCteMaterializationDirective.PlannerControlled
      )
    else
      config.family match {
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

  private def isFrequencyFamily(
      family: PostgreSqlAggregatePhysicalSweepV3Family
  ): Boolean = family match {
    case PostgreSqlAggregatePhysicalSweepV3Family.GlobalCount |
        PostgreSqlAggregatePhysicalSweepV3Family.DistributedGroupedCount => true
    case _ => false
  }

  private def isWidth3Frequency(config: Config): Boolean =
    config.widthBound == 3 && isFrequencyFamily(config.family)

  private def expectedAlias(
      family: PostgreSqlAggregatePhysicalSweepV3Family,
      rank: Int,
      mode: YannakakisReductionMode,
      directive: PostgreSqlCteMaterializationDirective
  ): String = {
    val modeId = mode match {
      case YannakakisReductionMode.FullTwoPass => "full"
      case YannakakisReductionMode.BottomUpOnly => "bu"
      case YannakakisReductionMode.NoReduction => "none"
    }
    val directiveId = directive match {
      case PostgreSqlCteMaterializationDirective.PlannerControlled => "pc"
      case PostgreSqlCteMaterializationDirective.Materialized => "mat"
      case PostgreSqlCteMaterializationDirective.NotMaterialized => "not-mat"
    }
    f"${family.id}-r$rank%02d-$modeId-$directiveId"
  }

  private[cli] final case class Config(
      jdbcUrl: String,
      user: String,
      password: String,
      schema: String,
      widthBound: Int,
      family: PostgreSqlAggregatePhysicalSweepV3Family,
      sqlFile: Path,
      outputDirectory: Path
  )

  private[cli] object Config {
    private val Required = Vector(
      "--jdbc-url",
      "--user",
      "--password-env",
      "--schema",
      "--family",
      "--sql-file",
      "--output-dir"
    )
    private val Optional = Vector("--width-bound")
    private val Known = Required ++ Optional
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
          option == null || !Known.contains(option) || values.contains(option) ||
          index + 1 >= arguments.length || arguments(index + 1) == null ||
          arguments(index + 1).startsWith("--")
        ) return Left(Failure.Invocation)
        values += option -> arguments(index + 1)
        index += 2
      }
      if (!Required.forall(values.contains)) return Left(Failure.Invocation)

      def nonBlank(option: String): Either[Failure, String] = {
        val value = values(option)
        if (value.trim.nonEmpty) Right(value) else Left(Failure.Invocation)
      }
      def path(option: String): Either[Failure, Path] =
        try {
          val value = values(option)
          if (value.trim.isEmpty) Left(Failure.Invocation)
          else Right(Paths.get(value).toAbsolutePath.normalize())
        } catch { case _: InvalidPathException => Left(Failure.Invocation) }

      for {
        jdbcUrl <- nonBlank("--jdbc-url")
        user <- nonBlank("--user")
        passwordName <- nonBlank("--password-env")
        _ <-
          if (EnvironmentName.pattern.matcher(passwordName).matches()) Right(())
          else Left(Failure.Invocation)
        password <- environment.get(passwordName).toRight(Failure.MissingPassword)
        schema <- nonBlank("--schema")
        widthBound <-
          values.get("--width-bound") match {
            case None => Right(2)
            case Some("2") => Right(2)
            case Some("3") => Right(3)
            case _ => Left(Failure.Invocation)
          }
        familyName <- nonBlank("--family")
        family <- PostgreSqlAggregatePhysicalSweepV3Family
          .parse(familyName)
          .toRight(Failure.Invocation)
        _ <-
          if (
            widthBound == 2 ||
            family == PostgreSqlAggregatePhysicalSweepV3Family.GuardedMin ||
            isFrequencyFamily(family)
          ) Right(())
          else Left(Failure.Invocation)
        sqlFile <- path("--sql-file")
        output <- path("--output-dir")
        _ <- if (sqlFile != output) Right(()) else Left(Failure.Invocation)
      } yield Config(
        jdbcUrl,
        user,
        password,
        schema,
        widthBound,
        family,
        sqlFile,
        output
      )
    }
  }

  private[cli] final case class Action(
      ordinal: Int,
      aliasId: String,
      structuralRank: Int,
      candidateId: String,
      physicalVariantId: String,
      configuration: BagFactorPhysicalConfiguration,
      reductionMode: YannakakisReductionMode,
      cteDirective: PostgreSqlCteMaterializationDirective,
      sql: String
  )

  private[cli] final case class Generated(
      protocolId: String,
      family: PostgreSqlAggregatePhysicalSweepV3Family,
      resultShapeId: String,
      aggregateProfileId: String,
      enumerationProfileId: String,
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

  private object LiveBackend extends PostgreSqlAggregatePhysicalSqlExportBackend {
    override def generate(
        config: Config,
        planningSql: String
    ): Either[Failure, Generated] = {
      try Class.forName(LivePostgreSqlConnectivityProbe.PostgreSqlDriverClass)
      catch { case NonFatal(_) => return Left(Failure.BackendUnavailable) }
      val rewriter = PostgreSqlQueryRewriter
        .forPostgreSqlJdbc(
          config.jdbcUrl,
          config.user,
          config.password,
          config.schema
        )
        .fold(_ => return Left(Failure.BackendUnavailable), identity)
      val rewrite = rewriter.rewriteDetailed(
        RewriteRequest(
          planningSql,
          options = RewriteOptions(
            widthBound = config.widthBound,
            planLimit = PostgreSqlAggregatePhysicalSweepV3.RequiredPlanLimit,
            requireConnectedCover = true,
            candidateSelection = CandidatePlanSelection.StructuralHead,
            reductionMode = YannakakisReductionMode.FullTwoPass
          )
        )
      )
      rewrite.outcome match {
        case _: RewriteOutcome.Rewritten => ()
        case _ => return Left(Failure.GenerationFailed)
      }
      val artifact = rewrite.report.artifact
        .getOrElse(return Left(Failure.GenerationFailed))
      val result = PostgreSqlAggregatePhysicalSweepV3
        .produce(artifact, config.family)
        .fold(_ => return Left(Failure.GenerationFailed), identity)
      val retained =
        if (isWidth3Frequency(config))
          result.actions.filter(
            _.cteMaterializationDirective ==
              PostgreSqlCteMaterializationDirective.PlannerControlled
          )
        else result.actions
      Right(
        Generated(
          result.protocolId,
          result.family,
          result.resultShapeId,
          result.aggregateRenderingProfileId,
          result.enumerationRenderingProfileId,
          retained.zipWithIndex.map { case (action, index) =>
            Action(
              index + 1,
              action.aliasId,
              action.structuralRank,
              action.candidateId.value,
              action.physicalVariantId.value,
              action.configuration,
              action.reductionMode,
              action.cteMaterializationDirective,
              action.sql
            )
          }
        )
      )
    }
  }

  private object Publisher {
    private val DirectoryPermissions = PosixFilePermissions.fromString("rwx------")
    private val FilePermissions = PosixFilePermissions.fromString("rw-------")
    private val DirectoryAttribute =
      PosixFilePermissions.asFileAttribute(DirectoryPermissions)
    private val FileAttribute = PosixFilePermissions.asFileAttribute(FilePermissions)

    private final case class FileReference(path: String, bytes: Long, sha256: String)

    def publish(
        config: Config,
        source: PostgreSqlGeneratedSqlOptimizationSource,
        generated: Generated
    ): Either[Failure, Unit] =
      try {
        val parent = config.outputDirectory.getParent
        if (
          parent == null || !Files.isDirectory(parent, LinkOption.NOFOLLOW_LINKS) ||
          Files.isSymbolicLink(parent)
        ) return Left(Failure.PublicationFailed)
        Files.createDirectory(config.outputDirectory, DirectoryAttribute)
        val actionDirectory = config.outputDirectory.resolve("actions")
        Files.createDirectory(actionDirectory, DirectoryAttribute)

        val sourceBytes = source.source.text.getBytes(StandardCharsets.UTF_8)
        val sourceRef = reference("source.sql", sourceBytes)
        val nativeRef = reference("native.sql", sourceBytes)
        write(config.outputDirectory.resolve(sourceRef.path), sourceBytes)
        write(config.outputDirectory.resolve(nativeRef.path), sourceBytes)
        val actionRefs = generated.actions.map { action =>
          val bytes = action.sql.getBytes(StandardCharsets.UTF_8)
          val ref = reference(s"actions/${action.aliasId}.sql", bytes)
          write(config.outputDirectory.resolve(ref.path), bytes)
          action -> ref
        }
        val manifest = manifestBytes(
          config,
          source,
          generated,
          sourceRef,
          nativeRef,
          actionRefs
        )
        write(config.outputDirectory.resolve("manifest.json"), manifest)
        Right(())
      } catch { case NonFatal(_) => Left(Failure.PublicationFailed) }

    private def manifestBytes(
        config: Config,
        source: PostgreSqlGeneratedSqlOptimizationSource,
        generated: Generated,
        sourceRef: FileReference,
        nativeRef: FileReference,
        actions: Vector[(Action, FileReference)]
    ): Array[Byte] = {
      import PostgreSqlEnumerationCampaignV2CanonicalJson._
      def file(value: FileReference): Value = obj(
        "bytes" -> number(value.bytes),
        "path" -> string(value.path),
        "sha256" -> string(value.sha256)
      )
      def configuration(value: BagFactorPhysicalConfiguration): Value = obj(
        "cover_assignments" -> array(value.coverAssignments.map(assignment =>
          obj(
            "bag_id" -> number(assignment.bagId.value),
            "edge_ids" -> array(assignment.edgeIds.map(edge => number(edge.value)))
          )
        )),
        "execution_root" -> number(value.executionRoot.value)
      )
      document(
        obj(
          "actions" -> array(actions.map { case (action, ref) =>
            obj(
              "alias_id" -> string(action.aliasId),
              "candidate_id" -> string(action.candidateId),
              "configuration" -> configuration(action.configuration),
              "cte_directive" -> string(action.cteDirective.id),
              "ordinal" -> number(action.ordinal),
              "physical_variant_id" -> string(action.physicalVariantId),
              "reduction_mode" -> string(action.reductionMode.id),
              "sql" -> file(ref),
              "structural_rank" -> number(action.structuralRank)
            )
          }),
          "contains_database_results" -> bool(false),
          "contains_execution_outcomes" -> bool(false),
          "contains_timing" -> bool(false),
          "family" -> string(config.family.id),
          "group_key_count" -> number(config.family.groupKeyCount),
          "files" -> obj(
            "native_sql" -> file(nativeRef),
            "source_sql" -> file(sourceRef)
          ),
          "input" -> obj(
            "plan_limit" -> number(PostgreSqlAggregatePhysicalSweepV3.RequiredPlanLimit),
            "planning_transform" -> string(source.planningTransform),
            "require_connected_cover" -> bool(true),
            "schema" -> string(config.schema),
            "width" -> number(config.widthBound)
          ),
          "protocol_id" -> string(generated.protocolId),
          "producer_result_shape" -> string(generated.resultShapeId),
          "renderer" -> obj(
            "aggregate_profile" -> string(generated.aggregateProfileId),
            "enumeration_profile" -> string(generated.enumerationProfileId),
            "version" -> string(RendererVersion)
          ),
          "result_shape" -> string(generated.resultShapeId),
          "schema_version" -> string(
            if (config.widthBound == 2) SchemaVersion
            else if (isWidth3Frequency(config)) Width3FrequencySchemaVersion
            else Width3SchemaVersion
          ),
          "semantic_execution_lane" ->
            string(config.family.semanticExecutionLane),
          "status" -> string("development-only-nonreportable"),
          "training_eligible" -> bool(false)
        )
      ).bytes
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
  }
}

private[cli] trait PostgreSqlAggregatePhysicalSqlExportBackend {
  def generate(
      config: PostgreSqlAggregatePhysicalSqlExportCli.Config,
      planningSql: String
  ): Either[
    PostgreSqlAggregatePhysicalSqlExportCli.Failure,
    PostgreSqlAggregatePhysicalSqlExportCli.Generated
  ]
}
