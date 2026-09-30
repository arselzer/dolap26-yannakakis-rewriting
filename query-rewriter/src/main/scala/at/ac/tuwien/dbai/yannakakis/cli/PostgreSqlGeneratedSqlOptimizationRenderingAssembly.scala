package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.security.MessageDigest

import org.postgresql.ds.PGSimpleDataSource

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlAggregateRenderingProfile,
  PostgreSqlBagFactorRenderer,
  PostgreSqlRenderingLimits,
  PostgreSqlStagePhysicalForm
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCteMaterializationDirective,
  PostgreSqlGeneratedSqlOptimizationActionId,
  PostgreSqlPhysicalVariantId,
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteArtifact
}

/** Closed query families admitted by the development-only rendering producer. */
private[cli] sealed trait PostgreSqlGeneratedSqlOptimizationFamily
    extends Product
    with Serializable {
  def id: String
  def factoryMode: String
  def usesC03G0V2Factory: Boolean
}

private[cli] object PostgreSqlGeneratedSqlOptimizationFamily {
  private val OrdinaryFactoryMode = "ordinary"
  private val C03FactoryMode = "c03-g0-v2-opt-in"

  case object GlobalCountStar extends PostgreSqlGeneratedSqlOptimizationFamily {
    override val id: String = "global-count-star"
    override val factoryMode: String = OrdinaryFactoryMode
    override val usesC03G0V2Factory: Boolean = false
  }

  case object GroupedCountStar extends PostgreSqlGeneratedSqlOptimizationFamily {
    override val id: String = "grouped-count-star"
    override val factoryMode: String = OrdinaryFactoryMode
    override val usesC03G0V2Factory: Boolean = false
  }

  case object DistributedGroupedCountStar
      extends PostgreSqlGeneratedSqlOptimizationFamily {
    override val id: String = "distributed-grouped-count-star"
    override val factoryMode: String = OrdinaryFactoryMode
    override val usesC03G0V2Factory: Boolean = false
  }

  case object C03G0AggregateVectorV2
      extends PostgreSqlGeneratedSqlOptimizationFamily {
    override val id: String = "c03-g0-aggregate-vector-v2"
    override val factoryMode: String = C03FactoryMode
    override val usesC03G0V2Factory: Boolean = true
  }

  val Values: Vector[PostgreSqlGeneratedSqlOptimizationFamily] = Vector(
    GlobalCountStar,
    GroupedCountStar,
    DistributedGroupedCountStar,
    C03G0AggregateVectorV2
  )

  def parse(value: String): Option[PostgreSqlGeneratedSqlOptimizationFamily] =
    if (value == null) None else Values.find(_.id == value)
}

private[cli] sealed trait PostgreSqlGeneratedSqlOptimizationPlanShape
    extends Product
    with Serializable {
  def id: String
  def accepts(factorCount: Int): Boolean
}

private[cli] object PostgreSqlGeneratedSqlOptimizationPlanShape {
  case object OneBag extends PostgreSqlGeneratedSqlOptimizationPlanShape {
    override val id: String = "one-bag"
    override def accepts(factorCount: Int): Boolean = factorCount == 1
  }

  case object MultiBag extends PostgreSqlGeneratedSqlOptimizationPlanShape {
    override val id: String = "multi-bag"
    override def accepts(factorCount: Int): Boolean = factorCount > 1
  }

  val Values: Vector[PostgreSqlGeneratedSqlOptimizationPlanShape] =
    Vector(OneBag, MultiBag)

  def parse(value: String): Option[PostgreSqlGeneratedSqlOptimizationPlanShape] =
    if (value == null) None else Values.find(_.id == value)
}

private[cli] sealed trait PostgreSqlGeneratedSqlOptimizationSourceFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlGeneratedSqlOptimizationSourceFailure {
  case object Empty extends PostgreSqlGeneratedSqlOptimizationSourceFailure {
    override val code: String = "generated-sql-rendering.source-empty"
  }
  case object TooLarge extends PostgreSqlGeneratedSqlOptimizationSourceFailure {
    override val code: String = "generated-sql-rendering.source-too-large"
  }
  case object InvalidUtf8 extends PostgreSqlGeneratedSqlOptimizationSourceFailure {
    override val code: String = "generated-sql-rendering.source-invalid-utf8"
  }
  case object ContainsNul extends PostgreSqlGeneratedSqlOptimizationSourceFailure {
    override val code: String = "generated-sql-rendering.source-contains-nul"
  }
  case object UnsupportedTerminalSemicolon
      extends PostgreSqlGeneratedSqlOptimizationSourceFailure {
    override val code: String =
      "generated-sql-rendering.source-unsupported-terminal-semicolon"
  }
}

private[cli] final case class PostgreSqlGeneratedSqlOptimizationSqlBinding(
    text: String,
    byteLength: Int,
    sha256: String
) {
  require(text != null && text.nonEmpty, "SQL binding text must not be empty")
  require(byteLength > 0, "SQL binding byte length must be positive")
  require(
    sha256 != null && sha256.matches("[0-9a-f]{64}"),
    "SQL binding digest must be lowercase SHA-256"
  )
}

/**
  * Exact file bytes and the sole planning-only terminal-semicolon transform.
  * Native registry bytes always come from `source`; only `planning` is passed
  * to Calcite. No whitespace trimming or statement normalization is allowed.
  */
private[cli] final case class PostgreSqlGeneratedSqlOptimizationSource private (
    source: PostgreSqlGeneratedSqlOptimizationSqlBinding,
    planning: PostgreSqlGeneratedSqlOptimizationSqlBinding,
    planningTransform: String
) {
  require(source != null && planning != null)
  require(
    planningTransform ==
      PostgreSqlGeneratedSqlOptimizationSource.IdentityTransform ||
      planningTransform ==
        PostgreSqlGeneratedSqlOptimizationSource.RemoveBeforeFinalLfTransform ||
      planningTransform ==
        PostgreSqlGeneratedSqlOptimizationSource.RemoveAtEofTransform,
    "planning transform must be one exact closed value"
  )
}

private[cli] object PostgreSqlGeneratedSqlOptimizationSource {
  import PostgreSqlGeneratedSqlOptimizationSourceFailure._

  val MaximumSourceBytes: Int = 16 * 1024 * 1024
  val MaximumGeneratedSqlBytes: Int = 4 * 1024 * 1024
  val IdentityTransform: String = "identity-v1"
  val RemoveBeforeFinalLfTransform: String =
    "remove-one-terminal-semicolon-before-final-lf-v1"
  val RemoveAtEofTransform: String =
    "remove-one-terminal-semicolon-at-eof-v1"

  def fromBytes(
      raw: Array[Byte]
  ): Either[
    PostgreSqlGeneratedSqlOptimizationSourceFailure,
    PostgreSqlGeneratedSqlOptimizationSource
  ] = {
    if (raw == null || raw.isEmpty) return Left(Empty)
    if (raw.length > MaximumSourceBytes) return Left(TooLarge)
    val sourceText = decodeStrict(raw).getOrElse(return Left(InvalidUtf8))
    if (sourceText.indexOf('\u0000') >= 0) return Left(ContainsNul)

    val (planningText, transform) =
      if (sourceText.endsWith(";\n"))
        sourceText.dropRight(2) + "\n" -> RemoveBeforeFinalLfTransform
      else if (sourceText.endsWith(";"))
        sourceText.dropRight(1) -> RemoveAtEofTransform
      else {
        val lastNonWhitespace = sourceText.lastIndexWhere(character =>
          !Character.isWhitespace(character)
        )
        if (
          lastNonWhitespace >= 0 && sourceText.charAt(lastNonWhitespace) == ';'
        ) return Left(UnsupportedTerminalSemicolon)
        sourceText -> IdentityTransform
      }

    for {
      sourceBinding <- binding(sourceText, MaximumSourceBytes).toRight(InvalidUtf8)
      planningBinding <- binding(planningText, MaximumSourceBytes).toRight(InvalidUtf8)
      _ <-
        if (java.util.Arrays.equals(strictBytes(sourceText).orNull, raw)) Right(())
        else Left(InvalidUtf8)
    } yield PostgreSqlGeneratedSqlOptimizationSource(
      sourceBinding,
      planningBinding,
      transform
    )
  }

  def generatedBinding(
      value: String
  ): Either[
    PostgreSqlGeneratedSqlOptimizationBackendFailure,
    PostgreSqlGeneratedSqlOptimizationSqlBinding
  ] =
    binding(value, MaximumGeneratedSqlBytes).toRight(
      PostgreSqlGeneratedSqlOptimizationBackendFailure.GeneratedSqlInvalid
    )

  private def binding(
      value: String,
      maximumBytes: Int
  ): Option[PostgreSqlGeneratedSqlOptimizationSqlBinding] =
    if (value == null || value.isEmpty || value.indexOf('\u0000') >= 0) None
    else
      strictBytes(value).filter(_.length <= maximumBytes).map(bytes =>
        PostgreSqlGeneratedSqlOptimizationSqlBinding(
          value,
          bytes.length,
          sha256(bytes)
        )
      )

  private def decodeStrict(bytes: Array[Byte]): Option[String] =
    try {
      Some(
        StandardCharsets.UTF_8
          .newDecoder()
          .onMalformedInput(CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)
          .decode(ByteBuffer.wrap(bytes))
          .toString
      )
    } catch { case _: java.nio.charset.CharacterCodingException => None }

  private def strictBytes(value: String): Option[Array[Byte]] =
    try {
      val buffer = StandardCharsets.UTF_8
        .newEncoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
        .encode(java.nio.CharBuffer.wrap(value))
      val bytes = new Array[Byte](buffer.remaining())
      buffer.get(bytes)
      Some(bytes)
    } catch { case _: java.nio.charset.CharacterCodingException => None }

  private[cli] def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}

private[cli] final case class PostgreSqlGeneratedSqlOptimizationAction(
    actionAlias: String,
    actionId: String,
    basePhysicalVariantId: Option[String],
    kind: String,
    oneBagTerminal: Option[String],
    profileId: Option[String],
    profileOrdinal: Option[Int],
    reductionFinalization: Option[String],
    reductionSupport: Option[String],
    sql: PostgreSqlGeneratedSqlOptimizationSqlBinding
) {
  require(actionAlias != null && actionAlias.matches("[a-z][a-z0-9_-]{0,127}"))
  require(actionId != null && actionId.nonEmpty)
  require(basePhysicalVariantId != null)
  require(kind == "native" || kind == "generated")
  require(oneBagTerminal != null && profileId != null && profileOrdinal != null)
  require(reductionFinalization != null && reductionSupport != null)
  require(sql != null)
  if (kind == "native") {
    require(
      actionAlias == "native" && actionId == "native" &&
        basePhysicalVariantId.isEmpty && oneBagTerminal.isEmpty &&
        profileId.isEmpty && profileOrdinal.isEmpty &&
        reductionFinalization.isEmpty && reductionSupport.isEmpty,
      "native action must use the exact closed null projection"
    )
  } else {
    require(actionAlias != "native" && actionId.matches("sha256:[0-9a-f]{64}"))
    require(basePhysicalVariantId.exists(_.matches("sha256:[0-9a-f]{64}")))
    require(
      oneBagTerminal.nonEmpty && profileId.nonEmpty && profileOrdinal.nonEmpty &&
        reductionFinalization.nonEmpty && reductionSupport.nonEmpty,
      "generated action must bind one complete rendering profile"
    )
  }
}

private[cli] final case class PostgreSqlGeneratedSqlOptimizationRenderingAssembly(
    queryId: String,
    family: PostgreSqlGeneratedSqlOptimizationFamily,
    cellId: String,
    datasetId: String,
    source: PostgreSqlGeneratedSqlOptimizationSource,
    candidateId: String,
    candidateRank: Int,
    retainedCandidateCount: Int,
    basePhysicalVariantId: String,
    planShape: PostgreSqlGeneratedSqlOptimizationPlanShape,
    actions: Vector[PostgreSqlGeneratedSqlOptimizationAction]
) {
  require(queryId != null && queryId.matches("[a-z][a-z0-9_-]{0,127}"))
  require(cellId != null && cellId.matches("[a-z][a-z0-9_-]{0,127}"))
  require(datasetId != null && datasetId.matches("[a-z][a-z0-9_-]{0,127}"))
  require(family != null && source != null && planShape != null)
  require(candidateId != null && candidateId.matches("sha256:[0-9a-f]{64}"))
  require(
    retainedCandidateCount > 0 &&
      retainedCandidateCount <= RewriteOptions.DefaultPlanLimit &&
      candidateRank > 0 && candidateRank <= retainedCandidateCount
  )
  require(basePhysicalVariantId != null && basePhysicalVariantId.matches("sha256:[0-9a-f]{64}"))
  require(actions != null && actions.size >= 2 && actions.size <= 9)
  require(actions.head.kind == "native" && actions.head.actionAlias == "native")
  require(actions.head.sql == source.source, "native action must equal source bytes")
  private val parsedCandidateId = CandidatePlanId.parse(candidateId).toOption.get
  require(
    PostgreSqlPhysicalVariantId
      .from(parsedCandidateId, YannakakisReductionMode.FullTwoPass)
      .value == basePhysicalVariantId,
    "base physical variant must be the frozen full-reduction v1 identity"
  )
  private val generated = actions.tail
  private val ordinals = generated.flatMap(_.profileOrdinal)
  require(
    ordinals.size == generated.size && ordinals.headOption.contains(1) &&
      ordinals == ordinals.sorted.distinct &&
      ordinals.forall(value => value >= 1 && value <= 8),
    "generated actions must use canonical unique profile order beginning at one"
  )
  generated.foreach { action =>
    val profile = PostgreSqlAggregateRenderingProfile.Values(
      action.profileOrdinal.get - 1
    )
    require(
      action.actionAlias == f"profile_${action.profileOrdinal.get}%02d" &&
        action.basePhysicalVariantId.contains(basePhysicalVariantId) &&
        action.oneBagTerminal.contains(profile.oneBagTerminal.id) &&
        action.profileId.contains(profile.id) &&
        action.reductionFinalization.contains(profile.reductionFinalization.id) &&
        action.reductionSupport.contains(profile.reductionSupport.id) &&
        action.actionId == PostgreSqlGeneratedSqlOptimizationActionId
          .from(parsedCandidateId, YannakakisReductionMode.FullTwoPass, profile)
          .value,
      "generated action must match its exact Scala rendering-profile identity"
    )
  }
}

private[cli] object PostgreSqlGeneratedSqlOptimizationRenderingAssembly {
  import PostgreSqlEnumerationCampaignV2CanonicalJson._

  val SchemaVersion: String =
    "yrw-generated-sql-optimization-rendering-assembly/v1"

  def render(
      assembly: PostgreSqlGeneratedSqlOptimizationRenderingAssembly
  ): PostgreSqlEnumerationCampaignV2JsonDocument = {
    require(assembly != null)
    def optionalString(value: Option[String]): Value =
      value.fold[Value](NullValue)(string)
    def optionalNumber(value: Option[Int]): Value =
      value.fold[Value](NullValue)(number(_))
    def action(value: PostgreSqlGeneratedSqlOptimizationAction): Value = obj(
      "action_alias" -> string(value.actionAlias),
      "action_id" -> string(value.actionId),
      "base_physical_variant_id" -> optionalString(value.basePhysicalVariantId),
      "kind" -> string(value.kind),
      "one_bag_terminal" -> optionalString(value.oneBagTerminal),
      "profile_id" -> optionalString(value.profileId),
      "profile_ordinal" -> optionalNumber(value.profileOrdinal),
      "reduction_finalization" -> optionalString(value.reductionFinalization),
      "reduction_support" -> optionalString(value.reductionSupport),
      "sql_byte_length" -> number(value.sql.byteLength),
      "sql_sha256" -> string(value.sql.sha256),
      "sql_utf8_hex" -> utf8HexString(value.sql.text, value.sql.byteLength.toLong)
    )

    document(
      obj(
        "action_scheme" -> string(PostgreSqlGeneratedSqlOptimizationActionId.Scheme),
        "actions" -> array(assembly.actions.map(action)),
        "base_physical_variant_id" -> string(assembly.basePhysicalVariantId),
        "candidate_id" -> string(assembly.candidateId),
        "candidate_rank" -> number(assembly.candidateRank),
        "cell_id" -> string(assembly.cellId),
        "classification" -> string("development-only"),
        "cte_materialization" -> string(
          PostgreSqlCteMaterializationDirective.PlannerControlled.id
        ),
        "dataset_id" -> string(assembly.datasetId),
        "factory_mode" -> string(assembly.family.factoryMode),
        "family" -> string(assembly.family.id),
        "plan_limit" -> number(RewriteOptions.DefaultPlanLimit),
        "plan_shape" -> string(assembly.planShape.id),
        "planning_source_byte_length" -> number(
          assembly.source.planning.byteLength
        ),
        "planning_source_sha256" -> string(assembly.source.planning.sha256),
        "planning_sql_transform" -> string(
          assembly.source.planningTransform
        ),
        "profile_scheme" -> string(PostgreSqlAggregateRenderingProfile.Scheme),
        "query_id" -> string(assembly.queryId),
        "reduction_mode" -> string(YannakakisReductionMode.FullTwoPass.id),
        "require_connected_cover" -> bool(
          RewriteOptions.DefaultRequireConnectedCover
        ),
        "retained_candidate_count" -> number(
          assembly.retainedCandidateCount
        ),
        "schema_version" -> string(SchemaVersion),
        "source_byte_length" -> number(assembly.source.source.byteLength),
        "source_sha256" -> string(assembly.source.source.sha256),
        "source_utf8_hex" -> utf8HexString(
          assembly.source.source.text,
          assembly.source.source.byteLength.toLong
        ),
        "width_bound" -> number(RewriteOptions.DefaultWidthBound)
      )
    )
  }
}

private[cli] sealed trait PostgreSqlGeneratedSqlOptimizationBackendFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlGeneratedSqlOptimizationBackendFailure {
  case object ConfigurationRejected
      extends PostgreSqlGeneratedSqlOptimizationBackendFailure {
    override val code: String = "generated-sql-rendering.configuration-rejected"
  }
  case object RewriteRejected
      extends PostgreSqlGeneratedSqlOptimizationBackendFailure {
    override val code: String = "generated-sql-rendering.rewrite-rejected"
  }
  case object IdentityMismatch
      extends PostgreSqlGeneratedSqlOptimizationBackendFailure {
    override val code: String = "generated-sql-rendering.identity-mismatch"
  }
  case object ProfileRenderingFailed
      extends PostgreSqlGeneratedSqlOptimizationBackendFailure {
    override val code: String = "generated-sql-rendering.profile-rendering-failed"
  }
  case object GeneratedSqlInvalid
      extends PostgreSqlGeneratedSqlOptimizationBackendFailure {
    override val code: String = "generated-sql-rendering.generated-sql-invalid"
  }
}

private[cli] trait PostgreSqlGeneratedSqlOptimizationRenderingBackend {
  def produce(
      config: PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig,
      source: PostgreSqlGeneratedSqlOptimizationSource
  ): Either[
    PostgreSqlGeneratedSqlOptimizationBackendFailure,
    PostgreSqlGeneratedSqlOptimizationRenderingAssembly
  ]
}

private[cli] object LivePostgreSqlGeneratedSqlOptimizationRenderingBackend
    extends PostgreSqlGeneratedSqlOptimizationRenderingBackend {
  import PostgreSqlGeneratedSqlOptimizationBackendFailure._

  override def produce(
      config: PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig,
      source: PostgreSqlGeneratedSqlOptimizationSource
  ): Either[
    PostgreSqlGeneratedSqlOptimizationBackendFailure,
    PostgreSqlGeneratedSqlOptimizationRenderingAssembly
  ] = {
    require(config != null && source != null)
    val dataSource = new PGSimpleDataSource()
    dataSource.setURL(config.jdbc.jdbcUrl)
    dataSource.setUser(config.jdbc.user)
    dataSource.setPassword(config.jdbc.password)

    val configured =
      if (config.family.usesC03G0V2Factory)
        PostgreSqlQueryRewriter.forJdbcDataSourceWithC03G0V2(
          dataSource,
          config.schemaName
        )
      else
        PostgreSqlQueryRewriter.forJdbcDataSource(
          dataSource,
          config.schemaName
        )
    configured.left.map(_ => ConfigurationRejected).flatMap(rewriter =>
      produceWithRewriter(config, source, rewriter)
    )
  }

  private def produceWithRewriter(
      config: PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig,
      source: PostgreSqlGeneratedSqlOptimizationSource,
      rewriter: PostgreSqlQueryRewriter
  ): Either[
    PostgreSqlGeneratedSqlOptimizationBackendFailure,
    PostgreSqlGeneratedSqlOptimizationRenderingAssembly
  ] = {
    val options = RewriteOptions(
      widthBound = RewriteOptions.DefaultWidthBound,
      planLimit = RewriteOptions.DefaultPlanLimit,
      requireConnectedCover = RewriteOptions.DefaultRequireConnectedCover,
      candidateSelection = CandidatePlanSelection.StructuralRank(
        config.candidateRank
      ),
      reductionMode = YannakakisReductionMode.FullTwoPass
    )
    val result = rewriter.rewriteDetailed(
      RewriteRequest(source.planning.text, options = options)
    )
    val artifact = result.report.artifact.getOrElse(return Left(RewriteRejected))
    if (!validLegacyResult(config, source, result.outcome, artifact))
      return Left(IdentityMismatch)

    val candidateId = artifact.selectedPlan.candidateId
    val basePhysicalVariantId = PostgreSqlPhysicalVariantId.from(
      candidateId,
      YannakakisReductionMode.FullTwoPass
    )
    if (
      artifact.candidateSelection.selectedPhysicalVariantId !=
        basePhysicalVariantId
    ) return Left(IdentityMismatch)

    val native = PostgreSqlGeneratedSqlOptimizationAction(
      actionAlias = "native",
      actionId = "native",
      basePhysicalVariantId = None,
      kind = "native",
      oneBagTerminal = None,
      profileId = None,
      profileOrdinal = None,
      reductionFinalization = None,
      reductionSupport = None,
      sql = source.source
    )
    val generated = Vector.newBuilder[PostgreSqlGeneratedSqlOptimizationAction]
    config.profileOrdinals.foreach { ordinal =>
      val profile = PostgreSqlAggregateRenderingProfile.Values(ordinal - 1)
      val rendered = PostgreSqlBagFactorRenderer
        .compileAndRenderWithAggregateProfile(
          artifact.query,
          artifact.candidateInput,
          artifact.bagFactorPlan,
          PostgreSqlRenderingLimits.default,
          BagFactorPlanningLimits.default,
          YannakakisReductionMode.FullTwoPass,
          profile
        )
        .fold(_ => return Left(ProfileRenderingFailed), _._2)
      if (
        ordinal == 1 &&
        (profile != PostgreSqlAggregateRenderingProfile.LegacyV1 ||
          rendered.sql != artifact.renderedQuery.sql)
      ) return Left(IdentityMismatch)
      val sql = PostgreSqlGeneratedSqlOptimizationSource
        .generatedBinding(rendered.sql)
        .fold(problem => return Left(problem), identity)
      generated += PostgreSqlGeneratedSqlOptimizationAction(
        actionAlias = f"profile_$ordinal%02d",
        actionId = PostgreSqlGeneratedSqlOptimizationActionId
          .from(candidateId, YannakakisReductionMode.FullTwoPass, profile)
          .value,
        basePhysicalVariantId = Some(basePhysicalVariantId.value),
        kind = "generated",
        oneBagTerminal = Some(profile.oneBagTerminal.id),
        profileId = Some(profile.id),
        profileOrdinal = Some(ordinal),
        reductionFinalization = Some(profile.reductionFinalization.id),
        reductionSupport = Some(profile.reductionSupport.id),
        sql = sql
      )
    }

    Right(
      PostgreSqlGeneratedSqlOptimizationRenderingAssembly(
        queryId = config.queryId,
        family = config.family,
        cellId = config.cellId,
        datasetId = config.datasetId,
        source = source,
        candidateId = candidateId.value,
        candidateRank = config.candidateRank,
        retainedCandidateCount = artifact.candidateSearch.plans.size,
        basePhysicalVariantId = basePhysicalVariantId.value,
        planShape = config.planShape,
        actions = native +: generated.result()
      )
    )
  }

  private def validLegacyResult(
      config: PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig,
      source: PostgreSqlGeneratedSqlOptimizationSource,
      outcome: RewriteOutcome,
      artifact: PostgreSqlRewriteArtifact
  ): Boolean = {
    val rewrittenMatches = outcome match {
      case RewriteOutcome.Rewritten(original, Vector(statement), _) =>
        original == source.planning.text && statement == artifact.renderedQuery.sql
      case _ => false
    }
    val expectedCandidate = artifact.candidateSearch.plans.lift(
      config.candidateRank - 1
    )
    rewrittenMatches &&
    artifact.query.resultShape.id == config.family.id &&
    artifact.selectedPlan.rank == config.candidateRank &&
    expectedCandidate.contains(artifact.selectedPlan) &&
    artifact.selectedPlan.candidateId ==
      CandidatePlanId.from(artifact.selectedPlan.decomposition) &&
    artifact.candidateSelection.policy ==
      CandidatePlanSelection.StructuralRank(config.candidateRank) &&
    artifact.candidateSelection.reductionMode ==
      YannakakisReductionMode.FullTwoPass &&
    artifact.candidateSelection.selectedCandidateId ==
      artifact.selectedPlan.candidateId &&
    artifact.candidateInput.widthBound == RewriteOptions.DefaultWidthBound &&
    artifact.candidateInput.requireConnectedCover ==
      RewriteOptions.DefaultRequireConnectedCover &&
    artifact.candidateSearch.requestedLimit == RewriteOptions.DefaultPlanLimit &&
    config.planShape.accepts(artifact.bagFactorPlan.factors.size) &&
    artifact.renderedQuery.trace.stages.forall(stage =>
      !stage.physicalForm.namedCte ||
        stage.physicalForm == PostgreSqlStagePhysicalForm.PlannerControlledCte
    )
  }
}
