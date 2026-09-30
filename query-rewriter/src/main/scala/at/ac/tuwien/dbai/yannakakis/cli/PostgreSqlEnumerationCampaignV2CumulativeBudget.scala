package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchArtifact,
  PostgreSqlEnumerationCampaignV2ProjectionView,
  PostgreSqlEnumerationSqlTransport,
  PostgreSqlEnumerationTransportLimitEvidence
}

/** Closed byte resources charged by the campaign-v2 cumulative boundary. */
private[yannakakis] sealed trait PostgreSqlEnumerationCumulativeByteResource
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlEnumerationCumulativeByteResource {
  case object SourceSqlBytes
      extends PostgreSqlEnumerationCumulativeByteResource {
    override val id: String = "source-sql-bytes"
  }
  case object GeneratedSqlBytes
      extends PostgreSqlEnumerationCumulativeByteResource {
    override val id: String = "generated-sql-bytes"
  }
  case object StageSqlBytes
      extends PostgreSqlEnumerationCumulativeByteResource {
    override val id: String = "stage-sql-bytes"
  }
  case object AllSqlBytes extends PostgreSqlEnumerationCumulativeByteResource {
    override val id: String = "all-sql-bytes"
  }
  case object CanonicalJsonDocumentBytes
      extends PostgreSqlEnumerationCumulativeByteResource {
    override val id: String = "canonical-json-document-bytes"
  }
}

/** Closed item kinds in the exact accounting transcript. */
private[yannakakis] sealed trait PostgreSqlEnumerationCumulativeItemKind
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlEnumerationCumulativeItemKind {
  case object SourceSql extends PostgreSqlEnumerationCumulativeItemKind {
    override val id: String = "source-sql"
  }
  case object GeneratedSql extends PostgreSqlEnumerationCumulativeItemKind {
    override val id: String = "generated-sql"
  }
  case object StageSql extends PostgreSqlEnumerationCumulativeItemKind {
    override val id: String = "stage-sql"
  }
  case object CanonicalJsonDocument
      extends PostgreSqlEnumerationCumulativeItemKind {
    override val id: String = "canonical-json-document"
  }
}

/** One exact, SQL-free item identity in the cumulative accounting transcript. */
private[yannakakis] final case class PostgreSqlEnumerationCumulativeByteItem private[cli] (
    kind: PostgreSqlEnumerationCumulativeItemKind,
    ordinal: Int,
    logicalId: String,
    byteLength: Long,
    sha256: String
) extends Serializable {
  require(kind != null, "cumulative item kind must not be null")
  require(ordinal > 0, "cumulative item ordinal must be positive")
  require(
    logicalId != null && logicalId.nonEmpty,
    "cumulative item logical ID must be nonempty"
  )
  require(byteLength > 0L, "cumulative item byte length must be positive")
  require(
    sha256 != null && sha256.matches("[0-9a-f]{64}"),
    "cumulative item SHA-256 must be lowercase canonical hex"
  )
}

/**
  * Fixed producer-wide cumulative ceilings for the primary `K <= 10` slice.
  *
  * These limits are additive to every existing per-item/per-document limit.
  * They are benchmark-local and do not alter the frozen rewrite JSON schemas.
  */
private[yannakakis] final class PostgreSqlEnumerationCumulativeByteLimits private (
    val maxSourceSqlBytes: Long,
    val maxGeneratedSqlBytes: Long,
    val maxStageSqlBytes: Long,
    val maxAllSqlBytes: Long,
    val maxCanonicalJsonDocumentBytes: Long
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlEnumerationCumulativeByteLimits =>
      maxSourceSqlBytes == that.maxSourceSqlBytes &&
        maxGeneratedSqlBytes == that.maxGeneratedSqlBytes &&
        maxStageSqlBytes == that.maxStageSqlBytes &&
        maxAllSqlBytes == that.maxAllSqlBytes &&
        maxCanonicalJsonDocumentBytes ==
          that.maxCanonicalJsonDocumentBytes
    case _ => false
  }

  override def hashCode(): Int =
    (
      maxSourceSqlBytes,
      maxGeneratedSqlBytes,
      maxStageSqlBytes,
      maxAllSqlBytes,
      maxCanonicalJsonDocumentBytes
    ).hashCode()

  override def toString: String =
    s"PostgreSqlEnumerationCumulativeByteLimits($maxSourceSqlBytes," +
      s"$maxGeneratedSqlBytes,$maxStageSqlBytes,$maxAllSqlBytes," +
      s"$maxCanonicalJsonDocumentBytes)"
}

private[yannakakis] object PostgreSqlEnumerationCumulativeByteLimits {
  sealed trait ValidationError extends Product with Serializable {
    def resource: PostgreSqlEnumerationCumulativeByteResource
    def value: Long
    def code: String = "enumeration-campaign-v2.cumulative.invalid-limit"
  }

  final case class NonPositive(
      resource: PostgreSqlEnumerationCumulativeByteResource,
      value: Long
  ) extends ValidationError

  private val MiB = 1024L * 1024L

  /**
    * The SQL maxima cover the exact primary K envelope. The 1 GiB canonical
    * ceiling is deliberately stricter than 48 independent 64 MiB documents.
    */
  val default: PostgreSqlEnumerationCumulativeByteLimits =
    create(
      maxSourceSqlBytes =
        PostgreSqlEnumerationTransportLimitEvidence.MaximumSourceSqlBytes,
      maxGeneratedSqlBytes = 160L * MiB,
      maxStageSqlBytes = 320L * MiB,
      maxAllSqlBytes = 496L * MiB,
      maxCanonicalJsonDocumentBytes = 1024L * MiB
    ).fold(
      errors =>
        throw new IllegalStateException(
          errors.map(_.code).mkString("invalid default limits: ", ";", "")
        ),
      identity
    )

  def create(
      maxSourceSqlBytes: Long,
      maxGeneratedSqlBytes: Long,
      maxStageSqlBytes: Long,
      maxAllSqlBytes: Long,
      maxCanonicalJsonDocumentBytes: Long
  ): Either[
    Vector[ValidationError],
    PostgreSqlEnumerationCumulativeByteLimits
  ] = {
    import PostgreSqlEnumerationCumulativeByteResource._
    val values = Vector(
      SourceSqlBytes -> maxSourceSqlBytes,
      GeneratedSqlBytes -> maxGeneratedSqlBytes,
      StageSqlBytes -> maxStageSqlBytes,
      AllSqlBytes -> maxAllSqlBytes,
      CanonicalJsonDocumentBytes -> maxCanonicalJsonDocumentBytes
    )
    val problems = Vector.newBuilder[ValidationError]
    values.foreach {
      case (resource, value) if value <= 0L =>
        problems += NonPositive(resource, value)
      case _ => ()
    }
    val errors = problems.result()
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new PostgreSqlEnumerationCumulativeByteLimits(
          maxSourceSqlBytes,
          maxGeneratedSqlBytes,
          maxStageSqlBytes,
          maxAllSqlBytes,
          maxCanonicalJsonDocumentBytes
        )
      )
  }
}

/** Exact successful or partial usage, always representable without overflow. */
private[yannakakis] final case class PostgreSqlEnumerationCumulativeByteUsage private[cli] (
    sourceSqlItemCount: Int,
    sourceSqlBytes: Long,
    generatedSqlItemCount: Int,
    generatedSqlBytes: Long,
    stageSqlItemCount: Int,
    stageSqlBytes: Long,
    allSqlItemCount: Int,
    allSqlBytes: Long,
    canonicalJsonDocumentCount: Int,
    canonicalJsonDocumentBytes: Long
) extends Serializable {
  require(
    Vector(
      sourceSqlItemCount,
      generatedSqlItemCount,
      stageSqlItemCount,
      allSqlItemCount,
      canonicalJsonDocumentCount
    ).forall(_ >= 0),
    "cumulative item counts must be nonnegative"
  )
  require(
    Vector(
      sourceSqlBytes,
      generatedSqlBytes,
      stageSqlBytes,
      allSqlBytes,
      canonicalJsonDocumentBytes
    ).forall(_ >= 0L),
    "cumulative byte usage must be nonnegative"
  )
  require(
    BigInt(allSqlItemCount) == BigInt(sourceSqlItemCount) +
      BigInt(generatedSqlItemCount) + BigInt(stageSqlItemCount) &&
      BigInt(allSqlBytes) == BigInt(sourceSqlBytes) +
        BigInt(generatedSqlBytes) + BigInt(stageSqlBytes),
    "all-SQL usage must equal the exact category sums"
  )
}

/** Typed, SQL-free cumulative-budget failure with exact partial usage. */
private[yannakakis] sealed trait PostgreSqlEnumerationCumulativeBudgetFailure
    extends Product
    with Serializable {
  def code: String
  def acceptedUsage: PostgreSqlEnumerationCumulativeByteUsage
}

private[yannakakis] object PostgreSqlEnumerationCumulativeBudgetFailure {
  final case class LimitExceeded(
      resource: PostgreSqlEnumerationCumulativeByteResource,
      maximumBytes: Long,
      attemptedBytes: BigInt,
      offendingItem: PostgreSqlEnumerationCumulativeByteItem,
      acceptedUsage: PostgreSqlEnumerationCumulativeByteUsage
  ) extends PostgreSqlEnumerationCumulativeBudgetFailure {
    require(resource != null, "failed cumulative resource must not be null")
    require(maximumBytes > 0L, "failed cumulative maximum must be positive")
    require(
      attemptedBytes != null && attemptedBytes > BigInt(maximumBytes),
      "attempted cumulative bytes must exceed the maximum"
    )
    require(offendingItem != null, "offending cumulative item must not be null")
    require(acceptedUsage != null, "accepted cumulative usage must not be null")
    override val code: String =
      "enumeration-campaign-v2.cumulative.limit-exceeded"
  }

  final case class SqlTransportRejected(
      itemKind: PostgreSqlEnumerationCumulativeItemKind,
      ordinal: Int,
      reasonCode: String,
      acceptedUsage: PostgreSqlEnumerationCumulativeByteUsage
  ) extends PostgreSqlEnumerationCumulativeBudgetFailure {
    require(itemKind != null, "rejected SQL item kind must not be null")
    require(ordinal > 0, "rejected SQL item ordinal must be positive")
    require(
      reasonCode != null && reasonCode.nonEmpty,
      "SQL transport rejection reason must be nonempty"
    )
    require(acceptedUsage != null, "accepted cumulative usage must not be null")
    override val code: String =
      "enumeration-campaign-v2.cumulative.sql-transport-rejected"
  }

  final case class ProjectionRejected(
      acceptedUsage: PostgreSqlEnumerationCumulativeByteUsage
  ) extends PostgreSqlEnumerationCumulativeBudgetFailure {
    require(acceptedUsage != null, "accepted cumulative usage must not be null")
    override val code: String =
      "enumeration-campaign-v2.cumulative.projection-rejected"
  }

  final case class DocumentInventoryRejected(
      reasonCode: String,
      acceptedUsage: PostgreSqlEnumerationCumulativeByteUsage
  ) extends PostgreSqlEnumerationCumulativeBudgetFailure {
    require(
      reasonCode != null && reasonCode.nonEmpty,
      "document inventory rejection reason must be nonempty"
    )
    require(acceptedUsage != null, "accepted cumulative usage must not be null")
    override val code: String =
      "enumeration-campaign-v2.cumulative.document-inventory-rejected"
  }

  final case class FinalPackageLimitExceeded(
      maximumBytes: Long,
      attemptedBytes: BigInt,
      acceptedUsage: PostgreSqlEnumerationCumulativeByteUsage
  ) extends PostgreSqlEnumerationCumulativeBudgetFailure {
    require(maximumBytes > 0L, "final-package maximum must be positive")
    require(
      attemptedBytes != null && attemptedBytes > BigInt(maximumBytes),
      "final-package attempted bytes must exceed the maximum"
    )
    require(acceptedUsage != null, "accepted cumulative usage must not be null")
    override val code: String =
      "enumeration-campaign-v2.cumulative.final-package-limit-exceeded"
  }
}

/**
  * Immutable successful logical-byte evidence over every charged item.
  *
  * These are retained UTF-8 payload/document bytes, not an estimate of JVM
  * heap, object headers, shared String backing, compiler state, or peak RSS.
  */
private[yannakakis] final case class PostgreSqlEnumerationCumulativeBudgetEvidence private[cli] (
    accountingScheme: String,
    limits: PostgreSqlEnumerationCumulativeByteLimits,
    usage: PostgreSqlEnumerationCumulativeByteUsage,
    items: Vector[PostgreSqlEnumerationCumulativeByteItem]
) extends Serializable {
  import PostgreSqlEnumerationCumulativeItemKind._
  require(
    accountingScheme ==
      PostgreSqlEnumerationCampaignV2CumulativeBudget.AccountingScheme,
    "cumulative evidence must retain the exact accounting scheme"
  )
  require(limits != null, "cumulative evidence limits must not be null")
  require(usage != null, "cumulative evidence usage must not be null")
  require(
    items != null && items.nonEmpty && items.forall(_ != null),
    "cumulative evidence items must be nonempty and contain no null"
  )

  private def selected(
      kind: PostgreSqlEnumerationCumulativeItemKind
  ): Vector[PostgreSqlEnumerationCumulativeByteItem] =
    items.filter(_.kind == kind)

  private def exactBytes(
      values: Vector[PostgreSqlEnumerationCumulativeByteItem]
  ): Long = {
    val exact = values.iterator.foldLeft(BigInt(0))((sum, item) =>
      sum + BigInt(item.byteLength)
    )
    require(exact.isValidLong, "successful cumulative usage must fit Long")
    exact.longValue
  }

  private val source = selected(SourceSql)
  private val generated = selected(GeneratedSql)
  private val stages = selected(StageSql)
  private val documents = selected(CanonicalJsonDocument)
  private val sql = source ++ generated ++ stages

  require(
    items.map(_.kind) ==
      Vector.fill(source.size)(SourceSql) ++
        Vector.fill(generated.size)(GeneratedSql) ++
        Vector.fill(stages.size)(StageSql) ++
        Vector.fill(documents.size)(CanonicalJsonDocument),
    "cumulative items must retain source/generated/stage/document order"
  )

  Vector(source, generated, stages, documents).foreach { category =>
    require(
      category.map(_.ordinal) == (1 to category.size).toVector,
      "cumulative item ordinals must be contiguous within each kind"
    )
    require(
      category.map(_.logicalId).distinct.size == category.size,
      "cumulative item logical IDs must be unique within each kind"
    )
  }
  require(source.size == 1, "successful cumulative evidence has one source SQL")
  require(
    usage == PostgreSqlEnumerationCumulativeByteUsage(
      source.size,
      exactBytes(source),
      generated.size,
      exactBytes(generated),
      stages.size,
      exactBytes(stages),
      sql.size,
      exactBytes(sql),
      documents.size,
      exactBytes(documents)
    ),
    "cumulative evidence usage must replay from the exact item transcript"
  )
  require(
    usage.sourceSqlBytes <= limits.maxSourceSqlBytes &&
      usage.generatedSqlBytes <= limits.maxGeneratedSqlBytes &&
      usage.stageSqlBytes <= limits.maxStageSqlBytes &&
      usage.allSqlBytes <= limits.maxAllSqlBytes &&
      usage.canonicalJsonDocumentBytes <=
        limits.maxCanonicalJsonDocumentBytes,
    "successful cumulative evidence must remain within every ceiling"
  )
}

/** One projection admitted together with its exact cumulative evidence. */
private[yannakakis] final case class PostgreSqlEnumerationCumulativeProjection(
    projection: PostgreSqlEnumerationCampaignV2Projection,
    evidence: PostgreSqlEnumerationCumulativeBudgetEvidence
) extends Serializable {
  require(projection != null, "bounded cumulative projection must not be null")
  require(evidence != null, "cumulative projection evidence must not be null")
}

/** Exact post-composition admission retained by the acquisition CLI. */
private[yannakakis] final case class PostgreSqlEnumerationFinalPackageBudgetEvidence(
    preSeal: PostgreSqlEnumerationCumulativeBudgetEvidence,
    finalPackageBytes: Long,
    totalCanonicalJsonDocumentCount: Int,
    totalCanonicalJsonBytes: Long
) extends Serializable {
  require(preSeal != null, "pre-seal cumulative evidence must not be null")
  require(finalPackageBytes > 0L, "final package bytes must be positive")
  require(
    totalCanonicalJsonDocumentCount ==
      preSeal.usage.canonicalJsonDocumentCount + 1,
    "final cumulative JSON count must add exactly one package"
  )
  require(
    BigInt(totalCanonicalJsonBytes) ==
      BigInt(preSeal.usage.canonicalJsonDocumentBytes) +
        BigInt(finalPackageBytes),
    "final cumulative JSON bytes must be exact"
  )
  require(
    totalCanonicalJsonBytes <= preSeal.limits.maxCanonicalJsonDocumentBytes,
    "final cumulative JSON bytes must remain within the ceiling"
  )
}

/** Exact post-composition admission for package, search input, and commit. */
private[yannakakis] final case class PostgreSqlEnumerationFinalBundleBudgetEvidence(
    preSeal: PostgreSqlEnumerationCumulativeBudgetEvidence,
    finalPackageBytes: Long,
    searchInputBytes: Long,
    completeRecordBytes: Long,
    totalCanonicalJsonDocumentCount: Int,
    totalCanonicalJsonBytes: Long
) extends Serializable {
  require(preSeal != null, "pre-seal cumulative evidence must not be null")
  require(
    Vector(finalPackageBytes, searchInputBytes, completeRecordBytes).forall(_ > 0L),
    "final bundle documents must have positive byte lengths"
  )
  require(
    totalCanonicalJsonDocumentCount ==
      preSeal.usage.canonicalJsonDocumentCount + 3,
    "final cumulative JSON count must add the three bundle documents"
  )
  require(
    BigInt(totalCanonicalJsonBytes) ==
      BigInt(preSeal.usage.canonicalJsonDocumentBytes) +
        BigInt(finalPackageBytes) + BigInt(searchInputBytes) +
        BigInt(completeRecordBytes),
    "final bundle cumulative JSON bytes must be exact"
  )
  require(
    totalCanonicalJsonBytes <= preSeal.limits.maxCanonicalJsonDocumentBytes,
    "final bundle cumulative JSON bytes must remain within the ceiling"
  )
}

/** Overflow-safe primitive used by the cumulative boundary and its hostiles. */
private[cli] object PostgreSqlEnumerationCumulativeByteCounter {
  final case class Exceeded(maximum: Long, attempted: BigInt) {
    require(maximum >= 0L, "byte-counter maximum must be nonnegative")
    require(
      attempted != null && attempted > BigInt(maximum),
      "byte-counter attempted sum must exceed its maximum"
    )
  }

  def add(
      current: Long,
      increment: Long,
      maximum: Long
  ): Either[Exceeded, Long] = {
    require(current >= 0L, "current byte count must be nonnegative")
    require(increment >= 0L, "byte increment must be nonnegative")
    require(maximum >= 0L, "byte maximum must be nonnegative")
    require(current <= maximum, "current byte count must not exceed maximum")
    if (increment > maximum - current)
      Left(Exceeded(maximum, BigInt(current) + BigInt(increment)))
    else Right(current + increment)
  }
}

/**
  * Producer-wide cumulative byte gate for the outcome-free v2 projection.
  *
  * Raw SQL is charged before canonical documents are constructed. Canonical
  * documents are then charged in one fixed package order before publication.
  * Per-document construction remains independently exact-size-preflighted.
  */
private[yannakakis] object PostgreSqlEnumerationCampaignV2CumulativeBudget {
  import PostgreSqlEnumerationCumulativeBudgetFailure._
  import PostgreSqlEnumerationCumulativeByteResource._
  import PostgreSqlEnumerationCumulativeItemKind._

  val AccountingScheme: String =
    "enumeration-campaign-v2-cumulative-logical-utf8-bytes-v2"

  /**
    * Charge the exact composed package before validation and publication.
    * The package is one additional canonical JSON document; it was not part
    * of the earlier pre-seal logical-document inventory.
    */
  def finalizePackage(
      preSeal: PostgreSqlEnumerationCumulativeBudgetEvidence,
      finalPackageBytes: Long
  ): Either[
    PostgreSqlEnumerationCumulativeBudgetFailure,
    PostgreSqlEnumerationFinalPackageBudgetEvidence
  ] = {
    require(preSeal != null, "pre-seal cumulative evidence must not be null")
    require(finalPackageBytes > 0L, "final package bytes must be positive")
    val attempted = BigInt(preSeal.usage.canonicalJsonDocumentBytes) +
      BigInt(finalPackageBytes)
    if (attempted > BigInt(preSeal.limits.maxCanonicalJsonDocumentBytes))
      Left(
        FinalPackageLimitExceeded(
          preSeal.limits.maxCanonicalJsonDocumentBytes,
          attempted,
          preSeal.usage
        )
      )
    else
      Right(
        PostgreSqlEnumerationFinalPackageBudgetEvidence(
          preSeal,
          finalPackageBytes,
          preSeal.usage.canonicalJsonDocumentCount + 1,
          attempted.longValue
        )
      )
  }

  /**
    * Charge every externally published canonical document in a paired replay
    * bundle. This is additive to, and does not change, the frozen v2 package
    * accounting returned by `finalizePackage`.
    */
  def finalizeSearchReplayBundle(
      preSeal: PostgreSqlEnumerationCumulativeBudgetEvidence,
      finalPackageBytes: Long,
      searchInputBytes: Long,
      completeRecordBytes: Long
  ): Either[
    PostgreSqlEnumerationCumulativeBudgetFailure,
    PostgreSqlEnumerationFinalBundleBudgetEvidence
  ] = {
    require(preSeal != null, "pre-seal cumulative evidence must not be null")
    require(
      Vector(finalPackageBytes, searchInputBytes, completeRecordBytes).forall(_ > 0L),
      "final bundle documents must have positive byte lengths"
    )
    val attempted = BigInt(preSeal.usage.canonicalJsonDocumentBytes) +
      BigInt(finalPackageBytes) + BigInt(searchInputBytes) +
      BigInt(completeRecordBytes)
    if (attempted > BigInt(preSeal.limits.maxCanonicalJsonDocumentBytes))
      Left(
        FinalPackageLimitExceeded(
          preSeal.limits.maxCanonicalJsonDocumentBytes,
          attempted,
          preSeal.usage
        )
      )
    else
      Right(
        PostgreSqlEnumerationFinalBundleBudgetEvidence(
          preSeal,
          finalPackageBytes,
          searchInputBytes,
          completeRecordBytes,
          preSeal.usage.canonicalJsonDocumentCount + 3,
          attempted.longValue
        )
      )
  }

  def project(
      artifact: PostgreSqlEnumerationBatchArtifact,
      authoringRequest: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      limits: PostgreSqlEnumerationCumulativeByteLimits =
        PostgreSqlEnumerationCumulativeByteLimits.default
  ): Either[
    PostgreSqlEnumerationCumulativeBudgetFailure,
    PostgreSqlEnumerationCumulativeProjection
  ] = {
    require(artifact != null, "enumeration artifact must not be null")
    require(authoringRequest != null, "authoring request must not be null")
    require(limits != null, "cumulative byte limits must not be null")
    val view = PostgreSqlEnumerationCampaignV2ProjectionView.from(artifact)
    accountSql(view, limits).flatMap { state =>
      val projected: Either[
        PostgreSqlEnumerationCumulativeBudgetFailure,
        PostgreSqlEnumerationCampaignV2Projection
      ] =
        try {
          Right(
            PostgreSqlEnumerationCampaignV2Json.project(
              artifact,
              authoringRequest
            )
          )
        } catch {
          case _: IllegalArgumentException =>
            Left(ProjectionRejected(state.usage))
        }
      projected.flatMap { projection =>
        accountDocuments(state, canonicalDocuments(projection)).map {
          evidence =>
            PostgreSqlEnumerationCumulativeProjection(projection, evidence)
        }
      }
    }
  }

  /** Account an already-built projection without regenerating its documents. */
  def account(
      artifact: PostgreSqlEnumerationBatchArtifact,
      projection: PostgreSqlEnumerationCampaignV2Projection,
      limits: PostgreSqlEnumerationCumulativeByteLimits =
        PostgreSqlEnumerationCumulativeByteLimits.default
  ): Either[
    PostgreSqlEnumerationCumulativeBudgetFailure,
    PostgreSqlEnumerationCumulativeBudgetEvidence
  ] = {
    require(artifact != null, "enumeration artifact must not be null")
    require(projection != null, "enumeration projection must not be null")
    require(limits != null, "cumulative byte limits must not be null")
    accountLogicalDocuments(
      artifact,
      canonicalDocuments(projection),
      limits
    )
  }

  /**
    * Account a caller's exact pre-seal assembly document inventory.
    *
    * Logical IDs are part of replay evidence and must be unique. This method
    * charges retained logical UTF-8 bytes only; it is deliberately not a JVM
    * heap, compiler-object-graph, allocation-peak, or process-RSS estimate.
    */
  def accountLogicalDocuments(
      artifact: PostgreSqlEnumerationBatchArtifact,
      documents: Vector[
        (String, PostgreSqlEnumerationCampaignV2JsonDocument)
      ],
      limits: PostgreSqlEnumerationCumulativeByteLimits =
        PostgreSqlEnumerationCumulativeByteLimits.default
  ): Either[
    PostgreSqlEnumerationCumulativeBudgetFailure,
    PostgreSqlEnumerationCumulativeBudgetEvidence
  ] = {
    require(artifact != null, "enumeration artifact must not be null")
    require(limits != null, "cumulative byte limits must not be null")
    val view = PostgreSqlEnumerationCampaignV2ProjectionView.from(artifact)
    accountSql(view, limits).flatMap { state =>
      val invalidReason =
        if (documents == null) Some("missing-inventory")
        else if (documents.isEmpty) Some("empty-inventory")
        else if (
          documents.exists(entry =>
            entry == null || entry._1 == null || entry._1.isEmpty ||
              entry._2 == null
          )
        ) Some("invalid-entry")
        else None
      invalidReason match {
        case Some(reason) =>
          Left(DocumentInventoryRejected(reason, state.usage))
        case None => accountDocuments(state, documents)
      }
    }
  }

  /** Exact source/document accounting for a typed native-only fallback. */
  def accountNativeFallbackLogicalDocuments(
      result: at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlEnumerationBatchResult,
      documents: Vector[
        (String, PostgreSqlEnumerationCampaignV2JsonDocument)
      ],
      limits: PostgreSqlEnumerationCumulativeByteLimits =
        PostgreSqlEnumerationCumulativeByteLimits.default
  ): Either[
    PostgreSqlEnumerationCumulativeBudgetFailure,
    PostgreSqlEnumerationCumulativeBudgetEvidence
  ] = {
    require(result != null, "enumeration fallback result must not be null")
    require(limits != null, "cumulative byte limits must not be null")
    val state = new AccountingState(limits)
    PostgreSqlEnumerationSqlTransport
      .validate(
        result.request.originalSql,
        PostgreSqlEnumerationTransportLimitEvidence.MaximumSourceSqlBytes,
        allowEmpty = false
      ) match {
      case Left(reason) =>
        Left(SqlTransportRejected(SourceSql, 1, reason.code, state.usage))
      case Right(byteLength) =>
        state.charge(
          PostgreSqlEnumerationCumulativeByteItem(
            SourceSql,
            1,
            "source",
            byteLength,
            result.request.originalSqlSha256
          )
        ) match {
          case Some(failure) => Left(failure)
          case None =>
            if (documents == null || documents.isEmpty)
              Left(DocumentInventoryRejected("empty-inventory", state.usage))
            else if (
              documents.exists(entry =>
                entry == null || entry._1 == null || entry._1.isEmpty ||
                  entry._2 == null
              )
            ) Left(DocumentInventoryRejected("invalid-entry", state.usage))
            else accountDocuments(state, documents)
        }
    }
  }

  private def accountSql(
      view: PostgreSqlEnumerationCampaignV2ProjectionView.Artifact,
      limits: PostgreSqlEnumerationCumulativeByteLimits
  ): Either[
    PostgreSqlEnumerationCumulativeBudgetFailure,
    AccountingState
  ] = {
    require(view != null, "enumeration projection view must not be null")
    require(limits != null, "cumulative byte limits must not be null")
    val state = new AccountingState(limits)

    def chargeSql(
        kind: PostgreSqlEnumerationCumulativeItemKind,
        ordinal: Int,
        logicalId: String,
        sql: String,
        sha256: String,
        maximumItemBytes: Long,
        allowEmpty: Boolean
    ): Option[PostgreSqlEnumerationCumulativeBudgetFailure] = {
      PostgreSqlEnumerationSqlTransport
        .validate(sql, maximumItemBytes, allowEmpty) match {
        case Left(reason) =>
          Some(SqlTransportRejected(kind, ordinal, reason.code, state.usage))
        case Right(byteLength) =>
          state.charge(
            PostgreSqlEnumerationCumulativeByteItem(
              kind,
              ordinal,
              logicalId,
              byteLength,
              sha256
            )
          )
      }
    }

    chargeSql(
      SourceSql,
      ordinal = 1,
      logicalId = "source",
      view.sourceSql,
      view.sourceSqlSha256,
      PostgreSqlEnumerationTransportLimitEvidence.MaximumSourceSqlBytes,
      allowEmpty = false
    ) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }

    var candidateIndex = 0
    while (candidateIndex < view.candidates.size) {
      val candidate = view.candidates(candidateIndex)
      val ordinal = candidateIndex + 1
      val actionId = f"rewrite-${candidate.structuralRank}%04d"
      chargeSql(
        GeneratedSql,
        ordinal,
        actionId,
        candidate.renderedSql,
        candidate.renderedSqlSha256,
        PostgreSqlEnumerationTransportLimitEvidence
          .MaximumGeneratedOrStageSqlBytes,
        allowEmpty = false
      ) match {
        case Some(failure) => return Left(failure)
        case None          => ()
      }
      candidateIndex += 1
    }

    var stageOrdinal = 0
    candidateIndex = 0
    while (candidateIndex < view.candidates.size) {
      val candidate = view.candidates(candidateIndex)
      val actionId = f"rewrite-${candidate.structuralRank}%04d"
      var stageIndex = 0
      while (stageIndex < candidate.orderedStages.size) {
        val stage = candidate.orderedStages(stageIndex)
        stageOrdinal += 1
        chargeSql(
          StageSql,
          stageOrdinal,
          f"$actionId/stage-${stageIndex + 1}%04d:${stage.stageId}",
          stage.sql,
          stage.statementSha256,
          PostgreSqlEnumerationTransportLimitEvidence
            .MaximumGeneratedOrStageSqlBytes,
          allowEmpty = false
        ) match {
          case Some(failure) => return Left(failure)
          case None          => ()
        }
        stageIndex += 1
      }
      candidateIndex += 1
    }
    Right(state)
  }

  private def accountDocuments(
      state: AccountingState,
      documents: Vector[
        (String, PostgreSqlEnumerationCampaignV2JsonDocument)
      ]
  ): Either[
    PostgreSqlEnumerationCumulativeBudgetFailure,
    PostgreSqlEnumerationCumulativeBudgetEvidence
  ] = {
    require(state != null, "cumulative accounting state must not be null")
    if (documents.map(_._1).distinct.size != documents.size)
      return Left(
        DocumentInventoryRejected("duplicate-logical-id", state.usage)
      )
    var index = 0
    while (index < documents.size) {
      val (logicalId, document) = documents(index)
      state.charge(
        PostgreSqlEnumerationCumulativeByteItem(
          CanonicalJsonDocument,
          index + 1,
          logicalId,
          document.byteLength.toLong,
          document.sha256
        )
      ) match {
        case Some(failure) => return Left(failure)
        case None          => ()
      }
      index += 1
    }
    Right(
      PostgreSqlEnumerationCumulativeBudgetEvidence(
        AccountingScheme,
        state.limits,
        state.usage,
        state.items
      )
    )
  }

  private def canonicalDocuments(
      projection: PostgreSqlEnumerationCampaignV2Projection
  ): Vector[(String, PostgreSqlEnumerationCampaignV2JsonDocument)] = {
    val documents = Vector.newBuilder[
      (String, PostgreSqlEnumerationCampaignV2JsonDocument)
    ]
    documents += "source-sql" -> projection.sourceSql
    documents += "semantic-search-core" -> projection.semanticSearchCore
    documents += "producer-search-contract" -> projection.producerSearchContract
    documents +=
      "producer-search-transcript" -> projection.producerSearchTranscript
    projection.canonicalDecompositions.zipWithIndex.foreach {
      case (document, index) =>
        documents += f"canonical-decomposition/rewrite-${index + 1}%04d" ->
          document
    }
    documents += "feature-registry" -> projection.featureRegistry
    projection.featureValueBundles.foreach { bundle =>
      documents += s"feature-values/${bundle.actionId}" -> bundle.document
    }
    projection.actions.foreach { action =>
      documents += s"action-sql/${action.actionId}" -> action.renderedSql
      documents += s"ordered-stages/${action.actionId}" -> action.orderedStages
    }
    documents.result()
  }

  private final class AccountingState(
      val limits: PostgreSqlEnumerationCumulativeByteLimits
  ) {
    private var accepted = Vector.empty[PostgreSqlEnumerationCumulativeByteItem]
    private var sourceBytes = 0L
    private var generatedBytes = 0L
    private var stageBytes = 0L
    private var allSqlBytes = 0L
    private var documentBytes = 0L

    def items: Vector[PostgreSqlEnumerationCumulativeByteItem] = accepted

    def usage: PostgreSqlEnumerationCumulativeByteUsage = {
      val sourceCount = accepted.count(_.kind == SourceSql)
      val generatedCount = accepted.count(_.kind == GeneratedSql)
      val stageCount = accepted.count(_.kind == StageSql)
      val documentCount = accepted.count(_.kind == CanonicalJsonDocument)
      PostgreSqlEnumerationCumulativeByteUsage(
        sourceCount,
        sourceBytes,
        generatedCount,
        generatedBytes,
        stageCount,
        stageBytes,
        sourceCount + generatedCount + stageCount,
        allSqlBytes,
        documentCount,
        documentBytes
      )
    }

    def charge(
        item: PostgreSqlEnumerationCumulativeByteItem
    ): Option[PostgreSqlEnumerationCumulativeBudgetFailure] = {
      require(item != null, "charged cumulative item must not be null")
      val (resource, current, maximum) = item.kind match {
        case SourceSql =>
          (SourceSqlBytes, sourceBytes, limits.maxSourceSqlBytes)
        case GeneratedSql =>
          (GeneratedSqlBytes, generatedBytes, limits.maxGeneratedSqlBytes)
        case StageSql =>
          (StageSqlBytes, stageBytes, limits.maxStageSqlBytes)
        case CanonicalJsonDocument =>
          (
            CanonicalJsonDocumentBytes,
            documentBytes,
            limits.maxCanonicalJsonDocumentBytes
          )
      }

      PostgreSqlEnumerationCumulativeByteCounter
        .add(current, item.byteLength, maximum) match {
        case Left(exceeded) =>
          return Some(
            LimitExceeded(
              resource,
              exceeded.maximum,
              exceeded.attempted,
              item,
              usage
            )
          )
        case Right(_) => ()
      }

      if (item.kind != CanonicalJsonDocument) {
        PostgreSqlEnumerationCumulativeByteCounter
          .add(allSqlBytes, item.byteLength, limits.maxAllSqlBytes) match {
          case Left(exceeded) =>
            return Some(
              LimitExceeded(
                AllSqlBytes,
                exceeded.maximum,
                exceeded.attempted,
                item,
                usage
              )
            )
          case Right(next) => allSqlBytes = next
        }
      }

      item.kind match {
        case SourceSql         => sourceBytes += item.byteLength
        case GeneratedSql      => generatedBytes += item.byteLength
        case StageSql          => stageBytes += item.byteLength
        case CanonicalJsonDocument => documentBytes += item.byteLength
      }
      accepted :+= item
      None
    }
  }
}
