package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

/** Physical source used by the scalar COUNT message program. */
private[yannakakis] sealed trait PostgreSqlGlobalCountMessageRealizationProfile
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlGlobalCountMessageRealizationProfile {
  /** Preserve the exact renderer-v2 terminal and reduced-factor inputs. */
  case object ReducedFactorMessagesV1
      extends PostgreSqlGlobalCountMessageRealizationProfile {
    override val id: String = "reduced-factor-messages-v1"
  }

  /**
    * Evaluate leaf-to-root messages directly over raw initial factors.
    *
    * Each non-root message joins all child messages to its raw factor before
    * grouping by the parent separator. No Yannakakis reduction CTE is part of
    * this physical realization.
    */
  case object FusedRawFactorMessagesV1
      extends PostgreSqlGlobalCountMessageRealizationProfile {
    override val id: String = "fused-raw-factor-messages-v1"
  }

  val Values: Vector[PostgreSqlGlobalCountMessageRealizationProfile] = Vector(
    ReducedFactorMessagesV1,
    FusedRawFactorMessagesV1
  )
}

/**
  * Additive development-only aggregate renderer-v3 profile.
  *
  * V3 wraps one exact renderer-v2 profile and adds a scalar-global-COUNT
  * message-realization axis. Existing v1/v2 entry points always request the
  * baseline realization and therefore retain their SQL bytes and identities.
  */
private[yannakakis] final class PostgreSqlAggregateRenderingProfileV3 private (
    val baseProfile: PostgreSqlAggregateRenderingProfileV2,
    val globalCountMessageRealization
        : PostgreSqlGlobalCountMessageRealizationProfile
) extends Serializable {
  def id: String = PostgreSqlAggregateRenderingProfileV3.profileId(this)

  def axisIds: Vector[String] = Vector(globalCountMessageRealization.id)

  def usesFusedRawFactorMessages: Boolean =
    globalCountMessageRealization ==
      PostgreSqlGlobalCountMessageRealizationProfile.FusedRawFactorMessagesV1

  def isBaseline: Boolean = !usesFusedRawFactorMessages

  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlAggregateRenderingProfileV3 =>
      baseProfile == that.baseProfile &&
        globalCountMessageRealization == that.globalCountMessageRealization
    case _ => false
  }

  override def hashCode(): Int =
    (baseProfile, globalCountMessageRealization).hashCode()

  override def toString: String =
    s"PostgreSqlAggregateRenderingProfileV3(${baseProfile.id},${axisIds.mkString(",")})"
}

private[yannakakis] object PostgreSqlAggregateRenderingProfileV3 {
  val Scheme: String = "postgresql_aggregate_rendering_profile_v3"

  final case class ValidationError private[postgresql] (
      field: String,
      detail: String
  ) extends Product
      with Serializable {
    require(field != null && field.nonEmpty, "profile error field must not be blank")
    require(detail != null && detail.nonEmpty, "profile error detail must not be blank")
    val code: String = s"aggregate-rendering-profile-v3.invalid.$field"
  }

  def create(
      baseProfile: PostgreSqlAggregateRenderingProfileV2,
      globalCountMessageRealization
          : PostgreSqlGlobalCountMessageRealizationProfile
  ): Either[Vector[ValidationError], PostgreSqlAggregateRenderingProfileV3] = {
    val errors = validateFields(baseProfile, globalCountMessageRealization)
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new PostgreSqlAggregateRenderingProfileV3(
          baseProfile,
          globalCountMessageRealization
        )
      )
  }

  /** Byte-compatible wrapper used by every pre-v3 renderer entry point. */
  def baseline(
      baseProfile: PostgreSqlAggregateRenderingProfileV2
  ): Either[Vector[ValidationError], PostgreSqlAggregateRenderingProfileV3] =
    create(
      baseProfile,
      PostgreSqlGlobalCountMessageRealizationProfile.ReducedFactorMessagesV1
    )

  /** The deliberately narrow development-only fused scalar-COUNT profile. */
  def fusedRawFactorMessages(
      baseProfile: PostgreSqlAggregateRenderingProfileV2
  ): Either[Vector[ValidationError], PostgreSqlAggregateRenderingProfileV3] =
    create(
      baseProfile,
      PostgreSqlGlobalCountMessageRealizationProfile.FusedRawFactorMessagesV1
    )

  def validate(
      profile: PostgreSqlAggregateRenderingProfileV3
  ): Vector[ValidationError] =
    if (profile == null)
      Vector(ValidationError("profile", "profile must not be null"))
    else
      validateFields(
        profile.baseProfile,
        profile.globalCountMessageRealization
      )

  private[yannakakis] def canonicalPreimage(
      profile: PostgreSqlAggregateRenderingProfileV3
  ): String = {
    val errors = validate(profile)
    require(
      errors.isEmpty,
      errors.map(_.code).mkString("invalid aggregate rendering profile v3: ", ",", "")
    )
    Vector(
      Scheme,
      PostgreSqlAggregateRenderingProfileV2.Scheme,
      profile.baseProfile.id
    ).iterator
      .concat(profile.baseProfile.axisIds.iterator)
      .concat(profile.axisIds.iterator)
      .map(lengthPrefix)
      .mkString
  }

  private def profileId(
      profile: PostgreSqlAggregateRenderingProfileV3
  ): String = "sha256:" + sha256(canonicalPreimage(profile))

  private def validateFields(
      baseProfile: PostgreSqlAggregateRenderingProfileV2,
      globalCountMessageRealization
          : PostgreSqlGlobalCountMessageRealizationProfile
  ): Vector[ValidationError] = {
    val baseErrors = PostgreSqlAggregateRenderingProfileV2.validate(baseProfile)
    val validBase = baseErrors.isEmpty
    val fused =
      globalCountMessageRealization ==
        PostgreSqlGlobalCountMessageRealizationProfile.FusedRawFactorMessagesV1

    Vector(
      Option.when(!validBase)(
        ValidationError(
          "base-profile",
          "base profile must be one exact valid renderer-v2 profile"
        )
      ),
      Option.when(
        globalCountMessageRealization == null ||
          !PostgreSqlGlobalCountMessageRealizationProfile.Values.contains(
            globalCountMessageRealization
          )
      )(
        ValidationError(
          "global-count-message-realization",
          "global COUNT message realization must be one exact registered value"
        )
      ),
      Option.when(
        fused && validBase &&
          (!baseProfile.isBaseline ||
            baseProfile.baseProfile !=
              PostgreSqlAggregateRenderingProfile.LegacyV1)
      )(
        ValidationError(
          "fused-raw-factor-compatibility",
          "fused raw-factor messages require the exact legacy-v1 renderer wrapped by the renderer-v2 baseline"
        )
      )
    ).flatten
  }

  private def lengthPrefix(value: String): String = {
    val size = value.getBytes(StandardCharsets.UTF_8).length
    s"$size:$value"
  }

  private def sha256(value: String): String = {
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
    bytes.map(byte => f"${byte & 0xff}%02x").mkString
  }
}

/** Compiler-private scope gate for the additive V3 realization. */
private[postgresql] object PostgreSqlAggregateRendererV3Compiler {
  def validateScope(
      reductionMode: YannakakisReductionMode,
      terminal: CompiledPostgreSqlTerminal,
      profile: PostgreSqlAggregateRenderingProfileV3,
      enumerationProfile: PostgreSqlEnumerationRenderingProfile
  ): Vector[PostgreSqlRenderingError] = {
    def reject(detail: String) = Vector(
      PostgreSqlRenderingError.ModelInvariantViolation(detail)
    )

    if (profile.isBaseline) Vector.empty
    else if (reductionMode != YannakakisReductionMode.NoReduction)
      reject("fused raw-factor COUNT messages require no-reduction mode")
    else if (
      enumerationProfile !=
        PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
    )
      reject(
        "fused raw-factor COUNT messages require statistics-safe-v2 enumeration SQL"
      )
    else
      terminal match {
        case CompiledPostgreSqlTerminal.GlobalCountStar(
              _,
              _: CompiledPostgreSqlTerminal.GlobalCountStrategy.RawFrequencyPropagation
            ) => Vector.empty
        case _ =>
          reject(
            "fused raw-factor messages require scalar global COUNT(*) frequency propagation"
          )
      }
  }
}
