package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Structural schema retained at an aggregate factor boundary. */
private[yannakakis] sealed trait PostgreSqlFactorBoundaryProjectionProfile
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlFactorBoundaryProjectionProfile {
  /** Preserve the factor-boundary behavior selected by the frozen v1 base. */
  case object FullFactorVerticesV1
      extends PostgreSqlFactorBoundaryProjectionProfile {
    override val id: String = "full-factor-vertices-v1"
  }

  /** Retain only the independently derived structural consumer demand. */
  case object DemandedStructuralColumnsV2
      extends PostgreSqlFactorBoundaryProjectionProfile {
    override val id: String = "demanded-structural-columns-v2"
  }

  val Values: Vector[PostgreSqlFactorBoundaryProjectionProfile] = Vector(
    FullFactorVerticesV1,
    DemandedStructuralColumnsV2
  )
}

/** Physical boundary used for one exact aggregate leaf message. */
private[yannakakis] sealed trait PostgreSqlAggregateLeafMessageProfile
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlAggregateLeafMessageProfile {
  /** Preserve the frozen v1 derived-SELECT message shape. */
  case object DerivedSelectV1 extends PostgreSqlAggregateLeafMessageProfile {
    override val id: String = "derived-select-v1"
  }

  /** Elide only a proved identity projection over exact compact factor state. */
  case object ExactDirectFactorV2
      extends PostgreSqlAggregateLeafMessageProfile {
    override val id: String = "exact-direct-factor-v2"
  }

  val Values: Vector[PostgreSqlAggregateLeafMessageProfile] = Vector(
    DerivedSelectV1,
    ExactDirectFactorV2
  )
}

/**
  * Additive development-only renderer-v2 profile.
  *
  * This value wraps one exact frozen v1 aggregate-rendering profile rather
  * than extending or reinterpreting its closed three-axis product. Its two
  * additional axes cover only factor-boundary structural demand and exact
  * leaf-message elision. Existing v1 profiles, identities, defaults, public
  * requests, and rewrite-artifact schemas remain unchanged.
  */
private[yannakakis] final class PostgreSqlAggregateRenderingProfileV2 private (
    val baseProfile: PostgreSqlAggregateRenderingProfile,
    val factorBoundaryProjection: PostgreSqlFactorBoundaryProjectionProfile,
    val leafMessage: PostgreSqlAggregateLeafMessageProfile
) extends Serializable {
  def id: String = PostgreSqlAggregateRenderingProfileV2.profileId(this)

  def axisIds: Vector[String] = Vector(
    factorBoundaryProjection.id,
    leafMessage.id
  )

  /** True exactly when this profile requests at least one renderer-v2 change. */
  def usesOptimizedAxes: Boolean =
    factorBoundaryProjection !=
      PostgreSqlFactorBoundaryProjectionProfile.FullFactorVerticesV1 ||
      leafMessage != PostgreSqlAggregateLeafMessageProfile.DerivedSelectV1

  /** The explicit byte-compatible wrapper around the frozen v1 base profile. */
  def isBaseline: Boolean = !usesOptimizedAxes

  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlAggregateRenderingProfileV2 =>
      baseProfile == that.baseProfile &&
        factorBoundaryProjection == that.factorBoundaryProjection &&
        leafMessage == that.leafMessage
    case _ => false
  }

  override def hashCode(): Int =
    (baseProfile, factorBoundaryProjection, leafMessage).hashCode()

  override def toString: String =
    s"PostgreSqlAggregateRenderingProfileV2(${baseProfile.id},${axisIds.mkString(",")})"
}

private[yannakakis] object PostgreSqlAggregateRenderingProfileV2 {
  val Scheme: String = "postgresql_aggregate_rendering_profile_v2"

  final case class ValidationError private[postgresql] (
      field: String,
      detail: String
  ) extends Product
      with Serializable {
    require(field != null && field.nonEmpty, "profile error field must not be blank")
    require(detail != null && detail.nonEmpty, "profile error detail must not be blank")
    val code: String = s"aggregate-rendering-profile-v2.invalid.$field"
  }

  /**
    * Constructs one exact v2 profile and rejects combinations that cannot
    * satisfy the design-frozen compact-state preconditions.
    */
  def create(
      baseProfile: PostgreSqlAggregateRenderingProfile,
      factorBoundaryProjection: PostgreSqlFactorBoundaryProjectionProfile,
      leafMessage: PostgreSqlAggregateLeafMessageProfile
  ): Either[Vector[ValidationError], PostgreSqlAggregateRenderingProfileV2] = {
    val errors = validateFields(
      baseProfile,
      factorBoundaryProjection,
      leafMessage
    )
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new PostgreSqlAggregateRenderingProfileV2(
          baseProfile,
          factorBoundaryProjection,
          leafMessage
        )
      )
  }

  /** The byte-compatible v1 behavior wrapped in an explicit v2 identity. */
  def baseline(
      baseProfile: PostgreSqlAggregateRenderingProfile
  ): Either[Vector[ValidationError], PostgreSqlAggregateRenderingProfileV2] =
    create(
      baseProfile,
      PostgreSqlFactorBoundaryProjectionProfile.FullFactorVerticesV1,
      PostgreSqlAggregateLeafMessageProfile.DerivedSelectV1
    )

  /** Closed compatible product for one exact frozen v1 base profile. */
  def valuesFor(
      baseProfile: PostgreSqlAggregateRenderingProfile
  ): Vector[PostgreSqlAggregateRenderingProfileV2] =
    for {
      factorBoundaryProjection <- PostgreSqlFactorBoundaryProjectionProfile.Values
      leafMessage <- PostgreSqlAggregateLeafMessageProfile.Values
      profile <- create(
        baseProfile,
        factorBoundaryProjection,
        leafMessage
      ).toOption
    } yield profile

  /** Closed compatible product over every frozen v1 base profile. */
  lazy val Values: Vector[PostgreSqlAggregateRenderingProfileV2] =
    PostgreSqlAggregateRenderingProfile.Values.flatMap(valuesFor)

  def validate(
      profile: PostgreSqlAggregateRenderingProfileV2
  ): Vector[ValidationError] =
    if (profile == null)
      Vector(ValidationError("profile", "profile must not be null"))
    else
      validateFields(
        profile.baseProfile,
        profile.factorBoundaryProjection,
        profile.leafMessage
      )

  private[yannakakis] def canonicalPreimage(
      profile: PostgreSqlAggregateRenderingProfileV2
  ): String = {
    val errors = validate(profile)
    require(
      errors.isEmpty,
      errors.map(_.code).mkString("invalid aggregate rendering profile v2: ", ",", "")
    )
    Vector(
      Scheme,
      PostgreSqlAggregateRenderingProfile.Scheme,
      profile.baseProfile.id
    ).iterator
      .concat(profile.baseProfile.axisIds.iterator)
      .concat(profile.axisIds.iterator)
      .map(lengthPrefix)
      .mkString
  }

  private def profileId(
      profile: PostgreSqlAggregateRenderingProfileV2
  ): String = "sha256:" + sha256(canonicalPreimage(profile))

  private def validateFields(
      baseProfile: PostgreSqlAggregateRenderingProfile,
      factorBoundaryProjection: PostgreSqlFactorBoundaryProjectionProfile,
      leafMessage: PostgreSqlAggregateLeafMessageProfile
  ): Vector[ValidationError] = {
    val baseErrors = PostgreSqlAggregateRenderingProfile.validate(baseProfile)
    val validBase = baseErrors.isEmpty
    val compactBase =
      validBase &&
        baseProfile.reductionFinalization ==
          PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2

    Vector(
      Option.when(!validBase)(
        ValidationError(
          "base-profile",
          "base profile must be one exact valid frozen v1 profile"
        )
      ),
      Option.when(
        factorBoundaryProjection == null ||
          !PostgreSqlFactorBoundaryProjectionProfile.Values.contains(
            factorBoundaryProjection
          )
      )(
        ValidationError(
          "factor-boundary-projection",
          "factor boundary projection must be one exact registered value"
        )
      ),
      Option.when(
        leafMessage == null ||
          !PostgreSqlAggregateLeafMessageProfile.Values.contains(leafMessage)
      )(
        ValidationError(
          "leaf-message",
          "leaf message must be one exact registered value"
        )
      ),
      Option.when(
        validBase &&
        factorBoundaryProjection ==
          PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2 &&
          !compactBase
      )(
        ValidationError(
          "factor-boundary-projection-compatibility",
          "demanded structural columns require compact-local-state-v2"
        )
      ),
      Option.when(
        validBase &&
        leafMessage ==
          PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2 &&
          !compactBase
      )(
        ValidationError(
          "leaf-message-compatibility",
          "exact direct leaf messages require compact-local-state-v2"
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
