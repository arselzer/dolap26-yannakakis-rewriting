package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Shape of the terminal aggregate for an exact one-bag plan. */
private[yannakakis] sealed trait PostgreSqlAggregateOneBagTerminalProfile
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlAggregateOneBagTerminalProfile {
  case object LegacyNestedV1
      extends PostgreSqlAggregateOneBagTerminalProfile {
    override val id: String = "legacy-nested-v1"
  }

  case object CollapsedDirectV2
      extends PostgreSqlAggregateOneBagTerminalProfile {
    override val id: String = "collapsed-direct-v2"
  }

  val Values: Vector[PostgreSqlAggregateOneBagTerminalProfile] = Vector(
    LegacyNestedV1,
    CollapsedDirectV2
  )
}

/** Schema emitted when a reduction first acquires an exact frequency. */
private[yannakakis] sealed trait PostgreSqlAggregateReductionFinalizationProfile
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlAggregateReductionFinalizationProfile {
  case object LegacyFullVerticesV1
      extends PostgreSqlAggregateReductionFinalizationProfile {
    override val id: String = "legacy-full-vertices-v1"
  }

  case object CompactLocalStateV2
      extends PostgreSqlAggregateReductionFinalizationProfile {
    override val id: String = "compact-local-state-v2"
  }

  val Values: Vector[PostgreSqlAggregateReductionFinalizationProfile] = Vector(
    LegacyFullVerticesV1,
    CompactLocalStateV2
  )
}

/** Set-valued source shape used inside one reduction EXISTS predicate. */
private[yannakakis] sealed trait PostgreSqlReductionSupportProfile
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlReductionSupportProfile {
  case object DistinctSeparatorSupportV1
      extends PostgreSqlReductionSupportProfile {
    override val id: String = "distinct-separator-support-v1"
  }

  case object DirectExistsV2 extends PostgreSqlReductionSupportProfile {
    override val id: String = "direct-exists-v2"
  }

  val Values: Vector[PostgreSqlReductionSupportProfile] = Vector(
    DistinctSeparatorSupportV1,
    DirectExistsV2
  )
}

/**
  * Development-only exact generated-SQL shape.
  *
  * This profile is deliberately separate from candidate selection, the
  * frozen v1/v2 physical-variant identities, and rewrite-artifact schemas
  * v1--v7. Existing renderer entry points use [[LegacyV1]]. A later explicit
  * benchmark boundary may opt into another member of the closed eight-profile
  * product without silently changing legacy SQL bytes.
  */
private[yannakakis] sealed trait PostgreSqlAggregateRenderingProfile
    extends Serializable {
  def oneBagTerminal: PostgreSqlAggregateOneBagTerminalProfile
  def reductionFinalization: PostgreSqlAggregateReductionFinalizationProfile
  def reductionSupport: PostgreSqlReductionSupportProfile

  final def id: String =
    PostgreSqlAggregateRenderingProfile.profileId(this)

  final def axisIds: Vector[String] = Vector(
    oneBagTerminal.id,
    reductionFinalization.id,
    reductionSupport.id
  )
}

private[yannakakis] object PostgreSqlAggregateRenderingProfile {
  val Scheme: String = "postgresql_aggregate_rendering_profile_v1"

  final case class ValidationError private[postgresql] (
      field: String,
      detail: String
  ) extends Product
      with Serializable {
    require(field != null && field.nonEmpty, "profile error field must not be blank")
    require(detail != null && detail.nonEmpty, "profile error detail must not be blank")
    val code: String = s"aggregate-rendering-profile.invalid.$field"
  }

  case object LegacyV1 extends PostgreSqlAggregateRenderingProfile {
    override val oneBagTerminal: PostgreSqlAggregateOneBagTerminalProfile =
      PostgreSqlAggregateOneBagTerminalProfile.LegacyNestedV1
    override val reductionFinalization
        : PostgreSqlAggregateReductionFinalizationProfile =
      PostgreSqlAggregateReductionFinalizationProfile.LegacyFullVerticesV1
    override val reductionSupport: PostgreSqlReductionSupportProfile =
      PostgreSqlReductionSupportProfile.DistinctSeparatorSupportV1
  }

  final class ExactV1 private[PostgreSqlAggregateRenderingProfile] (
      override val oneBagTerminal: PostgreSqlAggregateOneBagTerminalProfile,
      override val reductionFinalization
          : PostgreSqlAggregateReductionFinalizationProfile,
      override val reductionSupport: PostgreSqlReductionSupportProfile
  ) extends PostgreSqlAggregateRenderingProfile {
    override def equals(other: Any): Boolean = other match {
      case that: ExactV1 =>
        oneBagTerminal == that.oneBagTerminal &&
          reductionFinalization == that.reductionFinalization &&
          reductionSupport == that.reductionSupport
      case _ => false
    }

    override def hashCode(): Int =
      (oneBagTerminal, reductionFinalization, reductionSupport).hashCode()

    override def toString: String =
      s"PostgreSqlAggregateRenderingProfile.ExactV1(${axisIds.mkString(",")})"
  }

  /** Validates all three closed axes and canonicalizes the legacy triple. */
  def create(
      oneBagTerminal: PostgreSqlAggregateOneBagTerminalProfile,
      reductionFinalization: PostgreSqlAggregateReductionFinalizationProfile,
      reductionSupport: PostgreSqlReductionSupportProfile
  ): Either[Vector[ValidationError], PostgreSqlAggregateRenderingProfile] = {
    val errors = validateAxes(
      oneBagTerminal,
      reductionFinalization,
      reductionSupport
    )
    if (errors.nonEmpty) Left(errors)
    else if (
      oneBagTerminal == LegacyV1.oneBagTerminal &&
      reductionFinalization == LegacyV1.reductionFinalization &&
      reductionSupport == LegacyV1.reductionSupport
    ) Right(LegacyV1)
    else
      Right(
        new ExactV1(
          oneBagTerminal,
          reductionFinalization,
          reductionSupport
        )
      )
  }

  /** Closed Cartesian product in canonical axis order. */
  lazy val Values: Vector[PostgreSqlAggregateRenderingProfile] =
    for {
      terminal <- PostgreSqlAggregateOneBagTerminalProfile.Values
      finalization <- PostgreSqlAggregateReductionFinalizationProfile.Values
      support <- PostgreSqlReductionSupportProfile.Values
    } yield
      create(terminal, finalization, support).fold(
        problems =>
          throw new IllegalStateException(
            problems.map(_.code).mkString("invalid built-in profile: ", ",", "")
          ),
        identity
      )

  def validate(
      profile: PostgreSqlAggregateRenderingProfile
  ): Vector[ValidationError] =
    if (profile == null)
      Vector(ValidationError("profile", "profile must not be null"))
    else
      validateAxes(
        profile.oneBagTerminal,
        profile.reductionFinalization,
        profile.reductionSupport
      )

  private[yannakakis] def canonicalPreimage(
      profile: PostgreSqlAggregateRenderingProfile
  ): String = {
    val errors = validate(profile)
    require(
      errors.isEmpty,
      errors.map(_.code).mkString("invalid aggregate rendering profile: ", ",", "")
    )
    (Scheme +: profile.axisIds).iterator.map(lengthPrefix).mkString
  }

  private def profileId(
      profile: PostgreSqlAggregateRenderingProfile
  ): String = "sha256:" + sha256(canonicalPreimage(profile))

  private def validateAxes(
      oneBagTerminal: PostgreSqlAggregateOneBagTerminalProfile,
      reductionFinalization: PostgreSqlAggregateReductionFinalizationProfile,
      reductionSupport: PostgreSqlReductionSupportProfile
  ): Vector[ValidationError] =
    Vector(
      Option.when(
        oneBagTerminal == null ||
          !PostgreSqlAggregateOneBagTerminalProfile.Values.contains(
            oneBagTerminal
          )
      )(
        ValidationError(
          "one-bag-terminal",
          "one-bag terminal must be one exact registered value"
        )
      ),
      Option.when(
        reductionFinalization == null ||
          !PostgreSqlAggregateReductionFinalizationProfile.Values.contains(
            reductionFinalization
          )
      )(
        ValidationError(
          "reduction-finalization",
          "reduction finalization must be one exact registered value"
        )
      ),
      Option.when(
        reductionSupport == null ||
          !PostgreSqlReductionSupportProfile.Values.contains(reductionSupport)
      )(
        ValidationError(
          "reduction-support",
          "reduction support must be one exact registered value"
        )
      )
    ).flatten

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
