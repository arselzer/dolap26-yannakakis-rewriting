package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlAggregateRenderingProfile,
  PostgreSqlAggregateRenderingProfileV2
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

/**
  * Additive identity for one renderer-v2 generated-SQL action.
  *
  * The identity wraps the exact frozen v1 generated-SQL action selected by
  * the v2 profile's base profile. It never replaces or changes a v1 action ID.
  */
private[yannakakis] final class PostgreSqlGeneratedSqlOptimizationActionIdV2 private (
    val value: String
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlGeneratedSqlOptimizationActionIdV2 =>
      value == that.value
    case _ => false
  }

  override def hashCode(): Int = value.hashCode
  override def toString: String = value
}

private[yannakakis] object PostgreSqlGeneratedSqlOptimizationActionIdV2 {
  val Scheme: String = "postgresql_generated_sql_optimization_action_v2"

  /** Derives and binds the exact frozen v1 base action internally. */
  def from(
      candidateId: CandidatePlanId,
      reductionMode: YannakakisReductionMode,
      profile: PostgreSqlAggregateRenderingProfileV2
  ): PostgreSqlGeneratedSqlOptimizationActionIdV2 = {
    validateInputs(candidateId, reductionMode, profile)
    new PostgreSqlGeneratedSqlOptimizationActionIdV2(
      "sha256:" + sha256(canonicalPreimage(candidateId, reductionMode, profile))
    )
  }

  private[yannakakis] def canonicalPreimage(
      candidateId: CandidatePlanId,
      reductionMode: YannakakisReductionMode,
      profile: PostgreSqlAggregateRenderingProfileV2
  ): String = {
    validateInputs(candidateId, reductionMode, profile)
    val baseActionId = PostgreSqlGeneratedSqlOptimizationActionId.from(
      candidateId,
      reductionMode,
      profile.baseProfile
    )
    Vector(
      Scheme,
      PostgreSqlGeneratedSqlOptimizationActionId.Scheme,
      baseActionId.value,
      PostgreSqlAggregateRenderingProfileV2.Scheme,
      profile.id,
      PostgreSqlAggregateRenderingProfile.Scheme,
      profile.baseProfile.id
    ).iterator
      .concat(profile.baseProfile.axisIds.iterator)
      .concat(profile.axisIds.iterator)
      .map(lengthPrefix)
      .mkString
  }

  private def validateInputs(
      candidateId: CandidatePlanId,
      reductionMode: YannakakisReductionMode,
      profile: PostgreSqlAggregateRenderingProfileV2
  ): Unit = {
    require(candidateId != null, "candidate plan id must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    val profileErrors = PostgreSqlAggregateRenderingProfileV2.validate(profile)
    require(
      profileErrors.isEmpty,
      profileErrors.map(_.code).mkString("invalid aggregate profile v2: ", ",", "")
    )
    require(
      profile.usesOptimizedAxes,
      "renderer-v2 action identity requires at least one optimized axis"
    )
    require(
      reductionMode == YannakakisReductionMode.FullTwoPass,
      "optimized renderer-v2 actions require full reduction"
    )
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
