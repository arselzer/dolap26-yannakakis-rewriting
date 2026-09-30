package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlAggregateRenderingProfile
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

/**
  * Additive identity for one development generated-SQL optimization action.
  *
  * It wraps, and never replaces, the frozen v1 planner-controlled physical
  * variant derived from the exact candidate and reduction mode. Its preimage
  * additionally binds the aggregate-rendering-profile scheme and all three
  * exact axis identifiers. Existing physical IDs and JSON schemas therefore
  * remain unchanged.
  */
private[yannakakis] final class PostgreSqlGeneratedSqlOptimizationActionId private (
    val value: String
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlGeneratedSqlOptimizationActionId =>
      value == that.value
    case _ => false
  }

  override def hashCode(): Int = value.hashCode
  override def toString: String = value
}

private[yannakakis] object PostgreSqlGeneratedSqlOptimizationActionId {
  val Scheme: String = "postgresql_generated_sql_optimization_action_v1"

  /**
    * Derives the frozen v1 planner-controlled base identity internally.
    * Configured v2 variants have no profile-enabled rendering facade and
    * therefore cannot be named by this development action boundary.
    */
  def from(
      candidateId: CandidatePlanId,
      reductionMode: YannakakisReductionMode,
      profile: PostgreSqlAggregateRenderingProfile
  ): PostgreSqlGeneratedSqlOptimizationActionId = {
    require(candidateId != null, "candidate plan id must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    val profileErrors = PostgreSqlAggregateRenderingProfile.validate(profile)
    require(
      profileErrors.isEmpty,
      profileErrors.map(_.code).mkString("invalid aggregate profile: ", ",", "")
    )
    new PostgreSqlGeneratedSqlOptimizationActionId(
      "sha256:" + sha256(canonicalPreimage(candidateId, reductionMode, profile))
    )
  }

  private[yannakakis] def canonicalPreimage(
      candidateId: CandidatePlanId,
      reductionMode: YannakakisReductionMode,
      profile: PostgreSqlAggregateRenderingProfile
  ): String = {
    require(candidateId != null, "candidate plan id must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    val profileErrors = PostgreSqlAggregateRenderingProfile.validate(profile)
    require(
      profileErrors.isEmpty,
      profileErrors.map(_.code).mkString("invalid aggregate profile: ", ",", "")
    )
    val basePhysicalVariantId =
      PostgreSqlPhysicalVariantId.from(candidateId, reductionMode)
    Vector(
      Scheme,
      basePhysicalVariantId.value,
      PostgreSqlAggregateRenderingProfile.Scheme,
      profile.oneBagTerminal.id,
      profile.reductionFinalization.id,
      profile.reductionSupport.id
    ).iterator.map(lengthPrefix).mkString
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
