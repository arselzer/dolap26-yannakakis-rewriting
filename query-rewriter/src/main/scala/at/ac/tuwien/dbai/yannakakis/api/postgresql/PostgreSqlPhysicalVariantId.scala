package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPhysicalConfiguration
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

/** Stable identity of one candidate under one declared PostgreSQL shape. */
final class PostgreSqlPhysicalVariantId private (val value: String)
    extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlPhysicalVariantId => value == that.value
    case _                                 => false
  }

  override def hashCode(): Int = value.hashCode
  override def toString: String = value
}

object PostgreSqlPhysicalVariantId {
  /** Frozen legacy candidate/reduction identity. */
  val Scheme: String = "postgresql_candidate_reduction_variant_v1"
  /** Bounded physical-portfolio identity; independent of the legacy scheme. */
  val SchemeV2: String = "postgresql_candidate_physical_portfolio_variant_v2"
  val CanonicalReductionOrder: String = "canonical_plan_schedule_v1"

  private val Prefix = "sha256:"

  def from(
      candidateId: CandidatePlanId,
      reductionMode: YannakakisReductionMode
  ): PostgreSqlPhysicalVariantId = {
    require(candidateId != null, "candidate plan id must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    val canonical =
      s"{\"candidate_id\":\"${candidateId.value}\"," +
        s"\"reduction_mode\":\"${reductionMode.id}\"," +
        s"\"reduction_order\":\"$CanonicalReductionOrder\"}"
    new PostgreSqlPhysicalVariantId(Prefix + sha256(canonical))
  }

  /**
    * Identifies one exact bounded-portfolio configuration.
    *
    * The preimage is a positional concatenation of UTF-8 length-prefixed
    * values. `LP(x)` is the ASCII decimal UTF-8 byte length of `x`, one ASCII
    * colon, then the UTF-8 bytes of `x`. In order, the values are: v2 scheme,
    * candidate id, execution root, assignment count, each sorted assignment's
    * bag id, edge count, and sorted edge ids, reduction mode, canonical
    * reduction order, CTE directive, and directive scope.
    */
  def fromV2(
      candidateId: CandidatePlanId,
      configuration: BagFactorPhysicalConfiguration,
      reductionMode: YannakakisReductionMode,
      materialization: PostgreSqlCteMaterializationDirective
  ): PostgreSqlPhysicalVariantId = {
    require(candidateId != null, "candidate plan id must not be null")
    require(configuration != null, "physical configuration must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    require(materialization != null, "CTE materialization must not be null")

    val values = Vector.newBuilder[String]
    values += SchemeV2
    values += candidateId.value
    values += configuration.executionRoot.value.toString
    values += configuration.coverAssignments.size.toString
    configuration.coverAssignments.foreach { assignment =>
      values += assignment.bagId.value.toString
      values += assignment.edgeIds.size.toString
      assignment.edgeIds.foreach(edgeId => values += edgeId.value.toString)
    }
    values += reductionMode.id
    values += CanonicalReductionOrder
    values += materialization.id
    values += PostgreSqlCteMaterializationDirective.Scope

    val canonical = values.result().iterator.map(lengthPrefix).mkString
    new PostgreSqlPhysicalVariantId(Prefix + sha256(canonical))
  }

  private[postgresql] def v2CanonicalPreimage(
      candidateId: CandidatePlanId,
      configuration: BagFactorPhysicalConfiguration,
      reductionMode: YannakakisReductionMode,
      materialization: PostgreSqlCteMaterializationDirective
  ): String = {
    require(candidateId != null)
    require(configuration != null)
    require(reductionMode != null)
    require(materialization != null)
    val values = Vector.newBuilder[String]
    values += SchemeV2
    values += candidateId.value
    values += configuration.executionRoot.value.toString
    values += configuration.coverAssignments.size.toString
    configuration.coverAssignments.foreach { assignment =>
      values += assignment.bagId.value.toString
      values += assignment.edgeIds.size.toString
      assignment.edgeIds.foreach(edgeId => values += edgeId.value.toString)
    }
    values += reductionMode.id
    values += CanonicalReductionOrder
    values += materialization.id
    values += PostgreSqlCteMaterializationDirective.Scope
    values.result().iterator.map(lengthPrefix).mkString
  }

  private def lengthPrefix(value: String): String = {
    val byteCount = value.getBytes(StandardCharsets.UTF_8).length
    s"$byteCount:$value"
  }

  private def sha256(value: String): String = {
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
    val alphabet = "0123456789abcdef"
    val result = new StringBuilder(bytes.length * 2)
    bytes.foreach { byte =>
      val unsigned = byte & 0xff
      result.append(alphabet.charAt(unsigned >>> 4))
      result.append(alphabet.charAt(unsigned & 0x0f))
    }
    result.result()
  }
}
