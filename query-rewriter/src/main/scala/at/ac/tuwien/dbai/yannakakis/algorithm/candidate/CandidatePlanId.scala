package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition

/** Stable identifier for one canonical, cover-annotated decomposition.
  *
  * The versioned input representation deliberately matches the canonical
  * decomposition JSON hashed by the PostgreSQL rewrite artifact v1. Moving the
  * implementation out of the CLI makes the same identity available to public
  * API callers and prevents serializers and selectors from inventing separate
  * identifiers.
  */
final class CandidatePlanId private (val value: String) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: CandidatePlanId => value == that.value
    case _                     => false
  }

  override def hashCode(): Int = value.hashCode
  override def toString: String = value
}

sealed trait CandidatePlanIdError extends Product with Serializable {
  def code: String
  def message: String
}

object CandidatePlanIdError {
  case object MissingValue extends CandidatePlanIdError {
    override val code: String = "candidate-id.missing"
    override val message: String = "Candidate plan id must not be null"
  }

  final case class InvalidFormat(value: String) extends CandidatePlanIdError {
    override val code: String = "candidate-id.invalid-format"
    override val message: String =
      "Candidate plan id must be 'sha256:' followed by 64 lowercase hexadecimal characters"
  }
}

object CandidatePlanId {
  val Scheme: String = "covered_decomposition_json_v1"
  private val Prefix = "sha256:"
  private val EncodedLength = Prefix.length + 64

  /** Parses an untrusted external identifier without normalizing it. */
  def parse(value: String): Either[CandidatePlanIdError, CandidatePlanId] = {
    if (value == null) Left(CandidatePlanIdError.MissingValue)
    else if (!isCanonical(value)) {
      Left(CandidatePlanIdError.InvalidFormat(value))
    } else Right(new CandidatePlanId(value))
  }

  /** Computes the v1 identifier of a trusted canonical decomposition. */
  def from(decomposition: CoveredDecomposition): CandidatePlanId = {
    require(decomposition != null, "covered decomposition must not be null")
    new CandidatePlanId(Prefix + sha256(canonicalJson(decomposition)))
  }

  private def isCanonical(value: String): Boolean = {
    if (value.length != EncodedLength || !value.startsWith(Prefix)) false
    else {
      var index = Prefix.length
      while (index < value.length) {
        val character = value.charAt(index)
        if (!((character >= '0' && character <= '9') ||
              (character >= 'a' && character <= 'f'))) return false
        index += 1
      }
      true
    }
  }

  /** Must remain byte-identical to decompositionJson in artifact v1. */
  private[yannakakis] def canonicalJson(
      decomposition: CoveredDecomposition
  ): String = {
    val output = new StringBuilder
    output.append("{\"root_bag_id\":")
    output.append(decomposition.root.value)
    output.append(",\"width\":")
    output.append(decomposition.width)
    output.append(",\"bags\":[")
    decomposition.bags.sortBy(_.id.value).zipWithIndex.foreach {
      case (bag, index) =>
        if (index > 0) output.append(',')
        output.append("{\"id\":")
        output.append(bag.id.value)
        output.append(",\"vertices\":[")
        appendInts(output, bag.vertices.sortBy(_.value).map(_.value))
        output.append("],\"cover_edge_ids\":[")
        appendInts(output, bag.cover.sortBy(_.value).map(_.value))
        output.append("]}")
    }
    output.append("],\"tree_edges\":[")
    decomposition.treeEdges
      .sortBy(edge => (edge.first.value, edge.second.value))
      .zipWithIndex
      .foreach { case (edge, index) =>
        if (index > 0) output.append(',')
        output.append("{\"first\":")
        output.append(edge.first.value)
        output.append(",\"second\":")
        output.append(edge.second.value)
        output.append('}')
      }
    output.append("]}")
    output.result()
  }

  private def appendInts(output: StringBuilder, values: Vector[Int]): Unit =
    values.zipWithIndex.foreach { case (value, index) =>
      if (index > 0) output.append(',')
      output.append(value)
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
