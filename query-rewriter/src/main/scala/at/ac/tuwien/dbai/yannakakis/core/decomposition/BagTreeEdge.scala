package at.ac.tuwien.dbai.yannakakis.core.decomposition

import at.ac.tuwien.dbai.yannakakis.core.BagId

/** An undirected decomposition-tree edge in canonical endpoint order. */
final class BagTreeEdge private (val first: BagId, val second: BagId)
    extends Serializable {
  def endpoints: Vector[BagId] = Vector(first, second)

  def contains(bagId: BagId): Boolean = first == bagId || second == bagId

  def other(bagId: BagId): Option[BagId] = {
    if (first == bagId) Some(second)
    else if (second == bagId) Some(first)
    else None
  }

  override def equals(other: Any): Boolean = other match {
    case that: BagTreeEdge => first == that.first && second == that.second
    case _                 => false
  }

  override def hashCode(): Int = (first, second).hashCode()

  override def toString: String = s"${first.value}-${second.value}"
}

object BagTreeEdge {
  def create(
      left: BagId,
      right: BagId
  ): Either[Vector[DecompositionValidationError], BagTreeEdge] = {
    val errors = Vector.newBuilder[DecompositionValidationError]
    if (left eq null) {
      errors += DecompositionValidationError.MissingValue(
        "tree edge left endpoint"
      )
    }
    if (right eq null) {
      errors += DecompositionValidationError.MissingValue(
        "tree edge right endpoint"
      )
    }

    val canonicalErrors =
      DecompositionValidationError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) {
      Left(canonicalErrors)
    } else if (BagId.ordering.lteq(left, right)) {
      Right(new BagTreeEdge(left, right))
    } else {
      Right(new BagTreeEdge(right, left))
    }
  }

  def from(
      left: BagId,
      right: BagId
  ): Either[Vector[DecompositionValidationError], BagTreeEdge] =
    create(left, right)

  implicit val ordering: Ordering[BagTreeEdge] = new Ordering[BagTreeEdge] {
    override def compare(left: BagTreeEdge, right: BagTreeEdge): Int = {
      val firstComparison = BagId.ordering.compare(left.first, right.first)
      if (firstComparison != 0) firstComparison
      else BagId.ordering.compare(left.second, right.second)
    }
  }
}
