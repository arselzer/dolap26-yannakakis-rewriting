package at.ac.tuwien.dbai.yannakakis.core.decomposition

import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, VariableId}

/** A decomposition bag and one concrete hyperedge cover for that bag.
  *
  * Cover identity is part of equality. Two bags with the same id and vertices
  * but different covers therefore remain distinct physical alternatives.
  */
final class CoveredBag private (
    val id: BagId,
    val vertices: Vector[VariableId],
    val cover: Vector[EdgeId]
) extends Serializable {
  private val vertexSet: Set[VariableId] = vertices.toSet

  /** Number of guards used by this concrete cover. */
  def coverWidth: Int = cover.size

  def contains(variableId: VariableId): Boolean = vertexSet.contains(variableId)

  def containsAll(variableIds: Iterable[VariableId]): Boolean =
    (variableIds ne null) && variableIds.forall(vertexSet.contains)

  override def equals(other: Any): Boolean = other match {
    case that: CoveredBag =>
      id == that.id && vertices == that.vertices && cover == that.cover
    case _ => false
  }

  override def hashCode(): Int = (id, vertices, cover).hashCode()

  override def toString: String =
    s"CoveredBag(${id.value},[${vertices.map(_.value).mkString(",")}],[${cover.map(_.value).mkString(",")}])"
}

object CoveredBag {
  def create(
      id: BagId,
      vertices: Iterable[VariableId],
      cover: Iterable[EdgeId]
  ): Either[Vector[DecompositionValidationError], CoveredBag] = {
    val errors = Vector.newBuilder[DecompositionValidationError]

    if (id eq null) {
      errors += DecompositionValidationError.MissingValue("covered bag id")
    }

    val validVertices = canonicalValues(
      vertices,
      "covered bag vertices",
      errors
    )(VariableId.ordering)
    val validCover = canonicalValues(
      cover,
      "covered bag cover",
      errors
    )(EdgeId.ordering)

    val canonicalErrors =
      DecompositionValidationError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) Left(canonicalErrors)
    else Right(new CoveredBag(id, validVertices, validCover))
  }

  def from(
      id: BagId,
      vertices: Iterable[VariableId],
      cover: Iterable[EdgeId]
  ): Either[Vector[DecompositionValidationError], CoveredBag] =
    create(id, vertices, cover)

  implicit val ordering: Ordering[CoveredBag] = new Ordering[CoveredBag] {
    override def compare(left: CoveredBag, right: CoveredBag): Int = {
      val idComparison = BagId.ordering.compare(left.id, right.id)
      if (idComparison != 0) {
        idComparison
      } else {
        val vertexComparison = CanonicalOrdering.compareVectors(
          left.vertices,
          right.vertices
        )(VariableId.ordering)
        if (vertexComparison != 0) vertexComparison
        else
          CanonicalOrdering.compareVectors(left.cover, right.cover)(
            EdgeId.ordering
          )
      }
    }
  }

  private def canonicalValues[A](
      values: Iterable[A],
      path: String,
      errors: scala.collection.mutable.Builder[
        DecompositionValidationError,
        Vector[DecompositionValidationError]
      ]
  )(implicit ordering: Ordering[A]): Vector[A] = {
    if (values eq null) {
      errors += DecompositionValidationError.MissingValue(path)
      Vector.empty
    } else {
      val supplied = values.iterator.toVector
      supplied.zipWithIndex.foreach { case (value, index) =>
        if (value == null) {
          errors += DecompositionValidationError.MissingValue(s"$path $index")
        }
      }
      supplied.filter(_ != null).distinct.sorted
    }
  }
}
