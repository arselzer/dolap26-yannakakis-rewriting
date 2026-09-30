package at.ac.tuwien.dbai.yannakakis.core.decomposition

import at.ac.tuwien.dbai.yannakakis.core.BagId

/** A rooted, cover-annotated candidate tree decomposition.
  *
  * Construction canonicalizes input order but intentionally does not discard
  * duplicate bag ids or tree edges; [[DecompositionValidator]] reports those
  * structural errors rather than silently changing the candidate.
  */
final class CoveredDecomposition private (
    val root: BagId,
    val bags: Vector[CoveredBag],
    val treeEdges: Vector[BagTreeEdge]
) extends Serializable {
  val bagIds: Vector[BagId] = bags.map(_.id)
  val width: Int = bags.foldLeft(0)((maximum, bag) => maximum.max(bag.coverWidth))

  def bag(id: BagId): Option[CoveredBag] = bags.find(_.id == id)

  def neighbors(id: BagId): Vector[BagId] =
    treeEdges.iterator.flatMap(_.other(id)).toVector.distinct.sorted

  override def equals(other: Any): Boolean = other match {
    case that: CoveredDecomposition =>
      root == that.root && bags == that.bags && treeEdges == that.treeEdges
    case _ => false
  }

  override def hashCode(): Int = (root, bags, treeEdges).hashCode()

  override def toString: String =
    s"CoveredDecomposition(${root.value},${bags.mkString("[", ",", "]")},${treeEdges.mkString("[", ",", "]")})"
}

object CoveredDecomposition {
  def create(
      root: BagId,
      bags: Iterable[CoveredBag],
      treeEdges: Iterable[BagTreeEdge]
  ): Either[Vector[DecompositionValidationError], CoveredDecomposition] = {
    val errors = Vector.newBuilder[DecompositionValidationError]

    if (root eq null) {
      errors += DecompositionValidationError.MissingValue(
        "covered decomposition root"
      )
    }
    val validBags = collectValues(bags, "covered decomposition bags", errors)
    val validTreeEdges = collectValues(
      treeEdges,
      "covered decomposition tree edges",
      errors
    )

    val canonicalErrors =
      DecompositionValidationError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) {
      Left(canonicalErrors)
    } else {
      Right(
        new CoveredDecomposition(
          root,
          validBags.sorted,
          validTreeEdges.sorted
        )
      )
    }
  }

  def from(
      root: BagId,
      bags: Iterable[CoveredBag],
      treeEdges: Iterable[BagTreeEdge]
  ): Either[Vector[DecompositionValidationError], CoveredDecomposition] =
    create(root, bags, treeEdges)

  implicit val ordering: Ordering[CoveredDecomposition] =
    new Ordering[CoveredDecomposition] {
      override def compare(
          left: CoveredDecomposition,
          right: CoveredDecomposition
      ): Int = {
        val rootComparison = BagId.ordering.compare(left.root, right.root)
        if (rootComparison != 0) {
          rootComparison
        } else {
          val bagComparison = CanonicalOrdering.compareVectors(
            left.bags,
            right.bags
          )(CoveredBag.ordering)
          if (bagComparison != 0) bagComparison
          else
            CanonicalOrdering.compareVectors(
              left.treeEdges,
              right.treeEdges
            )(BagTreeEdge.ordering)
        }
      }
    }

  private def collectValues[A](
      values: Iterable[A],
      path: String,
      errors: scala.collection.mutable.Builder[
        DecompositionValidationError,
        Vector[DecompositionValidationError]
      ]
  ): Vector[A] = {
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
      supplied.filter(_ != null)
    }
  }
}
