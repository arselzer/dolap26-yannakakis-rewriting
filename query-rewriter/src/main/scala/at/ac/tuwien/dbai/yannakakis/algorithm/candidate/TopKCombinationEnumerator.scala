package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import scala.collection.mutable

/** Exact K-best Cartesian product for the child frontiers of one basis. */
private[candidate] object TopKCombinationEnumerator {
  def enumerate(
      basis: CandidateBasis,
      childFrontiers: Vector[Vector[CandidateTreeNode]],
      planLimit: Int,
      budget: CandidateTopKBudget
  ): Either[CandidateFailure, Vector[CandidateTreeNode]] = {
    if (childFrontiers.exists(_.isEmpty)) {
      Right(Vector.empty)
    } else {
      val seedCoordinates = Vector.fill(childFrontiers.size)(0)
      budget.admitCombinationState().flatMap { _ =>
        val queue = mutable.PriorityQueue.empty[CombinationState](
          combinationOrdering
        )
        val visited = mutable.HashSet(seedCoordinates)
        val uniquePlans = mutable.HashSet.empty[CandidateTreeNode]
        val result = Vector.newBuilder[CandidateTreeNode]
        var resultCount = 0
        var failure: Option[CandidateFailure] = None

        queue.enqueue(
          CombinationState(
            seedCoordinates,
            treeFor(basis, childFrontiers, seedCoordinates)
          )
        )

        while (
          queue.nonEmpty && resultCount < planLimit && failure.isEmpty
        ) {
          val current = queue.dequeue()
          if (uniquePlans.add(current.tree)) {
            result += current.tree
            resultCount += 1
          }

          // Do not materialize successors after the Kth distinct result.
          if (resultCount < planLimit) {
            var dimension = 0
            while (dimension < childFrontiers.size && failure.isEmpty) {
              val nextIndex = current.coordinates(dimension) + 1
              if (nextIndex < childFrontiers(dimension).size) {
                val nextCoordinates = current.coordinates.updated(
                  dimension,
                  nextIndex
                )
                if (!visited(nextCoordinates)) {
                  budget.admitCombinationState() match {
                    case Left(problem) => failure = Some(problem)
                    case Right(_) =>
                      visited += nextCoordinates
                      queue.enqueue(
                        CombinationState(
                          nextCoordinates,
                          treeFor(basis, childFrontiers, nextCoordinates)
                        )
                      )
                  }
                }
              }
              dimension += 1
            }
          }
        }

        failure match {
          case Some(problem) => Left(problem)
          case None =>
            Right(
              result.result().sortWith((left, right) =>
                CandidateOrdering.compareRankedTrees(left, right) < 0
              )
            )
        }
      }
    }
  }

  private def treeFor(
      basis: CandidateBasis,
      childFrontiers: Vector[Vector[CandidateTreeNode]],
      coordinates: Vector[Int]
  ): CandidateTreeNode = {
    val children = childFrontiers.indices.iterator
      .map(dimension => childFrontiers(dimension)(coordinates(dimension)))
      .toVector
      .sortWith((left, right) =>
        CandidateOrdering.compareTreeNodes(left, right) < 0
      )
    new CandidateTreeNode(basis.bag, children)
  }

  private final case class CombinationState(
      coordinates: Vector[Int],
      tree: CandidateTreeNode
  )

  /** PriorityQueue is a max-heap, so both comparisons are reversed. */
  private val combinationOrdering: Ordering[CombinationState] =
    new Ordering[CombinationState] {
      override def compare(
          left: CombinationState,
          right: CombinationState
      ): Int = {
        val treeResult = CandidateOrdering.compareRankedTrees(
          right.tree,
          left.tree
        )
        if (treeResult != 0) treeResult
        else
          CandidateOrdering.compareVectors(
            right.coordinates,
            left.coordinates
          )(Ordering.Int)
      }
    }
}
