package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftFailure._
import at.ac.tuwien.dbai.yannakakis.core.{EdgeId, QueryHypergraph}

/** Exhaustively enumerates independent edge subsets up to a fixed width. */
object CoverEnumerator {
  def enumerate(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      limits: SoftResourceLimits = SoftResourceLimits.default
  ): Either[SoftFailure, Vector[EdgeCover]] = {
    validateInputs(hypergraph, widthBound, limits).flatMap { _ =>
      val largestSize = math.min(widthBound, hypergraph.size)
      val required = countSubsets(hypergraph.size, largestSize)
      if (required > BigInt(limits.maxCovers)) {
        Left(
          ResourceLimitExceeded(
            SoftResource.Covers,
            required,
            limits.maxCovers
          )
        )
      } else {
        val result = Vector.newBuilder[EdgeCover]
        var size = 0
        while (size <= largestSize) {
          combinations(hypergraph.edgeIds, size).foreach { ids =>
            result += EdgeCover.canonical(ids)
          }
          size += 1
        }
        Right(result.result())
      }
    }
  }

  private[soft] def validateInputs(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      limits: SoftResourceLimits
  ): Either[SoftFailure, Unit] = {
    if (hypergraph eq null) Left(MissingInput("query hypergraph"))
    else if (widthBound < 1) Left(InvalidWidthBound(widthBound))
    else if (limits eq null) Left(MissingInput("soft resource limits"))
    else Right(())
  }

  private[soft] def countSubsets(edgeCount: Int, largestSize: Int): BigInt = {
    var total = BigInt(1)
    var binomial = BigInt(1)
    var size = 1
    while (size <= largestSize) {
      binomial = binomial * (edgeCount - size + 1) / size
      total += binomial
      size += 1
    }
    total
  }

  private def combinations(
      edgeIds: Vector[EdgeId],
      size: Int
  ): Iterator[Vector[EdgeId]] = {
    if (size == 0) Iterator.single(Vector.empty)
    else edgeIds.combinations(size)
  }
}
