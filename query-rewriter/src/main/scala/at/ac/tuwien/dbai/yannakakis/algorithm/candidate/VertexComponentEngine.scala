package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.core.{QueryHypergraph, VariableId}

import scala.collection.mutable

/** Connected components of the vertex hypergraph after deleting a separator. */
object VertexComponentEngine {
  def components(
      hypergraph: QueryHypergraph,
      separator: Iterable[VariableId]
  ): Either[CandidateFailure, Vector[CandidateComponent]] = {
    if (hypergraph eq null) {
      Left(CandidateFailure.MissingValue("vertex-component hypergraph"))
    } else if (separator eq null) {
      Left(CandidateFailure.MissingValue("vertex-component separator"))
    } else {
      val suppliedSeparator = separator.iterator.toVector
      if (suppliedSeparator.exists(_ eq null)) {
        Left(CandidateFailure.MissingValue("vertex-component separator vertex"))
      } else {
        val unknown = suppliedSeparator.distinct
          .filterNot(hypergraph.variables.toSet)
          .sorted
        if (unknown.nonEmpty) {
          Left(CandidateFailure.UnknownSeparatorVertices(unknown))
        } else {
          Right(compute(hypergraph, suppliedSeparator.toSet))
        }
      }
    }
  }

  private def compute(
      hypergraph: QueryHypergraph,
      separator: Set[VariableId]
  ): Vector[CandidateComponent] = {
    val remaining = mutable.LinkedHashSet.from(
      hypergraph.variables.filterNot(separator)
    )
    val result = Vector.newBuilder[CandidateComponent]

    while (remaining.nonEmpty) {
      val start = remaining.head
      remaining -= start
      val queue = mutable.Queue(start)
      val component = mutable.LinkedHashSet.empty[VariableId]

      while (queue.nonEmpty) {
        val current = queue.dequeue()
        component += current
        val adjacent = hypergraph
          .incidentEdges(current)
          .iterator
          .flatMap(_.vertices)
          .filterNot(separator)
          .filter(remaining)
          .toVector
          .distinct
          .sorted
        adjacent.foreach { vertex =>
          remaining -= vertex
          queue.enqueue(vertex)
        }
      }

      result += CandidateComponent.canonical(component)
    }

    result.result().sorted
  }
}
