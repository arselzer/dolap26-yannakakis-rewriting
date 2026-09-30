package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftFailure._
import at.ac.tuwien.dbai.yannakakis.core.{EdgeId, QueryHypergraph, VariableId}

import scala.collection.mutable

/** Computes the paper's [lambda]-edge components deterministically. */
object EdgeComponentEngine {
  def components(
      hypergraph: QueryHypergraph,
      separator: EdgeCover
  ): Either[SoftFailure, Vector[EdgeComponent]] = {
    if (hypergraph eq null) {
      Left(MissingInput("query hypergraph"))
    } else if (separator eq null) {
      Left(MissingInput("separator cover"))
    } else {
      val knownIds = hypergraph.edgeIds.toSet
      val unknownIds = separator.edgeIds.filterNot(knownIds).sorted
      if (unknownIds.nonEmpty) Left(UnknownEdgeIds(unknownIds))
      else Right(compute(hypergraph, separator))
    }
  }

  private def compute(
      hypergraph: QueryHypergraph,
      separator: EdgeCover
  ): Vector[EdgeComponent] = {
    val separatorVertices = separator.edgeIds.iterator
      .flatMap(edgeId => hypergraph.edge(edgeId).iterator)
      .flatMap(_.vertices)
      .toSet

    // Edges contained completely in the separator have no [lambda]-connected
    // vertex and therefore belong to no component (Definition 1 in the paper).
    val residualVerticesByEdge = hypergraph.edges.flatMap { edge =>
      val residual = edge.vertices.filterNot(separatorVertices)
      if (residual.nonEmpty) Some(edge.id -> residual) else None
    }.toMap

    val residualEdgesByVertex: Map[VariableId, Vector[EdgeId]] =
      residualVerticesByEdge.iterator
        .flatMap { case (edgeId, vertices) =>
          vertices.iterator.map(_ -> edgeId)
        }
        .toVector
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).distinct.sorted)
        .toMap

    val unseen = mutable.LinkedHashSet.from(residualVerticesByEdge.keys.toVector.sorted)
    val result = Vector.newBuilder[EdgeComponent]

    while (unseen.nonEmpty) {
      val start = unseen.head
      unseen -= start
      val queue = mutable.Queue(start)
      val component = Vector.newBuilder[EdgeId]

      while (queue.nonEmpty) {
        val current = queue.dequeue()
        component += current
        residualVerticesByEdge(current).foreach { variableId =>
          residualEdgesByVertex.getOrElse(variableId, Vector.empty).foreach {
            neighbour =>
              if (unseen.remove(neighbour)) queue.enqueue(neighbour)
          }
        }
      }

      result += EdgeComponent.canonical(component.result())
    }

    result.result().sorted
  }
}
