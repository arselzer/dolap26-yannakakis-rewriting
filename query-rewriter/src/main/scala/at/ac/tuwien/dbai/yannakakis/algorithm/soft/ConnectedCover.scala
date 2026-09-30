package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.core.{EdgeId, QueryHypergraph, VariableId}

import scala.collection.mutable

/** Bag-cover and ordinary hypergraph-connectivity predicates used by ConCov. */
object ConnectedCover {
  def covers(
      hypergraph: QueryHypergraph,
      cover: EdgeCover,
      bag: Iterable[VariableId]
  ): Either[SoftFailure, Boolean] = {
    if (bag eq null) Left(SoftFailure.MissingInput("bag vertices"))
    else
      validate(hypergraph, cover).flatMap { _ =>
        val suppliedVertices = bag.iterator.toVector
        if (suppliedVertices.exists(_ eq null)) {
          Left(SoftFailure.NullVertexInBag)
        } else {
          Right(suppliedVertices.toSet.subsetOf(unionVertices(hypergraph, cover)))
        }
      }
  }

  def isConnected(
      hypergraph: QueryHypergraph,
      cover: EdgeCover
  ): Either[SoftFailure, Boolean] =
    validate(hypergraph, cover).map(_ => computeConnectivity(hypergraph, cover))

  private def validate(
      hypergraph: QueryHypergraph,
      cover: EdgeCover
  ): Either[SoftFailure, Unit] = {
    if (hypergraph eq null) Left(SoftFailure.MissingInput("query hypergraph"))
    else if (cover eq null) Left(SoftFailure.MissingInput("edge cover"))
    else {
      val knownIds = hypergraph.edgeIds.toSet
      val unknown = cover.edgeIds.filterNot(knownIds).sorted
      if (unknown.nonEmpty) Left(SoftFailure.UnknownEdgeIds(unknown))
      else Right(())
    }
  }

  private[soft] def unionVertices(
      hypergraph: QueryHypergraph,
      cover: EdgeCover
  ): Set[VariableId] =
    cover.edgeIds.iterator
      .flatMap(edgeId => hypergraph.edge(edgeId).iterator)
      .flatMap(_.vertices)
      .toSet

  private[soft] def computeConnectivity(
      hypergraph: QueryHypergraph,
      cover: EdgeCover
  ): Boolean = {
    if (cover.isEmpty) false
    else if (cover.size == 1) true
    else {
      val verticesByEdge: Map[EdgeId, Set[VariableId]] = cover.edgeIds.iterator
        .map(edgeId => edgeId -> hypergraph.edge(edgeId).get.vertices.toSet)
        .toMap
      val remaining = mutable.LinkedHashSet.from(cover.edgeIds.tail)
      val queue = mutable.Queue(cover.edgeIds.head)

      while (queue.nonEmpty) {
        val current = queue.dequeue()
        val currentVertices = verticesByEdge(current)
        val adjacent = remaining.iterator
          .filter(edgeId => currentVertices.intersect(verticesByEdge(edgeId)).nonEmpty)
          .toVector
        adjacent.foreach { edgeId =>
          remaining -= edgeId
          queue.enqueue(edgeId)
        }
      }
      remaining.isEmpty
    }
  }
}
