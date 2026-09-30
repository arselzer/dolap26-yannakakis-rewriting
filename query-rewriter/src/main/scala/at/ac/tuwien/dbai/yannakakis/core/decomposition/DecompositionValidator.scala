package at.ac.tuwien.dbai.yannakakis.core.decomposition

import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  QueryHypergraph,
  VariableId
}

import scala.collection.mutable

/** Validates structural and hypergraph-dependent CandidateTD invariants. */
object DecompositionValidator {
  def validate(
      hypergraph: QueryHypergraph,
      decomposition: CoveredDecomposition
  ): Either[Vector[DecompositionValidationError], CoveredDecomposition] = {
    if (hypergraph eq null) {
      return Left(
        Vector(
          DecompositionValidationError.MissingValue(
            "decomposition validator hypergraph"
          )
        )
      )
    }
    if (decomposition eq null) {
      return Left(
        Vector(
          DecompositionValidationError.MissingValue(
            "decomposition validator decomposition"
          )
        )
      )
    }

    val errors = Vector.newBuilder[DecompositionValidationError]
    val bags = decomposition.bags
    val uniqueBagIds = bags.iterator.map(_.id).toSet

    bags
      .groupBy(_.id)
      .collect { case (bagId, occurrences) if occurrences.size > 1 => bagId }
      .toVector
      .sorted
      .foreach(bagId =>
        errors += DecompositionValidationError.DuplicateBagId(bagId)
      )

    if (!uniqueBagIds.contains(decomposition.root)) {
      errors += DecompositionValidationError.MissingRootBag(decomposition.root)
    }

    decomposition.treeEdges
      .groupBy(identity)
      .collect { case (treeEdge, occurrences) if occurrences.size > 1 => treeEdge }
      .toVector
      .sorted
      .foreach(treeEdge =>
        errors += DecompositionValidationError.DuplicateTreeEdge(treeEdge)
      )

    decomposition.treeEdges.foreach { treeEdge =>
      if (treeEdge.first == treeEdge.second) {
        errors += DecompositionValidationError.SelfLoop(treeEdge)
      }
      treeEdge.endpoints.distinct.foreach { endpoint =>
        if (!uniqueBagIds.contains(endpoint)) {
          errors += DecompositionValidationError.UnknownTreeEndpoint(
            treeEdge,
            endpoint
          )
        }
      }
    }

    val adjacency = adjacencyFor(uniqueBagIds, decomposition.treeEdges)
    connectivityErrors(decomposition, uniqueBagIds, adjacency).foreach(
      errors += _
    )
    if (containsCycle(uniqueBagIds, adjacency)) {
      errors += DecompositionValidationError.CyclicTree
    }

    validateCovers(hypergraph, bags).foreach(errors += _)
    validateHyperedgeCoverage(hypergraph, bags).foreach(errors += _)
    validateRunningIntersection(bags, adjacency).foreach(errors += _)

    val canonicalErrors =
      DecompositionValidationError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) Left(canonicalErrors)
    else Right(decomposition)
  }

  private def adjacencyFor(
      bagIds: Set[BagId],
      treeEdges: Vector[BagTreeEdge]
  ): Map[BagId, Set[BagId]] = {
    val adjacency = mutable.Map.empty[BagId, mutable.Set[BagId]]
    bagIds.foreach(bagId => adjacency.put(bagId, mutable.Set.empty))

    treeEdges.distinct.foreach { treeEdge =>
      if (
        treeEdge.first != treeEdge.second &&
        bagIds.contains(treeEdge.first) &&
        bagIds.contains(treeEdge.second)
      ) {
        adjacency(treeEdge.first) += treeEdge.second
        adjacency(treeEdge.second) += treeEdge.first
      }
    }

    adjacency.iterator.map { case (bagId, neighbors) =>
      bagId -> neighbors.toSet
    }.toMap
  }

  private def connectivityErrors(
      decomposition: CoveredDecomposition,
      bagIds: Set[BagId],
      adjacency: Map[BagId, Set[BagId]]
  ): Vector[DecompositionValidationError] = {
    val start =
      if (bagIds.contains(decomposition.root)) Some(decomposition.root)
      else bagIds.toVector.sorted.headOption

    start match {
      case None => Vector.empty
      case Some(startBag) =>
        val reached = reachable(startBag, adjacency, bagIds)
        val unreachable = (bagIds -- reached).toVector.sorted
        if (unreachable.nonEmpty) {
          Vector(DecompositionValidationError.DisconnectedTree(unreachable))
        } else Vector.empty
    }
  }

  private def containsCycle(
      bagIds: Set[BagId],
      adjacency: Map[BagId, Set[BagId]]
  ): Boolean = {
    val visited = mutable.Set.empty[BagId]

    bagIds.toVector.sorted.foreach { start =>
      if (!visited.contains(start)) {
        visited += start
        val pending = mutable.Stack(
          CycleFrame(
            current = start,
            parent = None,
            neighbors = adjacency.getOrElse(start, Set.empty).toVector.sorted,
            nextNeighbor = 0
          )
        )

        while (pending.nonEmpty) {
          val frame = pending.pop()
          if (frame.nextNeighbor < frame.neighbors.size) {
            val neighbor = frame.neighbors(frame.nextNeighbor)
            pending.push(frame.copy(nextNeighbor = frame.nextNeighbor + 1))

            if (!frame.parent.contains(neighbor)) {
              if (visited.contains(neighbor)) return true
              visited += neighbor
              pending.push(
                CycleFrame(
                  current = neighbor,
                  parent = Some(frame.current),
                  neighbors = adjacency
                    .getOrElse(neighbor, Set.empty)
                    .toVector
                    .sorted,
                  nextNeighbor = 0
                )
              )
            }
          }
        }
      }
    }

    false
  }

  private final case class CycleFrame(
      current: BagId,
      parent: Option[BagId],
      neighbors: Vector[BagId],
      nextNeighbor: Int
  )

  private def validateCovers(
      hypergraph: QueryHypergraph,
      bags: Vector[CoveredBag]
  ): Vector[DecompositionValidationError] = {
    val errors = Vector.newBuilder[DecompositionValidationError]

    bags.foreach { bag =>
      val knownCoverEdges = bag.cover.flatMap { edgeId =>
        hypergraph.edge(edgeId) match {
          case Some(edge) => Some(edge)
          case None =>
            errors += DecompositionValidationError.UnknownCoverEdge(
              bag.id,
              edgeId
            )
            None
        }
      }
      val coveredVariables =
        knownCoverEdges.iterator.flatMap(_.vertices).toSet
      bag.vertices
        .filterNot(coveredVariables.contains)
        .foreach(variableId =>
          errors += DecompositionValidationError.BagVertexNotCovered(
            bag.id,
            variableId
          )
        )
    }

    errors.result()
  }

  private def validateHyperedgeCoverage(
      hypergraph: QueryHypergraph,
      bags: Vector[CoveredBag]
  ): Vector[DecompositionValidationError] =
    hypergraph.edges.collect {
      case edge if !bags.exists(_.containsAll(edge.vertices)) =>
        DecompositionValidationError.HyperedgeNotCovered(edge.id)
    }

  private def validateRunningIntersection(
      bags: Vector[CoveredBag],
      adjacency: Map[BagId, Set[BagId]]
  ): Vector[DecompositionValidationError] = {
    val variables = bags.iterator.flatMap(_.vertices).toVector.distinct.sorted

    variables.flatMap { variableId =>
      val containingBags = bags
        .filter(_.contains(variableId))
        .map(_.id)
        .distinct
        .sorted
      if (containingBags.size <= 1) {
        Vector.empty
      } else {
        val allowed = containingBags.toSet
        val connected = reachable(containingBags.head, adjacency, allowed)
        if (connected == allowed) Vector.empty
        else
          Vector(
            DecompositionValidationError.RunningIntersectionViolation(
              variableId,
              containingBags
            )
          )
      }
    }
  }

  private def reachable(
      start: BagId,
      adjacency: Map[BagId, Set[BagId]],
      allowed: Set[BagId]
  ): Set[BagId] = {
    val visited = mutable.Set.empty[BagId]
    val pending = mutable.Queue(start)

    while (pending.nonEmpty) {
      val current = pending.dequeue()
      if (allowed.contains(current) && !visited.contains(current)) {
        visited += current
        adjacency
          .getOrElse(current, Set.empty)
          .filter(allowed.contains)
          .toVector
          .sorted
          .foreach(pending.enqueue(_))
      }
    }

    visited.toSet
  }
}
