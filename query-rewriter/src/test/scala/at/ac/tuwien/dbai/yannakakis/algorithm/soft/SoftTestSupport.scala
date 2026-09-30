package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.core._

import scala.collection.mutable

private[soft] object SoftTestSupport {
  final case class OracleWitness(
      lambda1: Vector[Int],
      lambda2: Vector[Int],
      component: Vector[Int]
  )

  final case class OracleCandidateCover(
      edgeIds: Vector[Int],
      isConnected: Boolean
  )

  final case class OracleCandidate(
      vertices: Vector[Int],
      witnesses: Vector[OracleWitness],
      physicalCovers: Vector[OracleCandidateCover]
  )

  def variable(value: Int): VariableId = right(VariableId.from(value))
  def edgeId(value: Int): EdgeId = right(EdgeId.from(value))

  def cover(values: Int*): EdgeCover =
    right(EdgeCover.create(values.map(edgeId)))

  def graph(vertexSets: Seq[Seq[Int]]): QueryHypergraph =
    graphWithIds(vertexSets.zipWithIndex.map { case (vertices, id) => id -> vertices })

  def graphWithIds(edges: Seq[(Int, Seq[Int])]): QueryHypergraph = {
    val hyperedges = edges.map { case (numericId, vertices) =>
      val occurrence = right(RelationOccurrenceId.from(numericId))
      val bindings = vertices.zipWithIndex.map { case (numericVariable, ordinal) =>
        right(ColumnId.from(occurrence, ordinal)) -> variable(numericVariable)
      }
      right(
        Hyperedge.create(edgeId(numericId), occurrence, bindings),
        (errors: Vector[ValidationError]) => errors.mkString("; ")
      )
    }
    right(
      QueryHypergraph.create(hyperedges),
      (errors: Vector[ValidationError]) => errors.mkString("; ")
    )
  }

  def edgeValues(component: EdgeComponent): Vector[Int] =
    component.edgeIds.map(_.value)

  def vertexValues(candidate: SoftBagCandidate): Vector[Int] =
    candidate.vertices.map(_.value)

  def candidate(
      generation: SoftBagGeneration,
      vertices: Int*
  ): SoftBagCandidate =
    generation.candidates.find(vertexValues(_) == vertices.toVector.sorted).getOrElse {
      throw new AssertionError(
        s"candidate ${vertices.mkString("{", ",", "}")} not found"
      )
    }

  def directSoftBags(
      hypergraph: QueryHypergraph,
      widthBound: Int
  ): Set[Vector[Int]] = {
    val allCovers = directCovers(hypergraph.edgeIds, widthBound)
    (for {
      lambda2 <- allCovers
      component <- directComponents(hypergraph, lambda2)
      lambda1 <- allCovers
    } yield {
      val lambdaVertices = lambda1.iterator
        .flatMap(id => hypergraph.edge(id).toVector)
        .flatMap(_.vertices)
        .toSet
      val componentVertices = component.iterator
        .flatMap(id => hypergraph.edge(id).toVector)
        .flatMap(_.vertices)
        .toSet
      lambdaVertices.intersect(componentVertices).toVector.sorted.map(_.value)
    }).toSet
  }

  /**
    * Exhaustive test oracle for all observable generation evidence. It follows
    * the definitions directly and deliberately does not call the production
    * cover enumerator, component engine, cover predicate, or connectivity
    * predicate.
    */
  def directSoftCandidates(
      hypergraph: QueryHypergraph,
      widthBound: Int
  ): Vector[OracleCandidate] = {
    val allCovers = directCovers(hypergraph.edgeIds, widthBound)
    val physicalCovers =
      if (hypergraph.isEmpty) allCovers else allCovers.filter(_.nonEmpty)
    val witnessesByVertices = mutable.Map.empty[
      Vector[VariableId],
      mutable.ArrayBuffer[OracleWitness]
    ]

    allCovers.foreach { lambda2 =>
      directComponents(hypergraph, lambda2).foreach { component =>
        val componentVertices = unionVertices(hypergraph, component)
        allCovers.foreach { lambda1 =>
          val vertices = unionVertices(hypergraph, lambda1)
            .intersect(componentVertices)
            .toVector
            .sorted
          witnessesByVertices
            .getOrElseUpdate(vertices, mutable.ArrayBuffer.empty)
            .append(
              OracleWitness(
                orderedEdgeValues(lambda1),
                orderedEdgeValues(lambda2),
                orderedEdgeValues(component)
              )
            )
        }
      }
    }

    witnessesByVertices.iterator
      .map { case (vertices, witnesses) =>
        val vertexSet = vertices.toSet
        val alternatives = physicalCovers.collect {
          case cover if vertexSet.subsetOf(unionVertices(hypergraph, cover)) =>
            OracleCandidateCover(
              orderedEdgeValues(cover),
              directConnectivity(hypergraph, cover)
            )
        }
        OracleCandidate(
          vertices.map(_.value),
          witnesses.toVector.sortWith(compareWitnesses(_, _) < 0),
          alternatives
        )
      }
      .toVector
      .sortWith((left, right) => compareInts(left.vertices, right.vertices) < 0)
  }

  def generatedEvidence(
      generation: SoftBagGeneration
  ): Vector[OracleCandidate] =
    generation.candidates.map { candidate =>
      OracleCandidate(
        candidate.vertices.map(_.value),
        candidate.witnesses.map { witness =>
          OracleWitness(
            witness.lambda1.edgeIds.map(_.value),
            witness.lambda2.edgeIds.map(_.value),
            witness.component.edgeIds.map(_.value)
          )
        },
        candidate.covers.map { alternative =>
          OracleCandidateCover(
            alternative.cover.edgeIds.map(_.value),
            alternative.isConnected
          )
        }
      )
    }

  /**
    * Directly follows the paper's vertex-path definition. This intentionally
    * does not use edge adjacency or the production component engine.
    */
  def directComponents(
      hypergraph: QueryHypergraph,
      lambda: Set[EdgeId]
  ): Vector[Set[EdgeId]] = {
    val separator = lambda.iterator
      .flatMap(id => hypergraph.edge(id).toVector)
      .flatMap(_.vertices)
      .toSet
    val remainingVertices = hypergraph.variables.filterNot(separator)
    val neighbours = remainingVertices.map { vertex =>
      val adjacent = hypergraph.edges.iterator
        .filter(_.contains(vertex))
        .flatMap(_.vertices)
        .filterNot(separator)
        .toSet - vertex
      vertex -> adjacent
    }.toMap

    @annotation.tailrec
    def closure(
        frontier: Set[VariableId],
        reached: Set[VariableId]
    ): Set[VariableId] = {
      val next = frontier.iterator.flatMap(neighbours).toSet -- reached
      if (next.isEmpty) reached else closure(next, reached ++ next)
    }

    var unseen = remainingVertices.toSet
    var vertexComponents = Vector.empty[Set[VariableId]]
    while (unseen.nonEmpty) {
      val start = unseen.min
      val connected = closure(Set(start), Set(start))
      vertexComponents :+= connected
      unseen --= connected
    }

    vertexComponents.flatMap { connectedVertices =>
      val edgeComponent = hypergraph.edges.iterator
        .filter(edge => edge.vertices.exists(connectedVertices))
        .map(_.id)
        .toSet
      if (edgeComponent.nonEmpty) Some(edgeComponent) else None
    }.sortWith { (left, right) =>
      compareInts(
        left.toVector.sorted.map(_.value),
        right.toVector.sorted.map(_.value)
      ) < 0
    }
  }

  def directCovers(
      edgeIds: Vector[EdgeId],
      widthBound: Int
  ): Vector[Set[EdgeId]] =
    (0 to math.min(widthBound, edgeIds.size)).iterator
      .flatMap(edgeIds.combinations)
      .map(_.toSet)
      .toVector

  private def unionVertices(
      hypergraph: QueryHypergraph,
      edgeIds: Set[EdgeId]
  ): Set[VariableId] =
    edgeIds.iterator
      .flatMap(id => hypergraph.edge(id).toVector)
      .flatMap(_.vertices)
      .toSet

  private def directConnectivity(
      hypergraph: QueryHypergraph,
      cover: Set[EdgeId]
  ): Boolean = {
    if (cover.isEmpty) false
    else {
      val verticesByEdge = cover.iterator.map { edgeId =>
        edgeId -> hypergraph.edge(edgeId).get.vertices.toSet
      }.toMap
      val neighbours = cover.iterator.map { edgeId =>
        edgeId -> (cover - edgeId).filter { other =>
          verticesByEdge(edgeId).intersect(verticesByEdge(other)).nonEmpty
        }
      }.toMap

      @annotation.tailrec
      def closure(
          frontier: Set[EdgeId],
          reached: Set[EdgeId]
      ): Set[EdgeId] = {
        val next = frontier.iterator.flatMap(neighbours).toSet -- reached
        if (next.isEmpty) reached else closure(next, reached ++ next)
      }

      val start = cover.min
      closure(Set(start), Set(start)) == cover
    }
  }

  private def orderedEdgeValues(edgeIds: Set[EdgeId]): Vector[Int] =
    edgeIds.toVector.sorted.map(_.value)

  private def compareWitnesses(
      left: OracleWitness,
      right: OracleWitness
  ): Int = {
    val lambda1 = compareCovers(left.lambda1, right.lambda1)
    if (lambda1 != 0) lambda1
    else {
      val lambda2 = compareCovers(left.lambda2, right.lambda2)
      if (lambda2 != 0) lambda2
      else compareInts(left.component, right.component)
    }
  }

  private def compareCovers(left: Vector[Int], right: Vector[Int]): Int = {
    val size = Integer.compare(left.size, right.size)
    if (size != 0) size else compareInts(left, right)
  }

  private def compareInts(left: Vector[Int], right: Vector[Int]): Int = {
    val commonLength = math.min(left.length, right.length)
    var index = 0
    while (index < commonLength) {
      val comparison = Integer.compare(left(index), right(index))
      if (comparison != 0) return comparison
      index += 1
    }
    Integer.compare(left.length, right.length)
  }

  private def right[A, B](
      result: Either[A, B],
      render: A => String = (value: A) => value.toString
  ): B = result.fold(error => throw new AssertionError(render(error)), identity)
}
