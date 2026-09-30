package at.ac.tuwien.dbai.yannakakis.core

/**
  * An immutable query hypergraph in canonical order.
  *
  * Edges are keyed by [[EdgeId]], not by their vertex sets. Each relation
  * occurrence contributes one edge; aliases of the same base relation are
  * therefore represented independently.
  */
final class QueryHypergraph private (val edges: Vector[Hyperedge])
    extends Serializable {
  val edgeIds: Vector[EdgeId] = edges.map(_.id)
  val relationOccurrenceIds: Vector[RelationOccurrenceId] =
    edges.map(_.relationOccurrenceId).sorted
  val variables: Vector[VariableId] =
    edges.iterator.flatMap(_.vertices).toVector.distinct.sorted
  val columns: Vector[ColumnId] =
    edges.iterator.flatMap(_.columns).toVector.distinct.sorted

  val incidence: Vector[(VariableId, Vector[EdgeId])] =
    variables.map(variableId => variableId -> incidentEdgeIds(variableId))

  private val edgeById: Map[EdgeId, Hyperedge] =
    edges.iterator.map(edge => edge.id -> edge).toMap
  private val edgeByOccurrence: Map[RelationOccurrenceId, Hyperedge] =
    edges.iterator.map(edge => edge.relationOccurrenceId -> edge).toMap

  def size: Int = edges.size
  def isEmpty: Boolean = edges.isEmpty
  def nonEmpty: Boolean = edges.nonEmpty

  def edge(id: EdgeId): Option[Hyperedge] = edgeById.get(id)

  def edgeFor(
      relationOccurrenceId: RelationOccurrenceId
  ): Option[Hyperedge] = edgeByOccurrence.get(relationOccurrenceId)

  def incidentEdges(variableId: VariableId): Vector[Hyperedge] =
    edges.filter(_.contains(variableId))

  def incidentEdgeIds(variableId: VariableId): Vector[EdgeId] =
    incidentEdges(variableId).map(_.id)

  override def equals(other: Any): Boolean = other match {
    case that: QueryHypergraph => edges == that.edges
    case _                     => false
  }

  override def hashCode(): Int = edges.hashCode()
  override def toString: String = edges.mkString("QueryHypergraph(", ",", ")")
}

object QueryHypergraph {
  val empty: QueryHypergraph = new QueryHypergraph(Vector.empty)

  /** Validates identity uniqueness and canonicalizes edge order. */
  def create(
      edges: Iterable[Hyperedge]
  ): Either[Vector[ValidationError], QueryHypergraph] = {
    val errors = Vector.newBuilder[ValidationError]

    val suppliedEdges =
      if (edges eq null) {
        errors += ValidationError.MissingValue("query hypergraph edges")
        Vector.empty
      } else {
        edges.iterator.toVector
      }

    val validEdges = suppliedEdges.flatMap { edge =>
      if (edge eq null) {
        errors += ValidationError.MissingValue("query hypergraph edge")
        None
      } else {
        Some(edge)
      }
    }

    validEdges
      .groupBy(_.id)
      .collect { case (edgeId, occurrences) if occurrences.size > 1 => edgeId }
      .toVector
      .sorted
      .foreach(edgeId => errors += ValidationError.DuplicateEdgeId(edgeId))

    validEdges
      .groupBy(_.relationOccurrenceId)
      .collect {
        case (relationOccurrenceId, occurrences) if occurrences.size > 1 =>
          relationOccurrenceId
      }
      .toVector
      .sorted
      .foreach(relationOccurrenceId =>
        errors += ValidationError.DuplicateRelationOccurrenceId(
          relationOccurrenceId
        )
      )

    val canonicalErrors = ValidationError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) {
      Left(canonicalErrors)
    } else if (validEdges.isEmpty) {
      Right(empty)
    } else {
      Right(new QueryHypergraph(validEdges.sorted))
    }
  }

  def from(
      edges: Iterable[Hyperedge]
  ): Either[Vector[ValidationError], QueryHypergraph] = create(edges)
}
