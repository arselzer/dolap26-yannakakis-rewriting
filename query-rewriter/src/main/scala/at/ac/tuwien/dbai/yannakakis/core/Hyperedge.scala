package at.ac.tuwien.dbai.yannakakis.core

/**
  * One relation occurrence in a query hypergraph.
  *
  * Edge identity is explicit and never derived from `vertices`. Consequently,
  * two self-join occurrences may have identical vertex sets while remaining
  * distinct edges.
  */
final class Hyperedge private (
    val id: EdgeId,
    val relationOccurrenceId: RelationOccurrenceId,
    val bindings: Vector[(ColumnId, VariableId)]
) extends Serializable {
  val columns: Vector[ColumnId] = bindings.map(_._1)
  val vertices: Vector[VariableId] = bindings.map(_._2).distinct.sorted

  private val vertexSet: Set[VariableId] = vertices.toSet

  def arity: Int = bindings.size

  def contains(variableId: VariableId): Boolean = vertexSet.contains(variableId)

  def variableFor(columnId: ColumnId): Option[VariableId] =
    bindings.collectFirst { case (`columnId`, variableId) => variableId }

  def sameVerticesAs(other: Hyperedge): Boolean =
    (other ne null) && vertices == other.vertices

  override def equals(other: Any): Boolean = other match {
    case that: Hyperedge =>
      id == that.id &&
        relationOccurrenceId == that.relationOccurrenceId &&
        bindings == that.bindings
    case _ => false
  }

  override def hashCode(): Int = (id, relationOccurrenceId, bindings).hashCode()

  override def toString: String = {
    val renderedBindings = bindings
      .map { case (columnId, variableId) => s"$columnId->${variableId.value}" }
      .mkString(",")
    s"Hyperedge(${id.value},${relationOccurrenceId.value},[$renderedBindings])"
  }
}

object Hyperedge {
  private val bindingOrdering: Ordering[(ColumnId, VariableId)] =
    new Ordering[(ColumnId, VariableId)] {
      override def compare(
          left: (ColumnId, VariableId),
          right: (ColumnId, VariableId)
      ): Int = {
        val columnComparison = ColumnId.ordering.compare(left._1, right._1)
        if (columnComparison != 0) columnComparison
        else VariableId.ordering.compare(left._2, right._2)
      }
    }

  /**
    * Validates and canonicalizes a relation occurrence's column bindings.
    * Repeated variables are valid, whereas a column may only be bound once.
    */
  def create(
      id: EdgeId,
      relationOccurrenceId: RelationOccurrenceId,
      bindings: Iterable[(ColumnId, VariableId)]
  ): Either[Vector[ValidationError], Hyperedge] = {
    val errors = Vector.newBuilder[ValidationError]

    if (id eq null) {
      errors += ValidationError.MissingValue("hyperedge id")
    }
    if (relationOccurrenceId eq null) {
      errors += ValidationError.MissingValue("hyperedge relation occurrence")
    }

    val suppliedBindings =
      if (bindings eq null) {
        errors += ValidationError.MissingValue("hyperedge bindings")
        Vector.empty
      } else {
        bindings.iterator.toVector
      }

    val validBindings = suppliedBindings.zipWithIndex.flatMap {
      case (null, index) =>
        errors += ValidationError.MissingValue(s"hyperedge binding $index")
        None
      case ((columnId, variableId), index) =>
        if (columnId eq null) {
          errors += ValidationError.MissingValue(
            s"hyperedge binding $index column id"
          )
        }
        if (variableId eq null) {
          errors += ValidationError.MissingValue(
            s"hyperedge binding $index variable id"
          )
        }
        if (
          (id ne null) &&
          (relationOccurrenceId ne null) &&
          (columnId ne null) &&
          columnId.relationOccurrenceId != relationOccurrenceId
        ) {
          errors += ValidationError.ColumnOccurrenceMismatch(
            id,
            relationOccurrenceId,
            columnId
          )
        }

        if ((columnId ne null) && (variableId ne null)) {
          Some(columnId -> variableId)
        } else {
          None
        }
    }

    if (id ne null) {
      validBindings
        .groupBy(_._1)
        .collect { case (columnId, occurrences) if occurrences.size > 1 => columnId }
        .toVector
        .sorted
        .foreach(columnId => errors += ValidationError.DuplicateColumn(id, columnId))
    }

    val canonicalErrors = ValidationError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) {
      Left(canonicalErrors)
    } else {
      Right(
        new Hyperedge(
          id,
          relationOccurrenceId,
          validBindings.sorted(bindingOrdering)
        )
      )
    }
  }

  def from(
      id: EdgeId,
      relationOccurrenceId: RelationOccurrenceId,
      bindings: Iterable[(ColumnId, VariableId)]
  ): Either[Vector[ValidationError], Hyperedge] =
    create(id, relationOccurrenceId, bindings)

  implicit val ordering: Ordering[Hyperedge] = new Ordering[Hyperedge] {
    override def compare(left: Hyperedge, right: Hyperedge): Int = {
      val idComparison = EdgeId.ordering.compare(left.id, right.id)
      if (idComparison != 0) {
        idComparison
      } else {
        val occurrenceComparison = RelationOccurrenceId.ordering.compare(
          left.relationOccurrenceId,
          right.relationOccurrenceId
        )
        if (occurrenceComparison != 0) {
          occurrenceComparison
        } else {
          compareBindings(left.bindings, right.bindings)
        }
      }
    }

    private def compareBindings(
        left: Vector[(ColumnId, VariableId)],
        right: Vector[(ColumnId, VariableId)]
    ): Int = {
      val commonLength = math.min(left.length, right.length)
      var index = 0
      while (index < commonLength) {
        val comparison = bindingOrdering.compare(left(index), right(index))
        if (comparison != 0) return comparison
        index += 1
      }
      Integer.compare(left.length, right.length)
    }
  }
}
