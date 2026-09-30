package at.ac.tuwien.dbai.yannakakis.core

/** A deterministic, data-only description of an invalid core model value. */
sealed trait ValidationError extends Product with Serializable {
  def message: String
}

object ValidationError {
  final case class NegativeIdentity(identityType: String, value: Int)
      extends ValidationError {
    override val message: String =
      s"$identityType must be non-negative, but was $value"
  }

  final case class MissingValue(path: String) extends ValidationError {
    override val message: String = s"$path must not be null"
  }

  final case class ColumnOccurrenceMismatch(
      edgeId: EdgeId,
      expected: RelationOccurrenceId,
      columnId: ColumnId
  ) extends ValidationError {
    override val message: String =
      s"edge ${edgeId.value} belongs to relation occurrence ${expected.value}, " +
        s"but column $columnId belongs to ${columnId.relationOccurrenceId.value}"
  }

  final case class DuplicateColumn(edgeId: EdgeId, columnId: ColumnId)
      extends ValidationError {
    override val message: String =
      s"edge ${edgeId.value} binds column $columnId more than once"
  }

  final case class DuplicateEdgeId(edgeId: EdgeId) extends ValidationError {
    override val message: String =
      s"query hypergraph contains edge id ${edgeId.value} more than once"
  }

  final case class DuplicateRelationOccurrenceId(
      relationOccurrenceId: RelationOccurrenceId
  ) extends ValidationError {
    override val message: String =
      s"query hypergraph contains relation occurrence ${relationOccurrenceId.value} more than once"
  }

  implicit val ordering: Ordering[ValidationError] = Ordering.by(_.message)

  private[core] def canonical(
      errors: Iterable[ValidationError]
  ): Vector[ValidationError] = errors.toVector.sorted
}
