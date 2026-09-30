package at.ac.tuwien.dbai.yannakakis.core

private[core] object CoreModelTestSupport {
  def identifier[A](value: Either[ValidationError, A]): A =
    value.fold(
      error => throw new AssertionError(error.message),
      identity
    )

  def model[A](value: Either[Vector[ValidationError], A]): A =
    value.fold(
      errors =>
        throw new AssertionError(errors.map(_.message).mkString("; ")),
      identity
    )

  def relation(value: Int): RelationOccurrenceId =
    identifier(RelationOccurrenceId.from(value))

  def column(
      relationOccurrenceId: RelationOccurrenceId,
      ordinal: Int
  ): ColumnId = identifier(ColumnId.from(relationOccurrenceId, ordinal))

  def variable(value: Int): VariableId = identifier(VariableId.from(value))
  def edgeId(value: Int): EdgeId = identifier(EdgeId.from(value))
  def bagId(value: Int): BagId = identifier(BagId.from(value))
}
