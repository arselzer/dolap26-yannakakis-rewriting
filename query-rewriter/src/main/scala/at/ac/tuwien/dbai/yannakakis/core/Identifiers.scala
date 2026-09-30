package at.ac.tuwien.dbai.yannakakis.core

/** Identity of one occurrence of a relation in a query. */
final class RelationOccurrenceId private (val value: Int) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: RelationOccurrenceId => value == that.value
    case _                          => false
  }

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString
}

object RelationOccurrenceId {
  def from(value: Int): Either[ValidationError, RelationOccurrenceId] =
    NumericIdentity.nonNegative(value, "relation occurrence id").map(
      new RelationOccurrenceId(_)
    )

  implicit val ordering: Ordering[RelationOccurrenceId] = Ordering.by(_.value)
}

/**
  * Identity of a column within a particular relation occurrence.
  *
  * The ordinal, rather than a SQL name, participates in identity. SQL permits
  * duplicate output names, so two equally named columns must remain distinct
  * when their ordinals differ. Names and aliases belong to later metadata.
  */
final class ColumnId private (
    val relationOccurrenceId: RelationOccurrenceId,
    val ordinal: Int
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: ColumnId =>
      relationOccurrenceId == that.relationOccurrenceId &&
        ordinal == that.ordinal
    case _ => false
  }

  override def hashCode(): Int =
    31 * relationOccurrenceId.hashCode() + ordinal.hashCode()
  override def toString: String = s"${relationOccurrenceId.value}[$ordinal]"
}

object ColumnId {
  def from(
      relationOccurrenceId: RelationOccurrenceId,
      ordinal: Int
  ): Either[ValidationError, ColumnId] = {
    if (relationOccurrenceId eq null) {
      Left(ValidationError.MissingValue("column id relation occurrence"))
    } else {
      NumericIdentity
        .nonNegative(ordinal, "column ordinal")
        .map(new ColumnId(relationOccurrenceId, _))
    }
  }

  implicit val ordering: Ordering[ColumnId] = new Ordering[ColumnId] {
    override def compare(left: ColumnId, right: ColumnId): Int = {
      val occurrenceComparison = RelationOccurrenceId.ordering.compare(
        left.relationOccurrenceId,
        right.relationOccurrenceId
      )
      if (occurrenceComparison != 0) occurrenceComparison
      else Integer.compare(left.ordinal, right.ordinal)
    }
  }
}

/** Identity of a logical variable (a hypergraph vertex). */
final class VariableId private (val value: Int) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: VariableId => value == that.value
    case _                => false
  }

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString
}

object VariableId {
  def from(value: Int): Either[ValidationError, VariableId] =
    NumericIdentity.nonNegative(value, "variable id").map(new VariableId(_))

  implicit val ordering: Ordering[VariableId] = Ordering.by(_.value)
}

/** Stable identity of a hyperedge, independent of its vertex set. */
final class EdgeId private (val value: Int) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: EdgeId => value == that.value
    case _            => false
  }

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString
}

object EdgeId {
  def from(value: Int): Either[ValidationError, EdgeId] =
    NumericIdentity.nonNegative(value, "edge id").map(new EdgeId(_))

  implicit val ordering: Ordering[EdgeId] = Ordering.by(_.value)
}

/** Stable identity of a decomposition bag, independent of its vertex set. */
final class BagId private (val value: Int) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: BagId => value == that.value
    case _           => false
  }

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString
}

object BagId {
  def from(value: Int): Either[ValidationError, BagId] =
    NumericIdentity.nonNegative(value, "bag id").map(new BagId(_))

  implicit val ordering: Ordering[BagId] = Ordering.by(_.value)
}

private[core] object NumericIdentity {
  def nonNegative(
      value: Int,
      identityType: String
  ): Either[ValidationError, Int] = {
    if (value < 0) Left(ValidationError.NegativeIdentity(identityType, value))
    else Right(value)
  }
}
