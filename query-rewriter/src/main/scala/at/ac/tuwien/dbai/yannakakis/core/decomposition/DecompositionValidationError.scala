package at.ac.tuwien.dbai.yannakakis.core.decomposition

import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, VariableId}

/** Deterministic, data-only description of an invalid decomposition. */
sealed trait DecompositionValidationError extends Product with Serializable {
  def message: String
}

object DecompositionValidationError {
  final case class MissingValue(path: String)
      extends DecompositionValidationError {
    override val message: String = s"$path must not be null"
  }

  final case class DuplicateBagId(bagId: BagId)
      extends DecompositionValidationError {
    override val message: String =
      s"decomposition contains bag id ${bagId.value} more than once"
  }

  final case class MissingRootBag(root: BagId)
      extends DecompositionValidationError {
    override val message: String =
      s"decomposition root ${root.value} does not identify a bag"
  }

  final case class UnknownTreeEndpoint(
      treeEdge: BagTreeEdge,
      bagId: BagId
  ) extends DecompositionValidationError {
    override val message: String =
      s"tree edge $treeEdge refers to unknown bag ${bagId.value}"
  }

  final case class DuplicateTreeEdge(treeEdge: BagTreeEdge)
      extends DecompositionValidationError {
    override val message: String =
      s"decomposition contains tree edge $treeEdge more than once"
  }

  final case class SelfLoop(treeEdge: BagTreeEdge)
      extends DecompositionValidationError {
    override val message: String =
      s"decomposition tree contains self-loop $treeEdge"
  }

  final case class DisconnectedTree(unreachable: Vector[BagId])
      extends DecompositionValidationError {
    override val message: String =
      s"decomposition tree is disconnected; unreachable bags: ${unreachable.map(_.value).mkString(",")}"
  }

  case object CyclicTree extends DecompositionValidationError {
    override val message: String = "decomposition tree contains a cycle"
  }

  final case class UnknownCoverEdge(bagId: BagId, edgeId: EdgeId)
      extends DecompositionValidationError {
    override val message: String =
      s"bag ${bagId.value} cover refers to unknown edge ${edgeId.value}"
  }

  final case class HyperedgeNotCovered(edgeId: EdgeId)
      extends DecompositionValidationError {
    override val message: String =
      s"hyperedge ${edgeId.value} is not contained in any bag"
  }

  final case class BagVertexNotCovered(
      bagId: BagId,
      variableId: VariableId
  ) extends DecompositionValidationError {
    override val message: String =
      s"bag ${bagId.value} vertex ${variableId.value} is not contained in the union of its cover"
  }

  final case class RunningIntersectionViolation(
      variableId: VariableId,
      containingBags: Vector[BagId]
  ) extends DecompositionValidationError {
    override val message: String =
      s"bags containing vertex ${variableId.value} are disconnected: ${containingBags.map(_.value).mkString(",")}"
  }

  implicit val ordering: Ordering[DecompositionValidationError] =
    Ordering.by(_.message)

  private[decomposition] def canonical(
      errors: Iterable[DecompositionValidationError]
  ): Vector[DecompositionValidationError] =
    errors.toVector.distinct.sorted
}
