package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.EdgeCover
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition

import scala.collection.mutable

/** A canonical structural candidate bag. */
final class CandidateBagKey private[candidate] (
    val vertices: Vector[VariableId]
) extends Serializable {
  private[candidate] val vertexSet: Set[VariableId] = vertices.toSet

  override def equals(other: Any): Boolean = other match {
    case that: CandidateBagKey => vertices == that.vertices
    case _                     => false
  }

  override def hashCode(): Int = vertices.hashCode()
  override def toString: String = vertices.mkString("CandidateBagKey(", ",", ")")
}

object CandidateBagKey {
  private[candidate] def canonical(
      vertices: Iterable[VariableId]
  ): CandidateBagKey = new CandidateBagKey(vertices.iterator.toVector.sorted)

  implicit val ordering: Ordering[CandidateBagKey] =
    (left: CandidateBagKey, right: CandidateBagKey) =>
      CandidateOrdering.compareVectors(left.vertices, right.vertices)
}

/** A structural candidate and all physical covers admissible in the mode. */
final class CandidateBag private[candidate] (
    val key: CandidateBagKey,
    val physicalCovers: Vector[EdgeCover]
) extends Serializable {
  def vertices: Vector[VariableId] = key.vertices

  override def equals(other: Any): Boolean = other match {
    case that: CandidateBag =>
      key == that.key && physicalCovers == that.physicalCovers
    case _ => false
  }

  override def hashCode(): Int = (key, physicalCovers).hashCode()
  override def toString: String =
    s"CandidateBag($key,${physicalCovers.mkString("[", ",", "]")})"
}

/** A vertex component, with an internal empty sentinel for base blocks. */
final class CandidateComponent private[candidate] (
    val vertices: Vector[VariableId]
) extends Serializable {
  private[candidate] val vertexSet: Set[VariableId] = vertices.toSet
  def isEmpty: Boolean = vertices.isEmpty
  def nonEmpty: Boolean = vertices.nonEmpty

  override def equals(other: Any): Boolean = other match {
    case that: CandidateComponent => vertices == that.vertices
    case _                        => false
  }

  override def hashCode(): Int = vertices.hashCode()
  override def toString: String =
    vertices.mkString("CandidateComponent(", ",", ")")
}

object CandidateComponent {
  private[candidate] val empty: CandidateComponent =
    new CandidateComponent(Vector.empty)

  private[candidate] def canonical(
      vertices: Iterable[VariableId]
  ): CandidateComponent =
    new CandidateComponent(vertices.iterator.toVector.sorted)

  implicit val ordering: Ordering[CandidateComponent] =
    (left: CandidateComponent, right: CandidateComponent) =>
      CandidateOrdering.compareVectors(left.vertices, right.vertices)
}

/** A canonical component-normal-form block (head separator, tail component). */
final class CandidateBlock private[candidate] (
    val head: CandidateBagKey,
    val tail: CandidateComponent
) extends Serializable {
  private[candidate] val extent: Set[VariableId] =
    head.vertexSet union tail.vertexSet

  override def equals(other: Any): Boolean = other match {
    case that: CandidateBlock => head == that.head && tail == that.tail
    case _                    => false
  }

  override def hashCode(): Int = (head, tail).hashCode()
  override def toString: String = s"CandidateBlock($head,$tail)"
}

object CandidateBlock {
  implicit val ordering: Ordering[CandidateBlock] =
    new Ordering[CandidateBlock] {
      override def compare(
          left: CandidateBlock,
          right: CandidateBlock
      ): Int = {
        val tailSize = Integer.compare(left.tail.vertices.size, right.tail.vertices.size)
        if (tailSize != 0) tailSize
        else {
          val headSize = Integer.compare(left.head.vertices.size, right.head.vertices.size)
          if (headSize != 0) headSize
          else {
            val head = CandidateBagKey.ordering.compare(left.head, right.head)
            if (head != 0) head
            else CandidateComponent.ordering.compare(left.tail, right.tail)
          }
        }
      }
    }
}

/** One exact recurrence alternative for a non-empty block. */
final class CandidateBasis private[candidate] (
    val bag: CandidateBagKey,
    val subordinateBlocks: Vector[CandidateBlock]
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: CandidateBasis =>
      bag == that.bag && subordinateBlocks == that.subordinateBlocks
    case _ => false
  }

  override def hashCode(): Int = (bag, subordinateBlocks).hashCode()
  override def toString: String =
    s"CandidateBasis($bag,${subordinateBlocks.mkString("[", ",", "]")})"
}

/** A deterministic structural CandidateTD witness. */
final class CandidateTreeNode private[candidate] (
    val bag: CandidateBagKey,
    val children: Vector[CandidateTreeNode]
) extends Serializable {
  val nodeCount: BigInt = children.iterator.foldLeft(BigInt(1)) {
    (total, child) => total + child.nodeCount
  }
  private[candidate] val structuralHash: Int =
    CandidateOrdering.treeNodeHash(bag, children)

  def size: BigInt = nodeCount

  override def equals(other: Any): Boolean = other match {
    case that: CandidateTreeNode =>
      (this eq that) || CandidateOrdering.compareTreeNodes(this, that) == 0
    case _ => false
  }

  override def hashCode(): Int = structuralHash
  override def toString: String = CandidateOrdering.renderTreeNode(this)
}

final class CandidateTree private[candidate] (
    val root: CandidateTreeNode
) extends Serializable {
  val nodeCount: BigInt = root.nodeCount
  def size: BigInt = nodeCount

  override def equals(other: Any): Boolean = other match {
    case that: CandidateTree =>
      (this eq that) || CandidateOrdering.compareTreeNodes(root, that.root) == 0
    case _ => false
  }

  override def hashCode(): Int = root.hashCode()
  override def toString: String =
    s"CandidateTree(${CandidateOrdering.renderTreeNode(root)})"
}

final case class CandidateStatistics(
    candidateBagCount: Int,
    blockCount: Int,
    basisAlternativeCount: Long,
    satisfiedBlockCount: Int
) extends Serializable

sealed trait CandidateDecision extends Product with Serializable

object CandidateDecision {
  final case class Accepted(
      witness: CandidateTree,
      decomposition: CoveredDecomposition,
      certificate: CandidateCertificate,
      statistics: CandidateStatistics
  ) extends CandidateDecision

  final case class Rejected(statistics: CandidateStatistics)
      extends CandidateDecision
}

/** The component assigned to one oriented decomposition-tree arc. */
final case class ComponentNormalFormArc(
    parent: BagId,
    child: BagId,
    component: Vector[VariableId]
) extends Serializable

/** Data-only evidence returned by the independent final validator. */
final case class CandidateCertificate(
    componentArcs: Vector[ComponentNormalFormArc],
    requireConnectedCover: Boolean,
    bagCount: Int
) extends Serializable

private[candidate] object CandidateOrdering {
  def compareVectors[A](left: Vector[A], right: Vector[A])(implicit
      ordering: Ordering[A]
  ): Int = {
    val common = math.min(left.size, right.size)
    var index = 0
    while (index < common) {
      val result = ordering.compare(left(index), right(index))
      if (result != 0) return result
      index += 1
    }
    Integer.compare(left.size, right.size)
  }

  def compareTreeNodes(
      left: CandidateTreeNode,
      right: CandidateTreeNode
  ): Int = {
    if (left eq right) return 0

    val pending = mutable.Stack[TreeComparisonFrame](
      CompareNodes(left, right)
    )
    while (pending.nonEmpty) {
      pending.pop() match {
        case CompareNodes(first, second) =>
          if (first ne second) {
            val bagResult = CandidateBagKey.ordering.compare(
              first.bag,
              second.bag
            )
            if (bagResult != 0) return bagResult
            pending.push(CompareChildren(first.children, second.children, 0))
          }
        case CompareChildren(first, second, index) =>
          val common = math.min(first.size, second.size)
          if (index < common) {
            pending.push(CompareChildren(first, second, index + 1))
            pending.push(CompareNodes(first(index), second(index)))
          } else {
            val sizeResult = Integer.compare(first.size, second.size)
            if (sizeResult != 0) return sizeResult
          }
      }
    }
    0
  }

  def compareRankedTrees(
      left: CandidateTreeNode,
      right: CandidateTreeNode
  ): Int = {
    val sizeResult = left.nodeCount.compare(right.nodeCount)
    if (sizeResult != 0) sizeResult else compareTreeNodes(left, right)
  }

  private[candidate] def treeNodeHash(
      bag: CandidateBagKey,
      children: Vector[CandidateTreeNode]
  ): Int = {
    var result = 31 + bag.hashCode()
    children.foreach(child => result = 31 * result + child.structuralHash)
    31 * result + children.size
  }

  private[candidate] def renderTreeNode(root: CandidateTreeNode): String = {
    val result = new StringBuilder
    result.append("CandidateTreeNode(")
    result.append(root.bag)
    result.append(",[")
    val pending = mutable.Stack[(CandidateTreeNode, Int)](root -> 0)

    while (pending.nonEmpty) {
      val (node, index) = pending.pop()
      if (index < node.children.size) {
        if (index > 0) result.append(',')
        pending.push(node -> (index + 1))
        val child = node.children(index)
        result.append("CandidateTreeNode(")
        result.append(child.bag)
        result.append(",[")
        pending.push(child -> 0)
      } else {
        result.append("])")
      }
    }

    result.result()
  }

  private sealed trait TreeComparisonFrame
  private final case class CompareNodes(
      left: CandidateTreeNode,
      right: CandidateTreeNode
  ) extends TreeComparisonFrame
  private final case class CompareChildren(
      left: Vector[CandidateTreeNode],
      right: Vector[CandidateTreeNode],
      index: Int
  ) extends TreeComparisonFrame
}
