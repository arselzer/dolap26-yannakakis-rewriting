package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.core.{QueryHypergraph, VariableId}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition

import scala.collection.mutable

/**
  * Independent, exhaustive top-K oracle for deliberately tiny fixtures.
  *
  * This implementation does not use the production block index, component
  * engine, Boolean/top-K dynamic programs, tree ordering, or physicalizer.
  * It reconstructs the CandidateTD recurrence directly, expands every child
  * Cartesian product, canonicalizes structural trees, and only then removes
  * duplicate derivations and applies the public ranking contract.
  */
private[candidate] object ExactTopKCandidateTDOracleTestSupport {
  import CandidateTDOracleTestSupport.{
    OracleCandidateBag,
    OracleInput,
    coveredBag,
    coveredDecomposition,
    treeEdge
  }

  final case class OracleBounds(
      maxCandidateBags: Int = 12,
      maxBlocks: Int = 256,
      maxTreesPerBlock: Int = 200000,
      maxCartesianCombinations: Long = 1000000L
  )

  final case class OracleTree(
      bag: Vector[VariableId],
      children: Vector[OracleTree]
  ) {
    val nodeCount: BigInt = children.iterator.foldLeft(BigInt(1)) {
      (total, child) => total + child.nodeCount
    }

    /** Stable, human-readable form used only in assertion diagnostics. */
    lazy val signature: String = {
      val renderedBag = bag.iterator.map(_.value).mkString("{", ",", "}")
      children.iterator.map(_.signature).mkString(renderedBag + "[", ",", "]")
    }
  }

  final case class OracleRankedResult(
      tree: OracleTree,
      decomposition: CoveredDecomposition
  )

  final case class RootDerivation(
      basisBag: Vector[VariableId],
      tree: OracleTree
  )

  final case class OracleEnumerationStatistics(
      blockCount: Int,
      basisAlternativeCount: Long,
      rootBasisAlternativeCount: Int,
      cartesianCombinationCount: Long,
      maxNonEmptyChildArity: Int,
      maxMultiChoiceChildArity: Int,
      maxCartesianCardinality: Long,
      duplicateDerivationCount: Long
  )

  final case class OracleEnumeration(
      ranked: Vector[OracleRankedResult],
      rootDerivations: Vector[RootDerivation],
      statistics: OracleEnumerationStatistics
  ) {
    def take(k: Int): Vector[OracleRankedResult] = {
      require(k > 0, s"oracle top-K requires positive K, but was $k")
      ranked.take(k)
    }
  }

  private final case class OracleBlock(
      head: Vector[VariableId],
      tail: Vector[VariableId]
  ) {
    val headSet: Set[VariableId] = head.toSet
    val tailSet: Set[VariableId] = tail.toSet
    val extent: Set[VariableId] = headSet union tailSet
  }

  private final case class OracleBasis(
      bag: OracleCandidateBag,
      children: Vector[OracleBlock]
  )

  private final class StatisticsAccumulator {
    var basisAlternativeCount = 0L
    var rootBasisAlternativeCount = 0
    var cartesianCombinationCount = 0L
    var maxNonEmptyChildArity = 0
    var maxMultiChoiceChildArity = 0
    var maxCartesianCardinality = 0L
    var duplicateDerivationCount = 0L
  }

  /** Enumerates every distinct structural result within the explicit bounds. */
  def enumerate(
      input: OracleInput,
      bounds: OracleBounds = OracleBounds()
  ): OracleEnumeration = {
    require(input ne null, "oracle input is required")
    require(bounds ne null, "oracle bounds are required")
    require(
      input.candidates.size <= bounds.maxCandidateBags,
      s"top-K oracle candidate bound ${bounds.maxCandidateBags} exceeded by ${input.candidates.size}"
    )
    require(
      input.candidates.map(_.vertices).distinct.size == input.candidates.size,
      "top-K oracle requires one candidate per structural bag"
    )

    val candidates = input.candidates.sortWith(compareCandidates(_, _) < 0)
    val blocks = candidates.flatMap { candidate =>
      val components = vertexComponents(input.hypergraph, candidate.vertexSet)
      (components :+ Set.empty[VariableId]).map(component =>
        OracleBlock(candidate.vertices, component.toVector.sorted)
      )
    }.distinct.sortWith(compareBlocks(_, _) < 0)

    require(
      blocks.size <= bounds.maxBlocks,
      s"top-K oracle block bound ${bounds.maxBlocks} exceeded by ${blocks.size}"
    )

    val blocksByHead = blocks.groupBy(_.head).view
      .mapValues(_.sortWith(compareBlocks(_, _) < 0))
      .toMap
      .withDefaultValue(Vector.empty)
    val plans = mutable.LinkedHashMap.empty[OracleBlock, Vector[Option[OracleTree]]]
    val statistics = new StatisticsAccumulator

    // Blocks are ranked by (|tail|, |head|), so every recurrence dependency
    // has already been exhaustively materialized when its parent is visited.
    blocks.foreach { block =>
      if (block.tail.isEmpty) {
        // The formal empty-tail witness contributes no physical tree node.
        plans.put(block, Vector(None))
      } else {
        val raw = mutable.ArrayBuffer.empty[OracleTree]
        basesFor(input.hypergraph, block, candidates, blocksByHead).foreach {
          basis =>
            statistics.basisAlternativeCount += 1L
            expandBasis(basis, plans.toMap, bounds, statistics).foreach { tree =>
              raw += tree
              require(
                raw.size <= bounds.maxTreesPerBlock,
                s"top-K oracle per-block tree bound ${bounds.maxTreesPerBlock} exceeded for $block"
              )
            }
        }
        val distinct = canonicalDistinct(raw.toVector)
        statistics.duplicateDerivationCount += raw.size.toLong - distinct.size
        plans.put(block, distinct.map(Some(_)))
      }
    }

    val root = OracleBlock(Vector.empty, input.hypergraph.variables)
    val rootDerivations = mutable.ArrayBuffer.empty[RootDerivation]
    basesFor(input.hypergraph, root, candidates, blocksByHead, isRoot = true)
      .foreach { basis =>
        statistics.basisAlternativeCount += 1L
        statistics.rootBasisAlternativeCount += 1
        expandBasis(basis, plans.toMap, bounds, statistics).foreach { tree =>
          rootDerivations += RootDerivation(basis.bag.vertices, tree)
          require(
            rootDerivations.size <= bounds.maxTreesPerBlock,
            s"top-K oracle root tree bound ${bounds.maxTreesPerBlock} exceeded"
          )
        }
      }

    val distinctRankedTrees = canonicalDistinct(rootDerivations.map(_.tree).toVector)
    statistics.duplicateDerivationCount +=
      rootDerivations.size.toLong - distinctRankedTrees.size

    OracleEnumeration(
      distinctRankedTrees.map(tree =>
        OracleRankedResult(tree, physicalize(tree, candidates))
      ),
      rootDerivations.toVector.sortWith(compareRootDerivations(_, _) < 0),
      OracleEnumerationStatistics(
        blocks.size,
        statistics.basisAlternativeCount,
        statistics.rootBasisAlternativeCount,
        statistics.cartesianCombinationCount,
        statistics.maxNonEmptyChildArity,
        statistics.maxMultiChoiceChildArity,
        statistics.maxCartesianCardinality,
        statistics.duplicateDerivationCount
      )
    )
  }

  /** Small independent Cartesian oracle, also useful for focused helper tests. */
  def cartesianProduct[A](axes: Vector[Vector[A]]): Vector[Vector[A]] =
    axes.foldLeft(Vector(Vector.empty[A])) { (prefixes, axis) =>
      for {
        prefix <- prefixes
        value <- axis
      } yield prefix :+ value
    }

  def canonicalTree(
      bag: Vector[VariableId],
      children: Vector[OracleTree]
  ): OracleTree =
    OracleTree(bag.sorted, children.sortWith(compareStructural(_, _) < 0))

  def rankAndDeduplicate(trees: Vector[OracleTree]): Vector[OracleTree] =
    canonicalDistinct(trees.map(tree => canonicalTree(tree.bag, tree.children)))

  private def expandBasis(
      basis: OracleBasis,
      plans: Map[OracleBlock, Vector[Option[OracleTree]]],
      bounds: OracleBounds,
      statistics: StatisticsAccumulator
  ): Vector[OracleTree] = {
    val axes = basis.children.map(child => plans.getOrElse(child, Vector.empty))
    val nonEmptyAxes = basis.children.zip(axes).filterNot(_._1.tail.isEmpty)
    val combinationCount = axes.iterator.foldLeft(BigInt(1)) {
      (total, axis) => total * axis.size
    }

    require(
      combinationCount <= BigInt(Long.MaxValue),
      s"top-K oracle Cartesian cardinality exceeds Long for ${basis.bag.vertices}"
    )
    val combinations = combinationCount.longValue
    val required = BigInt(statistics.cartesianCombinationCount) + combinationCount
    require(
      required <= BigInt(bounds.maxCartesianCombinations),
      s"top-K oracle Cartesian bound ${bounds.maxCartesianCombinations} exceeded (required=$required)"
    )
    statistics.cartesianCombinationCount = required.longValue
    statistics.maxCartesianCardinality =
      math.max(statistics.maxCartesianCardinality, combinations)
    statistics.maxNonEmptyChildArity =
      math.max(statistics.maxNonEmptyChildArity, nonEmptyAxes.size)
    statistics.maxMultiChoiceChildArity = math.max(
      statistics.maxMultiChoiceChildArity,
      nonEmptyAxes.count(_._2.size > 1)
    )

    cartesianProduct(axes).map { selected =>
      val children = selected.flatten.sortWith(compareStructural(_, _) < 0)
      canonicalTree(basis.bag.vertices, children)
    }
  }

  private def basesFor(
      hypergraph: QueryHypergraph,
      parent: OracleBlock,
      candidates: Vector[OracleCandidateBag],
      blocksByHead: Map[Vector[VariableId], Vector[OracleBlock]],
      isRoot: Boolean = false
  ): Vector[OracleBasis] = {
    candidates.flatMap { candidate =>
      val bagSet = candidate.vertexSet
      val eligible =
        candidate.vertices != parent.head &&
          bagSet.subsetOf(parent.extent) &&
          (!isRoot || candidate.vertices.nonEmpty)

      if (!eligible) Vector.empty
      else {
        val children = blocksByHead
          .getOrElse(candidate.vertices, Vector.empty)
          .filter { child =>
            child.extent.subsetOf(parent.extent) &&
            child.tailSet.subsetOf(parent.tailSet) &&
            lowerRank(child, parent)
          }
          .sortWith(compareBlocks(_, _) < 0)
        val represented = children.iterator.foldLeft(bagSet) {
          (vertices, child) => vertices union child.tailSet
        }
        val coversTail = parent.tailSet.subsetOf(represented)
        val coversIncidentEdges = hypergraph.edges.forall { edge =>
          !edge.vertices.exists(parent.tailSet) || edge.vertices.forall(represented)
        }
        if (coversTail && coversIncidentEdges) {
          Vector(OracleBasis(candidate, children))
        } else Vector.empty
      }
    }
  }

  /** Components of the Gaifman graph after deleting `separator`. */
  private def vertexComponents(
      hypergraph: QueryHypergraph,
      separator: Set[VariableId]
  ): Vector[Set[VariableId]] = {
    val remaining = hypergraph.variables.filterNot(separator).toSet
    val neighbours = remaining.iterator.map { vertex =>
      val adjacent = hypergraph.edges.iterator
        .filter(_.vertices.contains(vertex))
        .flatMap(_.vertices)
        .filter(remaining)
        .toSet - vertex
      vertex -> adjacent
    }.toMap
    val unseen = mutable.LinkedHashSet.from(remaining.toVector.sorted)
    val result = Vector.newBuilder[Set[VariableId]]

    while (unseen.nonEmpty) {
      val start = unseen.head
      val reached = mutable.Set(start)
      val pending = mutable.Queue(start)
      unseen -= start
      while (pending.nonEmpty) {
        val current = pending.dequeue()
        neighbours.getOrElse(current, Set.empty).toVector.sorted.foreach { next =>
          if (unseen.remove(next)) {
            reached += next
            pending.enqueue(next)
          }
        }
      }
      result += reached.toSet
    }

    result.result().sortWith { (left, right) =>
      compareVariables(left.toVector.sorted, right.toVector.sorted) < 0
    }
  }

  private def physicalize(
      tree: OracleTree,
      candidates: Vector[OracleCandidateBag]
  ): CoveredDecomposition = {
    val byVertices = candidates.iterator.map(candidate =>
      candidate.vertices -> candidate
    ).toMap
    val bags = Vector.newBuilder[at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredBag]
    val edges = Vector.newBuilder[at.ac.tuwien.dbai.yannakakis.core.decomposition.BagTreeEdge]
    var nextId = 0

    def visit(node: OracleTree, parent: Option[Int]): Int = {
      val id = nextId
      nextId += 1
      val candidate = byVertices.getOrElse(
        node.bag,
        throw new AssertionError(s"oracle tree uses unknown candidate ${node.bag}")
      )
      val firstCover = candidate.physicalCovers.headOption.getOrElse {
        throw new AssertionError(s"oracle candidate ${candidate.vertices} has no physical cover")
      }
      bags += coveredBag(
        id,
        candidate.vertices.map(_.value),
        firstCover.map(_.value)
      )
      parent.foreach(parentId => edges += treeEdge(parentId, id))
      node.children.foreach(child => visit(child, Some(id)))
      id
    }

    val root = visit(tree, None)
    coveredDecomposition(root, bags.result(), edges.result())
  }

  private def canonicalDistinct(trees: Vector[OracleTree]): Vector[OracleTree] =
    trees.distinct.sortWith(compareRanked(_, _) < 0)

  private def compareRootDerivations(
      left: RootDerivation,
      right: RootDerivation
  ): Int = {
    val tree = compareRanked(left.tree, right.tree)
    if (tree != 0) tree else compareVariables(left.basisBag, right.basisBag)
  }

  private def compareRanked(left: OracleTree, right: OracleTree): Int = {
    val count = left.nodeCount.compare(right.nodeCount)
    if (count != 0) count else compareStructural(left, right)
  }

  private def compareStructural(left: OracleTree, right: OracleTree): Int = {
    val bag = compareVariables(left.bag, right.bag)
    if (bag != 0) bag
    else {
      val common = math.min(left.children.size, right.children.size)
      var index = 0
      while (index < common) {
        val child = compareStructural(left.children(index), right.children(index))
        if (child != 0) return child
        index += 1
      }
      Integer.compare(left.children.size, right.children.size)
    }
  }

  private def compareCandidates(
      left: OracleCandidateBag,
      right: OracleCandidateBag
  ): Int = compareVariables(left.vertices, right.vertices)

  private def compareBlocks(left: OracleBlock, right: OracleBlock): Int = {
    val tailSize = Integer.compare(left.tail.size, right.tail.size)
    if (tailSize != 0) tailSize
    else {
      val headSize = Integer.compare(left.head.size, right.head.size)
      if (headSize != 0) headSize
      else {
        val head = compareVariables(left.head, right.head)
        if (head != 0) head else compareVariables(left.tail, right.tail)
      }
    }
  }

  private def lowerRank(child: OracleBlock, parent: OracleBlock): Boolean =
    child.tail.size < parent.tail.size ||
      (child.tail.size == parent.tail.size &&
        child.head.size < parent.head.size)

  private def compareVariables(
      left: Vector[VariableId],
      right: Vector[VariableId]
  ): Int = {
    val common = math.min(left.size, right.size)
    var index = 0
    while (index < common) {
      val comparison = VariableId.ordering.compare(left(index), right(index))
      if (comparison != 0) return comparison
      index += 1
    }
    Integer.compare(left.size, right.size)
  }
}
