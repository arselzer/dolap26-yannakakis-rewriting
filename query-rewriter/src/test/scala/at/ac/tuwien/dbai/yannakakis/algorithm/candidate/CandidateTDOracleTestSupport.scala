package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.AdmissibleSoftBagGeneration
import at.ac.tuwien.dbai.yannakakis.core._
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  BagTreeEdge,
  CoveredBag,
  CoveredDecomposition,
  DecompositionValidationError
}

import scala.collection.mutable

/** Independent, deliberately small-scale CandidateTD test machinery. */
private[candidate] object CandidateTDOracleTestSupport {
  final case class OracleCandidateBag(
      vertices: Vector[VariableId],
      physicalCovers: Vector[Vector[EdgeId]]
  ) {
    val vertexSet: Set[VariableId] = vertices.toSet
  }

  final case class OracleInput(
      hypergraph: QueryHypergraph,
      candidates: Vector[OracleCandidateBag]
  )

  final case class OracleBlock(
      head: Vector[VariableId],
      tail: Vector[VariableId]
  ) {
    val headSet: Set[VariableId] = head.toSet
    val tailSet: Set[VariableId] = tail.toSet
    val extent: Set[VariableId] = headSet ++ tailSet
    val rank: (Int, Int) = tail.size -> head.size
  }

  final case class OracleBasis(
      candidate: OracleCandidateBag,
      children: Vector[OracleBlock]
  )

  final case class OracleDecision(
      accepted: Boolean,
      bases: Map[OracleBlock, OracleBasis],
      root: OracleBlock,
      canonicalDecomposition: Option[CoveredDecomposition]
  )

  private final case class RootedTree(
      candidateIndex: Int,
      children: Vector[RootedTree]
  )

  def variable(value: Int): VariableId = core(VariableId.from(value))
  def edgeId(value: Int): EdgeId = core(EdgeId.from(value))
  def relation(value: Int): RelationOccurrenceId =
    core(RelationOccurrenceId.from(value))
  def bagId(value: Int): BagId = core(BagId.from(value))

  def graph(vertexSets: Seq[Seq[Int]]): QueryHypergraph =
    graphWithIds(vertexSets.zipWithIndex.map { case (vertices, index) =>
      index -> vertices
    })

  def graphWithIds(edges: Seq[(Int, Seq[Int])]): QueryHypergraph = {
    val hyperedges = edges.map { case (numericId, vertices) =>
      val occurrence = relation(numericId)
      val bindings = vertices.zipWithIndex.map { case (numericVertex, ordinal) =>
        core(ColumnId.from(occurrence, ordinal)) -> variable(numericVertex)
      }
      coreMany(
        Hyperedge.create(edgeId(numericId), occurrence, bindings)
      )
    }
    coreMany(QueryHypergraph.create(hyperedges))
  }

  def oracleInput(
      hypergraph: QueryHypergraph,
      generation: AdmissibleSoftBagGeneration
  ): OracleInput = {
    val candidates = generation.candidates.map { candidate =>
      OracleCandidateBag(
        candidate.vertices,
        candidate.physicalCovers.map(_.cover.edgeIds)
      )
    }
    OracleInput(hypergraph, candidates.sortWith(compareCandidateBags(_, _) < 0))
  }

  /**
    * Direct Algorithm-1 fixed point. Components and basis checks here do not
    * call any CandidateTD production helper.
    */
  def directDecision(input: OracleInput): OracleDecision = {
    val empty = Vector.empty[VariableId]
    val heads = (input.candidates.map(_.vertices) :+ empty).distinct
      .sortWith(compareVariables(_, _) < 0)
    val blocks = heads.flatMap { head =>
      val components = vertexComponents(input.hypergraph, head.toSet)
      (components :+ Set.empty[VariableId]).distinct.map { component =>
        OracleBlock(head, component.toVector.sorted)
      }
    }.distinct.sortWith(compareBlocks(_, _) < 0)
    val blocksByHead = blocks.groupBy(_.head).withDefaultValue(Vector.empty)
    val root = OracleBlock(empty, input.hypergraph.variables)
    val satisfied = mutable.LinkedHashMap.empty[OracleBlock, OracleBasis]

    blocks.filter(_.tail.isEmpty).foreach { block =>
      // Empty-tail blocks have the formal trivial basis. They never create a
      // physical node during reconstruction.
      satisfied.put(
        block,
        OracleBasis(
          OracleCandidateBag(Vector.empty, Vector.empty),
          Vector.empty
        )
      )
    }

    var changed = true
    while (changed) {
      changed = false
      val pending = (blocks :+ root).distinct
        .filterNot(satisfied.contains)
        .sortWith(compareBlocks(_, _) < 0)
      pending.foreach { block =>
        input.candidates.iterator
          .filter(candidate => candidate.vertices != block.head)
          .flatMap { candidate =>
            basisStructure(
              input.hypergraph,
              block,
              candidate,
              blocksByHead(candidate.vertices)
            )
          }
          .filter(_.children.forall(satisfied.contains))
          .take(1)
          .foreach { found =>
            satisfied.put(block, found)
            changed = true
          }
      }
    }

    val planMemo = mutable.Map.empty[
      OracleBlock,
      Option[Vector[RootedTree]]
    ]

    def canonicalPlan(block: OracleBlock): Option[Vector[RootedTree]] =
      planMemo.getOrElseUpdate(
        block,
        if (block.tail.isEmpty) Some(Vector.empty)
        else {
          val alternatives = input.candidates.flatMap { candidate =>
            if (candidate.vertices == block.head) Vector.empty
            else {
              basisStructure(
                input.hypergraph,
                block,
                candidate,
                blocksByHead(candidate.vertices)
              ).flatMap { found =>
                val children = found.children.map(canonicalPlan)
                if (children.forall(_.nonEmpty)) {
                  val childTrees = children.flatten.flatten
                  val candidateIndex = input.candidates.indexOf(candidate)
                  Some(
                    canonicalize(
                      RootedTree(candidateIndex, childTrees),
                      input.candidates
                    )
                  )
                } else None
              }.toVector
            }
          }
          bestTree(alternatives, input.candidates).map(Vector(_))
        }
      )

    val canonical = canonicalPlan(root)
      .flatMap(_.headOption)
      .map(tree => physicalize(tree, input.candidates))
    OracleDecision(
      satisfied.contains(root),
      satisfied.toMap,
      root,
      canonical
    )
  }

  /**
    * Exhaustively enumerates rooted trees over distinct candidate bags. This
    * is intentionally bounded and intended only for tiny oracle fixtures.
    */
  def bruteForceDistinctBagCanonical(
      input: OracleInput,
      maxCandidateBags: Int = 7
  ): Option[CoveredDecomposition] =
    bruteForceDistinctBagAll(input, maxCandidateBags).headOption

  /**
    * Exhaustively enumerates every structurally distinct valid rooted tree in
    * which each candidate bag occurs at most once. This is independent of the
    * CandidateTD recurrence, but it is only a sub-universe of production:
    * production permits the same structural bag at non-adjacent tree nodes.
    * Results are ranked by node count followed by the full canonical rooted
    * signature.
    */
  def bruteForceDistinctBagAll(
      input: OracleInput,
      maxCandidateBags: Int = 7
  ): Vector[CoveredDecomposition] = {
    val usable = input.candidates
    require(
      usable.size <= maxCandidateBags,
      s"brute-force oracle bound $maxCandidateBags exceeded by ${usable.size} bags"
    )

    val validTrees = Vector.newBuilder[RootedTree]
    nonEmptySubsets(usable.indices.toVector).foreach { selected =>
      undirectedTrees(selected).foreach { edges =>
        selected.filter(index => usable(index).vertices.nonEmpty).foreach { root =>
          val rooted = orient(root, None, adjacency(selected, edges))
          if (
            isTreeDecomposition(input.hypergraph, rooted, usable) &&
            isComponentNormalForm(input.hypergraph, rooted, usable)
          ) {
            validTrees += canonicalize(rooted, usable)
          }
        }
      }
    }

    validTrees.result()
      .distinct
      .sortWith { (left, right) =>
        val size = Integer.compare(flatten(left).size, flatten(right).size)
        if (size != 0) size < 0
        else compareTrees(left, right, usable) < 0
      }
      .map(tree => physicalize(tree, usable))
  }

  def isIndependentCompNF(
      hypergraph: QueryHypergraph,
      decomposition: CoveredDecomposition
  ): Boolean = {
    val bagById = decomposition.bags.iterator.map(bag => bag.id -> bag).toMap
    if (!bagById.contains(decomposition.root)) false
    else {
      val adjacency = decomposition.bagIds.map { id =>
        id -> decomposition.neighbors(id).toSet
      }.toMap

      def subtree(start: BagId, parent: Option[BagId]): Set[BagId] =
        Set(start) ++ adjacency
          .getOrElse(start, Set.empty)
          .filterNot(parent.contains)
          .flatMap(next => subtree(next, Some(start)))

      def visit(parentId: BagId, ancestor: Option[BagId]): Boolean = {
        val parent = bagById(parentId)
        adjacency
          .getOrElse(parentId, Set.empty)
          .filterNot(ancestor.contains)
          .forall { childId =>
            val child = bagById(childId)
            val subtreeVertices = subtree(childId, Some(parentId)).iterator
              .flatMap(id => bagById(id).vertices)
              .toSet
            val intersection = parent.vertices.toSet.intersect(child.vertices.toSet)
            val matchingComponents = vertexComponents(
              hypergraph,
              parent.vertices.toSet
            ).count(component => component ++ intersection == subtreeVertices)
            matchingComponents == 1 && visit(childId, Some(parentId))
          }
      }

      visit(decomposition.root, None)
    }
  }

  def coveredBag(
      id: Int,
      vertices: Iterable[Int],
      cover: Iterable[Int]
  ): CoveredBag =
    decomposition(
      CoveredBag.create(
        bagId(id),
        vertices.map(variable),
        cover.map(edgeId)
      )
    )

  def treeEdge(first: Int, second: Int): BagTreeEdge =
    decomposition(BagTreeEdge.create(bagId(first), bagId(second)))

  def coveredDecomposition(
      root: Int,
      bags: Iterable[CoveredBag],
      edges: Iterable[BagTreeEdge]
  ): CoveredDecomposition =
    decomposition(CoveredDecomposition.create(bagId(root), bags, edges))

  private def basisStructure(
      hypergraph: QueryHypergraph,
      block: OracleBlock,
      candidate: OracleCandidateBag,
      headedBlocks: Vector[OracleBlock]
  ): Option[OracleBasis] = {
    val candidateSet = candidate.vertexSet
    if (!candidateSet.subsetOf(block.extent)) None
    else {
      val children = headedBlocks.filter { child =>
        child.extent.subsetOf(block.extent) &&
        child.tailSet.subsetOf(block.tailSet) &&
        lowerRank(child, block)
      }
      val childExtent = candidateSet ++ children.iterator.flatMap(_.tail).toSet
      val coversTail = block.tailSet.subsetOf(childExtent)
      val coversIncidentEdges = hypergraph.edges
        .filter(edge => edge.vertices.exists(block.tailSet))
        .forall(edge => edge.vertices.forall(childExtent))
      if (coversTail && coversIncidentEdges) {
        Some(OracleBasis(candidate, children))
      } else None
    }
  }

  /** Components of the Gaifman graph induced after deleting `separator`. */
  private[candidate] def vertexComponents(
      hypergraph: QueryHypergraph,
      separator: Set[VariableId]
  ): Vector[Set[VariableId]] = {
    val remaining = hypergraph.variables.filterNot(separator).toSet
    val neighbours = remaining.iterator.map { vertex =>
      val adjacent = hypergraph.edges.iterator
        .filter(_.contains(vertex))
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
      val queue = mutable.Queue(start)
      unseen -= start
      while (queue.nonEmpty) {
        val current = queue.dequeue()
        neighbours.getOrElse(current, Set.empty).toVector.sorted.foreach { next =>
          if (unseen.remove(next)) {
            reached += next
            queue.enqueue(next)
          }
        }
      }
      result += reached.toSet
    }

    result.result().sortWith { (left, right) =>
      compareVariables(left.toVector.sorted, right.toVector.sorted) < 0
    }
  }

  private def nonEmptySubsets(values: Vector[Int]): Iterator[Vector[Int]] =
    (1 to values.size).iterator.flatMap(values.combinations)

  private def undirectedTrees(
      nodes: Vector[Int]
  ): Iterator[Vector[(Int, Int)]] = {
    if (nodes.size == 1) Iterator.single(Vector.empty)
    else if (nodes.size == 2) Iterator.single(Vector(nodes(0) -> nodes(1)))
    else pruferSequences(nodes, nodes.size - 2).map(decodePrufer(nodes, _))
  }

  private def pruferSequences(
      alphabet: Vector[Int],
      length: Int
  ): Iterator[Vector[Int]] = {
    if (length == 0) Iterator.single(Vector.empty)
    else {
      pruferSequences(alphabet, length - 1).flatMap { prefix =>
        alphabet.iterator.map(prefix :+ _)
      }
    }
  }

  private def decodePrufer(
      nodes: Vector[Int],
      sequence: Vector[Int]
  ): Vector[(Int, Int)] = {
    val degrees = mutable.Map(nodes.map(_ -> 1): _*)
    sequence.foreach(node => degrees.update(node, degrees(node) + 1))
    val result = Vector.newBuilder[(Int, Int)]

    sequence.foreach { node =>
      val leaf = nodes.filter(candidate => degrees(candidate) == 1).min
      result += canonicalPair(leaf, node)
      degrees.update(leaf, 0)
      degrees.update(node, degrees(node) - 1)
    }
    val remaining = nodes.filter(node => degrees(node) == 1)
    result += canonicalPair(remaining(0), remaining(1))
    result.result().sorted
  }

  private def canonicalPair(left: Int, right: Int): (Int, Int) =
    if (left < right) left -> right else right -> left

  private def adjacency(
      nodes: Vector[Int],
      edges: Vector[(Int, Int)]
  ): Map[Int, Vector[Int]] =
    nodes.map { node =>
      node -> edges.iterator.flatMap {
        case (`node`, other) => Some(other)
        case (other, `node`) => Some(other)
        case _               => None
      }.toVector.sorted
    }.toMap

  private def orient(
      current: Int,
      parent: Option[Int],
      graph: Map[Int, Vector[Int]]
  ): RootedTree =
    RootedTree(
      current,
      graph.getOrElse(current, Vector.empty)
        .filterNot(parent.contains)
        .map(child => orient(child, Some(current), graph))
    )

  private def isTreeDecomposition(
      hypergraph: QueryHypergraph,
      tree: RootedTree,
      candidates: Vector[OracleCandidateBag]
  ): Boolean = {
    val nodes = flatten(tree)
    val coversEveryEdge = hypergraph.edges.forall { edge =>
      nodes.exists(node => edge.vertices.forall(candidates(node).vertexSet))
    }
    coversEveryEdge && hypergraph.variables.forall { variable =>
      val containing = nodes.filter(candidates(_).vertexSet.contains(variable)).toSet
      containing.isEmpty || connectedInduced(tree, containing)
    }
  }

  private def isComponentNormalForm(
      hypergraph: QueryHypergraph,
      tree: RootedTree,
      candidates: Vector[OracleCandidateBag]
  ): Boolean = {
    val parentBag = candidates(tree.candidateIndex).vertexSet
    tree.children.forall { child =>
      val childBag = candidates(child.candidateIndex).vertexSet
      val subtreeVertices = flatten(child).iterator
        .flatMap(index => candidates(index).vertices)
        .toSet
      val intersection = parentBag.intersect(childBag)
      val matches = vertexComponents(hypergraph, parentBag)
        .count(component => component ++ intersection == subtreeVertices)
      matches == 1 && isComponentNormalForm(hypergraph, child, candidates)
    }
  }

  private def connectedInduced(
      tree: RootedTree,
      allowed: Set[Int]
  ): Boolean = {
    val graph = treeAdjacency(tree)
    val start = allowed.min
    val reached = mutable.Set.empty[Int]
    val queue = mutable.Queue(start)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (allowed(current) && !reached(current)) {
        reached += current
        graph.getOrElse(current, Set.empty).filter(allowed).foreach(queue.enqueue(_))
      }
    }
    reached.toSet == allowed
  }

  private def treeAdjacency(tree: RootedTree): Map[Int, Set[Int]] = {
    val result = mutable.Map.empty[Int, mutable.Set[Int]]
    def visit(node: RootedTree): Unit = {
      result.getOrElseUpdate(node.candidateIndex, mutable.Set.empty)
      node.children.foreach { child =>
        result(node.candidateIndex) += child.candidateIndex
        result.getOrElseUpdate(child.candidateIndex, mutable.Set.empty) +=
          node.candidateIndex
        visit(child)
      }
    }
    visit(tree)
    result.iterator.map { case (node, neighbours) =>
      node -> neighbours.toSet
    }.toMap
  }

  private def flatten(tree: RootedTree): Vector[Int] =
    tree.candidateIndex +: tree.children.flatMap(flatten)

  private def canonicalize(
      tree: RootedTree,
      candidates: Vector[OracleCandidateBag]
  ): RootedTree = {
    val children = tree.children.map(canonicalize(_, candidates)).sortWith {
      (left, right) =>
        compareTrees(left, right, candidates) < 0
    }
    tree.copy(children = children)
  }

  private def bestTree(
      trees: Vector[RootedTree],
      candidates: Vector[OracleCandidateBag]
  ): Option[RootedTree] =
    trees.sortWith { (left, right) =>
      val size = Integer.compare(flatten(left).size, flatten(right).size)
      if (size != 0) size < 0
      else compareTrees(left, right, candidates) < 0
    }.headOption

  private def compareTrees(
      left: RootedTree,
      right: RootedTree,
      candidates: Vector[OracleCandidateBag]
  ): Int = {
    val bag = compareVariables(
      candidates(left.candidateIndex).vertices,
      candidates(right.candidateIndex).vertices
    )
    if (bag != 0) bag
    else {
      val common = math.min(left.children.size, right.children.size)
      var index = 0
      while (index < common) {
        val child = compareTrees(
          left.children(index),
          right.children(index),
          candidates
        )
        if (child != 0) return child
        index += 1
      }
      Integer.compare(left.children.size, right.children.size)
    }
  }

  private def physicalize(
      tree: RootedTree,
      candidates: Vector[OracleCandidateBag]
  ): CoveredDecomposition = {
    val bags = Vector.newBuilder[CoveredBag]
    val edges = Vector.newBuilder[BagTreeEdge]
    var nextId = 0

    def visit(node: RootedTree, parent: Option[Int]): Int = {
      val id = nextId
      nextId += 1
      val candidate = candidates(node.candidateIndex)
      val cover = candidate.physicalCovers.headOption.getOrElse {
        throw new AssertionError("admissible oracle candidate has no physical cover")
      }
      bags += decomposition(
        CoveredBag.create(bagId(id), candidate.vertices, cover)
      )
      parent.foreach { parentId =>
        edges += decomposition(BagTreeEdge.create(bagId(parentId), bagId(id)))
      }
      node.children.foreach(child => visit(child, Some(id)))
      id
    }

    val root = visit(tree, None)
    decomposition(CoveredDecomposition.create(bagId(root), bags.result(), edges.result()))
  }

  private def compareCandidateBags(
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
  ): Int = compareInts(left.map(_.value), right.map(_.value))

  private def compareInts(left: Vector[Int], right: Vector[Int]): Int = {
    val common = math.min(left.size, right.size)
    var index = 0
    while (index < common) {
      val comparison = Integer.compare(left(index), right(index))
      if (comparison != 0) return comparison
      index += 1
    }
    Integer.compare(left.size, right.size)
  }

  private def core[A](result: Either[ValidationError, A]): A =
    result.fold(error => throw new AssertionError(error.message), identity)

  private def coreMany[A](result: Either[Vector[ValidationError], A]): A =
    result.fold(
      errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
      identity
    )

  private def decomposition[A](
      result: Either[Vector[DecompositionValidationError], A]
  ): A = result.fold(
    errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
    identity
  )
}
