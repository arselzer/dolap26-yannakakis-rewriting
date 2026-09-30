package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateCertificate,
  CandidateDecompositionValidator,
  CandidateSearchInput
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import at.ac.tuwien.dbai.yannakakis.core._
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  BagTreeEdge,
  CoveredBag,
  CoveredDecomposition,
  DecompositionValidationError
}

import scala.collection.mutable

/** Independent, small-fixture oracle for ownership and tree scheduling. */
private[lowering] object BagFactorPlanningOracleTestSupport {
  final case class OracleFactor(
      bagId: BagId,
      vertices: Vector[VariableId],
      supportCover: Vector[EdgeId],
      ownedEdgeIds: Vector[EdgeId],
      depth: Int,
      parent: Option[BagId],
      children: Vector[BagId]
  )

  final case class OracleOwnership(
      edgeId: EdgeId,
      owner: BagId,
      ownerDepth: Int
  )

  final case class OracleArc(
      parent: BagId,
      child: BagId,
      separator: Vector[VariableId]
  )

  final case class OracleStep(
      ordinal: Int,
      passCode: String,
      target: BagId,
      source: BagId,
      separator: Vector[VariableId]
  )

  final case class OracleSchedule(
      bottomUp: Vector[OracleStep],
      topDown: Vector[OracleStep]
  )

  final case class OraclePlan(
      root: BagId,
      factors: Vector[OracleFactor],
      ownership: Vector[OracleOwnership],
      arcs: Vector[OracleArc],
      schedule: OracleSchedule
  )

  final case class PlanningFixture(
      hypergraph: QueryHypergraph,
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition
  )

  private final case class Orientation(
      preorder: Vector[BagId],
      postorder: Vector[BagId],
      parent: Map[BagId, Option[BagId]],
      children: Map[BagId, Vector[BagId]],
      depth: Map[BagId, Int]
  )

  def variable(value: Int): VariableId =
    validated(VariableId.from(value))

  def edgeId(value: Int): EdgeId =
    validated(EdgeId.from(value))

  def occurrence(value: Int): RelationOccurrenceId =
    validated(RelationOccurrenceId.from(value))

  def bagId(value: Int): BagId =
    validated(BagId.from(value))

  def graph(vertexSets: Seq[Seq[Int]]): QueryHypergraph =
    graphWithIds(vertexSets.zipWithIndex.map { case (vertices, index) =>
      index -> vertices
    })

  def graphWithIds(edges: Seq[(Int, Seq[Int])]): QueryHypergraph = {
    val hyperedges = edges.map { case (numericId, vertices) =>
      val relation = occurrence(numericId)
      val bindings = vertices.zipWithIndex.map { case (numericVertex, ordinal) =>
        validated(ColumnId.from(relation, ordinal)) -> variable(numericVertex)
      }
      validatedMany(Hyperedge.create(edgeId(numericId), relation, bindings))
    }
    validatedMany(QueryHypergraph.create(hyperedges))
  }

  def candidateInput(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      requireConnectedCover: Boolean
  ): CandidateSearchInput = {
    val raw = SoftBagGenerator
      .generate(hypergraph, widthBound)
      .fold(failure => throw new AssertionError(failure.message), identity)
    CandidateSearchInput
      .create(
        hypergraph,
        widthBound,
        raw.admissible(requireConnectedCover)
      )
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )
  }

  def coveredBag(
      id: Int,
      vertices: Iterable[Int],
      cover: Iterable[Int]
  ): CoveredBag =
    decompositionValue(
      CoveredBag.create(
        bagId(id),
        vertices.map(variable),
        cover.map(edgeId)
      )
    )

  def treeEdge(first: Int, second: Int): BagTreeEdge =
    decompositionValue(BagTreeEdge.create(bagId(first), bagId(second)))

  def coveredDecomposition(
      root: Int,
      bags: Iterable[CoveredBag],
      edges: Iterable[BagTreeEdge]
  ): CoveredDecomposition =
    decompositionValue(
      CoveredDecomposition.create(bagId(root), bags, edges)
    )

  def certificate(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition
  ): CandidateCertificate =
    CandidateDecompositionValidator
      .validate(input, decomposition)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )

  def planned(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition
  ): BagFactorPlan =
    BagFactorPlanner
      .plan(input, decomposition)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )

  /**
    * Derives the contract without using production orientation, ownership,
    * separator, or schedule helpers. Support-cover membership is deliberately
    * absent from the ownership comparison.
    */
  def derive(
      hypergraph: QueryHypergraph,
      decomposition: CoveredDecomposition
  ): OraclePlan = {
    val orientation = orient(decomposition)
    val bagById = decomposition.bags.iterator.map(bag => bag.id -> bag).toMap
    val ownership = hypergraph.edges.map { edge =>
      val eligible = decomposition.bags.filter(_.containsAll(edge.vertices))
      if (eligible.isEmpty) {
        throw new AssertionError(s"oracle found no owner for edge ${edge.id.value}")
      }
      val minimumDepth = eligible.iterator.map(bag => orientation.depth(bag.id)).min
      val shallowest = eligible
        .filter(bag => orientation.depth(bag.id) == minimumDepth)
        .sortBy(_.id)
      if (shallowest.size != 1) {
        throw new AssertionError(
          s"oracle found ambiguous owners for edge ${edge.id.value}: " +
            shallowest.map(_.id.value).mkString(",")
        )
      }
      OracleOwnership(edge.id, shallowest.head.id, minimumDepth)
    }
    val ownedByBag = ownership
      .groupBy(_.owner)
      .view
      .mapValues(_.map(_.edgeId).sorted)
      .toMap

    val factors = decomposition.bags.sortBy(_.id).map { bag =>
      OracleFactor(
        bag.id,
        bag.vertices,
        bag.cover,
        ownedByBag.getOrElse(bag.id, Vector.empty),
        orientation.depth(bag.id),
        orientation.parent(bag.id),
        orientation.children.getOrElse(bag.id, Vector.empty)
      )
    }
    val arcs = orientation.preorder.tail.map { child =>
      val parent = orientation.parent(child).getOrElse {
        throw new AssertionError(s"non-root bag ${child.value} has no parent")
      }
      OracleArc(
        parent,
        child,
        intersection(bagById(parent).vertices, bagById(child).vertices)
      )
    }
    val separatorByChild = arcs.iterator.map(arc => arc.child -> arc.separator).toMap
    val bottomUp = orientation.postorder
      .filterNot(_ == decomposition.root)
      .zipWithIndex
      .map { case (child, ordinal) =>
        val parent = orientation.parent(child).get
        OracleStep(
          ordinal,
          SemijoinPass.BottomUp.code,
          target = parent,
          source = child,
          separatorByChild(child)
        )
      }
    val topDown = orientation.preorder.tail.zipWithIndex.map {
      case (child, ordinal) =>
        val parent = orientation.parent(child).get
        OracleStep(
          ordinal,
          SemijoinPass.TopDown.code,
          target = child,
          source = parent,
          separatorByChild(child)
        )
    }

    OraclePlan(
      decomposition.root,
      factors,
      ownership,
      arcs,
      OracleSchedule(bottomUp, topDown)
    )
  }

  def normalize(plan: BagFactorPlan): OraclePlan =
    OraclePlan(
      plan.root,
      plan.factors.map(factor =>
        OracleFactor(
          factor.bagId,
          factor.vertices,
          factor.supportCover,
          factor.ownedEdgeIds,
          factor.depth,
          factor.parent,
          factor.children
        )
      ),
      plan.ownership.map(owner =>
        OracleOwnership(owner.edgeId, owner.owner, owner.ownerDepth)
      ),
      plan.arcs.map(arc => OracleArc(arc.parent, arc.child, arc.separator)),
      OracleSchedule(
        plan.schedule.bottomUp.map(normalizeStep),
        plan.schedule.topDown.map(normalizeStep)
      )
    )

  private def normalizeStep(step: SemijoinStep): OracleStep =
    OracleStep(
      step.ordinal,
      step.pass.code,
      step.target,
      step.source,
      step.separator
    )

  private def orient(decomposition: CoveredDecomposition): Orientation = {
    val parent = mutable.Map.empty[BagId, Option[BagId]]
    val children = mutable.Map.empty[BagId, Vector[BagId]]
    val depth = mutable.Map.empty[BagId, Int]
    val preorder = Vector.newBuilder[BagId]
    val postorder = Vector.newBuilder[BagId]
    val visited = mutable.Set.empty[BagId]

    def visit(current: BagId, ancestor: Option[BagId], currentDepth: Int): Unit = {
      if (visited(current)) {
        throw new AssertionError(s"oracle orientation revisited bag ${current.value}")
      }
      visited += current
      parent.put(current, ancestor)
      depth.put(current, currentDepth)
      preorder += current
      val next = decomposition.neighbors(current)
        .filterNot(ancestor.contains)
        .sorted
      children.put(current, next)
      next.foreach(child => visit(child, Some(current), currentDepth + 1))
      postorder += current
    }

    visit(decomposition.root, None, 0)
    if (visited.size != decomposition.bags.size) {
      throw new AssertionError("oracle orientation did not visit every bag")
    }
    Orientation(
      preorder.result(),
      postorder.result(),
      parent.toMap,
      children.toMap,
      depth.toMap
    )
  }

  private def intersection(
      left: Vector[VariableId],
      right: Vector[VariableId]
  ): Vector[VariableId] =
    (left.toSet intersect right.toSet).toVector.sorted

  private def validated[A](result: Either[ValidationError, A]): A =
    result.fold(error => throw new AssertionError(error.message), identity)

  private def validatedMany[A](
      result: Either[Vector[ValidationError], A]
  ): A = result.fold(
    errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
    identity
  )

  private def decompositionValue[A](
      result: Either[Vector[DecompositionValidationError], A]
  ): A = result.fold(
    errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
    identity
  )
}
