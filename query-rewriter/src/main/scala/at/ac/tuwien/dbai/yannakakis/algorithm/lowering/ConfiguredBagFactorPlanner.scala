package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateCertificate,
  CandidateCertificateError,
  CandidateDecompositionValidator,
  CandidateSearchInput
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, VariableId}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  CoveredBag,
  CoveredDecomposition
}

import scala.collection.mutable

sealed trait ConfiguredBagFactorPlanningError
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object ConfiguredBagFactorPlanningError {
  final case class MissingValue(path: String)
      extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.input.missing"
    override val message: String = s"Missing $path"
  }
  final case class CandidateCertificateViolations(
      errors: Vector[CandidateCertificateError]
  ) extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.candidate-certificate"
    override val message: String =
      s"Structural candidate validation failed (${errors.size} errors)"
  }
  final case class ResourceLimitExceeded(
      resource: BagFactorResource,
      required: BigInt,
      limit: Long
  ) extends ConfiguredBagFactorPlanningError {
    override val code: String =
      s"physical-lowering.resource.exceeded.${resource.code}"
    override val message: String =
      s"${resource.code} requires $required items, exceeding $limit"
  }
  final case class EmptyBag(bagId: BagId)
      extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.bag.empty"
    override val message: String = s"Real bag ${bagId.value} must not be empty"
  }
  final case class InvalidExecutionRoot(
      executionRoot: BagId,
      structuralBagIds: Vector[BagId]
  ) extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.root.invalid"
    override val message: String =
      s"Execution root ${executionRoot.value} is not a structural bag"
  }
  final case class AssignmentBagIdsMismatch(
      expected: Vector[BagId],
      actual: Vector[BagId]
  ) extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.cover.bag-ids"
    override val message: String =
      "Physical cover assignment does not contain every structural bag exactly once"
  }
  final case class MissingAdmissibleCoverSet(bagId: BagId)
      extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.cover.set-missing"
    override val message: String =
      s"Structural bag ${bagId.value} has no candidate admissible-cover set"
  }
  final case class CoverNotAdmissible(
      bagId: BagId,
      edgeIds: Vector[EdgeId]
  ) extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.cover.not-admissible"
    override val message: String =
      s"Physical cover for bag ${bagId.value} is outside its exact admissible set"
  }
  final case class MissingEligibleOwner(edgeId: EdgeId)
      extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.ownership.missing"
    override val message: String =
      s"No structural bag contains every vertex of edge ${edgeId.value}"
  }
  final case class AmbiguousRootNearestOwner(
      edgeId: EdgeId,
      depth: Int,
      candidateBagIds: Vector[BagId]
  ) extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.ownership.ambiguous"
    override val message: String =
      s"Edge ${edgeId.value} has ${candidateBagIds.size} closest bags at depth $depth"
  }
  final case class ModelInvariantViolation(detail: String)
      extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.model.invariant"
    override val message: String = detail
  }
  final case class IndependentValidationFailure(
      errors: Vector[ConfiguredBagFactorPlanValidationError]
  ) extends ConfiguredBagFactorPlanningError {
    override val code: String = "physical-lowering.independent-validation"
    override val message: String =
      s"Independent configured-plan validation failed (${errors.size} errors)"
  }

  def canonical(
      errors: Iterable[ConfiguredBagFactorPlanningError]
  ): Vector[ConfiguredBagFactorPlanningError] =
    errors.iterator.toVector.distinct.sortBy(error => (error.code, error.message))
}

/** Builds a physical orientation without changing the structural candidate. */
object ConfiguredBagFactorPlanner {
  def plan(
      input: CandidateSearchInput,
      structuralDecomposition: CoveredDecomposition,
      configuration: BagFactorPhysicalConfiguration,
      limits: BagFactorPlanningLimits = BagFactorPlanningLimits.default
  ): Either[Vector[ConfiguredBagFactorPlanningError], ConfiguredBagFactorPlan] = {
    import ConfiguredBagFactorPlanningError._

    val missing = Vector.newBuilder[ConfiguredBagFactorPlanningError]
    if (input == null) missing += MissingValue("candidate-search input")
    if (structuralDecomposition == null)
      missing += MissingValue("structural covered decomposition")
    if (configuration == null)
      missing += MissingValue("bag-factor physical configuration")
    if (limits == null) missing += MissingValue("bag-factor planning limits")
    val missingErrors = canonical(missing.result())
    if (missingErrors.nonEmpty) return Left(missingErrors)

    val resourceProblems = resourceErrors(input, structuralDecomposition, limits)
    if (resourceProblems.nonEmpty) return Left(resourceProblems)

    val certificate = CandidateDecompositionValidator
      .validate(input, structuralDecomposition) match {
      case Left(problems) =>
        return Left(Vector(CandidateCertificateViolations(problems)))
      case Right(value) => value
    }

    val prerequisites = Vector.newBuilder[ConfiguredBagFactorPlanningError]
    structuralDecomposition.bags
      .filter(_.vertices.isEmpty)
      .sortBy(_.id)
      .foreach(bag => prerequisites += EmptyBag(bag.id))
    prerequisites ++= configurationErrors(
      input,
      structuralDecomposition,
      configuration
    )
    val prerequisiteErrors = canonical(prerequisites.result())
    if (prerequisiteErrors.nonEmpty) return Left(prerequisiteErrors)

    build(
      input,
      structuralDecomposition,
      certificate,
      configuration
    ).flatMap { configured =>
      ConfiguredBagFactorPlanValidator
        .validate(input, structuralDecomposition, configured, limits)
        .left
        .map(problems => Vector(IndependentValidationFailure(problems)))
    }
  }

  private def resourceErrors(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      limits: BagFactorPlanningLimits
  ): Vector[ConfiguredBagFactorPlanningError] = {
    import ConfiguredBagFactorPlanningError.ResourceLimitExceeded
    val bagCount = BigInt(decomposition.bags.size)
    val treeEdgeCount = BigInt(decomposition.treeEdges.size)
    val sourceEdgeCount = BigInt(input.hypergraph.edges.size)
    val counts = Vector(
      (BagFactorResource.Bags, bagCount, limits.maxBags),
      (BagFactorResource.TreeEdges, treeEdgeCount, limits.maxTreeEdges),
      (BagFactorResource.SourceEdges, sourceEdgeCount, limits.maxSourceEdges)
    ).collect {
      case (resource, required, limit) if required > BigInt(limit) =>
        ResourceLimitExceeded(resource, required, limit)
    }
    if (counts.nonEmpty) counts
    else {
      val occurrences = input.hypergraph.edges.foldLeft(BigInt(0))(
        (total, edge) => total + edge.vertices.size
      )
      val checks = bagCount.max(BigInt(1)) * occurrences
      if (checks > BigInt(limits.maxOwnershipChecks))
        Vector(
          ResourceLimitExceeded(
            BagFactorResource.OwnershipChecks,
            checks,
            limits.maxOwnershipChecks
          )
        )
      else Vector.empty
    }
  }

  private[lowering] def configurationErrors(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      configuration: BagFactorPhysicalConfiguration
  ): Vector[ConfiguredBagFactorPlanningError] = {
    import ConfiguredBagFactorPlanningError._
    val errors = Vector.newBuilder[ConfiguredBagFactorPlanningError]
    val structuralIds = decomposition.bagIds.sorted
    if (!structuralIds.contains(configuration.executionRoot)) {
      errors += InvalidExecutionRoot(configuration.executionRoot, structuralIds)
    }
    val assignedIds = configuration.coverAssignments.map(_.bagId)
    if (assignedIds != structuralIds) {
      errors += AssignmentBagIdsMismatch(structuralIds, assignedIds)
    }
    val assignmentByBag = configuration.coverAssignments.iterator
      .map(assignment => assignment.bagId -> assignment)
      .toMap
    decomposition.bags.sortBy(_.id).foreach { bag =>
      assignmentByBag.get(bag.id).foreach { assignment =>
        input.candidateBag(bag.vertices) match {
          case None => errors += MissingAdmissibleCoverSet(bag.id)
          case Some(candidate)
              if !candidate.physicalCovers.exists(
                _.edgeIds == assignment.edgeIds
              ) =>
            errors += CoverNotAdmissible(bag.id, assignment.edgeIds)
          case Some(_) => ()
        }
      }
    }
    ConfiguredBagFactorPlanningError.canonical(errors.result())
  }

  private def build(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      certificate: CandidateCertificate,
      configuration: BagFactorPhysicalConfiguration
  ): Either[Vector[ConfiguredBagFactorPlanningError], ConfiguredBagFactorPlan] =
    orient(decomposition, configuration.executionRoot).flatMap { orientation =>
      assignOwnership(input, decomposition, orientation).map { ownership =>
        val bagById = decomposition.bags.iterator.map(bag => bag.id -> bag).toMap
        val coverByBag = configuration.coverAssignments.iterator
          .map(assignment => assignment.bagId -> assignment.edgeIds)
          .toMap
        val ownedByBag = ownership.groupBy(_.owner).iterator.map {
          case (bagId, values) => bagId -> values.map(_.edgeId).sorted
        }.toMap
        val factors = decomposition.bags.sortBy(_.id).map { bag =>
          BagFactor(
            bag.id,
            bag.vertices,
            coverByBag(bag.id),
            ownedByBag.getOrElse(bag.id, Vector.empty),
            orientation.depthByBag(bag.id),
            orientation.parentByBag(bag.id),
            orientation.childrenByBag.getOrElse(bag.id, Vector.empty)
          )
        }
        val arcs = orientation.preorder.tail.map { child =>
          val parent = orientation.parentByBag(child).get
          OrientedBagArc(
            parent,
            child,
            separator(bagById(parent), bagById(child))
          )
        }
        val separatorByChild = arcs.iterator.map(arc => arc.child -> arc.separator).toMap
        val bottomUp = orientation.postorder
          .filterNot(_ == configuration.executionRoot)
          .zipWithIndex
          .map { case (child, ordinal) =>
            val parent = orientation.parentByBag(child).get
            SemijoinStep(
              ordinal,
              SemijoinPass.BottomUp,
              parent,
              child,
              separatorByChild(child)
            )
          }
        val topDown = orientation.preorder.tail.zipWithIndex.map {
          case (child, ordinal) =>
            val parent = orientation.parentByBag(child).get
            SemijoinStep(
              ordinal,
              SemijoinPass.TopDown,
              child,
              parent,
              separatorByChild(child)
            )
        }
        val plan = BagFactorPlan(
          input.hypergraph,
          input.widthBound,
          input.requireConnectedCover,
          decomposition,
          certificate,
          configuration.executionRoot,
          factors,
          ownership,
          arcs,
          YannakakisSchedule(bottomUp, topDown)
        )
        ConfiguredBagFactorPlan(
          decomposition,
          certificate,
          configuration,
          plan
        )
      }
    }

  private def orient(
      decomposition: CoveredDecomposition,
      root: BagId
  ): Either[Vector[ConfiguredBagFactorPlanningError], Orientation] = {
    val parent = mutable.Map.empty[BagId, Option[BagId]]
    val children = mutable.Map.empty[BagId, Vector[BagId]]
    val depth = mutable.Map.empty[BagId, Int]
    val preorder = Vector.newBuilder[BagId]
    val visited = mutable.HashSet.empty[BagId]
    val stack = mutable.Stack[(BagId, Option[BagId], Int)]((root, None, 0))
    while (stack.nonEmpty) {
      val (current, ancestor, currentDepth) = stack.pop()
      if (visited.add(current)) {
        parent.put(current, ancestor)
        depth.put(current, currentDepth)
        preorder += current
        val next = decomposition.neighbors(current)
          .filterNot(ancestor.contains)
          .filterNot(visited)
          .sorted
        children.put(current, next)
        next.reverseIterator.foreach(child =>
          stack.push((child, Some(current), currentDepth + 1))
        )
      }
    }
    val order = preorder.result()
    if (order.size != decomposition.bags.size)
      Left(
        Vector(
          ConfiguredBagFactorPlanningError.ModelInvariantViolation(
            "Configured orientation did not visit every structural bag"
          )
        )
      )
    else {
      val postorder = Vector.newBuilder[BagId]
      val pending = mutable.Stack[(BagId, Boolean)]((root, false))
      while (pending.nonEmpty) {
        val (current, expanded) = pending.pop()
        if (expanded) postorder += current
        else {
          pending.push((current, true))
          children.getOrElse(current, Vector.empty).reverseIterator.foreach(
            child => pending.push((child, false))
          )
        }
      }
      Right(
        Orientation(
          order,
          postorder.result(),
          parent.toMap,
          children.toMap,
          depth.toMap
        )
      )
    }
  }

  private def assignOwnership(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      orientation: Orientation
  ): Either[Vector[ConfiguredBagFactorPlanningError], Vector[EdgeOwnership]] = {
    import ConfiguredBagFactorPlanningError._
    val errors = Vector.newBuilder[ConfiguredBagFactorPlanningError]
    val ownership = Vector.newBuilder[EdgeOwnership]
    val bags = decomposition.bags.sortBy(_.id)
    input.hypergraph.edges.foreach { edge =>
      val eligible = bags.filter(_.containsAll(edge.vertices))
      if (eligible.isEmpty) errors += MissingEligibleOwner(edge.id)
      else {
        val minimumDepth = eligible.map(bag => orientation.depthByBag(bag.id)).min
        val nearest = eligible
          .filter(bag => orientation.depthByBag(bag.id) == minimumDepth)
          .map(_.id)
          .sorted
        if (nearest.size != 1)
          errors += AmbiguousRootNearestOwner(edge.id, minimumDepth, nearest)
        else ownership += EdgeOwnership(edge.id, nearest.head, minimumDepth)
      }
    }
    val problems = ConfiguredBagFactorPlanningError.canonical(errors.result())
    if (problems.nonEmpty) Left(problems) else Right(ownership.result())
  }

  private def separator(left: CoveredBag, right: CoveredBag): Vector[VariableId] =
    (left.vertices.toSet intersect right.vertices.toSet).toVector.sorted

  private final case class Orientation(
      preorder: Vector[BagId],
      postorder: Vector[BagId],
      parentByBag: Map[BagId, Option[BagId]],
      childrenByBag: Map[BagId, Vector[BagId]],
      depthByBag: Map[BagId, Int]
  )
}
