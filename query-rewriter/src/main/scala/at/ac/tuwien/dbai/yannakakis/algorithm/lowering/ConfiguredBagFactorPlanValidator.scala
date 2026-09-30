package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateCertificate,
  CandidateCertificateError,
  CandidateDecompositionValidator,
  CandidateSearchInput
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, QueryHypergraph, VariableId}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  CoveredBag,
  CoveredDecomposition
}

import scala.collection.mutable

sealed trait ConfiguredBagFactorPlanValidationError
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object ConfiguredBagFactorPlanValidationError {
  final case class MissingValue(path: String)
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.missing"
    override val message: String = s"Missing $path"
  }
  final case class CandidateCertificateViolations(
      errors: Vector[CandidateCertificateError]
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.candidate-certificate"
    override val message: String =
      s"Structural candidate validation failed (${errors.size} errors)"
  }
  final case class ResourceLimitExceeded(
      resource: BagFactorResource,
      required: BigInt,
      limit: Long
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      s"physical-lowering.validation.resource.exceeded.${resource.code}"
    override val message: String =
      s"${resource.code} requires $required items, exceeding $limit"
  }
  final case class EmptyBag(bagId: BagId)
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.bag.empty"
    override val message: String = s"Real bag ${bagId.value} must not be empty"
  }
  final case class InvalidExecutionRoot(root: BagId)
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.root.invalid"
    override val message: String =
      s"Execution root ${root.value} is not a structural bag"
  }
  final case class AssignmentBagIdsMismatch(
      expected: Vector[BagId],
      actual: Vector[BagId]
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.cover.bag-ids"
    override val message: String =
      "Physical cover assignment does not bind every structural bag once"
  }
  final case class MissingAdmissibleCoverSet(bagId: BagId)
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.cover.set-missing"
    override val message: String =
      s"Structural bag ${bagId.value} has no admissible cover set"
  }
  final case class CoverNotAdmissible(
      bagId: BagId,
      edgeIds: Vector[EdgeId]
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.cover.not-admissible"
    override val message: String =
      s"Configured cover for bag ${bagId.value} is not exactly admissible"
  }
  final case class MissingEligibleOwner(edgeId: EdgeId)
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.ownership.missing"
    override val message: String =
      s"No structural bag contains edge ${edgeId.value}"
  }
  final case class AmbiguousRootNearestOwner(
      edgeId: EdgeId,
      depth: Int,
      bags: Vector[BagId]
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.ownership.ambiguous"
    override val message: String =
      s"Edge ${edgeId.value} has ${bags.size} closest bags at depth $depth"
  }
  final case class OrientationInvariantViolation(detail: String)
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.orientation.invariant"
    override val message: String = detail
  }
  final case class SourceHypergraphMismatch(
      expected: QueryHypergraph,
      actual: QueryHypergraph
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.provenance.hypergraph"
    override val message: String =
      "Configured plan is bound to another source hypergraph"
  }
  final case class SourceWidthMismatch(expected: Int, actual: Int)
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.provenance.width"
    override val message: String =
      s"Configured plan width is $actual, expected $expected"
  }
  final case class AdmissibilityModeMismatch(expected: Boolean, actual: Boolean)
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.provenance.admissibility"
    override val message: String =
      s"Configured plan connected-cover mode is $actual, expected $expected"
  }
  final case class StructuralDecompositionMismatch()
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.structural-decomposition"
    override val message: String =
      "Configured plan does not retain the certified structural decomposition"
  }
  final case class StructuralCertificateMismatch(
      expected: CandidateCertificate,
      actual: CandidateCertificate
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String =
      "physical-lowering.validation.structural-certificate"
    override val message: String =
      "Configured plan does not retain the structural certificate"
  }
  final case class ExecutionRootMismatch(expected: BagId, actual: BagId)
      extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.execution-root"
    override val message: String =
      s"Configured plan root ${actual.value} differs from ${expected.value}"
  }
  final case class FactorsMismatch(
      expected: Vector[BagFactor],
      actual: Vector[BagFactor]
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.factors"
    override val message: String = "Configured factors are not canonical"
  }
  final case class OwnershipMismatch(
      expected: Vector[EdgeOwnership],
      actual: Vector[EdgeOwnership]
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.ownership"
    override val message: String = "Configured ownership is not canonical"
  }
  final case class ArcsMismatch(
      expected: Vector[OrientedBagArc],
      actual: Vector[OrientedBagArc]
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.arcs"
    override val message: String = "Configured oriented arcs are not canonical"
  }
  final case class ScheduleMismatch(
      expected: YannakakisSchedule,
      actual: YannakakisSchedule
  ) extends ConfiguredBagFactorPlanValidationError {
    override val code: String = "physical-lowering.validation.schedule"
    override val message: String = "Configured reduction schedule is not canonical"
  }

  def canonical(
      errors: Iterable[ConfiguredBagFactorPlanValidationError]
  ): Vector[ConfiguredBagFactorPlanValidationError] =
    errors.iterator.toVector.distinct.sortBy(error => (error.code, error.message))
}

/**
  * Independent validation boundary for alternate roots and cover assignments.
  * It never invokes the configured or legacy planner.
  */
object ConfiguredBagFactorPlanValidator {
  def validate(
      input: CandidateSearchInput,
      structuralDecomposition: CoveredDecomposition,
      configured: ConfiguredBagFactorPlan,
      limits: BagFactorPlanningLimits = BagFactorPlanningLimits.default
  ): Either[
    Vector[ConfiguredBagFactorPlanValidationError],
    ConfiguredBagFactorPlan
  ] = {
    import ConfiguredBagFactorPlanValidationError._

    val missing = Vector.newBuilder[ConfiguredBagFactorPlanValidationError]
    if (input == null) missing += MissingValue("candidate-search input")
    if (structuralDecomposition == null)
      missing += MissingValue("structural covered decomposition")
    if (configured == null) missing += MissingValue("configured bag-factor plan")
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

    val prerequisites = Vector.newBuilder[ConfiguredBagFactorPlanValidationError]
    structuralDecomposition.bags.filter(_.vertices.isEmpty).sortBy(_.id)
      .foreach(bag => prerequisites += EmptyBag(bag.id))
    prerequisites ++= configurationErrors(
      input,
      structuralDecomposition,
      configured.configuration
    )
    val prerequisiteErrors = canonical(prerequisites.result())
    if (prerequisiteErrors.nonEmpty) return Left(prerequisiteErrors)

    derive(
      input,
      structuralDecomposition,
      configured.configuration
    ).flatMap { expected =>
      val actual = configured.plan
      val errors = Vector.newBuilder[ConfiguredBagFactorPlanValidationError]
      if (actual.sourceHypergraph != input.hypergraph)
        errors += SourceHypergraphMismatch(input.hypergraph, actual.sourceHypergraph)
      if (actual.sourceWidthBound != input.widthBound)
        errors += SourceWidthMismatch(input.widthBound, actual.sourceWidthBound)
      if (actual.requireConnectedCover != input.requireConnectedCover)
        errors += AdmissibilityModeMismatch(
          input.requireConnectedCover,
          actual.requireConnectedCover
        )
      if (
        configured.structuralDecomposition != structuralDecomposition ||
        actual.decomposition != structuralDecomposition
      ) errors += StructuralDecompositionMismatch()
      if (
        configured.structuralCertificate != certificate ||
        actual.certificate != certificate
      ) errors += StructuralCertificateMismatch(certificate, actual.certificate)
      if (actual.root != expected.root)
        errors += ExecutionRootMismatch(expected.root, actual.root)
      if (actual.factors != expected.factors)
        errors += FactorsMismatch(expected.factors, actual.factors)
      if (actual.ownership != expected.ownership)
        errors += OwnershipMismatch(expected.ownership, actual.ownership)
      if (actual.arcs != expected.arcs)
        errors += ArcsMismatch(expected.arcs, actual.arcs)
      if (actual.schedule != expected.schedule)
        errors += ScheduleMismatch(expected.schedule, actual.schedule)
      val problems = canonical(errors.result())
      if (problems.nonEmpty) Left(problems) else Right(configured)
    }
  }

  private def resourceErrors(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      limits: BagFactorPlanningLimits
  ): Vector[ConfiguredBagFactorPlanValidationError] = {
    import ConfiguredBagFactorPlanValidationError.ResourceLimitExceeded
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

  private def configurationErrors(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      configuration: BagFactorPhysicalConfiguration
  ): Vector[ConfiguredBagFactorPlanValidationError] = {
    import ConfiguredBagFactorPlanValidationError._
    val errors = Vector.newBuilder[ConfiguredBagFactorPlanValidationError]
    val ids = decomposition.bagIds.sorted
    if (!ids.contains(configuration.executionRoot))
      errors += InvalidExecutionRoot(configuration.executionRoot)
    val assigned = configuration.coverAssignments.map(_.bagId)
    if (assigned != ids) errors += AssignmentBagIdsMismatch(ids, assigned)
    val byId = configuration.coverAssignments.iterator
      .map(value => value.bagId -> value.edgeIds)
      .toMap
    decomposition.bags.sortBy(_.id).foreach { bag =>
      byId.get(bag.id).foreach { selected =>
        input.candidateBag(bag.vertices) match {
          case None => errors += MissingAdmissibleCoverSet(bag.id)
          case Some(candidate)
              if !candidate.physicalCovers.exists(_.edgeIds == selected) =>
            errors += CoverNotAdmissible(bag.id, selected)
          case Some(_) => ()
        }
      }
    }
    canonical(errors.result())
  }

  private def derive(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      configuration: BagFactorPhysicalConfiguration
  ): Either[Vector[ConfiguredBagFactorPlanValidationError], Derived] =
    deriveOrientation(decomposition, configuration.executionRoot).flatMap {
      orientation =>
        deriveOwnership(input, decomposition, orientation).map { ownership =>
          val bagById = decomposition.bags.iterator.map(bag => bag.id -> bag).toMap
          val coverByBag = configuration.coverAssignments.iterator
            .map(value => value.bagId -> value.edgeIds)
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
              orientation.childrenByBag(bag.id)
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
          val separatorByChild = arcs.iterator
            .map(arc => arc.child -> arc.separator)
            .toMap
          val bottomUp = orientation.postorder
            .filterNot(_ == configuration.executionRoot)
            .zipWithIndex
            .map { case (child, ordinal) =>
              SemijoinStep(
                ordinal,
                SemijoinPass.BottomUp,
                orientation.parentByBag(child).get,
                child,
                separatorByChild(child)
              )
            }
          val topDown = orientation.preorder.tail.zipWithIndex.map {
            case (child, ordinal) =>
              SemijoinStep(
                ordinal,
                SemijoinPass.TopDown,
                child,
                orientation.parentByBag(child).get,
                separatorByChild(child)
              )
          }
          Derived(
            configuration.executionRoot,
            factors,
            ownership,
            arcs,
            YannakakisSchedule(bottomUp, topDown)
          )
        }
    }

  /** BFS parent/depth derivation followed by independent sorted DFS passes. */
  private def deriveOrientation(
      decomposition: CoveredDecomposition,
      root: BagId
  ): Either[Vector[ConfiguredBagFactorPlanValidationError], Orientation] = {
    val adjacency = decomposition.bagIds.iterator
      .map(id => id -> mutable.ArrayBuffer.empty[BagId])
      .toMap
    decomposition.treeEdges.foreach { edge =>
      adjacency(edge.first) += edge.second
      adjacency(edge.second) += edge.first
    }
    val parent = mutable.Map[BagId, Option[BagId]](root -> None)
    val depth = mutable.Map[BagId, Int](root -> 0)
    val queue = mutable.Queue(root)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      adjacency(current).toVector.distinct.sorted.foreach { next =>
        if (!parent.contains(next)) {
          parent.put(next, Some(current))
          depth.put(next, depth(current) + 1)
          queue.enqueue(next)
        }
      }
    }
    if (parent.size != decomposition.bags.size)
      return Left(
        Vector(
          ConfiguredBagFactorPlanValidationError.OrientationInvariantViolation(
            "Independent physical orientation is not a tree over every bag"
          )
        )
      )
    val children = decomposition.bagIds.iterator.map { id =>
      id -> parent.iterator.collect {
        case (child, Some(`id`)) => child
      }.toVector.sorted
    }.toMap
    val preorder = Vector.newBuilder[BagId]
    val postorder = Vector.newBuilder[BagId]
    val stack = mutable.Stack[(BagId, Boolean)]((root, false))
    while (stack.nonEmpty) {
      val (current, expanded) = stack.pop()
      if (expanded) postorder += current
      else {
        preorder += current
        stack.push((current, true))
        children(current).reverseIterator.foreach(child =>
          stack.push((child, false))
        )
      }
    }
    Right(
      Orientation(
        preorder.result(),
        postorder.result(),
        parent.toMap,
        children,
        depth.toMap
      )
    )
  }

  private def deriveOwnership(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      orientation: Orientation
  ): Either[
    Vector[ConfiguredBagFactorPlanValidationError],
    Vector[EdgeOwnership]
  ] = {
    import ConfiguredBagFactorPlanValidationError._
    val errors = Vector.newBuilder[ConfiguredBagFactorPlanValidationError]
    val values = Vector.newBuilder[EdgeOwnership]
    val bags = decomposition.bags.sortBy(_.id)
    input.hypergraph.edges.foreach { edge =>
      var minimum = Int.MaxValue
      val nearest = Vector.newBuilder[BagId]
      bags.foreach { bag =>
        if (bag.containsAll(edge.vertices)) {
          val bagDepth = orientation.depthByBag(bag.id)
          if (bagDepth < minimum) {
            minimum = bagDepth
            nearest.clear()
            nearest += bag.id
          } else if (bagDepth == minimum) nearest += bag.id
        }
      }
      val candidates = nearest.result().sorted
      if (candidates.isEmpty) errors += MissingEligibleOwner(edge.id)
      else if (candidates.size != 1)
        errors += AmbiguousRootNearestOwner(edge.id, minimum, candidates)
      else values += EdgeOwnership(edge.id, candidates.head, minimum)
    }
    val problems = canonical(errors.result())
    if (problems.nonEmpty) Left(problems) else Right(values.result())
  }

  private def separator(left: CoveredBag, right: CoveredBag): Vector[VariableId] = {
    val rightVertices = right.vertices.toSet
    left.vertices.filter(rightVertices)
  }

  private final case class Orientation(
      preorder: Vector[BagId],
      postorder: Vector[BagId],
      parentByBag: Map[BagId, Option[BagId]],
      childrenByBag: Map[BagId, Vector[BagId]],
      depthByBag: Map[BagId, Int]
  )
  private final case class Derived(
      root: BagId,
      factors: Vector[BagFactor],
      ownership: Vector[EdgeOwnership],
      arcs: Vector[OrientedBagArc],
      schedule: YannakakisSchedule
  )
}
