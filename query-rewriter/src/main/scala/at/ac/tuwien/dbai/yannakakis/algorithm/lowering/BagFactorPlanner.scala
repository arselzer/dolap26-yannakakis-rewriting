package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateCertificate,
  CandidateDecompositionValidator,
  CandidateSearchInput
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  CoveredBag,
  CoveredDecomposition
}

import scala.collection.mutable

/** Builds ownership-aware factors and a two-pass Yannakakis schedule. */
object BagFactorPlanner {
  def plan(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition
  ): Either[Vector[BagFactorPlanningError], BagFactorPlan] =
    plan(input, decomposition, BagFactorPlanningLimits.default)

  def plan(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      limits: BagFactorPlanningLimits
  ): Either[Vector[BagFactorPlanningError], BagFactorPlan] = {
    val errors = Vector.newBuilder[BagFactorPlanningError]

    if (input eq null) {
      errors += BagFactorPlanningError.MissingValue("candidate-search input")
    }
    if (decomposition eq null) {
      errors += BagFactorPlanningError.MissingValue("covered decomposition")
    }
    if (limits eq null) {
      errors += BagFactorPlanningError.MissingValue(
        "bag-factor planning limits"
      )
    }

    var resourceFailure = false
    if ((input ne null) && (decomposition ne null) && (limits ne null)) {
      val bagCount = BigInt(decomposition.bags.size)
      val treeEdgeCount = BigInt(decomposition.treeEdges.size)
      val sourceEdgeCount = BigInt(input.hypergraph.edges.size)

      if (bagCount > BigInt(limits.maxBags)) {
        resourceFailure = true
        errors += BagFactorPlanningError.ResourceLimitExceeded(
          BagFactorResource.Bags,
          bagCount,
          limits.maxBags
        )
      }
      if (treeEdgeCount > BigInt(limits.maxTreeEdges)) {
        resourceFailure = true
        errors += BagFactorPlanningError.ResourceLimitExceeded(
          BagFactorResource.TreeEdges,
          treeEdgeCount,
          limits.maxTreeEdges
        )
      }
      if (sourceEdgeCount > BigInt(limits.maxSourceEdges)) {
        resourceFailure = true
        errors += BagFactorPlanningError.ResourceLimitExceeded(
          BagFactorResource.SourceEdges,
          sourceEdgeCount,
          limits.maxSourceEdges
        )
      }
      if (!resourceFailure) {
        val sourceVertexOccurrences = input.hypergraph.edges.iterator
          .foldLeft(BigInt(0))((total, edge) => total + edge.vertices.size)
        val ownershipChecks =
          bagCount.max(BigInt(1)) * sourceVertexOccurrences
        if (ownershipChecks > BigInt(limits.maxOwnershipChecks)) {
          resourceFailure = true
          errors += BagFactorPlanningError.ResourceLimitExceeded(
            BagFactorResource.OwnershipChecks,
            ownershipChecks,
            limits.maxOwnershipChecks
          )
        }
      }
    }

    val certificate =
      if (
        (input eq null) || (decomposition eq null) || (limits eq null) ||
        resourceFailure
      ) None
      else {
        CandidateDecompositionValidator.validate(input, decomposition) match {
          case Left(problems) =>
            errors += BagFactorPlanningError.CandidateCertificateViolations(
              problems
            )
            None
          case Right(value) => Some(value)
        }
      }

    if ((decomposition ne null) && !resourceFailure) {
      decomposition.bags
        .filter(_.vertices.isEmpty)
        .sortBy(_.id)
        .foreach(bag => errors += BagFactorPlanningError.EmptyBag(bag.id))
    }

    val canonicalErrors = BagFactorPlanningError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) Left(canonicalErrors)
    else
      certificate match {
        case Some(value) =>
          buildValidated(input, decomposition, value).flatMap { plan =>
            BagFactorPlanValidator
              .validate(input, decomposition, plan, limits)
              .left
              .map(problems =>
                Vector(
                  BagFactorPlanningError.IndependentValidationFailure(problems)
                )
              )
          }
        case None =>
          Left(
            Vector(
              BagFactorPlanningError.ModelInvariantViolation(
                "Validated candidate certificate is unavailable"
              )
            )
          )
      }
  }

  private[lowering] def buildValidated(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      certificate: CandidateCertificate
  ): Either[Vector[BagFactorPlanningError], BagFactorPlan] = {
    orient(decomposition).flatMap { orientation =>
      assignOwnership(input, decomposition, orientation).flatMap { ownership =>
        val bagById = decomposition.bags.iterator.map(bag => bag.id -> bag).toMap
        val ownedByBag = ownership
          .groupBy(_.owner)
          .view
          .mapValues(_.map(_.edgeId).sorted)
          .toMap

        val factors = decomposition.bags.sortBy(_.id).map { bag =>
          BagFactor(
            bagId = bag.id,
            vertices = bag.vertices,
            supportCover = bag.cover,
            ownedEdgeIds = ownedByBag.getOrElse(bag.id, Vector.empty),
            depth = orientation.depthByBag(bag.id),
            parent = orientation.parentByBag(bag.id),
            children = orientation.childrenByBag.getOrElse(
              bag.id,
              Vector.empty
            )
          )
        }

        val arcs = orientation.preorder.tail.flatMap { child =>
          orientation.parentByBag(child).map { parent =>
            OrientedBagArc(
              parent,
              child,
              separator(bagById(parent), bagById(child))
            )
          }
        }
        val separatorByChild = arcs.iterator.map(arc => arc.child -> arc.separator).toMap

        val bottomUp = orientation.postorder
          .filterNot(_ == decomposition.root)
          .flatMap { child =>
            orientation.parentByBag(child).map(parent => child -> parent)
          }
          .zipWithIndex
          .map { case ((child, parent), ordinal) =>
            SemijoinStep(
              ordinal = ordinal,
              pass = SemijoinPass.BottomUp,
              target = parent,
              source = child,
              separator = separatorByChild.getOrElse(child, Vector.empty)
            )
          }

        val topDown = orientation.preorder.tail
          .flatMap { child =>
            orientation.parentByBag(child).map(parent => parent -> child)
          }
          .zipWithIndex
          .map { case ((parent, child), ordinal) =>
            SemijoinStep(
              ordinal = ordinal,
              pass = SemijoinPass.TopDown,
              target = child,
              source = parent,
              separator = separatorByChild.getOrElse(child, Vector.empty)
            )
          }

        val plan = BagFactorPlan(
          sourceHypergraph = input.hypergraph,
          sourceWidthBound = input.widthBound,
          requireConnectedCover = input.requireConnectedCover,
          decomposition = decomposition,
          certificate = certificate,
          root = decomposition.root,
          factors = factors,
          ownership = ownership,
          arcs = arcs,
          schedule = YannakakisSchedule(bottomUp, topDown)
        )

        constructedInvariantErrors(input, plan) match {
          case problems if problems.nonEmpty =>
            Left(
              BagFactorPlanningError.canonical(
                problems.map(BagFactorPlanningError.ModelInvariantViolation)
              )
            )
          case _ => Right(plan)
        }
      }
    }
  }

  private def orient(
      decomposition: CoveredDecomposition
  ): Either[Vector[BagFactorPlanningError], RootedOrientation] = {
    val adjacency = mutable.Map.empty[BagId, mutable.ArrayBuffer[BagId]]
    decomposition.bagIds.foreach(id => adjacency.put(id, mutable.ArrayBuffer.empty))
    decomposition.treeEdges.foreach { edge =>
      adjacency.getOrElseUpdate(edge.first, mutable.ArrayBuffer.empty) += edge.second
      adjacency.getOrElseUpdate(edge.second, mutable.ArrayBuffer.empty) += edge.first
    }

    val parent = mutable.Map.empty[BagId, Option[BagId]]
    val children = mutable.Map.empty[BagId, Vector[BagId]]
    val depth = mutable.Map.empty[BagId, Int]
    val preorder = Vector.newBuilder[BagId]
    val visited = mutable.HashSet.empty[BagId]
    val pending = mutable.Stack[(BagId, Option[BagId], Int)](
      (decomposition.root, None, 0)
    )

    while (pending.nonEmpty) {
      val (current, currentParent, currentDepth) = pending.pop()
      if (!visited(current)) {
        visited += current
        parent.put(current, currentParent)
        depth.put(current, currentDepth)
        preorder += current
        val next = adjacency
          .getOrElse(current, mutable.ArrayBuffer.empty)
          .iterator
          .filterNot(currentParent.contains)
          .filterNot(visited)
          .toVector
          .distinct
          .sorted
        children.put(current, next)
        next.reverseIterator.foreach(child =>
          pending.push((child, Some(current), currentDepth + 1))
        )
      }
    }

    val preorderResult = preorder.result()
    if (preorderResult.size != decomposition.bags.size) {
      Left(
        Vector(
          BagFactorPlanningError.ModelInvariantViolation(
            "Rooted orientation did not visit every decomposition bag"
          )
        )
      )
    } else {
      val postorder = Vector.newBuilder[BagId]
      val postorderPending = mutable.Stack[(BagId, Boolean)](
        decomposition.root -> false
      )
      while (postorderPending.nonEmpty) {
        val (current, expanded) = postorderPending.pop()
        if (expanded) postorder += current
        else {
          postorderPending.push(current -> true)
          children
            .getOrElse(current, Vector.empty)
            .reverseIterator
            .foreach(child => postorderPending.push(child -> false))
        }
      }

      Right(
        RootedOrientation(
          preorderResult,
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
      orientation: RootedOrientation
  ): Either[Vector[BagFactorPlanningError], Vector[EdgeOwnership]] = {
    val bags = decomposition.bags.sortBy(_.id)
    val ownership = Vector.newBuilder[EdgeOwnership]
    val errors = Vector.newBuilder[BagFactorPlanningError]

    input.hypergraph.edges.foreach { edge =>
      val eligible = bags.filter(bag => bag.containsAll(edge.vertices))
      if (eligible.isEmpty) {
        errors += BagFactorPlanningError.MissingEligibleOwner(edge.id)
      } else {
        val minimumDepth = eligible.iterator
          .map(bag => orientation.depthByBag(bag.id))
          .min
        val shallowest = eligible
          .filter(bag => orientation.depthByBag(bag.id) == minimumDepth)
          .map(_.id)
          .sorted

        if (shallowest.size != 1) {
          errors += BagFactorPlanningError.AmbiguousRootNearestOwner(
            edge.id,
            minimumDepth,
            shallowest
          )
        } else {
          ownership += EdgeOwnership(
            edge.id,
            shallowest.head,
            minimumDepth
          )
        }
      }
    }

    val canonicalErrors = BagFactorPlanningError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) Left(canonicalErrors)
    else Right(ownership.result())
  }

  private def separator(
      parent: CoveredBag,
      child: CoveredBag
  ): Vector[VariableId] =
    (parent.vertices.toSet intersect child.vertices.toSet).toVector.sorted

  private def constructedInvariantErrors(
      input: CandidateSearchInput,
      plan: BagFactorPlan
  ): Vector[String] = {
    val problems = Vector.newBuilder[String]
    if (plan.ownership.map(_.edgeId) != input.hypergraph.edgeIds) {
      problems += "Ownership does not contain every original edge exactly once"
    }
    if (plan.factors.map(_.bagId) != plan.decomposition.bagIds.sorted) {
      problems += "Factors do not correspond one-to-one with decomposition bags"
    }
    val expectedArcCount = math.max(0, plan.factors.size - 1)
    if (plan.arcs.size != expectedArcCount) {
      problems += "Oriented arc count does not describe a tree"
    }
    if (
      plan.schedule.bottomUp.size != expectedArcCount ||
      plan.schedule.topDown.size != expectedArcCount
    ) {
      problems += "Yannakakis passes do not contain one step per oriented arc"
    }
    problems.result()
  }

  private final case class RootedOrientation(
      preorder: Vector[BagId],
      postorder: Vector[BagId],
      parentByBag: Map[BagId, Option[BagId]],
      childrenByBag: Map[BagId, Vector[BagId]],
      depthByBag: Map[BagId, Int]
  )
}
