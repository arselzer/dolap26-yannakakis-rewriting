package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateCertificate,
  CandidateDecompositionValidator,
  CandidateSearchInput
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, VariableId}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  CoveredBag,
  CoveredDecomposition
}

import scala.collection.mutable

/**
  * Independently validates provenance and every derived bag-factor invariant.
  *
  * This boundary deliberately does not invoke [[BagFactorPlanner]] or any of
  * its construction helpers. It derives the rooted tree, ownership, factors,
  * separators, and schedule again so a construction defect cannot validate
  * itself by simple recomputation through the same implementation.
  */
object BagFactorPlanValidator {
  def validate(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      plan: BagFactorPlan
  ): Either[Vector[BagFactorPlanValidationError], BagFactorPlan] =
    validate(input, decomposition, plan, BagFactorPlanningLimits.default)

  def validate(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      plan: BagFactorPlan,
      limits: BagFactorPlanningLimits
  ): Either[Vector[BagFactorPlanValidationError], BagFactorPlan] = {
    val missing = Vector.newBuilder[BagFactorPlanValidationError]
    if (input eq null) {
      missing += BagFactorPlanValidationError.MissingValue(
        "candidate-search input"
      )
    }
    if (decomposition eq null) {
      missing += BagFactorPlanValidationError.MissingValue(
        "covered decomposition"
      )
    }
    if (plan eq null) {
      missing += BagFactorPlanValidationError.MissingValue("bag-factor plan")
    }
    if (limits eq null) {
      missing += BagFactorPlanValidationError.MissingValue(
        "bag-factor planning limits"
      )
    }

    val missingErrors =
      BagFactorPlanValidationError.canonical(missing.result())
    if (missingErrors.nonEmpty) return Left(missingErrors)

    val boundedErrors = resourceErrors(input, decomposition, limits)
    if (boundedErrors.nonEmpty) return Left(boundedErrors)

    val candidateCertificate =
      CandidateDecompositionValidator.validate(input, decomposition)
    val prerequisiteErrors = Vector.newBuilder[BagFactorPlanValidationError]
    candidateCertificate match {
      case Left(problems) =>
        prerequisiteErrors +=
          BagFactorPlanValidationError.CandidateCertificateViolations(problems)
      case Right(_) => ()
    }
    decomposition.bags
      .filter(_.vertices.isEmpty)
      .sortBy(_.id)
      .foreach(bag =>
        prerequisiteErrors += BagFactorPlanValidationError.EmptyBag(bag.id)
      )

    val canonicalPrerequisiteErrors =
      BagFactorPlanValidationError.canonical(prerequisiteErrors.result())
    if (canonicalPrerequisiteErrors.nonEmpty) {
      Left(canonicalPrerequisiteErrors)
    } else {
      candidateCertificate match {
        case Right(certificate) =>
          deriveCanonical(input, decomposition) match {
            case Left(problems) => Left(problems)
            case Right(derived) =>
              compareCanonical(
                input,
                decomposition,
                certificate,
                derived,
                plan
              )
          }
        case Left(_) =>
          Left(
            Vector(
              BagFactorPlanValidationError.OrientationInvariantViolation(
                "Validated candidate certificate is unavailable"
              )
            )
          )
      }
    }
  }

  private def resourceErrors(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      limits: BagFactorPlanningLimits
  ): Vector[BagFactorPlanValidationError] = {
    val bagCount = BigInt(decomposition.bags.size)
    val treeEdgeCount = BigInt(decomposition.treeEdges.size)
    val sourceEdgeCount = BigInt(input.hypergraph.edges.size)

    val countErrors = BagFactorPlanValidationError.canonical(
      Vector(
        (BagFactorResource.Bags, bagCount, limits.maxBags),
        (BagFactorResource.TreeEdges, treeEdgeCount, limits.maxTreeEdges),
        (BagFactorResource.SourceEdges, sourceEdgeCount, limits.maxSourceEdges)
      ).collect {
        case (resource, required, limit) if required > BigInt(limit) =>
          BagFactorPlanValidationError.ResourceLimitExceeded(
            resource,
            required,
            limit
          )
      }
    )

    if (countErrors.nonEmpty) countErrors
    else {
      val sourceVertexOccurrences = input.hypergraph.edges.iterator
        .foldLeft(BigInt(0))((total, edge) => total + edge.vertices.size)
      val ownershipChecks =
        bagCount.max(BigInt(1)) * sourceVertexOccurrences
      if (ownershipChecks > BigInt(limits.maxOwnershipChecks)) {
        Vector(
          BagFactorPlanValidationError.ResourceLimitExceeded(
            BagFactorResource.OwnershipChecks,
            ownershipChecks,
            limits.maxOwnershipChecks
          )
        )
      } else Vector.empty
    }
  }

  private def deriveCanonical(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition
  ): Either[Vector[BagFactorPlanValidationError], CanonicalDerived] = {
    deriveOrientation(decomposition).flatMap { orientation =>
      val parentByChild = orientation.parentByBag.collect {
        case (child, Some(parent)) => child -> parent
      }
      val missingParents = orientation.preorder.tail.filterNot(
        parentByChild.contains
      )

      if (missingParents.nonEmpty) {
        Left(
          Vector(
            BagFactorPlanValidationError.OrientationInvariantViolation(
              s"Non-root bags lack parents: ${missingParents.map(_.value).mkString(",")}"
            )
          )
        )
      } else deriveOwnership(input, decomposition, orientation).map { ownership =>
        val bagById = decomposition.bags.iterator.map(bag => bag.id -> bag).toMap
        val ownedByBag = ownership
          .groupBy(_.owner)
          .iterator
          .map { case (bagId, values) => bagId -> values.map(_.edgeId).sorted }
          .toMap

        val factors = decomposition.bags.sortBy(_.id).map { bag =>
          BagFactor(
            bagId = bag.id,
            vertices = bag.vertices,
            supportCover = bag.cover,
            ownedEdgeIds = ownedByBag.getOrElse(bag.id, Vector.empty),
            depth = orientation.depthByBag(bag.id),
            parent = orientation.parentByBag(bag.id),
            children = orientation.childrenByBag(bag.id)
          )
        }

        val arcs = orientation.preorder.tail.map { child =>
          val parent = parentByChild(child)
          OrientedBagArc(
            parent = parent,
            child = child,
            separator = separator(bagById(parent), bagById(child))
          )
        }
        val separatorByChild =
          arcs.iterator.map(arc => arc.child -> arc.separator).toMap

        val bottomUp = orientation.postorder
          .filterNot(_ == decomposition.root)
          .zipWithIndex
          .map { case (child, ordinal) =>
            SemijoinStep(
              ordinal = ordinal,
              pass = SemijoinPass.BottomUp,
              target = parentByChild(child),
              source = child,
              separator = separatorByChild(child)
            )
          }

        val topDown = orientation.preorder.tail.zipWithIndex.map {
          case (child, ordinal) =>
            SemijoinStep(
              ordinal = ordinal,
              pass = SemijoinPass.TopDown,
              target = child,
              source = parentByChild(child),
              separator = separatorByChild(child)
            )
        }

        CanonicalDerived(
          root = decomposition.root,
          factors = factors,
          ownership = ownership,
          arcs = arcs,
          schedule = YannakakisSchedule(bottomUp, topDown)
        )
      }
    }
  }

  /**
    * Establishes parents and depths with BFS, then independently materializes
    * the required sorted-child DFS traversals for the two schedule passes.
    */
  private def deriveOrientation(
      decomposition: CoveredDecomposition
  ): Either[Vector[BagFactorPlanValidationError], RootedOrientation] = {
    val adjacency = mutable.Map.empty[BagId, mutable.ArrayBuffer[BagId]]
    decomposition.bagIds.foreach(id => adjacency.put(id, mutable.ArrayBuffer.empty))
    decomposition.treeEdges.foreach { edge =>
      adjacency(edge.first) += edge.second
      adjacency(edge.second) += edge.first
    }

    val parent = mutable.Map[BagId, Option[BagId]](decomposition.root -> None)
    val depth = mutable.Map[BagId, Int](decomposition.root -> 0)
    val pending = mutable.Queue(decomposition.root)

    while (pending.nonEmpty) {
      val current = pending.dequeue()
      adjacency(current).toVector.distinct.sorted.foreach { neighbor =>
        if (!parent.contains(neighbor)) {
          parent.put(neighbor, Some(current))
          depth.put(neighbor, depth(current) + 1)
          pending.enqueue(neighbor)
        }
      }
    }

    val bagIds = decomposition.bagIds.toSet
    if (parent.keySet.toSet != bagIds) {
      val missing = (bagIds -- parent.keySet).toVector.sorted
      Left(
        Vector(
          BagFactorPlanValidationError.OrientationInvariantViolation(
            s"Independent orientation did not reach bags ${missing.map(_.value).mkString(",")}"
          )
        )
      )
    } else {
      val childBuffers = mutable.Map.empty[BagId, mutable.ArrayBuffer[BagId]]
      bagIds.foreach(id => childBuffers.put(id, mutable.ArrayBuffer.empty))
      parent.foreach {
        case (child, Some(parentId)) => childBuffers(parentId) += child
        case (_, None)               => ()
      }
      val childrenByBag = childBuffers.iterator.map {
        case (bagId, children) => bagId -> children.toVector.sorted
      }.toMap

      val preorder = Vector.newBuilder[BagId]
      val preorderPending = mutable.Stack(decomposition.root)
      while (preorderPending.nonEmpty) {
        val current = preorderPending.pop()
        preorder += current
        childrenByBag(current).reverseIterator.foreach(child =>
          preorderPending.push(child)
        )
      }

      val postorder = Vector.newBuilder[BagId]
      val postorderPending = mutable.Stack[(BagId, Boolean)](
        decomposition.root -> false
      )
      while (postorderPending.nonEmpty) {
        val (current, expanded) = postorderPending.pop()
        if (expanded) postorder += current
        else {
          postorderPending.push(current -> true)
          childrenByBag(current).reverseIterator.foreach(child =>
            postorderPending.push(child -> false)
          )
        }
      }

      val preorderResult = preorder.result()
      val postorderResult = postorder.result()
      val expectedArcCount = math.max(0, decomposition.bags.size - 1)
      val derivedArcCount = parent.valuesIterator.count(_.nonEmpty)
      if (
        preorderResult.size != decomposition.bags.size ||
        postorderResult.size != decomposition.bags.size ||
        derivedArcCount != expectedArcCount
      ) {
        Left(
          Vector(
            BagFactorPlanValidationError.OrientationInvariantViolation(
              "Independent orientation is not a rooted tree over every bag"
            )
          )
        )
      } else {
        Right(
          RootedOrientation(
            preorder = preorderResult,
            postorder = postorderResult,
            parentByBag = parent.toMap,
            childrenByBag = childrenByBag,
            depthByBag = depth.toMap
          )
        )
      }
    }
  }

  private def deriveOwnership(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      orientation: RootedOrientation
  ): Either[Vector[BagFactorPlanValidationError], Vector[EdgeOwnership]] = {
    val bags = decomposition.bags.sortBy(_.id)
    val ownership = Vector.newBuilder[EdgeOwnership]
    val errors = Vector.newBuilder[BagFactorPlanValidationError]

    input.hypergraph.edges.foreach { edge =>
      var minimumDepth = Int.MaxValue
      val shallowest = Vector.newBuilder[BagId]

      bags.foreach { bag =>
        if (bag.containsAll(edge.vertices)) {
          val bagDepth = orientation.depthByBag(bag.id)
          if (bagDepth < minimumDepth) {
            minimumDepth = bagDepth
            shallowest.clear()
            shallowest += bag.id
          } else if (bagDepth == minimumDepth) {
            shallowest += bag.id
          }
        }
      }

      val candidates = shallowest.result().sorted
      if (candidates.isEmpty) {
        errors += BagFactorPlanValidationError.MissingEligibleOwner(edge.id)
      } else if (candidates.size != 1) {
        errors += BagFactorPlanValidationError.AmbiguousRootNearestOwner(
          edge.id,
          minimumDepth,
          candidates
        )
      } else {
        ownership += EdgeOwnership(edge.id, candidates.head, minimumDepth)
      }
    }

    val canonicalErrors =
      BagFactorPlanValidationError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) Left(canonicalErrors)
    else Right(ownership.result())
  }

  private def separator(
      parent: CoveredBag,
      child: CoveredBag
  ): Vector[VariableId] = {
    val childVertices = child.vertices.toSet
    parent.vertices.filter(childVertices.contains)
  }

  private def compareCanonical(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      certificate: CandidateCertificate,
      expected: CanonicalDerived,
      actual: BagFactorPlan
  ): Either[Vector[BagFactorPlanValidationError], BagFactorPlan] = {
    val errors = Vector.newBuilder[BagFactorPlanValidationError]

    if (actual.sourceHypergraph != input.hypergraph) {
      errors += BagFactorPlanValidationError.SourceHypergraphMismatch(
        input.hypergraph,
        actual.sourceHypergraph
      )
    }
    if (actual.sourceWidthBound != input.widthBound) {
      errors += BagFactorPlanValidationError.SourceWidthMismatch(
        input.widthBound,
        actual.sourceWidthBound
      )
    }
    if (actual.requireConnectedCover != input.requireConnectedCover) {
      errors += BagFactorPlanValidationError.AdmissibilityModeMismatch(
        input.requireConnectedCover,
        actual.requireConnectedCover
      )
    }
    if (actual.decomposition != decomposition) {
      errors += BagFactorPlanValidationError.DecompositionMismatch(
        decomposition,
        actual.decomposition
      )
    }
    if (actual.certificate != certificate) {
      errors += BagFactorPlanValidationError.CertificateMismatch(
        certificate,
        actual.certificate
      )
    }
    if (actual.root != expected.root) {
      errors += BagFactorPlanValidationError.RootMismatch(
        expected.root,
        actual.root
      )
    }
    if (actual.factors != expected.factors) {
      errors += BagFactorPlanValidationError.FactorsMismatch(
        expected.factors,
        actual.factors
      )
    }
    if (actual.ownership != expected.ownership) {
      errors += BagFactorPlanValidationError.OwnershipMismatch(
        expected.ownership,
        actual.ownership
      )
    }
    if (actual.arcs != expected.arcs) {
      errors += BagFactorPlanValidationError.ArcsMismatch(
        expected.arcs,
        actual.arcs
      )
    }
    if (actual.schedule != expected.schedule) {
      errors += BagFactorPlanValidationError.ScheduleMismatch(
        expected.schedule,
        actual.schedule
      )
    }

    val canonicalErrors =
      BagFactorPlanValidationError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) Left(canonicalErrors) else Right(actual)
  }

  private final case class RootedOrientation(
      preorder: Vector[BagId],
      postorder: Vector[BagId],
      parentByBag: Map[BagId, Option[BagId]],
      childrenByBag: Map[BagId, Vector[BagId]],
      depthByBag: Map[BagId, Int]
  )

  private final case class CanonicalDerived(
      root: BagId,
      factors: Vector[BagFactor],
      ownership: Vector[EdgeOwnership],
      arcs: Vector[OrientedBagArc],
      schedule: YannakakisSchedule
  )
}
