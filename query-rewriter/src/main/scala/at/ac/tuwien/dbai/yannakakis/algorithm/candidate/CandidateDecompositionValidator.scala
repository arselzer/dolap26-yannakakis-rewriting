package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{ConnectedCover, EdgeCover}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  CoveredBag,
  CoveredDecomposition,
  DecompositionValidator
}

import scala.collection.mutable

/**
  * Independent certificate boundary for physical CandidateTDs.
  *
  * In addition to the core decomposition validator, this checks structural
  * candidate membership, exact physical-cover admissibility, ConCov, and the
  * component-normal-form equation on every oriented tree arc.
  */
object CandidateDecompositionValidator {
  def validate(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition
  ): Either[Vector[CandidateCertificateError], CandidateCertificate] = {
    if (input eq null) {
      return Left(
        Vector(CandidateCertificateError.MissingValue("candidate-search input"))
      )
    }
    if (decomposition eq null) {
      return Left(
        Vector(CandidateCertificateError.MissingValue("covered decomposition"))
      )
    }

    DecompositionValidator.validate(input.hypergraph, decomposition) match {
      case Left(coreErrors) =>
        Left(
          CandidateCertificateError.canonical(
            coreErrors.map(CandidateCertificateError.CoreDecompositionViolation)
          )
        )
      case Right(_) => validateCandidateCertificate(input, decomposition)
    }
  }

  private def validateCandidateCertificate(
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition
  ): Either[Vector[CandidateCertificateError], CandidateCertificate] = {
    val errors = Vector.newBuilder[CandidateCertificateError]

    decomposition.bag(decomposition.root).foreach { root =>
      if (root.vertices.isEmpty) {
        errors += CandidateCertificateError.EmptyRootBag(root.id)
      }
    }

    decomposition.bags.foreach { bag =>
      input.candidateBag(bag.vertices) match {
        case None =>
          errors += CandidateCertificateError.BagNotCandidate(
            bag.id,
            bag.vertices
          )
        case Some(candidate) =>
          if (!candidate.physicalCovers.exists(_.edgeIds == bag.cover)) {
            errors += CandidateCertificateError.CoverNotAdmissible(
              bag.id,
              bag.cover
            )
          }
      }

      if (input.requireConnectedCover) {
        verifyConnectedCover(input, bag, errors)
      }
    }

    val (childrenByParent, orientedEdges, traversalOrder) =
      orient(decomposition)
    val bagById = decomposition.bags.iterator.map(bag => bag.id -> bag).toMap
    val subtreeVertices = mutable.Map.empty[BagId, Set[VariableId]]

    traversalOrder.reverseIterator.foreach { bagId =>
      val childVertices = childrenByParent
        .getOrElse(bagId, Vector.empty)
        .iterator
        .flatMap(childId => subtreeVertices.getOrElse(childId, Set.empty))
        .toSet
      subtreeVertices.put(bagId, bagById(bagId).vertices.toSet union childVertices)
    }

    val componentsBySeparator = mutable.Map.empty[
      Vector[VariableId],
      Either[CandidateFailure, Vector[CandidateComponent]]
    ]
    val arcs = Vector.newBuilder[ComponentNormalFormArc]

    orientedEdges.foreach { case (parentId, childId) =>
      val parent = bagById(parentId)
      val child = bagById(childId)

      if (parent.vertices == child.vertices) {
        errors += CandidateCertificateError.EqualAdjacentBags(
          parentId,
          childId
        )
      }

      val components = componentsBySeparator.getOrElseUpdate(
        parent.vertices,
        VertexComponentEngine.components(input.hypergraph, parent.vertices)
      )
      components match {
        case Left(failure) =>
          errors += CandidateCertificateError.ComponentPredicateFailure(
            parentId,
            childId,
            failure.message
          )
        case Right(values) =>
          val subtree = subtreeVertices(childId)
          val separatorIntersection =
            parent.vertices.toSet intersect child.vertices.toSet
          val matching = values.filter { component =>
            (component.vertexSet union separatorIntersection) == subtree
          }
          if (matching.size != 1) {
            errors += CandidateCertificateError.ComponentNormalFormViolation(
              parentId,
              childId,
              subtree.toVector.sorted,
              matching.map(_.vertices)
            )
          } else {
            arcs += ComponentNormalFormArc(
              parentId,
              childId,
              matching.head.vertices
            )
          }
      }
    }

    val canonicalErrors =
      CandidateCertificateError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) Left(canonicalErrors)
    else {
      Right(
        CandidateCertificate(
          arcs.result().sortBy(arc => (arc.parent.value, arc.child.value)),
          input.requireConnectedCover,
          decomposition.bags.size
        )
      )
    }
  }

  private def verifyConnectedCover(
      input: CandidateSearchInput,
      bag: CoveredBag,
      errors: mutable.Builder[
        CandidateCertificateError,
        Vector[CandidateCertificateError]
      ]
  ): Unit = {
    EdgeCover.create(bag.cover) match {
      case Left(failure) =>
        errors += CandidateCertificateError.CoverPredicateFailure(
          bag.id,
          failure.message
        )
      case Right(cover) =>
        ConnectedCover.isConnected(input.hypergraph, cover) match {
          case Left(failure) =>
            errors += CandidateCertificateError.CoverPredicateFailure(
              bag.id,
              failure.message
            )
          case Right(false) =>
            errors += CandidateCertificateError.ConnectedCoverViolation(
              bag.id,
              bag.cover
            )
          case Right(true) => ()
        }
    }
  }

  private def orient(
      decomposition: CoveredDecomposition
  ): (
      Map[BagId, Vector[BagId]],
      Vector[(BagId, BagId)],
      Vector[BagId]
  ) = {
    val children = mutable.Map.empty[BagId, Vector[BagId]]
    val edges = Vector.newBuilder[(BagId, BagId)]
    val order = Vector.newBuilder[BagId]
    val visited = mutable.Set.empty[BagId]
    val queue = mutable.Queue(decomposition.root)

    while (queue.nonEmpty) {
      val parent = queue.dequeue()
      if (!visited(parent)) {
        visited += parent
        order += parent
        val next = decomposition
          .neighbors(parent)
          .filterNot(visited)
          .sorted
        children.put(parent, next)
        next.foreach { child =>
          edges += parent -> child
          queue.enqueue(child)
        }
      }
    }

    (children.toMap, edges.result(), order.result())
  }
}
