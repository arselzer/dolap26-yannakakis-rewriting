package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateCertificate,
  CandidateCertificateError
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, QueryHypergraph}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition

sealed trait BagFactorPlanningError extends Product with Serializable {
  def code: String
  def message: String
}

object BagFactorPlanningError {
  final case class MissingValue(path: String) extends BagFactorPlanningError {
    val code = "lowering.input.missing"
    val message = s"Missing $path"
  }
  final case class CandidateCertificateViolations(
      errors: Vector[CandidateCertificateError]
  ) extends BagFactorPlanningError {
    val code = "lowering.candidate-certificate"
    val message = s"Candidate decomposition validation failed (${errors.size} errors)"
  }
  final case class EmptyBag(bagId: BagId) extends BagFactorPlanningError {
    val code = "lowering.bag.empty"
    val message = s"Real bag ${bagId.value} must not be empty"
  }
  final case class MissingEligibleOwner(edgeId: EdgeId)
      extends BagFactorPlanningError {
    val code = "lowering.ownership.missing"
    val message = s"No bag contains every vertex of edge ${edgeId.value}"
  }
  final case class AmbiguousRootNearestOwner(
      edgeId: EdgeId,
      depth: Int,
      candidateBagIds: Vector[BagId]
  ) extends BagFactorPlanningError {
    val code = "lowering.ownership.ambiguous"
    val message = s"Edge ${edgeId.value} has ${candidateBagIds.size} containing bags at minimum depth $depth"
  }
  final case class InvalidResourceLimit(
      resource: BagFactorResource,
      value: Long
  ) extends BagFactorPlanningError {
    val code = "lowering.resource.invalid"
    val message = s"${resource.code} limit must be positive, but was $value"
  }
  final case class ResourceLimitExceeded(
      resource: BagFactorResource,
      required: BigInt,
      limit: Long
  ) extends BagFactorPlanningError {
    val code = "lowering.resource.exceeded"
    val message = s"${resource.code} requires $required items, exceeding limit $limit"
  }
  final case class ModelInvariantViolation(detail: String)
      extends BagFactorPlanningError {
    val code = "lowering.model.invariant"
    val message = detail
  }
  final case class IndependentValidationFailure(
      errors: Vector[BagFactorPlanValidationError]
  ) extends BagFactorPlanningError {
    val code = "lowering.independent-validation"
    val message = s"Independent bag-factor validation failed (${errors.size} errors)"
  }

  def canonical(
      errors: Iterable[BagFactorPlanningError]
  ): Vector[BagFactorPlanningError] =
    errors.iterator.toVector.distinct.sortBy(error => (error.code, error.message))
}

sealed trait BagFactorPlanValidationError extends Product with Serializable {
  def code: String
  def message: String
}

object BagFactorPlanValidationError {
  final case class MissingValue(path: String)
      extends BagFactorPlanValidationError {
    val code = "lowering.validation.missing"
    val message = s"Missing $path"
  }
  final case class CandidateCertificateViolations(
      errors: Vector[CandidateCertificateError]
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.candidate-certificate"
    val message = s"Candidate decomposition validation failed (${errors.size} errors)"
  }
  final case class ResourceLimitExceeded(
      resource: BagFactorResource,
      required: BigInt,
      limit: Long
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.resource.exceeded"
    val message = s"${resource.code} requires $required items, exceeding limit $limit"
  }
  final case class EmptyBag(bagId: BagId)
      extends BagFactorPlanValidationError {
    val code = "lowering.validation.bag.empty"
    val message = s"Real bag ${bagId.value} must not be empty"
  }
  final case class MissingEligibleOwner(edgeId: EdgeId)
      extends BagFactorPlanValidationError {
    val code = "lowering.validation.ownership.missing"
    val message = s"No bag contains every vertex of edge ${edgeId.value}"
  }
  final case class AmbiguousRootNearestOwner(
      edgeId: EdgeId,
      depth: Int,
      candidateBagIds: Vector[BagId]
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.ownership.ambiguous"
    val message = s"Edge ${edgeId.value} has ${candidateBagIds.size} containing bags at minimum depth $depth"
  }
  final case class OrientationInvariantViolation(detail: String)
      extends BagFactorPlanValidationError {
    val code = "lowering.validation.orientation.invariant"
    val message = detail
  }
  final case class SourceHypergraphMismatch(
      expected: QueryHypergraph,
      actual: QueryHypergraph
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.provenance.hypergraph"
    val message = "Bag-factor plan is bound to a different source hypergraph"
  }
  final case class SourceWidthMismatch(expected: Int, actual: Int)
      extends BagFactorPlanValidationError {
    val code = "lowering.validation.provenance.width"
    val message = s"Bag-factor plan width provenance is $actual, expected $expected"
  }
  final case class AdmissibilityModeMismatch(
      expected: Boolean,
      actual: Boolean
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.provenance.admissibility"
    val message = s"Bag-factor plan connected-cover mode is $actual, expected $expected"
  }
  final case class DecompositionMismatch(
      expected: CoveredDecomposition,
      actual: CoveredDecomposition
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.decomposition"
    val message = "Bag-factor plan is bound to a different covered decomposition"
  }
  final case class CertificateMismatch(
      expected: CandidateCertificate,
      actual: CandidateCertificate
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.certificate"
    val message = "Bag-factor plan carries a stale or different candidate certificate"
  }
  final case class RootMismatch(expected: BagId, actual: BagId)
      extends BagFactorPlanValidationError {
    val code = "lowering.validation.root"
    val message = "Bag-factor plan root differs from the decomposition root"
  }
  final case class FactorsMismatch(
      expected: Vector[BagFactor],
      actual: Vector[BagFactor]
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.factors"
    val message = "Bag factors differ from the canonical derived factors"
  }
  final case class OwnershipMismatch(
      expected: Vector[EdgeOwnership],
      actual: Vector[EdgeOwnership]
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.ownership"
    val message = "Edge ownership differs from the canonical assignment"
  }
  final case class ArcsMismatch(
      expected: Vector[OrientedBagArc],
      actual: Vector[OrientedBagArc]
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.arcs"
    val message = "Oriented arcs or separators differ from the canonical tree"
  }
  final case class ScheduleMismatch(
      expected: YannakakisSchedule,
      actual: YannakakisSchedule
  ) extends BagFactorPlanValidationError {
    val code = "lowering.validation.schedule"
    val message = "Yannakakis schedule differs from the canonical schedule"
  }

  def canonical(
      errors: Iterable[BagFactorPlanValidationError]
  ): Vector[BagFactorPlanValidationError] =
    errors.iterator.toVector.distinct.sortBy(error => (error.code, error.message))
}
