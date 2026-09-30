package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  EdgeId,
  QueryHypergraph,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.DecompositionValidationError

sealed trait CandidateInputError extends Product with Serializable {
  def code: String
  def message: String
}

object CandidateInputError {
  final case class MissingValue(path: String) extends CandidateInputError {
    val code = "candidate.input.missing"
    val message = s"Missing $path"
  }
  final case class InvalidWidthBound(value: Int) extends CandidateInputError {
    val code = "candidate.input.width"
    val message = s"Candidate width bound must be positive, but was $value"
  }
  final case class SourceHypergraphMismatch(
      requested: QueryHypergraph,
      generatedFrom: QueryHypergraph
  ) extends CandidateInputError {
    val code = "candidate.input.generation.hypergraph"
    val message = "Admissible candidates were generated from a different query hypergraph"
  }
  final case class SourceWidthMismatch(
      requested: Int,
      generatedAt: Int
  ) extends CandidateInputError {
    val code = "candidate.input.generation.width"
    val message = s"Admissible candidates were generated at width $generatedAt, not requested width $requested"
  }
  case object EmptyHypergraph extends CandidateInputError {
    val code = "candidate.input.hypergraph.empty"
    val message = "Candidate search requires a non-empty hypergraph"
  }
  case object HypergraphWithoutVertices extends CandidateInputError {
    val code = "candidate.input.hypergraph.vertices.empty"
    val message = "Candidate search requires at least one hypergraph vertex"
  }
  final case class EmptyHyperedges(edgeIds: Vector[EdgeId])
      extends CandidateInputError {
    val code = "candidate.input.hypergraph.edge.empty"
    val message = s"Connected candidate search does not admit empty hyperedges ${edgeIds.mkString("[", ",", "]")}"
  }
  final case class DisconnectedHypergraph(
      components: Vector[Vector[VariableId]]
  ) extends CandidateInputError {
    val code = "candidate.input.hypergraph.disconnected"
    val message = s"Candidate search requires a connected hypergraph; found ${components.size} components"
  }
  final case class ComponentComputationFailure(detail: String)
      extends CandidateInputError {
    val code = "candidate.input.hypergraph.components"
    val message = s"Could not compute hypergraph components: $detail"
  }
  final case class DefinitionCoverCountMismatch(
      expected: BigInt,
      actual: BigInt
  ) extends CandidateInputError {
    val code = "candidate.input.definition-covers"
    val message = s"Definition-cover count disagrees with width bound (expected=$expected, actual=$actual)"
  }
  final case class InvalidDefinitionCover(
      edgeIds: Vector[EdgeId],
      reason: String
  ) extends CandidateInputError {
    val code = "candidate.input.definition-cover.invalid"
    val message = s"Invalid definition cover ${edgeIds.mkString("[", ",", "]")}: $reason"
  }
  final case class NonCanonicalDefinitionCovers() extends CandidateInputError {
    val code = "candidate.input.definition-covers.order"
    val message = "Definition covers are not in canonical order or contain duplicates"
  }
  final case class NullCandidate(index: Int) extends CandidateInputError {
    val code = "candidate.input.candidate.null"
    val message = s"Candidate $index is null"
  }
  final case class NonCanonicalCandidateOrder() extends CandidateInputError {
    val code = "candidate.input.candidate.order"
    val message = "Candidate bags are not in canonical vertex order"
  }
  final case class NonCanonicalBagVertices(
      candidateIndex: Int,
      vertices: Vector[VariableId]
  ) extends CandidateInputError {
    val code = "candidate.input.candidate.vertices"
    val message = s"Candidate $candidateIndex has non-canonical or duplicate vertices"
  }
  final case class UnknownBagVertices(
      candidateIndex: Int,
      vertices: Vector[VariableId]
  ) extends CandidateInputError {
    val code = "candidate.input.candidate.vertices.unknown"
    val message = s"Candidate $candidateIndex contains unknown vertices ${vertices.mkString("[", ",", "]")}"
  }
  final case class DuplicateCandidateBag(vertices: Vector[VariableId])
      extends CandidateInputError {
    val code = "candidate.input.candidate.duplicate"
    val message = s"Duplicate candidate bag ${vertices.mkString("[", ",", "]")}"
  }
  final case class MissingGenerationWitness(vertices: Vector[VariableId])
      extends CandidateInputError {
    val code = "candidate.input.candidate.witness"
    val message = s"Candidate ${vertices.mkString("[", ",", "]")} has no valid generation witness"
  }
  final case class MissingPhysicalCovers(vertices: Vector[VariableId])
      extends CandidateInputError {
    val code = "candidate.input.cover.empty"
    val message = s"Candidate ${vertices.mkString("[", ",", "]")} has no physical cover"
  }
  final case class NullPhysicalCover(
      vertices: Vector[VariableId],
      coverIndex: Int
  ) extends CandidateInputError {
    val code = "candidate.input.cover.null"
    val message = s"Physical cover $coverIndex for candidate ${vertices.mkString("[", ",", "]")} is null"
  }
  final case class NonCanonicalPhysicalCovers(vertices: Vector[VariableId])
      extends CandidateInputError {
    val code = "candidate.input.cover.order"
    val message = s"Physical covers for candidate ${vertices.mkString("[", ",", "]")} are not canonical or contain duplicates"
  }
  final case class InvalidPhysicalCover(
      vertices: Vector[VariableId],
      edgeIds: Vector[EdgeId],
      reason: String
  ) extends CandidateInputError {
    val code = "candidate.input.cover.invalid"
    val message = s"Invalid physical cover ${edgeIds.mkString("[", ",", "]")} for candidate ${vertices.mkString("[", ",", "]")}: $reason"
  }
  final case class PhysicalCoverNotDefined(
      vertices: Vector[VariableId],
      edgeIds: Vector[EdgeId]
  ) extends CandidateInputError {
    val code = "candidate.input.cover.undefined"
    val message = s"Physical cover ${edgeIds.mkString("[", ",", "]")} is absent from the definition-cover universe"
  }
  final case class BagNotCovered(
      vertices: Vector[VariableId],
      edgeIds: Vector[EdgeId]
  ) extends CandidateInputError {
    val code = "candidate.input.cover.does-not-cover"
    val message = s"Physical cover ${edgeIds.mkString("[", ",", "]")} does not cover candidate ${vertices.mkString("[", ",", "]")}"
  }
  final case class ConnectivityMetadataMismatch(
      vertices: Vector[VariableId],
      edgeIds: Vector[EdgeId],
      supplied: Boolean,
      computed: Boolean
  ) extends CandidateInputError {
    val code = "candidate.input.cover.connectivity-metadata"
    val message = s"Connectivity metadata for cover ${edgeIds.mkString("[", ",", "]")} is $supplied, computed $computed"
  }
  final case class DisconnectedCoverInConnectedMode(
      vertices: Vector[VariableId],
      edgeIds: Vector[EdgeId]
  ) extends CandidateInputError {
    val code = "candidate.input.cover.connected-required"
    val message = s"Disconnected cover ${edgeIds.mkString("[", ",", "]")} retained in connected-cover mode"
  }

  def canonical(errors: Iterable[CandidateInputError]): Vector[CandidateInputError] =
    errors.iterator.toVector.distinct.sortBy(error => (error.code, error.message))
}

sealed trait CandidateFailure extends Product with Serializable {
  def code: String
  def message: String
}

object CandidateFailure {
  final case class InvalidPlanLimit(value: Int) extends CandidateFailure {
    val code = "candidate.failure.plan-limit.invalid"
    val message = s"Candidate plan limit must be positive, but was $value"
  }
  final case class InvalidResourceLimit(
      resource: CandidateResource,
      value: Long
  ) extends CandidateFailure {
    val code = "candidate.failure.resource-limit.invalid"
    val message = s"${resource.code} limit must be positive, but was $value"
  }
  final case class ResourceLimitExceeded(
      resource: CandidateResource,
      required: BigInt,
      limit: Long
  ) extends CandidateFailure {
    val code = "candidate.failure.resource-limit.exceeded"
    val message = s"${resource.code} requires $required items, exceeding limit $limit"
  }
  final case class MissingValue(path: String) extends CandidateFailure {
    val code = "candidate.failure.missing"
    val message = s"Missing $path"
  }
  final case class UnknownSeparatorVertices(vertices: Vector[VariableId])
      extends CandidateFailure {
    val code = "candidate.failure.separator.unknown"
    val message = s"Separator contains unknown vertices ${vertices.mkString("[", ",", "]")}"
  }
  final case class RankInvariantViolation(
      parent: CandidateBlock,
      child: CandidateBlock
  ) extends CandidateFailure {
    val code = "candidate.failure.rank"
    val message = s"Recurrence child $child does not have lower rank than $parent"
  }
  final case class RootRankInvariantViolation(
      rootTail: Vector[VariableId],
      child: CandidateBlock
  ) extends CandidateFailure {
    val code = "candidate.failure.rank.root"
    val message = s"Recurrence child $child does not have lower rank than virtual root (${rootTail.size},0)"
  }
  final case class MissingBlockWitness(block: CandidateBlock)
      extends CandidateFailure {
    val code = "candidate.failure.witness"
    val message = s"Missing accepted witness for subordinate block $block"
  }
  final case class MissingPhysicalCandidate(bag: CandidateBagKey)
      extends CandidateFailure {
    val code = "candidate.failure.physical.candidate"
    val message = s"Structural witness contains unknown candidate bag $bag"
  }
  final case class MissingPhysicalCover(bag: CandidateBagKey)
      extends CandidateFailure {
    val code = "candidate.failure.physical.cover"
    val message = s"Structural witness candidate $bag has no physical cover"
  }
  final case class CoreModelConstruction(
      stage: String,
      errors: Vector[String]
  ) extends CandidateFailure {
    val code = "candidate.failure.core-model"
    val message = s"Could not construct $stage: ${errors.mkString("; ")}"
  }
  final case class CertificateValidation(
      errors: Vector[CandidateCertificateError]
  ) extends CandidateFailure {
    val code = "candidate.failure.certificate"
    val message = s"Physicalized witness failed candidate validation (${errors.size} errors)"
  }
}

sealed trait CandidateCertificateError extends Product with Serializable {
  def code: String
  def message: String
}

object CandidateCertificateError {
  final case class MissingValue(path: String) extends CandidateCertificateError {
    val code = "candidate.certificate.missing"
    val message = s"Missing $path"
  }
  final case class CoreDecompositionViolation(
      error: DecompositionValidationError
  ) extends CandidateCertificateError {
    val code = "candidate.certificate.core"
    val message = error.toString
  }
  final case class BagNotCandidate(
      bagId: BagId,
      vertices: Vector[VariableId]
  ) extends CandidateCertificateError {
    val code = "candidate.certificate.bag"
    val message = s"Bag ${bagId.value} is not an admissible structural candidate"
  }
  final case class CoverNotAdmissible(
      bagId: BagId,
      edgeIds: Vector[EdgeId]
  ) extends CandidateCertificateError {
    val code = "candidate.certificate.cover"
    val message = s"Cover ${edgeIds.mkString("[", ",", "]")} is not admissible for bag ${bagId.value}"
  }
  final case class ConnectedCoverViolation(
      bagId: BagId,
      edgeIds: Vector[EdgeId]
  ) extends CandidateCertificateError {
    val code = "candidate.certificate.cover.connected"
    val message = s"Bag ${bagId.value} uses a disconnected cover in connected-cover mode"
  }
  final case class CoverPredicateFailure(
      bagId: BagId,
      detail: String
  ) extends CandidateCertificateError {
    val code = "candidate.certificate.cover.predicate"
    val message = s"Could not verify cover of bag ${bagId.value}: $detail"
  }
  final case class EmptyRootBag(root: BagId)
      extends CandidateCertificateError {
    val code = "candidate.certificate.root.empty"
    val message = s"Actual decomposition root ${root.value} must not be an empty candidate bag"
  }
  final case class EqualAdjacentBags(parent: BagId, child: BagId)
      extends CandidateCertificateError {
    val code = "candidate.certificate.normal-form.equal-bags"
    val message = s"Adjacent bags ${parent.value} and ${child.value} have equal structural vertices"
  }
  final case class ComponentNormalFormViolation(
      parent: BagId,
      child: BagId,
      subtreeVertices: Vector[VariableId],
      matchingComponents: Vector[Vector[VariableId]]
  ) extends CandidateCertificateError {
    val code = "candidate.certificate.normal-form"
    val message = s"Arc ${parent.value}->${child.value} has ${matchingComponents.size} matching parent-separator components"
  }
  final case class ComponentPredicateFailure(
      parent: BagId,
      child: BagId,
      detail: String
  ) extends CandidateCertificateError {
    val code = "candidate.certificate.normal-form.predicate"
    val message = s"Could not check component normal form on ${parent.value}->${child.value}: $detail"
  }

  def canonical(
      errors: Iterable[CandidateCertificateError]
  ): Vector[CandidateCertificateError] =
    errors.iterator.toVector.distinct.sortBy(error => (error.code, error.message))
}
