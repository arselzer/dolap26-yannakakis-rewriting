package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.core.EdgeId

/** A stable category for a bounded soft-bag computation. */
sealed trait SoftResource extends Product with Serializable {
  def code: String
}

object SoftResource {
  case object Covers extends SoftResource {
    override val code: String = "covers"
  }

  case object CoverPairs extends SoftResource {
    override val code: String = "cover-pairs"
  }

  case object Components extends SoftResource {
    override val code: String = "components"
  }

  case object Derivations extends SoftResource {
    override val code: String = "derivations"
  }

  case object Candidates extends SoftResource {
    override val code: String = "candidates"
  }

  case object CandidateCoverChecks extends SoftResource {
    override val code: String = "candidate-cover-checks"
  }

  val values: Vector[SoftResource] = Vector(
    Covers,
    CoverPairs,
    Components,
    Derivations,
    Candidates,
    CandidateCoverChecks
  )
}

/**
  * A deterministic, data-only failure from the pure soft-bag algorithms.
  *
  * Resource exhaustion is represented explicitly rather than by truncating a
  * candidate set: truncation would make a negative decomposition result
  * unsound.
  */
sealed trait SoftFailure extends Product with Serializable {
  def code: String
  def message: String
}

object SoftFailure {
  final case class MissingInput(input: String) extends SoftFailure {
    override val code: String = "missing-input"
    override val message: String = s"$input must be provided"
  }

  final case class InvalidWidthBound(value: Int) extends SoftFailure {
    override val code: String = "invalid-width-bound"
    override val message: String =
      s"width bound must be at least 1, but was $value"
  }

  final case class InvalidResourceLimit(
      resource: SoftResource,
      value: Long
  ) extends SoftFailure {
    override val code: String = "invalid-resource-limit"
    override val message: String =
      s"${resource.code} limit must be positive, but was $value"
  }

  case object NullEdgeIdInCover extends SoftFailure {
    override val code: String = "null-edge-id-in-cover"
    override val message: String = "an edge cover must not contain a null edge id"
  }

  case object NullVertexInBag extends SoftFailure {
    override val code: String = "null-vertex-in-bag"
    override val message: String = "bag vertices must not contain a null vertex id"
  }

  final case class DuplicateEdgeIdsInCover(edgeIds: Vector[EdgeId])
      extends SoftFailure {
    override val code: String = "duplicate-edge-ids-in-cover"
    override val message: String =
      s"an edge cover is a set; duplicate ids: ${edgeIds.mkString(",")}"
  }

  final case class UnknownEdgeIds(edgeIds: Vector[EdgeId]) extends SoftFailure {
    override val code: String = "unknown-edge-ids"
    override val message: String =
      s"edge ids do not occur in the hypergraph: ${edgeIds.mkString(",")}"
  }

  final case class ResourceLimitExceeded(
      resource: SoftResource,
      required: BigInt,
      limit: Long
  ) extends SoftFailure {
    override val code: String = "resource-limit-exceeded"
    override val message: String =
      s"${resource.code} requires $required items, exceeding limit $limit"
  }
}
