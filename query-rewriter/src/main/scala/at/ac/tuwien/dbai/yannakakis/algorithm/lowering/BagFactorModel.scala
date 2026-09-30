package at.ac.tuwien.dbai.yannakakis.algorithm.lowering

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateCertificate
import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  EdgeId,
  QueryHypergraph,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition

/**
  * One logical bag factor.
  *
  * `supportCover` is set-valued support/projection metadata. It is independent
  * of `ownedEdgeIds` and must never be interpreted as a multiplicity carrier.
  */
final case class BagFactor private[lowering] (
    bagId: BagId,
    vertices: Vector[VariableId],
    supportCover: Vector[EdgeId],
    ownedEdgeIds: Vector[EdgeId],
    depth: Int,
    parent: Option[BagId],
    children: Vector[BagId]
) extends Serializable

final case class EdgeOwnership private[lowering] (
    edgeId: EdgeId,
    owner: BagId,
    ownerDepth: Int
) extends Serializable

final case class OrientedBagArc private[lowering] (
    parent: BagId,
    child: BagId,
    separator: Vector[VariableId]
) extends Serializable

sealed trait SemijoinPass extends Product with Serializable {
  def code: String
}

object SemijoinPass {
  case object BottomUp extends SemijoinPass {
    val code = "bottom-up"
  }
  case object TopDown extends SemijoinPass {
    val code = "top-down"
  }
}

/**
  * A logical multiplicity-preserving semijoin step.
  *
  * `target` is filtered by an EXISTS-style equijoin against `source` on every
  * separator variable. A later SQL lowering must use ordinary equality for
  * those keys, not an ordinary join and not null-safe comparison.
  */
final case class SemijoinStep private[lowering] (
    ordinal: Int,
    pass: SemijoinPass,
    target: BagId,
    source: BagId,
    separator: Vector[VariableId]
) extends Serializable

final case class YannakakisSchedule private[lowering] (
    bottomUp: Vector[SemijoinStep],
    topDown: Vector[SemijoinStep]
) extends Serializable

final case class BagFactorPlan private[lowering] (
    sourceHypergraph: QueryHypergraph,
    sourceWidthBound: Int,
    requireConnectedCover: Boolean,
    decomposition: CoveredDecomposition,
    certificate: CandidateCertificate,
    root: BagId,
    factors: Vector[BagFactor],
    ownership: Vector[EdgeOwnership],
    arcs: Vector[OrientedBagArc],
    schedule: YannakakisSchedule
) extends Serializable
