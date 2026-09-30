package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition

/** Structural ranking only; no DBMS or physical-cover cost is implied. */
final case class StructuralPlanScore(nodeCount: BigInt) extends Serializable

/**
  * One structurally ranked CandidateTD with a deterministic physical witness.
  *
  * The decomposition uses the first canonical admissible cover for every bag.
  * Cover assignments are not separately enumerated or cost-ranked here.
  */
final case class RankedCandidatePlan(
    rank: Int,
    candidateId: CandidatePlanId,
    score: StructuralPlanScore,
    witness: CandidateTree,
    decomposition: CoveredDecomposition,
    certificate: CandidateCertificate
) extends Serializable

/** Exact accounting for one top-K enumeration call. */
final case class CandidateTopKStatistics(
    base: CandidateStatistics,
    retainedPartialPlanCount: Long,
    combinationStateCount: Long
) extends Serializable

sealed trait CandidatePlanDecision extends Product with Serializable

object CandidatePlanDecision {
  final case class Found(
      plans: Vector[RankedCandidatePlan],
      requestedLimit: Int,
      statistics: CandidateTopKStatistics
  ) extends CandidatePlanDecision

  final case class Rejected(
      requestedLimit: Int,
      statistics: CandidateTopKStatistics
  ) extends CandidatePlanDecision
}
