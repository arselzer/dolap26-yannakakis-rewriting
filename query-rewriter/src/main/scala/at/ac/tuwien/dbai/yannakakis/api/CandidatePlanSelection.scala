package at.ac.tuwien.dbai.yannakakis.api

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId

/** Declared policy for choosing from the exact retained structural top-K set. */
sealed trait CandidatePlanSelection extends Product with Serializable {
  def id: String
}

object CandidatePlanSelection {
  /** Existing behavior: select structural rank one. */
  case object StructuralHead extends CandidatePlanSelection {
    override val id: String = "structural_top_k_head_v1"
  }

  /** Select one exact retained structural rank without fallback. */
  final case class StructuralRank(rank: Int) extends CandidatePlanSelection {
    require(rank > 0, "candidate structural rank must be positive")
    override val id: String = "explicit_structural_rank_v1"
  }

  /** Select one exact retained physical decomposition identity. */
  final case class ExactCandidateId(candidateId: CandidatePlanId)
      extends CandidatePlanSelection {
    require(candidateId != null, "candidate plan id must not be null")
    override val id: String = "explicit_candidate_id_v1"
  }

  /**
    * Deterministic PostgreSQL emitted-shape heuristic, not a DBMS cost model.
    *
    * It ranks every retained candidate lexicographically by expanded scans,
    * materialized DISTINCT support count, CTEs, rendered terms, SQL characters,
    * and finally structural rank.
    */
  case object PostgreSqlStaticShapeV1 extends CandidatePlanSelection {
    override val id: String = "postgresql_static_shape_lexicographic_v1"
  }

  /**
    * Select by guarded PostgreSQL root-plan estimates without executing SQL.
    *
    * The original statement is planned first as a same-session root-row
    * baseline. Every retained structural candidate is then rendered under the
    * one requested reduction mode and planned with EXPLAIN JSON without
    * ANALYZE. Only candidates whose root Plan Rows are within the inclusive
    * 10x baseline guard are admissible; if none are, selection fails closed.
    * Admissible candidates use the stable total-cost, structural-rank, and
    * candidate-id tie order. This policy never compares reduction modes.
    */
  case object PostgreSqlExplainGuardedTotalCostV1
      extends CandidatePlanSelection {
    override val id: String =
      "postgresql_explain_row_guarded_total_cost_v1"
  }

  /**
    * Select from the exact structural K=100 prefix using PostgreSQL estimates
    * for every renderer-owned physical stage.
    *
    * The policy verifies independently enumerated K=10/25/50/100 prefixes,
    * evaluates every retained candidate through K=100 (or exact search
    * exhaustion), preserves the guarded final-row baseline, and orders
    * admissible candidates by peak named-stage estimated bytes, cumulative
    * named-stage estimated bytes, final total cost, structural rank, and
    * candidate id. It is deliberately opt-in and never compares reduction
    * modes.
    */
  case object PostgreSqlExplainFactorAwareProgressiveV1
      extends CandidatePlanSelection {
    override val id: String =
      "postgresql_explain_factor_aware_progressive_v1"
  }

  /**
    * Experimental COUNT(*)-only calibration of the progressive stage probes.
    *
    * This policy retains the v5 K=100 structural and probe envelope, but uses
    * the exact sum of named-stage PostgreSQL plan costs as its leading key.
    * It is deliberately a separate opt-in artifact contract: it applies only
    * to a certified global COUNT(*) under FullTwoPass and does not widen the
    * published v5 policy.
    */
  case object PostgreSqlExplainCountStageCostProgressiveV1
      extends CandidatePlanSelection {
    override val id: String =
      "postgresql_explain_count_stage_cost_progressive_v1"
  }

  /**
    * Select from a bounded deterministic portfolio of PostgreSQL physical
    * orientations, admissible covers, reduction modes, and CTE directives.
    * The certified structural top-K identities remain unchanged.
    */
  case object PostgreSqlExplainFactorAwarePhysicalPortfolioV1
      extends CandidatePlanSelection {
    override val id: String =
      "postgresql_explain_factor_aware_physical_portfolio_v1"
  }

  val Default: CandidatePlanSelection = StructuralHead
}
