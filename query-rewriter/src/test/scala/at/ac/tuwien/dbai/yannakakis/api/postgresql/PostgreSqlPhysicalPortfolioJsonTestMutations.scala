package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision
import at.ac.tuwien.dbai.yannakakis.api.RewriteOutcome

/** Test-only bridge for hostile v6 serializer inputs.
  *
  * Production result/report constructors deliberately remain scoped to the
  * PostgreSQL API package. Keeping these copies here lets the CLI contract
  * suite exercise serializer fail-closed behavior without widening them.
  */
object PostgreSqlPhysicalPortfolioJsonTestMutations {
  def withOutcome(
      result: PostgreSqlRewriteResult,
      outcome: RewriteOutcome
  ): PostgreSqlRewriteResult =
    result.copy(outcome = outcome)

  def withoutAttemptAt(
      result: PostgreSqlRewriteResult,
      completedPhase: PostgreSqlRewritePhase,
      candidateDecision: Option[CandidatePlanDecision] = None
  ): PostgreSqlRewriteResult =
    result.copy(
      report = result.report.copy(
        completedPhase = completedPhase,
        candidateDecision = candidateDecision,
        physicalPortfolioAttempt = None,
        physicalPortfolioArtifact = None
      )
    )

  def withoutAttemptButRetainingFound(
      result: PostgreSqlRewriteResult
  ): PostgreSqlRewriteResult = {
    require(
      result.report.candidateDecision.exists(
        _.isInstanceOf[
          at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision.Found
        ]
      ),
      "hostile copy requires a found candidate search"
    )
    result.copy(
      report = result.report.copy(
        physicalPortfolioAttempt = None,
        physicalPortfolioArtifact = None
      )
    )
  }

  /** Builds a replay-valid evidence-prefix terminal state from a selected run. */
  def asEvidenceInvalid(
      result: PostgreSqlRewriteResult
  ): PostgreSqlRewriteResult = {
    val attempt = result.report.physicalPortfolioAttempt.getOrElse(
      throw new IllegalArgumentException("selected fixture has no attempt")
    )
    val selected = attempt.decision match {
      case value: PostgreSqlPhysicalPortfolioDecision.Selected => value
      case other =>
        throw new IllegalArgumentException(
          s"evidence fixture requires selected decision, received $other"
        )
    }
    val failed = PostgreSqlPhysicalPortfolioDecision.Failed(
      PostgreSqlPhysicalPortfolioSelectionFailureKind.EvidenceInvalid,
      PostgreSqlPhysicalPortfolioSelectionFailureKind.EvidenceInvalid.id,
      None,
      Vector.empty,
      Vector.empty,
      None,
      Some(selected.enumeration),
      selected.rawRenderings,
      Some(selected.deduplication),
      Some(selected.explainRequest),
      selected.evaluationLimits,
      Some(selected.evaluationUsage),
      None,
      None,
      Some(selected.explainBatch),
      Vector.empty
    )
    val failedAttempt = attempt.copy(decision = failed)
    val report = result.report.copy(
      completedPhase = PostgreSqlRewritePhase.CandidateSelectionCompleted,
      physicalPortfolioAttempt = Some(failedAttempt),
      physicalPortfolioArtifact = None
    )
    result.copy(
      outcome = PostgreSqlPhysicalPortfolioOutcome.build(
        attempt.sourceSql,
        failed,
        None
      ),
      report = report
    )
  }
}
