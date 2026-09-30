package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{
  RewriteDiagnostic,
  RewriteOutcome,
  RewriteReasonCode
}

import scala.util.control.NonFatal

/** Exact public outcome projection for one terminal portfolio decision. */
private[yannakakis] object PostgreSqlPhysicalPortfolioOutcome {
  def build(
      originalSql: String,
      decision: PostgreSqlPhysicalPortfolioDecision,
      selectedSql: Option[String]
  ): RewriteOutcome = {
    require(originalSql != null)
    require(decision != null)
    require(selectedSql != null && selectedSql.forall(_ != null))
    decision match {
      case _: PostgreSqlPhysicalPortfolioDecision.Selected =>
        val sql = selectedSql.filter(_.nonEmpty).getOrElse(
          throw new IllegalArgumentException(
            "selected physical portfolio decision requires rendered SQL"
          )
        )
        RewriteOutcome.Rewritten(originalSql, Vector(sql))
      case _: PostgreSqlPhysicalPortfolioDecision.NoAdmissible =>
        require(
          selectedSql.isEmpty,
          "no-admissible physical portfolio decision cannot carry rendered SQL"
        )
        RewriteOutcome.Failed(
          originalSql,
          Vector(
            RewriteDiagnostic(
              RewriteReasonCode.CandidateSelectionFailed,
              "No physical portfolio workload passed the PostgreSQL row-estimate guard",
              context = Map(
                "stage" -> "candidate-selection",
                "algorithmCode" -> "physical-portfolio.no-admissible",
                "selectionPolicy" -> decision.policyId,
                "rowGuardFactor" ->
                  PostgreSqlExplainRowGuardEvidence.Factor.toString
              )
            )
          )
        )
      case failed: PostgreSqlPhysicalPortfolioDecision.Failed =>
        require(
          selectedSql.isEmpty,
          "failed physical portfolio decision cannot carry rendered SQL"
        )
        RewriteOutcome.Failed(
          originalSql,
          Vector(
            RewriteDiagnostic(
              RewriteReasonCode.CandidateSelectionFailed,
              "PostgreSQL physical portfolio selection failed",
              context = Map(
                "stage" -> "candidate-selection",
                "algorithmCode" -> failed.detailCode,
                "selectionPolicy" -> decision.policyId,
                "portfolioStage" -> failed.failureKind.stage.id,
                "failureKind" -> failed.failureKind.id
              )
            )
          )
        )
    }
  }

  def verify(
      originalSql: String,
      decision: PostgreSqlPhysicalPortfolioDecision,
      selectedSql: Option[String],
      outcome: RewriteOutcome
  ): Boolean =
    try outcome != null && outcome == build(originalSql, decision, selectedSql)
    catch { case NonFatal(_) => false }
}
