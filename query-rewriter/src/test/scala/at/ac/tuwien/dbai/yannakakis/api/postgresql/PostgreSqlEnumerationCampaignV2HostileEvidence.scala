package at.ac.tuwien.dbai.yannakakis.api.postgresql

/** Test-only corruptions that cross the package-private report boundary. */
private[yannakakis] object PostgreSqlEnumerationCampaignV2HostileEvidence {
  def withAnalysis(
      target: PostgreSqlEnumerationBatchResult,
      donor: PostgreSqlEnumerationBatchResult
  ): PostgreSqlEnumerationBatchResult =
    withReport(
      target,
      target.pipelineEvidence.copy(
        analysis = donor.pipelineEvidence.analysis
      )
    )

  def withHypergraph(
      target: PostgreSqlEnumerationBatchResult,
      donor: PostgreSqlEnumerationBatchResult
  ): PostgreSqlEnumerationBatchResult =
    withReport(
      target,
      target.pipelineEvidence.copy(
        hypergraph = donor.pipelineEvidence.hypergraph
      )
    )

  def withSoftGeneration(
      target: PostgreSqlEnumerationBatchResult,
      donor: PostgreSqlEnumerationBatchResult
  ): PostgreSqlEnumerationBatchResult =
    withReport(
      target,
      target.pipelineEvidence.copy(
        softGeneration = donor.pipelineEvidence.softGeneration
      )
    )

  def withAdmissibleGeneration(
      target: PostgreSqlEnumerationBatchResult,
      donor: PostgreSqlEnumerationBatchResult
  ): PostgreSqlEnumerationBatchResult =
    withReport(
      target,
      target.pipelineEvidence.copy(
        admissibleGeneration = donor.pipelineEvidence.admissibleGeneration
      )
    )

  def withCandidateInput(
      target: PostgreSqlEnumerationBatchResult,
      donor: PostgreSqlEnumerationBatchResult
  ): PostgreSqlEnumerationBatchResult =
    withReport(
      target,
      target.pipelineEvidence.copy(
        candidateInput = donor.pipelineEvidence.candidateInput
      )
    )

  private def withReport(
      target: PostgreSqlEnumerationBatchResult,
      report: PostgreSqlRewriteReport
  ): PostgreSqlEnumerationBatchResult =
    target.copy(pipelineEvidence = report)
}
