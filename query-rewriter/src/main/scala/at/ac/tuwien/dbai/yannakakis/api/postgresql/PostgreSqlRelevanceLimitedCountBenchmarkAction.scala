package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlRelevanceLimitedBaseTableEvidence,
  PostgreSqlRelevanceLimitedCountFailure,
  PostgreSqlRelevanceLimitedCountFailureClass,
  PostgreSqlRelevanceLimitedCountProfile,
  PostgreSqlRelevanceLimitedCountRenderedQuery,
  PostgreSqlRelevanceLimitedCountRenderer,
  PostgreSqlRenderingLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryResultShape

/** SQL-bearing identity for the additive query-factorized V4 action. */
private[yannakakis] final class PostgreSqlRelevanceLimitedCountActionId private (
    val value: String
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlRelevanceLimitedCountActionId => value == that.value
    case _                                             => false
  }
  override def hashCode(): Int = value.hashCode
  override def toString: String = value
}

private[yannakakis] object PostgreSqlRelevanceLimitedCountActionId {
  val Scheme: String = "postgresql_relevance_limited_count_action_v1"
  val RenderedStatementDigestScheme: String =
    "postgresql_rendered_statement_utf8_sha256_v1"

  def from(
      candidateId: CandidatePlanId,
      baseTableEvidenceId: String,
      renderedStatementSha256: String
  ): PostgreSqlRelevanceLimitedCountActionId = {
    require(candidateId != null, "candidate ID must not be null")
    require(
      baseTableEvidenceId != null &&
        baseTableEvidenceId.matches("sha256:[0-9a-f]{64}"),
      "base-table evidence ID must be SHA-256"
    )
    require(
      renderedStatementSha256 != null &&
        renderedStatementSha256.matches("[0-9a-f]{64}"),
      "rendered statement digest must be lowercase SHA-256"
    )
    val preimage = Vector(
      Scheme,
      PostgreSqlRelevanceLimitedCountProfile.Scheme,
      PostgreSqlRelevanceLimitedCountProfile.Id
    ).iterator
      .concat(PostgreSqlRelevanceLimitedCountProfile.AxisIds.iterator)
      .concat(
        Iterator(
          CandidatePlanId.Scheme,
          candidateId.value,
          at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql
            .PostgreSqlRelevanceLimitedBaseTableEvidence.Scheme,
          baseTableEvidenceId,
          PostgreSqlRelevanceLimitedCountProfile.ReductionMode.id,
          PostgreSqlRelevanceLimitedCountProfile.CteMaterializationScope,
          PostgreSqlRelevanceLimitedCountProfile.CteMaterialization.id,
          RenderedStatementDigestScheme,
          renderedStatementSha256
        )
      )
      .map(lengthPrefix)
      .mkString
    new PostgreSqlRelevanceLimitedCountActionId(
      "sha256:" + sha256(preimage)
    )
  }

  private def lengthPrefix(value: String): String = {
    val size = value.getBytes(StandardCharsets.UTF_8).length
    s"$size:$value"
  }

  private def sha256(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}

private[yannakakis] final case class PostgreSqlRelevanceLimitedCountAction(
    aliasId: String,
    structuralRank: Int,
    candidateId: CandidatePlanId,
    factorCount: Int,
    baseTableEvidenceId: String,
    actionId: PostgreSqlRelevanceLimitedCountActionId,
    renderedStatementSha256: String,
    renderedQuery: PostgreSqlRelevanceLimitedCountRenderedQuery
) extends Serializable {
  require(aliasId != null && aliasId.matches("[a-z][a-z0-9_-]{0,127}"))
  require(structuralRank > 0)
  require(candidateId != null && factorCount > 0)
  require(baseTableEvidenceId.matches("sha256:[0-9a-f]{64}"))
  require(actionId != null)
  require(renderedStatementSha256.matches("[0-9a-f]{64}"))
  require(renderedQuery != null)
  require(
    candidateId == CandidatePlanId.from(renderedQuery.provenancePlan.decomposition),
    "V4 action candidate must match its provenance decomposition"
  )
  require(
    factorCount == renderedQuery.provenancePlan.factors.size,
    "V4 action factor count must match its provenance plan"
  )
  require(
    baseTableEvidenceId == renderedQuery.trace.baseTableEvidenceId,
    "V4 action must bind the catalog evidence used by rendering"
  )
  require(
    renderedStatementSha256 ==
      PostgreSqlRelevanceLimitedCountBenchmarkAction.sha256(renderedQuery.sql),
    "V4 action digest must match its SQL"
  )
  require(
    actionId == PostgreSqlRelevanceLimitedCountActionId.from(
      candidateId,
      baseTableEvidenceId,
      renderedStatementSha256
    ),
    "V4 action ID must bind the exact candidate, evidence, profile, and SQL"
  )

  val realizationId: String =
    PostgreSqlRelevanceLimitedCountProfile.RealizationId
  val profileId: String = PostgreSqlRelevanceLimitedCountProfile.Id
  val reductionMode = PostgreSqlRelevanceLimitedCountProfile.ReductionMode
  val cteMaterializationDirective =
    PostgreSqlRelevanceLimitedCountProfile.CteMaterialization
  val trainingEligible: Boolean = false
  val reportable: Boolean = false
}

private[yannakakis] sealed trait PostgreSqlRelevanceLimitedCountActionDisposition
    extends Product
    with Serializable

private[yannakakis] object PostgreSqlRelevanceLimitedCountActionDisposition {
  final case class Rendered(action: PostgreSqlRelevanceLimitedCountAction)
      extends PostgreSqlRelevanceLimitedCountActionDisposition {
    require(action != null)
  }

  final case class Skipped(
      structuralRank: Int,
      candidateId: CandidatePlanId,
      failure: PostgreSqlRelevanceLimitedCountFailure
  ) extends PostgreSqlRelevanceLimitedCountActionDisposition {
    require(structuralRank > 0 && candidateId != null && failure != null)
    require(
      failure.failureClass ==
        PostgreSqlRelevanceLimitedCountFailureClass.NotApplicable
    )
  }
}

/**
  * Selected-plan producer for the benchmark-only V4 action.
  *
  * A semantic non-match is an explicit skipped disposition.  Invalid input,
  * resource exhaustion, and rendering faults remain failures and are never
  * relabelled as non-applicability.
  */
private[yannakakis] object PostgreSqlRelevanceLimitedCountBenchmarkAction {
  val ProtocolId: String = "postgresql-relevance-limited-count-action/v1"

  def produceSelected(
      artifact: PostgreSqlRewriteArtifact,
      baseTableEvidence: PostgreSqlRelevanceLimitedBaseTableEvidence,
      renderingLimits: PostgreSqlRenderingLimits =
        PostgreSqlRenderingLimits.default,
      planningLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default
  ): Either[
    PostgreSqlRelevanceLimitedCountFailure,
    PostgreSqlRelevanceLimitedCountActionDisposition
  ] = {
    if (artifact == null)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "artifact",
          "rewrite artifact is missing"
        )
      )
    if (!artifact.query.resultShape.isInstanceOf[QueryResultShape.GlobalCountStar])
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.notApplicable(
          "result-shape",
          s"expected global-count-star, found ${artifact.query.resultShape.id}"
        )
      )
    val candidate = artifact.selectedPlan
    if (candidate.rank != 1)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "provenance-rank",
          "V4 uses exactly the canonical structural-head candidate as provenance"
        )
      )
    val expectedCandidateId = CandidatePlanId.from(candidate.decomposition)
    if (candidate.candidateId != expectedCandidateId)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "candidate",
          "selected candidate identity does not match its decomposition"
        )
      )
    if (artifact.bagFactorPlan.decomposition != candidate.decomposition)
      return Left(
        PostgreSqlRelevanceLimitedCountFailure.invalid(
          "plan",
          "selected bag-factor plan does not match the selected candidate"
        )
      )

    PostgreSqlRelevanceLimitedCountRenderer
      .render(
        artifact.query,
        artifact.candidateInput,
        artifact.bagFactorPlan,
        baseTableEvidence,
        renderingLimits,
        planningLimits
      ) match {
      case Right(rendered) =>
        val digest = sha256(rendered.sql)
        val actionId = PostgreSqlRelevanceLimitedCountActionId.from(
          candidate.candidateId,
          baseTableEvidence.id,
          digest
        )
        Right(PostgreSqlRelevanceLimitedCountActionDisposition.Rendered(
          PostgreSqlRelevanceLimitedCountAction(
            f"global-count-r${candidate.rank}%02d-shared-v4-mat",
            candidate.rank,
            candidate.candidateId,
            artifact.bagFactorPlan.factors.size,
            baseTableEvidence.id,
            actionId,
            digest,
            rendered
          )
        ))
      case Left(failure)
          if failure.failureClass ==
            PostgreSqlRelevanceLimitedCountFailureClass.NotApplicable =>
        Right(
          PostgreSqlRelevanceLimitedCountActionDisposition.Skipped(
            candidate.rank,
            candidate.candidateId,
            failure
          )
        )
      case Left(failure) => Left(failure)
    }
  }

  private[postgresql] def sha256(sql: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(sql.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
