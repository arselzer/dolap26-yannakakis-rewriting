package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorCoverAssignment,
  BagFactorPhysicalConfiguration,
  BagFactorPlan,
  BagFactorPlanner,
  BagFactorPlanningLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlGlobalCountBenchmarkRealization,
  PostgreSqlGlobalCountBenchmarkRenderer,
  PostgreSqlGlobalCountRendererBinding,
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingLimits
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryResultShape

/** A SQL-bearing identity separate from every frozen physical-variant ID. */
private[yannakakis] final class PostgreSqlGlobalCountBenchmarkActionId private (
    val value: String
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlGlobalCountBenchmarkActionId => value == that.value
    case _                                            => false
  }

  override def hashCode(): Int = value.hashCode
  override def toString: String = value
}

private[yannakakis] object PostgreSqlGlobalCountBenchmarkActionId {
  val Scheme: String = "postgresql_global_count_benchmark_action_v1"
  val NativeRealizationId: String = "native-source"
  val RenderedStatementDigestScheme: String =
    "postgresql_rendered_statement_utf8_sha256_v1"

  def fromNativeSource(
      sourceStatementSha256: String
  ): PostgreSqlGlobalCountBenchmarkActionId = {
    require(
      sourceStatementSha256 != null &&
        sourceStatementSha256.matches("[0-9a-f]{64}"),
      "source statement digest must be lowercase SHA-256"
    )
    val preimage = Vector(
      Scheme,
      NativeRealizationId,
      RenderedStatementDigestScheme,
      sourceStatementSha256
    ).iterator.map(lengthPrefix).mkString
    new PostgreSqlGlobalCountBenchmarkActionId(
      "sha256:" + sha256(preimage)
    )
  }

  def from(
      realization: PostgreSqlGlobalCountBenchmarkRealization,
      candidateId: CandidatePlanId,
      basePhysicalVariantId: PostgreSqlPhysicalVariantId,
      renderer: PostgreSqlGlobalCountRendererBinding,
      renderedStatementSha256: String
  ): PostgreSqlGlobalCountBenchmarkActionId = {
    require(realization != null, "realization must not be null")
    require(candidateId != null, "candidate plan id must not be null")
    require(basePhysicalVariantId != null, "base physical id must not be null")
    require(renderer != null, "renderer binding must not be null")
    require(
      renderedStatementSha256 != null &&
        renderedStatementSha256.matches("[0-9a-f]{64}"),
      "rendered statement digest must be lowercase SHA-256"
    )
    new PostgreSqlGlobalCountBenchmarkActionId(
      "sha256:" + sha256(
        canonicalPreimage(
          realization,
          candidateId,
          basePhysicalVariantId,
          renderer,
          renderedStatementSha256
        )
      )
    )
  }

  private[yannakakis] def canonicalPreimage(
      realization: PostgreSqlGlobalCountBenchmarkRealization,
      candidateId: CandidatePlanId,
      basePhysicalVariantId: PostgreSqlPhysicalVariantId,
      renderer: PostgreSqlGlobalCountRendererBinding,
      renderedStatementSha256: String
  ): String = {
    require(realization != null && candidateId != null)
    require(basePhysicalVariantId != null && renderer != null)
    require(
      renderedStatementSha256 != null &&
        renderedStatementSha256.matches("[0-9a-f]{64}")
    )
    Vector(
      Scheme,
      realization.id,
      CandidatePlanId.Scheme,
      candidateId.value,
      PostgreSqlPhysicalVariantId.SchemeV2,
      basePhysicalVariantId.value,
      renderer.aggregateProfileV1Scheme,
      renderer.aggregateProfileV1Id
    ).iterator
      .concat(renderer.aggregateProfileV1AxisIds.iterator)
      .concat(
        Iterator(
          renderer.aggregateProfileV2Scheme,
          renderer.aggregateProfileV2Id
        )
      )
      .concat(renderer.aggregateProfileV2AxisIds.iterator)
      .concat(
        Iterator(
          renderer.aggregateProfileV3Scheme,
          renderer.aggregateProfileV3Id
        )
      )
      .concat(renderer.aggregateProfileV3AxisIds.iterator)
      .concat(
        Iterator(
          renderer.enumerationProfileScheme,
          renderer.enumerationProfileId,
          PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
          PostgreSqlCteMaterializationDirective.Scope,
          RenderedStatementDigestScheme,
          renderedStatementSha256
        )
      )
      .map(lengthPrefix)
      .mkString
  }

  private def lengthPrefix(value: String): String = {
    val size = value.getBytes(StandardCharsets.UTF_8).length
    s"$size:$value"
  }

  private def sha256(value: String): String = {
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
    bytes.map(byte => f"${byte & 0xff}%02x").mkString
  }
}

private[yannakakis] final case class PostgreSqlGlobalCountBenchmarkAction(
    ordinal: Int,
    aliasId: String,
    realization: PostgreSqlGlobalCountBenchmarkRealization,
    structuralRank: Int,
    candidateId: CandidatePlanId,
    factorCount: Int,
    configuration: BagFactorPhysicalConfiguration,
    reductionMode: YannakakisReductionMode,
    basePhysicalVariantId: PostgreSqlPhysicalVariantId,
    renderer: PostgreSqlGlobalCountRendererBinding,
    actionId: PostgreSqlGlobalCountBenchmarkActionId,
    renderedStatementSha256: String,
    renderedQuery: PostgreSqlRenderedQuery
) extends Serializable {
  require(ordinal > 0, "action ordinal must be positive")
  require(aliasId != null && aliasId.matches("[a-z][a-z0-9_-]{0,127}"))
  require(realization != null, "realization must not be null")
  require(structuralRank > 0, "structural rank must be positive")
  require(candidateId != null, "candidate id must not be null")
  require(factorCount > 0, "COUNT benchmark action must contain a factor")
  require(
    factorCount > 1 ||
      realization ==
        PostgreSqlGlobalCountBenchmarkRealization.FullReductionControl,
    "one-bag COUNT permits only the full-control action"
  )
  require(configuration != null, "configuration must not be null")
  require(reductionMode != null, "reduction mode must not be null")
  require(basePhysicalVariantId != null, "base physical id must not be null")
  require(renderer != null, "renderer binding must not be null")
  require(actionId != null, "action id must not be null")
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}")
  )
  require(renderedQuery != null && renderedQuery.sql.nonEmpty)
  require(
    candidateId ==
      CandidatePlanId.from(renderedQuery.bagFactorPlan.decomposition),
    "action candidate id must match its rendered decomposition"
  )
  require(
    factorCount == renderedQuery.bagFactorPlan.factors.size,
    "action factor count must match its rendered plan"
  )
  require(
    reductionMode ==
      PostgreSqlGlobalCountBenchmarkRenderer.reductionModeFor(realization),
    "action reduction mode must match its realization"
  )
  require(
    renderedQuery.trace.reduction.mode == reductionMode,
    "action reduction mode must match its rendered trace"
  )
  require(
    renderer == PostgreSqlGlobalCountBenchmarkRenderer.bindingFor(realization),
    "action renderer binding must match its realization"
  )
  require(
    configuration ==
      PostgreSqlGlobalCountBenchmarkActions.canonicalConfiguration(
        renderedQuery.bagFactorPlan
      ),
    "action configuration must match its rendered plan"
  )
  require(
    basePhysicalVariantId == PostgreSqlPhysicalVariantId.fromV2(
      candidateId,
      configuration,
      reductionMode,
      PostgreSqlGlobalCountBenchmarkRenderer.CteDirective
    ),
    "base physical id must match the action coordinates"
  )
  require(
    renderedStatementSha256 ==
      PostgreSqlGlobalCountBenchmarkActions.renderedStatementSha256(
        renderedQuery.sql
      ),
    "rendered statement digest must match SQL bytes"
  )
  require(
    actionId == PostgreSqlGlobalCountBenchmarkActionId.from(
      realization,
      candidateId,
      basePhysicalVariantId,
      renderer,
      renderedStatementSha256
    ),
    "action id must bind the exact physical, renderer, and SQL coordinates"
  )

  val cteMaterializationDirective =
    PostgreSqlGlobalCountBenchmarkRenderer.CteDirective
  val reductionOrder: String = PostgreSqlPhysicalVariantId.CanonicalReductionOrder
}

private[yannakakis] final case class PostgreSqlGlobalCountSkippedRealization(
    structuralRank: Int,
    candidateId: CandidatePlanId,
    factorCount: Int,
    realization: PostgreSqlGlobalCountBenchmarkRealization,
    reason: String
) extends Serializable {
  require(structuralRank > 0)
  require(candidateId != null)
  require(factorCount == 1)
  require(
    realization ==
      PostgreSqlGlobalCountBenchmarkRealization.FusedRawNoReduction
  )
  require(reason == PostgreSqlGlobalCountBenchmarkActions.OneBagReason)
}

private[yannakakis] final case class PostgreSqlGlobalCountBenchmarkResult(
    protocolId: String,
    resultShapeId: String,
    requestedRanks: Vector[Int],
    actions: Vector[PostgreSqlGlobalCountBenchmarkAction],
    skippedRealizations: Vector[PostgreSqlGlobalCountSkippedRealization]
) extends Serializable {
  require(protocolId == PostgreSqlGlobalCountBenchmarkActions.ProtocolId)
  require(resultShapeId == "global-count-star")
  require(requestedRanks.nonEmpty && requestedRanks == requestedRanks.sorted)
  require(requestedRanks.distinct.size == requestedRanks.size)
  require(actions.map(_.ordinal) == (1 to actions.size).toVector)
  require(actions.map(_.actionId).distinct.size == actions.size)
  require(
    (actions.map(_.structuralRank).distinct ++
      skippedRealizations.map(_.structuralRank)).distinct.sorted ==
      requestedRanks
  )
  requestedRanks.foreach { rank =>
    val dispositions =
      actions.filter(_.structuralRank == rank).map(_.realization) ++
        skippedRealizations
          .filter(_.structuralRank == rank)
          .map(_.realization)
    require(
      dispositions == PostgreSqlGlobalCountBenchmarkRealization.Values,
      s"rank $rank must dispose both registered COUNT realizations"
    )
  }
}

private[yannakakis] final case class PostgreSqlGlobalCountBenchmarkFailure(
    code: String,
    detail: String
) extends Serializable {
  require(code != null && code.nonEmpty)
  require(detail != null && detail.nonEmpty)
}

/**
  * Additive action producer for native/full-control/fused COUNT experiments.
  *
  * Native SQL remains the caller's exact source bytes. This producer emits the
  * same-candidate B/C pair for each explicitly requested retained rank, while
  * leaving all default rewrite paths and the frozen physical sweep unchanged.
  */
private[yannakakis] object PostgreSqlGlobalCountBenchmarkActions {
  val ProtocolId: String = "postgresql-global-count-benchmark-actions/v1"
  val SemanticExecutionLane: String =
    "scalar-global-count-frequency-propagation"
  val OneBagReason: String = "not-distinct-one-bag"
  val TrainingEligible: Boolean = false
  val Reportable: Boolean = false
  val ActionIdentityScheme: String =
    PostgreSqlGlobalCountBenchmarkActionId.Scheme

  def produce(
      artifact: PostgreSqlRewriteArtifact,
      requestedRanks: Vector[Int],
      planningLimits: BagFactorPlanningLimits = BagFactorPlanningLimits.default,
      renderingLimits: PostgreSqlRenderingLimits =
        PostgreSqlRenderingLimits.default
  ): Either[
    PostgreSqlGlobalCountBenchmarkFailure,
    PostgreSqlGlobalCountBenchmarkResult
  ] = {
    if (artifact == null)
      return failed("artifact.missing", "rewrite artifact is missing")
    if (requestedRanks == null || requestedRanks.isEmpty)
      return failed("ranks.missing", "at least one retained rank is required")
    if (
      requestedRanks.exists(_ <= 0) ||
      requestedRanks != requestedRanks.sorted ||
      requestedRanks.distinct.size != requestedRanks.size
    )
      return failed(
        "ranks.invalid",
        "requested ranks must be positive, unique, and increasing"
      )
    if (planningLimits == null || renderingLimits == null)
      return failed("limits.missing", "planning or rendering limits are missing")
    if (!artifact.query.resultShape.isInstanceOf[QueryResultShape.GlobalCountStar])
      return failed(
        "shape.unsupported",
        s"expected global-count-star, found ${artifact.query.resultShape.id}"
      )

    val actions = Vector.newBuilder[PostgreSqlGlobalCountBenchmarkAction]
    val skipped = Vector.newBuilder[PostgreSqlGlobalCountSkippedRealization]
    var actionOrdinal = 1

    requestedRanks.foreach { rank =>
      val candidate = artifact.candidateSearch.plans
        .find(_.rank == rank)
        .getOrElse(
          return failed("rank.not-retained", s"rank $rank was not retained")
        )
      if (candidate.candidateId != CandidatePlanId.from(candidate.decomposition))
        return failed("candidate.mismatch", s"rank $rank candidate id mismatch")

      val plan = BagFactorPlanner
        .plan(artifact.candidateInput, candidate.decomposition, planningLimits)
        .fold(
          problems =>
            return failed(
              "planning.failed",
              s"rank=$rank,errors=${problems.map(_.code).mkString(",")}"
            ),
          identity
        )
      if (plan.decomposition != candidate.decomposition)
        return failed("planning.mismatch", s"rank $rank decomposition changed")

      val configuration = canonicalConfiguration(plan)
      PostgreSqlGlobalCountBenchmarkRealization.Values.foreach { realization =>
        if (
          plan.factors.size == 1 &&
          realization ==
            PostgreSqlGlobalCountBenchmarkRealization.FusedRawNoReduction
        ) {
          skipped += PostgreSqlGlobalCountSkippedRealization(
            rank,
            candidate.candidateId,
            plan.factors.size,
            realization,
            OneBagReason
          )
        } else {
          val reductionMode =
            PostgreSqlGlobalCountBenchmarkRenderer.reductionModeFor(realization)
          val renderer =
            PostgreSqlGlobalCountBenchmarkRenderer.bindingFor(realization)
          val physicalId = PostgreSqlPhysicalVariantId.fromV2(
            candidate.candidateId,
            configuration,
            reductionMode,
            PostgreSqlGlobalCountBenchmarkRenderer.CteDirective
          )
          val rendered = PostgreSqlGlobalCountBenchmarkRenderer
            .render(
              realization,
              artifact.query,
              artifact.candidateInput,
              plan,
              renderingLimits,
              planningLimits
            )
            .fold(
              problems =>
                return failed(
                  "rendering.failed",
                  s"rank=$rank,realization=${realization.id}," +
                    s"errors=${problems.map(_.code).mkString(",")}"
                ),
              identity
            )
          val digest = renderedStatementSha256(rendered.sql)
          val actionId = PostgreSqlGlobalCountBenchmarkActionId.from(
            realization,
            candidate.candidateId,
            physicalId,
            renderer,
            digest
          )
          actions += PostgreSqlGlobalCountBenchmarkAction(
            actionOrdinal,
            aliasId(realization, rank),
            realization,
            rank,
            candidate.candidateId,
            plan.factors.size,
            configuration,
            reductionMode,
            physicalId,
            renderer,
            actionId,
            digest,
            rendered
          )
          actionOrdinal += 1
        }
      }
    }

    Right(
      PostgreSqlGlobalCountBenchmarkResult(
        ProtocolId,
        artifact.query.resultShape.id,
        requestedRanks,
        actions.result(),
        skipped.result()
      )
    )
  }

  private def aliasId(
      realization: PostgreSqlGlobalCountBenchmarkRealization,
      rank: Int
  ): String = realization match {
    case PostgreSqlGlobalCountBenchmarkRealization.FullReductionControl =>
      f"global-count-r$rank%02d-full-v2-pc"
    case PostgreSqlGlobalCountBenchmarkRealization.FusedRawNoReduction =>
      f"global-count-r$rank%02d-fused-v3-none-pc"
  }

  private[postgresql] def canonicalConfiguration(
      plan: BagFactorPlan
  ): BagFactorPhysicalConfiguration =
    BagFactorPhysicalConfiguration(
      plan.root,
      plan.factors.sortBy(_.bagId).map(factor =>
        BagFactorCoverAssignment(factor.bagId, factor.supportCover)
      )
    )

  private[postgresql] def renderedStatementSha256(sql: String): String = {
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(sql.getBytes(StandardCharsets.UTF_8))
    bytes.map(byte => f"${byte & 0xff}%02x").mkString
  }

  private def failed(
      suffix: String,
      detail: String
  ): Left[PostgreSqlGlobalCountBenchmarkFailure, Nothing] =
    Left(
      PostgreSqlGlobalCountBenchmarkFailure(
        s"global-count-actions.$suffix",
        detail
      )
    )
}
