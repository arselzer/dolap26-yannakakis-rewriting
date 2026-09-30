package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorCoverAssignment,
  BagFactorPhysicalConfiguration,
  BagFactorPlan,
  BagFactorPlanningLimits,
  ConfiguredBagFactorPlanner
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlBagFactorRenderer,
  PostgreSqlEnumerationRenderingProfile,
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingLimits,
  PostgreSqlStageDiagnosticLimits,
  PostgreSqlStageDiagnosticRendering,
  PostgreSqlStageId
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

/** One complete, transport-checked cell of the current physical dry run. */
private[yannakakis] final case class PostgreSqlEnumerationPhysicalFactorialV3Action(
    ordinal: Int,
    physicalVariantId: PostgreSqlPhysicalVariantId,
    renderingProfileId: String,
    reductionMode: YannakakisReductionMode,
    cteMaterializationDirective: PostgreSqlCteMaterializationDirective,
    renderedQuery: PostgreSqlRenderedQuery,
    stageDiagnostics: PostgreSqlStageDiagnosticRendering,
    renderedStatementSha256: String
) extends Serializable {
  require(ordinal > 0, "physical-factorial ordinal must be positive")
  require(physicalVariantId != null, "physical variant id must not be null")
  require(
    renderingProfileId ==
      PostgreSqlEnumerationPhysicalFactorialV3.RenderingProfileId,
    "physical-factorial action must use statistics-safe-v2"
  )
  require(reductionMode != null, "reduction mode must not be null")
  require(
    cteMaterializationDirective != null,
    "CTE materialization directive must not be null"
  )
  require(renderedQuery != null, "rendered query must not be null")
  require(
    stageDiagnostics != null && stageDiagnostics.renderedQuery == renderedQuery,
    "stage diagnostics must retain the rendered query"
  )
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}"),
    "rendered statement digest must be lowercase SHA-256"
  )

  val sql: String = renderedQuery.sql
}

/** Exact nine-cell result for one retained CandidateTD and its canonical plan. */
private[yannakakis] final case class PostgreSqlEnumerationPhysicalFactorialV3Result(
    protocolId: String,
    renderingProfileId: String,
    structuralRank: Int,
    candidateId: CandidatePlanId,
    configuration: BagFactorPhysicalConfiguration,
    canonicalBagFactorPlan: BagFactorPlan,
    actions: Vector[PostgreSqlEnumerationPhysicalFactorialV3Action]
) extends Serializable {
  require(
    protocolId == PostgreSqlEnumerationPhysicalFactorialV3.ProtocolId,
    "physical-factorial protocol id must be current-v3"
  )
  require(
    renderingProfileId ==
      PostgreSqlEnumerationPhysicalFactorialV3.RenderingProfileId,
    "physical-factorial renderer must be statistics-safe-v2"
  )
  require(structuralRank > 0, "structural rank must be positive")
  require(candidateId != null, "candidate id must not be null")
  require(configuration != null, "physical configuration must not be null")
  require(canonicalBagFactorPlan != null, "bag-factor plan must not be null")
  require(
    candidateId == CandidatePlanId.from(canonicalBagFactorPlan.decomposition),
    "candidate id must bind the structural decomposition"
  )
  require(
    actions != null &&
      actions.map(action =>
        action.reductionMode -> action.cteMaterializationDirective
      ) == PostgreSqlEnumerationPhysicalFactorialV3.AxisOrder,
    "physical-factorial actions must retain the exact 3x3 order"
  )
  require(
    actions.map(_.physicalVariantId).distinct.size == actions.size,
    "physical-factorial variant ids must be unique"
  )

  val decompositionId: CandidatePlanId = candidateId
}

/** Closed failure result; no partial action vector is returned. */
private[yannakakis] final case class PostgreSqlEnumerationPhysicalFactorialV3Failure(
    code: String,
    detail: String
) extends Serializable {
  require(code != null && code.nonEmpty, "failure code must not be blank")
  require(detail != null && detail.nonEmpty, "failure detail must not be blank")
}

/**
  * Thin current-v3 development boundary for a same-CandidateTD physical dry
  * run. It changes neither public rewrite options nor frozen serializers.
  */
private[yannakakis] object PostgreSqlEnumerationPhysicalFactorialV3 {
  val ProtocolId: String = "postgresql-enumeration-physical-factorial/current-v3"
  val RenderingProfileId: String =
    PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2.id

  val ReductionModeOrder: Vector[YannakakisReductionMode] = Vector(
    YannakakisReductionMode.FullTwoPass,
    YannakakisReductionMode.BottomUpOnly,
    YannakakisReductionMode.NoReduction
  )
  val CteMaterializationDirectiveOrder
      : Vector[PostgreSqlCteMaterializationDirective] = Vector(
    PostgreSqlCteMaterializationDirective.PlannerControlled,
    PostgreSqlCteMaterializationDirective.Materialized,
    PostgreSqlCteMaterializationDirective.NotMaterialized
  )
  val AxisOrder: Vector[
    (YannakakisReductionMode, PostgreSqlCteMaterializationDirective)
  ] = ReductionModeOrder.flatMap(mode =>
    CteMaterializationDirectiveOrder.map(directive => mode -> directive)
  )

  def produce(
      batchResult: PostgreSqlEnumerationBatchResult,
      structuralRank: Int
  ): Either[
    PostgreSqlEnumerationPhysicalFactorialV3Failure,
    PostgreSqlEnumerationPhysicalFactorialV3Result
  ] = {
    if (batchResult == null)
      return failed("batch.missing", "enumeration batch is missing")
    if (structuralRank <= 0)
      return failed("rank.invalid", s"structural rank $structuralRank is invalid")
    val artifact = batchResult.artifact match {
      case Some(value)
          if batchResult.status == PostgreSqlEnumerationBatchStatus.Produced =>
        value
      case _ =>
        return failed(
          "batch.not-produced",
          s"batch status=${batchResult.status.id}, phase=${batchResult.completedPhase.id}"
        )
    }
    if (
      PostgreSqlEnumerationRenderingProfile.default !=
        PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
    ) {
      return failed(
        "profile.mismatch",
        "current default enumeration renderer is not statistics-safe-v2"
      )
    }

    val candidate = artifact.batch.candidates
      .find(_.plan.rank == structuralRank)
      .getOrElse(
        return failed(
          "rank.not-retained",
          s"rank $structuralRank is outside the retained ${artifact.batch.retainedCount}"
        )
      )
    if (
      candidate.plan.candidateId !=
        CandidatePlanId.from(candidate.plan.decomposition)
    ) {
      return failed(
        "plan.mismatch",
        "candidate id does not bind its structural decomposition"
      )
    }

    val contract = batchResult.producerSearchContract
    val bagFactorLimits = BagFactorPlanningLimits
      .create(
        contract.bagFactor.maxBags,
        contract.bagFactor.maxOwnershipChecks,
        contract.bagFactor.maxTreeEdges,
        contract.bagFactor.maxSourceEdges
      )
      .fold(_ => return failed("limits.invalid", "bag-factor limits"), identity)
    val renderingLimits = PostgreSqlRenderingLimits
      .create(
        contract.rendering.maxExpandedScans,
        contract.rendering.maxRenderedTerms,
        contract.rendering.maxCtes,
        contract.rendering.maxSqlCharacters
      )
      .fold(_ => return failed("limits.invalid", "rendering limits"), identity)
    val diagnosticLimits = PostgreSqlStageDiagnosticLimits
      .create(
        contract.stageDiagnostics.maxStatements,
        contract.stageDiagnostics.maxCharactersPerStatement,
        contract.stageDiagnostics.maxTotalCharacters
      )
      .fold(
        _ => return failed("limits.invalid", "stage-diagnostic limits"),
        identity
      )

    val canonicalPlan = candidate.bagFactorPlan
    val configuration = BagFactorPhysicalConfiguration(
      canonicalPlan.root,
      canonicalPlan.factors.sortBy(_.bagId).map(factor =>
        BagFactorCoverAssignment(factor.bagId, factor.supportCover)
      )
    )
    val configured = ConfiguredBagFactorPlanner
      .plan(
        artifact.batch.candidateInput,
        candidate.plan.decomposition,
        configuration,
        bagFactorLimits
      )
      .fold(
        errors =>
          return failed(
            "replanning.failed",
            errors.map(_.code).mkString(",")
          ),
        identity
      )
    if (
      configured.structuralDecomposition != candidate.plan.decomposition ||
      configured.configuration != configuration ||
      configured.plan != canonicalPlan
    ) {
      return failed(
        "plan.mismatch",
        "configured replanning did not reproduce the canonical BagFactorPlan"
      )
    }

    val actions = Vector.newBuilder[PostgreSqlEnumerationPhysicalFactorialV3Action]
    AxisOrder.zipWithIndex.foreach { case ((mode, directive), index) =>
      val ordinal = index + 1
      val diagnostics = PostgreSqlBagFactorRenderer
        .renderConfiguredWithStageDiagnostics(
          artifact.batch.query,
          artifact.batch.candidateInput,
          configured,
          renderingLimits,
          bagFactorLimits,
          mode,
          directive,
          diagnosticLimits
        )
        .fold(
          errors =>
            return failed(
              "rendering.failed",
              s"action=$ordinal,mode=${mode.id},directive=${directive.id},errors=${errors.map(_.code).mkString(",")}"
            ),
          identity
        )
      val rendered = diagnostics.renderedQuery
      val renderedSha256 =
        PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
          rendered.sql
        )
      val transportAssets = Vector(rendered.sql) ++
        diagnostics.statements.map(_.sql)
      transportAssets.zipWithIndex.foreach { case (sql, assetIndex) =>
        PostgreSqlEnumerationSqlTransport
          .validate(
            sql,
            contract.transport.maxGeneratedOrStageSqlBytes,
            allowEmpty = false
          ) match {
          case Left(reason) =>
            return failed(
              "sql-transport.failed",
              s"action=$ordinal,asset=${assetIndex + 1},reason=${reason.code}"
            )
          case Right(_) => ()
        }
      }
      if (
        rendered.bagFactorPlan != canonicalPlan ||
        diagnostics.statements.isEmpty ||
        diagnostics.statements.last.stageId !=
          PostgreSqlStageId.FinalReconstruction ||
        diagnostics.statements.last.statementSha256 != renderedSha256
      ) {
        return failed(
          "rendering.invariant",
          s"action $ordinal did not retain the plan and terminal hash"
        )
      }
      actions += PostgreSqlEnumerationPhysicalFactorialV3Action(
        ordinal,
        PostgreSqlPhysicalVariantId.fromV2(
          candidate.plan.candidateId,
          configuration,
          mode,
          directive
        ),
        RenderingProfileId,
        mode,
        directive,
        rendered,
        diagnostics,
        renderedSha256
      )
    }

    val renderedActions = actions.result()
    val anchor = renderedActions.head
    if (
      anchor.renderedQuery != candidate.renderedQuery ||
      anchor.stageDiagnostics != candidate.stageDiagnostics ||
      anchor.renderedStatementSha256 != candidate.renderedStatementSha256
    ) {
      return failed(
        "profile.mismatch",
        "batch full/planner anchor differs from its statistics-safe-v2 re-render"
      )
    }

    Right(
      PostgreSqlEnumerationPhysicalFactorialV3Result(
        ProtocolId,
        RenderingProfileId,
        structuralRank,
        candidate.plan.candidateId,
        configuration,
        canonicalPlan,
        renderedActions
      )
    )
  }

  private def failed(
      suffix: String,
      detail: String
  ): Left[PostgreSqlEnumerationPhysicalFactorialV3Failure, Nothing] =
    Left(
      PostgreSqlEnumerationPhysicalFactorialV3Failure(
        s"physical-factorial-v3.$suffix",
        detail
      )
    )
}
