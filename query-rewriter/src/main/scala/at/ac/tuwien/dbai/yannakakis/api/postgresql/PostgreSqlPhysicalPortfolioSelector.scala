package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidateResourceLimits,
  CandidateSearchInput,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlanningLimits,
  ConfiguredBagFactorPlan,
  ConfiguredBagFactorPlanner,
  ConfiguredBagFactorPlanningError
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlBagFactorRenderer,
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingError,
  PostgreSqlRenderingLimits,
  PostgreSqlStageDiagnosticRendering
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{QueryIR, QueryResultShape}

import scala.util.control.NonFatal

private[postgresql] final case class PostgreSqlSelectedPhysicalPortfolio(
    plan: RankedCandidatePlan,
    configuredPlan: ConfiguredBagFactorPlan,
    renderedQuery: PostgreSqlRenderedQuery,
    decision: PostgreSqlPhysicalPortfolioDecision.Selected
) {
  require(plan != null && configuredPlan != null && renderedQuery != null)
  require(decision != null)
}

/** Bounded physical selection, wholly separate from the legacy v1-v5 model. */
private[postgresql] object PostgreSqlPhysicalPortfolioSelector {
  import PostgreSqlPhysicalPortfolioSelectionFailureKind._

  def select(
      query: QueryIR,
      input: CandidateSearchInput,
      found: CandidatePlanDecision.Found,
      sourceSql: String,
      evaluator: Option[PostgreSqlExplainCostEvaluator],
      candidateResourceLimits: CandidateResourceLimits,
      portfolioLimits: PostgreSqlPhysicalPortfolioLimits,
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      bagFactorLimits: BagFactorPlanningLimits,
      renderingLimits: PostgreSqlRenderingLimits
  ): Either[
    PostgreSqlPhysicalPortfolioDecision,
    PostgreSqlSelectedPhysicalPortfolio
  ] = {
    require(query != null && input != null && found != null)
    require(
      !query.resultShape.isInstanceOf[QueryResultShape.GlobalCountStar] &&
        !query.resultShape.isInstanceOf[QueryResultShape.GroupedCountStar] &&
        !query.resultShape
          .isInstanceOf[QueryResultShape.DistributedGroupedCountStar] &&
        !query.resultShape
          .isInstanceOf[QueryResultShape.C03G0AggregateVector] &&
        !query.resultShape
          .isInstanceOf[QueryResultShape.GlobalGuardedExtremum],
      "physical portfolio is not applicable to a certified aggregate root"
    )
    require(
      found.requestedLimit ==
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit,
      "physical portfolio selector requires the exact structural K=100 request"
    )
    require(sourceSql != null && sourceSql.nonEmpty)
    require(evaluator != null && evaluator.forall(_ != null))
    require(candidateResourceLimits != null && portfolioLimits != null)
    require(evaluationLimits != null && bagFactorLimits != null)
    require(renderingLimits != null)

    val enumeration = PostgreSqlPhysicalPortfolioEnumerator
      .enumerate(input, found.plans, portfolioLimits) match {
      case Left(problem) =>
        return Left(
          failed(
            EnumerationFailed,
            problem.code,
            evaluationLimits,
            enumerationFailure = Some(problem)
          )
        )
      case Right(value) => value
    }

    var rendered = Vector.empty[PostgreSqlPhysicalPortfolioRenderedVariant]
    var raw = Vector.empty[PostgreSqlPhysicalPortfolioRawRenderingEvidence]
    enumeration.entries.foreach { entry =>
      val plan = exactPlan(found.plans, entry).getOrElse(
        {
          val problem = ConfiguredBagFactorPlanningError.ModelInvariantViolation(
            "Enumerated physical variant does not bind one retained plan"
          )
        return Left(
          failed(
            CandidatePlanningFailed,
            problem.code,
            evaluationLimits,
            enumeration = Some(enumeration),
            failedOrdinal = Some(raw.size),
            planningProblems = Vector(problem),
            raw = raw
          )
        )
        }
      )
      val configured = ConfiguredBagFactorPlanner.plan(
        input,
        plan.decomposition,
        entry.variant.configuration,
        bagFactorLimits
      ) match {
        case Left(problems) =>
          return Left(
            failed(
              CandidatePlanningFailed,
              problems.head.code,
              evaluationLimits,
              enumeration = Some(enumeration),
              failedOrdinal = Some(raw.size),
              planningProblems = problems,
              raw = raw
            )
          )
        case Right(value) => value
      }
      val diagnostic = render(
        query,
        input,
        configured,
        entry,
        renderingLimits,
        bagFactorLimits,
        evaluationLimits
      ) match {
        case Left(problems) =>
          return Left(
            failed(
              StageDiagnosticRenderingFailed,
              problems.head.code,
              evaluationLimits,
              enumeration = Some(enumeration),
              failedOrdinal = Some(raw.size),
              renderingProblems = problems,
              raw = raw
            )
          )
        case Right(value) => value
      }
      val value = PostgreSqlPhysicalPortfolioRenderedVariant(
        entry,
        plan,
        configured,
        diagnostic,
        PostgreSqlPhysicalPortfolioWorkloadDescriptor.from(
          diagnostic,
          entry.variant.materialization
        )
      )
      val evidence = PostgreSqlPhysicalPortfolioRawRenderingEvidence(
        entry,
        value.workload
      )
      rendered :+= value
      raw :+= evidence
      PostgreSqlPhysicalPortfolioDecisionReplay
        .rawRenderingResourceExceeded(raw, evaluationLimits)
        .foreach { exceeded =>
          return Left(
            failed(
              RawRenderingResourceLimitExceeded,
              PostgreSqlPhysicalPortfolioDecisionReplay.resourceDetailCode(
                exceeded.resource
              ),
              evaluationLimits,
              enumeration = Some(enumeration),
              failedOrdinal = Some(raw.size - 1),
              raw = raw,
              resourceExceeded = Some(exceeded)
            )
          )
        }
    }

    val grouped = PostgreSqlPhysicalPortfolioWorkloadBuilder
      .build(sourceSql, rendered) match {
      case Left(collision) =>
        return Left(
          failed(
            WorkloadIdentityCollision,
            WorkloadIdentityCollision.id,
            evaluationLimits,
            enumeration = Some(enumeration),
            failedOrdinal = Some(collision.offending.enumerationOrdinal),
            raw = raw,
            collisionWitness = Some(collision)
          )
        )
      case Right(value) => value
    }

    val usage = PostgreSqlPhysicalPortfolioEvaluationEnvelope
      .validate(grouped.request, evaluationLimits) match {
      case Left(exceeded) =>
        return Left(
          failed(
            EvaluationEnvelopeLimitExceeded,
            PostgreSqlPhysicalPortfolioDecisionReplay.resourceDetailCode(
              exceeded.resource
            ),
            evaluationLimits,
            enumeration = Some(enumeration),
            raw = raw,
            deduplication = Some(grouped.deduplication),
            explainRequest = Some(grouped.request),
            resourceExceeded = Some(exceeded)
          )
        )
      case Right(value) => value
    }

    val activeEvaluator = evaluator.getOrElse(
      return Left(
        evaluatorFailure(
          enumeration,
          raw,
          grouped,
          evaluationLimits,
          usage,
          contractFailure(
            grouped.request,
            PostgreSqlPhysicalPortfolioExplainFailureCode.EvaluatorUnavailable
          )
        )
      )
    )
    val evaluated =
      try activeEvaluator.evaluatePhysicalPortfolio(grouped.request, evaluationLimits)
      catch {
        case NonFatal(_) =>
          return Left(
            evaluatorFailure(
              enumeration,
              raw,
              grouped,
              evaluationLimits,
              usage,
              contractFailure(
                grouped.request,
                PostgreSqlPhysicalPortfolioExplainFailureCode.EvaluatorContractViolation
              )
            )
          )
      }
    if (evaluated == null)
      return Left(
        evaluatorFailure(
          enumeration,
          raw,
          grouped,
          evaluationLimits,
          usage,
          contractFailure(
            grouped.request,
            PostgreSqlPhysicalPortfolioExplainFailureCode.EvaluatorContractViolation
          )
        )
      )

    evaluated match {
      case Left(problem)
          if problem != null &&
            PostgreSqlPhysicalPortfolioExplainReplay.verifyFailurePrefix(
              grouped.request,
              problem
            ) =>
        Left(
          evaluatorFailure(
            enumeration,
            raw,
            grouped,
            evaluationLimits,
            usage,
            problem
          )
        )
      case Left(_) =>
        Left(
          evaluatorFailure(
            enumeration,
            raw,
            grouped,
            evaluationLimits,
            usage,
            contractFailure(
              grouped.request,
              PostgreSqlPhysicalPortfolioExplainFailureCode.EvaluatorContractViolation
            )
          )
        )
      case Right(batch) =>
        val verified = Option(batch).flatMap(value =>
          PostgreSqlPhysicalPortfolioExplainReplay.verify(grouped.request, value)
        ).getOrElse(
          return Left(
            evaluatorFailure(
              enumeration,
              raw,
              grouped,
              evaluationLimits,
              usage,
              contractFailure(
                grouped.request,
                PostgreSqlPhysicalPortfolioExplainFailureCode.EvaluatorContractViolation
              )
            )
          )
        )
        var completed = Vector.empty[PostgreSqlPhysicalPortfolioEvaluationEvidence]
        grouped.request.workloads.zipWithIndex.foreach { case (workload, index) =>
          val representative = grouped.representatives(index)
          buildEvaluation(
            workload,
            representative,
            verified.source,
            verified.observationsByWorkload(index)
          ) match {
            case None =>
              return Left(
                failed(
                  EvidenceInvalid,
                  EvidenceInvalid.id,
                  evaluationLimits,
                  enumeration = Some(enumeration),
                  raw = raw,
                  deduplication = Some(grouped.deduplication),
                  explainRequest = Some(grouped.request),
                  evaluationUsage = Some(usage),
                  explainBatch = Some(batch),
                  completedEvaluations = completed
                )
              )
            case Some(value) => completed :+= value
          }
        }
        val admissible = completed.filter(_.rowGuard.withinGuard)
        if (admissible.isEmpty)
          Left(
            PostgreSqlPhysicalPortfolioDecision.NoAdmissible(
              enumeration,
              raw,
              grouped.deduplication,
              grouped.request,
              evaluationLimits,
              usage,
              batch,
              completed
            )
          )
        else {
          val winner = admissible.minBy(_.score)(
            PostgreSqlPhysicalPortfolioScore.ordering
          )
          val decision = PostgreSqlPhysicalPortfolioDecision.Selected(
            enumeration,
            raw,
            grouped.deduplication,
            grouped.request,
            evaluationLimits,
            usage,
            batch,
            completed,
            winner.workload.descriptor.workloadId,
            winner.workload.representative.physicalVariantId
          )
          val selected = grouped.representatives.find(
            _.entry.variant.physicalVariantId ==
              decision.selectedPhysicalVariantId
          ).get
          Right(
            PostgreSqlSelectedPhysicalPortfolio(
              selected.plan,
              selected.configuredPlan,
              selected.rendering.renderedQuery,
              decision
            )
          )
        }
    }
  }

  private def buildEvaluation(
      workload: PostgreSqlPhysicalPortfolioExplainWorkload,
      representative: PostgreSqlPhysicalPortfolioRenderedVariant,
      source: PostgreSqlExplainSourceEvaluation,
      observations: Vector[PostgreSqlPhysicalPortfolioExplainObservation]
  ): Option[PostgreSqlPhysicalPortfolioEvaluationEvidence] =
    try {
      val trace = representative.rendering.renderedQuery.trace.stages
      val frontier = PostgreSqlPhysicalPortfolioFinalFrontier
        .derive(trace)
        .toOption
        .getOrElse(return None)
      val frontierSet = frontier.toSet
      var emittedIndex = 0
      val stages = trace.map { stage =>
        if (stage.physicalForm.emitted) {
          val requestStage = workload.stages(emittedIndex)
          val observation = observations(emittedIndex)
          emittedIndex += 1
          PostgreSqlPhysicalPortfolioStageEvaluation(
            stage,
            Some(requestStage.statementSha256),
            Some(requestStage.characterCount),
            Some(requestStage.ctePrefixCount),
            Some(observation.estimate),
            frontierSet(stage.id)
          )
        } else
          PostgreSqlPhysicalPortfolioStageEvaluation(
            stage,
            None,
            None,
            None,
            None,
            finalFrontier = false
          )
      }
      if (emittedIndex != observations.size) return None
      val namedBytes = stages.collect {
        case stage if stage.trace.physicalForm.namedCte =>
          PostgreSqlFactorAwareScore.exactEstimatedBytes(
            stage.estimate.get.planRows,
            stage.estimate.get.planWidth
          )
      }
      val frontierBytes = stages.collect {
        case stage if stage.finalFrontier =>
          PostgreSqlFactorAwareScore.exactEstimatedBytes(
            stage.estimate.get.planRows,
            stage.estimate.get.planWidth
          )
      }
      val terminal = stages.last.estimate.get
      val score = PostgreSqlPhysicalPortfolioScore(
        namedBytes.foldLeft(BigDecimal(0))(_.max(_)),
        PostgreSqlFactorAwareScore.exactSum(frontierBytes),
        terminal.planTotalCost,
        representative.plan.rank,
        representative.plan.candidateId,
        representative.entry.variant.physicalVariantId
      )
      Some(
        PostgreSqlPhysicalPortfolioEvaluationEvidence(
          workload.group,
          source,
          stages,
          frontier,
          PostgreSqlExplainRowGuardEvidence.from(
            source.estimate.planRows,
            terminal.planRows
          ),
          score,
          PostgreSqlPhysicalPortfolioEvaluationV1.ScoreScheme,
          PostgreSqlPhysicalPortfolioEvaluationV1.FinalFrontierScheme,
          PostgreSqlFactorAwareNumericEnvelope.Scheme
        )
      )
    } catch {
      case NonFatal(_) => None
    }

  private def evaluatorFailure(
      enumeration: PostgreSqlPhysicalPortfolioEnumeration,
      raw: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      grouped: PostgreSqlPhysicalPortfolioGroupedWorkloads,
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      usage: PostgreSqlPhysicalPortfolioEvaluationUsage,
      problem: PostgreSqlPhysicalPortfolioExplainFailure
  ): PostgreSqlPhysicalPortfolioDecision.Failed =
    failed(
      EvaluatorFailed,
      problem.code.id,
      limits,
      enumeration = Some(enumeration),
      raw = raw,
      deduplication = Some(grouped.deduplication),
      explainRequest = Some(grouped.request),
      evaluationUsage = Some(usage),
      explainFailure = Some(problem)
    )

  private def contractFailure(
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      code: PostgreSqlPhysicalPortfolioExplainFailureCode
  ): PostgreSqlPhysicalPortfolioExplainFailure =
    PostgreSqlPhysicalPortfolioExplainFailure(
      code,
      PostgreSqlPhysicalPortfolioExplainStage.Session,
      None,
      None,
      Vector.empty,
      request.flattened.size,
      None,
      None
    )

  private def failed(
      kind: PostgreSqlPhysicalPortfolioSelectionFailureKind,
      detailCode: String,
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      enumerationFailure: Option[
        PostgreSqlPhysicalPortfolioEnumerationFailure
      ] = None,
      planningProblems: Vector[ConfiguredBagFactorPlanningError] = Vector.empty,
      renderingProblems: Vector[PostgreSqlRenderingError] = Vector.empty,
      failedOrdinal: Option[Int] = None,
      enumeration: Option[PostgreSqlPhysicalPortfolioEnumeration] = None,
      raw: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence] = Vector.empty,
      deduplication: Option[
        PostgreSqlPhysicalPortfolioDeduplicationEvidence
      ] = None,
      explainRequest: Option[
        PostgreSqlPhysicalPortfolioExplainBatchRequest
      ] = None,
      evaluationUsage: Option[
        PostgreSqlPhysicalPortfolioEvaluationUsage
      ] = None,
      resourceExceeded: Option[
        PostgreSqlPhysicalPortfolioEvaluationResourceExceeded
      ] = None,
      explainFailure: Option[PostgreSqlPhysicalPortfolioExplainFailure] = None,
      explainBatch: Option[PostgreSqlPhysicalPortfolioExplainBatch] = None,
      completedEvaluations: Vector[
        PostgreSqlPhysicalPortfolioEvaluationEvidence
      ] = Vector.empty,
      collisionWitness: Option[
        PostgreSqlPhysicalPortfolioCollisionWitness
      ] = None
  ): PostgreSqlPhysicalPortfolioDecision.Failed =
    PostgreSqlPhysicalPortfolioDecision.Failed(
      kind,
      detailCode,
      enumerationFailure,
      ConfiguredBagFactorPlanningError.canonical(planningProblems),
      PostgreSqlRenderingError.canonical(renderingProblems),
      failedOrdinal,
      enumeration,
      raw,
      deduplication,
      explainRequest,
      limits,
      evaluationUsage,
      resourceExceeded,
      explainFailure,
      explainBatch,
      completedEvaluations,
      collisionWitness
    )

  private[postgresql] def exactPlan(
      plans: Vector[RankedCandidatePlan],
      entry: PostgreSqlPhysicalPortfolioEntry
  ): Option[RankedCandidatePlan] =
    plans.find(plan =>
      plan.rank == entry.variant.structuralRank &&
        plan.candidateId == entry.variant.candidateId
    )

  private[postgresql] def render(
      query: QueryIR,
      input: CandidateSearchInput,
      configured: ConfiguredBagFactorPlan,
      entry: PostgreSqlPhysicalPortfolioEntry,
      renderingLimits: PostgreSqlRenderingLimits,
      bagFactorLimits: BagFactorPlanningLimits,
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlStageDiagnosticRendering] =
    PostgreSqlBagFactorRenderer.renderConfiguredWithStageDiagnostics(
      query,
      input,
      configured,
      renderingLimits,
      bagFactorLimits,
      entry.variant.reductionMode,
      entry.variant.materialization,
      evaluationLimits.stageDiagnosticLimits
    )
}

private[postgresql] final case class PostgreSqlPhysicalPortfolioGroupedWorkloads(
    deduplication: PostgreSqlPhysicalPortfolioDeduplicationEvidence,
    request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
    representatives: Vector[PostgreSqlPhysicalPortfolioRenderedVariant]
)

/** Full-string collision-safe grouping shared by selection and replay. */
private[postgresql] object PostgreSqlPhysicalPortfolioWorkloadBuilder {
  def build(
      sourceSql: String,
      rendered: Vector[PostgreSqlPhysicalPortfolioRenderedVariant]
  ): Either[
    PostgreSqlPhysicalPortfolioCollisionWitness,
    PostgreSqlPhysicalPortfolioGroupedWorkloads
  ] = {
    require(sourceSql != null && sourceSql.nonEmpty)
    require(rendered != null && rendered.nonEmpty && rendered.forall(_ != null))
    require(
      rendered.map(_.entry.enumerationOrdinal) == rendered.indices.toVector
    )
    var groups = Vector.empty[Vector[PostgreSqlPhysicalPortfolioRenderedVariant]]
    rendered.foreach { value =>
      val matchingIndex = groups.indexWhere(
        _.head.workload.workloadId == value.workload.workloadId
      )
      if (matchingIndex < 0) groups :+= Vector(value)
      else {
        val existing = groups(matchingIndex)
        existing.find(prior => !sameExactWorkload(prior, value)).foreach {
          prior =>
            return Left(
              PostgreSqlPhysicalPortfolioCollisionWitness(
                PostgreSqlPhysicalPortfolioCollisionRendering.from(prior),
                PostgreSqlPhysicalPortfolioCollisionRendering.from(value)
              )
            )
        }
        groups = groups.updated(matchingIndex, existing :+ value)
      }
    }
    val representatives = Vector.newBuilder[
      PostgreSqlPhysicalPortfolioRenderedVariant
    ]
    val evidence = groups.map { aliasesInEnumerationOrder =>
      val representative = aliasesInEnumerationOrder.minBy(
        _.entry.variant.physicalVariantId.value
      )
      representatives += representative
      val aliases = aliasesInEnumerationOrder
        .map(alias)
        .sortBy(_.physicalVariantId.value)
      PostgreSqlPhysicalPortfolioWorkloadGroupEvidence(
        representative.workload,
        alias(representative),
        aliases,
        aliasesInEnumerationOrder.map(_.entry.enumerationOrdinal).min
      )
    }
    val deduplication = PostgreSqlPhysicalPortfolioDeduplicationEvidence(
      evidence,
      rendered.size,
      evidence.size,
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.DeduplicationScheme
    )
    val representativeValues = representatives.result()
    val workloads = evidence.zip(representativeValues).map {
      case (group, representative) =>
        PostgreSqlPhysicalPortfolioExplainWorkload(
          group,
          representative.rendering.statements.map(statement =>
            PostgreSqlPhysicalPortfolioExplainStageStatement(
              statement.stageId,
              statement.statementSha256,
              statement.characterCount,
              statement.ctePrefixCount,
              statement.sql
            )
          )
        )
    }
    val request = PostgreSqlPhysicalPortfolioExplainBatchRequest(
      PostgreSqlExplainSourceStatement(
        PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sourceSql),
        sourceSql
      ),
      workloads,
      PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum,
      PostgreSqlCteMaterializationDirective.Scope
    )
    Right(
      PostgreSqlPhysicalPortfolioGroupedWorkloads(
        deduplication,
        request,
        representativeValues
      )
    )
  }

  private def sameExactWorkload(
      left: PostgreSqlPhysicalPortfolioRenderedVariant,
      right: PostgreSqlPhysicalPortfolioRenderedVariant
  ): Boolean =
    left.workload == right.workload &&
      left.rendering.statements.map(_.sql) == right.rendering.statements.map(_.sql)

  private def alias(
      value: PostgreSqlPhysicalPortfolioRenderedVariant
  ): PostgreSqlPhysicalPortfolioWorkloadAlias =
    PostgreSqlPhysicalPortfolioWorkloadAlias(
      value.entry.enumerationOrdinal,
      value.entry.variant.structuralRank,
      value.entry.variant.candidateId,
      value.entry.variant.physicalVariantId
    )
}

/** Exact pure regeneration invoked by attempt construction and v6 emission. */
object PostgreSqlPhysicalPortfolioSelectionReplay {
  def verifyAttempt(attempt: PostgreSqlPhysicalPortfolioAttempt): Boolean =
    try {
      if (attempt == null || attempt.decision.evaluationLimits != attempt.evaluationLimits)
        return false
      val failed = attempt.decision match {
        case value: PostgreSqlPhysicalPortfolioDecision.Failed => Some(value)
        case _                                                  => None
      }
      val enumeration = attempt.decision match {
        case value: PostgreSqlPhysicalPortfolioDecision.Selected =>
          Some(value.enumeration)
        case value: PostgreSqlPhysicalPortfolioDecision.NoAdmissible =>
          Some(value.enumeration)
        case value: PostgreSqlPhysicalPortfolioDecision.Failed =>
          value.enumeration
      }
      if (enumeration.isEmpty)
        return failed.exists(_.failureKind ==
          PostgreSqlPhysicalPortfolioSelectionFailureKind.EnumerationFailed)
      val rawEvidence = attempt.decision match {
        case value: PostgreSqlPhysicalPortfolioDecision.Selected =>
          value.rawRenderings
        case value: PostgreSqlPhysicalPortfolioDecision.NoAdmissible =>
          value.rawRenderings
        case value: PostgreSqlPhysicalPortfolioDecision.Failed =>
          value.rawRenderPrefix
      }
      var replayed = Vector.empty[PostgreSqlPhysicalPortfolioRenderedVariant]
      rawEvidence.zipWithIndex.foreach { case (expected, index) =>
        val entry = enumeration.get.entries(index)
        val plan = PostgreSqlPhysicalPortfolioSelector
          .exactPlan(attempt.candidateSearch.plans, entry)
          .getOrElse(return false)
        val configured = ConfiguredBagFactorPlanner
          .plan(
            attempt.candidateInput,
            plan.decomposition,
            entry.variant.configuration,
            attempt.bagFactorPlanningLimits
          )
          .toOption
          .getOrElse(return false)
        val diagnostic = PostgreSqlPhysicalPortfolioSelector
          .render(
            attempt.query,
            attempt.candidateInput,
            configured,
            entry,
            attempt.renderingLimits,
            attempt.bagFactorPlanningLimits,
            attempt.evaluationLimits
          )
          .toOption
          .getOrElse(return false)
        val value = PostgreSqlPhysicalPortfolioRenderedVariant(
          entry,
          plan,
          configured,
          diagnostic,
          PostgreSqlPhysicalPortfolioWorkloadDescriptor.from(
            diagnostic,
            entry.variant.materialization
          )
        )
        if (
          expected != PostgreSqlPhysicalPortfolioRawRenderingEvidence(
            entry,
            value.workload
          )
        ) return false
        replayed :+= value
      }
      val rawExceeded = PostgreSqlPhysicalPortfolioDecisionReplay
        .rawRenderingResourceExceeded(rawEvidence, attempt.evaluationLimits)
      if (
        rawExceeded.nonEmpty && failed.forall(_.failureKind !=
          PostgreSqlPhysicalPortfolioSelectionFailureKind.RawRenderingResourceLimitExceeded)
      ) return false
      failed match {
        case Some(value)
            if value.failureKind ==
              PostgreSqlPhysicalPortfolioSelectionFailureKind.CandidatePlanningFailed =>
          val entry = enumeration.get.entries(value.failedEnumerationOrdinal.get)
          val plan = PostgreSqlPhysicalPortfolioSelector
            .exactPlan(attempt.candidateSearch.plans, entry)
            .getOrElse(return false)
          return ConfiguredBagFactorPlanner
            .plan(
              attempt.candidateInput,
              plan.decomposition,
              entry.variant.configuration,
              attempt.bagFactorPlanningLimits
            )
            .left
            .toOption
            .contains(value.planningProblems)
        case Some(value)
            if value.failureKind ==
              PostgreSqlPhysicalPortfolioSelectionFailureKind.StageDiagnosticRenderingFailed =>
          val entry = enumeration.get.entries(value.failedEnumerationOrdinal.get)
          val plan = PostgreSqlPhysicalPortfolioSelector
            .exactPlan(attempt.candidateSearch.plans, entry)
            .getOrElse(return false)
          val configured = ConfiguredBagFactorPlanner
            .plan(
              attempt.candidateInput,
              plan.decomposition,
              entry.variant.configuration,
              attempt.bagFactorPlanningLimits
            )
            .toOption
            .getOrElse(return false)
          return PostgreSqlPhysicalPortfolioSelector
            .render(
              attempt.query,
              attempt.candidateInput,
              configured,
              entry,
              attempt.renderingLimits,
              attempt.bagFactorPlanningLimits,
              attempt.evaluationLimits
            )
            .left
            .toOption
            .contains(value.renderingProblems)
        case _ => ()
      }
      failed match {
        case Some(value)
            if value.failureKind ==
              PostgreSqlPhysicalPortfolioSelectionFailureKind.RawRenderingResourceLimitExceeded =>
          return rawExceeded == value.resourceExceeded
        case _ if rawExceeded.nonEmpty => return false
        case _                         => ()
      }
      if (replayed.size != enumeration.get.entries.size) return false
      PostgreSqlPhysicalPortfolioWorkloadBuilder.build(attempt.sourceSql, replayed) match {
        case Left(collision) =>
          failed.exists(value =>
            value.failureKind ==
              PostgreSqlPhysicalPortfolioSelectionFailureKind.WorkloadIdentityCollision &&
              value.collisionWitness.contains(collision)
          )
        case Right(grouped) =>
          attempt.decision match {
            case value: PostgreSqlPhysicalPortfolioDecision.Selected =>
              value.deduplication == grouped.deduplication &&
                value.explainRequest == grouped.request
            case value: PostgreSqlPhysicalPortfolioDecision.NoAdmissible =>
              value.deduplication == grouped.deduplication &&
                value.explainRequest == grouped.request
            case value: PostgreSqlPhysicalPortfolioDecision.Failed =>
              value.failureKind match {
                case PostgreSqlPhysicalPortfolioSelectionFailureKind.EvaluationEnvelopeLimitExceeded |
                    PostgreSqlPhysicalPortfolioSelectionFailureKind.EvaluatorFailed |
                    PostgreSqlPhysicalPortfolioSelectionFailureKind.EvidenceInvalid =>
                  value.deduplication.contains(grouped.deduplication) &&
                    value.explainRequest.contains(grouped.request)
                case _ => false
              }
          }
      }
    } catch {
      case NonFatal(_) => false
    }
}
