package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateResourceLimits,
  CandidatePlanDecision,
  CandidatePlanId,
  CandidateSearchInput,
  RankedCandidatePlan,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlanningLimits,
  ConfiguredBagFactorPlan,
  ConfiguredBagFactorPlanningError
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlBagFactorRenderer,
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingError,
  PostgreSqlRenderingLimits,
  PostgreSqlStageDiagnosticRendering
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{
  AdmissibleSoftBagGeneration,
  SoftBagGeneration
}
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  QueryHypergraphConverter,
  QueryIR
}

import scala.util.control.NonFatal

/** SQL-free evidence that one raw scheduler entry rendered successfully. */
final case class PostgreSqlPhysicalPortfolioRawRenderingEvidence private[postgresql] (
    entry: PostgreSqlPhysicalPortfolioEntry,
    workload: PostgreSqlPhysicalPortfolioWorkloadDescriptor
) extends Serializable {
  require(entry != null)
  require(workload != null)
  require(
    entry.variant.materialization == workload.materialization &&
      entry.variant.materializationScope == workload.materializationScope
  )
}

/** Internal full-string rendering retained until collision-safe grouping. */
private[postgresql] final case class PostgreSqlPhysicalPortfolioRenderedVariant(
    entry: PostgreSqlPhysicalPortfolioEntry,
    plan: RankedCandidatePlan,
    configuredPlan: ConfiguredBagFactorPlan,
    rendering: PostgreSqlStageDiagnosticRendering,
    workload: PostgreSqlPhysicalPortfolioWorkloadDescriptor
) {
  require(entry != null)
  require(plan != null)
  require(configuredPlan != null)
  require(rendering != null)
  require(workload != null)
  require(
    entry.variant.candidateId == plan.candidateId &&
      entry.variant.structuralRank == plan.rank &&
      plan.candidateId == CandidatePlanId.from(plan.decomposition) &&
      configuredPlan.structuralDecomposition == plan.decomposition &&
      configuredPlan.structuralCertificate == plan.certificate &&
      configuredPlan.configuration == entry.variant.configuration &&
      configuredPlan.structuralDecomposition ==
        configuredPlan.plan.decomposition &&
      workload == PostgreSqlPhysicalPortfolioWorkloadDescriptor.from(
        rendering,
        entry.variant.materialization
      )
  )
}

/** Package-private full-string evidence used only to reject hash collisions. */
private[postgresql] final case class PostgreSqlPhysicalPortfolioCollisionRendering(
    enumerationOrdinal: Int,
    descriptor: PostgreSqlPhysicalPortfolioWorkloadDescriptor,
    orderedStageSql: Vector[String]
) {
  require(enumerationOrdinal >= 0)
  require(descriptor != null)
  require(
    orderedStageSql != null && orderedStageSql.size == descriptor.stages.size &&
      orderedStageSql.forall(sql => sql != null && sql.nonEmpty)
  )
  require(
    orderedStageSql.zip(descriptor.stages).forall { case (sql, stage) =>
      sql.length.toLong == stage.characterCount &&
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql) ==
        stage.statementSha256
    },
    "collision rendering SQL must bind every ordered descriptor stage"
  )
}

private[postgresql] object PostgreSqlPhysicalPortfolioCollisionRendering {
  def from(
      rendered: PostgreSqlPhysicalPortfolioRenderedVariant
  ): PostgreSqlPhysicalPortfolioCollisionRendering = {
    require(rendered != null)
    PostgreSqlPhysicalPortfolioCollisionRendering(
      rendered.entry.enumerationOrdinal,
      rendered.workload,
      rendered.rendering.statements.map(_.sql)
    )
  }
}

/** The exact two full workloads proving a workload-id collision. */
private[postgresql] final case class PostgreSqlPhysicalPortfolioCollisionWitness(
    prior: PostgreSqlPhysicalPortfolioCollisionRendering,
    offending: PostgreSqlPhysicalPortfolioCollisionRendering
) {
  require(prior != null && offending != null)
  require(prior.enumerationOrdinal < offending.enumerationOrdinal)
  require(prior.descriptor.workloadId == offending.descriptor.workloadId)
  require(
    prior.descriptor != offending.descriptor ||
      prior.orderedStageSql != offending.orderedStageSql,
    "byte-identical workload aliases are not identity collisions"
  )
}

/** Narrow v6-only projection; no other decision state may expose raw SQL. */
private[yannakakis] final case class PostgreSqlPhysicalPortfolioCollisionSqlVector(
    enumerationOrdinal: Int,
    descriptor: PostgreSqlPhysicalPortfolioWorkloadDescriptor,
    orderedStageSql: Vector[String]
)

private[yannakakis] final case class PostgreSqlPhysicalPortfolioCollisionProjection(
    prior: PostgreSqlPhysicalPortfolioCollisionSqlVector,
    offending: PostgreSqlPhysicalPortfolioCollisionSqlVector
)

private[yannakakis] object PostgreSqlPhysicalPortfolioCollisionProjection {
  def from(
      decision: PostgreSqlPhysicalPortfolioDecision
  ): Option[PostgreSqlPhysicalPortfolioCollisionProjection] = decision match {
    case failed: PostgreSqlPhysicalPortfolioDecision.Failed
        if failed.failureKind ==
          PostgreSqlPhysicalPortfolioSelectionFailureKind.WorkloadIdentityCollision =>
      val witness = failed.collisionWitness.getOrElse(
        throw new IllegalArgumentException(
          "collision failure has no internal full-SQL witness"
        )
      )
      def project(
          value: PostgreSqlPhysicalPortfolioCollisionRendering
      ): PostgreSqlPhysicalPortfolioCollisionSqlVector =
        PostgreSqlPhysicalPortfolioCollisionSqlVector(
          value.enumerationOrdinal,
          value.descriptor,
          value.orderedStageSql
        )
      Some(
        PostgreSqlPhysicalPortfolioCollisionProjection(
          project(witness.prior),
          project(witness.offending)
        )
      )
    case _ => None
  }
}

sealed trait PostgreSqlPhysicalPortfolioSelectionStage
    extends Product
    with Serializable {
  def id: String
}

object PostgreSqlPhysicalPortfolioSelectionStage {
  case object Enumeration extends PostgreSqlPhysicalPortfolioSelectionStage {
    override val id: String = "enumeration"
  }
  case object CandidatePlanning
      extends PostgreSqlPhysicalPortfolioSelectionStage {
    override val id: String = "candidate-planning"
  }
  case object StageDiagnosticRendering
      extends PostgreSqlPhysicalPortfolioSelectionStage {
    override val id: String = "stage-diagnostic-rendering"
  }
  case object WorkloadDeduplication
      extends PostgreSqlPhysicalPortfolioSelectionStage {
    override val id: String = "workload-deduplication"
  }
  case object RawRenderingResourceValidation
      extends PostgreSqlPhysicalPortfolioSelectionStage {
    override val id: String = "raw-rendering-resource-validation"
  }
  case object EvaluationEnvelopeValidation
      extends PostgreSqlPhysicalPortfolioSelectionStage {
    override val id: String = "evaluation-envelope-validation"
  }
  case object Evaluator extends PostgreSqlPhysicalPortfolioSelectionStage {
    override val id: String = "evaluator"
  }
  case object EvidenceValidation
      extends PostgreSqlPhysicalPortfolioSelectionStage {
    override val id: String = "evidence-validation"
  }
}

sealed trait PostgreSqlPhysicalPortfolioSelectionFailureKind
    extends Product
    with Serializable {
  def id: String
  def stage: PostgreSqlPhysicalPortfolioSelectionStage
}

object PostgreSqlPhysicalPortfolioSelectionFailureKind {
  import PostgreSqlPhysicalPortfolioSelectionStage._

  case object EnumerationFailed
      extends PostgreSqlPhysicalPortfolioSelectionFailureKind {
    override val id: String = "physical-portfolio.enumeration-failed"
    override val stage: PostgreSqlPhysicalPortfolioSelectionStage = Enumeration
  }
  case object CandidatePlanningFailed
      extends PostgreSqlPhysicalPortfolioSelectionFailureKind {
    override val id: String = "physical-portfolio.candidate-planning-failed"
    override val stage: PostgreSqlPhysicalPortfolioSelectionStage =
      CandidatePlanning
  }
  case object StageDiagnosticRenderingFailed
      extends PostgreSqlPhysicalPortfolioSelectionFailureKind {
    override val id: String =
      "physical-portfolio.stage-diagnostic-rendering-failed"
    override val stage: PostgreSqlPhysicalPortfolioSelectionStage =
      StageDiagnosticRendering
  }
  case object WorkloadIdentityCollision
      extends PostgreSqlPhysicalPortfolioSelectionFailureKind {
    override val id: String = "physical-portfolio.workload-identity-collision"
    override val stage: PostgreSqlPhysicalPortfolioSelectionStage =
      WorkloadDeduplication
  }
  case object RawRenderingResourceLimitExceeded
      extends PostgreSqlPhysicalPortfolioSelectionFailureKind {
    override val id: String =
      "physical-portfolio.raw-rendering-resource-limit-exceeded"
    override val stage: PostgreSqlPhysicalPortfolioSelectionStage =
      RawRenderingResourceValidation
  }
  case object EvaluationEnvelopeLimitExceeded
      extends PostgreSqlPhysicalPortfolioSelectionFailureKind {
    override val id: String =
      "physical-portfolio.evaluation-envelope-limit-exceeded"
    override val stage: PostgreSqlPhysicalPortfolioSelectionStage =
      EvaluationEnvelopeValidation
  }
  case object EvaluatorFailed
      extends PostgreSqlPhysicalPortfolioSelectionFailureKind {
    override val id: String = "physical-portfolio.evaluator-failed"
    override val stage: PostgreSqlPhysicalPortfolioSelectionStage = Evaluator
  }
  case object EvidenceInvalid
      extends PostgreSqlPhysicalPortfolioSelectionFailureKind {
    override val id: String = "physical-portfolio.evidence-invalid"
    override val stage: PostgreSqlPhysicalPortfolioSelectionStage =
      EvidenceValidation
  }
}

sealed trait PostgreSqlPhysicalPortfolioDecision
    extends Product
    with Serializable {
  def policyId: String =
    PostgreSqlExplainFactorAwarePhysicalPortfolioV1.PolicyId
  def scope: String = PostgreSqlExplainFactorAwarePhysicalPortfolioV1.Scope
  def evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits
}

object PostgreSqlPhysicalPortfolioDecision {
  final case class Selected private[postgresql] (
      enumeration: PostgreSqlPhysicalPortfolioEnumeration,
      rawRenderings: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      deduplication: PostgreSqlPhysicalPortfolioDeduplicationEvidence,
      explainRequest: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      evaluationUsage: PostgreSqlPhysicalPortfolioEvaluationUsage,
      explainBatch: PostgreSqlPhysicalPortfolioExplainBatch,
      evaluations: Vector[PostgreSqlPhysicalPortfolioEvaluationEvidence],
      selectedWorkloadId: PostgreSqlPhysicalPortfolioWorkloadId,
      selectedPhysicalVariantId: PostgreSqlPhysicalVariantId
  ) extends PostgreSqlPhysicalPortfolioDecision {
    require(
      PostgreSqlPhysicalPortfolioDecisionReplay.verifyComplete(
        enumeration,
        rawRenderings,
        deduplication,
        explainRequest,
        evaluationLimits,
        evaluationUsage,
        explainBatch,
        evaluations
      )
    )
    private val admissible = evaluations.filter(_.rowGuard.withinGuard)
    require(admissible.nonEmpty)
    private val winner = admissible.minBy(_.score)(
      PostgreSqlPhysicalPortfolioScore.ordering
    )
    require(selectedWorkloadId == winner.workload.descriptor.workloadId)
    require(
      selectedPhysicalVariantId ==
        winner.workload.representative.physicalVariantId
    )
  }

  final case class NoAdmissible private[postgresql] (
      enumeration: PostgreSqlPhysicalPortfolioEnumeration,
      rawRenderings: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      deduplication: PostgreSqlPhysicalPortfolioDeduplicationEvidence,
      explainRequest: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      evaluationUsage: PostgreSqlPhysicalPortfolioEvaluationUsage,
      explainBatch: PostgreSqlPhysicalPortfolioExplainBatch,
      evaluations: Vector[PostgreSqlPhysicalPortfolioEvaluationEvidence]
  ) extends PostgreSqlPhysicalPortfolioDecision {
    require(
      PostgreSqlPhysicalPortfolioDecisionReplay.verifyComplete(
        enumeration,
        rawRenderings,
        deduplication,
        explainRequest,
        evaluationLimits,
        evaluationUsage,
        explainBatch,
        evaluations
      )
    )
    require(evaluations.forall(!_.rowGuard.withinGuard))
  }

  final case class Failed private[postgresql] (
      failureKind: PostgreSqlPhysicalPortfolioSelectionFailureKind,
      detailCode: String,
      enumerationFailure: Option[
        PostgreSqlPhysicalPortfolioEnumerationFailure
      ],
      planningProblems: Vector[ConfiguredBagFactorPlanningError],
      renderingProblems: Vector[PostgreSqlRenderingError],
      failedEnumerationOrdinal: Option[Int],
      enumeration: Option[PostgreSqlPhysicalPortfolioEnumeration],
      rawRenderPrefix: Vector[
        PostgreSqlPhysicalPortfolioRawRenderingEvidence
      ],
      deduplication: Option[PostgreSqlPhysicalPortfolioDeduplicationEvidence],
      explainRequest: Option[PostgreSqlPhysicalPortfolioExplainBatchRequest],
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      evaluationUsage: Option[PostgreSqlPhysicalPortfolioEvaluationUsage],
      resourceExceeded: Option[
        PostgreSqlPhysicalPortfolioEvaluationResourceExceeded
      ],
      explainFailure: Option[PostgreSqlPhysicalPortfolioExplainFailure],
      explainBatch: Option[PostgreSqlPhysicalPortfolioExplainBatch],
      completedEvaluations: Vector[
        PostgreSqlPhysicalPortfolioEvaluationEvidence
      ],
      private[postgresql] val collisionWitness: Option[
        PostgreSqlPhysicalPortfolioCollisionWitness
      ] = None
  ) extends PostgreSqlPhysicalPortfolioDecision {
    require(failureKind != null)
    require(detailCode != null && detailCode.nonEmpty)
    require(enumerationFailure != null && enumerationFailure.forall(_ != null))
    require(planningProblems != null && planningProblems.forall(_ != null))
    require(renderingProblems != null && renderingProblems.forall(_ != null))
    require(failedEnumerationOrdinal != null && failedEnumerationOrdinal.forall(_ >= 0))
    require(enumeration != null && enumeration.forall(_ != null))
    require(rawRenderPrefix != null && rawRenderPrefix.forall(_ != null))
    require(deduplication != null && deduplication.forall(_ != null))
    require(explainRequest != null && explainRequest.forall(_ != null))
    require(evaluationLimits != null)
    require(evaluationUsage != null && evaluationUsage.forall(_ != null))
    require(resourceExceeded != null && resourceExceeded.forall(_ != null))
    require(explainFailure != null && explainFailure.forall(_ != null))
    require(explainBatch != null && explainBatch.forall(_ != null))
    require(completedEvaluations != null && completedEvaluations.forall(_ != null))
    require(collisionWitness != null && collisionWitness.forall(_ != null))
    require(
      collisionWitness.nonEmpty ==
        (failureKind == PostgreSqlPhysicalPortfolioSelectionFailureKind.WorkloadIdentityCollision),
      "only workload-identity collisions may retain full SQL evidence"
    )
    require(
      rawRenderPrefix.map(_.entry.enumerationOrdinal) ==
        rawRenderPrefix.indices.toVector,
      "portfolio failure must retain the exact successful raw-render prefix"
    )
    require(
      enumeration.forall(value =>
        rawRenderPrefix.zip(value.entries).forall { case (rendered, entry) =>
          rendered.entry == entry
        } && failedEnumerationOrdinal.forall(_ < value.entries.size)
      )
    )
    failureKind.stage match {
      case PostgreSqlPhysicalPortfolioSelectionStage.Enumeration =>
        require(
          enumerationFailure.nonEmpty &&
            detailCode == enumerationFailure.get.code &&
            planningProblems.isEmpty && renderingProblems.isEmpty &&
            enumeration.isEmpty && rawRenderPrefix.isEmpty &&
            failedEnumerationOrdinal.isEmpty && deduplication.isEmpty &&
            explainRequest.isEmpty && evaluationUsage.isEmpty &&
            resourceExceeded.isEmpty && explainFailure.isEmpty &&
            explainBatch.isEmpty &&
            completedEvaluations.isEmpty
        )
      case PostgreSqlPhysicalPortfolioSelectionStage.CandidatePlanning =>
        require(
          enumerationFailure.isEmpty && planningProblems.nonEmpty &&
            planningProblems ==
              ConfiguredBagFactorPlanningError.canonical(planningProblems) &&
            detailCode == planningProblems.head.code &&
            renderingProblems.isEmpty && enumeration.nonEmpty &&
            failedEnumerationOrdinal.contains(rawRenderPrefix.size) &&
            deduplication.isEmpty && explainRequest.isEmpty &&
            evaluationUsage.isEmpty && resourceExceeded.isEmpty &&
            explainFailure.isEmpty &&
            explainBatch.isEmpty && completedEvaluations.isEmpty
        )
      case PostgreSqlPhysicalPortfolioSelectionStage.StageDiagnosticRendering =>
        require(
          enumerationFailure.isEmpty && planningProblems.isEmpty &&
            renderingProblems.nonEmpty && renderingProblems ==
              PostgreSqlRenderingError.canonical(renderingProblems) &&
            detailCode == renderingProblems.head.code &&
            enumeration.nonEmpty &&
            failedEnumerationOrdinal.contains(rawRenderPrefix.size) &&
            deduplication.isEmpty && explainRequest.isEmpty &&
            evaluationUsage.isEmpty && resourceExceeded.isEmpty &&
            explainFailure.isEmpty && explainBatch.isEmpty &&
            completedEvaluations.isEmpty
        )
      case PostgreSqlPhysicalPortfolioSelectionStage.WorkloadDeduplication =>
        require(
          enumerationFailure.isEmpty && planningProblems.isEmpty &&
            renderingProblems.isEmpty && failureKind ==
            PostgreSqlPhysicalPortfolioSelectionFailureKind.WorkloadIdentityCollision &&
            detailCode == failureKind.id &&
            enumeration.nonEmpty &&
            rawRenderPrefix.size == enumeration.get.entries.size &&
            PostgreSqlPhysicalPortfolioDecisionReplay.verifyCollisionWitness(
              rawRenderPrefix,
              collisionWitness.get
            ) &&
            failedEnumerationOrdinal.contains(
              collisionWitness.get.offending.enumerationOrdinal
            ) && deduplication.isEmpty &&
            explainRequest.isEmpty && evaluationUsage.isEmpty &&
            resourceExceeded.isEmpty &&
            explainFailure.isEmpty && explainBatch.isEmpty &&
            completedEvaluations.isEmpty
        )
      case PostgreSqlPhysicalPortfolioSelectionStage.RawRenderingResourceValidation =>
        require(
          enumerationFailure.isEmpty && planningProblems.isEmpty &&
            renderingProblems.isEmpty && failureKind == PostgreSqlPhysicalPortfolioSelectionFailureKind.RawRenderingResourceLimitExceeded &&
            enumeration.nonEmpty && rawRenderPrefix.nonEmpty &&
            failedEnumerationOrdinal.contains(rawRenderPrefix.size - 1) &&
            deduplication.isEmpty && explainRequest.isEmpty &&
            evaluationUsage.isEmpty && resourceExceeded.nonEmpty &&
            detailCode == PostgreSqlPhysicalPortfolioDecisionReplay
              .resourceDetailCode(resourceExceeded.get.resource) &&
            PostgreSqlPhysicalPortfolioDecisionReplay
              .verifyRawRenderingResourceFailure(
                rawRenderPrefix,
                evaluationLimits,
                resourceExceeded.get
              ) &&
            explainFailure.isEmpty && explainBatch.isEmpty &&
            completedEvaluations.isEmpty
        )
      case PostgreSqlPhysicalPortfolioSelectionStage.EvaluationEnvelopeValidation =>
        require(
          enumerationFailure.isEmpty && planningProblems.isEmpty &&
            renderingProblems.isEmpty && failureKind == PostgreSqlPhysicalPortfolioSelectionFailureKind.EvaluationEnvelopeLimitExceeded &&
            enumeration.nonEmpty &&
            rawRenderPrefix.size == enumeration.get.entries.size &&
            failedEnumerationOrdinal.isEmpty && deduplication.nonEmpty &&
            explainRequest.nonEmpty && evaluationUsage.isEmpty &&
            resourceExceeded.nonEmpty && explainFailure.isEmpty &&
            explainBatch.isEmpty && completedEvaluations.isEmpty &&
            detailCode == PostgreSqlPhysicalPortfolioDecisionReplay
              .resourceDetailCode(resourceExceeded.get.resource) &&
            PostgreSqlPhysicalPortfolioDecisionReplay
              .verifyEvaluationResourceFailure(
                enumeration.get,
                rawRenderPrefix,
                deduplication.get,
                explainRequest.get,
                evaluationLimits,
                resourceExceeded.get
              )
        )
      case PostgreSqlPhysicalPortfolioSelectionStage.Evaluator =>
        require(
          enumerationFailure.isEmpty && planningProblems.isEmpty &&
            renderingProblems.isEmpty && enumeration.nonEmpty &&
            rawRenderPrefix.size == enumeration.get.entries.size &&
            failedEnumerationOrdinal.isEmpty && deduplication.nonEmpty &&
            explainRequest.nonEmpty && evaluationUsage.nonEmpty &&
            resourceExceeded.isEmpty && explainFailure.nonEmpty &&
            detailCode == explainFailure.get.code.id &&
            explainBatch.isEmpty &&
            completedEvaluations.isEmpty &&
            PostgreSqlPhysicalPortfolioDecisionReplay.verifyPreEvaluation(
              enumeration.get,
              rawRenderPrefix,
              deduplication.get,
              explainRequest.get,
              evaluationLimits,
              evaluationUsage.get
            ) &&
            PostgreSqlPhysicalPortfolioExplainReplay.verifyFailurePrefix(
              explainRequest.get,
              explainFailure.get
            )
        )
      case PostgreSqlPhysicalPortfolioSelectionStage.EvidenceValidation =>
        require(
          enumerationFailure.isEmpty && planningProblems.isEmpty &&
            renderingProblems.isEmpty && detailCode == failureKind.id &&
            enumeration.nonEmpty &&
            rawRenderPrefix.size == enumeration.get.entries.size &&
            failedEnumerationOrdinal.isEmpty && deduplication.nonEmpty &&
            explainRequest.nonEmpty && evaluationUsage.nonEmpty &&
            resourceExceeded.isEmpty && explainFailure.isEmpty &&
            explainBatch.nonEmpty &&
            completedEvaluations.size <= deduplication.get.groups.size &&
            PostgreSqlPhysicalPortfolioDecisionReplay.verifyPreEvaluation(
              enumeration.get,
              rawRenderPrefix,
              deduplication.get,
              explainRequest.get,
              evaluationLimits,
              evaluationUsage.get
            ) &&
            PostgreSqlPhysicalPortfolioExplainReplay
              .verify(explainRequest.get, explainBatch.get)
              .nonEmpty &&
            PostgreSqlPhysicalPortfolioDecisionReplay.verifyEvaluationPrefix(
              explainRequest.get,
              explainBatch.get,
              completedEvaluations
            )
        )
    }
  }
}

/** Pure complete-state verifier shared by constructors and v6 serialization. */
object PostgreSqlPhysicalPortfolioDecisionReplay {
  def verifyCandidateInputPipeline(
      hypergraph: QueryHypergraph,
      softGeneration: SoftBagGeneration,
      admissibleGeneration: AdmissibleSoftBagGeneration,
      candidateInput: CandidateSearchInput
  ): Boolean =
    try {
      if (
        hypergraph == null || softGeneration == null ||
        admissibleGeneration == null || candidateInput == null ||
        softGeneration.sourceHypergraph != hypergraph
      ) return false
      val replayedAdmissible =
        softGeneration.admissible(admissibleGeneration.requireConnectedCover)
      if (replayedAdmissible != admissibleGeneration) return false
      CandidateSearchInput
        .create(hypergraph, admissibleGeneration.widthBound, replayedAdmissible)
        .toOption
        .exists(replayed =>
          replayed.hypergraph == candidateInput.hypergraph &&
            replayed.widthBound == candidateInput.widthBound &&
            replayed.requireConnectedCover ==
              candidateInput.requireConnectedCover &&
            replayed.candidateBags == candidateInput.candidateBags
        )
    } catch {
      case NonFatal(_) => false
    }

  def verifySearch(
      candidateInput: CandidateSearchInput,
      candidateSearch: CandidatePlanDecision.Found,
      candidateResourceLimits: CandidateResourceLimits,
      physicalPortfolioLimits: PostgreSqlPhysicalPortfolioLimits,
      decision: PostgreSqlPhysicalPortfolioDecision
  ): Boolean =
    try {
      if (
        candidateInput == null || candidateSearch == null ||
        candidateResourceLimits == null || physicalPortfolioLimits == null ||
        decision == null ||
        candidateSearch.requestedLimit !=
          PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit ||
        PostgreSqlPhysicalPortfolioLimits
          .validate(physicalPortfolioLimits)
          .nonEmpty
      ) return false
      val structuralReplay = TopKCandidateTD
        .enumerate(
          candidateInput,
          PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit,
          candidateResourceLimits
        )
        .toOption
      if (!structuralReplay.contains(candidateSearch)) return false
      val physicalReplay = PostgreSqlPhysicalPortfolioEnumerator.enumerate(
        candidateInput,
        candidateSearch.plans,
        physicalPortfolioLimits
      )
      decision match {
        case selected: PostgreSqlPhysicalPortfolioDecision.Selected =>
          selected.enumeration.limits == physicalPortfolioLimits &&
            physicalReplay.contains(selected.enumeration)
        case noAdmissible: PostgreSqlPhysicalPortfolioDecision.NoAdmissible =>
          noAdmissible.enumeration.limits == physicalPortfolioLimits &&
            physicalReplay.contains(noAdmissible.enumeration)
        case failed: PostgreSqlPhysicalPortfolioDecision.Failed =>
          failed.enumeration match {
            case Some(enumeration) =>
              enumeration.limits == physicalPortfolioLimits &&
                physicalReplay.contains(enumeration)
            case None =>
              failed.failureKind ==
                PostgreSqlPhysicalPortfolioSelectionFailureKind.EnumerationFailed &&
                physicalReplay.left.toOption == failed.enumerationFailure
          }
      }
    } catch {
      case NonFatal(_) => false
    }

  private[postgresql] def verifyCollisionWitness(
      rawRenderings: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      witness: PostgreSqlPhysicalPortfolioCollisionWitness
  ): Boolean =
    try {
      if (
        rawRenderings == null || rawRenderings.exists(_ == null) ||
        witness == null
      ) return false
      def binds(
          rendering: PostgreSqlPhysicalPortfolioCollisionRendering
      ): Boolean =
        rawRenderings.lift(rendering.enumerationOrdinal).exists(raw =>
          raw.entry.enumerationOrdinal == rendering.enumerationOrdinal &&
            raw.workload == rendering.descriptor
        )
      binds(witness.prior) && binds(witness.offending)
    } catch {
      case NonFatal(_) => false
    }

  def resourceDetailCode(
      resource: PostgreSqlPhysicalPortfolioEvaluationResource
  ): String = {
    require(resource != null)
    s"physical-portfolio.resource.exceeded.${resource.code}"
  }

  def verifyRawRenderingResourceFailure(
      rawRenderPrefix: Vector[
        PostgreSqlPhysicalPortfolioRawRenderingEvidence
      ],
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      resourceExceeded: PostgreSqlPhysicalPortfolioEvaluationResourceExceeded
  ): Boolean =
    try {
      rawRenderPrefix.nonEmpty &&
      rawRenderingResourceExceeded(
        rawRenderPrefix.dropRight(1),
        evaluationLimits
      ).isEmpty &&
      rawRenderingResourceExceeded(rawRenderPrefix, evaluationLimits)
        .contains(resourceExceeded)
    } catch {
      case NonFatal(_) => false
    }

  private[postgresql] def rawRenderingResourceExceeded(
      rawRenderPrefix: Vector[
        PostgreSqlPhysicalPortfolioRawRenderingEvidence
      ],
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits
  ): Option[PostgreSqlPhysicalPortfolioEvaluationResourceExceeded] = {
      import PostgreSqlPhysicalPortfolioEvaluationResource._
      require(rawRenderPrefix != null && rawRenderPrefix.forall(_ != null))
      require(evaluationLimits != null)
      require(
        PostgreSqlPhysicalPortfolioEvaluationLimits
          .validate(evaluationLimits)
          .isEmpty,
        "physical portfolio evaluation limits must be valid"
      )
      var cumulativeCharacters = BigInt(0)
      var firstViolation = Option.empty[
        PostgreSqlPhysicalPortfolioEvaluationResourceExceeded
      ]
      var index = 0
      while (index < rawRenderPrefix.size && firstViolation.isEmpty) {
        val descriptor = rawRenderPrefix(index).workload
        val workloadId = Some(descriptor.workloadId)
        val stageCount = BigInt(descriptor.stages.size)
        val firstOversizedProbe = descriptor.stages.iterator
          .map(stage => BigInt(stage.characterCount))
          .find(_ > BigInt(evaluationLimits.maxCharactersPerProbe))
        val workloadCharacters = descriptor.stages.foldLeft(BigInt(0))(
          (total, stage) => total + BigInt(stage.characterCount)
        )
        firstViolation =
          if (stageCount > BigInt(evaluationLimits.maxStageProbesPerWorkload))
            Some(
            PostgreSqlPhysicalPortfolioEvaluationResourceExceeded(
              StageProbesPerWorkload,
              workloadId,
              stageCount,
              evaluationLimits.maxStageProbesPerWorkload
            )
            )
          else
            firstOversizedProbe.map(required =>
              PostgreSqlPhysicalPortfolioEvaluationResourceExceeded(
                CharactersPerProbe,
                workloadId,
                required,
                evaluationLimits.maxCharactersPerProbe
              )
              ).orElse(
              Option.when(
                workloadCharacters > BigInt(
                  evaluationLimits.maxDiagnosticCharactersPerWorkload
                )
              )(
                PostgreSqlPhysicalPortfolioEvaluationResourceExceeded(
                  DiagnosticCharactersPerWorkload,
                  workloadId,
                  workloadCharacters,
                  evaluationLimits.maxDiagnosticCharactersPerWorkload
                )
              )
            )
        if (firstViolation.isEmpty) {
          cumulativeCharacters += workloadCharacters
          firstViolation =
            Option.when(
              cumulativeCharacters > BigInt(
                evaluationLimits.maxRawDiagnosticCharactersRetained
              )
            )(
              PostgreSqlPhysicalPortfolioEvaluationResourceExceeded(
                RawDiagnosticCharactersRetained,
                workloadId,
                cumulativeCharacters,
                evaluationLimits.maxRawDiagnosticCharactersRetained
              )
            )
        }
        index += 1
      }
      firstViolation
  }

  private def verifyGrouping(
      enumeration: PostgreSqlPhysicalPortfolioEnumeration,
      rawRenderings: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      deduplication: PostgreSqlPhysicalPortfolioDeduplicationEvidence,
      explainRequest: PostgreSqlPhysicalPortfolioExplainBatchRequest
  ): Boolean = {
    if (
      enumeration == null || rawRenderings == null || deduplication == null ||
      explainRequest == null ||
      rawRenderings.size != enumeration.entries.size ||
      rawRenderings.zip(enumeration.entries).exists {
        case (rendering, entry) => rendering == null || rendering.entry != entry
      } || deduplication.rawVariantCount != enumeration.entries.size ||
      deduplication.groups != explainRequest.workloads.map(_.group)
    ) return false
    val rawByOrdinal = rawRenderings.map(value =>
      value.entry.enumerationOrdinal -> value
    ).toMap
    rawByOrdinal.size == rawRenderings.size &&
    deduplication.groups.forall(group =>
      group.aliases.forall(alias =>
        rawByOrdinal.get(alias.enumerationOrdinal).exists(raw =>
          raw.workload == group.descriptor &&
            raw.entry.variant.structuralRank == alias.structuralRank &&
            raw.entry.variant.candidateId == alias.candidateId &&
            raw.entry.variant.physicalVariantId == alias.physicalVariantId
        )
      )
    )
  }

  def verifyPreEvaluation(
      enumeration: PostgreSqlPhysicalPortfolioEnumeration,
      rawRenderings: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      deduplication: PostgreSqlPhysicalPortfolioDeduplicationEvidence,
      explainRequest: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      evaluationUsage: PostgreSqlPhysicalPortfolioEvaluationUsage
  ): Boolean =
    try {
      if (
        evaluationLimits == null || evaluationUsage == null ||
        !verifyGrouping(
          enumeration,
          rawRenderings,
          deduplication,
          explainRequest
        )
      ) return false
      PostgreSqlPhysicalPortfolioEvaluationEnvelope
        .validate(explainRequest, evaluationLimits)
        .toOption
        .contains(evaluationUsage)
    } catch {
      case NonFatal(_) => false
    }

  def verifyEvaluationResourceFailure(
      enumeration: PostgreSqlPhysicalPortfolioEnumeration,
      rawRenderings: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      deduplication: PostgreSqlPhysicalPortfolioDeduplicationEvidence,
      explainRequest: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      resourceExceeded: PostgreSqlPhysicalPortfolioEvaluationResourceExceeded
  ): Boolean =
    try {
      evaluationLimits != null && resourceExceeded != null &&
      verifyGrouping(
        enumeration,
        rawRenderings,
        deduplication,
        explainRequest
      ) && PostgreSqlPhysicalPortfolioEvaluationEnvelope
        .validate(explainRequest, evaluationLimits)
        .left
        .toOption
        .contains(resourceExceeded)
    } catch {
      case NonFatal(_) => false
    }

  def verifyEvaluationPrefix(
      explainRequest: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      explainBatch: PostgreSqlPhysicalPortfolioExplainBatch,
      evaluations: Vector[PostgreSqlPhysicalPortfolioEvaluationEvidence]
  ): Boolean =
    try {
      if (
        explainRequest == null || explainBatch == null || evaluations == null ||
        evaluations.exists(_ == null) ||
        evaluations.size > explainRequest.workloads.size
      ) return false
      val verified = PostgreSqlPhysicalPortfolioExplainReplay
        .verify(explainRequest, explainBatch)
        .getOrElse(return false)
      evaluations.zipWithIndex.forall { case (evaluation, index) =>
        val workload = explainRequest.workloads(index)
        val observations = verified.observationsByWorkload(index)
        evaluation.workload == workload.group &&
          evaluation.source == verified.source &&
          evaluation.stages.filter(_.trace.physicalForm.emitted).size ==
            observations.size &&
          evaluation.stages.filter(_.trace.physicalForm.emitted)
            .zip(observations)
            .forall { case (stage, observation) =>
              stage.trace.id == observation.stageId &&
                stage.diagnosticStatementSha256.contains(
                  observation.statementSha256
                ) && stage.estimate.contains(observation.estimate)
            }
      }
    } catch {
      case NonFatal(_) => false
    }

  def verifyComplete(
      enumeration: PostgreSqlPhysicalPortfolioEnumeration,
      rawRenderings: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      deduplication: PostgreSqlPhysicalPortfolioDeduplicationEvidence,
      explainRequest: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      evaluationUsage: PostgreSqlPhysicalPortfolioEvaluationUsage,
      explainBatch: PostgreSqlPhysicalPortfolioExplainBatch,
      evaluations: Vector[PostgreSqlPhysicalPortfolioEvaluationEvidence]
  ): Boolean =
    try {
      if (
        enumeration == null || rawRenderings == null || deduplication == null ||
        explainRequest == null || evaluationLimits == null ||
        evaluationUsage == null || explainBatch == null || evaluations == null
      ) return false
      verifyPreEvaluation(
        enumeration,
        rawRenderings,
        deduplication,
        explainRequest,
        evaluationLimits,
        evaluationUsage
      ) && evaluations.map(_.workload) == deduplication.groups &&
        evaluations.size == explainRequest.workloads.size &&
        verifyEvaluationPrefix(explainRequest, explainBatch, evaluations)
    } catch {
      case NonFatal(_) => false
    }
}

/**
  * Common replay context for every physical-portfolio terminal state.
  * Planning, rendering, grouping, and optimizer evaluation are all selection
  * internals, so failures and no-admissible outcomes retain this context even
  * though they have no selected rendering artifact.
  */
final case class PostgreSqlPhysicalPortfolioAttempt private[postgresql] (
    sourceSql: String,
    query: QueryIR,
    hypergraph: QueryHypergraph,
    softGeneration: SoftBagGeneration,
    admissibleGeneration: AdmissibleSoftBagGeneration,
    candidateInput: CandidateSearchInput,
    candidateSearch: CandidatePlanDecision.Found,
    candidateResourceLimits: CandidateResourceLimits,
    physicalPortfolioLimits: PostgreSqlPhysicalPortfolioLimits,
    renderingLimits: PostgreSqlRenderingLimits,
    bagFactorPlanningLimits: BagFactorPlanningLimits,
    evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
    structuralSearchExhaustedBeforeLimit: Boolean,
    decision: PostgreSqlPhysicalPortfolioDecision
) extends Serializable {
  require(sourceSql != null && sourceSql.nonEmpty)
  require(query != null && hypergraph != null)
  require(softGeneration != null && admissibleGeneration != null)
  require(candidateInput != null && candidateSearch != null)
  require(candidateResourceLimits != null)
  require(physicalPortfolioLimits != null)
  require(renderingLimits != null && bagFactorPlanningLimits != null)
  require(evaluationLimits != null && decision != null)
  require(
    try QueryHypergraphConverter.convert(query).contains(hypergraph)
    catch { case NonFatal(_) => false },
    "portfolio attempt QueryIR must reproduce the exact hypergraph"
  )
  require(
    PostgreSqlPhysicalPortfolioDecisionReplay.verifyCandidateInputPipeline(
      hypergraph,
      softGeneration,
      admissibleGeneration,
      candidateInput
    ),
    "portfolio attempt input pipeline must exactly replay"
  )
  require(
    candidateSearch.requestedLimit ==
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit &&
      structuralSearchExhaustedBeforeLimit ==
        (candidateSearch.plans.size < candidateSearch.requestedLimit),
    "portfolio attempt must bind exact K=100 structural exhaustion"
  )
  require(
    PostgreSqlPhysicalPortfolioDecisionReplay.verifySearch(
      candidateInput,
      candidateSearch,
      candidateResourceLimits,
      physicalPortfolioLimits,
      decision
    ),
    "portfolio attempt structural and physical search must exactly replay"
  )
  require(
    PostgreSqlPhysicalPortfolioSelectionReplay.verifyAttempt(this),
    "portfolio attempt planning/rendering/workload evidence must exactly replay"
  )
}

/** Separate v6 success artifact; legacy PostgreSqlRewriteArtifact is unchanged. */
final case class PostgreSqlPhysicalPortfolioArtifact private[postgresql] (
    attempt: PostgreSqlPhysicalPortfolioAttempt,
    selectedPlan: RankedCandidatePlan,
    configuredPlan: ConfiguredBagFactorPlan,
    renderedQuery: PostgreSqlRenderedQuery
) extends Serializable {
  require(attempt != null)
  private val query = attempt.query
  private val hypergraph = attempt.hypergraph
  private val softGeneration = attempt.softGeneration
  private val admissibleGeneration = attempt.admissibleGeneration
  private val candidateInput = attempt.candidateInput
  private val candidateSearch = attempt.candidateSearch
  private val candidateResourceLimits = attempt.candidateResourceLimits
  private val physicalPortfolioLimits = attempt.physicalPortfolioLimits
  private val renderingLimits = attempt.renderingLimits
  private val bagFactorPlanningLimits = attempt.bagFactorPlanningLimits
  private val structuralSearchExhaustedBeforeLimit =
    attempt.structuralSearchExhaustedBeforeLimit
  private val decision = attempt.decision match {
    case value: PostgreSqlPhysicalPortfolioDecision.Selected => value
    case _ =>
      throw new IllegalArgumentException(
        "portfolio success artifact requires a selected attempt"
      )
  }
  require(query != null)
  require(hypergraph != null)
  require(softGeneration != null)
  require(admissibleGeneration != null)
  require(candidateInput != null)
  require(candidateSearch != null)
  require(candidateResourceLimits != null)
  require(
    physicalPortfolioLimits != null &&
      PostgreSqlPhysicalPortfolioLimits.validate(physicalPortfolioLimits).isEmpty
  )
  require(renderingLimits != null)
  require(bagFactorPlanningLimits != null)
  require(decision != null)
  require(selectedPlan != null)
  require(configuredPlan != null)
  require(renderedQuery != null)
  require(
    PostgreSqlPhysicalPortfolioDecisionReplay.verifyCandidateInputPipeline(
      hypergraph,
      softGeneration,
      admissibleGeneration,
      candidateInput
    ),
    "portfolio artifact soft/admissible/candidate input pipeline must replay"
  )
  require(
    candidateSearch.plans.map(_.rank) ==
      (1 to candidateSearch.plans.size).toVector &&
      candidateSearch.plans.map(_.candidateId).distinct.size ==
        candidateSearch.plans.size &&
      candidateSearch.plans.forall(plan =>
        plan.candidateId == CandidatePlanId.from(plan.decomposition)
      )
  )
  require(candidateSearch.plans.count(_ == selectedPlan) == 1)
  require(
    candidateSearch.requestedLimit ==
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.RequiredPlanLimit,
    "portfolio artifact must bind the exact structural K=100 request"
  )
  require(
    structuralSearchExhaustedBeforeLimit ==
      (candidateSearch.plans.size < candidateSearch.requestedLimit),
    "portfolio artifact must record structural exhaustion before K=100"
  )
  require(
    PostgreSqlPhysicalPortfolioDecisionReplay.verifySearch(
      candidateInput,
      candidateSearch,
      candidateResourceLimits,
      physicalPortfolioLimits,
      decision
    ),
    "portfolio artifact structural and physical search must exactly replay"
  )
  private val selectedEntry = decision.enumeration.entries
    .find(
      _.variant.physicalVariantId == decision.selectedPhysicalVariantId
    )
    .getOrElse(
      throw new IllegalArgumentException(
        "portfolio artifact selected variant must be enumerated"
      )
    )
  require(
    selectedEntry.variant.candidateId == selectedPlan.candidateId &&
      selectedEntry.variant.structuralRank == selectedPlan.rank
  )
  require(
    configuredPlan.structuralDecomposition == selectedPlan.decomposition &&
      configuredPlan.structuralCertificate == selectedPlan.certificate &&
      configuredPlan.configuration == selectedEntry.variant.configuration
  )
  require(
    renderedQuery.sourceQuery == query &&
      renderedQuery.bagFactorPlan == configuredPlan.plan &&
      renderedQuery.trace.reduction.mode == selectedEntry.variant.reductionMode &&
      renderedQuery.trace.reduction.reductionOrder ==
        selectedEntry.variant.reductionOrder &&
      configuredPlan.plan.sourceHypergraph == hypergraph
  )
  require(
    renderedQuery.trace.stages.filter(_.physicalForm.namedCte).forall(stage =>
      PostgreSqlPhysicalPortfolioWorkloadDescriptor
        .expectedPhysicalForm(selectedEntry.variant.materialization)
        .contains(stage.physicalForm)
    ),
    "portfolio artifact named CTE forms must match the selected directive"
  )
  private val selectedEvaluation = decision.evaluations
    .find(
      _.workload.descriptor.workloadId == decision.selectedWorkloadId
    )
    .getOrElse(
      throw new IllegalArgumentException(
        "portfolio artifact selected workload must have evaluation evidence"
      )
    )
  require(
    selectedEvaluation.workload.representative.physicalVariantId ==
      decision.selectedPhysicalVariantId &&
      selectedEvaluation.stages.map(_.trace) == renderedQuery.trace.stages
  )
  private val selectedRequest = decision.explainRequest.workloads
    .find(_.group.descriptor.workloadId == decision.selectedWorkloadId)
    .getOrElse(
      throw new IllegalArgumentException(
        "portfolio artifact selected workload must occur in evaluator request"
      )
    )
  require(
    selectedRequest.stages.last.sql == renderedQuery.sql &&
      selectedRequest.group.descriptor.renderedStatementSha256 ==
        PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
          renderedQuery.sql
        )
  )
  private val replayedSelectedRendering = PostgreSqlBagFactorRenderer
    .renderConfiguredWithStageDiagnostics(
      query,
      candidateInput,
      configuredPlan,
      renderingLimits,
      bagFactorPlanningLimits,
      selectedEntry.variant.reductionMode,
      selectedEntry.variant.materialization,
      decision.evaluationLimits.stageDiagnosticLimits
    )
    .fold(
      problems =>
        throw new IllegalArgumentException(
          s"portfolio artifact selected rendering replay failed: ${problems.map(_.code).mkString(",")}"
        ),
      identity
    )
  require(
    replayedSelectedRendering.renderedQuery == renderedQuery,
    "portfolio artifact normal rendering must exactly regenerate"
  )
  require(
    PostgreSqlPhysicalPortfolioWorkloadDescriptor.from(
      replayedSelectedRendering,
      selectedEntry.variant.materialization
    ) == selectedRequest.group.descriptor,
    "portfolio artifact selected workload descriptor must regenerate"
  )
  require(
    replayedSelectedRendering.statements.size == selectedRequest.stages.size &&
      replayedSelectedRendering.statements.zip(selectedRequest.stages).forall {
        case (replayed, requested) =>
          replayed.stageId == requested.stageId &&
            replayed.sql == requested.sql &&
            replayed.statementSha256 == requested.statementSha256 &&
            replayed.characterCount == requested.characterCount &&
            replayed.ctePrefixCount == requested.ctePrefixCount
      },
    "portfolio artifact selected diagnostic workload must exactly regenerate"
  )
}
