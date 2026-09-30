package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator

class PostgreSqlPhysicalPortfolioDecisionReplayContractSuite
    extends munit.FunSuite {
  import PostgreSqlPhysicalPortfolioContractTestSupport._
  import PostgreSqlPhysicalPortfolioSelectionFailureKind._

  test("selected and no-admissible decisions replay complete evidence and exact guards") {
    val selectedFixture = decisionFixture()
    val selected = PostgreSqlPhysicalPortfolioDecision.Selected(
      selectedFixture.enumeration,
      selectedFixture.rawRenderings,
      selectedFixture.deduplication,
      selectedFixture.explainRequest,
      selectedFixture.evaluationLimits,
      selectedFixture.evaluationUsage,
      selectedFixture.explainBatch,
      Vector(selectedFixture.evaluation),
      selectedFixture.evaluation.workload.descriptor.workloadId,
      selectedFixture.evaluation.workload.representative.physicalVariantId
    )
    assertEquals(
      selected.selectedPhysicalVariantId,
      selectedFixture.evaluation.score.representativePhysicalVariantId
    )
    intercept[IllegalArgumentException] {
      selected.copy(
        selectedPhysicalVariantId =
          fixture(2).group.representative.physicalVariantId
      )
    }
    intercept[IllegalArgumentException] {
      selected.copy(selectedWorkloadId = fixture(2).group.descriptor.workloadId)
    }

    val rejectedFixture = decisionFixture(terminalRows = BigDecimal(1001))
    val noAdmissible = PostgreSqlPhysicalPortfolioDecision.NoAdmissible(
      rejectedFixture.enumeration,
      rejectedFixture.rawRenderings,
      rejectedFixture.deduplication,
      rejectedFixture.explainRequest,
      rejectedFixture.evaluationLimits,
      rejectedFixture.evaluationUsage,
      rejectedFixture.explainBatch,
      Vector(rejectedFixture.evaluation)
    )
    assert(noAdmissible.evaluations.forall(!_.rowGuard.withinGuard))
    intercept[IllegalArgumentException] {
      noAdmissible.copy(evaluations = Vector(selectedFixture.evaluation))
    }
    intercept[IllegalArgumentException] {
      selected.copy(
        evaluations = Vector(rejectedFixture.evaluation),
        explainBatch = rejectedFixture.explainBatch
      )
    }
  }

  test("complete replay cross-binds enumeration, raw descriptors, grouping, request, usage, batch, and evaluation prefix") {
    val value = decisionFixture()
    intercept[IllegalArgumentException] {
      value.enumeration.copy(
        candidates = value.enumeration.candidates.map(
          _.copy(candidateId = candidateId(2))
        )
      )
    }
    assert(
      PostgreSqlPhysicalPortfolioDecisionReplay.verifyComplete(
        value.enumeration,
        value.rawRenderings,
        value.deduplication,
        value.explainRequest,
        value.evaluationLimits,
        value.evaluationUsage,
        value.explainBatch,
        Vector(value.evaluation)
      )
    )
    val wrongDescriptor = fixture(2).group.descriptor
    val wrongRaw = value.rawRenderings.updated(
      0,
      value.rawRenderings.head.copy(workload = wrongDescriptor)
    )
    assert(
      !PostgreSqlPhysicalPortfolioDecisionReplay.verifyPreEvaluation(
        value.enumeration,
        wrongRaw,
        value.deduplication,
        value.explainRequest,
        value.evaluationLimits,
        value.evaluationUsage
      )
    )
    assert(
      !PostgreSqlPhysicalPortfolioDecisionReplay.verifyPreEvaluation(
        value.enumeration,
        value.rawRenderings.reverse,
        value.deduplication,
        value.explainRequest,
        value.evaluationLimits,
        value.evaluationUsage
      )
    )
    assert(
      !PostgreSqlPhysicalPortfolioDecisionReplay.verifyPreEvaluation(
        value.enumeration,
        value.rawRenderings,
        value.deduplication,
        value.explainRequest,
        value.evaluationLimits,
        value.evaluationUsage.copy(
          diagnosticCharacterCount =
            value.evaluationUsage.diagnosticCharacterCount + 1
        )
      )
    )
    val incompatibleEvaluation = decisionFixture(BigDecimal(1001)).evaluation
    assert(
      !PostgreSqlPhysicalPortfolioDecisionReplay.verifyEvaluationPrefix(
        value.explainRequest,
        value.explainBatch,
        Vector(incompatibleEvaluation)
      )
    )
  }

  test("evaluator failure retains and verifies the exact completed optimizer prefix") {
    val value = decisionFixture()
    val completedCount = 2
    val explainFailure = PostgreSqlPhysicalPortfolioExplainFailure(
      PostgreSqlPhysicalPortfolioExplainFailureCode.StatementFailed,
      PostgreSqlPhysicalPortfolioExplainStage.StageExplain,
      Some(completedCount),
      Some(value.explainBatch.source),
      value.explainBatch.observations.take(completedCount),
      value.explainRequest.flattened.size,
      None,
      None
    )
    val failed = PostgreSqlPhysicalPortfolioDecision.Failed(
      EvaluatorFailed,
      explainFailure.code.id,
      None,
      Vector.empty,
      Vector.empty,
      None,
      Some(value.enumeration),
      value.rawRenderings,
      Some(value.deduplication),
      Some(value.explainRequest),
      value.evaluationLimits,
      Some(value.evaluationUsage),
      None,
      Some(explainFailure),
      None,
      Vector.empty
    )
    assertEquals(failed.explainFailure, Some(explainFailure))

    val mutated = explainFailure.copy(
      completed = explainFailure.completed.updated(
        0,
        explainFailure.completed.head.copy(statementSha256 = "a" * 64)
      )
    )
    intercept[IllegalArgumentException] {
      failed.copy(explainFailure = Some(mutated))
    }
    intercept[IllegalArgumentException] {
      failed.copy(
        evaluationUsage = Some(
          value.evaluationUsage.copy(
            explainStatementCount =
              value.evaluationUsage.explainStatementCount + 1
          )
        )
      )
    }
  }

  test("evidence-validation failure accepts only an exact evaluation prefix") {
    val value = decisionFixture()
    val failed = PostgreSqlPhysicalPortfolioDecision.Failed(
      EvidenceInvalid,
      EvidenceInvalid.id,
      None,
      Vector.empty,
      Vector.empty,
      None,
      Some(value.enumeration),
      value.rawRenderings,
      Some(value.deduplication),
      Some(value.explainRequest),
      value.evaluationLimits,
      Some(value.evaluationUsage),
      None,
      None,
      Some(value.explainBatch),
      Vector(value.evaluation)
    )
    assertEquals(failed.completedEvaluations, Vector(value.evaluation))

    val incompatible = decisionFixture(BigDecimal(1001)).evaluation
    intercept[IllegalArgumentException] {
      failed.copy(completedEvaluations = Vector(incompatible))
    }
    val swappedBatch = value.explainBatch.copy(
      observations = value.explainBatch.observations.updated(
        0,
        value.explainBatch.observations(1)
      ).updated(1, value.explainBatch.observations(0))
    )
    intercept[IllegalArgumentException] {
      failed.copy(explainBatch = Some(swappedBatch))
    }
  }

  test("early failure states enforce exact raw prefixes and complete-render collision ordinals") {
    val value = decisionFixture()
    val enumerationProblem =
      PostgreSqlPhysicalPortfolioEnumerationFailure.MissingValue("fixture")
    val enumerationFailure = PostgreSqlPhysicalPortfolioDecision.Failed(
      EnumerationFailed,
      enumerationProblem.code,
      Some(enumerationProblem),
      Vector.empty,
      Vector.empty,
      None,
      None,
      Vector.empty,
      None,
      None,
      value.evaluationLimits,
      None,
      None,
      None,
      None,
      Vector.empty
    )
    assertEquals(enumerationFailure.enumeration, None)

    val planningProblem =
      at.ac.tuwien.dbai.yannakakis.algorithm.lowering.ConfiguredBagFactorPlanningError
        .MissingValue("fixture")
    val planningFailure = PostgreSqlPhysicalPortfolioDecision.Failed(
      CandidatePlanningFailed,
      planningProblem.code,
      None,
      Vector(planningProblem),
      Vector.empty,
      Some(0),
      Some(value.enumeration),
      Vector.empty,
      None,
      None,
      value.evaluationLimits,
      None,
      None,
      None,
      None,
      Vector.empty
    )
    assertEquals(planningFailure.failedEnumerationOrdinal, Some(0))

    val renderingProblem =
      at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlRenderingError
        .MissingValue("fixture")
    val renderingFailure = PostgreSqlPhysicalPortfolioDecision.Failed(
      StageDiagnosticRenderingFailed,
      renderingProblem.code,
      None,
      Vector.empty,
      Vector(renderingProblem),
      Some(1),
      Some(value.enumeration),
      value.rawRenderings.take(1),
      None,
      None,
      value.evaluationLimits,
      None,
      None,
      None,
      None,
      Vector.empty
    )
    assertEquals(renderingFailure.rawRenderPrefix.size, 1)

    val orderedSql = fixture().statements.map(_.sql)
    val firstRendering = PostgreSqlPhysicalPortfolioCollisionRendering(
      0,
      value.rawRenderings.head.workload,
      orderedSql
    )
    val identicalAlias = PostgreSqlPhysicalPortfolioCollisionRendering(
      1,
      value.rawRenderings(1).workload,
      orderedSql
    )
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalPortfolioCollisionWitness(
        firstRendering,
        identicalAlias
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalPortfolioCollisionRendering(
        0,
        value.rawRenderings.head.workload,
        orderedSql.updated(0, orderedSql.head + " ")
      )
    }
    Vector(0, 1).foreach { forgedOrdinal =>
      intercept[IllegalArgumentException] {
        PostgreSqlPhysicalPortfolioDecision.Failed(
          WorkloadIdentityCollision,
          WorkloadIdentityCollision.id,
          None,
          Vector.empty,
          Vector.empty,
          Some(forgedOrdinal),
          Some(value.enumeration),
          value.rawRenderings,
          None,
          None,
          value.evaluationLimits,
          None,
          None,
          None,
          None,
          Vector.empty
        )
      }
    }
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalPortfolioDecision.Failed(
        WorkloadIdentityCollision,
        WorkloadIdentityCollision.id,
        None,
        Vector.empty,
        Vector.empty,
        Some(0),
        Some(value.enumeration),
        value.rawRenderings.take(1),
        None,
        None,
        value.evaluationLimits,
        None,
        None,
        None,
        None,
        Vector.empty
      )
    }
  }

  test("raw-render and evaluation-envelope resource failures replay distinct exact states") {
    import PostgreSqlPhysicalPortfolioEvaluationResource._
    val value = decisionFixture()
    val rawLimits = PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(maxStageProbesPerWorkload = 4L)
      .fold(problems => fail(problems.map(_.code).mkString(",")), identity)
    val rawExceeded = PostgreSqlPhysicalPortfolioEvaluationResourceExceeded(
      StageProbesPerWorkload,
      Some(value.rawRenderings.head.workload.workloadId),
      required = BigInt(5),
      limit = 4L
    )
    val rawFailure = PostgreSqlPhysicalPortfolioDecision.Failed(
      RawRenderingResourceLimitExceeded,
      PostgreSqlPhysicalPortfolioDecisionReplay.resourceDetailCode(
        StageProbesPerWorkload
      ),
      None,
      Vector.empty,
      Vector.empty,
      Some(0),
      Some(value.enumeration),
      value.rawRenderings.take(1),
      None,
      None,
      rawLimits,
      None,
      Some(rawExceeded),
      None,
      None,
      Vector.empty
    )
    assertEquals(rawFailure.resourceExceeded, Some(rawExceeded))
    intercept[IllegalArgumentException] {
      rawFailure.copy(
        failedEnumerationOrdinal = Some(1),
        rawRenderPrefix = value.rawRenderings
      )
    }
    intercept[IllegalArgumentException] {
      rawFailure.copy(
        resourceExceeded = Some(rawExceeded.copy(required = BigInt(6)))
      )
    }

    val retainedCharacters = value.rawRenderings.head.workload.stages
      .foldLeft(BigInt(0))((total, stage) => total + stage.characterCount)
    val largestProbe = value.rawRenderings.head.workload.stages
      .map(_.characterCount)
      .max
    val cumulativeRawLimits = PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(
        maxCharactersPerProbe = largestProbe,
        maxDiagnosticCharactersPerWorkload = retainedCharacters.toLong,
        maxRawDiagnosticCharactersRetained = retainedCharacters.toLong
      )
      .fold(problems => fail(problems.map(_.code).mkString(",")), identity)
    val cumulativeExceeded =
      PostgreSqlPhysicalPortfolioEvaluationResourceExceeded(
        RawDiagnosticCharactersRetained,
        Some(value.rawRenderings(1).workload.workloadId),
        required = retainedCharacters * 2,
        limit = retainedCharacters.toLong
      )
    val cumulativeFailure = PostgreSqlPhysicalPortfolioDecision.Failed(
      RawRenderingResourceLimitExceeded,
      PostgreSqlPhysicalPortfolioDecisionReplay.resourceDetailCode(
        RawDiagnosticCharactersRetained
      ),
      None,
      Vector.empty,
      Vector.empty,
      Some(1),
      Some(value.enumeration),
      value.rawRenderings,
      None,
      None,
      cumulativeRawLimits,
      None,
      Some(cumulativeExceeded),
      None,
      None,
      Vector.empty
    )
    assertEquals(cumulativeFailure.resourceExceeded, Some(cumulativeExceeded))
    intercept[IllegalArgumentException] {
      cumulativeFailure.copy(
        failedEnumerationOrdinal = Some(0),
        rawRenderPrefix = value.rawRenderings.take(1)
      )
    }

    val misleadingStatementExceeded =
      PostgreSqlPhysicalPortfolioEvaluationResourceExceeded(
        ExplainStatementsPerSession,
        Some(value.rawRenderings(1).workload.workloadId),
        required = BigInt(cumulativeRawLimits.maxExplainStatementsPerSession) + 1,
        limit = cumulativeRawLimits.maxExplainStatementsPerSession
      )
    assert(
      !PostgreSqlPhysicalPortfolioDecisionReplay
        .verifyRawRenderingResourceFailure(
          value.rawRenderings,
          cumulativeRawLimits,
          misleadingStatementExceeded
        )
    )
    assertEquals(
      value.evaluationUsage.explainStatementCount,
      BigInt(1 + value.explainRequest.flattened.size)
    )
    assertNotEquals(
      value.evaluationUsage.explainStatementCount,
      BigInt(
        1 + value.rawRenderings.size *
          value.rawRenderings.head.workload.stages.size
      )
    )

    val envelopeLimits = PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(
        maxExplainStatementsPerSession =
          value.explainRequest.flattened.size.toLong
      )
      .fold(problems => fail(problems.map(_.code).mkString(",")), identity)
    val envelopeExceeded = PostgreSqlPhysicalPortfolioEvaluationEnvelope
      .validate(value.explainRequest, envelopeLimits)
      .left
      .toOption
      .get
    val envelopeFailure = PostgreSqlPhysicalPortfolioDecision.Failed(
      EvaluationEnvelopeLimitExceeded,
      PostgreSqlPhysicalPortfolioDecisionReplay.resourceDetailCode(
        envelopeExceeded.resource
      ),
      None,
      Vector.empty,
      Vector.empty,
      None,
      Some(value.enumeration),
      value.rawRenderings,
      Some(value.deduplication),
      Some(value.explainRequest),
      envelopeLimits,
      None,
      Some(envelopeExceeded),
      None,
      None,
      Vector.empty
    )
    assertEquals(envelopeFailure.resourceExceeded, Some(envelopeExceeded))
    intercept[IllegalArgumentException] {
      envelopeFailure.copy(
        resourceExceeded = Some(
          envelopeExceeded.copy(required = envelopeExceeded.required + 1)
        )
      )
    }
    intercept[IllegalArgumentException] {
      envelopeFailure.copy(deduplication = None)
    }
  }

  test("soft generation, admissibility projection, and candidate input replay as one exact pipeline") {
    val prepared = PostgreSqlFactorAwareContractTestSupport.triangle
    val hypergraph = prepared.input.hypergraph
    val softGeneration = SoftBagGenerator
      .generate(hypergraph, prepared.input.widthBound)
      .fold(problem => fail(problem.message), identity)
    val admissible = softGeneration.admissible(
      prepared.input.requireConnectedCover
    )
    assert(
      PostgreSqlPhysicalPortfolioDecisionReplay.verifyCandidateInputPipeline(
        hypergraph,
        softGeneration,
        admissible,
        prepared.input
      )
    )

    val otherAdmissibility = softGeneration.admissible(
      !prepared.input.requireConnectedCover
    )
    val otherInput = CandidateSearchInput
      .create(hypergraph, prepared.input.widthBound, otherAdmissibility)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    assert(
      !PostgreSqlPhysicalPortfolioDecisionReplay.verifyCandidateInputPipeline(
        hypergraph,
        softGeneration,
        admissible,
        otherInput
      )
    )
    assert(
      !PostgreSqlPhysicalPortfolioDecisionReplay.verifyCandidateInputPipeline(
        hypergraph,
        softGeneration,
        otherAdmissibility,
        prepared.input
      )
    )
  }
}
