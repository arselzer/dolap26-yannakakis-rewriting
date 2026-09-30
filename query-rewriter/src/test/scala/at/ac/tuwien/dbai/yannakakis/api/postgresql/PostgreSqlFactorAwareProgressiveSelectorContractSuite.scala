package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateResourceLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlRenderingLimits,
  PostgreSqlStageId,
  PostgreSqlStagePhysicalForm
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  YannakakisReductionMode
}

class PostgreSqlFactorAwareProgressiveSelectorContractSuite
    extends munit.FunSuite {
  import PostgreSqlFactorAwareContractTestSupport._

  private val policy =
    CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1

  test("genuine top-100 selection evaluates every candidate and binds every emitted or elided stage") {
    val hugeRows = BigDecimal(
      "1234567890123456789012345678901234567890.12345"
    )
    val hugeProduct = PostgreSqlFactorAwareScore.exactEstimatedBytes(
      hugeRows,
      Long.MaxValue
    )
    val evaluator = new RecordingFactorEvaluator((request, _) =>
      Right(
        successfulBatch(request, sourceRows = BigDecimal(100)) {
          (candidate, statement, _) =>
            if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
              estimate(
                totalCost = BigDecimal(101 - candidate.structuralRank),
                rows = BigDecimal(100),
                width = 8
              )
            else if (candidate.structuralRank == 1)
              estimate(rows = hugeRows, width = Long.MaxValue)
            else estimate(rows = BigDecimal(0), width = Long.MaxValue)
        }
      )
    )

    val selected = select(q5, Some(evaluator))
      .fold(problem => fail(problem.toString), identity)
    val decision = selected.decision
    val request = evaluator.factorRequests.head

    assertEquals(evaluator.factorCalls, 1)
    assertEquals(evaluator.legacyCalls, 0)
    assertEquals(request.candidates.size, 100)
    assertEquals(request.candidates.map(_.structuralRank), (1 to 100).toVector)
    assertEquals(decision.evaluations.map(_.structuralRank), (1 to 100).toVector)
    assertEquals(selected.plan.rank, 100)
    assertEquals(decision.selectedStructuralRank, 100)
    assertEquals(decision.factorAwareSelectionLimits, Some(defaultFactorLimits))

    assertEquals(
      decision.factorAwareCheckpoints.map(_.requestedPrefix),
      Vector(10, 25, 50, 100)
    )
    assertEquals(
      decision.factorAwareCheckpoints.map(_.effectiveCandidateCount),
      Vector(10, 25, 50, 100)
    )
    assert(decision.factorAwareCheckpoints.forall(!_.truncatedBySearchExhaustion))
    assertEquals(
      decision.factorAwareCheckpoints.map(_.winnerStructuralRank),
      Vector(10, 25, 50, 100).map(Some(_))
    )
    decision.factorAwareCheckpoints.foreach { checkpoint =>
      assertEquals(
        checkpoint.verifiedPrefixSha256,
        PostgreSqlFactorAwarePrefixProof.digest(
          q5.found.plans.take(checkpoint.effectiveCandidateCount)
        )
      )
      assertEquals(
        checkpoint.prefixProofScheme,
        PostgreSqlFactorAwarePrefixProof.Scheme
      )
    }
    val source = decision.sourceEvaluation.getOrElse(
      fail("factor-aware success lost its source baseline")
    )
    assert(
      PostgreSqlFactorAwareSelectionReplay.verify(
        q5.found.plans,
        decision.evaluations,
        source,
        decision.factorAwareCheckpoints,
        defaultFactorLimits,
        Some(statementSha(q5.sql))
      )
    )
    val firstCheckpoint = decision.factorAwareCheckpoints.head
    val rankOneEvaluation = decision.evaluations.head
    val rankNineEvaluation = decision.evaluations(8)
    val checkpointMutations = Vector(
      firstCheckpoint.copy(verifiedPrefixSha256 = "0" * 64),
      firstCheckpoint.copy(
        effectiveCandidateCount = firstCheckpoint.effectiveCandidateCount - 1,
        truncatedBySearchExhaustion = true,
        winnerStructuralRank = Some(rankNineEvaluation.structuralRank),
        winnerCandidateId = Some(rankNineEvaluation.candidateId),
        winnerPhysicalVariantId = Some(rankNineEvaluation.physicalVariantId),
        winnerScore = rankNineEvaluation.factorAwareEvidence.map(_.score)
      ),
      firstCheckpoint.copy(
        winnerStructuralRank = Some(rankOneEvaluation.structuralRank),
        winnerCandidateId = Some(rankOneEvaluation.candidateId),
        winnerPhysicalVariantId = Some(rankOneEvaluation.physicalVariantId),
        winnerScore = rankOneEvaluation.factorAwareEvidence.map(_.score)
      )
    )
    checkpointMutations.foreach { mutation =>
      assert(
        !PostgreSqlFactorAwareSelectionReplay.verify(
          q5.found.plans,
          decision.evaluations,
          source,
          decision.factorAwareCheckpoints.updated(0, mutation),
          defaultFactorLimits,
          Some(statementSha(q5.sql))
        )
      )
    }

    var sawElided = false
    decision.evaluations.zip(request.candidates).foreach {
      case (evaluation, requested) =>
        val evidence = evaluation.factorAwareEvidence.getOrElse(
          fail(s"candidate ${evaluation.structuralRank} has no factor evidence")
        )
        val emitted = evidence.stages.filter(_.trace.physicalForm.emitted)
        val elided = evidence.stages.filterNot(_.trace.physicalForm.emitted)
        sawElided ||= elided.nonEmpty

        assertEquals(emitted.map(_.trace.id), requested.stages.map(_.stageId))
        emitted.zip(requested.stages).foreach { case (stage, statement) =>
          assertEquals(stage.diagnosticStatementSha256, Some(statement.statementSha256))
          assert(stage.diagnosticCharacterCount.nonEmpty)
          assert(stage.diagnosticCtePrefixCount.nonEmpty)
          assert(stage.estimate.nonEmpty)
        }
        elided.foreach { stage =>
          assertEquals(stage.trace.physicalForm, PostgreSqlStagePhysicalForm.Elided)
          assertEquals(stage.diagnosticStatementSha256, None)
          assertEquals(stage.diagnosticCharacterCount, None)
          assertEquals(stage.diagnosticCtePrefixCount, None)
          assertEquals(stage.estimate, None)
        }
        assertEquals(evidence.stages.last.trace.id, PostgreSqlStageId.FinalReconstruction)
        assertEquals(
          requested.stages.last.statementSha256,
          evaluation.renderedStatementSha256
        )
        assertEquals(
          evidence.stages.last.diagnosticStatementSha256,
          Some(evaluation.renderedStatementSha256)
        )
    }
    assert(sawElided, "the Q5 fixture must exercise explicit trace elision")

    val rankOne = decision.evaluations.head.factorAwareEvidence.get
    val rankOneNamedCount = rankOne.stages.count(_.trace.physicalForm.namedCte)
    assert(rankOneNamedCount > 0)
    assertEquals(
      rankOne.score.maxNamedStagePlanRowsTimesPlanWidth,
      hugeProduct
    )
    assertEquals(
      rankOne.score.sumNamedStagePlanRowsTimesPlanWidth,
      PostgreSqlFactorAwareScore.exactSum(
        Vector.fill(rankOneNamedCount)(hugeProduct)
      )
    )
  }

  test("exhausted search checkpoints preserve the exact inclusive final 10x guard") {
    val terminalRows = Vector(
      BigDecimal("9.999999999999999999"),
      BigDecimal("10"),
      BigDecimal("1000.000000000000000001"),
      BigDecimal("1000")
    )
    val namedRows = Vector(4, 2, 3, 1).map(BigDecimal(_))
    val evaluator = new RecordingFactorEvaluator((request, _) =>
      Right(
        successfulBatch(request, sourceRows = BigDecimal(100)) {
          (candidate, statement, _) =>
            if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
              estimate(
                totalCost = BigDecimal(1),
                rows = terminalRows(candidate.structuralRank - 1),
                width = 8
              )
            else
              estimate(
                rows = namedRows(candidate.structuralRank - 1),
                width = 1
              )
        }
      )
    )

    val selected = select(triangle, Some(evaluator))
      .fold(problem => fail(problem.toString), identity)
    val decision = selected.decision

    assertEquals(selected.plan.rank, 4)
    assertEquals(
      decision.evaluations.map(_.rowGuardEvidence.map(_.withinGuard)),
      Vector(false, true, false, true).map(Some(_))
    )
    assertEquals(
      decision.factorAwareCheckpoints.map(_.effectiveCandidateCount),
      Vector.fill(4)(4)
    )
    assert(decision.factorAwareCheckpoints.forall(_.truncatedBySearchExhaustion))
    assertEquals(
      decision.factorAwareCheckpoints.map(_.winnerStructuralRank),
      Vector.fill(4)(Some(4))
    )
    assertEquals(
      decision.factorAwareCheckpoints.map(_.verifiedPrefixSha256).distinct.size,
      1
    )
  }

  test("no admissible factor estimate retains all evidence and empty checkpoint winners") {
    val evaluator = new RecordingFactorEvaluator((request, _) =>
      Right(
        successfulBatch(request, sourceRows = BigDecimal(100)) {
          (candidate, statement, _) =>
            if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
              estimate(
                totalCost = BigDecimal(candidate.structuralRank),
                rows = BigDecimal(0),
                width = 8
              )
            else estimate(rows = BigDecimal(candidate.structuralRank), width = 1)
        }
      )
    )

    select(triangle, Some(evaluator)) match {
      case Left(failure: PostgreSqlCandidateSelectionFailure.NoAdmissibleEstimate) =>
        val report = failure.report
        assertEquals(report.evaluations.map(_.structuralRank), Vector(1, 2, 3, 4))
        assert(report.evaluations.forall(
          _.rowGuardEvidence.exists(evidence => !evidence.withinGuard)
        ))
        assertEquals(report.sourceEvaluation.map(_.estimate.planRows), Some(BigDecimal(100)))
        assert(report.factorAwareCheckpoints.forall(_.winnerScore.isEmpty))
        assert(report.factorAwareCheckpoints.forall(_.winnerCandidateId.isEmpty))
        assertEquals(report.factorAwareSelectionLimits, Some(defaultFactorLimits))
      case other => fail(s"expected a factor no-admissible result, received $other")
    }
  }

  test("a mid-stage evaluator failure retains only the exact completed candidate and stage prefixes") {
    var completeBatch: Option[PostgreSqlFactorAwareExplainBatch] = None
    val evaluator = new RecordingFactorEvaluator((request, _) => {
      val batch = successfulBatch(request, sourceRows = BigDecimal(100))()
      completeBatch = Some(batch)
      val firstCandidateSize = request.candidates.head.stages.size
      val failedIndex = math.min(
        firstCandidateSize + 1,
        request.flattened.size - 1
      )
      Left(
        PostgreSqlFactorAwareExplainFailure(
          PostgreSqlExplainCostFailureCode.StatementFailed,
          PostgreSqlExplainEvaluationStage.StageExplain,
          Some(failedIndex),
          Some(batch.source),
          batch.observations.take(failedIndex)
        )
      )
    })

    select(triangle, Some(evaluator)) match {
      case Left(failure: PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed) =>
        val request = evaluator.factorRequests.head
        val expectedIndex = math.min(
          request.candidates.head.stages.size + 1,
          request.flattened.size - 1
        )
        val expectedFailure = request.flattened(expectedIndex)
        assertEquals(failure.code, PostgreSqlExplainCostFailureCode.StatementFailed)
        assertEquals(failure.plan.map(_.rank), Some(expectedFailure._1.structuralRank))
        assertEquals(failure.report.stage, PostgreSqlExplainEvaluationStage.StageExplain)
        assertEquals(failure.report.failedStageObservationIndex, Some(expectedIndex))
        assertEquals(failure.report.failedStageId, Some(expectedFailure._2.stageId))
        assertEquals(
          failure.report.completedStageObservations.size,
          expectedIndex
        )
        assertEquals(failure.report.evaluations.map(_.structuralRank), Vector(1))
        assertEquals(failure.report.renderedCandidates.size, 4)
        assertEquals(failure.report.sourceEvaluation.map(_.estimate.planRows), Some(BigDecimal(100)))
        assert(
          PostgreSqlFactorAwareFailureReplay.verify(
            triangle.found.plans,
            failure.report,
            Some(statementSha(triangle.sql))
          )
        )
        assert(
          !PostgreSqlFactorAwareFailureReplay.verify(
            q5.found.plans,
            failure.report,
            Some(statementSha(triangle.sql))
          ),
          "failure replay must bind the genuine Found plan vector"
        )
        assert(
          !PostgreSqlFactorAwareFailureReplay.verify(
            triangle.found.plans,
            failure.report,
            Some("0" * 64)
          ),
          "failure replay must bind the original statement digest"
        )
        intercept[IllegalArgumentException] {
          failure.report.copy(
            completedStageObservations =
              failure.report.completedStageObservations.reverse
          )
        }
        val firstRendered = failure.report.renderedCandidates.head
        val mutatedFirstStage = firstRendered.stages.head.copy(
          statementSha256 = "0" * 64
        )
        val mutatedRendered = firstRendered.copy(
          stages = firstRendered.stages.updated(0, mutatedFirstStage)
        )
        intercept[IllegalArgumentException] {
          failure.report.copy(
            renderedCandidates =
              failure.report.renderedCandidates.updated(0, mutatedRendered)
          )
        }
        val completedTwoCount =
          request.candidates.take(2).map(_.stages.size).sum
        assert(completedTwoCount < request.flattened.size)
        val next = request.flattened(completedTwoCount)
        intercept[IllegalArgumentException] {
          failure.report.copy(
            completedStageObservations = completeBatch.get.observations.take(
              completedTwoCount
            ),
            failedStageObservationIndex = Some(completedTwoCount),
            failedCandidateId = Some(next._1.candidateId),
            failedStructuralRank = Some(next._1.structuralRank),
            failedPhysicalVariantId = Some(next._1.physicalVariantId),
            failedStageId = Some(next._2.stageId)
          )
        }
      case other => fail(s"expected a typed mid-stage failure, received $other")
    }
  }

  test("mutated completed-stage order and hostile numeric payloads fail closed") {
    val reordered = new RecordingFactorEvaluator((request, _) => {
      val batch = successfulBatch(request, sourceRows = BigDecimal(100))()
      val failedIndex = 1
      Left(
        PostgreSqlFactorAwareExplainFailure(
          PostgreSqlExplainCostFailureCode.StatementFailed,
          PostgreSqlExplainEvaluationStage.StageExplain,
          Some(failedIndex),
          Some(batch.source),
          Vector(batch.observations(1))
        )
      )
    })
    assertContractFailure(select(triangle, Some(reordered)))

    val hostileValues = Vector(
      BigDecimal("12345678901234567890123456789012345678901234567890123456789012345"),
      BigDecimal("1e-1025"),
      BigDecimal("1e1025")
    )
    hostileValues.foreach { hostile =>
      val evaluator = new RecordingFactorEvaluator((request, _) =>
        Right(
          successfulBatch(request, sourceRows = BigDecimal(100)) {
            (_, _, _) => estimate(rows = hostile)
          }
        )
      )
      assertContractFailure(select(triangle, Some(evaluator)))
    }
  }

  test("pre-evaluator factor failures retain limits and the rendered prefix without calling JDBC") {
    val unavailable = select(triangle, None)
    unavailable match {
      case Left(failure: PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed) =>
        assertEquals(failure.code, PostgreSqlExplainCostFailureCode.EvaluatorUnavailable)
        assertEquals(failure.report.stage, PostgreSqlExplainEvaluationStage.Session)
        assertEquals(failure.report.renderedCandidates.size, 4)
        assertEquals(failure.report.factorAwareSelectionLimits, Some(defaultFactorLimits))
      case other => fail(s"expected unavailable factor evaluator, received $other")
    }

    val evaluator = new RecordingFactorEvaluator((request, _) =>
      Right(successfulBatch(request)())
    )
    val tinySqlEnvelope = PostgreSqlCandidateSelectionLimits
      .create(maxCumulativeSqlCharacters = 1)
      .fold(errors => fail(errors.map(_.code).mkString(",")), identity)
    select(
      triangle,
      Some(evaluator),
      selectionLimits = tinySqlEnvelope
    ) match {
      case Left(failure: PostgreSqlCandidateSelectionFailure.FactorAwareResourceLimitExceeded) =>
        assertEquals(evaluator.factorCalls, 0)
        assertEquals(
          failure.resource,
          PostgreSqlCandidateSelectionResource.CumulativeSqlCharacters
        )
        assertEquals(failure.report.stage, PostgreSqlExplainEvaluationStage.ResourceValidation)
        assertEquals(failure.report.failedStructuralRank, Some(1))
        assertEquals(failure.report.renderedCandidates.size, 1)
        assertEquals(failure.report.factorAwareSelectionLimits, Some(defaultFactorLimits))
      case other => fail(s"expected pre-evaluator resource failure, received $other")
    }
  }

  private val defaultFactorLimits =
    PostgreSqlFactorAwareSelectionLimits.default

  private def select(
      prepared: Prepared,
      evaluator: Option[PostgreSqlExplainCostEvaluator],
      selectionLimits: PostgreSqlCandidateSelectionLimits =
        PostgreSqlCandidateSelectionLimits.default,
      factorLimits: PostgreSqlFactorAwareSelectionLimits = defaultFactorLimits
  ): Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate] =
    PostgreSqlCandidateSelector.select(
      prepared.query,
      prepared.input,
      prepared.found,
      policy,
      YannakakisReductionMode.NoReduction,
      selectionLimits,
      BagFactorPlanningLimits.default,
      PostgreSqlRenderingLimits.default,
      evaluator,
      PostgreSqlExplainSelectionLimits.default,
      Some(prepared.sql),
      CandidateResourceLimits.default,
      factorLimits
    )

  private def assertContractFailure(
      result: Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate]
  ): Unit =
    result match {
      case Left(failure: PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed) =>
        assertEquals(
          failure.code,
          PostgreSqlExplainCostFailureCode.EvaluatorContractViolation
        )
        assertEquals(failure.report.stage, PostgreSqlExplainEvaluationStage.Session)
        assertEquals(failure.report.evaluations, Vector.empty)
        assertEquals(failure.report.sourceEvaluation, None)
        assertEquals(failure.report.completedStageObservations, Vector.empty)
      case other => fail(s"expected fail-closed evaluator contract result, received $other")
    }

  private def statementSha(sql: String): String =
    PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql)
}
