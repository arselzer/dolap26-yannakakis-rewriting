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

/** Adversarial replay coverage for the persisted diagnostic resource envelope.
  *
  * SQL-free replay cannot recover an arbitrary in-envelope character count
  * from a digest. It can and must still enforce positive counts, every
  * persisted cap and aggregate, canonical CTE-prefix progression, and exact
  * agreement wherever failure evidence persists the count twice.
  */
class PostgreSqlFactorAwareResourceReplayContractSuite
    extends munit.FunSuite {
  import PostgreSqlFactorAwareContractTestSupport._

  private val policy =
    CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1

  test("successful replay enforces canonical diagnostic counts and every aggregate boundary") {
    val evaluator = new RecordingFactorEvaluator((request, _) =>
      Right(
        successfulBatch(request, sourceRows = BigDecimal(100)) {
          (candidate, statement, _) =>
            if (statement.stageId == PostgreSqlStageId.FinalReconstruction)
              estimate(
                totalCost = BigDecimal(candidate.structuralRank),
                rows = BigDecimal(100),
                width = 8
              )
            else
              estimate(
                rows = BigDecimal(candidate.structuralRank),
                width = 8
              )
        }
      )
    )
    val selected = select(Some(evaluator))
      .fold(problem => fail(problem.toString), identity)
    val decision = selected.decision
    val source = decision.sourceEvaluation.getOrElse(
      fail("factor-aware success lost its source estimate")
    )
    val evaluations = decision.evaluations
    val totals = diagnosticTotals(evaluations)

    assertCanonicalCtePrefixes(evaluations)
    assert(totals.maximumCharacters > 1L)
    assert(totals.maximumCandidateCharacters > totals.maximumCharacters)
    assert(totals.sessionCharacters > totals.maximumCandidateCharacters)
    assert(totals.statementCount > 1L)

    val exactLimits = limits(
      maxStageProbesPerCandidate = totals.maximumStagesPerCandidate,
      maxCharactersPerProbe = totals.maximumCharacters,
      maxDiagnosticCharactersPerCandidate =
        totals.maximumCandidateCharacters,
      maxExplainStatementsPerSession = totals.statementCount + 1L,
      maxDiagnosticCharactersPerSession = totals.sessionCharacters
    )
    assertSelectionReplay(
      evaluations,
      source,
      decision.factorAwareCheckpoints,
      exactLimits,
      expected = true
    )

    val maximumPosition = emittedPositions(evaluations).maxBy { position =>
      stageAt(evaluations, position).diagnosticCharacterCount.get
    }
    val overProbe = mutateStage(evaluations, maximumPosition) { stage =>
      stage.copy(
        diagnosticCharacterCount =
          stage.diagnosticCharacterCount.map(_ + 1L)
      )
    }
    val probeLimits = limits(
      maxCharactersPerProbe = totals.maximumCharacters,
      maxDiagnosticCharactersPerCandidate =
        PostgreSqlFactorAwareSelectionLimits.default.maxDiagnosticCharactersPerCandidate,
      maxDiagnosticCharactersPerSession =
        PostgreSqlFactorAwareSelectionLimits.default.maxDiagnosticCharactersPerSession
    )
    assertSelectionReplay(
      overProbe,
      source,
      decision.factorAwareCheckpoints,
      probeLimits,
      expected = false
    )

    val largestCandidateIndex = evaluations.indices.maxBy(index =>
      candidateCharacters(evaluations(index))
    )
    val candidatePosition = emittedPositions(evaluations)
      .find(_.evaluationIndex == largestCandidateIndex)
      .getOrElse(fail("largest candidate has no emitted diagnostic stage"))
    val overCandidate = mutateStage(evaluations, candidatePosition) { stage =>
      stage.copy(
        diagnosticCharacterCount =
          stage.diagnosticCharacterCount.map(_ + 1L)
      )
    }
    val candidateLimits = limits(
      maxCharactersPerProbe = totals.maximumCharacters + 1L,
      maxDiagnosticCharactersPerCandidate =
        totals.maximumCandidateCharacters,
      maxDiagnosticCharactersPerSession =
        PostgreSqlFactorAwareSelectionLimits.default.maxDiagnosticCharactersPerSession
    )
    assertSelectionReplay(
      overCandidate,
      source,
      decision.factorAwareCheckpoints,
      candidateLimits,
      expected = false
    )

    val overSession = mutateStage(evaluations, emittedPositions(evaluations).head) {
      stage =>
        stage.copy(
          diagnosticCharacterCount =
            stage.diagnosticCharacterCount.map(_ + 1L)
        )
    }
    val sessionLimits = limits(
      maxCharactersPerProbe = totals.maximumCharacters + 1L,
      maxDiagnosticCharactersPerCandidate =
        totals.maximumCandidateCharacters + 1L,
      maxDiagnosticCharactersPerSession = totals.sessionCharacters
    )
    assertSelectionReplay(
      overSession,
      source,
      decision.factorAwareCheckpoints,
      sessionLimits,
      expected = false
    )

    val oneStatementTooFew = limits(
      maxExplainStatementsPerSession = totals.statementCount
    )
    assertSelectionReplay(
      evaluations,
      source,
      decision.factorAwareCheckpoints,
      oneStatementTooFew,
      expected = false
    )

    val zeroCharacter = mutateStage(evaluations, emittedPositions(evaluations).head) {
      _.copy(diagnosticCharacterCount = Some(0L))
    }
    assertSelectionReplay(
      zeroCharacter,
      source,
      decision.factorAwareCheckpoints,
      PostgreSqlFactorAwareSelectionLimits.default,
      expected = false
    )

    val namedPosition = emittedPositions(evaluations)
      .find(position =>
        stageAt(evaluations, position).trace.physicalForm.namedCte
      )
      .getOrElse(fail("triangle fixture has no emitted named diagnostic stage"))
    val invalidPrefix = mutateStage(evaluations, namedPosition) { stage =>
      stage.copy(
        diagnosticCtePrefixCount =
          stage.diagnosticCtePrefixCount.map(_ + 1L)
      )
    }
    assertSelectionReplay(
      invalidPrefix,
      source,
      decision.factorAwareCheckpoints,
      PostgreSqlFactorAwareSelectionLimits.default,
      expected = false
    )

    val terminalPosition = emittedPositions(evaluations)
      .find(position =>
        stageAt(evaluations, position).trace.id ==
          PostgreSqlStageId.FinalReconstruction
      )
      .getOrElse(fail("triangle fixture has no terminal diagnostic stage"))
    val invalidTerminalPrefix = mutateStage(evaluations, terminalPosition) {
      stage =>
        stage.copy(
          diagnosticCtePrefixCount =
            stage.diagnosticCtePrefixCount.map(_ + 1L)
        )
    }
    assertSelectionReplay(
      invalidTerminalPrefix,
      source,
      decision.factorAwareCheckpoints,
      PostgreSqlFactorAwareSelectionLimits.default,
      expected = false
    )

    val hostileBigIntTotal = emittedPositions(evaluations).foldLeft(evaluations) {
      case (current, position) =>
        mutateStage(current, position)(
          _.copy(diagnosticCharacterCount = Some(Long.MaxValue))
        )
    }
    assertSelectionReplay(
      hostileBigIntTotal,
      source,
      decision.factorAwareCheckpoints,
      PostgreSqlFactorAwareSelectionLimits.default,
      expected = false
    )
  }

  test("failure-prefix replay cross-binds duplicated counts and rejects hostile aggregate totals") {
    val evaluator = new RecordingFactorEvaluator((request, _) => {
      val batch = successfulBatch(request, sourceRows = BigDecimal(100))()
      val failedIndex = request.candidates.head.stages.size
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
    val failure = select(Some(evaluator)) match {
      case Left(
            problem: PostgreSqlCandidateSelectionFailure.FactorAwareEvaluationFailed
          ) => problem.report
      case other => fail(s"expected a factor-aware stage failure, received $other")
    }
    assertEquals(failure.evaluations.map(_.structuralRank), Vector(1))
    assert(
      PostgreSqlFactorAwareFailureReplay.verify(
        triangle.found.plans,
        failure,
        Some(sourceStatementSha256)
      )
    )

    val renderedTotals = renderedDiagnosticTotals(failure.renderedCandidates)
    val exactLimits = limits(
      maxStageProbesPerCandidate =
        renderedTotals.maximumStagesPerCandidate,
      maxCharactersPerProbe = renderedTotals.maximumCharacters,
      maxDiagnosticCharactersPerCandidate =
        renderedTotals.maximumCandidateCharacters,
      maxExplainStatementsPerSession = renderedTotals.statementCount + 1L,
      maxDiagnosticCharactersPerSession = renderedTotals.sessionCharacters
    )
    val exactBoundary = failure.copy(
      factorAwareSelectionLimits = Some(exactLimits)
    )
    assert(
      PostgreSqlFactorAwareFailureReplay.verify(
        triangle.found.plans,
        exactBoundary,
        Some(sourceStatementSha256)
      )
    )

    val firstRendered = failure.renderedCandidates.head
    val firstRenderedStage = firstRendered.stages.head
    val driftedCharacter = failure.copy(
      renderedCandidates = failure.renderedCandidates.updated(
        0,
        firstRendered.copy(
          stages = firstRendered.stages.updated(
            0,
            firstRenderedStage.copy(
              characterCount = firstRenderedStage.characterCount + 1L
            )
          )
        )
      )
    )
    assert(
      !PostgreSqlFactorAwareFailureReplay.verify(
        triangle.found.plans,
        driftedCharacter,
        Some(sourceStatementSha256)
      )
    )

    val driftedPrefix = failure.copy(
      renderedCandidates = failure.renderedCandidates.updated(
        0,
        firstRendered.copy(
          stages = firstRendered.stages.updated(
            0,
            firstRenderedStage.copy(
              ctePrefixCount = firstRenderedStage.ctePrefixCount + 1L
            )
          )
        )
      )
    )
    assert(
      !PostgreSqlFactorAwareFailureReplay.verify(
        triangle.found.plans,
        driftedPrefix,
        Some(sourceStatementSha256)
      )
    )

    val evaluationPosition = emittedPositions(failure.evaluations).head
    val driftedEvaluation = failure.copy(
      evaluations = mutateStage(failure.evaluations, evaluationPosition) {
        stage =>
          stage.copy(
            diagnosticCharacterCount =
              stage.diagnosticCharacterCount.map(_ + 1L)
          )
      }
    )
    assert(
      !PostgreSqlFactorAwareFailureReplay.verify(
        triangle.found.plans,
        driftedEvaluation,
        Some(sourceStatementSha256)
      )
    )

    val overProbeLimits = limits(
      maxCharactersPerProbe = renderedTotals.maximumCharacters - 1L,
      maxDiagnosticCharactersPerCandidate =
        renderedTotals.maximumCandidateCharacters,
      maxDiagnosticCharactersPerSession = renderedTotals.sessionCharacters
    )
    assertFailureReplayWithLimits(failure, overProbeLimits, expected = false)

    val overCandidateLimits = limits(
      maxCharactersPerProbe = renderedTotals.maximumCharacters,
      maxDiagnosticCharactersPerCandidate =
        renderedTotals.maximumCandidateCharacters - 1L,
      maxDiagnosticCharactersPerSession = renderedTotals.sessionCharacters
    )
    assertFailureReplayWithLimits(
      failure,
      overCandidateLimits,
      expected = false
    )

    val overSessionLimits = limits(
      maxCharactersPerProbe = renderedTotals.maximumCharacters,
      maxDiagnosticCharactersPerCandidate =
        renderedTotals.maximumCandidateCharacters,
      maxDiagnosticCharactersPerSession =
        renderedTotals.sessionCharacters - 1L
    )
    assertFailureReplayWithLimits(failure, overSessionLimits, expected = false)

    intercept[IllegalArgumentException] {
      failure.copy(
        factorAwareSelectionLimits = Some(limits(
          maxExplainStatementsPerSession = renderedTotals.statementCount
        ))
      )
    }

    val hostileRendered = failure.renderedCandidates.map { candidate =>
      candidate.copy(stages = candidate.stages.map(
        _.copy(characterCount = Long.MaxValue)
      ))
    }
    val hostileBigIntTotal = failure.copy(
      renderedCandidates = hostileRendered
    )
    assert(
      !PostgreSqlFactorAwareFailureReplay.verify(
        triangle.found.plans,
        hostileBigIntTotal,
        Some(sourceStatementSha256)
      )
    )
  }

  private final case class StagePosition(
      evaluationIndex: Int,
      stageIndex: Int
  )

  private final case class DiagnosticTotals(
      maximumStagesPerCandidate: Long,
      maximumCharacters: Long,
      maximumCandidateCharacters: Long,
      statementCount: Long,
      sessionCharacters: Long
  )

  private def emittedPositions(
      evaluations: Vector[PostgreSqlCandidateEvaluation]
  ): Vector[StagePosition] =
    evaluations.zipWithIndex.flatMap { case (evaluation, evaluationIndex) =>
      evaluation.factorAwareEvidence.toVector.flatMap(_.stages.zipWithIndex)
        .collect {
          case (stage, stageIndex) if stage.trace.physicalForm.emitted =>
            StagePosition(evaluationIndex, stageIndex)
        }
    }

  private def stageAt(
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      position: StagePosition
  ): PostgreSqlFactorAwareStageEvaluation =
    evaluations(position.evaluationIndex).factorAwareEvidence.get
      .stages(position.stageIndex)

  private def mutateStage(
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      position: StagePosition
  )(
      mutate: PostgreSqlFactorAwareStageEvaluation =>
        PostgreSqlFactorAwareStageEvaluation
  ): Vector[PostgreSqlCandidateEvaluation] = {
    val evaluation = evaluations(position.evaluationIndex)
    val evidence = evaluation.factorAwareEvidence.get
    val updatedEvidence = evidence.copy(
      stages = evidence.stages.updated(
        position.stageIndex,
        mutate(evidence.stages(position.stageIndex))
      )
    )
    evaluations.updated(
      position.evaluationIndex,
      evaluation.copy(factorAwareEvidence = Some(updatedEvidence))
    )
  }

  private def candidateCharacters(
      evaluation: PostgreSqlCandidateEvaluation
  ): Long =
    evaluation.factorAwareEvidence.toVector
      .flatMap(_.stages)
      .filter(_.trace.physicalForm.emitted)
      .flatMap(_.diagnosticCharacterCount)
      .foldLeft(BigInt(0))(_ + _)
      .toLong

  private def diagnosticTotals(
      evaluations: Vector[PostgreSqlCandidateEvaluation]
  ): DiagnosticTotals = {
    val emitted = evaluations.map(evaluation =>
      evaluation.factorAwareEvidence.toVector
        .flatMap(_.stages)
        .filter(_.trace.physicalForm.emitted)
    )
    val candidateTotals = emitted.map(stages =>
      stages.flatMap(_.diagnosticCharacterCount).foldLeft(BigInt(0))(_ + _)
    )
    val allCounts = emitted.flatten.flatMap(_.diagnosticCharacterCount)
    DiagnosticTotals(
      emitted.map(_.size.toLong).max,
      allCounts.max,
      candidateTotals.max.toLong,
      emitted.foldLeft(BigInt(0))(_ + _.size).toLong,
      candidateTotals.foldLeft(BigInt(0))(_ + _).toLong
    )
  }

  private def renderedDiagnosticTotals(
      candidates: Vector[PostgreSqlFactorAwareRenderedCandidateIdentity]
  ): DiagnosticTotals = {
    val candidateTotals = candidates.map(candidate =>
      candidate.stages.foldLeft(BigInt(0))(_ + _.characterCount)
    )
    val allCounts = candidates.flatMap(_.stages.map(_.characterCount))
    DiagnosticTotals(
      candidates.map(_.stages.size.toLong).max,
      allCounts.max,
      candidateTotals.max.toLong,
      candidates.foldLeft(BigInt(0))(_ + _.stages.size).toLong,
      candidateTotals.foldLeft(BigInt(0))(_ + _).toLong
    )
  }

  private def assertCanonicalCtePrefixes(
      evaluations: Vector[PostgreSqlCandidateEvaluation]
  ): Unit =
    evaluations.foreach { evaluation =>
      val stages = evaluation.factorAwareEvidence.get.stages
      val named = stages.filter(_.trace.physicalForm.namedCte)
      assertEquals(
        named.flatMap(_.diagnosticCtePrefixCount),
        (1 to named.size).map(_.toLong).toVector
      )
      val terminal = stages.last
      assertEquals(
        terminal.trace.physicalForm,
        PostgreSqlStagePhysicalForm.TerminalSelect
      )
      assertEquals(
        terminal.diagnosticCtePrefixCount,
        Some(named.size.toLong)
      )
    }

  private def assertSelectionReplay(
      evaluations: Vector[PostgreSqlCandidateEvaluation],
      source: PostgreSqlExplainSourceEvaluation,
      checkpoints: Vector[PostgreSqlFactorAwareCheckpoint],
      activeLimits: PostgreSqlFactorAwareSelectionLimits,
      expected: Boolean
  ): Unit =
    assertEquals(
      PostgreSqlFactorAwareSelectionReplay.verify(
        triangle.found.plans,
        evaluations,
        source,
        checkpoints,
        activeLimits,
        Some(sourceStatementSha256)
      ),
      expected
    )

  private def assertFailureReplayWithLimits(
      failure: PostgreSqlCandidateSelectionDecision.FactorAwareEvaluationFailed,
      activeLimits: PostgreSqlFactorAwareSelectionLimits,
      expected: Boolean
  ): Unit = {
    val withLimits = failure.copy(
      factorAwareSelectionLimits = Some(activeLimits)
    )
    assertEquals(
      PostgreSqlFactorAwareFailureReplay.verify(
        triangle.found.plans,
        withLimits,
        Some(sourceStatementSha256)
      ),
      expected
    )
  }

  private def limits(
      maxStageProbesPerCandidate: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxStageProbesPerCandidate,
      maxCharactersPerProbe: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxCharactersPerProbe,
      maxDiagnosticCharactersPerCandidate: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxDiagnosticCharactersPerCandidate,
      maxExplainStatementsPerSession: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxExplainStatementsPerSession,
      maxDiagnosticCharactersPerSession: Long =
        PostgreSqlFactorAwareSelectionLimits.default.maxDiagnosticCharactersPerSession
  ): PostgreSqlFactorAwareSelectionLimits =
    PostgreSqlFactorAwareSelectionLimits
      .create(
        maxStageProbesPerCandidate = maxStageProbesPerCandidate,
        maxCharactersPerProbe = maxCharactersPerProbe,
        maxDiagnosticCharactersPerCandidate =
          maxDiagnosticCharactersPerCandidate,
        maxExplainStatementsPerSession = maxExplainStatementsPerSession,
        maxDiagnosticCharactersPerSession = maxDiagnosticCharactersPerSession
      )
      .fold(errors => fail(errors.map(_.code).mkString(",")), identity)

  private def select(
      evaluator: Option[PostgreSqlExplainCostEvaluator]
  ): Either[PostgreSqlCandidateSelectionFailure, PostgreSqlSelectedCandidate] =
    PostgreSqlCandidateSelector.select(
      triangle.query,
      triangle.input,
      triangle.found,
      policy,
      YannakakisReductionMode.NoReduction,
      PostgreSqlCandidateSelectionLimits.default,
      BagFactorPlanningLimits.default,
      PostgreSqlRenderingLimits.default,
      evaluator,
      PostgreSqlExplainSelectionLimits.default,
      Some(triangle.sql),
      CandidateResourceLimits.default,
      PostgreSqlFactorAwareSelectionLimits.default
    )

  private def sourceStatementSha256: String =
    PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(triangle.sql)
}
