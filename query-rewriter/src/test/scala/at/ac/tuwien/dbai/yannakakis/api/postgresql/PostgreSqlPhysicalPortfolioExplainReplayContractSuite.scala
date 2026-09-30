package at.ac.tuwien.dbai.yannakakis.api.postgresql

class PostgreSqlPhysicalPortfolioExplainReplayContractSuite extends munit.FunSuite {
  import PostgreSqlPhysicalPortfolioContractTestSupport._
  import PostgreSqlPhysicalPortfolioEvaluationResource._
  import PostgreSqlPhysicalPortfolioExplainFailureCode._
  import PostgreSqlPhysicalPortfolioExplainStage._

  test("request binds exact SQL identities and flattens workloads in scheduler then stage order") {
    val activeRequest = request(2)
    assertEquals(activeRequest.workloads.map(_.group.earliestEnumerationOrdinal), Vector(0, 1))
    assertEquals(
      activeRequest.flattened.map { case (workload, stage) =>
        workload.group.descriptor.workloadId -> stage.stageId
      },
      activeRequest.workloads.flatMap(workload =>
        workload.stages.map(stage =>
          workload.group.descriptor.workloadId -> stage.stageId
        )
      )
    )

    val statement = activeRequest.workloads.head.stages.head
    intercept[IllegalArgumentException] {
      statement.copy(characterCount = statement.characterCount + 1L)
    }
    intercept[IllegalArgumentException] {
      statement.copy(statementSha256 = "0" * 64)
    }
    intercept[IllegalArgumentException] {
      activeRequest.workloads.head.copy(
        stages = activeRequest.workloads.head.stages.reverse
      )
    }
    intercept[IllegalArgumentException] {
      activeRequest.copy(workloads = activeRequest.workloads.reverse)
    }
    intercept[IllegalArgumentException] {
      activeRequest.copy(minimumServerVersionNum = 119999)
    }
    intercept[IllegalArgumentException] {
      activeRequest.copy(materializationScope = "one-cte-only")
    }
  }

  test("BigInt envelope includes the source statement and reports the first exact workload cap") {
    val activeRequest = request(2)
    val result = PostgreSqlPhysicalPortfolioEvaluationEnvelope
      .validate(
        activeRequest,
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
      )
      .fold(problem => fail(problem.toString), identity)
    assertEquals(result.workloadCount, BigInt(2))
    assertEquals(
      result.explainStatementCount,
      BigInt(1 + activeRequest.flattened.size)
    )
    assertEquals(
      result.diagnosticCharacterCount,
      activeRequest.flattened.foldLeft(BigInt(0)) { case (total, (_, stage)) =>
        total + BigInt(stage.sql.length)
      }
    )

    val first = activeRequest.workloads.head
    val tooManyStages = envelopeLimits(maxStageProbesPerWorkload = 4L)
    val stageFailure = PostgreSqlPhysicalPortfolioEvaluationEnvelope
      .validate(activeRequest, tooManyStages)
      .left
      .toOption
      .get
    assertEquals(stageFailure.resource, StageProbesPerWorkload)
    assertEquals(stageFailure.workloadId, Some(first.group.descriptor.workloadId))
    assertEquals(stageFailure.required, BigInt(5))

    val longest = activeRequest.flattened.map(_._2.sql.length.toLong).max
    val characterFailure = PostgreSqlPhysicalPortfolioEvaluationEnvelope
      .validate(
        activeRequest,
        envelopeLimits(maxCharactersPerProbe = longest - 1L)
      )
      .left
      .toOption
      .get
    assertEquals(characterFailure.resource, CharactersPerProbe)
    assertEquals(characterFailure.required, BigInt(longest))

    val statementFailure = PostgreSqlPhysicalPortfolioEvaluationEnvelope
      .validate(
        activeRequest,
        envelopeLimits(
          maxExplainStatementsPerSession = activeRequest.flattened.size.toLong
        )
      )
      .left
      .toOption
      .get
    assertEquals(statementFailure.resource, ExplainStatementsPerSession)
    assertEquals(statementFailure.required, BigInt(1 + activeRequest.flattened.size))

    val huge = BigInt(Long.MaxValue) + 1
    val persisted = PostgreSqlPhysicalPortfolioEvaluationResourceExceeded(
      DiagnosticCharactersPerSession,
      None,
      huge,
      Long.MaxValue
    )
    assertEquals(persisted.required, huge)
  }

  test("successful replay consumes every observation exactly and restores workload grouping") {
    val activeRequest = request(2)
    val batch = successfulBatch(activeRequest)
    val verified = PostgreSqlPhysicalPortfolioExplainReplay
      .verify(activeRequest, batch)
      .getOrElse(fail("valid physical portfolio batch did not replay"))
    assertEquals(verified.source, batch.source)
    assertEquals(
      verified.observationsByWorkload.map(_.size),
      activeRequest.workloads.map(_.stages.size)
    )
    assertEquals(verified.observationsByWorkload.flatten, batch.observations)

    val swapped = batch.copy(
      observations = batch.observations.updated(0, batch.observations(1)).updated(1, batch.observations(0))
    )
    assertEquals(PostgreSqlPhysicalPortfolioExplainReplay.verify(activeRequest, swapped), None)

    val wrongWorkload = batch.observations.head.copy(
      workloadId = activeRequest.workloads(1).group.descriptor.workloadId
    )
    assertEquals(
      PostgreSqlPhysicalPortfolioExplainReplay.verify(
        activeRequest,
        batch.copy(observations = batch.observations.updated(0, wrongWorkload))
      ),
      None
    )
    val wrongSource = batch.source.copy(sourceStatementSha256 = "f" * 64)
    assertEquals(
      PostgreSqlPhysicalPortfolioExplainReplay.verify(
        activeRequest,
        batch.copy(source = wrongSource)
      ),
      None
    )
  }

  test("success replay rejects physical alias, stage, digest, provenance, count, and numeric drift") {
    val activeRequest = request(2)
    val batch = successfulBatch(activeRequest)
    val head = batch.observations.head
    val mutations = Vector(
      head.copy(
        representativePhysicalVariantId =
          activeRequest.workloads(1).group.representative.physicalVariantId
      ),
      head.copy(stageId = batch.observations(1).stageId),
      head.copy(statementSha256 = "e" * 64)
    )
    mutations.foreach { changed =>
      assertEquals(
        PostgreSqlPhysicalPortfolioExplainReplay.verify(
          activeRequest,
          batch.copy(observations = batch.observations.updated(0, changed))
        ),
        None
      )
    }
    assertEquals(
      PostgreSqlPhysicalPortfolioExplainReplay.verify(
        activeRequest,
        batch.copy(observations = batch.observations.dropRight(1))
      ),
      None
    )

    val changedProvenance = provenance.copy(serverVersionNum = 180005)
    intercept[IllegalArgumentException] {
      batch.copy(
        observations = batch.observations.updated(
          0,
          head.copy(estimate = head.estimate.copy(provenance = changedProvenance))
        )
      )
    }
    intercept[IllegalArgumentException] {
      head.copy(estimate = head.estimate.copy(planRows = BigDecimal("9" * 65)))
    }
  }

  test("failure matrix is exhaustive and rejects every mismatched code and stage") {
    val codes = Vector(
      EvaluatorUnavailable,
      ConnectionUnavailable,
      UnsafeRenderedStatement,
      SessionConfigurationFailed,
      ProvenanceUnavailable,
      UnsupportedServerVersion,
      StatementTimedOut,
      StatementFailed,
      PlanJsonOversized,
      PlanJsonMalformed,
      ProvenanceChanged,
      CleanupFailed,
      EvaluatorContractViolation,
      SessionResourceLimitExceeded
    )
    val stages = Vector(Session, VersionPreflight, SourceExplain, StageExplain, FinalProvenance)
    val accepted = Set[(PostgreSqlPhysicalPortfolioExplainFailureCode, PostgreSqlPhysicalPortfolioExplainStage)](
      EvaluatorUnavailable -> Session,
      ConnectionUnavailable -> Session,
      SessionConfigurationFailed -> Session,
      StatementTimedOut -> Session,
      CleanupFailed -> Session,
      EvaluatorContractViolation -> Session,
      SessionResourceLimitExceeded -> Session,
      ProvenanceUnavailable -> VersionPreflight,
      UnsupportedServerVersion -> VersionPreflight,
      StatementTimedOut -> VersionPreflight,
      CleanupFailed -> VersionPreflight,
      UnsafeRenderedStatement -> SourceExplain,
      StatementTimedOut -> SourceExplain,
      StatementFailed -> SourceExplain,
      PlanJsonOversized -> SourceExplain,
      PlanJsonMalformed -> SourceExplain,
      CleanupFailed -> SourceExplain,
      UnsafeRenderedStatement -> StageExplain,
      StatementTimedOut -> StageExplain,
      StatementFailed -> StageExplain,
      PlanJsonOversized -> StageExplain,
      PlanJsonMalformed -> StageExplain,
      CleanupFailed -> StageExplain,
      ProvenanceUnavailable -> FinalProvenance,
      UnsupportedServerVersion -> FinalProvenance,
      ProvenanceChanged -> FinalProvenance,
      StatementTimedOut -> FinalProvenance,
      CleanupFailed -> FinalProvenance
    )
    codes.foreach(code =>
      stages.foreach(stage =>
        assertEquals(
          PostgreSqlPhysicalPortfolioExplainFailureMatrix.accepts(code, stage),
          accepted(code -> stage),
          s"${code.id}/${stage.id}"
        )
      )
    )
    assert(!PostgreSqlPhysicalPortfolioExplainFailureMatrix.accepts(null, Session))
    assert(!PostgreSqlPhysicalPortfolioExplainFailureMatrix.accepts(EvaluatorUnavailable, null))
  }

  test("PostgreSQL pre-12 fails at version preflight with zero optimizer observations") {
    val activeRequest = request(1)
    val failure = PostgreSqlPhysicalPortfolioExplainFailure(
      UnsupportedServerVersion,
      VersionPreflight,
      None,
      None,
      Vector.empty,
      activeRequest.flattened.size,
      Some(119999),
      Some(PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum)
    )
    assert(
      PostgreSqlPhysicalPortfolioExplainReplay.verifyFailurePrefix(
        activeRequest,
        failure
      )
    )
    intercept[IllegalArgumentException] {
      failure.copy(requiredServerVersionNum = None)
    }
    intercept[IllegalArgumentException] {
      failure.copy(code = ProvenanceUnavailable)
    }
    assert(
      !PostgreSqlPhysicalPortfolioExplainReplay.verifyFailurePrefix(
        activeRequest,
        failure.copy(requestedStageStatementCount = activeRequest.flattened.size + 1)
      )
    )
  }

  test("failure replay accepts exact mid-stage and final prefixes and rejects mutation or truncation") {
    val activeRequest = request(2)
    val batch = successfulBatch(activeRequest)
    val middle = 3
    val stageFailure = PostgreSqlPhysicalPortfolioExplainFailure(
      StatementFailed,
      StageExplain,
      Some(middle),
      Some(batch.source),
      batch.observations.take(middle),
      activeRequest.flattened.size,
      None,
      None
    )
    assert(
      PostgreSqlPhysicalPortfolioExplainReplay.verifyFailurePrefix(
        activeRequest,
        stageFailure
      )
    )
    val mutatedPrefix = stageFailure.completed.updated(
      0,
      stageFailure.completed.head.copy(statementSha256 = "a" * 64)
    )
    assert(
      !PostgreSqlPhysicalPortfolioExplainReplay.verifyFailurePrefix(
        activeRequest,
        stageFailure.copy(completed = mutatedPrefix)
      )
    )
    intercept[IllegalArgumentException] {
      stageFailure.copy(failedStatementIndex = Some(middle - 1))
    }

    val finalFailure = PostgreSqlPhysicalPortfolioExplainFailure(
      ProvenanceChanged,
      FinalProvenance,
      None,
      Some(batch.source),
      batch.observations,
      activeRequest.flattened.size,
      None,
      None
    )
    assert(
      PostgreSqlPhysicalPortfolioExplainReplay.verifyFailurePrefix(
        activeRequest,
        finalFailure
      )
    )
    intercept[IllegalArgumentException] {
      finalFailure.copy(completed = finalFailure.completed.dropRight(1))
    }
  }

  test("session payloads and the optional evaluator boundary fail closed without reusing v5 evidence") {
    val activeRequest = request(1)
    val emptySessionFailure = PostgreSqlPhysicalPortfolioExplainFailure(
      EvaluatorContractViolation,
      Session,
      None,
      None,
      Vector.empty,
      activeRequest.flattened.size,
      None,
      None
    )
    assert(
      PostgreSqlPhysicalPortfolioExplainReplay.verifyFailurePrefix(
        activeRequest,
        emptySessionFailure
      )
    )
    val batch = successfulBatch(activeRequest)
    intercept[IllegalArgumentException] {
      emptySessionFailure.copy(
        source = Some(batch.source),
        completed = batch.observations
      )
    }

    val evaluator = new PostgreSqlExplainCostEvaluator {
      override def evaluate(
          request: PostgreSqlExplainCostBatchRequest,
          limits: PostgreSqlExplainSelectionLimits
      ): Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch] =
        Left(
          PostgreSqlExplainCostEvaluationFailure(
            PostgreSqlExplainCostFailureCode.EvaluatorUnavailable,
            PostgreSqlExplainEvaluationStage.Session,
            None,
            None,
            Vector.empty
          )
        )
    }
    val failure = evaluator
      .evaluatePhysicalPortfolio(
        activeRequest,
        PostgreSqlPhysicalPortfolioEvaluationLimits.default
      )
      .left
      .toOption
      .get
    assertEquals(failure.code, EvaluatorContractViolation)
    assertEquals(failure.stage, Session)
    assertEquals(failure.requestedStageStatementCount, activeRequest.flattened.size)
    assertEquals(failure.source, None)
    assertEquals(failure.completed, Vector.empty)
    assertEquals(
      PostgreSqlPhysicalPortfolioExplainFailureCode.fromLegacy(
        PostgreSqlExplainCostFailureCode.PrefixVerificationFailed
      ),
      EvaluatorContractViolation
    )
  }

  private def envelopeLimits(
      maxStageProbesPerWorkload: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxStageProbesPerWorkload,
      maxCharactersPerProbe: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxCharactersPerProbe,
      maxExplainStatementsPerSession: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxExplainStatementsPerSession
  ): PostgreSqlPhysicalPortfolioEvaluationLimits =
    PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(
        maxStageProbesPerWorkload = maxStageProbesPerWorkload,
        maxCharactersPerProbe = maxCharactersPerProbe,
        maxExplainStatementsPerSession = maxExplainStatementsPerSession
      )
      .fold(problems => fail(problems.map(_.code).mkString(",")), identity)
}
