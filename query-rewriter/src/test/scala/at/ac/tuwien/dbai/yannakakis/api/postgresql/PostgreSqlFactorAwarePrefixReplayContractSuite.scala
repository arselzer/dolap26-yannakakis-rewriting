package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions
}

class PostgreSqlFactorAwarePrefixReplayContractSuite extends munit.FunSuite {
  import PostgreSqlFactorAwareContractTestSupport._

  test("factor-aware policy freezes exact plan limit, checkpoints, scope, and score order") {
    val policy =
      CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
    assertEquals(policy.id, "postgresql_explain_factor_aware_progressive_v1")
    assertEquals(PostgreSqlFactorAwareProgressiveV1.RequiredPlanLimit, 100)
    assertEquals(
      PostgreSqlFactorAwareProgressiveV1.Checkpoints,
      Vector(10, 25, 50, 100)
    )
    assertEquals(
      PostgreSqlFactorAwareProgressiveV1.Scope,
      "retained_structural_top_k_first_canonical_cover_v1"
    )
    assertEquals(
      PostgreSqlFactorAwareProgressiveV1.ScoreOrder,
      Vector(
        "max_named_stage_plan_rows_times_plan_width",
        "sum_named_stage_plan_rows_times_plan_width",
        "final_plan_total_cost",
        "structural_rank",
        "candidate_id"
      )
    )

    Vector(99, 101).foreach { invalidLimit =>
      val diagnostics = RewriteOptions
        .validated(
          planLimit = invalidLimit,
          candidateSelection = policy
        )
        .left
        .toOption
        .getOrElse(fail(s"factor plan limit $invalidLimit was accepted"))
      assertEquals(diagnostics.size, 1)
      assertEquals(diagnostics.head.context.get("option"), Some("planLimit"))
      intercept[IllegalArgumentException] {
        RewriteOptions(
          planLimit = invalidLimit,
          candidateSelection = policy
        )
      }
    }
    assert(
      RewriteOptions
        .validated(planLimit = 100, candidateSelection = policy)
        .isRight
    )
  }

  test("factor-aware limits freeze the reviewed defaults and reject invalid envelopes") {
    val Mi = 1024L * 1024L
    val defaults = PostgreSqlFactorAwareSelectionLimits.default
    assertEquals(defaults.maxCandidates, 100L)
    assertEquals(defaults.maxStageProbesPerCandidate, 256L)
    assertEquals(defaults.maxCharactersPerProbe, 16L * Mi)
    assertEquals(defaults.maxDiagnosticCharactersPerCandidate, 64L * Mi)
    assertEquals(defaults.maxExplainStatementsPerSession, 4096L)
    assertEquals(defaults.maxDiagnosticCharactersPerSession, 256L * Mi)
    assertEquals(defaults.maxPlanJsonCharacters, Mi)
    assertEquals(defaults.maxCumulativePlanJsonCharacters, 128L * Mi)
    assertEquals(defaults.statementTimeoutSeconds, 30)
    assertEquals(defaults.explainStatementBudgetSeconds, 600)

    val nonPositive = PostgreSqlFactorAwareSelectionLimits
      .create(
        maxCandidates = 0,
        maxStageProbesPerCandidate = 0,
        maxCharactersPerProbe = 0,
        maxDiagnosticCharactersPerCandidate = 0,
        maxExplainStatementsPerSession = 0,
        maxDiagnosticCharactersPerSession = 0,
        maxPlanJsonCharacters = 0,
        maxCumulativePlanJsonCharacters = 0,
        statementTimeoutSeconds = 0,
        explainStatementBudgetSeconds = 0
      )
      .left
      .toOption
      .getOrElse(fail("non-positive factor-aware limits were accepted"))
    assertEquals(nonPositive.size, 10)
    assert(nonPositive.forall(
      _.isInstanceOf[PostgreSqlFactorAwareSelectionLimitError.InvalidResourceLimit]
    ))

    val frozen = PostgreSqlFactorAwareSelectionLimits
      .create(maxCandidates = 99)
      .left
      .toOption
      .getOrElse(fail("a non-v1 candidate bound was accepted"))
    assertEquals(
      frozen.map(_.code),
      Vector("factor-aware-limit.frozen-contract.candidates")
    )

    val inconsistent = PostgreSqlFactorAwareSelectionLimits
      .create(
        maxCharactersPerProbe = 101,
        maxDiagnosticCharactersPerCandidate = 100,
        maxDiagnosticCharactersPerSession = 99,
        maxPlanJsonCharacters = 11,
        maxCumulativePlanJsonCharacters = 10,
        statementTimeoutSeconds = 3,
        explainStatementBudgetSeconds = 2
      )
      .left
      .toOption
      .getOrElse(fail("inconsistent factor-aware limits were accepted"))
    assertEquals(
      inconsistent.map(_.code),
      Vector(
        "factor-aware-limit.inconsistent.characters-per-probe.diagnostic-characters-per-candidate",
        "factor-aware-limit.inconsistent.diagnostic-characters-per-candidate.diagnostic-characters-per-session",
        "factor-aware-limit.inconsistent.plan-json-characters.cumulative-plan-json-characters",
        "factor-aware-limit.inconsistent.statement-timeout-seconds.explain-statement-budget-seconds"
      )
    )

    val integerOverflow = PostgreSqlFactorAwareSelectionLimits
      .create(
        maxCharactersPerProbe = Int.MaxValue.toLong + 1L,
        maxDiagnosticCharactersPerCandidate = Int.MaxValue.toLong + 1L,
        maxDiagnosticCharactersPerSession = Int.MaxValue.toLong + 1L
      )
      .left
      .toOption
      .getOrElse(fail("an unmaterializable probe bound was accepted"))
    assert(
      integerOverflow.map(_.code).contains(
        "factor-aware-limit.integer-materialization.characters-per-probe"
      )
    )

    val planJsonIntegerOverflow = PostgreSqlFactorAwareSelectionLimits
      .create(
        maxPlanJsonCharacters = Int.MaxValue.toLong + 1L,
        maxCumulativePlanJsonCharacters = Int.MaxValue.toLong + 1L
      )
      .left
      .toOption
      .getOrElse(fail("an unmaterializable plan JSON bound was accepted"))
    assert(
      planJsonIntegerOverflow.map(_.code).contains(
        "factor-aware-limit.integer-materialization.plan-json-characters"
      )
    )
  }

  test("every reviewed factor-aware maximum is inclusive and max plus one is rejected") {
    val maximum = PostgreSqlFactorAwareSelectionLimits.default
    val exactMaximum = PostgreSqlFactorAwareSelectionLimits
      .create(
        maxCandidates = maximum.maxCandidates,
        maxStageProbesPerCandidate = maximum.maxStageProbesPerCandidate,
        maxCharactersPerProbe = maximum.maxCharactersPerProbe,
        maxDiagnosticCharactersPerCandidate =
          maximum.maxDiagnosticCharactersPerCandidate,
        maxExplainStatementsPerSession = maximum.maxExplainStatementsPerSession,
        maxDiagnosticCharactersPerSession =
          maximum.maxDiagnosticCharactersPerSession,
        maxPlanJsonCharacters = maximum.maxPlanJsonCharacters,
        maxCumulativePlanJsonCharacters =
          maximum.maxCumulativePlanJsonCharacters,
        statementTimeoutSeconds = maximum.statementTimeoutSeconds,
        explainStatementBudgetSeconds = maximum.explainStatementBudgetSeconds
      )
      .fold(errors => fail(errors.map(_.code).mkString(",")), identity)
    assertEquals(exactMaximum, maximum)

    val aboveMaximum = Vector(
      PostgreSqlFactorAwareSelectionLimits.create(
        maxCandidates = maximum.maxCandidates + 1L
      ) -> "factor-aware-limit.frozen-contract.candidates",
      PostgreSqlFactorAwareSelectionLimits.create(
        maxStageProbesPerCandidate = maximum.maxStageProbesPerCandidate + 1L
      ) -> "factor-aware-limit.maximum.stage-probes-per-candidate",
      PostgreSqlFactorAwareSelectionLimits.create(
        maxCharactersPerProbe = maximum.maxCharactersPerProbe + 1L
      ) -> "factor-aware-limit.maximum.characters-per-probe",
      PostgreSqlFactorAwareSelectionLimits.create(
        maxDiagnosticCharactersPerCandidate =
          maximum.maxDiagnosticCharactersPerCandidate + 1L
      ) -> "factor-aware-limit.maximum.diagnostic-characters-per-candidate",
      PostgreSqlFactorAwareSelectionLimits.create(
        maxExplainStatementsPerSession =
          maximum.maxExplainStatementsPerSession + 1L
      ) -> "factor-aware-limit.maximum.explain-statements-per-session",
      PostgreSqlFactorAwareSelectionLimits.create(
        maxDiagnosticCharactersPerSession =
          maximum.maxDiagnosticCharactersPerSession + 1L
      ) -> "factor-aware-limit.maximum.diagnostic-characters-per-session",
      PostgreSqlFactorAwareSelectionLimits.create(
        maxPlanJsonCharacters = maximum.maxPlanJsonCharacters + 1L
      ) -> "factor-aware-limit.maximum.plan-json-characters",
      PostgreSqlFactorAwareSelectionLimits.create(
        maxCumulativePlanJsonCharacters =
          maximum.maxCumulativePlanJsonCharacters + 1L
      ) -> "factor-aware-limit.maximum.cumulative-plan-json-characters",
      PostgreSqlFactorAwareSelectionLimits.create(
        statementTimeoutSeconds = maximum.statementTimeoutSeconds + 1
      ) -> "factor-aware-limit.maximum.statement-timeout-seconds",
      PostgreSqlFactorAwareSelectionLimits.create(
        explainStatementBudgetSeconds =
          maximum.explainStatementBudgetSeconds + 1
      ) -> "factor-aware-limit.maximum.explain-statement-budget-seconds"
    )
    aboveMaximum.foreach { case (result, expectedCode) =>
      assertEquals(
        result.left.toOption.map(_.map(_.code)),
        Some(Vector(expectedCode)),
        expectedCode
      )
    }

    val lower = PostgreSqlFactorAwareSelectionLimits
      .create(
        maxStageProbesPerCandidate = 2,
        maxCharactersPerProbe = 16,
        maxDiagnosticCharactersPerCandidate = 32,
        maxExplainStatementsPerSession = 3,
        maxDiagnosticCharactersPerSession = 32,
        maxPlanJsonCharacters = 16,
        maxCumulativePlanJsonCharacters = 16,
        statementTimeoutSeconds = 1,
        explainStatementBudgetSeconds = 1
      )
      .fold(errors => fail(errors.map(_.code).mkString(",")), identity)
    assertEquals(lower.maxCandidates, 100L)
    assertEquals(lower.maxStageProbesPerCandidate, 2L)
    assertEquals(lower.explainStatementBudgetSeconds, 1)
  }

  test("numeric envelope admits exact PostgreSQL-scale values and rejects hostile precision or scale") {
    assertEquals(
      PostgreSqlFactorAwareNumericEnvelope.Scheme,
      "postgresql_factor_numeric_precision_scale_v1"
    )
    assertEquals(PostgreSqlFactorAwareNumericEnvelope.MaxInputPrecision, 64)
    assertEquals(PostgreSqlFactorAwareNumericEnvelope.MaxDerivedPrecision, 128)
    assertEquals(PostgreSqlFactorAwareNumericEnvelope.MaxAbsoluteScale, 1024)

    val precision64 = BigDecimal("1234567890123456789012345678901234567890123456789012345678901234")
    val ordinaryFloat8Magnitude = BigDecimal("1e308")
    val smallestAllowedScale = BigDecimal("1e-1024")

    assert(PostgreSqlFactorAwareNumericEnvelope.accepts(precision64))
    assert(PostgreSqlFactorAwareNumericEnvelope.accepts(ordinaryFloat8Magnitude))
    assert(PostgreSqlFactorAwareNumericEnvelope.accepts(smallestAllowedScale))
    assertEquals(
      PostgreSqlFactorAwareScore.exactEstimatedBytes(
        ordinaryFloat8Magnitude,
        Long.MaxValue
      ),
      BigDecimal(
        ordinaryFloat8Magnitude.bigDecimal.multiply(
          java.math.BigDecimal.valueOf(Long.MaxValue)
        )
      )
    )

    val hostile = Vector(
      BigDecimal("12345678901234567890123456789012345678901234567890123456789012345"),
      BigDecimal("1e-1025"),
      BigDecimal("1e1025")
    )
    hostile.foreach(value =>
      assert(!PostgreSqlFactorAwareNumericEnvelope.accepts(value), value.toString)
    )
  }

  test("estimated-byte arithmetic and every score tie-break are exact") {
    val rows = BigDecimal(
      "1234567890123456789012345678901234567890.12345"
    )
    val width = Long.MaxValue
    val product = PostgreSqlFactorAwareScore.exactEstimatedBytes(rows, width)
    val expectedProduct = BigDecimal(
      rows.bigDecimal.multiply(java.math.BigDecimal.valueOf(width))
    )
    assertEquals(product, expectedProduct)
    assertEquals(
      product.bigDecimal.toPlainString,
      "11386878955363490700007386300000738630000073800382420759303.37415"
    )

    val exactSum = PostgreSqlFactorAwareScore.exactSum(Vector(product, product))
    assertEquals(
      exactSum,
      BigDecimal(product.bigDecimal.add(product.bigDecimal))
    )
    assertNotEquals(exactSum, product + product)

    def score(
        peak: String,
        sum: String,
        cost: String,
        rank: Int,
        idRank: Int
    ): PostgreSqlFactorAwareScore =
      PostgreSqlFactorAwareScore(
        BigDecimal(peak),
        BigDecimal(sum),
        BigDecimal(cost),
        rank,
        candidateId(idRank)
      )

    val ordering = PostgreSqlFactorAwareScore.ordering
    assert(ordering.lt(score("1", "999", "999", 99, 99), score("2", "0", "0", 1, 1)))
    assert(ordering.lt(score("2", "3", "999", 99, 99), score("2", "4", "0", 1, 1)))
    assert(ordering.lt(score("2", "4", "5", 99, 99), score("2", "4", "6", 1, 1)))
    assert(ordering.lt(score("2", "4", "6", 1, 99), score("2", "4", "6", 2, 1)))
    assert(ordering.lt(score("2", "4", "6", 2, 1), score("2", "4", "6", 2, 2)))
  }

  test("row guard remains exact when the inclusive 10x boundary has precision 65") {
    val denominatorText = "9" * 64
    val sourceRows = BigDecimal(denominatorText)
    val exactUpperBoundary = BigDecimal(denominatorText + "0")
    val oneAbove = BigDecimal(BigInt(denominatorText + "0") + 1)

    assertEquals(sourceRows.precision, 64)
    assertEquals(exactUpperBoundary.precision, 65)
    assert(
      PostgreSqlExplainRowGuardEvidence
        .from(sourceRows, exactUpperBoundary)
        .withinGuard
    )
    assert(
      !PostgreSqlExplainRowGuardEvidence
        .from(sourceRows, oneAbove)
        .withinGuard
    )
  }

  test("Q5-derived replay verifies the exact retained K=10/25/50/100 prefixes") {
    assertEquals(q5.found.requestedLimit, 100)
    assertEquals(q5.found.plans.size, 100)
    assertEquals(
      q5.found.plans.groupBy(_.decomposition.bags.size).view
        .mapValues(_.size)
        .toMap,
      Map(3 -> 17, 4 -> 83)
    )

    val verified = PostgreSqlFactorAwarePrefixVerifier
      .verify(q5.found, q5PrefixReruns)
      .getOrElse(fail("genuine Q5-derived prefix reruns did not verify"))
    assertEquals(
      verified.map(_.requestedPrefix),
      PostgreSqlFactorAwareProgressiveV1.Checkpoints
    )
    assertEquals(verified.map(_.effectiveCandidateCount), Vector(10, 25, 50, 100))
    assert(verified.forall(!_.truncatedBySearchExhaustion))
    verified.foreach { proof =>
      assertEquals(
        proof.verifiedPrefixSha256,
        PostgreSqlFactorAwarePrefixProof.digest(
          q5.found.plans.take(proof.effectiveCandidateCount)
        )
      )
    }
    assertEquals(
      PostgreSqlFactorAwarePrefixProof.Scheme,
      "postgresql_factor_prefix_rank_id_score_decomposition_certificate_length_prefixed_v1"
    )
  }

  test("prefix replay records genuine search exhaustion without pretending it reached K") {
    assertEquals(triangle.found.requestedLimit, 100)
    assertEquals(triangle.found.plans.size, 4)
    val verified = PostgreSqlFactorAwarePrefixVerifier
      .verify(triangle.found, trianglePrefixReruns)
      .getOrElse(fail("exhausted triangle prefixes did not verify"))

    assertEquals(verified.map(_.effectiveCandidateCount), Vector.fill(4)(4))
    assert(verified.forall(_.truncatedBySearchExhaustion))
    assertEquals(verified.map(_.verifiedPrefixSha256).distinct.size, 1)
  }

  test("prefix verifier fails closed on drift, truncation, and hostile payloads") {
    def replace(
        index: Int,
        decision: CandidatePlanDecision
    ): Vector[(Int, CandidatePlanDecision)] =
      q5PrefixReruns.updated(
        index,
        PostgreSqlFactorAwareProgressiveV1.Checkpoints(index) -> decision
      )

    val at25 = q5PrefixReruns(1)._2
      .asInstanceOf[CandidatePlanDecision.Found]
    val reordered = at25.copy(plans = at25.plans.updated(0, at25.plans(1)))
    assertEquals(
      PostgreSqlFactorAwarePrefixVerifier.verify(q5.found, replace(1, reordered)),
      None
    )

    val mutated = at25.plans.head.copy(
      decomposition = at25.plans(1).decomposition
    )
    val wrongDecomposition = at25.copy(plans = mutated +: at25.plans.tail)
    assertEquals(
      PostgreSqlFactorAwarePrefixVerifier.verify(
        q5.found,
        replace(1, wrongDecomposition)
      ),
      None
    )

    val short100 = q5PrefixReruns.last._2
      .asInstanceOf[CandidatePlanDecision.Found]
      .copy(plans = q5.found.plans.dropRight(1))
    assertEquals(
      PostgreSqlFactorAwarePrefixVerifier.verify(q5.found, replace(3, short100)),
      None
    )

    val hostileFull = q5.found.copy(
      plans = q5.found.plans.updated(0, q5.found.plans.head.copy(score = null))
    )
    assertEquals(
      PostgreSqlFactorAwarePrefixVerifier.verify(hostileFull, q5PrefixReruns),
      None
    )
    val hostileReruns = Vector(
      null.asInstanceOf[(Int, CandidatePlanDecision)]
    )
    assertEquals(
      PostgreSqlFactorAwarePrefixVerifier.verify(q5.found, hostileReruns),
      None
    )
  }

  test("pure evaluator replay binds source, physical variant, stage, digest, order, and provenance") {
    val request = factorRequest(2)
    val batch = successfulBatch(request)()
    val verified = PostgreSqlFactorAwareReplay
      .verify(request, batch)
      .getOrElse(fail("valid factor-aware batch did not replay"))
    assertEquals(verified.observationsByCandidate.map(_.size), Vector(1, 1))

    val mutations = Vector(
      batch.copy(observations = batch.observations.reverse),
      batch.copy(observations = batch.observations.dropRight(1)),
      batch.copy(
        observations = batch.observations.updated(
          0,
          batch.observations.head.copy(statementSha256 = "0" * 64)
        )
      ),
      batch.copy(
        source = batch.source.copy(sourceStatementSha256 = "0" * 64)
      )
    )
    mutations.foreach(value =>
      assertEquals(PostgreSqlFactorAwareReplay.verify(request, value), None)
    )
    assertEquals(PostgreSqlFactorAwareReplay.verify(null, batch), None)
    assertEquals(PostgreSqlFactorAwareReplay.verify(request, null), None)

    val stageFailure = PostgreSqlFactorAwareExplainFailure(
      PostgreSqlExplainCostFailureCode.StatementFailed,
      PostgreSqlExplainEvaluationStage.StageExplain,
      Some(1),
      Some(batch.source),
      batch.observations.take(1)
    )
    assert(PostgreSqlFactorAwareReplay.verifyFailurePrefix(request, stageFailure))

    val nonPrefix = stageFailure.copy(
      completed = Vector(batch.observations(1))
    )
    assert(!PostgreSqlFactorAwareReplay.verifyFailurePrefix(request, nonPrefix))

    val cleanup = PostgreSqlFactorAwareExplainFailure(
      PostgreSqlExplainCostFailureCode.CleanupFailed,
      PostgreSqlExplainEvaluationStage.Session,
      None,
      Some(batch.source),
      batch.observations
    )
    assert(PostgreSqlFactorAwareReplay.verifyFailurePrefix(request, cleanup))
    assert(
      !PostgreSqlFactorAwareFailureMatrix.accepts(
        PostgreSqlExplainCostFailureCode.StatementFailed,
        PostgreSqlExplainEvaluationStage.Session
      )
    )
    intercept[IllegalArgumentException] {
      cleanup.copy(code = PostgreSqlExplainCostFailureCode.StatementFailed)
    }

    val invalidCodeStagePairs = Vector(
      PostgreSqlExplainCostFailureCode.CleanupFailed ->
        PostgreSqlExplainEvaluationStage.SourceExplain,
      PostgreSqlExplainCostFailureCode.ConnectionUnavailable ->
        PostgreSqlExplainEvaluationStage.StageExplain,
      PostgreSqlExplainCostFailureCode.FactorCandidatePlanningFailed ->
        PostgreSqlExplainEvaluationStage.Session
    )
    invalidCodeStagePairs.foreach { case (code, stage) =>
      assert(!PostgreSqlFactorAwareFailureMatrix.accepts(code, stage))
      intercept[IllegalArgumentException] {
        PostgreSqlFactorAwareExplainFailure(
          code,
          stage,
          if (stage == PostgreSqlExplainEvaluationStage.StageExplain) Some(0)
          else None,
          if (stage == PostgreSqlExplainEvaluationStage.StageExplain)
            Some(batch.source)
          else None,
          Vector.empty
        )
      }
    }
  }

  test("legacy injected evaluators fail closed for the factor-aware boundary") {
    val legacyOnly = new PostgreSqlExplainCostEvaluator {
      override def evaluate(
          request: PostgreSqlExplainCostBatchRequest,
          limits: PostgreSqlExplainSelectionLimits
      ): Either[
        PostgreSqlExplainCostEvaluationFailure,
        PostgreSqlExplainCostBatch
      ] =
        Left(
          PostgreSqlExplainCostEvaluationFailure(
            PostgreSqlExplainCostFailureCode.StatementFailed,
            PostgreSqlExplainEvaluationStage.Session,
            None,
            None,
            Vector.empty
          )
        )
    }

    val failure = legacyOnly
      .evaluateFactorAware(
        factorRequest(1),
        PostgreSqlFactorAwareSelectionLimits.default
      )
      .left
      .toOption
      .getOrElse(fail("legacy evaluator unexpectedly accepted factor evidence"))
    assertEquals(
      failure.code,
      PostgreSqlExplainCostFailureCode.EvaluatorContractViolation
    )
    assertEquals(failure.stage, PostgreSqlExplainEvaluationStage.Session)
    assertEquals(failure.completed, Vector.empty)
  }
}
