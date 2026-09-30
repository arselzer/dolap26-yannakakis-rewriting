package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlStageId,
  PostgreSqlStageKind,
  PostgreSqlStagePhysicalForm
}

class PostgreSqlPhysicalPortfolioEvaluationContractSuite extends munit.FunSuite {
  import PostgreSqlPhysicalPortfolioContractTestSupport._

  test("evaluation limits freeze workloads, accept reviewed maxima, and allow coherent lower envelopes") {
    val defaults = PostgreSqlPhysicalPortfolioEvaluationLimits.default
    assertEquals(
      PostgreSqlPhysicalPortfolioEvaluationLimits.create(),
      Right(defaults)
    )
    val lower = PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(
        maxWorkloads = 256L,
        maxStageProbesPerWorkload = 2L,
        maxCharactersPerProbe = 10L,
        maxDiagnosticCharactersPerWorkload = 20L,
        maxRawDiagnosticCharactersRetained = 20L,
        maxExplainStatementsPerSession = 3L,
        maxDiagnosticCharactersPerSession = 20L,
        maxPlanJsonCharacters = 10L,
        maxCumulativePlanJsonCharacters = 20L,
        statementTimeoutSeconds = 1,
        explainStatementBudgetSeconds = 2
      )
      .fold(problems => fail(problems.map(_.code).mkString(",")), identity)
    assertEquals(lower.maxWorkloads, 256L)
    assertEquals(lower.stageDiagnosticLimits.maxStatements, 2L)

    val frozen = PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(maxWorkloads = 255L)
      .left
      .toOption
      .get
    assertEquals(
      frozen.map(_.code),
      Vector("physical-portfolio-evaluation.limit.frozen.workloads")
    )
  }

  test("every evaluation maximum rejects max plus one with its exact resource code") {
    val defaults = PostgreSqlPhysicalPortfolioEvaluationLimits.default
    val cases = Vector[(String, () => Vector[PostgreSqlPhysicalPortfolioEvaluationLimitError])](
      "stage-probes-per-workload" -> (() =>
        errors(maxStageProbesPerWorkload = defaults.maxStageProbesPerWorkload + 1L)),
      "characters-per-probe" -> (() =>
        errors(maxCharactersPerProbe = defaults.maxCharactersPerProbe + 1L)),
      "diagnostic-characters-per-workload" -> (() =>
        errors(
          maxDiagnosticCharactersPerWorkload =
            defaults.maxDiagnosticCharactersPerWorkload + 1L
        )),
      "raw-diagnostic-characters-retained" -> (() =>
        errors(
          maxRawDiagnosticCharactersRetained =
            defaults.maxRawDiagnosticCharactersRetained + 1L
        )),
      "explain-statements-per-session" -> (() =>
        errors(
          maxExplainStatementsPerSession =
            defaults.maxExplainStatementsPerSession + 1L
        )),
      "diagnostic-characters-per-session" -> (() =>
        errors(
          maxDiagnosticCharactersPerSession =
            defaults.maxDiagnosticCharactersPerSession + 1L
        )),
      "plan-json-characters" -> (() =>
        errors(maxPlanJsonCharacters = defaults.maxPlanJsonCharacters + 1L)),
      "cumulative-plan-json-characters" -> (() =>
        errors(
          maxCumulativePlanJsonCharacters =
            defaults.maxCumulativePlanJsonCharacters + 1L
        )),
      "statement-timeout-seconds" -> (() =>
        errors(statementTimeoutSeconds = defaults.statementTimeoutSeconds + 1)),
      "explain-statement-budget-seconds" -> (() =>
        errors(
          explainStatementBudgetSeconds =
            defaults.explainStatementBudgetSeconds + 1
        ))
    )
    cases.foreach { case (resource, evaluate) =>
      assert(
        evaluate().exists(
          _.code == s"physical-portfolio-evaluation.limit.maximum.$resource"
        ),
        resource
      )
    }
  }

  test("invalid, integer-materialization, and cross-resource evaluation limits fail closed") {
    assert(
      errors(maxStageProbesPerWorkload = 0L).exists(
        _.code ==
          "physical-portfolio-evaluation.limit.invalid.stage-probes-per-workload"
      )
    )
    assert(
      errors(maxPlanJsonCharacters = Int.MaxValue.toLong + 1L).exists(
        _.code ==
          "physical-portfolio-evaluation.limit.integer.plan-json-characters"
      )
    )
    assert(
      errors(
        maxCharactersPerProbe = 11L,
        maxDiagnosticCharactersPerWorkload = 10L
      ).exists(
        _.code ==
          "physical-portfolio-evaluation.limit.inconsistent.characters-per-probe.diagnostic-characters-per-workload"
      )
    )
    assert(
      errors(
        maxDiagnosticCharactersPerWorkload = 11L,
        maxRawDiagnosticCharactersRetained = 10L
      ).exists(
        _.code ==
          "physical-portfolio-evaluation.limit.inconsistent.diagnostic-characters-per-workload.raw-diagnostic-characters-retained"
      )
    )
    assert(
      errors(statementTimeoutSeconds = 2, explainStatementBudgetSeconds = 1)
        .exists(
          _.code ==
            "physical-portfolio-evaluation.limit.inconsistent.statement-timeout-seconds.explain-statement-budget-seconds"
        )
    )
    assert(PostgreSqlPhysicalPortfolioEvaluationLimits.validate(null).nonEmpty)
  }

  test("score ordering is exact over every frozen field including huge accepted decimals") {
    val value = fixture()
    val ids = Vector(
      value.group.representative.physicalVariantId,
      fixture(2).group.representative.physicalVariantId
    ).sortBy(_.value)
    val baseline = PostgreSqlPhysicalPortfolioScore(
      BigDecimal(1),
      BigDecimal(2),
      BigDecimal(3),
      structuralRank = 1,
      candidateId(1),
      ids.head
    )
    val ordered = Vector(
      baseline,
      baseline.copy(representativePhysicalVariantId = ids.last),
      baseline.copy(candidateId = candidateId(2)),
      baseline.copy(structuralRank = 2),
      baseline.copy(finalPlanTotalCost = BigDecimal(4)),
      baseline.copy(sumFinalFrontierPlanRowsTimesPlanWidth = BigDecimal(3)),
      baseline.copy(maxAllNamedStagePlanRowsTimesPlanWidth = BigDecimal(2))
    )
    assertEquals(ordered.sorted, ordered)
    assertEquals(
      PostgreSqlPhysicalPortfolioEvaluationV1.ScoreOrder,
      Vector(
        "max_all_named_stage_plan_rows_times_plan_width",
        "sum_final_frontier_plan_rows_times_plan_width",
        "final_plan_total_cost",
        "structural_rank",
        "candidate_id",
        "representative_physical_variant_id"
      )
    )

    val precision128 = BigDecimal("9" * 128)
    val precision64 = BigDecimal("8" * 64)
    val huge = baseline.copy(
      maxAllNamedStagePlanRowsTimesPlanWidth = precision128,
      sumFinalFrontierPlanRowsTimesPlanWidth = precision128,
      finalPlanTotalCost = precision64
    )
    assertEquals(huge.maxAllNamedStagePlanRowsTimesPlanWidth, precision128)

    intercept[IllegalArgumentException] {
      baseline.copy(maxAllNamedStagePlanRowsTimesPlanWidth = BigDecimal("9" * 129))
    }
    intercept[IllegalArgumentException] {
      baseline.copy(finalPlanTotalCost = BigDecimal("9" * 65))
    }
    intercept[IllegalArgumentException] {
      baseline.copy(sumFinalFrontierPlanRowsTimesPlanWidth = BigDecimal("1e-1025"))
    }
  }

  test("stage evaluation binds wholly present emitted evidence and rejects hostile numerics") {
    val value = fixture()
    val emitted = value.evaluations.head
    val elided = value.evaluations(1)
    assert(emitted.estimate.nonEmpty)
    assert(elided.estimate.isEmpty)

    intercept[IllegalArgumentException] {
      emitted.copy(diagnosticStatementSha256 = None)
    }
    intercept[IllegalArgumentException] {
      elided.copy(estimate = Some(estimate()))
    }
    intercept[IllegalArgumentException] {
      value.evaluations.last.copy(finalFrontier = true)
    }
    intercept[IllegalArgumentException] {
      emitted.copy(
        estimate = Some(estimate(rows = BigDecimal("9" * 65)))
      )
    }
  }

  test("final frontier is emitted supports plus latest factor in exact reconstruction order") {
    val value = fixture()
    val expected = Vector(
      PostgreSqlStageId.BagSupport(bagId(2)),
      PostgreSqlStageId.BagFactorVersion(bagId(2), 1),
      PostgreSqlStageId.BagFactorVersion(bagId(3), 0)
    )
    assertEquals(
      PostgreSqlPhysicalPortfolioFinalFrontier.derive(value.traces),
      Right(expected)
    )

    val terminal = value.traces.last
    val swappedInputs = value.traces.updated(
      value.traces.size - 1,
      terminal.copy(inputStageIds = terminal.inputStageIds.reverse)
    )
    assert(PostgreSqlPhysicalPortfolioFinalFrontier.derive(swappedInputs).isLeft)

    val reorderedTerminal = terminal.copy(
      logicalBagIds = terminal.logicalBagIds.reverse,
      inputStageIds = terminal.inputStageIds.reverse
    )
    val reordered = value.traces.updated(value.traces.size - 1, reorderedTerminal)
    assertEquals(
      PostgreSqlPhysicalPortfolioFinalFrontier.derive(reordered),
      Right(
        Vector(
          PostgreSqlStageId.BagSupport(bagId(2)),
          PostgreSqlStageId.BagFactorVersion(bagId(3), 0),
          PostgreSqlStageId.BagFactorVersion(bagId(2), 1)
        )
      )
    )
  }

  test("frontier derivation rejects missing initials, version gaps, duplicate identities, and nonterminal tails") {
    val value = fixture()
    assert(
      PostgreSqlPhysicalPortfolioFinalFrontier
        .derive(value.traces.filterNot(_.id == PostgreSqlStageId.BagFactorVersion(bagId(3), 0)))
        .isLeft
    )
    val gap = value.traces.updated(
      4,
      value.traces(4).copy(id = PostgreSqlStageId.BagFactorVersion(bagId(2), 2))
    )
    assert(PostgreSqlPhysicalPortfolioFinalFrontier.derive(gap).isLeft)
    val duplicate = value.traces.updated(2, value.traces.head)
    assert(PostgreSqlPhysicalPortfolioFinalFrontier.derive(duplicate).isLeft)
    assert(
      PostgreSqlPhysicalPortfolioFinalFrontier
        .derive(value.traces.dropRight(1))
        .isLeft
    )
  }

  test("evaluation evidence recomputes frontier, peak, exact sum, final cost, provenance, and row guard") {
    val value = evidence()
    assertEquals(
      value.finalFrontierStageIds,
      Vector(
        PostgreSqlStageId.BagSupport(bagId(2)),
        PostgreSqlStageId.BagFactorVersion(bagId(2), 1),
        PostgreSqlStageId.BagFactorVersion(bagId(3), 0)
      )
    )
    assertEquals(value.score.maxAllNamedStagePlanRowsTimesPlanWidth, BigDecimal(120))
    assertEquals(value.score.sumFinalFrontierPlanRowsTimesPlanWidth, BigDecimal(160))
    assert(value.rowGuard.withinGuard)

    intercept[IllegalArgumentException] {
      value.copy(
        score = value.score.copy(
          sumFinalFrontierPlanRowsTimesPlanWidth = BigDecimal(159)
        )
      )
    }
    intercept[IllegalArgumentException] {
      value.copy(
        rowGuard = PostgreSqlExplainRowGuardEvidence.from(
          BigDecimal(1000),
          BigDecimal(1)
        )
      )
    }
    intercept[IllegalArgumentException] {
      val changedProvenance = provenance.copy(serverVersionNum = 180005)
      value.copy(
        stages = value.stages.updated(
          0,
          value.stages.head.copy(
            estimate = value.stages.head.estimate.map(
              _.copy(provenance = changedProvenance)
            )
          )
        )
      )
    }
    intercept[IllegalArgumentException] {
      value.copy(
        stages = value.stages.updated(
          0,
          value.stages.head.copy(finalFrontier = false)
        )
      )
    }
  }

  private def errors(
      maxWorkloads: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxWorkloads,
      maxStageProbesPerWorkload: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxStageProbesPerWorkload,
      maxCharactersPerProbe: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxCharactersPerProbe,
      maxDiagnosticCharactersPerWorkload: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxDiagnosticCharactersPerWorkload,
      maxRawDiagnosticCharactersRetained: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxRawDiagnosticCharactersRetained,
      maxExplainStatementsPerSession: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxExplainStatementsPerSession,
      maxDiagnosticCharactersPerSession: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxDiagnosticCharactersPerSession,
      maxPlanJsonCharacters: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxPlanJsonCharacters,
      maxCumulativePlanJsonCharacters: Long = PostgreSqlPhysicalPortfolioEvaluationLimits.default.maxCumulativePlanJsonCharacters,
      statementTimeoutSeconds: Int = PostgreSqlPhysicalPortfolioEvaluationLimits.default.statementTimeoutSeconds,
      explainStatementBudgetSeconds: Int = PostgreSqlPhysicalPortfolioEvaluationLimits.default.explainStatementBudgetSeconds
  ): Vector[PostgreSqlPhysicalPortfolioEvaluationLimitError] =
    PostgreSqlPhysicalPortfolioEvaluationLimits
      .create(
        maxWorkloads = maxWorkloads,
        maxStageProbesPerWorkload = maxStageProbesPerWorkload,
        maxCharactersPerProbe = maxCharactersPerProbe,
        maxDiagnosticCharactersPerWorkload =
          maxDiagnosticCharactersPerWorkload,
        maxRawDiagnosticCharactersRetained =
          maxRawDiagnosticCharactersRetained,
        maxExplainStatementsPerSession = maxExplainStatementsPerSession,
        maxDiagnosticCharactersPerSession =
          maxDiagnosticCharactersPerSession,
        maxPlanJsonCharacters = maxPlanJsonCharacters,
        maxCumulativePlanJsonCharacters = maxCumulativePlanJsonCharacters,
        statementTimeoutSeconds = statementTimeoutSeconds,
        explainStatementBudgetSeconds = explainStatementBudgetSeconds
      )
      .left
      .toOption
      .getOrElse(Vector.empty)
}
