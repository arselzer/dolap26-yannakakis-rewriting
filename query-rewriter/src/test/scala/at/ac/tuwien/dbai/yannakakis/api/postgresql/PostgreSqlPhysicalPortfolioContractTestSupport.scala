package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorCoverAssignment,
  BagFactorPhysicalConfiguration
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlStageId,
  PostgreSqlStageKind,
  PostgreSqlStagePhysicalForm,
  PostgreSqlStageTrace
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, ValidationError}

/** Small SQL-free fixtures for the independently reviewed portfolio contracts. */
private[postgresql] object PostgreSqlPhysicalPortfolioContractTestSupport {
  final case class Fixture(
      group: PostgreSqlPhysicalPortfolioWorkloadGroupEvidence,
      traces: Vector[PostgreSqlStageTrace],
      statements: Vector[PostgreSqlPhysicalPortfolioExplainStageStatement],
      evaluations: Vector[PostgreSqlPhysicalPortfolioStageEvaluation]
  )

  final case class DecisionFixture(
      enumeration: PostgreSqlPhysicalPortfolioEnumeration,
      rawRenderings: Vector[PostgreSqlPhysicalPortfolioRawRenderingEvidence],
      deduplication: PostgreSqlPhysicalPortfolioDeduplicationEvidence,
      explainRequest: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      evaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      evaluationUsage: PostgreSqlPhysicalPortfolioEvaluationUsage,
      explainBatch: PostgreSqlPhysicalPortfolioExplainBatch,
      evaluation: PostgreSqlPhysicalPortfolioEvaluationEvidence
  )

  val provenance: PostgreSqlExplainCostProvenance =
    PostgreSqlExplainCostProvenance(
      serverVersionNum = 180004,
      plannerSettingsFingerprintSha256 = "d" * 64,
      plannerSettingsFingerprintScheme =
        PostgreSqlExplainCostProvenance.PlannerSettingsFingerprintScheme,
      plannerSettingCount =
        PostgreSqlExplainCostProvenance.PlannerSettingNames.size
    )

  val sourceSql: String = "SELECT 42 AS source_value"

  val sourceStatement: PostgreSqlExplainSourceStatement =
    PostgreSqlExplainSourceStatement(digest(sourceSql), sourceSql)

  def sourceEvaluation(
      rows: BigDecimal = BigDecimal(100),
      observedProvenance: PostgreSqlExplainCostProvenance = provenance
  ): PostgreSqlExplainSourceEvaluation =
    PostgreSqlExplainSourceEvaluation(
      sourceStatement.sourceStatementSha256,
      estimate(
        totalCost = BigDecimal(10),
        rows = rows,
        observedProvenance = observedProvenance
      )
    )

  def fixture(index: Int = 1): Fixture = {
    require(index > 0)
    val b1 = bagId(2)
    val b2 = bagId(3)
    val support1 = PostgreSqlStageId.BagSupport(b1)
    val support2 = PostgreSqlStageId.BagSupport(b2)
    val factor10 = PostgreSqlStageId.BagFactorVersion(b1, 0)
    val factor20 = PostgreSqlStageId.BagFactorVersion(b2, 0)
    val factor11 = PostgreSqlStageId.BagFactorVersion(b1, 1)
    val named = PostgreSqlStagePhysicalForm.PlannerControlledCte
    val traces = Vector(
      PostgreSqlStageTrace(
        support1,
        PostgreSqlStageKind.Support,
        named,
        Some(s"support_${index}_b2"),
        Vector(b1),
        Vector(edgeId(1)),
        Vector.empty,
        Vector.empty
      ),
      PostgreSqlStageTrace(
        support2,
        PostgreSqlStageKind.Support,
        PostgreSqlStagePhysicalForm.Elided,
        None,
        Vector(b2),
        Vector(edgeId(2)),
        Vector.empty,
        Vector.empty
      ),
      PostgreSqlStageTrace(
        factor10,
        PostgreSqlStageKind.InitialFactor,
        named,
        Some(s"factor_${index}_b2_v0"),
        Vector(b1),
        Vector(edgeId(1)),
        Vector.empty,
        Vector(support1)
      ),
      PostgreSqlStageTrace(
        factor20,
        PostgreSqlStageKind.InitialFactor,
        named,
        Some(s"factor_${index}_b3_v0"),
        Vector(b2),
        Vector(edgeId(2)),
        Vector.empty,
        Vector.empty
      ),
      PostgreSqlStageTrace(
        factor11,
        PostgreSqlStageKind.FusedReduction,
        named,
        Some(s"factor_${index}_b2_v1"),
        Vector(b1, b2),
        Vector.empty,
        Vector.empty,
        Vector(factor10, factor20)
      ),
      PostgreSqlStageTrace(
        PostgreSqlStageId.FinalReconstruction,
        PostgreSqlStageKind.FinalReconstruction,
        PostgreSqlStagePhysicalForm.TerminalSelect,
        None,
        Vector(b1, b2),
        Vector.empty,
        Vector.empty,
        Vector(factor11, factor20)
      )
    )

    val sqlByStage = Vector(
      support1 -> s"SELECT $index AS support_b2",
      factor10 -> s"SELECT $index AS factor_b2_v0",
      factor20 -> s"SELECT $index AS factor_b3_v0",
      factor11 -> s"SELECT $index AS factor_b2_v1",
      PostgreSqlStageId.FinalReconstruction ->
        s"SELECT $index AS final_reconstruction"
    )
    val namedCount = sqlByStage.size - 1
    val statements = sqlByStage.zipWithIndex.map {
      case ((stageId, sql), statementIndex) =>
        val prefix =
          if (statementIndex == namedCount) namedCount.toLong
          else statementIndex.toLong + 1L
        PostgreSqlPhysicalPortfolioExplainStageStatement(
          stageId,
          digest(sql),
          sql.length.toLong,
          prefix,
          sql
        )
    }
    val terminal = statements.last
    val identities = statements.map(_.identity)
    val descriptor = PostgreSqlPhysicalPortfolioWorkloadDescriptor(
      PostgreSqlPhysicalPortfolioWorkloadDescriptor.computeId(
        terminal.statementSha256,
        terminal.characterCount,
        identities,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      ),
      terminal.statementSha256,
      terminal.characterCount,
      identities.size,
      identities,
      PostgreSqlCteMaterializationDirective.PlannerControlled,
      PostgreSqlCteMaterializationDirective.Scope,
      PostgreSqlPhysicalPortfolioWorkloadId.Scheme,
      PostgreSqlStageId.Scheme,
      PostgreSqlStageTrace.OrderScheme,
      at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageDiagnosticStatement.HashScheme,
      at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageDiagnosticStatement.CharacterCountScheme
    )
    val candidate = candidateId(index)
    val configuration = BagFactorPhysicalConfiguration(
      b1,
      Vector(
        BagFactorCoverAssignment(b1, Vector(edgeId(1))),
        BagFactorCoverAssignment(b2, Vector(edgeId(2)))
      )
    )
    val physicalId = PostgreSqlPhysicalVariantId.fromV2(
      candidate,
      configuration,
      YannakakisReductionMode.FullTwoPass,
      PostgreSqlCteMaterializationDirective.PlannerControlled
    )
    val alias = PostgreSqlPhysicalPortfolioWorkloadAlias(
      enumerationOrdinal = index - 1,
      structuralRank = index,
      candidate,
      physicalId
    )
    val group = PostgreSqlPhysicalPortfolioWorkloadGroupEvidence(
      descriptor,
      alias,
      Vector(alias),
      earliestEnumerationOrdinal = index - 1
    )

    val statementById = statements.map(value => value.stageId -> value).toMap
    val estimateById = Map[PostgreSqlStageId, PostgreSqlExplainCostEstimate](
      support1 -> estimate(rows = BigDecimal(10), width = 2L),
      factor10 -> estimate(rows = BigDecimal(30), width = 2L),
      factor20 -> estimate(rows = BigDecimal(40), width = 3L),
      factor11 -> estimate(rows = BigDecimal(5), width = 4L),
      PostgreSqlStageId.FinalReconstruction ->
        estimate(totalCost = BigDecimal(9), rows = BigDecimal(100), width = 7L)
    )
    val frontier = Set[PostgreSqlStageId](support1, factor20, factor11)
    val evaluations = traces.map { trace =>
      statementById.get(trace.id) match {
        case Some(statement) =>
          PostgreSqlPhysicalPortfolioStageEvaluation(
            trace,
            Some(statement.statementSha256),
            Some(statement.characterCount),
            Some(statement.ctePrefixCount),
            Some(estimateById(trace.id)),
            frontier(trace.id)
          )
        case None =>
          PostgreSqlPhysicalPortfolioStageEvaluation(
            trace,
            None,
            None,
            None,
            None,
            finalFrontier = false
          )
      }
    }
    Fixture(group, traces, statements, evaluations)
  }

  def request(count: Int = 2): PostgreSqlPhysicalPortfolioExplainBatchRequest = {
    require(count > 0)
    val workloads = (1 to count).toVector.map { index =>
      val value = fixture(index)
      PostgreSqlPhysicalPortfolioExplainWorkload(value.group, value.statements)
    }
    PostgreSqlPhysicalPortfolioExplainBatchRequest(
      sourceStatement,
      workloads,
      PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum,
      PostgreSqlCteMaterializationDirective.Scope
    )
  }

  def successfulBatch(
      activeRequest: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      sourceRows: BigDecimal = BigDecimal(100),
      observedProvenance: PostgreSqlExplainCostProvenance = provenance
  ): PostgreSqlPhysicalPortfolioExplainBatch = {
    val source = sourceEvaluation(sourceRows, observedProvenance)
    val observations = activeRequest.flattened.zipWithIndex.map {
      case ((workload, statement), index) =>
        PostgreSqlPhysicalPortfolioExplainObservation(
          workload.group.descriptor.workloadId,
          workload.group.representative.physicalVariantId,
          statement.stageId,
          statement.statementSha256,
          estimate(
            totalCost = BigDecimal(index + 1),
            rows = BigDecimal(index + 2),
            width = index.toLong + 1L,
            observedProvenance = observedProvenance
          )
        )
    }
    PostgreSqlPhysicalPortfolioExplainBatch(
      observedProvenance,
      source,
      observations
    )
  }

  def evidence(index: Int = 1): PostgreSqlPhysicalPortfolioEvaluationEvidence = {
    val value = fixture(index)
    val source = sourceEvaluation()
    val frontier = PostgreSqlPhysicalPortfolioFinalFrontier
      .derive(value.traces)
      .fold(problem => throw new AssertionError(problem), identity)
    val score = PostgreSqlPhysicalPortfolioScore(
      maxAllNamedStagePlanRowsTimesPlanWidth = BigDecimal(120),
      sumFinalFrontierPlanRowsTimesPlanWidth = BigDecimal(160),
      finalPlanTotalCost = BigDecimal(9),
      structuralRank = value.group.representative.structuralRank,
      candidateId = value.group.representative.candidateId,
      representativePhysicalVariantId =
        value.group.representative.physicalVariantId
    )
    PostgreSqlPhysicalPortfolioEvaluationEvidence(
      value.group,
      source,
      value.evaluations,
      frontier,
      PostgreSqlExplainRowGuardEvidence.from(
        source.estimate.planRows,
        value.evaluations.last.estimate.get.planRows
      ),
      score,
      PostgreSqlPhysicalPortfolioEvaluationV1.ScoreScheme,
      PostgreSqlPhysicalPortfolioEvaluationV1.FinalFrontierScheme,
      PostgreSqlFactorAwareNumericEnvelope.Scheme
    )
  }

  def decisionFixture(
      terminalRows: BigDecimal = BigDecimal(100)
  ): DecisionFixture = {
    val base = fixture()
    val candidate = candidateId(1)
    val b1 = bagId(2)
    val b2 = bagId(3)
    val configuration = BagFactorPhysicalConfiguration(
      b1,
      Vector(
        BagFactorCoverAssignment(b1, Vector(edgeId(1))),
        BagFactorCoverAssignment(b2, Vector(edgeId(2)))
      )
    )
    def variant(
        mode: YannakakisReductionMode,
        localOrdinal: Int
    ): PostgreSqlPhysicalVariant =
      PostgreSqlPhysicalVariant(
        candidate,
        structuralRank = 1,
        configuration,
        mode,
        PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
        PostgreSqlCteMaterializationDirective.PlannerControlled,
        PostgreSqlCteMaterializationDirective.Scope,
        PostgreSqlPhysicalVariantId.fromV2(
          candidate,
          configuration,
          mode,
          PostgreSqlCteMaterializationDirective.PlannerControlled
        ),
        localOrdinal
      )
    def entry(
        ordinal: Int,
        physical: PostgreSqlPhysicalVariant,
        phase: String,
        lane: Option[String],
        modeOrdinal: Int
    ): PostgreSqlPhysicalPortfolioEntry =
      PostgreSqlPhysicalPortfolioEntry(
        ordinal,
        physical,
        phase,
        lane,
        PostgreSqlPhysicalVariantHeapKey(
          BigInt(modeOrdinal),
          rootOrdinal = 0,
          coverIndexVector = Vector(0, 0),
          modeOrdinal,
          directiveOrdinal = 0,
          physical.physicalVariantId
        )
      )
    val entries = Vector(
      entry(
        0,
        variant(YannakakisReductionMode.FullTwoPass, 0),
        PostgreSqlPhysicalPortfolioEnumerationPhase.Anchor,
        None,
        modeOrdinal = 0
      ),
      entry(
        1,
        variant(YannakakisReductionMode.NoReduction, 1),
        PostgreSqlPhysicalPortfolioEnumerationPhase.AxisSentinel,
        Some(PostgreSqlPhysicalPortfolioAxis.NoReduction),
        modeOrdinal = 2
      )
    )
    val candidateEvidence = PostgreSqlPhysicalCandidateEnumerationEvidence(
      candidate,
      structuralRank = 1,
      exactRootCount = BigInt(1),
      exactCoverAssignmentCount = BigInt(1),
      exactLogicalVariantCount = BigInt(2),
      applicableAxes = Vector(PostgreSqlPhysicalPortfolioAxis.NoReduction),
      retainedCoverStateCount = 1,
      coverStateSearchExhausted = true,
      retainedLocalVariantCount = 2,
      retainedCrossProductStateCount = 0,
      localVariantSearchExhausted = true
    )
    val enumeration = PostgreSqlPhysicalPortfolioEnumeration(
      entries,
      Vector(candidateEvidence),
      exactLogicalVariantCount = BigInt(2),
      crossProductReserveTarget = 0,
      globalWaveSearchExhausted = true,
      PostgreSqlPhysicalPortfolioLimits.default
    )
    val aliases = entries
      .map(entry =>
        PostgreSqlPhysicalPortfolioWorkloadAlias(
          entry.enumerationOrdinal,
          entry.variant.structuralRank,
          entry.variant.candidateId,
          entry.variant.physicalVariantId
        )
      )
      .sortBy(_.physicalVariantId.value)
    val group = PostgreSqlPhysicalPortfolioWorkloadGroupEvidence(
      base.group.descriptor,
      aliases.head,
      aliases,
      earliestEnumerationOrdinal = 0
    )
    val rawRenderings = entries.map(entry =>
      PostgreSqlPhysicalPortfolioRawRenderingEvidence(
        entry,
        base.group.descriptor
      )
    )
    val deduplication = PostgreSqlPhysicalPortfolioDeduplicationEvidence(
      Vector(group),
      rawVariantCount = entries.size,
      uniqueWorkloadCount = 1,
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.DeduplicationScheme
    )
    val workload = PostgreSqlPhysicalPortfolioExplainWorkload(
      group,
      base.statements
    )
    val explainRequest = PostgreSqlPhysicalPortfolioExplainBatchRequest(
      sourceStatement,
      Vector(workload),
      PostgreSqlCteMaterializationDirective.MinimumExplicitServerVersionNum,
      PostgreSqlCteMaterializationDirective.Scope
    )
    val source = sourceEvaluation()
    val stages = base.evaluations.updated(
      base.evaluations.size - 1,
      base.evaluations.last.copy(
        estimate = base.evaluations.last.estimate.map(
          _.copy(planRows = terminalRows)
        )
      )
    )
    val emitted = stages.filter(_.trace.physicalForm.emitted)
    val observations = emitted.map(stage =>
      PostgreSqlPhysicalPortfolioExplainObservation(
        group.descriptor.workloadId,
        group.representative.physicalVariantId,
        stage.trace.id,
        stage.diagnosticStatementSha256.get,
        stage.estimate.get
      )
    )
    val explainBatch = PostgreSqlPhysicalPortfolioExplainBatch(
      provenance,
      source,
      observations
    )
    val frontier = PostgreSqlPhysicalPortfolioFinalFrontier
      .derive(base.traces)
      .fold(problem => throw new AssertionError(problem), identity)
    val score = PostgreSqlPhysicalPortfolioScore(
      BigDecimal(120),
      BigDecimal(160),
      BigDecimal(9),
      group.representative.structuralRank,
      group.representative.candidateId,
      group.representative.physicalVariantId
    )
    val evaluation = PostgreSqlPhysicalPortfolioEvaluationEvidence(
      group,
      source,
      stages,
      frontier,
      PostgreSqlExplainRowGuardEvidence.from(
        source.estimate.planRows,
        terminalRows
      ),
      score,
      PostgreSqlPhysicalPortfolioEvaluationV1.ScoreScheme,
      PostgreSqlPhysicalPortfolioEvaluationV1.FinalFrontierScheme,
      PostgreSqlFactorAwareNumericEnvelope.Scheme
    )
    val evaluationLimits = PostgreSqlPhysicalPortfolioEvaluationLimits.default
    val evaluationUsage = PostgreSqlPhysicalPortfolioEvaluationEnvelope
      .validate(explainRequest, evaluationLimits)
      .fold(problem => throw new AssertionError(problem.toString), identity)
    DecisionFixture(
      enumeration,
      rawRenderings,
      deduplication,
      explainRequest,
      evaluationLimits,
      evaluationUsage,
      explainBatch,
      evaluation
    )
  }

  def estimate(
      totalCost: BigDecimal = BigDecimal(1),
      rows: BigDecimal = BigDecimal(1),
      width: Long = 8L,
      observedProvenance: PostgreSqlExplainCostProvenance = provenance
  ): PostgreSqlExplainCostEstimate =
    PostgreSqlExplainCostEstimate(
      startupCost = BigDecimal("0.125"),
      planTotalCost = totalCost,
      planRows = rows,
      planWidth = width,
      provenance = observedProvenance
    )

  def digest(sql: String): String =
    PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(sql)

  def candidateId(rank: Int): CandidatePlanId =
    CandidatePlanId
      .parse("sha256:" + f"$rank%064x")
      .fold(problem => throw new AssertionError(problem.message), identity)

  def bagId(value: Int): BagId = validated(BagId.from(value))
  def edgeId(value: Int): EdgeId = validated(EdgeId.from(value))

  private def validated[A](value: Either[ValidationError, A]): A =
    value.fold(problem => throw new AssertionError(problem.message), identity)
}
