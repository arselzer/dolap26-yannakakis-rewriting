package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidatePlanId,
  CandidateSearchInput,
  CandidateTopKStatistics,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  BagFactorPlanningLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlEnumerationRenderingProfile,
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingLimits,
  PostgreSqlStageDiagnosticLimits,
  PostgreSqlStageDiagnosticRendering,
  PostgreSqlStageId
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  QueryIR,
  QueryResultShape
}

/** One retained rank compiled under the campaign's single physical setting. */
private[postgresql] final case class PostgreSqlCompiledRetainedCandidate(
    plan: RankedCandidatePlan,
    bagFactorPlan: BagFactorPlan,
    renderedQuery: PostgreSqlRenderedQuery,
    stageDiagnostics: PostgreSqlStageDiagnosticRendering,
    physicalVariantId: PostgreSqlPhysicalVariantId,
    renderedStatementSha256: String
) extends Serializable {
  require(plan != null, "compiled retained plan must not be null")
  require(bagFactorPlan != null, "compiled bag-factor plan must not be null")
  require(renderedQuery != null, "compiled rendering must not be null")
  require(stageDiagnostics != null, "stage diagnostics must not be null")
  require(physicalVariantId != null, "physical variant id must not be null")
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}"),
    "rendered statement digest must be lowercase SHA-256"
  )
  require(
    bagFactorPlan.decomposition == plan.decomposition,
    "compiled bag-factor plan must retain the ranked decomposition"
  )
  require(
    renderedQuery.bagFactorPlan == bagFactorPlan &&
      stageDiagnostics.renderedQuery == renderedQuery,
    "compiled rendering and diagnostics must retain the bag-factor plan"
  )
  require(
    physicalVariantId == PostgreSqlPhysicalVariantId.from(
      plan.candidateId,
      YannakakisReductionMode.FullTwoPass
    ),
    "campaign physical variant id must bind full two-pass reduction"
  )
  require(
    renderedStatementSha256 ==
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
        renderedQuery.sql
      ),
    "rendered statement digest must bind the exact SQL bytes"
  )
  require(
    stageDiagnostics.statements.nonEmpty &&
      stageDiagnostics.statements.last.stageId ==
        PostgreSqlStageId.FinalReconstruction &&
      stageDiagnostics.statements.last.statementSha256 ==
        renderedStatementSha256,
    "stage diagnostics must end in the exact terminal statement"
  )
}

/**
  * Policy-neutral complete retained prefix for one exact top-K search.
  *
  * `exhaustedBeforeK` is derived from the exact search decision held here; it
  * is never accepted as caller-supplied evidence. There is deliberately no
  * selected rank or winner.
  */
private[postgresql] final case class PostgreSqlRetainedCandidateBatch private[postgresql] (
    query: QueryIR,
    candidateInput: CandidateSearchInput,
    search: CandidatePlanDecision.Found,
    candidates: Vector[PostgreSqlCompiledRetainedCandidate]
) extends Serializable {
  require(query != null, "batch query must not be null")
  require(candidateInput != null, "batch candidate input must not be null")
  require(search != null, "batch search decision must not be null")
  require(
    candidates != null && candidates.nonEmpty && candidates.forall(_ != null),
    "batch candidates must be nonempty and contain no null"
  )
  require(
    candidates.map(_.plan) == search.plans,
    "batch candidates must equal the complete retained search prefix"
  )
  require(
    candidates.map(_.plan.rank) == (1 to candidates.size).toVector,
    "batch ranks must be contiguous and increasing"
  )
  require(
    candidates.map(_.plan.candidateId).distinct.size == candidates.size,
    "batch candidate ids must be unique"
  )
  require(
    candidates.forall(
      _.renderedQuery.trace.reduction.mode ==
        YannakakisReductionMode.FullTwoPass
    ),
    "batch candidates must use full two-pass reduction"
  )

  val requestedLimit: Int = search.requestedLimit
  val retainedCount: Int = search.plans.size
  val exhaustedBeforeK: Boolean = retainedCount < requestedLimit
  val statistics: CandidateTopKStatistics = search.statistics
  val reductionMode: YannakakisReductionMode =
    YannakakisReductionMode.FullTwoPass
  val materializationDirective: PostgreSqlCteMaterializationDirective =
    PostgreSqlCteMaterializationDirective.PlannerControlled
}

private[postgresql] sealed trait PostgreSqlRetainedCandidateBatchFailure
    extends Product
    with Serializable

private[postgresql] object PostgreSqlRetainedCandidateBatchFailure {
  final case class InvalidCandidateSet(detail: String)
      extends PostgreSqlRetainedCandidateBatchFailure {
    require(detail != null && detail.nonEmpty, "failure detail must not be blank")
  }

  final case class InapplicableResultShape(resultShape: String)
      extends PostgreSqlRetainedCandidateBatchFailure {
    require(
      resultShape != null && resultShape.nonEmpty,
      "result shape must not be blank"
    )
  }

  final case class CompilationFailed(
      structuralRank: Int,
      candidateId: CandidatePlanId,
      cause: PostgreSqlCandidateCompiler.Failure
  ) extends PostgreSqlRetainedCandidateBatchFailure {
    require(structuralRank > 0, "failed structural rank must be positive")
    require(candidateId != null, "failed candidate id must not be null")
    require(cause != null, "compilation failure must not be null")
    require(
      cause.plan.rank == structuralRank && cause.plan.candidateId == candidateId,
      "compilation failure must match its retained candidate"
    )
  }

  final case class SqlTransportFailed(
      structuralRank: Int,
      candidateId: CandidatePlanId,
      assetId: String,
      reason: PostgreSqlEnumerationSqlTransportFailure
  ) extends PostgreSqlRetainedCandidateBatchFailure {
    require(structuralRank > 0, "failed structural rank must be positive")
    require(candidateId != null, "failed candidate id must not be null")
    require(
      assetId != null && assetId.nonEmpty,
      "failed SQL transport asset id must not be blank"
    )
    require(reason != null, "SQL transport failure reason must not be null")
  }
}

/** Compiles every exact retained rank once; it does not select among them. */
private[postgresql] object PostgreSqlRetainedCandidateBatchCompiler {
  val MaximumCampaignPlanLimit: Int = 10

  val DefaultDiagnosticLimits: PostgreSqlStageDiagnosticLimits =
    PostgreSqlStageDiagnosticLimits
      .create(
        maxStatements = 256L,
        maxCharactersPerStatement = 16L * 1024L * 1024L,
        maxTotalCharacters = 64L * 1024L * 1024L
      )
      .fold(
        problems =>
          throw new IllegalStateException(
            s"invalid enumeration batch diagnostic defaults: ${problems.map(_.code).mkString(",")}"
          ),
        identity
      )

  def compile(
      query: QueryIR,
      candidateInput: CandidateSearchInput,
      search: CandidatePlanDecision.Found,
      bagFactorLimits: BagFactorPlanningLimits,
      renderingLimits: PostgreSqlRenderingLimits,
      diagnosticLimits: PostgreSqlStageDiagnosticLimits
  ): Either[
    PostgreSqlRetainedCandidateBatchFailure,
    PostgreSqlRetainedCandidateBatch
  ] =
    compileWithEnumerationProfile(
      query,
      candidateInput,
      search,
      bagFactorLimits,
      renderingLimits,
      diagnosticLimits,
      PostgreSqlEnumerationRenderingProfile.default
    )

  def compileWithEnumerationProfile(
      query: QueryIR,
      candidateInput: CandidateSearchInput,
      search: CandidatePlanDecision.Found,
      bagFactorLimits: BagFactorPlanningLimits,
      renderingLimits: PostgreSqlRenderingLimits,
      diagnosticLimits: PostgreSqlStageDiagnosticLimits,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): Either[
    PostgreSqlRetainedCandidateBatchFailure,
    PostgreSqlRetainedCandidateBatch
  ] = {
    require(query != null, "batch query must not be null")
    require(candidateInput != null, "candidate-search input must not be null")
    require(search != null, "found search decision must not be null")
    require(bagFactorLimits != null, "bag-factor limits must not be null")
    require(renderingLimits != null, "rendering limits must not be null")
    require(diagnosticLimits != null, "diagnostic limits must not be null")
    require(
      enumerationRenderingProfile != null,
      "enumeration rendering profile must not be null"
    )

    if (query.resultShape != QueryResultShape.EnumerationProjection) {
      return Left(
        PostgreSqlRetainedCandidateBatchFailure.InapplicableResultShape(
          query.resultShape.id
        )
      )
    }
    if (
      search.requestedLimit <= 0 ||
      search.requestedLimit > MaximumCampaignPlanLimit ||
      search.plans.size > search.requestedLimit
    ) {
      return Left(
        PostgreSqlRetainedCandidateBatchFailure.InvalidCandidateSet(
          "Exact search decision is outside the campaign K <= 10 envelope"
        )
      )
    }

    PostgreSqlCandidateSelector.validatePlans(search.plans) match {
      case Left(problem) =>
        return Left(
          PostgreSqlRetainedCandidateBatchFailure.InvalidCandidateSet(
            problem.detail
          )
        )
      case Right(_) => ()
    }

    val compiled = Vector.newBuilder[PostgreSqlCompiledRetainedCandidate]
    val iterator = search.plans.iterator
    while (iterator.hasNext) {
      val plan = iterator.next()
      PostgreSqlCandidateCompiler.compileOne(
        query,
        candidateInput,
        plan,
        bagFactorLimits,
        renderingLimits,
        YannakakisReductionMode.FullTwoPass,
        Some(diagnosticLimits),
        enumerationRenderingProfile
      ) match {
        case Left(problem) =>
          return Left(
            PostgreSqlRetainedCandidateBatchFailure.CompilationFailed(
              plan.rank,
              plan.candidateId,
              problem
            )
          )
        case Right(value) =>
          val diagnostics = value.stageDiagnostics.getOrElse(
            throw new IllegalStateException(
              "campaign compilation omitted requested stage diagnostics"
            )
          )
          val transportValues =
            Vector("rendered-sql" -> value.renderedQuery.sql) ++
              diagnostics.statements.map(statement =>
                s"stage:${statement.stageId.code}" -> statement.sql
              )
          transportValues.foreach { case (assetId, sql) =>
            PostgreSqlEnumerationSqlTransport
              .validate(
                sql,
                PostgreSqlEnumerationBatchRequest.MaximumSourceSqlBytes,
                allowEmpty = false
              ) match {
              case Left(reason) =>
                return Left(
                  PostgreSqlRetainedCandidateBatchFailure.SqlTransportFailed(
                    plan.rank,
                    plan.candidateId,
                    assetId,
                    reason
                  )
                )
              case Right(_) => ()
            }
          }
          compiled += PostgreSqlCompiledRetainedCandidate(
            plan,
            value.bagFactorPlan,
            value.renderedQuery,
            diagnostics,
            PostgreSqlPhysicalVariantId.from(
              plan.candidateId,
              YannakakisReductionMode.FullTwoPass
            ),
            PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
              value.renderedQuery.sql
            )
          )
      }
    }

    Right(
      PostgreSqlRetainedCandidateBatch(
        query,
        candidateInput,
        search,
        compiled.result()
      )
    )
  }
}
