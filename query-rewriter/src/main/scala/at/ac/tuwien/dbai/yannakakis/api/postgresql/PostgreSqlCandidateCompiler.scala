package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateSearchInput,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  BagFactorPlanner,
  BagFactorPlanningError,
  BagFactorPlanningLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlBagFactorRenderer,
  PostgreSqlEnumerationRenderingProfile,
  PostgreSqlRenderedQuery,
  PostgreSqlRenderingError,
  PostgreSqlRenderingLimits,
  PostgreSqlStageDiagnosticLimits,
  PostgreSqlStageDiagnosticRendering
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  QueryIR,
  QueryResultShape
}

/**
  * One policy-neutral compilation of an already retained structural plan.
  *
  * This boundary deliberately performs no candidate scoring or selection and
  * never accesses PostgreSQL. The bag-factor planning path is shared by the
  * selectors and campaign producer. SQL emission is explicitly versioned:
  * current callers use the statistics-safe default, while a frozen producer
  * may request its legacy byte contract.
  */
private[postgresql] object PostgreSqlCandidateCompiler {
  sealed trait Failure extends Product with Serializable {
    def plan: RankedCandidatePlan
  }

  object Failure {
    final case class BagFactorPlanningFailed(
        plan: RankedCandidatePlan,
        problems: Vector[BagFactorPlanningError]
    ) extends Failure

    final case class Inapplicable(
        plan: RankedCandidatePlan,
        bagFactorPlan: BagFactorPlan,
        reason: String
    ) extends Failure

    final case class RenderingFailed(
        plan: RankedCandidatePlan,
        bagFactorPlan: BagFactorPlan,
        problems: Vector[PostgreSqlRenderingError]
    ) extends Failure
  }

  final case class Compiled(
      plan: RankedCandidatePlan,
      bagFactorPlan: BagFactorPlan,
      renderedQuery: PostgreSqlRenderedQuery,
      stageDiagnostics: Option[PostgreSqlStageDiagnosticRendering]
  ) extends Serializable {
    require(plan != null, "compiled candidate plan must not be null")
    require(
      bagFactorPlan != null,
      "compiled candidate bag-factor plan must not be null"
    )
    require(
      renderedQuery != null,
      "compiled candidate rendering must not be null"
    )
    require(
      stageDiagnostics != null && stageDiagnostics.forall(_ != null),
      "compiled candidate stage diagnostics must not be null or contain null"
    )
    require(
      bagFactorPlan.decomposition == plan.decomposition,
      "compiled bag-factor plan must retain the ranked decomposition"
    )
    require(
      renderedQuery.bagFactorPlan == bagFactorPlan,
      "compiled rendering must retain the bag-factor plan"
    )
    require(
      stageDiagnostics.forall(_.renderedQuery == renderedQuery),
      "stage diagnostics must retain the compiled rendering"
    )
  }

  def compileOne(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: RankedCandidatePlan,
      bagFactorLimits: BagFactorPlanningLimits,
      renderingLimits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode,
      diagnosticLimits: Option[PostgreSqlStageDiagnosticLimits],
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile =
        PostgreSqlEnumerationRenderingProfile.default
  ): Either[Failure, Compiled] = {
    require(query != null, "source QueryIR must not be null")
    require(input != null, "candidate-search input must not be null")
    require(plan != null, "ranked candidate plan must not be null")
    require(bagFactorLimits != null, "bag-factor limits must not be null")
    require(renderingLimits != null, "rendering limits must not be null")
    require(reductionMode != null, "reduction mode must not be null")
    require(
      enumerationRenderingProfile != null,
      "enumeration rendering profile must not be null"
    )
    require(
      diagnosticLimits != null && diagnosticLimits.forall(_ != null),
      "diagnostic limits must not be null or contain null"
    )

    val bagFactorPlan = BagFactorPlanner.plan(
      input,
      plan.decomposition,
      bagFactorLimits
    ) match {
      case Left(problems) =>
        return Left(Failure.BagFactorPlanningFailed(plan, problems))
      case Right(value) => value
    }

    aggregateOwnerInapplicability(
      query,
      input,
      bagFactorPlan,
      reductionMode
    ).foreach { reason =>
      return Left(Failure.Inapplicable(plan, bagFactorPlan, reason))
    }

    val rendered = diagnosticLimits match {
      case Some(limits) =>
        PostgreSqlBagFactorRenderer
          .renderWithStageDiagnostics(
            query,
            input,
            bagFactorPlan,
            renderingLimits,
            bagFactorLimits,
            reductionMode,
            limits,
            enumerationRenderingProfile
          )
          .map(value => value.renderedQuery -> Some(value))
      case None =>
        PostgreSqlBagFactorRenderer
          .renderWithEnumerationProfile(
            query,
            input,
            bagFactorPlan,
            renderingLimits,
            bagFactorLimits,
            reductionMode,
            enumerationRenderingProfile
          )
          .map(value => value -> None)
    }

    rendered match {
      case Left(problems) =>
        Left(Failure.RenderingFailed(plan, bagFactorPlan, problems))
      case Right((renderedQuery, stageDiagnostics)) =>
        Right(
          Compiled(
            plan,
            bagFactorPlan,
            renderedQuery,
            stageDiagnostics
          )
        )
    }
  }

  /** Owner-local aggregate terminals require one certified owner occurrence. */
  private[postgresql] def aggregateOwnerInapplicability(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      reductionMode: YannakakisReductionMode
  ): Option[String] = {
    require(query != null, "source QueryIR must not be null")
    require(input != null, "candidate-search input must not be null")
    require(plan != null, "bag-factor plan must not be null")
    require(reductionMode != null, "reduction mode must not be null")

    query.resultShape match {
      case shape: QueryResultShape.C03G0AggregateVector =>
        val groupOccurrence = shape.groupColumn.relationOccurrenceId
        val measureOccurrence = shape.measureColumn.relationOccurrenceId
        val guardEdges = input.hypergraph.edges.filter(
          _.relationOccurrenceId == groupOccurrence
        )
        val owners = guardEdges.flatMap(edge =>
          plan.ownership.collect {
            case ownership if ownership.edgeId == edge.id => ownership.owner
          }
        )
        if (
          groupOccurrence != measureOccurrence || guardEdges.size != 1 ||
          owners.distinct.size != 1
        ) Some(PostgreSqlCandidateSelector.C03G0OwnerUnavailableReason)
        else None
      case shape: QueryResultShape.GlobalGuardedExtremum =>
        val guardEdges = input.hypergraph.edges.filter(
          _.relationOccurrenceId == shape.guardColumn.relationOccurrenceId
        )
        val owners = guardEdges.flatMap(edge =>
          plan.ownership.collect {
            case ownership if ownership.edgeId == edge.id => ownership.owner
          }
        )
        if (guardEdges.size != 1 || owners.distinct.size != 1) {
          Some(
            PostgreSqlCandidateSelector.GuardedExtremumOwnerUnavailableReason
          )
        } else None
      case _ => None
    }
  }
}
