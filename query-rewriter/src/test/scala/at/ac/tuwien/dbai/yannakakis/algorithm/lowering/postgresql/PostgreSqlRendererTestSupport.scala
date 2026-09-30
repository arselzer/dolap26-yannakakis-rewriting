package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidateSearchInput,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  BagFactorPlanner
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  TestTable
}
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  CalciteQueryAnalyzer,
  QueryIR
}

private[postgresql] object PostgreSqlRendererTestSupport {
  final case class PreparedQuery(
      report: AnalysisReport,
      query: QueryIR,
      hypergraph: QueryHypergraph,
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      plan: BagFactorPlan
  )

  def prepareTopOne(
      sql: String,
      tables: Vector[TestTable],
      widthBound: Int,
      requireConnectedCover: Boolean
  ): PreparedQuery = {
    val fixture = CalcitePlannerFixture(tables)
    val report = CalciteQueryAnalyzer.analyze(fixture.plan(sql).root)
    if (!report.isSupported) {
      throw new AssertionError(
        report.diagnostics
          .map(error => s"${error.code.id}: ${error.message}")
          .mkString("; ")
      )
    }
    val hypergraph = report.hypergraph.fold(
      errors =>
        throw new AssertionError(
          errors.map(error => s"${error.code.id}: ${error.message}").mkString("; ")
        ),
      identity
    )
    prepareQuery(report, hypergraph, widthBound, requireConnectedCover)
  }

  def prepareQuery(
      report: AnalysisReport,
      hypergraph: QueryHypergraph,
      widthBound: Int,
      requireConnectedCover: Boolean
  ): PreparedQuery = {
    val generated = SoftBagGenerator
      .generate(hypergraph, widthBound)
      .fold(error => throw new AssertionError(error.message), identity)
    val input = CandidateSearchInput
      .create(
        hypergraph,
        widthBound,
        generated.admissible(requireConnectedCover)
      )
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )
    val decomposition = TopKCandidateTD
      .enumerate(input, planLimit = 1)
      .fold(
        error => throw new AssertionError(error.message),
        {
          case found: CandidatePlanDecision.Found =>
            found.plans.headOption
              .map(_.decomposition)
              .getOrElse(throw new AssertionError("top-1 decision contained no plan"))
          case rejected: CandidatePlanDecision.Rejected =>
            throw new AssertionError(
              s"candidate search rejected fixture: ${rejected.statistics}"
            )
        }
      )
    withDecomposition(report, hypergraph, input, decomposition)
  }

  def withDecomposition(
      report: AnalysisReport,
      hypergraph: QueryHypergraph,
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition
  ): PreparedQuery = {
    val plan = BagFactorPlanner
      .plan(input, decomposition)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )
    PreparedQuery(report, report.query, hypergraph, input, decomposition, plan)
  }

  def render(
      prepared: PreparedQuery
  ): PostgreSqlRenderedQuery =
    PostgreSqlBagFactorRenderer
      .render(prepared.query, prepared.input, prepared.plan)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )
}
