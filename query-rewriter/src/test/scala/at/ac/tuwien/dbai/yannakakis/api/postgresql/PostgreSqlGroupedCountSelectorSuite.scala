package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteQueryAnalyzer,
  CalciteSqlPlanner,
  QueryIR
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlGroupedCountSelectorSuite extends munit.FunSuite {
  private val query: QueryIR = {
    val schema = Frameworks.createRootSchema(true)
    Vector("selector_r", "selector_s").foreach { name =>
      schema.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory.builder().add("k", SqlTypeName.INTEGER).build()
        }
      )
    }
    val planner = CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
    val planned = CalciteSqlPlanner
      .planWithSourceEvidence(
        planner,
        "SELECT r.k, COUNT(*) FROM selector_r AS r " +
          "JOIN selector_s AS s ON r.k = s.k GROUP BY r.k"
      )
      .fold(problem => fail(problem.toString), identity)
    val report = CalciteQueryAnalyzer.analyze(planned)
    assert(report.isSupported, clues(report.diagnostics))
    report.query
  }

  test("full grouped COUNT admits structural, static, and factor-aware selection") {
    Vector(
      CandidatePlanSelection.StructuralHead,
      CandidatePlanSelection.StructuralRank(1),
      CandidatePlanSelection.PostgreSqlStaticShapeV1,
      CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
    ).foreach { policy =>
      assertEquals(
        PostgreSqlCandidateSelector.aggregateSelectionInapplicability(
          query,
          policy,
          YannakakisReductionMode.FullTwoPass
        ),
        None,
        clues(policy)
      )
    }
  }

  test("non-full grouped COUNT is explicitly inapplicable for every policy") {
    for {
      mode <- Vector(
        YannakakisReductionMode.BottomUpOnly,
        YannakakisReductionMode.NoReduction
      )
      policy <- Vector(
        CandidatePlanSelection.StructuralHead,
        CandidatePlanSelection.PostgreSqlStaticShapeV1,
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
      )
    } yield assertEquals(
      PostgreSqlCandidateSelector.aggregateSelectionInapplicability(
        query,
        policy,
        mode
      ),
      Some(PostgreSqlCandidateSelector.GroupedCountRequiresFullTwoPassReason),
      clues(mode, policy)
    )
  }

  test("unproven optimizer-backed grouped policies remain explicitly closed") {
    Vector(
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1,
      CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
    ).foreach { policy =>
      assertEquals(
        PostgreSqlCandidateSelector.aggregateSelectionInapplicability(
          query,
          policy,
          YannakakisReductionMode.FullTwoPass
        ),
        Some(
          PostgreSqlCandidateSelector.GroupedCountOptimizerPolicyUnsupportedReason
        ),
        clues(policy)
      )
    }
  }
}
