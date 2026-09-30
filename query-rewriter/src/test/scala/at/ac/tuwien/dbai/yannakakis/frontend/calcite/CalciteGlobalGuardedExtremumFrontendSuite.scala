package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.RewriteReasonCode
import org.apache.calcite.rel.core.AggregateCall
import org.apache.calcite.rel.logical.LogicalAggregate
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class CalciteGlobalGuardedExtremumFrontendSuite extends munit.FunSuite {
  private val schema = {
    val root = Frameworks.createRootSchema(true)
    root.add(
      "fact",
      new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType =
          factory
            .builder()
            .add("x", SqlTypeName.INTEGER)
            .nullable(false)
            .add("payload", SqlTypeName.VARCHAR, 64)
            .nullable(true)
            .build()
      }
    )
    root
  }

  private val frameworkPlanner: CalciteSqlPlanner =
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)

  private def planned(sql: String): CalciteSqlPlannedQuery =
    CalciteSqlPlanner
      .planWithSourceEvidence(frameworkPlanner, sql)
      .fold(problem => fail(problem.toString), identity)

  private def certified(sql: String): CalciteSqlPlannedQuery = {
    val result = planned(sql)
    result.sourceEvidence match {
      case _: CalciteSqlSourceEvidence.CertifiedGlobalGuardedExtremum =>
        result
      case other => fail(s"expected certified global MIN/MAX, found $other")
    }
  }

  private val joinedChild =
    "FROM fact AS left_fact JOIN fact AS right_fact " +
      "ON left_fact.x = right_fact.x"

  test("implicit MIN retains a nullable result and the left guard occurrence") {
    val guarded = certified(s"SELECT MIN(left_fact.x) $joinedChild")
    val report = CalciteQueryAnalyzer.analyze(guarded)

    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root match {
      case value: GlobalGuardedExtremumIR => value
      case other => fail(s"expected GlobalGuardedExtremumIR, found $other")
    }
    assertEquals(root.sourceCertificate.function, GlobalGuardedExtremumFunction.Min)
    assertEquals(
      root.sourceCertificate.aliasMode,
      GlobalGuardedExtremumAliasMode.Implicit
    )
    assertEquals(root.extremumOutput.name, "min")
    assert(root.extremumOutput.nullable)
    assert(root.extremum.nullable)
    assert(!root.extremumOutput.dataType.endsWith(" NOT NULL"))
    assertEquals(root.guardColumn, report.query.relations.head.columns.head.ref)
    assertEquals(root.guardOccurrenceId, report.query.relations.head.id)
    assertEquals(
      report.query.resultShape,
      QueryResultShape.GlobalGuardedExtremum(
        GlobalGuardedExtremumFunction.Min,
        root.guardColumn,
        root.extremumOutput,
        GlobalGuardedExtremumAliasMode.Implicit
      )
    )

    val rows = CalciteQueryAnalyzer.analyze(
      planned(s"SELECT left_fact.x $joinedChild")
    )
    assert(rows.isSupported, clues(rows.diagnostics))
    assertEquals(report.hypergraph, rows.hypergraph)
    assertEquals(report.query.relations, rows.query.relations)
    assertEquals(
      report.query.predicates,
      RelationalIR.predicateInventory(root.input)
    )
  }

  test("qualified MAX retains an explicit alias and the right guard occurrence") {
    val report = CalciteQueryAnalyzer.analyze(
      certified(s"SELECT MAX(right_fact.x) AS \"Top\" $joinedChild")
    )

    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root.asInstanceOf[GlobalGuardedExtremumIR]
    assertEquals(root.sourceCertificate.function, GlobalGuardedExtremumFunction.Max)
    assertEquals(
      root.sourceCertificate.aliasMode,
      GlobalGuardedExtremumAliasMode.Explicit
    )
    assertEquals(root.extremumOutput.name, "Top")
    assertEquals(report.query.outputNames, Vector("Top"))
    assertEquals(root.guardColumn, report.query.relations(1).columns.head.ref)
    assertEquals(root.guardOccurrenceId, report.query.relations(1).id)
  }

  test("an unqualified direct identifier is admitted") {
    val report = CalciteQueryAnalyzer.analyze(certified("SELECT MIN(x) FROM fact"))
    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root.asInstanceOf[GlobalGuardedExtremumIR]
    assertEquals(root.guardColumn, report.query.relations.head.columns.head.ref)
  }

  test("a projected derived-table guard fails the single-query-block boundary") {
    val projected = planned(
      "SELECT MAX(guard_x) FROM " +
        "(SELECT x AS guard_x FROM fact) AS projected_fact"
    )
    assertEquals(
      projected.sourceEvidence,
      CalciteSqlSourceEvidence.RejectedAggregate(
        CalciteRejectedAggregateReason.UnsupportedTopLevelAggregateShape
      )
    )
    assert(!CalciteQueryAnalyzer.analyze(projected).isSupported)
  }

  test("expressions, wrappers, quantifiers, filters, and multiple results stay uncertified") {
    Vector(
      "SELECT MIN(x + 1) FROM fact",
      "SELECT MIN(CAST(x AS INTEGER)) FROM fact",
      "SELECT MIN(1) FROM fact",
      "SELECT MIN(DISTINCT x) FROM fact",
      "SELECT MIN(ALL x) FROM fact",
      "SELECT MIN(x) FILTER (WHERE x > 0) FROM fact",
      "SELECT MIN(x) + 0 FROM fact",
      "SELECT COALESCE(MAX(x), 0) FROM fact",
      "SELECT MIN(x), MAX(x) FROM fact"
    ).foreach(assertNeverCertified)
  }

  test("grouped, windowed, ordered, and no-FROM forms stay uncertified") {
    Vector(
      "SELECT MIN(x) FROM fact GROUP BY payload",
      "SELECT MIN(x) OVER () FROM fact",
      "SELECT MIN(x) FROM fact ORDER BY 1",
      "SELECT MIN(1)"
    ).foreach(assertNeverCertified)
  }

  test("legacy overloads cannot infer a source certificate") {
    val result = certified("SELECT MIN(x) FROM fact")
    val legacy = CalciteQueryAnalyzer.analyze(result.root)

    assert(!legacy.isSupported)
    assert(
      legacy.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate),
      clues(legacy.diagnostics)
    )
    assert(!legacy.query.root.isInstanceOf[GlobalGuardedExtremumIR])
  }

  test("a genuine certificate cannot bless another function or aggregate flags") {
    val genuine = certified("SELECT MIN(x) FROM fact")
    val aggregate = exactAggregate(genuine)
    val approximateCall = aggregate.getAggCallList.get(0).withApproximate(true)
    val approximate = replaceAggregateCall(genuine, aggregate, approximateCall)
    assertUnsupportedAggregate(approximate, "approximate-flag")

    Vector(
      "different-function" -> "SELECT MAX(x) FROM fact",
      "filter" -> "SELECT MIN(x) FILTER (WHERE x > 0) FROM fact",
      "group" -> "SELECT MIN(x) FROM fact GROUP BY payload"
    ).foreach { case (label, sql) =>
      assertUnsupportedAggregate(
        CalciteSqlPlannedQuery(planned(sql).root, genuine.sourceEvidence),
        label
      )
    }
  }

  test("GlobalGuardedExtremumIR enforces result, function, and guard invariants") {
    val report = CalciteQueryAnalyzer.analyze(
      certified("SELECT MIN(x) FROM fact")
    )
    val root = report.query.root.asInstanceOf[GlobalGuardedExtremumIR]

    intercept[IllegalArgumentException] {
      root.copy(extremumOutput = root.extremumOutput.copy(nullable = false))
    }
    intercept[IllegalArgumentException] {
      root.copy(extremumOutput = root.extremumOutput.copy(name = "wrong"))
    }
    intercept[IllegalArgumentException] {
      root.copy(
        extremumOutput = root.extremumOutput.copy(
          expression = root.extremum.copy(
            function = GlobalGuardedExtremumFunction.Max
          )
        )
      )
    }
  }

  private def assertNeverCertified(sql: String): Unit =
    CalciteSqlPlanner.planWithSourceEvidence(frameworkPlanner, sql) match {
      case Left(_) => ()
      case Right(result) =>
        assert(
          !result.sourceEvidence.isInstanceOf[
            CalciteSqlSourceEvidence.CertifiedGlobalGuardedExtremum
          ],
          clues(sql, result.sourceEvidence)
        )
        val report = CalciteQueryAnalyzer.analyze(result)
        assert(!report.query.root.isInstanceOf[GlobalGuardedExtremumIR], sql)
    }

  private def exactAggregate(
      planned: CalciteSqlPlannedQuery
  ): LogicalAggregate =
    planned.root.project() match {
      case aggregate: LogicalAggregate => aggregate
      case other => fail(s"expected LogicalAggregate, found $other")
    }

  private def replaceAggregateCall(
      genuine: CalciteSqlPlannedQuery,
      aggregate: LogicalAggregate,
      alteredCall: AggregateCall
  ): CalciteSqlPlannedQuery = {
    val alteredAggregate = LogicalAggregate.create(
      aggregate.getInput,
      aggregate.getGroupSet,
      aggregate.getGroupSets,
      java.util.Collections.singletonList(alteredCall)
    )
    CalciteSqlPlannedQuery(
      genuine.root.withRel(alteredAggregate),
      genuine.sourceEvidence
    )
  }

  private def assertUnsupportedAggregate(
      forged: CalciteSqlPlannedQuery,
      label: String
  ): Unit = {
    val report = CalciteQueryAnalyzer.analyze(forged)
    assert(!report.isSupported, label)
    assert(
      report.diagnostics.exists(
        _.code == RewriteReasonCode.UnsupportedAggregate
      ),
      clues(label, report.diagnostics)
    )
    assert(!report.query.root.isInstanceOf[GlobalGuardedExtremumIR], label)
  }
}
