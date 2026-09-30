package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.RewriteReasonCode
import org.apache.calcite.rel.core.AggregateCall
import org.apache.calcite.rel.logical.LogicalAggregate
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class CalciteGlobalCountStarFrontendSuite extends munit.FunSuite {
  private val schema = {
    val root = Frameworks.createRootSchema(true)
    root.add(
      "fact",
      new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType =
          factory
            .builder()
            .add("x", SqlTypeName.INTEGER)
            .add("payload", SqlTypeName.VARCHAR, 64)
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
      case _: CalciteSqlSourceEvidence.CertifiedGlobalCountStar => result
      case other => fail(s"expected certified global COUNT(*), found $other")
    }
  }

  private def rejectionReason(
      sql: String
  ): CalciteRejectedAggregateReason =
    planned(sql).sourceEvidence match {
      case CalciteSqlSourceEvidence.RejectedAggregate(reason) => reason
      case other => fail(s"expected rejected aggregate evidence, found $other")
    }

  private val joinedChild =
    "FROM fact AS left_fact JOIN fact AS right_fact ON left_fact.x = right_fact.x"

  test("framework planner certifies exact global COUNT(*) and preserves child hypergraph") {
    val counted = certified(s"SELECT COUNT(*) AS \"Total\" $joinedChild")
    val report = CalciteQueryAnalyzer.analyze(counted)

    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root match {
      case value: GlobalCountStarIR => value
      case other => fail(s"expected GlobalCountStarIR, found $other")
    }
    assertEquals(
      root.sourceCertificate.aliasMode,
      GlobalCountStarAliasMode.Explicit
    )
    assertEquals(root.countOutput.name, "Total")
    assertEquals(root.countOutput.ordinal, 0)
    assertEquals(root.countOutput.dataType, "BIGINT NOT NULL")
    assert(!root.countOutput.nullable)
    assertEquals(root.input.output.size, 4)
    assertEquals(report.query.output, root.output)
    assertEquals(
      report.query.resultShape,
      QueryResultShape.GlobalCountStar(
        root.countOutput,
        GlobalCountStarAliasMode.Explicit
      )
    )

    val rows = CalciteQueryAnalyzer.analyze(
      planned(s"SELECT left_fact.x $joinedChild")
    )
    assert(rows.isSupported, clues(rows.diagnostics))
    assertEquals(report.hypergraph, rows.hypergraph)
    assertEquals(report.query.relations, rows.query.relations)
    assertEquals(report.query.predicates, rows.query.predicates)
  }

  test("implicit COUNT(*) output is the exact PostgreSQL name count") {
    val result = certified(s"SELECT COUNT(*) $joinedChild")
    val report = CalciteQueryAnalyzer.analyze(result)

    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root.asInstanceOf[GlobalCountStarIR]
    assertEquals(
      root.sourceCertificate.aliasMode,
      GlobalCountStarAliasMode.Implicit
    )
    assertEquals(root.countOutput.name, "count")
    assertEquals(report.query.outputNames, Vector("count"))
    assertEquals(report.query.output.map(_.name), Vector("count"))
    assertEquals(
      report.query.resultShape.asInstanceOf[QueryResultShape.GlobalCountStar].aliasMode,
      GlobalCountStarAliasMode.Implicit
    )
  }

  test("legacy analyzer overloads and custom planners remain aggregate-uncertified") {
    val frameworkResult = certified(s"SELECT COUNT(*) $joinedChild")

    val legacy = CalciteQueryAnalyzer.analyze(frameworkResult.root)
    assert(!legacy.isSupported)
    assert(
      legacy.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate),
      clues(legacy.diagnostics)
    )
    assert(!legacy.query.root.isInstanceOf[GlobalCountStarIR])

    val custom = CalciteSqlPlanner
      .fromFunction(_ => Right(frameworkResult.root))
      .fold(problem => fail(problem.toString), identity)
    val customResult = CalciteSqlPlanner
      .planWithSourceEvidence(custom, "SELECT COUNT(*) FROM fact")
      .fold(problem => fail(problem.toString), identity)
    assertEquals(customResult.sourceEvidence, CalciteSqlSourceEvidence.Uncertified)

    val customAnalysis = CalciteQueryAnalyzer.analyze(customResult)
    assert(!customAnalysis.isSupported)
    assert(
      customAnalysis.diagnostics.exists(
        _.code == RewriteReasonCode.UnsupportedAggregate
      ),
      clues(customAnalysis.diagnostics)
    )
  }

  test("non-aggregate framework queries carry explicit non-aggregate evidence") {
    assertEquals(
      planned(s"SELECT left_fact.x $joinedChild").sourceEvidence,
      CalciteSqlSourceEvidence.NonAggregate
    )
  }

  test("COUNT expression and other aggregate functions are rejected at source") {
    assertEquals(
      rejectionReason("SELECT COUNT(x) FROM fact"),
      CalciteRejectedAggregateReason.CountExpression
    )
    assertEquals(
      rejectionReason("SELECT SUM(x) FROM fact"),
      CalciteRejectedAggregateReason.OtherAggregateFunction
    )
    assertEquals(
      rejectionReason("SELECT COUNT(*), SUM(x) FROM fact"),
      CalciteRejectedAggregateReason.MultipleAggregateCalls
    )
    Vector(
      "SELECT COUNT(fact.*) FROM fact",
      "SELECT pg_catalog.COUNT(*) FROM fact"
    ).foreach { sql =>
      CalciteSqlPlanner.planWithSourceEvidence(frameworkPlanner, sql) match {
        case Left(problem) =>
          assert(problem.stage.invalidatesSql, clues(problem))
          assert(
            problem.stage == CalciteSqlPlanningStage.Parsing ||
              problem.stage == CalciteSqlPlanningStage.Validation,
            clues(problem)
          )
        case Right(result) =>
          assert(
            !result.sourceEvidence.isInstanceOf[
              CalciteSqlSourceEvidence.CertifiedGlobalCountStar
            ],
            clues(result.sourceEvidence)
          )
      }
    }
    assertEquals(
      rejectionReason("SELECT COUNT(DISTINCT fact.x) FROM fact"),
      CalciteRejectedAggregateReason.CountExpression
    )
  }

  test("group, distinct, window, and indirect aggregate shapes are rejected") {
    assertEquals(
      rejectionReason("SELECT COUNT(*) FROM fact GROUP BY x"),
      CalciteRejectedAggregateReason.GroupedOrDistinct
    )
    assertEquals(
      rejectionReason("SELECT DISTINCT x FROM fact"),
      CalciteRejectedAggregateReason.GroupedOrDistinct
    )
    assertEquals(
      rejectionReason("SELECT COUNT(*) OVER () FROM fact"),
      CalciteRejectedAggregateReason.WindowCall
    )
    assertEquals(
      rejectionReason("SELECT COUNT(*) + 0 FROM fact"),
      CalciteRejectedAggregateReason.AggregateNotDirectTopLevelResult
    )
    assertEquals(
      rejectionReason("SELECT COUNT(*) FROM fact ORDER BY 1"),
      CalciteRejectedAggregateReason.UnsupportedTopLevelAggregateShape
    )
    assertEquals(
      rejectionReason("SELECT COUNT(*)"),
      CalciteRejectedAggregateReason.UnsupportedTopLevelAggregateShape
    )
  }

  test("nested, filtered, and quantified COUNT forms never receive a source certificate") {
    assertEquals(
      rejectionReason("SELECT COALESCE(COUNT(*), 0) FROM fact"),
      CalciteRejectedAggregateReason.MultipleAggregateCalls
    )

    Vector(
      "SELECT CAST(COUNT(*) AS BIGINT) FROM fact",
      "SELECT COUNT(*) FILTER (WHERE x > 0) FROM fact"
    ).foreach { sql =>
      assertEquals(
        rejectionReason(sql),
        CalciteRejectedAggregateReason.AggregateNotDirectTopLevelResult,
        sql
      )
    }

    assertEquals(
      rejectionReason("SELECT COUNT(ALL x) FROM fact"),
      CalciteRejectedAggregateReason.CountExpression
    )
    assertNeverCertified("SELECT COUNT(ALL *) FROM fact")
  }

  test("empty grouping, HAVING, OFFSET, FETCH, and WITH wrappers are rejected") {
    Vector(
      "SELECT COUNT(*) FROM fact GROUP BY ()",
      "SELECT COUNT(*) FROM fact HAVING TRUE"
    ).foreach { sql =>
      assertEquals(
        rejectionReason(sql),
        CalciteRejectedAggregateReason.GroupedOrDistinct,
        sql
      )
    }

    Vector(
      "SELECT COUNT(*) FROM fact OFFSET 1 ROWS",
      "SELECT COUNT(*) FROM fact FETCH FIRST 1 ROW ONLY"
    ).foreach { sql =>
      assertEquals(
        rejectionReason(sql),
        CalciteRejectedAggregateReason.UnsupportedTopLevelAggregateShape,
        sql
      )
    }

    assertNeverCertified(
      "WITH source_rows AS (SELECT x FROM fact) SELECT COUNT(*) FROM source_rows"
    )
  }

  test("top-level set operations are never certified") {
    Vector(
      "SELECT COUNT(*) FROM fact UNION ALL SELECT COUNT(*) FROM fact",
      "SELECT COUNT(*) FROM fact INTERSECT SELECT COUNT(*) FROM fact",
      "SELECT COUNT(*) FROM fact EXCEPT SELECT COUNT(*) FROM fact",
      "SELECT COUNT(*) FROM fact UNION ALL SELECT CAST(x AS BIGINT) FROM fact"
    ).foreach(assertNeverCertified)
  }

  test("a nested set-operation child is rejected at the raw aggregate boundary") {
    val sql =
      "SELECT COUNT(*) FROM (" +
        "SELECT x FROM fact UNION ALL SELECT x FROM fact" +
        ") AS nested"
    val nested = planned(sql)
    assertEquals(
      nested.sourceEvidence,
      CalciteSqlSourceEvidence.RejectedAggregate(
        CalciteRejectedAggregateReason.UnsupportedTopLevelAggregateShape
      )
    )
    val analysis = CalciteQueryAnalyzer.analyze(nested)

    assert(!analysis.isSupported)
    assert(
      analysis.diagnostics.exists(
        _.code == RewriteReasonCode.UnsupportedQueryShape
      ),
      clues(analysis.diagnostics)
    )
    assert(!analysis.query.root.isInstanceOf[GlobalCountStarIR])
  }

  test("complete AST walk rejects aggregates and windows hidden in FROM subqueries") {
    assertEquals(
      rejectionReason(
        "SELECT COUNT(*) FROM (SELECT SUM(x) AS total FROM fact) AS nested"
      ),
      CalciteRejectedAggregateReason.MultipleAggregateCalls
    )
    assertEquals(
      rejectionReason(
        "SELECT COUNT(*) FROM (SELECT ROW_NUMBER() OVER () AS row_id FROM fact) AS nested"
      ),
      CalciteRejectedAggregateReason.WindowCall
    )
    assertEquals(
      rejectionReason(
        "SELECT COUNT(*) FROM (SELECT DISTINCT x FROM fact) AS nested"
      ),
      CalciteRejectedAggregateReason.GroupedOrDistinct
    )
  }

  test("rejected evidence never enters the certified analyzer path") {
    val rejected = planned("SELECT COUNT(x) FROM fact")
    val analysis = CalciteQueryAnalyzer.analyze(rejected)

    assert(!analysis.isSupported)
    assert(
      analysis.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate),
      clues(analysis.diagnostics)
    )
    assert(!analysis.query.root.isInstanceOf[GlobalCountStarIR])
  }

  test("relational aggregate mismatch invalidates a genuine source certificate") {
    val genuine = certified("SELECT COUNT(*) FROM fact")
    val sum = planned("SELECT SUM(x) FROM fact")
    val forged = CalciteSqlPlannedQuery(sum.root, genuine.sourceEvidence)
    val analysis = CalciteQueryAnalyzer.analyze(forged)

    assert(!analysis.isSupported)
    assert(
      analysis.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate),
      clues(analysis.diagnostics)
    )
    assert(!analysis.query.root.isInstanceOf[GlobalCountStarIR])
  }

  test("a genuine certificate cannot bless distinct, filtered, or grouped relational aggregates") {
    val evidence = certified("SELECT COUNT(*) FROM fact").sourceEvidence
    Vector(
      "distinct" -> "SELECT COUNT(DISTINCT x) FROM fact",
      "filter" -> "SELECT COUNT(*) FILTER (WHERE x > 0) FROM fact",
      "group" -> "SELECT COUNT(*) FROM fact GROUP BY x"
    ).foreach { case (label, sql) =>
      assertUnsupportedAggregate(
        CalciteSqlPlannedQuery(planned(sql).root, evidence),
        label
      )
    }
  }

  test("exact relational proof rejects forged AggregateCall flags") {
    val genuine = certified("SELECT COUNT(*) FROM fact")
    val aggregate = exactAggregate(genuine)
    val call = aggregate.getAggCallList.get(0)
    Vector(
      "distinct" -> call.withDistinct(true),
      "approximate" -> call.withApproximate(true),
      "ignore-nulls" -> call.withIgnoreNulls(true)
    ).foreach { case (label, alteredCall) =>
      assertUnsupportedAggregate(
        replaceAggregateCall(genuine, aggregate, alteredCall),
        label
      )
    }

    val filtered = planned("SELECT COUNT(*) FILTER (WHERE TRUE) FROM fact")
    assertUnsupportedAggregate(
      CalciteSqlPlannedQuery(filtered.root, genuine.sourceEvidence),
      "filter"
    )
  }

  test("exact relational proof rejects non-BIGINT and nullable COUNT outputs") {
    val genuine = certified("SELECT COUNT(*) FROM fact")
    val aggregate = exactAggregate(genuine)
    val originalCall = aggregate.getAggCallList.get(0)
    val factory = aggregate.getCluster.getTypeFactory
    val alteredTypes = Vector(
      "integer" -> factory.createTypeWithNullability(
        factory.createSqlType(SqlTypeName.INTEGER),
        false
      ),
      "nullable-bigint" -> factory.createTypeWithNullability(
        factory.createSqlType(SqlTypeName.BIGINT),
        true
      )
    )

    alteredTypes.foreach { case (label, alteredType) =>
      val alteredCall = AggregateCall.create(
        originalCall.getAggregation,
        false,
        originalCall.getArgList,
        -1,
        aggregate.getInput,
        alteredType,
        originalCall.getName
      )
      assertUnsupportedAggregate(
        replaceAggregateCall(genuine, aggregate, alteredCall),
        label
      )
    }
  }

  test("GlobalCountStarIR enforces count result metadata and implicit alias") {
    val report = CalciteQueryAnalyzer.analyze(
      certified("SELECT COUNT(*) FROM fact")
    )
    val root = report.query.root.asInstanceOf[GlobalCountStarIR]

    intercept[IllegalArgumentException] {
      root.copy(countOutput = root.countOutput.copy(nullable = true))
    }
    intercept[IllegalArgumentException] {
      root.copy(countOutput = root.countOutput.copy(name = "wrong"))
    }
    intercept[IllegalArgumentException] {
      root.copy(
        countOutput = root.countOutput.copy(
          expression = ScalarExpression.Literal("1", "BIGINT NOT NULL", false)
        )
      )
    }
  }

  test("COUNT(*) scalar is forbidden outside the sole root wrapper output") {
    val report = CalciteQueryAnalyzer.analyze(
      certified("SELECT COUNT(*) FROM fact")
    )
    val countRoot = report.query.root.asInstanceOf[GlobalCountStarIR]
    val child = countRoot.input

    intercept[IllegalArgumentException] {
      countRoot.copy(input = countRoot)
    }

    val project = ProjectIR(
      child,
      Vector(
        ProjectItem(
          ordinal = 0,
          name = "bad_count",
          expression = ScalarExpression.GlobalCountStar,
          dataType = "BIGINT NOT NULL",
          nullable = false
        )
      )
    )
    intercept[IllegalArgumentException] {
      queryFor(project, Vector("bad_count"))
    }

    val predicate = PredicateDescriptor(
      ordinal = 0,
      origin = PredicateOrigin.Filter("root"),
      expression = ScalarExpression.GlobalCountStar,
      classification = PredicateClassification.Unsupported(
        RewriteReasonCode.UnsupportedPredicate,
        "constructor adversary"
      )
    )
    val filter = FilterIR(child, Vector(predicate))
    intercept[IllegalArgumentException] {
      queryFor(filter, child.output.map(_.name))
    }

    val unsupported = UnsupportedRelationalIR(
      operatorName = "AdversarialAggregate",
      nodePath = "root",
      diagnosticCode = RewriteReasonCode.UnsupportedAggregate,
      inputs = Vector(child),
      output = Vector(
        OutputSlot(
          ordinal = 0,
          name = "bad_count",
          expression = ScalarExpression.GlobalCountStar,
          dataType = "BIGINT NOT NULL",
          nullable = false
        )
      )
    )
    intercept[IllegalArgumentException] {
      queryFor(unsupported, Vector("bad_count"))
    }
  }

  private def queryFor(
      root: RelationalIR,
      outputNames: Vector[String]
  ): QueryIR =
    QueryIR(
      root = root,
      relations = RelationalIR.relationInventory(root),
      predicates = RelationalIR.predicateInventory(root),
      outputNames = outputNames
    )

  private def assertNeverCertified(sql: String): Unit =
    CalciteSqlPlanner.planWithSourceEvidence(frameworkPlanner, sql) match {
      case Left(_) => ()
      case Right(result) =>
        assert(
          !result.sourceEvidence.isInstanceOf[
            CalciteSqlSourceEvidence.CertifiedGlobalCountStar
          ],
          clues(sql, result.sourceEvidence)
        )
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
    val analysis = CalciteQueryAnalyzer.analyze(forged)
    assert(!analysis.isSupported, label)
    assert(
      analysis.diagnostics.exists(
        _.code == RewriteReasonCode.UnsupportedAggregate
      ),
      clues(label, analysis.diagnostics)
    )
    assert(!analysis.query.root.isInstanceOf[GlobalCountStarIR], label)
  }
}
