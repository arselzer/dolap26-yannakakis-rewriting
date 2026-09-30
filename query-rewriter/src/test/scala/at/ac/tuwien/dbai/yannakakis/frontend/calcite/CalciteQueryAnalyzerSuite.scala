package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.RewriteReasonCode
import at.ac.tuwien.dbai.yannakakis.calcite.CalciteTestSchemas
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import org.apache.calcite.adapter.enumerable.EnumerableTableScan
import org.apache.calcite.adapter.jdbc.JdbcTableScan
import org.apache.calcite.rel.core.TableScan
import org.apache.calcite.rel.logical.{LogicalFilter, LogicalTableScan}

class CalciteQueryAnalyzerSuite extends munit.FunSuite {
  private def analyze(sql: String): AnalysisReport =
    CalciteQueryAnalyzer.analyze(CalciteTestSchemas.standard.plan(sql).root)

  private def hypergraph(report: AnalysisReport): QueryHypergraph =
    report.hypergraph match {
      case Right(value) => value
      case Left(errors) =>
        fail(errors.map(error => s"${error.code.id}: ${error.message}").mkString("\n"))
    }

  test("only exact bare logical and JDBC table-scan classes are accepted") {
    assert(
      CalciteQueryAnalyzer.isSupportedBaseScanClass(classOf[LogicalTableScan])
    )
    assert(
      CalciteQueryAnalyzer.isSupportedBaseScanClass(classOf[JdbcTableScan])
    )
    assert(
      !CalciteQueryAnalyzer.isSupportedBaseScanClass(classOf[TableScan])
    )
    assert(
      !CalciteQueryAnalyzer.isSupportedBaseScanClass(
        classOf[EnumerableTableScan]
      )
    )
  }

  test("chain uses stable scan occurrences and deterministic hypergraph variables") {
    val sql =
      """
        |SELECT a.ID, c.PAYLOAD
        |FROM CHAIN_A a
        |JOIN CHAIN_B b ON a.B_ID = b.ID
        |JOIN CHAIN_C c ON b.C_ID = c.ID
        |""".stripMargin

    val first = analyze(sql)
    val second = analyze(sql)

    assert(first.isSupported, clues(first.diagnostics))
    assertEquals(first, second)
    assertEquals(first.query.relations.map(_.id.value), Vector(0, 1, 2))
    assertEquals(
      first.query.relations.map(_.qualifiedName.last),
      Vector("CHAIN_A", "CHAIN_B", "CHAIN_C")
    )
    assertEquals(first.query.predicates.size, 2)
    assert(
      first.query.predicates.forall(
        _.classification.isInstanceOf[PredicateClassification.InnerEquality]
      )
    )

    val graph = hypergraph(first)
    assertEquals(
      graph.edges.map(edge =>
        edge.relationOccurrenceId.value -> edge.vertices.map(_.value)
      ),
      Vector(0 -> Vector(0), 1 -> Vector(0, 1), 2 -> Vector(1))
    )
    assertEquals(graph, hypergraph(second))
  }

  test("triangle inventories every conjunct and produces a cyclic hypergraph") {
    val report = analyze(
      """
        |SELECT r.A, r.B, s.C
        |FROM TRIANGLE_R r
        |JOIN TRIANGLE_S s ON r.B = s.B
        |JOIN TRIANGLE_T t ON s.C = t.C AND t.A = r.A
        |""".stripMargin
    )

    assert(report.isSupported, clues(report.diagnostics))
    assertEquals(report.query.predicates.size, 3)
    assertEquals(
      report.query.predicates.map(_.classification.id),
      Vector("inner-equality", "inner-equality", "inner-equality")
    )

    val graph = hypergraph(report)
    assertEquals(graph.variables.map(_.value), Vector(0, 1, 2))
    assertEquals(
      graph.edges.map(edge =>
        edge.relationOccurrenceId.value -> edge.vertices.map(_.value)
      ),
      Vector(
        0 -> Vector(0, 1),
        1 -> Vector(1, 2),
        2 -> Vector(0, 2)
      )
    )
  }

  test("self-join scans retain distinct occurrence and column identities") {
    val report = analyze(
      """
        |SELECT employee.ID, manager.ID
        |FROM EMPLOYEE employee
        |JOIN EMPLOYEE manager ON employee.MANAGER_ID = manager.ID
        |""".stripMargin
    )

    assert(report.isSupported, clues(report.diagnostics))
    val relations = report.query.relations
    assertEquals(relations.map(_.qualifiedName.last), Vector("EMPLOYEE", "EMPLOYEE"))
    assertEquals(relations.map(_.id.value), Vector(0, 1))
    assertNotEquals(relations(0).columns(0).ref, relations(1).columns(0).ref)

    val graph = hypergraph(report)
    assertEquals(graph.edges.size, 2)
    assertNotEquals(graph.edges(0).id, graph.edges(1).id)
    assert(graph.edges(0).sameVerticesAs(graph.edges(1)))
    assertEquals(
      graph.edges.map(_.relationOccurrenceId.value),
      Vector(0, 1)
    )
    assertEquals(
      graph.edges.map(_.bindings.map { case (columnId, variableId) =>
        columnId.ordinal -> variableId.value
      }),
      Vector(Vector(1 -> 0), Vector(0 -> 0))
    )
  }

  test("non-renderable local filters are inventoried and fail closed") {
    val report = analyze(
      """
        |SELECT a.ID, b.ID
        |FROM CHAIN_A a
        |JOIN CHAIN_B b ON a.B_ID = b.ID
        |WHERE a.ID > 10 AND a.ID < 100
        |""".stripMargin
    )

    assert(!report.isSupported)
    assertEquals(report.query.predicates.size, 3)
    val unsupportedFilters = report.query.predicates.filter { predicate =>
      !predicate.classification.supported
    }
    assertEquals(unsupportedFilters.size, 2)
    assert(unsupportedFilters.forall(_.expression.resolved))
    assert(unsupportedFilters.forall(_.expression.deterministic))
    assert(
      unsupportedFilters.forall(predicate =>
        !predicate.expression.losslesslyRetained
      )
    )
    assertEquals(
      unsupportedFilters
        .flatMap(_.expression.columnRefs)
        .map(_.relationOccurrenceId.value)
        .distinct,
      Vector(0)
    )
    assert(
      report.diagnostics.forall(_.code == RewriteReasonCode.UnsupportedPredicate),
      clues(report.diagnostics)
    )
    assert(report.hypergraph.isLeft)
  }

  test("cross-relation non-equality is inventoried and rejected") {
    val report = analyze(
      """
        |SELECT a.ID, b.ID
        |FROM CHAIN_A a
        |JOIN CHAIN_B b ON a.B_ID > b.ID
        |""".stripMargin
    )

    assert(!report.isSupported)
    assertEquals(report.query.predicates.size, 1)
    assertEquals(report.query.predicates.head.classification.id, "unsupported")
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedPredicate),
      clues(report.diagnostics)
    )
    assert(report.hypergraph.isLeft)
  }

  test("cartesian inner join fails the equality-connectivity gate") {
    val report = analyze(
      """
        |SELECT a.ID, b.ID
        |FROM CHAIN_A a
        |CROSS JOIN CHAIN_B b
        |""".stripMargin
    )

    assert(!report.isSupported)
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedQueryShape),
      clues(report.diagnostics)
    )
    assert(report.hypergraph.isLeft)
    val directErrors = QueryHypergraphConverter
      .convert(report.query)
      .swap
      .toOption
      .getOrElse(Vector.empty)
    assert(
      directErrors.exists(_.code == RewriteReasonCode.UnsupportedQueryShape),
      clues(directErrors)
    )
  }

  test("outer join is rejected without losing its equality predicate") {
    val report = analyze(
      """
        |SELECT a.ID, b.ID
        |FROM CHAIN_A a
        |LEFT JOIN CHAIN_B b ON a.B_ID = b.ID
        |""".stripMargin
    )

    assert(!report.isSupported)
    assertEquals(report.query.predicates.size, 1)
    assertEquals(
      report.query.predicates.head.classification.id,
      "inner-equality"
    )
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedJoinType),
      clues(report.diagnostics)
    )
    assert(report.hypergraph.isLeft)
  }

  test("duplicate output aliases remain positional") {
    val report = analyze(
      """
        |SELECT a.ID AS DUPLICATE_VALUE, b.ID AS DUPLICATE_VALUE
        |FROM CHAIN_A a
        |JOIN CHAIN_B b ON a.B_ID = b.ID
        |""".stripMargin
    )

    assert(report.isSupported, clues(report.diagnostics))
    assertEquals(
      report.query.output.map(_.name),
      Vector("DUPLICATE_VALUE", "DUPLICATE_VALUE")
    )
    val outputRefs = report.query.output.map(_.expression).collect {
      case ScalarExpression.Column(ref, _, _, _) => ref
    }
    assertEquals(outputRefs.size, 2)
    assertNotEquals(outputRefs(0), outputRefs(1))
  }

  test("reordered SQL outputs retain their source column ordinals") {
    val report = analyze(
      "SELECT a.B_ID, a.ID FROM CHAIN_A a"
    )

    assert(report.isSupported, clues(report.diagnostics))
    val outputRefs = report.query.output.map(_.expression).collect {
      case ScalarExpression.Column(ref, _, _, _) => ref
    }
    assertEquals(outputRefs.map(_.ordinal), Vector(1, 0))
    assertEquals(report.query.output.map(_.name), Vector("B_ID", "ID"))
  }

  test("hidden ORDER BY fields yield an unsupported report instead of throwing") {
    val report = analyze(
      "SELECT a.ID FROM CHAIN_A a ORDER BY a.B_ID"
    )

    assert(!report.isSupported)
    assertEquals(report.query.output.size, 1)
    assertEquals(report.query.output.map(_.name), Vector("ID"))
    assertEquals(report.query.relations.map(_.qualifiedName.last), Vector("CHAIN_A"))
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedQueryShape),
      clues(report.diagnostics)
    )
  }

  test("an always-true logical filter has an empty predicate inventory") {
    val planned = CalciteTestSchemas.standard.plan("SELECT * FROM CHAIN_A")
    val scan = planned.nodes.collectFirst { case value: LogicalTableScan => value }.get
    val condition = scan.getCluster.getRexBuilder.makeLiteral(true)
    val report = CalciteQueryAnalyzer.analyze(LogicalFilter.create(scan, condition))

    assert(report.isSupported, clues(report.diagnostics))
    assertEquals(report.query.predicates, Vector.empty)
    report.query.root match {
      case filter: FilterIR => assertEquals(filter.predicates, Vector.empty)
      case other            => fail(s"expected FilterIR, found $other")
    }
  }

  test("transitive and redundant equalities form one deterministic variable class") {
    val report = analyze(
      """
        |SELECT r.A, t.A
        |FROM TRIANGLE_R r
        |JOIN TRIANGLE_S s ON r.A = s.B AND r.B = s.B
        |JOIN TRIANGLE_T t ON s.B = t.A AND r.A = t.A
        |""".stripMargin
    )

    assert(report.isSupported, clues(report.diagnostics))
    val graph = hypergraph(report)
    assertEquals(graph.variables.map(_.value), Vector(0))
    assertEquals(
      graph.edges.map(_.bindings.map { case (columnId, variableId) =>
        columnId.ordinal -> variableId.value
      }),
      Vector(
        Vector(0 -> 0, 1 -> 0),
        Vector(0 -> 0),
        Vector(1 -> 0)
      )
    )
  }

  test("distinct-column local equality remains a predicate and not a structural edge") {
    val report = analyze(
      """
        |SELECT r.A, r.B
        |FROM TRIANGLE_R r
        |JOIN TRIANGLE_S s ON r.B = s.B
        |WHERE r.A = r.B
        |""".stripMargin
    )

    assert(report.isSupported, clues(report.diagnostics))
    val localEqualities = report.query.predicates.collect {
      case predicate @ PredicateDescriptor(
            _,
            _,
            ScalarExpression.Call(
              "EQUALS",
              _,
              Vector(
                ScalarExpression.Column(left, _, _, _),
                ScalarExpression.Column(right, _, _, _)
              ),
              _,
              _,
              _,
              _
            ),
            PredicateClassification.LocalFilter(occurrenceId)
          ) => predicate -> (occurrenceId, left, right)
    }
    assertEquals(localEqualities.size, 1)
    val (_, (occurrenceId, left, right)) = localEqualities.head
    assertEquals(occurrenceId.value, 0)
    assertEquals(left.relationOccurrenceId, occurrenceId)
    assertEquals(right.relationOccurrenceId, occurrenceId)
    assertEquals(Vector(left.ordinal, right.ordinal), Vector(0, 1))

    val graph = hypergraph(report)
    assertEquals(graph.variables.map(_.value), Vector(0))
    assertEquals(
      graph.edges.map(_.bindings.map { case (columnId, variableId) =>
        columnId.ordinal -> variableId.value
      }),
      Vector(Vector(1 -> 0), Vector(0 -> 0))
    )
  }

  test("equality encounter order does not change deterministic variable classes") {
    val first = analyze(
      """
        |SELECT r.A, r.B
        |FROM TRIANGLE_R r
        |JOIN TRIANGLE_S s ON r.A = s.B AND r.B = s.C
        |""".stripMargin
    )
    val reordered = analyze(
      """
        |SELECT r.A, r.B
        |FROM TRIANGLE_R r
        |JOIN TRIANGLE_S s ON r.B = s.C AND r.A = s.B
        |""".stripMargin
    )

    assert(first.isSupported, clues(first.diagnostics))
    assert(reordered.isSupported, clues(reordered.diagnostics))
    assertEquals(hypergraph(first), hypergraph(reordered))
    assertEquals(
      hypergraph(first).edges.map(_.bindings.map {
        case (columnId, variableId) => columnId.ordinal -> variableId.value
      }),
      Vector(Vector(0 -> 0, 1 -> 1), Vector(0 -> 0, 1 -> 1))
    )
  }

  test("QueryIR rejects a global predicate inventory that diverges from its tree") {
    val report = analyze(
      """
        |SELECT a.ID, b.ID
        |FROM CHAIN_A a
        |JOIN CHAIN_B b ON a.B_ID = b.ID
        |""".stripMargin
    )

    intercept[IllegalArgumentException] {
      report.query.copy(predicates = Vector.empty)
    }
  }

  test("QueryIR rejects a global relation inventory that diverges from its tree") {
    val report = analyze(
      """
        |SELECT a.ID, b.ID
        |FROM CHAIN_A a
        |JOIN CHAIN_B b ON a.B_ID = b.ID
        |""".stripMargin
    )

    intercept[IllegalArgumentException] {
      report.query.copy(relations = report.query.relations.reverse)
    }
  }

  test("unsupported aggregate retains analysis of its input scans") {
    val report = analyze(
      """
        |SELECT DEPARTMENT_ID, MAX(AMOUNT)
        |FROM FACT_SALE
        |GROUP BY DEPARTMENT_ID
        |""".stripMargin
    )

    assert(!report.isSupported)
    assertEquals(report.query.relations.map(_.qualifiedName.last), Vector("FACT_SALE"))
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate),
      clues(report.diagnostics)
    )
    assert(report.hypergraph.isLeft)
    val directErrors = QueryHypergraphConverter
      .convert(report.query)
      .swap
      .toOption
      .getOrElse(Vector.empty)
    val aggregateError = directErrors
      .find(_.code == RewriteReasonCode.UnsupportedAggregate)
      .getOrElse(fail(s"missing aggregate diagnostic: $directErrors"))
    assert(
      aggregateError.context.get("nodePath").exists(_.startsWith("root")),
      clues(aggregateError)
    )
  }
}
