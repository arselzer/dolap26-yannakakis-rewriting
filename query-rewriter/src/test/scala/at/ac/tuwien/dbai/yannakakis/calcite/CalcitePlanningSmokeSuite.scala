package at.ac.tuwien.dbai.yannakakis.calcite

import org.apache.calcite.rel.core.TableScan
import org.apache.calcite.tools.ValidationException

class CalcitePlanningSmokeSuite extends munit.FunSuite {
  private def assertFrontendCompleted(planned: PlannedQuery): Unit = {
    assert(planned.parsed ne null)
    assert(planned.validated ne null)
    assert(planned.rel ne null)
    assert(planned.renderedPlan.nonEmpty)
  }

  private def assertOperatorCounts(
      planned: PlannedQuery,
      expected: Map[String, Int]
  ): Unit = {
    expected.foreach { case (operatorName, count) =>
      assertEquals(
        planned.operatorCount(operatorName),
        count,
        clues(planned.renderedPlan)
      )
    }
  }

  test("a three-relation chain becomes two binary joins over three scans") {
    val planned = CalciteTestSchemas.standard.plan(
      """
        |SELECT a.ID, c.PAYLOAD
        |FROM CHAIN_A a
        |JOIN CHAIN_B b ON a.B_ID = b.ID
        |JOIN CHAIN_C c ON b.C_ID = c.ID
        |""".stripMargin
    )

    assertFrontendCompleted(planned)
    assertOperatorCounts(
      planned,
      Map("LogicalProject" -> 1, "LogicalJoin" -> 2, "LogicalTableScan" -> 3)
    )
    assertEquals(
      planned.operatorNames,
      Vector(
        "LogicalProject",
        "LogicalJoin",
        "LogicalJoin",
        "LogicalTableScan",
        "LogicalTableScan",
        "LogicalTableScan"
      )
    )
  }

  test("a triangle remains three scans and carries both closing predicates") {
    val planned = CalciteTestSchemas.standard.plan(
      """
        |SELECT r.A, r.B, s.C
        |FROM TRIANGLE_R r
        |JOIN TRIANGLE_S s ON r.B = s.B
        |JOIN TRIANGLE_T t ON s.C = t.C AND t.A = r.A
        |""".stripMargin
    )

    assertFrontendCompleted(planned)
    assertOperatorCounts(
      planned,
      Map("LogicalProject" -> 1, "LogicalJoin" -> 2, "LogicalTableScan" -> 3)
    )
    assertEquals(
      planned.operatorNames,
      Vector(
        "LogicalProject",
        "LogicalJoin",
        "LogicalJoin",
        "LogicalTableScan",
        "LogicalTableScan",
        "LogicalTableScan"
      )
    )
    assert(planned.renderedPlan.contains("AND"), clues(planned.renderedPlan))
  }

  test("a self-join creates two distinct scans of the same table") {
    val planned = CalciteTestSchemas.standard.plan(
      """
        |SELECT employee.ID, manager.ID
        |FROM EMPLOYEE employee
        |JOIN EMPLOYEE manager ON employee.MANAGER_ID = manager.ID
        |""".stripMargin
    )

    assertFrontendCompleted(planned)
    assertOperatorCounts(
      planned,
      Map("LogicalProject" -> 1, "LogicalJoin" -> 1, "LogicalTableScan" -> 2)
    )
    assertEquals(
      planned.operatorNames,
      Vector("LogicalProject", "LogicalJoin", "LogicalTableScan", "LogicalTableScan")
    )

    val scans = planned.nodes.collect { case scan: TableScan => scan }
    assertEquals(scans.map(_.getTable.getQualifiedName.get(0)), Vector("EMPLOYEE", "EMPLOYEE"))
    assert(scans(0) ne scans(1))
  }

  test("Calcite parses duplicate-named schemas but rejects ambiguous star expansion") {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.duplicateColumnNames)
    val sql = "SELECT * FROM DUPLICATE_NAMES"

    // Calcite's type builder accepts the two identically named fields and the
    // parser is schema-agnostic. Validation cannot assign either field to the
    // expanded star, so no RelNode exists for this deliberately ambiguous SQL.
    assert(fixture.parse(sql) ne null)
    val error = intercept[ValidationException](fixture.plan(sql))
    assert(error.getMessage.contains("ambiguous"), clues(error.getMessage))
  }

  test("duplicate output aliases remain positional at the validated SQL boundary") {
    val planned = CalciteTestSchemas.standard.plan(
      """
        |SELECT a.ID AS DUPLICATE_VALUE, b.ID AS DUPLICATE_VALUE
        |FROM CHAIN_A a
        |JOIN CHAIN_B b ON a.B_ID = b.ID
        |""".stripMargin
    )

    assertFrontendCompleted(planned)
    assertEquals(
      planned.validatedFieldNames,
      Vector("DUPLICATE_VALUE", "DUPLICATE_VALUE")
    )
    assertEquals(planned.fieldNames.size, 2)
  }

  test("GROUP BY with multiple aggregates becomes one LogicalAggregate") {
    val planned = CalciteTestSchemas.standard.plan(
      """
        |SELECT DEPARTMENT_ID, SUM(AMOUNT) AS TOTAL, MAX(AMOUNT) AS HIGHEST
        |FROM FACT_SALE
        |GROUP BY DEPARTMENT_ID
        |""".stripMargin
    )

    assertFrontendCompleted(planned)
    assertOperatorCounts(
      planned,
      Map("LogicalAggregate" -> 1, "LogicalTableScan" -> 1)
    )
    assertEquals(
      planned.operatorNames,
      Vector("LogicalAggregate", "LogicalProject", "LogicalTableScan")
    )
    assert(planned.renderedPlan.contains("SUM"), clues(planned.renderedPlan))
    assert(planned.renderedPlan.contains("MAX"), clues(planned.renderedPlan))
  }

  test("COUNT star becomes a global aggregate with an empty group set") {
    val planned = CalciteTestSchemas.standard.plan(
      "SELECT COUNT(*) AS ROW_COUNT FROM FACT_SALE"
    )

    assertFrontendCompleted(planned)
    assertOperatorCounts(
      planned,
      Map("LogicalAggregate" -> 1, "LogicalTableScan" -> 1)
    )
    assertEquals(
      planned.operatorNames,
      Vector("LogicalAggregate", "LogicalTableScan")
    )
    assert(planned.renderedPlan.contains("group=[{}]"), clues(planned.renderedPlan))
    assert(planned.renderedPlan.contains("COUNT()"), clues(planned.renderedPlan))
  }

  test("DISTINCT aggregate with FILTER is retained on LogicalAggregate") {
    val planned = CalciteTestSchemas.standard.plan(
      """
        |SELECT DEPARTMENT_ID,
        |       COUNT(DISTINCT EMPLOYEE_ID) FILTER (WHERE ACTIVE) AS ACTIVE_PEOPLE
        |FROM FACT_SALE
        |GROUP BY DEPARTMENT_ID
        |""".stripMargin
    )

    assertFrontendCompleted(planned)
    assertOperatorCounts(
      planned,
      Map("LogicalAggregate" -> 1, "LogicalTableScan" -> 1)
    )
    assertEquals(
      planned.operatorNames,
      Vector("LogicalAggregate", "LogicalProject", "LogicalTableScan")
    )
    assert(planned.renderedPlan.contains("DISTINCT"), clues(planned.renderedPlan))
    assert(planned.renderedPlan.contains("FILTER"), clues(planned.renderedPlan))
  }

  test("query-level DISTINCT is represented as a grouping aggregate") {
    val planned = CalciteTestSchemas.standard.plan(
      "SELECT DISTINCT DEPARTMENT_ID FROM FACT_SALE"
    )

    assertFrontendCompleted(planned)
    assertOperatorCounts(
      planned,
      Map("LogicalAggregate" -> 1, "LogicalTableScan" -> 1)
    )
    assertEquals(
      planned.operatorNames,
      Vector("LogicalAggregate", "LogicalProject", "LogicalTableScan")
    )
  }
}
