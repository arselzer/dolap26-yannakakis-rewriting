package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.RewriteReasonCode
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class CalciteGroupedCountStarFrontendSuite extends munit.FunSuite {
  private val schema = {
    val root = Frameworks.createRootSchema(true)
    Vector("group_r", "group_s").foreach { name =>
      root.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("k", SqlTypeName.INTEGER)
              .add("payload", SqlTypeName.INTEGER)
              .build()
        }
      )
    }
    root
  }

  private val planner = CalciteSqlPlanner
    .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
    .fold(problem => fail(problem.toString), identity)

  private val joined =
    "FROM group_r AS r JOIN group_s AS s ON r.k = s.k"

  test("one direct key and exact COUNT(*) receive dual source and relational proof") {
    val report = analyze(
      "SELECT r.k AS \"Group Key\", COUNT(*) AS \"Rows\" " +
        joined + " GROUP BY r.k"
    )

    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root match {
      case value: GroupedCountStarIR => value
      case other => fail(s"expected GroupedCountStarIR, found $other")
    }
    assertEquals(root.groupColumn.ref.relationOccurrenceId.value, 0)
    assertEquals(root.groupOutput.name, "Group Key")
    assertEquals(root.groupOutput.dataType, "INTEGER NOT NULL")
    assert(!root.groupOutput.nullable)
    assertEquals(root.countOutput.name, "Rows")
    assertEquals(root.countOutput.dataType, "BIGINT NOT NULL")
    assert(!root.countOutput.nullable)
    assertEquals(report.query.output.map(_.name), Vector("Group Key", "Rows"))
    assertEquals(
      report.query.resultShape,
      QueryResultShape.GroupedCountStar(
        root.groupOutput,
        root.countOutput,
        GlobalCountStarAliasMode.Explicit
      )
    )

    val rows = analyze("SELECT r.k " + joined)
    assert(rows.isSupported, clues(rows.diagnostics))
    assertEquals(report.hypergraph, rows.hypergraph)
  }

  test("implicit count naming and duplicate validated aliases are preserved") {
    val implicitCount = analyze(
      "SELECT r.k AS group_key, COUNT(*) " + joined + " GROUP BY r.k"
    )
    assert(implicitCount.isSupported, clues(implicitCount.diagnostics))
    val implicitRoot = implicitCount.query.root.asInstanceOf[GroupedCountStarIR]
    assertEquals(implicitRoot.output.map(_.name), Vector("group_key", "count"))
    assertEquals(
      implicitRoot.sourceCertificate.countAliasMode,
      GlobalCountStarAliasMode.Implicit
    )

    val duplicates = analyze(
      "SELECT r.k AS same_name, COUNT(*) AS same_name " +
        joined + " GROUP BY r.k"
    )
    assert(duplicates.isSupported, clues(duplicates.diagnostics))
    assertEquals(duplicates.query.output.map(_.name), Vector.fill(2)("same_name"))
  }

  test("qualified keys work while a one-bag direct key has no lowering binding") {
    val oneBag = analyze("SELECT k, COUNT(*) FROM group_r GROUP BY k")
    val reports = Vector(
      analyze("SELECT r.k, COUNT(*) " + joined + " GROUP BY r.k"),
      oneBag
    )
    reports.foreach { report =>
      assert(report.isSupported, clues(report.diagnostics))
      assert(report.query.root.isInstanceOf[GroupedCountStarIR])
    }
    val oneBagHypergraph = oneBag.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    assertEquals(oneBagHypergraph.variables, Vector.empty)
    assertEquals(oneBagHypergraph.edges.flatMap(_.bindings), Vector.empty)
    val generated = SoftBagGenerator
      .generate(oneBagHypergraph, widthBound = 1)
      .fold(problem => fail(problem.message), identity)
    assert(
      CandidateSearchInput
        .create(
          oneBagHypergraph,
          widthBound = 1,
          generated.admissible(requireConnectedCover = true)
        )
        .isLeft
    )
  }

  test("bounded grouped COUNT grammar rejects wider or indirect aggregate shapes") {
    val rejected = Vector(
      "SELECT r.k, r.payload, COUNT(*) " + joined +
        " GROUP BY r.k, r.payload",
      "SELECT CAST(r.k AS BIGINT), COUNT(*) " + joined +
        " GROUP BY CAST(r.k AS BIGINT)",
      "SELECT r.k, COUNT(r.payload) " + joined + " GROUP BY r.k",
      "SELECT r.k, COUNT(DISTINCT r.payload) " + joined + " GROUP BY r.k",
      "SELECT r.k, COUNT(*) FILTER (WHERE TRUE) " + joined + " GROUP BY r.k",
      "SELECT r.k, COUNT(*) " + joined + " GROUP BY r.k HAVING TRUE",
      "SELECT r.k, COUNT(*), SUM(r.payload) " + joined + " GROUP BY r.k",
      "SELECT COUNT(*), r.k " + joined + " GROUP BY r.k",
      "SELECT r.k, COUNT(*) " + joined + " GROUP BY 1",
      "SELECT r.k AS key_alias, COUNT(*) " + joined + " GROUP BY key_alias",
      "SELECT r.k, COUNT(*) " + joined + " GROUP BY r.k ORDER BY r.k"
    )

    rejected.foreach { sql =>
      CalciteSqlPlanner.planWithSourceEvidence(planner, sql) match {
        case Left(problem) => assert(problem.stage.invalidatesSql, clues(sql, problem))
        case Right(result) =>
          assert(
            !result.sourceEvidence.isInstanceOf[
              CalciteSqlSourceEvidence.CertifiedGroupedCountStar
            ],
            clues(sql, result.sourceEvidence)
          )
          val report = CalciteQueryAnalyzer.analyze(result)
          assert(!report.isSupported, clues(sql, report.diagnostics))
          assert(
            report.diagnostics.exists(
              _.code == RewriteReasonCode.UnsupportedAggregate
            ),
            clues(sql, report.diagnostics)
          )
      }
    }
  }

  test("grouped wrapper constructors reject non-column keys and nested counts") {
    val root = analyze(
      "SELECT r.k, COUNT(*) " + joined + " GROUP BY r.k"
    ).query.root.asInstanceOf[GroupedCountStarIR]

    intercept[IllegalArgumentException] {
      root.copy(
        groupOutput = root.groupOutput.copy(
          expression = ScalarExpression.Literal("1", "INTEGER", nullable = false)
        )
      )
    }
    intercept[IllegalArgumentException] {
      root.copy(countOutput = root.countOutput.copy(nullable = true))
    }
    intercept[IllegalArgumentException] {
      root.copy(input = root)
    }
  }

  private def planned(sql: String): CalciteSqlPlannedQuery =
    CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)

  private def analyze(sql: String): AnalysisReport =
    CalciteQueryAnalyzer.analyze(planned(sql))
}
