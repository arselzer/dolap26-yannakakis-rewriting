package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.RewriteReasonCode
import at.ac.tuwien.dbai.yannakakis.core.VariableId
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class CalciteDistributedGroupedCountStarFrontendSuite extends munit.FunSuite {
  private val schema = {
    val root = Frameworks.createRootSchema(true)

    root.add(
      "edge",
      new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType =
          factory
            .builder()
            .add("left_key", SqlTypeName.INTEGER)
            .nullable(true)
            .add("right_key", SqlTypeName.INTEGER)
            .nullable(true)
            .build()
      }
    )

    Vector(
      "u0" -> Vector("g0" -> false, "k01" -> true),
      "u1" -> Vector("k10" -> true, "k12" -> true),
      "u2" -> Vector("g2" -> true, "k23" -> true),
      "u3" -> Vector("k32" -> true, "k30" -> true)
    ).foreach { case (tableName, columns) =>
      root.add(
        tableName,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType = {
            val builder = factory.builder()
            columns.foreach { case (name, nullable) =>
              builder.add(name, SqlTypeName.INTEGER).nullable(nullable)
            }
            builder.build()
          }
        }
      )
    }
    root
  }

  private val planner = CalciteSqlPlanner
    .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
    .fold(problem => fail(problem.toString), identity)

  private val distinctTableC4 =
    "FROM u0 JOIN u1 ON u0.k01 = u1.k10 " +
      "JOIN u2 ON u1.k12 = u2.g2 " +
      "JOIN u3 ON u2.k23 = u3.k32 AND u3.k30 = u0.g0"

  private val selfJoinC4 =
    "FROM edge AS e0 " +
      "JOIN edge AS e1 ON e0.right_key = e1.left_key " +
      "JOIN edge AS e2 ON e1.right_key = e2.left_key " +
      "JOIN edge AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key"

  private def threeKeyPlanned(sql: String): CalciteSqlPlannedQuery = {
    val capable = CalciteSqlPlanner.forSchemaWithDistributedCountGrouping(schema, maximumGroupKeys = 3)
      .fold(problem => fail(problem.toString), identity)
    CalciteSqlPlanner.planWithSourceEvidence(capable, sql)
      .fold(problem => fail(problem.toString), identity)
  }

  test("opt-in three-key COUNT retains order, self-join identity and duplicate labels") {
    val sql = "SELECT e2.left_key AS x, e0.left_key AS x, e1.left_key AS x, COUNT(*) " +
      selfJoinC4 + " GROUP BY e2.left_key, e0.left_key, e1.left_key"
    assert(!analyze(sql).isSupported)
    val report = CalciteQueryAnalyzer.analyze(threeKeyPlanned(sql))
    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root.asInstanceOf[DistributedGroupedCountStarIR]
    assertEquals(root.sourceCertificate.groupKeyCount, 3)
    assertEquals(root.groupOccurrenceIds.map(_.value), Vector(2, 0, 1))
    assertEquals(root.output.map(_.name), Vector("x", "x", "x", "count"))
    assertEquals(root.countOutput.ordinal, 3)
    intercept[IllegalArgumentException] {
      root.copy(sourceCertificate = root.sourceCertificate.copy(groupKeyCount = 2))
    }
  }

  test("three-key capability rejects wider, indirect, same-occurrence and mismatched certificates") {
    val sql = "SELECT e0.left_key, e1.left_key, e2.left_key, COUNT(*) " +
      selfJoinC4 + " GROUP BY e0.left_key, e1.left_key, e2.left_key"
    val three = threeKeyPlanned(sql)
    val two = planned("SELECT e0.left_key, e2.left_key, COUNT(*) " + selfJoinC4 +
      " GROUP BY e0.left_key, e2.left_key")
    Vector(CalciteSqlPlannedQuery(three.root, two.sourceEvidence),
      CalciteSqlPlannedQuery(two.root, three.sourceEvidence)).foreach { hostile =>
      assert(!CalciteQueryAnalyzer.analyze(hostile).isSupported)
    }
    Vector(
      sql.replace("e2.left_key", "e0.right_key"),
      sql.replace("e2.left_key", "e2.left_key + 1"),
      "SELECT e0.left_key, e1.left_key, e2.left_key, e3.left_key, COUNT(*) " +
        selfJoinC4 + " GROUP BY e0.left_key, e1.left_key, e2.left_key, e3.left_key"
    ).foreach { rejected =>
      assert(!CalciteQueryAnalyzer.analyze(threeKeyPlanned(rejected)).isSupported, clues(rejected))
    }
  }

  test("the distributed grouping bound is configurable rather than a three-key special case") {
    val sql = "SELECT e3.left_key,e1.left_key,e0.left_key,e2.left_key,COUNT(*) " +
      selfJoinC4 + " GROUP BY e3.left_key,e1.left_key,e0.left_key,e2.left_key"
    val capable = CalciteSqlPlanner.forSchemaWithDistributedCountGrouping(schema, 4)
      .fold(p => fail(p.toString), identity)
    val query = CalciteSqlPlanner.planWithSourceEvidence(capable, sql)
      .fold(p => fail(p.toString), identity)
    val report = CalciteQueryAnalyzer.analyze(query)
    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root.asInstanceOf[DistributedGroupedCountStarIR]
    assertEquals(root.sourceCertificate.groupKeyCount, 4)
    assertEquals(root.groupOccurrenceIds.map(_.value), Vector(3, 1, 0, 2))
    assert(!CalciteQueryAnalyzer.analyze(threeKeyPlanned(sql)).isSupported)
    assert(CalciteSqlPlanner.forSchemaWithDistributedCountGrouping(schema, 1).isLeft)
  }

  test("two distributed direct keys receive concordant source and relational proof") {
    val sql =
      "SELECT g0 AS \"First Key\", u2.g2 AS \"Second Key\", " +
        "COUNT(*) AS \"Rows\" " + distinctTableC4 +
        " GROUP BY u0.g0, g2"
    val plannedQuery = planned(sql)
    plannedQuery.sourceEvidence match {
      case CalciteSqlSourceEvidence.CertifiedDistributedGroupedCountStar(
            certificate
          ) =>
        assertEquals(
          certificate.countAliasMode,
          GlobalCountStarAliasMode.Explicit
        )
      case other => fail(s"expected distributed grouped certificate, found $other")
    }

    val report = CalciteQueryAnalyzer.analyze(plannedQuery)
    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root match {
      case value: DistributedGroupedCountStarIR => value
      case other =>
        fail(s"expected DistributedGroupedCountStarIR, found $other")
    }
    assertEquals(
      root.groupOccurrenceIds.map(_.value),
      Vector(0, 2)
    )
    assertEquals(
      root.groupOutputs.map(output => output.dataType -> output.nullable),
      Vector("INTEGER NOT NULL" -> false, "INTEGER" -> true)
    )
    assertEquals(
      root.output.map(_.name),
      Vector("First Key", "Second Key", "Rows")
    )
    assertEquals(root.countOutput.dataType, "BIGINT NOT NULL")
    assert(!root.countOutput.nullable)
    assertEquals(
      report.query.resultShape,
      QueryResultShape.DistributedGroupedCountStar(
        root.groupOutputs,
        root.countOutput,
        GlobalCountStarAliasMode.Explicit
      )
    )

    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    val variables = root.groupColumnRefs.map(ref => variableFor(ref, hypergraph))
    assertEquals(variables.distinct.size, 2)

    val enumeration = analyze(
      "SELECT u0.g0, u2.g2 " + distinctTableC4
    )
    assert(enumeration.isSupported, clues(enumeration.diagnostics))
    assertEquals(report.hypergraph, enumeration.hypergraph)
  }

  test("validated output order, duplicate aliases, and implicit count name survive group reordering") {
    val report = analyze(
      "SELECT u2.g2 AS same_name, u0.g0 AS same_name, COUNT(*) " +
        distinctTableC4 + " GROUP BY u2.g2, u0.g0"
    )

    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root.asInstanceOf[DistributedGroupedCountStarIR]
    assertEquals(root.groupOccurrenceIds.map(_.value), Vector(2, 0))
    assertEquals(
      root.output.map(_.name),
      Vector("same_name", "same_name", "count")
    )
    assertEquals(
      root.sourceCertificate.countAliasMode,
      GlobalCountStarAliasMode.Implicit
    )
  }

  test("self-join keys retain distinct occurrence identity") {
    val report = analyze(
      "SELECT e0.left_key AS g0, e2.left_key AS g2, COUNT(*) AS n " +
        selfJoinC4 + " GROUP BY e0.left_key, e2.left_key"
    )

    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root.asInstanceOf[DistributedGroupedCountStarIR]
    assertEquals(root.groupOccurrenceIds.map(_.value), Vector(0, 2))
    assertEquals(root.groupColumnRefs.map(_.ordinal), Vector(0, 0))
    assertEquals(
      report.query.relations.map(_.qualifiedName.last).distinct,
      Vector("edge")
    )

    val implicitNames = analyze(
      "SELECT e0.left_key, e2.left_key, COUNT(*) " + selfJoinC4 +
        " GROUP BY e0.left_key, e2.left_key"
    ).query.root.asInstanceOf[DistributedGroupedCountStarIR]
    assertEquals(
      implicitNames.output.map(_.name),
      Vector("left_key", "left_key", "count")
    )
  }

  test("distinct occurrences may reach the frontend with one structural variable") {
    val report = analyze(
      "SELECT e0.right_key, e1.left_key, COUNT(*) " + selfJoinC4 +
        " GROUP BY e0.right_key, e1.left_key"
    )
    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root.asInstanceOf[DistributedGroupedCountStarIR]
    assertEquals(root.groupOccurrenceIds.map(_.value), Vector(0, 1))
    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    assertEquals(
      root.groupColumnRefs.map(ref => variableFor(ref, hypergraph)).distinct.size,
      1
    )
  }

  test("two keys supplied by one occurrence fail at the relational certificate") {
    val query = planned(
      "SELECT e0.left_key, e0.right_key, COUNT(*) " + selfJoinC4 +
        " GROUP BY e0.left_key, e0.right_key"
    )
    assert(
      query.sourceEvidence.isInstanceOf[
        CalciteSqlSourceEvidence.CertifiedDistributedGroupedCountStar
      ],
      clues(query.sourceEvidence)
    )

    val report = CalciteQueryAnalyzer.analyze(query)
    assert(!report.isSupported, clues(report.diagnostics))
    assert(!report.query.root.isInstanceOf[DistributedGroupedCountStarIR])
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate),
      clues(report.diagnostics)
    )
  }

  test("the bounded raw grammar rejects wider, reordered, and indirect shapes") {
    val rejected = Vector(
      "SELECT u0.g0, u2.g2, u3.k32, COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2, u3.k32",
      "SELECT CAST(u0.g0 AS BIGINT), u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY CAST(u0.g0 AS BIGINT), u2.g2",
      "SELECT u0.g0, u2.g2, COUNT(u1.k10) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2",
      "SELECT u0.g0, u2.g2, COUNT(DISTINCT u1.k10) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2",
      "SELECT u0.g0, u2.g2, COUNT(ALL *) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2",
      "SELECT u0.g0, u2.g2, pg_catalog.COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2",
      "SELECT u0.g0, u2.g2, COUNT(*) FILTER (WHERE TRUE) " +
        distinctTableC4 + " GROUP BY u0.g0, u2.g2",
      "SELECT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2 HAVING TRUE",
      "SELECT u0.g0, u2.g2, COUNT(*), SUM(u1.k10) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2",
      "SELECT COUNT(*), u0.g0, u2.g2 " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2",
      "SELECT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY u2.g2, u0.g0",
      "SELECT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY 1, 2",
      "SELECT u0.g0 AS a, u2.g2 AS b, COUNT(*) " + distinctTableC4 +
        " GROUP BY a, b",
      "SELECT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2 ORDER BY u0.g0",
      "SELECT u0.g0, u2.g2, COUNT(*) OVER () " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2",
      "SELECT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2 LIMIT 1",
      "SELECT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2 OFFSET 1",
      "SELECT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY GROUPING SETS ((u0.g0, u2.g2))",
      "SELECT DISTINCT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2",
      "SELECT u0.g0, u2.g2, COUNT(*) + 0 " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2",
      "SELECT u0.g0, u0.g0, COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u0.g0"
    )

    rejected.foreach { sql =>
      CalciteSqlPlanner.planWithSourceEvidence(planner, sql) match {
        case Left(problem) =>
          assert(problem.stage.invalidatesSql, clues(sql, problem))
        case Right(query) =>
          assert(
            !query.sourceEvidence.isInstanceOf[
              CalciteSqlSourceEvidence.CertifiedDistributedGroupedCountStar
            ],
            clues(sql, query.sourceEvidence)
          )
          val report = CalciteQueryAnalyzer.analyze(query)
          assert(!report.query.root.isInstanceOf[DistributedGroupedCountStarIR])
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

  test("raw and relational certificates must agree and legacy analysis remains rejecting") {
    val certifiedQuery = planned(
      "SELECT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2"
    )
    val oneKey = planned(
      "SELECT u0.g0, COUNT(*) " + distinctTableC4 + " GROUP BY u0.g0"
    )
    val hostile = CalciteSqlPlannedQuery(
      oneKey.root,
      certifiedQuery.sourceEvidence
    )
    val mismatch = CalciteQueryAnalyzer.analyze(hostile)
    assert(!mismatch.isSupported, clues(mismatch.diagnostics))
    assert(!mismatch.query.root.isInstanceOf[DistributedGroupedCountStarIR])
    assert(
      mismatch.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate)
    )

    val legacy = CalciteQueryAnalyzer.analyze(certifiedQuery.root)
    assert(!legacy.isSupported, clues(legacy.diagnostics))
    assert(!legacy.query.root.isInstanceOf[DistributedGroupedCountStarIR])
  }

  test("typed distributed wrapper rejects malformed keys, metadata, and nesting") {
    val root = analyze(
      "SELECT u0.g0, u2.g2, COUNT(*) " + distinctTableC4 +
        " GROUP BY u0.g0, u2.g2"
    ).query.root.asInstanceOf[DistributedGroupedCountStarIR]

    intercept[IllegalArgumentException] {
      root.copy(groupOutputs = Vector(root.groupOutputs.head))
    }
    intercept[IllegalArgumentException] {
      root.copy(
        groupOutputs = root.groupOutputs.updated(
          1,
          root.groupOutputs(1).copy(expression = root.groupColumns.head)
        )
      )
    }
    intercept[IllegalArgumentException] {
      root.copy(
        groupOutputs = root.groupOutputs.updated(
          0,
          root.groupOutputs.head.copy(
            expression = ScalarExpression.Literal(
              "1",
              "INTEGER NOT NULL",
              nullable = false
            )
          )
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

  private def variableFor(
      ref: ColumnRef,
      hypergraph: at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
  ): VariableId =
    hypergraph.edges
      .find(_.relationOccurrenceId == ref.relationOccurrenceId)
      .flatMap(_.bindings.collectFirst {
        case (columnId, variableId) if columnId == ref.id => variableId
      })
      .getOrElse(fail(s"missing structural variable for ${ref.id}"))

  private def planned(sql: String): CalciteSqlPlannedQuery =
    CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)

  private def analyze(sql: String): AnalysisReport =
    CalciteQueryAnalyzer.analyze(planned(sql))
}
