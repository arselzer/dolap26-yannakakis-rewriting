package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.calcite.{TestColumn, TestTable}
import at.ac.tuwien.dbai.yannakakis.core.{ColumnId, EdgeId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  CalciteQueryAnalyzer,
  CalciteSqlPlanner
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.tools.Frameworks

/** Focused tests for the SQL-free relevance/CSE certificate. */
final class PostgreSqlRelevanceLimitedCseSuite extends munit.FunSuite {
  private val tables = Vector(
    TestTable(
      "v4_link",
      Vector(TestColumn("left_key"), TestColumn("right_key"))
    ),
    TestTable(
      "v4_fact",
      Vector(
        TestColumn("movie_id"),
        TestColumn("alt_movie_id"),
        TestColumn("dim_id")
      )
    ),
    TestTable(
      "v4_fact_other",
      Vector(
        TestColumn("movie_id"),
        TestColumn("alt_movie_id"),
        TestColumn("dim_id")
      )
    ),
    TestTable("v4_dim", Vector(TestColumn("id"))),
    TestTable(
      "v4_left_anchor",
      Vector(TestColumn("join_id"), TestColumn("left_key"))
    ),
    TestTable(
      "v4_right_anchor",
      Vector(TestColumn("join_id"), TestColumn("right_key"))
    )
  )

  private val symmetric =
    """SELECT COUNT(*) AS n
      |FROM v4_link AS l
      |JOIN v4_fact AS f1 ON f1.movie_id = l.left_key
      |JOIN v4_dim AS d1 ON d1.id = f1.dim_id
      |JOIN v4_fact AS f2 ON f2.movie_id = l.right_key
      |JOIN v4_dim AS d2 ON d2.id = f2.dim_id""".stripMargin

  private val planner = {
    val schema = Frameworks.createRootSchema(true)
    tables.foreach { table =>
      schema.add(
        table.name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType = {
            val builder = factory.builder()
            table.columns.foreach { column =>
              val base = factory.createSqlType(column.sqlType)
              builder.add(
                column.name,
                factory.createTypeWithNullability(base, column.nullable)
              )
            }
            builder.build()
          }
        }
      )
    }
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  test("derives two maximal isomorphic one-key wings around one anchor") {
    val prepared = prepare(symmetric)
    val plan = PostgreSqlRelevanceLimitedCse
      .derive(prepared.query, prepared.input)
      .fold(errors => fail(errors.map(_.detail).mkString("; ")), identity)

    assertEquals(
      relationNames(prepared, Vector(plan.anchorEdgeId)),
      Vector("v4_link")
    )
    assertEquals(plan.anchorInterfaces.size, 2)
    assertEquals(
      plan.anchorInterfaces
        .map(interface => columnName(prepared, interface.anchorColumnId))
        .sorted,
      Vector("left_key", "right_key")
    )
    assertEquals(plan.sharedWing.members.size, 2)
    assertEquals(
      relationNames(prepared, plan.sharedWing.representative.edgeIds),
      Vector("v4_dim", "v4_fact")
    )
    assertEquals(relationNames(prepared, plan.coreEdgeIds), Vector("v4_link"))
    assertEquals(plan.keySqlTypeName, "INTEGER")
    assertEquals(
      PostgreSqlRelevanceLimitedCse
        .validate(prepared.query, prepared.input, plan),
      Right(plan)
    )
  }

  test("aliases and equality operand direction do not prevent matching") {
    val renamed =
      """SELECT COUNT(*) AS n
        |FROM v4_link AS root
        |JOIN v4_fact AS right_fact
        |  ON root.right_key = right_fact.movie_id
        |JOIN v4_dim AS right_dim ON right_fact.dim_id = right_dim.id
        |JOIN v4_fact AS left_fact ON root.left_key = left_fact.movie_id
        |JOIN v4_dim AS left_dim ON left_fact.dim_id = left_dim.id""".stripMargin

    val prepared = prepare(renamed)
    assert(
      PostgreSqlRelevanceLimitedCse
        .derive(prepared.query, prepared.input)
        .isRight
    )
  }

  test("does not equate different qualified relation identities") {
    assertInapplicable(
      symmetric.replace("v4_fact AS f2", "v4_fact_other AS f2")
    )
  }

  test("does not equate different physical boundary columns") {
    assertInapplicable(
      symmetric.replace(
        "f2.movie_id = l.right_key",
        "f2.alt_movie_id = l.right_key"
      )
    )
  }

  test("does not detach wings coupled by an internal variable") {
    assertInapplicable(symmetric + "\nAND d1.id = d2.id", widthBound = 2)
  }

  test("requires one anchor occurrence to bind both interface variables") {
    val noCommonAnchor =
      """SELECT COUNT(*) AS n
        |FROM v4_left_anchor AS la
        |JOIN v4_right_anchor AS ra ON ra.join_id = la.join_id
        |JOIN v4_fact AS f1 ON f1.movie_id = la.left_key
        |JOIN v4_dim AS d1 ON d1.id = f1.dim_id
        |JOIN v4_fact AS f2 ON f2.movie_id = ra.right_key
        |JOIN v4_dim AS d2 ON d2.id = f2.dim_id""".stripMargin
    assertInapplicable(noCommonAnchor)
  }

  private def prepare(sql: String, widthBound: Int = 1) = {
    val planned = CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)
    val report = CalciteQueryAnalyzer.analyze(planned)
    assert(report.isSupported, clues(sql, report.diagnostics))
    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    PostgreSqlRendererTestSupport.prepareQuery(
      report,
      hypergraph,
      widthBound,
      requireConnectedCover = true
    )
  }

  private def assertInapplicable(sql: String, widthBound: Int = 1): Unit = {
    val prepared = prepare(sql, widthBound)
    val result =
      PostgreSqlRelevanceLimitedCse.derive(prepared.query, prepared.input)
    assert(result.isLeft, clues(result))
  }

  private def relationNames(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      edgeIds: Vector[EdgeId]
  ): Vector[String] = {
    val occurrenceByEdge = prepared.hypergraph.edges.iterator
      .map(edge => edge.id -> edge.relationOccurrenceId)
      .toMap
    val relationByOccurrence = prepared.query.relations.iterator
      .map(relation => relation.id -> relation)
      .toMap
    edgeIds
      .map(edgeId =>
        relationByOccurrence(occurrenceByEdge(edgeId)).qualifiedName.last
      )
      .sorted
  }

  private def columnName(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      columnId: ColumnId
  ): String =
    prepared.query.relations
      .flatMap(_.columns)
      .find(_.ref.id == columnId)
      .map(_.sourceName)
      .getOrElse(fail(s"missing column $columnId"))
}
