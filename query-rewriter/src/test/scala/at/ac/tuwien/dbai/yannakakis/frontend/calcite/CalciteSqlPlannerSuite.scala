package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import org.apache.calcite.avatica.util.Casing
import org.apache.calcite.tools.Frameworks

class CalciteSqlPlannerSuite extends munit.FunSuite {
  test("PostgreSQL parser configuration matches identifier casing") {
    val config = CalciteSqlPlanner.PostgreSqlParserConfig

    assertEquals(config.unquotedCasing(), Casing.TO_LOWER)
    assertEquals(config.quotedCasing(), Casing.UNCHANGED)
    assert(config.caseSensitive())
  }

  test("schema factory creates a reusable fresh-planner boundary") {
    val schema = Frameworks.createRootSchema(true)
    val planner = CalciteSqlPlanner
      .forSchema(schema)
      .fold(problem => fail(problem.message), identity)

    val first = planner.plan("VALUES (1)")
    val second = planner.plan("VALUES (2)")
    assert(first.isRight)
    assert(second.isRight)
  }

  test("real parse and validation exceptions expose stable text, not SQL") {
    val schema = Frameworks.createRootSchema(true)
    val planner = CalciteSqlPlanner
      .forSchema(schema)
      .fold(problem => fail(problem.message), identity)

    val syntaxSecret = "jdbc:postgresql://user:pass@host/db?password=hunter2"
    val parse = planner.plan(s"SELECT '$syntaxSecret") match {
      case Left(problem) => problem
      case Right(_)      => fail("malformed SQL unexpectedly parsed")
    }
    assertEquals(parse.stage, CalciteSqlPlanningStage.Parsing)
    assertEquals(parse.message, "Calcite SQL parsing failed")
    assert(!parse.message.contains(syntaxSecret))

    val catalogSecret = "password=classified"
    val validation = planner.plan(
      s"SELECT * FROM \"missing_$catalogSecret\""
    ) match {
      case Left(problem) => problem
      case Right(_)      => fail("missing table unexpectedly validated")
    }
    assertEquals(validation.stage, CalciteSqlPlanningStage.Validation)
    assertEquals(validation.message, "Calcite SQL validation failed")
    assert(!validation.message.contains(catalogSecret))
  }

  test("planner factories reject missing configuration as typed values") {
    assert(CalciteSqlPlanner.fromFrameworkConfig(null).isLeft)
    assert(CalciteSqlPlanner.forSchema(null).isLeft)
    assert(CalciteSqlPlanner.fromFunction(null).isLeft)

    val jdbcFailure = CalciteSqlPlanner.forPostgreSqlJdbc(
      null,
      null,
      null,
      null
    )
    jdbcFailure match {
      case Left(problem) =>
        assertEquals(problem.stage, CalciteSqlPlanningStage.Configuration)
        assertEquals(problem.exceptionClass, None)
        assert(!problem.message.contains("null"))
      case Right(_) => fail("missing JDBC configuration unexpectedly accepted")
    }
  }

  test("planning stages have unique stable ids and user-error classification") {
    assertEquals(
      CalciteSqlPlanningStage.values.map(_.id).distinct.size,
      CalciteSqlPlanningStage.values.size
    )
    assertEquals(
      CalciteSqlPlanningStage.values.filter(_.invalidatesSql),
      Vector(
        CalciteSqlPlanningStage.Parsing,
        CalciteSqlPlanningStage.Validation
      )
    )
  }
}
