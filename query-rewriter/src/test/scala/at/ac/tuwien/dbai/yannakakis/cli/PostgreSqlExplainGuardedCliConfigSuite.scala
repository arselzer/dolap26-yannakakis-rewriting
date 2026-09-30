package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlExplainSelectionLimits

class PostgreSqlExplainGuardedCliConfigSuite extends munit.FunSuite {
  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
      "jdbc:postgresql://db.invalid/research",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "researcher",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "MiXeD Schema"
  )

  test("guarded EXPLAIN policy and timeout compose with every independent rewrite option") {
    val config = parsed(
      Array(
        "rewrite",
        "--stdin",
        "--selection",
        "postgresql-explain-row-guarded-total-cost-v1",
        "--explain-timeout-seconds",
        "17",
        "--width-bound",
        "3",
        "--plan-limit",
        "7",
        "--reduction-schedule",
        "none",
        "--no-connected-cover",
        "--schema",
        "MiXeD Schema"
      )
    )

    assertEquals(
      config.options.candidateSelection,
      CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1
    )
    assertEquals(config.explainLimits.statementTimeoutSeconds, 17)
    assertEquals(
      config.explainLimits.maxPlanJsonCharacters,
      PostgreSqlExplainSelectionLimits.default.maxPlanJsonCharacters
    )
    assertEquals(
      config.explainLimits.maxCumulativePlanJsonCharacters,
      PostgreSqlExplainSelectionLimits.default.maxCumulativePlanJsonCharacters
    )
    assertEquals(config.options.widthBound, 3)
    assertEquals(config.options.planLimit, 7)
    assertEquals(config.options.reductionMode, YannakakisReductionMode.NoReduction)
    assertEquals(config.options.requireConnectedCover, false)
    assertEquals(config.schemaName, "MiXeD Schema")
  }

  test("EXPLAIN option errors are diagnosed before unrelated database environment") {
    val emptyEnvironment = Map.empty[String, String]
    val incompatible = failed(
      Array(
        "rewrite",
        "--stdin",
        "--selection",
        "postgresql-static-shape-v1",
        "--explain-timeout-seconds",
        "5"
      ),
      emptyEnvironment
    )
    assertEquals(incompatible.context("option"), "--explain-timeout-seconds")
    assert(!incompatible.context.contains("required"))

    Vector("0", "-1", "not-an-integer").foreach { value =>
      val failure = failed(
        Array(
          "rewrite",
          "--stdin",
          "--selection",
          "postgresql-explain-row-guarded-total-cost-v1",
          "--explain-timeout-seconds",
          value
        ),
        emptyEnvironment
      )
      assertEquals(failure.context("option"), "--explain-timeout-seconds")
      assert(!failure.toString.contains(value), clues(value, failure))
    }
  }

  test("guarded policy remains mutually exclusive with exact rank and id selectors") {
    val candidateId =
      "sha256:b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d"
    Vector(
      Vector("--candidate-rank", "2"),
      Vector("--candidate-id", candidateId)
    ).foreach { exact =>
      val failure = failed(
        (Vector(
          "rewrite",
          "--stdin",
          "--selection",
          "postgresql-explain-row-guarded-total-cost-v1"
        ) ++ exact).toArray,
        Map.empty
      )
      assert(failure.message.contains("mutually exclusive"), clues(exact))
      assertEquals(
        failure.context("options").split(',').toVector,
        Vector("--selection", exact.head).sorted
      )
    }
  }

  test("default invocation retains structural selection and default EXPLAIN envelope") {
    val config = parsed(Array("rewrite", "--stdin"))
    assertEquals(
      config.options.candidateSelection,
      CandidatePlanSelection.StructuralHead
    )
    assertEquals(config.explainLimits, PostgreSqlExplainSelectionLimits.default)
  }

  test("help exposes only the frozen guarded policy and explicit timeout flag") {
    val usage = PostgreSqlRewriteCliConfig.Usage
    assert(usage.contains("postgresql-explain-row-guarded-total-cost-v1"))
    assert(usage.contains("--explain-timeout-seconds N"))
    assert(!usage.toLowerCase.contains("measured runtime"))
  }

  private def parsed(arguments: Array[String]): PostgreSqlRewriteCliConfig =
    PostgreSqlRewriteCliConfig.parse(arguments, environment) match {
      case Right(PostgreSqlRewriteCliCommand.Rewrite(config)) => config
      case other => fail(s"expected rewrite configuration, received $other")
    }

  private def failed(
      arguments: Array[String],
      selectedEnvironment: Map[String, String]
  ): PostgreSqlRewriteCliFailure =
    PostgreSqlRewriteCliConfig.parse(arguments, selectedEnvironment) match {
      case Left(problem) => problem
      case other         => fail(s"expected invocation failure, received $other")
    }
}
