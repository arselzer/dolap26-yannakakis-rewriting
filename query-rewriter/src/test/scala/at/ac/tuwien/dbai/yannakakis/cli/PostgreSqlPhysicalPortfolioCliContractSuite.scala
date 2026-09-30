package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlPhysicalPortfolioEvaluationLimits,
  PostgreSqlPhysicalPortfolioLimits
}

/** Frozen CLI/API activation contract for the opt-in physical portfolio. */
class PostgreSqlPhysicalPortfolioCliContractSuite extends munit.FunSuite {
  private val cliPolicy =
    "postgresql-explain-factor-aware-physical-portfolio-v1"
  private val internalPolicy =
    "postgresql_explain_factor_aware_physical_portfolio_v1"
  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
      "jdbc:postgresql://db.invalid/research",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "researcher",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "public"
  )

  test("CLI accepts only the frozen policy token with K=100 and no reduction flag") {
    val config = parsed(
      Array(
        "rewrite",
        "--stdin",
        "--selection",
        cliPolicy,
        "--plan-limit",
        "100",
        "--explain-timeout-seconds",
        "7"
      ),
      environment
    )

    assertEquals(
      config.options.candidateSelection,
      CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
    )
    assertEquals(config.options.planLimit, 100)
    assertEquals(
      config.options.reductionMode,
      YannakakisReductionMode.FullTwoPass
    )
    assertEquals(
      config.physicalPortfolioLimits,
      PostgreSqlPhysicalPortfolioLimits.default
    )
    assertEquals(
      config.physicalPortfolioEvaluationLimits.statementTimeoutSeconds,
      7
    )
    assertEquals(
      config.physicalPortfolioEvaluationLimits.explainStatementBudgetSeconds,
      PostgreSqlPhysicalPortfolioEvaluationLimits.default.explainStatementBudgetSeconds
    )
    assert(
      PostgreSqlRewriteCliConfig.Usage.contains(cliPolicy),
      clues(PostgreSqlRewriteCliConfig.Usage)
    )
    assert(
      PostgreSqlRewriteCliConfig.Usage.contains(
        "physical portfolio requires --plan-limit 100"
      ),
      clues(PostgreSqlRewriteCliConfig.Usage)
    )
    assert(
      PostgreSqlRewriteCliConfig.Usage.contains(
        "forbids --reduction-schedule"
      ),
      clues(PostgreSqlRewriteCliConfig.Usage)
    )
  }

  test("CLI rejects every explicit reduction schedule before database environment checks") {
    YannakakisReductionMode.Values.foreach { mode =>
      val permutations = Vector(
        Array(
          "rewrite",
          "--stdin",
          "--selection",
          cliPolicy,
          "--plan-limit",
          "100",
          "--reduction-schedule",
          mode.id
        ),
        Array(
          "rewrite",
          "--stdin",
          "--reduction-schedule",
          mode.id,
          "--selection",
          cliPolicy,
          "--plan-limit",
          "100"
        )
      )
      permutations.foreach { arguments =>
        val failure = failed(arguments, Map.empty)
        assertEquals(failure.context("option"), "--reduction-schedule")
        assertEquals(
          failure.context("selectionPolicy"),
          CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1.id
        )
        assert(!failure.context.contains("required"), clues(mode, failure))
      }
    }
  }

  test("CLI rejects non-K100 limits and the internal policy id") {
    Vector(None, Some("99"), Some("101")).foreach { suppliedLimit =>
      val arguments = Vector(
        "rewrite",
        "--stdin",
        "--selection",
        cliPolicy
      ) ++ suppliedLimit.toVector.flatMap(value =>
        Vector("--plan-limit", value)
      )
      val failure = failed(arguments.toArray, environment)
      assertEquals(failure.context("option"), "--plan-limit")
      assertEquals(
        failure.context("value"),
        suppliedLimit.getOrElse("10")
      )
      assertEquals(
        failure.context("valueSource"),
        suppliedLimit.fold("default")(_ => "supplied")
      )
      assertEquals(failure.context("required"), "100")
      assertEquals(
        failure.context("selectionPolicy"),
        CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1.id
      )
    }

    val internal = failed(
      Array(
        "rewrite",
        "--stdin",
        "--selection",
        internalPolicy,
        "--plan-limit",
        "100"
      ),
      environment
    )
    assertEquals(internal.context("option"), "--selection")
  }

  test("programmatic API requires FullTwoPass solely as a neutral sentinel") {
    val accepted = RewriteOptions(
      planLimit = 100,
      candidateSelection =
        CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1,
      reductionMode = YannakakisReductionMode.FullTwoPass
    )
    assertEquals(accepted.planLimit, RewriteOptions.PhysicalPortfolioPlanLimit)

    Vector(
      YannakakisReductionMode.BottomUpOnly,
      YannakakisReductionMode.NoReduction
    ).foreach { mode =>
      intercept[IllegalArgumentException] {
        accepted.copy(reductionMode = mode)
      }
      val diagnostics = RewriteOptions
        .validated(
          planLimit = 100,
          candidateSelection =
            CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1,
          reductionMode = mode
        )
        .left
        .getOrElse(fail(s"$mode unexpectedly passed validation"))
      assertEquals(diagnostics.map(_.context("option")), Vector("reductionMode"))
    }
  }

  private def parsed(
      arguments: Array[String],
      selectedEnvironment: Map[String, String]
  ): PostgreSqlRewriteCliConfig =
    PostgreSqlRewriteCliConfig.parse(arguments, selectedEnvironment) match {
      case Right(PostgreSqlRewriteCliCommand.Rewrite(config)) => config
      case other => fail(s"physical portfolio CLI did not parse: $other")
    }

  private def failed(
      arguments: Array[String],
      selectedEnvironment: Map[String, String]
  ): PostgreSqlRewriteCliFailure =
    PostgreSqlRewriteCliConfig.parse(arguments, selectedEnvironment) match {
      case Left(problem) => problem
      case other => fail(s"physical portfolio CLI unexpectedly parsed: $other")
    }
}
