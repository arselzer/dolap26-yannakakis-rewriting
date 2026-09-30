package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.CandidatePlanSelection

class PostgreSqlCandidateSelectionCliConfigSuite extends munit.FunSuite {
  private val candidateId =
    "sha256:b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d"
  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
      "jdbc:postgresql://db.invalid/research",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "researcher",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "public"
  )

  test("CLI defaults to structural head and parses every exact selector") {
    assertEquals(
      parsed(Array("rewrite", "--stdin")).options.candidateSelection,
      CandidatePlanSelection.StructuralHead
    )
    assertEquals(
      parsed(
        Array(
          "rewrite",
          "--stdin",
          "--selection",
          "structural-head"
        )
      ).options.candidateSelection,
      CandidatePlanSelection.StructuralHead
    )
    assertEquals(
      parsed(
        Array(
          "rewrite",
          "--stdin",
          "--selection",
          "postgresql-static-shape-v1"
        )
      ).options.candidateSelection,
      CandidatePlanSelection.PostgreSqlStaticShapeV1
    )
    assertEquals(
      parsed(
        Array("rewrite", "--stdin", "--candidate-rank", "4")
      ).options.candidateSelection,
      CandidatePlanSelection.StructuralRank(4)
    )
    parsed(
      Array("rewrite", "--stdin", "--candidate-id", candidateId)
    ).options.candidateSelection match {
      case CandidatePlanSelection.ExactCandidateId(parsedId) =>
        assertEquals(parsedId.value, candidateId)
      case other => fail(s"expected exact candidate id, received $other")
    }
  }

  test("selection options are pairwise mutually exclusive and deterministic") {
    val selectors = Vector(
      Vector("--selection", "structural-head"),
      Vector("--candidate-rank", "2"),
      Vector("--candidate-id", candidateId)
    )
    for {
      left <- selectors.indices
      right <- 0 until left
    } {
      val supplied = selectors(left) ++ selectors(right)
      val failure = failed(
        (Vector("rewrite", "--stdin") ++ supplied).toArray
      )
      assert(failure.message.contains("mutually exclusive"), clues(supplied))
      assertEquals(
        failure.context("options").split(',').toVector,
        Vector(selectors(left).head, selectors(right).head).sorted
      )
    }

    val duplicate = failed(
      Array(
        "rewrite",
        "--stdin",
        "--candidate-rank",
        "1",
        "--candidate-rank",
        "2"
      )
    )
    assertEquals(duplicate.context("option"), "--candidate-rank")
  }

  test("selector validation precedes unrelated database environment checks") {
    val conflict = PostgreSqlRewriteCliConfig.parse(
      Array(
        "rewrite",
        "--stdin",
        "--selection",
        "structural-head",
        "--candidate-rank",
        "2"
      ),
      Map.empty
    ) match {
      case Left(problem) => problem
      case other         => fail(s"expected selector conflict, received $other")
    }
    assert(conflict.message.contains("mutually exclusive"))
    assertEquals(
      conflict.context("options"),
      "--candidate-rank,--selection"
    )

    val outsideTopK = PostgreSqlRewriteCliConfig.parse(
      Array(
        "rewrite",
        "--stdin",
        "--plan-limit",
        "3",
        "--candidate-rank",
        "4"
      ),
      Map.empty
    ) match {
      case Left(problem) => problem
      case other         => fail(s"expected rank/top-k failure, received $other")
    }
    assertEquals(outsideTopK.context("options"), "candidateSelection")
  }

  test("rank parsing and the retained top-k envelope fail at the CLI boundary") {
    Vector("zero" -> "0", "negative" -> "-1", "malformed" -> "many")
      .foreach { case (label, value) =>
        val failure = failed(
          Array("rewrite", "--stdin", "--candidate-rank", value)
        )
        assertEquals(failure.context("option"), "--candidate-rank", label)
      }

    val outsideTopK = failed(
      Array(
        "rewrite",
        "--stdin",
        "--plan-limit",
        "3",
        "--candidate-rank",
        "4"
      )
    )
    assertEquals(outsideTopK.context("options"), "candidateSelection")
  }

  test("candidate ids and named policies are strict and are never echoed") {
    val invalid = Vector(
      candidateId.toUpperCase,
      candidateId.dropRight(1),
      candidateId.updated(candidateId.length - 1, 'g'),
      "not-a-candidate-id"
    )
    invalid.foreach { value =>
      val failure = failed(
        Array("rewrite", "--stdin", "--candidate-id", value)
      )
      assertEquals(failure.context("option"), "--candidate-id")
      assert(!failure.toString.contains(value))
    }

    val unsupported = "postgresql-cost-selected"
    val failure = failed(
      Array("rewrite", "--stdin", "--selection", unsupported)
    )
    assertEquals(failure.context("option"), "--selection")
    assert(!failure.toString.contains(unsupported))
  }

  test("help names physical shape honestly and exposes no cost-selection claim") {
    val usage = PostgreSqlRewriteCliConfig.Usage
    assert(usage.contains("--candidate-rank N"))
    assert(usage.contains("--candidate-id ID"))
    assert(usage.contains("postgresql-static-shape-v1"))
    assert(usage.contains("--reduction-schedule M"))
    assert(usage.contains("full (default), bottom-up, or none"))
    assert(!usage.toLowerCase.contains("cost-selected"))
    assert(!usage.toLowerCase.contains("lowest cost"))
  }

  private def parsed(arguments: Array[String]): PostgreSqlRewriteCliConfig =
    PostgreSqlRewriteCliConfig.parse(arguments, environment) match {
      case Right(PostgreSqlRewriteCliCommand.Rewrite(config)) => config
      case other => fail(s"expected rewrite config, received $other")
    }

  private def failed(arguments: Array[String]): PostgreSqlRewriteCliFailure =
    PostgreSqlRewriteCliConfig.parse(arguments, environment) match {
      case Left(problem) => problem
      case other         => fail(s"expected invocation failure, received $other")
    }
}
