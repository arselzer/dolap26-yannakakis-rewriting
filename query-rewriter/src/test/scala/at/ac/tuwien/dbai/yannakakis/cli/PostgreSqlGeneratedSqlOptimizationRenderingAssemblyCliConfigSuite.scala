package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.Paths

import munit.FunSuite

final class PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfigSuite
    extends FunSuite {
  private val cwd = Paths.get("/tmp/yrw-generated-sql-config").toAbsolutePath
  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
      "jdbc:postgresql://127.0.0.1/fixture",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "fixture-user",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "fixture-secret",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "fixture_schema"
  )
  private val validArguments = Array(
    "--query-file",
    "source.sql",
    "--output-assembly",
    "assembly.json",
    "--query-id",
    "win-c04-count",
    "--family",
    "global-count-star",
    "--cell-id",
    "win-c04-m5-rank1",
    "--dataset-id",
    "win-c04-m5",
    "--candidate-rank",
    "1",
    "--profile-ordinals",
    "1,2,3,4",
    "--plan-shape",
    "multi-bag"
  )

  test("configuration accepts the exact closed rendering surface") {
    val parsed =
      PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig
        .parse(validArguments, environment, cwd)
        .toOption
        .get
    assertEquals(parsed.queryFile, cwd.resolve("source.sql"))
    assertEquals(parsed.outputAssembly, cwd.resolve("assembly.json"))
    assertEquals(parsed.schemaName, "fixture_schema")
    assertEquals(parsed.candidateRank, 1)
    assertEquals(parsed.profileOrdinals, Vector(1, 2, 3, 4))
    assertEquals(parsed.family.factoryMode, "ordinary")
    assert(!parsed.family.usesC03G0V2Factory)
    assertEquals(parsed.planShape.id, "multi-bag")
    assertEquals(parsed.jdbc.password, "fixture-secret")
  }

  test("only C03 selects the opt-in catalog-verifying factory") {
    PostgreSqlGeneratedSqlOptimizationFamily.Values.foreach { family =>
      val expected =
        family == PostgreSqlGeneratedSqlOptimizationFamily.C03G0AggregateVectorV2
      assertEquals(family.usesC03G0V2Factory, expected, family.id)
      assertEquals(
        family.factoryMode,
        if (expected) "c03-g0-v2-opt-in" else "ordinary",
        family.id
      )
    }
  }

  test("rank and profile ordinals are exact canonical spellings") {
    def replace(option: String, value: String): Array[String] = {
      val copy = validArguments.clone()
      copy(copy.indexOf(option) + 1) = value
      copy
    }
    Vector("0", "01", "11", "999999999999999999999").foreach(value =>
      assert(
        PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig
          .parse(replace("--candidate-rank", value), environment, cwd)
          .isLeft,
        value
      )
    )
    Vector("2", "1,1", "1,3,2", "01", "1,9", "1, 2", "1,").foreach(
      value =>
        assert(
          PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig
            .parse(replace("--profile-ordinals", value), environment, cwd)
            .isLeft,
          value
        )
    )
  }

  test("configuration rejects missing secrets, unsafe IDs, aliases, and overrides") {
    val missingPassword = environment - PostgreSqlRewriteCliConfig.PasswordEnvironment
    assert(
      PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig
        .parse(validArguments, missingPassword, cwd)
        .isLeft
    )
    val unsafeId = validArguments.clone()
    unsafeId(unsafeId.indexOf("--cell-id") + 1) = "../private"
    assert(
      PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig
        .parse(unsafeId, environment, cwd)
        .isLeft
    )
    assert(
      PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig
        .parse(
          validArguments ++ Array("--reduction-mode", "none"),
          environment,
          cwd
        )
        .isLeft
    )
    val samePath = validArguments.clone()
    samePath(samePath.indexOf("--output-assembly") + 1) = "source.sql"
    assert(
      PostgreSqlGeneratedSqlOptimizationRenderingAssemblyCliConfig
        .parse(samePath, environment, cwd)
        .isLeft
    )
  }
}
