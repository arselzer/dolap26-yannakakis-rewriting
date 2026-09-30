package at.ac.tuwien.dbai.yannakakis.cli

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.api.RewriteRequest
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteResult
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

/** End-to-end process-contract smoke for bounded distributed grouped COUNT. */
class PostgreSqlDistributedGroupedCountCliJsonContractSuite
    extends munit.FunSuite {
  private val schemaDirectory = "docs/journal-extension/schemas"

  private val sql =
    "SELECT e0.left_key AS \"First\", e2.left_key AS \"Second\", " +
      "COUNT(*) AS \"Rows\" " +
      "FROM cli_dist_e0 AS e0 " +
      "JOIN cli_dist_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN cli_dist_e2 AS e2 ON e1.right_key = e2.left_key " +
      "JOIN cli_dist_e3 AS e3 ON e2.right_key = e3.left_key " +
      "AND e3.right_key = e0.left_key " +
      "GROUP BY e0.left_key, e2.left_key\n"

  private val environment = Map(
    PostgreSqlRewriteCliConfig.JdbcUrlEnvironment ->
      "jdbc:postgresql://localhost/unused",
    PostgreSqlRewriteCliConfig.UserEnvironment -> "unused",
    PostgreSqlRewriteCliConfig.PasswordEnvironment -> "unused",
    PostgreSqlRewriteCliConfig.SchemaEnvironment -> "public"
  )

  private val backend = {
    val schema = Frameworks.createRootSchema(true)
    (0 until 4).foreach { index =>
      schema.add(
        s"cli_dist_e$index",
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
    }
    val planner = CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)

    new PostgreSqlRewriteBackend {
      override def rewrite(
          config: PostgreSqlRewriteCliConfig,
          request: RewriteRequest
      ): Either[PostgreSqlRewriteCliFailure, PostgreSqlRewriteResult] =
        Right(rewriter.rewriteDetailed(request))
    }
  }

  test("U1 CLI success is deterministic strict v4 JSON with frozen schemas") {
    val arguments = Array(
      "rewrite",
      "--stdin",
      "--schema",
      "public",
      "--width-bound",
      "2",
      "--plan-limit",
      "4",
      "--reduction-schedule",
      "full",
      "--selection",
      "structural-head"
    )
    val first = invoke(arguments)
    val repeated = invoke(arguments)

    assertEquals(first.exitCode, PostgreSqlRewriteExitCode.Rewritten)
    assertEquals(repeated, first)
    assert(first.output.endsWith("\n"), clues(first.output))
    val bytes = first.output.getBytes(StandardCharsets.UTF_8)
    assertEquals(
      sha256(bytes),
      "db7d85a09342e857df3e7a4517079c0198e1655d2ce0f9bcc1793d4659450b84"
    )
    val compact = first.output.dropRight(1)
    assert(!compact.contains('\n'), clues(compact))
    assert(!compact.contains('\r'), clues(compact))

    val document = ujson.read(compact)
    assertEquals(
      document("schema_version").str,
      "yannakakis-postgresql-rewrite/v4"
    )
    assertEquals(document("status").str, "rewritten")
    assertEquals(document("exit_code").num.toInt, 0)
    assertEquals(document("request")("original_sql").str, sql)
    assertEquals(
      document("request")("options")("reduction_schedule").str,
      "full"
    )
    assertEquals(
      document("report")("analysis")("output_column_count").num.toInt,
      3
    )
    assertEquals(
      document("report")("candidate_selection")("policy").str,
      "structural_top_k_head_v1"
    )
    assertEquals(
      document("report")("rendering")("output_column_count").num.toInt,
      3
    )
    assertEquals(
      document("report")("rendering")("reduction_schedule").str,
      "full"
    )
    val rewritten = document("outcome")("rewritten_statements").arr
    assertEquals(rewritten.size, 1)
    assert(rewritten.head.str.contains("CAST(COALESCE(SUM("))
    assert(rewritten.head.str.contains("AS BIGINT) AS \"Rows\""))

    val v4Schema = ujson.read(Files.readString(repositoryFile(
      s"$schemaDirectory/postgresql-rewrite-artifact-v4.schema.json"
    )))
    val violations = TestJsonSchemaValidator.validate(document, v4Schema)
    assert(
      violations.isEmpty,
      violations.map(value => s"${value.path}: ${value.message}").mkString("\n")
    )

    val expectedSchemas = Vector(
      1 -> "cb490fd238d0a6044e5fe03f403a9a1e5257adddd76cee9635f53163662d3283",
      2 -> "b0bbff746dc349182f3fd55ae8d87acaf6bd1157c736aea89b2eabbb837f2adb",
      3 -> "c87261872c356391365c92955cf43480074fd5a4a77c9309e10ad84bcf21fdd0",
      4 -> "8bb50a5d0458797695207972d35b08c171006b8e0ce6e763f5fbdb3f439d0f8b",
      5 -> "edf059d678c1644b60beeb74e77018036211dbf2b2b45beafdeced72ad0e8ee8",
      6 -> "ae3df2a46439129c74b41b965198d63737a29f12150b48634a03c592ad88f0ae",
      7 -> "98ed10cdd27c92809d902cbaf9e904685172e8987bf3d58f4ffbdbe08ea1ff28"
    )
    expectedSchemas.foreach { case (version, expected) =>
      val schemaBytes = Files.readAllBytes(repositoryFile(
        s"$schemaDirectory/postgresql-rewrite-artifact-v$version.schema.json"
      ))
      assertEquals(sha256(schemaBytes), expected, clues(version))
    }
  }

  private def invoke(arguments: Array[String]): Invocation = {
    val output = new ByteArrayOutputStream
    val exitCode = PostgreSqlRewriteCli.run(
      arguments,
      environment,
      new ByteArrayInputStream(sql.getBytes(StandardCharsets.UTF_8)),
      output,
      PassingProbe,
      backend
    )
    Invocation(exitCode, output.toString(StandardCharsets.UTF_8.name()))
  }

  private case class Invocation(exitCode: Int, output: String)

  private object PassingProbe extends PostgreSqlConnectivityProbe {
    override def verify(
        jdbc: JdbcEnvironment,
        schemaName: String
    ): Either[PostgreSqlRewriteCliFailure, Unit] = Right(())
  }

  private def repositoryFile(relative: String): Path = {
    var current = Paths.get("").toAbsolutePath.normalize()
    while (current != null) {
      val candidate = current.resolve(relative)
      if (Files.isRegularFile(candidate)) return candidate
      current = current.getParent
    }
    fail(s"could not locate repository file $relative")
  }

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
