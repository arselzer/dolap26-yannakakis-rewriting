package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

class PostgreSqlRewriteSchemaContractSuite extends munit.FunSuite {
  private val schemaDirectory =
    "docs/journal-extension/schemas"

  test("the published v1 schema remains byte-frozen") {
    val path = repositoryFile(
      s"$schemaDirectory/postgresql-rewrite-artifact-v1.schema.json"
    )
    assertEquals(
      sha256(Files.readAllBytes(path)),
      "cb490fd238d0a6044e5fe03f403a9a1e5257adddd76cee9635f53163662d3283"
    )
    val schema = ujson.read(Files.readString(path))
    assertEquals(
      schema("properties")("schema_version")("const").str,
      "yannakakis-postgresql-rewrite/v1"
    )
    assertEquals(
      schema("$defs")("selectedPlan")("properties")("rank")("const").num.toInt,
      1
    )
  }

  test("v2 schema has a deterministic physical-selection contract") {
    val path = repositoryFile(
      s"$schemaDirectory/postgresql-rewrite-artifact-v2.schema.json"
    )
    val bytes = Files.readAllBytes(path)
    assertEquals(
      sha256(bytes),
      "b0bbff746dc349182f3fd55ae8d87acaf6bd1157c736aea89b2eabbb837f2adb"
    )
    val schema = ujson.read(new String(bytes, java.nio.charset.StandardCharsets.UTF_8))
    assertEquals(
      schema("properties")("schema_version")("const").str,
      "yannakakis-postgresql-rewrite/v2"
    )
    assertEquals(
      schema("$defs")("candidateIdScheme")("const").str,
      "covered_decomposition_json_v1"
    )
    assertEquals(
      schema("$defs")("selectionPolicy")("enum").arr.map(_.str).toVector,
      Vector(
        "structural_top_k_head_v1",
        "explicit_structural_rank_v1",
        "explicit_candidate_id_v1",
        "postgresql_static_shape_lexicographic_v1"
      )
    )
    assertEquals(
      schema("$defs")("candidateSelection")("properties")("score_order")(
        "const"
      ).arr.map(_.str).toVector,
      Vector(
        "expanded_scan_count",
        "materialized_distinct_support_count",
        "cte_count",
        "rendered_term_count",
        "sql_character_count",
        "structural_rank"
      )
    )
    assertEquals(
      schema("$defs")("selectedPlan")("properties")("rank")("$ref").str,
      "#/$defs/positive"
    )
    assert(
      schema("$defs")("report")("required").arr
        .map(_.str)
        .contains("candidate_selection")
    )
  }

  test("v3 schema is frozen and strictly declares reduction provenance") {
    val path = repositoryFile(
      s"$schemaDirectory/postgresql-rewrite-artifact-v3.schema.json"
    )
    val bytes = Files.readAllBytes(path)
    assertEquals(
      sha256(bytes),
      "c87261872c356391365c92955cf43480074fd5a4a77c9309e10ad84bcf21fdd0"
    )
    val schema = ujson.read(
      new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
    )
    assertEquals(
      schema("properties")("schema_version")("const").str,
      "yannakakis-postgresql-rewrite/v3"
    )
    assertEquals(
      schema("$defs")("reductionMode")("enum").arr.map(_.str).toVector,
      Vector("full", "bottom-up", "none")
    )
    assertEquals(
      schema("$defs")("reductionOrder")("const").str,
      "canonical_plan_schedule_v1"
    )
    assertEquals(
      schema("$defs")("physicalVariantIdScheme")("const").str,
      "postgresql_candidate_reduction_variant_v1"
    )
    Vector("candidateSelection", "candidateEvaluation", "selectedPlan", "rendering")
      .foreach { definition =>
        assertEquals(
          schema("$defs")(definition)("additionalProperties").bool,
          false,
          clues(definition)
        )
      }
  }

  test("v4 schema is frozen and strictly declares guarded EXPLAIN evidence") {
    val path = repositoryFile(
      s"$schemaDirectory/postgresql-rewrite-artifact-v4.schema.json"
    )
    val bytes = Files.readAllBytes(path)
    assertEquals(
      sha256(bytes),
      "8bb50a5d0458797695207972d35b08c171006b8e0ce6e763f5fbdb3f439d0f8b"
    )
    val schema = ujson.read(
      new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
    )
    assertEquals(
      schema("properties")("schema_version")("const").str,
      "yannakakis-postgresql-rewrite/v4"
    )
    assert(
      schema("$defs")("selectionPolicy")("enum").arr.map(_.str).contains(
        "postgresql_explain_row_guarded_total_cost_v1"
      )
    )
    assertEquals(
      schema("$defs")("optimizerEstimate")("properties")("analyze")("const").bool,
      false
    )
    assertEquals(
      schema("$defs")("optimizerEstimate")("properties")("runtime_measured")("const").bool,
      false
    )
    assertEquals(
      schema("$defs")("rowGuard")("properties")("factor")("const").num.toInt,
      10
    )
    val rendered = new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
    assert(rendered.contains("\"contains\""))
    assert(rendered.contains("\"no_admissible_estimate\""))
    assert(rendered.contains("\"explain_evaluation_failed\""))
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
