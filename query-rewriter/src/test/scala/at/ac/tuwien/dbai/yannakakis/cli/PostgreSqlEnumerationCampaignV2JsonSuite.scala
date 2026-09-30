package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.charset.StandardCharsets

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchArtifact,
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationCampaignV2ProjectionView,
  PostgreSqlQueryRewriter
}
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

class PostgreSqlEnumerationCampaignV2JsonSuite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  private val authoringHash = "a" * 64
  private val rankOneIdentity =
    "b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d"

  test("source and action SQL helpers match fixed canonical UTF-8 byte goldens") {
    val sql = "SELECT 'é'\n"
    val source = PostgreSqlEnumerationCampaignV2Json.sourceSqlDocument(sql)
    val action = PostgreSqlEnumerationCampaignV2Json.actionSqlDocument(sql)

    assertEquals(
      source.utf8,
      "{" +
        "\"byte_length\":12," +
        "\"encoding\":\"utf-8\"," +
        "\"schema_version\":\"yrw-enumeration-source-sql/v2\"," +
        "\"sha256\":\"8947c71f2b8bcdb340fdb30d282c9433eb432588f0185d16ad88fea7389b338c\"," +
        "\"utf8_hex\":\"53454c4543542027c3a9270a\"}\n"
    )
    assertEquals(
      action.utf8,
      "{" +
        "\"byte_length\":12," +
        "\"encoding\":\"utf-8\"," +
        "\"hash_scheme\":\"sha256_utf8_v1\"," +
        "\"schema_version\":\"yrw-enumeration-action-sql/v2\"," +
        "\"sha256\":\"8947c71f2b8bcdb340fdb30d282c9433eb432588f0185d16ad88fea7389b338c\"," +
        "\"utf8_hex\":\"53454c4543542027c3a9270a\"}\n"
    )
    assertEquals(new String(source.bytes, StandardCharsets.UTF_8), source.utf8)
    assertEquals(source.utf8.count(_ == '\n'), 1)
    assertEquals(action.utf8.count(_ == '\n'), 1)
  }

  test("canonical JSON sorts keys and renders arbitrary BigInt as an exact numeral") {
    import PostgreSqlEnumerationCampaignV2CanonicalJson._
    val huge = BigInt(2).pow(256) + 17
    val rendered = document(
      obj(
        "z" -> bool(value = true),
        "a" -> number(huge)
      )
    )

    assertEquals(
      rendered.utf8,
      s"{\"a\":$huge,\"z\":true}\n"
    )
    assert(!rendered.utf8.contains(s"\"$huge\""))
  }

  test("bounded canonical writer enforces the exact escaped UTF-8 boundary") {
    import PostgreSqlEnumerationCampaignV2CanonicalJson._
    val value = string("é\"\n😀")

    val exact = documentWithMaximumBytes(value, maximumBytes = 13)
    assertEquals(exact.utf8, "\"é\\\"\\n😀\"\n")
    assertEquals(exact.bytes.length, 13)

    val callerCopy = exact.bytes
    callerCopy(0) = 'x'.toByte
    assertEquals(exact.utf8, "\"é\\\"\\n😀\"\n")

    val failure = intercept[IllegalArgumentException] {
      documentWithMaximumBytes(value, maximumBytes = 12)
    }
    assert(
      failure.getMessage.contains(
        "canonical JSON document exceeds the campaign-v2 transport envelope"
      )
    )
  }

  test("bounded canonical writer accounts for aggregate ordered-stage hex expansion") {
    import PostgreSqlEnumerationCampaignV2CanonicalJson._
    val sql = "SELECT 'é'\n"
    val sqlByteLength = sql.getBytes(StandardCharsets.UTF_8).length.toLong
    val stages = array(
      (1 to 3).toVector.map { ordinal =>
        obj(
          "ordinal" -> number(BigInt(ordinal)),
          "stage_id" -> string(s"factor:b0:v$ordinal"),
          "statement" -> obj(
            "byte_length" -> number(BigInt(sqlByteLength)),
            "encoding" -> string("utf-8"),
            "hash_scheme" -> string("sha256_utf8_v1"),
            "schema_version" -> string(
              "yrw-enumeration-action-sql/v2"
            ),
            "sha256" -> string("a" * 64),
            "utf8_hex" -> utf8HexString(sql, sqlByteLength)
          )
        )
      }
    )

    val rendered = document(stages)
    val exactSize = rendered.bytes.length
    assertEquals(
      documentWithMaximumBytes(stages, exactSize),
      rendered
    )
    intercept[IllegalArgumentException] {
      documentWithMaximumBytes(stages, exactSize - 1)
    }
    val encodedSql = "53454c4543542027c3a9270a"
    assertEquals(
      rendered.utf8.sliding(encodedSql.length).count(_ == encodedSql),
      3
    )
    intercept[IllegalArgumentException] {
      utf8HexString(sql, sqlByteLength - 1L)
    }
  }

  test("produced batch projects a fixed semantic core and registry-aligned bundles") {
    val artifact = produce(planLimit = 1)
    val left = PostgreSqlEnumerationCampaignV2Json.projectWithUnboundAuthoringDigestForContractTest(
      artifact,
      authoringHash
    )
    val right = PostgreSqlEnumerationCampaignV2Json.projectWithUnboundAuthoringDigestForContractTest(
      artifact,
      authoringHash
    )

    assertEquals(left, right)
    assertEquals(
      left.semanticSearchCore.utf8,
      "{" +
        s"\"authoring_request_sha256\":\"$authoringHash\"," +
        "\"candidate_id_scheme\":\"covered_decomposition_json_v1\"," +
        "\"candidates\":[{" +
        s"\"candidate_id\":\"sha256:$rankOneIdentity\"," +
        s"\"decomposition_sha256\":\"$rankOneIdentity\"," +
        s"\"identity_sha256\":\"$rankOneIdentity\"," +
        "\"rank\":1}]," +
        "\"decomposition_hash_scheme\":\"covered_decomposition_json_v1\"," +
        "\"exhausted_before_k\":false," +
        "\"protocol_version\":\"yrw-enumeration-campaign-protocol/v2\"," +
        "\"rejection\":null," +
        "\"requested_k\":1," +
        "\"schema_version\":\"yrw-enumeration-semantic-search-core/v2\"," +
        "\"status\":\"found\"," +
        "\"termination\":\"reached-k\"}\n"
    )
    assertEquals(
      left.producerSearchContract.utf8,
      "{" +
        s"\"authoring_request_sha256\":\"$authoringHash\"," +
        "\"limits\":{" +
        "\"bag_factor\":{" +
        "\"max_bags\":250000," +
        "\"max_ownership_checks\":10000000," +
        "\"max_source_edges\":250000," +
        "\"max_tree_edges\":250000}," +
        "\"candidate\":{" +
        "\"max_basis_alternatives\":1000000," +
        "\"max_basis_checks\":10000000," +
        "\"max_blocks\":250000," +
        "\"max_combination_states\":5000000," +
        "\"max_plan_limit\":100," +
        "\"max_retained_partial_plans\":1000000," +
        "\"max_witness_nodes\":250000}," +
        "\"rendering\":{" +
        "\"character_count_scheme\":\"jvm_utf16_code_units_v1\"," +
        "\"max_ctes\":1000000," +
        "\"max_expanded_scans\":1000000," +
        "\"max_rendered_terms\":10000000," +
        "\"max_sql_characters\":16777216}," +
        "\"soft\":{" +
        "\"max_candidate_cover_checks\":10000000," +
        "\"max_candidates\":1000000," +
        "\"max_components\":1000000," +
        "\"max_cover_pairs\":100000000," +
        "\"max_covers\":100000," +
        "\"max_derivations\":5000000}," +
        "\"stage_diagnostics\":{" +
        "\"character_count_scheme\":\"jvm_utf16_code_units_v1\"," +
        "\"max_characters_per_statement\":16777216," +
        "\"max_statements\":256," +
        "\"max_total_characters\":67108864}," +
        "\"transport\":{" +
        "\"encoding\":\"utf-8\"," +
        "\"max_canonical_json_document_bytes\":67108864," +
        "\"max_generated_or_stage_sql_bytes\":16777216," +
        "\"max_source_sql_bytes\":16777216}}," +
        "\"protocol_version\":\"yrw-enumeration-campaign-protocol/v2\"," +
        "\"schema_version\":\"yrw-enumeration-producer-search-contract/v2\"," +
        "\"search\":{" +
        "\"algorithm\":\"top-k-candidate-td-v1\"," +
        "\"cover_distinct_assignments_enumerated\":false," +
        "\"exhaustion_scope\":\"structural-tree-witnesses-only-v1\"," +
        "\"physical_cover_policy\":\"canonical-first-admissible-cover-per-bag-v1\"," +
        "\"ranking\":\"node-count-then-canonical-rooted-tree-signature-v1\"," +
        "\"require_connected_cover\":true," +
        "\"structural_universe\":\"structural-tree-witnesses-v1\"," +
        "\"width_bound\":2}," +
        s"\"source_sql_sha256\":\"${artifact.request.originalSqlSha256}\"}\n"
    )
    val transcript = ujson.read(left.producerSearchTranscript.utf8)
    assertEquals(transcript("completed_phase").str, "rendered")
    assertEquals(transcript("candidate_search")("state").str, "found")
    assertEquals(transcript("soft_generation")("state").str, "completed")
    assertEquals(transcript("compiled_candidates").arr.size, 1)
    assertEquals(transcript("compiled_candidates")(0)("rank").num.toInt, 1)
    assertEquals(
      transcript("compiled_candidates")(0)("candidate_id").str,
      s"sha256:$rankOneIdentity"
    )
    assertEquals(
      transcript("semantic_search_core_sha256").str,
      left.semanticSearchCore.sha256
    )
    assertEquals(
      transcript("producer_search_contract_sha256").str,
      left.producerSearchContract.sha256
    )
    assertEquals(
      left.producerSearchTranscript.sha256,
      "1d098dec1df40475522cc1d3a29f633d982e891502960a779029c826b4591c36"
    )
    assertEquals(
      left.producerSearchTranscript.utf8,
      "{" +
        "\"candidate_search\":{" +
        "\"state\":\"found\"," +
        "\"statistics\":{" +
        "\"basis_alternative_count\":11," +
        "\"basis_check_count\":25," +
        "\"block_count\":9," +
        "\"candidate_bag_count\":5," +
        "\"combination_state_count\":11," +
        "\"maximum_witness_node_count\":1," +
        "\"retained_partial_plan_count\":5," +
        "\"satisfied_block_count\":9}}," +
        "\"compiled_candidates\":[{" +
        "\"bag_factor_counters\":{" +
        "\"bag_count\":1," +
        "\"ownership_check_count\":6," +
        "\"source_edge_count\":3," +
        "\"tree_edge_count\":0}," +
        s"\"candidate_id\":\"sha256:$rankOneIdentity\"," +
        "\"rank\":1," +
        "\"rendering_counters\":{" +
        "\"cte_count\":1," +
        "\"expanded_scan_count\":3," +
        "\"rendered_term_count\":21," +
        "\"sql_character_count\":1070}," +
        "\"stage_diagnostic_counters\":{" +
        "\"maximum_statement_character_count\":1070," +
        "\"statement_count\":2," +
        "\"total_statement_character_count\":1879}}]," +
        "\"completed_phase\":\"rendered\"," +
        "\"producer_search_contract_sha256\":" +
        "\"545f4a4141b7cc0f4d7c59a30d9358200b73db1c05f390b313922094e6b7e945\"," +
        "\"protocol_version\":\"yrw-enumeration-campaign-protocol/v2\"," +
        "\"schema_version\":\"yrw-enumeration-producer-search-transcript/v2\"," +
        "\"semantic_search_core_sha256\":" +
        "\"a8fb85465356d0291d33829ddb9b69ad5501a7e9e7618549ee3b303f73eb5fa4\"," +
        "\"soft_generation\":{" +
        "\"counters\":{" +
        "\"admissible_candidate_bag_count\":5," +
        "\"admissible_candidate_cover_alternative_count\":21," +
        "\"candidate_cover_check_count\":30," +
        "\"component_count\":4," +
        "\"cover_count\":7," +
        "\"cover_pair_count\":49," +
        "\"derivation_count\":28," +
        "\"generated_candidate_bag_count\":5," +
        "\"generated_candidate_cover_alternative_count\":21}," +
        "\"state\":\"completed\"}}\n"
    )
    assertEquals(left.producerSearchContract.utf8.count(_ == '\n'), 1)
    assertEquals(left.producerSearchTranscript.utf8.count(_ == '\n'), 1)
    assert(!left.producerSearchContract.utf8.contains("outcome"))
    assert(!left.producerSearchTranscript.utf8.contains("outcome"))
    assertEquals(
      left.featureRegistry.sha256,
      "d945eb526d7d29e2ebba1cd1038f7f737d9834957224a204d997395f9b5f208e"
    )
    assertEquals(
      left.featureValueBundles.map(bundle => (bundle.actionId, bundle.actionKind)),
      Vector(("native", "native"), ("rewrite-0001", "retained_candidate"))
    )
    assertEquals(left.actions.map(_.actionId), Vector("native", "rewrite-0001"))
    assert(left.featureValueBundles.forall(_.document.utf8.count(_ == '\n') == 1))
    assert(
      left.featureValueBundles.forall(
        _.document.utf8.contains(
          s"\"feature_registry_sha256\":\"${left.featureRegistry.sha256}\""
        )
      )
    )
    assert(!left.featureValueBundles.exists(_.document.utf8.contains("\"rank\"")))
    assert(!left.featureValueBundles.exists(_.document.utf8.contains("outcome")))
    assert(left.featureValueBundles.head.document.utf8.contains("\"value\":1"))
    assert(!left.featureValueBundles.head.document.utf8.contains("\"value\":\"1\""))
    assert(
      left.featureValueBundles.head.document.utf8.contains(
        "\"reason_code\":\"not-applicable-by-registry\""
      )
    )
  }

  test("package bridge preserves every source, generated, and ordered stage byte") {
    val artifact = produce(planLimit = 10)
    val view = PostgreSqlEnumerationCampaignV2ProjectionView.from(artifact)
    val projected = PostgreSqlEnumerationCampaignV2Json.projectWithUnboundAuthoringDigestForContractTest(
      artifact,
      authoringHash
    )

    assertEquals(
      projected.sourceSql,
      PostgreSqlEnumerationCampaignV2Json.sourceSqlDocument(view.sourceSql)
    )
    assertEquals(projected.actions.size, view.candidates.size + 1)
    assertEquals(
      projected.canonicalDecompositions.size,
      view.candidates.size
    )
    assertEquals(
      projected.actions.head.renderedSql,
      PostgreSqlEnumerationCampaignV2Json.actionSqlDocument(view.sourceSql)
    )
    assertEquals(projected.actions.head.orderedStages.utf8, "[]\n")

    view.candidates
      .zip(projected.actions.tail)
      .zip(projected.canonicalDecompositions)
      .foreach {
      case ((candidate, action), decompositionDocument) =>
        assertEquals(action.candidateRank, Some(candidate.structuralRank))
        assertEquals(action.candidateId, Some(candidate.candidateId))
        assertEquals(action.physicalVariantId, Some(candidate.physicalVariantId))
        assertEquals(
          action.renderedSql,
          PostgreSqlEnumerationCampaignV2Json.actionSqlDocument(
            candidate.renderedSql
          )
        )
        val expectedStages = candidate.orderedStages.zipWithIndex.map {
          case (stage, index) =>
            val statement = PostgreSqlEnumerationCampaignV2Json
              .actionSqlDocument(stage.sql)
              .utf8
              .stripSuffix("\n")
            s"{\"ordinal\":${index + 1},\"stage_id\":\"${stage.stageId}\",\"statement\":$statement}"
        }.mkString("[", ",", "]\n")
        assertEquals(action.orderedStages.utf8, expectedStages)
        assertEquals(candidate.orderedStages.last.sql, candidate.renderedSql)
        val decomposition = ujson.read(decompositionDocument.utf8)
        assertEquals(decomposition("candidate_rank").num.toInt, candidate.structuralRank)
        assertEquals(decomposition("candidate_id").str, candidate.candidateId)
        assertEquals(
          decomposition("canonicalization_scheme").str,
          "covered_decomposition_json_v1"
        )
        assertEquals(
          decomposition("utf8_hex").str,
          candidate.canonicalDecompositionJson
            .getBytes(StandardCharsets.UTF_8)
            .map(byte => f"${byte & 0xff}%02x")
            .mkString
        )
      }
    }

  test("projection rejects noncanonical hashes and malformed SQL strings") {
    val artifact = produce(planLimit = 1)
    Vector("a" * 63, "A" * 64, ("a" * 64) + " ").foreach { hostile =>
      intercept[IllegalArgumentException] {
        PostgreSqlEnumerationCampaignV2Json
          .projectWithUnboundAuthoringDigestForContractTest(artifact, hostile)
      }
    }
    intercept[IllegalArgumentException] {
      PostgreSqlEnumerationCampaignV2Json.sourceSqlDocument("SELECT '\u0000'")
    }
    intercept[IllegalArgumentException] {
      PostgreSqlEnumerationCampaignV2Json.actionSqlDocument(
        new String(Array('\udc00'))
      )
    }
  }

  private def produce(planLimit: Int): PostgreSqlEnumerationBatchArtifact = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
    val request = PostgreSqlEnumerationBatchRequest
      .create(triangleSql, planLimit = planLimit)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    rewriter
      .produceEnumerationBatchLegacyV2(request)
      .artifact
      .getOrElse(fail("enumeration batch did not produce an artifact"))
  }
}
