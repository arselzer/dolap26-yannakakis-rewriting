package at.ac.tuwien.dbai.yannakakis.cli

import PostgreSqlEnumerationSearchReplayV2TestSupport._

final class PostgreSqlEnumerationSearchReplayV2JsonSuite
    extends munit.FunSuite {

  test("found projection preserves every v1 pre-soft value under v2 framing") {
    val result = produce(width = 2, k = 4)
    val eligible = classify(result)
    val artifact = result.artifact.getOrElse(fail("expected found artifact"))
    val v1 = ujson.read(
      PostgreSqlEnumerationSearchReplayV1Json.project(artifact).utf8
    )
    val v2Document = PostgreSqlEnumerationSearchReplayV2Json.project(eligible)
    val v2 = ujson.read(v2Document.utf8)

    Vector(
      "candidate_limits",
      "claims_independent_sql_frontend",
      "edges",
      "requested_k",
      "require_connected_cover",
      "soft_limits",
      "source_sql_sha256",
      "width_bound"
    ).foreach(field => assertEquals(v2(field), v1(field), field))
    assertEquals(
      v2("schema_version").str,
      PostgreSqlEnumerationSearchReplayV2Json.SchemaVersion
    )
    assertEquals(
      v2("protocol_version").str,
      PostgreSqlEnumerationSearchReplayV2Json.ProtocolVersion
    )
    assertEquals(
      v2("capture_origin").str,
      "scala-one-pass-enumeration-eligible-result-v2"
    )
    assertEquals(v2Document.bytes.last, '\n'.toByte)
  }

  test("real width-one triangle fallback projects the same output-free boundary") {
    val result = produce(width = 1, k = 3)
    val eligible = classify(result)
    val fallback = eligible match {
      case value: PostgreSqlEnumerationCampaignV2EligibleResult.NativeFallback =>
        value
      case _ => fail("width-one triangle did not classify as native fallback")
    }
    assertEquals(fallback.reason.code, "no-decomposition")

    val root = ujson.read(
      PostgreSqlEnumerationSearchReplayV2Json.project(eligible).utf8
    ).obj
    assertEquals(
      root.keySet.toSet,
      Set(
        "candidate_limits",
        "capture_origin",
        "claims_independent_sql_frontend",
        "edges",
        "protocol_version",
        "requested_k",
        "require_connected_cover",
        "schema_version",
        "search_input_seal_sha256",
        "soft_limits",
        "source_sql_sha256",
        "width_bound"
      )
    )
    assertEquals(root("requested_k").num.toInt, 3)
    assertEquals(root("width_bound").num.toInt, 1)
    assertEquals(root("edges").arr.size, 3)
  }

  test("fallback capture serializes no search output or terminal reason") {
    val root = ujson.read(
      PostgreSqlEnumerationSearchReplayV2Json.project(
        classify(produce(width = 1, k = 3))
      ).utf8
    )
    val forbiddenKeys = Set(
      "candidate_id",
      "candidate_rank",
      "candidates",
      "candidate_search",
      "decomposition",
      "exhausted_before_k",
      "fallback",
      "outcome",
      "rank",
      "reason",
      "soft_counters",
      "soft_generation",
      "status",
      "termination"
    )
    assertEquals(allKeys(root).intersect(forbiddenKeys), Set.empty[String])
    assert(!root.render().contains("no-decomposition"))
    assert(!root.render().contains("native-fallback"))
  }

  test("independent fallback invocations yield deterministic v2 bytes") {
    val first = PostgreSqlEnumerationSearchReplayV2Json.project(
      classify(produce(width = 1, k = 3))
    )
    val second = PostgreSqlEnumerationSearchReplayV2Json.project(
      classify(produce(width = 1, k = 3))
    )
    assertEquals(first.bytes.toVector, second.bytes.toVector)
    assertEquals(first.sha256, second.sha256)
  }

  test("null eligible values fail before serialization") {
    intercept[IllegalArgumentException] {
      PostgreSqlEnumerationSearchReplayV2Json.project(null)
    }
  }

  private def allKeys(value: ujson.Value): Set[String] = value match {
    case objectValue: ujson.Obj =>
      objectValue.obj.iterator.flatMap { case (key, child) =>
        Iterator.single(key) ++ allKeys(child)
      }.toSet
    case arrayValue: ujson.Arr =>
      arrayValue.arr.iterator.flatMap(allKeys).toSet
    case _ => Set.empty
  }
}
