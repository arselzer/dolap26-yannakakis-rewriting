package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateResourceLimits
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlEnumerationBatchStatus

import PostgreSqlEnumerationSearchReplayV2TestSupport._

final class PostgreSqlEnumerationSearchReplayBundleV2AcquisitionSuite
    extends munit.FunSuite {

  test("found result keeps legacy package bytes and gains the exact v2 input") {
    val result = produce(width = 2, k = 2)
    val authoringRequest = authoring(result)
    val pair = PostgreSqlEnumerationSearchReplayBundleV2Acquisition
      .assemblePair(authoringRequest, result)
      .fold(problem => fail(problem.toString), identity)
    val legacy = PostgreSqlEnumerationCampaignV2Acquisition
      .assemble(authoringRequest, result)
      .fold(problem => fail(problem.toString), identity)

    assertEquals(pair.acquisitionStatus, "found-actions")
    assertEquals(
      pair.packageAssembly.document.bytes.toVector,
      legacy.document.bytes.toVector
    )
    assertEquals(
      pair.searchInput.bytes.toVector,
      PostgreSqlEnumerationSearchReplayV2Json
        .project(pair.eligible)
        .bytes
        .toVector
    )
  }

  test("real no-decomposition fallback yields a native-only package and v2 input") {
    val result = produce(width = 1, k = 3)
    val authoringRequest = authoring(result)
    val pair = PostgreSqlEnumerationSearchReplayBundleV2Acquisition
      .assemblePair(authoringRequest, result)
      .fold(problem => fail(problem.toString), identity)
    val assembly = ujson.read(pair.packageAssembly.document.utf8)
    val input = ujson.read(pair.searchInput.utf8)

    assertEquals(result.artifact, None)
    assertEquals(pair.acquisitionStatus, "typed-native-fallback")
    assertEquals(
      assembly("semantic_search_core")("status").str,
      "native-fallback"
    )
    assertEquals(
      assembly("semantic_search_core")("rejection")("code").str,
      "no-decomposition"
    )
    assertEquals(assembly("action_registry")("actions").arr.size, 1)
    assertEquals(input("schema_version").str, "yrw-enumeration-search-input/v2")
    assert(!pair.searchInput.utf8.contains("no-decomposition"))
  }

  test("v2 completion record distinguishes found and fallback without overclaiming") {
    Vector(
      produce(width = 2, k = 1) -> "found-actions",
      produce(width = 1, k = 3) -> "typed-native-fallback"
    ).foreach { case (result, expectedStatus) =>
      val authoringRequest = authoring(result)
      val pair = PostgreSqlEnumerationSearchReplayBundleV2Acquisition
        .assemblePair(authoringRequest, result)
        .fold(problem => fail(problem.toString), identity)
      val complete = PostgreSqlEnumerationSearchReplayBundleV2Json.build(
        authoringRequest,
        pair.eligible,
        PostgreSqlEnumerationSearchReplayV2AcquisitionCli.Entrypoint,
        PostgreSqlEnumerationSearchReplayBundleV1FileEvidence(7L, "a" * 64),
        PostgreSqlEnumerationSearchReplayBundleV1FileEvidence(
          pair.searchInput.byteLength.toLong,
          pair.searchInput.sha256
        ),
        pair.searchInput
      )
      val parsed = ujson.read(complete.utf8)
      assertEquals(parsed("acquisition_status").str, expectedStatus)
      assertEquals(parsed("claims_same_one_pass_result").bool, true)
      assertEquals(parsed("claims_cryptographic_same_result").bool, false)
      assertEquals(parsed("claims_complete_registered_denominator").bool, false)
      assertEquals(parsed("outcomes_in_scope").bool, false)
      assertEquals(parsed("reportable").bool, false)
      assertEquals(
        parsed("producer_entrypoint").str,
        PostgreSqlEnumerationSearchReplayV2AcquisitionCli.Entrypoint
      )
    }
  }

  test("invalid, resource-failed, and unsupported producer states stay rejected") {
    val tinyLimits = CandidateResourceLimits
      .create(maxBlocks = 1L)
      .fold(problems => fail(problems.mkString("; ")), identity)
    val cases = Vector(
      produce(sql = "   ", width = 2, k = 1),
      produce(width = 2, k = 1, candidateLimits = tinyLimits),
      produce(sql = UnsupportedSql, width = 2, k = 1)
    )
    assert(cases.exists(_.status == PostgreSqlEnumerationBatchStatus.InvalidQuery))
    assert(cases.exists(_.status == PostgreSqlEnumerationBatchStatus.Failed))
    assert(cases.exists(_.status == PostgreSqlEnumerationBatchStatus.NotApplicable))

    cases.foreach { result =>
      val failure = PostgreSqlEnumerationSearchReplayBundleV2Acquisition
        .assemblePair(authoring(result), result)
        .left
        .toOption
        .getOrElse(fail("ineligible producer state unexpectedly assembled"))
      assertEquals(
        failure.code,
        "enumeration-campaign-v2.acquisition.unpackageable-result"
      )
    }
  }
}
