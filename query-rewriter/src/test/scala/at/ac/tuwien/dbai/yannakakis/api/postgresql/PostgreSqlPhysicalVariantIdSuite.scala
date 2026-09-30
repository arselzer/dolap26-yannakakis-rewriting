package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

class PostgreSqlPhysicalVariantIdSuite extends munit.FunSuite {
  private val zeroCandidateId = CandidatePlanId
    .parse("sha256:" + "0" * 64)
    .fold(problem => fail(problem.message), identity)

  test("physical variant identity scheme and canonical-order identifier are frozen") {
    assertEquals(
      PostgreSqlPhysicalVariantId.Scheme,
      "postgresql_candidate_reduction_variant_v1"
    )
    assertEquals(
      PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
      "canonical_plan_schedule_v1"
    )
    assertEquals(
      YannakakisReductionMode.Values.map(mode =>
        PostgreSqlPhysicalVariantId.from(zeroCandidateId, mode).value
      ),
      Vector(
        "sha256:610557dc790e3c92593e3e7c86a1e7daa9dbaf182df561f5a959ff92fafe908d",
        "sha256:b5dfd02fa24fa586377cba6814f334f7fc1155e0a6e686530d928d9830faa3b6",
        "sha256:1a41e02211364cb903a6530b1e741c848a49efd07973decd4e341dd1e1a0ff2a"
      )
    )
  }

  test("candidate identity remains stable while each mode has one deterministic variant") {
    val frozenCandidate = CandidatePlanId
      .parse(
        "sha256:b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d"
      )
      .fold(problem => fail(problem.message), identity)
    val variants = YannakakisReductionMode.Values.map(mode =>
      PostgreSqlPhysicalVariantId.from(frozenCandidate, mode)
    )

    assertEquals(frozenCandidate.value, "sha256:b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d")
    assertEquals(
      variants.map(_.value),
      Vector(
        "sha256:f41032a42fc61ef3b912da6285e9260940797fba0e05cdaa1a01e454884cd671",
        "sha256:07ee02ed040b3a3e20d3700dd60388fb79d4807766c5e352154f9588e57e1acc",
        "sha256:54570c7a3c2779dc9d77325334e3d18985bbd71f5ea9b423cad5c85019003298"
      )
    )
    assertEquals(variants.distinct.size, YannakakisReductionMode.Values.size)
    variants.zip(YannakakisReductionMode.Values).foreach { case (variant, mode) =>
      val repeated = PostgreSqlPhysicalVariantId.from(frozenCandidate, mode)
      assertEquals(repeated, variant)
      assertEquals(repeated.hashCode, variant.hashCode)
      assertEquals(repeated.toString, variant.value)
    }
  }

  test("trusted physical-ID construction rejects missing inputs") {
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalVariantId.from(null, YannakakisReductionMode.FullTwoPass)
    }
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalVariantId.from(zeroCandidateId, null)
    }
  }
}
