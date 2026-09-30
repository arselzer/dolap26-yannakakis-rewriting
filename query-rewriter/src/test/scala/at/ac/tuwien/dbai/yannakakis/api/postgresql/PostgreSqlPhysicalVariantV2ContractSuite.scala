package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorCoverAssignment,
  BagFactorPhysicalConfiguration
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, ValidationError}

class PostgreSqlPhysicalVariantV2ContractSuite extends munit.FunSuite {
  private val zeroCandidateId = candidateId("0" * 64)
  private val configuration = BagFactorPhysicalConfiguration(
    bagId(10),
    Vector(
      BagFactorCoverAssignment(bagId(2), Vector(edgeId(3), edgeId(7))),
      BagFactorCoverAssignment(bagId(10), Vector(edgeId(11)))
    )
  )

  test("v2 positional length-prefixed preimage and SHA-256 vector are frozen") {
    val expectedPreimage =
      "50:postgresql_candidate_physical_portfolio_variant_v2" +
        "71:sha256:0000000000000000000000000000000000000000000000000000000000000000" +
        "2:10" +
        "1:2" +
        "1:2" +
        "1:2" +
        "1:3" +
        "1:7" +
        "2:10" +
        "1:1" +
        "2:11" +
        "9:bottom-up" +
        "26:canonical_plan_schedule_v1" +
        "16:not-materialized" +
        "19:all-emitted-ctes-v1"

    assertEquals(
      PostgreSqlPhysicalVariantId.SchemeV2,
      "postgresql_candidate_physical_portfolio_variant_v2"
    )
    assertEquals(
      PostgreSqlPhysicalVariantId.v2CanonicalPreimage(
        zeroCandidateId,
        configuration,
        YannakakisReductionMode.BottomUpOnly,
        PostgreSqlCteMaterializationDirective.NotMaterialized
      ),
      expectedPreimage
    )
    assertEquals(
      PostgreSqlPhysicalVariantId
        .fromV2(
          zeroCandidateId,
          configuration,
          YannakakisReductionMode.BottomUpOnly,
          PostgreSqlCteMaterializationDirective.NotMaterialized
        )
        .value,
      "sha256:ee6d0b579934a96f190e8a387607054fa983870ad42bcf8bd095f646142f1e41"
    )
    assertEquals(sha256(expectedPreimage), "ee6d0b579934a96f190e8a387607054fa983870ad42bcf8bd095f646142f1e41")
  }

  test("each v2 positional dimension participates in identity and repeat construction is stable") {
    val base = variantId(
      zeroCandidateId,
      configuration,
      YannakakisReductionMode.FullTwoPass,
      PostgreSqlCteMaterializationDirective.PlannerControlled
    )
    val anotherCandidate = candidateId("1" * 64)
    val anotherRoot = configuration.copy(executionRoot = bagId(2))
    val anotherCover = configuration.copy(
      coverAssignments = configuration.coverAssignments.updated(
        0,
        BagFactorCoverAssignment(bagId(2), Vector(edgeId(3), edgeId(8)))
      )
    )
    val changes = Vector(
      variantId(
        anotherCandidate,
        configuration,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      ),
      variantId(
        zeroCandidateId,
        anotherRoot,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      ),
      variantId(
        zeroCandidateId,
        anotherCover,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      ),
      variantId(
        zeroCandidateId,
        configuration,
        YannakakisReductionMode.BottomUpOnly,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      ),
      variantId(
        zeroCandidateId,
        configuration,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlCteMaterializationDirective.Materialized
      )
    )

    assertEquals(changes.distinct.size, changes.size)
    assert(!changes.contains(base))
    assertEquals(
      variantId(
        zeroCandidateId,
        configuration,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      ),
      base
    )
  }

  test("v2 identity relies on constructor-enforced canonical bag and edge order") {
    intercept[IllegalArgumentException] {
      BagFactorCoverAssignment(bagId(2), Vector(edgeId(7), edgeId(3)))
    }
    intercept[IllegalArgumentException] {
      BagFactorPhysicalConfiguration(
        bagId(10),
        configuration.coverAssignments.reverse
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalVariantId.fromV2(
        null,
        configuration,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalVariantId.fromV2(
        zeroCandidateId,
        null,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalVariantId.fromV2(
        zeroCandidateId,
        configuration,
        null,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlPhysicalVariantId.fromV2(
        zeroCandidateId,
        configuration,
        YannakakisReductionMode.FullTwoPass,
        null
      )
    }
  }

  test("legacy v1 scheme and all frozen candidate-mode vectors remain byte-identical") {
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

  private def variantId(
      candidate: CandidatePlanId,
      config: BagFactorPhysicalConfiguration,
      mode: YannakakisReductionMode,
      directive: PostgreSqlCteMaterializationDirective
  ): PostgreSqlPhysicalVariantId =
    PostgreSqlPhysicalVariantId.fromV2(candidate, config, mode, directive)

  private def candidateId(hex: String): CandidatePlanId =
    CandidatePlanId
      .parse("sha256:" + hex)
      .fold(problem => fail(problem.message), identity)

  private def bagId(value: Int): BagId = validated(BagId.from(value))
  private def edgeId(value: Int): EdgeId = validated(EdgeId.from(value))

  private def validated[A](value: Either[ValidationError, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def sha256(value: String): String = {
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
    bytes.map(byte => f"${byte & 0xff}%02x").mkString
  }
}
