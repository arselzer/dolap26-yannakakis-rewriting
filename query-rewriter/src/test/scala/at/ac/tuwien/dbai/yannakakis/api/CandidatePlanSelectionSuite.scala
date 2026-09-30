package at.ac.tuwien.dbai.yannakakis.api

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId

class CandidatePlanSelectionSuite extends munit.FunSuite {
  test("selection policy identifiers are stable, non-cost, and unique") {
    val candidateId = CandidatePlanId
      .parse(
        "sha256:b6f1006706778b364299cb342db7fb6087b8021bd24437c90de8d674135e8e9d"
      )
      .fold(problem => fail(problem.message), identity)
    val policies = Vector[CandidatePlanSelection](
      CandidatePlanSelection.StructuralHead,
      CandidatePlanSelection.StructuralRank(2),
      CandidatePlanSelection.ExactCandidateId(candidateId),
      CandidatePlanSelection.PostgreSqlStaticShapeV1
    )

    assertEquals(
      policies.map(_.id),
      Vector(
        "structural_top_k_head_v1",
        "explicit_structural_rank_v1",
        "explicit_candidate_id_v1",
        "postgresql_static_shape_lexicographic_v1"
      )
    )
    assertEquals(policies.map(_.id).distinct.size, policies.size)
    assertEquals(CandidatePlanSelection.Default, CandidatePlanSelection.StructuralHead)
  }

  test("rewrite options preserve structural-head source and behavior defaults") {
    val legacyShape = RewriteOptions(
      widthBound = 3,
      planLimit = 7,
      requireConnectedCover = false
    )

    assertEquals(legacyShape.widthBound, 3)
    assertEquals(legacyShape.planLimit, 7)
    assertEquals(legacyShape.requireConnectedCover, false)
    assertEquals(
      legacyShape.candidateSelection,
      CandidatePlanSelection.StructuralHead
    )
    assertEquals(
      legacyShape.reductionMode,
      YannakakisReductionMode.FullTwoPass
    )
    assertEquals(RewriteOptions.Default.candidateSelection, CandidatePlanSelection.StructuralHead)
    assertEquals(RewriteOptions.validated(), Right(RewriteOptions.Default))
  }

  test("trusted explicit rank and id constructors fail closed on invalid values") {
    intercept[IllegalArgumentException] {
      CandidatePlanSelection.StructuralRank(0)
    }
    intercept[IllegalArgumentException] {
      CandidatePlanSelection.StructuralRank(-1)
    }
    intercept[IllegalArgumentException] {
      CandidatePlanSelection.ExactCandidateId(null)
    }
    intercept[IllegalArgumentException] {
      RewriteOptions(
        planLimit = 2,
        candidateSelection = CandidatePlanSelection.StructuralRank(3)
      )
    }
    intercept[IllegalArgumentException] {
      RewriteOptions(candidateSelection = null)
    }
  }

  test("untrusted options report selection validation without throwing") {
    val rank = RewriteOptions.validated(
      planLimit = 2,
      candidateSelection = CandidatePlanSelection.StructuralRank(3)
    )
    val rankDiagnostics = rank.left.toOption.getOrElse(
      fail("rank beyond retained top-k envelope was accepted")
    )
    assertEquals(rankDiagnostics.map(_.code), Vector(RewriteReasonCode.InvalidOption))
    assertEquals(rankDiagnostics.head.context("option"), "candidateSelection")
    assertEquals(rankDiagnostics.head.context("rank"), "3")
    assertEquals(rankDiagnostics.head.context("planLimit"), "2")

    val missing = RewriteOptions.validated(candidateSelection = null)
    val missingDiagnostics = missing.left.toOption.getOrElse(
      fail("null selection policy was accepted")
    )
    assertEquals(missingDiagnostics.map(_.code), Vector(RewriteReasonCode.InvalidOption))
    assertEquals(missingDiagnostics.head.context("option"), "candidateSelection")

    val invalidPlanLimit = RewriteOptions.validated(
      planLimit = 0,
      candidateSelection = CandidatePlanSelection.StructuralRank(1)
    )
    assertEquals(
      invalidPlanLimit.left.toOption.toVector.flatten.map(_.context("option")),
      Vector("planLimit")
    )
  }
}
