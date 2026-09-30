package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlAggregateOneBagTerminalProfile,
  PostgreSqlAggregateReductionFinalizationProfile,
  PostgreSqlAggregateRenderingProfile,
  PostgreSqlReductionSupportProfile
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

class PostgreSqlGeneratedSqlOptimizationActionIdSuite extends munit.FunSuite {
  import PostgreSqlAggregateOneBagTerminalProfile._
  import PostgreSqlAggregateReductionFinalizationProfile._
  import PostgreSqlReductionSupportProfile._

  private val zeroCandidateId = CandidatePlanId
    .parse("sha256:" + "0" * 64)
    .fold(problem => fail(problem.message), identity)
  private val basePhysicalVariantId = PostgreSqlPhysicalVariantId.from(
    zeroCandidateId,
    YannakakisReductionMode.FullTwoPass
  )

  test("legacy profile and additive action preimages have frozen vectors") {
    val profilePreimage =
      "41:postgresql_aggregate_rendering_profile_v1" +
        "16:legacy-nested-v1" +
        "23:legacy-full-vertices-v1" +
        "29:distinct-separator-support-v1"
    val actionPreimage =
      "47:postgresql_generated_sql_optimization_action_v1" +
        "71:sha256:610557dc790e3c92593e3e7c86a1e7daa9dbaf182df561f5a959ff92fafe908d" +
        profilePreimage

    assertEquals(
      PostgreSqlAggregateRenderingProfile.Scheme,
      "postgresql_aggregate_rendering_profile_v1"
    )
    assertEquals(
      PostgreSqlGeneratedSqlOptimizationActionId.Scheme,
      "postgresql_generated_sql_optimization_action_v1"
    )
    assertEquals(
      basePhysicalVariantId.value,
      "sha256:610557dc790e3c92593e3e7c86a1e7daa9dbaf182df561f5a959ff92fafe908d"
    )
    assertEquals(
      PostgreSqlAggregateRenderingProfile.canonicalPreimage(
        PostgreSqlAggregateRenderingProfile.LegacyV1
      ),
      profilePreimage
    )
    assertEquals(
      PostgreSqlAggregateRenderingProfile.LegacyV1.id,
      "sha256:6a0e83e8b11b3404ad2df574864539e703f746cb84ec73edb49f13a537b39eab"
    )
    assertEquals(
      PostgreSqlGeneratedSqlOptimizationActionId.canonicalPreimage(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlAggregateRenderingProfile.LegacyV1
      ),
      actionPreimage
    )
    assertEquals(
      PostgreSqlGeneratedSqlOptimizationActionId
        .from(
          zeroCandidateId,
          YannakakisReductionMode.FullTwoPass,
          PostgreSqlAggregateRenderingProfile.LegacyV1
        )
        .value,
      "sha256:d25176ba5bcfafda6fc879f977cca7dd9af78c2f2229df4a61b3e15ab15861cb"
    )
  }

  test("the closed profile product is deterministic and every axis changes identity") {
    val profiles = PostgreSqlAggregateRenderingProfile.Values
    assertEquals(profiles.size, 8)
    assertEquals(profiles.distinct.size, 8)
    assertEquals(profiles.map(_.axisIds), expectedAxisProduct)
    assertEquals(profiles.head, PostgreSqlAggregateRenderingProfile.LegacyV1)
    assertEquals(profiles.map(_.id).distinct.size, profiles.size)

    val actions = profiles.map(profile =>
      PostgreSqlGeneratedSqlOptimizationActionId.from(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        profile
      )
    )
    assertEquals(actions.distinct.size, profiles.size)
    assertEquals(
      profiles.map(profile =>
        PostgreSqlGeneratedSqlOptimizationActionId.from(
          zeroCandidateId,
          YannakakisReductionMode.FullTwoPass,
          profile
        )
      ),
      actions
    )
    actions.foreach { action =>
      assert(action.value.matches("sha256:[0-9a-f]{64}"), clues(action))
      assertEquals(action.toString, action.value)
      assertEquals(action.hashCode, action.value.hashCode)
    }
  }

  test("construction canonicalizes only the exact legacy triple") {
    val legacy = create(
      LegacyNestedV1,
      LegacyFullVerticesV1,
      DistinctSeparatorSupportV1
    )
    assert(legacy eq PostgreSqlAggregateRenderingProfile.LegacyV1)

    val optimized = create(
      CollapsedDirectV2,
      CompactLocalStateV2,
      DirectExistsV2
    )
    val repeated = create(
      CollapsedDirectV2,
      CompactLocalStateV2,
      DirectExistsV2
    )
    assert(optimized.isInstanceOf[PostgreSqlAggregateRenderingProfile.ExactV1])
    assertEquals(optimized, repeated)
    assertEquals(optimized.hashCode, repeated.hashCode)
    assertEquals(
      optimized.axisIds,
      Vector(
        "collapsed-direct-v2",
        "compact-local-state-v2",
        "direct-exists-v2"
      )
    )
  }

  test("profile validation rejects every missing axis in canonical order") {
    val allMissing = PostgreSqlAggregateRenderingProfile.create(
      null,
      null,
      null
    )
    assertEquals(
      allMissing.left.toOption.toVector.flatten.map(_.code),
      Vector(
        "aggregate-rendering-profile.invalid.one-bag-terminal",
        "aggregate-rendering-profile.invalid.reduction-finalization",
        "aggregate-rendering-profile.invalid.reduction-support"
      )
    )

    val cases = Vector(
      PostgreSqlAggregateRenderingProfile.create(
        null,
        LegacyFullVerticesV1,
        DistinctSeparatorSupportV1
      ) -> "aggregate-rendering-profile.invalid.one-bag-terminal",
      PostgreSqlAggregateRenderingProfile.create(
        LegacyNestedV1,
        null,
        DistinctSeparatorSupportV1
      ) -> "aggregate-rendering-profile.invalid.reduction-finalization",
      PostgreSqlAggregateRenderingProfile.create(
        LegacyNestedV1,
        LegacyFullVerticesV1,
        null
      ) -> "aggregate-rendering-profile.invalid.reduction-support"
    )
    cases.foreach { case (result, expected) =>
      assertEquals(
        result.left.toOption.toVector.flatten.map(_.code),
        Vector(expected)
      )
    }
    assertEquals(
      PostgreSqlAggregateRenderingProfile.validate(null).map(_.code),
      Vector("aggregate-rendering-profile.invalid.profile")
    )
  }

  test("action identity rejects missing inputs and binds its base physical ID") {
    intercept[IllegalArgumentException] {
      PostgreSqlGeneratedSqlOptimizationActionId.from(
        null,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlAggregateRenderingProfile.LegacyV1
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlGeneratedSqlOptimizationActionId.from(
        zeroCandidateId,
        null,
        PostgreSqlAggregateRenderingProfile.LegacyV1
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlGeneratedSqlOptimizationActionId.from(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        null
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlGeneratedSqlOptimizationActionId.canonicalPreimage(
        null,
        YannakakisReductionMode.FullTwoPass,
        PostgreSqlAggregateRenderingProfile.LegacyV1
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlAggregateRenderingProfile.canonicalPreimage(null)
    }

    val legacy = PostgreSqlAggregateRenderingProfile.LegacyV1
    assertNotEquals(
      PostgreSqlGeneratedSqlOptimizationActionId.from(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        legacy
      ),
      PostgreSqlGeneratedSqlOptimizationActionId.from(
        zeroCandidateId,
        YannakakisReductionMode.BottomUpOnly,
        legacy
      )
    )
  }

  private def create(
      terminal: PostgreSqlAggregateOneBagTerminalProfile,
      finalization: PostgreSqlAggregateReductionFinalizationProfile,
      support: PostgreSqlReductionSupportProfile
  ): PostgreSqlAggregateRenderingProfile =
    PostgreSqlAggregateRenderingProfile
      .create(terminal, finalization, support)
      .fold(
        problems => fail(problems.map(_.code).mkString("; ")),
        identity
      )

  private val expectedAxisProduct: Vector[Vector[String]] = Vector(
    Vector(
      "legacy-nested-v1",
      "legacy-full-vertices-v1",
      "distinct-separator-support-v1"
    ),
    Vector(
      "legacy-nested-v1",
      "legacy-full-vertices-v1",
      "direct-exists-v2"
    ),
    Vector(
      "legacy-nested-v1",
      "compact-local-state-v2",
      "distinct-separator-support-v1"
    ),
    Vector(
      "legacy-nested-v1",
      "compact-local-state-v2",
      "direct-exists-v2"
    ),
    Vector(
      "collapsed-direct-v2",
      "legacy-full-vertices-v1",
      "distinct-separator-support-v1"
    ),
    Vector(
      "collapsed-direct-v2",
      "legacy-full-vertices-v1",
      "direct-exists-v2"
    ),
    Vector(
      "collapsed-direct-v2",
      "compact-local-state-v2",
      "distinct-separator-support-v1"
    ),
    Vector(
      "collapsed-direct-v2",
      "compact-local-state-v2",
      "direct-exists-v2"
    )
  )
}
