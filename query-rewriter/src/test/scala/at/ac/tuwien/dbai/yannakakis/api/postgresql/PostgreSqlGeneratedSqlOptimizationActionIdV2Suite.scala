package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlAggregateLeafMessageProfile,
  PostgreSqlAggregateOneBagTerminalProfile,
  PostgreSqlAggregateReductionFinalizationProfile,
  PostgreSqlAggregateRenderingProfile,
  PostgreSqlAggregateRenderingProfileV2,
  PostgreSqlFactorBoundaryProjectionProfile,
  PostgreSqlReductionSupportProfile
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

class PostgreSqlGeneratedSqlOptimizationActionIdV2Suite
    extends munit.FunSuite {
  import PostgreSqlAggregateLeafMessageProfile._
  import PostgreSqlAggregateOneBagTerminalProfile._
  import PostgreSqlAggregateReductionFinalizationProfile._
  import PostgreSqlFactorBoundaryProjectionProfile._
  import PostgreSqlReductionSupportProfile._

  private val zeroCandidateId = candidateId('0')
  private val oneCandidateId = candidateId('1')

  private val compactBase = createBase(
    LegacyNestedV1,
    CompactLocalStateV2,
    DistinctSeparatorSupportV1
  )
  private val optimizedProfile = createV2(
    compactBase,
    DemandedStructuralColumnsV2,
    ExactDirectFactorV2
  )

  test("renderer-v2 profile and action preimages have frozen vectors") {
    val baseProfilePreimage =
      "41:postgresql_aggregate_rendering_profile_v1" +
        "16:legacy-nested-v1" +
        "22:compact-local-state-v2" +
        "29:distinct-separator-support-v1"
    val profilePreimage =
      "41:postgresql_aggregate_rendering_profile_v2" +
        "41:postgresql_aggregate_rendering_profile_v1" +
        "71:sha256:9aacff86d725ce4e9dea4a65dbed50a95634d9a6dcab25f4cc3381c381e5aa77" +
        "16:legacy-nested-v1" +
        "22:compact-local-state-v2" +
        "29:distinct-separator-support-v1" +
        "30:demanded-structural-columns-v2" +
        "22:exact-direct-factor-v2"
    val baseActionPreimage =
      "47:postgresql_generated_sql_optimization_action_v1" +
        "71:sha256:610557dc790e3c92593e3e7c86a1e7daa9dbaf182df561f5a959ff92fafe908d" +
        baseProfilePreimage
    val actionPreimage =
      "47:postgresql_generated_sql_optimization_action_v2" +
        "47:postgresql_generated_sql_optimization_action_v1" +
        "71:sha256:f9c3e46f12e27d7182eb769922978d163c397ec2fa28e531f79df34773cfe9cf" +
        "41:postgresql_aggregate_rendering_profile_v2" +
        "71:sha256:27ab5d6e74162bc097dbd5394f952b312df43d8c9469ab56592954eea0ee5879" +
        "41:postgresql_aggregate_rendering_profile_v1" +
        "71:sha256:9aacff86d725ce4e9dea4a65dbed50a95634d9a6dcab25f4cc3381c381e5aa77" +
        "16:legacy-nested-v1" +
        "22:compact-local-state-v2" +
        "29:distinct-separator-support-v1" +
        "30:demanded-structural-columns-v2" +
        "22:exact-direct-factor-v2"

    assertEquals(
      PostgreSqlAggregateRenderingProfileV2.Scheme,
      "postgresql_aggregate_rendering_profile_v2"
    )
    assertEquals(
      PostgreSqlGeneratedSqlOptimizationActionIdV2.Scheme,
      "postgresql_generated_sql_optimization_action_v2"
    )
    assertEquals(
      PostgreSqlAggregateRenderingProfile.canonicalPreimage(compactBase),
      baseProfilePreimage
    )
    assertEquals(
      compactBase.id,
      "sha256:9aacff86d725ce4e9dea4a65dbed50a95634d9a6dcab25f4cc3381c381e5aa77"
    )
    assertEquals(
      PostgreSqlAggregateRenderingProfileV2.canonicalPreimage(
        optimizedProfile
      ),
      profilePreimage
    )
    assertEquals(
      optimizedProfile.id,
      "sha256:27ab5d6e74162bc097dbd5394f952b312df43d8c9469ab56592954eea0ee5879"
    )
    assertEquals(
      PostgreSqlGeneratedSqlOptimizationActionId.canonicalPreimage(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        compactBase
      ),
      baseActionPreimage
    )
    assertEquals(
      PostgreSqlGeneratedSqlOptimizationActionIdV2.canonicalPreimage(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        optimizedProfile
      ),
      actionPreimage
    )
    assertEquals(
      PostgreSqlGeneratedSqlOptimizationActionIdV2
        .from(
          zeroCandidateId,
          YannakakisReductionMode.FullTwoPass,
          optimizedProfile
        )
        .value,
      "sha256:3aa7a469c1d61a4cf6132beb95e6c7a3a3c11c24a8b59e332da6aa49da94661b"
    )
  }

  test("the additive closed product preserves every frozen v1 base") {
    val profiles = PostgreSqlAggregateRenderingProfileV2.Values
    assertEquals(profiles.size, 20)
    assertEquals(profiles.distinct.size, profiles.size)
    assertEquals(profiles.map(_.id).distinct.size, profiles.size)

    PostgreSqlAggregateRenderingProfile.Values.foreach { base =>
      val expectedSize =
        if (base.reductionFinalization == CompactLocalStateV2) 4 else 1
      val compatible = PostgreSqlAggregateRenderingProfileV2.valuesFor(base)
      assertEquals(compatible.size, expectedSize, clues(base.axisIds))
      assertEquals(compatible.head, baseline(base))
      assert(compatible.head.isBaseline, clues(base.axisIds))
      assert(!compatible.head.usesOptimizedAxes, clues(base.axisIds))
      assert(
        compatible.tail.forall(profile =>
          profile.usesOptimizedAxes && !profile.isBaseline
        ),
        clues(base.axisIds, compatible.map(_.axisIds))
      )
      assert(compatible.forall(_.baseProfile == base), clues(base.axisIds))
    }

    assertEquals(
      PostgreSqlAggregateRenderingProfileV2.valuesFor(compactBase).map(_.axisIds),
      Vector(
        Vector("full-factor-vertices-v1", "derived-select-v1"),
        Vector("full-factor-vertices-v1", "exact-direct-factor-v2"),
        Vector("demanded-structural-columns-v2", "derived-select-v1"),
        Vector("demanded-structural-columns-v2", "exact-direct-factor-v2")
      )
    )
    assertEquals(
      PostgreSqlAggregateRenderingProfileV2
        .valuesFor(PostgreSqlAggregateRenderingProfile.LegacyV1)
        .map(_.axisIds),
      Vector(Vector("full-factor-vertices-v1", "derived-select-v1"))
    )
  }

  test("optimized axes require an exact compact-state v1 base") {
    val legacy = PostgreSqlAggregateRenderingProfile.LegacyV1
    val demandOnly = PostgreSqlAggregateRenderingProfileV2.create(
      legacy,
      DemandedStructuralColumnsV2,
      DerivedSelectV1
    )
    val leafOnly = PostgreSqlAggregateRenderingProfileV2.create(
      legacy,
      FullFactorVerticesV1,
      ExactDirectFactorV2
    )
    val combined = PostgreSqlAggregateRenderingProfileV2.create(
      legacy,
      DemandedStructuralColumnsV2,
      ExactDirectFactorV2
    )

    assertEquals(
      errorCodes(demandOnly),
      Vector(
        "aggregate-rendering-profile-v2.invalid.factor-boundary-projection-compatibility"
      )
    )
    assertEquals(
      errorCodes(leafOnly),
      Vector("aggregate-rendering-profile-v2.invalid.leaf-message-compatibility")
    )
    assertEquals(
      errorCodes(combined),
      Vector(
        "aggregate-rendering-profile-v2.invalid.factor-boundary-projection-compatibility",
        "aggregate-rendering-profile-v2.invalid.leaf-message-compatibility"
      )
    )

    val compactProfiles = Vector(
      createV2(compactBase, DemandedStructuralColumnsV2, DerivedSelectV1),
      createV2(compactBase, FullFactorVerticesV1, ExactDirectFactorV2),
      optimizedProfile
    )
    compactProfiles.foreach(profile =>
      assertEquals(PostgreSqlAggregateRenderingProfileV2.validate(profile), Vector.empty)
    )
  }

  test("renderer-v2 profile validation rejects missing fields without cascades") {
    assertEquals(
      errorCodes(PostgreSqlAggregateRenderingProfileV2.create(null, null, null)),
      Vector(
        "aggregate-rendering-profile-v2.invalid.base-profile",
        "aggregate-rendering-profile-v2.invalid.factor-boundary-projection",
        "aggregate-rendering-profile-v2.invalid.leaf-message"
      )
    )
    assertEquals(
      errorCodes(
        PostgreSqlAggregateRenderingProfileV2.create(
          null,
          DemandedStructuralColumnsV2,
          ExactDirectFactorV2
        )
      ),
      Vector("aggregate-rendering-profile-v2.invalid.base-profile")
    )
    assertEquals(
      errorCodes(
        PostgreSqlAggregateRenderingProfileV2.create(
          compactBase,
          null,
          DerivedSelectV1
        )
      ),
      Vector("aggregate-rendering-profile-v2.invalid.factor-boundary-projection")
    )
    assertEquals(
      errorCodes(
        PostgreSqlAggregateRenderingProfileV2.create(
          compactBase,
          FullFactorVerticesV1,
          null
        )
      ),
      Vector("aggregate-rendering-profile-v2.invalid.leaf-message")
    )
    assertEquals(
      PostgreSqlAggregateRenderingProfileV2.validate(null).map(_.code),
      Vector("aggregate-rendering-profile-v2.invalid.profile")
    )
    intercept[IllegalArgumentException] {
      PostgreSqlAggregateRenderingProfileV2.canonicalPreimage(null)
    }
  }

  test("v2 action identity binds base action and every additive input") {
    val profiles = PostgreSqlAggregateRenderingProfileV2
      .valuesFor(compactBase)
      .filter(_.usesOptimizedAxes)
    val fullActions = profiles.map(profile =>
      PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        profile
      )
    )
    assertEquals(fullActions.distinct.size, profiles.size)
    fullActions.foreach { action =>
      assert(action.value.matches("sha256:[0-9a-f]{64}"), clues(action))
      assertEquals(action.toString, action.value)
      assertEquals(action.hashCode, action.value.hashCode)
    }

    val alternateBase = createBase(
      CollapsedDirectV2,
      CompactLocalStateV2,
      DistinctSeparatorSupportV1
    )
    val inputVariants = Vector(
      PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        optimizedProfile
      ),
      PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
        oneCandidateId,
        YannakakisReductionMode.FullTwoPass,
        optimizedProfile
      ),
      PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        createV2(
          compactBase,
          FullFactorVerticesV1,
          ExactDirectFactorV2
        )
      ),
      PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        createV2(
          alternateBase,
          DemandedStructuralColumnsV2,
          ExactDirectFactorV2
        )
      )
    )
    assertEquals(inputVariants.distinct.size, inputVariants.size)

    val repeated = PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
      zeroCandidateId,
      YannakakisReductionMode.FullTwoPass,
      optimizedProfile
    )
    assertEquals(repeated, fullActions.last)
    assertEquals(repeated.hashCode, fullActions.last.hashCode)
  }

  test("v2 action identities require an optimized full-reduction profile") {
    val baselineProfile = baseline(compactBase)
    interceptMessage[IllegalArgumentException](
      "requirement failed: renderer-v2 action identity requires at least one optimized axis"
    ) {
      PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        baselineProfile
      )
    }
    Vector(
      YannakakisReductionMode.BottomUpOnly,
      YannakakisReductionMode.NoReduction
    ).foreach { mode =>
      interceptMessage[IllegalArgumentException](
        "requirement failed: optimized renderer-v2 actions require full reduction"
      ) {
        PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
          zeroCandidateId,
          mode,
          optimizedProfile
        )
      }
    }
  }

  test("v2 action identity rejects missing inputs") {
    intercept[IllegalArgumentException] {
      PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
        null,
        YannakakisReductionMode.FullTwoPass,
        optimizedProfile
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
        zeroCandidateId,
        null,
        optimizedProfile
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlGeneratedSqlOptimizationActionIdV2.from(
        zeroCandidateId,
        YannakakisReductionMode.FullTwoPass,
        null
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlGeneratedSqlOptimizationActionIdV2.canonicalPreimage(
        null,
        YannakakisReductionMode.FullTwoPass,
        optimizedProfile
      )
    }
  }

  private def candidateId(digit: Char): CandidatePlanId =
    CandidatePlanId
      .parse("sha256:" + digit.toString * 64)
      .fold(problem => fail(problem.message), identity)

  private def createBase(
      terminal: PostgreSqlAggregateOneBagTerminalProfile,
      finalization: PostgreSqlAggregateReductionFinalizationProfile,
      support: PostgreSqlReductionSupportProfile
  ): PostgreSqlAggregateRenderingProfile =
    PostgreSqlAggregateRenderingProfile
      .create(terminal, finalization, support)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)

  private def createV2(
      base: PostgreSqlAggregateRenderingProfile,
      projection: PostgreSqlFactorBoundaryProjectionProfile,
      leaf: PostgreSqlAggregateLeafMessageProfile
  ): PostgreSqlAggregateRenderingProfileV2 =
    PostgreSqlAggregateRenderingProfileV2
      .create(base, projection, leaf)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)

  private def baseline(
      base: PostgreSqlAggregateRenderingProfile
  ): PostgreSqlAggregateRenderingProfileV2 =
    PostgreSqlAggregateRenderingProfileV2
      .baseline(base)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)

  private def errorCodes(
      result: Either[
        Vector[PostgreSqlAggregateRenderingProfileV2.ValidationError],
        PostgreSqlAggregateRenderingProfileV2
      ]
  ): Vector[String] =
    result.left.toOption.toVector.flatten.map(_.code)
}
