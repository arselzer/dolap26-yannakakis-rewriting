package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorCoverAssignment,
  BagFactorPhysicalConfiguration
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlStageDiagnosticStatement,
  PostgreSqlStageId,
  PostgreSqlStageTrace
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, ValidationError}

class PostgreSqlPhysicalPortfolioWorkloadContractSuite
    extends munit.FunSuite {
  private val normalDigest = "c" * 64
  private val stageIdentities = Vector(
    stage(PostgreSqlStageId.BagSupport(bagId(2)), "a", 100L, 1L),
    stage(PostgreSqlStageId.BagFactorVersion(bagId(2), 0), "b", 200L, 2L),
    stage(PostgreSqlStageId.FinalReconstruction, "c", 300L, 2L)
  )

  test("workload v1 freezes the tightened five-scheme positional LP preimage and SHA-256 vector") {
    val expectedFields = Vector(
      "postgresql_sql_stage_probe_workload_sha256_v1",
      "postgresql_renderer_stage_id_v1",
      "postgresql_cte_emission_then_terminal_v1",
      "sha256_utf8_v1",
      "jvm_utf16_code_units_v1",
      normalDigest,
      "300",
      "3",
      "support:b2",
      "a" * 64,
      "100",
      "1",
      "factor:b2:v0",
      "b" * 64,
      "200",
      "2",
      "final-reconstruction",
      "c" * 64,
      "300",
      "2",
      "not-materialized",
      "all-emitted-ctes-v1"
    )
    val expectedPreimage =
      "45:postgresql_sql_stage_probe_workload_sha256_v1" +
        "31:postgresql_renderer_stage_id_v1" +
        "40:postgresql_cte_emission_then_terminal_v1" +
        "14:sha256_utf8_v1" +
        "23:jvm_utf16_code_units_v1" +
        s"64:$normalDigest" +
        "3:300" +
        "1:3" +
        "10:support:b2" +
        s"64:${"a" * 64}" +
        "3:100" +
        "1:1" +
        "12:factor:b2:v0" +
        s"64:${"b" * 64}" +
        "3:200" +
        "1:2" +
        "20:final-reconstruction" +
        s"64:${"c" * 64}" +
        "3:300" +
        "1:2" +
        "16:not-materialized" +
        "19:all-emitted-ctes-v1"

    val fields = PostgreSqlPhysicalPortfolioWorkloadDescriptor.preimageFields(
      normalDigest,
      300L,
      stageIdentities,
      PostgreSqlCteMaterializationDirective.NotMaterialized
    )
    assertEquals(fields, expectedFields)
    assertEquals(
      new String(
        PostgreSqlPhysicalPortfolioWorkloadId.preimage(fields),
        StandardCharsets.UTF_8
      ),
      expectedPreimage
    )
    assertEquals(
      PostgreSqlPhysicalPortfolioWorkloadDescriptor
        .computeId(
          normalDigest,
          300L,
          stageIdentities,
          PostgreSqlCteMaterializationDirective.NotMaterialized
        )
        .value,
      "sha256:9b7493de1107988eba16733805181b3337b6577b0f1f080d1823f9561d1769ee"
    )
  }

  test("normal SQL, ordered stage tuple fields, directive, and scope-bound schemes all influence workload identity") {
    def id(
        normal: String = normalDigest,
        characters: Long = 300L,
        stages: Vector[PostgreSqlPhysicalPortfolioStageWorkloadIdentity] = stageIdentities,
        directive: PostgreSqlCteMaterializationDirective =
          PostgreSqlCteMaterializationDirective.NotMaterialized
    ): PostgreSqlPhysicalPortfolioWorkloadId =
      PostgreSqlPhysicalPortfolioWorkloadDescriptor.computeId(
        normal,
        characters,
        stages,
        directive
      )

    val baseline = id()
    val mutations = Vector(
      id(normal = "d" * 64),
      id(characters = 301L),
      id(stages = stageIdentities.updated(0, stageIdentities.head.copy(stageId = PostgreSqlStageId.BagSupport(bagId(3))))),
      id(stages = stageIdentities.updated(0, stageIdentities.head.copy(statementSha256 = "d" * 64))),
      id(stages = stageIdentities.updated(0, stageIdentities.head.copy(characterCount = 101L))),
      id(stages = stageIdentities.updated(0, stageIdentities.head.copy(ctePrefixCount = 2L))),
      id(stages = Vector(stageIdentities(1), stageIdentities(0), stageIdentities(2))),
      id(directive = PostgreSqlCteMaterializationDirective.Materialized)
    )
    assertEquals(mutations.distinct.size, mutations.size)
    assert(!mutations.contains(baseline))
    assertEquals(id(), baseline)
  }

  test("workload ID parser is strict lowercase canonical SHA-256") {
    val value =
      "sha256:9b7493de1107988eba16733805181b3337b6577b0f1f080d1823f9561d1769ee"
    assertEquals(
      PostgreSqlPhysicalPortfolioWorkloadId.parse(value).map(_.value),
      Right(value)
    )
    Vector(
      null,
      "",
      value.dropRight(1),
      value.toUpperCase,
      value.replace("sha256:", "sha512:"),
      value + "0"
    ).foreach(candidate =>
      assert(PostgreSqlPhysicalPortfolioWorkloadId.parse(candidate).isLeft)
    )
  }

  test("persisted descriptor rejects terminal drift, noncanonical prefixes, duplicate stages, scheme drift, and ID drift") {
    val good = descriptor(
      PostgreSqlCteMaterializationDirective.NotMaterialized,
      normalDigest,
      300L,
      stageIdentities
    )
    assertEquals(good.emittedStageCount, 3)

    intercept[IllegalArgumentException] {
      good.copy(renderedStatementSha256 = "d" * 64)
    }
    intercept[IllegalArgumentException] {
      val changed = good.stages.updated(
        2,
        good.stages.last.copy(statementSha256 = "d" * 64)
      )
      good.copy(
        stages = changed,
        workloadId = PostgreSqlPhysicalPortfolioWorkloadDescriptor.computeId(
          good.renderedStatementSha256,
          good.renderedCharacterCount,
          changed,
          good.materialization
        )
      )
    }
    intercept[IllegalArgumentException] {
      val changed = good.stages.updated(0, good.stages.head.copy(ctePrefixCount = 2L))
      good.copy(
        stages = changed,
        workloadId = PostgreSqlPhysicalPortfolioWorkloadDescriptor.computeId(
          good.renderedStatementSha256,
          good.renderedCharacterCount,
          changed,
          good.materialization
        )
      )
    }
    intercept[IllegalArgumentException] {
      val changed = good.stages.updated(1, good.stages.head)
      good.copy(
        stages = changed,
        workloadId = PostgreSqlPhysicalPortfolioWorkloadDescriptor.computeId(
          good.renderedStatementSha256,
          good.renderedCharacterCount,
          changed,
          good.materialization
        )
      )
    }
    intercept[IllegalArgumentException] {
      good.copy(stageOrderScheme = "drift")
    }
    intercept[IllegalArgumentException] {
      good.copy(
        workloadId = PostgreSqlPhysicalPortfolioWorkloadDescriptor.computeId(
          "d" * 64,
          300L,
          stageIdentities,
          PostgreSqlCteMaterializationDirective.NotMaterialized
        )
      )
    }
  }

  test("group representative is the minimum physical ID while earliest scheduler ordinal remains independent") {
    val aliases = physicalAliases()
    val group = PostgreSqlPhysicalPortfolioWorkloadGroupEvidence(
      descriptor(
        PostgreSqlCteMaterializationDirective.NotMaterialized,
        normalDigest,
        300L,
        stageIdentities
      ),
      aliases.head,
      aliases,
      earliestEnumerationOrdinal = 0
    )
    assertEquals(group.representative, aliases.head)
    assertEquals(group.earliestEnumerationOrdinal, 0)
    assert(group.representative.enumerationOrdinal != group.earliestEnumerationOrdinal)

    intercept[IllegalArgumentException] {
      group.copy(aliases = aliases.reverse)
    }
    intercept[IllegalArgumentException] {
      group.copy(representative = aliases(1))
    }
    intercept[IllegalArgumentException] {
      group.copy(earliestEnumerationOrdinal = 1)
    }
  }

  test("deduplication evidence covers every raw ordinal and physical ID exactly once in earliest-group order") {
    val aliases = physicalAliases()
    val first = PostgreSqlPhysicalPortfolioWorkloadGroupEvidence(
      descriptor(
        PostgreSqlCteMaterializationDirective.NotMaterialized,
        normalDigest,
        300L,
        stageIdentities
      ),
      aliases.head,
      aliases.take(2),
      earliestEnumerationOrdinal = 0
    )
    val second = PostgreSqlPhysicalPortfolioWorkloadGroupEvidence(
      descriptor(
        PostgreSqlCteMaterializationDirective.Materialized,
        normalDigest,
        300L,
        stageIdentities
      ),
      aliases.last,
      aliases.takeRight(1),
      earliestEnumerationOrdinal = 1
    )
    val evidence = PostgreSqlPhysicalPortfolioDeduplicationEvidence(
      Vector(first, second),
      rawVariantCount = 3,
      uniqueWorkloadCount = 2,
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.DeduplicationScheme
    )
    assertEquals(evidence.groups, Vector(first, second))

    intercept[IllegalArgumentException] {
      evidence.copy(groups = evidence.groups.reverse)
    }
    intercept[IllegalArgumentException] {
      evidence.copy(rawVariantCount = 4)
    }
    intercept[IllegalArgumentException] {
      evidence.copy(uniqueWorkloadCount = 3)
    }
    intercept[IllegalArgumentException] {
      val duplicate = second.copy(aliases = Vector(first.aliases.head), representative = first.aliases.head)
      evidence.copy(groups = Vector(first, duplicate))
    }
  }

  private def descriptor(
      directive: PostgreSqlCteMaterializationDirective,
      renderedDigest: String,
      renderedCharacters: Long,
      stages: Vector[PostgreSqlPhysicalPortfolioStageWorkloadIdentity]
  ): PostgreSqlPhysicalPortfolioWorkloadDescriptor =
    PostgreSqlPhysicalPortfolioWorkloadDescriptor(
      PostgreSqlPhysicalPortfolioWorkloadDescriptor.computeId(
        renderedDigest,
        renderedCharacters,
        stages,
        directive
      ),
      renderedDigest,
      renderedCharacters,
      stages.size,
      stages,
      directive,
      PostgreSqlCteMaterializationDirective.Scope,
      PostgreSqlPhysicalPortfolioWorkloadId.Scheme,
      PostgreSqlStageId.Scheme,
      PostgreSqlStageTrace.OrderScheme,
      PostgreSqlStageDiagnosticStatement.HashScheme,
      PostgreSqlStageDiagnosticStatement.CharacterCountScheme
    )

  private def stage(
      id: PostgreSqlStageId,
      digestCharacter: String,
      characters: Long,
      prefix: Long
  ): PostgreSqlPhysicalPortfolioStageWorkloadIdentity =
    PostgreSqlPhysicalPortfolioStageWorkloadIdentity(
      id,
      digestCharacter * 64,
      characters,
      prefix
    )

  private def physicalAliases(): Vector[PostgreSqlPhysicalPortfolioWorkloadAlias] = {
    val candidate = candidateId("0" * 64)
    val configuration = BagFactorPhysicalConfiguration(
      bagId(2),
      Vector(BagFactorCoverAssignment(bagId(2), Vector(edgeId(3))))
    )
    val ids = YannakakisReductionMode.Values.map(mode =>
      mode -> PostgreSqlPhysicalVariantId.fromV2(
        candidate,
        configuration,
        mode,
        PostgreSqlCteMaterializationDirective.PlannerControlled
      )
    ).sortBy(_._2.value)
    val ordinals = Vector(2, 0, 1)
    ids.zip(ordinals).map { case ((_, id), ordinal) =>
      PostgreSqlPhysicalPortfolioWorkloadAlias(
        ordinal,
        structuralRank = 1,
        candidate,
        id
      )
    }
  }

  private def candidateId(hex: String): CandidatePlanId =
    CandidatePlanId
      .parse("sha256:" + hex)
      .fold(problem => fail(problem.message), identity)

  private def bagId(value: Int): BagId = validated(BagId.from(value))
  private def edgeId(value: Int): EdgeId = validated(EdgeId.from(value))
  private def validated[A](value: Either[ValidationError, A]): A =
    value.fold(problem => fail(problem.message), identity)
}
