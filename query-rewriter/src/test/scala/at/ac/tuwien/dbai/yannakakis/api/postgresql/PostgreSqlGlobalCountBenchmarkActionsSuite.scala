package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql
  .PostgreSqlGlobalCountBenchmarkRealization
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

final class PostgreSqlGlobalCountBenchmarkActionsSuite extends munit.FunSuite {
  private val cycleSql =
    "SELECT COUNT(*) AS cycle_rows " +
      "FROM win_r AS r " +
      "JOIN win_s AS s ON r.x = s.x " +
      "JOIN win_t AS t ON s.y = t.y " +
      "JOIN win_u AS u ON t.z = u.z AND u.w = r.w"

  private val triangleSql =
    "SELECT COUNT(*) AS triangle_rows " +
      "FROM tri_r AS r " +
      "JOIN tri_s AS s ON r.b = s.b " +
      "JOIN tri_t AS t ON s.c = t.c AND t.a = r.a"

  private val rewriter = {
    val schema = Frameworks.createRootSchema(true)
    addTable(schema, "win_r", Vector("x", "w"))
    addTable(schema, "win_s", Vector("x", "y"))
    addTable(schema, "win_t", Vector("y", "z"))
    addTable(schema, "win_u", Vector("z", "w"))
    addTable(schema, "tri_r", Vector("a", "b"))
    addTable(schema, "tri_s", Vector("b", "c"))
    addTable(schema, "tri_t", Vector("c", "a"))
    val planner = CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
    PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
  }

  test("exports the optimized full control and fused raw action for one rank") {
    val value = PostgreSqlGlobalCountBenchmarkActions
      .produce(artifact(cycleSql, widthBound = 2, planLimit = 5), Vector(1))
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

    assertEquals(value.protocolId, PostgreSqlGlobalCountBenchmarkActions.ProtocolId)
    assertEquals(value.resultShapeId, "global-count-star")
    assertEquals(value.requestedRanks, Vector(1))
    assertEquals(value.skippedRealizations, Vector.empty)
    assertEquals(value.actions.size, 2)
    assertEquals(
      value.actions.map(_.realization),
      PostgreSqlGlobalCountBenchmarkRealization.Values
    )
    assertEquals(
      value.actions.map(_.aliasId),
      Vector(
        "global-count-r01-full-v2-pc",
        "global-count-r01-fused-v3-none-pc"
      )
    )

    val full = value.actions.head
    val fused = value.actions.last
    assertEquals(full.candidateId, fused.candidateId)
    assertEquals(full.configuration, fused.configuration)
    assertEquals(full.factorCount, fused.factorCount)
    assert(full.factorCount > 1)
    assertEquals(full.reductionMode, YannakakisReductionMode.FullTwoPass)
    assertEquals(fused.reductionMode, YannakakisReductionMode.NoReduction)
    assertNotEquals(full.basePhysicalVariantId, fused.basePhysicalVariantId)
    assertNotEquals(full.actionId, fused.actionId)
    assertNotEquals(full.renderedQuery.sql, fused.renderedQuery.sql)
    assertEquals(full.renderedQuery.trace.reduction.mode, full.reductionMode)
    assertEquals(fused.renderedQuery.trace.reduction.mode, fused.reductionMode)
    assert(fused.renderedQuery.trace.semijoins.isEmpty)
    assertEquals(
      fused.renderedQuery.trace.reduction.physicalReductionCteCount,
      0
    )
    value.actions.foreach(action => {
      assertEquals(
        action.renderedStatementSha256,
        sha256(action.renderedQuery.sql)
      )
      assertEquals(
        action.actionId,
        PostgreSqlGlobalCountBenchmarkActionId.from(
          action.realization,
          action.candidateId,
          action.basePhysicalVariantId,
          action.renderer,
          action.renderedStatementSha256
        )
      )
      assertEquals(action.renderer.enumerationProfileId, "statistics-safe-v2")
    })

    assertEquals(
      full.renderer.aggregateProfileV1AxisIds,
      Vector(
        "legacy-nested-v1",
        "compact-local-state-v2",
        "distinct-separator-support-v1"
      )
    )
    assertEquals(
      full.renderer.aggregateProfileV2AxisIds,
      Vector("demanded-structural-columns-v2", "exact-direct-factor-v2")
    )
    assertEquals(
      full.renderer.aggregateProfileV3AxisIds,
      Vector("reduced-factor-messages-v1")
    )
    assertEquals(
      fused.renderer.aggregateProfileV1AxisIds,
      Vector(
        "legacy-nested-v1",
        "legacy-full-vertices-v1",
        "distinct-separator-support-v1"
      )
    )
    assertEquals(
      fused.renderer.aggregateProfileV2AxisIds,
      Vector("full-factor-vertices-v1", "derived-select-v1")
    )
    assertEquals(
      fused.renderer.aggregateProfileV3AxisIds,
      Vector("fused-raw-factor-messages-v1")
    )
  }

  test("exports the one-bag full control and skips only fused V3") {
    val rewriteArtifact = artifact(triangleSql, widthBound = 2, planLimit = 1)
    assertEquals(rewriteArtifact.bagFactorPlan.factors.size, 1)
    val value = PostgreSqlGlobalCountBenchmarkActions
      .produce(rewriteArtifact, Vector(1))
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

    assertEquals(value.actions.size, 1)
    assertEquals(
      value.actions.head.realization,
      PostgreSqlGlobalCountBenchmarkRealization.FullReductionControl
    )
    assertEquals(value.actions.head.factorCount, 1)
    assertEquals(value.skippedRealizations.size, 1)
    assertEquals(value.skippedRealizations.head.structuralRank, 1)
    assertEquals(value.skippedRealizations.head.factorCount, 1)
    assertEquals(
      value.skippedRealizations.head.realization,
      PostgreSqlGlobalCountBenchmarkRealization.FusedRawNoReduction
    )
    assertEquals(
      value.skippedRealizations.head.reason,
      PostgreSqlGlobalCountBenchmarkActions.OneBagReason
    )
  }

  test("rejects invalid rank requests and non-count result shapes") {
    val count = artifact(cycleSql, widthBound = 2, planLimit = 2)
    assertEquals(
      PostgreSqlGlobalCountBenchmarkActions
        .produce(count, Vector(2, 1))
        .left
        .toOption
        .map(_.code),
      Some("global-count-actions.ranks.invalid")
    )
    assertEquals(
      PostgreSqlGlobalCountBenchmarkActions
        .produce(count, Vector(3))
        .left
        .toOption
        .map(_.code),
      Some("global-count-actions.rank.not-retained")
    )

    val enumeration = artifact(
      cycleSql.replace("COUNT(*) AS cycle_rows", "r.x AS cycle_key"),
      widthBound = 2,
      planLimit = 1
    )
    assertEquals(
      PostgreSqlGlobalCountBenchmarkActions
        .produce(enumeration, Vector(1))
        .left
        .toOption
        .map(_.code),
      Some("global-count-actions.shape.unsupported")
    )
  }

  test("native and generated action identities bind their exact SQL digests") {
    val zero = "0" * 64
    val one = "1" * 64
    val nativeZero =
      PostgreSqlGlobalCountBenchmarkActionId.fromNativeSource(zero)
    assertEquals(
      nativeZero,
      PostgreSqlGlobalCountBenchmarkActionId.fromNativeSource(zero)
    )
    assertNotEquals(
      nativeZero,
      PostgreSqlGlobalCountBenchmarkActionId.fromNativeSource(one)
    )

    val action = PostgreSqlGlobalCountBenchmarkActions
      .produce(artifact(cycleSql, widthBound = 2, planLimit = 1), Vector(1))
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
      .actions
      .head
    assertNotEquals(
      action.actionId,
      PostgreSqlGlobalCountBenchmarkActionId.from(
        action.realization,
        action.candidateId,
        action.basePhysicalVariantId,
        action.renderer,
        one
      )
    )
  }

  test("action invariants bind the candidate and factor count to rendered plan") {
    val rewriteArtifact = artifact(cycleSql, widthBound = 2, planLimit = 5)
    val action = PostgreSqlGlobalCountBenchmarkActions
      .produce(rewriteArtifact, Vector(1))
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
      .actions
      .head
    val wrongCandidate = rewriteArtifact.candidateSearch.plans
      .find(_.candidateId != action.candidateId)
      .map(_.candidateId)
      .getOrElse(fail("expected another retained candidate"))
    val wrongPhysical = PostgreSqlPhysicalVariantId.fromV2(
      wrongCandidate,
      action.configuration,
      action.reductionMode,
      action.cteMaterializationDirective
    )
    val wrongActionId = PostgreSqlGlobalCountBenchmarkActionId.from(
      action.realization,
      wrongCandidate,
      wrongPhysical,
      action.renderer,
      action.renderedStatementSha256
    )

    intercept[IllegalArgumentException] {
      action.copy(
        candidateId = wrongCandidate,
        basePhysicalVariantId = wrongPhysical,
        actionId = wrongActionId
      )
    }
    intercept[IllegalArgumentException] {
      action.copy(factorCount = action.factorCount + 1)
    }
  }

  private def artifact(
      sql: String,
      widthBound: Int,
      planLimit: Int
  ): PostgreSqlRewriteArtifact = {
    val result = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = widthBound,
          requireConnectedCover = true,
          planLimit = planLimit,
          candidateSelection = CandidatePlanSelection.StructuralHead,
          reductionMode = YannakakisReductionMode.FullTwoPass
        )
      )
    )
    result.outcome match {
      case _: RewriteOutcome.Rewritten =>
        result.report.artifact.getOrElse(fail("missing rewrite artifact"))
      case other => fail(s"expected rewritten result, found $other")
    }
  }

  private def sha256(sql: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(sql.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def addTable(
      schema: org.apache.calcite.schema.SchemaPlus,
      name: String,
      columns: Vector[String]
  ): Unit =
    schema.add(
      name,
      new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType = {
          val builder = factory.builder()
          columns.foreach(column => builder.add(column, SqlTypeName.INTEGER))
          builder.build()
        }
      }
    )
}
