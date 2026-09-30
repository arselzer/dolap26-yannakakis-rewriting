package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

class PostgreSqlEnumerationPhysicalFactorialV3Suite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("current batch yields the exact stable 3x3 same-plan factorial") {
    val batch = producedBatch(planLimit = 10)
    val first = PostgreSqlEnumerationPhysicalFactorialV3
      .produce(batch, structuralRank = 2)
      .fold(problem => fail(s"physical factorial failed: $problem"), identity)
    val replay = PostgreSqlEnumerationPhysicalFactorialV3
      .produce(batch, structuralRank = 2)
      .fold(problem => fail(s"physical factorial replay failed: $problem"), identity)

    assertEquals(
      first.protocolId,
      PostgreSqlEnumerationPhysicalFactorialV3.ProtocolId
    )
    assertEquals(first.renderingProfileId, "statistics-safe-v2")
    assertEquals(first.structuralRank, 2)
    assertEquals(first.decompositionId, first.candidateId)
    assertEquals(first.actions.map(_.ordinal), (1 to 9).toVector)
    assertEquals(
      first.actions.map(action =>
        action.reductionMode -> action.cteMaterializationDirective
      ),
      Vector(
        YannakakisReductionMode.FullTwoPass ->
          PostgreSqlCteMaterializationDirective.PlannerControlled,
        YannakakisReductionMode.FullTwoPass ->
          PostgreSqlCteMaterializationDirective.Materialized,
        YannakakisReductionMode.FullTwoPass ->
          PostgreSqlCteMaterializationDirective.NotMaterialized,
        YannakakisReductionMode.BottomUpOnly ->
          PostgreSqlCteMaterializationDirective.PlannerControlled,
        YannakakisReductionMode.BottomUpOnly ->
          PostgreSqlCteMaterializationDirective.Materialized,
        YannakakisReductionMode.BottomUpOnly ->
          PostgreSqlCteMaterializationDirective.NotMaterialized,
        YannakakisReductionMode.NoReduction ->
          PostgreSqlCteMaterializationDirective.PlannerControlled,
        YannakakisReductionMode.NoReduction ->
          PostgreSqlCteMaterializationDirective.Materialized,
        YannakakisReductionMode.NoReduction ->
          PostgreSqlCteMaterializationDirective.NotMaterialized
      )
    )
    assertEquals(first.actions.map(_.renderingProfileId).distinct, Vector("statistics-safe-v2"))
    assertEquals(first.actions.map(_.physicalVariantId).distinct.size, 9)
    assertEquals(first.actions, replay.actions)
    assertEquals(first.configuration, replay.configuration)

    val retained = batch.artifact.get.batch.candidates(1)
    val canonicalAnchor = first.actions.head
    assertEquals(first.canonicalBagFactorPlan, retained.bagFactorPlan)
    assertEquals(canonicalAnchor.renderedQuery, retained.renderedQuery)
    assertEquals(canonicalAnchor.stageDiagnostics, retained.stageDiagnostics)

    first.actions.foreach { action =>
      assertEquals(action.renderedQuery.bagFactorPlan, first.canonicalBagFactorPlan)
      assertEquals(
        action.renderedStatementSha256,
        PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(action.sql)
      )
      assertEquals(
        action.stageDiagnostics.statements.last.statementSha256,
        action.renderedStatementSha256
      )
      assertEquals(
        action.physicalVariantId,
        PostgreSqlPhysicalVariantId.fromV2(
          first.candidateId,
          first.configuration,
          action.reductionMode,
          action.cteMaterializationDirective
        )
      )
    }
  }

  test("legacy-v1 retained bytes cannot enter the current-v3 factorial") {
    val request = batchRequest(planLimit = 1)
    val legacy = standardRewriter().produceEnumerationBatchLegacyV2(request)
    assertEquals(legacy.status, PostgreSqlEnumerationBatchStatus.Produced)

    val failure = PostgreSqlEnumerationPhysicalFactorialV3
      .produce(legacy, structuralRank = 1)
      .swap
      .toOption
      .getOrElse(fail("legacy enumeration batch was accepted"))

    assertEquals(failure.code, "physical-factorial-v3.profile.mismatch")
  }

  test("non-produced batches and invalid or absent ranks fail without partial actions") {
    val produced = producedBatch(planLimit = 1)
    assertEquals(
      PostgreSqlEnumerationPhysicalFactorialV3
        .produce(produced, 0)
        .left
        .toOption
        .map(_.code),
      Some("physical-factorial-v3.rank.invalid")
    )
    assertEquals(
      PostgreSqlEnumerationPhysicalFactorialV3
        .produce(produced, 2)
        .left
        .toOption
        .map(_.code),
      Some("physical-factorial-v3.rank.not-retained")
    )

    val invalidRequest = PostgreSqlEnumerationBatchRequest
      .create("   ")
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    val invalid = standardRewriter().produceEnumerationBatch(invalidRequest)
    val failure = PostgreSqlEnumerationPhysicalFactorialV3
      .produce(invalid, 1)
      .swap
      .toOption
      .getOrElse(fail("non-produced batch was accepted"))
    assertEquals(failure.code, "physical-factorial-v3.batch.not-produced")
  }

  test("the producer reuses the captured renderer limits and fails closed") {
    val batch = producedBatch(planLimit = 1)
    val originalArtifact = batch.artifact.get
    val restrictiveContract = batch.producerSearchContract.copy(
      rendering = batch.producerSearchContract.rendering.copy(
        maxSqlCharacters = 1L
      )
    )
    val hostile = batch.copy(
      producerSearchContract = restrictiveContract,
      artifact = Some(
        originalArtifact.copy(producerSearchContract = restrictiveContract)
      )
    )

    val failure = PostgreSqlEnumerationPhysicalFactorialV3
      .produce(hostile, 1)
      .swap
      .toOption
      .getOrElse(fail("restrictive captured renderer limit was ignored"))
    assertEquals(failure.code, "physical-factorial-v3.rendering.failed")
    assert(failure.detail.contains("action=1"), clues(failure))
    assert(failure.detail.contains("mode=full"), clues(failure))
    assert(failure.detail.contains("directive=planner-controlled"), clues(failure))
  }

  private def producedBatch(planLimit: Int): PostgreSqlEnumerationBatchResult = {
    val result = standardRewriter().produceEnumerationBatch(
      batchRequest(planLimit)
    )
    assertEquals(result.status, PostgreSqlEnumerationBatchStatus.Produced)
    result
  }

  private def batchRequest(
      planLimit: Int
  ): PostgreSqlEnumerationBatchRequest =
    PostgreSqlEnumerationBatchRequest
      .create(triangleSql, planLimit = planLimit)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)

  private def standardRewriter(): PostgreSqlQueryRewriter = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(sql => Right(fixture.plan(sql).root))
      .fold(problem => fail(problem.message), identity)
    PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
  }
}
