package at.ac.tuwien.dbai.yannakakis.api.postgresql

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

final class PostgreSqlAggregatePhysicalSweepV3Suite extends munit.FunSuite {
  private val countSql =
    "SELECT COUNT(*) AS cycle_rows " +
      "FROM win_r AS r " +
      "JOIN win_s AS s ON r.x = s.x " +
      "JOIN win_t AS t ON s.y = t.y " +
      "JOIN win_u AS u ON t.z = u.z AND u.w = r.w"

  private val u1Sql =
    "SELECT e0.right_key AS group_key_0, " +
      "e2.right_key AS group_key_1, COUNT(*) AS witness_count " +
      "FROM synthetic_u1_chain_e0 AS e0 " +
      "JOIN synthetic_u1_chain_e1 AS e1 " +
      "ON e0.right_key = e1.left_key " +
      "JOIN synthetic_u1_chain_e2 AS e2 " +
      "ON e1.right_key = e2.left_key " +
      "JOIN synthetic_u1_chain_e3 AS e3 " +
      "ON e2.right_key = e3.left_key " +
      "GROUP BY e0.right_key, e2.right_key"

  private val minSql =
    "SELECT MIN(r0.s) AS min_guard_key " +
      "FROM td_r0 AS r0 " +
      "JOIN td_r1 AS r1 ON r1.s = r0.s " +
      "JOIN td_r2 AS r2 ON r2.s = r0.d " +
      "JOIN td_r3 AS r3 ON r3.s = r1.d AND r3.d = r2.d " +
      "JOIN td_r4 AS r4 ON r4.s = r3.d " +
      "JOIN td_r5 AS r5 ON r5.s = r4.s " +
      "JOIN td_r6 AS r6 ON r6.s = r4.d AND r6.d = r5.d"

  private val maxSql = minSql.replace("MIN(r0.s)", "MAX(r0.s)").replace(
    "min_guard_key",
    "max_guard_key"
  )

  private val lsqbMinSql =
    "SELECT MIN(pkp1.Person1Id) " +
      "FROM City AS CityA " +
      "JOIN City AS CityB ON CityB.isPartOf_CountryId = CityA.isPartOf_CountryId " +
      "JOIN City AS CityC ON CityC.isPartOf_CountryId = CityA.isPartOf_CountryId " +
      "JOIN Person AS PersonA ON PersonA.isLocatedIn_CityId = CityA.CityId " +
      "JOIN Person AS PersonB ON PersonB.isLocatedIn_CityId = CityB.CityId " +
      "JOIN Person_knows_Person AS pkp1 " +
      "ON pkp1.Person1Id = PersonA.PersonId " +
      "AND pkp1.Person2Id = PersonB.PersonId"

  private val rewriter = {
    val schema = Frameworks.createRootSchema(true)
    addTable(schema, "win_r", Vector("x", "w"))
    addTable(schema, "win_s", Vector("x", "y"))
    addTable(schema, "win_t", Vector("y", "z"))
    addTable(schema, "win_u", Vector("z", "w"))
    (0 until 4).foreach(index =>
      addTable(
        schema,
        s"synthetic_u1_chain_e$index",
        Vector("left_key", "right_key")
      )
    )
    (0 until 7).foreach(index =>
      addTable(schema, s"td_r$index", Vector("s", "d"))
    )
    addTable(schema, "city", Vector("cityid", "ispartof_countryid"))
    addTable(schema, "person", Vector("personid", "islocatedin_cityid"))
    addTable(
      schema,
      "person_knows_person",
      Vector("person1id", "person2id")
    )
    val planner = CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
    PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
  }

  test("global COUNT exports only ranks 1/5 and full planner/materialized") {
    assertSweep(
      countSql,
      PostgreSqlAggregatePhysicalSweepV3Family.GlobalCount,
      Vector(
        "global-count-r01-full-pc",
        "global-count-r01-full-mat",
        "global-count-r05-full-pc",
        "global-count-r05-full-mat"
      ),
      Vector(1, 1, 5, 5),
      Vector.fill(4)(YannakakisReductionMode.FullTwoPass),
      "global-count-star"
    )
  }

  test("U1 exports only ranks 1/10 and full planner/materialized") {
    assertSweep(
      u1Sql,
      PostgreSqlAggregatePhysicalSweepV3Family.DistributedGroupedCount,
      Vector(
        "u1-distributed-count-r01-full-pc",
        "u1-distributed-count-r01-full-mat",
        "u1-distributed-count-r10-full-pc",
        "u1-distributed-count-r10-full-mat"
      ),
      Vector(1, 1, 10, 10),
      Vector.fill(4)(YannakakisReductionMode.FullTwoPass),
      "distributed-grouped-count-star"
    )
  }

  test("guarded MIN exports only ranks 1/2 and full/bottom-up planner") {
    assertSweep(
      minSql,
      PostgreSqlAggregatePhysicalSweepV3Family.GuardedMin,
      Vector(
        "guarded-min-r01-full-pc",
        "guarded-min-r01-bu-pc",
        "guarded-min-r02-full-pc",
        "guarded-min-r02-bu-pc"
      ),
      Vector(1, 1, 2, 2),
      Vector(
        YannakakisReductionMode.FullTwoPass,
        YannakakisReductionMode.BottomUpOnly,
        YannakakisReductionMode.FullTwoPass,
        YannakakisReductionMode.BottomUpOnly
      ),
      "global-guarded-extremum"
    )
  }

  test("guarded MAX exports only ranks 1/2 and full/bottom-up planner") {
    assertSweep(
      maxSql,
      PostgreSqlAggregatePhysicalSweepV3Family.GuardedMax,
      Vector(
        "guarded-max-r01-full-pc",
        "guarded-max-r01-bu-pc",
        "guarded-max-r02-full-pc",
        "guarded-max-r02-bu-pc"
      ),
      Vector(1, 1, 2, 2),
      Vector(
        YannakakisReductionMode.FullTwoPass,
        YannakakisReductionMode.BottomUpOnly,
        YannakakisReductionMode.FullTwoPass,
        YannakakisReductionMode.BottomUpOnly
      ),
      "global-guarded-extremum"
    )
  }

  test("guarded MIN admits the explicit width-3 connected-cover LSQB arm") {
    val rewriteArtifact = artifact(lsqbMinSql, widthBound = 3)
    assertEquals(rewriteArtifact.admissibleGeneration.widthBound, 3)
    assert(rewriteArtifact.admissibleGeneration.requireConnectedCover)
    assertEquals(rewriteArtifact.admissibleGeneration.candidates.size, 31)
    assertEquals(rewriteArtifact.candidateSearch.plans.size, 10)
    val value = PostgreSqlAggregatePhysicalSweepV3
      .produce(
        rewriteArtifact,
        PostgreSqlAggregatePhysicalSweepV3Family.GuardedMin
      )
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
    assertEquals(value.actions.map(_.structuralRank), Vector(1, 1, 2, 2))
    assertEquals(value.actions.head.sql, rewriteArtifact.renderedQuery.sql)
  }

  test("family mismatch is a typed failure") {
    val result = PostgreSqlAggregatePhysicalSweepV3.produce(
      artifact(minSql),
      PostgreSqlAggregatePhysicalSweepV3Family.GuardedMax
    )
    assertEquals(
      result.left.toOption.map(_.code),
      Some("aggregate-physical-v3.family.mismatch")
    )
  }

  private def assertSweep(
      sql: String,
      family: PostgreSqlAggregatePhysicalSweepV3Family,
      aliases: Vector[String],
      ranks: Vector[Int],
      modes: Vector[YannakakisReductionMode],
      resultShape: String
  ): Unit = {
    val rewriteArtifact = artifact(sql)
    val value = PostgreSqlAggregatePhysicalSweepV3
      .produce(rewriteArtifact, family)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
    assertEquals(value.protocolId, PostgreSqlAggregatePhysicalSweepV3.ProtocolId)
    assertEquals(value.resultShapeId, resultShape)
    assertEquals(
      value.aggregateRenderingProfileId,
      PostgreSqlAggregatePhysicalSweepV3.AggregateProfileId
    )
    assertEquals(value.actions.map(_.aliasId), aliases)
    assertEquals(value.actions.map(_.structuralRank), ranks)
    assertEquals(value.actions.map(_.reductionMode), modes)
    assertEquals(value.actions.map(_.physicalVariantId).distinct.size, 4)
    assert(value.actions.forall(_.sql.nonEmpty))
    assert(value.actions.forall(_.renderedQuery.output.nonEmpty))
    assertEquals(
      value.actions.map(_.candidateId).distinct.size,
      family.requiredRanks.size
    )
    assertEquals(value.actions.head.sql, rewriteArtifact.renderedQuery.sql)
  }

  private def artifact(
      sql: String,
      widthBound: Int = 2
  ): PostgreSqlRewriteArtifact = {
    val result = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = widthBound,
          requireConnectedCover = true,
          planLimit = PostgreSqlAggregatePhysicalSweepV3.RequiredPlanLimit,
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
