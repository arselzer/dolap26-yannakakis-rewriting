package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlRelevanceLimitedBaseColumn,
  PostgreSqlRelevanceLimitedBaseRelation,
  PostgreSqlRelevanceLimitedBaseTableEvidence,
  PostgreSqlRelevanceLimitedCountFailureClass,
  PostgreSqlRelevanceLimitedCountProfile
}
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

/** Identity and typed-disposition contracts for the additive V4 action. */
final class PostgreSqlRelevanceLimitedCountBenchmarkActionSuite
    extends munit.FunSuite {
  private val symmetricSql =
    """SELECT COUNT(*) AS exact_count
      |FROM v4_link AS l
      |JOIN v4_fact AS f1 ON f1.movie_id = l.left_key
      |JOIN v4_dim AS d1 ON d1.id = f1.dim_id
      |JOIN v4_fact AS f2 ON f2.movie_id = l.right_key
      |JOIN v4_dim AS d2 ON d2.id = f2.dim_id""".stripMargin

  private val triangleSql =
    """SELECT COUNT(*) AS exact_count
      |FROM tri_r AS r
      |JOIN tri_s AS s ON s.b = r.b
      |JOIN tri_t AS t ON t.c = s.c AND t.a = r.a""".stripMargin

  private val rewriter = {
    val schema = Frameworks.createRootSchema(true)
    addTable(schema, "v4_link", Vector("left_key", "right_key"))
    addTable(schema, "v4_fact", Vector("movie_id", "dim_id"))
    addTable(schema, "v4_dim", Vector("id"))
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

  test("action and profile identities bind candidate, evidence, and SQL") {
    val rewriteArtifact = artifact(symmetricSql, widthBound = 1)
    val firstEvidence = evidence(
      Vector("v4_link", "v4_fact", "v4_dim"),
      relationOidBase = 300L
    )
    val action = renderedAction(rewriteArtifact, firstEvidence)
    val repeated = renderedAction(rewriteArtifact, firstEvidence)

    assertEquals(
      PostgreSqlRelevanceLimitedCountBenchmarkAction.ProtocolId,
      "postgresql-relevance-limited-count-action/v1"
    )
    assertEquals(
      PostgreSqlRelevanceLimitedCountProfile.Scheme,
      "postgresql_relevance_limited_count_profile_v1"
    )
    assertEquals(
      PostgreSqlRelevanceLimitedCountProfile.AxisIds,
      Vector(
        "relevance-limited-shared-pendant-v1",
        "postgresql_relevance_limited_cse_v1",
        "distinct-anchor-interface-union-v1",
        "numeric-unit-sum-v1",
        "numeric-frequency-product-sum-bigint-check-v1",
        "fresh-unqualified-source-name-suffix-v1",
        "canonical-structural-head-v1",
        "ordinary-base-table-no-inheritance-no-rls-v1",
        "pg-attribute-built-in-int2-int4-int8-v1",
        "pg-catalog-built-in-integral-equality-chain-v1",
        "materialized"
      )
    )
    assertEquals(
      PostgreSqlRelevanceLimitedCountProfile.Id,
      "sha256:54d5b8aee676e3b028dfbc63a8c40b93229a5a30ae630d3ff6dd48779ba51875"
    )

    assertEquals(action, repeated)
    assertEquals(rewriteArtifact.selectedPlan.rank, 1)
    assertEquals(
      action.aliasId,
      f"global-count-r${rewriteArtifact.selectedPlan.rank}%02d-shared-v4-mat"
    )
    assertEquals(action.structuralRank, rewriteArtifact.selectedPlan.rank)
    assertEquals(action.candidateId, rewriteArtifact.selectedPlan.candidateId)
    assertEquals(action.factorCount, rewriteArtifact.bagFactorPlan.factors.size)
    assertEquals(action.baseTableEvidenceId, firstEvidence.id)
    assertEquals(
      action.actionId,
      PostgreSqlRelevanceLimitedCountActionId.from(
        action.candidateId,
        action.baseTableEvidenceId,
        action.renderedStatementSha256
      )
    )
    assertEquals(
      action.renderedStatementSha256,
      PostgreSqlRelevanceLimitedCountBenchmarkAction.sha256(
        action.renderedQuery.sql
      )
    )
    assertEquals(
      action.realizationId,
      PostgreSqlRelevanceLimitedCountProfile.RealizationId
    )
    assertEquals(action.profileId, PostgreSqlRelevanceLimitedCountProfile.Id)
    assertEquals(action.reductionMode, YannakakisReductionMode.NoReduction)
    assertEquals(
      action.cteMaterializationDirective,
      PostgreSqlRelevanceLimitedCountProfile.CteMaterialization
    )
    assertEquals(action.trainingEligible, false)
    assertEquals(action.reportable, false)

    val secondEvidence = evidence(
      Vector("v4_link", "v4_fact", "v4_dim"),
      relationOidBase = 1300L
    )
    val rebound = renderedAction(rewriteArtifact, secondEvidence)
    assertEquals(rebound.candidateId, action.candidateId)
    assertEquals(rebound.renderedQuery.sql, action.renderedQuery.sql)
    assertEquals(
      rebound.renderedStatementSha256,
      action.renderedStatementSha256
    )
    assertNotEquals(rebound.baseTableEvidenceId, action.baseTableEvidenceId)
    assertNotEquals(rebound.actionId, action.actionId)
  }

  test("a semantic detector miss is a typed skipped action") {
    val rewriteArtifact = artifact(triangleSql, widthBound = 2)
    val result = PostgreSqlRelevanceLimitedCountBenchmarkAction
      .produceSelected(
        rewriteArtifact,
        evidence(Vector("tri_r", "tri_s", "tri_t"), relationOidBase = 2300L)
      )
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

    val skipped = result match {
      case value: PostgreSqlRelevanceLimitedCountActionDisposition.Skipped =>
        value
      case other => fail(s"expected a typed skipped action, found $other")
    }
    assertEquals(skipped.structuralRank, rewriteArtifact.selectedPlan.rank)
    assertEquals(skipped.candidateId, rewriteArtifact.selectedPlan.candidateId)
    assertEquals(
      skipped.failure.failureClass,
      PostgreSqlRelevanceLimitedCountFailureClass.NotApplicable
    )
    assertEquals(
      skipped.failure.code,
      "relevance-limited-count.not-applicable.no-reusable-wing"
    )
  }

  test("a non-head selected candidate is a typed provenance rejection") {
    val rewriteArtifact = artifact(
      symmetricSql,
      widthBound = 1,
      candidateSelection = CandidatePlanSelection.StructuralRank(2)
    )
    assertEquals(rewriteArtifact.selectedPlan.rank, 2)

    val failure = PostgreSqlRelevanceLimitedCountBenchmarkAction
      .produceSelected(
        rewriteArtifact,
        evidence(
          Vector("v4_link", "v4_fact", "v4_dim"),
          relationOidBase = 3300L
        )
      )
      .left
      .getOrElse(fail("expected canonical-provenance rejection"))
    assertEquals(
      failure.failureClass,
      PostgreSqlRelevanceLimitedCountFailureClass.InvalidInput
    )
    assertEquals(
      failure.code,
      "relevance-limited-count.invalid.provenance-rank"
    )
  }

  private def renderedAction(
      artifact: PostgreSqlRewriteArtifact,
      baseTableEvidence: PostgreSqlRelevanceLimitedBaseTableEvidence
  ): PostgreSqlRelevanceLimitedCountAction =
    PostgreSqlRelevanceLimitedCountBenchmarkAction
      .produceSelected(artifact, baseTableEvidence)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity) match {
      case PostgreSqlRelevanceLimitedCountActionDisposition.Rendered(action) =>
        action
      case skipped => fail(s"expected a rendered V4 action, found $skipped")
    }

  private def artifact(
      sql: String,
      widthBound: Int,
      candidateSelection: CandidatePlanSelection =
        CandidatePlanSelection.StructuralHead
  ): PostgreSqlRewriteArtifact = {
    val result = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = widthBound,
          requireConnectedCover = true,
          planLimit = 3,
          candidateSelection = candidateSelection,
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

  private def evidence(
      names: Vector[String],
      relationOidBase: Long
  ): PostgreSqlRelevanceLimitedBaseTableEvidence = {
    val columnsByTable = Map(
      "v4_link" -> Vector("left_key", "right_key"),
      "v4_fact" -> Vector("movie_id", "dim_id"),
      "v4_dim" -> Vector("id"),
      "tri_r" -> Vector("a", "b"),
      "tri_s" -> Vector("b", "c"),
      "tri_t" -> Vector("c", "a")
    )
    val relations = names.zipWithIndex.map { case (name, ordinal) =>
      PostgreSqlRelevanceLimitedBaseRelation(
        Vector(name),
        databaseOid = 100L,
        namespaceOid = 200L,
        relationOid = relationOidBase + ordinal,
        relkind = "r",
        hasSubclass = false,
        rowSecurity = false,
        forceRowSecurity = false,
        columns = columnsByTable
          .getOrElse(name, fail(s"missing unit table $name"))
          .zipWithIndex
          .map { case (column, columnOrdinal) =>
            PostgreSqlRelevanceLimitedBaseColumn(
              column,
              attributeNumber = columnOrdinal + 1,
              typeOid = PostgreSqlRelevanceLimitedBaseColumn.IntegerOid,
              structural = true
            )
          }
      )
    }
    PostgreSqlRelevanceLimitedBaseTableEvidence
      .create("unit_v4", 180004, relations)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
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
