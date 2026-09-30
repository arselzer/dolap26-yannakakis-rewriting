package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateSearchInput,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlanningOracleTestSupport,
  BagFactorCoverAssignment,
  BagFactorPhysicalConfiguration,
  BagFactorPlanner,
  BagFactorPlanningLimits,
  ConfiguredBagFactorPlan,
  ConfiguredBagFactorPlanner
}
import at.ac.tuwien.dbai.yannakakis.calcite.{TestColumn, TestTable}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryIR
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCteMaterializationDirective,
  PostgreSqlFactorAwareContractTestSupport,
  PostgreSqlPhysicalPortfolioAxis,
  PostgreSqlPhysicalPortfolioEnumerator
}
import org.apache.calcite.sql.`type`.SqlTypeName

class PostgreSqlPhysicalDirectiveRenderingContractSuite
    extends munit.FunSuite {
  import PostgreSqlCteMaterializationDirective._
  import PostgreSqlFactorAwareContractTestSupport._
  import PostgreSqlRendererTestSupport._

  private final case class RenderingFixture(
      query: QueryIR,
      input: CandidateSearchInput,
      decomposition: CoveredDecomposition,
      ranked: Option[RankedCandidatePlan],
      configured: ConfiguredBagFactorPlan
  )

  private lazy val fusedFixture: RenderingFixture = {
    val prepared = q5
    prepared.found.plans.take(10).iterator
      .flatMap { ranked =>
        val configuration = structuralConfiguration(ranked.decomposition)
        ConfiguredBagFactorPlanner
          .plan(prepared.input, ranked.decomposition, configuration)
          .toOption
          .map(configured =>
            RenderingFixture(
              prepared.query,
              prepared.input,
              ranked.decomposition,
              Some(ranked),
              configured
            )
          )
      }
      .find { value =>
        val rendered = renderConfigured(
          value,
          YannakakisReductionMode.FullTwoPass,
          PlannerControlled
        )
        rendered.trace.stages.exists(stage =>
          stage.kind == PostgreSqlStageKind.FusedReduction &&
            stage.semijoinMembers.size > 1
        )
      }
      .getOrElse(
        fail("top-10 Q5 physical plans contain no fused-reduction fixture")
      )
  }

  private lazy val supportFixture: RenderingFixture = {
    val base = prepareTopOne(
      q2Sql,
      q2Tables,
      widthBound = 2,
      requireConnectedCover = true
    )
    val first = base.hypergraph.edges.head
    val decomposition = BagFactorPlanningOracleTestSupport.coveredDecomposition(
      root = 10,
      bags = Vector(
        BagFactorPlanningOracleTestSupport.coveredBag(
          10,
          first.vertices.map(_.value),
          Vector(first.id.value)
        ),
        BagFactorPlanningOracleTestSupport.coveredBag(
          20,
          base.hypergraph.variables.map(_.value),
          base.hypergraph.edges.take(2).map(_.id.value)
        )
      ),
      edges = Vector(BagFactorPlanningOracleTestSupport.treeEdge(10, 20))
    )
    val configured = ConfiguredBagFactorPlanner
      .plan(base.input, decomposition, structuralConfiguration(decomposition))
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
    RenderingFixture(
      base.query,
      base.input,
      decomposition,
      None,
      configured
    )
  }

  private lazy val alternateCoverFixture: RenderingFixture = {
    val prepared = q5
    val ranked = prepared.found.plans.find(plan =>
      plan.decomposition.bags.exists { bag =>
        prepared.input.candidateBag(bag.vertices).exists(
          _.physicalCovers.map(_.edgeIds).distinct.size > 1
        )
      }
    ).getOrElse(fail("Q5 exact-100 plans contain no alternate-cover fixture"))
    val configured = ConfiguredBagFactorPlanner
      .plan(
        prepared.input,
        ranked.decomposition,
        structuralConfiguration(ranked.decomposition)
      )
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
    RenderingFixture(
      prepared.query,
      prepared.input,
      ranked.decomposition,
      Some(ranked),
      configured
    )
  }

  test("planner-controlled, MATERIALIZED, and NOT MATERIALIZED apply to every emitted support, initial, and fused CTE and every diagnostic prefix") {
    val cases = Vector(
      PlannerControlled ->
        (" AS (", PostgreSqlStagePhysicalForm.PlannerControlledCte),
      Materialized ->
        (" AS MATERIALIZED (", PostgreSqlStagePhysicalForm.MaterializedCte),
      NotMaterialized ->
        (" AS NOT MATERIALIZED (", PostgreSqlStagePhysicalForm.NotMaterializedCte)
    )

    cases.foreach { case (directive, (syntax, expectedForm)) =>
      val diagnostics = Vector(supportFixture, fusedFixture).map(value =>
        diagnose(value, YannakakisReductionMode.FullTwoPass, directive)
      )
      val allNamedStages = diagnostics.flatMap(
        _.renderedQuery.trace.stages.filter(_.physicalForm.namedCte)
      )
      assert(
        allNamedStages.exists(_.kind == PostgreSqlStageKind.Support),
        clues(directive, allNamedStages)
      )
      assert(
        allNamedStages.exists(_.kind == PostgreSqlStageKind.InitialFactor),
        clues(directive, allNamedStages)
      )
      assert(
        allNamedStages.exists(stage =>
          stage.kind == PostgreSqlStageKind.FusedReduction &&
            stage.semijoinMembers.size > 1
        ),
        clues(directive, allNamedStages)
      )
      assert(allNamedStages.forall(_.physicalForm == expectedForm))

      diagnostics.foreach { diagnostic =>
        val rendered = diagnostic.renderedQuery
        val namedStages = rendered.trace.stages.filter(_.physicalForm.namedCte)
        val names = namedStages.flatMap(_.physicalCteName)
        assert(names.nonEmpty, clues(directive))
        names.foreach(name =>
          assert(
            rendered.sql.contains(s"  \"$name\"$syntax"),
            clues(directive, name, rendered.sql)
          )
        )
        directive match {
          case PlannerControlled =>
            assert(!rendered.sql.contains(" AS MATERIALIZED ("))
            assert(!rendered.sql.contains(" AS NOT MATERIALIZED ("))
          case Materialized =>
            assert(!rendered.sql.contains(" AS NOT MATERIALIZED ("))
          case NotMaterialized => ()
        }

        val emitted = rendered.trace.stages.filter(_.physicalForm.emitted)
        assertEquals(diagnostic.statements.map(_.stageId), emitted.map(_.id))
        assertEquals(diagnostic.statements.size, names.size + 1)
        diagnostic.statements.zipWithIndex.foreach { case (statement, index) =>
          val expectedPrefix = if (index < names.size) index + 1 else names.size
          assertEquals(statement.ctePrefixCount, expectedPrefix.toLong)
          names.take(expectedPrefix).foreach(name =>
            assert(
              statement.sql.contains(s"  \"$name\"$syntax"),
              clues(directive, index, name, statement.sql)
            )
          )
          names.drop(expectedPrefix).foreach(name =>
            assert(
              !statement.sql.contains(s"  \"$name\"$syntax"),
              clues(directive, index, name, statement.sql)
            )
          )
          assertEquals(statement.characterCount, statement.sql.length.toLong)
          assertEquals(statement.statementSha256, sha256(statement.sql))
        }

        val terminal = diagnostic.statements.last
        assertEquals(terminal.stageId, PostgreSqlStageId.FinalReconstruction)
        assertEquals(terminal.sql, rendered.sql)
        assertEquals(terminal.statementSha256, sha256(rendered.sql))
        assertEquals(
          rendered.trace.stages.last.physicalForm,
          PostgreSqlStagePhysicalForm.TerminalSelect
        )
      }
    }
  }

  test("planner-controlled configured anchors preserve legacy normal and stage-diagnostic bytes in every reduction mode") {
    val value = fusedFixture
    val legacyPlan = BagFactorPlanner
      .plan(value.input, value.decomposition)
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
    assertEquals(value.configured.plan, legacyPlan)

    YannakakisReductionMode.Values.foreach { mode =>
      val legacy = PostgreSqlBagFactorRenderer
        .renderWithReductionMode(value.query, value.input, legacyPlan, mode)
        .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
      val configured = renderConfigured(value, mode, PlannerControlled)
      assertEquals(configured.sql, legacy.sql, clues(mode))
      assertEquals(configured.trace, legacy.trace, clues(mode))
      assertEquals(configured.statistics, legacy.statistics, clues(mode))
      assertEquals(configured.output, legacy.output, clues(mode))

      val legacyDiagnostic = PostgreSqlBagFactorRenderer
        .renderWithStageDiagnostics(
          value.query,
          value.input,
          legacyPlan,
          PostgreSqlRenderingLimits.default,
          BagFactorPlanningLimits.default,
          mode,
          generousDiagnosticLimits
        )
        .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
      val configuredDiagnostic = diagnose(value, mode, PlannerControlled)
      assertEquals(configuredDiagnostic.statements, legacyDiagnostic.statements, clues(mode))
      assertEquals(configuredDiagnostic.renderedQuery.sql, legacyDiagnostic.renderedQuery.sql)
    }
  }

  test("alternate execution roots and admissible cover assignments are consumed by configured rendering without changing the structural candidate") {
    val value = alternateCoverFixture
    val ranked = value.ranked.getOrElse(fail("alternate-cover fixture lost its rank"))
    val one = ranked.copy(rank = 1)
    val enumeration = PostgreSqlPhysicalPortfolioEnumerator
      .enumerate(value.input, Vector(one))
      .fold(problem => fail(problem.message), identity)
    val alternateRoot = enumeration.entries.find(
      _.lane.contains(PostgreSqlPhysicalPortfolioAxis.AlternateRoot)
    ).getOrElse(fail("fixture has no alternate-root seed"))
    val alternateCover = enumeration.entries.find(
      _.lane.contains(PostgreSqlPhysicalPortfolioAxis.AlternateCover)
    ).getOrElse(fail("fixture has no alternate-cover seed"))
    val baseline = renderConfigured(
      value,
      YannakakisReductionMode.FullTwoPass,
      PlannerControlled
    )

    Vector(alternateRoot, alternateCover).foreach { entry =>
      val configured = ConfiguredBagFactorPlanner
        .plan(
          value.input,
          value.decomposition,
          entry.variant.configuration
        )
        .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
      val rendered = renderConfigured(
        value.copy(configured = configured),
        YannakakisReductionMode.FullTwoPass,
        PlannerControlled
      )
      assertEquals(configured.structuralDecomposition, value.decomposition)
      assertEquals(configured.plan.decomposition, value.decomposition)
      assertEquals(configured.plan.root, entry.variant.configuration.executionRoot)
      assertEquals(
        configured.plan.factors.map(factor =>
          BagFactorCoverAssignment(factor.bagId, factor.supportCover)
        ),
        entry.variant.configuration.coverAssignments
      )
      assert(rendered.sql != baseline.sql, clues(entry.lane, rendered.sql))
    }
  }

  test("one configured plan renders the exact full, bottom-up-only, and no-reduction subsets") {
    val rendered = YannakakisReductionMode.Values.map { mode =>
      mode -> renderConfigured(fusedFixture, mode, PlannerControlled)
    }
    assertEquals(rendered.map(_._2.sql).distinct.size, 3)

    rendered.foreach { case (mode, value) =>
      assertEquals(value.trace.reduction.mode, mode)
      mode match {
        case YannakakisReductionMode.FullTwoPass =>
          assertEquals(
            value.trace.reduction.selectedBottomUpStepCount,
            value.trace.reduction.canonicalBottomUpStepCount
          )
          assertEquals(
            value.trace.reduction.selectedTopDownStepCount,
            value.trace.reduction.canonicalTopDownStepCount
          )
        case YannakakisReductionMode.BottomUpOnly =>
          assertEquals(
            value.trace.reduction.selectedBottomUpStepCount,
            value.trace.reduction.canonicalBottomUpStepCount
          )
          assertEquals(value.trace.reduction.selectedTopDownStepCount, 0)
        case YannakakisReductionMode.NoReduction =>
          assertEquals(value.trace.reduction.selectedBottomUpStepCount, 0)
          assertEquals(value.trace.reduction.selectedTopDownStepCount, 0)
          assert(!value.trace.stages.exists(_.kind == PostgreSqlStageKind.FusedReduction))
      }
    }
  }

  test("configured renderer fails closed on independent-plan corruption and missing physical inputs") {
    val value = fusedFixture
    val good = value.configured
    val corrupt = good.copy(
      plan = good.plan.copy(ownership = good.plan.ownership.reverse)
    )
    val errors = PostgreSqlBagFactorRenderer
      .renderConfigured(
        value.query,
        value.input,
        corrupt,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        PlannerControlled
      )
      .fold(identity, _ => fail("corrupt configured plan unexpectedly rendered"))
    assertEquals(errors.map(_.code), Vector("postgresql.rendering.physical-plan.invalid"))
    errors.head match {
      case PostgreSqlRenderingError.ConfiguredPlanValidationFailure(problems) =>
        assert(problems.exists(_.code == "physical-lowering.validation.ownership"))
      case other => fail(s"unexpected configured rendering failure: $other")
    }

    val missing = PostgreSqlBagFactorRenderer
      .renderConfigured(
        null,
        null,
        null,
        null,
        null,
        null,
        null
      )
      .fold(identity, _ => fail("missing configured inputs unexpectedly rendered"))
    assertEquals(
      missing.map(_.code),
      Vector.fill(7)("postgresql.rendering.input.missing")
    )
  }

  test("explicit CTE directives expose the frozen PostgreSQL 12 version gate, scope, and strict parser") {
    assertEquals(MinimumExplicitServerVersionNum, 120000)
    assertEquals(Scope, "all-emitted-ctes-v1")
    assertEquals(
      Values.map(_.id),
      Vector("planner-controlled", "materialized", "not-materialized")
    )
    Values.foreach(value => assertEquals(parse(value.id), Some(value)))
    assertEquals(parse(null), None)
    assertEquals(parse("MATERIALIZED"), None)
    assertEquals(parse(" materialized"), None)
  }

  private def structuralConfiguration(
      decomposition: CoveredDecomposition
  ): BagFactorPhysicalConfiguration =
    BagFactorPhysicalConfiguration(
      decomposition.root,
      decomposition.bags.sortBy(_.id).map(bag =>
        BagFactorCoverAssignment(bag.id, bag.cover)
      )
    )

  private def renderConfigured(
      fixture: RenderingFixture,
      mode: YannakakisReductionMode,
      directive: PostgreSqlCteMaterializationDirective
  ): PostgreSqlRenderedQuery = {
    PostgreSqlBagFactorRenderer
      .renderConfigured(
        fixture.query,
        fixture.input,
        fixture.configured,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        mode,
        directive
      )
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
  }

  private def diagnose(
      fixture: RenderingFixture,
      mode: YannakakisReductionMode,
      directive: PostgreSqlCteMaterializationDirective
  ): PostgreSqlStageDiagnosticRendering = {
    PostgreSqlBagFactorRenderer
      .renderConfiguredWithStageDiagnostics(
        fixture.query,
        fixture.input,
        fixture.configured,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        mode,
        directive,
        generousDiagnosticLimits
      )
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)
  }

  private val q2Tables = Vector(
    typedTable("lsqb_q2_knows", "person1_id", "person2_id"),
    typedTable("lsqb_q2_comment", "creator_id", "reply_post_id"),
    typedTable("lsqb_q2_post", "post_id", "creator_id")
  )

  private val q2Sql =
    """SELECT
      |  k.person1_id AS "person1_id",
      |  k.person2_id AS "person2_id",
      |  c.reply_post_id AS "post_id"
      |FROM lsqb_q2_knows AS k
      |JOIN lsqb_q2_comment AS c ON k.person1_id = c.creator_id
      |JOIN lsqb_q2_post AS p
      |  ON c.reply_post_id = p.post_id AND p.creator_id = k.person2_id
      |""".stripMargin

  private def typedTable(
      name: String,
      first: String,
      second: String
  ): TestTable =
    TestTable(
      name,
      Vector(
        TestColumn("row_id", SqlTypeName.BIGINT, nullable = false),
        TestColumn(first, SqlTypeName.BIGINT, nullable = true),
        TestColumn(second, SqlTypeName.BIGINT, nullable = true)
      )
    )

  private val generousDiagnosticLimits: PostgreSqlStageDiagnosticLimits =
    PostgreSqlStageDiagnosticLimits
      .create(
        maxStatements = 1000L,
        maxCharactersPerStatement = 16777216L,
        maxTotalCharacters = 67108864L
      )
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)

  private def sha256(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
