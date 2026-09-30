package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlanningLimits,
  BagFactorPlanningOracleTestSupport,
  SemijoinStep
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.calcite.{TestColumn, TestTable}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.CoveredDecomposition
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  DistributedGroupedCountStarIR,
  FilterIR,
  GlobalCountStarIR,
  GlobalGuardedExtremumIR,
  GroupedCountStarIR,
  JoinIR,
  ProjectIR,
  QueryIR,
  RelationOccurrence,
  RelationalIR,
  TableScanIR,
  UnsupportedRelationalIR
}
import org.apache.calcite.sql.`type`.SqlTypeName

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import PostgreSqlRendererTestSupport._

class PostgreSqlStageTraceSuite extends munit.FunSuite {
  test("stage replay schemes are explicit and stable") {
    assertEquals(
      PostgreSqlStageId.Scheme,
      "postgresql_renderer_stage_id_v1"
    )
    assertEquals(
      PostgreSqlStageTrace.OrderScheme,
      "postgresql_cte_emission_then_terminal_v1"
    )
    assertEquals(
      PostgreSqlStageDiagnosticStatement.HashScheme,
      "sha256_utf8_v1"
    )
    assertEquals(
      PostgreSqlStageDiagnosticStatement.CharacterCountScheme,
      "jvm_utf16_code_units_v1"
    )
  }

  test("adapted Q2 one-bag trace has stable elided support and terminal identities") {
    val prepared = q2OneBagPrepared
    val rendered = renderMode(prepared, YannakakisReductionMode.FullTwoPass)
    val stages = rendered.trace.stages
    val bagId = prepared.plan.factors.head.bagId
    val supportId = PostgreSqlStageId.BagSupport(bagId)
    val factorId = PostgreSqlStageId.BagFactorVersion(bagId, 0)

    assertEquals(prepared.plan.factors.size, 1)
    assertEquals(
      stages.map(_.id),
      Vector(
        supportId,
        factorId,
        PostgreSqlStageId.FinalReconstruction
      )
    )
    assertEquals(
      stages.map(_.kind),
      Vector(
        PostgreSqlStageKind.Support,
        PostgreSqlStageKind.InitialFactor,
        PostgreSqlStageKind.FinalReconstruction
      )
    )
    assertEquals(stages.head.physicalForm, PostgreSqlStagePhysicalForm.Elided)
    assertEquals(stages.head.physicalCteName, None)
    assertEquals(stages(1).inputStageIds, Vector.empty)
    assertEquals(stages.last.inputStageIds, Vector(factorId))
    assertEquals(stages.last.logicalBagIds, Vector(bagId))
    assertEquals(stages.map(_.id.code).distinct.size, stages.size)
    assertEquals(namedCteNames(rendered), Vector(stages(1).physicalCteName.get))
    assertPhysicalNamesFollowSql(rendered)
    assertTopological(rendered)
    assert(!stages.exists(_.physicalForm.explicitlyMaterialized))
  }

  test("mixed support records emitted planner-controlled CTEs and exact elision") {
    val prepared = q2MixedPrepared
    val rendered = renderMode(prepared, YannakakisReductionMode.FullTwoPass)
    val factors = prepared.plan.factors.map(factor => factor.bagId -> factor).toMap
    val supports = rendered.trace.stages.filter(
      _.kind == PostgreSqlStageKind.Support
    )
    val initials = rendered.trace.stages.filter(
      _.kind == PostgreSqlStageKind.InitialFactor
    )

    assert(supports.exists(
      _.physicalForm == PostgreSqlStagePhysicalForm.PlannerControlledCte
    ))
    assert(supports.exists(
      _.physicalForm == PostgreSqlStagePhysicalForm.Elided
    ))
    supports.foreach { support =>
      val bagId = support.logicalBagIds.head
      val factor = factors(bagId)
      val initial = initials.find(_.logicalBagIds == Vector(bagId)).get
      assertEquals(support.logicalEdgeIds, factor.supportCover.sorted)
      support.physicalForm match {
        case PostgreSqlStagePhysicalForm.PlannerControlledCte =>
          assert(support.physicalCteName.nonEmpty)
          assertEquals(initial.inputStageIds, Vector(support.id))
        case PostgreSqlStagePhysicalForm.Elided =>
          assertEquals(support.physicalCteName, None)
          assertEquals(initial.inputStageIds, Vector.empty)
        case other => fail(s"unexpected support form $other")
      }
    }
    initials.foreach { initial =>
      val factor = factors(initial.logicalBagIds.head)
      assertEquals(initial.logicalEdgeIds, factor.ownedEdgeIds.sorted)
    }
    assertPhysicalNamesFollowSql(rendered)
    assertTopological(rendered)
  }

  test("Q5-derived fused stages retain every logical member and target-local version") {
    val prepared = q5FusedPrepared
    val rendered = renderMode(prepared, YannakakisReductionMode.FullTwoPass)
    val reductions = rendered.trace.stages.filter(
      _.kind == PostgreSqlStageKind.FusedReduction
    )
    val fused = reductions.filter(_.semijoinMembers.size > 1)

    assert(fused.nonEmpty, clues(reductions))
    fused.foreach { stage =>
      val target = stage.semijoinMembers.head.target
      assert(stage.semijoinMembers.forall(_.target == target))
      assertEquals(stage.logicalBagIds.head, target)
      assertEquals(
        stage.semijoinMembers.map(member =>
          (
            member.pass,
            member.ordinal,
            member.target,
            member.source,
            member.separator
          )
        ),
        rendered.trace.semijoins
          .filter(_.outputCte == stage.physicalCteName.get)
          .map(trace =>
            (
              trace.pass,
              trace.ordinal,
              trace.target,
              trace.source,
              prepared.plan.schedule.bottomUp
                .concat(prepared.plan.schedule.topDown)
                .find(step =>
                  step.pass == trace.pass && step.ordinal == trace.ordinal
                )
                .get
                .separator
            )
          )
      )
      assertEquals(stage.inputStageIds.size, stage.semijoinMembers.size + 1)
    }
    assertContiguousVersions(rendered)
    assertPhysicalNamesFollowSql(rendered)
    assertTopological(rendered)
  }

  test("all reduction modes expose exact canonical subsets and one terminal stage") {
    val prepared = q5FusedPrepared
    val cases = Vector(
      YannakakisReductionMode.FullTwoPass ->
        (prepared.plan.schedule.bottomUp ++ prepared.plan.schedule.topDown),
      YannakakisReductionMode.BottomUpOnly -> prepared.plan.schedule.bottomUp,
      YannakakisReductionMode.NoReduction -> Vector.empty[SemijoinStep]
    )

    cases.foreach { case (mode, selectedSteps) =>
      val rendered = renderMode(prepared, mode)
      val stages = rendered.trace.stages
      val actualMembers = stages.flatMap(_.semijoinMembers)
      assertEquals(
        actualMembers.map(member =>
          (member.pass, member.ordinal, member.target, member.source, member.separator)
        ),
        selectedSteps.map(step =>
          (step.pass, step.ordinal, step.target, step.source, step.separator)
        ),
        clues(mode)
      )
      assertEquals(
        stages.count(_.id == PostgreSqlStageId.FinalReconstruction),
        1,
        clues(mode)
      )
      assertEquals(stages.last.id, PostgreSqlStageId.FinalReconstruction)
      assertEquals(
        stages.count(_.kind == PostgreSqlStageKind.Support),
        prepared.plan.factors.size
      )
      assertEquals(
        stages.count(_.kind == PostgreSqlStageKind.InitialFactor),
        prepared.plan.factors.size
      )
      assertEquals(
        namedCteNames(rendered).size.toLong,
        rendered.statistics.cteCount
      )
      val elided = stages.count(
        _.physicalForm == PostgreSqlStagePhysicalForm.Elided
      )
      assertEquals(
        stages.size.toLong,
        rendered.statistics.cteCount + elided.toLong + 1L
      )
      assertContiguousVersions(rendered)
      assertPhysicalNamesFollowSql(rendered)
      assertTopological(rendered)
    }
  }

  test("canonical trace is invariant to decomposition input permutations") {
    val prepared = q5FusedPrepared
    val baseline = renderMode(prepared, YannakakisReductionMode.FullTwoPass)
    val permutedDecomposition = CoveredDecomposition
      .create(
        prepared.decomposition.root,
        prepared.decomposition.bags.reverse,
        prepared.decomposition.treeEdges.reverse
      )
      .fold(
        errors => fail(errors.map(_.message).mkString("; ")),
        identity
      )
    val replanned = withDecomposition(
      prepared.report,
      prepared.hypergraph,
      prepared.input,
      permutedDecomposition
    )
    val decompositionPermutation = renderMode(
      replanned,
      YannakakisReductionMode.FullTwoPass
    )
    assertEquals(permutedDecomposition, prepared.decomposition)
    assertEquals(decompositionPermutation.sql, baseline.sql)
    assertEquals(decompositionPermutation.trace, baseline.trace)
  }

  test("physical name collisions and corrupted stage provenance fail closed") {
    val prepared = q5FusedPrepared
    val baseline = renderMode(prepared, YannakakisReductionMode.FullTwoPass)
    val firstPhysicalName = namedCteNames(baseline).head
    val firstRelation = prepared.query.relations.head.id
    val collisionQuery = replaceRelations(prepared.query) { relation =>
      if (relation.id == firstRelation)
        relation.copy(qualifiedName = Vector(firstPhysicalName))
      else relation
    }
    val collision = PostgreSqlBagFactorRenderer.renderWithReductionMode(
      collisionQuery,
      prepared.input,
      prepared.plan,
      YannakakisReductionMode.FullTwoPass
    )
    assert(
      collision.swap.toOption.get.exists(
        _.isInstanceOf[PostgreSqlRenderingError.GeneratedNameCollision]
      )
    )

    val compiled = compile(prepared, YannakakisReductionMode.FullTwoPass)
    val valid = compiled.trace.stages
    val duplicateId = valid.updated(1, valid(1).copy(id = valid.head.id))
    val initialIndex = valid.indexWhere(
      _.kind == PostgreSqlStageKind.InitialFactor
    )
    val futureInput = valid.updated(
      initialIndex,
      valid(initialIndex).copy(
        inputStageIds = Vector(PostgreSqlStageId.FinalReconstruction)
      )
    )
    val reductionIndex = valid.indexWhere(stage =>
      stage.kind == PostgreSqlStageKind.FusedReduction &&
        stage.semijoinMembers.size > 1
    )
    assert(reductionIndex >= 0)
    val reduction = valid(reductionIndex)
    val reductionId = reduction.id
      .asInstanceOf[PostgreSqlStageId.BagFactorVersion]
    val versionGap = valid.updated(
      reductionIndex,
      reduction.copy(
        id = PostgreSqlStageId.BagFactorVersion(
          reductionId.bagId,
          reductionId.version + 1
        )
      )
    )
    val corruptMember = reduction.semijoinMembers.head
    val memberProvenance = valid.updated(
      reductionIndex,
      reduction.copy(
        semijoinMembers = reduction.semijoinMembers.updated(
          0,
          corruptMember.copy(source = corruptMember.target)
        )
      )
    )
    Vector(duplicateId, futureInput, versionGap, memberProvenance, valid.dropRight(1))
      .foreach { stages =>
        val errors = validate(compiled, prepared, stages)
        assertEquals(errors.size, 1)
        errors.head match {
          case PostgreSqlRenderingError.ModelInvariantViolation(detail) =>
            assert(detail.contains("stage trace validation"), clues(detail))
          case other => fail(s"unexpected trace validation error: $other")
        }
      }
  }

  test("stage evidence preserves the exact physical CTE resource boundary") {
    val prepared = q5FusedPrepared
    val baseline = renderMode(prepared, YannakakisReductionMode.FullTwoPass)
    val exactCtes = baseline.statistics.cteCount
    assert(exactCtes > 1L)
    val exactLimits = limitsWithCtes(exactCtes)
    val exact = PostgreSqlBagFactorRenderer.renderWithReductionMode(
      prepared.query,
      prepared.input,
      prepared.plan,
      exactLimits,
      YannakakisReductionMode.FullTwoPass
    )
    assertEquals(exact.map(_.sql), Right(baseline.sql))
    assertEquals(exact.map(_.trace), Right(baseline.trace))

    val below = PostgreSqlBagFactorRenderer.renderWithReductionMode(
      prepared.query,
      prepared.input,
      prepared.plan,
      limitsWithCtes(exactCtes - 1L),
      YannakakisReductionMode.FullTwoPass
    )
    val errors = below.swap.toOption.getOrElse(fail("expected CTE limit failure"))
    assertEquals(
      errors,
      Vector(
        PostgreSqlRenderingError.ResourceLimitExceeded(
          PostgreSqlRenderingResource.Ctes,
          BigInt(exactCtes),
          exactCtes - 1L
        )
      )
    )
  }

  test("diagnostic rendering binds canonical emitted prefixes and exact normal SQL") {
    val prepared = q2MixedPrepared
    val normal = renderMode(prepared, YannakakisReductionMode.FullTwoPass)
    val diagnostic = diagnose(
      prepared,
      YannakakisReductionMode.FullTwoPass,
      generousDiagnosticLimits
    )
    val emitted = normal.trace.stages.filter(_.physicalForm.emitted)
    val elided = normal.trace.stages.filter(
      _.physicalForm == PostgreSqlStagePhysicalForm.Elided
    )

    assertEquals(diagnostic.renderedQuery, normal)
    assertEquals(diagnostic.statements.map(_.stageId), emitted.map(_.id))
    assert(
      diagnostic.statements.forall(statement =>
        !elided.exists(_.id == statement.stageId)
      )
    )
    val namedStages = emitted.dropRight(1)
    val physicalNames = namedStages.map(_.physicalCteName.get)
    diagnostic.statements.dropRight(1).zipWithIndex.foreach {
      case (statement, index) =>
        assertEquals(statement.ctePrefixCount, (index + 1).toLong)
        assertEquals(statement.characterCount, statement.sql.length.toLong)
        assertEquals(statement.statementSha256, independentSha256(statement.sql))
        assert(statement.sql.startsWith("WITH\n"), clues(statement.sql))
        assert(
          statement.sql.endsWith(
            s"\nSELECT * FROM \"${physicalNames(index)}\";"
          ),
          clues(statement.sql)
        )
        physicalNames.take(index + 1).foreach { name =>
          assert(
            statement.sql.contains(s"  \"$name\" AS ("),
            clues(name, index, statement.sql)
          )
        }
        physicalNames.drop(index + 1).foreach { name =>
          assert(
            !statement.sql.contains(s"  \"$name\" AS ("),
            clues(name, index, statement.sql)
          )
        }
    }
    val terminal = diagnostic.statements.last
    assertEquals(terminal.stageId, PostgreSqlStageId.FinalReconstruction)
    assertEquals(terminal.sql, normal.sql)
    assert(terminal.sql eq diagnostic.renderedQuery.sql)
    assertEquals(terminal.statementSha256, independentSha256(normal.sql))
    assertEquals(terminal.characterCount, normal.sql.length.toLong)
    assertEquals(terminal.ctePrefixCount, physicalNames.size.toLong)
  }

  test("diagnostic statements cover fused Q5-derived stages in every mode") {
    val prepared = q5FusedPrepared
    YannakakisReductionMode.Values.foreach { mode =>
      val normal = renderMode(prepared, mode)
      val diagnostic = diagnose(prepared, mode, generousDiagnosticLimits)
      assertEquals(diagnostic.renderedQuery, normal)
      assertEquals(
        diagnostic.statements.size.toLong,
        normal.statistics.cteCount + 1L,
        clues(mode)
      )
      assertEquals(
        diagnostic.statements.map(_.ctePrefixCount),
        (1L to normal.statistics.cteCount).toVector :+
          normal.statistics.cteCount,
        clues(mode)
      )
      assertEquals(diagnostic.statements.last.sql, normal.sql)
      assert(
        diagnostic.statements.forall(!_.sql.startsWith("EXPLAIN")),
        clues(mode)
      )
      val fusedIds = normal.trace.stages.collect {
        case stage
            if stage.kind == PostgreSqlStageKind.FusedReduction &&
              stage.semijoinMembers.size > 1 => stage.id
      }
      fusedIds.foreach { id =>
        val statement = diagnostic.statements.find(_.stageId == id).get
        assert(statement.sql.contains("AND EXISTS ("), clues(mode, statement.sql))
      }
    }
  }

  test("diagnostic count, per-statement, and total-character limits are exact") {
    val prepared = q5FusedPrepared
    val mode = YannakakisReductionMode.FullTwoPass
    val baseline = diagnose(prepared, mode, generousDiagnosticLimits)
    val count = baseline.statements.size.toLong
    val maximum = baseline.statements.map(_.characterCount).max
    val total = baseline.statements.map(_.characterCount).sum
    val exactLimits = diagnosticLimits(count, maximum, total)
    val exact = rawDiagnostic(prepared, mode, exactLimits)
    assertEquals(exact, Right(baseline))

    val cases = Vector(
      diagnosticLimits(count - 1L, maximum, total) ->
        (
          PostgreSqlRenderingResource.StageDiagnosticStatements,
          BigInt(count),
          count - 1L
        ),
      diagnosticLimits(count, maximum - 1L, total) ->
        (
          PostgreSqlRenderingResource.StageDiagnosticStatementCharacters,
          BigInt(maximum),
          maximum - 1L
        ),
      diagnosticLimits(count, maximum, total - 1L) ->
        (
          PostgreSqlRenderingResource.StageDiagnosticTotalCharacters,
          BigInt(total),
          total - 1L
        )
    )
    cases.foreach { case (limits, (resource, required, limit)) =>
      val errors = rawDiagnostic(prepared, mode, limits).swap.toOption
        .getOrElse(fail(s"expected diagnostic limit failure for $resource"))
      assertEquals(errors.size, 1)
      errors.head match {
        case PostgreSqlRenderingError.ResourceLimitExceeded(
              actualResource,
              actualRequired,
              actualLimit
            ) =>
          assertEquals(actualResource, resource)
          assertEquals(actualRequired, required)
          assertEquals(actualLimit, limit)
        case other => fail(s"unexpected diagnostic limit error: $other")
      }
    }

    assertEquals(
      PostgreSqlStageDiagnosticLimits.create(0L, 0L, 0L).swap.toOption.get.size,
      3
    )
    val excessivePerStatement = PostgreSqlStageDiagnosticLimits
      .create(1L, Int.MaxValue.toLong + 1L, 1L)
      .swap
      .toOption
      .get
    assertEquals(excessivePerStatement.size, 1)
    assertEquals(
      excessivePerStatement.head.resource,
      PostgreSqlRenderingResource.StageDiagnosticStatementCharacters
    )
  }

  test("diagnostic validation rejects reordered, rehashed, and corrupted bindings") {
    val prepared = q2MixedPrepared
    val mode = YannakakisReductionMode.FullTwoPass
    val compiled = compile(prepared, mode)
    val normal = renderMode(prepared, mode)
    val valid = diagnose(prepared, mode, generousDiagnosticLimits).statements
    assert(valid.size > 2)
    val reordered = valid.updated(0, valid(1)).updated(1, valid(0))
    val rehashed = valid.updated(
      0,
      valid.head.copy(statementSha256 = "0" * 64)
    )
    val recounted = valid.updated(
      0,
      valid.head.copy(characterCount = valid.head.characterCount + 1L)
    )
    val reprefixed = valid.updated(
      0,
      valid.head.copy(ctePrefixCount = valid.head.ctePrefixCount + 1L)
    )
    val rewritten = valid.updated(
      0,
      valid.head.copy(sql = valid.head.sql + " ")
    )
    val elidedId = normal.trace.stages.find(
      _.physicalForm == PostgreSqlStagePhysicalForm.Elided
    ).get.id
    val emittedElided = valid.updated(0, valid.head.copy(stageId = elidedId))

    Vector(reordered, rehashed, recounted, reprefixed, rewritten, emittedElided)
      .foreach { statements =>
        val errors = PostgreSqlStageDiagnosticRenderer.validate(
          compiled,
          normal,
          statements,
          generousDiagnosticLimits
        )
        assertEquals(errors.size, 1)
        errors.head match {
          case PostgreSqlRenderingError.ModelInvariantViolation(detail) =>
            assert(
              detail.contains("stage diagnostic validation"),
              clues(detail)
            )
          case other => fail(s"unexpected diagnostic validation error: $other")
        }
      }
  }

  private def compile(
      prepared: PreparedQuery,
      mode: YannakakisReductionMode
  ): CompiledPostgreSqlRendering =
    PostgreSqlRenderingCompiler
      .compile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        mode
      )
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)

  private def rawDiagnostic(
      prepared: PreparedQuery,
      mode: YannakakisReductionMode,
      limits: PostgreSqlStageDiagnosticLimits
  ): Either[
    Vector[PostgreSqlRenderingError],
    PostgreSqlStageDiagnosticRendering
  ] =
    PostgreSqlStageDiagnosticRenderer.render(
      prepared.query,
      prepared.input,
      prepared.plan,
      PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default,
      mode,
      limits
    )

  private def diagnose(
      prepared: PreparedQuery,
      mode: YannakakisReductionMode,
      limits: PostgreSqlStageDiagnosticLimits
  ): PostgreSqlStageDiagnosticRendering =
    rawDiagnostic(prepared, mode, limits)
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)

  private def validate(
      compiled: CompiledPostgreSqlRendering,
      prepared: PreparedQuery,
      stages: Vector[PostgreSqlStageTrace]
  ): Vector[PostgreSqlRenderingError] = {
    val mode = compiled.trace.reduction.mode
    val schedule = mode match {
      case YannakakisReductionMode.FullTwoPass =>
        CompiledPostgreSqlReductionSchedule(
          mode,
          prepared.plan.schedule.bottomUp,
          prepared.plan.schedule.topDown
        )
      case YannakakisReductionMode.BottomUpOnly =>
        CompiledPostgreSqlReductionSchedule(
          mode,
          prepared.plan.schedule.bottomUp,
          Vector.empty
        )
      case YannakakisReductionMode.NoReduction =>
        CompiledPostgreSqlReductionSchedule(mode, Vector.empty, Vector.empty)
    }
    PostgreSqlRenderingCompiler.validateStageTrace(
      prepared.plan,
      compiled.bags,
      compiled.trace.bags,
      schedule,
      compiled.reductionCtes,
      stages
    )
  }

  private def renderMode(
      prepared: PreparedQuery,
      mode: YannakakisReductionMode
  ): PostgreSqlRenderedQuery =
    PostgreSqlBagFactorRenderer
      .renderWithReductionMode(
        prepared.query,
        prepared.input,
        prepared.plan,
        mode
      )
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)

  private def namedCteNames(
      rendered: PostgreSqlRenderedQuery
  ): Vector[String] =
    rendered.trace.stages.collect {
      case stage
          if stage.physicalForm ==
            PostgreSqlStagePhysicalForm.PlannerControlledCte =>
        stage.physicalCteName.get
    }

  private def assertPhysicalNamesFollowSql(
      rendered: PostgreSqlRenderedQuery
  ): Unit = {
    val names = namedCteNames(rendered)
    val expected =
      rendered.trace.bags.flatMap(_.supportCte) ++
        rendered.trace.bags.map(_.initialFactorCte) ++
        rendered.trace.semijoins.map(_.outputCte).distinct
    assertEquals(names, expected)
    val positions = names.map(name =>
      rendered.sql.indexOf(s"  \"$name\" AS (")
    )
    assert(positions.forall(_ >= 0), clues(names, positions, rendered.sql))
    assert(
      positions.zip(positions.drop(1)).forall { case (left, right) =>
        left < right
      },
      clues(names, positions)
    )
  }

  private def assertTopological(rendered: PostgreSqlRenderedQuery): Unit = {
    val positions = rendered.trace.stages.zipWithIndex.map {
      case (stage, index) => stage.id -> index
    }.toMap
    assertEquals(positions.size, rendered.trace.stages.size)
    rendered.trace.stages.zipWithIndex.foreach { case (stage, index) =>
      stage.inputStageIds.foreach { input =>
        assert(positions(input) < index, clues(stage, input, positions))
        assert(rendered.trace.stages(positions(input)).physicalForm.emitted)
      }
    }
  }

  private def assertContiguousVersions(
      rendered: PostgreSqlRenderedQuery
  ): Unit = {
    val versionsByBag = rendered.trace.stages.collect {
      case stage @ PostgreSqlStageTrace(
            id: PostgreSqlStageId.BagFactorVersion,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) => id.bagId -> id.version
    }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    versionsByBag.foreach { case (bagId, versions) =>
      assertEquals(
        versions,
        (0 until versions.size).toVector,
        clues(bagId, versions)
      )
    }
  }

  private def limitsWithCtes(ctes: Long): PostgreSqlRenderingLimits =
    PostgreSqlRenderingLimits
      .create(maxCtes = ctes)
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)

  private val generousDiagnosticLimits: PostgreSqlStageDiagnosticLimits =
    diagnosticLimits(
      statements = 1000L,
      charactersPerStatement = 16777216L,
      totalCharacters = 67108864L
    )

  private def diagnosticLimits(
      statements: Long,
      charactersPerStatement: Long,
      totalCharacters: Long
  ): PostgreSqlStageDiagnosticLimits =
    PostgreSqlStageDiagnosticLimits
      .create(statements, charactersPerStatement, totalCharacters)
      .fold(errors => fail(errors.map(_.message).mkString("; ")), identity)

  private def independentSha256(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def replaceRelations(
      query: QueryIR
  )(transform: RelationOccurrence => RelationOccurrence): QueryIR = {
    val replacements = query.relations
      .map(relation => relation.id -> transform(relation))
      .toMap
    def visit(relational: RelationalIR): RelationalIR = relational match {
      case scan: TableScanIR => TableScanIR(replacements(scan.relation.id))
      case filter: FilterIR  => filter.copy(input = visit(filter.input))
      case join: JoinIR =>
        join.copy(left = visit(join.left), right = visit(join.right))
      case project: ProjectIR => project.copy(input = visit(project.input))
      case count: GlobalCountStarIR => count.copy(input = visit(count.input))
      case count: GroupedCountStarIR => count.copy(input = visit(count.input))
      case count: DistributedGroupedCountStarIR =>
        count.copy(input = visit(count.input))
      case extremum: GlobalGuardedExtremumIR =>
        extremum.copy(input = visit(extremum.input))
      case unsupported: UnsupportedRelationalIR =>
        unsupported.copy(inputs = unsupported.inputs.map(visit))
    }
    query.copy(
      root = visit(query.root),
      relations = query.relations.map(relation => replacements(relation.id))
    )
  }

  private lazy val q2OneBagPrepared: PreparedQuery =
    prepareTopOne(
      q2Sql,
      q2Tables,
      widthBound = 2,
      requireConnectedCover = true
    )

  private lazy val q2MixedPrepared: PreparedQuery = {
    val base = q2OneBagPrepared
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
    withDecomposition(base.report, base.hypergraph, base.input, decomposition)
  }

  private lazy val q5FusedPrepared: PreparedQuery = {
    val base = prepareTopOne(
      q5Sql,
      q5Tables,
      widthBound = 2,
      requireConnectedCover = true
    )
    val decompositions = TopKCandidateTD
      .enumerate(base.input, planLimit = 10)
      .fold(error => fail(error.message), {
        case found: CandidatePlanDecision.Found =>
          found.plans.map(_.decomposition)
        case rejected: CandidatePlanDecision.Rejected =>
          fail(s"Q5-derived candidates rejected: ${rejected.statistics}")
      })
    decompositions.iterator
      .map(decomposition =>
        withDecomposition(
          base.report,
          base.hypergraph,
          base.input,
          decomposition
        )
      )
      .find(prepared =>
        renderMode(prepared, YannakakisReductionMode.FullTwoPass).trace.stages
          .exists(stage =>
            stage.kind == PostgreSqlStageKind.FusedReduction &&
              stage.semijoinMembers.size > 1
          )
      )
      .getOrElse(fail("top-10 Q5-derived plans contain no fused reduction"))
  }

  private val q2Tables = Vector(
    table("lsqb_q2_knows", "person1_id", "person2_id"),
    table("lsqb_q2_comment", "creator_id", "reply_post_id"),
    table("lsqb_q2_post", "post_id", "creator_id")
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

  private val q5Tables = Vector(
    table("q5d_customer", "customer_id", "nation_id"),
    table("q5d_orders", "customer_id", "order_id"),
    table("q5d_lineitem", "order_id", "supplier_id"),
    table("q5d_supplier", "supplier_id", "nation_id"),
    table("q5d_nation", "nation_id", "region_id"),
    TestTable(
      "q5d_region",
      Vector(
        TestColumn("row_id", SqlTypeName.BIGINT, nullable = false),
        TestColumn("region_id", SqlTypeName.BIGINT, nullable = true)
      )
    )
  )

  private val q5Sql =
    """SELECT
      |  c.customer_id AS "customer_id",
      |  o.order_id AS "order_id",
      |  l.supplier_id AS "supplier_id",
      |  c.nation_id AS "nation_id",
      |  n.region_id AS "region_id"
      |FROM q5d_customer AS c
      |JOIN q5d_orders AS o ON c.customer_id = o.customer_id
      |JOIN q5d_lineitem AS l ON o.order_id = l.order_id
      |JOIN q5d_supplier AS s
      |  ON l.supplier_id = s.supplier_id AND s.nation_id = c.nation_id
      |JOIN q5d_nation AS n ON s.nation_id = n.nation_id
      |JOIN q5d_region AS r ON n.region_id = r.region_id
      |""".stripMargin

  private def table(name: String, first: String, second: String): TestTable =
    TestTable(
      name,
      Vector(
        TestColumn("row_id", SqlTypeName.BIGINT, nullable = false),
        TestColumn(first, SqlTypeName.BIGINT, nullable = true),
        TestColumn(second, SqlTypeName.BIGINT, nullable = true)
      )
    )
}
