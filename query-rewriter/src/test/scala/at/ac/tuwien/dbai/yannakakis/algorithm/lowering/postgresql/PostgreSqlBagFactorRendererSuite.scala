package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactor,
  BagFactorPlan,
  BagFactorPlanningLimits,
  SemijoinPass,
  SemijoinStep
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.calcite.{TestColumn, TestTable}
import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  EdgeId,
  QueryHypergraph,
  ValidationError,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  BagTreeEdge,
  CoveredBag,
  CoveredDecomposition,
  DecompositionValidationError
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  CalciteQueryAnalyzer,
  CalciteSqlPlanner,
  ColumnRef,
  DistributedGroupedCountStarIR,
  FilterIR,
  GlobalCountStarIR,
  GlobalGuardedExtremumIR,
  GroupedCountStarIR,
  JoinIR,
  PredicateClassification,
  PredicateDescriptor,
  ProjectIR,
  ProjectItem,
  QueryIR,
  QuerySourceProof,
  RelationOccurrence,
  RelationalIR,
  ScalarExpression,
  TableScanIR,
  UnsupportedRelationalIR
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  ObjectInputStream,
  ObjectOutputStream,
  ObjectStreamClass
}
import scala.collection.mutable

import PostgreSqlRendererTestSupport._

class PostgreSqlBagFactorRendererSuite extends munit.FunSuite {
  test("one-bag triangle elides redundant support without losing SQL semantics") {
    val prepared = trianglePrepared()
    val rendered = render(prepared)

    assertEquals(prepared.decomposition.bags.size, 1)
    assertEquals(
      prepared.decomposition.bags.head.vertices,
      prepared.hypergraph.variables
    )
    assertEquals(
      prepared.decomposition.bags.head.cover.map(_.value),
      Vector(0, 1)
    )
    assertEquals(prepared.plan.factors.head.ownedEdgeIds.map(_.value), Vector(0, 1, 2))
    assertEquals(rendered.sourceQuery, prepared.query)
    assertEquals(rendered.bagFactorPlan, prepared.plan)
    assertEquals(
      rendered.output.map(column => (column.ordinal, column.name)),
      prepared.query.output.map(column => (column.ordinal, column.name))
    )

    val postFactorColumnIds = (
      prepared.query.output.iterator.flatMap(_.expression.columnRefs.iterator) ++
        prepared.query.predicates.iterator.collect {
          case predicate
              if predicate.classification
                .isInstanceOf[PredicateClassification.InnerEquality] =>
            predicate.expression
        }.flatMap(_.columnRefs.iterator)
    ).map(_.id).toSet
    val expectedOccurrenceColumns = prepared.hypergraph.edges.flatMap { edge =>
      val relation = prepared.query.relations.find(_.id == edge.relationOccurrenceId).get
      val carriedIds = edge.bindings.iterator.map(_._1).toSet ++ postFactorColumnIds
      relation.columns.collect {
        case metadata if carriedIds(metadata.ref.id) => edge.id -> metadata.ref.id
      }
    }
    val tracedOccurrenceColumns = rendered.trace.bags.flatMap(_.occurrenceColumns)
      .map(column => column.edgeId -> column.columnId)
    assertEquals(tracedOccurrenceColumns, expectedOccurrenceColumns)
    assertEquals(
      rendered.trace.bags.flatMap(_.occurrenceColumns).map(_.columnName).distinct.size,
      expectedOccurrenceColumns.size
    )
    val bagTrace = rendered.trace.bags.head
    assertEquals(
      bagTrace.supportStrategy,
      PostgreSqlBagSupportStrategy.ElidedByOwnedCover
    )
    assertEquals(bagTrace.supportCte, None)
    assert(!rendered.sql.contains(PostgreSqlNames.support(bagTrace.bagId)))

    val hypergraphColumns = prepared.hypergraph.columns.toSet
    val payloadColumns = prepared.query.output.collect {
      case output if output.name.endsWith("_payload") =>
        output.expression.asInstanceOf[ScalarExpression.Column].ref.id
    }
    assert(payloadColumns.nonEmpty)
    assert(payloadColumns.forall(column => !hypergraphColumns(column)))
    assert(
      payloadColumns.forall(column => tracedOccurrenceColumns.exists(_._2 == column))
    )

    assert(!rendered.sql.contains("IS NOT DISTINCT FROM"))
    assert(!rendered.sql.contains("SELECT DISTINCT"), clues(rendered.sql))
    prepared.query.relations.foreach { relation =>
      val qualifiedName = relation.qualifiedName.map(quotedIdentifier).mkString(".")
      assertEquals(count(rendered.sql, qualifiedName), 1, clues(qualifiedName, rendered.sql))
    }
    val normalizationEqualities = ownedNormalizationEqualities(
      prepared,
      prepared.plan.factors.head
    )
    assertEquals(normalizationEqualities.size, 6)
    normalizationEqualities.foreach { equality =>
      assertEquals(count(rendered.sql, equality), 1, clues(equality, rendered.sql))
    }
    val innerEqualities = prepared.query.predicates.collect {
      case PredicateDescriptor(
            _,
            _,
            ScalarExpression.Call(
              "EQUALS",
              "=",
              Vector(
                ScalarExpression.Column(left, _, _, _),
                ScalarExpression.Column(right, _, _, _)
              ),
              _,
              _,
              _,
              true
            ),
            _: PredicateClassification.InnerEquality
          ) => left -> right
    }
    assertEquals(innerEqualities.size, 3)
    val compiled = compileRendering(prepared)
    assertEquals(compiled.innerEqualities.size, innerEqualities.size)
    assertEquals(compiled.terminalInnerEqualities, Vector.empty)
    innerEqualities.foreach { case (left, right) =>
      val equality = finalEqualitySql(prepared, left, right)
      assertEquals(count(finalSelectSql(rendered.sql), equality), 0, clues(equality, rendered.sql))
    }
    assert(!finalSelectSql(rendered.sql).contains("\nWHERE\n"), clues(rendered.sql))
    assertEquals(count(rendered.sql, " = "), normalizationEqualities.size)
    assertEquals(rendered.statistics.expandedScanCount, 3L)
    assertEquals(rendered.statistics.renderedTermCount, 24L)
    assertEquals(rendered.statistics.cteCount, 1L)
    assertEquals(rendered.statistics.sqlCharacterCount, rendered.sql.length.toLong)
    assertEquals(render(prepared), rendered)
  }

  test("depth-plus-branch trace always consumes the latest CTE version") {
    val prepared = depthBranchPrepared()
    val rendered = render(prepared)
    val trace = rendered.trace

    assertEquals(prepared.plan.factors.size, 5)
    assert(prepared.plan.factors.exists(_.ownedEdgeIds.isEmpty))
    val supportOnly = prepared.plan.factors.find(_.ownedEdgeIds.isEmpty).get
    val supportOnlyTrace = trace.bags.find(_.bagId == supportOnly.bagId).get
    assertEquals(supportOnlyTrace.occurrenceColumns, Vector.empty)
    assertEquals(
      supportOnlyTrace.supportStrategy,
      PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
    )
    assertEquals(
      supportOnlyTrace.supportCte,
      Some(PostgreSqlNames.support(supportOnly.bagId))
    )
    assert(rendered.sql.contains(s"\"${supportOnlyTrace.supportCte.get}\" AS ("))
    assert(prepared.plan.factors.exists(_.children.size == 2))
    assert(prepared.plan.factors.exists(_.depth == 2))

    val expectedSteps = prepared.plan.schedule.bottomUp ++ prepared.plan.schedule.topDown
    assertEquals(
      trace.semijoins.map(step =>
        (step.pass, step.ordinal, step.target, step.source)
      ),
      expectedSteps.map(step =>
        (step.pass, step.ordinal, step.target, step.source)
      )
    )
    expectedSteps.zip(trace.semijoins).foreach { case (step, renderedStep) =>
      val sourceReference =
        s"FROM \"${renderedStep.sourceInputCte}\" AS \"${PostgreSqlNames.semijoinSource(step)}\""
      assert(rendered.sql.contains(sourceReference), clues(sourceReference, rendered.sql))
    }
    var traceOffset = 0
    maximalReductionRuns(prepared.plan).foreach { run =>
      val firstTrace = trace.semijoins(traceOffset)
      val targetReference =
        s"FROM \"${firstTrace.targetInputCte}\" AS \"${PostgreSqlNames.semijoinTarget(run.last)}\""
      assert(rendered.sql.contains(targetReference), clues(targetReference, rendered.sql))
      traceOffset += run.size
    }

    assertLatestPhysicalVersions(prepared.plan, rendered)

    val right = prepared.plan.factors.find(factor =>
      factor.depth == 1 && factor.children.size == 2
    ).get
    val rightBottomUpOutputs = trace.semijoins.collect {
      case step
          if step.pass == SemijoinPass.BottomUp && step.target == right.bagId =>
        step.outputCte
    }
    assertEquals(rightBottomUpOutputs.size, 2)
    val rightToRoot = trace.semijoins.find(step =>
      step.pass == SemijoinPass.BottomUp && step.source == right.bagId
    ).get
    assertEquals(rightToRoot.sourceInputCte, rightBottomUpOutputs.last)

    val rootToRight = trace.semijoins.find(step =>
      step.pass == SemijoinPass.TopDown && step.target == right.bagId
    ).get
    val rightToGrandchildren = trace.semijoins.filter(step =>
      step.pass == SemijoinPass.TopDown && step.source == right.bagId
    )
    assert(rightToGrandchildren.nonEmpty)
    assert(rightToGrandchildren.forall(_.sourceInputCte == rootToRight.outputCte))
  }

  test(
    "adjacent same-pass target reductions fuse maximally without hiding logical trace"
  ) {
    val prepared = depthBranchPrepared()
    val rendered = render(prepared)
    val compiled = PostgreSqlRenderingCompiler
      .compile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        identity
      )
    val runs = maximalReductionRuns(prepared.plan)
    val logicalSteps = runs.flatten

    assertEquals(compiled.trace, rendered.trace)
    assertEquals(compiled.reductionCtes.size, runs.size)
    assertEquals(rendered.trace.semijoins.size, logicalSteps.size)
    assertEquals(compiled.reductionCtes.flatMap(_.members.map(_.step)), logicalSteps)
    assertEquals(runs.count(_.size > 1), 1)
    assertEquals(runs.filter(_.size > 1).map(_.size), Vector(2))

    var traceOffset = 0
    runs.zip(compiled.reductionCtes).foreach { case (run, reduction) =>
      val traces = rendered.trace.semijoins.slice(
        traceOffset,
        traceOffset + run.size
      )
      assertEquals(reduction.pass, run.head.pass)
      assertEquals(reduction.target, run.head.target)
      assertEquals(reduction.members.map(_.step), run)
      assertEquals(reduction.members.map(_.trace), traces)
      assertEquals(reduction.targetInputCte, traces.head.targetInputCte)
      assertEquals(reduction.outputCte, PostgreSqlNames.semijoin(run.last))
      assert(traces.forall(_.targetInputCte == reduction.targetInputCte))
      assert(traces.forall(_.outputCte == reduction.outputCte))
      val targetReference =
        s"FROM \"${reduction.targetInputCte}\" AS \"${PostgreSqlNames.semijoinTarget(run.last)}\""
      assertEquals(
        count(rendered.sql, targetReference),
        1,
        clues(targetReference, rendered.sql)
      )
      assertEquals(
        count(rendered.sql, s"  \"${reduction.outputCte}\" AS ("),
        1
      )
      run.dropRight(1).foreach { step =>
        val pruned = PostgreSqlNames.semijoin(step)
        assertEquals(
          count(rendered.sql, s"  \"$pruned\" AS ("),
          0,
          clues(pruned, rendered.sql)
        )
      }
      run.zip(traces).foreach { case (step, stepTrace) =>
        val sourceReference =
          s"FROM \"${stepTrace.sourceInputCte}\" AS \"${PostgreSqlNames.semijoinSource(step)}\""
        assertEquals(
          count(rendered.sql, sourceReference),
          1,
          clues(sourceReference, rendered.sql)
        )
      }
      traceOffset += run.size
    }
    assertEquals(traceOffset, logicalSteps.size)

    assertLatestPhysicalVersions(prepared.plan, rendered)
    assertEquals(
      count(rendered.sql, "WHERE EXISTS ("),
      runs.size
    )
    assertEquals(
      count(rendered.sql, "AND EXISTS ("),
      logicalSteps.size - runs.size
    )

    val physicalOutputNames = compiled.reductionCtes.map(_.outputCte)
    assertEquals(physicalOutputNames.distinct.size, physicalOutputNames.size)
    val expectedCtes = compiled.bags.count(
      _.supportStrategy ==
        PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
    ) + compiled.bags.size + compiled.reductionCtes.size
    assertEquals(rendered.statistics.cteCount, expectedCtes.toLong)
    assertEquals(
      rendered.statistics.renderedTermCount,
      expectedRenderedTerms(compiled)
    )
  }

  test(
    "fusion resource boundaries and physical-only generated names fail closed"
  ) {
    val prepared = depthBranchPrepared()
    val baseline = render(prepared)
    val runs = maximalReductionRuns(prepared.plan)
    val fusedRun = runs.find(_.size > 1).getOrElse(
      fail(s"fixture contains no fusible run: $runs")
    )
    val prunedName = PostgreSqlNames.semijoin(fusedRun.head)
    val physicalName = PostgreSqlNames.semijoin(fusedRun.last)
    assert(!baseline.sql.contains(s"  \"$prunedName\" AS ("), clues(baseline.sql))
    assert(baseline.sql.contains(s"  \"$physicalName\" AS ("), clues(baseline.sql))

    val prunedCollision = replaceRelations(prepared.query) { relation =>
      if (relation.id == prepared.query.relations.head.id)
        relation.copy(qualifiedName = Vector(prunedName))
      else relation
    }
    assert(
      PostgreSqlBagFactorRenderer
        .render(prunedCollision, prepared.input, prepared.plan)
        .isRight,
      "a pruned logical name is not a generated CTE collision"
    )

    val physicalCollision = replaceRelations(prepared.query) { relation =>
      if (relation.id == prepared.query.relations.head.id)
        relation.copy(qualifiedName = Vector(physicalName))
      else relation
    }
    val collisionErrors = errors(
      PostgreSqlBagFactorRenderer.render(
        physicalCollision,
        prepared.input,
        prepared.plan
      )
    )
    assertEquals(
      collisionErrors.collect {
        case error: PostgreSqlRenderingError.GeneratedNameCollision =>
          error.generatedName
      },
      Vector(physicalName)
    )

    val stats = baseline.statistics
    Vector(
      PostgreSqlRenderingResource.RenderedTerms -> renderingLimits(
        stats.expandedScanCount,
        stats.renderedTermCount - 1L,
        stats.cteCount,
        stats.sqlCharacterCount
      ),
      PostgreSqlRenderingResource.Ctes -> renderingLimits(
        stats.expandedScanCount,
        stats.renderedTermCount,
        stats.cteCount - 1L,
        stats.sqlCharacterCount
      )
    ).foreach { case (resource, limits) =>
      val failures = errors(
        PostgreSqlBagFactorRenderer.render(
          prepared.query,
          prepared.input,
          prepared.plan,
          limits
        )
      )
      assertEquals(failures.size, 1)
      failures.head match {
        case PostgreSqlRenderingError.ResourceLimitExceeded(
              actual,
              required,
              limit
            ) =>
          assertEquals(actual, resource)
          assertEquals(required, BigInt(limit + 1L))
        case other => fail(s"expected $resource failure, found $other")
      }
    }
  }

  test("physical reduction compilation rejects a self-sourced schedule step") {
    val prepared = depthBranchPrepared()
    val first = prepared.plan.schedule.bottomUp.head
    val corrupted = prepared.plan.copy(
      schedule = prepared.plan.schedule.copy(
        bottomUp = prepared.plan.schedule.bottomUp.updated(
          0,
          first.copy(source = first.target)
        )
      )
    )
    val failures = PostgreSqlRenderingCompiler
      .compile(
        prepared.query,
        prepared.input,
        corrupted,
        PostgreSqlRenderingLimits.default
      )
      .swap
      .toOption
      .getOrElse(fail("expected self-sourced reduction compilation failure"))

    assertEquals(failures.size, 1)
    failures.head match {
      case PostgreSqlRenderingError.ModelInvariantViolation(detail) =>
        assert(detail.contains("Physical semijoin reduction"), clues(detail))
        assert(detail.contains("own source"), clues(detail))
      case other => fail(s"expected physical reduction invariant, found $other")
    }
  }

  test("reduction modes select exact canonical subsets with exact trace and resources") {
    val prepared = depthBranchPrepared()
    val canonicalBottomUp = prepared.plan.schedule.bottomUp
    val canonicalTopDown = prepared.plan.schedule.topDown
    val cases = Vector(
      YannakakisReductionMode.FullTwoPass -> (canonicalBottomUp ++ canonicalTopDown),
      YannakakisReductionMode.BottomUpOnly -> canonicalBottomUp,
      YannakakisReductionMode.NoReduction -> Vector.empty[SemijoinStep]
    )

    cases.foreach { case (mode, expectedSteps) =>
      val rendered = renderMode(prepared, mode)
      val compiled = PostgreSqlRenderingCompiler
        .compile(
          prepared.query,
          prepared.input,
          prepared.plan,
          PostgreSqlRenderingLimits.default,
          mode
        )
        .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
      val expectedRuns = maximalReductionRuns(expectedSteps)
      val reduction = rendered.trace.reduction

      assertEquals(rendered.trace, compiled.trace)
      assertEquals(
        rendered.trace.semijoins.map(trace =>
          (trace.pass, trace.ordinal, trace.target, trace.source)
        ),
        expectedSteps.map(step =>
          (step.pass, step.ordinal, step.target, step.source)
        )
      )
      assertEquals(compiled.reductionCtes.map(_.members.map(_.step)), expectedRuns)
      assertEquals(compiled.reductionCtes.flatMap(_.members.map(_.trace)), rendered.trace.semijoins)
      assertEquals(reduction.mode, mode)
      assertEquals(
        reduction.reductionOrder,
        at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlPhysicalVariantId.CanonicalReductionOrder
      )
      assertEquals(reduction.canonicalBottomUpStepCount, canonicalBottomUp.size)
      assertEquals(reduction.canonicalTopDownStepCount, canonicalTopDown.size)
      assertEquals(
        reduction.selectedBottomUpStepCount,
        expectedSteps.count(_.pass == SemijoinPass.BottomUp)
      )
      assertEquals(
        reduction.selectedTopDownStepCount,
        expectedSteps.count(_.pass == SemijoinPass.TopDown)
      )
      assertEquals(reduction.physicalReductionCteCount, expectedRuns.size)
      assertEquals(rendered.statistics.cteCount, expectedPhysicalCtes(compiled))
      assertEquals(rendered.statistics.renderedTermCount, expectedRenderedTerms(compiled))
      assertEquals(rendered.statistics.sqlCharacterCount, rendered.sql.length.toLong)
      assertLatestPhysicalVersions(prepared.plan, rendered, mode)

      val supportOnly = prepared.plan.factors.find(_.ownedEdgeIds.isEmpty).get
      assertEquals(
        rendered.trace.bags.find(_.bagId == supportOnly.bagId).get.supportStrategy,
        PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
      )
      assert(!rendered.sql.contains("IS NOT DISTINCT FROM"), clues(mode, rendered.sql))

      val emittedReductionNames = expectedRuns.map(run => PostgreSqlNames.semijoin(run.last))
      emittedReductionNames.foreach(name =>
        assertEquals(count(rendered.sql, s"  \"$name\" AS ("), 1, clues(mode, name))
      )
      expectedSteps.map(PostgreSqlNames.semijoin).diff(emittedReductionNames).foreach {
        pruned =>
          assertEquals(count(rendered.sql, s"  \"$pruned\" AS ("), 0, clues(mode, pruned))
      }
    }
  }

  test("legacy render overloads remain explicit-full and null calls stay unambiguous") {
    val prepared = depthBranchPrepared()
    val limits = PostgreSqlRenderingLimits.default
    val planning = BagFactorPlanningLimits.default
    val legacy = PostgreSqlBagFactorRenderer
      .render(prepared.query, prepared.input, prepared.plan)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val equivalent = Vector(
      PostgreSqlBagFactorRenderer.render(
        prepared.query,
        prepared.input,
        prepared.plan,
        limits
      ),
      PostgreSqlBagFactorRenderer.render(
        prepared.query,
        prepared.input,
        prepared.plan,
        limits,
        planning
      ),
      PostgreSqlBagFactorRenderer.renderWithReductionMode(
        prepared.query,
        prepared.input,
        prepared.plan,
        YannakakisReductionMode.FullTwoPass
      ),
      PostgreSqlBagFactorRenderer.renderWithReductionMode(
        prepared.query,
        prepared.input,
        prepared.plan,
        limits,
        YannakakisReductionMode.FullTwoPass
      ),
      PostgreSqlBagFactorRenderer.render(
        prepared.query,
        prepared.input,
        prepared.plan,
        limits,
        planning,
        YannakakisReductionMode.FullTwoPass
      )
    )
    equivalent.foreach(result => assertEquals(result, Right(legacy)))
    assertEquals(legacy.trace.reduction.mode, YannakakisReductionMode.Default)

    val legacyNullLimits = PostgreSqlBagFactorRenderer.render(
      prepared.query,
      prepared.input,
      prepared.plan,
      null
    )
    val legacyNullPlanning = PostgreSqlBagFactorRenderer.render(
      prepared.query,
      prepared.input,
      prepared.plan,
      limits,
      null
    )
    val modeNull = PostgreSqlBagFactorRenderer.renderWithReductionMode(
      prepared.query,
      prepared.input,
      prepared.plan,
      null
    )
    val limitedModeNull = PostgreSqlBagFactorRenderer.renderWithReductionMode(
      prepared.query,
      prepared.input,
      prepared.plan,
      limits,
      null
    )
    val coreModeNull = PostgreSqlBagFactorRenderer.render(
      prepared.query,
      prepared.input,
      prepared.plan,
      limits,
      planning,
      null
    )

    assertEquals(
      errors(legacyNullLimits),
      Vector(PostgreSqlRenderingError.MissingValue("PostgreSQL rendering limits"))
    )
    assertEquals(
      errors(legacyNullPlanning),
      Vector(PostgreSqlRenderingError.MissingValue("bag-factor planning validation limits"))
    )
    Vector(modeNull, limitedModeNull, coreModeNull).foreach(result =>
      assertEquals(
        errors(result),
        Vector(PostgreSqlRenderingError.MissingValue("Yannakakis reduction mode"))
      )
    )
  }

  test("reduction trace provenance rejects inconsistent modes, counts, and order") {
    def trace(
        mode: YannakakisReductionMode,
        selectedBottomUp: Int,
        selectedTopDown: Int,
        physical: Int,
        order: String =
          at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlPhysicalVariantId.CanonicalReductionOrder
    ): PostgreSqlReductionScheduleTrace =
      PostgreSqlReductionScheduleTrace(
        mode = mode,
        reductionOrder = order,
        canonicalBottomUpStepCount = 2,
        canonicalTopDownStepCount = 2,
        selectedBottomUpStepCount = selectedBottomUp,
        selectedTopDownStepCount = selectedTopDown,
        physicalReductionCteCount = physical
      )

    assertEquals(
      trace(YannakakisReductionMode.FullTwoPass, 2, 2, 3).mode,
      YannakakisReductionMode.FullTwoPass
    )
    assertEquals(
      trace(YannakakisReductionMode.BottomUpOnly, 2, 0, 1).mode,
      YannakakisReductionMode.BottomUpOnly
    )
    assertEquals(
      trace(YannakakisReductionMode.NoReduction, 0, 0, 0).mode,
      YannakakisReductionMode.NoReduction
    )

    Vector[() => Unit](
      () => trace(null, 0, 0, 0),
      () => trace(YannakakisReductionMode.FullTwoPass, 2, 1, 2),
      () => trace(YannakakisReductionMode.BottomUpOnly, 2, 1, 2),
      () => trace(YannakakisReductionMode.NoReduction, 1, 0, 1),
      () => trace(YannakakisReductionMode.BottomUpOnly, 2, 0, 3),
      () => trace(YannakakisReductionMode.BottomUpOnly, 2, 0, 0),
      () => trace(YannakakisReductionMode.NoReduction, 0, 0, 0, "reordered")
    ).foreach(operation => intercept[IllegalArgumentException](operation()))
  }

  private def selectedReductionSteps(
      plan: BagFactorPlan,
      mode: YannakakisReductionMode
  ): Vector[SemijoinStep] = mode match {
    case YannakakisReductionMode.FullTwoPass =>
      plan.schedule.bottomUp ++ plan.schedule.topDown
    case YannakakisReductionMode.BottomUpOnly => plan.schedule.bottomUp
    case YannakakisReductionMode.NoReduction  => Vector.empty
  }

  private def maximalReductionRuns(
      plan: BagFactorPlan,
      mode: YannakakisReductionMode = YannakakisReductionMode.Default
  ): Vector[Vector[SemijoinStep]] =
    maximalReductionRuns(selectedReductionSteps(plan, mode))

  private def maximalReductionRuns(
      steps: Vector[SemijoinStep]
  ): Vector[Vector[SemijoinStep]] = {
    steps.foldLeft(Vector.empty[Vector[SemijoinStep]]) { (runs, step) =>
      runs.lastOption match {
        case Some(run)
            if run.head.pass == step.pass && run.head.target == step.target =>
          runs.updated(runs.size - 1, run :+ step)
        case _ => runs :+ Vector(step)
      }
    }
  }

  private def assertLatestPhysicalVersions(
      plan: BagFactorPlan,
      rendered: PostgreSqlRenderedQuery,
      mode: YannakakisReductionMode = YannakakisReductionMode.Default
  ): Unit = {
    val runs = maximalReductionRuns(plan, mode)
    val current = mutable.Map.empty[BagId, String]
    rendered.trace.bags.foreach(trace =>
      current.put(trace.bagId, trace.initialFactorCte)
    )
    var offset = 0
    runs.foreach { run =>
      val traces = rendered.trace.semijoins.slice(offset, offset + run.size)
      assertEquals(traces.size, run.size)
      val expectedTargetInput = current(run.head.target)
      val expectedOutput = PostgreSqlNames.semijoin(run.last)
      run.zip(traces).foreach { case (step, trace) =>
        assertEquals(
          (trace.pass, trace.ordinal, trace.target, trace.source),
          (step.pass, step.ordinal, step.target, step.source)
        )
        assertEquals(trace.targetInputCte, expectedTargetInput)
        assertEquals(trace.sourceInputCte, current(step.source))
        assertEquals(trace.outputCte, expectedOutput)
      }
      current.update(run.head.target, expectedOutput)
      offset += run.size
    }
    assertEquals(offset, rendered.trace.semijoins.size)
    rendered.trace.bags.foreach(trace =>
      assertEquals(trace.finalFactorCte, current(trace.bagId))
    )
  }

  /** Independent exact term oracle over the frozen compiled rendering model. */
  private def compileRendering(
      prepared: PreparedQuery,
      mode: YannakakisReductionMode = YannakakisReductionMode.Default
  ): CompiledPostgreSqlRendering =
    PostgreSqlRenderingCompiler
      .compile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        mode
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        identity
      )

  private def compileRenderingWithEnumerationProfile(
      prepared: PreparedQuery,
      profile: PostgreSqlEnumerationRenderingProfile
  ): CompiledPostgreSqlRendering =
    PostgreSqlRenderingCompiler
      .compile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        YannakakisReductionMode.Default,
        PostgreSqlCteMaterializationDirective.PlannerControlled,
        PostgreSqlAggregateRenderingProfile.LegacyV1,
        profile
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        identity
      )

  private def renderWithEnumerationProfile(
      prepared: PreparedQuery,
      profile: PostgreSqlEnumerationRenderingProfile
  ): PostgreSqlRenderedQuery =
    PostgreSqlBagFactorRenderer
      .renderWithEnumerationProfile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.Default,
        profile
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        identity
      )

  private def renderDiagnosticsWithEnumerationProfile(
      prepared: PreparedQuery,
      profile: PostgreSqlEnumerationRenderingProfile
  ): PostgreSqlStageDiagnosticRendering = {
    val diagnosticLimits = PostgreSqlStageDiagnosticLimits
      .create(
        maxStatements = 256L,
        maxCharactersPerStatement = 16L * 1024L * 1024L,
        maxTotalCharacters = 64L * 1024L * 1024L
      )
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    PostgreSqlBagFactorRenderer
      .renderWithStageDiagnostics(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.Default,
        diagnosticLimits,
        profile
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        identity
      )
  }

  private def expectedRenderedTerms(
      compiled: CompiledPostgreSqlRendering
  ): Long = {
    var terms = BigInt(compiled.query.output.size)
    compiled.bags.foreach { bag =>
      bag.supportStrategy match {
        case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
          terms += bag.factor.vertices.size
          terms += bag.supportEqualities.size
          bag.supportEdges.foreach { edge =>
            terms += compiled.localPredicatesByOccurrence
              .getOrElse(edge.relationOccurrenceId, Vector.empty)
              .size
          }
          terms += bag.factor.vertices.size
          bag.ownedOccurrences.foreach { occurrence =>
            terms += occurrence.edge.bindings.size
            terms += occurrence.carriedColumns.size
            terms += compiled.localPredicatesByOccurrence
              .getOrElse(occurrence.relation.id, Vector.empty)
              .size
          }

        case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
          terms += bag.factor.vertices.size
          terms += bag.ownedEqualities.size
          bag.ownedOccurrences.foreach { occurrence =>
            terms += occurrence.carriedColumns.size
            terms += compiled.localPredicatesByOccurrence
              .getOrElse(occurrence.relation.id, Vector.empty)
              .size
          }
      }
    }
    compiled.reductionCtes.foreach { reduction =>
      terms += 1
      reduction.members.foreach(member =>
        terms += 1 + member.step.separator.size
      )
    }
    compiled.plan.arcs.foreach(arc => terms += arc.separator.size)
    terms += compiled.terminalInnerEqualities.size
    assert(terms.isValidLong, clues(terms))
    terms.toLong
  }

  private def expectedPhysicalCtes(
      compiled: CompiledPostgreSqlRendering
  ): Long =
    (compiled.bags.size +
      compiled.bags.count(
        _.supportStrategy ==
          PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
      ) +
      compiled.reductionCtes.size).toLong

  test("a support cover joins on a variable projected out of its bag") {
    val base = chainPrepared(widthBound = 2)
    val graph = base.hypergraph
    val decomposition = coveredDecomposition(
      root = 0,
      bags = Vector(
        coveredBag(0, Vector(0), Vector(1, 2)),
        coveredBag(1, Vector(0, 1, 2), Vector(1, 2))
      ),
      edges = Vector(treeEdge(0, 1))
    )
    val prepared = withDecomposition(base.report, graph, base.input, decomposition)
    val rendered = render(prepared)
    val rootTrace = rendered.trace.bags.find(_.bagId == bagId(0)).get

    assertEquals(rootTrace.variableColumns.map(_.variableId.value), Vector(0))
    assert(!rootTrace.variableColumns.exists(_.variableId == variable(1)))
    assertEquals(
      rootTrace.supportStrategy,
      PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
    )
    assertEquals(rootTrace.supportCte, Some(PostgreSqlNames.support(bagId(0))))
    val expectedEquality =
      "\"yr_s_b0_e1\".\"q\" = \"yr_s_b0_e2\".\"q\""
    assert(rendered.sql.contains(expectedEquality), clues(rendered.sql))
  }

  test("support elision applies independently to eligible factors in a multi-bag plan") {
    val prepared = eligibleTwoBagPrepared()
    val rendered = render(prepared)

    assertEquals(prepared.plan.factors.size, 2)
    assertEquals(
      rendered.trace.bags.map(_.supportStrategy),
      Vector.fill(2)(PostgreSqlBagSupportStrategy.ElidedByOwnedCover)
    )
    assert(rendered.trace.bags.forall(_.supportCte.isEmpty))
    assert(!rendered.sql.contains("SELECT DISTINCT"), clues(rendered.sql))
    assertEquals(rendered.statistics.expandedScanCount, 3L)
    assertEquals(rendered.statistics.renderedTermCount, 26L)
    assertEquals(rendered.statistics.cteCount, 4L)

    val compiled = compileRendering(prepared)
    assertEquals(compiled.innerEqualities.size, 2)
    assertEquals(compiled.terminalInnerEqualities, Vector.empty)
    val finalSelect = finalSelectSql(rendered.sql)
    assert(!finalSelect.contains("\nWHERE\n"), clues(finalSelect))
    assertEquals(
      count(finalSelect, " = "),
      prepared.plan.arcs.iterator.map(_.separator.size).sum
    )

    val singletonGroups = prepared.plan.factors.flatMap { factor =>
      ownedBindingGroups(prepared, factor).collect {
        case (_, locations) if locations.size == 1 => locations.head
      }
    }
    assertEquals(singletonGroups.size, 2)
    singletonGroups.foreach { location =>
      val selfEquality = s"$location = $location"
      assertEquals(count(rendered.sql, selfEquality), 1, clues(selfEquality, rendered.sql))
    }

    assertEquals(render(prepared).trace, rendered.trace)
    assertEquals(render(prepared).sql, rendered.sql)
  }

  test("a cover edge not owned by its bag keeps the DISTINCT support path") {
    val prepared = triangleMixedPrepared()
    val rendered = render(prepared)
    val traceByBag = rendered.trace.bags.map(trace => trace.bagId -> trace).toMap

    prepared.plan.factors.foreach { factor =>
      val eligible = factor.supportCover.forall(factor.ownedEdgeIds.toSet)
      val expected =
        if (eligible) PostgreSqlBagSupportStrategy.ElidedByOwnedCover
        else PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
      assertEquals(traceByBag(factor.bagId).supportStrategy, expected)
    }
    val materializedFactor = prepared.plan.factors.find { factor =>
      !factor.supportCover.forall(factor.ownedEdgeIds.toSet)
    }.getOrElse(fail(s"missing ineligible factor: ${prepared.plan.factors}"))
    val materializedTrace = traceByBag(materializedFactor.bagId)
    val supportCte = materializedTrace.supportCte.getOrElse(
      fail(s"materialized trace omitted support CTE: $materializedTrace")
    )
    assertEquals(
      materializedTrace.supportStrategy,
      PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
    )
    assert(rendered.sql.contains(s"\"$supportCte\" AS (\n    SELECT DISTINCT"))
    assert(
      rendered.sql.contains(
        s"FROM \"$supportCte\" AS \"${PostgreSqlNames.factorSupport(materializedFactor.bagId)}\""
      ),
      clues(rendered.sql)
    )
    assertEquals(rendered.statistics.expandedScanCount, 5L)
    assertEquals(rendered.statistics.renderedTermCount, 40L)
    assertEquals(rendered.statistics.cteCount, 5L)
  }

  test("materialized support endpoint proof fails closed when its compiled owned binding is absent") {
    val prepared = triangleMixedPrepared()
    val compiled = compileRendering(prepared)
    val materialized = compiled.bags.find(
      _.supportStrategy ==
        PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
    ).getOrElse(fail(s"missing materialized bag: ${compiled.bags}"))
    val ownedEdgeIds = materialized.ownedOccurrences.map(_.edge.id).toSet
    val descriptors = prepared.query.predicates.collect {
      case predicate
          if predicate.classification
            .isInstanceOf[PredicateClassification.InnerEquality] => predicate
    }
    val target = descriptors.iterator.flatMap { descriptor =>
      descriptor.classification match {
        case equality: PredicateClassification.InnerEquality =>
          Vector(equality.left, equality.right).iterator.flatMap { ref =>
            compiled.edgeByOccurrence.get(ref.relationOccurrenceId).collect {
              case edge if ownedEdgeIds(edge.id) => descriptor -> edge.id
            }
          }
        case _ => Iterator.empty
      }
    }.toVector.headOption.getOrElse(
      fail(s"no inner-equality endpoint is owned by $materialized")
    )
    assertEquals(compiled.terminalInnerEqualities, Vector.empty)

    val corruptedMaterialized = materialized.copy(
      ownedOccurrences = materialized.ownedOccurrences.filterNot(
        _.edge.id == target._2
      )
    )
    val corruptedBags = compiled.bags.map { bag =>
      if (bag.factor.bagId == materialized.factor.bagId) corruptedMaterialized
      else bag
    }
    val retained = PostgreSqlRenderingCompiler
      .retainUnprovedReconstructionEqualities(
        descriptors,
        compiled.innerEqualities,
        prepared.plan,
        corruptedBags,
        compiled.edgeByOccurrence
      )
    assert(
      retained.exists(_.ordinal == target._1.ordinal),
      clues(target, retained)
    )
    assertEquals(
      PostgreSqlRenderingCompiler.retainUnprovedReconstructionEqualities(
        descriptors.dropRight(1),
        compiled.innerEqualities,
        prepared.plan,
        compiled.bags,
        compiled.edgeByOccurrence
      ),
      compiled.innerEqualities
    )
  }

  test("self joins keep EdgeId identity, quote names, and preserve duplicate aliases") {
    val table = TestTable(
      "order\"detail",
      Vector(
        TestColumn("join key", SqlTypeName.INTEGER, nullable = true),
        TestColumn("select", SqlTypeName.VARCHAR, nullable = true)
      ),
      schemaName = Some("Odd Schema")
    )
    val sql =
      """SELECT
        |  "left"."select" AS "duplicate""alias",
        |  "right"."select" AS "duplicate""alias"
        |FROM "Odd Schema"."order""detail" AS "left"
        |JOIN "Odd Schema"."order""detail" AS "right"
        |  ON "left"."join key" = "right"."join key"
        |""".stripMargin
    val prepared = prepareTopOne(sql, Vector(table), 1, requireConnectedCover = true)
    val rendered = render(prepared)
    val modeRenderings = YannakakisReductionMode.Values.map(mode =>
      mode -> renderMode(prepared, mode)
    )

    assertEquals(prepared.hypergraph.edges.map(_.vertices).distinct.size, 1)
    assertEquals(prepared.hypergraph.edgeIds.map(_.value), Vector(0, 1))
    assertEquals(
      rendered.trace.bags.map(_.supportStrategy),
      Vector(PostgreSqlBagSupportStrategy.ElidedByOwnedCover)
    )
    assert(!rendered.sql.contains("CAST("), clues(rendered.sql))
    assert(!rendered.sql.contains("SELECT DISTINCT"), clues(rendered.sql))
    assertEquals(rendered.output.map(_.name), Vector("duplicate\"alias", "duplicate\"alias"))
    assertEquals(
      rendered.trace.bags.flatMap(_.occurrenceColumns).map(_.edgeId).distinct.map(_.value),
      Vector(0, 1)
    )
    assert(
      rendered.sql.contains("\"Odd Schema\".\"order\"\"detail\""),
      clues(rendered.sql)
    )
    assert(rendered.sql.contains(".\"join key\""), clues(rendered.sql))
    assert(rendered.sql.contains(".\"select\""), clues(rendered.sql))
    assertEquals(count(rendered.sql, "AS \"duplicate\"\"alias\""), 2)
    prepared.hypergraph.edges.foreach { edge =>
      val owner = prepared.plan.ownership.find(_.edgeId == edge.id).get.owner
      val relation = prepared.query.relations.find(
        _.id == edge.relationOccurrenceId
      ).get
      val qualifiedName = relation.qualifiedName
        .map(quotedIdentifier)
        .mkString(".")
      val scan =
        s"$qualifiedName AS \"${PostgreSqlNames.factorScan(owner, edge.id)}\""
      assertEquals(count(rendered.sql, scan), 1, clues(scan, rendered.sql))
    }

    val projectedRefs = prepared.query.output.map(_.expression).collect {
      case ScalarExpression.Column(ref, _, _, _) => ref
    }
    assertEquals(projectedRefs.size, 2)
    assertNotEquals(
      projectedRefs.head.relationOccurrenceId,
      projectedRefs.last.relationOccurrenceId
    )
    val finalProjection = finalProjectionSql(rendered.sql)
    val projectedColumns = projectedRefs.map(finalColumnSql(prepared, _))
    assertNotEquals(projectedColumns.head, projectedColumns.last)
    projectedColumns.foreach { column =>
      assertEquals(count(finalProjection, column), 1, clues(column, finalProjection))
    }
    assertEquals(prepared.plan.schedule.bottomUp, Vector.empty)
    assertEquals(prepared.plan.schedule.topDown, Vector.empty)
    modeRenderings.foreach { case (mode, value) =>
      assertEquals(value.sql, rendered.sql)
      assertEquals(value.output, rendered.output)
      assertEquals(value.trace.semijoins, Vector.empty)
      assertEquals(value.trace.reduction.mode, mode)
      assertEquals(value.trace.reduction.physicalReductionCteCount, 0)
      assertEquals(value.statistics, rendered.statistics)
    }
  }

  test("direct column equality is retained as a Boolean output scalar") {
    val tables = Vector(
      TestTable(
        "output_equality_r",
        Vector(
          TestColumn("join_key", SqlTypeName.INTEGER, nullable = true),
          TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
        )
      ),
      TestTable(
        "output_equality_s",
        Vector(
          TestColumn("join_key", SqlTypeName.INTEGER, nullable = true),
          TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
        )
      )
    )
    val prepared = prepareTopOne(
      """SELECT r.payload = s.payload AS "payloads_equal"
        |FROM output_equality_r AS r
        |JOIN output_equality_s AS s ON r.join_key = s.join_key
        |""".stripMargin,
      tables,
      widthBound = 1,
      requireConnectedCover = true
    )
    val (left, right) = prepared.query.output.head.expression match {
      case ScalarExpression.Call(
            "EQUALS",
            "=",
            Vector(
              ScalarExpression.Column(left, _, _, _),
              ScalarExpression.Column(right, _, _, _)
            ),
            _,
            "BOOLEAN",
            true,
            true
          ) => left -> right
      case other => fail(s"expected nullable direct equality output, found $other")
    }

    val rendered = render(prepared)
    assertEquals(
      rendered.output.map(column =>
        (column.ordinal, column.name, column.dataType, column.nullable)
      ),
      Vector((0, "payloads_equal", "BOOLEAN", true))
    )
    val expectedEquality = finalEqualitySql(prepared, left, right)
    val finalProjection = finalProjectionSql(rendered.sql)
    assertEquals(
      count(finalProjection, expectedEquality),
      1,
      clues(expectedEquality, finalProjection)
    )
    assert(
      finalProjection.contains(s"$expectedEquality AS \"payloads_equal\""),
      clues(finalProjection)
    )
  }

  test("repeated bindings retain both positional columns of one occurrence") {
    val tables = Vector(
      TestTable(
        "binding_r",
        Vector(
          TestColumn("a", nullable = true),
          TestColumn("b", nullable = true),
          TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
        )
      ),
      TestTable(
        "binding_s",
        Vector(
          TestColumn("x", nullable = true),
          TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
        )
      )
    )
    val prepared = prepareTopOne(
      """SELECT r.a AS "a", r.b AS "b", r.payload AS "payload"
        |FROM binding_r AS r
        |JOIN binding_s AS s ON r.a = s.x AND r.b = s.x
        |""".stripMargin,
      tables,
      widthBound = 1,
      requireConnectedCover = true
    )
    val rendered = render(prepared)

    val repeated = prepared.hypergraph.edges.head.bindings
    assertEquals(repeated.map(_._2).distinct.size, 1)
    assertEquals(repeated.map(_._1.ordinal), Vector(0, 1))
    val edgeZeroColumns = rendered.trace.bags.flatMap(_.occurrenceColumns)
      .filter(_.edgeId == edgeId(0))
    assertEquals(edgeZeroColumns.map(_.columnId.ordinal), Vector(0, 1, 2))
    assertEquals(edgeZeroColumns.map(_.columnName).distinct.size, 3)
    assert(rendered.sql.contains("\"yr_e0_c0\""), clues(rendered.sql))
    assert(rendered.sql.contains("\"yr_e0_c1\""), clues(rendered.sql))
    val owner = prepared.plan.factors.find(_.ownedEdgeIds.contains(edgeId(0))).get
    assertEquals(
      rendered.trace.bags.find(_.bagId == owner.bagId).get.supportStrategy,
      PostgreSqlBagSupportStrategy.ElidedByOwnedCover
    )
    val repeatedVariableEqualities = ownedBindingGroups(prepared, owner)
      .find(_._1 == repeated.head._2)
      .toVector
      .flatMap { case (_, locations) =>
        locations.headOption.toVector.flatMap(representative =>
          locations.map(location => s"$representative = $location")
        )
      }
    assertEquals(repeatedVariableEqualities.size, 3)
    repeatedVariableEqualities.foreach { equality =>
      assertEquals(count(rendered.sql, equality), 1, clues(equality, rendered.sql))
    }
  }

  test("nullable local self-equality remains an explicit ordinary predicate") {
    val prepared = localSelfEqualityPrepared()
    val local = prepared.query.predicates.collectFirst {
      case descriptor @ PredicateDescriptor(
            _,
            _,
            ScalarExpression.Call(
              "EQUALS",
              _,
              Vector(
                ScalarExpression.Column(left, _, _, _),
                ScalarExpression.Column(right, _, _, _)
              ),
              _,
              _,
              _,
              _
            ),
            _: PredicateClassification.LocalFilter
          ) => descriptor -> (left -> right)
    }.getOrElse(fail(s"missing local equality: ${prepared.query.predicates}"))
    assertEquals(local._2._1, local._2._2)
    val metadata = prepared.query.relations.head.columns(local._2._1.ordinal)
    assert(metadata.nullable)

    val rendered = render(prepared)
    val explicitSelfEquality =
      raw"(?s)\"[^\"]+\"\.\"a\"\s*=\s*\"[^\"]+\"\.\"a\"".r
    assertEquals(explicitSelfEquality.findAllIn(rendered.sql).size, 1)
    assertEquals(
      rendered.trace.bags.map(_.supportStrategy),
      Vector(PostgreSqlBagSupportStrategy.ElidedByOwnedCover)
    )
    assert(!rendered.sql.contains("SELECT DISTINCT"), clues(rendered.sql))
    assert(!rendered.sql.contains("IS NOT DISTINCT FROM"))
  }

  test("legacy direct column equality SQL is byte-stable and never gains literal casts") {
    val prepared = localSelfEqualityPrepared()
    val local = prepared.query.predicates.find(
      _.classification.isInstanceOf[PredicateClassification.LocalFilter]
    ).getOrElse(fail(s"missing local predicate: ${prepared.query.predicates}"))
    val equality = local.expression.asInstanceOf[ScalarExpression.Call]
    val diagnosticMutation = equality.copy(
      operands = equality.operands.map {
        case column: ScalarExpression.Column =>
          column.copy(digest = "hostile_column_digest()")
        case other => other
      },
      digest = "hostile_equality_digest()"
    )
    val mutated = prepared.copy(
      query = replacePredicate(
        prepared.query,
        local.ordinal,
        _.copy(expression = diagnosticMutation)
      )
    )

    val legacySql = render(prepared).sql
    val mutatedSql = render(mutated).sql
    assertEquals(mutatedSql, legacySql)
    assert(!legacySql.contains("CAST("), clues(legacySql))
    assert(!legacySql.contains("hostile_"), clues(legacySql))
  }

  test("typed local integral equality renders structurally and rejects IR mutation") {
    val prepared = localIntegralPrepared("r.a = 7")
    val local = prepared.query.predicates.find(
      _.classification.isInstanceOf[PredicateClassification.LocalFilter]
    ).getOrElse(fail(s"missing local predicate: ${prepared.query.predicates}"))
    val diagnosticOnlyDigest = "hostile_digest_must_never_be_rendered()"
    val equality = local.expression.asInstanceOf[ScalarExpression.Call]
    val column = equality.operands.collectFirst {
      case value: ScalarExpression.Column => value
    }.getOrElse(fail(s"missing direct integral column: $equality"))
    val diagnosticMutation = equality.copy(
      operands = equality.operands.map {
        case value: ScalarExpression.Column =>
          value.copy(digest = diagnosticOnlyDigest)
        case value: ScalarExpression.ExactIntegralLiteral =>
          value.copy(digest = diagnosticOnlyDigest)
        case other => other
      },
      digest = diagnosticOnlyDigest
    )
    val mutatedQuery = replacePredicate(
      prepared.query,
      local.ordinal,
      _.copy(expression = diagnosticMutation)
    )

    val rendered = render(prepared)
    assert(
      rendered.sql.contains("\"a\" = CAST(7 AS INTEGER)"),
      clues(rendered.sql)
    )
    assert(!rendered.sql.contains(diagnosticOnlyDigest), clues(rendered.sql))

    val serialized = new ByteArrayOutputStream()
    val outputStream = new ObjectOutputStream(serialized)
    try outputStream.writeObject(prepared.query)
    finally outputStream.close()
    val queryClassLoader = prepared.query.getClass.getClassLoader
    val inputStream = new ObjectInputStream(
      new ByteArrayInputStream(serialized.toByteArray)
    ) {
      override protected def resolveClass(
          descriptor: ObjectStreamClass
      ): Class[_] =
        try Class.forName(descriptor.getName, false, queryClassLoader)
        catch {
          case _: ClassNotFoundException => super.resolveClass(descriptor)
        }
    }
    val roundTripped =
      try inputStream.readObject().asInstanceOf[QueryIR]
      finally inputStream.close()
    assertEquals(
      render(prepared.copy(query = roundTripped)).sql,
      rendered.sql
    )

    val mutationProblems = errors(
      PostgreSqlBagFactorRenderer.render(
        mutatedQuery,
        prepared.input,
        prepared.plan
      )
    )
    assert(
      mutationProblems.contains(
        PostgreSqlRenderingError.ModelInvariantViolation(
          "local integral source proof is not bound to this QueryIR"
        )
      ),
      clues(mutationProblems)
    )
    assertEquals(
      rendered.trace.bags
        .flatMap(_.occurrenceColumns)
        .count(_.columnId == column.ref.id),
      1
    )

    val symmetricSql = render(localIntegralPrepared("7 = r.a")).sql
    assert(
      symmetricSql.contains("CAST(7 AS INTEGER) = ") &&
        symmetricSql.contains(".\"a\""),
      clues(symmetricSql)
    )

    val widenedSql = render(localIntegralPrepared("r.a = 2147483648")).sql
    assert(
      widenedSql.contains(".\"a\" = CAST(2147483648 AS BIGINT)") &&
        widenedSql.contains("CAST(2147483648 AS BIGINT)"),
      clues(widenedSql)
    )
    assert(!widenedSql.contains("CAST(\"yr_f_"), clues(widenedSql))
  }

  test("typed integral predicates keep SMALLINT INTEGER and BIGINT columns statistics-visible") {
    final case class ExpectedIntegralSql(
        sqlType: SqlTypeName,
        columnSourceType: String,
        columnTargetType: String,
        literalSourceType: String,
        literalTargetType: String,
        literalSql: String
    )
    val cases = Vector(
      ExpectedIntegralSql(
        SqlTypeName.SMALLINT,
        "SMALLINT",
        "INTEGER",
        "INTEGER",
        "INTEGER",
        "CAST(7 AS INTEGER)"
      ),
      ExpectedIntegralSql(
        SqlTypeName.INTEGER,
        "INTEGER",
        "INTEGER",
        "INTEGER",
        "INTEGER",
        "CAST(7 AS INTEGER)"
      ),
      ExpectedIntegralSql(
        SqlTypeName.BIGINT,
        "BIGINT",
        "BIGINT",
        "BIGINT",
        "BIGINT",
        "CAST(7 AS BIGINT)"
      )
    )

    cases.foreach { expected =>
      val prepared = localIntegralPrepared("r.a = 7", expected.sqlType)
      val compiled = compileRendering(prepared)
      val equality = compiled.localPredicatesByOccurrence.valuesIterator
        .flatten
        .collectFirst {
          case CompiledPostgreSqlPredicate(
                _,
                value: CompiledPostgreSqlScalar.LocalIntegralEquality
              ) => value
        }
        .getOrElse(fail(s"missing compiled integral equality for $expected"))
      assertEquals(equality.columnSourceSqlTypeName, expected.columnSourceType)
      assertEquals(equality.columnTargetSqlTypeName, expected.columnTargetType)
      assertEquals(equality.literalSourceSqlTypeName, expected.literalSourceType)
      assertEquals(equality.literalTargetSqlTypeName, expected.literalTargetType)

      val edge = prepared.hypergraph.edges.find(
        _.relationOccurrenceId == equality.column.relationOccurrenceId
      ).get
      val owner = prepared.plan.ownership.find(_.edgeId == edge.id).get.owner
      val sourceColumn =
        s"${quotedIdentifier(PostgreSqlNames.factorScan(owner, edge.id))}.\"a\""
      val expectedPredicate =
        s"($sourceColumn = ${expected.literalSql})"
      val sql = render(prepared).sql
      assertEquals(count(sql, expectedPredicate), 1, clues(expected, sql))
      assertEquals(count(sql, s"CAST($sourceColumn"), 0, clues(expected, sql))
    }
  }

  test("statistics-safe v2 is default while legacy v1 remains explicitly reproducible") {
    val literalPrepared = localIntegralPrepared("r.a = 2147483648")
    val safeLiteral = render(literalPrepared)
    val legacyLiteral = renderWithEnumerationProfile(
      literalPrepared,
      PostgreSqlEnumerationRenderingProfile.LegacyV1
    )
    assertEquals(
      PostgreSqlEnumerationRenderingProfile.default,
      PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
    )
    assertNotEquals(safeLiteral.sql, legacyLiteral.sql)
    assert(
      safeLiteral.sql.contains(".\"a\" = CAST(2147483648 AS BIGINT)"),
      clues(safeLiteral.sql)
    )
    assert(
      legacyLiteral.sql.contains(
        ".\"a\" AS BIGINT) = CAST(2147483648 AS BIGINT)"
      ),
      clues(legacyLiteral.sql)
    )

    val reconstructionPrepared = triangleMixedPrepared()
    val safeReconstruction = compileRendering(reconstructionPrepared)
    val legacyReconstruction = compileRenderingWithEnumerationProfile(
      reconstructionPrepared,
      PostgreSqlEnumerationRenderingProfile.LegacyV1
    )
    assertEquals(safeReconstruction.terminalInnerEqualities, Vector.empty)
    assertEquals(
      legacyReconstruction.terminalInnerEqualities,
      legacyReconstruction.innerEqualities
    )

    val safeDiagnostics = renderDiagnosticsWithEnumerationProfile(
      literalPrepared,
      PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
    )
    val legacyDiagnostics = renderDiagnosticsWithEnumerationProfile(
      literalPrepared,
      PostgreSqlEnumerationRenderingProfile.LegacyV1
    )
    assertEquals(safeDiagnostics.renderedQuery, safeLiteral)
    assertEquals(legacyDiagnostics.renderedQuery, legacyLiteral)
    assertEquals(
      safeDiagnostics.statements.map(_.stageId),
      legacyDiagnostics.statements.map(_.stageId)
    )
    assert(
      safeDiagnostics.statements
        .zip(legacyDiagnostics.statements)
        .forall { case (safe, legacy) =>
          safe.sql != legacy.sql &&
          safe.statementSha256 != legacy.statementSha256
        },
      clues(safeDiagnostics.statements, legacyDiagnostics.statements)
    )
  }

  test("typed local integral proof cannot be omitted harvested or transplanted") {
    val prepared = localIntegralPrepared("r.a = 7")
    val uncertified = prepared.query.copy(
      sourceProof = QuerySourceProof.Uncertified
    )

    val problems = errors(
      PostgreSqlBagFactorRenderer.render(
        uncertified,
        prepared.input,
        prepared.plan
      )
    )
    assert(
      problems.exists {
        case PostgreSqlRenderingError.PredicateContractViolation(_, detail) =>
          detail.contains("analyzer-issued source proof")
        case _ => false
      },
      clues(problems)
    )

    def assertUnbound(query: QueryIR, clue: Any): Unit = {
      val transplantProblems = errors(
        PostgreSqlBagFactorRenderer.render(
          query,
          prepared.input,
          prepared.plan
        )
      )
      assert(
        transplantProblems.contains(
          PostgreSqlRenderingError.ModelInvariantViolation(
            "local integral source proof is not bound to this QueryIR"
          )
        ),
        clues(clue, transplantProblems)
      )
    }

    val explicitCastSql =
      "SELECT r.a AS \"a\" " +
        "FROM local_r AS r JOIN local_s AS s " +
        "ON r.join_key = s.join_key " +
        "WHERE r.a = CAST(7 AS INTEGER)"
    val explicitCastRoot = localIntegralPlanner
      .plan(explicitCastSql)
      .fold(problem => fail(problem.toString), identity)
    val explicitCastReport = CalciteQueryAnalyzer.analyze(explicitCastRoot)
    assert(!explicitCastReport.isSupported, clues(explicitCastReport.diagnostics))
    assertEquals(
      explicitCastReport.query.sourceProof,
      QuerySourceProof.Uncertified
    )

    val proofOnlyTransplant = explicitCastReport.query.copy(
      sourceProof = prepared.query.sourceProof
    )
    assertUnbound(proofOnlyTransplant, "proof-only explicit-CAST transplant")

    val certifiedLocal = prepared.query.predicates.find(
      _.classification.isInstanceOf[PredicateClassification.LocalFilter]
    ).getOrElse(
      fail(s"missing certified local predicate: ${prepared.query.predicates}")
    )
    val castLocal = explicitCastReport.query.predicates.find(
      _.classification.isInstanceOf[PredicateClassification.LocalFilter]
    ).getOrElse(
      fail(
        s"missing explicit-CAST local predicate: ${explicitCastReport.query.predicates}"
      )
    )
    val proofAndNodeTransplant = replacePredicate(
      explicitCastReport.query,
      castLocal.ordinal,
      _.copy(expression = certifiedLocal.expression)
    ).copy(sourceProof = prepared.query.sourceProof)
    assertUnbound(
      proofAndNodeTransplant,
      "proof-plus-certified-node explicit-CAST transplant"
    )

    val independentlyCertified = localIntegralPrepared("r.a = 7")
    assertEquals(independentlyCertified.query, prepared.query)
    assertUnbound(
      independentlyCertified.query.copy(sourceProof = prepared.query.sourceProof),
      "proof copied across separately analyzed equal SQL"
    )
  }

  test("retained local integral proof rejects value column orientation and order mutations") {
    val prepared = localIntegralPrepared("r.a = 7")
    val local = prepared.query.predicates.find(
      _.classification.isInstanceOf[PredicateClassification.LocalFilter]
    ).getOrElse(fail(s"missing local predicate: ${prepared.query.predicates}"))
    val equality = local.expression.asInstanceOf[ScalarExpression.Call]
    val column = equality.operands.collectFirst {
      case value: ScalarExpression.Column => value
    }.getOrElse(fail(s"missing direct integral column: $equality"))
    val literal = equality.operands.collectFirst {
      case value: ScalarExpression.ExactIntegralLiteral => value
    }.getOrElse(fail(s"missing exact integral literal: $equality"))
    val joinKey = prepared.query.relations.head.columns
      .find(_.sourceName == "join_key")
      .map(metadata =>
        ScalarExpression.Column(
          metadata.ref,
          metadata.sourceName,
          metadata.dataType,
          metadata.nullable
        )
      )
      .getOrElse(fail("missing join_key metadata"))

    val mutations = Vector(
      equality.copy(
        operands = Vector(column, literal.copy(value = literal.value + 1L))
      ),
      equality.copy(operands = Vector(joinKey, literal)),
      equality.copy(operands = Vector(literal, column))
    )
    def assertProofMismatch(query: QueryIR, clue: Any): Unit = {
      val problems = errors(
        PostgreSqlBagFactorRenderer.render(
          query,
          prepared.input,
          prepared.plan
        )
      )
      assert(
        problems.exists {
          case PostgreSqlRenderingError.ModelInvariantViolation(detail) =>
            detail.contains("source proof value/order bindings")
          case _ => false
        },
        clues(clue, problems)
      )
    }

    mutations.foreach { mutation =>
      assertProofMismatch(replacePredicate(
        prepared.query,
        local.ordinal,
        _.copy(expression = mutation)
      ), mutation)
    }

    val ordered = localIntegralPrepared("r.a = 7 AND r.join_key = 8")
    val orderedLocals = ordered.query.predicates.filter { predicate =>
      predicate.expression match {
        case call: ScalarExpression.Call =>
          call.columnRefs.size == 1 &&
            call.operands.exists(
              _.isInstanceOf[ScalarExpression.ExactIntegralLiteral]
            )
        case _ => false
      }
    }
    assertEquals(orderedLocals.size, 2)
    val first = orderedLocals(0)
    val second = orderedLocals(1)
    val swappedFirst = replacePredicate(
      ordered.query,
      first.ordinal,
      _.copy(expression = second.expression)
    )
    val swapped = replacePredicate(
      swappedFirst,
      second.ordinal,
      _.copy(expression = first.expression)
    )
    val orderProblems = errors(
      PostgreSqlBagFactorRenderer.render(
        swapped,
        ordered.input,
        ordered.plan
      )
    )
    assert(
      orderProblems.exists {
        case PostgreSqlRenderingError.ModelInvariantViolation(detail) =>
          detail.contains("source proof value/order bindings")
        case _ => false
      },
      clues(orderProblems)
    )
  }

  test("local integral proof is rejected outside enumeration projection") {
    val literal = localIntegralPrepared("r.a = 7")
    val aggregate = localIntegralAggregatePrepared()
    val literalPredicate = literal.query.predicates.find(
      _.classification.isInstanceOf[PredicateClassification.LocalFilter]
    ).getOrElse(fail(s"missing literal predicate: ${literal.query.predicates}"))
    val aggregatePredicate = aggregate.query.predicates.find(
      _.classification.isInstanceOf[PredicateClassification.LocalFilter]
    ).getOrElse(fail(s"missing aggregate predicate: ${aggregate.query.predicates}"))
    val forged = replacePredicate(
      aggregate.query,
      aggregatePredicate.ordinal,
      _.copy(expression = literalPredicate.expression)
    ).copy(sourceProof = literal.query.sourceProof)

    val problems = errors(
      PostgreSqlBagFactorRenderer.render(
        forged,
        aggregate.input,
        aggregate.plan
      )
    )
    assert(
      problems.contains(
        PostgreSqlRenderingError.ModelInvariantViolation(
          "local integral source proof is valid only for enumeration projection"
        )
      ),
      clues(problems)
    )
  }

  test("typed local integral metadata, range, coercion, and classification tampering fail closed") {
    val prepared = localIntegralPrepared("r.a = 7")
    val local = prepared.query.predicates.find(
      _.classification.isInstanceOf[PredicateClassification.LocalFilter]
    ).getOrElse(fail(s"missing local predicate: ${prepared.query.predicates}"))
    val column = local.expression
      .asInstanceOf[ScalarExpression.Call]
      .operands
      .head
      .asInstanceOf[ScalarExpression.Column]

    def expression(
        literal: ScalarExpression,
        columnExpression: ScalarExpression = column,
        dataType: String = "BOOLEAN",
        nullable: Boolean = true
    ): ScalarExpression.Call =
      ScalarExpression.Call(
        sqlKind = "EQUALS",
        operatorName = "=",
        operands = Vector(columnExpression, literal),
        digest = "diagnostic-only",
        dataType = dataType,
        nullable = nullable,
        deterministic = true
      )

    val invalidExpressions = Vector(
      expression(
        ScalarExpression.ExactIntegralLiteral(
          7L,
          "INTEGER",
          "bad-type",
          "BIGINT NOT NULL"
        )
      ),
      expression(
        ScalarExpression.ExactIntegralLiteral(
          40000L,
          "SMALLINT",
          "out-of-range",
          "SMALLINT NOT NULL"
        )
      ),
      expression(
        ScalarExpression.ExactIntegralLiteral(
          2147483648L,
          "INTEGER",
          "integer-out-of-range",
          "INTEGER NOT NULL"
        )
      ),
      expression(
        ScalarExpression.ExactIntegralLiteral(
          7L,
          "DECIMAL",
          "unsupported-literal-type",
          "DECIMAL NOT NULL"
        )
      ),
      expression(
        ScalarExpression.ExactIntegralLiteral(
          7L,
          "INTEGER",
          "forged-column-metadata",
          "INTEGER NOT NULL"
        ),
        columnExpression = column.copy(dataType = "BIGINT")
      ),
      expression(
        ScalarExpression.ExactIntegralLiteral(
          7L,
          "INTEGER",
          "bad-result-nullability",
          "INTEGER NOT NULL"
        ),
        nullable = false,
        dataType = "BOOLEAN NOT NULL"
      ),
      expression(
        ScalarExpression.ExactIntegralWideningCast(
          ScalarExpression.ExactIntegralLiteral(
            7L,
            "INTEGER",
            "unnecessary-literal-cast-input",
            "INTEGER NOT NULL"
          ),
          sourceSqlTypeName = "INTEGER",
          targetSqlTypeName = "BIGINT",
          digest = "unnecessary-literal-cast",
          dataType = "BIGINT NOT NULL",
          nullable = false
        ),
        columnExpression = ScalarExpression.ExactIntegralWideningCast(
          column,
          sourceSqlTypeName = "INTEGER",
          targetSqlTypeName = "BIGINT",
          digest = "unnecessary-column-cast",
          dataType = "BIGINT",
          nullable = true
        )
      ),
      expression(
        ScalarExpression.ExactIntegralLiteral(
          7L,
          "SMALLINT",
          "narrowing-column-literal",
          "SMALLINT NOT NULL"
        ),
        columnExpression = ScalarExpression.ExactIntegralWideningCast(
          column,
          sourceSqlTypeName = "INTEGER",
          targetSqlTypeName = "SMALLINT",
          digest = "narrowing-column-cast",
          dataType = "SMALLINT",
          nullable = true
        )
      ),
      expression(
        ScalarExpression.ExactIntegralWideningCast(
          ScalarExpression.ExactIntegralLiteral(
            7L,
            "INTEGER",
            "forged-nullable-literal-input",
            "INTEGER NOT NULL"
          ),
          sourceSqlTypeName = "INTEGER",
          targetSqlTypeName = "BIGINT",
          digest = "forged-nullable-literal-cast",
          dataType = "BIGINT",
          nullable = true
        ),
        columnExpression = ScalarExpression.ExactIntegralWideningCast(
          column,
          sourceSqlTypeName = "INTEGER",
          targetSqlTypeName = "BIGINT",
          digest = "valid-column-cast",
          dataType = "BIGINT",
          nullable = true
        )
      ),
      expression(
        ScalarExpression.ExactIntegralWideningCast(
          ScalarExpression.ExactIntegralLiteral(
            7L,
            "INTEGER",
            "valid-literal-input",
            "INTEGER NOT NULL"
          ),
          sourceSqlTypeName = "INTEGER",
          targetSqlTypeName = "BIGINT",
          digest = "valid-literal-cast",
          dataType = "BIGINT NOT NULL",
          nullable = false
        ),
        columnExpression = ScalarExpression.ExactIntegralWideningCast(
          column,
          sourceSqlTypeName = "INTEGER",
          targetSqlTypeName = "BIGINT",
          digest = "forged-column-nullability",
          dataType = "BIGINT NOT NULL",
          nullable = false
        )
      )
    )

    invalidExpressions.foreach { invalid =>
      val query = replacePredicate(
        prepared.query,
        local.ordinal,
        _.copy(expression = invalid)
      )
      val problems = errors(
        PostgreSqlBagFactorRenderer.render(
          query,
          prepared.input,
          prepared.plan
        )
      )
      assert(
        problems.exists(problem =>
          problem.isInstanceOf[PostgreSqlRenderingError.UnsupportedScalar] ||
            problem.isInstanceOf[
              PostgreSqlRenderingError.PredicateContractViolation
            ]
        ),
        clues(invalid, problems)
      )
    }

    intercept[IllegalArgumentException] {
      replacePredicate(
        prepared.query,
        local.ordinal,
        _.copy(
          expression = expression(
            ScalarExpression.ExactIntegralLiteral(
              7L,
              "INTEGER",
              "operator-tamper",
              "INTEGER NOT NULL"
            )
          ).copy(operatorName = "<>")
        )
      )
    }
  }

  test("predicate classifications inconsistent with their expressions fail closed") {
    val triangle = trianglePrepared()
    val inner = triangle.query.predicates.collect {
      case predicate
          if predicate.classification
            .isInstanceOf[PredicateClassification.InnerEquality] => predicate
    }
    val mismatchedInner = replacePredicate(
      triangle.query,
      inner.head.ordinal,
      _.copy(expression = inner(1).expression)
    )
    val innerErrors = errors(
      PostgreSqlBagFactorRenderer.render(
        mismatchedInner,
        triangle.input,
        triangle.plan
      )
    )
    assert(innerErrors.exists(_.isInstanceOf[
      PostgreSqlRenderingError.PredicateContractViolation
    ]))

    val local = localSelfEqualityPrepared()
    val localPredicate = local.query.predicates.find(
      _.classification.isInstanceOf[PredicateClassification.LocalFilter]
    ).get
    val wrongOccurrence = local.query.relations(1).id
    val mismatchedLocal = replacePredicate(
      local.query,
      localPredicate.ordinal,
      _.copy(classification = PredicateClassification.LocalFilter(wrongOccurrence))
    )
    val localErrors = errors(
      PostgreSqlBagFactorRenderer.render(
        mismatchedLocal,
        local.input,
        local.plan
      )
    )
    assert(localErrors.exists(_.isInstanceOf[
      PostgreSqlRenderingError.PredicateContractViolation
    ]))
  }

  test("forged scalar, type, and output metadata fail closed") {
    val prepared = trianglePrepared()
    val inner = prepared.query.predicates.collectFirst {
      case predicate
          if predicate.classification
            .isInstanceOf[PredicateClassification.InnerEquality] => predicate
    }.get
    val forgedOperator = replacePredicate(
      prepared.query,
      inner.ordinal,
      predicate =>
        predicate.copy(
          expression = predicate.expression
            .asInstanceOf[ScalarExpression.Call]
            .copy(operatorName = "<>")
        )
    )
    assert(errors(
      PostgreSqlBagFactorRenderer.render(
        forgedOperator,
        prepared.input,
        prepared.plan
      )
    ).exists(_.isInstanceOf[PostgreSqlRenderingError.UnsupportedScalar]))

    val forgedColumnScalar = replaceProject(prepared.query, 0, project => {
      val column = project.expression.asInstanceOf[ScalarExpression.Column]
      project.copy(
        expression = column.copy(dataType = "BIGINT"),
        dataType = "BIGINT"
      )
    })
    assert(errors(
      PostgreSqlBagFactorRenderer.render(
        forgedColumnScalar,
        prepared.input,
        prepared.plan
      )
    ).exists(_.isInstanceOf[PostgreSqlRenderingError.UnsupportedScalar]))

    val forgedOutputMetadata = replaceProject(
      prepared.query,
      0,
      _.copy(dataType = "BIGINT")
    )
    assert(errors(
      PostgreSqlBagFactorRenderer.render(
        forgedOutputMetadata,
        prepared.input,
        prepared.plan
      )
    ).exists(_.isInstanceOf[PostgreSqlRenderingError.UnsupportedScalar]))

    val structuralColumn = prepared.hypergraph.edges.head.bindings.head._1
    val forgedStructuralMetadata = replaceRelations(prepared.query) { relation =>
      relation.copy(
        columns = relation.columns.map { metadata =>
          if (metadata.ref.id == structuralColumn)
            metadata.copy(dataType = "INTEGER(10)")
          else metadata
        }
      )
    }
    assert(errors(
      PostgreSqlBagFactorRenderer.render(
        forgedStructuralMetadata,
        prepared.input,
        prepared.plan
      )
    ).exists(_.isInstanceOf[
      PostgreSqlRenderingError.InconsistentStructuralTypeMetadata
    ]))
  }

  test("noncontiguous output positions fail before rendering") {
    val prepared = trianglePrepared()
    val join = prepared.query.root match {
      case ProjectIR(value: JoinIR, _) => value
      case other => fail(s"expected a projected join, found $other")
    }
    val malformedSlots = join.output.updated(
      0,
      join.output.head.copy(ordinal = join.output.size)
    )
    val malformed = QueryIR(
      root = join.copy(output = malformedSlots),
      relations = prepared.query.relations,
      predicates = prepared.query.predicates,
      outputNames = malformedSlots.indices.map(index => s"column_$index").toVector
    )
    assertEquals(
      errors(
        PostgreSqlBagFactorRenderer.render(
          malformed,
          prepared.input,
          prepared.plan
        )
      ),
      Vector(
        PostgreSqlRenderingError.ModelInvariantViolation(
          "QueryIR output slots must use stable contiguous positional ordinals"
        )
      )
    )
  }

  test("unsupported structural types fail while VARCHAR payloads remain renderable") {
    val stringTables = Vector(
      TestTable("string_r", Vector(TestColumn("key", SqlTypeName.VARCHAR, true))),
      TestTable("string_s", Vector(TestColumn("key", SqlTypeName.VARCHAR, true)))
    )
    val prepared = prepareTopOne(
      """SELECT r.key AS "key"
        |FROM string_r AS r JOIN string_s AS s ON r.key = s.key
        |""".stripMargin,
      stringTables,
      widthBound = 1,
      requireConnectedCover = true
    )
    val failures = errors(
      PostgreSqlBagFactorRenderer.render(
        prepared.query,
        prepared.input,
        prepared.plan
      )
    )
    assertEquals(
      failures.collect {
        case error: PostgreSqlRenderingError.UnsupportedStructuralType =>
          error.sqlTypeName
      },
      Vector("VARCHAR")
    )

    assert(render(trianglePrepared()).output.exists(_.name == "r_payload"))
  }

  test("query provenance and every bag-factor-plan field corruption fail closed") {
    val prepared = depthBranchPrepared()
    val plan = prepared.plan
    val otherPrepared = prepareTopOne(
      depthBranchSql.replace("y.c = e.c", "x.a = e.c"),
      depthBranchTables,
      widthBound = 1,
      requireConnectedCover = true
    )
    assertEquals(otherPrepared.hypergraph.edgeIds, prepared.hypergraph.edgeIds)
    assertEquals(
      otherPrepared.hypergraph.variables,
      prepared.hypergraph.variables
    )
    assertNotEquals(otherPrepared.hypergraph, prepared.hypergraph)
    val queryErrors = errors(
      PostgreSqlBagFactorRenderer.render(
        otherPrepared.query,
        prepared.input,
        plan
      )
    )
    assert(queryErrors.exists(_.isInstanceOf[
      PostgreSqlRenderingError.QueryHypergraphMismatch
    ]))

    val alternativeRoot = plan.factors.find(_.bagId != plan.root).get.bagId
    val rerooted = coveredDecomposition(
      alternativeRoot.value,
      plan.decomposition.bags,
      plan.decomposition.treeEdges
    )
    val corruptions = Vector(
      plan.copy(sourceHypergraph = QueryHypergraph.empty),
      plan.copy(sourceWidthBound = plan.sourceWidthBound + 1),
      plan.copy(requireConnectedCover = !plan.requireConnectedCover),
      plan.copy(decomposition = rerooted),
      plan.copy(
        certificate = plan.certificate.copy(
          bagCount = plan.certificate.bagCount + 1
        )
      ),
      plan.copy(root = alternativeRoot),
      plan.copy(
        factors = plan.factors.updated(
          0,
          plan.factors.head.copy(depth = plan.factors.head.depth + 1)
        )
      ),
      plan.copy(ownership = plan.ownership.drop(1)),
      plan.copy(arcs = plan.arcs.drop(1)),
      plan.copy(
        schedule = plan.schedule.copy(bottomUp = plan.schedule.bottomUp.drop(1))
      )
    )
    corruptions.foreach { corrupted =>
      val failures = errors(
        PostgreSqlBagFactorRenderer.render(prepared.query, prepared.input, corrupted)
      )
      assertEquals(failures.size, 1)
      assert(failures.head.isInstanceOf[
        PostgreSqlRenderingError.PlanValidationFailure
      ], clues(failures))
    }
  }

  test("missing values, invalid identifiers, and generated-name collisions fail closed") {
    val prepared = trianglePrepared()
    val missing = errors(
      PostgreSqlBagFactorRenderer.render(null, null, null, null, null)
    )
    assertEquals(missing.collect {
      case error: PostgreSqlRenderingError.MissingValue => error.path
    }.toSet, Set(
      "source QueryIR",
      "candidate-search input",
      "bag-factor plan",
      "PostgreSQL rendering limits",
      "bag-factor planning validation limits"
    ))

    val blankName = replaceRelations(prepared.query) { relation =>
      if (relation.id == prepared.query.relations.head.id)
        relation.copy(qualifiedName = Vector(" "))
      else relation
    }
    assert(errors(
      PostgreSqlBagFactorRenderer.render(blankName, prepared.input, prepared.plan)
    ).exists(_.isInstanceOf[PostgreSqlRenderingError.InvalidIdentifier]))

    val elidedTrace = render(prepared).trace.bags.head
    assertEquals(elidedTrace.supportCte, None)
    val unusedSupportName = PostgreSqlNames.support(elidedTrace.bagId)
    val unusedSupportNameQuery = replaceRelations(prepared.query) { relation =>
      if (relation.id == prepared.query.relations.head.id)
        relation.copy(qualifiedName = Vector(unusedSupportName))
      else relation
    }
    val unusedSupportRendered = PostgreSqlBagFactorRenderer.render(
      unusedSupportNameQuery,
      prepared.input,
      prepared.plan
    )
    assert(unusedSupportRendered.isRight, clues(unusedSupportRendered))

    val collidingInitial = replaceRelations(prepared.query) { relation =>
      if (relation.id == prepared.query.relations.head.id)
        relation.copy(qualifiedName = Vector(elidedTrace.initialFactorCte))
      else relation
    }
    assert(errors(
      PostgreSqlBagFactorRenderer.render(
        collidingInitial,
        prepared.input,
        prepared.plan
      )
    ).exists(_.isInstanceOf[PostgreSqlRenderingError.GeneratedNameCollision]))

    val materialized = triangleMixedPrepared()
    val materializedSupport = render(materialized).trace.bags
      .flatMap(_.supportCte)
      .head
    val collidingSupport = replaceRelations(materialized.query) { relation =>
      if (relation.id == materialized.query.relations.head.id)
        relation.copy(qualifiedName = Vector(materializedSupport))
      else relation
    }
    assert(errors(
      PostgreSqlBagFactorRenderer.render(
        collidingSupport,
        materialized.input,
        materialized.plan
      )
    ).exists(_.isInstanceOf[PostgreSqlRenderingError.GeneratedNameCollision]))
  }

  test("all rendering resources accept the exact count and fail one below") {
    val prepared = trianglePrepared()
    val baseline = render(prepared)
    val stats = baseline.statistics
    assertEquals(stats.expandedScanCount, 3L)
    assertEquals(stats.renderedTermCount, 24L)
    assertEquals(stats.cteCount, 1L)
    val exact = renderingLimits(
      stats.expandedScanCount,
      stats.renderedTermCount,
      stats.cteCount,
      stats.sqlCharacterCount
    )
    assertEquals(
      PostgreSqlBagFactorRenderer.render(
        prepared.query,
        prepared.input,
        prepared.plan,
        exact
      ),
      Right(baseline)
    )

    val boundaries = Vector(
      PostgreSqlRenderingResource.ExpandedScans -> renderingLimits(
        stats.expandedScanCount - 1,
        stats.renderedTermCount,
        stats.cteCount,
        stats.sqlCharacterCount
      ),
      PostgreSqlRenderingResource.RenderedTerms -> renderingLimits(
        stats.expandedScanCount,
        stats.renderedTermCount - 1,
        stats.cteCount,
        stats.sqlCharacterCount
      ),
      PostgreSqlRenderingResource.SqlCharacters -> renderingLimits(
        stats.expandedScanCount,
        stats.renderedTermCount,
        stats.cteCount,
        stats.sqlCharacterCount - 1
      )
    )
    boundaries.foreach { case (resource, limits) =>
      val failures = errors(
        PostgreSqlBagFactorRenderer.render(
          prepared.query,
          prepared.input,
          prepared.plan,
          limits
        )
      )
      assertEquals(failures.size, 1)
      failures.head match {
        case PostgreSqlRenderingError.ResourceLimitExceeded(
              actualResource,
              required,
              limit
            ) =>
          assertEquals(actualResource, resource)
          assertEquals(required, BigInt(limit + 1L))
        case other => fail(s"expected resource failure, found $other")
      }
    }

    val mixedPrepared = triangleMixedPrepared()
    val mixedBaseline = render(mixedPrepared)
    val mixedStats = mixedBaseline.statistics
    val exactMixed = renderingLimits(
      mixedStats.expandedScanCount,
      mixedStats.renderedTermCount,
      mixedStats.cteCount,
      mixedStats.sqlCharacterCount
    )
    assertEquals(
      PostgreSqlBagFactorRenderer.render(
        mixedPrepared.query,
        mixedPrepared.input,
        mixedPrepared.plan,
        exactMixed
      ),
      Right(mixedBaseline)
    )
    val cteFailures = errors(
      PostgreSqlBagFactorRenderer.render(
        mixedPrepared.query,
        mixedPrepared.input,
        mixedPrepared.plan,
        renderingLimits(
          mixedStats.expandedScanCount,
          mixedStats.renderedTermCount,
          mixedStats.cteCount - 1L,
          mixedStats.sqlCharacterCount
        )
      )
    )
    assertEquals(
      cteFailures,
      Vector(
        PostgreSqlRenderingError.ResourceLimitExceeded(
          PostgreSqlRenderingResource.Ctes,
          BigInt(mixedStats.cteCount),
          mixedStats.cteCount - 1L
        )
      )
    )

    assertEquals(
      PostgreSqlRenderingLimits.create(0, 0, 0, 0).swap.toOption.get.size,
      4
    )
  }

  private def trianglePrepared(): PreparedQuery =
    prepareTopOne(
      triangleSql,
      triangleTables,
      widthBound = 2,
      requireConnectedCover = true
    )

  private def chainPrepared(widthBound: Int): PreparedQuery =
    prepareTopOne(
      chainSql,
      chainTables,
      widthBound,
      requireConnectedCover = true
    )

  private def eligibleTwoBagPrepared(): PreparedQuery = {
    val base = twoBagBase()
    val leftEdge = base.hypergraph.edges(0)
    val middleEdge = base.hypergraph.edges(1)
    val decomposition = coveredDecomposition(
      root = 10,
      bags = Vector(
        coveredBag(
          10,
          leftEdge.vertices.map(_.value),
          Vector(leftEdge.id.value)
        ),
        coveredBag(
          20,
          middleEdge.vertices.map(_.value),
          Vector(middleEdge.id.value)
        )
      ),
      edges = Vector(treeEdge(10, 20))
    )
    withDecomposition(base.report, base.hypergraph, base.input, decomposition)
  }

  private def triangleMixedPrepared(): PreparedQuery = {
    val base = trianglePrepared()
    val decomposition = coveredDecomposition(
      root = 10,
      bags = Vector(
        coveredBag(
          10,
          Vector(0, 1),
          Vector(0)
        ),
        coveredBag(
          20,
          Vector(0, 1, 2),
          Vector(0, 1)
        )
      ),
      edges = Vector(treeEdge(10, 20))
    )
    withDecomposition(base.report, base.hypergraph, base.input, decomposition)
  }

  private def twoBagBase(): PreparedQuery = {
    val tables = Vector(
      TestTable(
        "elision_l",
        Vector(
          TestColumn("a", nullable = true),
          TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
        )
      ),
      TestTable(
        "elision_m",
        Vector(
          TestColumn("a", nullable = true),
          TestColumn("b", nullable = true),
          TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
        )
      ),
      TestTable(
        "elision_r",
        Vector(
          TestColumn("b", nullable = true),
          TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
        )
      )
    )
    prepareTopOne(
      """SELECT
        |  l.a AS "a",
        |  m.b AS "b",
        |  l.payload AS "l_payload",
        |  m.payload AS "m_payload",
        |  r.payload AS "r_payload"
        |FROM elision_l AS l
        |JOIN elision_m AS m ON l.a = m.a
        |JOIN elision_r AS r ON m.b = r.b
        |""".stripMargin,
      tables,
      widthBound = 1,
      requireConnectedCover = true
    )
  }

  private def localSelfEqualityPrepared(): PreparedQuery = {
    val tables = Vector(
      TestTable(
        "local_r",
        Vector(
          TestColumn("a", SqlTypeName.INTEGER, nullable = true),
          TestColumn("join_key", SqlTypeName.INTEGER, nullable = true)
        )
      ),
      TestTable(
        "local_s",
        Vector(TestColumn("join_key", SqlTypeName.INTEGER, nullable = true))
      )
    )
    prepareTopOne(
      """SELECT r.a AS "a"
        |FROM local_r AS r
        |JOIN local_s AS s ON r.join_key = s.join_key
        |WHERE r.a = r.a
        |""".stripMargin,
      tables,
      widthBound = 1,
      requireConnectedCover = true
    )
  }

  private def buildLocalIntegralPlanner(
      valueSqlType: SqlTypeName
  ): CalciteSqlPlanner = {
    val schema = Frameworks.createRootSchema(true)
    schema.add(
      "local_r",
      new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType = {
          val nullableValue = factory.createTypeWithNullability(
            factory.createSqlType(valueSqlType),
            true
          )
          val nullableInteger = factory.createTypeWithNullability(
            factory.createSqlType(SqlTypeName.INTEGER),
            true
          )
          factory
            .builder()
            .add("a", nullableValue)
            .add("join_key", nullableInteger)
            .build()
        }
      }
    )
    schema.add(
      "local_s",
      new AbstractTable {
        override def getRowType(factory: RelDataTypeFactory): RelDataType = {
          val nullableInteger = factory.createTypeWithNullability(
            factory.createSqlType(SqlTypeName.INTEGER),
            true
          )
          factory.builder().add("join_key", nullableInteger).build()
        }
      }
    )
    CalciteSqlPlanner
      .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
      .fold(problem => fail(problem.toString), identity)
  }

  private lazy val localIntegralPlanner =
    buildLocalIntegralPlanner(SqlTypeName.INTEGER)

  private def localIntegralPrepared(
      predicate: String,
      valueSqlType: SqlTypeName = SqlTypeName.INTEGER
  ): PreparedQuery =
    prepareLocalIntegralSource(
      s"""SELECT r.a AS "a"
         |FROM local_r AS r
         |JOIN local_s AS s ON r.join_key = s.join_key
         |WHERE $predicate
         |""".stripMargin,
      if (valueSqlType == SqlTypeName.INTEGER) localIntegralPlanner
      else buildLocalIntegralPlanner(valueSqlType)
    )

  private def localIntegralAggregatePrepared(): PreparedQuery =
    prepareLocalIntegralSource(
      """SELECT COUNT(*) AS "count_result"
        |FROM local_r AS r
        |JOIN local_s AS s ON r.join_key = s.join_key
        |WHERE r.a = r.a
        |""".stripMargin
    )

  private def prepareLocalIntegralSource(
      sql: String,
      planner: CalciteSqlPlanner = localIntegralPlanner
  ): PreparedQuery = {
    val planned = CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)
    val report: AnalysisReport = CalciteQueryAnalyzer.analyze(planned)
    assert(report.isSupported, clues(sql, report.diagnostics))
    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    PostgreSqlRendererTestSupport.prepareQuery(
      report,
      hypergraph,
      widthBound = 1,
      requireConnectedCover = true
    )
  }

  private def depthBranchPrepared(): PreparedQuery = {
    val base = prepareTopOne(
      depthBranchSql,
      depthBranchTables,
      widthBound = 1,
      requireConnectedCover = true
    )
    val decomposition = coveredDecomposition(
      root = 10,
      bags = Vector(
        coveredBag(10, Vector(1), Vector(1)),
        coveredBag(20, Vector(0, 1), Vector(1)),
        coveredBag(30, Vector(1, 2), Vector(2)),
        coveredBag(40, Vector(2, 3), Vector(3)),
        coveredBag(50, Vector(2, 4), Vector(5))
      ),
      edges = Vector(
        treeEdge(10, 20),
        treeEdge(10, 30),
        treeEdge(30, 40),
        treeEdge(30, 50)
      )
    )
    withDecomposition(base.report, base.hypergraph, base.input, decomposition)
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
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        identity
      )

  private val triangleTables = Vector(
    TestTable(
      "triangle_r",
      Vector(
        TestColumn("row_id"),
        TestColumn("x", nullable = true),
        TestColumn("y", nullable = true),
        TestColumn("a", nullable = true),
        TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
      ),
      Some("pg_temp")
    ),
    TestTable(
      "triangle_s",
      Vector(
        TestColumn("row_id"),
        TestColumn("y", nullable = true),
        TestColumn("z", nullable = true),
        TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
      ),
      Some("pg_temp")
    ),
    TestTable(
      "triangle_t",
      Vector(
        TestColumn("row_id"),
        TestColumn("z", nullable = true),
        TestColumn("x", nullable = true),
        TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
      ),
      Some("pg_temp")
    )
  )

  private val triangleSql =
    """SELECT
      |  "r"."x" AS "x",
      |  "r"."y" AS "y",
      |  "s"."z" AS "z",
      |  "r"."payload" AS "r_payload",
      |  "s"."payload" AS "s_payload",
      |  "t"."payload" AS "t_payload"
      |FROM "pg_temp"."triangle_r" AS "r"
      |JOIN "pg_temp"."triangle_s" AS "s" ON "r"."y" = "s"."y"
      |JOIN "pg_temp"."triangle_t" AS "t"
      |  ON "s"."z" = "t"."z" AND "t"."x" = "r"."x"
      |""".stripMargin

  private val chainTables = Vector(
    TestTable("chain_l", Vector(TestColumn("a", nullable = true), TestColumn("payload", SqlTypeName.VARCHAR, true))),
    TestTable("chain_x", Vector(TestColumn("a", nullable = true), TestColumn("q", nullable = true), TestColumn("payload", SqlTypeName.VARCHAR, true))),
    TestTable("chain_y", Vector(TestColumn("q", nullable = true), TestColumn("b", nullable = true), TestColumn("payload", SqlTypeName.VARCHAR, true))),
    TestTable("chain_r", Vector(TestColumn("b", nullable = true), TestColumn("payload", SqlTypeName.VARCHAR, true)))
  )

  private val chainSql =
    """SELECT l.payload AS "l_payload", r.payload AS "r_payload"
      |FROM chain_l AS l
      |JOIN chain_x AS x ON l.a = x.a
      |JOIN chain_y AS y ON x.q = y.q
      |JOIN chain_r AS r ON y.b = r.b
      |""".stripMargin

  private val depthBranchTables = Vector(
    TestTable("branch_l", Vector(TestColumn("a", nullable = true))),
    TestTable("branch_x", Vector(TestColumn("a", nullable = true), TestColumn("b", nullable = true))),
    TestTable("branch_y", Vector(TestColumn("b", nullable = true), TestColumn("c", nullable = true))),
    TestTable("branch_d", Vector(TestColumn("c", nullable = true), TestColumn("d", nullable = true))),
    TestTable("branch_dl", Vector(TestColumn("d", nullable = true))),
    TestTable("branch_e", Vector(TestColumn("c", nullable = true), TestColumn("e", nullable = true))),
    TestTable("branch_el", Vector(TestColumn("e", nullable = true)))
  )

  private val depthBranchSql =
    """SELECT l.a AS "a", dl.d AS "d", el.e AS "e"
      |FROM branch_l AS l
      |JOIN branch_x AS x ON l.a = x.a
      |JOIN branch_y AS y ON x.b = y.b
      |JOIN branch_d AS d ON y.c = d.c
      |JOIN branch_dl AS dl ON d.d = dl.d
      |JOIN branch_e AS e ON y.c = e.c
      |JOIN branch_el AS el ON e.e = el.e
      |""".stripMargin

  private def replacePredicate(
      query: QueryIR,
      ordinal: Int,
      transform: PredicateDescriptor => PredicateDescriptor
  ): QueryIR = {
    def rewrite(values: Vector[PredicateDescriptor]): Vector[PredicateDescriptor] =
      values.map(value => if (value.ordinal == ordinal) transform(value) else value)

    def visit(relational: RelationalIR): RelationalIR = relational match {
      case scan: TableScanIR => scan
      case filter: FilterIR => filter.copy(input = visit(filter.input), predicates = rewrite(filter.predicates))
      case join: JoinIR =>
        join.copy(
          left = visit(join.left),
          right = visit(join.right),
          predicates = rewrite(join.predicates)
        )
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

    query.copy(root = visit(query.root), predicates = rewrite(query.predicates))
  }

  private def replaceRelations(
      query: QueryIR
  )(transform: RelationOccurrence => RelationOccurrence): QueryIR = {
    val replacements = query.relations.map(relation => relation.id -> transform(relation)).toMap
    def visit(relational: RelationalIR): RelationalIR = relational match {
      case scan: TableScanIR => TableScanIR(replacements(scan.relation.id))
      case filter: FilterIR => filter.copy(input = visit(filter.input))
      case join: JoinIR => join.copy(left = visit(join.left), right = visit(join.right))
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

  private def replaceProject(
      query: QueryIR,
      ordinal: Int,
      transform: ProjectItem => ProjectItem
  ): QueryIR = query.root match {
    case project: ProjectIR =>
      query.copy(
        root = project.copy(
          projects = project.projects.map(value =>
            if (value.ordinal == ordinal) transform(value) else value
          )
        )
      )
    case other => throw new AssertionError(s"expected project root, found $other")
  }

  private def renderingLimits(
      scans: Long,
      terms: Long,
      ctes: Long,
      characters: Long
  ): PostgreSqlRenderingLimits =
    PostgreSqlRenderingLimits
      .create(scans, terms, ctes, characters)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )

  private def errors(
      result: Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery]
  ): Vector[PostgreSqlRenderingError] =
    result.swap.toOption.getOrElse(fail("expected PostgreSQL rendering failure"))

  private def validated[A](value: Either[ValidationError, A]): A =
    value.fold(error => throw new AssertionError(error.message), identity)

  private def decompositionValue[A](
      value: Either[Vector[DecompositionValidationError], A]
  ): A = value.fold(
    errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
    identity
  )

  private def variable(value: Int): VariableId = validated(VariableId.from(value))
  private def edgeId(value: Int): EdgeId = validated(EdgeId.from(value))
  private def bagId(value: Int): BagId = validated(BagId.from(value))

  private def coveredBag(
      id: Int,
      vertices: Vector[Int],
      cover: Vector[Int]
  ): CoveredBag =
    decompositionValue(
      CoveredBag.create(bagId(id), vertices.map(variable), cover.map(edgeId))
    )

  private def treeEdge(left: Int, right: Int): BagTreeEdge =
    decompositionValue(BagTreeEdge.create(bagId(left), bagId(right)))

  private def coveredDecomposition(
      root: Int,
      bags: Vector[CoveredBag],
      edges: Vector[BagTreeEdge]
  ): CoveredDecomposition =
    decompositionValue(CoveredDecomposition.create(bagId(root), bags, edges))

  private def count(value: String, fragment: String): Int = {
    var total = 0
    var offset = 0
    var next = value.indexOf(fragment, offset)
    while (next >= 0) {
      total += 1
      offset = next + fragment.length
      next = value.indexOf(fragment, offset)
    }
    total
  }

  private def finalSelectSql(sql: String): String = {
    val marker = "\nSELECT\n"
    val offset = sql.lastIndexOf(marker)
    if (offset < 0) throw new AssertionError(s"missing final SELECT in SQL:\n$sql")
    sql.substring(offset + 1)
  }

  private def finalProjectionSql(sql: String): String = {
    val finalSelect = finalSelectSql(sql)
    val fromOffset = finalSelect.indexOf("\nFROM ")
    if (fromOffset < 0)
      throw new AssertionError(s"missing final FROM in SQL:\n$finalSelect")
    finalSelect.substring(0, fromOffset)
  }

  private def finalEqualitySql(
      prepared: PreparedQuery,
      left: ColumnRef,
      right: ColumnRef
  ): String =
    s"(${finalColumnSql(prepared, left)} = ${finalColumnSql(prepared, right)})"

  private def finalColumnSql(
      prepared: PreparedQuery,
      ref: ColumnRef
  ): String = {
    val edge = prepared.hypergraph.edges.find(
      _.relationOccurrenceId == ref.relationOccurrenceId
    ).getOrElse(
      throw new AssertionError(s"missing hyperedge for column ${ref.id}")
    )
    val owner = prepared.plan.ownership.find(_.edgeId == edge.id).map(_.owner)
      .getOrElse(
        throw new AssertionError(s"missing owner for edge ${edge.id.value}")
      )
    val factorAlias = PostgreSqlNames.finalFactor(owner)
    val columnAlias = PostgreSqlNames.occurrenceColumn(edge.id, ref.id)
    s"\"$factorAlias\".\"$columnAlias\""
  }

  private def ownedNormalizationEqualities(
      prepared: PreparedQuery,
      factor: BagFactor
  ): Vector[String] =
    ownedBindingGroups(prepared, factor).flatMap { case (_, locations) =>
      locations.headOption.toVector.flatMap(representative =>
        locations.map(location => s"$representative = $location")
      )
    }

  private def ownedBindingGroups(
      prepared: PreparedQuery,
      factor: BagFactor
  ): Vector[(VariableId, Vector[String])] = {
    val edgeById = prepared.hypergraph.edges.map(edge => edge.id -> edge).toMap
    val relationById = prepared.query.relations.map(relation => relation.id -> relation).toMap
    factor.ownedEdgeIds
      .flatMap(edgeById.get)
      .flatMap { edge =>
        val relation = relationById(edge.relationOccurrenceId)
        edge.bindings.map { case (columnId, variableId) =>
          val sourceName = relation.columns.find(_.ref.id == columnId).get.sourceName
          val location =
            s"${quotedIdentifier(PostgreSqlNames.factorScan(factor.bagId, edge.id))}." +
              quotedIdentifier(sourceName)
          (variableId, edge.id, columnId, location)
        }
      }
      .groupBy(_._1)
      .toVector
      .sortBy(_._1)
      .map { case (variableId, values) =>
        variableId -> values
          .sortBy(value => (value._2.value, value._3))
          .map(_._4)
      }
  }

  private def quotedIdentifier(value: String): String =
    s"\"${value.replace("\"", "\"\"")}\""
}
