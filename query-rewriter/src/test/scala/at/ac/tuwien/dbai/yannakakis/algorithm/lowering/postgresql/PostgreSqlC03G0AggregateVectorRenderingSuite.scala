package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanDecision,
  CandidateSearchInput,
  TopKCandidateTD
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlanner,
  BagFactorPlanningLimits
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.Hyperedge
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  C03G0CatalogBinding,
  CalciteQueryAnalyzer,
  CalciteSqlPlanner,
  QueryResultShape
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.{AbstractSchema, AbstractTable}
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

class PostgreSqlC03G0AggregateVectorRenderingSuite extends munit.FunSuite {
  private val physicalSchemaName = "c03_rendering"

  private val catalogBinding = C03G0CatalogBinding(
    databaseName = "c03_rendering_database",
    databaseOid = 11L,
    physicalSchemaName = physicalSchemaName,
    relationName = "synthetic_triangle_e0",
    groupColumnName = "payload",
    measureColumnName = "row_id",
    serverMajorVersion = 18,
    namespaceOid = 12L,
    relationOid = 13L,
    groupAttnum = 4,
    groupTypeOid = 25L,
    measureAttnum = 1,
    measureTypeOid = 20L,
    groupCollationOid = 100L,
    groupCollationName = "C",
    groupCollationProvider = "c",
    groupCollationDeterministic = true
  )

  private val planner = {
    val root = Frameworks.createRootSchema(true)
    val schema = root.add(physicalSchemaName, new AbstractSchema {})
    Vector(
      "synthetic_triangle_e0",
      "synthetic_triangle_e1",
      "synthetic_triangle_e2"
    ).foreach { name =>
      schema.add(
        name,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType =
            factory
              .builder()
              .add("row_id", SqlTypeName.BIGINT)
              .nullable(false)
              .add("left_key", SqlTypeName.BIGINT)
              .nullable(true)
              .add("right_key", SqlTypeName.BIGINT)
              .nullable(true)
              .add("payload", SqlTypeName.VARCHAR)
              .nullable(true)
              .build()
        }
      )
    }
    CalciteSqlPlanner
      .forSchemaWithC03G0V2(
        schema,
        catalogBinding,
        CalciteSqlPlanner.PostgreSqlParserConfig
      )
      .fold(problem => fail(problem.toString), identity)
  }

  // Exact frozen starter-corpus bytes, including the final LF.
  private val c03Sql = """/*
 * C03: independently authored guarded triangle grouping with multiple
 * aggregate calls. The current rewriter language does not support this shape;
 * it is retained as an explicit coverage target and must not be timed as a
 * rewritten query until a later language contract and live differential gate
 * both succeed.
 */
SELECT
  e0.payload AS guard_group,
  COUNT(*) AS witness_count,
  COUNT(e0.payload) AS nonnull_guard_count,
  MIN(e0.row_id) AS min_guard_row_id,
  MAX(e0.row_id) AS max_guard_row_id
FROM synthetic_triangle_e0 AS e0
JOIN synthetic_triangle_e1 AS e1
  ON e0.right_key = e1.left_key
JOIN synthetic_triangle_e2 AS e2
  ON e1.right_key = e2.left_key
 AND e2.right_key = e0.left_key
GROUP BY e0.payload
"""

  test("one- and multi-bag C03-G0 SQL stays owner-local and raw-factor based") {
    val candidates = preparedCandidates()
    val oneBag = candidates
      .find(_.plan.factors.size == 1)
      .getOrElse(
        fail(s"C03 candidate prefix has no one-bag plan: ${candidateSizes(candidates)}")
      )
    val multiBag = candidates
      .find(_.plan.factors.size > 1)
      .getOrElse(
        fail(s"C03 candidate prefix has no multi-bag plan: ${candidateSizes(candidates)}")
      )

    val fixtures = Vector(compile(oneBag), compile(multiBag))
    fixtures.foreach { fixture =>
      val terminal = c03Terminal(fixture.compiled)
      val propagation = terminal.propagationPlan
      val expectedKey =
        PostgreSqlAggregatePropagation.AggregateKey.OccurrenceValue(
          terminal.groupColumn.ref.id
        )

      assertEquals(propagation.root, terminal.aggregateBagId)
      assertEquals(propagation.finalization.groupKeys, Vector(expectedKey))
      assertEquals(
        propagation.keyBindings.map(binding => binding.key -> binding.owner),
        Vector(expectedKey -> terminal.aggregateBagId)
      )
      assertEquals(propagation.laneBindings, Vector.empty)
      assert(propagation.messages.forall(_.carriedKeys.isEmpty))
      assert(!terminal.usesStagedCountFrequency(fixture.compiled.plan))
      assert(fixture.compiled.reductionCtes.forall(!_.finalizesCountFrequency))
      assertEquals(
        fixture.compiled.bags
          .flatMap(_.ownedOccurrences.flatMap(_.carriedColumns.map(_.ref.id)))
          .toSet,
        Set(terminal.groupColumn.ref.id, terminal.measureColumn.ref.id)
      )
      assertEquals(
        occurrences(fixture.rendered.sql, "CAST(COUNT(*) AS NUMERIC)"),
        propagation.bags.size,
        clues(fixture.rendered.sql)
      )
      assert(
        fixture.rendered.sql.contains(
          s"AS \"${PostgreSqlNames.C03LocalMinimumColumn}\""
        ),
        clues(fixture.rendered.sql)
      )
      assert(
        fixture.rendered.sql.contains(
          s"AS \"${PostgreSqlNames.C03LocalMaximumColumn}\""
        ),
        clues(fixture.rendered.sql)
      )
      assert(
        fixture.rendered.sql.contains("IS NULL THEN CAST(0 AS BIGINT) ELSE"),
        clues(fixture.rendered.sql)
      )
      assertEquals(
        occurrences(fixture.rendered.sql, "CAST(COALESCE(SUM("),
        2,
        clues(fixture.rendered.sql)
      )
      assert(!fixture.rendered.sql.contains("AS BIGINT) AS \"yr_freq\""))
      assertEquals(
        fixture.rendered.output.map(column =>
          (column.ordinal, column.name, column.dataType, column.nullable)
        ),
        Vector(
          (0, "guard_group", "VARCHAR", true),
          (1, "witness_count", "BIGINT NOT NULL", false),
          (2, "nonnull_guard_count", "BIGINT NOT NULL", false),
          (3, "min_guard_row_id", "BIGINT NOT NULL", false),
          (4, "max_guard_row_id", "BIGINT NOT NULL", false)
        )
      )
      assertEquals(
        fixture.rendered.trace.stages.last.logicalBagIds,
        terminal.inputBagIds(fixture.compiled.plan)
      )
      assertEquals(
        fixture.rendered.statistics.renderedTermCount,
        exactRenderedTerms(fixture.compiled)
      )
      assertEquals(
        fixture.rendered.statistics.sqlCharacterCount,
        fixture.rendered.sql.length.toLong
      )
    }

    assertEquals(c03Terminal(fixtures.head.compiled).propagationPlan.messages.size, 0)
    assert(c03Terminal(fixtures(1).compiled).propagationPlan.messages.nonEmpty)
    assert(fixtures(1).rendered.sql.contains("\"yr_cm_b"))
  }

  test("C03-G0 reduction and exact rendered-term boundaries fail closed") {
    val prepared = preparedCandidates().find(_.plan.factors.size > 1).getOrElse(
      fail("C03 candidate prefix has no multi-bag plan")
    )
    Vector(
      YannakakisReductionMode.BottomUpOnly,
      YannakakisReductionMode.NoReduction
    ).foreach { mode =>
      val problems = PostgreSqlBagFactorRenderer
        .compileAndRender(
          prepared.query,
          prepared.input,
          prepared.plan,
          PostgreSqlRenderingLimits.default,
          BagFactorPlanningLimits.default,
          mode
        )
        .fold(identity, value => fail(s"C03 rendered under ${mode.id}: $value"))
      assert(
        problems.exists {
          case PostgreSqlRenderingError.C03G0RequiresFullTwoPass(`mode`) => true
          case _                                                        => false
        },
        clues(problems)
      )
    }

    val fixture = compile(prepared)
    val exact = limitsWithRenderedTerms(
      fixture.rendered.statistics.renderedTermCount
    )
    assert(
      PostgreSqlBagFactorRenderer
        .compileAndRender(
          prepared.query,
          prepared.input,
          prepared.plan,
          exact,
          BagFactorPlanningLimits.default,
          YannakakisReductionMode.FullTwoPass
        )
        .isRight
    )
    val below = limitsWithRenderedTerms(
      fixture.rendered.statistics.renderedTermCount - 1
    )
    val belowProblems = PostgreSqlBagFactorRenderer
      .compileAndRender(
        prepared.query,
        prepared.input,
        prepared.plan,
        below,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass
      )
      .fold(identity, value => fail(s"below-exact C03 rendered: $value"))
    assert(
      belowProblems.exists {
        case PostgreSqlRenderingError.ResourceLimitExceeded(
              PostgreSqlRenderingResource.RenderedTerms,
              required,
              limit
            ) =>
          required == BigInt(fixture.rendered.statistics.renderedTermCount) &&
            limit == fixture.rendered.statistics.renderedTermCount - 1
        case _ => false
      },
      clues(belowProblems)
    )
  }

  test("development profiles collapse one bag and compact multi-bag C03 state") {
    val candidates = preparedCandidates()
    val oneBag = candidates.find(_.plan.factors.size == 1).getOrElse(
      fail("C03 candidate prefix has no one-bag plan")
    )
    val multiBag = candidates.find(_.plan.factors.size > 1).getOrElse(
      fail("C03 candidate prefix has no multi-bag plan")
    )

    val collapsedProfile = profile(
      PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2,
      PostgreSqlAggregateReductionFinalizationProfile.LegacyFullVerticesV1,
      PostgreSqlReductionSupportProfile.DistinctSeparatorSupportV1
    )
    val legacyOneBag = compile(oneBag)
    assertEquals(
      compile(oneBag, PostgreSqlAggregateRenderingProfile.LegacyV1).rendered,
      legacyOneBag.rendered
    )
    val collapsed = compile(oneBag, collapsedProfile)
    assertEquals(collapsed.compiled.reductionCtes, Vector.empty)
    assertEquals(collapsed.rendered.output, legacyOneBag.rendered.output)
    assert(!collapsed.rendered.sql.contains("CAST(COALESCE(SUM("))
    assert(!collapsed.rendered.sql.contains(PostgreSqlNames.C03LocalMinimumColumn))
    assert(!collapsed.rendered.sql.contains(PostgreSqlNames.C03LocalMaximumColumn))
    assertEquals(occurrences(collapsed.rendered.sql, "COUNT(*)"), 1)
    assertEquals(occurrences(collapsed.rendered.sql, "COUNT("), 2)
    assertEquals(occurrences(collapsed.rendered.sql, "MIN("), 1)
    assertEquals(occurrences(collapsed.rendered.sql, "MAX("), 1)
    assert(
      collapsed.rendered.statistics.renderedTermCount <
        legacyOneBag.rendered.statistics.renderedTermCount
    )

    val compactProfile = profile(
      PostgreSqlAggregateOneBagTerminalProfile.LegacyNestedV1,
      PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2,
      PostgreSqlReductionSupportProfile.DirectExistsV2
    )
    val legacyMultiBag = compile(multiBag)
    assertEquals(
      compile(multiBag, PostgreSqlAggregateRenderingProfile.LegacyV1).rendered,
      legacyMultiBag.rendered
    )
    val compact = compile(multiBag, compactProfile)
    val finalizations = compact.compiled.reductionCtes
      .map(_.finalization)
      .filterNot(_ == CompiledPostgreSqlReductionFinalization.RetainRawRows)
    assertEquals(finalizations.size, compact.compiled.plan.factors.size)
    assertEquals(
      finalizations.count(
        _.isInstanceOf[CompiledPostgreSqlReductionFinalization.CompactC03Owner]
      ),
      1
    )
    assertEquals(
      finalizations.count(
        _.isInstanceOf[CompiledPostgreSqlReductionFinalization.CompactFrequency]
      ),
      compact.compiled.plan.factors.size - 1
    )
    assert(!compact.rendered.sql.contains("SELECT DISTINCT\n          \"yr_cd_"))
    assertEquals(
      occurrences(
        compact.rendered.sql,
        s"AS \"${PostgreSqlNames.C03LocalMinimumColumn}\""
      ),
      1
    )
    assertEquals(
      occurrences(
        compact.rendered.sql,
        s"AS \"${PostgreSqlNames.C03LocalMaximumColumn}\""
      ),
      1
    )
    assertEquals(compact.rendered.output, legacyMultiBag.rendered.output)
    assertNotEquals(compact.rendered.sql, legacyMultiBag.rendered.sql)
    assertEquals(
      compact.rendered.statistics.sqlCharacterCount,
      compact.rendered.sql.length.toLong
    )
  }

  test("emitter rejects corrupted C03 plans, liveness, and structural payloads") {
    val fixture = compile(
      preparedCandidates().find(_.plan.factors.size > 1).getOrElse(
        fail("C03 candidate prefix has no multi-bag plan")
      )
    )
    val terminal = c03Terminal(fixture.compiled)

    val corruptedResources = fixture.compiled.copy(
      terminal = terminal.copy(
        propagationPlan = terminal.propagationPlan.copy(
          resources = terminal.propagationPlan.resources.copy(
            terminalGroupingTerms = BigInt(0)
          )
        )
      )
    )
    intercept[IllegalArgumentException] {
      PostgreSqlEmitter.characterCount(corruptedResources)
    }

    val missingMeasureLiveness = fixture.compiled.copy(
      ownerByCarriedColumn =
        fixture.compiled.ownerByCarriedColumn - terminal.measureColumn.ref.id
    )
    intercept[IllegalArgumentException] {
      PostgreSqlEmitter.characterCount(missingMeasureLiveness)
    }

    val guardEdge = fixture.compiled.edgeByOccurrence(
      terminal.groupColumn.ref.relationOccurrenceId
    )
    val structuralVariable = guardEdge.bindings.head._2
    val structurallyBoundGroup = Hyperedge
      .create(
        guardEdge.id,
        guardEdge.relationOccurrenceId,
        guardEdge.bindings :+ (terminal.groupColumn.ref.id -> structuralVariable)
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    val hostileStructuralPayload = fixture.compiled.copy(
      edgeByOccurrence = fixture.compiled.edgeByOccurrence.updated(
        guardEdge.relationOccurrenceId,
        structurallyBoundGroup
      )
    )
    intercept[IllegalArgumentException] {
      PostgreSqlEmitter.characterCount(hostileStructuralPayload)
    }

    val firstMessage = terminal.propagationPlan.messages.head
    val groupKey = PostgreSqlAggregatePropagation.AggregateKey.OccurrenceValue(
      terminal.groupColumn.ref.id
    )
    val corruptedMessages = terminal.propagationPlan.messages.updated(
      firstMessage.ordinal,
      firstMessage.copy(carriedKeys = Vector(groupKey))
    )
    intercept[IllegalArgumentException] {
      terminal.copy(
        propagationPlan = terminal.propagationPlan.copy(
          messages = corruptedMessages
        )
      )
    }
  }

  private final class CompiledFixture(
      val compiled: CompiledPostgreSqlRendering,
      val rendered: PostgreSqlRenderedQuery
  )

  private def preparedCandidates()
      : Vector[PostgreSqlRendererTestSupport.PreparedQuery] = {
    val report = analyzed()
    val hypergraph = report.hypergraph.fold(
      problems => fail(problems.mkString("; ")),
      identity
    )
    val generated = SoftBagGenerator
      .generate(hypergraph, widthBound = 2)
      .fold(problem => fail(problem.message), identity)
    val input = CandidateSearchInput
      .create(
        hypergraph,
        widthBound = 2,
        generated.admissible(requireConnectedCover = true)
      )
      .fold(
        problems => fail(problems.map(_.message).mkString("; ")),
        identity
      )
    val decompositions = TopKCandidateTD
      .enumerate(input, planLimit = 100)
      .fold(
        problem => fail(problem.message),
        {
          case found: CandidatePlanDecision.Found =>
            found.plans.map(_.decomposition)
          case rejected: CandidatePlanDecision.Rejected =>
            fail(s"C03 candidate search rejected: ${rejected.statistics}")
        }
      )
    decompositions.map { decomposition =>
      val plan = BagFactorPlanner
        .plan(input, decomposition)
        .fold(
          problems => fail(problems.map(_.message).mkString("; ")),
          identity
        )
      PostgreSqlRendererTestSupport.PreparedQuery(
        report,
        report.query,
        hypergraph,
        input,
        decomposition,
        plan
      )
    }
  }

  private def analyzed(): AnalysisReport = {
    val planned = CalciteSqlPlanner
      .planWithSourceEvidence(planner, c03Sql)
      .fold(problem => fail(problem.toString), identity)
    val report = CalciteQueryAnalyzer.analyze(planned)
    assert(
      report.isSupported,
      clues(planned.sourceEvidence, report.diagnostics)
    )
    assert(
      report.query.resultShape
        .isInstanceOf[QueryResultShape.C03G0AggregateVector]
    )
    report
  }

  private def compile(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery
  ): CompiledFixture =
    compile(prepared, PostgreSqlAggregateRenderingProfile.LegacyV1)

  private def compile(
      prepared: PostgreSqlRendererTestSupport.PreparedQuery,
      aggregateRenderingProfile: PostgreSqlAggregateRenderingProfile
  ): CompiledFixture = {
    val (compiled, rendered) = PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfile(
        prepared.query,
        prepared.input,
        prepared.plan,
        PostgreSqlRenderingLimits.default,
        BagFactorPlanningLimits.default,
        YannakakisReductionMode.FullTwoPass,
        aggregateRenderingProfile
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    new CompiledFixture(compiled, rendered)
  }

  private def profile(
      oneBagTerminal: PostgreSqlAggregateOneBagTerminalProfile,
      reductionFinalization: PostgreSqlAggregateReductionFinalizationProfile,
      reductionSupport: PostgreSqlReductionSupportProfile
  ): PostgreSqlAggregateRenderingProfile =
    PostgreSqlAggregateRenderingProfile
      .create(oneBagTerminal, reductionFinalization, reductionSupport)
      .fold(
        problems => fail(problems.map(_.code).mkString("; ")),
        identity
      )

  private def c03Terminal(
      compiled: CompiledPostgreSqlRendering
  ): CompiledPostgreSqlTerminal.C03G0AggregateVector = compiled.terminal match {
    case value: CompiledPostgreSqlTerminal.C03G0AggregateVector => value
    case other => fail(s"expected C03-G0 terminal, found $other")
  }

  private def candidateSizes(
      candidates: Vector[PostgreSqlRendererTestSupport.PreparedQuery]
  ): Vector[Int] = candidates.map(_.plan.factors.size)

  private def occurrences(haystack: String, needle: String): Int = {
    require(needle.nonEmpty, "needle must not be empty")
    var count = 0
    var from = 0
    var next = haystack.indexOf(needle, from)
    while (next >= 0) {
      count += 1
      from = next + needle.length
      next = haystack.indexOf(needle, from)
    }
    count
  }

  private def limitsWithRenderedTerms(value: Long): PostgreSqlRenderingLimits =
    PostgreSqlRenderingLimits
      .create(
        PostgreSqlRenderingLimits.default.maxExpandedScans,
        value,
        PostgreSqlRenderingLimits.default.maxCtes,
        PostgreSqlRenderingLimits.default.maxSqlCharacters
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def exactRenderedTerms(compiled: CompiledPostgreSqlRendering): Long = {
    var terms = BigInt(compiled.terminal.renderedTermCount)
    compiled.bags.foreach { bag =>
      bag.supportStrategy match {
        case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
          terms += bag.factor.vertices.size + bag.supportEqualities.size
          bag.supportEdges.foreach(edge =>
            terms += compiled.localPredicatesByOccurrence
              .getOrElse(edge.relationOccurrenceId, Vector.empty)
              .size
          )
          terms += bag.factor.vertices.size
          bag.ownedOccurrences.foreach { occurrence =>
            terms += occurrence.edge.bindings.size
            terms += occurrence.carriedColumns.size
            terms += compiled.localPredicatesByOccurrence
              .getOrElse(occurrence.relation.id, Vector.empty)
              .size
          }
        case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
          terms += bag.factor.vertices.size + bag.ownedEqualities.size
          bag.ownedOccurrences.foreach { occurrence =>
            terms += occurrence.carriedColumns.size
            terms += compiled.localPredicatesByOccurrence
              .getOrElse(occurrence.relation.id, Vector.empty)
              .size
          }
      }
    }
    compiled.reductionCtes.foreach { reduction =>
      assert(!reduction.finalizesCountFrequency)
      terms += 1
      reduction.members.foreach(member =>
        terms += 1 + member.step.separator.size
      )
    }
    val propagation = c03Terminal(compiled).propagationPlan
    terms += propagation.resources.additionalRenderedTerms + 2
    assert(terms.isValidLong, clues(terms))
    terms.toLong
  }
}
