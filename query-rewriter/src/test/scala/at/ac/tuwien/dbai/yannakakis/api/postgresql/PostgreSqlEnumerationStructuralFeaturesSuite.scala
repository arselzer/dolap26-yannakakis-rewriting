package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets

import scala.collection.mutable

import at.ac.tuwien.dbai.yannakakis.calcite.{
  CalcitePlannerFixture,
  CalciteTestSchemas
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanner

class PostgreSqlEnumerationStructuralFeaturesSuite extends munit.FunSuite {
  private val triangleSql =
    """SELECT r.a AS x, r.b, s.c
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.b = s.b
      |JOIN triangle_t AS t ON s.c = t.c AND t.a = r.a""".stripMargin

  test("registry is closed, alphabetic, typed, and outcome-free") {
    val registry = PostgreSqlEnumerationStructuralFeatures.Registry
    val expectedNames = Vector(
      "candidate.bag_count",
      "candidate.enumeration_occurrence_count",
      "candidate.maximum_bag_size",
      "candidate.maximum_branching_factor",
      "candidate.maximum_separator_size",
      "candidate.semijoin_step_count",
      "candidate.tree_depth",
      "hypergraph.edge_count",
      "hypergraph.maximum_edge_arity",
      "hypergraph.vertex_count",
      "query.distinct_join_variable_count",
      "query.join_predicate_count",
      "query.relation_occurrence_count",
      "render.maximum_stage_dependency_depth",
      "render.maximum_statement_bytes",
      "render.statement_count",
      "render.total_sql_bytes",
      "search.exhausted_before_k",
      "search.requested_limit",
      "search.retained_count",
      "search.width_bound"
    )

    assertEquals(
      PostgreSqlEnumerationStructuralFeatures.RegistryVersion,
      "postgresql_enumeration_structural_features/v1"
    )
    assertEquals(registry.map(_.name), expectedNames)
    assertEquals(registry.map(_.name), registry.map(_.name).sorted)
    assertEquals(registry.map(_.name).distinct.size, registry.size)
    assert(registry.forall(_.definition.nonEmpty))
    assert(registry.forall(_.source.id.nonEmpty))
    assert(registry.forall(_.scope.id.nonEmpty))
    assert(registry.forall(_.valueType.id.nonEmpty))
    assert(registry.forall(_.unit.id.nonEmpty))
    assert(registry.forall(_.absenceRule.id.nonEmpty))
    assert(!registry.exists(_.name == "candidate.rank"))
    assert(!registry.exists(_.name == "hypergraph.conjunctive_query_width"))

    val forbidden = Set(
      "cardinality",
      "duration",
      "id",
      "latency",
      "outcome",
      "rank",
      "runtime",
      "speedup",
      "timing",
      "topology",
      "winner"
    )
    assert(
      registry.forall(spec =>
        spec.name.split("[._-]").forall(token => !forbidden.contains(token))
      )
    )
    assert(
      registry.filter(
        _.scope == PostgreSqlEnumerationFeatureScope.Workload
      ).forall(
        _.absenceRule ==
          PostgreSqlEnumerationFeatureAbsenceRule.Required
      )
    )
    assert(
      registry.filter(
        _.scope == PostgreSqlEnumerationFeatureScope.Action
      ).forall(
        _.absenceRule ==
          PostgreSqlEnumerationFeatureAbsenceRule.MayBeNotApplicable
      )
    )
  }

  test("export covers native and every retained candidate without leaking action metadata") {
    val artifact = produce(triangleSql, planLimit = 10)
    val exported = PostgreSqlEnumerationStructuralFeatures.export(artifact)

    assertEquals(exported.registry, PostgreSqlEnumerationStructuralFeatures.Registry)
    assertEquals(exported.actions.size, artifact.batch.candidates.size + 1)
    assertEquals(
      exported.actions.head.action,
      PostgreSqlEnumerationFeatureAction.Native
    )
    val candidateActions = exported.actions.tail.map(_.action).map {
      case action: PostgreSqlEnumerationFeatureAction.RetainedCandidate =>
        action
      case other => fail(s"unexpected feature action: $other")
    }
    assertEquals(
      candidateActions.map(_.structuralRank),
      artifact.batch.candidates.map(_.plan.rank)
    )
    assertEquals(
      candidateActions.map(_.candidateId),
      artifact.batch.candidates.map(_.plan.candidateId)
    )
    assertEquals(
      candidateActions.map(_.physicalVariantId),
      artifact.batch.candidates.map(_.physicalVariantId)
    )

    val workloadIndices = exported.registry.indices.filter(index =>
      exported.registry(index).scope ==
        PostgreSqlEnumerationFeatureScope.Workload
    )
    workloadIndices.foreach { index =>
      val native = exported.actions.head.values(index)
      assertEquals(native.state, PostgreSqlEnumerationFeatureState.Observed)
      exported.actions.tail.foreach(row =>
        assertEquals(row.values(index), native)
      )
    }

    val actionIndices = exported.registry.indices.filter(index =>
      exported.registry(index).scope == PostgreSqlEnumerationFeatureScope.Action
    )
    actionIndices.foreach { index =>
      val native = exported.actions.head.values(index)
      assertEquals(
        native.state,
        PostgreSqlEnumerationFeatureState.NotApplicable
      )
      assertEquals(native.value, None)
      exported.actions.tail.foreach { row =>
        assertEquals(
          row.values(index).state,
          PostgreSqlEnumerationFeatureState.Observed
        )
        assert(row.values(index).value.nonEmpty)
      }
    }

    val native = valuesByName(exported.actions.head)
    assertEquals(integer(native("hypergraph.edge_count")), BigInt(3))
    assertEquals(integer(native("hypergraph.maximum_edge_arity")), BigInt(2))
    assertEquals(integer(native("hypergraph.vertex_count")), BigInt(3))
    assertEquals(
      integer(native("query.distinct_join_variable_count")),
      BigInt(3)
    )
    assertEquals(integer(native("query.join_predicate_count")), BigInt(3))
    assertEquals(integer(native("query.relation_occurrence_count")), BigInt(3))
    assertEquals(integer(native("search.requested_limit")), BigInt(10))
    assertEquals(integer(native("search.retained_count")), BigInt(4))
    assertEquals(integer(native("search.width_bound")), BigInt(2))
    assertEquals(boolean(native("search.exhausted_before_k")), true)
  }

  test("render bytes and iterative tree and stage depths are exact and deterministic") {
    val nonAsciiSql = triangleSql.replace("AS x", "AS \"é\"")
    val artifact = produce(nonAsciiSql, planLimit = 10)
    val first = artifact.batch.candidates.head
    val left = PostgreSqlEnumerationStructuralFeatures.export(artifact)
    val right = PostgreSqlEnumerationStructuralFeatures.export(artifact)
    assertEquals(left, right)

    val row = valuesByName(left.actions(1))
    val byteCounts = first.stageDiagnostics.statements.map(statement =>
      BigInt(statement.sql.getBytes(StandardCharsets.UTF_8).length)
    )
    val characterCounts = first.stageDiagnostics.statements.map(statement =>
      BigInt(statement.sql.length)
    )
    assertEquals(
      integer(row("render.statement_count")),
      BigInt(first.stageDiagnostics.statements.size)
    )
    assertEquals(integer(row("render.total_sql_bytes")), byteCounts.sum)
    assertEquals(integer(row("render.maximum_statement_bytes")), byteCounts.max)
    assert(byteCounts.sum > characterCounts.sum)

    val plan = first.bagFactorPlan
    assertEquals(
      integer(row("candidate.tree_depth")),
      plan.factors.iterator.map(factor => BigInt(factor.depth)).max
    )
    assertEquals(
      integer(row("candidate.maximum_branching_factor")),
      plan.factors.iterator.map(factor => BigInt(factor.children.size)).max
    )
    assertEquals(
      integer(row("candidate.maximum_separator_size")),
      plan.arcs.iterator.map(arc => BigInt(arc.separator.size)).maxOption
        .getOrElse(BigInt(0))
    )
    assertEquals(
      integer(row("render.maximum_stage_dependency_depth")),
      emittedStageDepth(first)
    )
    left.actions.tail.foreach { action =>
      action.values.foreach { feature =>
        feature.value.foreach {
          case value: PostgreSqlEnumerationObservedFeatureValue.IntegerValue =>
            assert(value.value >= 0)
            assertEquals(value.canonicalScalar, value.value.toString)
          case _: PostgreSqlEnumerationObservedFeatureValue.BooleanValue => ()
        }
      }
    }
  }

  test("explicit states and exact integers reject sentinel and type confusion") {
    assertEquals(
      Vector(
        PostgreSqlEnumerationFeatureState.Observed,
        PostgreSqlEnumerationFeatureState.NotApplicable,
        PostgreSqlEnumerationFeatureState.MissingOptional,
        PostgreSqlEnumerationFeatureState.CaptureFailed
      ).map(_.id),
      Vector(
        "observed",
        "not_applicable",
        "missing_optional",
        "capture_failed"
      )
    )

    val integerSpec = PostgreSqlEnumerationStructuralFeatures.Registry.find(
      _.name == "search.retained_count"
    ).getOrElse(fail("integer feature is missing"))
    val huge = BigInt(2).pow(256) + 17
    val exact = PostgreSqlEnumerationFeatureValue.observed(
      integerSpec,
      PostgreSqlEnumerationObservedFeatureValue.IntegerValue(huge)
    )
    assertEquals(exact.canonicalScalar, Some(huge.toString))

    intercept[IllegalArgumentException] {
      PostgreSqlEnumerationFeatureValue.observed(
        integerSpec,
        PostgreSqlEnumerationObservedFeatureValue.BooleanValue(value = true)
      )
    }
    intercept[IllegalArgumentException] {
      PostgreSqlEnumerationFeatureValue.notApplicable(integerSpec)
    }
    intercept[IllegalArgumentException] {
      PostgreSqlEnumerationObservedFeatureValue.IntegerValue(BigInt(-1))
    }
  }

  private def produce(
      sql: String,
      planLimit: Int
  ): PostgreSqlEnumerationBatchArtifact = {
    val fixture = CalcitePlannerFixture(CalciteTestSchemas.standard.tables)
    val planner = CalciteSqlPlanner
      .fromFunction(source => Right(fixture.plan(source).root))
      .fold(problem => fail(problem.message), identity)
    val rewriter = PostgreSqlQueryRewriter
      .create(planner)
      .fold(problem => fail(problem.message), identity)
    val request = PostgreSqlEnumerationBatchRequest
      .create(sql, planLimit = planLimit)
      .fold(problems => fail(problems.map(_.code).mkString("; ")), identity)
    val result = rewriter.produceEnumerationBatch(request)
    result.artifact.getOrElse(
      fail(s"enumeration feature fixture failed: ${result.diagnostics}")
    )
  }

  private def valuesByName(
      row: PostgreSqlEnumerationFeatureActionRow
  ): Map[String, PostgreSqlEnumerationFeatureValue] =
    row.values.iterator.map(value => value.spec.name -> value).toMap

  private def integer(value: PostgreSqlEnumerationFeatureValue): BigInt =
    value.value match {
      case Some(PostgreSqlEnumerationObservedFeatureValue.IntegerValue(number)) =>
        number
      case other => fail(s"expected observed integer feature, received $other")
    }

  private def boolean(value: PostgreSqlEnumerationFeatureValue): Boolean =
    value.value match {
      case Some(PostgreSqlEnumerationObservedFeatureValue.BooleanValue(flag)) =>
        flag
      case other => fail(s"expected observed boolean feature, received $other")
    }

  private def emittedStageDepth(
      candidate: PostgreSqlCompiledRetainedCandidate
  ): BigInt = {
    val depth = mutable.Map.empty[
      at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId,
      BigInt
    ]
    var maximum = BigInt(0)
    candidate.renderedQuery.trace.stages
      .filter(_.physicalForm.emitted)
      .foreach { stage =>
        val current = stage.inputStageIds
          .map(input => depth(input))
          .maxOption
          .fold(BigInt(0))(_ + 1)
        depth.put(stage.id, current)
        maximum = maximum.max(current)
      }
    maximum
  }
}
