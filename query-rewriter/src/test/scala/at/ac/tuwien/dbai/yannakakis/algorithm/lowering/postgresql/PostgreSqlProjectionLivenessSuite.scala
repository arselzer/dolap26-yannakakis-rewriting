package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.calcite.{TestColumn, TestTable}
import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  ColumnId,
  EdgeId,
  Hyperedge,
  RelationOccurrenceId,
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
  DistributedGroupedCountStarIR,
  FilterIR,
  GlobalCountStarIR,
  GlobalGuardedExtremumIR,
  GroupedCountStarIR,
  JoinIR,
  PredicateClassification,
  ProjectIR,
  QueryIR,
  RelationOccurrence,
  RelationalIR,
  TableScanIR,
  UnsupportedRelationalIR
}
import org.apache.calcite.sql.`type`.SqlTypeName

import scala.collection.mutable

import PostgreSqlRendererTestSupport._

private[postgresql] final case class ProjectionExpectedResources(
    scans: Long,
    terms: Long,
    ctes: Long
)

/**
  * Independent projection-liveness oracles. These tests intentionally derive
  * demand from QueryIR and the hypergraph instead of reusing compiler helpers.
  */
class PostgreSqlProjectionLivenessSuite extends munit.FunSuite {
  test(
    "QueryIR demand is exact for elided and materialized support with sparse ordinals"
  ) {
    val elided = sparseTrianglePrepared()
    val materialized = sparseTriangleMaterialized(elided)

    Vector(elided, materialized).foreach(assertExactLiveness)

    val elidedRendered = render(elided)
    val materializedRendered = render(materialized)
    assertEquals(
      elidedRendered.trace.bags.map(_.supportStrategy),
      Vector(PostgreSqlBagSupportStrategy.ElidedByOwnedCover)
    )
    assert(
      materializedRendered.trace.bags.exists(
        _.supportStrategy ==
          PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
      ),
      clues(materializedRendered.trace.bags)
    )

    Vector(elided -> elidedRendered, materialized -> materializedRendered)
      .foreach { case (prepared, rendered) =>
        assertCarriedAliasShape(prepared, rendered)
        assert(rendered.sql.contains(".\"local_left\""), clues(rendered.sql))
        assert(rendered.sql.contains(".\"local_right\""), clues(rendered.sql))
        assert(!rendered.sql.contains("\"yr_e0_c4\""), clues(rendered.sql))
        assert(!rendered.sql.contains("\"yr_e0_c5\""), clues(rendered.sql))
        assertEquals(
          rendered.output.map(_.name),
          Vector("same", "duplicate", "duplicate")
        )
        assertEquals(
          rendered.output.map(output => output.dataType -> output.nullable),
          Vector(
            "BOOLEAN" -> true,
            "VARCHAR" -> true,
            "VARCHAR" -> true
          )
        )
      }

    val rEdge = elided.hypergraph.edges.head
    val rOccurrence = elided.query.relations.head
    val compiled = compile(elided)
    val rLiveness = compiled.bags.flatMap(_.ownedOccurrences)
      .find(_.edge.id == rEdge.id)
      .getOrElse(fail("missing R occurrence liveness"))
    assertEquals(
      rLiveness.scanColumns.map(_.ref.ordinal),
      Vector(1, 3, 4, 5, 6)
    )
    assertEquals(rLiveness.carriedColumns.map(_.ref.ordinal), Vector(1, 3, 6))
    assertEquals(rOccurrence.columns.map(_.ref.ordinal), (0 to 6).toVector)
  }

  test(
    "self joins, repeated bindings, nullable equality output, and duplicate aliases retain identities"
  ) {
    val selfJoin = selfJoinPrepared()
    val repeated = repeatedBindingPrepared()
    Vector(selfJoin, repeated).foreach(assertExactLiveness)

    val selfRendered = render(selfJoin)
    assertCarriedAliasShape(selfJoin, selfRendered)
    assertEquals(
      selfJoin.hypergraph.edges.map(_.relationOccurrenceId).distinct.size,
      2
    )
    assertEquals(selfRendered.output.map(_.name), Vector("duplicate", "duplicate", "nullable_equal"))
    assertEquals(
      selfRendered.output.last.dataType -> selfRendered.output.last.nullable,
      "BOOLEAN" -> true
    )
    assert(selfRendered.sql.contains(".\"local_left\""), clues(selfRendered.sql))
    assert(selfRendered.sql.contains(".\"local_right\""), clues(selfRendered.sql))
    assert(!selfRendered.sql.contains("\"yr_e0_c2\""), clues(selfRendered.sql))
    assert(!selfRendered.sql.contains("\"yr_e0_c3\""), clues(selfRendered.sql))
    assert(!selfRendered.sql.contains("\"yr_e0_c0\""), clues(selfRendered.sql))
    assert(!selfRendered.sql.contains("\"yr_e1_c0\""), clues(selfRendered.sql))

    val repeatedEdge = repeated.hypergraph.edges.head
    assertEquals(repeatedEdge.bindings.map(_._1.ordinal), Vector(1, 2))
    assertEquals(repeatedEdge.bindings.map(_._2).distinct.size, 1)
    val repeatedRendered = render(repeated)
    assertCarriedAliasShape(repeated, repeatedRendered)
    assert(!repeatedRendered.sql.contains("\"yr_e0_c0\""), clues(repeatedRendered.sql))
    assert(!repeatedRendered.sql.contains("\"yr_e0_c3\""), clues(repeatedRendered.sql))
    assert(!repeatedRendered.sql.contains("\"yr_e1_c0\""), clues(repeatedRendered.sql))
    assert(!repeatedRendered.sql.contains("\"yr_e1_c2\""), clues(repeatedRendered.sql))
  }

  test("support-only bags carry nothing and every semijoin consumes the latest version") {
    val prepared = depthBranchPrepared()
    val compiled = assertExactLiveness(prepared)
    val rendered = render(prepared)

    val supportOnly = prepared.plan.factors.find(_.ownedEdgeIds.isEmpty)
      .getOrElse(fail("fixture lost its support-only bag"))
    val compiledSupportOnly = compiled.bags.find(_.factor.bagId == supportOnly.bagId)
      .getOrElse(fail("compiled support-only bag is missing"))
    assertEquals(compiledSupportOnly.ownedOccurrences, Vector.empty)
    val supportOnlyTrace = rendered.trace.bags.find(_.bagId == supportOnly.bagId)
      .getOrElse(fail("support-only trace is missing"))
    assertEquals(supportOnlyTrace.occurrenceColumns, Vector.empty)
    assertEquals(
      supportOnlyTrace.supportStrategy,
      PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
    )

    val current = mutable.Map.empty[BagId, String]
    rendered.trace.bags.foreach(trace =>
      current.put(trace.bagId, trace.initialFactorCte)
    )
    var traceOffset = 0
    while (traceOffset < rendered.trace.semijoins.size) {
      val first = rendered.trace.semijoins(traceOffset)
      val group = rendered.trace.semijoins
        .drop(traceOffset)
        .takeWhile(_.outputCte == first.outputCte)
      assert(group.nonEmpty)
      assert(group.forall(_.pass == first.pass))
      assert(group.forall(_.target == first.target))
      group.foreach { step =>
        assertEquals(step.targetInputCte, current(step.target))
        assertEquals(step.sourceInputCte, current(step.source))
      }
      current.update(first.target, first.outputCte)
      traceOffset += group.size
    }
    rendered.trace.bags.foreach(trace =>
      assertEquals(trace.finalFactorCte, current(trace.bagId))
    )

    prepared.hypergraph.edges.foreach { edge =>
      assert(!rendered.sql.contains(
        quoted(PostgreSqlNames.occurrenceColumn(edge.id, columnId(edge.relationOccurrenceId, 0)))
      ), clues(edge.id, rendered.sql))
    }
  }

  test("malformed and duplicate identifiers fail closed even when their columns are dead") {
    val prepared = sparseTrianglePrepared()
    val deadColumn = prepared.query.relations.head.columns.head
    assert(!expectedCarriedIds(prepared, prepared.hypergraph.edges.head)(deadColumn.ref.id))

    val malformed = replaceRelations(prepared.query) { relation =>
      if (relation.id == deadColumn.ref.relationOccurrenceId)
        relation.copy(
          columns = relation.columns.updated(
            deadColumn.ref.ordinal,
            deadColumn.copy(sourceName = "\u0000dead")
          )
        )
      else relation
    }
    val malformedErrors = renderErrors(prepared, malformed)
    assert(malformedErrors.exists {
      case PostgreSqlRenderingError.InvalidIdentifier(path, _, _) =>
        path == s"relation ${deadColumn.ref.relationOccurrenceId.value} column ${deadColumn.ref.ordinal}"
      case _ => false
    }, clues(malformedErrors))

    val duplicate = replaceRelations(prepared.query) { relation =>
      if (relation.id == deadColumn.ref.relationOccurrenceId)
        relation.copy(
          columns = relation.columns.zipWithIndex.map { case (metadata, ordinal) =>
            if (ordinal == 0 || ordinal == 2)
              metadata.copy(sourceName = "dead_duplicate")
            else metadata
          }
        )
      else relation
    }
    val duplicateErrors = renderErrors(prepared, duplicate)
    assert(duplicateErrors.exists {
      case PostgreSqlRenderingError.DuplicateSourceColumn(
            occurrenceId,
            "dead_duplicate",
            ordinals
          ) =>
        occurrenceId == deadColumn.ref.relationOccurrenceId &&
          ordinals == Vector(0, 2)
      case _ => false
    }, clues(duplicateErrors))
  }

  test("independent resource counts are exact at and one below every boundary") {
    val elided = sparseTrianglePrepared()
    val materialized = sparseTriangleMaterialized(elided)

    Vector(elided, materialized).foreach { prepared =>
      val baseline = render(prepared)
      val expected = expectedResources(prepared)
      assertEquals(baseline.statistics.expandedScanCount, expected.scans)
      assertEquals(baseline.statistics.renderedTermCount, expected.terms)
      assertEquals(baseline.statistics.cteCount, expected.ctes)
      assertEquals(baseline.statistics.sqlCharacterCount, baseline.sql.length.toLong)

      val exact = renderingLimits(
        expected.scans,
        expected.terms,
        expected.ctes,
        baseline.sql.length.toLong
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

      val values = Vector(
        PostgreSqlRenderingResource.ExpandedScans -> expected.scans,
        PostgreSqlRenderingResource.RenderedTerms -> expected.terms,
        PostgreSqlRenderingResource.Ctes -> expected.ctes,
        PostgreSqlRenderingResource.SqlCharacters -> baseline.sql.length.toLong
      )
      values.filter(_._2 > 1L).foreach { case (resource, required) =>
        val limits = renderingLimits(
          if (resource == PostgreSqlRenderingResource.ExpandedScans) required - 1L
          else expected.scans,
          if (resource == PostgreSqlRenderingResource.RenderedTerms) required - 1L
          else expected.terms,
          if (resource == PostgreSqlRenderingResource.Ctes) required - 1L
          else expected.ctes,
          if (resource == PostgreSqlRenderingResource.SqlCharacters) required - 1L
          else baseline.sql.length.toLong
        )
        val failures = PostgreSqlBagFactorRenderer
          .render(prepared.query, prepared.input, prepared.plan, limits)
          .swap
          .toOption
          .getOrElse(fail(s"expected $resource limit failure"))
        assertEquals(
          failures,
          Vector(
            PostgreSqlRenderingError.ResourceLimitExceeded(
              resource,
              BigInt(required),
              required - 1L
            )
          )
        )
      }
    }
  }

  private def assertExactLiveness(
      prepared: PreparedQuery
  ): CompiledPostgreSqlRendering = {
    val compiled = compile(prepared)
    val actualByEdge = compiled.bags.flatMap(_.ownedOccurrences)
      .map(value => value.edge.id -> value)
      .toMap
    assertEquals(actualByEdge.keySet, prepared.hypergraph.edgeIds.toSet)

    val innerEqualityRefs = prepared.query.predicates.iterator.collect {
      case predicate
          if predicate.classification
            .isInstanceOf[PredicateClassification.InnerEquality] =>
        predicate.expression.columnRefs
    }.flatten.map(_.id).toSet
    assertEquals(innerEqualityRefs, prepared.hypergraph.columns.toSet)

    prepared.hypergraph.edges.foreach { edge =>
      val actual = actualByEdge(edge.id)
      assertEquals(
        actual.carriedColumns.map(_.ref.id),
        expectedCarriedIds(prepared, edge).toVector.sorted,
        s"carried edge ${edge.id.value}"
      )
      assertEquals(
        actual.scanColumns.map(_.ref.id),
        expectedScanIds(prepared, edge).toVector.sorted,
        s"scan edge ${edge.id.value}"
      )
      assertEquals(actual.edge, edge)
      assertEquals(actual.relation.id, edge.relationOccurrenceId)
    }

    val traceByEdge = compiled.trace.bags.flatMap(_.occurrenceColumns)
      .groupBy(_.edgeId)
      .view
      .mapValues(_.map(_.columnId))
      .toMap
    prepared.hypergraph.edges.foreach { edge =>
      assertEquals(
        traceByEdge.getOrElse(edge.id, Vector.empty),
        expectedCarriedIds(prepared, edge).toVector.sorted
      )
    }

    val ownerByEdge = prepared.plan.ownership.map(value => value.edgeId -> value.owner).toMap
    val expectedOwners = prepared.hypergraph.edges.flatMap { edge =>
      expectedCarriedIds(prepared, edge).toVector.map(_ -> ownerByEdge(edge.id))
    }.toMap
    assertEquals(compiled.ownerByCarriedColumn, expectedOwners)
    compiled
  }

  private def expectedCarriedIds(
      prepared: PreparedQuery,
      edge: Hyperedge
  ): Set[ColumnId] = {
    val structural = edge.bindings.iterator.map(_._1)
    val output = prepared.query.output.iterator.flatMap(_.expression.columnRefs.iterator)
    val reEmittedInner = prepared.query.predicates.iterator.collect {
      case predicate
          if predicate.classification
            .isInstanceOf[PredicateClassification.InnerEquality] =>
        predicate.expression
    }.flatMap(_.columnRefs.iterator)
    (structural ++ output.map(_.id) ++ reEmittedInner.map(_.id))
      .filter(_.relationOccurrenceId == edge.relationOccurrenceId)
      .toSet
  }

  private def expectedScanIds(
      prepared: PreparedQuery,
      edge: Hyperedge
  ): Set[ColumnId] = {
    val local = prepared.query.predicates.iterator.collect {
      case predicate
          if predicate.classification ==
            PredicateClassification.LocalFilter(edge.relationOccurrenceId) =>
        predicate.expression
    }.flatMap(_.columnRefs.iterator).map(_.id)
    expectedCarriedIds(prepared, edge) ++ local
  }

  private def assertCarriedAliasShape(
      prepared: PreparedQuery,
      rendered: PostgreSqlRenderedQuery
  ): Unit = prepared.hypergraph.edges.foreach { edge =>
    val expected = expectedCarriedIds(prepared, edge)
    val relation = prepared.query.relations.find(
      _.id == edge.relationOccurrenceId
    ).getOrElse(fail(s"missing relation for edge ${edge.id.value}"))
    relation.columns.foreach { metadata =>
      val alias = quoted(PostgreSqlNames.occurrenceColumn(edge.id, metadata.ref.id))
      if (expected(metadata.ref.id))
        assert(rendered.sql.contains(alias), clues(edge.id, metadata.ref.id, rendered.sql))
      else
        assert(!rendered.sql.contains(alias), clues(edge.id, metadata.ref.id, rendered.sql))
    }
  }

  private def expectedResources(
      prepared: PreparedQuery
  ): ProjectionExpectedResources = {
    val edgeById = prepared.hypergraph.edges.map(edge => edge.id -> edge).toMap
    val localCounts = prepared.query.predicates.collect {
      case predicate
          if predicate.classification
            .isInstanceOf[PredicateClassification.LocalFilter] =>
        predicate.classification match {
          case PredicateClassification.LocalFilter(occurrenceId) => occurrenceId
          case _ => throw new AssertionError("guarded local-filter match changed")
        }
    }.groupBy(identity).view.mapValues(_.size.toLong).toMap

    var scans = 0L
    var terms = prepared.query.output.size.toLong
    var ctes = 0L
    prepared.plan.factors.foreach { factor =>
      val supportEdges = factor.supportCover.map(edgeById)
      val ownedEdges = factor.ownedEdgeIds.map(edgeById)
      val elided = factor.supportCover.forall(factor.ownedEdgeIds.toSet)
      if (elided) {
        scans += ownedEdges.size
        ctes += 1L
        terms += factor.vertices.size
        terms += ownedEdges.iterator.map(_.bindings.size).sum
        ownedEdges.foreach { edge =>
          terms += expectedCarriedIds(prepared, edge).size
          terms += localCounts.getOrElse(edge.relationOccurrenceId, 0L)
        }
      } else {
        scans += supportEdges.size + ownedEdges.size
        ctes += 2L
        terms += factor.vertices.size
        val supportBindingCounts = supportEdges.flatMap(_.bindings)
          .groupBy(_._2)
          .values
          .map(_.size)
        terms += supportBindingCounts.map(count => math.max(0, count - 1)).sum
        supportEdges.foreach(edge =>
          terms += localCounts.getOrElse(edge.relationOccurrenceId, 0L)
        )

        terms += factor.vertices.size
        ownedEdges.foreach { edge =>
          terms += edge.bindings.size
          terms += expectedCarriedIds(prepared, edge).size
          terms += localCounts.getOrElse(edge.relationOccurrenceId, 0L)
        }
      }
    }

    val logicalSteps =
      prepared.plan.schedule.bottomUp ++ prepared.plan.schedule.topDown
    val physicalReductionCount = logicalSteps.indices.count { index =>
      index == 0 || {
        val previous = logicalSteps(index - 1)
        val current = logicalSteps(index)
        previous.pass != current.pass || previous.target != current.target
      }
    }
    ctes += physicalReductionCount
    terms += physicalReductionCount
    logicalSteps.foreach(step => terms += 1L + step.separator.size)
    prepared.plan.arcs.foreach(arc => terms += arc.separator.size)
    // Every source inner equality in these exact fixtures is enforced by its
    // owner-factor binding(s) and, where owners differ, the reconstruction
    // separator path. None is therefore a rendered terminal term.
    ProjectionExpectedResources(scans, terms, ctes)
  }

  private def sparseTrianglePrepared(): PreparedQuery = prepareTopOne(
    sparseTriangleSql,
    sparseTriangleTables,
    widthBound = 2,
    requireConnectedCover = true
  )

  private def sparseTriangleMaterialized(base: PreparedQuery): PreparedQuery = {
    val decomposition = coveredDecomposition(
      root = 10,
      bags = Vector(
        coveredBag(10, Vector(0, 1), Vector(0)),
        coveredBag(20, Vector(0, 1, 2), Vector(0, 1))
      ),
      edges = Vector(treeEdge(10, 20))
    )
    withDecomposition(base.report, base.hypergraph, base.input, decomposition)
  }

  private def selfJoinPrepared(): PreparedQuery = prepareTopOne(
    """SELECT
      |  l.payload AS "duplicate",
      |  r.payload AS "duplicate",
      |  l.payload = r.payload AS "nullable_equal"
      |FROM liveness_self AS l
      |JOIN liveness_self AS r ON l.join_key = r.join_key
      |WHERE l.local_left = l.local_right
      |""".stripMargin,
    Vector(
      TestTable(
        "liveness_self",
        Vector(
          TestColumn("row_id"),
          TestColumn("join_key", nullable = true),
          TestColumn("local_left", nullable = true),
          TestColumn("local_right", nullable = true),
          TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
        )
      )
    ),
    widthBound = 1,
    requireConnectedCover = true
  )

  private def repeatedBindingPrepared(): PreparedQuery = prepareTopOne(
    """SELECT r.payload AS "payload"
      |FROM repeated_r AS r
      |JOIN repeated_s AS s ON r.a = s.x AND r.b = s.x
      |""".stripMargin,
    Vector(
      TestTable(
        "repeated_r",
        Vector(
          TestColumn("row_id"),
          TestColumn("a", nullable = true),
          TestColumn("b", nullable = true),
          TestColumn("unused"),
          TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
        )
      ),
      TestTable(
        "repeated_s",
        Vector(
          TestColumn("row_id"),
          TestColumn("x", nullable = true),
          TestColumn("unused")
        )
      )
    ),
    widthBound = 1,
    requireConnectedCover = true
  )

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

  private val sparseTriangleTables = Vector(
    TestTable(
      "sparse_r",
      Vector(
        TestColumn("row_id"),
        TestColumn("x", nullable = true),
        TestColumn("unused_gap"),
        TestColumn("y", nullable = true),
        TestColumn("local_left", nullable = true),
        TestColumn("local_right", nullable = true),
        TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
      )
    ),
    TestTable(
      "sparse_s",
      Vector(
        TestColumn("row_id"),
        TestColumn("y", nullable = true),
        TestColumn("unused_gap"),
        TestColumn("z", nullable = true),
        TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
      )
    ),
    TestTable(
      "sparse_t",
      Vector(
        TestColumn("row_id"),
        TestColumn("z", nullable = true),
        TestColumn("unused_gap"),
        TestColumn("x", nullable = true),
        TestColumn("payload", SqlTypeName.VARCHAR, nullable = true)
      )
    )
  )

  private val sparseTriangleSql =
    """SELECT
      |  r.payload = s.payload AS "same",
      |  r.payload AS "duplicate",
      |  t.payload AS "duplicate"
      |FROM sparse_r AS r
      |JOIN sparse_s AS s ON r.y = s.y
      |JOIN sparse_t AS t ON s.z = t.z AND t.x = r.x
      |WHERE r.local_left = r.local_right
      |""".stripMargin

  private val depthBranchTables = Vector(
    TestTable("branch_l", Vector(TestColumn("row_id"), TestColumn("a", nullable = true))),
    TestTable("branch_x", Vector(TestColumn("row_id"), TestColumn("a", nullable = true), TestColumn("b", nullable = true))),
    TestTable("branch_y", Vector(TestColumn("row_id"), TestColumn("b", nullable = true), TestColumn("c", nullable = true))),
    TestTable("branch_d", Vector(TestColumn("row_id"), TestColumn("c", nullable = true), TestColumn("d", nullable = true))),
    TestTable("branch_dl", Vector(TestColumn("row_id"), TestColumn("d", nullable = true))),
    TestTable("branch_e", Vector(TestColumn("row_id"), TestColumn("c", nullable = true), TestColumn("e", nullable = true))),
    TestTable("branch_el", Vector(TestColumn("row_id"), TestColumn("e", nullable = true)))
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

  private def compile(prepared: PreparedQuery): CompiledPostgreSqlRendering =
    PostgreSqlRenderingCompiler
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

  private def renderErrors(
      prepared: PreparedQuery,
      query: QueryIR
  ): Vector[PostgreSqlRenderingError] =
    PostgreSqlBagFactorRenderer
      .render(query, prepared.input, prepared.plan)
      .swap
      .toOption
      .getOrElse(fail("expected rendering failure"))

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

  private def renderingLimits(
      scans: Long,
      terms: Long,
      ctes: Long,
      characters: Long
  ): PostgreSqlRenderingLimits = PostgreSqlRenderingLimits
    .create(scans, terms, ctes, characters)
    .fold(
      problems => fail(problems.map(_.message).mkString("; ")),
      identity
    )

  private def id[A](value: Either[ValidationError, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def decomposition[A](
      value: Either[Vector[DecompositionValidationError], A]
  ): A = value.fold(
    problems => fail(problems.map(_.message).mkString("; ")),
    identity
  )

  private def variable(value: Int): VariableId = id(VariableId.from(value))
  private def edgeId(value: Int): EdgeId = id(EdgeId.from(value))
  private def bagId(value: Int): BagId = id(BagId.from(value))
  private def columnId(
      occurrenceId: RelationOccurrenceId,
      ordinal: Int
  ): ColumnId = id(ColumnId.from(occurrenceId, ordinal))

  private def coveredBag(
      value: Int,
      vertices: Vector[Int],
      cover: Vector[Int]
  ): CoveredBag = decomposition(
    CoveredBag.create(
      bagId(value),
      vertices.map(variable),
      cover.map(edgeId)
    )
  )

  private def treeEdge(left: Int, right: Int): BagTreeEdge =
    decomposition(BagTreeEdge.create(bagId(left), bagId(right)))

  private def coveredDecomposition(
      root: Int,
      bags: Vector[CoveredBag],
      edges: Vector[BagTreeEdge]
  ): CoveredDecomposition = decomposition(
    CoveredDecomposition.create(bagId(root), bags, edges)
  )

  private def quoted(value: String): String =
    s"\"${value.replace("\"", "\"\"")}\""
}
