package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactor,
  BagFactorPlan,
  EdgeOwnership,
  OrientedBagArc
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningOracleTestSupport._
import at.ac.tuwien.dbai.yannakakis.core.{BagId, ColumnId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.ColumnRef

class PostgreSqlAggregateNestingLimitSuite extends munit.FunSuite {
  private val limit =
    PostgreSqlRenderingLimits.MaximumAggregateNestingDepth.toInt

  test("scalar, one-key, and distributed frequency paths enforce depth 64") {
    assertEquals(limit, 64)

    val scalarAtLimit = frequency(chainPlan(limit))
    val scalarPastLimit = frequency(chainPlan(limit + 1))
    assertEquals(scalarAtLimit.maximumMessageNestingDepth, limit)
    assertEquals(scalarPastLimit.maximumMessageNestingDepth, limit + 1)
    assertEquals(nestingError(scalarAtLimit.maximumMessageNestingDepth), None)
    assertEquals(
      nestingError(scalarPastLimit.maximumMessageNestingDepth),
      Some(expectedLimitError(limit + 1))
    )

    val groupedAtLimit = groupedFrequency(chainPlan(limit), limit)
    val groupedPastLimit = groupedFrequency(chainPlan(limit + 1), limit + 1)
    assertEquals(groupedAtLimit.maximumMessageNestingDepth, limit)
    assertEquals(groupedPastLimit.maximumMessageNestingDepth, limit + 1)
    assertEquals(nestingError(groupedAtLimit.maximumMessageNestingDepth), None)
    assertEquals(
      nestingError(groupedPastLimit.maximumMessageNestingDepth),
      Some(expectedLimitError(limit + 1))
    )

    val distributedAtLimit = distributedFrequency(chainPlan(limit), limit)
    val distributedPastLimit =
      distributedFrequency(chainPlan(limit + 1), limit + 1)
    assertEquals(distributedAtLimit.maximumMessageNestingDepth, limit)
    assertEquals(distributedPastLimit.maximumMessageNestingDepth, limit + 1)
    assertEquals(
      nestingError(distributedAtLimit.maximumMessageNestingDepth),
      None
    )
    assertEquals(
      nestingError(distributedPastLimit.maximumMessageNestingDepth),
      Some(expectedLimitError(limit + 1))
    )
  }

  test("guarded staged supports admit depth 64 and fail closed at depth 65") {
    val atLimit = guardedPathPlan(edgeCount = limit + 2)
    val accepted = PostgreSqlGuardedExtremumSemijoin
      .deriveWithMaximumNestingDepth(
        atLimit,
        guardColumn(atLimit),
        bagId(0),
        limit
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    assertEquals(accepted.maximumStageNestingDepth, limit)

    val pastLimit = guardedPathPlan(edgeCount = limit + 3)
    val problems = PostgreSqlGuardedExtremumSemijoin
      .deriveWithMaximumNestingDepth(
        pastLimit,
        guardColumn(pastLimit),
        bagId(0),
        limit
      )
      .fold(identity, value => fail(s"depth-65 plan was accepted: $value"))
    assertEquals(problems.size, 1)
    problems.head.code match {
      case PostgreSqlGuardedExtremumSemijoin.ErrorCode
            .NestedStageDepthExceeded(required, actualLimit) =>
        assertEquals(required, limit + 1)
        assertEquals(actualLimit, limit)
        assertEquals(
          nestingError(required),
          Some(expectedLimitError(limit + 1))
        )
      case other => fail(s"unexpected guarded-depth error: $other")
    }
  }

  test("C03 occurrence-key propagation admits depth 64 and fails at 65") {
    val atLimit = c03Frequency(chainPlan(limit))
    val pastLimit = c03Frequency(chainPlan(limit + 1))

    assertEquals(atLimit.maximumMessageNestingDepth, limit)
    assertEquals(pastLimit.maximumMessageNestingDepth, limit + 1)
    assertEquals(atLimit.laneBindings, Vector.empty)
    assert(atLimit.messages.forall(_.carriedKeys.isEmpty))
    assertEquals(nestingError(atLimit.maximumMessageNestingDepth), None)
    assertEquals(
      nestingError(pastLimit.maximumMessageNestingDepth),
      Some(expectedLimitError(limit + 1))
    )
  }

  test("20k-bag chain is stack-safe and returns one typed depth error") {
    val plan = frequency(chainPlan(messageDepth = 19999))
    assertEquals(plan.bags.size, 20000)
    assertEquals(plan.maximumMessageNestingDepth, 19999)
    assertEquals(
      nestingError(plan.maximumMessageNestingDepth),
      Some(expectedLimitError(19999))
    )
  }

  test("20k-bag star is bounded by path depth rather than message count") {
    val plan = frequency(starPlan(bagCount = 20000))
    assertEquals(plan.bags.size, 20000)
    assertEquals(plan.messages.size, 19999)
    assertEquals(plan.maximumMessageNestingDepth, 1)
    assertEquals(nestingError(plan.maximumMessageNestingDepth), None)
  }

  test("20k guarded-stage chain fails typed without recursive derivation") {
    val source = guardedPathPlan(edgeCount = 20002)
    val problems = PostgreSqlGuardedExtremumSemijoin
      .deriveWithMaximumNestingDepth(
        source,
        guardColumn(source),
        bagId(0),
        limit
      )
      .fold(identity, value => fail(s"20k-stage plan was accepted: $value"))

    assertEquals(problems.size, 1)
    problems.head.code match {
      case PostgreSqlGuardedExtremumSemijoin.ErrorCode
            .NestedStageDepthExceeded(required, actualLimit) =>
        assertEquals(required, 20000)
        assertEquals(actualLimit, limit)
        assertEquals(nestingError(required), Some(expectedLimitError(20000)))
      case other => fail(s"unexpected guarded-depth error: $other")
    }
  }

  test("fixed engine ceiling does not change serialized caller limits") {
    val recreated = PostgreSqlRenderingLimits
      .create(
        maxExpandedScans = PostgreSqlRenderingLimits.default.maxExpandedScans,
        maxRenderedTerms = PostgreSqlRenderingLimits.default.maxRenderedTerms,
        maxCtes = PostgreSqlRenderingLimits.default.maxCtes,
        maxSqlCharacters = PostgreSqlRenderingLimits.default.maxSqlCharacters
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

    assertEquals(recreated, PostgreSqlRenderingLimits.default)
    assertEquals(
      recreated.toString,
      "PostgreSqlRenderingLimits(1000000,10000000,1000000,16777216)"
    )
  }

  private def nestingError(
      required: Int
  ): Option[PostgreSqlRenderingError.ResourceLimitExceeded] =
    PostgreSqlRenderingCompiler.aggregateNestingError(required)

  private def expectedLimitError(
      required: Int
  ): PostgreSqlRenderingError.ResourceLimitExceeded =
    PostgreSqlRenderingError.ResourceLimitExceeded(
      PostgreSqlRenderingResource.AggregateNestingDepth,
      BigInt(required),
      limit.toLong
    )

  private def frequency(
      source: BagFactorPlan
  ): PostgreSqlFrequencyPropagation.Plan =
    PostgreSqlFrequencyPropagation
      .derive(source)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def groupedFrequency(
      source: BagFactorPlan,
      finalizationBag: Int
  ): PostgreSqlFrequencyPropagation.Plan =
    PostgreSqlFrequencyPropagation
      .deriveGrouped(
        source,
        bagId(finalizationBag),
        variable(finalizationBag + 1)
      )
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def distributedFrequency(
      source: BagFactorPlan,
      lastVariable: Int
  ): PostgreSqlAggregatePropagation.Plan = {
    val request = PostgreSqlAggregatePropagation.AggregateRequest
      .countStarGroupedBy(
        PostgreSqlAggregatePropagation.AggregateKey.Structural(variable(0)),
        PostgreSqlAggregatePropagation.AggregateKey.Structural(
          variable(lastVariable + 1)
        )
      )
    val root = PostgreSqlAggregatePropagation
      .defaultFinalizationRoot(source, request)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    PostgreSqlAggregatePropagation
      .derive(source, root, request)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
  }

  private def c03Frequency(
      source: BagFactorPlan
  ): PostgreSqlAggregatePropagation.Plan = {
    val guardEdge = source.sourceHypergraph.edges.head
    val groupColumn = ColumnId
      .from(guardEdge.relationOccurrenceId, ordinal = 99)
      .fold(problem => fail(problem.message), identity)
    assert(!guardEdge.columns.contains(groupColumn))
    val request = PostgreSqlAggregatePropagation.AggregateRequest
      .countStarGroupedBy(
        PostgreSqlAggregatePropagation.AggregateKey.OccurrenceValue(groupColumn)
      )
    val root = PostgreSqlAggregatePropagation
      .defaultFinalizationRoot(source, request)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
    PostgreSqlAggregatePropagation
      .derive(source, root, request)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)
  }

  /**
    * Frequency propagation validates only its structural tree boundary. These
    * large synthetic plans deliberately reuse unrelated proof metadata from a
    * genuine seed plan so the stress gate remains small and deterministic.
    */
  private def chainPlan(messageDepth: Int): BagFactorPlan = {
    require(messageDepth >= 0)
    val factors = (0 to messageDepth).iterator.map { index =>
      BagFactor(
        bagId(index),
        Vector(variable(index), variable(index + 1)),
        Vector(edgeId(0)),
        Vector.empty,
        depth = index,
        parent = if (index == 0) None else Some(bagId(index - 1)),
        children =
          if (index == messageDepth) Vector.empty
          else Vector(bagId(index + 1))
      )
    }.toVector
    val arcs = (0 until messageDepth).iterator.map { index =>
      OrientedBagArc(
        bagId(index),
        bagId(index + 1),
        Vector(variable(index + 1))
      )
    }.toVector
    seedPlan.copy(root = bagId(0), factors = factors, arcs = arcs)
  }

  private def starPlan(bagCount: Int): BagFactorPlan = {
    require(bagCount >= 1)
    val children = (1 until bagCount).iterator.map(bagId).toVector
    val factors = BagFactor(
      bagId(0),
      Vector(variable(0)),
      Vector(edgeId(0)),
      Vector.empty,
      depth = 0,
      parent = None,
      children = children
    ) +: children.map { child =>
      BagFactor(
        child,
        Vector(variable(0)),
        Vector(edgeId(0)),
        Vector.empty,
        depth = 1,
        parent = Some(bagId(0)),
        children = Vector.empty
      )
    }
    val arcs = children.map(child =>
      OrientedBagArc(bagId(0), child, Vector(variable(0)))
    )
    seedPlan.copy(root = bagId(0), factors = factors, arcs = arcs)
  }

  private def guardedPathPlan(edgeCount: Int): BagFactorPlan = {
    require(edgeCount >= 2)
    val hypergraph = graph(
      (0 until edgeCount).map(index => Seq(index, index + 1))
    )
    val bags = (0 until edgeCount).map { index =>
      coveredBag(index, Seq(index, index + 1), Seq(index))
    }
    val treeEdges = (0 until edgeCount - 1).map(index =>
      treeEdge(index, index + 1)
    )
    val decomposition = coveredDecomposition(0, bags, treeEdges)
    val factors = (0 until edgeCount).map { index =>
      BagFactor(
        bagId(index),
        Vector(variable(index), variable(index + 1)),
        Vector(edgeId(index)),
        Vector(edgeId(index)),
        depth = index,
        parent = if (index == 0) None else Some(bagId(index - 1)),
        children =
          if (index + 1 == edgeCount) Vector.empty
          else Vector(bagId(index + 1))
      )
    }.toVector
    val ownership = (0 until edgeCount).map { index =>
      EdgeOwnership(edgeId(index), bagId(index), ownerDepth = index)
    }.toVector
    val arcs = (0 until edgeCount - 1).map { index =>
      OrientedBagArc(
        bagId(index),
        bagId(index + 1),
        Vector(variable(index + 1))
      )
    }.toVector
    seedPlan.copy(
      sourceHypergraph = hypergraph,
      decomposition = decomposition,
      root = bagId(0),
      factors = factors,
      ownership = ownership,
      arcs = arcs
    )
  }

  private def guardColumn(plan: BagFactorPlan): ColumnRef =
    ColumnRef(plan.sourceHypergraph.edges.head.bindings.head._1)

  private lazy val seedPlan: BagFactorPlan = {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2)))
    val input = candidateInput(
      hypergraph,
      widthBound = 1,
      requireConnectedCover = true
    )
    val decomposition = coveredDecomposition(
      root = 0,
      bags = Vector(
        coveredBag(0, Vector(0, 1), Vector(0)),
        coveredBag(1, Vector(1, 2), Vector(1))
      ),
      edges = Vector(treeEdge(0, 1))
    )
    certificate(input, decomposition)
    planned(input, decomposition)
  }
}
