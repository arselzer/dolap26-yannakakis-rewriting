package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactor,
  BagFactorPlan,
  OrientedBagArc
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningOracleTestSupport._
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlFrequencyPropagation.{
  Plan,
  SaturatingBigInt64
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}

import scala.collection.mutable

/**
  * SQL- and renderer-independent oracle for local factor preaggregation and
  * the future cutoff-semiring carrier.
  */
class PostgreSqlFrequencyLocalPreaggregationIsolatedSuite
    extends munit.FunSuite {
  private type SqlValue = Option[Int]
  private type MessageTable = Map[Vector[SqlValue], BigInt]

  private final case class WeightedRow(
      values: Map[VariableId, SqlValue],
      multiplicity: BigInt
  )

  test("local keys are the canonical union of every neighboring separator") {
    val plan = frequencyPlan(overlappingSeparatorPlan())

    assertEquals(
      plan.bags.map(bag => bag.bagId.value -> bag.localGrouping.map(_.value)),
      Vector(
        10 -> Vector(0, 1, 2),
        20 -> Vector(0, 1, 3),
        30 -> Vector(1, 2),
        40 -> Vector(1, 3)
      )
    )
    plan.bags.foreach { bag =>
      assertEquals(bag.localGrouping, bag.localGrouping.distinct.sorted)
      assert(bag.localGrouping.forall(bag.vertices.contains))
    }

    val resources = plan.resources
    assertEquals(resources.localAggregateTerms, BigInt(4))
    assertEquals(resources.localKeyProjectionTerms, BigInt(10))
    assertEquals(resources.localKeyGroupingTerms, BigInt(10))
    assertEquals(resources.propagatedMessageAggregateTerms, BigInt(1))
    assertEquals(resources.propagatedSeparatorProjectionTerms, BigInt(6))
    assertEquals(resources.propagatedSeparatorGroupingTerms, BigInt(2))
    assertEquals(resources.propagatedJoinEqualityTerms, BigInt(6))
    assertEquals(resources.additionalRenderedTerms, BigInt(39))
    assertEquals(
      plan.messages.map(message => message.source.value -> message.requiresRegrouping),
      Vector(40 -> false, 20 -> true, 30 -> false)
    )
  }

  test("local preaggregation preserves duplicate and NULL-sensitive count") {
    val plan = frequencyPlan(overlappingSeparatorPlan())
    val rows = Map(
      bagId(10) -> Vector(
        row(2)(0 -> Some(1), 1 -> Some(5), 2 -> Some(9)),
        row(3)(0 -> Some(1), 1 -> Some(5), 2 -> Some(9)),
        row(7)(0 -> Some(1), 1 -> None, 2 -> Some(9)),
        row(11)(0 -> Some(2), 1 -> Some(6), 2 -> Some(8))
      ),
      bagId(20) -> Vector(
        row(13)(0 -> Some(1), 1 -> Some(5), 3 -> Some(4)),
        row(17)(0 -> Some(1), 1 -> Some(5), 3 -> Some(4)),
        row(19)(0 -> Some(1), 1 -> None, 3 -> Some(4)),
        row(23)(0 -> Some(2), 1 -> Some(6), 3 -> Some(7))
      ),
      bagId(30) -> Vector(
        row(41)(1 -> Some(5), 2 -> Some(9)),
        row(43)(1 -> Some(5), 2 -> Some(9))
      ),
      bagId(40) -> Vector(
        row(29)(1 -> Some(5), 3 -> Some(4), 4 -> Some(0)),
        row(31)(1 -> Some(5), 3 -> Some(4), 4 -> Some(1)),
        row(37)(1 -> Some(6), 3 -> Some(7), 4 -> Some(2))
      )
    )

    val withoutLocalAggregation = evaluate(plan, rows, preaggregate = false)
    val withLocalAggregation = evaluate(plan, rows, preaggregate = true)

    assertEquals(withoutLocalAggregation, BigInt(756000))
    assertEquals(withLocalAggregation, withoutLocalAggregation)
  }

  test("an empty neighbor-key union is one scalar local-frequency group") {
    val plan = frequencyPlan(cartesianPlan())
    assertEquals(
      plan.bags.map(bag => bag.bagId.value -> bag.localGrouping),
      Vector(100 -> Vector.empty, 200 -> Vector.empty)
    )

    val nonEmpty = Map(
      bagId(100) -> Vector(row(2)(0 -> Some(1)), row(3)(0 -> Some(2))),
      bagId(200) -> Vector(row(7)(1 -> Some(3)), row(11)(1 -> Some(4)))
    )
    assertEquals(evaluate(plan, nonEmpty, preaggregate = true), BigInt(90))

    val emptyChild = nonEmpty.updated(bagId(200), Vector.empty)
    assertEquals(evaluate(plan, emptyChild, preaggregate = true), BigInt(0))

    val emptyRoot = nonEmpty.updated(bagId(100), Vector.empty)
    assertEquals(evaluate(plan, emptyRoot, preaggregate = true), BigInt(0))
  }

  test("staged set supports discard huge and NULL orphan rows before frequency aggregation") {
    val separator = variable(0)
    val hugeOrphan = BigInt(10).pow(200000)
    val rootRows = Vector(
      WeightedRow(Map(separator -> Some(1)), BigInt(7)),
      WeightedRow(Map(separator -> Some(2)), hugeOrphan),
      WeightedRow(Map(separator -> None), hugeOrphan)
    )
    val childRows = Vector(
      WeightedRow(Map(separator -> Some(1)), BigInt(11)),
      WeightedRow(Map(separator -> Some(3)), hugeOrphan),
      WeightedRow(Map(separator -> None), hugeOrphan)
    )

    // Bottom-up observes only the child's non-NULL set support. Only after
    // that filter is the root globally extendible and safe to aggregate.
    val childSupport = childRows.iterator
      .flatMap(_.values(separator))
      .toSet
    val globallySafeRoot = rootRows.filter(row =>
      row.values(separator).exists(childSupport)
    )
    val rootFrequency = aggregateTable(globallySafeRoot, Vector(separator))

    // The inbound top-down support makes the child globally extendible before
    // its own aggregation. Ordinary equality rejects both NULL rows.
    val rootSupport = globallySafeRoot.iterator
      .flatMap(_.values(separator))
      .toSet
    val globallySafeChild = childRows.filter(row =>
      row.values(separator).exists(rootSupport)
    )
    val childFrequency = aggregateTable(globallySafeChild, Vector(separator))
    val exact = rootFrequency.iterator.map { case (key, frequency) =>
      frequency * childFrequency.getOrElse(key, BigInt(0))
    }.sum

    assertEquals(
      rootFrequency,
      Map[Vector[SqlValue], BigInt](Vector(Some(1)) -> BigInt(7))
    )
    assertEquals(
      childFrequency,
      Map[Vector[SqlValue], BigInt](Vector(Some(1)) -> BigInt(11))
    )
    assertEquals(exact, BigInt(77))
    assert(!rootFrequency.valuesIterator.exists(_ == hugeOrphan))
    assert(!childFrequency.valuesIterator.exists(_ == hugeOrphan))
  }

  test("an empty-separator support suppresses the target before any count") {
    val hugeOrphan = BigInt(10).pow(200000)
    val rootRows = Vector(WeightedRow(Map(variable(0) -> Some(1)), hugeOrphan))
    val emptyChild = Vector.empty[WeightedRow]
    val globallySafeRoot =
      if (emptyChild.nonEmpty) rootRows else Vector.empty[WeightedRow]

    assertEquals(
      aggregateTable(globallySafeRoot, Vector(variable(0))),
      Map.empty[Vector[SqlValue], BigInt]
    )
  }

  test("one isolated bag has a scalar local group and exact resource terms") {
    val plan = frequencyPlan(oneBagPlan())
    assertEquals(plan.bags.map(_.localGrouping), Vector(Vector.empty))
    assertEquals(plan.resources.localAggregateTerms, BigInt(1))
    assertEquals(plan.resources.localKeyProjectionTerms, BigInt(0))
    assertEquals(plan.resources.localKeyGroupingTerms, BigInt(0))
    assertEquals(plan.resources.propagatedMessageAggregateTerms, BigInt(0))
    assertEquals(plan.resources.additionalRenderedTerms, BigInt(1))
  }

  test("cutoff addition and multiplication are homomorphic at boundaries") {
    val cutoff = SaturatingBigInt64.OverflowSentinel
    val samples = Vector(
      BigInt(0),
      BigInt(1),
      cutoff - 2,
      cutoff - 1,
      cutoff,
      cutoff + 1,
      BigInt(10).pow(50000)
    )

    samples.foreach { left =>
      samples.foreach { right =>
        val boundedLeft = bounded(left)
        val boundedRight = bounded(right)
        assertEquals(
          SaturatingBigInt64.plus(boundedLeft, boundedRight),
          bounded(left + right)
        )
        assertEquals(
          SaturatingBigInt64.times(boundedLeft, boundedRight),
          bounded(left * right)
        )
      }
    }
  }

  test("cutoff finalization distinguishes maximum exact count from overflow") {
    import SaturatingBigInt64.Finalized

    assertEquals(
      SaturatingBigInt64.finalizeCount(bounded(BigInt(0))),
      Finalized.Exact(0L)
    )
    assertEquals(
      SaturatingBigInt64.finalizeCount(
        bounded(SaturatingBigInt64.MaximumExactCount)
      ),
      Finalized.Exact(Long.MaxValue)
    )
    assertEquals(
      SaturatingBigInt64.finalizeCount(
        bounded(SaturatingBigInt64.OverflowSentinel)
      ),
      Finalized.Overflow
    )
    assertEquals(
      SaturatingBigInt64.finalizeCount(bounded(BigInt(10).pow(100000))),
      Finalized.Overflow
    )
  }

  test("the pure cutoff carrier contains huge orphan groups without changing a match") {
    val hugeOrphan = SaturatingBigInt64
      .sum(Vector(BigInt(10).pow(200000)))
      .fold(problem => throw new AssertionError(problem.message), identity)
    val matching = bounded(BigInt(7))
    val message: Map[Option[Int], SaturatingBigInt64.Value] =
      Map(Some(2) -> hugeOrphan, Some(1) -> matching)

    assertEquals(hugeOrphan.value, SaturatingBigInt64.OverflowSentinel)
    assertEquals(message.get(Some(1)), Some(matching))
    assertEquals(message.get(None), None)
    assertEquals(
      SaturatingBigInt64.finalizeCount(message(Some(1))),
      SaturatingBigInt64.Finalized.Exact(7L)
    )
  }

  test("cutoff folds pin identities, invalid input, and constant-width state") {
    assertEquals(SaturatingBigInt64.sum(Vector.empty), Right(SaturatingBigInt64.zero))
    assertEquals(
      SaturatingBigInt64.product(Vector.empty),
      Right(SaturatingBigInt64.one)
    )
    assertEquals(
      SaturatingBigInt64.sum(Vector(BigInt(1), BigInt(-1))),
      Left(SaturatingBigInt64.NegativeMultiplicity(BigInt(-1)))
    )

    val resources = SaturatingBigInt64.foldResources(BigInt(1000000))
    assertEquals(resources.binaryOperationCount, BigInt(999999))
    assertEquals(resources.maximumLiveCarrierValues, 2)
    assertEquals(resources.maximumStoredDecimalDigits, 19)
    assertEquals(resources.maximumBinaryIntermediateDecimalDigits, 38)
    assertEquals(
      SaturatingBigInt64.foldResources(BigInt(0)).binaryOperationCount,
      BigInt(0)
    )
  }

  test("the saturating contract remains explicitly separate from current SQL") {
    val lowering = SaturatingBigInt64.aggregateLoweringRequirements
    assertEquals(
      PostgreSqlFrequencyPropagation.SaturatingCountArithmetic.multiplicity,
      PostgreSqlFrequencyPropagation.MultiplicityCarrier
        .SaturatingBigInt64WithOverflowSentinel
    )
    assertEquals(
      frequencyPlan(oneBagPlan()).arithmetic,
      PostgreSqlFrequencyPropagation.ExactCountArithmetic
    )
    assertEquals(lowering.cutoff, SaturatingBigInt64.OverflowSentinel)
    assert(lowering.mustClampEveryTransitionAddition)
    assert(!lowering.postAggregateClampIsSufficientForArbitraryGroups)
    assert(!lowering.supportedByCurrentBuiltInNumericSum)
  }

  private def evaluate(
      plan: Plan,
      rows: Map[BagId, Vector[WeightedRow]],
      preaggregate: Boolean
  ): BigInt = {
    val messages = mutable.ArrayBuffer.empty[MessageTable]
    plan.messages.foreach { message =>
      val localRows =
        if (preaggregate)
          aggregateRows(rows(message.source), plan.bagById(message.source).localGrouping)
        else rows(message.source)
      val weighted = applyMessages(localRows, message.inputMessageOrdinals, plan, messages)
      val output =
        if (preaggregate && !message.requiresRegrouping)
          projectFunctionalMessage(weighted, message.separator)
        else aggregateTable(weighted, message.separator)
      messages += output
    }

    val rootRows =
      if (preaggregate)
        aggregateRows(rows(plan.root), plan.bagById(plan.root).localGrouping)
      else rows(plan.root)
    applyMessages(
      rootRows,
      plan.finalization.inputMessageOrdinals,
      plan,
      messages
    ).iterator.map(_.multiplicity).sum
  }

  private def aggregateRows(
      rows: Vector[WeightedRow],
      keys: Vector[VariableId]
  ): Vector[WeightedRow] = {
    val grouped = aggregateTable(rows, keys)
    if (grouped.isEmpty && keys.isEmpty)
      Vector(WeightedRow(Map.empty, BigInt(0)))
    else
      grouped.iterator.map { case (values, multiplicity) =>
        WeightedRow(keys.zip(values).toMap, multiplicity)
      }.toVector
  }

  private def aggregateTable(
      rows: Vector[WeightedRow],
      keys: Vector[VariableId]
  ): MessageTable =
    rows.foldLeft(Map.empty[Vector[SqlValue], BigInt]) { (result, row) =>
      val key = keys.map(row.values)
      result.updated(key, result.getOrElse(key, BigInt(0)) + row.multiplicity)
    }

  private def projectFunctionalMessage(
      rows: Vector[WeightedRow],
      keys: Vector[VariableId]
  ): MessageTable = {
    val entries = rows.map(row => keys.map(row.values) -> row.multiplicity)
    assertEquals(entries.map(_._1).distinct.size, entries.size)
    entries.toMap
  }

  private def applyMessages(
      initial: Vector[WeightedRow],
      ordinals: Vector[Int],
      plan: Plan,
      tables: mutable.ArrayBuffer[MessageTable]
  ): Vector[WeightedRow] =
    ordinals.foldLeft(initial) { (current, ordinal) =>
      val message = plan.messages(ordinal)
      current.flatMap { row =>
        val key = message.separator.map(row.values)
        val ordinaryEqualityRejectsNull =
          message.separator.nonEmpty && key.exists(_.isEmpty)
        if (ordinaryEqualityRejectsNull) Vector.empty
        else
          tables(ordinal)
            .get(key)
            .map(frequency => row.copy(multiplicity = row.multiplicity * frequency))
            .toVector
      }
    }

  private def overlappingSeparatorPlan(): BagFactorPlan = {
    val seed = oneBagPlan()
    val root = bagId(10)
    val left = bagId(20)
    val right = bagId(30)
    val leaf = bagId(40)
    seed.copy(
      root = root,
      factors = Vector(
        factor(root, Vector(0, 1, 2), 0, None, Vector(left, right)),
        factor(left, Vector(0, 1, 3), 1, Some(root), Vector(leaf)),
        factor(right, Vector(1, 2), 1, Some(root), Vector.empty),
        factor(leaf, Vector(1, 3, 4), 2, Some(left), Vector.empty)
      ),
      arcs = Vector(
        OrientedBagArc(root, left, variables(0, 1)),
        OrientedBagArc(root, right, variables(1, 2)),
        OrientedBagArc(left, leaf, variables(1, 3))
      )
    )
  }

  private def cartesianPlan(): BagFactorPlan = {
    val seed = oneBagPlan()
    val root = bagId(100)
    val child = bagId(200)
    seed.copy(
      root = root,
      factors = Vector(
        factor(root, Vector(0), 0, None, Vector(child)),
        factor(child, Vector(1), 1, Some(root), Vector.empty)
      ),
      arcs = Vector(OrientedBagArc(root, child, Vector.empty))
    )
  }

  private def oneBagPlan(): BagFactorPlan = {
    val hypergraph = graph(Seq(Seq(0)))
    val input = candidateInput(hypergraph, 1, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 1,
      bags = Vector(coveredBag(1, Vector(0), Vector(0))),
      edges = Vector.empty
    )
    certificate(input, decomposition)
    planned(input, decomposition)
  }

  private def factor(
      id: BagId,
      vertexIds: Vector[Int],
      depth: Int,
      parent: Option[BagId],
      children: Vector[BagId]
  ): BagFactor =
    BagFactor(
      bagId = id,
      vertices = vertexIds.map(variable),
      supportCover = Vector.empty,
      ownedEdgeIds = Vector.empty,
      depth = depth,
      parent = parent,
      children = children
    )

  private def row(
      multiplicity: Int
  )(entries: (Int, SqlValue)*): WeightedRow =
    WeightedRow(
      entries.iterator.map { case (id, value) => variable(id) -> value }.toMap,
      BigInt(multiplicity)
    )

  private def variables(ids: Int*): Vector[VariableId] = ids.toVector.map(variable)

  private def bounded(value: BigInt): SaturatingBigInt64.Value =
    SaturatingBigInt64
      .fromNonNegative(value)
      .fold(problem => throw new AssertionError(problem.message), identity)

  private def frequencyPlan(source: BagFactorPlan): Plan =
    PostgreSqlFrequencyPropagation
      .derive(source)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )
}
