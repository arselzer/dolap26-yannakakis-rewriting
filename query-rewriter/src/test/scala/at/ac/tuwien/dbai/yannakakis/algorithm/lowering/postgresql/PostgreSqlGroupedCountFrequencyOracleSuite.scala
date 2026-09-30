package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlan
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningOracleTestSupport._
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlFrequencyPropagation.{
  ErrorCode,
  Message,
  Plan
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}

import scala.collection.mutable

/**
  * Independent multiset oracle for the one-key grouped COUNT message model.
  * It deliberately does not call the renderer, emitter, or a database.
  */
class PostgreSqlGroupedCountFrequencyOracleSuite extends munit.FunSuite {
  private type SqlValue = Option[Int]
  private type FactorRows = Map[BagId, Vector[FactorRow]]
  private type MessageTable = Map[Vector[SqlValue], BigInt]
  private type GroupedCount = Map[SqlValue, BigInt]

  private final class FactorRow(
      val values: Map[VariableId, SqlValue],
      val multiplicity: BigInt
  ) {
    def withMultiplicity(value: BigInt): FactorRow =
      new FactorRow(values, value)
  }

  test("terminal messages reroot canonically at the retained-key owner") {
    val source = chainPlan()
    val grouped = groupedPlan(source, groupVariable = 3)
    val scalar = scalarPlan(source)

    assertEquals(source.root.value, 7)
    assertEquals(grouped.root.value, 9)
    assertEquals(grouped.postorder.map(_.value), Vector(7, 3, 9))
    assertEquals(
      grouped.messages.map(messageShape),
      Vector(
        (0, 7, 3, Vector(1), Vector.empty),
        (1, 3, 9, Vector(2), Vector(0))
      )
    )
    assertEquals(grouped.finalization.root.value, 9)
    assertEquals(grouped.finalization.inputMessageOrdinals, Vector(1))
    assertEquals(grouped.finalization.groupingVariables.map(_.value), Vector(3))
    assertEquals(
      grouped.bagById(bagId(9)).localGrouping.map(_.value),
      Vector(2, 3)
    )
    assertEquals(
      grouped.resources.additionalRenderedTerms,
      scalar.resources.additionalRenderedTerms + 2
    )
    assertEquals(
      PostgreSqlFrequencyPropagation.validateGrouped(
        source,
        bagId(9),
        variable(3),
        grouped
      ),
      Right(grouped)
    )

    assertEquals(
      errorCodes(
        PostgreSqlFrequencyPropagation.deriveGrouped(
          source,
          bagId(99),
          variable(3)
        )
      ),
      Set[ErrorCode](ErrorCode.UnknownFinalizationRoot)
    )
    assertEquals(
      errorCodes(
        PostgreSqlFrequencyPropagation.deriveGrouped(
          source,
          bagId(7),
          variable(3)
        )
      ),
      Set[ErrorCode](ErrorCode.RetainedVariableOutsideRoot)
    )
    assertEquals(
      errorCodes(
        PostgreSqlFrequencyPropagation.validateGrouped(
          source,
          bagId(9),
          variable(3),
          grouped.copy(
            finalization = grouped.finalization.copy(
              groupingVariables = Vector(variable(2))
            )
          )
        )
      ),
      Set[ErrorCode](ErrorCode.CandidateMismatch)
    )
  }

  test("duplicates, shared messages, NULL separators, and empty input are exact") {
    val source = chainPlan()
    val plan = groupedPlan(source, groupVariable = 3)
    val rows = adversarialRows()
    val expected = Map[SqlValue, BigInt](
      Some(100) -> BigInt(825),
      Some(101) -> BigInt(731)
    )

    assertGroupedExact(plan, rows, expected)

    val emptyMiddle = rows.updated(bagId(3), Vector.empty)
    assertGroupedExact(plan, emptyMiddle, Map.empty)
  }

  test("a nullable retained separator uses ordinary equality and emits no NULL group") {
    val source = chainPlan()
    val plan = groupedPlan(source, groupVariable = 2)
    val expected = Map[SqlValue, BigInt](
      Some(10) -> BigInt(1290),
      Some(11) -> BigInt(266)
    )

    assertEquals(plan.finalization.groupingVariables, Vector(variable(2)))
    assert(plan.messages.last.separator.contains(variable(2)))
    val actual = assertGroupedExact(plan, adversarialRows(), expected)
    assert(!actual.contains(None))
  }

  test("BIGINT representability is checked independently for every group") {
    val source = chainPlan()
    val plan = groupedPlan(source, groupVariable = 3)
    val maximum = BigInt(Long.MaxValue)

    val independentlyRepresentable = factorRows(
      7 -> Vector(row(1)(0 -> 0, 1 -> 1)),
      3 -> Vector(row(1)(1 -> 1, 2 -> 10)),
      9 -> Vector(
        bigRow(maximum)(2 -> 10, 3 -> 100),
        bigRow(maximum)(2 -> 10, 3 -> 101)
      )
    )
    val representable = assertGroupedExact(
      plan,
      independentlyRepresentable,
      Map[SqlValue, BigInt](
        Some(100) -> maximum,
        Some(101) -> maximum
      )
    )
    assert(representable.values.forall(_ <= maximum))
    assert(representable.values.sum > maximum)

    val rows = factorRows(
      7 -> Vector(row(1)(0 -> 0, 1 -> 1)),
      3 -> Vector(row(1)(1 -> 1, 2 -> 10)),
      9 -> Vector(
        bigRow(maximum)(2 -> 10, 3 -> 100),
        bigRow(1)(2 -> 10, 3 -> 100),
        bigRow(maximum)(2 -> 10, 3 -> 101)
      )
    )
    val expected = Map[SqlValue, BigInt](
      Some(100) -> (maximum + 1),
      Some(101) -> maximum
    )

    val actual = assertGroupedExact(plan, rows, expected)
    assertEquals(plan.arithmetic.finalCount,
      PostgreSqlFrequencyPropagation.FinalCountCarrier.SignedBigInt64)
    assert(plan.arithmetic.checkedFinalConversion)
    assert(actual(Some(100)) > maximum)
    assert(actual(Some(101)) <= maximum)
  }

  private def chainPlan(): BagFactorPlan = {
    val hypergraph = graph(
      Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3), Seq(3))
    )
    val input = candidateInput(hypergraph, 1, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 7,
      bags = Vector(
        coveredBag(7, Vector(0, 1), Vector(0)),
        coveredBag(3, Vector(1, 2), Vector(1)),
        coveredBag(9, Vector(2, 3), Vector(2))
      ),
      edges = Vector(treeEdge(3, 9), treeEdge(7, 3))
    )
    certificate(input, decomposition)
    planned(input, decomposition)
  }

  private def adversarialRows(): FactorRows = {
    val oversizedOrphan = BigInt(10).pow(50)
    factorRows(
      7 -> Vector(
        row(2)(0 -> 0, 1 -> 1),
        row(3)(0 -> 9, 1 -> 2),
        nullableBigRow(oversizedOrphan)(0 -> Some(8), 1 -> None)
      ),
      3 -> Vector(
        row(5)(1 -> 1, 2 -> 10),
        row(7)(1 -> 1, 2 -> 11),
        row(11)(1 -> 2, 2 -> 10),
        nullableBigRow(oversizedOrphan)(1 -> None, 2 -> Some(10)),
        nullableBigRow(oversizedOrphan)(1 -> Some(1), 2 -> None)
      ),
      9 -> Vector(
        row(13)(2 -> 10, 3 -> 100),
        row(17)(2 -> 10, 3 -> 101),
        row(19)(2 -> 11, 3 -> 100),
        nullableBigRow(oversizedOrphan)(2 -> None, 3 -> Some(100))
      )
    )
  }

  private def assertGroupedExact(
      plan: Plan,
      rows: FactorRows,
      expected: GroupedCount
  ): GroupedCount = {
    val bruteForce = bruteForceGroupedNaturalJoin(plan, rows)
    val propagated = messageGroupedCount(plan, rows)
    assertEquals(bruteForce, expected)
    assertEquals(propagated, expected)
    propagated
  }

  private def bruteForceGroupedNaturalJoin(
      plan: Plan,
      rows: FactorRows
  ): GroupedCount = {
    validateRows(plan, rows)
    val identity = new FactorRow(Map.empty, BigInt(1))
    val joined = plan.bags.foldLeft(Vector(identity)) { (prefix, bag) =>
      for {
        left <- prefix
        right <- rows(bag.bagId)
        merged <- mergeOnSqlEquality(left, right)
      } yield merged
    }
    groupByRetainedVariable(plan, joined)
  }

  private def messageGroupedCount(
      plan: Plan,
      rows: FactorRows
  ): GroupedCount = {
    validateRows(plan, rows)
    val tables = mutable.ArrayBuffer.empty[MessageTable]
    plan.messages.foreach { message =>
      assertEquals(message.ordinal, tables.size)
      val weighted = applyInputMessages(
        rows(message.source),
        message.inputMessageOrdinals,
        plan,
        tables
      )
      tables += groupMessage(weighted, message.separator)
    }
    val rootRows = applyInputMessages(
      rows(plan.root),
      plan.finalization.inputMessageOrdinals,
      plan,
      tables
    )
    groupByRetainedVariable(plan, rootRows)
  }

  private def applyInputMessages(
      initial: Vector[FactorRow],
      ordinals: Vector[Int],
      plan: Plan,
      tables: mutable.ArrayBuffer[MessageTable]
  ): Vector[FactorRow] =
    ordinals.foldLeft(initial) { (current, ordinal) =>
      val input = plan.messages(ordinal)
      val table = tables(ordinal)
      current.flatMap { factorRow =>
        val key = input.separator.map(factorRow.values)
        val nullRejectingMiss = input.separator.nonEmpty && key.exists(_.isEmpty)
        if (nullRejectingMiss) Vector.empty
        else
          table
            .get(key)
            .map(value =>
              factorRow.withMultiplicity(factorRow.multiplicity * value)
            )
            .toVector
      }
    }

  private def groupMessage(
      rows: Vector[FactorRow],
      separator: Vector[VariableId]
  ): MessageTable =
    rows.foldLeft(Map.empty[Vector[SqlValue], BigInt]) { (grouped, row) =>
      val key = separator.map(row.values)
      grouped.updated(key, grouped.getOrElse(key, BigInt(0)) + row.multiplicity)
    }

  private def groupByRetainedVariable(
      plan: Plan,
      rows: Vector[FactorRow]
  ): GroupedCount = {
    assertEquals(plan.finalization.groupingVariables.size, 1)
    val variableId = plan.finalization.groupingVariables.head
    rows.foldLeft(Map.empty[SqlValue, BigInt]) { (grouped, row) =>
      val key = row.values(variableId)
      grouped.updated(key, grouped.getOrElse(key, BigInt(0)) + row.multiplicity)
    }
  }

  private def mergeOnSqlEquality(
      left: FactorRow,
      right: FactorRow
  ): Option[FactorRow] = {
    val shared = left.values.keySet intersect right.values.keySet
    val matches = shared.forall { variableId =>
      (left.values(variableId), right.values(variableId)) match {
        case (Some(leftValue), Some(rightValue)) => leftValue == rightValue
        case _                                  => false
      }
    }
    if (!matches) None
    else
      Some(
        new FactorRow(
          left.values ++ right.values,
          left.multiplicity * right.multiplicity
        )
      )
  }

  private def validateRows(plan: Plan, rows: FactorRows): Unit = {
    assertEquals(rows.keySet, plan.bagById.keySet)
    plan.bags.foreach { bag =>
      rows(bag.bagId).foreach { row =>
        assertEquals(row.values.keySet, bag.vertices.toSet)
        assert(row.multiplicity >= 0)
      }
    }
  }

  private def groupedPlan(
      source: BagFactorPlan,
      groupVariable: Int
  ): Plan =
    PostgreSqlFrequencyPropagation
      .deriveGrouped(source, bagId(9), variable(groupVariable))
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )

  private def scalarPlan(source: BagFactorPlan): Plan =
    PostgreSqlFrequencyPropagation
      .derive(source)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )

  private def factorRows(entries: (Int, Vector[FactorRow])*): FactorRows =
    entries.iterator.map { case (numericBagId, rows) =>
      bagId(numericBagId) -> rows
    }.toMap

  private def row(
      multiplicity: Int
  )(entries: (Int, Int)*): FactorRow =
    bigRow(BigInt(multiplicity))(entries: _*)

  private def bigRow(
      multiplicity: BigInt
  )(entries: (Int, Int)*): FactorRow =
    nullableBigRow(multiplicity)(entries.map { case (id, value) =>
      id -> Some(value)
    }: _*)

  private def nullableBigRow(
      multiplicity: BigInt
  )(entries: (Int, SqlValue)*): FactorRow =
    new FactorRow(
      entries.iterator.map { case (id, value) => variable(id) -> value }.toMap,
      multiplicity
    )

  private def messageShape(
      message: Message
  ): (Int, Int, Int, Vector[Int], Vector[Int]) =
    (
      message.ordinal,
      message.source.value,
      message.target.value,
      message.separator.map(_.value),
      message.inputMessageOrdinals
    )

  private def errorCodes(
      result: Either[Vector[PostgreSqlFrequencyPropagation.Error], Plan]
  ): Set[ErrorCode] =
    result.fold(_.iterator.map(_.code).toSet, _ => Set.empty)
}
