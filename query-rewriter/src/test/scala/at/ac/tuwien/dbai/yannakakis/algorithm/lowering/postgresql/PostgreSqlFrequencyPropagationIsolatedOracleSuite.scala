package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactor,
  BagFactorPlan,
  OrientedBagArc
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningOracleTestSupport._
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlFrequencyPropagation.{
  ErrorCode,
  Message,
  Plan
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}

import scala.collection.mutable

/**
  * Independent multiset oracle for the SQL-free frequency-message model.
  * No renderer, database, or production evaluator is used here.
  */
class PostgreSqlFrequencyPropagationIsolatedOracleSuite
    extends munit.FunSuite {
  private type SqlValue = Option[Int]
  private type FactorRows = Map[BagId, Vector[FactorRow]]
  private type MessageTable = Map[Vector[SqlValue], BigInt]

  private final class FactorRow(
      val values: Map[VariableId, SqlValue],
      val multiplicity: BigInt
  ) {
    def withMultiplicity(value: BigInt): FactorRow =
      new FactorRow(values, value)
  }

  test("canonical postorder and message dependencies ignore source vector order") {
    val source = branchingPlan()
    val expected = frequencyPlan(source)
    val reordered = source.copy(
      factors = source.factors.reverse,
      arcs = source.arcs.reverse
    )
    val actual = frequencyPlan(reordered)

    assertEquals(actual, expected)
    assertEquals(actual.bags.map(_.bagId.value), Vector(2, 5, 9))
    assertEquals(actual.postorder.map(_.value), Vector(2, 5, 9))
    assertEquals(
      actual.messages.map(messageShape),
      Vector(
        (0, 2, 9, Vector(0), Vector.empty),
        (1, 5, 9, Vector(0), Vector.empty)
      )
    )
    assertEquals(actual.finalization.root.value, 9)
    assertEquals(actual.finalization.inputMessageOrdinals, Vector(0, 1))
    assertEquals(
      PostgreSqlFrequencyPropagation.validate(reordered, actual),
      Right(actual)
    )
  }

  test("one bag counts every duplicate and retains NULL without a join") {
    val source = oneBagPlan()
    val rows = factorRows(
      4 -> Vector(
        nullableRow(2)(0 -> Some(1), 1 -> None),
        nullableRow(3)(0 -> Some(1), 1 -> None),
        row(1)(0 -> 2, 1 -> 3)
      )
    )

    assertExactCount(source, rows, BigInt(6))
    val plan = frequencyPlan(source)
    assertEquals(plan.postorder.map(_.value), Vector(4))
    assertEquals(plan.messages, Vector.empty)
    assertEquals(plan.finalization.inputMessageOrdinals, Vector.empty)
  }

  test("chain messages multiply and sum duplicate frequencies exactly") {
    val source = chainPlan()
    val rows = factorRows(
      7 -> Vector(
        row(2)(0 -> 0, 1 -> 1),
        row(1)(0 -> 9, 1 -> 2)
      ),
      3 -> Vector(
        row(3)(1 -> 1, 2 -> 5),
        row(4)(1 -> 2, 2 -> 8)
      ),
      9 -> Vector(
        row(5)(2 -> 5, 3 -> 7),
        row(2)(2 -> 8, 3 -> 6)
      )
    )

    assertExactCount(source, rows, BigInt(38))
    val plan = frequencyPlan(source)
    assertEquals(plan.postorder.map(_.value), Vector(9, 3, 7))
    assertEquals(
      plan.messages.map(messageShape),
      Vector(
        (0, 9, 3, Vector(2), Vector.empty),
        (1, 3, 7, Vector(1), Vector(0))
      )
    )
    assertEquals(plan.finalization.inputMessageOrdinals, Vector(1))
  }

  test("branching messages multiply independent child counts at the root") {
    val source = branchingPlan()
    val rows = factorRows(
      9 -> Vector(row(2)(0 -> 1), row(1)(0 -> 2)),
      2 -> Vector(row(3)(0 -> 1, 2 -> 20), row(5)(0 -> 2, 2 -> 21)),
      5 -> Vector(row(7)(0 -> 1, 1 -> 10))
    )

    assertExactCount(source, rows, BigInt(42))
  }

  test("C4 two-bag decomposition compresses duplicate separator groups exactly") {
    val source = c4Plan()
    val rows = factorRows(
      40 -> Vector(
        row(2)(0 -> 0, 1 -> 1, 3 -> 9),
        row(3)(0 -> 8, 1 -> 1, 3 -> 9),
        row(5)(0 -> 2, 1 -> 2, 3 -> 8)
      ),
      10 -> Vector(
        row(4)(1 -> 1, 2 -> 5, 3 -> 9),
        row(5)(1 -> 1, 2 -> 6, 3 -> 9),
        row(7)(1 -> 2, 2 -> 4, 3 -> 8)
      )
    )

    assertExactCount(source, rows, BigInt(80))
    val plan = frequencyPlan(source)
    assertEquals(plan.postorder.map(_.value), Vector(10, 40))
    assertEquals(
      plan.messages.map(messageShape),
      Vector((0, 10, 40, Vector(1, 3), Vector.empty))
    )
  }

  test("ordinary separator equality rejects NULL-to-NULL matches") {
    val source = twoBagPlan()
    val rows = factorRows(
      7 -> Vector(
        nullableRow(2)(0 -> Some(10), 1 -> None),
        row(3)(0 -> 11, 1 -> 1)
      ),
      3 -> Vector(
        nullableRow(5)(1 -> None, 2 -> Some(20)),
        row(7)(1 -> 1, 2 -> 21)
      )
    )

    assertExactCount(source, rows, BigInt(21))
  }

  test("an unmatched intermediate frequency may exceed 38 decimal digits") {
    val source = twoBagPlan()
    val oversized = BigInt(10).pow(50)
    val rows = factorRows(
      7 -> Vector(row(1)(0 -> 10, 1 -> 1)),
      3 -> Vector(bigRow(oversized)(1 -> 2, 2 -> 20))
    )

    assertExactCount(source, rows, BigInt(0))
  }

  test("an empty factor makes both natural join and message count zero") {
    val source = chainPlan()
    val rows = factorRows(
      7 -> Vector(row(2)(0 -> 0, 1 -> 1)),
      3 -> Vector.empty[FactorRow],
      9 -> Vector(row(5)(2 -> 5, 3 -> 7))
    )

    assertExactCount(source, rows, BigInt(0))
    assertExactCount(oneBagPlan(), factorRows(4 -> Vector.empty), BigInt(0))
  }

  test("empty separator is a valid scalar message and yields Cartesian count") {
    val source = structurallyValidCartesianPlan()
    val derived = frequencyPlan(source)
    val rows = factorRows(
      100 -> Vector(row(2)(0 -> 1), row(3)(0 -> 2)),
      200 -> Vector(row(7)(1 -> 10), row(11)(1 -> 20))
    )

    assertEquals(derived.messages.map(_.separator), Vector(Vector.empty))
    assertEquals(
      derived.bags.find(_.bagId == bagId(200)).get.separatorToParent,
      Some(Vector.empty)
    )
    assertExactCount(source, rows, BigInt(90))
  }

  test("source and candidate adversarial mutations fail closed") {
    val source = chainPlan()
    val canonical = frequencyPlan(source)
    val rootFactor = source.factors.find(_.bagId == bagId(7)).get
    val middleFactor = source.factors.find(_.bagId == bagId(3)).get
    val firstArc = source.arcs.find(_.child == bagId(3)).get

    val mutations = Vector(
      source.copy(factors = source.factors :+ source.factors.head) ->
        ErrorCode.DuplicateBag,
      source.copy(root = bagId(99)) -> ErrorCode.MissingRoot,
      source.copy(
        factors = replaceFactor(
          source,
          middleFactor.copy(parent = None)
        )
      ) -> ErrorCode.ParentMismatch,
      source.copy(
        factors = replaceFactor(
          source,
          rootFactor.copy(children = Vector.empty)
        )
      ) -> ErrorCode.ChildrenMismatch,
      source.copy(
        arcs = source.arcs.map(arc =>
          if (arc == firstArc) arc.copy(separator = Vector.empty) else arc
        )
      ) -> ErrorCode.SeparatorMismatch,
      source.copy(arcs = source.arcs.dropRight(1)) -> ErrorCode.NonTree,
      source.copy(arcs = source.arcs :+ source.arcs.head) ->
        ErrorCode.DuplicateArcChild,
      source.copy(
        factors = replaceFactor(
          source,
          rootFactor.copy(vertices = rootFactor.vertices.reverse)
        )
      ) -> ErrorCode.NonCanonicalBag
    )

    mutations.foreach { case (mutated, expectedCode) =>
      assert(
        errorCodes(PostgreSqlFrequencyPropagation.derive(mutated))
          .contains(expectedCode),
        s"expected ${expectedCode.value}"
      )
    }

    assertEquals(
      errorCodes(PostgreSqlFrequencyPropagation.validate(source, null)),
      Set[ErrorCode](ErrorCode.MissingValue)
    )
    assertEquals(
      errorCodes(
        PostgreSqlFrequencyPropagation.validate(
          source,
          canonical.copy(postorder = canonical.postorder.reverse)
        )
      ),
      Set[ErrorCode](ErrorCode.CandidateMismatch)
    )
  }

  test("arithmetic contract freezes unbounded numeric messages and checked bigint finalization") {
    val arithmetic = frequencyPlan(oneBagPlan()).arithmetic
    assertEquals(
      arithmetic.multiplicity,
      PostgreSqlFrequencyPropagation.MultiplicityCarrier.ArbitraryPrecisionNumeric
    )
    assertEquals(
      arithmetic.finalCount,
      PostgreSqlFrequencyPropagation.FinalCountCarrier.SignedBigInt64
    )
    assertEquals(arithmetic.emptyInputCount, BigInt(0))
    assert(arithmetic.checkedFinalConversion)
  }

  private def oneBagPlan(): BagFactorPlan = {
    val hypergraph = graph(Seq(Seq(0, 1)))
    val input = candidateInput(hypergraph, 1, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 4,
      bags = Vector(coveredBag(4, Vector(0, 1), Vector(0))),
      edges = Vector.empty
    )
    certificate(input, decomposition)
    planned(input, decomposition)
  }

  private def twoBagPlan(): BagFactorPlan = {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2)))
    val input = candidateInput(hypergraph, 1, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 7,
      bags = Vector(
        coveredBag(7, Vector(0, 1), Vector(0)),
        coveredBag(3, Vector(1, 2), Vector(1))
      ),
      edges = Vector(treeEdge(7, 3))
    )
    certificate(input, decomposition)
    planned(input, decomposition)
  }

  private def chainPlan(): BagFactorPlan = {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3)))
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

  private def branchingPlan(): BagFactorPlan = {
    val hypergraph = graph(Seq(Seq(0), Seq(0, 1), Seq(0, 2)))
    val input = candidateInput(hypergraph, 2, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 9,
      bags = Vector(
        coveredBag(9, Vector(0), Vector(2)),
        coveredBag(2, Vector(0, 2), Vector(2)),
        coveredBag(5, Vector(0, 1), Vector(1))
      ),
      edges = Vector(treeEdge(9, 5), treeEdge(2, 9))
    )
    certificate(input, decomposition)
    planned(input, decomposition)
  }

  private def c4Plan(): BagFactorPlan = {
    val hypergraph = graph(
      Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3), Seq(3, 0))
    )
    val input = candidateInput(hypergraph, 2, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 40,
      bags = Vector(
        coveredBag(40, Vector(0, 1, 3), Vector(0, 3)),
        coveredBag(10, Vector(1, 2, 3), Vector(1, 2))
      ),
      edges = Vector(treeEdge(40, 10))
    )
    certificate(input, decomposition)
    planned(input, decomposition)
  }

  /**
    * CandidateSearchInput currently rejects disconnected hypergraphs, so the
    * public planner cannot originate an empty-separator tree. This fixture
    * deliberately changes only the structural fields consumed by the pure
    * model, starting from a genuine planner-produced BagFactorPlan.
    */
  private def structurallyValidCartesianPlan(): BagFactorPlan = {
    val seed = oneBagPlan()
    val root = bagId(100)
    val child = bagId(200)
    seed.copy(
      root = root,
      factors = Vector(
        BagFactor(
          bagId = root,
          vertices = Vector(variable(0)),
          supportCover = Vector.empty,
          ownedEdgeIds = Vector.empty,
          depth = 0,
          parent = None,
          children = Vector(child)
        ),
        BagFactor(
          bagId = child,
          vertices = Vector(variable(1)),
          supportCover = Vector.empty,
          ownedEdgeIds = Vector.empty,
          depth = 1,
          parent = Some(root),
          children = Vector.empty
        )
      ),
      arcs = Vector(OrientedBagArc(root, child, Vector.empty))
    )
  }

  private def assertExactCount(
      source: BagFactorPlan,
      rows: FactorRows,
      expected: BigInt
  ): Unit = {
    val plan = frequencyPlan(source)
    assertEquals(bruteForceNaturalJoinCount(plan, rows), expected)
    assertEquals(messageCount(plan, rows), expected)
  }

  private def bruteForceNaturalJoinCount(
      plan: Plan,
      rows: FactorRows
  ): BigInt = {
    validateRows(plan, rows)
    val identity = new FactorRow(Map.empty, BigInt(1))
    val joined = plan.bags.foldLeft(Vector(identity)) { (prefix, bag) =>
      for {
        left <- prefix
        right <- rows(bag.bagId)
        merged <- mergeOnSqlEquality(left, right)
      } yield merged
    }
    joined.iterator.map(_.multiplicity).sum
  }

  private def messageCount(plan: Plan, rows: FactorRows): BigInt = {
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
      tables += group(weighted, message.separator)
    }

    val rootRows = applyInputMessages(
      rows(plan.root),
      plan.finalization.inputMessageOrdinals,
      plan,
      tables
    )
    rootRows.iterator.map(_.multiplicity).sum
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
        val nullRejectingMiss =
          input.separator.nonEmpty && key.exists(_.isEmpty)
        if (nullRejectingMiss) Vector.empty
        else
          table
            .get(key)
            .map(frequency =>
              factorRow.withMultiplicity(factorRow.multiplicity * frequency)
            )
            .toVector
      }
    }

  private def group(
      rows: Vector[FactorRow],
      separator: Vector[VariableId]
  ): MessageTable =
    rows.foldLeft(Map.empty[Vector[SqlValue], BigInt]) { (grouped, row) =>
      val key = separator.map(row.values)
      grouped.updated(
        key,
        grouped.getOrElse(key, BigInt(0)) + row.multiplicity
      )
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

  private def frequencyPlan(source: BagFactorPlan): Plan =
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

  private def nullableRow(
      multiplicity: Int
  )(entries: (Int, SqlValue)*): FactorRow =
    nullableBigRow(BigInt(multiplicity))(entries: _*)

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

  private def replaceFactor(
      plan: BagFactorPlan,
      replacement: BagFactor
  ): Vector[BagFactor] =
    plan.factors.map(factor =>
      if (factor.bagId == replacement.bagId) replacement else factor
    )

  private def errorCodes(
      result: Either[Vector[PostgreSqlFrequencyPropagation.Error], Plan]
  ): Set[ErrorCode] =
    result.fold(_.iterator.map(_.code).toSet, _ => Set.empty)
}
