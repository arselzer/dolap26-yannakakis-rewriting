package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactor,
  BagFactorPlan,
  EdgeOwnership,
  OrientedBagArc
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningOracleTestSupport._
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlAggregatePropagation.{
  AggregateKey,
  AggregateRequest,
  Annotation,
  ErrorCode,
  ExactDecimal,
  LaneKind,
  LaneLoweringStatus,
  LaneRequest,
  MeasureLane,
  Plan,
  Terminal,
  TerminalValue
}
import at.ac.tuwien.dbai.yannakakis.core.{BagId, ColumnId, VariableId}

/**
  * Independent multiset oracle for the SQL-free unguarded aggregate annotation
  * model (U0 of `docs/journal-extension/unguarded-aggregate-plan.md`).
  *
  * The expected value of every correctness assertion comes from
  * [[bruteForce]], which materialises the natural join of the supplied bag
  * factors and aggregates it directly with its own arithmetic. It never
  * consults [[PostgreSqlAggregatePropagation]] -- not even its semiring
  * operations. [[messageResult]] separately *executes* the derived message
  * program without re-deriving it. Agreement between the two is the U0
  * correctness statement; no renderer, database, or production evaluator is
  * involved.
  */
class PostgreSqlAggregatePropagationIsolatedOracleSuite extends munit.FunSuite {

  /** One SQL cell. `None` is SQL NULL. */
  private type Cell = Option[BigDecimal]
  private type GroupTuple = Vector[Cell]
  private type FactorRows = Map[BagId, Vector[FactorRow]]

  /** A schema position: either a structural variable or a payload column. */
  private sealed trait Term
  private object Term {
    final case class Var(variable: VariableId) extends Term
    final case class Col(column: ColumnId) extends Term
  }

  private type Binding = Map[Term, Cell]

  /** One group's aggregated state, in a form both sides can produce. */
  private final case class GroupResult(
      witnesses: BigInt,
      lanes: Vector[(BigInt, BigDecimal)]
  )

  /**
    * One stored bag-factor tuple. `values` binds the bag's structural
    * variables, `payload` binds occurrence-owned columns that are not
    * hypergraph variables, and `multiplicity` is at least one because a stored
    * SQL bag tuple exists.
    */
  private final class FactorRow(
      val values: Map[VariableId, Cell],
      val payload: Map[ColumnId, Cell],
      val multiplicity: BigInt
  )

  // -------------------------------------------------------------------
  // Algebra
  // -------------------------------------------------------------------

  test("the annotation algebra is a semiring with the documented identities") {
    val a = annotation(2, Vector((1, 5)))
    val b = annotation(7, Vector((0, 0)))
    val c = annotation(3, Vector((0, 0)))

    assertEquals(Annotation.join(a, Annotation.unit(1)), a)
    assertEquals(Annotation.merge(a, Annotation.empty(1)), a)
    assertEquals(Annotation.join(a, b), Annotation.join(b, a))
    assertEquals(Annotation.merge(a, b), Annotation.merge(b, a))
    assertEquals(
      Annotation.join(Annotation.join(a, b), c),
      Annotation.join(a, Annotation.join(b, c))
    )
    assertEquals(
      Annotation.merge(Annotation.merge(a, b), c),
      Annotation.merge(a, Annotation.merge(b, c))
    )
    assertEquals(
      Annotation.join(a, Annotation.merge(b, c)),
      Annotation.merge(Annotation.join(a, b), Annotation.join(a, c))
    )
    assertEquals(Annotation.join(a, Annotation.empty(1)), Annotation.empty(1))
  }

  test("a lane supplied by one component is weighted by the other's witnesses") {
    val measured = annotation(2, Vector((2, 10)))
    val unmeasured = annotation(7, Vector((0, 0)))
    val joined = Annotation.join(measured, unmeasured)

    assertEquals(joined.witnesses, BigInt(14))
    assertEquals(joined.lanes.head.nonNullWitnesses, BigInt(14))
    assertEquals(joined.lanes.head.sum, BigDecimal(70))
  }

  test("support is idempotent and equivalent to a positive witness count") {
    val supported = annotation(4, Vector((0, 0)))
    assert(Annotation.merge(supported, supported).support)
    assert(Annotation.join(supported, supported).support)
    assert(!Annotation.join(supported, Annotation.empty(1)).support)

    Vector(supported, annotation(1, Vector((1, 3))), Annotation.empty(1))
      .foreach(state => assertEquals(state.support, state.witnesses > 0))
  }

  test("terminal rules distinguish an absent group from an all-NULL measure") {
    val allNull = annotation(9, Vector((0, 0)))
    assert(Terminal.isEmitted(allNull))
    assertEquals(Terminal.countStar(allNull), BigInt(9))
    assertEquals(Terminal.countColumn(allNull, 0), BigInt(0))
    assertEquals(Terminal.sumColumn(allNull, 0), TerminalValue.SqlNull)
    assertEquals(Terminal.averageColumn(allNull, 0), TerminalValue.SqlNull)

    val measured = annotation(9, Vector((3, 12)))
    assertEquals(Terminal.countColumn(measured, 0), BigInt(3))
    assertEquals(
      Terminal.sumColumn(measured, 0),
      TerminalValue.ExactDecimalValue(BigDecimal(12))
    )
    assertEquals(
      Terminal.averageColumn(measured, 0),
      TerminalValue.ExactRatio(BigDecimal(12), BigInt(3))
    )

    assert(!Terminal.isEmitted(Annotation.empty(1)))
  }

  test("lane arithmetic is exact beyond signed 64 bits and beyond DECIMAL128") {
    val huge = BigInt(10).pow(40)
    val wide = BigDecimal("0.1234567890123456789012345678901234567890")
    val left = Annotation(
      support = true,
      witnesses = huge,
      lanes = Vector(MeasureLane(huge, wide))
    )
    val right = Annotation(
      support = true,
      witnesses = huge,
      lanes = Vector(MeasureLane(BigInt(0), ExactDecimal.zero))
    )
    val joined = Annotation.join(left, right)

    assertEquals(joined.witnesses, huge * huge)
    assertEquals(joined.lanes.head.nonNullWitnesses, huge * huge)
    assertEquals(joined.lanes.head.sum, exactProduct(wide, huge))
    assertEquals(Annotation.merge(left, left).lanes.head.sum, exactSum(wide, wide))
  }

  test("the arithmetic contract records both still-unproved PostgreSQL lanes") {
    val contract = PostgreSqlAggregatePropagation.ExactAggregateArithmetic
    assertEquals(contract.emptyInputGroupCount, BigInt(0))
    assertEquals(contract.absentCountColumnValue, BigInt(0))
    assert(contract.allNullMeasureSumIsNull)
    assert(!contract.nullableCountHasPostgreSqlCarrierProof)
    assert(!contract.signedSumHasPostgreSqlCarrierProof)
    assertEquals(
      LaneKind.NonNullCount.loweringStatus,
      LaneLoweringStatus.BlockedPendingSupportSplitProof
    )
    assertEquals(
      LaneKind.ExactSum.loweringStatus,
      LaneLoweringStatus.BlockedPendingCarrierProof
    )
  }

  // -------------------------------------------------------------------
  // Derivation shape
  // -------------------------------------------------------------------

  test("canonical postorder and message dependencies ignore source vector order") {
    val source = starPlan()
    val request = AggregateRequest.countStarGroupedBy(structural(1), structural(2))
    val root = bagId(9)
    val expected = derived(source, root, request)
    val reordered = source.copy(
      factors = source.factors.reverse,
      arcs = source.arcs.reverse
    )

    assertEquals(derived(reordered, root, request), expected)
    assertEquals(
      PostgreSqlAggregatePropagation.validate(reordered, root, request, expected),
      Right(expected)
    )
  }

  test("group keys on different branches are owned apart and multiply at the terminal") {
    val source = starPlan()
    val request = AggregateRequest.countStarGroupedBy(structural(1), structural(2))
    val plan = derived(source, bagId(9), request)

    assertEquals(
      plan.keyBindings.map(binding => (keyLabel(binding.key), binding.owner.value)),
      Vector(("v1", 5), ("v2", 2))
    )
    assertEquals(
      plan.messages.map(message =>
        (
          message.source.value,
          message.separator.map(_.value),
          message.carriedKeys.map(keyLabel)
        )
      ),
      Vector((2, Vector(0), Vector("v2")), (5, Vector(0), Vector("v1")))
    )
    assertEquals(plan.carriedKeyProductBags.map(_.value), Vector(9))
    assertEquals(plan.maximumCarriedKeys, 1)
    assertEquals(plan.finalization.groupKeys, request.groupKeys)
  }

  test("the default terminal is a deterministic owner of the first output key") {
    val star = starPlan()
    assertEquals(
      defaultRoot(star, AggregateRequest.countStarGroupedBy(structural(1))),
      bagId(5)
    )
    assertEquals(
      defaultRoot(star, AggregateRequest.countStarGroupedBy(structural(2))),
      bagId(2)
    )
    assertEquals(
      defaultRoot(star, AggregateRequest.countStarGroupedBy(structural(0))),
      bagId(2)
    )
    assertEquals(defaultRoot(star, AggregateRequest.globalCountStar), star.root)
    assertEquals(
      defaultRoot(star, AggregateRequest.countStarGroupedBy(value(1, 5))),
      bagId(5)
    )
  }

  test("a live key is never a separator variable on any arc it crosses") {
    everyFixture.foreach { case (label, source, request) =>
      source.factors.map(_.bagId).foreach { root =>
        PostgreSqlAggregatePropagation.derive(source, root, request) match {
          case Left(_) => ()
          case Right(plan) =>
            plan.messages.foreach { message =>
              message.carriedKeys.foreach {
                case AggregateKey.Structural(variable) =>
                  assert(
                    !message.separator.contains(variable),
                    s"$label carried v${variable.value} inside a separator"
                  )
                case AggregateKey.OccurrenceValue(_) => ()
              }
            }
        }
      }
    }
  }

  test("terminal choice re-roots only the annotation orientation") {
    val source = c4Plan()
    val request = AggregateRequest.countStarGroupedBy(structural(0), structural(2))
    val atForty = derived(source, bagId(40), request)
    val atTen = derived(source, bagId(10), request)

    assertEquals(atForty.root, bagId(40))
    assertEquals(atTen.root, bagId(10))
    assertEquals(atForty.messages.map(_.source), Vector(bagId(10)))
    assertEquals(atTen.messages.map(_.source), Vector(bagId(40)))
    assertEquals(atForty.bags.map(_.vertices), atTen.bags.map(_.vertices))
  }

  test("a carried live key does not by itself force a message regrouping") {
    val source = chainPlan()

    // Bag 3 owns v2 and also separates bag 9 on v2, so its message schema
    // already equals the joined schema and no regrouping SUM is needed.
    val grouped = derived(
      source,
      bagId(7),
      AggregateRequest.countStarGroupedBy(structural(2))
    )
    assertEquals(
      grouped.messages.map(message =>
        (message.source.value, message.carriedKeys.map(keyLabel), message.requiresRegrouping)
      ),
      Vector((9, Vector.empty[String], false), (3, Vector("v2"), false))
    )
    assertEquals(grouped.resources.propagatedMessageAggregateTerms, BigInt(0))
    assertEquals(grouped.resources.propagatedMessageProjectionTerms, BigInt(2))

    // Without that key, bag 3 must still eliminate v2 before answering bag 7.
    val scalar = derived(source, bagId(7), AggregateRequest.globalCountStar)
    assertEquals(
      scalar.messages.map(message =>
        (message.source.value, message.requiresRegrouping)
      ),
      Vector((9, false), (3, true))
    )
    assertEquals(scalar.resources.propagatedMessageAggregateTerms, BigInt(1))
    assertEquals(scalar.resources.propagatedMessageProjectionTerms, BigInt(1))
  }

  test("message nesting depth follows the terminal path, not the message count") {
    assertEquals(derivedCountStar(syntheticChain(64)).maximumMessageNestingDepth, 64)
    assertEquals(derivedCountStar(syntheticChain(65)).maximumMessageNestingDepth, 65)
    val star = derivedCountStar(syntheticStar(2000))
    assertEquals(star.messages.size, 1999)
    assertEquals(star.maximumMessageNestingDepth, 1)
  }

  test("a 20,000-message live-key chain derives without materializing every subtree") {
    val source = syntheticChain(20000)
    val plan = derived(
      source,
      bagId(0),
      AggregateRequest.countStarGroupedBy(structural(20001))
    )

    assertEquals(plan.messages.size, 20000)
    assertEquals(plan.maximumMessageNestingDepth, 20000)
    assertEquals(plan.maximumCarriedKeys, 1)
    assert(
      plan.messages.forall(_.carriedKeys == Vector(structural(20001))),
      "the live key must cross every owner-to-terminal arc"
    )
  }

  // -------------------------------------------------------------------
  // Correctness against the independent brute-force oracle
  // -------------------------------------------------------------------

  test("one bag counts every duplicate and groups NULL payloads together") {
    val source = oneBagPlan()
    val request = AggregateRequest(
      Vector(value(0, 2)),
      Vector(LaneRequest(LaneKind.NonNullCount, column(0, 3)))
    )
    val rows = factorRows(
      4 -> Vector(
        row(2)(0 -> 1, 1 -> 1)(column(0, 2) -> some(7), column(0, 3) -> some(5)),
        row(3)(0 -> 1, 1 -> 1)(column(0, 2) -> None, column(0, 3) -> None),
        row(1)(0 -> 2, 1 -> 3)(column(0, 2) -> None, column(0, 3) -> some(9))
      )
    )

    assertAgrees(source, request, rows)
    val result = messageResult(source, request, rows)
    assertEquals(result.size, 2)
    assertEquals(Terminal.countStar(result(Vector(some(7)))), BigInt(2))
    assertEquals(Terminal.countStar(result(Vector(None))), BigInt(4))
    assertEquals(Terminal.countColumn(result(Vector(None)), 0), BigInt(1))
  }

  test("chain messages weight a distant measure by every other factor's frequency") {
    val source = chainPlan()
    val request = AggregateRequest(
      Vector(structural(0)),
      Vector(LaneRequest(LaneKind.ExactSum, column(2, 2)))
    )
    val rows = factorRows(
      7 -> Vector(row(2)(0 -> 0, 1 -> 1)(), row(1)(0 -> 9, 1 -> 2)()),
      3 -> Vector(row(3)(1 -> 1, 2 -> 5)(), row(4)(1 -> 2, 2 -> 8)()),
      9 -> Vector(
        row(5)(2 -> 5, 3 -> 7)(column(2, 2) -> Some(BigDecimal("1.5"))),
        row(2)(2 -> 8, 3 -> 6)(column(2, 2) -> None)
      )
    )

    assertAgrees(source, request, rows)
  }

  test("C4 compresses duplicate separator groups while retaining both group keys") {
    val source = c4Plan()
    val request = AggregateRequest.countStarGroupedBy(structural(0), structural(2))
    val rows = factorRows(
      40 -> Vector(
        row(2)(0 -> 0, 1 -> 1, 3 -> 9)(),
        row(3)(0 -> 8, 1 -> 1, 3 -> 9)(),
        row(5)(0 -> 2, 1 -> 2, 3 -> 8)()
      ),
      10 -> Vector(
        row(4)(1 -> 1, 2 -> 5, 3 -> 9)(),
        row(5)(1 -> 1, 2 -> 6, 3 -> 9)(),
        row(7)(1 -> 2, 2 -> 4, 3 -> 8)()
      )
    )

    assertAgrees(source, request, rows)
    assert(messageResult(source, request, rows).size > 1)
  }

  test("diamond and bow-tie topologies agree with the brute-force oracle") {
    assertAgrees(
      diamondPlan(),
      AggregateRequest(
        Vector(structural(1), structural(3)),
        Vector(LaneRequest(LaneKind.ExactSum, column(2, 1)))
      ),
      factorRows(
        1 -> Vector(
          row(2)(0 -> 1, 1 -> 5, 2 -> 3)(),
          row(1)(0 -> 1, 1 -> 6, 2 -> 3)(),
          row(4)(0 -> 2, 1 -> 5, 2 -> 4)()
        ),
        2 -> Vector(
          row(3)(0 -> 1, 2 -> 3, 3 -> 8)(column(2, 1) -> Some(BigDecimal("2.25"))),
          row(1)(0 -> 1, 2 -> 3, 3 -> 9)(column(2, 1) -> None),
          row(2)(0 -> 2, 2 -> 4, 3 -> 8)(column(2, 1) -> Some(BigDecimal(-7)))
        )
      )
    )

    assertAgrees(
      bowTiePlan(),
      AggregateRequest(
        Vector(structural(0), structural(4)),
        Vector(LaneRequest(LaneKind.NonNullCount, column(3, 1)))
      ),
      factorRows(
        1 -> Vector(
          row(2)(0 -> 1, 1 -> 2, 2 -> 3)(),
          row(3)(0 -> 1, 1 -> 4, 2 -> 3)(),
          row(1)(0 -> 5, 1 -> 2, 2 -> 6)()
        ),
        2 -> Vector(
          row(4)(2 -> 3, 3 -> 7, 4 -> 8)(column(3, 1) -> some(1)),
          row(2)(2 -> 3, 3 -> 7, 4 -> 9)(column(3, 1) -> None),
          row(1)(2 -> 6, 3 -> 7, 4 -> 8)(column(3, 1) -> some(4))
        )
      )
    )
  }

  test("group and measure supplied by the same occurrence stay exact") {
    val source = chainPlan()
    val request = AggregateRequest(
      Vector(structural(3)),
      Vector(LaneRequest(LaneKind.ExactSum, column(2, 2)))
    )
    val rows = factorRows(
      7 -> Vector(row(2)(0 -> 0, 1 -> 1)(), row(3)(0 -> 4, 1 -> 1)()),
      3 -> Vector(row(3)(1 -> 1, 2 -> 5)()),
      9 -> Vector(
        row(5)(2 -> 5, 3 -> 7)(column(2, 2) -> some(3)),
        row(2)(2 -> 5, 3 -> 7)(column(2, 2) -> some(4)),
        row(1)(2 -> 5, 3 -> 6)(column(2, 2) -> None)
      )
    )

    assertAgrees(source, request, rows)
  }

  test("ordinary separator equality rejects NULL-to-NULL matches on both sides") {
    assertAgrees(
      twoBagPlan(),
      AggregateRequest.countStarGroupedBy(structural(0)),
      factorRows(
        7 -> Vector(
          nullableRow(2)(0 -> some(10), 1 -> None)(),
          nullableRow(3)(0 -> some(11), 1 -> some(1))()
        ),
        3 -> Vector(
          nullableRow(5)(1 -> None, 2 -> some(20))(),
          nullableRow(7)(1 -> some(1), 2 -> some(21))()
        )
      )
    )
  }

  test("an empty factor and an empty result emit no group at all") {
    val source = chainPlan()
    val request = AggregateRequest.countStarGroupedBy(structural(0))
    assertAgrees(
      source,
      request,
      factorRows(
        7 -> Vector(row(2)(0 -> 0, 1 -> 1)()),
        3 -> Vector.empty[FactorRow],
        9 -> Vector(row(5)(2 -> 5, 3 -> 7)())
      ),
      allowEmpty = true
    )
    assertEquals(messageResult(source, request, emptyRows(source)).size, 0)
    assertEquals(bruteForce(source, request, emptyRows(source)).size, 0)
  }

  test("an empty separator is a valid scalar message and forms a Cartesian product") {
    val source = cartesianPlan()
    val request = AggregateRequest.countStarGroupedBy(structural(0), structural(1))
    val plan = derived(source, bagId(100), request)
    assertEquals(plan.messages.map(_.separator), Vector(Vector.empty[VariableId]))
    assertEquals(plan.messages.map(_.carriedKeys), Vector(Vector(structural(1))))
    assertAgrees(
      source,
      request,
      factorRows(
        100 -> Vector(row(2)(0 -> 1)(), row(3)(0 -> 2)()),
        200 -> Vector(row(7)(1 -> 10)(), row(11)(1 -> 20)())
      ),
      bagId(100)
    )
  }

  test("several groups share one child message and duplicate separator assignments") {
    assertAgrees(
      starPlan(),
      AggregateRequest.countStarGroupedBy(structural(1), structural(2)),
      factorRows(
        9 -> Vector(row(2)(0 -> 1)(), row(1)(0 -> 1)(), row(4)(0 -> 2)()),
        2 -> Vector(
          row(3)(0 -> 1, 2 -> 20)(),
          row(5)(0 -> 1, 2 -> 21)(),
          row(2)(0 -> 2, 2 -> 20)()
        ),
        5 -> Vector(
          row(7)(0 -> 1, 1 -> 10)(),
          row(2)(0 -> 1, 1 -> 10)(),
          row(3)(0 -> 2, 1 -> 11)()
        )
      ),
      bagId(9)
    )
  }

  test("NULL group payloads survive as their own group instead of being joined away") {
    assertAgrees(
      starPlan(),
      AggregateRequest(
        Vector(value(1, 5), structural(2)),
        Vector(LaneRequest(LaneKind.ExactSum, column(2, 6)))
      ),
      factorRows(
        9 -> Vector(row(2)(0 -> 1)(), row(1)(0 -> 2)()),
        2 -> Vector(
          row(3)(0 -> 1, 2 -> 20)(column(2, 6) -> Some(BigDecimal("-1.5"))),
          row(1)(0 -> 2, 2 -> 20)(column(2, 6) -> None)
        ),
        5 -> Vector(
          row(2)(0 -> 1, 1 -> 10)(column(1, 5) -> None),
          row(4)(0 -> 1, 1 -> 11)(column(1, 5) -> some(3)),
          row(1)(0 -> 2, 1 -> 12)(column(1, 5) -> None)
        )
      ),
      bagId(5)
    )
  }

  test("input order, child order, and terminal choice do not change the result") {
    val source = c4Plan()
    val request = AggregateRequest(
      Vector(structural(0), structural(2)),
      Vector(LaneRequest(LaneKind.NonNullCount, column(1, 2)))
    )
    val rows = factorRows(
      40 -> Vector(
        row(2)(0 -> 0, 1 -> 1, 3 -> 9)(),
        row(3)(0 -> 8, 1 -> 1, 3 -> 9)()
      ),
      10 -> Vector(
        row(4)(1 -> 1, 2 -> 5, 3 -> 9)(column(1, 2) -> some(2)),
        row(5)(1 -> 1, 2 -> 6, 3 -> 9)(column(1, 2) -> None)
      )
    )
    val reordered = source.copy(
      factors = source.factors.reverse,
      arcs = source.arcs.reverse
    )
    val baseline = plainResult(source, request, rows, bagId(40))

    assertEquals(plainResult(reordered, request, rows, bagId(40)), baseline)
    assertEquals(plainResult(source, request, rows, bagId(10)), baseline)
    assertEquals(bruteForce(source, request, rows), baseline)
  }

  test("a deep chain and a wide star stay exact end to end") {
    val chain = syntheticChain(8)
    val chainRows = (0 to 8).iterator.map { index =>
      bagId(index) -> Vector(
        row(2)(index -> 1, index + 1 -> 1)(),
        row(3)(index -> 1, index + 1 -> 2)()
      )
    }.toMap
    assertAgrees(
      chain,
      AggregateRequest.countStarGroupedBy(structural(0)),
      chainRows,
      bagId(0)
    )

    val star = syntheticStar(6)
    val starRows = (0 until 6).iterator
      .map(index => bagId(index) -> Vector(row(index + 1)(0 -> 1)()))
      .toMap
    assertAgrees(
      star,
      AggregateRequest.countStarGroupedBy(structural(0)),
      starRows,
      bagId(0)
    )
  }

  test("multiplicities far beyond signed 64 bits stay exact in every lane") {
    val source = twoBagPlan()
    val huge = BigInt(10).pow(25)
    val request = AggregateRequest(
      Vector(structural(0)),
      Vector(
        LaneRequest(LaneKind.NonNullCount, column(1, 3)),
        LaneRequest(LaneKind.ExactSum, column(1, 4))
      )
    )
    val rows = factorRows(
      7 -> Vector(bigRow(huge)(0 -> 10, 1 -> 1)()),
      3 -> Vector(
        bigRow(huge)(1 -> 1, 2 -> 20)(
          column(1, 3) -> some(1),
          column(1, 4) -> Some(BigDecimal("0.5"))
        )
      )
    )

    assertAgrees(source, request, rows)
    val group = messageResult(source, request, rows)(Vector(some(10)))
    assertEquals(Terminal.countStar(group), huge * huge)
    assertEquals(Terminal.countColumn(group, 0), huge * huge)
    assertEquals(
      Terminal.sumColumn(group, 1),
      TerminalValue.ExactDecimalValue(exactProduct(BigDecimal("0.5"), huge * huge))
    )
  }

  test("two distributed keys keep BIGINT representability local to each group") {
    val source = starPlan()
    val request =
      AggregateRequest.countStarGroupedBy(structural(1), structural(2))
    val root = bagId(9)
    val maximum = BigInt(Long.MaxValue)

    val independentlyRepresentable = factorRows(
      9 -> Vector(row(1)(0 -> 1)()),
      2 -> Vector(row(1)(0 -> 1, 2 -> 20)()),
      5 -> Vector(
        bigRow(maximum)(0 -> 1, 1 -> 10)(),
        bigRow(maximum)(0 -> 1, 1 -> 11)()
      )
    )
    assertAgrees(source, request, independentlyRepresentable, root)
    val representable = messageResult(
      source,
      request,
      independentlyRepresentable,
      root
    ).map { case (key, state) => key -> Terminal.countStar(state) }
    assertEquals(
      representable,
      Map(
        Vector(some(10), some(20)) -> maximum,
        Vector(some(11), some(20)) -> maximum
      )
    )
    assert(representable.values.forall(_ <= maximum))
    assert(representable.values.sum > maximum)

    val oneOverflowingGroup = factorRows(
      9 -> Vector(row(1)(0 -> 1)()),
      2 -> Vector(row(1)(0 -> 1, 2 -> 20)()),
      5 -> Vector(
        bigRow(maximum)(0 -> 1, 1 -> 10)(),
        row(1)(0 -> 1, 1 -> 10)(),
        bigRow(maximum)(0 -> 1, 1 -> 11)()
      )
    )
    assertAgrees(source, request, oneOverflowingGroup, root)
    val overflowBoundary = messageResult(
      source,
      request,
      oneOverflowingGroup,
      root
    ).map { case (key, state) => key -> Terminal.countStar(state) }
    assertEquals(
      overflowBoundary,
      Map(
        Vector(some(10), some(20)) -> (maximum + 1),
        Vector(some(11), some(20)) -> maximum
      )
    )
  }

  // -------------------------------------------------------------------
  // Fail-closed behaviour
  // -------------------------------------------------------------------

  test("malformed requests, ownership, and lineage fail closed") {
    val source = chainPlan()
    val root = bagId(7)

    assertEquals(
      errorCodes(PostgreSqlAggregatePropagation.derive(source, root, null)),
      Set[ErrorCode](ErrorCode.MissingValue)
    )
    assertEquals(
      errorCodes(
        PostgreSqlAggregatePropagation
          .derive(source, null, AggregateRequest.globalCountStar)
      ),
      Set[ErrorCode](ErrorCode.MissingValue)
    )

    val expectations = Vector(
      (
        PostgreSqlAggregatePropagation
          .derive(source, bagId(99), AggregateRequest.globalCountStar),
        ErrorCode.UnknownFinalizationRoot
      ),
      (
        PostgreSqlAggregatePropagation.derive(
          source,
          root,
          AggregateRequest.countStarGroupedBy(structural(0), structural(0))
        ),
        ErrorCode.DuplicateGroupKey
      ),
      (
        PostgreSqlAggregatePropagation.derive(
          source,
          root,
          AggregateRequest(
            Vector.empty,
            Vector(
              LaneRequest(LaneKind.ExactSum, column(0, 4)),
              LaneRequest(LaneKind.ExactSum, column(0, 4))
            )
          )
        ),
        ErrorCode.DuplicateLaneMeasure
      ),
      (
        PostgreSqlAggregatePropagation
          .derive(source, root, AggregateRequest.countStarGroupedBy(structural(99))),
        ErrorCode.UnknownVariable
      ),
      (
        PostgreSqlAggregatePropagation
          .derive(source, root, AggregateRequest.countStarGroupedBy(value(99, 0))),
        ErrorCode.UnknownOccurrence
      ),
      (
        PostgreSqlAggregatePropagation.derive(
          source.copy(ownership = Vector.empty),
          root,
          AggregateRequest.countStarGroupedBy(value(0, 4))
        ),
        ErrorCode.UnownedOccurrence
      ),
      (
        PostgreSqlAggregatePropagation.derive(
          source.copy(
            ownership = source.ownership.map(entry =>
              EdgeOwnership(entry.edgeId, bagId(98), entry.ownerDepth)
            )
          ),
          root,
          AggregateRequest.countStarGroupedBy(value(0, 4))
        ),
        ErrorCode.UnknownOwnerBag
      )
    )

    expectations.foreach { case (result, expected) =>
      assert(
        errorCodes(result).contains(expected),
        s"expected ${expected.value} but found ${errorCodes(result).map(_.value)}"
      )
    }
  }

  test("a violated running intersection is rejected whatever the request asks for") {
    val source = brokenRunningIntersectionPlan()

    // Variable 0 sits in bags 1 and 3 while the bag joining them omits it, so
    // no message schema can carry it and its equality would never be enforced.
    // The rejection must therefore not depend on the request mentioning it.
    val requests = Vector(
      ("group by the disconnected variable", AggregateRequest.countStarGroupedBy(structural(0))),
      ("group by a well-connected variable", AggregateRequest.countStarGroupedBy(structural(1))),
      ("global count star", AggregateRequest.globalCountStar)
    )

    source.factors.map(_.bagId).foreach { root =>
      requests.foreach { case (label, request) =>
        assertEquals(
          errorCodes(PostgreSqlAggregatePropagation.derive(source, root, request)),
          Set[ErrorCode](ErrorCode.NonDecomposition),
          s"$label at terminal ${root.value} must fail closed"
        )
      }
    }
  }

  test("every fixture the model accepts does satisfy running intersection") {
    everyFixture.foreach { case (label, source, request) =>
      source.factors.map(_.bagId).foreach { root =>
        PostgreSqlAggregatePropagation.derive(source, root, request) match {
          case Left(problems) =>
            assert(
              !problems.exists(_.code == ErrorCode.NonDecomposition),
              s"$label was rejected as a non-decomposition at terminal ${root.value}"
            )
          case Right(plan) =>
            // Every variable shared by two bags must appear in the separator of
            // each arc on the path between them, otherwise the message program
            // drops a join condition.
            val holdersOf = source.factors
              .flatMap(factor => factor.vertices.map(_ -> factor.bagId))
              .groupBy(_._1)
              .map { case (variable, pairs) => variable -> pairs.map(_._2).toSet }
            holdersOf.foreach { case (variable, holders) =>
              if (holders.size > 1) {
                val tops = holders.filterNot(bag =>
                  plan.bagById(bag).parent.exists(holders.contains)
                )
                assertEquals(
                  tops.size,
                  1,
                  s"$label variable ${variable.value} is disconnected at terminal ${root.value}"
                )
              }
            }
        }
      }
    }
  }

  test("a structurally invalid source is rejected before any aggregate binding") {
    val source = chainPlan()
    assertEquals(
      errorCodes(
        PostgreSqlAggregatePropagation.derive(
          source.copy(arcs = source.arcs.dropRight(1)),
          bagId(7),
          AggregateRequest.globalCountStar
        )
      ),
      Set[ErrorCode](ErrorCode.StructuralSource)
    )
  }

  test("corrupted candidate plans are rejected field by field") {
    val source = starPlan()
    val request = AggregateRequest.countStarGroupedBy(structural(1), structural(2))
    val root = bagId(9)
    val canonicalPlan = derived(source, root, request)

    assertEquals(
      PostgreSqlAggregatePropagation.validate(source, root, request, canonicalPlan),
      Right(canonicalPlan)
    )
    assertEquals(
      errorCodes(PostgreSqlAggregatePropagation.validate(source, root, request, null)),
      Set[ErrorCode](ErrorCode.MissingValue)
    )

    val corruptions = Vector(
      canonicalPlan.copy(postorder = canonicalPlan.postorder.reverse),
      canonicalPlan.copy(
        messages =
          canonicalPlan.messages.map(_.copy(inputMessageOrdinals = Vector(0)))
      ),
      canonicalPlan.copy(
        messages = canonicalPlan.messages.map(message =>
          message.copy(
            separator = message.separator ++ message.carriedKeys.collect {
              case AggregateKey.Structural(variable) => variable
            }
          )
        )
      ),
      canonicalPlan.copy(messages = canonicalPlan.messages.map(_.copy(carriedKeys = Vector.empty))),
      canonicalPlan.copy(keyBindings = canonicalPlan.keyBindings.reverse),
      canonicalPlan.copy(
        finalization = canonicalPlan.finalization
          .copy(groupKeys = canonicalPlan.finalization.groupKeys.reverse)
      ),
      canonicalPlan.copy(
        resources = canonicalPlan.resources.copy(terminalGroupingTerms = BigInt(99))
      )
    )

    corruptions.foreach(candidate =>
      assertEquals(
        errorCodes(
          PostgreSqlAggregatePropagation.validate(source, root, request, candidate)
        ),
        Set[ErrorCode](ErrorCode.CandidateMismatch)
      )
    )
  }

  // -------------------------------------------------------------------
  // Independent brute-force oracle
  // -------------------------------------------------------------------

  /**
    * Materialises the natural join of every bag factor under ordinary
    * NULL-rejecting equality and aggregates the witnesses with its own
    * arithmetic. This consults only the supplied plan's bag vertices and the
    * request; it neither derives nor executes a message program, and it does
    * not use the model's semiring operations.
    */
  private def bruteForce(
      source: BagFactorPlan,
      request: AggregateRequest,
      rows: FactorRows
  ): Map[GroupTuple, GroupResult] = {
    val start =
      Vector((Map.empty[VariableId, Cell], Map.empty[ColumnId, Cell], BigInt(1)))
    val witnesses = source.factors.sortBy(_.bagId).foldLeft(start) {
      (prefix, factor) =>
        for {
          (values, payload, multiplicity) <- prefix
          candidate <- rows(factor.bagId)
          merged <- mergeOnSqlEquality(values, candidate.values)
        } yield {
          candidate.payload.keysIterator.foreach(columnId =>
            assert(!payload.contains(columnId), s"payload $columnId is not unique")
          )
          (merged, payload ++ candidate.payload, multiplicity * candidate.multiplicity)
        }
    }

    witnesses.foldLeft(Map.empty[GroupTuple, GroupResult]) {
      case (grouped, (values, payload, multiplicity)) =>
        val key = request.groupKeys.map {
          case AggregateKey.Structural(variable)    => values(variable)
          case AggregateKey.OccurrenceValue(column) => payload.getOrElse(column, None)
        }
        val contribution = request.lanes.map { lane =>
          payload.getOrElse(lane.measure, None) match {
            case None          => (BigInt(0), BigDecimal(0))
            case Some(measure) => (multiplicity, exactProduct(measure, multiplicity))
          }
        }
        val previous =
          grouped.getOrElse(key, GroupResult(BigInt(0), request.lanes.map(_ => (BigInt(0), BigDecimal(0)))))
        grouped.updated(
          key,
          GroupResult(
            previous.witnesses + multiplicity,
            previous.lanes.zip(contribution).map {
              case ((leftCount, leftSum), (rightCount, rightSum)) =>
                (leftCount + rightCount, exactSum(leftSum, rightSum))
            }
          )
        )
    }
  }

  private def mergeOnSqlEquality(
      left: Map[VariableId, Cell],
      right: Map[VariableId, Cell]
  ): Option[Map[VariableId, Cell]] = {
    val shared = left.keySet intersect right.keySet
    val matches = shared.forall(variableId =>
      (left(variableId), right(variableId)) match {
        case (Some(leftValue), Some(rightValue)) => leftValue == rightValue
        case _                                   => false
      }
    )
    if (matches) Some(left ++ right) else None
  }

  /** Exact decimal arithmetic, independent of the model's own helper. */
  private def exactProduct(left: BigDecimal, right: BigInt): BigDecimal =
    new scala.math.BigDecimal(
      left.bigDecimal.multiply(new java.math.BigDecimal(right.bigInteger)),
      java.math.MathContext.UNLIMITED
    )

  private def exactSum(left: BigDecimal, right: BigDecimal): BigDecimal =
    new scala.math.BigDecimal(
      left.bigDecimal.add(right.bigDecimal),
      java.math.MathContext.UNLIMITED
    )

  // -------------------------------------------------------------------
  // Message-program executor (consumes the derived plan, never re-derives)
  // -------------------------------------------------------------------

  private def messageResult(
      source: BagFactorPlan,
      request: AggregateRequest,
      rows: FactorRows,
      finalizationRoot: BagId = null
  ): Map[GroupTuple, Annotation] = {
    val root =
      if (finalizationRoot ne null) finalizationRoot else defaultRoot(source, request)
    val plan = derived(source, root, request)
    validateRows(plan, rows)
    val laneCount = plan.laneCount

    val tables = Array.fill(plan.messages.size)(Vector.empty[(Binding, Annotation)])
    plan.messages.zipWithIndex.foreach { case (message, index) =>
      assertEquals(message.ordinal, index)
      var current = localRows(plan, plan.bagById(message.source), rows, laneCount)
      message.inputMessageOrdinals.foreach { ordinal =>
        current = joinOn(current, tables(ordinal), plan.messages(ordinal).separator)
      }
      val schema = message.separator.map(Term.Var) ++ message.carriedKeys.map(term)
      tables(message.ordinal) = groupBy(current, schema, laneCount)
    }

    var terminal = localRows(plan, plan.bagById(plan.root), rows, laneCount)
    plan.finalization.inputMessageOrdinals.foreach { ordinal =>
      terminal = joinOn(terminal, tables(ordinal), plan.messages(ordinal).separator)
    }
    val terminalSchema = plan.finalization.groupKeys.map(term)
    groupBy(terminal, terminalSchema, laneCount).iterator
      .filter { case (_, annotation) => Terminal.isEmitted(annotation) }
      .map { case (binding, annotation) => terminalSchema.map(binding) -> annotation }
      .toMap
  }

  private def plainResult(
      source: BagFactorPlan,
      request: AggregateRequest,
      rows: FactorRows,
      finalizationRoot: BagId = null
  ): Map[GroupTuple, GroupResult] =
    messageResult(source, request, rows, finalizationRoot).map {
      case (key, annotation) =>
        key -> GroupResult(
          annotation.witnesses,
          annotation.lanes.map(lane => (lane.nonNullWitnesses, lane.sum))
        )
    }

  /**
    * The weighted local factor: each measure becomes its lane before any
    * grouping that could merge distinct measure values.
    */
  private def localRows(
      plan: Plan,
      bag: PostgreSqlAggregatePropagation.Bag,
      rows: FactorRows,
      laneCount: Int
  ): Vector[(Binding, Annotation)] = {
    val schema =
      bag.localGroupingVariables.map(Term.Var) ++ bag.ownedValueKeyColumns.map(Term.Col)
    val annotated = rows(bag.bagId).map { candidate =>
      val binding: Binding = schema.iterator.map {
        case position @ Term.Var(variable) => position -> candidate.values(variable)
        case position @ Term.Col(columnId) =>
          position -> candidate.payload.getOrElse(columnId, None)
      }.toMap
      val lanes = (0 until laneCount).iterator.map { ordinal =>
        if (!bag.ownedLanes.contains(ordinal)) MeasureLane.zero
        else
          candidate.payload.getOrElse(plan.laneBindings(ordinal).measure, None) match {
            case None => MeasureLane.zero
            case Some(measure) =>
              MeasureLane(
                candidate.multiplicity,
                exactProduct(measure, candidate.multiplicity)
              )
          }
      }.toVector
      (binding, Annotation(support = true, candidate.multiplicity, lanes))
    }
    groupBy(annotated, schema, laneCount)
  }

  /** Ordinary NULL-rejecting equality on structural separator variables only. */
  private def joinOn(
      left: Vector[(Binding, Annotation)],
      right: Vector[(Binding, Annotation)],
      separator: Vector[VariableId]
  ): Vector[(Binding, Annotation)] =
    for {
      (leftBinding, leftAnnotation) <- left
      (rightBinding, rightAnnotation) <- right
      if separator.forall { variable =>
        (leftBinding(Term.Var(variable)), rightBinding(Term.Var(variable))) match {
          case (Some(leftValue), Some(rightValue)) => leftValue == rightValue
          case _                                   => false
        }
      }
    } yield (
      leftBinding ++ rightBinding,
      Annotation.join(leftAnnotation, rightAnnotation)
    )

  /** SQL GROUP BY equivalence: NULL forms and shares its own group. */
  private def groupBy(
      rows: Vector[(Binding, Annotation)],
      schema: Vector[Term],
      laneCount: Int
  ): Vector[(Binding, Annotation)] =
    rows
      .foldLeft(Map.empty[GroupTuple, (Binding, Annotation)]) {
        case (grouped, (binding, annotation)) =>
          val key = schema.map(binding)
          val restricted: Binding =
            schema.iterator.map(position => position -> binding(position)).toMap
          val merged = grouped.get(key) match {
            case None                => annotation
            case Some((_, existing)) => Annotation.merge(existing, annotation)
          }
          grouped.updated(key, (restricted, merged))
      }
      .valuesIterator
      .toVector

  /**
    * Compares the executed message program against the independent oracle. The
    * oracle result must be non-empty unless the case is explicitly about an
    * empty result, so that no comparison can pass vacuously.
    */
  private def assertAgrees(
      source: BagFactorPlan,
      request: AggregateRequest,
      rows: FactorRows,
      finalizationRoot: BagId = null,
      allowEmpty: Boolean = false
  ): Unit = {
    val expected = bruteForce(source, request, rows)
    assert(
      allowEmpty || expected.nonEmpty,
      "the brute-force oracle produced no group, so this comparison is vacuous"
    )
    assertEquals(plainResult(source, request, rows, finalizationRoot), expected)
  }

  private def validateRows(plan: Plan, rows: FactorRows): Unit = {
    assertEquals(rows.keySet, plan.bagById.keySet)
    plan.bags.foreach { bag =>
      rows(bag.bagId).foreach { candidate =>
        assertEquals(candidate.values.keySet, bag.vertices.toSet)
        assert(candidate.multiplicity >= 1)
      }
    }
  }

  // -------------------------------------------------------------------
  // Fixtures
  // -------------------------------------------------------------------

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

  private def starPlan(): BagFactorPlan = {
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
    val hypergraph = graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3), Seq(3, 0)))
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

  /** A four-cycle with a chord, so the two bags share a two-variable separator. */
  private def diamondPlan(): BagFactorPlan = {
    val hypergraph =
      graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3), Seq(3, 0), Seq(0, 2)))
    val input = candidateInput(hypergraph, 2, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 1,
      bags = Vector(
        coveredBag(1, Vector(0, 1, 2), Vector(0, 1)),
        coveredBag(2, Vector(0, 2, 3), Vector(2, 3))
      ),
      edges = Vector(treeEdge(1, 2))
    )
    certificate(input, decomposition)
    planned(input, decomposition)
  }

  /** Two triangles sharing exactly one variable. */
  private def bowTiePlan(): BagFactorPlan = {
    val hypergraph = graph(
      Seq(Seq(0, 1), Seq(1, 2), Seq(0, 2), Seq(2, 3), Seq(3, 4), Seq(2, 4))
    )
    val input = candidateInput(hypergraph, 2, requireConnectedCover = true)
    val decomposition = coveredDecomposition(
      root = 1,
      bags = Vector(
        coveredBag(1, Vector(0, 1, 2), Vector(0, 1)),
        coveredBag(2, Vector(2, 3, 4), Vector(3, 4))
      ),
      edges = Vector(treeEdge(1, 2))
    )
    certificate(input, decomposition)
    planned(input, decomposition)
  }

  /**
    * `CandidateSearchInput` rejects disconnected hypergraphs, so the public
    * planner cannot originate an empty-separator tree. Only the structural
    * fields the pure model consumes are replaced here, starting from a genuine
    * planner-produced plan.
    */
  private def cartesianPlan(): BagFactorPlan = {
    val root = bagId(100)
    val child = bagId(200)
    oneBagPlan().copy(
      root = root,
      factors = Vector(
        BagFactor(
          root,
          Vector(variable(0)),
          Vector.empty,
          Vector.empty,
          0,
          None,
          Vector(child)
        ),
        BagFactor(
          child,
          Vector(variable(1)),
          Vector.empty,
          Vector.empty,
          1,
          Some(root),
          Vector.empty
        )
      ),
      arcs = Vector(OrientedBagArc(root, child, Vector.empty))
    )
  }

  /**
    * Structurally a valid tree whose separators are exact bag intersections,
    * but variable 0 occurs in two bags while the bag connecting them omits it.
    * No aggregate key may be introduced for such a variable.
    */
  private def brokenRunningIntersectionPlan(): BagFactorPlan =
    chainPlan().copy(
      root = bagId(1),
      factors = Vector(
        BagFactor(
          bagId(1),
          Vector(variable(0), variable(1)),
          Vector.empty,
          Vector.empty,
          0,
          None,
          Vector(bagId(2))
        ),
        BagFactor(
          bagId(2),
          Vector(variable(1), variable(2)),
          Vector.empty,
          Vector.empty,
          1,
          Some(bagId(1)),
          Vector(bagId(3))
        ),
        BagFactor(
          bagId(3),
          Vector(variable(0), variable(2)),
          Vector.empty,
          Vector.empty,
          2,
          Some(bagId(2)),
          Vector.empty
        )
      ),
      arcs = Vector(
        OrientedBagArc(bagId(1), bagId(2), Vector(variable(1))),
        OrientedBagArc(bagId(2), bagId(3), Vector(variable(2)))
      )
    )

  private def syntheticChain(messageDepth: Int): BagFactorPlan = {
    val factors = (0 to messageDepth).iterator.map { index =>
      BagFactor(
        bagId(index),
        Vector(variable(index), variable(index + 1)),
        Vector.empty,
        Vector.empty,
        depth = index,
        parent = if (index == 0) None else Some(bagId(index - 1)),
        children =
          if (index == messageDepth) Vector.empty else Vector(bagId(index + 1))
      )
    }.toVector
    val arcs = (0 until messageDepth).iterator
      .map(index =>
        OrientedBagArc(bagId(index), bagId(index + 1), Vector(variable(index + 1)))
      )
      .toVector
    oneBagPlan().copy(root = bagId(0), factors = factors, arcs = arcs)
  }

  private def syntheticStar(bagCount: Int): BagFactorPlan = {
    val children = (1 until bagCount).iterator.map(bagId).toVector
    val factors = BagFactor(
      bagId(0),
      Vector(variable(0)),
      Vector.empty,
      Vector.empty,
      depth = 0,
      parent = None,
      children = children
    ) +: children.map(child =>
      BagFactor(
        child,
        Vector(variable(0)),
        Vector.empty,
        Vector.empty,
        depth = 1,
        parent = Some(bagId(0)),
        children = Vector.empty
      )
    )
    val arcs =
      children.map(child => OrientedBagArc(bagId(0), child, Vector(variable(0))))
    oneBagPlan().copy(root = bagId(0), factors = factors, arcs = arcs)
  }

  private def everyFixture: Vector[(String, BagFactorPlan, AggregateRequest)] =
    Vector(
      ("one bag", oneBagPlan(), AggregateRequest.countStarGroupedBy(structural(0))),
      (
        "two bag",
        twoBagPlan(),
        AggregateRequest.countStarGroupedBy(structural(0), structural(2))
      ),
      (
        "chain",
        chainPlan(),
        AggregateRequest.countStarGroupedBy(structural(0), structural(3))
      ),
      (
        "star",
        starPlan(),
        AggregateRequest.countStarGroupedBy(structural(1), structural(2))
      ),
      ("c4", c4Plan(), AggregateRequest.countStarGroupedBy(structural(0), structural(2))),
      (
        "diamond",
        diamondPlan(),
        AggregateRequest.countStarGroupedBy(structural(1), structural(3))
      ),
      (
        "bow tie",
        bowTiePlan(),
        AggregateRequest.countStarGroupedBy(structural(0), structural(4))
      ),
      (
        "cartesian",
        cartesianPlan(),
        AggregateRequest.countStarGroupedBy(structural(0), structural(1))
      )
    )

  // -------------------------------------------------------------------
  // Helpers
  // -------------------------------------------------------------------

  private def derived(
      source: BagFactorPlan,
      finalizationRoot: BagId,
      request: AggregateRequest
  ): Plan =
    PostgreSqlAggregatePropagation
      .derive(source, finalizationRoot, request)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def derivedCountStar(source: BagFactorPlan): Plan =
    derived(source, source.root, AggregateRequest.globalCountStar)

  private def defaultRoot(source: BagFactorPlan, request: AggregateRequest): BagId =
    PostgreSqlAggregatePropagation
      .defaultFinalizationRoot(source, request)
      .fold(problems => fail(problems.map(_.message).mkString("; ")), identity)

  private def structural(id: Int): AggregateKey =
    AggregateKey.Structural(variable(id))

  private def value(occurrenceId: Int, ordinal: Int): AggregateKey =
    AggregateKey.OccurrenceValue(column(occurrenceId, ordinal))

  private def column(occurrenceId: Int, ordinal: Int): ColumnId =
    ColumnId
      .from(occurrence(occurrenceId), ordinal)
      .fold(problem => fail(problem.message), identity)

  private def keyLabel(key: AggregateKey): String = key match {
    case AggregateKey.Structural(variableId)  => s"v${variableId.value}"
    case AggregateKey.OccurrenceValue(columnId) => s"c$columnId"
  }

  private def term(key: AggregateKey): Term = key match {
    case AggregateKey.Structural(variableId)    => Term.Var(variableId)
    case AggregateKey.OccurrenceValue(columnId) => Term.Col(columnId)
  }

  private def annotation(witnesses: Int, lanes: Vector[(Int, Int)]): Annotation =
    Annotation(
      support = witnesses > 0,
      BigInt(witnesses),
      lanes.map { case (nonNull, sum) => MeasureLane(BigInt(nonNull), BigDecimal(sum)) }
    )

  private def some(value: Int): Cell = Some(BigDecimal(value))

  private def factorRows(entries: (Int, Vector[FactorRow])*): FactorRows =
    entries.iterator.map { case (numericBagId, rows) =>
      bagId(numericBagId) -> rows
    }.toMap

  private def emptyRows(source: BagFactorPlan): FactorRows =
    source.factors.iterator.map(_.bagId -> Vector.empty[FactorRow]).toMap

  private def row(
      multiplicity: Int
  )(values: (Int, Int)*)(payload: (ColumnId, Cell)*): FactorRow =
    bigRow(BigInt(multiplicity))(values: _*)(payload: _*)

  private def bigRow(
      multiplicity: BigInt
  )(values: (Int, Int)*)(payload: (ColumnId, Cell)*): FactorRow =
    nullableBigRow(multiplicity)(values.map { case (id, cell) =>
      id -> some(cell)
    }: _*)(payload: _*)

  private def nullableRow(
      multiplicity: Int
  )(values: (Int, Cell)*)(payload: (ColumnId, Cell)*): FactorRow =
    nullableBigRow(BigInt(multiplicity))(values: _*)(payload: _*)

  private def nullableBigRow(
      multiplicity: BigInt
  )(values: (Int, Cell)*)(payload: (ColumnId, Cell)*): FactorRow =
    new FactorRow(
      values.iterator.map { case (id, cell) => variable(id) -> cell }.toMap,
      payload.iterator.toMap,
      multiplicity
    )

  private def errorCodes[A](
      result: Either[Vector[PostgreSqlAggregatePropagation.Error], A]
  ): Set[ErrorCode] =
    result.fold(_.iterator.map(_.code).toSet, _ => Set.empty)
}
