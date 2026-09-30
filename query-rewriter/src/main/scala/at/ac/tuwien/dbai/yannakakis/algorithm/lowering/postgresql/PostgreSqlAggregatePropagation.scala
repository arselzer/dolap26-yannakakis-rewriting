package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlan
import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  ColumnId,
  EdgeId,
  RelationOccurrenceId,
  VariableId
}

import scala.collection.mutable

/**
  * U0 of `docs/journal-extension/unguarded-aggregate-plan.md`: a SQL-free
  * annotation algebra for *unguarded* aggregation, where a grouping key and an
  * aggregate argument need not be supplied by the same base relation
  * occurrence.
  *
  * This object contains no SQL, no selector, and no serialization. It is a
  * pure model plus its derivation and a candidate validator. Nothing here
  * claims that every lane is lowerable to PostgreSQL today; see
  * [[LaneKind.loweringStatus]], which records the still-unproved signed-`SUM`
  * transition/carrier obligation as data rather than as prose.
  *
  * ==Semantic contract==
  *
  * A [[BagFactorPlan]] supplies one multiset (bag) factor per bag, each having
  * already applied every predicate assigned to that bag's factor. The modelled
  * source result is the natural join of those bags under ordinary
  * NULL-rejecting equality on shared variables, retaining multiplicity. For
  * one witness `t` of that join with multiplicity `m(t)`:
  *
  *   - `COUNT(*)` per group is `sum m(t)`;
  *   - `COUNT(x)` per group is `sum m(t)` over witnesses with `x(t)` not NULL;
  *   - `SUM(x)` per group is `sum m(t) * x(t)` over the same witnesses, and is
  *     SQL NULL when no such witness exists;
  *   - `AVG(x)` is the exact ratio of those two lanes.
  *
  * ==Annotation algebra==
  *
  * For one boundary-key assignment an [[Annotation]] carries an idempotent
  * support flag, the exact witness count `n`, and one [[MeasureLane]] per
  * aggregate call holding that call's non-NULL witness count `c` and exact sum
  * `s`. [[Annotation.join]] combines independent components and
  * [[Annotation.merge]] eliminates a variable:
  *
  * {{{
  * join:  n = product(n_i)
  *        c = sum over i of c_i * product(n_k for k != i)
  *        s = sum over i of s_i * product(n_k for k != i)
  * merge: n = sum(n_i), c = sum(c_i), s = sum(s_i)
  * }}}
  *
  * `(n, s)` is the dual-number (expectation) semiring `N[e]/(e * e)`, extended
  * to one independent `e_j` per lane. Because `join` is the semiring product
  * and `merge` its sum, evaluating the bag join bottom-up through the message
  * program is a semiring homomorphism of the source bag join, indexed by
  * separator plus live grouping values. When a lane's measure belongs to
  * exactly one component -- the only shape the staged language envelope admits
  * -- every other component contributes `c_i = s_i = 0` and the general rule
  * specialises to `c = c_k * product(n_i for i != k)`.
  *
  * The support flag is deliberately *not* derived from `n > 0`. It is
  * mathematically equivalent under bag semantics (every stored tuple has
  * multiplicity at least one), but a physical lowering must be able to decide
  * group existence without first evaluating a total multiplicity that may be
  * arbitrarily larger than any admitted group's result. That is the U3
  * group-support versus weighted-lane split, modelled here so the property can
  * be proved before any SQL exists.
  *
  * ==NULL contract==
  *
  * Two different equalities coexist and are never interchanged:
  *
  *   - message joins use the ordinary NULL-rejecting `=` on structural
  *     separator variables only; and
  *   - carrying and grouping a live aggregate key uses SQL `GROUP BY`
  *     equivalence, under which NULL forms its own group.
  *
  * A live key is therefore projected and grouped, never turned into a join
  * predicate. [[Plan]] derivation proves the enabling lemma (see
  * [[ErrorCode.CarriedKeyInSeparator]]): a carried key never appears in the
  * separator of any arc it crosses, so no key ever becomes a join predicate by
  * accident and a NULL payload group value survives to the terminal.
  *
  * ==Ownership and liveness==
  *
  * Each live key and each lane measure is bound to exactly one owner bag.
  *   - A structural key is introduced at its unique deterministic *highest*
  *     bag in the terminal orientation. Bags containing a variable form a
  *     connected subtree, so that bag exists and is unique.
  *   - An occurrence-owned value key or lane measure is introduced at the bag
  *     that owns its relation occurrence's hyperedge. That root-nearest
  *     ownership comes from the supplied plan and is deliberately unchanged
  *     here; only the terminal annotation orientation is re-rooted.
  *
  * A key introduced at owner `o` is live on every arc from `o` up to the
  * terminal and is consumed only by the terminal grouping. A lane measure is
  * live only until its owner converts it into a lane, which happens before any
  * grouping that could merge distinct measure values.
  */
object PostgreSqlAggregatePropagation {

  /** A live grouping key, bound to exact lineage rather than to a name. */
  sealed trait AggregateKey extends Product with Serializable

  object AggregateKey {

    /**
      * A hypergraph join variable. It is carried and grouped structurally, and
      * it is the only key shape the first production slice (U1) admits.
      */
    final case class Structural(variable: VariableId) extends AggregateKey

    /**
      * An occurrence-owned payload column that is not a hypergraph variable,
      * such as a `nation.name` group label. It is identified by exact column
      * lineage precisely so that it is never mistaken for a join variable when
      * two message schemas happen to mention it (U2).
      */
    final case class OccurrenceValue(column: ColumnId) extends AggregateKey

    implicit val ordering: Ordering[AggregateKey] =
      new Ordering[AggregateKey] {
        override def compare(left: AggregateKey, right: AggregateKey): Int =
          (left, right) match {
            case (Structural(a), Structural(b)) =>
              VariableId.ordering.compare(a, b)
            case (OccurrenceValue(a), OccurrenceValue(b)) =>
              ColumnId.ordering.compare(a, b)
            case (Structural(_), OccurrenceValue(_)) => -1
            case (OccurrenceValue(_), Structural(_)) => 1
          }
      }
  }

  /**
    * Which terminal calls a lane may serve, and whether that lane has an
    * executable PostgreSQL carrier proof yet.
    */
  sealed abstract class LaneKind(
      val code: String,
      val loweringStatus: LaneLoweringStatus
  ) extends Product
      with Serializable

  object LaneKind {

    /**
      * Serves `COUNT(x)` in the pure algebra. Although every partial `c` is a
      * non-negative count, its join rule also multiplies by the total witness
      * counts `n` of off-path components. Those `n` values need not be bounded
      * by the final `COUNT(x)`: every measure value can be NULL while an
      * arbitrarily large witness product still exists. A PostgreSQL lowering
      * therefore needs a separately reduced `x IS NOT NULL` weighted lane plus
      * an idempotent group-support lane (or another proved transition-safe
      * carrier); the U0 message program alone is not yet lowerable.
      */
    case object NonNullCount
        extends LaneKind(
          "non-null-count",
          LaneLoweringStatus.BlockedPendingSupportSplitProof
        )

    /**
      * Serves `SUM(x)` and, with its companion count, `AVG(x)`. Signed partial
      * sums can cancel, so a partial `s` is *not* bounded by its final group's
      * magnitude and the `COUNT` extendibility argument does not carry over.
      * This stays blocked until the plan's transition/type/overflow gate is
      * discharged; the model computes it exactly so the obligation can be
      * tested, not so it can be emitted.
      */
    case object ExactSum
        extends LaneKind(
          "exact-sum",
          LaneLoweringStatus.BlockedPendingCarrierProof
        )

    val all: Vector[LaneKind] = Vector(NonNullCount, ExactSum)
  }

  sealed abstract class LaneLoweringStatus(val code: String)
      extends Product
      with Serializable

  object LaneLoweringStatus {
    case object Lowerable extends LaneLoweringStatus("lowerable")
    case object BlockedPendingSupportSplitProof
        extends LaneLoweringStatus("blocked-pending-support-split-proof")
    case object BlockedPendingCarrierProof
        extends LaneLoweringStatus("blocked-pending-carrier-proof")
  }

  /** One requested aggregate lane over one occurrence-local measure column. */
  final case class LaneRequest(kind: LaneKind, measure: ColumnId)
      extends Serializable

  /**
    * The aggregate shape to derive a message program for.
    *
    * `groupKeys` is in output order and may be empty, which denotes a global
    * aggregate. `lanes` may be empty, which denotes `COUNT(*)` only.
    */
  final case class AggregateRequest(
      groupKeys: Vector[AggregateKey],
      lanes: Vector[LaneRequest]
  ) extends Serializable

  object AggregateRequest {
    val globalCountStar: AggregateRequest =
      AggregateRequest(Vector.empty, Vector.empty)

    def countStarGroupedBy(keys: AggregateKey*): AggregateRequest =
      AggregateRequest(keys.toVector, Vector.empty)
  }

  // ---------------------------------------------------------------------
  // Annotation algebra
  // ---------------------------------------------------------------------

  /**
    * One aggregate call's exact state: how many witnesses had a non-NULL
    * measure, and the exact weighted sum of that measure over them.
    *
    * `AVG` is represented by this pair rather than by a quotient so that no
    * rounding or PostgreSQL scale decision is baked into the pure model.
    */
  final case class MeasureLane(nonNullWitnesses: BigInt, sum: BigDecimal)
      extends Serializable {
    require(
      nonNullWitnesses >= 0,
      s"non-NULL witness count must not be negative: $nonNullWitnesses"
    )
  }

  object MeasureLane {
    val zero: MeasureLane = MeasureLane(BigInt(0), ExactDecimal.zero)
  }

  /**
    * Exact decimal arithmetic.
    *
    * `scala.math.BigDecimal` defaults to `MathContext.DECIMAL128`, whose
    * operations round. Every lane operation therefore goes through the
    * unlimited-precision Java operations so that the model is exact and any
    * future rounding is a deliberate PostgreSQL finalization decision instead
    * of an accident of the oracle.
    */
  object ExactDecimal {
    private val unlimited: java.math.MathContext = java.math.MathContext.UNLIMITED

    val zero: BigDecimal = exact(java.math.BigDecimal.ZERO)

    def exact(value: java.math.BigDecimal): BigDecimal =
      new scala.math.BigDecimal(value, unlimited)

    def of(value: BigInt): BigDecimal =
      exact(new java.math.BigDecimal(value.bigInteger))

    def plus(left: BigDecimal, right: BigDecimal): BigDecimal =
      exact(left.bigDecimal.add(right.bigDecimal))

    def times(left: BigDecimal, right: BigDecimal): BigDecimal =
      exact(left.bigDecimal.multiply(right.bigDecimal))

    def timesCount(left: BigDecimal, right: BigInt): BigDecimal =
      times(left, of(right))
  }

  /**
    * The exact annotation of one boundary-key assignment.
    *
    * `support` is idempotent and answers "does this group have a witness at
    * all"; `witnesses` is the exact `COUNT(*)` multiplicity; `lanes` is
    * positional and parallel to [[AggregateRequest.lanes]].
    *
    * Bag-derived states in the currently admitted single-owner lane fragment
    * satisfy `support == (witnesses > 0)` and each non-NULL count is at most
    * `witnesses`. The value type intentionally admits the larger product
    * algebra (for example `support = true, witnesses = 0`) so the future U3
    * support lane can be evaluated without first forcing an unsafe total-count
    * transition. Such a state is not by itself a certificate that a SQL
    * lowering is reachable; the compiler must validate the admitted request
    * and physical lane program separately.
    */
  final case class Annotation(
      support: Boolean,
      witnesses: BigInt,
      lanes: Vector[MeasureLane]
  ) extends Serializable {
    require(witnesses >= 0, s"witness count must not be negative: $witnesses")
    require(
      support || (witnesses == 0 && lanes.forall(_ == MeasureLane.zero)),
      "an unsupported annotation must be the additive identity"
    )
  }

  object Annotation {

    /** The `merge` identity: no witness at all. */
    def empty(laneCount: Int): Annotation =
      Annotation(support = false, BigInt(0), Vector.fill(laneCount)(MeasureLane.zero))

    /** The `join` identity: exactly one witness contributing no measure. */
    def unit(laneCount: Int): Annotation =
      Annotation(support = true, BigInt(1), Vector.fill(laneCount)(MeasureLane.zero))

    /**
      * Semiring product for independent components. Each lane is weighted by
      * the witness counts of every *other* component, which is what makes a
      * measure supplied by one occurrence correct across an unguarded join.
      */
    def join(left: Annotation, right: Annotation): Annotation = {
      require(left != null, "left annotation must not be null")
      require(right != null, "right annotation must not be null")
      require(
        left.lanes.size == right.lanes.size,
        "joined annotations must have the same lane arity"
      )
      if (!left.support || !right.support) empty(left.lanes.size)
      else
        Annotation(
          support = true,
          witnesses = left.witnesses * right.witnesses,
          lanes = left.lanes.iterator
            .zip(right.lanes.iterator)
            .map { case (leftLane, rightLane) =>
              MeasureLane(
                nonNullWitnesses =
                  leftLane.nonNullWitnesses * right.witnesses +
                    rightLane.nonNullWitnesses * left.witnesses,
                sum = ExactDecimal.plus(
                  ExactDecimal.timesCount(leftLane.sum, right.witnesses),
                  ExactDecimal.timesCount(rightLane.sum, left.witnesses)
                )
              )
            }
            .toVector
        )
    }

    /** Semiring sum, used to eliminate a variable or to form a group. */
    def merge(left: Annotation, right: Annotation): Annotation = {
      require(left != null, "left annotation must not be null")
      require(right != null, "right annotation must not be null")
      require(
        left.lanes.size == right.lanes.size,
        "merged annotations must have the same lane arity"
      )
      Annotation(
        support = left.support || right.support,
        witnesses = left.witnesses + right.witnesses,
        lanes = left.lanes.iterator
          .zip(right.lanes.iterator)
          .map { case (leftLane, rightLane) =>
            MeasureLane(
              nonNullWitnesses =
                leftLane.nonNullWitnesses + rightLane.nonNullWitnesses,
              sum = ExactDecimal.plus(leftLane.sum, rightLane.sum)
            )
          }
          .toVector
      )
    }

    def joinAll(laneCount: Int, inputs: IterableOnce[Annotation]): Annotation =
      inputs.iterator.foldLeft(unit(laneCount))(join)

    def mergeAll(laneCount: Int, inputs: IterableOnce[Annotation]): Annotation =
      inputs.iterator.foldLeft(empty(laneCount))(merge)
  }

  /**
    * A terminal aggregate value under SQL semantics. `AVG` deliberately stays
    * an exact ratio: PostgreSQL's scale-sensitive finalization is a lowering
    * decision and is out of scope for U0.
    */
  sealed trait TerminalValue extends Product with Serializable

  object TerminalValue {
    case object SqlNull extends TerminalValue
    final case class ExactCount(value: BigInt) extends TerminalValue
    final case class ExactDecimalValue(value: BigDecimal) extends TerminalValue
    final case class ExactRatio(numerator: BigDecimal, denominator: BigInt)
        extends TerminalValue
  }

  /**
    * The frozen terminal rules of the plan's algebra section, as pure
    * functions of a group's finalized annotation.
    */
  object Terminal {

    /** A group is emitted exactly when the idempotent support lane holds. */
    def isEmitted(annotation: Annotation): Boolean = annotation.support

    def countStar(annotation: Annotation): BigInt = annotation.witnesses

    /**
      * Zero rather than NULL for a supported group whose measure was NULL in
      * every witness. This is the `COALESCE(count, 0)` half of the U3
      * two-lane terminal, and it is why the support lane may not be replaced
      * by the weighted lane.
      */
    def countColumn(annotation: Annotation, lane: Int): BigInt =
      annotation.lanes(lane).nonNullWitnesses

    def sumColumn(annotation: Annotation, lane: Int): TerminalValue = {
      val measure = annotation.lanes(lane)
      if (measure.nonNullWitnesses == 0) TerminalValue.SqlNull
      else TerminalValue.ExactDecimalValue(measure.sum)
    }

    def averageColumn(annotation: Annotation, lane: Int): TerminalValue = {
      val measure = annotation.lanes(lane)
      if (measure.nonNullWitnesses == 0) TerminalValue.SqlNull
      else TerminalValue.ExactRatio(measure.sum, measure.nonNullWitnesses)
    }
  }

  // ---------------------------------------------------------------------
  // Arithmetic contract
  // ---------------------------------------------------------------------

  sealed abstract class LaneCarrier(val code: String)
      extends Product
      with Serializable

  object LaneCarrier {
    case object ArbitraryPrecisionInteger
        extends LaneCarrier("arbitrary-precision-integer")
    case object ArbitraryPrecisionExactDecimal
        extends LaneCarrier("arbitrary-precision-exact-decimal")
    case object IdempotentBoolean extends LaneCarrier("idempotent-boolean")
  }

  /**
    * What the model guarantees arithmetically, and what it explicitly does not
    * yet guarantee about a PostgreSQL lowering.
    */
  final case class AggregateArithmeticContract private[postgresql] (
      support: LaneCarrier,
      witnesses: LaneCarrier,
      nonNullWitnesses: LaneCarrier,
      sum: LaneCarrier,
      emptyInputGroupCount: BigInt,
      absentCountColumnValue: BigInt,
      allNullMeasureSumIsNull: Boolean,
      nullableCountHasPostgreSqlCarrierProof: Boolean,
      signedSumHasPostgreSqlCarrierProof: Boolean
  ) extends Serializable

  val ExactAggregateArithmetic: AggregateArithmeticContract =
    AggregateArithmeticContract(
      support = LaneCarrier.IdempotentBoolean,
      witnesses = LaneCarrier.ArbitraryPrecisionInteger,
      nonNullWitnesses = LaneCarrier.ArbitraryPrecisionInteger,
      sum = LaneCarrier.ArbitraryPrecisionExactDecimal,
      emptyInputGroupCount = BigInt(0),
      absentCountColumnValue = BigInt(0),
      allNullMeasureSumIsNull = true,
      nullableCountHasPostgreSqlCarrierProof = false,
      signedSumHasPostgreSqlCarrierProof = false
    )

  // ---------------------------------------------------------------------
  // Derived plan
  // ---------------------------------------------------------------------

  /** Where one live key or lane measure is introduced. */
  final case class KeyBinding private[postgresql] (
      key: AggregateKey,
      owner: BagId
  ) extends Serializable

  final case class LaneBinding private[postgresql] (
      ordinal: Int,
      kind: LaneKind,
      measure: ColumnId,
      owner: BagId
  ) extends Serializable {
    def loweringStatus: LaneLoweringStatus = kind.loweringStatus
  }

  final case class Bag private[postgresql] (
      bagId: BagId,
      vertices: Vector[VariableId],
      parent: Option[BagId],
      children: Vector[BagId],
      separatorToParent: Option[Vector[VariableId]],
      /**
        * Exact union of the parent separator (when present) and every child
        * separator. Grouping the local weighted factor by these keys together
        * with [[ownedKeys]] eliminates every variable no neighbouring message
        * and no live key needs.
        */
      separatorGrouping: Vector[VariableId],
      /** Live keys introduced here, in request output order. */
      ownedKeys: Vector[AggregateKey],
      /** Lane ordinals whose measure column is converted here. */
      ownedLanes: Vector[Int]
  ) extends Serializable {

    /**
      * The structural columns the local weighted factor groups by. Structural
      * owned keys may coincide with a child separator variable and are
      * deduplicated; occurrence-owned value keys are grouped by their exact
      * column lineage and are listed by [[ownedValueKeyColumns]].
      */
    lazy val localGroupingVariables: Vector[VariableId] =
      (separatorGrouping.iterator ++ ownedKeys.iterator.collect {
        case AggregateKey.Structural(variable) => variable
      }).toVector.distinct.sorted

    lazy val ownedValueKeyColumns: Vector[ColumnId] =
      ownedKeys.collect { case AggregateKey.OccurrenceValue(column) => column }
  }

  /**
    * One message.
    *
    * `separator` is the ordinary structural separator and is the *only* set of
    * columns joined with NULL-rejecting equality. `carriedKeys` are live keys
    * introduced in this message's subtree; they are projected and grouped with
    * SQL grouping equivalence and never appear in `separator`.
    */
  final case class Message private[postgresql] (
      ordinal: Int,
      source: BagId,
      target: BagId,
      separator: Vector[VariableId],
      carriedKeys: Vector[AggregateKey],
      inputMessageOrdinals: Vector[Int],
      /**
        * False when the locally grouped schema already equals this message
        * schema, so the weighted annotation can be projected without a
        * redundant regrouping.
        */
      requiresRegrouping: Boolean
  ) extends Serializable {
    def carriedKeyCount: Int = carriedKeys.size
  }

  final case class TerminalFinalization private[postgresql] (
      root: BagId,
      inputMessageOrdinals: Vector[Int],
      /** Every live key, in original request output order. */
      groupKeys: Vector[AggregateKey]
  ) extends Serializable

  final case class ResourceTerms private[postgresql] (
      localAggregateTerms: BigInt,
      localKeyProjectionTerms: BigInt,
      localKeyGroupingTerms: BigInt,
      localLaneConversionTerms: BigInt,
      propagatedMessageAggregateTerms: BigInt,
      /** Non-regrouped messages still project one computed frequency scalar. */
      propagatedMessageProjectionTerms: BigInt,
      propagatedSchemaProjectionTerms: BigInt,
      propagatedSchemaGroupingTerms: BigInt,
      /** Separator equalities only. Carried keys are never join terms. */
      propagatedJoinEqualityTerms: BigInt,
      terminalGroupingTerms: BigInt
  ) extends Serializable {
    lazy val additionalRenderedTerms: BigInt =
      localAggregateTerms +
        localKeyProjectionTerms +
        localKeyGroupingTerms +
        localLaneConversionTerms +
        propagatedMessageAggregateTerms +
        propagatedMessageProjectionTerms +
        propagatedSchemaProjectionTerms +
        propagatedSchemaGroupingTerms +
        propagatedJoinEqualityTerms +
        terminalGroupingTerms
  }

  final case class Plan private[postgresql] (
      root: BagId,
      bags: Vector[Bag],
      postorder: Vector[BagId],
      messages: Vector[Message],
      finalization: TerminalFinalization,
      keyBindings: Vector[KeyBinding],
      laneBindings: Vector[LaneBinding],
      arithmetic: AggregateArithmeticContract,
      resources: ResourceTerms
  ) extends Serializable {

    lazy val bagById: Map[BagId, Bag] =
      bags.iterator.map(bag => bag.bagId -> bag).toMap

    lazy val messageBySource: Map[BagId, Message] =
      messages.iterator.map(message => message.source -> message).toMap

    def laneCount: Int = laneBindings.size

    /**
      * Maximum number of recursively nested message subqueries between the
      * terminal and a leaf. Sibling messages do not increase it, so a wide
      * shallow star stays depth one. The fixed ceiling of 64 is enforced by
      * the PostgreSQL compiler, not here.
      */
    lazy val maximumMessageNestingDepth: Int = {
      val depthByOrdinal = Array.fill(messages.size)(0)
      messages.foreach { message =>
        val childDepth = message.inputMessageOrdinals.iterator
          .map(ordinal => depthByOrdinal(ordinal))
          .foldLeft(0)(math.max)
        depthByOrdinal(message.ordinal) = childDepth + 1
      }
      finalization.inputMessageOrdinals.iterator
        .map(ordinal => depthByOrdinal(ordinal))
        .foldLeft(0)(math.max)
    }

    /**
      * Bags that join two or more inbound messages each carrying at least one
      * live key, so the annotation state there is a product of partial group
      * tuples. The plan requires this state size to be exposed rather than
      * hidden, because multiple group keys on different branches can make it
      * unavoidable.
      */
    lazy val carriedKeyProductBags: Vector[BagId] =
      bags.iterator
        .filter { bag =>
          bag.children.count(child =>
            messageBySource.get(child).exists(_.carriedKeyCount > 0)
          ) > 1
        }
        .map(_.bagId)
        .toVector
        .sorted

    /** Largest number of live keys present in any single message schema. */
    lazy val maximumCarriedKeys: Int =
      messages.iterator.map(_.carriedKeyCount).foldLeft(0)(math.max)
  }

  // ---------------------------------------------------------------------
  // Errors
  // ---------------------------------------------------------------------

  sealed abstract class ErrorCode(val value: String)
      extends Product
      with Serializable

  object ErrorCode {
    case object MissingValue extends ErrorCode("missing-value")

    /**
      * The supplied bag-factor plan is not a structurally valid decomposition
      * plan. The frozen structural boundary is reused as a validator only; no
      * frequency-propagation message program is consumed here.
      */
    case object StructuralSource extends ErrorCode("structural-source")

    case object UnknownFinalizationRoot
        extends ErrorCode("unknown-finalization-root")

    /**
      * The supplied bag tree violates the running-intersection property: the
      * bags holding some variable do not form one connected subtree. Message
      * passing over such a tree silently drops that variable's equality, so it
      * is rejected regardless of which keys were requested.
      */
    case object NonDecomposition extends ErrorCode("non-decomposition")
    case object DuplicateGroupKey extends ErrorCode("duplicate-group-key")
    case object UnknownVariable extends ErrorCode("unknown-variable")
    case object DisconnectedKeySubtree
        extends ErrorCode("disconnected-key-subtree")
    case object UnknownOccurrence extends ErrorCode("unknown-occurrence")
    case object UnownedOccurrence extends ErrorCode("unowned-occurrence")
    case object UnknownOwnerBag extends ErrorCode("unknown-owner-bag")
    case object DuplicateLaneMeasure extends ErrorCode("duplicate-lane-measure")
    case object CarriedKeyInSeparator
        extends ErrorCode("carried-key-in-separator")
    case object KeyNotLiveAtTerminal extends ErrorCode("key-not-live-at-terminal")
    case object CandidateMismatch extends ErrorCode("candidate-mismatch")
  }

  final case class Error(code: ErrorCode, detail: String)
      extends Product
      with Serializable {
    def message: String = s"${code.value}: $detail"
  }

  // ---------------------------------------------------------------------
  // Derivation
  // ---------------------------------------------------------------------

  /**
    * The deterministic terminal bag: an owner of the first output key, with
    * stable lowest-bag-id tie-breaking, or the supplied plan's own root for a
    * global aggregate. Root choice uses bag *membership* so that it does not
    * depend on the orientation it is about to define.
    */
  def defaultFinalizationRoot(
      source: BagFactorPlan,
      request: AggregateRequest
  ): Either[Vector[Error], BagId] =
    missingInputs(source, request) match {
      case problems if problems.nonEmpty => Left(problems)
      case _ =>
        structuralProblems(source) match {
          case problems if problems.nonEmpty => Left(problems)
          case _ =>
            request.groupKeys.headOption match {
              case None => Right(source.root)
              case Some(AggregateKey.Structural(variable)) =>
                source.factors
                  .filter(_.vertices.contains(variable))
                  .map(_.bagId)
                  .sorted
                  .headOption
                  .toRight(
                    Vector(
                      Error(
                        ErrorCode.UnknownVariable,
                        s"variable ${variable.value} is absent from every bag"
                      )
                    )
                  )
              case Some(AggregateKey.OccurrenceValue(column)) =>
                occurrenceOwner(source, column).fold(
                  problem => Left(Vector(problem)),
                  owner => Right(owner)
                )
            }
        }
    }

  /**
    * Validates the aggregate boundary and derives the unique leaf-to-terminal
    * message program for `request`. Input vector order is immaterial; only the
    * request's own group-key order is observable, and only at the terminal.
    */
  def derive(
      source: BagFactorPlan,
      finalizationRoot: BagId,
      request: AggregateRequest
  ): Either[Vector[Error], Plan] = {
    val missing = missingInputs(source, request) ++
      (if (finalizationRoot eq null)
         Vector(Error(ErrorCode.MissingValue, "finalization root"))
       else Vector.empty)
    if (missing.nonEmpty) return Left(canonical(missing))

    val structural = structuralProblems(source)
    if (structural.nonEmpty) return Left(structural)

    if (!source.factors.exists(_.bagId == finalizationRoot))
      return Left(
        Vector(
          Error(
            ErrorCode.UnknownFinalizationRoot,
            s"bag ${finalizationRoot.value} has no factor"
          )
        )
      )

    val requestProblems = validateRequestShape(request)
    if (requestProblems.nonEmpty) return Left(requestProblems)

    val orientation = orient(source, finalizationRoot)

    val decompositionProblems = validateRunningIntersection(source, orientation)
    if (decompositionProblems.nonEmpty) return Left(decompositionProblems)

    val keyResult = bindKeys(source, orientation, request)
    val laneResult = bindLanes(source, request)
    val bindingProblems = canonical(
      keyResult.fold(identity, _ => Vector.empty) ++
        laneResult.fold(identity, _ => Vector.empty)
    )
    if (bindingProblems.nonEmpty) return Left(bindingProblems)

    build(
      source,
      orientation,
      request,
      keyResult.getOrElse(Vector.empty),
      laneResult.getOrElse(Vector.empty)
    )
  }

  /**
    * Independently re-derives the canonical model and compares every field, so
    * that a cached or transported aggregate message program is rejected when
    * it no longer matches its bag-factor plan and request.
    */
  def validate(
      source: BagFactorPlan,
      finalizationRoot: BagId,
      request: AggregateRequest,
      candidate: Plan
  ): Either[Vector[Error], Plan] =
    derive(source, finalizationRoot, request) match {
      case Left(problems) => Left(problems)
      case Right(_) if candidate eq null =>
        Left(Vector(Error(ErrorCode.MissingValue, "aggregate-propagation plan")))
      case Right(expected) => compare(expected, candidate)
    }

  private def compare(
      expected: Plan,
      candidate: Plan
  ): Either[Vector[Error], Plan] = {
    val problems = Vector.newBuilder[Error]
    if (candidate.root != expected.root) problems += mismatch("root")
    if (candidate.bags != expected.bags) problems += mismatch("bags")
    if (candidate.postorder != expected.postorder) problems += mismatch("postorder")
    if (candidate.messages != expected.messages) problems += mismatch("messages")
    if (candidate.finalization != expected.finalization)
      problems += mismatch("finalization")
    if (candidate.keyBindings != expected.keyBindings)
      problems += mismatch("key bindings")
    if (candidate.laneBindings != expected.laneBindings)
      problems += mismatch("lane bindings")
    if (candidate.arithmetic != expected.arithmetic)
      problems += mismatch("arithmetic")
    if (candidate.resources != expected.resources) problems += mismatch("resources")
    val result = canonical(problems.result())
    if (result.nonEmpty) Left(result) else Right(candidate)
  }

  private def missingInputs(
      source: BagFactorPlan,
      request: AggregateRequest
  ): Vector[Error] = {
    val problems = Vector.newBuilder[Error]
    if (source eq null) problems += Error(ErrorCode.MissingValue, "bag-factor plan")
    if (request eq null) problems += Error(ErrorCode.MissingValue, "aggregate request")
    else {
      if (request.groupKeys eq null)
        problems += Error(ErrorCode.MissingValue, "group keys")
      else if (request.groupKeys.exists(_ eq null))
        problems += Error(ErrorCode.MissingValue, "group keys contains null")
      if (request.lanes eq null)
        problems += Error(ErrorCode.MissingValue, "lanes")
      else request.lanes.zipWithIndex.foreach { case (lane, index) =>
        if (lane eq null)
          problems += Error(ErrorCode.MissingValue, s"lanes[$index]")
        else {
          if (lane.kind eq null)
            problems += Error(ErrorCode.MissingValue, s"lanes[$index].kind")
          if (lane.measure eq null)
            problems += Error(ErrorCode.MissingValue, s"lanes[$index].measure")
        }
      }
    }
    canonical(problems.result())
  }

  /**
    * Reuses the frozen structural boundary as a *validator only*. This model
    * deliberately does not consume, generalize, or extend the frequency
    * message program; sharing internals is gated on the plan's independent
    * byte-equivalence tests.
    */
  private def structuralProblems(source: BagFactorPlan): Vector[Error] =
    PostgreSqlFrequencyPropagation.derive(source) match {
      case Right(_) => Vector.empty
      case Left(problems) =>
        canonical(
          problems.map(problem =>
            Error(ErrorCode.StructuralSource, problem.message)
          )
        )
    }

  private def validateRequestShape(request: AggregateRequest): Vector[Error] = {
    val problems = Vector.newBuilder[Error]
    request.groupKeys
      .groupBy(identity)
      .collect { case (key, occurrences) if occurrences.size > 1 => key }
      .toVector
      .sorted(AggregateKey.ordering)
      .foreach(key =>
        problems += Error(
          ErrorCode.DuplicateGroupKey,
          s"group key ${keyLabel(key)} appears more than once"
        )
      )
    request.lanes
      .groupBy(lane => (lane.kind, lane.measure))
      .collect { case (key, occurrences) if occurrences.size > 1 => key }
      .toVector
      .sortBy { case (kind, measure) => (kind.code, measure.toString) }
      .foreach { case (kind, measure) =>
        problems += Error(
          ErrorCode.DuplicateLaneMeasure,
          s"lane ${kind.code} over $measure is requested more than once"
        )
      }
    canonical(problems.result())
  }

  /** The terminal orientation. The physical semijoin schedule is untouched. */
  private final case class Orientation(
      root: BagId,
      parent: Map[BagId, BagId],
      children: Map[BagId, Vector[BagId]],
      separatorToParent: Map[BagId, Vector[VariableId]],
      depth: Map[BagId, Int],
      postorder: Vector[BagId]
  )

  private def orient(
      source: BagFactorPlan,
      finalizationRoot: BagId
  ): Orientation = {
    val neighborBuffers = mutable.Map.empty[
      BagId,
      mutable.ArrayBuffer[(BagId, Vector[VariableId])]
    ]
    source.factors.foreach(factor =>
      neighborBuffers.put(factor.bagId, mutable.ArrayBuffer.empty)
    )
    source.arcs.foreach { arc =>
      neighborBuffers(arc.parent) += (arc.child -> arc.separator)
      neighborBuffers(arc.child) += (arc.parent -> arc.separator)
    }
    val neighbors = neighborBuffers.iterator.map { case (bagId, values) =>
      bagId -> values.sortBy(_._1).toVector
    }.toMap

    val parent = mutable.Map.empty[BagId, BagId]
    val separatorToParent = mutable.Map.empty[BagId, Vector[VariableId]]
    val children = mutable.Map.empty[BagId, Vector[BagId]]
    val depth = mutable.Map[BagId, Int](finalizationRoot -> 0)
    val pending = mutable.Stack[(BagId, Option[BagId])](finalizationRoot -> None)
    while (pending.nonEmpty) {
      val (current, from) = pending.pop()
      val outgoing = neighbors(current).collect {
        case (neighbor, separator) if !from.contains(neighbor) =>
          parent.put(neighbor, current)
          separatorToParent.put(neighbor, separator)
          depth.put(neighbor, depth(current) + 1)
          neighbor
      }.sorted
      children.put(current, outgoing)
      outgoing.reverseIterator.foreach(child => pending.push(child -> Some(current)))
    }

    val postorderBuilder = Vector.newBuilder[BagId]
    val walk = mutable.Stack[(BagId, Boolean)](finalizationRoot -> false)
    while (walk.nonEmpty) {
      val (current, expanded) = walk.pop()
      if (expanded) postorderBuilder += current
      else {
        walk.push(current -> true)
        children(current).reverseIterator.foreach(child => walk.push(child -> false))
      }
    }

    Orientation(
      root = finalizationRoot,
      parent = parent.toMap,
      children = children.toMap,
      separatorToParent = separatorToParent.toMap,
      depth = depth.toMap,
      postorder = postorderBuilder.result()
    )
  }

  /**
    * Rejects any source whose bags do not satisfy the running-intersection
    * property.
    *
    * The structural boundary shared with the frozen frequency model checks tree
    * shape, exact-intersection separators, depth, and reachability, but none of
    * those imply running intersection. Without it a variable can occur in two
    * bags whose connecting bag omits it; no message schema then carries that
    * variable, its equality is never enforced, and the message program computes
    * a strictly larger relation than the source join. Production plans come
    * from a validated `CoveredDecomposition`, so this is defence in depth --
    * but it must hold for *every* variable, not only for the ones a particular
    * request happens to group by.
    *
    * In a rooted tree, the bags containing a variable form a connected subtree
    * exactly when precisely one of them has no parent inside that set.
    */
  private def validateRunningIntersection(
      source: BagFactorPlan,
      orientation: Orientation
  ): Vector[Error] = {
    val bagsByVariable = mutable.Map.empty[VariableId, mutable.ArrayBuffer[BagId]]
    source.factors.foreach { factor =>
      factor.vertices.foreach { variable =>
        bagsByVariable
          .getOrElseUpdate(variable, mutable.ArrayBuffer.empty)
          .append(factor.bagId)
      }
    }

    val problems = Vector.newBuilder[Error]
    bagsByVariable.toVector.sortBy(_._1).foreach { case (variable, holders) =>
      val holderSet = holders.toSet
      val tops = holders.iterator
        .filterNot(bag => orientation.parent.get(bag).exists(holderSet.contains))
        .toVector
        .sorted
      if (tops.size != 1)
        problems += Error(
          ErrorCode.NonDecomposition,
          s"variable ${variable.value} occupies a disconnected bag set with " +
            s"${tops.size} highest bags (" + tops.map(_.value).mkString(",") + ")"
        )
    }
    canonical(problems.result())
  }

  /**
    * Binds each live key to its unique introduction bag.
    *
    * A structural key is introduced at the shallowest bag containing it. Bags
    * containing a variable are connected in a valid decomposition, so that bag
    * is unique; the connectivity is re-checked here rather than assumed.
    */
  private def bindKeys(
      source: BagFactorPlan,
      orientation: Orientation,
      request: AggregateRequest
  ): Either[Vector[Error], Vector[KeyBinding]] = {
    val problems = Vector.newBuilder[Error]
    val bindings = Vector.newBuilder[KeyBinding]

    request.groupKeys.foreach {
      case key @ AggregateKey.Structural(variable) =>
        val containing = source.factors
          .filter(_.vertices.contains(variable))
          .map(_.bagId)
        if (containing.isEmpty)
          problems += Error(
            ErrorCode.UnknownVariable,
            s"variable ${variable.value} is absent from every bag"
          )
        else {
          // Defence in depth. `validateRunningIntersection` has already
          // rejected any source whose bags for a variable are disconnected, so
          // a unique shallowest bag must exist here; these checks stay so that
          // a future change to that validation cannot silently invent an
          // introduction point for a carried key.
          val containingSet = containing.toSet
          val minimumDepth = containing.iterator.map(orientation.depth).min
          val shallowest =
            containing.filter(orientation.depth(_) == minimumDepth).sorted
          val detached = containing
            .filter(orientation.depth(_) != minimumDepth)
            .filterNot(bag =>
              orientation.parent.get(bag).exists(containingSet.contains)
            )
            .sorted
          if (shallowest.size != 1)
            problems += Error(
              ErrorCode.DisconnectedKeySubtree,
              s"variable ${variable.value} has ${shallowest.size} shallowest bags"
            )
          else if (detached.nonEmpty)
            problems += Error(
              ErrorCode.DisconnectedKeySubtree,
              s"variable ${variable.value} is absent from the parent of bags " +
                detached.map(_.value).mkString(",")
            )
          else bindings += KeyBinding(key, shallowest.head)
        }
      case key @ AggregateKey.OccurrenceValue(column) =>
        occurrenceOwner(source, column) match {
          case Left(problem) => problems += problem
          case Right(owner)  => bindings += KeyBinding(key, owner)
        }
    }

    val result = canonical(problems.result())
    if (result.nonEmpty) Left(result) else Right(bindings.result())
  }

  private def bindLanes(
      source: BagFactorPlan,
      request: AggregateRequest
  ): Either[Vector[Error], Vector[LaneBinding]] = {
    val problems = Vector.newBuilder[Error]
    val bindings = Vector.newBuilder[LaneBinding]
    request.lanes.zipWithIndex.foreach { case (lane, ordinal) =>
      occurrenceOwner(source, lane.measure) match {
        case Left(problem) => problems += problem
        case Right(owner) =>
          bindings += LaneBinding(ordinal, lane.kind, lane.measure, owner)
      }
    }
    val result = canonical(problems.result())
    if (result.nonEmpty) Left(result) else Right(bindings.result())
  }

  /**
    * Root-nearest occurrence ownership from the supplied plan. This is the
    * current structural contract and is deliberately not re-derived for the
    * terminal orientation.
    */
  private def occurrenceOwner(
      source: BagFactorPlan,
      column: ColumnId
  ): Either[Error, BagId] = {
    val occurrence: RelationOccurrenceId = column.relationOccurrenceId
    source.sourceHypergraph.edgeFor(occurrence) match {
      case None =>
        Left(
          Error(
            ErrorCode.UnknownOccurrence,
            s"relation occurrence ${occurrence.value} is absent from the hypergraph"
          )
        )
      case Some(edge) => ownerOfEdge(source, edge.id, occurrence)
    }
  }

  private def ownerOfEdge(
      source: BagFactorPlan,
      edgeId: EdgeId,
      occurrence: RelationOccurrenceId
  ): Either[Error, BagId] =
    source.ownership.find(_.edgeId == edgeId) match {
      case None =>
        Left(
          Error(
            ErrorCode.UnownedOccurrence,
            s"edge ${edgeId.value} for occurrence ${occurrence.value} has no owner"
          )
        )
      case Some(ownership) if !source.factors.exists(_.bagId == ownership.owner) =>
        Left(
          Error(
            ErrorCode.UnknownOwnerBag,
            s"edge ${edgeId.value} is owned by unknown bag ${ownership.owner.value}"
          )
        )
      case Some(ownership) => Right(ownership.owner)
    }

  private def build(
      source: BagFactorPlan,
      orientation: Orientation,
      request: AggregateRequest,
      keyBindings: Vector[KeyBinding],
      laneBindings: Vector[LaneBinding]
  ): Either[Vector[Error], Plan] = {
    val root = orientation.root

    // Liveness: a key introduced at `owner` is live on every arc from `owner`
    // up to the terminal, and is consumed only by the terminal grouping.
    //
    // Do not materialize the subtree of every bag here. That representation is
    // quadratic on a chain and would allocate it before the compiler can apply
    // the fixed aggregate-depth guard. Walking each requested key's unique
    // owner-to-terminal path is linear in the message state that the request
    // actually exposes (O(sum of live-key path lengths)); for U1 there are
    // exactly two such keys.
    val carriedKeyBuffers = mutable.Map.empty[
      BagId,
      mutable.ArrayBuffer[AggregateKey]
    ]
    orientation.postorder.foreach(bagId =>
      carriedKeyBuffers.put(bagId, mutable.ArrayBuffer.empty)
    )
    keyBindings.foreach { binding =>
      var current = binding.owner
      while (current != root) {
        carriedKeyBuffers(current) += binding.key
        current = orientation.parent(current)
      }
    }
    val carriedKeysByBag = carriedKeyBuffers.iterator.map {
      case (bagId, keys) => bagId -> keys.toVector
    }.toMap

    def carriedFrom(bagId: BagId): Vector[AggregateKey] =
      carriedKeysByBag.getOrElse(bagId, Vector.empty)

    val ownedKeysByBag = keyBindings
      .groupBy(_.owner)
      .iterator
      .map { case (owner, group) => owner -> group.map(_.key) }
      .toMap
    val ownedLanesByBag = laneBindings
      .groupBy(_.owner)
      .iterator
      .map { case (owner, group) => owner -> group.map(_.ordinal).sorted }
      .toMap

    val problems = Vector.newBuilder[Error]

    val messages = Vector.newBuilder[Message]
    val ordinalBySource = mutable.Map.empty[BagId, Int]
    orientation.postorder.filterNot(_ == root).foreach { sourceBagId =>
      val ordinal = ordinalBySource.size
      val children = orientation.children(sourceBagId)
      val separator = orientation.separatorToParent(sourceBagId)
      val carried = carriedFrom(sourceBagId)

      // Enabling lemma for the NULL contract: a live key never appears in the
      // separator of an arc it crosses, so carrying a key can never turn it
      // into a NULL-rejecting join predicate.
      carried.foreach {
        case AggregateKey.Structural(variable) if separator.contains(variable) =>
          problems += Error(
            ErrorCode.CarriedKeyInSeparator,
            s"variable ${variable.value} is carried across a separator that already binds it"
          )
        case _ => ()
      }

      // After joining the child messages the row schema is exactly
      // `separatorGrouping ++ carriedKeys`, while the message schema is
      // `separator ++ carriedKeys`. A regrouping aggregate is therefore
      // redundant precisely when the two agree, that is when every separator
      // variable beyond the parent separator is itself a carried live key.
      // Live keys never force a regrouping: a key owned here joins the local
      // grouping, and a key from a child arrives already grouped in that
      // child's message.
      val separatorGrouping =
        (separator.iterator ++
          children.iterator.flatMap(orientation.separatorToParent))
          .toVector
          .distinct
          .sorted
      val carriedVariables = carried.collect {
        case AggregateKey.Structural(variable) => variable
      }.toSet

      messages += Message(
        ordinal = ordinal,
        source = sourceBagId,
        target = orientation.parent(sourceBagId),
        separator = separator,
        carriedKeys = carried,
        inputMessageOrdinals = children.map(ordinalBySource),
        requiresRegrouping =
          separatorGrouping.filterNot(carriedVariables.contains) != separator
      )
      ordinalBySource.put(sourceBagId, ordinal)
    }
    val messageResult = messages.result()

    // Every requested key must reach the terminal. This holds by construction
    // for a connected tree and is re-checked so a future re-rooting cannot
    // silently drop a live key.
    val liveAtTerminal =
      (ownedKeysByBag.getOrElse(root, Vector.empty).iterator ++
        orientation
          .children(root)
          .iterator
          .flatMap(child => carriedFrom(child))).toSet
    request.groupKeys.foreach { key =>
      if (!liveAtTerminal.contains(key))
        problems += Error(
          ErrorCode.KeyNotLiveAtTerminal,
          s"group key ${keyLabel(key)} does not reach bag ${root.value}"
        )
    }

    val collected = canonical(problems.result())
    if (collected.nonEmpty) return Left(collected)

    val bags = source.factors.sortBy(_.bagId).map { factor =>
      val parent = orientation.parent.get(factor.bagId)
      val children = orientation.children(factor.bagId)
      val parentSeparator =
        parent.iterator.flatMap(_ => orientation.separatorToParent(factor.bagId))
      val childSeparators =
        children.iterator.flatMap(orientation.separatorToParent)
      Bag(
        bagId = factor.bagId,
        vertices = factor.vertices,
        parent = parent,
        children = children,
        separatorToParent =
          parent.map(_ => orientation.separatorToParent(factor.bagId)),
        separatorGrouping =
          (parentSeparator ++ childSeparators).toVector.distinct.sorted,
        ownedKeys = ownedKeysByBag.getOrElse(factor.bagId, Vector.empty),
        ownedLanes = ownedLanesByBag.getOrElse(factor.bagId, Vector.empty)
      )
    }

    val localGroupingTerms = bags.iterator.foldLeft(BigInt(0)) { (total, bag) =>
      total + bag.localGroupingVariables.size + bag.ownedValueKeyColumns.size
    }
    val separatorTerms = messageResult.iterator.foldLeft(BigInt(0)) {
      (total, message) => total + message.separator.size
    }
    val schemaTerms = messageResult.iterator.foldLeft(BigInt(0)) {
      (total, message) => total + message.separator.size + message.carriedKeyCount
    }
    val regroupedSchemaTerms = messageResult.iterator
      .filter(_.requiresRegrouping)
      .foldLeft(BigInt(0)) { (total, message) =>
        total + message.separator.size + message.carriedKeyCount
      }

    val resources = ResourceTerms(
      localAggregateTerms = BigInt(bags.size),
      localKeyProjectionTerms = localGroupingTerms,
      localKeyGroupingTerms = localGroupingTerms,
      localLaneConversionTerms = BigInt(laneBindings.size),
      propagatedMessageAggregateTerms =
        BigInt(messageResult.count(_.requiresRegrouping)),
      propagatedMessageProjectionTerms =
        BigInt(messageResult.count(message => !message.requiresRegrouping)),
      propagatedSchemaProjectionTerms = schemaTerms,
      propagatedSchemaGroupingTerms = regroupedSchemaTerms,
      propagatedJoinEqualityTerms = separatorTerms,
      terminalGroupingTerms = BigInt(request.groupKeys.size)
    )

    Right(
      Plan(
        root = root,
        bags = bags,
        postorder = orientation.postorder,
        messages = messageResult,
        finalization = TerminalFinalization(
          root = root,
          inputMessageOrdinals = orientation.children(root).map(ordinalBySource),
          groupKeys = request.groupKeys
        ),
        keyBindings = keyBindings,
        laneBindings = laneBindings,
        arithmetic = ExactAggregateArithmetic,
        resources = resources
      )
    )
  }

  private def mismatch(field: String): Error =
    Error(ErrorCode.CandidateMismatch, s"non-canonical $field")

  private def keyLabel(key: AggregateKey): String = key match {
    case AggregateKey.Structural(variable)    => s"v${variable.value}"
    case AggregateKey.OccurrenceValue(column) => s"c$column"
  }

  private def canonical(errors: Vector[Error]): Vector[Error] =
    errors.distinct.sortBy(error => (error.code.value, error.detail))
}
