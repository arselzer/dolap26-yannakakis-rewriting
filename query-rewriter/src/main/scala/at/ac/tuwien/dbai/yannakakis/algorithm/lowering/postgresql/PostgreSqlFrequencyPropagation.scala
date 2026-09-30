package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlan
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}

import scala.collection.mutable

/**
  * SQL-free description of exact multiplicity propagation for scalar or
  * one-key grouped COUNT(*).
  *
  * A message is evaluated only after all of its direct input messages. A
  * multi-bag FullTwoPass lowering first uses set-valued separator supports for
  * every semijoin source. It assigns a full-bag frequency to the root only
  * after the last bottom-up filter, and to each non-root only after its inbound
  * top-down filter. Those are the unique points where the surviving rows are
  * globally extendible, so no orphan multiplicity reaches an aggregate. The
  * weighted factor is then grouped by the union of every neighboring separator.
  * Resulting rows are grouped by the separator to the parent; the root is
  * summed without that final grouping. Ordinary equality, rather than
  * null-safe equality, is required for separator joins. An empty separator is
  * valid and denotes one scalar message joined as a Cartesian product.
  *
  * This model intentionally contains no SQL. The PostgreSQL compiler derives
  * and validates it for the full-two-pass global-COUNT fast path, and the
  * emitter lowers the validated messages. Support-cover projections remain
  * set-valued planning metadata and are never treated as multiplicity carriers
  * here. Exactness also requires each supplied bag-factor multiset to have
  * already applied every predicate assigned to that factor; predicates outside
  * the factor tree are not represented by a [[BagFactorPlan]].
  */
object PostgreSqlFrequencyPropagation {
  sealed trait MultiplicityCarrier extends Product with Serializable

  object MultiplicityCarrier {
    /** Unconstrained exact decimal; no intermediate precision boundary. */
    case object ArbitraryPrecisionNumeric extends MultiplicityCarrier

    /**
      * The finite carrier `0 .. Long.MaxValue + 1`, where the last value is an
      * overflow sentinel and both addition and multiplication saturate at it.
      *
      * This is a mathematical carrier, not a claim about the SQL emitted
      * today. A PostgreSQL lowering must saturate *inside* every aggregate
      * transition. Applying `LEAST` only after the built-in `SUM(numeric)` has
      * completed is not sufficient for an arbitrarily large orphan group.
      */
    case object SaturatingBigInt64WithOverflowSentinel
        extends MultiplicityCarrier
  }

  sealed trait FinalCountCarrier extends Product with Serializable

  object FinalCountCarrier {
    /** Non-negative result checked against the signed 64-bit output range. */
    case object SignedBigInt64 extends FinalCountCarrier
  }

  final case class ArithmeticContract private[postgresql] (
      multiplicity: MultiplicityCarrier,
      finalCount: FinalCountCarrier,
      emptyInputCount: BigInt,
      checkedFinalConversion: Boolean
  ) extends Serializable

  val ExactCountArithmetic: ArithmeticContract = ArithmeticContract(
    multiplicity = MultiplicityCarrier.ArbitraryPrecisionNumeric,
    finalCount = FinalCountCarrier.SignedBigInt64,
    emptyInputCount = BigInt(0),
    checkedFinalConversion = true
  )

  /** Future-only until a true transition-saturating SQL aggregate exists. */
  val SaturatingCountArithmetic: ArithmeticContract = ArithmeticContract(
    multiplicity =
      MultiplicityCarrier.SaturatingBigInt64WithOverflowSentinel,
    finalCount = FinalCountCarrier.SignedBigInt64,
    emptyInputCount = BigInt(0),
    checkedFinalConversion = true
  )

  final case class SaturatingAggregateLoweringRequirements private[postgresql] (
      cutoff: BigInt,
      mustClampEveryTransitionAddition: Boolean,
      postAggregateClampIsSufficientForArbitraryGroups: Boolean,
      supportedByCurrentBuiltInNumericSum: Boolean
  ) extends Serializable

  /**
    * Pure cutoff-semiring operations used to specify and test a future
    * transition-saturating PostgreSQL aggregate.
    *
    * Let `C = Long.MaxValue + 1`. `clamp(n) = min(C, n)` is a homomorphism from
    * non-negative integer addition and multiplication to these operations.
    * Consequently a sum-product tree evaluated with this carrier returns the
    * exact signed-64-bit count when representable and the single sentinel
    * otherwise. Every stored value has at most 19 decimal digits; every binary
    * operation forms at most 38 decimal digits before clamping. A fold is
    * linear in its number of inputs and uses constant carrier storage.
    */
  object SaturatingBigInt64 {
    val MaximumExactCount: BigInt = BigInt(Long.MaxValue)
    val OverflowSentinel: BigInt = MaximumExactCount + 1
    val MaximumCarrierDecimalDigits: Int =
      OverflowSentinel.toString.length
    val MaximumBinaryIntermediateDecimalDigits: Int =
      (OverflowSentinel * OverflowSentinel).toString.length

    val aggregateLoweringRequirements: SaturatingAggregateLoweringRequirements =
      SaturatingAggregateLoweringRequirements(
        cutoff = OverflowSentinel,
        mustClampEveryTransitionAddition = true,
        postAggregateClampIsSufficientForArbitraryGroups = false,
        supportedByCurrentBuiltInNumericSum = false
      )

    final case class NegativeMultiplicity(value: BigInt)
        extends Product
        with Serializable {
      def message: String = s"negative multiplicity: $value"
    }

    final class Value private[postgresql] (val value: BigInt)
        extends Serializable {
      override def equals(other: Any): Boolean = other match {
        case that: Value => value == that.value
        case _           => false
      }
      override def hashCode(): Int = value.hashCode()
      override def toString: String = s"SaturatingBigInt64($value)"
    }

    sealed trait Finalized extends Product with Serializable

    object Finalized {
      final case class Exact(value: Long) extends Finalized
      case object Overflow extends Finalized
    }

    final case class FoldResources private[postgresql] (
        inputCount: BigInt,
        binaryOperationCount: BigInt,
        maximumLiveCarrierValues: Int,
        maximumStoredDecimalDigits: Int,
        maximumBinaryIntermediateDecimalDigits: Int
    ) extends Serializable

    val zero: Value = unsafe(BigInt(0))
    val one: Value = unsafe(BigInt(1))

    def fromNonNegative(
        value: BigInt
    ): Either[NegativeMultiplicity, Value] =
      if (value < 0) Left(NegativeMultiplicity(value))
      else Right(unsafe(clamp(value)))

    def plus(left: Value, right: Value): Value = {
      require(left != null, "left saturating count must not be null")
      require(right != null, "right saturating count must not be null")
      unsafe(clamp(left.value + right.value))
    }

    def times(left: Value, right: Value): Value = {
      require(left != null, "left saturating count must not be null")
      require(right != null, "right saturating count must not be null")
      unsafe(clamp(left.value * right.value))
    }

    def sum(
        inputs: Iterable[BigInt]
    ): Either[NegativeMultiplicity, Value] =
      fold(inputs, zero, plus)

    def product(
        inputs: Iterable[BigInt]
    ): Either[NegativeMultiplicity, Value] =
      fold(inputs, one, times)

    def finalizeCount(value: Value): Finalized = {
      require(value != null, "saturating count must not be null")
      if (value.value == OverflowSentinel) Finalized.Overflow
      else Finalized.Exact(value.value.longValue)
    }

    def foldResources(inputCount: BigInt): FoldResources = {
      require(inputCount >= 0, "input count must not be negative")
      FoldResources(
        inputCount = inputCount,
        binaryOperationCount = (inputCount - 1).max(BigInt(0)),
        maximumLiveCarrierValues = if (inputCount <= 1) 1 else 2,
        maximumStoredDecimalDigits = MaximumCarrierDecimalDigits,
        maximumBinaryIntermediateDecimalDigits =
          MaximumBinaryIntermediateDecimalDigits
      )
    }

    private def fold(
        inputs: Iterable[BigInt],
        identity: Value,
        combine: (Value, Value) => Value
    ): Either[NegativeMultiplicity, Value] = {
      require(inputs != null, "multiplicity inputs must not be null")
      val iterator = inputs.iterator
      if (!iterator.hasNext) return Right(identity)
      var accumulator = fromNonNegative(iterator.next()) match {
        case Left(problem) => return Left(problem)
        case Right(value)  => value
      }
      while (iterator.hasNext) {
        fromNonNegative(iterator.next()) match {
          case Left(problem) => return Left(problem)
          case Right(value)  => accumulator = combine(accumulator, value)
        }
      }
      Right(accumulator)
    }

    private def clamp(value: BigInt): BigInt =
      if (value >= OverflowSentinel) OverflowSentinel else value

    private def unsafe(value: BigInt): Value = new Value(value)
  }

  final case class Bag private[postgresql] (
      bagId: BagId,
      vertices: Vector[VariableId],
      parent: Option[BagId],
      children: Vector[BagId],
      separatorToParent: Option[Vector[VariableId]],
      /**
        * Exact union of the parent separator (when present) and every child
        * separator. Grouping the local factor by these keys before joining
        * messages eliminates all variables that no neighboring message uses.
        */
      localGrouping: Vector[VariableId]
  ) extends Serializable

  /**
  * Additional rendered-term accounting for local weighted-factor aggregation
  * and message propagation. Physical set-support projections and the exact
  * full-bag frequency finalizations live in reduction CTEs and are accounted
  * by the PostgreSQL compiler. The root terminal aggregate is deliberately
  * excluded because it is already the terminal's one rendered term.
    */
  final case class ResourceTerms private[postgresql] (
      localAggregateTerms: BigInt,
      localKeyProjectionTerms: BigInt,
      localKeyGroupingTerms: BigInt,
      propagatedMessageAggregateTerms: BigInt,
      propagatedSeparatorProjectionTerms: BigInt,
      propagatedSeparatorGroupingTerms: BigInt,
      propagatedJoinEqualityTerms: BigInt
  ) extends Serializable {
    lazy val additionalRenderedTerms: BigInt =
      localAggregateTerms +
        localKeyProjectionTerms +
        localKeyGroupingTerms +
        propagatedMessageAggregateTerms +
        propagatedSeparatorProjectionTerms +
        propagatedSeparatorGroupingTerms +
        propagatedJoinEqualityTerms
  }

  /** `inputMessageOrdinals` are ordered by the source bag's child ids. */
  final case class Message private[postgresql] (
      ordinal: Int,
      source: BagId,
      target: BagId,
      separator: Vector[VariableId],
      inputMessageOrdinals: Vector[Int],
      /**
        * False when the locally grouped key already equals this separator. In
        * that case child messages are functional on their keys, so the message
        * can project its weighted local frequency without a redundant SUM and
        * GROUP BY.
        */
      requiresRegrouping: Boolean
  ) extends Serializable

  final case class RootFinalization private[postgresql] (
      root: BagId,
      inputMessageOrdinals: Vector[Int],
      /** Empty for scalar COUNT; one retained structural key for grouped COUNT. */
      groupingVariables: Vector[VariableId] = Vector.empty
  ) extends Serializable

  final case class Plan private[postgresql] (
      root: BagId,
      bags: Vector[Bag],
      postorder: Vector[BagId],
      messages: Vector[Message],
      finalization: RootFinalization,
      arithmetic: ArithmeticContract,
      resources: ResourceTerms
  ) extends Serializable {
    lazy val bagById: Map[BagId, Bag] =
      bags.iterator.map(bag => bag.bagId -> bag).toMap

    lazy val messageBySource: Map[BagId, Message] =
      messages.iterator.map(message => message.source -> message).toMap

    /**
      * Maximum number of recursively nested message subqueries on a path from
      * the terminal root to a leaf. Sibling messages do not increase it.
      * Canonical message ordinals are postorder, so this is stack-safe and
      * linear in the message program size.
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
  }

  sealed abstract class ErrorCode(val value: String)
      extends Product
      with Serializable

  object ErrorCode {
    case object MissingValue extends ErrorCode("missing-value")
    case object DuplicateBag extends ErrorCode("duplicate-bag")
    case object NonCanonicalBag extends ErrorCode("non-canonical-bag")
    case object UnknownArcEndpoint extends ErrorCode("unknown-arc-endpoint")
    case object DuplicateArcChild extends ErrorCode("duplicate-arc-child")
    case object SelfArc extends ErrorCode("self-arc")
    case object MissingRoot extends ErrorCode("missing-root")
    case object RootInvariant extends ErrorCode("root-invariant")
    case object ParentMismatch extends ErrorCode("parent-mismatch")
    case object ChildrenMismatch extends ErrorCode("children-mismatch")
    case object SeparatorMismatch extends ErrorCode("separator-mismatch")
    case object DepthMismatch extends ErrorCode("depth-mismatch")
    case object NonTree extends ErrorCode("non-tree")
    case object UnreachableBag extends ErrorCode("unreachable-bag")
    case object UnknownFinalizationRoot
        extends ErrorCode("unknown-finalization-root")
    case object RetainedVariableOutsideRoot
        extends ErrorCode("retained-variable-outside-root")
    case object CandidateMismatch extends ErrorCode("candidate-mismatch")
  }

  final case class Error(code: ErrorCode, detail: String)
      extends Product
      with Serializable {
    def message: String = s"${code.value}: $detail"
  }

  /**
    * Validates the structural frequency-propagation boundary and derives its
    * unique leaf-to-root message schedule. Input vector order is immaterial.
    */
  def derive(source: BagFactorPlan): Either[Vector[Error], Plan] = {
    if (source eq null) {
      Left(Vector(Error(ErrorCode.MissingValue, "bag-factor plan")))
    } else {
      validateSource(source) match {
        case problems if problems.nonEmpty => Left(problems)
        case _ => Right(build(source, source.root, Vector.empty))
      }
    }
  }

  /**
    * Reorients only the terminal message tree around a bag containing the
    * retained grouping key. The source plan's physical semijoin schedule is
    * deliberately unchanged.
    */
  def deriveGrouped(
      source: BagFactorPlan,
      finalizationRoot: BagId,
      groupVariable: VariableId
  ): Either[Vector[Error], Plan] = {
    if (source eq null) {
      Left(Vector(Error(ErrorCode.MissingValue, "bag-factor plan")))
    } else if (finalizationRoot eq null) {
      Left(Vector(Error(ErrorCode.MissingValue, "finalization root")))
    } else if (groupVariable eq null) {
      Left(Vector(Error(ErrorCode.MissingValue, "group variable")))
    } else {
      validateSource(source) match {
        case problems if problems.nonEmpty => Left(problems)
        case _ =>
          source.factors.find(_.bagId == finalizationRoot) match {
            case None =>
              Left(
                Vector(
                  Error(
                    ErrorCode.UnknownFinalizationRoot,
                    s"bag ${finalizationRoot.value} has no factor"
                  )
                )
              )
            case Some(factor) if !factor.vertices.contains(groupVariable) =>
              Left(
                Vector(
                  Error(
                    ErrorCode.RetainedVariableOutsideRoot,
                    s"variable ${groupVariable.value} is absent from bag ${finalizationRoot.value}"
                  )
                )
              )
            case Some(_) =>
              Right(build(source, finalizationRoot, Vector(groupVariable)))
          }
      }
    }
  }

  /**
    * Independently re-derives the canonical model and compares every field.
    * This is useful at a future renderer boundary where a cached or transported
    * message plan must be rejected if it no longer matches its bag-factor plan.
    */
  def validate(
      source: BagFactorPlan,
      candidate: Plan
  ): Either[Vector[Error], Plan] =
    derive(source) match {
      case Left(problems) => Left(problems)
      case Right(expected) if candidate eq null =>
        Left(Vector(Error(ErrorCode.MissingValue, "frequency-propagation plan")))
      case Right(expected) => compare(expected, candidate)
    }

  def validateGrouped(
      source: BagFactorPlan,
      finalizationRoot: BagId,
      groupVariable: VariableId,
      candidate: Plan
  ): Either[Vector[Error], Plan] =
    deriveGrouped(source, finalizationRoot, groupVariable) match {
      case Left(problems) => Left(problems)
      case Right(_) if candidate eq null =>
        Left(Vector(Error(ErrorCode.MissingValue, "frequency-propagation plan")))
      case Right(expected) => compare(expected, candidate)
    }

  private def compare(
      expected: Plan,
      candidate: Plan
  ): Either[Vector[Error], Plan] = {
    val problems = Vector.newBuilder[Error]
    if (candidate.root != expected.root)
      problems += mismatch("root")
    if (candidate.bags != expected.bags)
      problems += mismatch("bags")
    if (candidate.postorder != expected.postorder)
      problems += mismatch("postorder")
    if (candidate.messages != expected.messages)
      problems += mismatch("messages")
    if (candidate.finalization != expected.finalization)
      problems += mismatch("finalization")
    if (candidate.arithmetic != expected.arithmetic)
      problems += mismatch("arithmetic")
    if (candidate.resources != expected.resources)
      problems += mismatch("resources")
    val result = canonical(problems.result())
    if (result.nonEmpty) Left(result) else Right(candidate)
  }

  private def validateSource(source: BagFactorPlan): Vector[Error] = {
    val shapeErrors = Vector.newBuilder[Error]

    if (source.root eq null)
      shapeErrors += Error(ErrorCode.MissingValue, "root")
    if (source.factors eq null)
      shapeErrors += Error(ErrorCode.MissingValue, "factors")
    if (source.arcs eq null)
      shapeErrors += Error(ErrorCode.MissingValue, "arcs")

    if (source.factors ne null) {
      source.factors.zipWithIndex.foreach { case (factor, index) =>
        if (factor eq null) {
          shapeErrors += Error(ErrorCode.MissingValue, s"factors[$index]")
        } else {
          if (factor.bagId eq null)
            shapeErrors += Error(
              ErrorCode.MissingValue,
              s"factors[$index].bagId"
            )
          if (factor.vertices eq null)
            shapeErrors += Error(
              ErrorCode.MissingValue,
              s"factors[$index].vertices"
            )
          else if (factor.vertices.exists(_ eq null))
            shapeErrors += Error(
              ErrorCode.MissingValue,
              s"factors[$index].vertices contains null"
            )
          else if (
            factor.vertices.isEmpty ||
            factor.vertices != factor.vertices.distinct.sorted
          )
            shapeErrors += Error(
              ErrorCode.NonCanonicalBag,
              s"bag ${bagLabel(factor.bagId)} has empty, duplicate, or unsorted vertices"
            )

          if (factor.parent eq null)
            shapeErrors += Error(
              ErrorCode.MissingValue,
              s"factors[$index].parent"
            )
          else if (factor.parent.exists(_ eq null))
            shapeErrors += Error(
              ErrorCode.MissingValue,
              s"factors[$index].parent contains null"
            )

          if (factor.children eq null)
            shapeErrors += Error(
              ErrorCode.MissingValue,
              s"factors[$index].children"
            )
          else if (factor.children.exists(_ eq null))
            shapeErrors += Error(
              ErrorCode.MissingValue,
              s"factors[$index].children contains null"
            )
          else if (factor.children != factor.children.distinct.sorted)
            shapeErrors += Error(
              ErrorCode.NonCanonicalBag,
              s"bag ${bagLabel(factor.bagId)} has duplicate or unsorted children"
            )

          if (factor.depth < 0)
            shapeErrors += Error(
              ErrorCode.DepthMismatch,
              s"bag ${bagLabel(factor.bagId)} has negative depth ${factor.depth}"
            )
        }
      }
    }

    if (source.arcs ne null) {
      source.arcs.zipWithIndex.foreach { case (arc, index) =>
        if (arc eq null) {
          shapeErrors += Error(ErrorCode.MissingValue, s"arcs[$index]")
        } else {
          if (arc.parent eq null)
            shapeErrors += Error(ErrorCode.MissingValue, s"arcs[$index].parent")
          if (arc.child eq null)
            shapeErrors += Error(ErrorCode.MissingValue, s"arcs[$index].child")
          if (arc.separator eq null)
            shapeErrors += Error(
              ErrorCode.MissingValue,
              s"arcs[$index].separator"
            )
          else if (arc.separator.exists(_ eq null))
            shapeErrors += Error(
              ErrorCode.MissingValue,
              s"arcs[$index].separator contains null"
            )
          else if (arc.separator != arc.separator.distinct.sorted)
            shapeErrors += Error(
              ErrorCode.SeparatorMismatch,
              s"arc $index has duplicate or unsorted separator variables"
            )
        }
      }
    }

    val initial = canonical(shapeErrors.result())
    if (initial.nonEmpty) initial else validateRelationships(source)
  }

  private def validateRelationships(source: BagFactorPlan): Vector[Error] = {
    val problems = Vector.newBuilder[Error]
    val factors = source.factors
    val duplicateBagIds = factors
      .groupBy(_.bagId)
      .collect { case (bagId, occurrences) if occurrences.size != 1 => bagId }
      .toVector
      .sorted

    duplicateBagIds.foreach(bagId =>
      problems += Error(
        ErrorCode.DuplicateBag,
        s"bag ${bagId.value} occurs more than once"
      )
    )
    if (factors.isEmpty || !factors.exists(_.bagId == source.root))
      problems += Error(
        ErrorCode.MissingRoot,
        s"root ${source.root.value} has no factor"
      )

    val early = canonical(problems.result())
    if (early.nonEmpty) return early

    val factorById = factors.iterator.map(factor => factor.bagId -> factor).toMap
    val arcs = source.arcs

    arcs.zipWithIndex.foreach { case (arc, index) =>
      if (!factorById.contains(arc.parent))
        problems += Error(
          ErrorCode.UnknownArcEndpoint,
          s"arc $index has unknown parent ${arc.parent.value}"
        )
      if (!factorById.contains(arc.child))
        problems += Error(
          ErrorCode.UnknownArcEndpoint,
          s"arc $index has unknown child ${arc.child.value}"
        )
      if (arc.parent == arc.child)
        problems += Error(
          ErrorCode.SelfArc,
          s"bag ${arc.child.value} is both ends of an arc"
        )
    }

    val knownArcs = arcs.filter(arc =>
      factorById.contains(arc.parent) && factorById.contains(arc.child)
    )
    knownArcs
      .groupBy(_.child)
      .collect { case (child, incoming) if incoming.size > 1 => child }
      .toVector
      .sorted
      .foreach(child =>
        problems += Error(
          ErrorCode.DuplicateArcChild,
          s"bag ${child.value} has more than one incoming arc"
        )
      )

    val incomingByChild = knownArcs.groupBy(_.child)
    val childrenByParent = knownArcs
      .groupBy(_.parent)
      .iterator
      .map { case (parent, outgoing) =>
        parent -> outgoing.iterator.map(_.child).toVector.sorted
      }
      .toMap

    if (arcs.size != math.max(0, factors.size - 1))
      problems += Error(
        ErrorCode.NonTree,
        s"expected ${math.max(0, factors.size - 1)} arcs but found ${arcs.size}"
      )

    factors.sortBy(_.bagId).foreach { factor =>
      val incoming = incomingByChild.getOrElse(factor.bagId, Vector.empty)
      val expectedParent = incoming.headOption.map(_.parent)
      if (factor.bagId == source.root) {
        if (factor.parent.nonEmpty || incoming.nonEmpty || factor.depth != 0)
          problems += Error(
            ErrorCode.RootInvariant,
            s"root ${factor.bagId.value} must have no parent, no incoming arc, and depth zero"
          )
      } else {
        if (incoming.size != 1 || factor.parent != expectedParent)
          problems += Error(
            ErrorCode.ParentMismatch,
            s"bag ${factor.bagId.value} parent metadata does not match exactly one incoming arc"
          )
      }

      val expectedChildren =
        childrenByParent.getOrElse(factor.bagId, Vector.empty)
      if (factor.children != expectedChildren)
        problems += Error(
          ErrorCode.ChildrenMismatch,
          s"bag ${factor.bagId.value} children do not match outgoing arcs"
        )
    }

    knownArcs.foreach { arc =>
      val parent = factorById(arc.parent)
      val child = factorById(arc.child)
      val childVertices = child.vertices.toSet
      val expectedSeparator = parent.vertices.filter(childVertices.contains)
      if (arc.separator != expectedSeparator)
        problems += Error(
          ErrorCode.SeparatorMismatch,
          s"arc ${arc.parent.value}->${arc.child.value} separator is not the exact bag intersection"
        )
      if (child.depth != parent.depth + 1)
        problems += Error(
          ErrorCode.DepthMismatch,
          s"arc ${arc.parent.value}->${arc.child.value} does not increase depth by one"
        )
    }

    val relationshipErrors = canonical(problems.result())
    if (relationshipErrors.nonEmpty) relationshipErrors
    else {
      val reached = mutable.HashSet.empty[BagId]
      val pending = mutable.Stack(source.root)
      while (pending.nonEmpty) {
        val current = pending.pop()
        if (!reached(current)) {
          reached += current
          childrenByParent
            .getOrElse(current, Vector.empty)
            .reverseIterator
            .foreach(child => pending.push(child))
        }
      }
      val missing = (factorById.keySet -- reached).toVector.sorted
      if (missing.nonEmpty)
        Vector(
          Error(
            ErrorCode.UnreachableBag,
            s"root cannot reach bags ${missing.map(_.value).mkString(",")}"
          )
        )
      else Vector.empty
    }
  }

  private def build(
      source: BagFactorPlan,
      finalizationRoot: BagId,
      retainedRootVariables: Vector[VariableId]
  ): Plan = {
    val factorById = source.factors.iterator
      .map(factor => factor.bagId -> factor)
      .toMap

    val neighborBuffers = mutable.Map.empty[
      BagId,
      mutable.ArrayBuffer[(BagId, Vector[VariableId])]
    ]
    factorById.keysIterator.foreach(bagId =>
      neighborBuffers.put(bagId, mutable.ArrayBuffer.empty)
    )
    source.arcs.foreach { arc =>
      neighborBuffers(arc.parent) += (arc.child -> arc.separator)
      neighborBuffers(arc.child) += (arc.parent -> arc.separator)
    }
    val neighbors = neighborBuffers.iterator.map { case (bagId, values) =>
      bagId -> values.sortBy(_._1).toVector
    }.toMap

    val parentByBag = mutable.Map.empty[BagId, BagId]
    val separatorToParent = mutable.Map.empty[BagId, Vector[VariableId]]
    val childrenByBag = mutable.Map.empty[BagId, Vector[BagId]]
    val orient = mutable.Stack[(BagId, Option[BagId])](
      finalizationRoot -> None
    )
    while (orient.nonEmpty) {
      val (current, parent) = orient.pop()
      val children = neighbors(current).collect {
        case (neighbor, separator) if !parent.contains(neighbor) =>
          parentByBag.put(neighbor, current)
          separatorToParent.put(neighbor, separator)
          neighbor
      }.sorted
      childrenByBag.put(current, children)
      children.reverseIterator.foreach(child =>
        orient.push(child -> Some(current))
      )
    }

    val postorderBuilder = Vector.newBuilder[BagId]
    val pending = mutable.Stack[(BagId, Boolean)](finalizationRoot -> false)
    while (pending.nonEmpty) {
      val (current, expanded) = pending.pop()
      if (expanded) postorderBuilder += current
      else {
        pending.push(current -> true)
        childrenByBag(current).reverseIterator.foreach(child =>
          pending.push(child -> false)
        )
      }
    }
    val postorder = postorderBuilder.result()

    val messages = Vector.newBuilder[Message]
    val messageOrdinalBySource = mutable.Map.empty[BagId, Int]
    postorder.filterNot(_ == finalizationRoot).foreach { sourceBagId =>
      val ordinal = messageOrdinalBySource.size
      val children = childrenByBag(sourceBagId)
      val inputOrdinals = children.map(messageOrdinalBySource)
      messages += Message(
        ordinal = ordinal,
        source = sourceBagId,
        target = parentByBag(sourceBagId),
        separator = separatorToParent(sourceBagId),
        inputMessageOrdinals = inputOrdinals,
        requiresRegrouping = {
          val parentSeparator = separatorToParent(sourceBagId)
          val childSeparators = children.iterator.flatMap(separatorToParent)
          (parentSeparator.iterator ++ childSeparators).toVector.distinct.sorted !=
            parentSeparator
        }
      )
      messageOrdinalBySource.put(sourceBagId, ordinal)
    }
    val messageResult = messages.result()

    val bags = source.factors.sortBy(_.bagId).map { factor =>
      val parent = parentByBag.get(factor.bagId)
      val children = childrenByBag(factor.bagId)
      val parentSeparator = parent.iterator.flatMap(_ =>
        separatorToParent(factor.bagId)
      )
      val childSeparators = children.iterator.flatMap(separatorToParent)
      val retained =
        if (factor.bagId == finalizationRoot) retainedRootVariables.iterator
        else Iterator.empty
      Bag(
        bagId = factor.bagId,
        vertices = factor.vertices,
        parent = parent,
        children = children,
        separatorToParent = parent.map(_ => separatorToParent(factor.bagId)),
        localGrouping =
          (parentSeparator ++ childSeparators ++ retained).toVector.distinct.sorted
      )
    }
    val rootInputs = childrenByBag(finalizationRoot).map(messageOrdinalBySource)

    val localKeyTerms = bags.iterator.foldLeft(BigInt(0)) { (total, bag) =>
      total + bag.localGrouping.size
    }
    val separatorTerms = messageResult.iterator.foldLeft(BigInt(0)) {
      (total, message) => total + message.separator.size
    }
    val regroupingSeparatorTerms = messageResult.iterator
      .filter(_.requiresRegrouping)
      .foldLeft(BigInt(0)) { (total, message) =>
        total + message.separator.size
      }
    val resources = ResourceTerms(
      localAggregateTerms = BigInt(bags.size),
      localKeyProjectionTerms = localKeyTerms,
      localKeyGroupingTerms = localKeyTerms,
      propagatedMessageAggregateTerms =
        BigInt(messageResult.count(_.requiresRegrouping)),
      propagatedSeparatorProjectionTerms = separatorTerms,
      propagatedSeparatorGroupingTerms = regroupingSeparatorTerms,
      propagatedJoinEqualityTerms = separatorTerms
    )

    Plan(
      root = finalizationRoot,
      bags = bags,
      postorder = postorder,
      messages = messageResult,
      finalization = RootFinalization(
        finalizationRoot,
        rootInputs,
        retainedRootVariables
      ),
      arithmetic = ExactCountArithmetic,
      resources = resources
    )
  }

  private def mismatch(field: String): Error =
    Error(ErrorCode.CandidateMismatch, s"non-canonical $field")

  private def bagLabel(bagId: BagId): String =
    if (bagId eq null) "<null>" else bagId.value.toString

  private def canonical(errors: Vector[Error]): Vector[Error] =
    errors.distinct.sortBy(error => (error.code.value, error.detail))
}
