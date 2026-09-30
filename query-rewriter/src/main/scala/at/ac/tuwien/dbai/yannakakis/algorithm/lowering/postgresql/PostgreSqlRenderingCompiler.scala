package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactor,
  BagFactorPlan,
  SemijoinPass,
  SemijoinStep
}
import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  ColumnId,
  EdgeId,
  Hyperedge,
  RelationOccurrenceId,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  C03G0AggregateVectorIR,
  CalciteLocalPredicates,
  ColumnMetadata,
  ColumnRef,
  DistributedGroupedCountStarIR,
  GlobalCountStarIR,
  GlobalGuardedExtremumFunction,
  GlobalGuardedExtremumIR,
  GroupedCountStarIR,
  PredicateClassification,
  PredicateDescriptor,
  QueryIR,
  QueryResultShape,
  RelationOccurrence,
  ScalarExpression
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCteMaterializationDirective,
  PostgreSqlPhysicalVariantId
}

import java.nio.charset.StandardCharsets
import scala.collection.mutable

private[postgresql] final case class PostgreSqlBindingLocation(
    edgeId: EdgeId,
    columnId: ColumnId,
    variableId: VariableId
)

/** Exact source-column demand for one structural relation occurrence. */
private[postgresql] final case class PostgreSqlOccurrenceLiveness(
    edge: Hyperedge,
    relation: RelationOccurrence,
    scanColumns: Vector[ColumnMetadata],
    carriedColumns: Vector[ColumnMetadata]
)

/** Additive compiler-private trace of one physical initial-factor schema. */
private[postgresql] final case class PostgreSqlPhysicalFactorProjectionV2(
    structuralVariables: Vector[VariableId],
    occurrenceColumns: Vector[PostgreSqlOccurrenceColumnTrace],
    unitColumn: Option[String]
)

/** Additive compiler-private provenance for one renderer-v2 factor boundary. */
private[postgresql] final case class PostgreSqlAggregateRendererV2FactorTrace(
    bagId: BagId,
    structuralVariables: Vector[VariableId],
    occurrenceColumns: Vector[PostgreSqlOccurrenceColumnTrace],
    unitColumn: Option[String]
)

/** Additive compiler-private result; frozen public render traces stay unchanged. */
private[postgresql] final case class PostgreSqlAggregateRendererV2Trace(
    requestedProfile: PostgreSqlAggregateRenderingProfileV2,
    factorProjections: Vector[PostgreSqlAggregateRendererV2FactorTrace],
    elidedLogicalMessageOrdinals: Set[Int]
)

private[postgresql] final case class CompiledPostgreSqlBag(
    factor: BagFactor,
    physicalProjection: PostgreSqlPhysicalFactorProjectionV2,
    supportStrategy: PostgreSqlBagSupportStrategy,
    supportEdges: Vector[Hyperedge],
    ownedOccurrences: Vector[PostgreSqlOccurrenceLiveness],
    supportRepresentatives: Map[VariableId, PostgreSqlBindingLocation],
    supportEqualities: Vector[(PostgreSqlBindingLocation, PostgreSqlBindingLocation)],
    ownedRepresentatives: Map[VariableId, PostgreSqlBindingLocation],
    ownedEqualities: Vector[(PostgreSqlBindingLocation, PostgreSqlBindingLocation)]
)

private[postgresql] sealed trait CompiledPostgreSqlScalar
    extends Product
    with Serializable {
  def columnRefs: Vector[ColumnRef]
  def renderedTermCount: Int = 1
}

private[postgresql] object CompiledPostgreSqlScalar {
  final case class LocalPredicate(tree: CalciteLocalPredicates.Tree[ColumnRef])
      extends CompiledPostgreSqlScalar {
    override val columnRefs: Vector[ColumnRef] = tree.columns.distinct.sorted
    override def renderedTermCount: Int = tree.termCount
  }
  final case class Column(ref: ColumnRef) extends CompiledPostgreSqlScalar {
    override val columnRefs: Vector[ColumnRef] = Vector(ref)
  }
  final case class Equality(left: ColumnRef, right: ColumnRef)
      extends CompiledPostgreSqlScalar {
    override val columnRefs: Vector[ColumnRef] =
      Vector(left, right).distinct.sorted
  }
  final case class LocalIntegralEquality(
      column: ColumnRef,
      columnSourceSqlTypeName: String,
      columnTargetSqlTypeName: String,
      literalValue: Long,
      literalSourceSqlTypeName: String,
      literalTargetSqlTypeName: String,
      literalOnLeft: Boolean
  ) extends CompiledPostgreSqlScalar {
    require(column != null, "local integral equality column must not be null")
    require(
      columnSourceSqlTypeName != null && columnTargetSqlTypeName != null,
      "local integral equality column types must not be null"
    )
    require(
      literalSourceSqlTypeName != null && literalTargetSqlTypeName != null,
      "local integral equality literal types must not be null"
    )
    override val columnRefs: Vector[ColumnRef] = Vector(column)
  }
}

/**
  * Terminal result construction. A fully reduced guarded extremum reads its
  * guard-owner factor; partial modes use terminal-local staged base support.
  */
private[postgresql] sealed trait CompiledPostgreSqlTerminal
    extends Product
    with Serializable {
  def outputScalars: Vector[CompiledPostgreSqlScalar]
  def renderedTermCount: Int
  def reconstructsFactors: Boolean
  def inputBagIds(plan: BagFactorPlan): Vector[BagId]
  def usesStagedCountFrequency(plan: BagFactorPlan): Boolean = false
}

private[postgresql] object CompiledPostgreSqlTerminal {
  private def aggregateInputBagIds(
      root: BagId,
      children: BagId => Vector[BagId]
  ): Vector[BagId] = {
    val result = Vector.newBuilder[BagId]
    val pending = mutable.Stack(root)
    while (pending.nonEmpty) {
      val current = pending.pop()
      result += current
      children(current).reverseIterator.foreach(child => pending.push(child))
    }
    result.result()
  }

  sealed trait GlobalCountStrategy extends Product with Serializable {
    def reconstructsFactors: Boolean
  }

  object GlobalCountStrategy {
    final case class FrequencyPropagation(
        plan: PostgreSqlFrequencyPropagation.Plan
    ) extends GlobalCountStrategy {
      require(plan != null, "COUNT(*) frequency plan must not be null")
      override val reconstructsFactors: Boolean = false
    }

    /**
      * Development-only leaf-to-root propagation over raw initial factors.
      * Unlike [[FrequencyPropagation]], this strategy installs no frequency
      * in a reduction CTE; the emitter joins child messages to each raw factor
      * before grouping it into the next message.
      */
    final case class RawFrequencyPropagation(
        plan: PostgreSqlFrequencyPropagation.Plan
    ) extends GlobalCountStrategy {
      require(plan != null, "raw COUNT(*) frequency plan must not be null")
      override val reconstructsFactors: Boolean = false
    }

    case object Reconstruction extends GlobalCountStrategy {
      override val reconstructsFactors: Boolean = true
    }
  }

  final case class Projection(
      outputScalars: Vector[CompiledPostgreSqlScalar]
  ) extends CompiledPostgreSqlTerminal {
    require(outputScalars != null, "projection output scalars must not be null")
    require(
      outputScalars.forall(_ != null),
      "projection output scalars must not contain null"
    )
    override val renderedTermCount: Int = outputScalars.size
    override val reconstructsFactors: Boolean = true
    override def inputBagIds(plan: BagFactorPlan): Vector[BagId] =
      plan.root +: plan.arcs.map(_.child)
  }

  final case class GlobalCountStar(
      output: PostgreSqlOutputColumn,
      strategy: GlobalCountStrategy
  )
      extends CompiledPostgreSqlTerminal {
    require(output != null, "global COUNT(*) output must not be null")
    require(strategy != null, "global COUNT(*) strategy must not be null")
    require(output.ordinal == 0, "global COUNT(*) output ordinal must be zero")
    require(
      output.dataType == "BIGINT NOT NULL" && !output.nullable,
      "global COUNT(*) output must be BIGINT NOT NULL"
    )
    override val outputScalars: Vector[CompiledPostgreSqlScalar] = Vector.empty
    override val renderedTermCount: Int = 1
    override val reconstructsFactors: Boolean = strategy.reconstructsFactors
    override def inputBagIds(plan: BagFactorPlan): Vector[BagId] =
      plan.root +: plan.arcs.map(_.child)
    override def usesStagedCountFrequency(plan: BagFactorPlan): Boolean =
      plan.arcs.nonEmpty && strategy.isInstanceOf[
        GlobalCountStrategy.FrequencyPropagation
      ]
  }

  final case class GroupedCountStar(
      groupColumn: CompiledPostgreSqlScalar.Column,
      groupVariable: VariableId,
      groupOutput: PostgreSqlOutputColumn,
      countOutput: PostgreSqlOutputColumn,
      aggregateBagId: BagId,
      frequencyPlan: PostgreSqlFrequencyPropagation.Plan
  ) extends CompiledPostgreSqlTerminal {
    require(groupColumn != null, "grouped COUNT(*) key must not be null")
    require(groupVariable != null, "grouped COUNT(*) variable must not be null")
    require(groupOutput != null, "grouped COUNT(*) key output must not be null")
    require(countOutput != null, "grouped COUNT(*) count output must not be null")
    require(aggregateBagId != null, "grouped COUNT(*) root bag must not be null")
    require(frequencyPlan != null, "grouped COUNT(*) frequency plan must not be null")
    require(groupOutput.ordinal == 0, "grouped COUNT(*) key ordinal must be zero")
    require(countOutput.ordinal == 1, "grouped COUNT(*) count ordinal must be one")
    require(
      countOutput.dataType == "BIGINT NOT NULL" && !countOutput.nullable,
      "grouped COUNT(*) count must be BIGINT NOT NULL"
    )
    require(
      frequencyPlan.root == aggregateBagId &&
        frequencyPlan.finalization.groupingVariables == Vector(groupVariable),
      "grouped COUNT(*) frequency root must retain exactly its grouping key"
    )
    override val outputScalars: Vector[CompiledPostgreSqlScalar] =
      Vector(groupColumn)
    override val renderedTermCount: Int = 3
    override val reconstructsFactors: Boolean = false
    override def inputBagIds(plan: BagFactorPlan): Vector[BagId] =
      aggregateInputBagIds(
        frequencyPlan.root,
        bagId => frequencyPlan.bagById(bagId).children
      )
    override def usesStagedCountFrequency(plan: BagFactorPlan): Boolean =
      plan.arcs.nonEmpty
  }

  /**
    * Exact distributed grouped COUNT(*) whose structural grouping variables are
    * supplied by distinct relation occurrences and need not share a guard.
    * The SQL-free aggregate-propagation plan carries each key independently;
    * only ordinary decomposition separators become join predicates.
    */
  final case class DistributedGroupedCountStar(
      groupColumns: Vector[CompiledPostgreSqlScalar.Column],
      groupVariables: Vector[VariableId],
      groupOutputs: Vector[PostgreSqlOutputColumn],
      countOutput: PostgreSqlOutputColumn,
      aggregateBagId: BagId,
      propagationPlan: PostgreSqlAggregatePropagation.Plan
  ) extends CompiledPostgreSqlTerminal {
    require(
      groupColumns != null && groupColumns.size >= 2 &&
        groupColumns.forall(_ != null),
      "distributed grouped COUNT(*) must have at least two compiled group columns"
    )
    require(
      groupColumns.map(_.ref.relationOccurrenceId).distinct.size == groupColumns.size,
      "distributed grouped COUNT(*) columns must retain distinct occurrence identity"
    )
    require(
      groupVariables != null && groupVariables.size == groupColumns.size &&
        groupVariables.distinct.size == groupColumns.size,
      "distributed grouped COUNT(*) keys must bind distinct structural variables"
    )
    require(
      groupOutputs != null && groupOutputs.size == groupColumns.size &&
        groupOutputs.forall(_ != null),
      "distributed grouped COUNT(*) output arity must match its keys"
    )
    require(
      groupOutputs.map(_.ordinal) == groupOutputs.indices.toVector,
      "distributed grouped COUNT(*) group outputs must be positional"
    )
    require(
      countOutput != null && countOutput.ordinal == groupColumns.size,
      "distributed grouped COUNT(*) count output must follow its keys"
    )
    require(
      countOutput.dataType == "BIGINT NOT NULL" && !countOutput.nullable,
      "distributed grouped COUNT(*) count must be BIGINT NOT NULL"
    )
    require(
      aggregateBagId != null && propagationPlan != null &&
        propagationPlan.root == aggregateBagId,
      "distributed grouped COUNT(*) propagation root must match its aggregate bag"
    )
    require(
      propagationPlan.finalization.groupKeys == groupVariables.map(
        PostgreSqlAggregatePropagation.AggregateKey.Structural
      ),
      "distributed grouped COUNT(*) propagation must retain all keys in output order"
    )
    require(
      propagationPlan.laneBindings.isEmpty,
      "distributed grouped COUNT(*) must not contain measure lanes"
    )

    override val outputScalars: Vector[CompiledPostgreSqlScalar] = groupColumns
    // Key projections plus the checked COUNT aggregate. The propagation model
    // accounts for terminal GROUP BY terms independently.
    override val renderedTermCount: Int = groupColumns.size + 1
    override val reconstructsFactors: Boolean = false
    override def inputBagIds(plan: BagFactorPlan): Vector[BagId] =
      aggregateInputBagIds(
        propagationPlan.root,
        bagId => propagationPlan.bagById(bagId).children
      )
    override def usesStagedCountFrequency(plan: BagFactorPlan): Boolean =
      plan.arcs.nonEmpty
  }

  /**
    * Exact C03-G0 terminal. Both payload columns remain owner-local: legacy
    * rendering reads the raw fully reduced factor, while compact rendering
    * replaces it with equivalent owner-local G/frequency/MIN/MAX state. Only
    * count annotations traverse message arcs.
    */
  final case class C03G0AggregateVector(
      groupColumn: CompiledPostgreSqlScalar.Column,
      measureColumn: CompiledPostgreSqlScalar.Column,
      outputs: Vector[PostgreSqlOutputColumn],
      aggregateBagId: BagId,
      propagationPlan: PostgreSqlAggregatePropagation.Plan
  ) extends CompiledPostgreSqlTerminal {
    require(groupColumn != null, "C03-G0 group column must not be null")
    require(measureColumn != null, "C03-G0 measure column must not be null")
    require(
      groupColumn.ref.relationOccurrenceId ==
        measureColumn.ref.relationOccurrenceId,
      "C03-G0 payload columns must come from one occurrence"
    )
    require(
      outputs != null && outputs.size == 5 && outputs.forall(_ != null),
      "C03-G0 must have exactly five outputs"
    )
    require(
      outputs.map(_.ordinal) == Vector(0, 1, 2, 3, 4),
      "C03-G0 outputs must be positional"
    )
    require(
      outputs.slice(1, 3).forall(output =>
        output.dataType == "BIGINT NOT NULL" && !output.nullable
      ),
      "C03-G0 count outputs must be BIGINT NOT NULL"
    )
    require(
      outputs(0).nullable && outputs.slice(3, 5).forall(output =>
        output.dataType == "BIGINT NOT NULL" && !output.nullable
      ),
      "C03-G0 group and extrema nullability/types must match the fixed profile"
    )
    require(
      aggregateBagId != null && propagationPlan != null &&
        propagationPlan.root == aggregateBagId,
      "C03-G0 propagation root must match its aggregate bag"
    )
    private val groupKey =
      PostgreSqlAggregatePropagation.AggregateKey.OccurrenceValue(
        groupColumn.ref.id
      )
    require(
      propagationPlan.finalization.root == aggregateBagId &&
        propagationPlan.finalization.groupKeys == Vector(groupKey),
      "C03-G0 propagation must finalize exactly its occurrence-valued group key at the owner"
    )
    require(
      propagationPlan.keyBindings.size == 1 &&
        propagationPlan.keyBindings.head.key == groupKey &&
        propagationPlan.keyBindings.head.owner == aggregateBagId,
      "C03-G0 propagation must bind exactly its group key at the owner"
    )
    require(
      propagationPlan.laneBindings.isEmpty &&
        propagationPlan.bags.forall(_.ownedLanes.isEmpty),
      "C03-G0 propagation must not contain measure lanes"
    )
    require(
      propagationPlan.messages.forall(_.carriedKeys.isEmpty),
      "C03-G0 payload keys must not cross message arcs"
    )
    require(
      propagationPlan.bagById
        .get(aggregateBagId)
        .exists(_.ownedKeys == Vector(groupKey)) &&
        propagationPlan.bags
          .filterNot(_.bagId == aggregateBagId)
          .forall(_.ownedKeys.isEmpty),
      "C03-G0 group-key ownership must be owner-local"
    )

    override val outputScalars: Vector[CompiledPostgreSqlScalar] =
      Vector(groupColumn, measureColumn)
    override val renderedTermCount: Int = 5
    override val reconstructsFactors: Boolean = false
    override def inputBagIds(plan: BagFactorPlan): Vector[BagId] =
      aggregateInputBagIds(
        propagationPlan.root,
        bagId => propagationPlan.bagById(bagId).children
      )
  }

  final case class GlobalGuardedExtremum(
      function: GlobalGuardedExtremumFunction,
      argument: CompiledPostgreSqlScalar.Column,
      output: PostgreSqlOutputColumn,
      aggregateBagId: BagId,
      usesFullyReducedGuardFactor: Boolean,
      semijoinPlan: PostgreSqlGuardedExtremumSemijoin.Plan
  ) extends CompiledPostgreSqlTerminal {
    require(function != null, "guarded extremum function must not be null")
    require(argument != null, "guarded extremum argument must not be null")
    require(output != null, "guarded extremum output must not be null")
    require(aggregateBagId != null, "guarded extremum bag must not be null")
    require(
      semijoinPlan != null,
      "guarded extremum semijoin plan must not be null"
    )
    require(output.ordinal == 0, "guarded extremum output ordinal must be zero")
    require(output.nullable, "guarded extremum output must be nullable")
    override val outputScalars: Vector[CompiledPostgreSqlScalar] =
      Vector(argument)
    override val renderedTermCount: Int = 1
    override val reconstructsFactors: Boolean = false
    override def inputBagIds(plan: BagFactorPlan): Vector[BagId] =
      Vector(aggregateBagId)
  }
}

private[postgresql] final case class CompiledPostgreSqlPredicate(
    ordinal: Int,
    scalar: CompiledPostgreSqlScalar
)

/** One logical semijoin predicate inside a physical reduction CTE. */
private[postgresql] final case class CompiledPostgreSqlReductionMember(
    step: SemijoinStep,
    trace: PostgreSqlSemijoinCteTrace
)

/**
  * Aggregate state, if any, installed at the unique reduction where one bag
  * first becomes globally extendible. The variants deliberately carry their
  * exact grouping signature so both the compiler validator and emitter can
  * reject a stale or over-narrow state instead of inferring columns from a
  * Boolean marker.
  */
private[postgresql] sealed trait CompiledPostgreSqlReductionFinalization
    extends Product
    with Serializable {
  def finalizesCountFrequency: Boolean
}

private[postgresql] object CompiledPostgreSqlReductionFinalization {
  /** Preserve the reduction target's raw rows and complete projected schema. */
  case object RetainRawRows
      extends CompiledPostgreSqlReductionFinalization {
    override val finalizesCountFrequency: Boolean = false
  }

  /**
    * Frozen v1 behavior: group by every structural vertex and add one exact
    * local frequency. Factor vertices are nonempty at this boundary.
    */
  final case class LegacyFullVerticesFrequency(
      groupingVariables: Vector[VariableId]
  ) extends CompiledPostgreSqlReductionFinalization {
    require(
      groupingVariables != null && groupingVariables.nonEmpty &&
        groupingVariables == groupingVariables.distinct.sorted,
      "legacy frequency grouping must be nonempty, distinct, and sorted"
    )
    override val finalizesCountFrequency: Boolean = true
  }

  /**
    * Compact zero-lane COUNT state. An empty grouping needs an explicit
    * positive-input guard in SQL; otherwise scalar aggregation would invent a
    * zero-frequency support row on empty input.
    */
  final case class CompactFrequency(
      groupingVariables: Vector[VariableId],
      suppressEmptyGroup: Boolean
  ) extends CompiledPostgreSqlReductionFinalization {
    require(
      groupingVariables != null &&
        groupingVariables == groupingVariables.distinct.sorted,
      "compact frequency grouping must be distinct and sorted"
    )
    require(
      suppressEmptyGroup == groupingVariables.isEmpty,
      "compact scalar frequency must suppress its invented empty group exactly when its grouping is empty"
    )
    override val finalizesCountFrequency: Boolean = true
  }

  /**
    * Compact C03 owner state. Structural grouping variables retain every
    * adjacent separator; the exact owner-local payload G is an additional SQL
    * group key, while X is converted to local MIN/MAX state before it can be
    * discarded.
    */
  final case class CompactC03Owner(
      groupingVariables: Vector[VariableId],
      groupColumn: ColumnId,
      measureColumn: ColumnId
  ) extends CompiledPostgreSqlReductionFinalization {
    require(
      groupingVariables != null &&
        groupingVariables == groupingVariables.distinct.sorted,
      "compact C03 structural grouping must be distinct and sorted"
    )
    require(groupColumn != null, "compact C03 group column must not be null")
    require(measureColumn != null, "compact C03 measure column must not be null")
    require(
      groupColumn.relationOccurrenceId == measureColumn.relationOccurrenceId,
      "compact C03 payload columns must have one occurrence owner"
    )
    require(
      groupColumn != measureColumn,
      "compact C03 group and measure columns must be distinct"
    )
    override val finalizesCountFrequency: Boolean = true
  }
}

/**
  * One physical CTE containing a maximal adjacent same-pass, same-target run.
  */
private[postgresql] final case class CompiledPostgreSqlReductionCte(
    pass: SemijoinPass,
    target: BagId,
    targetInputCte: String,
    outputCte: String,
    members: Vector[CompiledPostgreSqlReductionMember],
    finalization: CompiledPostgreSqlReductionFinalization
) {
  /** Compatibility predicate for read-only tests and common emitter control. */
  def finalizesCountFrequency: Boolean = finalization.finalizesCountFrequency
}

private[postgresql] final case class CompiledPostgreSqlReductionSchedule(
    mode: YannakakisReductionMode,
    bottomUp: Vector[SemijoinStep],
    topDown: Vector[SemijoinStep]
) {
  def allSteps: Vector[SemijoinStep] = bottomUp ++ topDown
}

private[postgresql] final case class CompiledPostgreSqlRendering(
    query: QueryIR,
    input: CandidateSearchInput,
    plan: BagFactorPlan,
    relationByOccurrence: Map[RelationOccurrenceId, RelationOccurrence],
    metadataByColumn: Map[ColumnId, ColumnMetadata],
    edgeById: Map[EdgeId, Hyperedge],
    edgeByOccurrence: Map[RelationOccurrenceId, Hyperedge],
    ownerByCarriedColumn: Map[ColumnId, BagId],
    localPredicatesByOccurrence: Map[
      RelationOccurrenceId,
      Vector[CompiledPostgreSqlPredicate]
    ],
    innerEqualities: Vector[CompiledPostgreSqlPredicate],
    terminalInnerEqualities: Vector[CompiledPostgreSqlPredicate],
    terminal: CompiledPostgreSqlTerminal,
    bags: Vector[CompiledPostgreSqlBag],
    reductionCtes: Vector[CompiledPostgreSqlReductionCte],
    aggregateRenderingProfile: PostgreSqlAggregateRenderingProfile,
    aggregateRendererV2Trace: PostgreSqlAggregateRendererV2Trace,
    aggregateRenderingProfileV3: PostgreSqlAggregateRenderingProfileV3,
    enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile,
    cteMaterialization: PostgreSqlCteMaterializationDirective,
    output: Vector[PostgreSqlOutputColumn],
    trace: PostgreSqlRenderTrace,
    statistics: PostgreSqlRenderingStatistics
)

private[postgresql] object PostgreSqlNames {
  def support(bagId: BagId): String = s"yr_s_b${bagId.value}"
  def initialFactor(bagId: BagId): String = s"yr_f_b${bagId.value}_i"
  def semijoin(step: SemijoinStep): String = step.pass match {
    case SemijoinPass.BottomUp =>
      s"yr_f_b${step.target.value}_bu${step.ordinal}"
    case SemijoinPass.TopDown =>
      s"yr_f_b${step.target.value}_td${step.ordinal}"
  }
  def supportScan(bagId: BagId, edgeId: EdgeId): String =
    s"yr_s_b${bagId.value}_e${edgeId.value}"
  def factorScan(bagId: BagId, edgeId: EdgeId): String =
    s"yr_f_b${bagId.value}_e${edgeId.value}"
  def factorSupport(bagId: BagId): String = s"yr_fs_b${bagId.value}"
  def finalFactor(bagId: BagId): String = s"yr_o_b${bagId.value}"
  def countFactorInput(bagId: BagId): String = s"yr_ci_b${bagId.value}"
  def countFactor(bagId: BagId): String = s"yr_cf_b${bagId.value}"
  def countMessage(bagId: BagId): String = s"yr_cm_b${bagId.value}"
  def countReductionSource(step: SemijoinStep): String =
    s"yr_cd_${step.pass.code.replace('-', '_')}${step.ordinal}_b${step.source.value}"
  def extremumScan(edgeId: EdgeId): String = s"yr_g_e${edgeId.value}"
  def extremumStage(ordinal: Int): String = s"yr_g_s$ordinal"
  val CountFrequencyColumn: String = "yr_freq"
  val C03LocalMinimumColumn: String = "yr_local_min"
  val C03LocalMaximumColumn: String = "yr_local_max"
  def factorUnit(bagId: BagId): String = s"yr_unit_b${bagId.value}"
  def variable(variableId: VariableId): String = s"yr_v${variableId.value}"
  def occurrenceColumn(edgeId: EdgeId, columnId: ColumnId): String =
    s"yr_e${edgeId.value}_c${columnId.ordinal}"
  def semijoinTarget(step: SemijoinStep): String =
    s"yr_t_${step.pass.code.replace('-', '_')}${step.ordinal}_b${step.target.value}"
  def semijoinSource(step: SemijoinStep): String =
    s"yr_x_${step.pass.code.replace('-', '_')}${step.ordinal}_b${step.source.value}"
}

private[postgresql] object PostgreSqlRenderingCompiler {
  private val SupportedStructuralTypes = Set("SMALLINT", "INTEGER", "BIGINT")
  private val IntegralTypesByRank = Vector("SMALLINT", "INTEGER", "BIGINT")
  private val IntegralTypeRanks = IntegralTypesByRank.zipWithIndex.toMap
  private val PostgreSqlIdentifierBytes = 63
  private val MaximumAggregateNestingDepth =
    PostgreSqlRenderingLimits.MaximumAggregateNestingDepth.toInt

  def compile(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits
  ): Either[Vector[PostgreSqlRenderingError], CompiledPostgreSqlRendering] =
    compile(
      query,
      input,
      plan,
      limits,
      YannakakisReductionMode.Default,
      PostgreSqlCteMaterializationDirective.PlannerControlled
    )

  def compile(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode
  ): Either[Vector[PostgreSqlRenderingError], CompiledPostgreSqlRendering] =
    compile(
      query,
      input,
      plan,
      limits,
      reductionMode,
      PostgreSqlCteMaterializationDirective.PlannerControlled
    )

  def compile(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective
  ): Either[Vector[PostgreSqlRenderingError], CompiledPostgreSqlRendering] =
    compile(
      query,
      input,
      plan,
      limits,
      reductionMode,
      cteMaterialization,
      PostgreSqlAggregateRenderingProfile.LegacyV1,
      PostgreSqlEnumerationRenderingProfile.default
    )

  def compile(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective,
      aggregateRenderingProfile: PostgreSqlAggregateRenderingProfile
  ): Either[Vector[PostgreSqlRenderingError], CompiledPostgreSqlRendering] =
    compile(
      query,
      input,
      plan,
      limits,
      reductionMode,
      cteMaterialization,
      aggregateRenderingProfile,
      PostgreSqlEnumerationRenderingProfile.default
    )

  def compile(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective,
      aggregateRenderingProfile: PostgreSqlAggregateRenderingProfile,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): Either[Vector[PostgreSqlRenderingError], CompiledPostgreSqlRendering] = {
    val profileV2 =
      if (aggregateRenderingProfile == null) null
      else
        PostgreSqlAggregateRenderingProfileV2
          .baseline(aggregateRenderingProfile)
          .fold(
            problems =>
              return Left(
                Vector(
                  PostgreSqlRenderingError.ModelInvariantViolation(
                    "Frozen aggregate rendering profile cannot be wrapped by renderer v2: " +
                      problems.map(_.code).mkString(", ")
                  )
                )
              ),
            identity
          )
    compileWithAggregateProfileV2(
      query,
      input,
      plan,
      limits,
      reductionMode,
      cteMaterialization,
      profileV2,
      enumerationRenderingProfile
    )
  }

  /** Additive development-only renderer-v2 compiler boundary. */
  def compileWithAggregateProfileV2(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective,
      aggregateRenderingProfileV2: PostgreSqlAggregateRenderingProfileV2,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): Either[Vector[PostgreSqlRenderingError], CompiledPostgreSqlRendering] = {
    val profileV3 =
      if (aggregateRenderingProfileV2 == null) null
      else
        PostgreSqlAggregateRenderingProfileV3
          .baseline(aggregateRenderingProfileV2)
          .fold(
            problems =>
              return Left(
                Vector(
                  PostgreSqlRenderingError.ModelInvariantViolation(
                    "Renderer-v2 aggregate profile cannot be wrapped by renderer v3: " +
                      problems.map(_.code).mkString(", ")
                  )
                )
              ),
            identity
          )
    compileWithAggregateProfileV3(
      query,
      input,
      plan,
      limits,
      reductionMode,
      cteMaterialization,
      profileV3,
      enumerationRenderingProfile
    )
  }

  /** Additive development-only renderer-v3 compiler boundary. */
  def compileWithAggregateProfileV3(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective,
      aggregateRenderingProfileV3: PostgreSqlAggregateRenderingProfileV3,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): Either[Vector[PostgreSqlRenderingError], CompiledPostgreSqlRendering] = {
    if (reductionMode == null) {
      return Left(
        Vector(
          PostgreSqlRenderingError.MissingValue(
            "Yannakakis reduction mode"
          )
        )
      )
    }
    if (cteMaterialization == null) {
      return Left(
        Vector(
          PostgreSqlRenderingError.MissingValue(
            "PostgreSQL CTE materialization directive"
          )
        )
      )
    }
    if (aggregateRenderingProfileV3 == null) {
      return Left(
        Vector(
          PostgreSqlRenderingError.MissingValue(
            "PostgreSQL aggregate rendering profile"
          )
        )
      )
    }
    if (enumerationRenderingProfile == null) {
      return Left(
        Vector(
          PostgreSqlRenderingError.MissingValue(
            "PostgreSQL enumeration rendering profile"
          )
        )
      )
    }
    val profileV3Errors =
      PostgreSqlAggregateRenderingProfileV3.validate(
        aggregateRenderingProfileV3
      )
    if (profileV3Errors.nonEmpty) {
      return Left(
        Vector(
          PostgreSqlRenderingError.ModelInvariantViolation(
            "Invalid PostgreSQL aggregate rendering profile v3: " +
              profileV3Errors.map(_.code).mkString(", ")
          )
        )
      )
    }
    val aggregateRenderingProfileV2 = aggregateRenderingProfileV3.baseProfile
    val aggregateRenderingProfile = aggregateRenderingProfileV2.baseProfile
    val relationByOccurrence =
      query.relations.iterator.map(relation => relation.id -> relation).toMap
    val metadataByColumn = query.relations.iterator
      .flatMap(_.columns.iterator)
      .map(metadata => metadata.ref.id -> metadata)
      .toMap
    val edgeById =
      input.hypergraph.edges.iterator.map(edge => edge.id -> edge).toMap
    val edgesByOccurrence = input.hypergraph.edges.groupBy(
      _.relationOccurrenceId
    )

    val errors = Vector.newBuilder[PostgreSqlRenderingError]
    validateIdentifiers(query, errors)
    if (query.output.isEmpty) {
      errors += PostgreSqlRenderingError.ModelInvariantViolation(
        "PostgreSQL rendering requires at least one output column"
      )
    }

    query.relations.foreach { relation =>
      val edges = edgesByOccurrence.getOrElse(relation.id, Vector.empty)
      if (edges.size != 1) {
        errors += PostgreSqlRenderingError.AmbiguousRelationEdgeMapping(
          relation.id,
          edges.map(_.id).sorted
        )
      }
    }
    input.hypergraph.edges.foreach { edge =>
      if (!relationByOccurrence.contains(edge.relationOccurrenceId)) {
        errors += PostgreSqlRenderingError.AmbiguousRelationEdgeMapping(
          edge.relationOccurrenceId,
          Vector(edge.id)
        )
      }
      edge.bindings.foreach { case (columnId, _) =>
        metadataByColumn.get(columnId) match {
          case Some(metadata)
              if metadata.ref.relationOccurrenceId == edge.relationOccurrenceId =>
            ()
          case _ =>
            errors += PostgreSqlRenderingError.UnknownBindingColumn(
              edge.id,
              columnId
            )
        }
      }
    }

    validateStructuralTypes(query, input, metadataByColumn, errors)
    validatePredicates(query, metadataByColumn, errors)
    validateLocalIntegralSourceProof(query, metadataByColumn, errors)
    val generalPredicates = query.predicates.filter(_.expression.isInstanceOf[ScalarExpression.LocalPredicate])
    if (generalPredicates.nonEmpty &&
        (!query.sourceProof.isBoundTo(query) ||
          !query.sourceProof.localPredicateCertificate.exists(CalciteLocalPredicates.concordant(_, query)))) {
      errors += PostgreSqlRenderingError.ModelInvariantViolation(
        "local predicates require an analyzer-issued, query-bound concordant source proof")
    }

    val ownershipByEdge = plan.ownership.groupBy(_.edgeId)
    val ownerByAllColumns = mutable.Map.empty[ColumnId, BagId]
    query.relations.foreach { relation =>
      val mappedEdges = edgesByOccurrence.getOrElse(relation.id, Vector.empty)
      val candidateOwners = mappedEdges.flatMap(edge =>
        ownershipByEdge.getOrElse(edge.id, Vector.empty).map(_.owner)
      ).distinct.sorted
      relation.columns.foreach { metadata =>
        if (candidateOwners.size != 1) {
          errors += PostgreSqlRenderingError.AmbiguousColumnOwner(
            metadata.ref,
            candidateOwners
          )
        } else ownerByAllColumns.put(metadata.ref.id, candidateOwners.head)
      }
    }

    val output = query.output.map { slot =>
      PostgreSqlOutputColumn(
        slot.ordinal,
        slot.name,
        slot.dataType,
        slot.nullable
      )
    }
    query.resultShape match {
      case QueryResultShape.EnumerationProjection =>
        validateOutputScalars(
          query,
          metadataByColumn,
          ownerByAllColumns.toMap,
          errors
        )
      case count: QueryResultShape.GlobalCountStar =>
        validateGlobalCountStarOutput(query, count, output, errors)
      case count: QueryResultShape.GroupedCountStar =>
        validateGroupedCountStarOutput(
          query,
          count,
          output,
          metadataByColumn,
          reductionMode,
          errors
        )
      case count: QueryResultShape.DistributedGroupedCountStar =>
        validateDistributedGroupedCountStarOutput(
          query,
          count,
          output,
          metadataByColumn,
          reductionMode,
          errors
        )
      case aggregate: QueryResultShape.C03G0AggregateVector =>
        validateC03G0AggregateVectorOutput(
          query,
          aggregate,
          output,
          metadataByColumn,
          ownerByAllColumns.toMap,
          edgeById,
          plan,
          reductionMode,
          errors
        )
      case extremum: QueryResultShape.GlobalGuardedExtremum =>
        validateGlobalGuardedExtremumOutput(
          query,
          extremum,
          output,
          metadataByColumn,
          ownerByAllColumns.toMap,
          edgeById,
          plan,
          reductionMode,
          errors
        )
    }

    val preliminaryErrors =
      PostgreSqlRenderingError.canonical(errors.result())
    if (preliminaryErrors.nonEmpty) return Left(preliminaryErrors)

    val edgeByOccurrence = edgesByOccurrence.iterator.collect {
      case (occurrenceId, Vector(edge)) => occurrenceId -> edge
    }.toMap

    val localPredicateDescriptors = query.predicates.collect {
      case predicate @ PredicateDescriptor(
            _,
            _,
            _,
            PredicateClassification.LocalFilter(occurrenceId)
          ) => occurrenceId -> predicate
    }.groupBy(_._1).iterator.map { case (occurrenceId, values) =>
      occurrenceId -> values.map(_._2).sortBy(_.ordinal)
    }.toMap
    val innerEqualityDescriptors = query.predicates.collect {
      case predicate @ PredicateDescriptor(
            _,
            _,
            _,
            _: PredicateClassification.InnerEquality
          ) => predicate
    }.sortBy(_.ordinal)

    val localPredicatesByOccurrence = localPredicateDescriptors.map {
      case (occurrenceId, predicates) =>
        occurrenceId -> predicates.flatMap(predicate =>
          compileLocalPredicateScalar(
            predicate.expression,
            metadataByColumn
          ).map(scalar =>
            CompiledPostgreSqlPredicate(predicate.ordinal, scalar)
          )
        )
    }
    val innerEqualities = innerEqualityDescriptors.flatMap(predicate =>
      compileScalar(predicate.expression).map(scalar =>
        CompiledPostgreSqlPredicate(predicate.ordinal, scalar)
      )
    )
    val projectionScalars = query.resultShape match {
      case QueryResultShape.EnumerationProjection =>
        query.output.flatMap(output => compileScalar(output.expression))
      case _: QueryResultShape.GlobalCountStar => Vector.empty
      case _: QueryResultShape.GroupedCountStar => Vector.empty
      case _: QueryResultShape.DistributedGroupedCountStar => Vector.empty
      case _: QueryResultShape.C03G0AggregateVector => Vector.empty
      case _: QueryResultShape.GlobalGuardedExtremum => Vector.empty
    }
    if (
      localPredicatesByOccurrence.valuesIterator.map(_.size).sum !=
        localPredicateDescriptors.valuesIterator.map(_.size).sum ||
      innerEqualities.size != innerEqualityDescriptors.size ||
      (query.resultShape == QueryResultShape.EnumerationProjection &&
        projectionScalars.size != query.output.size)
    ) {
      return Left(
        Vector(
          PostgreSqlRenderingError.ModelInvariantViolation(
            "Validated retained scalar could not be compiled"
          )
        )
      )
    }
    val terminal = query.resultShape match {
      case QueryResultShape.EnumerationProjection =>
        CompiledPostgreSqlTerminal.Projection(projectionScalars)
      case _: QueryResultShape.GlobalCountStar =>
        val usesReducedMessagePropagation =
          reductionMode == YannakakisReductionMode.FullTwoPass
        val usesRawMessagePropagation =
          reductionMode == YannakakisReductionMode.NoReduction &&
            aggregateRenderingProfileV3.usesFusedRawFactorMessages
        val strategy =
          if (usesReducedMessagePropagation || usesRawMessagePropagation) {
            val frequencyPlan = PostgreSqlFrequencyPropagation.derive(plan) match {
              case Left(problems) =>
                return Left(
                  Vector(
                    PostgreSqlRenderingError.ModelInvariantViolation(
                      "Global COUNT(*) frequency propagation could not be derived: " +
                        problems.map(_.message).mkString("; ")
                    )
                  )
                )
              case Right(value) => value
            }
            aggregateNestingError(
              frequencyPlan.maximumMessageNestingDepth
            ) match {
              case Some(problem) => return Left(Vector(problem))
              case None          => ()
            }

            if (usesRawMessagePropagation) {
              // This separately identified development-only strategy does
              // not use the full-reduction overflow argument below. It runs
              // the exact sum-product program over raw initial factors and
              // may form a larger nonnegative NUMERIC intermediate than the
              // final representable BIGINT count.
              CompiledPostgreSqlTerminal.GlobalCountStrategy
                .RawFrequencyPropagation(frequencyPlan)
            } else {
              // Set-valued separator supports perform the reductions. The root
              // receives a frequency only after bottom-up completion and every
              // non-root only after its inbound top-down filter. Every counted
              // row is then globally extendible, so each nonnegative frequency
              // is bounded by the final COUNT and PostgreSQL NUMERIC cannot hit
              // its engine digit boundary before a representable BIGINT result.
              CompiledPostgreSqlTerminal.GlobalCountStrategy
                .FrequencyPropagation(frequencyPlan)
            }
          } else {
            // These modes can retain an arbitrarily large orphan subtree even
            // when the final join is empty. Reconstructing preserves native
            // COUNT(*) semantics without exposing that intermediate carrier.
            CompiledPostgreSqlTerminal.GlobalCountStrategy.Reconstruction
          }
        CompiledPostgreSqlTerminal.GlobalCountStar(output.head, strategy)
      case shape: QueryResultShape.GroupedCountStar =>
        if (reductionMode != YannakakisReductionMode.FullTwoPass) {
          return Left(
            Vector(
              PostgreSqlRenderingError.GroupedCountRequiresFullTwoPass(
                reductionMode
              )
            )
          )
        }
        val groupRef = shape.groupColumn.ref
        val groupVariable = edgeByOccurrence
          .get(groupRef.relationOccurrenceId)
          .flatMap(_.bindings.find(_._1 == groupRef.id).map(_._2)) match {
          case Some(value) => value
          case None =>
            return Left(
              Vector(
                PostgreSqlRenderingError.UnsupportedGroupedCountKey(
                  groupRef,
                  "the direct key is not a structural join binding"
                )
              )
            )
        }
        val groupOwner = ownerByAllColumns.get(groupRef.id) match {
          case Some(value) => value
          case None =>
            return Left(
              Vector(
                PostgreSqlRenderingError.UnsupportedGroupedCountKey(
                  groupRef,
                  "the grouping occurrence has no unique owner bag"
                )
              )
            )
        }
        val frequencyPlan = PostgreSqlFrequencyPropagation
          .deriveGrouped(plan, groupOwner, groupVariable) match {
          case Left(problems) =>
            return Left(
              Vector(
                PostgreSqlRenderingError.ModelInvariantViolation(
                  "Grouped COUNT(*) frequency propagation could not be derived: " +
                    problems.map(_.message).mkString("; ")
                )
              )
            )
          case Right(value) => value
        }
        aggregateNestingError(
          frequencyPlan.maximumMessageNestingDepth
        ) match {
          case Some(problem) => return Left(Vector(problem))
          case None          => ()
        }
        CompiledPostgreSqlTerminal.GroupedCountStar(
          CompiledPostgreSqlScalar.Column(groupRef),
          groupVariable,
          output.head,
          output(1),
          groupOwner,
          frequencyPlan
        )
      case shape: QueryResultShape.DistributedGroupedCountStar =>
        if (reductionMode != YannakakisReductionMode.FullTwoPass) {
          return Left(
            Vector(
              PostgreSqlRenderingError
                .DistributedGroupedCountRequiresFullTwoPass(reductionMode)
            )
          )
        }
        val groupRefs = shape.groupColumns.map(_.ref)
        val groupVariables = groupRefs.flatMap { groupRef =>
          edgeByOccurrence
            .get(groupRef.relationOccurrenceId)
            .flatMap(_.bindings.find(_._1 == groupRef.id).map(_._2))
        }
        if (groupVariables.size != groupRefs.size) {
          return Left(
            Vector(
              PostgreSqlRenderingError.UnsupportedDistributedGroupedCountKey(
                groupRefs,
                "each direct key must be a structural join binding"
              )
            )
          )
        }
        if (groupVariables.distinct.size != groupRefs.size) {
          return Left(
            Vector(
              PostgreSqlRenderingError.UnsupportedDistributedGroupedCountKey(
                groupRefs,
                "the direct columns must bind distinct structural variables"
              )
            )
          )
        }
        // Full reduction has already removed every orphan factor row before
        // staged yr_freq finalization. U0 introduces each grouping key once
        // and never eliminates it thereafter, so every non-negative local or
        // message frequency contributes to at least one compatible final
        // output group. It is therefore bounded by that group's representable
        // COUNT(*), exactly the carrier argument used by scalar/one-key COUNT.
        val request = PostgreSqlAggregatePropagation.AggregateRequest
          .countStarGroupedBy(
            groupVariables.map(
              PostgreSqlAggregatePropagation.AggregateKey.Structural
            ): _*
          )
        val aggregateBagId = PostgreSqlAggregatePropagation
          .defaultFinalizationRoot(plan, request) match {
          case Left(problems) =>
            return Left(
              Vector(
                PostgreSqlRenderingError.ModelInvariantViolation(
                  "Distributed grouped COUNT(*) finalization root could not be derived: " +
                    problems.map(_.message).mkString("; ")
                )
              )
            )
          case Right(value) => value
        }
        val propagationPlan = PostgreSqlAggregatePropagation
          .derive(plan, aggregateBagId, request) match {
          case Left(problems) =>
            return Left(
              Vector(
                PostgreSqlRenderingError.ModelInvariantViolation(
                  "Distributed grouped COUNT(*) propagation could not be derived: " +
                    problems.map(_.message).mkString("; ")
                )
              )
            )
          case Right(value) => value
        }
        aggregateNestingError(
          propagationPlan.maximumMessageNestingDepth
        ) match {
          case Some(problem) => return Left(Vector(problem))
          case None          => ()
        }
        CompiledPostgreSqlTerminal.DistributedGroupedCountStar(
          groupRefs.map(CompiledPostgreSqlScalar.Column),
          groupVariables,
          output.take(groupRefs.size),
          output(groupRefs.size),
          aggregateBagId,
          propagationPlan
        )
      case shape: QueryResultShape.C03G0AggregateVector =>
        if (reductionMode != YannakakisReductionMode.FullTwoPass) {
          return Left(
            Vector(
              PostgreSqlRenderingError.C03G0RequiresFullTwoPass(reductionMode)
            )
          )
        }
        val groupRef = shape.groupColumn
        val measureRef = shape.measureColumn
        val groupOwner = ownerByAllColumns.get(groupRef.id)
        val measureOwner = ownerByAllColumns.get(measureRef.id)
        val aggregateBagId = (groupOwner, measureOwner) match {
          case (Some(group), Some(measure)) if group == measure => group
          case _ =>
            return Left(
              Vector(
                PostgreSqlRenderingError.UnsupportedC03G0Owner(
                  groupRef,
                  measureRef,
                  "both direct columns must have the same unique owner bag"
                )
              )
            )
        }
        val groupKey =
          PostgreSqlAggregatePropagation.AggregateKey.OccurrenceValue(
            groupRef.id
          )
        val request = PostgreSqlAggregatePropagation.AggregateRequest
          .countStarGroupedBy(groupKey)
        PostgreSqlAggregatePropagation.defaultFinalizationRoot(plan, request) match {
          case Left(problems) =>
            return Left(
              Vector(
                PostgreSqlRenderingError.ModelInvariantViolation(
                  "C03-G0 finalization root could not be derived: " +
                    problems.map(_.message).mkString("; ")
                )
              )
            )
          case Right(root) if root != aggregateBagId =>
            return Left(
              Vector(
                PostgreSqlRenderingError.UnsupportedC03G0Owner(
                  groupRef,
                  measureRef,
                  "the occurrence-valued key does not finalize at its common owner"
                )
              )
            )
          case Right(_) => ()
        }
        val propagationPlan = PostgreSqlAggregatePropagation
          .derive(plan, aggregateBagId, request) match {
          case Left(problems) =>
            return Left(
              Vector(
                PostgreSqlRenderingError.ModelInvariantViolation(
                  "C03-G0 aggregate propagation could not be derived: " +
                    problems.map(_.message).mkString("; ")
                )
              )
            )
          case Right(value) => value
        }
        aggregateNestingError(
          propagationPlan.maximumMessageNestingDepth
        ) match {
          case Some(problem) => return Left(Vector(problem))
          case None          => ()
        }
        CompiledPostgreSqlTerminal.C03G0AggregateVector(
          CompiledPostgreSqlScalar.Column(groupRef),
          CompiledPostgreSqlScalar.Column(measureRef),
          output,
          aggregateBagId,
          propagationPlan
        )
      case shape: QueryResultShape.GlobalGuardedExtremum =>
        val guardOwner = ownerByAllColumns(shape.guardColumn.id)
        val semijoinPlan = PostgreSqlGuardedExtremumSemijoin
          .deriveWithMaximumNestingDepth(
            plan,
            shape.guardColumn,
            guardOwner,
            MaximumAggregateNestingDepth
          ) match {
          case Left(
                Vector(
                  PostgreSqlGuardedExtremumSemijoin.Error(
                    PostgreSqlGuardedExtremumSemijoin.ErrorCode
                      .NestedStageDepthExceeded(required, limit),
                    _
                  )
                )
              ) =>
            return Left(
              Vector(
                aggregateNestingError(required).getOrElse(
                  PostgreSqlRenderingError.ModelInvariantViolation(
                    s"Guarded aggregate reported nesting depth $required against inconsistent limit $limit"
                  )
                )
              )
            )
          case Left(problems) =>
            return Left(
              Vector(
                PostgreSqlRenderingError.ModelInvariantViolation(
                  "Global guarded MIN/MAX semijoin could not be derived: " +
                    problems.map(_.message).mkString("; ")
                )
              )
            )
          case Right(value) => value
        }
        CompiledPostgreSqlTerminal.GlobalGuardedExtremum(
          shape.function,
          CompiledPostgreSqlScalar.Column(shape.guardColumn),
          output.head,
          guardOwner,
          reductionMode == YannakakisReductionMode.FullTwoPass,
          semijoinPlan
        )
    }

    if (!terminal.reconstructsFactors) {
      val equalityErrors = validateFactorizedInnerEqualities(
        innerEqualityDescriptors,
        edgeByOccurrence
      )
      if (equalityErrors.nonEmpty) return Left(equalityErrors)
    }

    PostgreSqlAggregateRendererV2Compiler.validateScope(
      reductionMode,
      terminal,
      aggregateRenderingProfileV2,
      enumerationRenderingProfile
    ) match {
      case problems if problems.nonEmpty => return Left(problems)
      case _                             => ()
    }
    PostgreSqlAggregateRendererV3Compiler.validateScope(
      reductionMode,
      terminal,
      aggregateRenderingProfileV3,
      enumerationRenderingProfile
    ) match {
      case problems if problems.nonEmpty => return Left(problems)
      case _                             => ()
    }

    val occurrenceLiveness = compileOccurrenceLiveness(
      query,
      edgeByOccurrence,
      localPredicatesByOccurrence,
      innerEqualities,
      terminal
    ) match {
      case Left(problems) => return Left(problems)
      case Right(value)   => value
    }
    val livenessByEdge = occurrenceLiveness.iterator
      .map(liveness => liveness.edge.id -> liveness)
      .toMap

    val compiledBagErrors = Vector.newBuilder[PostgreSqlRenderingError]
    val logicalCompiledBags = plan.factors.sortBy(_.bagId).map { factor =>
      compileBag(factor, edgeById, livenessByEdge, compiledBagErrors)
    }
    val canonicalBagErrors =
      PostgreSqlRenderingError.canonical(compiledBagErrors.result())
    if (canonicalBagErrors.nonEmpty) return Left(canonicalBagErrors)

    val ownerByCarriedColumn = logicalCompiledBags.iterator
      .flatMap(bag =>
        bag.ownedOccurrences.iterator.flatMap(occurrence =>
          occurrence.carriedColumns.iterator.map(metadata =>
            metadata.ref.id -> bag.factor.bagId
          )
        )
      )
      .toMap

    // The occurrence columns remain conservatively live. In the
    // statistics-safe profile the reconstruction terminal need not repeat an
    // original equality when the compiled factors already bind both endpoints
    // to the same structural variable and every tree edge between their owners
    // joins that variable through its separator. The proof below reads the
    // compiled bag strategies rather than assuming that a logical variable
    // alone is sufficient; a predicate for which any link cannot be
    // re-established is retained verbatim at the terminal. LegacyV1 retains
    // every equality solely for frozen-byte compatibility.
    val terminalInnerEqualities =
      if (!terminal.reconstructsFactors) Vector.empty
      else if (enumerationRenderingProfile.retainAllReconstructionEqualities)
        innerEqualities
      else
        retainUnprovedReconstructionEqualities(
          innerEqualityDescriptors,
          innerEqualities,
          plan,
          logicalCompiledBags,
          edgeByOccurrence
        )

    val selectedSchedule = selectReductionSchedule(plan, reductionMode)
    val reductionFinalizations = deriveReductionFinalizations(
      plan,
      terminal,
      aggregateRenderingProfile
    ) match {
      case Left(problems) => return Left(problems)
      case Right(value)   => value
    }
    val compiledBags = PostgreSqlAggregateRendererV2Compiler
      .projectFactorBoundaries(
        plan,
        logicalCompiledBags,
        selectedSchedule,
        terminal,
        reductionFinalizations,
        aggregateRenderingProfileV2
      ) match {
      case Left(problems) => return Left(problems)
      case Right(value)   => value
    }
    val (bagTrace, semijoinTrace, reductionCtes) =
      compileTrace(
        plan,
        compiledBags,
        selectedSchedule,
        reductionFinalizations
      ) match {
        case Left(problems) => return Left(problems)
        case Right(value)   => value
      }
    val reductionErrors = validateReductionCtes(
      plan,
      selectedSchedule,
      bagTrace,
      semijoinTrace,
      reductionCtes,
      reductionFinalizations
    )
    if (reductionErrors.nonEmpty) return Left(reductionErrors)
    val exactDirectFactorMessageOrdinals =
      PostgreSqlAggregateRendererV2Compiler.directLeafMessageOrdinals(
        compiledBags,
        reductionCtes,
        terminal,
        aggregateRenderingProfileV2
      ) match {
        case Left(problems) => return Left(problems)
        case Right(value)   => value
      }
    val aggregateRendererV2Trace = PostgreSqlAggregateRendererV2Trace(
      aggregateRenderingProfileV2,
      compiledBags.map { bag =>
        val projection = bag.physicalProjection
        PostgreSqlAggregateRendererV2FactorTrace(
          bag.factor.bagId,
          projection.structuralVariables,
          projection.occurrenceColumns,
          projection.unitColumn
        )
      },
      exactDirectFactorMessageOrdinals
    )
    val stageTrace = compileStageTrace(
      plan,
      compiledBags,
      bagTrace,
      reductionCtes,
      terminal,
      cteMaterialization
    ) match {
      case Left(problems) => return Left(problems)
      case Right(value)   => value
    }
    val stageTraceErrors = validateStageTrace(
      plan,
      compiledBags,
      bagTrace,
      selectedSchedule,
      reductionCtes,
      stageTrace,
      terminal,
      cteMaterialization
    )
    if (stageTraceErrors.nonEmpty) return Left(stageTraceErrors)
    val livenessErrors = validateOccurrenceLiveness(
      query,
      edgeByOccurrence,
      plan,
      localPredicatesByOccurrence,
      innerEqualities,
      terminal,
      occurrenceLiveness,
      compiledBags,
      ownerByCarriedColumn,
      bagTrace
    )
    if (livenessErrors.nonEmpty) return Left(livenessErrors)

    val resourceCounts = calculateResourceCounts(
      plan,
      compiledBags,
      reductionCtes,
      localPredicatesByOccurrence,
      terminal,
      terminalInnerEqualities,
      aggregateRenderingProfile,
      aggregateRenderingProfileV2,
      aggregateRenderingProfileV3,
      aggregateRendererV2Trace.elidedLogicalMessageOrdinals
    )
    val resourceErrors = resourceCounts.errors(limits)
    if (resourceErrors.nonEmpty) return Left(resourceErrors)

    validateGeneratedNameCollisions(
      query,
      bagTrace,
      reductionCtes,
      terminal
    ) match {
      case problems if problems.nonEmpty => return Left(problems)
      case _                             => ()
    }

    val statistics = PostgreSqlRenderingStatistics(
      expandedScanCount = resourceCounts.expandedScans.toLong,
      renderedTermCount = resourceCounts.renderedTerms.toLong,
      cteCount = resourceCounts.ctes.toLong,
      sqlCharacterCount = 0L
    )

    Right(
      CompiledPostgreSqlRendering(
        query,
        input,
        plan,
        relationByOccurrence,
        metadataByColumn,
        edgeById,
        edgeByOccurrence,
        ownerByCarriedColumn,
        localPredicatesByOccurrence,
        innerEqualities,
        terminalInnerEqualities,
        terminal,
        compiledBags,
        reductionCtes,
        aggregateRenderingProfile,
        aggregateRendererV2Trace,
        aggregateRenderingProfileV3,
        enumerationRenderingProfile,
        cteMaterialization,
        output,
        PostgreSqlRenderTrace(
          bagTrace,
          semijoinTrace,
          PostgreSqlReductionScheduleTrace(
            mode = selectedSchedule.mode,
            reductionOrder =
              PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
            canonicalBottomUpStepCount = plan.schedule.bottomUp.size,
            canonicalTopDownStepCount = plan.schedule.topDown.size,
            selectedBottomUpStepCount = selectedSchedule.bottomUp.size,
            selectedTopDownStepCount = selectedSchedule.topDown.size,
            physicalReductionCteCount = reductionCtes.size
          ),
          stageTrace
        ),
        statistics
      )
    )
  }

  private[postgresql] def aggregateNestingError(
      required: Int
  ): Option[PostgreSqlRenderingError.ResourceLimitExceeded] =
    if (required > MaximumAggregateNestingDepth) {
      Some(
        PostgreSqlRenderingError.ResourceLimitExceeded(
          PostgreSqlRenderingResource.AggregateNestingDepth,
          BigInt(required),
          MaximumAggregateNestingDepth.toLong
        )
      )
    } else None

  private def validateIdentifiers(
      query: QueryIR,
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = {
    query.relations.foreach { relation =>
      if (
        relation.qualifiedName.size < 1 || relation.qualifiedName.size > 3
      ) {
        errors += PostgreSqlRenderingError.InvalidQualifiedName(
          relation.id,
          relation.qualifiedName,
          "PostgreSQL names must contain between one and three components"
        )
      }
      relation.qualifiedName.zipWithIndex.foreach { case (component, index) =>
        identifierError(
          s"relation ${relation.id.value} qualified-name component $index",
          component
        ).foreach(errors += _)
      }

      relation.columns
        .groupBy(_.sourceName)
        .collect {
          case (name, occurrences) if occurrences.size > 1 =>
            PostgreSqlRenderingError.DuplicateSourceColumn(
              relation.id,
              name,
              occurrences.map(_.ref.ordinal).sorted
            )
        }
        .toVector
        .sortBy(_.sourceName)
        .foreach(errors += _)

      relation.columns.foreach { metadata =>
        identifierError(
          s"relation ${relation.id.value} column ${metadata.ref.ordinal}",
          metadata.sourceName
        ).foreach(errors += _)
      }
    }
    query.output.foreach { output =>
      identifierError(s"output ${output.ordinal} alias", output.name).foreach(
        errors += _
      )
    }
  }

  private def identifierError(
      path: String,
      value: String
  ): Option[PostgreSqlRenderingError] = {
    if (value.trim.isEmpty) {
      Some(
        PostgreSqlRenderingError.InvalidIdentifier(
          path,
          value,
          "identifier must not be blank"
        )
      )
    } else if (value.indexOf('\u0000') >= 0) {
      Some(
        PostgreSqlRenderingError.InvalidIdentifier(
          path,
          value,
          "identifier must not contain NUL"
        )
      )
    } else if (
      value.getBytes(StandardCharsets.UTF_8).length > PostgreSqlIdentifierBytes
    ) {
      Some(
        PostgreSqlRenderingError.InvalidIdentifier(
          path,
          value,
          s"identifier exceeds $PostgreSqlIdentifierBytes UTF-8 bytes"
        )
      )
    } else None
  }

  private def validateStructuralTypes(
      query: QueryIR,
      input: CandidateSearchInput,
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = {
    val bindingsByVariable = input.hypergraph.edges.iterator
      .flatMap(edge => edge.bindings.iterator.map(binding => edge -> binding))
      .toVector
      .groupBy { case (_, (_, variableId)) => variableId }

    input.hypergraph.variables.foreach { variableId =>
      val bindings = bindingsByVariable.getOrElse(variableId, Vector.empty)
      val occurrences = bindings
        .map { case (edge, _) => edge.relationOccurrenceId }
        .distinct
        .sorted
      if (occurrences.size < 2) {
        errors += PostgreSqlRenderingError.NonJoinStructuralVariable(
          variableId,
          occurrences
        )
      }

      val columnsAndTypes = bindings.flatMap { case (_, (columnId, _)) =>
        metadataByColumn.get(columnId).map(metadata =>
          columnId -> metadata.sqlTypeName
        )
      }.sortBy(_._1)
      val types = columnsAndTypes.map(_._2).distinct
      if (columnsAndTypes.size == bindings.size && types.size != 1) {
        errors += PostgreSqlRenderingError.NonUniformVariableTypes(
          variableId,
          columnsAndTypes
        )
      } else if (
        columnsAndTypes.size == bindings.size && types.size == 1 &&
        !SupportedStructuralTypes(types.head) &&
        !query.textKeyProof.exists(_.accepts(query, columnsAndTypes.map(_._1)))
      ) {
        errors += PostgreSqlRenderingError.UnsupportedStructuralType(
          variableId,
          types.head
        )
      }

      bindings.foreach { case (_, (columnId, _)) =>
        metadataByColumn.get(columnId).foreach { metadata =>
          if (SupportedStructuralTypes(metadata.sqlTypeName)) {
            val expectedDataType = metadata.sqlTypeName +
              (if (metadata.nullable) "" else " NOT NULL")
            if (metadata.dataType != expectedDataType) {
              errors +=
                PostgreSqlRenderingError.InconsistentStructuralTypeMetadata(
                  variableId,
                  columnId,
                  metadata.sqlTypeName,
                  metadata.dataType,
                  metadata.nullable
                )
            }
          }
        }
      }
    }
  }

  private def validatePredicates(
      query: QueryIR,
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = {
    query.predicates.foreach { predicate =>
      validateRetainedScalarMetadata(
        predicate.expression,
        s"predicate ${predicate.ordinal}",
        metadataByColumn,
        errors
      )
      predicate.classification match {
        case equality: PredicateClassification.InnerEquality =>
          directEquality(predicate.expression) match {
            case Some((left, right))
                if sameEquality(left, right, equality.left, equality.right) &&
                  metadataByColumn.contains(left.id) &&
                  metadataByColumn.contains(right.id) => ()
            case _ =>
              errors += PostgreSqlRenderingError.PredicateContractViolation(
                predicate.ordinal,
                "InnerEquality must match one deterministic direct column equality"
              )
          }

        case PredicateClassification.LocalFilter(occurrenceId) =>
          predicate.expression match {
            case local: ScalarExpression.LocalPredicate =>
              val metadata = metadataByColumn.valuesIterator.map(m => m.ref -> m).toMap
              if (local.dataType != "BOOLEAN" && local.dataType != "BOOLEAN NOT NULL" ||
                  !local.columnRefs.forall(_.relationOccurrenceId == occurrenceId) ||
                  !CalciteLocalPredicates.validLocal(local.tree, metadata))
                errors += PostgreSqlRenderingError.PredicateContractViolation(
                  predicate.ordinal, "local predicate must be a bounded BOOLEAN tree over its declared occurrence")
            case column: ScalarExpression.Column =>
              metadataByColumn.get(column.ref.id) match {
                case Some(metadata)
                    if column.ref.relationOccurrenceId == occurrenceId &&
                      metadata.sqlTypeName == "BOOLEAN" => ()
                case _ =>
                  errors +=
                    PostgreSqlRenderingError.PredicateContractViolation(
                      predicate.ordinal,
                      "bare local column must be a BOOLEAN column of the declared occurrence"
                    )
              }
            case expression =>
              directEquality(expression) match {
                case Some((left, right))
                    if left.relationOccurrenceId == occurrenceId &&
                      right.relationOccurrenceId == occurrenceId &&
                      metadataByColumn.contains(left.id) &&
                      metadataByColumn.contains(right.id) => ()
                case _ =>
                  compileLocalIntegralEquality(
                    expression,
                    metadataByColumn
                  ) match {
                    case Some(equality)
                        if equality.column.relationOccurrenceId == occurrenceId =>
                      ()
                    case _ =>
                      errors +=
                        PostgreSqlRenderingError.PredicateContractViolation(
                          predicate.ordinal,
                          "LocalFilter must be a bare BOOLEAN column, deterministic direct equality within its occurrence, or exact typed local integral equality"
                        )
                  }
              }
          }

        case PredicateClassification.ConstantFilter =>
          if (predicate.expression.columnRefs.nonEmpty) {
            errors += PostgreSqlRenderingError.PredicateContractViolation(
              predicate.ordinal,
              "ConstantFilter must not reference columns"
            )
          }
          errors += PostgreSqlRenderingError.UnsupportedScalar(
            s"predicate ${predicate.ordinal}",
            predicate.expression.digest,
            "typed constant values are not retained by QueryIR"
          )

        case unsupported: PredicateClassification.Unsupported =>
          errors += PostgreSqlRenderingError.UnsupportedScalar(
            s"predicate ${predicate.ordinal}",
            predicate.expression.digest,
            unsupported.reason
          )
      }
    }
  }

  /** Replays the opaque raw-source certificate against the complete typed
    * predicate inventory. Exact literal/cast nodes are deliberately not a
    * public standalone scalar grammar: without this proof, a caller could
    * harvest such nodes from an unsupported direct RelRoot analysis and feed
    * them to the public renderer.
    */
  private def validateLocalIntegralSourceProof(
      query: QueryIR,
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = {
    val typedPredicates = query.predicates.filter(predicate =>
      containsExactIntegralNode(predicate.expression)
    )

    query.sourceProof.localIntegralPredicateCertificate match {
      case None =>
        typedPredicates.foreach { predicate =>
          errors += PostgreSqlRenderingError.PredicateContractViolation(
            predicate.ordinal,
            "exact integral literal/cast nodes require an analyzer-issued source proof"
          )
        }

      case Some(source) =>
        if (!query.sourceProof.isBoundTo(query)) {
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            "local integral source proof is not bound to this QueryIR"
          )
        }
        val sourceShapeCanonical =
          source.totalPredicateCount > 0 &&
            source.occurrences.nonEmpty &&
            source.predicates.nonEmpty &&
            source.occurrences.map(_.ordinal) ==
              source.occurrences.indices.toVector &&
            source.occurrences.forall(occurrence =>
              occurrence.qualifiedName.nonEmpty &&
                occurrence.qualifiedName.forall(_.nonEmpty) &&
                occurrence.alias.nonEmpty
            ) &&
            source.occurrences.map(_.alias).distinct.size ==
              source.occurrences.size &&
            source.predicates.map(_.predicateOrdinal).distinct.size ==
              source.predicates.size &&
            source.predicates.forall(binding =>
              binding.predicateOrdinal >= 0 &&
                binding.predicateOrdinal < source.totalPredicateCount &&
                binding.literalValue >= 0 &&
                binding.literalPrecision == binding.literalValue.toString.length
            )
        if (!sourceShapeCanonical) {
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            "local integral source proof is not canonical"
          )
        }
        if (query.resultShape != QueryResultShape.EnumerationProjection) {
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            "local integral source proof is valid only for enumeration projection"
          )
        }
        if (typedPredicates.isEmpty) {
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            "local integral source proof has no exact integral predicate to authorize"
          )
        }
        if (source.totalPredicateCount != query.predicates.size) {
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            "local integral source proof predicate count differs from QueryIR"
          )
        }

        val occurrencesConcordant =
          source.occurrences.size == query.relations.size &&
            source.occurrences.zip(query.relations).forall {
              case (sourceOccurrence, relation) =>
                sourceOccurrence.ordinal == relation.id.value &&
                relation.qualifiedName.endsWith(sourceOccurrence.qualifiedName)
            }
        if (!occurrencesConcordant) {
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            "local integral source proof occurrence inventory differs from QueryIR"
          )
        }

        val aliasesConcordant = source.predicates.forall(binding =>
          source.occurrences
            .lift(binding.occurrenceOrdinal)
            .exists(_.alias == binding.alias)
        )
        if (!aliasesConcordant) {
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            "local integral source proof alias binding is inconsistent"
          )
        }

        val sourceBindings = source.predicates.map(binding =>
          (
            binding.predicateOrdinal,
            binding.occurrenceOrdinal,
            binding.columnName,
            binding.literalValue,
            binding.literalOnLeft
          )
        )
        val relationalBindings = query.predicates.flatMap { predicate =>
          compileLocalIntegralEquality(
            predicate.expression,
            metadataByColumn
          ).flatMap { equality =>
            metadataByColumn.get(equality.column.id).map(metadata =>
              (
                predicate.ordinal,
                equality.column.relationOccurrenceId.value,
                metadata.sourceName,
                equality.literalValue,
                equality.literalOnLeft
              )
            )
          }
        }
        if (sourceBindings != relationalBindings) {
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            "local integral source proof value/order bindings differ from QueryIR"
          )
        }

        typedPredicates.foreach { predicate =>
          val classificationConcordant = compileLocalIntegralEquality(
            predicate.expression,
            metadataByColumn
          ).exists(equality =>
            predicate.classification == PredicateClassification.LocalFilter(
              equality.column.relationOccurrenceId
            )
          )
          if (!classificationConcordant) {
            errors += PostgreSqlRenderingError.PredicateContractViolation(
              predicate.ordinal,
              "source-certified exact integral predicate must be one local equality of its declared occurrence"
            )
          }
        }
    }
  }

  private def containsExactIntegralNode(
      expression: ScalarExpression
  ): Boolean = expression match {
    case _: ScalarExpression.ExactIntegralLiteral      => true
    case _: ScalarExpression.ExactIntegralWideningCast => true
    case call: ScalarExpression.Call =>
      call.operands.exists(containsExactIntegralNode)
    case _ => false
  }

  private def validateOutputScalars(
      query: QueryIR,
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      ownerByColumn: Map[ColumnId, BagId],
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
    ]
  ): Unit = {
    query.output.foreach { output =>
      validateRetainedScalarMetadata(
        output.expression,
        s"output ${output.ordinal}",
        metadataByColumn,
        errors
      )
      if (
        output.dataType != output.expression.dataType ||
        output.nullable != output.expression.nullable
      ) {
        errors += PostgreSqlRenderingError.UnsupportedScalar(
          s"output ${output.ordinal}",
          output.expression.digest,
          "output metadata differs from retained expression metadata"
        )
      }
      val refs = output.expression match {
        case column: ScalarExpression.Column => Vector(column.ref)
        case expression =>
          directEquality(expression) match {
            case Some((left, right)) => Vector(left, right)
            case None =>
              errors += PostgreSqlRenderingError.UnsupportedScalar(
                s"output ${output.ordinal}",
                output.expression.digest,
                "only positional columns and deterministic direct equality are supported"
              )
              Vector.empty
          }
      }
      refs.distinct.foreach { ref =>
        if (!metadataByColumn.contains(ref.id) || !ownerByColumn.contains(ref.id)) {
          errors += PostgreSqlRenderingError.AmbiguousColumnOwner(
            ref,
            Vector.empty
          )
        }
      }
    }
  }

  private def validateGlobalCountStarOutput(
      query: QueryIR,
      shape: QueryResultShape.GlobalCountStar,
      output: Vector[PostgreSqlOutputColumn],
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = {
    val exact =
      query.root.isInstanceOf[GlobalCountStarIR] &&
        query.output == Vector(shape.output) &&
        output.size == 1 &&
        output.head.ordinal == 0 &&
        output.head.name == shape.output.name &&
        output.head.dataType == ScalarExpression.GlobalCountStar.dataType &&
        !output.head.nullable &&
        shape.output.expression == ScalarExpression.GlobalCountStar
    if (!exact) {
      errors += PostgreSqlRenderingError.ModelInvariantViolation(
        "Global COUNT(*) result metadata differs from its certified terminal shape"
      )
    }
  }

  private def validateGroupedCountStarOutput(
      query: QueryIR,
      shape: QueryResultShape.GroupedCountStar,
      output: Vector[PostgreSqlOutputColumn],
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      reductionMode: YannakakisReductionMode,
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = {
    val root = query.root match {
      case value: GroupedCountStarIR => Some(value)
      case _                         => None
    }
    validateRetainedScalarMetadata(
      shape.groupColumn,
      "grouped COUNT(*) key",
      metadataByColumn,
      errors
    )
    val exact =
      root.exists(value =>
        value.groupOutput == shape.groupOutput &&
          value.countOutput == shape.countOutput &&
          value.groupColumn == shape.groupColumn &&
          value.sourceCertificate.countAliasMode == shape.countAliasMode
      ) &&
        query.output == Vector(shape.groupOutput, shape.countOutput) &&
        output.size == 2 &&
        output.head.ordinal == 0 &&
        output.head.name == shape.groupOutput.name &&
        output.head.dataType == shape.groupOutput.dataType &&
        output.head.nullable == shape.groupOutput.nullable &&
        output(1).ordinal == 1 &&
        output(1).name == shape.countOutput.name &&
        output(1).dataType == "BIGINT NOT NULL" &&
        !output(1).nullable &&
        metadataByColumn.contains(shape.groupColumn.ref.id)
    if (!exact) {
      errors += PostgreSqlRenderingError.ModelInvariantViolation(
        "Grouped COUNT(*) result metadata differs from its certified terminal shape"
      )
    }
    if (reductionMode != YannakakisReductionMode.FullTwoPass) {
      errors += PostgreSqlRenderingError.GroupedCountRequiresFullTwoPass(
        reductionMode
      )
    }
  }

  private def validateDistributedGroupedCountStarOutput(
      query: QueryIR,
      shape: QueryResultShape.DistributedGroupedCountStar,
      output: Vector[PostgreSqlOutputColumn],
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      reductionMode: YannakakisReductionMode,
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = {
    val keyCount = shape.groupColumns.size
    val root = query.root match {
      case value: DistributedGroupedCountStarIR => Some(value)
      case _                                    => None
    }
    shape.groupColumns.zipWithIndex.foreach { case (column, index) =>
      validateRetainedScalarMetadata(
        column,
        s"distributed grouped COUNT(*) key $index",
        metadataByColumn,
        errors
      )
    }
    val exact =
      root.exists(value =>
        value.groupOutputs == shape.groupOutputs &&
          value.countOutput == shape.countOutput &&
          value.groupColumns == shape.groupColumns &&
          value.sourceCertificate.countAliasMode == shape.countAliasMode
      ) &&
        query.output == (shape.groupOutputs :+ shape.countOutput) &&
        output.size == keyCount + 1 &&
        output.take(keyCount).zip(shape.groupOutputs).zipWithIndex.forall {
          case ((actual, expected), index) =>
            actual.ordinal == index &&
              actual.name == expected.name &&
              actual.dataType == expected.dataType &&
              actual.nullable == expected.nullable &&
              metadataByColumn.contains(shape.groupColumns(index).ref.id)
        } &&
        output(keyCount).ordinal == keyCount &&
        output(keyCount).name == shape.countOutput.name &&
        output(keyCount).dataType == "BIGINT NOT NULL" &&
        !output(keyCount).nullable
    if (!exact) {
      errors += PostgreSqlRenderingError.ModelInvariantViolation(
        "Distributed grouped COUNT(*) result metadata differs from its certified terminal shape"
      )
    }
    if (reductionMode != YannakakisReductionMode.FullTwoPass) {
      errors += PostgreSqlRenderingError
        .DistributedGroupedCountRequiresFullTwoPass(reductionMode)
    }
  }

  private def validateC03G0AggregateVectorOutput(
      query: QueryIR,
      shape: QueryResultShape.C03G0AggregateVector,
      output: Vector[PostgreSqlOutputColumn],
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      ownerByColumn: Map[ColumnId, BagId],
      edgeById: Map[EdgeId, Hyperedge],
      plan: BagFactorPlan,
      reductionMode: YannakakisReductionMode,
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = {
    val root = query.root match {
      case value: C03G0AggregateVectorIR => Some(value)
      case _                             => None
    }
    root.foreach { value =>
      validateRetainedScalarMetadata(
        value.groupColumn,
        "C03-G0 group column",
        metadataByColumn,
        errors
      )
      validateRetainedScalarMetadata(
        value.measureColumn,
        "C03-G0 measure column",
        metadataByColumn,
        errors
      )
    }

    val exactOutputs = root.map(value =>
      Vector(
        value.groupOutput,
        value.countStarOutput,
        value.countGroupOutput,
        value.minOutput,
        value.maxOutput
      )
    )
    val metadataExact =
      root.exists(value =>
        value.groupColumn.ref == shape.groupColumn &&
          value.measureColumn.ref == shape.measureColumn &&
          exactOutputs.contains(shape.outputs) &&
          value.sourceCertificate.catalogBinding == shape.catalogBinding
      ) &&
        query.output == shape.outputs &&
        output.size == 5 &&
        output.zip(shape.outputs).zipWithIndex.forall {
          case ((actual, expected), index) =>
            actual.ordinal == index &&
              actual.name == expected.name &&
              actual.dataType == expected.dataType &&
              actual.nullable == expected.nullable
        } &&
        metadataByColumn.contains(shape.groupColumn.id) &&
        metadataByColumn.contains(shape.measureColumn.id) &&
        shape.groupColumn.relationOccurrenceId ==
          shape.measureColumn.relationOccurrenceId
    if (!metadataExact) {
      errors += PostgreSqlRenderingError.ModelInvariantViolation(
        "C03-G0 result metadata differs from its certified terminal shape"
      )
    }

    val guardEdges = edgeById.valuesIterator
      .filter(
        _.relationOccurrenceId == shape.groupColumn.relationOccurrenceId
      )
      .toVector
    val groupOwner = ownerByColumn.get(shape.groupColumn.id)
    val measureOwner = ownerByColumn.get(shape.measureColumn.id)
    val commonOwner = (groupOwner, measureOwner) match {
      case (Some(group), Some(measure)) if group == measure => Some(group)
      case _                                                => None
    }
    val payloadColumnIds = Set(shape.groupColumn.id, shape.measureColumn.id)
    val payloadIsOwnerLocal = guardEdges.size == 1 &&
      guardEdges.head.bindings.forall { case (columnId, _) =>
        !payloadColumnIds(columnId)
      } && query.predicates.forall(predicate =>
        predicate.expression.columnRefs.forall(ref => !payloadColumnIds(ref.id))
      )
    val ownerExact = payloadIsOwnerLocal && commonOwner.exists { owner =>
      plan.ownership.count(ownership =>
        ownership.edgeId == guardEdges.head.id && ownership.owner == owner
      ) == 1 && plan.factors
        .find(_.bagId == owner)
        .exists(factor =>
          guardEdges.head.vertices.forall(factor.vertices.contains)
        )
    }
    if (!ownerExact) {
      errors += PostgreSqlRenderingError.UnsupportedC03G0Owner(
        shape.groupColumn,
        shape.measureColumn,
        "both direct columns must be nonstructural, predicate-free payloads of one uniquely owned guard edge"
      )
    }
    if (reductionMode != YannakakisReductionMode.FullTwoPass) {
      errors += PostgreSqlRenderingError.C03G0RequiresFullTwoPass(
        reductionMode
      )
    }
  }

  private def validateGlobalGuardedExtremumOutput(
      query: QueryIR,
      shape: QueryResultShape.GlobalGuardedExtremum,
      output: Vector[PostgreSqlOutputColumn],
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      ownerByColumn: Map[ColumnId, BagId],
      edgeById: Map[EdgeId, Hyperedge],
      plan: BagFactorPlan,
      reductionMode: YannakakisReductionMode,
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = {
    val root = query.root match {
      case value: GlobalGuardedExtremumIR => Some(value)
      case _                              => None
    }
    root.foreach(value =>
      validateRetainedScalarMetadata(
        value.extremum.argument,
        "global guarded extremum argument",
        metadataByColumn,
        errors
      )
    )
    val guardEdges = edgeById.valuesIterator
      .filter(_.relationOccurrenceId == shape.guardColumn.relationOccurrenceId)
      .toVector
    val guardOwner = ownerByColumn.get(shape.guardColumn.id)
    val ownerFactor = guardOwner.flatMap(owner =>
      plan.factors.find(_.bagId == owner)
    )
    val exact =
      root.exists(value =>
        value.extremumOutput == shape.output &&
          value.extremum.function == shape.function &&
          value.guardColumn == shape.guardColumn &&
          value.sourceCertificate.aliasMode == shape.aliasMode
      ) &&
        query.output == Vector(shape.output) &&
        output.size == 1 &&
        output.head.ordinal == 0 &&
        output.head.name == shape.output.name &&
        output.head.dataType == shape.output.dataType &&
        output.head.nullable &&
        shape.output.nullable &&
        root.exists(value => shape.output.expression == value.extremum) &&
        metadataByColumn.contains(shape.guardColumn.id) &&
        guardEdges.size == 1 &&
        guardOwner.nonEmpty &&
        plan.ownership.count(ownership =>
          ownership.edgeId == guardEdges.head.id &&
            guardOwner.contains(ownership.owner)
        ) == 1 &&
        ownerFactor.exists(factor =>
          guardEdges.head.vertices.forall(factor.vertices.contains)
        )
    if (!exact) {
      errors += PostgreSqlRenderingError.ModelInvariantViolation(
        "Global guarded MIN/MAX metadata or guard ownership differs from its certified terminal shape"
      )
    }

    // A full two-pass schedule makes every row of every final factor globally
    // extendible. The terminal may therefore aggregate the guard occurrence
    // carried by its owning factor directly. Partial/no-reduction schedules do
    // not have that proof and retain the base-support existential program.
  }

  /**
    * Proves that aggregate terminals may omit the legacy final occurrence-
    * column equalities: both sides must be bindings of the same structural
    * variable in the exact QueryIR-derived hypergraph.
    */
  private def validateFactorizedInnerEqualities(
      predicates: Vector[PredicateDescriptor],
      edgeByOccurrence: Map[RelationOccurrenceId, Hyperedge]
  ): Vector[PostgreSqlRenderingError] = {
    val problems = predicates.flatMap { predicate =>
      predicate.classification match {
        case equality: PredicateClassification.InnerEquality =>
          val left = edgeByOccurrence
            .get(equality.left.relationOccurrenceId)
            .flatMap(_.bindings.find(_._1 == equality.left.id).map(_._2))
          val right = edgeByOccurrence
            .get(equality.right.relationOccurrenceId)
            .flatMap(_.bindings.find(_._1 == equality.right.id).map(_._2))
          if (left.nonEmpty && left == right) Vector.empty
          else
            Vector(
              PostgreSqlRenderingError.ModelInvariantViolation(
                s"Aggregate terminal predicate ${predicate.ordinal} is not represented by one structural variable"
              )
            )
        case _ =>
          Vector(
            PostgreSqlRenderingError.ModelInvariantViolation(
              s"Aggregate terminal predicate ${predicate.ordinal} is not an inner equality"
            )
          )
      }
    }
    PostgreSqlRenderingError.canonical(problems)
  }

  /**
    * Retains only reconstruction-terminal equalities for which the exact
    * compiled factor program does not provide a complete implication proof.
    *
    * For each endpoint, the owning initial factor must demonstrably bind the
    * exact source position to the variable represented by that bag. For
    * endpoints owned by different bags, the unique tree path must keep the
    * variable in every bag and in every emitted reconstruction separator.
    * This is deliberately fail-closed: malformed, ambiguous, or merely
    * plausible structure leaves the source predicate in the terminal SQL.
    */
  private[postgresql] def retainUnprovedReconstructionEqualities(
      descriptors: Vector[PredicateDescriptor],
      compiled: Vector[CompiledPostgreSqlPredicate],
      plan: BagFactorPlan,
      bags: Vector[CompiledPostgreSqlBag],
      edgeByOccurrence: Map[RelationOccurrenceId, Hyperedge]
  ): Vector[CompiledPostgreSqlPredicate] = {
    // The production compiler establishes this one-to-one correspondence
    // before calling the helper. Keep the helper itself fail-closed as well:
    // zip would otherwise truncate a hostile or future mismatched input.
    if (descriptors.size != compiled.size) return compiled

    val bagById = bags.groupBy(_.factor.bagId)
    val ownershipByEdge = plan.ownership.groupBy(_.edgeId)

    def only[A](values: Vector[A]): Option[A] = values match {
      case Vector(value) => Some(value)
      case _             => None
    }

    def exactBinding(
        ref: ColumnRef
    ): Option[(CompiledPostgreSqlBag, PostgreSqlBindingLocation)] =
      for {
        edge <- edgeByOccurrence.get(ref.relationOccurrenceId)
        variableId <- only(
          edge.bindings.collect {
            case (columnId, variable) if columnId == ref.id => variable
          }
        )
        ownership <- ownershipByEdge.get(edge.id).flatMap(only)
        bag <- bagById.get(ownership.owner).flatMap(only)
        occurrence <- only(bag.ownedOccurrences.filter(_.edge.id == edge.id))
        if occurrence.edge == edge
        if occurrence.edge.bindings.count {
          case (columnId, variable) =>
            columnId == ref.id && variable == variableId
        } == 1
        location = PostgreSqlBindingLocation(edge.id, ref.id, variableId)
        if bag.factor.vertices.contains(variableId)
        if (bag.supportStrategy match {
          case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
            bag.ownedRepresentatives.get(variableId).exists { representative =>
              representative.variableId == variableId &&
              // The representative self-comparison is an intentional NULL
              // rejection obligation and is part of this proof even when the
              // requested endpoint is a different binding.
              bag.ownedEqualities.count(
                _ == (representative -> representative)
              ) == 1 &&
              bag.ownedEqualities.count(_ == (representative -> location)) == 1
            }
          case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
            // Materialized bags intentionally have no ownedEqualities
            // inventory. Their authoritative compiled binding inventory is
            // ownedOccurrences: emitInitialFactorWithSupport iterates every
            // exact occurrence.edge.binding and equates it to the named
            // support variable. The checks above prove this endpoint is one
            // such binding; the checks below prove where that support variable
            // originates.
            bag.supportRepresentatives.get(variableId).exists {
              representative =>
                representative.variableId == variableId &&
                bag.supportEdges.count(_.id == representative.edgeId) == 1 &&
                bag.supportEdges.exists(_.bindings.count {
                  case (columnId, variable) =>
                    columnId == representative.columnId &&
                      variable == variableId
                } == 1)
            }
        })
      } yield bag -> location

    def endpointKey(left: BagId, right: BagId): (BagId, BagId) =
      if (left.value <= right.value) left -> right else right -> left

    val arcByEndpoints = plan.arcs.groupBy(arc =>
      endpointKey(arc.parent, arc.child)
    )
    val neighborBuffers = mutable.Map.empty[BagId, mutable.ArrayBuffer[BagId]]
    bags.foreach(bag =>
      neighborBuffers.put(bag.factor.bagId, mutable.ArrayBuffer.empty)
    )
    plan.arcs.foreach { arc =>
      neighborBuffers.get(arc.parent).foreach(_ += arc.child)
      neighborBuffers.get(arc.child).foreach(_ += arc.parent)
    }

    def uniquePath(start: BagId, end: BagId): Option[Vector[BagId]] = {
      if (start == end) Some(Vector(start))
      else if (!neighborBuffers.contains(start) || !neighborBuffers.contains(end))
        None
      else {
        val predecessor = mutable.Map.empty[BagId, BagId]
        val visited = mutable.Set[BagId](start)
        val pending = mutable.Queue[BagId](start)
        while (pending.nonEmpty && !visited(end)) {
          val current = pending.dequeue()
          neighborBuffers(current).toVector.distinct.sorted.foreach { neighbor =>
            if (!visited(neighbor)) {
              visited += neighbor
              predecessor.put(neighbor, current)
              pending.enqueue(neighbor)
            }
          }
        }
        if (!visited(end)) None
        else {
          val reverse = Vector.newBuilder[BagId]
          var current = end
          var complete = true
          reverse += current
          while (current != start && complete) {
            predecessor.get(current) match {
              case Some(parent) =>
                current = parent
                reverse += current
              case None => complete = false
            }
          }
          if (complete) Some(reverse.result().reverse) else None
        }
      }
    }

    def pathProvesRunningIntersection(
        leftOwner: BagId,
        rightOwner: BagId,
        variableId: VariableId
    ): Boolean =
      uniquePath(leftOwner, rightOwner).exists { path =>
        path.forall(bagId =>
          bagById.get(bagId).flatMap(only).exists(
            _.factor.vertices.contains(variableId)
          )
        ) && (path.size == 1 || path.sliding(2).forall {
          case Vector(left, right) =>
            arcByEndpoints
              .get(endpointKey(left, right))
              .flatMap(only)
              .exists(_.separator.contains(variableId))
          case _ => false
        })
      }

    descriptors.zip(compiled).collect {
      case (descriptor, predicate)
          if !(
            descriptor.ordinal == predicate.ordinal &&
              (descriptor.classification match {
                case equality: PredicateClassification.InnerEquality =>
                  predicate.scalar match {
                    case CompiledPostgreSqlScalar.Equality(left, right)
                        if sameEquality(
                          left,
                          right,
                          equality.left,
                          equality.right
                        ) =>
                      (exactBinding(equality.left), exactBinding(equality.right)) match {
                        case (
                              Some((leftBag, leftLocation)),
                              Some((rightBag, rightLocation))
                            )
                            if leftLocation.variableId == rightLocation.variableId =>
                          pathProvesRunningIntersection(
                            leftBag.factor.bagId,
                            rightBag.factor.bagId,
                            leftLocation.variableId
                          )
                        case _ => false
                      }
                    case _ => false
                  }
                case _ => false
              })
          ) => predicate
    }
  }

  private def directEquality(
      expression: ScalarExpression
  ): Option[(ColumnRef, ColumnRef)] = expression match {
    case ScalarExpression.Call(
          "EQUALS",
          "=",
          Vector(
            ScalarExpression.Column(left, _, _, _),
            ScalarExpression.Column(right, _, _, _)
          ),
          _,
          dataType,
          nullable,
          true
        ) if
          (nullable && dataType == "BOOLEAN") ||
            (!nullable && dataType == "BOOLEAN NOT NULL") =>
      Some(left -> right)
    case _ => None
  }

  private final case class CompiledIntegralOperand(
      column: Option[ColumnRef],
      literalValue: Option[Long],
      sourceSqlTypeName: String,
      targetSqlTypeName: String,
      nullable: Boolean
  )

  private def exactIntegralWidening(
      sourceSqlTypeName: String,
      targetSqlTypeName: String
  ): Boolean =
    IntegralTypeRanks.get(sourceSqlTypeName).exists { sourceRank =>
      IntegralTypeRanks
        .get(targetSqlTypeName)
        .exists(targetRank => sourceRank < targetRank)
    }

  private def exactIntegralRange(sqlTypeName: String, value: Long): Boolean =
    sqlTypeName match {
      case "SMALLINT" => value >= Short.MinValue && value <= Short.MaxValue
      case "INTEGER"  => value >= Int.MinValue && value <= Int.MaxValue
      case "BIGINT"   => true
      case _          => false
    }

  private def exactIntegralComparisonType(
      leftSourceSqlTypeName: String,
      rightSourceSqlTypeName: String
  ): Option[String] =
    for {
      leftRank <- IntegralTypeRanks.get(leftSourceSqlTypeName)
      rightRank <- IntegralTypeRanks.get(rightSourceSqlTypeName)
    } yield IntegralTypesByRank(math.max(leftRank, rightRank))

  private def exactTypeDigest(
      sqlTypeName: String,
      nullable: Boolean
  ): String =
    if (nullable) sqlTypeName else s"$sqlTypeName NOT NULL"

  private def compileIntegralColumnOperand(
      expression: ScalarExpression,
      metadataByColumn: Map[ColumnId, ColumnMetadata]
  ): Option[CompiledIntegralOperand] = expression match {
    case column: ScalarExpression.Column =>
      metadataByColumn.get(column.ref.id).filter(metadata =>
        IntegralTypeRanks.contains(metadata.sqlTypeName) &&
          column.dataType == metadata.dataType &&
          column.nullable == metadata.nullable
      ).map(metadata =>
        CompiledIntegralOperand(
          column = Some(column.ref),
          literalValue = None,
          sourceSqlTypeName = metadata.sqlTypeName,
          targetSqlTypeName = metadata.sqlTypeName,
          nullable = metadata.nullable
        )
      )
    case cast: ScalarExpression.ExactIntegralWideningCast =>
      cast.operand match {
        case column: ScalarExpression.Column =>
          compileIntegralColumnOperand(column, metadataByColumn).filter(base =>
            base.sourceSqlTypeName == cast.sourceSqlTypeName &&
              exactIntegralWidening(
                cast.sourceSqlTypeName,
                cast.targetSqlTypeName
              ) &&
              cast.dataType == exactTypeDigest(
                cast.targetSqlTypeName,
                base.nullable
              ) &&
              cast.nullable == base.nullable
          ).map(base =>
            base.copy(targetSqlTypeName = cast.targetSqlTypeName)
          )
        case _ => None
      }
    case _ => None
  }

  private def compileIntegralLiteralOperand(
      expression: ScalarExpression
  ): Option[CompiledIntegralOperand] = expression match {
    case literal: ScalarExpression.ExactIntegralLiteral
        if exactIntegralRange(literal.sqlTypeName, literal.value) &&
          literal.dataType == exactTypeDigest(
            literal.sqlTypeName,
            nullable = false
          ) =>
      Some(
        CompiledIntegralOperand(
          column = None,
          literalValue = Some(literal.value),
          sourceSqlTypeName = literal.sqlTypeName,
          targetSqlTypeName = literal.sqlTypeName,
          nullable = false
        )
      )
    case cast: ScalarExpression.ExactIntegralWideningCast =>
      cast.operand match {
        case literal: ScalarExpression.ExactIntegralLiteral =>
          compileIntegralLiteralOperand(literal).filter(base =>
            base.sourceSqlTypeName == cast.sourceSqlTypeName &&
              exactIntegralWidening(
                cast.sourceSqlTypeName,
                cast.targetSqlTypeName
              ) &&
              cast.dataType == exactTypeDigest(
                cast.targetSqlTypeName,
                nullable = false
              ) &&
              !cast.nullable
          ).map(base =>
            base.copy(targetSqlTypeName = cast.targetSqlTypeName)
          )
        case _ => None
      }
    case _ => None
  }

  private def compileLocalIntegralEquality(
      expression: ScalarExpression,
      metadataByColumn: Map[ColumnId, ColumnMetadata]
  ): Option[CompiledPostgreSqlScalar.LocalIntegralEquality] = expression match {
    case call: ScalarExpression.Call
        if call.sqlKind == "EQUALS" && call.operatorName == "=" &&
          call.deterministic && call.operands.size == 2 =>
      val left = call.operands(0)
      val right = call.operands(1)

      def checked(
          columnExpression: ScalarExpression,
          literalExpression: ScalarExpression,
          literalOnLeft: Boolean
      ): Option[CompiledPostgreSqlScalar.LocalIntegralEquality] =
        for {
          column <- compileIntegralColumnOperand(
            columnExpression,
            metadataByColumn
          )
          literal <- compileIntegralLiteralOperand(literalExpression)
          comparisonType <- exactIntegralComparisonType(
            column.sourceSqlTypeName,
            literal.sourceSqlTypeName
          )
          if column.targetSqlTypeName == comparisonType
          if literal.targetSqlTypeName == comparisonType
          if call.nullable == column.nullable
          if call.dataType == exactTypeDigest("BOOLEAN", column.nullable)
          columnRef <- column.column
          literalValue <- literal.literalValue
        } yield CompiledPostgreSqlScalar.LocalIntegralEquality(
          column = columnRef,
          columnSourceSqlTypeName = column.sourceSqlTypeName,
          columnTargetSqlTypeName = column.targetSqlTypeName,
          literalValue = literalValue,
          literalSourceSqlTypeName = literal.sourceSqlTypeName,
          literalTargetSqlTypeName = literal.targetSqlTypeName,
          literalOnLeft = literalOnLeft
        )

      checked(left, right, literalOnLeft = false).orElse(
        checked(right, left, literalOnLeft = true)
      )
    case _ => None
  }

  private def validateRetainedScalarMetadata(
      expression: ScalarExpression,
      path: String,
      metadataByColumn: Map[ColumnId, ColumnMetadata],
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): Unit = expression match {
    case column: ScalarExpression.Column =>
      metadataByColumn.get(column.ref.id).foreach { metadata =>
        if (
          column.dataType != metadata.dataType ||
          column.nullable != metadata.nullable
        ) {
          errors += PostgreSqlRenderingError.UnsupportedScalar(
            path,
            column.digest,
            "column scalar metadata differs from its source ColumnMetadata"
          )
        }
      }

    case call @ ScalarExpression.Call(
          "EQUALS",
          operatorName,
          Vector(left: ScalarExpression.Column, right: ScalarExpression.Column),
          _,
          dataType,
          nullable,
          deterministic
        ) =>
      validateRetainedScalarMetadata(left, path, metadataByColumn, errors)
      validateRetainedScalarMetadata(right, path, metadataByColumn, errors)
      val expectedNullable = for {
        leftMetadata <- metadataByColumn.get(left.ref.id)
        rightMetadata <- metadataByColumn.get(right.ref.id)
      } yield leftMetadata.nullable || rightMetadata.nullable
      val expectedDataType = expectedNullable.map(value =>
        if (value) "BOOLEAN" else "BOOLEAN NOT NULL"
      )
      if (
        operatorName != "=" || !deterministic ||
        expectedNullable.exists(_ != nullable) ||
        expectedDataType.exists(_ != dataType)
      ) {
        errors += PostgreSqlRenderingError.UnsupportedScalar(
          path,
          call.digest,
          "direct equality operator or BOOLEAN result metadata is inconsistent"
        )
      }

    case call: ScalarExpression.Call
        if call.operands.exists {
          case _: ScalarExpression.ExactIntegralLiteral       => true
          case _: ScalarExpression.ExactIntegralWideningCast  => true
          case _                                              => false
        } =>
      if (compileLocalIntegralEquality(call, metadataByColumn).isEmpty) {
        errors += PostgreSqlRenderingError.UnsupportedScalar(
          path,
          call.digest,
          "typed local integral equality metadata or widening cast is inconsistent"
        )
      }

    case literal: ScalarExpression.ExactIntegralLiteral =>
      errors += PostgreSqlRenderingError.UnsupportedScalar(
        path,
        literal.digest,
        "exact integral literal is retained only inside a typed local equality"
      )

    case cast: ScalarExpression.ExactIntegralWideningCast =>
      errors += PostgreSqlRenderingError.UnsupportedScalar(
        path,
        cast.digest,
        "exact integral widening cast is retained only inside a typed local equality"
      )

    case _ => ()
  }

  private def compileScalar(
      expression: ScalarExpression
  ): Option[CompiledPostgreSqlScalar] = expression match {
    case column: ScalarExpression.Column =>
      Some(CompiledPostgreSqlScalar.Column(column.ref))
    case other =>
      directEquality(other).map { case (left, right) =>
        CompiledPostgreSqlScalar.Equality(left, right)
      }
  }

  private def compileLocalPredicateScalar(
      expression: ScalarExpression,
      metadataByColumn: Map[ColumnId, ColumnMetadata]
  ): Option[CompiledPostgreSqlScalar] = expression match {
    case local: ScalarExpression.LocalPredicate =>
      Some(CompiledPostgreSqlScalar.LocalPredicate(local.tree))
    case _ => compileScalar(expression).orElse(
      compileLocalIntegralEquality(expression, metadataByColumn))
  }

  private def sameEquality(
      actualLeft: ColumnRef,
      actualRight: ColumnRef,
      expectedLeft: ColumnRef,
      expectedRight: ColumnRef
  ): Boolean =
    (actualLeft == expectedLeft && actualRight == expectedRight) ||
      (actualLeft == expectedRight && actualRight == expectedLeft)

  private def compileOccurrenceLiveness(
      query: QueryIR,
      edgeByOccurrence: Map[RelationOccurrenceId, Hyperedge],
      localPredicates: Map[
        RelationOccurrenceId,
        Vector[CompiledPostgreSqlPredicate]
      ],
      innerEqualities: Vector[CompiledPostgreSqlPredicate],
      terminal: CompiledPostgreSqlTerminal
  ): Either[
    Vector[PostgreSqlRenderingError],
    Vector[PostgreSqlOccurrenceLiveness]
  ] = {
    val terminalEqualities =
      if (terminal.reconstructsFactors) innerEqualities else Vector.empty
    val terminalOccurrenceScalars = terminal match {
      case _: CompiledPostgreSqlTerminal.GroupedCountStar => Vector.empty
      case _: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
        Vector.empty
      case _ => terminal.outputScalars
    }
    val postFactorColumnIds: Set[ColumnId] = (
      terminalOccurrenceScalars.iterator.flatMap(_.columnRefs.iterator) ++
        terminalEqualities.iterator.flatMap(_.scalar.columnRefs.iterator)
    ).map(_.id).toSet
    val postFactorColumnIdsByOccurrence =
      postFactorColumnIds.groupBy(columnId => columnId.relationOccurrenceId)
    val errors = Vector.newBuilder[PostgreSqlRenderingError]
    val liveness = query.relations.flatMap { relation =>
      edgeByOccurrence.get(relation.id) match {
        case None =>
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            s"Relation occurrence ${relation.id.value} lost its unique hyperedge during liveness compilation"
          )
          None
        case Some(edge) if edge.relationOccurrenceId != relation.id =>
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            s"Edge ${edge.id.value} is not bound to relation occurrence ${relation.id.value} during liveness compilation"
          )
          None
        case Some(edge) =>
          val structuralColumnIds = edge.bindings.iterator.map(_._1).toSet
          val localColumnIds = localPredicates
            .getOrElse(relation.id, Vector.empty)
            .iterator
            .flatMap(_.scalar.columnRefs.iterator)
            .map(_.id)
            .toSet
          val terminalColumnIds = postFactorColumnIdsByOccurrence.getOrElse(
            relation.id,
            Set.empty[ColumnId]
          )
          val carriedColumnIds =
            if (terminal.reconstructsFactors)
              structuralColumnIds ++ terminalColumnIds
            else terminalColumnIds
          val scanColumnIds =
            structuralColumnIds ++ carriedColumnIds ++ localColumnIds
          Some(
            PostgreSqlOccurrenceLiveness(
              edge,
              relation,
              relation.columns.filter(metadata => scanColumnIds(metadata.ref.id)),
              relation.columns.filter(metadata => carriedColumnIds(metadata.ref.id))
            )
          )
      }
    }
    val canonicalErrors = PostgreSqlRenderingError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) Left(canonicalErrors)
    else Right(liveness)
  }

  /**
    * Independently checks the exact projection demand before any SQL is emitted.
    * A mismatch is an internal lowering error and must fail closed.
    */
  private def validateOccurrenceLiveness(
      query: QueryIR,
      edgeByOccurrence: Map[RelationOccurrenceId, Hyperedge],
      plan: BagFactorPlan,
      localPredicates: Map[
        RelationOccurrenceId,
        Vector[CompiledPostgreSqlPredicate]
      ],
      innerEqualities: Vector[CompiledPostgreSqlPredicate],
      terminal: CompiledPostgreSqlTerminal,
      occurrenceLiveness: Vector[PostgreSqlOccurrenceLiveness],
      bags: Vector[CompiledPostgreSqlBag],
      ownerByCarriedColumn: Map[ColumnId, BagId],
      bagTrace: Vector[PostgreSqlBagCteTrace]
  ): Vector[PostgreSqlRenderingError] = {
    val errors = Vector.newBuilder[PostgreSqlRenderingError]
    def invariant(detail: String): Unit =
      errors += PostgreSqlRenderingError.ModelInvariantViolation(detail)

    val queryTerminalEqualities =
      if (terminal.reconstructsFactors)
        query.predicates.iterator.collect {
          case predicate
              if predicate.classification
                .isInstanceOf[PredicateClassification.InnerEquality] =>
            predicate.expression
        }.toVector
      else Vector.empty
    val compiledTerminalEqualities =
      if (terminal.reconstructsFactors) innerEqualities else Vector.empty
    val queryTerminalOutputs = terminal match {
      case _: CompiledPostgreSqlTerminal.GroupedCountStar =>
        query.output.filterNot(_.ordinal == 0)
      case count: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
        query.output.filter(_.ordinal == count.countOutput.ordinal)
      case _ => query.output
    }
    val compiledTerminalScalars = terminal match {
      case _: CompiledPostgreSqlTerminal.GroupedCountStar => Vector.empty
      case _: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
        Vector.empty
      case _ => terminal.outputScalars
    }
    val queryPostFactorColumnIds = (
      queryTerminalOutputs.iterator.flatMap(_.expression.columnRefs.iterator) ++
        queryTerminalEqualities.iterator.flatMap(_.columnRefs.iterator)
    ).map(_.id).toSet
    val compiledPostFactorColumnIds = (
      compiledTerminalScalars.iterator.flatMap(_.columnRefs.iterator) ++
        compiledTerminalEqualities.iterator.flatMap(_.scalar.columnRefs.iterator)
    ).map(_.id).toSet
    terminal match {
      case aggregate: CompiledPostgreSqlTerminal.C03G0AggregateVector =>
        val expected = Set(
          aggregate.groupColumn.ref.id,
          aggregate.measureColumn.ref.id
        )
        if (
          queryPostFactorColumnIds != expected ||
          compiledPostFactorColumnIds != expected
        ) {
          invariant(
            "C03-G0 post-factor liveness must contain exactly its owner-local G and X columns"
          )
        }
      case _ => ()
    }
    val queryPostFactorColumnIdsByOccurrence =
      queryPostFactorColumnIds.groupBy(_.relationOccurrenceId)
    val compiledPostFactorColumnIdsByOccurrence =
      compiledPostFactorColumnIds.groupBy(_.relationOccurrenceId)
    val queryLocalColumnIdsByOccurrence = query.predicates.iterator.collect {
      case predicate @ PredicateDescriptor(
            _,
            _,
            _,
            PredicateClassification.LocalFilter(occurrenceId)
          ) => occurrenceId -> predicate.expression.columnRefs.map(_.id)
    }.toVector.groupBy(_._1).iterator.map { case (occurrenceId, values) =>
      occurrenceId -> values.iterator.flatMap(_._2.iterator).toSet
    }.toMap
    if (compiledPostFactorColumnIds != queryPostFactorColumnIds) {
      invariant("Compiled post-factor column obligations differ from QueryIR")
    }

    val livenessByOccurrence = occurrenceLiveness.groupBy(_.relation.id)
    val livenessByEdge = occurrenceLiveness.groupBy(_.edge.id)
    if (
      occurrenceLiveness.size != query.relations.size ||
      livenessByOccurrence.exists(_._2.size != 1) ||
      livenessByEdge.exists(_._2.size != 1)
    ) {
      invariant(
        "Occurrence liveness must contain exactly one entry per relation occurrence and edge"
      )
    }

    query.relations.foreach { relation =>
      val expectedEdge = edgeByOccurrence.get(relation.id)
      val actual = livenessByOccurrence.getOrElse(relation.id, Vector.empty)
      (expectedEdge, actual) match {
        case (Some(edge), Vector(value)) =>
          val structuralColumnIds = edge.bindings.iterator.map(_._1).toSet
          val queryLocalColumnIds = queryLocalColumnIdsByOccurrence
            .getOrElse(relation.id, Set.empty[ColumnId])
          val compiledLocalColumnIds = localPredicates
            .getOrElse(relation.id, Vector.empty)
            .iterator
            .flatMap(_.scalar.columnRefs.iterator)
            .map(_.id)
            .toSet
          if (compiledLocalColumnIds != queryLocalColumnIds) {
            invariant(
              s"Compiled local column obligations differ for relation occurrence ${relation.id.value}"
            )
          }
          val queryPostFactorIds = queryPostFactorColumnIdsByOccurrence
            .getOrElse(relation.id, Set.empty[ColumnId])
          val compiledPostFactorIds = compiledPostFactorColumnIdsByOccurrence
            .getOrElse(relation.id, Set.empty[ColumnId])
          if (compiledPostFactorIds != queryPostFactorIds) {
            invariant(
              s"Compiled post-factor column obligations differ for relation occurrence ${relation.id.value}"
            )
          }
          val expectedCarriedIds =
            if (terminal.reconstructsFactors)
              structuralColumnIds ++ queryPostFactorIds
            else queryPostFactorIds
          val expectedScanIds =
            structuralColumnIds ++ expectedCarriedIds ++ queryLocalColumnIds
          val expectedCarried = relation.columns.filter(metadata =>
            expectedCarriedIds(metadata.ref.id)
          )
          val expectedScan = relation.columns.filter(metadata =>
            expectedScanIds(metadata.ref.id)
          )
          if (
            value.edge != edge || value.relation != relation ||
            value.carriedColumns != expectedCarried ||
            value.scanColumns != expectedScan
          ) {
            invariant(
              s"Occurrence liveness is not the exact canonical projection for edge ${edge.id.value}"
            )
          }
        case _ => ()
      }
    }

    val compiledOwnership = bags.iterator
      .flatMap(bag =>
        bag.ownedOccurrences.iterator.map(occurrence =>
          occurrence.edge.id -> (bag.factor.bagId -> occurrence)
        )
      )
      .toVector
      .groupBy(_._1)
    val expectedOwnerByEdge = plan.ownership.iterator
      .map(ownership => ownership.edgeId -> ownership.owner)
      .toMap
    expectedOwnerByEdge.foreach { case (edgeId, expectedOwner) =>
      compiledOwnership.getOrElse(edgeId, Vector.empty).map(_._2) match {
        case Vector((actualOwner, actualLiveness))
            if actualOwner == expectedOwner &&
              livenessByEdge.get(edgeId).contains(Vector(actualLiveness)) => ()
        case _ =>
          invariant(
            s"Edge ${edgeId.value} does not have exactly one canonical compiled owner occurrence"
          )
      }
    }
    if (compiledOwnership.keySet != expectedOwnerByEdge.keySet) {
      invariant("Compiled occurrence ownership differs from the bag-factor plan")
    }

    val expectedOwnerByCarriedColumn = occurrenceLiveness.iterator
      .flatMap { liveness =>
        expectedOwnerByEdge.get(liveness.edge.id).iterator.flatMap(owner =>
          liveness.carriedColumns.iterator.map(metadata => metadata.ref.id -> owner)
        )
      }
      .toMap
    if (ownerByCarriedColumn != expectedOwnerByCarriedColumn) {
      invariant("Final column ownership differs from carried occurrence columns")
    }
    if (!compiledPostFactorColumnIds.subsetOf(ownerByCarriedColumn.keySet)) {
      invariant("A post-factor scalar refers to a column that is not carried")
    }

    val traceByBag = bagTrace.iterator.map(trace => trace.bagId -> trace).toMap
    bags.foreach { bag =>
      val expectedOccurrenceTrace = bag.ownedOccurrences.flatMap { occurrence =>
        occurrence.carriedColumns.map { metadata =>
          PostgreSqlOccurrenceColumnTrace(
            occurrence.edge.id,
            metadata.ref.id,
            PostgreSqlNames.occurrenceColumn(occurrence.edge.id, metadata.ref.id)
          )
        }
      }
      traceByBag.get(bag.factor.bagId) match {
        case Some(trace) =>
          val expectedVariableTrace =
            bag.physicalProjection.structuralVariables.map(variableId =>
              PostgreSqlVariableColumnTrace(
                variableId,
                PostgreSqlNames.variable(variableId)
              )
            )
          if (trace.variableColumns != expectedVariableTrace) {
            invariant(
              s"Trace variable columns differ from emitted projection for bag ${bag.factor.bagId.value}"
            )
          }
          if (trace.occurrenceColumns != expectedOccurrenceTrace) {
            invariant(
              s"Trace occurrence columns differ from emitted projection for bag ${bag.factor.bagId.value}"
            )
          }
          val generatedColumnNames =
            trace.variableColumns.map(_.columnName) ++
              trace.occurrenceColumns.map(_.columnName) ++
              bag.physicalProjection.unitColumn.toVector
          if (generatedColumnNames.distinct.size != generatedColumnNames.size) {
            invariant(
              s"Generated factor column names collide in bag ${bag.factor.bagId.value}"
            )
          }
        case None =>
          invariant(s"Trace is missing bag ${bag.factor.bagId.value}")
      }
    }
    if (traceByBag.keySet != bags.iterator.map(_.factor.bagId).toSet) {
      invariant("Trace bag identities differ from compiled bags")
    }

    PostgreSqlRenderingError.canonical(errors.result())
  }

  private def compileBag(
      factor: BagFactor,
      edgeById: Map[EdgeId, Hyperedge],
      livenessByEdge: Map[EdgeId, PostgreSqlOccurrenceLiveness],
      errors: mutable.Builder[
        PostgreSqlRenderingError,
        Vector[PostgreSqlRenderingError]
      ]
  ): CompiledPostgreSqlBag = {
    val supportEdges = factor.supportCover.flatMap(edgeById.get)
    val ownedEdges = factor.ownedEdgeIds.flatMap(edgeById.get)
    val ownedOccurrences = factor.ownedEdgeIds.flatMap { edgeId =>
      livenessByEdge.get(edgeId) match {
        case Some(liveness) => Some(liveness)
        case None =>
          errors += PostgreSqlRenderingError.ModelInvariantViolation(
            s"Owned edge ${edgeId.value} has no compiled occurrence liveness"
          )
          None
      }
    }
    val ownedEdgeIds = factor.ownedEdgeIds.toSet
    val supportStrategy =
      if (factor.supportCover.forall(ownedEdgeIds))
        PostgreSqlBagSupportStrategy.ElidedByOwnedCover
      else PostgreSqlBagSupportStrategy.MaterializedDistinctSupport

    val supportLocations = supportEdges.flatMap { edge =>
      edge.bindings.map { case (columnId, variableId) =>
        PostgreSqlBindingLocation(edge.id, columnId, variableId)
      }
    }
    val groupedSupport = supportLocations.groupBy(_.variableId)
    val supportRepresentatives = factor.vertices.flatMap { variableId =>
      groupedSupport
        .get(variableId)
        .flatMap(canonicalLocations(_).headOption) match {
        case Some(location) => Some(variableId -> location)
        case None =>
          errors += PostgreSqlRenderingError.MissingSupportBinding(
            factor.bagId,
            variableId
          )
          None
      }
    }.toMap
    val supportEqualities = groupedSupport.toVector.sortBy(_._1).flatMap {
      case (_, locations) =>
        val canonical = canonicalLocations(locations)
        canonical.headOption.toVector.flatMap(head =>
          canonical.tail.map(head -> _)
        )
    }

    val bagVariables = factor.vertices.toSet
    ownedEdges.foreach { edge =>
      edge.bindings.foreach { case (_, variableId) =>
        if (!bagVariables(variableId)) {
          errors += PostgreSqlRenderingError.OwnedBindingOutsideBag(
            factor.bagId,
            edge.id,
            variableId
          )
        }
      }
    }

    val ownedLocations = ownedEdges.flatMap { edge =>
      edge.bindings.map { case (columnId, variableId) =>
        PostgreSqlBindingLocation(edge.id, columnId, variableId)
      }
    }
    val groupedOwned = ownedLocations.groupBy(_.variableId)
    val ownedRepresentatives = supportStrategy match {
      case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
        factor.vertices.flatMap { variableId =>
          groupedOwned
            .get(variableId)
            .flatMap(canonicalLocations(_).headOption) match {
            case Some(location) => Some(variableId -> location)
            case None =>
              errors += PostgreSqlRenderingError.MissingOwnedBinding(
                factor.bagId,
                variableId
              )
              None
          }
        }.toMap
      case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
        Map.empty[VariableId, PostgreSqlBindingLocation]
    }
    val ownedEqualities = supportStrategy match {
      case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
        groupedOwned.toVector.sortBy(_._1).flatMap { case (_, locations) =>
          val canonical = canonicalLocations(locations)
          canonical.headOption.toVector.flatMap(representative =>
            canonical.map(representative -> _)
          )
        }
      case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
        Vector.empty
    }

    CompiledPostgreSqlBag(
      factor,
      PostgreSqlPhysicalFactorProjectionV2(
        factor.vertices,
        ownedOccurrences.flatMap { occurrence =>
          occurrence.carriedColumns.map(metadata =>
            PostgreSqlOccurrenceColumnTrace(
              occurrence.edge.id,
              metadata.ref.id,
              PostgreSqlNames.occurrenceColumn(
                occurrence.edge.id,
                metadata.ref.id
              )
            )
          )
        },
        None
      ),
      supportStrategy,
      supportEdges,
      ownedOccurrences,
      supportRepresentatives,
      supportEqualities,
      ownedRepresentatives,
      ownedEqualities
    )
  }

  private def canonicalLocations(
      values: Vector[PostgreSqlBindingLocation]
  ): Vector[PostgreSqlBindingLocation] =
    values.sortBy(value => (value.edgeId.value, value.columnId))

  private def selectReductionSchedule(
      plan: BagFactorPlan,
      mode: YannakakisReductionMode
  ): CompiledPostgreSqlReductionSchedule = mode match {
    case YannakakisReductionMode.FullTwoPass =>
      CompiledPostgreSqlReductionSchedule(
        mode,
        plan.schedule.bottomUp,
        plan.schedule.topDown
      )
    case YannakakisReductionMode.BottomUpOnly =>
      CompiledPostgreSqlReductionSchedule(
        mode,
        plan.schedule.bottomUp,
        Vector.empty
      )
    case YannakakisReductionMode.NoReduction =>
      CompiledPostgreSqlReductionSchedule(mode, Vector.empty, Vector.empty)
  }

  /**
    * Derives the exact schema installed at each bag's globally-extendible
    * reduction boundary. An empty map means every reduction must retain raw
    * rows. A nonempty map is total over the factor plan so a later emitter
    * cannot silently mix compact and legacy state shapes.
    */
  private def deriveReductionFinalizations(
      plan: BagFactorPlan,
      terminal: CompiledPostgreSqlTerminal,
      aggregateRenderingProfile: PostgreSqlAggregateRenderingProfile
  ): Either[
    Vector[PostgreSqlRenderingError],
    Map[BagId, CompiledPostgreSqlReductionFinalization]
  ] = {
    import CompiledPostgreSqlReductionFinalization._

    if (plan.arcs.isEmpty) return Right(Map.empty)

    val derived: Map[BagId, CompiledPostgreSqlReductionFinalization] =
      aggregateRenderingProfile.reductionFinalization match {
        case PostgreSqlAggregateReductionFinalizationProfile
              .LegacyFullVerticesV1 =>
          if (terminal.usesStagedCountFrequency(plan))
            plan.factors.iterator.map(factor =>
              factor.bagId -> LegacyFullVerticesFrequency(factor.vertices)
            ).toMap
          else Map.empty

        case PostgreSqlAggregateReductionFinalizationProfile
              .CompactLocalStateV2 =>
          terminal match {
            case count: CompiledPostgreSqlTerminal.GlobalCountStar =>
              count.strategy match {
                case CompiledPostgreSqlTerminal.GlobalCountStrategy
                      .FrequencyPropagation(frequencyPlan) =>
                  frequencyPlan.bags.iterator.map { bag =>
                    bag.bagId -> CompactFrequency(
                      bag.localGrouping,
                      suppressEmptyGroup = bag.localGrouping.isEmpty
                    )
                  }.toMap
                case CompiledPostgreSqlTerminal.GlobalCountStrategy
                      .RawFrequencyPropagation(_) => Map.empty
                case CompiledPostgreSqlTerminal.GlobalCountStrategy
                      .Reconstruction => Map.empty
              }

            case count: CompiledPostgreSqlTerminal.GroupedCountStar =>
              count.frequencyPlan.bags.iterator.map { bag =>
                bag.bagId -> CompactFrequency(
                  bag.localGrouping,
                  suppressEmptyGroup = bag.localGrouping.isEmpty
                )
              }.toMap

            case count: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
              count.propagationPlan.bags.iterator.map { bag =>
                bag.bagId -> CompactFrequency(
                  bag.localGroupingVariables,
                  suppressEmptyGroup = bag.localGroupingVariables.isEmpty
                )
              }.toMap

            case aggregate: CompiledPostgreSqlTerminal.C03G0AggregateVector =>
              aggregate.propagationPlan.bags.iterator.map { bag =>
                val finalization: CompiledPostgreSqlReductionFinalization =
                  if (bag.bagId == aggregate.aggregateBagId)
                    CompactC03Owner(
                      bag.localGroupingVariables,
                      aggregate.groupColumn.ref.id,
                      aggregate.measureColumn.ref.id
                    )
                  else
                    CompactFrequency(
                      bag.localGroupingVariables,
                      suppressEmptyGroup = bag.localGroupingVariables.isEmpty
                    )
                bag.bagId -> finalization
              }.toMap

            case _: CompiledPostgreSqlTerminal.Projection |
                _: CompiledPostgreSqlTerminal.GlobalGuardedExtremum =>
              Map.empty
          }
      }

    if (derived.isEmpty) Right(derived)
    else {
      val factorById = plan.factors.iterator
        .map(factor => factor.bagId -> factor)
        .toMap
      val expectedBagIds = factorById.keySet
      val invalidSignature = derived.toVector.sortBy(_._1).collectFirst {
        case (bagId, finalization)
            if !factorById.get(bagId).exists { factor =>
              val groupingVariables = finalization match {
                case LegacyFullVerticesFrequency(grouping) => grouping
                case CompactFrequency(grouping, _)          => grouping
                case CompactC03Owner(grouping, _, _)        => grouping
                case RetainRawRows                          => Vector.empty
              }
              finalization != RetainRawRows &&
              groupingVariables.forall(factor.vertices.contains)
            } =>
          bagId
      }
      if (derived.keySet != expectedBagIds)
        Left(
          Vector(
            PostgreSqlRenderingError.ModelInvariantViolation(
              "Aggregate reduction-finalization plan must cover every factor exactly once"
            )
          )
        )
      else if (invalidSignature.nonEmpty)
        Left(
          Vector(
            PostgreSqlRenderingError.ModelInvariantViolation(
              s"Aggregate reduction-finalization grouping is not a target-bag subset for bag ${invalidSignature.get.value}"
            )
          )
        )
      else Right(derived)
    }
  }

  private def compileTrace(
      plan: BagFactorPlan,
      bags: Vector[CompiledPostgreSqlBag],
      schedule: CompiledPostgreSqlReductionSchedule,
      finalizationsByBag: Map[
        BagId,
        CompiledPostgreSqlReductionFinalization
      ]
  ): Either[
    Vector[PostgreSqlRenderingError],
    (
        Vector[PostgreSqlBagCteTrace],
        Vector[PostgreSqlSemijoinCteTrace],
        Vector[CompiledPostgreSqlReductionCte]
    )
  ] = {
    val current = mutable.Map.empty[BagId, String]
    plan.factors.foreach(factor =>
      current.put(factor.bagId, PostgreSqlNames.initialFactor(factor.bagId))
    )
    val allSteps = schedule.allSteps
    var envelopeIndex = 0
    while (envelopeIndex < allSteps.size) {
      validateReductionStepEnvelope(allSteps(envelopeIndex), envelopeIndex) match {
        case Some(problem) => return Left(Vector(problem))
        case None          => ()
      }
      envelopeIndex += 1
    }
    val lastStepIndexByTarget = allSteps.zipWithIndex
      .groupBy(_._1.target)
      .iterator
      .map { case (bagId, values) => bagId -> values.map(_._2).max }
      .toMap
    val reductions = Vector.newBuilder[CompiledPostgreSqlReductionCte]
    val semijoinTrace = Vector.newBuilder[PostgreSqlSemijoinCteTrace]
    var start = 0
    while (start < allSteps.size) {
      val first = allSteps(start)
      validateReductionStepEnvelope(first, start) match {
        case Some(problem) => return Left(Vector(problem))
        case None          => ()
      }

      var end = start + 1
      while (
        end < allSteps.size &&
        allSteps(end) != null &&
        allSteps(end).pass == first.pass &&
        allSteps(end).target == first.target
      ) {
        end += 1
      }
      if (end < allSteps.size) {
        validateReductionStepEnvelope(allSteps(end), end) match {
          case Some(problem) => return Left(Vector(problem))
          case None          => ()
        }
      }

      val targetInputCte = current.get(first.target) match {
        case Some(value) => value
        case None =>
          return reductionInvariant(
            s"step $start targets unknown bag ${first.target.value}"
          )
      }
      val outputCte = PostgreSqlNames.semijoin(allSteps(end - 1))
      val members = Vector.newBuilder[CompiledPostgreSqlReductionMember]
      var index = start
      while (index < end) {
        val step = allSteps(index)
        validateReductionStepEnvelope(step, index) match {
          case Some(problem) => return Left(Vector(problem))
          case None          => ()
        }
        if (step.source == step.target) {
          return reductionInvariant(
            s"step $index uses target bag ${step.target.value} as its own source"
          )
        }
        val sourceInputCte = current.get(step.source) match {
          case Some(value) => value
          case None =>
            return reductionInvariant(
              s"step $index reads unknown source bag ${step.source.value}"
            )
        }
        val trace = PostgreSqlSemijoinCteTrace(
          step.pass,
          step.ordinal,
          step.target,
          step.source,
          targetInputCte,
          sourceInputCte,
          outputCte
        )
        members += CompiledPostgreSqlReductionMember(step, trace)
        semijoinTrace += trace
        index += 1
      }
      val finalization =
        if (lastStepIndexByTarget.get(first.target).contains(end - 1))
          finalizationsByBag.getOrElse(
            first.target,
            CompiledPostgreSqlReductionFinalization.RetainRawRows
          )
        else CompiledPostgreSqlReductionFinalization.RetainRawRows
      reductions += CompiledPostgreSqlReductionCte(
        first.pass,
        first.target,
        targetInputCte,
        outputCte,
        members.result(),
        finalization
      )
      current.update(first.target, outputCte)
      start = end
    }

    val bagById = bags.iterator.map(bag => bag.factor.bagId -> bag).toMap
    val bagTrace = Vector.newBuilder[PostgreSqlBagCteTrace]
    val sortedFactors = plan.factors.sortBy(_.bagId)
    var factorIndex = 0
    while (factorIndex < sortedFactors.size) {
      val factor = sortedFactors(factorIndex)
      val compiledBag = bagById.get(factor.bagId) match {
        case Some(value) => value
        case None =>
          return reductionInvariant(
            s"compiled bag ${factor.bagId.value} is unavailable to trace construction"
          )
      }
      val finalFactorCte = current.get(factor.bagId) match {
        case Some(value) => value
        case None =>
          return reductionInvariant(
            s"bag ${factor.bagId.value} has no current physical CTE version"
          )
      }
      val occurrenceColumns = compiledBag.physicalProjection.occurrenceColumns
      bagTrace += PostgreSqlBagCteTrace(
        factor.bagId,
        compiledBag.supportStrategy,
        compiledBag.supportStrategy match {
          case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
            Some(PostgreSqlNames.support(factor.bagId))
          case PostgreSqlBagSupportStrategy.ElidedByOwnedCover => None
        },
        PostgreSqlNames.initialFactor(factor.bagId),
        finalFactorCte,
        compiledBag.physicalProjection.structuralVariables.map(variableId =>
          PostgreSqlVariableColumnTrace(
            variableId,
            PostgreSqlNames.variable(variableId)
          )
        ),
        occurrenceColumns
      )
      factorIndex += 1
    }
    Right((bagTrace.result(), semijoinTrace.result(), reductions.result()))
  }

  private def validateReductionStepEnvelope(
      step: SemijoinStep,
      index: Int
  ): Option[PostgreSqlRenderingError] = {
    val detail =
      if (step == null) Some(s"schedule step $index is null")
      else if (step.pass == null) Some(s"schedule step $index has no pass")
      else if (step.target == null) Some(s"schedule step $index has no target")
      else if (step.source == null) Some(s"schedule step $index has no source")
      else if (step.separator == null)
        Some(s"schedule step $index has no separator")
      else None
    detail.map(value =>
      PostgreSqlRenderingError.ModelInvariantViolation(
        s"Physical semijoin reduction invariant violated: $value"
      )
    )
  }

  private def reductionInvariant[A](
      detail: String
  ): Left[Vector[PostgreSqlRenderingError], A] =
    Left(
      Vector(
        PostgreSqlRenderingError.ModelInvariantViolation(
          s"Physical semijoin reduction invariant violated: $detail"
        )
      )
    )

  private def compileStageTrace(
      plan: BagFactorPlan,
      compiledBags: Vector[CompiledPostgreSqlBag],
      bagTrace: Vector[PostgreSqlBagCteTrace],
      reductions: Vector[CompiledPostgreSqlReductionCte],
      terminal: CompiledPostgreSqlTerminal,
      cteMaterialization: PostgreSqlCteMaterializationDirective
  ): Either[Vector[PostgreSqlRenderingError], Vector[PostgreSqlStageTrace]] = {
    import PostgreSqlStageId.{
      BagFactorVersion,
      BagSupport,
      FinalReconstruction
    }

    def failure[A](detail: String): Either[Vector[PostgreSqlRenderingError], A] =
      Left(
        Vector(
          PostgreSqlRenderingError.ModelInvariantViolation(
            s"PostgreSQL stage trace construction failed: $detail"
          )
        )
      )

    val namedCteForm = stagePhysicalForm(cteMaterialization)
    val factors = plan.factors.sortBy(_.bagId)
    val compiledByBag = compiledBags.iterator
      .map(bag => bag.factor.bagId -> bag)
      .toMap
    val traceByBag = bagTrace.iterator.map(trace => trace.bagId -> trace).toMap
    if (
      factors.size != compiledBags.size ||
      compiledByBag.size != compiledBags.size ||
      traceByBag.size != bagTrace.size ||
      factors.map(_.bagId) != bagTrace.map(_.bagId)
    ) {
      return failure("bag provenance is not unique and canonical")
    }

    val stages = Vector.newBuilder[PostgreSqlStageTrace]
    var factorIndex = 0
    while (factorIndex < factors.size) {
      val factor = factors(factorIndex)
      val compiled = compiledByBag.get(factor.bagId) match {
        case Some(value) => value
        case None =>
          return failure(
            s"bag ${factor.bagId.value} has no compiled support provenance"
          )
      }
      val trace = traceByBag.get(factor.bagId) match {
        case Some(value) => value
        case None =>
          return failure(
            s"bag ${factor.bagId.value} has no physical CTE provenance"
          )
      }
      val (physicalForm, physicalCteName) = compiled.supportStrategy match {
        case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
          namedCteForm -> trace.supportCte
        case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
          PostgreSqlStagePhysicalForm.Elided -> None
      }
      stages += PostgreSqlStageTrace(
        id = BagSupport(factor.bagId),
        kind = PostgreSqlStageKind.Support,
        physicalForm = physicalForm,
        physicalCteName = physicalCteName,
        logicalBagIds = Vector(factor.bagId),
        logicalEdgeIds = factor.supportCover.sorted,
        semijoinMembers = Vector.empty,
        inputStageIds = Vector.empty
      )
      factorIndex += 1
    }

    val current = mutable.Map.empty[BagId, BagFactorVersion]
    val versions = mutable.Map.empty[BagId, Int]
    factorIndex = 0
    while (factorIndex < factors.size) {
      val factor = factors(factorIndex)
      val compiled = compiledByBag(factor.bagId)
      val trace = traceByBag(factor.bagId)
      val id = BagFactorVersion(factor.bagId, 0)
      val inputs = compiled.supportStrategy match {
        case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
          Vector[PostgreSqlStageId](BagSupport(factor.bagId))
        case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
          Vector.empty[PostgreSqlStageId]
      }
      stages += PostgreSqlStageTrace(
        id = id,
        kind = PostgreSqlStageKind.InitialFactor,
        physicalForm = namedCteForm,
        physicalCteName = Some(trace.initialFactorCte),
        logicalBagIds = Vector(factor.bagId),
        logicalEdgeIds = factor.ownedEdgeIds.sorted,
        semijoinMembers = Vector.empty,
        inputStageIds = inputs
      )
      current.put(factor.bagId, id)
      versions.put(factor.bagId, 0)
      factorIndex += 1
    }

    var reductionIndex = 0
    while (reductionIndex < reductions.size) {
      val reduction = reductions(reductionIndex)
      if (reduction == null || reduction.members == null || reduction.members.isEmpty) {
        return failure(s"physical reduction $reductionIndex has no members")
      }
      val target = reduction.target
      val previousTarget = current.get(target) match {
        case Some(value) => value
        case None =>
          return failure(
            s"physical reduction $reductionIndex targets an unknown bag"
          )
      }
      val previousVersion = versions.get(target) match {
        case Some(value) => value
        case None =>
          return failure(
            s"physical reduction $reductionIndex has no target version"
          )
      }
      if (previousVersion == Int.MaxValue) {
        return failure(
          s"physical reduction $reductionIndex overflows the target version"
        )
      }
      val id = BagFactorVersion(target, previousVersion + 1)
      val inputs = Vector.newBuilder[PostgreSqlStageId]
      inputs += previousTarget
      val members = Vector.newBuilder[PostgreSqlStageSemijoinMember]
      val logicalBags = Vector.newBuilder[BagId]
      logicalBags += target
      var memberIndex = 0
      while (memberIndex < reduction.members.size) {
        val member = reduction.members(memberIndex)
        if (member == null || member.step == null) {
          return failure(
            s"physical reduction $reductionIndex member $memberIndex is missing"
          )
        }
        val step = member.step
        val sourceVersion = current.get(step.source) match {
          case Some(value) => value
          case None =>
            return failure(
              s"physical reduction $reductionIndex member $memberIndex reads an unknown bag"
            )
        }
        inputs += sourceVersion
        if (step.source != target) logicalBags += step.source
        members += PostgreSqlStageSemijoinMember(
          step.pass,
          step.ordinal,
          step.target,
          step.source,
          step.separator
        )
        memberIndex += 1
      }
      stages += PostgreSqlStageTrace(
        id = id,
        kind = PostgreSqlStageKind.FusedReduction,
        physicalForm = namedCteForm,
        physicalCteName = Some(reduction.outputCte),
        logicalBagIds = logicalBags.result().distinct,
        logicalEdgeIds = Vector.empty,
        semijoinMembers = members.result(),
        inputStageIds = inputs.result()
      )
      current.update(target, id)
      versions.update(target, previousVersion + 1)
      reductionIndex += 1
    }

    val terminalProvenance = terminalStageProvenance(plan, terminal)
    val terminalBags = terminalProvenance.logicalBagIds
    val terminalInputs = Vector.newBuilder[PostgreSqlStageId]
    if (terminalProvenance.readsFactorStages) {
      factorIndex = 0
      while (factorIndex < terminalBags.size) {
        val bagId = terminalBags(factorIndex)
        current.get(bagId) match {
          case Some(value) => terminalInputs += value
          case None =>
            return failure(
              s"terminal SELECT reads unknown bag ${bagId.value}"
            )
        }
        factorIndex += 1
      }
    }
    stages += PostgreSqlStageTrace(
      id = FinalReconstruction,
      kind = PostgreSqlStageKind.FinalReconstruction,
      physicalForm = PostgreSqlStagePhysicalForm.TerminalSelect,
      physicalCteName = None,
      logicalBagIds = terminalBags,
      logicalEdgeIds = terminalProvenance.logicalEdgeIds,
      semijoinMembers = Vector.empty,
      inputStageIds = terminalInputs.result()
    )
    Right(stages.result())
  }

  /**
    * Validates a stage trace against renderer-owned plan structure. Kept
    * package-visible so focused tests can prove corrupted provenance fails
    * closed without exposing an unsafe trace constructor to API callers.
    */
  private[postgresql] def validateStageTrace(
      plan: BagFactorPlan,
      compiledBags: Vector[CompiledPostgreSqlBag],
      bagTrace: Vector[PostgreSqlBagCteTrace],
      schedule: CompiledPostgreSqlReductionSchedule,
      reductions: Vector[CompiledPostgreSqlReductionCte],
      stages: Vector[PostgreSqlStageTrace]
  ): Vector[PostgreSqlRenderingError] =
    validateStageTrace(
      plan,
      compiledBags,
      bagTrace,
      schedule,
      reductions,
      stages,
      PostgreSqlCteMaterializationDirective.PlannerControlled
    )

  private[postgresql] def validateStageTrace(
      plan: BagFactorPlan,
      compiledBags: Vector[CompiledPostgreSqlBag],
      bagTrace: Vector[PostgreSqlBagCteTrace],
      schedule: CompiledPostgreSqlReductionSchedule,
      reductions: Vector[CompiledPostgreSqlReductionCte],
      stages: Vector[PostgreSqlStageTrace],
      cteMaterialization: PostgreSqlCteMaterializationDirective
  ): Vector[PostgreSqlRenderingError] =
    validateStageTrace(
      plan,
      compiledBags,
      bagTrace,
      schedule,
      reductions,
      stages,
      plan.root +: plan.arcs.map(_.child),
      cteMaterialization
    )

  private def validateStageTrace(
      plan: BagFactorPlan,
      compiledBags: Vector[CompiledPostgreSqlBag],
      bagTrace: Vector[PostgreSqlBagCteTrace],
      schedule: CompiledPostgreSqlReductionSchedule,
      reductions: Vector[CompiledPostgreSqlReductionCte],
      stages: Vector[PostgreSqlStageTrace],
      terminal: CompiledPostgreSqlTerminal,
      cteMaterialization: PostgreSqlCteMaterializationDirective
  ): Vector[PostgreSqlRenderingError] =
    validateStageTrace(
      plan,
      compiledBags,
      bagTrace,
      schedule,
      reductions,
      stages,
      terminalStageProvenance(plan, terminal),
      cteMaterialization
    )

  private def validateStageTrace(
      plan: BagFactorPlan,
      compiledBags: Vector[CompiledPostgreSqlBag],
      bagTrace: Vector[PostgreSqlBagCteTrace],
      schedule: CompiledPostgreSqlReductionSchedule,
      reductions: Vector[CompiledPostgreSqlReductionCte],
      stages: Vector[PostgreSqlStageTrace],
      terminalBagIds: Vector[BagId],
      cteMaterialization: PostgreSqlCteMaterializationDirective
  ): Vector[PostgreSqlRenderingError] =
    validateStageTrace(
      plan,
      compiledBags,
      bagTrace,
      schedule,
      reductions,
      stages,
      TerminalStageProvenance(
        terminalBagIds,
        Vector.empty,
        readsFactorStages = true
      ),
      cteMaterialization
    )

  private def validateStageTrace(
      plan: BagFactorPlan,
      compiledBags: Vector[CompiledPostgreSqlBag],
      bagTrace: Vector[PostgreSqlBagCteTrace],
      schedule: CompiledPostgreSqlReductionSchedule,
      reductions: Vector[CompiledPostgreSqlReductionCte],
      stages: Vector[PostgreSqlStageTrace],
      terminalProvenance: TerminalStageProvenance,
      cteMaterialization: PostgreSqlCteMaterializationDirective
  ): Vector[PostgreSqlRenderingError] = {
    def failure(detail: String): Vector[PostgreSqlRenderingError] =
      Vector(
        PostgreSqlRenderingError.ModelInvariantViolation(
          s"PostgreSQL stage trace validation failed: $detail"
        )
      )

    if (cteMaterialization == null) {
      return failure("CTE materialization directive is missing")
    }
    if (stages == null) return failure("stage vector is missing")
    if (stages.exists(_ == null)) return failure("a stage is missing")
    if (
      stages.exists(stage =>
        stage.id == null ||
          stage.kind == null ||
          stage.physicalForm == null ||
          stage.physicalCteName == null ||
          stage.logicalBagIds == null ||
          stage.logicalEdgeIds == null ||
          stage.semijoinMembers == null ||
          stage.inputStageIds == null
      )
    ) {
      return failure("a stage field is missing")
    }
    if (
      stages.exists(stage =>
        stage.logicalBagIds.exists(_ == null) ||
          stage.logicalEdgeIds.exists(_ == null) ||
          stage.semijoinMembers.exists(_ == null) ||
          stage.inputStageIds.exists(_ == null)
      )
    ) {
      return failure("a logical provenance member is missing")
    }
    if (
      stages.flatMap(_.semijoinMembers).exists(member =>
        member.pass == null ||
          member.target == null ||
          member.source == null ||
          member.separator == null ||
          member.separator.exists(_ == null)
      )
    ) {
      return failure("a semijoin provenance member is incomplete")
    }

    val namedCteForm = stagePhysicalForm(cteMaterialization)
    val ids = stages.map(_.id)
    if (ids.distinct.size != ids.size) {
      return failure("stage identities are not unique")
    }
    val cteNames = stages.flatMap { stage =>
      stage.physicalForm match {
        case form if form == namedCteForm =>
          stage.physicalCteName match {
            case Some(name) if name.nonEmpty => Vector(name)
            case _ =>
              return failure("a named CTE has no physical name")
          }
        case PostgreSqlStagePhysicalForm.Elided | PostgreSqlStagePhysicalForm.TerminalSelect =>
          if (stage.physicalCteName.nonEmpty) {
            return failure("a non-CTE stage has a physical CTE name")
          }
          Vector.empty
        case form if form.namedCte =>
          return failure(
            "a named CTE uses a different materialization directive"
          )
        case _ =>
          return failure("a stage uses an unknown physical form")
      }
    }
    if (cteNames.distinct.size != cteNames.size) {
      return failure("physical CTE names are not unique")
    }
    val positionById = ids.zipWithIndex.toMap
    var stageIndex = 0
    while (stageIndex < stages.size) {
      val stage = stages(stageIndex)
      var inputIndex = 0
      while (inputIndex < stage.inputStageIds.size) {
        val input = stage.inputStageIds(inputIndex)
        positionById.get(input) match {
          case Some(position)
              if position < stageIndex &&
                stages(position).physicalForm.emitted => ()
          case _ =>
            return failure(
              s"stage $stageIndex input $inputIndex is not an earlier emitted stage"
            )
        }
        inputIndex += 1
      }
      stageIndex += 1
    }

    val expectedCteNames =
      bagTrace.flatMap(_.supportCte) ++
        bagTrace.map(_.initialFactorCte) ++
        reductions.map(_.outputCte)
    if (cteNames != expectedCteNames) {
      return failure(
        "emitted stage CTE names differ from the SQL emission subsequence"
      )
    }
    if (
      stages.count(
        _.id == PostgreSqlStageId.FinalReconstruction
      ) != 1 ||
      stages.count(
        _.kind == PostgreSqlStageKind.FinalReconstruction
      ) != 1 ||
      stages.lastOption.forall(
        _.id != PostgreSqlStageId.FinalReconstruction
      )
    ) {
      return failure("trace must end in exactly one terminal reconstruction")
    }

    // Independently re-derive every expected entry in one forward pass. This
    // also proves target-local factor versions and all input dependencies are
    // topological without relying on PostgreSQL plan-node order.
    if (
      compiledBags.size != bagTrace.size ||
      compiledBags.indices.exists(index =>
        compiledBags(index).factor.bagId != bagTrace(index).bagId
      ) ||
      compiledBags.indices.drop(1).exists(index =>
        BagId.ordering.gteq(
          compiledBags(index - 1).factor.bagId,
          compiledBags(index).factor.bagId
        )
      )
    ) {
      return failure("compiled bags are not in unique canonical order")
    }

    var expectedIndex = 0
    def expect(expected: PostgreSqlStageTrace): Unit = {
      if (expectedIndex >= stages.size || stages(expectedIndex) != expected) {
        throw StageTraceMismatch(expectedIndex)
      }
      expectedIndex += 1
    }

    try {
      var bagIndex = 0
      while (bagIndex < compiledBags.size) {
        val compiled = compiledBags(bagIndex)
        val factor = compiled.factor
        val trace = bagTrace(bagIndex)
        val (form, name) = compiled.supportStrategy match {
          case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
            namedCteForm -> trace.supportCte
          case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
            PostgreSqlStagePhysicalForm.Elided -> None
        }
        expect(
          PostgreSqlStageTrace(
            PostgreSqlStageId.BagSupport(factor.bagId),
            PostgreSqlStageKind.Support,
            form,
            name,
            Vector(factor.bagId),
            factor.supportCover.sorted,
            Vector.empty,
            Vector.empty
          )
        )
        bagIndex += 1
      }

      val current = mutable.Map.empty[BagId, PostgreSqlStageId.BagFactorVersion]
      val versions = mutable.Map.empty[BagId, Int]
      bagIndex = 0
      while (bagIndex < compiledBags.size) {
        val compiled = compiledBags(bagIndex)
        val factor = compiled.factor
        val trace = bagTrace(bagIndex)
        val id = PostgreSqlStageId.BagFactorVersion(factor.bagId, 0)
        val inputs = compiled.supportStrategy match {
          case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
            Vector[PostgreSqlStageId](
              PostgreSqlStageId.BagSupport(factor.bagId)
            )
          case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
            Vector.empty[PostgreSqlStageId]
        }
        expect(
          PostgreSqlStageTrace(
            id,
            PostgreSqlStageKind.InitialFactor,
            namedCteForm,
            Some(trace.initialFactorCte),
            Vector(factor.bagId),
            factor.ownedEdgeIds.sorted,
            Vector.empty,
            inputs
          )
        )
        current.put(factor.bagId, id)
        versions.put(factor.bagId, 0)
        bagIndex += 1
      }

      var reductionIndex = 0
      val flattenedSteps = Vector.newBuilder[SemijoinStep]
      while (reductionIndex < reductions.size) {
        val reduction = reductions(reductionIndex)
        val previousVersion = versions.getOrElse(
          reduction.target,
          throw StageTraceMismatch(expectedIndex)
        )
        if (previousVersion == Int.MaxValue) {
          throw StageTraceMismatch(expectedIndex)
        }
        val id = PostgreSqlStageId.BagFactorVersion(
          reduction.target,
          previousVersion + 1
        )
        val inputs = Vector.newBuilder[PostgreSqlStageId]
        inputs += current.getOrElse(
          reduction.target,
          throw StageTraceMismatch(expectedIndex)
        )
        val members = Vector.newBuilder[PostgreSqlStageSemijoinMember]
        val logicalBags = Vector.newBuilder[BagId]
        logicalBags += reduction.target
        var memberIndex = 0
        while (memberIndex < reduction.members.size) {
          val step = reduction.members(memberIndex).step
          flattenedSteps += step
          inputs += current.getOrElse(
            step.source,
            throw StageTraceMismatch(expectedIndex)
          )
          if (step.source != reduction.target) logicalBags += step.source
          members += PostgreSqlStageSemijoinMember(
            step.pass,
            step.ordinal,
            step.target,
            step.source,
            step.separator
          )
          memberIndex += 1
        }
        expect(
          PostgreSqlStageTrace(
            id,
            PostgreSqlStageKind.FusedReduction,
            namedCteForm,
            Some(reduction.outputCte),
            logicalBags.result().distinct,
            Vector.empty,
            members.result(),
            inputs.result()
          )
        )
        current.update(reduction.target, id)
        versions.update(reduction.target, previousVersion + 1)
        reductionIndex += 1
      }
      if (flattenedSteps.result() != schedule.allSteps) {
        return failure(
          "fused reduction members differ from the selected logical schedule"
        )
      }

      val terminalInputs =
        if (terminalProvenance.readsFactorStages)
          terminalProvenance.logicalBagIds.map(bagId =>
            current.getOrElse(bagId, throw StageTraceMismatch(expectedIndex))
          )
        else Vector.empty
      expect(
        PostgreSqlStageTrace(
          PostgreSqlStageId.FinalReconstruction,
          PostgreSqlStageKind.FinalReconstruction,
          PostgreSqlStagePhysicalForm.TerminalSelect,
          None,
          terminalProvenance.logicalBagIds,
          terminalProvenance.logicalEdgeIds,
          Vector.empty,
          terminalInputs
        )
      )
    } catch {
      case StageTraceMismatch(index) =>
        return failure(
          s"stage $index has a non-canonical identity, order, version, or provenance"
        )
    }
    if (expectedIndex != stages.size) {
      return failure("trace contains extra stages")
    }

    val expectedCteCount = compiledBags.count(
      _.supportStrategy ==
        PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
    ) + compiledBags.size + reductions.size
    if (cteNames.size != expectedCteCount) {
      return failure("physical CTE count differs from rendering resources")
    }
    val expectedStageCount = expectedCteCount + compiledBags.count(
      _.supportStrategy == PostgreSqlBagSupportStrategy.ElidedByOwnedCover
    ) + 1
    if (stages.size != expectedStageCount) {
      return failure("logical stage count differs from its exact resource bound")
    }
    Vector.empty
  }

  private final case class TerminalStageProvenance(
      logicalBagIds: Vector[BagId],
      logicalEdgeIds: Vector[EdgeId],
      readsFactorStages: Boolean
  )

  private def terminalStageProvenance(
      plan: BagFactorPlan,
      terminal: CompiledPostgreSqlTerminal
  ): TerminalStageProvenance = terminal match {
    case extremum: CompiledPostgreSqlTerminal.GlobalGuardedExtremum =>
      if (extremum.usesFullyReducedGuardFactor)
        TerminalStageProvenance(
          Vector(extremum.aggregateBagId),
          Vector.empty,
          readsFactorStages = true
        )
      else
        TerminalStageProvenance(
          Vector(extremum.aggregateBagId),
          extremum.semijoinPlan.sourceEdgeIds,
          readsFactorStages = false
        )
    case _ =>
      TerminalStageProvenance(
        terminal.inputBagIds(plan),
        Vector.empty,
        readsFactorStages = true
      )
  }

  private final case class StageTraceMismatch(index: Int)
      extends RuntimeException

  private def stagePhysicalForm(
      directive: PostgreSqlCteMaterializationDirective
  ): PostgreSqlStagePhysicalForm = directive match {
    case PostgreSqlCteMaterializationDirective.PlannerControlled =>
      PostgreSqlStagePhysicalForm.PlannerControlledCte
    case PostgreSqlCteMaterializationDirective.Materialized =>
      PostgreSqlStagePhysicalForm.MaterializedCte
    case PostgreSqlCteMaterializationDirective.NotMaterialized =>
      PostgreSqlStagePhysicalForm.NotMaterializedCte
  }

  /**
    * Re-derives physical runs and version dependencies without using the
    * construction loop above. Validation is linear in bags and schedule size.
    */
  private[postgresql] def validateReductionCtes(
      plan: BagFactorPlan,
      schedule: CompiledPostgreSqlReductionSchedule,
      bags: Vector[PostgreSqlBagCteTrace],
      semijoins: Vector[PostgreSqlSemijoinCteTrace],
      reductions: Vector[CompiledPostgreSqlReductionCte],
      finalizationsByBag: Map[
        BagId,
        CompiledPostgreSqlReductionFinalization
      ]
  ): Vector[PostgreSqlRenderingError] = {
    def failure(detail: String): Vector[PostgreSqlRenderingError] =
      Vector(
        PostgreSqlRenderingError.ModelInvariantViolation(
          s"Physical semijoin reduction validation failed: $detail"
        )
      )

    val expectedBags = plan.factors.sortBy(_.bagId)
    if (bags.map(_.bagId) != expectedBags.map(_.bagId)) {
      return failure("bag traces are not in canonical factor order")
    }

    val current = mutable.Map.empty[BagId, String]
    var bagIndex = 0
    while (bagIndex < expectedBags.size) {
      val factor = expectedBags(bagIndex)
      val trace = bags(bagIndex)
      val expectedInitial = PostgreSqlNames.initialFactor(factor.bagId)
      if (trace.initialFactorCte != expectedInitial) {
        return failure(
          s"bag ${factor.bagId.value} has unexpected initial CTE ${trace.initialFactorCte}"
        )
      }
      current.put(factor.bagId, expectedInitial)
      bagIndex += 1
    }

    val expectedSchedule = selectReductionSchedule(plan, schedule.mode)
    if (schedule != expectedSchedule) {
      return failure("selected schedule is not the declared canonical subset")
    }

    val allSteps = schedule.allSteps
    var envelopeIndex = 0
    while (envelopeIndex < allSteps.size) {
      validateReductionStepEnvelope(allSteps(envelopeIndex), envelopeIndex) match {
        case Some(problem) => return Vector(problem)
        case None          => ()
      }
      envelopeIndex += 1
    }
    val lastStepIndexByTarget = allSteps.zipWithIndex
      .groupBy(_._1.target)
      .iterator
      .map { case (bagId, values) => bagId -> values.map(_._2).max }
      .toMap
    val factorById = plan.factors.iterator
      .map(factor => factor.bagId -> factor)
      .toMap
    val physicalOutputs = mutable.HashSet.empty[String]
    var stepIndex = 0
    var traceIndex = 0
    var reductionIndex = 0
    while (stepIndex < allSteps.size) {
      if (reductionIndex >= reductions.size) {
        return failure(s"missing physical group for schedule step $stepIndex")
      }
      val first = allSteps(stepIndex)
      validateReductionStepEnvelope(first, stepIndex) match {
        case Some(problem) => return Vector(problem)
        case None          => ()
      }

      var runEnd = stepIndex + 1
      while (
        runEnd < allSteps.size &&
        allSteps(runEnd) != null &&
        allSteps(runEnd).pass == first.pass &&
        allSteps(runEnd).target == first.target
      ) {
        runEnd += 1
      }
      if (runEnd < allSteps.size) {
        validateReductionStepEnvelope(allSteps(runEnd), runEnd) match {
          case Some(problem) => return Vector(problem)
          case None          => ()
        }
      }

      val reduction = reductions(reductionIndex)
      val expectedSize = runEnd - stepIndex
      if (reduction.members.isEmpty) {
        return failure(s"physical group $reductionIndex is empty")
      }
      if (reduction.members.size != expectedSize) {
        return failure(
          s"physical group $reductionIndex is not the maximal adjacent run"
        )
      }
      if (reduction.pass != first.pass || reduction.target != first.target) {
        return failure(
          s"physical group $reductionIndex differs from its schedule key"
        )
      }

      val targetInputCte = current.get(first.target) match {
        case Some(value) => value
        case None =>
          return failure(
            s"physical group $reductionIndex targets an unknown bag"
          )
      }
      val outputCte = PostgreSqlNames.semijoin(allSteps(runEnd - 1))
      if (
        reduction.targetInputCte != targetInputCte ||
        reduction.outputCte != outputCte
      ) {
        return failure(
          s"physical group $reductionIndex has incorrect input or output version"
        )
      }
      val expectedFinalization =
        if (lastStepIndexByTarget.get(first.target).contains(runEnd - 1))
          finalizationsByBag.getOrElse(
            first.target,
            CompiledPostgreSqlReductionFinalization.RetainRawRows
          )
        else CompiledPostgreSqlReductionFinalization.RetainRawRows
      if (reduction.finalization != expectedFinalization) {
        return failure(
          s"physical group $reductionIndex has an incorrect aggregate reduction-finalization specification"
        )
      }
      if (reduction.finalizesCountFrequency) {
        factorById.get(first.target) match {
          case Some(factor)
              if first.target == plan.root &&
                first.pass == SemijoinPass.BottomUp &&
                factor.parent.isEmpty => ()
          case Some(factor)
              if first.target != plan.root &&
                first.pass == SemijoinPass.TopDown &&
                factor.parent.contains(first.source) &&
                expectedSize == 1 => ()
          case _ =>
            return failure(
              s"physical group $reductionIndex finalizes COUNT before global extendibility"
            )
        }
      }
      if (!physicalOutputs.add(outputCte)) {
        return failure(s"physical output CTE $outputCte is not unique")
      }

      var memberIndex = 0
      while (memberIndex < expectedSize) {
        val logicalIndex = stepIndex + memberIndex
        val step = allSteps(logicalIndex)
        validateReductionStepEnvelope(step, logicalIndex) match {
          case Some(problem) => return Vector(problem)
          case None          => ()
        }
        if (step.source == step.target) {
          return failure(s"schedule step $logicalIndex is self-sourced")
        }
        val sourceInputCte = current.get(step.source) match {
          case Some(value) => value
          case None =>
            return failure(
              s"schedule step $logicalIndex reads an unknown source bag"
            )
        }
        val expectedTrace = PostgreSqlSemijoinCteTrace(
          step.pass,
          step.ordinal,
          step.target,
          step.source,
          targetInputCte,
          sourceInputCte,
          outputCte
        )
        val member = reduction.members(memberIndex)
        if (member.step != step || member.trace != expectedTrace) {
          return failure(
            s"physical group $reductionIndex member $memberIndex differs from the logical step"
          )
        }
        if (
          traceIndex >= semijoins.size ||
          semijoins(traceIndex) != expectedTrace
        ) {
          return failure(
            s"logical semijoin trace $traceIndex is missing or inconsistent"
          )
        }
        memberIndex += 1
        traceIndex += 1
      }

      current.update(first.target, outputCte)
      stepIndex = runEnd
      reductionIndex += 1
    }

    if (reductionIndex != reductions.size) {
      return failure("physical reduction trace contains extra groups")
    }
    if (traceIndex != semijoins.size || traceIndex != allSteps.size) {
      return failure("logical semijoin traces are not one-to-one with the schedule")
    }
    val actualFinalizations = reductions.iterator.collect {
      case reduction if reduction.finalizesCountFrequency =>
        reduction.target -> reduction.finalization
    }.toMap
    if (actualFinalizations != finalizationsByBag) {
      return failure(
        "aggregate finalizations must cover exactly their declared bags once"
      )
    }

    bagIndex = 0
    while (bagIndex < bags.size) {
      val trace = bags(bagIndex)
      current.get(trace.bagId) match {
        case Some(expected) if trace.finalFactorCte == expected => ()
        case _ =>
          return failure(
            s"bag ${trace.bagId.value} has an incorrect final physical CTE"
          )
      }
      bagIndex += 1
    }
    Vector.empty
  }

  private def validateGeneratedNameCollisions(
      query: QueryIR,
      bags: Vector[PostgreSqlBagCteTrace],
      reductions: Vector[CompiledPostgreSqlReductionCte],
      terminal: CompiledPostgreSqlTerminal
  ): Vector[PostgreSqlRenderingError] = {
    val terminalCtes = terminal match {
      case extremum: CompiledPostgreSqlTerminal.GlobalGuardedExtremum
          if !extremum.usesFullyReducedGuardFactor =>
        extremum.semijoinPlan.stages.map(stage =>
          PostgreSqlNames.extremumStage(stage.ordinal)
        )
      case _ => Vector.empty
    }
    val generated = (
      bags.flatMap(bag => bag.supportCte.toVector :+ bag.initialFactorCte) ++
        reductions.map(_.outputCte) ++ terminalCtes
    ).toSet
    PostgreSqlRenderingError.canonical(
      query.relations.collect {
        case relation
            if relation.qualifiedName.size == 1 &&
              generated(relation.qualifiedName.head) =>
          PostgreSqlRenderingError.GeneratedNameCollision(
            relation.qualifiedName.head,
            relation.id
          )
      }
    )
  }

  private def calculateResourceCounts(
      plan: BagFactorPlan,
      bags: Vector[CompiledPostgreSqlBag],
      reductions: Vector[CompiledPostgreSqlReductionCte],
      localPredicates: Map[
        RelationOccurrenceId,
        Vector[CompiledPostgreSqlPredicate]
      ],
      terminal: CompiledPostgreSqlTerminal,
      terminalInnerEqualities: Vector[CompiledPostgreSqlPredicate],
      aggregateRenderingProfile: PostgreSqlAggregateRenderingProfile,
      aggregateRenderingProfileV2: PostgreSqlAggregateRenderingProfileV2,
      aggregateRenderingProfileV3: PostgreSqlAggregateRenderingProfileV3,
      exactDirectFactorMessageOrdinals: Set[Int]
  ): ResourceCounts = {
    var expandedScans = BigInt(0)
    var renderedTerms = BigInt(terminal.renderedTermCount)
    val compatibilityCtes = bags.foldLeft(BigInt(0)) { (total, bag) =>
      total + (bag.supportStrategy match {
        case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport => 2
        case PostgreSqlBagSupportStrategy.ElidedByOwnedCover           => 1
      })
    } + reductions.size
    val terminalCtes = terminal match {
      case extremum: CompiledPostgreSqlTerminal.GlobalGuardedExtremum
          if !extremum.usesFullyReducedGuardFactor =>
        BigInt(extremum.semijoinPlan.stages.size)
      case _ => BigInt(0)
    }
    val ctes = compatibilityCtes + terminalCtes

    bags.foreach { bag =>
      bag.supportStrategy match {
        case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
          expandedScans += bag.supportEdges.size
          expandedScans += bag.ownedOccurrences.size
          renderedTerms += bag.factor.vertices.size
          renderedTerms += bag.supportEqualities.size
          bag.supportEdges.foreach { edge =>
            renderedTerms += localPredicates
              .getOrElse(edge.relationOccurrenceId, Vector.empty)
              .map(p => BigInt(p.scalar.renderedTermCount)).sum
          }

          renderedTerms += bag.physicalProjection.structuralVariables.size
          renderedTerms += bag.physicalProjection.unitColumn.size
          bag.ownedOccurrences.foreach { occurrence =>
            renderedTerms += occurrence.edge.bindings.size
            renderedTerms += occurrence.carriedColumns.size
            renderedTerms += localPredicates
              .getOrElse(occurrence.relation.id, Vector.empty)
              .map(p => BigInt(p.scalar.renderedTermCount)).sum
          }

        case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
          expandedScans += bag.ownedOccurrences.size
          renderedTerms += bag.physicalProjection.structuralVariables.size
          renderedTerms += bag.physicalProjection.unitColumn.size
          renderedTerms += bag.ownedEqualities.size
          bag.ownedOccurrences.foreach { occurrence =>
            renderedTerms += occurrence.carriedColumns.size
            renderedTerms += localPredicates
              .getOrElse(occurrence.relation.id, Vector.empty)
              .map(p => BigInt(p.scalar.renderedTermCount)).sum
          }
      }
    }

    val hasAggregateFinalizations = reductions.exists(_.finalizesCountFrequency)
    reductions.foreach { reduction =>
      import CompiledPostgreSqlReductionFinalization._
      reduction.finalization match {
        case RetainRawRows => renderedTerms += 1
        case LegacyFullVerticesFrequency(groupingVariables) =>
          renderedTerms += (BigInt(2) * groupingVariables.size) + 1
        case CompactFrequency(groupingVariables, suppressEmptyGroup) =>
          renderedTerms += (BigInt(2) * groupingVariables.size) + 1
          if (suppressEmptyGroup) renderedTerms += 1
        case CompactC03Owner(groupingVariables, _, _) =>
          // Structural keys and the owner-local G are each projected and
          // grouped; frequency, local MIN, and local MAX are aggregate terms.
          renderedTerms += (BigInt(2) * groupingVariables.size) + 5
      }
      reduction.members.foreach { member =>
        renderedTerms += 1 + member.step.separator.size
        // EXISTS observes only set membership. DISTINCT separator supports
        // remove duplicate source work without carrying a frequency before
        // the source is globally extendible.
        if (
          hasAggregateFinalizations &&
          aggregateRenderingProfile.reductionSupport ==
            PostgreSqlReductionSupportProfile.DistinctSeparatorSupportV1 &&
          member.step.separator.nonEmpty
        )
          renderedTerms += member.step.separator.size
      }
    }
    val collapsedOneBag =
      plan.factors.size == 1 &&
        aggregateRenderingProfile.oneBagTerminal ==
          PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2
    val compactFinalization = reductions.exists(_.finalization match {
      case _: CompiledPostgreSqlReductionFinalization.CompactFrequency | _: CompiledPostgreSqlReductionFinalization.CompactC03Owner =>
        true
      case _ => false
    })
    val additiveRendererV2 = aggregateRenderingProfileV2.usesOptimizedAxes
    def frequencyIdentityProjectionAdjustment(
        messages: Vector[PostgreSqlFrequencyPropagation.Message]
    ): BigInt = {
      if (!additiveRendererV2) BigInt(0)
      else {
        // Frozen v1 accounting predates explicit charging of the computed
        // frequency projection on a non-regrouped message. Renderer v2 makes
        // that term explicit, then removes the complete identity projection
        // only for messages whose direct-factor proof succeeded.
        val legacyOmission =
          BigInt(messages.count(message => !message.requiresRegrouping))
        val elided = messages.iterator
          .filter(message =>
            exactDirectFactorMessageOrdinals.contains(message.ordinal)
          )
          .foldLeft(BigInt(0)) { (total, message) =>
            total + message.separator.size + 1
          }
        legacyOmission - elided
      }
    }
    def aggregateIdentityProjectionRemoval(
        messages: Vector[PostgreSqlAggregatePropagation.Message]
    ): BigInt =
      messages.iterator
        .filter(message =>
          exactDirectFactorMessageOrdinals.contains(message.ordinal)
        )
        .foldLeft(BigInt(0)) { (total, message) =>
          total + message.separator.size + message.carriedKeys.size + 1
        }
    terminal match {
      case _: CompiledPostgreSqlTerminal.Projection =>
        plan.arcs.foreach(arc => renderedTerms += arc.separator.size)
        renderedTerms += terminalInnerEqualities.size
      case count: CompiledPostgreSqlTerminal.GlobalCountStar =>
        count.strategy match {
          case CompiledPostgreSqlTerminal.GlobalCountStrategy.Reconstruction =>
            if (!collapsedOneBag) {
              plan.arcs.foreach(arc => renderedTerms += arc.separator.size)
              renderedTerms += terminalInnerEqualities.size
            }
          case CompiledPostgreSqlTerminal.GlobalCountStrategy
                .FrequencyPropagation(frequencyPlan) =>
            // The root terminal aggregate is counted by the terminal. The
            // validated frequency model accounts for every local aggregate,
            // local-key SELECT/GROUP term, propagated message aggregate, and
            // separator SELECT/GROUP/ordinary-equality term.
            if (!collapsedOneBag) {
              val resources = frequencyPlan.resources
              val compactLocalTerms =
                resources.localAggregateTerms +
                  resources.localKeyProjectionTerms +
                  resources.localKeyGroupingTerms
              renderedTerms += resources.additionalRenderedTerms -
                (if (compactFinalization) compactLocalTerms else BigInt(0)) +
                frequencyIdentityProjectionAdjustment(frequencyPlan.messages)
            }
          case CompiledPostgreSqlTerminal.GlobalCountStrategy
                .RawFrequencyPropagation(frequencyPlan) =>
            // Every non-root raw factor is aggregated exactly once after its
            // child-message joins. Each separator is projected, grouped, and
            // used in one ordinary-equality join. The root aggregate is
            // already counted by the terminal.
            val separatorTerms = frequencyPlan.messages.iterator
              .foldLeft(BigInt(0))((total, message) =>
                total + message.separator.size
              )
            renderedTerms += BigInt(frequencyPlan.messages.size) +
              (BigInt(3) * separatorTerms)
        }
      case count: CompiledPostgreSqlTerminal.GroupedCountStar =>
        if (!collapsedOneBag) {
          val resources = count.frequencyPlan.resources
          val compactLocalTerms =
            resources.localAggregateTerms +
              resources.localKeyProjectionTerms +
              resources.localKeyGroupingTerms
          renderedTerms += resources.additionalRenderedTerms -
            (if (compactFinalization) compactLocalTerms else BigInt(0)) +
            frequencyIdentityProjectionAdjustment(
              count.frequencyPlan.messages
            )
        }
      case count: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
        if (collapsedOneBag)
          renderedTerms += count.groupVariables.size
        else {
          val resources = count.propagationPlan.resources
          val compactLocalTerms =
            resources.localAggregateTerms +
              resources.localKeyProjectionTerms +
              resources.localKeyGroupingTerms
          renderedTerms += resources.additionalRenderedTerms -
            (if (compactFinalization) compactLocalTerms else BigInt(0)) -
            aggregateIdentityProjectionRemoval(
              count.propagationPlan.messages
            )
        }
      case aggregate: CompiledPostgreSqlTerminal.C03G0AggregateVector =>
        // The propagation model already charges the owner-local frequency.
        // C03-G0 adds exactly one local MIN and one local MAX state term.
        if (collapsedOneBag) renderedTerms += 1
        else {
          val resources = aggregate.propagationPlan.resources
          val compactLocalTerms =
            resources.localAggregateTerms +
              resources.localKeyProjectionTerms +
              resources.localKeyGroupingTerms
          renderedTerms += resources.additionalRenderedTerms -
            (if (compactFinalization) compactLocalTerms else BigInt(0)) +
            (if (compactFinalization) BigInt(0) else BigInt(2)) -
            aggregateIdentityProjectionRemoval(
              aggregate.propagationPlan.messages
            )
        }
      case extremum: CompiledPostgreSqlTerminal.GlobalGuardedExtremum =>
        if (!extremum.usesFullyReducedGuardFactor) {
          // Across its guard and handle inputs, the exact set-safe terminal
          // scans every source occurrence once. Its decomposition-guided
          // DISTINCT supports are terminal-owned MATERIALIZED CTEs rather than
          // public renderer stages, and compatibility factor CTEs are not
          // terminal inputs. The plan accounts for every support projection,
          // SELECT 1, and generated ordinary-equality term; local predicates
          // remain attached to their unique base scan.
          expandedScans += extremum.semijoinPlan.sourceEdgeIds.size
          renderedTerms += extremum.semijoinPlan.structuralRenderedTermCount
          renderedTerms += localPredicates.valuesIterator.flatten
            .map(p => BigInt(p.scalar.renderedTermCount)).sum
        }
    }

    ResourceCounts(expandedScans, renderedTerms, ctes)
  }

  private final case class ResourceCounts(
      expandedScans: BigInt,
      renderedTerms: BigInt,
      ctes: BigInt
  ) {
    def errors(
        limits: PostgreSqlRenderingLimits
    ): Vector[PostgreSqlRenderingError] =
      PostgreSqlRenderingError.canonical(
        Vector(
          (
            PostgreSqlRenderingResource.ExpandedScans,
            expandedScans,
            limits.maxExpandedScans
          ),
          (
            PostgreSqlRenderingResource.RenderedTerms,
            renderedTerms,
            limits.maxRenderedTerms
          ),
          (PostgreSqlRenderingResource.Ctes, ctes, limits.maxCtes)
        ).collect {
          case (resource, required, limit) if required > BigInt(limit) =>
            PostgreSqlRenderingError.ResourceLimitExceeded(
              resource,
              required,
              limit
            )
        }
      )
  }
}
