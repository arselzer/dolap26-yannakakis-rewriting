package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlan
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}

import scala.collection.mutable

/** Compiler-private implementation of the two additive renderer-v2 axes. */
private[postgresql] object PostgreSqlAggregateRendererV2Compiler {
  private type Errors = Vector[PostgreSqlRenderingError]

  def validateScope(
      reductionMode: YannakakisReductionMode,
      terminal: CompiledPostgreSqlTerminal,
      profile: PostgreSqlAggregateRenderingProfileV2,
      enumerationProfile: PostgreSqlEnumerationRenderingProfile
  ): Errors = {
    if (profile.isBaseline) Vector.empty
    else if (reductionMode != YannakakisReductionMode.FullTwoPass)
      invariant("renderer-v2 aggregate optimizations require full two-pass reduction")
    else if (
      enumerationProfile !=
        PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
    )
      invariant(
        "renderer-v2 aggregate optimizations require statistics-safe-v2 enumeration SQL"
      )
    else
      terminal match {
        case CompiledPostgreSqlTerminal.GlobalCountStar(
              _,
              _: CompiledPostgreSqlTerminal.GlobalCountStrategy.FrequencyPropagation
            ) |
            _: CompiledPostgreSqlTerminal.GroupedCountStar |
            _: CompiledPostgreSqlTerminal.DistributedGroupedCountStar |
            _: CompiledPostgreSqlTerminal.C03G0AggregateVector => Vector.empty
        case _ =>
          invariant(
            "renderer-v2 aggregate optimizations require a compact frequency-propagation terminal"
          )
      }
  }

  /** Narrows only the emitted initial-factor boundary, never its support. */
  def projectFactorBoundaries(
      plan: BagFactorPlan,
      bags: Vector[CompiledPostgreSqlBag],
      schedule: CompiledPostgreSqlReductionSchedule,
      terminal: CompiledPostgreSqlTerminal,
      finalizations: Map[BagId, CompiledPostgreSqlReductionFinalization],
      profile: PostgreSqlAggregateRenderingProfileV2
  ): Either[Errors, Vector[CompiledPostgreSqlBag]] =
    profile.factorBoundaryProjection match {
      case PostgreSqlFactorBoundaryProjectionProfile.FullFactorVerticesV1 =>
        Right(bags)
      case PostgreSqlFactorBoundaryProjectionProfile
            .DemandedStructuralColumnsV2 =>
        val demand = mutable.Map.from(bags.map(bag =>
          bag.factor.bagId -> mutable.Set.empty[VariableId]
        ))
        val errors = Vector.newBuilder[PostgreSqlRenderingError]
        def retain(bagId: BagId, variables: Iterable[VariableId]): Unit =
          demand.get(bagId) match {
            case Some(values) => values ++= variables
            case None =>
              errors ++= invariant(
                s"factor-boundary demand names unknown bag ${bagId.value}"
              )
          }

        schedule.allSteps.foreach { step =>
          retain(step.source, step.separator)
          retain(step.target, step.separator)
        }
        if (terminal.reconstructsFactors)
          plan.arcs.foreach { arc =>
            retain(arc.parent, arc.separator)
            retain(arc.child, arc.separator)
          }
        terminalDemand(terminal).foreach { case (bagId, variables) =>
          retain(bagId, variables)
        }
        finalizations.foreach { case (bagId, finalization) =>
          retain(bagId, groupingVariables(finalization))
        }

        val problems = PostgreSqlRenderingError.canonical(errors.result())
        if (problems.nonEmpty) Left(problems)
        else
          Right(bags.map { bag =>
            val variables = bag.factor.vertices.filter(
              demand(bag.factor.bagId).contains
            )
            val occurrenceColumns = bag.physicalProjection.occurrenceColumns
            val unitColumn = Option.when(
              variables.isEmpty && occurrenceColumns.isEmpty
            )(PostgreSqlNames.factorUnit(bag.factor.bagId))
            bag.copy(
              physicalProjection = PostgreSqlPhysicalFactorProjectionV2(
                variables,
                occurrenceColumns,
                unitColumn
              )
            )
          })
    }

  /** Validates every physical consumer, then selects only exact leaf identities. */
  def directLeafMessageOrdinals(
      bags: Vector[CompiledPostgreSqlBag],
      reductions: Vector[CompiledPostgreSqlReductionCte],
      terminal: CompiledPostgreSqlTerminal,
      profile: PostgreSqlAggregateRenderingProfileV2
  ): Either[Errors, Set[Int]] =
    if (profile.isBaseline) Right(Set.empty)
    else
      schemasAfterReductions(bags, reductions, terminal).map { schemas =>
        profile.leafMessage match {
          case PostgreSqlAggregateLeafMessageProfile.DerivedSelectV1 =>
            Set.empty
          case PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2 =>
            def exact(source: BagId, variables: Vector[VariableId]): Boolean =
              schemas.get(source).contains(
                PhysicalSchema(
                  variables.toSet,
                  Set.empty,
                  None,
                  Set(PostgreSqlNames.CountFrequencyColumn)
                )
              )
            terminal match {
              case CompiledPostgreSqlTerminal.GlobalCountStar(
                    _,
                    CompiledPostgreSqlTerminal.GlobalCountStrategy
                      .FrequencyPropagation(plan)
                  ) => directFrequencyMessages(plan.messages, exact)
              case count: CompiledPostgreSqlTerminal.GroupedCountStar =>
                directFrequencyMessages(count.frequencyPlan.messages, exact)
              case count: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
                directAggregateMessages(count.propagationPlan, exact)
              case aggregate: CompiledPostgreSqlTerminal.C03G0AggregateVector =>
                directAggregateMessages(aggregate.propagationPlan, exact)
              case _ => Set.empty
            }
        }
      }

  private final case class PhysicalSchema(
      structuralVariables: Set[VariableId],
      occurrenceColumnNames: Set[String],
      unitColumn: Option[String],
      aggregateStateColumns: Set[String]
  )

  /** Forward schema transfer for every fused physical reduction boundary. */
  private def schemasAfterReductions(
      bags: Vector[CompiledPostgreSqlBag],
      reductions: Vector[CompiledPostgreSqlReductionCte],
      terminal: CompiledPostgreSqlTerminal
  ): Either[Errors, Map[BagId, PhysicalSchema]] = {
    val current = mutable.Map.from(bags.map { bag =>
      val projection = bag.physicalProjection
      bag.factor.bagId -> PhysicalSchema(
        projection.structuralVariables.toSet,
        projection.occurrenceColumns.iterator.map(_.columnName).toSet,
        projection.unitColumn,
        Set.empty
      )
    })
    val payloadNameByColumn = bags.iterator
      .flatMap(_.physicalProjection.occurrenceColumns)
      .map(column => column.columnId -> column.columnName)
      .toMap
    val errors = Vector.newBuilder[PostgreSqlRenderingError]
    def reject(detail: String): Unit = errors ++= invariant(
      s"renderer-v2 physical factor schema invalid: $detail"
    )
    def containsVariables(bagId: BagId, variables: Iterable[VariableId]) =
      current.get(bagId).exists(schema =>
        variables.forall(schema.structuralVariables.contains)
      )
    def frequencySchema(
        bagId: BagId,
        grouping: Vector[VariableId],
        input: PhysicalSchema
    ): PhysicalSchema = {
      if (!grouping.forall(input.structuralVariables.contains))
        reject(s"finalization at bag ${bagId.value} requests an absent structural variable")
      PhysicalSchema(
        grouping.toSet,
        Set.empty,
        None,
        Set(PostgreSqlNames.CountFrequencyColumn)
      )
    }

    reductions.foreach { reduction =>
      val targetInput = current.get(reduction.target)
      reduction.members.foreach { member =>
        val step = member.step
        if (!containsVariables(step.source, step.separator))
          reject(
            s"source bag ${step.source.value} lacks separator variables for " +
              s"${step.pass.code}:${step.ordinal}"
          )
        if (!containsVariables(step.target, step.separator))
          reject(
            s"target bag ${step.target.value} lacks separator variables for " +
              s"${step.pass.code}:${step.ordinal}"
          )
      }
      targetInput match {
        case None => reject(s"reduction targets unknown bag ${reduction.target.value}")
        case Some(input) =>
          val output = reduction.finalization match {
            case CompiledPostgreSqlReductionFinalization.RetainRawRows => input
            case CompiledPostgreSqlReductionFinalization
                  .LegacyFullVerticesFrequency(grouping) =>
              frequencySchema(reduction.target, grouping, input)
            case CompiledPostgreSqlReductionFinalization
                  .CompactFrequency(grouping, _) =>
              frequencySchema(reduction.target, grouping, input)
            case finalization: CompiledPostgreSqlReductionFinalization.CompactC03Owner =>
              val grouping = finalization.groupingVariables
              val groupName = payloadNameByColumn.get(finalization.groupColumn)
              val measureName = payloadNameByColumn.get(finalization.measureColumn)
              if (!grouping.forall(input.structuralVariables.contains))
                reject(
                  s"C03 finalization at bag ${reduction.target.value} " +
                    "requests an absent structural variable"
                )
              if (
                !groupName.exists(input.occurrenceColumnNames.contains) ||
                !measureName.exists(input.occurrenceColumnNames.contains)
              ) reject(
                s"C03 finalization at bag ${reduction.target.value} " +
                  "requests an absent owner payload"
              )
              PhysicalSchema(
                grouping.toSet,
                groupName.toSet,
                None,
                Set(
                  PostgreSqlNames.CountFrequencyColumn,
                  PostgreSqlNames.C03LocalMinimumColumn,
                  PostgreSqlNames.C03LocalMaximumColumn
                )
              )
          }
          current.update(reduction.target, output)
      }
    }
    terminalDemand(terminal).foreach { case (bagId, variables) =>
      if (!containsVariables(bagId, variables))
        reject(
          s"terminal annotation requests an absent structural variable from bag ${bagId.value}"
        )
    }
    val problems = PostgreSqlRenderingError.canonical(errors.result())
    if (problems.nonEmpty) Left(problems) else Right(current.toMap)
  }

  private def directFrequencyMessages(
      messages: Vector[PostgreSqlFrequencyPropagation.Message],
      exact: (BagId, Vector[VariableId]) => Boolean
  ): Set[Int] =
    messages.iterator
      .filter(message =>
        message.inputMessageOrdinals.isEmpty &&
          !message.requiresRegrouping &&
          exact(message.source, message.separator)
      )
      .map(_.ordinal)
      .toSet

  private def directAggregateMessages(
      plan: PostgreSqlAggregatePropagation.Plan,
      exact: (BagId, Vector[VariableId]) => Boolean
  ): Set[Int] =
    plan.messages.iterator
      .filter { message =>
        val structuralKeys = message.carriedKeys.collect {
          case PostgreSqlAggregatePropagation.AggregateKey.Structural(variable) =>
            variable
        }
        val bag = plan.bagById(message.source)
        message.inputMessageOrdinals.isEmpty &&
        !message.requiresRegrouping &&
        structuralKeys.size == message.carriedKeys.size &&
        bag.ownedValueKeyColumns.isEmpty &&
        bag.ownedLanes.isEmpty &&
        exact(message.source, message.separator ++ structuralKeys)
      }
      .map(_.ordinal)
      .toSet

  private def terminalDemand(
      terminal: CompiledPostgreSqlTerminal
  ): Vector[(BagId, Vector[VariableId])] = terminal match {
    case CompiledPostgreSqlTerminal.GlobalCountStar(
          _,
          CompiledPostgreSqlTerminal.GlobalCountStrategy
            .FrequencyPropagation(plan)
        ) => plan.bags.map(bag => bag.bagId -> bag.localGrouping)
    case count: CompiledPostgreSqlTerminal.GroupedCountStar =>
      count.frequencyPlan.bags.map(bag => bag.bagId -> bag.localGrouping)
    case count: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
      count.propagationPlan.bags.map(bag =>
        bag.bagId -> bag.localGroupingVariables
      )
    case aggregate: CompiledPostgreSqlTerminal.C03G0AggregateVector =>
      aggregate.propagationPlan.bags.map(bag =>
        bag.bagId -> bag.localGroupingVariables
      )
    case _ => Vector.empty
  }

  private def groupingVariables(
      finalization: CompiledPostgreSqlReductionFinalization
  ): Vector[VariableId] = finalization match {
    case CompiledPostgreSqlReductionFinalization
          .LegacyFullVerticesFrequency(grouping) => grouping
    case CompiledPostgreSqlReductionFinalization.CompactFrequency(grouping, _) =>
      grouping
    case CompiledPostgreSqlReductionFinalization
          .CompactC03Owner(grouping, _, _) => grouping
    case CompiledPostgreSqlReductionFinalization.RetainRawRows => Vector.empty
  }

  private def invariant(detail: String): Errors =
    Vector(PostgreSqlRenderingError.ModelInvariantViolation(detail))
}
