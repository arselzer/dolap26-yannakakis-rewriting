package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.core.{BagId, ColumnId, EdgeId, Hyperedge, VariableId}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  ColumnRef,
  QueryResultShape,
  RelationOccurrence
}

private[postgresql] object PostgreSqlEmitter {
  /** One immutable compiler-derived physical plan is shared by count and emit. */
  private[postgresql] def physicalCharacterCount(compiled: CompiledPostgreSqlRendering,
      physical: PostgreSqlPhysicalEmissionPlan): BigInt = {
    val sink = new CountingSink(physical)
    emit(compiled, sink)
    sink.length
  }

  private[postgresql] def renderPhysical(compiled: CompiledPostgreSqlRendering,
      exactCharacterCount: Int, physical: PostgreSqlPhysicalEmissionPlan): String = {
    val sink = new MaterializingSink(exactCharacterCount, physical)
    emit(compiled, sink)
    sink.result()
  }
  /** Validated raw factor definition for the opt-in branch-count renderer. */
  private[postgresql] def initialFactorDefinition(
      compiled: CompiledPostgreSqlRendering,
      bag: CompiledPostgreSqlBag,
      maximumCharacters: Int
  ): Option[String] = {
    val counter = new CountingSink
    emitInitialFactor(compiled, bag, counter)
    if (counter.length > maximumCharacters) None
    else {
      val sink = new MaterializingSink(counter.length.toInt)
      emitInitialFactor(compiled, bag, sink)
      Some(sink.result())
    }
  }

  /** Count one certified occurrence partition without fabricating a BagFactor.
    * The core-count renderer proves the partition/boundary conditions. Source
    * names and local predicates use the same typed emission as initial factors;
    * no rendered SQL is inspected or rewritten. Count before allocating SQL.
    */
  private[postgresql] def occurrenceCountDefinition(
      compiled: CompiledPostgreSqlRendering, name: String, edges: Vector[Hyperedge],
      projected: Vector[PostgreSqlBindingLocation],
      restriction: Option[(String, PostgreSqlBindingLocation)], maximumCharacters: Int
  ): Option[String] = {
    def emitCount(sink: SqlSink): Unit = {
      def alias(id: EdgeId) = s"yr_cc_e${id.value}"
      def column(binding: PostgreSqlBindingLocation): Unit =
        emitSourceColumn(compiled, alias(binding.edgeId), binding.columnId, sink)
      quoted(name, sink)
      sink.append(" AS MATERIALIZED (\n    SELECT ")
      projected.foreach { binding =>
        column(binding); sink.append(" AS ")
        quoted(PostgreSqlNames.variable(binding.variableId), sink); sink.append(", ")
      }
      sink.append("SUM(CAST(1 AS NUMERIC)) AS yr_freq\n    FROM ")
      edges.zipWithIndex.foreach { case (edge, index) =>
        if (index > 0) sink.append("\n      CROSS JOIN ")
        emitRelation(compiled.relationByOccurrence(edge.relationOccurrenceId), sink)
        sink.append(" AS "); quoted(alias(edge.id), sink)
      }
      val bindings = edges.flatMap(e => e.bindings.map { case (col, v) => PostgreSqlBindingLocation(e.id, col, v) })
        .groupBy(_.variableId).toVector.sortBy(_._1)
      var emitted = false
      def conjunction(): Unit = {
        sink.append(if (emitted) "\n      AND " else "\n    WHERE ")
        emitted = true
      }
      bindings.foreach { case (_, locations) =>
        // Include the representative's self-comparison: a structural binding
        // must reject NULL even if its other occurrences are in another part.
        locations.foreach { binding =>
          conjunction(); column(locations.head); sink.append(" = "); column(binding)
        }
      }
      edges.foreach { edge =>
        compiled.localPredicatesByOccurrence.getOrElse(edge.relationOccurrenceId, Vector.empty).foreach { predicate =>
          conjunction()
          emitScalar(compiled, predicate.scalar,
            ref => emitSourceColumn(compiled, alias(edge.id), ref.id, sink), sink)
        }
      }
      restriction.foreach { case (core, binding) =>
        conjunction(); sink.append("EXISTS (SELECT 1 FROM "); quoted(core, sink)
        sink.append(" AS yr_cc_domain WHERE ")
        column(binding); sink.append(" = ")
        qualifiedColumn("yr_cc_domain", PostgreSqlNames.variable(binding.variableId), sink)
        sink.append(")")
      }
      sink.append("\n    GROUP BY ")
      projected.zipWithIndex.foreach { case (binding, index) =>
        if (index > 0) sink.append(", ")
        column(binding)
      }
      sink.append("\n  )")
    }
    val counter = new CountingSink
    emitCount(counter)
    if (counter.length > maximumCharacters) None
    else {
      val sink = new MaterializingSink(counter.length.toInt)
      emitCount(sink)
      Some(sink.result())
    }
  }

  def characterCount(compiled: CompiledPostgreSqlRendering): BigInt = {
    val sink = new CountingSink
    emit(compiled, sink)
    sink.length
  }

  def render(
      compiled: CompiledPostgreSqlRendering,
      exactCharacterCount: Int
  ): String = {
    val sink = new MaterializingSink(exactCharacterCount)
    emit(compiled, sink)
    sink.result()
  }

  private[postgresql] def stageDiagnosticCharacterCount(
      compiled: CompiledPostgreSqlRendering,
      ctePrefixCount: Int,
      targetCteName: String
  ): BigInt = {
    val sink = new CountingSink
    emitStageDiagnostic(compiled, ctePrefixCount, targetCteName, sink)
    sink.length
  }

  /**
    * Counts every canonical prefix statement in one pass over CTE bodies.
    * The supplied names must use physical CTE emission order.
    */
  private[postgresql] def stageDiagnosticCharacterCounts(
      compiled: CompiledPostgreSqlRendering,
      targetCteNames: Vector[String]
  ): Vector[BigInt] = {
    val definitionCounts = Vector.newBuilder[BigInt]
    compiled.bags.foreach { bag =>
      if (
        bag.supportStrategy ==
          PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
      ) {
        val sink = new CountingSink
        emitSupport(compiled, bag, sink)
        definitionCounts += sink.length
      }
    }
    compiled.bags.foreach { bag =>
      val sink = new CountingSink
      emitInitialFactor(compiled, bag, sink)
      definitionCounts += sink.length
    }
    compiled.reductionCtes.foreach { reduction =>
      val sink = new CountingSink
      emitReductionCte(compiled, reduction, sink)
      definitionCounts += sink.length
    }

    val definitions = definitionCounts.result()
    if (definitions.size != targetCteNames.size) return Vector.empty
    val counts = Vector.newBuilder[BigInt]
    var prefixCharacters = BigInt("WITH\n".length)
    var index = 0
    while (index < definitions.size) {
      if (index > 0) prefixCharacters += ",\n".length
      prefixCharacters += definitions(index)
      val targetSink = new CountingSink
      quoted(targetCteNames(index), targetSink)
      counts += prefixCharacters +
        BigInt("\nSELECT * FROM ".length) +
        targetSink.length +
        BigInt(1) // semicolon
      index += 1
    }
    counts.result()
  }

  private[postgresql] def renderStageDiagnostic(
      compiled: CompiledPostgreSqlRendering,
      ctePrefixCount: Int,
      targetCteName: String,
      exactCharacterCount: Int
  ): String = {
    val sink = new MaterializingSink(exactCharacterCount)
    emitStageDiagnostic(compiled, ctePrefixCount, targetCteName, sink)
    sink.result()
  }

  private def emit(
      compiled: CompiledPostgreSqlRendering,
      sink: SqlSink
  ): Unit = {
    sink.append("WITH\n")
    var firstCte = true

    compiled.bags.foreach { bag =>
      if (
        bag.supportStrategy ==
          PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
      ) {
        if (!firstCte) sink.append(",\n")
        emitSupport(compiled, bag, sink)
        firstCte = false
      }
    }
    compiled.bags.foreach { bag =>
      if (!firstCte) sink.append(",\n")
      emitInitialFactor(compiled, bag, sink)
      firstCte = false
    }

    compiled.reductionCtes.foreach { reduction =>
      if (!firstCte) sink.append(",\n")
      emitReductionCte(compiled, reduction, sink)
      firstCte = false
    }

    compiled.terminal match {
      case extremum: CompiledPostgreSqlTerminal.GlobalGuardedExtremum
          if !extremum.usesFullyReducedGuardFactor =>
        extremum.semijoinPlan.stages.foreach { stage =>
          if (!firstCte) sink.append(",\n")
          emitExtremumStageCte(
            compiled,
            extremum.semijoinPlan,
            stage,
            sink
          )
          firstCte = false
        }
      case _ => ()
    }

    sink.append("\n")
    emitFinalSelect(compiled, sink)
    sink.append(";")
  }

  /** Emits a canonical physical CTE prefix followed by a probe projection. */
  private def emitStageDiagnostic(
      compiled: CompiledPostgreSqlRendering,
      ctePrefixCount: Int,
      targetCteName: String,
      sink: SqlSink
  ): Unit = {
    sink.append("WITH\n")
    var emitted = 0
    var firstCte = true

    compiled.bags.foreach { bag =>
      if (
        emitted < ctePrefixCount &&
        bag.supportStrategy ==
          PostgreSqlBagSupportStrategy.MaterializedDistinctSupport
      ) {
        if (!firstCte) sink.append(",\n")
        emitSupport(compiled, bag, sink)
        firstCte = false
        emitted += 1
      }
    }
    compiled.bags.foreach { bag =>
      if (emitted < ctePrefixCount) {
        if (!firstCte) sink.append(",\n")
        emitInitialFactor(compiled, bag, sink)
        firstCte = false
        emitted += 1
      }
    }
    compiled.reductionCtes.foreach { reduction =>
      if (emitted < ctePrefixCount) {
        if (!firstCte) sink.append(",\n")
        emitReductionCte(compiled, reduction, sink)
        firstCte = false
        emitted += 1
      }
    }

    sink.append("\nSELECT * FROM ")
    quoted(targetCteName, sink)
    sink.append(";")
  }

  private def emitSupport(
      compiled: CompiledPostgreSqlRendering,
      bag: CompiledPostgreSqlBag,
      sink: SqlSink
  ): Unit = {
    val bagId = bag.factor.bagId
    val partial = sink.physical.partialSupports.get(bagId)
    val supportVariables = partial.fold(bag.factor.vertices)(_.variables)
    val supportEdges = partial.fold(bag.supportEdges)(_.edges)
    val supportRepresentatives = partial.fold(bag.supportRepresentatives)(_.representatives)
    val supportEqualities = partial.fold(bag.supportEqualities)(_.equalities)
    sink.append("  ")
    quoted(PostgreSqlNames.support(bagId), sink)
    emitCteBodyStart(compiled, sink)
    sink.append("    SELECT DISTINCT\n")
    supportVariables.zipWithIndex.foreach { case (variableId, index) =>
      sink.append("      ")
      val representative = supportRepresentatives(variableId)
      emitSourceColumn(
        compiled,
        PostgreSqlNames.supportScan(bagId, representative.edgeId),
        representative.columnId,
        sink
      )
      sink.append(" AS ")
      quoted(PostgreSqlNames.variable(variableId), sink)
      if (index + 1 < supportVariables.size) sink.append(",")
      sink.append("\n")
    }

    sink.append("    FROM ")
    supportEdges.zipWithIndex.foreach { case (edge, index) =>
      if (index > 0) sink.append("\n      CROSS JOIN ")
      emitRelation(compiled.relationByOccurrence(edge.relationOccurrenceId), sink)
      sink.append(" AS ")
      quoted(PostgreSqlNames.supportScan(bagId, edge.id), sink)
    }

    val localCount = supportEdges.iterator.map { edge =>
      compiled.localPredicatesByOccurrence
        .getOrElse(edge.relationOccurrenceId, Vector.empty)
        .size
    }.sum
    if (supportEqualities.nonEmpty || localCount > 0) {
      sink.append("\n    WHERE\n")
      var emitted = 0
      supportEqualities.foreach { case (left, right) =>
        if (emitted > 0) sink.append("\n      AND ") else sink.append("      ")
        emitSourceColumn(
          compiled,
          PostgreSqlNames.supportScan(bagId, left.edgeId),
          left.columnId,
          sink
        )
        sink.append(" = ")
        emitSourceColumn(
          compiled,
          PostgreSqlNames.supportScan(bagId, right.edgeId),
          right.columnId,
          sink
        )
        emitted += 1
      }
      supportEdges.foreach { edge =>
        val alias = PostgreSqlNames.supportScan(bagId, edge.id)
        compiled.localPredicatesByOccurrence
          .getOrElse(edge.relationOccurrenceId, Vector.empty)
          .foreach { predicate =>
            if (emitted > 0) sink.append("\n      AND ")
            else sink.append("      ")
            emitScalar(
              compiled,
              predicate.scalar,
              ref => emitSourceColumn(compiled, alias, ref.id, sink),
              sink
            )
            emitted += 1
          }
      }
    }
    sink.append("\n  )")
  }

  private def emitInitialFactor(
      compiled: CompiledPostgreSqlRendering,
      bag: CompiledPostgreSqlBag,
      sink: SqlSink
  ): Unit = bag.supportStrategy match {
    case PostgreSqlBagSupportStrategy.MaterializedDistinctSupport =>
      emitInitialFactorWithSupport(compiled, bag, sink)
    case PostgreSqlBagSupportStrategy.ElidedByOwnedCover =>
      emitInitialFactorFromOwnedEdges(compiled, bag, sink)
  }

  private def emitInitialFactorWithSupport(
      compiled: CompiledPostgreSqlRendering,
      bag: CompiledPostgreSqlBag,
      sink: SqlSink
  ): Unit = {
    val bagId = bag.factor.bagId
    val supportAlias = PostgreSqlNames.factorSupport(bagId)
    val partial = sink.physical.partialSupports.get(bagId)
    def emitBinding(variableId: VariableId): Unit = partial.flatMap(_.ownedRepresentatives.get(variableId)) match {
      case Some(location) => emitSourceColumn(compiled,
        PostgreSqlNames.factorScan(bagId, location.edgeId), location.columnId, sink)
      case None => qualifiedColumn(supportAlias, PostgreSqlNames.variable(variableId), sink)
    }
    sink.append("  ")
    quoted(PostgreSqlNames.initialFactor(bagId), sink)
    emitCteBodyStart(compiled, sink, Some(bagId))
    sink.append("    SELECT\n")

    val projectedVariables = bag.physicalProjection.structuralVariables
    val selectedCount = projectedVariables.size +
      bag.ownedOccurrences.iterator.map(_.carriedColumns.size).sum +
      bag.physicalProjection.unitColumn.size
    var selected = 0
    projectedVariables.foreach { variableId =>
      sink.append("      ")
      emitBinding(variableId)
      sink.append(" AS ")
      quoted(PostgreSqlNames.variable(variableId), sink)
      selected += 1
      if (selected < selectedCount) sink.append(",")
      sink.append("\n")
    }
    bag.ownedOccurrences.foreach { occurrence =>
      val alias = PostgreSqlNames.factorScan(bagId, occurrence.edge.id)
      occurrence.carriedColumns.foreach { metadata =>
        sink.append("      ")
        emitSourceColumn(compiled, alias, metadata.ref.id, sink)
        sink.append(" AS ")
        quoted(
          PostgreSqlNames.occurrenceColumn(occurrence.edge.id, metadata.ref.id),
          sink
        )
        selected += 1
        if (selected < selectedCount) sink.append(",")
        sink.append("\n")
      }
    }
    bag.physicalProjection.unitColumn.foreach { columnName =>
      sink.append("      CAST(1 AS SMALLINT) AS ")
      quoted(columnName, sink)
      selected += 1
      if (selected < selectedCount) sink.append(",")
      sink.append("\n")
    }

    sink.append("    FROM ")
    quoted(PostgreSqlNames.support(bagId), sink)
    sink.append(" AS ")
    quoted(supportAlias, sink)
    bag.ownedOccurrences.foreach { occurrence =>
      sink.append("\n      CROSS JOIN ")
      emitRelation(occurrence.relation, sink)
      sink.append(" AS ")
      quoted(PostgreSqlNames.factorScan(bagId, occurrence.edge.id), sink)
    }

    val bindingCount = bag.ownedOccurrences.iterator.map(_.edge.bindings.size).sum
    val localCount = bag.ownedOccurrences.iterator.map { occurrence =>
      compiled.localPredicatesByOccurrence
        .getOrElse(occurrence.relation.id, Vector.empty)
        .size
    }.sum
    if (bindingCount > 0 || localCount > 0) {
      sink.append("\n    WHERE\n")
      var emitted = 0
      bag.ownedOccurrences.foreach { occurrence =>
        val edge = occurrence.edge
        val alias = PostgreSqlNames.factorScan(bagId, edge.id)
        edge.bindings.foreach { case (columnId, variableId) =>
          if (emitted > 0) sink.append("\n      AND ") else sink.append("      ")
          emitSourceColumn(compiled, alias, columnId, sink)
          sink.append(" = ")
          emitBinding(variableId)
          emitted += 1
        }
        compiled.localPredicatesByOccurrence
          .getOrElse(occurrence.relation.id, Vector.empty)
          .foreach { predicate =>
            if (emitted > 0) sink.append("\n      AND ")
            else sink.append("      ")
            emitScalar(
              compiled,
              predicate.scalar,
              ref => emitSourceColumn(compiled, alias, ref.id, sink),
              sink
            )
            emitted += 1
          }
      }
    }
    sink.append("\n  )")
  }

  private def emitInitialFactorFromOwnedEdges(
      compiled: CompiledPostgreSqlRendering,
      bag: CompiledPostgreSqlBag,
      sink: SqlSink
  ): Unit = {
    val bagId = bag.factor.bagId
    sink.append("  ")
    quoted(PostgreSqlNames.initialFactor(bagId), sink)
    emitCteBodyStart(compiled, sink, Some(bagId))
    sink.append("    SELECT\n")

    val projectedVariables = bag.physicalProjection.structuralVariables
    val selectedCount = projectedVariables.size +
      bag.ownedOccurrences.iterator.map(_.carriedColumns.size).sum +
      bag.physicalProjection.unitColumn.size
    var selected = 0
    projectedVariables.foreach { variableId =>
      val representative = bag.ownedRepresentatives(variableId)
      sink.append("      ")
      emitSourceColumn(
        compiled,
        PostgreSqlNames.factorScan(bagId, representative.edgeId),
        representative.columnId,
        sink
      )
      sink.append(" AS ")
      quoted(PostgreSqlNames.variable(variableId), sink)
      selected += 1
      if (selected < selectedCount) sink.append(",")
      sink.append("\n")
    }
    bag.ownedOccurrences.foreach { occurrence =>
      val alias = PostgreSqlNames.factorScan(bagId, occurrence.edge.id)
      occurrence.carriedColumns.foreach { metadata =>
        sink.append("      ")
        emitSourceColumn(compiled, alias, metadata.ref.id, sink)
        sink.append(" AS ")
        quoted(
          PostgreSqlNames.occurrenceColumn(occurrence.edge.id, metadata.ref.id),
          sink
        )
        selected += 1
        if (selected < selectedCount) sink.append(",")
        sink.append("\n")
      }
    }
    bag.physicalProjection.unitColumn.foreach { columnName =>
      sink.append("      CAST(1 AS SMALLINT) AS ")
      quoted(columnName, sink)
      selected += 1
      if (selected < selectedCount) sink.append(",")
      sink.append("\n")
    }

    sink.append("    FROM ")
    bag.ownedOccurrences.zipWithIndex.foreach { case (occurrence, index) =>
      if (index > 0) sink.append("\n      CROSS JOIN ")
      emitRelation(occurrence.relation, sink)
      sink.append(" AS ")
      quoted(PostgreSqlNames.factorScan(bagId, occurrence.edge.id), sink)
    }

    val localCount = bag.ownedOccurrences.iterator.map { occurrence =>
      compiled.localPredicatesByOccurrence
        .getOrElse(occurrence.relation.id, Vector.empty)
        .size
    }.sum
    if (bag.ownedEqualities.nonEmpty || localCount > 0) {
      sink.append("\n    WHERE\n")
      var emitted = 0
      bag.ownedEqualities.foreach { case (representative, binding) =>
        if (emitted > 0) sink.append("\n      AND ") else sink.append("      ")
        emitSourceColumn(
          compiled,
          PostgreSqlNames.factorScan(bagId, representative.edgeId),
          representative.columnId,
          sink
        )
        sink.append(" = ")
        emitSourceColumn(
          compiled,
          PostgreSqlNames.factorScan(bagId, binding.edgeId),
          binding.columnId,
          sink
        )
        emitted += 1
      }
      bag.ownedOccurrences.foreach { occurrence =>
        val alias = PostgreSqlNames.factorScan(bagId, occurrence.edge.id)
        compiled.localPredicatesByOccurrence
          .getOrElse(occurrence.relation.id, Vector.empty)
          .foreach { predicate =>
            if (emitted > 0) sink.append("\n      AND ")
            else sink.append("      ")
            emitScalar(
              compiled,
              predicate.scalar,
              ref => emitSourceColumn(compiled, alias, ref.id, sink),
              sink
            )
            emitted += 1
          }
      }
    }
    sink.append("\n  )")
  }

  private def emitReductionCte(
      compiled: CompiledPostgreSqlRendering,
      reduction: CompiledPostgreSqlReductionCte,
      sink: SqlSink
  ): Unit = {
    val targetAlias =
      PostgreSqlNames.semijoinTarget(reduction.members.last.step)
    val stagedCountFrequency = usesStagedCountFrequency(compiled)
    val distinctSeparatorSupport =
      stagedCountFrequency &&
        compiled.aggregateRenderingProfile.reductionSupport ==
          PostgreSqlReductionSupportProfile.DistinctSeparatorSupportV1
    sink.append("  ")
    quoted(reduction.outputCte, sink)
    emitCteBodyStart(compiled, sink)
    sink.append("    SELECT")
    reduction.finalization match {
      case CompiledPostgreSqlReductionFinalization.RetainRawRows =>
        sink.append(" ")
        quoted(targetAlias, sink)
        sink.append(".*")

      case finalization =>
        sink.append("\n")
        emitReductionFinalizationProjection(
          compiled,
          targetAlias,
          finalization,
          sink
        )
    }
    sink.append("\n    FROM ")
    quoted(reduction.targetInputCte, sink)
    sink.append(" AS ")
    quoted(targetAlias, sink)
    reduction.members.zipWithIndex.foreach { case (member, memberIndex) =>
      val step = member.step
      val sourceAlias = PostgreSqlNames.semijoinSource(step)
      if (memberIndex == 0) sink.append("\n    WHERE EXISTS (")
      else sink.append("\n      AND EXISTS (")
      sink.append("\n      SELECT 1\n      FROM ")
      if (distinctSeparatorSupport && step.separator.nonEmpty) {
        val distinctInputAlias = PostgreSqlNames.countReductionSource(step)
        sink.append("(\n        SELECT DISTINCT\n")
        step.separator.zipWithIndex.foreach { case (variableId, index) =>
          sink.append("          ")
          qualifiedColumn(
            distinctInputAlias,
            PostgreSqlNames.variable(variableId),
            sink
          )
          sink.append(" AS ")
          quoted(PostgreSqlNames.variable(variableId), sink)
          if (index + 1 < step.separator.size) sink.append(",")
          sink.append("\n")
        }
        sink.append("        FROM ")
        quoted(member.trace.sourceInputCte, sink)
        sink.append(" AS ")
        quoted(distinctInputAlias, sink)
        sink.append("\n      ) AS ")
      } else {
        quoted(member.trace.sourceInputCte, sink)
        sink.append(" AS ")
      }
      quoted(sourceAlias, sink)
      if (step.separator.nonEmpty) {
        sink.append("\n      WHERE\n")
        step.separator.zipWithIndex.foreach { case (variableId, index) =>
          if (index > 0) sink.append("\n        AND ")
          else sink.append("        ")
          qualifiedColumn(
            targetAlias,
            PostgreSqlNames.variable(variableId),
            sink
          )
          sink.append(" = ")
          qualifiedColumn(
            sourceAlias,
            PostgreSqlNames.variable(variableId),
            sink
          )
        }
      }
      sink.append("\n    )")
    }
    emitReductionFinalizationGrouping(
      compiled,
      targetAlias,
      reduction.finalization,
      sink
    )
    sink.append("\n  )")
  }

  private def emitReductionFinalizationProjection(
      compiled: CompiledPostgreSqlRendering,
      targetAlias: String,
      finalization: CompiledPostgreSqlReductionFinalization,
      sink: SqlSink
  ): Unit = {
    import CompiledPostgreSqlReductionFinalization._

    val groupingVariables = finalization match {
      case LegacyFullVerticesFrequency(grouping) => grouping
      case CompactFrequency(grouping, _)          => grouping
      case CompactC03Owner(grouping, _, _)        => grouping
      case RetainRawRows =>
        throw new IllegalArgumentException(
          "Raw reduction rows do not have an aggregate finalization projection"
        )
    }
    groupingVariables.foreach { variableId =>
      sink.append("      ")
      qualifiedColumn(targetAlias, PostgreSqlNames.variable(variableId), sink)
      sink.append(" AS ")
      quoted(PostgreSqlNames.variable(variableId), sink)
      sink.append(",\n")
    }

    finalization match {
      case CompactC03Owner(_, groupColumnId, measureColumnId) =>
        val groupColumnName = occurrenceColumnName(
          compiled,
          groupColumnId
        )
        val measureColumnName = occurrenceColumnName(
          compiled,
          measureColumnId
        )
        sink.append("      ")
        qualifiedColumn(targetAlias, groupColumnName, sink)
        sink.append(" AS ")
        quoted(groupColumnName, sink)
        sink.append(",\n")
        sink.append("      CAST(COUNT(*) AS NUMERIC) AS ")
        quoted(PostgreSqlNames.CountFrequencyColumn, sink)
        sink.append(",\n      MIN(")
        qualifiedColumn(targetAlias, measureColumnName, sink)
        sink.append(") AS ")
        quoted(PostgreSqlNames.C03LocalMinimumColumn, sink)
        sink.append(",\n      MAX(")
        qualifiedColumn(targetAlias, measureColumnName, sink)
        sink.append(") AS ")
        quoted(PostgreSqlNames.C03LocalMaximumColumn, sink)

      case LegacyFullVerticesFrequency(_) | CompactFrequency(_, _) =>
        sink.append("      CAST(COUNT(*) AS NUMERIC) AS ")
        quoted(PostgreSqlNames.CountFrequencyColumn, sink)

      case RetainRawRows =>
        throw new IllegalArgumentException(
          "Raw reduction rows do not have aggregate state"
        )
    }
  }

  private def emitReductionFinalizationGrouping(
      compiled: CompiledPostgreSqlRendering,
      targetAlias: String,
      finalization: CompiledPostgreSqlReductionFinalization,
      sink: SqlSink
  ): Unit = {
    import CompiledPostgreSqlReductionFinalization._

    val structuralGrouping = finalization match {
      case LegacyFullVerticesFrequency(grouping) => grouping
      case CompactFrequency(grouping, _)          => grouping
      case CompactC03Owner(grouping, _, _)        => grouping
      case RetainRawRows                          => Vector.empty
    }
    val c03GroupColumnName = finalization match {
      case CompactC03Owner(_, groupColumnId, _) =>
        Some(
          occurrenceColumnName(
            compiled,
            groupColumnId
          )
        )
      case _ => None
    }
    val groupingTermCount = structuralGrouping.size + c03GroupColumnName.size
    if (groupingTermCount > 0) {
      sink.append("\n    GROUP BY\n")
      structuralGrouping.zipWithIndex.foreach { case (variableId, index) =>
        if (index > 0) sink.append(",\n")
        sink.append("      ")
        qualifiedColumn(targetAlias, PostgreSqlNames.variable(variableId), sink)
      }
      c03GroupColumnName.foreach { columnName =>
        if (structuralGrouping.nonEmpty) sink.append(",\n")
        sink.append("      ")
        qualifiedColumn(targetAlias, columnName, sink)
      }
    }
    finalization match {
      case CompactFrequency(grouping, suppressEmptyGroup)
          if grouping.isEmpty && suppressEmptyGroup =>
        sink.append("\n    HAVING COUNT(*) > 0")
      case _ => ()
    }
  }

  private def usesStagedCountFrequency(
      compiled: CompiledPostgreSqlRendering
  ): Boolean =
    compiled.reductionCtes.exists(_.finalizesCountFrequency)

  private def usesCompactReductionFinalization(
      compiled: CompiledPostgreSqlRendering
  ): Boolean =
    compiled.reductionCtes.exists(_.finalization match {
      case _: CompiledPostgreSqlReductionFinalization.CompactFrequency | _: CompiledPostgreSqlReductionFinalization.CompactC03Owner =>
        true
      case _ => false
    })

  /**
    * Starts every named CTE with the request-wide physical directive. The
    * planner-controlled branch deliberately preserves the legacy byte stream.
    */
  private def emitCteBodyStart(
      compiled: CompiledPostgreSqlRendering,
      sink: SqlSink,
      initialFactor: Option[BagId] = None
  ): Unit = {
    sink.append(" AS ")
    initialFactor.flatMap(sink.physical.initialFactorDirectives.get).getOrElse(compiled.cteMaterialization) match {
      case PostgreSqlCteMaterializationDirective.PlannerControlled => ()
      case PostgreSqlCteMaterializationDirective.Materialized =>
        sink.append("MATERIALIZED ")
      case PostgreSqlCteMaterializationDirective.NotMaterialized =>
        sink.append("NOT MATERIALIZED ")
    }
    sink.append("(\n")
  }

  private def emitFinalSelect(
      compiled: CompiledPostgreSqlRendering,
      sink: SqlSink
  ): Unit = compiled.terminal match {
    case CompiledPostgreSqlTerminal.Projection(outputScalars) =>
      emitProjectionFinalSelect(compiled, outputScalars, sink)
    case CompiledPostgreSqlTerminal.GlobalCountStar(
          output,
          strategy
        ) =>
      if (usesCollapsedOneBagTerminal(compiled))
        emitCollapsedOneBagGlobalCountStar(compiled, output, sink)
      else
        strategy match {
          case CompiledPostgreSqlTerminal.GlobalCountStrategy
                .FrequencyPropagation(frequencyPlan) =>
            emitFrequencyPropagatedGlobalCountStar(
              compiled,
              output,
              frequencyPlan,
              false,
              sink
            )
          case CompiledPostgreSqlTerminal.GlobalCountStrategy
                .RawFrequencyPropagation(frequencyPlan) =>
            emitFrequencyPropagatedGlobalCountStar(
              compiled,
              output,
              frequencyPlan,
              true,
              sink
            )
          case CompiledPostgreSqlTerminal.GlobalCountStrategy.Reconstruction =>
            emitReconstructedGlobalCountStar(compiled, output, sink)
        }
    case CompiledPostgreSqlTerminal.GroupedCountStar(
          groupColumn,
          groupVariable,
          groupOutput,
          countOutput,
          aggregateBagId,
          frequencyPlan
        ) =>
      if (usesCollapsedOneBagTerminal(compiled))
        emitCollapsedOneBagGroupedCountStar(
          compiled,
          groupVariable,
          groupOutput,
          countOutput,
          sink
        )
      else
        emitFrequencyPropagatedGroupedCountStar(
          compiled,
          groupColumn,
          groupVariable,
          groupOutput,
          countOutput,
          aggregateBagId,
          frequencyPlan,
          sink
        )
    case CompiledPostgreSqlTerminal.DistributedGroupedCountStar(
          groupColumns,
          groupVariables,
          groupOutputs,
          countOutput,
          aggregateBagId,
          propagationPlan
        ) =>
      if (usesCollapsedOneBagTerminal(compiled))
        emitCollapsedOneBagDistributedGroupedCountStar(
          compiled,
          groupVariables,
          groupOutputs,
          countOutput,
          sink
        )
      else
        emitDistributedGroupedCountStar(
          compiled,
          groupColumns,
          groupVariables,
          groupOutputs,
          countOutput,
          aggregateBagId,
          propagationPlan,
          sink
        )
    case CompiledPostgreSqlTerminal.C03G0AggregateVector(
          groupColumn,
          measureColumn,
          outputs,
          aggregateBagId,
          propagationPlan
        ) =>
      if (usesCollapsedOneBagTerminal(compiled))
        emitCollapsedOneBagC03G0AggregateVector(
          compiled,
          groupColumn,
          measureColumn,
          outputs,
          sink
        )
      else
        emitC03G0AggregateVector(
          compiled,
          groupColumn,
          measureColumn,
          outputs,
          aggregateBagId,
          propagationPlan,
          sink
        )
    case CompiledPostgreSqlTerminal.GlobalGuardedExtremum(
          function,
          argument,
          output,
          aggregateBagId,
          usesFullyReducedGuardFactor,
          semijoinPlan
        ) =>
      emitGlobalGuardedExtremum(
        compiled,
        function.sqlName,
        argument,
        output,
        aggregateBagId,
        usesFullyReducedGuardFactor,
        semijoinPlan,
        sink
      )
  }

  private def usesCollapsedOneBagTerminal(
      compiled: CompiledPostgreSqlRendering
  ): Boolean =
    compiled.plan.factors.size == 1 &&
      compiled.aggregateRenderingProfile.oneBagTerminal ==
        PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2

  private def onlyFinalFactor(
      compiled: CompiledPostgreSqlRendering
  ): (BagId, String) = {
    if (compiled.plan.factors.size != 1 || compiled.trace.bags.size != 1)
      throw new IllegalArgumentException(
        "Collapsed aggregate terminal requires exactly one factor"
      )
    val trace = compiled.trace.bags.head
    trace.bagId -> trace.finalFactorCte
  }

  private def emitCollapsedOneBagGlobalCountStar(
      compiled: CompiledPostgreSqlRendering,
      output: PostgreSqlOutputColumn,
      sink: SqlSink
  ): Unit = {
    val (bagId, inputCte) = onlyFinalFactor(compiled)
    val alias = PostgreSqlNames.countFactor(bagId)
    sink.append("SELECT\n  COUNT(*) AS ")
    quoted(output.name, sink)
    sink.append("\nFROM ")
    quoted(inputCte, sink)
    sink.append(" AS ")
    quoted(alias, sink)
  }

  private def emitCollapsedOneBagGroupedCountStar(
      compiled: CompiledPostgreSqlRendering,
      groupVariable: VariableId,
      groupOutput: PostgreSqlOutputColumn,
      countOutput: PostgreSqlOutputColumn,
      sink: SqlSink
  ): Unit = {
    val (bagId, inputCte) = onlyFinalFactor(compiled)
    val alias = PostgreSqlNames.countFactor(bagId)
    sink.append("SELECT\n  ")
    qualifiedColumn(alias, PostgreSqlNames.variable(groupVariable), sink)
    sink.append(" AS ")
    quoted(groupOutput.name, sink)
    sink.append(",\n  COUNT(*) AS ")
    quoted(countOutput.name, sink)
    sink.append("\nFROM ")
    quoted(inputCte, sink)
    sink.append(" AS ")
    quoted(alias, sink)
    sink.append("\nGROUP BY\n  ")
    qualifiedColumn(alias, PostgreSqlNames.variable(groupVariable), sink)
  }

  private def emitCollapsedOneBagDistributedGroupedCountStar(
      compiled: CompiledPostgreSqlRendering,
      groupVariables: Vector[VariableId],
      groupOutputs: Vector[PostgreSqlOutputColumn],
      countOutput: PostgreSqlOutputColumn,
      sink: SqlSink
  ): Unit = {
    val (bagId, inputCte) = onlyFinalFactor(compiled)
    val alias = PostgreSqlNames.countFactor(bagId)
    sink.append("SELECT\n")
    groupVariables.zip(groupOutputs).foreach { case (variableId, output) =>
      sink.append("  ")
      qualifiedColumn(alias, PostgreSqlNames.variable(variableId), sink)
      sink.append(" AS ")
      quoted(output.name, sink)
      sink.append(",\n")
    }
    sink.append("  COUNT(*) AS ")
    quoted(countOutput.name, sink)
    sink.append("\nFROM ")
    quoted(inputCte, sink)
    sink.append(" AS ")
    quoted(alias, sink)
    sink.append("\nGROUP BY\n")
    groupVariables.zipWithIndex.foreach { case (variableId, index) =>
      if (index > 0) sink.append(",\n")
      sink.append("  ")
      qualifiedColumn(alias, PostgreSqlNames.variable(variableId), sink)
    }
  }

  private def emitCollapsedOneBagC03G0AggregateVector(
      compiled: CompiledPostgreSqlRendering,
      groupColumn: CompiledPostgreSqlScalar.Column,
      measureColumn: CompiledPostgreSqlScalar.Column,
      outputs: Vector[PostgreSqlOutputColumn],
      sink: SqlSink
  ): Unit = {
    val (bagId, inputCte) = onlyFinalFactor(compiled)
    val alias = PostgreSqlNames.countFactor(bagId)
    val groupColumnName = occurrenceColumnName(compiled, groupColumn.ref)
    val measureColumnName = occurrenceColumnName(compiled, measureColumn.ref)
    sink.append("SELECT\n  ")
    qualifiedColumn(alias, groupColumnName, sink)
    sink.append(" AS ")
    quoted(outputs(0).name, sink)
    sink.append(",\n  COUNT(*) AS ")
    quoted(outputs(1).name, sink)
    sink.append(",\n  COUNT(")
    qualifiedColumn(alias, groupColumnName, sink)
    sink.append(") AS ")
    quoted(outputs(2).name, sink)
    sink.append(",\n  MIN(")
    qualifiedColumn(alias, measureColumnName, sink)
    sink.append(") AS ")
    quoted(outputs(3).name, sink)
    sink.append(",\n  MAX(")
    qualifiedColumn(alias, measureColumnName, sink)
    sink.append(") AS ")
    quoted(outputs(4).name, sink)
    sink.append("\nFROM ")
    quoted(inputCte, sink)
    sink.append(" AS ")
    quoted(alias, sink)
    sink.append("\nGROUP BY\n  ")
    qualifiedColumn(alias, groupColumnName, sink)
  }

  /** The enumeration branch deliberately retains the legacy byte stream. */
  private def emitProjectionFinalSelect(
      compiled: CompiledPostgreSqlRendering,
      outputScalars: Vector[CompiledPostgreSqlScalar],
      sink: SqlSink
  ): Unit = {
    sink.append("SELECT\n")
    outputScalars.zip(compiled.query.output).zipWithIndex.foreach {
      case ((scalar, output), index) =>
        sink.append("  ")
        emitScalar(
          compiled,
          scalar,
          ref => emitFinalColumn(compiled, ref, sink),
          sink
        )
        sink.append(" AS ")
        quoted(output.name, sink)
        if (index + 1 < compiled.query.output.size) sink.append(",")
        sink.append("\n")
    }

    emitFactorReconstruction(compiled, sink)
  }

  /** Retains the legacy COUNT(*) reconstruction for non-full reduction modes. */
  private def emitReconstructedGlobalCountStar(
      compiled: CompiledPostgreSqlRendering,
      output: PostgreSqlOutputColumn,
      sink: SqlSink
  ): Unit = {
    sink.append("SELECT\n  COUNT(*) AS ")
    quoted(output.name, sink)
    sink.append("\n")
    emitFactorReconstruction(compiled, sink)
  }

  private def emitFactorReconstruction(
      compiled: CompiledPostgreSqlRendering,
      sink: SqlSink
  ): Unit = {
    val finalCteByBag = compiled.trace.bags.iterator
      .map(trace => trace.bagId -> trace.finalFactorCte)
      .toMap
    val root = compiled.plan.root
    sink.append("FROM ")
    quoted(finalCteByBag(root), sink)
    sink.append(" AS ")
    quoted(PostgreSqlNames.finalFactor(root), sink)
    compiled.plan.arcs.foreach { arc =>
      val childAlias = PostgreSqlNames.finalFactor(arc.child)
      if (arc.separator.isEmpty) sink.append("\nCROSS JOIN ")
      else sink.append("\nJOIN ")
      quoted(finalCteByBag(arc.child), sink)
      sink.append(" AS ")
      quoted(childAlias, sink)
      if (arc.separator.nonEmpty) {
        sink.append(" ON\n")
        arc.separator.zipWithIndex.foreach { case (variableId, index) =>
          if (index > 0) sink.append("\n  AND ") else sink.append("  ")
          qualifiedColumn(
            PostgreSqlNames.finalFactor(arc.parent),
            PostgreSqlNames.variable(variableId),
            sink
          )
          sink.append(" = ")
          qualifiedColumn(
            childAlias,
            PostgreSqlNames.variable(variableId),
            sink
          )
        }
      }
    }

    if (compiled.terminalInnerEqualities.nonEmpty) {
      sink.append("\nWHERE\n")
      compiled.terminalInnerEqualities.zipWithIndex.foreach {
        case (predicate, index) =>
          if (index > 0) sink.append("\n  AND ") else sink.append("  ")
          emitScalar(
            compiled,
            predicate.scalar,
            ref => emitFinalColumn(compiled, ref, sink),
            sink
          )
      }
    }
  }

  private sealed trait CountEmission
  private final case class EmitCountMessage(
      message: PostgreSqlFrequencyPropagation.Message
  ) extends CountEmission
  private final case class OpenCountChild(
      message: PostgreSqlFrequencyPropagation.Message
  ) extends CountEmission
  private final case class CloseCountChild(
      parent: BagId,
      message: PostgreSqlFrequencyPropagation.Message
  ) extends CountEmission
  private final case class FinishCountMessage(
      message: PostgreSqlFrequencyPropagation.Message
  ) extends CountEmission

  /**
    * Emits exact sum-product messages as nested derived subqueries inside the
    * existing terminal stage. No additional named CTE or trace stage is
    * introduced, and every factor relation occurs exactly once.
    */
  private def emitFrequencyPropagatedGlobalCountStar(
      compiled: CompiledPostgreSqlRendering,
      output: PostgreSqlOutputColumn,
      frequencyPlan: PostgreSqlFrequencyPropagation.Plan,
      fusedRawFactors: Boolean,
      sink: SqlSink
  ): Unit = {
    PostgreSqlFrequencyPropagation.validate(compiled.plan, frequencyPlan) match {
      case Left(problems) =>
        throw new IllegalArgumentException(
          "Invalid compiled COUNT(*) frequency plan: " +
            problems.map(_.message).mkString("; ")
        )
      case Right(_) => ()
    }

    emitFrequencyPropagatedCountStar(
      compiled,
      output,
      frequencyPlan,
      None,
      fusedRawFactors,
      sink
    )
  }

  private def emitFrequencyPropagatedGroupedCountStar(
      compiled: CompiledPostgreSqlRendering,
      groupColumn: CompiledPostgreSqlScalar.Column,
      groupVariable: VariableId,
      groupOutput: PostgreSqlOutputColumn,
      countOutput: PostgreSqlOutputColumn,
      aggregateBagId: BagId,
      frequencyPlan: PostgreSqlFrequencyPropagation.Plan,
      sink: SqlSink
  ): Unit = {
    PostgreSqlFrequencyPropagation.validateGrouped(
      compiled.plan,
      aggregateBagId,
      groupVariable,
      frequencyPlan
    ) match {
      case Left(problems) =>
        throw new IllegalArgumentException(
          "Invalid compiled grouped COUNT(*) frequency plan: " +
            problems.map(_.message).mkString("; ")
        )
      case Right(_) => ()
    }
    if (
      compiled.query.output.headOption.forall(
        _.expression.columnRefs != Vector(groupColumn.ref)
      )
    ) {
      throw new IllegalArgumentException(
        "Compiled grouped COUNT(*) key differs from QueryIR"
      )
    }

    emitFrequencyPropagatedCountStar(
      compiled,
      countOutput,
      frequencyPlan,
      Some(groupVariable -> groupOutput),
      false,
      sink
    )
  }

  private def emitFrequencyPropagatedCountStar(
      compiled: CompiledPostgreSqlRendering,
      countOutput: PostgreSqlOutputColumn,
      frequencyPlan: PostgreSqlFrequencyPropagation.Plan,
      grouping: Option[(VariableId, PostgreSqlOutputColumn)],
      fusedRawFactors: Boolean,
      sink: SqlSink
  ): Unit = {

    val finalCteByBag = compiled.trace.bags.iterator
      .map(trace => trace.bagId -> trace.finalFactorCte)
      .toMap
    val messageByOrdinal = frequencyPlan.messages.iterator
      .map(message => message.ordinal -> message)
      .toMap
    val localBagById = frequencyPlan.bagById
    val inputHasFrequency = usesStagedCountFrequency(compiled)
    val inputIsCompact = usesCompactReductionFinalization(compiled)
    sink.append("SELECT\n")
    grouping.foreach { case (variableId, output) =>
      sink.append("  ")
      qualifiedColumn(
        PostgreSqlNames.countFactor(frequencyPlan.root),
        PostgreSqlNames.variable(variableId),
        sink
      )
      sink.append(" AS ")
      quoted(output.name, sink)
      sink.append(",\n")
    }
    sink.append("  ")
    if (
      fusedRawFactors &&
      frequencyPlan.finalization.inputMessageOrdinals.isEmpty
    ) sink.append("COUNT(*)")
    else {
      sink.append("CAST(COALESCE(SUM(")
      emitCountWeight(
        frequencyPlan.root,
        frequencyPlan.finalization.inputMessageOrdinals,
        messageByOrdinal,
        fusedRawFactors,
        sink
      )
      sink.append("), CAST(0 AS NUMERIC)) AS BIGINT)")
    }
    sink.append(" AS ")
    quoted(countOutput.name, sink)
    sink.append("\nFROM ")
    emitCountLocalFactorSource(
      localBagById(frequencyPlan.root),
      finalCteByBag(frequencyPlan.root),
      inputHasFrequency,
      inputIsCompact,
      fusedRawFactors,
      sink
    )
    sink.append(" AS ")
    quoted(PostgreSqlNames.countFactor(frequencyPlan.root), sink)

    var pending = countChildEvents(
      frequencyPlan.root,
      frequencyPlan.finalization.inputMessageOrdinals,
      messageByOrdinal
    )
    while (pending.nonEmpty) {
      pending.head match {
        case OpenCountChild(message) =>
          val direct = isExactDirectFactorMessage(compiled, message.ordinal)
          if (message.separator.isEmpty) sink.append("\nCROSS JOIN ")
          else sink.append("\nJOIN ")
          if (!direct) sink.append("(\n")
          pending = pending.tail

        case EmitCountMessage(message) =>
          if (isExactDirectFactorMessage(compiled, message.ordinal))
            quoted(finalCteByBag(message.source), sink)
          else
            emitCountMessagePrelude(
              message,
              messageByOrdinal,
              finalCteByBag,
              localBagById,
              inputHasFrequency,
              inputIsCompact,
              fusedRawFactors,
              sink
            )
          pending =
            countChildEvents(
              message.source,
              message.inputMessageOrdinals,
              messageByOrdinal
            ) ::: FinishCountMessage(message) :: pending.tail

        case CloseCountChild(parent, message) =>
          if (isExactDirectFactorMessage(compiled, message.ordinal))
            sink.append(" AS ")
          else sink.append("\n) AS ")
          quoted(PostgreSqlNames.countMessage(message.source), sink)
          emitCountMessageJoin(parent, message, sink)
          pending = pending.tail

        case FinishCountMessage(message) =>
          if (
            (fusedRawFactors || message.requiresRegrouping) &&
            message.separator.nonEmpty
          ) {
            sink.append("\nGROUP BY\n")
            message.separator.zipWithIndex.foreach { case (variableId, index) =>
              if (index > 0) sink.append(",\n")
              sink.append("  ")
              qualifiedColumn(
                PostgreSqlNames.countFactor(message.source),
                PostgreSqlNames.variable(variableId),
                sink
              )
            }
          }
          pending = pending.tail
      }
    }
    grouping.foreach { case (variableId, _) =>
      sink.append("\nGROUP BY\n  ")
      qualifiedColumn(
        PostgreSqlNames.countFactor(frequencyPlan.root),
        PostgreSqlNames.variable(variableId),
        sink
      )
    }
  }

  private def isExactDirectFactorMessage(
      compiled: CompiledPostgreSqlRendering,
      ordinal: Int
  ): Boolean =
    compiled.aggregateRendererV2Trace.elidedLogicalMessageOrdinals.contains(
      ordinal
    )

  private def countChildEvents(
      parent: BagId,
      inputMessageOrdinals: Vector[Int],
      messageByOrdinal: Map[Int, PostgreSqlFrequencyPropagation.Message]
  ): List[CountEmission] =
    inputMessageOrdinals.toList.flatMap { ordinal =>
      val message = messageByOrdinal(ordinal)
      List(
        OpenCountChild(message),
        EmitCountMessage(message),
        CloseCountChild(parent, message)
      )
    }

  private def emitCountMessagePrelude(
      message: PostgreSqlFrequencyPropagation.Message,
      messageByOrdinal: Map[Int, PostgreSqlFrequencyPropagation.Message],
      finalCteByBag: Map[BagId, String],
      localBagById: Map[BagId, PostgreSqlFrequencyPropagation.Bag],
      inputHasFrequency: Boolean,
      inputIsCompact: Boolean,
      fusedRawFactors: Boolean,
      sink: SqlSink
  ): Unit = {
    val factorAlias = PostgreSqlNames.countFactor(message.source)
    sink.append("SELECT\n")
    message.separator.foreach { variableId =>
      sink.append("  ")
      qualifiedColumn(
        factorAlias,
        PostgreSqlNames.variable(variableId),
        sink
      )
      sink.append(" AS ")
      quoted(PostgreSqlNames.variable(variableId), sink)
      sink.append(",\n")
    }
    sink.append("  ")
    val requiresRegrouping = fusedRawFactors || message.requiresRegrouping
    if (fusedRawFactors && message.inputMessageOrdinals.isEmpty)
      sink.append("CAST(COUNT(*) AS NUMERIC)")
    else {
      if (requiresRegrouping) sink.append("COALESCE(SUM(")
      emitCountWeight(
        message.source,
        message.inputMessageOrdinals,
        messageByOrdinal,
        fusedRawFactors,
        sink
      )
      if (requiresRegrouping)
        sink.append("), CAST(0 AS NUMERIC))")
    }
    sink.append(" AS ")
    quoted(PostgreSqlNames.CountFrequencyColumn, sink)
    sink.append("\nFROM ")
    emitCountLocalFactorSource(
      localBagById(message.source),
      finalCteByBag(message.source),
      inputHasFrequency,
      inputIsCompact,
      fusedRawFactors,
      sink
    )
    sink.append(" AS ")
    quoted(factorAlias, sink)
  }

  private def emitCountLocalFactorSource(
      bag: PostgreSqlFrequencyPropagation.Bag,
      inputCte: String,
      inputHasFrequency: Boolean,
      inputIsCompact: Boolean,
      fusedRawFactors: Boolean,
      sink: SqlSink
  ): Unit = {
    if (fusedRawFactors) {
      quoted(inputCte, sink)
    } else if (inputIsCompact) {
      if (!inputHasFrequency)
        throw new IllegalArgumentException(
          "Compact COUNT factor input must carry an exact frequency"
        )
      quoted(inputCte, sink)
    } else {
      sink.append("(\n")
      emitCountLocalFactor(bag, inputCte, inputHasFrequency, sink)
      sink.append("\n)")
    }
  }

  private def emitCountWeight(
      localBagId: BagId,
      inputMessageOrdinals: Vector[Int],
      messageByOrdinal: Map[Int, PostgreSqlFrequencyPropagation.Message],
      fusedRawFactors: Boolean,
      sink: SqlSink
  ): Unit = {
    if (fusedRawFactors) {
      require(
        inputMessageOrdinals.nonEmpty,
        "fused raw-factor weight requires at least one child message"
      )
      if (inputMessageOrdinals.size > 1) sink.append("(")
      inputMessageOrdinals.zipWithIndex.foreach { case (ordinal, index) =>
        if (index > 0) sink.append(" * ")
        val message = messageByOrdinal(ordinal)
        qualifiedColumn(
          PostgreSqlNames.countMessage(message.source),
          PostgreSqlNames.CountFrequencyColumn,
          sink
        )
      }
      if (inputMessageOrdinals.size > 1) sink.append(")")
    } else {
      if (inputMessageOrdinals.nonEmpty) sink.append("(")
      qualifiedColumn(
        PostgreSqlNames.countFactor(localBagId),
        PostgreSqlNames.CountFrequencyColumn,
        sink
      )
      if (inputMessageOrdinals.nonEmpty) {
        inputMessageOrdinals.foreach { ordinal =>
          sink.append(" * ")
          val message = messageByOrdinal(ordinal)
          qualifiedColumn(
            PostgreSqlNames.countMessage(message.source),
            PostgreSqlNames.CountFrequencyColumn,
            sink
          )
        }
        sink.append(")")
      }
    }
  }

  /**
    * Eliminates every local variable that no neighboring message can observe.
    * In a multi-bag FullTwoPass plan, the latest factor already carries exact
    * full-bag frequencies created at the unique point where that bag became
    * globally extendible. This stage merely sums those frequencies after
    * eliminating variables no neighboring message observes. A one-bag plan
    * is globally extendible without reduction and counts its rows directly.
    */
  private def emitCountLocalFactor(
      bag: PostgreSqlFrequencyPropagation.Bag,
      inputCte: String,
      inputHasFrequency: Boolean,
      sink: SqlSink
  ): Unit = {
    val inputAlias = PostgreSqlNames.countFactorInput(bag.bagId)
    sink.append("SELECT\n")
    bag.localGrouping.foreach { variableId =>
      sink.append("  ")
      qualifiedColumn(
        inputAlias,
        PostgreSqlNames.variable(variableId),
        sink
      )
      sink.append(" AS ")
      quoted(PostgreSqlNames.variable(variableId), sink)
      sink.append(",\n")
    }
    if (inputHasFrequency) {
      sink.append("  COALESCE(SUM(")
      qualifiedColumn(
        inputAlias,
        PostgreSqlNames.CountFrequencyColumn,
        sink
      )
      sink.append("), CAST(0 AS NUMERIC)) AS ")
    } else {
      sink.append("  CAST(COUNT(*) AS NUMERIC) AS ")
    }
    quoted(PostgreSqlNames.CountFrequencyColumn, sink)
    sink.append("\nFROM ")
    quoted(inputCte, sink)
    sink.append(" AS ")
    quoted(inputAlias, sink)
    if (bag.localGrouping.nonEmpty) {
      sink.append("\nGROUP BY\n")
      bag.localGrouping.zipWithIndex.foreach { case (variableId, index) =>
        if (index > 0) sink.append(",\n")
        sink.append("  ")
        qualifiedColumn(
          inputAlias,
          PostgreSqlNames.variable(variableId),
          sink
        )
      }
    }
  }

  private def emitCountMessageJoin(
      parent: BagId,
      message: PostgreSqlFrequencyPropagation.Message,
      sink: SqlSink
  ): Unit = {
    if (message.separator.nonEmpty) {
      sink.append(" ON\n")
      message.separator.zipWithIndex.foreach { case (variableId, index) =>
        if (index > 0) sink.append("\n  AND ") else sink.append("  ")
        qualifiedColumn(
          PostgreSqlNames.countFactor(parent),
          PostgreSqlNames.variable(variableId),
          sink
        )
        sink.append(" = ")
        qualifiedColumn(
          PostgreSqlNames.countMessage(message.source),
          PostgreSqlNames.variable(variableId),
          sink
        )
      }
    }
  }

  /**
    * Lowers the independently validated U1 annotation plan. Carried grouping
    * keys are projected and grouped, never compared: only the ordinary
    * decomposition separator of a child message is emitted as a NULL-
    * rejecting join predicate.
    */
  private def emitDistributedGroupedCountStar(
      compiled: CompiledPostgreSqlRendering,
      groupColumns: Vector[CompiledPostgreSqlScalar.Column],
      groupVariables: Vector[VariableId],
      groupOutputs: Vector[PostgreSqlOutputColumn],
      countOutput: PostgreSqlOutputColumn,
      aggregateBagId: BagId,
      propagationPlan: PostgreSqlAggregatePropagation.Plan,
      sink: SqlSink
  ): Unit = {
    val request = PostgreSqlAggregatePropagation.AggregateRequest
      .countStarGroupedBy(
        groupVariables.map(
          PostgreSqlAggregatePropagation.AggregateKey.Structural
        ): _*
      )
    PostgreSqlAggregatePropagation.validate(
      compiled.plan,
      aggregateBagId,
      request,
      propagationPlan
    ) match {
      case Left(problems) =>
        throw new IllegalArgumentException(
          "Invalid compiled distributed grouped COUNT(*) propagation plan: " +
            problems.map(_.message).mkString("; ")
        )
      case Right(_) => ()
    }
    if (
      groupColumns.size < 2 ||
      compiled.query.output.take(groupColumns.size).map(_.expression.columnRefs) !=
        groupColumns.map(column => Vector(column.ref))
    ) {
      throw new IllegalArgumentException(
        "Compiled distributed grouped COUNT(*) keys differ from QueryIR"
      )
    }

    val finalCteByBag = compiled.trace.bags.iterator
      .map(trace => trace.bagId -> trace.finalFactorCte)
      .toMap
    val messageByOrdinal = propagationPlan.messages.iterator
      .map(message => message.ordinal -> message)
      .toMap
    val messageBySource = propagationPlan.messageBySource
    val inputHasFrequency = usesStagedCountFrequency(compiled)
    val inputIsCompact = usesCompactReductionFinalization(compiled)

    sink.append("SELECT\n")
    propagationPlan.finalization.groupKeys.zip(groupOutputs).foreach {
      case (key, output) =>
        sink.append("  ")
        emitAggregateKeyProvider(
          propagationPlan,
          propagationPlan.root,
          key,
          messageBySource,
          sink
        )
        sink.append(" AS ")
        quoted(output.name, sink)
        sink.append(",\n")
    }
    sink.append("  CAST(COALESCE(SUM(")
    emitAggregateWeight(
      propagationPlan.root,
      propagationPlan.finalization.inputMessageOrdinals,
      messageByOrdinal,
      sink
    )
    sink.append("), CAST(0 AS NUMERIC)) AS BIGINT) AS ")
    quoted(countOutput.name, sink)
    sink.append("\nFROM ")
    emitAggregateLocalFactorSource(
      propagationPlan.bagById(propagationPlan.root),
      finalCteByBag(propagationPlan.root),
      inputHasFrequency,
      inputIsCompact,
      sink
    )
    sink.append(" AS ")
    quoted(PostgreSqlNames.countFactor(propagationPlan.root), sink)
    propagationPlan.finalization.inputMessageOrdinals.foreach { ordinal =>
      emitAggregateChildJoin(
        compiled,
        propagationPlan.root,
        messageByOrdinal(ordinal),
        propagationPlan,
        messageByOrdinal,
        finalCteByBag,
        inputHasFrequency,
        inputIsCompact,
        sink
      )
    }
    sink.append("\nGROUP BY\n")
    propagationPlan.finalization.groupKeys.zipWithIndex.foreach {
      case (key, index) =>
        if (index > 0) sink.append(",\n")
        sink.append("  ")
        emitAggregateKeyProvider(
          propagationPlan,
          propagationPlan.root,
          key,
          messageBySource,
          sink
        )
    }
  }

  /**
    * Lowers the exact owner-local C03-G0 vector from fully reduced owner input.
    * Legacy input is grouped here; compact input already carries equivalent
    * G/frequency/MIN/MAX state. G and X never enter a message schema, and the
    * owner joins only the ordinary count messages of its terminal-oriented
    * children.
    */
  private def emitC03G0AggregateVector(
      compiled: CompiledPostgreSqlRendering,
      groupColumn: CompiledPostgreSqlScalar.Column,
      measureColumn: CompiledPostgreSqlScalar.Column,
      outputs: Vector[PostgreSqlOutputColumn],
      aggregateBagId: BagId,
      propagationPlan: PostgreSqlAggregatePropagation.Plan,
      sink: SqlSink
  ): Unit = {
    val groupKey =
      PostgreSqlAggregatePropagation.AggregateKey.OccurrenceValue(
        groupColumn.ref.id
      )
    val request = PostgreSqlAggregatePropagation.AggregateRequest
      .countStarGroupedBy(groupKey)
    PostgreSqlAggregatePropagation.validate(
      compiled.plan,
      aggregateBagId,
      request,
      propagationPlan
    ) match {
      case Left(problems) =>
        throw new IllegalArgumentException(
          "Invalid compiled C03-G0 propagation plan: " +
            problems.map(_.message).mkString("; ")
        )
      case Right(_) => ()
    }

    val queryShapeMatches = compiled.query.resultShape match {
      case shape: QueryResultShape.C03G0AggregateVector =>
        shape.groupColumn == groupColumn.ref &&
          shape.measureColumn == measureColumn.ref &&
          shape.outputs == compiled.query.output
      case _ => false
    }
    val payloadColumnIds = Set(groupColumn.ref.id, measureColumn.ref.id)
    val payloadIsOwnerLocal = compiled.edgeByOccurrence
      .get(groupColumn.ref.relationOccurrenceId)
      .exists(_.bindings.forall { case (columnId, _) =>
        !payloadColumnIds(columnId)
      }) && compiled.query.predicates.forall(predicate =>
      predicate.expression.columnRefs.forall(ref => !payloadColumnIds(ref.id))
    )
    if (
      !queryShapeMatches || outputs != compiled.output ||
      !payloadIsOwnerLocal ||
      groupColumn.ref.relationOccurrenceId !=
        measureColumn.ref.relationOccurrenceId ||
      compiled.ownerByCarriedColumn.get(groupColumn.ref.id) !=
        Some(aggregateBagId) ||
      compiled.ownerByCarriedColumn.get(measureColumn.ref.id) !=
        Some(aggregateBagId) ||
      propagationPlan.keyBindings.size != 1 ||
      propagationPlan.keyBindings.head.key != groupKey ||
      propagationPlan.keyBindings.head.owner != aggregateBagId ||
      propagationPlan.laneBindings.nonEmpty ||
      propagationPlan.messages.exists(_.carriedKeys.nonEmpty)
    ) {
      throw new IllegalArgumentException(
        "Compiled C03-G0 terminal differs from its QueryIR or owner-local certificate"
      )
    }
    val inputHasFrequency = usesStagedCountFrequency(compiled)
    val inputIsCompact = usesCompactReductionFinalization(compiled)
    if (inputHasFrequency && !inputIsCompact) {
      throw new IllegalArgumentException(
        "C03-G0 frequency state must use the compact owner-local finalization"
      )
    }

    val finalCteByBag = compiled.trace.bags.iterator
      .map(trace => trace.bagId -> trace.finalFactorCte)
      .toMap
    val messageByOrdinal = propagationPlan.messages.iterator
      .map(message => message.ordinal -> message)
      .toMap
    val groupColumnName = occurrenceColumnName(compiled, groupColumn.ref)
    val measureColumnName = occurrenceColumnName(compiled, measureColumn.ref)
    val rootAlias = PostgreSqlNames.countFactor(propagationPlan.root)

    sink.append("SELECT\n  ")
    qualifiedColumn(rootAlias, groupColumnName, sink)
    sink.append(" AS ")
    quoted(outputs(0).name, sink)
    sink.append(",\n  ")
    emitC03G0FinalCount(propagationPlan, messageByOrdinal, sink)
    sink.append(" AS ")
    quoted(outputs(1).name, sink)
    sink.append(",\n  CASE WHEN ")
    qualifiedColumn(rootAlias, groupColumnName, sink)
    sink.append(" IS NULL THEN CAST(0 AS BIGINT) ELSE ")
    emitC03G0FinalCount(propagationPlan, messageByOrdinal, sink)
    sink.append(" END AS ")
    quoted(outputs(2).name, sink)
    sink.append(",\n  MIN(")
    qualifiedColumn(rootAlias, PostgreSqlNames.C03LocalMinimumColumn, sink)
    sink.append(") AS ")
    quoted(outputs(3).name, sink)
    sink.append(",\n  MAX(")
    qualifiedColumn(rootAlias, PostgreSqlNames.C03LocalMaximumColumn, sink)
    sink.append(") AS ")
    quoted(outputs(4).name, sink)
    sink.append("\nFROM ")
    if (inputIsCompact) quoted(finalCteByBag(propagationPlan.root), sink)
    else {
      sink.append("(\n")
      emitC03G0OwnerLocalFactor(
        propagationPlan.bagById(propagationPlan.root),
        finalCteByBag(propagationPlan.root),
        groupColumnName,
        measureColumnName,
        sink
      )
      sink.append("\n)")
    }
    sink.append(" AS ")
    quoted(rootAlias, sink)
    propagationPlan.finalization.inputMessageOrdinals.foreach { ordinal =>
      emitAggregateChildJoin(
        compiled,
        propagationPlan.root,
        messageByOrdinal(ordinal),
        propagationPlan,
        messageByOrdinal,
        finalCteByBag,
        inputHasFrequency = inputHasFrequency,
        inputIsCompact = inputIsCompact,
        sink = sink
      )
    }
    sink.append("\nGROUP BY\n  ")
    qualifiedColumn(rootAlias, groupColumnName, sink)
  }

  private def emitC03G0FinalCount(
      propagationPlan: PostgreSqlAggregatePropagation.Plan,
      messageByOrdinal: Map[Int, PostgreSqlAggregatePropagation.Message],
      sink: SqlSink
  ): Unit = {
    sink.append("CAST(COALESCE(SUM(")
    emitAggregateWeight(
      propagationPlan.root,
      propagationPlan.finalization.inputMessageOrdinals,
      messageByOrdinal,
      sink
    )
    sink.append("), CAST(0 AS NUMERIC)) AS BIGINT)")
  }

  private def emitC03G0OwnerLocalFactor(
      bag: PostgreSqlAggregatePropagation.Bag,
      inputCte: String,
      groupColumnName: String,
      measureColumnName: String,
      sink: SqlSink
  ): Unit = {
    if (
      bag.ownedValueKeyColumns.size != 1 || bag.ownedLanes.nonEmpty
    ) {
      throw new IllegalArgumentException(
        "C03-G0 owner-local factor must own exactly one payload key and no lanes"
      )
    }
    val inputAlias = PostgreSqlNames.countFactorInput(bag.bagId)
    sink.append("SELECT\n")
    bag.localGroupingVariables.foreach { variableId =>
      sink.append("  ")
      qualifiedColumn(inputAlias, PostgreSqlNames.variable(variableId), sink)
      sink.append(" AS ")
      quoted(PostgreSqlNames.variable(variableId), sink)
      sink.append(",\n")
    }
    sink.append("  ")
    qualifiedColumn(inputAlias, groupColumnName, sink)
    sink.append(" AS ")
    quoted(groupColumnName, sink)
    sink.append(",\n  CAST(COUNT(*) AS NUMERIC) AS ")
    quoted(PostgreSqlNames.CountFrequencyColumn, sink)
    sink.append(",\n  MIN(")
    qualifiedColumn(inputAlias, measureColumnName, sink)
    sink.append(") AS ")
    quoted(PostgreSqlNames.C03LocalMinimumColumn, sink)
    sink.append(",\n  MAX(")
    qualifiedColumn(inputAlias, measureColumnName, sink)
    sink.append(") AS ")
    quoted(PostgreSqlNames.C03LocalMaximumColumn, sink)
    sink.append("\nFROM ")
    quoted(inputCte, sink)
    sink.append(" AS ")
    quoted(inputAlias, sink)
    sink.append("\nGROUP BY\n")
    bag.localGroupingVariables.foreach { variableId =>
      sink.append("  ")
      qualifiedColumn(inputAlias, PostgreSqlNames.variable(variableId), sink)
      sink.append(",\n")
    }
    sink.append("  ")
    qualifiedColumn(inputAlias, groupColumnName, sink)
  }

  private def occurrenceColumnName(
      compiled: CompiledPostgreSqlRendering,
      ref: ColumnRef
  ): String = {
    val edge = compiled.edgeByOccurrence.getOrElse(
      ref.relationOccurrenceId,
      throw new IllegalArgumentException(
        s"C03-G0 occurrence ${ref.relationOccurrenceId.value} has no unique edge"
      )
    )
    PostgreSqlNames.occurrenceColumn(edge.id, ref.id)
  }

  private def occurrenceColumnName(
      compiled: CompiledPostgreSqlRendering,
      columnId: ColumnId
  ): String = {
    val ref = compiled.metadataByColumn
      .get(columnId)
      .map(_.ref)
      .getOrElse(
        throw new IllegalArgumentException(
          s"Aggregate finalization column $columnId has no compiled metadata"
        )
      )
    occurrenceColumnName(compiled, ref)
  }

  private def emitAggregateChildJoin(
      compiled: CompiledPostgreSqlRendering,
      parent: BagId,
      message: PostgreSqlAggregatePropagation.Message,
      plan: PostgreSqlAggregatePropagation.Plan,
      messageByOrdinal: Map[Int, PostgreSqlAggregatePropagation.Message],
      finalCteByBag: Map[BagId, String],
      inputHasFrequency: Boolean,
      inputIsCompact: Boolean,
      sink: SqlSink
  ): Unit = {
    val direct = isExactDirectFactorMessage(compiled, message.ordinal)
    if (message.separator.isEmpty) sink.append("\nCROSS JOIN ")
    else sink.append("\nJOIN ")
    if (direct) quoted(finalCteByBag(message.source), sink)
    else {
      sink.append("(\n")
      emitAggregateMessage(
        compiled,
        message,
        plan,
        messageByOrdinal,
        finalCteByBag,
        inputHasFrequency,
        inputIsCompact,
        sink
      )
      sink.append("\n)")
    }
    sink.append(" AS ")
    quoted(PostgreSqlNames.countMessage(message.source), sink)
    if (message.separator.nonEmpty) {
      sink.append(" ON\n")
      message.separator.zipWithIndex.foreach { case (variableId, index) =>
        if (index > 0) sink.append("\n  AND ") else sink.append("  ")
        qualifiedColumn(
          PostgreSqlNames.countFactor(parent),
          PostgreSqlNames.variable(variableId),
          sink
        )
        sink.append(" = ")
        qualifiedColumn(
          PostgreSqlNames.countMessage(message.source),
          PostgreSqlNames.variable(variableId),
          sink
        )
      }
    }
  }

  private def emitAggregateMessage(
      compiled: CompiledPostgreSqlRendering,
      message: PostgreSqlAggregatePropagation.Message,
      plan: PostgreSqlAggregatePropagation.Plan,
      messageByOrdinal: Map[Int, PostgreSqlAggregatePropagation.Message],
      finalCteByBag: Map[BagId, String],
      inputHasFrequency: Boolean,
      inputIsCompact: Boolean,
      sink: SqlSink
  ): Unit = {
    val factorAlias = PostgreSqlNames.countFactor(message.source)
    val messageBySource = plan.messageBySource
    sink.append("SELECT\n")
    message.separator.foreach { variableId =>
      sink.append("  ")
      qualifiedColumn(factorAlias, PostgreSqlNames.variable(variableId), sink)
      sink.append(" AS ")
      quoted(PostgreSqlNames.variable(variableId), sink)
      sink.append(",\n")
    }
    message.carriedKeys.foreach { key =>
      sink.append("  ")
      emitAggregateKeyProvider(
        plan,
        message.source,
        key,
        messageBySource,
        sink
      )
      sink.append(" AS ")
      quoted(aggregateKeyColumnName(key), sink)
      sink.append(",\n")
    }
    sink.append("  ")
    if (message.requiresRegrouping) sink.append("COALESCE(SUM(")
    emitAggregateWeight(
      message.source,
      message.inputMessageOrdinals,
      messageByOrdinal,
      sink
    )
    if (message.requiresRegrouping)
      sink.append("), CAST(0 AS NUMERIC))")
    sink.append(" AS ")
    quoted(PostgreSqlNames.CountFrequencyColumn, sink)
    sink.append("\nFROM ")
    emitAggregateLocalFactorSource(
      plan.bagById(message.source),
      finalCteByBag(message.source),
      inputHasFrequency,
      inputIsCompact,
      sink
    )
    sink.append(" AS ")
    quoted(factorAlias, sink)
    message.inputMessageOrdinals.foreach { ordinal =>
      emitAggregateChildJoin(
        compiled,
        message.source,
        messageByOrdinal(ordinal),
        plan,
        messageByOrdinal,
        finalCteByBag,
        inputHasFrequency,
        inputIsCompact,
        sink
      )
    }
    if (message.requiresRegrouping) {
      val schema = message.separator.map(
        PostgreSqlAggregatePropagation.AggregateKey.Structural
      ) ++ message.carriedKeys
      if (schema.nonEmpty) {
        sink.append("\nGROUP BY\n")
        schema.zipWithIndex.foreach { case (key, index) =>
          if (index > 0) sink.append(",\n")
          sink.append("  ")
          key match {
            case PostgreSqlAggregatePropagation.AggregateKey.Structural(variable)
                if message.separator.contains(variable) =>
              qualifiedColumn(
                factorAlias,
                PostgreSqlNames.variable(variable),
                sink
              )
            case carried =>
              emitAggregateKeyProvider(
                plan,
                message.source,
                carried,
                messageBySource,
                sink
              )
          }
        }
      }
    }
  }

  private def emitAggregateLocalFactorSource(
      bag: PostgreSqlAggregatePropagation.Bag,
      inputCte: String,
      inputHasFrequency: Boolean,
      inputIsCompact: Boolean,
      sink: SqlSink
  ): Unit = {
    if (inputIsCompact) {
      if (!inputHasFrequency)
        throw new IllegalArgumentException(
          "Compact aggregate factor input must carry an exact frequency"
        )
      quoted(inputCte, sink)
    } else {
      sink.append("(\n")
      emitAggregateLocalFactor(bag, inputCte, inputHasFrequency, sink)
      sink.append("\n)")
    }
  }

  private def emitAggregateLocalFactor(
      bag: PostgreSqlAggregatePropagation.Bag,
      inputCte: String,
      inputHasFrequency: Boolean,
      sink: SqlSink
  ): Unit = {
    if (bag.ownedValueKeyColumns.nonEmpty || bag.ownedLanes.nonEmpty)
      throw new IllegalArgumentException(
        "Generic aggregate local factors cannot lower payload keys or measure lanes"
      )
    val inputAlias = PostgreSqlNames.countFactorInput(bag.bagId)
    sink.append("SELECT\n")
    bag.localGroupingVariables.foreach { variableId =>
      sink.append("  ")
      qualifiedColumn(inputAlias, PostgreSqlNames.variable(variableId), sink)
      sink.append(" AS ")
      quoted(PostgreSqlNames.variable(variableId), sink)
      sink.append(",\n")
    }
    if (inputHasFrequency) {
      sink.append("  COALESCE(SUM(")
      qualifiedColumn(
        inputAlias,
        PostgreSqlNames.CountFrequencyColumn,
        sink
      )
      sink.append("), CAST(0 AS NUMERIC)) AS ")
    } else sink.append("  CAST(COUNT(*) AS NUMERIC) AS ")
    quoted(PostgreSqlNames.CountFrequencyColumn, sink)
    sink.append("\nFROM ")
    quoted(inputCte, sink)
    sink.append(" AS ")
    quoted(inputAlias, sink)
    if (bag.localGroupingVariables.nonEmpty) {
      sink.append("\nGROUP BY\n")
      bag.localGroupingVariables.zipWithIndex.foreach {
        case (variableId, index) =>
          if (index > 0) sink.append(",\n")
          sink.append("  ")
          qualifiedColumn(inputAlias, PostgreSqlNames.variable(variableId), sink)
      }
    }
  }

  private def emitAggregateWeight(
      localBagId: BagId,
      inputMessageOrdinals: Vector[Int],
      messageByOrdinal: Map[Int, PostgreSqlAggregatePropagation.Message],
      sink: SqlSink
  ): Unit = {
    if (inputMessageOrdinals.nonEmpty) sink.append("(")
    qualifiedColumn(
      PostgreSqlNames.countFactor(localBagId),
      PostgreSqlNames.CountFrequencyColumn,
      sink
    )
    inputMessageOrdinals.foreach { ordinal =>
      sink.append(" * ")
      val message = messageByOrdinal(ordinal)
      qualifiedColumn(
        PostgreSqlNames.countMessage(message.source),
        PostgreSqlNames.CountFrequencyColumn,
        sink
      )
    }
    if (inputMessageOrdinals.nonEmpty) sink.append(")")
  }

  private def emitAggregateKeyProvider(
      plan: PostgreSqlAggregatePropagation.Plan,
      bagId: BagId,
      key: PostgreSqlAggregatePropagation.AggregateKey,
      messageBySource: Map[BagId, PostgreSqlAggregatePropagation.Message],
      sink: SqlSink
  ): Unit = {
    val bag = plan.bagById(bagId)
    val local = bag.ownedKeys.contains(key)
    val childSources = bag.children.filter(child =>
      messageBySource.get(child).exists(_.carriedKeys.contains(key))
    )
    if (local && childSources.isEmpty)
      qualifiedColumn(
        PostgreSqlNames.countFactor(bagId),
        aggregateKeyColumnName(key),
        sink
      )
    else if (!local && childSources.size == 1)
      qualifiedColumn(
        PostgreSqlNames.countMessage(childSources.head),
        aggregateKeyColumnName(key),
        sink
      )
    else
      throw new IllegalArgumentException(
        s"Aggregate key $key has ${if (local) 1 else 0} local and ${childSources.size} child providers at bag ${bagId.value}"
      )
  }

  private def aggregateKeyColumnName(
      key: PostgreSqlAggregatePropagation.AggregateKey
  ): String = key match {
    case PostgreSqlAggregatePropagation.AggregateKey.Structural(variable) =>
      PostgreSqlNames.variable(variable)
    case PostgreSqlAggregatePropagation.AggregateKey.OccurrenceValue(_) =>
      throw new IllegalArgumentException(
        "Occurrence-valued aggregate keys are not lowerable in U1"
      )
  }

  private def emitGlobalGuardedExtremum(
      compiled: CompiledPostgreSqlRendering,
      functionName: String,
      argument: CompiledPostgreSqlScalar.Column,
      output: PostgreSqlOutputColumn,
      aggregateBagId: BagId,
      usesFullyReducedGuardFactor: Boolean,
      semijoinPlan: PostgreSqlGuardedExtremumSemijoin.Plan,
      sink: SqlSink
  ): Unit = {
    PostgreSqlGuardedExtremumSemijoin.validate(
      compiled.plan,
      argument.ref,
      aggregateBagId,
      semijoinPlan
    ) match {
      case Left(problems) =>
        throw new IllegalArgumentException(
          "Invalid compiled guarded MIN/MAX semijoin plan: " +
            problems.map(_.message).mkString("; ")
        )
      case Right(_) => ()
    }

    if (usesFullyReducedGuardFactor) {
      emitFullyReducedGuardFactorExtremum(
        compiled,
        functionName,
        argument,
        output,
        aggregateBagId,
        sink
      )
      return
    }

    val guardHandle = semijoinPlan.terminalHandles.head
    val guardEdge = edgeForHandle(compiled, guardHandle)
    guardHandle.source match {
      case PostgreSqlGuardedExtremumSemijoin.HandleSource.Base(edgeId)
          if edgeId == semijoinPlan.guardEdgeId => ()
      case _ =>
        throw new IllegalArgumentException(
          "Guarded MIN/MAX terminal does not start with its base guard"
        )
    }
    val guardRelation = compiled.relationByOccurrence.getOrElse(
      guardEdge.relationOccurrenceId,
      throw new IllegalArgumentException(
        s"Missing guard occurrence ${guardEdge.relationOccurrenceId.value}"
      )
    )
    val guardAlias = extremumTerminalAlias(guardHandle)

    sink.append("SELECT\n  ")
    sink.append(functionName)
    sink.append("(")
    emitSourceColumn(compiled, guardAlias, argument.ref.id, sink)
    sink.append(") AS ")
    quoted(output.name, sink)
    sink.append("\nFROM ")
    emitRelation(guardRelation, sink)
    sink.append(" AS ")
    quoted(guardAlias, sink)

    val guardPredicates = extremumBasePredicates(compiled, guardEdge.id)
    val existentialHandles = semijoinPlan.terminalHandles.tail
    if (guardPredicates.nonEmpty || existentialHandles.nonEmpty) {
      sink.append("\nWHERE\n")
    }
    var outerTerms = 0
    guardPredicates.foreach { predicate =>
      if (outerTerms > 0) sink.append("\n  AND ") else sink.append("  ")
      emitScalar(
        compiled,
        predicate.scalar,
        ref => emitSourceColumn(compiled, guardAlias, ref.id, sink),
        sink
      )
      outerTerms += 1
    }
    if (existentialHandles.nonEmpty) {
      if (outerTerms > 0) sink.append("\n  AND EXISTS (")
      else sink.append("  EXISTS (")
      sink.append("\n    SELECT 1\n    FROM ")
      val existentialAliases = existentialHandles.map(extremumTerminalAlias)
      existentialHandles.zip(existentialAliases).zipWithIndex.foreach {
        case ((handle, alias), index) =>
          if (index > 0) sink.append("\n      CROSS JOIN ")
          emitExtremumHandleSource(
            compiled,
            semijoinPlan,
            handle,
            alias,
            "    ",
            sink
          )
      }

      val allHandles = guardHandle +: existentialHandles
      val allAliases = guardAlias +: existentialAliases
      val basePredicates = existentialHandles.zip(existentialAliases).flatMap {
        case (handle, alias) =>
          handle.source match {
            case PostgreSqlGuardedExtremumSemijoin.HandleSource.Base(edgeId) =>
              extremumBasePredicates(compiled, edgeId).map(_ -> alias)
            case _: PostgreSqlGuardedExtremumSemijoin.HandleSource.Stage =>
              Vector.empty
          }
      }
      if (semijoinPlan.terminalEqualities.nonEmpty || basePredicates.nonEmpty) {
        sink.append("\n    WHERE\n")
        var witnessTerms = 0
        semijoinPlan.terminalEqualities.foreach { equality =>
          if (witnessTerms > 0) sink.append("\n      AND ")
          else sink.append("      ")
          emitExtremumEquality(
            compiled,
            equality,
            allHandles,
            allAliases,
            sink
          )
          witnessTerms += 1
        }
        basePredicates.foreach { case (predicate, alias) =>
          if (witnessTerms > 0) sink.append("\n      AND ")
          else sink.append("      ")
          emitScalar(
            compiled,
            predicate.scalar,
            ref => emitSourceColumn(compiled, alias, ref.id, sink),
            sink
          )
          witnessTerms += 1
        }
      }
      sink.append("\n  )")
    }
  }

  /**
    * Full two-pass reduction proves that every surviving owner-factor row has
    * a complete witness, so MIN/MAX can read the exact carried guard occurrence
    * without reconstructing or rescanning the source relations.
    */
  private def emitFullyReducedGuardFactorExtremum(
      compiled: CompiledPostgreSqlRendering,
      functionName: String,
      argument: CompiledPostgreSqlScalar.Column,
      output: PostgreSqlOutputColumn,
      aggregateBagId: BagId,
      sink: SqlSink
  ): Unit = {
    val owner = compiled.ownerByCarriedColumn.getOrElse(
      argument.ref.id,
      throw new IllegalArgumentException(
        s"Guarded MIN/MAX argument ${argument.ref.id} is not carried"
      )
    )
    if (owner != aggregateBagId) {
      throw new IllegalArgumentException(
        s"Guarded MIN/MAX argument owner ${owner.value} differs from aggregate bag ${aggregateBagId.value}"
      )
    }
    val finalCte = compiled.trace.bags
      .find(_.bagId == aggregateBagId)
      .map(_.finalFactorCte)
      .getOrElse(
        throw new IllegalArgumentException(
          s"Missing final guarded MIN/MAX factor for bag ${aggregateBagId.value}"
        )
      )

    sink.append("SELECT\n  ")
    sink.append(functionName)
    sink.append("(")
    emitFinalColumn(compiled, argument.ref, sink)
    sink.append(") AS ")
    quoted(output.name, sink)
    sink.append("\nFROM ")
    quoted(finalCte, sink)
    sink.append(" AS ")
    quoted(PostgreSqlNames.finalFactor(aggregateBagId), sink)
  }

  private val ExtremumPresenceColumn = "yr_present"

  /**
    * Emits one guarded-extremum handle. Staged handles reference an earlier
    * terminal-owned MATERIALIZED CTE so PostgreSQL cannot parameterize and
    * repeat the DISTINCT support for outer guard rows.
    */
  private def emitExtremumHandleSource(
      compiled: CompiledPostgreSqlRendering,
      plan: PostgreSqlGuardedExtremumSemijoin.Plan,
      handle: PostgreSqlGuardedExtremumSemijoin.Handle,
      alias: String,
      indent: String,
      sink: SqlSink
  ): Unit = handle.source match {
    case PostgreSqlGuardedExtremumSemijoin.HandleSource.Base(edgeId) =>
      val edge = compiled.edgeById.getOrElse(
        edgeId,
        throw new IllegalArgumentException(
          s"Missing guarded-extremum base edge ${edgeId.value}"
        )
      )
      emitRelation(compiled.relationByOccurrence(edge.relationOccurrenceId), sink)
      sink.append(" AS ")
      quoted(alias, sink)
    case PostgreSqlGuardedExtremumSemijoin.HandleSource.Stage(ordinal) =>
      val stage = plan.stages.lift(ordinal).getOrElse(
        throw new IllegalArgumentException(
          s"Missing guarded-extremum stage $ordinal"
        )
      )
      if (stage.output != handle) {
        throw new IllegalArgumentException(
          s"Guarded-extremum stage $ordinal output provenance differs"
        )
      }
      quoted(PostgreSqlNames.extremumStage(ordinal), sink)
      sink.append(" AS ")
      quoted(alias, sink)
  }

  /** Emits one terminal-internal support exactly once per statement. */
  private def emitExtremumStageCte(
      compiled: CompiledPostgreSqlRendering,
      plan: PostgreSqlGuardedExtremumSemijoin.Plan,
      stage: PostgreSqlGuardedExtremumSemijoin.Stage,
      sink: SqlSink
  ): Unit = {
    sink.append("  ")
    quoted(PostgreSqlNames.extremumStage(stage.ordinal), sink)
    sink.append(" AS MATERIALIZED (\n")
    emitExtremumStage(compiled, plan, stage, "    ", sink)
    sink.append("\n  )")
  }

  private def emitExtremumStage(
      compiled: CompiledPostgreSqlRendering,
      plan: PostgreSqlGuardedExtremumSemijoin.Plan,
      stage: PostgreSqlGuardedExtremumSemijoin.Stage,
      indent: String,
      sink: SqlSink
  ): Unit = {
    val anchorAlias = s"yr_g_s${stage.ordinal}_a"
    val existentialAliases = stage.existentialInputs.indices.map(index =>
      s"yr_g_s${stage.ordinal}_i$index"
    ).toVector

    sink.append(indent)
    sink.append("SELECT DISTINCT\n")
    if (stage.output.variables.nonEmpty) {
      stage.output.variables.zipWithIndex.foreach { case (variableId, index) =>
        sink.append(indent)
        sink.append("  ")
        emitExtremumHandleVariable(
          compiled,
          stage.anchorInput,
          anchorAlias,
          variableId,
          sink
        )
        sink.append(" AS ")
        quoted(PostgreSqlNames.variable(variableId), sink)
        if (index + 1 < stage.output.variables.size) sink.append(",")
        sink.append("\n")
      }
    } else {
      sink.append(indent)
      sink.append("  1 AS ")
      quoted(ExtremumPresenceColumn, sink)
      sink.append("\n")
    }
    sink.append(indent)
    sink.append("FROM ")
    emitExtremumHandleSource(
      compiled,
      plan,
      stage.anchorInput,
      anchorAlias,
      indent,
      sink
    )
    sink.append("\n")
    sink.append(indent)
    sink.append("WHERE\n")

    val anchorPredicates = stage.anchorInput.source match {
      case PostgreSqlGuardedExtremumSemijoin.HandleSource.Base(edgeId) =>
        extremumBasePredicates(compiled, edgeId)
      case _: PostgreSqlGuardedExtremumSemijoin.HandleSource.Stage =>
        Vector.empty
    }
    var outerTerms = 0
    anchorPredicates.foreach { predicate =>
      sink.append(indent)
      if (outerTerms > 0) sink.append("  AND ") else sink.append("  ")
      emitScalar(
        compiled,
        predicate.scalar,
        ref => emitSourceColumn(compiled, anchorAlias, ref.id, sink),
        sink
      )
      sink.append("\n")
      outerTerms += 1
    }
    sink.append(indent)
    if (outerTerms > 0) sink.append("  AND EXISTS (\n")
    else sink.append("  EXISTS (\n")
    sink.append(indent)
    sink.append("    SELECT 1\n")
    sink.append(indent)
    sink.append("    FROM ")
    stage.existentialInputs.zip(existentialAliases).zipWithIndex.foreach {
      case ((input, alias), index) =>
        if (index > 0) {
          sink.append("\n")
          sink.append(indent)
          sink.append("      CROSS JOIN ")
        }
        emitExtremumHandleSource(
          compiled,
          plan,
          input,
          alias,
          indent + "    ",
          sink
        )
    }

    val allHandles = stage.anchorInput +: stage.existentialInputs
    val allAliases = anchorAlias +: existentialAliases
    val basePredicates = stage.existentialInputs.zip(existentialAliases).flatMap {
      case (handle, alias) =>
        handle.source match {
          case PostgreSqlGuardedExtremumSemijoin.HandleSource.Base(edgeId) =>
            extremumBasePredicates(compiled, edgeId).map(_ -> alias)
          case _: PostgreSqlGuardedExtremumSemijoin.HandleSource.Stage =>
            Vector.empty
        }
    }
    if (stage.equalities.nonEmpty || basePredicates.nonEmpty) {
      sink.append("\n")
      sink.append(indent)
      sink.append("    WHERE\n")
      var witnessTerms = 0
      stage.equalities.foreach { equality =>
        sink.append(indent)
        if (witnessTerms > 0) sink.append("      AND ")
        else sink.append("      ")
        emitExtremumEquality(
          compiled,
          equality,
          allHandles,
          allAliases,
          sink
        )
        sink.append("\n")
        witnessTerms += 1
      }
      basePredicates.foreach { case (predicate, alias) =>
        sink.append(indent)
        if (witnessTerms > 0) sink.append("      AND ")
        else sink.append("      ")
        emitScalar(
          compiled,
          predicate.scalar,
          ref => emitSourceColumn(compiled, alias, ref.id, sink),
          sink
        )
        sink.append("\n")
        witnessTerms += 1
      }
      // Remove the final predicate newline before the closing parenthesis only
      // logically; whitespace before ')' is canonical and harmless.
    } else {
      sink.append("\n")
    }
    sink.append(indent)
    sink.append("  )")
  }

  private def extremumBasePredicates(
      compiled: CompiledPostgreSqlRendering,
      edgeId: EdgeId
  ): Vector[CompiledPostgreSqlPredicate] = {
    val edge = compiled.edgeById.getOrElse(
      edgeId,
      throw new IllegalArgumentException(
        s"Missing guarded-extremum edge ${edgeId.value}"
      )
    )
    val local = compiled.localPredicatesByOccurrence
      .getOrElse(edge.relationOccurrenceId, Vector.empty)
    val sameOccurrenceEqualities = compiled.innerEqualities.filter { predicate =>
      predicate.scalar.columnRefs
        .map(_.relationOccurrenceId)
        .distinct
        .size == 1 &&
      predicate.scalar.columnRefs.headOption.exists(
        _.relationOccurrenceId == edge.relationOccurrenceId
      )
    }
    (local ++ sameOccurrenceEqualities).sortBy(_.ordinal)
  }

  private def emitExtremumEquality(
      compiled: CompiledPostgreSqlRendering,
      equality: PostgreSqlGuardedExtremumSemijoin.Equality,
      handles: Vector[PostgreSqlGuardedExtremumSemijoin.Handle],
      aliases: Vector[String],
      sink: SqlSink
  ): Unit = {
    emitExtremumHandleVariable(
      compiled,
      handles(equality.leftInput),
      aliases(equality.leftInput),
      equality.variableId,
      sink
    )
    sink.append(" = ")
    emitExtremumHandleVariable(
      compiled,
      handles(equality.rightInput),
      aliases(equality.rightInput),
      equality.variableId,
      sink
    )
  }

  private def emitExtremumHandleVariable(
      compiled: CompiledPostgreSqlRendering,
      handle: PostgreSqlGuardedExtremumSemijoin.Handle,
      alias: String,
      variableId: at.ac.tuwien.dbai.yannakakis.core.VariableId,
      sink: SqlSink
  ): Unit = handle.source match {
    case PostgreSqlGuardedExtremumSemijoin.HandleSource.Base(edgeId) =>
      val edge = compiled.edgeById.getOrElse(
        edgeId,
        throw new IllegalArgumentException(
          s"Missing guarded-extremum edge ${edgeId.value}"
        )
      )
      val columnId = edge.bindings.collectFirst {
        case (candidate, `variableId`) => candidate
      }.getOrElse(
        throw new IllegalArgumentException(
          s"Edge ${edgeId.value} does not carry variable ${variableId.value}"
        )
      )
      emitSourceColumn(compiled, alias, columnId, sink)
    case _: PostgreSqlGuardedExtremumSemijoin.HandleSource.Stage =>
      qualifiedColumn(alias, PostgreSqlNames.variable(variableId), sink)
  }

  private def extremumTerminalAlias(
      handle: PostgreSqlGuardedExtremumSemijoin.Handle
  ): String = handle.source match {
    case PostgreSqlGuardedExtremumSemijoin.HandleSource.Base(edgeId) =>
      PostgreSqlNames.extremumScan(edgeId)
    case PostgreSqlGuardedExtremumSemijoin.HandleSource.Stage(ordinal) =>
      s"yr_g_h$ordinal"
  }

  private def edgeForHandle(
      compiled: CompiledPostgreSqlRendering,
      handle: PostgreSqlGuardedExtremumSemijoin.Handle
  ) = compiled.edgeById.getOrElse(
    handle.anchorEdgeId,
    throw new IllegalArgumentException(
      s"Missing guarded-extremum anchor edge ${handle.anchorEdgeId.value}"
    )
  )

  private def emitFinalColumn(
      compiled: CompiledPostgreSqlRendering,
      ref: ColumnRef,
      sink: SqlSink
  ): Unit = {
    val edge = compiled.edgeByOccurrence(ref.relationOccurrenceId)
    val owner = compiled.ownerByCarriedColumn(ref.id)
    qualifiedColumn(
      PostgreSqlNames.finalFactor(owner),
      PostgreSqlNames.occurrenceColumn(edge.id, ref.id),
      sink
    )
  }

  private def emitScalar(
      compiled: CompiledPostgreSqlRendering,
      scalar: CompiledPostgreSqlScalar,
      emitColumn: ColumnRef => Unit,
      sink: SqlSink
  ): Unit = scalar match {
    case CompiledPostgreSqlScalar.LocalPredicate(tree) =>
      emitLocalPredicate(tree, emitColumn, sink)
    case CompiledPostgreSqlScalar.Column(ref) => emitColumn(ref)
    case CompiledPostgreSqlScalar.Equality(left, right) =>
      sink.append("(")
      emitColumn(left)
      sink.append(" = ")
      emitColumn(right)
      sink.append(")")
    case equality: CompiledPostgreSqlScalar.LocalIntegralEquality =>
      sink.append("(")
      if (equality.literalOnLeft) {
        emitIntegralLiteral(equality, sink)
        sink.append(" = ")
        emitIntegralColumn(compiled, equality, emitColumn, sink)
      } else {
        emitIntegralColumn(compiled, equality, emitColumn, sink)
        sink.append(" = ")
        emitIntegralLiteral(equality, sink)
      }
      sink.append(")")
  }

  /** Emit the closed predicate tree, never a diagnostic digest or source SQL. */
  private def emitLocalPredicate(
      tree: at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteLocalPredicates.Tree[ColumnRef],
      emitColumn: ColumnRef => Unit,
      sink: SqlSink
  ): Unit = {
    import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteLocalPredicates._
    def quoted(value: String): Unit = {
      sink.append("'")
      sink.append(value.replace("'", "''"))
      sink.append("'")
    }
    def term(value: Term[ColumnRef]): Unit = value match {
      case Column(ref) => emitColumn(ref)
      case Integral(number) => sink.append(number.toString)
      case Text(text) => quoted(text)
      case Timestamp(text) => sink.append("TIMESTAMP "); quoted(text)
    }
    sink.append("(")
    tree match {
      case Comparison(operator, left, right) =>
        term(left); sink.append(" "); sink.append(operator.sql); sink.append(" "); term(right)
      case NullTest(column, not) =>
        emitColumn(column); sink.append(if (not) " IS NOT NULL" else " IS NULL")
      case BooleanColumn(column) => emitColumn(column)
      case Junction(and, terms) =>
        terms.zipWithIndex.foreach { case (child, index) =>
          if (index > 0) sink.append(if (and) " AND " else " OR ")
          emitLocalPredicate(child, emitColumn, sink)
        }
    }
    sink.append(")")
  }

  private def emitIntegralColumn(
      compiled: CompiledPostgreSqlRendering,
      equality: CompiledPostgreSqlScalar.LocalIntegralEquality,
      emitColumn: ColumnRef => Unit,
      sink: SqlSink
  ): Unit = {
    val widened =
      equality.columnSourceSqlTypeName != equality.columnTargetSqlTypeName
    if (
      widened &&
      compiled.enumerationRenderingProfile.castIntegralSourceColumn
    ) {
      sink.append("CAST(")
      emitColumn(equality.column)
      sink.append(" AS ")
      emitIntegralSqlType(equality.columnTargetSqlTypeName, sink)
      sink.append(")")
    } else emitColumn(equality.column)
  }

  private def emitIntegralLiteral(
      equality: CompiledPostgreSqlScalar.LocalIntegralEquality,
      sink: SqlSink
  ): Unit = {
    val widened =
      equality.literalSourceSqlTypeName != equality.literalTargetSqlTypeName
    if (widened) sink.append("CAST(")
    sink.append("CAST(")
    sink.append(equality.literalValue.toString)
    sink.append(" AS ")
    emitIntegralSqlType(equality.literalSourceSqlTypeName, sink)
    sink.append(")")
    if (widened) {
      sink.append(" AS ")
      emitIntegralSqlType(equality.literalTargetSqlTypeName, sink)
      sink.append(")")
    }
  }

  private def emitIntegralSqlType(
      sqlTypeName: String,
      sink: SqlSink
  ): Unit = sqlTypeName match {
    case "SMALLINT" => sink.append("SMALLINT")
    case "INTEGER"  => sink.append("INTEGER")
    case "BIGINT"   => sink.append("BIGINT")
    case other =>
      throw new IllegalArgumentException(
        s"Unsupported compiled integral SQL type '$other'"
      )
  }

  private def emitRelation(
      relation: RelationOccurrence,
      sink: SqlSink
  ): Unit = {
    relation.qualifiedName.zipWithIndex.foreach { case (component, index) =>
      if (index > 0) sink.append(".")
      quoted(component, sink)
    }
  }

  private def emitSourceColumn(
      compiled: CompiledPostgreSqlRendering,
      alias: String,
      columnId: ColumnId,
      sink: SqlSink
  ): Unit =
    qualifiedColumn(
      alias,
      compiled.metadataByColumn(columnId).sourceName,
      sink
    )

  private def qualifiedColumn(
      qualifier: String,
      column: String,
      sink: SqlSink
  ): Unit = {
    quoted(qualifier, sink)
    sink.append(".")
    quoted(column, sink)
  }

  private def quoted(value: String, sink: SqlSink): Unit = {
    sink.append("\"")
    var start = 0
    var index = value.indexOf('"')
    while (index >= 0) {
      sink.append(value.substring(start, index))
      sink.append("\"\"")
      start = index + 1
      index = value.indexOf('"', start)
    }
    sink.append(value.substring(start))
    sink.append("\"")
  }

  private sealed trait SqlSink {
    def append(value: String): Unit
    def physical: PostgreSqlPhysicalEmissionPlan
  }

  private final class CountingSink(val physical: PostgreSqlPhysicalEmissionPlan = PostgreSqlPhysicalEmissionPlan()) extends SqlSink {
    private var count = BigInt(0)
    override def append(value: String): Unit = count += value.length
    def length: BigInt = count
  }

  private final class MaterializingSink(capacity: Int,
      val physical: PostgreSqlPhysicalEmissionPlan = PostgreSqlPhysicalEmissionPlan()) extends SqlSink {
    private val builder = new java.lang.StringBuilder(capacity)
    override def append(value: String): Unit = builder.append(value)
    def result(): String = builder.toString
  }
}
