package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

/**
  * Test-only structural oracle for the rendered terms introduced by an
  * aggregate-propagation program.
  *
  * This deliberately walks bags, messages, bindings, and finalization. It
  * never consumes the resource summary stored on the production plan, and it
  * never calls the SQL emitter or compiler resource calculator. A production
  * accounting defect therefore cannot make both sides of a boundary test
  * agree through a shared cached total.
  */
private[postgresql] object PostgreSqlAggregateStructuralResourceCounter {
  final case class Counts(
      localAggregateTerms: BigInt,
      localKeyProjectionTerms: BigInt,
      localKeyGroupingTerms: BigInt,
      localLaneConversionTerms: BigInt,
      propagatedMessageAggregateTerms: BigInt,
      propagatedMessageProjectionTerms: BigInt,
      propagatedSchemaProjectionTerms: BigInt,
      propagatedSchemaGroupingTerms: BigInt,
      propagatedJoinEqualityTerms: BigInt,
      terminalGroupingTerms: BigInt
  ) {
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

  def count(
      plan: PostgreSqlAggregatePropagation.Plan
  ): Counts = {
    require(plan != null, "aggregate propagation plan must not be null")

    val localGroupingTerms = plan.bags.iterator.foldLeft(BigInt(0)) {
      (total, bag) =>
        val structuralKeys = bag.ownedKeys.collect {
          case PostgreSqlAggregatePropagation.AggregateKey.Structural(variable) =>
            variable
        }
        val structuralGrouping =
          (bag.separatorGrouping.iterator ++ structuralKeys).toVector.distinct
        val occurrenceValueKeys = bag.ownedKeys.count {
          case _: PostgreSqlAggregatePropagation.AggregateKey.OccurrenceValue =>
            true
          case _ => false
        }
        total + structuralGrouping.size + occurrenceValueKeys
    }
    val schemaTerms = plan.messages.iterator.foldLeft(BigInt(0)) {
      (total, message) =>
        total + message.separator.size + message.carriedKeys.size
    }
    val regroupedSchemaTerms = plan.messages.iterator
      .filter(_.requiresRegrouping)
      .foldLeft(BigInt(0)) { (total, message) =>
        total + message.separator.size + message.carriedKeys.size
      }

    Counts(
      localAggregateTerms = BigInt(plan.bags.size),
      localKeyProjectionTerms = localGroupingTerms,
      localKeyGroupingTerms = localGroupingTerms,
      localLaneConversionTerms = BigInt(plan.laneBindings.size),
      propagatedMessageAggregateTerms =
        BigInt(plan.messages.count(_.requiresRegrouping)),
      propagatedMessageProjectionTerms =
        BigInt(plan.messages.count(message => !message.requiresRegrouping)),
      propagatedSchemaProjectionTerms = schemaTerms,
      propagatedSchemaGroupingTerms = regroupedSchemaTerms,
      propagatedJoinEqualityTerms = plan.messages.iterator.foldLeft(BigInt(0)) {
        (total, message) => total + message.separator.size
      },
      terminalGroupingTerms = BigInt(plan.finalization.groupKeys.size)
    )
  }
}
