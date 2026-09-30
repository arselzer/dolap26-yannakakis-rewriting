package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlan
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryIR

/** Additive discovery, not an automatic winner/default or a change to pool v1.
  * A small deterministic structural budget bounds inlining alternatives. Every
  * candidate changes just one nontrivial initial join; reductions, support and
  * frequency stages retain their original directives. No outcome-based rule.
  */
private[yannakakis] object PostgreSqlAggregateCandidatePoolV2 {
  import PostgreSqlAggregateCandidatePool.{Action, Decline, Pool}
  val ProfileId = "bounded-aggregate-candidate-pool-v2"
  val MaximumInlineAlternatives = 4
  val MaximumRewrites = PostgreSqlAggregateCandidatePool.MaximumRewrites + MaximumInlineAlternatives + 1

  def generate(query: QueryIR, input: CandidateSearchInput, plan: BagFactorPlan)
      : Either[Vector[PostgreSqlRenderingError], Pool] = {
    PostgreSqlAggregateCandidatePool.generate(query, input, plan).map { previous =>
      val actions = scala.collection.mutable.ArrayBuffer.from(previous.actions)
      val declines = scala.collection.mutable.ArrayBuffer.from(previous.declines)
      val owners = scala.collection.mutable.Map.from(previous.uniqueActions.map(a => a.sql -> a.name))
      def add(name: String, profile: String, actionId: String, sql: String): Unit = {
        val alias = owners.get(sql)
        actions += Action(name, profile, actionId, sql, alias)
        if (alias.isEmpty) owners(sql) = name
      }
      // Baseline admission also prevents inspecting malformed plan internals.
      if (previous.actions.exists(_.name == "baseline-v2")) {
        if (plan.factors.size > 1) {
          val bags = plan.factors.filter(_.ownedEdgeIds.size > 1)
            .sortBy(f => (-f.ownedEdgeIds.size, f.bagId.value)).take(MaximumInlineAlternatives)
          bags.foreach { bag =>
            val name = s"partial-inline-b${bag.bagId.value}"
            PostgreSqlPhysicalOptimizationCompiler.renderWithExactGroupSources(query, input, plan, PostgreSqlAggregateCandidatePool.base,
              PostgreSqlPhysicalOptimizationCompiler.Options(partialSupport = true,
                initialFactorDirectives = Map(bag.bagId -> PostgreSqlCteMaterializationDirective.NotMaterialized))) match {
              case Left(errors) => declines += Decline(name, errors)
              case Right(r) => add(name, PostgreSqlPhysicalOptimizationCompiler.ProfileId, r.actionId, r.sql)
            }
          }
        }
        PostgreSqlCyclicCoreCountRenderer.render(query, input, plan) match {
          case Left(errors) => declines += Decline("cyclic-core-count", errors)
          case Right(r) => add("cyclic-core-count", PostgreSqlCyclicCoreCountRenderer.ProfileId, r.actionId, r.sql)
        }
      }
      Pool(actions.toVector, declines.toVector)
    }
  }
}
