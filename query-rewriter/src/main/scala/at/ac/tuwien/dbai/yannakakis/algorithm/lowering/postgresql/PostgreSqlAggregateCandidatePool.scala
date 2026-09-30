package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{BagFactorPlan, BagFactorPlanningLimits}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{QueryIR, QueryResultShape}

/** Optional action discovery, not a selector. The caller always retains native.
  * All alternatives reuse one analyzed query and one structural plan. Existing
  * renderers independently validate their physical boundaries; no catalog or
  * runtime probes, SQL parsing, FK assumptions or default changes occur here.
  */
private[yannakakis] object PostgreSqlAggregateCandidatePool {
  val ProfileId = "bounded-aggregate-candidate-pool-v1"
  val MaximumRewrites = 7 // three general profiles plus four specialized aliases
  final case class Action(name: String, profileId: String, rendererActionId: String,
      sql: String, aliasOf: Option[String])
  final case class Decline(name: String, errors: Vector[PostgreSqlRenderingError])
  final case class Pool(actions: Vector[Action], declines: Vector[Decline]) {
    def uniqueActions: Vector[Action] = actions.filter(_.aliasOf.isEmpty)
  }

  private[postgresql] val base = PostgreSqlAggregateRenderingProfileV2.create(
    PostgreSqlAggregateRenderingProfile.Values.find(p =>
      p.oneBagTerminal == PostgreSqlAggregateOneBagTerminalProfile.CollapsedDirectV2 &&
      p.reductionFinalization == PostgreSqlAggregateReductionFinalizationProfile.CompactLocalStateV2 &&
      p.reductionSupport == PostgreSqlReductionSupportProfile.DirectExistsV2).get,
    PostgreSqlFactorBoundaryProjectionProfile.DemandedStructuralColumnsV2,
    PostgreSqlAggregateLeafMessageProfile.ExactDirectFactorV2).toOption.get
  private val fused = PostgreSqlAggregateRenderingProfileV3.fusedRawFactorMessages(
    PostgreSqlAggregateRenderingProfileV2.baseline(PostgreSqlAggregateRenderingProfile.LegacyV1).toOption.get).toOption.get

  def generate(query: QueryIR, input: CandidateSearchInput, plan: BagFactorPlan)
      : Either[Vector[PostgreSqlRenderingError], Pool] = {
    def error(message: String) = Vector(PostgreSqlRenderingError.ModelInvariantViolation(ProfileId + ": " + message))
    if (query == null || input == null || plan == null) return Left(error("non-null query, input and plan required"))
    query.resultShape match {
      case _: QueryResultShape.GlobalCountStar | _: QueryResultShape.GroupedCountStar |
          _: QueryResultShape.DistributedGroupedCountStar => ()
      case _ => return Left(error("only already-admitted COUNT(*) terminals are in this pool"))
    }
    val actions = Vector.newBuilder[Action]
    val declines = Vector.newBuilder[Decline]
    val owners = scala.collection.mutable.Map.empty[String, String]
    def add(name: String, profile: String, id: String, sql: String): Unit = {
      val alias = owners.get(sql) // exact SQL, not a possibly colliding digest
      actions += Action(name, profile, id, sql, alias)
      if (alias.isEmpty) owners(sql) = name
    }
    def physical(name: String, partial: Boolean): Unit = {
      PostgreSqlPhysicalOptimizationCompiler.render(query, input, plan, base,
        PostgreSqlPhysicalOptimizationCompiler.Options(partialSupport = partial)) match {
        case Left(errors) => declines += Decline(name, errors)
        case Right(r) => add(name, if (partial) PostgreSqlPhysicalOptimizationCompiler.ProfileId else base.id, r.actionId, r.sql)
      }
    }
    physical("baseline-v2", partial = false)
    physical("partial-support", partial = true)
    // Failure of the specialized star renderer never suppresses general actions.
    if (plan.factors.size <= 1) declines += Decline("fused-v3", error("one-bag message program is not a distinct action"))
    else PostgreSqlBagFactorRenderer.compileAndRenderWithAggregateProfileV3(query, input, plan,
      PostgreSqlRenderingLimits.default, BagFactorPlanningLimits.default,
      YannakakisReductionMode.NoReduction, fused, PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2) match {
      case Left(errors) => declines += Decline("fused-v3", errors)
      case Right((_, r)) => add("fused-v3", fused.id, "sha256:" + digest(fused.id + "\n" + r.sql), r.sql)
    }
    query.resultShape match {
      case _: QueryResultShape.GlobalCountStar =>
        PostgreSqlScalarBranchCountRenderer.prepare(query, input, plan).flatMap(_.candidatesWithoutProbes) match {
          case Left(errors) => declines += Decline("scalar-branch-family", errors)
          case Right(candidates) => candidates.foreach { c =>
            add("scalar-" + c.name, PostgreSqlScalarBranchCountRenderer.ProfileId, c.rendered.actionId, c.rendered.sql)
          }
        }
      case _ =>
        PostgreSqlGroupedBranchCountRenderer.prepare(query, input, plan).flatMap(_.candidates) match {
          case Left(errors) => declines += Decline("grouped-branch-family", errors)
          case Right(candidates) => candidates.foreach { c =>
            add("grouped-" + c.name, PostgreSqlGroupedBranchCountRenderer.ProfileId, c.rendered.actionId, c.rendered.sql)
          }
        }
    }
    Right(Pool(actions.result(), declines.result()))
  }

  private def digest(value: String): String = MessageDigest.getInstance("SHA-256")
    .digest(value.getBytes(UTF_8)).map(b => f"${b & 0xff}%02x").mkString
}
