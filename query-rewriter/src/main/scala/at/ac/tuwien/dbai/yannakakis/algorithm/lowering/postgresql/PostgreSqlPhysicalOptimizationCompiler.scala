package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{BagFactorPlan, BagFactorPlanningLimits}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.core.{BagId, EdgeId, Hyperedge, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryIR

/** Physical support only; the certified logical cover and ownership never change. */
private[postgresql] final case class PostgreSqlPartialSupportPlan(
    edges: Vector[Hyperedge],
    variables: Vector[VariableId],
    representatives: Map[VariableId, PostgreSqlBindingLocation],
    equalities: Vector[(PostgreSqlBindingLocation, PostgreSqlBindingLocation)],
    ownedRepresentatives: Map[VariableId, PostgreSqlBindingLocation],
    removedEdges: Vector[EdgeId]
)

private[postgresql] final case class PostgreSqlPhysicalEmissionPlan(
    partialSupports: Map[BagId, PostgreSqlPartialSupportPlan] = Map.empty,
    initialFactorDirectives: Map[BagId, PostgreSqlCteMaterializationDirective] = Map.empty
)

/** Additive development-only entry point; no existing public/default path calls it. */
private[yannakakis] object PostgreSqlPhysicalOptimizationCompiler {
  val ProfileId = "postgresql-support-and-initial-stage-optimization-v1"
  final case class Options(
      partialSupport: Boolean = false,
      initialFactorDirectives: Map[BagId, PostgreSqlCteMaterializationDirective] = Map.empty
  )
  final case class SupportChange(bagId: BagId, retainedEdges: Vector[EdgeId],
      removedEdges: Vector[EdgeId], supportVariables: Vector[VariableId])
  final case class Result(sql: String, actionId: String, output: Vector[PostgreSqlOutputColumn],
      supportChanges: Vector[SupportChange],
      initialFactorDirectives: Vector[(BagId, PostgreSqlCteMaterializationDirective)])

  def render(query: QueryIR, input: CandidateSearchInput, plan: BagFactorPlan,
      baseProfile: PostgreSqlAggregateRenderingProfileV2, options: Options,
      reductionMode: YannakakisReductionMode = YannakakisReductionMode.FullTwoPass
  ): Either[Vector[PostgreSqlRenderingError], Result] =
    renderChecked(query, input, plan, baseProfile, options, reductionMode, requireExactGroupSources = false)

  /** New candidate discovery must not inherit the old structural aggregate
    * renderer's substitution of an equivalent column with different origin or
    * nullability metadata. Decline without changing any old rendering bytes.
    */
  private[postgresql] def renderWithExactGroupSources(query: QueryIR, input: CandidateSearchInput,
      plan: BagFactorPlan, baseProfile: PostgreSqlAggregateRenderingProfileV2, options: Options
  ): Either[Vector[PostgreSqlRenderingError], Result] =
    renderChecked(query, input, plan, baseProfile, options, YannakakisReductionMode.FullTwoPass,
      requireExactGroupSources = true)

  private def renderChecked(query: QueryIR, input: CandidateSearchInput, plan: BagFactorPlan,
      baseProfile: PostgreSqlAggregateRenderingProfileV2, options: Options,
      reductionMode: YannakakisReductionMode, requireExactGroupSources: Boolean
  ): Either[Vector[PostgreSqlRenderingError], Result] = {
    def reject(message: String) = Left(Vector(PostgreSqlRenderingError.ModelInvariantViolation(message)))
    if (query == null || input == null || plan == null || baseProfile == null || options == null ||
        options.initialFactorDirectives == null || options.initialFactorDirectives.exists {
          case (bag, directive) => bag == null || directive == null
        }) return reject("physical optimization requires non-null validated inputs")
    if (PostgreSqlAggregateRenderingProfileV2.validate(baseProfile).nonEmpty)
      return reject("invalid aggregate base profile")
    val wrapped = PostgreSqlAggregateRenderingProfileV3.baseline(baseProfile).toOption.get
    PostgreSqlBagFactorRenderer.compileValidatedWithAggregateProfileV3(
      query, input, plan, PostgreSqlRenderingLimits.default, BagFactorPlanningLimits.default,
      reductionMode, wrapped, PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
    ).flatMap { compiled =>
      if (!options.initialFactorDirectives.keySet.subsetOf(compiled.bags.map(_.factor.bagId).toSet))
        reject("initial-factor directive refers to an absent bag")
      else {
        val partial = if (!options.partialSupport) Map.empty[BagId, PostgreSqlPartialSupportPlan]
          else compiled.bags.flatMap(b => derivePartialSupport(b).map(b.factor.bagId -> _)).toMap
        val physical = PostgreSqlPhysicalEmissionPlan(partial, options.initialFactorDirectives)
        if (requireExactGroupSources && !hasExactGroupSources(compiled, physical))
          return reject("selected grouping-column origin is not preserved by the existing physical aggregate plan")
        val count = PostgreSqlEmitter.physicalCharacterCount(compiled, physical)
        if (count > PostgreSqlRenderingLimits.default.maxSqlCharacters)
          reject("physical optimization SQL exceeds the default rendering bound")
        else {
          val sql = PostgreSqlEmitter.renderPhysical(compiled, count.toInt, physical)
          if (BigInt(sql.length) != count) reject("physical counting and emitting passes differ")
          else {
            val directives = options.initialFactorDirectives.toVector.sortBy(_._1.value)
            val changes = partial.toVector.sortBy(_._1.value).map { case (id, p) =>
              SupportChange(id, p.edges.map(_.id), p.removedEdges, p.variables)
            }
            val config = options.partialSupport.toString + "\n" + directives
              .map { case (id, directive) => s"${id.value}:${directive.id}" }.mkString("\n")
            Right(Result(sql, "sha256:" + sha(ProfileId + "\n" + config + "\n" + sql),
              compiled.output, changes, directives))
          }
        }
      }
    }
  }

  private def hasExactGroupSources(c: CompiledPostgreSqlRendering, physical: PostgreSqlPhysicalEmissionPlan): Boolean = {
    val groups = c.terminal match {
      case t: CompiledPostgreSqlTerminal.GroupedCountStar => Vector((t.groupColumn.ref.id, t.groupVariable, t.aggregateBagId))
      case t: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
        t.groupColumns.zip(t.groupVariables).map { case (column, variable) =>
          val owner = t.propagationPlan.keyBindings.find(_.key ==
            PostgreSqlAggregatePropagation.AggregateKey.Structural(variable)).get.owner
          (column.ref.id, variable, owner)
        }
      case _ => Vector.empty
    }
    groups.forall { case (expected, variable, owner) =>
      val bag = c.bags.find(_.factor.bagId == owner).get
      val partial = physical.partialSupports.get(owner)
      val binding = if (bag.supportStrategy == PostgreSqlBagSupportStrategy.ElidedByOwnedCover)
        bag.ownedRepresentatives(variable)
      else partial.flatMap(_.ownedRepresentatives.get(variable))
        .getOrElse(partial.fold(bag.supportRepresentatives)(_.representatives)(variable))
      binding.columnId == expected
    }
  }

  /** Called only after full source/plan/compiler validation. No name or statistics rules. */
  private[postgresql] def derivePartialSupport(bag: CompiledPostgreSqlBag): Option[PostgreSqlPartialSupportPlan] = {
    if (bag.supportStrategy != PostgreSqlBagSupportStrategy.MaterializedDistinctSupport) return None
    val owned = bag.ownedOccurrences.map(_.edge.id).toSet
    val (removed, retained) = bag.supportEdges.partition(e => owned(e.id))
    // Complete owned-cover elision stays with the existing implementation.
    if (removed.isEmpty || retained.isEmpty) return None
    val locations = retained.flatMap(e => e.bindings.map { case (c, v) => PostgreSqlBindingLocation(e.id, c, v) })
    val grouped = locations.groupBy(_.variableId).view.mapValues(_.sortBy(l => (l.edgeId.value, l.columnId))).toMap
    val variables = bag.factor.vertices.filter(grouped.contains)
    // The zero-column support case requires a separate existence/unit convention.
    if (variables.isEmpty) return None
    val ownedLocations = bag.ownedOccurrences.flatMap(o => o.edge.bindings.map {
      case (c, v) => PostgreSqlBindingLocation(o.edge.id, c, v)
    }).groupBy(_.variableId).view.mapValues(_.sortBy(l => (l.edgeId.value, l.columnId))).toMap
    val missing = bag.factor.vertices.filterNot(grouped.contains)
    if (!missing.forall(ownedLocations.contains)) return None
    Some(PostgreSqlPartialSupportPlan(retained, variables,
      variables.map(v => v -> grouped(v).head).toMap,
      grouped.toVector.sortBy(_._1).flatMap { case (_, ls) => ls.tail.map(ls.head -> _) },
      missing.map(v => v -> ownedLocations(v).head).toMap, removed.map(_.id)))
  }

  private def sha(value: String): String = MessageDigest.getInstance("SHA-256")
    .digest(value.getBytes(StandardCharsets.UTF_8)).map(b => f"${b & 0xff}%02x").mkString
}
