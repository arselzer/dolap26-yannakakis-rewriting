package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{BagFactorPlan, BagFactorPlanningLimits}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{ColumnRef, QueryIR}

/** Optional grouped successor to the scalar/U1 branch-count realizations.
  * Only caller-admitted structural grouping is accepted; no SQL parsing, FK
  * elimination, new language, statistics access or default selection occurs.
  * Conditional independence and key routing use the shared factor-star proof.
  */
private[yannakakis] object PostgreSqlGroupedBranchCountRenderer {
  val ProfileId = "grouped-progressive-branch-count-v1"
  val MaximumCharacters = 1024 * 1024
  // These physical choices have exactly the same meaning in the scalar lane.
  import PostgreSqlScalarBranchCountRenderer.{Strategy => CountStrategy}
  final case class Options(strategy: CountStrategy, anchor: Option[BagId] = None,
      branchOrder: Vector[BagId] = Vector.empty, maximumCharacters: Int = MaximumCharacters)
  final case class Result(sql: String, actionId: String, anchor: BagId,
      branchOrder: Vector[BagId], output: Vector[PostgreSqlOutputColumn])
  final case class Candidate(name: String, rendered: Result)
  private case class Terminal(keys: Vector[VariableId], refs: Vector[ColumnRef], groups: Vector[PostgreSqlOutputColumn],
      count: PostgreSqlOutputColumn)

  private def decline(message: String) = Left(Vector(
    PostgreSqlRenderingError.ModelInvariantViolation(ProfileId + ": " + message)))

  final class Prepared private[PostgreSqlGroupedBranchCountRenderer] (
      private val compiled: CompiledPostgreSqlRendering, private val terminal: Terminal,
      private val shapes: Vector[PostgreSqlFactorStar.GroupedShape]) {
    val anchor: BagId = shapes.head.star.anchor
    val branchOrder: Vector[BagId] = compiled.bags.map(_.factor.bagId).filterNot(_ == anchor).sortBy(_.value)

    def render(options: Options): Either[Vector[PostgreSqlRenderingError], Result] = {
      if (options == null || options.strategy == null || options.anchor == null ||
          options.anchor.exists(_ == null) || options.branchOrder == null || options.branchOrder.exists(_ == null))
        return decline("non-null physical options required")
      if (options.maximumCharacters <= 0 || options.maximumCharacters > MaximumCharacters)
        return decline("SQL-character limit must be within 1..1048576")
      shapes.find(s => options.anchor.forall(_ == s.star.anchor)) match {
        case None => decline("no certified grouped factor-star for the requested anchor")
        case Some(shape) =>
          val leaves = compiled.bags.map(_.factor.bagId).filterNot(_ == shape.star.anchor).sortBy(_.value)
          val order = if (options.branchOrder.isEmpty) leaves else options.branchOrder
          if (order.size != leaves.size || order.distinct.size != order.size || order.toSet != leaves.toSet)
            return decline("branch order must contain every non-anchor factor exactly once")
          // Group-producing leaves follow plain leaves; their relative order is explicit.
          val effective = order.filterNot(shape.outputLeaves.contains) ++ order.filter(shape.outputLeaves.contains)
          emit(compiled, terminal, shape, effective, options) match {
            case None => decline("SQL exceeds the physical profile's character limit")
            case Some(sql) =>
              val identity = Vector(ProfileId, options.strategy.id, shape.star.anchor.value.toString,
                effective.map(_.value).mkString(","), sql).mkString("\n")
              val digest = MessageDigest.getInstance("SHA-256").digest(identity.getBytes(UTF_8))
                .map(b => f"${b & 0xff}%02x").mkString
              Right(Result(sql, "sha256:" + digest, shape.star.anchor, effective, compiled.output))
          }
      }
    }

    /** Fixed aliases; identical SQL may alias, never become extra evidence. */
    def candidates: Either[Vector[PostgreSqlRenderingError], Vector[Candidate]] = {
      val configs = for (strategy <- Vector(CountStrategy.PerKey, CountStrategy.RestrictedGroup);
          (name, order) <- Vector("forward" -> branchOrder, "reverse" -> branchOrder.reverse))
        yield strategy.id + "-" + name -> Options(strategy, Some(anchor), order)
      configs.foldLeft[Either[Vector[PostgreSqlRenderingError], Vector[Candidate]]](Right(Vector.empty)) {
        case (acc, (name, options)) => acc.flatMap(xs => render(options).map(r => xs :+ Candidate(name, r)))
      }
    }
  }

  def render(query: QueryIR, input: CandidateSearchInput, plan: BagFactorPlan, options: Options)
      : Either[Vector[PostgreSqlRenderingError], Result] = prepare(query, input, plan).flatMap(_.render(options))

  def prepare(query: QueryIR, input: CandidateSearchInput, plan: BagFactorPlan)
      : Either[Vector[PostgreSqlRenderingError], Prepared] = {
    if (query == null || input == null || plan == null) return decline("non-null query, input and plan required")
    if (input.widthBound != 1 || plan.sourceWidthBound != 1 || plan.factors.size > PostgreSqlFactorStar.MaximumFactors)
      return decline("requires a bounded width-one plan")
    PostgreSqlBagFactorRenderer.compileValidatedWithEnumerationProfile(query, input, plan,
      PostgreSqlRenderingLimits.default, BagFactorPlanningLimits.default,
      YannakakisReductionMode.FullTwoPass, PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2).flatMap { c =>
      val terminal = c.terminal match {
        case t: CompiledPostgreSqlTerminal.GroupedCountStar =>
          Some(Terminal(Vector(t.groupVariable), Vector(t.groupColumn.ref), Vector(t.groupOutput), t.countOutput))
        case t: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
          Some(Terminal(t.groupVariables, t.groupColumns.map(_.ref), t.groupOutputs, t.countOutput))
        case _ => None
      }
      terminal match {
        case None => decline("requires already admitted structural grouped COUNT(*)")
        case Some(t) =>
          if (c.bags.exists(_.supportStrategy == PostgreSqlBagSupportStrategy.MaterializedDistinctSupport))
            decline("support-bearing factors are outside this profile")
          else {
            val shapes = PostgreSqlFactorStar.certifyGrouping(c.bags.map(b => b.factor.bagId -> b.factor.vertices), t.keys)
            if (shapes.isEmpty) decline("no certified factor-star with live grouping keys")
            else {
              // Equality preserves values, not JDBC origin/nullability metadata.
              // Keep each selected source column as its variable's representative
              // in its unique owner factor, and route the output from that owner.
              // The original owned equalities still enforce the same binding class.
              val bindings = t.keys.zip(t.refs).map { case (key, ref) =>
                PostgreSqlBindingLocation(c.edgeByOccurrence(ref.relationOccurrenceId).id, ref.id, key)
              }
              val providers = bindings.map { binding =>
                c.bags.find(_.factor.ownedEdgeIds.contains(binding.edgeId)).get.factor.bagId
              }
              val bags = c.bags.map { bag =>
                val preferred = bindings.filter(b => bag.factor.ownedEdgeIds.contains(b.edgeId))
                bag.copy(ownedRepresentatives = bag.ownedRepresentatives ++ preferred.map(b => b.variableId -> b))
              }
              val routed = shapes.map(_.copy(providers = providers))
              Right(new Prepared(c.copy(bags = bags,
                cteMaterialization = PostgreSqlCteMaterializationDirective.NotMaterialized), t, routed))
            }
          }
      }
    }
  }

  private def emit(c: CompiledPostgreSqlRendering, terminal: Terminal,
      shape: PostgreSqlFactorStar.GroupedShape, order: Vector[BagId], options: Options): Option[String] = {
    val definitions = scala.collection.mutable.ArrayBuffer.empty[String]
    var characters = 5 // WITH and newline
    def add(sql: String): Boolean = {
      val next = characters.toLong + sql.length + 2
      if (next > options.maximumCharacters) false
      else { definitions += sql; characters = next.toInt; true }
    }
    c.bags.sortBy(_.factor.bagId.value).foreach { bag =>
      PostgreSqlEmitter.initialFactorDefinition(c, bag, options.maximumCharacters - characters) match {
        case Some(sql) if add(sql) => ()
        case _ => return None
      }
    }
    def raw(id: BagId) = quote(PostgreSqlNames.initialFactor(id))
    def cols(alias: String, keys: Vector[VariableId]) = keys.map(col(alias, _)).mkString(", ")
    def eqs(left: String, right: String, keys: Vector[VariableId]) =
      keys.map(v => col(left, v) + " = " + col(right, v)).mkString(" AND ")
    val unit = "SUM(CAST(1 AS NUMERIC))"
    var seed = quote("yr_gc_anchor")
    if (!add(seed + " AS MATERIALIZED (SELECT " + cols("r", shape.anchorKeys) + ", " + unit +
        " AS yr_freq FROM " + raw(shape.star.anchor) + " AS r WHERE " +
        shape.star.shared.map(v => col("r", v) + " IS NOT NULL").mkString(" AND ") +
        " GROUP BY " + cols("r", shape.anchorKeys) + ")")) return None

    // Each returned source contributes exactly one factor's weight. A subset
    // boundary uses a DISTINCT domain so extra anchor keys never multiply counts.
    def restricted(id: BagId, keys: Vector[VariableId]): Option[String] = {
      val boundary = shape.star.boundaries(id)
      def perKey(domain: String): String = {
        val selected = if (keys.isEmpty) "" else cols("r", keys) + ", "
        val group = if (keys.isEmpty) "" else " GROUP BY " + cols("r", keys)
        "SELECT " + selected + unit + " AS yr_freq FROM " + raw(id) + " AS r WHERE " +
          eqs("r", domain, boundary) + group
      }
      val alias = s"b${id.value}"
      if (options.strategy == CountStrategy.PerKey && boundary == shape.anchorKeys) {
        // This seed is unique on the complete boundary. Output-group branches
        // use the same indexed correlated join as plain scalar branches.
        Some(" CROSS JOIN LATERAL (" + perKey("a") + ") AS " + alias)
      } else {
        val counted = quote(s"yr_gc_count_${id.value}")
        val groupedKeys = (boundary ++ keys).distinct.sorted
        val body = options.strategy match {
          case CountStrategy.PerKey =>
            "SELECT z.* FROM (SELECT DISTINCT " + boundary.map(v => quote(PostgreSqlNames.variable(v))).mkString(", ") +
              " FROM " + seed + ") AS d CROSS JOIN LATERAL (" + perKey("d") + ") AS z"
          case CountStrategy.RestrictedGroup =>
            "SELECT " + cols("r", groupedKeys) + ", " + unit + " AS yr_freq FROM " + raw(id) +
              " AS r WHERE EXISTS (SELECT 1 FROM " + seed + " AS d WHERE " + eqs("r", "d", boundary) +
              ") GROUP BY " + cols("r", groupedKeys)
        }
        if (!add(counted + " AS MATERIALIZED (" + body + ")")) None
        else Some(" JOIN " + counted + " AS " + alias + " ON " + eqs("a", alias, boundary))
      }
    }

    val plain = order.filterNot(shape.outputLeaves.contains)
    plain.foreach { id =>
      // Boundary columns are needed when a counted state is rejoined, but a
      // direct scalar LATERAL needs only its frequency.
      val keys = if (options.strategy == CountStrategy.PerKey && shape.star.boundaries(id) == shape.anchorKeys)
        Vector.empty else shape.star.boundaries(id)
      val source = restricted(id, keys).getOrElse(return None)
      val next = quote(s"yr_gc_step_${id.value}")
      if (!add(next + " AS MATERIALIZED (SELECT " + cols("a", shape.anchorKeys) +
          s", a.yr_freq * b${id.value}.yr_freq AS yr_freq FROM " + seed + " AS a" + source +
          s" WHERE b${id.value}.yr_freq IS NOT NULL)")) return None
      seed = next
    }
    val outputLeaves = order.filter(shape.outputLeaves.contains)
    val joins = Vector.newBuilder[String]
    outputLeaves.foreach { id =>
      joins += restricted(id, shape.keysFor(id)).getOrElse(return None)
    }
    val outputs = terminal.keys.zip(shape.providers).map { case (v, id) =>
      col(if (id == shape.star.anchor) "a" else s"b${id.value}", v)
    }
    val weights = Vector("a.yr_freq") ++ outputLeaves.map(id => s"b${id.value}.yr_freq")
    val suffix = "\nSELECT " + outputs.zip(terminal.groups).map { case (value, output) =>
      value + " AS " + quote(output.name)
    }.mkString(", ") + ", CAST(SUM(" + weights.mkString(" * ") + ") AS BIGINT) AS " +
      quote(terminal.count.name) + " FROM " + seed + " AS a" + joins.result().mkString +
      " GROUP BY " + outputs.mkString(", ") + ";"
    if (characters.toLong - 2 + suffix.length > options.maximumCharacters) None
    else Some("WITH\n" + definitions.mkString(",\n") + suffix)
  }

  private def quote(value: String) = "\"" + value.replace("\"", "\"\"") + "\""
  private def col(alias: String, v: VariableId) = alias + "." + quote(PostgreSqlNames.variable(v))
}
