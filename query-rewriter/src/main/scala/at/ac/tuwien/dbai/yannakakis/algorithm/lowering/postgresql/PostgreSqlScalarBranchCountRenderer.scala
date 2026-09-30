package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{BagFactorPlan, BagFactorPlanningLimits}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryIR

/** Opt-in scalar COUNT physical alternatives over a validated width-one star.
  * No source-SQL parsing, FK assumption, catalog access, or default selection.
  * Raw numeric intermediates retain the development carrier premise of V3.
  */
private[yannakakis] object PostgreSqlScalarBranchCountRenderer {
  val ProfileId = "scalar-progressive-branch-count-v1"
  val TerminalCollapsedProfileId = "scalar-progressive-branch-count-terminal-v2"
  val MaximumCharacters = 1024 * 1024

  sealed trait Strategy { def id: String }
  object Strategy {
    case object PerKey extends Strategy { val id = "per-key" }
    case object RestrictedGroup extends Strategy { val id = "restricted-group" }
  }
  final case class Options(
      strategy: Strategy,
      anchor: Option[BagId] = None,
      branchOrder: Vector[BagId] = Vector.empty,
      maximumCharacters: Int = MaximumCharacters
  )
  final case class Result private[postgresql] (
      sql: String, actionId: String, anchor: BagId, branchOrder: Vector[BagId],
      strategy: Strategy, output: Vector[PostgreSqlOutputColumn]
  )
  final case class Probe(bagId: BagId, kind: String, sql: String)
  final case class BoundaryColumn(relation: Vector[String], column: String)
  final case class Branch(bagId: BagId, boundarySize: Int, sharedSize: Int,
      ownedOccurrences: Int, boundaryColumns: Vector[BoundaryColumn])
  final case class Candidate(name: String, rendered: Result, stages: Vector[Probe])

  private def decline(message: String) = Left(Vector(
    PostgreSqlRenderingError.ModelInvariantViolation(ProfileId + ": " + message)))

  /** Query-bound compiled context; only prepare may construct one. */
  final class Prepared private[PostgreSqlScalarBranchCountRenderer] (
      private val compiled: CompiledPostgreSqlRendering,
      private val terminal: CompiledPostgreSqlTerminal.GlobalCountStar,
      private val shapes: Vector[PostgreSqlFactorStar.Shape]) {
    val anchor: BagId = shapes.head.anchor
    val branchOrder: Vector[BagId] = compiled.bags.map(_.factor.bagId).filterNot(_ == anchor).sortBy(_.value)

    def render(options: Options): Either[Vector[PostgreSqlRenderingError], Result] =
      rendered(options, None, collapseTerminal = false)

    /** Additive experiment: fuse only the last, singly consumed weighted stage.
      * Earlier stages can serve both restriction and multiplication; keep them shared.
      */
    def renderTerminalCollapsed(options: Options): Either[Vector[PostgreSqlRenderingError], Result] =
      rendered(options, None, collapseTerminal = true)

    private def rendered(options: Options, probe: Option[Probe => Unit], collapseTerminal: Boolean)
        : Either[Vector[PostgreSqlRenderingError], Result] = {
      if (options == null || options.strategy == null || options.anchor == null ||
          options.anchor.exists(_ == null) || options.branchOrder == null || options.branchOrder.exists(_ == null))
        return decline("non-null physical options required")
      if (options.maximumCharacters <= 0 || options.maximumCharacters > MaximumCharacters)
        return decline("SQL-character limit must be within 1..1048576")
      shapes.find(s => options.anchor.forall(_ == s.anchor)) match {
        case None => decline("no certified factor-star for the requested anchor")
        case Some(shape) =>
          val leaves = compiled.bags.map(_.factor.bagId).filterNot(_ == shape.anchor).sortBy(_.value)
          val order = if (options.branchOrder.isEmpty) leaves else options.branchOrder
          if (order.size != leaves.size || order.distinct.size != order.size || order.toSet != leaves.toSet)
            decline("branch order must contain every non-anchor factor exactly once")
          else emit(compiled, terminal, shape, order, options, probe, collapseTerminal) match {
            case None => decline("SQL exceeds the physical profile's character limit")
            case Some(sql) =>
              val profile = if (collapseTerminal) TerminalCollapsedProfileId else ProfileId
              val identity = Vector(profile, options.strategy.id, shape.anchor.value.toString,
                order.map(_.value).mkString(","), sql).mkString("\n")
              val digest = MessageDigest.getInstance("SHA-256").digest(identity.getBytes(UTF_8))
                .map(b => f"${b & 0xff}%02x").mkString
              Right(Result(sql, "sha256:" + digest, shape.anchor, order, options.strategy, compiled.output))
          }
      }
    }

    /** Four fixed physical aliases, no statistics or outcome-based pruning. */
    def candidates: Either[Vector[PostgreSqlRenderingError], Vector[Candidate]] =
      candidatePool(includeProbes = true, collapseTerminal = false)

    /** Identical legacy actions without constructing unused diagnostic SQL. */
    def candidatesWithoutProbes: Either[Vector[PostgreSqlRenderingError], Vector[Candidate]] =
      candidatePool(includeProbes = false, collapseTerminal = false)

    def terminalCollapsedCandidates: Either[Vector[PostgreSqlRenderingError], Vector[Candidate]] =
      candidatePool(includeProbes = false, collapseTerminal = true)

    private def candidatePool(includeProbes: Boolean, collapseTerminal: Boolean)
        : Either[Vector[PostgreSqlRenderingError], Vector[Candidate]] = {
      val configs = for (strategy <- Vector(Strategy.PerKey, Strategy.RestrictedGroup);
        (label, order) <- Vector("forward" -> branchOrder, "reverse" -> branchOrder.reverse))
        yield strategy.id + "-" + label -> Options(strategy, Some(anchor), order)
      configs.foldLeft[Either[Vector[PostgreSqlRenderingError], Vector[Candidate]]](Right(Vector.empty)) {
        case (acc, (name, options)) => acc.flatMap { xs =>
          val probes = Vector.newBuilder[Probe]
          val capture = if (includeProbes) Some((p: Probe) => { probes += p; () }) else None
          rendered(options, capture, collapseTerminal)
            .map(r => xs :+ Candidate(name, r, probes.result()))
        }
      }
    }

    /** Branch roles derive from the validated ownership/binding graph, never SQL text. */
    def branches: Vector[Branch] = compiled.bags.sortBy(_.factor.bagId.value).map { bag =>
      val boundary = shapes.head.boundaries(bag.factor.bagId)
      val columns = for (o <- bag.ownedOccurrences; (column, variable) <- o.edge.bindings if boundary.contains(variable))
        yield BoundaryColumn(o.relation.qualifiedName, o.relation.columns(column.ordinal).sourceName)
      Branch(bag.factor.bagId, boundary.size, shapes.head.shared.size,
        bag.ownedOccurrences.size, columns.distinct)
    }

    /** Standalone raw, key-domain and one-branch survival EXPLAIN targets. */
    def probes: Either[Vector[PostgreSqlRenderingError], Vector[Probe]] = {
      val defs = compiled.bags.map { bag => bag.factor.bagId ->
        PostgreSqlEmitter.initialFactorDefinition(compiled, bag, MaximumCharacters / 3) }
      if (defs.exists(_._2.isEmpty)) return decline("branch probe exceeds its SQL bound")
      val byId = defs.map { case (id, sql) => id -> sql.get }.toMap
      def raw(id: BagId) = quote(PostgreSqlNames.initialFactor(id))
      val shape = shapes.head
      val out = compiled.bags.sortBy(_.factor.bagId.value).flatMap { bag =>
        val id = bag.factor.bagId; val keys = shape.boundaries(id)
        val rawProbe = Probe(id, "raw", "WITH\n" + byId(id) + "\nSELECT * FROM " + raw(id) + ";")
        val keyProbe = Probe(id, "keys", "WITH\n" + byId(id) + "\nSELECT DISTINCT " +
          keys.map(v => col("r", v)).mkString(", ") + " FROM " + raw(id) + " r WHERE " +
          keys.map(v => col("r", v) + " IS NOT NULL").mkString(" AND ") + ";")
        val survival = if (id == anchor) Vector.empty else Vector(Probe(id, "survival",
          "WITH\n" + byId(anchor) + ",\n" + byId(id) + "\nSELECT DISTINCT " +
          shape.shared.map(v => col("a", v)).mkString(", ") + " FROM " + raw(anchor) + " a WHERE " +
          shape.shared.map(v => col("a", v) + " IS NOT NULL").mkString(" AND ") +
          " AND EXISTS (SELECT 1 FROM " + raw(id) + " r WHERE " +
          keys.map(v => col("r", v) + " = " + col("a", v)).mkString(" AND ") + ");"))
        Vector(rawProbe, keyProbe) ++ survival
      }
      Right(out)
    }
  }

  def render(query: QueryIR, input: CandidateSearchInput, plan: BagFactorPlan,
      options: Options): Either[Vector[PostgreSqlRenderingError], Result] =
    prepare(query, input, plan).flatMap(_.render(options))

  def prepare(query: QueryIR, input: CandidateSearchInput, plan: BagFactorPlan)
      : Either[Vector[PostgreSqlRenderingError], Prepared] = {
    if (query == null || input == null || plan == null)
      return decline("non-null query, input and plan required")
    if (input.widthBound != 1 || plan.sourceWidthBound != 1 ||
        plan.factors.size > PostgreSqlFactorStar.MaximumFactors)
      return decline("requires a bounded width-one plan")
    PostgreSqlBagFactorRenderer.compileValidatedWithEnumerationProfile(query, input, plan,
      PostgreSqlRenderingLimits.default, BagFactorPlanningLimits.default,
      YannakakisReductionMode.FullTwoPass, PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
    ).flatMap { compiled =>
      compiled.terminal match {
        case terminal: CompiledPostgreSqlTerminal.GlobalCountStar =>
          if (compiled.bags.exists(_.supportStrategy == PostgreSqlBagSupportStrategy.MaterializedDistinctSupport))
            decline("support-bearing factors are outside this profile")
          else {
            val stars = PostgreSqlFactorStar.certify(compiled.bags.map(b => b.factor.bagId -> b.factor.vertices))
            if (stars.isEmpty) decline("no certified factor-star")
            else Right(new Prepared(compiled.copy(cteMaterialization = PostgreSqlCteMaterializationDirective.NotMaterialized),
              terminal, stars))
          }
        case _ => decline("requires an already admitted scalar global COUNT(*)")
      }
    }
  }

  private def emit(c: CompiledPostgreSqlRendering, t: CompiledPostgreSqlTerminal.GlobalCountStar,
      shape: PostgreSqlFactorStar.Shape, order: Vector[BagId], options: Options,
      probe: Option[Probe => Unit], collapseTerminal: Boolean): Option[String] = {
    val definitions = scala.collection.mutable.ArrayBuffer.empty[String]
    var characters = 5 // WITH and newline
    def add(definition: String): Boolean = {
      val next = characters.toLong + definition.length + 2 // comma and newline
      if (next > options.maximumCharacters) false
      else { definitions += definition; characters = next.toInt; true }
    }
    c.bags.sortBy(_.factor.bagId.value).foreach { bag =>
      PostgreSqlEmitter.initialFactorDefinition(c, bag, options.maximumCharacters - characters) match {
        case Some(sql) if add(sql) => ()
        case _ => return None
      }
    }
    def raw(id: BagId) = quote(PostgreSqlNames.initialFactor(id))
    def cols(alias: String, vs: Vector[VariableId]) = vs.map(v => col(alias, v)).mkString(", ")
    def eqs(a: String, b: String, vs: Vector[VariableId]) =
      vs.map(v => col(a, v) + " = " + col(b, v)).mkString(" AND ")
    val unitSum = "SUM(CAST(1 AS NUMERIC))"
    var seed = quote("yr_sc_anchor")
    if (!add(seed + " AS MATERIALIZED (SELECT " + cols("r", shape.shared) + ", " + unitSum +
        " AS yr_freq FROM " + raw(shape.anchor) + " AS r WHERE " +
        shape.shared.map(v => col("r", v) + " IS NOT NULL").mkString(" AND ") +
        " GROUP BY " + cols("r", shape.shared) + ")")) return None
    def stage(id: BagId): Unit = probe.foreach(_(Probe(id, "stage",
      "WITH\n" + definitions.mkString(",\n") + "\nSELECT * FROM " + seed + ";")))
    stage(shape.anchor)
    var finalSource: Option[String] = None
    order.foreach { id =>
      val boundary = shape.boundaries(id)
      val next = quote(s"yr_sc_step_${id.value}")
      def perKey(domain: String) = "SELECT " + unitSum + " AS yr_freq FROM " + raw(id) +
        " AS r WHERE " + eqs("r", domain, boundary)
      val source = if (options.strategy == Strategy.PerKey && boundary == shape.shared) {
        // The seed is already unique on this full boundary: no domain or regrouping needed.
        seed + " AS a CROSS JOIN LATERAL (" + perKey("a") + ") AS b WHERE b.yr_freq IS NOT NULL"
      } else {
        val counted = quote(s"yr_sc_count_${id.value}")
        val countSql = options.strategy match {
          case Strategy.PerKey =>
            // Different anchor tuples can share a proper-subset boundary. Count
            // its DISTINCT domain once, then reattach each anchor weight once.
            "SELECT " + cols("d", boundary) + ", b.yr_freq FROM (SELECT DISTINCT " +
              boundary.map(v => quote(PostgreSqlNames.variable(v))).mkString(", ") +
              " FROM " + seed + ") AS d CROSS JOIN LATERAL (" + perKey("d") +
              ") AS b WHERE b.yr_freq IS NOT NULL"
          case Strategy.RestrictedGroup =>
            "SELECT " + cols("r", boundary) + ", " + unitSum + " AS yr_freq FROM " +
              raw(id) + " AS r WHERE EXISTS (SELECT 1 FROM " + seed + " AS a WHERE " +
              eqs("r", "a", boundary) + ") GROUP BY " + cols("r", boundary)
        }
        if (!add(counted + " AS MATERIALIZED (" + countSql + ")")) return None
        seed + " AS a JOIN " + counted + " AS b ON " + eqs("a", "b", boundary)
      }
      if (collapseTerminal && id == order.last) finalSource = Some(source)
      else {
        if (!add(next + " AS MATERIALIZED (SELECT " + cols("a", shape.shared) +
          ", a.yr_freq * b.yr_freq AS yr_freq FROM " + source + ")")) return None
        seed = next
        stage(id)
      }
    }
    val sum = if (finalSource.nonEmpty) "a.yr_freq * b.yr_freq" else "yr_freq"
    val terminal = "\nSELECT CAST(COALESCE(SUM(" + sum + "), CAST(0 AS NUMERIC)) AS BIGINT) AS " +
      quote(t.output.name) + " FROM " + finalSource.getOrElse(seed) + ";"
    // add() charged a separator after the final definition; remove exactly that charge.
    if (characters.toLong - 2 + terminal.length > options.maximumCharacters) None
    else Some("WITH\n" + definitions.mkString(",\n") + terminal)
  }

  private def quote(s: String) = "\"" + s.replace("\"", "\"\"") + "\""
  private def col(alias: String, v: VariableId) = alias + "." + quote(PostgreSqlNames.variable(v))
}
