package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{BagFactorPlan, BagFactorPlanningLimits}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryIR

/**
  * Explicit, development-only U1 physical alternative. No public/default path
  * calls this renderer. The language boundary remains the validated U1 fragment.
  *
  * A width-one factor star is certified independently of its execution root:
  * every variable shared by factors is in an anchor, every leaf meets it, and
  * each output key belongs to exactly one non-anchor factor. Thus different
  * branches are independent conditional on the anchor's shared key tuple.
  *
  * Non-output branches are counted first, then joined to counted anchor rows.
  * Output-key branches are semijoin-filtered by this core before counting.
  * Every original occurrence still contributes its bag multiplicity; no FK or
  * uniqueness assumption, support-cover multiplicity, or floating weight is used.
  */
private[yannakakis] object PostgreSqlAcyclicBranchCountRenderer {
  val ProfileId: String = "acyclic-u1-anchor-branch-count-v1"
  val ProgressiveProfileId: String = "acyclic-u1-progressive-branch-count-v1"
  private val MaximumFactors = PostgreSqlFactorStar.MaximumFactors
  private val MaximumCharacters = 1024 * 1024

  final case class Result private[postgresql] (
      sql: String,
      actionId: String,
      anchor: BagId,
      factorCount: Int,
      output: Vector[PostgreSqlOutputColumn]
  )

  private[postgresql] final case class Shape(
      anchor: BagId,
      shared: Vector[VariableId],
      boundaries: Map[BagId, Vector[VariableId]],
      keyProviders: Vector[BagId]
  )

  /** SQL-free shape check; callers must first validate the input factor plan. */
  private[postgresql] def certify(
      factors: Vector[(BagId, Vector[VariableId])],
      keys: Vector[VariableId]
  ): Option[Shape] = {
    val stars = PostgreSqlFactorStar.certify(factors)
    if (stars.isEmpty || keys == null || keys.size != 2 ||
        keys.exists(_ == null) || keys.distinct.size != 2) return None
    val occurrences = factors.flatMap { case (id, vs) => vs.map(_ -> id) }
      .groupMap(_._1)(_._2)
    if (keys.exists(k => occurrences.getOrElse(k, Vector.empty).size != 1)) return None
    val providers = keys.map(k => occurrences(k).head)
    stars.find(s => !providers.contains(s.anchor))
      .map(s => Shape(s.anchor, s.shared, s.boundaries, providers))
  }

  def render(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan
  ): Either[Vector[PostgreSqlRenderingError], Result] =
    renderWithFirstBranch(query, input, plan, None)

  /** Explicit physical choice, not a statistics query or a query-name heuristic. */
  def renderWithFirstBranch(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      firstBranch: Option[BagId]
  ): Either[Vector[PostgreSqlRenderingError], Result] = {
    def decline(detail: String) = Left(Vector(
      PostgreSqlRenderingError.ModelInvariantViolation(ProfileId + ": " + detail)
    ))
    if (plan == null || input == null || query == null || firstBranch == null || firstBranch.exists(_ == null))
      return decline("query, input and plan are required")
    if (input.widthBound != 1 || plan.sourceWidthBound != 1 ||
        plan.factors.size > MaximumFactors)
      return decline("requires a bounded width-one plan")

    PostgreSqlBagFactorRenderer.compileValidatedWithEnumerationProfile(
      query, input, plan, PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default, YannakakisReductionMode.FullTwoPass,
      PostgreSqlEnumerationRenderingProfile.StatisticsSafeV2
    ).flatMap { compiled =>
      compiled.terminal match {
        case terminal: CompiledPostgreSqlTerminal.DistributedGroupedCountStar =>
          if (compiled.bags.exists(b => b.supportStrategy ==
              PostgreSqlBagSupportStrategy.MaterializedDistinctSupport))
            decline("support-bearing factors are outside this profile")
          else certify(compiled.bags.map(b => b.factor.bagId -> b.factor.vertices),
              terminal.groupVariables) match {
            case None => decline("no certified anchor-star with branch-local output keys")
            case Some(shape) =>
              val plain = compiled.bags.map(_.factor.bagId)
                .filterNot(id => id == shape.anchor || shape.keyProviders.contains(id))
              if (firstBranch.exists(id => !plain.contains(id) || plain.size < 2))
                return decline("first branch must be a non-output leaf with another non-output leaf remaining")
              val raw = compiled.copy(cteMaterialization =
                PostgreSqlCteMaterializationDirective.NotMaterialized)
              emit(raw, terminal, shape, firstBranch) match {
                case None => decline("SQL exceeds the one-Mi-character profile limit")
                case Some(sql) => Right(Result(sql, "sha256:" + sha256(
                  firstBranch.fold(ProfileId)(_ => ProgressiveProfileId) + "\n" + sql),
                  shape.anchor, compiled.bags.size, compiled.output))
              }
          }
        case _ => decline("requires an already admitted distributed two-key COUNT(*)")
      }
    }
  }

  private def emit(
      compiled: CompiledPostgreSqlRendering,
      terminal: CompiledPostgreSqlTerminal.DistributedGroupedCountStar,
      shape: Shape,
      firstBranch: Option[BagId]
  ): Option[String] = {
    val definitions = Vector.newBuilder[String]
    var characters = 0
    def add(value: String): Boolean = {
      characters += value.length + 2
      if (characters > MaximumCharacters) false
      else { definitions += value; true }
    }
    val bags = compiled.bags.sortBy(_.factor.bagId.value)
    bags.foreach { bag =>
      PostgreSqlEmitter.initialFactorDefinition(compiled, bag, MaximumCharacters - characters) match {
        case Some(sql) if add(sql) => ()
        case _ => return None
      }
    }
    def raw(id: BagId): String = quote(PostgreSqlNames.initialFactor(id))
    def state(id: BagId): String = quote(s"yr_bc_b${id.value}")
    def col(alias: String, v: VariableId): String = alias + "." + quote(PostgreSqlNames.variable(v))
    def equal(left: String, right: String, vs: Vector[VariableId]): String =
      vs.map(v => col(left, v) + " = " + col(right, v)).mkString(" AND ")
    def count(id: BagId, keys: Vector[VariableId], filter: Option[String]): String = {
      val select = keys.map(v => col("r", v)).mkString(", ")
      // NUMERIC SUM avoids a local COUNT BIGINT overflow before later pruning.
      state(id) + " AS MATERIALIZED (\nSELECT " + select +
        ", SUM(CAST(1 AS NUMERIC)) AS yr_freq\nFROM " + raw(id) + " AS r" +
        filter.fold("")(f => "\nWHERE EXISTS (SELECT 1 FROM " + f + " AS a WHERE " +
          equal("r", "a", shape.boundaries(id)) + ")") +
        "\nGROUP BY " + select + "\n)"
    }
    if (!add(count(shape.anchor, shape.shared, None))) return None
    val leaves = bags.map(_.factor.bagId).filterNot(_ == shape.anchor)
    val keyLeaves = shape.keyProviders.distinct
    val plainLeaves = leaves.filterNot(id => keyLeaves.contains(id) || firstBranch.contains(id))
    val seed = firstBranch.fold(state(shape.anchor)) { id =>
      if (!add(count(id, shape.boundaries(id), Some(state(shape.anchor))))) return None
      val name = quote("yr_bc_seed")
      val sql = name + " AS MATERIALIZED (\nSELECT " +
        shape.shared.map(v => col("a", v)).mkString(", ") +
        s", a.yr_freq * b${id.value}.yr_freq AS yr_freq\nFROM " + state(shape.anchor) + " AS a" +
        "\nJOIN " + state(id) + s" AS b${id.value} ON " +
        equal("a", s"b${id.value}", shape.boundaries(id)) + "\n)"
      if (!add(sql)) return None
      name
    }
    plainLeaves.foreach { id =>
      if (!add(count(id, shape.boundaries(id), Some(seed)))) return None
    }
    val core = quote("yr_bc_core")
    val weights = Vector("a.yr_freq") ++ plainLeaves.map(id => s"b${id.value}.yr_freq")
    val coreBody = core + " AS MATERIALIZED (\nSELECT " +
      shape.shared.map(v => col("a", v)).mkString(", ") + ", " +
      weights.mkString(" * ") + " AS yr_freq\nFROM " + seed + " AS a" +
      plainLeaves.map { id => "\nJOIN " + state(id) + s" AS b${id.value} ON " +
        equal("a", s"b${id.value}", shape.boundaries(id)) }.mkString + "\n)"
    if (!add(coreBody)) return None
    keyLeaves.foreach { id =>
      val keys = (shape.boundaries(id) ++ terminal.groupVariables.zip(shape.keyProviders)
        .collect { case (key, provider) if provider == id => key }).distinct.sorted
      if (!add(count(id, keys, Some(core)))) return None
    }
    val outputs = terminal.groupVariables.zip(shape.keyProviders).map {
      case (v, id) => col(s"b${id.value}", v)
    }
    val finalSelect = "\nSELECT " + outputs.zip(terminal.groupOutputs).map {
      case (value, output) => value + " AS " + quote(output.name)
    }.mkString(", ") + ", CAST(SUM(" +
      (Vector("a.yr_freq") ++ keyLeaves.map(id => s"b${id.value}.yr_freq")).mkString(" * ") +
      ") AS BIGINT) AS " + quote(terminal.countOutput.name) + "\nFROM " + core + " AS a" +
      keyLeaves.map { id => "\nJOIN " + state(id) + s" AS b${id.value} ON " +
        equal("a", s"b${id.value}", shape.boundaries(id)) }.mkString +
      "\nGROUP BY " + outputs.mkString(", ") + ";"
    if (characters.toLong + finalSelect.length + 5 > MaximumCharacters) None
    else Some("WITH\n" + definitions.result().mkString(",\n") + finalSelect)
  }

  private def quote(value: String): String = "\"" + value.replace("\"", "\"\"") + "\""
  private def sha256(value: String): String = MessageDigest.getInstance("SHA-256")
    .digest(value.getBytes(StandardCharsets.UTF_8)).map(b => f"${b & 0xff}%02x").mkString
}
