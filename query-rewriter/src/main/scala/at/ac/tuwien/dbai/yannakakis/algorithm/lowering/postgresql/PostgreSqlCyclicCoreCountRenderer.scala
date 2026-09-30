package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{BagFactorPlan, BagFactorPlanningLimits}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.{EdgeId, VariableId}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{ColumnRef, QueryIR}

/** Additive physical realization of already-admitted grouped COUNT(*).
  * A connected cyclic core is counted once; independent, group-producing
  * pendant components are counted on their boundary and private output keys.
  * Detection uses occurrence-distinct hyperedges, never SQL text, table names,
  * foreign keys, uniqueness assumptions, or observed cardinalities.
  */
private[yannakakis] object PostgreSqlCyclicCoreCountRenderer {
  val ProfileId = "cyclic-core-branch-count-v1"
  val MaximumOccurrences = 64
  val MaximumVariables = 64
  val MaximumCharacters = 1024 * 1024

  final case class Branch(edges: Vector[EdgeId], boundary: VariableId)
  final case class Shape(core: Vector[EdgeId], branches: Vector[Branch])
  final case class Result(sql: String, actionId: String, shape: Shape,
      output: Vector[PostgreSqlOutputColumn])
  private case class Terminal(keys: Vector[VariableId], refs: Vector[ColumnRef],
      groups: Vector[PostgreSqlOutputColumn], count: PostgreSqlOutputColumn)

  private def decline(message: String) = Left(Vector(
    PostgreSqlRenderingError.ModelInvariantViolation(ProfileId + ": " + message)))

  /** Bounded, SQL-free recognition. Each cut is a component after removing one
    * variable from adjacency, not an arbitrary subset search. Prefer maximal
    * branches; recheck the complete partition after resolving overlaps.
    */
  private[postgresql] def recognize(graph: Vector[(EdgeId, Vector[VariableId])],
      keys: Vector[VariableId]): Option[Shape] = {
    if (graph == null || keys == null || graph.isEmpty || graph.size > MaximumOccurrences ||
        graph.exists { case (id, vs) => id == null || vs == null || vs.isEmpty || vs.exists(_ == null) } ||
        keys.isEmpty || keys.exists(_ == null) || keys.distinct.size != keys.size ||
        graph.map(_._1).distinct.size != graph.size) return None
    val all = graph.sortBy(_._1.value)
    val vertices = all.flatMap(_._2).distinct.sorted
    if (vertices.size > MaximumVariables || !keys.forall(vertices.contains) ||
        components(all).size != 1 || acyclic(all.map(_._2.toSet))) return None
    val byId = all.toMap
    def vars(ids: Vector[EdgeId]) = ids.flatMap(byId).toSet
    val ids = all.map(_._1)
    val candidates = vertices.flatMap { cut =>
      components(all.map { case (id, vs) => id -> vs.filterNot(_ == cut) }).flatMap { part =>
        val outside = ids.filterNot(part.contains)
        val privateKeys = keys.toSet.intersect(vars(part)) -- vars(outside)
        if (outside.nonEmpty && vars(part).intersect(vars(outside)) == Set(cut) &&
            privateKeys.nonEmpty && acyclic(part.map(id => byId(id).toSet)))
          Some(Branch(part, cut)) else None
      }
    }.distinct.sortBy(b => (-b.edges.size, b.edges.map(_.value).mkString(","), b.boundary.value))
    val picked = candidates.foldLeft(Vector.empty[Branch]) { (chosen, next) =>
      if (chosen.exists(_.edges.exists(next.edges.contains))) chosen else chosen :+ next
    }.sortBy(_.edges.head.value)
    val core = ids.filterNot(id => picked.exists(_.edges.contains(id)))
    if (picked.isEmpty || core.isEmpty || components(core.map(id => id -> byId(id))).size != 1 ||
        acyclic(core.map(id => byId(id).toSet))) None
    else if (!picked.forall { b =>
      vars(b.edges).intersect(vars(core)) == Set(b.boundary) &&
        vars(b.edges).intersect(vars(ids.filterNot(b.edges.contains))) == Set(b.boundary)
    }) None
    else Some(Shape(core, picked))
  }

  private def components(graph: Vector[(EdgeId, Vector[VariableId])]): Vector[Vector[EdgeId]] = {
    var remaining = graph.map(_._1).toSet
    val byId = graph.toMap
    val result = Vector.newBuilder[Vector[EdgeId]]
    while (remaining.nonEmpty) {
      var found = Set(remaining.minBy(_.value))
      var frontier = found
      while (frontier.nonEmpty) {
        val reachable = frontier.toVector.flatMap(byId).toSet
        val next = remaining.filter(id => !found(id) && byId(id).exists(reachable))
        found ++= next
        frontier = next
      }
      remaining --= found
      result += found.toVector.sortBy(_.value)
    }
    result.result()
  }

  /** Keep every emitted join prefix connected. Large explicit join lists may
    * exceed the optimizer's join-collapse budget; occurrence-ID order can put
    * unrelated dimension tables in its first search block. Seed on the edge
    * meeting most branch boundaries, then close known interfaces before adding
    * new variables. This is a deterministic structural heuristic, not a claim
    * of optimal join order or a query-name/statistics-based special case.
    */
  private[postgresql] def connectedOrder(graph: Vector[(EdgeId, Vector[VariableId])],
      boundaries: Vector[VariableId]): Vector[EdgeId] = {
    val byId = graph.map { case (id, vs) => id -> vs.toSet }.toMap
    val interface = boundaries.toSet
    val seed = graph.minBy { case (id, _) => (-byId(id).intersect(interface).size, -byId(id).size, id.value) }._1
    var order = Vector(seed)
    var seen = byId(seed)
    var remaining = graph.map(_._1).filterNot(_ == seed)
    while (remaining.nonEmpty) {
      val next = remaining.filter(id => byId(id).exists(seen)).minBy { id =>
        ((byId(id) -- seen).size, -byId(id).intersect(seen).size, id.value)
      }
      order :+= next
      seen ++= byId(next)
      remaining = remaining.filterNot(_ == next)
    }
    order
  }

  /** GYO on sets is a scope check only. Occurrence identities and multiplicities
    * remain untouched in the physical partition and SQL.
    */
  private def acyclic(graph: Vector[Set[VariableId]]): Boolean = {
    var edges = graph.filter(_.nonEmpty).distinct
    var changed = true
    while (changed && edges.nonEmpty) {
      val maximal = edges.filterNot(e => edges.exists(other => e != other && e.subsetOf(other)))
      val counts = maximal.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
      val next = maximal.map(_.filter(v => counts(v) > 1)).filter(_.nonEmpty).distinct
      changed = next.toSet != edges.toSet
      edges = next
    }
    edges.isEmpty
  }

  def render(query: QueryIR, input: CandidateSearchInput, plan: BagFactorPlan,
      maximumCharacters: Int = MaximumCharacters): Either[Vector[PostgreSqlRenderingError], Result] = {
    if (query == null || input == null || plan == null) return decline("non-null query, input and plan required")
    if (maximumCharacters <= 0 || maximumCharacters > MaximumCharacters)
      return decline("SQL-character limit must be within 1..1048576")
    if (query.relations.size > MaximumOccurrences) return decline("occurrence limit exceeded")
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
        case None => decline("requires already-admitted structural grouped COUNT(*)")
        case Some(t) =>
          // Structural closure here relies only on ordinary uniform integer
          // equality. Catalog-bound text profiles keep their existing renderers.
          val edges = c.edgeById.values.toVector.sortBy(_.id.value)
          val integral = Set("SMALLINT", "INTEGER", "BIGINT")
          if (!edges.flatMap(_.columns).forall(id => integral(c.metadataByColumn(id).sqlTypeName)))
            decline("requires uniform integer structural bindings")
          else recognize(edges.map(e => e.id -> e.vertices), t.keys) match {
            case None => decline("no connected cyclic core with independent group-producing pendant branches")
            case Some(shape) if c.query.relations.exists { relation =>
              relation.qualifiedName.size == 1 &&
                (0 to shape.branches.size).exists(i => name(i) == relation.qualifiedName.head)
            } => decline("an unqualified source relation would be captured by a generated core-count CTE")
            case Some(shape) => emit(c, t, shape, maximumCharacters) match {
              case None => decline("SQL exceeds the physical profile's character limit")
              case Some(sql) =>
                val identity = ProfileId + "\n" + sql
                val hash = MessageDigest.getInstance("SHA-256").digest(identity.getBytes(UTF_8))
                  .map(b => f"${b & 0xff}%02x").mkString
                Right(Result(sql, "sha256:" + hash, shape, c.output))
            }
          }
      }
    }
  }

  private def emit(c: CompiledPostgreSqlRendering, t: Terminal, shape: Shape,
      maximumCharacters: Int): Option[String] = {
    val partitions = Vector(shape.core) ++ shape.branches.map(_.edges)
    val providers = t.refs.map(ref => partitions.indexWhere(_.contains(c.edgeByOccurrence(ref.relationOccurrenceId).id)))
    val preferred = t.keys.zip(t.refs).map { case (v, ref) =>
      PostgreSqlBindingLocation(c.edgeByOccurrence(ref.relationOccurrenceId).id, ref.id, v)
    }
    val definitions = Vector.newBuilder[String]
    var size = 5
    for ((ids, index) <- partitions.zipWithIndex) {
      val boundary = if (index == 0) shape.branches.map(_.boundary) else Vector(shape.branches(index - 1).boundary)
      val order = connectedOrder(ids.map(id => id -> c.edgeById(id).vertices), boundary)
      val edges = order.map(c.edgeById)
      val variables = edges.flatMap(_.vertices).toSet
      val projected = (boundary ++ t.keys.filter(variables)).distinct.sorted
      val bindings = edges.flatMap(e => e.bindings.map { case (col, v) => PostgreSqlBindingLocation(e.id, col, v) })
      val representatives = projected.map(v => preferred.find(b => b.variableId == v && ids.contains(b.edgeId))
        .getOrElse(bindings.find(_.variableId == v).get))
      val restriction = if (index == 0) None else Some(name(0) -> representatives.find(_.variableId == boundary.head).get)
      PostgreSqlEmitter.occurrenceCountDefinition(c, name(index), edges, representatives, restriction,
        maximumCharacters - size) match {
        case None => return None
        case Some(sql) => definitions += sql; size += sql.length + 2
      }
    }
    val outputs = t.keys.zip(providers).map { case (v, p) => col(p, v) }
    val select = outputs.zip(t.groups).map { case (value, out) => value + " AS " + quote(out.name) }.mkString(", ")
    val weights = partitions.indices.map(i => s"p$i.yr_freq").mkString(" * ")
    val joins = shape.branches.zipWithIndex.map { case (b, i) =>
      " JOIN " + quote(name(i + 1)) + s" AS p${i + 1} ON " + col(0, b.boundary) + " = " + col(i + 1, b.boundary)
    }.mkString
    val suffix = "\nSELECT " + select + ", CAST(SUM(" + weights + ") AS BIGINT) AS " + quote(t.count.name) +
      " FROM " + quote(name(0)) + " AS p0" + joins + " GROUP BY " + outputs.mkString(", ") + ";"
    if (size.toLong - 2 + suffix.length > maximumCharacters) None
    else Some("WITH\n" + definitions.result().mkString(",\n") + suffix)
  }

  private def name(index: Int) = s"yr_cc_$index"
  private def quote(value: String) = "\"" + value.replace("\"", "\"\"") + "\""
  private def col(index: Int, v: VariableId) = s"p$index." + quote(PostgreSqlNames.variable(v))
}
