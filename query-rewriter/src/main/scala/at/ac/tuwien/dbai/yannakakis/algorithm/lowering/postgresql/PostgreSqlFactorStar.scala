package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.core.{BagId, VariableId}

/** Conditional independence certificate, not a join-order or cost heuristic.
  * Callers must separately validate the source, ownership and decomposition.
  */
private[postgresql] object PostgreSqlFactorStar {
  val MaximumFactors = 32
  final case class Shape(
      anchor: BagId,
      shared: Vector[VariableId],
      boundaries: Map[BagId, Vector[VariableId]]
  )

  /** Algebraic default providers prefer the anchor when it contains a key.
    * Binding closure establishes equal values, not equal SQL descriptors:
    * a SQL consumer must route selected columns through their exact owners.
    * Every non-shared group key must be local to one factor.
    */
  final case class GroupedShape(star: Shape, groupKeys: Vector[VariableId],
      providers: Vector[BagId], anchorKeys: Vector[VariableId]) {
    val outputLeaves: Vector[BagId] = providers.filterNot(_ == star.anchor).distinct
    def keysFor(id: BagId): Vector[VariableId] =
      (star.boundaries(id) ++ groupKeys.zip(providers).collect {
        case (key, provider) if provider == id => key
      }).distinct.sorted
  }

  def certifyGrouping(factors: Vector[(BagId, Vector[VariableId])],
      keys: Vector[VariableId]): Vector[GroupedShape] = {
    if (keys == null || keys.isEmpty || keys.exists(_ == null) || keys.distinct != keys)
      return Vector.empty
    val stars = certify(factors)
    if (stars.isEmpty) return Vector.empty
    val occurrences = factors.flatMap { case (id, vs) => vs.map(_ -> id) }.groupMap(_._1)(_._2)
    if (keys.exists(k => !occurrences.contains(k))) return Vector.empty
    val byId = factors.toMap
    stars.flatMap { star =>
      val providers = keys.map { key =>
        if (byId(star.anchor).contains(key)) Some(star.anchor)
        else occurrences(key) match { case Vector(id) => Some(id); case _ => None }
      }
      if (providers.exists(_.isEmpty)) None
      else {
        val ids = providers.flatten
        val anchorKeys = (star.shared ++ keys.zip(ids).collect {
          case (key, id) if id == star.anchor => key
        }).distinct.sorted
        Some(GroupedShape(star, keys, ids, anchorKeys))
      }
    }
  }

  def certify(factors: Vector[(BagId, Vector[VariableId])]): Vector[Shape] = {
    if (factors == null || factors.size < 3 || factors.size > MaximumFactors ||
        factors.exists(p => p == null || p._1 == null || p._2 == null || p._2.isEmpty ||
          p._2.exists(_ == null) || p._2.distinct != p._2) ||
        factors.map(_._1).distinct.size != factors.size) return Vector.empty
    val shared = factors.flatMap { case (id, vs) => vs.map(_ -> id) }
      .groupMap(_._1)(_._2).collect { case (v, ids) if ids.size > 1 => v }.toVector.sorted
    if (shared.isEmpty) return Vector.empty
    val boundaries = factors.map { case (id, vs) => id -> vs.filter(shared.contains).sorted }.toMap
    if (boundaries.values.exists(_.isEmpty)) return Vector.empty
    factors.sortBy(_._1.value).collect {
      case (id, vs) if shared.forall(vs.contains) => Shape(id, shared, boundaries)
    }
  }
}
