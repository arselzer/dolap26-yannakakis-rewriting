package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftFailure._
import at.ac.tuwien.dbai.yannakakis.core.{EdgeId, QueryHypergraph, VariableId}

/** A set of edge occurrences in canonical identity order. */
final class EdgeCover private (val edgeIds: Vector[EdgeId])
    extends Serializable {
  def size: Int = edgeIds.size
  def isEmpty: Boolean = edgeIds.isEmpty
  def nonEmpty: Boolean = edgeIds.nonEmpty
  def contains(edgeId: EdgeId): Boolean = edgeIds.contains(edgeId)

  override def equals(other: Any): Boolean = other match {
    case that: EdgeCover => edgeIds == that.edgeIds
    case _               => false
  }

  override def hashCode(): Int = edgeIds.hashCode()
  override def toString: String = edgeIds.mkString("EdgeCover(", ",", ")")
}

object EdgeCover {
  val empty: EdgeCover = new EdgeCover(Vector.empty)

  def create(edgeIds: Iterable[EdgeId]): Either[SoftFailure, EdgeCover] = {
    if (edgeIds eq null) {
      Left(MissingInput("edge cover ids"))
    } else {
      val supplied = edgeIds.iterator.toVector
      if (supplied.exists(_ eq null)) {
        Left(NullEdgeIdInCover)
      } else {
        val duplicates = supplied
          .groupBy(identity)
          .collect { case (edgeId, occurrences) if occurrences.size > 1 => edgeId }
          .toVector
          .sorted
        if (duplicates.nonEmpty) Left(DuplicateEdgeIdsInCover(duplicates))
        else Right(canonical(supplied))
      }
    }
  }

  private[soft] def canonical(edgeIds: Iterable[EdgeId]): EdgeCover = {
    val ids = edgeIds.iterator.toVector.sorted
    if (ids.isEmpty) empty else new EdgeCover(ids)
  }

  implicit val ordering: Ordering[EdgeCover] = new Ordering[EdgeCover] {
    override def compare(left: EdgeCover, right: EdgeCover): Int = {
      val sizeComparison = Integer.compare(left.size, right.size)
      if (sizeComparison != 0) sizeComparison
      else SoftOrdering.compareVectors(left.edgeIds, right.edgeIds)
    }
  }
}

/** A non-empty [lambda]-edge component, retaining occurrence identities. */
final class EdgeComponent private[soft] (val edgeIds: Vector[EdgeId])
    extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: EdgeComponent => edgeIds == that.edgeIds
    case _                   => false
  }

  override def hashCode(): Int = edgeIds.hashCode()
  override def toString: String = edgeIds.mkString("EdgeComponent(", ",", ")")
}

object EdgeComponent {
  private[soft] def canonical(edgeIds: Iterable[EdgeId]): EdgeComponent =
    new EdgeComponent(edgeIds.iterator.toVector.sorted)

  implicit val ordering: Ordering[EdgeComponent] =
    (left: EdgeComponent, right: EdgeComponent) =>
      SoftOrdering.compareVectors(left.edgeIds, right.edgeIds)
}

/** One direct Definition-3 derivation of a structural soft bag. */
final case class SoftBagWitness(
    lambda1: EdgeCover,
    lambda2: EdgeCover,
    component: EdgeComponent
) extends Serializable

object SoftBagWitness {
  implicit val ordering: Ordering[SoftBagWitness] =
    new Ordering[SoftBagWitness] {
      override def compare(
          left: SoftBagWitness,
          right: SoftBagWitness
      ): Int = {
        val firstCover = EdgeCover.ordering.compare(left.lambda1, right.lambda1)
        if (firstCover != 0) firstCover
        else {
          val secondCover = EdgeCover.ordering.compare(left.lambda2, right.lambda2)
          if (secondCover != 0) secondCover
          else EdgeComponent.ordering.compare(left.component, right.component)
        }
      }
    }
}

/** One width-bounded cover of a candidate bag and its ConCov status. */
final case class CandidateCover(cover: EdgeCover, isConnected: Boolean)
    extends Serializable

/**
  * A structural soft bag together with all derivations and all bounded covers.
  *
  * Bags are deduplicated by vertices. Edge-distinct witnesses and covers are
  * deliberately retained for self joins and later physical-plan selection.
  */
final class SoftBagCandidate private[soft] (
    val vertices: Vector[VariableId],
    val witnesses: Vector[SoftBagWitness],
    val covers: Vector[CandidateCover]
) extends Serializable {
  val hasConnectedCover: Boolean = covers.exists(_.isConnected)

  override def equals(other: Any): Boolean = other match {
    case that: SoftBagCandidate =>
      vertices == that.vertices &&
        witnesses == that.witnesses &&
        covers == that.covers
    case _ => false
  }

  override def hashCode(): Int = (vertices, witnesses, covers).hashCode()
  override def toString: String =
    s"SoftBagCandidate(${vertices.mkString("[", ",", "]")}," +
      s"witnesses=${witnesses.size},covers=${covers.size}," +
      s"hasConnectedCover=$hasConnectedCover)"
}

object SoftBagCandidate {
  implicit val ordering: Ordering[SoftBagCandidate] =
    (left: SoftBagCandidate, right: SoftBagCandidate) =>
      SoftOrdering.compareVectors(left.vertices, right.vertices)
}

final class SoftBagGeneration private[soft] (
    val sourceHypergraph: QueryHypergraph,
    val widthBound: Int,
    val enumeratedCovers: Vector[EdgeCover],
    val candidates: Vector[SoftBagCandidate]
) extends Serializable {

  /**
    * Projects exhaustive structural candidates into the set admissible to a
    * decomposition search.
    *
    * Callers choose the mode explicitly: `false` is unconstrained soft width
    * and retains every physical cover, whereas `true` is ConCov and retains
    * only connected physical covers, dropping bags for which none exist.
    * Definition-3 covers and exhaustive derivation witnesses are unaffected.
    */
  def admissible(
      requireConnectedCover: Boolean
  ): AdmissibleSoftBagGeneration = {
    val projected = candidates.flatMap { candidate =>
      val physicalCovers =
        if (requireConnectedCover) candidate.covers.filter(_.isConnected)
        else candidate.covers

      if (requireConnectedCover && physicalCovers.isEmpty) None
      else
        Some(
          AdmissibleSoftBagCandidate(
            candidate.vertices,
            candidate.witnesses,
            physicalCovers
          )
        )
    }

    new AdmissibleSoftBagGeneration(
      sourceHypergraph,
      widthBound,
      requireConnectedCover,
      enumeratedCovers,
      projected
    )
  }

  override def equals(other: Any): Boolean = other match {
    case that: SoftBagGeneration =>
      sourceHypergraph == that.sourceHypergraph &&
        widthBound == that.widthBound &&
        enumeratedCovers == that.enumeratedCovers &&
        candidates == that.candidates
    case _ => false
  }

  override def hashCode(): Int =
    (sourceHypergraph, widthBound, enumeratedCovers, candidates).hashCode()

  override def toString: String =
    s"SoftBagGeneration(widthBound=$widthBound,covers=${enumeratedCovers.size},candidates=${candidates.size})"
}

/**
  * A structural candidate and exactly the physical covers allowed by the
  * selected admissibility mode.
  */
final case class AdmissibleSoftBagCandidate private[soft] (
    vertices: Vector[VariableId],
    witnesses: Vector[SoftBagWitness],
    physicalCovers: Vector[CandidateCover]
) extends Serializable

/** A typed boundary between exhaustive generation and decomposition search. */
final class AdmissibleSoftBagGeneration private[soft] (
    val sourceHypergraph: QueryHypergraph,
    val widthBound: Int,
    val requireConnectedCover: Boolean,
    val definitionCovers: Vector[EdgeCover],
    val candidates: Vector[AdmissibleSoftBagCandidate]
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: AdmissibleSoftBagGeneration =>
      sourceHypergraph == that.sourceHypergraph &&
        widthBound == that.widthBound &&
        requireConnectedCover == that.requireConnectedCover &&
        definitionCovers == that.definitionCovers &&
        candidates == that.candidates
    case _ => false
  }

  override def hashCode(): Int =
    (
      sourceHypergraph,
      widthBound,
      requireConnectedCover,
      definitionCovers,
      candidates
    ).hashCode()

  override def toString: String =
    s"AdmissibleSoftBagGeneration(widthBound=$widthBound,connected=$requireConnectedCover,covers=${definitionCovers.size},candidates=${candidates.size})"
}

private[soft] object SoftOrdering {
  def compareVectors[A](left: Vector[A], right: Vector[A])(implicit
      ordering: Ordering[A]
  ): Int = {
    val commonLength = math.min(left.length, right.length)
    var index = 0
    while (index < commonLength) {
      val comparison = ordering.compare(left(index), right(index))
      if (comparison != 0) return comparison
      index += 1
    }
    Integer.compare(left.length, right.length)
  }
}
