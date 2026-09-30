package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{
  AdmissibleSoftBagGeneration,
  CandidateCover,
  ConnectedCover,
  EdgeCover
}
import at.ac.tuwien.dbai.yannakakis.core.{EdgeId, QueryHypergraph, VariableId}

import scala.collection.mutable

/** Strict, immutable boundary from soft-bag generation to CandidateTD search. */
final class CandidateSearchInput private (
    val hypergraph: QueryHypergraph,
    val widthBound: Int,
    val requireConnectedCover: Boolean,
    val candidateBags: Vector[CandidateBag]
) extends Serializable {
  private[candidate] val bagByKey: Map[CandidateBagKey, CandidateBag] =
    candidateBags.iterator.map(bag => bag.key -> bag).toMap

  def candidateBag(vertices: Iterable[VariableId]): Option[CandidateBag] = {
    if (vertices eq null) None
    else {
      val supplied = vertices.iterator.toVector
      if (supplied.exists(_ eq null)) None
      else bagByKey.get(CandidateBagKey.canonical(supplied))
    }
  }
}

object CandidateSearchInput {
  def create(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      generation: AdmissibleSoftBagGeneration
  ): Either[Vector[CandidateInputError], CandidateSearchInput] = {
    val errors = Vector.newBuilder[CandidateInputError]

    if (hypergraph eq null) {
      errors += CandidateInputError.MissingValue("candidate-search hypergraph")
    }
    if (widthBound < 1) {
      errors += CandidateInputError.InvalidWidthBound(widthBound)
    }
    if (generation eq null) {
      errors += CandidateInputError.MissingValue("admissible soft-bag generation")
    } else {
      if (generation.sourceHypergraph eq null) {
        errors += CandidateInputError.MissingValue(
          "admissible generation source hypergraph"
        )
      } else if (
        (hypergraph ne null) && generation.sourceHypergraph != hypergraph
      ) {
        errors += CandidateInputError.SourceHypergraphMismatch(
          hypergraph,
          generation.sourceHypergraph
        )
      }
      if (generation.widthBound != widthBound) {
        errors += CandidateInputError.SourceWidthMismatch(
          widthBound,
          generation.widthBound
        )
      }
    }

    if (hypergraph ne null) {
      if (hypergraph.isEmpty) errors += CandidateInputError.EmptyHypergraph
      val emptyEdges = hypergraph.edges.filter(_.vertices.isEmpty).map(_.id)
      if (emptyEdges.nonEmpty) {
        errors += CandidateInputError.EmptyHyperedges(emptyEdges)
      }
      if (hypergraph.variables.isEmpty) {
        errors += CandidateInputError.HypergraphWithoutVertices
      } else {
        VertexComponentEngine.components(hypergraph, Vector.empty) match {
          case Left(failure) =>
            errors += CandidateInputError.ComponentComputationFailure(
              failure.message
            )
          case Right(components) if components.size != 1 =>
            errors += CandidateInputError.DisconnectedHypergraph(
              components.map(_.vertices)
            )
          case Right(_) => ()
        }
      }
    }

    val definitionCovers =
      if ((generation eq null) || (generation.definitionCovers eq null)) {
        if (generation ne null) {
          errors += CandidateInputError.MissingValue("definition covers")
        }
        Vector.empty[EdgeCover]
      } else generation.definitionCovers

    val nonNullDefinitionCovers = definitionCovers.zipWithIndex.flatMap {
      case (cover, index) if cover eq null =>
        errors += CandidateInputError.MissingValue(s"definition cover $index")
        None
      case (cover, _) => Some(cover)
    }
    val actualDefinitionIds = nonNullDefinitionCovers.map(_.edgeIds)

    if (nonNullDefinitionCovers.size == definitionCovers.size) {
      val canonical = nonNullDefinitionCovers.distinct.sorted
      if (canonical != nonNullDefinitionCovers) {
        errors += CandidateInputError.NonCanonicalDefinitionCovers()
      }
    }

    if ((hypergraph ne null) && widthBound >= 1) {
      val knownIds = hypergraph.edgeIds.toSet
      actualDefinitionIds.foreach { ids =>
        val unknown = ids.filterNot(knownIds).sorted
        if (unknown.nonEmpty) {
          errors += CandidateInputError.InvalidDefinitionCover(
            ids,
            s"unknown edge ids ${unknown.mkString("[", ",", "]")}"
          )
        }
        if (ids.size > widthBound) {
          errors += CandidateInputError.InvalidDefinitionCover(
            ids,
            s"cover width ${ids.size} exceeds bound $widthBound"
          )
        }
      }

      // Once covers are canonical, unique, width-bounded subsets of known
      // edge ids, equality with the number of such subsets proves that the
      // universe is exhaustive without materializing it a second time.
      val expectedCount = definitionCoverCount(
        hypergraph.size,
        math.min(widthBound, hypergraph.size)
      )
      val actualCount = BigInt(actualDefinitionIds.distinct.size)
      if (actualCount != expectedCount) {
        errors += CandidateInputError.DefinitionCoverCountMismatch(
          expectedCount,
          actualCount
        )
      }
    }

    val rawCandidates =
      if ((generation eq null) || (generation.candidates eq null)) {
        if (generation ne null) {
          errors += CandidateInputError.MissingValue("admissible candidates")
        }
        Vector.empty
      } else generation.candidates

    val validRawCandidates = rawCandidates.zipWithIndex.flatMap {
      case (candidate, index) if candidate eq null =>
        errors += CandidateInputError.NullCandidate(index)
        None
      case (candidate, index) => Some(index -> candidate)
    }

    val safeVertices = validRawCandidates.map { case (index, candidate) =>
      val vertices =
        if (candidate.vertices eq null) {
          errors += CandidateInputError.MissingValue(
            s"candidate $index vertices"
          )
          Vector.empty[VariableId]
        } else candidate.vertices
      if (vertices.exists(_ eq null)) {
        errors += CandidateInputError.MissingValue(
          s"candidate $index vertex"
        )
      }
      val valid = vertices.filter(_ ne null)
      if (valid != valid.distinct.sorted || valid.size != vertices.size) {
        errors += CandidateInputError.NonCanonicalBagVertices(index, valid)
      }
      if (hypergraph ne null) {
        val unknown = valid.distinct.filterNot(hypergraph.variables.toSet).sorted
        if (unknown.nonEmpty) {
          errors += CandidateInputError.UnknownBagVertices(index, unknown)
        }
      }
      index -> valid
    }.toMap

    safeVertices.values
      .groupBy(identity)
      .collect { case (vertices, occurrences) if occurrences.size > 1 => vertices }
      .toVector
      .sortWith((left, right) =>
        CandidateOrdering.compareVectors(left, right)(VariableId.ordering) < 0
      )
      .foreach(vertices =>
        errors += CandidateInputError.DuplicateCandidateBag(vertices)
      )

    if (validRawCandidates.size == rawCandidates.size) {
      val suppliedOrder = validRawCandidates.map { case (index, _) =>
        safeVertices(index)
      }
      val canonicalOrder = suppliedOrder.sortWith((left, right) =>
        CandidateOrdering.compareVectors(left, right)(VariableId.ordering) < 0
      )
      if (suppliedOrder != canonicalOrder) {
        errors += CandidateInputError.NonCanonicalCandidateOrder()
      }
    }

    val definitionIdSet = actualDefinitionIds.toSet
    val candidateBags = Vector.newBuilder[CandidateBag]

    validRawCandidates.foreach { case (index, candidate) =>
      val vertices = safeVertices(index)
      if (
        (candidate.witnesses eq null) || candidate.witnesses.isEmpty ||
        candidate.witnesses.exists(_ eq null)
      ) {
        errors += CandidateInputError.MissingGenerationWitness(vertices)
      }

      val physical =
        if (candidate.physicalCovers eq null) {
          errors += CandidateInputError.MissingValue(
            s"candidate $index physical covers"
          )
          Vector.empty[CandidateCover]
        } else candidate.physicalCovers

      if (physical.isEmpty) {
        errors += CandidateInputError.MissingPhysicalCovers(vertices)
      }

      val nonNullPhysical = physical.zipWithIndex.flatMap {
        case (value, coverIndex) if value eq null =>
          errors += CandidateInputError.NullPhysicalCover(vertices, coverIndex)
          None
        case (value, coverIndex) if value.cover eq null =>
          errors += CandidateInputError.NullPhysicalCover(vertices, coverIndex)
          None
        case (value, _) => Some(value)
      }

      if (nonNullPhysical.size == physical.size) {
        val canonical = nonNullPhysical
          .groupBy(_.cover)
          .values
          .map(_.head)
          .toVector
          .sortBy(_.cover)(EdgeCover.ordering)
        if (canonical != nonNullPhysical) {
          errors += CandidateInputError.NonCanonicalPhysicalCovers(vertices)
        }
      }

      nonNullPhysical.foreach { candidateCover =>
        validatePhysicalCover(
          hypergraph,
          widthBound,
          generation,
          vertices,
          candidateCover,
          definitionIdSet,
          errors
        )
      }

      candidateBags += new CandidateBag(
        CandidateBagKey.canonical(vertices),
        nonNullPhysical.map(_.cover)
      )
    }

    val canonicalErrors = CandidateInputError.canonical(errors.result())
    if (canonicalErrors.nonEmpty) Left(canonicalErrors)
    else {
      Right(
        new CandidateSearchInput(
          hypergraph,
          widthBound,
          generation.requireConnectedCover,
          candidateBags.result()
        )
      )
    }
  }

  private def definitionCoverCount(
      edgeCount: Int,
      maximumSize: Int
  ): BigInt = {
    var total = BigInt(1)
    var binomial = BigInt(1)
    var size = 1
    while (size <= maximumSize) {
      binomial = binomial * (edgeCount - size + 1) / size
      total += binomial
      size += 1
    }
    total
  }

  private def validatePhysicalCover(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      generation: AdmissibleSoftBagGeneration,
      vertices: Vector[VariableId],
      candidateCover: CandidateCover,
      definitionIdSet: Set[Vector[EdgeId]],
      errors: mutable.Builder[CandidateInputError, Vector[CandidateInputError]]
  ): Unit = {
    val cover = candidateCover.cover
    val ids = cover.edgeIds

    if ((hypergraph eq null) || (generation eq null)) return

    val knownIds = hypergraph.edgeIds.toSet
    val unknown = ids.filterNot(knownIds).sorted
    if (cover.isEmpty) {
      errors += CandidateInputError.InvalidPhysicalCover(
        vertices,
        ids,
        "physical covers of a non-empty query must be non-empty"
      )
    }
    if (cover.size > widthBound) {
      errors += CandidateInputError.InvalidPhysicalCover(
        vertices,
        ids,
        s"cover width ${cover.size} exceeds bound $widthBound"
      )
    }
    if (unknown.nonEmpty) {
      errors += CandidateInputError.InvalidPhysicalCover(
        vertices,
        ids,
        s"unknown edge ids ${unknown.mkString("[", ",", "]")}"
      )
    }
    if (!definitionIdSet(ids)) {
      errors += CandidateInputError.PhysicalCoverNotDefined(vertices, ids)
    }

    if (unknown.isEmpty) {
      ConnectedCover.covers(hypergraph, cover, vertices) match {
        case Left(failure) =>
          errors += CandidateInputError.InvalidPhysicalCover(
            vertices,
            ids,
            failure.message
          )
        case Right(false) =>
          errors += CandidateInputError.BagNotCovered(vertices, ids)
        case Right(true) => ()
      }
      ConnectedCover.isConnected(hypergraph, cover) match {
        case Left(failure) =>
          errors += CandidateInputError.InvalidPhysicalCover(
            vertices,
            ids,
            failure.message
          )
        case Right(computed) =>
          if (computed != candidateCover.isConnected) {
            errors += CandidateInputError.ConnectivityMetadataMismatch(
              vertices,
              ids,
              candidateCover.isConnected,
              computed
            )
          }
          if (generation.requireConnectedCover && !computed) {
            errors += CandidateInputError.DisconnectedCoverInConnectedMode(
              vertices,
              ids
            )
          }
      }
    }
  }
}
