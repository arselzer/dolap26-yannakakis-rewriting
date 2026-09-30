package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftFailure.ResourceLimitExceeded
import at.ac.tuwien.dbai.yannakakis.core.{QueryHypergraph, VariableId}

import scala.collection.mutable

/** Exhaustive fixed-k generation of the structural set Soft(H,k). */
object SoftBagGenerator {
  def generate(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      limits: SoftResourceLimits = SoftResourceLimits.default
  ): Either[SoftFailure, SoftBagGeneration] = {
    CoverEnumerator.enumerate(hypergraph, widthBound, limits).flatMap { covers =>
      val pairCount = BigInt(covers.size) * covers.size
      if (pairCount > BigInt(limits.maxCoverPairs)) {
        Left(
          ResourceLimitExceeded(
            SoftResource.CoverPairs,
            pairCount,
            limits.maxCoverPairs
          )
        )
      } else {
        generateFromCovers(hypergraph, widthBound, covers, limits)
      }
    }
  }

  private def generateFromCovers(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      covers: Vector[EdgeCover],
      limits: SoftResourceLimits
  ): Either[SoftFailure, SoftBagGeneration] = {
    val componentsByLambda2 = Vector.newBuilder[(EdgeCover, Vector[EdgeComponent])]
    var componentCount = BigInt(0)
    var componentFailure: Option[SoftFailure] = None

    val iterator = covers.iterator
    while (iterator.hasNext && componentFailure.isEmpty) {
      val lambda2 = iterator.next()
      EdgeComponentEngine.components(hypergraph, lambda2) match {
        case Left(failure) => componentFailure = Some(failure)
        case Right(components) =>
          componentCount += components.size
          if (componentCount > BigInt(limits.maxComponents)) {
            componentFailure = Some(
              ResourceLimitExceeded(
                SoftResource.Components,
                componentCount,
                limits.maxComponents
              )
            )
          } else componentsByLambda2 += lambda2 -> components
      }
    }

    componentFailure match {
      case Some(failure) => Left(failure)
      case None =>
        val derivationCount = componentCount * covers.size
        if (derivationCount > BigInt(limits.maxDerivations)) {
          Left(
            ResourceLimitExceeded(
              SoftResource.Derivations,
              derivationCount,
              limits.maxDerivations
            )
          )
        } else {
          buildCandidates(
            hypergraph,
            widthBound,
            covers,
            componentsByLambda2.result(),
            derivationCount,
            limits
          )
        }
    }
  }

  private def buildCandidates(
      hypergraph: QueryHypergraph,
      widthBound: Int,
      covers: Vector[EdgeCover],
      componentsByLambda2: Vector[(EdgeCover, Vector[EdgeComponent])],
      derivationCount: BigInt,
      limits: SoftResourceLimits
  ): Either[SoftFailure, SoftBagGeneration] = {
    val verticesByCover = covers.iterator
      .map(cover => cover -> ConnectedCover.unionVertices(hypergraph, cover))
      .toMap
    // The empty subset participates in Definition 3, but is not a physical
    // cover alternative for an ordinary nonempty query. It remains available
    // for the explicit empty-hypergraph sentinel.
    val physicalCovers =
      if (hypergraph.isEmpty) covers else covers.filter(_.nonEmpty)
    val witnessesByVertices = mutable.Map.empty[
      Vector[VariableId],
      mutable.ArrayBuffer[SoftBagWitness]
    ]

    var failure: Option[SoftFailure] = None
    val componentIterator = componentsByLambda2.iterator
    while (componentIterator.hasNext && failure.isEmpty) {
      val (lambda2, components) = componentIterator.next()
      components.foreach { component =>
        if (failure.isEmpty) {
          val componentVertices = component.edgeIds.iterator
            .flatMap(edgeId => hypergraph.edge(edgeId).iterator)
            .flatMap(_.vertices)
            .toSet
          covers.foreach { lambda1 =>
            if (failure.isEmpty) {
              val bag = verticesByCover(lambda1)
                .intersect(componentVertices)
                .toVector
                .sorted
              val firstOccurrence = !witnessesByVertices.contains(bag)
              if (
                firstOccurrence &&
                BigInt(witnessesByVertices.size + 1) > BigInt(limits.maxCandidates)
              ) {
                failure = Some(
                  ResourceLimitExceeded(
                    SoftResource.Candidates,
                    witnessesByVertices.size + 1,
                    limits.maxCandidates
                  )
                )
              } else {
                val witness = SoftBagWitness(lambda1, lambda2, component)
                witnessesByVertices
                  .getOrElseUpdate(bag, mutable.ArrayBuffer.empty)
                  .append(witness)
              }
            }
          }
        }
      }
    }

    failure match {
      case Some(problem) => Left(problem)
      case None =>
        val coverChecks = BigInt(witnessesByVertices.size) * physicalCovers.size
        if (coverChecks > BigInt(limits.maxCandidateCoverChecks)) {
          Left(
            ResourceLimitExceeded(
              SoftResource.CandidateCoverChecks,
              coverChecks,
              limits.maxCandidateCoverChecks
            )
          )
        } else {
          val connectedByCover = physicalCovers.iterator
            .map(cover => cover -> ConnectedCover.computeConnectivity(hypergraph, cover))
            .toMap
          val candidates = witnessesByVertices.iterator.map {
            case (vertices, witnesses) =>
              val bagSet = vertices.toSet
              val candidateCovers = physicalCovers.collect {
                case cover if bagSet.subsetOf(verticesByCover(cover)) =>
                  CandidateCover(cover, connectedByCover(cover))
              }
              new SoftBagCandidate(
                vertices,
                witnesses.toVector.sorted,
                candidateCovers
              )
          }.toVector.sorted
          // The prior Long-valued derivation limit bounds every materialized
          // witness. Keep the consistency check in BigInt to avoid narrowing.
          val materializedDerivations = candidates.iterator.foldLeft(BigInt(0)) {
            (total, candidate) => total + candidate.witnesses.size
          }
          assert(
            materializedDerivations == derivationCount
          )
          Right(
            new SoftBagGeneration(
              hypergraph,
              widthBound,
              covers,
              candidates
            )
          )
        }
    }
  }
}
