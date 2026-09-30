package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.core.VariableId

import scala.collection.mutable

/**
  * Canonical materialization of component-normal-form blocks and exact bases.
  *
  * For a block `(S,C)`, a candidate `X` is considered only when `X != S` and
  * `X subseteq S union C`. Its subordinate blocks are exactly the `(X,Y)` with
  * `X union Y subseteq S union C` and `Y subseteq C`. The basis is retained
  * iff their represented region covers `C` and contains every hyperedge that
  * intersects `C`.
  */
final class CandidateBlockIndex private (
    val blocks: Vector[CandidateBlock],
    val basesByBlock: Map[CandidateBlock, Vector[CandidateBasis]],
    val rootBases: Vector[CandidateBasis],
    val basisAlternativeCount: Long
) extends Serializable {
  def bases(block: CandidateBlock): Vector[CandidateBasis] =
    basesByBlock.getOrElse(block, Vector.empty)

}

object CandidateBlockIndex {
  def build(
      input: CandidateSearchInput
  ): Either[CandidateFailure, CandidateBlockIndex] =
    build(input, CandidateResourceLimits.default)

  def build(
      input: CandidateSearchInput,
      limits: CandidateResourceLimits
  ): Either[CandidateFailure, CandidateBlockIndex] = {
    if (input eq null) {
      Left(CandidateFailure.MissingValue("candidate-search input"))
    } else if (limits eq null) {
      Left(CandidateFailure.MissingValue("candidate resource limits"))
    } else {
      buildBlocks(input, limits).flatMap { blocks =>
        val requiredBasisChecks =
          BigInt(blocks.count(_.tail.nonEmpty) + 1) * input.candidateBags.size
        if (requiredBasisChecks > BigInt(limits.maxBasisChecks)) {
          return Left(
            CandidateFailure.ResourceLimitExceeded(
              CandidateResource.BasisChecks,
              requiredBasisChecks,
              limits.maxBasisChecks
            )
          )
        }

        val byHead = blocks.groupBy(_.head).view.mapValues(_.sorted).toMap
        val bases = mutable.LinkedHashMap.empty[
          CandidateBlock,
          Vector[CandidateBasis]
        ]
        val alternativeBudget = new AlternativeBudget(
          limits.maxBasisAlternatives
        )

        val blockIterator = blocks.iterator
        var failure: Option[CandidateFailure] = None
        while (blockIterator.hasNext && failure.isEmpty) {
          val block = blockIterator.next()
          if (block.tail.isEmpty) {
            bases.put(block, Vector.empty)
          } else {
            alternativesFor(
              input,
              block,
              byHead,
              alternativeBudget
            ) match {
              case Left(problem) => failure = Some(problem)
              case Right(alternatives) => bases.put(block, alternatives)
            }
          }
        }

        failure match {
          case Some(problem) => Left(problem)
          case None =>
            rootAlternatives(input, byHead, alternativeBudget).map { roots =>
              new CandidateBlockIndex(
                blocks,
                bases.toMap,
                roots,
                alternativeBudget.used
              )
            }
        }
      }
    }
  }

  private def buildBlocks(
      input: CandidateSearchInput,
      limits: CandidateResourceLimits
  ): Either[CandidateFailure, Vector[CandidateBlock]] = {
    if (BigInt(input.candidateBags.size) > BigInt(limits.maxBlocks)) {
      return Left(
        CandidateFailure.ResourceLimitExceeded(
          CandidateResource.Blocks,
          input.candidateBags.size,
          limits.maxBlocks
        )
      )
    }

    val result = Vector.newBuilder[CandidateBlock]
    val iterator = input.candidateBags.iterator
    var failure: Option[CandidateFailure] = None
    var blockCount = BigInt(0)

    while (iterator.hasNext && failure.isEmpty) {
      val bag = iterator.next()
      VertexComponentEngine.components(input.hypergraph, bag.vertices) match {
        case Left(problem) => failure = Some(problem)
        case Right(components) =>
          val required = blockCount + components.size + 1
          if (required > BigInt(limits.maxBlocks)) {
            failure = Some(
              CandidateFailure.ResourceLimitExceeded(
                CandidateResource.Blocks,
                required,
                limits.maxBlocks
              )
            )
          } else {
            result += new CandidateBlock(bag.key, CandidateComponent.empty)
            components.foreach(component =>
              result += new CandidateBlock(bag.key, component)
            )
            blockCount = required
          }
      }
    }

    failure match {
      case Some(problem) => Left(problem)
      case None          => Right(result.result().distinct.sorted)
    }
  }

  private def alternativesFor(
      input: CandidateSearchInput,
      parent: CandidateBlock,
      blocksByHead: Map[CandidateBagKey, Vector[CandidateBlock]],
      alternativeBudget: AlternativeBudget
  ): Either[CandidateFailure, Vector[CandidateBasis]] = {
    val result = Vector.newBuilder[CandidateBasis]
    val iterator = input.candidateBags.iterator
    var failure: Option[CandidateFailure] = None

    while (iterator.hasNext && failure.isEmpty) {
      val basisBag = iterator.next().key
      if (
        basisBag != parent.head &&
        basisBag.vertexSet.subsetOf(parent.extent)
      ) {
        val subordinate = blocksByHead
          .getOrElse(basisBag, Vector.empty)
          .filter(child => isSubordinate(child, parent))
          .sorted

        if (isExactBasis(input, parent.tail.vertexSet, basisBag, subordinate)) {
          subordinate.find(child => !rankDecreases(parent, child)) match {
            case Some(child) =>
              failure = Some(
                CandidateFailure.RankInvariantViolation(parent, child)
              )
            case None =>
              alternativeBudget.consume() match {
                case Left(problem) => failure = Some(problem)
                case Right(_) =>
                  result += new CandidateBasis(basisBag, subordinate)
              }
          }
        }
      }
    }

    failure match {
      case Some(problem) => Left(problem)
      case None          => Right(sortBases(result.result()))
    }
  }

  private def rootAlternatives(
      input: CandidateSearchInput,
      blocksByHead: Map[CandidateBagKey, Vector[CandidateBlock]],
      alternativeBudget: AlternativeBudget
  ): Either[CandidateFailure, Vector[CandidateBasis]] = {
    val allVertices = input.hypergraph.variables.toSet
    val result = Vector.newBuilder[CandidateBasis]
    val iterator = input.candidateBags.iterator
    var failure: Option[CandidateFailure] = None

    while (iterator.hasNext && failure.isEmpty) {
      val basisBag = iterator.next().key
      // The virtual root separator is empty, so X != S excludes an empty bag.
      if (basisBag.vertices.nonEmpty && basisBag.vertexSet.subsetOf(allVertices)) {
        val subordinate = blocksByHead
          .getOrElse(basisBag, Vector.empty)
          .filter { child =>
            child.extent.subsetOf(allVertices) &&
            child.tail.vertexSet.subsetOf(allVertices)
          }
          .sorted

        if (isExactBasis(input, allVertices, basisBag, subordinate)) {
          subordinate.find(child => !rootRankDecreases(allVertices, child)) match {
            case Some(child) =>
              // A concrete diagnostic parent is unnecessary here: with a
              // non-empty X, a child cannot retain the virtual root rank.
              failure = Some(
                CandidateFailure.RootRankInvariantViolation(
                  allVertices.toVector.sorted,
                  child
                )
              )
            case None =>
              alternativeBudget.consume() match {
                case Left(problem) => failure = Some(problem)
                case Right(_) =>
                  result += new CandidateBasis(basisBag, subordinate)
              }
          }
        }
      }
    }

    failure match {
      case Some(problem) => Left(problem)
      case None          => Right(sortBases(result.result()))
    }
  }

  private def isSubordinate(
      child: CandidateBlock,
      parent: CandidateBlock
  ): Boolean =
    child.extent.subsetOf(parent.extent) &&
      child.tail.vertexSet.subsetOf(parent.tail.vertexSet)

  private def isExactBasis(
      input: CandidateSearchInput,
      requiredTail: Set[VariableId],
      basisBag: CandidateBagKey,
      subordinate: Vector[CandidateBlock]
  ): Boolean = {
    val represented = subordinate.iterator.foldLeft(basisBag.vertexSet) {
      (vertices, block) => vertices union block.tail.vertexSet
    }
    val coversTail = requiredTail.subsetOf(represented)
    val coversIncidentEdges = input.hypergraph.edges.forall { edge =>
      val intersectsTail = edge.vertices.exists(requiredTail)
      !intersectsTail || edge.vertices.forall(represented)
    }
    coversTail && coversIncidentEdges
  }

  private def rankDecreases(
      parent: CandidateBlock,
      child: CandidateBlock
  ): Boolean = {
    val childTailSize = child.tail.vertices.size
    val parentTailSize = parent.tail.vertices.size
    childTailSize < parentTailSize ||
    (childTailSize == parentTailSize &&
      child.head.vertices.size < parent.head.vertices.size)
  }

  private def rootRankDecreases(
      allVertices: Set[VariableId],
      child: CandidateBlock
  ): Boolean =
    child.tail.vertices.size < allVertices.size

  private def sortBases(
      bases: Vector[CandidateBasis]
  ): Vector[CandidateBasis] =
    bases.sortWith { (left, right) =>
      val bag = CandidateBagKey.ordering.compare(left.bag, right.bag)
      if (bag != 0) bag < 0
      else
        CandidateOrdering.compareVectors(
          left.subordinateBlocks,
          right.subordinateBlocks
        )(CandidateBlock.ordering) < 0
    }

  private final class AlternativeBudget(limit: Long) {
    private var materialized = 0L
    def used: Long = materialized

    def consume(): Either[CandidateFailure, Unit] = {
      val required = BigInt(materialized) + 1
      if (required > BigInt(limit)) {
        Left(
          CandidateFailure.ResourceLimitExceeded(
            CandidateResource.BasisAlternatives,
            required,
            limit
          )
        )
      } else {
        materialized += 1L
        Right(())
      }
    }
  }
}
