package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateDecision.{
  Accepted,
  Rejected
}

import scala.collection.mutable

/** Exact deterministic Boolean CandidateTD search in component normal form. */
object BooleanCandidateTD {
  def decide(
      input: CandidateSearchInput
  ): Either[CandidateFailure, CandidateDecision] =
    decide(input, CandidateResourceLimits.default)

  def decide(
      input: CandidateSearchInput,
      limits: CandidateResourceLimits
  ): Either[CandidateFailure, CandidateDecision] = {
    if (input eq null) {
      Left(CandidateFailure.MissingValue("candidate-search input"))
    } else if (limits eq null) {
      Left(CandidateFailure.MissingValue("candidate resource limits"))
    } else {
      CandidateBlockIndex
        .build(input, limits)
        .flatMap(index => decide(input, index, limits))
    }
  }

  private def decide(
      input: CandidateSearchInput,
      index: CandidateBlockIndex,
      limits: CandidateResourceLimits
  ): Either[CandidateFailure, CandidateDecision] = {
    val witnesses = mutable.LinkedHashMap.empty[
      CandidateBlock,
      BlockWitness
    ]

    // CandidateBlock.ordering starts with the exact decreasing rank
    // (|tail|, |head|), so every subordinate decision is already available.
    index.blocks.foreach { block =>
      if (block.tail.isEmpty) {
        witnesses.put(block, EmptyTailWitness)
      } else {
        bestTreeForBases(index.bases(block), witnesses).foreach(tree =>
          witnesses.put(block, TreeWitness(tree))
        )
      }
    }

    val statistics = CandidateStatistics(
      input.candidateBags.size,
      index.blocks.size,
      index.basisAlternativeCount,
      witnesses.size
    )

    bestTreeForBases(index.rootBases, witnesses) match {
      case None => Right(Rejected(statistics))
      case Some(root) =>
        if (root.nodeCount > BigInt(limits.maxWitnessNodes)) {
          Left(
            CandidateFailure.ResourceLimitExceeded(
              CandidateResource.WitnessNodes,
              root.nodeCount,
              limits.maxWitnessNodes
            )
          )
        } else {
          val witness = new CandidateTree(root)
          CandidatePhysicalizer.physicalize(input, witness).flatMap {
            decomposition =>
              CandidateDecompositionValidator.validate(input, decomposition) match {
                case Left(errors) =>
                  Left(CandidateFailure.CertificateValidation(errors))
                case Right(certificate) =>
                  Right(
                    Accepted(
                      witness,
                      decomposition,
                      certificate,
                      statistics
                    )
                  )
              }
          }
        }
    }
  }

  /**
    * Builds the globally best tree for one basis from already optimal child
    * witnesses. Node count is additive across independent subordinate blocks;
    * after equal counts, canonical child sorting makes recursive-signature
    * comparison exact as well.
    */
  private def treeFor(
      basis: CandidateBasis,
      witnesses: scala.collection.Map[CandidateBlock, BlockWitness]
  ): Option[CandidateTreeNode] = {
    val children = Vector.newBuilder[CandidateTreeNode]
    val iterator = basis.subordinateBlocks.iterator.filter(_.tail.nonEmpty)
    var satisfied = true

    while (iterator.hasNext && satisfied) {
      witnesses.get(iterator.next()) match {
        case Some(TreeWitness(child)) => children += child
        case _                        => satisfied = false
      }
    }

    if (!satisfied) None
    else {
      val canonicalChildren = children.result().sortWith { (left, right) =>
        CandidateOrdering.compareTreeNodes(left, right) < 0
      }
      Some(new CandidateTreeNode(basis.bag, canonicalChildren))
    }
  }

  private def bestTreeForBases(
      bases: Vector[CandidateBasis],
      witnesses: scala.collection.Map[CandidateBlock, BlockWitness]
  ): Option[CandidateTreeNode] = {
    var best: Option[CandidateTreeNode] = None
    bases.foreach { basis =>
      treeFor(basis, witnesses).foreach { candidate =>
        if (
          best.forall(current =>
            CandidateOrdering.compareRankedTrees(candidate, current) < 0
          )
        ) {
          best = Some(candidate)
        }
      }
    }
    best
  }

  private sealed trait BlockWitness
  private case object EmptyTailWitness extends BlockWitness
  private final case class TreeWitness(tree: CandidateTreeNode)
      extends BlockWitness
}
