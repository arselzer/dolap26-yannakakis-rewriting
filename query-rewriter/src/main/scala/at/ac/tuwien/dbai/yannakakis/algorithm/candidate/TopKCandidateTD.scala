package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision.{
  Found,
  Rejected
}

import scala.collection.mutable

/**
  * Exact structural top-K CandidateTD enumeration.
  *
  * Ranking is by node count and canonical rooted-tree signature. Physical
  * output uses one canonical-first cover assignment; cover-distinct and
  * cost-ranked enumeration require a richer future DP state.
  */
object TopKCandidateTD {
  def enumerate(
      input: CandidateSearchInput,
      planLimit: Int
  ): Either[CandidateFailure, CandidatePlanDecision] =
    enumerate(input, planLimit, CandidateResourceLimits.default)

  def enumerate(
      input: CandidateSearchInput,
      planLimit: Int,
      limits: CandidateResourceLimits
  ): Either[CandidateFailure, CandidatePlanDecision] = {
    if (input eq null) {
      Left(CandidateFailure.MissingValue("candidate-search input"))
    } else if (planLimit <= 0) {
      Left(CandidateFailure.InvalidPlanLimit(planLimit))
    } else if (limits eq null) {
      Left(CandidateFailure.MissingValue("candidate resource limits"))
    } else if (BigInt(planLimit) > BigInt(limits.maxPlanLimit)) {
      Left(
        CandidateFailure.ResourceLimitExceeded(
          CandidateResource.PlanLimit,
          planLimit,
          limits.maxPlanLimit
        )
      )
    } else {
      CandidateBlockIndex.build(input, limits).flatMap { index =>
        enumerate(input, planLimit, limits, index)
      }
    }
  }

  private def enumerate(
      input: CandidateSearchInput,
      planLimit: Int,
      limits: CandidateResourceLimits,
      index: CandidateBlockIndex
  ): Either[CandidateFailure, CandidatePlanDecision] = {
    val budget = new CandidateTopKBudget(limits)
    val frontiers = mutable.LinkedHashMap.empty[
      CandidateBlock,
      Vector[CandidateTreeNode]
    ]
    val blocks = index.blocks.iterator
    var failure: Option[CandidateFailure] = None

    while (blocks.hasNext && failure.isEmpty) {
      val block = blocks.next()
      if (block.tail.nonEmpty) {
        frontierForBases(
          index.bases(block),
          frontiers,
          planLimit,
          budget
        ) match {
          case Left(problem) => failure = Some(problem)
          case Right(frontier) =>
            budget.retainPartialPlans(frontier.size) match {
              case Left(problem) => failure = Some(problem)
              case Right(_)      => frontiers.put(block, frontier)
            }
        }
      }
    }

    failure match {
      case Some(problem) => Left(problem)
      case None =>
        frontierForBases(
          index.rootBases,
          frontiers,
          planLimit,
          budget
        ).flatMap { roots =>
          budget.retainPartialPlans(roots.size).flatMap { _ =>
            val baseStatistics = CandidateStatistics(
              input.candidateBags.size,
              index.blocks.size,
              index.basisAlternativeCount,
              index.blocks.count(_.tail.isEmpty) +
                frontiers.valuesIterator.count(_.nonEmpty)
            )
            val statistics = CandidateTopKStatistics(
              baseStatistics,
              budget.retainedPartialPlanCount,
              budget.combinationStateCount
            )

            if (roots.isEmpty) {
              Right(Rejected(planLimit, statistics))
            } else {
              enforceWitnessLimit(roots, limits).flatMap { _ =>
                physicalize(input, roots, planLimit, statistics)
              }
            }
          }
        }
    }
  }

  private def frontierForBases(
      bases: Vector[CandidateBasis],
      frontiers: scala.collection.Map[
        CandidateBlock,
        Vector[CandidateTreeNode]
      ],
      planLimit: Int,
      budget: CandidateTopKBudget
  ): Either[CandidateFailure, Vector[CandidateTreeNode]] = {
    val globalBest = mutable.HashSet.empty[CandidateTreeNode]
    val iterator = bases.iterator
    var failure: Option[CandidateFailure] = None

    while (iterator.hasNext && failure.isEmpty) {
      val basis = iterator.next()
      val childFrontiers = basis.subordinateBlocks
        .filter(_.tail.nonEmpty)
        .map(block => frontiers.getOrElse(block, Vector.empty))

      TopKCombinationEnumerator.enumerate(
        basis,
        childFrontiers,
        planLimit,
        budget
      ) match {
        case Left(problem) => failure = Some(problem)
        case Right(perBasis) =>
          perBasis.foreach(plan => retainGlobal(plan, globalBest, planLimit))
      }
    }

    failure match {
      case Some(problem) => Left(problem)
      case None =>
        Right(
          globalBest.toVector.sortWith((left, right) =>
            CandidateOrdering.compareRankedTrees(left, right) < 0
          )
        )
    }
  }

  private def retainGlobal(
      plan: CandidateTreeNode,
      retained: mutable.HashSet[CandidateTreeNode],
      planLimit: Int
  ): Unit = {
    if (retained.add(plan) && retained.size > planLimit) {
      val worst = retained.max(treeOrdering)
      retained -= worst
    }
  }

  private def enforceWitnessLimit(
      roots: Vector[CandidateTreeNode],
      limits: CandidateResourceLimits
  ): Either[CandidateFailure, Unit] = {
    val required = roots.iterator.map(_.nodeCount).max
    if (required > BigInt(limits.maxWitnessNodes)) {
      Left(
        CandidateFailure.ResourceLimitExceeded(
          CandidateResource.WitnessNodes,
          required,
          limits.maxWitnessNodes
        )
      )
    } else Right(())
  }

  private def physicalize(
      input: CandidateSearchInput,
      roots: Vector[CandidateTreeNode],
      requestedLimit: Int,
      statistics: CandidateTopKStatistics
  ): Either[CandidateFailure, CandidatePlanDecision] = {
    val plans = Vector.newBuilder[RankedCandidatePlan]
    val iterator = roots.iterator.zipWithIndex
    var failure: Option[CandidateFailure] = None

    while (iterator.hasNext && failure.isEmpty) {
      val (root, index) = iterator.next()
      val witness = new CandidateTree(root)
      CandidatePhysicalizer.physicalize(input, witness) match {
        case Left(problem) => failure = Some(problem)
        case Right(decomposition) =>
          CandidateDecompositionValidator.validate(input, decomposition) match {
            case Left(errors) =>
              failure = Some(CandidateFailure.CertificateValidation(errors))
            case Right(certificate) =>
              plans += RankedCandidatePlan(
                rank = index + 1,
                candidateId = CandidatePlanId.from(decomposition),
                score = StructuralPlanScore(root.nodeCount),
                witness = witness,
                decomposition = decomposition,
                certificate = certificate
              )
          }
      }
    }

    failure match {
      case Some(problem) => Left(problem)
      case None => Right(Found(plans.result(), requestedLimit, statistics))
    }
  }

  private val treeOrdering: Ordering[CandidateTreeNode] =
    (left: CandidateTreeNode, right: CandidateTreeNode) =>
      CandidateOrdering.compareRankedTrees(left, right)
}
