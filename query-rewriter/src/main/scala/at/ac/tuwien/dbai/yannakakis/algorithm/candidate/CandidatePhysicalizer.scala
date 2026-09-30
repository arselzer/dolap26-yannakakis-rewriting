package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.core.BagId
import at.ac.tuwien.dbai.yannakakis.core.decomposition.{
  BagTreeEdge,
  CoveredBag,
  CoveredDecomposition
}

import scala.collection.mutable

/** Canonical first-cover physicalization of a structural Boolean witness. */
private[candidate] object CandidatePhysicalizer {
  def physicalize(
      input: CandidateSearchInput,
      tree: CandidateTree
  ): Either[CandidateFailure, CoveredDecomposition] = {
    if (input eq null) {
      Left(CandidateFailure.MissingValue("candidate-search input"))
    } else if (tree eq null) {
      Left(CandidateFailure.MissingValue("candidate tree"))
    } else {
      val bags = Vector.newBuilder[CoveredBag]
      val edges = Vector.newBuilder[BagTreeEdge]
      val pending = mutable.Stack[(CandidateTreeNode, Option[BagId])](
        tree.root -> None
      )
      var nextId = 0L
      var rootId: Option[BagId] = None
      var failure: Option[CandidateFailure] = None

      while (pending.nonEmpty && failure.isEmpty) {
        val (node, parent) = pending.pop()
        if (nextId > Int.MaxValue.toLong) {
          failure = Some(
            CandidateFailure.CoreModelConstruction(
              "covered decomposition",
              Vector("candidate tree exceeds the BagId identity range")
            )
          )
        } else {
          BagId.from(nextId.toInt) match {
            case Left(problem) =>
              failure = Some(
                CandidateFailure.CoreModelConstruction(
                  "bag id",
                  Vector(problem.message)
                )
              )
            case Right(bagId) =>
              input.bagByKey.get(node.bag) match {
                case None =>
                  failure = Some(
                    CandidateFailure.MissingPhysicalCandidate(node.bag)
                  )
                case Some(candidate) if candidate.physicalCovers.isEmpty =>
                  failure = Some(
                    CandidateFailure.MissingPhysicalCover(node.bag)
                  )
                case Some(candidate) =>
                  CoveredBag.create(
                    bagId,
                    node.bag.vertices,
                    candidate.physicalCovers.head.edgeIds
                  ) match {
                    case Left(problems) =>
                      failure = Some(
                        CandidateFailure.CoreModelConstruction(
                          "covered bag",
                          problems.map(_.message)
                        )
                      )
                    case Right(bag) =>
                      bags += bag
                      if (rootId.isEmpty) rootId = Some(bagId)
                      parent.foreach { parentId =>
                        BagTreeEdge.create(parentId, bagId) match {
                          case Left(problems) =>
                            failure = Some(
                              CandidateFailure.CoreModelConstruction(
                                "bag-tree edge",
                                problems.map(_.message)
                              )
                            )
                          case Right(edge) => edges += edge
                        }
                      }

                      // Stack reversal preserves canonical child order in
                      // preorder, hence canonical BagId assignment.
                      node.children.reverseIterator.foreach(child =>
                        pending.push(child -> Some(bagId))
                      )
                      nextId += 1L
                  }
              }
          }
        }
      }

      failure match {
        case Some(problem) => Left(problem)
        case None =>
          rootId match {
            case None =>
              Left(
                CandidateFailure.CoreModelConstruction(
                  "covered decomposition",
                  Vector("candidate tree has no root")
                )
              )
            case Some(root) =>
              CoveredDecomposition.create(root, bags.result(), edges.result())
                .left
                .map(problems =>
                  CandidateFailure.CoreModelConstruction(
                    "covered decomposition",
                    problems.map(_.message)
                  )
                )
          }
      }
    }
  }
}
