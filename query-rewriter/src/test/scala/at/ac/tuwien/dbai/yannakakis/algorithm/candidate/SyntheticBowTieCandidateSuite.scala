package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision.{
  Found,
  Rejected
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import CandidateTDOracleTestSupport.{graph, variable}

/** Structural gate for the synthetic two-cycle articulation topology. */
class SyntheticBowTieCandidateSuite extends munit.FunSuite {
  private val bowTie = graph(
    Seq(0 -> 1, 1 -> 2, 2 -> 0, 0 -> 3, 3 -> 4, 4 -> 0).map {
      case (left, right) => Seq(left, right)
    }
  )

  test("bow-tie rejects width one and retains multi-bag width-two connected plans") {
    val widthOne = enumerate(bowTie, widthBound = 1, connected = true, limit = 10)
    val widthTwo = enumerate(bowTie, widthBound = 2, connected = true, limit = 10)

    assert(widthOne.isInstanceOf[Rejected], clues(widthOne))
    val found = widthTwo match {
      case accepted: Found => accepted
      case rejected: Rejected =>
        fail(s"expected width-two connected-cover plans, received $rejected")
    }
    assert(found.plans.size > 1, clues(found.statistics))
    assertEquals(found.plans.map(_.rank), (1 to found.plans.size).toVector)
    found.plans.foreach { plan =>
      val decomposition = plan.decomposition
      assertEquals(decomposition.width, 2, clues(plan.rank, decomposition))
      assert(decomposition.bags.size > 1, clues(plan.rank, decomposition))
      assert(decomposition.treeEdges.nonEmpty, clues(plan.rank, decomposition))
      assert(
        decomposition.bags.count(_.vertices.contains(variable(0))) >= 2,
        clues(plan.rank, decomposition)
      )
    }
  }

  private def enumerate(
      source: QueryHypergraph,
      widthBound: Int,
      connected: Boolean,
      limit: Int
  ): CandidatePlanDecision = {
    val generation = SoftBagGenerator
      .generate(source, widthBound)
      .fold(failure => throw new AssertionError(failure.message), identity)
      .admissible(requireConnectedCover = connected)
    val input = CandidateSearchInput
      .create(source, widthBound, generation)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )
    TopKCandidateTD
      .enumerate(input, limit)
      .fold(failure => throw new AssertionError(failure.message), identity)
  }
}
