package at.ac.tuwien.dbai.yannakakis.algorithm.candidate

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanDecision.{
  Found,
  Rejected
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGenerator
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import CandidateTDOracleTestSupport.graph

/** Deterministic structural stress cases; no wall-clock expectations. */
class CandidateHardnessSuite extends munit.FunSuite {
  test("C4 is accepted at width two with connected-cover admissibility") {
    val result = enumerate(cycle(4), widthBound = 2, connected = true, limit = 10)
    val found = assertFound(result)

    assert(found.plans.nonEmpty)
    assert(found.plans.head.decomposition.bags.size > 1)
  }

  test("C6 exposes the connected-cover width boundary") {
    val source = cycle(6)
    val connectedWidthTwo = enumerate(
      source,
      widthBound = 2,
      connected = true,
      limit = 10
    )
    val unconstrainedWidthTwo = assertFound(
      enumerate(source, widthBound = 2, connected = false, limit = 10)
    )
    val connectedWidthThree = assertFound(
      enumerate(source, widthBound = 3, connected = true, limit = 10)
    )

    assert(connectedWidthTwo.isInstanceOf[Rejected], clues(connectedWidthTwo))
    assert(unconstrainedWidthTwo.plans.nonEmpty)
    assert(unconstrainedWidthTwo.plans.head.decomposition.bags.size > 1)
    assert(connectedWidthThree.plans.nonEmpty)
    assert(connectedWidthThree.plans.head.decomposition.bags.size > 1)
  }

  test("C12 exact top-K has a stable prefix and exercises combination search") {
    val input = candidateInput(cycle(12), widthBound = 2, connected = false)
    val firstTen = assertFound(enumerate(input, limit = 10))
    val firstHundred = assertFound(enumerate(input, limit = 100))

    assertEquals(firstTen.plans.size, 10)
    assertEquals(firstHundred.plans.size, 100)
    assertEquals(firstHundred.plans.take(firstTen.plans.size), firstTen.plans)
    assert(
      firstHundred.statistics.combinationStateCount >= 100000L,
      clues(firstHundred.statistics)
    )
    assert(
      firstHundred.statistics.retainedPartialPlanCount > firstHundred.plans.size,
      clues(firstHundred.statistics)
    )
  }

  private def cycle(length: Int): QueryHypergraph = {
    require(length >= 3, "cycle length must be at least three")
    graph((0 until length).map(index => Seq(index, (index + 1) % length)))
  }

  private def candidateInput(
      source: QueryHypergraph,
      widthBound: Int,
      connected: Boolean
  ): CandidateSearchInput = {
    val generation = SoftBagGenerator
      .generate(source, widthBound)
      .fold(failure => throw new AssertionError(failure.message), identity)
      .admissible(requireConnectedCover = connected)
    CandidateSearchInput
      .create(source, widthBound, generation)
      .fold(
        errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
        identity
      )
  }

  private def enumerate(
      source: QueryHypergraph,
      widthBound: Int,
      connected: Boolean,
      limit: Int
  ): CandidatePlanDecision =
    enumerate(candidateInput(source, widthBound, connected), limit)

  private def enumerate(
      input: CandidateSearchInput,
      limit: Int
  ): CandidatePlanDecision =
    TopKCandidateTD
      .enumerate(input, limit)
      .fold(failure => throw new AssertionError(failure.message), identity)

  private def assertFound(decision: CandidatePlanDecision): Found =
    decision match {
      case found: Found => found
      case rejected: Rejected =>
        throw new AssertionError(s"expected retained candidates, got $rejected")
    }
}
