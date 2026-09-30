package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftFailure._
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftResource.Covers
import SoftTestSupport._

class CoverEnumeratorSuite extends munit.FunSuite {
  test("enumerates all independent edge subsets in canonical order") {
    val hypergraph = graphWithIds(
      Seq(30 -> Seq(0), 10 -> Seq(1), 20 -> Seq(2))
    )

    val covers = CoverEnumerator.enumerate(hypergraph, widthBound = 2).toOption.get

    assertEquals(
      covers.map(_.edgeIds.map(_.value)),
      Vector(
        Vector.empty,
        Vector(10),
        Vector(20),
        Vector(30),
        Vector(10, 20),
        Vector(10, 30),
        Vector(20, 30)
      )
    )
  }

  test("repeated equal-vertex edges remain distinct choices") {
    val hypergraph = graph(Seq(Seq(0, 1), Seq(0, 1)))
    val covers = CoverEnumerator.enumerate(hypergraph, 1).toOption.get

    assertEquals(
      covers.map(_.edgeIds.map(_.value)),
      Vector(Vector.empty, Vector(0), Vector(1))
    )
  }

  test("reports width and cover-count bounds without partial output") {
    val hypergraph = graph(Seq(Seq(0), Seq(1), Seq(2)))
    val limits = SoftResourceLimits.create(maxCovers = 6).toOption.get

    assertEquals(
      CoverEnumerator.enumerate(hypergraph, 0),
      Left(InvalidWidthBound(0))
    )
    assertEquals(
      CoverEnumerator.enumerate(hypergraph, 2, limits),
      Left(ResourceLimitExceeded(Covers, required = 7, limit = 6))
    )
  }

  test("validates resource limits structurally") {
    val errors = SoftResourceLimits
      .create(maxCovers = 0, maxCandidates = -1)
      .swap
      .toOption
      .get

    assertEquals(
      errors,
      Vector(
        InvalidResourceLimit(SoftResource.Covers, 0),
        InvalidResourceLimit(SoftResource.Candidates, -1)
      )
    )
  }
}
