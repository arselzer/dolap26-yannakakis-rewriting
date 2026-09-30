package at.ac.tuwien.dbai.yannakakis.algorithm.soft

import SoftTestSupport._

class EdgeComponentEngineSuite extends munit.FunSuite {
  test("chain components are computed after separator vertices are deleted") {
    val chain = graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 3)))

    assertEquals(
      components(chain, cover()),
      Vector(Vector(0, 1, 2))
    )
    assertEquals(
      components(chain, cover(1)),
      Vector(Vector(0), Vector(2))
    )
    assertEquals(
      components(chain, cover(0)),
      Vector(Vector(1, 2))
    )
  }

  test("triangle retains the two edges joined outside a separator") {
    val triangle = graph(Seq(Seq(0, 1), Seq(1, 2), Seq(2, 0)))

    assertEquals(
      components(triangle, cover(0)),
      Vector(Vector(1, 2))
    )
  }

  test("deleting one edge of a five-cycle leaves one edge component") {
    val cycle = graph(
      Seq(
        Seq(0, 1),
        Seq(1, 2),
        Seq(2, 3),
        Seq(3, 4),
        Seq(4, 0)
      )
    )

    assertEquals(
      components(cycle, cover(0)),
      Vector(Vector(1, 2, 3, 4))
    )
  }

  test("equal-vertex edge occurrences remain distinct inside a component") {
    val repeated = graph(Seq(Seq(0, 1), Seq(0, 1), Seq(1, 2)))

    assertEquals(
      components(repeated, cover()),
      Vector(Vector(0, 1, 2))
    )
    assertEquals(
      components(repeated, cover(2)),
      Vector(Vector(0, 1))
    )
  }

  test("singleton edges survive, empty and fully deleted edges do not") {
    val hypergraph = graph(Seq(Seq.empty, Seq(0)))

    assertEquals(components(hypergraph, cover()), Vector(Vector(1)))
    assertEquals(components(hypergraph, cover(0)), Vector(Vector(1)))
    assertEquals(components(hypergraph, cover(1)), Vector.empty)
  }

  test("an unknown separator occurrence is rejected") {
    val hypergraph = graph(Seq(Seq(0)))

    assertEquals(
      EdgeComponentEngine.components(hypergraph, cover(99)),
      Left(SoftFailure.UnknownEdgeIds(Vector(edgeId(99))))
    )
  }

  private def components(
      hypergraph: at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph,
      separator: EdgeCover
  ): Vector[Vector[Int]] =
    EdgeComponentEngine
      .components(hypergraph, separator)
      .toOption
      .get
      .map(edgeValues)
}
