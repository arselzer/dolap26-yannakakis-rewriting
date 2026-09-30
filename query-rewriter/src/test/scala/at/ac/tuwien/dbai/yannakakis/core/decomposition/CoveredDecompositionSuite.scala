package at.ac.tuwien.dbai.yannakakis.core.decomposition

import at.ac.tuwien.dbai.yannakakis.core.CoreModelTestSupport.{
  bagId,
  edgeId,
  variable
}
import DecompositionTestSupport._

class CoveredDecompositionSuite extends munit.ScalaCheckSuite {
  test("covered bags canonicalize set-valued vertices and covers") {
    val coveredBag = decompositionModel(
      CoveredBag.create(
        bagId(7),
        Vector(variable(30), variable(10), variable(30), variable(20)),
        Vector(edgeId(9), edgeId(3), edgeId(9))
      )
    )

    assertEquals(
      coveredBag.vertices.map(_.value),
      Vector(10, 20, 30)
    )
    assertEquals(coveredBag.cover.map(_.value), Vector(3, 9))
  }

  test("empty bags and empty covers are representable") {
    val coveredBag = bag(0, Seq.empty, Seq.empty)

    assertEquals(coveredBag.vertices, Vector.empty)
    assertEquals(coveredBag.cover, Vector.empty)
    assertEquals(coveredBag.coverWidth, 0)
  }

  test("tree edges are undirected and use canonical endpoint order") {
    val forward = treeEdge(3, 9)
    val reversed = treeEdge(9, 3)

    assertEquals(forward, reversed)
    assertEquals(forward.first, bagId(3))
    assertEquals(forward.second, bagId(9))
    assertEquals(forward.other(bagId(3)), Some(bagId(9)))
  }

  test("construction preserves self-loops for validator diagnostics") {
    val loop = treeEdge(4, 4)

    assertEquals(loop.first, loop.second)
  }

  test("decompositions canonicalize bag and tree-edge input order") {
    val bag10 = bag(10, Seq(1), Seq(10))
    val bag20 = bag(20, Seq(1, 2), Seq(20))
    val bag30 = bag(30, Seq(2), Seq(30))
    val edge10To20 = treeEdge(10, 20)
    val edge20To30 = treeEdge(20, 30)

    val actual = decomposition(
      20,
      Seq(bag30, bag10, bag20),
      Seq(edge20To30, edge10To20)
    )

    assertEquals(actual.bags, Vector(bag10, bag20, bag30))
    assertEquals(actual.treeEdges, Vector(edge10To20, edge20To30))
    assertEquals(actual.neighbors(bagId(20)), Vector(bagId(10), bagId(30)))
    assertEquals(actual.width, 1)
  }

  test("decomposition width is the largest concrete bag-cover size") {
    val actual = decomposition(
      10,
      Seq(
        bag(10, Seq(1, 2), Seq(10)),
        bag(20, Seq(2, 3), Seq(10, 20)),
        bag(30, Seq(3, 4), Seq(20, 30, 40))
      ),
      Seq(treeEdge(10, 20), treeEdge(20, 30))
    )

    assertEquals(actual.width, 3)
  }

  test("cover-distinct plans remain unequal and deterministically ordered") {
    val first = decomposition(
      1,
      Seq(bag(1, Seq(5), Seq(10))),
      Seq.empty
    )
    val second = decomposition(
      1,
      Seq(bag(1, Seq(5), Seq(20))),
      Seq.empty
    )

    assertNotEquals(first, second)
    assert(CoveredDecomposition.ordering.compare(first, second) < 0)
    assertEquals(Vector(second, first).sorted, Vector(first, second))
  }

  test("model factories report null data without throwing") {
    assert(CoveredBag.create(null, null, null).isLeft)
    assert(BagTreeEdge.create(null, null).isLeft)
    assert(CoveredDecomposition.create(null, null, null).isLeft)
  }

  property("bag and tree-edge permutations produce the same decomposition") {
    val bags = Vector(
      bag(10, Seq(1), Seq(10)),
      bag(20, Seq(1, 2), Seq(20)),
      bag(30, Seq(2), Seq(30))
    )
    val treeEdges = Vector(treeEdge(10, 20), treeEdge(20, 30))
    val expected = decomposition(10, bags, treeEdges)
    val input = for {
      permutedBags <- org.scalacheck.Gen.pick(bags.size, bags)
      permutedEdges <- org.scalacheck.Gen.pick(treeEdges.size, treeEdges)
    } yield permutedBags -> permutedEdges

    org.scalacheck.Prop.forAll(input) { case (permutedBags, permutedEdges) =>
      assertEquals(
        decomposition(10, permutedBags, permutedEdges),
        expected
      )
    }
  }
}
