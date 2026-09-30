package at.ac.tuwien.dbai.yannakakis.core.decomposition

import at.ac.tuwien.dbai.yannakakis.core.{QueryHypergraph, VariableId}
import at.ac.tuwien.dbai.yannakakis.core.CoreModelTestSupport.{bagId, edgeId, variable}
import DecompositionTestSupport._

class DecompositionValidatorSuite extends munit.ScalaCheckSuite {
  private val pathGraph = graph(
    edge(10, 1, 2),
    edge(20, 2, 3),
    edge(30, 3, 4)
  )

  private val validPath = decomposition(
    10,
    Seq(
      bag(10, Seq(1, 2), Seq(10)),
      bag(20, Seq(2, 3), Seq(20)),
      bag(30, Seq(3, 4), Seq(30))
    ),
    Seq(treeEdge(10, 20), treeEdge(20, 30))
  )

  test("a canonical covered path decomposition is valid") {
    assertEquals(
      DecompositionValidator.validate(pathGraph, validPath),
      Right(validPath)
    )
  }

  test("duplicate bag ids are rejected without collapsing cover alternatives") {
    val candidate = decomposition(
      10,
      Seq(
        bag(10, Seq(1, 2), Seq(10)),
        bag(10, Seq(2, 3), Seq(20))
      ),
      Seq.empty
    )

    assert(
      errors(pathGraph, candidate).contains(
        DecompositionValidationError.DuplicateBagId(bagId(10))
      )
    )
  }

  test("the root must identify an existing bag") {
    val candidate = decomposition(
      99,
      Seq(bag(10, Seq(1, 2, 3, 4), Seq(10, 20, 30))),
      Seq.empty
    )

    assert(
      errors(pathGraph, candidate).contains(
        DecompositionValidationError.MissingRootBag(bagId(99))
      )
    )
  }

  test("tree edges may only reference existing bags") {
    val unknownEdge = treeEdge(10, 99)
    val candidate = decomposition(
      10,
      Seq(bag(10, Seq(1, 2, 3, 4), Seq(10, 20, 30))),
      Seq(unknownEdge)
    )

    assert(
      errors(pathGraph, candidate).contains(
        DecompositionValidationError.UnknownTreeEndpoint(
          unknownEdge,
          bagId(99)
        )
      )
    )
  }

  test("duplicate tree edges and self-loops are rejected") {
    val duplicate = treeEdge(10, 20)
    val loop = treeEdge(10, 10)
    val candidate = decomposition(
      10,
      Seq(
        bag(10, Seq(1, 2), Seq(10)),
        bag(20, Seq(2, 3, 4), Seq(20, 30))
      ),
      Seq(duplicate, duplicate, loop)
    )
    val actual = errors(pathGraph, candidate)

    assert(
      actual.contains(
        DecompositionValidationError.DuplicateTreeEdge(duplicate)
      )
    )
    assert(actual.contains(DecompositionValidationError.SelfLoop(loop)))
  }

  test("the bag graph must be connected") {
    val candidate = decomposition(
      10,
      validPath.bags,
      Seq(treeEdge(10, 20))
    )

    assert(
      errors(pathGraph, candidate).contains(
        DecompositionValidationError.DisconnectedTree(Vector(bagId(30)))
      )
    )
  }

  test("the bag graph must be acyclic") {
    val candidate = decomposition(
      10,
      validPath.bags,
      Seq(treeEdge(10, 20), treeEdge(20, 30), treeEdge(10, 30))
    )

    assert(errors(pathGraph, candidate).contains(DecompositionValidationError.CyclicTree))
  }

  test("every cover edge id must exist in the query hypergraph") {
    val candidate = decomposition(
      10,
      Seq(bag(10, Seq(1, 2), Seq(10, 99))),
      Seq.empty
    )

    assert(
      errors(pathGraph, candidate).contains(
        DecompositionValidationError.UnknownCoverEdge(bagId(10), edgeId(99))
      )
    )
  }

  test("every bag vertex must lie in the union of its cover") {
    val candidate = decomposition(
      10,
      Seq(bag(10, Seq(1, 2, 3, 4), Seq(10, 20))),
      Seq.empty
    )

    assert(
      errors(pathGraph, candidate).contains(
        DecompositionValidationError.BagVertexNotCovered(
          bagId(10),
          variable(4)
        )
      )
    )
  }

  test("every hyperedge must be contained in at least one bag") {
    val candidate = decomposition(
      10,
      Seq(
        bag(10, Seq(1, 2), Seq(10)),
        bag(20, Seq(2, 3), Seq(20))
      ),
      Seq(treeEdge(10, 20))
    )

    assert(
      errors(pathGraph, candidate).contains(
        DecompositionValidationError.HyperedgeNotCovered(edgeId(30))
      )
    )
  }

  test("edge coverage does not require that edge to appear in a bag cover") {
    val sameVertexGraph = graph(edge(10, 1), edge(20, 1))
    val candidate = decomposition(
      10,
      Seq(bag(10, Seq(1), Seq(10))),
      Seq.empty
    )

    assertEquals(
      DecompositionValidator.validate(sameVertexGraph, candidate),
      Right(candidate)
    )
  }

  test("bags containing each variable must induce a connected subtree") {
    val triangleGraph = graph(
      edge(10, 1, 2),
      edge(20, 2, 3),
      edge(30, 1, 3)
    )
    val candidate = decomposition(
      10,
      Seq(
        bag(10, Seq(1, 2), Seq(10)),
        bag(20, Seq(2, 3), Seq(20)),
        bag(30, Seq(1, 3), Seq(30))
      ),
      Seq(treeEdge(10, 20), treeEdge(20, 30))
    )

    assert(
      errors(triangleGraph, candidate).contains(
        DecompositionValidationError.RunningIntersectionViolation(
          variable(1),
          Vector(bagId(10), bagId(30))
        )
      )
    )
  }

  test("an empty hypergraph has a valid one-empty-bag decomposition") {
    val candidate = decomposition(
      0,
      Seq(bag(0, Seq.empty, Seq.empty)),
      Seq.empty
    )

    assertEquals(
      DecompositionValidator.validate(QueryHypergraph.empty, candidate),
      Right(candidate)
    )
  }

  test("a zero-vertex hyperedge is covered by an empty bag") {
    val zeroVertexGraph = graph(edge(10))
    val candidate = decomposition(
      0,
      Seq(bag(0, Seq.empty, Seq(10))),
      Seq.empty
    )

    assertEquals(
      DecompositionValidator.validate(zeroVertexGraph, candidate),
      Right(candidate)
    )
  }

  test("validator is null-safe") {
    assertEquals(
      DecompositionValidator.validate(null, validPath),
      Left(
        Vector(
          DecompositionValidationError.MissingValue(
            "decomposition validator hypergraph"
          )
        )
      )
    )
    assertEquals(
      DecompositionValidator.validate(pathGraph, null),
      Left(
        Vector(
          DecompositionValidationError.MissingValue(
            "decomposition validator decomposition"
          )
        )
      )
    )
  }

  property("validation result is invariant under model input permutations") {
    val bags = validPath.bags
    val treeEdges = validPath.treeEdges
    val inputs = for {
      permutedBags <- org.scalacheck.Gen.pick(bags.size, bags)
      permutedTreeEdges <- org.scalacheck.Gen.pick(treeEdges.size, treeEdges)
    } yield permutedBags -> permutedTreeEdges

    org.scalacheck.Prop.forAll(inputs) {
      case (permutedBags, permutedTreeEdges) =>
        val candidate = decomposition(10, permutedBags, permutedTreeEdges)
        assertEquals(
          DecompositionValidator.validate(pathGraph, candidate),
          Right(validPath)
        )
    }
  }

  property("cover-distinct valid plans are preserved as separate values") {
    val sameVertexGraph = graph(edge(10, 1), edge(20, 1))
    val choices = org.scalacheck.Gen.oneOf(10, 20)

    org.scalacheck.Prop.forAll(choices) { coverId =>
      val candidate = decomposition(
        1,
        Seq(bag(1, Seq(1), Seq(coverId))),
        Seq.empty
      )
      assertEquals(
        DecompositionValidator.validate(sameVertexGraph, candidate),
        Right(candidate)
      )
      assertEquals(candidate.bags.head.cover, Vector(edgeId(coverId)))
    }
  }
}
