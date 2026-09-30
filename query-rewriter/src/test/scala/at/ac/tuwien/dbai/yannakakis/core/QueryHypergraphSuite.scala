package at.ac.tuwien.dbai.yannakakis.core

import CoreModelTestSupport._

class QueryHypergraphSuite extends munit.ScalaCheckSuite {
  private val sharedVariable = variable(100)

  private val leftOccurrence = relation(10)
  private val leftEdge = model(
    Hyperedge.create(
      edgeId(10),
      leftOccurrence,
      Vector(column(leftOccurrence, 0) -> sharedVariable)
    )
  )

  private val rightOccurrence = relation(30)
  private val rightEdge = model(
    Hyperedge.create(
      edgeId(30),
      rightOccurrence,
      Vector(column(rightOccurrence, 0) -> sharedVariable)
    )
  )

  test("self-join occurrences with identical vertices remain distinct edges") {
    assert(leftEdge.sameVerticesAs(rightEdge))
    assertNotEquals(leftEdge, rightEdge)

    val graph = model(QueryHypergraph.create(Vector(rightEdge, leftEdge)))

    assertEquals(graph.size, 2)
    assertEquals(graph.edges, Vector(leftEdge, rightEdge))
    assertEquals(
      graph.incidentEdgeIds(sharedVariable),
      Vector(edgeId(10), edgeId(30))
    )
    assertEquals(graph.edgeFor(leftOccurrence), Some(leftEdge))
    assertEquals(graph.edgeFor(rightOccurrence), Some(rightEdge))
  }

  test("duplicate edge ids and relation occurrence ids are rejected") {
    val duplicateEdgeId = model(
      Hyperedge.create(leftEdge.id, relation(40), Vector.empty)
    )
    val duplicateOccurrence = model(
      Hyperedge.create(
        edgeId(40),
        leftOccurrence,
        Vector(column(leftOccurrence, 1) -> variable(101))
      )
    )

    val duplicateEdgeErrors = QueryHypergraph
      .create(Vector(leftEdge, duplicateEdgeId))
      .swap
      .toOption
      .getOrElse(Vector.empty)
    val duplicateOccurrenceErrors = QueryHypergraph
      .create(Vector(leftEdge, duplicateOccurrence))
      .swap
      .toOption
      .getOrElse(Vector.empty)

    assertEquals(
      duplicateEdgeErrors,
      Vector(ValidationError.DuplicateEdgeId(leftEdge.id))
    )
    assertEquals(
      duplicateOccurrenceErrors,
      Vector(ValidationError.DuplicateRelationOccurrenceId(leftOccurrence))
    )
  }

  test("the empty hypergraph is valid and canonical") {
    assertEquals(model(QueryHypergraph.create(Vector.empty)), QueryHypergraph.empty)
    assert(QueryHypergraph.empty.isEmpty)
    assertEquals(QueryHypergraph.empty.incidence, Vector.empty)
  }

  test("factory validation is null-safe") {
    assertEquals(
      QueryHypergraph.create(null),
      Left(Vector(ValidationError.MissingValue("query hypergraph edges")))
    )
  }

  property("edge input order does not affect graph or incidence order") {
    val middleOccurrence = relation(20)
    val middleEdge = model(
      Hyperedge.create(
        edgeId(20),
        middleOccurrence,
        Vector(column(middleOccurrence, 0) -> sharedVariable)
      )
    )
    val canonicalEdges = Vector(leftEdge, middleEdge, rightEdge)
    val permutations = org.scalacheck.Gen.pick(canonicalEdges.size, canonicalEdges)

    org.scalacheck.Prop.forAll(permutations) { permutedEdges =>
      val graph = model(QueryHypergraph.create(permutedEdges))
      assertEquals(graph.edges, canonicalEdges)
      assertEquals(
        graph.incidence,
        Vector(
          sharedVariable -> Vector(edgeId(10), edgeId(20), edgeId(30))
        )
      )
    }
  }
}
