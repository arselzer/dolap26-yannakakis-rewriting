package at.ac.tuwien.dbai.yannakakis.core

import CoreModelTestSupport._

class HyperedgeSuite extends munit.ScalaCheckSuite {
  test("bindings and vertices have canonical deterministic order") {
    val occurrence = relation(10)
    val firstColumn = column(occurrence, 0)
    val secondColumn = column(occurrence, 1)
    val firstVariable = variable(20)
    val secondVariable = variable(30)

    val hyperedge = model(
      Hyperedge.create(
        edgeId(5),
        occurrence,
        Vector(secondColumn -> secondVariable, firstColumn -> firstVariable)
      )
    )

    assertEquals(hyperedge.columns, Vector(firstColumn, secondColumn))
    assertEquals(hyperedge.vertices, Vector(firstVariable, secondVariable))
    assertEquals(hyperedge.variableFor(firstColumn), Some(firstVariable))
  }

  test("repeated variables and zero-vertex edges are valid") {
    val occurrence = relation(10)
    val sharedVariable = variable(20)
    val repeatedVariableEdge = model(
      Hyperedge.create(
        edgeId(5),
        occurrence,
        Vector(
          column(occurrence, 0) -> sharedVariable,
          column(occurrence, 1) -> sharedVariable
        )
      )
    )
    val zeroVertexEdge = model(
      Hyperedge.create(edgeId(6), relation(11), Vector.empty)
    )

    assertEquals(repeatedVariableEdge.vertices, Vector(sharedVariable))
    assertEquals(repeatedVariableEdge.arity, 2)
    assertEquals(zeroVertexEdge.vertices, Vector.empty)
    assertEquals(zeroVertexEdge.arity, 0)
  }

  test("a column must belong to the edge's relation occurrence") {
    val edgeOccurrence = relation(10)
    val result = Hyperedge.create(
      edgeId(5),
      edgeOccurrence,
      Vector(column(relation(11), 0) -> variable(20))
    )

    val errors = result.swap.toOption.getOrElse(Vector.empty)
    assert(errors.exists(_.isInstanceOf[ValidationError.ColumnOccurrenceMismatch]))
  }

  test("a column ordinal cannot be bound more than once") {
    val occurrence = relation(10)
    val duplicate = column(occurrence, 0)
    val result = Hyperedge.create(
      edgeId(5),
      occurrence,
      Vector(duplicate -> variable(20), duplicate -> variable(30))
    )

    assertEquals(
      result.swap.toOption.getOrElse(Vector.empty),
      Vector(ValidationError.DuplicateColumn(edgeId(5), duplicate))
    )
  }

  test("factory validation is null-safe") {
    val result = Hyperedge.create(null, relation(10), null)
    val errors = result.swap.toOption.getOrElse(Vector.empty)

    assert(errors.contains(ValidationError.MissingValue("hyperedge id")))
    assert(errors.contains(ValidationError.MissingValue("hyperedge bindings")))
  }

  property("binding input order does not affect a hyperedge") {
    val occurrence = relation(10)
    val originalBindings = Vector(
      column(occurrence, 0) -> variable(20),
      column(occurrence, 1) -> variable(30),
      column(occurrence, 2) -> variable(40)
    )
    val permutations = org.scalacheck.Gen.pick(
      originalBindings.size,
      originalBindings
    )

    org.scalacheck.Prop.forAll(permutations) { permutedBindings =>
      val actual = model(Hyperedge.create(edgeId(5), occurrence, permutedBindings))
      val expected = model(Hyperedge.create(edgeId(5), occurrence, originalBindings))
      assertEquals(actual, expected)
      assertEquals(actual.bindings, expected.bindings)
    }
  }
}
