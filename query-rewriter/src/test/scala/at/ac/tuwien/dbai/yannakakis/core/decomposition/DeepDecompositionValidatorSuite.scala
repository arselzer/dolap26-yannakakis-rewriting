package at.ac.tuwien.dbai.yannakakis.core.decomposition

import at.ac.tuwien.dbai.yannakakis.core._

class DeepDecompositionValidatorSuite extends munit.FunSuite {
  test("cycle detection and connectivity are stack-safe on a 20000-bag chain") {
    val depth = 20000
    val variable = value(VariableId.from(0))
    val edgeId = value(EdgeId.from(0))
    val occurrence = value(RelationOccurrenceId.from(0))
    val column = value(ColumnId.from(occurrence, 0))
    val hyperedge = coreValues(
      Hyperedge.create(edgeId, occurrence, Vector(column -> variable))
    )
    val hypergraph = coreValues(QueryHypergraph.create(Vector(hyperedge)))
    val bags = Vector.tabulate(depth) { index =>
      decompositionValue(
        CoveredBag.create(
          value(BagId.from(index)),
          Vector(variable),
          Vector(edgeId)
        )
      )
    }
    val edges = Vector.tabulate(depth - 1) { index =>
      decompositionValue(
        BagTreeEdge.create(
          value(BagId.from(index)),
          value(BagId.from(index + 1))
        )
      )
    }
    val decomposition = decompositionValue(
      CoveredDecomposition.create(value(BagId.from(0)), bags, edges)
    )

    assertEquals(
      DecompositionValidator.validate(hypergraph, decomposition),
      Right(decomposition)
    )
  }

  private def value[A](result: Either[ValidationError, A]): A =
    result.fold(error => throw new AssertionError(error.message), identity)

  private def coreValues[A](
      result: Either[Vector[ValidationError], A]
  ): A = result.fold(
    errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
    identity
  )

  private def decompositionValue[A](
      result: Either[Vector[DecompositionValidationError], A]
  ): A = result.fold(
    errors => throw new AssertionError(errors.map(_.message).mkString("; ")),
    identity
  )
}
