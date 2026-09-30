package at.ac.tuwien.dbai.yannakakis.core

import CoreModelTestSupport._

class IdentifiersSuite extends munit.ScalaCheckSuite {
  test("zero and positive internal identities are valid") {
    assertEquals(relation(0).value, 0)
    assertEquals(variable(1).value, 1)
    assertEquals(edgeId(2).value, 2)
    assertEquals(bagId(Int.MaxValue).value, Int.MaxValue)
  }

  test("negative identities and column ordinals are rejected") {
    assert(RelationOccurrenceId.from(-1).isLeft)
    assert(VariableId.from(-1).isLeft)
    assert(EdgeId.from(-1).isLeft)
    assert(BagId.from(-1).isLeft)
    assert(ColumnId.from(relation(0), -1).isLeft)
  }

  test("column identity is occurrence plus ordinal, never a SQL name") {
    val occurrence = relation(4)
    val firstDuplicateNamedColumn = column(occurrence, 0)
    val secondDuplicateNamedColumn = column(occurrence, 1)

    // A later metadata layer may assign both columns the same SQL name. Their
    // different ordinals keep them distinct in the core regardless.
    assertNotEquals(firstDuplicateNamedColumn, secondDuplicateNamedColumn)
    assertEquals(firstDuplicateNamedColumn.relationOccurrenceId, occurrence)
    assertEquals(secondDuplicateNamedColumn.relationOccurrenceId, occurrence)
  }

  test("the same ordinal in different occurrences remains distinct") {
    assertNotEquals(column(relation(1), 0), column(relation(2), 0))
  }

  test("column construction validates a null relation occurrence") {
    assertEquals(
      ColumnId.from(null, 0),
      Left(ValidationError.MissingValue("column id relation occurrence"))
    )
  }

  test("different identifier types remain nominally distinct") {
    val relationId: Any = relation(7)
    val anEdgeId: Any = edgeId(7)
    val aBagId: Any = bagId(7)

    assertNotEquals(relationId, anEdgeId)
    assertNotEquals(anEdgeId, aBagId)
  }

  property("identifier ordering is deterministic numeric ordering") {
    org.scalacheck.Prop.forAll(
      org.scalacheck.Gen.listOf(org.scalacheck.Gen.choose(0, Int.MaxValue))
    ) { rawValues =>
      val ids = rawValues.map(variable)
      assertEquals(ids.sorted.map(_.value), rawValues.sorted)
    }
  }
}
