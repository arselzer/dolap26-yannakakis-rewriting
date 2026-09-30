package at.ac.tuwien.dbai.yannakakis.api

class TargetDialectSuite extends munit.FunSuite {
  test("PostgreSQL is the only dialect promised by the initial API") {
    assertEquals(TargetDialect.supported, Vector(TargetDialect.PostgreSql))
    assertEquals(TargetDialect.PostgreSql.id, "postgresql")
  }

  test("supported dialect identifiers are non-blank and unique") {
    val ids = TargetDialect.supported.map(_.id)

    assert(ids.forall(_.trim.nonEmpty))
    assertEquals(ids.distinct, ids)
  }
}
