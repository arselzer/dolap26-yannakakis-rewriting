package at.ac.tuwien.dbai.yannakakis.api

class YannakakisReductionModeSuite extends munit.FunSuite {
  import YannakakisReductionMode._
  import YannakakisReductionModeError._

  test("mode identifiers, declaration order, and default are stable") {
    assertEquals(Values, Vector(FullTwoPass, BottomUpOnly, NoReduction))
    assertEquals(Values.map(_.id), Vector("full", "bottom-up", "none"))
    assertEquals(Values.map(_.id).distinct.size, Values.size)
    assertEquals(Default, FullTwoPass)
  }

  test("untrusted mode parsing is exact and fail closed") {
    Values.foreach(mode => assertEquals(parse(mode.id), Right(mode)))

    Vector("", "FULL", " full", "full ", "bottom_up", "no-reduction").foreach {
      value =>
        assertEquals(parse(value), Left(UnsupportedValue(value)))
    }
    assertEquals(parse(null), Left(MissingValue))
    assertEquals(MissingValue.code, "reduction-mode.missing")
    assertEquals(UnsupportedValue("x").code, "reduction-mode.unsupported")
  }
}
