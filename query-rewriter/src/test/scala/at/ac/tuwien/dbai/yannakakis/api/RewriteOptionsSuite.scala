package at.ac.tuwien.dbai.yannakakis.api

class RewriteOptionsSuite extends munit.FunSuite {
  test("defaults use soft width two, ten plans, ConCov, and full reduction") {
    val options = RewriteOptions()

    assertEquals(options.widthBound, 2)
    assertEquals(options.planLimit, 10)
    assert(options.requireConnectedCover)
    assertEquals(options.reductionMode, YannakakisReductionMode.FullTwoPass)
    assertEquals(RewriteOptions.Default, options)
  }

  test("positive custom bounds and disabled ConCov are accepted") {
    val options = RewriteOptions(
      widthBound = 4,
      planLimit = 25,
      requireConnectedCover = false
    )

    assertEquals(options.widthBound, 4)
    assertEquals(options.planLimit, 25)
    assert(!options.requireConnectedCover)
  }

  test("constructor rejects non-positive width bounds") {
    Seq(0, -1, Int.MinValue).foreach { invalid =>
      val exception = intercept[IllegalArgumentException] {
        RewriteOptions(widthBound = invalid)
      }
      assert(exception.getMessage.contains("widthBound"))
    }
  }

  test("constructor rejects non-positive plan limits") {
    Seq(0, -1, Int.MinValue).foreach { invalid =>
      val exception = intercept[IllegalArgumentException] {
        RewriteOptions(planLimit = invalid)
      }
      assert(exception.getMessage.contains("planLimit"))
    }
  }

  test("case-class copy cannot bypass validation") {
    intercept[IllegalArgumentException] {
      RewriteOptions.Default.copy(widthBound = 0)
    }
    intercept[IllegalArgumentException] {
      RewriteOptions.Default.copy(planLimit = 0)
    }
    intercept[IllegalArgumentException] {
      RewriteOptions.Default.copy(reductionMode = null)
    }
  }

  test("validated returns configured options without throwing") {
    val result = RewriteOptions.validated(
      widthBound = 3,
      planLimit = 7,
      requireConnectedCover = false,
      reductionMode = YannakakisReductionMode.BottomUpOnly
    )

    assertEquals(
      result,
      Right(
        RewriteOptions(
          widthBound = 3,
          planLimit = 7,
          requireConnectedCover = false,
          reductionMode = YannakakisReductionMode.BottomUpOnly
        )
      )
    )
  }

  test("validated uses the same defaults as the constructor") {
    assertEquals(RewriteOptions.validated(), Right(RewriteOptions.Default))
  }

  test("validated reports every invalid numeric field in declaration order") {
    val result = RewriteOptions.validated(widthBound = 0, planLimit = -4)

    val diagnostics = result match {
      case Left(values) => values
      case Right(value) => fail(s"expected validation errors, got $value")
    }

    assertEquals(diagnostics.map(_.code), Vector.fill(2)(RewriteReasonCode.InvalidOption))
    assertEquals(
      diagnostics.map(_.context("option")),
      Vector("widthBound", "planLimit")
    )
    assertEquals(diagnostics.map(_.context("value")), Vector("0", "-4"))
    assert(diagnostics.forall(_.severity == DiagnosticSeverity.Error))
  }

  test("validated reports null selection and reduction mode in declaration order") {
    val diagnostics = RewriteOptions
      .validated(candidateSelection = null, reductionMode = null)
      .left
      .toOption
      .getOrElse(fail("null request options were accepted"))

    assertEquals(
      diagnostics.map(_.context("option")),
      Vector("candidateSelection", "reductionMode")
    )
    assertEquals(diagnostics.map(_.code), Vector.fill(2)(RewriteReasonCode.InvalidOption))
    assertEquals(diagnostics.last.context("constraint"), "non-null")
  }

  test("validated errors are non-empty when validation fails") {
    val result = RewriteOptions.validated(widthBound = -1)

    assert(result.left.exists(_.nonEmpty))
  }

  test("no arbitrary upper bound is imposed") {
    assertEquals(
      RewriteOptions.validated(Int.MaxValue, Int.MaxValue),
      Right(RewriteOptions(Int.MaxValue, Int.MaxValue))
    )
  }
}
