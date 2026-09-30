package at.ac.tuwien.dbai.yannakakis.api

import at.ac.tuwien.dbai.yannakakis.api.RewriteOutcome._

class RewriteOutcomeSuite extends munit.FunSuite {
  private val exactSql = "  /* original */\nSELECT * FROM t;  "

  private val warning = RewriteDiagnostic(
    RewriteReasonCode.RewriteNotBeneficial,
    "The original plan is preferred",
    DiagnosticSeverity.Warning
  )

  private val invalidSql = RewriteDiagnostic(
    RewriteReasonCode.InvalidSql,
    "SQL parsing failed"
  )

  private val internalError = RewriteDiagnostic(
    RewriteReasonCode.InternalError,
    "Unexpected planner failure"
  )

  test("Rewritten preserves original SQL and rendered statements") {
    val outcome = Rewritten(
      exactSql,
      Vector("CREATE TEMP TABLE q AS SELECT * FROM t", "SELECT * FROM q"),
      Vector(warning)
    )

    assertEquals(outcome.originalSql, exactSql)
    assertEquals(
      outcome.rewrittenStatements,
      Vector("CREATE TEMP TABLE q AS SELECT * FROM t", "SELECT * FROM q")
    )
    assertEquals(outcome.diagnostics, Vector(warning))
  }

  test("Rewritten allows no diagnostics") {
    val outcome = Rewritten(exactSql, Vector("SELECT * FROM t"))

    assertEquals(outcome.diagnostics, Vector.empty[RewriteDiagnostic])
  }

  test("every non-success outcome preserves original SQL") {
    val outcomes: Vector[RewriteOutcome] = Vector(
      NotApplicable(exactSql, Vector(warning)),
      InvalidQuery(exactSql, Vector(invalidSql)),
      Failed(exactSql, Vector(internalError))
    )

    assert(outcomes.forall(_.originalSql == exactSql))
  }

  test("NotApplicable can carry a warning as its structured reason") {
    val outcome = NotApplicable(exactSql, Vector(warning))

    assertEquals(outcome.diagnostics, Vector(warning))
  }

  test("successful and not-applicable outcomes reject error diagnostics") {
    intercept[IllegalArgumentException] {
      Rewritten(exactSql, Vector("SELECT 1"), Vector(internalError))
    }
    intercept[IllegalArgumentException] {
      NotApplicable(exactSql, Vector(internalError))
    }
  }

  test("non-success outcomes require at least one diagnostic") {
    intercept[IllegalArgumentException] {
      NotApplicable(exactSql, Vector.empty)
    }
    intercept[IllegalArgumentException] {
      InvalidQuery(exactSql, Vector.empty)
    }
    intercept[IllegalArgumentException] {
      Failed(exactSql, Vector.empty)
    }
  }

  test("InvalidQuery and Failed require an error diagnostic") {
    intercept[IllegalArgumentException] {
      InvalidQuery(exactSql, Vector(warning))
    }
    intercept[IllegalArgumentException] {
      Failed(exactSql, Vector(warning))
    }
  }

  test("InvalidQuery and Failed may include warnings alongside an error") {
    assertEquals(
      InvalidQuery(exactSql, Vector(warning, invalidSql)).diagnostics,
      Vector(warning, invalidSql)
    )
    assertEquals(
      Failed(exactSql, Vector(warning, internalError)).diagnostics,
      Vector(warning, internalError)
    )
  }

  test("Rewritten requires at least one non-blank statement") {
    intercept[IllegalArgumentException] {
      Rewritten(exactSql, Vector.empty)
    }
    intercept[IllegalArgumentException] {
      Rewritten(exactSql, Vector("  \n"))
    }
    intercept[IllegalArgumentException] {
      Rewritten(exactSql, Vector(null.asInstanceOf[String]))
    }
    intercept[IllegalArgumentException] {
      Rewritten(exactSql, null)
    }
  }

  test("all outcomes reject null original SQL") {
    intercept[IllegalArgumentException] {
      Rewritten(null, Vector("SELECT 1"))
    }
    intercept[IllegalArgumentException] {
      NotApplicable(null, Vector(warning))
    }
    intercept[IllegalArgumentException] {
      InvalidQuery(null, Vector(invalidSql))
    }
    intercept[IllegalArgumentException] {
      Failed(null, Vector(internalError))
    }
  }

  test("outcomes reject null diagnostic collections and entries") {
    intercept[IllegalArgumentException] {
      Rewritten(exactSql, Vector("SELECT 1"), null)
    }
    intercept[IllegalArgumentException] {
      NotApplicable(exactSql, null)
    }
    intercept[IllegalArgumentException] {
      Failed(exactSql, Vector(null.asInstanceOf[RewriteDiagnostic]))
    }
  }

  test("blank original SQL is preserved rather than normalized") {
    val blank = " \t\n "

    assertEquals(InvalidQuery(blank, Vector(invalidSql)).originalSql, blank)
  }
}
