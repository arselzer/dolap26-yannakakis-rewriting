package at.ac.tuwien.dbai.yannakakis.api

class RewriteDiagnosticSuite extends munit.FunSuite {
  test("diagnostic retains structured fields without normalizing them") {
    val diagnostic = RewriteDiagnostic(
      code = RewriteReasonCode.UnsupportedJoinType,
      message = "  LEFT JOIN is not supported yet  ",
      severity = DiagnosticSeverity.Warning,
      context = Map("nodePath" -> "0/1", "joinType" -> "LEFT")
    )

    assertEquals(diagnostic.code, RewriteReasonCode.UnsupportedJoinType)
    assertEquals(diagnostic.message, "  LEFT JOIN is not supported yet  ")
    assertEquals(diagnostic.severity, DiagnosticSeverity.Warning)
    assertEquals(
      diagnostic.context,
      Map("nodePath" -> "0/1", "joinType" -> "LEFT")
    )
  }

  test("error is the default severity") {
    val diagnostic = RewriteDiagnostic(
      RewriteReasonCode.InvalidSql,
      "SQL parsing failed"
    )

    assertEquals(diagnostic.severity, DiagnosticSeverity.Error)
    assertEquals(diagnostic.context, Map.empty[String, String])
  }

  test("reason-code identifiers are stable-shaped and unique") {
    val ids = RewriteReasonCode.values.map(_.id)

    assert(ids.forall(_.matches("[A-Z][A-Z0-9_]*")))
    assertEquals(ids.distinct, ids)
  }

  test("severity identifiers are non-blank and unique") {
    val ids = DiagnosticSeverity.values.map(_.id)

    assert(ids.forall(_.trim.nonEmpty))
    assertEquals(ids.distinct, ids)
  }

  test("blank diagnostic messages are rejected") {
    val exception = intercept[IllegalArgumentException] {
      RewriteDiagnostic(RewriteReasonCode.InternalError, " \t\n ")
    }

    assert(exception.getMessage.contains("message must not be blank"))
  }

  test("null diagnostic fields are rejected") {
    intercept[IllegalArgumentException] {
      RewriteDiagnostic(null, "message")
    }
    intercept[IllegalArgumentException] {
      RewriteDiagnostic(RewriteReasonCode.InternalError, null)
    }
    intercept[IllegalArgumentException] {
      RewriteDiagnostic(
        RewriteReasonCode.InternalError,
        "message",
        severity = null
      )
    }
    intercept[IllegalArgumentException] {
      RewriteDiagnostic(
        RewriteReasonCode.InternalError,
        "message",
        context = null
      )
    }
  }

  test("diagnostic context rejects null and blank keys") {
    intercept[IllegalArgumentException] {
      RewriteDiagnostic(
        RewriteReasonCode.InternalError,
        "message",
        context = Map(null.asInstanceOf[String] -> "value")
      )
    }
    intercept[IllegalArgumentException] {
      RewriteDiagnostic(
        RewriteReasonCode.InternalError,
        "message",
        context = Map("  " -> "value")
      )
    }
  }

  test("diagnostic context rejects null values") {
    intercept[IllegalArgumentException] {
      RewriteDiagnostic(
        RewriteReasonCode.InternalError,
        "message",
        context = Map("key" -> null.asInstanceOf[String])
      )
    }
  }
}
