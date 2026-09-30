package at.ac.tuwien.dbai.yannakakis.api

import at.ac.tuwien.dbai.yannakakis.api.RewriteOutcome._

class QueryRewriterSuite extends munit.FunSuite {
  private val diagnostic = RewriteDiagnostic(
    RewriteReasonCode.UnsupportedQueryShape,
    "Query shape is not supported",
    DiagnosticSeverity.Warning
  )

  test("rewrite delegates to the implementation") {
    val expectedSql = "SELECT * FROM t"
    val expected = NotApplicable(expectedSql, Vector(diagnostic))
    val rewriter = new QueryRewriter {
      override protected def doRewrite(
          request: RewriteRequest
      ): RewriteOutcome = expected
    }

    assertEquals(rewriter.rewrite(RewriteRequest(expectedSql)), expected)
  }

  test("rewrite accepts an outcome preserving whitespace and comments exactly") {
    val sql = "  /* exact */\nSELECT 1; \n"
    val rewriter = new QueryRewriter {
      override protected def doRewrite(
          request: RewriteRequest
      ): RewriteOutcome =
        Rewritten(request.originalSql, Vector("SELECT 1"))
    }

    assertEquals(rewriter.rewrite(RewriteRequest(sql)).originalSql, sql)
  }

  test("rewrite rejects an implementation that substitutes different SQL") {
    val rewriter = new QueryRewriter {
      override protected def doRewrite(
          request: RewriteRequest
      ): RewriteOutcome =
        NotApplicable("SELECT something_else", Vector(diagnostic))
    }

    val exception = intercept[IllegalStateException] {
      rewriter.rewrite(RewriteRequest("SELECT original"))
    }
    assert(exception.getMessage.contains("preserve"))
  }

  test("rewrite rejects a null request") {
    val rewriter = new QueryRewriter {
      override protected def doRewrite(
          request: RewriteRequest
      ): RewriteOutcome =
        NotApplicable(request.originalSql, Vector(diagnostic))
    }

    intercept[IllegalArgumentException] {
      rewriter.rewrite(null)
    }
  }

  test("rewrite rejects a null outcome from an implementation") {
    val rewriter = new QueryRewriter {
      override protected def doRewrite(
          request: RewriteRequest
      ): RewriteOutcome = null
    }

    val exception = intercept[IllegalStateException] {
      rewriter.rewrite(RewriteRequest("SELECT 1"))
    }
    assert(exception.getMessage.contains("null outcome"))
  }
}
