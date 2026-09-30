package at.ac.tuwien.dbai.yannakakis.api

class RewriteRequestSuite extends munit.FunSuite {
  test("request uses PostgreSQL and validated algorithm defaults") {
    val request = RewriteRequest("SELECT 1")

    assertEquals(request.targetDialect, TargetDialect.PostgreSql)
    assertEquals(request.options, RewriteOptions.Default)
  }

  test("request preserves SQL verbatim") {
    val sql = "  /* keep me */\nSELECT  *  FROM t;  \n"
    val request = RewriteRequest(sql)

    assertEquals(request.originalSql, sql)
  }

  test("blank SQL is retained for a later InvalidQuery outcome") {
    val request = RewriteRequest(" \n\t ")

    assertEquals(request.originalSql, " \n\t ")
  }

  test("custom target and options are retained") {
    val options = RewriteOptions(3, 5, requireConnectedCover = false)
    val request = RewriteRequest(
      originalSql = "SELECT 1",
      targetDialect = TargetDialect.PostgreSql,
      options = options
    )

    assertEquals(request.targetDialect, TargetDialect.PostgreSql)
    assertEquals(request.options, options)
  }

  test("request rejects null fields") {
    intercept[IllegalArgumentException] {
      RewriteRequest(null)
    }
    intercept[IllegalArgumentException] {
      RewriteRequest("SELECT 1", targetDialect = null)
    }
    intercept[IllegalArgumentException] {
      RewriteRequest("SELECT 1", options = null)
    }
  }
}
