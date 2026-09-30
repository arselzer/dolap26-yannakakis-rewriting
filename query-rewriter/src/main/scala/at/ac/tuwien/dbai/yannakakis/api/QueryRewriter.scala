package at.ac.tuwien.dbai.yannakakis.api

/** Public entry point for SQL rewriting implementations.
  *
  * Implementations provide [[doRewrite]]. The final wrapper enforces the API's
  * central provenance invariant: every outcome contains the request SQL
  * verbatim, including whitespace and comments.
  */
trait QueryRewriter {
  final def rewrite(request: RewriteRequest): RewriteOutcome = {
    require(request != null, "request must not be null")

    val outcome = doRewrite(request)
    if (outcome == null) {
      throw new IllegalStateException("rewriter returned a null outcome")
    }
    if (outcome.originalSql != request.originalSql) {
      throw new IllegalStateException(
        "rewriter outcome did not preserve the request's original SQL"
      )
    }
    outcome
  }

  protected def doRewrite(request: RewriteRequest): RewriteOutcome
}
