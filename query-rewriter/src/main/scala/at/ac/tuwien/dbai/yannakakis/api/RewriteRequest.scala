package at.ac.tuwien.dbai.yannakakis.api

/** Immutable input to a [[QueryRewriter]].
  *
  * Empty or blank SQL is retained so that parsing can return an
  * [[RewriteOutcome.InvalidQuery]] which still contains the exact input.
  */
final case class RewriteRequest(
    originalSql: String,
    targetDialect: TargetDialect = TargetDialect.PostgreSql,
    options: RewriteOptions = RewriteOptions.Default
) {
  require(originalSql != null, "originalSql must not be null")
  require(targetDialect != null, "targetDialect must not be null")
  require(options != null, "options must not be null")
}
