package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlRenderingError.InvalidResourceLimit

/** Explicit per-candidate envelope for renderer-owned diagnostic statements. */
private[yannakakis] final class PostgreSqlStageDiagnosticLimits private (
    val maxStatements: Long,
    val maxCharactersPerStatement: Long,
    val maxTotalCharacters: Long
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlStageDiagnosticLimits =>
      maxStatements == that.maxStatements &&
        maxCharactersPerStatement == that.maxCharactersPerStatement &&
        maxTotalCharacters == that.maxTotalCharacters
    case _ => false
  }

  override def hashCode(): Int =
    (
      maxStatements,
      maxCharactersPerStatement,
      maxTotalCharacters
    ).hashCode()

  override def toString: String =
    s"PostgreSqlStageDiagnosticLimits($maxStatements,$maxCharactersPerStatement,$maxTotalCharacters)"
}

private[yannakakis] object PostgreSqlStageDiagnosticLimits {
  def create(
      maxStatements: Long,
      maxCharactersPerStatement: Long,
      maxTotalCharacters: Long
  ): Either[Vector[InvalidResourceLimit], PostgreSqlStageDiagnosticLimits] = {
    val supplied = Vector(
      PostgreSqlRenderingResource.StageDiagnosticStatements -> maxStatements,
      PostgreSqlRenderingResource.StageDiagnosticStatementCharacters ->
        maxCharactersPerStatement,
      PostgreSqlRenderingResource.StageDiagnosticTotalCharacters ->
        maxTotalCharacters
    )
    val errors = supplied.collect {
      case (resource, value) if value <= 0L =>
        InvalidResourceLimit(resource, value, "limit must be positive")
      case (
            PostgreSqlRenderingResource.StageDiagnosticStatementCharacters,
            value
          ) if value > Int.MaxValue.toLong =>
        InvalidResourceLimit(
          PostgreSqlRenderingResource.StageDiagnosticStatementCharacters,
          value,
          s"limit must not exceed ${Int.MaxValue}"
        )
    }
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new PostgreSqlStageDiagnosticLimits(
          maxStatements,
          maxCharactersPerStatement,
          maxTotalCharacters
        )
      )
  }
}

/** One standalone, hash-bound statement suitable as EXPLAIN input. */
private[yannakakis] final case class PostgreSqlStageDiagnosticStatement(
    stageId: PostgreSqlStageId,
    sql: String,
    statementSha256: String,
    characterCount: Long,
    ctePrefixCount: Long
) extends Serializable

private[yannakakis] object PostgreSqlStageDiagnosticStatement {
  /** SHA-256 of the exact statement encoded as UTF-8 bytes. */
  val HashScheme: String = "sha256_utf8_v1"

  /** JVM `String.length`: UTF-16 code units, not bytes or code points. */
  val CharacterCountScheme: String = "jvm_utf16_code_units_v1"
}

/** Unchanged normal rendering paired with its emitted-stage diagnostics. */
private[yannakakis] final case class PostgreSqlStageDiagnosticRendering(
    renderedQuery: PostgreSqlRenderedQuery,
    statements: Vector[PostgreSqlStageDiagnosticStatement]
) extends Serializable
