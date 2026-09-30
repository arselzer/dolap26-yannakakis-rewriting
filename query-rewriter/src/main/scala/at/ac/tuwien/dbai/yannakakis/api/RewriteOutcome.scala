package at.ac.tuwien.dbai.yannakakis.api

/** Result of attempting to rewrite one SQL statement.
  *
  * Every variant retains the input SQL verbatim. Consumers can therefore log,
  * execute, or return the original statement without retaining the request.
  */
sealed trait RewriteOutcome extends Product with Serializable {
  def originalSql: String
  def diagnostics: Vector[RewriteDiagnostic]
}

object RewriteOutcome {
  final case class Rewritten(
      originalSql: String,
      rewrittenStatements: Vector[String],
      diagnostics: Vector[RewriteDiagnostic] = Vector.empty
  ) extends RewriteOutcome {
    OutcomeValidation.originalSql(originalSql)
    OutcomeValidation.diagnostics(diagnostics, allowEmpty = true)
    OutcomeValidation.requireNoError(diagnostics, "Rewritten")
    require(
      rewrittenStatements != null,
      "rewrittenStatements must not be null"
    )
    require(
      rewrittenStatements.nonEmpty,
      "rewrittenStatements must not be empty"
    )
    require(
      rewrittenStatements.forall(statement =>
        statement != null && statement.trim.nonEmpty
      ),
      "rewrittenStatements must not contain null or blank statements"
    )
  }

  final case class NotApplicable(
      originalSql: String,
      diagnostics: Vector[RewriteDiagnostic]
  ) extends RewriteOutcome {
    OutcomeValidation.originalSql(originalSql)
    OutcomeValidation.diagnostics(diagnostics, allowEmpty = false)
    OutcomeValidation.requireNoError(diagnostics, "NotApplicable")
  }

  final case class InvalidQuery(
      originalSql: String,
      diagnostics: Vector[RewriteDiagnostic]
  ) extends RewriteOutcome {
    OutcomeValidation.originalSql(originalSql)
    OutcomeValidation.diagnostics(diagnostics, allowEmpty = false)
    OutcomeValidation.requireError(diagnostics, "InvalidQuery")
  }

  final case class Failed(
      originalSql: String,
      diagnostics: Vector[RewriteDiagnostic]
  ) extends RewriteOutcome {
    OutcomeValidation.originalSql(originalSql)
    OutcomeValidation.diagnostics(diagnostics, allowEmpty = false)
    OutcomeValidation.requireError(diagnostics, "Failed")
  }

  private object OutcomeValidation {
    def originalSql(value: String): Unit =
      require(value != null, "originalSql must not be null")

    def diagnostics(
        values: Vector[RewriteDiagnostic],
        allowEmpty: Boolean
    ): Unit = {
      require(values != null, "diagnostics must not be null")
      require(
        values.forall(_ != null),
        "diagnostics must not contain null entries"
      )
      if (!allowEmpty) {
        require(values.nonEmpty, "diagnostics must not be empty")
      }
    }

    def requireError(
        values: Vector[RewriteDiagnostic],
        outcomeName: String
    ): Unit =
      require(
        values.exists(_.severity == DiagnosticSeverity.Error),
        s"$outcomeName must contain at least one error diagnostic"
      )

    def requireNoError(
        values: Vector[RewriteDiagnostic],
        outcomeName: String
    ): Unit =
      require(
        !values.exists(_.severity == DiagnosticSeverity.Error),
        s"$outcomeName must not contain an error diagnostic"
      )
  }
}
