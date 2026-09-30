package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.frontend.calcite.CalciteSqlPlanningStage

/** Typed, secret-free construction failure, distinct from a rewrite outcome. */
sealed trait PostgreSqlRewriterConfigurationError
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object PostgreSqlRewriterConfigurationError {
  final case class MissingValues(fields: Vector[String])
      extends PostgreSqlRewriterConfigurationError {
    require(fields != null, "missing fields must not be null")
    require(fields.nonEmpty, "missing fields must not be empty")
    require(
      fields.forall(value => value != null && value.trim.nonEmpty),
      "missing fields must not contain null or blank values"
    )

    override val code: String = "postgresql-rewriter.configuration.missing"
    override val message: String =
      s"Required configuration is missing: ${fields.mkString(", ")}"
  }

  final case class CalciteConfigurationFailure(
      stage: CalciteSqlPlanningStage,
      exceptionClass: Option[String]
  ) extends PostgreSqlRewriterConfigurationError {
    require(stage != null, "configuration stage must not be null")
    require(exceptionClass != null, "exception class option must not be null")
    require(
      exceptionClass.forall(value => value != null && value.trim.nonEmpty),
      "exception class must not contain a null or blank value"
    )

    override val code: String = "postgresql-rewriter.configuration.calcite"
    override val message: String =
      s"Calcite configuration failed at stage '${stage.id}'"
  }
}
