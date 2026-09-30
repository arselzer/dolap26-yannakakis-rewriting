package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlRenderingError.InvalidResourceLimit

sealed trait PostgreSqlRenderingResource extends Product with Serializable {
  def code: String
}

object PostgreSqlRenderingResource {
  case object ExpandedScans extends PostgreSqlRenderingResource {
    val code = "expanded-scans"
  }
  case object RenderedTerms extends PostgreSqlRenderingResource {
    val code = "rendered-terms"
  }
  case object Ctes extends PostgreSqlRenderingResource {
    val code = "ctes"
  }
  case object SqlCharacters extends PostgreSqlRenderingResource {
    val code = "sql-characters"
  }
  case object AggregateNestingDepth extends PostgreSqlRenderingResource {
    val code = "aggregate-nesting-depth"
  }
  case object StageDiagnosticStatements extends PostgreSqlRenderingResource {
    val code = "stage-diagnostic-statements"
  }
  case object StageDiagnosticStatementCharacters
      extends PostgreSqlRenderingResource {
    val code = "stage-diagnostic-statement-characters"
  }
  case object StageDiagnosticTotalCharacters
      extends PostgreSqlRenderingResource {
    val code = "stage-diagnostic-total-characters"
  }
}

final class PostgreSqlRenderingLimits private (
    val maxExpandedScans: Long,
    val maxRenderedTerms: Long,
    val maxCtes: Long,
    val maxSqlCharacters: Long
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlRenderingLimits =>
      maxExpandedScans == that.maxExpandedScans &&
        maxRenderedTerms == that.maxRenderedTerms &&
        maxCtes == that.maxCtes &&
        maxSqlCharacters == that.maxSqlCharacters
    case _ => false
  }

  override def hashCode(): Int =
    (
      maxExpandedScans,
      maxRenderedTerms,
      maxCtes,
      maxSqlCharacters
    ).hashCode()

  override def toString: String =
    s"PostgreSqlRenderingLimits($maxExpandedScans,$maxRenderedTerms,$maxCtes,$maxSqlCharacters)"
}

object PostgreSqlRenderingLimits {
  /**
    * Non-configurable engine-safety ceiling for recursively nested aggregate
    * terminal subqueries. Keeping this separate from the four serialized
    * caller limits preserves the v1--v6 artifact schemas and replay bytes.
    *
    * PostgreSQL 18.4 accepts 80 but rejects 81 guarded-stage levels, and
    * accepts 81 but rejects 82 frequency-message levels, at its minimum
    * supported `max_stack_depth` setting of 100kB. The lower fixed ceiling
    * leaves headroom for expression complexity within each level.
    */
  val MaximumAggregateNestingDepth: Long = 64L

  val default: PostgreSqlRenderingLimits = new PostgreSqlRenderingLimits(
    maxExpandedScans = 1000000L,
    maxRenderedTerms = 10000000L,
    maxCtes = 1000000L,
    maxSqlCharacters = 16777216L
  )

  def create(
      maxExpandedScans: Long = default.maxExpandedScans,
      maxRenderedTerms: Long = default.maxRenderedTerms,
      maxCtes: Long = default.maxCtes,
      maxSqlCharacters: Long = default.maxSqlCharacters
  ): Either[Vector[InvalidResourceLimit], PostgreSqlRenderingLimits] = {
    val supplied = Vector(
      PostgreSqlRenderingResource.ExpandedScans -> maxExpandedScans,
      PostgreSqlRenderingResource.RenderedTerms -> maxRenderedTerms,
      PostgreSqlRenderingResource.Ctes -> maxCtes,
      PostgreSqlRenderingResource.SqlCharacters -> maxSqlCharacters
    )
    val errors = supplied.collect {
      case (resource, value) if value <= 0L =>
        InvalidResourceLimit(resource, value, "limit must be positive")
      case (PostgreSqlRenderingResource.SqlCharacters, value)
          if value > Int.MaxValue.toLong =>
        InvalidResourceLimit(
          PostgreSqlRenderingResource.SqlCharacters,
          value,
          s"limit must not exceed ${Int.MaxValue}"
        )
    }

    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new PostgreSqlRenderingLimits(
          maxExpandedScans,
          maxRenderedTerms,
          maxCtes,
          maxSqlCharacters
        )
      )
  }
}
