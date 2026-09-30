package at.ac.tuwien.dbai.yannakakis.api.postgresql

sealed trait PostgreSqlCandidateSelectionResource
    extends Product
    with Serializable {
  def code: String
}

object PostgreSqlCandidateSelectionResource {
  case object EvaluatedCandidates extends PostgreSqlCandidateSelectionResource {
    override val code: String = "evaluated-candidates"
  }
  case object CumulativeExpandedScans
      extends PostgreSqlCandidateSelectionResource {
    override val code: String = "cumulative-expanded-scans"
  }
  case object CumulativeCtes extends PostgreSqlCandidateSelectionResource {
    override val code: String = "cumulative-ctes"
  }
  case object CumulativeRenderedTerms
      extends PostgreSqlCandidateSelectionResource {
    override val code: String = "cumulative-rendered-terms"
  }
  case object CumulativeSqlCharacters
      extends PostgreSqlCandidateSelectionResource {
    override val code: String = "cumulative-sql-characters"
  }
  case object CumulativeStageProbes
      extends PostgreSqlCandidateSelectionResource {
    override val code: String = "cumulative-stage-probes"
  }
  case object CumulativeStageDiagnosticCharacters
      extends PostgreSqlCandidateSelectionResource {
    override val code: String = "cumulative-stage-diagnostic-characters"
  }
}

sealed trait PostgreSqlCandidateSelectionLimitError
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object PostgreSqlCandidateSelectionLimitError {
  final case class InvalidResourceLimit(
      resource: PostgreSqlCandidateSelectionResource,
      value: Long
  ) extends PostgreSqlCandidateSelectionLimitError {
    override val code: String = s"selection-limit.invalid.${resource.code}"
    override val message: String =
      s"Candidate selection limit ${resource.code} must be positive, received $value"
  }
}

/** Exact cumulative envelope for multi-candidate physical-shape evaluation. */
final class PostgreSqlCandidateSelectionLimits private (
    val maxEvaluatedCandidates: Long,
    val maxCumulativeExpandedScans: Long,
    val maxCumulativeCtes: Long,
    val maxCumulativeRenderedTerms: Long,
    val maxCumulativeSqlCharacters: Long
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlCandidateSelectionLimits =>
      maxEvaluatedCandidates == that.maxEvaluatedCandidates &&
        maxCumulativeExpandedScans == that.maxCumulativeExpandedScans &&
        maxCumulativeCtes == that.maxCumulativeCtes &&
        maxCumulativeRenderedTerms == that.maxCumulativeRenderedTerms &&
        maxCumulativeSqlCharacters == that.maxCumulativeSqlCharacters
    case _ => false
  }

  override def hashCode(): Int =
    (
      maxEvaluatedCandidates,
      maxCumulativeExpandedScans,
      maxCumulativeCtes,
      maxCumulativeRenderedTerms,
      maxCumulativeSqlCharacters
    ).hashCode()

  override def toString: String =
    s"PostgreSqlCandidateSelectionLimits($maxEvaluatedCandidates," +
      s"$maxCumulativeExpandedScans,$maxCumulativeCtes," +
      s"$maxCumulativeRenderedTerms,$maxCumulativeSqlCharacters)"
}

object PostgreSqlCandidateSelectionLimits {
  val default: PostgreSqlCandidateSelectionLimits =
    new PostgreSqlCandidateSelectionLimits(
      maxEvaluatedCandidates = 100L,
      maxCumulativeExpandedScans = 10000000L,
      maxCumulativeCtes = 10000000L,
      maxCumulativeRenderedTerms = 100000000L,
      maxCumulativeSqlCharacters = 167772160L
    )

  def create(
      maxEvaluatedCandidates: Long = default.maxEvaluatedCandidates,
      maxCumulativeExpandedScans: Long = default.maxCumulativeExpandedScans,
      maxCumulativeCtes: Long = default.maxCumulativeCtes,
      maxCumulativeRenderedTerms: Long = default.maxCumulativeRenderedTerms,
      maxCumulativeSqlCharacters: Long = default.maxCumulativeSqlCharacters
  ): Either[
    Vector[PostgreSqlCandidateSelectionLimitError.InvalidResourceLimit],
    PostgreSqlCandidateSelectionLimits
  ] = {
    import PostgreSqlCandidateSelectionResource._
    val supplied = Vector(
      EvaluatedCandidates -> maxEvaluatedCandidates,
      CumulativeExpandedScans -> maxCumulativeExpandedScans,
      CumulativeCtes -> maxCumulativeCtes,
      CumulativeRenderedTerms -> maxCumulativeRenderedTerms,
      CumulativeSqlCharacters -> maxCumulativeSqlCharacters
    )
    val errors = supplied.collect {
      case (resource, value) if value <= 0L =>
        PostgreSqlCandidateSelectionLimitError.InvalidResourceLimit(
          resource,
          value
        )
    }
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        new PostgreSqlCandidateSelectionLimits(
          maxEvaluatedCandidates,
          maxCumulativeExpandedScans,
          maxCumulativeCtes,
          maxCumulativeRenderedTerms,
          maxCumulativeSqlCharacters
        )
      )
  }
}
