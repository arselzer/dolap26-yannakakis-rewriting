package at.ac.tuwien.dbai.yannakakis.api

/** Machine-readable severity attached to a rewrite diagnostic. */
sealed trait DiagnosticSeverity extends Product with Serializable {
  def id: String
}

object DiagnosticSeverity {
  case object Info extends DiagnosticSeverity {
    override val id: String = "info"
  }

  case object Warning extends DiagnosticSeverity {
    override val id: String = "warning"
  }

  case object Error extends DiagnosticSeverity {
    override val id: String = "error"
  }

  val values: Vector[DiagnosticSeverity] = Vector(Info, Warning, Error)
}

/** Stable reason code suitable for programmatic handling and serialization. */
sealed trait RewriteReasonCode extends Product with Serializable {
  def id: String
}

object RewriteReasonCode {
  case object InvalidOption extends RewriteReasonCode {
    override val id: String = "INVALID_OPTION"
  }

  case object InvalidSql extends RewriteReasonCode {
    override val id: String = "INVALID_SQL"
  }

  case object UnsupportedQueryShape extends RewriteReasonCode {
    override val id: String = "UNSUPPORTED_QUERY_SHAPE"
  }

  case object UnsupportedJoinType extends RewriteReasonCode {
    override val id: String = "UNSUPPORTED_JOIN_TYPE"
  }

  case object UnsupportedPredicate extends RewriteReasonCode {
    override val id: String = "UNSUPPORTED_PREDICATE"
  }

  case object UnsupportedAggregate extends RewriteReasonCode {
    override val id: String = "UNSUPPORTED_AGGREGATE"
  }

  case object UnresolvedLineage extends RewriteReasonCode {
    override val id: String = "UNRESOLVED_LINEAGE"
  }

  case object UnsafeBagSemantics extends RewriteReasonCode {
    override val id: String = "UNSAFE_BAG_SEMANTICS"
  }

  case object NoDecomposition extends RewriteReasonCode {
    override val id: String = "NO_DECOMPOSITION"
  }

  case object NoConnectedCover extends RewriteReasonCode {
    override val id: String = "NO_CONNECTED_COVER"
  }

  case object RewriteNotBeneficial extends RewriteReasonCode {
    override val id: String = "REWRITE_NOT_BENEFICIAL"
  }

  case object PlanningFailed extends RewriteReasonCode {
    override val id: String = "PLANNING_FAILED"
  }

  case object CandidateSelectionFailed extends RewriteReasonCode {
    override val id: String = "CANDIDATE_SELECTION_FAILED"
  }

  case object RenderingFailed extends RewriteReasonCode {
    override val id: String = "RENDERING_FAILED"
  }

  case object InternalError extends RewriteReasonCode {
    override val id: String = "INTERNAL_ERROR"
  }

  val values: Vector[RewriteReasonCode] = Vector(
    InvalidOption,
    InvalidSql,
    UnsupportedQueryShape,
    UnsupportedJoinType,
    UnsupportedPredicate,
    UnsupportedAggregate,
    UnresolvedLineage,
    UnsafeBagSemantics,
    NoDecomposition,
    NoConnectedCover,
    RewriteNotBeneficial,
    PlanningFailed,
    CandidateSelectionFailed,
    RenderingFailed,
    InternalError
  )
}

/** Structured explanation emitted during rewriting.
  *
  * `context` is intended for stable machine-readable facts such as an option
  * name, relational node path, or unsupported operator. It deliberately does
  * not contain a Throwable, keeping the public result immutable and portable.
  */
final case class RewriteDiagnostic(
    code: RewriteReasonCode,
    message: String,
    severity: DiagnosticSeverity = DiagnosticSeverity.Error,
    context: Map[String, String] = Map.empty
) {
  require(code != null, "diagnostic code must not be null")
  require(message != null, "diagnostic message must not be null")
  require(message.trim.nonEmpty, "diagnostic message must not be blank")
  require(severity != null, "diagnostic severity must not be null")
  require(context != null, "diagnostic context must not be null")
  require(
    context.keysIterator.forall(key => key != null && key.trim.nonEmpty),
    "diagnostic context keys must not be null or blank"
  )
  require(
    context.valuesIterator.forall(_ != null),
    "diagnostic context values must not be null"
  )
}
