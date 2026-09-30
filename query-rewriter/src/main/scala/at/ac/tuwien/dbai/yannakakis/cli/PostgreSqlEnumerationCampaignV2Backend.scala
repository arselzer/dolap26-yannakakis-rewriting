package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchRequest,
  PostgreSqlEnumerationBatchResult,
  PostgreSqlQueryRewriter
}

/** Stable backend failures; database diagnostics and credentials are absent. */
private[cli] sealed trait PostgreSqlEnumerationCampaignV2BackendFailure
    extends Product
    with Serializable {
  def code: String
}

private[cli] object PostgreSqlEnumerationCampaignV2BackendFailure {
  case object PostgreSqlUnavailable
      extends PostgreSqlEnumerationCampaignV2BackendFailure {
    override val code: String = "acquisition-backend.postgresql-unavailable"
  }

  case object RewriterConfigurationFailed
      extends PostgreSqlEnumerationCampaignV2BackendFailure {
    override val code: String =
      "acquisition-backend.rewriter-configuration-failed"
  }
}

private[cli] trait PostgreSqlEnumerationCampaignV2Backend {
  def acquire(
      config: PostgreSqlEnumerationCampaignV2CliConfig,
      request: PostgreSqlEnumerationBatchRequest
  ): Either[
    PostgreSqlEnumerationCampaignV2BackendFailure,
    PostgreSqlEnumerationBatchResult
  ]
}

/**
  * Live catalog-backed producer. The batch method is invoked exactly once;
  * candidate selection and execution are not part of this boundary.
  */
private[cli] object LivePostgreSqlEnumerationCampaignV2Backend
    extends PostgreSqlEnumerationCampaignV2Backend {
  import PostgreSqlEnumerationCampaignV2BackendFailure._

  override def acquire(
      config: PostgreSqlEnumerationCampaignV2CliConfig,
      request: PostgreSqlEnumerationBatchRequest
  ): Either[
    PostgreSqlEnumerationCampaignV2BackendFailure,
    PostgreSqlEnumerationBatchResult
  ] = {
    require(config != null, "acquisition backend config must not be null")
    require(request != null, "acquisition batch request must not be null")
    LivePostgreSqlConnectivityProbe
      .verify(config.jdbc, config.schemaName)
      .left
      .map(_ => PostgreSqlUnavailable)
      .flatMap { _ =>
        PostgreSqlQueryRewriter
          .forPostgreSqlJdbc(
            jdbcUrl = config.jdbc.jdbcUrl,
            user = config.jdbc.user,
            password = config.jdbc.password,
            schemaName = config.schemaName
          )
          .left
          .map(_ => RewriterConfigurationFailed)
          // This backend is part of the frozen v2 producer and therefore
          // retains its pre-hardening SQL hashes. A future v3 backend must call
          // the statistics-safe batch boundary under a new study identity.
          .map(_.produceEnumerationBatchLegacyV2(request))
      }
  }
}
