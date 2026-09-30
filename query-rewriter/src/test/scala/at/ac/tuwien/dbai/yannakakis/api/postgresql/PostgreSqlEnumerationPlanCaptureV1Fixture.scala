package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets

/** Test-only bridge into the frozen walker's private constructor boundary. */
object PostgreSqlEnumerationPlanCaptureV1Fixture {
  val SearchPath: String = "pg_catalog, fixture, pg_temp"

  def rawPlan: Array[Byte] =
    ("[{\"Plan\":{" +
      "\"Async Capable\":false," +
      "\"Disabled\":false," +
      "\"Filter\":\"'λ'::text IS NOT NULL\"," +
      "\"Node Type\":\"Result\"," +
      "\"Parallel Aware\":false," +
      "\"Plan Rows\":1," +
      "\"Plan Width\":1," +
      "\"Startup Cost\":0," +
      "\"Total Cost\":1}," +
      "\"Settings\":{\"search_path\":\"" + SearchPath + "\"}}]")
      .getBytes(StandardCharsets.UTF_8)

  def batch(
      request: PostgreSqlEnumerationCampaignV2WholePlanRequest
  ): PostgreSqlEnumerationCampaignV2WholePlanBatch = {
    val values = Map(
      "client_encoding" -> "UTF8",
      "datestyle" -> "ISO, MDY",
      "extra_float_digits" -> "3",
      "search_path" -> SearchPath,
      "server_encoding" -> "UTF8",
      "standard_conforming_strings" -> "on",
      "statement_timeout" -> "10s",
      "timezone" -> "UTC",
      "transaction_isolation" -> "repeatable read",
      "transaction_read_only" -> "on"
    )
    val settings = PostgreSqlEnumerationCampaignV2WholePlanProvenance.SettingNames
      .map(name => PostgreSqlEnumerationCampaignV2ObservedSetting(name, Some(values.getOrElse(name, "fixture"))))
    val provenance = PostgreSqlEnumerationCampaignV2WholePlanProvenance(
      serverVersionNum = 180004,
      databaseEncoding = "UTF8",
      clientEncoding = "UTF8",
      settings = settings,
      settingsFingerprintSha256 =
        PostgreSqlEnumerationCampaignV2WholePlanProvenance.fingerprint(settings),
      settingsFingerprintScheme =
        PostgreSqlEnumerationCampaignV2WholePlanProvenance.FingerprintScheme,
      settingCount = settings.size
    )
    val raw = rawPlan
    val document = PostgreSqlEnumerationCampaignV2PlanWalker
      .validate(raw, PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.default)
      .fold(problem => throw new IllegalStateException(problem.id), identity)
    val observations = request.actions.map { action =>
      PostgreSqlEnumerationCampaignV2WholePlanObservation(
        action.actionId,
        action.actionKind,
        action.renderedStatementSha256,
        document
      )
    }
    PostgreSqlEnumerationCampaignV2WholePlanBatch(
      provenance,
      observations,
      raw.length.toLong * observations.size,
      PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.default
    )
  }
}
