package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlEnumerationBatchResult

/** Both v2 bundle children derived from one exact classified producer result. */
private[cli] final case class PostgreSqlEnumerationSearchReplayBundleV2Pair(
    packageAssembly: PostgreSqlEnumerationCampaignV2Assembly,
    searchInput: PostgreSqlEnumerationCampaignV2JsonDocument,
    eligible: PostgreSqlEnumerationCampaignV2EligibleResult
) {
  require(packageAssembly != null, "v2 package assembly must not be null")
  require(searchInput != null, "v2 search input must not be null")
  require(eligible != null, "v2 eligible result must not be null")

  val acquisitionStatus: String = eligible match {
    case _: PostgreSqlEnumerationCampaignV2EligibleResult.Found =>
      PostgreSqlEnumerationSearchReplayBundleV2Pair.FoundStatus
    case _: PostgreSqlEnumerationCampaignV2EligibleResult.NativeFallback =>
      PostgreSqlEnumerationSearchReplayBundleV2Pair.FallbackStatus
  }
}

private[cli] object PostgreSqlEnumerationSearchReplayBundleV2Pair {
  val FoundStatus: String = "found-actions"
  val FallbackStatus: String = "typed-native-fallback"
}

/** Pure additive one-classification boundary for the complete eligible ADT. */
private[cli] object PostgreSqlEnumerationSearchReplayBundleV2Acquisition {
  import PostgreSqlEnumerationCampaignV2AcquisitionFailure._

  def assemblePair(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult,
      cumulativeLimits: PostgreSqlEnumerationCumulativeByteLimits =
        PostgreSqlEnumerationCumulativeByteLimits.default
  ): Either[
    PostgreSqlEnumerationCampaignV2AcquisitionFailure,
    PostgreSqlEnumerationSearchReplayBundleV2Pair
  ] = {
    require(authoring != null, "v2 authoring request must not be null")
    require(result != null, "v2 enumeration result must not be null")
    require(cumulativeLimits != null, "v2 cumulative limits must not be null")

    PostgreSqlEnumerationCampaignV2Packageability
      .classify(authoring, result)
      .flatMap(assembleEligiblePair(authoring, _, cumulativeLimits))
  }

  /**
    * Assembly after a caller has made the one exact packageability decision.
    * The legacy wrapper above retains its byte-for-byte behavior; the
    * disposition command uses this entry point so it never classifies the
    * producer result a second time.
    */
  def assembleEligiblePair(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      eligible: PostgreSqlEnumerationCampaignV2EligibleResult,
      cumulativeLimits: PostgreSqlEnumerationCumulativeByteLimits =
        PostgreSqlEnumerationCumulativeByteLimits.default
  ): Either[
    PostgreSqlEnumerationCampaignV2AcquisitionFailure,
    PostgreSqlEnumerationSearchReplayBundleV2Pair
  ] = {
    require(authoring != null, "v2 authoring request must not be null")
    require(eligible != null, "v2 eligible result must not be null")
    require(cumulativeLimits != null, "v2 cumulative limits must not be null")

    PostgreSqlEnumerationCampaignV2Acquisition
      .assembleEligible(authoring, eligible, cumulativeLimits)
      .flatMap { assembly =>
        try {
          Right(
            PostgreSqlEnumerationSearchReplayBundleV2Pair(
              packageAssembly = assembly,
              searchInput =
                PostgreSqlEnumerationSearchReplayV2Json.project(eligible),
              eligible = eligible
            )
          )
        } catch {
          case _: IllegalArgumentException =>
            Left(ProjectionRejected(eligible.completedPhase))
        }
      }
  }
}
