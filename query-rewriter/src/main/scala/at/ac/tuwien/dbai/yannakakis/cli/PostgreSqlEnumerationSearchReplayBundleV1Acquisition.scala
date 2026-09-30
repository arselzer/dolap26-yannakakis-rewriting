package at.ac.tuwien.dbai.yannakakis.cli

import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlEnumerationBatchArtifact,
  PostgreSqlEnumerationBatchResult
}

/**
  * Both pre-publication documents derived from one exact produced artifact.
  * The pair contains no fallback representation: an independent search replay
  * needs the complete occurrence-distinct search input.
  */
private[yannakakis] final case class PostgreSqlEnumerationSearchReplayBundleV1Pair(
    packageAssembly: PostgreSqlEnumerationCampaignV2Assembly,
    searchInput: PostgreSqlEnumerationCampaignV2JsonDocument,
    artifact: PostgreSqlEnumerationBatchArtifact
) {
  require(packageAssembly != null, "paired package assembly must not be null")
  require(searchInput != null, "paired search input must not be null")
  require(artifact != null, "paired produced artifact must not be null")
}

/** Pure, additive one-classification boundary for paired acquisition. */
private[yannakakis] object PostgreSqlEnumerationSearchReplayBundleV1Acquisition {
  import PostgreSqlEnumerationCampaignV2AcquisitionFailure._
  import PostgreSqlEnumerationCampaignV2EligibleResult._

  def assembleFoundPair(
      authoring: PostgreSqlEnumerationCampaignV2AuthoringRequest,
      result: PostgreSqlEnumerationBatchResult,
      cumulativeLimits: PostgreSqlEnumerationCumulativeByteLimits =
        PostgreSqlEnumerationCumulativeByteLimits.default
  ): Either[
    PostgreSqlEnumerationCampaignV2AcquisitionFailure,
    PostgreSqlEnumerationSearchReplayBundleV1Pair
  ] = {
    require(authoring != null, "authoring request must not be null")
    require(result != null, "enumeration result must not be null")
    require(cumulativeLimits != null, "cumulative limits must not be null")

    PostgreSqlEnumerationCampaignV2Packageability
      .classify(authoring, result)
      .flatMap {
        case found @ Found(artifact) =>
          PostgreSqlEnumerationCampaignV2Acquisition
            .assembleEligible(authoring, found, cumulativeLimits)
            .flatMap { assembly =>
              try {
                Right(
                  PostgreSqlEnumerationSearchReplayBundleV1Pair(
                    packageAssembly = assembly,
                    searchInput =
                      PostgreSqlEnumerationSearchReplayV1Json.project(artifact),
                    artifact = artifact
                  )
                )
              } catch {
                case _: IllegalArgumentException =>
                  Left(ProjectionRejected(found.completedPhase))
              }
            }
        case other => Left(UnpackageableResult(other.completedPhase))
      }
  }
}
