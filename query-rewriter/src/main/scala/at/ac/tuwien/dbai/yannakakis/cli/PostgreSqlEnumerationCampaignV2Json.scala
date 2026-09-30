package at.ac.tuwien.dbai.yannakakis.api.postgresql {

  import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
    CandidatePlanDecision,
    CandidatePlanId
  }

  /**
    * Minimal immutable view across the PostgreSQL-package compiler boundary.
    *
    * The campaign serializer deliberately receives only exact identities and
    * SQL/stage bytes. It cannot observe the compiler objects, choose a winner,
    * or project execution outcomes.
    */
  private[yannakakis] object PostgreSqlEnumerationCampaignV2ProjectionView {
    final case class SoftLimits(
        maxCovers: BigInt,
        maxCoverPairs: BigInt,
        maxComponents: BigInt,
        maxDerivations: BigInt,
        maxCandidates: BigInt,
        maxCandidateCoverChecks: BigInt
    )

    final case class CandidateLimits(
        maxBlocks: BigInt,
        maxBasisChecks: BigInt,
        maxBasisAlternatives: BigInt,
        maxWitnessNodes: BigInt,
        maxPlanLimit: BigInt,
        maxRetainedPartialPlans: BigInt,
        maxCombinationStates: BigInt
    )

    final case class BagFactorLimits(
        maxBags: BigInt,
        maxOwnershipChecks: BigInt,
        maxTreeEdges: BigInt,
        maxSourceEdges: BigInt
    )

    final case class RenderingLimits(
        maxExpandedScans: BigInt,
        maxRenderedTerms: BigInt,
        maxCtes: BigInt,
        maxSqlCharacters: BigInt,
        characterCountScheme: String
    )

    final case class StageDiagnosticLimits(
        maxStatements: BigInt,
        maxCharactersPerStatement: BigInt,
        maxTotalCharacters: BigInt,
        characterCountScheme: String
    )

    final case class TransportLimits(
        encoding: String,
        maxSourceSqlBytes: BigInt,
        maxGeneratedOrStageSqlBytes: BigInt,
        maxCanonicalJsonDocumentBytes: BigInt
    )

    final case class SearchContract(
        algorithm: String,
        structuralUniverse: String,
        ranking: String,
        physicalCoverPolicy: String,
        exhaustionScope: String,
        coverDistinctAssignmentsEnumerated: Boolean,
        widthBound: Int,
        requireConnectedCover: Boolean,
        soft: SoftLimits,
        candidate: CandidateLimits,
        bagFactor: BagFactorLimits,
        rendering: RenderingLimits,
        stageDiagnostics: StageDiagnosticLimits,
        transport: TransportLimits
    )

    final case class SoftGenerationCounters(
        coverCount: BigInt,
        coverPairCount: BigInt,
        componentCount: BigInt,
        derivationCount: BigInt,
        generatedCandidateBagCount: BigInt,
        candidateCoverCheckCount: BigInt,
        generatedCandidateCoverAlternativeCount: BigInt,
        admissibleCandidateBagCount: BigInt,
        admissibleCandidateCoverAlternativeCount: BigInt
    )

    final case class CandidateSearchCounters(
        candidateBagCount: BigInt,
        blockCount: BigInt,
        basisCheckCount: BigInt,
        basisAlternativeCount: BigInt,
        satisfiedBlockCount: BigInt,
        retainedPartialPlanCount: BigInt,
        combinationStateCount: BigInt,
        maximumWitnessNodeCount: BigInt
    )

    final case class BagFactorCounters(
        bagCount: BigInt,
        ownershipCheckCount: BigInt,
        treeEdgeCount: BigInt,
        sourceEdgeCount: BigInt
    )

    final case class RenderingCounters(
        expandedScanCount: BigInt,
        renderedTermCount: BigInt,
        cteCount: BigInt,
        sqlCharacterCount: BigInt
    )

    final case class StageDiagnosticCounters(
        statementCount: BigInt,
        maximumStatementCharacterCount: BigInt,
        totalStatementCharacterCount: BigInt
    )

    final case class Stage(
        stageId: String,
        sql: String,
        statementSha256: String
    )

    final case class Candidate(
        structuralRank: Int,
        candidateId: String,
        canonicalDecompositionJson: String,
        physicalVariantId: String,
        renderedSql: String,
        renderedSqlSha256: String,
        orderedStages: Vector[Stage],
        bagFactorCounters: BagFactorCounters,
        renderingCounters: RenderingCounters,
        stageDiagnosticCounters: StageDiagnosticCounters
    )

    final case class Artifact(
        sourceSql: String,
        sourceSqlSha256: String,
        requestedLimit: Int,
        exhaustedBeforeK: Boolean,
        completedPhase: String,
        searchContract: SearchContract,
        softGenerationCounters: SoftGenerationCounters,
        candidateSearchCounters: CandidateSearchCounters,
        candidates: Vector[Candidate]
    )

    def from(
        artifact: PostgreSqlEnumerationBatchArtifact
    ): Artifact = {
      require(artifact != null, "enumeration artifact must not be null")
      val batch = artifact.batch
      val contract = searchContractFromEvidence(artifact.producerSearchContract)
      val softCounters = softGenerationCounters(artifact, contract.soft)
      val searchCounters = candidateSearchCounters(
        artifact,
        contract.candidate
      )
      val sourceVertexOccurrences = artifact.hypergraph.edges.iterator
        .foldLeft(BigInt(0))((total, edge) => total + edge.vertices.size)
      val candidates = batch.candidates.map { candidate =>
        val stages = candidate.stageDiagnostics.statements.map { stage =>
          Stage(stage.stageId.code, stage.sql, stage.statementSha256)
        }
        require(
          stages.nonEmpty &&
            stages.last.stageId == "final-reconstruction" &&
            stages.last.sql == candidate.renderedQuery.sql,
          "campaign stage view must end in the exact rendered SQL bytes"
        )
        Candidate(
          candidate.plan.rank,
          candidate.plan.candidateId.value,
          CandidatePlanId.canonicalJson(candidate.plan.decomposition),
          candidate.physicalVariantId.value,
          candidate.renderedQuery.sql,
          candidate.renderedStatementSha256,
          stages,
          bagFactorCounters(
            candidate,
            sourceVertexOccurrences,
            contract.bagFactor
          ),
          renderingCounters(candidate, contract.rendering),
          stageDiagnosticCounters(candidate, contract.stageDiagnostics)
        )
      }
      Artifact(
        artifact.request.originalSql,
        artifact.request.originalSqlSha256,
        batch.requestedLimit,
        batch.exhaustedBeforeK,
        PostgreSqlRewritePhase.Rendered.id,
        contract,
        softCounters,
        searchCounters,
        candidates
      )
    }

    private[yannakakis] def searchContractFromEvidence(
        evidence: PostgreSqlEnumerationProducerSearchContract
    ): SearchContract = {
      require(evidence != null, "producer search contract must not be null")
      SearchContract(
        evidence.algorithm,
        evidence.structuralUniverse,
        evidence.ranking,
        evidence.physicalCoverPolicy,
        evidence.exhaustionScope,
        evidence.coverDistinctAssignmentsEnumerated,
        evidence.widthBound,
        evidence.requireConnectedCover,
        SoftLimits(
          BigInt(evidence.soft.maxCovers),
          BigInt(evidence.soft.maxCoverPairs),
          BigInt(evidence.soft.maxComponents),
          BigInt(evidence.soft.maxDerivations),
          BigInt(evidence.soft.maxCandidates),
          BigInt(evidence.soft.maxCandidateCoverChecks)
        ),
        CandidateLimits(
          BigInt(evidence.candidate.maxBlocks),
          BigInt(evidence.candidate.maxBasisChecks),
          BigInt(evidence.candidate.maxBasisAlternatives),
          BigInt(evidence.candidate.maxWitnessNodes),
          BigInt(evidence.candidate.maxPlanLimit),
          BigInt(evidence.candidate.maxRetainedPartialPlans),
          BigInt(evidence.candidate.maxCombinationStates)
        ),
        BagFactorLimits(
          BigInt(evidence.bagFactor.maxBags),
          BigInt(evidence.bagFactor.maxOwnershipChecks),
          BigInt(evidence.bagFactor.maxTreeEdges),
          BigInt(evidence.bagFactor.maxSourceEdges)
        ),
        RenderingLimits(
          BigInt(evidence.rendering.maxExpandedScans),
          BigInt(evidence.rendering.maxRenderedTerms),
          BigInt(evidence.rendering.maxCtes),
          BigInt(evidence.rendering.maxSqlCharacters),
          evidence.rendering.characterCountScheme
        ),
        StageDiagnosticLimits(
          BigInt(evidence.stageDiagnostics.maxStatements),
          BigInt(evidence.stageDiagnostics.maxCharactersPerStatement),
          BigInt(evidence.stageDiagnostics.maxTotalCharacters),
          evidence.stageDiagnostics.characterCountScheme
        ),
        TransportLimits(
          evidence.transport.encoding,
          BigInt(evidence.transport.maxSourceSqlBytes),
          BigInt(evidence.transport.maxGeneratedOrStageSqlBytes),
          BigInt(evidence.transport.maxCanonicalJsonDocumentBytes)
        )
      )
    }

    private def softGenerationCounters(
        artifact: PostgreSqlEnumerationBatchArtifact,
        limits: SoftLimits
    ): SoftGenerationCounters =
      softGenerationCountersFromEvidence(
        artifact.softGeneration,
        artifact.admissibleGeneration,
        artifact.hypergraph.isEmpty,
        limits
      )

    private[yannakakis] def softGenerationCountersFromResult(
        result: PostgreSqlEnumerationBatchResult
    ): SoftGenerationCounters = {
      require(result != null, "enumeration batch result must not be null")
      val report = result.pipelineEvidence
      val contract = searchContractFromEvidence(result.producerSearchContract)
      softGenerationCountersFromEvidence(
        report.softGeneration.getOrElse(
          throw new IllegalArgumentException(
            "fallback result must retain completed soft generation"
          )
        ),
        report.admissibleGeneration.getOrElse(
          throw new IllegalArgumentException(
            "fallback result must retain admissible soft generation"
          )
        ),
        report.hypergraph.exists(_.isEmpty),
        contract.soft
      )
    }

    private def softGenerationCountersFromEvidence(
        generation: at.ac.tuwien.dbai.yannakakis.algorithm.soft.SoftBagGeneration,
        admissible: at.ac.tuwien.dbai.yannakakis.algorithm.soft.AdmissibleSoftBagGeneration,
        hypergraphIsEmpty: Boolean,
        limits: SoftLimits
    ): SoftGenerationCounters = {
      val coverCount = BigInt(generation.enumeratedCovers.size)
      require(coverCount > 0, "soft generation must retain a cover universe")
      val derivationCount = generation.candidates.iterator.foldLeft(BigInt(0)) {
        (total, candidate) => total + candidate.witnesses.size
      }
      require(
        derivationCount % coverCount == 0,
        "soft derivations must retain an exact component-by-cover product"
      )
      val physicalCoverCount =
        if (hypergraphIsEmpty) coverCount
        else BigInt(generation.enumeratedCovers.count(_.nonEmpty))
      val result = SoftGenerationCounters(
        coverCount = coverCount,
        coverPairCount = coverCount * coverCount,
        componentCount = derivationCount / coverCount,
        derivationCount = derivationCount,
        generatedCandidateBagCount = BigInt(generation.candidates.size),
        candidateCoverCheckCount =
          BigInt(generation.candidates.size) * physicalCoverCount,
        generatedCandidateCoverAlternativeCount =
          generation.candidates.iterator.foldLeft(BigInt(0)) {
            (total, candidate) => total + candidate.covers.size
          },
        admissibleCandidateBagCount = BigInt(admissible.candidates.size),
        admissibleCandidateCoverAlternativeCount =
          admissible.candidates.iterator.foldLeft(BigInt(0)) {
            (total, candidate) => total + candidate.physicalCovers.size
          }
      )
      requireWithin(result.coverCount, limits.maxCovers, "soft covers")
      requireWithin(
        result.coverPairCount,
        limits.maxCoverPairs,
        "soft cover pairs"
      )
      requireWithin(
        result.componentCount,
        limits.maxComponents,
        "soft components"
      )
      requireWithin(
        result.derivationCount,
        limits.maxDerivations,
        "soft derivations"
      )
      requireWithin(
        result.generatedCandidateBagCount,
        limits.maxCandidates,
        "soft candidate bags"
      )
      requireWithin(
        result.candidateCoverCheckCount,
        limits.maxCandidateCoverChecks,
        "soft candidate-cover checks"
      )
      require(
        result.admissibleCandidateBagCount <=
          result.generatedCandidateBagCount &&
          result.admissibleCandidateCoverAlternativeCount <=
            result.generatedCandidateCoverAlternativeCount,
        "admissibility projection must not invent bags or cover alternatives"
      )
      result
    }

    private def candidateSearchCounters(
        artifact: PostgreSqlEnumerationBatchArtifact,
        limits: CandidateLimits
    ): CandidateSearchCounters =
      candidateSearchCountersFromEvidence(
        artifact.batch.search,
        artifact.candidateInput.candidateBags.size,
        maximumWitnessNodeCount =
          artifact.batch.search.plans.iterator.map(_.witness.nodeCount).max,
        limits
      )

    private[yannakakis] def rejectedCandidateSearchCountersFromResult(
        result: PostgreSqlEnumerationBatchResult
    ): CandidateSearchCounters = {
      require(result != null, "enumeration batch result must not be null")
      val report = result.pipelineEvidence
      val rejected = report.candidateDecision.collect {
        case value: CandidatePlanDecision.Rejected => value
      }.getOrElse(
        throw new IllegalArgumentException(
          "fallback result must retain an exact rejected candidate search"
        )
      )
      val candidateInput = report.candidateInput.getOrElse(
        throw new IllegalArgumentException(
          "rejected fallback must retain its candidate-search input"
        )
      )
      val contract = searchContractFromEvidence(result.producerSearchContract)
      candidateSearchCountersFromEvidence(
        rejected,
        candidateInput.candidateBags.size,
        maximumWitnessNodeCount = 0,
        contract.candidate
      )
    }

    private def candidateSearchCountersFromEvidence(
        decision: CandidatePlanDecision,
        candidateInputBagCount: Int,
        maximumWitnessNodeCount: BigInt,
        limits: CandidateLimits
    ): CandidateSearchCounters = {
      require(decision != null, "candidate decision must not be null")
      val (requestedLimit, statistics) = decision match {
        case found: CandidatePlanDecision.Found =>
          (found.requestedLimit, found.statistics)
        case rejected: CandidatePlanDecision.Rejected =>
          (rejected.requestedLimit, rejected.statistics)
      }
      val base = statistics.base
      val candidateBagCount = BigInt(base.candidateBagCount)
      val blockCount = BigInt(base.blockCount)
      val basisCheckCount =
        (blockCount - candidateBagCount + 1) * candidateBagCount
      val result = CandidateSearchCounters(
        candidateBagCount = candidateBagCount,
        blockCount = blockCount,
        basisCheckCount = basisCheckCount,
        basisAlternativeCount = BigInt(base.basisAlternativeCount),
        satisfiedBlockCount = BigInt(base.satisfiedBlockCount),
        retainedPartialPlanCount =
          BigInt(statistics.retainedPartialPlanCount),
        combinationStateCount = BigInt(statistics.combinationStateCount),
        maximumWitnessNodeCount = maximumWitnessNodeCount
      )
      require(
        result.candidateBagCount ==
          BigInt(candidateInputBagCount) &&
          result.blockCount >= result.candidateBagCount &&
          result.satisfiedBlockCount <= result.blockCount,
        "CandidateTopK statistics must match the exact search input"
      )
      requireWithin(
        result.candidateBagCount,
        limits.maxBlocks,
        "candidate bags"
      )
      requireWithin(result.blockCount, limits.maxBlocks, "candidate blocks")
      requireWithin(
        result.basisCheckCount,
        limits.maxBasisChecks,
        "candidate basis checks"
      )
      requireWithin(
        result.basisAlternativeCount,
        limits.maxBasisAlternatives,
        "candidate basis alternatives"
      )
      requireWithin(
        result.maximumWitnessNodeCount,
        limits.maxWitnessNodes,
        "candidate witness nodes"
      )
      requireWithin(
        BigInt(requestedLimit),
        limits.maxPlanLimit,
        "candidate plan limit"
      )
      requireWithin(
        result.retainedPartialPlanCount,
        limits.maxRetainedPartialPlans,
        "candidate retained partial plans"
      )
      requireWithin(
        result.combinationStateCount,
        limits.maxCombinationStates,
        "candidate combination states"
      )
      result
    }

    private def bagFactorCounters(
        candidate: PostgreSqlCompiledRetainedCandidate,
        sourceVertexOccurrences: BigInt,
        limits: BagFactorLimits
    ): BagFactorCounters = {
      val plan = candidate.bagFactorPlan
      val bagCount = BigInt(plan.decomposition.bags.size)
      val result = BagFactorCounters(
        bagCount = bagCount,
        ownershipCheckCount = bagCount.max(BigInt(1)) * sourceVertexOccurrences,
        treeEdgeCount = BigInt(plan.decomposition.treeEdges.size),
        sourceEdgeCount = BigInt(plan.sourceHypergraph.edges.size)
      )
      require(
        BigInt(plan.factors.size) == result.bagCount &&
          BigInt(plan.ownership.size) == result.sourceEdgeCount,
        "bag-factor counters must match the exact compiled physical plan"
      )
      requireWithin(result.bagCount, limits.maxBags, "bag-factor bags")
      requireWithin(
        result.ownershipCheckCount,
        limits.maxOwnershipChecks,
        "bag-factor ownership checks"
      )
      requireWithin(
        result.treeEdgeCount,
        limits.maxTreeEdges,
        "bag-factor tree edges"
      )
      requireWithin(
        result.sourceEdgeCount,
        limits.maxSourceEdges,
        "bag-factor source edges"
      )
      result
    }

    private def renderingCounters(
        candidate: PostgreSqlCompiledRetainedCandidate,
        limits: RenderingLimits
    ): RenderingCounters = {
      val statistics = candidate.renderedQuery.statistics
      val result = RenderingCounters(
        expandedScanCount = BigInt(statistics.expandedScanCount),
        renderedTermCount = BigInt(statistics.renderedTermCount),
        cteCount = BigInt(statistics.cteCount),
        sqlCharacterCount = BigInt(statistics.sqlCharacterCount)
      )
      require(
        result.sqlCharacterCount == BigInt(candidate.renderedQuery.sql.length),
        "rendering character count must bind the exact rendered SQL string"
      )
      requireWithin(
        result.expandedScanCount,
        limits.maxExpandedScans,
        "rendered expanded scans"
      )
      requireWithin(
        result.renderedTermCount,
        limits.maxRenderedTerms,
        "rendered terms"
      )
      requireWithin(result.cteCount, limits.maxCtes, "rendered CTEs")
      requireWithin(
        result.sqlCharacterCount,
        limits.maxSqlCharacters,
        "rendered SQL characters"
      )
      result
    }

    private def stageDiagnosticCounters(
        candidate: PostgreSqlCompiledRetainedCandidate,
        limits: StageDiagnosticLimits
    ): StageDiagnosticCounters = {
      val statements = candidate.stageDiagnostics.statements
      require(statements.nonEmpty, "stage diagnostics must be nonempty")
      require(
        statements.forall(statement =>
          BigInt(statement.characterCount) == BigInt(statement.sql.length)
        ),
        "stage character counts must bind exact stage SQL strings"
      )
      val characterCounts = statements.map(statement =>
        BigInt(statement.characterCount)
      )
      val result = StageDiagnosticCounters(
        statementCount = BigInt(statements.size),
        maximumStatementCharacterCount = characterCounts.max,
        totalStatementCharacterCount = characterCounts.sum
      )
      requireWithin(
        result.statementCount,
        limits.maxStatements,
        "stage diagnostic statements"
      )
      requireWithin(
        result.maximumStatementCharacterCount,
        limits.maxCharactersPerStatement,
        "stage diagnostic statement characters"
      )
      requireWithin(
        result.totalStatementCharacterCount,
        limits.maxTotalCharacters,
        "stage diagnostic total characters"
      )
      result
    }

    private def requireWithin(
        value: BigInt,
        limit: BigInt,
        label: String
    ): Unit =
      require(
        value >= 0 && value <= limit,
        s"$label consumption must be nonnegative and within its captured limit"
      )
  }
}

package at.ac.tuwien.dbai.yannakakis.cli {

  import java.nio.charset.StandardCharsets
  import java.security.MessageDigest
  import java.util.Arrays
  import java.util.Locale

  import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
  import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId
  import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
    PostgreSqlEnumerationBatchArtifact,
    PostgreSqlEnumerationBatchRequest,
    PostgreSqlEnumerationCampaignV2ProjectionView,
    PostgreSqlEnumerationFeatureAction,
    PostgreSqlEnumerationFeatureState,
    PostgreSqlEnumerationFeatureValue,
    PostgreSqlEnumerationObservedFeatureValue,
    PostgreSqlEnumerationSqlTransport,
    PostgreSqlEnumerationTransportLimitEvidence,
    PostgreSqlEnumerationProducerSearchContract,
    PostgreSqlEnumerationStructuralFeatureExport,
    PostgreSqlEnumerationStructuralFeatures
  }

  /** One immutable canonical JSON document, including its sole final LF. */
  private[yannakakis] final class PostgreSqlEnumerationCampaignV2JsonDocument private[cli] (
      private val canonicalBytes: Array[Byte]
  ) {
    require(
      canonicalBytes != null && canonicalBytes.nonEmpty,
      "canonical JSON document bytes must be nonempty"
    )
    require(
      canonicalBytes.length <=
        PostgreSqlEnumerationCampaignV2Json.MaximumCanonicalJsonBytes,
      "canonical JSON document exceeds the campaign-v2 transport envelope"
    )
    require(
      canonicalBytes.last == '\n'.toByte &&
        canonicalBytes.count(_ == '\n'.toByte) == 1,
      "canonical JSON document must contain exactly one final LF"
    )

    lazy val utf8: String = new String(canonicalBytes, StandardCharsets.UTF_8)
    val byteLength: Int = canonicalBytes.length
    private[cli] val valueByteLength: Int = canonicalBytes.length - 1
    def bytes: Array[Byte] = canonicalBytes.clone()

    private[cli] def copyValueBytes(
        output: Array[Byte],
        start: Int
    ): Int = {
      require(output != null, "canonical JSON copy output must not be null")
      require(
        start >= 0 && start <= output.length - valueByteLength,
        "canonical JSON copy must fit the destination"
      )
      System.arraycopy(canonicalBytes, 0, output, start, valueByteLength)
      start + valueByteLength
    }

    private[cli] def writeValueBytesAsLowerHex(
        output: Array[Byte],
        start: Int,
        alphabet: String
    ): Int = writeBytesAsLowerHex(output, start, alphabet, valueByteLength)

    private[cli] def writeCanonicalBytesAsLowerHex(
        output: Array[Byte],
        start: Int,
        alphabet: String
    ): Int = writeBytesAsLowerHex(output, start, alphabet, canonicalBytes.length)

    private def writeBytesAsLowerHex(
        output: Array[Byte],
        start: Int,
        alphabet: String,
        inputLength: Int
    ): Int = {
      require(output != null, "canonical JSON hex output must not be null")
      require(
        alphabet != null && alphabet.length == 16,
        "canonical JSON hex alphabet must contain sixteen characters"
      )
      require(
        start >= 0 && inputLength <= (output.length - start) / 2,
        "canonical JSON hex output must fit the destination"
      )
      var inputOffset = 0
      var outputOffset = start
      while (inputOffset < inputLength) {
        val unsigned = canonicalBytes(inputOffset) & 0xff
        output(outputOffset) = alphabet.charAt(unsigned >>> 4).toByte
        output(outputOffset + 1) = alphabet.charAt(unsigned & 0x0f).toByte
        inputOffset += 1
        outputOffset += 2
      }
      outputOffset
    }

    private[cli] def updateDigest(digest: MessageDigest): Unit = {
      require(digest != null, "canonical JSON digest must not be null")
      digest.update(canonicalBytes)
    }

    lazy val sha256: String =
      PostgreSqlEnumerationCampaignV2Json.sha256(canonicalBytes)

    override def equals(other: Any): Boolean = other match {
      case that: PostgreSqlEnumerationCampaignV2JsonDocument =>
        Arrays.equals(canonicalBytes, that.canonicalBytes)
      case _ => false
    }

    override def hashCode(): Int = Arrays.hashCode(canonicalBytes)
  }

  /** One registry-aligned, action-keyed feature-value document. */
  private[yannakakis] final case class PostgreSqlEnumerationCampaignV2FeatureBundleDocument(
      actionId: String,
      actionKind: String,
      document: PostgreSqlEnumerationCampaignV2JsonDocument
  )

  /** Exact action SQL and renderer-stage helper documents. */
  private[yannakakis] final case class PostgreSqlEnumerationCampaignV2ActionDocuments(
      actionId: String,
      actionKind: String,
      candidateRank: Option[Int],
      candidateId: Option[String],
      physicalVariantId: Option[String],
      renderedSqlBytesSha256: String,
      renderedSql: PostgreSqlEnumerationCampaignV2JsonDocument,
      orderedStages: PostgreSqlEnumerationCampaignV2JsonDocument
  ) {
    require(
      renderedSqlBytesSha256 != null &&
        renderedSqlBytesSha256.matches("[0-9a-f]{64}"),
      "action SQL bytes must retain a canonical SHA-256"
    )
  }

  /**
    * Outcome-free canonical projection of one verified produced batch.
    *
    * The structural rank is retained only in semantic/action metadata; it is
    * absent from the feature registry and every feature value vector.
    */
  private[yannakakis] final case class PostgreSqlEnumerationCampaignV2Projection(
      sourceSql: PostgreSqlEnumerationCampaignV2JsonDocument,
      semanticSearchCore: PostgreSqlEnumerationCampaignV2JsonDocument,
      producerSearchContract: PostgreSqlEnumerationCampaignV2JsonDocument,
      producerSearchTranscript: PostgreSqlEnumerationCampaignV2JsonDocument,
      canonicalDecompositions: Vector[
        PostgreSqlEnumerationCampaignV2JsonDocument
      ],
      featureRegistry: PostgreSqlEnumerationCampaignV2JsonDocument,
      featureValueBundles: Vector[
        PostgreSqlEnumerationCampaignV2FeatureBundleDocument
      ],
      actions: Vector[PostgreSqlEnumerationCampaignV2ActionDocuments]
  ) {
    require(sourceSql != null, "source SQL document must not be null")
    require(
      semanticSearchCore != null,
      "semantic search core document must not be null"
    )
    require(
      producerSearchContract != null,
      "producer search contract document must not be null"
    )
    require(
      producerSearchTranscript != null,
      "producer search transcript document must not be null"
    )
    require(
      canonicalDecompositions != null &&
        canonicalDecompositions.forall(_ != null),
      "canonical decomposition documents must not be null or contain null"
    )
    require(featureRegistry != null, "feature registry must not be null")
    require(
      featureValueBundles != null &&
        featureValueBundles.nonEmpty &&
        featureValueBundles.forall(_ != null),
      "feature bundles must be nonempty and contain no null"
    )
    require(
      actions != null && actions.nonEmpty && actions.forall(_ != null),
      "action documents must be nonempty and contain no null"
    )
    require(
      canonicalDecompositions.size == actions.size - 1,
      "canonical decomposition documents must cover every retained action"
    )
    require(
      featureValueBundles.map(bundle => (bundle.actionId, bundle.actionKind)) ==
        actions.map(action => (action.actionId, action.actionKind)),
      "feature and SQL documents must cover the same ordered actions"
    )
  }

  /** Small canonical JSON algebra shared only inside the campaign-v2 CLI slice. */
  private[cli] object PostgreSqlEnumerationCampaignV2CanonicalJson {
    sealed trait Value
    case object NullValue extends Value
    private final case class StringValue(value: String) extends Value
    private final case class NumberValue(value: String) extends Value
    private final case class BooleanValue(value: Boolean) extends Value
    private final case class ArrayValue(values: Vector[Value]) extends Value
    private final case class ObjectValue(fields: Vector[(String, Value)])
        extends Value
    private final case class Utf8HexStringValue(
        value: String,
        utf8ByteLength: Long
    ) extends Value
    private final case class EmbeddedDocumentValue(
        document: PostgreSqlEnumerationCampaignV2JsonDocument
    ) extends Value
    private final case class Utf8HexDocumentValue(
        document: PostgreSqlEnumerationCampaignV2JsonDocument
    ) extends Value
    private final case class Utf8HexCanonicalDocumentValue(
        document: PostgreSqlEnumerationCampaignV2JsonDocument
    ) extends Value

    private val HexAlphabet = "0123456789abcdef"
    private val CanonicalDocumentLimitMessage =
      "canonical JSON document exceeds the campaign-v2 transport envelope"

    def string(value: String): Value = {
      require(value != null, "JSON string must not be null")
      requireValidUnicode(value, "JSON string")
      StringValue(value)
    }

    def number(value: BigInt): Value = {
      require(value != null, "JSON integer must not be null")
      NumberValue(value.toString)
    }

    /**
      * Defers UTF-8 hex expansion until the bounded canonical writer runs.
      * The caller-supplied count is checked without allocating encoded bytes.
      */
    private[cli] def utf8HexString(
        value: String,
        expectedByteLength: Long
    ): Value = {
      require(value != null, "UTF-8 hex source must not be null")
      require(
        expectedByteLength >= 0L &&
          exactUtf8ByteLength(value, "UTF-8 hex source") == expectedByteLength,
        "UTF-8 hex source must match its exact byte length"
      )
      Utf8HexStringValue(value, expectedByteLength)
    }

    /** Embeds one already-canonical document without its final LF. */
    private[cli] def embeddedDocument(
        value: PostgreSqlEnumerationCampaignV2JsonDocument
    ): Value = {
      require(value != null, "embedded canonical document must not be null")
      EmbeddedDocumentValue(value)
    }

    /** Streams one canonical document's value bytes as lowercase UTF-8 hex. */
    private[cli] def utf8HexDocument(
        value: PostgreSqlEnumerationCampaignV2JsonDocument
    ): Value = {
      require(value != null, "hex canonical document must not be null")
      Utf8HexDocumentValue(value)
    }

    /** Streams all canonical bytes, including the required final LF, as hex. */
    private[cli] def utf8HexCanonicalDocument(
        value: PostgreSqlEnumerationCampaignV2JsonDocument
    ): Value = {
      require(value != null, "hex canonical document must not be null")
      Utf8HexCanonicalDocumentValue(value)
    }

    def bool(value: Boolean): Value = BooleanValue(value)
    def array(values: Vector[Value]): Value = {
      require(
        values != null && values.forall(_ != null),
        "JSON array must not be null or contain null"
      )
      ArrayValue(values)
    }

    def obj(fields: (String, Value)*): Value = obj(fields.toVector)

    def obj(fields: Vector[(String, Value)]): Value = {
      require(
        fields != null &&
          fields.forall(entry => entry != null && entry._1 != null && entry._2 != null),
        "JSON object fields must not be null or contain null"
      )
      fields.foreach(entry => requireValidUnicode(entry._1, "JSON object key"))
      val sorted = fields.sortBy(_._1)
      require(
        sorted.map(_._1).distinct.size == sorted.size,
        "JSON object keys must be unique"
      )
      ObjectValue(sorted)
    }

    def document(value: Value): PostgreSqlEnumerationCampaignV2JsonDocument = {
      documentWithMaximumBytes(
        value,
        PostgreSqlEnumerationCampaignV2Json.MaximumCanonicalJsonBytes
      )
    }

    /** Testable small-limit entry point for the exact production preflight. */
    private[cli] def documentWithMaximumBytes(
        value: Value,
        maximumBytes: Int
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      require(value != null, "JSON document value must not be null")
      require(
        maximumBytes > 0 &&
          maximumBytes <=
            PostgreSqlEnumerationCampaignV2Json.MaximumCanonicalJsonBytes,
        "canonical JSON byte limit must be within the production envelope"
      )

      // This is deliberately a per-document bound. Cumulative package and
      // batch serialized-byte budgets remain uncertified by this projection.
      val count = new BoundedByteCount(maximumBytes.toLong)
      measure(value, count)
      count.add(1L)

      val output = new Array[Byte](count.value.toInt)
      val end = write(value, output, 0)
      output(end) = '\n'.toByte
      require(
        end + 1 == output.length,
        "canonical JSON preflight and writer must agree exactly"
      )
      new PostgreSqlEnumerationCampaignV2JsonDocument(output)
    }

    private final class BoundedByteCount(maximum: Long) {
      private var current = 0L
      def value: Long = current

      def add(delta: Long): Unit = {
        require(delta >= 0L, "canonical JSON byte increments must be nonnegative")
        if (delta > maximum - current)
          throw new IllegalArgumentException(CanonicalDocumentLimitMessage)
        current += delta
      }

      def addTwice(delta: Long): Unit = {
        require(delta >= 0L, "canonical JSON byte increments must be nonnegative")
        if (delta > (maximum - current) / 2L)
          throw new IllegalArgumentException(CanonicalDocumentLimitMessage)
        current += delta * 2L
      }
    }

    private def measure(value: Value, count: BoundedByteCount): Unit =
      value match {
        case NullValue            => count.add(4L)
        case StringValue(text)    => measureString(text, count)
        case NumberValue(number)  => count.add(number.length.toLong)
        case BooleanValue(flag)   => count.add(if (flag) 4L else 5L)
        case Utf8HexStringValue(_, byteLength) =>
          count.add(2L)
          count.addTwice(byteLength)
        case EmbeddedDocumentValue(document) =>
          count.add(document.valueByteLength.toLong)
        case Utf8HexDocumentValue(document) =>
          count.add(2L)
          count.addTwice(document.valueByteLength.toLong)
        case Utf8HexCanonicalDocumentValue(document) =>
          count.add(2L)
          count.addTwice(document.byteLength.toLong)
        case ArrayValue(values) =>
          count.add(1L)
          values.zipWithIndex.foreach { case (item, index) =>
            if (index > 0) count.add(1L)
            measure(item, count)
          }
          count.add(1L)
        case ObjectValue(fields) =>
          count.add(1L)
          fields.zipWithIndex.foreach { case ((key, item), index) =>
            if (index > 0) count.add(1L)
            measureString(key, count)
            count.add(1L)
            measure(item, count)
          }
          count.add(1L)
      }

    private def measureString(value: String, count: BoundedByteCount): Unit = {
      count.add(1L)
      var index = 0
      while (index < value.length) {
        val character = value.charAt(index)
        character match {
          case '"' | '\\' | '\b' | '\f' | '\n' | '\r' | '\t' =>
            count.add(2L)
          case control if control < ' ' => count.add(6L)
          case high if Character.isHighSurrogate(high) =>
            require(
              index + 1 < value.length &&
                Character.isLowSurrogate(value.charAt(index + 1)),
              "JSON string contains an unpaired UTF-16 surrogate"
            )
            count.add(4L)
            index += 1
          case low if Character.isLowSurrogate(low) =>
            throw new IllegalArgumentException(
              "JSON string contains an unpaired UTF-16 surrogate"
            )
          case ascii if ascii <= 0x7f => count.add(1L)
          case twoByte if twoByte <= 0x7ff => count.add(2L)
          case _ => count.add(3L)
        }
        index += 1
      }
      count.add(1L)
    }

    private def write(value: Value, output: Array[Byte], start: Int): Int =
      value match {
        case NullValue => writeAscii("null", output, start)
        case StringValue(text) => writeString(text, output, start)
        case NumberValue(number) => writeAscii(number, output, start)
        case BooleanValue(flag) =>
          writeAscii(if (flag) "true" else "false", output, start)
        case Utf8HexStringValue(text, _) =>
          writeUtf8HexString(text, output, start)
        case EmbeddedDocumentValue(document) =>
          document.copyValueBytes(output, start)
        case Utf8HexDocumentValue(document) =>
          var offset = writeByte('"', output, start)
          offset = document.writeValueBytesAsLowerHex(
            output,
            offset,
            HexAlphabet
          )
          writeByte('"', output, offset)
        case Utf8HexCanonicalDocumentValue(document) =>
          var offset = writeByte('"', output, start)
          offset = document.writeCanonicalBytesAsLowerHex(
            output,
            offset,
            HexAlphabet
          )
          writeByte('"', output, offset)
        case ArrayValue(values) =>
          var offset = writeByte('[', output, start)
          values.zipWithIndex.foreach { case (item, index) =>
            if (index > 0) offset = writeByte(',', output, offset)
            offset = write(item, output, offset)
          }
          writeByte(']', output, offset)
        case ObjectValue(fields) =>
          var offset = writeByte('{', output, start)
          fields.zipWithIndex.foreach { case ((key, item), index) =>
            if (index > 0) offset = writeByte(',', output, offset)
            offset = writeString(key, output, offset)
            offset = writeByte(':', output, offset)
            offset = write(item, output, offset)
          }
          writeByte('}', output, offset)
      }

    private def writeString(
        value: String,
        output: Array[Byte],
        start: Int
    ): Int = {
      var offset = writeByte('"', output, start)
      var index = 0
      while (index < value.length) {
        val character = value.charAt(index)
        character match {
          case '"'   => offset = writeAscii("\\\"", output, offset)
          case '\\'  => offset = writeAscii("\\\\", output, offset)
          case '\b'  => offset = writeAscii("\\b", output, offset)
          case '\f'  => offset = writeAscii("\\f", output, offset)
          case '\n'  => offset = writeAscii("\\n", output, offset)
          case '\r'  => offset = writeAscii("\\r", output, offset)
          case '\t'  => offset = writeAscii("\\t", output, offset)
          case control if control < ' ' =>
            offset = writeUnicodeEscape(control, output, offset)
          case high if Character.isHighSurrogate(high) =>
            val low = value.charAt(index + 1)
            val codePoint = Character.toCodePoint(high, low)
            offset = writeUtf8CodePoint(codePoint, output, offset)
            index += 1
          case other =>
            offset = writeUtf8CodePoint(other.toInt, output, offset)
        }
        index += 1
      }
      writeByte('"', output, offset)
    }

    private def writeUtf8HexString(
        value: String,
        output: Array[Byte],
        start: Int
    ): Int = {
      var offset = writeByte('"', output, start)
      var index = 0
      while (index < value.length) {
        val character = value.charAt(index)
        if (Character.isHighSurrogate(character)) {
          val codePoint = Character.toCodePoint(
            character,
            value.charAt(index + 1)
          )
          offset = writeUtf8CodePointAsHex(codePoint, output, offset)
          index += 1
        } else {
          offset = writeUtf8CodePointAsHex(character.toInt, output, offset)
        }
        index += 1
      }
      writeByte('"', output, offset)
    }

    private def writeUtf8CodePoint(
        codePoint: Int,
        output: Array[Byte],
        start: Int
    ): Int = {
      var offset = start
      if (codePoint <= 0x7f) {
        offset = writeUnsignedByte(codePoint, output, offset)
      } else if (codePoint <= 0x7ff) {
        offset = writeUnsignedByte(0xc0 | (codePoint >>> 6), output, offset)
        offset = writeUnsignedByte(0x80 | (codePoint & 0x3f), output, offset)
      } else if (codePoint <= 0xffff) {
        offset = writeUnsignedByte(0xe0 | (codePoint >>> 12), output, offset)
        offset = writeUnsignedByte(
          0x80 | ((codePoint >>> 6) & 0x3f),
          output,
          offset
        )
        offset = writeUnsignedByte(0x80 | (codePoint & 0x3f), output, offset)
      } else {
        offset = writeUnsignedByte(0xf0 | (codePoint >>> 18), output, offset)
        offset = writeUnsignedByte(
          0x80 | ((codePoint >>> 12) & 0x3f),
          output,
          offset
        )
        offset = writeUnsignedByte(
          0x80 | ((codePoint >>> 6) & 0x3f),
          output,
          offset
        )
        offset = writeUnsignedByte(0x80 | (codePoint & 0x3f), output, offset)
      }
      offset
    }

    private def writeUtf8CodePointAsHex(
        codePoint: Int,
        output: Array[Byte],
        start: Int
    ): Int = {
      var offset = start
      def hexByte(value: Int): Unit = {
        offset = writeByte(
          HexAlphabet.charAt((value >>> 4) & 0x0f),
          output,
          offset
        )
        offset = writeByte(HexAlphabet.charAt(value & 0x0f), output, offset)
      }
      if (codePoint <= 0x7f) {
        hexByte(codePoint)
      } else if (codePoint <= 0x7ff) {
        hexByte(0xc0 | (codePoint >>> 6))
        hexByte(0x80 | (codePoint & 0x3f))
      } else if (codePoint <= 0xffff) {
        hexByte(0xe0 | (codePoint >>> 12))
        hexByte(0x80 | ((codePoint >>> 6) & 0x3f))
        hexByte(0x80 | (codePoint & 0x3f))
      } else {
        hexByte(0xf0 | (codePoint >>> 18))
        hexByte(0x80 | ((codePoint >>> 12) & 0x3f))
        hexByte(0x80 | ((codePoint >>> 6) & 0x3f))
        hexByte(0x80 | (codePoint & 0x3f))
      }
      offset
    }

    private def writeUnicodeEscape(
        value: Char,
        output: Array[Byte],
        start: Int
    ): Int = {
      var offset = writeAscii("\\u", output, start)
      offset = writeByte(
        HexAlphabet.charAt((value.toInt >>> 12) & 0x0f),
        output,
        offset
      )
      offset = writeByte(
        HexAlphabet.charAt((value.toInt >>> 8) & 0x0f),
        output,
        offset
      )
      offset = writeByte(
        HexAlphabet.charAt((value.toInt >>> 4) & 0x0f),
        output,
        offset
      )
      writeByte(HexAlphabet.charAt(value.toInt & 0x0f), output, offset)
    }

    private def writeAscii(
        value: String,
        output: Array[Byte],
        start: Int
    ): Int = {
      var offset = start
      var index = 0
      while (index < value.length) {
        offset = writeByte(value.charAt(index), output, offset)
        index += 1
      }
      offset
    }

    private def writeByte(
        value: Char,
        output: Array[Byte],
        offset: Int
    ): Int = {
      require(value <= 0x7f, "canonical JSON direct bytes must be ASCII")
      output(offset) = value.toByte
      offset + 1
    }

    private def writeUnsignedByte(
        value: Int,
        output: Array[Byte],
        offset: Int
    ): Int = {
      output(offset) = (value & 0xff).toByte
      offset + 1
    }

    private[cli] def exactUtf8ByteLength(value: String, where: String): Long = {
      requireValidUnicode(value, where)
      var length = 0L
      var index = 0
      while (index < value.length) {
        val character = value.charAt(index)
        if (character <= 0x7f) length += 1L
        else if (character <= 0x7ff) length += 2L
        else if (Character.isHighSurrogate(character)) {
          length += 4L
          index += 1
        } else length += 3L
        index += 1
      }
      length
    }

    private[cli] def requireValidUnicode(value: String, where: String): Unit = {
      require(value != null, s"$where must not be null")
      var index = 0
      while (index < value.length) {
        val character = value.charAt(index)
        if (Character.isHighSurrogate(character)) {
          require(
            index + 1 < value.length &&
              Character.isLowSurrogate(value.charAt(index + 1)),
            s"$where contains an unpaired UTF-16 surrogate"
          )
          index += 1
        } else {
          require(
            !Character.isLowSurrogate(character),
            s"$where contains an unpaired UTF-16 surrogate"
          )
        }
        index += 1
      }
    }
  }

  /**
    * Canonical additive projection for the inactive enumeration-campaign-v2
    * registry contract. It does not change or reuse the frozen rewrite JSON.
    */
  private[yannakakis] object PostgreSqlEnumerationCampaignV2Json {
    import PostgreSqlEnumerationCampaignV2CanonicalJson._
    import PostgreSqlEnumerationCampaignV2ProjectionView.{
      Artifact => ArtifactView,
      Candidate => CandidateView,
      SearchContract => SearchContractView,
      Stage => StageView
    }

    val ProtocolVersion: String = "yrw-enumeration-campaign-protocol/v2"
    val SourceSqlSchema: String = "yrw-enumeration-source-sql/v2"
    val ActionSqlSchema: String = "yrw-enumeration-action-sql/v2"
    val FeatureRegistrySchema: String =
      "yrw-enumeration-feature-registry/v2"
    val FeatureValueBundleSchema: String =
      "yrw-enumeration-feature-value-bundle/v2"
    val SemanticSearchCoreSchema: String =
      "yrw-enumeration-semantic-search-core/v2"
    val ProducerSearchContractSchema: String =
      "yrw-enumeration-producer-search-contract/v2"
    val ProducerSearchTranscriptSchema: String =
      "yrw-enumeration-producer-search-transcript/v2"
    val CanonicalDecompositionBytesSchema: String =
      "yrw-enumeration-canonical-decomposition-bytes/v2"
    val StatementHashScheme: String = "sha256_utf8_v1"

    private val MaximumSqlBytes: Int = 16 * 1024 * 1024
    private[cli] val MaximumCanonicalJsonBytes: Int =
      PostgreSqlEnumerationTransportLimitEvidence
        .MaximumCanonicalJsonDocumentBytes
        .toInt
    private val Sha256Pattern = "[0-9a-f]{64}".r
    private final case class ExactUtf8Value(
        value: String,
        byteLength: Long,
        sha256: String
    )

    /**
      * Project one artifact only after proving that its complete producer
      * request is the request committed by the canonical authoring bytes.
      */
    def project(
        artifact: PostgreSqlEnumerationBatchArtifact,
        authoringRequest: PostgreSqlEnumerationCampaignV2AuthoringRequest
    ): PostgreSqlEnumerationCampaignV2Projection = {
      require(artifact != null, "enumeration artifact must not be null")
      require(authoringRequest != null, "authoring request must not be null")
      require(
        artifact.request == authoringRequest.batchRequest,
        "enumeration artifact must derive solely from the authoring request"
      )
      val projection = projectWithUnboundAuthoringDigestForContractTest(
        artifact,
        authoringRequest.sha256
      )
      require(
        projection.sourceSql == authoringRequest.sourceSqlDocument &&
          projection.featureRegistry == authoringRequest.featureRegistryDocument,
        "enumeration projection must retain the authoring source and feature registry"
      )
      projection
    }

    /** Digest-only bridge retained exclusively for isolated serializer tests. */
    private[cli] def projectWithUnboundAuthoringDigestForContractTest(
        artifact: PostgreSqlEnumerationBatchArtifact,
        authoringRequestSha256: String
    ): PostgreSqlEnumerationCampaignV2Projection = {
      require(artifact != null, "enumeration artifact must not be null")
      validateSha256(
        authoringRequestSha256,
        "authoring request SHA-256"
      )

      val view = PostgreSqlEnumerationCampaignV2ProjectionView.from(artifact)
      val exported = PostgreSqlEnumerationStructuralFeatures.export(artifact)
      requireExportMatchesView(exported, view)

      val registry = featureRegistryDocument(exported)
      val featureBundles = featureValueBundleDocuments(exported, registry.sha256)
      val exactSourceSql = exactSql(view.sourceSql, "source SQL")
      val source = sourceSqlDocument(exactSourceSql)
      require(
        exactSourceSql.sha256 == view.sourceSqlSha256,
        "source SQL digest must bind its exact UTF-8 bytes"
      )
      val semanticSearchCore =
        semanticSearchCoreDocument(view, authoringRequestSha256)
      val producerSearchContract = producerSearchContractDocument(
        view.searchContract,
        authoringRequestSha256,
        view.sourceSqlSha256
      )
      val producerSearchTranscript = producerSearchTranscriptDocument(
        view,
        semanticSearchCore.sha256,
        producerSearchContract.sha256
      )
      val nativeAction = PostgreSqlEnumerationCampaignV2ActionDocuments(
        actionId = "native",
        actionKind = "native",
        candidateRank = None,
        candidateId = None,
        physicalVariantId = None,
        renderedSqlBytesSha256 = exactSourceSql.sha256,
        renderedSql = actionSqlDocument(exactSourceSql),
        orderedStages = document(array(Vector.empty))
      )
      val rewrites = view.candidates.map { candidate =>
        val exactRenderedSql = exactSql(
          candidate.renderedSql,
          "rendered action SQL"
        )
        val renderedSql = actionSqlDocument(exactRenderedSql)
        require(
          exactRenderedSql.sha256 == candidate.renderedSqlSha256,
          "rendered SQL digest must bind its exact UTF-8 bytes"
        )
        PostgreSqlEnumerationCampaignV2ActionDocuments(
          actionId = rewriteActionId(candidate.structuralRank),
          actionKind = "retained_candidate",
          candidateRank = Some(candidate.structuralRank),
          candidateId = Some(candidate.candidateId),
          physicalVariantId = Some(candidate.physicalVariantId),
          renderedSqlBytesSha256 = exactRenderedSql.sha256,
          renderedSql = renderedSql,
          orderedStages = orderedStagesDocument(candidate)
        )
      }

      PostgreSqlEnumerationCampaignV2Projection(
        source,
        semanticSearchCore,
        producerSearchContract,
        producerSearchTranscript,
        view.candidates.map(canonicalDecompositionDocument),
        registry,
        featureBundles,
        nativeAction +: rewrites
      )
    }

    private[cli] def projectNativeFallback(
        authoringRequest: PostgreSqlEnumerationCampaignV2AuthoringRequest,
        fallback: PostgreSqlEnumerationCampaignV2EligibleResult.NativeFallback
    ): PostgreSqlEnumerationCampaignV2Projection = {
      require(authoringRequest != null, "authoring request must not be null")
      require(fallback != null, "native fallback must not be null")
      val result = fallback.result
      require(
        result.request == authoringRequest.batchRequest,
        "native fallback must derive solely from the authoring request"
      )
      val exactSourceSql = exactSql(result.request.originalSql, "source SQL")
      require(
        exactSourceSql.sha256 == result.request.originalSqlSha256,
        "native fallback source digest must bind exact UTF-8 bytes"
      )
      val source = sourceSqlDocument(exactSourceSql)
      val exported =
        PostgreSqlEnumerationStructuralFeatures.exportNativeFallback(result)
      val registry = featureRegistryDocument(exported)
      val bundles = featureValueBundleDocuments(exported, registry.sha256)
      val contract = producerSearchContractDocumentForAcquisition(
        result.producerSearchContract,
        result.request.originalSqlSha256,
        result.request,
        authoringRequest
      )
      val core = nativeFallbackSemanticSearchCoreDocument(
        authoringRequest,
        fallback
      )
      val transcript = nativeFallbackTranscriptDocument(
        fallback,
        core.sha256,
        contract.sha256
      )
      val native = PostgreSqlEnumerationCampaignV2ActionDocuments(
        actionId = "native",
        actionKind = "native",
        candidateRank = None,
        candidateId = None,
        physicalVariantId = None,
        renderedSqlBytesSha256 = exactSourceSql.sha256,
        renderedSql = actionSqlDocument(exactSourceSql),
        orderedStages = document(array(Vector.empty))
      )
      require(
        source == authoringRequest.sourceSqlDocument &&
          registry == authoringRequest.featureRegistryDocument &&
          bundles.size == 1,
        "native fallback must retain the authoring source and exact native feature row"
      )
      PostgreSqlEnumerationCampaignV2Projection(
        source,
        core,
        contract,
        transcript,
        Vector.empty,
        registry,
        bundles,
        Vector(native)
      )
    }

    private def nativeFallbackSemanticSearchCoreDocument(
        authoringRequest: PostgreSqlEnumerationCampaignV2AuthoringRequest,
        fallback: PostgreSqlEnumerationCampaignV2EligibleResult.NativeFallback
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      val reason = fallback.reason
      document(
        obj(
          "authoring_request_sha256" -> string(authoringRequest.sha256),
          "candidate_id_scheme" -> string(CandidatePlanId.Scheme),
          "candidates" -> array(Vector.empty),
          "decomposition_hash_scheme" -> string(CandidatePlanId.Scheme),
          "exhausted_before_k" -> bool(false),
          "protocol_version" -> string(ProtocolVersion),
          "rejection" -> obj(
            "code" -> string(reason.code),
            "detail_sha256" -> string(fallback.detailSha256),
            "phase" -> string(reason.phase)
          ),
          "requested_k" -> number(
            BigInt(authoringRequest.batchRequest.planLimit)
          ),
          "schema_version" -> string(SemanticSearchCoreSchema),
          "status" -> string("native-fallback"),
          "termination" -> string("native-fallback")
        )
      )
    }

    private def nativeFallbackTranscriptDocument(
        fallback: PostgreSqlEnumerationCampaignV2EligibleResult.NativeFallback,
        semanticSearchCoreSha256: String,
        producerSearchContractSha256: String
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      validateSha256(
        semanticSearchCoreSha256,
        "native fallback search-core SHA-256"
      )
      validateSha256(
        producerSearchContractSha256,
        "native fallback contract SHA-256"
      )
      document(
        obj(
          "candidate_search" ->
            PostgreSqlEnumerationCampaignV2ReasonDetails
              .candidateSearchValue(fallback.candidateCounters),
          "compiled_candidates" -> array(Vector.empty),
          "completed_phase" -> string(fallback.reason.completedPhase.id),
          "producer_search_contract_sha256" -> string(
            producerSearchContractSha256
          ),
          "protocol_version" -> string(ProtocolVersion),
          "schema_version" -> string(ProducerSearchTranscriptSchema),
          "semantic_search_core_sha256" -> string(
            semanticSearchCoreSha256
          ),
          "soft_generation" ->
            PostgreSqlEnumerationCampaignV2ReasonDetails
              .softGenerationValue(fallback.softCounters)
        )
      )
    }

    def sourceSqlDocument(
        sql: String
    ): PostgreSqlEnumerationCampaignV2JsonDocument =
      sourceSqlDocument(exactSql(sql, "source SQL"))

    private def sourceSqlDocument(
        sql: ExactUtf8Value
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      document(
        obj(
          "schema_version" -> string(SourceSqlSchema),
          "encoding" -> string("utf-8"),
          "byte_length" -> number(BigInt(sql.byteLength)),
          "utf8_hex" -> utf8HexString(sql.value, sql.byteLength),
          "sha256" -> string(sql.sha256)
        )
      )
    }

    def actionSqlDocument(
        sql: String
    ): PostgreSqlEnumerationCampaignV2JsonDocument =
      actionSqlDocument(exactSql(sql, "action SQL"))

    private def actionSqlDocument(
        sql: ExactUtf8Value
    ): PostgreSqlEnumerationCampaignV2JsonDocument =
      document(actionSqlValue(sql))

    private def canonicalDecompositionDocument(
        candidate: CandidateView
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      require(candidate != null, "candidate decomposition view must not be null")
      val canonical = candidate.canonicalDecompositionJson
      require(
        canonical != null && canonical.nonEmpty &&
          !canonical.contains('\n') && !canonical.contains('\r'),
        "canonical decomposition bytes must be nonempty and contain no newline"
      )
      PostgreSqlEnumerationCampaignV2CanonicalJson.requireValidUnicode(
        canonical,
        "canonical decomposition"
      )
      val byteLength =
        PostgreSqlEnumerationCampaignV2CanonicalJson.exactUtf8ByteLength(
          canonical,
          "canonical decomposition"
        )
      require(
        byteLength <= MaximumSqlBytes.toLong,
        "canonical decomposition exceeds the campaign-v2 byte envelope"
      )
      val digest = sha256Utf8(canonical)
      require(
        candidate.candidateId == s"sha256:$digest",
        "canonical decomposition bytes must derive the candidate identity"
      )
      document(
        obj(
          "byte_length" -> number(BigInt(byteLength)),
          "candidate_id" -> string(candidate.candidateId),
          "candidate_rank" -> number(BigInt(candidate.structuralRank)),
          "canonicalization_scheme" -> string(CandidatePlanId.Scheme),
          "encoding" -> string("utf-8"),
          "schema_version" -> string(CanonicalDecompositionBytesSchema),
          "sha256" -> string(digest),
          "utf8_hex" -> utf8HexString(canonical, byteLength)
        )
      )
    }

    private def semanticSearchCoreDocument(
        view: ArtifactView,
        authoringRequestSha256: String
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      require(view != null, "enumeration projection view must not be null")
      require(
        view.candidates.nonEmpty &&
          view.candidates.size <= view.requestedLimit &&
          view.candidates.map(_.structuralRank) ==
            (1 to view.candidates.size).toVector,
        "semantic search core requires one exact contiguous retained prefix"
      )
      require(
        view.exhaustedBeforeK ==
          (view.candidates.size < view.requestedLimit),
        "semantic search exhaustion must be derived from exact retained search"
      )

      val candidates = view.candidates.map { candidate =>
        val identity = candidateIdentity(candidate.candidateId)
        obj(
          "rank" -> number(BigInt(candidate.structuralRank)),
          "candidate_id" -> string(candidate.candidateId),
          "identity_sha256" -> string(identity),
          "decomposition_sha256" -> string(identity)
        )
      }
      document(
        obj(
          "schema_version" -> string(SemanticSearchCoreSchema),
          "protocol_version" -> string(ProtocolVersion),
          "authoring_request_sha256" -> string(authoringRequestSha256),
          "requested_k" -> number(BigInt(view.requestedLimit)),
          "candidate_id_scheme" -> string(CandidatePlanId.Scheme),
          "decomposition_hash_scheme" -> string(CandidatePlanId.Scheme),
          "status" -> string("found"),
          "termination" -> string(
            if (view.exhaustedBeforeK) "search-exhausted" else "reached-k"
          ),
          "exhausted_before_k" -> bool(view.exhaustedBeforeK),
          "rejection" -> NullValue,
          "candidates" -> array(candidates)
        )
      )
    }

    private def producerSearchContractDocument(
        contract: SearchContractView,
        authoringRequestSha256: String,
        sourceSqlSha256: String
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      require(contract != null, "producer search contract view must not be null")
      validateSha256(authoringRequestSha256, "authoring request SHA-256")
      validateSha256(sourceSqlSha256, "source SQL SHA-256")
      require(
        contract.widthBound > 0 &&
          !contract.coverDistinctAssignmentsEnumerated &&
          contract.algorithm == "top-k-candidate-td-v1" &&
          contract.structuralUniverse == "structural-tree-witnesses-v1" &&
          contract.ranking ==
            "node-count-then-canonical-rooted-tree-signature-v1" &&
          contract.physicalCoverPolicy ==
            "canonical-first-admissible-cover-per-bag-v1" &&
          contract.exhaustionScope ==
            "structural-tree-witnesses-only-v1",
        "producer search contract must retain the exact structural-only scope"
      )
      require(
        contract.transport.encoding == "utf-8" &&
          contract.transport.maxSourceSqlBytes == BigInt(MaximumSqlBytes) &&
          contract.transport.maxGeneratedOrStageSqlBytes ==
            BigInt(MaximumSqlBytes) &&
          contract.transport.maxCanonicalJsonDocumentBytes ==
            BigInt(MaximumCanonicalJsonBytes),
        "producer search contract must retain the exact per-item transport limits"
      )

      document(
        obj(
          "authoring_request_sha256" -> string(authoringRequestSha256),
          "limits" -> obj(
            "bag_factor" -> obj(
              "max_bags" -> number(contract.bagFactor.maxBags),
              "max_ownership_checks" -> number(
                contract.bagFactor.maxOwnershipChecks
              ),
              "max_source_edges" -> number(contract.bagFactor.maxSourceEdges),
              "max_tree_edges" -> number(contract.bagFactor.maxTreeEdges)
            ),
            "candidate" -> obj(
              "max_basis_alternatives" -> number(
                contract.candidate.maxBasisAlternatives
              ),
              "max_basis_checks" -> number(
                contract.candidate.maxBasisChecks
              ),
              "max_blocks" -> number(contract.candidate.maxBlocks),
              "max_combination_states" -> number(
                contract.candidate.maxCombinationStates
              ),
              "max_plan_limit" -> number(contract.candidate.maxPlanLimit),
              "max_retained_partial_plans" -> number(
                contract.candidate.maxRetainedPartialPlans
              ),
              "max_witness_nodes" -> number(
                contract.candidate.maxWitnessNodes
              )
            ),
            "rendering" -> obj(
              "character_count_scheme" -> string(
                contract.rendering.characterCountScheme
              ),
              "max_ctes" -> number(contract.rendering.maxCtes),
              "max_expanded_scans" -> number(
                contract.rendering.maxExpandedScans
              ),
              "max_rendered_terms" -> number(
                contract.rendering.maxRenderedTerms
              ),
              "max_sql_characters" -> number(
                contract.rendering.maxSqlCharacters
              )
            ),
            "soft" -> obj(
              "max_candidate_cover_checks" -> number(
                contract.soft.maxCandidateCoverChecks
              ),
              "max_candidates" -> number(contract.soft.maxCandidates),
              "max_components" -> number(contract.soft.maxComponents),
              "max_cover_pairs" -> number(contract.soft.maxCoverPairs),
              "max_covers" -> number(contract.soft.maxCovers),
              "max_derivations" -> number(contract.soft.maxDerivations)
            ),
            "stage_diagnostics" -> obj(
              "character_count_scheme" -> string(
                contract.stageDiagnostics.characterCountScheme
              ),
              "max_characters_per_statement" -> number(
                contract.stageDiagnostics.maxCharactersPerStatement
              ),
              "max_statements" -> number(
                contract.stageDiagnostics.maxStatements
              ),
              "max_total_characters" -> number(
                contract.stageDiagnostics.maxTotalCharacters
              )
            ),
            "transport" -> obj(
              "encoding" -> string(contract.transport.encoding),
              "max_canonical_json_document_bytes" -> number(
                contract.transport.maxCanonicalJsonDocumentBytes
              ),
              "max_generated_or_stage_sql_bytes" -> number(
                contract.transport.maxGeneratedOrStageSqlBytes
              ),
              "max_source_sql_bytes" -> number(
                contract.transport.maxSourceSqlBytes
              )
            )
          ),
          "protocol_version" -> string(ProtocolVersion),
          "schema_version" -> string(ProducerSearchContractSchema),
          "search" -> obj(
            "algorithm" -> string(contract.algorithm),
            "cover_distinct_assignments_enumerated" -> bool(
              contract.coverDistinctAssignmentsEnumerated
            ),
            "exhaustion_scope" -> string(contract.exhaustionScope),
            "physical_cover_policy" -> string(contract.physicalCoverPolicy),
            "ranking" -> string(contract.ranking),
            "require_connected_cover" -> bool(
              contract.requireConnectedCover
            ),
            "structural_universe" -> string(contract.structuralUniverse),
            "width_bound" -> number(BigInt(contract.widthBound))
          ),
          "source_sql_sha256" -> string(sourceSqlSha256)
        )
      )
    }

    private[cli] def producerSearchContractDocumentForAcquisition(
        evidence: PostgreSqlEnumerationProducerSearchContract,
        sourceSqlSha256: String,
        batchRequest: PostgreSqlEnumerationBatchRequest,
        authoringRequest: PostgreSqlEnumerationCampaignV2AuthoringRequest
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      require(evidence != null, "producer search evidence must not be null")
      require(batchRequest != null, "producer batch request must not be null")
      require(authoringRequest != null, "authoring request must not be null")
      require(
        batchRequest == authoringRequest.batchRequest &&
          sourceSqlSha256 == batchRequest.originalSqlSha256,
        "producer search contract must derive solely from the authoring request"
      )
      producerSearchContractDocument(
        PostgreSqlEnumerationCampaignV2ProjectionView
          .searchContractFromEvidence(evidence),
        authoringRequest.sha256,
        sourceSqlSha256
      )
    }

    private def producerSearchTranscriptDocument(
        view: ArtifactView,
        semanticSearchCoreSha256: String,
        producerSearchContractSha256: String
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      require(view != null, "producer search transcript view must not be null")
      validateSha256(
        semanticSearchCoreSha256,
        "semantic search core document SHA-256"
      )
      validateSha256(
        producerSearchContractSha256,
        "producer search contract document SHA-256"
      )
      require(
        view.completedPhase == "rendered" &&
          view.candidates.nonEmpty &&
          view.candidates.map(_.structuralRank) ==
            (1 to view.candidates.size).toVector,
        "success transcript must contain one fully rendered retained prefix"
      )

      val soft = view.softGenerationCounters
      val search = view.candidateSearchCounters
      val compiled = view.candidates.map { candidate =>
        obj(
          "bag_factor_counters" -> obj(
            "bag_count" -> number(candidate.bagFactorCounters.bagCount),
            "ownership_check_count" -> number(
              candidate.bagFactorCounters.ownershipCheckCount
            ),
            "source_edge_count" -> number(
              candidate.bagFactorCounters.sourceEdgeCount
            ),
            "tree_edge_count" -> number(
              candidate.bagFactorCounters.treeEdgeCount
            )
          ),
          "candidate_id" -> string(candidate.candidateId),
          "rank" -> number(BigInt(candidate.structuralRank)),
          "rendering_counters" -> obj(
            "cte_count" -> number(candidate.renderingCounters.cteCount),
            "expanded_scan_count" -> number(
              candidate.renderingCounters.expandedScanCount
            ),
            "rendered_term_count" -> number(
              candidate.renderingCounters.renderedTermCount
            ),
            "sql_character_count" -> number(
              candidate.renderingCounters.sqlCharacterCount
            )
          ),
          "stage_diagnostic_counters" -> obj(
            "maximum_statement_character_count" -> number(
              candidate.stageDiagnosticCounters
                .maximumStatementCharacterCount
            ),
            "statement_count" -> number(
              candidate.stageDiagnosticCounters.statementCount
            ),
            "total_statement_character_count" -> number(
              candidate.stageDiagnosticCounters.totalStatementCharacterCount
            )
          )
        )
      }

      document(
        obj(
          "candidate_search" -> obj(
            "state" -> string("found"),
            "statistics" -> obj(
              "basis_alternative_count" -> number(
                search.basisAlternativeCount
              ),
              "basis_check_count" -> number(search.basisCheckCount),
              "block_count" -> number(search.blockCount),
              "candidate_bag_count" -> number(search.candidateBagCount),
              "combination_state_count" -> number(
                search.combinationStateCount
              ),
              "maximum_witness_node_count" -> number(
                search.maximumWitnessNodeCount
              ),
              "retained_partial_plan_count" -> number(
                search.retainedPartialPlanCount
              ),
              "satisfied_block_count" -> number(search.satisfiedBlockCount)
            )
          ),
          "compiled_candidates" -> array(compiled),
          "completed_phase" -> string(view.completedPhase),
          "producer_search_contract_sha256" -> string(
            producerSearchContractSha256
          ),
          "protocol_version" -> string(ProtocolVersion),
          "schema_version" -> string(ProducerSearchTranscriptSchema),
          "semantic_search_core_sha256" -> string(
            semanticSearchCoreSha256
          ),
          "soft_generation" -> obj(
            "counters" -> obj(
              "admissible_candidate_bag_count" -> number(
                soft.admissibleCandidateBagCount
              ),
              "admissible_candidate_cover_alternative_count" -> number(
                soft.admissibleCandidateCoverAlternativeCount
              ),
              "candidate_cover_check_count" -> number(
                soft.candidateCoverCheckCount
              ),
              "component_count" -> number(soft.componentCount),
              "cover_count" -> number(soft.coverCount),
              "cover_pair_count" -> number(soft.coverPairCount),
              "derivation_count" -> number(soft.derivationCount),
              "generated_candidate_bag_count" -> number(
                soft.generatedCandidateBagCount
              ),
              "generated_candidate_cover_alternative_count" -> number(
                soft.generatedCandidateCoverAlternativeCount
              )
            ),
            "state" -> string("completed")
          )
        )
      )
    }

    private def featureRegistryDocument(
        exported: PostgreSqlEnumerationStructuralFeatureExport
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      require(exported != null, "structural feature export must not be null")
      require(
        exported.registryVersion ==
          PostgreSqlEnumerationStructuralFeatures.RegistryVersion &&
          exported.registry == PostgreSqlEnumerationStructuralFeatures.Registry,
        "structural feature export must retain the exact frozen registry"
      )
      structuralFeatureRegistryDocument()
    }

    private[cli] def structuralFeatureRegistryDocument()
        : PostgreSqlEnumerationCampaignV2JsonDocument = {
      val features = PostgreSqlEnumerationStructuralFeatures.Registry.map { spec =>
        obj(
          "absence_rule" -> string(spec.absenceRule.id),
          "definition" -> string(spec.definition),
          "name" -> string(spec.name),
          "scope" -> string(spec.scope.id),
          "source" -> string(spec.source.id),
          "unit" -> string(spec.unit.id),
          "value_type" -> string(spec.valueType.id)
        )
      }
      document(
        obj(
          "schema_version" -> string(FeatureRegistrySchema),
          "registry_version" -> string(
            PostgreSqlEnumerationStructuralFeatures.RegistryVersion
          ),
          "features" -> array(features)
        )
      )
    }

    private def featureValueBundleDocuments(
        exported: PostgreSqlEnumerationStructuralFeatureExport,
        registrySha256: String
    ): Vector[PostgreSqlEnumerationCampaignV2FeatureBundleDocument] = {
      validateSha256(registrySha256, "feature registry SHA-256")
      exported.actions.map { row =>
        val actionId = featureActionId(row.action)
        val values = row.values.map(featureValue)
        val result = document(
          obj(
            "schema_version" -> string(FeatureValueBundleSchema),
            "action_id" -> string(actionId),
            "action_kind" -> string(row.action.kind),
            "feature_registry_sha256" -> string(registrySha256),
            "values" -> array(values)
          )
        )
        PostgreSqlEnumerationCampaignV2FeatureBundleDocument(
          actionId,
          row.action.kind,
          result
        )
      }
    }

    private def featureValue(
        feature: PostgreSqlEnumerationFeatureValue
    ): Value = {
      require(feature != null, "feature value must not be null")
      val (reason, value) = feature.state match {
        case PostgreSqlEnumerationFeatureState.Observed =>
          val observed = feature.value.getOrElse(
            throw new IllegalArgumentException(
              "observed feature must retain its exact scalar"
            )
          )
          val scalar = observed match {
            case PostgreSqlEnumerationObservedFeatureValue.BooleanValue(flag) =>
              bool(flag)
            case PostgreSqlEnumerationObservedFeatureValue.IntegerValue(count) =>
              number(count)
          }
          (NullValue: Value, scalar)
        case PostgreSqlEnumerationFeatureState.NotApplicable =>
          (string("not-applicable-by-registry"), NullValue)
        case PostgreSqlEnumerationFeatureState.MissingOptional |
            PostgreSqlEnumerationFeatureState.CaptureFailed =>
          throw new IllegalArgumentException(
            "structural feature export cannot invent an absent capture reason"
          )
      }
      obj(
        "feature_name" -> string(feature.spec.name),
        "reason_code" -> reason,
        "state" -> string(feature.state.id),
        "value" -> value
      )
    }

    private def orderedStagesDocument(
        candidate: CandidateView
    ): PostgreSqlEnumerationCampaignV2JsonDocument = {
      require(candidate != null, "candidate stage view must not be null")
      require(
        candidate.orderedStages.nonEmpty &&
          candidate.orderedStages.map(_.stageId).distinct.size ==
            candidate.orderedStages.size &&
          candidate.orderedStages.last.stageId ==
            PostgreSqlStageId.FinalReconstruction.code &&
          candidate.orderedStages.last.sql == candidate.renderedSql,
        "ordered stages must end in the exact rendered SQL bytes"
      )
      val stages = candidate.orderedStages.zipWithIndex.map {
        case (stage, index) => orderedStageValue(stage, index + 1)
      }
      document(array(stages))
    }

    private def orderedStageValue(stage: StageView, ordinal: Int): Value = {
      require(stage != null, "renderer stage view must not be null")
      val sql = exactSql(stage.sql, "renderer stage SQL")
      require(
        sql.sha256 == stage.statementSha256,
        "renderer stage digest must bind its exact UTF-8 SQL bytes"
      )
      obj(
        "ordinal" -> number(BigInt(ordinal)),
        "stage_id" -> string(stage.stageId),
        "statement" -> actionSqlValue(sql)
      )
    }

    private def actionSqlValue(sql: ExactUtf8Value): Value = {
      require(
        sql != null && sql.byteLength > 0L,
        "action SQL bytes must be nonempty"
      )
      obj(
        "schema_version" -> string(ActionSqlSchema),
        "encoding" -> string("utf-8"),
        "hash_scheme" -> string(StatementHashScheme),
        "byte_length" -> number(BigInt(sql.byteLength)),
        "utf8_hex" -> utf8HexString(sql.value, sql.byteLength),
        "sha256" -> string(sql.sha256)
      )
    }

    private def requireExportMatchesView(
        exported: PostgreSqlEnumerationStructuralFeatureExport,
        view: ArtifactView
    ): Unit = {
      require(
        exported.registryVersion ==
          PostgreSqlEnumerationStructuralFeatures.RegistryVersion &&
          exported.registry == PostgreSqlEnumerationStructuralFeatures.Registry,
        "campaign projection requires the exact Scala structural registry"
      )
      val featureActions = exported.actions.map(row =>
        (featureActionId(row.action), row.action.kind)
      )
      val viewActions =
        Vector(("native", "native")) ++ view.candidates.map(candidate =>
          (rewriteActionId(candidate.structuralRank), "retained_candidate")
        )
      require(
        featureActions == viewActions,
        "feature export and compiled action view must align exactly"
      )
    }

    private def featureActionId(
        action: PostgreSqlEnumerationFeatureAction
    ): String = action match {
      case PostgreSqlEnumerationFeatureAction.Native => "native"
      case candidate: PostgreSqlEnumerationFeatureAction.RetainedCandidate =>
        rewriteActionId(candidate.structuralRank)
    }

    private def rewriteActionId(rank: Int): String = {
      require(rank > 0 && rank <= 10, "campaign action rank must be in 1..10")
      String.format(Locale.ROOT, "rewrite-%04d", Int.box(rank))
    }

    private def candidateIdentity(candidateId: String): String = {
      require(
        candidateId != null && candidateId.startsWith("sha256:") &&
          candidateId.length == 71,
        "candidate identity must use the canonical sha256: form"
      )
      val digest = candidateId.substring("sha256:".length)
      validateSha256(digest, "candidate identity")
      digest
    }

    private def exactSql(sql: String, where: String): ExactUtf8Value = {
      require(sql != null && sql.nonEmpty, s"$where must be nonempty")
      val expectedByteLength = PostgreSqlEnumerationSqlTransport
        .validate(sql, MaximumSqlBytes.toLong, allowEmpty = false)
        .fold(
          failure =>
            throw new IllegalArgumentException(
              s"$where violates the campaign-v2 SQL transport envelope: ${failure.code}"
            ),
          identity
        )
      require(
        expectedByteLength > 0L,
        s"$where must have a positive validated exact byte count"
      )
      ExactUtf8Value(sql, expectedByteLength, sha256Utf8(sql))
    }

    private def sha256Utf8(value: String): String = {
      require(value != null, "UTF-8 SHA-256 input must not be null")
      val digest = MessageDigest.getInstance("SHA-256")
      var index = 0
      while (index < value.length) {
        val character = value.charAt(index)
        val codePoint =
          if (Character.isHighSurrogate(character)) {
            val result = Character.toCodePoint(
              character,
              value.charAt(index + 1)
            )
            index += 1
            result
          } else character.toInt
        updateDigestWithUtf8CodePoint(digest, codePoint)
        index += 1
      }
      lowerHex(digest.digest())
    }

    private def updateDigestWithUtf8CodePoint(
        digest: MessageDigest,
        codePoint: Int
    ): Unit = {
      def update(value: Int): Unit = digest.update((value & 0xff).toByte)
      if (codePoint <= 0x7f) {
        update(codePoint)
      } else if (codePoint <= 0x7ff) {
        update(0xc0 | (codePoint >>> 6))
        update(0x80 | (codePoint & 0x3f))
      } else if (codePoint <= 0xffff) {
        update(0xe0 | (codePoint >>> 12))
        update(0x80 | ((codePoint >>> 6) & 0x3f))
        update(0x80 | (codePoint & 0x3f))
      } else {
        update(0xf0 | (codePoint >>> 18))
        update(0x80 | ((codePoint >>> 12) & 0x3f))
        update(0x80 | ((codePoint >>> 6) & 0x3f))
        update(0x80 | (codePoint & 0x3f))
      }
    }

    private def validateSha256(value: String, where: String): Unit =
      require(
        value != null && Sha256Pattern.pattern.matcher(value).matches(),
        s"$where must be exactly 64 lowercase hexadecimal characters"
      )

    private[cli] def sha256(bytes: Array[Byte]): String = {
      require(bytes != null, "SHA-256 input must not be null")
      lowerHex(MessageDigest.getInstance("SHA-256").digest(bytes))
    }

    private[cli] def sha256Digest(bytes: Array[Byte]): String = {
      require(bytes != null && bytes.length == 32, "SHA-256 digest has wrong size")
      lowerHex(bytes)
    }

    private def lowerHex(bytes: Array[Byte]): String = {
      require(bytes != null, "hex input must not be null")
      val alphabet = "0123456789abcdef"
      val output = new StringBuilder(bytes.length * 2)
      bytes.foreach { byte =>
        val unsigned = byte & 0xff
        output.append(alphabet.charAt(unsigned >>> 4))
        output.append(alphabet.charAt(unsigned & 0x0f))
      }
      output.result()
    }
  }
}
