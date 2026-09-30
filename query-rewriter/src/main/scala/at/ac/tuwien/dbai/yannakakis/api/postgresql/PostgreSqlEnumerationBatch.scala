package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidateResourceLimits,
  CandidateSearchInput
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlanningLimits
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlRenderingLimits,
  PostgreSqlStageDiagnosticLimits,
  PostgreSqlStageDiagnosticStatement
}
import at.ac.tuwien.dbai.yannakakis.algorithm.soft.{
  AdmissibleSoftBagGeneration,
  SoftBagGeneration,
  SoftResourceLimits
}
import at.ac.tuwien.dbai.yannakakis.api.{
  RewriteDiagnostic,
  RewriteOutcome
}
import at.ac.tuwien.dbai.yannakakis.core.QueryHypergraph
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  QueryResultShape
}

private[yannakakis] sealed trait PostgreSqlEnumerationSqlTransportFailure
    extends Product
    with Serializable {
  def code: String
}

private[yannakakis] object PostgreSqlEnumerationSqlTransportFailure {
  case object Empty extends PostgreSqlEnumerationSqlTransportFailure {
    override val code: String = "empty"
  }
  case object ContainsNul extends PostgreSqlEnumerationSqlTransportFailure {
    override val code: String = "contains-nul"
  }
  case object MalformedUtf16 extends PostgreSqlEnumerationSqlTransportFailure {
    override val code: String = "malformed-utf16"
  }
  final case class TooLarge(byteLength: Long)
      extends PostgreSqlEnumerationSqlTransportFailure {
    override val code: String = "utf8-byte-limit-exceeded"
  }
}

/** Exact UTF-8 transport check shared by request acceptance and publication. */
private[yannakakis] object PostgreSqlEnumerationSqlTransport {
  def validate(
      value: String,
      maximumBytes: Long,
      allowEmpty: Boolean
  ): Either[PostgreSqlEnumerationSqlTransportFailure, Long] = {
    require(value != null, "SQL transport value must not be null")
    require(maximumBytes > 0L, "SQL transport byte limit must be positive")
    if (!allowEmpty && value.isEmpty)
      Left(PostgreSqlEnumerationSqlTransportFailure.Empty)
    else if (value.indexOf('\u0000') >= 0)
      Left(PostgreSqlEnumerationSqlTransportFailure.ContainsNul)
    else exactUtf8ByteLength(value).flatMap { byteLength =>
      if (byteLength > maximumBytes)
        Left(PostgreSqlEnumerationSqlTransportFailure.TooLarge(byteLength))
      else Right(byteLength)
    }
  }

  /** Counts exact UTF-8 bytes without allocating an attacker-sized byte array. */
  private def exactUtf8ByteLength(
      value: String
  ): Either[PostgreSqlEnumerationSqlTransportFailure, Long] = {
    var index = 0
    var byteLength = 0L
    while (index < value.length) {
      val character = value.charAt(index)
      if (Character.isHighSurrogate(character)) {
        if (
          index + 1 >= value.length ||
          !Character.isLowSurrogate(value.charAt(index + 1))
        ) return Left(PostgreSqlEnumerationSqlTransportFailure.MalformedUtf16)
        byteLength += 4L
        index += 1
      } else if (Character.isLowSurrogate(character)) {
        return Left(PostgreSqlEnumerationSqlTransportFailure.MalformedUtf16)
      } else if (character <= '\u007f') byteLength += 1L
      else if (character <= '\u07ff') byteLength += 2L
      else byteLength += 3L
      index += 1
    }
    Right(byteLength)
  }
}

/** Outcome-neutral request for the primary enumeration action universe. */
private[yannakakis] final case class PostgreSqlEnumerationBatchRequest private (
    originalSql: String,
    widthBound: Int,
    planLimit: Int,
    requireConnectedCover: Boolean
) extends Serializable {
  require(originalSql != null, "enumeration source SQL must not be null")
  require(
    PostgreSqlEnumerationSqlTransport
      .validate(
        originalSql,
        PostgreSqlEnumerationBatchRequest.MaximumSourceSqlBytes,
        allowEmpty = true
      )
      .isRight,
    "enumeration source SQL must satisfy the exact UTF-8 transport envelope"
  )
  require(widthBound > 0, "enumeration width bound must be positive")
  require(
    planLimit > 0 &&
      planLimit <=
        PostgreSqlRetainedCandidateBatchCompiler.MaximumCampaignPlanLimit,
    "enumeration plan limit must be in 1..10"
  )

  val originalSqlSha256: String =
    PostgreSqlEnumerationBatchRequest.sha256(originalSql)
}

private[yannakakis] object PostgreSqlEnumerationBatchRequest {
  val MaximumSourceSqlBytes: Int = 16 * 1024 * 1024

  sealed trait ValidationError extends Product with Serializable {
    def code: String
  }

  object ValidationError {
    case object MissingSql extends ValidationError {
      override val code: String = "enumeration-batch.request.missing-sql"
    }
    final case class InvalidWidth(value: Int) extends ValidationError {
      override val code: String = "enumeration-batch.request.invalid-width"
    }
    final case class InvalidPlanLimit(value: Int) extends ValidationError {
      override val code: String =
        "enumeration-batch.request.invalid-plan-limit"
    }
    case object InvalidSourceSqlEncoding extends ValidationError {
      override val code: String =
        "enumeration-batch.request.invalid-source-sql-encoding"
    }
    final case class SourceSqlTooLarge(byteLength: Long)
        extends ValidationError {
      override val code: String =
        "enumeration-batch.request.source-sql-too-large"
    }
  }

  def create(
      originalSql: String,
      widthBound: Int = 2,
      planLimit: Int = 10,
      requireConnectedCover: Boolean = true
  ): Either[Vector[ValidationError], PostgreSqlEnumerationBatchRequest] = {
    val problems = Vector.newBuilder[ValidationError]
    if (originalSql == null) problems += ValidationError.MissingSql
    else {
      PostgreSqlEnumerationSqlTransport
        .validate(originalSql, MaximumSourceSqlBytes, allowEmpty = true) match {
        case Left(
              PostgreSqlEnumerationSqlTransportFailure.TooLarge(byteLength)
            ) =>
          problems += ValidationError.SourceSqlTooLarge(byteLength)
        case Left(_) =>
          problems += ValidationError.InvalidSourceSqlEncoding
        case Right(_) => ()
      }
    }
    if (widthBound <= 0) problems += ValidationError.InvalidWidth(widthBound)
    if (
      planLimit <= 0 ||
      planLimit >
        PostgreSqlRetainedCandidateBatchCompiler.MaximumCampaignPlanLimit
    ) problems += ValidationError.InvalidPlanLimit(planLimit)
    val errors = problems.result()
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        PostgreSqlEnumerationBatchRequest(
          originalSql,
          widthBound,
          planLimit,
          requireConnectedCover
        )
      )
  }

  private def sha256(value: String): String = {
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
    val alphabet = "0123456789abcdef"
    val result = new StringBuilder(bytes.length * 2)
    bytes.foreach { byte =>
      val unsigned = byte & 0xff
      result.append(alphabet.charAt(unsigned >>> 4))
      result.append(alphabet.charAt(unsigned & 0x0f))
    }
    result.result()
  }
}

private[yannakakis] sealed trait PostgreSqlEnumerationBatchStatus
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlEnumerationBatchStatus {
  case object Produced extends PostgreSqlEnumerationBatchStatus {
    override val id: String = "produced"
  }
  case object NotApplicable extends PostgreSqlEnumerationBatchStatus {
    override val id: String = "not_applicable"
  }
  case object InvalidQuery extends PostgreSqlEnumerationBatchStatus {
    override val id: String = "invalid_query"
  }
  case object Failed extends PostgreSqlEnumerationBatchStatus {
    override val id: String = "failed"
  }
}

/** Exact primitive soft-generation limits used by one producer invocation. */
private[yannakakis] final case class PostgreSqlEnumerationSoftLimitEvidence(
    maxCovers: Long,
    maxCoverPairs: Long,
    maxComponents: Long,
    maxDerivations: Long,
    maxCandidates: Long,
    maxCandidateCoverChecks: Long
) extends Serializable {
  require(
    Vector(
      maxCovers,
      maxCoverPairs,
      maxComponents,
      maxDerivations,
      maxCandidates,
      maxCandidateCoverChecks
    ).forall(_ > 0L),
    "soft-generation limit evidence must be positive"
  )
}

/** Exact primitive CandidateTD limits used by one producer invocation. */
private[yannakakis] final case class PostgreSqlEnumerationCandidateLimitEvidence(
    maxBlocks: Long,
    maxBasisChecks: Long,
    maxBasisAlternatives: Long,
    maxWitnessNodes: Long,
    maxPlanLimit: Long,
    maxRetainedPartialPlans: Long,
    maxCombinationStates: Long
) extends Serializable {
  require(
    Vector(
      maxBlocks,
      maxBasisChecks,
      maxBasisAlternatives,
      maxWitnessNodes,
      maxPlanLimit,
      maxRetainedPartialPlans,
      maxCombinationStates
    ).forall(_ > 0L),
    "CandidateTD limit evidence must be positive"
  )
}

/** Exact primitive bag-factor limits used for every retained rank. */
private[yannakakis] final case class PostgreSqlEnumerationBagFactorLimitEvidence(
    maxBags: Long,
    maxOwnershipChecks: Long,
    maxTreeEdges: Long,
    maxSourceEdges: Long
) extends Serializable {
  require(
    Vector(maxBags, maxOwnershipChecks, maxTreeEdges, maxSourceEdges)
      .forall(_ > 0L),
    "bag-factor limit evidence must be positive"
  )
}

/** Exact primitive SQL-renderer limits used for every retained rank. */
private[yannakakis] final case class PostgreSqlEnumerationRenderingLimitEvidence(
    maxExpandedScans: Long,
    maxRenderedTerms: Long,
    maxCtes: Long,
    maxSqlCharacters: Long,
    characterCountScheme: String
) extends Serializable {
  require(
    Vector(maxExpandedScans, maxRenderedTerms, maxCtes, maxSqlCharacters)
      .forall(_ > 0L),
    "rendering limit evidence must be positive"
  )
  require(
    characterCountScheme ==
      PostgreSqlStageDiagnosticStatement.CharacterCountScheme,
    "rendering character-count scheme must be the exact JVM scheme"
  )
}

/** Exact primitive diagnostic-renderer limits used for every retained rank. */
private[yannakakis] final case class PostgreSqlEnumerationStageDiagnosticLimitEvidence(
    maxStatements: Long,
    maxCharactersPerStatement: Long,
    maxTotalCharacters: Long,
    characterCountScheme: String
) extends Serializable {
  require(
    Vector(maxStatements, maxCharactersPerStatement, maxTotalCharacters)
      .forall(_ > 0L),
    "stage-diagnostic limit evidence must be positive"
  )
  require(
    characterCountScheme ==
      PostgreSqlStageDiagnosticStatement.CharacterCountScheme,
    "stage-diagnostic character-count scheme must be the exact JVM scheme"
  )
}

/** Per-item/document transport ceilings; no cumulative-output claim is made. */
private[yannakakis] final case class PostgreSqlEnumerationTransportLimitEvidence(
    encoding: String,
    maxSourceSqlBytes: Long,
    maxGeneratedOrStageSqlBytes: Long,
    maxCanonicalJsonDocumentBytes: Long
) extends Serializable {
  require(
    encoding == PostgreSqlEnumerationTransportLimitEvidence.Encoding,
    "enumeration transport encoding must be UTF-8"
  )
  require(
    maxSourceSqlBytes ==
      PostgreSqlEnumerationTransportLimitEvidence.MaximumSourceSqlBytes &&
      maxGeneratedOrStageSqlBytes ==
        PostgreSqlEnumerationTransportLimitEvidence.MaximumGeneratedOrStageSqlBytes &&
      maxCanonicalJsonDocumentBytes ==
        PostgreSqlEnumerationTransportLimitEvidence.MaximumCanonicalJsonDocumentBytes,
    "enumeration transport evidence must retain the exact shared ceilings"
  )
}

private[yannakakis] object PostgreSqlEnumerationTransportLimitEvidence {
  val Encoding: String = "utf-8"
  val MaximumSourceSqlBytes: Long =
    PostgreSqlEnumerationBatchRequest.MaximumSourceSqlBytes.toLong
  val MaximumGeneratedOrStageSqlBytes: Long = 16L * 1024L * 1024L
  val MaximumCanonicalJsonDocumentBytes: Long = 64L * 1024L * 1024L

  val exact: PostgreSqlEnumerationTransportLimitEvidence =
    PostgreSqlEnumerationTransportLimitEvidence(
      Encoding,
      MaximumSourceSqlBytes,
      MaximumGeneratedOrStageSqlBytes,
      MaximumCanonicalJsonDocumentBytes
    )
}

/**
  * Immutable snapshot of the complete success-path structural search contract.
  *
  * Exhaustion is intentionally limited to structural-tree witnesses. Physical
  * cover assignments are canonicalized, not separately enumerated.
  */
private[yannakakis] final case class PostgreSqlEnumerationProducerSearchContract private (
    widthBound: Int,
    requireConnectedCover: Boolean,
    soft: PostgreSqlEnumerationSoftLimitEvidence,
    candidate: PostgreSqlEnumerationCandidateLimitEvidence,
    bagFactor: PostgreSqlEnumerationBagFactorLimitEvidence,
    rendering: PostgreSqlEnumerationRenderingLimitEvidence,
    stageDiagnostics: PostgreSqlEnumerationStageDiagnosticLimitEvidence,
    transport: PostgreSqlEnumerationTransportLimitEvidence
) extends Serializable {
  require(widthBound > 0, "producer search width bound must be positive")
  require(soft != null, "producer soft limits must not be null")
  require(candidate != null, "producer candidate limits must not be null")
  require(bagFactor != null, "producer bag-factor limits must not be null")
  require(rendering != null, "producer rendering limits must not be null")
  require(
    stageDiagnostics != null,
    "producer stage-diagnostic limits must not be null"
  )
  require(transport != null, "producer transport limits must not be null")

  val algorithm: String =
    PostgreSqlEnumerationProducerSearchContract.Algorithm
  val structuralUniverse: String =
    PostgreSqlEnumerationProducerSearchContract.StructuralUniverse
  val ranking: String = PostgreSqlEnumerationProducerSearchContract.Ranking
  val physicalCoverPolicy: String =
    PostgreSqlEnumerationProducerSearchContract.PhysicalCoverPolicy
  val exhaustionScope: String =
    PostgreSqlEnumerationProducerSearchContract.ExhaustionScope
  val coverDistinctAssignmentsEnumerated: Boolean = false
}

private[yannakakis] object PostgreSqlEnumerationProducerSearchContract {
  val Algorithm: String = "top-k-candidate-td-v1"
  val StructuralUniverse: String = "structural-tree-witnesses-v1"
  val Ranking: String =
    "node-count-then-canonical-rooted-tree-signature-v1"
  val PhysicalCoverPolicy: String =
    "canonical-first-admissible-cover-per-bag-v1"
  val ExhaustionScope: String = "structural-tree-witnesses-only-v1"

  def capture(
      request: PostgreSqlEnumerationBatchRequest,
      softLimits: SoftResourceLimits,
      candidateLimits: CandidateResourceLimits,
      bagFactorLimits: BagFactorPlanningLimits,
      renderingLimits: PostgreSqlRenderingLimits,
      diagnosticLimits: PostgreSqlStageDiagnosticLimits
  ): PostgreSqlEnumerationProducerSearchContract = {
    require(request != null, "enumeration request must not be null")
    require(softLimits != null, "soft limits must not be null")
    require(candidateLimits != null, "candidate limits must not be null")
    require(bagFactorLimits != null, "bag-factor limits must not be null")
    require(renderingLimits != null, "rendering limits must not be null")
    require(diagnosticLimits != null, "diagnostic limits must not be null")

    PostgreSqlEnumerationProducerSearchContract(
      request.widthBound,
      request.requireConnectedCover,
      PostgreSqlEnumerationSoftLimitEvidence(
        softLimits.maxCovers,
        softLimits.maxCoverPairs,
        softLimits.maxComponents,
        softLimits.maxDerivations,
        softLimits.maxCandidates,
        softLimits.maxCandidateCoverChecks
      ),
      PostgreSqlEnumerationCandidateLimitEvidence(
        candidateLimits.maxBlocks,
        candidateLimits.maxBasisChecks,
        candidateLimits.maxBasisAlternatives,
        candidateLimits.maxWitnessNodes,
        candidateLimits.maxPlanLimit,
        candidateLimits.maxRetainedPartialPlans,
        candidateLimits.maxCombinationStates
      ),
      PostgreSqlEnumerationBagFactorLimitEvidence(
        bagFactorLimits.maxBags,
        bagFactorLimits.maxOwnershipChecks,
        bagFactorLimits.maxTreeEdges,
        bagFactorLimits.maxSourceEdges
      ),
      PostgreSqlEnumerationRenderingLimitEvidence(
        renderingLimits.maxExpandedScans,
        renderingLimits.maxRenderedTerms,
        renderingLimits.maxCtes,
        renderingLimits.maxSqlCharacters,
        PostgreSqlStageDiagnosticStatement.CharacterCountScheme
      ),
      PostgreSqlEnumerationStageDiagnosticLimitEvidence(
        diagnosticLimits.maxStatements,
        diagnosticLimits.maxCharactersPerStatement,
        diagnosticLimits.maxTotalCharacters,
        PostgreSqlStageDiagnosticStatement.CharacterCountScheme
      ),
      PostgreSqlEnumerationTransportLimitEvidence.exact
    )
  }
}

/** Rich in-memory evidence; serialization is a separate audited boundary. */
private[yannakakis] final case class PostgreSqlEnumerationBatchArtifact(
    request: PostgreSqlEnumerationBatchRequest,
    analysis: AnalysisReport,
    hypergraph: QueryHypergraph,
    softGeneration: SoftBagGeneration,
    admissibleGeneration: AdmissibleSoftBagGeneration,
    candidateInput: CandidateSearchInput,
    batch: PostgreSqlRetainedCandidateBatch,
    producerSearchContract: PostgreSqlEnumerationProducerSearchContract
) extends Serializable {
  require(request != null, "enumeration request must not be null")
  require(analysis != null, "enumeration analysis must not be null")
  require(hypergraph != null, "enumeration hypergraph must not be null")
  require(softGeneration != null, "soft generation must not be null")
  require(admissibleGeneration != null, "admissible generation must not be null")
  require(candidateInput != null, "candidate input must not be null")
  require(batch != null, "compiled batch must not be null")
  require(
    producerSearchContract != null,
    "producer search contract must not be null"
  )
  require(
    analysis.diagnostics.isEmpty &&
      analysis.query.resultShape == QueryResultShape.EnumerationProjection,
    "enumeration artifact must retain a supported enumeration analysis"
  )
  require(
    batch.query eq analysis.query,
    "enumeration batch must retain the exact source-proof-bound QueryIR"
  )
  require(
    candidateInput.hypergraph == hypergraph &&
      batch.candidateInput == candidateInput,
    "enumeration batch must retain the exact candidate-search input"
  )
  require(
    batch.requestedLimit == request.planLimit,
    "enumeration search must retain the requested plan limit"
  )
  require(
    softGeneration.sourceHypergraph == hypergraph &&
      admissibleGeneration.sourceHypergraph == hypergraph &&
      softGeneration.widthBound == request.widthBound &&
      admissibleGeneration.widthBound == request.widthBound &&
      candidateInput.widthBound == request.widthBound &&
      admissibleGeneration.requireConnectedCover ==
        request.requireConnectedCover &&
      candidateInput.requireConnectedCover == request.requireConnectedCover,
    "enumeration request, generations, and candidate input must retain exact width/ConCov settings"
  )
  require(
    producerSearchContract.widthBound == request.widthBound &&
      producerSearchContract.requireConnectedCover ==
        request.requireConnectedCover &&
      BigInt(request.planLimit) <=
        BigInt(producerSearchContract.candidate.maxPlanLimit),
    "producer search contract must bind the exact request settings"
  )
  require(
    batch.candidates.forall(candidate =>
      candidate.plan.certificate.requireConnectedCover ==
        request.requireConnectedCover &&
        candidate.bagFactorPlan.certificate == candidate.plan.certificate &&
        candidate.bagFactorPlan.sourceHypergraph == hypergraph &&
        candidate.bagFactorPlan.sourceWidthBound == request.widthBound &&
        candidate.bagFactorPlan.requireConnectedCover ==
          request.requireConnectedCover
    ),
    "every retained physical plan must bind the exact source, width, and ConCov settings"
  )
}

private[yannakakis] final case class PostgreSqlEnumerationBatchResult(
    request: PostgreSqlEnumerationBatchRequest,
    status: PostgreSqlEnumerationBatchStatus,
    completedPhase: PostgreSqlRewritePhase,
    diagnostics: Vector[RewriteDiagnostic],
    pipelineEvidence: PostgreSqlRewriteReport,
    producerSearchContract: PostgreSqlEnumerationProducerSearchContract,
    artifact: Option[PostgreSqlEnumerationBatchArtifact]
) extends Serializable {
  require(request != null, "enumeration request must not be null")
  require(status != null, "enumeration status must not be null")
  require(completedPhase != null, "completed phase must not be null")
  require(
    diagnostics != null && diagnostics.forall(_ != null),
    "enumeration diagnostics must not be null or contain null"
  )
  require(
    pipelineEvidence != null &&
      pipelineEvidence.completedPhase == completedPhase,
    "enumeration pipeline evidence must retain the exact completed phase"
  )
  require(
    producerSearchContract != null,
    "enumeration producer search contract must not be null"
  )
  require(
    artifact != null && artifact.forall(_ != null),
    "enumeration artifact option must not be null or contain null"
  )
  require(
    (status == PostgreSqlEnumerationBatchStatus.Produced) == artifact.nonEmpty,
    "only a produced enumeration result may retain an artifact"
  )
  require(
    status != PostgreSqlEnumerationBatchStatus.Produced ||
      (diagnostics.isEmpty && completedPhase == PostgreSqlRewritePhase.Rendered),
    "produced enumeration result must be fully rendered and diagnostic-free"
  )
  artifact.foreach { value =>
    require(
      (value.producerSearchContract eq producerSearchContract) &&
      pipelineEvidence.analysis.contains(value.analysis) &&
        pipelineEvidence.hypergraph.contains(value.hypergraph) &&
        pipelineEvidence.softGeneration.contains(value.softGeneration) &&
        pipelineEvidence.admissibleGeneration.contains(
          value.admissibleGeneration
        ) &&
        pipelineEvidence.candidateInput.contains(value.candidateInput) &&
        pipelineEvidence.candidateDecision.contains(value.batch.search),
      "produced enumeration evidence must bind the exact completed pipeline"
    )
  }
}

private[postgresql] object PostgreSqlEnumerationBatchResult {
  def fromRewriteFailure(
      request: PostgreSqlEnumerationBatchRequest,
      result: PostgreSqlRewriteResult,
      producerSearchContract: PostgreSqlEnumerationProducerSearchContract
  ): PostgreSqlEnumerationBatchResult = {
    require(request != null, "enumeration request must not be null")
    require(result != null, "rewrite failure must not be null")
    require(
      producerSearchContract != null &&
        producerSearchContract.widthBound == request.widthBound &&
        producerSearchContract.requireConnectedCover ==
          request.requireConnectedCover,
      "rewrite failure must retain the exact producer search contract"
    )
    require(
      !result.outcome.isInstanceOf[RewriteOutcome.Rewritten],
      "a rewritten outcome is not an enumeration-batch failure"
    )
    val status = result.outcome match {
      case _: RewriteOutcome.NotApplicable =>
        PostgreSqlEnumerationBatchStatus.NotApplicable
      case _: RewriteOutcome.InvalidQuery =>
        PostgreSqlEnumerationBatchStatus.InvalidQuery
      case _: RewriteOutcome.Failed => PostgreSqlEnumerationBatchStatus.Failed
      case _: RewriteOutcome.Rewritten =>
        throw new IllegalArgumentException(
          "a rewritten outcome is not an enumeration-batch failure"
        )
    }
    PostgreSqlEnumerationBatchResult(
      request,
      status,
      result.report.completedPhase,
      result.outcome.diagnostics,
      result.report,
      producerSearchContract,
      None
    )
  }
}
