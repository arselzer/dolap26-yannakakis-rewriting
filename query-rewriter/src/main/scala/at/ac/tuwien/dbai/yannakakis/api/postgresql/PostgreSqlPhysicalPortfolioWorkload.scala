package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlStageDiagnosticRendering,
  PostgreSqlStageDiagnosticStatement,
  PostgreSqlStageId,
  PostgreSqlStagePhysicalForm,
  PostgreSqlStageTrace
}

/** Versioned identity of one exact ordered evaluator workload. */
final case class PostgreSqlPhysicalPortfolioWorkloadId private (
    value: String
) extends Serializable {
  require(
    value != null && value.matches("sha256:[0-9a-f]{64}"),
    "physical portfolio workload id must use canonical SHA-256 form"
  )
}

object PostgreSqlPhysicalPortfolioWorkloadId {
  val Scheme: String =
    PostgreSqlExplainFactorAwarePhysicalPortfolioV1.WorkloadIdentityScheme

  def parse(
      value: String
  ): Either[String, PostgreSqlPhysicalPortfolioWorkloadId] =
    if (value != null && value.matches("sha256:[0-9a-f]{64}"))
      Right(new PostgreSqlPhysicalPortfolioWorkloadId(value))
    else Left("Physical portfolio workload id is not canonical")

  private[postgresql] def fromFields(
      fields: Vector[String]
  ): PostgreSqlPhysicalPortfolioWorkloadId = {
    require(fields != null && fields.nonEmpty && fields.forall(_ != null))
    val digest = MessageDigest.getInstance("SHA-256").digest(preimage(fields))
    new PostgreSqlPhysicalPortfolioWorkloadId(
      "sha256:" + digest.map(byte => f"${byte & 0xff}%02x").mkString
    )
  }

  /** Exact UTF-8 `byteLength:value` concatenation used by v1 workload ids. */
  private[postgresql] def preimage(fields: Vector[String]): Array[Byte] = {
    require(fields != null && fields.nonEmpty && fields.forall(_ != null))
    val output = new ByteArrayOutputStream()
    fields.foreach { value =>
      val bytes = value.getBytes(StandardCharsets.UTF_8)
      output.write(bytes.length.toString.getBytes(StandardCharsets.US_ASCII))
      output.write(':')
      output.write(bytes)
    }
    output.toByteArray
  }
}

/** Hash-bound identity of one emitted diagnostic statement. */
final case class PostgreSqlPhysicalPortfolioStageWorkloadIdentity private[postgresql] (
    stageId: PostgreSqlStageId,
    statementSha256: String,
    characterCount: Long,
    ctePrefixCount: Long
) extends Serializable {
  require(stageId != null, "portfolio workload stage id must not be null")
  require(
    statementSha256 != null && statementSha256.matches("[0-9a-f]{64}"),
    "portfolio stage digest must be lowercase SHA-256"
  )
  require(characterCount >= 0L, "portfolio stage character count must be nonnegative")
  require(ctePrefixCount >= 0L, "portfolio stage prefix count must be nonnegative")
}

/**
  * SQL-free descriptor of the complete ordered evaluator workload.
  *
  * Candidate/configuration/physical ids are deliberately absent. Exact SQL
  * strings are compared independently before two descriptors may be grouped;
  * hashes alone never establish workload equality.
  */
final case class PostgreSqlPhysicalPortfolioWorkloadDescriptor private[postgresql] (
    workloadId: PostgreSqlPhysicalPortfolioWorkloadId,
    renderedStatementSha256: String,
    renderedCharacterCount: Long,
    emittedStageCount: Int,
    stages: Vector[PostgreSqlPhysicalPortfolioStageWorkloadIdentity],
    materialization: PostgreSqlCteMaterializationDirective,
    materializationScope: String,
    workloadIdentityScheme: String,
    stageIdScheme: String,
    stageOrderScheme: String,
    statementHashScheme: String,
    characterCountScheme: String
) extends Serializable {
  require(workloadId != null, "portfolio workload id must not be null")
  require(
    renderedStatementSha256 != null &&
      renderedStatementSha256.matches("[0-9a-f]{64}"),
    "rendered statement digest must be lowercase SHA-256"
  )
  require(renderedCharacterCount >= 0L)
  require(emittedStageCount > 0)
  require(stages != null && stages.size == emittedStageCount)
  require(stages.forall(_ != null) && stages.map(_.stageId).distinct.size == stages.size)
  require(stages.last.stageId == PostgreSqlStageId.FinalReconstruction)
  private val namedStageCount = stages.size - 1
  require(
    stages.dropRight(1).map(_.ctePrefixCount) ==
      (1 to namedStageCount).map(_.toLong).toVector &&
      stages.last.ctePrefixCount == namedStageCount.toLong,
    "portfolio workload CTE prefixes must be canonical and terminal-complete"
  )
  require(
    stages.last.statementSha256 == renderedStatementSha256 &&
      stages.last.characterCount == renderedCharacterCount,
    "portfolio terminal workload identity must equal normal rendering"
  )
  require(materialization != null)
  require(
    materializationScope == PostgreSqlCteMaterializationDirective.Scope,
    "portfolio workload materialization scope must be frozen"
  )
  require(
    workloadIdentityScheme == PostgreSqlPhysicalPortfolioWorkloadId.Scheme &&
      stageIdScheme == PostgreSqlStageId.Scheme &&
      stageOrderScheme == PostgreSqlStageTrace.OrderScheme &&
      statementHashScheme == PostgreSqlStageDiagnosticStatement.HashScheme &&
      characterCountScheme ==
        PostgreSqlStageDiagnosticStatement.CharacterCountScheme,
    "portfolio workload descriptor schemes must match the frozen renderer contract"
  )
  require(
    workloadId == PostgreSqlPhysicalPortfolioWorkloadDescriptor.computeId(
      renderedStatementSha256,
      renderedCharacterCount,
      stages,
      materialization
    ),
    "portfolio workload id must match its exact positional preimage"
  )
}

object PostgreSqlPhysicalPortfolioWorkloadDescriptor {
  def from(
      rendering: PostgreSqlStageDiagnosticRendering,
      materialization: PostgreSqlCteMaterializationDirective
  ): PostgreSqlPhysicalPortfolioWorkloadDescriptor = {
    require(rendering != null, "portfolio diagnostic rendering must not be null")
    require(materialization != null, "portfolio materialization must not be null")
    val rendered = rendering.renderedQuery
    require(rendered != null && rendered.sql != null && rendered.sql.nonEmpty)
    require(rendering.statements != null && rendering.statements.nonEmpty)
    require(rendering.statements.forall(_ != null))

    val emitted = rendered.trace.stages.filter(_.physicalForm.emitted)
    require(
      rendering.statements.map(_.stageId) == emitted.map(_.id),
      "portfolio diagnostics must follow exact emitted-stage order"
    )
    require(
      rendering.statements.last.stageId == PostgreSqlStageId.FinalReconstruction &&
        rendering.statements.last.sql == rendered.sql,
      "portfolio terminal diagnostic must equal normal rendering"
    )
    require(
      emitted.filter(_.physicalForm.namedCte).forall(stage =>
        expectedPhysicalForm(materialization).contains(stage.physicalForm)
      ),
      "every emitted named CTE must use the request-wide materialization directive"
    )

    val stages = rendering.statements.map { statement =>
      require(statement.sql != null && statement.sql.nonEmpty)
      require(statement.characterCount == statement.sql.length.toLong)
      require(
        statement.statementSha256 ==
          PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(
            statement.sql
          )
      )
      PostgreSqlPhysicalPortfolioStageWorkloadIdentity(
        statement.stageId,
        statement.statementSha256,
        statement.characterCount,
        statement.ctePrefixCount
      )
    }
    val renderedDigest =
      PostgreSqlCandidateEvaluationEvidence.renderedStatementSha256(rendered.sql)
    PostgreSqlPhysicalPortfolioWorkloadDescriptor(
      computeId(
        renderedDigest,
        rendered.sql.length.toLong,
        stages,
        materialization
      ),
      renderedDigest,
      rendered.sql.length.toLong,
      stages.size,
      stages,
      materialization,
      PostgreSqlCteMaterializationDirective.Scope,
      PostgreSqlPhysicalPortfolioWorkloadId.Scheme,
      PostgreSqlStageId.Scheme,
      PostgreSqlStageTrace.OrderScheme,
      PostgreSqlStageDiagnosticStatement.HashScheme,
      PostgreSqlStageDiagnosticStatement.CharacterCountScheme
    )
  }

  private[postgresql] def computeId(
      renderedStatementSha256: String,
      renderedCharacterCount: Long,
      stages: Vector[PostgreSqlPhysicalPortfolioStageWorkloadIdentity],
      materialization: PostgreSqlCteMaterializationDirective
  ): PostgreSqlPhysicalPortfolioWorkloadId =
    PostgreSqlPhysicalPortfolioWorkloadId.fromFields(
      preimageFields(
        renderedStatementSha256,
        renderedCharacterCount,
        stages,
        materialization
      )
    )

  /** Positional vector frozen independently of the hash implementation. */
  private[postgresql] def preimageFields(
      renderedStatementSha256: String,
      renderedCharacterCount: Long,
      stages: Vector[PostgreSqlPhysicalPortfolioStageWorkloadIdentity],
      materialization: PostgreSqlCteMaterializationDirective
  ): Vector[String] = {
    require(renderedStatementSha256 != null)
    require(renderedCharacterCount >= 0L)
    require(stages != null && stages.nonEmpty && stages.forall(_ != null))
    require(materialization != null)
    Vector(
      PostgreSqlPhysicalPortfolioWorkloadId.Scheme,
      PostgreSqlStageId.Scheme,
      PostgreSqlStageTrace.OrderScheme,
      PostgreSqlStageDiagnosticStatement.HashScheme,
      PostgreSqlStageDiagnosticStatement.CharacterCountScheme,
      renderedStatementSha256,
      renderedCharacterCount.toString,
      stages.size.toString
    ) ++ stages.flatMap(stage =>
      Vector(
        stage.stageId.code,
        stage.statementSha256,
        stage.characterCount.toString,
        stage.ctePrefixCount.toString
      )
    ) ++ Vector(
      materialization.id,
      PostgreSqlCteMaterializationDirective.Scope
    )
  }

  private[postgresql] def expectedPhysicalForm(
      directive: PostgreSqlCteMaterializationDirective
  ): Option[PostgreSqlStagePhysicalForm] = directive match {
    case PostgreSqlCteMaterializationDirective.PlannerControlled =>
      Some(PostgreSqlStagePhysicalForm.PlannerControlledCte)
    case PostgreSqlCteMaterializationDirective.Materialized =>
      Some(PostgreSqlStagePhysicalForm.MaterializedCte)
    case PostgreSqlCteMaterializationDirective.NotMaterialized =>
      Some(PostgreSqlStagePhysicalForm.NotMaterializedCte)
  }
}

/** One raw physical variant's membership in a deduplicated workload group. */
final case class PostgreSqlPhysicalPortfolioWorkloadAlias private[postgresql] (
    enumerationOrdinal: Int,
    structuralRank: Int,
    candidateId: CandidatePlanId,
    physicalVariantId: PostgreSqlPhysicalVariantId
) extends Serializable {
  require(enumerationOrdinal >= 0)
  require(structuralRank > 0)
  require(candidateId != null)
  require(physicalVariantId != null)
}

/** Canonical minimum-id representative and every byte-identical alias. */
final case class PostgreSqlPhysicalPortfolioWorkloadGroupEvidence private[postgresql] (
    descriptor: PostgreSqlPhysicalPortfolioWorkloadDescriptor,
    representative: PostgreSqlPhysicalPortfolioWorkloadAlias,
    aliases: Vector[PostgreSqlPhysicalPortfolioWorkloadAlias],
    earliestEnumerationOrdinal: Int
) extends Serializable {
  require(descriptor != null)
  require(representative != null)
  require(aliases != null && aliases.nonEmpty && aliases.forall(_ != null))
  require(
    aliases.map(_.physicalVariantId.value) ==
      aliases.map(_.physicalVariantId.value).sorted.distinct,
    "portfolio workload aliases must be sorted by distinct physical id"
  )
  require(
    representative == aliases.head,
    "portfolio workload representative must be the minimum physical id alias"
  )
  require(
    earliestEnumerationOrdinal == aliases.map(_.enumerationOrdinal).min,
    "portfolio workload must retain its earliest scheduler ordinal separately"
  )
}

/** Complete, non-truncated workload grouping after every raw render succeeds. */
final case class PostgreSqlPhysicalPortfolioDeduplicationEvidence private[postgresql] (
    groups: Vector[PostgreSqlPhysicalPortfolioWorkloadGroupEvidence],
    rawVariantCount: Int,
    uniqueWorkloadCount: Int,
    deduplicationScheme: String
) extends Serializable {
  require(groups != null && groups.nonEmpty && groups.forall(_ != null))
  require(rawVariantCount > 0)
  require(uniqueWorkloadCount == groups.size)
  require(uniqueWorkloadCount <= rawVariantCount)
  require(
    deduplicationScheme ==
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.DeduplicationScheme
  )
  require(
    groups.map(_.earliestEnumerationOrdinal) ==
      groups.map(_.earliestEnumerationOrdinal).sorted,
    "portfolio workload groups must follow earliest scheduler order"
  )
  require(groups.map(_.descriptor.workloadId).distinct.size == groups.size)
  private val aliases = groups.flatMap(_.aliases)
  require(aliases.size == rawVariantCount)
  require(aliases.map(_.physicalVariantId).distinct.size == rawVariantCount)
  require(
    aliases.map(_.enumerationOrdinal).sorted == (0 until rawVariantCount).toVector,
    "portfolio deduplication must represent every raw scheduler entry"
  )
}
