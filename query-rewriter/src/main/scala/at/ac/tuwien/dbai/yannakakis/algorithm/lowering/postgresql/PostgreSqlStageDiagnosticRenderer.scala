package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  BagFactorPlanningLimits,
  ConfiguredBagFactorPlan
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.QueryIR

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Package-private, compile-once renderer for per-stage EXPLAIN inputs. */
private[yannakakis] object PostgreSqlStageDiagnosticRenderer {
  def render(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      renderingLimits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      diagnosticLimits: PostgreSqlStageDiagnosticLimits,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile =
        PostgreSqlEnumerationRenderingProfile.default
  ): Either[
    Vector[PostgreSqlRenderingError],
    PostgreSqlStageDiagnosticRendering
  ] = {
    if (diagnosticLimits eq null) {
      return Left(
        Vector(
          PostgreSqlRenderingError.MissingValue(
            "PostgreSQL stage diagnostic limits"
          )
        )
      )
    }
    PostgreSqlBagFactorRenderer
      .compileAndRenderWithAggregateProfile(
        query,
        input,
        plan,
        renderingLimits,
        planningValidationLimits,
        reductionMode,
        PostgreSqlAggregateRenderingProfile.LegacyV1,
        enumerationRenderingProfile
      )
      .flatMap { case (compiled, rendered) =>
        materialize(compiled, rendered, diagnosticLimits)
      }
  }

  def renderConfigured(
      query: QueryIR,
      input: CandidateSearchInput,
      configured: ConfiguredBagFactorPlan,
      renderingLimits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective,
      diagnosticLimits: PostgreSqlStageDiagnosticLimits
  ): Either[
    Vector[PostgreSqlRenderingError],
    PostgreSqlStageDiagnosticRendering
  ] = {
    if (diagnosticLimits eq null) {
      return Left(
        Vector(
          PostgreSqlRenderingError.MissingValue(
            "PostgreSQL stage diagnostic limits"
          )
        )
      )
    }
    PostgreSqlBagFactorRenderer
      .compileAndRenderConfigured(
        query,
        input,
        configured,
        renderingLimits,
        planningValidationLimits,
        reductionMode,
        cteMaterialization
      )
      .flatMap { case (compiled, rendered) =>
        materialize(compiled, rendered, diagnosticLimits)
      }
  }

  private def materialize(
      compiled: CompiledPostgreSqlRendering,
      rendered: PostgreSqlRenderedQuery,
      limits: PostgreSqlStageDiagnosticLimits
  ): Either[
    Vector[PostgreSqlRenderingError],
    PostgreSqlStageDiagnosticRendering
  ] = {
    val emittedStages = compiled.trace.stages.filter(_.physicalForm.emitted)
    val statementCount = BigInt(emittedStages.size)
    if (statementCount > BigInt(limits.maxStatements)) {
      return Left(
        Vector(
          PostgreSqlRenderingError.ResourceLimitExceeded(
            PostgreSqlRenderingResource.StageDiagnosticStatements,
            statementCount,
            limits.maxStatements
          )
        )
      )
    }

    val namedStages = emittedStages.dropRight(1)
    val expectedNamedForm = namedCteForm(compiled)
    val targetNames = Vector.newBuilder[String]
    var index = 0
    while (index < namedStages.size) {
      val stage = namedStages(index)
      if (
        stage.physicalForm != expectedNamedForm ||
        stage.physicalCteName.isEmpty
      ) {
        return invariant(
          s"emitted stage $index does not use the request-wide named CTE form"
        )
      }
      targetNames += stage.physicalCteName.get
      index += 1
    }
    emittedStages.lastOption match {
      case Some(stage)
          if stage.id == PostgreSqlStageId.FinalReconstruction &&
            stage.physicalForm == PostgreSqlStagePhysicalForm.TerminalSelect =>
        ()
      case _ => return invariant("emitted stages do not end in reconstruction")
    }

    val names = targetNames.result()
    val namedCharacterCounts =
      PostgreSqlEmitter.stageDiagnosticCharacterCounts(compiled, names)
    if (namedCharacterCounts.size != namedStages.size) {
      return invariant("diagnostic CTE layout differs from the stage trace")
    }
    val allCharacterCounts = namedCharacterCounts :+ BigInt(rendered.sql.length)
    val maximumCharacters = allCharacterCounts.foldLeft(BigInt(0))(_.max(_))
    val totalCharacters = allCharacterCounts.foldLeft(BigInt(0))(_ + _)
    val resourceErrors = PostgreSqlRenderingError.canonical(
      Vector(
        (
          PostgreSqlRenderingResource.StageDiagnosticStatementCharacters,
          maximumCharacters,
          limits.maxCharactersPerStatement
        ),
        (
          PostgreSqlRenderingResource.StageDiagnosticTotalCharacters,
          totalCharacters,
          limits.maxTotalCharacters
        )
      ).collect {
        case (resource, required, limit) if required > BigInt(limit) =>
          PostgreSqlRenderingError.ResourceLimitExceeded(
            resource,
            required,
            limit
          )
      }
    )
    if (resourceErrors.nonEmpty) return Left(resourceErrors)

    val statements = Vector.newBuilder[PostgreSqlStageDiagnosticStatement]
    index = 0
    while (index < namedStages.size) {
      val stage = namedStages(index)
      val characterCount = namedCharacterCounts(index)
      val sql = PostgreSqlEmitter.renderStageDiagnostic(
        compiled,
        index + 1,
        names(index),
        characterCount.toInt
      )
      if (BigInt(sql.length) != characterCount) {
        return invariant(
          s"diagnostic counting and materializing emitters diverged at stage $index"
        )
      }
      statements += PostgreSqlStageDiagnosticStatement(
        stage.id,
        sql,
        sha256(sql),
        characterCount.toLong,
        (index + 1).toLong
      )
      index += 1
    }
    val terminal = emittedStages.last
    statements += PostgreSqlStageDiagnosticStatement(
      terminal.id,
      rendered.sql,
      sha256(rendered.sql),
      rendered.sql.length.toLong,
      namedStages.size.toLong
    )
    val values = statements.result()
    val validationErrors = validate(
      compiled,
      rendered,
      values,
      limits
    )
    if (validationErrors.nonEmpty) Left(validationErrors)
    else Right(PostgreSqlStageDiagnosticRendering(rendered, values))
  }

  /** Replays trace, name, order, SQL, count, and hash bindings. */
  private[postgresql] def validate(
      compiled: CompiledPostgreSqlRendering,
      rendered: PostgreSqlRenderedQuery,
      statements: Vector[PostgreSqlStageDiagnosticStatement],
      limits: PostgreSqlStageDiagnosticLimits
  ): Vector[PostgreSqlRenderingError] = {
    def failure(detail: String): Vector[PostgreSqlRenderingError] =
      Vector(
        PostgreSqlRenderingError.ModelInvariantViolation(
          s"PostgreSQL stage diagnostic validation failed: $detail"
        )
      )

    if (
      compiled == null || rendered == null || statements == null ||
      limits == null
    ) {
      return failure("validation input is missing")
    }
    if (statements.exists(_ == null)) return failure("a statement is missing")
    val emittedStages = compiled.trace.stages.filter(_.physicalForm.emitted)
    val expectedNamedForm = namedCteForm(compiled)
    if (emittedStages.dropRight(1).exists(_.physicalForm != expectedNamedForm)) {
      return failure("a named stage differs from the request-wide CTE form")
    }
    if (statements.map(_.stageId) != emittedStages.map(_.id)) {
      return failure("statement stage identities or order differ from the trace")
    }
    if (statements.map(_.stageId).distinct.size != statements.size) {
      return failure("statement stage identities are not unique")
    }
    val elidedIds = compiled.trace.stages.collect {
      case stage
          if stage.physicalForm == PostgreSqlStagePhysicalForm.Elided =>
        stage.id
    }.toSet
    if (statements.exists(statement => elidedIds(statement.stageId))) {
      return failure("an elided stage has a diagnostic statement")
    }

    val namedStages = emittedStages.dropRight(1)
    var index = 0
    var totalCharacters = BigInt(0)
    var maximumCharacters = BigInt(0)
    while (index < statements.size) {
      val statement = statements(index)
      if (
        statement.sql == null || statement.statementSha256 == null ||
        statement.stageId == null
      ) {
        return failure(s"statement $index has a missing field")
      }
      val actualCharacters = BigInt(statement.sql.length)
      if (BigInt(statement.characterCount) != actualCharacters) {
        return failure(s"statement $index character count is inconsistent")
      }
      if (statement.statementSha256 != sha256(statement.sql)) {
        return failure(s"statement $index SHA-256 is inconsistent")
      }
      totalCharacters += actualCharacters
      maximumCharacters = maximumCharacters.max(actualCharacters)

      if (index < namedStages.size) {
        val stage = namedStages(index)
        val targetName = stage.physicalCteName match {
          case Some(value) => value
          case None => return failure(s"named stage $index has no CTE name")
        }
        val expectedPrefixCount = index + 1
        if (statement.ctePrefixCount != expectedPrefixCount.toLong) {
          return failure(s"statement $index has an incorrect CTE prefix count")
        }
        val expectedCharacters =
          PostgreSqlEmitter.stageDiagnosticCharacterCount(
            compiled,
            expectedPrefixCount,
            targetName
          )
        if (expectedCharacters != actualCharacters) {
          return failure(s"statement $index has an incorrect emitted layout")
        }
        val expectedSql = PostgreSqlEmitter.renderStageDiagnostic(
          compiled,
          expectedPrefixCount,
          targetName,
          expectedCharacters.toInt
        )
        if (statement.sql != expectedSql) {
          return failure(s"statement $index differs from canonical emission")
        }
      } else {
        if (index != statements.size - 1) {
          return failure("terminal reconstruction is not last")
        }
        if (
          statement.stageId != PostgreSqlStageId.FinalReconstruction ||
          statement.sql != rendered.sql ||
          statement.ctePrefixCount != namedStages.size.toLong
        ) {
          return failure("terminal reconstruction does not match normal SQL")
        }
      }
      index += 1
    }

    val resourceErrors = PostgreSqlRenderingError.canonical(
      Vector(
        (
          PostgreSqlRenderingResource.StageDiagnosticStatements,
          BigInt(statements.size),
          limits.maxStatements
        ),
        (
          PostgreSqlRenderingResource.StageDiagnosticStatementCharacters,
          maximumCharacters,
          limits.maxCharactersPerStatement
        ),
        (
          PostgreSqlRenderingResource.StageDiagnosticTotalCharacters,
          totalCharacters,
          limits.maxTotalCharacters
        )
      ).collect {
        case (resource, required, limit) if required > BigInt(limit) =>
          PostgreSqlRenderingError.ResourceLimitExceeded(
            resource,
            required,
            limit
          )
      }
    )
    resourceErrors
  }

  private def invariant[A](
      detail: String
  ): Left[Vector[PostgreSqlRenderingError], A] =
    Left(
      Vector(
        PostgreSqlRenderingError.ModelInvariantViolation(
          s"PostgreSQL stage diagnostic construction failed: $detail"
        )
      )
    )

  private[postgresql] def sha256(value: String): String = {
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
    val alphabet = "0123456789abcdef"
    val result = new java.lang.StringBuilder(bytes.length * 2)
    var index = 0
    while (index < bytes.length) {
      val unsigned = bytes(index) & 0xff
      result.append(alphabet.charAt(unsigned >>> 4))
      result.append(alphabet.charAt(unsigned & 0x0f))
      index += 1
    }
    result.toString
  }

  private def namedCteForm(
      compiled: CompiledPostgreSqlRendering
  ): PostgreSqlStagePhysicalForm = compiled.cteMaterialization match {
    case PostgreSqlCteMaterializationDirective.PlannerControlled =>
      PostgreSqlStagePhysicalForm.PlannerControlledCte
    case PostgreSqlCteMaterializationDirective.Materialized =>
      PostgreSqlStagePhysicalForm.MaterializedCte
    case PostgreSqlCteMaterializationDirective.NotMaterialized =>
      PostgreSqlStagePhysicalForm.NotMaterializedCte
  }
}
