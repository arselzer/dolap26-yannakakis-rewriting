package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.io.Reader
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.sql.{Connection, DriverManager, ResultSet, SQLException, SQLTimeoutException, Statement}
import javax.sql.DataSource

import com.fasterxml.jackson.core.{JsonParser => JacksonJsonParser, JsonToken}
import com.fasterxml.jackson.databind.ObjectMapper

import scala.util.control.NonFatal

/**
  * Injectable batch boundary for PostgreSQL optimizer estimates.
  *
  * Implementations must preserve request order and statement digests. The
  * supplied data-source implementation obtains one connection, runs the whole
  * batch in one explicit repeatable-read/read-only transaction, rolls it back,
  * restores auto-commit, and closes the acquired connection. It never closes
  * or mutates the caller-owned data source.
  */
trait PostgreSqlExplainCostEvaluator {
  def evaluate(
      request: PostgreSqlExplainCostBatchRequest,
      limits: PostgreSqlExplainSelectionLimits
  ): Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch]

  /**
    * Optional factor-aware boundary. Existing injected v1 evaluators remain
    * source-compatible and fail closed unless they implement this method.
    */
  def evaluateFactorAware(
      request: PostgreSqlFactorAwareExplainBatchRequest,
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Either[
    PostgreSqlFactorAwareExplainFailure,
    PostgreSqlFactorAwareExplainBatch
  ] =
    Left(
      PostgreSqlFactorAwareExplainFailure(
        PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
        PostgreSqlExplainEvaluationStage.Session,
        None,
        None,
        Vector.empty
      )
    )

  /**
    * Optional physical-portfolio boundary. Existing injected evaluators stay
    * source-compatible and fail closed until they implement the v6 contract.
    */
  def evaluatePhysicalPortfolio(
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits
  ): Either[
    PostgreSqlPhysicalPortfolioExplainFailure,
    PostgreSqlPhysicalPortfolioExplainBatch
  ] = {
    val requestedStatements =
      Option(request).map(_.flattened.size).filter(_ > 0).getOrElse(1)
    Left(
      PostgreSqlPhysicalPortfolioExplainFailure(
        PostgreSqlPhysicalPortfolioExplainFailureCode.EvaluatorContractViolation,
        PostgreSqlPhysicalPortfolioExplainStage.Session,
        None,
        None,
        Vector.empty,
        requestedStatements,
        None,
        None
      )
    )
  }
}

sealed trait PostgreSqlExplainCostEvaluatorConfigurationError
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object PostgreSqlExplainCostEvaluatorConfigurationError {
  case object MissingDataSource
      extends PostgreSqlExplainCostEvaluatorConfigurationError {
    override val code: String = "explain-evaluator.configuration.data-source"
    override val message: String =
      "A PostgreSQL EXPLAIN data source must be provided"
  }
  case object MissingPhysicalSchema
      extends PostgreSqlExplainCostEvaluatorConfigurationError {
    override val code: String = "explain-evaluator.configuration.schema"
    override val message: String =
      "A physical PostgreSQL schema must be provided"
  }
}

object PostgreSqlExplainCostEvaluator {
  val ExplainPrefix: String =
    "EXPLAIN (FORMAT JSON, ANALYZE FALSE, SETTINGS TRUE) "
  val TransactionStatement: String =
    "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY"
  val SearchPathStatement: String =
    "SELECT pg_catalog.set_config(" +
      "'search_path', 'pg_catalog, ' || pg_catalog.quote_ident(?) || ', pg_temp', true)"

  /** Creates an evaluator over a caller-owned data source. */
  def forDataSource(
      dataSource: DataSource,
      physicalSchemaName: String
  ): Either[
    PostgreSqlExplainCostEvaluatorConfigurationError,
    PostgreSqlExplainCostEvaluator
  ] =
    if (dataSource eq null)
      Left(PostgreSqlExplainCostEvaluatorConfigurationError.MissingDataSource)
    else if (
      physicalSchemaName == null || physicalSchemaName.trim.isEmpty ||
      physicalSchemaName.indexOf('\u0000') >= 0
    ) Left(PostgreSqlExplainCostEvaluatorConfigurationError.MissingPhysicalSchema)
    else
      Right(
        new ManagedConnectionEvaluator(
          () => dataSource.getConnection,
          physicalSchemaName
        )
      )

  /** URL convenience used by the PostgreSQL rewriter factory.
    *
    * Credentials are captured only by the connection supplier and never enter
    * an evaluator result, error code, diagnostic, or provenance value.
    */
  private[postgresql] def forPostgreSqlJdbc(
      jdbcUrl: String,
      user: String,
      password: String,
      physicalSchemaName: String
  ): PostgreSqlExplainCostEvaluator =
    new ManagedConnectionEvaluator(
      () => DriverManager.getConnection(jdbcUrl, user, password),
      physicalSchemaName
    )

  private final class ManagedConnectionEvaluator(
      openConnection: () => Connection,
      physicalSchemaName: String
  ) extends PostgreSqlExplainCostEvaluator {
    override def evaluate(
        request: PostgreSqlExplainCostBatchRequest,
        limits: PostgreSqlExplainSelectionLimits
    ): Either[
      PostgreSqlExplainCostEvaluationFailure,
      PostgreSqlExplainCostBatch
    ] = {
      if (request == null || limits == null) {
        return Left(
          evaluationFailure(
            PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
      }

      val connection =
        try openConnection()
        catch {
          case NonFatal(_) =>
            return Left(
              evaluationFailure(
                PostgreSqlExplainCostFailureCode.ConnectionUnavailable,
                PostgreSqlExplainEvaluationStage.Session
              )
            )
        }
      if (connection eq null) {
        return Left(
          evaluationFailure(
            PostgreSqlExplainCostFailureCode.ConnectionUnavailable,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
      }

      evaluateAndClose(
        connection,
        physicalSchemaName,
        request,
        limits
      )
    }

    override def evaluateFactorAware(
        request: PostgreSqlFactorAwareExplainBatchRequest,
        limits: PostgreSqlFactorAwareSelectionLimits
    ): Either[
      PostgreSqlFactorAwareExplainFailure,
      PostgreSqlFactorAwareExplainBatch
    ] = {
      if (request == null || limits == null) {
        return Left(
          factorFailure(
            PostgreSqlExplainCostFailureCode.EvaluatorContractViolation,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
      }

      validateFactorRequestEnvelope(request, limits) match {
        case Some(code) =>
          return Left(factorFailure(code, PostgreSqlExplainEvaluationStage.Session))
        case None => ()
      }

      // The monotonic statement-scheduling budget begins before acquisition.
      // JDBC acquisition, transaction/session configuration, rollback, and
      // close have no portable cancellation boundary and may overrun it;
      // after they return, the next JDBC statement is refused or capped by
      // the remaining budget.
      val deadline = new FactorExplainStatementBudget(
        System.nanoTime(),
        limits.explainStatementBudgetSeconds.toLong * 1000000000L
      )

      val connection =
        try openConnection()
        catch {
          case NonFatal(_) =>
            return Left(
              factorFailure(
                PostgreSqlExplainCostFailureCode.ConnectionUnavailable,
                PostgreSqlExplainEvaluationStage.Session
              )
            )
        }
      if (connection eq null) {
        return Left(
          factorFailure(
            PostgreSqlExplainCostFailureCode.ConnectionUnavailable,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
      }

      evaluateFactorAwareAndClose(
        connection,
        physicalSchemaName,
        request,
        limits,
        deadline
      )
    }

    override def evaluatePhysicalPortfolio(
        request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
        limits: PostgreSqlPhysicalPortfolioEvaluationLimits
    ): Either[
      PostgreSqlPhysicalPortfolioExplainFailure,
      PostgreSqlPhysicalPortfolioExplainBatch
    ] = {
      val requestedStatements =
        Option(request).map(_.flattened.size).filter(_ > 0).getOrElse(1)
      if (
        request == null || limits == null ||
        PostgreSqlPhysicalPortfolioEvaluationLimits.validate(limits).nonEmpty
      ) {
        return Left(
          portfolioFailure(
            PostgreSqlPhysicalPortfolioExplainFailureCode.EvaluatorContractViolation,
            PostgreSqlPhysicalPortfolioExplainStage.Session,
            requestedStatements
          )
        )
      }
      PostgreSqlPhysicalPortfolioEvaluationEnvelope.validate(request, limits) match {
        case Left(_) =>
          return Left(
            portfolioFailure(
              PostgreSqlPhysicalPortfolioExplainFailureCode.SessionResourceLimitExceeded,
              PostgreSqlPhysicalPortfolioExplainStage.Session,
              requestedStatements
            )
          )
        case Right(_) => ()
      }

      val deadline = new FactorExplainStatementBudget(
        System.nanoTime(),
        limits.explainStatementBudgetSeconds.toLong * 1000000000L
      )
      val connection =
        try openConnection()
        catch {
          case NonFatal(_) =>
            return Left(
              portfolioFailure(
                PostgreSqlPhysicalPortfolioExplainFailureCode.ConnectionUnavailable,
                PostgreSqlPhysicalPortfolioExplainStage.Session,
                requestedStatements
              )
            )
        }
      if (connection eq null) {
        return Left(
          portfolioFailure(
            PostgreSqlPhysicalPortfolioExplainFailureCode.ConnectionUnavailable,
            PostgreSqlPhysicalPortfolioExplainStage.Session,
            requestedStatements
          )
        )
      }
      evaluatePhysicalPortfolioAndClose(
        connection,
        physicalSchemaName,
        request,
        limits,
        deadline
      )
    }
  }

  private final class FactorExplainStatementBudget(
      startedAtNanos: Long,
      durationNanos: Long
  ) {
    def queryTimeoutSeconds(
        perStatementSeconds: Int
    ): Either[PostgreSqlExplainCostFailureCode, Int] = {
      val elapsed = System.nanoTime() - startedAtNanos
      val remaining = durationNanos - elapsed
      if (remaining <= 0L)
        Left(PostgreSqlExplainCostFailureCode.StatementTimedOut)
      else {
        val wholeRemainingSeconds = remaining / 1000000000L
        if (wholeRemainingSeconds <= 0L)
          Left(PostgreSqlExplainCostFailureCode.StatementTimedOut)
        else
          Right(
            math.min(perStatementSeconds.toLong, wholeRemainingSeconds).toInt
          )
      }
    }
  }

  private def validateFactorRequestEnvelope(
      request: PostgreSqlFactorAwareExplainBatchRequest,
      limits: PostgreSqlFactorAwareSelectionLimits
  ): Option[PostgreSqlExplainCostFailureCode] = {
    val candidates = BigInt(request.candidates.size)
    val statements = BigInt(1) + request.candidates.foldLeft(BigInt(0))(
      _ + _.stages.size
    )
    val totalCharacters = request.candidates.foldLeft(BigInt(0)) {
      case (total, candidate) =>
        total + candidate.stages.foldLeft(BigInt(0))(_ + _.sql.length)
    }
    val invalidCandidate = request.candidates.exists { candidate =>
      val count = BigInt(candidate.stages.size)
      val characters = candidate.stages.foldLeft(BigInt(0))(_ + _.sql.length)
      count > BigInt(limits.maxStageProbesPerCandidate) ||
      characters > BigInt(limits.maxDiagnosticCharactersPerCandidate) ||
      candidate.stages.exists(statement =>
        BigInt(statement.sql.length) > BigInt(limits.maxCharactersPerProbe)
      )
    }
    if (
      candidates > BigInt(limits.maxCandidates) || invalidCandidate ||
      statements > BigInt(limits.maxExplainStatementsPerSession) ||
      totalCharacters > BigInt(limits.maxDiagnosticCharactersPerSession)
    ) Some(PostgreSqlExplainCostFailureCode.SessionResourceLimitExceeded)
    else None
  }

  private def evaluateFactorAwareAndClose(
      connection: Connection,
      physicalSchemaName: String,
      request: PostgreSqlFactorAwareExplainBatchRequest,
      limits: PostgreSqlFactorAwareSelectionLimits,
      deadline: FactorExplainStatementBudget
  ): Either[
    PostgreSqlFactorAwareExplainFailure,
    PostgreSqlFactorAwareExplainBatch
  ] = {
    var autoCommitDisabled = false
    var result: Either[
      PostgreSqlFactorAwareExplainFailure,
      PostgreSqlFactorAwareExplainBatch
    ] = Left(
      factorFailure(
        PostgreSqlExplainCostFailureCode.SessionConfigurationFailed,
        PostgreSqlExplainEvaluationStage.Session
      )
    )
    var cleanupFailed = false

    try {
      if (connection.isClosed || !connection.getAutoCommit) {
        result = Left(
          factorFailure(
            PostgreSqlExplainCostFailureCode.SessionConfigurationFailed,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
      } else {
        connection.setAutoCommit(false)
        autoCommitDisabled = true
        factorLimitsForNextStatement(limits, deadline) match {
          case Left(code) =>
            result = Left(factorFailure(code, PostgreSqlExplainEvaluationStage.Session))
          case Right(transactionLimits) =>
            executeTransactionStatement(connection, transactionLimits) match {
              case Left(code) =>
                result = Left(factorFailure(code, PostgreSqlExplainEvaluationStage.Session))
              case Right(_) =>
                factorLimitsForNextStatement(limits, deadline) match {
                  case Left(code) =>
                    result = Left(factorFailure(code, PostgreSqlExplainEvaluationStage.Session))
                  case Right(searchPathLimits) =>
                    configureSearchPath(
                      connection,
                      physicalSchemaName,
                      searchPathLimits
                    ) match {
                      case Left(code) =>
                        result = Left(
                          factorFailure(code, PostgreSqlExplainEvaluationStage.Session)
                        )
                      case Right(_) =>
                        result = evaluateFactorTransaction(
                          connection,
                          request,
                          limits,
                          deadline
                        )
                    }
                }
            }
        }
      }
    } catch {
      case NonFatal(problem) =>
        result = Left(
          factorFailure(
            sqlFailureCode(
              problem,
              PostgreSqlExplainCostFailureCode.SessionConfigurationFailed
            ),
            PostgreSqlExplainEvaluationStage.Session
          )
        )
    } finally {
      if (autoCommitDisabled) {
        try connection.rollback()
        catch { case NonFatal(_) => cleanupFailed = true }
        try connection.setAutoCommit(true)
        catch { case NonFatal(_) => cleanupFailed = true }
      }
      try connection.close()
      catch { case NonFatal(_) => cleanupFailed = true }
    }

    if (!cleanupFailed) result
    else {
      val (source, completed) = result match {
        case Left(problem) => problem.source -> problem.completed
        case Right(batch)  => Some(batch.source) -> batch.observations
      }
      Left(
        factorFailure(
          PostgreSqlExplainCostFailureCode.CleanupFailed,
          PostgreSqlExplainEvaluationStage.Session,
          source = source,
          completed = completed
        )
      )
    }
  }

  private def evaluateFactorTransaction(
      connection: Connection,
      request: PostgreSqlFactorAwareExplainBatchRequest,
      limits: PostgreSqlFactorAwareSelectionLimits,
      deadline: FactorExplainStatementBudget
  ): Either[
    PostgreSqlFactorAwareExplainFailure,
    PostgreSqlFactorAwareExplainBatch
  ] = {
    if (!PostgreSqlExplainSafeStatement.isReadOnlySelect(request.source.sql)) {
      return Left(
        factorFailure(
          PostgreSqlExplainCostFailureCode.UnsafeRenderedStatement,
          PostgreSqlExplainEvaluationStage.SourceExplain
        )
      )
    }
    val initialProvenance = readFactorProvenance(
      connection,
      limits,
      deadline
    ) match {
      case Left(code) =>
        return Left(
          factorFailure(
            code,
            factorFailureStage(code, PostgreSqlExplainEvaluationStage.Provenance)
          )
        )
      case Right(value) => value
    }

    var cumulativeCharacters = 0L
    val source = factorLimitsForNextStatement(limits, deadline) match {
      case Left(code) =>
        return Left(
          factorFailure(code, PostgreSqlExplainEvaluationStage.SourceExplain)
        )
      case Right(statementLimits) =>
        runExplainEstimate(
          connection,
          request.source.sql,
          initialProvenance,
          statementLimits,
          cumulativeCharacters
        ) match {
          case Left(code) =>
            return Left(
              factorFailure(
                code,
                factorFailureStage(
                  code,
                  PostgreSqlExplainEvaluationStage.SourceExplain
                )
              )
            )
          case Right((estimate, characters)) =>
            if (!PostgreSqlFactorAwareNumericEnvelope.accepts(estimate)) {
              return Left(
                factorFailure(
                  PostgreSqlExplainCostFailureCode.PlanJsonMalformed,
                  PostgreSqlExplainEvaluationStage.SourceExplain
                )
              )
            }
            cumulativeCharacters += characters
            PostgreSqlExplainSourceEvaluation(
              request.source.sourceStatementSha256,
              estimate
            )
        }
    }

    val completed = new scala.collection.mutable.ArrayBuffer[
      PostgreSqlFactorAwareExplainObservation
    ](request.flattened.size)
    var index = 0
    while (index < request.flattened.size) {
      val (candidate, stage) = request.flattened(index)
      if (!PostgreSqlExplainSafeStatement.isReadOnlySelect(stage.sql)) {
        return Left(
          factorFailure(
            PostgreSqlExplainCostFailureCode.UnsafeRenderedStatement,
            PostgreSqlExplainEvaluationStage.StageExplain,
            Some(index),
            Some(source),
            completed.toVector
          )
        )
      }
      factorLimitsForNextStatement(limits, deadline) match {
        case Left(code) =>
          return Left(
            factorFailure(
              code,
              PostgreSqlExplainEvaluationStage.StageExplain,
              Some(index),
              Some(source),
              completed.toVector
            )
          )
        case Right(statementLimits) =>
          runExplainEstimate(
            connection,
            stage.sql,
            initialProvenance,
            statementLimits,
            cumulativeCharacters
          ) match {
            case Left(code) =>
              return Left(
                factorFailure(
                  code,
                  factorFailureStage(
                    code,
                    PostgreSqlExplainEvaluationStage.StageExplain
                  ),
                  if (code == PostgreSqlExplainCostFailureCode.CleanupFailed)
                    None
                  else Some(index),
                  Some(source),
                  completed.toVector
                )
              )
            case Right((estimate, characters)) =>
              if (!PostgreSqlFactorAwareNumericEnvelope.accepts(estimate)) {
                return Left(
                  factorFailure(
                    PostgreSqlExplainCostFailureCode.PlanJsonMalformed,
                    PostgreSqlExplainEvaluationStage.StageExplain,
                    Some(index),
                    Some(source),
                    completed.toVector
                  )
                )
              }
              cumulativeCharacters += characters
              completed += PostgreSqlFactorAwareExplainObservation(
                candidate.physicalVariantId,
                stage.stageId,
                stage.statementSha256,
                estimate
              )
          }
      }
      index += 1
    }

    readFactorProvenance(connection, limits, deadline) match {
      case Left(code) =>
        Left(
          factorFailure(
            code,
            factorFailureStage(code, PostgreSqlExplainEvaluationStage.Provenance),
            source = Some(source),
            completed = completed.toVector
          )
        )
      case Right(finalProvenance) if finalProvenance != initialProvenance =>
        Left(
          factorFailure(
            PostgreSqlExplainCostFailureCode.ProvenanceChanged,
            PostgreSqlExplainEvaluationStage.Provenance,
            source = Some(source),
            completed = completed.toVector
          )
        )
      case Right(_) =>
        Right(
          PostgreSqlFactorAwareExplainBatch(
            initialProvenance,
            source,
            completed.toVector
          )
        )
    }
  }

  private def readFactorProvenance(
      connection: Connection,
      limits: PostgreSqlFactorAwareSelectionLimits,
      deadline: FactorExplainStatementBudget
  ): Either[PostgreSqlExplainCostFailureCode, PostgreSqlExplainCostProvenance] =
    for {
      versionLimits <- factorLimitsForNextStatement(limits, deadline)
      version <- readServerVersion(connection, versionLimits)
      settingsLimits <- factorLimitsForNextStatement(limits, deadline)
      settings <- readPlannerSettings(connection, settingsLimits)
    } yield PostgreSqlExplainCostProvenance(
      serverVersionNum = version,
      plannerSettingsFingerprintSha256 = fingerprint(settings),
      plannerSettingsFingerprintScheme =
        PostgreSqlExplainCostProvenance.PlannerSettingsFingerprintScheme,
      plannerSettingCount = settings.size
    )

  private def factorLimitsForNextStatement(
      limits: PostgreSqlFactorAwareSelectionLimits,
      deadline: FactorExplainStatementBudget
  ): Either[PostgreSqlExplainCostFailureCode, PostgreSqlExplainSelectionLimits] =
    deadline.queryTimeoutSeconds(limits.statementTimeoutSeconds).flatMap {
      timeout =>
        PostgreSqlExplainSelectionLimits
          .create(
            statementTimeoutSeconds = timeout,
            maxPlanJsonCharacters = limits.maxPlanJsonCharacters,
            maxCumulativePlanJsonCharacters =
              limits.maxCumulativePlanJsonCharacters
          )
          .fold(
            _ =>
              Left(
                PostgreSqlExplainCostFailureCode.EvaluatorContractViolation
              ),
            Right(_)
          )
    }

  private def factorFailure(
      code: PostgreSqlExplainCostFailureCode,
      stage: PostgreSqlExplainEvaluationStage,
      failedStatementIndex: Option[Int] = None,
      source: Option[PostgreSqlExplainSourceEvaluation] = None,
      completed: Vector[PostgreSqlFactorAwareExplainObservation] = Vector.empty
  ): PostgreSqlFactorAwareExplainFailure =
    PostgreSqlFactorAwareExplainFailure(
      code,
      stage,
      failedStatementIndex,
      source,
      completed
    )

  private def factorFailureStage(
      code: PostgreSqlExplainCostFailureCode,
      intended: PostgreSqlExplainEvaluationStage
  ): PostgreSqlExplainEvaluationStage =
    if (code == PostgreSqlExplainCostFailureCode.CleanupFailed)
      PostgreSqlExplainEvaluationStage.Session
    else intended

  private def evaluatePhysicalPortfolioAndClose(
      connection: Connection,
      physicalSchemaName: String,
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      deadline: FactorExplainStatementBudget
  ): Either[
    PostgreSqlPhysicalPortfolioExplainFailure,
    PostgreSqlPhysicalPortfolioExplainBatch
  ] = {
    val requestedStatements = request.flattened.size
    var autoCommitDisabled = false
    var result: Either[
      PostgreSqlPhysicalPortfolioExplainFailure,
      PostgreSqlPhysicalPortfolioExplainBatch
    ] = Left(
      portfolioFailure(
        PostgreSqlPhysicalPortfolioExplainFailureCode.SessionConfigurationFailed,
        PostgreSqlPhysicalPortfolioExplainStage.Session,
        requestedStatements
      )
    )
    var cleanupFailed = false

    try {
      if (connection.isClosed || !connection.getAutoCommit) {
        result = Left(
          portfolioFailure(
            PostgreSqlPhysicalPortfolioExplainFailureCode.SessionConfigurationFailed,
            PostgreSqlPhysicalPortfolioExplainStage.Session,
            requestedStatements
          )
        )
      } else {
        connection.setAutoCommit(false)
        autoCommitDisabled = true
        portfolioLimitsForNextStatement(limits, deadline) match {
          case Left(code) =>
            result = Left(
              portfolioFailureFromLegacy(
                code,
                PostgreSqlPhysicalPortfolioExplainStage.Session,
                requestedStatements
              )
            )
          case Right(transactionLimits) =>
            executeTransactionStatement(connection, transactionLimits) match {
              case Left(code) =>
                result = Left(
                  portfolioFailureFromLegacy(
                    code,
                    PostgreSqlPhysicalPortfolioExplainStage.Session,
                    requestedStatements
                  )
                )
              case Right(_) =>
                portfolioLimitsForNextStatement(limits, deadline) match {
                  case Left(code) =>
                    result = Left(
                      portfolioFailureFromLegacy(
                        code,
                        PostgreSqlPhysicalPortfolioExplainStage.Session,
                        requestedStatements
                      )
                    )
                  case Right(searchPathLimits) =>
                    configureSearchPath(
                      connection,
                      physicalSchemaName,
                      searchPathLimits
                    ) match {
                      case Left(code) =>
                        result = Left(
                          portfolioFailureFromLegacy(
                            code,
                            PostgreSqlPhysicalPortfolioExplainStage.Session,
                            requestedStatements
                          )
                        )
                      case Right(_) =>
                        result = evaluatePhysicalPortfolioTransaction(
                          connection,
                          request,
                          limits,
                          deadline
                        )
                    }
                }
            }
        }
      }
    } catch {
      case NonFatal(problem) =>
        result = Left(
          portfolioFailureFromLegacy(
            sqlFailureCode(
              problem,
              PostgreSqlExplainCostFailureCode.SessionConfigurationFailed
            ),
            PostgreSqlPhysicalPortfolioExplainStage.Session,
            requestedStatements
          )
        )
    } finally {
      if (autoCommitDisabled) {
        try connection.rollback()
        catch { case NonFatal(_) => cleanupFailed = true }
        try connection.setAutoCommit(true)
        catch { case NonFatal(_) => cleanupFailed = true }
      }
      try connection.close()
      catch { case NonFatal(_) => cleanupFailed = true }
    }

    if (!cleanupFailed) result
    else {
      val (source, completed) = result match {
        case Left(problem) => problem.source -> problem.completed
        case Right(batch)  => Some(batch.source) -> batch.observations
      }
      Left(
        portfolioFailure(
          PostgreSqlPhysicalPortfolioExplainFailureCode.CleanupFailed,
          PostgreSqlPhysicalPortfolioExplainStage.Session,
          requestedStatements,
          source = source,
          completed = completed
        )
      )
    }
  }

  private def evaluatePhysicalPortfolioTransaction(
      connection: Connection,
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      deadline: FactorExplainStatementBudget
  ): Either[
    PostgreSqlPhysicalPortfolioExplainFailure,
    PostgreSqlPhysicalPortfolioExplainBatch
  ] = {
    val requestedStatements = request.flattened.size
    val initialProvenance = readPhysicalPortfolioProvenance(
      connection,
      request,
      limits,
      deadline,
      PostgreSqlPhysicalPortfolioExplainStage.VersionPreflight,
      None,
      Vector.empty
    ) match {
      case Left(problem) => return Left(problem)
      case Right(value)  => value
    }

    if (!PostgreSqlExplainSafeStatement.isReadOnlySelect(request.source.sql)) {
      return Left(
        portfolioFailure(
          PostgreSqlPhysicalPortfolioExplainFailureCode.UnsafeRenderedStatement,
          PostgreSqlPhysicalPortfolioExplainStage.SourceExplain,
          requestedStatements
        )
      )
    }

    var cumulativeCharacters = 0L
    val source = portfolioLimitsForNextStatement(limits, deadline) match {
      case Left(code) =>
        return Left(
          portfolioFailureFromLegacy(
            code,
            PostgreSqlPhysicalPortfolioExplainStage.SourceExplain,
            requestedStatements
          )
        )
      case Right(statementLimits) =>
        runExplainEstimate(
          connection,
          request.source.sql,
          initialProvenance,
          statementLimits,
          cumulativeCharacters
        ) match {
          case Left(code) =>
            return Left(
              portfolioFailureFromLegacy(
                code,
                PostgreSqlPhysicalPortfolioExplainStage.SourceExplain,
                requestedStatements
              )
            )
          case Right((estimate, characters)) =>
            if (!PostgreSqlFactorAwareNumericEnvelope.accepts(estimate)) {
              return Left(
                portfolioFailure(
                  PostgreSqlPhysicalPortfolioExplainFailureCode.PlanJsonMalformed,
                  PostgreSqlPhysicalPortfolioExplainStage.SourceExplain,
                  requestedStatements
                )
              )
            }
            cumulativeCharacters += characters
            PostgreSqlExplainSourceEvaluation(
              request.source.sourceStatementSha256,
              estimate
            )
        }
    }

    val completed = new scala.collection.mutable.ArrayBuffer[
      PostgreSqlPhysicalPortfolioExplainObservation
    ](requestedStatements)
    var index = 0
    while (index < request.flattened.size) {
      val (workload, stage) = request.flattened(index)
      if (!PostgreSqlExplainSafeStatement.isReadOnlySelect(stage.sql)) {
        return Left(
          portfolioFailure(
            PostgreSqlPhysicalPortfolioExplainFailureCode.UnsafeRenderedStatement,
            PostgreSqlPhysicalPortfolioExplainStage.StageExplain,
            requestedStatements,
            Some(index),
            Some(source),
            completed.toVector
          )
        )
      }
      portfolioLimitsForNextStatement(limits, deadline) match {
        case Left(code) =>
          return Left(
            portfolioFailureFromLegacy(
              code,
              PostgreSqlPhysicalPortfolioExplainStage.StageExplain,
              requestedStatements,
              Some(index),
              Some(source),
              completed.toVector
            )
          )
        case Right(statementLimits) =>
          runExplainEstimate(
            connection,
            stage.sql,
            initialProvenance,
            statementLimits,
            cumulativeCharacters
          ) match {
            case Left(code) =>
              return Left(
                portfolioFailureFromLegacy(
                  code,
                  PostgreSqlPhysicalPortfolioExplainStage.StageExplain,
                  requestedStatements,
                  Some(index),
                  Some(source),
                  completed.toVector
                )
              )
            case Right((estimate, characters)) =>
              if (!PostgreSqlFactorAwareNumericEnvelope.accepts(estimate)) {
                return Left(
                  portfolioFailure(
                    PostgreSqlPhysicalPortfolioExplainFailureCode.PlanJsonMalformed,
                    PostgreSqlPhysicalPortfolioExplainStage.StageExplain,
                    requestedStatements,
                    Some(index),
                    Some(source),
                    completed.toVector
                  )
                )
              }
              cumulativeCharacters += characters
              completed += PostgreSqlPhysicalPortfolioExplainObservation(
                workload.group.descriptor.workloadId,
                workload.group.representative.physicalVariantId,
                stage.stageId,
                stage.statementSha256,
                estimate
              )
          }
      }
      index += 1
    }

    readPhysicalPortfolioProvenance(
      connection,
      request,
      limits,
      deadline,
      PostgreSqlPhysicalPortfolioExplainStage.FinalProvenance,
      Some(source),
      completed.toVector
    ) match {
      case Left(problem) => Left(problem)
      case Right(finalProvenance) if finalProvenance != initialProvenance =>
        Left(
          portfolioFailure(
            PostgreSqlPhysicalPortfolioExplainFailureCode.ProvenanceChanged,
            PostgreSqlPhysicalPortfolioExplainStage.FinalProvenance,
            requestedStatements,
            source = Some(source),
            completed = completed.toVector
          )
        )
      case Right(_) =>
        Right(
          PostgreSqlPhysicalPortfolioExplainBatch(
            initialProvenance,
            source,
            completed.toVector
          )
        )
    }
  }

  private def readPhysicalPortfolioProvenance(
      connection: Connection,
      request: PostgreSqlPhysicalPortfolioExplainBatchRequest,
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      deadline: FactorExplainStatementBudget,
      intendedStage: PostgreSqlPhysicalPortfolioExplainStage,
      source: Option[PostgreSqlExplainSourceEvaluation],
      completed: Vector[PostgreSqlPhysicalPortfolioExplainObservation]
  ): Either[
    PostgreSqlPhysicalPortfolioExplainFailure,
    PostgreSqlExplainCostProvenance
  ] = {
    val requestedStatements = request.flattened.size
    val versionLimits = portfolioLimitsForNextStatement(limits, deadline) match {
      case Left(code) =>
        return Left(
          portfolioFailureFromLegacy(
            code,
            intendedStage,
            requestedStatements,
            source = source,
            completed = completed
          )
        )
      case Right(value) => value
    }
    val version = readServerVersion(connection, versionLimits) match {
      case Left(code) =>
        return Left(
          portfolioFailureFromLegacy(
            code,
            intendedStage,
            requestedStatements,
            source = source,
            completed = completed
          )
        )
      case Right(value) => value
    }
    if (version < request.minimumServerVersionNum) {
      return Left(
        portfolioFailure(
          PostgreSqlPhysicalPortfolioExplainFailureCode.UnsupportedServerVersion,
          intendedStage,
          requestedStatements,
          source = source,
          completed = completed,
          observedServerVersionNum = Some(version),
          requiredServerVersionNum = Some(request.minimumServerVersionNum)
        )
      )
    }
    val settingsLimits = portfolioLimitsForNextStatement(limits, deadline) match {
      case Left(code) =>
        return Left(
          portfolioFailureFromLegacy(
            code,
            intendedStage,
            requestedStatements,
            source = source,
            completed = completed
          )
        )
      case Right(value) => value
    }
    readPlannerSettings(connection, settingsLimits) match {
      case Left(code) =>
        Left(
          portfolioFailureFromLegacy(
            code,
            intendedStage,
            requestedStatements,
            source = source,
            completed = completed
          )
        )
      case Right(settings) =>
        Right(
          PostgreSqlExplainCostProvenance(
            serverVersionNum = version,
            plannerSettingsFingerprintSha256 = fingerprint(settings),
            plannerSettingsFingerprintScheme =
              PostgreSqlExplainCostProvenance.PlannerSettingsFingerprintScheme,
            plannerSettingCount = settings.size
          )
        )
    }
  }

  private def portfolioLimitsForNextStatement(
      limits: PostgreSqlPhysicalPortfolioEvaluationLimits,
      deadline: FactorExplainStatementBudget
  ): Either[PostgreSqlExplainCostFailureCode, PostgreSqlExplainSelectionLimits] =
    deadline.queryTimeoutSeconds(limits.statementTimeoutSeconds).flatMap {
      timeout =>
        PostgreSqlExplainSelectionLimits
          .create(
            statementTimeoutSeconds = timeout,
            maxPlanJsonCharacters = limits.maxPlanJsonCharacters,
            maxCumulativePlanJsonCharacters =
              limits.maxCumulativePlanJsonCharacters
          )
          .fold(
            _ => Left(PostgreSqlExplainCostFailureCode.EvaluatorContractViolation),
            Right(_)
          )
    }

  private def portfolioFailureFromLegacy(
      code: PostgreSqlExplainCostFailureCode,
      intendedStage: PostgreSqlPhysicalPortfolioExplainStage,
      requestedStageStatementCount: Int,
      failedStatementIndex: Option[Int] = None,
      source: Option[PostgreSqlExplainSourceEvaluation] = None,
      completed: Vector[PostgreSqlPhysicalPortfolioExplainObservation] =
        Vector.empty
  ): PostgreSqlPhysicalPortfolioExplainFailure = {
    val mapped = PostgreSqlPhysicalPortfolioExplainFailureCode.fromLegacy(code)
    val stage =
      if (mapped == PostgreSqlPhysicalPortfolioExplainFailureCode.CleanupFailed)
        PostgreSqlPhysicalPortfolioExplainStage.Session
      else intendedStage
    portfolioFailure(
      mapped,
      stage,
      requestedStageStatementCount,
      if (stage == PostgreSqlPhysicalPortfolioExplainStage.StageExplain)
        failedStatementIndex
      else None,
      source,
      completed
    )
  }

  private def portfolioFailure(
      code: PostgreSqlPhysicalPortfolioExplainFailureCode,
      stage: PostgreSqlPhysicalPortfolioExplainStage,
      requestedStageStatementCount: Int,
      failedStatementIndex: Option[Int] = None,
      source: Option[PostgreSqlExplainSourceEvaluation] = None,
      completed: Vector[PostgreSqlPhysicalPortfolioExplainObservation] =
        Vector.empty,
      observedServerVersionNum: Option[Int] = None,
      requiredServerVersionNum: Option[Int] = None
  ): PostgreSqlPhysicalPortfolioExplainFailure =
    PostgreSqlPhysicalPortfolioExplainFailure(
      code,
      stage,
      failedStatementIndex,
      source,
      completed,
      requestedStageStatementCount,
      observedServerVersionNum,
      requiredServerVersionNum
    )

  private def evaluateAndClose(
      connection: Connection,
      physicalSchemaName: String,
      request: PostgreSqlExplainCostBatchRequest,
      limits: PostgreSqlExplainSelectionLimits
  ): Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch] = {
    var autoCommitDisabled = false
    var result: Either[
      PostgreSqlExplainCostEvaluationFailure,
      PostgreSqlExplainCostBatch
    ] = Left(
      evaluationFailure(
        PostgreSqlExplainCostFailureCode.SessionConfigurationFailed,
        PostgreSqlExplainEvaluationStage.Session
      )
    )
    var cleanupFailed = false

    try {
      if (connection.isClosed || !connection.getAutoCommit) {
        result = Left(
          evaluationFailure(
            PostgreSqlExplainCostFailureCode.SessionConfigurationFailed,
            PostgreSqlExplainEvaluationStage.Session
          )
        )
      } else {
        connection.setAutoCommit(false)
        autoCommitDisabled = true
        executeTransactionStatement(connection, limits) match {
          case Left(code) =>
            result = Left(
              evaluationFailure(code, PostgreSqlExplainEvaluationStage.Session)
            )
          case Right(_) =>
            configureSearchPath(
              connection,
              physicalSchemaName,
              limits
            ) match {
              case Left(code) =>
                result = Left(
                  evaluationFailure(
                    code,
                    PostgreSqlExplainEvaluationStage.Session
                  )
                )
              case Right(_) =>
                result = evaluateTransaction(connection, request, limits)
            }
        }
      }
    } catch {
      case NonFatal(problem) =>
        result = Left(
          evaluationFailure(
            sqlFailureCode(
              problem,
              PostgreSqlExplainCostFailureCode.SessionConfigurationFailed
            ),
            PostgreSqlExplainEvaluationStage.Session
          )
        )
    } finally {
      if (autoCommitDisabled) {
        try connection.rollback()
        catch { case NonFatal(_) => cleanupFailed = true }
        try connection.setAutoCommit(true)
        catch { case NonFatal(_) => cleanupFailed = true }
      }
      try connection.close()
      catch { case NonFatal(_) => cleanupFailed = true }
    }

    if (cleanupFailed) cleanupFailure(request, result)
    else result
  }

  private def cleanupFailure(
      request: PostgreSqlExplainCostBatchRequest,
      result: Either[
        PostgreSqlExplainCostEvaluationFailure,
        PostgreSqlExplainCostBatch
      ]
  ): Left[PostgreSqlExplainCostEvaluationFailure, Nothing] = {
    val (source, observed) = result match {
      case Left(problem) => problem.source -> problem.completed
      case Right(batch)  => Some(batch.source) -> batch.observations
    }
    Left(
      evaluationFailure(
        PostgreSqlExplainCostFailureCode.CleanupFailed,
        PostgreSqlExplainEvaluationStage.Session,
        source = source,
        completed = observed
      )
    )
  }

  private def executeTransactionStatement(
      connection: Connection,
      limits: PostgreSqlExplainSelectionLimits
  ): Either[PostgreSqlExplainCostFailureCode, Unit] = {
    var statement: Statement = null
    var closeFailed = false
    val result =
      try {
        statement = connection.createStatement()
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.execute(TransactionStatement)
        Right(())
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlExplainCostFailureCode.SessionConfigurationFailed
            )
          )
      } finally {
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed) Left(PostgreSqlExplainCostFailureCode.CleanupFailed)
    else result
  }

  private def configureSearchPath(
      connection: Connection,
      physicalSchemaName: String,
      limits: PostgreSqlExplainSelectionLimits
  ): Either[PostgreSqlExplainCostFailureCode, Unit] = {
    var statement: java.sql.PreparedStatement = null
    var resultSet: ResultSet = null
    var closeFailed = false
    val result =
      try {
        statement = connection.prepareStatement(SearchPathStatement)
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.setString(1, physicalSchemaName)
        resultSet = statement.executeQuery()
        if (
          resultSet.getMetaData.getColumnCount != 1 || !resultSet.next() ||
          resultSet.getString(1) == null || resultSet.next()
        ) Left(PostgreSqlExplainCostFailureCode.SessionConfigurationFailed)
        else Right(())
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlExplainCostFailureCode.SessionConfigurationFailed
            )
          )
      } finally {
        if (resultSet != null) {
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed) Left(PostgreSqlExplainCostFailureCode.CleanupFailed)
    else result
  }

  private def evaluateTransaction(
      connection: Connection,
      request: PostgreSqlExplainCostBatchRequest,
      limits: PostgreSqlExplainSelectionLimits
  ): Either[PostgreSqlExplainCostEvaluationFailure, PostgreSqlExplainCostBatch] = {
    val statements = request.statements
    if (!PostgreSqlExplainSafeStatement.isReadOnlySelect(request.source.sql)) {
      return Left(
        evaluationFailure(
          PostgreSqlExplainCostFailureCode.UnsafeRenderedStatement,
          PostgreSqlExplainEvaluationStage.SourceExplain
        )
      )
    }
    val initialProvenance = readProvenance(connection, limits) match {
      case Left(code) =>
        return Left(
          evaluationFailure(code, PostgreSqlExplainEvaluationStage.Provenance)
        )
      case Right(value) => value
    }

    val sourceResult = runExplainEstimate(
      connection,
      request.source.sql,
      initialProvenance,
      limits,
      cumulativeCharacters = 0L
    ) match {
      case Left(code) =>
        return Left(
          evaluationFailure(code, PostgreSqlExplainEvaluationStage.SourceExplain)
        )
      case Right((estimate, characters)) =>
        PostgreSqlExplainSourceEvaluation(
          request.source.sourceStatementSha256,
          estimate
        ) -> characters
    }
    val source = sourceResult._1
    var cumulativeCharacters = sourceResult._2
    var completed = Vector.empty[PostgreSqlExplainCostObservation]
    var index = 0
    while (index < statements.size) {
      val candidate = statements(index)
      if (!PostgreSqlExplainSafeStatement.isReadOnlySelect(candidate.sql)) {
        return Left(
          evaluationFailure(
            PostgreSqlExplainCostFailureCode.UnsafeRenderedStatement,
            PostgreSqlExplainEvaluationStage.CandidateExplain,
            failedCandidateIndex = Some(index),
            source = Some(source),
            completed = completed
          )
        )
      }
      runExplain(
        connection,
        candidate,
        initialProvenance,
        limits,
        cumulativeCharacters
      ) match {
        case Left(code) =>
          return Left(
            evaluationFailure(
              code,
              PostgreSqlExplainEvaluationStage.CandidateExplain,
              failedCandidateIndex = Some(index),
              source = Some(source),
              completed = completed
            )
          )
        case Right((observation, characters)) =>
          cumulativeCharacters += characters
          completed = completed :+ observation
      }
      index += 1
    }

    readProvenance(connection, limits) match {
      case Left(code) =>
        Left(
          evaluationFailure(
            code,
            PostgreSqlExplainEvaluationStage.Provenance,
            source = Some(source),
            completed = completed
          )
        )
      case Right(finalProvenance) if finalProvenance != initialProvenance =>
        Left(
          evaluationFailure(
            PostgreSqlExplainCostFailureCode.ProvenanceChanged,
            PostgreSqlExplainEvaluationStage.Provenance,
            source = Some(source),
            completed = completed
          )
        )
      case Right(_) =>
        Right(
          PostgreSqlExplainCostBatch(initialProvenance, source, completed)
        )
    }
  }

  private def readProvenance(
      connection: Connection,
      limits: PostgreSqlExplainSelectionLimits
  ): Either[
    PostgreSqlExplainCostFailureCode,
    PostgreSqlExplainCostProvenance
  ] =
    for {
      version <- readServerVersion(connection, limits)
      settings <- readPlannerSettings(connection, limits)
    } yield PostgreSqlExplainCostProvenance(
      serverVersionNum = version,
      plannerSettingsFingerprintSha256 = fingerprint(settings),
      plannerSettingsFingerprintScheme =
        PostgreSqlExplainCostProvenance.PlannerSettingsFingerprintScheme,
      plannerSettingCount = settings.size
    )

  private def readServerVersion(
      connection: Connection,
      limits: PostgreSqlExplainSelectionLimits
  ): Either[PostgreSqlExplainCostFailureCode, Int] = {
    var statement: Statement = null
    var resultSet: ResultSet = null
    var closeFailed = false
    val result =
      try {
        statement = connection.createStatement()
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.setMaxRows(2)
        resultSet = statement.executeQuery(
          "SELECT current_setting('server_version_num')"
        )
        if (
          resultSet.getMetaData.getColumnCount != 1 || !resultSet.next()
        ) Left(PostgreSqlExplainCostFailureCode.ProvenanceUnavailable)
        else {
          val parsed = Option(resultSet.getString(1)).flatMap(_.toIntOption)
          if (resultSet.next() || parsed.forall(_ <= 0))
            Left(PostgreSqlExplainCostFailureCode.ProvenanceUnavailable)
          else Right(parsed.get)
        }
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlExplainCostFailureCode.ProvenanceUnavailable
            )
          )
      } finally {
        if (resultSet != null) {
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed) Left(PostgreSqlExplainCostFailureCode.CleanupFailed)
    else result
  }

  private def readPlannerSettings(
      connection: Connection,
      limits: PostgreSqlExplainSelectionLimits
  ): Either[PostgreSqlExplainCostFailureCode, Vector[(String, Option[String])]] = {
    val names = PostgreSqlExplainCostProvenance.PlannerSettingNames
    val values = names.indices.map(index => s"(${index + 1},?)").mkString(",")
    val sql =
      s"SELECT requested.name, current_setting(requested.name, true) " +
        s"FROM (VALUES $values) AS requested(ordinal, name) " +
        s"ORDER BY requested.ordinal"
    var statement: java.sql.PreparedStatement = null
    var resultSet: ResultSet = null
    var closeFailed = false
    val result =
      try {
        statement = connection.prepareStatement(sql)
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.setMaxRows(names.size + 1)
        names.zipWithIndex.foreach { case (name, index) =>
          statement.setString(index + 1, name)
        }
        resultSet = statement.executeQuery()
        val observed = Vector.newBuilder[(String, Option[String])]
        while (resultSet.next()) {
          observed += resultSet.getString(1) -> Option(resultSet.getString(2))
        }
        val rows = observed.result()
        if (rows.map(_._1) != names)
          Left(PostgreSqlExplainCostFailureCode.ProvenanceUnavailable)
        else Right(rows)
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlExplainCostFailureCode.ProvenanceUnavailable
            )
          )
      } finally {
        if (resultSet != null) {
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed) Left(PostgreSqlExplainCostFailureCode.CleanupFailed)
    else result
  }

  private def fingerprint(
      settings: Vector[(String, Option[String])]
  ): String = {
    val canonical = new StringBuilder
    settings.foreach { case (name, value) =>
      val nameLength = name.getBytes(StandardCharsets.UTF_8).length
      canonical.append(nameLength).append(':').append(name).append('=')
      value match {
        case None => canonical.append("-1:")
        case Some(text) =>
          val valueLength = text.getBytes(StandardCharsets.UTF_8).length
          canonical.append(valueLength).append(':').append(text)
      }
      canonical.append('\n')
    }
    sha256(canonical.result())
  }

  private def runExplain(
      connection: Connection,
      candidate: PostgreSqlExplainCandidateStatement,
      provenance: PostgreSqlExplainCostProvenance,
      limits: PostgreSqlExplainSelectionLimits,
      cumulativeCharacters: Long
  ): Either[
    PostgreSqlExplainCostFailureCode,
    (PostgreSqlExplainCostObservation, Long)
  ] =
    runExplainEstimate(
      connection,
      candidate.sql,
      provenance,
      limits,
      cumulativeCharacters
    ).map { case (estimate, characters) =>
      PostgreSqlExplainCostObservation(
        candidate.candidateId,
        candidate.structuralRank,
        candidate.physicalVariantId,
        candidate.renderedStatementSha256,
        estimate
      ) -> characters
    }

  private def runExplainEstimate(
      connection: Connection,
      sql: String,
      provenance: PostgreSqlExplainCostProvenance,
      limits: PostgreSqlExplainSelectionLimits,
      cumulativeCharacters: Long
  ): Either[
    PostgreSqlExplainCostFailureCode,
    (PostgreSqlExplainCostEstimate, Long)
  ] = {
    var statement: Statement = null
    var resultSet: ResultSet = null
    var closeFailed = false
    val result =
      try {
        statement = connection.createStatement()
        statement.setQueryTimeout(limits.statementTimeoutSeconds)
        statement.setMaxRows(2)
        resultSet = statement.executeQuery(ExplainPrefix + sql)
        if (
          resultSet.getMetaData.getColumnCount != 1 || !resultSet.next()
        ) Left(PostgreSqlExplainCostFailureCode.PlanJsonMalformed)
        else {
          val remaining =
            limits.maxCumulativePlanJsonCharacters - cumulativeCharacters
          readBounded(
            resultSet.getCharacterStream(1),
            math.min(limits.maxPlanJsonCharacters, remaining)
          ).flatMap { json =>
            if (resultSet.next())
              Left(PostgreSqlExplainCostFailureCode.PlanJsonMalformed)
            else
              parseRootEstimate(json).map { root =>
                val estimate = PostgreSqlExplainCostEstimate(
                  startupCost = root.startupCost,
                  planTotalCost = root.totalCost,
                  planRows = root.planRows,
                  planWidth = root.planWidth,
                  provenance = provenance
                )
                estimate -> json.length.toLong
              }
          }
        }
      } catch {
        case NonFatal(problem) =>
          Left(
            sqlFailureCode(
              problem,
              PostgreSqlExplainCostFailureCode.StatementFailed
            )
          )
      } finally {
        if (resultSet != null) {
          try resultSet.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
        if (statement != null) {
          try statement.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed) Left(PostgreSqlExplainCostFailureCode.CleanupFailed)
    else result
  }

  private def readBounded(
      reader: Reader,
      maximumCharacters: Long
  ): Either[PostgreSqlExplainCostFailureCode, String] = {
    if (reader == null || maximumCharacters <= 0L) {
      return Left(PostgreSqlExplainCostFailureCode.PlanJsonOversized)
    }
    val output = new StringBuilder(math.min(maximumCharacters, 8192L).toInt)
    val buffer = new Array[Char](8192)
    var total = 0L
    var read = reader.read(buffer)
    while (read != -1) {
      if (read == 0) {
        val one = reader.read()
        if (one == -1) read = -1
        else {
          if (total == maximumCharacters)
            return Left(PostgreSqlExplainCostFailureCode.PlanJsonOversized)
          output.append(one.toChar)
          total += 1L
          read = reader.read(buffer)
        }
      } else {
        if (read.toLong > maximumCharacters - total)
          return Left(PostgreSqlExplainCostFailureCode.PlanJsonOversized)
        output.appendAll(buffer, 0, read)
        total += read.toLong
        read = reader.read(buffer)
      }
    }
    Right(output.result())
  }

  private final case class RootEstimate(
      startupCost: BigDecimal,
      totalCost: BigDecimal,
      planRows: BigDecimal,
      planWidth: Long
  )

  private def parseRootEstimate(
      json: String
  ): Either[PostgreSqlExplainCostFailureCode, RootEstimate] = {
    var parser: JacksonJsonParser = null
    var closeFailed = false
    val result =
      try {
        parser = JsonParser.getFactory.createParser(json)
        parseRootEstimateTokens(parser)
      } catch {
        case NonFatal(_) => Left(PostgreSqlExplainCostFailureCode.PlanJsonMalformed)
      } finally {
        if (parser != null) {
          try parser.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed) Left(PostgreSqlExplainCostFailureCode.PlanJsonMalformed)
    else result
  }

  private def parseRootEstimateTokens(
      parser: JacksonJsonParser
  ): Either[PostgreSqlExplainCostFailureCode, RootEstimate] = {
    if (parser.nextToken() != JsonToken.START_ARRAY)
      return malformedPlanJson
    if (parser.nextToken() != JsonToken.START_OBJECT)
      return malformedPlanJson

    var estimate: Option[RootEstimate] = None
    var token = parser.nextToken()
    while (token != JsonToken.END_OBJECT) {
      if (token != JsonToken.FIELD_NAME) return malformedPlanJson
      val fieldName = parser.currentName()
      if (isExecutionEvidenceField(fieldName)) return malformedPlanJson
      val valueToken = parser.nextToken()
      if (valueToken == null) return malformedPlanJson
      if (fieldName == "Plan") {
        if (estimate.nonEmpty || valueToken != JsonToken.START_OBJECT)
          return malformedPlanJson
        parseRootPlanObject(parser) match {
          case Left(problem) => return Left(problem)
          case Right(value)  => estimate = Some(value)
        }
      } else if (skipValueHasExecutionEvidence(parser, valueToken)) {
        return malformedPlanJson
      }
      token = parser.nextToken()
      if (token == null) return malformedPlanJson
    }

    if (parser.nextToken() != JsonToken.END_ARRAY || parser.nextToken() != null)
      malformedPlanJson
    else estimate.toRight(PostgreSqlExplainCostFailureCode.PlanJsonMalformed)
  }

  private def parseRootPlanObject(
      parser: JacksonJsonParser
  ): Either[PostgreSqlExplainCostFailureCode, RootEstimate] = {
    var startupCost: Option[BigDecimal] = None
    var totalCost: Option[BigDecimal] = None
    var planRows: Option[BigDecimal] = None
    var planWidth: Option[BigDecimal] = None
    var token = parser.nextToken()
    while (token != JsonToken.END_OBJECT) {
      if (token != JsonToken.FIELD_NAME) return malformedPlanJson
      val fieldName = parser.currentName()
      if (isExecutionEvidenceField(fieldName)) return malformedPlanJson
      val valueToken = parser.nextToken()
      if (valueToken == null) return malformedPlanJson
      fieldName match {
        case "Startup Cost" =>
          parseExactNumber(parser, valueToken) match {
            case Some(value) if startupCost.isEmpty => startupCost = Some(value)
            case _                                  => return malformedPlanJson
          }
        case "Total Cost" =>
          parseExactNumber(parser, valueToken) match {
            case Some(value) if totalCost.isEmpty => totalCost = Some(value)
            case _                                => return malformedPlanJson
          }
        case "Plan Rows" =>
          parseExactNumber(parser, valueToken) match {
            case Some(value) if planRows.isEmpty => planRows = Some(value)
            case _                               => return malformedPlanJson
          }
        case "Plan Width" =>
          parseExactNumber(parser, valueToken) match {
            case Some(value) if planWidth.isEmpty => planWidth = Some(value)
            case _                                => return malformedPlanJson
          }
        case _ =>
          if (skipValueHasExecutionEvidence(parser, valueToken))
            return malformedPlanJson
      }
      token = parser.nextToken()
      if (token == null) return malformedPlanJson
    }

    for {
      startup <- startupCost.toRight(
        PostgreSqlExplainCostFailureCode.PlanJsonMalformed
      )
      total <- totalCost.toRight(
        PostgreSqlExplainCostFailureCode.PlanJsonMalformed
      )
      rows <- planRows.toRight(
        PostgreSqlExplainCostFailureCode.PlanJsonMalformed
      )
      width <- planWidth.toRight(
        PostgreSqlExplainCostFailureCode.PlanJsonMalformed
      )
      result <- exactRootEstimate(startup, total, rows, width)
    } yield result
  }

  private def parseExactNumber(
      parser: JacksonJsonParser,
      token: JsonToken
  ): Option[BigDecimal] =
    if (
      token != JsonToken.VALUE_NUMBER_INT &&
      token != JsonToken.VALUE_NUMBER_FLOAT
    ) None
    else
      try Some(BigDecimal(parser.getText))
      catch { case NonFatal(_) => None }

  private def exactRootEstimate(
      startupCost: BigDecimal,
      totalCost: BigDecimal,
      planRows: BigDecimal,
      planWidth: BigDecimal
  ): Either[PostgreSqlExplainCostFailureCode, RootEstimate] =
    try {
      if (
        startupCost.signum < 0 || totalCost.signum < 0 || planRows.signum < 0 ||
        planWidth.signum < 0 ||
        planWidth.bigDecimal.stripTrailingZeros().scale() > 0
      ) malformedPlanJson
      else
        Right(
          RootEstimate(
            startupCost,
            totalCost,
            planRows,
            planWidth.bigDecimal.longValueExact()
          )
        )
    } catch {
      case NonFatal(_) => malformedPlanJson
    }

  private val JsonParser: ObjectMapper = new ObjectMapper()
    .enable(JacksonJsonParser.Feature.STRICT_DUPLICATE_DETECTION)

  private def skipValueHasExecutionEvidence(
      parser: JacksonJsonParser,
      firstToken: JsonToken
  ): Boolean = {
    if (firstToken == null) return true
    if (firstToken.isScalarValue) return false
    if (
      firstToken != JsonToken.START_OBJECT &&
      firstToken != JsonToken.START_ARRAY
    ) return true

    var depth = 1
    while (depth > 0) {
      val token = parser.nextToken()
      if (token == null) return true
      if (
        token == JsonToken.FIELD_NAME &&
        isExecutionEvidenceField(parser.currentName())
      ) return true
      if (token == JsonToken.START_OBJECT || token == JsonToken.START_ARRAY)
        depth += 1
      else if (token == JsonToken.END_OBJECT || token == JsonToken.END_ARRAY)
        depth -= 1
    }
    false
  }

  private def isExecutionEvidenceField(name: String): Boolean =
    name == "Execution Time" || (name != null && name.startsWith("Actual "))

  private def malformedPlanJson[A]
      : Left[PostgreSqlExplainCostFailureCode, A] =
    Left(PostgreSqlExplainCostFailureCode.PlanJsonMalformed)

  private def sqlFailureCode(
      problem: Throwable,
      otherwise: PostgreSqlExplainCostFailureCode
  ): PostgreSqlExplainCostFailureCode = problem match {
    case _: SQLTimeoutException =>
      PostgreSqlExplainCostFailureCode.StatementTimedOut
    case sql: SQLException if sql.getSQLState == "57014" =>
      PostgreSqlExplainCostFailureCode.StatementTimedOut
    case _ => otherwise
  }

  private def evaluationFailure(
      code: PostgreSqlExplainCostFailureCode,
      stage: PostgreSqlExplainEvaluationStage,
      failedCandidateIndex: Option[Int] = None,
      source: Option[PostgreSqlExplainSourceEvaluation] = None,
      completed: Vector[PostgreSqlExplainCostObservation] = Vector.empty
  ): PostgreSqlExplainCostEvaluationFailure =
    PostgreSqlExplainCostEvaluationFailure(
      code,
      stage,
      failedCandidateIndex,
      source,
      completed
    )

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

/** Conservative lexical gate for renderer-owned SELECT/CTE statements. */
private[postgresql] object PostgreSqlExplainSafeStatement {
  private val ForbiddenTokens: Set[String] = Set(
    "alter",
    "call",
    "copy",
    "create",
    "delete",
    "do",
    "drop",
    "execute",
    "grant",
    "insert",
    "into",
    "lock",
    "merge",
    "prepare",
    "refresh",
    "reindex",
    "revoke",
    "set",
    "truncate",
    "update",
    "vacuum"
  )

  def isReadOnlySelect(sql: String): Boolean = {
    if (sql == null) return false
    val trimmed = sql.trim
    var index = 0
    var state = 0 // normal, single, escaped single, double, line comment, block, dollar
    var blockDepth = 0
    var dollarDelimiter: String = null
    var terminated = false
    var firstToken: String = null
    val token = new StringBuilder

    def flushToken(): Boolean = {
      if (token.isEmpty) true
      else {
        val normalized = token.result().toLowerCase(java.util.Locale.ROOT)
        token.clear()
        if (terminated || ForbiddenTokens(normalized)) false
        else {
          if (firstToken == null) firstToken = normalized
          true
        }
      }
    }

    while (index < trimmed.length) {
      val character = trimmed.charAt(index)
      if (character == '\u0000') return false
      if (state == 1 || state == 2) {
        if (state == 1 && character == '\\') return false
        if (state == 2 && character == '\\') {
          if (index + 1 >= trimmed.length) return false
          index += 1
        } else if (character == '\'') {
          if (index + 1 < trimmed.length && trimmed.charAt(index + 1) == '\'')
            index += 1
          else state = 0
        }
      } else if (state == 3) {
        if (character == '"') {
          if (index + 1 < trimmed.length && trimmed.charAt(index + 1) == '"')
            index += 1
          else state = 0
        }
      } else if (state == 4) {
        if (character == '\n' || character == '\r') state = 0
      } else if (state == 5) {
        if (
          character == '/' && index + 1 < trimmed.length &&
          trimmed.charAt(index + 1) == '*'
        ) {
          blockDepth += 1
          index += 1
        } else if (
          character == '*' && index + 1 < trimmed.length &&
          trimmed.charAt(index + 1) == '/'
        ) {
          blockDepth -= 1
          index += 1
          if (blockDepth == 0) state = 0
        }
      } else if (state == 6) {
        if (trimmed.startsWith(dollarDelimiter, index)) {
          index += dollarDelimiter.length - 1
          dollarDelimiter = null
          state = 0
        }
      } else if (
        character == '-' && index + 1 < trimmed.length &&
        trimmed.charAt(index + 1) == '-'
      ) {
        if (!flushToken()) return false
        state = 4
        index += 1
      } else if (
        character == '/' && index + 1 < trimmed.length &&
        trimmed.charAt(index + 1) == '*'
      ) {
        if (!flushToken()) return false
        state = 5
        blockDepth = 1
        index += 1
      } else if (terminated) {
        if (!Character.isWhitespace(character)) return false
      } else if (character == '\'') {
        val escaped = token.result().equalsIgnoreCase("e")
        if (escaped) token.clear()
        else if (!flushToken()) return false
        state = if (escaped) 2 else 1
      } else if (character == '"') {
        if (!flushToken()) return false
        state = 3
      } else if (character == '$') {
        dollarDelimiterAt(trimmed, index) match {
          case Some(delimiter) =>
            if (!flushToken()) return false
            dollarDelimiter = delimiter
            state = 6
            index += delimiter.length - 1
          case None =>
            if (!flushToken()) return false
        }
      } else if (character == ';') {
        if (!flushToken() || terminated) return false
        terminated = true
      } else if (Character.isLetter(character) || character == '_') {
        token.append(character)
      } else if (!flushToken()) return false
      index += 1
    }

    val completeState = state == 0 || state == 4
    flushToken() && completeState &&
    (firstToken == "select" || firstToken == "with")
  }

  private def dollarDelimiterAt(
      sql: String,
      start: Int
  ): Option[String] = {
    var end = start + 1
    while (end < sql.length && sql.charAt(end) != '$') end += 1
    if (end >= sql.length) None
    else {
      val tag = sql.substring(start + 1, end)
      val valid =
        tag.isEmpty ||
          ((Character.isLetter(tag.head) || tag.head == '_') &&
            tag.tail.forall(character =>
              Character.isLetterOrDigit(character) || character == '_'
            ))
      if (valid) Some(sql.substring(start, end + 1)) else None
    }
  }
}
