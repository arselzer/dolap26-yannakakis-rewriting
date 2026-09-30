package at.ac.tuwien.dbai.yannakakis.cli

import java.nio.file.{InvalidPathException, Path, Paths}

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlExplainSelectionLimits,
  PostgreSqlFactorAwareSelectionLimits,
  PostgreSqlPhysicalPortfolioEvaluationLimits,
  PostgreSqlPhysicalPortfolioLimits
}

private[cli] object PostgreSqlRewriteExitCode {
  val Rewritten: Int = 0
  val NotApplicable: Int = 2
  val InvalidQuery: Int = 3
  val RewriteFailed: Int = 4
  val InvalidInvocation: Int = 64
  val InputError: Int = 66
  val ServiceUnavailable: Int = 69
  val InternalError: Int = 70

  def forOutcome(outcome: RewriteOutcome): Int = outcome match {
    case _: RewriteOutcome.Rewritten     => Rewritten
    case _: RewriteOutcome.NotApplicable => NotApplicable
    case _: RewriteOutcome.InvalidQuery  => InvalidQuery
    case _: RewriteOutcome.Failed        => RewriteFailed
  }
}

private[cli] sealed trait SqlSource extends Product with Serializable

private[cli] object SqlSource {
  case object StandardInput extends SqlSource
  final case class File(path: Path) extends SqlSource {
    require(path != null, "SQL file path must not be null")
  }
}

private[cli] final case class JdbcEnvironment(
    jdbcUrl: String,
    user: String,
    password: String
) {
  require(jdbcUrl != null && jdbcUrl.trim.nonEmpty, "JDBC URL must not be blank")
  require(user != null && user.trim.nonEmpty, "database user must not be blank")
  require(password != null, "database password must be defined")
}

private[cli] final case class PostgreSqlRewriteCliConfig(
    sqlSource: SqlSource,
    schemaName: String,
    options: RewriteOptions,
    explainLimits: PostgreSqlExplainSelectionLimits,
    factorAwareLimits: PostgreSqlFactorAwareSelectionLimits,
    jdbc: JdbcEnvironment,
    physicalPortfolioLimits: PostgreSqlPhysicalPortfolioLimits =
      PostgreSqlPhysicalPortfolioLimits.default,
    physicalPortfolioEvaluationLimits: PostgreSqlPhysicalPortfolioEvaluationLimits =
      PostgreSqlPhysicalPortfolioEvaluationLimits.default
) {
  require(sqlSource != null, "SQL source must not be null")
  require(
    schemaName != null && schemaName.trim.nonEmpty,
    "database schema must not be blank"
  )
  require(options != null, "rewrite options must not be null")
  require(explainLimits != null, "EXPLAIN selection limits must not be null")
  require(factorAwareLimits != null, "factor-aware selection limits must not be null")
  require(jdbc != null, "JDBC environment must not be null")
  require(physicalPortfolioLimits != null, "physical portfolio limits must not be null")
  require(
    physicalPortfolioEvaluationLimits != null,
    "physical portfolio evaluation limits must not be null"
  )
}

private[cli] sealed trait PostgreSqlRewriteCliCommand
    extends Product
    with Serializable

private[cli] object PostgreSqlRewriteCliCommand {
  case object Help extends PostgreSqlRewriteCliCommand
  final case class Rewrite(config: PostgreSqlRewriteCliConfig)
      extends PostgreSqlRewriteCliCommand {
    require(config != null, "CLI configuration must not be null")
  }
}

private[cli] final case class PostgreSqlRewriteCliFailure(
    code: String,
    status: String,
    message: String,
    exitCode: Int,
    context: Map[String, String] = Map.empty
) {
  require(code != null && code.nonEmpty, "CLI failure code must not be blank")
  require(status != null && status.nonEmpty, "CLI failure status must not be blank")
  require(message != null && message.nonEmpty, "CLI failure message must not be blank")
  require(exitCode != PostgreSqlRewriteExitCode.Rewritten, "failure exit must be nonzero")
  require(context != null, "CLI failure context must not be null")
  require(
    context.forall { case (key, value) =>
      key != null && key.nonEmpty && value != null
    },
    "CLI failure context must contain non-null keys and values"
  )
}

private[cli] object PostgreSqlRewriteCliFailure {
  def invocation(
      message: String,
      context: Map[String, String] = Map.empty
  ): PostgreSqlRewriteCliFailure =
    PostgreSqlRewriteCliFailure(
      code = "CLI_INVALID_INVOCATION",
      status = "invalid_invocation",
      message = message,
      exitCode = PostgreSqlRewriteExitCode.InvalidInvocation,
      context = context
    )

  def input(
      message: String,
      source: String
  ): PostgreSqlRewriteCliFailure =
    PostgreSqlRewriteCliFailure(
      code = "CLI_INPUT_ERROR",
      status = "input_error",
      message = message,
      exitCode = PostgreSqlRewriteExitCode.InputError,
      context = Map("source" -> source)
    )

  def unavailable(
      code: String,
      message: String
  ): PostgreSqlRewriteCliFailure =
    PostgreSqlRewriteCliFailure(
      code = code,
      status = "service_unavailable",
      message = message,
      exitCode = PostgreSqlRewriteExitCode.ServiceUnavailable
    )

  val internal: PostgreSqlRewriteCliFailure = PostgreSqlRewriteCliFailure(
    code = "CLI_INTERNAL_ERROR",
    status = "internal_error",
    message = "An unexpected CLI failure occurred",
    exitCode = PostgreSqlRewriteExitCode.InternalError
  )
}

private[cli] object PostgreSqlRewriteCliConfig {
  val JdbcUrlEnvironment: String = "YANNAKAKIS_JDBC_URL"
  val UserEnvironment: String = "YANNAKAKIS_DB_USER"
  val PasswordEnvironment: String = "YANNAKAKIS_DB_PASSWORD"
  val SchemaEnvironment: String = "YANNAKAKIS_DB_SCHEMA"

  val Usage: String =
    """Usage:
      |  PostgreSqlRewriteCli rewrite (--sql-file FILE | --stdin) [options]
      |
      |Options:
      |  --schema NAME           PostgreSQL schema; overrides YANNAKAKIS_DB_SCHEMA
      |  --width-bound N         Fixed soft-width bound (default: 2)
      |  --plan-limit N          Exact structural top-k limit (default: 10)
      |  --selection POLICY      structural-head (default), postgresql-static-shape-v1,
      |                          postgresql-explain-row-guarded-total-cost-v1,
      |                          postgresql-explain-factor-aware-progressive-v1,
      |                          postgresql-explain-count-stage-cost-progressive-v1,
      |                          or postgresql-explain-factor-aware-physical-portfolio-v1
      |                          (physical portfolio requires --plan-limit 100
      |                           and forbids --reduction-schedule)
      |  --candidate-rank N      Select exactly one retained structural rank
      |  --candidate-id ID       Select exactly one retained sha256 candidate id
      |  --reduction-schedule M  full (default), bottom-up, or none
      |  --explain-timeout-seconds N
      |                          Per-statement EXPLAIN-session timeout (default: 30)
      |  --connected-cover       Require ConCov (default)
      |  --no-connected-cover    Disable the ConCov restriction
      |  --help                  Show this help
      |
      |Required environment:
      |  YANNAKAKIS_JDBC_URL
      |  YANNAKAKIS_DB_USER
      |  YANNAKAKIS_DB_PASSWORD
      |  YANNAKAKIS_DB_SCHEMA    unless --schema is supplied
      |
      |The password is accepted only through the environment. SQL is accepted
      |only from a UTF-8 file or standard input.
      |""".stripMargin

  def parse(
      arguments: Array[String],
      environment: Map[String, String]
  ): Either[PostgreSqlRewriteCliFailure, PostgreSqlRewriteCliCommand] = {
    if (arguments == null) {
      return Left(PostgreSqlRewriteCliFailure.invocation("Arguments were not provided"))
    }
    if (environment == null) {
      return Left(PostgreSqlRewriteCliFailure.internal)
    }

    val args = arguments.toVector
    if (args == Vector("--help") || args == Vector("-h") ||
        args == Vector("rewrite", "--help") || args == Vector("rewrite", "-h")) {
      return Right(PostgreSqlRewriteCliCommand.Help)
    }
    if (args.headOption != Some("rewrite")) {
      return Left(
        PostgreSqlRewriteCliFailure.invocation(
          "The first argument must be the 'rewrite' command"
        )
      )
    }

    var sqlFile: Option[String] = None
    var useStandardInput = false
    var schemaFlag: Option[String] = None
    var widthBound = RewriteOptions.DefaultWidthBound
    var planLimit = RewriteOptions.DefaultPlanLimit
    var requireConnectedCover = RewriteOptions.DefaultRequireConnectedCover
    var connectedCoverFlag: Option[Boolean] = None
    var selectionFlag: Option[CandidatePlanSelection] = None
    var candidateRankFlag: Option[Int] = None
    var candidateIdFlag: Option[CandidatePlanId] = None
    var reductionMode = YannakakisReductionMode.Default
    var explainTimeoutSeconds =
      PostgreSqlExplainSelectionLimits.default.statementTimeoutSeconds
    var index = 1
    val seen = scala.collection.mutable.Set.empty[String]

    def duplicate(option: String): Left[PostgreSqlRewriteCliFailure, Nothing] =
      Left(
        PostgreSqlRewriteCliFailure.invocation(
          "An option was supplied more than once",
          Map("option" -> option)
        )
      )

    def valueAfter(
        option: String
    ): Either[PostgreSqlRewriteCliFailure, String] = {
      if (
        index + 1 >= args.size || args(index + 1) == null ||
        args(index + 1).startsWith("--")
      ) {
        Left(
          PostgreSqlRewriteCliFailure.invocation(
            "An option value is missing",
            Map("option" -> option)
          )
        )
      } else Right(args(index + 1))
    }

    while (index < args.size) {
      val option = args(index)
      if (args(index) == null) {
        return Left(
          PostgreSqlRewriteCliFailure.invocation("An argument must not be null")
        )
      }
      option match {
        case "--sql-file" | "--schema" | "--width-bound" | "--plan-limit" |
            "--selection" | "--candidate-rank" | "--candidate-id" |
            "--reduction-schedule" | "--explain-timeout-seconds" =>
          if (seen(option)) return duplicate(option)
          seen += option
          valueAfter(option) match {
            case Left(problem) => return Left(problem)
            case Right(value) =>
              option match {
                case "--sql-file" => sqlFile = Some(value)
                case "--schema"   => schemaFlag = Some(value)
                case "--width-bound" =>
                  parseInteger(option, value) match {
                    case Left(problem) => return Left(problem)
                    case Right(parsed) => widthBound = parsed
                  }
                case "--plan-limit" =>
                  parseInteger(option, value) match {
                    case Left(problem) => return Left(problem)
                    case Right(parsed) => planLimit = parsed
                  }
                case "--selection" =>
                  parseSelection(value) match {
                    case Left(problem) => return Left(problem)
                    case Right(parsed) => selectionFlag = Some(parsed)
                  }
                case "--candidate-rank" =>
                  parseInteger(option, value) match {
                    case Left(problem) => return Left(problem)
                    case Right(parsed) if parsed <= 0 =>
                      return Left(
                        PostgreSqlRewriteCliFailure.invocation(
                          "Candidate rank must be a positive integer",
                          Map("option" -> option)
                        )
                      )
                    case Right(parsed) => candidateRankFlag = Some(parsed)
                  }
                case "--candidate-id" =>
                  CandidatePlanId.parse(value) match {
                    case Left(_) =>
                      return Left(
                        PostgreSqlRewriteCliFailure.invocation(
                          "Candidate id must use canonical lowercase SHA-256 form",
                          Map("option" -> option)
                        )
                      )
                    case Right(parsed) => candidateIdFlag = Some(parsed)
                  }
                case "--reduction-schedule" =>
                  YannakakisReductionMode.parse(value) match {
                    case Left(_) =>
                      return Left(
                        PostgreSqlRewriteCliFailure.invocation(
                          "Reduction schedule must be full, bottom-up, or none",
                          Map("option" -> option)
                        )
                      )
                    case Right(parsed) => reductionMode = parsed
                  }
                case "--explain-timeout-seconds" =>
                  parseInteger(option, value) match {
                    case Left(problem) => return Left(problem)
                    case Right(parsed) if parsed <= 0 =>
                      return Left(
                        PostgreSqlRewriteCliFailure.invocation(
                          "EXPLAIN timeout must be a positive integer",
                          Map("option" -> option)
                        )
                      )
                    case Right(parsed) => explainTimeoutSeconds = parsed
                  }
                case _ => ()
              }
              index += 2
          }

        case "--stdin" =>
          if (seen(option)) return duplicate(option)
          seen += option
          useStandardInput = true
          index += 1

        case "--connected-cover" | "--no-connected-cover" =>
          if (seen(option)) return duplicate(option)
          if (connectedCoverFlag.nonEmpty) {
            return Left(
              PostgreSqlRewriteCliFailure.invocation(
                "Connected-cover options are mutually exclusive",
                Map("option" -> option)
              )
            )
          }
          seen += option
          requireConnectedCover = option == "--connected-cover"
          connectedCoverFlag = Some(requireConnectedCover)
          index += 1

        case _ =>
          return Left(
            PostgreSqlRewriteCliFailure.invocation(
              "An unsupported argument was supplied",
              Map("argument" -> "unsupported")
            )
          )
      }
    }

    val source = (sqlFile, useStandardInput) match {
      case (Some(_), true) =>
        return Left(
          PostgreSqlRewriteCliFailure.invocation(
            "Exactly one SQL input source must be selected",
            Map("options" -> "--sql-file,--stdin")
          )
        )
      case (None, false) =>
        return Left(
          PostgreSqlRewriteCliFailure.invocation(
            "Exactly one SQL input source must be selected",
            Map("options" -> "--sql-file,--stdin")
          )
        )
      case (Some(value), false) =>
        if (value.trim.isEmpty) {
          return Left(
            PostgreSqlRewriteCliFailure.invocation(
              "The SQL file path must not be blank",
              Map("option" -> "--sql-file")
            )
          )
        }
        try SqlSource.File(Paths.get(value))
        catch {
          case _: InvalidPathException =>
            return Left(
              PostgreSqlRewriteCliFailure.invocation(
                "The SQL file path is invalid",
                Map("option" -> "--sql-file")
              )
            )
        }
      case (None, true) => SqlSource.StandardInput
    }

    // Invocation semantics are catalog-independent. Diagnose selector
    // conflicts and cross-option rank errors before requiring DB settings.
    val suppliedSelectors = Vector(
      selectionFlag.map(_ => "--selection"),
      candidateRankFlag.map(_ => "--candidate-rank"),
      candidateIdFlag.map(_ => "--candidate-id")
    ).flatten
    if (suppliedSelectors.size > 1) {
      return Left(
        PostgreSqlRewriteCliFailure.invocation(
          "Candidate selection options are mutually exclusive",
          Map("options" -> suppliedSelectors.sorted.mkString(","))
        )
      )
    }
    val candidateSelection = selectionFlag
      .orElse(candidateRankFlag.map(CandidatePlanSelection.StructuralRank))
      .orElse(candidateIdFlag.map(CandidatePlanSelection.ExactCandidateId))
      .getOrElse(CandidatePlanSelection.Default)

    if (
      seen("--explain-timeout-seconds") &&
      candidateSelection != CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1 &&
      candidateSelection !=
        CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 &&
      candidateSelection !=
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 &&
      candidateSelection !=
        CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
    ) {
      return Left(
        PostgreSqlRewriteCliFailure.invocation(
          "The EXPLAIN timeout requires PostgreSQL EXPLAIN cost selection",
          Map("option" -> "--explain-timeout-seconds")
        )
      )
    }

    if (
      candidateSelection ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 &&
      planLimit != RewriteOptions.CountStageCostProgressivePlanLimit
    ) {
      return Left(
        PostgreSqlRewriteCliFailure.invocation(
          "The COUNT stage-cost progressive policy requires --plan-limit 100",
          Map(
            "option" -> "--plan-limit",
            "value" -> planLimit.toString,
            "valueSource" ->
              (if (seen("--plan-limit")) "supplied" else "default"),
            "required" ->
              RewriteOptions.CountStageCostProgressivePlanLimit.toString,
            "selectionPolicy" -> candidateSelection.id
          )
        )
      )
    }

    if (
      candidateSelection ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 &&
      reductionMode !=
        at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode.FullTwoPass
    ) {
      return Left(
        PostgreSqlRewriteCliFailure.invocation(
          "The COUNT stage-cost progressive policy requires --reduction-schedule full",
          Map(
            "option" -> "--reduction-schedule",
            "value" -> reductionMode.id,
            "required" ->
              at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode.FullTwoPass.id,
            "selectionPolicy" -> candidateSelection.id
          )
        )
      )
    }

    if (
      candidateSelection ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 &&
      seen("--reduction-schedule")
    ) {
      return Left(
        PostgreSqlRewriteCliFailure.invocation(
          "The physical portfolio policy explores reduction schedules and does not accept --reduction-schedule",
          Map(
            "option" -> "--reduction-schedule",
            "selectionPolicy" -> candidateSelection.id
          )
        )
      )
    }

    if (
      candidateSelection ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 &&
      planLimit != RewriteOptions.PhysicalPortfolioPlanLimit
    ) {
      return Left(
        PostgreSqlRewriteCliFailure.invocation(
          "The physical portfolio policy requires --plan-limit 100",
          Map(
            "option" -> "--plan-limit",
            "value" -> planLimit.toString,
            "valueSource" ->
              (if (seen("--plan-limit")) "supplied" else "default"),
            "required" -> RewriteOptions.PhysicalPortfolioPlanLimit.toString,
            "selectionPolicy" -> candidateSelection.id
          )
        )
      )
    }

    val explainLimits = PostgreSqlExplainSelectionLimits
      .create(statementTimeoutSeconds = explainTimeoutSeconds) match {
      case Left(_) =>
        return Left(
          PostgreSqlRewriteCliFailure.invocation(
            "PostgreSQL EXPLAIN limits are invalid",
            Map("option" -> "--explain-timeout-seconds")
          )
        )
      case Right(value) => value
    }
    val factorAwareLimits =
      if (
        candidateSelection ==
          CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 ||
        candidateSelection ==
          CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
      ) {
        PostgreSqlFactorAwareSelectionLimits
          .create(statementTimeoutSeconds = explainTimeoutSeconds) match {
          case Left(_) =>
            return Left(
              PostgreSqlRewriteCliFailure.invocation(
                "PostgreSQL factor-aware limits are invalid",
                Map("option" -> "--explain-timeout-seconds")
              )
            )
          case Right(value) => value
        }
      } else PostgreSqlFactorAwareSelectionLimits.default
    val physicalPortfolioEvaluationLimits =
      if (
        candidateSelection ==
          CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
      ) {
        PostgreSqlPhysicalPortfolioEvaluationLimits
          .create(statementTimeoutSeconds = explainTimeoutSeconds) match {
          case Left(_) =>
            return Left(
              PostgreSqlRewriteCliFailure.invocation(
                "PostgreSQL physical portfolio limits are invalid",
                Map("option" -> "--explain-timeout-seconds")
              )
            )
          case Right(value) => value
        }
      } else PostgreSqlPhysicalPortfolioEvaluationLimits.default

    val options = RewriteOptions.validated(
      widthBound,
      planLimit,
      requireConnectedCover,
      candidateSelection,
      reductionMode
    ) match {
      case Left(diagnostics) =>
        val invalidFields = diagnostics.iterator
          .flatMap(_.context.get("option"))
          .toVector
          .distinct
          .sorted
          .mkString(",")
        return Left(
          PostgreSqlRewriteCliFailure.invocation(
            "Rewrite options are invalid",
            Map("options" -> invalidFields)
          )
        )
      case Right(value) => value
    }

    val schemaName = schemaFlag.orElse(environment.get(SchemaEnvironment)) match {
      case Some(value) if value != null && value.trim.nonEmpty => value
      case _ =>
        return Left(
          missingConfiguration(
            Vector(if (schemaFlag.isDefined) "--schema" else SchemaEnvironment)
          )
        )
    }

    val missingEnvironment = Vector(
      JdbcUrlEnvironment -> true,
      UserEnvironment -> true,
      PasswordEnvironment -> false
    ).collect {
      case (name, requireNonBlank)
          if environment.get(name).forall(value =>
            value == null || (requireNonBlank && value.trim.isEmpty)
          ) => name
    }
    if (missingEnvironment.nonEmpty) {
      return Left(missingConfiguration(missingEnvironment))
    }

    Right(
      PostgreSqlRewriteCliCommand.Rewrite(
        PostgreSqlRewriteCliConfig(
          sqlSource = source,
          schemaName = schemaName,
          options = options,
          explainLimits = explainLimits,
          factorAwareLimits = factorAwareLimits,
          jdbc = JdbcEnvironment(
            jdbcUrl = environment(JdbcUrlEnvironment),
            user = environment(UserEnvironment),
            password = environment(PasswordEnvironment)
          ),
          physicalPortfolioLimits = PostgreSqlPhysicalPortfolioLimits.default,
          physicalPortfolioEvaluationLimits =
            physicalPortfolioEvaluationLimits
        )
      )
    )
  }

  private def parseInteger(
      option: String,
      value: String
  ): Either[PostgreSqlRewriteCliFailure, Int] =
    value.toIntOption match {
      case Some(parsed) => Right(parsed)
      case None =>
        Left(
          PostgreSqlRewriteCliFailure.invocation(
            "An option requires a base-10 integer",
            Map("option" -> option)
          )
        )
    }

  private def parseSelection(
      value: String
  ): Either[PostgreSqlRewriteCliFailure, CandidatePlanSelection] = value match {
    case "structural-head" => Right(CandidatePlanSelection.StructuralHead)
    case "postgresql-static-shape-v1" =>
      Right(CandidatePlanSelection.PostgreSqlStaticShapeV1)
    case "postgresql-explain-row-guarded-total-cost-v1" =>
      Right(CandidatePlanSelection.PostgreSqlExplainGuardedTotalCostV1)
    case "postgresql-explain-factor-aware-progressive-v1" =>
      Right(CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1)
    case "postgresql-explain-count-stage-cost-progressive-v1" =>
      Right(
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
      )
    case "postgresql-explain-factor-aware-physical-portfolio-v1" =>
      Right(
        CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
      )
    case _ =>
      Left(
        PostgreSqlRewriteCliFailure.invocation(
          "Candidate selection policy is unsupported",
          Map("option" -> "--selection")
        )
      )
  }

  private def missingConfiguration(
      names: Vector[String]
  ): PostgreSqlRewriteCliFailure =
    PostgreSqlRewriteCliFailure.invocation(
      "Required CLI configuration is missing",
      Map("required" -> names.sorted.mkString(","))
    )
}
