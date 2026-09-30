package at.ac.tuwien.dbai.yannakakis.api

/** Algorithm controls shared by acyclic and future CandidateTD rewriting.
  *
  * The public constructor validates trusted/programmatic use immediately.
  * [[RewriteOptions.validated]] is the non-throwing boundary for CLI and other
  * untrusted inputs and reports every invalid field in one pass.
  */
final case class RewriteOptions(
    widthBound: Int = RewriteOptions.DefaultWidthBound,
    planLimit: Int = RewriteOptions.DefaultPlanLimit,
    requireConnectedCover: Boolean = RewriteOptions.DefaultRequireConnectedCover,
    candidateSelection: CandidatePlanSelection =
      CandidatePlanSelection.Default,
    reductionMode: YannakakisReductionMode = YannakakisReductionMode.Default
) {
  require(widthBound > 0, "widthBound must be a positive integer")
  require(planLimit > 0, "planLimit must be a positive integer")
  require(candidateSelection != null, "candidateSelection must not be null")
  require(reductionMode != null, "reductionMode must not be null")
  candidateSelection match {
    case CandidatePlanSelection.StructuralRank(rank) =>
      require(
        rank <= planLimit,
        "candidate structural rank must not exceed planLimit"
      )
    case CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1 =>
      require(
        planLimit == RewriteOptions.FactorAwareProgressivePlanLimit,
        s"factor-aware progressive selection requires planLimit == ${RewriteOptions.FactorAwareProgressivePlanLimit}"
      )
    case CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 =>
      require(
        planLimit == RewriteOptions.CountStageCostProgressivePlanLimit,
        s"COUNT stage-cost progressive selection requires planLimit == ${RewriteOptions.CountStageCostProgressivePlanLimit}"
      )
      require(
        reductionMode == YannakakisReductionMode.FullTwoPass,
        "COUNT stage-cost progressive selection requires FullTwoPass"
      )
    case CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 =>
      require(
        planLimit == RewriteOptions.PhysicalPortfolioPlanLimit,
        s"physical portfolio selection requires planLimit == ${RewriteOptions.PhysicalPortfolioPlanLimit}"
      )
      require(
        reductionMode == YannakakisReductionMode.FullTwoPass,
        "physical portfolio selection requires FullTwoPass as its neutral request sentinel"
      )
    case _ => ()
  }
}

object RewriteOptions {
  val DefaultWidthBound: Int = 2
  val DefaultPlanLimit: Int = 10
  val DefaultRequireConnectedCover: Boolean = true
  val FactorAwareProgressivePlanLimit: Int = 100
  val CountStageCostProgressivePlanLimit: Int = 100
  val PhysicalPortfolioPlanLimit: Int = 100

  val Default: RewriteOptions = RewriteOptions()

  /** Validates externally supplied option values without throwing.
    *
    * The left side is non-empty and ordered by field declaration.
    */
  def validated(
      widthBound: Int = DefaultWidthBound,
      planLimit: Int = DefaultPlanLimit,
      requireConnectedCover: Boolean = DefaultRequireConnectedCover,
      candidateSelection: CandidatePlanSelection = CandidatePlanSelection.Default,
      reductionMode: YannakakisReductionMode = YannakakisReductionMode.Default
  ): Either[Vector[RewriteDiagnostic], RewriteOptions] = {
    val diagnostics = Vector.newBuilder[RewriteDiagnostic]

    if (widthBound <= 0) {
      diagnostics += invalidPositiveInteger("widthBound", widthBound)
    }
    if (planLimit <= 0) {
      diagnostics += invalidPositiveInteger("planLimit", planLimit)
    }
    if (candidateSelection == null) {
      diagnostics += RewriteDiagnostic(
        code = RewriteReasonCode.InvalidOption,
        message = "candidateSelection must not be null",
        context = Map(
          "option" -> "candidateSelection",
          "constraint" -> "non-null"
        )
      )
    } else {
      candidateSelection match {
        case CandidatePlanSelection.StructuralRank(rank)
            if planLimit > 0 && rank > planLimit =>
          diagnostics += RewriteDiagnostic(
            code = RewriteReasonCode.InvalidOption,
            message = "candidate structural rank must not exceed planLimit",
            context = Map(
              "option" -> "candidateSelection",
              "rank" -> rank.toString,
              "planLimit" -> planLimit.toString,
              "constraint" -> "rank <= planLimit"
            )
          )
        case CandidatePlanSelection.PostgreSqlExplainFactorAwareProgressiveV1
            if planLimit > 0 &&
              planLimit != FactorAwareProgressivePlanLimit =>
          diagnostics += RewriteDiagnostic(
            code = RewriteReasonCode.InvalidOption,
            message =
              s"factor-aware progressive selection requires planLimit == $FactorAwareProgressivePlanLimit",
            context = Map(
              "option" -> "planLimit",
              "value" -> planLimit.toString,
              "constraint" -> s"exactly $FactorAwareProgressivePlanLimit",
              "selectionPolicy" -> candidateSelection.id
            )
          )
        case CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1
            if planLimit > 0 &&
              planLimit != CountStageCostProgressivePlanLimit =>
          diagnostics += RewriteDiagnostic(
            code = RewriteReasonCode.InvalidOption,
            message =
              s"COUNT stage-cost progressive selection requires planLimit == $CountStageCostProgressivePlanLimit",
            context = Map(
              "option" -> "planLimit",
              "value" -> planLimit.toString,
              "constraint" -> s"exactly $CountStageCostProgressivePlanLimit",
              "selectionPolicy" -> candidateSelection.id
            )
          )
        case CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1
            if planLimit > 0 && planLimit != PhysicalPortfolioPlanLimit =>
          diagnostics += RewriteDiagnostic(
            code = RewriteReasonCode.InvalidOption,
            message =
              s"physical portfolio selection requires planLimit == $PhysicalPortfolioPlanLimit",
            context = Map(
              "option" -> "planLimit",
              "value" -> planLimit.toString,
              "constraint" -> s"exactly $PhysicalPortfolioPlanLimit",
              "selectionPolicy" -> candidateSelection.id
            )
          )
        case _ => ()
      }
    }
    if (reductionMode == null) {
      diagnostics += RewriteDiagnostic(
        code = RewriteReasonCode.InvalidOption,
        message = "reductionMode must not be null",
        context = Map(
          "option" -> "reductionMode",
          "constraint" -> "non-null"
        )
      )
    } else if (
      candidateSelection ==
        CandidatePlanSelection.PostgreSqlExplainCountStageCostProgressiveV1 &&
      reductionMode != YannakakisReductionMode.FullTwoPass
    ) {
      diagnostics += RewriteDiagnostic(
        code = RewriteReasonCode.InvalidOption,
        message =
          "COUNT stage-cost progressive selection requires FullTwoPass",
        context = Map(
          "option" -> "reductionMode",
          "value" -> reductionMode.id,
          "constraint" -> YannakakisReductionMode.FullTwoPass.id,
          "selectionPolicy" -> candidateSelection.id
        )
      )
    } else if (
      candidateSelection ==
        CandidatePlanSelection.PostgreSqlExplainFactorAwarePhysicalPortfolioV1 &&
      reductionMode != YannakakisReductionMode.FullTwoPass
    ) {
      diagnostics += RewriteDiagnostic(
        code = RewriteReasonCode.InvalidOption,
        message =
          "physical portfolio selection requires FullTwoPass as its neutral request sentinel",
        context = Map(
          "option" -> "reductionMode",
          "value" -> reductionMode.id,
          "constraint" -> YannakakisReductionMode.FullTwoPass.id,
          "selectionPolicy" -> candidateSelection.id
        )
      )
    }

    val errors = diagnostics.result()
    if (errors.nonEmpty) Left(errors)
    else
      Right(
        RewriteOptions(
          widthBound,
          planLimit,
          requireConnectedCover,
          candidateSelection,
          reductionMode
        )
      )
  }

  private def invalidPositiveInteger(
      option: String,
      value: Int
  ): RewriteDiagnostic =
    RewriteDiagnostic(
      code = RewriteReasonCode.InvalidOption,
      message = s"$option must be a positive integer",
      context = Map(
        "option" -> option,
        "value" -> value.toString,
        "constraint" -> "positive integer"
      )
    )
}
