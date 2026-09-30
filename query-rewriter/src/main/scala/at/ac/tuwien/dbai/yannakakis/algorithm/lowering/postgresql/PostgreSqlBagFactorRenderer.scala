package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlan,
  BagFactorPlanningLimits,
  BagFactorPlanValidator,
  ConfiguredBagFactorPlan,
  ConfiguredBagFactorPlanValidator
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  QueryHypergraphConverter,
  QueryIR
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlCteMaterializationDirective

/** PostgreSQL SQL-text renderer for validated Calcite IR bag-factor plans.
  *
  * Every legacy `render` overload retains full two-pass reduction. Callers
  * must opt into another request-wide mode through `renderWithReductionMode`
  * or the explicit six-argument `render` overload.
  */
object PostgreSqlBagFactorRenderer {
  def render(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] =
    render(
      query,
      input,
      plan,
      PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default,
      YannakakisReductionMode.Default
    )

  /** Renders with an explicit reduction mode and default resource limits. */
  def renderWithReductionMode(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      reductionMode: YannakakisReductionMode
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] =
    render(
      query,
      input,
      plan,
      PostgreSqlRenderingLimits.default,
      BagFactorPlanningLimits.default,
      reductionMode
    )

  def render(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] =
    render(
      query,
      input,
      plan,
      limits,
      BagFactorPlanningLimits.default,
      YannakakisReductionMode.Default
    )

  /** Renders with explicit rendering limits and reduction mode. */
  def renderWithReductionMode(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] =
    render(
      query,
      input,
      plan,
      limits,
      BagFactorPlanningLimits.default,
      reductionMode
    )

  /**
    * Renders a plan using explicit, independently enforced construction and
    * rendering resource envelopes.
    *
    * The planning limits are validation limits only: they never cause a plan
    * to be rebuilt or truncated.
    */
  def render(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] =
    render(
      query,
      input,
      plan,
      limits,
      planningValidationLimits,
      YannakakisReductionMode.Default
    )

  /**
    * Renders one validated canonical plan with the declared reduction subset.
    */
  def render(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] =
    compileAndRender(
      query,
      input,
      plan,
      limits,
      planningValidationLimits,
      reductionMode
    ).map(_._2)

  /** Narrow internal facade for hash-bound per-stage EXPLAIN inputs. */
  private[yannakakis] def renderWithStageDiagnostics(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      diagnosticLimits: PostgreSqlStageDiagnosticLimits,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile =
        PostgreSqlEnumerationRenderingProfile.default
  ): Either[
    Vector[PostgreSqlRenderingError],
    PostgreSqlStageDiagnosticRendering
  ] =
    PostgreSqlStageDiagnosticRenderer.render(
      query,
      input,
      plan,
      limits,
      planningValidationLimits,
      reductionMode,
      diagnosticLimits,
      enumerationRenderingProfile
    )

  /** Internal compatibility facade for a named enumeration emitter profile. */
  private[yannakakis] def renderWithEnumerationProfile(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] =
    compileAndRenderWithAggregateProfile(
      query,
      input,
      plan,
      limits,
      planningValidationLimits,
      reductionMode,
      PostgreSqlAggregateRenderingProfile.LegacyV1,
      enumerationRenderingProfile
    ).map(_._2)

  /**
    * Internal compiled-only compatibility facade for a named enumeration
    * emitter profile.
    *
    * This deliberately shares the complete validation boundary used by the
    * normal renderer while avoiding SQL materialization. It is intended for
    * specialized renderers that need the validated output descriptor but emit
    * their own SQL text.
    */
  private[postgresql] def compileValidatedWithEnumerationProfile(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): Either[Vector[PostgreSqlRenderingError], CompiledPostgreSqlRendering] = {
    val profileV2 = PostgreSqlAggregateRenderingProfileV2
      .baseline(PostgreSqlAggregateRenderingProfile.LegacyV1)
      .fold(
        problems =>
          return Left(
            Vector(
              PostgreSqlRenderingError.ModelInvariantViolation(
                "Frozen aggregate rendering profile cannot be wrapped by renderer v2: " +
                  problems.map(_.code).mkString(", ")
              )
            )
          ),
        identity
      )
    val profileV3 = PostgreSqlAggregateRenderingProfileV3
      .baseline(profileV2)
      .fold(
        problems =>
          return Left(
            Vector(
              PostgreSqlRenderingError.ModelInvariantViolation(
                "Renderer-v2 aggregate profile cannot be wrapped by renderer v3: " +
                  problems.map(_.code).mkString(", ")
              )
            )
          ),
        identity
      )
    compileValidatedWithAggregateProfileV3(
      query,
      input,
      plan,
      limits,
      planningValidationLimits,
      reductionMode,
      profileV3,
      enumerationRenderingProfile
    )
  }

  /**
    * Renders one independently validated physical orientation. The certified
    * structural decomposition remains attached to the configured plan.
    */
  private[yannakakis] def renderConfigured(
      query: QueryIR,
      input: CandidateSearchInput,
      configured: ConfiguredBagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective
  ): Either[Vector[PostgreSqlRenderingError], PostgreSqlRenderedQuery] =
    compileAndRenderConfigured(
      query,
      input,
      configured,
      limits,
      planningValidationLimits,
      reductionMode,
      cteMaterialization
    ).map(_._2)

  /** Physical-portfolio facade for hash-bound per-stage EXPLAIN inputs. */
  private[yannakakis] def renderConfiguredWithStageDiagnostics(
      query: QueryIR,
      input: CandidateSearchInput,
      configured: ConfiguredBagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective,
      diagnosticLimits: PostgreSqlStageDiagnosticLimits
  ): Either[
    Vector[PostgreSqlRenderingError],
    PostgreSqlStageDiagnosticRendering
  ] =
    PostgreSqlStageDiagnosticRenderer.renderConfigured(
      query,
      input,
      configured,
      limits,
      planningValidationLimits,
      reductionMode,
      cteMaterialization,
      diagnosticLimits
    )

  /** One validated compilation shared by normal and diagnostic rendering. */
  private[postgresql] def compileAndRender(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode
  ): Either[
    Vector[PostgreSqlRenderingError],
    (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
  ] =
    compileAndRenderWithAggregateProfile(
      query,
      input,
      plan,
      limits,
      planningValidationLimits,
      reductionMode,
      PostgreSqlAggregateRenderingProfile.LegacyV1
    )

  /**
    * Development-only generated-SQL boundary. The profile is intentionally
    * absent from public rewrite requests and frozen artifact identities.
    */
  private[yannakakis] def compileAndRenderWithAggregateProfile(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      aggregateRenderingProfile: PostgreSqlAggregateRenderingProfile,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile =
        PostgreSqlEnumerationRenderingProfile.default
  ): Either[
    Vector[PostgreSqlRenderingError],
    (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
  ] = {
    val profileV2 =
      if (aggregateRenderingProfile == null) null
      else
        PostgreSqlAggregateRenderingProfileV2
          .baseline(aggregateRenderingProfile)
          .fold(
            problems =>
              return Left(
                Vector(
                  PostgreSqlRenderingError.ModelInvariantViolation(
                    "Frozen aggregate rendering profile cannot be wrapped by renderer v2: " +
                      problems.map(_.code).mkString(", ")
                  )
                )
              ),
            identity
          )
    compileAndRenderWithAggregateProfileV2(
      query,
      input,
      plan,
      limits,
      planningValidationLimits,
      reductionMode,
      profileV2,
      enumerationRenderingProfile
    )
  }

  /** Additive renderer-v2 boundary; not part of public rewrite requests. */
  private[yannakakis] def compileAndRenderWithAggregateProfileV2(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      aggregateRenderingProfileV2: PostgreSqlAggregateRenderingProfileV2,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile =
        PostgreSqlEnumerationRenderingProfile.default
  ): Either[
    Vector[PostgreSqlRenderingError],
    (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
  ] = {
    val profileV3 =
      if (aggregateRenderingProfileV2 == null) null
      else
        PostgreSqlAggregateRenderingProfileV3
          .baseline(aggregateRenderingProfileV2)
          .fold(
            problems =>
              return Left(
                Vector(
                  PostgreSqlRenderingError.ModelInvariantViolation(
                    "Renderer-v2 aggregate profile cannot be wrapped by renderer v3: " +
                      problems.map(_.code).mkString(", ")
                  )
                )
              ),
            identity
          )
    compileAndRenderWithAggregateProfileV3(
      query,
      input,
      plan,
      limits,
      planningValidationLimits,
      reductionMode,
      profileV3,
      enumerationRenderingProfile
    )
  }

  /** Additive renderer-v3 boundary; not part of public rewrite requests. */
  private[yannakakis] def compileAndRenderWithAggregateProfileV3(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      aggregateRenderingProfileV3: PostgreSqlAggregateRenderingProfileV3,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile =
        PostgreSqlEnumerationRenderingProfile.default
  ): Either[
    Vector[PostgreSqlRenderingError],
    (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
  ] =
    compileValidatedWithAggregateProfileV3(
      query,
      input,
      plan,
      limits,
      planningValidationLimits,
      reductionMode,
      aggregateRenderingProfileV3,
      enumerationRenderingProfile
    ).flatMap(compiled => materialize(compiled, query, plan, limits))

  /**
    * Compiles only after applying the exact validation boundary shared by the
    * normal renderer-v3 entry point.
    */
  private[postgresql] def compileValidatedWithAggregateProfileV3(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      aggregateRenderingProfileV3: PostgreSqlAggregateRenderingProfileV3,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile =
        PostgreSqlEnumerationRenderingProfile.default
  ): Either[Vector[PostgreSqlRenderingError], CompiledPostgreSqlRendering] = {
    val missing = Vector.newBuilder[PostgreSqlRenderingError]
    if (query eq null) {
      missing += PostgreSqlRenderingError.MissingValue("source QueryIR")
    }
    if (input eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "candidate-search input"
      )
    }
    if (plan eq null) {
      missing += PostgreSqlRenderingError.MissingValue("bag-factor plan")
    }
    if (limits eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "PostgreSQL rendering limits"
      )
    }
    if (planningValidationLimits eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "bag-factor planning validation limits"
      )
    }
    if (reductionMode eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "Yannakakis reduction mode"
      )
    }
    if (aggregateRenderingProfileV3 eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "PostgreSQL aggregate rendering profile"
      )
    }
    if (enumerationRenderingProfile eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "PostgreSQL enumeration rendering profile"
      )
    }
    val missingErrors = PostgreSqlRenderingError.canonical(missing.result())
    if (missingErrors.nonEmpty) return Left(missingErrors)

    if (query.output.map(_.ordinal) != query.output.indices.toVector) {
      return Left(
        Vector(
          PostgreSqlRenderingError.ModelInvariantViolation(
            "QueryIR output slots must use stable contiguous positional ordinals"
          )
        )
      )
    }

    val converted = QueryHypergraphConverter.convert(query) match {
      case Left(diagnostics) =>
        return Left(
          Vector(
            PostgreSqlRenderingError.QueryConversionFailure(diagnostics)
          )
        )
      case Right(hypergraph) => hypergraph
    }
    if (converted != input.hypergraph) {
      return Left(
        Vector(
          PostgreSqlRenderingError.QueryHypergraphMismatch(
            input.hypergraph,
            converted
          )
        )
      )
    }

    BagFactorPlanValidator.validate(
      input,
      plan.decomposition,
      plan,
      planningValidationLimits
    ) match {
      case Left(errors) =>
        return Left(
          Vector(PostgreSqlRenderingError.PlanValidationFailure(errors))
        )
      case Right(_) => ()
    }

    PostgreSqlRenderingCompiler.compileWithAggregateProfileV3(
      query,
      input,
      plan,
      limits,
      reductionMode,
      PostgreSqlCteMaterializationDirective.PlannerControlled,
      aggregateRenderingProfileV3,
      enumerationRenderingProfile
    )
  }

  private[postgresql] def compileAndRenderConfigured(
      query: QueryIR,
      input: CandidateSearchInput,
      configured: ConfiguredBagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      planningValidationLimits: BagFactorPlanningLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective
  ): Either[
    Vector[PostgreSqlRenderingError],
    (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
  ] = {
    val missing = Vector.newBuilder[PostgreSqlRenderingError]
    if (query eq null) {
      missing += PostgreSqlRenderingError.MissingValue("source QueryIR")
    }
    if (input eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "candidate-search input"
      )
    }
    if (configured eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "configured bag-factor plan"
      )
    }
    if (limits eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "PostgreSQL rendering limits"
      )
    }
    if (planningValidationLimits eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "bag-factor planning validation limits"
      )
    }
    if (reductionMode eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "Yannakakis reduction mode"
      )
    }
    if (cteMaterialization eq null) {
      missing += PostgreSqlRenderingError.MissingValue(
        "PostgreSQL CTE materialization directive"
      )
    }
    val missingErrors = PostgreSqlRenderingError.canonical(missing.result())
    if (missingErrors.nonEmpty) return Left(missingErrors)

    if (query.output.map(_.ordinal) != query.output.indices.toVector) {
      return Left(
        Vector(
          PostgreSqlRenderingError.ModelInvariantViolation(
            "QueryIR output slots must use stable contiguous positional ordinals"
          )
        )
      )
    }

    val converted = QueryHypergraphConverter.convert(query) match {
      case Left(diagnostics) =>
        return Left(
          Vector(
            PostgreSqlRenderingError.QueryConversionFailure(diagnostics)
          )
        )
      case Right(hypergraph) => hypergraph
    }
    if (converted != input.hypergraph) {
      return Left(
        Vector(
          PostgreSqlRenderingError.QueryHypergraphMismatch(
            input.hypergraph,
            converted
          )
        )
      )
    }

    ConfiguredBagFactorPlanValidator.validate(
      input,
      configured.structuralDecomposition,
      configured,
      planningValidationLimits
    ) match {
      case Left(errors) =>
        return Left(
          Vector(
            PostgreSqlRenderingError.ConfiguredPlanValidationFailure(errors)
          )
        )
      case Right(_) => ()
    }

    compileAndMaterialize(
      query,
      input,
      configured.plan,
      limits,
      reductionMode,
      cteMaterialization,
      PostgreSqlAggregateRenderingProfile.LegacyV1,
      PostgreSqlEnumerationRenderingProfile.default
    )
  }

  private def compileAndMaterialize(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective,
      aggregateRenderingProfile: PostgreSqlAggregateRenderingProfile,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): Either[
    Vector[PostgreSqlRenderingError],
    (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
  ] =
    PostgreSqlRenderingCompiler
      .compile(
        query,
        input,
        plan,
        limits,
        reductionMode,
        cteMaterialization,
        aggregateRenderingProfile,
        enumerationRenderingProfile
      )
      .flatMap(compiled => materialize(compiled, query, plan, limits))

  private def compileAndMaterialize(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective,
      aggregateRenderingProfileV2: PostgreSqlAggregateRenderingProfileV2,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): Either[
    Vector[PostgreSqlRenderingError],
    (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
  ] =
    PostgreSqlRenderingCompiler
      .compileWithAggregateProfileV2(
        query,
        input,
        plan,
        limits,
        reductionMode,
        cteMaterialization,
        aggregateRenderingProfileV2,
        enumerationRenderingProfile
      )
      .flatMap(compiled => materialize(compiled, query, plan, limits))

  private def compileAndMaterialize(
      query: QueryIR,
      input: CandidateSearchInput,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits,
      reductionMode: YannakakisReductionMode,
      cteMaterialization: PostgreSqlCteMaterializationDirective,
      aggregateRenderingProfileV3: PostgreSqlAggregateRenderingProfileV3,
      enumerationRenderingProfile: PostgreSqlEnumerationRenderingProfile
  ): Either[
    Vector[PostgreSqlRenderingError],
    (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
  ] =
    PostgreSqlRenderingCompiler
      .compileWithAggregateProfileV3(
        query,
        input,
        plan,
        limits,
        reductionMode,
        cteMaterialization,
        aggregateRenderingProfileV3,
        enumerationRenderingProfile
      )
      .flatMap(compiled => materialize(compiled, query, plan, limits))

  private def materialize(
      compiled: CompiledPostgreSqlRendering,
      query: QueryIR,
      plan: BagFactorPlan,
      limits: PostgreSqlRenderingLimits
  ): Either[
    Vector[PostgreSqlRenderingError],
    (CompiledPostgreSqlRendering, PostgreSqlRenderedQuery)
  ] = {
    val characterCount = PostgreSqlEmitter.characterCount(compiled)
    if (characterCount > BigInt(limits.maxSqlCharacters)) {
      Left(
        Vector(
          PostgreSqlRenderingError.ResourceLimitExceeded(
            PostgreSqlRenderingResource.SqlCharacters,
            characterCount,
            limits.maxSqlCharacters
          )
        )
      )
    } else {
      val sql = PostgreSqlEmitter.render(
        compiled,
        characterCount.toInt
      )
      if (BigInt(sql.length) != characterCount) {
        Left(
          Vector(
            PostgreSqlRenderingError.ModelInvariantViolation(
              "PostgreSQL counting and materializing emitters diverged"
            )
          )
        )
      } else {
        val rendered = PostgreSqlRenderedQuery(
          sourceQuery = query,
          bagFactorPlan = plan,
          sql = sql,
          output = compiled.output,
          trace = compiled.trace,
          statistics = compiled.statistics.copy(
            sqlCharacterCount = characterCount.toLong
          )
        )
        Right(compiled -> rendered)
      }
    }
  }
}
