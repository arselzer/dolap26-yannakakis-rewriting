package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import javax.sql.DataSource

import org.apache.calcite.avatica.util.Casing
import org.apache.calcite.adapter.jdbc.JdbcSchema
import org.apache.calcite.rel.RelRoot
import org.apache.calcite.schema.SchemaPlus
import org.apache.calcite.sql.{
  SqlCall,
  SqlIdentifier,
  SqlJoin,
  SqlKind,
  SqlNode,
  SqlNodeList,
  SqlNumericLiteral,
  SqlSelect
}
import org.apache.calcite.sql.parser.SqlParser
import org.apache.calcite.tools.{FrameworkConfig, Frameworks, Planner}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.control.NonFatal

/** Stable phase at which Calcite SQL planning stopped. */
sealed trait CalciteSqlPlanningStage extends Product with Serializable {
  def id: String
  def invalidatesSql: Boolean
}

object CalciteSqlPlanningStage {
  case object Configuration extends CalciteSqlPlanningStage {
    override val id: String = "configuration"
    override val invalidatesSql: Boolean = false
  }
  case object PlannerCreation extends CalciteSqlPlanningStage {
    override val id: String = "planner-creation"
    override val invalidatesSql: Boolean = false
  }
  case object Parsing extends CalciteSqlPlanningStage {
    override val id: String = "parsing"
    override val invalidatesSql: Boolean = true
  }
  case object Validation extends CalciteSqlPlanningStage {
    override val id: String = "validation"
    override val invalidatesSql: Boolean = true
  }
  case object RelationalConversion extends CalciteSqlPlanningStage {
    override val id: String = "relational-conversion"
    override val invalidatesSql: Boolean = false
  }
  case object PlannerClose extends CalciteSqlPlanningStage {
    override val id: String = "planner-close"
    override val invalidatesSql: Boolean = false
  }
  case object Boundary extends CalciteSqlPlanningStage {
    override val id: String = "boundary"
    override val invalidatesSql: Boolean = false
  }

  val values: Vector[CalciteSqlPlanningStage] = Vector(
    Configuration,
    PlannerCreation,
    Parsing,
    Validation,
    RelationalConversion,
    PlannerClose,
    Boundary
  )
}

/** Data-only Calcite planning failure; no Throwable crosses the public API. */
final case class CalciteSqlPlanningError(
    stage: CalciteSqlPlanningStage,
    message: String,
    exceptionClass: Option[String] = None
) extends Product
    with Serializable {
  require(stage != null, "planning-error stage must not be null")
  require(message != null && message.trim.nonEmpty, "planning-error message must not be blank")
  require(exceptionClass != null, "planning-error exceptionClass must not be null")
  require(
    exceptionClass.forall(value => value != null && value.trim.nonEmpty),
    "planning-error exceptionClass must not contain a null or blank value"
  )
}

/**
  * Pluggable, catalog-aware SQL-to-Calcite boundary.
  *
  * Implementations must treat `sql` as read-only. A `RelRoot` carries
  * Calcite's validated relational field names; the supplied framework path
  * separately captures PostgreSQL result-label evidence before validation.
  * The supplied implementations create and close a fresh Calcite `Planner`
  * per call and are therefore reusable.
  */
trait CalciteSqlPlanner {
  def plan(sql: String): Either[CalciteSqlPlanningError, RelRoot]
}

/** Whether a supported COUNT(*) result name was explicit in the raw SQL AST. */
sealed trait GlobalCountStarAliasMode extends Product with Serializable {
  def id: String
}

object GlobalCountStarAliasMode {
  case object Implicit extends GlobalCountStarAliasMode {
    override val id: String = "implicit"
  }

  case object Explicit extends GlobalCountStarAliasMode {
    override val id: String = "explicit"
  }
}

/**
  * Parser-origin certificate for the only aggregate shape retained by QueryIR.
  *
  * The certificate deliberately stores no source spelling. Explicit result
  * names come from the validated RelRoot; PostgreSQL's implicit COUNT(*) name
  * is represented independently as `count` by the analyzer.
  */
final case class GlobalCountStarSourceCertificate private[calcite] (
    aliasMode: GlobalCountStarAliasMode
) extends Product
    with Serializable {
  require(aliasMode != null, "global COUNT(*) alias mode must not be null")
}

/**
  * Parser-origin proof for the bounded one-key grouped COUNT(*) shape.
  *
  * The direct grouping-column identity is established later from Calcite's
  * validated group ordinal and positional input lineage. As for the scalar
  * COUNT certificate, only the count result's implicit/explicit naming mode
  * must cross the source boundary.
  */
final case class GroupedCountStarSourceCertificate private[calcite] (
    countAliasMode: GlobalCountStarAliasMode
) extends Product
    with Serializable {
  require(
    countAliasMode != null,
    "grouped COUNT(*) alias mode must not be null"
  )
}

/**
  * Parser-origin proof for bounded distributed grouped COUNT(*).
  *
  * The raw certificate establishes the exact select/group/call grammar. Exact
  * column lineage, distinct occurrence ownership, and structural-variable
  * membership are deliberately established at later typed boundaries.
  */
final case class DistributedGroupedCountStarSourceCertificate private[calcite] (
    countAliasMode: GlobalCountStarAliasMode,
    groupKeyCount: Int = 2
) extends Product
    with Serializable {
  require(groupKeyCount >= 2, "distributed grouping requires at least two keys")
  require(
    countAliasMode != null,
    "distributed grouped COUNT(*) alias mode must not be null"
  )
}

/** One left-to-right raw table occurrence used to bind a direct local literal
  * predicate despite Calcite dropping SQL aliases from `TableScan`.
  */
private[yannakakis] final case class LocalIntegralPredicateSourceOccurrence(
    ordinal: Int,
    qualifiedName: Vector[String],
    alias: String
) {
  require(ordinal >= 0, "source occurrence ordinal must be non-negative")
  require(
    qualifiedName != null && qualifiedName.nonEmpty &&
      qualifiedName.forall(value => value != null && value.nonEmpty),
    "source occurrence qualified name must not be empty"
  )
  require(alias != null && alias.nonEmpty, "source occurrence alias must not be empty")
}

/** Raw direct `identifier = exact-integer-literal` evidence in relational
  * predicate traversal order. `literalOnLeft` makes the symmetric spelling an
  * explicit, independently bound case rather than an accidental match.
  */
private[yannakakis] final case class LocalIntegralPredicateSourceBinding(
    predicateOrdinal: Int,
    occurrenceOrdinal: Int,
    alias: String,
    columnName: String,
    literalValue: Long,
    literalPrecision: Int,
    literalOnLeft: Boolean
) {
  require(predicateOrdinal >= 0, "source predicate ordinal must be non-negative")
  require(occurrenceOrdinal >= 0, "source predicate occurrence must be non-negative")
  require(alias != null && alias.nonEmpty, "source predicate alias must not be empty")
  require(
    columnName != null && columnName.nonEmpty,
    "source predicate column must not be empty"
  )
  require(
    literalValue >= 0,
    "the bounded direct source-literal grammar excludes signed prefix expressions"
  )
  require(
    literalPrecision == literalValue.toString.length,
    "source literal precision must be canonical for its exact value"
  )
}

/** Concordant raw/validated-source certificate for the only non-aggregate
  * literal-predicate extension. It contains no SQL text or digest-derived
  * rendering input.
  */
private[yannakakis] final case class LocalIntegralPredicateSourceCertificate(
    totalPredicateCount: Int,
    occurrences: Vector[LocalIntegralPredicateSourceOccurrence],
    predicates: Vector[LocalIntegralPredicateSourceBinding]
) {
  require(totalPredicateCount > 0, "source predicate count must be positive")
  require(occurrences != null && occurrences.nonEmpty, "source occurrences must not be empty")
  require(predicates != null && predicates.nonEmpty, "source predicates must not be empty")
  require(
    occurrences.map(_.ordinal) == occurrences.indices.toVector,
    "source occurrences must use contiguous traversal ordinals"
  )
  require(
    occurrences.map(_.alias).distinct.size == occurrences.size,
    "source occurrence aliases must be unique"
  )
  require(
    predicates.map(_.predicateOrdinal).distinct.size == predicates.size &&
      predicates.forall(_.predicateOrdinal < totalPredicateCount),
    "source local predicates must bind distinct in-range ordinals"
  )
  require(
    predicates.forall(binding =>
      occurrences.lift(binding.occurrenceOrdinal).exists(_.alias == binding.alias)
    ),
    "source local predicates must bind their declared occurrence alias"
  )
}

/** Duplicate-insensitive extremum retained by the bounded 0MA frontend. */
sealed trait GlobalGuardedExtremumFunction extends Product with Serializable {
  def id: String
  def sqlName: String
  final def implicitOutputName: String = id
}

object GlobalGuardedExtremumFunction {
  case object Min extends GlobalGuardedExtremumFunction {
    override val id: String = "min"
    override val sqlName: String = "MIN"
  }

  case object Max extends GlobalGuardedExtremumFunction {
    override val id: String = "max"
    override val sqlName: String = "MAX"
  }

  val values: Vector[GlobalGuardedExtremumFunction] = Vector(Min, Max)
}

/** Whether PostgreSQL's `min`/`max` result name was explicit in raw SQL. */
sealed trait GlobalGuardedExtremumAliasMode
    extends Product
    with Serializable {
  def id: String
}

object GlobalGuardedExtremumAliasMode {
  case object Implicit extends GlobalGuardedExtremumAliasMode {
    override val id: String = "implicit"
  }

  case object Explicit extends GlobalGuardedExtremumAliasMode {
    override val id: String = "explicit"
  }
}

/**
  * Parser-origin proof for one direct top-level global MIN/MAX column call.
  * Guard identity is deliberately absent here and is established later from
  * the validated AggregateCall and positional input lineage.
  */
final case class GlobalGuardedExtremumSourceCertificate private[calcite] (
    function: GlobalGuardedExtremumFunction,
    aliasMode: GlobalGuardedExtremumAliasMode
) extends Product
    with Serializable {
  require(function != null, "global guarded extremum function must not be null")
  require(aliasMode != null, "global guarded extremum alias mode must not be null")
}

/**
  * Immutable catalog facts independently verified before the C03-G0 V2
  * frontend can be constructed. This value is an authorization input, not a
  * substitute for catalog I/O: the planner never queries or infers these
  * fields itself.
  */
private[yannakakis] final case class C03G0CatalogBinding(
    databaseName: String,
    databaseOid: Long,
    physicalSchemaName: String,
    relationName: String,
    groupColumnName: String,
    measureColumnName: String,
    serverMajorVersion: Int,
    namespaceOid: Long,
    relationOid: Long,
    groupAttnum: Int,
    groupTypeOid: Long,
    measureAttnum: Int,
    measureTypeOid: Long,
    groupCollationOid: Long,
    groupCollationName: String,
    groupCollationProvider: String,
    groupCollationDeterministic: Boolean
) extends Product
    with Serializable {
  private def nonBlank(value: String): Boolean =
    value != null && value.nonEmpty && value.trim == value

  require(nonBlank(physicalSchemaName), "C03-G0 physical schema must not be blank")
  require(nonBlank(databaseName), "C03-G0 database name must not be blank")
  require(databaseOid > 0L, "C03-G0 database OID must be positive")
  require(
    relationName == "synthetic_triangle_e0",
    "C03-G0 relation must be synthetic_triangle_e0"
  )
  require(groupColumnName == "payload", "C03-G0 group column must be payload")
  require(measureColumnName == "row_id", "C03-G0 measure column must be row_id")
  require(serverMajorVersion == 18, "C03-G0 requires PostgreSQL major version 18")
  require(namespaceOid > 0L, "C03-G0 namespace OID must be positive")
  require(relationOid > 0L, "C03-G0 relation OID must be positive")
  require(groupAttnum == 4, "C03-G0 payload attribute number must be four")
  require(groupTypeOid == 25L, "C03-G0 payload type OID must be built-in TEXT")
  require(measureAttnum == 1, "C03-G0 row_id attribute number must be one")
  require(measureTypeOid == 20L, "C03-G0 row_id type OID must be built-in INT8")
  require(groupCollationOid > 0L, "C03-G0 group collation OID must be positive")
  require(nonBlank(groupCollationName), "C03-G0 group collation name must not be blank")
  require(
    nonBlank(groupCollationProvider),
    "C03-G0 group collation provider must not be blank"
  )
  require(
    groupCollationDeterministic,
    "C03-G0 group collation must be independently verified deterministic"
  )
}

/** Construction-time aggregate language boundary. Legacy remains the default. */
private[yannakakis] sealed trait CalciteAggregateLanguageProfile
    extends Product
    with Serializable

private[yannakakis] object CalciteAggregateLanguageProfile {
  case object LegacyV1 extends CalciteAggregateLanguageProfile

  final case class C03G0V2(catalogBinding: C03G0CatalogBinding)
      extends CalciteAggregateLanguageProfile {
    require(catalogBinding != null, "C03-G0 catalog binding must not be null")
  }
}

/**
  * Raw/validated-source proof for the exact fixed C03-G0 aggregate vector.
  * Identifiers and aliases are retained as data so later boundaries never
  * recover the source promise from a digest or permissive name comparison.
  */
private[yannakakis] final case class C03G0SourceCertificate private[calcite] (
    sourceSqlSha256: String,
    groupIdentifier: Vector[String],
    measureIdentifier: Vector[String],
    outputAliases: Vector[String],
    catalogBinding: C03G0CatalogBinding
) extends Product
    with Serializable {
  require(
    sourceSqlSha256 == C03G0SourceCertificate.SourceSqlSha256,
    "C03-G0 source SQL must match the frozen fixture bytes"
  )
  require(
    groupIdentifier == C03G0SourceCertificate.GroupIdentifier,
    "C03-G0 group identifier must be exactly e0.payload"
  )
  require(
    measureIdentifier == C03G0SourceCertificate.MeasureIdentifier,
    "C03-G0 measure identifier must be exactly e0.row_id"
  )
  require(
    outputAliases == C03G0SourceCertificate.OutputAliases,
    "C03-G0 output aliases must match the fixed V2 vector"
  )
  require(catalogBinding != null, "C03-G0 source catalog binding must not be null")
}

private[yannakakis] object C03G0SourceCertificate {
  val SourceSqlSha256: String =
    "ab0b34531a6d5ceeec30aee44ce37de3e81f625e060eb5728eb13446ba20005b"
  val GroupIdentifier: Vector[String] = Vector("e0", "payload")
  val MeasureIdentifier: Vector[String] = Vector("e0", "row_id")
  val OutputAliases: Vector[String] = Vector(
    "guard_group",
    "witness_count",
    "nonnull_guard_count",
    "min_guard_row_id",
    "max_guard_row_id"
  )

  private[calcite] def exact(binding: C03G0CatalogBinding): C03G0SourceCertificate =
    C03G0SourceCertificate(
      SourceSqlSha256,
      GroupIdentifier,
      MeasureIdentifier,
      OutputAliases,
      binding
    )
}

/** Finite fail-closed reasons produced by the complete raw-AST walk. */
private[yannakakis] sealed trait CalciteRejectedAggregateReason
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object CalciteRejectedAggregateReason {
  case object WindowCall extends CalciteRejectedAggregateReason {
    override val id: String = "window-call"
  }
  case object GroupedOrDistinct extends CalciteRejectedAggregateReason {
    override val id: String = "grouped-or-distinct"
  }
  case object MultipleAggregateCalls extends CalciteRejectedAggregateReason {
    override val id: String = "multiple-aggregate-calls"
  }
  case object CountExpression extends CalciteRejectedAggregateReason {
    override val id: String = "count-expression"
  }
  case object OtherAggregateFunction extends CalciteRejectedAggregateReason {
    override val id: String = "other-aggregate-function"
  }
  case object AggregateNotDirectTopLevelResult
      extends CalciteRejectedAggregateReason {
    override val id: String = "aggregate-not-direct-top-level-result"
  }
  case object UnsupportedTopLevelAggregateShape
      extends CalciteRejectedAggregateReason {
    override val id: String = "unsupported-top-level-aggregate-shape"
  }

  val values: Vector[CalciteRejectedAggregateReason] = Vector(
    WindowCall,
    GroupedOrDistinct,
    MultipleAggregateCalls,
    CountExpression,
    OtherAggregateFunction,
    AggregateNotDirectTopLevelResult,
    UnsupportedTopLevelAggregateShape
  )
}

/** Source evidence attached only by the framework-backed planner boundary. */
private[yannakakis] sealed trait CalciteSqlSourceEvidence
    extends Product
    with Serializable

private[yannakakis] object CalciteSqlSourceEvidence {
  case object Uncertified extends CalciteSqlSourceEvidence
  case object NonAggregate extends CalciteSqlSourceEvidence
  final case class CertifiedNonAggregateLocalIntegralPredicates(
      certificate: LocalIntegralPredicateSourceCertificate
  ) extends CalciteSqlSourceEvidence {
    require(
      certificate != null,
      "local integral predicate certificate must not be null"
    )
  }
  final case class CertifiedGlobalCountStar(
      certificate: GlobalCountStarSourceCertificate
  ) extends CalciteSqlSourceEvidence {
    require(certificate != null, "global COUNT(*) certificate must not be null")
  }
  final case class CertifiedGroupedCountStar(
      certificate: GroupedCountStarSourceCertificate
  ) extends CalciteSqlSourceEvidence {
    require(certificate != null, "grouped COUNT(*) certificate must not be null")
  }
  final case class CertifiedDistributedGroupedCountStar(
      certificate: DistributedGroupedCountStarSourceCertificate
  ) extends CalciteSqlSourceEvidence {
    require(
      certificate != null,
      "distributed grouped COUNT(*) certificate must not be null"
    )
  }
  final case class CertifiedGlobalGuardedExtremum(
      certificate: GlobalGuardedExtremumSourceCertificate
  ) extends CalciteSqlSourceEvidence {
    require(
      certificate != null,
      "global guarded extremum certificate must not be null"
    )
  }
  final case class CertifiedC03G0(
      certificate: C03G0SourceCertificate
  ) extends CalciteSqlSourceEvidence {
    require(certificate != null, "C03-G0 source certificate must not be null")
  }
  final case class RejectedAggregate(reason: CalciteRejectedAggregateReason)
      extends CalciteSqlSourceEvidence {
    require(reason != null, "rejected aggregate reason must not be null")
  }
}

/** Raw-source evidence for PostgreSQL's positional result labels.
  *
  * Calcite mutates a parsed SELECT while validating it: stars are expanded,
  * duplicate expansion labels are uniquified, and unaliased expressions are
  * assigned `EXPR$<n>` names.  Those are useful relational field names but
  * are not PostgreSQL result labels.  The framework-backed planner therefore
  * captures the raw select-item intent before validation and resolves it
  * against the validated positional expansion afterwards.
  */
private[yannakakis] sealed trait CalciteSqlOutputLabelEvidence
    extends Product
    with Serializable

private[yannakakis] object CalciteSqlOutputLabelEvidence {
  case object Uncertified extends CalciteSqlOutputLabelEvidence

  final case class Certified(names: Vector[String])
      extends CalciteSqlOutputLabelEvidence {
    require(names != null, "certified output labels must not be null")
    require(
      names.forall(name => name != null && name.nonEmpty),
      "certified output labels must not contain null or empty values"
    )
  }

  final case class Rejected(reason: String)
      extends CalciteSqlOutputLabelEvidence {
    require(
      reason != null && reason.trim.nonEmpty,
      "rejected output-label reason must not be blank"
    )
  }
}

/** RelRoot plus raw-source evidence, kept internal to the rewrite pipeline. */
private[yannakakis] final case class CalciteSqlPlannedQuery(
    root: RelRoot,
    sourceEvidence: CalciteSqlSourceEvidence,
    aggregateLanguageProfile: CalciteAggregateLanguageProfile,
    outputLabelEvidence: CalciteSqlOutputLabelEvidence,
    localPredicateCertificate: Option[CalciteLocalPredicates.SourceCertificate] = None,
    textKeyCatalog: Option[PostgreSqlTextKeyCatalog.Capture] = None,
    textKeySourceSafe: Boolean = false
) {
  require(root != null, "planned relational root must not be null")
  require(sourceEvidence != null, "planned source evidence must not be null")
  require(
    aggregateLanguageProfile != null,
    "planned aggregate language profile must not be null"
  )
  require(
    outputLabelEvidence != null,
    "planned output-label evidence must not be null"
  )
}

private[yannakakis] object CalciteSqlPlannedQuery {
  /** Compatibility constructor for tests and alternate planners without raw
    * source-label evidence.
    */
  def apply(
      root: RelRoot,
      sourceEvidence: CalciteSqlSourceEvidence,
      aggregateLanguageProfile: CalciteAggregateLanguageProfile
  ): CalciteSqlPlannedQuery =
    new CalciteSqlPlannedQuery(
      root,
      sourceEvidence,
      aggregateLanguageProfile,
      CalciteSqlOutputLabelEvidence.Uncertified
    )

  /** Compatibility constructor is deliberately and explicitly legacy-only. */
  def apply(
      root: RelRoot,
      sourceEvidence: CalciteSqlSourceEvidence
  ): CalciteSqlPlannedQuery =
    new CalciteSqlPlannedQuery(
      root,
      sourceEvidence,
      CalciteAggregateLanguageProfile.LegacyV1,
      CalciteSqlOutputLabelEvidence.Uncertified
    )
}

/** Optional capability; the public CalciteSqlPlanner trait remains unchanged. */
private[yannakakis] trait CalciteSqlPlannerWithSourceEvidence {
  self: CalciteSqlPlanner =>

  def planWithSourceEvidence(
      sql: String
  ): Either[CalciteSqlPlanningError, CalciteSqlPlannedQuery]
}

object CalciteSqlPlanner {
  val DefaultParserConfig: SqlParser.Config =
    SqlParser.config().withCaseSensitive(false)

  /** PostgreSQL identifier semantics: unquoted names fold to lower case,
    * while quoted names retain exact, case-sensitive spelling.
    */
  val PostgreSqlParserConfig: SqlParser.Config =
    SqlParser
      .config()
      .withUnquotedCasing(Casing.TO_LOWER)
      .withQuotedCasing(Casing.UNCHANGED)
      .withCaseSensitive(true)

  private[yannakakis] val PostgreSqlQualifiedNameParserConfig: SqlParser.Config =
    PostgreSqlParserConfig.withParserFactory(CalcitePostgreSqlQualifiedNames)

  /**
    * Internal capability dispatch. Alternate/custom planners are intentionally
    * unable to certify source syntax and therefore remain fail-closed for all
    * aggregate relational plans.
    */
  private[yannakakis] def planWithSourceEvidence(
      planner: CalciteSqlPlanner,
      sql: String
  ): Either[CalciteSqlPlanningError, CalciteSqlPlannedQuery] = {
    require(planner != null, "Calcite SQL planner must not be null")
    planner match {
      case capable: CalciteSqlPlannerWithSourceEvidence =>
        capable.planWithSourceEvidence(sql)
      case _ =>
        planner
          .plan(sql)
          .map(root =>
            CalciteSqlPlannedQuery(
              root,
              CalciteSqlSourceEvidence.Uncertified,
              CalciteAggregateLanguageProfile.LegacyV1,
              CalciteSqlOutputLabelEvidence.Uncertified
            )
          )
    }
  }

  /** Wraps a reusable Calcite framework configuration without planning yet. */
  def fromFrameworkConfig(
      config: FrameworkConfig
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] = {
    if (config eq null) {
      Left(configurationError("framework config must be provided"))
    } else
      Right(
        new FrameworkCalciteSqlPlanner(
          config,
          CalciteAggregateLanguageProfile.LegacyV1
        )
      )
  }

  /** Additive bounded grouping capability, independent of any benchmark.
    * Legacy factories still certify only two distributed keys. A caller must
    * supply the admission bound; the actual arity is certified against the
    * validated aggregate and existing lowering resource limits still apply.
    */
  private[yannakakis] def forSchemaWithDistributedCountGrouping(
      defaultSchema: SchemaPlus,
      maximumGroupKeys: Int,
      parserConfig: SqlParser.Config = PostgreSqlParserConfig
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] =
    if (defaultSchema == null || parserConfig == null || maximumGroupKeys < 2)
      Left(configurationError("schema, parser config and a grouping bound of at least two are required"))
    else attempt(CalciteSqlPlanningStage.Configuration) {
      new FrameworkCalciteSqlPlanner(
        Frameworks.newConfigBuilder().defaultSchema(defaultSchema)
          .parserConfig(parserConfig).build(),
        CalciteAggregateLanguageProfile.LegacyV1,
        maximumDistributedGroupKeys = maximumGroupKeys
      )
    }

  /** Additive language capabilities; public legacy factories are unchanged. */
  private[yannakakis] def forSchemaWithLocalPredicates(
      defaultSchema: SchemaPlus,
      maximumGroupKeys: Int = 2,
      parserConfig: SqlParser.Config = PostgreSqlQualifiedNameParserConfig
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] =
    if (defaultSchema == null || parserConfig == null || maximumGroupKeys < 2)
      Left(configurationError("schema, parser config and a grouping bound of at least two are required"))
    else attempt(CalciteSqlPlanningStage.Configuration) {
      new FrameworkCalciteSqlPlanner(
        Frameworks.newConfigBuilder().defaultSchema(defaultSchema)
          .parserConfig(parserConfig)
          .sqlToRelConverterConfig(org.apache.calcite.sql2rel.SqlToRelConverter.config()
            .withInSubQueryThreshold(Int.MaxValue)).build(),
        CalciteAggregateLanguageProfile.LegacyV1,
        maximumDistributedGroupKeys = maximumGroupKeys,
        allowLocalPredicates = true
      )
    }

  /** Creates a planner using a caller-owned default schema. */
  def forSchema(
      defaultSchema: SchemaPlus,
      parserConfig: SqlParser.Config = DefaultParserConfig
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] = {
    if (defaultSchema eq null) {
      Left(configurationError("default schema must be provided"))
    } else if (parserConfig eq null) {
      Left(configurationError("parser config must be provided"))
    } else {
      attempt(CalciteSqlPlanningStage.Configuration) {
        Frameworks
          .newConfigBuilder()
          .defaultSchema(defaultSchema)
          .parserConfig(parserConfig)
          .build()
      }.flatMap(fromFrameworkConfig)
    }
  }

  /**
    * Internal additive factory used only after a caller has independently
    * verified the immutable C03-G0 catalog binding. Public/default factories
    * remain on the legacy aggregate language.
    */
  private[yannakakis] def forSchemaWithC03G0V2(
      defaultSchema: SchemaPlus,
      catalogBinding: C03G0CatalogBinding,
      parserConfig: SqlParser.Config = PostgreSqlParserConfig
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] = {
    if (defaultSchema eq null) {
      Left(configurationError("default schema must be provided"))
    } else if (catalogBinding eq null) {
      Left(configurationError("C03-G0 catalog binding must be provided"))
    } else if (parserConfig eq null) {
      Left(configurationError("parser config must be provided"))
    } else if (defaultSchema.getName != catalogBinding.physicalSchemaName) {
      Left(
        configurationError(
          "default schema must match the verified C03-G0 physical schema"
        )
      )
    } else {
      attempt(CalciteSqlPlanningStage.Configuration) {
        Frameworks
          .newConfigBuilder()
          .defaultSchema(defaultSchema)
          .parserConfig(parserConfig)
          .build()
      }.map(config =>
        new FrameworkCalciteSqlPlanner(
          config,
          CalciteAggregateLanguageProfile.C03G0V2(catalogBinding)
        )
      )
    }
  }

  /**
    * Creates a Calcite JDBC schema backed by a caller-owned `DataSource`.
    * When `jdbcSchema` is absent, `schemaName` is also the exact physical JDBC
    * schema. `Some(value)` is the explicit advanced physical-schema override.
    * Neither the planner nor the resulting rewriter closes that data source.
    * PostgreSQL's session-local `pg_temp` alias is not a physical JDBC schema
    * and remains unsupported by these connection-opening factories.
    */
  def forJdbcDataSource(
      dataSource: DataSource,
      schemaName: String,
      jdbcCatalog: Option[String] = None,
      jdbcSchema: Option[String] = None,
      parserConfig: SqlParser.Config = DefaultParserConfig
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] =
    jdbcPlanner(dataSource, schemaName, jdbcCatalog, jdbcSchema, parserConfig,
      schema => forSchema(schema, parserConfig))

  private[yannakakis] def forJdbcDataSourceWithDistributedCountGrouping(
      dataSource: DataSource,
      schemaName: String,
      maximumGroupKeys: Int,
      jdbcCatalog: Option[String] = None,
      jdbcSchema: Option[String] = None,
      parserConfig: SqlParser.Config = PostgreSqlParserConfig
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] =
    jdbcPlanner(dataSource, schemaName, jdbcCatalog, jdbcSchema, parserConfig,
      schema => forSchemaWithDistributedCountGrouping(schema, maximumGroupKeys, parserConfig))

  private def jdbcPlanner(
      dataSource: DataSource,
      schemaName: String,
      jdbcCatalog: Option[String],
      jdbcSchema: Option[String],
      parserConfig: SqlParser.Config,
      createPlanner: SchemaPlus => Either[CalciteSqlPlanningError, CalciteSqlPlanner]
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] = {
    validateJdbcConfiguration(
      dataSource,
      schemaName,
      jdbcCatalog,
      jdbcSchema,
      parserConfig
    ).flatMap { _ =>
      attempt(CalciteSqlPlanningStage.Configuration) {
        val root = Frameworks.createRootSchema(true)
        val physicalSchemaName = jdbcSchema.getOrElse(schemaName)
        val planningDataSource =
          CalciteExactJdbcMetadataDataSource.wrap(dataSource)
        val schema = JdbcSchema.create(
          root,
          schemaName,
          planningDataSource,
          jdbcCatalog.orNull,
          physicalSchemaName
        )
        root.add(schemaName, schema)
      }.flatMap(createPlanner)
    }
  }

  private[yannakakis] def forJdbcDataSourceWithLocalPredicates(
      dataSource: DataSource,
      schemaName: String,
      maximumGroupKeys: Int = 2,
      jdbcCatalog: Option[String] = None,
      jdbcSchema: Option[String] = None,
      parserConfig: SqlParser.Config = PostgreSqlQualifiedNameParserConfig
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] =
    jdbcPlanner(dataSource, schemaName, jdbcCatalog, jdbcSchema, parserConfig,
      schema => forSchemaWithLocalPredicates(schema, maximumGroupKeys, parserConfig))

  /** Additive PostgreSQL text-key capability. Every call plans with a fresh
    * JDBC schema and reads exact base-column type/collation evidence. Legacy
    * factories remain unchanged. The caller still owns the DataSource.
    */
  def forJdbcDataSourceWithTextKeys(
      dataSource: DataSource,
      schemaName: String,
      maximumGroupKeys: Int = 2
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] = {
    def fresh() = forJdbcDataSourceWithLocalPredicates(dataSource, schemaName, maximumGroupKeys)
    fresh().map { _ =>
      new CalciteSqlPlanner with CalciteSqlPlannerWithSourceEvidence {
        override def plan(sql: String): Either[CalciteSqlPlanningError, RelRoot] =
          planWithSourceEvidence(sql).map(_.root)

        override def planWithSourceEvidence(sql: String)
            : Either[CalciteSqlPlanningError, CalciteSqlPlannedQuery] =
          fresh().flatMap(p => CalciteSqlPlanner.planWithSourceEvidence(p, sql)).flatMap { planned =>
            attempt(CalciteSqlPlanningStage.Boundary) {
              require(planned.textKeySourceSafe, "explicit source collations are outside text-key admission")
              planned.copy(textKeyCatalog = Some(PostgreSqlTextKeyCatalog.capture(
                planned.root, dataSource, schemaName)))
            }
          }
      }
    }
  }

  /**
    * Additive JDBC constructor for an already verified C03-G0 binding. It
    * performs no catalog query and deliberately requires the logical Calcite
    * schema name and physical JDBC schema to equal the bound physical schema,
    * so later occurrence lineage cannot be rebound through an alias.
    */
  private[yannakakis] def forJdbcDataSourceWithC03G0V2(
      dataSource: DataSource,
      schemaName: String,
      jdbcCatalog: Option[String],
      jdbcSchema: Option[String],
      parserConfig: SqlParser.Config,
      catalogBinding: C03G0CatalogBinding
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] = {
    validateJdbcConfiguration(
      dataSource,
      schemaName,
      jdbcCatalog,
      jdbcSchema,
      parserConfig
    ).flatMap { _ =>
      if (catalogBinding eq null) {
        Left(configurationError("C03-G0 catalog binding must be provided"))
      } else {
        val physicalSchemaName = jdbcSchema.getOrElse(schemaName)
        if (
          schemaName != catalogBinding.physicalSchemaName ||
          physicalSchemaName != catalogBinding.physicalSchemaName
        ) {
          Left(
            configurationError(
              "logical and physical JDBC schemas must match the verified C03-G0 schema"
            )
          )
        } else {
          attempt(CalciteSqlPlanningStage.Configuration) {
            val root = Frameworks.createRootSchema(true)
            val planningDataSource =
              CalciteExactJdbcMetadataDataSource.wrap(dataSource)
            val schema = JdbcSchema.create(
              root,
              schemaName,
              planningDataSource,
              jdbcCatalog.orNull,
              physicalSchemaName
            )
            root.add(schemaName, schema)
          }.flatMap(schema =>
            forSchemaWithC03G0V2(schema, catalogBinding, parserConfig)
          )
        }
      }
    }
  }

  /**
    * Convenience factory for a PostgreSQL JDBC catalog.
    *
    * The password is passed directly to Calcite's data-source factory. It is
    * retained by that data source as required for later connections, but is
    * never retained separately by this planner or copied into public errors.
    */
  def forPostgreSqlJdbc(
      jdbcUrl: String,
      user: String,
      password: String,
      schemaName: String,
      parserConfig: SqlParser.Config = PostgreSqlParserConfig
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] = {
    val invalid = Vector(
      requiredText("JDBC URL", jdbcUrl),
      requiredText("JDBC user", user),
      requiredValue("JDBC password", password),
      requiredText("schema name", schemaName)
    ).flatten
    if (invalid.nonEmpty) Left(configurationError(invalid.mkString("; ")))
    else {
      attempt(CalciteSqlPlanningStage.Configuration) {
        JdbcSchema.dataSource(
          jdbcUrl,
          "org.postgresql.Driver",
          user,
          password
        )
      }.flatMap { dataSource =>
        forJdbcDataSource(
          dataSource = dataSource,
          schemaName = schemaName,
          jdbcSchema = Some(schemaName),
          parserConfig = parserConfig
        )
      }
    }
  }

  /** Small adapter useful for alternate catalogs and deterministic tests. */
  def fromFunction(
      function: String => Either[CalciteSqlPlanningError, RelRoot]
  ): Either[CalciteSqlPlanningError, CalciteSqlPlanner] = {
    if (function eq null) {
      Left(configurationError("planning function must be provided"))
    } else {
      Right(new CalciteSqlPlanner {
        override def plan(
            sql: String
        ): Either[CalciteSqlPlanningError, RelRoot] = function(sql)
      })
    }
  }

  private[yannakakis] def validateJdbcConfiguration(
      dataSource: DataSource,
      schemaName: String,
      jdbcCatalog: Option[String],
      jdbcSchema: Option[String],
      parserConfig: SqlParser.Config
  ): Either[CalciteSqlPlanningError, Unit] = {
    val invalid = Vector.newBuilder[String]
    if (dataSource eq null) invalid += "data source must be provided"
    requiredText("schema name", schemaName).foreach(invalid += _)
    if (jdbcCatalog eq null) invalid += "JDBC catalog option must be provided"
    else optionalText("JDBC catalog", jdbcCatalog).foreach(invalid += _)
    if (jdbcSchema eq null) invalid += "JDBC schema option must be provided"
    else optionalText("JDBC schema", jdbcSchema).foreach(invalid += _)
    if (parserConfig eq null) invalid += "parser config must be provided"

    val errors = invalid.result()
    if (errors.isEmpty) Right(())
    else Left(configurationError(errors.mkString("; ")))
  }

  private def requiredText(label: String, value: String): Option[String] =
    if (value == null || value.trim.isEmpty) Some(s"$label must be provided")
    else None

  private def requiredValue(label: String, value: String): Option[String] =
    if (value == null) Some(s"$label must be provided") else None

  private def optionalText(
      label: String,
      value: Option[String]
  ): Option[String] =
    value.collect {
      case supplied if supplied == null || supplied.trim.isEmpty =>
        s"$label must not be blank when supplied"
    }

  private def configurationError(message: String): CalciteSqlPlanningError =
    CalciteSqlPlanningError(
      CalciteSqlPlanningStage.Configuration,
      message
    )

  private def attempt[A](
      stage: CalciteSqlPlanningStage
  )(operation: => A): Either[CalciteSqlPlanningError, A] =
    try Right(operation)
    catch {
      case NonFatal(problem) => Left(fromThrowable(stage, problem))
    }

  private def fromThrowable(
      stage: CalciteSqlPlanningStage,
      problem: Throwable
  ): CalciteSqlPlanningError =
    CalciteSqlPlanningError(
      stage,
      stageMessage(stage),
      Some(problem.getClass.getName)
    )

  // Throwable messages are deliberately excluded: JDBC/catalog exceptions
  // may embed URLs, credentials, SQL literals, or driver properties.
  private def stageMessage(stage: CalciteSqlPlanningStage): String =
    stage match {
      case CalciteSqlPlanningStage.Configuration =>
        "Calcite configuration failed"
      case CalciteSqlPlanningStage.PlannerCreation =>
        "Calcite planner creation failed"
      case CalciteSqlPlanningStage.Parsing =>
        "Calcite SQL parsing failed"
      case CalciteSqlPlanningStage.Validation =>
        "Calcite SQL validation failed"
      case CalciteSqlPlanningStage.RelationalConversion =>
        "Calcite relational conversion failed"
      case CalciteSqlPlanningStage.PlannerClose =>
        "Calcite planner close failed"
      case CalciteSqlPlanningStage.Boundary =>
        "Calcite planning boundary failed"
    }

  private final class FrameworkCalciteSqlPlanner(
      config: FrameworkConfig,
      aggregateLanguageProfile: CalciteAggregateLanguageProfile,
      maximumDistributedGroupKeys: Int = 2,
      allowLocalPredicates: Boolean = false
  )
      extends CalciteSqlPlanner
      with CalciteSqlPlannerWithSourceEvidence {
    require(config != null, "framework config must not be null")
    require(
      aggregateLanguageProfile != null,
      "aggregate language profile must not be null"
    )

    override def plan(
        sql: String
    ): Either[CalciteSqlPlanningError, RelRoot] =
      planWithSourceEvidence(sql).map(_.root)

    override def planWithSourceEvidence(
        sql: String
    ): Either[CalciteSqlPlanningError, CalciteSqlPlannedQuery] = {
      if (sql == null) {
        return Left(
          CalciteSqlPlanningError(
            CalciteSqlPlanningStage.Parsing,
            "SQL must be provided"
          )
        )
      }

      attempt(CalciteSqlPlanningStage.PlannerCreation) {
        Frameworks.getPlanner(config)
      }.flatMap(planAndClose(_, sql))
    }

    private def planAndClose(
        planner: Planner,
        sql: String
    ): Either[CalciteSqlPlanningError, CalciteSqlPlannedQuery] = {
      val planned = attempt(CalciteSqlPlanningStage.Parsing) {
        planner.parse(sql)
      }.flatMap { parsed =>
        // Raw source certification is a rewriter boundary, not SQL parsing.
        // A defensive recognizer failure must never relabel valid SQL as a
        // parse error merely because the closed rewrite grammar rejects it.
        attempt(CalciteSqlPlanningStage.Boundary) {
          (parsed, RawAstAggregateEvidence.inspectRaw(parsed, sql, maximumDistributedGroupKeys),
            if (allowLocalPredicates) CalciteLocalPredicates.inspect(parsed) else None,
            PostgreSqlTextKeyCatalog.sourceSafe(parsed))
        }
      }.flatMap { case (parsed, rawInspection, localPredicates, textKeySourceSafe) =>
        attempt(CalciteSqlPlanningStage.Validation) {
          val validated = planner.validate(parsed)
          (
            validated,
            RawAstAggregateEvidence.certify(
              rawInspection,
              validated,
              aggregateLanguageProfile
            ),
            RawAstAggregateEvidence.outputLabelEvidence(
              rawInspection,
              validated
            ),
            localPredicates,
            textKeySourceSafe
          )
        }
      }.flatMap { validated =>
        attempt(CalciteSqlPlanningStage.RelationalConversion) {
          CalciteSqlPlannedQuery(
            root = planner.rel(validated._1),
            sourceEvidence = validated._2,
            aggregateLanguageProfile = aggregateLanguageProfile,
            outputLabelEvidence = validated._3,
            localPredicateCertificate = validated._4,
            textKeySourceSafe = validated._5
          )
        }
      }

      val closed = attempt(CalciteSqlPlanningStage.PlannerClose) {
        planner.close()
      }
      planned match {
        case failure @ Left(_) => failure
        case success @ Right(_) => closed.fold(Left(_), _ => success)
      }
    }
  }

  /** Complete raw-AST aggregate inventory and bounded exact recognizers. */
  private object RawAstAggregateEvidence {
    final case class SourcePosition(
        line: Int,
        column: Int,
        endLine: Int,
        endColumn: Int
    )

    sealed trait RawOutputLabelItem extends Product with Serializable {
      def position: SourcePosition
    }

    final case class FixedOutputLabel(
        position: SourcePosition,
        name: String
    ) extends RawOutputLabelItem

    final case class ExpandedStarOutputLabel(position: SourcePosition)
        extends RawOutputLabelItem

    final case class RawOutputLabelPlan(
        items: Vector[RawOutputLabelItem]
    )

    final case class AstInventory(
        aggregateCalls: Vector[SqlCall],
        hasWindowCall: Boolean,
        hasGroupingOrDistinct: Boolean
    )

    final case class C03G0RawCandidate private (
        sourceSqlSha256: String,
        groupIdentifier: Vector[String],
        measureIdentifier: Vector[String],
        outputAliases: Vector[String]
    )

    final case class RawInspection private (
        inventory: AstInventory,
        queryBlockCount: Int,
        outputLabelPlan: Either[String, RawOutputLabelPlan],
        exactLocalIntegralPredicates: Option[
          LocalIntegralPredicateSourceCertificate
        ],
        exactTopLevelAliasMode: Option[GlobalCountStarAliasMode],
        exactTopLevelGroupedCountAliasMode: Option[GlobalCountStarAliasMode],
        exactTopLevelDistributedGroupedCountAliasMode: Option[
          (GlobalCountStarAliasMode, Int)
        ],
        exactTopLevelGuardedExtremum: Option[
          (
              GlobalGuardedExtremumFunction,
              GlobalGuardedExtremumAliasMode
          )
        ],
        exactC03G0: Option[C03G0RawCandidate],
        hasDirectTopLevelCountResult: Boolean,
        hasDirectTopLevelGuardedExtremumResult: Boolean
    )

    def inspectRaw(parsed: SqlNode, sql: String, maximumDistributedGroupKeys: Int): RawInspection = {
      require(sql != null, "raw SQL must not be null")
      val inventory = collect(parsed, trustResolvedOperators = false)
      val queryBlocks = queryBlockCount(parsed)
      val exactAlias = parsed match {
        case select: SqlSelect if exactTopLevelSelectShape(select) =>
          directCountItem(select).collect {
            case (count, aliasMode)
                if isRawCountStar(count) && count.getFunctionQuantifier == null =>
              aliasMode
          }
        case _ => None
      }
      val exactGroupedCountAlias = parsed match {
        case select: SqlSelect if exactTopLevelGroupedCountShape(select) =>
          directCountItemAt(select, 1).collect {
            case (count, aliasMode)
                if isRawCountStar(count) && count.getFunctionQuantifier == null =>
              aliasMode
          }
        case _ => None
      }
      val exactDistributedGroupedCountAlias = parsed match {
        case select: SqlSelect
            if exactTopLevelDistributedGroupedCountShape(select, maximumDistributedGroupKeys) =>
          directCountItemAt(select, select.getGroup.size).collect {
            case (count, aliasMode)
                if isRawCountStar(count) && count.getFunctionQuantifier == null =>
              aliasMode -> select.getGroup.size
          }
        case _ => None
      }
      val exactExtremum = parsed match {
        case select: SqlSelect if exactTopLevelSelectShape(select) =>
          directGuardedExtremumItem(select).map {
            case (_, function, aliasMode) => function -> aliasMode
          }
        case _ => None
      }
      RawInspection(
        inventory = inventory,
        queryBlockCount = queryBlocks,
        outputLabelPlan = rawOutputLabelPlan(parsed, queryBlocks),
        exactLocalIntegralPredicates =
          inspectLocalIntegralPredicates(parsed),
        exactTopLevelAliasMode = exactAlias,
        exactTopLevelGroupedCountAliasMode = exactGroupedCountAlias,
        exactTopLevelDistributedGroupedCountAliasMode =
          exactDistributedGroupedCountAlias,
        exactTopLevelGuardedExtremum = exactExtremum,
        exactC03G0 = exactC03G0RawCandidate(parsed, sha256Utf8(sql)),
        hasDirectTopLevelCountResult = directTopLevelCountItem(parsed).nonEmpty,
        hasDirectTopLevelGuardedExtremumResult =
          directTopLevelGuardedExtremumItem(parsed).nonEmpty
      )
    }

    /** Resolves raw PostgreSQL label rules against Calcite's validated
      * positional star expansion.  The raw plan was captured before
      * validation mutated the parsed SELECT.
      */
    def outputLabelEvidence(
        rawInspection: RawInspection,
        validated: SqlNode
    ): CalciteSqlOutputLabelEvidence =
      rawInspection.outputLabelPlan
        .flatMap(plan => resolveOutputLabels(plan, validated)) match {
        case Right(names) => CalciteSqlOutputLabelEvidence.Certified(names)
        case Left(reason) => CalciteSqlOutputLabelEvidence.Rejected(reason)
      }

    private def rawOutputLabelPlan(
        parsed: SqlNode,
        queryBlocks: Int
    ): Either[String, RawOutputLabelPlan] =
      if (queryBlocks != 1)
        Left("nested query blocks are outside the certified output-label grammar")
      else parsed match {
      case select: SqlSelect =>
        val items = select.getSelectList.getList.asScala.toVector.map { item =>
          val position = sourcePosition(item)
          item match {
            case identifier: SqlIdentifier if identifier.isStar =>
              Right(ExpandedStarOutputLabel(position): RawOutputLabelItem)
            case call: SqlCall if call.getKind == SqlKind.AS =>
              call.getOperandList.asScala.toVector match {
                case Vector(_, alias: SqlIdentifier) if alias.isSimple =>
                  Right(FixedOutputLabel(position, alias.getSimple): RawOutputLabelItem)
                case _ =>
                  Left("top-level output alias could not be source-certified")
              }
            case identifier: SqlIdentifier =>
              identifier.names.asScala.lastOption match {
                case Some(name) if name.nonEmpty =>
                  Right(FixedOutputLabel(position, name): RawOutputLabelItem)
                case _ => Left("top-level direct-column label was empty")
              }
            case _ =>
              Right(FixedOutputLabel(position, "?column?"): RawOutputLabelItem)
          }
        }
        val problems = items.collect { case Left(problem) => problem }
        if (problems.nonEmpty) Left(problems.head)
        else Right(RawOutputLabelPlan(items.collect { case Right(item) => item }))
      case _ =>
        Left("top-level query is not a directly source-certified SELECT")
    }

    private def queryBlockCount(root: SqlNode): Int = {
      def count(node: SqlNode): Int = {
        if (node == null) 0
        else
          node match {
            case list: SqlNodeList => list.asScala.iterator.map(count).sum
            case call: SqlCall =>
              (if (call.isInstanceOf[SqlSelect]) 1 else 0) +
                call.getOperandList.asScala.iterator.map(count).sum
            case _ => 0
          }
      }
      count(root)
    }

    private def resolveOutputLabels(
        plan: RawOutputLabelPlan,
        validated: SqlNode
    ): Either[String, Vector[String]] = validated match {
      case select: SqlSelect =>
        val rawByPosition = plan.items.groupBy(_.position)
        if (rawByPosition.valuesIterator.exists(_.size != 1)) {
          Left("raw output items did not have unique source positions")
        } else {
          val names = select.getSelectList.getList.asScala.toVector.map { item =>
            rawByPosition.getOrElse(sourcePosition(item), Vector.empty) match {
              case Vector(FixedOutputLabel(_, name)) => Right(name)
              case Vector(_: ExpandedStarOutputLabel) =>
                expandedStarSourceName(item).toRight(
                  "validated star expansion was not one direct source column"
                )
              case _ =>
                Left("validated output position did not match one raw select item")
            }
          }
          names.collectFirst { case Left(problem) => problem } match {
            case Some(problem) => Left(problem)
            case None          => Right(names.collect { case Right(name) => name })
          }
        }
      case _ =>
        Left("validated top-level query is not a directly source-certified SELECT")
    }

    private def expandedStarSourceName(item: SqlNode): Option[String] = {
      val source = item match {
        case call: SqlCall if call.getKind == SqlKind.AS =>
          call.getOperandList.asScala.headOption.orNull
        case other => other
      }
      source match {
        case identifier: SqlIdentifier if !identifier.isStar =>
          identifier.names.asScala.lastOption.filter(_.nonEmpty)
        case _ => None
      }
    }

    private def sourcePosition(node: SqlNode): SourcePosition = {
      val position = node.getParserPosition
      SourcePosition(
        position.getLineNum,
        position.getColumnNum,
        position.getEndLineNum,
        position.getEndColumnNum
      )
    }

    private def exactC03G0RawCandidate(
        parsed: SqlNode,
        sourceSqlSha256: String
    ): Option[C03G0RawCandidate] = parsed match {
      case select: SqlSelect
          if sourceSqlSha256 == C03G0SourceCertificate.SourceSqlSha256 &&
            exactTopLevelC03G0Shape(select) =>
        Some(
          C03G0RawCandidate(
            sourceSqlSha256,
            C03G0SourceCertificate.GroupIdentifier,
            C03G0SourceCertificate.MeasureIdentifier,
            C03G0SourceCertificate.OutputAliases
          )
        )
      case _ => None
    }

    private def sha256Utf8(value: String): String =
      MessageDigest
        .getInstance("SHA-256")
        .digest(value.getBytes(StandardCharsets.UTF_8))
        .iterator
        .map(byte => f"${byte & 0xff}%02x")
        .mkString

    def certify(
        rawInspection: RawInspection,
        validated: SqlNode,
        aggregateLanguageProfile: CalciteAggregateLanguageProfile
    ): CalciteSqlSourceEvidence = {
      require(
        aggregateLanguageProfile != null,
        "aggregate language profile must not be null"
      )
      val rawInventory = rawInspection.inventory
      val validatedInventory = collect(validated, trustResolvedOperators = true)
      val hasAggregateSemantics =
        validatedInventory.aggregateCalls.nonEmpty ||
          rawInventory.hasWindowCall ||
          validatedInventory.hasWindowCall ||
          rawInventory.hasGroupingOrDistinct ||
          validatedInventory.hasGroupingOrDistinct

      if (!hasAggregateSemantics) {
        val validatedLocalPredicates =
          inspectLocalIntegralPredicates(validated)
        rawInspection.exactLocalIntegralPredicates match {
          case Some(certificate)
              if validatedLocalPredicates.exists(validatedCertificate =>
                concordantLocalIntegralPredicateCertificates(
                  certificate,
                  validatedCertificate
                )
              ) =>
            CalciteSqlSourceEvidence.CertifiedNonAggregateLocalIntegralPredicates(
              certificate
            )
          case _ => CalciteSqlSourceEvidence.NonAggregate
        }
      }
      else if (rawInspection.queryBlockCount != 1) {
        // The paper fragment excludes subqueries. Calcite may flatten a
        // derived-table aggregate into an otherwise admissible relational
        // shape, so enforce the raw single-query-block premise before any
        // aggregate certificate is issued.
        CalciteSqlSourceEvidence.RejectedAggregate(
          rejectionReason(rawInspection, validatedInventory)
        )
      } else {
        exactC03G0(
          rawInspection,
          validatedInventory,
          aggregateLanguageProfile
        ) match {
          case Some(certificate) =>
            CalciteSqlSourceEvidence.CertifiedC03G0(certificate)
          case None =>
            exactDistributedGroupedCountStar(
              rawInspection,
              validatedInventory
            ) match {
              case Some(certificate) =>
                CalciteSqlSourceEvidence.CertifiedDistributedGroupedCountStar(
                  certificate
                )
              case None =>
                exactGroupedCountStar(rawInspection, validatedInventory) match {
                  case Some(certificate) =>
                    CalciteSqlSourceEvidence.CertifiedGroupedCountStar(certificate)
                  case None =>
                    exactGlobalCountStar(rawInspection, validatedInventory) match {
                      case Some(certificate) =>
                        CalciteSqlSourceEvidence.CertifiedGlobalCountStar(certificate)
                      case None =>
                        exactGlobalGuardedExtremum(
                          rawInspection,
                          validatedInventory
                        ) match {
                          case Some(certificate) =>
                            CalciteSqlSourceEvidence.CertifiedGlobalGuardedExtremum(
                              certificate
                            )
                          case None =>
                            CalciteSqlSourceEvidence.RejectedAggregate(
                              rejectionReason(rawInspection, validatedInventory)
                            )
                        }
                    }
                }
            }
        }
      }
    }

    private def concordantLocalIntegralPredicateCertificates(
        raw: LocalIntegralPredicateSourceCertificate,
        validated: LocalIntegralPredicateSourceCertificate
    ): Boolean =
      raw.totalPredicateCount == validated.totalPredicateCount &&
        raw.predicates == validated.predicates &&
        raw.occurrences.size == validated.occurrences.size &&
        raw.occurrences.zip(validated.occurrences).forall {
          case (rawOccurrence, validatedOccurrence) =>
            rawOccurrence.ordinal == validatedOccurrence.ordinal &&
              rawOccurrence.alias == validatedOccurrence.alias &&
              validatedOccurrence.qualifiedName.endsWith(
                rawOccurrence.qualifiedName
              )
        }

    /**
      * Certifies only top-level SELECTs whose FROM tree can bind each direct
      * two-part `alias.column = exact-integer-literal` predicate to one
      * left-to-right table occurrence. Predicate ordinals use the same
      * postorder join traversal followed by WHERE order as the relational
      * analyzer. Any cast, expression, subquery, ambiguous alias, signed
      * prefix, approximate literal, or wider source grammar yields no binding.
      */
    private def inspectLocalIntegralPredicates(
        root: SqlNode
    ): Option[LocalIntegralPredicateSourceCertificate] = root match {
      case select: SqlSelect if select.getFrom != null =>
        val occurrences = mutable.ArrayBuffer.empty[
          LocalIntegralPredicateSourceOccurrence
        ]
        val occurrenceByAlias = mutable.LinkedHashMap.empty[String, Int]
        val predicates = mutable.ArrayBuffer.empty[
          LocalIntegralPredicateSourceBinding
        ]
        var predicateOrdinal = 0
        var valid = true

        def registerRelation(node: SqlNode): Unit = node match {
          case aliasCall: SqlCall
              if aliasCall.getKind == SqlKind.AS &&
                aliasCall.operandCount() == 2 =>
            (
              aliasCall.operand[SqlNode](0),
              aliasCall.operand[SqlNode](1)
            ) match {
              case (relation: SqlIdentifier, alias: SqlIdentifier)
                  if !relation.isStar && relation.names != null &&
                    !relation.names.isEmpty && alias.isSimple =>
                registerOccurrence(
                  relation.names.asScala.toVector,
                  alias.getSimple
                )
              case _ => valid = false
            }
          case relation: SqlIdentifier
              if !relation.isStar && relation.names != null &&
                !relation.names.isEmpty =>
            val names = relation.names.asScala.toVector
            registerOccurrence(names, names.last)
          case _ => valid = false
        }

        def registerOccurrence(
            qualifiedName: Vector[String],
            alias: String
        ): Unit = {
          if (
            alias == null || alias.isEmpty || occurrenceByAlias.contains(alias)
          ) valid = false
          else {
            val ordinal = occurrences.size
            occurrences += LocalIntegralPredicateSourceOccurrence(
              ordinal,
              qualifiedName,
              alias
            )
            occurrenceByAlias.update(alias, ordinal)
          }
        }

        def conjuncts(node: SqlNode): Vector[SqlNode] = node match {
          case call: SqlCall
              if call.getKind == SqlKind.AND && call.operandCount() == 2 =>
            conjuncts(call.operand[SqlNode](0)) ++
              conjuncts(call.operand[SqlNode](1))
          case other => Vector(other)
        }

        def inspectPredicate(node: SqlNode): Unit = {
          directLocalIntegralBinding(
            node,
            predicateOrdinal,
            occurrenceByAlias.toMap
          ).foreach(predicates += _)
          predicateOrdinal += 1
        }

        def inspectCondition(node: SqlNode): Unit =
          if (node != null) conjuncts(node).foreach(inspectPredicate)

        def visitFrom(node: SqlNode): Unit = node match {
          case join: SqlJoin =>
            visitFrom(join.getLeft)
            visitFrom(join.getRight)
            inspectCondition(join.getCondition)
          case relation => registerRelation(relation)
        }

        visitFrom(select.getFrom)
        inspectCondition(select.getWhere)

        if (valid && predicateOrdinal > 0 && predicates.nonEmpty) {
          Some(
            LocalIntegralPredicateSourceCertificate(
              predicateOrdinal,
              occurrences.toVector,
              predicates.toVector
            )
          )
        } else None
      case _ => None
    }

    private def directLocalIntegralBinding(
        node: SqlNode,
        predicateOrdinal: Int,
        occurrenceByAlias: Map[String, Int]
    ): Option[LocalIntegralPredicateSourceBinding] = node match {
      case equality: SqlCall
          if equality.getKind == SqlKind.EQUALS &&
            equality.operandCount() == 2 =>
        val left = equality.operand[SqlNode](0)
        val right = equality.operand[SqlNode](1)
        directLocalIntegralOperands(
          left,
          right,
          literalOnLeft = false,
          predicateOrdinal,
          occurrenceByAlias
        ).orElse(
          directLocalIntegralOperands(
            right,
            left,
            literalOnLeft = true,
            predicateOrdinal,
            occurrenceByAlias
          )
        )
      case _ => None
    }

    private def directLocalIntegralOperands(
        columnNode: SqlNode,
        literalNode: SqlNode,
        literalOnLeft: Boolean,
        predicateOrdinal: Int,
        occurrenceByAlias: Map[String, Int]
    ): Option[LocalIntegralPredicateSourceBinding] =
      (columnNode, literalNode) match {
        case (column: SqlIdentifier, literal: SqlNumericLiteral)
            if !column.isStar && column.names != null &&
              column.names.size == 2 && literal.isExact &&
              literal.isInteger && literal.getPrec != null &&
              literal.getPrec > 0 && literal.getScale != null &&
              literal.getScale == 0 =>
          val names = column.names.asScala.toVector
          val alias = names.head
          for {
            occurrenceOrdinal <- occurrenceByAlias.get(alias)
            value <- Try(
              literal.bigDecimalValue.toBigIntegerExact.longValueExact
            ).toOption
            if value >= 0
            if literal.getPrec == value.toString.length
          } yield LocalIntegralPredicateSourceBinding(
            predicateOrdinal = predicateOrdinal,
            occurrenceOrdinal = occurrenceOrdinal,
            alias = alias,
            columnName = names(1),
            literalValue = value,
            literalPrecision = literal.getPrec,
            literalOnLeft = literalOnLeft
          )
        case _ => None
      }

    private def collect(
        root: SqlNode,
        trustResolvedOperators: Boolean
    ): AstInventory = {
      val aggregateCalls = mutable.ArrayBuffer.empty[SqlCall]
      var hasWindowCall = false
      var hasGroupingOrDistinct = false

      def visit(node: SqlNode): Unit = {
        if (node == null) return
        node match {
          case list: SqlNodeList => list.asScala.foreach(visit)
          case call: SqlCall =>
            if (trustResolvedOperators && call.getOperator.isAggregator)
              aggregateCalls += call
            if (
              call.getKind == SqlKind.OVER ||
              call.getKind == SqlKind.WINDOW ||
              call.getOperator.requiresOver()
            ) hasWindowCall = true
            call match {
              case select: SqlSelect =>
                if (
                  select.isDistinct ||
                  select.getGroup != null ||
                  select.getHaving != null
                ) hasGroupingOrDistinct = true
              case _ => ()
            }
            call.getOperandList.asScala.foreach(visit)
          case _ => ()
        }
      }

      visit(root)
      AstInventory(
        aggregateCalls.toVector,
        hasWindowCall,
        hasGroupingOrDistinct
      )
    }

    private def exactGlobalCountStar(
        rawInspection: RawInspection,
        validatedInventory: AstInventory
    ): Option[GlobalCountStarSourceCertificate] =
      rawInspection.exactTopLevelAliasMode.collect {
        case aliasMode
            if validatedInventory.aggregateCalls.size == 1 &&
              !rawInspection.inventory.hasWindowCall &&
              !validatedInventory.hasWindowCall &&
              !rawInspection.inventory.hasGroupingOrDistinct &&
              !validatedInventory.hasGroupingOrDistinct &&
              isValidatedCountStar(validatedInventory.aggregateCalls.head) =>
          GlobalCountStarSourceCertificate(aliasMode)
      }

    private def exactC03G0(
        rawInspection: RawInspection,
        validatedInventory: AstInventory,
        aggregateLanguageProfile: CalciteAggregateLanguageProfile
    ): Option[C03G0SourceCertificate] = aggregateLanguageProfile match {
      case CalciteAggregateLanguageProfile.C03G0V2(catalogBinding) =>
        rawInspection.exactC03G0.collect {
          case candidate
              if candidate.sourceSqlSha256 ==
                C03G0SourceCertificate.SourceSqlSha256 &&
                candidate.groupIdentifier ==
                C03G0SourceCertificate.GroupIdentifier &&
                candidate.measureIdentifier ==
                  C03G0SourceCertificate.MeasureIdentifier &&
                candidate.outputAliases ==
                  C03G0SourceCertificate.OutputAliases &&
                !rawInspection.inventory.hasWindowCall &&
                !validatedInventory.hasWindowCall &&
                rawInspection.inventory.hasGroupingOrDistinct &&
                validatedInventory.hasGroupingOrDistinct &&
                exactValidatedC03G0Calls(
                  validatedInventory.aggregateCalls,
                  catalogBinding
                ) =>
            C03G0SourceCertificate.exact(catalogBinding)
        }
      case CalciteAggregateLanguageProfile.LegacyV1 => None
    }

    private def exactValidatedC03G0Calls(
        calls: Vector[SqlCall],
        catalogBinding: C03G0CatalogBinding
    ): Boolean = calls match {
      case Vector(countStar, countGroup, minimum, maximum) =>
        val countGroupArgument = validatedDirectArgument(countGroup)
        val minimumArgument = validatedDirectArgument(minimum)
        val maximumArgument = validatedDirectArgument(maximum)
        isValidatedCountStar(countStar) &&
        isValidatedCountColumn(countGroup) &&
        isValidatedGuardedExtremum(
          minimum,
          GlobalGuardedExtremumFunction.Min
        ) &&
        isValidatedGuardedExtremum(
          maximum,
          GlobalGuardedExtremumFunction.Max
        ) &&
        countGroupArgument.exists(
          _.lastOption.contains(catalogBinding.groupColumnName)
        ) &&
        minimumArgument.exists(
          _.lastOption.contains(catalogBinding.measureColumnName)
        ) &&
        minimumArgument == maximumArgument
      case _ => false
    }

    private def validatedDirectArgument(call: SqlCall): Option[Vector[String]] =
      if (call != null && call.operandCount() == 1) {
        call.operand[SqlNode](0) match {
          case identifier: SqlIdentifier
              if !identifier.isStar && identifier.names != null &&
                !identifier.names.isEmpty =>
            Some(identifier.names.asScala.toVector)
          case _ => None
        }
      } else None

    private def isValidatedCountColumn(call: SqlCall): Boolean =
      call.getKind == SqlKind.COUNT &&
        call.getOperator.isAggregator &&
        !call.isCountStar &&
        call.getFunctionQuantifier == null &&
        validatedDirectArgument(call).nonEmpty

    private def exactGroupedCountStar(
        rawInspection: RawInspection,
        validatedInventory: AstInventory
    ): Option[GroupedCountStarSourceCertificate] =
      rawInspection.exactTopLevelGroupedCountAliasMode.collect {
        case aliasMode
            if validatedInventory.aggregateCalls.size == 1 &&
              !rawInspection.inventory.hasWindowCall &&
              !validatedInventory.hasWindowCall &&
              rawInspection.inventory.hasGroupingOrDistinct &&
              validatedInventory.hasGroupingOrDistinct &&
              isValidatedCountStar(validatedInventory.aggregateCalls.head) =>
          GroupedCountStarSourceCertificate(aliasMode)
      }

    private def exactDistributedGroupedCountStar(
        rawInspection: RawInspection,
        validatedInventory: AstInventory
    ): Option[DistributedGroupedCountStarSourceCertificate] =
      rawInspection.exactTopLevelDistributedGroupedCountAliasMode.collect {
        case (aliasMode, keyCount)
            if validatedInventory.aggregateCalls.size == 1 &&
              !rawInspection.inventory.hasWindowCall &&
              !validatedInventory.hasWindowCall &&
              rawInspection.inventory.hasGroupingOrDistinct &&
              validatedInventory.hasGroupingOrDistinct &&
              isValidatedCountStar(validatedInventory.aggregateCalls.head) =>
          DistributedGroupedCountStarSourceCertificate(aliasMode, keyCount)
      }

    private def isValidatedCountStar(call: SqlCall): Boolean =
      call.getKind == SqlKind.COUNT &&
        call.getOperator.isAggregator &&
        call.isCountStar &&
        call.getFunctionQuantifier == null

    private def exactGlobalGuardedExtremum(
        rawInspection: RawInspection,
        validatedInventory: AstInventory
    ): Option[GlobalGuardedExtremumSourceCertificate] =
      rawInspection.exactTopLevelGuardedExtremum.collect {
        case (function, aliasMode)
            if validatedInventory.aggregateCalls.size == 1 &&
              !rawInspection.inventory.hasWindowCall &&
              !validatedInventory.hasWindowCall &&
              !rawInspection.inventory.hasGroupingOrDistinct &&
              !validatedInventory.hasGroupingOrDistinct &&
              isValidatedGuardedExtremum(
                validatedInventory.aggregateCalls.head,
                function
              ) =>
          GlobalGuardedExtremumSourceCertificate(function, aliasMode)
      }

    private def isValidatedGuardedExtremum(
        call: SqlCall,
        function: GlobalGuardedExtremumFunction
    ): Boolean = {
      val expectedKind = function match {
        case GlobalGuardedExtremumFunction.Min => SqlKind.MIN
        case GlobalGuardedExtremumFunction.Max => SqlKind.MAX
      }
      call.getKind == expectedKind &&
      call.getOperator.isAggregator &&
      call.getOperator.getName.equalsIgnoreCase(function.sqlName) &&
      call.getFunctionQuantifier == null &&
      call.operandCount() == 1
    }

    private def exactTopLevelSelectShape(select: SqlSelect): Boolean =
      select.getSelectList != null &&
        select.getSelectList.size == 1 &&
        select.getFrom != null &&
        !select.isDistinct &&
        select.getGroup == null &&
        select.getHaving == null &&
        (select.getWindowList == null || select.getWindowList.isEmpty) &&
        select.getQualify == null &&
        (select.getOrderList == null || select.getOrderList.isEmpty) &&
        select.getOffset == null &&
        select.getFetch == null &&
        !select.hasHints

    private def exactTopLevelC03G0Shape(select: SqlSelect): Boolean = {
      val group = select.getGroup
      val aliases = C03G0SourceCertificate.OutputAliases
      val groupIdentifier = C03G0SourceCertificate.GroupIdentifier
      val measureIdentifier = C03G0SourceCertificate.MeasureIdentifier
      val selectedGroup = exactAliasedItem(select, 0, aliases(0)).collect {
        case identifier: SqlIdentifier => identifier
      }
      val countStar = exactAliasedItem(select, 1, aliases(1)).collect {
        case call: SqlCall => call
      }
      val countGroup = exactAliasedItem(select, 2, aliases(2)).collect {
        case call: SqlCall => call
      }
      val minimum = exactAliasedItem(select, 3, aliases(3)).collect {
        case call: SqlCall => call
      }
      val maximum = exactAliasedItem(select, 4, aliases(4)).collect {
        case call: SqlCall => call
      }

      select.getSelectList != null &&
      select.getSelectList.size == 5 &&
      select.getFrom != null &&
      select.getWhere == null &&
      !select.isDistinct &&
      group != null &&
      group.size == 1 &&
      selectedGroup.exists(exactDirectIdentifier(_, groupIdentifier)) &&
      (group.get(0) match {
        case identifier: SqlIdentifier =>
          exactDirectIdentifier(identifier, groupIdentifier)
        case _ => false
      }) &&
      countStar.exists(call =>
        isRawCountStar(call) && call.getFunctionQuantifier == null
      ) &&
      countGroup.exists(isRawCountColumn(_, groupIdentifier)) &&
      minimum.exists(
        isRawDirectExtremum(
          _,
          GlobalGuardedExtremumFunction.Min,
          measureIdentifier
        )
      ) &&
      maximum.exists(
        isRawDirectExtremum(
          _,
          GlobalGuardedExtremumFunction.Max,
          measureIdentifier
        )
      ) &&
      select.getHaving == null &&
      (select.getWindowList == null || select.getWindowList.isEmpty) &&
      select.getQualify == null &&
      (select.getOrderList == null || select.getOrderList.isEmpty) &&
      select.getOffset == null &&
      select.getFetch == null &&
      !select.hasHints
    }

    private def exactAliasedItem(
        select: SqlSelect,
        ordinal: Int,
        expectedAlias: String
    ): Option[SqlNode] = {
      if (
        select == null || select.getSelectList == null || ordinal < 0 ||
        ordinal >= select.getSelectList.size
      ) None
      else
        select.getSelectList.get(ordinal) match {
          case alias: SqlCall
              if alias.getKind == SqlKind.AS && alias.operandCount() == 2 =>
            alias.operand[SqlNode](1) match {
              case name: SqlIdentifier
                  if name.isSimple && name.getSimple == expectedAlias =>
                Some(alias.operand[SqlNode](0))
              case _ => None
            }
          case _ => None
        }
    }

    private def exactDirectIdentifier(
        identifier: SqlIdentifier,
        expectedNames: Vector[String]
    ): Boolean =
      identifier != null && !identifier.isStar && identifier.names != null &&
        identifier.names.asScala.toVector == expectedNames

    private def isRawCountColumn(
        call: SqlCall,
        expectedArgument: Vector[String]
    ): Boolean = {
      val operatorName = call.getOperator.getNameAsId
      call.getOperator.getName.equalsIgnoreCase("COUNT") &&
      operatorName != null &&
      operatorName.names.size == 1 &&
      call.getFunctionQuantifier == null &&
      call.operandCount() == 1 &&
      (call.operand[SqlNode](0) match {
        case identifier: SqlIdentifier =>
          exactDirectIdentifier(identifier, expectedArgument)
        case _ => false
      })
    }

    private def isRawDirectExtremum(
        call: SqlCall,
        expectedFunction: GlobalGuardedExtremumFunction,
        expectedArgument: Vector[String]
    ): Boolean =
      rawGuardedExtremumFunction(call).contains(expectedFunction) &&
        (call.operand[SqlNode](0) match {
          case identifier: SqlIdentifier =>
            exactDirectIdentifier(identifier, expectedArgument)
          case _ => false
        })

    private def exactTopLevelGroupedCountShape(select: SqlSelect): Boolean = {
      val group = select.getGroup
      select.getSelectList != null &&
      select.getSelectList.size == 2 &&
      select.getFrom != null &&
      !select.isDistinct &&
      group != null &&
      group.size == 1 &&
      directColumnItem(select.getSelectList.get(0)).exists { selected =>
        group.get(0) match {
          case grouped: SqlIdentifier => sameDirectColumn(selected, grouped)
          case _                      => false
        }
      } &&
      directCountItemAt(select, 1).nonEmpty &&
      select.getHaving == null &&
      (select.getWindowList == null || select.getWindowList.isEmpty) &&
      select.getQualify == null &&
      (select.getOrderList == null || select.getOrderList.isEmpty) &&
      select.getOffset == null &&
      select.getFetch == null &&
      !select.hasHints
    }

    private def exactTopLevelDistributedGroupedCountShape(
        select: SqlSelect,
        maximumGroupKeys: Int
    ): Boolean = {
      val group = select.getGroup
      val keyCount = if (group == null) 0 else group.size
      val selectedGroups =
        if (
          select.getSelectList == null || select.getSelectList.size < 2
        ) Vector.empty
        else (0 until math.min(keyCount, select.getSelectList.size)).toVector.flatMap(ordinal =>
          directColumnItem(select.getSelectList.get(ordinal))
        )
      select.getSelectList != null &&
      keyCount >= 2 && keyCount <= maximumGroupKeys &&
      select.getSelectList.size == keyCount + 1 &&
      select.getFrom != null &&
      !select.isDistinct &&
      group != null &&
      selectedGroups.size == keyCount &&
      !selectedGroups.combinations(2).exists(pair => sameDirectColumn(pair(0), pair(1))) &&
      selectedGroups.indices.forall { ordinal =>
        group.get(ordinal) match {
          case grouped: SqlIdentifier =>
            sameDirectColumn(selectedGroups(ordinal), grouped)
          case _ => false
        }
      } &&
      directCountItemAt(select, keyCount).nonEmpty &&
      select.getHaving == null &&
      (select.getWindowList == null || select.getWindowList.isEmpty) &&
      select.getQualify == null &&
      (select.getOrderList == null || select.getOrderList.isEmpty) &&
      select.getOffset == null &&
      select.getFetch == null &&
      !select.hasHints
    }

    private def directCountItem(
        select: SqlSelect
    ): Option[(SqlCall, GlobalCountStarAliasMode)] =
      directCountItemAt(select, 0)

    private def directCountItemAt(
        select: SqlSelect,
        ordinal: Int
    ): Option[(SqlCall, GlobalCountStarAliasMode)] = {
      if (
        select.getSelectList == null || ordinal < 0 ||
        ordinal >= select.getSelectList.size
      ) return None
      val item = select.getSelectList.get(ordinal)
      item match {
        case count: SqlCall if isRawCountStar(count) =>
          Some(count -> GlobalCountStarAliasMode.Implicit)
        case alias: SqlCall
            if alias.getKind == SqlKind.AS && alias.operandCount() == 2 =>
          (
            alias.operand[SqlNode](0),
            alias.operand[SqlNode](1)
          ) match {
            case (count: SqlCall, name: SqlIdentifier)
                if isRawCountStar(count) && name.isSimple =>
              Some(count -> GlobalCountStarAliasMode.Explicit)
            case _ => None
          }
        case _ => None
      }
    }

    private def directColumnItem(item: SqlNode): Option[SqlIdentifier] =
      item match {
        case identifier: SqlIdentifier
            if !identifier.isStar && identifier.names != null &&
              !identifier.names.isEmpty =>
          Some(identifier)
        case alias: SqlCall
            if alias.getKind == SqlKind.AS && alias.operandCount() == 2 =>
          (alias.operand[SqlNode](0), alias.operand[SqlNode](1)) match {
            case (identifier: SqlIdentifier, name: SqlIdentifier)
                if !identifier.isStar && identifier.names != null &&
                  !identifier.names.isEmpty && name.isSimple =>
              Some(identifier)
            case _ => None
          }
        case _ => None
      }

    /** Qualified and unqualified spellings may denote the same direct key. */
    private def sameDirectColumn(
        selected: SqlIdentifier,
        grouped: SqlIdentifier
    ): Boolean = {
      if (
        selected == null || grouped == null || selected.isStar ||
        grouped.isStar || selected.names == null || grouped.names == null ||
        selected.names.isEmpty || grouped.names.isEmpty
      ) false
      else {
        val selectedNames = selected.names.asScala.toVector
        val groupedNames = grouped.names.asScala.toVector
        selectedNames == groupedNames ||
        (selectedNames.last == groupedNames.last &&
          (selected.isSimple || grouped.isSimple))
      }
    }

    private def directGuardedExtremumItem(
        select: SqlSelect
    ): Option[
      (
          SqlCall,
          GlobalGuardedExtremumFunction,
          GlobalGuardedExtremumAliasMode
      )
    ] = {
      val item = select.getSelectList.get(0)
      item match {
        case extremum: SqlCall
            if rawGuardedExtremumFunction(extremum).nonEmpty =>
          rawGuardedExtremumFunction(extremum).map { function =>
            (
              extremum,
              function,
              GlobalGuardedExtremumAliasMode.Implicit
            )
          }
        case alias: SqlCall
            if alias.getKind == SqlKind.AS && alias.operandCount() == 2 =>
          (
            alias.operand[SqlNode](0),
            alias.operand[SqlNode](1)
          ) match {
            case (extremum: SqlCall, name: SqlIdentifier) if name.isSimple =>
              rawGuardedExtremumFunction(extremum).map { function =>
                (
                  extremum,
                  function,
                  GlobalGuardedExtremumAliasMode.Explicit
                )
              }
            case _ => None
          }
        case _ => None
      }
    }

    /** Calcite leaves parsed function names unresolved and parser-cased. */
    private def isRawCountStar(call: SqlCall): Boolean = {
      val operatorName = call.getOperator.getNameAsId
      call.getOperator.getName.equalsIgnoreCase("COUNT") &&
      operatorName != null &&
      operatorName.names.size == 1 &&
      call.operandCount() == 1 &&
      (call.operand[SqlNode](0) match {
        case identifier: SqlIdentifier =>
          identifier.isStar && identifier.names.size == 1
        case _ => false
      })
    }

    /** Only a direct qualified or unqualified non-star identifier is admitted. */
    private def rawGuardedExtremumFunction(
        call: SqlCall
    ): Option[GlobalGuardedExtremumFunction] = {
      val operatorName = call.getOperator.getNameAsId
      val function = GlobalGuardedExtremumFunction.values.find(value =>
        call.getOperator.getName.equalsIgnoreCase(value.sqlName)
      )
      function.filter { _ =>
        operatorName != null &&
        operatorName.names.size == 1 &&
        call.getFunctionQuantifier == null &&
        call.operandCount() == 1 &&
        (call.operand[SqlNode](0) match {
          case identifier: SqlIdentifier =>
            !identifier.isStar && identifier.names != null &&
              !identifier.names.isEmpty
          case _ => false
        })
      }
    }

    private def directTopLevelCountItem(parsed: SqlNode): Option[SqlCall] =
      parsed match {
        case select: SqlSelect => directCountItem(select).map(_._1)
        case call: SqlCall if call.getKind == SqlKind.ORDER_BY =>
          call.operand[SqlNode](0) match {
            case select: SqlSelect => directCountItem(select).map(_._1)
            case _ => None
          }
        case _ => None
      }

    private def directTopLevelGuardedExtremumItem(
        parsed: SqlNode
    ): Option[SqlCall] =
      parsed match {
        case select: SqlSelect =>
          directGuardedExtremumItem(select).map(_._1)
        case call: SqlCall if call.getKind == SqlKind.ORDER_BY =>
          call.operand[SqlNode](0) match {
            case select: SqlSelect =>
              directGuardedExtremumItem(select).map(_._1)
            case _ => None
          }
        case _ => None
      }

    private def rejectionReason(
        rawInspection: RawInspection,
        validatedInventory: AstInventory
    ): CalciteRejectedAggregateReason = {
      import CalciteRejectedAggregateReason._
      val rawInventory = rawInspection.inventory
      if (rawInventory.hasWindowCall || validatedInventory.hasWindowCall)
        WindowCall
      else if (
        rawInventory.hasGroupingOrDistinct ||
        validatedInventory.hasGroupingOrDistinct
      ) GroupedOrDistinct
      else if (validatedInventory.aggregateCalls.size > 1)
        MultipleAggregateCalls
      else {
        validatedInventory.aggregateCalls.headOption match {
          case Some(call) if call.getKind == SqlKind.COUNT && !call.isCountStar =>
            CountExpression
          case Some(call)
              if call.getKind == SqlKind.MIN || call.getKind == SqlKind.MAX =>
            if (rawInspection.hasDirectTopLevelGuardedExtremumResult)
              UnsupportedTopLevelAggregateShape
            else AggregateNotDirectTopLevelResult
          case Some(call) if call.getKind != SqlKind.COUNT =>
            OtherAggregateFunction
          case Some(_) =>
            if (rawInspection.hasDirectTopLevelCountResult)
              UnsupportedTopLevelAggregateShape
            else AggregateNotDirectTopLevelResult
          case None => GroupedOrDistinct
        }
      }
    }
  }
}
