package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.nio.charset.StandardCharsets
import java.util.ArrayDeque

import scala.collection.mutable

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidatePlanId
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlan
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.PostgreSqlStageId
import at.ac.tuwien.dbai.yannakakis.core.{BagId, QueryHypergraph}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  PredicateClassification,
  QueryIR,
  QueryResultShape
}

/** Closed scope vocabulary for the outcome-free enumeration feature surface. */
private[yannakakis] sealed trait PostgreSqlEnumerationFeatureScope
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlEnumerationFeatureScope {
  case object Workload extends PostgreSqlEnumerationFeatureScope {
    override val id: String = "workload"
  }

  case object Action extends PostgreSqlEnumerationFeatureScope {
    override val id: String = "action"
  }
}

/** Closed scalar vocabulary. Integer values remain exact Scala `BigInt`s. */
private[yannakakis] sealed trait PostgreSqlEnumerationFeatureValueType
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlEnumerationFeatureValueType {
  case object Boolean extends PostgreSqlEnumerationFeatureValueType {
    override val id: String = "boolean"
  }

  case object Integer extends PostgreSqlEnumerationFeatureValueType {
    override val id: String = "integer"
  }
}

/** Exact in-process source model used to derive one feature. */
private[yannakakis] sealed trait PostgreSqlEnumerationFeatureSource
    extends Product
    with Serializable {
  def id: String
  private[postgresql] def namePrefix: String
}

private[yannakakis] object PostgreSqlEnumerationFeatureSource {
  case object CandidatePlan extends PostgreSqlEnumerationFeatureSource {
    override val id: String = "bag_factor_plan"
    override private[postgresql] val namePrefix: String = "candidate."
  }

  case object Hypergraph extends PostgreSqlEnumerationFeatureSource {
    override val id: String = "query_hypergraph"
    override private[postgresql] val namePrefix: String = "hypergraph."
  }

  case object QueryAnalysis extends PostgreSqlEnumerationFeatureSource {
    override val id: String = "query_analysis"
    override private[postgresql] val namePrefix: String = "query."
  }

  case object Rendering extends PostgreSqlEnumerationFeatureSource {
    override val id: String = "stage_diagnostic_rendering"
    override private[postgresql] val namePrefix: String = "render."
  }

  case object Search extends PostgreSqlEnumerationFeatureSource {
    override val id: String = "exact_candidate_search"
    override private[postgresql] val namePrefix: String = "search."
  }
}

/** Closed unit vocabulary; SQL sizes are UTF-8 bytes, never JVM characters. */
private[yannakakis] sealed trait PostgreSqlEnumerationFeatureUnit
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlEnumerationFeatureUnit {
  case object BooleanFlag extends PostgreSqlEnumerationFeatureUnit {
    override val id: String = "boolean_flag"
  }

  case object Count extends PostgreSqlEnumerationFeatureUnit {
    override val id: String = "count"
  }

  case object Utf8Bytes extends PostgreSqlEnumerationFeatureUnit {
    override val id: String = "utf8_bytes"
  }
}

/** Structural absence contract, independent of an exceptional capture failure. */
private[yannakakis] sealed trait PostgreSqlEnumerationFeatureAbsenceRule
    extends Product
    with Serializable {
  def id: String
  private[postgresql] def permits(
      state: PostgreSqlEnumerationFeatureState
  ): Boolean
}

private[yannakakis] object PostgreSqlEnumerationFeatureAbsenceRule {
  case object Required extends PostgreSqlEnumerationFeatureAbsenceRule {
    override val id: String = "required"
    override private[postgresql] def permits(
        state: PostgreSqlEnumerationFeatureState
    ): Boolean = state == PostgreSqlEnumerationFeatureState.Observed
  }

  case object MayBeNotApplicable
      extends PostgreSqlEnumerationFeatureAbsenceRule {
    override val id: String = "may_be_not_applicable"
    override private[postgresql] def permits(
        state: PostgreSqlEnumerationFeatureState
    ): Boolean =
      state == PostgreSqlEnumerationFeatureState.Observed ||
        state == PostgreSqlEnumerationFeatureState.NotApplicable
  }

  case object MayBeMissingOptional
      extends PostgreSqlEnumerationFeatureAbsenceRule {
    override val id: String = "may_be_missing_optional"
    override private[postgresql] def permits(
        state: PostgreSqlEnumerationFeatureState
    ): Boolean =
      state == PostgreSqlEnumerationFeatureState.Observed ||
        state == PostgreSqlEnumerationFeatureState.MissingOptional
  }

  case object MayBeBoth extends PostgreSqlEnumerationFeatureAbsenceRule {
    override val id: String = "may_be_both"
    override private[postgresql] def permits(
        state: PostgreSqlEnumerationFeatureState
    ): Boolean =
      state == PostgreSqlEnumerationFeatureState.Observed ||
        state == PostgreSqlEnumerationFeatureState.NotApplicable ||
        state == PostgreSqlEnumerationFeatureState.MissingOptional
  }
}

/** One closed, versioned feature declaration. */
private[yannakakis] final case class PostgreSqlEnumerationFeatureSpec(
    name: String,
    scope: PostgreSqlEnumerationFeatureScope,
    valueType: PostgreSqlEnumerationFeatureValueType,
    source: PostgreSqlEnumerationFeatureSource,
    unit: PostgreSqlEnumerationFeatureUnit,
    absenceRule: PostgreSqlEnumerationFeatureAbsenceRule,
    definition: String
) extends Serializable {
  require(
    name != null && name.matches("[a-z][a-z0-9_]*(?:\\.[a-z][a-z0-9_]*)+"),
    "feature name must be canonical dotted lower snake case"
  )
  require(scope != null, "feature scope must not be null")
  require(valueType != null, "feature value type must not be null")
  require(source != null, "feature source must not be null")
  require(unit != null, "feature unit must not be null")
  require(absenceRule != null, "feature absence rule must not be null")
  require(
    definition != null && definition.trim.nonEmpty,
    "feature definition must not be blank"
  )
  require(
    name.startsWith(source.namePrefix),
    "feature name must match its exact source namespace"
  )
  require(
    (valueType == PostgreSqlEnumerationFeatureValueType.Boolean) ==
      (unit == PostgreSqlEnumerationFeatureUnit.BooleanFlag),
    "boolean features and boolean units must agree"
  )
}

/** Explicit capture state; no sentinel numeric value represents absence. */
private[yannakakis] sealed trait PostgreSqlEnumerationFeatureState
    extends Product
    with Serializable {
  def id: String
}

private[yannakakis] object PostgreSqlEnumerationFeatureState {
  case object Observed extends PostgreSqlEnumerationFeatureState {
    override val id: String = "observed"
  }

  case object NotApplicable extends PostgreSqlEnumerationFeatureState {
    override val id: String = "not_applicable"
  }

  case object MissingOptional extends PostgreSqlEnumerationFeatureState {
    override val id: String = "missing_optional"
  }

  case object CaptureFailed extends PostgreSqlEnumerationFeatureState {
    override val id: String = "capture_failed"
  }
}

/** Exact observed scalar value. */
private[yannakakis] sealed trait PostgreSqlEnumerationObservedFeatureValue
    extends Product
    with Serializable {
  def valueType: PostgreSqlEnumerationFeatureValueType
  def canonicalScalar: String
}

private[yannakakis] object PostgreSqlEnumerationObservedFeatureValue {
  final case class BooleanValue(value: Boolean)
      extends PostgreSqlEnumerationObservedFeatureValue {
    override val valueType: PostgreSqlEnumerationFeatureValueType =
      PostgreSqlEnumerationFeatureValueType.Boolean
    override val canonicalScalar: String = value.toString
  }

  final case class IntegerValue(value: BigInt)
      extends PostgreSqlEnumerationObservedFeatureValue {
    require(value != null && value >= 0, "feature count must be non-negative")
    override val valueType: PostgreSqlEnumerationFeatureValueType =
      PostgreSqlEnumerationFeatureValueType.Integer
    override val canonicalScalar: String = value.toString
  }
}

/** One registry-aligned feature value. */
private[yannakakis] final case class PostgreSqlEnumerationFeatureValue private[postgresql] (
    spec: PostgreSqlEnumerationFeatureSpec,
    state: PostgreSqlEnumerationFeatureState,
    value: Option[PostgreSqlEnumerationObservedFeatureValue]
) extends Serializable {
  require(spec != null, "feature spec must not be null")
  require(state != null, "feature state must not be null")
  require(
    value != null && value.forall(_ != null),
    "feature value option must not be null or contain null"
  )
  require(
    (state == PostgreSqlEnumerationFeatureState.Observed) == value.nonEmpty,
    "only an observed feature may retain a scalar value"
  )
  require(
    value.forall(_.valueType == spec.valueType),
    "observed feature scalar must match the registry type"
  )
  require(
    state == PostgreSqlEnumerationFeatureState.CaptureFailed ||
      spec.absenceRule.permits(state),
    "feature state conflicts with the registry absence rule"
  )

  val canonicalScalar: Option[String] = value.map(_.canonicalScalar)
}

private[postgresql] object PostgreSqlEnumerationFeatureValue {
  def observed(
      spec: PostgreSqlEnumerationFeatureSpec,
      value: PostgreSqlEnumerationObservedFeatureValue
  ): PostgreSqlEnumerationFeatureValue =
    PostgreSqlEnumerationFeatureValue(
      spec,
      PostgreSqlEnumerationFeatureState.Observed,
      Some(value)
    )

  def notApplicable(
      spec: PostgreSqlEnumerationFeatureSpec
  ): PostgreSqlEnumerationFeatureValue =
    PostgreSqlEnumerationFeatureValue(
      spec,
      PostgreSqlEnumerationFeatureState.NotApplicable,
      None
    )
}

/** Action identity is metadata, not a model feature. */
private[yannakakis] sealed trait PostgreSqlEnumerationFeatureAction
    extends Product
    with Serializable {
  def kind: String
}

private[yannakakis] object PostgreSqlEnumerationFeatureAction {
  case object Native extends PostgreSqlEnumerationFeatureAction {
    override val kind: String = "native"
  }

  final case class RetainedCandidate(
      structuralRank: Int,
      candidateId: CandidatePlanId,
      physicalVariantId: PostgreSqlPhysicalVariantId
  ) extends PostgreSqlEnumerationFeatureAction {
    require(structuralRank > 0, "candidate action rank must be positive")
    require(candidateId != null, "candidate action id must not be null")
    require(
      physicalVariantId != null,
      "candidate physical variant id must not be null"
    )
    override val kind: String = "retained_candidate"
  }
}

/** One action and the complete registry-aligned value vector. */
private[yannakakis] final case class PostgreSqlEnumerationFeatureActionRow private[postgresql] (
    action: PostgreSqlEnumerationFeatureAction,
    values: Vector[PostgreSqlEnumerationFeatureValue]
) extends Serializable {
  require(action != null, "feature action must not be null")
  require(
    values != null && values.forall(_ != null),
    "feature values must not be null or contain null"
  )
}

/**
  * Complete native-plus-retained-action feature matrix.
  *
  * Workload values are repeated exactly across rows so each action can be
  * consumed independently. Candidate/render values are structurally absent
  * for native and observed for every compiled retained candidate.
  */
private[yannakakis] final case class PostgreSqlEnumerationStructuralFeatureExport private[postgresql] (
    registryVersion: String,
    registry: Vector[PostgreSqlEnumerationFeatureSpec],
    actions: Vector[PostgreSqlEnumerationFeatureActionRow]
) extends Serializable {
  require(
    registryVersion == PostgreSqlEnumerationStructuralFeatures.RegistryVersion,
    "feature export must retain the frozen registry version"
  )
  require(
    registry == PostgreSqlEnumerationStructuralFeatures.Registry,
    "feature export must retain the exact closed registry"
  )
  require(actions != null && actions.nonEmpty, "feature actions must be nonempty")
  require(actions.forall(_ != null), "feature actions must not contain null")
  require(
    actions.head.action == PostgreSqlEnumerationFeatureAction.Native &&
      actions.tail.forall(
        _.action.isInstanceOf[
          PostgreSqlEnumerationFeatureAction.RetainedCandidate
        ]
      ),
    "native must precede every retained candidate action"
  )

  private val retained = actions.tail.map(
    _.action.asInstanceOf[PostgreSqlEnumerationFeatureAction.RetainedCandidate]
  )
  require(
    retained.map(_.structuralRank) == (1 to retained.size).toVector,
    "candidate action ranks must be contiguous and increasing"
  )
  require(
    retained.map(_.candidateId).distinct.size == retained.size &&
      retained.map(_.physicalVariantId).distinct.size == retained.size,
    "candidate and physical action identities must be unique"
  )
  require(
    actions.forall(_.values.map(_.spec) == registry),
    "every action must cover the complete registry in canonical order"
  )

  private val workloadIndices = registry.indices.filter(index =>
    registry(index).scope == PostgreSqlEnumerationFeatureScope.Workload
  )
  private val actionIndices = registry.indices.filter(index =>
    registry(index).scope == PostgreSqlEnumerationFeatureScope.Action
  )
  private val nativeValues = actions.head.values
  require(
    workloadIndices.forall(index =>
      actions.forall(_.values(index) == nativeValues(index)) &&
        nativeValues(index).state == PostgreSqlEnumerationFeatureState.Observed
    ),
    "workload values must be observed and equality-identical across actions"
  )
  require(
    actionIndices.forall(index =>
      nativeValues(index).state ==
        PostgreSqlEnumerationFeatureState.NotApplicable &&
        actions.tail.forall(
          _.values(index).state == PostgreSqlEnumerationFeatureState.Observed
        )
    ),
    "native action features must be not-applicable and candidates observed"
  )
}

/** Pure extractor for the initial, semantic-only enumeration feature registry. */
private[yannakakis] object PostgreSqlEnumerationStructuralFeatures {
  val RegistryVersion: String =
    "postgresql_enumeration_structural_features/v1"

  import PostgreSqlEnumerationFeatureAbsenceRule.{
    MayBeNotApplicable,
    Required
  }
  import PostgreSqlEnumerationFeatureScope.{Action, Workload}
  import PostgreSqlEnumerationFeatureSource.{
    CandidatePlan,
    Hypergraph,
    QueryAnalysis,
    Rendering,
    Search
  }
  import PostgreSqlEnumerationFeatureUnit.{
    BooleanFlag,
    Count,
    Utf8Bytes
  }
  import PostgreSqlEnumerationFeatureValueType.{Boolean, Integer}

  val Registry: Vector[PostgreSqlEnumerationFeatureSpec] = Vector(
    PostgreSqlEnumerationFeatureSpec(
      "candidate.bag_count",
      Action,
      Integer,
      CandidatePlan,
      Count,
      MayBeNotApplicable,
      "Number of bags in the retained covered decomposition."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "candidate.enumeration_occurrence_count",
      Action,
      Integer,
      CandidatePlan,
      Count,
      MayBeNotApplicable,
      "Number of exact source-occurrence ownership assignments."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "candidate.maximum_bag_size",
      Action,
      Integer,
      CandidatePlan,
      Count,
      MayBeNotApplicable,
      "Maximum number of distinct structural variables in one bag."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "candidate.maximum_branching_factor",
      Action,
      Integer,
      CandidatePlan,
      Count,
      MayBeNotApplicable,
      "Maximum number of oriented child bags of one factor."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "candidate.maximum_separator_size",
      Action,
      Integer,
      CandidatePlan,
      Count,
      MayBeNotApplicable,
      "Maximum exact parent-child separator variable count."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "candidate.semijoin_step_count",
      Action,
      Integer,
      CandidatePlan,
      Count,
      MayBeNotApplicable,
      "Total logical steps in the canonical two-pass semijoin schedule."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "candidate.tree_depth",
      Action,
      Integer,
      CandidatePlan,
      Count,
      MayBeNotApplicable,
      "Maximum oriented root-to-bag edge depth, with the root at depth zero."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "hypergraph.edge_count",
      Workload,
      Integer,
      Hypergraph,
      Count,
      Required,
      "Number of occurrence-distinct hyperedges."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "hypergraph.maximum_edge_arity",
      Workload,
      Integer,
      Hypergraph,
      Count,
      Required,
      "Maximum exact column-binding arity reported by a hyperedge."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "hypergraph.vertex_count",
      Workload,
      Integer,
      Hypergraph,
      Count,
      Required,
      "Number of distinct structural join variables."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "query.distinct_join_variable_count",
      Workload,
      Integer,
      QueryAnalysis,
      Count,
      Required,
      "Number of distinct join variables certified by analysis and conversion."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "query.join_predicate_count",
      Workload,
      Integer,
      QueryAnalysis,
      Count,
      Required,
      "Number of supported cross-occurrence inner-equality conjuncts."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "query.relation_occurrence_count",
      Workload,
      Integer,
      QueryAnalysis,
      Count,
      Required,
      "Number of occurrence-distinct relation scans in the analyzed query."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "render.maximum_stage_dependency_depth",
      Action,
      Integer,
      Rendering,
      Count,
      MayBeNotApplicable,
      "Maximum emitted-stage dependency-edge depth, with source stages at zero."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "render.maximum_statement_bytes",
      Action,
      Integer,
      Rendering,
      Utf8Bytes,
      MayBeNotApplicable,
      "Maximum UTF-8 size among renderer-owned stage diagnostic statements."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "render.statement_count",
      Action,
      Integer,
      Rendering,
      Count,
      MayBeNotApplicable,
      "Number of renderer-owned emitted-stage diagnostic statements."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "render.total_sql_bytes",
      Action,
      Integer,
      Rendering,
      Utf8Bytes,
      MayBeNotApplicable,
      "Exact sum of UTF-8 sizes over all stage diagnostic statements."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "search.exhausted_before_k",
      Workload,
      Boolean,
      Search,
      BooleanFlag,
      Required,
      "Whether exact retained search proved fewer results than requested K."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "search.requested_limit",
      Workload,
      Integer,
      Search,
      Count,
      Required,
      "Requested exact retained-prefix limit K."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "search.retained_count",
      Workload,
      Integer,
      Search,
      Count,
      Required,
      "Number of candidates returned by the exact retained search."
    ),
    PostgreSqlEnumerationFeatureSpec(
      "search.width_bound",
      Workload,
      Integer,
      Search,
      Count,
      Required,
      "Requested fixed soft-width bound, recorded as search configuration."
    )
  )

  private val ForbiddenModelNameTokens: Set[String] = Set(
    "cardinality",
    "duration",
    "id",
    "latency",
    "outcome",
    "rank",
    "runtime",
    "speedup",
    "timing",
    "topology",
    "winner"
  )

  private val registryNames = Registry.map(_.name)
  require(
    registryNames == registryNames.sorted &&
      registryNames.distinct.size == registryNames.size,
    "feature registry names must be unique and alphabetically ordered"
  )
  require(
    Registry.forall(spec =>
      spec.name
        .split("[._-]")
        .forall(token => !ForbiddenModelNameTokens.contains(token))
    ),
    "feature registry must not expose identities, rank, topology, or outcomes"
  )

  def export(
      artifact: PostgreSqlEnumerationBatchArtifact
  ): PostgreSqlEnumerationStructuralFeatureExport = {
    require(artifact != null, "enumeration batch artifact must not be null")

    val workload = workloadValues(artifact)
    val workloadNames = Registry.collect {
      case spec if spec.scope == Workload => spec.name
    }.toSet
    require(
      workload.keySet == workloadNames,
      "workload extractor must cover exactly the workload registry"
    )

    val nativeValues = Registry.map { spec =>
      if (spec.scope == Workload)
        PostgreSqlEnumerationFeatureValue.observed(spec, workload(spec.name))
      else PostgreSqlEnumerationFeatureValue.notApplicable(spec)
    }
    val rows = Vector.newBuilder[PostgreSqlEnumerationFeatureActionRow]
    rows += PostgreSqlEnumerationFeatureActionRow(
      PostgreSqlEnumerationFeatureAction.Native,
      nativeValues
    )

    val candidates = artifact.batch.candidates.iterator
    while (candidates.hasNext) {
      val candidate = candidates.next()
      val action = candidateValues(candidate)
      val actionNames = Registry.collect {
        case spec if spec.scope == Action => spec.name
      }.toSet
      require(
        action.keySet == actionNames,
        "candidate extractor must cover exactly the action registry"
      )
      val values = Registry.map { spec =>
        val observed =
          if (spec.scope == Workload) workload(spec.name)
          else action(spec.name)
        PostgreSqlEnumerationFeatureValue.observed(spec, observed)
      }
      rows += PostgreSqlEnumerationFeatureActionRow(
        PostgreSqlEnumerationFeatureAction.RetainedCandidate(
          candidate.plan.rank,
          candidate.plan.candidateId,
          candidate.physicalVariantId
        ),
        values
      )
    }

    PostgreSqlEnumerationStructuralFeatureExport(
      RegistryVersion,
      Registry,
      rows.result()
    )
  }

  /**
    * Export the same closed registry for an exact executable native fallback.
    * Candidate-scoped values remain structurally not applicable; no diagnostic
    * text or downstream outcome is consulted.
    */
  def exportNativeFallback(
      result: PostgreSqlEnumerationBatchResult
  ): PostgreSqlEnumerationStructuralFeatureExport = {
    require(result != null, "enumeration fallback result must not be null")
    require(
      result.status == PostgreSqlEnumerationBatchStatus.NotApplicable &&
        result.artifact.isEmpty,
      "native fallback features require a non-produced not-applicable result"
    )
    val report = result.pipelineEvidence
    val analysis = report.analysis.getOrElse(
      throw new IllegalArgumentException(
        "native fallback must retain analyzed query evidence"
      )
    )
    val hypergraph = report.hypergraph.getOrElse(
      throw new IllegalArgumentException(
        "native fallback must retain exact hypergraph evidence"
      )
    )
    require(
      analysis.diagnostics.isEmpty &&
        analysis.query.resultShape == QueryResultShape.EnumerationProjection,
      "native fallback must retain a supported diagnostic-free enumeration analysis"
    )
    val workload = workloadValues(
      analysis.query,
      hypergraph,
      requestedLimit = result.request.planLimit,
      retainedCount = 0,
      exhaustedBeforeK = false,
      widthBound = result.request.widthBound
    )
    val nativeValues = Registry.map { spec =>
      if (spec.scope == Workload)
        PostgreSqlEnumerationFeatureValue.observed(spec, workload(spec.name))
      else PostgreSqlEnumerationFeatureValue.notApplicable(spec)
    }
    PostgreSqlEnumerationStructuralFeatureExport(
      RegistryVersion,
      Registry,
      Vector(
        PostgreSqlEnumerationFeatureActionRow(
          PostgreSqlEnumerationFeatureAction.Native,
          nativeValues
        )
      )
    )
  }

  private def workloadValues(
      artifact: PostgreSqlEnumerationBatchArtifact
  ): Map[String, PostgreSqlEnumerationObservedFeatureValue] =
    workloadValues(
      artifact.analysis.query,
      artifact.hypergraph,
      artifact.batch.requestedLimit,
      artifact.batch.retainedCount,
      artifact.batch.exhaustedBeforeK,
      artifact.request.widthBound
    )

  private def workloadValues(
      query: QueryIR,
      hypergraph: QueryHypergraph,
      requestedLimit: Int,
      retainedCount: Int,
      exhaustedBeforeK: Boolean,
      widthBound: Int
  ): Map[String, PostgreSqlEnumerationObservedFeatureValue] = {
    require(query != null, "feature query must not be null")
    require(hypergraph != null, "feature hypergraph must not be null")
    require(requestedLimit > 0, "feature requested limit must be positive")
    require(retainedCount >= 0, "feature retained count must be nonnegative")
    require(widthBound > 0, "feature width bound must be positive")
    val maximumEdgeArity = hypergraph.edges.iterator
      .foldLeft(BigInt(0))((maximum, edge) => maximum.max(BigInt(edge.arity)))
    val joinPredicateCount = query.predicates.iterator.count(
      _.classification.isInstanceOf[PredicateClassification.InnerEquality]
    )

    Map(
      "hypergraph.edge_count" -> integer(hypergraph.edges.size),
      "hypergraph.maximum_edge_arity" -> integer(maximumEdgeArity),
      "hypergraph.vertex_count" -> integer(hypergraph.variables.size),
      "query.distinct_join_variable_count" -> integer(
        hypergraph.variables.size
      ),
      "query.join_predicate_count" -> integer(joinPredicateCount),
      "query.relation_occurrence_count" -> integer(query.relations.size),
      "search.exhausted_before_k" -> boolean(exhaustedBeforeK),
      "search.requested_limit" -> integer(requestedLimit),
      "search.retained_count" -> integer(retainedCount),
      "search.width_bound" -> integer(widthBound)
    )
  }

  private def candidateValues(
      candidate: PostgreSqlCompiledRetainedCandidate
  ): Map[String, PostgreSqlEnumerationObservedFeatureValue] = {
    require(candidate != null, "compiled retained candidate must not be null")
    val plan = candidate.bagFactorPlan
    val shape = deriveTreeShape(plan)
    val rendering = deriveRenderingShape(candidate)
    val maximumBagSize = plan.decomposition.bags.iterator.foldLeft(BigInt(0))(
      (maximum, bag) => maximum.max(BigInt(bag.vertices.size))
    )
    val maximumSeparatorSize = plan.arcs.iterator.foldLeft(BigInt(0))(
      (maximum, arc) => maximum.max(BigInt(arc.separator.size))
    )
    val semijoinStepCount =
      BigInt(plan.schedule.bottomUp.size) + BigInt(plan.schedule.topDown.size)

    Map(
      "candidate.bag_count" -> integer(plan.decomposition.bags.size),
      "candidate.enumeration_occurrence_count" -> integer(
        plan.ownership.size
      ),
      "candidate.maximum_bag_size" -> integer(maximumBagSize),
      "candidate.maximum_branching_factor" -> integer(
        shape.maximumBranchingFactor
      ),
      "candidate.maximum_separator_size" -> integer(maximumSeparatorSize),
      "candidate.semijoin_step_count" -> integer(semijoinStepCount),
      "candidate.tree_depth" -> integer(shape.maximumDepth),
      "render.maximum_stage_dependency_depth" -> integer(
        rendering.maximumStageDependencyDepth
      ),
      "render.maximum_statement_bytes" -> integer(
        rendering.maximumStatementBytes
      ),
      "render.statement_count" -> integer(rendering.statementCount),
      "render.total_sql_bytes" -> integer(rendering.totalSqlBytes)
    )
  }

  private final case class TreeShape(
      maximumDepth: BigInt,
      maximumBranchingFactor: BigInt
  )

  /** Iterative traversal keeps feature extraction stack-safe for deep trees. */
  private def deriveTreeShape(plan: BagFactorPlan): TreeShape = {
    val byId = plan.factors.iterator.map(factor => factor.bagId -> factor).toMap
    require(
      byId.size == plan.factors.size && byId.contains(plan.root),
      "bag-factor tree must have unique factors and a root"
    )

    val queue = new ArrayDeque[BagId]()
    val depthByBag = mutable.Map.empty[BagId, BigInt]
    queue.addLast(plan.root)
    depthByBag.put(plan.root, BigInt(0))
    var maximumDepth = BigInt(0)
    var maximumBranchingFactor = BigInt(0)

    while (!queue.isEmpty) {
      val bagId = queue.removeFirst()
      val factor = byId(bagId)
      val depth = depthByBag(bagId)
      require(
        BigInt(factor.depth) == depth,
        "bag-factor depth differs from iterative tree depth"
      )
      maximumDepth = maximumDepth.max(depth)
      maximumBranchingFactor =
        maximumBranchingFactor.max(BigInt(factor.children.size))

      val children = factor.children.iterator
      while (children.hasNext) {
        val child = children.next()
        require(byId.contains(child), "bag-factor child is missing")
        require(
          !depthByBag.contains(child),
          "bag-factor orientation is cyclic or has multiple parents"
        )
        depthByBag.put(child, depth + 1)
        queue.addLast(child)
      }
    }
    require(
      depthByBag.size == plan.factors.size,
      "bag-factor orientation must reach every factor"
    )
    TreeShape(maximumDepth, maximumBranchingFactor)
  }

  private final case class RenderingShape(
      statementCount: BigInt,
      totalSqlBytes: BigInt,
      maximumStatementBytes: BigInt,
      maximumStageDependencyDepth: BigInt
  )

  /**
    * Uses the validated renderer order as an iterative topological order.
    * Elided logical stages do not have diagnostic statements and therefore do
    * not participate in this emitted-stage dependency metric.
    */
  private def deriveRenderingShape(
      candidate: PostgreSqlCompiledRetainedCandidate
  ): RenderingShape = {
    val statements = candidate.stageDiagnostics.statements
    val emittedStages = candidate.renderedQuery.trace.stages.filter(
      _.physicalForm.emitted
    )
    require(
      statements.map(_.stageId) == emittedStages.map(_.id),
      "diagnostic statements must equal emitted stages in renderer order"
    )

    val depthByStage = mutable.Map.empty[PostgreSqlStageId, BigInt]
    var maximumStageDepth = BigInt(0)
    val stages = emittedStages.iterator
    while (stages.hasNext) {
      val stage = stages.next()
      var maximumInputDepth: Option[BigInt] = None
      val inputs = stage.inputStageIds.iterator
      while (inputs.hasNext) {
        val input = inputs.next()
        val inputDepth = depthByStage.getOrElse(
          input,
          throw new IllegalArgumentException(
            "stage dependency is not an earlier emitted stage"
          )
        )
        maximumInputDepth = Some(
          maximumInputDepth.fold(inputDepth)(_.max(inputDepth))
        )
      }
      val depth = maximumInputDepth.fold(BigInt(0))(_ + 1)
      require(
        !depthByStage.contains(stage.id),
        "emitted stage identities must be unique"
      )
      depthByStage.put(stage.id, depth)
      maximumStageDepth = maximumStageDepth.max(depth)
    }

    var totalSqlBytes = BigInt(0)
    var maximumStatementBytes = BigInt(0)
    val statementIterator = statements.iterator
    while (statementIterator.hasNext) {
      val byteCount = BigInt(
        statementIterator.next().sql.getBytes(StandardCharsets.UTF_8).length
      )
      totalSqlBytes += byteCount
      maximumStatementBytes = maximumStatementBytes.max(byteCount)
    }

    RenderingShape(
      BigInt(statements.size),
      totalSqlBytes,
      maximumStatementBytes,
      maximumStageDepth
    )
  }

  private def integer(
      value: Int
  ): PostgreSqlEnumerationObservedFeatureValue = integer(BigInt(value))

  private def integer(
      value: BigInt
  ): PostgreSqlEnumerationObservedFeatureValue =
    PostgreSqlEnumerationObservedFeatureValue.IntegerValue(value)

  private def boolean(
      value: Boolean
  ): PostgreSqlEnumerationObservedFeatureValue =
    PostgreSqlEnumerationObservedFeatureValue.BooleanValue(value)
}
