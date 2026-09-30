package at.ac.tuwien.dbai.yannakakis.api.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.{
  CandidatePlanId,
  CandidateSearchInput,
  RankedCandidatePlan
}
import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorCoverAssignment,
  BagFactorPhysicalConfiguration
}
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode
import at.ac.tuwien.dbai.yannakakis.core.BagId

import scala.collection.mutable

/** Request-wide materialization syntax applied to every named emitted CTE. */
sealed trait PostgreSqlCteMaterializationDirective
    extends Product
    with Serializable {
  def id: String
  private[postgresql] def sql: String
  final def isExplicit: Boolean =
    this != PostgreSqlCteMaterializationDirective.PlannerControlled
}

object PostgreSqlCteMaterializationDirective {
  val Scope: String = "all-emitted-ctes-v1"
  val MinimumExplicitServerVersionNum: Int = 120000

  case object PlannerControlled extends PostgreSqlCteMaterializationDirective {
    override val id: String = "planner-controlled"
    override private[postgresql] val sql: String = ""
  }
  case object Materialized extends PostgreSqlCteMaterializationDirective {
    override val id: String = "materialized"
    override private[postgresql] val sql: String = "MATERIALIZED "
  }
  case object NotMaterialized extends PostgreSqlCteMaterializationDirective {
    override val id: String = "not-materialized"
    override private[postgresql] val sql: String = "NOT MATERIALIZED "
  }

  val Values: Vector[PostgreSqlCteMaterializationDirective] = Vector(
    PlannerControlled,
    Materialized,
    NotMaterialized
  )

  def parse(value: String): Option[PostgreSqlCteMaterializationDirective] =
    if (value == null) None else Values.find(_.id == value)
}

/** Frozen definition of the opt-in bounded physical portfolio. */
object PostgreSqlExplainFactorAwarePhysicalPortfolioV1 {
  val PolicyId: String =
    "postgresql_explain_factor_aware_physical_portfolio_v1"
  val Scope: String =
    "bounded_physical_portfolio_over_retained_structural_top_k_v1"
  val GlobalOptimality: Boolean = false
  val RequiredPlanLimit: Int = 100
  val WorkloadIdentityScheme: String =
    "postgresql_sql_stage_probe_workload_sha256_v1"
  val EnumerationScheme: String =
    "postgresql_bounded_physical_portfolio_heap_wave_v1"
  val DeduplicationScheme: String =
    "postgresql_workload_minimum_physical_variant_id_v1"
  val CrossProductReserve: Int = 16
  val ReductionModeOrder: Vector[YannakakisReductionMode] = Vector(
    YannakakisReductionMode.FullTwoPass,
    YannakakisReductionMode.BottomUpOnly,
    YannakakisReductionMode.NoReduction
  )
  val MaterializationDirectiveOrder
      : Vector[PostgreSqlCteMaterializationDirective] = Vector(
    PostgreSqlCteMaterializationDirective.PlannerControlled,
    PostgreSqlCteMaterializationDirective.Materialized,
    PostgreSqlCteMaterializationDirective.NotMaterialized
  )
}

sealed trait PostgreSqlPhysicalPortfolioResource
    extends Product
    with Serializable {
  def code: String
}

object PostgreSqlPhysicalPortfolioResource {
  case object StructuralCandidates extends PostgreSqlPhysicalPortfolioResource {
    override val code: String = "structural-candidates"
  }
  case object RootsPerCandidate extends PostgreSqlPhysicalPortfolioResource {
    override val code: String = "roots-per-candidate"
  }
  case object CoversPerBag extends PostgreSqlPhysicalPortfolioResource {
    override val code: String = "covers-per-bag"
  }
  case object CoverStatesPerCandidate
      extends PostgreSqlPhysicalPortfolioResource {
    override val code: String = "cover-states-per-candidate"
  }
  case object VariantsPerCandidate extends PostgreSqlPhysicalPortfolioResource {
    override val code: String = "variants-per-candidate"
  }
  case object RawVariantsPerSession extends PostgreSqlPhysicalPortfolioResource {
    override val code: String = "raw-variants-per-session"
  }
  case object UniqueWorkloadsPerSession
      extends PostgreSqlPhysicalPortfolioResource {
    override val code: String = "unique-workloads-per-session"
  }
}

/** Fixed hard maxima with optionally lower, internally consistent prefixes. */
final class PostgreSqlPhysicalPortfolioLimits private (
    val maxStructuralCandidates: Int,
    val maxRootsPerCandidate: Int,
    val maxCoversPerBag: Int,
    val maxCoverStatesPerCandidate: Int,
    val maxVariantsPerCandidate: Int,
    val maxRawVariantsPerSession: Int,
    val maxUniqueWorkloadsPerSession: Int
) extends Serializable {
  override def equals(other: Any): Boolean = other match {
    case that: PostgreSqlPhysicalPortfolioLimits =>
      maxStructuralCandidates == that.maxStructuralCandidates &&
        maxRootsPerCandidate == that.maxRootsPerCandidate &&
        maxCoversPerBag == that.maxCoversPerBag &&
        maxCoverStatesPerCandidate == that.maxCoverStatesPerCandidate &&
        maxVariantsPerCandidate == that.maxVariantsPerCandidate &&
        maxRawVariantsPerSession == that.maxRawVariantsPerSession &&
        maxUniqueWorkloadsPerSession == that.maxUniqueWorkloadsPerSession
    case _ => false
  }
  override def hashCode(): Int =
    (
      maxStructuralCandidates,
      maxRootsPerCandidate,
      maxCoversPerBag,
      maxCoverStatesPerCandidate,
      maxVariantsPerCandidate,
      maxRawVariantsPerSession,
      maxUniqueWorkloadsPerSession
    ).hashCode()
  override def toString: String =
    s"PostgreSqlPhysicalPortfolioLimits($maxStructuralCandidates," +
      s"$maxRootsPerCandidate,$maxCoversPerBag," +
      s"$maxCoverStatesPerCandidate,$maxVariantsPerCandidate," +
      s"$maxRawVariantsPerSession,$maxUniqueWorkloadsPerSession)"
}

sealed trait PostgreSqlPhysicalPortfolioLimitError
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object PostgreSqlPhysicalPortfolioLimitError {
  final case class InvalidValue(field: String, value: Int, constraint: String)
      extends PostgreSqlPhysicalPortfolioLimitError {
    override val code: String = s"physical-portfolio.limit.invalid.$field"
    override val message: String =
      s"Physical portfolio limit $field=$value must satisfy $constraint"
  }
}

object PostgreSqlPhysicalPortfolioLimits {
  val MaxStructuralCandidates = 100
  val MaxRootsPerCandidate = 64
  val MaxCoversPerBag = 64
  val MaxCoverStatesPerCandidate = 256
  val MaxVariantsPerCandidate = 256
  val MaxRawVariantsPerSession = 256
  val MaxUniqueWorkloadsPerSession = 256
  val MinCoverStatesPerCandidate = 2
  val MinVariantsPerCandidate = 7
  val MinRawVariantsPerSession =
    2 * MaxStructuralCandidates + 5 +
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.CrossProductReserve

  val default: PostgreSqlPhysicalPortfolioLimits =
    new PostgreSqlPhysicalPortfolioLimits(
      maxStructuralCandidates = MaxStructuralCandidates,
      maxRootsPerCandidate = MaxRootsPerCandidate,
      maxCoversPerBag = MaxCoversPerBag,
      maxCoverStatesPerCandidate = MaxCoverStatesPerCandidate,
      maxVariantsPerCandidate = MaxVariantsPerCandidate,
      maxRawVariantsPerSession = MaxRawVariantsPerSession,
      maxUniqueWorkloadsPerSession = MaxUniqueWorkloadsPerSession
    )

  def create(
      maxStructuralCandidates: Int = default.maxStructuralCandidates,
      maxRootsPerCandidate: Int = default.maxRootsPerCandidate,
      maxCoversPerBag: Int = default.maxCoversPerBag,
      maxCoverStatesPerCandidate: Int = default.maxCoverStatesPerCandidate,
      maxVariantsPerCandidate: Int = default.maxVariantsPerCandidate,
      maxRawVariantsPerSession: Int = default.maxRawVariantsPerSession,
      maxUniqueWorkloadsPerSession: Int = default.maxUniqueWorkloadsPerSession
  ): Either[
    Vector[PostgreSqlPhysicalPortfolioLimitError],
    PostgreSqlPhysicalPortfolioLimits
  ] = {
    val candidate = new PostgreSqlPhysicalPortfolioLimits(
      maxStructuralCandidates,
      maxRootsPerCandidate,
      maxCoversPerBag,
      maxCoverStatesPerCandidate,
      maxVariantsPerCandidate,
      maxRawVariantsPerSession,
      maxUniqueWorkloadsPerSession
    )
    val errors = validate(candidate)
    if (errors.nonEmpty) Left(errors) else Right(candidate)
  }

  private[postgresql] def validate(
      limits: PostgreSqlPhysicalPortfolioLimits
  ): Vector[PostgreSqlPhysicalPortfolioLimitError] = {
    import PostgreSqlPhysicalPortfolioLimitError.InvalidValue
    if (limits == null)
      return Vector(InvalidValue("limits", 0, "non-null"))
    Vector(
      Option.when(
        limits.maxStructuralCandidates != MaxStructuralCandidates
      )(
        InvalidValue(
          "structural-candidates",
          limits.maxStructuralCandidates,
          s"exactly $MaxStructuralCandidates"
        )
      ),
      bounded(
        "roots-per-candidate",
        limits.maxRootsPerCandidate,
        1,
        MaxRootsPerCandidate
      ),
      bounded("covers-per-bag", limits.maxCoversPerBag, 1, MaxCoversPerBag),
      bounded(
        "cover-states-per-candidate",
        limits.maxCoverStatesPerCandidate,
        MinCoverStatesPerCandidate,
        MaxCoverStatesPerCandidate
      ),
      bounded(
        "variants-per-candidate",
        limits.maxVariantsPerCandidate,
        MinVariantsPerCandidate,
        MaxVariantsPerCandidate
      ),
      bounded(
        "raw-variants-per-session",
        limits.maxRawVariantsPerSession,
        MinRawVariantsPerSession,
        MaxRawVariantsPerSession
      ),
      Option.when(
        limits.maxUniqueWorkloadsPerSession !=
          limits.maxRawVariantsPerSession
      )(
        InvalidValue(
          "unique-workloads-per-session",
          limits.maxUniqueWorkloadsPerSession,
          "equal to raw-variants-per-session"
        )
      ),
      bounded(
        "unique-workloads-per-session",
        limits.maxUniqueWorkloadsPerSession,
        MinRawVariantsPerSession,
        MaxUniqueWorkloadsPerSession
      )
    ).flatten
  }

  private def bounded(
      field: String,
      value: Int,
      minimum: Int,
      maximum: Int
  ): Option[PostgreSqlPhysicalPortfolioLimitError] =
    Option.when(value < minimum || value > maximum)(
      PostgreSqlPhysicalPortfolioLimitError.InvalidValue(
        field,
        value,
        s"$minimum <= value <= $maximum"
      )
    )
}

object PostgreSqlPhysicalPortfolioEnumerationPhase {
  val Anchor: String = "anchor"
  val AxisSentinel: String = "axis-sentinel"
  val CandidateCoverage: String = "candidate-coverage"
  val CrossProductReserve: String = "cross-product-reserve"
  val RemainingPrimaryAxis: String = "remaining-primary-axis"
  val ResidualHeap: String = "residual-heap"

  val Values: Set[String] = Set(
    Anchor,
    AxisSentinel,
    CandidateCoverage,
    CrossProductReserve,
    RemainingPrimaryAxis,
    ResidualHeap
  )
}

object PostgreSqlPhysicalPortfolioAxis {
  val BottomUp: String = "bottom-up"
  val NoReduction: String = "no-reduction"
  val Materialized: String = "materialized"
  val NotMaterialized: String = "not-materialized"
  val AlternateRoot: String = "alternate-root"
  val AlternateCover: String = "alternate-cover"
  val Values: Vector[String] = Vector(
    BottomUp,
    NoReduction,
    Materialized,
    NotMaterialized,
    AlternateRoot,
    AlternateCover
  )
}

/** Persisted exact heap key, using the cover index vector rather than rank. */
final case class PostgreSqlPhysicalVariantHeapKey private[postgresql] (
    deviationMagnitude: BigInt,
    rootOrdinal: Int,
    coverIndexVector: Vector[Int],
    modeOrdinal: Int,
    directiveOrdinal: Int,
    physicalVariantId: PostgreSqlPhysicalVariantId
) extends Serializable {
  require(deviationMagnitude >= 0)
  require(rootOrdinal >= 0)
  require(
    coverIndexVector != null && coverIndexVector.nonEmpty &&
      coverIndexVector.forall(_ >= 0)
  )
  require(modeOrdinal >= 0)
  require(directiveOrdinal >= 0)
  require(physicalVariantId != null)
  require(
    deviationMagnitude ==
      BigInt(rootOrdinal) + coverIndexVector.foldLeft(BigInt(0))(_ + _) +
        BigInt(modeOrdinal) + BigInt(directiveOrdinal),
    "physical heap magnitude must equal the exact coordinate deviation"
  )
}

/** One exact physical variant over a retained structural candidate. */
final case class PostgreSqlPhysicalVariant private[postgresql] (
    candidateId: CandidatePlanId,
    structuralRank: Int,
    configuration: BagFactorPhysicalConfiguration,
    reductionMode: YannakakisReductionMode,
    reductionOrder: String,
    materialization: PostgreSqlCteMaterializationDirective,
    materializationScope: String,
    physicalVariantId: PostgreSqlPhysicalVariantId,
    localOrdinal: Int
) extends Serializable {
  require(candidateId != null, "portfolio candidate id must not be null")
  require(structuralRank > 0, "portfolio structural rank must be positive")
  require(configuration != null, "portfolio configuration must not be null")
  require(reductionMode != null, "portfolio reduction mode must not be null")
  require(
    reductionOrder == PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
    "portfolio reduction order must be canonical"
  )
  require(materialization != null, "portfolio materialization must not be null")
  require(
    materializationScope == PostgreSqlCteMaterializationDirective.Scope,
    "portfolio materialization scope must be frozen"
  )
  require(physicalVariantId != null, "portfolio physical id must not be null")
  require(
    physicalVariantId == PostgreSqlPhysicalVariantId.fromV2(
      candidateId,
      configuration,
      reductionMode,
      materialization
    ),
    "portfolio physical id must match the exact configuration"
  )
  require(localOrdinal >= 0, "portfolio local ordinal must not be negative")
}

final case class PostgreSqlPhysicalPortfolioEntry private[postgresql] (
    enumerationOrdinal: Int,
    variant: PostgreSqlPhysicalVariant,
    phase: String,
    lane: Option[String],
    heapKey: PostgreSqlPhysicalVariantHeapKey
) extends Serializable {
  require(enumerationOrdinal >= 0, "enumeration ordinal must not be negative")
  require(variant != null, "enumerated physical variant must not be null")
  require(
    PostgreSqlPhysicalPortfolioEnumerationPhase.Values(phase),
    "enumeration phase must be frozen"
  )
  require(lane != null && lane.forall(value => value != null && value.nonEmpty))
  require(heapKey != null)
  require(heapKey.physicalVariantId == variant.physicalVariantId)
}

/** Exact counts and bounded-prefix state for one structural candidate. */
final case class PostgreSqlPhysicalCandidateEnumerationEvidence private[postgresql] (
    candidateId: CandidatePlanId,
    structuralRank: Int,
    exactRootCount: BigInt,
    exactCoverAssignmentCount: BigInt,
    exactLogicalVariantCount: BigInt,
    applicableAxes: Vector[String],
    retainedCoverStateCount: Int,
    coverStateSearchExhausted: Boolean,
    retainedLocalVariantCount: Int,
    retainedCrossProductStateCount: Int,
    localVariantSearchExhausted: Boolean
) extends Serializable {
  require(candidateId != null)
  require(structuralRank > 0)
  require(exactRootCount > 0)
  require(exactCoverAssignmentCount > 0)
  require(exactLogicalVariantCount > 0)
  require(
    applicableAxes != null && applicableAxes.nonEmpty &&
      applicableAxes ==
        PostgreSqlPhysicalPortfolioAxis.Values.filter(applicableAxes.toSet),
    "applicable axes must be a non-empty canonical subset"
  )
  require(retainedCoverStateCount > 0)
  require(retainedLocalVariantCount > 0)
  require(
    retainedCrossProductStateCount >= 0 &&
      retainedCrossProductStateCount <= retainedLocalVariantCount,
    "retained cross-product count must describe the retained local prefix"
  )
}

final case class PostgreSqlPhysicalPortfolioEnumeration private[postgresql] (
    entries: Vector[PostgreSqlPhysicalPortfolioEntry],
    candidates: Vector[PostgreSqlPhysicalCandidateEnumerationEvidence],
    exactLogicalVariantCount: BigInt,
    crossProductReserveTarget: Int,
    globalWaveSearchExhausted: Boolean,
    limits: PostgreSqlPhysicalPortfolioLimits
) extends Serializable {
  require(entries != null && entries.nonEmpty && entries.forall(_ != null))
  require(entries.map(_.enumerationOrdinal) == entries.indices.toVector)
  require(entries.map(_.variant.physicalVariantId).distinct.size == entries.size)
  require(candidates != null && candidates.nonEmpty && candidates.forall(_ != null))
  require(exactLogicalVariantCount == candidates.map(_.exactLogicalVariantCount).sum)
  require(
    crossProductReserveTarget == math.min(
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.CrossProductReserve,
      candidates.map(_.retainedCrossProductStateCount).sum
    ),
    "cross-product reserve must be derived from retained local prefixes"
  )
  require(limits != null)
  require(entries.size <= limits.maxRawVariantsPerSession)
  private val anchors = entries.take(candidates.size).map(_.variant)
  require(
    anchors.map(value => value.structuralRank -> value.candidateId) ==
      candidates.map(value => value.structuralRank -> value.candidateId) &&
      candidates.map(_.candidateId).distinct.size == candidates.size &&
      anchors.forall(_.localOrdinal == 0) &&
      entries.take(candidates.size).forall(
        _.phase == PostgreSqlPhysicalPortfolioEnumerationPhase.Anchor
      ),
    "every retained structural candidate must publish its anchor first"
  )
  require(
    entries.groupBy(_.variant.candidateId).values.forall(values =>
      values.map(_.variant.localOrdinal).sorted == values.indices.toVector
    ),
    "per-candidate publication ordinals must be contiguous"
  )
  private val phaseOrder = Map(
    PostgreSqlPhysicalPortfolioEnumerationPhase.Anchor -> 0,
    PostgreSqlPhysicalPortfolioEnumerationPhase.AxisSentinel -> 1,
    PostgreSqlPhysicalPortfolioEnumerationPhase.CandidateCoverage -> 2,
    PostgreSqlPhysicalPortfolioEnumerationPhase.CrossProductReserve -> 3,
    PostgreSqlPhysicalPortfolioEnumerationPhase.RemainingPrimaryAxis -> 4,
    PostgreSqlPhysicalPortfolioEnumerationPhase.ResidualHeap -> 5
  )
  require(
    entries.map(entry => phaseOrder(entry.phase)) ==
      entries.map(entry => phaseOrder(entry.phase)).sorted,
    "physical portfolio phases must be contiguous and increasing"
  )
  private val expectedSentinelAxes =
    PostgreSqlPhysicalPortfolioAxis.Values.filter(axis =>
      candidates.exists(_.applicableAxes.contains(axis))
    )
  private val actualSentinelAxes = entries.collect {
    case entry
        if entry.phase ==
          PostgreSqlPhysicalPortfolioEnumerationPhase.AxisSentinel =>
      entry.lane.get
  }
  require(
    actualSentinelAxes == expectedSentinelAxes,
    "every globally applicable physical axis must have one sentinel"
  )
  require(
    entries.groupBy(_.variant.candidateId).size == candidates.size &&
      entries.groupBy(_.variant.candidateId).values.forall(_.size >= 2),
    "every structural candidate must retain its anchor and an alternative"
  )
  private val primaryCoverageCount = entries.count(entry =>
    Set(
      PostgreSqlPhysicalPortfolioEnumerationPhase.Anchor,
      PostgreSqlPhysicalPortfolioEnumerationPhase.AxisSentinel,
      PostgreSqlPhysicalPortfolioEnumerationPhase.CandidateCoverage
    )(entry.phase)
  )
  require(
    primaryCoverageCount <= 2 * candidates.size + 5 &&
      primaryCoverageCount <= 205,
    "anchor, axis, and candidate coverage must satisfy the frozen cap proof"
  )
  require(
    entries.count(
      _.phase ==
        PostgreSqlPhysicalPortfolioEnumerationPhase.CrossProductReserve
    ) == crossProductReserveTarget,
    "cross-product reserve must publish the retained-prefix target"
  )
}

sealed trait PostgreSqlPhysicalPortfolioEnumerationFailure
    extends Product
    with Serializable {
  def code: String
  def message: String
}

object PostgreSqlPhysicalPortfolioEnumerationFailure {
  final case class MissingValue(path: String)
      extends PostgreSqlPhysicalPortfolioEnumerationFailure {
    override val code: String = "physical-portfolio.input.missing"
    override val message: String = s"Missing $path"
  }
  final case class InvalidCandidateSet(detail: String)
      extends PostgreSqlPhysicalPortfolioEnumerationFailure {
    override val code: String = "physical-portfolio.candidate-set.invalid"
    override val message: String = detail
  }
  final case class InvalidLimits(
      errors: Vector[PostgreSqlPhysicalPortfolioLimitError]
  ) extends PostgreSqlPhysicalPortfolioEnumerationFailure {
    override val code: String = "physical-portfolio.limits.invalid"
    override val message: String =
      s"Physical portfolio limits are invalid (${errors.size} errors)"
  }
  final case class ResourceLimitExceeded(
      resource: PostgreSqlPhysicalPortfolioResource,
      candidateId: Option[CandidatePlanId],
      bagId: Option[BagId],
      required: BigInt,
      limit: Int
  ) extends PostgreSqlPhysicalPortfolioEnumerationFailure {
    override val code: String =
      s"physical-portfolio.resource.exceeded.${resource.code}"
    override val message: String =
      s"Physical portfolio ${resource.code} requires $required, exceeding $limit"
  }
  final case class MissingAdmissibleCovers(
      candidateId: CandidatePlanId,
      bagId: BagId
  ) extends PostgreSqlPhysicalPortfolioEnumerationFailure {
    override val code: String = "physical-portfolio.cover.missing"
    override val message: String =
      s"Candidate ${candidateId.value} bag ${bagId.value} has no admissible cover set"
  }
  final case class StructuralCoverNotAdmissible(
      candidateId: CandidatePlanId,
      bagId: BagId
  ) extends PostgreSqlPhysicalPortfolioEnumerationFailure {
    override val code: String = "physical-portfolio.cover.structural-not-admissible"
    override val message: String =
      s"Candidate ${candidateId.value} bag ${bagId.value} uses a cover outside the admissible set"
  }
}

/** Deterministic, bounded enumeration after exact structural top-K search. */
object PostgreSqlPhysicalPortfolioEnumerator {
  import PostgreSqlPhysicalPortfolioEnumerationFailure._
  import PostgreSqlPhysicalPortfolioResource._

  private val CrossProductReserve =
    PostgreSqlExplainFactorAwarePhysicalPortfolioV1.CrossProductReserve

  def enumerate(
      input: CandidateSearchInput,
      plans: Vector[RankedCandidatePlan],
      limits: PostgreSqlPhysicalPortfolioLimits =
        PostgreSqlPhysicalPortfolioLimits.default
  ): Either[
    PostgreSqlPhysicalPortfolioEnumerationFailure,
    PostgreSqlPhysicalPortfolioEnumeration
  ] = {
    if (input == null) return Left(MissingValue("candidate-search input"))
    if (plans == null) return Left(MissingValue("retained structural plans"))
    if (limits == null) return Left(MissingValue("physical portfolio limits"))
    val limitErrors = PostgreSqlPhysicalPortfolioLimits.validate(limits)
    if (limitErrors.nonEmpty) return Left(InvalidLimits(limitErrors))
    if (plans.isEmpty) return Left(InvalidCandidateSet("Retained plans are empty"))
    if (plans.exists(_ == null))
      return Left(InvalidCandidateSet("Retained plans contain null"))
    if (plans.map(_.rank) != (1 to plans.size).toVector)
      return Left(
        InvalidCandidateSet(
          "Retained structural ranks must be contiguous and increasing"
        )
      )
    if (
      plans.exists(plan =>
        plan.candidateId == null || plan.decomposition == null ||
          CandidatePlanId.from(plan.decomposition) != plan.candidateId
      ) || plans.map(_.candidateId).distinct.size != plans.size
    )
      return Left(
        InvalidCandidateSet(
          "Retained candidate identities must be unique and match their decompositions"
        )
      )
    if (plans.size > limits.maxStructuralCandidates)
      return Left(
        ResourceLimitExceeded(
          StructuralCandidates,
          None,
          None,
          BigInt(plans.size),
          limits.maxStructuralCandidates
        )
      )

    val localResults = Vector.newBuilder[LocalEnumeration]
    plans.foreach { plan =>
      enumerateCandidate(input, plan, limits) match {
        case Left(problem) => return Left(problem)
        case Right(value)  => localResults += value
      }
    }
    val locals = localResults.result()

    val entries = Vector.newBuilder[PostgreSqlPhysicalPortfolioEntry]
    val emitted = mutable.HashSet.empty[PostgreSqlPhysicalVariantId]
    val localPublicationCounts = mutable.Map.empty[CandidatePlanId, Int]
    var globalCount = 0

    def publish(
        value: LocalVariant,
        phase: String,
        lane: Option[String]
    ): Boolean = {
      if (
        globalCount >= limits.maxRawVariantsPerSession ||
        !emitted.add(value.variant.physicalVariantId)
      ) false
      else {
        val candidateId = value.variant.candidateId
        val localOrdinal = localPublicationCounts.getOrElse(candidateId, 0)
        localPublicationCounts.put(candidateId, localOrdinal + 1)
        entries += PostgreSqlPhysicalPortfolioEntry(
          globalCount,
          value.variant.copy(localOrdinal = localOrdinal),
          phase,
          lane,
          value.heapKey
        )
        globalCount += 1
        true
      }
    }

    // Phase 0: no physical exploration can displace a structural anchor.
    locals.foreach(local =>
      publish(
        local.anchor,
        PostgreSqlPhysicalPortfolioEnumerationPhase.Anchor,
        None
      )
    )

    // Phase 1: globally witness every raw-applicable physical axis.
    PhysicalAxis.Values.foreach { axis =>
      locals.iterator
        .flatMap(local => local.primarySeed(axis))
        .take(1)
        .foreach(value =>
          publish(
            value,
            PostgreSqlPhysicalPortfolioEnumerationPhase.AxisSentinel,
            Some(axis.code)
          )
        )
    }

    // Phase 2: every expandable candidate receives an alternative before any
    // candidate receives a general second alternative.
    locals.foreach { local =>
      if (localPublicationCounts.getOrElse(local.candidateId, 0) == 1) {
        local.rotatedPrimarySeeds.headOption.foreach { case (axis, value) =>
          publish(
            value,
            PostgreSqlPhysicalPortfolioEnumerationPhase.CandidateCoverage,
            Some(axis.code)
          )
        }
      }
    }

    val crossProductReserveTarget = math.min(
      CrossProductReserve,
      locals.map(_.crossProducts.size).sum
    )

    // Phase 3: a fixed quota keeps genuine cross-products reachable at K=100.
    var reservedCrossProducts = 0
    var crossOrdinal = 0
    var crossDone = false
    while (
      !crossDone &&
      reservedCrossProducts < crossProductReserveTarget &&
      globalCount < limits.maxRawVariantsPerSession
    ) {
      var found = false
      locals.foreach { local =>
        local.crossProducts.lift(crossOrdinal).foreach { value =>
          found = true
          if (
            reservedCrossProducts < crossProductReserveTarget &&
            publish(
              value,
              PostgreSqlPhysicalPortfolioEnumerationPhase.CrossProductReserve,
              Some("cross-product")
            )
          ) reservedCrossProducts += 1
        }
      }
      if (!found) crossDone = true else crossOrdinal += 1
    }

    // Phase 4: diagonal rounds over the remaining primary one-axis seeds.
    var primaryRound = 0
    var primaryDone = false
    while (!primaryDone && globalCount < limits.maxRawVariantsPerSession) {
      var found = false
      locals.foreach { local =>
        local.rotatedPrimarySeeds.lift(primaryRound).foreach {
          case (axis, value) =>
            found = true
            publish(
              value,
              PostgreSqlPhysicalPortfolioEnumerationPhase.RemainingPrimaryAxis,
              Some(axis.code)
            )
        }
      }
      if (!found) primaryDone = true else primaryRound += 1
    }

    // Phase 5: only residual heap states compete for any remaining capacity.
    var residualOrdinal = 0
    var residualDone = false
    while (!residualDone && globalCount < limits.maxRawVariantsPerSession) {
      var found = false
      locals.foreach { local =>
        local.residualHeap.lift(residualOrdinal).foreach { value =>
          found = true
          publish(
            value,
            PostgreSqlPhysicalPortfolioEnumerationPhase.ResidualHeap,
            Some("heap")
          )
        }
      }
      if (!found) residualDone = true else residualOrdinal += 1
    }

    val entryValues = entries.result()
    val allLocalRetained = locals.foldLeft(BigInt(0))(
      _ + _.retainedVariantCount
    )
    val globalExhausted =
      BigInt(entryValues.size) == allLocalRetained &&
        locals.forall(_.evidence.localVariantSearchExhausted)
    Right(
      PostgreSqlPhysicalPortfolioEnumeration(
        entryValues,
        locals.map(_.evidence),
        locals.map(_.evidence.exactLogicalVariantCount).sum,
        crossProductReserveTarget,
        globalExhausted,
        limits
      )
    )
  }

  private def enumerateCandidate(
      input: CandidateSearchInput,
      plan: RankedCandidatePlan,
      limits: PostgreSqlPhysicalPortfolioLimits
  ): Either[PostgreSqlPhysicalPortfolioEnumerationFailure, LocalEnumeration] = {
    val bags = plan.decomposition.bags.sortBy(_.id)
    if (bags.size > limits.maxRootsPerCandidate)
      return Left(
        ResourceLimitExceeded(
          RootsPerCandidate,
          Some(plan.candidateId),
          None,
          BigInt(bags.size),
          limits.maxRootsPerCandidate
        )
      )

    val orderedCovers = Vector.newBuilder[Vector[Vector[at.ac.tuwien.dbai.yannakakis.core.EdgeId]]]
    bags.foreach { bag =>
      val candidateBag = input.candidateBag(bag.vertices) match {
        case Some(value) => value
        case None =>
          return Left(MissingAdmissibleCovers(plan.candidateId, bag.id))
      }
      if (candidateBag.physicalCovers.isEmpty)
        return Left(MissingAdmissibleCovers(plan.candidateId, bag.id))
      if (candidateBag.physicalCovers.size > limits.maxCoversPerBag)
        return Left(
          ResourceLimitExceeded(
            CoversPerBag,
            Some(plan.candidateId),
            Some(bag.id),
            BigInt(candidateBag.physicalCovers.size),
            limits.maxCoversPerBag
          )
        )
      val admissible = candidateBag.physicalCovers.map(_.edgeIds)
      if (!admissible.contains(bag.cover))
        return Left(StructuralCoverNotAdmissible(plan.candidateId, bag.id))
      val canonical = admissible.distinct.sortWith(compareEdgeVectors(_, _) < 0)
      orderedCovers += (bag.cover +: canonical.filterNot(_ == bag.cover))
    }
    val coversByBag = orderedCovers.result()
    val exactCoverCount = coversByBag.foldLeft(BigInt(1))(
      (product, covers) => product * covers.size
    )
    val coverStates = enumerateCoverStates(
      coversByBag.map(_.size),
      limits.maxCoverStatesPerCandidate
    )

    val roots = plan.decomposition.root +:
      bags.map(_.id).filterNot(_ == plan.decomposition.root)
    val exactLogicalCount =
      BigInt(roots.size) * exactCoverCount *
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.ReductionModeOrder.size *
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.MaterializationDirectiveOrder.size
    val local = enumerateLocalVariants(
      plan,
      bags.map(_.id),
      coversByBag,
      coverStates.states,
      roots,
      limits.maxVariantsPerCandidate
    )
    val localSearchExhausted = coverStates.exhausted && local.exhausted
    val evidence = PostgreSqlPhysicalCandidateEnumerationEvidence(
      plan.candidateId,
      plan.rank,
      BigInt(roots.size),
      exactCoverCount,
      exactLogicalCount,
      local.primarySeeds.map(_._1.code),
      coverStates.states.size,
      coverStates.exhausted,
      local.retainedVariantCount,
      local.retainedCrossProductStateCount,
      localSearchExhausted
    )
    Right(
      LocalEnumeration(
        plan.candidateId,
        plan.rank,
        local.anchor,
        local.primarySeeds,
        local.residualHeap,
        evidence
      )
    )
  }

  private def enumerateCoverStates(
      dimensionSizes: Vector[Int],
      limit: Int
  ): StateEnumeration = {
    val start = Vector.fill(dimensionSizes.size)(0)
    val pending = mutable.PriorityQueue(start)(coverStateOrdering)
    val seen = mutable.HashSet(start)
    val result = Vector.newBuilder[Vector[Int]]
    var count = 0
    while (pending.nonEmpty && count < limit) {
      val state = pending.dequeue()
      result += state
      count += 1
      state.indices.foreach { index =>
        if (state(index) + 1 < dimensionSizes(index)) {
          val next = state.updated(index, state(index) + 1)
          if (seen.add(next)) pending.enqueue(next)
        }
      }
    }
    StateEnumeration(result.result(), pending.isEmpty)
  }

  private def enumerateLocalVariants(
      plan: RankedCandidatePlan,
      bagIds: Vector[BagId],
      coversByBag: Vector[Vector[Vector[at.ac.tuwien.dbai.yannakakis.core.EdgeId]]],
      coverStates: Vector[Vector[Int]],
      roots: Vector[BagId],
      limit: Int
  ): VariantEnumeration = {
    val start = LocalState(0, 0, 0, 0)
    val seedStates = PhysicalAxis.Values.flatMap { axis =>
      axis.seedState(roots.size, coverStates.size).map(axis -> _)
    }
    val reservedStates = (Vector(start) ++ seedStates.map(_._2)).toSet
    val anchor = localVariantFrom(
      plan,
      bagIds,
      coversByBag,
      coverStates,
      roots,
      start
    )
    val primarySeeds = seedStates.map { case (axis, state) =>
      axis -> localVariantFrom(
        plan,
        bagIds,
        coversByBag,
        coverStates,
        roots,
        state
      )
    }
    val pending = mutable.PriorityQueue(start)(
      localStateOrdering(plan, bagIds, coversByBag, coverStates, roots)
    )
    val seen = mutable.HashSet(start)
    val residual = Vector.newBuilder[LocalVariant]
    var retainedCount = 1 + primarySeeds.size
    while (pending.nonEmpty && retainedCount < limit) {
      val state = pending.dequeue()
      if (!reservedStates(state)) {
        residual += localVariantFrom(
          plan,
          bagIds,
          coversByBag,
          coverStates,
          roots,
          state
        )
        retainedCount += 1
      }
      val maxima = Vector(
        roots.size,
        coverStates.size,
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.ReductionModeOrder.size,
        PostgreSqlExplainFactorAwarePhysicalPortfolioV1.MaterializationDirectiveOrder.size
      )
      var dimension = 0
      while (dimension < 4) {
        val values = state.values
        if (values(dimension) + 1 < maxima(dimension)) {
          val next = LocalState.from(values.updated(dimension, values(dimension) + 1))
          if (seen.add(next)) pending.enqueue(next)
        }
        dimension += 1
      }
    }
    VariantEnumeration(
      anchor,
      primarySeeds,
      residual.result(),
      retainedCount,
      pending.isEmpty
    )
  }

  private def localVariantFrom(
      plan: RankedCandidatePlan,
      bagIds: Vector[BagId],
      coversByBag: Vector[Vector[Vector[at.ac.tuwien.dbai.yannakakis.core.EdgeId]]],
      coverStates: Vector[Vector[Int]],
      roots: Vector[BagId],
      state: LocalState
  ): LocalVariant = {
    val coverState = coverStates(state.coverOrdinal)
    val assignments = bagIds.indices.map { index =>
      BagFactorCoverAssignment(
        bagIds(index),
        coversByBag(index)(coverState(index))
      )
    }.toVector
    val configuration = BagFactorPhysicalConfiguration(
      roots(state.rootOrdinal),
      assignments
    )
    val mode =
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.ReductionModeOrder(
        state.modeOrdinal
      )
    val directive =
      PostgreSqlExplainFactorAwarePhysicalPortfolioV1.MaterializationDirectiveOrder(
        state.directiveOrdinal
      )
    val variant = PostgreSqlPhysicalVariant(
      plan.candidateId,
      plan.rank,
      configuration,
      mode,
      PostgreSqlPhysicalVariantId.CanonicalReductionOrder,
      directive,
      PostgreSqlCteMaterializationDirective.Scope,
      PostgreSqlPhysicalVariantId.fromV2(
        plan.candidateId,
        configuration,
        mode,
        directive
      ),
      0
    )
    LocalVariant(
      variant,
      state,
      PostgreSqlPhysicalVariantHeapKey(
        deviationMagnitude =
          BigInt(state.rootOrdinal) +
            coverState.foldLeft(BigInt(0))(_ + _) +
            BigInt(state.modeOrdinal) + BigInt(state.directiveOrdinal),
        rootOrdinal = state.rootOrdinal,
        coverIndexVector = coverState,
        modeOrdinal = state.modeOrdinal,
        directiveOrdinal = state.directiveOrdinal,
        physicalVariantId = variant.physicalVariantId
      )
    )
  }

  private val coverStateOrdering: Ordering[Vector[Int]] =
    new Ordering[Vector[Int]] {
      override def compare(left: Vector[Int], right: Vector[Int]): Int = {
        val sum = right.foldLeft(BigInt(0))(_ + _).compare(
          left.foldLeft(BigInt(0))(_ + _)
        )
        if (sum != 0) sum else -compareIntVectors(left, right)
      }
    }

  private def localStateOrdering(
      plan: RankedCandidatePlan,
      bagIds: Vector[BagId],
      coversByBag: Vector[Vector[Vector[at.ac.tuwien.dbai.yannakakis.core.EdgeId]]],
      coverStates: Vector[Vector[Int]],
      roots: Vector[BagId]
  ): Ordering[LocalState] = new Ordering[LocalState] {
    override def compare(left: LocalState, right: LocalState): Int = {
      val leftCover = coverStates(left.coverOrdinal)
      val rightCover = coverStates(right.coverOrdinal)
      val leftMagnitude =
        BigInt(left.rootOrdinal) + leftCover.foldLeft(BigInt(0))(_ + _) +
          BigInt(left.modeOrdinal) + BigInt(left.directiveOrdinal)
      val rightMagnitude =
        BigInt(right.rootOrdinal) + rightCover.foldLeft(BigInt(0))(_ + _) +
          BigInt(right.modeOrdinal) + BigInt(right.directiveOrdinal)
      val sum = rightMagnitude.compare(leftMagnitude)
      if (sum != 0) sum
      else {
        val root = Integer.compare(right.rootOrdinal, left.rootOrdinal)
        if (root != 0) root
        else {
          val cover = -compareIntVectors(leftCover, rightCover)
          if (cover != 0) cover
          else {
            val mode = Integer.compare(right.modeOrdinal, left.modeOrdinal)
            if (mode != 0) mode
            else {
              val directive = Integer.compare(
                right.directiveOrdinal,
                left.directiveOrdinal
              )
              if (directive != 0) directive
              else {
                val leftId = localVariantFrom(
                  plan,
                  bagIds,
                  coversByBag,
                  coverStates,
                  roots,
                  left
                ).variant.physicalVariantId.value
                val rightId = localVariantFrom(
                  plan,
                  bagIds,
                  coversByBag,
                  coverStates,
                  roots,
                  right
                ).variant.physicalVariantId.value
                rightId.compareTo(leftId)
              }
            }
          }
        }
      }
    }
  }

  private def compareIntVectors(left: Vector[Int], right: Vector[Int]): Int = {
    var index = 0
    while (index < left.size && index < right.size) {
      val compared = Integer.compare(left(index), right(index))
      if (compared != 0) return compared
      index += 1
    }
    Integer.compare(left.size, right.size)
  }

  private def compareEdgeVectors(
      left: Vector[at.ac.tuwien.dbai.yannakakis.core.EdgeId],
      right: Vector[at.ac.tuwien.dbai.yannakakis.core.EdgeId]
  ): Int = {
    var index = 0
    while (index < left.size && index < right.size) {
      val compared =
        at.ac.tuwien.dbai.yannakakis.core.EdgeId.ordering.compare(
          left(index),
          right(index)
        )
      if (compared != 0) return compared
      index += 1
    }
    Integer.compare(left.size, right.size)
  }

  private final case class LocalState(
      rootOrdinal: Int,
      coverOrdinal: Int,
      modeOrdinal: Int,
      directiveOrdinal: Int
  ) {
    val values: Vector[Int] =
      Vector(rootOrdinal, coverOrdinal, modeOrdinal, directiveOrdinal)
  }
  private object LocalState {
    def from(values: Vector[Int]): LocalState =
      LocalState(values(0), values(1), values(2), values(3))
  }
  private final case class StateEnumeration(
      states: Vector[Vector[Int]],
      exhausted: Boolean
  )
  private final case class LocalVariant(
      variant: PostgreSqlPhysicalVariant,
      state: LocalState,
      heapKey: PostgreSqlPhysicalVariantHeapKey
  ) {
    def deviatingAxisCount: Int =
      Vector(
        state.rootOrdinal > 0,
        heapKey.coverIndexVector.exists(_ > 0),
        state.modeOrdinal > 0,
        state.directiveOrdinal > 0
      ).count(identity)
  }
  private final case class VariantEnumeration(
      anchor: LocalVariant,
      primarySeeds: Vector[(PhysicalAxis, LocalVariant)],
      residualHeap: Vector[LocalVariant],
      retainedVariantCount: Int,
      exhausted: Boolean
  ) {
    val retainedCrossProductStateCount: Int =
      residualHeap.count(_.deviatingAxisCount >= 2)
  }
  private final case class LocalEnumeration(
      candidateId: CandidatePlanId,
      structuralRank: Int,
      anchor: LocalVariant,
      primarySeeds: Vector[(PhysicalAxis, LocalVariant)],
      residualHeap: Vector[LocalVariant],
      evidence: PostgreSqlPhysicalCandidateEnumerationEvidence
  ) {
    private val seedByAxis = primarySeeds.toMap
    def primarySeed(axis: PhysicalAxis): Option[LocalVariant] =
      seedByAxis.get(axis)
    val rotatedPrimarySeeds: Vector[(PhysicalAxis, LocalVariant)] = {
      if (primarySeeds.isEmpty) Vector.empty
      else {
        val offset = (structuralRank - 1) % primarySeeds.size
        primarySeeds.drop(offset) ++ primarySeeds.take(offset)
      }
    }
    val crossProducts: Vector[LocalVariant] =
      residualHeap.filter(_.deviatingAxisCount >= 2)
    val retainedVariantCount: Int =
      1 + primarySeeds.size + residualHeap.size
  }

  private sealed trait PhysicalAxis extends Product with Serializable {
    def code: String
    def seedState(rootCount: Int, coverStateCount: Int): Option[LocalState]
  }
  private object PhysicalAxis {
    case object BottomUp extends PhysicalAxis {
      override val code: String = PostgreSqlPhysicalPortfolioAxis.BottomUp
      override def seedState(
          rootCount: Int,
          coverStateCount: Int
      ): Option[LocalState] = Some(LocalState(0, 0, 1, 0))
    }
    case object NoReduction extends PhysicalAxis {
      override val code: String = PostgreSqlPhysicalPortfolioAxis.NoReduction
      override def seedState(
          rootCount: Int,
          coverStateCount: Int
      ): Option[LocalState] = Some(LocalState(0, 0, 2, 0))
    }
    case object Materialized extends PhysicalAxis {
      override val code: String = PostgreSqlPhysicalPortfolioAxis.Materialized
      override def seedState(
          rootCount: Int,
          coverStateCount: Int
      ): Option[LocalState] = Some(LocalState(0, 0, 0, 1))
    }
    case object NotMaterialized extends PhysicalAxis {
      override val code: String =
        PostgreSqlPhysicalPortfolioAxis.NotMaterialized
      override def seedState(
          rootCount: Int,
          coverStateCount: Int
      ): Option[LocalState] = Some(LocalState(0, 0, 0, 2))
    }
    case object AlternateRoot extends PhysicalAxis {
      override val code: String = PostgreSqlPhysicalPortfolioAxis.AlternateRoot
      override def seedState(
          rootCount: Int,
          coverStateCount: Int
      ): Option[LocalState] =
        if (rootCount > 1) Some(LocalState(1, 0, 0, 0)) else None
    }
    case object AlternateCover extends PhysicalAxis {
      override val code: String = PostgreSqlPhysicalPortfolioAxis.AlternateCover
      override def seedState(
          rootCount: Int,
          coverStateCount: Int
      ): Option[LocalState] =
        if (coverStateCount > 1) Some(LocalState(0, 1, 0, 0)) else None
    }

    val Values: Vector[PhysicalAxis] = Vector(
      BottomUp,
      NoReduction,
      Materialized,
      NotMaterialized,
      AlternateRoot,
      AlternateCover
    )
  }
}
