package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.BagFactorPlan
import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  ColumnId,
  EdgeId,
  RelationOccurrenceId,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.ColumnRef

import scala.collection.mutable

/**
  * Exact set-safe terminal plan for a certified global MIN/MAX.
  *
  * A handle denotes rows of one anchor occurrence which extend to witnesses
  * over every occurrence in `consumedEdgeIds`. A base handle consumes only its
  * anchor. A staged handle is produced by retaining distinct anchor bindings
  * for which every existential input joins. DISTINCT is intentional: besides
  * being valid for existential MIN/MAX support, each stage selected by a
  * partial/no-reduction terminal is emitted as a terminal-owned MATERIALIZED
  * CTE. That makes the support execute once and prevents PostgreSQL from
  * parameterizing it under outer guard rows or flattening it back into the
  * multiplicity-producing source join. Full reduction retains the same exact
  * plan proof but can read the fully reduced guard-owner factor directly.
  *
  * Stages are derived bottom-up from the decomposition re-rooted at the guard
  * owner. A subtree is collapsed only when one anchor carries its complete
  * boundary separator. Otherwise its already-reduced handles are deferred to
  * the ancestor. Thus the lowering never materializes a separator cross
  * product merely to make a subtree set-valued.
  */
private[postgresql] object PostgreSqlGuardedExtremumSemijoin {
  sealed trait HandleSource extends Product with Serializable

  object HandleSource {
    final case class Base(edgeId: EdgeId) extends HandleSource
    final case class Stage(ordinal: Int) extends HandleSource
  }

  final case class Handle private[postgresql] (
      source: HandleSource,
      anchorEdgeId: EdgeId,
      variables: Vector[VariableId],
      consumedEdgeIds: Vector[EdgeId]
  ) extends Serializable {
    require(source != null, "guarded-extremum handle source must not be null")
    require(anchorEdgeId != null, "guarded-extremum anchor must not be null")
    require(
      variables != null && variables.forall(_ != null) &&
        variables == variables.distinct.sorted,
      "guarded-extremum handle variables must be canonical"
    )
    require(
      consumedEdgeIds != null && consumedEdgeIds.nonEmpty &&
        consumedEdgeIds.forall(_ != null) &&
        consumedEdgeIds == consumedEdgeIds.distinct.sorted &&
        consumedEdgeIds.contains(anchorEdgeId),
      "guarded-extremum consumed edges must be canonical and contain the anchor"
    )
  }

  /** One ordinary-equality link between two handles in a stage input vector. */
  final case class Equality private[postgresql] (
      variableId: VariableId,
      leftInput: Int,
      rightInput: Int
  ) extends Serializable {
    require(variableId != null, "semijoin equality variable must not be null")
    require(leftInput >= 0, "semijoin equality left input must not be negative")
    require(
      rightInput > leftInput,
      "semijoin equality inputs must be increasing"
    )
  }

  final case class Stage private[postgresql] (
      ordinal: Int,
      logicalBagId: BagId,
      anchorInput: Handle,
      existentialInputs: Vector[Handle],
      equalities: Vector[Equality],
      output: Handle
  ) extends Serializable {
    require(ordinal >= 0, "guarded-extremum stage ordinal must not be negative")
    require(logicalBagId != null, "guarded-extremum stage bag must not be null")
    require(anchorInput != null, "guarded-extremum stage anchor must not be null")
    require(
      existentialInputs != null && existentialInputs.nonEmpty &&
        existentialInputs.forall(_ != null),
      "guarded-extremum stage existential inputs must not be empty or contain null"
    )
    require(
      equalities != null && equalities.forall(_ != null),
      "guarded-extremum stage equalities must not contain null"
    )
    require(output != null, "guarded-extremum stage output must not be null")
    require(
      output.source == HandleSource.Stage(ordinal) &&
        output.anchorEdgeId == anchorInput.anchorEdgeId &&
        output.variables == anchorInput.variables,
      "guarded-extremum stage output must retain its anchor identity"
    )
  }

  final case class Plan private[postgresql] (
      guardEdgeId: EdgeId,
      guardOccurrenceId: RelationOccurrenceId,
      guardColumnId: ColumnId,
      guardOwner: BagId,
      stages: Vector[Stage],
      terminalHandles: Vector[Handle],
      terminalEqualities: Vector[Equality],
      sourceEdgeIds: Vector[EdgeId]
  ) extends Serializable {
    require(guardEdgeId != null, "guard edge must not be null")
    require(guardOccurrenceId != null, "guard occurrence must not be null")
    require(guardColumnId != null, "guard column must not be null")
    require(guardOwner != null, "guard owner must not be null")
    require(
      stages != null && stages.forall(_ != null) &&
        stages.map(_.ordinal) == stages.indices,
      "guarded-extremum stages must have canonical dense ordinals"
    )
    require(
      terminalHandles != null && terminalHandles.nonEmpty &&
        terminalHandles.forall(_ != null),
      "guarded-extremum terminal handles must not be empty or contain null"
    )
    require(
      terminalHandles.head.source == HandleSource.Base(guardEdgeId),
      "guarded-extremum terminal must retain the base guard as its first handle"
    )
    require(
      terminalEqualities != null && terminalEqualities.forall(_ != null),
      "guarded-extremum terminal equalities must not contain null"
    )
    require(
      sourceEdgeIds != null && sourceEdgeIds.nonEmpty &&
        sourceEdgeIds.forall(_ != null) &&
        sourceEdgeIds == sourceEdgeIds.distinct.sorted,
      "source edge ids must be one canonical non-empty set"
    )

    /** Terms emitted in addition to the aggregate and retained predicates. */
    val structuralRenderedTermCount: BigInt = {
      val stageTerms = stages.iterator.map { stage =>
        BigInt(math.max(1, stage.output.variables.size)) +
          BigInt(1) + BigInt(stage.equalities.size)
      }.foldLeft(BigInt(0))(_ + _)
      val terminalTerms =
        if (terminalHandles.size > 1)
          BigInt(1) + BigInt(terminalEqualities.size)
        else BigInt(0)
      stageTerms + terminalTerms
    }

    /** Maximum staged-support dependency depth at the terminal. */
    lazy val maximumStageNestingDepth: Int = {
      val depthByOrdinal = Array.fill(stages.size)(0)
      def handleDepth(handle: Handle): Int = handle.source match {
        case _: HandleSource.Base          => 0
        case HandleSource.Stage(ordinal) => depthByOrdinal(ordinal)
      }
      stages.foreach { stage =>
        val inputDepth = (stage.anchorInput +: stage.existentialInputs).iterator
          .map(handleDepth)
          .foldLeft(0)(math.max)
        depthByOrdinal(stage.ordinal) = inputDepth + 1
      }
      terminalHandles.iterator.map(handleDepth).foldLeft(0)(math.max)
    }
  }

  sealed abstract class ErrorCode(val value: String)
      extends Product
      with Serializable

  object ErrorCode {
    case object MissingValue extends ErrorCode("missing-value")
    case object GuardEdgeMismatch extends ErrorCode("guard-edge-mismatch")
    case object GuardOwnerMismatch extends ErrorCode("guard-owner-mismatch")
    case object EdgeCoverageMismatch extends ErrorCode("edge-coverage-mismatch")
    case object DecompositionMismatch extends ErrorCode("decomposition-mismatch")
    case object CandidateMismatch extends ErrorCode("candidate-mismatch")
    final case class NestedStageDepthExceeded(required: Int, limit: Int)
        extends ErrorCode("nested-stage-depth-exceeded")
  }

  final case class Error(code: ErrorCode, detail: String)
      extends Product
      with Serializable {
    def message: String = s"${code.value}: $detail"
  }

  def derive(
      source: BagFactorPlan,
      guard: ColumnRef,
      guardOwner: BagId
  ): Either[Vector[Error], Plan] =
    deriveWithMaximumNestingDepth(source, guard, guardOwner, Int.MaxValue)

  /**
    * Derives the canonical program while bounding recursive SQL depth. Once
    * the bound is crossed, coverage vectors become constant-size placeholders
    * so an adversarial deep chain is still diagnosed in linear space. Anchor
    * choice depends only on handle variables and provenance, not coverage.
    */
  def deriveWithMaximumNestingDepth(
      source: BagFactorPlan,
      guard: ColumnRef,
      guardOwner: BagId,
      maximumNestingDepth: Int
  ): Either[Vector[Error], Plan] = {
    require(maximumNestingDepth >= 0, "maximum nesting depth must not be negative")
    val missing = Vector.newBuilder[Error]
    if (source eq null)
      missing += Error(ErrorCode.MissingValue, "bag-factor plan")
    if (guard eq null)
      missing += Error(ErrorCode.MissingValue, "guard column")
    if (guardOwner eq null)
      missing += Error(ErrorCode.MissingValue, "guard owner")
    val missingResult = canonical(missing.result())
    if (missingResult.nonEmpty) return Left(missingResult)

    val edges = source.sourceHypergraph.edges
    val edgeById = edges.iterator.map(edge => edge.id -> edge).toMap
    val sourceEdgeIds = edges.map(_.id)
    val matchingGuardEdges = edges.filter(
      _.relationOccurrenceId == guard.relationOccurrenceId
    )
    if (matchingGuardEdges.size != 1) {
      return Left(
        Vector(
          Error(
            ErrorCode.GuardEdgeMismatch,
            s"guard occurrence ${guard.relationOccurrenceId.value} maps to ${matchingGuardEdges.size} edges"
          )
        )
      )
    }

    val guardEdge = matchingGuardEdges.head
    val guardOwnership = source.ownership.filter(_.edgeId == guardEdge.id)
    val bagIds = source.factors.map(_.bagId)
    val problems = Vector.newBuilder[Error]
    if (
      sourceEdgeIds.isEmpty ||
      sourceEdgeIds != sourceEdgeIds.distinct.sorted ||
      source.ownership.map(_.edgeId).sorted != sourceEdgeIds
    ) {
      problems += Error(
        ErrorCode.EdgeCoverageMismatch,
        "source edges or ownership are not one canonical complete occurrence set"
      )
    }
    if (
      guardOwnership.size != 1 ||
      guardOwnership.headOption.forall(_.owner != guardOwner) ||
      !bagIds.contains(guardOwner)
    ) {
      problems += Error(
        ErrorCode.GuardOwnerMismatch,
        s"edge ${guardEdge.id.value} is not uniquely owned by bag ${guardOwner.value}"
      )
    }
    if (
      bagIds.isEmpty || bagIds != bagIds.distinct.sorted ||
      source.decomposition.bagIds.sorted != bagIds ||
      source.decomposition.treeEdges.size != bagIds.size - 1
    ) {
      problems += Error(
        ErrorCode.DecompositionMismatch,
        "factor bags and the decomposition tree are not one canonical connected bag set"
      )
    }

    val initialProblems = canonical(problems.result())
    if (initialProblems.nonEmpty) return Left(initialProblems)

    val bagById = source.decomposition.bags.iterator.map(bag => bag.id -> bag).toMap
    val adjacency = mutable.Map.empty[BagId, mutable.ArrayBuffer[BagId]]
    bagIds.foreach(bagId => adjacency.put(bagId, mutable.ArrayBuffer.empty))
    source.decomposition.treeEdges.foreach { treeEdge =>
      adjacency(treeEdge.first) += treeEdge.second
      adjacency(treeEdge.second) += treeEdge.first
    }

    // Iterative re-rooting avoids making the renderer's stack depth depend on
    // an otherwise-valid decomposition depth.
    val parent = mutable.Map.empty[BagId, Option[BagId]]
    val depth = mutable.Map.empty[BagId, Int]
    val pending = mutable.Queue.empty[BagId]
    parent.put(guardOwner, None)
    depth.put(guardOwner, 0)
    pending.enqueue(guardOwner)
    while (pending.nonEmpty) {
      val current = pending.dequeue()
      adjacency(current).sorted.foreach { neighbor =>
        if (!parent.contains(neighbor)) {
          parent.put(neighbor, Some(current))
          depth.put(neighbor, depth(current) + 1)
          pending.enqueue(neighbor)
        }
      }
    }
    if (parent.size != bagIds.size) {
      return Left(
        Vector(
          Error(
            ErrorCode.DecompositionMismatch,
            "guard-rooted traversal does not reach every factor bag"
          )
        )
      )
    }

    val childBuffers = mutable.Map.empty[BagId, mutable.ArrayBuffer[BagId]]
    bagIds.foreach(bagId => childBuffers.put(bagId, mutable.ArrayBuffer.empty))
    parent.foreach {
      case (childId, Some(parentId)) => childBuffers(parentId) += childId
      case _                         => ()
    }
    val children = childBuffers.iterator.map { case (bagId, values) =>
      bagId -> values.sorted.toVector
    }.toMap
    val ownedByBag = source.ownership.groupBy(_.owner).view.mapValues(
      _.map(_.edgeId).sorted
    ).toMap
    val guardVariables = guardEdge.vertices.toSet
    val representations = mutable.Map.empty[BagId, Vector[Handle]]
    val stages = Vector.newBuilder[Stage]
    val stageDepths = mutable.ArrayBuffer.empty[Int]
    var nextStageOrdinal = 0
    var maximumObservedStageDepth = 0

    def baseHandle(edgeId: EdgeId): Handle = {
      val edge = edgeById(edgeId)
      Handle(
        HandleSource.Base(edgeId),
        edgeId,
        edge.vertices,
        Vector(edgeId)
      )
    }

    def handleSort(values: Vector[Handle]): Vector[Handle] =
      values.sortBy(handle => (
        handle.anchorEdgeId.value,
        handle.source match {
          case _: HandleSource.Base  => -1
          case HandleSource.Stage(n) => n
        }
      ))

    def separator(left: BagId, right: BagId): Vector[VariableId] =
      bagById(left).vertices.filter(bagById(right).vertices.toSet).sorted

    def chooseAnchor(
        handles: Vector[Handle],
        required: Vector[VariableId],
        parentSeparator: Vector[VariableId]
    ): Option[Handle] = {
      val requiredSet = required.toSet
      val uncoveredFrontier = parentSeparator.filterNot(guardVariables).toSet
      handles
        .filter(handle =>
          handle.anchorEdgeId != guardEdge.id &&
            requiredSet.subsetOf(handle.variables.toSet)
        )
        .sortBy(handle => (
          -handle.variables.count(uncoveredFrontier),
          handle.anchorEdgeId.value,
          handle.source match {
            case _: HandleSource.Base  => -1
            case HandleSource.Stage(n) => n
          }
        ))
        .headOption
    }

    def equalities(inputs: Vector[Handle]): Vector[Equality] = {
      val variables = inputs.iterator.flatMap(_.variables).toVector.distinct.sorted
      variables.flatMap { variableId =>
        val carrying = inputs.indices.filter(index =>
          inputs(index).variables.contains(variableId)
        ).toVector
        carrying.headOption.toVector.flatMap(first =>
          carrying.tail.map(other => Equality(variableId, first, other))
        )
      }
    }

    def handleDepth(handle: Handle): Int = handle.source match {
      case _: HandleSource.Base          => 0
      case HandleSource.Stage(ordinal) => stageDepths(ordinal)
    }

    def makeStage(
        bagId: BagId,
        anchor: Handle,
        existential: Vector[Handle]
    ): Handle = {
      val canonicalExistential = handleSort(existential)
      val inputs = anchor +: canonicalExistential
      val stageDepth =
        inputs.iterator.map(handleDepth).foldLeft(0)(math.max) + 1
      maximumObservedStageDepth =
        math.max(maximumObservedStageDepth, stageDepth)
      val consumed =
        if (maximumObservedStageDepth <= maximumNestingDepth) {
          inputs.iterator
            .flatMap(_.consumedEdgeIds)
            .toVector
            .distinct
            .sorted
        } else {
          // Coverage is not consulted by anchor selection. Retaining only the
          // anchor bounds memory after the terminal has already become
          // unrenderable, while the complete depth derivation continues.
          Vector(anchor.anchorEdgeId)
        }
      val output = Handle(
        HandleSource.Stage(nextStageOrdinal),
        anchor.anchorEdgeId,
        anchor.variables,
        consumed
      )
      stages += Stage(
        nextStageOrdinal,
        bagId,
        anchor,
        canonicalExistential,
        equalities(inputs),
        output
      )
      stageDepths += stageDepth
      nextStageOrdinal += 1
      output
    }

    val processingOrder = bagIds.sortBy(bagId => (-depth(bagId), bagId.value))
    processingOrder.foreach { bagId =>
      val parentSeparator = parent(bagId)
        .map(parentId => separator(bagId, parentId))
        .getOrElse(Vector.empty)
      var current = handleSort(
        ownedByBag.getOrElse(bagId, Vector.empty).map(baseHandle)
      )

      children(bagId).foreach { childId =>
        val childHandles = representations(childId)
        if (childHandles.nonEmpty) {
          val childSeparator = separator(bagId, childId)
          chooseAnchor(current, childSeparator, parentSeparator) match {
            case Some(anchor) =>
              val output = makeStage(bagId, anchor, childHandles)
              current = current.filterNot(_ == anchor) :+ output
            case None =>
              current = current ++ childHandles
          }
        }
      }

      parent(bagId) match {
        case Some(_) if current.size > 1 =>
          chooseAnchor(current, parentSeparator, parentSeparator) match {
            case Some(anchor) =>
              current = Vector(
                makeStage(bagId, anchor, current.filterNot(_ == anchor))
              )
            case None => ()
          }
        case _ => ()
      }
      representations.put(bagId, handleSort(current))
    }

    if (maximumObservedStageDepth > maximumNestingDepth) {
      return Left(
        Vector(
          Error(
            ErrorCode.NestedStageDepthExceeded(
              maximumObservedStageDepth,
              maximumNestingDepth
            ),
            s"guarded aggregate stage nesting depth $maximumObservedStageDepth exceeds fixed engine-safety ceiling $maximumNestingDepth"
          )
        )
      )
    }

    val rootHandles = representations(guardOwner)
    val guardHandle = rootHandles.find(
      _.source == HandleSource.Base(guardEdge.id)
    )
    if (guardHandle.isEmpty) {
      return Left(
        Vector(
          Error(
            ErrorCode.GuardOwnerMismatch,
            "guard base handle was consumed by a staged support"
          )
        )
      )
    }
    val terminalHandles = guardHandle.get +:
      handleSort(rootHandles.filterNot(_ == guardHandle.get))
    val covered = terminalHandles.iterator
      .flatMap(_.consumedEdgeIds)
      .toVector
      .sorted
    if (covered != sourceEdgeIds) {
      return Left(
        Vector(
          Error(
            ErrorCode.EdgeCoverageMismatch,
            "terminal handles do not consume each source occurrence exactly once"
          )
        )
      )
    }

    Right(
      Plan(
        guardEdge.id,
        guard.relationOccurrenceId,
        guard.id,
        guardOwner,
        stages.result(),
        terminalHandles,
        equalities(terminalHandles),
        sourceEdgeIds
      )
    )
  }

  /** Independently re-derives and compares every physical provenance field. */
  def validate(
      source: BagFactorPlan,
      guard: ColumnRef,
      guardOwner: BagId,
      candidate: Plan
  ): Either[Vector[Error], Plan] =
    derive(source, guard, guardOwner) match {
      case Left(problems) => Left(problems)
      case Right(_) if candidate eq null =>
        Left(Vector(Error(ErrorCode.MissingValue, "guard-row semijoin plan")))
      case Right(expected) if candidate != expected =>
        Left(
          Vector(
            Error(
              ErrorCode.CandidateMismatch,
              "guard-row semijoin plan differs from its canonical derivation"
            )
          )
        )
      case Right(_) => Right(candidate)
    }

  private def canonical(values: Vector[Error]): Vector[Error] =
    values.distinct.sortBy(value => (value.code.value, value.detail))
}
