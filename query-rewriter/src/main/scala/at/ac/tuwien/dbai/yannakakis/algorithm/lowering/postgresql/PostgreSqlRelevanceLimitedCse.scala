package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.candidate.CandidateSearchInput
import at.ac.tuwien.dbai.yannakakis.core.{
  ColumnId,
  EdgeId,
  Hyperedge,
  RelationOccurrenceId,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  PredicateClassification,
  QueryHypergraphConverter,
  QueryIR,
  QueryResultShape,
  RelationOccurrence
}

import scala.collection.mutable

/**
  * Pure detector for one deliberately narrow query-level common subexpression.
  *
  * Given an occurrence `a`, delete `a` and ignore its variables while computing
  * connectivity among the remaining occurrences. Components whose only
  * boundary is one variable `x` of `a` form the maximal pendant wing at `x`.
  * Two such wings may share one frequency relation exactly when their coloured
  * occurrence/variable incidence graphs are isomorphic with their boundary
  * variables identified. Relation names and bound column ordinals are colours;
  * relation occurrences and private variables are otherwise alpha-renamable.
  *
  * The detector intentionally has no SQL and does not inspect a CandidateTD.
  * A later additive renderer can use the retained CandidateTD as action
  * provenance, but this optimization is a query factorization rather than a
  * different realization of that decomposition. Current scope is equality-only
  * scalar global COUNT(*). In particular, local predicates are rejected until
  * their typed normalized form participates in the isomorphism colour.
  */
private[postgresql] object PostgreSqlRelevanceLimitedCse {

  final case class SearchLimits(
      maxAnchorOccurrences: Int,
      maxDetachedComponents: Int,
      maxWingOccurrences: Int,
      maxIsomorphismStates: Long
  ) extends Serializable {
    require(maxAnchorOccurrences > 0, "anchor-occurrence limit must be positive")
    require(maxDetachedComponents > 0, "component limit must be positive")
    require(maxWingOccurrences > 0, "wing-occurrence limit must be positive")
    require(maxIsomorphismStates > 0L, "isomorphism-state limit must be positive")
  }

  object SearchLimits {
    val default: SearchLimits = SearchLimits(
      maxAnchorOccurrences = 64,
      maxDetachedComponents = 256,
      maxWingOccurrences = 32,
      maxIsomorphismStates = 100000L
    )
  }

  sealed abstract class ErrorCode(val value: String)
      extends Product
      with Serializable

  object ErrorCode {
    case object MissingValue extends ErrorCode("missing-value")
    case object UnsupportedResultShape
        extends ErrorCode("unsupported-result-shape")
    case object UnsupportedPredicate extends ErrorCode("unsupported-predicate")
    case object QueryConversionFailed extends ErrorCode("query-conversion-failed")
    case object InputHypergraphMismatch
        extends ErrorCode("input-hypergraph-mismatch")
    case object UnsupportedKeyType extends ErrorCode("unsupported-key-type")
    case object ResourceLimitExceeded
        extends ErrorCode("resource-limit-exceeded")
    case object NoReusableWing extends ErrorCode("no-reusable-wing")
    case object CandidateMismatch extends ErrorCode("candidate-mismatch")
  }

  final case class Error(code: ErrorCode, detail: String)
      extends Product
      with Serializable {
    require(code != null, "CSE error code must not be null")
    require(detail != null && detail.nonEmpty, "CSE error detail must not be blank")
    def message: String = s"${code.value}: $detail"
  }

  /** One selected anchor column supplying the relevance domain for a wing. */
  final case class AnchorInterface private[postgresql] (
      variableId: VariableId,
      anchorColumnId: ColumnId
  ) extends Serializable {
    require(variableId != null, "anchor interface variable must not be null")
    require(anchorColumnId != null, "anchor interface column must not be null")
  }

  /** A maximal union of detached components with the same singleton boundary. */
  final case class Wing private[postgresql] (
      interfaceVariable: VariableId,
      edgeIds: Vector[EdgeId],
      privateVariables: Vector[VariableId]
  ) extends Serializable {
    require(interfaceVariable != null, "wing interface must not be null")
    require(edgeIds != null && edgeIds.nonEmpty, "wing edges must not be empty")
    require(edgeIds == edgeIds.distinct.sorted, "wing edges must be canonical")
    require(privateVariables != null, "wing variables must not be null")
    require(
      privateVariables == privateVariables.distinct.sorted &&
        !privateVariables.contains(interfaceVariable),
      "wing private variables must be canonical and exclude the interface"
    )
  }

  /** Exact representative-to-member alpha-renaming certificate. */
  final case class WingIsomorphism private[postgresql] (
      memberInterfaceVariable: VariableId,
      edgeMapping: Vector[(EdgeId, EdgeId)],
      variableMapping: Vector[(VariableId, VariableId)]
  ) extends Serializable {
    require(
      memberInterfaceVariable != null,
      "wing-isomorphism member interface must not be null"
    )
    require(edgeMapping != null && edgeMapping.nonEmpty, "edge mapping must not be empty")
    require(variableMapping != null && variableMapping.nonEmpty, "variable mapping must not be empty")
    require(
      edgeMapping.map(_._1) == edgeMapping.map(_._1).distinct.sorted &&
        edgeMapping.map(_._2).distinct.size == edgeMapping.size,
      "edge mapping must be a canonical injection"
    )
    require(
      variableMapping.map(_._1) ==
        variableMapping.map(_._1).distinct.sorted &&
        variableMapping.map(_._2).distinct.size == variableMapping.size,
      "variable mapping must be a canonical injection"
    )
  }

  final case class SharedWing private[postgresql] (
      representative: Wing,
      members: Vector[Wing],
      isomorphisms: Vector[WingIsomorphism]
  ) extends Serializable {
    require(representative != null, "shared-wing representative must not be null")
    require(members != null && members.size >= 2, "shared wing needs at least two members")
    require(
      members.map(_.interfaceVariable) ==
        members.map(_.interfaceVariable).distinct.sorted,
      "shared-wing members must be canonical"
    )
    require(members.head == representative, "representative must be the first member")
    require(
      isomorphisms != null &&
        isomorphisms.map(_.memberInterfaceVariable) ==
          members.map(_.interfaceVariable),
      "shared-wing isomorphisms must cover members in order"
    )
  }

  /** Complete SQL-free input to a future one-shared-CTE COUNT renderer. */
  final case class Plan private[postgresql] (
      anchorEdgeId: EdgeId,
      anchorInterfaces: Vector[AnchorInterface],
      sharedWing: SharedWing,
      coreEdgeIds: Vector[EdgeId],
      keySqlTypeName: String
  ) extends Serializable {
    require(anchorEdgeId != null, "CSE anchor edge must not be null")
    require(
      anchorInterfaces != null && anchorInterfaces.size >= 2,
      "CSE plan needs at least two anchor interfaces"
    )
    require(
      anchorInterfaces.map(_.variableId) ==
        anchorInterfaces.map(_.variableId).distinct.sorted,
      "CSE anchor interfaces must be canonical"
    )
    require(sharedWing != null, "CSE shared wing must not be null")
    require(
      anchorInterfaces.map(_.variableId) ==
        sharedWing.members.map(_.interfaceVariable),
      "CSE interfaces must equal shared-wing member interfaces"
    )
    require(
      coreEdgeIds != null &&
        coreEdgeIds.nonEmpty &&
        coreEdgeIds == coreEdgeIds.distinct.sorted &&
        coreEdgeIds.contains(anchorEdgeId),
      "CSE core edges must be canonical and contain the anchor"
    )
    require(
      keySqlTypeName != null && keySqlTypeName.nonEmpty,
      "CSE key SQL type must not be blank"
    )
  }

  def derive(
      query: QueryIR,
      input: CandidateSearchInput,
      limits: SearchLimits = SearchLimits.default
  ): Either[Vector[Error], Plan] = {
    if (query == null)
      return Left(Vector(Error(ErrorCode.MissingValue, "query")))
    if (input == null)
      return Left(Vector(Error(ErrorCode.MissingValue, "candidate-search input")))
    if (limits == null)
      return Left(Vector(Error(ErrorCode.MissingValue, "search limits")))
    if (!query.resultShape.isInstanceOf[QueryResultShape.GlobalCountStar])
      return Left(
        Vector(
          Error(
            ErrorCode.UnsupportedResultShape,
            "relevance-limited reuse requires scalar global COUNT(*)"
          )
        )
      )

    val unsupportedPredicates = query.predicates.filterNot(
      _.classification.isInstanceOf[PredicateClassification.InnerEquality]
    )
    if (unsupportedPredicates.nonEmpty)
      return Left(
        Vector(
          Error(
            ErrorCode.UnsupportedPredicate,
            "only inner equalities are currently coloured for wing reuse; " +
              s"unsupported predicate ordinals=${unsupportedPredicates.map(_.ordinal).mkString(",")}"
          )
        )
      )

    val expectedHypergraph = QueryHypergraphConverter.convert(query) match {
      case Left(problems) =>
        return Left(
          Vector(
            Error(
              ErrorCode.QueryConversionFailed,
              problems.map(problem => s"${problem.code.id}:${problem.message}").mkString("; ")
            )
          )
        )
      case Right(value) => value
    }
    if (input.hypergraph != expectedHypergraph)
      return Left(
        Vector(
          Error(
            ErrorCode.InputHypergraphMismatch,
            "candidate-search hypergraph differs from the QueryIR conversion"
          )
        )
      )

    val edges = expectedHypergraph.edges
    if (edges.size > limits.maxAnchorOccurrences)
      return resource(
        "anchor occurrences",
        edges.size.toLong,
        limits.maxAnchorOccurrences.toLong
      )

    val relationByOccurrence = query.relations.iterator
      .map(relation => relation.id -> relation)
      .toMap
    val metadataByColumn = query.relations.iterator
      .flatMap(_.columns.iterator.map(metadata => metadata.ref.id -> metadata))
      .toMap
    val budget = new IsomorphismBudget(limits.maxIsomorphismStates)
    val candidates = Vector.newBuilder[Candidate]

    edges.foreach { anchor =>
      detachedWings(anchor, edges, limits) match {
        case Left(problem) => return Left(Vector(problem))
        case Right(wings) if wings.size >= 2 =>
          partitionIsomorphic(
            wings,
            expectedHypergraph.edges.iterator.map(edge => edge.id -> edge).toMap,
            relationByOccurrence,
            budget
          ) match {
            case Left(problem) => return Left(Vector(problem))
            case Right(groups) =>
              groups.filter(_.members.size >= 2).foreach { group =>
                buildCandidate(
                  anchor,
                  group,
                  edges,
                  metadataByColumn
                ).foreach(candidates += _)
              }
          }
        case Right(_) => ()
      }
    }

    candidates.result().sortBy(_.orderingKey).headOption match {
      case None =>
        Left(
          Vector(
            Error(
              ErrorCode.NoReusableWing,
              "no anchor has two type-compatible isomorphic singleton-boundary wings"
            )
          )
        )
      case Some(candidate) => Right(candidate.plan)
    }
  }

  /** Independently re-derives every field; transported plans are not trusted. */
  def validate(
      query: QueryIR,
      input: CandidateSearchInput,
      candidate: Plan,
      limits: SearchLimits = SearchLimits.default
  ): Either[Vector[Error], Plan] =
    derive(query, input, limits) match {
      case Left(problems) => Left(problems)
      case Right(_) if candidate == null =>
        Left(Vector(Error(ErrorCode.MissingValue, "relevance-limited CSE plan")))
      case Right(expected) if expected != candidate =>
        Left(
          Vector(
            Error(
              ErrorCode.CandidateMismatch,
              "non-canonical relevance-limited CSE plan"
            )
          )
        )
      case Right(_) => Right(candidate)
    }

  private final case class EdgeShape(
      qualifiedName: Vector[String],
      relationColumns: Vector[(String, String, String, Boolean)],
      boundColumnOrdinals: Vector[Int]
  )

  private final case class MutableClass(
      representative: Wing,
      members: mutable.ArrayBuffer[Wing],
      isomorphisms: mutable.ArrayBuffer[WingIsomorphism]
  )

  private final case class Candidate(plan: Plan, orderingKey: (Int, Int, Int, Int))

  private final class IsomorphismBudget(val limit: Long) {
    var visited: Long = 0L

    def enter(): Boolean = {
      if (visited >= limit) false
      else {
        visited += 1L
        true
      }
    }
  }

  private def detachedWings(
      anchor: Hyperedge,
      allEdges: Vector[Hyperedge],
      limits: SearchLimits
  ): Either[Error, Vector[Wing]] = {
    val anchorVariables = anchor.vertices.toSet
    val remaining = allEdges.filterNot(_.id == anchor.id)
    val remainingById = remaining.iterator.map(edge => edge.id -> edge).toMap
    val byPrivateVariable = mutable.Map.empty[VariableId, Vector[EdgeId]]
    remaining.foreach { edge =>
      edge.vertices.filterNot(anchorVariables).foreach { variableId =>
        byPrivateVariable.update(
          variableId,
          (byPrivateVariable.getOrElse(variableId, Vector.empty) :+ edge.id).sorted
        )
      }
    }

    val visited = mutable.Set.empty[EdgeId]
    val components = Vector.newBuilder[Vector[Hyperedge]]
    remaining.sortBy(_.id).foreach { start =>
      if (!visited(start.id)) {
        val component = Vector.newBuilder[Hyperedge]
        val pending = mutable.Stack(start.id)
        while (pending.nonEmpty) {
          val edgeId = pending.pop()
          if (!visited(edgeId)) {
            visited += edgeId
            val edge = remainingById(edgeId)
            component += edge
            edge.vertices
              .filterNot(anchorVariables)
              .reverseIterator
              .flatMap(variableId =>
                byPrivateVariable.getOrElse(variableId, Vector.empty).reverseIterator
              )
              .foreach(neighbor => if (!visited(neighbor)) pending.push(neighbor))
          }
        }
        components += component.result().sortBy(_.id)
      }
    }
    val componentResult = components.result()
    if (componentResult.size > limits.maxDetachedComponents)
      return Left(
        Error(
          ErrorCode.ResourceLimitExceeded,
          s"detached components actual=${componentResult.size} limit=${limits.maxDetachedComponents}"
        )
      )

    val singletonBoundaryComponents = componentResult.flatMap { component =>
      val boundary = component.iterator
        .flatMap(_.vertices)
        .filter(anchorVariables)
        .toVector
        .distinct
        .sorted
      if (boundary.size == 1) Some(boundary.head -> component) else None
    }
    val wings = singletonBoundaryComponents
      .groupBy(_._1)
      .toVector
      .sortBy(_._1)
      .map { case (interface, values) =>
        val wingEdges = values.iterator.flatMap(_._2).toVector.distinct.sortBy(_.id)
        if (wingEdges.size > limits.maxWingOccurrences)
          return Left(
            Error(
              ErrorCode.ResourceLimitExceeded,
              s"wing occurrences actual=${wingEdges.size} limit=${limits.maxWingOccurrences}"
            )
          )
        val privateVariables = wingEdges.iterator
          .flatMap(_.vertices)
          .filterNot(_ == interface)
          .toVector
          .distinct
          .sorted
        Wing(interface, wingEdges.map(_.id), privateVariables)
      }
    Right(wings)
  }

  private def partitionIsomorphic(
      wings: Vector[Wing],
      edgeById: Map[EdgeId, Hyperedge],
      relationByOccurrence: Map[RelationOccurrenceId, RelationOccurrence],
      budget: IsomorphismBudget
  ): Either[Error, Vector[SharedWing]] = {
    val classes = mutable.ArrayBuffer.empty[MutableClass]
    wings.sortBy(_.interfaceVariable).foreach { wing =>
      var matched = false
      var index = 0
      while (!matched && index < classes.size) {
        val current = classes(index)
        findIsomorphism(
          current.representative,
          wing,
          edgeById,
          relationByOccurrence,
          budget
        ) match {
          case Left(problem) => return Left(problem)
          case Right(Some(isomorphism)) =>
            current.members += wing
            current.isomorphisms += isomorphism
            matched = true
          case Right(None) => index += 1
        }
      }
      if (!matched) {
        val identity = identityIsomorphism(wing)
        classes += MutableClass(
          wing,
          mutable.ArrayBuffer(wing),
          mutable.ArrayBuffer(identity)
        )
      }
    }
    Right(
      classes.toVector.filter(_.members.size >= 2).map(group =>
        SharedWing(
          group.representative,
          group.members.toVector,
          group.isomorphisms.toVector
        )
      )
    )
  }

  private def findIsomorphism(
      representative: Wing,
      member: Wing,
      edgeById: Map[EdgeId, Hyperedge],
      relationByOccurrence: Map[RelationOccurrenceId, RelationOccurrence],
      budget: IsomorphismBudget
  ): Either[Error, Option[WingIsomorphism]] = {
    if (
      representative.edgeIds.size != member.edgeIds.size ||
      representative.privateVariables.size != member.privateVariables.size
    ) return Right(None)

    val sourceEdges = representative.edgeIds.map(edgeById)
    val targetEdges = member.edgeIds.map(edgeById)
    val sourceShapes = sourceEdges.iterator.map(edge => edge.id -> edgeShape(edge, relationByOccurrence)).toMap
    val targetShapes = targetEdges.iterator.map(edge => edge.id -> edgeShape(edge, relationByOccurrence)).toMap
    if (sourceShapes.values.toVector.sortBy(shapeOrderingKey) !=
      targetShapes.values.toVector.sortBy(shapeOrderingKey)) return Right(None)

    val targetByShape = targetEdges.groupBy(edge => targetShapes(edge.id)).view
      .mapValues(_.sortBy(_.id))
      .toMap
    val initialVariables = Map(representative.interfaceVariable -> member.interfaceVariable)
    val initialTargets = Map(member.interfaceVariable -> representative.interfaceVariable)

    def loop(
        ordinal: Int,
        usedTargets: Set[EdgeId],
        variableMap: Map[VariableId, VariableId],
        reverseVariableMap: Map[VariableId, VariableId],
        edgeMap: Vector[(EdgeId, EdgeId)]
    ): Either[Error, Option[WingIsomorphism]] = {
      if (!budget.enter())
        return Left(
          Error(
            ErrorCode.ResourceLimitExceeded,
            s"isomorphism states actual>${budget.limit} limit=${budget.limit}"
          )
        )
      if (ordinal == sourceEdges.size) {
        val sourceVariables =
          (representative.interfaceVariable +: representative.privateVariables).sorted
        val targetVariables =
          (member.interfaceVariable +: member.privateVariables).toSet
        if (
          sourceVariables.forall(variableMap.contains) &&
          variableMap.values.toSet == targetVariables
        )
          Right(
            Some(
              WingIsomorphism(
                member.interfaceVariable,
                edgeMap.sortBy(_._1),
                sourceVariables.map(variable => variable -> variableMap(variable))
              )
            )
          )
        else Right(None)
      } else {
        val source = sourceEdges(ordinal)
        val possibleTargets = targetByShape
          .getOrElse(sourceShapes(source.id), Vector.empty)
          .filterNot(edge => usedTargets(edge.id))
        possibleTargets.iterator.foreach { target =>
          extendVariableMap(
            source,
            target,
            variableMap,
            reverseVariableMap
          ) match {
            case Some((nextVariables, nextReverse)) =>
              loop(
                ordinal + 1,
                usedTargets + target.id,
                nextVariables,
                nextReverse,
                edgeMap :+ (source.id -> target.id)
              ) match {
                case Left(problem) => return Left(problem)
                case found @ Right(Some(_)) => return found
                case Right(None) => ()
              }
            case None => ()
          }
        }
        Right(None)
      }
    }

    loop(0, Set.empty, initialVariables, initialTargets, Vector.empty)
  }

  private def extendVariableMap(
      source: Hyperedge,
      target: Hyperedge,
      current: Map[VariableId, VariableId],
      reverse: Map[VariableId, VariableId]
  ): Option[(Map[VariableId, VariableId], Map[VariableId, VariableId])] = {
    if (source.bindings.map(_._1.ordinal) != target.bindings.map(_._1.ordinal))
      return None
    var forward = current
    var backward = reverse
    source.bindings.zip(target.bindings).foreach {
      case ((_, sourceVariable), (_, targetVariable)) =>
        forward.get(sourceVariable) match {
          case Some(existing) if existing != targetVariable => return None
          case _ => ()
        }
        backward.get(targetVariable) match {
          case Some(existing) if existing != sourceVariable => return None
          case _ => ()
        }
        forward = forward.updated(sourceVariable, targetVariable)
        backward = backward.updated(targetVariable, sourceVariable)
    }
    Some(forward -> backward)
  }

  private def identityIsomorphism(wing: Wing): WingIsomorphism =
    WingIsomorphism(
      wing.interfaceVariable,
      wing.edgeIds.map(edgeId => edgeId -> edgeId),
      (wing.interfaceVariable +: wing.privateVariables).sorted.map(variable =>
        variable -> variable
      )
    )

  private def edgeShape(
      edge: Hyperedge,
      relationByOccurrence: Map[RelationOccurrenceId, RelationOccurrence]
  ): EdgeShape = {
    val relation = relationByOccurrence(edge.relationOccurrenceId)
    EdgeShape(
      relation.qualifiedName,
      relation.columns.map(metadata =>
        (
          metadata.sourceName,
          metadata.sqlTypeName,
          metadata.dataType,
          metadata.nullable
        )
      ),
      edge.bindings.map(_._1.ordinal)
    )
  }

  private def shapeOrderingKey(
      shape: EdgeShape
  ): (String, String, String) =
    (
      shape.qualifiedName.mkString("\u0000"),
      shape.relationColumns
        .map { case (name, sqlType, dataType, nullable) =>
          s"$name\u0000$sqlType\u0000$dataType\u0000$nullable"
        }
        .mkString("\u0001"),
      shape.boundColumnOrdinals.mkString(",")
    )

  private def buildCandidate(
      anchor: Hyperedge,
      sharedWing: SharedWing,
      allEdges: Vector[Hyperedge],
      metadataByColumn: Map[ColumnId, at.ac.tuwien.dbai.yannakakis.frontend.calcite.ColumnMetadata]
  ): Option[Candidate] = {
    val interfaces = sharedWing.members.map { wing =>
      val anchorColumn = anchor.bindings.iterator
        .collect {
          case (columnId, variableId)
              if variableId == wing.interfaceVariable => columnId
        }
        .toVector
        .sorted
        .headOption
      anchorColumn.map(columnId => AnchorInterface(wing.interfaceVariable, columnId))
    }
    if (interfaces.exists(_.isEmpty)) return None
    val exactInterfaces = interfaces.flatten
    val interfaceTypes = exactInterfaces.map(anchorInterface =>
      metadataByColumn(anchorInterface.anchorColumnId).sqlTypeName
    ).distinct
    if (
      interfaceTypes.size != 1 ||
      !Set("SMALLINT", "INTEGER", "BIGINT").contains(interfaceTypes.head)
    ) return None

    val selectedVariables = sharedWing.members.map(_.interfaceVariable)
    val selectedTypesAreUniform = selectedVariables.forall { variableId =>
      allEdges.iterator
        .flatMap(_.bindings.iterator)
        .collect { case (columnId, `variableId`) => metadataByColumn(columnId).sqlTypeName }
        .toSet == Set(interfaceTypes.head)
    }
    if (!selectedTypesAreUniform) return None

    val selectedEdges = sharedWing.members.iterator.flatMap(_.edgeIds).toSet
    val coreEdges = allEdges.map(_.id).filterNot(selectedEdges)
    val avoided =
      (sharedWing.members.size - 1) * sharedWing.representative.edgeIds.size
    val plan = Plan(
      anchor.id,
      exactInterfaces,
      sharedWing,
      coreEdges,
      interfaceTypes.head
    )
    Some(
      Candidate(
        plan,
        (
          -avoided,
          -sharedWing.members.size,
          anchor.id.value,
          sharedWing.representative.interfaceVariable.value
        )
      )
    )
  }

  private def resource(
      label: String,
      actual: Long,
      limit: Long
  ): Left[Vector[Error], Nothing] =
    Left(
      Vector(
        Error(
          ErrorCode.ResourceLimitExceeded,
          s"$label actual=$actual limit=$limit"
        )
      )
    )
}
