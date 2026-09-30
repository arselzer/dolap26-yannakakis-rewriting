package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.{
  DiagnosticSeverity,
  RewriteDiagnostic,
  RewriteReasonCode
}
import at.ac.tuwien.dbai.yannakakis.core.{
  EdgeId,
  Hyperedge,
  QueryHypergraph,
  RelationOccurrenceId,
  VariableId
}

import scala.collection.mutable

/** Deterministic conversion from supported analyzer IR to the core hypergraph. */
object QueryHypergraphConverter {
  def convert(
      query: QueryIR
  ): Either[Vector[RewriteDiagnostic], QueryHypergraph] = {
    require(query != null, "query IR must not be null")

    val applicabilityErrors = applicabilityDiagnostics(query)
    if (applicabilityErrors.nonEmpty) return Left(applicabilityErrors)

    val knownColumns = query.relations.iterator
      .flatMap(_.columns.iterator.map(_.ref))
      .toSet
    val equalities = query.predicates.collect {
      case PredicateDescriptor(
            _,
            _,
            _,
            equality: PredicateClassification.InnerEquality
          ) => equality
    }
    val equalityColumns = ColumnRef.canonical(
      equalities.iterator
        .flatMap(equality => Iterator(equality.left, equality.right))
        .toVector
    )
    val unknownColumns = equalityColumns.filterNot(knownColumns.contains)
    if (unknownColumns.nonEmpty) {
      return Left(
        unknownColumns.map { ref =>
          diagnostic(
            code = RewriteReasonCode.UnresolvedLineage,
            message = "join equality refers to a column outside the analyzed scans",
            severity = DiagnosticSeverity.Error,
            context = Map("column" -> ref.id.toString)
          )
        }
      )
    }

    val unionFind = new DeterministicUnionFind(equalityColumns)
    equalities.foreach(equality => unionFind.union(equality.left, equality.right))
    val classes = equalityColumns
      .groupBy(unionFind.find)
      .valuesIterator
      .map(_.sorted)
      .toVector
      .sortBy(_.head)

    val variableByColumn = classes.zipWithIndex.flatMap {
      case (members, ordinal) =>
        val variableId = VariableId
          .from(ordinal)
          .fold(
            error =>
              throw new IllegalStateException(
                s"invalid deterministic variable identity: ${error.message}"
              ),
            identity
          )
        members.map(_ -> variableId)
    }.toMap

    val edges = Vector.newBuilder[Hyperedge]
    val coreErrors = Vector.newBuilder[RewriteDiagnostic]
    query.relations.foreach { relation =>
      val edgeId = EdgeId
        .from(relation.id.value)
        .fold(
          error =>
            throw new IllegalStateException(
              s"invalid deterministic edge identity: ${error.message}"
            ),
          identity
        )
      val bindings = relation.columns.flatMap { column =>
        variableByColumn.get(column.ref).map(column.ref.id -> _)
      }
      Hyperedge.create(edgeId, relation.id, bindings) match {
        case Right(edge) => edges += edge
        case Left(errors) =>
          errors.foreach { error =>
            coreErrors += diagnostic(
              code = RewriteReasonCode.InternalError,
              message = "analyzed relation produced an invalid hyperedge",
              severity = DiagnosticSeverity.Error,
              context = Map(
                "relationOccurrence" -> relation.id.value.toString,
                "validationError" -> error.message
              )
            )
          }
      }
    }

    val edgeErrors = coreErrors.result()
    if (edgeErrors.nonEmpty) Left(edgeErrors)
    else {
      QueryHypergraph.create(edges.result()) match {
        case Right(hypergraph) => Right(hypergraph)
        case Left(errors) =>
          Left(
            errors.map { error =>
              diagnostic(
                code = RewriteReasonCode.InternalError,
                message = "analyzed query produced an invalid hypergraph",
                severity = DiagnosticSeverity.Error,
                context = Map("validationError" -> error.message)
              )
            }
          )
      }
    }
  }

  private def unsupportedShapeDiagnostics(
      query: QueryIR
  ): Vector[RewriteDiagnostic] = {
    val predicateErrors = query.predicates.collect {
      case predicate
          if !predicate.classification.supported =>
        val unsupported = predicate.classification
          .asInstanceOf[PredicateClassification.Unsupported]
        diagnostic(
          code = unsupported.code,
          message = unsupported.reason,
          severity = DiagnosticSeverity.Warning,
          context = Map(
            "predicateOrdinal" -> predicate.ordinal.toString,
            "nodePath" -> predicate.origin.nodePath,
            "expression" -> predicate.expression.digest
          )
        )
    }

    val relationalErrors = collectRelationalErrors(query.root)
    predicateErrors ++ relationalErrors
  }

  private def applicabilityDiagnostics(
      query: QueryIR
  ): Vector[RewriteDiagnostic] = {
    val shapeErrors = unsupportedShapeDiagnostics(query)
    if (shapeErrors.nonEmpty) shapeErrors
    else {
      val c03Errors = c03G0StructuralColumnDiagnostic(query).toVector
      if (c03Errors.nonEmpty) c03Errors
      else connectivityDiagnostic(query).toVector
    }
  }

  private def c03G0StructuralColumnDiagnostic(
      query: QueryIR
  ): Option[RewriteDiagnostic] = query.resultShape match {
    case shape: QueryResultShape.C03G0AggregateVector =>
      val structuralColumns = query.predicates.iterator.collect {
        case PredicateDescriptor(
              _,
              _,
              _,
              equality: PredicateClassification.InnerEquality
            ) => Vector(equality.left, equality.right)
      }.flatten.toSet
      if (
        structuralColumns(shape.groupColumn) ||
        structuralColumns(shape.measureColumn)
      ) {
        Some(
          diagnostic(
            code = RewriteReasonCode.UnsupportedAggregate,
            message =
              "C03-G0 group and measure columns must remain owner-local and non-structural",
            severity = DiagnosticSeverity.Warning,
            context = Map("resultShape" -> shape.id)
          )
        )
      } else None
    case _ => None
  }

  /** Shared applicability gate used by analysis and direct IR conversion. */
  private[calcite] def connectivityDiagnostic(
      query: QueryIR
  ): Option[RewriteDiagnostic] = {
    if (query.relations.size <= 1) return None

    val knownOccurrences = query.relations.iterator.map(_.id).toSet
    val equalities = query.predicates.collect {
      case PredicateDescriptor(
            _,
            _,
            _,
            equality: PredicateClassification.InnerEquality
          ) => equality
    }
    val hasUnknownOccurrence = equalities.exists { equality =>
      !knownOccurrences(equality.left.relationOccurrenceId) ||
      !knownOccurrences(equality.right.relationOccurrenceId)
    }
    // Unknown equality columns receive a more precise UnresolvedLineage
    // diagnostic later in convert(). Do not let connectivity indexing throw.
    if (hasUnknownOccurrence) return None

    val adjacency = mutable.Map.empty[
      RelationOccurrenceId,
      mutable.Set[RelationOccurrenceId]
    ]
    query.relations.foreach(relation =>
      adjacency.put(relation.id, mutable.Set.empty)
    )
    equalities.foreach { equality =>
      adjacency(equality.left.relationOccurrenceId) +=
        equality.right.relationOccurrenceId
      adjacency(equality.right.relationOccurrenceId) +=
        equality.left.relationOccurrenceId
    }

    val visited = mutable.Set.empty[RelationOccurrenceId]
    val pending = mutable.Queue(query.relations.head.id)
    while (pending.nonEmpty) {
      val current = pending.dequeue()
      if (!visited(current)) {
        visited += current
        adjacency(current).toVector.sorted.foreach(pending.enqueue(_))
      }
    }

    if (visited.size == query.relations.size) None
    else
      Some(
        diagnostic(
          code = RewriteReasonCode.UnsupportedQueryShape,
          message =
            "relation occurrences are disconnected by supported inner equalities",
          severity = DiagnosticSeverity.Warning,
          context = Map(
            "relationOccurrences" -> query.relations.size.toString,
            "supportedEqualities" -> equalities.size.toString
          )
        )
      )
  }

  private def collectRelationalErrors(
      relational: RelationalIR
  ): Vector[RewriteDiagnostic] = relational match {
    case unsupported: UnsupportedRelationalIR =>
      Vector(
        diagnostic(
          code = unsupported.diagnosticCode,
          message = s"relational operator '${unsupported.operatorName}' is unsupported",
          severity = DiagnosticSeverity.Warning,
          context = Map(
            "nodePath" -> unsupported.nodePath,
            "operator" -> unsupported.operatorName
          )
        )
      ) ++ unsupported.inputs.flatMap(collectRelationalErrors)
    case join: JoinIR if join.joinType != "inner" =>
      Vector(
        diagnostic(
          code = RewriteReasonCode.UnsupportedJoinType,
          message = s"join type '${join.joinType}' is unsupported",
          severity = DiagnosticSeverity.Warning,
          context = Map("joinType" -> join.joinType)
        )
      ) ++ collectRelationalErrors(join.left) ++ collectRelationalErrors(join.right)
    case join: JoinIR =>
      collectRelationalErrors(join.left) ++ collectRelationalErrors(join.right)
    case project: ProjectIR =>
      val expressionErrors = project.projects.flatMap { item =>
        if (!item.expression.resolved) {
          Vector(
            diagnostic(
              code = RewriteReasonCode.UnresolvedLineage,
              message = "project expression lineage is unresolved",
              severity = DiagnosticSeverity.Warning,
              context = Map(
                "outputOrdinal" -> item.ordinal.toString,
                "expression" -> item.expression.digest
              )
            )
          )
        } else if (!item.expression.deterministic) {
          Vector(
            diagnostic(
              code = RewriteReasonCode.UnsupportedQueryShape,
              message = "non-deterministic project expression is unsupported",
              severity = DiagnosticSeverity.Warning,
              context = Map(
                "outputOrdinal" -> item.ordinal.toString,
                "expression" -> item.expression.digest
              )
            )
          )
        } else if (!item.expression.losslesslyRetained) {
          Vector(
            diagnostic(
              code = RewriteReasonCode.UnsupportedQueryShape,
              message =
                "project expression is outside the losslessly retained scalar grammar",
              severity = DiagnosticSeverity.Warning,
              context = Map(
                "outputOrdinal" -> item.ordinal.toString,
                "expression" -> item.expression.digest
              )
            )
          )
        } else Vector.empty
      }
      expressionErrors ++ collectRelationalErrors(project.input)
    case count: GlobalCountStarIR =>
      collectRelationalErrors(count.input)
    case count: GroupedCountStarIR =>
      collectRelationalErrors(count.input)
    case count: DistributedGroupedCountStarIR =>
      collectRelationalErrors(count.input)
    case extremum: GlobalGuardedExtremumIR =>
      collectRelationalErrors(extremum.input)
    case aggregate: C03G0AggregateVectorIR =>
      collectRelationalErrors(aggregate.input)
    case filter: FilterIR => collectRelationalErrors(filter.input)
    case _: TableScanIR => Vector.empty
  }

  private def diagnostic(
      code: RewriteReasonCode,
      message: String,
      severity: DiagnosticSeverity,
      context: Map[String, String]
  ): RewriteDiagnostic =
    RewriteDiagnostic(code, message, severity, context)

  /** Union roots are always the smallest positional column identity. */
  private final class DeterministicUnionFind(columns: Vector[ColumnRef]) {
    private val parent = mutable.Map.empty[ColumnRef, ColumnRef]
    columns.foreach(column => parent.put(column, column))

    def find(column: ColumnRef): ColumnRef = {
      val current = parent(column)
      if (current == column) current
      else {
        val root = find(current)
        parent.update(column, root)
        root
      }
    }

    def union(left: ColumnRef, right: ColumnRef): Unit = {
      val leftRoot = find(left)
      val rightRoot = find(right)
      if (leftRoot != rightRoot) {
        val roots = Vector(leftRoot, rightRoot).sorted
        parent.update(roots(1), roots(0))
      }
    }
  }
}
