package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.{
  RewriteDiagnostic,
  RewriteReasonCode
}
import at.ac.tuwien.dbai.yannakakis.core.{
  ColumnId,
  QueryHypergraph,
  RelationOccurrenceId
}

/** Positional identity of one column in one relation occurrence. */
final case class ColumnRef(id: ColumnId) {
  require(id != null, "column reference id must not be null")

  def relationOccurrenceId: RelationOccurrenceId = id.relationOccurrenceId
  def ordinal: Int = id.ordinal
}

object ColumnRef {
  implicit val ordering: Ordering[ColumnRef] =
    Ordering.by[ColumnRef, ColumnId](_.id)(ColumnId.ordering)

  private[calcite] def canonical(
      values: Iterable[ColumnRef]
  ): Vector[ColumnRef] = values.toVector.distinct.sorted
}

/** Display and type metadata kept separate from positional column identity.
  *
  * `sqlTypeName` is Calcite's structured SQL type-family name. `dataType` is
  * the full diagnostic digest, which may also encode nullability and type
  * parameters and is never parsed as SQL by a renderer.
  */
final case class ColumnMetadata(
    ref: ColumnRef,
    sourceName: String,
    sqlTypeName: String,
    dataType: String,
    nullable: Boolean
) {
  require(ref != null, "column metadata reference must not be null")
  require(sourceName != null, "column source name must not be null")
  require(sqlTypeName != null, "column SQL type name must not be null")
  require(sqlTypeName.nonEmpty, "column SQL type name must not be blank")
  require(dataType != null, "column data type must not be null")
}

/** One left-to-right occurrence of a table scan in a relational plan.
  *
  * Calcite's `TableScan` retains the qualified table name but not the SQL alias.
  * Occurrence identity is therefore semantic; a renderer must assign a stable
  * generated alias later instead of trying to recover the source spelling.
  */
final case class RelationOccurrence(
    id: RelationOccurrenceId,
    qualifiedName: Vector[String],
    columns: Vector[ColumnMetadata]
) {
  require(id != null, "relation occurrence id must not be null")
  require(qualifiedName != null, "qualified table name must not be null")
  require(
    qualifiedName.nonEmpty && qualifiedName.forall(_ != null),
    "qualified table name must contain at least one non-null component"
  )
  require(columns != null, "relation columns must not be null")
  require(columns.forall(_ != null), "relation columns must not contain null")
  require(
    columns.map(_.ref.ordinal) == columns.indices.toVector,
    "relation columns must be stored in ordinal order"
  )
  require(
    columns.forall(_.ref.relationOccurrenceId == id),
    "relation columns must belong to their relation occurrence"
  )
}

/** Immutable analyzer expression resolved through Calcite output positions.
  *
  * `digest` is retained for diagnostics and deterministic inspection. It is not
  * a rendering representation. Expressions outside the explicitly lossless
  * grammar below make analysis unsupported until typed lowering is added.
  */
sealed trait ScalarExpression extends Product with Serializable {
  def digest: String
  def dataType: String
  def nullable: Boolean
  def deterministic: Boolean
  def columnRefs: Vector[ColumnRef]
  def resolved: Boolean

  /**
    * Whether this data-only expression contains enough information for exact
    * later reconstruction without parsing `digest`.
    *
    * The initial grammar is deliberately narrow: positional columns and a
    * direct equality between two positional columns. General calls and
    * literals stay in the analysis inventory, but are not called renderable
    * until a typed, dialect-aware lowering representation is implemented.
    */
  def losslesslyRetained: Boolean

  /** Whether this expression is retained exactly in the bounded predicate
    * grammar. This is deliberately separate from [[losslesslyRetained]]:
    * exact integral literals and Calcite-inserted integral widening casts are
    * admitted only as operands of a source-certified occurrence-local
    * equality, never as projection expressions or general scalar values.
    */
  def predicateLosslesslyRetained: Boolean = losslesslyRetained
}

object ScalarExpression {
  /** Predicate-only syntax. Its raw-source proof is mandatory at rendering;
    * this node never grants admission to computed projections.
    */
  private[yannakakis] final case class LocalPredicate(
      tree: CalciteLocalPredicates.Tree[ColumnRef],
      digest: String,
      dataType: String,
      nullable: Boolean
  ) extends ScalarExpression {
    require(tree != null && digest != null && dataType != null)
    override val deterministic: Boolean = true
    override val columnRefs: Vector[ColumnRef] = tree.columns.distinct.sorted
    override val resolved: Boolean = true
    override val losslesslyRetained: Boolean = false
    override val predicateLosslesslyRetained: Boolean = true
  }

  final case class Column(
      ref: ColumnRef,
      digest: String,
      dataType: String,
      nullable: Boolean
  ) extends ScalarExpression {
    require(ref != null, "column expression reference must not be null")
    require(digest != null, "column expression digest must not be null")
    require(dataType != null, "column expression type must not be null")

    override val deterministic: Boolean = true
    override val columnRefs: Vector[ColumnRef] = Vector(ref)
    override val resolved: Boolean = true
    override val losslesslyRetained: Boolean = true
  }

  final case class Literal(
      digest: String,
      dataType: String,
      nullable: Boolean
  ) extends ScalarExpression {
    require(digest != null, "literal digest must not be null")
    require(dataType != null, "literal type must not be null")

    override val deterministic: Boolean = true
    override val columnRefs: Vector[ColumnRef] = Vector.empty
    override val resolved: Boolean = true
    override val losslesslyRetained: Boolean = false
  }

  /** Exact non-NULL integral value retained structurally, never via `digest`.
    *
    * The type and range are revalidated by the PostgreSQL compiler. The value
    * itself is a signed `Long` because the bounded grammar is restricted to
    * Calcite/PostgreSQL SMALLINT, INTEGER, and BIGINT literals.
    */
  final case class ExactIntegralLiteral(
      value: Long,
      sqlTypeName: String,
      digest: String,
      dataType: String
  ) extends ScalarExpression {
    require(sqlTypeName != null, "integral literal SQL type must not be null")
    require(digest != null, "integral literal digest must not be null")
    require(dataType != null, "integral literal type must not be null")

    override val nullable: Boolean = false
    override val deterministic: Boolean = true
    override val columnRefs: Vector[ColumnRef] = Vector.empty
    override val resolved: Boolean = true
    // A literal is executable only inside the certified local equality below.
    override val losslesslyRetained: Boolean = false
  }

  /** An exact widening cast inserted by Calcite while validating an integral
    * comparison. Source SQL casts do not receive the mandatory raw-source
    * certificate and therefore cannot reach rendering through this node.
    */
  final case class ExactIntegralWideningCast(
      operand: ScalarExpression,
      sourceSqlTypeName: String,
      targetSqlTypeName: String,
      digest: String,
      dataType: String,
      nullable: Boolean
  ) extends ScalarExpression {
    require(operand != null, "integral cast operand must not be null")
    require(
      operand.isInstanceOf[Column] ||
        operand.isInstanceOf[ExactIntegralLiteral],
      "integral widening cast operand must be one exact column or literal"
    )
    require(sourceSqlTypeName != null, "integral cast source type must not be null")
    require(targetSqlTypeName != null, "integral cast target type must not be null")
    require(digest != null, "integral cast digest must not be null")
    require(dataType != null, "integral cast type must not be null")

    override val deterministic: Boolean = operand.deterministic
    override val columnRefs: Vector[ColumnRef] = operand.columnRefs
    override val resolved: Boolean = operand.resolved
    // A cast is executable only as an independently revalidated operand of the
    // source-certified local equality; it is never a general scalar grammar.
    override val losslesslyRetained: Boolean = false
  }

  /** Exact top-level global COUNT(*) retained by source and relational proof. */
  case object GlobalCountStar extends ScalarExpression {
    override val digest: String = "COUNT(*)"
    override val dataType: String = "BIGINT NOT NULL"
    override val nullable: Boolean = false
    override val deterministic: Boolean = true
    override val columnRefs: Vector[ColumnRef] = Vector.empty
    override val resolved: Boolean = true
    override val losslesslyRetained: Boolean = true
  }

  /**
    * One source- and relationally-certified global MIN/MAX over a direct guard
    * column. The function and positional argument are retained structurally;
    * `digest` remains diagnostic-only.
    */
  final case class GlobalGuardedExtremum(
      function: GlobalGuardedExtremumFunction,
      argument: Column,
      digest: String,
      dataType: String,
      nullable: Boolean
  ) extends ScalarExpression {
    require(function != null, "guarded extremum function must not be null")
    require(argument != null, "guarded extremum argument must not be null")
    require(digest != null, "guarded extremum digest must not be null")
    require(dataType != null, "guarded extremum type must not be null")
    require(nullable, "global guarded extremum output must be nullable")

    override val deterministic: Boolean = true
    override val columnRefs: Vector[ColumnRef] = Vector(argument.ref)
    override val resolved: Boolean = true
    override val losslesslyRetained: Boolean = true
  }

  /** Exact grouped COUNT(G) retained only inside the C03-G0 terminal root. */
  private[yannakakis] final case class C03G0CountColumn(argument: Column)
      extends ScalarExpression {
    require(argument != null, "C03-G0 COUNT(G) argument must not be null")

    override val digest: String = s"COUNT(${argument.digest})"
    override val dataType: String = "BIGINT NOT NULL"
    override val nullable: Boolean = false
    override val deterministic: Boolean = true
    override val columnRefs: Vector[ColumnRef] = Vector(argument.ref)
    override val resolved: Boolean = true
    override val losslesslyRetained: Boolean = true
  }

  /** Exact grouped MIN(X)/MAX(X) retained only in the C03-G0 terminal root. */
  private[yannakakis] final case class C03G0Extremum(
      function: GlobalGuardedExtremumFunction,
      argument: Column,
      dataType: String,
      nullable: Boolean
  ) extends ScalarExpression {
    require(function != null, "C03-G0 extremum function must not be null")
    require(argument != null, "C03-G0 extremum argument must not be null")
    require(dataType != null, "C03-G0 extremum type must not be null")
    require(
      !nullable,
      "Calcite C03-G0 grouped extremum metadata must be NOT NULL"
    )

    override val digest: String = s"${function.sqlName}(${argument.digest})"
    override val deterministic: Boolean = true
    override val columnRefs: Vector[ColumnRef] = Vector(argument.ref)
    override val resolved: Boolean = true
    override val losslesslyRetained: Boolean = true
  }

  final case class Call(
      sqlKind: String,
      operatorName: String,
      operands: Vector[ScalarExpression],
      digest: String,
      dataType: String,
      nullable: Boolean,
      deterministic: Boolean
  ) extends ScalarExpression {
    require(sqlKind != null, "call SQL kind must not be null")
    require(operatorName != null, "call operator name must not be null")
    require(operands != null, "call operands must not be null")
    require(operands.forall(_ != null), "call operands must not contain null")
    require(digest != null, "call digest must not be null")
    require(dataType != null, "call type must not be null")

    override val columnRefs: Vector[ColumnRef] =
      ColumnRef.canonical(operands.iterator.flatMap(_.columnRefs).toVector)
    override val resolved: Boolean = operands.forall(_.resolved)
    override val losslesslyRetained: Boolean =
      sqlKind == "EQUALS" && (operands match {
        case Vector(_: Column, _: Column) => true
        case _                            => false
      })

    override val predicateLosslesslyRetained: Boolean =
      losslesslyRetained ||
        (sqlKind == "EQUALS" && operatorName == "=" && (operands match {
          case Vector(left, right) =>
            (isIntegralColumnOperand(left) && isIntegralLiteralOperand(right)) ||
              (isIntegralLiteralOperand(left) && isIntegralColumnOperand(right))
          case _ => false
        }))
  }

  private def isIntegralColumnOperand(expression: ScalarExpression): Boolean =
    expression match {
      case _: Column => true
      case ExactIntegralWideningCast(_: Column, _, _, _, _, _) => true
      case _ => false
    }

  private def isIntegralLiteralOperand(expression: ScalarExpression): Boolean =
    expression match {
      case _: ExactIntegralLiteral => true
      case ExactIntegralWideningCast(
            _: ExactIntegralLiteral,
            _,
            _,
            _,
            _,
            _
          ) => true
      case _ => false
    }

  /** A Calcite expression whose input lineage cannot be represented yet. */
  final case class Unresolved(
      digest: String,
      dataType: String,
      nullable: Boolean,
      reason: String
  ) extends ScalarExpression {
    require(digest != null, "unresolved expression digest must not be null")
    require(dataType != null, "unresolved expression type must not be null")
    require(reason != null && reason.trim.nonEmpty, "reason must not be blank")

    override val deterministic: Boolean = false
    override val columnRefs: Vector[ColumnRef] = Vector.empty
    override val resolved: Boolean = false
    override val losslesslyRetained: Boolean = false
  }
}

sealed trait PredicateOrigin extends Product with Serializable {
  def nodePath: String
  def id: String
}

object PredicateOrigin {
  final case class Filter(nodePath: String) extends PredicateOrigin {
    require(nodePath != null && nodePath.nonEmpty, "node path must not be blank")
    override val id: String = "filter"
  }

  final case class Join(nodePath: String, joinType: String)
      extends PredicateOrigin {
    require(nodePath != null && nodePath.nonEmpty, "node path must not be blank")
    require(joinType != null && joinType.nonEmpty, "join type must not be blank")
    override val id: String = "join"
  }
}

/** Semantic classification of one conjunct; no conjunct is omitted. */
sealed trait PredicateClassification extends Product with Serializable {
  def supported: Boolean
  def id: String
}

object PredicateClassification {
  final case class InnerEquality(left: ColumnRef, right: ColumnRef)
      extends PredicateClassification {
    require(left != null, "equality left column must not be null")
    require(right != null, "equality right column must not be null")
    require(
      left.relationOccurrenceId != right.relationOccurrenceId,
      "inner equality must connect two relation occurrences"
    )

    override val supported: Boolean = true
    override val id: String = "inner-equality"
  }

  final case class LocalFilter(relationOccurrenceId: RelationOccurrenceId)
      extends PredicateClassification {
    require(relationOccurrenceId != null, "filter occurrence must not be null")
    override val supported: Boolean = true
    override val id: String = "local-filter"
  }

  case object ConstantFilter extends PredicateClassification {
    override val supported: Boolean = true
    override val id: String = "constant-filter"
  }

  final case class Unsupported(
      code: RewriteReasonCode,
      reason: String
  ) extends PredicateClassification {
    require(code != null, "unsupported predicate code must not be null")
    require(reason != null && reason.trim.nonEmpty, "reason must not be blank")
    override val supported: Boolean = false
    override val id: String = "unsupported"
  }
}

final case class PredicateDescriptor(
    ordinal: Int,
    origin: PredicateOrigin,
    expression: ScalarExpression,
    classification: PredicateClassification
) {
  require(ordinal >= 0, "predicate ordinal must be non-negative")
  require(origin != null, "predicate origin must not be null")
  require(expression != null, "predicate expression must not be null")
  require(classification != null, "predicate classification must not be null")
  require(
    !classification.supported || expression.predicateLosslesslyRetained,
    "a supported predicate must be retained losslessly"
  )
}

final case class OutputSlot(
    ordinal: Int,
    name: String,
    expression: ScalarExpression,
    dataType: String,
    nullable: Boolean
) {
  require(ordinal >= 0, "output ordinal must be non-negative")
  require(name != null, "output name must not be null")
  require(expression != null, "output expression must not be null")
  require(dataType != null, "output type must not be null")
}

sealed trait RelationalIR extends Product with Serializable {
  def output: Vector[OutputSlot]
}

object RelationalIR {
  /** Scan occurrences in the same deterministic left-to-right traversal order. */
  private[calcite] def relationInventory(
      relational: RelationalIR
  ): Vector[RelationOccurrence] = relational match {
    case scan: TableScanIR => Vector(scan.relation)
    case filter: FilterIR => relationInventory(filter.input)
    case join: JoinIR =>
      relationInventory(join.left) ++ relationInventory(join.right)
    case project: ProjectIR => relationInventory(project.input)
    case count: GlobalCountStarIR => relationInventory(count.input)
    case count: GroupedCountStarIR => relationInventory(count.input)
    case count: DistributedGroupedCountStarIR =>
      relationInventory(count.input)
    case extremum: GlobalGuardedExtremumIR =>
      relationInventory(extremum.input)
    case aggregate: C03G0AggregateVectorIR =>
      relationInventory(aggregate.input)
    case unsupported: UnsupportedRelationalIR =>
      unsupported.inputs.flatMap(relationInventory)
  }

  /** Predicates in the same deterministic post-order used by the analyzer. */
  private[calcite] def predicateInventory(
      relational: RelationalIR
  ): Vector[PredicateDescriptor] = relational match {
    case _: TableScanIR => Vector.empty
    case filter: FilterIR =>
      predicateInventory(filter.input) ++ filter.predicates
    case join: JoinIR =>
      predicateInventory(join.left) ++
        predicateInventory(join.right) ++
        join.predicates
    case project: ProjectIR => predicateInventory(project.input)
    case count: GlobalCountStarIR => predicateInventory(count.input)
    case count: GroupedCountStarIR => predicateInventory(count.input)
    case count: DistributedGroupedCountStarIR =>
      predicateInventory(count.input)
    case extremum: GlobalGuardedExtremumIR =>
      predicateInventory(extremum.input)
    case aggregate: C03G0AggregateVectorIR =>
      predicateInventory(aggregate.input)
    case unsupported: UnsupportedRelationalIR =>
      unsupported.inputs.flatMap(predicateInventory)
  }

  /** Aggregate scalars may occur only in one certified terminal wrapper. */
  private[calcite] def containsAggregateScalar(
      relational: RelationalIR
  ): Boolean = relational match {
    case _: TableScanIR => false
    case filter: FilterIR =>
      filter.predicates.exists(containsAggregateScalar) ||
        containsAggregateScalar(filter.input)
    case join: JoinIR =>
      join.predicates.exists(containsAggregateScalar) ||
        join.output.exists(slot => containsAggregateScalar(slot.expression)) ||
        containsAggregateScalar(join.left) ||
        containsAggregateScalar(join.right)
    case project: ProjectIR =>
      project.projects.exists(item => containsAggregateScalar(item.expression)) ||
        containsAggregateScalar(project.input)
    case _: GlobalCountStarIR => true
    case _: GroupedCountStarIR => true
    case _: DistributedGroupedCountStarIR => true
    case _: GlobalGuardedExtremumIR => true
    case _: C03G0AggregateVectorIR => true
    case unsupported: UnsupportedRelationalIR =>
      unsupported.output.exists(slot => containsAggregateScalar(slot.expression)) ||
        unsupported.inputs.exists(containsAggregateScalar)
  }

  private def containsAggregateScalar(
      predicate: PredicateDescriptor
  ): Boolean = containsAggregateScalar(predicate.expression)

  private def containsAggregateScalar(
      expression: ScalarExpression
  ): Boolean = expression match {
    case ScalarExpression.GlobalCountStar => true
    case _: ScalarExpression.GlobalGuardedExtremum => true
    case _: ScalarExpression.C03G0CountColumn => true
    case _: ScalarExpression.C03G0Extremum => true
    case call: ScalarExpression.Call =>
      call.operands.exists(containsAggregateScalar)
    case cast: ScalarExpression.ExactIntegralWideningCast =>
      containsAggregateScalar(cast.operand)
    case _ => false
  }
}

final case class TableScanIR(relation: RelationOccurrence)
    extends RelationalIR {
  require(relation != null, "scan relation must not be null")

  override val output: Vector[OutputSlot] = relation.columns.map { column =>
    OutputSlot(
      ordinal = column.ref.ordinal,
      name = column.sourceName,
      expression = ScalarExpression.Column(
        ref = column.ref,
        digest = column.ref.id.toString,
        dataType = column.dataType,
        nullable = column.nullable
      ),
      dataType = column.dataType,
      nullable = column.nullable
    )
  }
}

final case class FilterIR(
    input: RelationalIR,
    predicates: Vector[PredicateDescriptor]
) extends RelationalIR {
  require(input != null, "filter input must not be null")
  require(predicates != null, "filter predicates must not be null")
  require(predicates.forall(_ != null), "filter predicates must not contain null")

  override val output: Vector[OutputSlot] = input.output
}

final case class JoinIR(
    left: RelationalIR,
    right: RelationalIR,
    joinType: String,
    predicates: Vector[PredicateDescriptor],
    override val output: Vector[OutputSlot]
) extends RelationalIR {
  require(left != null, "join left input must not be null")
  require(right != null, "join right input must not be null")
  require(joinType != null && joinType.nonEmpty, "join type must not be blank")
  require(predicates != null, "join predicates must not be null")
  require(predicates.forall(_ != null), "join predicates must not contain null")
  require(output != null, "join output must not be null")
  require(output.forall(_ != null), "join output must not contain null")
}

final case class ProjectItem(
    ordinal: Int,
    name: String,
    expression: ScalarExpression,
    dataType: String,
    nullable: Boolean
) {
  require(ordinal >= 0, "project ordinal must be non-negative")
  require(name != null, "project name must not be null")
  require(expression != null, "project expression must not be null")
  require(dataType != null, "project type must not be null")
}

final case class ProjectIR(input: RelationalIR, projects: Vector[ProjectItem])
    extends RelationalIR {
  require(input != null, "project input must not be null")
  require(projects != null, "project items must not be null")
  require(projects.forall(_ != null), "project items must not contain null")
  require(
    projects.map(_.ordinal) == projects.indices.toVector,
    "project items must be in positional order"
  )

  override val output: Vector[OutputSlot] = projects.map { project =>
    OutputSlot(
      project.ordinal,
      project.name,
      project.expression,
      project.dataType,
      project.nullable
    )
  }
}

/**
  * Certified top-level global COUNT(*) over an otherwise ordinary relational
  * child. The wrapper is transparent to relation/predicate inventory and
  * hypergraph construction, but preserves the one-row result shape for later
  * reconstruction.
  */
final case class GlobalCountStarIR(
    input: RelationalIR,
    countOutput: OutputSlot,
    sourceCertificate: GlobalCountStarSourceCertificate
) extends RelationalIR {
  require(input != null, "global COUNT(*) input must not be null")
  require(countOutput != null, "global COUNT(*) output must not be null")
  require(
    sourceCertificate != null,
    "global COUNT(*) source certificate must not be null"
  )
  require(countOutput.ordinal == 0, "global COUNT(*) output ordinal must be zero")
  require(!countOutput.nullable, "global COUNT(*) output must be NOT NULL")
  require(
    countOutput.expression == ScalarExpression.GlobalCountStar,
    "global COUNT(*) output must use the retained COUNT(*) scalar"
  )
  require(
    countOutput.dataType == "BIGINT NOT NULL" &&
      countOutput.dataType == countOutput.expression.dataType &&
      countOutput.nullable == countOutput.expression.nullable,
    "global COUNT(*) output metadata must match its retained scalar"
  )
  require(
    sourceCertificate.aliasMode != GlobalCountStarAliasMode.Implicit ||
      countOutput.name == "count",
    "implicit PostgreSQL COUNT(*) output must be named 'count'"
  )
  require(
    !RelationalIR.containsAggregateScalar(input),
    "global COUNT(*) input must not contain a nested aggregate scalar or wrapper"
  )

  override val output: Vector[OutputSlot] = Vector(countOutput)
}

/**
  * Certified one-key grouped COUNT(*) over an ordinary positive relational
  * child. The grouping key is one direct source column; structural membership
  * is proved later against the converted hypergraph before PostgreSQL lowering.
  */
final case class GroupedCountStarIR(
    input: RelationalIR,
    groupOutput: OutputSlot,
    countOutput: OutputSlot,
    sourceCertificate: GroupedCountStarSourceCertificate
) extends RelationalIR {
  require(input != null, "grouped COUNT(*) input must not be null")
  require(groupOutput != null, "grouped COUNT(*) key output must not be null")
  require(countOutput != null, "grouped COUNT(*) count output must not be null")
  require(
    sourceCertificate != null,
    "grouped COUNT(*) source certificate must not be null"
  )
  require(groupOutput.ordinal == 0, "grouped COUNT(*) key ordinal must be zero")
  val groupColumn: ScalarExpression.Column = groupOutput.expression match {
    case value: ScalarExpression.Column => value
    case _ =>
      throw new IllegalArgumentException(
        "grouped COUNT(*) key must be one retained direct column"
      )
  }
  require(
    groupOutput.dataType == groupColumn.dataType &&
      groupOutput.nullable == groupColumn.nullable,
    "grouped COUNT(*) key metadata must match its retained column"
  )
  require(countOutput.ordinal == 1, "grouped COUNT(*) count ordinal must be one")
  require(!countOutput.nullable, "grouped COUNT(*) count must be NOT NULL")
  require(
    countOutput.expression == ScalarExpression.GlobalCountStar &&
      countOutput.dataType == "BIGINT NOT NULL",
    "grouped COUNT(*) count must use the canonical COUNT(*) scalar and type"
  )
  require(
    sourceCertificate.countAliasMode != GlobalCountStarAliasMode.Implicit ||
      countOutput.name == "count",
    "implicit PostgreSQL grouped COUNT(*) output must be named 'count'"
  )
  require(
    !RelationalIR.containsAggregateScalar(input),
    "grouped COUNT(*) input must not contain a nested aggregate scalar or wrapper"
  )

  override val output: Vector[OutputSlot] = Vector(groupOutput, countOutput)
}

/**
  * Certified distributed grouped COUNT(*) over an ordinary positive
  * relational child. All keys have direct positional lineage and originate
  * in distinct relation occurrences. Their distinct structural VariableIds
  * are proved later against the converted hypergraph before lowering.
  */
final case class DistributedGroupedCountStarIR(
    input: RelationalIR,
    groupOutputs: Vector[OutputSlot],
    countOutput: OutputSlot,
    sourceCertificate: DistributedGroupedCountStarSourceCertificate
) extends RelationalIR {
  require(input != null, "distributed grouped COUNT(*) input must not be null")
  require(
    groupOutputs != null && sourceCertificate != null &&
      groupOutputs.size == sourceCertificate.groupKeyCount,
    "distributed grouped COUNT(*) outputs must match certified arity"
  )
  require(
    groupOutputs.forall(_ != null),
    "distributed grouped COUNT(*) group outputs must not contain null"
  )
  require(
    groupOutputs.map(_.ordinal) == groupOutputs.indices.toVector,
    "distributed grouped COUNT(*) group outputs must be in positional order"
  )
  require(
    countOutput != null,
    "distributed grouped COUNT(*) count output must not be null"
  )
  require(
    sourceCertificate != null,
    "distributed grouped COUNT(*) source certificate must not be null"
  )

  val groupColumns: Vector[ScalarExpression.Column] = groupOutputs.map {
    output =>
      output.expression match {
        case value: ScalarExpression.Column => value
        case _ =>
          throw new IllegalArgumentException(
            "distributed grouped COUNT(*) keys must be retained direct columns"
          )
      }
  }
  val groupColumnRefs: Vector[ColumnRef] = groupColumns.map(_.ref)
  val groupOccurrenceIds: Vector[RelationOccurrenceId] =
    groupColumnRefs.map(_.relationOccurrenceId)
  private val inputColumns = RelationalIR
    .relationInventory(input)
    .iterator
    .flatMap(_.columns.iterator.map(_.ref))
    .toVector

  require(
    groupOccurrenceIds.distinct.size == groupOutputs.size,
    "distributed grouped COUNT(*) keys must come from distinct relation occurrences"
  )
  require(
    groupColumnRefs.forall(ref => inputColumns.count(_ == ref) == 1),
    "distributed grouped COUNT(*) keys must each identify one input column"
  )
  require(
    groupOutputs.zip(groupColumns).forall { case (output, column) =>
      output.dataType == column.dataType && output.nullable == column.nullable
    },
    "distributed grouped COUNT(*) key metadata must match retained columns"
  )
  require(
    countOutput.ordinal == groupOutputs.size,
    "distributed grouped COUNT(*) count must follow its keys"
  )
  require(
    !countOutput.nullable,
    "distributed grouped COUNT(*) count must be NOT NULL"
  )
  require(
    countOutput.expression == ScalarExpression.GlobalCountStar &&
      countOutput.dataType == "BIGINT NOT NULL",
    "distributed grouped COUNT(*) count must use canonical COUNT(*) metadata"
  )
  require(
    sourceCertificate.countAliasMode != GlobalCountStarAliasMode.Implicit ||
      countOutput.name == "count",
    "implicit PostgreSQL distributed grouped COUNT(*) output must be named 'count'"
  )
  require(
    !RelationalIR.containsAggregateScalar(input),
    "distributed grouped COUNT(*) input must not contain a nested aggregate scalar or wrapper"
  )

  override val output: Vector[OutputSlot] = groupOutputs :+ countOutput
}

/**
  * Certified top-level global MIN/MAX over one direct column from one guard
  * relation occurrence. The wrapper is transparent to relation/predicate
  * inventory and preserves the exact guard identity for physical lowering.
  */
final case class GlobalGuardedExtremumIR(
    input: RelationalIR,
    extremumOutput: OutputSlot,
    sourceCertificate: GlobalGuardedExtremumSourceCertificate
) extends RelationalIR {
  require(input != null, "global guarded extremum input must not be null")
  require(
    extremumOutput != null,
    "global guarded extremum output must not be null"
  )
  require(
    sourceCertificate != null,
    "global guarded extremum source certificate must not be null"
  )
  require(
    extremumOutput.ordinal == 0,
    "global guarded extremum output ordinal must be zero"
  )
  require(
    extremumOutput.nullable,
    "global guarded extremum output must be nullable"
  )

  val extremum: ScalarExpression.GlobalGuardedExtremum =
    extremumOutput.expression match {
      case value: ScalarExpression.GlobalGuardedExtremum => value
      case _ =>
        throw new IllegalArgumentException(
          "global guarded extremum output must use its retained scalar"
        )
    }
  val guardColumn: ColumnRef = extremum.argument.ref
  val guardOccurrenceId: RelationOccurrenceId =
    guardColumn.relationOccurrenceId
  private val certifiedGuardColumns = RelationalIR
    .relationInventory(input)
    .iterator
    .flatMap(_.columns.iterator)
    .filter(_.ref == guardColumn)
    .toVector

  require(
    extremum.function == sourceCertificate.function,
    "guarded extremum scalar and source certificate functions must match"
  )
  require(
    certifiedGuardColumns.size == 1,
    "global guarded extremum guard must identify exactly one input column"
  )
  require(
    extremumOutput.dataType == extremum.dataType &&
      extremumOutput.nullable == extremum.nullable,
    "global guarded extremum output metadata must match its retained scalar"
  )
  require(
    sourceCertificate.aliasMode !=
      GlobalGuardedExtremumAliasMode.Implicit ||
      extremumOutput.name == extremum.function.implicitOutputName,
    "implicit PostgreSQL guarded extremum output name must match its function"
  )
  require(
    !RelationalIR.containsAggregateScalar(input),
    "global guarded extremum input must not contain a nested aggregate scalar or wrapper"
  )

  override val output: Vector[OutputSlot] = Vector(extremumOutput)
}

/**
  * Exact opt-in C03-G0 V2 terminal vector:
  * G, COUNT(*), COUNT(G), MIN(X), MAX(X), grouped by direct G. G and X are
  * direct columns of the same verified guard occurrence. The wrapper is
  * transparent to relational inventories and hypergraph construction.
  */
private[yannakakis] final case class C03G0AggregateVectorIR(
    input: RelationalIR,
    groupOutput: OutputSlot,
    countStarOutput: OutputSlot,
    countGroupOutput: OutputSlot,
    minOutput: OutputSlot,
    maxOutput: OutputSlot,
    sourceCertificate: C03G0SourceCertificate
) extends RelationalIR {
  require(input != null, "C03-G0 input must not be null")
  require(groupOutput != null, "C03-G0 group output must not be null")
  require(countStarOutput != null, "C03-G0 COUNT(*) output must not be null")
  require(countGroupOutput != null, "C03-G0 COUNT(G) output must not be null")
  require(minOutput != null, "C03-G0 MIN(X) output must not be null")
  require(maxOutput != null, "C03-G0 MAX(X) output must not be null")
  require(sourceCertificate != null, "C03-G0 source certificate must not be null")

  override val output: Vector[OutputSlot] = Vector(
    groupOutput,
    countStarOutput,
    countGroupOutput,
    minOutput,
    maxOutput
  )
  require(
    output.map(_.ordinal) == output.indices.toVector,
    "C03-G0 outputs must use exact positional ordinals"
  )
  require(
    output.map(_.name) == C03G0SourceCertificate.OutputAliases,
    "C03-G0 outputs must use the exact fixed aliases"
  )

  val groupColumn: ScalarExpression.Column = groupOutput.expression match {
    case value: ScalarExpression.Column => value
    case _ =>
      throw new IllegalArgumentException(
        "C03-G0 group output must retain one direct column"
      )
  }
  val countGroup: ScalarExpression.C03G0CountColumn =
    countGroupOutput.expression match {
      case value: ScalarExpression.C03G0CountColumn => value
      case _ =>
        throw new IllegalArgumentException(
          "C03-G0 COUNT(G) output must retain its dedicated scalar"
        )
    }
  val minimum: ScalarExpression.C03G0Extremum = minOutput.expression match {
    case value: ScalarExpression.C03G0Extremum => value
    case _ =>
      throw new IllegalArgumentException(
        "C03-G0 MIN(X) output must retain its dedicated scalar"
      )
  }
  val maximum: ScalarExpression.C03G0Extremum = maxOutput.expression match {
    case value: ScalarExpression.C03G0Extremum => value
    case _ =>
      throw new IllegalArgumentException(
        "C03-G0 MAX(X) output must retain its dedicated scalar"
      )
  }
  val measureColumn: ScalarExpression.Column = minimum.argument
  val guardOccurrenceId: RelationOccurrenceId =
    groupColumn.ref.relationOccurrenceId
  val catalogBinding: C03G0CatalogBinding = sourceCertificate.catalogBinding

  private val inputRelations = RelationalIR.relationInventory(input)
  private val inputMetadata = inputRelations.iterator.flatMap(_.columns).toVector
  private val groupMetadata = inputMetadata.filter(_.ref == groupColumn.ref)
  private val measureMetadata = inputMetadata.filter(_.ref == measureColumn.ref)
  private val guardRelations = inputRelations.filter(_.id == guardOccurrenceId)
  private val predicateColumns = RelationalIR
    .predicateInventory(input)
    .iterator
    .flatMap(_.expression.columnRefs)
    .toSet

  require(groupColumn.ref != measureColumn.ref, "C03-G0 G and X must be distinct columns")
  require(
    groupColumn.ref.ordinal == catalogBinding.groupAttnum - 1 &&
      measureColumn.ref.ordinal == catalogBinding.measureAttnum - 1,
    "C03-G0 G and X ordinals must match the verified attribute numbers"
  )
  require(
    measureColumn.ref.relationOccurrenceId == guardOccurrenceId,
    "C03-G0 G and X must have the same occurrence lineage"
  )
  require(
    !predicateColumns(groupColumn.ref) && !predicateColumns(measureColumn.ref),
    "C03-G0 G and X must remain non-structural owner-local columns"
  )
  require(
    groupMetadata.size == 1 && measureMetadata.size == 1,
    "C03-G0 G and X must each identify one input column"
  )
  require(
    guardRelations.size == 1 &&
      guardRelations.head.qualifiedName.endsWith(
        Vector(catalogBinding.physicalSchemaName, catalogBinding.relationName)
      ),
    "C03-G0 guard occurrence must match the verified schema and relation"
  )
  require(
    groupMetadata.head.sourceName == catalogBinding.groupColumnName &&
      groupMetadata.head.sqlTypeName == "VARCHAR" &&
      groupMetadata.head.nullable,
    "C03-G0 G must be the verified nullable VARCHAR payload column"
  )
  require(
    measureMetadata.head.sourceName == catalogBinding.measureColumnName &&
      measureMetadata.head.sqlTypeName == "BIGINT" &&
      !measureMetadata.head.nullable,
    "C03-G0 X must be the verified BIGINT NOT NULL row_id column"
  )
  require(
    groupOutput.dataType == groupColumn.dataType &&
      groupOutput.nullable == groupColumn.nullable && groupOutput.nullable,
    "C03-G0 group metadata must match nullable direct G"
  )
  require(
    countStarOutput.expression == ScalarExpression.GlobalCountStar &&
      countStarOutput.dataType == "BIGINT NOT NULL" &&
      !countStarOutput.nullable,
    "C03-G0 COUNT(*) must use canonical BIGINT NOT NULL metadata"
  )
  require(
    countGroup.argument.ref == groupColumn.ref &&
      countGroupOutput.dataType == countGroup.dataType &&
      countGroupOutput.dataType == "BIGINT NOT NULL" &&
      !countGroupOutput.nullable,
    "C03-G0 COUNT(G) must reference the sole group column"
  )
  require(
    minimum.function == GlobalGuardedExtremumFunction.Min &&
      maximum.function == GlobalGuardedExtremumFunction.Max &&
      minimum.argument.ref == measureColumn.ref &&
      maximum.argument.ref == measureColumn.ref,
    "C03-G0 MIN/MAX must reference the same direct X column"
  )
  require(
    Vector(minOutput -> minimum, maxOutput -> maximum).forall {
      case (result, scalar) =>
        result.dataType == scalar.dataType &&
          result.dataType == "BIGINT NOT NULL" &&
          result.nullable == scalar.nullable && !result.nullable
    },
    "C03-G0 extrema must use exact BIGINT NOT NULL metadata"
  )
  require(
    !RelationalIR.containsAggregateScalar(input),
    "C03-G0 input must not contain a nested aggregate scalar or wrapper"
  )
}

/** Derived terminal result shape; QueryIR's constructor remains unchanged. */
sealed trait QueryResultShape extends Product with Serializable {
  def id: String
}

object QueryResultShape {
  case object EnumerationProjection extends QueryResultShape {
    override val id: String = "enumeration-projection"
  }

  final case class GlobalCountStar(
      output: OutputSlot,
      aliasMode: GlobalCountStarAliasMode
  ) extends QueryResultShape {
    require(output != null, "global COUNT(*) result output must not be null")
    require(aliasMode != null, "global COUNT(*) result alias mode must not be null")
    require(output.ordinal == 0, "global COUNT(*) result ordinal must be zero")
    require(!output.nullable, "global COUNT(*) result must be NOT NULL")
    require(
      output.expression == ScalarExpression.GlobalCountStar &&
        output.dataType == "BIGINT NOT NULL",
      "global COUNT(*) result must use the canonical COUNT(*) scalar and type"
    )
    require(
      aliasMode != GlobalCountStarAliasMode.Implicit || output.name == "count",
      "implicit PostgreSQL COUNT(*) result must be named 'count'"
    )

    override val id: String = "global-count-star"
  }

  final case class GroupedCountStar(
      groupOutput: OutputSlot,
      countOutput: OutputSlot,
      countAliasMode: GlobalCountStarAliasMode
  ) extends QueryResultShape {
    require(groupOutput != null, "grouped COUNT(*) key must not be null")
    require(countOutput != null, "grouped COUNT(*) count must not be null")
    require(countAliasMode != null, "grouped COUNT(*) alias mode must not be null")
    require(groupOutput.ordinal == 0, "grouped COUNT(*) key ordinal must be zero")
    val groupColumn: ScalarExpression.Column = groupOutput.expression match {
      case value: ScalarExpression.Column => value
      case _ =>
        throw new IllegalArgumentException(
          "grouped COUNT(*) key must use one retained direct column"
        )
    }
    require(
      groupOutput.dataType == groupColumn.dataType &&
        groupOutput.nullable == groupColumn.nullable,
      "grouped COUNT(*) key metadata must match its retained column"
    )
    require(countOutput.ordinal == 1, "grouped COUNT(*) count ordinal must be one")
    require(
      !countOutput.nullable &&
        countOutput.expression == ScalarExpression.GlobalCountStar &&
        countOutput.dataType == "BIGINT NOT NULL",
      "grouped COUNT(*) count must use canonical BIGINT NOT NULL metadata"
    )
    require(
      countAliasMode != GlobalCountStarAliasMode.Implicit ||
        countOutput.name == "count",
      "implicit PostgreSQL grouped COUNT(*) output must be named 'count'"
    )

    override val id: String = "grouped-count-star"
  }

  final case class DistributedGroupedCountStar(
      groupOutputs: Vector[OutputSlot],
      countOutput: OutputSlot,
      countAliasMode: GlobalCountStarAliasMode
  ) extends QueryResultShape {
    require(
      groupOutputs != null && groupOutputs.size >= 2,
      "distributed grouped COUNT(*) result must have at least two group outputs"
    )
    require(
      groupOutputs.forall(_ != null),
      "distributed grouped COUNT(*) group outputs must not contain null"
    )
    require(
      groupOutputs.map(_.ordinal) == groupOutputs.indices.toVector,
      "distributed grouped COUNT(*) group outputs must be positional"
    )
    require(
      countOutput != null,
      "distributed grouped COUNT(*) count output must not be null"
    )
    require(
      countAliasMode != null,
      "distributed grouped COUNT(*) alias mode must not be null"
    )
    val groupColumns: Vector[ScalarExpression.Column] = groupOutputs.map {
      output =>
        output.expression match {
          case value: ScalarExpression.Column => value
          case _ =>
            throw new IllegalArgumentException(
              "distributed grouped COUNT(*) keys must use retained direct columns"
            )
        }
    }
    require(
      groupColumns.map(_.ref.relationOccurrenceId).distinct.size == groupOutputs.size,
      "distributed grouped COUNT(*) keys must have distinct occurrence lineage"
    )
    require(
      groupOutputs.zip(groupColumns).forall { case (output, column) =>
        output.dataType == column.dataType && output.nullable == column.nullable
      },
      "distributed grouped COUNT(*) key metadata must match retained columns"
    )
    require(
      countOutput.ordinal == groupOutputs.size &&
        !countOutput.nullable &&
        countOutput.expression == ScalarExpression.GlobalCountStar &&
        countOutput.dataType == "BIGINT NOT NULL",
      "distributed grouped COUNT(*) count must use canonical BIGINT NOT NULL metadata"
    )
    require(
      countAliasMode != GlobalCountStarAliasMode.Implicit ||
        countOutput.name == "count",
      "implicit PostgreSQL distributed grouped COUNT(*) output must be named 'count'"
    )

    override val id: String = "distributed-grouped-count-star"
  }

  final case class GlobalGuardedExtremum(
      function: GlobalGuardedExtremumFunction,
      guardColumn: ColumnRef,
      output: OutputSlot,
      aliasMode: GlobalGuardedExtremumAliasMode
  ) extends QueryResultShape {
    require(function != null, "guarded extremum function must not be null")
    require(guardColumn != null, "guarded extremum guard column must not be null")
    require(output != null, "guarded extremum output must not be null")
    require(aliasMode != null, "guarded extremum alias mode must not be null")
    require(output.ordinal == 0, "guarded extremum result ordinal must be zero")
    require(output.nullable, "guarded extremum result must be nullable")
    val extremum: ScalarExpression.GlobalGuardedExtremum =
      output.expression match {
        case value: ScalarExpression.GlobalGuardedExtremum => value
        case _ =>
          throw new IllegalArgumentException(
            "guarded extremum result must use its retained scalar"
          )
      }
    require(
      extremum.function == function && extremum.argument.ref == guardColumn,
      "guarded extremum result metadata must match its retained scalar"
    )
    require(
      output.dataType == extremum.dataType &&
        output.nullable == extremum.nullable,
      "guarded extremum result type must match its retained scalar"
    )
    require(
      aliasMode != GlobalGuardedExtremumAliasMode.Implicit ||
        output.name == function.implicitOutputName,
      "implicit PostgreSQL guarded extremum result name must match its function"
    )

    override val id: String = "global-guarded-extremum"
  }

  private[yannakakis] final case class C03G0AggregateVector(
      groupColumn: ColumnRef,
      measureColumn: ColumnRef,
      outputs: Vector[OutputSlot],
      catalogBinding: C03G0CatalogBinding
  ) extends QueryResultShape {
    require(groupColumn != null, "C03-G0 result group column must not be null")
    require(measureColumn != null, "C03-G0 result measure column must not be null")
    require(
      groupColumn != measureColumn &&
        groupColumn.relationOccurrenceId == measureColumn.relationOccurrenceId,
      "C03-G0 result columns must be distinct columns of one occurrence"
    )
    require(
      outputs != null && outputs.size == 5 && outputs.forall(_ != null),
      "C03-G0 result must contain exactly five outputs"
    )
    require(
      outputs.map(_.ordinal) == outputs.indices.toVector &&
        outputs.map(_.name) == C03G0SourceCertificate.OutputAliases,
      "C03-G0 result outputs must use the exact fixed order and aliases"
    )
    require(catalogBinding != null, "C03-G0 result catalog binding must not be null")

    override val id: String = "c03-g0-aggregate-vector-v2"
  }
}

/** Placeholder that retains child analysis while making the report unsupported. */
final case class UnsupportedRelationalIR(
    operatorName: String,
    nodePath: String,
    diagnosticCode: RewriteReasonCode,
    inputs: Vector[RelationalIR],
    output: Vector[OutputSlot]
) extends RelationalIR {
  require(operatorName != null && operatorName.nonEmpty, "operator must not be blank")
  require(nodePath != null && nodePath.nonEmpty, "node path must not be blank")
  require(diagnosticCode != null, "diagnostic code must not be null")
  require(inputs != null, "unsupported inputs must not be null")
  require(inputs.forall(_ != null), "unsupported inputs must not contain null")
  require(output != null, "unsupported output must not be null")
  require(output.forall(_ != null), "unsupported output must not contain null")
}

/** Opaque source authorization for scalar nodes whose executable meaning
  * cannot be established from Calcite's relational tree alone.
  *
  * External QueryIR producers can explicitly represent the absence of source
  * proof, but cannot mint a certified value. The PostgreSQL compiler replays
  * the retained certificate against the complete QueryIR before accepting any
  * exact integral literal or widening cast, so copying a proof onto a mutated
  * query does not authorize the mutation.
  */
sealed trait QuerySourceProof extends Product with Serializable {
  private[yannakakis] def localPredicateCertificate
      : Option[CalciteLocalPredicates.SourceCertificate] = None
  private[yannakakis] def localIntegralPredicateCertificate
      : Option[LocalIntegralPredicateSourceCertificate]

  /** Whether this proof was minted for the exact immutable QueryIR inventories
    * presented to a downstream compiler.
    *
    * Structural equality is deliberately insufficient here. Calcite can erase
    * source-only syntax (for example, an explicit integral CAST) while
    * producing a relational tree equal to one produced from the admitted raw
    * grammar. A proof copied from that admitted query must therefore not act as
    * a bearer token for an independently analyzed tree.
    */
  private[yannakakis] def isBoundTo(query: QueryIR): Boolean
}

object QuerySourceProof {
  private[calcite] final case class CertifiedLocalPredicates(
      certificate: CalciteLocalPredicates.SourceCertificate,
      private val queryBinding: LocalIntegralPredicateQueryBinding
  ) extends QuerySourceProof {
    require(certificate != null && queryBinding != null)
    override private[yannakakis] val localIntegralPredicateCertificate
        : Option[LocalIntegralPredicateSourceCertificate] = None
    override private[yannakakis] val localPredicateCertificate = Some(certificate)
    override private[yannakakis] def isBoundTo(query: QueryIR): Boolean =
      queryBinding.matches(query)
  }

  private[calcite] def certifiedLocalPredicates(
      certificate: CalciteLocalPredicates.SourceCertificate,
      query: QueryIR
  ): QuerySourceProof =
    CertifiedLocalPredicates(certificate, LocalIntegralPredicateQueryBinding(query))

  case object Uncertified extends QuerySourceProof {
    override private[yannakakis] val localIntegralPredicateCertificate
        : Option[LocalIntegralPredicateSourceCertificate] = None

    override private[yannakakis] def isBoundTo(query: QueryIR): Boolean = false
  }

  private[calcite] final case class CertifiedLocalIntegralPredicates(
      certificate: LocalIntegralPredicateSourceCertificate,
      private val queryBinding: LocalIntegralPredicateQueryBinding
  ) extends QuerySourceProof {
    require(certificate != null, "local integral source proof must not be null")
    require(
      queryBinding != null,
      "local integral query binding must not be null"
    )

    override private[yannakakis] val localIntegralPredicateCertificate
        : Option[LocalIntegralPredicateSourceCertificate] = Some(certificate)

    override private[yannakakis] def isBoundTo(query: QueryIR): Boolean =
      queryBinding.matches(query)
  }

  private[calcite] def certifiedLocalIntegralPredicates(
      certificate: LocalIntegralPredicateSourceCertificate,
      query: QueryIR
  ): QuerySourceProof =
    CertifiedLocalIntegralPredicates(
      certificate,
      LocalIntegralPredicateQueryBinding(query)
    )
}

/** Opaque, analyzer-minted binding to the immutable QueryIR inventories that
  * were checked against raw and validated SQL evidence.
  *
  * The binding intentionally uses reference identity rather than structural
  * equality. This preserves ordinary `QueryIR.copy()` calls that retain the
  * exact immutable fields, and Java serialization preserves the shared object
  * references, while a proof transplanted onto a separately analyzed (even
  * structurally equal) query fails closed. The binding itself is not exposed
  * through the public QueryIR API.
  */
private[calcite] final class LocalIntegralPredicateQueryBinding private (
    private val root: RelationalIR,
    private val relations: Vector[RelationOccurrence],
    private val predicates: Vector[PredicateDescriptor],
    private val outputNames: Vector[String]
) extends Serializable {
  private[calcite] def matches(query: QueryIR): Boolean =
    query != null &&
      (root.asInstanceOf[AnyRef] eq query.root.asInstanceOf[AnyRef]) &&
      (relations.asInstanceOf[AnyRef] eq query.relations.asInstanceOf[AnyRef]) &&
      (predicates.asInstanceOf[AnyRef] eq query.predicates.asInstanceOf[AnyRef]) &&
      (outputNames.asInstanceOf[AnyRef] eq query.outputNames.asInstanceOf[AnyRef])

  // Keep QueryIR's data-value equality stable across independent analyses.
  // The surrounding proof already compares its certificate, and QueryIR
  // already compares each bound inventory. Authorization never uses this
  // equality; `matches` above is the only compiler boundary and deliberately
  // uses object identity.
  override def equals(other: Any): Boolean = other match {
    case _: LocalIntegralPredicateQueryBinding => true
    case _                                     => false
  }

  override def hashCode(): Int = 0

  override def toString: String = "LocalIntegralPredicateQueryBinding(<opaque>)"
}

private[calcite] object LocalIntegralPredicateQueryBinding {
  def apply(query: QueryIR): LocalIntegralPredicateQueryBinding = {
    require(query != null, "local integral proof query must not be null")
    new LocalIntegralPredicateQueryBinding(
      query.root,
      query.relations,
      query.predicates,
      query.outputNames
    )
  }
}

final case class QueryIR(
    root: RelationalIR,
    relations: Vector[RelationOccurrence],
    predicates: Vector[PredicateDescriptor],
    outputNames: Vector[String],
    sourceProof: QuerySourceProof = QuerySourceProof.Uncertified,
    textKeyProof: Option[PostgreSqlTextKeyProof] = None
) {
  require(root != null, "query root must not be null")
  require(relations != null, "query relations must not be null")
  require(relations.forall(_ != null), "query relations must not contain null")
  require(predicates != null, "query predicates must not be null")
  require(predicates.forall(_ != null), "query predicates must not contain null")
  require(outputNames != null, "query output names must not be null")
  require(outputNames.forall(_ != null), "query output names must not contain null")
  require(sourceProof != null, "query source proof must not be null")
  require(textKeyProof != null, "text-key proof option must not be null")
  require(
    outputNames.size == root.output.size,
    "query output names must match the root output arity"
  )
  require(
    relations.map(_.id.value) == relations.indices.toVector,
    "relation occurrences must use stable contiguous traversal ids"
  )
  require(
    relations == RelationalIR.relationInventory(root),
    "global relations must exactly match the relational scan inventory"
  )
  require(
    predicates.map(_.ordinal) == predicates.indices.toVector,
    "predicates must use stable contiguous traversal ordinals"
  )
  require(
    predicates == RelationalIR.predicateInventory(root),
    "global predicates must exactly match the relational predicate inventory"
  )
  root match {
    case count: GlobalCountStarIR =>
      require(
        count.countOutput.name == outputNames.head,
        "COUNT(*) result name must match the certified output name"
      )
    case count: GroupedCountStarIR =>
      require(
        count.output.map(_.name) == outputNames,
        "grouped COUNT(*) result names must match the certified outputs"
      )
    case count: DistributedGroupedCountStarIR =>
      require(
        count.output.map(_.name) == outputNames,
        "distributed grouped COUNT(*) result names must match the certified outputs"
      )
    case extremum: GlobalGuardedExtremumIR =>
      require(
        extremum.extremumOutput.name == outputNames.head,
        "guarded extremum result name must match the certified output name"
      )
    case aggregate: C03G0AggregateVectorIR =>
      require(
        aggregate.output.map(_.name) == outputNames,
        "C03-G0 result names must match the certified fixed outputs"
      )
    case _ => ()
  }
  root match {
    case count: GlobalCountStarIR =>
      require(
        !RelationalIR.containsAggregateScalar(count.input),
        "COUNT(*) may occur exactly once as the sole aggregate root output"
      )
    case count: GroupedCountStarIR =>
      require(
        !RelationalIR.containsAggregateScalar(count.input),
        "grouped COUNT(*) may occur exactly once at the aggregate root"
      )
    case count: DistributedGroupedCountStarIR =>
      require(
        !RelationalIR.containsAggregateScalar(count.input),
        "distributed grouped COUNT(*) may occur exactly once at the aggregate root"
      )
    case extremum: GlobalGuardedExtremumIR =>
      require(
        !RelationalIR.containsAggregateScalar(extremum.input),
        "guarded extremum may occur exactly once as the sole aggregate root output"
      )
    case aggregate: C03G0AggregateVectorIR =>
      require(
        !RelationalIR.containsAggregateScalar(aggregate.input),
        "C03-G0 aggregate scalars may occur exactly once in their terminal root"
      )
    case other =>
      require(
        !RelationalIR.containsAggregateScalar(other),
        "aggregate scalars may occur only in their certified root wrappers"
      )
  }

  /** Result output names, positionally overlaid on relational lineage.
    *
    * Calcite may uniquify duplicate star-expansion labels in both a `RelNode`
    * and its validated `RelRoot`. Framework-backed SQL planning therefore
    * supplies PostgreSQL labels certified from the raw select list; legacy
    * relational callers retain the validated-name fallback. Certified
    * aggregate wrappers supply their exact admitted implicit or explicit
    * names separately.
    */
  val output: Vector[OutputSlot] = root match {
    case count: GlobalCountStarIR => count.output
    case count: GroupedCountStarIR => count.output
    case count: DistributedGroupedCountStarIR => count.output
    case extremum: GlobalGuardedExtremumIR => extremum.output
    case aggregate: C03G0AggregateVectorIR => aggregate.output
    case _ =>
      root.output.zip(outputNames).map {
        case (slot, name) => slot.copy(name = name)
      }
  }

  val resultShape: QueryResultShape = root match {
    case count: GlobalCountStarIR =>
      QueryResultShape.GlobalCountStar(
        count.countOutput,
        count.sourceCertificate.aliasMode
      )
    case count: GroupedCountStarIR =>
      QueryResultShape.GroupedCountStar(
        count.groupOutput,
        count.countOutput,
        count.sourceCertificate.countAliasMode
      )
    case count: DistributedGroupedCountStarIR =>
      QueryResultShape.DistributedGroupedCountStar(
        count.groupOutputs,
        count.countOutput,
        count.sourceCertificate.countAliasMode
      )
    case extremum: GlobalGuardedExtremumIR =>
      QueryResultShape.GlobalGuardedExtremum(
        extremum.extremum.function,
        extremum.guardColumn,
        extremum.extremumOutput,
        extremum.sourceCertificate.aliasMode
      )
    case aggregate: C03G0AggregateVectorIR =>
      QueryResultShape.C03G0AggregateVector(
        aggregate.groupColumn.ref,
        aggregate.measureColumn.ref,
        aggregate.output,
        aggregate.catalogBinding
      )
    case _ => QueryResultShape.EnumerationProjection
  }
}

/** Complete analysis, including partial IR when the query is unsupported. */
final case class AnalysisReport(
    query: QueryIR,
    diagnostics: Vector[RewriteDiagnostic]
) {
  require(query != null, "analysis query must not be null")
  require(diagnostics != null, "analysis diagnostics must not be null")
  require(diagnostics.forall(_ != null), "diagnostics must not contain null")

  def isSupported: Boolean = diagnostics.isEmpty

  def supportedQuery: Either[Vector[RewriteDiagnostic], QueryIR] =
    if (isSupported) Right(query) else Left(diagnostics)

  def hypergraph: Either[Vector[RewriteDiagnostic], QueryHypergraph] =
    if (isSupported) QueryHypergraphConverter.convert(query)
    else Left(diagnostics)
}
