package at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.{
  BagFactorPlanValidationError,
  ConfiguredBagFactorPlanValidationError
}
import at.ac.tuwien.dbai.yannakakis.api.RewriteDiagnostic
import at.ac.tuwien.dbai.yannakakis.core.{
  BagId,
  ColumnId,
  EdgeId,
  QueryHypergraph,
  RelationOccurrenceId,
  VariableId
}
import at.ac.tuwien.dbai.yannakakis.frontend.calcite.ColumnRef
import at.ac.tuwien.dbai.yannakakis.api.YannakakisReductionMode

sealed trait PostgreSqlRenderingError extends Product with Serializable {
  def code: String
  def message: String
}

object PostgreSqlRenderingError {
  final case class MissingValue(path: String) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.input.missing"
    val message = s"Missing $path"
  }

  final case class QueryConversionFailure(
      diagnostics: Vector[RewriteDiagnostic]
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.query.unsupported"
    val message = s"Query IR cannot be converted losslessly (${diagnostics.size} diagnostics)"
  }

  final case class QueryHypergraphMismatch(
      expected: QueryHypergraph,
      actual: QueryHypergraph
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.provenance.hypergraph"
    val message = "Converted QueryIR hypergraph differs from CandidateSearchInput"
  }

  final case class PlanValidationFailure(
      errors: Vector[BagFactorPlanValidationError]
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.plan.invalid"
    val message = s"Bag-factor plan validation failed (${errors.size} errors)"
  }

  final case class ConfiguredPlanValidationFailure(
      errors: Vector[ConfiguredBagFactorPlanValidationError]
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.physical-plan.invalid"
    val message =
      s"Configured bag-factor plan validation failed (${errors.size} errors)"
  }

  final case class InvalidIdentifier(
      path: String,
      value: String,
      reason: String
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.identifier.invalid"
    val message = s"Identifier at $path is not renderable: $reason"
  }

  final case class InvalidQualifiedName(
      relationOccurrenceId: RelationOccurrenceId,
      qualifiedName: Vector[String],
      reason: String
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.relation.qualified-name"
    val message = s"Relation occurrence ${relationOccurrenceId.value} has an invalid qualified name: $reason"
  }

  final case class DuplicateSourceColumn(
      relationOccurrenceId: RelationOccurrenceId,
      sourceName: String,
      ordinals: Vector[Int]
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.relation.duplicate-column"
    val message = s"Relation occurrence ${relationOccurrenceId.value} has ambiguous source column '$sourceName'"
  }

  final case class UnsupportedScalar(
      path: String,
      digest: String,
      reason: String
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.scalar.unsupported"
    val message = s"Scalar at $path is unsupported: $reason ($digest)"
  }

  final case class UnsupportedGroupedCountKey(
      columnRef: ColumnRef,
      reason: String
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.aggregate.grouped-count-key"
    val message = s"Grouped COUNT(*) key ${columnRef.id} is unsupported: $reason"
  }

  final case class GroupedCountRequiresFullTwoPass(
      mode: YannakakisReductionMode
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.aggregate.grouped-count-reduction-mode"
    val message =
      s"Grouped COUNT(*) frequency propagation requires full-two-pass reduction, not ${mode.id}"
  }

  final case class UnsupportedDistributedGroupedCountKey(
      columnRefs: Vector[ColumnRef],
      reason: String
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.aggregate.distributed-grouped-count-key"
    val message =
      s"Distributed grouped COUNT(*) keys ${columnRefs.map(_.id).mkString(",")} are unsupported: $reason"
  }

  final case class DistributedGroupedCountRequiresFullTwoPass(
      mode: YannakakisReductionMode
  ) extends PostgreSqlRenderingError {
    val code =
      "postgresql.rendering.aggregate.distributed-grouped-count-reduction-mode"
    val message =
      s"Distributed grouped COUNT(*) propagation requires full-two-pass reduction, not ${mode.id}"
  }

  final case class UnsupportedC03G0Owner(
      groupColumn: ColumnRef,
      measureColumn: ColumnRef,
      reason: String
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.aggregate.c03-g0-owner"
    val message =
      s"C03-G0 columns ${groupColumn.id},${measureColumn.id} are unsupported: $reason"
  }

  final case class C03G0RequiresFullTwoPass(
      mode: YannakakisReductionMode
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.aggregate.c03-g0-reduction-mode"
    val message =
      s"C03-G0 aggregate propagation requires full-two-pass reduction, not ${mode.id}"
  }

  final case class PredicateContractViolation(
      predicateOrdinal: Int,
      detail: String
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.predicate.contract"
    val message = s"Predicate $predicateOrdinal is inconsistent with its classification: $detail"
  }

  final case class AmbiguousRelationEdgeMapping(
      relationOccurrenceId: RelationOccurrenceId,
      edgeIds: Vector[EdgeId]
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.mapping.relation-edge"
    val message = s"Relation occurrence ${relationOccurrenceId.value} maps to ${edgeIds.size} hyperedges"
  }

  final case class UnknownBindingColumn(
      edgeId: EdgeId,
      columnId: ColumnId
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.mapping.binding-column"
    val message = s"Edge ${edgeId.value} binding refers to unknown column $columnId"
  }

  final case class AmbiguousColumnOwner(
      columnRef: ColumnRef,
      candidateBagIds: Vector[BagId]
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.mapping.column-owner"
    val message = s"Column ${columnRef.id} resolves to ${candidateBagIds.size} owner bags"
  }

  final case class MissingSupportBinding(
      bagId: BagId,
      variableId: VariableId
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.mapping.support-binding"
    val message = s"Bag ${bagId.value} support cover has no binding for variable ${variableId.value}"
  }

  final case class MissingOwnedBinding(
      bagId: BagId,
      variableId: VariableId
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.mapping.owned-binding-missing"
    val message = s"Bag ${bagId.value} cannot elide support because its owned edges have no binding for variable ${variableId.value}"
  }

  final case class OwnedBindingOutsideBag(
      bagId: BagId,
      edgeId: EdgeId,
      variableId: VariableId
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.mapping.owned-binding"
    val message = s"Owned edge ${edgeId.value} binds variable ${variableId.value} outside bag ${bagId.value}"
  }

  final case class NonUniformVariableTypes(
      variableId: VariableId,
      columnsAndTypes: Vector[(ColumnId, String)]
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.type.non-uniform"
    val message = s"Variable ${variableId.value} has non-uniform structured SQL types"
  }

  final case class UnsupportedStructuralType(
      variableId: VariableId,
      sqlTypeName: String
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.type.unsupported"
    val message = s"Variable ${variableId.value} uses unsupported structural type $sqlTypeName"
  }

  final case class InconsistentStructuralTypeMetadata(
      variableId: VariableId,
      columnId: ColumnId,
      sqlTypeName: String,
      dataType: String,
      nullable: Boolean
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.type.inconsistent-metadata"
    val message = s"Variable ${variableId.value} column $columnId has inconsistent structured and full type metadata"
  }

  final case class NonJoinStructuralVariable(
      variableId: VariableId,
      relationOccurrenceIds: Vector[RelationOccurrenceId]
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.variable.non-join"
    val message = s"Variable ${variableId.value} spans fewer than two relation occurrences"
  }

  final case class GeneratedNameCollision(
      generatedName: String,
      relationOccurrenceId: RelationOccurrenceId
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.identifier.generated-collision"
    val message = s"Generated CTE name '$generatedName' shadows relation occurrence ${relationOccurrenceId.value}"
  }

  final case class InvalidResourceLimit(
      resource: PostgreSqlRenderingResource,
      value: Long,
      reason: String
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.resource.invalid"
    val message = s"${resource.code} limit $value is invalid: $reason"
  }

  final case class ResourceLimitExceeded(
      resource: PostgreSqlRenderingResource,
      required: BigInt,
      limit: Long
  ) extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.resource.exceeded"
    val message = s"${resource.code} requires $required items, exceeding limit $limit"
  }

  final case class ModelInvariantViolation(detail: String)
      extends PostgreSqlRenderingError {
    val code = "postgresql.rendering.model.invariant"
    val message = detail
  }

  def canonical(
      errors: Iterable[PostgreSqlRenderingError]
  ): Vector[PostgreSqlRenderingError] =
    errors.iterator.toVector.distinct.sortBy(error => (error.code, error.message))
}
