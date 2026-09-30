package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import at.ac.tuwien.dbai.yannakakis.api.{
  DiagnosticSeverity,
  RewriteDiagnostic,
  RewriteReasonCode
}
import at.ac.tuwien.dbai.yannakakis.core.{
  ColumnId,
  RelationOccurrenceId
}
import org.apache.calcite.adapter.jdbc.JdbcTableScan
import org.apache.calcite.plan.RelOptUtil
import org.apache.calcite.rel.{RelNode, RelRoot}
import org.apache.calcite.rel.core.{Aggregate, AggregateCall, Filter, Join, Project, TableScan}
import org.apache.calcite.rel.logical.{LogicalAggregate, LogicalProject, LogicalTableScan}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeField}
import org.apache.calcite.rex.{RexBuilder, RexCall, RexInputRef, RexLiteral, RexNode, RexUtil}
import org.apache.calcite.sql.SqlKind
import org.apache.calcite.sql.fun.{SqlCountAggFunction, SqlMinMaxAggFunction}
import org.apache.calcite.sql.`type`.{SqlTypeName, SqlTypeUtil}

import scala.annotation.nowarn
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** Converts Calcite relational plans into occurrence-safe immutable IR. */
object CalciteQueryAnalyzer {
  /** Exact scan classes whose nodes carry no pushed predicate or projection. */
  private[calcite] def isSupportedBaseScanClass(scanClass: Class[_]): Boolean =
    scanClass == classOf[LogicalTableScan] ||
      scanClass == classOf[JdbcTableScan]

  def analyze(root: RelNode): AnalysisReport = {
    require(root != null, "Calcite root must not be null")
    ensureLocalIntegralPredicateCertificate(
      analyzeRelational(root),
      certificate = None
    )
  }

  /** Compatibility entry point for callers without raw SQL label evidence. */
  def analyze(root: RelRoot): AnalysisReport = {
    require(root != null, "Calcite relational root must not be null")
    ensureLocalIntegralPredicateCertificate(
      analyzeRelational(root),
      certificate = None
    )
  }

  private def analyzeRelational(root: RelNode): AnalysisReport =
    new Analyzer().analyze(root, root.getRowType.getFieldNames.asScala.toVector)

  private def analyzeRelational(root: RelRoot): AnalysisReport =
    analyzeRelational(
      root,
      root.validatedRowType.getFieldNames.asScala.toVector
    )

  private def analyzeRelational(
      root: RelRoot,
      outputNames: Vector[String],
      allowLocalPredicates: Boolean = false
  ): AnalysisReport =
    new Analyzer(allowLocalPredicates).analyze(
      // RelRoot.fields, rather than validatedRowType names alone, defines the
      // mapping from relational fields to SQL result positions. project()
      // materializes that mapping and removes hidden fields. Framework-backed
      // SQL planning supplies a separately source-certified PostgreSQL label
      // vector because Calcite's validated names are not authoritative for
      // star-expanded duplicates or unaliased expressions.
      root.project(),
      outputNames
    )

  private def analyzeEnumeration(
      planned: CalciteSqlPlannedQuery
  ): AnalysisReport = {
    val expectedArity = planned.root.validatedRowType.getFieldCount
    planned.outputLabelEvidence match {
      case CalciteSqlOutputLabelEvidence.Certified(names)
          if names.size == expectedArity =>
        analyzeRelational(planned.root, names, planned.localPredicateCertificate.nonEmpty)
      case CalciteSqlOutputLabelEvidence.Certified(names) =>
        rejectUncertifiedOutputLabels(
          analyzeRelational(planned.root),
          s"source-certified output-label arity ${names.size} differed from validated arity $expectedArity"
        )
      case CalciteSqlOutputLabelEvidence.Rejected(reason) =>
        rejectUncertifiedOutputLabels(analyzeRelational(planned.root), reason)
      case CalciteSqlOutputLabelEvidence.Uncertified =>
        // Compatibility-only planners do not expose raw SQL evidence. Their
        // historical RelRoot contract remains unchanged; the framework-backed
        // PostgreSQL rewrite path never takes this branch.
        analyzeRelational(planned.root)
    }
  }

  private def rejectUncertifiedOutputLabels(
      report: AnalysisReport,
      reason: String
  ): AnalysisReport =
    report.copy(
      diagnostics = report.diagnostics :+ RewriteDiagnostic(
        code = RewriteReasonCode.UnsupportedQueryShape,
        message = "PostgreSQL result labels could not be source-certified",
        severity = DiagnosticSeverity.Warning,
        context = Map(
          "sourceBoundary" -> "result-labels",
          "reason" -> reason
        )
      )
    )

  /**
    * Source-certified entry point used by the SQL rewrite pipeline. Existing
    * RelRoot/RelNode overloads intentionally retain their aggregate-rejecting
    * behavior.
    */
  private[yannakakis] def analyze(
      planned: CalciteSqlPlannedQuery
  ): AnalysisReport = {
    require(planned != null, "Calcite planned query must not be null")
    val localSource = planned.localPredicateCertificate
    def certify(report: AnalysisReport): AnalysisReport =
      ensureLocalIntegralPredicateCertificate(report, None, localSource)
    val report = planned.sourceEvidence match {
      case CalciteSqlSourceEvidence.CertifiedGlobalCountStar(certificate) =>
        certify(analyzeCertifiedGlobalCountStar(planned.root, certificate, localSource.nonEmpty))
      case CalciteSqlSourceEvidence.CertifiedGroupedCountStar(certificate) =>
        certify(analyzeCertifiedGroupedCountStar(planned.root, certificate, localSource.nonEmpty))
      case CalciteSqlSourceEvidence.CertifiedDistributedGroupedCountStar(
            certificate
          ) =>
        certify(analyzeCertifiedDistributedGroupedCountStar(planned.root, certificate, localSource.nonEmpty))
      case CalciteSqlSourceEvidence.CertifiedGlobalGuardedExtremum(certificate) =>
        certify(analyzeCertifiedGlobalGuardedExtremum(planned.root, certificate, localSource.nonEmpty))
      case CalciteSqlSourceEvidence.CertifiedC03G0(certificate) =>
        planned.aggregateLanguageProfile match {
          case CalciteAggregateLanguageProfile.C03G0V2(catalogBinding)
              if catalogBinding == certificate.catalogBinding =>
            ensureLocalIntegralPredicateCertificate(
              analyzeCertifiedC03G0(planned.root, certificate),
              certificate = None
            )
          case _ =>
            ensureLocalIntegralPredicateCertificate(
              ensureAggregateRejected(
                analyzeRelational(planned.root),
                "C03-G0 source evidence requires its exact construction-time V2 profile",
                Map("sourceCertificate" -> "c03-g0-v2")
              ),
              certificate = None
            )
        }
      case CalciteSqlSourceEvidence.CertifiedNonAggregateLocalIntegralPredicates(
            certificate
          ) =>
        ensureLocalIntegralPredicateCertificate(
          analyzeEnumeration(planned),
          Some(certificate),
          localSource
        )
      case CalciteSqlSourceEvidence.RejectedAggregate(reason) =>
        ensureLocalIntegralPredicateCertificate(
          ensureAggregateRejected(
            analyzeRelational(planned.root),
            "raw SQL aggregate shape was rejected",
            Map("sourceReason" -> reason.id)
          ),
          certificate = None
        )
      case CalciteSqlSourceEvidence.Uncertified | CalciteSqlSourceEvidence.NonAggregate =>
        certify(analyzeEnumeration(planned))
    }
    planned.textKeyCatalog.fold(report)(_.certify(report))
  }

  private def analyzeCertifiedGlobalCountStar(
      root: RelRoot,
      certificate: GlobalCountStarSourceCertificate,
      allowLocalPredicates: Boolean = false
  ): AnalysisReport = {
    exactGlobalCountAggregate(root) match {
      case Some(aggregate) =>
        val validatedNames = root.validatedRowType.getFieldNames.asScala.toVector
        new Analyzer(allowLocalPredicates).analyzeGlobalCountStar(
          aggregate,
          validatedNames,
          certificate
        )
      case None =>
        ensureAggregateRejected(
          analyzeRelational(root),
          "source-certified COUNT(*) did not match the exact relational aggregate contract",
          Map("sourceCertificate" -> "global-count-star")
        )
    }
  }

  private def exactGlobalCountAggregate(root: RelRoot): Option[LogicalAggregate] =
    root.project() match {
      case aggregate: LogicalAggregate if exactGlobalCountAggregate(aggregate, root) =>
        Some(aggregate)
      case _ => None
    }

  private def analyzeCertifiedGroupedCountStar(
      root: RelRoot,
      certificate: GroupedCountStarSourceCertificate,
      allowLocalPredicates: Boolean = false
  ): AnalysisReport = {
    exactGroupedCountAggregate(root) match {
      case Some(aggregate) =>
        val validatedNames = root.validatedRowType.getFieldNames.asScala.toVector
        new Analyzer(allowLocalPredicates).analyzeGroupedCountStar(
          aggregate,
          validatedNames,
          certificate
        )
      case None =>
        ensureAggregateRejected(
          analyzeRelational(root),
          "source-certified grouped COUNT(*) did not match the exact relational aggregate contract",
          Map("sourceCertificate" -> "grouped-count-star")
        )
    }
  }

  private def exactGroupedCountAggregate(
      root: RelRoot
  ): Option[LogicalAggregate] = {
    root.project() match {
      case aggregate: LogicalAggregate
          if exactGroupedCountAggregate(aggregate, root) =>
        Some(aggregate)
      case project: LogicalProject =>
        project.getInput match {
          case aggregate: LogicalAggregate
              if exactIdentityAggregateProjection(
                project,
                aggregate,
                root,
                expectedArity = 2
              ) &&
                exactGroupedCountAggregate(aggregate, root) =>
            Some(aggregate)
          case _ => None
        }
      case _ => None
    }
  }

  private def analyzeCertifiedDistributedGroupedCountStar(
      root: RelRoot,
      certificate: DistributedGroupedCountStarSourceCertificate,
      allowLocalPredicates: Boolean = false
  ): AnalysisReport = {
    exactDistributedGroupedCountAggregate(root, certificate.groupKeyCount) match {
      case Some((aggregate, groupInputOrdinals)) =>
        val validatedFields =
          root.validatedRowType.getFieldList.asScala.toVector
        new Analyzer(allowLocalPredicates).analyzeDistributedGroupedCountStar(
          aggregate,
          groupInputOrdinals,
          validatedFields,
          certificate
        )
      case None =>
        ensureAggregateRejected(
          analyzeRelational(root),
          "source-certified distributed grouped COUNT(*) did not match the exact relational aggregate contract",
          Map("sourceCertificate" -> "distributed-grouped-count-star")
        )
    }
  }

  /**
    * Returns the certified grouping input ordinals in validated output order. Calcite
    * stores an Aggregate's group set in input-ordinal order, while RelRoot may
    * retain a projection that restores the SQL select/group order.
    */
  private def exactDistributedGroupedCountAggregate(
      root: RelRoot,
      keyCount: Int
  ): Option[(LogicalAggregate, Vector[Int])] = {
    root.project() match {
      case aggregate: LogicalAggregate
          if exactDistributedGroupedCountAggregate(aggregate, keyCount) &&
            exactDistributedAggregateOutputMapping(
              (0 to keyCount).toVector,
              aggregate,
              root
            ) =>
        Some(aggregate -> distributedGroupInputOrdinals(aggregate, (0 until keyCount).toVector))
      case project: LogicalProject =>
        project.getInput match {
          case aggregate: LogicalAggregate
              if exactDistributedGroupedCountAggregate(aggregate, keyCount) =>
            exactDistributedAggregateProjection(project, aggregate, root).map {
              mapping =>
                aggregate -> distributedGroupInputOrdinals(
                  aggregate,
                  mapping.take(keyCount)
                )
            }
          case _ => None
        }
      case _ => None
    }
  }

  private def exactDistributedAggregateProjection(
      project: LogicalProject,
      aggregate: LogicalAggregate,
      root: RelRoot
  ): Option[Vector[Int]] = {
    val expressions = project.getProjects.asScala.toVector
    val mapping = expressions.collect { case ref: RexInputRef => ref.getIndex }
    val projectFields = project.getRowType.getFieldList.asScala.toVector
    val aggregateFields = aggregate.getRowType.getFieldList.asScala.toVector
    val validatedFields = root.validatedRowType.getFieldList.asScala.toVector
    val keyCount = aggregate.getGroupCount
    if (
      project.getClass == classOf[LogicalProject] &&
      project.getHints.isEmpty &&
      expressions.size == keyCount + 1 &&
      mapping.size == keyCount + 1 &&
      mapping.take(keyCount).toSet == (0 until keyCount).toSet &&
      mapping(keyCount) == keyCount &&
      mapping.distinct.size == keyCount + 1 &&
      projectFields.size == keyCount + 1 &&
      aggregateFields.size == keyCount + 1 &&
      validatedFields.size == keyCount + 1 &&
      projectFields.indices.forall { index =>
        sameExactType(
          projectFields(index).getType,
          aggregateFields(mapping(index)).getType
        ) && sameExactType(
          projectFields(index).getType,
          validatedFields(index).getType
        )
      } &&
      exactDistributedAggregateOutputMapping(mapping, aggregate, root)
    ) Some(mapping)
    else None
  }

  private def exactDistributedAggregateOutputMapping(
      mapping: Vector[Int],
      aggregate: LogicalAggregate,
      root: RelRoot
  ): Boolean = {
    val aggregateFields = aggregate.getRowType.getFieldList.asScala.toVector
    val validatedFields = root.validatedRowType.getFieldList.asScala.toVector
    val keyCount = aggregate.getGroupCount
    mapping.size == keyCount + 1 &&
    mapping.take(keyCount).toSet == (0 until keyCount).toSet &&
    mapping(keyCount) == keyCount &&
    mapping.distinct.size == keyCount + 1 &&
    aggregateFields.size == keyCount + 1 &&
    validatedFields.size == keyCount + 1 &&
    validatedFields.indices.forall(index =>
      sameExactType(
        validatedFields(index).getType,
        aggregateFields(mapping(index)).getType
      )
    )
  }

  private def distributedGroupInputOrdinals(
      aggregate: LogicalAggregate,
      aggregateOutputOrdinals: Vector[Int]
  ): Vector[Int] = {
    val groupInputOrdinals = aggregate.getGroupSet.asList.asScala.toVector
      .map(_.intValue())
    aggregateOutputOrdinals.map(groupInputOrdinals)
  }

  private def exactIdentityAggregateProjection(
      project: LogicalProject,
      aggregate: LogicalAggregate,
      root: RelRoot,
      expectedArity: Int
  ): Boolean = {
    require(expectedArity > 0, "identity aggregate arity must be positive")
    val expressions = project.getProjects.asScala.toVector
    val projectFields = project.getRowType.getFieldList.asScala.toVector
    val aggregateFields = aggregate.getRowType.getFieldList.asScala.toVector
    val validatedFields = root.validatedRowType.getFieldList.asScala.toVector
    project.getClass == classOf[LogicalProject] &&
    project.getHints.isEmpty &&
    expressions.size == expectedArity &&
    expressions.zipWithIndex.forall {
      case (ref: RexInputRef, ordinal) => ref.getIndex == ordinal
      case _                           => false
    } &&
    projectFields.size == expectedArity &&
    projectFields.size == aggregateFields.size &&
    projectFields.size == validatedFields.size &&
    projectFields.indices.forall(index =>
      sameExactType(projectFields(index).getType, aggregateFields(index).getType) &&
        sameExactType(projectFields(index).getType, validatedFields(index).getType)
    )
  }

  private def analyzeCertifiedC03G0(
      root: RelRoot,
      certificate: C03G0SourceCertificate
  ): AnalysisReport = {
    exactC03G0Aggregate(root) match {
      case Some(aggregate) =>
        val validatedFields =
          root.validatedRowType.getFieldList.asScala.toVector
        new Analyzer().analyzeC03G0(
          aggregate,
          validatedFields,
          certificate
        )
      case None =>
        ensureAggregateRejected(
          analyzeRelational(root),
          "source-certified C03-G0 did not match the exact relational aggregate contract",
          Map("sourceCertificate" -> "c03-g0-v2")
        )
    }
  }

  private def exactC03G0Aggregate(root: RelRoot): Option[LogicalAggregate] =
    root.project() match {
      case aggregate: LogicalAggregate
          if exactC03G0Aggregate(aggregate, root) =>
        Some(aggregate)
      case project: LogicalProject =>
        project.getInput match {
          case aggregate: LogicalAggregate
              if exactIdentityAggregateProjection(
                project,
                aggregate,
                root,
                expectedArity = 5
              ) && exactC03G0Aggregate(aggregate, root) =>
            Some(aggregate)
          case _ => None
        }
      case _ => None
    }

  private def exactC03G0Aggregate(
      aggregate: LogicalAggregate,
      root: RelRoot
  ): Boolean = {
    val calls = aggregate.getAggCallList.asScala.toVector
    val aggregateFields = aggregate.getRowType.getFieldList.asScala.toVector
    val validatedFields = root.validatedRowType.getFieldList.asScala.toVector
    val inputFields = aggregate.getInput.getRowType.getFieldList.asScala.toVector
    val groupOrdinals = aggregate.getGroupSet.asList.asScala.toVector
      .map(_.intValue())
    val groupOrdinal = groupOrdinals.headOption
    val groupInputType = groupOrdinal.flatMap(inputFields.lift).map(_.getType)
    val measureArguments = calls.lift(2).toVector.flatMap(
      _.getArgList.asScala.toVector.map(_.intValue())
    )
    val measureOrdinal = measureArguments.headOption
    val measureInputType = measureOrdinal.flatMap(inputFields.lift).map(_.getType)

    aggregate.getClass == classOf[LogicalAggregate] &&
    aggregate.getInput != null &&
    aggregate.getHints.isEmpty &&
    hasNoIndicator(aggregate) &&
    aggregate.getGroupCount == 1 &&
    aggregate.getGroupSet.cardinality() == 1 &&
    groupOrdinals.size == 1 &&
    aggregate.getGroupSets != null &&
    aggregate.getGroupSets.size == 1 &&
    aggregate.getGroupSets.get(0) == aggregate.getGroupSet &&
    Aggregate.isSimple(aggregate) &&
    aggregate.getGroupType == Aggregate.Group.SIMPLE &&
    calls.size == 4 &&
    aggregateFields.size == 5 &&
    validatedFields.size == 5 &&
    validatedFields.map(_.getName) == C03G0SourceCertificate.OutputAliases &&
    groupInputType.exists(value =>
      value.getSqlTypeName == SqlTypeName.VARCHAR && value.isNullable &&
        sameExactType(value, aggregateFields(0).getType)
    ) &&
    sameExactType(aggregateFields(0).getType, validatedFields(0).getType) &&
    exactGlobalCountCall(calls(0), aggregateFields(1).getType) &&
    sameExactType(aggregateFields(1).getType, validatedFields(1).getType) &&
    groupOrdinal.exists(
      exactC03G0CountColumnCall(
        calls(1),
        _,
        aggregateFields(2).getType
      )
    ) &&
    sameExactType(aggregateFields(2).getType, validatedFields(2).getType) &&
    measureInputType.exists(value =>
      value.getSqlTypeName == SqlTypeName.BIGINT && !value.isNullable
    ) &&
    measureOrdinal.exists(
      exactC03G0ExtremumCall(
        calls(2),
        _,
        inputFields,
        aggregateFields(3).getType,
        GlobalGuardedExtremumFunction.Min
      )
    ) &&
    sameExactType(aggregateFields(3).getType, validatedFields(3).getType) &&
    measureOrdinal.exists(
      exactC03G0ExtremumCall(
        calls(3),
        _,
        inputFields,
        aggregateFields(4).getType,
        GlobalGuardedExtremumFunction.Max
      )
    ) &&
    sameExactType(aggregateFields(4).getType, validatedFields(4).getType)
  }

  private def exactC03G0CountColumnCall(
      call: AggregateCall,
      argumentOrdinal: Int,
      outputType: RelDataType
  ): Boolean = {
    val aggregation = call.getAggregation
    val arguments = call.getArgList.asScala.toVector.map(_.intValue())
    aggregation != null &&
    aggregation.getClass == classOf[SqlCountAggFunction] &&
    aggregation.getKind == SqlKind.COUNT &&
    aggregation.getName == "COUNT" &&
    !call.isDistinct &&
    !call.isApproximate &&
    !call.ignoreNulls &&
    !call.hasFilter &&
    call.filterArg < 0 &&
    call.distinctKeys == null &&
    arguments == Vector(argumentOrdinal) &&
    call.rexList != null &&
    call.rexList.isEmpty &&
    call.getCollation != null &&
    call.getCollation.getFieldCollations.isEmpty &&
    call.getType != null &&
    call.getType.getSqlTypeName == SqlTypeName.BIGINT &&
    !call.getType.isNullable &&
    sameExactType(call.getType, outputType)
  }

  private def exactC03G0ExtremumCall(
      call: AggregateCall,
      argumentOrdinal: Int,
      inputFields: Vector[RelDataTypeField],
      outputType: RelDataType,
      function: GlobalGuardedExtremumFunction
  ): Boolean = {
    val aggregation = call.getAggregation
    val expectedKind = function match {
      case GlobalGuardedExtremumFunction.Min => SqlKind.MIN
      case GlobalGuardedExtremumFunction.Max => SqlKind.MAX
    }
    val arguments = call.getArgList.asScala.toVector.map(_.intValue())
    val argumentType = inputFields.lift(argumentOrdinal).map(_.getType)
    aggregation != null &&
    aggregation.getClass == classOf[SqlMinMaxAggFunction] &&
    aggregation.getKind == expectedKind &&
    aggregation.getName == function.sqlName &&
    !call.isDistinct &&
    !call.isApproximate &&
    !call.ignoreNulls &&
    !call.hasFilter &&
    call.filterArg < 0 &&
    call.distinctKeys == null &&
    arguments == Vector(argumentOrdinal) &&
    argumentType.exists(value =>
      value.getSqlTypeName == SqlTypeName.BIGINT && !value.isNullable
    ) &&
    call.rexList != null &&
    call.rexList.isEmpty &&
    call.getCollation != null &&
    call.getCollation.getFieldCollations.isEmpty &&
    call.getType != null &&
    call.getType.getSqlTypeName == SqlTypeName.BIGINT &&
    !call.getType.isNullable &&
    argumentType.exists(SqlTypeUtil.equalSansNullability(_, call.getType)) &&
    sameExactType(call.getType, outputType)
  }

  private def analyzeCertifiedGlobalGuardedExtremum(
      root: RelRoot,
      certificate: GlobalGuardedExtremumSourceCertificate,
      allowLocalPredicates: Boolean = false
  ): AnalysisReport = {
    exactGlobalGuardedExtremumAggregate(root, certificate) match {
      case Some(aggregate) =>
        val validatedNames = root.validatedRowType.getFieldNames.asScala.toVector
        new Analyzer(allowLocalPredicates).analyzeGlobalGuardedExtremum(
          aggregate,
          validatedNames,
          certificate
        )
      case None =>
        ensureAggregateRejected(
          analyzeRelational(root),
          "source-certified MIN/MAX did not match the exact relational aggregate contract",
          Map(
            "sourceCertificate" -> "global-guarded-extremum",
            "function" -> certificate.function.id
          )
        )
    }
  }

  private def exactGlobalGuardedExtremumAggregate(
      root: RelRoot,
      certificate: GlobalGuardedExtremumSourceCertificate
  ): Option[LogicalAggregate] =
    root.project() match {
      case aggregate: LogicalAggregate
          if exactGlobalGuardedExtremumAggregate(
            aggregate,
            root,
            certificate.function
          ) =>
        Some(aggregate)
      case _ => None
    }

  private def exactGlobalGuardedExtremumAggregate(
      aggregate: LogicalAggregate,
      root: RelRoot,
      function: GlobalGuardedExtremumFunction
  ): Boolean = {
    val calls = aggregate.getAggCallList.asScala.toVector
    val aggregateFields = aggregate.getRowType.getFieldList.asScala.toVector
    val validatedFields = root.validatedRowType.getFieldList.asScala.toVector
    val inputFields = aggregate.getInput.getRowType.getFieldList.asScala.toVector
    aggregate.getClass == classOf[LogicalAggregate] &&
    aggregate.getInput != null &&
    aggregate.getHints.isEmpty &&
    hasNoIndicator(aggregate) &&
    aggregate.getGroupCount == 0 &&
    aggregate.getGroupSet.isEmpty &&
    aggregate.getGroupSets != null &&
    aggregate.getGroupSets.size == 1 &&
    aggregate.getGroupSets.get(0).isEmpty &&
    Aggregate.isSimple(aggregate) &&
    aggregate.getGroupType == Aggregate.Group.SIMPLE &&
    calls.size == 1 &&
    aggregateFields.size == 1 &&
    validatedFields.size == 1 &&
    exactGlobalGuardedExtremumCall(
      calls.head,
      inputFields,
      aggregateFields.head.getType,
      function
    ) &&
    sameExactType(aggregateFields.head.getType, validatedFields.head.getType)
  }

  private def exactGlobalGuardedExtremumCall(
      call: AggregateCall,
      inputFields: Vector[RelDataTypeField],
      outputType: RelDataType,
      function: GlobalGuardedExtremumFunction
  ): Boolean = {
    val aggregation = call.getAggregation
    val expectedKind = function match {
      case GlobalGuardedExtremumFunction.Min => SqlKind.MIN
      case GlobalGuardedExtremumFunction.Max => SqlKind.MAX
    }
    val arguments = call.getArgList.asScala.toVector.map(_.intValue())
    val argumentType = arguments.headOption.flatMap(inputFields.lift).map(_.getType)
    aggregation != null &&
    aggregation.getClass == classOf[SqlMinMaxAggFunction] &&
    aggregation.getKind == expectedKind &&
    aggregation.getName == function.sqlName &&
    !call.isDistinct &&
    !call.isApproximate &&
    !call.ignoreNulls &&
    !call.hasFilter &&
    call.filterArg < 0 &&
    call.distinctKeys == null &&
    arguments.size == 1 &&
    argumentType.nonEmpty &&
    call.rexList != null &&
    call.rexList.isEmpty &&
    call.getCollation != null &&
    call.getCollation.getFieldCollations.isEmpty &&
    call.getType != null &&
    call.getType.isNullable &&
    SqlTypeUtil.equalSansNullability(argumentType.get, call.getType) &&
    sameExactType(call.getType, outputType)
  }

  private def exactGlobalCountAggregate(
      aggregate: LogicalAggregate,
      root: RelRoot
  ): Boolean = {
    val calls = aggregate.getAggCallList.asScala.toVector
    val aggregateFields = aggregate.getRowType.getFieldList.asScala.toVector
    val validatedFields = root.validatedRowType.getFieldList.asScala.toVector
    aggregate.getClass == classOf[LogicalAggregate] &&
    aggregate.getInput != null &&
    aggregate.getHints.isEmpty &&
    hasNoIndicator(aggregate) &&
    aggregate.getGroupCount == 0 &&
    aggregate.getGroupSet.isEmpty &&
    aggregate.getGroupSets != null &&
    aggregate.getGroupSets.size == 1 &&
    aggregate.getGroupSets.get(0).isEmpty &&
    Aggregate.isSimple(aggregate) &&
    aggregate.getGroupType == Aggregate.Group.SIMPLE &&
    calls.size == 1 &&
    aggregateFields.size == 1 &&
    validatedFields.size == 1 &&
    exactGlobalCountCall(calls.head, aggregateFields.head.getType) &&
    sameExactType(aggregateFields.head.getType, validatedFields.head.getType)
  }

  private def exactGroupedCountAggregate(
      aggregate: LogicalAggregate,
      root: RelRoot
  ): Boolean = {
    val calls = aggregate.getAggCallList.asScala.toVector
    val aggregateFields = aggregate.getRowType.getFieldList.asScala.toVector
    val validatedFields = root.validatedRowType.getFieldList.asScala.toVector
    val inputFields = aggregate.getInput.getRowType.getFieldList.asScala.toVector
    val groupOrdinals = aggregate.getGroupSet.asList.asScala.toVector
      .map(_.intValue())
    val groupInputType = groupOrdinals.headOption
      .flatMap(inputFields.lift)
      .map(_.getType)
    aggregate.getClass == classOf[LogicalAggregate] &&
    aggregate.getInput != null &&
    aggregate.getHints.isEmpty &&
    hasNoIndicator(aggregate) &&
    aggregate.getGroupCount == 1 &&
    aggregate.getGroupSet.cardinality() == 1 &&
    groupOrdinals.size == 1 &&
    aggregate.getGroupSets != null &&
    aggregate.getGroupSets.size == 1 &&
    aggregate.getGroupSets.get(0) == aggregate.getGroupSet &&
    Aggregate.isSimple(aggregate) &&
    aggregate.getGroupType == Aggregate.Group.SIMPLE &&
    calls.size == 1 &&
    aggregateFields.size == 2 &&
    validatedFields.size == 2 &&
    groupInputType.exists(sameExactType(_, aggregateFields.head.getType)) &&
    sameExactType(aggregateFields.head.getType, validatedFields.head.getType) &&
    exactGlobalCountCall(calls.head, aggregateFields(1).getType) &&
    sameExactType(aggregateFields(1).getType, validatedFields(1).getType)
  }

  private def exactDistributedGroupedCountAggregate(
      aggregate: LogicalAggregate,
      keyCount: Int
  ): Boolean = {
    val calls = aggregate.getAggCallList.asScala.toVector
    val aggregateFields = aggregate.getRowType.getFieldList.asScala.toVector
    val inputFields = aggregate.getInput.getRowType.getFieldList.asScala.toVector
    val groupOrdinals = aggregate.getGroupSet.asList.asScala.toVector
      .map(_.intValue())
    val groupInputTypes = groupOrdinals.map(inputFields.lift)
    aggregate.getClass == classOf[LogicalAggregate] &&
    aggregate.getInput != null &&
    aggregate.getHints.isEmpty &&
    hasNoIndicator(aggregate) &&
    aggregate.getGroupCount == keyCount &&
    aggregate.getGroupSet.cardinality() == keyCount &&
    groupOrdinals.size == keyCount &&
    aggregate.getGroupSets != null &&
    aggregate.getGroupSets.size == 1 &&
    aggregate.getGroupSets.get(0) == aggregate.getGroupSet &&
    Aggregate.isSimple(aggregate) &&
    aggregate.getGroupType == Aggregate.Group.SIMPLE &&
    calls.size == 1 &&
    aggregateFields.size == keyCount + 1 &&
    groupInputTypes.forall(_.nonEmpty) &&
    groupInputTypes.indices.forall(index =>
      sameExactType(
        groupInputTypes(index).get.getType,
        aggregateFields(index).getType
      )
    ) &&
    exactGlobalCountCall(calls.head, aggregateFields(keyCount).getType)
  }

  @nowarn("cat=deprecation")
  private def hasNoIndicator(aggregate: Aggregate): Boolean =
    !aggregate.indicator && aggregate.getIndicatorCount == 0

  private def exactGlobalCountCall(
      call: AggregateCall,
      outputType: RelDataType
  ): Boolean = {
    val aggregation = call.getAggregation
    aggregation != null &&
    aggregation.getClass == classOf[SqlCountAggFunction] &&
    aggregation.getKind == SqlKind.COUNT &&
    aggregation.getName == "COUNT" &&
    !call.isDistinct &&
    !call.isApproximate &&
    !call.ignoreNulls &&
    !call.hasFilter &&
    call.filterArg < 0 &&
    call.distinctKeys == null &&
    call.getArgList.isEmpty &&
    call.rexList != null &&
    call.rexList.isEmpty &&
    call.getCollation != null &&
    call.getCollation.getFieldCollations.isEmpty &&
    call.getType != null &&
    call.getType.getSqlTypeName == SqlTypeName.BIGINT &&
    !call.getType.isNullable &&
    sameExactType(call.getType, outputType)
  }

  private def sameExactType(left: RelDataType, right: RelDataType): Boolean =
    left != null && right != null && left.getFullTypeString == right.getFullTypeString

  private final case class LocalIntegralRelationalBinding(
      predicateOrdinal: Int,
      occurrenceOrdinal: Int,
      columnName: String,
      literalValue: Long,
      literalOnLeft: Boolean
  )

  private final case class IntegralOperand(
      column: Option[ScalarExpression.Column],
      literal: Option[ScalarExpression.ExactIntegralLiteral],
      sourceSqlTypeName: String,
      targetSqlTypeName: String,
      nullable: Boolean
  )

  private val integralTypesByRank = Vector("SMALLINT", "INTEGER", "BIGINT")
  private val integralTypeRanks: Map[String, Int] =
    integralTypesByRank.zipWithIndex.toMap

  private def exactIntegralWidening(
      sourceSqlTypeName: String,
      targetSqlTypeName: String
  ): Boolean =
    integralTypeRanks.get(sourceSqlTypeName).exists { sourceRank =>
      integralTypeRanks
        .get(targetSqlTypeName)
        .exists(targetRank => sourceRank < targetRank)
    }

  private def exactIntegralRange(sqlTypeName: String, value: Long): Boolean =
    sqlTypeName match {
      case "SMALLINT" => value >= Short.MinValue && value <= Short.MaxValue
      case "INTEGER"  => value >= Int.MinValue && value <= Int.MaxValue
      case "BIGINT"   => true
      case _          => false
    }

  private def exactIntegralComparisonType(
      leftSourceSqlTypeName: String,
      rightSourceSqlTypeName: String
  ): Option[String] =
    for {
      leftRank <- integralTypeRanks.get(leftSourceSqlTypeName)
      rightRank <- integralTypeRanks.get(rightSourceSqlTypeName)
    } yield integralTypesByRank(math.max(leftRank, rightRank))

  private def exactTypeDigest(
      sqlTypeName: String,
      nullable: Boolean
  ): String =
    if (nullable) sqlTypeName else s"$sqlTypeName NOT NULL"

  private def localIntegralColumnOperand(
      expression: ScalarExpression,
      metadataByColumn: Map[ColumnRef, ColumnMetadata]
  ): Option[IntegralOperand] = expression match {
    case column: ScalarExpression.Column =>
      metadataByColumn.get(column.ref).filter(metadata =>
        integralTypeRanks.contains(metadata.sqlTypeName) &&
          column.dataType == metadata.dataType &&
          column.nullable == metadata.nullable
      ).map(metadata =>
        IntegralOperand(
          column = Some(column),
          literal = None,
          sourceSqlTypeName = metadata.sqlTypeName,
          targetSqlTypeName = metadata.sqlTypeName,
          nullable = metadata.nullable
        )
      )
    case cast: ScalarExpression.ExactIntegralWideningCast =>
      cast.operand match {
        case column: ScalarExpression.Column =>
          localIntegralColumnOperand(column, metadataByColumn).filter(base =>
            base.sourceSqlTypeName == cast.sourceSqlTypeName &&
              exactIntegralWidening(
                cast.sourceSqlTypeName,
                cast.targetSqlTypeName
              ) &&
              cast.dataType == exactTypeDigest(
                cast.targetSqlTypeName,
                base.nullable
              ) &&
              cast.nullable == base.nullable
          ).map(base =>
            base.copy(targetSqlTypeName = cast.targetSqlTypeName)
          )
        case _ => None
      }
    case _ => None
  }

  private def localIntegralLiteralOperand(
      expression: ScalarExpression
  ): Option[IntegralOperand] = expression match {
    case literal: ScalarExpression.ExactIntegralLiteral
        if exactIntegralRange(literal.sqlTypeName, literal.value) &&
          literal.dataType == exactTypeDigest(
            literal.sqlTypeName,
            nullable = false
          ) =>
      Some(
        IntegralOperand(
          column = None,
          literal = Some(literal),
          sourceSqlTypeName = literal.sqlTypeName,
          targetSqlTypeName = literal.sqlTypeName,
          nullable = false
        )
      )
    case cast: ScalarExpression.ExactIntegralWideningCast =>
      cast.operand match {
        case literal: ScalarExpression.ExactIntegralLiteral =>
          localIntegralLiteralOperand(literal).filter(base =>
            base.sourceSqlTypeName == cast.sourceSqlTypeName &&
              exactIntegralWidening(
                cast.sourceSqlTypeName,
                cast.targetSqlTypeName
              ) &&
              cast.dataType == exactTypeDigest(
                cast.targetSqlTypeName,
                nullable = false
              ) &&
              !cast.nullable
          ).map(base =>
            base.copy(targetSqlTypeName = cast.targetSqlTypeName)
          )
        case _ => None
      }
    case _ => None
  }

  private def directLocalIntegralEquality(
      expression: ScalarExpression,
      metadataByColumn: Map[ColumnRef, ColumnMetadata]
  ): Option[(ScalarExpression.Column, ScalarExpression.ExactIntegralLiteral, Boolean)] =
    expression match {
      case call: ScalarExpression.Call
          if call.sqlKind == SqlKind.EQUALS.toString &&
            call.operatorName == "=" && call.deterministic &&
            call.operands.size == 2 =>
        val left = call.operands(0)
        val right = call.operands(1)

        def checked(
            columnExpression: ScalarExpression,
            literalExpression: ScalarExpression,
            literalOnLeft: Boolean
        ): Option[(ScalarExpression.Column, ScalarExpression.ExactIntegralLiteral, Boolean)] =
          for {
            column <- localIntegralColumnOperand(
              columnExpression,
              metadataByColumn
            )
            literal <- localIntegralLiteralOperand(literalExpression)
            comparisonType <- exactIntegralComparisonType(
              column.sourceSqlTypeName,
              literal.sourceSqlTypeName
            )
            if column.targetSqlTypeName == comparisonType
            if literal.targetSqlTypeName == comparisonType
            if call.nullable == column.nullable
            if call.dataType == exactTypeDigest("BOOLEAN", column.nullable)
            columnValue <- column.column
            literalValue <- literal.literal
          } yield (columnValue, literalValue, literalOnLeft)

        checked(left, right, literalOnLeft = false).orElse(
          checked(right, left, literalOnLeft = true)
        )
      case _ => None
    }

  private def directLocalIntegralEqualityShape(
      expression: ScalarExpression
  ): Option[(ColumnRef, ScalarExpression.ExactIntegralLiteral, Boolean)] = {
    def column(expression: ScalarExpression): Option[ColumnRef] = expression match {
      case value: ScalarExpression.Column => Some(value.ref)
      case ScalarExpression.ExactIntegralWideningCast(
            value: ScalarExpression.Column,
            _,
            _,
            _,
            _,
            _
          ) => Some(value.ref)
      case _ => None
    }

    def literal(
        expression: ScalarExpression
    ): Option[ScalarExpression.ExactIntegralLiteral] = expression match {
      case value: ScalarExpression.ExactIntegralLiteral => Some(value)
      case ScalarExpression.ExactIntegralWideningCast(
            value: ScalarExpression.ExactIntegralLiteral,
            _,
            _,
            _,
            _,
            _
          ) => Some(value)
      case _ => None
    }

    expression match {
      case ScalarExpression.Call(
            sqlKind,
            operatorName,
            Vector(left, right),
            _,
            _,
            _,
            deterministic
          ) if sqlKind == SqlKind.EQUALS.toString && operatorName == "=" &&
            deterministic =>
        (column(left), literal(right)) match {
          case (Some(columnRef), Some(literalValue)) =>
            Some((columnRef, literalValue, false))
          case _ =>
            (literal(left), column(right)) match {
              case (Some(literalValue), Some(columnRef)) =>
                Some((columnRef, literalValue, true))
              case _ => None
            }
        }
      case _ => None
    }
  }

  private def ensureLocalIntegralPredicateCertificate(
      report: AnalysisReport,
      certificate: Option[LocalIntegralPredicateSourceCertificate],
      localSource: Option[CalciteLocalPredicates.SourceCertificate] = None
  ): AnalysisReport = {
    if (localSource.nonEmpty) {
      val source = localSource.get
      if (CalciteLocalPredicates.concordant(source, report.query))
        return report.copy(query = report.query.copy(
          sourceProof = QuerySourceProof.certifiedLocalPredicates(source, report.query)))
      return report.copy(diagnostics = report.diagnostics :+ RewriteDiagnostic(
        code = RewriteReasonCode.UnsupportedPredicate,
        message = "local predicates require concordant raw-source and validated occurrence bindings",
        severity = DiagnosticSeverity.Warning,
        context = Map("sourceBoundary" -> CalciteLocalPredicates.CapabilityId)))
    }
    val metadataByColumn = report.query.relations.iterator
      .flatMap(_.columns)
      .map(metadata => metadata.ref -> metadata)
      .toMap
    val relationalBindings = report.query.predicates.flatMap { predicate =>
      directLocalIntegralEquality(predicate.expression, metadataByColumn).map {
        case (column, literal, literalOnLeft) =>
          val metadata = metadataByColumn(column.ref)
          LocalIntegralRelationalBinding(
            predicateOrdinal = predicate.ordinal,
            occurrenceOrdinal = column.ref.relationOccurrenceId.value,
            columnName = metadata.sourceName,
            literalValue = literal.value,
            literalOnLeft = literalOnLeft
          )
      }
    }
    val hasTypedLocalShape = report.query.predicates.exists(predicate =>
      directLocalIntegralEqualityShape(predicate.expression).nonEmpty
    )

    val concordant = certificate.exists { source =>
      report.query.resultShape == QueryResultShape.EnumerationProjection &&
      source.totalPredicateCount == report.query.predicates.size &&
      source.occurrences.size == report.query.relations.size &&
      source.occurrences.zip(report.query.relations).forall {
        case (sourceOccurrence, relation) =>
          sourceOccurrence.ordinal == relation.id.value &&
          relation.qualifiedName.endsWith(sourceOccurrence.qualifiedName)
      } &&
      source.predicates.map { binding =>
        LocalIntegralRelationalBinding(
          predicateOrdinal = binding.predicateOrdinal,
          occurrenceOrdinal = binding.occurrenceOrdinal,
          columnName = binding.columnName,
          literalValue = binding.literalValue,
          literalOnLeft = binding.literalOnLeft
        )
      } == relationalBindings &&
      source.predicates.forall(binding =>
        source.occurrences
          .lift(binding.occurrenceOrdinal)
          .exists(_.alias == binding.alias)
      ) &&
      report.query.predicates.forall { predicate =>
        directLocalIntegralEqualityShape(predicate.expression).isEmpty ||
        predicate.classification == PredicateClassification.LocalFilter(
          predicate.expression.columnRefs.head.relationOccurrenceId
        )
      }
    }

    if (!hasTypedLocalShape && certificate.isEmpty) report
    else if (concordant) {
      report.copy(
        query = report.query.copy(
          sourceProof = QuerySourceProof.certifiedLocalIntegralPredicates(
            certificate.get,
            report.query
          )
        )
      )
    } else {
      val diagnostic = RewriteDiagnostic(
        code = RewriteReasonCode.UnsupportedPredicate,
        message =
          "typed local integral predicates require exact concordant raw and relational evidence",
        severity = DiagnosticSeverity.Warning,
        context = Map(
          "sourceCertificate" -> certificate.fold("missing")(_ => "mismatch"),
          "sourcePredicateCount" -> certificate
            .map(_.totalPredicateCount.toString)
            .getOrElse("0"),
          "relationalPredicateCount" -> report.query.predicates.size.toString,
          "sourceLocalPredicateCount" -> certificate
            .map(_.predicates.size.toString)
            .getOrElse("0"),
          "relationalLocalPredicateCount" -> relationalBindings.size.toString
        )
      )
      report.copy(diagnostics = report.diagnostics :+ diagnostic)
    }
  }

  private def ensureAggregateRejected(
      report: AnalysisReport,
      message: String,
      context: Map[String, String]
  ): AnalysisReport = {
    if (report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate)) {
      report
    } else {
      report.copy(
        diagnostics = report.diagnostics :+ RewriteDiagnostic(
          code = RewriteReasonCode.UnsupportedAggregate,
          message = message,
          severity = DiagnosticSeverity.Warning,
          context = context
        )
      )
    }
  }

  private final class Analyzer(allowLocalPredicates: Boolean = false) {
    private val relations = mutable.ArrayBuffer.empty[RelationOccurrence]
    private val predicates = mutable.ArrayBuffer.empty[PredicateDescriptor]
    private val diagnostics = mutable.ArrayBuffer.empty[RewriteDiagnostic]

    def analyze(root: RelNode, outputNames: Vector[String]): AnalysisReport = {
      val relational = visit(root, "root")
      finish(relational, outputNames)
    }

    def analyzeGlobalCountStar(
        aggregate: LogicalAggregate,
        outputNames: Vector[String],
        certificate: GlobalCountStarSourceCertificate
    ): AnalysisReport = {
      require(aggregate != null, "global COUNT(*) aggregate must not be null")
      require(outputNames != null, "global COUNT(*) output names must not be null")
      require(certificate != null, "global COUNT(*) certificate must not be null")

      val input = visit(aggregate.getInput, "root/input")
      val field = aggregate.getRowType.getFieldList.get(0)
      val outputName = certificate.aliasMode match {
        case GlobalCountStarAliasMode.Implicit => "count"
        case GlobalCountStarAliasMode.Explicit => outputNames.head
      }
      val countOutput = OutputSlot(
        ordinal = 0,
        name = outputName,
        expression = ScalarExpression.GlobalCountStar,
        dataType = typeDigest(field.getType),
        nullable = false
      )
      finish(
        GlobalCountStarIR(input, countOutput, certificate),
        Vector(outputName)
      )
    }

    def analyzeGroupedCountStar(
        aggregate: LogicalAggregate,
        outputNames: Vector[String],
        certificate: GroupedCountStarSourceCertificate
    ): AnalysisReport = {
      require(aggregate != null, "grouped COUNT(*) aggregate must not be null")
      require(
        outputNames != null && outputNames.size == 2,
        "grouped COUNT(*) must have two output names"
      )
      require(certificate != null, "grouped COUNT(*) certificate must not be null")

      val input = visit(aggregate.getInput, "root/input")
      val groupOrdinal = aggregate.getGroupSet.asList.get(0).intValue()
      val fields = aggregate.getRowType.getFieldList
      input.output.lift(groupOrdinal).map(_.expression) match {
        case Some(groupColumn: ScalarExpression.Column) =>
          val groupField = fields.get(0)
          val countField = fields.get(1)
          val countName = certificate.countAliasMode match {
            case GlobalCountStarAliasMode.Implicit => "count"
            case GlobalCountStarAliasMode.Explicit => outputNames(1)
          }
          val groupOutput = OutputSlot(
            ordinal = 0,
            name = outputNames.head,
            expression = groupColumn,
            dataType = typeDigest(groupField.getType),
            nullable = groupField.getType.isNullable
          )
          val countOutput = OutputSlot(
            ordinal = 1,
            name = countName,
            expression = ScalarExpression.GlobalCountStar,
            dataType = typeDigest(countField.getType),
            nullable = false
          )
          finish(
            GroupedCountStarIR(
              input,
              groupOutput,
              countOutput,
              certificate
            ),
            Vector(groupOutput.name, countOutput.name)
          )
        case _ =>
          diagnostics += warning(
            code = RewriteReasonCode.UnsupportedAggregate,
            message =
              "grouped COUNT(*) key did not resolve to one direct input column",
            context = Map("groupOrdinal" -> groupOrdinal.toString)
          )
          val unsupportedOutput = fields.asScala.toVector.zipWithIndex.map {
            case (field, ordinal) =>
              OutputSlot(
                ordinal = ordinal,
                name = field.getName,
                expression = ScalarExpression.Unresolved(
                  digest = s"${aggregate.getRelTypeName}$$$ordinal",
                  dataType = typeDigest(field.getType),
                  nullable = field.getType.isNullable,
                  reason = "grouped aggregate key lacks direct column lineage"
                ),
                dataType = typeDigest(field.getType),
                nullable = field.getType.isNullable
              )
          }
          finish(
            UnsupportedRelationalIR(
              operatorName = aggregate.getRelTypeName,
              nodePath = "root",
              diagnosticCode = RewriteReasonCode.UnsupportedAggregate,
              inputs = Vector(input),
              output = unsupportedOutput
            ),
            outputNames
          )
      }
    }

    def analyzeDistributedGroupedCountStar(
        aggregate: LogicalAggregate,
        groupInputOrdinals: Vector[Int],
        outputFields: Vector[RelDataTypeField],
        certificate: DistributedGroupedCountStarSourceCertificate
    ): AnalysisReport = {
      require(certificate != null, "distributed grouped COUNT(*) certificate must not be null")
      val keyCount = certificate.groupKeyCount
      require(
        aggregate != null,
        "distributed grouped COUNT(*) aggregate must not be null"
      )
      require(
        groupInputOrdinals != null && groupInputOrdinals.size == keyCount,
        "distributed grouped COUNT(*) input ordinals must match certified arity"
      )
      require(
        outputFields != null && outputFields.size == keyCount + 1,
        "distributed grouped COUNT(*) output fields must match certified arity"
      )
      val input = visit(aggregate.getInput, "root/input")
      val groupExpressions = groupInputOrdinals.map(ordinal =>
        input.output.lift(ordinal).map(_.expression)
      )
      val groupColumns = groupExpressions.collect { case Some(column: ScalarExpression.Column) => column }
      groupColumns match {
        case columns if columns.size == keyCount &&
            columns.map(_.ref.relationOccurrenceId).distinct.size == keyCount =>
          val groupOutputs = groupColumns.zipWithIndex.map {
            case (groupColumn, ordinal) =>
              val field = outputFields(ordinal)
              OutputSlot(
                ordinal = ordinal,
                name = field.getName,
                expression = groupColumn,
                dataType = typeDigest(field.getType),
                nullable = field.getType.isNullable
              )
          }
          val countField = outputFields(keyCount)
          val countName = certificate.countAliasMode match {
            case GlobalCountStarAliasMode.Implicit => "count"
            case GlobalCountStarAliasMode.Explicit => countField.getName
          }
          val countOutput = OutputSlot(
            ordinal = keyCount,
            name = countName,
            expression = ScalarExpression.GlobalCountStar,
            dataType = typeDigest(countField.getType),
            nullable = false
          )
          finish(
            DistributedGroupedCountStarIR(
              input,
              groupOutputs,
              countOutput,
              certificate
            ),
            groupOutputs.map(_.name) :+ countOutput.name
          )
        case _ =>
          diagnostics += warning(
            code = RewriteReasonCode.UnsupportedAggregate,
            message =
              "distributed grouped COUNT(*) keys must resolve to direct columns from distinct relation occurrences",
            context = Map(
              "groupInputOrdinals" -> groupInputOrdinals.mkString(",")
            )
          )
          val unsupportedOutput = outputFields.zipWithIndex.map {
            case (field, ordinal) =>
              OutputSlot(
                ordinal = ordinal,
                name = field.getName,
                expression = ScalarExpression.Unresolved(
                  digest = s"${aggregate.getRelTypeName}$$$ordinal",
                  dataType = typeDigest(field.getType),
                  nullable = field.getType.isNullable,
                  reason =
                    "distributed grouped aggregate keys lack distinct direct column lineage"
                ),
                dataType = typeDigest(field.getType),
                nullable = field.getType.isNullable
              )
          }
          finish(
            UnsupportedRelationalIR(
              operatorName = aggregate.getRelTypeName,
              nodePath = "root",
              diagnosticCode = RewriteReasonCode.UnsupportedAggregate,
              inputs = Vector(input),
              output = unsupportedOutput
            ),
            outputFields.map(_.getName)
          )
      }
    }

    def analyzeC03G0(
        aggregate: LogicalAggregate,
        outputFields: Vector[RelDataTypeField],
        certificate: C03G0SourceCertificate
    ): AnalysisReport = {
      require(aggregate != null, "C03-G0 aggregate must not be null")
      require(
        outputFields != null && outputFields.size == 5,
        "C03-G0 aggregate must have five validated output fields"
      )
      require(certificate != null, "C03-G0 certificate must not be null")

      val input = visit(aggregate.getInput, "root/input")
      val groupOrdinal = aggregate.getGroupSet.asList.get(0).intValue()
      val measureOrdinal =
        aggregate.getAggCallList.get(2).getArgList.get(0).intValue()
      val groupExpression = input.output.lift(groupOrdinal).map(_.expression)
      val measureExpression = input.output.lift(measureOrdinal).map(_.expression)

      (groupExpression, measureExpression) match {
        case (
              Some(groupColumn: ScalarExpression.Column),
              Some(measureColumn: ScalarExpression.Column)
            ) if exactC03G0CatalogLineage(
              groupColumn,
              measureColumn,
              certificate.catalogBinding
            ) =>
          val groupOutput = OutputSlot(
            ordinal = 0,
            name = outputFields(0).getName,
            expression = groupColumn,
            dataType = typeDigest(outputFields(0).getType),
            nullable = outputFields(0).getType.isNullable
          )
          val countStarOutput = OutputSlot(
            ordinal = 1,
            name = outputFields(1).getName,
            expression = ScalarExpression.GlobalCountStar,
            dataType = typeDigest(outputFields(1).getType),
            nullable = outputFields(1).getType.isNullable
          )
          val countGroup = ScalarExpression.C03G0CountColumn(groupColumn)
          val countGroupOutput = OutputSlot(
            ordinal = 2,
            name = outputFields(2).getName,
            expression = countGroup,
            dataType = typeDigest(outputFields(2).getType),
            nullable = outputFields(2).getType.isNullable
          )
          val minimum = ScalarExpression.C03G0Extremum(
            GlobalGuardedExtremumFunction.Min,
            measureColumn,
            typeDigest(outputFields(3).getType),
            outputFields(3).getType.isNullable
          )
          val minOutput = OutputSlot(
            ordinal = 3,
            name = outputFields(3).getName,
            expression = minimum,
            dataType = minimum.dataType,
            nullable = minimum.nullable
          )
          val maximum = ScalarExpression.C03G0Extremum(
            GlobalGuardedExtremumFunction.Max,
            measureColumn,
            typeDigest(outputFields(4).getType),
            outputFields(4).getType.isNullable
          )
          val maxOutput = OutputSlot(
            ordinal = 4,
            name = outputFields(4).getName,
            expression = maximum,
            dataType = maximum.dataType,
            nullable = maximum.nullable
          )
          val relational = C03G0AggregateVectorIR(
            input,
            groupOutput,
            countStarOutput,
            countGroupOutput,
            minOutput,
            maxOutput,
            certificate
          )
          finish(relational, relational.output.map(_.name))

        case _ =>
          diagnostics += warning(
            code = RewriteReasonCode.UnsupportedAggregate,
            message =
              "C03-G0 G and X must resolve to the exact verified direct guard occurrence",
            context = Map(
              "groupInputOrdinal" -> groupOrdinal.toString,
              "measureInputOrdinal" -> measureOrdinal.toString,
              "sourceCertificate" -> "c03-g0-v2"
            )
          )
          val unsupportedOutput = outputFields.zipWithIndex.map {
            case (field, ordinal) =>
              OutputSlot(
                ordinal = ordinal,
                name = field.getName,
                expression = ScalarExpression.Unresolved(
                  digest = s"${aggregate.getRelTypeName}$$$ordinal",
                  dataType = typeDigest(field.getType),
                  nullable = field.getType.isNullable,
                  reason = "C03-G0 direct catalog lineage mismatch"
                ),
                dataType = typeDigest(field.getType),
                nullable = field.getType.isNullable
              )
          }
          finish(
            UnsupportedRelationalIR(
              operatorName = aggregate.getRelTypeName,
              nodePath = "root",
              diagnosticCode = RewriteReasonCode.UnsupportedAggregate,
              inputs = Vector(input),
              output = unsupportedOutput
            ),
            outputFields.map(_.getName)
          )
      }
    }

    private def exactC03G0CatalogLineage(
        groupColumn: ScalarExpression.Column,
        measureColumn: ScalarExpression.Column,
        binding: C03G0CatalogBinding
    ): Boolean = {
      val occurrenceId = groupColumn.ref.relationOccurrenceId
      val guardRelations = relations.filter(_.id == occurrenceId).toVector
      val groupMetadata = guardRelations.iterator
        .flatMap(_.columns)
        .filter(_.ref == groupColumn.ref)
        .toVector
      val measureMetadata = guardRelations.iterator
        .flatMap(_.columns)
        .filter(_.ref == measureColumn.ref)
        .toVector
      val predicateColumns = predicates.iterator
        .flatMap(_.expression.columnRefs)
        .toSet
      binding != null &&
      groupColumn.ref != measureColumn.ref &&
      groupColumn.ref.ordinal == binding.groupAttnum - 1 &&
      measureColumn.ref.ordinal == binding.measureAttnum - 1 &&
      measureColumn.ref.relationOccurrenceId == occurrenceId &&
      !predicateColumns(groupColumn.ref) &&
      !predicateColumns(measureColumn.ref) &&
      guardRelations.size == 1 &&
      guardRelations.head.qualifiedName.endsWith(
        Vector(binding.physicalSchemaName, binding.relationName)
      ) &&
      groupMetadata.size == 1 &&
      groupMetadata.head.sourceName == binding.groupColumnName &&
      groupMetadata.head.sqlTypeName == "VARCHAR" &&
      groupMetadata.head.nullable &&
      groupColumn.dataType == groupMetadata.head.dataType &&
      groupColumn.nullable == groupMetadata.head.nullable &&
      measureMetadata.size == 1 &&
      measureMetadata.head.sourceName == binding.measureColumnName &&
      measureMetadata.head.sqlTypeName == "BIGINT" &&
      !measureMetadata.head.nullable &&
      measureColumn.dataType == measureMetadata.head.dataType &&
      measureColumn.nullable == measureMetadata.head.nullable
    }

    def analyzeGlobalGuardedExtremum(
        aggregate: LogicalAggregate,
        outputNames: Vector[String],
        certificate: GlobalGuardedExtremumSourceCertificate
    ): AnalysisReport = {
      require(aggregate != null, "global guarded extremum aggregate must not be null")
      require(outputNames != null, "global guarded extremum output names must not be null")
      require(certificate != null, "global guarded extremum certificate must not be null")

      val input = visit(aggregate.getInput, "root/input")
      val call = aggregate.getAggCallList.get(0)
      val argumentOrdinal = call.getArgList.get(0).intValue()
      val field = aggregate.getRowType.getFieldList.get(0)
      input.output.lift(argumentOrdinal).map(_.expression) match {
        case Some(argument: ScalarExpression.Column) =>
          val outputName = certificate.aliasMode match {
            case GlobalGuardedExtremumAliasMode.Implicit =>
              certificate.function.implicitOutputName
            case GlobalGuardedExtremumAliasMode.Explicit => outputNames.head
          }
          val extremum = ScalarExpression.GlobalGuardedExtremum(
            function = certificate.function,
            argument = argument,
            digest = s"${certificate.function.sqlName}(${argument.digest})",
            dataType = typeDigest(field.getType),
            nullable = true
          )
          val extremumOutput = OutputSlot(
            ordinal = 0,
            name = outputName,
            expression = extremum,
            dataType = typeDigest(field.getType),
            nullable = true
          )
          finish(
            GlobalGuardedExtremumIR(input, extremumOutput, certificate),
            Vector(outputName)
          )
        case _ =>
          diagnostics += warning(
            code = RewriteReasonCode.UnsupportedAggregate,
            message =
              "global MIN/MAX argument did not resolve to one direct guard column",
            context = Map(
              "argumentOrdinal" -> argumentOrdinal.toString,
              "function" -> certificate.function.id
            )
          )
          val unsupportedOutput = OutputSlot(
            ordinal = 0,
            name = field.getName,
            expression = ScalarExpression.Unresolved(
              digest = call.toString,
              dataType = typeDigest(field.getType),
              nullable = field.getType.isNullable,
              reason = "aggregate argument lacks direct column lineage"
            ),
            dataType = typeDigest(field.getType),
            nullable = field.getType.isNullable
          )
          finish(
            UnsupportedRelationalIR(
              operatorName = aggregate.getRelTypeName,
              nodePath = "root",
              diagnosticCode = RewriteReasonCode.UnsupportedAggregate,
              inputs = Vector(input),
              output = Vector(unsupportedOutput)
            ),
            outputNames
          )
      }
    }

    private def finish(
        relational: RelationalIR,
        outputNames: Vector[String]
    ): AnalysisReport = {
      val query = QueryIR(
        root = relational,
        relations = relations.toVector,
        predicates = predicates.toVector,
        outputNames = outputNames
      )

      if (diagnostics.isEmpty) {
        QueryHypergraphConverter
          .connectivityDiagnostic(query)
          .foreach(diagnostics += _)
      }

      AnalysisReport(query, diagnostics.toVector)
    }

    private def visit(node: RelNode, path: String): RelationalIR = node match {
      case scan: TableScan if isSupportedBaseScanClass(scan.getClass) =>
        visitScan(scan)
      // Adapter and optimized TableScan subclasses may carry pushed filters
      // or projections. Treating them as plain scans would silently omit
      // semantics. Accept only the exact bare LogicalTableScan and
      // JdbcTableScan classes; every other subclass remains unsupported.
      case scan: TableScan => visitUnsupported(scan, path)
      case project: Project => visitProject(project, path)
      case filter: Filter => visitFilter(filter, path)
      case join: Join => visitJoin(join, path)
      case other => visitUnsupported(other, path)
    }

    private def visitScan(scan: TableScan): TableScanIR = {
      val occurrenceId = relationOccurrenceId(relations.size)
      val columns = scan.getRowType.getFieldList.asScala.toVector.map { field =>
        val columnId = ColumnId
          .from(occurrenceId, field.getIndex)
          .fold(
            error =>
              throw new IllegalStateException(
                s"invalid Calcite scan column identity: ${error.message}"
              ),
            identity
          )
        ColumnMetadata(
          ref = ColumnRef(columnId),
          sourceName = field.getName,
          sqlTypeName = field.getType.getSqlTypeName.getName,
          dataType = typeDigest(field.getType),
          nullable = field.getType.isNullable
        )
      }
      val qualifiedName = scan.getTable.getQualifiedName.asScala.toVector
      val occurrence = RelationOccurrence(occurrenceId, qualifiedName, columns)
      relations += occurrence
      TableScanIR(occurrence)
    }

    private def visitProject(project: Project, path: String): ProjectIR = {
      val input = visit(project.getInput, s"$path/input")
      val fields = project.getRowType.getFieldList.asScala.toVector
      val projects = project.getProjects.asScala.toVector.zipWithIndex.map {
        case (expression, ordinal) =>
          val translated = translate(expression, input.output)
          if (!translated.resolved) {
            diagnostics += warning(
              code = RewriteReasonCode.UnresolvedLineage,
              message = "project expression lineage could not be resolved",
              context = expressionContext(path, expression) ++ Map(
                "outputOrdinal" -> ordinal.toString
              )
            )
          } else if (!translated.deterministic) {
            diagnostics += warning(
              code = RewriteReasonCode.UnsupportedQueryShape,
              message = "non-deterministic project expressions are not supported",
              context = expressionContext(path, expression) ++ Map(
                "outputOrdinal" -> ordinal.toString
              )
            )
          } else if (!translated.losslesslyRetained) {
            diagnostics += warning(
              code = RewriteReasonCode.UnsupportedQueryShape,
              message =
                "project expression is outside the losslessly retained scalar grammar",
              context = expressionContext(path, expression) ++ Map(
                "outputOrdinal" -> ordinal.toString
              )
            )
          }
          val field = fields(ordinal)
          ProjectItem(
            ordinal = ordinal,
            name = field.getName,
            expression = translated,
            dataType = typeDigest(field.getType),
            nullable = field.getType.isNullable
          )
      }
      ProjectIR(input, projects)
    }

    private def visitFilter(filter: Filter, path: String): FilterIR = {
      val input = visit(filter.getInput, s"$path/input")
      val analyzed = analyzePredicates(
        filter.getCondition,
        PredicateOrigin.Filter(path),
        input.output,
        filter.getCluster.getRexBuilder
      )
      FilterIR(input, analyzed)
    }

    private def visitJoin(join: Join, path: String): JoinIR = {
      val left = visit(join.getLeft, s"$path/left")
      val right = visit(join.getRight, s"$path/right")
      val joinType = join.getJoinType.toString.toLowerCase

      if (join.getJoinType != org.apache.calcite.rel.core.JoinRelType.INNER) {
        diagnostics += warning(
          code = RewriteReasonCode.UnsupportedJoinType,
          message = s"join type '$joinType' is not supported",
          context = Map(
            "nodePath" -> path,
            "joinType" -> joinType
          )
        )
      }

      val inputOutput = left.output ++ right.output
      val analyzed = analyzePredicates(
        join.getCondition,
        PredicateOrigin.Join(path, joinType),
        inputOutput,
        join.getCluster.getRexBuilder
      )
      val output = relabelOutput(
        inputOutput.map(_.expression),
        join.getRowType,
        path
      )
      JoinIR(left, right, joinType, analyzed, output)
    }

    private def visitUnsupported(
        node: RelNode,
        path: String
    ): UnsupportedRelationalIR = {
      val inputs = node.getInputs.asScala.toVector.zipWithIndex.map {
        case (input, ordinal) => visit(input, s"$path/input$ordinal")
      }
      val isAggregate = node.getRelTypeName == "LogicalAggregate" ||
        node.isInstanceOf[org.apache.calcite.rel.core.Aggregate]
      val diagnosticCode =
        if (isAggregate) RewriteReasonCode.UnsupportedAggregate
        else RewriteReasonCode.UnsupportedQueryShape
      diagnostics += warning(
        code = diagnosticCode,
        message = s"relational operator '${node.getRelTypeName}' is not supported",
        context = Map(
          "nodePath" -> path,
          "operator" -> node.getRelTypeName
        )
      )

      val output = node.getRowType.getFieldList.asScala.toVector.map { field =>
        OutputSlot(
          ordinal = field.getIndex,
          name = field.getName,
          expression = ScalarExpression.Unresolved(
            digest = s"${node.getRelTypeName}$$${field.getIndex}",
            dataType = typeDigest(field.getType),
            nullable = field.getType.isNullable,
            reason = s"output of unsupported operator ${node.getRelTypeName}"
          ),
          dataType = typeDigest(field.getType),
          nullable = field.getType.isNullable
        )
      }
      UnsupportedRelationalIR(
        operatorName = node.getRelTypeName,
        nodePath = path,
        diagnosticCode = diagnosticCode,
        inputs = inputs,
        output = output
      )
    }

    private def analyzePredicates(
        condition: RexNode,
        origin: PredicateOrigin,
        input: Vector[OutputSlot],
        rexBuilder: RexBuilder
    ): Vector[PredicateDescriptor] = {
      val conjuncts = RelOptUtil.conjunctions(condition).asScala.toVector
      conjuncts.map { conjunct =>
        val original = translate(conjunct, input)
        val expression = if (!allowLocalPredicates) original else {
          val metadata = relations.iterator.flatMap(_.columns).map(m => m.ref -> m).toMap
          CalciteLocalPredicates.fromRex(conjunct, input, rexBuilder)
            .filter(CalciteLocalPredicates.validLocal(_, metadata))
            .map(tree => ScalarExpression.LocalPredicate(tree, conjunct.toString,
              typeDigest(conjunct.getType), conjunct.getType.isNullable))
            .getOrElse(original)
        }
        val classification = classify(expression)
        val descriptor = PredicateDescriptor(
          ordinal = predicates.size,
          origin = origin,
          expression = expression,
          classification = classification
        )
        predicates += descriptor

        classification match {
          case unsupported: PredicateClassification.Unsupported =>
            diagnostics += warning(
              code = unsupported.code,
              message = unsupported.reason,
              context = expressionContext(origin.nodePath, conjunct) ++ Map(
                "predicateOrdinal" -> descriptor.ordinal.toString,
                "predicateOrigin" -> origin.id
              )
            )
          case _ => ()
        }
        descriptor
      }
    }

    private def classify(
        expression: ScalarExpression
    ): PredicateClassification = {
      if (!expression.resolved) {
        PredicateClassification.Unsupported(
          RewriteReasonCode.UnresolvedLineage,
          "predicate input lineage could not be resolved"
        )
      } else if (!expression.deterministic) {
        PredicateClassification.Unsupported(
          RewriteReasonCode.UnsupportedPredicate,
          "non-deterministic predicates are not supported"
        )
      } else {
        directColumnEquality(expression) match {
          case Some((left, right))
              if left.relationOccurrenceId != right.relationOccurrenceId =>
            val ordered = Vector(left, right).sorted
            PredicateClassification.InnerEquality(ordered(0), ordered(1))
          case _ =>
            directLocalIntegralEqualityShape(expression) match {
              case Some((column, _, _)) =>
                PredicateClassification.LocalFilter(
                  column.relationOccurrenceId
                )
              case _ if !expression.predicateLosslesslyRetained =>
                PredicateClassification.Unsupported(
                  RewriteReasonCode.UnsupportedPredicate,
                  "predicate is outside the losslessly retained scalar grammar"
                )
              case _ =>
                val occurrences = expression.columnRefs
                  .map(_.relationOccurrenceId)
                  .distinct
                  .sorted
                occurrences match {
                  case Vector() => PredicateClassification.ConstantFilter
                  case Vector(occurrenceId) =>
                    PredicateClassification.LocalFilter(occurrenceId)
                  case _ =>
                    PredicateClassification.Unsupported(
                      RewriteReasonCode.UnsupportedPredicate,
                      "cross-relation predicate is not a direct column equality"
                    )
                }
            }
        }
      }
    }

    private def directColumnEquality(
        expression: ScalarExpression
    ): Option[(ColumnRef, ColumnRef)] = expression match {
      case ScalarExpression.Call(
            sqlKind,
            _,
            Vector(
              ScalarExpression.Column(left, _, _, _),
              ScalarExpression.Column(right, _, _, _)
            ),
            _,
            _,
            _,
            _
          ) if sqlKind == SqlKind.EQUALS.toString => Some(left -> right)
      case _ => None
    }

    private def translate(
        expression: RexNode,
        input: Vector[OutputSlot]
    ): ScalarExpression = expression match {
      case ref: RexInputRef =>
        input.lift(ref.getIndex) match {
          case Some(slot) => slot.expression
          case None =>
            ScalarExpression.Unresolved(
              digest = ref.toString,
              dataType = typeDigest(ref.getType),
              nullable = ref.getType.isNullable,
              reason = s"input index ${ref.getIndex} is outside ${input.size} fields"
            )
        }
      case literal: RexLiteral =>
        exactIntegralLiteral(literal).getOrElse(
          ScalarExpression.Literal(
            digest = literal.toString,
            dataType = typeDigest(literal.getType),
            nullable = literal.getType.isNullable
          )
        )
      case call: RexCall =>
        val operands = call.getOperands.asScala.toVector.map(translate(_, input))
        exactIntegralWideningCast(call, operands).getOrElse(
          ScalarExpression.Call(
            sqlKind = call.getKind.toString,
            operatorName = call.getOperator.getName,
            operands = operands,
            digest = call.toString,
            dataType = typeDigest(call.getType),
            nullable = call.getType.isNullable,
            deterministic = RexUtil.isDeterministic(call)
          )
        )
      case other =>
        ScalarExpression.Unresolved(
          digest = other.toString,
          dataType = typeDigest(other.getType),
          nullable = other.getType.isNullable,
          reason = s"unsupported RexNode ${other.getClass.getSimpleName}"
        )
    }

    private def exactIntegralLiteral(
        literal: RexLiteral
    ): Option[ScalarExpression.ExactIntegralLiteral] = {
      val sqlTypeName = Option(literal.getType)
        .flatMap(dataType => Option(dataType.getSqlTypeName))
        .map(_.getName)
      for {
        name <- sqlTypeName
        if integralTypeRanks.contains(name)
        if !literal.isNull && !literal.getType.isNullable
        decimal <- Option(
          literal.getValueAs(classOf[java.math.BigDecimal])
        )
        value <- scala.util.Try(
          decimal.toBigIntegerExact.longValueExact
        ).toOption
        if exactIntegralRange(name, value)
      } yield ScalarExpression.ExactIntegralLiteral(
        value = value,
        sqlTypeName = name,
        digest = literal.toString,
        dataType = typeDigest(literal.getType)
      )
    }

    private def exactIntegralWideningCast(
        call: RexCall,
        operands: Vector[ScalarExpression]
    ): Option[ScalarExpression.ExactIntegralWideningCast] = {
      val sourceType = call.getOperands.asScala.toVector.headOption
        .flatMap(node => Option(node.getType))
        .flatMap(dataType => Option(dataType.getSqlTypeName))
        .map(_.getName)
      val targetType = Option(call.getType)
        .flatMap(dataType => Option(dataType.getSqlTypeName))
        .map(_.getName)
      for {
        operand <- operands match {
          case Vector(value: ScalarExpression.Column) => Some(value)
          case Vector(value: ScalarExpression.ExactIntegralLiteral) =>
            Some(value)
          case _ => None
        }
        source <- sourceType
        target <- targetType
        if call.getKind == SqlKind.CAST
        if call.getOperator.getName == "CAST"
        if RexUtil.isDeterministic(call)
        if exactIntegralWidening(source, target)
        if operand.nullable == call.getType.isNullable
      } yield ScalarExpression.ExactIntegralWideningCast(
        operand = operand,
        sourceSqlTypeName = source,
        targetSqlTypeName = target,
        digest = call.toString,
        dataType = typeDigest(call.getType),
        nullable = call.getType.isNullable
      )
    }

    private def relabelOutput(
        expressions: Vector[ScalarExpression],
        rowType: RelDataType,
        path: String
    ): Vector[OutputSlot] = {
      val fields = rowType.getFieldList.asScala.toVector
      if (expressions.size != fields.size) {
        diagnostics += warning(
          code = RewriteReasonCode.UnresolvedLineage,
          message = "relational output arity does not match its input lineage",
          context = Map(
            "nodePath" -> path,
            "expressions" -> expressions.size.toString,
            "fields" -> fields.size.toString
          )
        )
      }
      fields.map { field =>
        val expression = expressions.lift(field.getIndex).getOrElse(
          unresolvedOutput(field, "missing input expression")
        )
        OutputSlot(
          ordinal = field.getIndex,
          name = field.getName,
          expression = expression,
          dataType = typeDigest(field.getType),
          nullable = field.getType.isNullable
        )
      }
    }

    private def unresolvedOutput(
        field: RelDataTypeField,
        reason: String
    ): ScalarExpression.Unresolved =
      ScalarExpression.Unresolved(
        digest = s"$$${field.getIndex}",
        dataType = typeDigest(field.getType),
        nullable = field.getType.isNullable,
        reason = reason
      )

    private def relationOccurrenceId(value: Int): RelationOccurrenceId =
      RelationOccurrenceId
        .from(value)
        .fold(
          error =>
            throw new IllegalStateException(
              s"invalid relation traversal identity: ${error.message}"
            ),
          identity
        )

    private def expressionContext(
        path: String,
        expression: RexNode
    ): Map[String, String] = Map(
      "nodePath" -> path,
      "expression" -> expression.toString,
      "sqlKind" -> expression.getKind.toString
    )

    private def warning(
        code: RewriteReasonCode,
        message: String,
        context: Map[String, String]
    ): RewriteDiagnostic =
      RewriteDiagnostic(
        code = code,
        message = message,
        severity = DiagnosticSeverity.Warning,
        context = context
      )

    private def typeDigest(dataType: RelDataType): String =
      dataType.getFullTypeString
  }
}
