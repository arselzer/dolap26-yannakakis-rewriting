package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import at.ac.tuwien.dbai.yannakakis.api.RewriteReasonCode
import org.apache.calcite.rel.logical.{LogicalAggregate, LogicalProject}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.SchemaPlus
import org.apache.calcite.schema.impl.{AbstractSchema, AbstractTable}
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

import scala.jdk.CollectionConverters._

class CalciteC03G0FrontendSuite extends munit.FunSuite {
  private val catalogBinding = C03G0CatalogBinding(
    databaseName = "c03_frontend_test",
    databaseOid = 100L,
    physicalSchemaName = "public",
    relationName = "synthetic_triangle_e0",
    groupColumnName = "payload",
    measureColumnName = "row_id",
    serverMajorVersion = 18,
    namespaceOid = 101L,
    relationOid = 102L,
    groupAttnum = 4,
    groupTypeOid = 25L,
    measureAttnum = 1,
    measureTypeOid = 20L,
    groupCollationOid = 103L,
    groupCollationName = "C",
    groupCollationProvider = "c",
    groupCollationDeterministic = true
  )

  private val schema = c03Schema()
  private val legacyPlanner = CalciteSqlPlanner
    .forSchema(schema, CalciteSqlPlanner.PostgreSqlParserConfig)
    .fold(problem => fail(problem.toString), identity)
  private val c03Planner = profiledPlanner(schema)
  private val c03Sql = Files.readString(c03SourcePath, StandardCharsets.UTF_8)

  test("exact frozen C03 bytes receive the opt-in source and relational proof") {
    val planned = plan(c03Planner, c03Sql)
    val certificate = planned.sourceEvidence match {
      case CalciteSqlSourceEvidence.CertifiedC03G0(value) => value
      case other => fail(s"expected C03-G0 source evidence, found $other")
    }
    assertEquals(
      certificate.sourceSqlSha256,
      C03G0SourceCertificate.SourceSqlSha256
    )
    assertEquals(certificate.catalogBinding, catalogBinding)
    assertEquals(
      planned.aggregateLanguageProfile,
      CalciteAggregateLanguageProfile.C03G0V2(catalogBinding)
    )

    val report = CalciteQueryAnalyzer.analyze(planned)
    assert(
      report.isSupported,
      clues(report.diagnostics, c03PlanDetails(planned))
    )
    val root = report.query.root match {
      case value: C03G0AggregateVectorIR => value
      case other => fail(s"expected C03G0AggregateVectorIR, found $other")
    }
    assertEquals(root.output.map(_.name), C03G0SourceCertificate.OutputAliases)
    assertEquals(root.output.map(_.ordinal), Vector(0, 1, 2, 3, 4))
    assertEquals(root.groupColumn.ref.relationOccurrenceId.value, 0)
    assertEquals(root.groupColumn.ref.ordinal, catalogBinding.groupAttnum - 1)
    assert(root.groupColumn.nullable)
    assertEquals(root.measureColumn.ref.relationOccurrenceId.value, 0)
    assertEquals(root.measureColumn.ref.ordinal, catalogBinding.measureAttnum - 1)
    assert(!root.measureColumn.nullable)
    assertEquals(root.countStarOutput.dataType, "BIGINT NOT NULL")
    assertEquals(root.countGroup.argument.ref, root.groupColumn.ref)
    assertEquals(root.minimum.argument.ref, root.measureColumn.ref)
    assertEquals(root.maximum.argument.ref, root.measureColumn.ref)
    assert(!root.minOutput.nullable)
    assert(!root.maxOutput.nullable)
    assertEquals(
      report.query.resultShape,
      QueryResultShape.C03G0AggregateVector(
        root.groupColumn.ref,
        root.measureColumn.ref,
        root.output,
        catalogBinding
      )
    )
    assert(
      report.query.predicates.forall(predicate =>
        !predicate.expression.columnRefs.contains(root.groupColumn.ref) &&
          !predicate.expression.columnRefs.contains(root.measureColumn.ref)
      )
    )

    val enumeration = analyze(
      legacyPlanner,
      "SELECT e0.payload " + triangleFrom
    )
    assert(enumeration.isSupported, clues(enumeration.diagnostics))
    assertEquals(report.hypergraph, enumeration.hypergraph)
  }

  test("legacy planning and a legacy compatibility envelope reject exact C03") {
    val legacy = plan(legacyPlanner, c03Sql)
    assert(
      !legacy.sourceEvidence.isInstanceOf[
        CalciteSqlSourceEvidence.CertifiedC03G0
      ]
    )
    val legacyReport = CalciteQueryAnalyzer.analyze(legacy)
    assertUnsupportedAggregate(legacyReport)

    val profiled = plan(c03Planner, c03Sql)
    val forcedLegacy = CalciteSqlPlannedQuery(
      profiled.root,
      profiled.sourceEvidence
    )
    assertUnsupportedAggregate(CalciteQueryAnalyzer.analyze(forcedLegacy))
  }

  test("byte, predicate, and self-join source mutations never certify") {
    val mutations = Vector(
      c03Sql + " ",
      c03Sql.replace("GROUP BY e0.payload", "GROUP  BY e0.payload"),
      c03Sql.replace(
        " AND e2.right_key = e0.left_key",
        " AND TRUE\n AND e2.right_key = e0.left_key"
      ),
      c03Sql
        .replace("synthetic_triangle_e1 AS e1", "synthetic_triangle_e0 AS e1")
        .replace("synthetic_triangle_e2 AS e2", "synthetic_triangle_e0 AS e2")
    )
    mutations.foreach { sql =>
      val result = plan(c03Planner, sql)
      assert(
        !result.sourceEvidence.isInstanceOf[
          CalciteSqlSourceEvidence.CertifiedC03G0
        ],
        clues(result.sourceEvidence)
      )
      assertUnsupportedAggregate(CalciteQueryAnalyzer.analyze(result))
    }
  }

  test("relational replay independently rejects G or X as structural columns") {
    val genuine = plan(c03Planner, c03Sql)
    Vector(
      "e0.right_key = e1.left_key" -> "e0.payload = e1.payload",
      "e0.right_key = e1.left_key" -> "e0.row_id = e1.row_id"
    ).foreach { case (original, replacement) =>
      val hostileRoot = plan(c03Planner, c03Sql.replace(original, replacement)).root
      val forged = CalciteSqlPlannedQuery(
        hostileRoot,
        genuine.sourceEvidence,
        genuine.aggregateLanguageProfile
      )
      assertUnsupportedAggregate(CalciteQueryAnalyzer.analyze(forged))
    }
  }

  test("typed wrapper rejects predicate carriage of G or X") {
    val report = analyze(c03Planner, c03Sql)
    assert(report.isSupported, clues(report.diagnostics))
    val root = report.query.root.asInstanceOf[C03G0AggregateVectorIR]
    val original = report.query.predicates.head
    val originalCall = original.expression.asInstanceOf[ScalarExpression.Call]
    val remoteColumn = originalCall.operands.collectFirst {
      case column: ScalarExpression.Column
          if column.ref.relationOccurrenceId != root.guardOccurrenceId => column
    }.getOrElse(fail("expected a remote join column"))

    Vector(root.groupColumn, root.measureColumn).foreach { localColumn =>
      val expression = originalCall.copy(
        operands = Vector(localColumn, remoteColumn),
        digest = s"${localColumn.digest} = ${remoteColumn.digest}"
      )
      val predicate = original.copy(
        ordinal = report.query.predicates.size,
        expression = expression,
        classification = PredicateClassification.InnerEquality(
          localColumn.ref,
          remoteColumn.ref
        )
      )
      intercept[IllegalArgumentException] {
        root.copy(input = FilterIR(root.input, Vector(predicate)))
      }
    }
  }

  test("attribute-number and catalog-profile bindings fail closed") {
    intercept[IllegalArgumentException] {
      catalogBinding.copy(groupAttnum = 3)
    }
    intercept[IllegalArgumentException] {
      catalogBinding.copy(measureAttnum = 2)
    }
    intercept[IllegalArgumentException] {
      catalogBinding.copy(groupCollationDeterministic = false)
    }

    val reorderedSchema = c03Schema(payloadBeforeRightKey = true)
    val reordered = analyze(profiledPlanner(reorderedSchema), c03Sql)
    assertUnsupportedAggregate(reordered)

    val genuine = plan(c03Planner, c03Sql)
    val otherBinding = catalogBinding.copy(databaseOid = 200L)
    val mismatchedProfile = genuine.copy(
      aggregateLanguageProfile = CalciteAggregateLanguageProfile.C03G0V2(
        otherBinding
      )
    )
    assertUnsupportedAggregate(CalciteQueryAnalyzer.analyze(mismatchedProfile))
  }

  private def c03Schema(payloadBeforeRightKey: Boolean = false): SchemaPlus = {
    val root = Frameworks.createRootSchema(true)
    val publicSchema = root.add("public", new AbstractSchema {})
    Vector(
      "synthetic_triangle_e0",
      "synthetic_triangle_e1",
      "synthetic_triangle_e2"
    ).foreach { tableName =>
      publicSchema.add(
        tableName,
        new AbstractTable {
          override def getRowType(factory: RelDataTypeFactory): RelDataType = {
            val ordinary = Vector(
              ("row_id", SqlTypeName.BIGINT, false),
              ("left_key", SqlTypeName.BIGINT, true),
              ("right_key", SqlTypeName.BIGINT, true),
              ("payload", SqlTypeName.VARCHAR, true)
            )
            val columns =
              if (payloadBeforeRightKey)
                Vector(ordinary(0), ordinary(1), ordinary(3), ordinary(2))
              else ordinary
            val builder = factory.builder()
            columns.foreach { case (name, sqlType, nullable) =>
              if (sqlType == SqlTypeName.VARCHAR)
                builder.add(name, sqlType, 128).nullable(nullable)
              else builder.add(name, sqlType).nullable(nullable)
            }
            builder.build()
          }
        }
      )
    }
    publicSchema
  }

  private def profiledPlanner(schema: SchemaPlus): CalciteSqlPlanner =
    CalciteSqlPlanner
      .forSchemaWithC03G0V2(
        schema,
        catalogBinding,
        CalciteSqlPlanner.PostgreSqlParserConfig
      )
      .fold(problem => fail(problem.toString), identity)

  private def plan(
      planner: CalciteSqlPlanner,
      sql: String
  ): CalciteSqlPlannedQuery =
    CalciteSqlPlanner
      .planWithSourceEvidence(planner, sql)
      .fold(problem => fail(problem.toString), identity)

  private def analyze(
      planner: CalciteSqlPlanner,
      sql: String
  ): AnalysisReport = CalciteQueryAnalyzer.analyze(plan(planner, sql))

  private def assertUnsupportedAggregate(report: AnalysisReport): Unit = {
    assert(!report.isSupported, clues(report.diagnostics))
    assert(
      report.diagnostics.exists(_.code == RewriteReasonCode.UnsupportedAggregate),
      clues(report.diagnostics)
    )
  }

  private def c03PlanDetails(planned: CalciteSqlPlannedQuery): String = {
    val candidate = planned.root.project() match {
      case aggregate: LogicalAggregate => Some(aggregate)
      case project: LogicalProject => project.getInput match {
          case aggregate: LogicalAggregate => Some(aggregate)
          case _                           => None
        }
      case _ => None
    }
    candidate.fold(planned.root.project().toString) { aggregate =>
      val fields = aggregate.getRowType.getFieldList.asScala.toVector
        .map(field => s"${field.getName}:${field.getType.getFullTypeString}")
      val calls = aggregate.getAggCallList.asScala.toVector.map { call =>
        s"${call.getAggregation.getClass.getName}:${call.getAggregation.getName}:" +
          s"args=${call.getArgList}:type=${call.getType.getFullTypeString}:" +
          s"distinct=${call.isDistinct}:filter=${call.filterArg}"
      }
      s"fields=${fields.mkString("|")};calls=${calls.mkString("|")};" +
        s"validated=${planned.root.validatedRowType.getFullTypeString}"
    }
  }

  private val triangleFrom: String =
    "FROM synthetic_triangle_e0 AS e0 " +
      "JOIN synthetic_triangle_e1 AS e1 ON e0.right_key = e1.left_key " +
      "JOIN synthetic_triangle_e2 AS e2 ON e1.right_key = e2.left_key " +
      "AND e2.right_key = e0.left_key"

  private def c03SourcePath: Path = {
    val relative = Paths.get(
      "benchmarks",
      "cyclic-postgresql",
      "starter-corpus",
      "queries",
      "c03-triangle-guarded-multiagg.sql"
    )
    Vector(relative, Paths.get("..").resolve(relative))
      .map(_.toAbsolutePath.normalize())
      .find(Files.isRegularFile(_))
      .getOrElse(fail("could not locate frozen C03 source SQL"))
  }
}
