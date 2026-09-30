package at.ac.tuwien.dbai.yannakakis.calcite

import org.apache.calcite.rel.{RelNode, RelRoot, RelVisitor}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.{AbstractSchema, AbstractTable}
import org.apache.calcite.sql.SqlNode
import org.apache.calcite.sql.parser.SqlParser
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.{Frameworks, Planner}

import scala.jdk.CollectionConverters._

final case class TestColumn(
    name: String,
    sqlType: SqlTypeName = SqlTypeName.INTEGER,
    nullable: Boolean = false
)

final case class TestTable(
    name: String,
    columns: Vector[TestColumn],
    schemaName: Option[String] = None
)

/** Parse, validate, and relational-conversion artifacts for one SQL string. */
final case class PlannedQuery(
    sql: String,
    parsed: SqlNode,
    validated: SqlNode,
    root: RelRoot
) {
  val rel: RelNode = root.rel
  val renderedPlan: String = org.apache.calcite.plan.RelOptUtil.toString(rel)

  val nodes: Vector[RelNode] = {
    val result = Vector.newBuilder[RelNode]
    new RelVisitor {
      override def visit(node: RelNode, ordinal: Int, parent: RelNode): Unit = {
        result += node
        super.visit(node, ordinal, parent)
      }
    }.go(rel)
    result.result()
  }

  val operatorNames: Vector[String] = nodes.map(_.getRelTypeName)

  def operatorCount(operatorName: String): Int =
    operatorNames.count(_ == operatorName)

  def fieldNames: Vector[String] = rel.getRowType.getFieldNames.asScala.toVector

  def validatedFieldNames: Vector[String] =
    root.validatedRowType.getFieldNames.asScala.toVector
}

/**
  * Reusable, data-free Calcite frontend. A fresh Planner is created per query
  * because Calcite planners have a single-query lifecycle.
  */
final class CalcitePlannerFixture private (val tables: Vector[TestTable]) {
  private val rootSchema = Frameworks.createRootSchema(true)

  private val schemas = tables.iterator
    .flatMap(_.schemaName)
    .toVector
    .distinct
    .map(name => name -> rootSchema.add(name, new AbstractSchema))
    .toMap

  tables.foreach { tableDefinition =>
    val owningSchema = tableDefinition.schemaName
      .flatMap(schemas.get)
      .getOrElse(rootSchema)
    owningSchema.add(
      tableDefinition.name,
      new AbstractTable {
        override def getRowType(typeFactory: RelDataTypeFactory): RelDataType = {
          val builder = typeFactory.builder()
          tableDefinition.columns.foreach { column =>
            val sqlType = typeFactory.createSqlType(column.sqlType)
            val fieldType = typeFactory.createTypeWithNullability(
              sqlType,
              column.nullable
            )
            builder.add(column.name, fieldType)
          }
          builder.build()
        }
      }
    )
  }

  private val frameworkConfig = Frameworks
    .newConfigBuilder()
    .defaultSchema(rootSchema)
    .parserConfig(SqlParser.config().withCaseSensitive(false))
    .build()

  def parse(sql: String): SqlNode =
    withPlanner(_.parse(sql))

  def plan(sql: String): PlannedQuery =
    withPlanner { planner =>
      val parsed = planner.parse(sql)
      val validated = planner.validate(parsed)
      PlannedQuery(sql, parsed, validated, planner.rel(validated))
    }

  private def withPlanner[A](operation: Planner => A): A = {
    val planner: Planner = Frameworks.getPlanner(frameworkConfig)
    try operation(planner)
    finally planner.close()
  }
}

object CalcitePlannerFixture {
  def apply(tables: Iterable[TestTable]): CalcitePlannerFixture =
    new CalcitePlannerFixture(tables.toVector)
}

object CalciteTestSchemas {
  val chain: Vector[TestTable] = Vector(
    TestTable(
      "CHAIN_A",
      Vector(TestColumn("ID"), TestColumn("B_ID"))
    ),
    TestTable(
      "CHAIN_B",
      Vector(TestColumn("ID"), TestColumn("C_ID"))
    ),
    TestTable(
      "CHAIN_C",
      Vector(TestColumn("ID"), TestColumn("PAYLOAD"))
    )
  )

  val triangle: Vector[TestTable] = Vector(
    TestTable("TRIANGLE_R", Vector(TestColumn("A"), TestColumn("B"))),
    TestTable("TRIANGLE_S", Vector(TestColumn("B"), TestColumn("C"))),
    TestTable("TRIANGLE_T", Vector(TestColumn("C"), TestColumn("A")))
  )

  val selfJoin: Vector[TestTable] = Vector(
    TestTable(
      "EMPLOYEE",
      Vector(TestColumn("ID"), TestColumn("MANAGER_ID"))
    )
  )

  val duplicateColumnNames: Vector[TestTable] = Vector(
    TestTable(
      "DUPLICATE_NAMES",
      Vector(TestColumn("DUPLICATE_VALUE"), TestColumn("DUPLICATE_VALUE"))
    )
  )

  val aggregates: Vector[TestTable] = Vector(
    TestTable(
      "FACT_SALE",
      Vector(
        TestColumn("DEPARTMENT_ID"),
        TestColumn("EMPLOYEE_ID"),
        TestColumn("AMOUNT", SqlTypeName.BIGINT),
        TestColumn("ACTIVE", SqlTypeName.BOOLEAN)
      )
    )
  )

  def standard: CalcitePlannerFixture =
    CalcitePlannerFixture(chain ++ triangle ++ selfJoin ++ aggregates)
}
