package at.ac.tuwien.dbai.yannakakis.integration

import at.ac.tuwien.dbai.yannakakis.frontend.calcite.{
  AnalysisReport,
  CalciteQueryAnalyzer
}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.{AbstractSchema, AbstractTable}
import org.apache.calcite.sql.parser.SqlParser
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

/** Data-free Calcite schema matching [[TriangleMultisetFixture]]. */
private[integration] object GeneratedRendererCalciteFixture {
  private final case class Column(
      name: String,
      sqlType: SqlTypeName,
      nullable: Boolean
  )

  private val tables = Vector(
    "triangle_r" -> Vector(
      Column("row_id", SqlTypeName.INTEGER, nullable = false),
      Column("x", SqlTypeName.INTEGER, nullable = true),
      Column("y", SqlTypeName.INTEGER, nullable = true),
      Column("a", SqlTypeName.INTEGER, nullable = true),
      Column("payload", SqlTypeName.VARCHAR, nullable = true)
    ),
    "triangle_s" -> Vector(
      Column("row_id", SqlTypeName.INTEGER, nullable = false),
      Column("y", SqlTypeName.INTEGER, nullable = true),
      Column("z", SqlTypeName.INTEGER, nullable = true),
      Column("payload", SqlTypeName.VARCHAR, nullable = true)
    ),
    "triangle_t" -> Vector(
      Column("row_id", SqlTypeName.INTEGER, nullable = false),
      Column("z", SqlTypeName.INTEGER, nullable = true),
      Column("x", SqlTypeName.INTEGER, nullable = true),
      Column("payload", SqlTypeName.VARCHAR, nullable = true)
    ),
    "renderer_chain_r" -> Vector(
      Column("row_id", SqlTypeName.INTEGER, nullable = false),
      Column("a", SqlTypeName.INTEGER, nullable = true),
      Column("b", SqlTypeName.INTEGER, nullable = true),
      Column("payload", SqlTypeName.VARCHAR, nullable = true)
    ),
    "renderer_chain_s" -> Vector(
      Column("row_id", SqlTypeName.INTEGER, nullable = false),
      Column("b", SqlTypeName.INTEGER, nullable = true),
      Column("c", SqlTypeName.INTEGER, nullable = true),
      Column("payload", SqlTypeName.VARCHAR, nullable = true)
    ),
    "renderer_chain_t" -> Vector(
      Column("row_id", SqlTypeName.INTEGER, nullable = false),
      Column("c", SqlTypeName.INTEGER, nullable = true),
      Column("d", SqlTypeName.INTEGER, nullable = true),
      Column("payload", SqlTypeName.VARCHAR, nullable = true)
    ),
    "renderer_chain_u" -> Vector(
      Column("row_id", SqlTypeName.INTEGER, nullable = false),
      Column("d", SqlTypeName.INTEGER, nullable = true),
      Column("e", SqlTypeName.INTEGER, nullable = true),
      Column("payload", SqlTypeName.VARCHAR, nullable = true)
    )
  )

  def analyze(sql: String): AnalysisReport = {
    val rootSchema = Frameworks.createRootSchema(true)
    val temporarySchema = rootSchema.add("pg_temp", new AbstractSchema)
    tables.foreach { case (tableName, columns) =>
      temporarySchema.add(
        tableName,
        new AbstractTable {
          override def getRowType(
              typeFactory: RelDataTypeFactory
          ): RelDataType = {
            val builder = typeFactory.builder()
            columns.foreach { column =>
              val baseType = typeFactory.createSqlType(column.sqlType)
              builder.add(
                column.name,
                typeFactory.createTypeWithNullability(baseType, column.nullable)
              )
            }
            builder.build()
          }
        }
      )
    }

    val config = Frameworks
      .newConfigBuilder()
      .defaultSchema(rootSchema)
      .parserConfig(SqlParser.config().withCaseSensitive(true))
      .build()
    val planner = Frameworks.getPlanner(config)
    try {
      val parsed = planner.parse(sql)
      val validated = planner.validate(parsed)
      CalciteQueryAnalyzer.analyze(planner.rel(validated))
    } finally planner.close()
  }
}
