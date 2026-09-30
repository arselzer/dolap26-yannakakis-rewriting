package at.ac.tuwien.dbai.yannakakis.integration

import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.SchemaPlus
import org.apache.calcite.schema.impl.{AbstractSchema, AbstractTable}
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

/** Data-free Calcite catalog matching [[BenchmarkDerivedMultisetFixture]]. */
private[integration] object BenchmarkDerivedCalciteFixture {
  private final case class Column(
      name: String,
      sqlType: SqlTypeName,
      nullable: Boolean
  )

  private val rowId = Column("row_id", SqlTypeName.BIGINT, nullable = false)
  private val payload = Column("payload", SqlTypeName.VARCHAR, nullable = true)
  private def key(name: String): Column =
    Column(name, SqlTypeName.BIGINT, nullable = true)

  private val tables: Vector[(String, Vector[Column])] = Vector(
    "lsqb_q2_knows" -> Vector(
      Column("edge_id", SqlTypeName.BIGINT, nullable = false),
      key("person1_id"),
      key("person2_id"),
      payload
    ),
    "lsqb_q2_comment" -> Vector(
      Column("comment_id", SqlTypeName.BIGINT, nullable = false),
      key("creator_id"),
      key("reply_post_id"),
      payload
    ),
    "lsqb_q2_post" -> Vector(
      key("post_id"),
      key("creator_id"),
      payload
    ),
    "lsqb_q3_city" -> Vector(
      rowId,
      key("city_id"),
      key("country_id"),
      payload
    ),
    "lsqb_q3_person" -> Vector(
      rowId,
      key("person_id"),
      key("city_id"),
      payload
    ),
    "lsqb_q3_knows" -> Vector(
      rowId,
      key("person1_id"),
      key("person2_id"),
      payload
    ),
    "q5d_customer" -> Vector(
      rowId,
      key("customer_id"),
      key("nation_id"),
      payload
    ),
    "q5d_orders" -> Vector(
      rowId,
      key("customer_id"),
      key("order_id"),
      payload
    ),
    "q5d_lineitem" -> Vector(
      rowId,
      key("order_id"),
      key("supplier_id"),
      payload
    ),
    "q5d_supplier" -> Vector(
      rowId,
      key("supplier_id"),
      key("nation_id"),
      payload
    ),
    "q5d_nation" -> Vector(
      rowId,
      key("nation_id"),
      key("region_id"),
      payload
    ),
    "q5d_region" -> Vector(
      rowId,
      key("region_id"),
      payload
    )
  )

  /** A fresh root is returned because Calcite planners own mutable state. */
  def rootSchema(): SchemaPlus = {
    val root = Frameworks.createRootSchema(true)
    val temporarySchema = root.add("pg_temp", new AbstractSchema)
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
    root
  }
}
