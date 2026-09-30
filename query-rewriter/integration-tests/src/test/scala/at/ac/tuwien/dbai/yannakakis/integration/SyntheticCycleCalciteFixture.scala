package at.ac.tuwien.dbai.yannakakis.integration

import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.SchemaPlus
import org.apache.calcite.schema.impl.{AbstractSchema, AbstractTable}
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks

/** Data-free catalog for the bounded sparse/dense cycle fixtures. */
private[integration] object SyntheticCycleCalciteFixture {
  private final case class Column(
      name: String,
      sqlType: SqlTypeName,
      nullable: Boolean
  )

  private val columns = Vector(
    Column("row_id", SqlTypeName.BIGINT, nullable = false),
    Column("left_key", SqlTypeName.BIGINT, nullable = true),
    Column("right_key", SqlTypeName.BIGINT, nullable = true),
    Column("payload", SqlTypeName.VARCHAR, nullable = true)
  )

  def rootSchema(): SchemaPlus = {
    val root = Frameworks.createRootSchema(true)
    val temporarySchema = root.add("pg_temp", new AbstractSchema)
    SyntheticCycleMultisetFixture.All.foreach { dataset =>
      (0 until dataset.topology.edgeCount).foreach { edgeIndex =>
        temporarySchema.add(
          SyntheticCycleMultisetFixture.tableName(dataset, edgeIndex),
          new AbstractTable {
            override def getRowType(
                typeFactory: RelDataTypeFactory
            ): RelDataType = {
              val builder = typeFactory.builder()
              columns.foreach { column =>
                val baseType = typeFactory.createSqlType(column.sqlType)
                builder.add(
                  column.name,
                  typeFactory.createTypeWithNullability(
                    baseType,
                    column.nullable
                  )
                )
              }
              builder.build()
            }
          }
        )
      }
    }
    root
  }
}
