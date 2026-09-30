package at.ac.tuwien.dbai.yannakakis.integration

import at.ac.tuwien.dbai.yannakakis.integration.{
  ExactResultMultisetV2 => ExactResultMultiset
}
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.sql.{ResultSetMetaData, SQLException, Types}

import org.postgresql.PGResultSetMetaData

private object ExactResultMultisetDescriptorSuite {
  final case class Descriptor(
      label: String,
      jdbcType: Int,
      databaseType: String,
      precision: Int,
      scale: Int,
      nullability: Int,
      tableName: String,
      columnName: String,
      baseTableName: String,
      baseColumnName: String
  )
}

class ExactResultMultisetDescriptorSuite extends munit.FunSuite {
  import ExactResultMultiset.{ColumnSchema, ResultMultiset}
  import ExactResultMultisetDescriptorSuite.Descriptor

  test("PostgreSQL base origin is retained independently of the output label") {
    val metadata = metadataProxy(
      Vector(
        Descriptor(
          label = "visible alias",
          jdbcType = Types.INTEGER,
          databaseType = "int4",
          precision = 10,
          scale = 0,
          nullability = ResultSetMetaData.columnNoNulls,
          tableName = "standard table",
          columnName = "visible alias",
          baseTableName = "base_table",
          baseColumnName = "base_column"
        ),
        Descriptor(
          label = "?column?",
          jdbcType = Types.BOOLEAN,
          databaseType = "bool",
          precision = 1,
          scale = 0,
          nullability = ResultSetMetaData.columnNullableUnknown,
          tableName = "",
          columnName = "?column?",
          baseTableName = "",
          baseColumnName = ""
        )
      ),
      postgres = true
    )

    assertEquals(
      ExactResultMultiset.readSchema(metadata),
      Vector(
        ColumnSchema(
          columnLabel = "visible alias",
          jdbcType = Types.INTEGER,
          databaseType = "int4",
          precision = 10,
          scale = 0,
          nullability = ResultSetMetaData.columnNoNulls,
          originTable = Some("base_table"),
          originColumn = Some("base_column")
        ),
        ColumnSchema(
          columnLabel = "?column?",
          jdbcType = Types.BOOLEAN,
          databaseType = "bool",
          precision = 1,
          scale = 0,
          nullability = ResultSetMetaData.columnNullableUnknown,
          originTable = None,
          originColumn = None
        )
      )
    )
  }

  test("generic JDBC origin is retained only when a table origin exists") {
    val metadata = metadataProxy(
      Vector(
        Descriptor(
          label = "renamed",
          jdbcType = Types.BIGINT,
          databaseType = "int8",
          precision = 19,
          scale = 0,
          nullability = ResultSetMetaData.columnNullable,
          tableName = "generic_table",
          columnName = "generic_column",
          baseTableName = "unused",
          baseColumnName = "unused"
        ),
        Descriptor(
          label = "expression label",
          jdbcType = Types.BIGINT,
          databaseType = "int8",
          precision = 19,
          scale = 0,
          nullability = ResultSetMetaData.columnNullableUnknown,
          tableName = "",
          columnName = "expression label",
          baseTableName = "unused",
          baseColumnName = "unused"
        )
      ),
      postgres = false,
      wrapperFailure = true
    )

    val schema = ExactResultMultiset.readSchema(metadata)
    assertEquals(schema.head.originTable, Some("generic_table"))
    assertEquals(schema.head.originColumn, Some("generic_column"))
    assertEquals(schema(1).originTable, None)
    assertEquals(schema(1).originColumn, None)
  }

  test("contract comparison includes labels and nullability but not origin") {
    val base = ColumnSchema(
      columnLabel = "value",
      jdbcType = Types.INTEGER,
      databaseType = "int4",
      precision = 10,
      scale = 0,
      nullability = ResultSetMetaData.columnNoNulls,
      originTable = Some("r"),
      originColumn = Some("a")
    )

    Vector(
      base.copy(columnLabel = "value0"),
      base.copy(nullability = ResultSetMetaData.columnNullable)
    ).foreach { changed =>
      val expected = ResultMultiset(Vector(base), Map.empty)
      val actual = ResultMultiset(Vector(changed), Map.empty)
      val difference = expected.difference(actual)
      assert(!difference.schemaMatches, clues(changed))
      assert(!difference.isEmpty, clues(changed))
    }

    Vector(
      base.copy(originTable = Some("s")),
      base.copy(originColumn = Some("b")),
      base.copy(originTable = None, originColumn = None)
    ).foreach { changed =>
      val expected = ResultMultiset(Vector(base), Map.empty)
      val actual = ResultMultiset(Vector(changed), Map.empty)
      val contractDifference = expected.difference(actual)
      assert(contractDifference.schemaMatches, clues(changed))
      assert(contractDifference.isEmpty, clues(changed))

      val originDiagnostic = expected.differenceIncludingOrigin(actual)
      assert(!originDiagnostic.schemaMatches, clues(changed))
      assert(!originDiagnostic.isEmpty, clues(changed))
    }

    val labelDifference = ResultMultiset(Vector(base), Map.empty)
      .differenceIgnoringNullabilityAndOrigin(
        ResultMultiset(Vector(base.copy(columnLabel = "value0")), Map.empty)
      )
    assert(!labelDifference.schemaMatches)
  }

  private def metadataProxy(
      descriptors: Vector[Descriptor],
      postgres: Boolean,
      wrapperFailure: Boolean = false
  ): ResultSetMetaData = {
    val interfaces =
      if (postgres)
        Array[Class[_]](
          classOf[ResultSetMetaData],
          classOf[PGResultSetMetaData]
        )
      else Array[Class[_]](classOf[ResultSetMetaData])

    Proxy
      .newProxyInstance(
        getClass.getClassLoader,
        interfaces,
        new InvocationHandler {
          override def invoke(
              proxy: Any,
              method: Method,
              arguments: Array[AnyRef]
          ): AnyRef = {
            def descriptor: Descriptor = {
              val column = arguments(0).asInstanceOf[java.lang.Integer].intValue()
              descriptors(column - 1)
            }

            method.getName match {
              case "getColumnCount" => Int.box(descriptors.size)
              case "getColumnLabel" => descriptor.label
              case "getColumnType" => Int.box(descriptor.jdbcType)
              case "getColumnTypeName" => descriptor.databaseType
              case "getPrecision" => Int.box(descriptor.precision)
              case "getScale" => Int.box(descriptor.scale)
              case "isNullable" => Int.box(descriptor.nullability)
              case "getTableName" => descriptor.tableName
              case "getColumnName" => descriptor.columnName
              case "getBaseTableName" => descriptor.baseTableName
              case "getBaseColumnName" => descriptor.baseColumnName
              case "isWrapperFor" if wrapperFailure =>
                throw new SQLException("wrapper probing is unsupported")
              case "isWrapperFor" => Boolean.box(false)
              case "unwrap" => null
              case "toString" => "ResultSetMetaData proxy"
              case "hashCode" => Int.box(System.identityHashCode(proxy))
              case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq arguments(0))
              case other =>
                fail(s"unexpected metadata call: $other")
            }
          }
        }
      )
      .asInstanceOf[ResultSetMetaData]
  }
}
