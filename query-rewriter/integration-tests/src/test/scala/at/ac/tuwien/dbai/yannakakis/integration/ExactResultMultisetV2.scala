package at.ac.tuwien.dbai.yannakakis.integration

import java.sql.{
  Connection,
  ResultSet,
  ResultSetMetaData,
  SQLException
}

import org.postgresql.PGResultSetMetaData
import scala.collection.mutable
import scala.util.Using

/** Additive descriptor-aware successor to [[ExactResultMultiset]].
  *
  * Column position, label, JDBC type, PostgreSQL type name, precision, scale,
  * nullability, NULL markers, and duplicate multiplicities participate in the
  * contract comparison. Driver-supplied base-column origin is retained for the
  * stronger [[ResultMultiset.differenceIncludingOrigin]] diagnostic, but is not
  * part of the paper's observable-result contract.
  */
object ExactResultMultisetV2 {

  sealed trait CanonicalValue extends Product with Serializable

  case object SqlNull extends CanonicalValue

  final case class ScalarValue(kind: String, rendered: String)
      extends CanonicalValue

  final case class BinaryValue(bytes: Vector[Byte]) extends CanonicalValue

  final case class Cell(
      jdbcType: Int,
      databaseType: String,
      value: CanonicalValue
  )

  final case class ColumnSchema(
      columnLabel: String,
      jdbcType: Int,
      databaseType: String,
      precision: Int,
      scale: Int,
      nullability: Int = ResultSetMetaData.columnNullableUnknown,
      originTable: Option[String] = None,
      originColumn: Option[String] = None
  )

  final case class Row(cells: Vector[Cell]) {
    private[integration] def diagnostic: String = cells.mkString("[", ", ", "]")
  }

  final case class ResultMultiset private (
      schema: Vector[ColumnSchema],
      frequencies: Map[Row, Long]
  ) {
    require(frequencies.values.forall(_ > 0L), "frequencies must be positive")

    lazy val totalMultiplicity: Long = frequencies.values.sum

    def distinctRowCount: Int = frequencies.size

    def isEmpty: Boolean = frequencies.isEmpty

    def containsDuplicate: Boolean = frequencies.valuesIterator.exists(_ > 1L)

    def containsNull: Boolean = frequencies.keysIterator.exists(
      _.cells.exists(_.value == SqlNull)
    )

    /** Compare the observable JDBC contract. Base-column origin is diagnostic
      * metadata rather than part of the paper contract.
      */
    def difference(actual: ResultMultiset): Difference =
      withoutOrigin.differenceIncludingOrigin(actual.withoutOrigin)

    /** Additionally compare the driver-supplied base table/column origin.
      * Equivalent expressions can expose different yet observationally
      * irrelevant origins, so this is a diagnostic rather than a contract gate.
      */
    def differenceIncludingOrigin(actual: ResultMultiset): Difference = {
      val rows = frequencies.keySet ++ actual.frequencies.keySet

      val missing = rows.iterator.flatMap { row =>
        val count = frequencies.getOrElse(row, 0L) -
          actual.frequencies.getOrElse(row, 0L)
        Option.when(count > 0L)(row -> count)
      }.toMap

      val unexpected = rows.iterator.flatMap { row =>
        val count = actual.frequencies.getOrElse(row, 0L) -
          frequencies.getOrElse(row, 0L)
        Option.when(count > 0L)(row -> count)
      }.toMap

      Difference(
        expectedSchema = schema,
        actualSchema = actual.schema,
        missingFromActual = missing,
        unexpectedInActual = unexpected
      )
    }

    /** Compare a hand-authored value oracle without claiming that its
      * synthetic SELECT reproduces source-column nullability or origin.
      *
      * Native-vs-rewritten differential checks must use [[difference]] so the
      * observable driver descriptor remains part of the contract.
      */
    def differenceIgnoringNullabilityAndOrigin(
        actual: ResultMultiset
    ): Difference =
      withoutNullabilityAndOrigin.difference(
        actual.withoutNullabilityAndOrigin
      )

    private def withoutOrigin: ResultMultiset =
      copy(schema = schema.map(_.copy(
        originTable = None,
        originColumn = None
      )))

    private def withoutNullabilityAndOrigin: ResultMultiset =
      copy(schema = schema.map(_.copy(
        nullability = ResultSetMetaData.columnNullableUnknown,
        originTable = None,
        originColumn = None
      )))
  }

  final case class Difference(
      expectedSchema: Vector[ColumnSchema],
      actualSchema: Vector[ColumnSchema],
      missingFromActual: Map[Row, Long],
      unexpectedInActual: Map[Row, Long]
  ) {
    def schemaMatches: Boolean = expectedSchema == actualSchema

    def isEmpty: Boolean =
      schemaMatches && missingFromActual.isEmpty && unexpectedInActual.isEmpty

    def render: String = {
      def renderRows(rows: Map[Row, Long]): String =
        rows.toVector
          .sortBy(_._1.diagnostic)
          .map { case (row, multiplicity) =>
            s"$multiplicity x ${row.diagnostic}"
          }
          .mkString("\n")

      val schemaDifference =
        if (schemaMatches) "Schema matches."
        else
          s"""Schema mismatch:
             |  expected: ${expectedSchema.mkString("[", ", ", "]")}
             |  actual:   ${actualSchema.mkString("[", ", ", "]")}""".stripMargin

      s"""$schemaDifference
         |Missing from actual (${missingFromActual.values.sum} rows):
         |${renderRows(missingFromActual)}
         |Unexpected in actual (${unexpectedInActual.values.sum} rows):
         |${renderRows(unexpectedInActual)}""".stripMargin
    }
  }

  def execute(connection: Connection, sql: String): ResultMultiset =
    Using.resource(connection.createStatement()) { statement =>
      Using.resource(statement.executeQuery(sql))(read)
    }

  private def read(resultSet: ResultSet): ResultMultiset = {
    val metadata = resultSet.getMetaData
    val columnCount = metadata.getColumnCount
    val schema = readSchema(metadata)
    val counts = mutable.HashMap.empty[Row, Long].withDefaultValue(0L)

    while (resultSet.next()) {
      val cells = (1 to columnCount).iterator.map { column =>
        val rawValue = resultSet.getObject(column)
        Cell(
          jdbcType = metadata.getColumnType(column),
          databaseType = Option(metadata.getColumnTypeName(column)).getOrElse(""),
          value = canonicalize(rawValue)
        )
      }.toVector
      val row = Row(cells)
      counts.update(row, counts(row) + 1L)
    }

    ResultMultiset(schema = schema, frequencies = counts.toMap)
  }

  private[integration] def readSchema(
      metadata: ResultSetMetaData
  ): Vector[ColumnSchema] = {
    require(metadata != null, "result metadata must not be null")
    val postgresMetadata = postgresResultMetadata(metadata)

    (1 to metadata.getColumnCount).iterator.map { column =>
      val (originTable, originColumn) = columnOrigin(
        metadata,
        postgresMetadata,
        column
      )
      ColumnSchema(
        columnLabel = Option(metadata.getColumnLabel(column)).getOrElse(""),
        jdbcType = metadata.getColumnType(column),
        databaseType = Option(metadata.getColumnTypeName(column)).getOrElse(""),
        precision = metadata.getPrecision(column),
        scale = metadata.getScale(column),
        nullability = metadata.isNullable(column),
        originTable = originTable,
        originColumn = originColumn
      )
    }.toVector
  }

  private def postgresResultMetadata(
      metadata: ResultSetMetaData
  ): Option[PGResultSetMetaData] = metadata match {
    case postgres: PGResultSetMetaData => Some(postgres)
    case _ =>
      try {
        if (metadata.isWrapperFor(classOf[PGResultSetMetaData]))
          Option(metadata.unwrap(classOf[PGResultSetMetaData]))
        else None
      } catch {
        case _: SQLException => None
      }
  }

  private def columnOrigin(
      metadata: ResultSetMetaData,
      postgresMetadata: Option[PGResultSetMetaData],
      column: Int
  ): (Option[String], Option[String]) = postgresMetadata match {
    case Some(postgres) =>
      normalized(postgres.getBaseTableName(column)) ->
        normalized(postgres.getBaseColumnName(column))
    case None =>
      val table = normalized(metadata.getTableName(column))
      val columnName =
        table.flatMap(_ => normalized(metadata.getColumnName(column)))
      table -> columnName
  }

  private def normalized(value: String): Option[String] =
    Option(value).filter(_.nonEmpty)

  private def canonicalize(value: Any): CanonicalValue = value match {
    case null => SqlNull
    case bytes: Array[Byte] => BinaryValue(bytes.toVector)
    case decimal: java.math.BigDecimal =>
      ScalarValue("decimal", decimal.stripTrailingZeros().toPlainString)
    case number: java.math.BigInteger =>
      ScalarValue("integer", number.toString)
    case number: java.lang.Byte =>
      ScalarValue("byte", number.toString)
    case number: java.lang.Short =>
      ScalarValue("short", number.toString)
    case number: java.lang.Integer =>
      ScalarValue("integer", number.toString)
    case number: java.lang.Long =>
      ScalarValue("long", number.toString)
    case number: java.lang.Float =>
      ScalarValue(
        "float-bits",
        java.lang.Integer.toUnsignedString(
          java.lang.Float.floatToRawIntBits(number.floatValue())
        )
      )
    case number: java.lang.Double =>
      ScalarValue(
        "double-bits",
        java.lang.Long.toUnsignedString(
          java.lang.Double.doubleToRawLongBits(number.doubleValue())
        )
      )
    case value: java.lang.Boolean =>
      ScalarValue("boolean", value.toString)
    case value: java.lang.Character =>
      ScalarValue("character", value.toString)
    case value: String =>
      ScalarValue("string", value)
    case value: java.sql.Date =>
      ScalarValue("sql-date", value.toLocalDate.toString)
    case value: java.sql.Time =>
      ScalarValue("sql-time", value.toLocalTime.toString)
    case value: java.sql.Timestamp =>
      ScalarValue("sql-timestamp", value.toLocalDateTime.toString)
    case value: java.time.LocalDate =>
      ScalarValue("local-date", value.toString)
    case value: java.time.LocalTime =>
      ScalarValue("local-time", value.toString)
    case value: java.time.LocalDateTime =>
      ScalarValue("local-date-time", value.toString)
    case value: java.time.OffsetTime =>
      ScalarValue("offset-time", value.toString)
    case value: java.time.OffsetDateTime =>
      ScalarValue("offset-date-time", value.toString)
    case value: java.time.Instant =>
      ScalarValue("instant", value.toString)
    case value: java.util.UUID =>
      ScalarValue("uuid", value.toString)
    case other =>
      throw new IllegalArgumentException(
        s"Unsupported JDBC result value class: ${other.getClass.getName}"
      )
  }
}
