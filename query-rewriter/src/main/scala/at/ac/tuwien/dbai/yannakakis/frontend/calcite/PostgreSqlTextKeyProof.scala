package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import javax.sql.DataSource
import at.ac.tuwien.dbai.yannakakis.core.ColumnId
import org.apache.calcite.rel.{RelNode, RelRoot}
import org.apache.calcite.rel.core.TableScan
import org.apache.calcite.sql.{SqlCall, SqlCollation, SqlIdentifier, SqlLiteral, SqlNode}
import org.apache.calcite.sql.util.SqlShuttle
import org.apache.calcite.util.NlsString
import scala.jdk.CollectionConverters._
import scala.util.Using

/** Catalog evidence for byte-preserving structural equality. Constructor and
  * minting stay inside the JDBC frontend; a Calcite VARCHAR name alone is not
  * evidence of PostgreSQL type or collation semantics.
  */
final class PostgreSqlTextKeyProof private[calcite] (
    private val binding: QueryIR,
    private val columns: Map[ColumnId, PostgreSqlTextKeyCatalog.Column]
) extends Serializable {
  /** Stable catalog observations for explicitly versioned action manifests.
    * No connection details or credentials are retained.
    */
  private[yannakakis] def catalogEvidence: Vector[Map[String,String]] =
    columns.toVector.sortBy(_._1).map { case (id,c) => Map(
      "occurrence" -> id.relationOccurrenceId.value.toString,
      "column_ordinal" -> id.ordinal.toString, "schema" -> c.schema, "table" -> c.table,
      "column" -> c.name, "relation_oid" -> c.relationOid.toString,
      "attribute_number" -> c.attributeNumber.toString, "type_oid" -> c.typeOid.toString,
      "type_modifier" -> c.typeModifier.toString, "collation_oid" -> c.collationOid.toString,
      "deterministic" -> c.deterministic.toString, "calcite_type" -> c.dataType)
    }

  private[yannakakis] def accepts(query: QueryIR, keys: Vector[ColumnId]): Boolean = {
    val bound = query != null &&
      (query.root eq binding.root) && (query.relations eq binding.relations) &&
      (query.predicates eq binding.predicates) && (query.outputNames eq binding.outputNames) &&
      (query.sourceProof eq binding.sourceProof)
    val selected = keys.flatMap(columns.get)
    bound && keys.nonEmpty && selected.size == keys.size &&
      selected.forall(_.eligible) &&
      selected.map(c => (c.typeOid, c.typeModifier, c.collationOid)).distinct.size == 1
  }

  override def toString: String = "PostgreSqlTextKeyProof(<catalog-bound>)"
  // Data-value equality stays reproducible across independent analyses.
  // Authorization uses accepts' reference binding, never equals/hashCode.
  override def equals(other: Any): Boolean = other match {
    case proof: PostgreSqlTextKeyProof => columns == proof.columns
    case _ => false
  }
  override def hashCode(): Int = columns.hashCode()
}

/** One fresh, read-only catalog capture for the exact Calcite TableScans.
  * Relation resolution and absence of intervening DDL remain execution
  * premises, as for the existing integer path. No names are LIKE patterns.
  */
private[calcite] object PostgreSqlTextKeyCatalog {
  /** Inspect the raw AST before Calcite normalization can erase an explicit
    * collation. This is a typed traversal, not a search of the SQL string.
    */
  def sourceSafe(source: SqlNode): Boolean = {
    var safe = true
    source.accept(new SqlShuttle {
      override def visit(id: SqlIdentifier): SqlNode = {
        if (id.getCollation != null) safe = false
        super.visit(id)
      }
      override def visit(call: SqlCall): SqlNode = {
        if (call.getOperator.getName.equalsIgnoreCase("COLLATE")) safe = false
        super.visit(call)
      }
      override def visit(literal: SqlLiteral): SqlNode = {
        literal.getValue match {
          case value: NlsString if value.getCollation != null &&
              value.getCollation.getCoercibility == SqlCollation.Coercibility.EXPLICIT => safe = false
          case _ => ()
        }
        super.visit(literal)
      }
    })
    safe
  }

  final case class Column(
      schema: String,
      table: String,
      name: String,
      relationOid: Long,
      attributeNumber: Int,
      typeOid: Long,
      typeModifier: Int,
      collationOid: Long,
      deterministic: Boolean,
      sqlTypeName: String,
      dataType: String,
      nullable: Boolean
  ) extends Serializable {
    def eligible: Boolean = Set(25L, 1043L)(typeOid) && deterministic &&
      collationOid > 0L && sqlTypeName == "VARCHAR"
  }

  final class Capture private[PostgreSqlTextKeyCatalog] (
      private val columns: Vector[Column]
  ) {
    def certify(report: AnalysisReport): AnalysisReport = {
      val query = report.query
      val byName = columns.map(c => (Vector(c.schema,c.table),c.name) -> c).toMap
      val bound = query.relations.flatMap { relation =>
        relation.columns.flatMap { metadata =>
          byName.get((relation.qualifiedName, metadata.sourceName)).filter { c =>
            c.eligible && c.sqlTypeName == metadata.sqlTypeName &&
              c.dataType == metadata.dataType && c.nullable == metadata.nullable
          }.map(c => metadata.ref.id -> c)
        }
      }.toMap
      report.copy(query = query.copy(textKeyProof = Some(new PostgreSqlTextKeyProof(query, bound))))
    }
  }

  def capture(root: RelRoot, dataSource: DataSource, schema: String): Capture = {
    def scans(node: RelNode): Vector[TableScan] = node match {
      case scan: TableScan => Vector(scan)
      case other => other.getInputs.asScala.toVector.flatMap(scans)
    }
    val relations = scans(root.rel).groupBy(_.getTable.getQualifiedName.asScala.toVector)
    val columns = Using.resource(dataSource.getConnection) { connection =>
      connection.setReadOnly(true)
      connection.setTransactionIsolation(java.sql.Connection.TRANSACTION_REPEATABLE_READ)
      connection.setAutoCommit(false)
      try {
        Using.resource(connection.createStatement()) { statement =>
          statement.execute("SET LOCAL search_path = pg_catalog")
          statement.execute("SET LOCAL statement_timeout = 10000")
        }
        val sql = """SELECT c.oid, c.relkind, a.attnum, a.attname, a.atttypid,
          a.atttypmod, a.attcollation, co.collisdeterministic, a.attnotnull
          FROM pg_catalog.pg_namespace n
          JOIN pg_catalog.pg_class c ON c.relnamespace=n.oid
          JOIN pg_catalog.pg_attribute a ON a.attrelid=c.oid
          LEFT JOIN pg_catalog.pg_collation co ON co.oid=a.attcollation
          WHERE n.nspname=? AND c.relname=? AND a.attnum>0 AND NOT a.attisdropped
          ORDER BY a.attnum"""
        Using.resource(connection.prepareStatement(sql)) { statement =>
          statement.setQueryTimeout(10)
          relations.toVector.sortBy(_._1.mkString("\u0000")).flatMap { case (name, occurrences) =>
            require(name.size == 2 && name.head == schema,
              "text-key evidence requires exact same-schema base relations")
            val fields = occurrences.head.getRowType.getFieldList.asScala.toVector
            require(occurrences.forall(_.getRowType == occurrences.head.getRowType),
              "inconsistent Calcite scan metadata")
            statement.setString(1, schema); statement.setString(2, name.last)
            Using.resource(statement.executeQuery()) { result =>
              val rows = Vector.newBuilder[Column]
              var ordinal = 0
              while (result.next()) {
                require(Set("r", "p")(result.getString(2)), "text-key evidence requires a base table")
                require(ordinal < fields.size && fields(ordinal).getName == result.getString(4),
                  "Calcite/catalog column inventory mismatch")
                val field = fields(ordinal).getType
                val typeOid = result.getLong(5)
                val typmod = result.getInt(6)
                val nullable = !result.getBoolean(9)
                require(field.isNullable == nullable, "Calcite/catalog nullability mismatch")
                if (typeOid == 1043L && typmod >= 4)
                  require(field.getPrecision == typmod-4, "Calcite/catalog VARCHAR length mismatch")
                rows += Column(schema, name.last, fields(ordinal).getName, result.getLong(1),
                  result.getInt(3), typeOid, typmod, result.getLong(7), result.getBoolean(8),
                  field.getSqlTypeName.getName, field.getFullTypeString, nullable)
                ordinal += 1
              }
              require(ordinal == fields.size && ordinal > 0, "missing catalog relation or columns")
              rows.result()
            }
          }
        }
      } finally connection.rollback()
    }
    new Capture(columns)
  }
}
