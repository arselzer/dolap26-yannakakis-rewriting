package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.impl.AbstractTable
import org.apache.calcite.sql.{SqlCollation, SqlIdentifier}
import org.apache.calcite.sql.parser.{SqlParser, SqlParserPos}
import org.apache.calcite.sql.`type`.SqlTypeName
import org.apache.calcite.tools.Frameworks
import at.ac.tuwien.dbai.yannakakis.api.RewriteRequest
import at.ac.tuwien.dbai.yannakakis.api.postgresql.PostgreSqlQueryRewriter

class PostgreSqlTextKeyProofSuite extends munit.FunSuite {
  private val schema = Frameworks.createRootSchema(true)
  schema.add("edge", new AbstractTable {
    override def getRowType(f: RelDataTypeFactory): RelDataType = f.builder()
      .add("a", SqlTypeName.VARCHAR,64).nullable(true)
      .add("b", SqlTypeName.VARCHAR,64).nullable(true).build()
  })
  private val planner = CalciteSqlPlanner.forSchemaWithLocalPredicates(schema).toOption.get
  private val sql = "SELECT COUNT(*) FROM edge x JOIN edge y ON x.b=y.a"
  private def query() = CalciteQueryAnalyzer.analyze(
    CalciteSqlPlanner.planWithSourceEvidence(planner,sql).toOption.get).query
  private def columns(q: QueryIR) = q.relations.flatMap { r => r.columns.map { m =>
    m.ref.id -> PostgreSqlTextKeyCatalog.Column("fixture","edge",m.sourceName,123L,
      m.ref.ordinal+1,1043L,68,100L,true,m.sqlTypeName,m.dataType,m.nullable)
  }}.toMap

  test("VARCHAR metadata alone never grants structural admission") {
    val result = PostgreSqlQueryRewriter.create(planner).toOption.get.rewriteDetailed(RewriteRequest(sql))
    assert(result.report.artifact.isEmpty)
    assertEquals(query().textKeyProof, None)
  }

  test("proof is bound to immutable query inventories, not a transferable type flag") {
    val a = query(); val b = query(); val cols = columns(a)
    val proof = new PostgreSqlTextKeyProof(a,cols)
    assert(proof.accepts(a,cols.keys.toVector))
    assert(proof.accepts(a.copy(textKeyProof=Some(proof)),cols.keys.toVector))
    assert(!proof.accepts(b.copy(textKeyProof=Some(proof)),cols.keys.toVector))
    assert(!proof.accepts(a,Vector.empty))
    assert(!new PostgreSqlTextKeyProof(a,cols.tail).accepts(a,cols.keys.toVector))
  }

  test("type, length and collation must agree throughout each variable class") {
    val q = query(); val cols = columns(q); val key = cols.keys.head
    Vector(cols(key).copy(typeOid=25L), cols(key).copy(typeModifier=132),
      cols(key).copy(collationOid=200L), cols(key).copy(deterministic=false)).foreach { changed =>
      assert(!new PostgreSqlTextKeyProof(q,cols.updated(key,changed)).accepts(q,cols.keys.toVector))
    }
  }

  test("only built-in text/varchar with deterministic nonzero collation is admitted") {
    val base = columns(query()).values.head
    assert(base.eligible)
    assert(base.copy(typeOid=25L,typeModifier = -1).eligible)
    Vector(base.copy(typeOid=1042L),base.copy(typeOid=99999L),base.copy(collationOid=0),
      base.copy(deterministic=false),base.copy(sqlTypeName="CHAR")).foreach(c => assert(!c.eligible))
  }

  test("explicit source collations are rejected structurally, not via SQL text matching") {
    assert(PostgreSqlTextKeyCatalog.sourceSafe(SqlParser.create(
      "SELECT x.a FROM edge x WHERE x.a = 'COLLATE'", CalciteSqlPlanner.PostgreSqlParserConfig).parseQuery()))
    val explicit = new SqlIdentifier("a",
      new SqlCollation("UTF-8$en_US",SqlCollation.Coercibility.EXPLICIT),SqlParserPos.ZERO)
    assert(!PostgreSqlTextKeyCatalog.sourceSafe(explicit))
  }

  test("text-key factory rejects invalid caller inputs before JDBC access") {
    assert(CalciteSqlPlanner.forJdbcDataSourceWithTextKeys(null,"fixture").isLeft)
  }
}
