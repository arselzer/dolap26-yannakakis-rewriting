package at.ac.tuwien.dbai.yannakakis.integration

import java.sql.{Connection, DriverManager}

import at.ac.tuwien.dbai.yannakakis.algorithm.lowering.postgresql.{
  PostgreSqlRelevanceLimitedBaseTableEvidence,
  PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier,
  PostgreSqlRelevanceLimitedCountFailureClass,
  PostgreSqlRelevanceLimitedCountRenderer
}
import at.ac.tuwien.dbai.yannakakis.api.{
  CandidatePlanSelection,
  RewriteOptions,
  RewriteOutcome,
  RewriteRequest,
  YannakakisReductionMode
}
import at.ac.tuwien.dbai.yannakakis.api.postgresql.{
  PostgreSqlCandidateSelectionDecision,
  PostgreSqlQueryRewriter,
  PostgreSqlRewriteArtifact
}
import org.postgresql.ds.PGSimpleDataSource
import org.testcontainers.postgresql.PostgreSQLContainer

import scala.util.Using

/** Live bag/NULL/empty-input differential gate for the V4 shared-wing SQL. */
final class PostgreSqlRelevanceLimitedCountDifferentialSuite
    extends munit.FunSuite {
  private final case class Database(
      jdbcUrl: String,
      username: String,
      password: String
  )

  private var postgres: PostgreSQLContainer = _
  private var connection: Connection = _
  private var dataSource: PGSimpleDataSource = _
  private var rewriter: PostgreSqlQueryRewriter = _
  private var schemaName: String = _
  private var database: Database = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    database = externalDatabase.getOrElse {
      postgres = new PostgreSQLContainer("postgres:18.4-alpine")
        .withDatabaseName("relevance_limited_count")
        .withUsername("relevance_limited_count")
        .withPassword("relevance_limited_count")
      postgres.start()
      Database(postgres.getJdbcUrl, postgres.getUsername, postgres.getPassword)
    }
    schemaName = s"yrw_v4_count_gate_${java.lang.Long.toUnsignedString(System.nanoTime())}"
    connection = DriverManager.getConnection(
      database.jdbcUrl,
      database.username,
      database.password
    )
    install(connection, schemaName)
    dataSource = new PGSimpleDataSource
    dataSource.setURL(database.jdbcUrl)
    dataSource.setUser(database.username)
    dataSource.setPassword(database.password)
    rewriter = PostgreSqlQueryRewriter
      .forPostgreSqlJdbc(
        database.jdbcUrl,
        database.username,
        database.password,
        schemaName
      )
      .fold(
        problem =>
          throw new IllegalStateException(s"${problem.code}: ${problem.message}"),
        identity
      )
  }

  override def afterAll(): Unit =
    try {
      if (connection != null && schemaName != null) {
        try {
          Using.resource(connection.createStatement())(
            _.execute(s"DROP SCHEMA ${quoted(schemaName)} CASCADE")
          )
        } catch { case _: Throwable => () }
      }
      if (connection != null) connection.close()
    } finally {
      try if (postgres != null) postgres.stop()
      finally super.afterAll()
    }

  test("V4 is exact for duplicates, equal endpoints, NULLs, and dangling keys") {
    val artifact = rewrite(sourceSql)
    val evidence = PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier
      .verify(dataSource, artifact.query)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
    val rendered = PostgreSqlRelevanceLimitedCountRenderer
      .render(
        artifact.query,
        artifact.candidateInput,
        artifact.bagFactorPlan,
        evidence
      )
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
    revalidate(evidence)

    val native = execute(sourceSql)
    val actual = execute(rendered.sql)
    assertEquals(native.totalMultiplicity, 1L)
    assertEquals(actual.totalMultiplicity, 1L)
    assertEquals(scalarLong(native), 30L)
    val difference = native.difference(actual)
    assert(difference.isEmpty, difference.render)
  }

  test("V4 returns the same scalar zero on empty input") {
    val artifact = rewrite(sourceSql)
    val evidence = PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier
      .verify(dataSource, artifact.query)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
    val rendered = PostgreSqlRelevanceLimitedCountRenderer
      .render(
        artifact.query,
        artifact.candidateInput,
        artifact.bagFactorPlan,
        evidence
      )
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
    revalidate(evidence)

    connection.setAutoCommit(false)
    try {
      Using.resource(connection.createStatement())(
        _.execute(s"DELETE FROM ${qualified("v4_link")}")
      )
      val native = execute(sourceSql)
      val actual = execute(rendered.sql)
      assertEquals(scalarLong(native), 0L)
      val difference = native.difference(actual)
      assert(difference.isEmpty, difference.render)
    } finally {
      connection.rollback()
      connection.setAutoCommit(true)
    }
  }

  test("catalog revalidation fails closed when a scanned table gains a child") {
    val artifact = rewrite(sourceSql)
    val evidence = PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier
      .verify(dataSource, artifact.query)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
    val child = qualified("v4_dim_child")

    Using.resource(connection.createStatement())(
      _.execute(s"CREATE TABLE $child () INHERITS (${qualified("v4_dim")})")
    )
    try {
      val failure = PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier
        .revalidate(dataSource, evidence)
        .left
        .getOrElse(fail("expected inheritance drift to fail closed"))
      assertEquals(
        failure.failureClass,
        PostgreSqlRelevanceLimitedCountFailureClass.NotApplicable
      )
      assertEquals(
        failure.code,
        "relevance-limited-count.not-applicable.catalog-inheritance"
      )
    } finally {
      Using.resource(connection.createStatement())(_.execute(s"DROP TABLE $child"))
    }
  }

  test("V4 is exact for a JOB-shaped composite wing and cyclic core") {
    val artifact = rewrite(jobShapedSql, widthBound = 3)
    val evidence = PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier
      .verify(dataSource, artifact.query)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
    val rendered = PostgreSqlRelevanceLimitedCountRenderer
      .render(
        artifact.query,
        artifact.candidateInput,
        artifact.bagFactorPlan,
        evidence
      )
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
    revalidate(evidence)

    assertEquals(rendered.trace.members.size, 2)
    assertEquals(rendered.trace.representativeWingEdgeIds.size, 4)
    assert(rendered.trace.coreEdgeIds.size >= 6, clues(rendered.trace))
    val native = execute(jobShapedSql)
    val actual = execute(rendered.sql)
    assertEquals(scalarLong(native), 336L)
    val difference = native.difference(actual)
    assert(difference.isEmpty, difference.render)
  }

  test("catalog admission rejects a PostgreSQL oid misreported as BIGINT") {
    val artifact = rewrite(oidMismatchSql)
    val failure = PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier
      .verify(dataSource, artifact.query)
      .left
      .getOrElse(fail("expected exact PostgreSQL type-OID rejection"))

    assertEquals(
      failure.failureClass,
      PostgreSqlRelevanceLimitedCountFailureClass.NotApplicable
    )
    assertEquals(
      failure.code,
      "relevance-limited-count.not-applicable.catalog-structural-type"
    )
    assert(failure.detail.contains("OID 26"), clues(failure))
  }

  test("catalog admission is immune to a hostile oid equality operator") {
    val artifact = rewrite(jobShapedSql, widthBound = 3)
    val hostileSchema = schemaName + "_hostile"
    Using.resource(connection.createStatement()) { statement =>
      statement.execute(s"CREATE SCHEMA ${quoted(hostileSchema)}")
      statement.execute(
        s"CREATE FUNCTION ${quoted(hostileSchema)}.always_false(oid, oid) " +
          "RETURNS boolean LANGUAGE SQL IMMUTABLE AS 'SELECT false'"
      )
      statement.execute(
        s"CREATE OPERATOR ${quoted(hostileSchema)}.= " +
          s"(FUNCTION = ${quoted(hostileSchema)}.always_false, " +
          "LEFTARG = oid, RIGHTARG = oid)"
      )
    }
    try {
      val hostile = new PGSimpleDataSource
      hostile.setURL(database.jdbcUrl)
      hostile.setUser(database.username)
      hostile.setPassword(database.password)
      hostile.setCurrentSchema(s"$hostileSchema,pg_catalog")
      Using.resource(hostile.getConnection) { hostileConnection =>
        Using.resource(hostileConnection.createStatement()) { statement =>
          Using.resource(statement.executeQuery("SELECT 1::oid = 1::oid")) { result =>
            assert(result.next())
            assertEquals(result.getBoolean(1), false)
          }
        }
      }

      val evidence = PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier
        .verify(hostile, artifact.query)
        .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
      assert(evidence.relations.nonEmpty)
    } finally {
      Using.resource(connection.createStatement())(
        _.execute(s"DROP SCHEMA ${quoted(hostileSchema)} CASCADE")
      )
    }
  }

  test("V4 is exact for built-in SMALLINT and BIGINT structural columns") {
    Vector(
      (integralSql("v4_small_anchor", "v4_small_wing"), 2L),
      (integralSql("v4_big_anchor", "v4_big_wing"), 2L)
    ).foreach { case (sql, expected) =>
      val artifact = rewrite(sql)
      val evidence = PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier
        .verify(dataSource, artifact.query)
        .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
      val rendered = PostgreSqlRelevanceLimitedCountRenderer
        .render(
          artifact.query,
          artifact.candidateInput,
          artifact.bagFactorPlan,
          evidence
        )
        .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
      revalidate(evidence)

      val native = execute(sql)
      val actual = execute(rendered.sql)
      assertEquals(scalarLong(native), expected)
      val difference = native.difference(actual)
      assert(difference.isEmpty, clues(sql, difference.render))
    }
  }

  private def sourceSql: String =
    s"""SELECT COUNT(*) AS exact_count
       |FROM ${qualified("v4_link")} AS l
       |JOIN ${qualified("v4_fact")} AS f1 ON f1.movie_id = l.left_key
       |JOIN ${qualified("v4_dim")} AS d1 ON d1.id = f1.dim_id
       |JOIN ${qualified("v4_fact")} AS f2 ON f2.movie_id = l.right_key
       |JOIN ${qualified("v4_dim")} AS d2 ON d2.id = f2.dim_id""".stripMargin

  private def jobShapedSql: String =
    s"""SELECT COUNT(*) AS exact_count
       |FROM ${qualified("v4_link_job")} AS l
       |JOIN ${qualified("v4_title")} AS t1 ON t1.movie_id = l.left_key
       |JOIN ${qualified("v4_kind")} AS k1 ON k1.id = t1.kind_id
       |JOIN ${qualified("v4_info")} AS i1 ON i1.movie_id = l.left_key
       |JOIN ${qualified("v4_info_type")} AS it1 ON it1.id = i1.info_type_id
       |JOIN ${qualified("v4_title")} AS t2 ON t2.movie_id = l.right_key
       |JOIN ${qualified("v4_kind")} AS k2 ON k2.id = t2.kind_id
       |JOIN ${qualified("v4_info")} AS i2 ON i2.movie_id = l.right_key
       |JOIN ${qualified("v4_info_type")} AS it2 ON it2.id = i2.info_type_id
       |JOIN ${qualified("v4_link_type")} AS lt ON lt.id = l.link_type_id
       |JOIN ${qualified("v4_company")} AS c1 ON c1.movie_id = l.left_key
       |JOIN ${qualified("v4_company_name")} AS n1 ON n1.id = c1.company_id
       |JOIN ${qualified("v4_company")} AS c2 ON c2.movie_id = l.right_key
       |JOIN ${qualified("v4_company_name")} AS n2 ON n2.id = c2.company_id
       |WHERE c1.company_id = c2.company_id""".stripMargin

  private def oidMismatchSql: String =
    s"""SELECT COUNT(*) AS exact_count
       |FROM ${qualified("v4_oid_anchor")} AS a
       |JOIN ${qualified("v4_oid_wing")} AS w1 ON w1.movie_id = a.left_key
       |JOIN ${qualified("v4_oid_wing")} AS w2 ON w2.movie_id = a.right_key""".stripMargin

  private def integralSql(anchor: String, wing: String): String =
    s"""SELECT COUNT(*) AS exact_count
       |FROM ${qualified(anchor)} AS a
       |JOIN ${qualified(wing)} AS w1 ON w1.movie_id = a.left_key
       |JOIN ${qualified(wing)} AS w2 ON w2.movie_id = a.right_key""".stripMargin

  private def rewrite(
      sql: String,
      widthBound: Int = 1
  ): PostgreSqlRewriteArtifact = {
    val result = rewriter.rewriteDetailed(
      RewriteRequest(
        sql,
        options = RewriteOptions(
          widthBound = widthBound,
          planLimit = 10,
          requireConnectedCover = true,
          candidateSelection = CandidatePlanSelection.StructuralHead,
          reductionMode = YannakakisReductionMode.NoReduction
        )
      )
    )
    result.outcome match {
      case _: RewriteOutcome.Rewritten => ()
      case other => fail(s"source did not rewrite: $other")
    }
    val artifact = result.report.artifact.getOrElse(fail("rewrite artifact missing"))
    artifact.candidateSelection match {
      case _: PostgreSqlCandidateSelectionDecision.Selected => artifact
    }
  }

  private def install(connection: Connection, schema: String): Unit =
    Using.resource(connection.createStatement()) { statement =>
      statement.execute(s"CREATE SCHEMA ${quoted(schema)}")
      statement.execute(
        s"CREATE TABLE ${qualified("v4_link")} (left_key INTEGER, right_key INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_fact")} (movie_id INTEGER, dim_id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_dim")} (id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_link_job")} " +
          "(left_key INTEGER, right_key INTEGER, link_type_id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_title")} (movie_id INTEGER, kind_id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_kind")} (id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_info")} (movie_id INTEGER, info_type_id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_info_type")} (id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_link_type")} (id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_company")} (movie_id INTEGER, company_id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_company_name")} (id INTEGER)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_oid_anchor")} " +
          "(left_key OID, right_key BIGINT)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_oid_wing")} (movie_id BIGINT)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_small_anchor")} " +
          "(left_key SMALLINT, right_key SMALLINT)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_small_wing")} (movie_id SMALLINT)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_big_anchor")} " +
          "(left_key BIGINT, right_key BIGINT)"
      )
      statement.execute(
        s"CREATE TABLE ${qualified("v4_big_wing")} (movie_id BIGINT)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_dim")} VALUES (10)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_fact")} VALUES " +
          "(1,10),(1,10),(1,10),(2,10),(2,10),(77,999)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_link")} VALUES " +
          "(1,1),(1,1),(1,2),(2,1),(NULL,1),(1,NULL),(99,1)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_link_job")} VALUES " +
          "(1,2,7),(1,2,7),(1,1,7),(NULL,1,7),(1,99,7)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_kind")} VALUES (10),(11)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_title")} VALUES (1,10),(1,11),(2,10)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_info_type")} VALUES (20),(21)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_info")} VALUES " +
          "(1,20),(1,20),(1,21),(2,20),(2,21)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_link_type")} VALUES (7)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_company")} VALUES " +
          "(1,100),(1,100),(1,101),(2,100),(2,102)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_company_name")} VALUES " +
          "(100),(101),(101),(102)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_small_anchor")} VALUES (1,2),(1,NULL)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_small_wing")} VALUES (1),(1),(2)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_big_anchor")} VALUES " +
          "(4000000000,5000000000),(4000000000,NULL)"
      )
      statement.execute(
        s"INSERT INTO ${qualified("v4_big_wing")} VALUES " +
          "(4000000000),(4000000000),(5000000000)"
      )
    }

  private def execute(sql: String): ExactResultMultisetV2.ResultMultiset =
    ExactResultMultisetV2.execute(connection, sql)

  private def revalidate(
      evidence: PostgreSqlRelevanceLimitedBaseTableEvidence
  ): Unit =
    PostgreSqlRelevanceLimitedBaseTableEvidenceVerifier
      .revalidate(dataSource, evidence)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def scalarLong(result: ExactResultMultisetV2.ResultMultiset): Long = {
    val row = result.frequencies.iterator.toVector match {
      case Vector((values, multiplicity)) if multiplicity == 1L => values.cells
      case other => fail(s"expected one scalar result row, found $other")
    }
    row match {
      case Vector(ExactResultMultisetV2.Cell(
            _,
            _,
            ExactResultMultisetV2.ScalarValue(_, value)
          )) => value.toLong
      case other => fail(s"expected one non-NULL scalar, found $other")
    }
  }

  private def qualified(relation: String): String =
    s"${quoted(schemaName)}.${quoted(relation)}"

  private def quoted(value: String): String =
    "\"" + value.replace("\"", "\"\"") + "\""

  private def externalDatabase: Option[Database] = {
    val urlName = "YANNAKAKIS_TEST_JDBC_URL"
    val userName = "YANNAKAKIS_TEST_DB_USER"
    val passwordName = "YANNAKAKIS_TEST_DB_PASSWORD"
    val names = Vector(urlName, userName, passwordName)
    val present = names.filter(sys.env.contains)
    if (present.isEmpty) None
    else if (present.size != names.size)
      throw new IllegalArgumentException(
        "External PostgreSQL test configuration is incomplete; missing: " +
          names.filterNot(sys.env.contains).mkString(", ")
      )
    else
      Some(Database(sys.env(urlName), sys.env(userName), sys.env(passwordName)))
  }
}
