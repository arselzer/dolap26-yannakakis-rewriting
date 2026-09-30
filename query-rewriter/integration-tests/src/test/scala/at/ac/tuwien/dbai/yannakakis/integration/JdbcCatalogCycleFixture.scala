package at.ac.tuwien.dbai.yannakakis.integration

import java.sql.Connection

import scala.util.Using

/**
  * Persistent-schema fixture for exercising Calcite's real PostgreSQL catalog
  * path. Its triangle query is independently authored for this integration
  * gate and is not a benchmark workload.
  */
private[integration] object JdbcCatalogCycleFixture {
  val ExpectedMultiplicity: Long = 9L
  val Tables: Vector[String] =
    Vector("jdbc_cycle_r", "jdbc_cycle_s", "jdbc_cycle_t")

  val UnqualifiedEnumerationCore: String =
    """SELECT
      |  "r"."x" AS "x",
      |  "r"."y" AS "y",
      |  "s"."z" AS "z",
      |  "r"."payload" AS "r_payload",
      |  "s"."payload" AS "s_payload",
      |  "t"."payload" AS "t_payload"
      |FROM "jdbc_cycle_r" AS "r"
      |JOIN "jdbc_cycle_s" AS "s" ON "r"."y" = "s"."y"
      |JOIN "jdbc_cycle_t" AS "t"
      |  ON "s"."z" = "t"."z" AND "t"."x" = "r"."x"
      |""".stripMargin

  /** Same query with PostgreSQL's lower-casing of unquoted identifiers. */
  val UnquotedEnumerationCore: String =
    UnqualifiedEnumerationCore.replace("\"", "")

  def qualifiedEnumerationCore(schemaName: String): String = {
    val schema = quotedSchema(schemaName)
    s"""SELECT
       |  "r"."x" AS "x",
       |  "r"."y" AS "y",
       |  "s"."z" AS "z",
       |  "r"."payload" AS "r_payload",
       |  "s"."payload" AS "s_payload",
       |  "t"."payload" AS "t_payload"
       |FROM $schema."jdbc_cycle_r" AS "r"
       |JOIN $schema."jdbc_cycle_s" AS "s" ON "r"."y" = "s"."y"
       |JOIN $schema."jdbc_cycle_t" AS "t"
       |  ON "s"."z" = "t"."z" AND "t"."x" = "r"."x"
       |""".stripMargin
  }

  def install(connection: Connection, schemaName: String): Unit = {
    val schema = quotedSchema(schemaName)
    transactionally(connection) {
      Using.resource(connection.createStatement()) { statement =>
        Vector(
          s"CREATE SCHEMA $schema",
          s"""CREATE TABLE $schema."jdbc_cycle_r" (
             |  row_id BIGINT NOT NULL,
             |  x BIGINT,
             |  y BIGINT,
             |  payload TEXT
             |)""".stripMargin,
          s"""CREATE TABLE $schema."jdbc_cycle_s" (
             |  row_id BIGINT NOT NULL,
             |  y BIGINT,
             |  z BIGINT,
             |  payload TEXT
             |)""".stripMargin,
          s"""CREATE TABLE $schema."jdbc_cycle_t" (
             |  row_id BIGINT NOT NULL,
             |  z BIGINT,
             |  x BIGINT,
             |  payload TEXT
             |)""".stripMargin,
          s"""INSERT INTO $schema."jdbc_cycle_r"
             |  (row_id, x, y, payload) VALUES
             |  (1, 10, 20, 'r-a'),
             |  (1, 10, 20, 'r-a'),
             |  (3, 11, 21, NULL),
             |  (4, NULL, 20, 'r-null-key'),
             |  (5, 12, 22, 'r-dangling')""".stripMargin,
          s"""INSERT INTO $schema."jdbc_cycle_s"
             |  (row_id, y, z, payload) VALUES
             |  (1, 20, 30, 's-a'),
             |  (1, 20, 30, 's-a'),
             |  (3, 21, 31, NULL),
             |  (4, 20, NULL, 's-null-key'),
             |  (5, 22, 32, 's-dangling')""".stripMargin,
          s"""INSERT INTO $schema."jdbc_cycle_t"
             |  (row_id, z, x, payload) VALUES
             |  (1, 30, 10, 't-a'),
             |  (1, 30, 10, 't-a'),
             |  (3, 31, 11, NULL),
             |  (4, NULL, NULL, 't-null-key'),
             |  (5, 99, 99, 't-dangling')""".stripMargin
        ).foreach(statement.execute)
      }
    }
  }

  def drop(connection: Connection, schemaName: String): Unit = {
    val schema = quotedSchema(schemaName)
    Using.resource(connection.createStatement()) { statement =>
      statement.execute(s"DROP SCHEMA IF EXISTS $schema CASCADE")
    }
  }

  private def quotedSchema(schemaName: String): String = {
    require(
      schemaName != null &&
        schemaName.matches("YannakakisCycle_[0-9a-f]{32}"),
      "test schema must have the generated YannakakisCycle_<uuid> shape"
    )
    s"\"$schemaName\""
  }

  /** PostgreSQL DDL is transactional; isolate setup on shared connections. */
  private def transactionally(
      connection: Connection
  )(operation: => Unit): Unit = {
    val originalAutoCommit = connection.getAutoCommit
    if (originalAutoCommit) {
      connection.setAutoCommit(false)
      var primary: Throwable = null
      try {
        operation
        connection.commit()
      } catch {
        case problem: Throwable =>
          primary = problem
          cleanupPreserving(problem)(connection.rollback())
          throw problem
      } finally {
        cleanupPreserving(primary)(connection.setAutoCommit(true))
      }
    } else {
      val savepoint = connection.setSavepoint()
      try {
        operation
        connection.releaseSavepoint(savepoint)
      } catch {
        case problem: Throwable =>
          cleanupPreserving(problem)(connection.rollback(savepoint))
          cleanupPreserving(problem)(connection.releaseSavepoint(savepoint))
          throw problem
      }
    }
  }

  private def cleanupPreserving(primary: Throwable)(cleanup: => Unit): Unit =
    try cleanup
    catch {
      case cleanupProblem: Throwable if primary != null =>
        if (cleanupProblem ne primary) primary.addSuppressed(cleanupProblem)
      case cleanupProblem: Throwable => throw cleanupProblem
    }
}
