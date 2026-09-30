package at.ac.tuwien.dbai.yannakakis.integration

import java.sql.Connection

import scala.util.Using

/** Deterministic triangle data designed to expose set-versus-bag mistakes. */
object TriangleMultisetFixture {

  val ExpectedEnumerationMultiplicity: Long = 28L
  val ExpectedBadSupportMultiplicity: Long = 13L
  val ExpectedNullSafeMultiplicity: Long = 37L
  val ExpectedSelfEqualityMultiplicity: Long = 19L
  val ExpectedChainMultiplicity: Long = 17L

  /** Fully qualified, explicitly labelled input for generated rendering tests. */
  val QualifiedNativeEnumeration: String =
    """SELECT
      |  "r"."x" AS "x",
      |  "r"."y" AS "y",
      |  "s"."z" AS "z",
      |  "r"."payload" AS "r_payload",
      |  "s"."payload" AS "s_payload",
      |  "t"."payload" AS "t_payload"
      |FROM "pg_temp"."triangle_r" AS "r"
      |JOIN "pg_temp"."triangle_s" AS "s" ON "r"."y" = "s"."y"
      |JOIN "pg_temp"."triangle_t" AS "t"
      |  ON "s"."z" = "t"."z" AND "t"."x" = "r"."x"
      |""".stripMargin

  val QualifiedSelfEqualityEnumeration: String =
    QualifiedNativeEnumeration + " WHERE \"r\".\"a\" = \"r\".\"a\""

  /** Multi-bag acyclic control for generated semijoin and reconstruction SQL. */
  val QualifiedChainEnumeration: String =
    """SELECT
      |  "r"."a" AS "a",
      |  "r"."b" AS "b",
      |  "s"."c" AS "c",
      |  "t"."d" AS "d",
      |  "u"."e" AS "e",
      |  "r"."payload" AS "r_payload",
      |  "s"."payload" AS "s_payload",
      |  "t"."payload" AS "t_payload",
      |  "u"."payload" AS "u_payload"
      |FROM "pg_temp"."renderer_chain_r" AS "r"
      |JOIN "pg_temp"."renderer_chain_s" AS "s" ON "r"."b" = "s"."b"
      |JOIN "pg_temp"."renderer_chain_t" AS "t" ON "s"."c" = "t"."c"
      |JOIN "pg_temp"."renderer_chain_u" AS "u" ON "t"."d" = "u"."d"
      |""".stripMargin

  def install(connection: Connection): Unit =
    Using.resource(connection.createStatement()) { statement =>
      Vector(
        """CREATE TEMPORARY TABLE triangle_r (
          |  row_id INTEGER NOT NULL,
          |  x INTEGER,
          |  y INTEGER,
          |  a INTEGER,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE triangle_s (
          |  row_id INTEGER NOT NULL,
          |  y INTEGER,
          |  z INTEGER,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE triangle_t (
          |  row_id INTEGER NOT NULL,
          |  z INTEGER,
          |  x INTEGER,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE renderer_chain_r (
          |  row_id INTEGER NOT NULL,
          |  a INTEGER,
          |  b INTEGER,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE renderer_chain_s (
          |  row_id INTEGER NOT NULL,
          |  b INTEGER,
          |  c INTEGER,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE renderer_chain_t (
          |  row_id INTEGER NOT NULL,
          |  c INTEGER,
          |  d INTEGER,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE renderer_chain_u (
          |  row_id INTEGER NOT NULL,
          |  d INTEGER,
          |  e INTEGER,
          |  payload TEXT
          |)""".stripMargin,
        """INSERT INTO triangle_r (row_id, x, y, a, payload) VALUES
          |  (1, 10, 20, 1, 'r-a'),
          |  (1, 10, 20, 1, 'r-a'),
          |  (3, 10, 20, NULL, NULL),
          |  (4, 11, 21, 2, 'r-b'),
          |  (5, 12, 22, 3, 'r-dangling'),
          |  (6, NULL, 20, 4, 'r-null-x'),
          |  (7, 10, NULL, 5, 'r-null-y')""".stripMargin,
        """INSERT INTO triangle_s (row_id, y, z, payload) VALUES
          |  (1, 20, 30, 's-a'),
          |  (1, 20, 30, 's-a'),
          |  (3, 20, 30, NULL),
          |  (4, 21, 31, 's-b'),
          |  (5, 22, 32, 's-dangling'),
          |  (6, NULL, 30, 's-null-y'),
          |  (7, 20, NULL, 's-null-z')""".stripMargin,
        """INSERT INTO triangle_t (row_id, z, x, payload) VALUES
          |  (1, 30, 10, 't-a'),
          |  (1, 30, 10, 't-a'),
          |  (3, 30, 10, NULL),
          |  (4, 31, 11, 't-b'),
          |  (5, 99, 99, 't-dangling'),
          |  (6, NULL, 10, 't-null-z'),
          |  (7, 30, NULL, 't-null-x')""".stripMargin,
        """INSERT INTO renderer_chain_r (row_id, a, b, payload) VALUES
          |  (1, 10, 20, 'r-a'),
          |  (1, 10, 20, 'r-a'),
          |  (3, 11, 21, NULL),
          |  (4, 12, NULL, 'r-null-b'),
          |  (5, 13, 23, 'r-dangling')""".stripMargin,
        """INSERT INTO renderer_chain_s (row_id, b, c, payload) VALUES
          |  (1, 20, 30, 's-a'),
          |  (1, 20, 30, 's-a'),
          |  (3, 21, 31, NULL),
          |  (4, NULL, 32, 's-null-b'),
          |  (5, 23, NULL, 's-null-c')""".stripMargin,
        """INSERT INTO renderer_chain_t (row_id, c, d, payload) VALUES
          |  (1, 30, 40, 't-a'),
          |  (1, 30, 40, 't-a'),
          |  (3, 31, 41, NULL),
          |  (4, 32, 42, 't-unreachable'),
          |  (5, NULL, 43, 't-null-c')""".stripMargin,
        """INSERT INTO renderer_chain_u (row_id, d, e, payload) VALUES
          |  (1, 40, 50, 'u-a'),
          |  (1, 40, 50, 'u-a'),
          |  (3, 41, 51, NULL),
          |  (4, 42, 52, 'u-unreachable'),
          |  (5, NULL, 53, 'u-null-d')""".stripMargin
      ).foreach(statement.execute)
    }

  def nativeEnumeration(finalPredicate: String = "TRUE"): String =
    s"""SELECT
       |  r.x,
       |  r.y,
       |  s.z,
       |  r.payload AS r_payload,
       |  s.payload AS s_payload,
       |  t.payload AS t_payload
       |FROM triangle_r AS r
       |JOIN triangle_s AS s ON r.y = s.y
       |JOIN triangle_t AS t ON s.z = t.z AND t.x = r.x
       |WHERE ($finalPredicate)""".stripMargin

  def supportFilteredEnumeration(finalPredicate: String = "TRUE"): String =
    s"""WITH rs_support AS (
       |  SELECT DISTINCT r.x, r.y, s.z
       |  FROM triangle_r AS r
       |  JOIN triangle_s AS s ON r.y = s.y
       |),
       |live_support AS (
       |  SELECT support.x, support.y, support.z
       |  FROM rs_support AS support
       |  WHERE EXISTS (
       |    SELECT 1
       |    FROM triangle_t AS t
       |    WHERE t.z = support.z AND t.x = support.x
       |  )
       |)
       |SELECT
       |  r.x,
       |  r.y,
       |  s.z,
       |  r.payload AS r_payload,
       |  s.payload AS s_payload,
       |  t.payload AS t_payload
       |FROM triangle_r AS r
       |JOIN triangle_s AS s ON r.y = s.y
       |JOIN triangle_t AS t ON s.z = t.z AND t.x = r.x
       |WHERE EXISTS (
       |  SELECT 1
       |  FROM live_support AS support
       |  WHERE support.x = r.x
       |    AND support.y = r.y
       |    AND support.z = s.z
       |)
       |AND ($finalPredicate)""".stripMargin

  def nativeCount(finalPredicate: String = "TRUE"): String =
    s"""SELECT COUNT(*) AS result_count
       |FROM triangle_r AS r
       |JOIN triangle_s AS s ON r.y = s.y
       |JOIN triangle_t AS t ON s.z = t.z AND t.x = r.x
       |WHERE ($finalPredicate)""".stripMargin

  def supportFilteredCount(finalPredicate: String = "TRUE"): String =
    s"""WITH rs_support AS (
       |  SELECT DISTINCT r.x, r.y, s.z
       |  FROM triangle_r AS r
       |  JOIN triangle_s AS s ON r.y = s.y
       |),
       |live_support AS (
       |  SELECT support.x, support.y, support.z
       |  FROM rs_support AS support
       |  WHERE EXISTS (
       |    SELECT 1
       |    FROM triangle_t AS t
       |    WHERE t.z = support.z AND t.x = support.x
       |  )
       |)
       |SELECT COUNT(*) AS result_count
       |FROM triangle_r AS r
       |JOIN triangle_s AS s ON r.y = s.y
       |JOIN triangle_t AS t ON s.z = t.z AND t.x = r.x
       |WHERE EXISTS (
       |  SELECT 1
       |  FROM live_support AS support
       |  WHERE support.x = r.x
       |    AND support.y = r.y
       |    AND support.z = s.z
       |)
       |AND ($finalPredicate)""".stripMargin

  /** Incorrect because DISTINCT support rows replace the original R and S
    * occurrences in the final join, erasing their bag multiplicities.
    */
  val BadSupportAsFinalRelation: String =
    """WITH rs_support AS (
      |  SELECT DISTINCT
      |    r.x,
      |    r.y,
      |    s.z,
      |    r.payload AS r_payload,
      |    s.payload AS s_payload
      |  FROM triangle_r AS r
      |  JOIN triangle_s AS s ON r.y = s.y
      |)
      |SELECT
      |  support.x,
      |  support.y,
      |  support.z,
      |  support.r_payload,
      |  support.s_payload,
      |  t.payload AS t_payload
      |FROM rs_support AS support
      |JOIN triangle_t AS t
      |  ON t.z = support.z AND t.x = support.x""".stripMargin

  /** Incorrect because NULL-safe equality admits rows that SQL equi-joins must
    * reject when either join-key operand is NULL.
    */
  val BadNullSafeJoin: String =
    """SELECT
      |  r.x,
      |  r.y,
      |  s.z,
      |  r.payload AS r_payload,
      |  s.payload AS s_payload,
      |  t.payload AS t_payload
      |FROM triangle_r AS r
      |JOIN triangle_s AS s ON r.y IS NOT DISTINCT FROM s.y
      |JOIN triangle_t AS t
      |  ON s.z IS NOT DISTINCT FROM t.z
      |  AND t.x IS NOT DISTINCT FROM r.x""".stripMargin
}
