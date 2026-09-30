package at.ac.tuwien.dbai.yannakakis.integration

import java.sql.Connection

import scala.util.Using

/**
  * Small, independently authored enumeration fixtures whose join topologies
  * are adapted from two benchmark queries. These statements are correctness
  * inputs, not copies of either benchmark query and not performance workloads.
  */
private[integration] object BenchmarkDerivedMultisetFixture {

  val LsqbQ2ExpectedMultiplicity: Long = 9L
  val LsqbQ3DerivedExpectedMultiplicity: Long = 24L
  val TpchQ5DerivedExpectedMultiplicity: Long = 65L

  val LsqbQ2Tables: Vector[String] = Vector(
    "lsqb_q2_knows",
    "lsqb_q2_comment",
    "lsqb_q2_post"
  )

  val TpchQ5DerivedTables: Vector[String] = Vector(
    "q5d_customer",
    "q5d_orders",
    "q5d_lineitem",
    "q5d_supplier",
    "q5d_nation",
    "q5d_region"
  )

  val LsqbQ3DerivedTables: Vector[String] = Vector(
    "lsqb_q3_city",
    "lsqb_q3_person",
    "lsqb_q3_knows"
  )

  /**
    * Adapted from the LSQB Q2 triangle topology. Unlike the official query,
    * this fixture enumerates witnesses and deliberately retains duplicates.
  */
  val LsqbQ2EnumerationCore: String =
    """/* lsqb-q2-enum-core: independently authored, adapted topology. */
      |SELECT
      |  "k"."person1_id" AS "person1_id",
      |  "k"."person2_id" AS "person2_id",
      |  "c"."comment_id" AS "comment_id",
      |  "p"."post_id" AS "post_id",
      |  "k"."payload" AS "knows_payload",
      |  "c"."payload" AS "comment_payload",
      |  "p"."payload" AS "post_payload"
      |FROM "pg_temp"."lsqb_q2_knows" AS "k"
      |JOIN "pg_temp"."lsqb_q2_comment" AS "c"
      |  ON "k"."person1_id" = "c"."creator_id"
      |JOIN "pg_temp"."lsqb_q2_post" AS "p"
      |  ON "c"."reply_post_id" = "p"."post_id"
      | AND "p"."creator_id" = "k"."person2_id"
      |""".stripMargin

  /**
    * Independently authored nine-occurrence fixture adapted from the LSQB Q3
    * topology. Every source alias contributes projected identity and payload
    * columns. This is not the aggregate query distributed by LSQB.
    */
  val LsqbQ3DerivedEnumerationCore: String =
    """/* lsqb-q3-derived-enum-core: independently authored topology fixture. */
      |SELECT
      |  "city_a"."row_id" AS "city_a_row_id",
      |  "city_a"."city_id" AS "city_a_id",
      |  "city_a"."payload" AS "city_a_payload",
      |  "city_b"."row_id" AS "city_b_row_id",
      |  "city_b"."city_id" AS "city_b_id",
      |  "city_b"."payload" AS "city_b_payload",
      |  "city_c"."row_id" AS "city_c_row_id",
      |  "city_c"."city_id" AS "city_c_id",
      |  "city_c"."payload" AS "city_c_payload",
      |  "person_a"."row_id" AS "person_a_row_id",
      |  "person_a"."person_id" AS "person_a_id",
      |  "person_a"."payload" AS "person_a_payload",
      |  "person_b"."row_id" AS "person_b_row_id",
      |  "person_b"."person_id" AS "person_b_id",
      |  "person_b"."payload" AS "person_b_payload",
      |  "person_c"."row_id" AS "person_c_row_id",
      |  "person_c"."person_id" AS "person_c_id",
      |  "person_c"."payload" AS "person_c_payload",
      |  "knows_ab"."row_id" AS "knows_ab_row_id",
      |  "knows_ab"."payload" AS "knows_ab_payload",
      |  "knows_bc"."row_id" AS "knows_bc_row_id",
      |  "knows_bc"."payload" AS "knows_bc_payload",
      |  "knows_ca"."row_id" AS "knows_ca_row_id",
      |  "knows_ca"."payload" AS "knows_ca_payload"
      |FROM "pg_temp"."lsqb_q3_city" AS "city_a"
      |JOIN "pg_temp"."lsqb_q3_city" AS "city_b"
      |  ON "city_a"."country_id" = "city_b"."country_id"
      |JOIN "pg_temp"."lsqb_q3_city" AS "city_c"
      |  ON "city_b"."country_id" = "city_c"."country_id"
      |JOIN "pg_temp"."lsqb_q3_person" AS "person_a"
      |  ON "city_a"."city_id" = "person_a"."city_id"
      |JOIN "pg_temp"."lsqb_q3_person" AS "person_b"
      |  ON "city_b"."city_id" = "person_b"."city_id"
      |JOIN "pg_temp"."lsqb_q3_person" AS "person_c"
      |  ON "city_c"."city_id" = "person_c"."city_id"
      |JOIN "pg_temp"."lsqb_q3_knows" AS "knows_ab"
      |  ON "person_a"."person_id" = "knows_ab"."person1_id"
      | AND "person_b"."person_id" = "knows_ab"."person2_id"
      |JOIN "pg_temp"."lsqb_q3_knows" AS "knows_bc"
      |  ON "person_b"."person_id" = "knows_bc"."person1_id"
      | AND "person_c"."person_id" = "knows_bc"."person2_id"
      |JOIN "pg_temp"."lsqb_q3_knows" AS "knows_ca"
      |  ON "person_c"."person_id" = "knows_ca"."person1_id"
      | AND "person_a"."person_id" = "knows_ca"."person2_id"
      |""".stripMargin

  /**
    * Derived from the TPC-H Q5 join topology, with all filters, arithmetic,
    * aggregation, and official projections intentionally removed. Results
    * from this fixture are not comparable to published TPC-H measurements.
  */
  val TpchQ5DerivedEnumerationCore: String =
    """/* tpch-q5-derived-enum-core: independently authored, non-comparable. */
      |SELECT
      |  "c"."customer_id" AS "customer_id",
      |  "o"."order_id" AS "order_id",
      |  "l"."supplier_id" AS "supplier_id",
      |  "c"."nation_id" AS "nation_id",
      |  "n"."region_id" AS "region_id",
      |  "c"."payload" AS "customer_payload",
      |  "o"."payload" AS "orders_payload",
      |  "l"."payload" AS "lineitem_payload",
      |  "s"."payload" AS "supplier_payload",
      |  "n"."payload" AS "nation_payload",
      |  "r"."payload" AS "region_payload"
      |FROM "pg_temp"."q5d_customer" AS "c"
      |JOIN "pg_temp"."q5d_orders" AS "o"
      |  ON "c"."customer_id" = "o"."customer_id"
      |JOIN "pg_temp"."q5d_lineitem" AS "l"
      |  ON "o"."order_id" = "l"."order_id"
      |JOIN "pg_temp"."q5d_supplier" AS "s"
      |  ON "l"."supplier_id" = "s"."supplier_id"
      | AND "s"."nation_id" = "c"."nation_id"
      |JOIN "pg_temp"."q5d_nation" AS "n"
      |  ON "s"."nation_id" = "n"."nation_id"
      |JOIN "pg_temp"."q5d_region" AS "r"
      |  ON "n"."region_id" = "r"."region_id"
      |""".stripMargin

  def install(connection: Connection): Unit =
    Using.resource(connection.createStatement()) { statement =>
      Vector(
        """CREATE TEMPORARY TABLE lsqb_q2_knows (
          |  edge_id BIGINT NOT NULL,
          |  person1_id BIGINT,
          |  person2_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE lsqb_q2_comment (
          |  comment_id BIGINT NOT NULL,
          |  creator_id BIGINT,
          |  reply_post_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE lsqb_q2_post (
          |  post_id BIGINT,
          |  creator_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE lsqb_q3_city (
          |  row_id BIGINT NOT NULL,
          |  city_id BIGINT,
          |  country_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE lsqb_q3_person (
          |  row_id BIGINT NOT NULL,
          |  person_id BIGINT,
          |  city_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE lsqb_q3_knows (
          |  row_id BIGINT NOT NULL,
          |  person1_id BIGINT,
          |  person2_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE q5d_customer (
          |  row_id BIGINT NOT NULL,
          |  customer_id BIGINT,
          |  nation_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE q5d_orders (
          |  row_id BIGINT NOT NULL,
          |  customer_id BIGINT,
          |  order_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE q5d_lineitem (
          |  row_id BIGINT NOT NULL,
          |  order_id BIGINT,
          |  supplier_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE q5d_supplier (
          |  row_id BIGINT NOT NULL,
          |  supplier_id BIGINT,
          |  nation_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE q5d_nation (
          |  row_id BIGINT NOT NULL,
          |  nation_id BIGINT,
          |  region_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """CREATE TEMPORARY TABLE q5d_region (
          |  row_id BIGINT NOT NULL,
          |  region_id BIGINT,
          |  payload TEXT
          |)""".stripMargin,
        """INSERT INTO lsqb_q2_knows
          |  (edge_id, person1_id, person2_id, payload) VALUES
          |  (1, 100, 200, 'knows-a'),
          |  (1, 100, 200, 'knows-a'),
          |  (3, 101, 201, NULL),
          |  (4, NULL, 200, 'knows-null-person1'),
          |  (5, 100, NULL, 'knows-null-person2'),
          |  (6, 102, 202, 'knows-dangling')""".stripMargin,
        """INSERT INTO lsqb_q2_comment
          |  (comment_id, creator_id, reply_post_id, payload) VALUES
          |  (1, 100, 300, 'comment-a'),
          |  (1, 100, 300, 'comment-a'),
          |  (3, 101, 301, NULL),
          |  (4, NULL, 300, 'comment-null-creator'),
          |  (5, 100, NULL, 'comment-null-post'),
          |  (6, 102, 302, 'comment-dangling')""".stripMargin,
        """INSERT INTO lsqb_q2_post (post_id, creator_id, payload) VALUES
          |  (300, 200, 'post-a'),
          |  (300, 200, 'post-a'),
          |  (301, 201, NULL),
          |  (NULL, 200, 'post-null-id'),
          |  (300, NULL, 'post-null-creator'),
          |  (999, 202, 'post-dangling')""".stripMargin,
        """INSERT INTO lsqb_q3_city
          |  (row_id, city_id, country_id, payload) VALUES
          |  (1, 10, 1, 'city-a'),
          |  (1, 10, 1, 'city-a'),
          |  (3, 11, 1, NULL),
          |  (4, 12, 1, 'city-c'),
          |  (5, 13, 2, 'city-other-country'),
          |  (6, NULL, 1, 'city-null-id'),
          |  (7, 14, NULL, 'city-null-country')""".stripMargin,
        """INSERT INTO lsqb_q3_person
          |  (row_id, person_id, city_id, payload) VALUES
          |  (1, 100, 10, 'person-a'),
          |  (1, 100, 10, 'person-a'),
          |  (3, 101, 11, NULL),
          |  (4, 102, 12, 'person-c'),
          |  (5, 103, 13, 'person-other-country'),
          |  (6, 104, NULL, 'person-null-city'),
          |  (7, NULL, 10, 'person-null-id')""".stripMargin,
        """INSERT INTO lsqb_q3_knows
          |  (row_id, person1_id, person2_id, payload) VALUES
          |  (1, 100, 101, 'knows-ab'),
          |  (1, 100, 101, 'knows-ab'),
          |  (3, 101, 102, NULL),
          |  (4, 102, 100, 'knows-ca'),
          |  (5, 103, 999, 'knows-dangling'),
          |  (6, NULL, 101, 'knows-null-left'),
          |  (7, 100, NULL, 'knows-null-right')""".stripMargin,
        """INSERT INTO q5d_customer
          |  (row_id, customer_id, nation_id, payload) VALUES
          |  (1, 100, 10, 'customer-a'),
          |  (1, 100, 10, 'customer-a'),
          |  (3, 101, 11, NULL),
          |  (4, NULL, 10, 'customer-null-key'),
          |  (5, 102, 12, 'customer-dangling')""".stripMargin,
        """INSERT INTO q5d_orders
          |  (row_id, customer_id, order_id, payload) VALUES
          |  (1, 100, 1000, 'orders-a'),
          |  (1, 100, 1000, 'orders-a'),
          |  (3, 101, 1001, NULL),
          |  (4, NULL, 1000, 'orders-null-key'),
          |  (5, 999, 1002, 'orders-dangling')""".stripMargin,
        """INSERT INTO q5d_lineitem
          |  (row_id, order_id, supplier_id, payload) VALUES
          |  (1, 1000, 500, 'lineitem-a'),
          |  (1, 1000, 500, 'lineitem-a'),
          |  (3, 1001, 501, NULL),
          |  (4, 1000, NULL, 'lineitem-null-key'),
          |  (5, 1003, 502, 'lineitem-dangling')""".stripMargin,
        """INSERT INTO q5d_supplier
          |  (row_id, supplier_id, nation_id, payload) VALUES
          |  (1, 500, 10, 'supplier-a'),
          |  (1, 500, 10, 'supplier-a'),
          |  (3, 501, 11, NULL),
          |  (4, NULL, 10, 'supplier-null-key'),
          |  (5, 503, 13, 'supplier-dangling')""".stripMargin,
        """INSERT INTO q5d_nation
          |  (row_id, nation_id, region_id, payload) VALUES
          |  (1, 10, 1, 'nation-a'),
          |  (1, 10, 1, 'nation-a'),
          |  (3, 11, 2, NULL),
          |  (4, 10, NULL, 'nation-null-key'),
          |  (5, 14, 3, 'nation-dangling')""".stripMargin,
        """INSERT INTO q5d_region
          |  (row_id, region_id, payload) VALUES
          |  (1, 1, 'region-a'),
          |  (1, 1, 'region-a'),
          |  (3, 2, NULL),
          |  (4, NULL, 'region-null-key'),
          |  (5, 4, 'region-dangling')""".stripMargin
      ).foreach(statement.execute)
    }
}
