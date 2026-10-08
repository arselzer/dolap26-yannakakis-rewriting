WITH
"yr_cc_0" AS MATERIALIZED (
    SELECT "yr_cc_e8"."movie_id" AS "yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq
    FROM "public"."movie_link" AS "yr_cc_e8"
      CROSS JOIN "public"."title" AS "yr_cc_e7"
      CROSS JOIN "public"."title" AS "yr_cc_e9"
      CROSS JOIN "public"."movie_keyword" AS "yr_cc_e10"
      CROSS JOIN "public"."movie_keyword" AS "yr_cc_e12"
      CROSS JOIN "public"."keyword" AS "yr_cc_e11"
      CROSS JOIN "public"."keyword" AS "yr_cc_e13"
    WHERE "yr_cc_e8"."movie_id" = "yr_cc_e8"."movie_id"
      AND "yr_cc_e8"."movie_id" = "yr_cc_e7"."id"
      AND "yr_cc_e8"."movie_id" = "yr_cc_e10"."movie_id"
      AND "yr_cc_e8"."linked_movie_id" = "yr_cc_e8"."linked_movie_id"
      AND "yr_cc_e8"."linked_movie_id" = "yr_cc_e9"."id"
      AND "yr_cc_e8"."linked_movie_id" = "yr_cc_e12"."movie_id"
      AND "yr_cc_e10"."keyword_id" = "yr_cc_e10"."keyword_id"
      AND "yr_cc_e10"."keyword_id" = "yr_cc_e12"."keyword_id"
      AND "yr_cc_e10"."keyword_id" = "yr_cc_e11"."id"
      AND "yr_cc_e10"."keyword_id" = "yr_cc_e13"."id"
    GROUP BY "yr_cc_e8"."movie_id"
  ),
"yr_cc_1" AS MATERIALIZED (
    SELECT "yr_cc_e0"."person_id" AS "yr_v0", "yr_cc_e2"."movie_id" AS "yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq
    FROM "public"."cast_info" AS "yr_cc_e2"
      CROSS JOIN "public"."aka_name" AS "yr_cc_e0"
      CROSS JOIN "public"."char_name" AS "yr_cc_e1"
      CROSS JOIN "public"."name" AS "yr_cc_e5"
      CROSS JOIN "public"."role_type" AS "yr_cc_e6"
    WHERE "yr_cc_e2"."person_id" = "yr_cc_e2"."person_id"
      AND "yr_cc_e2"."person_id" = "yr_cc_e0"."person_id"
      AND "yr_cc_e2"."person_id" = "yr_cc_e5"."id"
      AND "yr_cc_e2"."person_role_id" = "yr_cc_e2"."person_role_id"
      AND "yr_cc_e2"."person_role_id" = "yr_cc_e1"."id"
      AND "yr_cc_e2"."movie_id" = "yr_cc_e2"."movie_id"
      AND "yr_cc_e2"."role_id" = "yr_cc_e2"."role_id"
      AND "yr_cc_e2"."role_id" = "yr_cc_e6"."id"
      AND EXISTS (SELECT 1 FROM "yr_cc_0" AS yr_cc_domain WHERE "yr_cc_e2"."movie_id" = "yr_cc_domain"."yr_v2")
    GROUP BY "yr_cc_e0"."person_id", "yr_cc_e2"."movie_id"
  ),
"yr_cc_2" AS MATERIALIZED (
    SELECT "yr_cc_e4"."movie_id" AS "yr_v2", "yr_cc_e3"."id" AS "yr_v4", SUM(CAST(1 AS NUMERIC)) AS yr_freq
    FROM "public"."movie_companies" AS "yr_cc_e4"
      CROSS JOIN "public"."company_name" AS "yr_cc_e3"
    WHERE "yr_cc_e4"."movie_id" = "yr_cc_e4"."movie_id"
      AND "yr_cc_e4"."company_id" = "yr_cc_e4"."company_id"
      AND "yr_cc_e4"."company_id" = "yr_cc_e3"."id"
      AND EXISTS (SELECT 1 FROM "yr_cc_0" AS yr_cc_domain WHERE "yr_cc_e4"."movie_id" = "yr_cc_domain"."yr_v2")
    GROUP BY "yr_cc_e4"."movie_id", "yr_cc_e3"."id"
  )
SELECT p1."yr_v0" AS "g1", p2."yr_v4" AS "g2", CAST(SUM(p0.yr_freq * p1.yr_freq * p2.yr_freq) AS BIGINT) AS "witness_count" FROM "yr_cc_0" AS p0 JOIN "yr_cc_1" AS p1 ON p0."yr_v2" = p1."yr_v2" JOIN "yr_cc_2" AS p2 ON p0."yr_v2" = p2."yr_v2" GROUP BY p1."yr_v0", p2."yr_v4";