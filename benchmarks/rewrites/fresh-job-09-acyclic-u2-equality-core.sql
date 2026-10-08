WITH
  "yr_f_b0_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b0_e0"."person_id" AS "yr_v0",
      "yr_f_b0_e1"."id" AS "yr_v1",
      "yr_f_b0_e2"."movie_id" AS "yr_v2",
      "yr_f_b0_e2"."role_id" AS "yr_v3"
    FROM "public"."aka_name" AS "yr_f_b0_e0"
      CROSS JOIN "public"."char_name" AS "yr_f_b0_e1"
      CROSS JOIN "public"."cast_info" AS "yr_f_b0_e2"
      CROSS JOIN "public"."name" AS "yr_f_b0_e5"
      CROSS JOIN "public"."role_type" AS "yr_f_b0_e6"
      CROSS JOIN "public"."title" AS "yr_f_b0_e7"
    WHERE
      "yr_f_b0_e0"."person_id" = "yr_f_b0_e0"."person_id"
      AND "yr_f_b0_e0"."person_id" = "yr_f_b0_e2"."person_id"
      AND "yr_f_b0_e0"."person_id" = "yr_f_b0_e5"."id"
      AND "yr_f_b0_e1"."id" = "yr_f_b0_e1"."id"
      AND "yr_f_b0_e1"."id" = "yr_f_b0_e2"."person_role_id"
      AND "yr_f_b0_e2"."movie_id" = "yr_f_b0_e2"."movie_id"
      AND "yr_f_b0_e2"."movie_id" = "yr_f_b0_e7"."id"
      AND "yr_f_b0_e2"."role_id" = "yr_f_b0_e2"."role_id"
      AND "yr_f_b0_e2"."role_id" = "yr_f_b0_e6"."id"
  ),
  "yr_f_b1_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b1_e4"."movie_id" AS "yr_v2",
      "yr_f_b1_e3"."id" AS "yr_v4"
    FROM "public"."company_name" AS "yr_f_b1_e3"
      CROSS JOIN "public"."movie_companies" AS "yr_f_b1_e4"
    WHERE
      "yr_f_b1_e4"."movie_id" = "yr_f_b1_e4"."movie_id"
      AND "yr_f_b1_e3"."id" = "yr_f_b1_e3"."id"
      AND "yr_f_b1_e3"."id" = "yr_f_b1_e4"."company_id"
  ),
  "yr_f_b2_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b2_e8"."movie_id" AS "yr_v2",
      "yr_f_b2_e8"."linked_movie_id" AS "yr_v5"
    FROM "public"."movie_link" AS "yr_f_b2_e8"
      CROSS JOIN "public"."title" AS "yr_f_b2_e9"
    WHERE
      "yr_f_b2_e8"."movie_id" = "yr_f_b2_e8"."movie_id"
      AND "yr_f_b2_e8"."linked_movie_id" = "yr_f_b2_e8"."linked_movie_id"
      AND "yr_f_b2_e8"."linked_movie_id" = "yr_f_b2_e9"."id"
  ),
  "yr_f_b3_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b3_e12"."movie_id" AS "yr_v5",
      "yr_f_b3_e12"."keyword_id" AS "yr_v7"
    FROM "public"."movie_keyword" AS "yr_f_b3_e12"
      CROSS JOIN "public"."keyword" AS "yr_f_b3_e13"
    WHERE
      "yr_f_b3_e12"."movie_id" = "yr_f_b3_e12"."movie_id"
      AND "yr_f_b3_e12"."keyword_id" = "yr_f_b3_e12"."keyword_id"
      AND "yr_f_b3_e12"."keyword_id" = "yr_f_b3_e13"."id"
  ),
  "yr_f_b4_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b4_e10"."movie_id" AS "yr_v2",
      "yr_f_b4_e10"."keyword_id" AS "yr_v6"
    FROM "public"."movie_keyword" AS "yr_f_b4_e10"
      CROSS JOIN "public"."keyword" AS "yr_f_b4_e11"
    WHERE
      "yr_f_b4_e10"."movie_id" = "yr_f_b4_e10"."movie_id"
      AND "yr_f_b4_e10"."keyword_id" = "yr_f_b4_e10"."keyword_id"
      AND "yr_f_b4_e10"."keyword_id" = "yr_f_b4_e11"."id"
  ),
"yr_gc_anchor" AS MATERIALIZED (SELECT r."yr_v2", r."yr_v5", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b2_i" AS r WHERE r."yr_v2" IS NOT NULL AND r."yr_v5" IS NOT NULL GROUP BY r."yr_v2", r."yr_v5"),
"yr_gc_count_3" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v5" FROM "yr_gc_anchor") AS d CROSS JOIN LATERAL (SELECT r."yr_v5", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b3_i" AS r WHERE r."yr_v5" = d."yr_v5" GROUP BY r."yr_v5") AS z),
"yr_gc_step_3" AS MATERIALIZED (SELECT a."yr_v2", a."yr_v5", a.yr_freq * b3.yr_freq AS yr_freq FROM "yr_gc_anchor" AS a JOIN "yr_gc_count_3" AS b3 ON a."yr_v5" = b3."yr_v5" WHERE b3.yr_freq IS NOT NULL),
"yr_gc_count_4" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v2" FROM "yr_gc_step_3") AS d CROSS JOIN LATERAL (SELECT r."yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b4_i" AS r WHERE r."yr_v2" = d."yr_v2" GROUP BY r."yr_v2") AS z),
"yr_gc_step_4" AS MATERIALIZED (SELECT a."yr_v2", a."yr_v5", a.yr_freq * b4.yr_freq AS yr_freq FROM "yr_gc_step_3" AS a JOIN "yr_gc_count_4" AS b4 ON a."yr_v2" = b4."yr_v2" WHERE b4.yr_freq IS NOT NULL),
"yr_gc_count_0" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v2" FROM "yr_gc_step_4") AS d CROSS JOIN LATERAL (SELECT r."yr_v0", r."yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b0_i" AS r WHERE r."yr_v2" = d."yr_v2" GROUP BY r."yr_v0", r."yr_v2") AS z),
"yr_gc_count_1" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v2" FROM "yr_gc_step_4") AS d CROSS JOIN LATERAL (SELECT r."yr_v2", r."yr_v4", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b1_i" AS r WHERE r."yr_v2" = d."yr_v2" GROUP BY r."yr_v2", r."yr_v4") AS z)
SELECT b0."yr_v0" AS "g1", b1."yr_v4" AS "g2", CAST(SUM(a.yr_freq * b0.yr_freq * b1.yr_freq) AS BIGINT) AS "witness_count" FROM "yr_gc_step_4" AS a JOIN "yr_gc_count_0" AS b0 ON a."yr_v2" = b0."yr_v2" JOIN "yr_gc_count_1" AS b1 ON a."yr_v2" = b1."yr_v2" GROUP BY b0."yr_v0", b1."yr_v4";