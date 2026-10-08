WITH
  "yr_f_b0_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b0_e0"."person_id" AS "yr_v0",
      "yr_f_b0_e0"."movie_id" AS "yr_v1"
    FROM "public"."cast_info" AS "yr_f_b0_e0"
      CROSS JOIN "public"."name" AS "yr_f_b0_e9"
      CROSS JOIN "public"."title" AS "yr_f_b0_e10"
    WHERE
      "yr_f_b0_e0"."person_id" = "yr_f_b0_e0"."person_id"
      AND "yr_f_b0_e0"."person_id" = "yr_f_b0_e9"."id"
      AND "yr_f_b0_e0"."movie_id" = "yr_f_b0_e0"."movie_id"
      AND "yr_f_b0_e0"."movie_id" = "yr_f_b0_e10"."id"
  ),
  "yr_f_b1_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b1_e5"."movie_id" AS "yr_v1",
      "yr_f_b1_e1"."id" AS "yr_v2"
    FROM "public"."company_name" AS "yr_f_b1_e1"
      CROSS JOIN "public"."movie_companies" AS "yr_f_b1_e5"
    WHERE
      "yr_f_b1_e5"."movie_id" = "yr_f_b1_e5"."movie_id"
      AND "yr_f_b1_e1"."id" = "yr_f_b1_e1"."id"
      AND "yr_f_b1_e1"."id" = "yr_f_b1_e5"."company_id"
  ),
  "yr_f_b2_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b2_e6"."movie_id" AS "yr_v1",
      "yr_f_b2_e2"."id" AS "yr_v3"
    FROM "public"."info_type" AS "yr_f_b2_e2"
      CROSS JOIN "public"."movie_info" AS "yr_f_b2_e6"
    WHERE
      "yr_f_b2_e6"."movie_id" = "yr_f_b2_e6"."movie_id"
      AND "yr_f_b2_e2"."id" = "yr_f_b2_e2"."id"
      AND "yr_f_b2_e2"."id" = "yr_f_b2_e6"."info_type_id"
  ),
  "yr_f_b3_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b3_e7"."movie_id" AS "yr_v1",
      "yr_f_b3_e3"."id" AS "yr_v4"
    FROM "public"."info_type" AS "yr_f_b3_e3"
      CROSS JOIN "public"."movie_info_idx" AS "yr_f_b3_e7"
    WHERE
      "yr_f_b3_e7"."movie_id" = "yr_f_b3_e7"."movie_id"
      AND "yr_f_b3_e3"."id" = "yr_f_b3_e3"."id"
      AND "yr_f_b3_e3"."id" = "yr_f_b3_e7"."info_type_id"
  ),
  "yr_f_b4_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b4_e8"."movie_id" AS "yr_v1",
      "yr_f_b4_e4"."id" AS "yr_v5"
    FROM "public"."keyword" AS "yr_f_b4_e4"
      CROSS JOIN "public"."movie_keyword" AS "yr_f_b4_e8"
    WHERE
      "yr_f_b4_e8"."movie_id" = "yr_f_b4_e8"."movie_id"
      AND "yr_f_b4_e4"."id" = "yr_f_b4_e4"."id"
      AND "yr_f_b4_e4"."id" = "yr_f_b4_e8"."keyword_id"
  ),
  "yr_f_b5_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b5_e11"."movie_id" AS "yr_v1",
      "yr_f_b5_e11"."linked_movie_id" AS "yr_v6"
    FROM "public"."movie_link" AS "yr_f_b5_e11"
      CROSS JOIN "public"."title" AS "yr_f_b5_e12"
    WHERE
      "yr_f_b5_e11"."movie_id" = "yr_f_b5_e11"."movie_id"
      AND "yr_f_b5_e11"."linked_movie_id" = "yr_f_b5_e11"."linked_movie_id"
      AND "yr_f_b5_e11"."linked_movie_id" = "yr_f_b5_e12"."id"
  ),
  "yr_f_b6_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b6_e15"."movie_id" AS "yr_v6",
      "yr_f_b6_e15"."keyword_id" AS "yr_v8"
    FROM "public"."movie_keyword" AS "yr_f_b6_e15"
      CROSS JOIN "public"."keyword" AS "yr_f_b6_e16"
    WHERE
      "yr_f_b6_e15"."movie_id" = "yr_f_b6_e15"."movie_id"
      AND "yr_f_b6_e15"."keyword_id" = "yr_f_b6_e15"."keyword_id"
      AND "yr_f_b6_e15"."keyword_id" = "yr_f_b6_e16"."id"
  ),
  "yr_f_b7_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b7_e13"."movie_id" AS "yr_v1",
      "yr_f_b7_e13"."keyword_id" AS "yr_v7"
    FROM "public"."movie_keyword" AS "yr_f_b7_e13"
      CROSS JOIN "public"."keyword" AS "yr_f_b7_e14"
    WHERE
      "yr_f_b7_e13"."movie_id" = "yr_f_b7_e13"."movie_id"
      AND "yr_f_b7_e13"."keyword_id" = "yr_f_b7_e13"."keyword_id"
      AND "yr_f_b7_e13"."keyword_id" = "yr_f_b7_e14"."id"
  ),
"yr_gc_anchor" AS MATERIALIZED (SELECT r."yr_v1", r."yr_v6", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b5_i" AS r WHERE r."yr_v1" IS NOT NULL AND r."yr_v6" IS NOT NULL GROUP BY r."yr_v1", r."yr_v6"),
"yr_gc_count_2" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v1" FROM "yr_gc_anchor") AS d CROSS JOIN LATERAL (SELECT r."yr_v1", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b2_i" AS r WHERE r."yr_v1" = d."yr_v1" GROUP BY r."yr_v1") AS z),
"yr_gc_step_2" AS MATERIALIZED (SELECT a."yr_v1", a."yr_v6", a.yr_freq * b2.yr_freq AS yr_freq FROM "yr_gc_anchor" AS a JOIN "yr_gc_count_2" AS b2 ON a."yr_v1" = b2."yr_v1" WHERE b2.yr_freq IS NOT NULL),
"yr_gc_count_3" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v1" FROM "yr_gc_step_2") AS d CROSS JOIN LATERAL (SELECT r."yr_v1", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b3_i" AS r WHERE r."yr_v1" = d."yr_v1" GROUP BY r."yr_v1") AS z),
"yr_gc_step_3" AS MATERIALIZED (SELECT a."yr_v1", a."yr_v6", a.yr_freq * b3.yr_freq AS yr_freq FROM "yr_gc_step_2" AS a JOIN "yr_gc_count_3" AS b3 ON a."yr_v1" = b3."yr_v1" WHERE b3.yr_freq IS NOT NULL),
"yr_gc_count_4" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v1" FROM "yr_gc_step_3") AS d CROSS JOIN LATERAL (SELECT r."yr_v1", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b4_i" AS r WHERE r."yr_v1" = d."yr_v1" GROUP BY r."yr_v1") AS z),
"yr_gc_step_4" AS MATERIALIZED (SELECT a."yr_v1", a."yr_v6", a.yr_freq * b4.yr_freq AS yr_freq FROM "yr_gc_step_3" AS a JOIN "yr_gc_count_4" AS b4 ON a."yr_v1" = b4."yr_v1" WHERE b4.yr_freq IS NOT NULL),
"yr_gc_count_6" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v6" FROM "yr_gc_step_4") AS d CROSS JOIN LATERAL (SELECT r."yr_v6", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b6_i" AS r WHERE r."yr_v6" = d."yr_v6" GROUP BY r."yr_v6") AS z),
"yr_gc_step_6" AS MATERIALIZED (SELECT a."yr_v1", a."yr_v6", a.yr_freq * b6.yr_freq AS yr_freq FROM "yr_gc_step_4" AS a JOIN "yr_gc_count_6" AS b6 ON a."yr_v6" = b6."yr_v6" WHERE b6.yr_freq IS NOT NULL),
"yr_gc_count_7" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v1" FROM "yr_gc_step_6") AS d CROSS JOIN LATERAL (SELECT r."yr_v1", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b7_i" AS r WHERE r."yr_v1" = d."yr_v1" GROUP BY r."yr_v1") AS z),
"yr_gc_step_7" AS MATERIALIZED (SELECT a."yr_v1", a."yr_v6", a.yr_freq * b7.yr_freq AS yr_freq FROM "yr_gc_step_6" AS a JOIN "yr_gc_count_7" AS b7 ON a."yr_v1" = b7."yr_v1" WHERE b7.yr_freq IS NOT NULL),
"yr_gc_count_0" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v1" FROM "yr_gc_step_7") AS d CROSS JOIN LATERAL (SELECT r."yr_v0", r."yr_v1", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b0_i" AS r WHERE r."yr_v1" = d."yr_v1" GROUP BY r."yr_v0", r."yr_v1") AS z),
"yr_gc_count_1" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v1" FROM "yr_gc_step_7") AS d CROSS JOIN LATERAL (SELECT r."yr_v1", r."yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b1_i" AS r WHERE r."yr_v1" = d."yr_v1" GROUP BY r."yr_v1", r."yr_v2") AS z)
SELECT b0."yr_v0" AS "g1", b1."yr_v2" AS "g2", CAST(SUM(a.yr_freq * b0.yr_freq * b1.yr_freq) AS BIGINT) AS "witness_count" FROM "yr_gc_step_7" AS a JOIN "yr_gc_count_0" AS b0 ON a."yr_v1" = b0."yr_v1" JOIN "yr_gc_count_1" AS b1 ON a."yr_v1" = b1."yr_v1" GROUP BY b0."yr_v0", b1."yr_v2";