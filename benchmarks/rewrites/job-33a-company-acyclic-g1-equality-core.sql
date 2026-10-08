WITH
  "yr_f_b0_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b0_e0"."id" AS "yr_v0",
      "yr_f_b0_e7"."movie_id" AS "yr_v7"
    FROM "public"."company_name" AS "yr_f_b0_e0"
      CROSS JOIN "public"."movie_companies" AS "yr_f_b0_e7"
    WHERE
      "yr_f_b0_e0"."id" = "yr_f_b0_e0"."id"
      AND "yr_f_b0_e0"."id" = "yr_f_b0_e7"."company_id"
      AND "yr_f_b0_e7"."movie_id" = "yr_f_b0_e7"."movie_id"
  ),
  "yr_f_b1_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b1_e2"."id" AS "yr_v2",
      "yr_f_b1_e9"."movie_id" AS "yr_v7"
    FROM "public"."info_type" AS "yr_f_b1_e2"
      CROSS JOIN "public"."movie_info_idx" AS "yr_f_b1_e9"
    WHERE
      "yr_f_b1_e2"."id" = "yr_f_b1_e2"."id"
      AND "yr_f_b1_e2"."id" = "yr_f_b1_e9"."info_type_id"
      AND "yr_f_b1_e9"."movie_id" = "yr_f_b1_e9"."movie_id"
  ),
  "yr_f_b2_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b2_e4"."id" AS "yr_v4",
      "yr_f_b2_e12"."id" AS "yr_v7"
    FROM "public"."kind_type" AS "yr_f_b2_e4"
      CROSS JOIN "public"."title" AS "yr_f_b2_e12"
    WHERE
      "yr_f_b2_e4"."id" = "yr_f_b2_e4"."id"
      AND "yr_f_b2_e4"."id" = "yr_f_b2_e12"."kind_id"
      AND "yr_f_b2_e12"."id" = "yr_f_b2_e12"."id"
  ),
  "yr_f_b3_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b3_e6"."id" AS "yr_v6",
      "yr_f_b3_e11"."movie_id" AS "yr_v7",
      "yr_f_b3_e11"."linked_movie_id" AS "yr_v8"
    FROM "public"."link_type" AS "yr_f_b3_e6"
      CROSS JOIN "public"."movie_link" AS "yr_f_b3_e11"
    WHERE
      "yr_f_b3_e6"."id" = "yr_f_b3_e6"."id"
      AND "yr_f_b3_e6"."id" = "yr_f_b3_e11"."link_type_id"
      AND "yr_f_b3_e11"."movie_id" = "yr_f_b3_e11"."movie_id"
      AND "yr_f_b3_e11"."linked_movie_id" = "yr_f_b3_e11"."linked_movie_id"
  ),
  "yr_f_b4_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b4_e1"."id" AS "yr_v1",
      "yr_f_b4_e8"."movie_id" AS "yr_v8"
    FROM "public"."company_name" AS "yr_f_b4_e1"
      CROSS JOIN "public"."movie_companies" AS "yr_f_b4_e8"
    WHERE
      "yr_f_b4_e1"."id" = "yr_f_b4_e1"."id"
      AND "yr_f_b4_e1"."id" = "yr_f_b4_e8"."company_id"
      AND "yr_f_b4_e8"."movie_id" = "yr_f_b4_e8"."movie_id"
  ),
  "yr_f_b5_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b5_e3"."id" AS "yr_v3",
      "yr_f_b5_e10"."movie_id" AS "yr_v8"
    FROM "public"."info_type" AS "yr_f_b5_e3"
      CROSS JOIN "public"."movie_info_idx" AS "yr_f_b5_e10"
    WHERE
      "yr_f_b5_e3"."id" = "yr_f_b5_e3"."id"
      AND "yr_f_b5_e3"."id" = "yr_f_b5_e10"."info_type_id"
      AND "yr_f_b5_e10"."movie_id" = "yr_f_b5_e10"."movie_id"
  ),
  "yr_f_b6_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b6_e5"."id" AS "yr_v5",
      "yr_f_b6_e13"."id" AS "yr_v8"
    FROM "public"."kind_type" AS "yr_f_b6_e5"
      CROSS JOIN "public"."title" AS "yr_f_b6_e13"
    WHERE
      "yr_f_b6_e5"."id" = "yr_f_b6_e5"."id"
      AND "yr_f_b6_e5"."id" = "yr_f_b6_e13"."kind_id"
      AND "yr_f_b6_e13"."id" = "yr_f_b6_e13"."id"
  ),
  "yr_f_b7_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b7_e16"."movie_id" AS "yr_v8",
      "yr_f_b7_e16"."keyword_id" AS "yr_v10"
    FROM "public"."movie_keyword" AS "yr_f_b7_e16"
      CROSS JOIN "public"."keyword" AS "yr_f_b7_e17"
    WHERE
      "yr_f_b7_e16"."movie_id" = "yr_f_b7_e16"."movie_id"
      AND "yr_f_b7_e16"."keyword_id" = "yr_f_b7_e16"."keyword_id"
      AND "yr_f_b7_e16"."keyword_id" = "yr_f_b7_e17"."id"
  ),
  "yr_f_b8_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b8_e14"."movie_id" AS "yr_v7",
      "yr_f_b8_e14"."person_id" AS "yr_v9"
    FROM "public"."cast_info" AS "yr_f_b8_e14"
      CROSS JOIN "public"."name" AS "yr_f_b8_e15"
    WHERE
      "yr_f_b8_e14"."movie_id" = "yr_f_b8_e14"."movie_id"
      AND "yr_f_b8_e14"."person_id" = "yr_f_b8_e14"."person_id"
      AND "yr_f_b8_e14"."person_id" = "yr_f_b8_e15"."id"
  ),
"yr_gc_anchor" AS MATERIALIZED (SELECT r."yr_v7", r."yr_v8", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b3_i" AS r WHERE r."yr_v7" IS NOT NULL AND r."yr_v8" IS NOT NULL GROUP BY r."yr_v7", r."yr_v8"),
"yr_gc_count_0" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v7" FROM "yr_gc_anchor") AS d CROSS JOIN LATERAL (SELECT r."yr_v7", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b0_i" AS r WHERE r."yr_v7" = d."yr_v7" GROUP BY r."yr_v7") AS z),
"yr_gc_step_0" AS MATERIALIZED (SELECT a."yr_v7", a."yr_v8", a.yr_freq * b0.yr_freq AS yr_freq FROM "yr_gc_anchor" AS a JOIN "yr_gc_count_0" AS b0 ON a."yr_v7" = b0."yr_v7" WHERE b0.yr_freq IS NOT NULL),
"yr_gc_count_1" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v7" FROM "yr_gc_step_0") AS d CROSS JOIN LATERAL (SELECT r."yr_v7", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b1_i" AS r WHERE r."yr_v7" = d."yr_v7" GROUP BY r."yr_v7") AS z),
"yr_gc_step_1" AS MATERIALIZED (SELECT a."yr_v7", a."yr_v8", a.yr_freq * b1.yr_freq AS yr_freq FROM "yr_gc_step_0" AS a JOIN "yr_gc_count_1" AS b1 ON a."yr_v7" = b1."yr_v7" WHERE b1.yr_freq IS NOT NULL),
"yr_gc_count_2" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v7" FROM "yr_gc_step_1") AS d CROSS JOIN LATERAL (SELECT r."yr_v7", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b2_i" AS r WHERE r."yr_v7" = d."yr_v7" GROUP BY r."yr_v7") AS z),
"yr_gc_step_2" AS MATERIALIZED (SELECT a."yr_v7", a."yr_v8", a.yr_freq * b2.yr_freq AS yr_freq FROM "yr_gc_step_1" AS a JOIN "yr_gc_count_2" AS b2 ON a."yr_v7" = b2."yr_v7" WHERE b2.yr_freq IS NOT NULL),
"yr_gc_count_4" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v8" FROM "yr_gc_step_2") AS d CROSS JOIN LATERAL (SELECT r."yr_v8", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b4_i" AS r WHERE r."yr_v8" = d."yr_v8" GROUP BY r."yr_v8") AS z),
"yr_gc_step_4" AS MATERIALIZED (SELECT a."yr_v7", a."yr_v8", a.yr_freq * b4.yr_freq AS yr_freq FROM "yr_gc_step_2" AS a JOIN "yr_gc_count_4" AS b4 ON a."yr_v8" = b4."yr_v8" WHERE b4.yr_freq IS NOT NULL),
"yr_gc_count_5" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v8" FROM "yr_gc_step_4") AS d CROSS JOIN LATERAL (SELECT r."yr_v8", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b5_i" AS r WHERE r."yr_v8" = d."yr_v8" GROUP BY r."yr_v8") AS z),
"yr_gc_step_5" AS MATERIALIZED (SELECT a."yr_v7", a."yr_v8", a.yr_freq * b5.yr_freq AS yr_freq FROM "yr_gc_step_4" AS a JOIN "yr_gc_count_5" AS b5 ON a."yr_v8" = b5."yr_v8" WHERE b5.yr_freq IS NOT NULL),
"yr_gc_count_6" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v8" FROM "yr_gc_step_5") AS d CROSS JOIN LATERAL (SELECT r."yr_v8", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b6_i" AS r WHERE r."yr_v8" = d."yr_v8" GROUP BY r."yr_v8") AS z),
"yr_gc_step_6" AS MATERIALIZED (SELECT a."yr_v7", a."yr_v8", a.yr_freq * b6.yr_freq AS yr_freq FROM "yr_gc_step_5" AS a JOIN "yr_gc_count_6" AS b6 ON a."yr_v8" = b6."yr_v8" WHERE b6.yr_freq IS NOT NULL),
"yr_gc_count_7" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v8" FROM "yr_gc_step_6") AS d CROSS JOIN LATERAL (SELECT r."yr_v8", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b7_i" AS r WHERE r."yr_v8" = d."yr_v8" GROUP BY r."yr_v8") AS z),
"yr_gc_step_7" AS MATERIALIZED (SELECT a."yr_v7", a."yr_v8", a.yr_freq * b7.yr_freq AS yr_freq FROM "yr_gc_step_6" AS a JOIN "yr_gc_count_7" AS b7 ON a."yr_v8" = b7."yr_v8" WHERE b7.yr_freq IS NOT NULL),
"yr_gc_count_8" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v7" FROM "yr_gc_step_7") AS d CROSS JOIN LATERAL (SELECT r."yr_v7", r."yr_v9", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b8_i" AS r WHERE r."yr_v7" = d."yr_v7" GROUP BY r."yr_v7", r."yr_v9") AS z)
SELECT b8."yr_v9" AS "g1", CAST(SUM(a.yr_freq * b8.yr_freq) AS BIGINT) AS "witness_count" FROM "yr_gc_step_7" AS a JOIN "yr_gc_count_8" AS b8 ON a."yr_v7" = b8."yr_v7" GROUP BY b8."yr_v9";