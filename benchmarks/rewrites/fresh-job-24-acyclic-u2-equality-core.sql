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
      CROSS JOIN "public"."name" AS "yr_f_b0_e9"
      CROSS JOIN "public"."role_type" AS "yr_f_b0_e10"
      CROSS JOIN "public"."title" AS "yr_f_b0_e11"
    WHERE
      "yr_f_b0_e0"."person_id" = "yr_f_b0_e0"."person_id"
      AND "yr_f_b0_e0"."person_id" = "yr_f_b0_e2"."person_id"
      AND "yr_f_b0_e0"."person_id" = "yr_f_b0_e9"."id"
      AND "yr_f_b0_e1"."id" = "yr_f_b0_e1"."id"
      AND "yr_f_b0_e1"."id" = "yr_f_b0_e2"."person_role_id"
      AND "yr_f_b0_e2"."movie_id" = "yr_f_b0_e2"."movie_id"
      AND "yr_f_b0_e2"."movie_id" = "yr_f_b0_e11"."id"
      AND "yr_f_b0_e2"."role_id" = "yr_f_b0_e2"."role_id"
      AND "yr_f_b0_e2"."role_id" = "yr_f_b0_e10"."id"
  ),
  "yr_f_b1_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b1_e6"."movie_id" AS "yr_v2",
      "yr_f_b1_e3"."id" AS "yr_v4"
    FROM "public"."company_name" AS "yr_f_b1_e3"
      CROSS JOIN "public"."movie_companies" AS "yr_f_b1_e6"
    WHERE
      "yr_f_b1_e6"."movie_id" = "yr_f_b1_e6"."movie_id"
      AND "yr_f_b1_e3"."id" = "yr_f_b1_e3"."id"
      AND "yr_f_b1_e3"."id" = "yr_f_b1_e6"."company_id"
  ),
  "yr_f_b2_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b2_e7"."movie_id" AS "yr_v2",
      "yr_f_b2_e4"."id" AS "yr_v5"
    FROM "public"."info_type" AS "yr_f_b2_e4"
      CROSS JOIN "public"."movie_info" AS "yr_f_b2_e7"
    WHERE
      "yr_f_b2_e7"."movie_id" = "yr_f_b2_e7"."movie_id"
      AND "yr_f_b2_e4"."id" = "yr_f_b2_e4"."id"
      AND "yr_f_b2_e4"."id" = "yr_f_b2_e7"."info_type_id"
  ),
  "yr_f_b3_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b3_e8"."movie_id" AS "yr_v2",
      "yr_f_b3_e5"."id" AS "yr_v6"
    FROM "public"."keyword" AS "yr_f_b3_e5"
      CROSS JOIN "public"."movie_keyword" AS "yr_f_b3_e8"
    WHERE
      "yr_f_b3_e8"."movie_id" = "yr_f_b3_e8"."movie_id"
      AND "yr_f_b3_e5"."id" = "yr_f_b3_e5"."id"
      AND "yr_f_b3_e5"."id" = "yr_f_b3_e8"."keyword_id"
  ),
  "yr_f_b4_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b4_e12"."movie_id" AS "yr_v2",
      "yr_f_b4_e12"."linked_movie_id" AS "yr_v7"
    FROM "public"."movie_link" AS "yr_f_b4_e12"
      CROSS JOIN "public"."title" AS "yr_f_b4_e13"
    WHERE
      "yr_f_b4_e12"."movie_id" = "yr_f_b4_e12"."movie_id"
      AND "yr_f_b4_e12"."linked_movie_id" = "yr_f_b4_e12"."linked_movie_id"
      AND "yr_f_b4_e12"."linked_movie_id" = "yr_f_b4_e13"."id"
  ),
  "yr_f_b5_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b5_e16"."movie_id" AS "yr_v7",
      "yr_f_b5_e16"."keyword_id" AS "yr_v9"
    FROM "public"."movie_keyword" AS "yr_f_b5_e16"
      CROSS JOIN "public"."keyword" AS "yr_f_b5_e17"
    WHERE
      "yr_f_b5_e16"."movie_id" = "yr_f_b5_e16"."movie_id"
      AND "yr_f_b5_e16"."keyword_id" = "yr_f_b5_e16"."keyword_id"
      AND "yr_f_b5_e16"."keyword_id" = "yr_f_b5_e17"."id"
  ),
  "yr_f_b6_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b6_e14"."movie_id" AS "yr_v2",
      "yr_f_b6_e14"."keyword_id" AS "yr_v8"
    FROM "public"."movie_keyword" AS "yr_f_b6_e14"
      CROSS JOIN "public"."keyword" AS "yr_f_b6_e15"
    WHERE
      "yr_f_b6_e14"."movie_id" = "yr_f_b6_e14"."movie_id"
      AND "yr_f_b6_e14"."keyword_id" = "yr_f_b6_e14"."keyword_id"
      AND "yr_f_b6_e14"."keyword_id" = "yr_f_b6_e15"."id"
  ),
"yr_gc_anchor" AS MATERIALIZED (SELECT r."yr_v2", r."yr_v7", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b4_i" AS r WHERE r."yr_v2" IS NOT NULL AND r."yr_v7" IS NOT NULL GROUP BY r."yr_v2", r."yr_v7"),
"yr_gc_count_2" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v2" FROM "yr_gc_anchor") AS d CROSS JOIN LATERAL (SELECT r."yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b2_i" AS r WHERE r."yr_v2" = d."yr_v2" GROUP BY r."yr_v2") AS z),
"yr_gc_step_2" AS MATERIALIZED (SELECT a."yr_v2", a."yr_v7", a.yr_freq * b2.yr_freq AS yr_freq FROM "yr_gc_anchor" AS a JOIN "yr_gc_count_2" AS b2 ON a."yr_v2" = b2."yr_v2" WHERE b2.yr_freq IS NOT NULL),
"yr_gc_count_3" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v2" FROM "yr_gc_step_2") AS d CROSS JOIN LATERAL (SELECT r."yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b3_i" AS r WHERE r."yr_v2" = d."yr_v2" GROUP BY r."yr_v2") AS z),
"yr_gc_step_3" AS MATERIALIZED (SELECT a."yr_v2", a."yr_v7", a.yr_freq * b3.yr_freq AS yr_freq FROM "yr_gc_step_2" AS a JOIN "yr_gc_count_3" AS b3 ON a."yr_v2" = b3."yr_v2" WHERE b3.yr_freq IS NOT NULL),
"yr_gc_count_5" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v7" FROM "yr_gc_step_3") AS d CROSS JOIN LATERAL (SELECT r."yr_v7", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b5_i" AS r WHERE r."yr_v7" = d."yr_v7" GROUP BY r."yr_v7") AS z),
"yr_gc_step_5" AS MATERIALIZED (SELECT a."yr_v2", a."yr_v7", a.yr_freq * b5.yr_freq AS yr_freq FROM "yr_gc_step_3" AS a JOIN "yr_gc_count_5" AS b5 ON a."yr_v7" = b5."yr_v7" WHERE b5.yr_freq IS NOT NULL),
"yr_gc_count_6" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v2" FROM "yr_gc_step_5") AS d CROSS JOIN LATERAL (SELECT r."yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b6_i" AS r WHERE r."yr_v2" = d."yr_v2" GROUP BY r."yr_v2") AS z),
"yr_gc_step_6" AS MATERIALIZED (SELECT a."yr_v2", a."yr_v7", a.yr_freq * b6.yr_freq AS yr_freq FROM "yr_gc_step_5" AS a JOIN "yr_gc_count_6" AS b6 ON a."yr_v2" = b6."yr_v2" WHERE b6.yr_freq IS NOT NULL),
"yr_gc_count_0" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v2" FROM "yr_gc_step_6") AS d CROSS JOIN LATERAL (SELECT r."yr_v0", r."yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b0_i" AS r WHERE r."yr_v2" = d."yr_v2" GROUP BY r."yr_v0", r."yr_v2") AS z),
"yr_gc_count_1" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v2" FROM "yr_gc_step_6") AS d CROSS JOIN LATERAL (SELECT r."yr_v2", r."yr_v4", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b1_i" AS r WHERE r."yr_v2" = d."yr_v2" GROUP BY r."yr_v2", r."yr_v4") AS z)
SELECT b0."yr_v0" AS "g1", b1."yr_v4" AS "g2", CAST(SUM(a.yr_freq * b0.yr_freq * b1.yr_freq) AS BIGINT) AS "witness_count" FROM "yr_gc_step_6" AS a JOIN "yr_gc_count_0" AS b0 ON a."yr_v2" = b0."yr_v2" JOIN "yr_gc_count_1" AS b1 ON a."yr_v2" = b1."yr_v2" GROUP BY b0."yr_v0", b1."yr_v4";