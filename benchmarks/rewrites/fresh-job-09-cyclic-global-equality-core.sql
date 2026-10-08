WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e4"."movie_id" AS "yr_v2"
    FROM "public"."company_name" AS "yr_f_b0_e3"
      CROSS JOIN "public"."movie_companies" AS "yr_f_b0_e4"
      CROSS JOIN "public"."title" AS "yr_f_b0_e7"
      CROSS JOIN "public"."movie_link" AS "yr_f_b0_e8"
      CROSS JOIN "public"."title" AS "yr_f_b0_e9"
      CROSS JOIN "public"."movie_keyword" AS "yr_f_b0_e10"
      CROSS JOIN "public"."keyword" AS "yr_f_b0_e11"
      CROSS JOIN "public"."movie_keyword" AS "yr_f_b0_e12"
      CROSS JOIN "public"."keyword" AS "yr_f_b0_e13"
    WHERE
      "yr_f_b0_e4"."movie_id" = "yr_f_b0_e4"."movie_id"
      AND "yr_f_b0_e4"."movie_id" = "yr_f_b0_e7"."id"
      AND "yr_f_b0_e4"."movie_id" = "yr_f_b0_e8"."movie_id"
      AND "yr_f_b0_e4"."movie_id" = "yr_f_b0_e10"."movie_id"
      AND "yr_f_b0_e3"."id" = "yr_f_b0_e3"."id"
      AND "yr_f_b0_e3"."id" = "yr_f_b0_e4"."company_id"
      AND "yr_f_b0_e8"."linked_movie_id" = "yr_f_b0_e8"."linked_movie_id"
      AND "yr_f_b0_e8"."linked_movie_id" = "yr_f_b0_e9"."id"
      AND "yr_f_b0_e8"."linked_movie_id" = "yr_f_b0_e12"."movie_id"
      AND "yr_f_b0_e10"."keyword_id" = "yr_f_b0_e10"."keyword_id"
      AND "yr_f_b0_e10"."keyword_id" = "yr_f_b0_e11"."id"
      AND "yr_f_b0_e10"."keyword_id" = "yr_f_b0_e12"."keyword_id"
      AND "yr_f_b0_e10"."keyword_id" = "yr_f_b0_e13"."id"
  ),
  "yr_f_b1_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b1_e2"."movie_id" AS "yr_v2"
    FROM "public"."aka_name" AS "yr_f_b1_e0"
      CROSS JOIN "public"."char_name" AS "yr_f_b1_e1"
      CROSS JOIN "public"."cast_info" AS "yr_f_b1_e2"
      CROSS JOIN "public"."name" AS "yr_f_b1_e5"
      CROSS JOIN "public"."role_type" AS "yr_f_b1_e6"
    WHERE
      "yr_f_b1_e0"."person_id" = "yr_f_b1_e0"."person_id"
      AND "yr_f_b1_e0"."person_id" = "yr_f_b1_e2"."person_id"
      AND "yr_f_b1_e0"."person_id" = "yr_f_b1_e5"."id"
      AND "yr_f_b1_e1"."id" = "yr_f_b1_e1"."id"
      AND "yr_f_b1_e1"."id" = "yr_f_b1_e2"."person_role_id"
      AND "yr_f_b1_e2"."movie_id" = "yr_f_b1_e2"."movie_id"
      AND "yr_f_b1_e2"."role_id" = "yr_f_b1_e2"."role_id"
      AND "yr_f_b1_e2"."role_id" = "yr_f_b1_e6"."id"
  ),
  "yr_f_b0_bu0" AS (
    SELECT
      "yr_t_bottom_up0_b0"."yr_v2" AS "yr_v2",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b0_i" AS "yr_t_bottom_up0_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_i" AS "yr_x_bottom_up0_b1"
      WHERE
        "yr_t_bottom_up0_b0"."yr_v2" = "yr_x_bottom_up0_b1"."yr_v2"
    )
    GROUP BY
      "yr_t_bottom_up0_b0"."yr_v2"
  ),
  "yr_f_b1_td0" AS (
    SELECT
      "yr_t_top_down0_b1"."yr_v2" AS "yr_v2",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b1_i" AS "yr_t_top_down0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu0" AS "yr_x_top_down0_b0"
      WHERE
        "yr_t_top_down0_b1"."yr_v2" = "yr_x_top_down0_b0"."yr_v2"
    )
    GROUP BY
      "yr_t_top_down0_b1"."yr_v2"
  )
SELECT
  CAST(COALESCE(SUM(("yr_cf_b0"."yr_freq" * "yr_cm_b1"."yr_freq")), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_bu0" AS "yr_cf_b0"
JOIN "yr_f_b1_td0" AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v2" = "yr_cm_b1"."yr_v2";