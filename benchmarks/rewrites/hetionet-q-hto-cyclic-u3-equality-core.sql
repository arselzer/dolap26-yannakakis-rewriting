WITH
  "yr_s_b1" AS (
    SELECT DISTINCT
      "yr_s_b1_e2"."d" AS "yr_v3"
    FROM "real_hetionet_endpoint"."hetio45160" AS "yr_s_b1_e2"
  ),
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."s" AS "yr_v0",
      "yr_f_b0_e2"."d" AS "yr_v3"
    FROM "real_hetionet_endpoint"."hetio45173" AS "yr_f_b0_e0"
      CROSS JOIN "real_hetionet_endpoint"."hetio45173" AS "yr_f_b0_e1"
      CROSS JOIN "real_hetionet_endpoint"."hetio45160" AS "yr_f_b0_e2"
      CROSS JOIN "real_hetionet_endpoint"."hetio45160" AS "yr_f_b0_e3"
    WHERE
      "yr_f_b0_e0"."s" = "yr_f_b0_e0"."s"
      AND "yr_f_b0_e0"."s" = "yr_f_b0_e1"."s"
      AND "yr_f_b0_e0"."d" = "yr_f_b0_e0"."d"
      AND "yr_f_b0_e0"."d" = "yr_f_b0_e2"."s"
      AND "yr_f_b0_e1"."d" = "yr_f_b0_e1"."d"
      AND "yr_f_b0_e1"."d" = "yr_f_b0_e3"."s"
      AND "yr_f_b0_e2"."d" = "yr_f_b0_e2"."d"
      AND "yr_f_b0_e2"."d" = "yr_f_b0_e3"."d"
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e5"."s" AS "yr_v3",
      "yr_f_b1_e6"."s" AS "yr_v4"
    FROM "yr_s_b1" AS "yr_fs_b1"
      CROSS JOIN "real_hetionet_endpoint"."hetio45160" AS "yr_f_b1_e4"
      CROSS JOIN "real_hetionet_endpoint"."hetio45159" AS "yr_f_b1_e5"
      CROSS JOIN "real_hetionet_endpoint"."hetio45159" AS "yr_f_b1_e6"
    WHERE
      "yr_f_b1_e4"."s" = "yr_fs_b1"."yr_v3"
      AND "yr_f_b1_e4"."d" = "yr_f_b1_e4"."d"
      AND "yr_f_b1_e5"."s" = "yr_fs_b1"."yr_v3"
      AND "yr_f_b1_e5"."d" = "yr_f_b1_e5"."d"
      AND "yr_f_b1_e6"."s" = "yr_f_b1_e4"."d"
      AND "yr_f_b1_e6"."d" = "yr_f_b1_e5"."d"
  ),
  "yr_f_b0_bu0" AS (
    SELECT
      "yr_t_bottom_up0_b0"."yr_v0" AS "yr_v0",
      "yr_t_bottom_up0_b0"."yr_v3" AS "yr_v3",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b0_i" AS "yr_t_bottom_up0_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_i" AS "yr_x_bottom_up0_b1"
      WHERE
        "yr_t_bottom_up0_b0"."yr_v3" = "yr_x_bottom_up0_b1"."yr_v3"
    )
    GROUP BY
      "yr_t_bottom_up0_b0"."yr_v0",
      "yr_t_bottom_up0_b0"."yr_v3"
  ),
  "yr_f_b1_td0" AS (
    SELECT
      "yr_t_top_down0_b1"."yr_v3" AS "yr_v3",
      "yr_t_top_down0_b1"."yr_v4" AS "yr_v4",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b1_i" AS "yr_t_top_down0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu0" AS "yr_x_top_down0_b0"
      WHERE
        "yr_t_top_down0_b1"."yr_v3" = "yr_x_top_down0_b0"."yr_v3"
    )
    GROUP BY
      "yr_t_top_down0_b1"."yr_v3",
      "yr_t_top_down0_b1"."yr_v4"
  )
SELECT
  "yr_cf_b1"."yr_v4" AS "g1",
  "yr_cm_b0"."yr_v0" AS "g2",
  "yr_cf_b1"."yr_v3" AS "g3",
  CAST(COALESCE(SUM(("yr_cf_b1"."yr_freq" * "yr_cm_b0"."yr_freq")), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b1_td0" AS "yr_cf_b1"
JOIN "yr_f_b0_bu0" AS "yr_cm_b0" ON
  "yr_cf_b1"."yr_v3" = "yr_cm_b0"."yr_v3"
GROUP BY
  "yr_cf_b1"."yr_v4",
  "yr_cm_b0"."yr_v0",
  "yr_cf_b1"."yr_v3";