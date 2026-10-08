WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."s" AS "yr_v0"
    FROM "real_hetionet_endpoint"."hetio45173" AS "yr_f_b0_e0"
      CROSS JOIN "real_hetionet_endpoint"."hetio45173" AS "yr_f_b0_e2"
    WHERE
      "yr_f_b0_e0"."s" = "yr_f_b0_e0"."s"
      AND "yr_f_b0_e0"."d" = "yr_f_b0_e0"."d"
      AND "yr_f_b0_e0"."d" = "yr_f_b0_e2"."s"
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e1"."s" AS "yr_v0",
      "yr_f_b1_e1"."d" AS "yr_v2"
    FROM "real_hetionet_endpoint"."hetio45173" AS "yr_f_b1_e1"
    WHERE
      "yr_f_b1_e1"."s" = "yr_f_b1_e1"."s"
      AND "yr_f_b1_e1"."d" = "yr_f_b1_e1"."d"
  ),
  "yr_f_b2_i" AS (
    SELECT
      "yr_f_b2_e3"."s" AS "yr_v2",
      "yr_f_b2_e4"."s" AS "yr_v3"
    FROM "real_hetionet_endpoint"."hetio45173" AS "yr_f_b2_e3"
      CROSS JOIN "real_hetionet_endpoint"."hetio45160" AS "yr_f_b2_e4"
    WHERE
      "yr_f_b2_e3"."s" = "yr_f_b2_e3"."s"
      AND "yr_f_b2_e3"."d" = "yr_f_b2_e3"."d"
      AND "yr_f_b2_e3"."d" = "yr_f_b2_e4"."s"
  ),
  "yr_f_b3_i" AS (
    SELECT
      "yr_f_b3_e5"."s" AS "yr_v3",
      "yr_f_b3_e5"."d" AS "yr_v4"
    FROM "real_hetionet_endpoint"."hetio45176" AS "yr_f_b3_e5"
      CROSS JOIN "real_hetionet_endpoint"."hetio45176" AS "yr_f_b3_e6"
    WHERE
      "yr_f_b3_e5"."s" = "yr_f_b3_e5"."s"
      AND "yr_f_b3_e5"."d" = "yr_f_b3_e5"."d"
      AND "yr_f_b3_e5"."d" = "yr_f_b3_e6"."d"
  ),
  "yr_f_b2_bu0" AS (
    SELECT "yr_t_bottom_up0_b2".*
    FROM "yr_f_b2_i" AS "yr_t_bottom_up0_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b3_i" AS "yr_x_bottom_up0_b3"
      WHERE
        "yr_t_bottom_up0_b2"."yr_v3" = "yr_x_bottom_up0_b3"."yr_v3"
    )
  ),
  "yr_f_b1_bu1" AS (
    SELECT "yr_t_bottom_up1_b1".*
    FROM "yr_f_b1_i" AS "yr_t_bottom_up1_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_bu0" AS "yr_x_bottom_up1_b2"
      WHERE
        "yr_t_bottom_up1_b1"."yr_v2" = "yr_x_bottom_up1_b2"."yr_v2"
    )
  ),
  "yr_f_b0_bu2" AS (
    SELECT
      "yr_t_bottom_up2_b0"."yr_v0" AS "yr_v0",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b0_i" AS "yr_t_bottom_up2_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_bu1" AS "yr_x_bottom_up2_b1"
      WHERE
        "yr_t_bottom_up2_b0"."yr_v0" = "yr_x_bottom_up2_b1"."yr_v0"
    )
    GROUP BY
      "yr_t_bottom_up2_b0"."yr_v0"
  ),
  "yr_f_b1_td0" AS (
    SELECT
      "yr_t_top_down0_b1"."yr_v0" AS "yr_v0",
      "yr_t_top_down0_b1"."yr_v2" AS "yr_v2",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b1_bu1" AS "yr_t_top_down0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu2" AS "yr_x_top_down0_b0"
      WHERE
        "yr_t_top_down0_b1"."yr_v0" = "yr_x_top_down0_b0"."yr_v0"
    )
    GROUP BY
      "yr_t_top_down0_b1"."yr_v0",
      "yr_t_top_down0_b1"."yr_v2"
  ),
  "yr_f_b2_td1" AS (
    SELECT
      "yr_t_top_down1_b2"."yr_v2" AS "yr_v2",
      "yr_t_top_down1_b2"."yr_v3" AS "yr_v3",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b2_bu0" AS "yr_t_top_down1_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_td0" AS "yr_x_top_down1_b1"
      WHERE
        "yr_t_top_down1_b2"."yr_v2" = "yr_x_top_down1_b1"."yr_v2"
    )
    GROUP BY
      "yr_t_top_down1_b2"."yr_v2",
      "yr_t_top_down1_b2"."yr_v3"
  ),
  "yr_f_b3_td2" AS (
    SELECT
      "yr_t_top_down2_b3"."yr_v3" AS "yr_v3",
      "yr_t_top_down2_b3"."yr_v4" AS "yr_v4",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b3_i" AS "yr_t_top_down2_b3"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_td1" AS "yr_x_top_down2_b2"
      WHERE
        "yr_t_top_down2_b3"."yr_v3" = "yr_x_top_down2_b2"."yr_v3"
    )
    GROUP BY
      "yr_t_top_down2_b3"."yr_v3",
      "yr_t_top_down2_b3"."yr_v4"
  )
SELECT
  "yr_cf_b0"."yr_v0" AS "g1",
  "yr_cm_b1"."yr_v4" AS "g2",
  "yr_cm_b1"."yr_v3" AS "g3",
  CAST(COALESCE(SUM(("yr_cf_b0"."yr_freq" * "yr_cm_b1"."yr_freq")), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_bu2" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v0" AS "yr_v0",
  "yr_cm_b2"."yr_v4" AS "yr_v4",
  "yr_cm_b2"."yr_v3" AS "yr_v3",
  COALESCE(SUM(("yr_cf_b1"."yr_freq" * "yr_cm_b2"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b1_td0" AS "yr_cf_b1"
JOIN (
SELECT
  "yr_cf_b2"."yr_v2" AS "yr_v2",
  "yr_cm_b3"."yr_v4" AS "yr_v4",
  "yr_cf_b2"."yr_v3" AS "yr_v3",
  ("yr_cf_b2"."yr_freq" * "yr_cm_b3"."yr_freq") AS "yr_freq"
FROM "yr_f_b2_td1" AS "yr_cf_b2"
JOIN "yr_f_b3_td2" AS "yr_cm_b3" ON
  "yr_cf_b2"."yr_v3" = "yr_cm_b3"."yr_v3"
) AS "yr_cm_b2" ON
  "yr_cf_b1"."yr_v2" = "yr_cm_b2"."yr_v2"
GROUP BY
  "yr_cf_b1"."yr_v0",
  "yr_cm_b2"."yr_v4",
  "yr_cm_b2"."yr_v3"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v0" = "yr_cm_b1"."yr_v0"
GROUP BY
  "yr_cf_b0"."yr_v0",
  "yr_cm_b1"."yr_v4",
  "yr_cm_b1"."yr_v3";