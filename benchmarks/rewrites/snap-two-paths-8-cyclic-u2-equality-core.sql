WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."src" AS "yr_v0",
      "yr_f_b0_e0"."dst" AS "yr_v1",
      "yr_f_b0_e2"."dst" AS "yr_v3"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b0_e0"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b0_e1"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b0_e2"
    WHERE
      "yr_f_b0_e0"."src" = "yr_f_b0_e0"."src"
      AND "yr_f_b0_e0"."dst" = "yr_f_b0_e0"."dst"
      AND "yr_f_b0_e0"."dst" = "yr_f_b0_e1"."src"
      AND "yr_f_b0_e1"."dst" = "yr_f_b0_e1"."dst"
      AND "yr_f_b0_e1"."dst" = "yr_f_b0_e2"."src"
      AND "yr_f_b0_e2"."dst" = "yr_f_b0_e2"."dst"
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e4"."src" AS "yr_v0",
      "yr_f_b1_e3"."src" AS "yr_v3",
      "yr_f_b1_e3"."dst" AS "yr_v4",
      "yr_f_b1_e4"."dst" AS "yr_v5"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b1_e3"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b1_e4"
    WHERE
      "yr_f_b1_e4"."src" = "yr_f_b1_e4"."src"
      AND "yr_f_b1_e3"."src" = "yr_f_b1_e3"."src"
      AND "yr_f_b1_e3"."dst" = "yr_f_b1_e3"."dst"
      AND "yr_f_b1_e4"."dst" = "yr_f_b1_e4"."dst"
  ),
  "yr_f_b2_i" AS (
    SELECT
      "yr_f_b2_e7"."dst" AS "yr_v4",
      "yr_f_b2_e5"."src" AS "yr_v5",
      "yr_f_b2_e6"."dst" AS "yr_v7"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b2_e5"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b2_e6"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b2_e7"
    WHERE
      "yr_f_b2_e7"."dst" = "yr_f_b2_e7"."dst"
      AND "yr_f_b2_e5"."src" = "yr_f_b2_e5"."src"
      AND "yr_f_b2_e5"."dst" = "yr_f_b2_e5"."dst"
      AND "yr_f_b2_e5"."dst" = "yr_f_b2_e6"."src"
      AND "yr_f_b2_e6"."dst" = "yr_f_b2_e6"."dst"
      AND "yr_f_b2_e6"."dst" = "yr_f_b2_e7"."src"
  ),
  "yr_f_b1_bu0" AS (
    SELECT "yr_t_bottom_up0_b1".*
    FROM "yr_f_b1_i" AS "yr_t_bottom_up0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_i" AS "yr_x_bottom_up0_b2"
      WHERE
        "yr_t_bottom_up0_b1"."yr_v4" = "yr_x_bottom_up0_b2"."yr_v4"
        AND "yr_t_bottom_up0_b1"."yr_v5" = "yr_x_bottom_up0_b2"."yr_v5"
    )
  ),
  "yr_f_b0_bu1" AS (
    SELECT
      "yr_t_bottom_up1_b0"."yr_v0" AS "yr_v0",
      "yr_t_bottom_up1_b0"."yr_v1" AS "yr_v1",
      "yr_t_bottom_up1_b0"."yr_v3" AS "yr_v3",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b0_i" AS "yr_t_bottom_up1_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_bu0" AS "yr_x_bottom_up1_b1"
      WHERE
        "yr_t_bottom_up1_b0"."yr_v0" = "yr_x_bottom_up1_b1"."yr_v0"
        AND "yr_t_bottom_up1_b0"."yr_v3" = "yr_x_bottom_up1_b1"."yr_v3"
    )
    GROUP BY
      "yr_t_bottom_up1_b0"."yr_v0",
      "yr_t_bottom_up1_b0"."yr_v1",
      "yr_t_bottom_up1_b0"."yr_v3"
  ),
  "yr_f_b1_td0" AS (
    SELECT
      "yr_t_top_down0_b1"."yr_v0" AS "yr_v0",
      "yr_t_top_down0_b1"."yr_v3" AS "yr_v3",
      "yr_t_top_down0_b1"."yr_v4" AS "yr_v4",
      "yr_t_top_down0_b1"."yr_v5" AS "yr_v5",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b1_bu0" AS "yr_t_top_down0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu1" AS "yr_x_top_down0_b0"
      WHERE
        "yr_t_top_down0_b1"."yr_v0" = "yr_x_top_down0_b0"."yr_v0"
        AND "yr_t_top_down0_b1"."yr_v3" = "yr_x_top_down0_b0"."yr_v3"
    )
    GROUP BY
      "yr_t_top_down0_b1"."yr_v0",
      "yr_t_top_down0_b1"."yr_v3",
      "yr_t_top_down0_b1"."yr_v4",
      "yr_t_top_down0_b1"."yr_v5"
  ),
  "yr_f_b2_td1" AS (
    SELECT
      "yr_t_top_down1_b2"."yr_v4" AS "yr_v4",
      "yr_t_top_down1_b2"."yr_v5" AS "yr_v5",
      "yr_t_top_down1_b2"."yr_v7" AS "yr_v7",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b2_i" AS "yr_t_top_down1_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_td0" AS "yr_x_top_down1_b1"
      WHERE
        "yr_t_top_down1_b2"."yr_v4" = "yr_x_top_down1_b1"."yr_v4"
        AND "yr_t_top_down1_b2"."yr_v5" = "yr_x_top_down1_b1"."yr_v5"
    )
    GROUP BY
      "yr_t_top_down1_b2"."yr_v4",
      "yr_t_top_down1_b2"."yr_v5",
      "yr_t_top_down1_b2"."yr_v7"
  )
SELECT
  "yr_cf_b0"."yr_v1" AS "g1",
  "yr_cm_b1"."yr_v7" AS "g2",
  CAST(COALESCE(SUM(("yr_cf_b0"."yr_freq" * "yr_cm_b1"."yr_freq")), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_bu1" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v0" AS "yr_v0",
  "yr_cf_b1"."yr_v3" AS "yr_v3",
  "yr_cm_b2"."yr_v7" AS "yr_v7",
  COALESCE(SUM(("yr_cf_b1"."yr_freq" * "yr_cm_b2"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b1_td0" AS "yr_cf_b1"
JOIN "yr_f_b2_td1" AS "yr_cm_b2" ON
  "yr_cf_b1"."yr_v4" = "yr_cm_b2"."yr_v4"
  AND "yr_cf_b1"."yr_v5" = "yr_cm_b2"."yr_v5"
GROUP BY
  "yr_cf_b1"."yr_v0",
  "yr_cf_b1"."yr_v3",
  "yr_cm_b2"."yr_v7"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v0" = "yr_cm_b1"."yr_v0"
  AND "yr_cf_b0"."yr_v3" = "yr_cm_b1"."yr_v3"
GROUP BY
  "yr_cf_b0"."yr_v1",
  "yr_cm_b1"."yr_v7";