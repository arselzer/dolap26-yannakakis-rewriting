WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."src" AS "yr_v0",
      "yr_f_b0_e0"."dst" AS "yr_v1"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b0_e0"
    WHERE
      "yr_f_b0_e0"."src" = "yr_f_b0_e0"."src"
      AND "yr_f_b0_e0"."dst" = "yr_f_b0_e0"."dst"
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e4"."src" AS "yr_v0",
      "yr_f_b1_e4"."dst" AS "yr_v4"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b1_e4"
    WHERE
      "yr_f_b1_e4"."src" = "yr_f_b1_e4"."src"
      AND "yr_f_b1_e4"."dst" = "yr_f_b1_e4"."dst"
  ),
  "yr_f_b2_i" AS (
    SELECT
      "yr_f_b2_e5"."src" AS "yr_v4",
      "yr_f_b2_e5"."dst" AS "yr_v5"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b2_e5"
    WHERE
      "yr_f_b2_e5"."src" = "yr_f_b2_e5"."src"
      AND "yr_f_b2_e5"."dst" = "yr_f_b2_e5"."dst"
  ),
  "yr_f_b3_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b3_e6"."src" AS "yr_v5"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b3_e6"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b3_e7"
    WHERE
      "yr_f_b3_e6"."src" = "yr_f_b3_e6"."src"
      AND "yr_f_b3_e6"."dst" = "yr_f_b3_e6"."dst"
      AND "yr_f_b3_e6"."dst" = "yr_f_b3_e7"."src"
  ),
  "yr_f_b4_i" AS (
    SELECT
      "yr_f_b4_e1"."src" AS "yr_v1",
      "yr_f_b4_e1"."dst" AS "yr_v2"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b4_e1"
    WHERE
      "yr_f_b4_e1"."src" = "yr_f_b4_e1"."src"
      AND "yr_f_b4_e1"."dst" = "yr_f_b4_e1"."dst"
  ),
  "yr_f_b5_i" AS (
    SELECT
      "yr_f_b5_e2"."src" AS "yr_v2"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b5_e2"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b5_e3"
    WHERE
      "yr_f_b5_e2"."src" = "yr_f_b5_e2"."src"
      AND "yr_f_b5_e2"."dst" = "yr_f_b5_e2"."dst"
      AND "yr_f_b5_e2"."dst" = "yr_f_b5_e3"."src"
  ),
  "yr_f_b2_bu0" AS (
    SELECT "yr_t_bottom_up0_b2".*
    FROM "yr_f_b2_i" AS "yr_t_bottom_up0_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b3_i" AS "yr_x_bottom_up0_b3"
      WHERE
        "yr_t_bottom_up0_b2"."yr_v5" = "yr_x_bottom_up0_b3"."yr_v5"
    )
  ),
  "yr_f_b1_bu1" AS (
    SELECT "yr_t_bottom_up1_b1".*
    FROM "yr_f_b1_i" AS "yr_t_bottom_up1_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_bu0" AS "yr_x_bottom_up1_b2"
      WHERE
        "yr_t_bottom_up1_b1"."yr_v4" = "yr_x_bottom_up1_b2"."yr_v4"
    )
  ),
  "yr_f_b0_bu2" AS (
    SELECT "yr_t_bottom_up2_b0".*
    FROM "yr_f_b0_i" AS "yr_t_bottom_up2_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_bu1" AS "yr_x_bottom_up2_b1"
      WHERE
        "yr_t_bottom_up2_b0"."yr_v0" = "yr_x_bottom_up2_b1"."yr_v0"
    )
  ),
  "yr_f_b4_bu3" AS (
    SELECT "yr_t_bottom_up3_b4".*
    FROM "yr_f_b4_i" AS "yr_t_bottom_up3_b4"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b5_i" AS "yr_x_bottom_up3_b5"
      WHERE
        "yr_t_bottom_up3_b4"."yr_v2" = "yr_x_bottom_up3_b5"."yr_v2"
    )
  ),
  "yr_f_b0_bu4" AS (
    SELECT
      "yr_t_bottom_up4_b0"."yr_v0" AS "yr_v0",
      "yr_t_bottom_up4_b0"."yr_v1" AS "yr_v1",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b0_bu2" AS "yr_t_bottom_up4_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b4_bu3" AS "yr_x_bottom_up4_b4"
      WHERE
        "yr_t_bottom_up4_b0"."yr_v1" = "yr_x_bottom_up4_b4"."yr_v1"
    )
    GROUP BY
      "yr_t_bottom_up4_b0"."yr_v0",
      "yr_t_bottom_up4_b0"."yr_v1"
  ),
  "yr_f_b1_td0" AS (
    SELECT
      "yr_t_top_down0_b1"."yr_v0" AS "yr_v0",
      "yr_t_top_down0_b1"."yr_v4" AS "yr_v4",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b1_bu1" AS "yr_t_top_down0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu4" AS "yr_x_top_down0_b0"
      WHERE
        "yr_t_top_down0_b1"."yr_v0" = "yr_x_top_down0_b0"."yr_v0"
    )
    GROUP BY
      "yr_t_top_down0_b1"."yr_v0",
      "yr_t_top_down0_b1"."yr_v4"
  ),
  "yr_f_b2_td1" AS (
    SELECT
      "yr_t_top_down1_b2"."yr_v4" AS "yr_v4",
      "yr_t_top_down1_b2"."yr_v5" AS "yr_v5",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b2_bu0" AS "yr_t_top_down1_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_td0" AS "yr_x_top_down1_b1"
      WHERE
        "yr_t_top_down1_b2"."yr_v4" = "yr_x_top_down1_b1"."yr_v4"
    )
    GROUP BY
      "yr_t_top_down1_b2"."yr_v4",
      "yr_t_top_down1_b2"."yr_v5"
  ),
  "yr_f_b3_td2" AS (
    SELECT
      "yr_t_top_down2_b3"."yr_v5" AS "yr_v5",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b3_i" AS "yr_t_top_down2_b3"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_td1" AS "yr_x_top_down2_b2"
      WHERE
        "yr_t_top_down2_b3"."yr_v5" = "yr_x_top_down2_b2"."yr_v5"
    )
    GROUP BY
      "yr_t_top_down2_b3"."yr_v5"
  ),
  "yr_f_b4_td3" AS (
    SELECT
      "yr_t_top_down3_b4"."yr_v1" AS "yr_v1",
      "yr_t_top_down3_b4"."yr_v2" AS "yr_v2",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b4_bu3" AS "yr_t_top_down3_b4"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu4" AS "yr_x_top_down3_b0"
      WHERE
        "yr_t_top_down3_b4"."yr_v1" = "yr_x_top_down3_b0"."yr_v1"
    )
    GROUP BY
      "yr_t_top_down3_b4"."yr_v1",
      "yr_t_top_down3_b4"."yr_v2"
  ),
  "yr_f_b5_td4" AS (
    SELECT
      "yr_t_top_down4_b5"."yr_v2" AS "yr_v2",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b5_i" AS "yr_t_top_down4_b5"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b4_td3" AS "yr_x_top_down4_b4"
      WHERE
        "yr_t_top_down4_b5"."yr_v2" = "yr_x_top_down4_b4"."yr_v2"
    )
    GROUP BY
      "yr_t_top_down4_b5"."yr_v2"
  )
SELECT
  "yr_cf_b0"."yr_v1" AS "g1",
  CAST(COALESCE(SUM(("yr_cf_b0"."yr_freq" * "yr_cm_b1"."yr_freq" * "yr_cm_b4"."yr_freq")), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_bu4" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v0" AS "yr_v0",
  COALESCE(SUM(("yr_cf_b1"."yr_freq" * "yr_cm_b2"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b1_td0" AS "yr_cf_b1"
JOIN (
SELECT
  "yr_cf_b2"."yr_v4" AS "yr_v4",
  COALESCE(SUM(("yr_cf_b2"."yr_freq" * "yr_cm_b3"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b2_td1" AS "yr_cf_b2"
JOIN "yr_f_b3_td2" AS "yr_cm_b3" ON
  "yr_cf_b2"."yr_v5" = "yr_cm_b3"."yr_v5"
GROUP BY
  "yr_cf_b2"."yr_v4"
) AS "yr_cm_b2" ON
  "yr_cf_b1"."yr_v4" = "yr_cm_b2"."yr_v4"
GROUP BY
  "yr_cf_b1"."yr_v0"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v0" = "yr_cm_b1"."yr_v0"
JOIN (
SELECT
  "yr_cf_b4"."yr_v1" AS "yr_v1",
  COALESCE(SUM(("yr_cf_b4"."yr_freq" * "yr_cm_b5"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b4_td3" AS "yr_cf_b4"
JOIN "yr_f_b5_td4" AS "yr_cm_b5" ON
  "yr_cf_b4"."yr_v2" = "yr_cm_b5"."yr_v2"
GROUP BY
  "yr_cf_b4"."yr_v1"
) AS "yr_cm_b4" ON
  "yr_cf_b0"."yr_v1" = "yr_cm_b4"."yr_v1"
GROUP BY
  "yr_cf_b0"."yr_v1";