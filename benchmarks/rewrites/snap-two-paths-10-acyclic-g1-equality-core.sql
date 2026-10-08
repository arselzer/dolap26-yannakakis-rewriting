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
      "yr_f_b1_e5"."src" AS "yr_v0",
      "yr_f_b1_e5"."dst" AS "yr_v5"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b1_e5"
    WHERE
      "yr_f_b1_e5"."src" = "yr_f_b1_e5"."src"
      AND "yr_f_b1_e5"."dst" = "yr_f_b1_e5"."dst"
  ),
  "yr_f_b2_i" AS (
    SELECT
      "yr_f_b2_e6"."src" AS "yr_v5",
      "yr_f_b2_e6"."dst" AS "yr_v6"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b2_e6"
    WHERE
      "yr_f_b2_e6"."src" = "yr_f_b2_e6"."src"
      AND "yr_f_b2_e6"."dst" = "yr_f_b2_e6"."dst"
  ),
  "yr_f_b3_i" AS (
    SELECT
      "yr_f_b3_e7"."src" AS "yr_v6",
      "yr_f_b3_e7"."dst" AS "yr_v7"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b3_e7"
    WHERE
      "yr_f_b3_e7"."src" = "yr_f_b3_e7"."src"
      AND "yr_f_b3_e7"."dst" = "yr_f_b3_e7"."dst"
  ),
  "yr_f_b4_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b4_e8"."src" AS "yr_v7"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b4_e8"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b4_e9"
    WHERE
      "yr_f_b4_e8"."src" = "yr_f_b4_e8"."src"
      AND "yr_f_b4_e8"."dst" = "yr_f_b4_e8"."dst"
      AND "yr_f_b4_e8"."dst" = "yr_f_b4_e9"."src"
  ),
  "yr_f_b5_i" AS (
    SELECT
      "yr_f_b5_e1"."src" AS "yr_v1",
      "yr_f_b5_e1"."dst" AS "yr_v2"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b5_e1"
    WHERE
      "yr_f_b5_e1"."src" = "yr_f_b5_e1"."src"
      AND "yr_f_b5_e1"."dst" = "yr_f_b5_e1"."dst"
  ),
  "yr_f_b6_i" AS (
    SELECT
      "yr_f_b6_e2"."src" AS "yr_v2",
      "yr_f_b6_e2"."dst" AS "yr_v3"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b6_e2"
    WHERE
      "yr_f_b6_e2"."src" = "yr_f_b6_e2"."src"
      AND "yr_f_b6_e2"."dst" = "yr_f_b6_e2"."dst"
  ),
  "yr_f_b7_i" AS (
    SELECT
      "yr_f_b7_e3"."src" AS "yr_v3"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b7_e3"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b7_e4"
    WHERE
      "yr_f_b7_e3"."src" = "yr_f_b7_e3"."src"
      AND "yr_f_b7_e3"."dst" = "yr_f_b7_e3"."dst"
      AND "yr_f_b7_e3"."dst" = "yr_f_b7_e4"."src"
  ),
  "yr_f_b3_bu0" AS (
    SELECT "yr_t_bottom_up0_b3".*
    FROM "yr_f_b3_i" AS "yr_t_bottom_up0_b3"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b4_i" AS "yr_x_bottom_up0_b4"
      WHERE
        "yr_t_bottom_up0_b3"."yr_v7" = "yr_x_bottom_up0_b4"."yr_v7"
    )
  ),
  "yr_f_b2_bu1" AS (
    SELECT "yr_t_bottom_up1_b2".*
    FROM "yr_f_b2_i" AS "yr_t_bottom_up1_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b3_bu0" AS "yr_x_bottom_up1_b3"
      WHERE
        "yr_t_bottom_up1_b2"."yr_v6" = "yr_x_bottom_up1_b3"."yr_v6"
    )
  ),
  "yr_f_b1_bu2" AS (
    SELECT "yr_t_bottom_up2_b1".*
    FROM "yr_f_b1_i" AS "yr_t_bottom_up2_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_bu1" AS "yr_x_bottom_up2_b2"
      WHERE
        "yr_t_bottom_up2_b1"."yr_v5" = "yr_x_bottom_up2_b2"."yr_v5"
    )
  ),
  "yr_f_b0_bu3" AS (
    SELECT "yr_t_bottom_up3_b0".*
    FROM "yr_f_b0_i" AS "yr_t_bottom_up3_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_bu2" AS "yr_x_bottom_up3_b1"
      WHERE
        "yr_t_bottom_up3_b0"."yr_v0" = "yr_x_bottom_up3_b1"."yr_v0"
    )
  ),
  "yr_f_b6_bu4" AS (
    SELECT "yr_t_bottom_up4_b6".*
    FROM "yr_f_b6_i" AS "yr_t_bottom_up4_b6"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b7_i" AS "yr_x_bottom_up4_b7"
      WHERE
        "yr_t_bottom_up4_b6"."yr_v3" = "yr_x_bottom_up4_b7"."yr_v3"
    )
  ),
  "yr_f_b5_bu5" AS (
    SELECT "yr_t_bottom_up5_b5".*
    FROM "yr_f_b5_i" AS "yr_t_bottom_up5_b5"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b6_bu4" AS "yr_x_bottom_up5_b6"
      WHERE
        "yr_t_bottom_up5_b5"."yr_v2" = "yr_x_bottom_up5_b6"."yr_v2"
    )
  ),
  "yr_f_b0_bu6" AS (
    SELECT
      "yr_t_bottom_up6_b0"."yr_v0" AS "yr_v0",
      "yr_t_bottom_up6_b0"."yr_v1" AS "yr_v1",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b0_bu3" AS "yr_t_bottom_up6_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b5_bu5" AS "yr_x_bottom_up6_b5"
      WHERE
        "yr_t_bottom_up6_b0"."yr_v1" = "yr_x_bottom_up6_b5"."yr_v1"
    )
    GROUP BY
      "yr_t_bottom_up6_b0"."yr_v0",
      "yr_t_bottom_up6_b0"."yr_v1"
  ),
  "yr_f_b1_td0" AS (
    SELECT
      "yr_t_top_down0_b1"."yr_v0" AS "yr_v0",
      "yr_t_top_down0_b1"."yr_v5" AS "yr_v5",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b1_bu2" AS "yr_t_top_down0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu6" AS "yr_x_top_down0_b0"
      WHERE
        "yr_t_top_down0_b1"."yr_v0" = "yr_x_top_down0_b0"."yr_v0"
    )
    GROUP BY
      "yr_t_top_down0_b1"."yr_v0",
      "yr_t_top_down0_b1"."yr_v5"
  ),
  "yr_f_b2_td1" AS (
    SELECT
      "yr_t_top_down1_b2"."yr_v5" AS "yr_v5",
      "yr_t_top_down1_b2"."yr_v6" AS "yr_v6",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b2_bu1" AS "yr_t_top_down1_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_td0" AS "yr_x_top_down1_b1"
      WHERE
        "yr_t_top_down1_b2"."yr_v5" = "yr_x_top_down1_b1"."yr_v5"
    )
    GROUP BY
      "yr_t_top_down1_b2"."yr_v5",
      "yr_t_top_down1_b2"."yr_v6"
  ),
  "yr_f_b3_td2" AS (
    SELECT
      "yr_t_top_down2_b3"."yr_v6" AS "yr_v6",
      "yr_t_top_down2_b3"."yr_v7" AS "yr_v7",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b3_bu0" AS "yr_t_top_down2_b3"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_td1" AS "yr_x_top_down2_b2"
      WHERE
        "yr_t_top_down2_b3"."yr_v6" = "yr_x_top_down2_b2"."yr_v6"
    )
    GROUP BY
      "yr_t_top_down2_b3"."yr_v6",
      "yr_t_top_down2_b3"."yr_v7"
  ),
  "yr_f_b4_td3" AS (
    SELECT
      "yr_t_top_down3_b4"."yr_v7" AS "yr_v7",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b4_i" AS "yr_t_top_down3_b4"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b3_td2" AS "yr_x_top_down3_b3"
      WHERE
        "yr_t_top_down3_b4"."yr_v7" = "yr_x_top_down3_b3"."yr_v7"
    )
    GROUP BY
      "yr_t_top_down3_b4"."yr_v7"
  ),
  "yr_f_b5_td4" AS (
    SELECT
      "yr_t_top_down4_b5"."yr_v1" AS "yr_v1",
      "yr_t_top_down4_b5"."yr_v2" AS "yr_v2",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b5_bu5" AS "yr_t_top_down4_b5"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu6" AS "yr_x_top_down4_b0"
      WHERE
        "yr_t_top_down4_b5"."yr_v1" = "yr_x_top_down4_b0"."yr_v1"
    )
    GROUP BY
      "yr_t_top_down4_b5"."yr_v1",
      "yr_t_top_down4_b5"."yr_v2"
  ),
  "yr_f_b6_td5" AS (
    SELECT
      "yr_t_top_down5_b6"."yr_v2" AS "yr_v2",
      "yr_t_top_down5_b6"."yr_v3" AS "yr_v3",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b6_bu4" AS "yr_t_top_down5_b6"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b5_td4" AS "yr_x_top_down5_b5"
      WHERE
        "yr_t_top_down5_b6"."yr_v2" = "yr_x_top_down5_b5"."yr_v2"
    )
    GROUP BY
      "yr_t_top_down5_b6"."yr_v2",
      "yr_t_top_down5_b6"."yr_v3"
  ),
  "yr_f_b7_td6" AS (
    SELECT
      "yr_t_top_down6_b7"."yr_v3" AS "yr_v3",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b7_i" AS "yr_t_top_down6_b7"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b6_td5" AS "yr_x_top_down6_b6"
      WHERE
        "yr_t_top_down6_b7"."yr_v3" = "yr_x_top_down6_b6"."yr_v3"
    )
    GROUP BY
      "yr_t_top_down6_b7"."yr_v3"
  )
SELECT
  "yr_cf_b0"."yr_v1" AS "g1",
  CAST(COALESCE(SUM(("yr_cf_b0"."yr_freq" * "yr_cm_b1"."yr_freq" * "yr_cm_b5"."yr_freq")), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_bu6" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v0" AS "yr_v0",
  COALESCE(SUM(("yr_cf_b1"."yr_freq" * "yr_cm_b2"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b1_td0" AS "yr_cf_b1"
JOIN (
SELECT
  "yr_cf_b2"."yr_v5" AS "yr_v5",
  COALESCE(SUM(("yr_cf_b2"."yr_freq" * "yr_cm_b3"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b2_td1" AS "yr_cf_b2"
JOIN (
SELECT
  "yr_cf_b3"."yr_v6" AS "yr_v6",
  COALESCE(SUM(("yr_cf_b3"."yr_freq" * "yr_cm_b4"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b3_td2" AS "yr_cf_b3"
JOIN "yr_f_b4_td3" AS "yr_cm_b4" ON
  "yr_cf_b3"."yr_v7" = "yr_cm_b4"."yr_v7"
GROUP BY
  "yr_cf_b3"."yr_v6"
) AS "yr_cm_b3" ON
  "yr_cf_b2"."yr_v6" = "yr_cm_b3"."yr_v6"
GROUP BY
  "yr_cf_b2"."yr_v5"
) AS "yr_cm_b2" ON
  "yr_cf_b1"."yr_v5" = "yr_cm_b2"."yr_v5"
GROUP BY
  "yr_cf_b1"."yr_v0"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v0" = "yr_cm_b1"."yr_v0"
JOIN (
SELECT
  "yr_cf_b5"."yr_v1" AS "yr_v1",
  COALESCE(SUM(("yr_cf_b5"."yr_freq" * "yr_cm_b6"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b5_td4" AS "yr_cf_b5"
JOIN (
SELECT
  "yr_cf_b6"."yr_v2" AS "yr_v2",
  COALESCE(SUM(("yr_cf_b6"."yr_freq" * "yr_cm_b7"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b6_td5" AS "yr_cf_b6"
JOIN "yr_f_b7_td6" AS "yr_cm_b7" ON
  "yr_cf_b6"."yr_v3" = "yr_cm_b7"."yr_v3"
GROUP BY
  "yr_cf_b6"."yr_v2"
) AS "yr_cm_b6" ON
  "yr_cf_b5"."yr_v2" = "yr_cm_b6"."yr_v2"
GROUP BY
  "yr_cf_b5"."yr_v1"
) AS "yr_cm_b5" ON
  "yr_cf_b0"."yr_v1" = "yr_cm_b5"."yr_v1"
GROUP BY
  "yr_cf_b0"."yr_v1";