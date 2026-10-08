WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."src" AS "yr_v0",
      "yr_f_b0_e0"."dst" AS "yr_v1",
      "yr_f_b0_e1"."dst" AS "yr_v2",
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
      "yr_f_b1_e3"."src" AS "yr_v0",
      "yr_f_b1_e5"."dst" AS "yr_v3",
      "yr_f_b1_e3"."dst" AS "yr_v4",
      "yr_f_b1_e4"."dst" AS "yr_v5"
    FROM "recognized_snap_wikivote"."edge" AS "yr_f_b1_e3"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b1_e4"
      CROSS JOIN "recognized_snap_wikivote"."edge" AS "yr_f_b1_e5"
    WHERE
      "yr_f_b1_e3"."src" = "yr_f_b1_e3"."src"
      AND "yr_f_b1_e5"."dst" = "yr_f_b1_e5"."dst"
      AND "yr_f_b1_e3"."dst" = "yr_f_b1_e3"."dst"
      AND "yr_f_b1_e3"."dst" = "yr_f_b1_e4"."src"
      AND "yr_f_b1_e4"."dst" = "yr_f_b1_e4"."dst"
      AND "yr_f_b1_e4"."dst" = "yr_f_b1_e5"."src"
  )
SELECT
  CAST(COALESCE(SUM("yr_cm_b1"."yr_freq"), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_i" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v0" AS "yr_v0",
  "yr_cf_b1"."yr_v3" AS "yr_v3",
  CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
FROM "yr_f_b1_i" AS "yr_cf_b1"
GROUP BY
  "yr_cf_b1"."yr_v0",
  "yr_cf_b1"."yr_v3"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v0" = "yr_cm_b1"."yr_v0"
  AND "yr_cf_b0"."yr_v3" = "yr_cm_b1"."yr_v3";