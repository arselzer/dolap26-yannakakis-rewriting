WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."s" AS "yr_v0",
      "yr_f_b0_e0"."d" AS "yr_v1"
    FROM "real_hetionet_endpoint"."hetio45173" AS "yr_f_b0_e0"
      CROSS JOIN "real_hetionet_endpoint"."hetio45160" AS "yr_f_b0_e2"
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
      "yr_f_b2_e3"."d" AS "yr_v3"
    FROM "real_hetionet_endpoint"."hetio45160" AS "yr_f_b2_e3"
      CROSS JOIN "real_hetionet_endpoint"."hetio45159" AS "yr_f_b2_e5"
    WHERE
      "yr_f_b2_e3"."s" = "yr_f_b2_e3"."s"
      AND "yr_f_b2_e3"."d" = "yr_f_b2_e3"."d"
      AND "yr_f_b2_e3"."d" = "yr_f_b2_e5"."s"
  ),
  "yr_f_b3_i" AS (
    SELECT
      "yr_f_b3_e4"."s" AS "yr_v3",
      "yr_f_b3_e4"."d" AS "yr_v4"
    FROM "real_hetionet_endpoint"."hetio45160" AS "yr_f_b3_e4"
      CROSS JOIN "real_hetionet_endpoint"."hetio45159" AS "yr_f_b3_e6"
    WHERE
      "yr_f_b3_e4"."s" = "yr_f_b3_e4"."s"
      AND "yr_f_b3_e4"."d" = "yr_f_b3_e4"."d"
      AND "yr_f_b3_e4"."d" = "yr_f_b3_e6"."s"
  )
SELECT
  CAST(COALESCE(SUM("yr_cm_b1"."yr_freq"), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_i" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v0" AS "yr_v0",
  COALESCE(SUM("yr_cm_b2"."yr_freq"), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b1_i" AS "yr_cf_b1"
JOIN (
SELECT
  "yr_cf_b2"."yr_v2" AS "yr_v2",
  COALESCE(SUM("yr_cm_b3"."yr_freq"), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b2_i" AS "yr_cf_b2"
JOIN (
SELECT
  "yr_cf_b3"."yr_v3" AS "yr_v3",
  CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
FROM "yr_f_b3_i" AS "yr_cf_b3"
GROUP BY
  "yr_cf_b3"."yr_v3"
) AS "yr_cm_b3" ON
  "yr_cf_b2"."yr_v3" = "yr_cm_b3"."yr_v3"
GROUP BY
  "yr_cf_b2"."yr_v2"
) AS "yr_cm_b2" ON
  "yr_cf_b1"."yr_v2" = "yr_cm_b2"."yr_v2"
GROUP BY
  "yr_cf_b1"."yr_v0"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v0" = "yr_cm_b1"."yr_v0";