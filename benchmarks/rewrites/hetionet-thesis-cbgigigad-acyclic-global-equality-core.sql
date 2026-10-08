WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."nid" AS "yr_v0",
      "yr_f_b0_e1"."tid" AS "yr_v1"
    FROM "hetionet_thesis_original_a1"."compound" AS "yr_f_b0_e0"
      CROSS JOIN "hetionet_thesis_original_a1"."binds" AS "yr_f_b0_e1"
      CROSS JOIN "hetionet_thesis_original_a1"."gene" AS "yr_f_b0_e2"
      CROSS JOIN "hetionet_thesis_original_a1"."interacts" AS "yr_f_b0_e9"
    WHERE
      "yr_f_b0_e0"."nid" = "yr_f_b0_e0"."nid"
      AND "yr_f_b0_e0"."nid" = "yr_f_b0_e1"."sid"
      AND "yr_f_b0_e1"."tid" = "yr_f_b0_e1"."tid"
      AND "yr_f_b0_e1"."tid" = "yr_f_b0_e2"."nid"
      AND "yr_f_b0_e1"."tid" = "yr_f_b0_e9"."sid"
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e3"."sid" AS "yr_v1",
      "yr_f_b1_e3"."tid" AS "yr_v2"
    FROM "hetionet_thesis_original_a1"."interacts" AS "yr_f_b1_e3"
      CROSS JOIN "hetionet_thesis_original_a1"."gene" AS "yr_f_b1_e4"
    WHERE
      "yr_f_b1_e3"."sid" = "yr_f_b1_e3"."sid"
      AND "yr_f_b1_e3"."tid" = "yr_f_b1_e3"."tid"
      AND "yr_f_b1_e3"."tid" = "yr_f_b1_e4"."nid"
  ),
  "yr_f_b2_i" AS (
    SELECT
      "yr_f_b2_e5"."tid" AS "yr_v2",
      "yr_f_b2_e5"."sid" AS "yr_v3"
    FROM "hetionet_thesis_original_a1"."interacts" AS "yr_f_b2_e5"
      CROSS JOIN "hetionet_thesis_original_a1"."gene" AS "yr_f_b2_e6"
    WHERE
      "yr_f_b2_e5"."tid" = "yr_f_b2_e5"."tid"
      AND "yr_f_b2_e5"."sid" = "yr_f_b2_e5"."sid"
      AND "yr_f_b2_e5"."sid" = "yr_f_b2_e6"."nid"
  ),
  "yr_f_b3_i" AS (
    SELECT
      "yr_f_b3_e7"."tid" AS "yr_v3",
      "yr_f_b3_e7"."sid" AS "yr_v4"
    FROM "hetionet_thesis_original_a1"."associates" AS "yr_f_b3_e7"
      CROSS JOIN "hetionet_thesis_original_a1"."disease" AS "yr_f_b3_e8"
    WHERE
      "yr_f_b3_e7"."tid" = "yr_f_b3_e7"."tid"
      AND "yr_f_b3_e7"."sid" = "yr_f_b3_e7"."sid"
      AND "yr_f_b3_e7"."sid" = "yr_f_b3_e8"."nid"
  )
SELECT
  CAST(COALESCE(SUM("yr_cm_b1"."yr_freq"), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_i" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v1" AS "yr_v1",
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
  "yr_cf_b1"."yr_v1"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v1" = "yr_cm_b1"."yr_v1";