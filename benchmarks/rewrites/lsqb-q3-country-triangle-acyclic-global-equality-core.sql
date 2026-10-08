WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."ispartof_countryid" AS "yr_v0",
      "yr_f_b0_e2"."cityid" AS "yr_v2"
    FROM "real_lsqb_sf10_q_lb"."city" AS "yr_f_b0_e0"
      CROSS JOIN "real_lsqb_sf10_q_lb"."city" AS "yr_f_b0_e2"
    WHERE
      "yr_f_b0_e0"."ispartof_countryid" = "yr_f_b0_e0"."ispartof_countryid"
      AND "yr_f_b0_e0"."ispartof_countryid" = "yr_f_b0_e2"."ispartof_countryid"
      AND "yr_f_b0_e2"."cityid" = "yr_f_b0_e2"."cityid"
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e5"."islocatedin_cityid" AS "yr_v2",
      "yr_f_b1_e5"."personid" AS "yr_v5"
    FROM "real_lsqb_sf10_q_lb"."person" AS "yr_f_b1_e5"
    WHERE
      "yr_f_b1_e5"."islocatedin_cityid" = "yr_f_b1_e5"."islocatedin_cityid"
      AND "yr_f_b1_e5"."personid" = "yr_f_b1_e5"."personid"
  ),
  "yr_f_b2_i" AS (
    SELECT
      "yr_f_b2_e3"."personid" AS "yr_v3",
      "yr_f_b2_e8"."person1id" AS "yr_v5"
    FROM "real_lsqb_sf10_q_lb"."person" AS "yr_f_b2_e3"
      CROSS JOIN "real_lsqb_sf10_q_lb"."person_knows_person" AS "yr_f_b2_e8"
    WHERE
      "yr_f_b2_e3"."personid" = "yr_f_b2_e3"."personid"
      AND "yr_f_b2_e3"."personid" = "yr_f_b2_e8"."person2id"
      AND "yr_f_b2_e8"."person1id" = "yr_f_b2_e8"."person1id"
  ),
  "yr_f_b3_i" AS (
    SELECT
      "yr_f_b3_e6"."person2id" AS "yr_v4",
      "yr_f_b3_e7"."person2id" AS "yr_v5"
    FROM "real_lsqb_sf10_q_lb"."person_knows_person" AS "yr_f_b3_e6"
      CROSS JOIN "real_lsqb_sf10_q_lb"."person_knows_person" AS "yr_f_b3_e7"
    WHERE
      "yr_f_b3_e6"."person2id" = "yr_f_b3_e6"."person2id"
      AND "yr_f_b3_e6"."person2id" = "yr_f_b3_e7"."person1id"
      AND "yr_f_b3_e7"."person2id" = "yr_f_b3_e7"."person2id"
  ),
  "yr_f_b4_i" AS (
    SELECT
      "yr_f_b4_e1"."cityid" AS "yr_v1",
      "yr_f_b4_e4"."personid" AS "yr_v4"
    FROM "real_lsqb_sf10_q_lb"."city" AS "yr_f_b4_e1"
      CROSS JOIN "real_lsqb_sf10_q_lb"."person" AS "yr_f_b4_e4"
    WHERE
      "yr_f_b4_e1"."cityid" = "yr_f_b4_e1"."cityid"
      AND "yr_f_b4_e1"."cityid" = "yr_f_b4_e4"."islocatedin_cityid"
      AND "yr_f_b4_e4"."personid" = "yr_f_b4_e4"."personid"
  )
SELECT
  CAST(COALESCE(SUM("yr_cm_b1"."yr_freq"), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_i" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v2" AS "yr_v2",
  COALESCE(SUM(("yr_cm_b2"."yr_freq" * "yr_cm_b3"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b1_i" AS "yr_cf_b1"
JOIN (
SELECT
  "yr_cf_b2"."yr_v5" AS "yr_v5",
  CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
FROM "yr_f_b2_i" AS "yr_cf_b2"
GROUP BY
  "yr_cf_b2"."yr_v5"
) AS "yr_cm_b2" ON
  "yr_cf_b1"."yr_v5" = "yr_cm_b2"."yr_v5"
JOIN (
SELECT
  "yr_cf_b3"."yr_v5" AS "yr_v5",
  COALESCE(SUM("yr_cm_b4"."yr_freq"), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b3_i" AS "yr_cf_b3"
JOIN (
SELECT
  "yr_cf_b4"."yr_v4" AS "yr_v4",
  CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
FROM "yr_f_b4_i" AS "yr_cf_b4"
GROUP BY
  "yr_cf_b4"."yr_v4"
) AS "yr_cm_b4" ON
  "yr_cf_b3"."yr_v4" = "yr_cm_b4"."yr_v4"
GROUP BY
  "yr_cf_b3"."yr_v5"
) AS "yr_cm_b3" ON
  "yr_cf_b1"."yr_v5" = "yr_cm_b3"."yr_v5"
GROUP BY
  "yr_cf_b1"."yr_v2"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v2" = "yr_cm_b1"."yr_v2";