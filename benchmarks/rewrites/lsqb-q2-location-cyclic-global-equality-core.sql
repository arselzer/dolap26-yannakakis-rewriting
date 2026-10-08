WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."person1id" AS "yr_v0",
      "yr_f_b0_e0"."person2id" AS "yr_v1",
      "yr_f_b0_e1"."replyof_postid" AS "yr_v2",
      "yr_f_b0_e3"."islocatedin_cityid" AS "yr_v3"
    FROM "real_lsqb_sf10_extended_a1"."person_knows_person" AS "yr_f_b0_e0"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."comment" AS "yr_f_b0_e1"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."post" AS "yr_f_b0_e2"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."person" AS "yr_f_b0_e3"
    WHERE
      "yr_f_b0_e0"."person1id" = "yr_f_b0_e0"."person1id"
      AND "yr_f_b0_e0"."person1id" = "yr_f_b0_e1"."hascreator_personid"
      AND "yr_f_b0_e0"."person1id" = "yr_f_b0_e3"."personid"
      AND "yr_f_b0_e0"."person2id" = "yr_f_b0_e0"."person2id"
      AND "yr_f_b0_e0"."person2id" = "yr_f_b0_e2"."hascreator_personid"
      AND "yr_f_b0_e1"."replyof_postid" = "yr_f_b0_e1"."replyof_postid"
      AND "yr_f_b0_e1"."replyof_postid" = "yr_f_b0_e2"."postid"
      AND "yr_f_b0_e3"."islocatedin_cityid" = "yr_f_b0_e3"."islocatedin_cityid"
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e4"."cityid" AS "yr_v3",
      "yr_f_b1_e4"."ispartof_countryid" AS "yr_v4"
    FROM "real_lsqb_sf10_extended_a1"."city" AS "yr_f_b1_e4"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."country" AS "yr_f_b1_e5"
    WHERE
      "yr_f_b1_e4"."cityid" = "yr_f_b1_e4"."cityid"
      AND "yr_f_b1_e4"."ispartof_countryid" = "yr_f_b1_e4"."ispartof_countryid"
      AND "yr_f_b1_e4"."ispartof_countryid" = "yr_f_b1_e5"."countryid"
  )
SELECT
  CAST(COALESCE(SUM("yr_cm_b1"."yr_freq"), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_i" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v3" AS "yr_v3",
  CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
FROM "yr_f_b1_i" AS "yr_cf_b1"
GROUP BY
  "yr_cf_b1"."yr_v3"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v3" = "yr_cm_b1"."yr_v3";