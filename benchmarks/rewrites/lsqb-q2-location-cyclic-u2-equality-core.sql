WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e1"."replyof_postid" AS "yr_v2",
      "yr_f_b0_e4"."ispartof_countryid" AS "yr_v4"
    FROM "real_lsqb_sf10_extended_a1"."person_knows_person" AS "yr_f_b0_e0"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."comment" AS "yr_f_b0_e1"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."post" AS "yr_f_b0_e2"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."person" AS "yr_f_b0_e3"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."city" AS "yr_f_b0_e4"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."country" AS "yr_f_b0_e5"
    WHERE
      "yr_f_b0_e0"."person1id" = "yr_f_b0_e0"."person1id"
      AND "yr_f_b0_e0"."person1id" = "yr_f_b0_e1"."hascreator_personid"
      AND "yr_f_b0_e0"."person1id" = "yr_f_b0_e3"."personid"
      AND "yr_f_b0_e0"."person2id" = "yr_f_b0_e0"."person2id"
      AND "yr_f_b0_e0"."person2id" = "yr_f_b0_e2"."hascreator_personid"
      AND "yr_f_b0_e1"."replyof_postid" = "yr_f_b0_e1"."replyof_postid"
      AND "yr_f_b0_e1"."replyof_postid" = "yr_f_b0_e2"."postid"
      AND "yr_f_b0_e3"."islocatedin_cityid" = "yr_f_b0_e3"."islocatedin_cityid"
      AND "yr_f_b0_e3"."islocatedin_cityid" = "yr_f_b0_e4"."cityid"
      AND "yr_f_b0_e4"."ispartof_countryid" = "yr_f_b0_e4"."ispartof_countryid"
      AND "yr_f_b0_e4"."ispartof_countryid" = "yr_f_b0_e5"."countryid"
  )
SELECT
  "yr_cf_b0"."yr_v2" AS "g1",
  "yr_cf_b0"."yr_v4" AS "g2",
  COUNT(*) AS "witness_count"
FROM "yr_f_b0_i" AS "yr_cf_b0"
GROUP BY
  "yr_cf_b0"."yr_v2",
  "yr_cf_b0"."yr_v4";