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
  "yr_f_b2_i" AS NOT MATERIALIZED (
    SELECT
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
      "yr_f_b4_e4"."personid" AS "yr_v4"
    FROM "real_lsqb_sf10_q_lb"."city" AS "yr_f_b4_e1"
      CROSS JOIN "real_lsqb_sf10_q_lb"."person" AS "yr_f_b4_e4"
    WHERE
      "yr_f_b4_e1"."cityid" = "yr_f_b4_e1"."cityid"
      AND "yr_f_b4_e1"."cityid" = "yr_f_b4_e4"."islocatedin_cityid"
      AND "yr_f_b4_e4"."personid" = "yr_f_b4_e4"."personid"
  ),
  "yr_f_b1_bu0" AS (
    SELECT "yr_t_bottom_up0_b1".*
    FROM "yr_f_b1_i" AS "yr_t_bottom_up0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_i" AS "yr_x_bottom_up0_b2"
      WHERE
        "yr_t_bottom_up0_b1"."yr_v5" = "yr_x_bottom_up0_b2"."yr_v5"
    )
  ),
  "yr_f_b3_bu1" AS (
    SELECT "yr_t_bottom_up1_b3".*
    FROM "yr_f_b3_i" AS "yr_t_bottom_up1_b3"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b4_i" AS "yr_x_bottom_up1_b4"
      WHERE
        "yr_t_bottom_up1_b3"."yr_v4" = "yr_x_bottom_up1_b4"."yr_v4"
    )
  ),
  "yr_f_b1_bu2" AS (
    SELECT "yr_t_bottom_up2_b1".*
    FROM "yr_f_b1_bu0" AS "yr_t_bottom_up2_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b3_bu1" AS "yr_x_bottom_up2_b3"
      WHERE
        "yr_t_bottom_up2_b1"."yr_v5" = "yr_x_bottom_up2_b3"."yr_v5"
    )
  ),
  "yr_f_b0_bu3" AS (
    SELECT
      "yr_t_bottom_up3_b0"."yr_v0" AS "yr_v0",
      "yr_t_bottom_up3_b0"."yr_v2" AS "yr_v2",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b0_i" AS "yr_t_bottom_up3_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_bu2" AS "yr_x_bottom_up3_b1"
      WHERE
        "yr_t_bottom_up3_b0"."yr_v2" = "yr_x_bottom_up3_b1"."yr_v2"
    )
    GROUP BY
      "yr_t_bottom_up3_b0"."yr_v0",
      "yr_t_bottom_up3_b0"."yr_v2"
  ),
  "yr_f_b1_td0" AS (
    SELECT
      "yr_t_top_down0_b1"."yr_v2" AS "yr_v2",
      "yr_t_top_down0_b1"."yr_v5" AS "yr_v5",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b1_bu2" AS "yr_t_top_down0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu3" AS "yr_x_top_down0_b0"
      WHERE
        "yr_t_top_down0_b1"."yr_v2" = "yr_x_top_down0_b0"."yr_v2"
    )
    GROUP BY
      "yr_t_top_down0_b1"."yr_v2",
      "yr_t_top_down0_b1"."yr_v5"
  ),
  "yr_f_b2_td1" AS (
    SELECT
      "yr_t_top_down1_b2"."yr_v5" AS "yr_v5",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b2_i" AS "yr_t_top_down1_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_td0" AS "yr_x_top_down1_b1"
      WHERE
        "yr_t_top_down1_b2"."yr_v5" = "yr_x_top_down1_b1"."yr_v5"
    )
    GROUP BY
      "yr_t_top_down1_b2"."yr_v5"
  ),
  "yr_f_b3_td2" AS (
    SELECT
      "yr_t_top_down2_b3"."yr_v4" AS "yr_v4",
      "yr_t_top_down2_b3"."yr_v5" AS "yr_v5",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b3_bu1" AS "yr_t_top_down2_b3"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_td0" AS "yr_x_top_down2_b1"
      WHERE
        "yr_t_top_down2_b3"."yr_v5" = "yr_x_top_down2_b1"."yr_v5"
    )
    GROUP BY
      "yr_t_top_down2_b3"."yr_v4",
      "yr_t_top_down2_b3"."yr_v5"
  ),
  "yr_f_b4_td3" AS (
    SELECT
      "yr_t_top_down3_b4"."yr_v4" AS "yr_v4",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b4_i" AS "yr_t_top_down3_b4"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b3_td2" AS "yr_x_top_down3_b3"
      WHERE
        "yr_t_top_down3_b4"."yr_v4" = "yr_x_top_down3_b3"."yr_v4"
    )
    GROUP BY
      "yr_t_top_down3_b4"."yr_v4"
  )
SELECT
  "yr_cf_b0"."yr_v0" AS "g1",
  CAST(COALESCE(SUM(("yr_cf_b0"."yr_freq" * "yr_cm_b1"."yr_freq")), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b0_bu3" AS "yr_cf_b0"
JOIN (
SELECT
  "yr_cf_b1"."yr_v2" AS "yr_v2",
  COALESCE(SUM(("yr_cf_b1"."yr_freq" * "yr_cm_b2"."yr_freq" * "yr_cm_b3"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b1_td0" AS "yr_cf_b1"
JOIN "yr_f_b2_td1" AS "yr_cm_b2" ON
  "yr_cf_b1"."yr_v5" = "yr_cm_b2"."yr_v5"
JOIN (
SELECT
  "yr_cf_b3"."yr_v5" AS "yr_v5",
  COALESCE(SUM(("yr_cf_b3"."yr_freq" * "yr_cm_b4"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b3_td2" AS "yr_cf_b3"
JOIN "yr_f_b4_td3" AS "yr_cm_b4" ON
  "yr_cf_b3"."yr_v4" = "yr_cm_b4"."yr_v4"
GROUP BY
  "yr_cf_b3"."yr_v5"
) AS "yr_cm_b3" ON
  "yr_cf_b1"."yr_v5" = "yr_cm_b3"."yr_v5"
GROUP BY
  "yr_cf_b1"."yr_v2"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v2" = "yr_cm_b1"."yr_v2"
GROUP BY
  "yr_cf_b0"."yr_v0";