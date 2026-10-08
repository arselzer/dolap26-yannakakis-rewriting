WITH
  "yr_f_b0_i" AS (
    SELECT
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
  ),
  "yr_f_b2_bu0" AS (
    SELECT "yr_t_bottom_up0_b2".*
    FROM "yr_f_b2_i" AS "yr_t_bottom_up0_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b3_i" AS "yr_x_bottom_up0_b3"
      WHERE
        "yr_t_bottom_up0_b2"."yr_v3" = "yr_x_bottom_up0_b3"."yr_v3"
    )
  ),
  "yr_f_b1_bu1" AS (
    SELECT "yr_t_bottom_up1_b1".*
    FROM "yr_f_b1_i" AS "yr_t_bottom_up1_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_bu0" AS "yr_x_bottom_up1_b2"
      WHERE
        "yr_t_bottom_up1_b1"."yr_v2" = "yr_x_bottom_up1_b2"."yr_v2"
    )
  ),
  "yr_f_b0_bu2" AS (
    SELECT
      "yr_t_bottom_up2_b0"."yr_v1" AS "yr_v1",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b0_i" AS "yr_t_bottom_up2_b0"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_bu1" AS "yr_x_bottom_up2_b1"
      WHERE
        "yr_t_bottom_up2_b0"."yr_v1" = "yr_x_bottom_up2_b1"."yr_v1"
    )
    GROUP BY
      "yr_t_bottom_up2_b0"."yr_v1"
  ),
  "yr_f_b1_td0" AS (
    SELECT
      "yr_t_top_down0_b1"."yr_v1" AS "yr_v1",
      "yr_t_top_down0_b1"."yr_v2" AS "yr_v2",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b1_bu1" AS "yr_t_top_down0_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b0_bu2" AS "yr_x_top_down0_b0"
      WHERE
        "yr_t_top_down0_b1"."yr_v1" = "yr_x_top_down0_b0"."yr_v1"
    )
    GROUP BY
      "yr_t_top_down0_b1"."yr_v1",
      "yr_t_top_down0_b1"."yr_v2"
  ),
  "yr_f_b2_td1" AS (
    SELECT
      "yr_t_top_down1_b2"."yr_v2" AS "yr_v2",
      "yr_t_top_down1_b2"."yr_v3" AS "yr_v3",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b2_bu0" AS "yr_t_top_down1_b2"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_td0" AS "yr_x_top_down1_b1"
      WHERE
        "yr_t_top_down1_b2"."yr_v2" = "yr_x_top_down1_b1"."yr_v2"
    )
    GROUP BY
      "yr_t_top_down1_b2"."yr_v2",
      "yr_t_top_down1_b2"."yr_v3"
  ),
  "yr_f_b3_td2" AS (
    SELECT
      "yr_t_top_down2_b3"."yr_v3" AS "yr_v3",
      "yr_t_top_down2_b3"."yr_v4" AS "yr_v4",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b3_i" AS "yr_t_top_down2_b3"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_td1" AS "yr_x_top_down2_b2"
      WHERE
        "yr_t_top_down2_b3"."yr_v3" = "yr_x_top_down2_b2"."yr_v3"
    )
    GROUP BY
      "yr_t_top_down2_b3"."yr_v3",
      "yr_t_top_down2_b3"."yr_v4"
  )
SELECT
  "yr_cf_b3"."yr_v4" AS "g1",
  CAST(COALESCE(SUM(("yr_cf_b3"."yr_freq" * "yr_cm_b2"."yr_freq")), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b3_td2" AS "yr_cf_b3"
JOIN (
SELECT
  "yr_cf_b2"."yr_v3" AS "yr_v3",
  COALESCE(SUM(("yr_cf_b2"."yr_freq" * "yr_cm_b1"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b2_td1" AS "yr_cf_b2"
JOIN (
SELECT
  "yr_cf_b1"."yr_v2" AS "yr_v2",
  COALESCE(SUM(("yr_cf_b1"."yr_freq" * "yr_cm_b0"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b1_td0" AS "yr_cf_b1"
JOIN "yr_f_b0_bu2" AS "yr_cm_b0" ON
  "yr_cf_b1"."yr_v1" = "yr_cm_b0"."yr_v1"
GROUP BY
  "yr_cf_b1"."yr_v2"
) AS "yr_cm_b1" ON
  "yr_cf_b2"."yr_v2" = "yr_cm_b1"."yr_v2"
GROUP BY
  "yr_cf_b2"."yr_v3"
) AS "yr_cm_b2" ON
  "yr_cf_b3"."yr_v3" = "yr_cm_b2"."yr_v3"
GROUP BY
  "yr_cf_b3"."yr_v4";