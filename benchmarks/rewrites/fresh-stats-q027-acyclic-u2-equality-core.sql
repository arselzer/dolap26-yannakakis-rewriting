WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e1"."id" AS "yr_v1"
    FROM "paper_stats_20260906_a1"."comments" AS "yr_f_b0_e0"
      CROSS JOIN "paper_stats_20260906_a1"."posts" AS "yr_f_b0_e1"
      CROSS JOIN "paper_stats_20260906_a1"."users" AS "yr_f_b0_e2"
      CROSS JOIN "paper_stats_20260906_a1"."votes" AS "yr_f_b0_e3"
    WHERE
      "yr_f_b0_e0"."userid" = "yr_f_b0_e0"."userid"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e1"."owneruserid"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e2"."id"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e3"."userid"
      AND "yr_f_b0_e1"."id" = "yr_f_b0_e1"."id"
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e4"."postid" AS "yr_v1",
      "yr_f_b1_e4"."relatedpostid" AS "yr_v2"
    FROM "paper_stats_20260906_a1"."postlinks" AS "yr_f_b1_e4"
      CROSS JOIN "paper_stats_20260906_a1"."posts" AS "yr_f_b1_e5"
    WHERE
      "yr_f_b1_e4"."postid" = "yr_f_b1_e4"."postid"
      AND "yr_f_b1_e4"."relatedpostid" = "yr_f_b1_e4"."relatedpostid"
      AND "yr_f_b1_e4"."relatedpostid" = "yr_f_b1_e5"."id"
  ),
  "yr_f_b2_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b2_e6"."postid" AS "yr_v2",
      "yr_f_b2_e6"."userid" AS "yr_v3"
    FROM "paper_stats_20260906_a1"."comments" AS "yr_f_b2_e6"
      CROSS JOIN "paper_stats_20260906_a1"."users" AS "yr_f_b2_e7"
    WHERE
      "yr_f_b2_e6"."postid" = "yr_f_b2_e6"."postid"
      AND "yr_f_b2_e6"."userid" = "yr_f_b2_e6"."userid"
      AND "yr_f_b2_e6"."userid" = "yr_f_b2_e7"."id"
  ),
  "yr_f_b3_i" AS (
    SELECT
      "yr_f_b3_e8"."postid" AS "yr_v2",
      "yr_f_b3_e8"."userid" AS "yr_v4"
    FROM "paper_stats_20260906_a1"."votes" AS "yr_f_b3_e8"
      CROSS JOIN "paper_stats_20260906_a1"."users" AS "yr_f_b3_e9"
    WHERE
      "yr_f_b3_e8"."postid" = "yr_f_b3_e8"."postid"
      AND "yr_f_b3_e8"."userid" = "yr_f_b3_e8"."userid"
      AND "yr_f_b3_e8"."userid" = "yr_f_b3_e9"."id"
  ),
  "yr_f_b1_bu1" AS (
    SELECT "yr_t_bottom_up1_b1".*
    FROM "yr_f_b1_i" AS "yr_t_bottom_up1_b1"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b2_i" AS "yr_x_bottom_up0_b2"
      WHERE
        "yr_t_bottom_up1_b1"."yr_v2" = "yr_x_bottom_up0_b2"."yr_v2"
    )
      AND EXISTS (
      SELECT 1
      FROM "yr_f_b3_i" AS "yr_x_bottom_up1_b3"
      WHERE
        "yr_t_bottom_up1_b1"."yr_v2" = "yr_x_bottom_up1_b3"."yr_v2"
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
    FROM "yr_f_b2_i" AS "yr_t_top_down1_b2"
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
      "yr_t_top_down2_b3"."yr_v2" AS "yr_v2",
      "yr_t_top_down2_b3"."yr_v4" AS "yr_v4",
      CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
    FROM "yr_f_b3_i" AS "yr_t_top_down2_b3"
    WHERE EXISTS (
      SELECT 1
      FROM "yr_f_b1_td0" AS "yr_x_top_down2_b1"
      WHERE
        "yr_t_top_down2_b3"."yr_v2" = "yr_x_top_down2_b1"."yr_v2"
    )
    GROUP BY
      "yr_t_top_down2_b3"."yr_v2",
      "yr_t_top_down2_b3"."yr_v4"
  )
SELECT
  "yr_cf_b2"."yr_v3" AS "g1",
  "yr_cm_b1"."yr_v4" AS "g2",
  CAST(COALESCE(SUM(("yr_cf_b2"."yr_freq" * "yr_cm_b1"."yr_freq")), CAST(0 AS NUMERIC)) AS BIGINT) AS "witness_count"
FROM "yr_f_b2_td1" AS "yr_cf_b2"
JOIN (
SELECT
  "yr_cf_b1"."yr_v2" AS "yr_v2",
  "yr_cm_b3"."yr_v4" AS "yr_v4",
  COALESCE(SUM(("yr_cf_b1"."yr_freq" * "yr_cm_b0"."yr_freq" * "yr_cm_b3"."yr_freq")), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b1_td0" AS "yr_cf_b1"
JOIN "yr_f_b0_bu2" AS "yr_cm_b0" ON
  "yr_cf_b1"."yr_v1" = "yr_cm_b0"."yr_v1"
JOIN "yr_f_b3_td2" AS "yr_cm_b3" ON
  "yr_cf_b1"."yr_v2" = "yr_cm_b3"."yr_v2"
GROUP BY
  "yr_cf_b1"."yr_v2",
  "yr_cm_b3"."yr_v4"
) AS "yr_cm_b1" ON
  "yr_cf_b2"."yr_v2" = "yr_cm_b1"."yr_v2"
GROUP BY
  "yr_cf_b2"."yr_v3",
  "yr_cm_b1"."yr_v4";