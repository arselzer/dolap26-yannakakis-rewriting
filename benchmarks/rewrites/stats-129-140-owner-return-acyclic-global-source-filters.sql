WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."postid" AS "yr_v0",
      "yr_f_b0_e0"."userid" AS "yr_v1"
    FROM "paper_stats_20260906_a1"."comments" AS "yr_f_b0_e0"
      CROSS JOIN "paper_stats_20260906_a1"."posts" AS "yr_f_b0_e1"
      CROSS JOIN "paper_stats_20260906_a1"."posthistory" AS "yr_f_b0_e3"
      CROSS JOIN "paper_stats_20260906_a1"."votes" AS "yr_f_b0_e4"
      CROSS JOIN "paper_stats_20260906_a1"."badges" AS "yr_f_b0_e5"
      CROSS JOIN "paper_stats_20260906_a1"."users" AS "yr_f_b0_e6"
    WHERE
      "yr_f_b0_e0"."postid" = "yr_f_b0_e0"."postid"
      AND "yr_f_b0_e0"."postid" = "yr_f_b0_e1"."id"
      AND "yr_f_b0_e0"."postid" = "yr_f_b0_e3"."postid"
      AND "yr_f_b0_e0"."postid" = "yr_f_b0_e4"."postid"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e0"."userid"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e5"."userid"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e6"."id"
      AND ("yr_f_b0_e0"."score" = 0)
      AND ("yr_f_b0_e1"."viewcount" >= 0)
      AND ("yr_f_b0_e1"."answercount" <= 5)
      AND ("yr_f_b0_e1"."commentcount" <= 12)
      AND ("yr_f_b0_e1"."favoritecount" >= 0)
      AND ("yr_f_b0_e4"."creationdate" >= TIMESTAMP '2010-07-19 00:00:00')
      AND ("yr_f_b0_e4"."creationdate" <= TIMESTAMP '2014-08-31 00:00:00')
      AND ("yr_f_b0_e5"."date" >= TIMESTAMP '2010-08-06 10:36:45')
      AND ("yr_f_b0_e5"."date" <= TIMESTAMP '2014-09-12 07:19:35')
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e2"."relatedpostid" AS "yr_v0",
      "yr_f_b1_e2"."postid" AS "yr_v2"
    FROM "paper_stats_20260906_a1"."postlinks" AS "yr_f_b1_e2"
      CROSS JOIN "paper_stats_20260906_a1"."posts" AS "yr_f_b1_e7"
    WHERE
      "yr_f_b1_e2"."relatedpostid" = "yr_f_b1_e2"."relatedpostid"
      AND "yr_f_b1_e2"."postid" = "yr_f_b1_e2"."postid"
      AND "yr_f_b1_e2"."postid" = "yr_f_b1_e7"."id"
      AND ("yr_f_b1_e2"."linktypeid" = 1)
      AND ("yr_f_b1_e2"."creationdate" >= TIMESTAMP '2011-02-16 20:04:50')
      AND ("yr_f_b1_e2"."creationdate" <= TIMESTAMP '2014-09-01 16:48:04')
  ),
  "yr_f_b2_i" AS (
    SELECT
      "yr_f_b2_e8"."postid" AS "yr_v2",
      "yr_f_b2_e8"."userid" AS "yr_v3"
    FROM "paper_stats_20260906_a1"."comments" AS "yr_f_b2_e8"
      CROSS JOIN "paper_stats_20260906_a1"."users" AS "yr_f_b2_e9"
    WHERE
      "yr_f_b2_e8"."postid" = "yr_f_b2_e8"."postid"
      AND "yr_f_b2_e8"."userid" = "yr_f_b2_e8"."userid"
      AND "yr_f_b2_e8"."userid" = "yr_f_b2_e9"."id"
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
  CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
FROM "yr_f_b2_i" AS "yr_cf_b2"
GROUP BY
  "yr_cf_b2"."yr_v2"
) AS "yr_cm_b2" ON
  "yr_cf_b1"."yr_v2" = "yr_cm_b2"."yr_v2"
GROUP BY
  "yr_cf_b1"."yr_v0"
) AS "yr_cm_b1" ON
  "yr_cf_b0"."yr_v0" = "yr_cm_b1"."yr_v0";