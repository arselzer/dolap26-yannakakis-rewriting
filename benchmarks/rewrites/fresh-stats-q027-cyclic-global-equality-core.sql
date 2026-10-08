WITH
  "yr_f_b0_i" AS (
    SELECT
      CAST(1 AS SMALLINT) AS "yr_unit_b0"
    FROM "paper_stats_20260906_a1"."comments" AS "yr_f_b0_e0",
      "paper_stats_20260906_a1"."posts" AS "yr_f_b0_e1",
      "paper_stats_20260906_a1"."users" AS "yr_f_b0_e2",
      "paper_stats_20260906_a1"."votes" AS "yr_f_b0_e3",
      "paper_stats_20260906_a1"."postlinks" AS "yr_f_b0_e4",
      "paper_stats_20260906_a1"."posts" AS "yr_f_b0_e5",
      "paper_stats_20260906_a1"."comments" AS "yr_f_b0_e6",
      "paper_stats_20260906_a1"."users" AS "yr_f_b0_e7",
      "paper_stats_20260906_a1"."votes" AS "yr_f_b0_e8",
      "paper_stats_20260906_a1"."users" AS "yr_f_b0_e9"
    WHERE
      "yr_f_b0_e0"."userid" = "yr_f_b0_e0"."userid"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e1"."owneruserid"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e2"."id"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e3"."userid"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e6"."userid"
      AND "yr_f_b0_e0"."userid" = "yr_f_b0_e7"."id"
      AND "yr_f_b0_e1"."id" = "yr_f_b0_e1"."id"
      AND "yr_f_b0_e1"."id" = "yr_f_b0_e4"."postid"
      AND "yr_f_b0_e4"."relatedpostid" = "yr_f_b0_e4"."relatedpostid"
      AND "yr_f_b0_e4"."relatedpostid" = "yr_f_b0_e5"."id"
      AND "yr_f_b0_e4"."relatedpostid" = "yr_f_b0_e6"."postid"
      AND "yr_f_b0_e4"."relatedpostid" = "yr_f_b0_e8"."postid"
      AND "yr_f_b0_e8"."userid" = "yr_f_b0_e8"."userid"
      AND "yr_f_b0_e8"."userid" = "yr_f_b0_e9"."id"
  )
SELECT
  COUNT(*) AS "witness_count"
FROM "yr_f_b0_i" AS "yr_cf_b0";