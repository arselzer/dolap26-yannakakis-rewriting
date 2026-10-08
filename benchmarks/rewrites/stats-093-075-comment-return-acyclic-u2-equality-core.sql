WITH
  "yr_f_b0_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b0_e0"."postid" AS "yr_v0",
      "yr_f_b0_e4"."userid" AS "yr_v1"
    FROM "paper_stats_20260906_a1"."comments" AS "yr_f_b0_e0"
      CROSS JOIN "paper_stats_20260906_a1"."posts" AS "yr_f_b0_e1"
      CROSS JOIN "paper_stats_20260906_a1"."posthistory" AS "yr_f_b0_e2"
      CROSS JOIN "paper_stats_20260906_a1"."votes" AS "yr_f_b0_e3"
      CROSS JOIN "paper_stats_20260906_a1"."badges" AS "yr_f_b0_e4"
      CROSS JOIN "paper_stats_20260906_a1"."users" AS "yr_f_b0_e5"
    WHERE
      "yr_f_b0_e0"."postid" = "yr_f_b0_e0"."postid"
      AND "yr_f_b0_e0"."postid" = "yr_f_b0_e1"."id"
      AND "yr_f_b0_e0"."postid" = "yr_f_b0_e2"."postid"
      AND "yr_f_b0_e0"."postid" = "yr_f_b0_e3"."postid"
      AND "yr_f_b0_e1"."owneruserid" = "yr_f_b0_e1"."owneruserid"
      AND "yr_f_b0_e1"."owneruserid" = "yr_f_b0_e4"."userid"
      AND "yr_f_b0_e1"."owneruserid" = "yr_f_b0_e5"."id"
  ),
  "yr_f_b1_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b1_e6"."postid" AS "yr_v0",
      "yr_f_b1_e6"."relatedpostid" AS "yr_v2"
    FROM "paper_stats_20260906_a1"."postlinks" AS "yr_f_b1_e6"
      CROSS JOIN "paper_stats_20260906_a1"."comments" AS "yr_f_b1_e7"
    WHERE
      "yr_f_b1_e6"."postid" = "yr_f_b1_e6"."postid"
      AND "yr_f_b1_e6"."relatedpostid" = "yr_f_b1_e6"."relatedpostid"
      AND "yr_f_b1_e6"."relatedpostid" = "yr_f_b1_e7"."postid"
  ),
  "yr_f_b2_i" AS NOT MATERIALIZED (
    SELECT
      "yr_f_b2_e8"."postid" AS "yr_v2",
      "yr_f_b2_e9"."id" AS "yr_v3"
    FROM "paper_stats_20260906_a1"."votes" AS "yr_f_b2_e8"
      CROSS JOIN "paper_stats_20260906_a1"."users" AS "yr_f_b2_e9"
    WHERE
      "yr_f_b2_e8"."postid" = "yr_f_b2_e8"."postid"
      AND "yr_f_b2_e8"."userid" = "yr_f_b2_e8"."userid"
      AND "yr_f_b2_e8"."userid" = "yr_f_b2_e9"."id"
  ),
"yr_gc_anchor" AS MATERIALIZED (SELECT r."yr_v0", r."yr_v2", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b1_i" AS r WHERE r."yr_v0" IS NOT NULL AND r."yr_v2" IS NOT NULL GROUP BY r."yr_v0", r."yr_v2"),
"yr_gc_count_0" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v0" FROM "yr_gc_anchor") AS d CROSS JOIN LATERAL (SELECT r."yr_v0", r."yr_v1", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b0_i" AS r WHERE r."yr_v0" = d."yr_v0" GROUP BY r."yr_v0", r."yr_v1") AS z),
"yr_gc_count_2" AS MATERIALIZED (SELECT z.* FROM (SELECT DISTINCT "yr_v2" FROM "yr_gc_anchor") AS d CROSS JOIN LATERAL (SELECT r."yr_v2", r."yr_v3", SUM(CAST(1 AS NUMERIC)) AS yr_freq FROM "yr_f_b2_i" AS r WHERE r."yr_v2" = d."yr_v2" GROUP BY r."yr_v2", r."yr_v3") AS z)
SELECT b0."yr_v1" AS "g1", b2."yr_v3" AS "g2", CAST(SUM(a.yr_freq * b0.yr_freq * b2.yr_freq) AS BIGINT) AS "witness_count" FROM "yr_gc_anchor" AS a JOIN "yr_gc_count_0" AS b0 ON a."yr_v0" = b0."yr_v0" JOIN "yr_gc_count_2" AS b2 ON a."yr_v2" = b2."yr_v2" GROUP BY b0."yr_v1", b2."yr_v3";