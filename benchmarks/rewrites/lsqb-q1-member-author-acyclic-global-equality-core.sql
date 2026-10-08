WITH
  "yr_f_b0_i" AS (
    SELECT
      "yr_f_b0_e0"."countryid" AS "yr_v0",
      "yr_f_b0_e1"."cityid" AS "yr_v1"
    FROM "real_lsqb_sf10_extended_a1"."country" AS "yr_f_b0_e0"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."city" AS "yr_f_b0_e1"
    WHERE
      "yr_f_b0_e0"."countryid" = "yr_f_b0_e0"."countryid"
      AND "yr_f_b0_e0"."countryid" = "yr_f_b0_e1"."ispartof_countryid"
      AND "yr_f_b0_e1"."cityid" = "yr_f_b0_e1"."cityid"
  ),
  "yr_f_b1_i" AS (
    SELECT
      "yr_f_b1_e2"."islocatedin_cityid" AS "yr_v1",
      "yr_f_b1_e2"."personid" AS "yr_v2"
    FROM "real_lsqb_sf10_extended_a1"."person" AS "yr_f_b1_e2"
    WHERE
      "yr_f_b1_e2"."islocatedin_cityid" = "yr_f_b1_e2"."islocatedin_cityid"
      AND "yr_f_b1_e2"."personid" = "yr_f_b1_e2"."personid"
  ),
  "yr_f_b2_i" AS (
    SELECT
      "yr_f_b2_e3"."personid" AS "yr_v2",
      "yr_f_b2_e3"."forumid" AS "yr_v3"
    FROM "real_lsqb_sf10_extended_a1"."forum_hasmember_person" AS "yr_f_b2_e3"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."forum" AS "yr_f_b2_e4"
    WHERE
      "yr_f_b2_e3"."personid" = "yr_f_b2_e3"."personid"
      AND "yr_f_b2_e3"."forumid" = "yr_f_b2_e3"."forumid"
      AND "yr_f_b2_e3"."forumid" = "yr_f_b2_e4"."forumid"
  ),
  "yr_f_b3_i" AS (
    SELECT
      "yr_f_b3_e5"."forum_containerofid" AS "yr_v3",
      "yr_f_b3_e5"."postid" AS "yr_v4"
    FROM "real_lsqb_sf10_extended_a1"."post" AS "yr_f_b3_e5"
    WHERE
      "yr_f_b3_e5"."forum_containerofid" = "yr_f_b3_e5"."forum_containerofid"
      AND "yr_f_b3_e5"."postid" = "yr_f_b3_e5"."postid"
  ),
  "yr_f_b4_i" AS (
    SELECT
      "yr_f_b4_e6"."replyof_postid" AS "yr_v4",
      "yr_f_b4_e6"."commentid" AS "yr_v5"
    FROM "real_lsqb_sf10_extended_a1"."comment" AS "yr_f_b4_e6"
    WHERE
      "yr_f_b4_e6"."replyof_postid" = "yr_f_b4_e6"."replyof_postid"
      AND "yr_f_b4_e6"."commentid" = "yr_f_b4_e6"."commentid"
  ),
  "yr_f_b5_i" AS (
    SELECT
      "yr_f_b5_e7"."commentid" AS "yr_v5",
      "yr_f_b5_e7"."tagid" AS "yr_v6"
    FROM "real_lsqb_sf10_extended_a1"."comment_hastag_tag" AS "yr_f_b5_e7"
    WHERE
      "yr_f_b5_e7"."commentid" = "yr_f_b5_e7"."commentid"
      AND "yr_f_b5_e7"."tagid" = "yr_f_b5_e7"."tagid"
  ),
  "yr_f_b6_i" AS (
    SELECT
      "yr_f_b6_e8"."tagid" AS "yr_v6",
      "yr_f_b6_e8"."hastype_tagclassid" AS "yr_v7"
    FROM "real_lsqb_sf10_extended_a1"."tag" AS "yr_f_b6_e8"
      CROSS JOIN "real_lsqb_sf10_extended_a1"."tagclass" AS "yr_f_b6_e9"
    WHERE
      "yr_f_b6_e8"."tagid" = "yr_f_b6_e8"."tagid"
      AND "yr_f_b6_e8"."hastype_tagclassid" = "yr_f_b6_e8"."hastype_tagclassid"
      AND "yr_f_b6_e8"."hastype_tagclassid" = "yr_f_b6_e9"."tagclassid"
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
  COALESCE(SUM("yr_cm_b4"."yr_freq"), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b3_i" AS "yr_cf_b3"
JOIN (
SELECT
  "yr_cf_b4"."yr_v4" AS "yr_v4",
  COALESCE(SUM("yr_cm_b5"."yr_freq"), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b4_i" AS "yr_cf_b4"
JOIN (
SELECT
  "yr_cf_b5"."yr_v5" AS "yr_v5",
  COALESCE(SUM("yr_cm_b6"."yr_freq"), CAST(0 AS NUMERIC)) AS "yr_freq"
FROM "yr_f_b5_i" AS "yr_cf_b5"
JOIN (
SELECT
  "yr_cf_b6"."yr_v6" AS "yr_v6",
  CAST(COUNT(*) AS NUMERIC) AS "yr_freq"
FROM "yr_f_b6_i" AS "yr_cf_b6"
GROUP BY
  "yr_cf_b6"."yr_v6"
) AS "yr_cm_b6" ON
  "yr_cf_b5"."yr_v6" = "yr_cm_b6"."yr_v6"
GROUP BY
  "yr_cf_b5"."yr_v5"
) AS "yr_cm_b5" ON
  "yr_cf_b4"."yr_v5" = "yr_cm_b5"."yr_v5"
GROUP BY
  "yr_cf_b4"."yr_v4"
) AS "yr_cm_b4" ON
  "yr_cf_b3"."yr_v4" = "yr_cm_b4"."yr_v4"
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