WITH
"yr_cc_0" AS MATERIALIZED (
    SELECT "yr_cc_e0"."movie_id" AS "yr_v0", "yr_cc_e6"."id" AS "yr_v6", SUM(CAST(1 AS NUMERIC)) AS yr_freq
    FROM "public"."complete_cast" AS "yr_cc_e0"
      CROSS JOIN "public"."comp_cast_type" AS "yr_cc_e1"
      CROSS JOIN "public"."comp_cast_type" AS "yr_cc_e2"
      CROSS JOIN "public"."title" AS "yr_cc_e11"
      CROSS JOIN "public"."movie_info" AS "yr_cc_e7"
      CROSS JOIN "public"."info_type" AS "yr_cc_e4"
      CROSS JOIN "public"."movie_info_idx" AS "yr_cc_e8"
      CROSS JOIN "public"."info_type" AS "yr_cc_e5"
      CROSS JOIN "public"."movie_keyword" AS "yr_cc_e9"
      CROSS JOIN "public"."keyword" AS "yr_cc_e6"
      CROSS JOIN "public"."movie_link" AS "yr_cc_e12"
      CROSS JOIN "public"."movie_keyword" AS "yr_cc_e13"
    WHERE "yr_cc_e0"."movie_id" = "yr_cc_e0"."movie_id"
      AND "yr_cc_e0"."movie_id" = "yr_cc_e11"."id"
      AND "yr_cc_e0"."movie_id" = "yr_cc_e7"."movie_id"
      AND "yr_cc_e0"."movie_id" = "yr_cc_e8"."movie_id"
      AND "yr_cc_e0"."movie_id" = "yr_cc_e9"."movie_id"
      AND "yr_cc_e0"."movie_id" = "yr_cc_e12"."movie_id"
      AND "yr_cc_e0"."subject_id" = "yr_cc_e0"."subject_id"
      AND "yr_cc_e0"."subject_id" = "yr_cc_e1"."id"
      AND "yr_cc_e0"."status_id" = "yr_cc_e0"."status_id"
      AND "yr_cc_e0"."status_id" = "yr_cc_e2"."id"
      AND "yr_cc_e7"."info_type_id" = "yr_cc_e7"."info_type_id"
      AND "yr_cc_e7"."info_type_id" = "yr_cc_e4"."id"
      AND "yr_cc_e8"."info_type_id" = "yr_cc_e8"."info_type_id"
      AND "yr_cc_e8"."info_type_id" = "yr_cc_e5"."id"
      AND "yr_cc_e9"."keyword_id" = "yr_cc_e9"."keyword_id"
      AND "yr_cc_e9"."keyword_id" = "yr_cc_e6"."id"
      AND "yr_cc_e9"."keyword_id" = "yr_cc_e13"."keyword_id"
      AND "yr_cc_e12"."linked_movie_id" = "yr_cc_e12"."linked_movie_id"
      AND "yr_cc_e12"."linked_movie_id" = "yr_cc_e13"."movie_id"
      AND (("yr_cc_e1"."kind" = 'cast') OR ("yr_cc_e1"."kind" = 'crew'))
      AND ("yr_cc_e2"."kind" = 'complete+verified')
      AND ("yr_cc_e11"."production_year" > 2000)
      AND (("yr_cc_e7"."info" = 'Horror') OR ("yr_cc_e7"."info" = 'Thriller'))
      AND ("yr_cc_e4"."info" = 'genres')
      AND ("yr_cc_e5"."info" = 'votes')
      AND (("yr_cc_e6"."keyword" = 'blood') OR ("yr_cc_e6"."keyword" = 'death') OR ("yr_cc_e6"."keyword" = 'female-nudity') OR ("yr_cc_e6"."keyword" = 'gore') OR ("yr_cc_e6"."keyword" = 'hospital') OR ("yr_cc_e6"."keyword" = 'murder') OR ("yr_cc_e6"."keyword" = 'violence'))
    GROUP BY "yr_cc_e0"."movie_id", "yr_cc_e6"."id"
  ),
"yr_cc_1" AS MATERIALIZED (
    SELECT "yr_cc_e3"."movie_id" AS "yr_v0", "yr_cc_e3"."person_id" AS "yr_v3", SUM(CAST(1 AS NUMERIC)) AS yr_freq
    FROM "public"."cast_info" AS "yr_cc_e3"
      CROSS JOIN "public"."name" AS "yr_cc_e10"
    WHERE "yr_cc_e3"."movie_id" = "yr_cc_e3"."movie_id"
      AND "yr_cc_e3"."person_id" = "yr_cc_e3"."person_id"
      AND "yr_cc_e3"."person_id" = "yr_cc_e10"."id"
      AND (("yr_cc_e3"."note" = '(head writer)') OR ("yr_cc_e3"."note" = '(story editor)') OR ("yr_cc_e3"."note" = '(story)') OR ("yr_cc_e3"."note" = '(writer)') OR ("yr_cc_e3"."note" = '(written by)'))
      AND ("yr_cc_e10"."gender" = 'm')
      AND EXISTS (SELECT 1 FROM "yr_cc_0" AS yr_cc_domain WHERE "yr_cc_e3"."movie_id" = "yr_cc_domain"."yr_v0")
    GROUP BY "yr_cc_e3"."movie_id", "yr_cc_e3"."person_id"
  )
SELECT p1."yr_v3" AS "g1", p0."yr_v6" AS "g2", CAST(SUM(p0.yr_freq * p1.yr_freq) AS BIGINT) AS "witness_count" FROM "yr_cc_0" AS p0 JOIN "yr_cc_1" AS p1 ON p0."yr_v0" = p1."yr_v0" GROUP BY p1."yr_v3", p0."yr_v6";