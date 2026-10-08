WITH
"yr_cc_0" AS MATERIALIZED (
    SELECT "yr_cc_e11"."movie_id" AS "yr_v6", "yr_cc_e11"."linked_movie_id" AS "yr_v7", SUM(CAST(1 AS NUMERIC)) AS yr_freq
    FROM "public"."movie_link" AS "yr_cc_e11"
      CROSS JOIN "public"."link_type" AS "yr_cc_e6"
      CROSS JOIN "public"."movie_companies" AS "yr_cc_e7"
      CROSS JOIN "public"."movie_companies" AS "yr_cc_e8"
      CROSS JOIN "public"."company_name" AS "yr_cc_e0"
      CROSS JOIN "public"."company_name" AS "yr_cc_e1"
      CROSS JOIN "public"."movie_info_idx" AS "yr_cc_e9"
      CROSS JOIN "public"."info_type" AS "yr_cc_e2"
      CROSS JOIN "public"."movie_info_idx" AS "yr_cc_e10"
      CROSS JOIN "public"."info_type" AS "yr_cc_e3"
      CROSS JOIN "public"."title" AS "yr_cc_e12"
      CROSS JOIN "public"."kind_type" AS "yr_cc_e4"
      CROSS JOIN "public"."title" AS "yr_cc_e13"
      CROSS JOIN "public"."kind_type" AS "yr_cc_e5"
    WHERE "yr_cc_e7"."company_id" = "yr_cc_e7"."company_id"
      AND "yr_cc_e7"."company_id" = "yr_cc_e8"."company_id"
      AND "yr_cc_e7"."company_id" = "yr_cc_e0"."id"
      AND "yr_cc_e7"."company_id" = "yr_cc_e1"."id"
      AND "yr_cc_e9"."info_type_id" = "yr_cc_e9"."info_type_id"
      AND "yr_cc_e9"."info_type_id" = "yr_cc_e2"."id"
      AND "yr_cc_e10"."info_type_id" = "yr_cc_e10"."info_type_id"
      AND "yr_cc_e10"."info_type_id" = "yr_cc_e3"."id"
      AND "yr_cc_e12"."kind_id" = "yr_cc_e12"."kind_id"
      AND "yr_cc_e12"."kind_id" = "yr_cc_e4"."id"
      AND "yr_cc_e13"."kind_id" = "yr_cc_e13"."kind_id"
      AND "yr_cc_e13"."kind_id" = "yr_cc_e5"."id"
      AND "yr_cc_e11"."link_type_id" = "yr_cc_e11"."link_type_id"
      AND "yr_cc_e11"."link_type_id" = "yr_cc_e6"."id"
      AND "yr_cc_e11"."movie_id" = "yr_cc_e11"."movie_id"
      AND "yr_cc_e11"."movie_id" = "yr_cc_e7"."movie_id"
      AND "yr_cc_e11"."movie_id" = "yr_cc_e9"."movie_id"
      AND "yr_cc_e11"."movie_id" = "yr_cc_e12"."id"
      AND "yr_cc_e11"."linked_movie_id" = "yr_cc_e11"."linked_movie_id"
      AND "yr_cc_e11"."linked_movie_id" = "yr_cc_e8"."movie_id"
      AND "yr_cc_e11"."linked_movie_id" = "yr_cc_e10"."movie_id"
      AND "yr_cc_e11"."linked_movie_id" = "yr_cc_e13"."id"
      AND (("yr_cc_e6"."link" = 'followed by') OR ("yr_cc_e6"."link" = 'follows') OR ("yr_cc_e6"."link" = 'sequel'))
      AND ("yr_cc_e0"."country_code" = '[us]')
      AND ("yr_cc_e2"."info" = 'rating')
      AND ("yr_cc_e10"."info" < '3.0')
      AND ("yr_cc_e3"."info" = 'rating')
      AND ("yr_cc_e4"."kind" = 'tv series')
      AND ("yr_cc_e13"."production_year" >= 2005)
      AND ("yr_cc_e13"."production_year" <= 2008)
      AND ("yr_cc_e5"."kind" = 'tv series')
    GROUP BY "yr_cc_e11"."movie_id", "yr_cc_e11"."linked_movie_id"
  ),
"yr_cc_1" AS MATERIALIZED (
    SELECT "yr_cc_e14"."movie_id" AS "yr_v6", "yr_cc_e14"."person_id" AS "yr_v8", SUM(CAST(1 AS NUMERIC)) AS yr_freq
    FROM "public"."cast_info" AS "yr_cc_e14"
      CROSS JOIN "public"."name" AS "yr_cc_e15"
    WHERE "yr_cc_e14"."movie_id" = "yr_cc_e14"."movie_id"
      AND "yr_cc_e14"."person_id" = "yr_cc_e14"."person_id"
      AND "yr_cc_e14"."person_id" = "yr_cc_e15"."id"
      AND EXISTS (SELECT 1 FROM "yr_cc_0" AS yr_cc_domain WHERE "yr_cc_e14"."movie_id" = "yr_cc_domain"."yr_v6")
    GROUP BY "yr_cc_e14"."movie_id", "yr_cc_e14"."person_id"
  ),
"yr_cc_2" AS MATERIALIZED (
    SELECT "yr_cc_e16"."movie_id" AS "yr_v7", "yr_cc_e17"."id" AS "yr_v9", SUM(CAST(1 AS NUMERIC)) AS yr_freq
    FROM "public"."movie_keyword" AS "yr_cc_e16"
      CROSS JOIN "public"."keyword" AS "yr_cc_e17"
    WHERE "yr_cc_e16"."movie_id" = "yr_cc_e16"."movie_id"
      AND "yr_cc_e16"."keyword_id" = "yr_cc_e16"."keyword_id"
      AND "yr_cc_e16"."keyword_id" = "yr_cc_e17"."id"
      AND EXISTS (SELECT 1 FROM "yr_cc_0" AS yr_cc_domain WHERE "yr_cc_e16"."movie_id" = "yr_cc_domain"."yr_v7")
    GROUP BY "yr_cc_e16"."movie_id", "yr_cc_e17"."id"
  )
SELECT p1."yr_v8" AS "g1", p2."yr_v9" AS "g2", CAST(SUM(p0.yr_freq * p1.yr_freq * p2.yr_freq) AS BIGINT) AS "witness_count" FROM "yr_cc_0" AS p0 JOIN "yr_cc_1" AS p1 ON p0."yr_v6" = p1."yr_v6" JOIN "yr_cc_2" AS p2 ON p0."yr_v7" = p2."yr_v7" GROUP BY p1."yr_v8", p2."yr_v9";