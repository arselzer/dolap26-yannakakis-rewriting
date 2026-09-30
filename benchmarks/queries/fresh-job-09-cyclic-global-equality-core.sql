SELECT COUNT(*) AS witness_count
FROM aka_name AS an,
     char_name AS chn,
     cast_info AS ci,
     company_name AS cn,
     movie_companies AS mc,
     name AS n,
     role_type AS rt,
     title AS t,
     movie_link AS fx_link,
     title AS fx_title,
     movie_keyword AS fx_left,
     keyword AS fx_lkey,
     movie_keyword AS fx_right,
     keyword AS fx_rkey
WHERE an.person_id = ci.person_id
  AND an.person_id = n.id
  AND chn.id = ci.person_role_id
  AND ci.movie_id = fx_left.movie_id
  AND ci.movie_id = fx_link.movie_id
  AND ci.movie_id = mc.movie_id
  AND ci.movie_id = t.id
  AND ci.role_id = rt.id
  AND cn.id = mc.company_id
  AND fx_left.keyword_id = fx_lkey.id
  AND fx_left.keyword_id = fx_right.keyword_id
  AND fx_left.keyword_id = fx_rkey.id
  AND fx_link.linked_movie_id = fx_right.movie_id
  AND fx_link.linked_movie_id = fx_title.id;
