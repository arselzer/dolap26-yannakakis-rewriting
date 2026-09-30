SELECT ci.person_id AS g1, cn.id AS g2, COUNT(*) AS witness_count
FROM cast_info AS ci,
     company_name AS cn,
     info_type AS it1,
     info_type AS it2,
     keyword AS k,
     movie_companies AS mc,
     movie_info AS mi,
     movie_info_idx AS mi_idx,
     movie_keyword AS mk,
     name AS n,
     title AS t,
     movie_link AS fx_link,
     title AS fx_title,
     movie_keyword AS fx_left,
     keyword AS fx_lkey,
     movie_keyword AS fx_right,
     keyword AS fx_rkey
WHERE ci.movie_id = fx_left.movie_id
  AND ci.movie_id = fx_link.movie_id
  AND ci.movie_id = mc.movie_id
  AND ci.movie_id = mi.movie_id
  AND ci.movie_id = mi_idx.movie_id
  AND ci.movie_id = mk.movie_id
  AND ci.movie_id = t.id
  AND ci.person_id = n.id
  AND cn.id = mc.company_id
  AND fx_left.keyword_id = fx_lkey.id
  AND fx_left.keyword_id = fx_right.keyword_id
  AND fx_left.keyword_id = fx_rkey.id
  AND fx_link.linked_movie_id = fx_right.movie_id
  AND fx_link.linked_movie_id = fx_title.id
  AND it1.id = mi.info_type_id
  AND it2.id = mi_idx.info_type_id
  AND k.id = mk.keyword_id
GROUP BY ci.person_id, cn.id;
