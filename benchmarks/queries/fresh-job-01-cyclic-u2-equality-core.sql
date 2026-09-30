SELECT ct.id AS g1, fx_left.keyword_id AS g2, COUNT(*) AS witness_count
FROM company_type AS ct,
     info_type AS it,
     movie_companies AS mc,
     movie_info_idx AS mi_idx,
     title AS t,
     movie_link AS fx_link,
     title AS fx_title,
     movie_keyword AS fx_left,
     keyword AS fx_lkey,
     movie_keyword AS fx_right,
     keyword AS fx_rkey
WHERE ct.id = mc.company_type_id
  AND fx_left.keyword_id = fx_lkey.id
  AND fx_left.keyword_id = fx_right.keyword_id
  AND fx_left.keyword_id = fx_rkey.id
  AND fx_left.movie_id = fx_link.movie_id
  AND fx_left.movie_id = mc.movie_id
  AND fx_left.movie_id = mi_idx.movie_id
  AND fx_left.movie_id = t.id
  AND fx_link.linked_movie_id = fx_right.movie_id
  AND fx_link.linked_movie_id = fx_title.id
  AND it.id = mi_idx.info_type_id
GROUP BY ct.id, fx_left.keyword_id;
