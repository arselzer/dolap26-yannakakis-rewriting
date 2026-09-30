SELECT COUNT(*) AS witness_count
FROM company_name AS cn,
     company_type AS ct,
     info_type AS it,
     info_type AS it2,
     kind_type AS kt,
     movie_companies AS mc,
     movie_info AS mi,
     movie_info_idx AS miidx,
     title AS t,
     movie_link AS fx_link,
     title AS fx_title,
     movie_keyword AS fx_left,
     keyword AS fx_lkey,
     movie_keyword AS fx_right,
     keyword AS fx_rkey
WHERE cn.id = mc.company_id
  AND ct.id = mc.company_type_id
  AND fx_left.keyword_id = fx_lkey.id
  AND fx_left.keyword_id = fx_right.keyword_id
  AND fx_left.keyword_id = fx_rkey.id
  AND fx_left.movie_id = fx_link.movie_id
  AND fx_left.movie_id = mc.movie_id
  AND fx_left.movie_id = mi.movie_id
  AND fx_left.movie_id = miidx.movie_id
  AND fx_left.movie_id = t.id
  AND fx_link.linked_movie_id = fx_right.movie_id
  AND fx_link.linked_movie_id = fx_title.id
  AND it.id = miidx.info_type_id
  AND it2.id = mi.info_type_id
  AND kt.id = t.kind_id;
