SELECT ci.person_id AS g1, k.id AS g2, COUNT(*) AS witness_count
FROM complete_cast AS cc,
     comp_cast_type AS cct1,
     comp_cast_type AS cct2,
     cast_info AS ci,
     info_type AS it1,
     info_type AS it2,
     keyword AS k,
     movie_info AS mi,
     movie_info_idx AS mi_idx,
     movie_keyword AS mk,
     name AS n,
     title AS t,
     movie_link AS ml2,
     movie_keyword AS mk2
WHERE cc.movie_id = ci.movie_id
  AND cc.movie_id = mi.movie_id
  AND cc.movie_id = mi_idx.movie_id
  AND cc.movie_id = mk.movie_id
  AND cc.movie_id = ml2.movie_id
  AND cc.movie_id = t.id
  AND cc.status_id = cct2.id
  AND cc.subject_id = cct1.id
  AND ci.person_id = n.id
  AND it1.id = mi.info_type_id
  AND it2.id = mi_idx.info_type_id
  AND k.id = mk.keyword_id
  AND mk2.movie_id = ml2.linked_movie_id
GROUP BY ci.person_id, k.id;
