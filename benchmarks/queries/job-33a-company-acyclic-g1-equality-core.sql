SELECT ca3.person_id AS g1, COUNT(*) AS witness_count
FROM company_name AS cn1,
     company_name AS cn2,
     info_type AS it1,
     info_type AS it2,
     kind_type AS kt1,
     kind_type AS kt2,
     link_type AS lt,
     movie_companies AS mc1,
     movie_companies AS mc2,
     movie_info_idx AS mi_idx1,
     movie_info_idx AS mi_idx2,
     movie_link AS ml,
     title AS t1,
     title AS t2,
     cast_info AS ca3,
     name AS n3,
     movie_keyword AS mk3,
     keyword AS k3
WHERE ca3.movie_id = mc1.movie_id
  AND ca3.movie_id = mi_idx1.movie_id
  AND ca3.movie_id = ml.movie_id
  AND ca3.movie_id = t1.id
  AND ca3.person_id = n3.id
  AND cn1.id = mc1.company_id
  AND cn2.id = mc2.company_id
  AND it1.id = mi_idx1.info_type_id
  AND it2.id = mi_idx2.info_type_id
  AND k3.id = mk3.keyword_id
  AND kt1.id = t1.kind_id
  AND kt2.id = t2.kind_id
  AND lt.id = ml.link_type_id
  AND mc2.movie_id = mi_idx2.movie_id
  AND mc2.movie_id = mk3.movie_id
  AND mc2.movie_id = ml.linked_movie_id
  AND mc2.movie_id = t2.id
GROUP BY ca3.person_id;
