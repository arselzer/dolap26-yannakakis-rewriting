SELECT an.person_id AS g1, COUNT(*) AS witness_count
FROM aka_name AS an,
     complete_cast AS cc,
     comp_cast_type AS cct1,
     comp_cast_type AS cct2,
     char_name AS chn,
     cast_info AS ci,
     company_name AS cn,
     info_type AS it,
     info_type AS it3,
     keyword AS k,
     movie_companies AS mc,
     movie_info AS mi,
     movie_keyword AS mk,
     name AS n,
     person_info AS pi,
     role_type AS rt,
     title AS t,
     movie_link AS ml2,
     cast_info AS ci2
WHERE an.person_id = ci.person_id
  AND an.person_id = n.id
  AND an.person_id = pi.person_id
  AND cc.movie_id = ci.movie_id
  AND cc.movie_id = mc.movie_id
  AND cc.movie_id = mi.movie_id
  AND cc.movie_id = mk.movie_id
  AND cc.movie_id = ml2.movie_id
  AND cc.movie_id = t.id
  AND cc.status_id = cct2.id
  AND cc.subject_id = cct1.id
  AND chn.id = ci.person_role_id
  AND ci.role_id = rt.id
  AND ci2.movie_id = ml2.linked_movie_id
  AND cn.id = mc.company_id
  AND it.id = mi.info_type_id
  AND it3.id = pi.info_type_id
  AND k.id = mk.keyword_id
  AND (cct1.kind ='cast'
  AND cct2.kind ='complete+verified'
  AND chn.name = 'Queen'
  AND ci.note IN ('(voice)',
                  '(voice) (uncredited)',
                  '(voice: English version)')
  AND cn.country_code ='[us]'
  AND it.info = 'release dates'
  AND it3.info = 'trivia'
  AND k.keyword = 'computer-animation'
  AND mi.info IS NOT NULL
  AND (mi.info LIKE 'Japan:%200%'
       OR mi.info LIKE 'USA:%200%')
  AND n.gender ='f'
  AND n.name LIKE '%An%'
  AND rt.role ='actress'
  AND t.title = 'Shrek 2'
  AND t.production_year BETWEEN 2000 AND 2010)
GROUP BY an.person_id;
