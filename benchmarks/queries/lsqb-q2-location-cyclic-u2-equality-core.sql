SELECT c.replyof_postid AS g1, ca.ispartof_countryid AS g2, COUNT(*) AS witness_count
FROM person_knows_person AS k,
     comment AS c,
     post AS p,
     person AS pa,
     city AS ca,
     country AS co
WHERE c.hascreator_personid = k.person1id
  AND c.hascreator_personid = pa.personid
  AND c.replyof_postid = p.postid
  AND ca.cityid = pa.islocatedin_cityid
  AND ca.ispartof_countryid = co.countryid
  AND k.person2id = p.hascreator_personid
GROUP BY c.replyof_postid, ca.ispartof_countryid;
