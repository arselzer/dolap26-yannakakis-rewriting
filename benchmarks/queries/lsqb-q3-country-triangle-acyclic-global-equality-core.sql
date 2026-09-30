SELECT COUNT(*) AS witness_count
FROM city AS citya,
     city AS cityb,
     city AS cityc,
     person AS persona,
     person AS personb,
     person AS personc,
     person_knows_person AS pkp1,
     person_knows_person AS pkp2,
     person_knows_person AS pkp3
WHERE citya.ispartof_countryid = cityc.ispartof_countryid
  AND cityb.cityid = personb.islocatedin_cityid
  AND cityc.cityid = personc.islocatedin_cityid
  AND persona.personid = pkp3.person2id
  AND personb.personid = pkp1.person2id
  AND personb.personid = pkp2.person1id
  AND personc.personid = pkp2.person2id
  AND personc.personid = pkp3.person1id;
