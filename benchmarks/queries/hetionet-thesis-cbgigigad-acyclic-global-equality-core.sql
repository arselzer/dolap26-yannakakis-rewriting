SELECT COUNT(*) AS witness_count
FROM compound AS c,
     binds AS b,
     gene AS g1,
     interacts AS int1,
     gene AS g2,
     interacts AS int2,
     gene AS g3,
     associates AS a,
     disease AS d,
     interacts AS int3
WHERE a.sid = d.nid
  AND a.tid = g3.nid
  AND a.tid = int2.sid
  AND b.sid = c.nid
  AND b.tid = g1.nid
  AND b.tid = int1.sid
  AND b.tid = int3.sid
  AND g2.nid = int1.tid
  AND g2.nid = int2.tid;
