SELECT e0.dst AS g1, e4.dst AS g2, e1.dst AS g3, COUNT(*) AS witness_count
FROM edge AS e0,
     edge AS e1,
     edge AS e2,
     edge AS e3,
     edge AS e4,
     edge AS e5
WHERE e0.dst = e1.src
  AND e1.dst = e2.src
  AND e3.dst = e4.src
  AND e4.dst = e5.src
  AND e0.src = e3.src
  AND e2.dst = e5.dst
GROUP BY e0.dst, e4.dst, e1.dst;
