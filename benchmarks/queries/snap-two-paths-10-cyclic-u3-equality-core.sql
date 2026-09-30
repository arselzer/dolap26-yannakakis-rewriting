SELECT e0.dst AS g1, e8.dst AS g2, e2.dst AS g3, COUNT(*) AS witness_count
FROM edge AS e0,
     edge AS e1,
     edge AS e2,
     edge AS e3,
     edge AS e4,
     edge AS e5,
     edge AS e6,
     edge AS e7,
     edge AS e8,
     edge AS e9
WHERE e0.dst = e1.src
  AND e1.dst = e2.src
  AND e2.dst = e3.src
  AND e3.dst = e4.src
  AND e5.dst = e6.src
  AND e6.dst = e7.src
  AND e7.dst = e8.src
  AND e8.dst = e9.src
  AND e0.src = e5.src
  AND e4.dst = e9.dst
GROUP BY e0.dst, e8.dst, e2.dst;
