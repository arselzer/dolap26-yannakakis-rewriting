SELECT hetio45159_6.s AS g1, hetio45173_0.s AS g2, hetio45159_5.s AS g3, COUNT(*) AS witness_count
FROM hetio45173 AS hetio45173_0,
     hetio45173 AS hetio45173_1,
     hetio45160 AS hetio45160_2,
     hetio45160 AS hetio45160_3,
     hetio45160 AS hetio45160_4,
     hetio45159 AS hetio45159_5,
     hetio45159 AS hetio45159_6
WHERE hetio45159_5.s = hetio45160_3.d
  AND hetio45159_5.s = hetio45160_4.s
  AND hetio45159_6.s = hetio45160_4.d
  AND hetio45160_2.s = hetio45173_0.d
  AND hetio45160_3.s = hetio45173_1.d
  AND hetio45173_0.s = hetio45173_1.s
GROUP BY hetio45159_6.s, hetio45173_0.s, hetio45159_5.s;
