SELECT hetio45173_0.s AS g1, COUNT(*) AS witness_count
FROM hetio45173 AS hetio45173_0,
     hetio45173 AS hetio45173_1,
     hetio45173 AS hetio45173_2,
     hetio45173 AS hetio45173_3,
     hetio45160 AS hetio45160,
     hetio45176 AS hetio45176_5,
     hetio45176 AS hetio45176_6
WHERE hetio45160.s = hetio45173_3.d
  AND hetio45160.s = hetio45176_5.s
  AND hetio45173_0.d = hetio45173_2.s
  AND hetio45173_0.s = hetio45173_1.s
  AND hetio45173_1.d = hetio45173_3.s
  AND hetio45176_5.d = hetio45176_6.d
GROUP BY hetio45173_0.s;
