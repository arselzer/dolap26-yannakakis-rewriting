# All 204 fixed-rule queries: runtimes and timeouts

One row per registered derived `COUNT` query. `C+U` means cyclic with
unguarded grouping; `C` cyclic without unguarded grouping; `U` acyclic
unguarded; and `O` the remaining acyclic global/guarded controls.
`Cover` is the minimum number of relation occurrences needed to cover
the grouping keys: 0 for global COUNT; values above 1 are unguarded.
The frozen rule selected a rewrite for 76 queries and native SQL for 128.

A numeric runtime is the median of six completed PostgreSQL planning
plus execution measurements. `TO` counts the measured runs that reached
the 60-second statement limit. A native choice uses one physical
measurement: `same native` and `—` in the chosen columns do not mean a
second run. Temporary-file failures and SQL errors remain explicit.
The ratio is shown only for two completed six-run arms, or as a strict
lower bound when native timed out in all six runs and the rewrite
completed all six. `Check` is the separate output-comparison result.

Timing source: [full CSV](queries.csv), SHA-256 `af658b3c071af3d77d780f812808ac79ccc52cd502eec21e795d007c7de28eda`.
Structure source: [query structure CSV](query-structure.csv), SHA-256 `6d814c4ca86e9a560c07c5456e760d8e508544750579ac98d99c27f3faa6df5f`.

## Cyclic and unguarded breakdown

These four groups are disjoint. Fifty-four cyclic queries also have
unguarded grouping, so summing all cyclic and all unguarded queries
would count them twice. Capped sums use only pairs where both arms
were finite or timeout-censored, with each timeout charged 60 seconds.

| Class | Queries | Rewrite chosen | Native with TO | Rewrite with TO | Comparable pairs | Native capped sum | Chosen capped sum | Finite pairs | Median finite speedup |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| cyclic + unguarded | 54 | 18 | 6 | 1 | 12 | 133.223 s | 138.561 s | 11 | 3.05× |
| cyclic only | 54 | 14 | 5 | 5 | 12 | 337.023 s | 279.334 s | 7 | 1.06× |
| unguarded only | 54 | 21 | 21 | 7 | 15 | 662.371 s | 342.097 s | 5 | 12.27× |
| neither | 42 | 23 | 17 | 2 | 16 | 753.235 s | 181.456 s | 4 | 13.61× |

The finite speedup is the median of the per-query native/rewrite
median ratios; its small denominators are shown explicitly.

### By dataset and query type

The same four disjoint types are split by underlying schema. Each
capped runtime pair sums only the listed comparable selected rewrites;
it is not a total over every query in that row. `—` means no such
pair or no fully completed pair, not a measured zero.

| Dataset | Type | Queries | Rewrite chosen | Comparable pairs | Native → chosen capped (s) | Finite median speedup (pairs) |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| JOB | C+U | 18 | 7 | 7 | 2.261 → 0.559 | 6.87× (7) |
| JOB | C | 18 | 3 | 3 | 0.902 → 0.268 | 5.36× (3) |
| JOB | U | 18 | 3 | 3 | 86.250 → 0.739 | 53.25× (2) |
| JOB | O | 12 | 2 | 2 | 12.295 → 0.430 | 27.07× (2) |
| STATS | C+U | 18 | 2 | 2 | 34.327 → 41.083 | 0.83× (2) |
| STATS | C | 18 | 2 | 2 | 10.329 → 10.020 | 1.03× (2) |
| STATS | U | 18 | 7 | 6 | 216.122 → 73.878 | 12.27× (3) |
| STATS | O | 12 | 4 | 3 | 80.940 → 10.484 | 8.25× (2) |
| LSQB | C+U | 6 | 2 | 2 | 36.634 → 36.918 | 0.99× (2) |
| LSQB | C | 6 | 2 | 2 | 25.792 → 25.676 | 1.00× (2) |
| LSQB | U | 6 | 2 | 0 | — | — |
| LSQB | O | 6 | 5 | 3 | 180.000 → 156.713 | — |
| Hetionet | C+U | 6 | 1 | 1 | 60.000 → 60.000 | — |
| Hetionet | C | 6 | 1 | 1 | 60.000 → 3.370 | — |
| Hetionet | U | 6 | 3 | 2 | 120.000 → 29.626 | — |
| Hetionet | O | 6 | 6 | 4 | 240.000 → 5.340 | — |
| SNAP | C+U | 6 | 6 | 0 | — | — |
| SNAP | C | 6 | 6 | 4 | 240.000 → 240.000 | — |
| SNAP | U | 6 | 6 | 4 | 240.000 → 237.853 | — |
| SNAP | O | 6 | 6 | 4 | 240.000 → 8.489 | — |

### Group-cover levels

| Minimum group cover | Cyclic queries | Acyclic queries | Meaning |
| ---: | ---: | ---: | --- |
| 0 | 33 | 21 | global COUNT |
| 1 | 21 | 21 | one relation guards the key |
| 2 | 46 | 45 | unguarded grouping |
| 3 | 8 | 9 | unguarded grouping |

## Timeout locations by dataset

| Dataset | Queries | Native choice | Rewrite choice | Native queries with TO | Rewrite queries with TO | Native queries with temp limit | Rewrite queries with temp limit |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| JOB | 66 | 51 | 15 | 5 | 0 | 1 | 0 |
| STATS | 66 | 51 | 15 | 9 | 1 | 3 | 0 |
| LSQB | 24 | 13 | 11 | 7 | 2 | 9 | 3 |
| Hetionet | 24 | 13 | 11 | 11 | 1 | 8 | 0 |
| SNAP | 24 | 0 | 24 | 17 | 11 | 7 | 7 |

The timeout and temp-limit counts above count **queries with at
least one affected run**, not individual executions. Individual
timeout counts are below; other mixed states are kept verbatim.

## JOB (66 queries)

| Query | Type | Cover | Chosen action | Native runtime/status | Chosen runtime/status | Native TO | Chosen TO | Ratio | Check |
| --- | --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | --- |
| fresh-job-01-acyclic-u2-equality-core | U | 2 | native | 0.269 s | same native | 0 | — | — | same SQL |
| fresh-job-01-cyclic-global-equality-core | C | 0 | native | 0.154 s | same native | 0 | — | — | same SQL |
| fresh-job-01-cyclic-u2-equality-core | C+U | 2 | native | 0.173 s | same native | 0 | — | — | same SQL |
| fresh-job-09-acyclic-u2-equality-core | U | 2 | structural-grouped-per-key-forward | 2.713 s | 0.243 s | 0 | 0 | 11.17× | exact |
| fresh-job-09-cyclic-global-equality-core | C | 0 | cost-partial-inline-b1 | 0.392 s | 0.187 s | 0 | 0 | 2.10× | exact |
| fresh-job-09-cyclic-u2-equality-core | C+U | 2 | structural-cyclic-core-count | 0.428 s | 0.173 s | 0 | 0 | 2.47× | exact |
| fresh-job-13-acyclic-u2-equality-core | U | 2 | native | 11.686 s | same native | 0 | — | — | same SQL |
| fresh-job-13-cyclic-global-equality-core | C | 0 | native | 0.320 s | same native | 0 | — | — | same SQL |
| fresh-job-13-cyclic-u2-equality-core | C+U | 2 | native | 0.343 s | same native | 0 | — | — | same SQL |
| fresh-job-14-acyclic-u2-equality-core | U | 2 | native | 44.837 s | same native | 0 | — | — | same SQL |
| fresh-job-14-cyclic-global-equality-core | C | 0 | native | 0.322 s | same native | 0 | — | — | same SQL |
| fresh-job-14-cyclic-u2-equality-core | C+U | 2 | native | 0.381 s | same native | 0 | — | — | same SQL |
| fresh-job-24-acyclic-u2-equality-core | U | 2 | structural-grouped-per-key-forward | 23.537 s | 0.247 s | 0 | 0 | 95.33× | exact |
| fresh-job-24-cyclic-global-equality-core | C | 0 | native | 1.461 s | same native | 0 | — | — | same SQL |
| fresh-job-24-cyclic-u2-equality-core | C+U | 2 | native | 0.863 s | same native | 0 | — | — | same SQL |
| fresh-job-31-acyclic-u2-equality-core | U | 2 | structural-grouped-per-key-forward | 60s+ (6/6) | 0.250 s | 6 | 0 | >240.34× | unresolved: 120s+ |
| fresh-job-31-cyclic-global-equality-core | C | 0 | native | 1.394 s | same native | 0 | — | — | same SQL |
| fresh-job-31-cyclic-u2-equality-core | C+U | 2 | native | 1.457 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-acyclic-g1-equality-core | O | 1 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| job-29a-returning-actor-acyclic-g1-source-filters | O | 1 | native | 0.410 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-acyclic-global-equality-core | O | 0 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| job-29a-returning-actor-acyclic-global-source-filters | O | 0 | native | 0.262 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-acyclic-u2-equality-core | U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| job-29a-returning-actor-acyclic-u2-source-filters | U | 2 | native | 0.259 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-acyclic-u3-equality-core | U | 3 | native | 60s+:4/6; temp-file-limit:2/6 | same native | 4 | — | — | same SQL |
| job-29a-returning-actor-acyclic-u3-source-filters | U | 3 | native | 0.261 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-cyclic-g1-equality-core | C | 1 | native | 7.709 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-cyclic-g1-source-filters | C | 1 | native | 1.257 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-cyclic-global-equality-core | C | 0 | native | 7.432 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-cyclic-global-source-filters | C | 0 | native | 0.258 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-cyclic-u2-equality-core | C+U | 2 | native | 7.791 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-cyclic-u2-source-filters | C+U | 2 | native | 0.279 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-cyclic-u3-equality-core | C+U | 3 | native | 7.584 s | same native | 0 | — | — | same SQL |
| job-29a-returning-actor-cyclic-u3-source-filters | C+U | 3 | native | 1.601 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-acyclic-g1-equality-core | O | 1 | native | 1.532 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-acyclic-g1-source-filters | O | 1 | native | 0.238 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-acyclic-global-equality-core | O | 0 | native | 2.276 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-acyclic-global-source-filters | O | 0 | native | 0.210 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-acyclic-u2-equality-core | U | 2 | native | 1.709 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-acyclic-u2-source-filters | U | 2 | native | 0.213 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-acyclic-u3-equality-core | U | 2 | native | 1.789 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-acyclic-u3-source-filters | U | 2 | native | 0.251 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-cyclic-g1-equality-core | C | 1 | native | 0.300 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-cyclic-g1-source-filters | C | 1 | structural-cyclic-core-count | 0.246 s | 0.032 s | 0 | 0 | 7.65× | exact |
| job-30a-shared-keyword-cyclic-global-equality-core | C | 0 | native | 0.320 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-cyclic-global-source-filters | C | 0 | native | 0.249 s | same native | 0 | — | — | same SQL |
| job-30a-shared-keyword-cyclic-u2-equality-core | C+U | 2 | structural-cyclic-core-count | 0.375 s | 0.117 s | 0 | 0 | 3.20× | exact |
| job-30a-shared-keyword-cyclic-u2-source-filters | C+U | 2 | structural-cyclic-core-count | 0.252 s | 0.033 s | 0 | 0 | 7.74× | exact |
| job-30a-shared-keyword-cyclic-u3-equality-core | C+U | 2 | structural-cyclic-core-count | 0.363 s | 0.119 s | 0 | 0 | 3.05× | exact |
| job-30a-shared-keyword-cyclic-u3-source-filters | C+U | 2 | structural-cyclic-core-count | 0.255 s | 0.033 s | 0 | 0 | 7.85× | exact |
| job-33a-company-acyclic-g1-equality-core | O | 1 | structural-grouped-per-key-forward | 3.203 s | 0.179 s | 0 | 0 | 17.89× | exact |
| job-33a-company-acyclic-g1-source-filters | O | 1 | native | 0.275 s | same native | 0 | — | — | same SQL |
| job-33a-company-acyclic-global-equality-core | O | 0 | structural-scalar-restricted-group-forward | 9.092 s | 0.251 s | 0 | 0 | 36.26× | exact |
| job-33a-company-acyclic-global-source-filters | O | 0 | native | 0.260 s | same native | 0 | — | — | same SQL |
| job-33a-company-acyclic-u2-equality-core | U | 2 | native | 5.126 s | same native | 0 | — | — | same SQL |
| job-33a-company-acyclic-u2-source-filters | U | 2 | native | 0.269 s | same native | 0 | — | — | same SQL |
| job-33a-company-acyclic-u3-equality-core | U | 3 | native | 3.354 s | same native | 0 | — | — | same SQL |
| job-33a-company-acyclic-u3-source-filters | U | 3 | native | 0.284 s | same native | 0 | — | — | same SQL |
| job-33a-company-cyclic-g1-equality-core | C | 1 | native | 4.903 s | same native | 0 | — | — | same SQL |
| job-33a-company-cyclic-g1-source-filters | C | 1 | structural-cyclic-core-count | 0.264 s | 0.049 s | 0 | 0 | 5.36× | exact |
| job-33a-company-cyclic-global-equality-core | C | 0 | native | 3.790 s | same native | 0 | — | — | same SQL |
| job-33a-company-cyclic-global-source-filters | C | 0 | native | 0.264 s | same native | 0 | — | — | same SQL |
| job-33a-company-cyclic-u2-equality-core | C+U | 2 | native | 1.415 s | same native | 0 | — | — | same SQL |
| job-33a-company-cyclic-u2-source-filters | C+U | 2 | structural-cyclic-core-count | 0.288 s | 0.042 s | 0 | 0 | 6.92× | exact |
| job-33a-company-cyclic-u3-equality-core | C+U | 3 | native | 1.799 s | same native | 0 | — | — | same SQL |
| job-33a-company-cyclic-u3-source-filters | C+U | 3 | structural-cyclic-core-count | 0.300 s | 0.044 s | 0 | 0 | 6.87× | exact |

## STATS (66 queries)

| Query | Type | Cover | Chosen action | Native runtime/status | Chosen runtime/status | Native TO | Chosen TO | Ratio | Check |
| --- | --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | --- |
| fresh-stats-q027-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b2 | 60s+ (6/6) | 4.781 s | 6 | 0 | >12.55× | unresolved: 120s+ |
| fresh-stats-q027-cyclic-global-equality-core | C | 0 | structural-flat-initial | 4.687 s | 4.721 s | 0 | 0 | 0.99× | exact |
| fresh-stats-q027-cyclic-u2-equality-core | C+U | 2 | structural-flat-initial | 14.018 s | 18.062 s | 0 | 0 | 0.78× | exact |
| fresh-stats-q028-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b2 | 28.004 s | 0.515 s | 0 | 0 | 54.33× | exact |
| fresh-stats-q028-cyclic-global-equality-core | C | 0 | native | 0.257 s | same native | 0 | — | — | same SQL |
| fresh-stats-q028-cyclic-u2-equality-core | C+U | 2 | native | 0.484 s | same native | 0 | — | — | same SQL |
| fresh-stats-q040-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b2 | 3.072 s | 0.294 s | 0 | 0 | 10.44× | exact |
| fresh-stats-q040-cyclic-global-equality-core | C | 0 | native | 0.155 s | same native | 0 | — | — | same SQL |
| fresh-stats-q040-cyclic-u2-equality-core | C+U | 2 | native | 0.137 s | same native | 0 | — | — | same SQL |
| fresh-stats-q042-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b2 | 5.046 s | 0.411 s | 0 | 0 | 12.27× | exact |
| fresh-stats-q042-cyclic-global-equality-core | C | 0 | native | 0.177 s | same native | 0 | — | — | same SQL |
| fresh-stats-q042-cyclic-u2-equality-core | C+U | 2 | native | 0.216 s | same native | 0 | — | — | same SQL |
| fresh-stats-q043-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b2 | 60s+ (6/6) | 7.876 s | 6 | 0 | >7.62× | unresolved: 120s+ |
| fresh-stats-q043-cyclic-global-equality-core | C | 0 | structural-flat-initial | 5.642 s | 5.298 s | 0 | 0 | 1.06× | exact |
| fresh-stats-q043-cyclic-u2-equality-core | C+U | 2 | structural-flat-initial | 20.309 s | 23.021 s | 0 | 0 | 0.88× | exact |
| fresh-stats-q060-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b2 | temp-file-limit:6/6 | 5.700 s | 0 | 0 | — | unresolved: temp |
| fresh-stats-q060-cyclic-global-equality-core | C | 0 | native | 0.682 s | same native | 0 | — | — | same SQL |
| fresh-stats-q060-cyclic-u2-equality-core | C+U | 2 | native | 1.720 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-acyclic-g1-equality-core | O | 1 | native | 0.917 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-acyclic-g1-source-filters | O | 1 | native | 0.824 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-acyclic-global-equality-core | O | 0 | native | 0.835 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-acyclic-global-source-filters | O | 0 | native | 0.663 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-acyclic-u2-equality-core | U | 2 | native | 1.870 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-acyclic-u2-source-filters | U | 2 | native | 1.528 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-acyclic-u3-equality-core | U | 2 | native | 2.243 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-acyclic-u3-source-filters | U | 2 | native | 1.743 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-cyclic-g1-equality-core | C | 1 | native | 0.252 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-cyclic-g1-source-filters | C | 1 | native | 0.238 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-cyclic-global-equality-core | C | 0 | native | 0.195 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-cyclic-global-source-filters | C | 0 | native | 0.195 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-cyclic-u2-equality-core | C+U | 2 | native | 0.282 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-cyclic-u2-source-filters | C+U | 2 | native | 0.260 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-cyclic-u3-equality-core | C+U | 2 | native | 0.272 s | same native | 0 | — | — | same SQL |
| stats-073-146-owner-return-cyclic-u3-source-filters | C+U | 2 | native | 0.282 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-acyclic-g1-equality-core | O | 1 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-acyclic-g1-source-filters | O | 1 | native | 6.734 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-acyclic-global-equality-core | O | 0 | structural-fused-v3 | temp-file-limit:6/6 | 4.345 s | 0 | 0 | — | unresolved: temp |
| stats-093-075-comment-return-acyclic-global-source-filters | O | 0 | structural-fused-v3 | 4.213 s | 0.451 s | 0 | 0 | 9.34× | exact |
| stats-093-075-comment-return-acyclic-u2-equality-core | U | 2 | structural-grouped-per-key-forward | 60s+ (6/6) | 60s+ (6/6) | 6 | 6 | — | unresolved: 120s+ |
| stats-093-075-comment-return-acyclic-u2-source-filters | U | 2 | native | 15.292 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-acyclic-u3-equality-core | U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| stats-093-075-comment-return-acyclic-u3-source-filters | U | 2 | native | 16.599 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-cyclic-g1-equality-core | C | 1 | native | 9.837 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-cyclic-g1-source-filters | C | 1 | native | 0.322 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-cyclic-global-equality-core | C | 0 | native | 4.591 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-cyclic-global-source-filters | C | 0 | native | 0.297 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-cyclic-u2-equality-core | C+U | 2 | native | 10.427 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-cyclic-u2-source-filters | C+U | 2 | native | 0.343 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-cyclic-u3-equality-core | C+U | 2 | native | 11.467 s | same native | 0 | — | — | same SQL |
| stats-093-075-comment-return-cyclic-u3-source-filters | C+U | 2 | native | 0.345 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-acyclic-g1-equality-core | O | 1 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| stats-129-140-owner-return-acyclic-g1-source-filters | O | 1 | native | 26.807 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-acyclic-global-equality-core | O | 0 | structural-fused-v3 | 60s+ (6/6) | 7.694 s | 6 | 0 | >7.80× | unresolved: 120s+ |
| stats-129-140-owner-return-acyclic-global-source-filters | O | 0 | structural-fused-v3 | 16.727 s | 2.340 s | 0 | 0 | 7.15× | exact |
| stats-129-140-owner-return-acyclic-u2-equality-core | U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| stats-129-140-owner-return-acyclic-u2-source-filters | U | 2 | native | 60s+:2/6; ok:4/6 | same native | 2 | — | — | same SQL |
| stats-129-140-owner-return-acyclic-u3-equality-core | U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| stats-129-140-owner-return-acyclic-u3-source-filters | U | 2 | native | 45.063 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-cyclic-g1-equality-core | C | 1 | native | 1.312 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-cyclic-g1-source-filters | C | 1 | native | 0.965 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-cyclic-global-equality-core | C | 0 | native | 0.839 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-cyclic-global-source-filters | C | 0 | native | 0.421 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-cyclic-u2-equality-core | C+U | 2 | native | 2.394 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-cyclic-u2-source-filters | C+U | 2 | native | 1.261 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-cyclic-u3-equality-core | C+U | 2 | native | 2.622 s | same native | 0 | — | — | same SQL |
| stats-129-140-owner-return-cyclic-u3-source-filters | C+U | 2 | native | 1.420 s | same native | 0 | — | — | same SQL |

## LSQB (24 queries)

| Query | Type | Cover | Chosen action | Native runtime/status | Chosen runtime/status | Native TO | Chosen TO | Ratio | Check |
| --- | --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | --- |
| lsqb-q1-member-author-acyclic-g1-equality-core | O | 1 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| lsqb-q1-member-author-acyclic-global-equality-core | O | 0 | structural-fused-v3 | 60s+ (6/6) | 56.236 s | 6 | 0 | >1.07× | unresolved: 120s+ |
| lsqb-q1-member-author-acyclic-u2-equality-core | U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| lsqb-q1-member-author-acyclic-u3-equality-core | U | 3 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |
| lsqb-q1-member-author-cyclic-g1-equality-core | C | 1 | native | 22.122 s | same native | 0 | — | — | same SQL |
| lsqb-q1-member-author-cyclic-global-equality-core | C | 0 | native | 20.488 s | same native | 0 | — | — | same SQL |
| lsqb-q1-member-author-cyclic-u2-equality-core | C+U | 2 | native | 27.778 s | same native | 0 | — | — | same SQL |
| lsqb-q1-member-author-cyclic-u3-equality-core | C+U | 3 | native | 36.318 s | same native | 0 | — | — | same SQL |
| lsqb-q2-location-acyclic-g1-equality-core | O | 1 | structural-partial-inline-b3 | 60s+ (6/6) | 60s+ (6/6) | 6 | 6 | — | unresolved: 120s+ |
| lsqb-q2-location-acyclic-global-equality-core | O | 0 | structural-fused-v3 | 60s+ (6/6) | 40.477 s | 6 | 0 | >1.48× | exact |
| lsqb-q2-location-acyclic-u2-equality-core | U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| lsqb-q2-location-acyclic-u3-equality-core | U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| lsqb-q2-location-cyclic-g1-equality-core | C | 1 | structural-baseline-v2 | 14.442 s | 14.151 s | 0 | 0 | 1.02× | exact |
| lsqb-q2-location-cyclic-global-equality-core | C | 0 | cost-fused-v3 | 11.350 s | 11.525 s | 0 | 0 | 0.98× | exact |
| lsqb-q2-location-cyclic-u2-equality-core | C+U | 2 | structural-baseline-v2 | 18.055 s | 18.089 s | 0 | 0 | 1.00× | exact |
| lsqb-q2-location-cyclic-u3-equality-core | C+U | 2 | structural-baseline-v2 | 18.580 s | 18.830 s | 0 | 0 | 0.99× | exact |
| lsqb-q3-country-triangle-acyclic-g1-equality-core | O | 1 | structural-partial-inline-b2 | temp-file-limit:6/6 | temp-file-limit:6/6 | 0 | 0 | — | unresolved: temp |
| lsqb-q3-country-triangle-acyclic-global-equality-core | O | 0 | structural-fused-v3 | temp-file-limit:6/6 | 60s+ (6/6) | 0 | 6 | — | unresolved: temp |
| lsqb-q3-country-triangle-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b2 | temp-file-limit:6/6 | temp-file-limit:6/6 | 0 | 0 | — | unresolved: temp |
| lsqb-q3-country-triangle-acyclic-u3-equality-core | U | 3 | structural-partial-inline-b2 | temp-file-limit:6/6 | temp-file-limit:6/6 | 0 | 0 | — | unresolved: temp |
| lsqb-q3-country-triangle-cyclic-g1-equality-core | C | 1 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |
| lsqb-q3-country-triangle-cyclic-global-equality-core | C | 0 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |
| lsqb-q3-country-triangle-cyclic-u2-equality-core | C+U | 2 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |
| lsqb-q3-country-triangle-cyclic-u3-equality-core | C+U | 2 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |

## Hetionet (24 queries)

| Query | Type | Cover | Chosen action | Native runtime/status | Chosen runtime/status | Native TO | Chosen TO | Ratio | Check |
| --- | --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | --- |
| hetionet-q-hto-acyclic-g1-equality-core | O | 1 | structural-exact-origin | temp-file-limit:6/6 | 11.823 s | 0 | 0 | — | unresolved: temp |
| hetionet-q-hto-acyclic-global-equality-core | O | 0 | structural-fused-v3 | temp-file-limit:6/6 | 3.030 s | 0 | 0 | — | unresolved: temp |
| hetionet-q-hto-acyclic-u2-equality-core | U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| hetionet-q-hto-acyclic-u3-equality-core | U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| hetionet-q-hto-cyclic-g1-equality-core | C | 1 | cost-partial-exact-origin | 60s+ (6/6) | 3.370 s | 6 | 0 | >17.81× | exact |
| hetionet-q-hto-cyclic-global-equality-core | C | 0 | native | 45.490 s | same native | 0 | — | — | same SQL |
| hetionet-q-hto-cyclic-u2-equality-core | C+U | 2 | native | 60s+ (6/6) | same native | 6 | — | — | same SQL |
| hetionet-q-hto-cyclic-u3-equality-core | C+U | 2 | cost-partial-exact-origin | 60s+ (6/6) | 60s+:4/6; ok:2/6 | 6 | 4 | — | unresolved: 120s+ |
| hetionet-q2-hto-acyclic-g1-equality-core | O | 1 | structural-partial-inline-b3 | 60s+ (6/6) | 2.335 s | 6 | 0 | >25.70× | unresolved: 120s+ |
| hetionet-q2-hto-acyclic-global-equality-core | O | 0 | structural-fused-v3 | 60s+ (6/6) | 1.597 s | 6 | 0 | >37.58× | unresolved: 120s+ |
| hetionet-q2-hto-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b3 | 60s+ (6/6) | 6.839 s | 6 | 0 | >8.77× | unresolved: 120s+ |
| hetionet-q2-hto-acyclic-u3-equality-core | U | 2 | structural-exact-origin | 60s+ (6/6) | 22.788 s | 6 | 0 | >2.63× | unresolved: 120s+ |
| hetionet-q2-hto-cyclic-g1-equality-core | C | 1 | native | 9.761 s | same native | 0 | — | — | same SQL |
| hetionet-q2-hto-cyclic-global-equality-core | C | 0 | native | 7.229 s | same native | 0 | — | — | same SQL |
| hetionet-q2-hto-cyclic-u2-equality-core | C+U | 2 | native | 21.440 s | same native | 0 | — | — | same SQL |
| hetionet-q2-hto-cyclic-u3-equality-core | C+U | 2 | native | 23.019 s | same native | 0 | — | — | same SQL |
| hetionet-thesis-cbgigigad-acyclic-g1-equality-core | O | 1 | structural-baseline-v2 | 60s+ (6/6) | 0.814 s | 6 | 0 | >73.71× | unresolved: 120s+ |
| hetionet-thesis-cbgigigad-acyclic-global-equality-core | O | 0 | structural-fused-v3 | 60s+ (6/6) | 0.594 s | 6 | 0 | >100.98× | unresolved: 120s+ |
| hetionet-thesis-cbgigigad-acyclic-u2-equality-core | U | 2 | structural-exact-origin | temp-file-limit:6/6 | 23.431 s | 0 | 0 | — | unresolved: temp |
| hetionet-thesis-cbgigigad-acyclic-u3-equality-core | U | 3 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |
| hetionet-thesis-cbgigigad-cyclic-g1-equality-core | C | 1 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |
| hetionet-thesis-cbgigigad-cyclic-global-equality-core | C | 0 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |
| hetionet-thesis-cbgigigad-cyclic-u2-equality-core | C+U | 2 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |
| hetionet-thesis-cbgigigad-cyclic-u3-equality-core | C+U | 3 | native | temp-file-limit:6/6 | same native | 0 | — | — | same SQL |

## SNAP (24 queries)

| Query | Type | Cover | Chosen action | Native runtime/status | Chosen runtime/status | Native TO | Chosen TO | Ratio | Check |
| --- | --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | --- |
| snap-two-paths-10-acyclic-g1-equality-core | O | 1 | structural-partial-inline-b4 | temp-file-limit:6/6 | 3.521 s | 0 | 0 | — | unresolved: temp |
| snap-two-paths-10-acyclic-global-equality-core | O | 0 | structural-fused-v3 | 60s+ (6/6) | error:6/6 | 6 | 0 | — | unresolved: 120s+ |
| snap-two-paths-10-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b4 | 60s+ (6/6) | 60s+ (6/6) | 6 | 6 | — | unresolved: 120s+ |
| snap-two-paths-10-acyclic-u3-equality-core | U | 3 | structural-partial-inline-b4 | temp-file-limit:6/6 | 60s+ (6/6) | 0 | 6 | — | unresolved: temp |
| snap-two-paths-10-cyclic-g1-equality-core | C | 1 | cost-partial-inline-b3 | 60s+ (6/6) | 60s+ (6/6) | 6 | 6 | — | unresolved: temp |
| snap-two-paths-10-cyclic-global-equality-core | C | 0 | cost-partial-inline-b3 | 60s+ (6/6) | 60s+ (6/6) | 6 | 6 | — | unresolved: temp |
| snap-two-paths-10-cyclic-u2-equality-core | C+U | 2 | cost-baseline-v2 | 60s+ (6/6) | temp-file-limit:6/6 | 6 | 0 | — | unresolved: temp |
| snap-two-paths-10-cyclic-u3-equality-core | C+U | 3 | cost-baseline-v2 | 60s+ (6/6) | temp-file-limit:6/6 | 6 | 0 | — | unresolved: temp |
| snap-two-paths-6-acyclic-g1-equality-core | O | 1 | structural-partial-inline-b2 | 60s+ (6/6) | 3.179 s | 6 | 0 | >18.88× | unresolved: 120s+ |
| snap-two-paths-6-acyclic-global-equality-core | O | 0 | structural-fused-v3 | 60s+ (6/6) | 0.909 s | 6 | 0 | >65.97× | unresolved: 120s+ |
| snap-two-paths-6-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b2 | 60s+ (6/6) | 60s+:1/6; ok:5/6 | 6 | 1 | — | unresolved: 120s+ |
| snap-two-paths-6-acyclic-u3-equality-core | U | 2 | structural-partial-inline-b3 | 60s+ (6/6) | 60s+ (6/6) | 6 | 6 | — | unresolved: temp |
| snap-two-paths-6-cyclic-g1-equality-core | C | 1 | cost-partial-inline-b1 | temp-file-limit:6/6 | temp-file-limit:6/6 | 0 | 0 | — | unresolved: temp |
| snap-two-paths-6-cyclic-global-equality-core | C | 0 | cost-fused-v3 | temp-file-limit:6/6 | 60s+ (6/6) | 0 | 6 | — | unresolved: temp |
| snap-two-paths-6-cyclic-u2-equality-core | C+U | 2 | cost-partial-inline-b1 | temp-file-limit:6/6 | temp-file-limit:6/6 | 0 | 0 | — | unresolved: temp |
| snap-two-paths-6-cyclic-u3-equality-core | C+U | 2 | cost-partial-inline-b1 | temp-file-limit:6/6 | temp-file-limit:6/6 | 0 | 0 | — | unresolved: temp |
| snap-two-paths-8-acyclic-g1-equality-core | O | 1 | structural-partial-inline-b3 | 60s+ (6/6) | 3.448 s | 6 | 0 | >17.40× | unresolved: 120s+ |
| snap-two-paths-8-acyclic-global-equality-core | O | 0 | structural-fused-v3 | 60s+ (6/6) | 0.953 s | 6 | 0 | >62.96× | unresolved: 120s+ |
| snap-two-paths-8-acyclic-u2-equality-core | U | 2 | structural-partial-inline-b3 | 60s+ (6/6) | 60s+ (6/6) | 6 | 6 | — | unresolved: 120s+ |
| snap-two-paths-8-acyclic-u3-equality-core | U | 3 | structural-partial-inline-b5 | temp-file-limit:6/6 | 60s+ (6/6) | 0 | 6 | — | unresolved: temp |
| snap-two-paths-8-cyclic-g1-equality-core | C | 1 | cost-partial-inline-b2 | 60s+ (6/6) | 60s+ (6/6) | 6 | 6 | — | unresolved: temp |
| snap-two-paths-8-cyclic-global-equality-core | C | 0 | cost-partial-inline-b2 | 60s+ (6/6) | 60s+ (6/6) | 6 | 6 | — | unresolved: temp |
| snap-two-paths-8-cyclic-u2-equality-core | C+U | 2 | cost-baseline-v2 | 60s+ (6/6) | temp-file-limit:6/6 | 6 | 0 | — | unresolved: temp |
| snap-two-paths-8-cyclic-u3-equality-core | C+U | 3 | cost-baseline-v2 | 60s+ (6/6) | temp-file-limit:6/6 | 6 | 0 | — | unresolved: temp |
