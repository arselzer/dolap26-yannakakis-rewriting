# Benchmarks

`queries/` contains the 204 derived SQL queries used by the final
fixed-estimated-cost validation. The parent-query texts are not required to
run or check this final query set; their family/source identifiers remain in
the manifests. These are modified workload queries, not official JOB, LSQB,
STATS, SNAP, or Hetionet benchmark scores. Third-party notices are under
`notices/`; underlying datasets are not redistributed. The three CSVs in
`manifests/` pin each query's SHA-256 (168 original-panel, 18 fresh JOB, and
18 fresh STATS texts).

Start with `results/timings.csv`: one row per query, six native runs, six
rewrite runs where a rewrite was selected, medians, and a finite speedup when
both sides completed. `selected_action=native` means no rewrite was selected;
its rewrite cells are empty because the native measurements were reused, not
run a second time. The three `type` values are `cyclic`,
`acyclic-unguarded`, and `acyclic-guarded` (the last is the continuity control).
The `cyclic` label includes queries with and without unguarded grouping;
`query-structure.csv` gives the minimum group-key cover for the finer split.
The speedup is `native_median_s / rewrite_median_s`; blank means no finite
two-sided ratio, **not** no benefit. The readable CSV rounds displayed seconds
to seven decimal places and speedups to six; the original values remain in
`six-run-timings.csv`.

`results/queries.csv` retains the full correctness dispositions and selected
estimated costs. `results/six-run-timings.csv` retains the original complete
timing statuses; `results/estimated-costs.csv` and `results/query-structure.csv`
give the corresponding detail. `results/all-query-runtimes.md` is the older
readable per-query table. `python3 benchmarks/make_timings.py` regenerates
the simple CSV from the complete six-run data. Run
`python3 benchmarks/verify.py` from the repository root to check the shared
204-ID inventory, exact SQL hashes, and table derivation.

The historical run chose a rewrite only if its PostgreSQL `EXPLAIN` total
cost was below 80% of native, choosing the lowest-cost available action;
otherwise it ran native. It did not use observed runtimes or a trained model.
It used two warm-ups and six measurements, 60-second execution limits, and
separate bounded exact-bag comparisons for selected rewrites. A `60s+` value
is a lower bound, not a measured 60-second runtime. A `temp-file-limit` is a
resource failure, not a timeout. `unresolved-*` correctness means that the
comparison itself did not finish; it does not establish equivalence or an
incorrect result. The result is development evidence, not a holdout or a
promoted production selector.

The historical execution environment was PostgreSQL 18.4 on DL380VM, with
`work_mem=256MB`, `temp_file_limit=2GB`, `jit=off`,
`max_parallel_workers_per_gather=2`, and `hash_mem_multiplier=2` for the
measured actions. Reported seconds include PostgreSQL planning and execution,
but exclude candidate generation, feature collection, and selector time.

The SQL texts and compact result data are complete, but this minimal snapshot
is **not a turnkey replay of the historical DL380VM experiment**. The
loaded JOB/STATS/SNAP/LSQB/Hetionet databases, exact selected generated-SQL
action registry, private acquisition/EXPLAIN receipts, and original benchmark
runner environment are not included. To reproduce the timings, those inputs
must be supplied and the host/PostgreSQL settings matched. The setup script
builds the rewriter and verifies the shared query/result inventory; it does
not claim to recreate the historical measurements.
