# Workload provenance notice

The six SQL files in the top-level `queries` directory are independently
authored enumeration queries. Six additional source SQL files under
`starter-corpus/queries` implement C01--C04, C06, and C07 as independently
authored synthetic contracts; C05 reuses the top-level C4 SQL. They preserve
selected equi-join topologies for rewriter engineering and are not copies of
source benchmark SQL.

`lsqb-q2-enum-core.sql` adapts the cyclic join topology associated with LSQB
Q2. The aggregate query from the LSQB workload is not included, and timings of
this enumeration core must not be reported as LSQB Q2 timings.

`lsqb-q3-enum-core.sql` adapts the nine-occurrence self-join topology associated
with LSQB Q3. It is an enumeration-only engineering fixture, not the aggregate
query distributed by LSQB, and its timings must not be reported as LSQB Q3
timings. The three aliases of each repeated base relation are intentionally
kept as distinct occurrences.

`tpch-q5-derived-enum-core.sql` is a workload derived from the join topology
associated with TPC-H Q5. It omits the source filters, arithmetic, aggregation,
grouping, and ordering. It is not a TPC Benchmark H query or result, and its
measurements are not comparable with published TPC-H results. No TPC primary
metric may be derived from this smoke harness.

`synthetic-c4-enum-core.sql` and `synthetic-c6-enum-core.sql` are deterministic
cycle controls with no source benchmark data. C4 exercises the ordinary
width-two connected-cover path. C6 exposes the distinction between a
width-two unconstrained cover and a wider connected cover.

`synthetic-bow-tie-enum-core.sql` contains two triangles sharing one vertex.
It was added because the articulation point is a structural property absent
from simple cycles: a decomposition can separate two cyclic blocks at a small
interface. A C4-with-tail fixture was not also added because it would enlarge
the matrix without adding a second cyclic block.

All three synthetic queries and every dataset emitted by
`generate_fixture.py` are engineering fixtures. Their timings are
non-reportable and are not comparable to any published workload result.

The C01--C07 starter corpus reuses the top-level C4 SQL byte-for-byte and its
correctness generator profile unchanged for C05. The newly pinned starter seed
means its generated fixture bytes are distinct. It does not reuse the top-level
bow-tie for C07: that query shares one vertex, while the frozen C07 template
shares one whole edge. Every dataset emitted by
`starter-corpus/generate_fixture.py` is a bounded correctness fixture with
`performance_eligible: false`. The C03 native query is kept as an explicit
unsupported-language target and must not be described as a successful rewrite.

The manifest repeats these restrictions in machine-readable form so that
result records retain the classification.
