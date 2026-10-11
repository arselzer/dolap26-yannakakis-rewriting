# Provenance and redistribution notice

The three files under `queries/` are byte-for-byte copies of query files in
the public reproducibility repository for *Soft and Constrained Hypertree
Width*, commit `6ef74a9ffd084486e1e75cdc3171c80e9561d5ef`. They are distributed
under the repository's MIT license; see `LICENSE-PODS-MIT.txt`.

`q_hto_count_derived.sql` and `q2_hto_count_derived.sql` are project-derived
COUNT probes. They retain the corresponding MIT-licensed FROM/WHERE text but
are not queries from the PODS artifact and must not be compared to its results.

`q_lsqb.sql` is the PODS artifact's `q_lsqb.sql`. It is not an official LSQB
query. Measurements using it must be described as the PODS `q_lb` query on an
LSQB SF10 dataset, and not as official LSQB or LDBC benchmark results.

The LSQB query/schema repository is Apache-2.0 and carries the attribution
"Copyright [2020-]2026 Linked Data Benchmark Council". This directory does
not copy its data archive. The official dataset download page states no
separate redistribution terms for the archive, so downloaded data must remain
external to this repository until redistribution permission is established.

The Hetionet data are also external. Hetionet's original repository releases
its original content under CC0, but the integrated network contains material
from many sources with per-node and per-edge licensing. Do not mirror a
Hetionet or CE-derived archive without auditing those source licenses.
