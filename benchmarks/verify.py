#!/usr/bin/env python3
"""Check that the compact final result tables cover the supplied SQL set."""

import csv
import hashlib
from pathlib import Path

from make_timings import transform, decomposition_row
from recorded_actions import load
from check_connected_cover import derive as connected_cover_rows


ROOT = Path(__file__).resolve().parent
RESULTS = ROOT / "results"


def rows(name):
    with (RESULTS / name).open(newline="") as stream:
        return list(csv.DictReader(stream))


def unique_ids(name):
    values = [row["query_id"] for row in rows(name)]
    if len(values) != 204 or len(set(values)) != 204:
        raise ValueError(f"{name}: expected 204 distinct query IDs, got {len(values)} rows")
    return set(values)


def main():
    expected = unique_ids("queries.csv")
    records = load()['records']
    by_id = {row['query_id']: row for row in records}
    if set(by_id) != expected:
        raise ValueError('Recorded choices differ from result IDs')
    for row in rows('queries.csv'):
        if row['selected_action'] != by_id[row['query_id']]['selected_action']:
            raise ValueError('Recorded action differs from final result table')
    for name in ("estimated-costs.csv", "six-run-timings.csv", "query-structure.csv", "timings.csv", "decompositions.csv"):
        if unique_ids(name) != expected:
            raise ValueError(f"{name}: query IDs differ from queries.csv")
    original = rows("six-run-timings.csv")
    simple = rows("timings.csv")
    if simple != [transform(row, by_id[row['query_id']]) for row in original]:
        raise ValueError("timings.csv differs from the final six-run data")
    if rows('decompositions.csv') != [decomposition_row(row) for row in records]:
        raise ValueError('Decomposition table differs from the recorded plans')
    if unique_ids('connected-cover.csv') != expected or rows('connected-cover.csv') != connected_cover_rows():
        raise ValueError('Connected Cover table differs from the saved plans and source graphs')
    supplied = {path.stem for path in (ROOT / "queries").glob("*.sql")}
    if supplied != expected:
        raise ValueError(
            f"SQL inventory differs: {len(expected - supplied)} missing, "
            f"{len(supplied - expected)} extra"
        )
    authored = {}
    for name, count in (("base-168.csv", 168), ("fresh-job-18.csv", 18), ("fresh-stats-18.csv", 18)):
        with (ROOT / "manifests" / name).open(newline="") as stream:
            records = list(csv.DictReader(stream))
        if len(records) != count:
            raise ValueError(f"{name}: expected {count} rows, got {len(records)}")
        for row in records:
            query_id = row["query_id"]
            if query_id in authored:
                raise ValueError(f"duplicate query ID in manifests: {query_id}")
            authored[query_id] = row["sql_sha256"]
    if set(authored) != expected:
        raise ValueError("authored query IDs differ from result IDs")
    for query_id, digest in authored.items():
        actual = hashlib.sha256((ROOT / "queries" / f"{query_id}.sql").read_bytes()).hexdigest()
        if actual != digest:
            raise ValueError(f"SQL SHA-256 mismatch: {query_id}")
    print("Verified 204 original queries, 76 exact recorded rewrite files, seven matching result tables, and Connected Cover")


if __name__ == "__main__":
    main()
