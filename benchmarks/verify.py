#!/usr/bin/env python3
"""Check that the compact final result tables cover the supplied SQL set."""

import csv
import hashlib
from pathlib import Path

from make_timings import transform


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
    for name in ("estimated-costs.csv", "six-run-timings.csv", "query-structure.csv", "timings.csv"):
        if unique_ids(name) != expected:
            raise ValueError(f"{name}: query IDs differ from queries.csv")
    original = rows("six-run-timings.csv")
    simple = rows("timings.csv")
    if simple != [transform(row) for row in original]:
        raise ValueError("timings.csv differs from the final six-run data")
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
    print("Benchmark inventory: 204 hash-matched SQL files and five matching result tables")


if __name__ == "__main__":
    main()
