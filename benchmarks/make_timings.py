#!/usr/bin/env python3
"""Produce the small, one-query-per-row runtime table from the final receipt export."""

import csv
from decimal import Decimal, InvalidOperation
from pathlib import Path

from recorded_actions import load


ROOT = Path(__file__).resolve().parent
SOURCE = ROOT / "results" / "six-run-timings.csv"
DESTINATION = ROOT / "results" / "timings.csv"
TYPE_NAMES = {
    "cyclic": "cyclic",
    "acyclic-unguarded": "acyclic-unguarded",
    "continuity-control": "acyclic-guarded",
}
NATIVE = [f"native_run_{i}_s_or_status" for i in range(1, 7)]
SELECTED = [f"selected_run_{i}_s_or_status" for i in range(1, 7)]
REWRITE = [f"rewrite_run_{i}_s_or_status" for i in range(1, 7)]
FIELDS = ["query_id", "dataset", "type", "selected_action", "selected_plan_width", *NATIVE, *REWRITE,
          "native_median_s", "rewrite_median_s", "speedup"]
DECOMPOSITION_FIELDS = ['query_id', 'dataset', 'selected_action', 'search_width_bound',
                        'connected_cover_required', 'selected_plan_width', 'plan_origin', 'bag_count']


def display(value, places=7):
    if not value:
        return ""
    try:
        rounded = Decimal(value).quantize(Decimal(1).scaleb(-places))
    except InvalidOperation:
        return value
    return format(rounded, "f").rstrip("0").rstrip(".") or "0"


def transform(row, record):
    kind = TYPE_NAMES[row["stratum"]]
    action = row["selected_action"]
    fallback = action == "native"
    if fallback != (row["shared_native_observation"] == "True"):
        raise ValueError(f"inconsistent native fallback: {row['query_id']}")
    selected = [row[name] for name in SELECTED]
    if fallback and selected != ["shared native run"] * 6:
        raise ValueError(f"inconsistent shared-native timing: {row['query_id']}")
    if not fallback and "shared native run" in selected:
        raise ValueError(f"rewrite has shared-native marker: {row['query_id']}")
    output = dict(query_id=row["query_id"], dataset=row["dataset"], type=kind,
                  selected_action=action, native_median_s=display(row["native_median_s"]),
                  rewrite_median_s="" if fallback else display(row["selected_median_s"]),
                  speedup="" if fallback else display(row["finite_speedup"], 6),
                  selected_plan_width='' if fallback else str(record['selected_plan_width']))
    output.update({name: display(row[name]) for name in NATIVE})
    output.update({name: "" if fallback else display(value)
                   for name, value in zip(REWRITE, selected)})
    return output


def decomposition_row(record):
    return {field: '' if value is None else str(value) for field, value in (
        ('query_id', record['query_id']), ('dataset', record['dataset']),
        ('selected_action', record['selected_action']), ('search_width_bound', record['search_width']),
        ('connected_cover_required', record['connected_cover']),
        ('selected_plan_width', record['selected_plan_width']), ('plan_origin', record['plan_origin']),
        ('bag_count', record['bag_count']))}


def main():
    records = load()['records']
    by_id = {r['query_id']: r for r in records}
    with SOURCE.open(newline="") as stream:
        rows = list(csv.DictReader(stream))
    if len(rows) != 204 or len({row["query_id"] for row in rows}) != 204:
        raise ValueError("Expected exactly 204 distinct final result rows")
    with DESTINATION.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(transform(row, by_id[row['query_id']]) for row in rows)
    with (ROOT / 'results' / 'decompositions.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=DECOMPOSITION_FIELDS, lineterminator='\n')
        writer.writeheader()
        writer.writerows(decomposition_row(record) for record in records)
    print(f"Wrote {DESTINATION}")


if __name__ == "__main__":
    main()
