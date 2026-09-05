#!/usr/bin/env python3
"""Merge tasty-bench CSVs into a baseline with cross-run variance."""

import argparse
import csv
import statistics


def read_run(path):
    with open(path, newline="") as stream:
        rows = list(csv.DictReader(stream))
    if not rows:
        raise ValueError(f"{path}: benchmark CSV is empty")
    return rows


def merge(paths, output):
    runs = [read_run(path) for path in paths]
    names = [row["Name"] for row in runs[0]]
    if any([row["Name"] for row in run] != names for run in runs[1:]):
        raise ValueError("benchmark names differ between CSV runs")

    fields = runs[0][0].keys()
    with open(output, "w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        for index, name in enumerate(names):
            means = [float(run[index]["Mean (ps)"]) for run in runs]
            row = dict(runs[0][index])
            row["Mean (ps)"] = str(round(statistics.mean(means)))
            row["2*Stdev (ps)"] = str(
                round(2 * statistics.stdev(means)) if len(means) > 1 else 0
            )
            writer.writerow(row)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("inputs", nargs="+", help="tasty-bench CSV files")
    parser.add_argument("-o", "--output", required=True)
    args = parser.parse_args()
    if len(args.inputs) < 2:
        parser.error("at least two input CSVs are required")
    merge(args.inputs, args.output)
