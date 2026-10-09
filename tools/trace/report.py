#!/usr/bin/env python3
"""Reads what a Lyra run recorded about itself.

A run asked with `--time-trace` and `--stats-file` writes where its time went
and its numbers; the compiler prints no summary of either, because which
reading is wanted depends on who is reading. This is two of those readings.

`summary` prints one run: the stages that ran alone with their time and peak
memory, the time each per-unit stage took summed over units with the slowest
unit, the units by what they left behind, and how long each tool the build ran
took.

`compare` holds a run against a base and exits with the number of regressions,
so a check can be built on it. Counts -- units, and files per unit -- are exact
and any growth is a regression. A size or a peak regresses only past both a
percentage and an absolute floor, so noise on a small number is not reported.
Time is not compared: it moves with the machine, and a regression in it is
found by measuring instructions outside the run.

Usage:
    python3 tools/trace/report.py summary --stats STATS [--trace TRACE]
    python3 tools/trace/report.py compare BASE_STATS STATS
                                  [--percent P] [--floor-bytes B]
"""

import argparse
import json
import sys
from collections import defaultdict
from pathlib import Path

# The stages a unit passes through, which overlap between units and so are
# summed over units rather than read as one interval.
PER_UNIT_SPANS = (
    "declare unit",
    "lower to HIR",
    "lower to MIR",
    "lower to LIR",
    "emit C++",
    "emit LLVM IR",
    "generate object",
)

DEFAULT_PERCENT = 5.0
DEFAULT_FLOOR_BYTES = 1 << 20


def load(path):
    with Path(path).open() as handle:
        return json.load(handle)


def mib(count):
    return f"{count / (1 << 20):.1f} MiB"


def seconds(microseconds):
    return f"{microseconds / 1e6:.3f} s"


def stage_times(trace):
    """The duration of each span name that ran once, outermost first."""
    times = {}
    for event in trace["traceEvents"]:
        if event.get("ph") == "X" and not event["name"].startswith("Total "):
            times.setdefault(event["name"], event["dur"])
    return times


def per_unit_times(trace):
    """For each per-unit stage, the time summed over units and the slowest."""
    summed = defaultdict(int)
    slowest = {}
    for event in trace["traceEvents"]:
        name = event.get("name")
        if event.get("ph") != "X" or name not in PER_UNIT_SPANS:
            continue
        summed[name] += event["dur"]
        unit = event.get("args", {}).get("detail", "")
        if name not in slowest or event["dur"] > slowest[name][1]:
            slowest[name] = (unit, event["dur"])
    return summed, slowest


def summary(args):
    stats = load(args.stats)
    trace = load(args.trace) if args.trace else None
    times = stage_times(trace) if trace else {}

    print(f"width {stats['width']}")
    print()
    print("stages that ran alone")
    for stage in stats["stages"]:
        took = times.get(stage["name"])
        peak = stage.get("peak_rss_bytes")
        print(
            f"  {stage['name']:<24}"
            f"{seconds(took) if took is not None else '':>12}"
            f"{mib(peak) if peak is not None else '':>14}"
        )

    if trace:
        summed, slowest = per_unit_times(trace)
        print()
        print("per-unit stages, summed over units")
        for name in PER_UNIT_SPANS:
            if name in summed:
                unit, took = slowest[name]
                print(
                    f"  {name:<24}{seconds(summed[name]):>12}"
                    f"   slowest {unit} {seconds(took)}"
                )

    print()
    print("units")
    for unit in sorted(
        stats["units"],
        key=lambda u: sum(a["bytes"] for a in u["artifacts"]),
        reverse=True,
    ):
        artifacts = unit["artifacts"]
        made = sum(1 for a in artifacts if a["made"])
        print(
            f"  {unit['name']:<40}{len(artifacts):>6} files"
            f"{sum(a['bytes'] for a in artifacts):>12} bytes"
            f"   {made} made"
        )

    if stats["children"]:
        print()
        print("tools the build ran")
        for child in stats["children"]:
            print(
                f"  {seconds(child['wall_us']):>10} wall"
                f"{seconds(child['cpu_us']):>10} cpu   {child['command'][:80]}"
            )
    return 0


def grew(base, now, percent, floor):
    return now - base > floor and now > base * (1 + percent / 100)


def compare(args):
    base = load(args.base)
    now = load(args.stats)
    regressions = []

    base_units = {u["name"]: u["artifacts"] for u in base["units"]}
    now_units = {u["name"]: u["artifacts"] for u in now["units"]}
    if len(now_units) > len(base_units):
        regressions.append(f"units: {len(base_units)} -> {len(now_units)}")
    for name, artifacts in now_units.items():
        before = base_units.get(name)
        if before is None:
            continue
        if len(artifacts) > len(before):
            regressions.append(
                f"{name}: files {len(before)} -> {len(artifacts)}"
            )
        size_before = sum(a["bytes"] for a in before)
        size_now = sum(a["bytes"] for a in artifacts)
        if grew(size_before, size_now, args.percent, args.floor_bytes):
            regressions.append(f"{name}: bytes {size_before} -> {size_now}")

    # A stage carries no peak on a platform that offers no reading of one, and
    # a peak only one side has is nothing to compare.
    base_peaks = {
        s["name"]: s["peak_rss_bytes"]
        for s in base["stages"]
        if "peak_rss_bytes" in s
    }
    for stage in now["stages"]:
        before = base_peaks.get(stage["name"])
        peak = stage.get("peak_rss_bytes")
        if (
            before is not None
            and peak is not None
            and grew(before, peak, args.percent, args.floor_bytes)
        ):
            regressions.append(
                f"stage {stage['name']}: peak {mib(before)} -> {mib(peak)}"
            )

    for regression in regressions:
        print(regression)
    return len(regressions)


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    commands = parser.add_subparsers(dest="command", required=True)

    summary_parser = commands.add_parser("summary")
    summary_parser.add_argument("--stats", required=True)
    summary_parser.add_argument("--trace")
    summary_parser.set_defaults(run=summary)

    compare_parser = commands.add_parser("compare")
    compare_parser.add_argument("base")
    compare_parser.add_argument("stats")
    compare_parser.add_argument("--percent", type=float, default=DEFAULT_PERCENT)
    compare_parser.add_argument(
        "--floor-bytes", type=int, default=DEFAULT_FLOOR_BYTES
    )
    compare_parser.set_defaults(run=compare)

    args = parser.parse_args()
    return args.run(args)


if __name__ == "__main__":
    sys.exit(main())
