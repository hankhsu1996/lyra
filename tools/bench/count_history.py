#!/usr/bin/env python3
"""Keep each merge's instruction counts, and read them over a range of merges.

Every merge to main is counted against its parent on one runner, and the two
sides' counts are kept as a git note on the merge, under `NOTES_REF`. A count
moves from one runner to another, so no two merges' counts are compared with
each other: what compares is each merge's head over its base, and over a range
of merges the product of those is how far the range moved.

`record BASE HEAD --base SHA` prints the note for one merge from the result
files `run_benchmarks.py --count` wrote for each side, as `compare_counts.py`
reads them; SHA is the revision the base side was built from.

`read RANGE` reads the notes of every merge on main's first-parent line in a
git range such as `A..B`, and prints, for each real design's build and run, each
merge's change and the product; then the same product for each stage of each
design's build. A product that leaves a merge out is short by whatever that
merge moved, so it then names every merge it left out: each with no note, or
with a note counted against another base than its parent, and for each
measurement each merge whose note has no count of it. It fetches the notes
first unless told not to.

Usage:
    python3 tools/bench/count_history.py record BASE HEAD --base SHA
    python3 tools/bench/count_history.py read RANGE [--no-fetch] [--corpus]
"""

import argparse
import json
import subprocess
import sys
from collections import defaultdict

from compare_counts import percent, read_side, stage_factors
from run_benchmarks import (
    BUILD_SPLIT_ROWS,
    DESIGN_FAMILY,
    MEASURE_BUILD,
    STATUS_OK,
)

NOTES_REF = "refs/notes/instruction-counts"

# The version of a note's shape, raised whenever a field is removed or changes
# type.
NOTE_VERSION = 1

# What a note keeps of a result: enough to compare it, and nothing a run
# printed, which is in the run's own log.
KEPT_FIELDS = (
    "case", "family", "measure", "backend", "status",
    "instructions_per_unit", "stages")


def git(*args: str) -> str:
    done = subprocess.run(["git", *args], capture_output=True, text=True)
    if done.returncode != 0:
        raise ValueError(f"git {' '.join(args)}: {done.stderr.strip()}")
    return done.stdout


def record(args: argparse.Namespace) -> None:
    sides = {}
    for side, path in (("base", args.base_counts), ("head", args.head_counts)):
        _, results = read_side(path)
        sides[side] = [
            {name: r[name] for name in KEPT_FIELDS if name in r}
            for _, r in sorted(results.items())
        ]
    json.dump({
        "note_version": NOTE_VERSION,
        "base": args.base,
        "results": sides,
    }, sys.stdout, indent=1, sort_keys=True)
    sys.stdout.write("\n")


def note_of(commit: str) -> dict | None:
    shown = subprocess.run(
        ["git", "notes", f"--ref={NOTES_REF}", "show", commit],
        capture_output=True, text=True)
    if shown.returncode != 0:
        return None
    note = json.loads(shown.stdout)
    if note.get("note_version") != NOTE_VERSION:
        raise ValueError(f"the note on {commit} is not of version {NOTE_VERSION}")
    return note


def factors_of(note: dict) -> dict[tuple[str, ...], float]:
    """Each measurement's head over its base, and each stage's of a design's
    build, where both sides counted it."""
    sides = [
        {(r["case"], r["measure"], r["backend"]): r
         for r in note["results"][side] if r["status"] == STATUS_OK}
        for side in ("base", "head")
    ]
    factors: dict[tuple[str, ...], float] = {}
    for key in sides[0].keys() & sides[1].keys():
        b, h = sides[0][key], sides[1][key]
        if b["instructions_per_unit"] > 0.0:
            factors[key] = h["instructions_per_unit"] / b["instructions_per_unit"]
        for stage, factor in stage_factors(b, h).items():
            factors[(*key, stage)] = factor
    return factors


def read(args: argparse.Namespace) -> None:
    if not args.no_fetch:
        git("fetch", "origin", f"+{NOTES_REF}:{NOTES_REF}")
    merges = git(
        "rev-list", "--first-parent", "--reverse", args.range).split()
    designs: set[tuple[str, str, str]] = set()
    product: dict[tuple[str, ...], float] = defaultdict(lambda: 1.0)
    counted_in: dict[tuple[str, ...], int] = defaultdict(int)
    rows: list[tuple[str, dict[tuple[str, ...], float]]] = []
    unnoted: list[str] = []
    for merge in merges:
        subject = git("log", "-1", "--format=%h %s", merge).strip()
        note = note_of(merge)
        parent = git("rev-parse", f"{merge}^").strip()
        if note is None or note["base"] != parent:
            unnoted.append(subject)
            continue
        factors = factors_of(note)
        for key, factor in factors.items():
            product[key] *= factor
            counted_in[key] += 1
        for r in note["results"]["head"]:
            if r["family"] == DESIGN_FAMILY:
                designs.add((r["case"], r["measure"], r["backend"]))
        rows.append((subject, factors))

    columns = sorted(designs)
    print(f"{len(merges)} merges in {args.range}, {len(rows)} with counts")
    print()
    print("Each real design, each merge's change against its parent:")
    print()
    print(" | ".join(["merge"] + ["/".join(c) for c in columns]))
    for subject, factors in rows:
        cells = [
            percent(factors[c]) if c in factors else "-" for c in columns]
        print(" | ".join([subject[:60]] + cells))
    print(" | ".join(
        ["product, of the merges counted"]
        + [f"{percent(product[c])} of {counted_in[c]}" if c in counted_in
           else "-" for c in columns]))
    print()
    print("Each design's build by stage, the product and how many merges "
          "counted it:")
    print()
    for design in columns:
        if design[1] != MEASURE_BUILD:
            continue
        for stage in BUILD_SPLIT_ROWS:
            key = (*design, stage)
            if key in counted_in:
                print(f"{'/'.join(design)} | {stage} | "
                      f"{percent(product[key])} | {counted_in[key]}")
    if args.corpus:
        print()
        print("Each corpus case, the product and how many merges counted it:")
        print()
        for key in sorted(k for k in counted_in if len(k) == 3
                          and k not in designs):
            print(f"{'/'.join(key)} | {percent(product[key])} | "
                  f"{counted_in[key]}")
    if unnoted:
        print()
        print("Merges with no counts against their parent, which every "
              "product above leaves out:")
        for subject in unnoted:
            print(f"- {subject}")
    for column in columns:
        uncounted = [subject for subject, factors in rows
                     if column not in factors]
        if uncounted:
            print()
            print(f"Merges whose counts have none of {'/'.join(column)}, "
                  "which its product leaves out:")
            for subject in uncounted:
                print(f"- {subject}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    commands = parser.add_subparsers(dest="command", required=True)
    recording = commands.add_parser("record")
    recording.add_argument("base_counts")
    recording.add_argument("head_counts")
    recording.add_argument("--base", required=True)
    reading = commands.add_parser("read")
    reading.add_argument("range")
    reading.add_argument("--no-fetch", action="store_true")
    reading.add_argument("--corpus", action="store_true")
    args = parser.parse_args()
    try:
        (record if args.command == "record" else read)(args)
    except (OSError, ValueError, KeyError) as e:
        print(f"error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
