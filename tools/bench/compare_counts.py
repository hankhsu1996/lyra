#!/usr/bin/env python3
"""Compare two instruction-count runs.

Reads the result files `run_benchmarks.py --count` wrote for two compilers --
a base and a head -- and prints how many instructions each measurement cost
each of them and how far that moved: the real designs first, since they are
what someone building a design waits on, and then the corpus cases, each one
cost family, which say which path a move came from.

A measurement is marked where it moved by `MARK_PERCENT` or more, or where base
counted it and head could not. A corpus case whose own two counts disagree by
too much to resolve that has no count, so a mark is never a count's
disagreement with itself. A corpus case that moved by less is listed only in
the collapsed table.

The exit status is 0 whatever moved: what a run does about a number is for
whoever reads the outputs to decide. It is 1 only when a file cannot be read
as a count run.

Usage:
    python3 tools/bench/compare_counts.py BASE HEAD [--github-output PATH]

BASE and HEAD are each a result file, or a directory whose result files are
read together.
"""

import argparse
import sys
from pathlib import Path

from run_benchmarks import (
    CELL_FAILED,
    CELL_NOT_RUN,
    DESIGN_FAMILY,
    MARK_PERCENT,
    STATUS_OK,
    read_count_file,
)


def read_side(path: str) -> tuple[str, dict[tuple[str, str, str], dict]]:
    """The revision a side was counted at and its results, by measurement."""
    given = Path(path)
    files = sorted(given.glob("*.json")) if given.is_dir() else [given]
    if not files:
        raise ValueError(f"{path} holds no result file")
    revisions = set()
    results: dict[tuple[str, str, str], dict] = {}
    for file in files:
        data = read_count_file(str(file))
        revisions.add(data["git"])
        for r in data["results"]:
            results[(r["case"], r["measure"], r["backend"])] = r
    if len(revisions) != 1:
        raise ValueError(
            f"{path} holds counts of more than one revision: "
            f"{', '.join(sorted(revisions))}")
    return revisions.pop(), results


def counted(result: dict | None) -> bool:
    return result is not None and result["status"] == STATUS_OK


def change_percent(base: dict | None, head: dict | None) -> float | None:
    """How far head moved from base, or None where either has no count."""
    if (not counted(base) or not counted(head)
            or base["instructions_per_unit"] <= 0.0):
        return None
    return (head["instructions_per_unit"] / base["instructions_per_unit"]
            - 1.0) * 100.0


def cell(result: dict | None) -> str:
    if result is None:
        return CELL_NOT_RUN
    if result["status"] != STATUS_OK:
        return CELL_FAILED
    return f"{result['instructions_per_unit']:,.0f}"


def print_table(header: str, rows: list[tuple]) -> None:
    print(header)
    print("|------|---------|------|-----:|-----:|-------:|")
    for _, case, backend, unit, b, h, text in rows:
        print(f"| {case} | {backend} | {unit} | {b} | {h} | {text} |")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("base")
    parser.add_argument("head")
    parser.add_argument(
        "--github-output", default=None,
        help="Append `moved=`, `worse=` and `lost=`, each `true` or `false`, "
             "to this file: lost where head has no count of a measurement it "
             f"made or base counted, worse where one rose by {MARK_PERCENT:g}% "
             "or more, moved where one changed by as much either way or base "
             "counted one head could not")
    args = parser.parse_args()

    try:
        base_revision, base = read_side(args.base)
        head_revision, head = read_side(args.head)
    except (OSError, ValueError, KeyError) as e:
        print(f"error: {e}", file=sys.stderr)
        sys.exit(1)

    moved = worse = lost = False
    primary: list[tuple] = []
    secondary: list[tuple] = []
    secondary_marked: list[tuple] = []
    for key in sorted(base | head):
        b, h = base.get(key), head.get(key)
        change = change_percent(b, h)
        # A measurement head tried and could not count is red whether or not
        # base could: one that can never be counted is a case nobody can read,
        # and staying red is what has someone fix it or take it out. One head
        # has no row of at all is red where base counted it. Only what this
        # change did is marked, so one that could not be counted on either
        # side is red without being news on every merge.
        uncounted = not counted(h) if h is not None else counted(b)
        case_lost = counted(b) and not counted(h)
        case_worse = change is not None and change >= MARK_PERCENT
        marked = case_lost or (
            change is not None and abs(change) >= MARK_PERCENT)
        lost = lost or uncounted
        worse = worse or case_worse
        moved = moved or marked
        text = "-" if change is None else f"{change:+.1f}%"
        if marked:
            text = f"**{text}**"
        either = h or b
        # Largest change first, either way; a count lost on head first of all,
        # since that is a measurement that no longer builds or runs.
        order = float("inf") if case_lost else abs(change or 0.0)
        row = (order, key[0], key[2], either["work_unit"], cell(b), cell(h),
               text)
        if either["family"] == DESIGN_FAMILY:
            primary.append(row)
        else:
            secondary.append(row)
            if marked:
                secondary_marked.append(row)
    for rows in (primary, secondary, secondary_marked):
        rows.sort(key=lambda row: (-row[0], row[1], row[2]))

    print("## Instruction count")
    print()
    print(
        f"> base `{base_revision}` -> head `{head_revision}` | under "
        f"callgrind | a change of {MARK_PERCENT:g}% or more is in bold")
    header = "| {} | Backend | {} | Base | Head | Change |"
    if primary:
        print()
        print("### Real designs")
        print()
        print("Instructions Lyra executes building each design, and that its "
              "program executes running the design's quick check.")
        print()
        print_table(header.format("Design", "Measure"), primary)
    if secondary:
        print()
        print("### Cost families")
        print()
        print("Instructions per unit of each corpus case's work.")
        print()
        if secondary_marked:
            print_table(header.format("Case", "Unit"), secondary_marked)
            print()
        print("<details><summary>Every case</summary>")
        print()
        print_table(header.format("Case", "Unit"), secondary)
        print()
        print("</details>")

    failed = [
        (key, r) for key, r in sorted(head.items())
        if r["status"] != STATUS_OK
    ]
    if failed:
        print()
        print("### Not counted on head")
        print()
        for (case, measure, backend), r in failed:
            print(f"- **{case}/{measure}/{backend}**: {r['detail']}")
    print()

    if args.github_output:
        with open(args.github_output, "a") as f:
            for name, value in (("moved", moved), ("worse", worse),
                                ("lost", lost)):
                f.write(f"{name}={'true' if value else 'false'}\n")


if __name__ == "__main__":
    main()
