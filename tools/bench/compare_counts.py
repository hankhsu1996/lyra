#!/usr/bin/env python3
"""Compare two instruction-count runs of the benchmark corpus.

Reads the result files `run_benchmarks.py --count` wrote for two compilers --
a base and a head -- and prints, per backend, how many instructions one unit of
each case's work cost each of them and how far that moved.

The exit status is 0 whatever moved: what a run does about a number is for
whoever reads the outputs to decide. It is 1 only when a file cannot be read
as a count run.

Usage:
    python3 tools/bench/compare_counts.py BASE.json HEAD.json
                                          [--github-output PATH]
"""

import argparse
import sys

from run_benchmarks import (
    CELL_FAILED,
    CELL_NOT_RUN,
    STATUS_OK,
    read_count_file,
)

# Two counts of one compiler agree to a few hundredths of a percent at most, so
# what this leaves unmarked is not noise but a change too small to be worth
# marking.
THRESHOLD_PERCENT = 2.0


def by_case_and_backend(data: dict) -> dict[tuple[str, str], dict]:
    return {(r["case"], r["backend"]): r for r in data["results"]}


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


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("base")
    parser.add_argument("head")
    parser.add_argument(
        "--github-output", default=None,
        help="Append `moved=`, `worse=` and `lost=`, each `true` or `false`, "
             "to this file: lost where a case counted on base and not on "
             "head, worse where one rose by the threshold or more, moved "
             "where either holds or one fell by as much")
    args = parser.parse_args()

    try:
        base_data = read_count_file(args.base)
        head_data = read_count_file(args.head)
    except (OSError, ValueError, KeyError) as e:
        print(f"error: {e}", file=sys.stderr)
        sys.exit(1)
    base = by_case_and_backend(base_data)
    head = by_case_and_backend(head_data)

    moved = worse = lost = False
    print("## Instruction count")
    print()
    print(
        f"> base `{base_data['git']}` -> head `{head_data['git']}` | "
        f"instructions per unit of work, under callgrind | "
        f"a change of {THRESHOLD_PERCENT:g}% or more is in bold")

    backends = sorted({backend for _, backend in base | head})
    for backend in backends:
        rows = []
        for key in sorted(k for k in base | head if k[1] == backend):
            b, h = base.get(key), head.get(key)
            change = change_percent(b, h)
            case_lost = counted(b) and not counted(h)
            case_worse = change is not None and change >= THRESHOLD_PERCENT
            marked = case_lost or (
                change is not None and abs(change) >= THRESHOLD_PERCENT)
            lost = lost or case_lost
            worse = worse or case_worse
            moved = moved or marked
            text = "-" if change is None else f"{change:+.1f}%"
            if marked:
                text = f"**{text}**"
            unit = (h or b)["work_unit"]
            # Largest change first, either way; a count lost on head first of
            # all, since that is a case that no longer builds or runs.
            order = float("inf") if case_lost else abs(change or 0.0)
            rows.append((order, key[0], unit, cell(b), cell(h), text))
        rows.sort(key=lambda row: (-row[0], row[1]))

        print()
        print(f"### {backend}")
        print()
        print("| Case | Unit | Base | Head | Change |")
        print("|------|------|-----:|-----:|-------:|")
        for _, case, unit, b, h, text in rows:
            print(f"| {case} | {unit} | {b} | {h} | {text} |")

    failed = [
        (key, r) for key, r in sorted(head.items())
        if r["status"] != STATUS_OK
    ]
    if failed:
        print()
        print("### Not counted on head")
        print()
        for (case, backend), r in failed:
            print(f"- **{case}/{backend}**: {r['detail']}")
    print()

    if args.github_output:
        with open(args.github_output, "a") as f:
            for name, value in (("moved", moved), ("worse", worse),
                                ("lost", lost)):
                f.write(f"{name}={'true' if value else 'false'}\n")


if __name__ == "__main__":
    main()
