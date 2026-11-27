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

Each design's build is also split by the stages of `BUILD_STAGES`, in a
collapsed table of its own and never marked: a stage says where a move of the
build came from, and work moved from one stage to another is no move of the
build. A stage's change is shown only where both builds were split into the
same stages, since a stage one of them lacks puts its work in that one's rest.
Both sides are split by the table of the comparison that runs, which on a merge
is head's, so a stage missing from head's split is lost, as a measurement is;
base was built before that table, and a stage it lacks is only a gap.

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
    BUILD_SPLIT_ROWS,
    BUILD_STAGES,
    CELL_FAILED,
    CELL_NOT_RUN,
    DESIGN_FAMILY,
    LYRA,
    MARK_PERCENT,
    MEASURE_BUILD,
    STATUS_OK,
    read_count_file,
)


def read_side(path: str) -> tuple[str, dict[tuple[str, str, str], dict]]:
    """The revision a side was counted at and Lyra's results, by measurement.

    A hand-written program a case holds is counted beside Lyra in each run,
    and is the same program on both sides, so what it counts is no move of
    the compiler's.
    """
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
            if r["tool"] == LYRA:
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


def percent(factor: float) -> str:
    """Head over base, as the change a table shows."""
    return f"{(factor - 1.0) * 100.0:+.1f}%"


def stage_factors(base: dict, head: dict) -> dict[str, float]:
    """Each stage's head over base, where both builds were split into the same
    stages; a stage one of them lacks puts its work in that one's rest, so no
    row of two different splits compares."""
    b, h = base.get("stages", {}), head.get("stages", {})
    if not b or b.keys() != h.keys():
        return {}
    return {stage: h[stage] / count for stage, count in b.items() if count > 0}


def stage_cell(result: dict | None, stage: str) -> str:
    """A stage's count of a design's build, as a cell."""
    if not counted(result):
        return cell(result)
    count = result.get("stages", {}).get(stage)
    return CELL_NOT_RUN if count is None else f"{count:,}"


def stage_rows(
    base: dict[tuple[str, str, str], dict],
    head: dict[tuple[str, str, str], dict],
) -> tuple[list[tuple], list[tuple[str, str]]]:
    """Each design build's stages, base against head, and why each stage of a
    build head counted is missing from its split."""
    entries = dict(BUILD_STAGES)
    rows: list[tuple] = []
    lost: list[tuple[str, str]] = []
    for key in sorted(base | head):
        b, h = base.get(key), head.get(key)
        if key[1] != MEASURE_BUILD or (h or b)["family"] != DESIGN_FAMILY:
            continue
        factors = stage_factors(b, h) if counted(b) and counted(h) else {}
        split = h.get("stages", {}) if counted(h) else None
        for stage in BUILD_SPLIT_ROWS:
            if split is not None and stage not in split:
                lost.append((
                    f"{'/'.join(key)}: {stage}",
                    f"{entries[stage]} is not in the profile" if split else
                    "the build could not be split, and the count job's log "
                    "says why"))
            text = percent(factors[stage]) if stage in factors else "-"
            rows.append((0.0, key[0], key[2], stage, stage_cell(b, stage),
                         stage_cell(h, stage), text))
    return rows, lost


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
    stages, stages_lost = stage_rows(base, head)
    lost = lost or bool(stages_lost)

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
    if stages:
        print()
        print("<details><summary>Where each build went</summary>")
        print()
        print_table(header.format("Design", "Stage"), stages)
        print()
        print("</details>")
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
    if failed or stages_lost:
        print()
        print("### Not counted on head")
        print()
        for (case, measure, backend), r in failed:
            print(f"- **{case}/{measure}/{backend}**: {r['detail']}")
        for stage, why in stages_lost:
            print(f"- **{stage}**: {why}")
    print()

    if args.github_output:
        with open(args.github_output, "a") as f:
            for name, value in (("moved", moved), ("worse", worse),
                                ("lost", lost)):
                f.write(f"{name}={'true' if value else 'false'}\n")


if __name__ == "__main__":
    main()
