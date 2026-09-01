# Benchmark Result Schema (v5)

Schema for the `bench.json` output produced by `run_benchmarks.py`. The corpus it reads and the
reasoning behind the shape are in `docs/decisions/benchmark-case-shape.md`; this file describes only
what a run writes out.

## What the runner can say

Every number here is observed from outside the compiler: a wall time around a subprocess, a file
size, an exit status. Nothing is read out of the compiler's internals, which is what keeps a
measurement independent of whether any diagnostic happens to be available.

## Top-level object

| Field            | Type   | Description                                        |
| ---------------- | ------ | -------------------------------------------------- |
| `schema_version` | int    | Always `5`.                                        |
| `git`            | string | Short git SHA of the tested commit.                |
| `target_seconds` | float  | The duration each measurement was scaled to reach. |
| `timestamp`      | string | ISO 8601 UTC timestamp of the run.                 |
| `results`        | array  | One object per case and tool.                      |

## Result object

All fields are always present. A field that does not apply carries its zero value.

| Field       | Type   | Description                                                    |
| ----------- | ------ | -------------------------------------------------------------- |
| `case`      | string | The case's name, unique across the corpus.                     |
| `family`    | string | The cost family it isolates, which is its directory.           |
| `measure`   | string | `run` or `build` -- which of the two the timing covers.        |
| `work_unit` | string | What one unit of this case's work is called.                   |
| `tool`      | string | `lyra` or `verilator`.                                         |
| `status`    | string | `ok`, `unsupported`, or `error`.                               |
| `work`      | int    | How many units the final measurement covered.                  |
| `seconds`   | float  | What that measurement took.                                    |
| `rate`      | float  | Units per second, with the fixed part removed. **The number.** |
| `build_s`   | float  | What building the case cost.                                   |
| `binary_kb` | int    | Size of the produced binary in KB.                             |
| `probes`    | int    | How many measurements were taken to reach the target.          |
| `detail`    | string | The diagnostic behind a non-`ok` status; empty when `ok`.      |

## Reading `rate`

**`rate` is the number to compare; `seconds` and `work` are how it was arrived at.** A duration
means nothing without the amount beside it, so two runs at different amounts cannot be compared and
an amount can never change without invalidating the history. A rate divides that out, which is what
lets the runner choose the amount freely -- and it does, per tool, so a reference simulator hundreds
of times quicker is given hundreds of times the work rather than being measured over a millisecond
of mostly process startup.

`rate` also has the fixed part of a measurement removed: whatever a run costs before any of the
work, such as a process starting or a compiler reading its prelude. Two probes separate it, which is
why at least two are always taken. With only one -- a case so slow that a single unit already
overshoots the target -- there is nothing to separate and the whole duration is charged to the work,
so such a case's rate is a lower bound on its speed.

## Statuses

`unsupported` is a tool that cannot measure this case: Verilator absent from `PATH`, or a construct
Lyra does not yet carry out. It is reported and does not fail the run.

`error` is a build or run that failed, timed out, or produced no binary. Any `error` makes the run
exit non-zero. A duration never does: failing a run on a duration would be reporting on the machine
that produced it.

## Compatibility rules

- Consumers must check `schema_version` before parsing.
- Fields may be added; consumers should ignore unknown ones.
- Removing a field or changing its type requires a version bump.
- The v4 fields `intent`, `primary`, `secondary`, `focus`, `optimized`, `params`, `rss_max_mb`,
  `wall_s`, `sim_s` and `subcategory` do not appear, and neither do the scale profiles and tiers
  that `params` recorded.
