# Benchmark Result Schema (v6)

Schema for the `bench.json` output produced by `run_benchmarks.py`. The corpus it reads and the
reasoning behind the shape are in `docs/decisions/benchmark-case-shape.md`; this file describes only
what a run writes out.

## What the runner can say

Every number here is observed from outside the compiler: a wall time around a subprocess, a file
size, an exit status. Nothing is read out of the compiler's internals, which is what keeps a
measurement independent of whether any diagnostic happens to be available.

## Top-level object

| Field            | Type   | Description                                       |
| ---------------- | ------ | ------------------------------------------------- |
| `schema_version` | int    | Always `6`.                                       |
| `git`            | string | Short git SHA of the tested commit.               |
| `target_seconds` | float  | The duration each simulation was scaled to reach. |
| `timestamp`      | string | ISO 8601 UTC timestamp of the run.                |
| `results`        | array  | One object per case and tool.                     |

## Result object

All fields are always present. A field that does not apply carries its zero value.

| Field       | Type   | Description                                                    |
| ----------- | ------ | -------------------------------------------------------------- |
| `case`      | string | The case's name, unique across the corpus.                     |
| `family`    | string | The cost family it isolates, which is its directory.           |
| `measure`   | string | `run` or `build` -- which of the two the timing covers.        |
| `work_unit` | string | What one unit of this case's work is called.                   |
| `tool`      | string | `lyra` or `verilator`.                                         |
| `status`    | string | `ok` or `error`.                                               |
| `work`      | int    | How many units the final measurement covered.                  |
| `seconds`   | float  | What that measurement took.                                    |
| `rate`      | float  | Units per second, with the fixed part removed. **The number.** |
| `build_s`   | float  | What building the case cost.                                   |
| `binary_kb` | int    | Size of the produced binary in KB.                             |
| `readings`  | array  | Every measurement taken, in order, each `[work, seconds]`.     |
| `growth`    | float  | The power of `work` the cost grew as; zero where unseen.       |
| `detail`    | string | Which step failed and how it ended; empty when `ok`.           |
| `output`    | string | The last lines that step printed; empty when `ok`.             |

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

## Reading `growth`

**A rate divides the amount out only where the cost was proportional to it, and `growth` says
whether it was.** Repeating a simulation's work is proportional by construction. A build's work is
the design, and a design twice the size may cost four times as much; there the rate belongs to the
`work` it was taken at, and two tools given different amounts were not measured on the same thing.

`growth` is the exponent read off the last measurement and the nearest one at most half its amount,
once the fixed part is taken out of each: `1.0` is a line and `2.0` a square. It is zero where the
work did not show in both.

## Statuses

`error` is a case one tool could not measure: a build or a run that exited non-zero, was killed by a
signal, outlived the time limit, could not be started, or produced no binary. `detail` names the
step and how it ended -- `build exited with status 1`,
`run at +work=4 was killed by SIGSEGV (status 139)` -- and `output` holds the end of what that step
printed. The fields a measurement fills stay at zero, so `status` and never a zero `rate` is what
says a case has no number.

**An `error` costs only its own case.** The run goes on to every other one, and the file is
rewritten as each result arrives, so a run that is itself stopped leaves the results it had. A case
it never reached has no object at all, and neither has any case under a tool that was not on `PATH`.

## Exit status and the printed report

The run exits 0 when every case was measured by every tool it had, and 1 when any result is an
`error` -- after the report is printed and the file written, so a failed run still publishes every
number it obtained. A duration never fails a run: that would be reporting on the machine that
produced it. Verilator being absent does not either.

The report has a row for every case whatever happened to it. Where a tool has no number the rate
cell says `failed` or `not run` rather than a dash, a line under the heading counts both, and a "Not
measured" section names each one with its `detail` and `output`.

## Compatibility rules

- Consumers must check `schema_version` before parsing.
- Fields may be added; consumers should ignore unknown ones.
- Removing a field or changing its type requires a version bump.
- `output` was added within v6, and `detail` changed what it says without changing its type.
- The v5 field `probes` does not appear; it was the length of `readings`.
- The v4 fields `intent`, `primary`, `secondary`, `focus`, `optimized`, `params`, `rss_max_mb`,
  `wall_s`, `sim_s` and `subcategory` do not appear, and neither do the scale profiles and tiers
  that `params` recorded.
