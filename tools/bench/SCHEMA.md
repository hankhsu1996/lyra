# Benchmark Result Schema (v8)

Schema for the `bench.json` output produced by `run_benchmarks.py`. The corpus it reads and the
reasoning behind the shape are in `docs/decisions/benchmark-case-shape.md`; this file describes only
what a run writes out.

## What the runner can say

Every number here is observed from outside the compiler: a wall time around a subprocess, the
instructions callgrind counted in one, a file size, an exit status. Nothing is read out of the
compiler's internals, which is what keeps a measurement independent of whether any diagnostic
happens to be available.

A run either times or counts, and `metric` says which. A timing run measures Lyra on each backend
and the reference simulator, and its number is `rate`. A count run, `--count`, measures Lyra alone,
and its number is `instructions_per_unit`. A count run given `--design` counts real designs instead
of the corpus, and each design has two results, its `build` and its `run`.

## Top-level object

| Field            | Type   | Description                                                 |
| ---------------- | ------ | ----------------------------------------------------------- |
| `schema_version` | int    | Always `8`.                                                 |
| `git`            | string | The revision the measured compiler was built from.          |
| `metric`         | string | `time` or `instructions` -- what the run measured.          |
| `target_seconds` | float  | The duration each simulation was scaled to reach, if timed. |
| `timestamp`      | string | ISO 8601 UTC timestamp of the run.                          |
| `results`        | array  | One object per case, measure, tool and backend.             |

## Result object

All fields are always present. A field that does not apply carries its zero value.

| Field                   | Type          | Description                                                                                              |
| ----------------------- | ------------- | -------------------------------------------------------------------------------------------------------- |
| `case`                  | string        | The case's name, unique across the corpus, or the suite's for a design.                                  |
| `family`                | string        | The cost family it isolates, which is its directory; `designs` for a design.                             |
| `measure`               | string        | `run` or `build` -- which of the two the measurement covers.                                             |
| `work_unit`             | string        | What one unit of this case's work is called.                                                             |
| `tool`                  | string        | `lyra` or `verilator`.                                                                                   |
| `backend`               | string        | `llvm` or `cpp` for Lyra; empty for the reference simulator.                                             |
| `status`                | string        | `ok` or `error`.                                                                                         |
| `work`                  | int           | How many units the final timing covered, or the lower count's.                                           |
| `seconds`               | float         | What the final timing took.                                                                              |
| `rate`                  | float         | Units per second, with the fixed part removed. **A timing's number.**                                    |
| `instructions_per_unit` | float         | Instructions per unit, the fixed part removed. **A count's number.**                                     |
| `resolution_percent`    | float or null | How far two counts of the same work differed, as a percent of the work counted; null where counted once. |
| `build_s`               | float         | What building the case cost.                                                                             |
| `binary_kb`             | int           | Size of the produced binary in KB.                                                                       |
| `readings`              | array         | Each measurement, in order: `[work, seconds]` or `[work, count]`.                                        |
| `growth`                | float         | The power of `work` a timing's cost grew as; zero where unseen.                                          |
| `detail`                | string        | Which step failed and how it ended; empty when `ok`.                                                     |
| `output`                | string        | The last lines that step printed; empty when `ok`.                                                       |

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

## Reading `instructions_per_unit`

**A count is taken at `work` units and at twice that, and the difference is divided by `work`.**
What a run executes once -- starting, constructing the design, a compiler reading its prelude -- is
in both and cancels exactly. For a `run` case the count is of the program; for a `build` case it is
of Lyra building the design, and the host compiler Lyra starts is not counted.

The lower amount is counted a second time, and `resolution_percent` is how far the two disagree as a
share of the work counted, which says how near a case is to that limit. What a build does before any
of the design can be nearly all of each count, so even a small disagreement between two counts can
be much of the difference. A case whose resolution is past a third of the least change worth marking
is an `error`, with what it measured, and so is one whose higher amount counted no more.

A design's counts are totals rather than differences, `work` is 1, and `resolution_percent` is null:
the suite fixes how much its quick check does, and what a run does once is part of what it costs.

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
signal, outlived the time limit, could not be started, or produced no binary, a count callgrind
wrote no total for, or a count whose two readings of the same work disagree by too much to resolve a
change worth marking. A simulation case whose backend's store could not be prepared is an `error`
naming that, and so is each of a design's two results where its build failed. `detail` names the
step and how it ended -- `build exited with status 1`,
`run at +work=4 was killed by SIGSEGV (status 139)` -- and `output` holds the end of what that step
printed. The fields a measurement fills stay at zero, so `status` and never a zero number is what
says a case has none.

**An `error` costs only its own case.** The run goes on to every other one, and the file is
rewritten as each result arrives, so a run that is itself stopped leaves the results it had. A case
it never reached has no object at all, and neither has any case under a tool that was not on `PATH`.

## Exit status and the printed report

The run exits 0 when every case was measured by every tool it had, and 1 when any result is an
`error` -- after the report is printed and the file written, so a failed run still publishes every
number it obtained. A duration or a count never fails a run by its size, since what a number means
is a reader's call, and a duration is also a report on the machine that produced it. A count whose
case cannot resolve a change worth marking is an `error` of that case. Verilator being absent does
not fail it either.

The report has a row for every case on every backend whatever happened to it. Where a tool has no
number the cell says `failed` or `not run` rather than a dash, a line under the heading counts both,
and a "Not measured" section names each one with its `detail` and `output`.

## Compatibility rules

- Consumers must check `schema_version` before parsing.
- Fields may be added; consumers should ignore unknown ones.
- Removing a field or changing its type requires a version bump.
- v8 added `resolution_percent`, and a design has a result per measure, which a v7 consumer keying
  results by case and backend would read as one.
- v7 added `metric`, `backend` and `instructions_per_unit`, and a timing run now has one Lyra result
  per backend, which a v6 consumer keying results by case and tool would read as one.
- `output` was added within v6, and `detail` changed what it says without changing its type.
- The v5 field `probes` does not appear; it was the length of `readings`.
- The v4 fields `intent`, `primary`, `secondary`, `focus`, `optimized`, `params`, `rss_max_mb`,
  `wall_s`, `sim_s` and `subcategory` do not appear, and neither do the scale profiles and tiers
  that `params` recorded.
