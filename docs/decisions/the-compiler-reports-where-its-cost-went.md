# The Compiler Reports Where Its Cost Went

## Date

2026-10-05

## Status

Accepted.

## Why this decision matters

A design that takes too long to compile, too much memory, or too many files has to be answered with
where: which stage, which unit, which scope. Without the compiler saying so, the only way to find
out is to compare builds -- an old compiler against a new one, a design against a smaller one --
which is a search, and a slow one: three regressions in one week were each found by a user's design,
and each was then attributed by building old commits. One run of the compiler already knows every
one of those answers while it is running. The requirement is that it can be asked to say them.

## What the field does

Read from source: clang's `-ftime-trace` (`llvm/lib/Support/TimeProfiler.cpp`), `-ftime-report`
(`Timer.cpp`), `-stats-file=` and `STATISTIC` (`Statistic.cpp`), and the driver's
`-fproc-stat-report` (`clang/lib/Driver/Driver.cpp`); rustc's `-Z self-profile` and `-Z time-passes`
(`rustc_data_structures/src/profiling.rs`); Verilator's `--stats` (`V3StatsReport.cpp`). From
documentation: Swift's `-stats-output-dir` and `process-stats-dir.py`, Bazel's `--profile`, Cargo's
`--timings`, MSVC's vcperf, rustc-perf and LLVM's compile-time tracker.

- **The compiler records and something else presents.** Each writes a machine-readable file per run,
  and aggregation, comparison and viewing live outside it: Perfetto and ClangBuildAnalyzer for
  clang's trace, `process-stats-dir.py` for Swift's statistics, measureme's tools for rustc's
  profile. A table printed by the compiler is the older shape (GCC, `-ftime-report`); presentation
  lives in the tool only where the tool is the orchestrator showing a build to a person.
- **Time is a trace, numbers are a separate file.** clang writes spans with `-ftime-trace` and named
  numbers with `-stats-file=`; Swift writes counters beside an event stream. A span is an entity and
  an interval; a number is a fact a comparison reads.
- **A thread is a lane.** One process with threads (LLVM, Bazel, Go) writes one trace with a lane
  per thread.
- **A child's peak memory is taken from the system's account of the reaped process**, by clang's
  driver and by Swift. That figure starts at the parent's high-water mark: measured here, a process
  holding 1 GiB spawned `/bin/true` and it was reported at 1,034 MiB. Their parent is a small
  driver, so it does not show. rustc records none.
- **Nobody attributes a peak to a phase.** rustc samples current memory at pass boundaries, which
  misses a peak inside a pass; Verilator reads the high-water mark at each stage, a running peak.
- **Regressions are judged on retired instructions and peak memory measured from outside**, never on
  wall time, with thresholds set by each benchmark's noise.

Where our conditions differ, as conditions:

- **A peak has to be attributed to a stage.** A compiler of one file has one file's peak. This one
  holds a design, and which stage holds its peak is the question a user's report asks. So a stage
  that runs alone lowers the process's high-water mark to what it holds as it starts, and reads it
  as it ends.
- **The compiler is also the orchestrator, and it is large.** It spawns the host compiles and the
  link, so how long each took is its to record. Their peak memory is not: under a parent holding a
  design, the system's figure for a child says how large the parent was.

## Decisions

### D1. Asked, a run writes where its time went as a trace, and its numbers as a second file

`--time-trace <file>` writes a Chrome trace through LLVM's profiler, the one clang's `-ftime-trace`
uses: a lane per thread, a span per stage, per unit within each stage a unit passes through, per
scope a unit lowers, and per function lowered or emitted. A span names what it works on. LLVM's own
spans land in the same lanes. A span shorter than `--time-trace-granularity` microseconds is left
out, 500 when not given, which is clang's default.

`--stats-file <file>` writes JSON holding what a span cannot: how many units ran at once, each stage
that runs alone with its peak resident memory, each unit with every file it left behind and whether
this run made it or took it from the store, and each tool the build ran with its wall time and CPU
time.

Nothing is recorded unless asked, and then each record is a single check.

### D2. The compiler prints no summary

A table, a slowest unit, a format for a duration are each a reading of the two files, and which one
is wanted depends on who is reading: a person, a comparison of two runs, a report from someone
else's machine. A summary the compiler printed would be a second statement of the trace, computed in
a second place. A person opens the trace in Perfetto as it is, and `tools/trace/report.py` is the
reading the project keeps: a summary of one run, and a comparison of two that exits with the number
of regressions. It compares counts exactly and sizes and peaks past a percentage and a floor, and
compares no time, which moves with the machine.

### D3. Stages that overlap are measured together

Stages of different units run at once on one heap, so a peak per such stage does not exist, and
lowering the high-water mark for one would corrupt another's reading. The section that runs them is
measured as one stage; a unit's size is what it left behind.

### D4. A tool the build runs is timed, and its memory is not recorded

Every tool the build runs is waited on in one place, and its wall time and the processor time the
system charged it are recorded there. Its peak memory is left out, because the only figure the
system offers is wrong under a large parent, and a number known to be wrong is worse than none. What
a host compile or a link holds is measured from outside the run, as the field's regression trackers
measure a compiler. The program a run executes is not a tool of the build and is not recorded.

### D5. A number the platform cannot give is absent

The compiler targets Linux and macOS. A stage's peak is read through what Linux offers; where the
platform offers no reading, the stage is recorded without one, and the rest of the file is written
as it would be. rustc does the same with its resident-size reading.

## Rejected alternatives

- **One file holding both.** It needs a trace writer of our own, because LLVM's events carry no
  numbers, and it loses LLVM's own spans. The only reason for it would be a gap in LLVM's interface.
- **A summary printed by the compiler.** See D2.
- **Peak memory sampled at stage boundaries.** It misses a peak inside a stage, which is the peak
  the question is about.
- **Starting each tool from a small helper process**, so that the system's figure for it is its own.
  It is how a child's peak could be had, and it is machinery no compiler surveyed carries, for a
  number a wrapper around the build already gives.

## Consequences

- Running host tools lives above the library the runtime also reaches: the runtime runs none.
- A comparison of two runs -- a merge gate holding a design's output to its size, or a report from a
  design outside the project -- reads these files and nothing the compiler prints.
