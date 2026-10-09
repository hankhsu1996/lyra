# Profiling

How to measure where a simulation spends its time, and how to read what comes back. Where the
compiler's own time and memory go is the next section, and needs nothing but the compiler and a
script in this repository.

## Where a compile's cost went

The compiler records its own run when asked:

```bash
./bazel-bin/lyra build --top Top --time-trace trace.json --stats-file stats.json design.sv
```

`trace.json` is a Chrome trace: open it in ui.perfetto.dev, where each thread is a lane and each
span names the stage, unit, scope or function it timed. Its `Total` rows sum each span name over the
run, which is the quickest answer to which step took the time. `stats.json` holds what a span
cannot: each stage's peak resident memory, every file each unit left behind and whether this run
made it, and how long every tool the build ran took. What such a tool holds in memory is not in it:
the system's figure for a spawned child starts at the compiler's own peak, so measure a host compile
or a link from outside the run. Spans under 500 us are left out of the trace;
`--time-trace-granularity 0` keeps all of them.

```bash
python3 tools/trace/report.py summary --stats stats.json --trace trace.json
python3 tools/trace/report.py compare base.stats.json stats.json
```

`summary` prints one run; `compare` prints what grew against a base run and exits with how many
things did, so a check can be built on it.

## Ask what you are measuring before you measure

A program Lyra produces has two halves compiled separately: the design's own translation unit, and
the runtime library it links. The runtime ships optimized whatever built the compiler, so it is
never the variable. The design's unit is compiled unoptimized unless `--release` says otherwise,
because iterating pays that compile on every edit.

**A profile of the default build is a profile of unoptimized code**, and its cost distribution is
not the optimized one -- inlining collapses whole layers, and the functions that dominate without it
disappear. Profile `--release` builds, or the ranking is of something nobody runs.

## Tools

Callgrind, from Valgrind. It counts instructions rather than sampling, so two runs of a
deterministic workload produce the same numbers and a single run is enough. `kcachegrind` opens its
output interactively.

```bash
sudo apt install -y valgrind kcachegrind
```

`perf` is the better tool on bare metal, but under WSL2 the kernel is Microsoft's and does not match
Ubuntu's `linux-tools-*` packages, so it is not part of the standard workflow here.

## Producing something worth profiling

```bash
bazel build //:lyra
./bazel-bin/lyra build --top Top --release -o out/program design.sv
```

The cases under `tests/benchmark/` are the designs to measure, each isolating one cost family:
`scheduling/` for the pressure a clocked design puts on the engine, `memory/` and `arithmetic/` and
`control-flow/` for narrower generated-code behavior, and `compile/` for compile-time rather than
run-time cost. Keep to one case across a before/after pair -- changing it invalidates the
comparison.

A case that times a simulation takes its amount of work from a plusarg, so one build profiles at any
size:

```bash
out/program +work=2000
```

Pick an amount that finishes in well under a second natively. Callgrind costs 20-50x, so a workload
that runs for a second natively takes a minute under it.

## Running it

```bash
valgrind --tool=callgrind --callgrind-out-file=callgrind.out out/program
```

Profile the program directly rather than `lyra run`. Valgrind follows only the process it launches,
and `run` builds a program and executes it as a child, so profiling `run` measures the compiler.
Passing `--trace-children=yes` captures both, in separate files, if compile cost is what you want.

Callgrind output is a local artifact; it is git-ignored and belongs nowhere but the working tree.

## Reading it

Two views, answering different questions. Look at both.

```bash
callgrind_annotate --inclusive=no  callgrind.out | head -40   # where instructions execute
callgrind_annotate --inclusive=yes callgrind.out | head -40   # which call paths own the work
```

- **Ir** is Callgrind's own count of simulated instructions. It is deterministic and good for
  comparison, but it is not a hardware counter: cache misses and branch mispredictions are invisible
  to it.
- **Self cost** is Ir in a function's own code. High self cost means that body is expensive.
- **Inclusive cost** is self plus callees. High inclusive cost usually means the function owns a hot
  path, not that its body needs work -- trace into the callees before optimizing it.

Start from the top self-cost entries and walk up the caller chain to find which part of the design
or runtime owns that cost. Standard-library internals near the top are rarely a problem with the
library: they point at allocation churn or an abstraction in the path that reaches them.

A function that is cheap per call but appears high is telling you about frequency, not about its
body. That distinction decides the fix: a costly body wants a better implementation, a frequent call
wants a different algorithm.

## Comparing before and after

Reprofile the same case at the same amount of work and compare both the total and the ranking. A
lower total does not mean the bottleneck moved -- it may have shrunk proportionally and still be
first. If the top entries are in the same order, the shape of the cost did not change.

For wall-clock rather than instruction counts, `hyperfine` compares built programs directly and
reports the spread, which matters because a difference smaller than the run-to-run deviation is not
a difference.
