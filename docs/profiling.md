# Profiling

How to measure where a simulation spends its time, and how to read what comes back.

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
./bazel-bin/lyra compile --no-project --top Top --release -o out design.sv
```

The fixtures under `tools/bench/fixtures/` are the designs to measure: `simulation-engine/` for
scheduler pressure, `scheduling/` and `process-kernel/` for narrower engine behavior, and `compile/`
for compile-time rather than run-time cost. Keep to one fixture across a before/after pair --
changing it invalidates the comparison.

Pick a cycle count that finishes in well under a second natively. Callgrind costs 20-50x, so a
workload that runs for a second natively takes a minute under it.

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

Reprofile the same fixture at the same cycle count and compare both the total and the ranking. A
lower total does not mean the bottleneck moved -- it may have shrunk proportionally and still be
first. If the top entries are in the same order, the shape of the cost did not change.

For wall-clock rather than instruction counts, `hyperfine` compares built programs directly and
reports the spread, which matters because a difference smaller than the run-to-run deviation is not
a difference.
