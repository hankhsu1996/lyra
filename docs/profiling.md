# Profiling

How to profile Lyra's simulation runtime and interpret the results.

## Tools

**Primary:** Callgrind (part of Valgrind). Instruction-level profiling with call-graph attribution. Works reliably on WSL2/Ubuntu 24.04 without kernel-tools matching issues.

**Viewer:** KCachegrind for interactive exploration of callgrind output. Available as `kcachegrind` on Ubuntu.

**Why not perf:** WSL2 uses a Microsoft kernel that doesn't match Ubuntu's `linux-tools-*` packages, causing recurring version mismatch issues. Use `perf` on bare-metal Linux if available, but don't depend on it for the standard workflow.

Install:

```bash
sudo apt install -y valgrind kcachegrind
```

## Canonical profiling fixture

All profiling runs use the **pipeline benchmark** (`tools/bench/fixtures/simulation-engine/clock-pipeline/`). This is the fixed profiling target -- do not use other designs for profiling unless comparing specific behaviors.

The fixture must stay fixed so that profiles are comparable across gap rankings and before/after measurements. Changing the fixture invalidates all previous profile baselines.

Why pipeline:

- 8-stage pipe, 10K cycles -- exercises the scheduler hot loop heavily
- Largest performance gap (11x vs Verilator at `-c opt`, post-G7f)
- Completes in ~50ms natively, ~2-3min under Callgrind
- Reproduces the same bottleneck pattern as large designs (ibex) but at 1/200th the cost

## AOT architecture and profiling implications

`lyra run` in AOT mode (the default) has two processes:

1. **Compiler process** (`lyra` binary) -- parses, lowers, emits object code, links the AOT binary, then forks/execs it
2. **Simulation process** (the AOT binary, e.g., `out/Top`) -- runs the actual simulation

Callgrind (and valgrind in general) profiles only the process it launches. If you profile `lyra run`, you get the **compiler** profile (~208M instructions for pipeline), not the simulation. The simulation runs in a child process that callgrind does not follow by default.

The simulation is where the runtime performance gap lives (~471M instructions for pipeline post-G7f). Always profile the AOT binary directly, not `lyra run`.

If you must profile through `lyra run` (e.g., to include compile time), use `--trace-children=yes`:

```bash
valgrind --tool=callgrind --trace-children=yes \
  --callgrind-out-file=callgrind.out \
  ./bazel-bin/lyra -C tools/bench/fixtures/simulation-engine/clock-pipeline run
```

This produces separate output files per process. The simulation process will have the higher instruction count.

## Workflow

### 1. Build

Compile the pipeline AOT binary with optimization. The AOT path produces a native executable with the Lyra runtime linked in.

**Always build with `-c opt`.** This applies to both profiling and benchmarking. Bazel's default `fastbuild` mode uses `-O0`, which prevents inlining and inflates STL/container overhead by 5x+. Profiles taken at `-O0` show a completely different cost distribution (dominated by iterator constructors, vector::empty checks, and function call overhead that vanishes at `-O2`) and cannot be used for hotspot ranking. The benchmark runner (`tools/bench/run_benchmarks.py`) enforces this by building with `-c opt` and always using the resulting optimized binary. See the `-c opt` discovery note in `docs/queues/performance.md` for the full story.

```bash
bazel build -c opt //:lyra
./bazel-bin/lyra -C tools/bench/fixtures/simulation-engine/clock-pipeline compile
```

### 2. Profile

Run under Callgrind. Output goes to a fixed filename for easy comparison.

```bash
cd tools/bench/fixtures/simulation-engine/clock-pipeline
valgrind --tool=callgrind \
  --callgrind-out-file=callgrind.out \
  out/Top
```

Expected runtime: ~2-3 minutes with `-c opt` (Callgrind adds ~20-50x overhead).

### 3. Inspect

Two complementary views. Always look at both -- they answer different questions.

**Top functions by self cost** (where instructions are actually executed):

```bash
callgrind_annotate --inclusive=no callgrind.out | head -80
```

**Top functions by inclusive cost** (which call paths own the most total work):

```bash
callgrind_annotate --inclusive=yes callgrind.out | head -80
```

**Interactive exploration:**

```bash
kcachegrind callgrind.out
```

### 4. Interpret

Key concepts:

- **Ir (instruction cost):** Callgrind's cost metric for attribution and comparison. It counts the number of instructions Callgrind simulates executing in each function. This is not a hardware counter; it is Callgrind's own accounting of the program's instruction stream. Because it is deterministic (not sampling-based), it is useful for stable comparisons, but it does not account for cache effects, branch mispredictions, or other microarchitectural costs.
- **Self cost:** Ir attributed directly to a function's own code, excluding callees. High self cost means the function itself is doing expensive work.
- **Inclusive cost:** Self cost plus all callee costs. A high inclusive cost often means the function owns a hot call path, not that its own body is expensive. Do not treat high inclusive cost as evidence that the function itself needs optimization -- trace into its callees to find the actual bottleneck.

**Reading workflow:** Start from the top self-cost functions. For each one, walk up the caller chain (using KCachegrind or `callgrind_annotate --inclusive=yes`) to find which architectural path owns that cost. This maps low-level hotspots (e.g., `RangeSet::Insert`) to high-level gap categories (e.g., G5 dirty tracking).

What to look for:

- **STL/container internals in self cost:** If STL internals (iterator constructors, vector growth, hash map operations) appear high in self cost, trace upward to the owning Lyra call path. This often indicates allocator churn or abstraction overhead in that path, not a problem with the STL itself.
- **Surprising entries:** Functions you didn't expect on the hot path. A simple getter or check with high inclusive cost usually means it is called at extreme frequency, not that its body is complex.

### 5. Compare before/after

After making a change, reprofile and compare:

```bash
# Before: save callgrind.out as callgrind.before
cp callgrind.out callgrind.before

# After: rebuild with -c opt, reprofile
bazel build -c opt //:lyra
./bazel-bin/lyra -C tools/bench/fixtures/simulation-engine/clock-pipeline compile
cd tools/bench/fixtures/simulation-engine/clock-pipeline
valgrind --tool=callgrind --callgrind-out-file=callgrind.out out/Top

# Compare total instruction counts
grep "Collected" callgrind.before callgrind.out
```

Always compare both total Ir and the top hot functions. A lower total does not necessarily mean the dominant bottleneck moved -- it may have shrunk proportionally while remaining the top cost center. Check whether the ranking of top functions changed, not just the totals.

For function-level comparison, open both files in KCachegrind side by side, or diff the `callgrind_annotate` text output.

## Avoiding noise

- **Startup:** For the clock-pipeline fixture at 10K cycles, startup is a negligible fraction of total cost. No need to filter it. This may not hold for shorter workloads -- verify if you change the fixture.
- **Determinism:** For deterministic workloads (no randomness, no I/O timing), repeated Callgrind runs should produce identical or near-identical instruction counts. No need for multiple trials in the common case.
- **Optimization level:** Always profile with `-c opt`. Bazel's default `fastbuild` mode uses `-O0`, which inflates instruction counts by ~5x with STL overhead that does not exist in optimized builds. The cost distribution at `-O0` is misleading -- functions that dominate at `-O0` (iterator constructors, vector::empty, begin/end comparisons) are fully inlined at `-O2` and disappear from the profile. Never use `-O0` profiles for performance prioritization.

## Callgrind output artifacts

Callgrind output files (`callgrind.out`, `callgrind.before`) are gitignored. They are local profiling artifacts, not checked in.

The `tools/bench/fixtures/simulation-engine/clock-pipeline/` directory may contain these files after profiling. Clean up with:

```bash
rm -f tools/bench/fixtures/simulation-engine/clock-pipeline/callgrind.*
```

## Cost model

Raw Ir totals and hotspot rankings are necessary but not sufficient. Every profile analysis should produce a structured cost breakdown that answers: where does the time go, and is the bottleneck frequency or per-unit cost?

### Fixed cost buckets

Classify every self-cost function into exactly one bucket. The bucket list is fixed across profiles so trends are comparable:

| Bucket                 | What it contains                                                                                                                                |
| ---------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| actual logic           | Emitted process bodies (`body_*_proc_*`), descriptor dispatch trampoline (`__lyra_descriptor_dispatch`)                                         |
| dirty tracking         | MarkSlotDirty, TouchSlot, MarkDirtyRange, ClearDelta                                                                                            |
| subscriptions          | FlushSignalUpdates, FlushDirtySlot, FlushSlotEdgeGroups, FlushSlotChangeSubs, FlushSlotRebindSubs, FlushSlotContainerSubs, EnqueueProcessWakeup |
| dispatch / reconcile   | ExecuteRegion, RunOneActivation, DescriptorProcessDispatch, ReconcilePostActivation, LyraSuspendWait, TraceWake, LyraResetIterationLimit        |
| connection propagation | FlushAndPropagateConnections (connection phase)                                                                                                 |
| comb propagation       | FlushAndPropagateConnections (comb phase), comb kernel functions                                                                                |
| NBA lifecycle          | ScheduleNba, SmallByteBuffer, ApplyFullOverwriteNba, ApplyMaskedMergeNba                                                                        |
| memory ops             | memcpy, memset, memcmp (libc)                                                                                                                   |
| allocator              | malloc, free, \_int_free, operator new, operator delete                                                                                         |
| other                  | Everything not in the above buckets                                                                                                             |

FlushAndPropagateConnections contains both connection and comb phases. Splitting its self cost between the two buckets requires manual attribution: either source-level annotation in KCachegrind, or instrumented builds with phase-specific counters. Runtime stats (`conn_considered`, `comb_considered`) provide call counts but not per-phase Ir. Treat the connection/comb bucket split as approximate when based on callgrind alone.

When a function straddles two buckets (e.g., memcpy called from both connection propagation and NBA), attribute it to the bucket of its dominant caller if possible. Otherwise, keep it in the generic memory ops bucket.

### Normalized metrics

Total Ir is useful for before/after comparison but hides whether a cost is from high frequency or high per-unit cost. Always compute normalized metrics alongside totals:

| Metric                  | Source                                  | Formula                       |
| ----------------------- | --------------------------------------- | ----------------------------- |
| Ir per cycle            | callgrind total, cycle count            | total_Ir / cycles             |
| activations per cycle   | runtime counter                         | total_activations / cycles    |
| Ir per activation       | callgrind total, activation count       | total_Ir / total_activations  |
| Ir per dirty-mark call  | dirty tracking bucket, dirty_mark_calls | bucket_Ir / dirty_mark_calls  |
| Ir per propagation call | connection bucket, propagation calls    | bucket_Ir / propagation_calls |
| Ir per NBA entry        | NBA bucket, nba_entries                 | bucket_Ir / nba_entries       |

These require runtime counters (see below). When counters are not yet available, estimate from propagation stats or design structure.

### Runtime counters

Callgrind tells you where instructions execute. Runtime counters tell you why. Counters answer semantic questions that profiles cannot:

- How many activations were productive (dirtied at least one slot)?
- How many subscription checks resulted in a wakeup vs no-op?
- What fraction of memcmp guards actually detected a change?
- How many fixpoint iterations converged in 1 round vs 2+?

Counters live in `RuntimeStats` on Engine, printed by `DumpRuntimeStats`. Two tiers: core counters (always collected, printed when `kDumpRuntimeStats` is set) and detailed per-element counters (collected and printed when `kDetailedStats` is set). The feature flags are the real runtime boundary; `-vv` and `-vvv` are the current CLI wiring (`-vv` sets `kDumpRuntimeStats`, `-vvv` adds `kDetailedStats`). Output lines: `[core]` (summary counters + static design shape) and `[detailed]` (inner-loop accounting, only when enabled).

### Benchmark diversity

Pipeline is the canonical profiling fixture for scheduler overhead. But optimizations must not be pipeline-specific. Validate against multiple workload shapes:

| Fixture                     | Workload character                    | What it catches                      |
| --------------------------- | ------------------------------------- | ------------------------------------ |
| pipeline                    | Clock-heavy, many small processes     | Scheduler/dispatch overhead          |
| (needed) comb-heavy         | Deep combinational chains, few clocks | Fixpoint iteration, comb kernel cost |
| (needed) subscription-heavy | Many signals observed per process     | Flush/subscription dispatch cost     |
| (needed) NBA-heavy          | Wide NBA writes, large value buffers  | SmallByteBuffer, NBA apply cost      |

New fixtures go in `tools/bench/fixtures/`. They must be deterministic, complete in <1s natively, and produce stable callgrind numbers.

### Analysis workflow

Combine top-down bucket analysis with bottom-up hotspot analysis and counters:

1. **Top-down:** Compute bucket percentages. Which bucket dominates?
2. **Bottom-up:** Within the dominant bucket, which function has highest self cost?
3. **Counters:** Is the hot function called too many times, or is each call too expensive? Compute per-unit Ir.
4. **Decide:** If per-unit cost is high, optimize the function body. If frequency is high, reduce how often it is called (algorithmic change). If data shape is wrong, restructure the data.

This three-layer analysis (buckets, hotspots, counters) prevents chasing symptoms. A function with high total Ir but low per-call cost needs a frequency reduction, not a micro-optimization.

## Linking profiles to performance gaps

Every gap in `docs/queues/performance.md` should cite either:

- A benchmark delta (before/after wall-clock numbers from `run_benchmarks.py`)
- A profiling result (inclusive/self cost percentage from callgrind)

This ensures the gap inventory is evidence-backed, not intuition-based.
