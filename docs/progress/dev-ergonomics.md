# Dev Ergonomics

Tracks gaps in the developer feedback loop: taking a single SystemVerilog file, observing how it
behaves, and pinpointing where Lyra and a reference simulator diverge -- without hand-writing a test
case. The work is done when a developer can do that for one source file and locate the divergent
layer directly.

## Sub-Steps

- [x] D1 -- Run a SystemVerilog file end-to-end from one command, with a sibling command that builds
      without running.

- [x] D4 -- Emitting the C++ backend produces a self-contained project that rebuilds and runs on
      another machine of the same platform without a Lyra checkout.

- [ ] D5 -- A failing end-to-end case surfaces its underlying cause -- the emitted-C++ compile error
      or the runtime message -- directly from the test command. Today that detail is truncated in
      the test summary and the full text lives only in per-shard log files outside the readable
      path, so finding why a case failed takes several manual reruns of the same suite.

- [ ] D6 -- A test run reports its true pass/fail outcome even when its output is filtered. Piping a
      `bazel test` through `tail` (or any filter) makes the pipeline's exit status that of the
      filter, not of bazel, so a failing suite looks like it passed -- and a backgrounded run's
      completion status is then a false green that can hide a regression until an unfiltered run
      catches it. Target: a test command, wrapper, or convention whose exit status reflects the
      actual test outcome regardless of output handling (e.g. `pipefail`, or a runner that records
      the bazel result separately from the streamed text). Related to D5 but more fundamental: D5 is
      about seeing _why_ a case failed; this is about not missing _that_ it failed.

## Out of Scope

- New SystemVerilog feature coverage. This file tracks the developer feedback loop, not language
  features.
- Comparison tooling that drives both Lyra and a reference simulator.
- Performance instrumentation.
- Readability of the emitted C++ artifact (see `emit-readability.md`). This file owns the feedback
  loop; that one owns how legible its output is.
