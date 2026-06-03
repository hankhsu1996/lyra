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

## Out of Scope

- New SystemVerilog feature coverage. This file tracks the developer feedback loop, not language
  features.
- Comparison tooling that drives both Lyra and a reference simulator.
- Performance instrumentation.
- Readability of the emitted C++ artifact (see `emit-readability.md`). This file owns the feedback
  loop; that one owns how legible its output is.
