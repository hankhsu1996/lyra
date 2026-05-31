# Dev Ergonomics

Tracks gaps in the developer feedback loop: taking a single SystemVerilog file, observing how it
behaves, and pinpointing where Lyra and a reference simulator diverge -- without hand-writing a test
case. The work is done when a developer can do that for one source file and locate the divergent
layer directly.

## Sub-Steps

- [ ] D1 -- A single command that runs a SystemVerilog file end-to-end. Today the CLI can dump the
      intermediate forms and emit the C++ backend's output, but no command takes a source file all
      the way to an executed simulation, so developers fall back on the test harness. Execution
      should be a first-class command accepting the same inputs as the dump and emit commands.

- [ ] D4 -- A self-contained build for the C++ backend's output. The emitted C++ depends only on the
      C++ standard library and the Lyra runtime, so a standalone build needs no third-party include
      paths. What remains is that the backend emits no build recipe, so a developer still assembles
      the compile and link by hand. The backend should emit the build alongside the C++ so one
      documented command produces a runnable binary and the output is portable on its own.

## Out of Scope

- New SystemVerilog feature coverage. This file tracks the developer feedback loop, not language
  features.
- Comparison tooling that drives both Lyra and a reference simulator.
- Performance instrumentation.
