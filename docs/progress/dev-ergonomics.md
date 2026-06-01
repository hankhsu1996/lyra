# Dev Ergonomics

Tracks gaps in the developer feedback loop: taking a single SystemVerilog file, observing how it
behaves, and pinpointing where Lyra and a reference simulator diverge -- without hand-writing a test
case. The work is done when a developer can do that for one source file and locate the divergent
layer directly.

## Sub-Steps

- [x] D1 -- A single command that runs a SystemVerilog file end-to-end. Running a source file is now
      a first-class command that takes the same inputs as the dump and emit commands, builds it, and
      streams the simulation's output. A sibling command produces a built executable without running
      it. The backend that performs the build is an implementation detail of these commands.

- [x] D4 -- A self-contained build for the C++ backend's output. Emitting the C++ now produces a
      complete project on disk: the translation units, a build recipe, and a bundled copy of the
      Lyra runtime the emitted code depends on. The directory stands on its own -- one documented
      command rebuilds it, and it can be handed to another machine of the same platform without a
      Lyra checkout. Building it and running it are the compile and run commands layered on top.

## Out of Scope

- New SystemVerilog feature coverage. This file tracks the developer feedback loop, not language
  features.
- Comparison tooling that drives both Lyra and a reference simulator.
- Performance instrumentation.
