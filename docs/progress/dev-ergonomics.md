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

- [x] D5 -- A failing end-to-end case surfaces its underlying cause -- the emitted-C++ compile error
      or the runtime message -- directly from the test command when it is run unpiped. The detail
      was always captured; the project test convention now keeps it visible by not routing the run
      through a downstream filter that scrolls it away.

- [x] D6 -- A test run reports its true pass/fail outcome when run unpiped. The convention is to run
      the test command without a downstream filter: piping through `tail` would make the exit status
      the filter's, not the test's, so a failing suite reads as green -- a false pass that is most
      dangerous for a backgrounded run. Related to D5 but more fundamental: D5 is about seeing _why_
      a case failed; this is about not missing _that_ it failed.

## Out of Scope

- New SystemVerilog feature coverage. This file tracks the developer feedback loop, not language
  features.
- Comparison tooling that drives both Lyra and a reference simulator.
- Performance instrumentation.
- Readability of the emitted C++ artifact (see `emit-readability.md`). This file owns the feedback
  loop; that one owns how legible its output is.
