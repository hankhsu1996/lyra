# Dev Ergonomics

Tracks gaps in the developer feedback loop: taking a single SystemVerilog file, observing how it
behaves, and pinpointing where Lyra and a reference simulator diverge -- without hand-writing a test
case. The work is done when a developer can do that for one source file and locate the divergent
layer directly.

## Sub-Steps

- [x] D1 -- Run a SystemVerilog file end-to-end from one command, with a sibling command that builds
      without running.

- [ ] Variable assertions on a multi-module source. A case whose source declares more than one
      module can only assert on stdout, so a feature that spans modules is verified through a
      formatted print rather than through the variables it actually produces. The probe the
      assertion injects has to name the top among several declarations and read what it needs from
      there.

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

- [x] D7 -- A design describes itself once, in a `lyra.toml` beside it, so a multi-file design is
      not respelled at every invocation: its sources, include directories, defines, undefines,
      parameter overrides, library search, tops, language version, timescale, compilation-unit
      model, assertion policy, and the native sources that give DPI-C its foreign symbols. The file
      carries what is true of the design for everyone who builds it and never what is true of one
      invocation or one machine, so a command line still names a design outright and a file that is
      absent is simply no defaults. A command line adds to what the file lists where the field is
      material and replaces it where the field is a choice; naming sources on the command line uses
      no file at all. The file is found by walking up from the working directory, and a path inside
      it means the same thing from wherever the compiler was invoked. `decisions/project-file.md`
      settles the shape.

- [ ] D8 -- A design that finds its modules through a library rather than by listing them can
      declare that: library files, library maps, the library search order, and the default library
      name. Design material by the rule D7 already applies, and absent only because the language's
      library and configuration system (LRM 33) is not implemented, so a field for it would declare
      something the compiler cannot act on. Until then such a design passes those settings on the
      command line.

- [ ] D9 -- A design written for a dialect another tool defined can declare that: legacy protect
      envelopes, translate-off comment formats, ignored directives, keyword-version mapping, and
      include-lookup order. Each changes what program the source text denotes, so each is design
      material rather than an invocation setting; none has been needed yet.

## Out of Scope

- New SystemVerilog feature coverage. This file tracks the developer feedback loop, not language
  features.
- Comparison tooling that drives both Lyra and a reference simulator.
- Performance instrumentation.
- Readability of the emitted C++ artifact (see `emit-readability.md`). This file owns the feedback
  loop; that one owns how legible its output is.
