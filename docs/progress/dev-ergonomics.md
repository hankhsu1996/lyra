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

- [x] D10 -- One run reports every construct the compiler could not lower, rather than the first. A
      refusal is collected and the walk goes on across the units of a compilation, the members of a
      unit, and the scopes nested in one; a stage that reported anything is the last one that runs,
      and what it produced is discarded. This is the loop's own length: a compiler answering one gap
      per run makes the number of runs the number of gaps, and no stage can be made fast enough to
      make up for that. What is still unseen is a gap standing behind another **inside one body**,
      which is abandoned at its first refusal. `decisions/reporting-every-gap-in-one-run.md` settles
      the shape.

- [x] D11 -- A design's translation units can be compiled several at a time, and how many is stated
      by whoever asked for the build rather than chosen by the build. Both things that build a
      design take it: the command line for the build the compiler drives, and its own argument for
      the recipe an emitted project ships. Asked for nothing, a build compiles one unit at a time,
      because a build told nothing cannot know what else holds the machine -- a conformance run
      drives sixteen of them at once, and a width each picked for itself would multiply rather than
      add. Zero asks for one compile per processor, which is how a caller says the machine is its
      own.

      Measured on the Ibex Simple System testbench, 49 translation units, unoptimized, emit
      included: one at a time takes 3:08 of wall for 2:59 of CPU; four at a time takes 0:55 for
      3:00. Same work either way, and 3.4x less waiting for it. The precompiled header is doing its
      job across that split rather than being lost by it -- disabling it costs 43 s of CPU.
      `decisions/a-build-is-told-how-wide-to-run.md` settles the shape.

      **A figure recorded here before does not reproduce and is being replaced rather than
      updated.** The sequential build was written down as 1:42, which cannot be right for a
      workload of 2:59 of CPU on one core at a time. Nothing in this change would slow a sequential
      build, so the discrepancy is in the earlier figure or in what the design emitted when it was
      taken; a type's readings are emitted per declared type since then, which R96 in `refactor.md`
      counts at 337 uncalled bodies for this very design. That is a candidate rather than a
      finding -- attributing it needs a build of the older compiler, which nothing here has done.

- [ ] D12 -- A build recompiles only what a change reached. Every build compiles every unit, however
      many at a time, because nothing records which artifact a change invalidated. A unit's
      declarations and its bodies are already separate files and each unit already compiles to its
      own object, so what is missing is the record rather than the shape: what a referrer compiled
      against, and whether it still holds.

      The second question behind it is now answered, which is what makes the record worth building.
      What a referrer compiles against is the part the declaring unit published and nothing else, so
      a change to what a unit kept to itself moves no text any referrer reads -- an invalidation
      record laid over that is precise rather than nominally correct. The signature workstream owned
      that half; this one owns the record.

      Measured 2026-09-16 on a three-unit design: a build that changes nothing still takes the same
      two and a quarter seconds as the one before it, and every object is written again. The Ibex
      testbench figures above put the same statement at three minutes of processor time per build
      whatever was edited, because nothing is reused between one build and the next except the
      precompiled header. That is the larger of the two costs on this page by a wide margin.

      **What makes the record cheap here is that the compiler wrote the inputs.** A build cache
      elsewhere has to discover what an artifact depended on -- preprocessing the source, or
      believing a declaration -- because the thing being compiled arrived from outside. Here the
      text handed to the host compiler is text this compiler just produced, and the settings it is
      compiled under are settings this compiler just chose, so what determines an object is already
      in hand and needs no discovering. Naming an artifact by what determines it then makes reuse a
      lookup rather than a decision, with no timestamp anywhere in it.

      **And it makes an edit that changes nothing stop at the boundary.** Two designs that differ in
      a way the emitted text does not record produce the same text for a unit, so that unit's
      object is reused rather than rebuilt, and so is everything that would have followed from
      rebuilding it. That is worth stating because it is the property a record keyed on what was
      edited cannot have, and it costs nothing extra to get.

- [ ] D13 -- What a build keeps between runs is bounded without anyone having to remember it. The
      precompiled header is cached under one directory shared by every checkout on the machine,
      keyed so that each distinct compiler, header tree and optimization gets an entry of its own,
      and nothing ever removes one. Measured 2026-09-16: forty megabytes an entry, six entries and
      two hundred and thirty-eight megabytes after a single day's work, and one more the moment any
      header's content changes. A command exists that empties it, which means the bound today is
      that somebody notices and asks.

      The shape this wants is the one a build cache usually has: an entry that has not been wanted
      for long enough goes, on a schedule cheap enough that no build waits for it. Settling it needs
      a reading of how often an entry is actually wanted again, which nothing here has taken, and it
      belongs with D12 rather than before it -- both are the same question about what a build keeps
      and for how long, and answering one without the other fixes half a policy.

## Out of Scope

- New SystemVerilog feature coverage. This file tracks the developer feedback loop, not language
  features.
- Comparison tooling that drives both Lyra and a reference simulator.
- Performance instrumentation.
- Readability of the emitted C++ artifact (see `emit-readability.md`). This file owns the feedback
  loop; that one owns how legible its output is.
