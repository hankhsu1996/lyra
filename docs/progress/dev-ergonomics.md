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
      parameter overrides, search for a cell by name, tops, language version, timescale,
      compilation-unit model, assertion policy, and the native sources that give DPI-C its foreign
      symbols. The file names a library, which every cell compiled there belongs to, and states
      apart from it what is run of it -- the tops and whatever only a testbench needs -- so that
      what something depending on the library would be given is already separate. The file carries
      what is true of the design for everyone who builds it and never what is true of one invocation
      or one machine, so a command line still names a design outright and a file that is absent is
      simply no defaults. A command line adds to what the file lists where the field is material and
      replaces it where the field is a choice; naming sources on the command line uses no file at
      all. The file is found by walking up from the working directory, and a path inside it means
      the same thing from wherever the compiler was invoked. `decisions/project-file.md` settles the
      shape.

- [ ] D8 -- A design that finds its modules through a library rather than by listing them can
      declare that: library files, library maps, and the library search order (the default library's
      name is the declared library's). Design material by the rule D7 already applies. The compiler
      acts on all three given on the command line -- a design binding cells from several libraries,
      under a configuration or a search order, builds and runs -- so what is absent is only the
      fields.

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

- [x] D11 -- A design's units can be compiled several at a time, on either backend, and how many is
      stated by whoever asked for the build rather than chosen by the build. Both things that build
      a design take it: the command line for the build the compiler drives, and its own argument for
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

- [ ] D12 -- A build recompiles only what a change reached. **Done on the LLVM backend**: a unit's
      module is complete in itself, so its object is kept under a name computed from the module's
      text, the code generator's build and the level, and an edit recompiles only the units whose
      module changed. Measured on Ibex at `-j 4`: 6.9 s from nothing, 2.9 s when every object is
      kept. **What is left is the C++ backend**, where every build compiles every unit, however many
      at a time, because nothing records which artifact a change invalidated. A unit's declarations
      and its bodies are already separate files and each unit already compiles to its own object, so
      what is missing is the record rather than the shape: what a referrer compiled against, and
      whether it still holds.

      The second question behind it is now answered, which is what makes the record worth building.
      What a referrer compiles against is the part the declaring unit published and nothing else, so
      an edit confined to a unit's bodies moves no text any referrer reads, on either backend -- an
      invalidation record laid over that is precise rather than nominally correct. The signature workstream owned
      that half; this one owns the record.

      A prerequisite nobody had seen is now settled: emitting one unchanged design twice produces
      the same bytes. It did not, for about a quarter of a large design's units, and the difference
      was an ordering no simulated program can observe -- so every case passed and no coverage
      record could have listed it. Nothing derived from emitted content could have been built while
      that held, which puts it in front of the record rather than beside it.

      Measured 2026-09-16 on the C++ backend, a three-unit design: a build that changes nothing still takes the same
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

- [x] D14 -- A translation unit of an emitted design pays for what it contains rather than for what
      the runtime offers. It used to pay a fixed cost first, and the cost was large enough to hide
      the design: a unit holding the runtime surface and no design code at all took 0.63 s, against
      1.60 s for the same design's own unit. Nearly all of it was instantiating the same templates
      again, not reading the header a build prepares in advance -- two thirds of that in one
      standard formatting facility, half of which serves wide characters that no simulated program
      can ask for. The work now happens once, while that header is prepared.

      Measured 2026-09-22: the empty unit 0.63 s to 0.11 s, the design's own 1.60 s to 0.42 s, and
      ten conformance cases end to end -- emit, build and run -- a mean of 3.12 s to 1.54 s each.
      Preparing the header costs about 0.9 s more and is repaid by the second unit built against it.

      This is a per-unit cost, so what it is worth to a design grows with how many units the design
      has, and it is independent of D11 and D12: compiling several units at a time divides the
      waiting rather than the work, and reusing an object skips a unit rather than making one
      cheaper. `decisions/a-prepared-header-carries-the-work.md` settles the shape.

- [ ] D15 -- A project built by a compiler other than the one that produced it gets the same
      treatment. The recipe prepares a header for clang and for nothing else, so anyone building an
      emitted project with GCC compiles the runtime surface from source in every unit: measured
      2026-09-22 on one unit, 2.70 s against 1.11 s once GCC is given a prepared header of its own,
      which is the same shape the other compiler shows.

      What makes this open rather than done is that the trade inverts at this size. Preparing one
      costs 5.43 s and takes 211 MB, so a project of three units is better off without it and the
      break-even is around four; a real design is far past that and a conformance case never will
      be. So the gain is a user's rather than this repository's, which is why it waits -- and the
      figure to re-take before acting is the break-even, not the per-unit one.

- [x] D16 -- A translation unit of an emitted design hands the linker its own classes and a set of
      calls, rather than a copy of what the library it links already holds. It used to hand over
      both: a unit of 3,907 bytes produced a 1,688,384 byte object holding 1,503 bytes of code, the
      rest being the dispatch tables and members of the runtime classes a design's scopes derive
      from -- and, smaller, the type information of the types a run can raise -- written out by
      every unit and then discarded by the linker down to one copy.

      Measured 2026-09-23 on one emitted project: its objects total 783,048 bytes against 3,904,344,
      and a probe unit adding a single scope class to the shipped surface goes from 1,600,704 bytes
      to 24,016. The same probe built optimized goes from 135,664 to 4,912, so this is a separate
      axis from how hard the host compiler is asked to work rather than a restatement of it.

      **This buys disk and not time** -- the same project builds in 1.87 s against 1.77 s, which is
      noise at that size -- and disk is what it has to buy, because an object set is held whole
      while a build runs and a design of a thousand units holds a thousand of these at once.
      `decisions/a-published-class-is-emitted-once.md` settles the shape and states what is left.

- [x] D17 -- The same for the library's functions, which is where most of it turned out to be. A
      function the shipped headers defined was compiled again by every unit that called it, along
      with everything its body reached -- and for anything touching a runtime value that is the
      machinery of a variant over every value domain. Stating one wait in a unit cost 400,064 bytes
      of object, and a unit stating one scope, one variable, one write and one wait weighed 636,328
      against an empty one's 1,112.

      Measured 2026-09-23 on a 32-unit design: its objects total 6,720,864 bytes against
      26,613,176, and its leaf unit 155,232 against 756,496. **This one buys build time as well**,
      where the entry above bought only disk, because a unit no longer performs the instantiations
      it was also writing out: the functions and families alone took a warm build from 17.46 s to
      13.66 s, and constructing and destroying what the runtime defines took the same project from
      16.1 s to 12.5 s, measured side by side.

      What a unit now holds of the runtime is what its design decided, and a test reading the
      emitted objects fails on anything else. `decisions/a-published-operation-is-compiled-once.md`
      settles the shape.

- [x] D18 -- A tool the build runs can print any amount without taking the build down with it. A
      link that failed with tens of gigabytes of errors used to end the build on its own memory
      before it reported anything; a failing compile or link is now reported with the start of what
      the tool said, whatever the amount. Measured on a stand-in compiler printing 6 GB from each of
      three compiles: the build peaks at 92 MB and ends as a reported failure.

- [x] D19 -- A read of an object that is gone fails a gate instead of reaching a user. A warning in
      Lyra's own sources fails the build under either compiler, and the address sanitizer run builds
      and reports a reference held across a pool's growth on every execution that reads one, not
      only where the growth happened to move the storage. Before this the one compiler warning in
      the tree named a defect that made a design wait on the wrong bits of a packed structure's
      first member, and the sanitizer run had failed at build on every run since it was added.

- [x] D20 -- Where a compile's time, memory and output went is read off one run. Asked, a run writes
      a trace with a span per stage, unit, scope and function, and a file with each stage's peak
      memory, every file each unit left behind, and how long each tool it ran took. On a loop of a
      thousand empty generate blocks beside a 16,384-bit parameter, the first trace put 3.24 s of
      3.26 s in declaring the unit's structural identities, where the reported symptom had pointed
      at lowering its bodies.

- [x] D21 -- A cost that grows with how often a design repeats something is refused at the merge
      gate. Seven designs each state one thing N times -- a loop of one body, a loop selecting by
      its index, a loop of instances, a loop of instances each reading a variable of their parent,
      an instance array, a loop choosing by an outer index, an unread parameter's width -- and are
      compiled at N and at twice N on both backends: the larger run may leave behind no more units
      or files and no more than a quarter more bytes, and each stage may take under three times the
      time and the peak memory. What `main` exceeds today is recorded per path and has to go on
      being exceeded, so the record only shrinks. Two are left in it: the C++ backend writes files
      and bytes per generate block, and an instance array's emitted bytes follow its element count
      on both backends. A third went: a unit an override written elsewhere could reach worked its
      own name out again for every block it declared and for every instance naming it, so declaring
      a loop cost the square of its count (0.3 s at 512 blocks, 1.2 s at 1024), and so did a loop of
      instances reading upward (1.7 s and 6.3 s). A name is now worked out once.

- [x] D22 -- The address sanitizer run reaches the cases it was added to watch. It had built since
      D19 and still reported nothing: the runtime library a built program links was instrumented
      along with the compiler, so every case stopped where its program was linked, and each printed
      more than a run's log keeps. The library is now built as it ships whatever builds the
      compiler, and the run leaves out the one check that bounds what a compile costs, since under
      the sanitizer an append costs the pool it lands in. On `main` the run is clean: no report over
      the whole corpus. What a simulation's own memory does while it runs is not something this run
      sees.

- [x] D23 -- A library uses another by naming it. A `lyra.toml` lists the libraries its own depends
      on, each by name and the directory declaring it, and a build reads every library it reaches
      once. A library that is depended on is read as its own declaration says, whoever uses it: its
      cells are in a library of its name, its text is read under its own defines and include
      directories and no other library's, a cell it instantiates is its own before any other of that
      name, and it finds only cells of the libraries it declared. A dependent receives its cells and
      the include directories it exports; what is run where the library is developed stays there.
      The command line reaches every library. Where the command line and a declaration both define a
      macro or override a parameter, the command line's is the one in effect.

- [ ] D24 -- What a library that is depended on may not yet declare. Refused with the reason: a
      language version or a default time scale other than the root's. Two libraries declaring a
      package of one name cannot be used in one build, which the standard's single package name
      space decides. Not built: fetching a library that is not on the machine, versions, a dependent
      choosing among variants of a library, and dependencies only a testbench needs. A library's
      unit has one name where the library is built and another where it is depended on.

- [x] D25 -- A source can tell it is being read by Lyra. `__lyra__` is defined as 1 for every text
      of a build: the library being built, one it depends on, and a file named on the command line.
      The front end's own `__slang__` stays beside it. An invocation can undefine either and cannot
      give either another value. No macro states a version, since Lyra shows none.

## Out of Scope

- New SystemVerilog feature coverage. This file tracks the developer feedback loop, not language
  features.
- Comparison tooling that drives both Lyra and a reference simulator.
- Instrumenting the simulated program's own run time.
- Readability of the emitted C++ artifact (see `emit-readability.md`). This file owns the feedback
  loop; that one owns how legible its output is.
