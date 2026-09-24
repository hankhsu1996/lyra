# A Build Is Told How Wide To Run

## Date

2026-09-16

## Status

Accepted. Spends the per-unit artifact boundary
[only-a-base-links-two-signatures](only-a-base-links-two-signatures.md) established, and draws the
same line [project-file](project-file.md) D2 draws between what is true of a design and what is true
of one machine.

## Why this decision matters

A design's units are emitted as separate translation units, and the program is formed by compiling
each and linking the results. Turning that into time rather than only memory means compiling several
at once -- and the moment anything does, the question is how many, which is a claim about a machine
rather than about the design.

`north_star.md` forbids "architectural choices that force global serialization of otherwise
independent compilation work". A build that hands every unit to one compiler invocation forces
exactly that: there is no way to say anything else. So the deliverable is not speed, it is that the
width becomes sayable -- and then the only remaining question is who says it.

## What the survey says about who owns a width

- **GNU make** states the problem in its own manual and then answers it. "When the build environment
  is such that a top level make invokes sub-makes ... **no individual instance of make knows how
  many tasks are running in parallel, so keeping the number of tasks under the upper limit would be
  impossible without communication between all the make instances running.**" Its answer is a shared
  pipe of tokens passed to sub-makes. The load-bearing half is what it does for everything else: "if
  the job to be run is not a sub-make then make will **close the jobserver pipe file descriptors**
  before invoking the commands". A child that does not participate is given nothing, and one job is
  what it gets.
- **Cargo** defaults `build.jobs` to "the number of logical CPUs", overridable by `--jobs` and by
  `CARGO_BUILD_JOBS`. **Ninja** does the same: "Builds are always run in parallel, based by default
  on the number of CPUs your system has", with `pool` to hold particular rules below that.
- **Verilator** is the closest system in kind, because it too both writes a build recipe and drives
  a build itself, and it keeps the two widths apart: `--build-jobs` is "the level of parallelism for
  `--build`", `--verilate-jobs` is its own internal work, and `-j` is the fallback for both. Zero
  means "the number of threads available to the process".

**Where our conditions differ, and it decides the default.** Cargo and ninja default to the machine
because they are always the outermost thing a person types. Make defaults to one because recursive
make -- a build nested inside another -- is the normal shape of what it builds. Lyra is make's case
rather than cargo's: a person types `lyra run`, and the conformance corpus runs sixteen shards that
each drive one of these builds, on the machine running the editor. The nesting is not hypothetical
and it is not rare.

**What we cannot take from make is its mechanism.** A token pipe needs inherited file descriptors at
both ends. Bazel hands a test no such pipe, and the compile is a subprocess of a binary Bazel
produced rather than an action Bazel schedules, so nothing upstream could offer one; and the recipe
an emitted project ships is POSIX `sh`, which has no way to work one. What survives is make's rule,
which needs no channel at all: absent a statement, one.

## Decisions

### D1. The width is stated by the invoker and never chosen by the build

How many of a design's units are compiled at once is a property of this invocation on this machine,
whichever compiler does the compiling -- the host C++ compiler on one path, Lyra's own code
generator on the other. A compiler sees one design and cannot see what else holds the machine, so a
width it picked for itself would be a claim it has no basis for.

Both things that build a design take it as an argument: the command line for the build Lyra drives,
and the recipe's own argument for a build somebody runs by hand. Zero is how a caller says "this
machine is mine", which resolves to one compile per processor.

### D2. Told nothing, a build runs one compile at a time

The default asserts nothing. It is also the only default that moves no existing behaviour: sixteen
conformance shards each driving one compile is what runs today, and any other default would re-tune
the test suite's demand on the machine inside a change about the build.

The cost is named rather than hidden: a person who wants the machine says so every time, because
Lyra has no per-machine settings file to say it once in. That absence is a gap of its own and not
this entry's to close.

### D3. The width does not reach the emitted recipe, though the toolchain does

An emitted project bakes in the compiler that produced it and the optimization it was emitted at,
because each decides what the build produces or whether it works at all. The width decides neither:
the program is identical however many compiles ran at once. A committed artifact recording how much
of one machine to take would be answering for every machine that ever builds it, which is the line
[project-file](project-file.md) D2 draws for the design declaration and which holds here for the
same reason.

So the recipe takes `-j` of its own, exactly as it already takes `--no-pch` -- the other value that
is about the running machine rather than about the program.

### D4. Each unit compiles to its own object, which is what concurrency requires rather than a step toward reuse

Several compiles running at once and one link reading them all needs somewhere for a finished
compile to leave its result. That is an object file, and it follows from D1 rather than being chosen
beside it.

Objects on disk are also what a build that recompiles less than everything would need. This entry
does not take that. The LLVM path has since, by keeping each unit's object under a name computed
from what it was compiled from (`a-program-is-kept-by-what-built-it.md` D9); on the C++ path no step
asks whether an object is still current, because a translation unit reads other units' headers and
answering it needs a record of what a change invalidated, which no artifact carries.

### D5. Every compile is attempted and every failure reported

A build runs all of its compiles even where one has already failed, and names each that did. Emitted
text that does not compile is a defect in Lyra rather than in the design, and one run that names
every unit is what saves the run each remaining one would otherwise cost -- the same reading
`reporting-every-gap-in-one-run.md` gives a lowering that meets a construct it cannot carry.

## Rejected alternatives

- **Defaulting to one compile per processor.** It is what cargo and ninja do and it gives a person
  the win without asking. Rejected on two counts. The conformance corpus would multiply rather than
  add -- sixteen shards times the machine's width, on the machine running the editor, which has
  already been wedged once that way. And the measured return does not pay for the risk: on the
  standing integration target, four at a time is most of the available gain, while a width of twenty
  would hold twenty compiles' peak memory at once against a machine that does not have it.

- **A field in the design declaration.** Refused by name in `project-file.md` D2, and the
  re-derivation agrees rather than defers: a manifest is committed and shared, so a value true of
  one developer's machine poisons it for everyone else who builds the design.

- **A recipe that emits a dependency graph for `make` or `ninja` to run.** It is Verilator's shape,
  and it hands the width to a scheduler that already knows how to compose. Rejected because an
  emitted project must build on another machine with nothing installed but a shell and a C++
  compiler, and requiring a build tool is a dependency that property cannot carry. The recipe keeps
  the graph it actually has -- N independent compiles and one link -- which needs no tool to
  express.

- **Replacing each finished compile as it exits, rather than waiting for a batch.** It is the better
  schedule, and it is what the in-process build does. The recipe waits for a batch instead, because
  `wait -n` is not POSIX and the recipe may not assume a shell richer than `/bin/sh`. The reason
  offered for accepting it -- that units are of roughly even size, so the difference is the tail of
  one batch -- does not hold: on Ibex at `-O2` the recipe takes 679 s where a pool reaches 226 s.
  The batches stay because nobody builds through the recipe at scale except the project's own
  measurement, while `build` and `run` go through the in-process pool.

- **Compiling a foreign source through a path of its own.** A DPI-C source compiled for C linkage is
  a compile like any other; giving it its own sequential step would leave two schedules to reason
  about and buy nothing. It compiles as wide as the units do, and a design with no foreign sources
  contributes none. What it cannot do is start with them: it includes the header composed from what
  every unit stated of the foreign name space, so it waits for the last unit to be collected.

## Consequences

- The recipe and the build Lyra drives agree on the compiler, the optimization and the precompiled
  header, and deliberately disagree on nothing else -- the width is asked for separately at each,
  because each has its own invoker.
- A build writes objects into the project, so an emitted directory now holds derived files beside
  its sources.
- A second build into a directory that already holds one succeeds. It did not before: the runtime
  headers were rewritten byte-identically on every run, and clang validates a precompiled header by
  modification time rather than by the content its cache key is built from, so it rejected the
  header it had just been handed. Writing a file now leaves an unchanged one alone, which is the
  general rule a generator owes anything that watches timestamps.
- This entry recompiles nothing less than everything; the LLVM path's kept objects are a later
  decision's (D4).
- The width reaches every stage that takes one unit at a time, not only the host compile: lowering a
  unit and compiling its module run as wide as the build was told, and what they produce is
  collected in the order the design lists its units, so the program is the same however many ran.

## Cross-references

- [only-a-base-links-two-signatures](only-a-base-links-two-signatures.md) -- the per-unit artifacts
  this spends, and why a unit's declarations and its bodies are separate files.
- [project-file](project-file.md) -- D2's line between what is true of a design and what is true of
  one invocation or one machine, which D3 applies to the recipe.
- `reporting-every-gap-in-one-run.md` -- the same reading of a failure that D5 gives a compile.
- `../architecture/emission_model.md` -- the artifact rules, and what the boundary now buys.
