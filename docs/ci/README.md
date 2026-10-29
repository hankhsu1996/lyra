# CI Design Notes

## Strategy

Build and test by asking for the whole graph -- `bazel build //...`, `bazel test //...` -- rather
than naming targets, so a new target is covered the moment it exists and no list has to be kept in
step with the `BUILD` files.

One split cuts across that, and what it draws is a subject rather than a price: whether an emitted
C++ project still builds and runs. `cpp_tests` asks it over the whole corpus, once per case, and
`emitted_project_tests` asks what a project does once built. No per-PR budget holds either, so both
carry `nightly`, `.bazelrc` makes the default set their complement, and `host-cxx-nightly.yml` takes
`--config=nightly`. `--config=full` is both sides. So a developer running the tests locally runs
what the gate runs without naming anything, and the expensive ones are asked for rather than
avoided.

Needing a host compiler is a separate question from being too expensive to gate on, and from where a
test can run. Every program either backend builds is linked by the host C++ compiler, so every
target that runs a design needs one, and a compiler spawned from inside a test is not a Bazel
action: it is whatever the executing machine has on its PATH. The remote image has the platform's
own C++ compiler and no clang, and Lyra takes whichever of the two it finds, so a test runs remotely
unless what it asserts is about clang itself, and such a test carries `no-remote-exec`.
`runtime_surface_test` is one, since it measures the objects clang produces from an emitted unit and
skips where there is no clang. The precompiled header is not such a subject, because either compiler
prepares and reads one; `prepared_header_test` asks the same questions of clang and of GCC side by
side, so it carries the tag for the machine that has both, and `emitted_project_tests` runs under
GCC remotely and under clang on the nightly's runner. A target kept out of the gate should be kept
out for its own reason, never for a neighbour's.

**What the CLI suite cost was once "a fraction of a minute", and by 2026-09-22 it was 117 s of
processor time and the longest target in the gate -- about four times the corpus run beside it.**
Six of its sixteen cases build an emitted project, several more than once, and they were the whole
of it. None of that is repetition that could be shared: what a prepared header holds is bound to the
paths of the headers it was made from, and every emitted project carries its own copy, so no two
projects can share one. The cost is what those cases assert.

So they are not in the gate, and the reason is their subject rather than their price. What they ask
-- that a directory rebuilds standalone through its shipped recipe, that a second toolchain accepts
the headers and still links the runtime the first one compiled, that a build whose prepared header
is refused still produces a program -- is the same question `cpp_tests` asks, so they are answered
on the same schedule, as `emitted_project_tests`. A change that could break any of it already owes
that schedule before it commits. What remains in `cli_tests` is the command line itself, each case a
few builds of a small design, mostly on the backend that compiles no C++; the target is cut into
more shards than it has cases, so it costs its slowest case, and remotely.

The two sides of `nightly` are complements, so every test target is covered once and none twice.
That is the property to preserve: a new target joins whichever side its tag puts it on, and neither
list is maintained by hand.

One target on the gate's side builds an emitted C++ project and runs it: `examples_tests`, which
runs every directory under `examples/` the way its README tells a reader to, from that directory,
with a command that names nothing. That command builds on the default backend, so a change that
leaves the renderer's output uncompilable goes red at merge time and not the next morning, and an
example whose `lyra.toml` the compiler refuses, or whose documented command no longer works, goes
red with it. It is one build per shipped example, seconds each, which is why it is not on the other
side. An example there has to end in a failure when what it checks is wrong: the test reads how the
run ended and nothing it printed.

## How the corpus divides into targets

The conformance corpus is one set of cases, and a target is the path that runs it. A case carrying
foreign sources needs nothing a path does not already need -- the compiler that links the program
compiles them too -- so the corpus is not divided any further.

| Target       | Runs             | Where  | At merge time |
| ------------ | ---------------- | ------ | ------------- |
| `llvm_tests` | the whole corpus | remote | yes           |
| `cpp_tests`  | the whole corpus | remote | no            |

`llvm_tests` is therefore the gate, and it grows on its own: `tests/paths/llvm.yaml` records what
that path still refuses, only ever shrinks, and is the measure of how much of the corpus the merge
gate actually covers.

Both run remotely, and remote execution scales out, so what a target covers costs the machine that
asked for it nothing however far it grows. What it costs is how long the slowest shard takes, so a
remote target is cut into as many shards as keep that short: another shard is one more slot in the
remote queue, never another core here. A remote shard is given three cores, and a case compiles its
units one after another, so a shard runs three cases at once; `tests/BUILD.bazel` states the count
once, and a run on this machine reserves as many cores as it uses.

## When to run what

Each moment answers a different question, and the answers are not interchangeable.

| Moment     | Question                        | Command                                      |
| ---------- | ------------------------------- | -------------------------------------------- |
| edit loop  | did that edit do what I meant   | one case, or the binary on one file directly |
| pre-commit | will this land green            | `bazel test //...`                           |
| merge gate | is `main` still correct         | `bazel test //...`                           |
| merge gate | does it still build optimized   | `bazel build -c opt //...`                   |
| merge gate | does it still build under GCC   | `bazel build //... --config=gcc`             |
| nightly    | is the C++ path still correct   | `bazel test //... --config=nightly`          |
| nightly    | is any of it reading freed bits | `bazel test //... --config=asan`             |
| nightly    | how far does a real design get  | `bazel test //tests:external_design_tests`   |
| each merge | how much more work does it make | `instruction-count.yml`, base against head   |

Pre-commit and the merge gate run the same tests, and that is the whole point: a green run before
committing means "this lands green" only while the two sets are identical. Any change that makes the
local command wider or narrower than the gate turns its answer back into a guess. The two builds are
where the gate is wider, and the section on the gating workflows says why each is there all the
same.

Every run someone waits on stops at the first case that fails, and every scheduled run does not. The
two want different things from a red result. A defect the whole corpus shares and a defect one case
has are indistinguishable from outside while a run is still going, and only the first is worth
waiting through -- so a person waiting is told as soon as anything is wrong, and pays one case for
it rather than the corpus. What a scheduled run produces is the list of which cases fail, and a list
that ends at its first entry is not one, so the two sets a schedule draws on turn it off and so does
the gate. `--config=full` is not one of them: it is a developer asking for everything, and they are
still waiting.

On top of the default set, what a change touches selects what else to run:

- **What the C++ backend emits** -- `--config=nightly`, the only thing that compiles emitted text.
  Read this one carefully: it is not "did I edit `backend/cpp`". The renderer is a function of MIR
  and LIR node shapes, so changing one of those shapes changes the emit without touching the
  backend, and a green default run says nothing about whether the result compiles. Emitting one case
  and reading the file catches most of that class in seconds.
- **The foreign boundary** -- the cases carrying foreign sources, which `llvm_tests` already runs.
- **What an emitted project is or how it is built** -- the shipped recipe, what the project is given
  of the runtime, how a prepared header is named -- `--config=nightly`, which carries
  `emitted_project_tests` beside the corpus. This is the same selection as the line above it, seen
  from the build rather than from the text.
- **The command line, the prelude PCH, or what the shipped runtime headers oblige a unit to emit**
  -- `cli_tests`, `prepared_header_test` and `runtime_surface_test`, already in the gate.
- **A shipped example, its declaration, or the command its README gives** -- `examples_tests`,
  already in the gate.
- **What a repeated construct or a wide type leaves behind when compiled** -- `growth_tests`,
  already in the gate.
- **Which units an edit makes again** -- `incremental_tests`, already in the gate.
- **How long an expression the compiler follows** -- `compile_stack_tests`, already in the gate.
- **Anything else** -- HIR, MIR, LIR, the execution backend, the runtime value library -- the
  default set is the whole answer.

As the LLVM path fills in, this shortens rather than grows: `cpp_tests` stops being the backend
under test.

## What a design's size may cost

The corpus cannot say what a compile costs: a compiler that writes a file per repetition, or
declares a loop in the square of its count, passes every case and is fast on each. Nor can one
compile, at any size. So `growth_tests` compiles each design under `tests/growth/` at one size and
at twice that, on both backends, each run asked for its trace and its statistics, and holds the
larger run to the smaller: what it left behind grown by what the design says its extra steps add.

**What a compile spent -- a stage's time, its peak memory -- is not held**, though the runs report
both. A test gives one answer for one tree, and those two are read off the machine running it as
much as off the tree, so a busy executor moves them from one run of the same tree to the next. A
stage whose time grows faster than its size is still a defect; finding it by counting what the stage
does rather than by timing it is not yet built.

A count the source text writes out -- the members of a structure or a union, the operands of a
chain, the arguments of a call, the ports of an instance -- is no parameter the language can
generate from, so a design may write one as `[[piece|separator]]`: the harness writes the piece N
times, joined by what follows the last `|`, with `{i}` in it each step's index and `{last}` anywhere
the last one's. The designs written that way reach every member and operand they declare, which is
what the emitted code does and what a test reaching only the first and the last would not.

A design states one thing and takes N steps of it, and its own file says what one more step adds.
Most say nothing, because a step of theirs repeats what is written once, and repeating adds nothing.
A design whose steps are unlike one another -- a width that follows the index, a child handed a
different size -- carries a `// @each-adds:` line naming each kind of thing a step is worth, with
how many where the source shows how many: one procedure written is one callable. What a compile left
is read from the statistics as every kind of thing its units hold in each form they pass through, so
the check names no kind itself. A kind the design does not name must not follow N, and one it names
must follow N as named; a count is held exactly, and the difference of the two runs holds nothing
that does not depend on the size. So a kind of thing the compiler begins to make once per step is
held from the day it is reported, and a statement in a design cannot outlive what it speaks of. How
deep the text a unit writes nests at the deepest is read off that text and held as one more such
kind, which no design names: every compiler reading the text bounds its depth, and nothing bounds a
count a design chooses.

Bytes are the one size among the counts. A design that does not name them may leave a quarter more
at twice the size, since a file can carry the size it was compiled at; one that names them must
leave more, and how much more is not held, because what a body is made of follows how much the
source wrote. What holds such a design is the counts.

Where a design departs from its statement today is recorded per path, in
`tests/paths/<path>.growth.yaml`, beside that path's refusal and defect records and by the same
rule. A design is held to exactly what is listed for it: something new departing fails the run, and
so does something listed that has stopped, until it goes. So the file only shrinks, and what is left
in it is what that path still owes.

## What an edit may cost

A build after an edit makes again the units that now mean something else, and no others. The corpus
cannot say whether that holds, and neither can one build. So each case under `tests/incremental/` is
one design twice, `before.sv` and `after.sv`, and its first line names the units the edit gives
another meaning. `incremental_tests` builds the design as it was and then as it is, on both
backends, and holds the units the second build made to that line. On the execution backend the two
builds share one store of kept objects and the second build's own statistics say which units it
made; the C++ backend keeps no object, so there a unit is made again where the text its translation
units are handed differs.

A case states one kind of edit -- text moved down, a comment added, a name changed, a body changed
that nothing else reads -- so what the record names is that kind. An edit that is not about where
text sits keeps every line where it was, because a unit that follows its position would otherwise
answer for every case. The record is `tests/paths/<path>.incremental.yaml`, held by the same rule as
the growth record: `made X` is a unit made again that the edit left meaning what it did, and
`kept X` one kept that it did not.

What is in it today: a unit's code holds the file and line of what a run reports by -- a severity
task's call, the procedure a stopped run was in -- so a unit holding either is made again when its
text only moves; and on the C++ backend a unit is made again when a variable of its own is renamed
or a class it includes gains a member it never reads.

## How much more work a change makes

Every test above asks whether something is right, and a change can pass all of them while every
program Lyra builds does a tenth more work. What says so is a count: `instruction-count.yml` builds
the compiler at a change and at the revision before it, and counts under callgrind how many
instructions each of them costs on two kinds of measurement, split the way rustc-perf splits its
benchmarks.

The real designs `tests/benchmark/designs.txt` lists come first, because they are what someone
building a design waits on: cases of Verilator's benchmark suite, each declared as
`external_design_tests` declares it, and counted whole on the execution backend -- the instructions
Lyra executes building it as an edit loop builds, and those its program executes running the suite's
quick check, built as a long run ships it. Counting runs a program about fifty times slower than
running it, so the list holds the designs whose count of both compilers fits about half an hour, and
each is a job of its own. The corpus cases come second: each is one cost family, counted on both
backends, and what it says is which path a real design's move came from. Two counts at an amount and
at twice it are differenced, so what a run does once, starting and constructing the design, cancels
exactly.

A count is the same however loaded the machine is, which is why it can say what a timing cannot: a
change of a few percent stands out on a runner whose wall clock moves by half from one night to the
next. It is not the same on every machine, though -- the C library picks its copy routines by
processor, and the runner image moves its compiler and standard library on its own schedule -- so
the two counts of each measurement are taken in one job, on one machine, and none is stored for
another day to compare against. What compares across days is the nightly benchmark's ratio to
Verilator.

A measurement is marked where it moved by two percent or more, or where the parent counted it and
the change could not. Two counts of one compiler agree to a few hundredths of a percent of the
count, which is far under two percent of a total. A corpus case reports a difference instead, and
what a build does before any of the design can be nearly all of each count, so the same few
hundredths can be most of the work counted. So each corpus case is counted twice at its lower
amount, and the disagreement, as a share of the work, is the case's resolution. A case whose
resolution is past a third of two percent fails, since a number it reports moves by itself as far as
any change would move it.

It is not on the path a change takes to merge, since the question is not one a merge waits for. A
merge to `main` is counted against its parent, so each count belongs to exactly one pull request,
and where any measurement was marked the table goes on that pull request as a comment: the real
designs whole, and of the corpus the cases marked, with every case below them, collapsed. A branch
expected to move one is counted before it merges by dispatching the workflow on it, and the same
dispatch compares any revision with another, its parent by default. The jobs take four runners at
most, so a pull request's own checks never wait on them.

The job goes red where the change has no count of a measurement -- one the parent counted, or one
that cannot be counted at all, such as a case whose resolution fails it -- and where one got worse
by two percent or more. A case that can never be counted is red on every merge until someone fixes
it or takes it out, which is the point: which cases stand is decided by a person, and the red is
what puts the question in front of one. It is not marked, though, and so posts no comment of its
own, because nothing about it changed. A duration is never failed on, because it reports on the
machine; a count does not, so a count that rose is a statement about the change. Being red holds up
nothing, since the job is not one a merge waits for: it marks the commit that did it, and the next
merge, counted against that commit, starts clean of whatever that commit moved.

The host compiler a build starts is not counted: tracing it would cost fifty times a compile that is
most of a build, and what Lyra hands it -- how many files, their size and how deep they nest -- is
held by `growth_tests` along the `cpp` path.

## What a real design stops on

The corpus states the standard a clause at a time, and each case is one small unit. What stops a
design someone wants to simulate is a hierarchy, a size, or a way of writing that no case thinks to
write, so `external_design_tests` builds and runs the designs of Verilator's benchmark suite: for
each, the case the suite itself marks as its quick check, built on the execution backend, run with
the suite's arguments, and held to the suite's own check of the output.

Nothing of Lyra's is kept in the suite. `MODULE.bazel` names one commit of it, and the test writes
each design's declaration from the descriptor the suite gives every simulator. Three things are said
of every design that its descriptor does not say, because they are how the simulator the sources
were prepared for reads them: the macros that simulator predefines, every file as one compilation
unit, and, where the sources state one time scale and leave other elements without, that time scale
for all of them.

Anything applied on top of that commit, as a patch `MODULE.bazel` names, is a correction the suite
itself has been sent. Its testbenches were run on a simulator that keeps no unknown value, so one
can ask for what the standard does not grant, and a design stopped there is measured on nothing.
Such a correction is right for any simulator that follows the standard, is argued from the clause
with a case of the corpus holding it, and is applied only until the named commit contains it. One
the suite declines is taken out again, and the case is then recorded as stopping where it does.

A text the standard does not settle is not corrected, since its author is not wrong either, and
neither is a forbidden text Lyra runs as an extension when asked
([`language_conformance.md`](../architecture/language_conformance.md) says when that is so), nor a
forbidden text that is its author's where the front end has an option that reads it. The front end
refuses each unless asked, so the design is built with that option, which is how someone who cannot
edit the design builds it, and what stands behind the text is still measured. That holds only where
the text means one thing under every reading the option could give it and the option lets nothing
else in the design through; where the meaning would depend on the reading, the case stops there. The
options each design is built with are listed in `tests/external_design_options.yaml`, and one a
design no longer needs fails the run until it is taken out.

A build that takes more than half an hour, a run or a check that takes more than five minutes, or a
step that asks for more memory than the smallest machine these run on holds, stops the case there.
The build's limit is set well past the slowest build measured, so that a design does not fall on one
side of it on one machine and the other on the next; what it catches is a build that will not end. A
run past its limit is either stuck or slow, and waiting longer does not say which. It is asked to
end before it is killed, and the simulation time it says it had reached, with the procedure running
then, is printed beside where it stopped. Each build compiles a fixed number of units at once, so
how wide a build ran does not differ from one machine to the next either. How long each case took is
printed beside where it stopped, and a case that stops keeps the directory it was built in and names
it, while one that gets all the way leaves nothing behind.

Where each case stops is recorded in `tests/paths/llvm.external_designs.yaml`, and a case is held to
exactly what is listed for it, so the file is the account of what stands between Lyra and each
design. Two things keep the target out of every run that does not name it. The suite is half a
gigabyte to fetch, and a target that a tag filter leaves out is still analyzed, which is what
fetches; `manual` is the tag that prevents that. And one case is minutes, so the nightly gives each
its own job.

## The corpus runs the program a user builds

A case on the LLVM path is built into a program and that program is run, which is exactly what
`lyra run` does for a user -- so the gate measures the artifact that ships, linked against the same
static runtime library, rather than some other way of executing the same module.

The one thing this leaves out is optimization: the corpus builds at the default level, and `-O`
optimizes. An optimizer is free to do anything at all with a module that contains undefined
behaviour, so an optimized build and an unoptimized one can disagree where the emitted module is
already wrong in a way one of them happens not to expose. That is not a statement about IEEE 1800,
and it belongs in the same place the C++ path is heading: a small set of designs, run for the
artifact rather than for the claim.

The C++ path is on its way out of this table. Its subject is not what IEEE 1800 requires -- the
execution backend answers that -- but whether an emitted project still builds and runs under a host
toolchain, which makes it a tool test rather than a conformance path. When the execution backend
covers enough of the language, it stops running the corpus and becomes a small, explicitly chosen
set of designs -- which is what `emitted_project_tests` already is, so the two become one target
rather than the corpus shrinking into a new one. Nothing in the corpus has to change for that,
because no case names a path.

## Gating workflows

Each runs on pull requests, and all but `exception-policy.yml`, `architecture.yml` and
`runtime-abi.yml` on push to `main` as well.

| Workflow               | What it enforces                                                                                       |
| ---------------------- | ------------------------------------------------------------------------------------------------------ |
| `bazel-build.yml`      | `bazel build //...` with warnings fatal, then the default set, the same build optimized, and under GCC |
| `cpp-style.yml`        | `clang-format` over `src include tests`, plus C++ style policy                                         |
| `bazel-lint.yml`       | `buildifier` formatting and lint warnings                                                              |
| `md-format.yml`        | Prettier over every markdown file                                                                      |
| `ascii-policy.yml`     | ASCII-only, on the diff against `origin/main`                                                          |
| `exception-policy.yml` | The thrown-type policy, on the same diff                                                               |
| `architecture.yml`     | Layer boundaries between the IRs and the backends                                                      |
| `docs-policy.yml`      | The doc claims a machine can settle (paths, links, indexes)                                            |
| `pinned-sources.yml`   | The front end's pinned commit is on the fork's branch for its release                                  |
| `runtime-abi.yml`      | Each runtime entry is declared, defined and published alike                                            |

`bazel-build.yml` runs its two commands as steps of one job rather than as two jobs. The separate
durations are worth having, but a second runner would not inherit the first's analysis: Bazel holds
that in its server's memory, no cache moves it between machines, and it is the dominant cost here --
around fifty seconds against a critical path of one, because both commands find every action already
cached remotely. One job keeps the two durations honest, since the first pays for analysis once and
the second then reports what running the tests costs. A compile failure still shows as the build
step going red.

Its third command, `bazel build -c opt //...`, is a job of its own: the whole graph built optimized,
and nothing run. The benchmark and the external designs both start from an optimized compiler, and a
compiler reports of an optimized build what it does not of an unoptimized one, because what it
concludes about a value after inlining exists only there. With warnings fatal, such a report stops
both scheduled jobs from measuring anything, and a nightly would be the first to find it, a day
after the change that caused it had merged. A different compilation mode shares neither analysis nor
one output with the build beside it, which is why it is not a step of that job. A change can pass
`bazel test //...` on a developer's machine and fail here; the command above reproduces it, and
under remote execution it compiles only what the change touched.

Its fourth command, `bazel build //... --config=gcc`, is a job of its own too. Lyra is built by the
pinned clang and by GCC, each warns of things the other does not, and every warning is fatal, so a
change is clean only when both build it. The job builds under the remote image's GCC and runs
nothing, since the tests already ran under clang. It executes remotely beside the other jobs, so it
adds work to the remote queue and no time to the gate, and the same command reproduces it wherever a
key is set. That GCC exists only in the image, so with no key the job builds nothing and says so.

## Nightly workflows

These run on a schedule and on demand rather than per merge, and they do not agree on severity. Each
carries `-nightly` in its file name and `(Nightly)` in its title, so a listing of the directory
separates them from everything that runs per change without opening any of them. The suffix states a
property rather than distinguishing a variant, and that is the whole of the convention: there is
deliberately no non-nightly sibling of any of them. One that gained a sibling would be the same
subject at two cadences, which is a reason to reconsider the split rather than to name around it.

`cpp-tidy-nightly.yml` re-analyzes every translation unit from scratch. It **reports rather than
gates**, so its findings are a backlog to pay down and a red run is not an alarm.

`host-cxx-nightly.yml` runs the `nightly` target. It **gates its own run**: a corpus case that stops
passing is a regression, and a red nightly is the thing to act on that day. Its actions run on the
runner, under the clang installed there, because a developer's own run of the set already answers
for the remote image's GCC; they keep their results in the shared cache where the key is set, so a
night compiles what changed rather than the whole of Lyra and LLVM on four cores.

`asan-nightly.yml` runs the merge gate's own set under the address sanitizer -- the gate's set and
not the `nightly` complement, because the compiler it is watching is exercised there rather than in
the program the host-compiling group builds. It **gates its own run**, for the reason the host C++
one does: a sanitizer report carries a stack trace and names a defect. What makes it worth a nightly
at all is that the class it catches is invisible to every other job: a read of memory that is gone
usually reads what was there, so a green ordinary run is consistent with the code being wrong. Under
the sanitizer such a read is reported on every execution that makes it; what the job still cannot
see is an execution the corpus does not reach. Under it the corpus builds every program it runs with
`--sanitize address`, so the job watches what the compiler builds as well as the compiler: about
half of the lifetime defects so far were in generated code or in the runtime a program drives. The
compiler's own IR pools are not in that class, since they never move what they hold
([arena-reference-lifetime](../decisions/arena-reference-lifetime.md)). It runs the set twice, once
under each compiler that builds Lyra, both under remote execution where the key is set. The two
evaluate a call's arguments and an operator's operands in opposite orders, which C++ leaves to the
compiler, so a read in one operand of storage another one frees is reported under one of them only.

Three things about it follow from what is instrumented. A program the corpus builds is checked
because it asks to be, never because the compiler around it is: the runtime library it links is the
one built for the sanitizer it names, and what such a program finds that is not fixed at once is
held in `tests/paths/llvm.address.defects.yaml`, since the same case passes in every other run. And
the targets tagged `cost` are left out of its set: each bounds what the compiler, or a program it
built, costs as it ships -- time, stack, memory -- which is not what a sanitizer build is. And every
size's time limit is doubled, since the instrumentation makes each test about twice as slow and
Bazel's override of the limits is meant for a condition known to be slow.

`external-designs-nightly.yml` runs `external_design_tests`, one job per case, with the compiler
built optimized. It **gates its own run**: a case that stops somewhere other than where it is
recorded as stopping is either further along, which is an entry to update, or a regression. One that
newly stops at its check is read against the standard before it is called either, since the check is
the suite's and was written for another simulator. The jobs are the cases the test itself lists, so
a design the suite gains is run without this file changing.

`benchmark-nightly.yml` builds every case under `tests/benchmark/` on each backend and times it
beside Verilator on the same sources, giving each tool the amount of work it needs to reach the same
duration. It gates its own run on a narrower claim than a timing: a case that fails to build, fails
to run, or outlives the time limit is red, while the rates themselves are printed for a reader.
Failing a run on a rate would be reporting on the machine that ran it. Such a case costs only
itself: the runner goes on to the others and exits non-zero only after printing its report, so a red
run's summary still carries every rate that was obtained and names each case that has none, with the
step that failed, how it ended, and the last lines it printed.

The comparison it prints needs no stored history, which is what makes it worth running unattended:
both tools are measured on one machine in one run, so the ratio between them survives a slow runner
and a change of runner in a way an absolute time does not. Whether _one change_ regressed is a
different question, and the instruction count above answers it.

## What is deliberately absent

**A timing per change.** A timing carries a regression only once something compares it to the same
case before the change, and on a shared runner the two differ by more than most regressions do, so
the comparison would need repeated runs and a statistical threshold before it said anything. The
instruction count needs neither, because it does not move with the machine's load; that is why the
per-change job counts and the nightly times.

**A standing diagnosis job.** One lived here, comparing a cached compiler's artifacts against a
freshly built one to chase a specific incident: a build that selected AVX-512 and produced a binary
that died on a machine without it. Every fact it rested on -- an environment variable, a marker
string, a library name, the emitted binary's name -- had since stopped being true, and it had been
disabled long enough that nobody noticed. A diagnosis job is written against the failure in front of
you, and keeping a disabled one costs more than writing the next one, because a stale one has to be
disbelieved before it can be replaced.

## Remote execution

Every flow that can run remotely takes its flags from the `BUILDBUDDY_API_KEY` repository secret.
With the secret unset -- a fork, or a dispatch without secret access -- the same commands run
locally. Nothing requires an account.

A flow running one bazel command builds them inline:

```bash
RBE_FLAGS=""
if [[ -n "$BUILDBUDDY_API_KEY" ]]; then
  RBE_FLAGS="--config=rbe --remote_header=x-buildbuddy-api-key=$BUILDBUDDY_API_KEY"
fi
bazel build -c opt //:lyra $RBE_FLAGS
```

The job of `bazel-build.yml` that builds and tests runs two, and `instruction-count.yml` builds at
two revisions, so each appends them to `.bazelrc.user` instead -- the untracked layer the build
configuration already reserves for one checkout's own key. Two command lines can drift apart; one rc
file cannot, and flags that differ between the two invocations would throw away the analysis that
keeping them in one job exists to share.

The pinned clang builds Lyra remotely and locally alike, and so do the C library and the C++ library
it compiles against, which the build makes from source at the versions the target platform names. So
a remote build and a local one compile the same thing, whatever either machine has installed
(`docs/decisions/a-build-compiles-for-the-oldest-platform-it-supports.md`).

Every runner that runs Bazel holds that compiler's files, even when no action runs there, because
each action's key hashes its inputs on the machine running Bazel. So it is a fixed cost of every job
that builds with clang; the module that provides it publishes a minimal archive of the files its
toolchain reads, rather than the official release, which unpacks to 11 GB. Moving the pin is the
LLVM release in `MODULE.bazel`, which names that archive too.

## LLVM toolchain

A job that needs a specific LLVM gets it from the `setup-clang` composite action, naming only the
extra tools it wants:

```yaml
- name: Setup Clang
  uses: ./.github/actions/setup-clang
  with:
    tools: llvm
```

The action also points the unsuffixed names at that release, and every call site uses them: a job
runs `clang-format` or `run-clang-tidy`, never a version-suffixed binary. So the release is written
once, inside the action, and a bump is one edit with nothing else to keep in step.

Pointing a name at a release and that name resolving there are different facts, and the action
verifies the second one before any job proceeds. It has to: `update-alternatives` governs `/usr/bin`
alone, so anything earlier on PATH still wins, and it declines a path that is already a plain file
rather than a symlink. Both failures are silent, and a job that installs a toolchain and then
compiles with the runner's own would go green while testing something nobody asked for. The check
prints each tool's full `--version` output and fails the step on a major-version mismatch, so the
wrong toolchain surfaces at setup rather than as a puzzling result much later.

It checks only the names the job installed a release binary for. The runner image ships older LLVM
tools at unsuffixed paths -- a `clang-tidy` several majors behind is there whether or not anything
asked for it -- and failing a job over a tool it never invokes would be a false alarm rather than
the hazard. A job that wants a tool covered asks for it in `tools`.

None of this is the compiler that builds Lyra, which is a toolchain the build fetches itself, pinned
in `MODULE.bazel`; the action installs what a job runs outside Bazel -- a formatter, clang-tidy, the
host compiler a test or a benchmark hands an emitted program to.
