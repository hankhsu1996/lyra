# Building A Program

A command that builds a design is a compiler and a build tool at once: it lowers and compiles every
unit itself and drives the host compiler where the C++ backend needs one, so what a build tool owes
the person waiting on it is this command's to owe. This doc owns that part -- the files a command
claims and what it cleans up however it ends, what it shows while it runs, how it offers the host
compiler a prepared header, how it optimizes a program together with the runtime under `--lto`, how
it builds a program that checks its own memory, and how it builds one that counts its scheduling
work. Which artifacts a design compiles into is [compiling_a_design.md](compiling_a_design.md)'s,
how a result is kept and found again is [incremental_build.md](incremental_build.md)'s, what a
project declares is [project_model.md](project_model.md)'s, and the runtime a program links is
[runtime_distribution.md](runtime_distribution.md)'s.

## A build answers for what it starts

**A build refuses a request it cannot answer before doing work the refusal throws away. However it
ends, it leaves what was named and what the store keeps, and nothing it started. While it runs,
whoever waits on it can see what it is doing, in the words of someone who wrote the design.** Each
half fails in proportion to the design -- a whole compile thrown away for a destination that could
not be written, a directory the size of the design left by a killed build, a watchdog unable to tell
a slow stage from a stuck one -- which is why a small design never shows any of them.

Three conditions separate this command from the compilers it is modelled on. It is also the build
tool, so the status is its to print where clang and rustc say nothing and leave it to their driver.
What it makes on the way is a tree -- a directory of emitted sources and objects -- which a signal
handler may not walk, where LLVM's handler removes single files. And a design can take the machine
past what it has, so the system ends the build with no turn to act, and an answer covering only the
signals a process can catch leaves a tree each time.

**A file a request names is claimed when the request is read.** Whether a place can be written is
known when the request is read, and the reading of it that cannot disagree with the later write is
the write's own first step, as clang opens its output before it compiles. So claiming a file makes
its directory and begins a temporary file beside it, what is produced is written into that, and
finishing renames it onto the destination. A claim that cannot be made refuses the request before
the design is read; a claim dropped unfinished removes what it began, so a place a request named
holds the whole file or what it held before. The program, the time trace and the statistics are such
files, and an emitted project's claim is its directory, made at the same moment. A program that
takes its name from the design is claimed once the design has one, which is after elaboration and
before anything is lowered: the name of the library its `lyra.toml` declares, or else the design's
one top. A design named on the command line alone with several tops has no name to take, so `build`
refuses it unless `-o` names the program, rather than choosing one of the tops. A directory that is
only missing is made, for every one of these; a file's place that holds a directory is refused,
since writing the file there would replace a tree the request never named.

**What the process holds, it gives up on every ending it can act in.** The process keeps a list of
what it would have to remove -- the directories it builds in, the claims it has not finished -- and
of the tools it has started; returning and throwing unwind them. A request to end is answered by one
thread that waits for it while every other thread and no child holds it off: it passes the request
to each tool still running, removes what is listed, and ends the process by that same signal, so
whoever asked reads the ending they asked for. Nothing is added to the list, and no tool is started,
once the answer has begun. A new file a command can be told to write is claimed where the others
are, and a new tool is started where the others are, or a signal does not reach it.

**The next build removes what a killed one left.** A directory a build works in is held locked for
as long as its owner lives, and the system releases the lock however the process ends. Making one
removes every other that nobody holds, and such a directory has a name of its own kind, so nothing
else in the temporary directory is ever considered. A build killed outright therefore leaves its
directory until the next build on the machine starts.

## A command shows what it is doing now

What is shown is the state at the moment the display looks, never a record of each thing that
happened: the phases that are over, the phase under way, how many of its pieces are done, and the
pieces being worked on with how long each has been, longest first, which brings a slow piece to the
top. A display that looks at intervals is what makes the rest follow without a rule of its own: a
command over before the first look shows nothing, so the error stream of a short command is its
diagnostics and nothing else; a phase that is one indivisible step still shows time passing; and a
build that takes everything from what was kept shows nothing, because nothing was under way long
enough to be seen.

**The words are the reader's.** Whoever waits on a build wrote the design and not the compiler, so a
phase is named for what happens to their design -- elaborating, generating C++, compiling, linking
-- and a piece by the identifier the source declares it under, which a unit carries beside the name
its artifacts take. A piece the source never named is counted and not listed. The time trace names
its stages for a compiler developer, and the two vocabularies are stated at separate places.

On a terminal the status is a line for each phase that is over with how long it took, then a line of
phase, count and time above the pieces under way longest, redrawn several times a second from one
second in and taken off before anything else is printed; a phase over between two looks leaves its
line, and one that took no time anyone would read leaves none. Anywhere else the status is one line
at ten seconds, at thirty, and every minute after, opening with the elapsed time in brackets, which
nothing else the compiler prints does, so a reader wanting only diagnostics drops the lines that
open with a bracket. A command that showed a status and did what it was asked leaves how long it
took. How many errors the command has to report is part of the status, so a build that has already
failed can be ended instead of waited out; the errors themselves are reported once, in source order,
when the command ends.

Which commands show it follows from what else their streams are for. A command whose product is what
it prints shows nothing. `run` shows it only on a terminal, because what a run prints belongs to the
program. The rest show it wherever the stream goes, and `--progress` overrides this either way.

## The host compiler is offered a prepared header, never required to take one

Every emitted translation unit includes one header naming the whole runtime surface, and compiling
it costs a unit more than its own code does: most of that is instantiating the same templates in
every unit, the rest parsing. A header prepared once from those headers takes both off each unit and
is worth it from the first unit, so both things that build an emitted project prepare one: the build
behind `build` and `run`, and the `build.sh` a project ships, which may assume nothing beyond POSIX
sh.

**An accelerator may change how long a build takes. It may not change whether the build succeeds,
and it may not change what the build produces.** The two halves fail in opposite directions -- a
refused header must not fail the build, and a stale one must not be used -- and anything that only
makes either rarer tunes a probability where the requirement is absolute.

- **A prepared header is offered to a compile and never required by one.** Where a compile given one
  fails, it runs again without one before its output counts as a failure. A compile that then
  succeeds was refused the header rather than the source: the build goes on, nobody is told, and the
  header is dropped so the next build prepares it again. So the compiler's own rule for accepting a
  header need not be known or kept in step with, as ccache falls back to the real compiler whenever
  it cannot be certain a cached result applies. A refusal has to be one the build can see, so GCC,
  which passes over a header it cannot use with only a warning, is asked to make that warning an
  error. A source that genuinely does not compile runs a second time without one, and the
  diagnostics the user sees come from that compile.
- **A prepared header is named by every file it was made from and by how it was prepared.** Not
  every compiler checks a header's inputs as it loads one -- GCC compiles against what a changed
  header used to say -- so the name covers the list the compiler itself gives of what the prelude
  reads, the C++ library's and the system's headers among them, each by its content, and the command
  line that prepared it. A header that does not match what is on the machine is never found,
  whatever the compiler would have said of it, and two entries with one name are equivalent by
  construction. The files are those the prelude reads and nothing else, so an edit no design can
  observe renames no entry.
- **Two measures keep the fast path fast, and neither is load-bearing.** Where the compiler checks a
  header's inputs as it loads it, it is asked to decide by content rather than by timestamp, on the
  command line that prepares the header and every one that loads it; and whatever writes a file
  leaves it alone when it already holds those bytes. Either failing costs speed.
- **The command that prepares a header asks for the templates it reaches to be instantiated into
  it**, so a unit including the result performs none of them: work that is the same for every unit
  of every build is done once, where the artifact those units share is made. Only the preparing
  command carries this, since it decides what the artifact holds. It asks that every header the
  surface names compile on its own, which is what an umbrella header is.
- **A build asks the compiler in hand about each optional flag, one at a time, and never which
  compiler it is.** An empty header is prepared with the flag alone, and the flag is taken where the
  command succeeds and prints nothing, since a compiler may warn of a flag and ignore it; the answer
  is kept per compiler. This is how cc-rs's `flag_if_supported` and Bazel's configuration of a host
  compiler treat an optional flag, and it is right for a compiler nobody listed.

Both builders offer the header the same way whichever compiler runs: every compile is told to
include a header holding one include of the prelude, and the prepared header sits beside it under
its name and `.gch`, where GCC looks for it and clang's driver looks too. The shipped recipe
compiles everything again rather than only what failed, since tracking which jobs failed costs more
shell than the rare path is worth, and holds back what the compiler said until the build is known to
have failed, so a retried build that succeeded says nothing.

## A build under `--lto`

`--lto` asks for the design's code and the runtime it calls to be optimized as one program, so a
runtime function can be inlined into the design's code. The runtime ships in the forms that needs
([runtime_distribution.md](runtime_distribution.md)); how each backend uses them follows clang's and
the host toolchain's own way of asking for it.

**On the execution backend the link is ThinLTO's, run by Lyra as a linker runs it.** Each unit is
compiled as clang compiles a file for `-O<n> -flto=thin` -- the pre-link pipeline at that level --
and written as bitcode with its summary. The link reads that bitcode and the runtime's, and resolves
each symbol as a linker does: a runtime member joins the program where it defines a symbol the
program still leaves undefined, the first definition met prevails, and a definition anything outside
the bitcode names stays visible to it. It then optimizes and compiles each module in parallel, as
many at once as the build's width, keeps each result in the store under the key LLVM gives it, and
hands the host link machine code. A runtime member carrying no bitcode is left to the host link.
Every function the execution backend defines states the processor clang names for the target when
given none, as clang states it on every function it emits, because a call between functions compiled
for different processors is never inlined, and the runtime was compiled for that one.

**On the C++ backend the host compiler does it, in the first way its toolchain accepts.** The
compiler compiles the runtime's sources beside the design at the build's level, every compile and
the link told the same form. Which form a toolchain accepts is found once per compiler, by compiling
and linking a probe in each form in turn: ThinLTO with the gold plugin's cache, which bfd and mold
read too; ThinLTO with lld's cache, since lld refuses the plugin's spelling; ThinLTO with no cache;
then GCC's link-time optimization with its incremental cache, and with none. GCC refuses ThinLTO's
spelling, so one list serves either compiler without asking which it is. Where no form is accepted
the program is built the plain way; where one is, a failure after it is the build's own and is
reported as one.

## A program built to check its memory

A read of storage that has ended usually reads what was there, so a program that makes one prints
the right answer and every test of its output passes. Much of the code that can make one -- what
Lyra generates, and its runtime as a generated program drives it -- is in no source file of Lyra's,
so no analysis of Lyra's source sees it; what sees such a read is the program, built to check every
load and store it makes.

**`--sanitize address` builds the program so that its first read or write of storage that has ended,
or was never its own, ends the run with a report naming where it was made and where the storage was
freed.** It is an invocation property, refused in `lyra.toml` like `-O` and `--lto`, and it changes
what a build compiles and links and nothing the program computes.

**Whoever instruments the code links the sanitizer's runtime**, because instrumented code names its
runtime's version. On the execution backend Lyra instruments a design's modules itself, as clang
does at the end of its optimization pipeline, so the program links the sanitizer runtime of the LLVM
release Lyra links and the runtime library that release compiled for the sanitizer
([runtime_distribution.md](runtime_distribution.md)); under `--lto` the design's modules are
optimized together and the runtime linked as a member carrying no bitcode. A user's foreign sources
are compiled there by the user's compiler, whose instrumentation goes with its own runtime, so they
are not instrumented, and their reads through the C library are still checked, since the sanitizer
runtime intercepts it. On the C++ backend the host compiler instruments everything: it compiles the
runtime's sources beside the design and links its own sanitizer's runtime, and the user's foreign
sources are instrumented with them, in the shipped recipe as in the in-place build. Foreign code
handed over compiled was instrumented, or not, by whoever compiled it.

**A program `run` starts for a checked build is told where the release's symbolizer is, unless the
person running Lyra already named one**, since Lyra both supplies the instrumenting release, as
clang does, and starts the program, as a test runner does. A program `build` wrote and a person
starts finds a symbolizer as clang's and rustc's programs do, on `PATH`.

## A program built to count its scheduling work

**`--runtime-statistics` links the build of the runtime that counts its own scheduling work**, which
a run of the program writes where its `+lyra+stats=<file>` says ([scheduling.md](scheduling.md)). A
count is work on every event, a fixed share of each event's cost and a growing one as the event path
gets cheaper, so only a program built to count carries any counting, as LLVM compiles its statistics
only into the builds that ask for them and Tokio its worker metrics. The counting build has the same
headers as the plain one, so every unit's object is the same either way and asking for counts only
links again. It is an invocation property like `--sanitize`, and the two are not asked for together,
since the build compiled for the sanitizer counts nothing. `emit cpp` does not take it: counts are
for the runs a command builds.

## Forbidden shapes

- **Checking that a destination is writable, then writing it later.** It fails refusing a request
  before work the refusal throws away with one answer: two answers to one question, and the check is
  the one that can be wrong.
- **Removing a build's tree from a signal handler.** It fails giving up what the process holds on
  every ending it can act in, since a handler may unlink a file and may not walk a directory.
- **Building beside the output, in the user's project.** It fails leaving nothing a build started: a
  killed build's leftovers would land where nothing of Lyra's may sweep.
- **A line printed as each stage begins and each piece ends.** It fails the requirement that a short
  command's error stream is its diagnostics, and writes a log proportional to the design whether or
  not anything was compiled.
- **The time trace's stage names as the status, or an estimate of the time left.** The first fails
  the reader's words; the second states what the count done cannot say, since one unit can take a
  hundred times another.
- **A prepared header a compile requires, or a refusal recognised by reading the compiler's text.**
  Each fails the requirement that an accelerator never decides whether a build succeeds; running the
  compile again answers the question by construction.
- **A prepared header named by fewer files than it was made from, left to the compiler's own
  check.** It fails the requirement that a stale header is never used, wherever the compiler does
  not check.
- **A table of optional flags per named compiler.** It fails a compiler, or a release, the table did
  not name.
- **Every emitted project carrying the runtime's sources.** It fails the requirement that a unit
  pays for what it contains: a project not built with `--lto` or `--sanitize address` compiles none
  of them, and copying them costs every such project for nothing.
- **Trying link-time optimization on fat objects in every build.** It fails the build's iteration
  time: a build not asked for `--lto` would read and discard the bitcode of every object it links.
- **A plain rebuild after a form the toolchain accepted fails.** It fails the requirement that a
  build's failure is reported as what it is: the probe already showed the form works, so a later
  failure is the design's or the build's, and building again hides it behind a slower success.
- **A C++ link under `--lto` that keeps no cache where the toolchain offers one.** It fails the
  iteration time the store exists for: every link after an edit would optimize and compile every
  module again, where a cache compiles only the modules whose code or imports changed.
- **Merging a design's units into one to pay the fixed cost once.** It fails `north_star.md`
  invariant 4: one unit has neither partial recompilation nor concurrent compilation.
- **Linking the host compiler's sanitizer runtime to code the execution backend instrumented.** It
  fails linking the runtime of the release that instrumented the code (premise: the host's compiler
  need not be that release).
