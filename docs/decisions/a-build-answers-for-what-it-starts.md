# A build answers for what it starts

Date: 2026-10-09 Status: accepted

## The requirement

> A build refuses a request it cannot answer before doing work the refusal throws away. However it
> ends, it leaves what was named and what the store keeps, and nothing it started. While it runs,
> whoever waits on it can see what it is doing, in the words of someone who wrote the design.

Three reports from a design far larger than the corpus stated the three halves separately. A program
named into a directory that did not exist was compiled in full and then failed to copy. A build that
was killed left the directory it had been building in. A compile that ran for minutes printed
nothing until it ended, and a watchdog around it could not tell a slow stage from a stuck one. Each
costs in proportion to the design, which is why a small design never shows any of them.

They were three positions of wider axes. The file a run writes its time trace or its statistics to
was opened only after the whole build, so a build could succeed, print where the program was, and
exit in failure. A project emitted into a place that could not be written was refused only after
every unit had been declared. A build asked to end left the host compiler it had started running.

## What the field does

Read in source:

- **clang** opens its output before it compiles: a temporary beside the destination, renamed at the
  end and registered for removal on a signal (`CompilerInstance::createOutputFileImpl`, through
  `setAtomicWrite` and `setDiscardOnSignal`). It makes no directory for `-o`.
- **LLVM** takes SIGHUP, SIGINT and SIGTERM in a handler that unlinks the files registered with it
  and raises the signal again under its default action (`lib/Support/Unix/Signals.inc`). What it
  removes is files, which a handler may unlink.
- **rustc** settles its outputs before analysis: an output that would replace a directory or the
  input is fatal, and the output directory is created (`rustc_interface/src/passes.rs`,
  `write_dep_info`). What it links from is made beside the output, never in a shared temporary
  directory (`rustc_codegen_ssa/src/back/link.rs`).

- **Ninja** shows the last edge that finished, on one line rewritten in place when the output is a
  terminal that is not `dumb`, and a line per edge otherwise (`status_printer.cc`,
  `line_printer.cc`). It has no timer and shows nothing of what is still running.
- **Bazel** documents a display redrawn at most every fifth of a second under cursor control, and
  where there is none a progress line after ten seconds, after thirty, and then every minute
  (`--curses`, `--show_progress_rate_limit`, `--progress_report_interval`).
- **Zig**'s `std.Progress` holds a tree of what is under way and draws it from a thread of its own
  after an initial delay, so work that is soon over is never drawn, and draws nothing when the
  output is not a terminal.
- **Docker** takes `--progress=auto|tty|plain|quiet`: the display follows the stream unless told.

Recalled rather than read: make and ninja pass a terminating signal on to the commands they started
and delete the target being made; Bazel removes sandbox directories a previous server left when it
starts; Bazel's terminal display is one line of count and elapsed time above the actions running,
each with its own time; cargo tells a terminal how far it has got only where the terminal is one it
recognizes.

Where the conditions differ:

- **The compiler is also the build tool.** clang and rustc compile one unit and are driven by
  something else, so they say nothing while they work and the driver reports. A build here lowers
  and compiles every unit of a design itself, so the status is its to print.
- **What a build makes on the way is a tree.** LLVM removes from the handler because it removes
  single files. A directory of emitted sources and objects is walked to be removed, which a handler
  may not do, so the request is taken by a thread that waits for it.
- **A build is killed for memory.** A design can take the machine past what it has, and the system
  then ends the build with no turn to act. An answer that covers only the signals a process can
  catch leaves a directory proportional to the design each time that happens.

## Decisions

### D1. A file a request names is claimed when the request is read

Whether a place can be written is known when the request is read, and the reading of it that cannot
disagree with the later write is the write's own first step. So claiming a file makes its directory
and begins a temporary file beside it; what is produced is written into that; finishing renames it
onto the destination. A claim that cannot be made refuses the request before the design is read. A
claim dropped unfinished removes what it began, so a place a request named holds the whole file or
what it held before.

The program, the time trace and the statistics are three such files. An emitted project's claim is
its directory, made at the same moment. A program that takes its name from the design is claimed
once the design has one, which is after elaboration and before anything is lowered.

A directory that is only missing is made, for every one of these. An emitted project's always was,
and one option meaning two things on two commands is the defect, not the convenience.

### D2. What the process holds, it gives up on every ending it can act in

The process keeps a list of what it would have to remove -- the directories it builds in, the claims
it has not finished -- and of the tools it has started. Returning and throwing unwind them. A
request to end is answered by one thread that waits for it while every other thread and no child
holds it off: it passes the request to each tool still running, removes what is listed, and ends the
process by that same signal, so whoever asked reads the ending they asked for. Nothing is added to
the list, and no tool is started, once the answer has begun.

### D3. The next build removes what a killed one left

A directory a build works in is held locked for as long as its owner lives, and the system releases
the lock however the process ends. Making one removes every other that nobody holds. Such a
directory has a name of its own kind, so nothing else in the temporary directory is ever considered.

### D4. A command shows what it is doing now, on the error stream

What is shown is the state at the moment the display looks, never a record of each thing that
happened: the phases that are over, the phase under way, how many of its pieces are done, and the
pieces being worked on with how long each has been. A display that looks at intervals is what makes
the rest follow without a rule of its own. A command over before the first look has shown nothing,
so the error stream of a short command is its diagnostics and nothing else. A phase that is one
indivisible step still shows time passing. A build that takes everything from what was kept shows
nothing, because nothing was under way long enough to be seen.

**The words are the reader's.** Whoever waits on a build wrote the design and not the compiler, so a
phase is named for what happens to their design -- elaborating, generating C++, compiling, linking
-- and a piece by the identifier the source declares it under, which a unit carries beside the name
its artifacts take. A piece the source never named is counted and not listed. The names the time
trace gives its stages are a compiler developer's and stay as they are; the two are separate
vocabularies stated at separate places.

On a terminal the status is a line for each phase that is over with how long it took, then a line of
phase, count and time above the pieces that have been under way longest, redrawn several times a
second from one second in, and taken off before anything else is printed. Longest first is what
brings a slow piece to the top. A phase over between two looks was never seen under way, so the line
it leaves is where it is seen at all; one that took no time anyone would read leaves none. Between
two phases those lines stay, so the status never vanishes and comes back. Anywhere else the status
is one line at ten seconds, at thirty, and every minute after, opening with the elapsed time in
brackets, which nothing else the compiler prints does. A command that showed a status and did what
it was asked leaves how long it took, and on a terminal the lines of its phases above that, which is
where someone asks first why this build was slow.

How many errors the command has to report is part of the status, so a build that has already failed
can be ended instead of waited out. The errors themselves are still reported once, in source order,
when the command ends.

Which commands show it follows from what else their streams are for. A command whose product is what
it prints shows nothing. `run` shows it only on a terminal, because what a run prints belongs to the
program. The rest show it wherever the stream goes. `--progress` overrides this in either direction.

## Rejected alternatives

- **Checking that a destination is writable, then writing it later.** Two answers to one question,
  and the check is the one that can be wrong: rustc's own asks whether an existing file is marked
  read-only and says yes to everything else.
- **Removing from the signal handler.** Sound for a file and not for a tree.
- **Building beside the output instead of in the temporary directory**, as rustc does. It makes a
  killed build's leftovers appear in the user's project, where nothing of ours may sweep.
- **Ending each tool with its parent through the kernel.** It covers the kill nothing catches, and
  it means starting every tool by forking a process that holds a design. A tool left by a killed
  build finishes the one file it was on.
- **Status only when asked for.** The person who meets a silent ten-minute build does not know there
  is an option.
- **A line as each stage begins and each piece ends.** A record of events: a design of a thousand
  units writes two thousand lines into a log, a build that reused everything writes as many as one
  that compiled everything, and a short command's error stream is no longer its diagnostics.
- **The trace's stage names as the status.** One vocabulary for two readers, and the reader of the
  status does not know what a front end is.
- **An estimate of the time left.** One unit can take a hundred times another, so the count done
  says little about the time left.
- **A summary of where the cost went when the build ends.** A reading of the cost files, which
  `the-compiler-reports-where-its-cost-went.md` leaves to whoever reads them. How long the command
  and each of its phases took is not such a reading: it is what the display already held when the
  command ended.

## Consequences

- A new file a command can be told to write is claimed where the others are, and a new tool is
  started where the others are; either done apart leaves something a signal does not reach.
- The error stream of `check`, `emit cpp` and `build` carries a status line once the command has run
  ten seconds, when it is not a terminal. A reader of that stream that wants only diagnostics drops
  the lines that open with a bracket.
- A build killed outright leaves its directory until the next build on the machine starts.
- Two units of one definition compiled for different parameters are shown under one name. Telling
  them apart takes saying which parameters differ, which nothing states yet.
- The C++ backend's compile says nothing of what was kept, because it reuses the whole program or
  nothing.

## Cross-references

- `a-program-is-kept-by-what-built-it.md` -- the store, whose entries are written through the same
  temporary-and-rename, and the rule that a command leaves one file.
- `the-compiler-reports-where-its-cost-went.md` -- the two files a run writes about itself, and why
  the compiler prints no account of its cost.
- `a-build-is-told-how-wide-to-run.md` -- how many of the pieces a stage counts are under way at
  once.
