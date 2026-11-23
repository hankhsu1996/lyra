# Testing Strategy

## Purpose

Define what a test may claim, where it lives, and who decides whether it passed. The corpus answers
one question -- does a tool implement IEEE 1800 correctly -- so it is a conformance suite against a
published standard that Lyra is held to like any other tool, and everything below follows from that.
The suite decides whether a run conforms; what Lyra keeps of its own answers decides only whether a
run matches what Lyra is known to answer. A benchmark is held to the same discipline from the other
side: it measures a cost, never a verdict, and its numbers are worth having only where one taken
today reads against one taken a year ago.

## Owns

- The distinction between a conformance case and every other kind of test.
- What a case may state, and in what form.
- Where a case lives, and why the location is the standard's structure.
- What Lyra records of its own answers -- what a path refuses, where it answers wrongly, what only a
  checked run sees, what it cannot run, and what it chose -- and how those records are kept honest.
- What a design written elsewhere, and its own check, may be taken to show.
- What a benchmark case fixes, what it lets the harness choose, and what it reports.

## Does Not Own

- Individual test contents.
- The harness implementation.
- What a case is, as the conformance suite states it for every tool (`conformance/README.md`).

## Core Invariants

1. **A case states what IEEE 1800 requires of a program, and nothing else.** It names no path
   through the compiler, no backend, no command, no option, no generated file, and no wording of a
   tool's message. The same file is valid whether Lyra has one path or twenty, and runs unmodified
   under any conforming simulator.

2. **A case checks itself in SystemVerilog.** It compares in the language, calls `$fatal` on
   mismatch, and prints a fixed sentinel once every check has passed. Where its requirement is a
   report, that report is the one thing it claims beside the checks, by kind and by what the
   standard fixes of it, and where the standard lets a tool end the run at it the case says it is
   ready for the report before it, with a marker of its own. Nothing else about the program's output
   is read.

3. **The expected value is written by a human and is never a value a tool produced, and every answer
   the standard allows passes.** A comparison whose both sides pass through the same implementation
   proves only that the implementation agrees with itself, and a case that pins one of several legal
   answers tests a choice, which lives apart from the corpus.

4. **The corpus rests on the smallest subset that can express a check** -- a conditional, case
   equality, and `$fatal`. A construct the corpus depends on is a construct whose defects fail every
   case, so the foundation stays narrow and everything above it, immediate assertions included, is a
   subject the corpus tests rather than a tool it uses.

5. **A case's directory is the LRM clause it tests.** The standard's structure is the only
   enumeration of its requirements, so it is the only basis on which coverage can be measured. Two
   clauses that separately require the same behavior each get a case; those are two requirements.

6. **Each requirement has one owning case.** A case that would extend an existing case's subject
   extends it instead of joining the corpus beside it.

7. **No record of Lyra's changes a verdict, and no case states what Lyra answers.** The suite holds
   no expectation of any tool, as test262 and the Web Platform Tests hold none of an engine's; Lyra
   keeps what it answers in its own tree, and a run off that baseline fails whichever way it moved,
   so an answer that changes is noticed by the build rather than by reading.

8. **What a path does other than run a case correctly is recorded once per path, never on a case.**
   What the path refuses is one record, where it answers wrongly a second, and what only a run whose
   programs check their memory sees a third, read only by that run since the same case passes in
   every other. Each pairs a case with text the run it produces has to contain, and a case that
   stops producing it fails the run, so a record can only shrink as a path fills in. A case sits in
   at most one record, since refused and wrong are different outcomes, so the path's coverage is
   what is absent from all of them. A record names a case by its id, its path under the suite's
   cases, and an entry naming no case fails the run, so a case that moves cannot leave an entry
   silently stale.

   A defect belongs in a record rather than in the case, commented out. A check that does not run
   cannot report that the behaviour became right, which leaves the defect resting on someone
   remembering it. What goes in is a defect someone has diagnosed, never a failure that is merely
   inconvenient.

9. **A path is the artifact it produces, not the way that artifact is run.** Execution modes over
   one emitted form share an acceptance surface and therefore one record; they cannot disagree about
   which programs are accepted.

10. **A case Lyra cannot run without harm is named, with why, and held only to elaborating.** One
    that never ends, or takes the machine's memory with it, stays as it is in the suite, so every
    other tool runs it. It is the last resort, because a parked case is the one shape nothing
    watches.

11. **What Lyra chose where the standard leaves the answer to the tool, and its reading of a text
    two clauses read differently, are held by cases of the suite's shape that only Lyra runs.** They
    are regressions of Lyra's, never statements of the standard, and where two clauses disagree the
    conformance case beside the choice keeps what both readings require
    ([language_conformance.md](language_conformance.md)).

12. **Lyra's checks that read the corpus as a set of designs stay in Lyra.** Each case compiled
    twice to the same answer, no static initializer reading another's value: these ask about Lyra's
    compiler or need its front end, and none is a verdict on a tool.

13. **Where Lyra's own build is instrumented, the corpus builds every program it runs to check its
    memory.** A read of storage that has ended usually reads what was there, so only the program
    built to check every load and store sees it; one scheduled run then answers whether anything
    reads ended storage, in the compiler or in what it built. A report fails the case that made it
    through the exit status, which is how a case passes or fails at all.

14. **Everything whose subject is not an IEEE 1800 requirement is not a case** -- the command line,
    the emitted project's portability, the harness's own machinery. Such a test may never stand in
    for language coverage.

15. **No test asserts on the text of an HIR, MIR, or LIR dump.** A dump is a debugging view whose
    wording is free to change; what a lowering produced is proven by what the program does.

16. **A property no program can observe is stated by a test that reads what the compiler built.**
    How many artifacts a design compiles into is the standing example: a structure compiled once and
    the same structure compiled once per repetition run identically, so nothing the corpus can write
    separates them and the standard requires neither. Such a property still has to be held to,
    because which of the two a design gets decides whether a large one compiles at all -- so it is
    stated by reading the lowered form through the interface the command line uses, never through a
    dump, and asserted in both directions: what must be shared and what must not. An assertion that
    only ever holds one way is satisfied by a compiler that shares nothing, which is the failure it
    exists to catch. This is not language coverage and never stands in for any.

17. **Every body of tests asks one question of a set of designs that each state one thing, and what
    a path falls short of on that question is a record held exactly.** The corpus asks what a
    program does; other bodies ask what compiling a design costs as it grows, which units an edit
    makes again, how far a design written elsewhere gets. Whatever the question, a subject is held
    to exactly what its path's record lists: a shortfall not listed fails the run, and so does one
    listed that has stopped, until its line goes. So a record only shrinks, and what is left in it
    is what that path still owes on that question. A new question is a new body in this shape, never
    a second way of excusing a failure -- a skipped test, a tolerance widened to pass, an
    expectation edited to match.

    A record holds only what one tree answers the same on every run. A time or a peak memory is read
    off the machine as much as off the tree, so no body of tests holds one.

18. **What a design written elsewhere checks of itself is evidence, never the oracle.** Its
    testbench and its check were validated on the simulator its authors ran. A check that passes
    says a second tool agreed with that one; a check that fails says only that the two differ, and
    which of them is right is read off the standard, where a case of the corpus states the clause. A
    testbench that asks for something the standard does not grant is corrected in the suite it came
    from, and that same correction is applied here until the suite holds it, so the design goes on
    being measured on the work it was written to do.

19. **A run is read by a reader for its tool and judged once for every tool.** The reader asks its
    tool for what the case needs and reads what it wrote; the judgement gives conforms, does not
    conform, unresolved where the reading cannot decide, or not run where the case never reached the
    tool. A failure of the harness is never read as an answer of the tool.

## Benchmarks

A benchmark's numbers are worth having only if one taken today can be read against one taken a year
ago, and the tool under measurement is expected to get faster by orders of magnitude: a workload
sized to take a second today takes two milliseconds on an engine a few hundred times quicker, which
is process startup and nothing else. No established suite settles this by writing a number down.
Go's `testing.B` and Google Benchmark raise the iteration count until a run reaches a target
duration and report the cost of one operation, and everything below takes that shape. A benchmark is
not a test: a timing needs a machine running nothing else, and a build system runs its tests in
parallel and caches their results.

**Every case carries two quantities that behave oppositely.** The shape of the work -- an array's
length, a process count, a pipeline's depth -- is what the case measures and its identity, fixed in
the source, never a parameter and never adjusted to fit a budget: shrinking an array to make a suite
quick shrinks whatever its length was costing, frequently the thing the case exists to show. The
amount of the work -- iterations, cycles -- only decides how long the measurement takes, and is
divided out.

- **A simulation's amount is a run-time argument and a build's is its design.** A case timing a
  simulation reads its amount from a plusarg (LRM 21.6), so one build serves every amount, and
  consumes its own result observably, since an optimized loop whose result nothing reads is free to
  be deleted. A case timing a build is given its amount as a top-level parameter (LRM 23.10), each
  probe a fresh build where nothing has built, compiled the way an edit loop compiles. A case still
  carries a default amount, which is what it runs at by hand; nothing measured reads it.
- **The harness chooses the amount**, measuring probes and raising the amount from what they cost
  until one reaches a target duration, never far past the largest amount already measured, so an
  unexpected growth costs at most a step. No amount the measurement uses is written down, so none
  goes stale, and an engine a thousand times faster is followed without editing a file.
- **The reported quantity is work per second, with what a measurement costs before any of the work
  taken out.** A time means nothing without the amount beside it, and a rate is the same number
  whatever the amount wherever the cost is proportional to it. Two probes at least separate the
  fixed part; a case too slow to reach the target at one unit takes its two readings at one and two
  units, and a rate charged a fixed part it could not separate is a lower bound. How the cost grew
  with the amount is reported beside the rate, as the power read off two probes at least a doubling
  apart, since a rate is one number only where that power is one. Where the language bounds how much
  work a case can hold -- the width of a packed type -- the case states the bound in a directive and
  no probe goes past it.
- **A case can be counted rather than timed**: the instructions one unit of its work costs Lyra,
  under callgrind, read off counts at an amount and twice it and differenced. A count does not move
  with the load on the machine that took it, so it is what tells two revisions of Lyra apart, while
  it moves from one machine to another, so it is compared only with one taken beside it and what is
  read across days is the rate. Two compilers being compared are counted at the same amounts, the
  second taken from the first one's result file; a build is counted at one design size and twice it,
  the same for every case and backend. The lower amount is counted twice and what the two disagree
  by, as a share of the work counted, is the case's resolution; a case whose resolution is too
  coarse to tell a change worth marking from its own variation fails, with what it measured.
- **A reference simulator is measured in the same run, on the same machine, scaled to the same
  target independently**, and compared as a ratio of rates, which survives a slow, loaded or
  different machine; a count is Lyra's alone, since a reference simulator's would count another
  engine whole. A case may also hold the program a careful C++ programmer would write for its work
  -- keeping every guarantee the language gives what it uses, compiled at `-O2` by the compiler
  Lyra's builds take when none is named, and held to printing what Lyra's program prints at one
  small amount -- so whether Lyra reaches the floor its work allows is read at any revision.
- **What a case is, the corpus says in its own files.** A case is a directory of SystemVerilog
  entered through one file, on the conformance corpus's terms: what the source cannot state is a
  `// @key: value` directive at its top, never a manifest beside it, and the outer directory is the
  cost family the case isolates, so a regression is placed in a family.
- **A reference simulator not installed is said so and fails nothing; every other measurement a tool
  cannot make fails the run**, a build Lyra refuses included, since a case exists to give Lyra a
  rate for its family; the run goes on to every other case and fails after its report. The scheduled
  run gates on a case building and running, never on a duration, which is a claim about the machine.
  Comparing two revisions is a separate program reading two result files; the runner knows nothing
  of what "before" was.

## Boundary to Adjacent Layers

- A case exercises the whole pipeline, so a failure in one may point at any layer. Bisecting it is
  done by reading the dumps, never by pinning them in another test.
- A refusal recorded against a path is a refusal the compiler stated. A crash, or an invariant
  violation reported as a compiler bug, is not a limitation and may not be recorded as one, and a
  run that reports one fails whatever the records say; the error-handling policy in `CLAUDE.md` is
  what keeps the two distinguishable. Every case is lowered with Lyra's verification of its middle
  form after each rewrite, and one that fails it fails as a compiler bug.
- A cost a design's build leaves that has to stay flat as the design grows is a question of growth
  against a bound, which the growth tests ask; it is not a benchmark, since a relative change of a
  quantity near zero says nothing at any amount.

## Forbidden Shapes

- A manifest that states the expected value of a variable inside the program. It fails invariant 3:
  reaching a value from outside means reaching into the program, and the mechanisms for that --
  rewriting the source, synthesizing a probe -- put the implementation on both sides of the
  comparison.
- Golden output used as the general mechanism. It fails invariant 2 by coupling every case to output
  formatting; it is kept for cases whose subject is the output channel itself.
- A path, backend, or execution mode named anywhere in the corpus tree or in a case. It fails
  invariant 1.
- A tool's option, a header a tool generates, or a tool's message wording named by a case. It fails
  invariant 1.
- A per-case tag declaring which paths run it. It fails invariant 8: a tag's absence cannot tell a
  refusal from an omission.
- An expectation derived by recording a run. It fails invariant 3 by freezing present behavior as
  the specification.
- A case that reports success without having run its checks. It fails invariant 2.
- A case recorded in two records, or a record that can grow without failing a run. Each fails
  invariant 8: a case recorded twice is held to neither outcome, and a record that grows stops being
  the coverage report.
- A check whose answer depends on the machine running it -- a duration, a peak memory, a ratio of
  two -- held as pass or fail. It fails invariant 17: one tree would pass on one run and fail on the
  next, and a red gate would no longer say the tree is wrong.
- A check commented out to hold a known defect. It fails invariant 8: it stops running, so it cannot
  report that the behavior became right, and the defect survives only as long as someone remembers
  it.
- A C++ test that constructs compiler objects and asserts on their fields to establish a
  SystemVerilog behavior. It fails invariant 14: what a SystemVerilog behavior is, the program
  shows.
- A file-path pattern as the primary classification between kinds of test. What a test answers
  decides what it is.
- A benchmark case whose amount of work is written down, or whose shape is a parameter. The first
  fails reading a number across an engine that changes by orders of magnitude, since no one amount
  serves both ends; the second fails comparability, since per-unit cost is a function of the shape.
- Elapsed time reported as a benchmark's number. It ties every number to the amount it was taken at,
  so the amount can never change without invalidating the history.
- A benchmark run as Bazel tests. It fails measuring on a machine running nothing else, since a
  build system shards its tests concurrently and returns a cached duration from an earlier state.
- A benchmark run that passes over a construct Lyra refuses. It fails giving Lyra a rate for every
  cost family: a case that built last month and is refused today is a regression a green run would
  not show.
- Regressions detected inside the benchmark runner. Comparison needs a baseline and, for a timing, a
  statistical model, and a runner owning both cannot answer a one-off question.

## Notes / Examples

A case is one file. Prose states the claim and cites the clause; the checks and the sentinel sit in
one `final` procedure:

```systemverilog
// A conditional generate block whose condition holds is elaborated, and the
// initial procedure it contains runs (LRM 27.5).
module Top;
  bit ran;
  if (1) begin : g
    initial ran = 1;
  end
  final begin
    if (!ran) $fatal(1, "the generate block's initial procedure did not run");
    $display("All checks passed");
  end
endmodule
```

What a case genuinely needs beyond that -- the instances to elaborate, the simulated program's argv,
the compilation-unit use model its files are compiled under, a required refusal or report -- is a
`// @key: value` line at the top of the same file; extra sources are files beside it. Golden output,
where the output channel is the subject, is a sibling file.

Adding a SystemVerilog feature means adding or extending a case first, then implementing until it
passes.

Current implementation: Lyra's records of its answers are `tests/paths/` (`<path>.yaml`,
`<path>.defects.yaml`, `<path>.address.defects.yaml`, `parked.yaml`), its choices are
`tests/choices/`, the benchmark corpus is `tests/benchmark/`, and its runner and result schema are
`tools/bench/`.

The bodies of tests that keep a record, each with the question it asks:

| Question                                     | Designs               | Record, per path                                                    |
| -------------------------------------------- | --------------------- | ------------------------------------------------------------------- |
| What does a program do?                      | `conformance/cases/`  | `<path>.yaml`, `<path>.defects.yaml`, `<path>.address.defects.yaml` |
| What does compiling cost as a design grows?  | `tests/growth/`       | `<path>.growth.yaml`                                                |
| Which units does an edit make again?         | `tests/incremental/`  | `<path>.incremental.yaml`                                           |
| How far does a design written elsewhere get? | the benchmark suite's | `<path>.external_designs.yaml`                                      |

The second and third are the same kind of question along two axes: a cost no program can observe and
no single compile shows, measured by compiling twice and holding one run to the other.
