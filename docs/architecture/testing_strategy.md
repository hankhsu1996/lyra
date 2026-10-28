# Testing Strategy

## Purpose

Define what a test may claim, where it lives, and who decides whether it passed. The corpus answers
one question -- does a tool implement IEEE 1800 correctly -- so it is a conformance suite against a
published standard that Lyra is held to like any other tool, and everything below follows from that.

## Owns

- The distinction between a conformance case and every other kind of test.
- What a case may state, and in what form.
- Where a case lives, and why the location is the standard's structure.
- Where a path's limitations and its defects are recorded, and how they are kept honest.
- What a design written elsewhere, and its own check, may be taken to show.

## Does Not Own

- Individual test contents.
- The harness implementation.
- Benchmark methodology.

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

7. **What a path does other than run a case correctly is recorded once per path, never on a case.**
   Two records: what the path refuses, and where it answers wrongly. Each pairs a case with text the
   run it produces has to contain, and a case that stops producing it fails the run, so a record can
   only shrink as a path fills in. A case sits in one record or neither, so the path's coverage is
   what is absent from both.

   A defect belongs in the second record rather than in the case, commented out. A check that does
   not run cannot report that the behaviour became right, which leaves the defect resting on someone
   remembering it.

8. **A path is the artifact it produces, not the way that artifact is run.** Execution modes over
   one emitted form share an acceptance surface and therefore one record; they cannot disagree about
   which programs are accepted.

9. **Everything whose subject is not an IEEE 1800 requirement is not a case** -- the command line,
   the emitted project's portability, the harness's own machinery. Such a test may never stand in
   for language coverage.

10. **No test asserts on the text of an HIR, MIR, or LIR dump.** A dump is a debugging view whose
    wording is free to change; what a lowering produced is proven by what the program does.

11. **A property no program can observe is stated by a test that reads what the compiler built.**
    How many artifacts a design compiles into is the standing example: a structure compiled once and
    the same structure compiled once per repetition run identically, so nothing the corpus can write
    separates them and the standard requires neither. Such a property still has to be held to,
    because which of the two a design gets decides whether a large one compiles at all -- so it is
    stated by reading the lowered form through the interface the command line uses, never through a
    dump, and asserted in both directions: what must be shared and what must not. An assertion that
    only ever holds one way is satisfied by a compiler that shares nothing, which is the failure it
    exists to catch. This is not language coverage and never stands in for any.

12. **Every body of tests asks one question of a set of designs that each state one thing, and what
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

13. **What a design written elsewhere checks of itself is evidence, never the oracle.** Its
    testbench and its check were validated on the simulator its authors ran. A check that passes
    says a second tool agreed with that one; a check that fails says only that the two differ, and
    which of them is right is read off the standard, where a case of the corpus states the clause. A
    testbench that asks for something the standard does not grant is corrected in the suite it came
    from, and that same correction is applied here until the suite holds it, so the design goes on
    being measured on the work it was written to do.

14. **A run is read by a reader for its tool and judged once for every tool.** The reader asks its
    tool for what the case needs and reads what it wrote; the judgement gives conforms, does not
    conform, unresolved where the reading cannot decide, or not run where the case never reached the
    tool. A failure of the harness is never read as an answer of the tool.

## Boundary to Adjacent Layers

- A case exercises the whole pipeline, so a failure in one may point at any layer. Bisecting it is
  done by reading the dumps, never by pinning them in another test.
- A refusal recorded against a path is a refusal the compiler stated. A crash, or an invariant
  violation reported as a compiler bug, is not a limitation and may not be recorded as one; the
  error-handling policy in `CLAUDE.md` is what keeps the two distinguishable.

## Forbidden Shapes

- A manifest that states the expected value of a variable inside the program. Reaching a value from
  outside means reaching into the program, and the mechanisms for that -- rewriting the source,
  synthesizing a probe -- put the implementation on both sides of the comparison.
- Golden output used as the general mechanism. It couples every case to output formatting; reserve
  it for cases whose subject is the output channel itself.
- A path, backend, or execution mode named anywhere in the corpus tree or in a case.
- A tool's option, a header a tool generates, or a tool's message wording named by a case.
- A per-case tag declaring which paths run it. Absence of such a tag cannot distinguish a refusal
  from an omission.
- An expectation derived by recording a run, which freezes present behavior as the specification.
- A case that reports success without having run its checks.
- A check whose answer depends on the machine running it -- a duration, a peak memory, a ratio of
  two -- held as pass or fail. One tree would pass on one run and fail on the next, and a red gate
  would no longer say the tree is wrong.
- A check commented out to hold a known defect. It stops running, so it cannot report that the
  behavior became right, and the defect survives only as long as someone remembers it.
- A C++ test that constructs compiler objects and asserts on their fields to establish a
  SystemVerilog behavior.
- Relying on a file-path pattern as the primary classification between kinds of test. What a test
  answers decides what it is.

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
where the output channel is the subject, is a sibling file. What Lyra chose where the standard
leaves the answer open is held by cases of the same shape in `tests/choices/`, which only Lyra runs.

Adding a SystemVerilog feature means adding or extending a case first, then implementing until it
passes.

The bodies of tests that keep a record, each with the question it asks:

| Question                                     | Designs               | Record, per path                                                    |
| -------------------------------------------- | --------------------- | ------------------------------------------------------------------- |
| What does a program do?                      | `tests/conformance/`  | `<path>.yaml`, `<path>.defects.yaml`, `<path>.address.defects.yaml` |
| What does compiling cost as a design grows?  | `tests/growth/`       | `<path>.growth.yaml`                                                |
| Which units does an edit make again?         | `tests/incremental/`  | `<path>.incremental.yaml`                                           |
| How far does a design written elsewhere get? | the benchmark suite's | `<path>.external_designs.yaml`                                      |

The second and third are the same kind of question along two axes: a cost no program can observe and
no single compile shows, measured by compiling twice and holding one run to the other.
