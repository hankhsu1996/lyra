# The compiler's own failure says what it was working on

Date: 2026-10-01 Status: accepted

## Why this decision matters

When the compiler broke one of its own invariants it said which invariant and nothing else. On a
design of a few modules that is enough. On a design of more than a thousand units one such failure
could not be located at all: the message named an index, and finding the construct behind it meant
reducing the design by hand. It also ended the run, so one run reported one failure however many
stood behind it.

## What the objective requires

`north_star.md`'s first invariant is the whole edit-compile-run-inspect loop. A failure that has to
be reduced before it can be reported makes one turn of that loop cost a day, and the cost grows with
the design. So the requirement is: **when the compiler fails through its own fault while working on
a design, its report says which part of that design it was working on.**

## Findings that shaped the design

**F1. The place that fails does not know where it is.** A failure is raised at several hundred
sites, deep in helpers holding an id or a type. What they lack is not carried to them by any
parameter, and no site can be asked to supply it.

**F2. The place that walks does.** The step that takes one unit through the pipeline knows the unit;
the lowering of a body knows the body; the lowering of a statement knows the statement, and HIR
states a source location for each.

**F3. Not every failure is one the compiler raised.** A standard-library exception is as much a
defect as a checked invariant and is constructed by code the compiler does not own.

**F4. A unit's failure is contained by the unit.** A unit's pipeline reads nothing another unit's
writes, so one of them breaking says nothing about the rest. The units also run on several threads,
and a failure that crosses to the thread that reports has left behind everything that knew where it
happened.

**F5. Saying where must cost nothing when nothing fails.** Composing a description for every body
was measured at 3.37% of an emission, against 0.88% when composed only on failure.

## How others answer it

- Clang and LLVM keep a per-thread list of scope objects, each pushed by the component that knows
  what it is doing -- the code generator pushes one naming the declaration it is generating code for
  -- and a crash handler prints the list while the objects are alive. Below the AST the entry is the
  function's name.
- Verilator passes the node at the failing site, and every node carries its file and line.

The first is F2's answer. One condition differs: here a failure is a thrown error that has to become
one unit's report while the other units go on (F4), so by the time anything prints, the scope
objects are gone. The second answer puts the fact at the site, which F1 rules out, and does not
reach the failure in F3.

## Decision

1. **Whoever walks says what it is working on, as an object on its stack.** The unit step names the
   unit; each lowering names the declaration, the statement and the expression it is translating;
   each stage below names the function. No failing site says anything.

2. **A failure collects what was said above it.** An exception leaving such an object adds what the
   object said to its thread's trail, innermost first. Nothing is composed unless that happens.

3. **What is said is a place or a name, and the report is one line.** A declaration, a statement and
   an expression are places: the report stands at the innermost one, and the rest are dropped as
   wider views of the same spot. A unit and a function are names, which the source cannot show --
   one module may be compiled as several units -- and the line carries each of them after the
   message. Nothing is attached beneath it. The request to report the bug is made once for the run,
   after the count.

4. **A unit's failure is that unit's report.** It is caught where the unit's pipeline is run, on the
   thread that ran it, and reported like any other failure of the unit; the other units are still
   attempted. A failure outside any unit is caught where the command runs, while the sources it may
   name are still held.

5. **It is a diagnostic kind of its own.** It renders through the same renderer as every report
   about the source. A run that reported one exits with the status that says the compiler failed,
   whatever else it reported.

6. **The error type carries nothing about where.** The same type is raised inside a built program,
   where there is no design being compiled, so nothing about locating a failure is on it.

## Consequences

- A broken invariant is reported at a file and a line, with the unit it was met in, and one run
  reports every unit that breaks.
- Where the recovery point sits differs from a refusal's. A refused body is reported and its
  siblings lower anyway; a body that broke an invariant ends its unit, because a refusal returns in
  order and a broken invariant says the lowering's own state may be wrong. Only the unit boundary
  shares nothing.
- Below HIR a body is named by the name it has there, because MIR and LIR state no source location.
  For a subroutine that is the name the source gave it; for a process it is a name the compiler
  made.
- A failure that is not a thrown error -- a fault in memory, an abort, a fatal error inside the code
  generator -- unwinds nothing, so it collects nothing and names no place. No real design has met
  one, and reporting it needs a signal handler reading the work still in progress, which nothing
  automatic can test.

## Rejected

- **A source location passed to every failing site.** The fact is not known there (F1), a site that
  omits it fails silently, and the failure in F3 is not reached.
- **Capturing the work in the error type's constructor.** It reaches only failures the compiler
  constructs, and puts a compiler concern on a type the runtime shares.
- **A line for every level of work the failure left.** The statement, each expression inside it and
  the declaration around it name the spot the report already stands at, so the reader is shown one
  location several times; and lines that open with a place alternate with lines that open with the
  tool's name, which reads as several reports where there is one.
- **The compiler's own call stack in place of the design's.** It says where in the compiler, which
  the message already names, and nothing about which part of the design.
- **Ending the run at the first failure.** It makes a design's count of failures a count of runs,
  which is what collecting refusals was introduced to end.
- **Recovering at the body.** It would report more per run, and rests on a property nothing states:
  that a lowering whose invariant broke left its own state fit to continue.

## Cross-references

- `../architecture/north_star.md` -- the iteration-loop objective this serves.
- `reporting-every-gap-in-one-run.md` -- the same objective for a refusal, and the recovery point it
  takes.
- `diagnostic-construction.md` -- a diagnostic's kind comes from its code.
