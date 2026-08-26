# Testing Strategy

## Purpose

Define what a test may claim, where it lives, and who decides whether it passed. The corpus answers
one question -- does Lyra implement IEEE 1800 correctly -- so it is a conformance suite against a
published standard, and everything below follows from that.

## Owns

- The distinction between a conformance case and every other kind of test.
- What a case may state, and in what form.
- Where a case lives, and why the location is the standard's structure.
- Where a path's limitations are recorded, and how they are kept honest.

## Does Not Own

- Individual test contents.
- The harness implementation.
- Benchmark methodology.

## Core Invariants

1. **A case states what IEEE 1800 requires of a program, and nothing else.** It names no path
   through the compiler, no backend, no command, and no host tool. The same file is valid whether
   Lyra has one path or twenty, and runs unmodified under any conforming simulator.

2. **A case checks itself in SystemVerilog.** It compares in the language, calls `$fatal` on
   mismatch, and prints a fixed sentinel from a single `final` procedure once every check has
   passed. Pass or fail is read from the exit status and the presence of that sentinel; nothing else
   about the program's output is parsed. The two signals divide the failures between them: `$fatal`
   sets a nonzero exit (LRM 20.10), and a missing sentinel means the checks never ran.

3. **The expected value is written by a human and is never a value Lyra produced.** A comparison
   whose both sides pass through the same implementation proves only that the implementation agrees
   with itself.

4. **The corpus rests on the smallest subset that can express a check** -- a conditional, case
   equality, and `$fatal`. A construct the corpus depends on is a construct whose defects fail every
   case, so the foundation stays narrow and everything above it, immediate assertions included, is a
   subject the corpus tests rather than a tool it uses.

5. **A case's directory is the LRM clause it tests.** The standard's structure is the only
   enumeration of its requirements, so it is the only basis on which coverage can be measured. Two
   clauses that separately require the same behavior each get a case; those are two requirements.

6. **Each requirement has one owning case.** A case that would extend an existing case's subject
   extends it instead of joining the corpus beside it.

7. **What a path cannot do is recorded once per path, never on a case.** The record pairs a case
   with the refusal that path currently produces. A case that unexpectedly passes fails the run
   exactly as an unexpected refusal does, so the record can only shrink as a path fills in, and it
   is the coverage report.

8. **A path is the artifact it produces, not the way that artifact is run.** Execution modes over
   one emitted form share an acceptance surface and therefore one record; they cannot disagree about
   which programs are accepted.

9. **Everything whose subject is not an IEEE 1800 requirement is not a case** -- the command line,
   the emitted project's portability, the harness's own machinery. Such a test may never stand in
   for language coverage.

10. **No test asserts on the text of an HIR, MIR, or LIR dump.** A dump is a debugging view whose
    wording is free to change; what a lowering produced is proven by what the program does.

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
- A per-case tag declaring which paths run it. Absence of such a tag cannot distinguish a refusal
  from an omission.
- An expectation derived by recording a run, which freezes present behavior as the specification.
- A case that reports success without having run its checks.
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

What a case genuinely needs beyond that -- extra sources, a front-end flag, the simulated program's
argv, a required refusal -- is a `// @key: value` line at the top of the same file. Golden output,
where the output channel is the subject, is a sibling file.

Adding a SystemVerilog feature means adding or extending a case first, then implementing until it
passes.
