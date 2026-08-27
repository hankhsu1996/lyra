# A conformance case is a self-checking SystemVerilog program, indexed by LRM clause

Date: 2026-08-26. Status: accepted.

## Why this decision matters

The corpus answers one question: does Lyra implement IEEE 1800 correctly? That makes it a
conformance suite against a published standard, and a conformance suite has to settle three things
before it can be trusted -- what a case is allowed to say, where a case lives, and who decides
whether a case passed. Getting any of them wrong is expensive to undo once the corpus is large.

Each answer here is checked against what established conformance suites do, because the problem is
not new: test262 (ECMA-262), Web Platform Tests, Rust's compiletest, LLVM's test-suite, TypeScript's
baselines, and Verilator's own regression suite have all solved it.

## The axis that decides everything: one claim, many realizations

A program's meaning does not depend on which path through the compiler produced it. So a case states
what IEEE 1800 requires, and every path is held to that same statement. For a given (case, path)
pair there are exactly three outcomes:

```text
the path produces what the case claims        -> pass
the path refuses the program, with a reason   -> a recorded limitation of that path
the path produces something else              -> fail
```

The second outcome is a fact about the **path**, not about the case. That is what decides where each
piece of information lives, and it is the mistake the previous shape made: it put per-backend tags
on the case, so teaching a path a new construct meant editing every case that used it.

## The decisions

```text
D1. A case is one SystemVerilog file that checks itself. It computes, compares in SystemVerilog,
    calls $fatal on mismatch, and prints a fixed sentinel from a single `final` procedure when every
    check has passed. The harness reads the exit status and whether the sentinel appears; it parses
    nothing else. `$fatal` terminates with an error code (LRM 20.10), and a `final` procedure runs
    once at the end of simulation (LRM 9.2.3), so the two signals together separate "a check failed"
    from "the checks never ran."

D2. The expected value is written by a human, in the case, and is never a value Lyra computed. This
    is what makes the comparison an independent oracle rather than a tautology.

D3. A case names no path, no backend, and no command. It would be valid if Lyra had one path or
    twenty, and it runs unmodified under any conforming simulator.

D4. The directory is the LRM clause the case tests; the filename is the subject. The clause is the
    only enumeration of the standard's requirements that exists, so it is the only basis on which
    coverage can be measured. Two clauses that each require the same behavior each get a case; that
    is two requirements, not duplication.

D5. There is no per-case manifest file. Metadata that a case genuinely needs -- extra source files,
    front-end flags, the simulated program's argv, a required refusal -- is a `// @key: value`
    directive line at the top of the file it describes. Prose describing the claim is an ordinary
    comment and is never parsed.

D6. Golden output is a sibling file, and exists only where the output channel is itself the subject
    (LRM 21: what $display writes, how stdout and stderr divide, the order output interleaves).
    Format-string behavior is checked in-language through $sformatf, which returns the formatted text
    as a value.

D7. What a path cannot do is recorded once per path, not once per case, as a list of case paths and
    the refusal each currently produces. A case that unexpectedly passes fails the run just as an
    unexpected refusal does, so the list can only shrink as a path fills in, and it is the coverage
    report.

D8. A path is defined by the artifact it produces, not by how that artifact is executed. `jit`,
    `aot`, and `lli` share one emitted LLVM module and therefore one acceptance surface; they can
    never disagree about which programs are accepted, so they share one list.

D8a. Where a path answers wrongly is recorded too, in a second list beside the first, holding text
    the failing run produces. The case keeps every check it makes, so the day the answer becomes
    right the case passes and the run fails until the entry goes. A case is in one list or neither,
    never both. Coverage is therefore what is absent from both.

D9. A test whose subject is not an IEEE 1800 requirement is not a case. The command line, the
    emitted project's portability, and the harness's own machinery are ordinary tests outside the
    corpus.
```

## Rejected alternatives

- **A per-case manifest stating the expected value of a named variable.** Reaching a variable from
  outside the program requires the harness to reach in -- the previous shape rewrote the case's
  source to append a probe block, printed each variable through a synthesized `$display`, and
  compared that against the same value rendered by the same runtime formatter. Both sides of the
  comparison therefore ran through one piece of Lyra, so a formatting defect moved them together and
  the check passed. None of the surveyed suites states a program value in metadata; every one of
  them asserts in the language under test, because the language already has equality and the harness
  already has an exit status.

- **Golden stdout as the general mechanism.** It couples every case to LRM 21 formatting: a change
  to how an aggregate prints would fail hundreds of cases that are about aggregates, not about
  printing. Reserved for the cases where the channel is the subject.

- **The language's own `assert` as the checking construct.** An immediate assertion is the natural
  spelling, but it puts LRM 16 into the foundation every other case stands on, so a defect there
  would fail the whole corpus. The corpus rests on the smallest subset that can express a check --
  `if`, case equality, and `$fatal` -- and treats assertions as a subject it tests rather than a
  tool it uses. A helper task or macro would enlarge that foundation rather than shrink it.

- **Per-backend tags on the case.** Absence of a tag cannot distinguish "this path refuses the
  construct" from "nobody has tagged it yet", so a path's real coverage is unmeasurable, and
  teaching a path one construct edits every case that uses it.

- **A directory named for a backend.** The previous layout had one, from a time when there was only
  one backend; the second backend reused the directory and the name became false. A path name in the
  corpus tree is a claim that will expire.

- **Deriving expected output by recording a run.** It freezes present behavior, including defects,
  as the specification.

## Where D8a came from

The first cut of this decision gave a wrong answer no home at all: it failed, and stayed failing
until the case or the implementation was right. That is the stricter rule and it reads well, but
what it produced in practice was worse than what it forbade. Twenty-two cases met a diagnosed Lyra
defect during the migration, and every one of them handled it the same way -- comment the check out,
name the symptom in a `TODO`, write the gap into a progress doc. So the wrong answers were
normalized anyway, in the one form nothing watches: a commented-out check does not run, and fixing
the defect produces no signal at all. Prose in a progress doc was the only thing left remembering.

D8a takes the property that makes the refusal record work -- an entry that becomes false fails the
run -- and gives it to the case a path gets wrong. It costs the strictness of "no home", and it buys
the thing the strict rule was for: a defect that cannot be quietly forgotten.

## Consequences

- The harness decides pass or fail from an exit status and one fixed string. Source rewriting, the
  probe-marker protocol, the expected-value renderer, and the SV-literal parser all leave with the
  mechanism they served.
- A case is executable by any conforming simulator with no harness at all, which is what makes a
  second simulator usable as an oracle.
- Coverage becomes a question with an answer: which clauses hold cases, and which cases each path
  refuses.
- A case that checks nothing cannot pass silently, because the sentinel is printed after the checks
  in the same `final` procedure.

## Cross-references

- `../architecture/testing_strategy.md` -- the contract form of this decision.
- `../../CLAUDE.md` -- the error-handling policy that separates a path's refusal from a compiler
  bug, which is what lets a refusal be recorded rather than investigated.
