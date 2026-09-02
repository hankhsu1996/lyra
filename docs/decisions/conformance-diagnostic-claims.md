# A requirement whose observable is a report is stated as a directive, in both directions

Date: 2026-09-01. Status: accepted.

## Why this decision matters

A conformance case checks itself in SystemVerilog, and the harness reads an exit status and one
sentinel. That shape is what makes a case portable and its oracle independent, and it is settled in
[conformance-case-shape](conformance-case-shape.md). It also has a boundary: IEEE 1800 states
requirements whose entire content is that a tool issues -- or does not issue -- a message, and a
program cannot read a message about itself. Such a requirement moves neither signal the harness
reads, so a case for it passes whatever the implementation does.

The boundary is not hypothetical and it is not narrow. LRM 12.4.2 and 12.5.3 require a violation
report when a `unique` or `priority` statement matches nothing, and forbid one when an explicit
catch-all covers the rest. Lyra had that requirement wrong in four of its twelve forms for as long
as the forms existed, with a case for each statement kind passing the whole time. What surfaced it
was a production design flooding its own output, which is the most expensive instrument available
and the only one that was looking.

So the question this settles is not whether such requirements can be tested -- it is which ones the
corpus is structurally silent about, so that they are routed somewhere that can see them rather than
assumed covered.

## The axis that decides everything: a claim about a report has two directions

An implementation that never reports anything satisfies every "no report is issued" claim, and one
that reports on everything satisfies every "a report is issued" claim. Either direction alone is a
checker whose passing state is its default. The two are not alternatives to pick between; a
requirement of this kind is only covered when both are stated, by two cases that differ in the one
thing the standard says decides it.

## The decisions

```text
E1. A case may state what the run reports, as a directive. `@reports:` gives text the run's
    diagnostics must contain; `@reports-nothing:` requires that it produce none at all. A case
    carrying neither is held to nothing about its diagnostics, which is what every case is held to
    today.

E2. Both directions are written, as separate cases, whenever a requirement of this kind is covered.
    The pair is the unit of coverage; either half alone passes against an implementation that is
    uniformly silent or uniformly noisy.

E3. `@reports-nothing:` is the run producing no diagnostic output whatsoever, not the absence of one
    particular message. A conforming tool has nothing to say about a correct program, so the claim
    is stated as that and needs no text -- which is what keeps it from pinning a wording, and what
    makes it fail when some unrelated report appears where the standard allows none.

E4. A case still checks itself. A diagnostic directive is stated beside the checks, never instead of
    them: the program's values are claimed in SystemVerilog as always, and the directive claims the
    one thing the language cannot observe.

E5. The text `@reports:` names is the shortest phrase that identifies the requirement, drawn from
    the standard's own vocabulary where the standard has one. It is the one place a case is not
    portable across tools, and it is kept small for that reason.
```

## What this does not become

The directive states a claim about a report; it is not a golden-output mechanism and does not grow
into one. `conformance-case-shape` D6 reserves recorded output for the cases whose subject is the
output channel itself -- how stdout and stderr divide, and the order output interleaves -- and that
stays the only place a case is compared against text the tool wrote. The difference is what is being
claimed: a golden file claims a tool's whole output is this, and a diagnostic directive claims the
standard required a report and one is there.

## Rejected alternatives

- **Golden stderr for these cases.** It reaches further than the claim: any rewording of any message
  in the run breaks cases whose subject is a statement qualifier. The wording is not the
  requirement, and a mechanism that pins it makes every message change a corpus edit.

- **Checking the requirement in SystemVerilog.** There is nothing to check against. LRM 12.4.2.1
  lets assertion control tasks turn violation reporting on and off, and nothing in the standard lets
  a program observe that a report was issued. The check would have to invent an observable the
  language does not have, which is what makes this the boundary rather than an oversight.

- **A per-message directive on both sides, `@reports:` paired with `@reports-not: <text>`.** It is
  the symmetric-looking shape and it is weaker: an implementation that stops issuing the report
  entirely satisfies it, and a rewording satisfies it too. Requiring silence outright is stronger
  and states what the standard actually allows.

- **Leaving these requirements uncovered and noting the gap.** What was in place already, in the
  form of prose. The gap was written down accurately and read by nobody in a position to act on it,
  and the requirement stayed wrong for as long as it took a real design to run.

- **Recording the wrong answers in the defect record instead.** The defect record holds text a
  failing run produces, and these cases do not fail: they pass, with a wrong diagnostic beside a
  correct answer. Nothing distinguishes them from a covered case, which is the property this
  decision is for.

## Consequences

- The harness reads one more thing about a run, and only for a case that asks. The pass rule for
  every other case is unchanged: an exit status and the sentinel.
- A requirement of this kind now has a home, so the answer to "is this covered" stops being "there
  is no way to say".
- `@reports-nothing:` makes an unrelated diagnostic fail the case that carries it. That is
  deliberate: the cases carrying it are legal programs a conforming tool has no comment on.

## Cross-references

- [conformance-case-shape](conformance-case-shape.md) -- the shape this extends, and the reasoning
  behind the self-checking rule it does not replace.
- [qualified-statement-violation-check](qualified-statement-violation-check.md) -- the requirement
  that forced this, and what the corpus could not see about it.
- `../../tests/conformance/README.md` -- the contract cases are written against.
