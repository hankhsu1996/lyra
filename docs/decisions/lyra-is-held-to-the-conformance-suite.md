# Lyra is held to the conformance suite, and records only what it answers

Date: 2026-10-29. Status: accepted. Takes over the decisions of the suite's case-shape record that
were Lyra's (its D7, D8, D8a, D11 and D12) when the suite became a sub-project of its own; the rest
of that record is `../../conformance/docs/decisions/case-shape.md`.

## Why this decision matters

The conformance suite (`../../conformance/`) decides whether a run conforms to IEEE 1800, the same
for every tool, and holds no record of what any tool answers. Lyra still needs to know when one of
its own answers moves -- a refusal that is now carried out, a wrong answer that is now right, a case
that starts failing -- and to have that fail a build rather than be noticed by reading. The records
here do that, and only that.

## The rule

The suite decides whether a run conforms. Lyra's records decide only whether a run matches what Lyra
is known to answer. No record changes a verdict, and no case states what Lyra answers.

## The decisions

```text
D1. What a path cannot do is recorded once per path, not once per case, as a list of case ids and
    the refusal each currently produces (`tests/paths/<path>.yaml`). A case that unexpectedly
    passes fails the run just as an unexpected refusal does, so the list can only shrink as a path
    fills in, and it is the coverage report.

D2. A path is defined by the artifact it produces, not by how that artifact is executed. Every way
    of running one path's artifact shares that artifact and therefore one acceptance surface; they
    can never disagree about which programs are accepted, so they share one list.

D3. Where a path answers wrongly is recorded too, in a second list beside the first, holding text
    the failing run produces (`<path>.defects.yaml`, and `<path>.address.defects.yaml` for what only
    a run under the address sanitizer sees). The case keeps every check it makes, so the day the
    answer becomes right the case passes and the run fails until the entry goes. A case is in one
    list or none. Coverage is therefore what is absent from all of them.

D4. A record names a case by its id, its path under the suite's `cases/`. An entry naming an id
    that matches no case fails the run, so a case that moves cannot leave an entry silently stale.

D5. What Lyra chose where the standard leaves the answer to the tool, and Lyra's reading of a text
    two clauses read differently, are held in cases of the suite's shape that only Lyra runs
    (`tests/choices/`). They are regressions of Lyra's, never statements of the standard.

D6. A case Lyra cannot run without harm is named in `tests/paths/parked.yaml` with why, and is held
    only to elaborating. It stays as it is in the suite, so every other tool runs it.

D7. Lyra's checks that read the corpus as a set of designs -- each case compiled twice to the same
    answer, no static initializer reading another's value -- stay in Lyra. They ask about Lyra's
    compiler or need its front end, and none is a verdict on a tool.
```

## Where D3 came from

The first cut gave a wrong answer no home at all: it failed, and stayed failing until the case or
the implementation was right. That is the stricter rule and it reads well, but what it produced in
practice was worse than what it forbade. Twenty-two cases met a diagnosed Lyra defect during the
migration, and every one of them handled it the same way -- comment the check out, name the symptom
in a `TODO`, write the gap into a progress doc. So the wrong answers were normalized anyway, in the
one form nothing watches: a commented-out check does not run, and fixing the defect produces no
signal at all. Prose in a progress doc was the only thing left remembering.

D3 takes the property that makes the refusal record work -- an entry that becomes false fails the
run -- and gives it to the case a path gets wrong. It costs the strictness of "no home", and it buys
the thing the strict rule was for: a defect that cannot be quietly forgotten.

## How established suites are consumed

Recalled rather than read. Engines consuming test262 keep what they currently fail in their own tree
(V8's `test262.status`), and browsers consuming Web Platform Tests keep their expectations in theirs
(Chromium's `TestExpectations`); neither suite holds an expectation of any engine. Lyra takes that
shape, with the suite in the same repository.

## Consequences

- A refusal is recorded rather than investigated only because Lyra separates a path's refusal from a
  compiler bug (`../../CLAUDE.md`, error handling); a run reporting a compiler bug fails whatever
  the records say.
- A change that moves or renames a case renames its entries in the same change.

## Cross-references

- `../../tests/paths/README.md` -- the records and how they are read.
- `../../tests/choices/README.md` -- the cases holding Lyra's own choices.
- `../../conformance/docs/decisions/case-shape.md` -- what a case is.
