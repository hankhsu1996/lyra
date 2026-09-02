# A qualifier states two assertions, and a catch-all discharges one of them

Date: 2026-09-01. Status: accepted.

## Why this decision matters

`unique`, `unique0`, and `priority` are the only SystemVerilog constructs whose entire observable
effect is a message. They change no value and select no different statement, so an implementation
can have their semantics exactly backwards while every program still computes the right answer. That
makes the rule they encode easy to state loosely and expensive to get wrong: a tool that reports
where the standard forbids it floods a real design's output with reports about statements that are
correct, and a reader who learns the reports are noise stops reading the ones that are not.

The rule also decides how the statement it qualifies is lowered, which is the part that is not
obvious. A check that needs every arm's predicate forces the statement to evaluate every arm; a
check that needs none leaves the statement free to stop at the first match. So the same paragraph of
the standard settles both what is reported and what is evaluated.

## The axis that decides everything: two assertions, not one qualifier

A qualified statement asserts two independent things about its arms.

- **Uniqueness** -- at most one condition is true, so it is safe to evaluate them in parallel and in
  any order.
- **Totality** -- at least one condition is true, so no execution falls past the whole statement
  unhandled.

Each qualifier selects a subset, and the subset is the whole of what the qualifier means:

| Qualifier  | Uniqueness | Totality |
| ---------- | ---------- | -------- |
| `unique`   | asserted   | asserted |
| `unique0`  | asserted   | --       |
| `priority` | --         | asserted |

Reading the three qualifiers as three checks hides that they are two checks in three combinations,
and a check written as one comparison against the number of arms that matched -- "exactly one" --
carries both assertions in a single value, where neither can be discharged without the other.

## The decisions

```text
D1. A qualifier is lowered as the set of assertions it states, never as a qualifier kind. Each
    assertion is decided independently, produces its own report, and is emitted only when it is
    live.

D2. An explicit catch-all discharges totality. LRM 12.4.2 states it for the `else` of an if-else-if
    chain outright; the `default` of a case statement is the same construct and is read the same
    way (see below). Uniqueness is untouched by a catch-all: two arms that both match are still two
    arms that both match.

D3. Totality is the statement reaching its fall-through arm, not a count of arms that matched. A
    statement asserting totality and nothing else therefore needs no arm predicate, no snapshot,
    and no captured environment: arriving where nothing matched is the violation.

D4. What the live assertions need decides what the statement evaluates. Uniqueness needs every
    arm's predicate, and LRM 12.4.2 and 12.5.3 require evaluating every arm for `unique` and
    `unique0` regardless. Totality needs only the fall-through. A statement with no live assertion
    is lowered exactly as the unqualified statement is, so `priority` with a catch-all costs
    nothing and stops at the first match, as LRM 12.5's linear search and 12.4.2's "evaluated in
    the order listed" both describe.

D5. A violation report names the assertion that failed in the standard's own vocabulary --
    "violation", against "condition" for an if and "case item" for a case -- and never a count that
    two different failures could both produce.
```

## Reading a `default` as discharging totality

LRM 12.4.2 carves the `else` out in its own words: a violation report "shall be issued if no
condition matches **unless there is an explicit `else`**", and the clause's worked `priority if`
example annotates its final `else` with "covers all other possible values, so no violation report".
Nothing is left to decide there.

LRM 12.5.3 has no matching sentence. It says a violation report is issued "if no `case_item`
matches", and 12.5 says of the linear search that a `default` item "is ignored during this linear
search" -- which read alone suggests a `default` does not match and so does not suppress. Three
things settle it the other way.

- **`default` is a `case_item`.** Syntax 12-3 gives `case_item` exactly two forms, an expression
  list and `default`. A statement carrying one always selects a `case_item`, so "no `case_item`
  matches" describes no reachable state. Being ignored during the search is how the default is
  selected, not evidence that it is not.
- **12.5.3's own note.** "By specifying `unique` or `priority`, it is not necessary to code a
  `default` case to trap unexpected case values." The note offers the report and the `default` as
  two ways to handle the same thing. It is advice only if writing the `default` settles the matter;
  under the other reading a design that takes the advice is warned at anyway.
- **The two clauses state one requirement for two spellings, and the standard says so.** An
  if-else-if chain's `else` and a case statement's `default` are the same construct -- the arm that
  runs when nothing else did -- and 12.5.3.1 makes the correspondence explicit, sending the case
  form's violation-report mechanics to 12.4.2.1 as "identical". Reading 12.4.2 and 12.5.3 as
  requiring opposite behaviour would mean rewriting a statement from one form into the other changes
  what a conforming tool reports, against a clause that has just said the two are handled the same
  way.

The reading is also what an independent implementation does: Verilator 5.045 under `--assert`
reports `unique case` with no `default` and is silent once one is added, in both directions.

## Rejected alternatives

- **One comparison against the matched-arm count.** The shape this decision replaces: `unique` is
  violated when the count is not one, `unique0` when it exceeds one, `priority` when it is zero. It
  is compact and it is why the defect existed -- "not one" is two assertions folded into one value,
  so discharging totality alone cannot be expressed, and every form carrying a catch-all reported
  where the standard forbids it. It also produced one message for two different failures.

- **Keeping the all-predicates lowering for every qualifier and suppressing only the report.** It
  fixes what is reported and leaves `priority` evaluating arms past the first match, which the
  standard asks for only for `unique` and `unique0`. That is observable through a side effect in a
  later condition, and it breaks the ordinary guarded idiom outright: in
  `priority if (h == null) ... else if (h.v == 3) ...` the guarded condition is evaluated with the
  handle known null. A qualifier that changes what its statement evaluates is not a check.

- **Treating a `default` as not discharging totality, on 12.5's "ignored during this linear
  search".** Rejected on the three readings above. Adopting it would also make the two statement
  forms disagree, which is the shape most likely to be read as a defect by anyone converting between
  them.

- **Reporting both a uniqueness and a totality violation from one execution.** They cannot both hold
  -- more than one arm matched and no arm matched are disjoint -- so the shape would add a
  combination that has no producer.

## Consequences

- `priority` with a catch-all is lowered exactly as the unqualified statement: no snapshot, no
  closure, no submission to the Observed region, and first-match evaluation. This is the common form
  in synthesizable RTL, so the qualifier stops carrying a per-execution cost there.
- A `unique` violation and a `unique0` violation are the same uniqueness report; the two qualifiers
  differ only in whether totality is also asserted.
- Reporting stays where LRM 12.4.2.1 puts it, and where 12.5.3.1 sends the case form. The check is
  evaluated where the statement executes and the report matures in the Observed region, subject to
  the flush points that make it immune to zero-delay glitches; `ambient-runtime-services.md`
  invariant 8 owns that machinery and this decision does not touch it.
- No layer below HIR-to-MIR learns anything. MIR sees a cascade and, where an assertion is live, a
  closure submitted to a region -- primitives it already has.

## Cross-references

- [ambient-runtime-services](ambient-runtime-services.md) -- who owns a pending violation report,
  and what discards it before it matures.
- [conformance-diagnostic-claims](conformance-diagnostic-claims.md) -- the instrument that can see a
  requirement whose whole observable is a report, which a self-checking case cannot.
- `../architecture/scheduling.md` -- the Observed region the report matures in.
