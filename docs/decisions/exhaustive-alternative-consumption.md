# Exhaustive consumption of a closed alternative set

Date: 2026-08-27 Status: accepted

## Context

The front end resolves facts Lyra cannot re-derive, and a pass that drops one produces a wrong
answer with nothing to point at. One such drop was found and traced end to end.

slang classifies every conversion it inserts. A conversion the type-propagation step pushes down
into a context-determined operand is marked `Propagated`; one at an assignment-like boundary is
`Implicit`. The distinction is load-bearing: LRM 11.8.2 extends a propagated operand by the
propagated type's signedness, while LRM 11.8.3 extends an assignment's right-hand side by its own,
and the two disagree for a signed value reaching a wider unsigned type. slang implements both, and
says so in its own source: it restates the sign flag before resizing for the propagated case, and
resizes before restating it for the assignment case.

AST-to-HIR carried that classification faithfully into `hir::ConversionExpr`. HIR-to-MIR then read
the operand and never read the classification, so every conversion took the assignment rule and
`8'b11110000 & 4'sb1010` answered `11110000` where the standard requires `00000000`.

Nothing was missing from the front end. The fact arrived and the pass that needed it did not look.

## What let it happen

HIR expresses **which construct** an expression is as an arm of a `std::variant`, and **which
flavour of that construct** as a field beside the payload. A consumer reaches the payload through
`std::visit` or `std::get_if`, which the compiler holds to the whole variant; the field beside it is
invisible to that check. Every fact found dropped in the audit that followed was a flavour, never a
construct.

The project already owned the enforcement mechanism -- `-Werror=switch` is on, and a `switch` over
an `enum class` that omits an enumerator is a build error. Most of the alternative sets HIR carried
were consumed that way and were sound; eight were consumed by `==` instead, which opts out of the
mechanism at no cost and reads perfectly naturally. Nothing distinguishes "asking a yes/no question
of a two-valued flag" from "classifying five alternatives and forgetting three".

The style rule that should have caught it -- a closed set of alternatives is a variant of per-kind
structs, not a tag enum beside spare fields -- is justified by "so an invalid combination cannot be
spelled". Every `(operand, kind)` pair is valid, so read against its own rationale the rule did not
apply. It named a mechanism and one of that mechanism's two benefits, and the benefit it left out is
the one that was needed.

## Decision

A closed set of alternatives is **consumed by a `switch` or a `std::visit`, never by `==` alone**.
Gaining an alternative must break the build until every consumer says what the new one means.

Where several places ask the same question of one set, the question gets a name and that name is
defined by a single switch: `RequiresWriteback(ParamDirection)`, `NamesStorage(LocalKind)`,
`CapturesByReference(CaptureView)`. A hand-rolled grouping repeated at each call site is the same
defect distributed.

A set that nothing dispatches on is exempt by saying so: metadata the compiler only prints, an
encoding it only emits for the simulated program to compare, or an ordered scale whose consumers
compare position rather than name a member. The exemption is a claim the author writes in the
comment above the declaration -- "not a dispatch set" -- and is held to it.

`tools/policy/check_architecture.py` A013 enforces this from three alternatives up, over every enum
Lyra declares, a file-local one in a `.cpp` included. A two-valued set is left alone because an
if-else over it is already total; the day it gains a third value the rule fires and the consumer has
to be found.

A registry read in both directions cannot be a switch and so cannot be checked this way. There is
one, the diagnostic-code table, and A014 holds every code to an entry in it.

## Rejected

- **Make every such set a `std::variant` of per-kind structs.** The shape that carries a per-kind
  payload, not the shape that carries an alternative with no payload of its own: five conversion
  kinds over one operand would become five structs with identical contents, paying a structural cost
  for an invalid state that does not exist. The switch gets the same compiler enforcement with none
  of that.

- **Fix the instances and leave the rule as prose.** The instances were found by writing the query;
  a rule nobody can run finds the next one only by luck. The query is forty lines and the project
  already gates five policy checks in CI.

- **Forbid a `default:` arm on a switch over an alternative set.** It is the same opt-out in another
  disguise, and where the set is a large open registry -- the builtin table, the format specifiers
  -- a default arm is the only workable shape. A rule that fires on those is noise, so the ban is
  not written; a default arm that silently routes a new alternative somewhere plausible remains a
  judgement call at review.

## Consequences

- Adding an alternative to any set of three or more fails the build or the policy check until each
  consumer decides what it means.
- A question asked of an alternative set is a named predicate, so the answer is stated once and read
  everywhere rather than re-derived per site.
- An exemption is legible: the comment says the set is not a dispatch set and why, which is a claim
  a reviewer can disagree with.
- Two constructs the pipeline had been folding into an unrelated path now refuse by name: a
  bitstream cast and the streaming operators, which had been reaching a conversion that returned
  their operand unchanged and failing later as a host compile error or an internal error.

## Cross-references

- `architecture/mir.md` (a backend never re-derives a stated fact).
- `front-end-semantic-boundary.md` (slang owns semantic resolution; Lyra owns translation, and never
  classifies from degraded information).
