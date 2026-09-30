# A part a construction value selects is the select the source wrote

Date: 2026-09-28 Status: accepted

## Context

A body shared by several constructions -- a loop's blocks, or instances handed different values --
is one compiled class only if every construction states the same thing. A select whose index is a
genvar or a parameter is a constant select (LRM 11.5.3, 27.4), so the front end settles which bits
it reaches in each elaboration. Three places took that settled answer into HIR:

- the bits a continuous assignment, an event control, `always_comb`, `@*` or `wait` watches, which
  the front end's data-flow analysis reports as a flat-bit range per symbol;
- the type of an indexed part-select, which the front end gives the range it reached (`v[i*2 +: 2]`
  is `[1:0]` in one block and `[3:2]` in the next);
- the positions a bidirectional connection or an `alias` joins.

So `assign d[e] = v[e];` in a loop was one class per index, and a module reading `v[N]` was one unit
per value, the second caught only by comparing instances. A large design measured outside the
project spent most of its emitted C++ this way, and could not be linked.

## Decision

**D1. HIR states such a part as the select the source wrote.** That is the longest static prefix, an
ordinary select expression whose index is whatever the source wrote -- a literal, a genvar, a
parameter. Each construction evaluates it with its own index or value. The layer below turns the
select into a position with the same step a read of it takes, so a wait on a part and a read of it
cannot disagree about where it lies.

**D2. Which bits are watched stays the front end's answer; only its spelling changes.** For each run
of bits the data-flow analysis reports, the prefixes in the analyzed node that make up exactly that
run are used in its place. A run no prefixes make up -- a read inside a called function, what is
left of reads once a procedure's own writes are excluded -- stays the settled run. Must-def and
local exclusion (LRM 9.2.2.2.1) therefore keep the precision the read-set decision chose them for.

**D3. Soundness across constructions is not argued, it is checked.** Every construction is still
lowered and compared -- a loop's blocks against each other, an instance handed new values against
the unit it shares. A construction whose exclusions came out differently spells its part differently
and stays apart. D2 never has to prove that one spelling holds for every construction.

**D4. A part-select's HIR type is a vector as wide as the select, numbered from zero** in the
direction of the value it selects from. Which bits it came from belongs to the select, and LRM
11.5.1 holds only the width of a part-select constant. The front end's own type still answers a
query on the expression, since queries read it.

## Consequences

- A loop of `assign d[e] = v[e];` at 64 iterations emits two scope classes for the whole module.
- A wait or a join may carry a position that is an expression rather than a literal; both backends
  already took one.
- A connection or an alias reaching a net whose data type is an unpacked aggregate is refused by
  name, whether it names the whole net or one element. Such a net keeps no runs of positions another
  net could join. It used to stop the run with an internal error, or produce C++ that did not
  compile.

## Rejected alternatives

**Watch the whole variable where the range depends on a construction value.** Invisible for a
continuous assignment (LRM 10.3.2 assigns only when the value changes), but visible for
`always_comb`, whose body re-runs -- a `$display` in it prints once more -- against LRM 9.2.2.2.1's
implicit list. It also wakes every block of a loop on every bit that changes.

**Compute the read set from the source's selects instead of the data-flow analysis.** It gives the
symbolic form directly and loses must-def exclusion, which the read-set decision rejected on
precision grounds that still hold.

**Pass each construction's settled range as a construction argument.** It works inside one unit,
where the loop's blocks are all visible, and fails at a module boundary: the parent would have to
read its child's body to know the child's ranges.
