# A conditional generate's alternative is chosen at construction

Date: 2026-09-22 Status: accepted

## Context

A loop generate's blocks compile to one body where they lower alike and to one body per index where
they do not. The commonest reason they do not is a conditional written inside the loop, and it is
the shape most repeated structure in real RTL is written with: a chain seeds at its first index and
steps at every other, a reduction tree's leaves differ from its nodes, a pipeline's end stages
differ from its middle.

Measured on a 64-iteration chain whose first block seeds and whose others read the block before it:
522,370 bytes of bodies, 214,675 of declarations and 386 compiled classes, against 131,650 / 55,346
/ 98 at 16 iterations. That is 8.1 KB of bodies, 3.3 KB of declarations and 6 classes per iteration
for a loop holding **two** distinct bodies. The all-alike loop beside it is 13 KB at any iteration
count, and the declarations half is the one that travels, because every referrer includes and parses
it.

Every figure here is a C++ emit of a module declaring an unpacked array of `N` elements and a loop
generate over `N` indices with the conditional written inside it, and nothing else. Bodies is the
emitted source file, declarations the header beside it, and classes the type declarations in that
header.

So the artifact grows with what elaboration counted out, for a source that wrote one loop and one
conditional. `north_star.md` invariant 2 is written against exactly that, and its own sentence says
where the answer goes: a generate `if` is constructor-time logic the runtime executes to build the
object graph at time zero.

## The tension this addresses

A conditional generate selects at most one block from a set of alternatives, on constant expressions
evaluated during elaboration (LRM 27.5). Where the construct stands inside a loop, the expression
reads the loop index, which is a different value in every block the loop counts out (LRM 27.4) -- so
one construct selects different alternatives at different indices, and the front end hands back only
the alternative each index selected.

Two things could be stated in the artifact, and only one of them is what the source wrote.

**The answer.** Which alternative each index got, as data the construction reads back: a table of
one entry per index, or the index ranges that share an alternative. Both fail the requirement rather
than merely costing something. A table grows with the iteration count, moved from code into data. A
range list grows with the number of times the answer changes along the index order, which is two for
a chain and every index for a loop whose condition alternates -- and deriving ranges from an
arbitrary partition is a prediction about the source with no completeness argument behind it, which
is the shape [one-body-built-at-every-index](one-body-built-at-every-index.md) D2 exists to remove.

**The question.** The condition, as the expression the source wrote, evaluated where the index is
already a value the construction is handed. Its size is the source's, whatever the iteration count.

## Decision

### D1. The construct states the conditionals the source nested, and the construction runs them

A conditional generate compiles to the conditionals the source wrote, held as the source nested
them, together with one entry per alternative. Each conditional states its own question once: an
`if` states its condition and what stands on either side of it, a `case` states the expression it
selects on, its items in the order the source wrote them, and what stands where none matched. What
stands on a side is one of three things -- nothing, one of the construct's alternatives, or a
further conditional. The construction runs that against the index it was handed.

A conditional the source wrote inside another's selected side contributes its blocks to the outer
construct (LRM 27.5), and a side leading to a further conditional is exactly that. So what the
construct holds is the shape the source wrote, and the ordering LRM 12.5 gives a `case` -- the
search stops at the first match, so the `default` is reached only once every item has failed -- is
kept as the order of the items rather than restated on each of them.

**Stating each condition once is what keeps the artifact the size of the source.** The alternative
is to give every alternative the whole path it stands under, which repeats the prefix that every
alternative before it also carries: an n-alternative construct then states on the order of n^2
conditions where the source wrote n. Measured on an 8-item `case` inside a loop, that is 35 label
comparisons against 7; on a 32-item one, 239,407 bytes of bodies against 181,418.

A construct outside any loop is stated the same way. Its conditions read only what a specialization
fixes, so the construction always reaches the alternative elaboration did -- but what selects a
block is a fact about the source either way, and a layer that states it for one position and not the
other has two shapes for one rule.

### D2. The alternatives come from the source; only the bodies are collected across indices

The conditionals and the alternatives under them are stated wherever the construct stands, at every
index alike, because the front end binds each condition whether or not this elaboration reached it
and keeps a block for every alternative while marking the ones it did not select. So two elaborated
blocks of one loop state the same conditionals in the same shape, and what differs between them is
only which alternative each of them produced a body for.

**The front end had to be asked where each block sits, and that is the shape of the ask.** It walks
down through the conditionals evaluating each, so it holds that path while it decides; what it
published was each block's own condition alone, which is not the same fact -- `if (a) if (b) x;` and
`if (b) x;` publish identically, and only the first stands where `a` held too. Recovering it
downstream would mean reading the syntax the binder has already interpreted. Carrying it is a fact
the front end computed, which is the one thing a fork change is for.

The paths are what the shape is recovered from: two blocks are under the same conditional exactly
where their paths agree at that position, so walking every block's path and sharing the agreed
prefix rebuilds the nesting the source wrote. That needs the front end to identify a conditional the
same way along every path through it, which it does, and it is why the ask is a path rather than a
name of its own.

Two blocks are one body when everything outside their conditionals agrees and, for every alternative
both produced a body for, those bodies are one body in their turn. What that produces is one block
holding every body either of them produced.

**A body cannot come from the index where its alternative was not selected, and that is forced
rather than chosen.** An arm is frequently unselected precisely because it would not elaborate
there: `if (i > 0) begin ... g[i-1].x ... end` names something that does not exist at index 0. The
front end marks such a block rather than resolving it, which is why what it offers everywhere is the
selection and not the body.

**An alternative no index ever selected keeps its place and has no body.** It cannot be selected at
construction either, since the same expressions are read against the same inputs, so nothing is
compiled for it -- but dropping it would move every alternative after it, and where those positions
sit is what a name resolves against.

### D3. A route names the alternative's position, never what it compiled to

A hierarchical name reaching into a conditional's block is resolved before anything knows which
alternatives this or any other index produced a body for. What it can be resolved against is the
position the source wrote the alternative at, which is the same number wherever the construct
stands. Whichever compiled scope that turns out to be is the construct's own answer, stated once
where every consumer of a route asks it.

## Rejected alternatives

- **State which indices share an alternative, as ranges.** Above, under the tension: it grows with
  how often the answer changes, and recovering ranges from an arbitrary partition is a prediction
  about the source.

- **Keep the elaborated blocks and fold the identical compiled artifacts afterwards.** This is what
  the field does, because by the time a linker or an IR pass sees the artifacts the loop that
  produced them is gone and comparison is the only thing left. We are upstream of that and still
  hold the loop, so folding afterwards is a way of recovering what has not been discarded yet -- and
  for the backend that emits source a host compiler parses, the cost being removed is paid before
  any folding pass could run.

- **Give the enclosing block one member covering both alternatives.** The alternatives are separate
  classes, so one member means a base pointer and a cast at every name reaching into an alternative.
  A member per alternative keeps every such name typed, and at most one is ever filled.

- **Flatten the alternatives into one ordered list and let the construction search it.** The
  alternatives of one construct do not form a single order: a conditional written inside another's
  selected side contributes its blocks to the outer construct (LRM 27.5), so the list mixes levels
  and no sequence of `else`s says which condition each alternative sits under. Built that way, a
  construct whose outer condition failed still reached its inner alternatives, which is a wrong
  answer rather than a cost.

- **Give each alternative the whole path of conditions it stands under.** This is order-free, states
  what selects each alternative where that alternative is, and is correct -- it was built and it
  passed. It is rejected for its size: the path of the k-th alternative repeats the prefix the k-1
  before it also carry, so the artifact grows with the square of how many alternatives the source
  wrote, and the nesting the source already stated is recovered by nobody. The shape that states
  each condition once is the nesting itself, which is D1.

## Consequences

- A loop whose blocks differ only in which alternative of a conditional stood compiles to one body.
  Measured on the 64-iteration chain above: 16,957 bytes of bodies, 7,988 of declarations and 12
  classes, against 522,370 / 214,675 / 386. The same source at 256 iterations emits 16,958 and 7,990
  -- the difference is the digits in the loop bound.

- This reaches every conditional the language has, not the two-alternative `if` alone: an
  `if ... else if` chain, which the standard makes one construct with one alternative per arm (LRM
  27.5); a `case`; a conditional that produced no block at some index, whose construct is stated
  there all the same; and a conditional written inside another's selected side, whose blocks belong
  to the outer construct. A three-alternative chain over 64 iterations emits 22,243 bytes of bodies,
  10,132 of declarations and 16 classes against 522,167 / 214,675 / 386, and at 256 iterations
  22,244 and 10,135 -- again the digits in the loop bound. A loop whose `case` sits inside an outer
  `if` emits 22,077 / 10,170 / 16 against 509,649 / 214,509 / 386, and 22,078 / 10,173 at 256.

- Two alternatives of one construct may carry the same name, which the standard allows precisely
  because at most one is instantiated (LRM 27.5). They are separate members of the enclosing scope
  and only the one that stands ever attaches to the hierarchy, so a name reaching the construct
  reaches what elaboration said it would.

- [one-body-built-at-every-index](one-body-built-at-every-index.md)'s consequence that a
  `generate if` and a `generate case` "are untouched and need nothing here", on the grounds that one
  of them produces at most one block, is withdrawn. It is true of a construct written outside a loop
  and false of one written inside, which is elaborated once per index and so has an iteration count
  like anything else there. That entry's table of what this lowering bakes into the artifact was
  missing the row for the same reason.

- A conditional's alternative now costs a test at construction where it previously cost none,
  including where a specialization already fixed the answer. Whether that test is worth removing is
  a question for whatever runs after the shape is fixed.

## Cross-references

- [one-body-built-at-every-index](one-body-built-at-every-index.md) -- the loop axis, which this
  completes for the commonest reason blocks differ, and whose consequence this withdraws.
- [an-elaboration-time-value-is-an-input](an-elaboration-time-value-is-an-input.md) -- the general
  rule. A conditional's answer is one more elaboration-time value, and the question it poses --
  would a different value here be a different class, or the same class holding a different value --
  answers the second way here, because both alternatives' classes are in the artifact whichever one
  any index selected.
- `../architecture/hierarchy_and_generate.md` -- generate as constructor-time logic building the
  object tree.
