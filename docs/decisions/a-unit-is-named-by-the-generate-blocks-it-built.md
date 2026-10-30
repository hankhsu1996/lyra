# A Unit Is Named By The Generate Blocks It Built

## Date

2026-10-14

## Status

Accepted. Revises
[a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
D1 and D2,
[a-unit-is-compiled-to-one-body-per-distinct-code](a-unit-is-compiled-to-one-body-per-distinct-code.md)
D3,
[a-conditional-generate-chooses-at-construction](a-conditional-generate-chooses-at-construction.md)
D1 and D2, [one-body-built-at-every-index](one-body-built-at-every-index.md) D2, and
[a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) D4 and D5: a
compiled body holds the blocks its own instances built and no other instance's. Answers
[parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) Decision 2 for
what an instance built (below).

D2 and D5 reversed 2026-10-25 by
[a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) D5 and
[a-body-states-nothing-of-another-bodys-implementation](a-body-states-nothing-of-another-bodys-implementation.md)
D1 and D4: every block is lowered on its own and found by comparing what it came out as, which body
stands at each place is its object's construction, and a block reaches the scopes around it through
what they published. The objection "Why the direction changed" raised against a block as an artifact
of its own -- every name a block reads in its module crossing units -- is answered by the published
class, as a Java inner class reaches its outer object through a reference it is handed (JLS 15.9.2).
D1, D3 and D4 stand: the blocks an instance built are still part of its unit's name.

D1 and D4 revised 2026-10-28 by
[a-unit-is-the-specializations-that-publish-alike](a-unit-is-the-specializations-that-publish-alike.md):
which blocks an instance built is part of what it publishes, which is what decides its class, not an
input to a name worked out before it is declared; a value deciding what a child builds names nothing
of the scope handing it.

D1 and D4 reversed 2026-10-29 by the same record: which blocks an instance built is no part of what
its unit publishes either. The requirement above stands, and does not select D1: the blocks built
are chosen from values handed at construction, so instances that built different blocks are one unit
publishing every block class any of them built, and a body is still a function of its name.

## Requirement

A compiled body is a function of its name. Two instances of one name compile to the same artifact,
and an instance added anywhere in the design changes no artifact another name already produced.

## Facts it rests on

1. A generate scheme is evaluated during elaboration, and its result is fixed before simulation (LRM
   27.3). A conditional builds the alternative its condition selects and no other (LRM 27.5), and an
   alternative it did not select is not elaborated: its text need not be valid for the instance's
   values. `if (i > 0) ... s[i-1].x ...` names a block that does not exist at `i == 0`, and the
   language accepts it because that arm is never built there. The front end marks such a block
   uninstantiated and binds nothing inside it.
2. A parameter read only as a value is handed to the object at construction and is not part of any
   name
   ([a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)).
   So two instances of one name can be handed values that select different alternatives.
3. C++ does not instantiate the branch an `if constexpr` discards, and the condition must be a
   constant of the instantiation, so two instantiations whose selected branches differ are two
   instantiations. rustc names each `Instance` by its definition and its whole arguments
   (`rustc_monomorphize/src/collector.rs` lists functions, methods and closures as items, each
   resolved from its own arguments); Rust has no `if constexpr`, so what it shows here is only that
   identity is the definition and its arguments.
4. Measured at the step that collects the design's bodies, counting for every body how many
   different sets of blocks its instances built: Ibex 35 bodies become 36; OpenTitan 1253 become
   1347 (7.5%, all small primitives such as a register-write arbiter split into up to five);
   Caliptra 872 become 885; Vortex 389 become 392; OpenPiton 336 become 338; the other twelve
   RTLMeter designs do not change (3673 bodies). A large design measured outside the project went
   from 1085 to 1107 (2.0%): nineteen bodies of eleven definitions split, at most into three, all
   but two of them small arithmetic or control modules. Counting the scopes a value handed to such a
   child splits as well (D4), the same RTLMeter designs come to: OpenTitan 1388 (10.8% over 1253),
   Caliptra 900 (3.2%), Vortex 394, Ibex 36, OpenPiton 338, and the other twelve unchanged.

## Decision

**D1 (reversed 2026-10-29, see Status). The generate blocks an instance built are part of its unit's
name.** Each block enters as the steps from the instance down to it, each step the block's label,
which one it is among blocks of that label, and the application of its definition it is
([a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) D1). A loop's
blocks enter as the kinds of block it built and not as how many: the construction counts them out
(LRM 27.4), so a loop of eight and a loop of sixteen blocks of one kind are one name. Where one loop
built blocks of several kinds, which kind stands at each index enters too, since the construction
chooses by it. An instance it holds is not crossed; what that instance built is its own name's.

**D2. Which body each block is enters the body's name, where any block is one of several bodies of
its class.** The unit's name already holds the classes. A block whose code uses its index is a body
per value of it (D5 of the nested-definition record), and those values are then part of which body
the instance is.

**D3. A body is lowered from the instance it is named for, and from nothing else.** Every instance
of a name built what that instance built, so the one instance is the whole of what the body holds.
What a unit publishes is declared from the same instance, by every body of the unit alike. Where
every instance is read through its own body -- the check of the compile against itself -- each other
instance of the name is lowered too and has to come out equal; one that does not is a defect of how
the name was worked out.

**D4 (reversed 2026-10-29, see Status). A value a scope hands a child, where it decides which blocks
the child builds, is one the scope's signature uses.** The scope declares the child as a member of
the child's unit, and which blocks the child built is part of that unit's name, so the value names
the scope's class as it would if the child's signature used it -- `Leaf<i>` in C++, one class per
value. A loop handing each child its index, where the index decides what the child builds, therefore
compiles its block once per index. What decides is every value a generate scheme reads: a
conditional's condition, a case's selector and items, and a loop's initial value, stop condition and
iteration (LRM 27.4, 27.5), each of which the front end records as it evaluates the scheme.

**D5. Within one instance, a loop's blocks of one name are still one body.** Blocks of one class at
different indices may build different nested alternatives, a chain's first block and its others for
instance, and that body holds every alternative its blocks built -- which the name holds, by D1. It
is lowered once, as D3 lowers an instance's body: its own declarations and code from one of its
blocks, each nested alternative and loop body from one block that built it, and which blocks were
counted out and selected from all of them. Nothing is lowered twice and nothing is compared. Where
the compile checks itself, every part is lowered from the last block that built it rather than the
first, so the comparison of the two compiles reads a second block of every part. A loop of one body
lists no values of its index: it builds that body wherever it counts a block out, and two objects of
one class may count differently.

## Why the direction changed

**The shape this replaces.** A unit's body held every block any of its instances built. Each
instance handed values none before it was ("a witness") was lowered too and merged into the body,
alternative by alternative and loop body by loop body, and the unit's signature was declared from
all of them and merged the same way. The construction ran the source's conditions and loops against
the values the object was handed, building whichever blocks they selected. It produced correct
programs. What it cost was the merge -- a comparison of lowered scopes field by field, and an
internal error wherever two instances differed in anything else, which is how two leaks in the key
first showed on Ibex -- and a body whose content followed instances it was not named for: with one
instance of a module the body held one alternative, and adding an instance elsewhere that selected
the other changed it.

**What was proposed next, and why it was not built.** The first answer (2026-10-13) moved every
"which" out of the body: the design root would state, per instance, which alternative each
conditional built, how many blocks each loop counted and which body each child is, as constant data;
each generate block would be a compiled artifact of its own; and a parent would hold a child through
a base pointer so that its name did not follow the child's. Held against what rustc and C++ do,
three of its premises went beyond them:

- Sharing one body across instances whose selected branches differ. No compiler does, because the
  branch not selected may not be valid there (fact 1, fact 3); serving it anyway needs the branches
  compiled apart from the body, which made every name a block reads in its enclosing module -- each
  signal, parameter, type, function and net -- a reference across compiled units.
- Never evaluating at construction what the front end already decided. A C++ constructor runs its
  own loop over a count it was handed; nothing requires the answer to arrive as data.
- A parent compiled once per raw value of a parameter it hands a child (eight parents for `.W(P/2)`
  over eight values of `P`). `Mid<P>` naming `Leaf<P/2>` is eight instantiations in C++ as well, so
  this is the field's behaviour and not a defect.

The requirement those premises served is the one above, and C++ meets it the plain way: what an
`if constexpr` selected is part of the instantiation. That is D1, with the one difference SV forces:
the name holds which blocks were built rather than the value that selected them, because a value
read only as a value is not part of a name here (fact 2) -- so two values selecting the same
alternative are still one body.

**What it costs.** A module whose instances build different sets of blocks compiles once per set,
and a scope handing such a module the value that decides it compiles once per value (D4). Fact 4 is
that cost on nineteen real designs: none at all on twelve of them, and at most 10.8% more bodies,
almost all of them small primitives.

## What this reads of the elaborated design

[parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) Decision 2
forbids reading more of the elaborated instance graph -- expanded per-instance values, materialized
sub-instances -- as compile-time facts, because the graph would then decide identity where the
definition and its arguments should (the North Star forbids the graph as the authority for unit or
specialization identity). D1 reads which blocks an instance built off its elaboration, so it has to
answer that.

It reads the evaluation of the instance's own generate scheme, not the graph. Which blocks stand is
fixed by the definition and the values the scheme read (LRM 27.3), and the front end records those
values as it evaluates the scheme (D4), so two instances of one definition whose schemes read the
same values built the same blocks, wherever they sit and whatever else elaborated. That is how a
constant the front end evaluated is read, and how an `if constexpr` branch follows from a template's
arguments. What the graph would add beyond it is not read: D1 stops at an instance the scope holds,
whose blocks are that instance's own name, and the front end's record that two instances duplicate
each other decides nothing. What the reading costs is that the name waits on the front end's
elaboration of the instance, where rustc names an `Instance` from its definition and arguments
without building anything.

## Limits, each with its cause

- A module compiles once per set of blocks its instances built (D1), because an alternative not
  selected is not elaborated and cannot be compiled from an instance that did not select it (LRM
  27.5).
- A loop whose blocks are of several kinds compiles once per way its indices map to those kinds
  (D1), because the construction chooses the kind by the index.
- A loop handing each child its index, where the index decides which blocks the child builds,
  compiles a block class per index (D4), because the block declares the child by its unit. Naming
  the block by which unit its child is instead would bound the classes by the child's kinds, but the
  construction would still choose among them by a list of indices, and no compiler in the field
  names a scope by what its members were instantiated to rather than by its own arguments.
- A scope handing a child the count of a loop whose blocks are all of one kind compiles once per
  value (D4), though the child's name does not follow the count, because the front end records which
  values a scheme read and not what the scheme built from them.

## Rejected

- **A body holding every block any of its instances built.** The shape replaced: its content follows
  instances it is not named for, and building it merges every instance handed new values.
- **Each block its own compiled artifact, chosen by data the design root states.** Above: it serves
  a requirement no compiler in the field has, and turns every access from a block into its enclosing
  module into a reference across units.
- **The values that select blocks in the name.** A loop handing each child its index, where the
  index selects an alternative, would compile a child per index instead of one per alternative; the
  blocks built are the coarsest name that still compiles only what was built.

## Revises

- [a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
  D1 said a unit holds every alternative and every loop body any of its instances built, and D2 had
  every instance handed new values lowered into its unit. A unit now holds what the instances of its
  name built, and those are one set (D1, D3 here).
- [a-unit-is-compiled-to-one-body-per-distinct-code](a-unit-is-compiled-to-one-body-per-distinct-code.md)
  D3 had the first body of a unit declared from every instance of it and the others import the
  classes they built no scope of. Every body declares from its own instance (D3 here).
- [a-conditional-generate-chooses-at-construction](a-conditional-generate-chooses-at-construction.md)
  D1 and D2 stand for a conditional written inside a loop, whose blocks are collected within one
  instance; what a unit holds of a conditional outside a loop is now the alternative its name says.
  Its D2's two blocks being one body where they compare equal, each pair merged, is replaced: the
  body is lowered once, each part from one block that built it (D5 here).
- [one-body-built-at-every-index](one-body-built-at-every-index.md) D2 and
  [a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) D5: blocks
  of one name are one body within an instance, never across instances, lowered once rather than each
  lowered and compared (D5 here); D4 of the nested-definition record, which listed what selects an
  alternative among what decides nothing for a name, is narrowed: it decides nothing for the block's
  own name, and what it selected is part of the name of the instance holding the block.
