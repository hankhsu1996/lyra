# A Body States Nothing Of Another Body's Implementation

## Date

2026-10-24

## Status

Accepted. Narrows how
[a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) reaches a
block of a loop from inside the unit holding it, and how
[a-unit-is-compiled-to-a-body-per-value-its-code-relies-on](a-unit-is-compiled-to-a-body-per-value-its-code-relies-on.md)
D6 lays out the table a body's scopes are built from.

## Requirement

What a body's code states about anything outside itself is what that thing publishes and where the
source wrote it, never which body it compiled to, so two bodies that do the same thing state the
same code.

## Facts it rests on

1. A reach into another unit already goes through what the scope it lands on published: the
   published loop or choice, the class each block was published as, a published member, a published
   callable called through its slot.
2. Every named object is reachable by a hierarchical name (LRM 23.6), so everything a name can reach
   is published; there is no part of a body another body may name that its publication lacks.
3. A block of a loop is another body than whichever reaches it: the select a name writes picks one
   in each object (LRM 27.4), and blocks of one loop may compile to different bodies. Before this
   entry, a reach from inside the unit named the body of the block it landed on, so a block reaching
   its neighbour stated its neighbour's implementation, and a chain of such reaches made every
   block's code differ from every other's.
4. A scope's code read the construction table by positions laid out across the whole object, so a
   block's code named where its instantiations stood among every other scope's.

## Decision

**D1. A reach into a block of a loop goes through what the loop's scope published,** inside the unit
as across it: the published loop, the select as written evaluated in each object, the class the
block was published as, then published members and blocks only. A reach that stays in the reader's
own body or climbs to a scope enclosing it is unchanged, since nothing it names differs by which
body a neighbour compiled to.

**D2. A subroutine of a block of a loop is called through the slot its published class introduces.**
A call to a subroutine of the caller's own body, or of a scope enclosing it, stays direct. The slot
is how the published class is realized; it is no SystemVerilog `virtual` method and adds none of
that method's semantics.

**D3. A scope's construction table is its own.** It lists what the scope makes at each instantiation
it declares, in the order it declares them, and the table each of its generate blocks is built with;
a scope's code names only positions of its own table, and a scope building a block hands that block
its table. Which body is made at an instantiation stays the table's, stated where the program is
assembled.

## Limits

- A loop block's published class is still named by the index where what the block declares relies on
  it, so blocks whose declarations differ only in that name still state different code.
- A class declared in a block is a class per body, named by the unit's own identity for it.
- Blocks whose select is worked out from their own index are still a body per index, since the
  parameters the select reads name each block.

## Survey

- **A call through what a value publishes.** rustc lays out a trait object's vtable from the trait,
  one entry per dispatchable method (`vtable_entries` in
  `compiler/rustc_trait_selection/src/traits/vtable.rs`, entries `VtblEntry` in
  `compiler/rustc_middle/src/ty/vtable.rs`), so a caller's code names the trait's slot and never the
  implementing type. D2 is that shape with the block's published class in the trait's place. Ours
  differs in what picks the implementation: a select evaluated in each object, not a value's dynamic
  type.
- **Removing the slot once the callees are known.** LLVM's whole-program devirtualization replaces a
  virtual call with a direct one where only one callee is possible
  (`llvm/lib/Transforms/IPO/WholeProgramDevirt.cpp`, its opening comment), after the program's
  classes are all known. That is the later pass the first rejected alternative leaves the direct
  call to.
- **A table per scope.** Recalled, not read: a C++ constructor hands each member and base its own
  construction arguments rather than a position in a table of the whole object. No source was opened
  for this one.

## Rejected

- **Calling a subroutine of another block directly where its body is known.** It states the other
  body's implementation again; an optimization that removes the slot is a later pass's, once which
  bodies are shared is settled.
- **Each scope's table as a section of one table of the object, reached by an offset.** Equivalent
  in what code states, but the offset is a position among the object's scopes again, now carried as
  data; a table per scope keeps every position a scope's own.
