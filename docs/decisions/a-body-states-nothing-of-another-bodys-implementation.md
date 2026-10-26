# A Body States Nothing Of Another Body's Implementation

## Date

2026-10-24

## Status

Accepted. Narrows how
[a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) reaches a
block of a loop from inside the unit holding it, and how
[a-unit-is-compiled-to-one-body-per-distinct-code](a-unit-is-compiled-to-one-body-per-distinct-code.md)
D6 lays out the table a body's scopes are built from. D1 to D3 widened and D4 to D6 added
2026-10-25: what held for a reach into a neighbouring block holds for every reach leaving a body,
upward ones included, and which body each child of an object is, is that object's construction, so a
parent's code names no child body either; that is what lets bodies be found by comparing what they
state (fact 6).

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
5. Audit (agent, 2026-10-25): a block also reached its ancestors through their implementations -- a
   data object, a static, a disable target or a subroutine of an enclosing scope by that scope's own
   identity, and a climb through each enclosing scope's own class -- so equal code was the same code
   only among blocks one parent body held.
6. A body is found by comparing what it states
   ([a-unit-is-compiled-to-one-body-per-distinct-code](a-unit-is-compiled-to-one-body-per-distinct-code.md)
   D3, [a-generate-block-is-a-nested-definition](a-generate-block-is-a-nested-definition.md) D5), so
   two bodies are one only where nothing either states differs by which body anything else is.
7. Field: a Java inner class is one implementation per source class, reaching its outer object
   through one reference typed by the outer class and passed at construction (JLS 15.9); a Rust item
   nested in a function cannot use the function's generics (E0401), and inferring afterwards that it
   does not (polymorphization) was removed (rust-lang/rust #133883); CIRCT's `Dedup` compares a
   module's children by their representative.

## Decision

**D1. A reach leaving the reader's own scope goes through what the scope it reaches published,**
inside the unit as across it: into a block, the published loop or choice, the select as written
evaluated in each object, the class the block was published as, then published members and blocks
only; up to an enclosing scope, the class that scope published and its published members and disable
targets. Only a reach that stays in the reader's own scope names what that scope declares by its own
identity, and the HIR verifier refuses any other.

**D2. A subroutine of another scope -- a block, or a scope enclosing the caller -- is called through
the slot its published class introduces.** A call to a subroutine of the caller's own scope stays
direct. The slot is how the published class is realized; it is no SystemVerilog `virtual` method and
adds none of that method's semantics.

**D3. A scope's construction table is its own, and says how each of its children is made.** It
lists, for each instantiation the scope declares, the entry of the body made there and that body's
table, and for each generate the scope holds, the same for the block at each place: each alternative
a conditional wrote, each block a loop counts out, a lone block's one place. A scope's code names
only positions of its own table -- an instantiation, or a generate and a place -- and builds every
child through the entry there. Which body each child is, is the table's, stated where the program is
assembled, so the scope's code is the same whichever bodies its children are.

**D4. A block keeps the scope that built it as what that scope published.** The link is a field of
the block's published class, set by the published constructor from the parent it is handed as the
base every scope extends, so every body realizing the class has it in one place. A climb reads it
once per scope and views what it reached as the class that scope published, so a block's code names
no enclosing scope's implementation.

**D5. A block body is lowered inside the scope publishing the class its own class sits in.** It is
the unit's own scope or, inside a block, the first block body publishing that class: every scope
publishing one class publishes alike, and a body reaches the scopes around it only through what they
published (D1, D4), so whichever stands there is read the same, and taking the first makes the
lowering a function of the body alone and never of which object built it first.

**D6. Two reaches still name another scope's own identity, each where only one scope can stand.** An
interface port is declared only on the unit's own scope, which one implementation holds once. A
class's static is kept by the scope declaring the class, and a block declaring a class is named by
its index, as is every block holding one, so its body is the only one of its class.

## Limits

- A block declaring a class compiles per index, since each block's class is a type of its own.
- A call to a subroutine of an enclosing scope goes through a slot even where only one body realizes
  that scope (D2); removing it is a pass over the program once every body is known.
- Blocks that reach, each by its own index, blocks of a different shape are a body per index, since
  the class a reach lands on is part of the code making it.

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
- **A block lowered inside whichever scope a specialization's construction first built it under.**
  Two specializations of one implementation could then lower its blocks differently, in the order of
  classes and of the walk, so what an implementation compiles to would follow which specialization
  reached it first.
- **Each scope's table as a section of one table of the object, reached by an offset.** Equivalent
  in what code states, but the offset is a position among the object's scopes again, now carried as
  data; a table per scope keeps every position a scope's own.
