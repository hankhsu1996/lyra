# A member is reached at an offset derived below the execution IR

Date: 2026-09-25 Status: accepted

## Context

A member step names the class that declares the member and the position that class gave it. The two
backends realized that step differently. The C++ backend emits a class whose members are fields, and
the compiler that builds it places each at a fixed offset. The execution backend asked the runtime
for the address on every access: a call that checked the class's definition, indexed a vector,
followed a pointer to a separately allocated slot, and dispatched on the slot's kind to find the
storage inside it. Measured on the RISC-V core, that was about 1.0 G of 18.9 G instructions of a
run, the largest place where the two backends did different work for the same step.

Nothing required the lookup. Physical layout is the execution IR's to leave out and the code
generator's to derive, from the members' types and what the runtime states about its own types.
Everything that derivation needs is in the unit, except in one case.

## Decision

**A member's address is its value's address plus a distance the code generator derives: past the
kind of value it is, past the members its lineage carries ahead of the declaring class's own, to the
member's position among those.** The runtime lays every value out so that the distance holds and
states the figures it depends on, each asserted against its own types.

- A value is one allocation: the kind of value first, then one slot per member of its lineage, in
  lineage order. Every slot is one size, so a member's place is its position in the lineage times
  that size.
- Which kind of value holds a class's values follows from its lineage: a class standing in the
  design hierarchy is held by a scope, every other class by a plain object. Each kind has a fixed
  size, so where members begin is a figure per kind.
- How many members a lineage carries ahead of a class's own is a constant wherever the lineage is
  this unit's classes down to a root the runtime provides. Where it passes through another unit's
  class it is not this unit's to know, and the step reads the count the runtime recorded when it
  realized the declaring class.
- Only the allocation that makes room for the members builds a value whose members the runtime
  holds. Neither kind of value can be built any other way, so a value made without that room does
  not compile rather than overwriting what lies past it; a target that lays out its own scopes
  builds the scope only as the base of its own class, whose class has no slots.

The cross-unit case is where this departs from the C++ backend on purpose. A class extending another
unit's class compiles there against that class's full declaration, `local` properties included (LRM
8.18 restricts who may name a property, not whether a subclass carries its storage), so a private
change to the base recompiles every unit extending it. A unit's signature does not publish that
storage, because a change a unit keeps to itself must not recompile other units. So the execution
backend takes the answer Objective-C's non-fragile layout takes: the base's size is fixed where the
base is realized, and a subclass reads it. The case is a class of the source language extending one
another unit declares; a design element's object is never reached this way, since a referrer calls
what it promised rather than stepping into its storage.

## Rejected alternatives

- **Asking the runtime for every member's address.** The shape this replaces. It deferred to the run
  a layout the compiler already had the facts for.
- **Publishing a class's `local` property storage in its signature, so every layout is a constant.**
  What the C++ backend effectively does. It makes a change a unit keeps to itself recompile the
  units extending its classes.
- **A per-class offset every class exports, filled when the runtime realizes it.** It pays a load on
  every access, including where the lineage is visible and the distance is a constant.
- **Holding the members behind a pointer the value carries.** One more load per access and no
  simpler: the kinds of value differ in size, but each is a fixed figure the runtime can state.
- **Sizing each slot by its storage kind.** Denser, but slot size is the value representation's
  question and is the same with or without a derived offset; uniform slots are what make a position
  a multiplication.

## Consequences

- A member step inside a unit's own lineage is one address computation from constants, and the
  runtime call it replaced is gone from the entry table.
- A value is one allocation where it was one per member plus a table, and the members are
  contiguous.
- The figures the code generator uses -- slot size, where each kind of value's members begin, where
  a definition records its lineage count -- are stated once beside the runtime's other layout
  statements and asserted by the runtime. A change to a runtime type that moves one fails the
  runtime's build rather than a program.
- On the clock-pipeline benchmark the execution backend runs 5.5% fewer instructions (632.5 M to
  597.4 M for 2000 cycles), which is about the share the lookup had.
