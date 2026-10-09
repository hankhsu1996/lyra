# A Specialization Is Held Where Its Arguments Are

## Date

2026-10-05

## Status

Accepted.

## Why this decision matters

A package's generic class is specialized wherever a site names it, with arguments that site writes:
`a::Box#(C)` in package `b`, on a class `b` declares. Every specialization of a generic used to be a
class of the unit declaring the generic, so package `a` held a class whose field is `b`'s class
while `b` held a variable of that class -- each unit's content depended on the other, though the
program only has `b` depending on `a` (LRM 26.3 orders packages). What `a` compiled followed from
how other units used its declarations, which no independently compiled unit can do, and `b` read its
own class back through `a`'s signature as another unit's, which the MIR verifier refused.
Specialized on a class a module declares, the specialization is a different type for each instance
of that module (LRM 6.22, 8.25), and as a class of the package it had one set of static properties
for all of them.

The same shape stood in the C++ backend's files: every struct a unit declares went into one header,
which included the header of every unit whose namespace it consumed, so two units whose structs hold
each other's by value included each other.

## What the objective requires

`north_star.md` invariants 4 and 5: a unit compiles from its own source and the signatures of what
it names, and every cross-unit relationship is declared rather than discovered. So **a unit holds
exactly the declarations its own source fixes**, and each specialization is one type with one set of
static properties for each thing that replicates it.

## Survey

- clang makes an implicit instantiation in every translation unit that needs it and gives it
  discardable ODR linkage (`basicGVALinkageForFunction`, `GVA_DiscardableODR` for
  `TSK_ImplicitInstantiation`, clang/lib/AST/ASTContext.cpp); the linker keeps one. The template's
  own translation unit never sees an instantiation another one asked for. It makes them everywhere
  because a translation unit has no view of the program.
- rustc publishes a generic's MIR as part of its crate (`should_encode_mir`,
  rustc_metadata/src/rmeta/encoder.rs, for an item that `requires_monomorphization`), and the crate
  using it makes the instance; under share-generics, on by default below `-O2`
  (rustc_session/src/config.rs), a downstream crate reuses an instance an upstream crate already
  made (`Instance::upstream_monomorphization`, rustc_middle/src/ty/instance.rs).
- Neither ever places an instance whose arguments come from downstream in the crate or translation
  unit declaring the generic.
- Where Lyra differs: it knows every specialization the design uses before any unit compiles, so a
  specialization can have one home instead of being made everywhere and merged; and units compile in
  no order, so "whoever made it first" names no unit.

## Decisions

### D1. The scope replicating a type decides where it is held

A type declared in a module or a generate block is a type of each instance of that scope (LRM 6.22).
A specialization is a type of its own for each set of parameters (LRM 8.25), so one specialized on
such a type is replicated by the same scope, wherever its generic was declared. The scope that
replicates a type is the innermost instance scope among the one enclosing it and those replicating
each type argument of a specialization it is or lies in.

Every placement follows from that one answer: the unit holding a declaration is the design element
that scope lies in; the cells a class keeps for itself are one per instance of it; and a class takes
the instance it belongs to when that scope is an instance's. A specialization no instance replicates
is a unit of its own, since the unit declaring its generic fixes none of its arguments. Names inside
a specialization's bodies still resolve where the generic is written.

### D2. A specialization's own unit is a namespace unit holding that class

It is lowered from the scope declaring its generic and holds nothing of that scope but itself. Its
name is the generic's unit joined to the specialization's name the way a specialization's name joins
its definition to the rest, so it is an identifier as a module's is.

### D3. Every body is bound before anything is placed

The front end makes a specialization where an expression naming it is first bound, and binds a body
it judged a duplicate only when something reads it. So every body a unit is lowered from is bound
before placement is decided; otherwise a specialization appears while its unit is already lowering.

### D4. A unit reading its own class through another's signature reads its own class

A specialization of another unit's generic holds values of this unit's classes and structs, so the
signature it publishes names them, and the unit reading it resolves each to its own declaration
(`a-class-has-one-identity-in-a-unit.md` D1, now for every class a unit declares).

### D5. A C++ header holds one struct

A struct is defined in a file of its own that includes the file of each struct it holds by value, as
a class's file includes its bases'. A struct cannot hold itself by value, so the files form no
cycle; a header holding all of one unit's structs does as soon as two units hold each other's.

That a struct holds another is already what MIR states, as the types of its members. That the held
one is defined first is a requirement of C++ text alone: the execution backend lays a struct out by
laying out each member where it meets it, and needs no order. So the C++ backend meets it the same
way, in the walk it already makes: a name written into a file states the file it needs read first,
and every emitted file includes what its own text asked for. How the name is used says which file --
a class reached through a pointer needs it declared, a base and a struct held by value need it
defined -- so one rule covers all three, and no separate pass works out what a declaration rests on.
MIR states no order among declarations.

The one need text cannot state is a body's: it reaches a member of another unit's object without
writing the class's name. That is a unit reading a part of another's signature, which MIR records
where the lowering resolves it, and the code file includes what that record names.

## Where this departs from earlier records

- `cross-unit-class-translation.md` D1 has the pass minting a unit's own classes trust its context
  and never ask which unit a class belongs to. That held while a scope declared only what its unit
  holds; a package's generic declares specializations other units hold, so every walk of a unit's
  scope asks, of each declaration, whether this unit holds it -- one question, answered by the same
  walk up the front end's tree its D2 concentrates in one place.
- The same record rejects a design-wide table, populated before lowering, of which unit declares
  each class: it relocates a walk every unit can do itself and shares a table among units. The list
  of specializations placed in a design element is a different fact. A unit cannot find it by
  walking its own scope, since the generics are declared elsewhere, and a unit lowering is
  deliberately given no access to the rest of the design; it is the content of the unit, produced
  where the design's units are enumerated, as the list of units itself is.

- `only-a-base-links-two-signatures.md` D2 holds that a type another unit declares is not a
  cross-unit name, because a referrer interns it into its own types. That held while a struct was
  spelled structurally; a struct is now named by its declaration, so a struct held by value is a
  second name a declaration needs defined, beside D3's base, and D5 here gives it D3's answer.

## Rejected alternatives

- **Make each specialization in every unit naming it, merged at link.** clang's answer; it lowers a
  specialization once per naming unit for want of a view of the design that Lyra has, and needs
  merged static storage and class constants on both backends.
- **Keep it in the first unit that needs it.** rustc's share-generics; it needs an order among
  units, which parallel compilation does not have.
- **Keep it in the generic's unit and resolve the reader's own names.** Removes the refusal and
  keeps the unit's content following other units' uses.

## Consequences

- A package generic specialized only on package and built-in types is a unit of its own; one
  specialized on a module's type is held by that module, with static properties per instance.
- `compilation_unit_model.md` invariant 1 counts a specialization among the units.
- `class-declared-in-a-structural-scope.md` Decision 4's specialization lives in the declaring
  unit's design element rather than in the package.

## Cross-references

- `a-class-has-one-identity-in-a-unit.md` -- one identity per class in a unit.
- `class-declared-in-a-structural-scope.md` -- per-instance types and the instance a class takes.
- `specialization-identity.md` -- what identifies a specialization.
