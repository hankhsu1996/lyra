# A Class Has One Identity in a Unit

## Date

2026-10-05

## Status

Accepted.

## Why this decision matters

A hierarchical name may leave the instance it is written in and reach another instance of the same
module (LRM 23.6), and a signature another unit published may name a class of the unit reading it --
the type of a member another unit holds one of this unit's instances by. Either way the name arrives
as what every cross-unit name is, the unit and the class, and nothing about the arriving form says
that the unit is this one. Left as it arrives, one class of a unit is spelled two ways inside that
unit: as its own class where its own scope names it, and as another unit's class where the
roundabout name does -- its type, its fields and its methods each in both forms, which both backends
happen to realize as one symbol and nothing checks.

## Survey

- clang has one `RecordType` per class in a translation unit, whether its definition is present or
  only declared; `CodeGenTypes::ConvertRecordDeclType` (clang/lib/CodeGen/CodeGenTypes.cpp) keys the
  LLVM type by the canonical tag type and leaves it opaque until a definition is seen. Reaching a
  member is what requires the definition (`RequireCompleteType` in member lookup,
  clang/lib/Sema/SemaExprMember.cpp).
- rustc has one type kind, `TyKind::Adt(AdtDef, args)`, whose identity is `DefId{krate, index}`; the
  local crate is crate number 0, not another kind (rustc_span/src/def_id.rs). What a struct is, is
  one query, `adt_def`, with two providers: the local one from HIR and the extern one from metadata.
  A reader decoding metadata maps the stable cross-crate identity back to a local `DefId` where the
  item turns out to be its own.
- Where Lyra differs: a unit names another unit's class by name, because a name is the only identity
  that survives independent compilation, and a referrer lays out what it reaches from the signature
  rather than reading the declarer's answer. Neither condition asks for a second type kind.

## Decisions

### D1. A class is named by its identity, and a class of this unit is always intra-unit

An object type and a field target name their class by one identity, intra-unit or external-unit, and
which one is a fact about the class: a class this unit declares is named by its id however the name
reaching it was written. Translating a name to that identity is the lowering's one job here -- a
class of this unit published under the name resolves to this unit's class, any other to the other
unit's -- as a struct and a namespace callable already resolve.

### D2. What is known of a class is one question with two answers

Of a class of this unit, what is known is the declaration this unit states and the layout its own
shape gave what was published; of another unit's, the record of what that unit published. A consumer
asks the class's identity and is answered by one of the two, as rustc's two providers answer one
query. Naming a class is not depending on it: a dependency is what is read of another unit's record,
so a class of this unit is never a dependency of it.

### D3. Members are named alike; callables keep their two coordinate spaces

A field of a published class is at the same slot whichever unit names it -- the published members
are the class's first fields, in order -- so a field target is the class's identity and the slot. A
callable is not: this unit names its own by position, including bodies the lowering made that answer
to no name, while another unit's is named by what that unit published. A published scope class
therefore reserves one method per published subroutine, in the signature's order, before any body
lowers, so a body reaching another instance of its own unit calls the method by position.

### D4. What holds objects of a loop generate stays the scope every block is

Which blocks of a loop share a body is decided by lowering the bodies, and what a unit publishes
does not depend on its bodies -- an edit to a body never changes what a referrer compiles against.
So the member holding what a loop built holds the base every block extends, each block published
under its own class name, as a C++ header keeps a pointer to a base whose derived classes its
implementation defines. A set of instances of other units is different: which unit each element is
was settled by elaboration, and the signature states it, so a set of one kind is held as that kind's
class.

## Rejected alternatives

- **Keep two object type kinds and resolve this unit's names to the intra-unit one.** Removes the
  double spelling and keeps a vocabulary in which a struct is one type and a class two, so every
  consumer of a class type, a field and a property branches on which.
- **Name every class by unit and name, this unit's included.** A class the lowering builds for its
  own use answers to no name.
- **Hold a loop generate's blocks as one class where they share one body.** Puts a fact of the
  bodies into what the unit publishes.

## Consequences

- The MIR verifier refuses a unit that names a class of its own as another unit's -- in a type, in a
  record of what a unit published, or in what it consumed -- so the rule is checked rather than
  stated.
- A unit's published scope classes are minted, under every name each answers to, before any type
  translates, since a type another unit's signature states may name one.
- The execution layer keeps telling a class this unit defines from one it only declares, as LLVM
  tells a defined struct from an opaque one; that is what the two forms of the identity are below
  MIR.

## Cross-references

- `interface-port-binding.md` D6 -- naming a class is not depending on it.
- `published-member-placement.md` D4 -- both sides lay a published member out through one function.
- `cross-unit-class-translation.md` -- how AST-to-HIR tells an SV class of this unit from another's.
