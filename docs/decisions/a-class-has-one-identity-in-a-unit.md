# A Class Has One Identity in a Unit

## Date

2026-10-05

## Status

Accepted. Revised 2026-10-23: D3 says where a virtual behavior of this unit's class, reached through
another unit's lineage, is named as its own. Revised 2026-10-24: one identity and one answer for a
class of this unit and another unit's alike (D1, D2), every member in one coordinate space (D3).

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

### D1. A class is named by one identity, whichever unit defines it

A class is known across units by its stable key, the declaring unit and the class's published path,
and inside a unit by the handle the unit's class registry gives that key the first time it enters --
the one place a key becomes an identity, as rustc maps a crate number read from metadata to its own
where it decodes it. A key naming this unit lands on the class it declares, so a name reaching a
class of this unit through another instance or through a signature naming this unit is that class.
Every reference -- an object type, a property, a method, a static property, a behavior, a
construction -- names the class by the handle, and MIR and LIR keep it: a class another unit defines
is an entry of the same registry, declared with what that unit published, and whether this unit
defines the class is a fact of the entry. The same holds of a covergroup's class and of the class a
scope of the design hierarchy is published as. A record and a member of a unit's namespace are named
in HIR by their key, one form for this unit's and another's, and from MIR down by one entry of the
reader's tables.

### D2. What is known of a class is one answer, with two providers

What a class declares -- what it extends and implements, whether it is an interface class, its
properties, its static properties, its constructor's formals and each method's prototype and part in
dispatch -- is one type for a class of this unit and another unit's: computed where this unit
interns its own, and read back from the signature for another unit's, every key in it turned into
the reader's handles once, where it is decoded. Publishing a class is projecting its declaration
onto the signature's keys, and reading one is the inverse, so the two cannot describe different
classes. A class of this unit also has bodies, which only this unit holds.

Naming a class is not depending on it. A dependency is what the unit's finished MIR reads of a
declaration another unit made: a field it accesses, a method it calls or dispatches through, a class
it constructs, a value whose layout it holds. A read of a class closes over the classes it extends
and implements, since its layout and its tables read theirs. A handle to a class reads nothing, as a
pointer to an incomplete type does not, and neither does a `$cast` to it, which names only its
definition's address, as Itanium C++ names a class's type information by its mangled name alone. The
set is derived by one walk over that MIR once lowering is done, as a C++ compiler writes out the
headers a translation unit read (`-MD`), so the lowering records nothing and no read it adds can go
unrecorded. A class of this unit is never a dependency of it.

### D3. Every member is named in one coordinate space

A member is named by its class's handle and its position in that class, whichever unit declares the
class. A property of another unit's class is at the slot that unit gave it, its published properties
being the class's first fields in order; a method at the position its declaration publishes it; and
a behavior by the class that introduced it and that method. Which class introduced the behavior an
override replaces is found once, by the unit declaring the overriding class, along its own lineage
as the front end resolved it, and published, so a reader never re-derives it. A lineage another unit
published may lead back to this unit -- a specialization of a generic this unit declares, extending
one of this unit's classes -- and the introducing class is then this unit's own by D1, with no
second step. A published scope class reserves one method per published subroutine, in the
signature's order, before any body lowers, so a body reaching another instance of its own unit calls
the method by position.

### D4. What holds objects of a loop generate stays the scope every block is

Which blocks of a loop share a body is decided by lowering the bodies, and what a unit publishes
does not depend on its bodies -- an edit to a body never changes what a referrer compiles against.
So the member holding what a loop built holds the base every block extends, each block published
under its own class name, as a C++ header keeps a pointer to a base whose derived classes its
implementation defines. A set of instances of other units is different: which unit each element is
was settled by elaboration, and the signature states it, so a set of one kind is held as that kind's
class.

## Rejected alternatives

- **Keep one form for this unit's class and another for another unit's, resolving this unit's names
  to the first.** Removes the double spelling and keeps a vocabulary in which a struct is one type
  and a class two, so every consumer of a class type, a field, a method and a behavior branches on
  which, and each new kind of declaration repeats the pair and its rebasing.
- **Name every class by unit and name, this unit's included.** A class the lowering builds for its
  own use answers to no name.
- **Hold a loop generate's blocks as one class where they share one body.** Puts a fact of the
  bodies into what the unit publishes.

## Consequences

- The MIR verifier refuses a declared class that names this unit, one declared twice, and one
  holding code or data of its own, so every walk emitting definitions can skip what another unit
  defines.
- A class only named here -- a handle's type, never reached into -- is declared with nothing of what
  it declares, as an incomplete type is.
- Every class, covergroup class and scope class a unit names takes its identity before any type
  translates, since a type another unit's signature states may name one.
- Below MIR the declared entry stays an entry, telling a class this unit defines from one it only
  declares as LLVM tells a defined struct from an opaque one.

## Cross-references

- `interface-port-binding.md` D6 -- naming a class is not depending on it.
- `published-member-placement.md` D4 -- both sides lay a published member out through one function.
- `cross-unit-class-translation.md` -- how AST-to-HIR tells an SV class of this unit from another's.
