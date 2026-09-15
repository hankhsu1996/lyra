# A computation a type owns has no object, so the unit's namespace owns it

Date: 2026-09-13 Status: accepted

## Status

Accepted. Revises point 3 of [rendering-a-value-by-its-type](rendering-a-value-by-its-type.md),
which stated where such a callable is homed as a mechanism ("the class the print site lowers into")
rather than as the constraint it was reaching for. Reverses nothing else.

## Why this decision matters

Some of what a value reads as is decided by its declaration rather than by the value: an enumeration
answers with the name it declares for a value and with its neighbour in the declared order (LRM
6.19.5), and `%p` prints an aggregate as the assignment pattern its declaration names the members of
(LRM 21.2.1.6). Each is realized as a function the compiler synthesizes once per type, whose body is
generic MIR.

Where that function lives was answered by whatever was in hand at the first site that needed one.
The print site and the enumeration method are both lowered from inside a design element, a design
element's body is lowered into a class, and a class owns a callable arena -- so the function was put
there, and the three sites that build one refused when the lowering had no class. Every one of them
said "in a package context", which is the context they guessed rather than the condition they
tested.

**A real design stopped there and nowhere else.** Both constructs are ordinary in package code, and
two diagnostics stood between that design and a simulation.

## What the standard requires

**LRM 6.19.5** gives `name`, `next` and `prev` to a variable of an enumerated type, and **LRM
21.2.1.6** gives `%p` to a value of any type. Neither is stated over an object.

**LRM 26.3** lets a package declare subroutines, and **LRM 3.12.1** gives the compilation-unit scope
"any item a package may hold". Both are namespaces with no instance, so a subroutine declared in
either has no object to be applied to and none to reach through. The two constructs are therefore
reachable where nothing encloses them, and the language says so twice.

## What other systems do, and where our conditions differ

The question worth asking is who **owns** the synthesized function, not what it computes.

| System | Synthesized per type               | Which component, at which stage                                               | Owner                                                                         | Receiver                                           |
| ------ | ---------------------------------- | ----------------------------------------------------------------------------- | ----------------------------------------------------------------------------- | -------------------------------------------------- |
| Rust   | `#[derive(Debug)]`'s `fmt`         | macro expansion, before type checking                                         | items "appended following the input item within the same **module or block**" | the item is the module's; the method takes `&self` |
| Go     | a type's equality and hash         | `cmd/compile/internal/reflectdata`, while compiling the package that needs it | a **package-level** symbol, `types.TypeSymLookup(".eq." + sig)`               | none: `(p, q unsafe.Pointer) bool`                 |
| Swift  | a protocol witness table           | the module that declares the conformance                                      | module level                                                                  | none                                               |
| Java   | an enum's `values()` / `valueOf()` | javac, implicitly declared (JLS 8.9.3)                                        | **static members of the class**                                               | none                                               |

Three of the four put it in a namespace. Java is the one that does not, and Java has no
namespace-level function at all -- a condition SystemVerilog does not share, and neither does MIR,
whose stated peers are C++, Rust and Python.

**Where our conditions differ, and this sentence decides the design: every one of them mints a name
for the synthesized entity, in an identifier space the source language cannot spell** -- Go's
leading `.`, Rust's hygiene, the Itanium ABI's reserved range. Lyra does not mint one, because a
program-wide symbol is parts under a category rather than a spelling
([a-name-is-a-relation-not-an-identity](a-name-is-a-relation-not-an-identity.md)) and because a
SystemVerilog identifier admits every printable non-space character (LRM 5.6.1), leaving no range
reserved to mint from. So the entity is reached by its position and each backend mints its own
target-side spelling. That is the same split taken one layer earlier: the symbol a backend composes
is still a minted name, and it is minted where the target's rules are known.

## Decision

**A computation a type owns takes no object, so the compilation unit's namespace owns the function
that performs it, and its identity is the position that function's declaration sits at.**

### D1. The owner is the unit, in every context

A type-associated function takes the value and nothing else. A class is therefore never its owner --
not in a package, where there is none, and not in a design element, where there happens to be one.
Every unit has a namespace whether or not it has classes, so what encloses the site that needs one
decides nothing, and there is no case to refuse.

This is the rule
[type-associated-storage-is-the-declarers](type-associated-storage-is-the-declarers.md) reached on
the storage axis, read on the code axis: what no instance holds belongs to the namespace.

### D2. Identity is the position; a name is a relation over it

`mir.md` invariant 7 states this for a member: identity is the position it occupies, and being
reachable by an identifier is a relation its owner holds over that position, stated only where the
source declared one. A callable is the same. A body the compiler synthesized simply does not take
part in that relation, which is an answer rather than a case to work around -- the namespace's own
storage pool already reads this way, because it holds the static-lifetime cells of the unit's
subroutines and those answer to nothing.

### D3. Inside the unit a body is named by position, outside it by what the namespace published

A call written in the unit that owns the body holds the arena, so it names the position. A call from
another unit has only the identifier that namespace published, which is the whole of what a unit
publishes, so it names that. The two are total and do not overlap, and a synthesized body is
unreachable from outside for the same reason it needs no name: nothing published it. This mirrors
the storage axis exactly, where an intra-unit reference already names a position and a cross-unit
one a published identifier.

### D4. Which reading of which type is keyed by the SystemVerilog type

The declaration is what decides, and a lowering answers facts it then stops carrying: a packed
tagged union reads as its tag and the member that tag names where an untagged one reads as its first
member, while both project onto one vector below the front end. Keying there is what lets MIR keep
one packed union type. MIR never needs to tell the two apart, because it is handed two bodies and
not one body plus a discriminator.

### D5. A backend qualifies a unit's namespace from the global scope

A class carries its own name inside its body, and a unit's namespace carries the same identifier as
the class at the root of that unit's hierarchy. An unqualified reference from inside such a class
therefore names the class, and the namespace behind it is unreachable. Reaching into a unit's
namespace is qualified from the root; opening one is not.

## Rejected alternatives

- **Home the function on a class where there is one and on the unit where there is not.** A
  predicate selecting between two output shapes, which every consumer of the target then carries.
  The refusal it replaces is the same defect stated as an error message.

- **Mint a name for the synthesized body and reach it through the namespace's published
  identifiers.** What Go, Rust and C++ all do -- and what
  [a-name-is-a-relation-not-an-identity](a-name-is-a-relation-not-an-identity.md) rejects, because
  such a word sits in the same name space as the unit's own subroutines. Their reserved ranges have
  no counterpart in a language whose identifiers admit every printable character.

- **Give MIR a target naming the type and the reading, the way a witness table is keyed.** The shape
  Swift and Haskell take, and the one that would make "one per type" structural rather than held by
  the lowering. It requires MIR to enumerate the readings -- an assignment-pattern text, a declared
  name, a step -- which are LRM 6.19.5 and 21.2.1.6, so the source language would be shaping MIR's
  vocabulary against `mir.md`'s Purpose.

- **Describe the type as data and have the runtime walk it.** Rejected already by
  [rendering-a-value-by-its-type](rendering-a-value-by-its-type.md), and independently here: it
  moves the decision below the backends, where `mir.md` invariant 10 says no consumer decides.

- **Synthesize into the unit that declares the type and reach it across the boundary.** A unit's
  emission is a function of its own contents and the signatures it consumes
  (`../architecture/compilation_unit_model.md` invariant 11), and a signature carries the
  declarations a unit publishes -- which a synthesized body is not. Each unit synthesizes its own
  from the type it took into its own storage.

## Consequences

- An enumeration's `name` / `next` / `prev` and `%p` of a declared type answer in a package
  subroutine and at compilation-unit scope, on both backends. Three refusals are removed rather than
  made conditional.
- A unit's namespace holds bodies the source never named, so what spells one is a position where
  nothing else does, and the fallback that could name no such body is gone.
- An intra-unit call to a package subroutine names a position rather than resolving the unit's own
  published identifier against itself.
- The three per-reading caches the lowering kept become one, keyed by the reading and the type.
- The C++ backend emits every callable of a unit's namespace as a declaration before the unit's
  classes, because a class body may now call one.

## Cross-references

- LRM 3.12.1, 5.6.1, 6.19.5, 21.2.1.6, 26.3
- The Rust Reference, Procedural macros (derive output is appended within the same module or block)
- Go, `cmd/compile/internal/reflectdata/alg.go` (`hashFunc` / `eqFunc`)
- The Java Language Specification 8.9.3 (an enum's implicitly declared methods)
- [rendering-a-value-by-its-type](rendering-a-value-by-its-type.md) -- the rendering this places.
- [type-associated-storage-is-the-declarers](type-associated-storage-is-the-declarers.md) -- the
  same rule on the storage axis.
- [a-name-is-a-relation-not-an-identity](a-name-is-a-relation-not-an-identity.md) -- why no name is
  minted. Its rule is about program-wide symbols; this decision is about an identity inside one
  unit, which that entry does not reach.
- `../architecture/mir.md` invariant 7 -- identity is the position, a name is a relation over it.
- `../architecture/callable.md` invariant 2 -- a type-associated function has no receiver.
