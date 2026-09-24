# A constant is stated by the program, not computed while it runs

Date: 2026-09-17 Status: accepted. D4's example of a select taking its receiver's coordinate system
as an operand no longer holds: [a-select-names-a-position](a-select-names-a-position.md) reads that
system where the select is lowered, and the operand pattern D4 states stands for the conversions and
factories that still take one. That record's P3 is this entry carried one step further: an integral
operation over constants is itself a constant of the unit.

## Context

MIR has literal forms for machine scalars -- an integer, a float, a bool, a string -- and none for a
value of the language it is expressing. A SystemVerilog integral constant lowered to a **call**: the
bits went into a machine-integer literal and that literal was handed to a library factory, so `8'd5`
arrived in MIR as `kFromInt(5, <type>)` and arrived in the emitted program as a construction
performed wherever control reached it.

That is the one shape the layer is least able to carry. `mir.md` states that the primitive set is
shaped for the downstream optimizer -- primitives "that its optimizer can fold, propagate, and
pattern-match across" -- and states in the same paragraph that a runtime helper call is opaque to
exactly those, "which LTO mitigates but does not eliminate". So a constant, which is the most
foldable thing a program contains, was the one thing expressed in the form nothing can fold.

**The cost was being deferred to an optimizer, and the deferral is written down.**
[conversion-folding](conversion-folding.md) settles that a conversion over a literal is lowered like
any other -- rightly, on symmetry -- and rests the cost on "the LLVM backend, the load-bearing
target, folds the constant conversion for free". Measured on 2026-09-17, it does not, and neither
does the other one:

| representative compute block, five interleaved rounds                         | stated once | built at each use |
| ----------------------------------------------------------------------------- | ----------- | ----------------- |
| design compiled unoptimized (the default)                                     | 1.00        | 1.21              |
| design compiled optimized                                                     | 1.00        | 1.23              |
| design compiled optimized, value layer's definitions visible to the optimizer | 1.00        | 1.29              |

The third row is the one that decides this entry: with the factory's body fully visible, the
optimizer still does not recover the cost. Two reasons, and both survive any amount of visibility.
The descriptor a constant names is a dynamically initialized object, so the construction is not a
constant expression in the target language's sense and folding cannot reach it. And an optimizer
hoists to a loop or to a function; only the artifact reaches the program, so a body entered a
thousand times builds its constants a thousand times either way.

## Decision

**A value fixed before the program runs is stated by the program's own artifact, once per artifact,
and reaching it costs a read.**

### D1. A unit holds the constants it was written with, and an occurrence names one

The compilation unit carries a pool of constants beside the pool of types it already carries. A
source occurrence does not build a value; it names the entry holding it. This is the relation a
type's run-time description already has to the uses that name it, one level over: that names what
describes a value, this names a value.

### D2. A constant's identity is its bits and its type together

The same bits at two types are two constants. The type decides how many bits are read, whether they
are read as signed, and whether an unknown plane is present at all, so folding them would hand a use
a value of the wrong shape. The pool is an interner over that pair, in the discipline
[mir-type-interning](mir-type-interning.md) fixes for types: the key is every field the entry
carries, and the entry carries nothing that is not part of what the constant is.

Bits reaching the pool are canonical first -- one word per 64 bits of the declared width, the top
word's bits above that width cleared. Two spellings of one value must not become two entries, and a
consumer must never read bits the type does not have.

### D3. How a constant is built is asked for, never stored

The expression that builds a constant is derived from the entry on demand, the way a type's
description is derived from the type. It is not a field on the entry: storing it would put an
expression tree in the pool's key, and the key is what makes two occurrences one entry.

One shape serves both, because a description of a type and a constant of the program are the same
kind of thing -- an expression with no statements, settled before the run, named by every use.

### D4. A fact a declaration supplies is named the same way whatever family it belongs to

The same relation holds one step over, for what a value's declaration says rather than for the value
itself. An operation that names a position inside a value needs the coordinate system the receiver
was declared with, and the value cannot supply it: a slice of an array carries no declared range,
and an aggregate's flat base carries no dimension stack. So it arrives as an operand read off the
receiver's static type, stated once by the unit and named by every use.

**What the unit holds is the description, not the type it was read off.** A type is where the
description is found; it is not what the description is, and no consumer of one can tell which
declaration it came from -- two arrays of different elements that span the same declared range want
the same answer from it. So the entry is the description itself, and two declarations saying one
thing reach one entry.

That also settles which families exist as a closed set the compiler must answer for rather than as a
test each answer repeats. Telling them apart happens once, where the description is taken; what a
family's description is a value of, and how it is built, are answers over that closed set, so a
family admitted later cannot be half-handled.

**It is one mechanism across the families, not one per family.** An integral type's description is
its dimension stack with its signedness and state domain; an unpacked array's is its declared range.
Which of the two a use is naming follows from the description's own runtime type, so nothing at the
use decides it and nothing downstream asks which family it came from.

An earlier form of this entry said one description per _type_, on the ground that an identity has to
be derivable from something. It does, and the description is something: interning by what is said
derives an identity just as well, folds two declarations that say the same thing onto one entry, and
leaves nothing needing the type back.

A family that took the same fact apart instead -- pushing a declared range's two endpoints into the
argument list, leaving every access to put them back together -- had the fact on the type all along
and was rebuilding it per access. The endpoints are not what an access wants; what it wants is the
answer the range gives, which is where a source coordinate lands.

### D5. Each backend realizes what the unit holds in its own terms, and the lifetime follows

What "the unit holds this" means is the target's answer. A backend emitting source says it with the
storage duration that language has for it. A backend emitting a machine model says it with a
module-level cell filled by the first use that finds it empty.

**The lifetime a backend must then answer for is its own, not a fact MIR states.** Where values
cross into generated code as handles owned by the call that made them, keeping a constant's address
past that call is a defect, and the backend that has that boundary is the one that repairs it --
here by taking the built value into storage that lives as long as the run. A target whose values are
its own has no such question to answer, which is why the answer cannot sit upstream of both.

## Consequences

- An occurrence of a constant is a reference, so the emitted artifact holds one definition per
  distinct value however many times the source wrote it, and a dump shows the bits once in a table
  rather than at every use.
- A constant is an operand rather than a call, which is the form the optimizer can act on at all.
  This entry does not fold anything; it makes folding reachable, which is what
  [conversion-folding](conversion-folding.md) assumed was already true.
- The pool is per unit, so it introduces no cross-unit dependency and nothing about it has to be
  agreed between two artifacts.
- A constant is not a place. Nothing writes through one, and the paths that ask a reference where
  its storage is answer that it names none.

## Rejected alternatives

- **Leave it to the optimizer.** The measurement above is the answer: unrecovered at three
  optimization levels, including one where the factory's body is fully visible. The general form is
  that an optimizer reconstructs a fact the compiler already had, and only within a loop or a body
  -- so the reconstruction is both unreliable and narrower than the fact.

- **Have the backend hoist a call whose operands are all literals.** A renderer inspecting a node's
  operands to decide the output form is the peephole [conversion-folding](conversion-folding.md)
  removed, for the reason it gives: a render makes no optimization decisions. It also serves one
  backend, leaving the other to answer the same question separately.

- **Give every occurrence its own entry and skip the interning.** The artifact then holds one
  definition per occurrence rather than per value, which is the same cost moved rather than removed
  for a design that writes `0` in a thousand places. Interning is also what makes an entry's
  identity a property of the value rather than of where it was written, which is what lets two
  consumers agree without either inventing a rule.

- **Keep the bits on the expression and let each backend pool them.** Two backends would invent the
  same table independently, and a table one of them invents is a structure the IR does not state --
  which is the shape [value-construction-forms](value-construction-forms.md) names: a render that
  has to compose something the IR never said is looking at something the IR should have said.

## Cross-references

- `../architecture/mir.md` -- the primitive set is shaped for the downstream optimizer, and a
  runtime helper call is opaque to it.
- [mir-type-interning](mir-type-interning.md) -- the same pool discipline for types, and the key
  rules this entry follows.
- [conversion-folding](conversion-folding.md) -- the entry whose stated reason this measurement
  corrects; its decision stands and its consequence does not.
- [value-construction-forms](value-construction-forms.md) -- a construction states which form it is,
  and a backend composes nothing.
- [integral-representation](integral-representation.md) -- what a value carries about itself, which
  a constant carries too.
