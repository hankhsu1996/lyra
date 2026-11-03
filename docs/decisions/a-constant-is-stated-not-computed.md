# A constant is stated by the program, not computed while it runs

Date: 2026-09-17 Status: accepted. D4's example of a select taking its receiver's coordinate system
as an operand no longer holds: [a-select-names-a-position](a-select-names-a-position.md) reads that
system where the select is lowered, and the operand pattern D4 states stands for the conversions and
factories that still take one. That record's P3 is this entry carried one step further: an integral
operation over constants is itself a constant of the unit.
[a-packed-value-is-its-words](a-packed-value-is-its-words.md) D2 narrows both for a value too large
to lie in what holds it. A fill, a default or a small number widened to such a type is stated as the
extension of the bits that differ, and is carried out where it is evaluated. Revised 2026-11-02: D1
and D2 now hold of every value settled before the program runs, not of integral values alone; D3 is
replaced, since what the unit holds is the value and not how to build it; D6 is added.

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

The pool holds every value settled before the program runs, whatever reads it: the integral values
the bodies were written with, the member tables of the enumerations they ask about, where each
operation that can fail was written, and every table a class's definition points at. A class's
definition is such a value as well, but it is held by the class rather than by the pool, because
every unit names it by the class and its address is one for the whole program.

### D2. A constant's identity is its value and its type together

The same bits at two types are two constants. The type decides how many bits are read, whether they
are read as signed, whether an unknown plane is present at all, and how a structure's parts are laid
out, so folding them would hand a use a value of the wrong shape. The pool is an interner over that
pair, in the discipline [mir-type-interning](mir-type-interning.md) fixes for types: the key is
every field the entry carries, and the entry carries nothing that is not part of what the constant
is. Two tables holding the same entries are therefore one constant, however many makers asked for
it.

Bits reaching the pool are canonical first -- one word per 64 bits of the declared width, the top
word's bits above that width cleared. Two spellings of one value must not become two entries, and a
consumer must never read bits the type does not have.

### D3. What the unit holds is the value, never how to build it

A constant is stated as what it is: an integral value's bits, a machine integer, a null, a text, an
address, a structure of the library a part per member, an array, or a view of an array. The address
names what it points at the way a reference in a body names it -- another constant, a class's
definition, a dispatch table, a variable, code -- so nothing in a value runs, and nothing reading
one evaluates it. The form is clang's `APValue` and rustc's `ConstValue`: a value apart from the
expression that denotes it and from the bytes a target lays it out as, carrying no type of its own,
since what holds it states the type.

Below MIR the same holds of an address. The lowering to the execution model translates what an
address names once, into one set of things a target can take the address of, and a constant's
pointer, an operand and a slot of a class's table all name one of them, as rustc's operand and the
pointers inside a constant's memory both point into one `GlobalAlloc`. A class's definition is named
by the class there as in a body, never by a symbol composed for it, which would be the same entity
spelled two ways.

An earlier form of this entry said the expression that builds a constant is derived from the entry
on demand and never stored, because storing it would put an expression tree in the pool's key. That
was written when the only constant was an integral value and "how it is built" meant the factory
call a target writes. The tables a class's definition points at were then stored exactly that way,
as expression trees, and the lowering to the execution model evaluated each, refusing every kind of
expression that was not data. Its reason holds and selects the other way: the key is the value, and
a target writes a value from the value and its type.

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
storage duration that language has for it, and a backend emitting a machine model says it as data of
its module. Each does so by one function from a value and its type to the target's form, which knows
no kind of constant: a structure is written by its members, an array by its elements, an address by
what it names.

**The lifetime a backend must then answer for is its own, not a fact MIR states.** Where values
cross into generated code as handles owned by the call that made them, keeping a constant's address
past that call is a defect, and the backend that has that boundary is the one that repairs it. A
target whose values are its own has no such question to answer, which is why the answer cannot sit
upstream of both.

### D6. A constant is each artifact's own data, and a target keeps a shared definition meaning one

A constant has no linkage of its own: every artifact holds the constants its own code and its own
definitions name. A definition every body of a unit states alike is kept in place of the others, so
whatever it names has to mean the same in each, and how a target makes it so is that target's, as
[a-unit-is-compiled-to-one-body-per-distinct-code](a-unit-is-compiled-to-one-body-per-distinct-code.md)
D3 says of every definition. On the execution backend nothing is needed: a `weak_odr` definition may
read its own module's private data, since only that the copies behave alike is asked, which is how
clang's and rustc's constants are each module's private data. C++ asks that every name in the kept
definition denote one entity in every copy ([basic.def.odr]/16), which a constant of one file's own
is not and a static of an inline definition is ([dcl.inline]), so such a definition spells each
constant it names in place, as a static of its own. A static whose initializer is a constant
expression is initialized before anything runs ([basic.start.static]/2), so reaching one then builds
nothing. Each static is named by where it stands in that spelling, so the text is the same in every
body whatever position the unit holds the constant at.

What MIR checks is the other half: a definition every body states alike names nothing one body has
alone, through every constant it reaches.

## Consequences

- An occurrence of a constant is a reference, so the emitted artifact holds one definition per
  distinct value however many times the source wrote it, and a dump shows each value once in a table
  rather than at every use.
- A constant is an operand rather than a call, which is the form the optimizer can act on at all.
  This entry does not fold anything; it makes folding reachable, which is what
  [conversion-folding](conversion-folding.md) assumed was already true.
- No lowering decides whether part of a constant is data, and none refuses one: every value is data
  by what it is.
- An enumeration's member table, a location and a table a definition points at are each a structure
  of the library, so a target writes each as it writes any other, and the library's own type for a
  member table is a record of two views and two numbers rather than a type each table instantiates.
- The pool is per unit, so it introduces no cross-unit dependency and nothing about it has to be
  agreed between two artifacts.
- A definition every body of a unit states alike carries in its own text each constant it names, so
  the C++ text of that definition depends only on what it states, never on where the unit holds a
  constant; a definition of one body names the constants its code file defines.
- A constant is not a place. Nothing writes through one, and a place over one opens at its address,
  through which it is only read.

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

- **A constant as an expression tree, evaluated where the program is lowered to its execution
  model.** It was how a class's tables were stated. The tree admits every operation the language
  has, so the lowering reading it refused all but literals, names, addresses and composites, and a
  target writing source wrote it through the general expression render, which knows nothing of data.
  A form whose readers refuse most of what it admits is stating a narrower thing than it can hold.

- **One pool per kind of constant.** Integral values, member tables and locations were each a pool
  of their own, each with its own reference node, its own symbol and its own writer in each backend,
  and the backends told them apart where only the type differed.

- **Name a constant every artifact shares by a digest of its content.** It was designed first and
  dropped before it was built: the condition it served is the C++ one, that a kept definition names
  one entity in every copy, and C++ already answers it in the language with a static of an inline
  definition.

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
