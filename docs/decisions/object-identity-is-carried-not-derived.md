# Object Identity Is Carried, Not Derived

## Date

2026-09-11

## Status

Accepted. Settles how a source-rendering backend projects an object reference, under
`../architecture/object_model.md` invariant 4's split between an object's identity and a reference's
static view. Reverses nothing; it replaces a realization that predates that invariant.

## Why this decision matters

An object reference has to answer two questions that look like one: **which object** it names, and
**what a consumer may assume** going through it. A projection that realizes both as a single typed
pointer makes the first depend on the second, and the target language then supplies an answer to the
first that varies with the second.

The variation is not hypothetical and not uniform, which is what makes it dangerous. It is zero for
the majority of classes and nonzero for the rest, so a projection built on the fused shape is
correct in most designs and silently wrong in the others, with nothing at any layer to catch it.
Identity is what equality, null-testing, storage, and every cross-boundary read are defined on, so
where it is wrong the errors are wrong answers rather than failures.

## What measurement fixed, before any shape was chosen

Three facts about the target language, each measured against the shapes this backend emits rather
than reasoned about.

**Which subobject sits at an object's own address depends on the design's source.** For a class that
implements an interface, if its root declares no virtual method the inheritance chain sits eight
bytes in and the interface at zero; if the root declares one, the chain sits at zero and the
interface thirty-two bytes in. The deciding input is whether the SystemVerilog declared a `virtual`
method -- a property of the design being compiled, not of the compiler. _Consequence: no fixed
subobject can serve as identity, and a typed view cannot be recomputed from identity._

**Not every object a reference can name shares a common emitted base.** The object a process handle
names (LRM 9.7) and the object the execution backend builds are both reference targets and neither
sits in any emitted class hierarchy. _Consequence: identity cannot be typed as a pointer to a common
base, because there is no such base._

**Recovering a reference from the object a body runs on goes through a base subobject.** The
mechanism available for it answers with that subobject's address, which by the first fact is not the
object's own. _Consequence: `this` (LRM 8.11) would mint an identity unequal to the one construction
produced -- at the one site with no existing reference to copy from._

## Decisions

### D1. Identity is established at creation and carried by every reference; nothing derives it

An object's identity is fixed when the object is created, and every reference to it carries that
value unchanged -- through assignment, through conversion to a base or to a contract the class
conforms to, and through conversion to a reference with no class view. No operation recomputes it
from a pointer, from a subobject, or from the static view, because the first measurement says any
such computation varies with the design's own source.

### D2. What a reference observes is its identity; what a copy preserves is the whole reference

Two references are equal exactly when they carry one identity, whatever each was declared as, and a
reference names no object exactly when its identity names none. Those are the observations, and the
view takes no part in either -- there is one comparison, not one per pair of static types.

Copying is not an observation and narrows nothing. A copy carries the identity together with
whatever view the source had, so it is the same reference at the same program point; storing one
into a variable or a member is that copy and behaves the same way. What drops a view is conversion
toward a reference that has none, which is a stated operation and not something a copy does on its
own.

### D3. One storage shape for every object reference; the static view decides how the view is read, never whether it is there

A reference is an identity and a view, and that is its shape at every program point -- whether the
view names a class, a base of one, a contract the class conforms to, or nothing this unit can name.
The view is stored rather than derived, because the first measurement rules derivation out. It is
stored in one place, one width, and one type whatever the static view is, for a reason that has
nothing to do with that measurement:

> A difference in static view must not imply a difference in storage representation.

Two sides hold different static views of one cell **by construction** -- that is what it means for a
name to resolve at elaboration rather than against a promise. A projection in which the static view
chooses the storage type therefore puts two types on one cell, and can then read it only by
arranging for the two to agree, which is not an operation either side states and is exactly what
`../architecture/reference_resolution.md` forbids.

So the static view governs what a program point may **do** with a reference, never what the
reference **is**. A point that names a class recovers that class's pointer from the view exactly,
because the view holds the pointer the reference was formed with. A point with no class to name
reads the identity and does not read the view -- not because the view is absent, but because
reaching a member is what needs the class.

### D4. A conversion between related classes moves no identity and re-derives no relation

The subtype relation is settled before lowering, and a conversion between object references is
already one stated operation. Its projection reproduces that operation: identity passes through
untouched, and the view is formed from the view it came from. The target language's own conversion
rules are used to form the view and for nothing else -- the projection never asks the host compiler
whether the conversion is legal, because that question was answered by the front end and stated in
the IR.

### D5. An object a reference can be recovered from records the identity it was created with

`this` yields a reference to the object a body is running on, and a body holds only a borrowed
pointer to it. The identity therefore comes from the object's own record of it, written when the
object was created, never from the borrowed pointer or from any base it reaches. The record exists
for objects a reference can be recovered from and for no others.

This is the one piece of the projection whose existence is owed to a realization rather than to the
language. Under the terminal lifetime model a body that can reach a safepoint already holds its
receiver as a root, so the reference is the value the body has rather than something recovered from
the object -- and the record goes with the realization rather than being carried into it.

### D6. The class appears where it is used, not where storage is declared

A declaration says a value is an object reference. A use says which class the program point is
reading it through. That split is not a concession to D3 -- it is what the IR says, since a static
view is a property of a program point rather than of the storage, and a projection that writes the
class into the declaration instead is telling the reader something the IR did not.

An untyped pointer spelling stays rejected for the same reason it always was: it is accurate about
the representation and silent about the concept. What a reader must be able to recover is that the
value is a reference to a managed object, and, at each use, which class that use assumes.

## Rejected alternatives

**Keep the fused typed owner and accept the variation.** What today's projection does. Rejected by
the first measurement: the variation is zero for a class that implements no interface and nonzero
for one that does, so the shape is right in most designs and silently wrong in the rest, and the
symptom is a wrong answer rather than a failure.

**Store identity only, and compute the view at each access.** Rejected by the same measurement --
the offset from identity to a view is not a constant the projection knows, and for a view naming a
contract the class conforms to there is no conversion from identity at all without the concrete
class.

**Store the view only where it cannot be computed.** Rejected: the condition is whether the design
declared a virtual method, so the projection would answer "where does a reference's view live" two
ways, chosen by a fact about the source. One shape answers both.

**A distinct reference type per static view, so a declaration names the class.** The spelling is
better and it is what this entry first chose. Rejected once the emitted text was read: a cell a
producer declares under one static view and a consumer reaches under another then carries two types,
and the read is correct only while the two layouts happen to agree. The failure is silent, and the
shapes it is wrong for are the ones nobody writes a case for. The class is not lost by rejecting
this -- it moves to the use, which is where the IR puts it.

**Publishing a read and a write beside each cell, so the class stays in the declaration.** The
producer, which knows the class, emits an erased pair that a consumer calls; this is the shape a
scope's exports already use, and its safety argument is written down. Rejected because it keeps two
representations and adds a protocol to convert between them, where one representation needs neither.
It also has no demand signal: a producer does not know who will reach into it, so it would publish
the pair for every reference-typed member against the chance that someone does.

**Give the identity a common emitted base so it can be a typed pointer.** Rejected by the second
measurement: two of the objects a reference can name are not emitted classes at all, so the base
would have to be retrofitted onto the runtime library for the sake of a spelling.

## Revisit condition

D3 stores a view because deriving one is ruled out, and deriving one is ruled out because a view may
name a contract the class conforms to, which the target language reaches through a base of its own.
A projection that instead reaches such a view the way the execution backend does -- by a coordinate
rather than by a language-level conversion -- has no such views to form, and the inheritance chain
then sits at the object's own address unconditionally. At that point a reference is its identity and
nothing else, and D3's view word should be re-derived rather than kept out of habit.

Nothing in that revisit touches the rule D3 rests on. One storage shape for every static view holds
whether or not a view word is part of the shape.

## Consequences

- In this projection, a reference is three words while ownership is realized by sharing, against two
  before, paid per reference-typed variable and per reference-typed member. The number is a property
  of how this backend spells a reference and of the staging it currently rides; no IR states it, and
  the other backend's answer is its own.
- Equality between references of different static views becomes expressible, and correct, which the
  fused shape made unspellable rather than wrong.
- A cell holding a reference has one type on both sides of a compilation-unit boundary, so reaching
  it needs no agreement between two layouts and no protocol to convert between them.
- The class a program point assumes moves out of declarations and into uses. What a reader loses at
  a declaration they gain at every access, and what the emitted text now says matches what the IR
  says, which it did not before.
- The reference type family collapses to one type. A projection that had a type per class has a
  type, and the class becomes an argument at the sites that need one.
- The identity record on an object is added by this decision and deleted by the terminal lifetime
  model; it is not a shape to build on.

## Cross-references

- `../architecture/object_model.md` -- invariant 4's split between an object's identity and a
  reference's static view, which this entry realizes, and invariant 2's fourth identity
  representation.
- `../architecture/object_lifetime.md` -- the terminal lifetime model, whose receiver-rooting
  invariant is what removes D5.
- `object-model.md` -- Decision 3, precise tracing over reference counting, and the shared-ownership
  staging this projection currently rides.
- `structural-access-on-an-opaque-object.md` -- what happens when a source name continues past such
  a reference, and the coordinate the revisit condition above refers to.
- `interface-conformance-realization.md` -- the conformance realization D3 depends on, and whose
  replacement is the revisit condition.
- `../architecture/backend_contract.md` -- a type's target representation comes from one
  type-mapping dispatch, which is what makes the spelling in D6 this backend's own choice.
