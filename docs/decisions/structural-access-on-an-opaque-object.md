# Structural Access on an Opaque Object

## Date

2026-09-11

## Status

Accepted. Answers the open target category `hierarchical-reference-routing.md` D5 leaves for a
target whose access surface no existing protocol covers, and extends `reference_resolution.md`
invariant 7 with what a route seals when its leaf is an object reference. Reverses nothing.

## Why this decision matters

A unit compiles once per specialization, and a name reaching past another unit's signature resolves
at elaboration. Where such a name lands on an object reference, the class is a property of the
instance rather than of the specialization. IEEE 1800-2023 8.13 gives classes single inheritance and
6.22 puts a data type identifier's scope inside the hierarchical instance scope: "each instance with
a user-defined type declared inside the instance creates a unique type", so two instances of one
module do not share the class, and the standard's own remedy is to declare it in a package instead.

Carrying such a reference is already covered: it is nullable, identity-comparable, copyable, and
assignable with no class view, because none of those operations reads anything the class holds.

The next character in the source is what is not covered. `holder.h.tag`, `holder.h.Drive()`, and
`holder.q[0].tag` are legal SystemVerilog, and each has to know which storage position or which
dispatch position the source name picks out. That is instance knowledge, needed in the middle of a
body that compiles once for every instance.

The failure mode if this is answered at the wrong layer is quiet. A position computed from the wrong
class still compiles, still links, and reads whatever sits at that offset in some other class's
layout.

## The shape of the problem

Three parties, and the knowledge is split across them:

- The body knows the source name and the kind of operation. It cannot know the class.
- The elaboration that binds the reference knows the instance, so it knows the class, so it can turn
  the name into a position.
- The runtime knows which object the reference holds at the moment of the access, which is not
  decided until then.

One constraint separates this from every route the model already carries. An object reference's
target changes during simulation: the design assigns a new object to the same variable, and both are
legal values of it. Every existing route walks the static instance tree, which is built once and
never re-pointed, so its endpoint is a stable address. A route whose leaf is an object reference has
no such address to commit to.

What is sealed is therefore not where the target is. It is **how to reach the target once an object
is in hand**.

## Where other systems put this

Two families, and which one a compiler lands in is decided by whether the operations themselves
differ per type.

**Carry a position in a per-class record; the body reads it.** The Itanium C++ ABI does this for
virtual bases, whose offset within a complete object is not a compile-time constant because it
depends on the most-derived class's layout. The ABI puts the offset in the virtual table: "Virtual
Base (vbase) offsets are used to access the virtual bases of an object. Such an entry is added to
the derived class object address (i.e. the address of its virtual table pointer) to get the address
of a virtual base class subobject." One compiled body, the position supplied by whichever class the
object turns out to be.

**Carry a record of operations; the body calls through it.** Swift compiles an unspecialized generic
function once and passes type metadata and protocol witness tables as implicit arguments, built at
the call site where the concrete type is known. Haskell type classes are the origin of the shape --
Wadler and Blott's dictionary-passing translation compiles a class-polymorphic function once and
hands it a dictionary of methods at each use.

**Where the conditions differ, and it is the load-bearing difference.** Swift and Haskell must pass
code because the operations themselves differ per type: a protocol requirement has a separate
implementation for each conforming type, reachable by no uniform position. That is not the situation
here. The object model already gives one access operation for every property and one for every
behavior, parameterized by a position; nothing about the operation varies with the class. Passing a
record of operations would therefore deliver code where data suffices, and would add a per-pair
record that has to be built, stored, and kept alive, where a position is a value the route mechanism
already has somewhere to put.

## Decisions

### D1. The coordinate is the one that already exists; what is new is that it may be formed at elaboration

A referrer that can name a class already turns a source name into a **coordinate** -- the class that
declares the thing, plus where it sits among what that class declares -- and resolves it by walking
the promises that class publishes. This entry adds no second concept. It states that the same
coordinate may instead be formed where the instance is known, for exactly the references whose class
no signature publishes.

Which side forms it changes nothing about what it is or what reads it. A coordinate formed at the
referrer's compile time and one formed at elaboration are the same value, name the same thing, and
are consumed by the same access.

**The class it is formed against is the one the reached storage was declared with**, which the
instance fixes because a class a design element declares is a type of that element's instance. It is
never the class an object turns out to be. Two facts force this and either is enough: a reference
commonly names no object when its route seals, since the design assigns one later; and which
property an access reaches is decided by the class the access names rather than by the object (IEEE
1800-2023 8.14), so the object's class would be the wrong answer even where one is in hand.

The coordinate is committed at the sealing barrier like any other route result and is read directly
thereafter. The body applies it to whichever object the reference holds at each access. Name
resolution happens once per reference; the hot path performs no lookup, which is what
`../architecture/reference_resolution.md` already forbids.

### D2. Three coordinate kinds, and collapsing any two loses a distinction a consumer reads

A coordinate names what the access is, not how it is realized. There are exactly three, and they
differ in who answers the last question.

**A property coordinate** names the class that declares the property and the position the property
occupies among what that class declares. Resolution is complete at elaboration: the pair fully
determines which storage the access reaches on any object of that class. IEEE 1800-2023 8.14 makes
this the right split -- a subclass may declare a property of the same name as its base, and which
one an access reaches is fixed by the class the access names rather than by what the object turns
out to be.

**A non-virtual behavior coordinate** names the class declaring the behavior and the behavior. The
body is determined at elaboration, because a non-virtual behavior is not overridden: what the class
declares is what runs.

**A virtual behavior coordinate** names the class that _introduced_ the behavior and the behavior's
ordinal within that class. Resolution at elaboration answers **which dispatch position the source
name means**, and nothing more. Which body fills that position is answered by the object at the
moment of the call, because IEEE 1800-2023 8.20 lets any class in the lineage take the position over
and 8.22 makes the object's own class decide.

This last kind is the one that must not be written as "elaboration decides which body runs." It does
not. The split is:

```text
elaboration:  source name  ->  (introducing class, ordinal)
runtime:      (object, (introducing class, ordinal))  ->  body
```

Both halves are needed and neither substitutes for the other. Reading the first half as the whole
resolution turns a virtual call into a static one; reading the second half as the whole resolution
turns a resolved name into a run-time name lookup. The reference is never what decides -- its static
view carries no class, and the object's class is what it always was.

### D3. Being opaque to a referrer is not being dynamic

A reference with no class view is not a reference whose view is chosen at run time. Every legality
question -- whether the name exists, whether the operand types match, whether the assignment is
permitted -- is answered before any lowering runs, against a class the front end resolved. What the
referrer lacks is a **name** for that class, not knowledge of it.

The class lives on the object, which is where the object model already puts it, and that is what
lets a virtual behavior dispatch and a checked downcast (IEEE 1800-2023 8.16) work at all. An opaque
reference and an object that knows its class are the same picture from two sides, not a
contradiction.

### D4. A class's runtime record answers by name during resolution, and by position afterwards

Turning a source name into a coordinate requires asking a class what it declares under that name.
The class record therefore carries a by-name table, consulted while a reference resolves and never
on the simulation path. This is the discipline a runtime scope already follows for the names reached
past its unit's signature; a class gains the same thing for the same reason.

The positional schema stays the authority for access. The by-name table is a resolution aid and is
never what an access reads.

### D5. The coordinate is an operand of the access, not a second kind of access

An access states the target it reaches. A coordinate computed elsewhere is one of the ways a target
is named, taking its place in the closed set of target forms beside a position this artifact owns
and a position read off a consumed signature. It is not a second access operation, and no consumer
decides between them by inspecting anything other than which form the access states.

A backend that cannot realize the coordinate form refuses it; it never falls back to another form.

### D6. A name that does not resolve is a sealing failure with a user diagnostic

A source name that the class does not declare, or declares with an incompatible shape, is rejected
at the sealing barrier with a user diagnostic, in the same class of outcome as a route reaching a
non-constructed target. It is not an `InternalError` and there is no run-time fallback.

## Rejected alternatives

**A record of operations built per class and carried by the reference** -- the witness-table and
dictionary-passing shape. Rejected because the operations do not vary with the class: one access
operation parameterized by a position covers every case, so the record would carry code to achieve
what a position achieves. It also introduces a per-class-per-referrer object that must be built,
owned, and kept alive for as long as any reference to it, where a coordinate is a value the route
mechanism already stores. Its enabling condition is real and absent here -- were a backend ever to
need per-class implementations of an operation that cannot be reached by position, this is the shape
to reach for.

**By-name lookup at each access.** The body carries the source name and the runtime resolves it on
the object every time. Rejected by `reference_resolution.md`'s existing prohibition on a per-access
lookup on the simulation path, and the cost is not incidental: the access sits wherever the design
wrote it, including inside the hottest loops.

**Specializing the body per endpoint class.** Two instances whose upward names land on different
classes become two specializations. Rejected because the key would have to distinguish them by where
each instance sits, and `specialization_model.md` forbids a specialization key derived from instance
path, ordinal, or enumeration. It also scales with instance count, which the same doc rules out
directly.

**Compatible representations across the boundary.** The producer's typed form and the consumer's
opaque form are arranged to agree in layout, so the consumer reads the producer's storage directly.
Rejected: agreement in layout is a property of a target language's choices, not a contract either
side states, and it holds for some shapes and not others -- so the failure is silent and appears
only for the shapes where it does not hold. The cross-unit reach is by-contract or it is not
correct.

**Carrying the class in the reference's type anyway, resolved per instance.** Rejected because it
puts an instance fact in an artifact shared by every instance; the same body would need two
incompatible types at one position.

## Consequences

- A legal SystemVerilog program that reaches a property or a behavior through a name past another
  unit's signature has a lowering, and the lowering is per-specialization.
- The abstraction is the operation, not the syntax that produced it. Any value reached across a unit
  boundary with no class view takes the same shape when a structural operation follows -- a handle
  read from a member, an element of a collection of handles, a handle returned by a behavior. No
  form gets its own mechanism.
- A class's runtime record gains a by-name table used only while references resolve, matching what a
  runtime scope already carries.
- The access target becomes a closed set with a coordinate form in it, so every consumer states what
  it means and gaining the form breaks the build until each one does.
- A backend reaches a property or a behavior on an opaque reference by coordinate rather than by
  resolving a name in the target language. A backend that realizes typed access through its own
  language's name resolution keeps doing so for the cases that have a class view, and answers this
  one the same way every other backend does.

## Cross-references

- `../architecture/reference_resolution.md` -- the route model this extends: segment classification,
  the sealing barrier, and the prohibition on per-access lookup.
- `hierarchical-reference-routing.md` -- D3, the endpoint that inherits its target's access
  protocol, and D5, the open target family whose requirements this entry answers.
- `../architecture/object_model.md` -- the object model that gives one access operation per property
  and per behavior, and puts the class on the object.
- `../architecture/specialization_model.md` -- one artifact per specialization, and the forbidden
  keys that rule out specializing per endpoint class.
- `../architecture/compilation_unit_model.md` -- the two ways into another unit, which is what makes
  such a name resolve at elaboration rather than at the referrer's compile time.
- `dispatch-position-is-a-lineage-coordinate.md` -- the introducing class plus ordinal that a
  virtual behavior coordinate names, and why a lineage is not flattened into an absolute position.
- `reaching-past-a-published-class.md` -- the coordinate a referrer forms when it _can_ name the
  class, which this entry is the counterpart of for when it cannot.
- `class-declared-in-a-structural-scope.md` -- the class whose type is per instance, which is what
  makes the referrer's lack of a name permanent rather than incidental.
