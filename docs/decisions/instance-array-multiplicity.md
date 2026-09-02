# The Multiplicity of an Instance Array

## Date

2026-09-01

## Status

Accepted. Realizes `../architecture/mir.md` invariant 7 and
`../architecture/hierarchy_and_generate.md` invariant 9 on both backends, and settles what an
interface port carrying a range publishes. It reverses no recorded decision; it replaces an
implemented shape that states a member's multiplicity outside the member's type.

## Why this decision matters

A bundle of identical blocks joined by a bundle of identical links is how most of a design's
connectivity is written, and both halves of that sentence are arrays. The construct is ordinary, so
what it costs is paid by every design rather than by an unusual one.

A member per element makes the emitted artifact a function of the element count: one handle member
and one construction statement each, for a set of objects that are all the same unit. That is
compile-time work scaling with instance count, which is the one scaling law the compiler is built to
refuse. It also leaves the multiplicity with nowhere to live. The member's type says "one child", so
which element a member stands for survives only in its name, and a consumer recovers it by matching
a spelling convention.

The same fact decides a construct that has no representation at all otherwise. A module header may
carry a range on an interface port, so one published member stands for several objects. A published
member's position is its position in the signature, so an array port is one member, and the only
channel a signature has for the count is the member's type. An owned array that keeps its
multiplicity in a naming convention and a published array that keeps it on a type are two ways to
say one thing, maintained separately and by different code.

## The tension this addresses

Three constraints hold at once, and together they leave exactly one shape.

- **The element count is not a compile-time fact.** A parameter steers construction -- which
  children are built and how many -- without forking the unit's compile-time artifacts. So the count
  cannot be part of a type identity, or a design would emit a different artifact per array size.
- **A member's classification is its type.** Nothing beside the type may state that a member is an
  array, which rules out a count on the declaration, a flag beside the type, and a naming
  convention.
- **A place has no index step.** An array of storage is reached through the indirection its elements
  already carry, so the element must itself be a pointer rather than a slot addressed into.

A sequence wrapper over a pointer satisfies all three: it carries multiplicity without a length, it
is a type rather than a side fact, and its element is the indirection the place vocabulary requires.

## Decisions

### D1. An array of children is one member whose type is a sequence of the child pointer

A member holding one child is a pointer to the child's object. A member holding an array of children
is a sequence of that pointer, and a multidimensional array nests the wrapper once per dimension.
The member's name is the array's name as the source wrote it, undecorated, and nothing beside the
type states how many objects stand behind it.

This is the same wrapper for an owned child and for a borrowed one. What differs between a module
instantiating four children and a module reaching four it does not own is which pointer the sequence
is over, which is a fact the pointer already carries.

### D2. The element index is an operand, not part of a member's identity

Because the multiplicity is in the type rather than in the member set, which element a reference
names is an operand of a projection over the one member: project the member, index the sequence once
per dimension. A reference names a constant coordinate, since the route a reference lowers to
carries coordinates already resolved during elaboration rather than an expression. So a route
through an array of children and one through a single child differ by that projection and nothing
else -- not by which member they name, and not by a spelling convention either of them has to know.

The sequence itself is composed where it is built: the value a declaration's member holds is built
whole, elements and all, rather than created empty and filled afterwards. That is the rule every
sequence value follows, and it is what makes the member complete at the moment it exists, so nothing
observes a half-built array.

### D3. The sequence carries no length

The wrapper states that a member holds many, never how many. A length in the type would make the
type -- and therefore the artifact naming it -- a function of the element count, which is the
scaling this decision exists to remove, and would state as a compile-time fact something the
constructor decides.

The length is a property of the value the constructor builds. A backend that wants a contiguous
block sized once reads the length from the construction, not from the type.

### D4. An interface port carrying a range is the same member over a borrowed pointer

A module header may give an interface port a range, so the port stands for as many instances as the
range has elements and selecting an element of the port reaches one of them. That is D1 with the
sequence over a borrowed pointer to another unit's object, and it introduces no vocabulary of its
own: the published member is one member at one signature position, its type is the sequence, and the
declared range belongs to that type the way an unpacked range belongs to any type.

The connection supplies an array of that size, and each element is bound to its own instance during
elaboration, which is the scalar port's binding performed once per element rather than a second kind
of binding.

## Rejected alternatives

- **A member per element, with the coordinate spelled into the member's name.** It needs no sequence
  wrapper and no indexing, and every element is an ordinary scalar member. Rejected because it puts
  the multiplicity outside the type: the member set becomes a function of the element count, the
  artifact grows with it, and a consumer asking which element a member is has only the name to read.
  A member's type is the classification, and a naming convention standing in for one is the shape
  that rule names.

- **A fixed-size aggregate carrying the count.** The count is known once parameters are bound, so a
  sized aggregate would carry it in the type and need no length beside the value. Rejected twice
  over: it makes the type a function of the element count, which forks artifacts by array size; and
  its element is plain machine data, which an owning pointer to an object is not.

- **An index step in the place vocabulary**, so an element is a place addressed into the member.
  Rejected by the place contract: a value aggregate's interior is not independently addressable, and
  an array of storage is reached through the indirection its elements carry. The
  sequence-of-pointers shape is what that contract asks for, not a workaround for it.

- **Publishing one member per element for an array port.** The signature would carry four members
  for `Bus b[4]`, and placement would need no new answer. Rejected because a published member's
  position is its position in the signature: a member occupying a range of positions makes a
  member's identity a range rather than a name, and a referrer counting positions would have to know
  each earlier member's element count to find the next one.

- **Realizing the sequence as a container of the simulation value system.** The value system already
  carries sequence containers with element access, so an array of object handles could reuse one.
  Rejected because an object handle is structural, not a simulation value: routing hierarchy through
  the value domains puts object identity behind an erasing boundary built for values, and makes
  every backend's value container a dependency of hierarchy navigation.

## Consequences

- A scope's member set is a function of what the source declared, not of how many elements each
  declaration has, and neither is the number of statements that build it. How many element
  constructions stand inside that one composition is a separate question about how a sequence value
  comes to be, which this entry does not settle.
- The sequence wrapper gains its first producer. It is already declared in the type systems of both
  execution IRs and rendered by the C++ backend; what this decision adds is the lowering that builds
  one and the machine-code realization of a member holding one.
- An interface port carrying a range needs no vocabulary beyond D1. The member, the route step that
  reaches through it, and the binding are the scalar forms with a coordinate added.
- A route step through an array of children and one through an array port state their coordinates
  the same way, so a reference reaching an element does not depend on which kind of array it passed
  through.
- A port connection on an array remains one connection per element. The frontend distributes an
  array connection across the elements, so each element's connected expression arrives already
  matched to its index and is not a function of the index this decision makes available. Whether a
  connection on an array is one connection over the array is a separate question about what a
  connection is, and it is not answered here.

## Cross-references

- `../architecture/north_star.md` -- compile-time work scales with the number of distinct unit
  specializations, not with instance count, which is what the member-per-element shape violated.
- `../architecture/mir.md` -- invariant 7 states the member shape this entry realizes, and the
  forbidden shape that a classification outside the type system is.
- `../architecture/hierarchy_and_generate.md` -- invariant 9 keeps multiplicity and generate
  orthogonal, and invariant 6 states that a parameter steers construction without forking artifacts,
  which is what D3 rests on.
- `../architecture/lir.md` -- the place vocabulary with no index step, which fixes the element as an
  indirection.
- `published-member-placement.md` -- a published member's position is its position in the signature,
  which is why an array port is one member.
- `interface-port-binding.md` -- what an interface port's member holds and who binds it; D4 is that
  member once per element.
- `unpacked-range-belongs-to-type.md` -- the declared range as a property of the type, which is what
  carries an array port's range across a signature.
