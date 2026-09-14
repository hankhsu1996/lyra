# Storage

Parts of this contract are not realized on the execution backend. The rules below bind every layer
regardless, and a layer that cannot yet meet one refuses rather than approximating it.

## Purpose

SystemVerilog decides which storage a second name may denote, and that decision is the language's
rather than the compiler's. This document states what has storage identity, what may alias it, what
an aggregate's components are, and what an assignment to a whole aggregate does to them.

The distinction it exists to hold: **an expression that can be assigned is not necessarily an entity
that can be aliased.** A packed part select is a legal assignment target and is not a legal `ref`
actual; an unpacked array element is both. Every rule below follows from taking that seriously.

## Owns

- The set of entities that have **storage identity**, and the rule that decides membership.
- The separation between an **access path**, which is evaluated and may locate something different
  each time, and a **storage identity**, which is resolved once and retained.
- What a reference binds, and what it must go on denoting while the aggregate around it changes.
- What a whole-aggregate assignment does to the identity of its components.
- The separation of storage identity from the identity a write raises an event on, and from the
  identity a process waits on.

## Does Not Own

- How long storage lives, and who ends it (`lifetime.md`).
- How a value's bits are represented. Extent and alignment are all a compiler needs of a
  representation; everything inside one is a backend and runtime realization.
- Compiler-internal identity -- typed ids, cross-unit reference variants, which entity holds
  architecture-bearing state (`identity_and_ownership.md`).
- Resolving a hierarchical name to a typed endpoint (`reference_resolution.md`).
- When a process is considered for evaluation (`scheduling.md`).
- The structure and construction protocol of a class object (`object_model.md`).

## Core Invariants

1. **Storage identity is a closed set, and membership follows from whether the aggregate gives each
   component storage of its own.** A variable, a class property, a member of an unpacked structure,
   and an element of an unpacked array have it; nothing else does (LRM 13.5.2). Packed aggregates
   give their components bit ranges of one vector and a union gives its members one shared storage,
   so neither yields components with identity. _Consequence: the storage hierarchy stops descending
   at the first packed aggregate and at any union, while the value hierarchy descends to bits._

2. **Assignability and storage identity are different questions.** A component with no identity is
   still writable, and the write is a write to the entity that contains it. _Consequence: no
   consumer may use left-hand-side position as the test for what may be aliased._

3. **An access path is evaluated; a storage identity is retained.** A reference resolves the path
   once, at the bind, and holds the entity. _Consequence: nothing that survives a bind may carry a
   coordinate, because re-evaluating it later is not the same question._

4. **Forming a reference is an operation on the aggregate, not a projection of it.** Binding to an
   absent associative entry allocates it, before any read or write through the reference (LRM
   7.8.7). _Consequence: a lowering may not treat reference formation as side-effect-free, and may
   not reorder it with respect to observation of the container._

5. **A component's identity is independent of its position and of its membership.** Inserting at any
   position in a queue moves every element and outdates a reference only to an element the insertion
   itself removes (LRM 7.10.3); an element removed from a variable-size container while a reference
   is bound goes on existing, and writes through that reference reach it and are not visible through
   the container (LRM 13.5.2). _Consequence: membership, position, identity, and duration are four
   properties, and a realization that fuses any two of them is wrong for some legal program._

6. **A whole-aggregate assignment writes into the existing components of a fixed-size aggregate, and
   replaces the population of a variable-size one.** The first is element-wise into the components
   that are already there (LRM 7.6), so references to them stay valid and observe the new values;
   the second resizes or clears first, which is why the language outdates references there and
   nowhere else. _Consequence: whether a whole-value store may replace a component-bearing object is
   decided by fixed versus variable size, never by the store being whole._

7. **A reference aliases and does not own.** The language extends a lexically enclosing scope to
   cover the processes a `fork` spawns (LRM 6.21), and it makes the program illegal where that would
   not reach (LRM 9.3.2); it never makes a reference an owner. _Consequence: no realization of a
   reference may retain its referent, and the one detached-element rule of invariant 5 is a property
   of the element rather than of the reference._

8. **Storage identity, update identity, and event identity are three separate things.** A write
   raises an update event on the variable (LRM 4.3); a process waits on an expression, and observes
   an event only when that expression's result changes (LRM 9.4.2). _Consequence: giving a component
   storage identity does not oblige anything to give it a subscriber, and a component's write is an
   event on the entity the language names around it -- the variable that contains it (LRM 4.3), or
   the object, where 9.4.2 requires a change to a data member to reevaluate an expression that
   reached it._

## Boundary to Adjacent Layers

- **MIR** carries access as expressions -- a local reference, a member access, a dereference, a
  designated part of a value. It states which entity a reference is built over and does not state
  when the path is evaluated, because a reference is built by one node whose operand is that path.
- **LIR** carries the place vocabulary and the operation that turns a place into a value. The phase
  boundary of invariant 3 lands here: the place is the access path and is consumed where it is
  written, and the value that operation yields is the retained identity.
- **Each backend** realizes a storage identity in its own terms and answers, per storage kind, how a
  load through it, a store through it, and lending it are performed (`backend_contract.md`).
- **`lifetime.md`** takes over at duration. This document says a detached element goes on existing;
  that document says which regime ends it.
- **`scheduling.md`** takes over at invariant 8's second and third identities.

## Forbidden Shapes

The test each answers: does it fuse two things this document keeps apart -- an access path and an
entity, assignability and identity, a component's position or membership and its identity, or
storage identity and the identity a write or a wait uses?

- **A retained reference that stores a coordinate into a container.** Positions move while
  identities do not, so the reference would follow the position and denote a different element.
- **Left-hand-side position used as the test for what may be aliased.** It admits every packed part
  select, none of which is an entity.
- **Storage identity given to a component of a packed aggregate.** The aggregate is one vector and
  the component is a range of its bits, so two such components can overlap and neither is
  independently addressable.
- **Storage identity given to a member of a union.** A union is a single piece of storage whose
  members are alternative readings of it, so distinct member identities would necessarily alias.
- **A whole-value store on a fixed-size aggregate that replaces the object holding its components.**
  The assignment is defined as writing into those components, so replacing them makes a reference to
  one denote storage the program can no longer reach.
- **A reference that keeps its referent alive.** Ownership by reference is a discipline the language
  does not have, and adding one makes a program's storage outlive what the language says it may.
- **A subscriber list on a component's storage.** The event a process waits on is a change in an
  expression's result, which storage cannot answer, so observers held per component make observation
  a property of storage without deciding anything. Notifying at a finer grain than the entity above
  is an optimization over which waiters are reconsidered and is not this shape.
- **A storage hierarchy derived from a value hierarchy.** They descend to different depths, so the
  derived one either invents identities the language withholds or drops ones it requires.

## Notes / Examples

The two hierarchies, on one declaration each:

```systemverilog
logic [31:0] p;
int a[4];
```

```
value hierarchy      p -> p[31] ... p[0]      a -> a[0] ... a[3]
storage hierarchy    p                        a -> a[0], a[1], a[2], a[3]
```

`p[3]` is a projection of one entity; `a[3]` is an entity. Reading a path outward-in, every layer
has identity until the first packed aggregate or union, and from there down nothing does -- so in a
struct holding `int m[4]` and `logic [31:0] q`, the entities are the struct, `m`, each `m[i]`, and
`q`, while `q[7:0]` is a projection.

Why invariant 3 is not a stylistic preference:

```
before push_front(X)     q = [A, B, C, D]        a reference denotes C, at q[2]
after  push_front(X)     q = [X, A, B, C, D]     the same reference denotes C, at q[3]
```

Every position changed and no identity did, so a reference holding `(container, index)` would have
begun denoting B.

**Current implementation.** LIR spells the access path as a place -- a base operand plus a
projection chain -- and the address-of instruction is the operation of invariant 3 that yields the
retained identity. A place appears only inside the instructions that consume it, which is what makes
it an access path rather than an entity; the value that address-of produces is what a reference
holds, what crosses to a callee, and what the lifetime operations take.
