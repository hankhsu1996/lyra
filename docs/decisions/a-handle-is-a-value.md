# A class handle is a value, and a value states its own managed edges

Date: 2026-09-11 Status: accepted

## Context

LRM 8.3 makes a class handle a variable whose value names an object or is null, and Table 11-1 gives
it the "Any data type" row -- assignment, the conditional operator, and both equality families --
and no relational operator. The language then states its aggregates over _any data type_: an element
of a fixed-size unpacked array, a dynamic array, a queue or an associative array (LRM 7.4.2, 7.5,
7.8, 7.10), a member of an unpacked structure (LRM 7.2), an associative array's index (LRM 7.8.3),
and an operand of the assignment-pattern conversion (LRM 21.2.1.6). A handle reaches every one of
those because it is a value, not because any clause grants it each position separately.

The runtime realized it as something short of one. The handle had a value domain of its own and a
member slot could hold one, but the type-erased value an aggregate holds admitted every domain the
runtime realizes except this one, and the monomorphized handle the C++ backend renders claimed no
value contract at all. Four positions therefore failed in three unrelated ways: an element or a
member met a missing library entry on one backend and a template constraint on the other, an index
was refused before any backend, and a format operand met a refusal written against LRM 6.14 that LRM
21.2.1.6 contradicts.

The refusal was the informative one. A template constraint rejecting a handle as a value is the one
reviewer that read the value contract, and the answer is to satisfy it rather than to route a
predicate around it.

## Decision

### D1. The handle is a value of its own domain, and the domain claims the value contract

A handle's value is which object it names and nothing else. It therefore satisfies the same contract
every other value domain satisfies -- the universal equality family, the change-detection predicate,
the unknown-bit queries and the declared default -- and claims the case-equality family, which LRM
11.4.5 gives the same meaning as equality here. It claims no ordering, because Table 11-1 defines no
relational operator on it.

Nothing beyond that is granted per position. Once the contract holds, an element, a member, an index
and a format operand each follow from the clause that states them over a data type.

### D2. Two realizations of one concept, split by how a member is reached

A handle carries the object's identity, and where the target reaches a member through a typed
pointer it carries that pointer beside the identity; where a member is reached by a coordinate the
object answers for, the identity alone is the whole value. These are the erased and monomorphized
realizations every aggregate domain already has, one layer down, and both live in the value layer
beside the domains they pair with.

The object's allocation, the recovery of a typed owner from an identity, and the record a body reads
to name its own receiver are not part of the value and stay with the runtime that owns them.

### D3. Handle equality answers with the value, not with a predicate the call site widens

LRM 11.4.5 makes the answer always a known 1'b0 or 1'b1, so the operation is stated at that, as it
is for every other domain of the same table row. A handle comparison is not a machine predicate the
layer above lifts back into a value.

### D4. A class index orders by which object

LRM 7.8.3 says the entries of a class-indexed associative array order deterministically but
arbitrarily and that a null index is valid. Which object a handle names is therefore the order.
SystemVerilog states no ordering operator on a handle, so the order is the host's, exactly as LRM
6.14 leaves a chandle's to be.

### D5. A handle prints under the assignment-pattern conversion and under no other

LRM 21.2.1.6 gives a chandle, a class handle, an interface class handle, an event and a virtual
interface an implementation-dependent text, and fixes only that a handle naming nothing prints the
word `null`. What the text stands for is which object the handle names, so two handles naming one
object print alike and no two objects share a text.

The language defines the handle under no other conversion. Where the conversion is known before the
run, lowering says so; a format string the program computes carries its conversions only at run
time, so the formatter answers that one itself.

### D6. A value states its own managed edges, so no storage owes a description beside it

An aggregate holding handles is not a new collector root. It is an edge, reached through a value
that already lives in described storage, and which edges a value holds is answered by the value: its
own domain is what a runtime value carries, and the party that knows an aggregate's layout is the
library that implemented it. Nothing is computed at any stage and no generated code says anything
for the collector's sake.

## Invariants

1. A class handle satisfies the value contract every value domain satisfies. A position the language
   states over a data type admits one without being granted it.

2. A handle's value is which object it names. A realization may carry more so that a member can be
   reached, and what it carries beyond the identity is never part of the value.

3. Which managed edges a value holds is answered by the value. No storage that can hold a value owes
   a separate description of what is inside it, and no root enumeration descends into one.

## Rejected

- **A predicate that lets a handle past the value contract.** The shape this replaces: a query true
  of a chandle and of a handle together, used to route both past the constraint that refused them.
  The two differ in exactly the way that matters -- a chandle is the pointer it carries and owns
  nothing -- and the constraint was the one reader that had the definition in front of it.

- **A per-aggregate description the compiler emits for the collector.** The answer of every runtime
  whose compiler knows the physical layout of what it describes. Ours does not: the layout belongs
  to the runtime library, and the erased value already carries its own domain, so a description
  emitted beside it would be a second statement of one fact that can disagree with the first.

- **Making the aggregate a collector root.** An aggregate always lives in storage that is already a
  root or is reached from one, so rooting it would add a second path to the same objects.

- **One handle realization for both targets.** The monomorphized side must hand back the pointer a
  reference was formed with, because a program point that names a base or an interface class reads
  its own pointer out rather than converting one; the erased side has no such pointer and would
  carry a word that answers nothing.

## Consequences

- A collection of handles works, and every operation such a collection supports follows from the
  domain rather than from the collection, so the answer is one and not one per container.
- A handle equality is one operation at every layer, so the lowering that lifted it from a host bool
  and widened it back is gone.
- A chandle prints under `%p`. The refusal that read LRM 6.14 as forbidding it, and the type walk
  that looked for one nested inside an aggregate, are both gone; slang rejects an aggregate operand
  under every conversion but the assignment pattern, so a handle reaches a conversion it has no text
  for only as a bare operand.
- Streaming a class object (LRM 6.24.3 admits a class of bit-stream-type properties) is a separate
  operation over the object's own properties and is not yet carried out.

## Cross-references

- [value-type-concepts](value-type-concepts.md) -- the operator-family lattice the domain claims its
  row of.
- [jit-aggregate-realization](jit-aggregate-realization.md) -- the erased / monomorphized pair every
  aggregate domain has, which D2 is the handle's instance of.
- [managed-value-realization](managed-value-realization.md) -- the described storages that are the
  root set, which D6 adds no member to.
- [object-model](object-model.md) -- the managed reference realized by precise tracing, and the
  shared-owner staging that realization admits.
- `../architecture/object_lifetime.md` -- traceability as a recursive type property, which D6 states
  at the value.
