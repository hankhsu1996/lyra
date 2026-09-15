# A source anchors an intrusive ring, and a membership names a leaf

Date: 2026-09-11 Status: accepted

## Context

[event-source-has-two-realizations](event-source-has-two-realizations.md) D5 left the physical shape
of a declared variable's source open and said where the remaining gain was: most cells genuinely
need a source, so shrinking one is worth a multiple of making it conditional. This entry decides
that shape.

**What the current 48 bytes on a cell are.** An observable cell holds a list whose header is a full
subscription node, measured field by field:

| field          | bytes | what it is                   |
| -------------- | ----- | ---------------------------- |
| `prev`, `next` | 16    | the anchor of the member set |
| `activation`   | 8     | which waiter                 |
| edge, padding  | 8     | the fire condition           |
| bit offset     | 8     | the fire condition           |
| bit width      | 8     | the fire condition           |

A header names no waiter and carries no fire condition, by definition, so **32 of those 48 bytes are
never read on any cell**. A subscription node splits differently: 16 bytes of linkage and 8 naming
the waiter are its own, and the remaining 24 -- edge, bit offset, bit width -- are the leaf's
predicate, which [event-subscription-model](event-subscription-model.md) D2 puts on the leaf and D6
calls a fast path rather than a primitive. Those 24 bytes ride on every membership in the system,
including the scheduler-queue memberships created and destroyed every delta cycle, which have no
fire condition at all.

So a source, as the accepted entries define it, is required to hold **nothing**: not a value, not a
previous result, not a predicate, not a gate, not a waiter. Its whole job is to reach the leaves
that depend on it, and the only question is what anchors that set.

**What the anchor could be.** An intrusive ring closed through a header node gives target-agnostic
O(1) unlink -- the operation an activation performs when it drops its enrollment without consulting
whichever target holds it -- for two pointers of header. Linux's `hlist` gives the same O(1)
target-agnostic unlink for a **one-pointer** header, by making each node hold `next` and a pointer
to the previous node's `next` field rather than to the previous node. The node is the same two
pointers either way, so the whole difference is one pointer per source: on Ibex, 16.5 KiB across
2109 cells, with no per-membership cost to trade against it.

**What decides it is not the memory.** Every target kind in the runtime holds one of these lists,
and the measured usage separates them cleanly: the observable cell does `PushBack`, `ForEach` and
`Unlink` and nothing else, while a named event, a cancellation, a fork's parked parent, a process's
terminated waiters and all of the scheduler's region queues use `PopFront`, and the scheduler also
splices a whole queue onto another every delta cycle when the next delta's work becomes active.
**Splicing a list onto another in constant time needs the ring**; an `hlist` has no tail, and giving
it one makes unlinking the last node fix up the anchor, which destroys the target-agnostic unlink
that is the only property the design actually requires. A node holds either a `prev` or a
`pointer-to-previous-next`, never both, so one membership primitive means one anchor shape.

## Decision

**A source is a 16-byte intrusive ring anchor. A membership is 24 bytes: two links and one payload
pointer. Everything a leaf owns lives on the leaf.**

**D1. The anchor is the ring, and it is the same primitive for every target kind.** One membership
type serves the observable cell, the named event, the cancellation, the fork parent, the terminated
waiters and the scheduler queues. The cell pays two pointers it does not individually need so that
the runtime has one membership primitive rather than two.

**D2. A header is not a node.** The anchor holds the two links that close the ring and nothing else.
The waiter identity and the fire condition leave the header, which is 32 bytes on every cell in the
design.

**D3. A membership carries one payload pointer whose meaning belongs to its target.** For an
observable cell it names the leaf to reevaluate; for a queue or an event it names the activation.
Nothing in the membership describes when to fire.

**D4. The predicate lives on the leaf, once.** Edge, bit offset and bit width stop being membership
fields. A leaf that depends on several sources holds one predicate rather than a copy per source,
and a membership on a target that has no fire condition carries none.

**D5. A zero-byte source is a specialization, never the baseline.** Where the compiler proves a
source's member set is fixed for the life of the program it may encode the fanout directly and omit
the anchor. That proof does not hold in general: a procedural `@` inside a loop enters and leaves
its membership on every iteration, a class object's source is materialized on demand, and a waiter
that observes everything can appear at run time. The runtime shape is the ring; the static encoding
is an optimization above it.

## Invariants

1. A membership unlinks itself without naming, consulting or searching the target that holds it.

2. A source holds no state describing when to fire, which waiter to reach, or what the value was.

3. One membership primitive serves every target kind. A target that needs an operation the primitive
   does not have is a reason to revisit the primitive, not to add a second one.

4. A predicate exists once per leaf. No membership carries a copy of one.

## Rejected

- **A one-pointer anchor in the shape of Linux's `hlist`.** It is strictly smaller -- the node is
  the same two pointers, so it saves one pointer per source and nothing is traded against it, 16.5
  KiB on Ibex. It is refused because the node shape is exclusive: a node holds a `prev` or a
  pointer-to-previous-`next`, and the scheduler's constant-time splice of one queue onto another,
  performed every delta cycle, exists only in the ring. Keeping both shapes would buy a sixth of the
  observation metadata on this design with a second membership primitive to maintain forever.

- **A zero-byte source as the general shape.** The population of declared variables is fixed by
  elaboration but the membership set on one of them is not: a procedural `@` in a loop joins and
  leaves on every iteration. Compile-time fanout is real and is D5's specialization; it is not the
  runtime shape.

- **Keeping the fire condition on the membership.** What it costs is visible now that the membership
  type is shared: 24 bytes on every membership in the runtime, including ones whose target has no
  concept of an edge, on a path that churns every delta cycle. What it costs semantically is worse
  -- a leaf with several dependencies would hold its predicate once per source, so the copies could
  disagree.

- **A per-target membership type, sized to what each target uses.** It is the shape that minimizes
  bytes and it multiplies the primitive the whole scheduler is built on. The measured spread is one
  pointer per source.

- **Shrinking the header by making the cell's waiter set implicit.** Considered and not pursued: the
  cell would then have to be found from the waiter rather than the other way round, which is the
  address-keyed lookup that
  [event-source-has-two-realizations](event-source-has-two-realizations.md) D4 refuses on the write
  path.

## Consequences

- **The observation machinery on Ibex falls by between 38 and 48 percent**, from 231 KiB to between
  143 and 121. A cell falls from 48 bytes to 16. An edge does not fall to 24: the membership is 24,
  and the predicate and the waiter it stops carrying still have to exist once per leaf, so the
  honest per-edge figure is 40 bytes where a leaf is a separate object and 32 where a single-leaf
  subscription and its one membership are one allocation. The gain is real and it is the anchor plus
  the oversized predicate, not the membership alone.

- **The delta-cycle path gets smaller, not just the static footprint.** Every time a process becomes
  runnable it takes a membership on a region queue; that membership stops carrying 24 bytes of fire
  condition it never reads.

- **Two of the three changes this entry implies are not decisions.** A header that is not a node,
  and a predicate that lives on the leaf, are the implementation catching up with entries already
  accepted. Only the anchor shape was open.

- **A second membership primitive is now a decision to revisit this entry**, not a local choice. If
  a target ever needs one, the question is whether the ring is still the right single primitive.

- **The remaining per-cell cost is deliberate and known.** Two pointers on a cell whose median
  declared width is one bit is still large in ratio; it is what one universal membership primitive
  costs, and the way to reduce it further is the static fanout of D5 rather than a second shape.

## Cross-references

- [event-source-has-two-realizations](event-source-has-two-realizations.md) -- which left this shape
  open and said the size of a source was where the gain was.
- [event-subscription-model](event-subscription-model.md) -- the leaf that owns the predicate this
  entry moves off the membership.
- [object-is-an-event-source](object-is-an-event-source.md) -- the other realization, whose anchor
  is materialized on demand rather than provisioned.
- `../architecture/scheduling.md` -- the region queues whose splice decides the anchor shape.
