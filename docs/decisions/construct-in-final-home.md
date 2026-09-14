# Construct in the final home, never build and move

Date: 2026-09-11 Status: accepted

## Context

A closure value is built in the per-stretch arena and moved into whatever will own it: a region
queue for a nonblocking assignment, a postponed print or a deferred assertion's action, and the
execution itself for a `fork` branch. Only the `with` clause of an array method keeps it where it
was built, because the method runs it to completion before returning.

[closure-value-realization](closure-value-realization.md) records that transfer in its consequences
rather than deciding it: "the deferred-effect submits take a built closure and move it into the
region, which is the established consumption of a handle that must outlive the call it was built
in". The analogy is to a value handle, which is copied and moved freely. A closure value is not
that. Its own execution state binds to its address -- entering a coroutine body builds a frame that
reads every capture through the closure's pointer -- so the code already has to work around the
move, and says so where it does: building the frame "cannot happen until this environment is where
it will stay".

Two costs follow, on two different destinations.

**Allocation, on the region path.** One nonblocking assignment builds a closure, and that costs one
allocation for the arena-owned value, one for its slot pointer vector, one per capture, and one more
for the copy the region takes -- **N + 3**, with the moved-from husk still held by the arena until
the stretch returns. Every nonblocking assignment in the design pays it, and the nonblocking
assignment is already the dominant cost family in the integration profile.

**A two-phase choreography, on the `fork` path.** The frame cannot be built until after the move, so
construction and activation are two steps with an ownership transfer wedged between them. That
sequencing exists for no reason except the move.

Neither is a property of what a closure is. Both are properties of building it somewhere it will not
stay.

## Decision

**A long-lived runtime object whose internal execution state binds to its address is constructed
directly in its final lifetime home. Where the home is known before activation, build-then-move is
not used.**

**D1. The closure value is constructed in its final home.** The region path constructs the shared,
region-owned value directly; the `fork` path constructs into the storage the execution will hold.
Nothing is built in the arena and transferred.

**D2. The construction entry takes the storage to construct into.** One operation -- make a closure
-- that receives a caller-provided home, rather than one entry per destination. This is the shape
the runtime already uses where a caller owns the storage and the runtime fills it.

**D3. Captures become inline.** With the block's owner no longer moving, a closure meets the
condition in [inline-member-slots](inline-member-slots.md) D1, so its captures are inline like an
object's properties and a scope's members. That entry's D3 exception does not need amending; it was
written as a condition on the owner, and the owner's property is what changed.

**D4. Activation happens at construction.** With the value already where it will stay, a coroutine
body's frame is built when the closure is built. The deferral disappears rather than being kept as a
safety rule.

**D5. The `with` clause is unaffected.** Its body is borrowed, runs to completion inside the stretch
that built it, and never moves. It keeps the arena.

## Invariants

1. An object whose execution state binds to its own address is never relocated after that state
   exists. The way this is guaranteed is that it is never relocated at all.

2. Where the final home is known before the object is activated, the object is constructed there. A
   move is admissible only where the home is genuinely not known until later, and that case must be
   argued rather than assumed.

3. The storage a construction fills is the caller's to supply. The runtime does not choose an
   object's home and then hand it over.

## Rejected

- **Building in the arena and moving.** The shape in place. Its recorded justification is an analogy
  to a value handle that must outlive its call, and the analogy does not reach an object whose own
  frame points into it -- which is why the code that performs the move also has to defer activation
  past it. It costs N + 3 allocations per nonblocking assignment and leaves a husk in the arena for
  the rest of the stretch.

- **One construction entry per destination.** Three entries -- make-for-region, make-for-fork,
  make-for-array-method -- each constructing into the home it knows. Rejected against
  [runtime-entry-naming](runtime-entry-naming.md): an entry is named by the operation it performs,
  and the operation is making a closure. The destination is the caller's, and D2 is how the caller
  says it.

- **Keeping the move and indirecting the captures permanently**, so that moving the block stays
  cheap. This is today's shape stated as a design rather than an accident. Rejected because it buys
  movement nothing needs with an indirection everything pays, which is the trade refused throughout
  the storage model.

- **Making the capture slots relocatable so a closure can be moved after activation.** It would keep
  build-then-move and make it safe. Rejected for the reason a slot is non-movable everywhere else: a
  cell's identity is its address, and a relocatable slot would make every reference into a block
  conditional on nobody having moved it.

## Consequences

- **A nonblocking assignment costs one allocation instead of N + 3.** The region-owned value is
  constructed once, with its captures inside it. This is the largest single effect of the decision
  and the reason it is worth doing before the value representation is settled.

- **The `fork` path loses its two-phase sequencing**, and what replaces it is the destination
  acquisition: the execution's storage has to exist before the closure is built into it. That is a
  question about how a destination is obtained, not about what a closure is, and it is the one piece
  of this that is not mechanical.

- **The construction protocol is decided in shape and open in detail.** D2 fixes make-into; which
  entry supplies the home for each destination, and how the fork path orders execution creation
  against closure construction, is the remaining work.

- **[inline-member-slots](inline-member-slots.md) becomes uniform.** All three users of a storage
  block -- an object's properties, a scope's members, a closure's captures -- hold their slots
  inline, and no user of that entry is an exception. The entry itself does not change, which is what
  stating it as a condition rather than a list bought.

- **A capture still has no storage identity.** Nothing reads a capture except through the closure's
  own access, and nothing takes a capture slot's address. This decision does not give captures
  identity and is not motivated by their having it; if a reference is ever to bind one, the storage
  is already in the shape that would allow it.

## Cross-references

- [closure-value-realization](closure-value-realization.md) -- the closure value this changes the
  construction of; its transfer consequence is what this replaces.
- [inline-member-slots](inline-member-slots.md) -- the condition a closure now meets, removing its
  one exception.
- [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) -- why a slot is non-movable,
  which is what makes build-then-move expensive rather than merely inelegant.
- [runtime-entry-naming](runtime-entry-naming.md) -- why the destination is a parameter and not a
  family of entries.
- [generated-behavior-boundary](generated-behavior-boundary.md) -- the bodies the runtime runs on
  the program's behalf, which are what gets constructed.
- `../architecture/lifetime.md` -- the regimes a closure's final home belongs to.
