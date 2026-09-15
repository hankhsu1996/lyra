# A storage block whose owner never moves holds its slots inline

Date: 2026-09-11 Status: accepted

## Context

One storage block realizes three things -- an object's properties, a scope's members, and a
closure's captures -- and it holds each slot behind its own allocation. The block is built once from
a schema, reserved to its exact size, and never grows.

Two facts make the indirection look like pure cost.
[referenceable-objects-have-stable-addresses](referenceable-objects-have-stable-addresses.md) fixes
that a referenceable object does not move, so a slot's address is already stable without a separate
allocation to make it so. And a slot is a variant over every member storage kind, so it is sized by
its largest alternative wherever it lives: separate allocation buys no space either.

The third fact is the one that changes the shape of the decision. **A slot is deliberately
non-movable** -- a value cell's identity is its address, so both the cell and the member storage
around it delete their move operations. A block of such slots therefore cannot itself be moved, and
today it is moved: a closure value is built in the per-stretch arena and moved into longer-lived
storage when a `fork` branch or a deferred body takes ownership of it. That works only because
moving the block moves a vector of _pointers_ while the slots stay where they are.

So the per-slot allocation is not buying address stability, which the owner now provides. It is
buying **movability of the block**, and only one of the three users needs it.

## Decision

**A storage block whose owner cannot move holds its slots inline: one allocation for the block, each
slot constructed in place within it.**

**D1. The condition is the owner's, not the block's.** A block may be inline exactly where the
object that contains it cannot move. That makes the rule checkable at each use site rather than a
list to keep current.

**D2. An object's properties and a scope's members are inline.** A class object is allocated in
place and does not move; a scope deletes its move operations and is built once into the instance
tree. Both meet D1, so `ref c.x` is the address of a slot inside the object's own storage.

**D3. A closure's captures keep the indirection until the closure value stops moving.** A closure
value is moved out of the arena that built it, so its block must be movable. This is a property of
how a closure value is transferred, not of what a capture is, and it is the thing to change if the
shapes are to be unified -- constructing the closure where it will stay rather than moving it there.

**D4. A slot stays non-movable.** That is not an obstacle to be removed. A cell's identity is its
address, which is what every reference decided this week depends on; a movable slot would
reintroduce exactly what those decisions took out.

**D5. Space amplification belongs to the value representation, not here.** A slot is sized by the
largest alternative of a type-erased member storage variant, and where that matters the answer is a
member layout computed per class from the declared types -- the native value model's work. Returning
to per-slot allocation would not fix it either, since the allocation is the same size.

## Invariants

1. A slot's address is fixed for the life of the block that contains it.

2. A block is constructed at its schema's size and never grows. Nothing appends to it.

3. Inline storage is used exactly where the owner cannot move. A block whose owner moves keeps its
   slots separately allocated, and the two are the same block type serving owners with different
   properties.

## Rejected

- **Keeping one allocation per slot everywhere.** The shape in place. It costs an allocation per
  property at every object construction and scatters a class's properties across the heap, and the
  stability it appears to buy is now supplied by the owner. What it does buy -- a movable block --
  is needed by one of its three users, so it is kept there rather than everywhere.

- **Making member storage movable so every block can be inline.** It would collapse the three users
  onto one shape. Rejected outright: a value cell's identity is its address
  ([reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md),
  [referenceable-objects-have-stable-addresses](referenceable-objects-have-stable-addresses.md)),
  and a movable cell would make every reference into a block conditional on nobody having moved it.

- **Holding the slots in a `std::vector` of slot objects.** The obvious way to write "inline". It
  does not compile against a non-movable slot, because a vector requires its element to be
  move-insertable whether or not it ever reallocates. Inline means storage the block allocates once
  and constructs into, with destruction in reverse order at teardown.

- **Giving closures a different block type.** It would let each shape be simple. Rejected because
  the three users are one relation -- a described shape and a value realizing it -- and splitting
  the type to encode a transfer property would put the difference in the wrong place. The block
  stays one type; D1 says which storage strategy an owner's block uses.

## Consequences

- **One allocation per object instead of one per property**, and a class's properties are
  contiguous, so constructing an object and walking its members both improve. The gain scales with
  property count and is largest for the many-small-property classes a testbench is made of.

- **Teardown is one deallocation and an explicit destruction pass.** It is not automatically
  simpler: what a per-slot owning pointer did implicitly becomes a reverse-order destroy the block
  performs. Fewer allocations, more explicit code.

- **The alignment the reference ABI requires is preserved.** Slots inline in a block are as aligned
  as the block, so the low bits a tagged reference uses stay free
  ([reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) invariant 5).

- **The closure path is the one thing left uneven**, and the decision names why rather than hiding
  it. Whether a closure value should be constructed where it will stay -- which its own contract
  already half-assumes, since starting one is only valid once it is where it will stay -- is a
  separate change on the closure's transfer, not on its captures.

## Cross-references

- [referenceable-objects-have-stable-addresses](referenceable-objects-have-stable-addresses.md) --
  the owner stability D1 is a condition on.
- [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) -- the reference that reads a
  slot address directly, and the alignment invariant inline storage preserves.
- [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) -- why a
  slot is plain or observable, which is what the variant's alternatives are.
- [member-slot-storage](member-slot-storage.md) -- the member slot this changes the allocation of.
- [closure-value-realization](closure-value-realization.md) -- the closure value whose transfer D3
  depends on.
- `../architecture/lifetime.md` -- the structural regime a scope's block lives in.
