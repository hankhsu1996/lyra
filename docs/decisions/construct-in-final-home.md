# Construct in the final home, never build and move

Date: 2026-09-11 Status: accepted; realized 2026-09-25 with D2, D4 and D5 revised; D2 restored and
D5 reread for a sort's order 2026-10-17; the region path made where its keeper says 2026-10-24 (see
the last three sections)

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

## Revised when realized, 2026-09-25

The realization gives every closure the same home -- an allocation of its own, the closure first and
its capture slots after it, which never moves -- and what the building body holds, and hands to
whatever keeps the closure, is that allocation's owning hold, the only one. Three decisions above
change with that.

- **D2, and invariant 3.** The construction entry makes the allocation rather than filling one the
  caller supplies. What invariant 3 guarded against was the runtime choosing a home per destination
  and handing the object over, and "which entry supplies the home for each destination" was the part
  this entry left open. With one home for every closure there is no destination to choose and
  nothing to hand over but the owner: the object stays put and the hold on it moves, exactly as a
  class handle does.
- **D4.** A coroutine body's frame is built where the execution is entered, at once, rather than
  deferred to the execution's first resume. The deferral existed only to survive the move. Without
  it, an execution holds the same two things whichever way it was entered: a frame built and not yet
  begun, and whatever that frame reads that nothing else keeps alive -- the hold on the closure's
  allocation for a spawned branch, nothing for a body reading a receiver -- ended after the frame.
  That is also where the field puts it: a C++ coroutine's parameter copies end after its promise,
  and a Rust `async move` future owns what it captured. The owner stays with the execution rather
  than travelling into the frame, because a body takes its receiver as a borrow whatever it is.
- **D5.** The `with` clause's closure lives in an allocation of its own like every other, borrowed
  for the call and ended after it, rather than in the frame. One home serves every consumer with no
  branch on which consumer it is, and the arena this entry kept for it had no other user.

A fourth keeper is added to the three named above: an event control's observation, which runs its
closure each time a change asks whether the watched expression moved (LRM 9.4.2), holds the owner
the same way. Measured on the RISC-V core before this: 1.17 M closures built at about 800
instructions each, 707 M of it building capture storage.

## Revised again, 2026-10-17: MIR states the home

The 2026-09-25 home -- an allocation of its own for every closure -- was the execution backend's
choice, made for every consumer alike, and MIR said nothing of it. Once a value too large for a
frame is held through a box MIR states
([a-value-too-large-for-a-frame-is-held-through-a-box](a-value-too-large-for-a-frame-is-held-through-a-box.md)),
a closure whose captures make it large was boxed by MIR around a value the backend had already put
in an allocation, and the backend could not say how large the boxed value was. So the home is MIR's
to state, and D2 and invariant 3 hold as first written: the storage a closure is made in is the
caller's.

- A closure kept past the statement building it -- by a region, a timer, an observation, an
  assertion's action or a foreign fiber -- is made by a new-expression, and the keeper takes the
  box. That is still one allocation per nonblocking assignment.
- A closure a sort borrows for its order lies where its value is, in the frame, as D5 said of a
  borrowed body; it costs no allocation. A `with` clause is no closure at all but a loop at its call
  ([an-array-method-is-a-loop-at-its-call](an-array-method-is-a-loop-at-its-call.md)).
- A closure started as a coroutine is made by a new-expression and called through its box, which the
  started body takes over as its receiver, so the box ends when the body does. D4's revision kept
  the hold beside the frame because a body then took its receiver as a borrow; a body that takes its
  closure over has nothing left beside it to keep.
- The code building the closure builds it in the storage it is given, storing its definition's
  address first as a constructor stores a class's table address; no library entry makes it. Nothing
  moves a closure once made, which invariant 1 requires.

## Revised again, 2026-10-24: a region's effect is made where its keeper says

The 2026-10-17 revision made MIR state the home and left the region path at "one allocation per
nonblocking assignment": a box of its own, which the region took and freed after running it. That
allocation and its release were about a fifth of the instructions of a benchmark that does little
but nonblocking updates. D1 as first written already named the home -- the region path constructs
the region-owned value directly -- and what was missing was a way for MIR to say so.

- An effect lasts exactly as long as its region holds it, so the region's effects are a keeper: they
  hand out where to make one, and they commit what was made there, named by the address it was made
  at, so nothing is remembered between the two. Making an effect there is one generic function of
  the unit, `emplace_into<K, T, F>(K* keeper, const F* make)`: it asks the keeper for storage sized
  and aligned for `T`, builds `T` there from what calling `make` answers, and commits it. That is
  C++'s `emplace`, which builds its element where the container holds it from arguments it is handed
  by reference; `F`, a closure answering `T`, stands for those arguments, since MIR has no argument
  pack, as Rust bounds such a parameter by `F: FnOnce() -> T`. The body uses nothing but MIR's own
  construction and call: a placement new-expression, whose allocation function the keeper's type
  supplies, and the keeper's commit.
- The values the effect is made of are read into the frame where the statement is reached, before a
  delay is evaluated (LRM 9.4.5), and the maker refers to each where it lies there, so the effect is
  built once, in the keeper's storage.
- The keeper's type says which protocol it keeps by -- a region's effects, or the keeper of
  violation reports, which keeps a report in the Observed region unless its process is flushed first
  -- as a Rust trait's impls do: the definition bounds `K` by keeping deferred effects, and each
  keeper type answers that bound by its own allocate and commit entries.
- A node of its own for making a closure in a keeper, naming the keeper and the closure's
  construction, was built first and dropped: it wrote one library type's operation as an IR node,
  which neither clang nor rustc has, where both state the same thing as a library function over
  placement new. A placement new-expression with a keep after it at every site was dropped too: the
  keeper was stated twice per site, once where the closure was made and once where it was kept. The
  generic function states it once, as its argument.
- A region's commit allocates nothing, so an effect made for a region is always kept. The keeper of
  violation reports commits a report by making, in the Observed region, the condition that holds it,
  so its commit can raise for a failed allocation alone, and the report then ends unrun, as the
  statement that raised keeps nothing. A maker that raises leaves bytes handed out and never
  committed, which are never handed out again; only a failed allocation can raise there.
- The pointer type gains no allocator. A box carrying its allocator in its type (Rust's `Box<T, A>`)
  was considered and rejected: its one reader would be the keep that takes it at once, so the
  allocator would be stated in a type only to be read off by the next node.
- Where the bytes come from is the run's, not the region's: one storage for every effect of the run,
  a free list per size class refilled a block at a time, as SystemC keeps its timed events
  (`sc_event.cpp`, `sc_event_timed::allocate`). An effect's bytes go back when it ends, so what is
  held follows how many effects are pending at once and how large each is. This is a closure type's
  free list stated for a runtime told sizes rather than types: every closure of one size shares a
  class.
- An arena per region, made new after each pass, was built first and dropped. Every region of every
  later slot that held an effect took a first chunk of its own, so 100,000 updates each due at a
  time of its own held 513 MB at their peak, where one allocation per update had held 88 MB. LLVM's
  `BumpPtrAllocator` and rustc's arenas are built for a few long-lived arenas holding many objects,
  not one per due time.
- Every effect a region holds is made so, the library's own included; closures of any other keeper
  keep a box.

## Cross-references

- [a-unit-defines-generic-functions](a-unit-defines-generic-functions.md) -- the generic function a
  region's effect is made by.
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
