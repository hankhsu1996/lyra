# Storage owns its value, and a referenceable component is storage

Date: 2026-09-11 Status: accepted

## Context

Two accepted decisions left the same question open, in nearly the same words.
[jit-value-realization](jit-value-realization.md)'s revisit: "whether a place owns a value's
representation or holds a handle to an independently lived one -- invariant 6 being the written form
of the second answer, taken when only the transient case was in view."
[reference-binds-a-cell](reference-binds-a-cell.md)'s reopening names it as the one thing still
unsettled: "Whether a place owns a value's representation, or holds a handle to an independently
lived one. The two answers want different things from a reference and from where a representation
lives." Neither could answer it, because the answer turns on what the language requires of a
component's identity and on what stable storage costs, and neither was established then.

Both are now. `../architecture/storage.md` states the language side: a variable, a class property, a
member of an unpacked structure, and an element of an unpacked array each have an identity a second
name may denote, and that identity is independent of the component's position, of its membership in
a container, and of the value its parent currently holds. And the cost side was measured rather than
argued, on containers and reference representations in isolation.

What the measurement found is not what the hesitation assumed. Stable component storage was expected
to trade access speed for correctness; it does not. And the shape it would replace turned out not to
be merely slower: it answers legal programs wrongly.

## Decision

**A storage entity owns the representation of its current value, and every component the language
gives storage identity is itself a storage entity.**

**D1. A storage entity owns its value's representation.** A second name for that entity is an alias
to the storage, not a handle to an independently lived value object that the storage happens to
point at. This is the first of the two answers the open question named.

**D2. A reference is pointer-like to stable storage.** The hot path is a load or a store through it.
A reference may carry a small tag where storage classes genuinely differ -- plain against observable
-- but it is never an owner plus an access path to re-evaluate, and never a decode chain walked per
access. Measurement supports the tag and refuses the chain: every representation tried lands within
1.4x of a raw pointer, the cost tracks the number of dependent loads and nothing else, and a
predictable arm test costs nothing measurable.

**D3. A fixed unpacked array has persistent per-element storage and an unpacked struct has
persistent per-member storage.** Each component is real storage with its own identity, not a part of
one replaceable whole value. So a component write lands in that component, a whole-aggregate
assignment writes into the components that are already there (LRM 7.6, 10.9.2), and a reference
bound to a component observes the new value and stays valid across it.

**D4. A component with no storage identity stays a value projection.** A packed bit or part select,
a packed struct or union member, an unpacked union member, and a string character select are views
of one storage object, so each keeps the functional whole-value update.
[value-projection-write](value-projection-write.md)'s D1 -- that a target is either a place or a
value projection, stated structurally rather than re-derived per backend -- survives intact; what
moves is only which components fall on which side of it.

**D5. The variable-size containers are not settled here.** A dynamic array, a queue, and an
associative array each need element identity that survives position changes and membership changes,
and an element that outlives its own removal for as long as a reference to it is live (LRM 13.5.2).
That their elements need stable identity follows from D1 and D3; which representation provides it
does not. [container-element-storage](container-element-storage.md) answers it for the queue and the
associative array; the dynamic array is still open.

## Invariants

1. A component the language gives storage identity is addressable. A component it does not is
   reached by value projection, and no consumer may treat the two alike.

2. A whole-aggregate assignment to a fixed-size aggregate does not replace the object holding its
   components. Replacing it is the forbidden shape `../architecture/storage.md` names, and a
   reference to a component is what it breaks.

3. A reference never re-evaluates the access path that resolved it. The path is evaluated once, at
   the bind.

4. **Value semantics are preserved by copying at assignment, not by making a representation
   immutable.** An aggregate that owns its components copies them when it is assigned, so a write
   through a reference to one component is unobservable through any copy -- which is what value
   semantics require. Immutability is one way to reach that property and not the only one.

## Rejected

- **Immutable value objects with functional whole-value updates, for the identity-bearing
  components.** The shape this replaces. [value-projection-write](value-projection-write.md) D4
  rejected an in-place component reference on the ground that "an in-place interior reference would
  alias a value's part across copies, and value semantics forbid observing a sub-write through a
  copy." That reasoning holds where the aggregate is reached through a shared handle, which
  [jit-value-realization](jit-value-realization.md) invariant 6 made universal; it does not hold
  once the storage entity owns its value, because then an assignment copies the components and there
  is no shared object for a sub-write to be observed through. Invariant 4 above is that correction.

  The stronger reason is that the functional form does not merely cost something here -- it is
  wrong. Measured on 2026-09-10 against the C++ backend, where this shape is realized: a reference
  bound to a container element behaves as a designator over the container value **as it was at the
  bind**, so a write through it stores that stale whole value back and discards every change made to
  the container since. With `que = '{0,1,2,3}` and a reference bound to `que[2]`, a callee's own
  `push_front` is not visible to the caller at all, and after a write of 777 through the reference
  the caller's queue reads `'{0, 1, 777, 3}` -- the push gone, the write landed at the pre-push
  index. The same failure appears with a single reference bound and the mutation performed by a
  different process, so it is not an artefact of two references interacting. No case in the corpus
  sees it, because no case in the corpus mutates a container while a reference to one of its
  elements is live.

- **One storage representation for every container kind.** Measured, the contiguous-payload form
  with an id-to-position map is the fastest representation on every ordinary access -- 0.48x a deque
  on dependent random read, 0.08x on iteration -- and costs 1080 ns on `push_front` at 4096 elements
  against the deque's 1.2 ns, because every position shifts and every shifted id must be rewritten.
  A fixed unpacked array never performs that operation and a queue performs it constantly. One
  representation cannot serve both well, and the per-category choice is therefore part of the design
  rather than a later optimization.

- **Dynamic per-element promotion on the hot path.** Promoting a component to stable storage only
  when something takes a reference to it would need a per-access test asking whether this component
  has been promoted. Every stable form measured costs one uniform extra dependent load, about 1.1
  ns, so there is no expensive case for such a test to avoid -- the test would cost more than the
  indirection it was added to dodge. Where a promotion decision is worth making it is made
  statically, per declaration, the way a lent local's storage is already decided at MIR-to-LIR.

- **Making every storage object a managed heap object.** The model that makes component identity
  trivial: allocate each one separately and let references retain them. Rejected on the north star
  -- a simulator spends its life reading and writing small values, and per-component allocation buys
  pointer chasing, poor locality and allocation pressure on exactly that path. Measurement bounds
  the claim: a node-per-element container pays 4164 allocations where a slab pays 70 and a slot
  arena 48 for the same 4096 elements and the same identity guarantee, and it is the only form that
  degrades under churn (+29% on dependent random read). Per-element allocation is a property of one
  implementation choice, not the price of stable identity.

## Consequences

- **Stable component identity is cheaper than the shape it replaces on the operations the language
  constrains.** One extra dependent load on small-payload random access (about 1.30x), nothing
  measurable on sequential iteration, and nothing at a realistic payload size. Middle insert and
  delete become 23x to 48x cheaper, because a representation with stable components moves handles
  where a contiguous one moves payloads. The trade this decision was feared to make does not exist.

- **The execution backend's refusal to lend a component becomes a gap against a decided shape.** It
  refuses every component reference today with one diagnostic; that refusal is now measured against
  this decision rather than against an open question.

- **The C++ backend's in-place component write stops being an optimization and becomes the required
  shape.** [value-projection-write](value-projection-write.md) D3 admitted it as behavior-preserving
  where a write proxy was semantically equivalent; for an identity-bearing component it is what the
  model states, and the designator-over-the-owner path is the defect above rather than the fallback.

- **An obligation on the IR and runtime contract, not discharged here.** A component with storage
  identity has to be nameable as storage, and LIR's place vocabulary has member and dereference
  steps and no index step. What that contract becomes -- and how a detached element's ownership is
  expressed -- is a separate design, deliberately not decided in this entry.

- **`jit-value-realization` invariant 6 no longer describes the identity-bearing aggregates.** Its
  ABI half -- values crossing as opaque handles, the runtime performing every operation -- is
  untouched; what this answers is the ownership half that its own revisit named.

## Cross-references

- [jit-value-realization](jit-value-realization.md) -- invariant 6 and the revisit this answers.
- [reference-binds-a-cell](reference-binds-a-cell.md) -- reopened on the same open question; its
  settled half, that a lending requirement must not decide a place's representation, stands.
- [value-projection-write](value-projection-write.md) -- its place-versus-projection boundary
  stands; its functional update and owner-relative projection reference are superseded for the
  components that have storage identity.
- [value-projection-designator](value-projection-designator.md) -- the designator shape, which
  remains the form for the components D4 keeps as projections.
- [jit-aggregate-realization](jit-aggregate-realization.md) -- erasure as the aggregate realization;
  this decision constrains what an erased aggregate must do for its components, not whether it is
  erased.
- [variable-lifetime-storage](variable-lifetime-storage.md) -- the automatic-versus-static lifetime
  model this assumes and does not revisit.
- `../architecture/storage.md` -- the language contract this is the realization answer to.
- `../architecture/lifetime.md` -- the regimes, and why placement is not part of one.
