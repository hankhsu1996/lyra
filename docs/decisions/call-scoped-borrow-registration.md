# Detached storage is reclaimed by call-scoped borrow registration

Date: 2026-09-11 Status: accepted

## Context

An element removed from a queue or an associative array, and a dynamic array generation replaced by
a resize, must go on existing while a reference to it is live (LRM 13.5.2). Both
[container-element-storage](container-element-storage.md) and
[array-element-storage](array-element-storage.md) state that requirement and neither says who keeps
the storage alive or when it is reclaimed.

**The bound the language gives is static and structural**, which is what makes a general lifetime
mechanism unnecessary. The clause says the element "shall continue to exist within the scope of the
called subroutines until they complete", and three separate rules close every route by which a
reference could outlive that scope:

- A reference cannot be stored: the source language has no reference data type.
- A reference cannot reach a detached branch: LRM 9.3.2 makes a `ref` formal illegal inside
  `fork ... join_any` or `join_none` unless it is `ref static`, and 13.5.2 bars `ref static` from
  being passed "elements of dynamically sized array variables".
- A reference cannot be captured by a deferred effect: LRM 6.21 p.134 bars elements of dynamically
  sized array variables from being written by nonblocking, continuous or procedural continuous
  assignment, and adds that "references to automatic variables and elements or members of dynamic
  variables shall be limited to procedural blocks".

So nothing needs to discover at run time how long a detached element must live. What it needs to
know is only whether any call that was handed it is still running.

## Decision

**Storage keeps its owner; a call registers a borrow of it; retired storage is reclaimed when the
last borrowing call has ended.**

**D1. Ownership does not transfer.** The slot arena owns a queue or associative element's storage
and the array owns its generation, before and after detachment alike. A call extent registers a
borrow; it never takes ownership, so the detachment site does not have to decide whom to hand the
storage to -- which it could not, since two calls may hold references to one element.

**D2. The unit of retention is the live call extent, deduplicated per storage identity within one
invocation.** `foo(q[1], q[1])` borrows one storage from one invocation, and both formals end
together, so it is one retention rather than two. A realization may count registrations instead of
holding a set, because every registration of one invocation is released at that invocation's end, so
the count reaches zero at the same moment the last extent closes.

**D3. Registration happens at a source-level bind, and forwarding is not a bind.** A `ref` formal
handed on to another `ref` formal denotes the storage at the end of the chain rather than binding
afresh, and the outer invocation's registration already covers the inner call's use. Only an actual
that names the container element registers.

**D4. A call that binds container storage opens a call extent.** It is opened before the actuals
bind and closed when the invocation completes, by every way out: falling off the end, a control
effect leaving a region, and the driver ending a parked execution.

**D5. Reclamation is conditional on both facts.** Storage is recycled when membership has ended and
no live call extent borrows it. Either alone is insufficient: still a member means still reachable,
still borrowed means still required.

**D6. Nothing reaches the access path.** A reference stays one tagged pointer, copying one changes
no state, and a read or a write through one performs no bookkeeping. The only points that touch the
registration are a source-level bind and an invocation's completion, and both are statically placed
and few.

**D7. This is strict-mode machinery.** A fast mode that rejects a destructive mutation while a
borrow is live removes the registration and the retired state entirely, leaving the same storage
reached by the same pointer. The separation is clean because the bookkeeping sits at invocation
boundaries rather than in the access path.

## Invariants

1. Physical storage has exactly one owner at every moment, and that owner is never a call extent.
   Detachment changes membership and retention, never ownership.

2. Lifetime bookkeeping occurs only where a source-level bind happens and where an invocation
   completes. It never occurs on an access, on a reference copy, or on a reference being passed on.

3. Every exit from an invocation unregisters what that invocation borrowed. An exit the body does
   not spell is still an exit.

4. A slot and a generation follow the same protocol. Only the granularity of the retained thing
   differs -- one element for a queue or associative array, a whole generation for a dynamic array.

## Rejected

- **Reference counting the storage, in the manner of a shared pointer.** Already rejected by
  [container-element-storage](container-element-storage.md) on the ground that it "would put its
  traffic on every bind and every release to pay for a case that is rare". This decision is not that
  and the difference is worth stating precisely: what is counted here is **live invocations at
  invocation boundaries**, not references at every copy. A reference copy, a forward, a read and a
  write all touch nothing. The rejected shape puts traffic on the access path; this one cannot,
  because the access path has no access to the registration.

- **Transferring ownership of the detached storage to a frame.** The shape considered first: at
  detachment, hand the storage to the frame that holds the reference. It cannot be written, because
  the detachment happens wherever the container is mutated -- which may be a different call from the
  one holding the reference -- and because two calls may hold references to one element, so there is
  no single frame to hand it to. The failure is not a difficulty of implementation; the question
  "which frame owns it" has no answer.

- **A container-side retire list, recycled when the borrow ends.** The container marks the slot
  retired and keeps it. It does not reach the dynamic array, where the retained thing is a
  generation rather than a slot, and it requires the container to outlive its own replacement, which
  is exactly the operation that causes an array's detachment.

- **A general lifetime token carried by the reference.** The most general answer, and the one the
  three clauses above make unnecessary: the bound is static, so nothing has to be carried. It would
  also widen the reference, which [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md)
  fixes at one word.

- **Registering at block granularity rather than per call.** The lowering's existing extents are per
  block, so this would be the cheap way to reuse them. It is conservative rather than wrong -- it
  never reclaims too early -- and it is rejected because the over-retention is unbounded: a loop
  that borrows and detaches once per iteration accumulates every detached element until the
  enclosing block ends, where the language's bound would have released each at its own call.

## Consequences

- **The call extent is the one new compiler concept**, and it is a placement rather than a kind. The
  cleanup machinery it hangs on already exists, and all three exits already walk it: falling off the
  end of a scope, a control effect leaving a region, and the abandon path a driver takes when it
  ends a parked execution rather than resuming it. What is new is that a call opens one; today
  nothing does, and the existing region scope is the callee's rather than the call site's.

- **The cost is per container-element bind, and is paid whether or not a detachment follows.** A
  bind that names a plain variable pays nothing, because the lowering can see which kind of storage
  an actual names. This is the price of the model and the clearest thing that would argue for fast
  mode.

- **A suspending callee is covered without a special case.** A task call does not return until the
  task completes, so the caller's extent spans the suspension, and the abandon path covers the
  execution being ended while parked.

- **Where the extent is opened in the lowering is the implementation question left.** A call is an
  expression and the existing extents are opened per block, so a call that binds container storage
  needs a scope of its own. The machinery is a stack push and pop; what is missing is the site that
  performs it.

## Cross-references

- [container-element-storage](container-element-storage.md) -- detachment for a queue and an
  associative array, whose reclamation this settles.
- [array-element-storage](array-element-storage.md) -- the retained generation, which follows the
  same protocol at a different granularity.
- [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) -- the reference this leaves
  untouched, and the one-word width that rules out a carried token.
- [referenceable-objects-have-stable-addresses](referenceable-objects-have-stable-addresses.md) --
  the sibling rule for a managed object, where survival is likewise a fact about the storage rather
  than retention by the reference.
- `../architecture/storage.md` -- membership, position, identity and duration as four properties, of
  which this decision implements the last.
- `../architecture/lifetime.md` -- the regimes, and the rule that every exit owes the endings a
  scope opened.
