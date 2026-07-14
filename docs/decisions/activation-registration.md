# An activation's membership in a wake target is one record, owned by the activation

Date: 2026-07-13. Status: accepted.

## Why this decision matters

A suspended activation can only be resumed because something holds the means to resume it: a
value-change observable, a named event, a fork's join or `wait fork` condition, a scheduler region
queue, a delay slot. Process control (LRM 9.6) releases an activation **while it is parked**, so
every one of those holders must be revocable, or `disable fork` leaves a token that later resumes
freed storage.

`activation.md` invariant 4 already required this, and named the unit: a **revocable registration
the activation owns**. What it did not settle is how the relation is _recorded_ -- and that omission
is what this entry fixes, because the first implementation got it wrong despite following the
contract to the letter.

## The failure this decision is drawn from

The relation "target T can currently resume activation A" was first recorded **twice**: the target
kept a waiter record naming the activation, and the activation kept a list naming its targets. Both
copies authoritative; neither derived from the other.

The implementation worked and passed the whole suite. It was still the wrong shape, and the evidence
was that **almost none of its machinery served the relation** -- it served the reconciliation of the
two copies:

- enrolling had to be idempotent, or the copies drifted when the engine moved an activation between
  its own queues;
- two teardown directions were needed (target-dies-first, activation-dies-first) to keep the copies
  agreeing;
- a target's revoke was forbidden from touching the activation's list, because the activation's own
  revoke was iterating it;
- a target revoked by **searching** its waiter records for the activation -- although the caller
  already held it;
- the engine revoked by **scanning every queue it owns** -- region queues, delay slots, the
  in-flight drain snapshot -- for one activation, which forced a helper whose only purpose was to
  enumerate them so the scan could reach everywhere;
- three destructors walked their waiter records purely to fix the other copy;
- cancelling an activation mid-drain could shrink the container an index-based drain loop was
  walking.

A structure whose surface is consistency maintenance is the wrong structure. The correct reading is
`identity_and_ownership.md`'s forbidden shape -- **duplicate ownership: the same relationship
represented in two authoritative places** -- and its note: if a fix needs a new lookup, the
ownership is wrong; move the data onto the entity that needs it and the lookup disappears.

The single-record design was on the table from the start and was rejected because it "changes every
scheduler container." That is a churn argument, and churn is not a design axis. Recording this is
the point of the entry: the shape was chosen by diff size, and the cost was a structure nobody could
hold in their head.

## The model

```text
Registration  = one membership: "this target can resume this activation"
  activation      -- who to resume
  prev / next     -- linked into the target's list
  fire condition  -- the edge and bit projection of a value-change wait (LRM 9.4.2);
                     a target with no condition (event, join, queue, delay) leaves it unset

Activation  owns its registrations. Releasing it destroys them.
Target      owns a list. It links registrations; it does not copy them.
```

Two indexes, one record. The activation's set answers "what can resume me" (drop them all on wake or
release); the target's list answers "who is parked on me" (walk it when the condition fires). Both
reach the **same** object, so neither can hold a belief the other has abandoned, and neither has to
search the other.

Revoking is pointer surgery on the ring:

```text
prev->next = next;
next->prev = prev;
```

which needs nothing but the node -- not the target, not its list head. That is the load-bearing
property: it is why a registration carries no back-pointer to its target, why no interface exists to
dispatch through, and why cancelling an activation parked anywhere costs the same constant time.

## The decisions

```text
D1. A membership is ONE record. The activation owns it; the target links it. Neither end stores a
    second description of the relation. A target holding a raw token for an activation, or an
    activation holding a list of the targets that hold it, are the same forbidden shape stated from
    opposite ends.

D2. The activation-side set and the target-side list are two INDEXES over that one record, never two
    records. An index may be reorganized freely; it may not restate the relation.

D3. Revoking is a detach, never a search. A registration unlinks itself from whichever target holds
    it using its own links alone. No target is consulted, no container is scanned, and no side needs
    to know what kind of target the other end is.

D4. Condition data belongs to the membership, not to either end. A value-change wait's edge and bit
    projection describe the (observable, activation) pair, so they live on the registration. A target
    that fires unconditionally carries none.

D5. A registration settles exactly once: the target claims it when the wait is satisfied, or the
    activation revokes it. Either way it is unlinked, and an unlinked registration can resume
    nothing. There is no third state and no re-arming.

D6. Cancellation safety is RAII, not a protocol. Releasing an activation destroys its registrations,
    which detach themselves. No caller has to remember to revoke, so no caller can forget.

D7. The engine is not special. A region queue and a delay slot are targets exactly as an event or an
    observable is: an activation parks on them, and cancelling it while queued is the same detach.
    The engine implements no registration interface and is never searched.

D8. There is no registration-target interface. Because the detach in D3 is pointer surgery, no
    virtual dispatch exists to abstract over. A target is a list plus its own fire rule, not an
    implementor of a contract.
```

## Rejected alternatives

- **Two authoritative copies of the relation** (a waiter record on the target, a target list on the
  activation). What was built first. It forces idempotent enrollment, two teardown directions,
  iteration restrictions, search-based revoke on the target, a whole-scheduler scan on the engine,
  and destructor walks whose only job is to fix the other copy. Every one of those is the cost of
  the duplication, not of the relation.

- **A `WakeSource` / `RegistrationTarget` interface implemented by each target.** The abstraction is
  drawn from what the implementations happened to share (they all hold a token), not from what the
  relation requires. Once revoking is a detach, there is no dispatch left to abstract, and the
  interface is pure ceremony -- plus it makes the engine claim to be a "wake source," which a region
  queue is not: an activation in it is already runnable, not waiting for anything.

- **Requiring every scheduler container to adopt one physical realization.** The requirement is
  constant-time revoke through the membership's own identity, not container uniformity. A target may
  realize its list however its ordering semantics demand, provided it honors the registration
  lifecycle.

- **One inline registration slot plus an overflow list** (to avoid allocation for the common
  single-membership case). Premature: it reintroduces the two-storage-path complexity this decision
  exists to remove, and no profile yet says the single path costs anything. Storage strategy is an
  implementation review, not part of this decision.

## Consequences

- The activation owns its registrations in stable-address storage: a target's list points at those
  addresses, so appending a membership must not move the ones already linked.
- Revoking, cancelling a queued activation, and cancelling a parked one are all one constant-time
  detach. The scheduler is never searched for an activation.
- Moving a whole set of activations between scheduler queues -- promoting the next delta, or
  releasing a delay slot when time advances -- is a constant-time relink of one list onto another,
  not a walk that re-registers each activation.
- Releasing a target detaches whatever is still linked to it, so an activation that outlives a
  target at shutdown never revokes through freed storage.
- Adding a new kind of wait means giving a target a list. It does not mean teaching the scheduler a
  new place to search, and it cannot mean adding a token an activation is unable to revoke.

## Cross-references

- `../architecture/activation.md` -- invariant 4 (the registration set) and the forbidden shape this
  entry's D1 states.
- `../architecture/scheduling.md` -- the engine as a construct-neutral mechanism; a registration is
  not a construct hint the scheduler reads, and the engine still branches only on queue and region.
- `../architecture/identity_and_ownership.md` -- duplicate ownership as a forbidden shape, and the
  rule that a needed lookup is a symptom of misplaced ownership.
- `../architecture/object_lifetime.md` -- managed object lifetime is reference-counted today, so a
  registration carries no tracing obligation; a future precise collector may reuse the activation
  and scheduling identity established here, but nothing in this decision anticipates it.
