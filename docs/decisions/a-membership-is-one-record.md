# A membership in a wake target is one record, owned by what shares its life

Date: 2026-07-13, revised 2026-10-05. Status: accepted.

## Why this decision matters

A suspended activation can only be resumed because something holds the means to resume it: a
value-change observable, a named event, a process's termination, a `wait fork` condition, a
scheduler region queue, a delay slot. Process control (LRM 9.7) releases an activation **while it is
parked**, and a `disable` (LRM 9.6.2) reaches every process inside a target, so every one of those
holders must be able to let go of what it names, or `disable fork` leaves a token that later resumes
freed storage.

`activation.md` already required that the relation be one the owner can end. What it did not settle
is how the relation is _recorded_ -- and that omission is what this entry fixes, because the first
implementation got it wrong despite following the contract to the letter.

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
- a target's removal was forbidden from touching the activation's list, because the activation's own
  removal was iterating it;
- a target removed an activation by **searching** its waiter records for it -- although the caller
  already held it;
- the engine removed one by **scanning every queue it owns** -- region queues, delay slots, the
  in-flight drain snapshot -- which forced a helper whose only purpose was to enumerate them so the
  scan could reach everywhere;
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
membership  = one record: "this target can reach this owner"
  prev / next  -- linked into the target's list
  what it names, and whatever the target asks of it

kinds of membership, each owned by what shares its life:
  a wait's membership on what it watches     owned by the wait      (the body's frame holds it)
  a process's membership on a disable target owned by the process's record of being inside
  an activation's place in a scheduler queue owned by the activation

owner   holds its memberships; ending it destroys them, and each unlinks itself
target  holds a list of one kind of membership; it links them and never copies them
```

The target's list answers "who can I reach" (walk it when the target fires); the owner answers "what
am I in" by holding the records themselves. Both reach the **same** object, so neither can hold a
belief the other has abandoned, and neither has to search the other.

Leaving a list is pointer surgery on the ring:

```text
prev->next = next;
next->prev = prev;
```

which needs nothing but the node -- not the target, not its list head. That is the load-bearing
property: it is why a membership carries no back-pointer to its target, why no interface exists to
dispatch through, and why cancelling an activation parked anywhere costs the same constant time.

## The decisions

```text
D1. A membership is ONE record. One end owns it -- whatever shares its life -- and the target links
    it. Neither end stores a second description of the relation. A target holding a raw token for
    an activation, or an activation holding a list of the targets that hold it, are the same
    forbidden shape stated from opposite ends.

D2. A target's list holds memberships of one kind, so walking it needs no question about what each
    node is. Which kinds exist is a closed set: a wait's, a process's inside a disable target, an
    activation's in a queue.

D3. Leaving a list is a detach, never a search. A membership unlinks itself using its own links
    alone. No target is consulted, no container is scanned, and no side needs to know what kind of
    target the other end is.

D4. Condition data belongs to the membership, not to either end. A value-change wait's bit
    projection describes the (observable, wait) pair, so it lives on the membership -- and the test
    is that description: what an event control compares is the value of one expression, which the
    leaves of that expression share rather than each holding, so it is reached from the membership
    rather than copied onto each. A target that fires unconditionally carries none.

D5. A membership stands for as long as its owner does, and being reached does not consume it. A
    wait's memberships stand for the wait's life, and reaching one asks the wait whether an
    activation is parked on it now and whether this is an event for it; a process's stands for as
    long as it is inside the target; a queue place is linked while the activation is queued.
    Whether something is parked is the wait's gate, not the membership's presence.

D6. Cancellation safety is RAII, not a protocol. Releasing an activation destroys its frame, and
    with it every wait the frame holds and the memberships each owns, and its queue place; leaving
    a disable target destroys that membership. No caller has to remember to unlink, so no caller
    can forget.

D7. The engine is not special. A region queue and a delay slot are targets exactly as an event or an
    observable is: an activation sits in them, and cancelling it while queued is the same detach.
    The engine implements no membership interface and is never searched.

D8. There is no membership-target interface. Because the detach in D3 is pointer surgery, no
    virtual dispatch exists to abstract over. A target is a list plus its own fire rule, not an
    implementor of a contract.
```

## Rejected alternatives

- **Two authoritative copies of the relation** (a waiter record on the target, a target list on the
  activation). What was built first. It forces idempotent enrollment, two teardown directions,
  iteration restrictions, search-based removal on the target, a whole-scheduler scan on the engine,
  and destructor walks whose only job is to fix the other copy. Every one of those is the cost of
  the duplication, not of the relation.

- **Every membership owned by the activation.** What this entry first said, when the activation was
  the only thing a membership could serve. A wait that stands for a body's whole run outlives every
  stop the activation makes at it, and a process is inside a disable target across many activations,
  so tying each to the activation either builds and tears them down at every stop or gives the
  activation records of lives that are not its own.

- **A `WakeSource` / `RegistrationTarget` interface implemented by each target.** The abstraction is
  drawn from what the implementations happened to share (they all hold a token), not from what the
  relation requires. Once leaving is a detach, there is no dispatch left to abstract, and the
  interface is pure ceremony -- plus it makes the engine claim to be a "wake source," which a region
  queue is not: an activation in it is already runnable, not waiting for anything.

- **Requiring every scheduler container to adopt one physical realization.** The requirement is
  constant-time removal through the membership's own identity, not container uniformity. A target
  may realize its list however its ordering semantics demand, provided it honors the membership's
  lifecycle.

## Consequences

- Each owner holds its memberships in stable-address storage: a target's list points at those
  addresses, so adding a membership must not move the ones already linked.
- Leaving a target, cancelling a queued activation, and cancelling a parked one are all one
  constant-time detach. The scheduler is never searched for an activation.
- Moving a whole set of activations between scheduler queues -- taking a region's snapshot out
  before draining it, so that work the drain produces waits for the next pass -- is a constant-time
  relink of one list onto another, not a walk that re-enrols each activation.
- Releasing a target detaches whatever is still linked to it, so an owner that outlives a target at
  shutdown never unlinks through freed storage.
- Adding a new kind of wait means giving a target a list. It does not mean teaching the scheduler a
  new place to search, and it cannot mean adding a token an owner is unable to end.
- Waking only queues, and a queued activation whose frame is released leaves its queue with the
  frame, so a bulk termination can wake activations as it goes even where a later step of it
  releases one of them.

## Cross-references

- `a-wait-is-storage-of-its-activation.md` -- why a wait is held in the body's frame for as long as
  what it watches stands, which is what moved a wait's memberships off the activation.
- `../architecture/activation.md` -- what an activation holds while it is parked.
- `../architecture/scheduling.md` -- the engine as a construct-neutral mechanism; a membership is
  not a construct hint the scheduler reads, and the engine still branches only on queue and region.
- `../architecture/identity_and_ownership.md` -- duplicate ownership as a forbidden shape, and the
  rule that a needed lookup is a symptom of misplaced ownership.
