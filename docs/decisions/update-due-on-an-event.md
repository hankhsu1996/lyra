# An update due on an event is carried by an execution of no lineage

Date: 2026-09-05 Status: accepted

## Context

A deferred effect states when it runs as a **placement**: a time slot and a region of it
(`architecture/scheduling.md`). A nonblocking assignment submits a closure to one -- this slot's NBA
region, or the region of the slot an intra-assignment delay names -- and the procedure carries on.
Every deferred effect in the simulator had a placement it could state where the statement stood.

`a <= @(ev) b` does not. Its region is the ordinary one (LRM 4.4.2.4), but its slot is whichever one
the event happens in, and nothing at the statement can say which that is. The RHS is read where the
statement is reached and the procedure does not block (LRM 9.4.5), so what is owed is a placement
made later, by something that watched for the event.

Only two things in the runtime watch for an event, and both name an activation: an observable cell's
membership list and a named event's. So either a membership learns to name something other than an
activation, or the thing that waits is an activation.

## Decision

The update is carried by an **execution of no lineage**: a coroutine that holds what the update
needs, waits for the event, places itself in the NBA region of the slot it lands in, and makes the
write there.

Three things follow, and each is the point.

**What the update writes and where it writes it are frozen where the statement stands**, into that
execution, by the same freeze an ordinary nonblocking closure uses -- the target's root captured as
a reference, the selector coordinates and the operands snapshotted. LRM 10.4.2 settles both there,
so a later write to the index or the operand does not reach an update still to come.

**It belongs to no lineage.** LRM 9.4.5 makes no process of the update, so nothing that names
processes may find it: `wait fork` (LRM 9.6.1) does not wait for it, `disable fork` (LRM 9.6.3) does
not reach it, and it draws no seed from the process that reached the statement, which would move
that process's own random stream (LRM 18.14.1). It draws none because it evaluates nothing of the
design: every expression it needed was read before it existed.

**It reaches the region itself rather than submitting a closure into it.** Once the event has named
the slot, the carrier suspends into that slot's NBA region and writes when it resumes. This is the
only construct that reaches a region that way; everything whose slot is knowable hands the region a
closure and suspends nothing.

## Consequences

- The repeat form (`a <= repeat (n) @(ev) b`) is the same carrier with the wait in a loop, so the
  count is read once where the statement stands and nothing about the count reaches the runtime.
- Every event control the statement form accepts serves here unchanged -- an event list, an edge, a
  named event, an `iff` qualifier -- because what the carrier contains is the same wait a statement
  would have contained.
- Two updates to one variable from one procedure land in the order the statements ran: their
  carriers register on the event in that order, wake in it, and place themselves in it (LRM 10.4.2).
- A disable that reaches the process does not cancel a pending update. LRM 9.6.2 leaves the outcome
  of a scheduled-but-unexecuted nonblocking assignment unspecified, so this is a choice; it is the
  one an update carrying a delay already makes, whose closure the engine holds and no disable
  touches.

## Rejected

- **A membership that names a closure instead of an activation.** The observable cell's and the
  named event's membership lists would each gain an alternative, every consumer of a fired
  membership would branch on which it got, and the repeat count would become runtime state. It also
  spreads: each new kind of target that can hold a wait would have to carry both. One shape covers
  both -- a membership names an activation, and an update that must wait is one.
- **A `fork` branch.** The spawning path exists and would need no new verb, but it adopts the branch
  into the spawner's lineage, which is exactly what LRM 9.6.1 and 9.6.3 must not see. Reusing it and
  then hiding the branch from `wait fork` states one relation in two places and keeps neither true.
- **Submitting a closure once the event has happened.** The carrier could build the update as an
  ordinary nonblocking closure and submit it from the Active region, which reuses the submit path
  exactly and needs no way to reach a region. It was written that way first. Everything the closure
  needs is already frozen in the carrier, so it gets frozen a second time one boundary further in,
  and the second freeze takes a reference over a reference the carrier already captured -- which the
  execution backend refuses to lower at all today. That refusal is what made the cost visible rather
  than what decides it: two freezes for one update is a boundary the shape does not have, and the
  carrier can reach the region itself.
- **Writing in the region the event fired in.** Simplest of all, and wrong: LRM 4.4.2.4 puts the
  update in the NBA region, so a procedure the same event wakes would read a value the standard says
  it cannot see yet.

## Cross-references

- `architecture/scheduling.md` -- deferred work is a closure submit; this is where a placement that
  cannot yet be stated makes the exception, and what that exception may not do.
- [value-change-wait-as-runtime-call](value-change-wait-as-runtime-call.md) -- the wait the carrier
  contains, unchanged.
- [event-control-unification](event-control-unification.md) -- one shape for every value-change
  wait, which is why the carrier accepts every form the statement form accepts.
