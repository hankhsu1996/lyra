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

Only one thing in the runtime watches for an event, and it names an activation: an observable's
membership list, whether the observable is a cell or the event source of the object an event names.
So either a membership learns to name something other than an activation, or the thing that waits is
an activation.

## Decision

The update is carried by an **execution of no lineage**: a coroutine that holds what the update
needs, waits for the event, places itself in the NBA region of the slot it lands in, and makes the
write there.

Four things follow, and each is the point.

**It starts waiting where the statement stands.** The control is executed by the statement (LRM
9.4.5), so the execution is run at once, as the statement that reached it, up to its wait, and only
then does that process carry on: a change the process makes straight after the statement is an event
for the wait, and an event the control names is the object it names there. An execution scheduled to
start later would begin watching after the process had moved on and miss both. For the same reason a
run-time error evaluating the control before the wait begins is the statement's, raised in the
process that reached it, which stops there as it does for any error it raises; the execution is no
process of the design, so it has nothing else to answer for that error. Once it waits, an error it
meets is its own.

**What the update writes and where it writes it are frozen where the statement stands**, into that
execution, by the same freeze an ordinary nonblocking closure uses -- the target's root captured as
a reference, the selector coordinates and the operands snapshotted. LRM 10.4.2 settles both there,
so a later write to the index or the operand does not reach an update still to come.

**It belongs to no lineage.** LRM 9.4.5 makes no process of the update, so nothing that names
processes may find it: `wait fork` (LRM 9.6.1) does not wait for it, `disable fork` (LRM 9.6.3) does
not reach it. It does evaluate the control it carries, where the statement stands and again at each
candidacy, so it draws from a generator of its own. Being no thread, it takes nothing from the
process that reached the statement, whose stream reaching it leaves where it was: its generator is
the next one of the initialization RNG of that process's instance or package, the one source LRM
18.14.1 names for what is made with no active thread. Taken from the reaching process, as a thread's
is, it would move that process's later draws at every such statement, a control that draws nothing
included.

**It reaches the region itself rather than submitting a closure into it.** Once the event has named
the slot, the carrier suspends into that slot's NBA region and writes when it resumes. This is the
only construct that reaches a region that way; everything whose slot is knowable hands the region a
closure and suspends nothing.

## Consequences

- **The carrier is not about assignment, and the standard says so.** LRM 15.5.1 defines the
  nonblocking event trigger in the same words -- it "creates a nonblocking assign update event in
  the time in which the delay control expires or the event control occurs", triggering the event in
  the NBA region -- and the grammar gives both statements the same `delay_or_event_control`
  production. So the three placements and this carrier take whatever the effect is: `->> e` submits
  a closure to this slot's region, `->> #d e` to the region of the slot the delay names, and
  `->> @(ev) e` is carried the same way an update is. A trigger freezes the event it names, which is
  a reference to a synchronization object (LRM 6.17): the object triggered where it lands is the one
  the event named where the statement stood.
- The repeat form (`a <= repeat (n) @(ev) b`) is the same carrier with the wait in a loop, so the
  count is read once where the statement stands and nothing about the count reaches the runtime.
- **One statement's whole effect is one deferred effect**, however many places it writes. A
  destructuring left-hand side (LRM 11.4.12) is the case that shows it: every part is frozen into
  one closure or one carrier, so a control on such an assignment is read once and every part's share
  lands in the same slot.
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

- **A membership that names a closure instead of an activation.** An observable's membership list
  would gain an alternative, every consumer of a fired membership would branch on which it got, and
  the repeat count would become runtime state. It also spreads: each new kind of target that can
  hold a wait would have to carry both. One shape covers both -- a membership names an activation,
  and an update that must wait is one.
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
