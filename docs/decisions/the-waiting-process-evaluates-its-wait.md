# The waiting process evaluates its wait

Date: 2026-10-04 Status: accepted. Revises the collection consequence of
[object-is-an-event-source](object-is-an-event-source.md) and the decision of
[a-function-reports-what-a-call-of-it-reads](a-function-reports-what-a-call-of-it-reads.md), each in
place.

## Context

LRM 4.5's reference algorithm separates the two events a wait involves: an update event "schedule[s]
evaluation event for any process sensitive to the object", and the evaluation event then evaluates
that process. LRM 9.4.2 has a change to a member a method reads "cause the event expression to be
reevaluated", and LRM 9.4.2.3 has an `iff` qualifier "evaluated when `a` changes". So the evaluation
of an event expression belongs to the waiting process, once per change that reaches it, and happens
after the change rather than inside it.

When the evaluation runs is not fixed: LRM 4.7 lets a simulator suspend a process in the middle of a
statement and run another's events, "and the order of interleaved execution is nondeterministic". So
evaluating the waiting process's expression the moment a write lands is a schedule the standard
allows. What it does not allow is the evaluation running as some other process, or running inside an
evaluation of the same process, which is not waiting while it evaluates.

The runtime had the change decide: the write's publish asked each registration whether it was an
event, which ran the waiting process's expression and qualifier on the writer's stack and as the
writer. Where the expression only reads storage, nothing can tell that from the waiting process
evaluating it. Where it calls, writes or can fail, these were wrong on both backends:

- a class method waiting on `@(posedge buses[counted(0)].grant)`, `counted` writing a property of
  the same object, overflowed the stack: the write's publish asked the same registration again,
  inside its own evaluation;
- `@(a iff f())` with `f` toggling `a` overflowed the same way;
- `@(e iff f())` with `f` triggering `e` ran `f` once per nested trigger;
- `@(h.v)` then `h = null` raised the null access as the writer, inside its `h = null`, and the
  writer never reached its next statement.

That the writer's next statement already saw a write the waiter's call made is not among them: it is
one of the interleavings LRM 4.7 permits.

A wait reaching through a handle or a call also evaluated its operands once per consumer: once for
the value, once more to learn the objects it reached, and once again before waiting.
`@(held[f(0)] .data)` ran `f` five times where the wait began.

## Decision

**A wait is decided by its process: a change to anything its last evaluation reached resumes it, and
it evaluates the expression and the qualifier once, compares, and waits again on what that
evaluation reached. Where the expression and its qualifier read nothing but storage elaboration
sealed, the change decides instead**, evaluating them where it lands. That is one of the schedules
LRM 4.7 permits, and such an evaluation does nothing, fails in no way and depends on no process, so
running it there cannot be told from the process running it; what it saves is resuming a process to
learn that a change was no event -- for `@(posedge clk)`, every falling edge.

- **The one evaluation states what it reaches.** Each object a property is read on and each
  interface variable read through a handle is stated where the evaluation reads it, from the value
  it read it through, and each function it calls is handed the report and states what it reads
  before it runs. Nothing evaluates an operand a second time to learn what it reaches.
- **Building an observation evaluates nothing.** It is armed where the wait begins -- by the process
  for a wait it decides, whose first evaluation is no event -- and re-armed by that process after a
  restart (LRM 9.7), never by whoever asked for the restart.
- **Which waits a change decides is a question about the expression**, answered by walking it the
  way clang's `Expr::HasSideEffects` does: any call, write, read through a handle or a virtual
  interface, or property read makes the process decide. A system function counts, since what one
  does is not stated anywhere a lowering can read.

## Rejected

- **Guarding the watch against re-entry.** Ends the overflow and leaves every other position above
  wrong: the writer still runs the waiter's calls and its failures.
- **Having the process decide every wait.** LRM 4.5 literally, and Verilator's answer for a trigger
  that is impure or under a class (`V3Timing.cpp`, `needDynamicTrigger`;
  `VlDynamicTriggerScheduler`). It is as correct as the decision taken, and it resumes every
  `@(posedge clk)` waiter on both edges to find half of them were nothing. Which of the two a
  transition undone within one process run is -- an edge or not -- does not choose between them: LRM
  4.7 leaves it to the interleaving.
- **Evaluating at the change, as the waiting process.** Also a schedule LRM 4.7 permits, for any
  expression: the change would take the waiter off what it watches, make it the running process
  while it evaluates, and keep what the evaluation raises as the waiter's. It saves a resumption on
  the waits that call or reach through a handle, which are testbench waits, at the price of the
  engine running one process's code on another's stack.

## Consequences

- An event control over storage alone -- the RTL case -- is decided at the write as before; that is
  an optimization the expression permits, not a meaning, and a change to what a pure expression may
  contain moves a wait from one side to the other without changing what it means.
- A wait whose expression calls something, reads through a handle, or carries a qualifier that does
  is woken on every candidacy; it was before as well, after an evaluation on the writer's stack that
  no longer happens.
