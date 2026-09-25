# Waiting is an operation both backends call, and what is waited for belongs to the execution

Date: 2026-09-17. Status: accepted.

## Context

A process waits: `#5`, `@(posedge clk)`, `wait (cond)`, `@e`, a join, `wait fork`, awaiting another
process. Something outside it may stop it while it waits and start it again, and LRM 9.7 is precise
about what that owes:

> Suspending a process in the `WAITING` state shall cause the process to be **desensitized to the
> event expression, wait condition, or delay expiration** on which it is blocked.

> Calling `resume()` on a process that was suspended while in the `WAITING` state shall
> **resensitize** the process to the event expression or to wait for the wait condition to become
> true or for the delay to expire. If the wait condition is now true or the original delay has
> transpired, the process is scheduled into the Active or Reactive region to continue its execution
> in the current time step.

Three things to wait for, one rule: withdraw the enrolment when the process is stopped, make the
wait again when it is started, and continue at once where what it waits for has already happened.
None of it may run a statement of the body, because the body is parked.

Measured against that clause on 2026-09-16, three of four positions answered wrongly, split across
the two backends: a delay and an event control continued at the moment the process was started
rather than at the moment they were waiting for, and a `wait` whose condition became true while the
process was stopped never continued at all.

## The cause, which is one layer above any of them

Both backends consume the same MIR and reach the same runtime. What they do not share is the
function behind the name the shared declaration gives an operation. For the eleven operations that
park their caller, that declaration named a factory returning a **C++ awaitable** -- an object whose
work happens in `await_ready` / `await_suspend` / `await_resume`, which the host C++ compiler
generates around it.

Generated LLVM IR cannot use one: `await_suspend` is a template over a promise type and takes a
`std::coroutine_handle<P>`, which generated code has no way to form. So the execution backend wrote
a second body for each of the eleven. Audited across the whole table: of the entries that park, ten
of eleven called a different function than the one declared, while **118 of the entries that do not
park called exactly the declared function**. The line the architecture holds along everywhere else
is the line an awaitable crosses.

Two bodies for one operation is two answers to one question, and `backend_contract.md` invariant 6
says what that produces: "each consumer works it out alone, from whatever is nearest to hand, and
the answers agree until the day one of them does not." Here the day arrived at LRM 9.7, which only
one of the two bodies had ever been written against.

The field puts a protocol object strictly above the machine-model IR its backends read. LLVM IR has
no notion of an awaitable at all -- `llvm.coro.suspend` returns an `i8` and the frontend branches on
it, and the `await_*` protocol "belongs entirely to the language frontend". rustc expands `await`
into a call to `poll`, a branch on `Poll::Ready` / `Poll::Pending`, and a `TerminatorKind::Yield`;
the `Future` trait is resolved before that IR exists.

**Which layer that is, here, is worth being exact about, because getting it wrong is how this was
first designed.** LLVM IR and rustc's MIR are LIR's peers, not MIR's -- `lir.md` says so, and MIR's
own peers are C++, Rust and Python, where awaiting is one construct a program writes. So the
decomposition those two show belongs at MIR-to-LIR, which is where `lir.md` already puts it: "an
await, a sensitivity wait, becomes suspend/resume edges with scheduler calls". What does not belong
anywhere is the protocol _object_, and the shared runtime declaration -- which both backends read,
below MIR -- is where it was.

## Decision

**D1. An operation the shared declaration names is a function, never a protocol object of one
target.** A call that may park its caller does the whole operation and answers whether the caller
must give up control. Both backends call that one function and differ only in how they spell giving
up control.

**D2. MIR states a suspending construct the way a source language writes it: waiting on that call.**
At the layer whose peers are languages, with no decomposition. The language writes two different
constructs here, and MIR states two: awaiting an execution, which ends when another body completes
and hands back what it completed with (LRM 13.3), and waiting on a call that has already arranged
this execution's resumption and answers whether control must be given up (LRM 9.4). Each is a node
of its own.

_Revised 2026-09-24._ This first read "one node, whose operand's type says which of the two it is".
That held only for an execution, whose type is its own; a registration's type is a machine boolean,
which says nothing, so each consumer decided the operation by testing for the other type, and the
execution lowering also tested whether the operand was a call -- a second input to the same
question. Two constructs the source writes differently are two nodes at the layer whose peers are
languages, which is this record's own requirement; a single node was taken from C++, where both
reach one protocol object this record removed. What two nodes cost is that each can now be written
over the other's operand, so verifying a unit refuses an await on anything but an execution and a
wait on anything but a registration's answer.

**D3. Each backend realizes the await in its own terms, and neither invents a name to do it.** The
machine-model path decomposes it into the call, a branch on its answer, a suspend edge and the
question a resumption asks, which is what that layer is for. The target that renders MIR directly
awaits a library type whose whole content is "give up control where the call says it parked, and ask
that question on the way back" -- one type for every construct, named where a library type's
spelling is named.

**D4. An execution holds what it is waiting for, for as long as it waits.** It is built where the
body parks, because that is the last moment anything knows what the body asked for, and it answers
two things: make this wait, and make it again after the process was stopped. Stopping the process
revokes the enrolment and keeps it; starting the process asks it again.

**D5. Making a wait and making it again are separate questions, because the standard answers them
separately.** For most constructs the second is the first asked afresh. For a `wait (cond)` it is
not: the condition is read by the body, so starting the process again answers "already satisfied"
and lets the body's own loop read it -- which is the evaluation the clause asks for. That is why a
level wait registers through a call of its own rather than through the event control's.

**D6. Which frame is waiting is the runtime's to know.** Nothing that registers a wait is handed
one. A called task takes over its process's thread and gives it back, on both backends, so the
runtime can answer at any point which frame would park.

## Invariants

1. The declaration both backends read names an operation. A target language's own protocol object --
   an awaitable, a future, a promise -- never appears in it, because only the target that generated
   it can consume one and the other consumer must then write a second body.

2. A body stops to wait in exactly one way, and that way takes what is being waited for. There is no
   way to reach the scheduler without it, so no wait exists that process control cannot restart.

3. An execution holds what it waits for exactly while it is blocked. Holding nothing is how a
   runnable execution says it has nothing left to wait for.

4. No scheduler or activation path asks which construct a wait came from.

## Rejected

- **Eleven thin shells, one per construct, over eleven shared bodies.** That removes today's
  divergence and leaves a target's protocol object in the layer both backends read, so the next
  suspending construct added has somewhere to diverge again. The one library type D3 names is not
  that: it holds no construct and no wait, it is reached where a library type's spelling is reached
  rather than through the shared declaration, and the other target needs no counterpart for it.

- **Decomposing the await in MIR** -- the call, a test of its answer, a suspension statement, and
  the resumption's question, as four statements HIR-to-MIR emits. It reads as the machine-model IRs
  the survey cites, and that is the mistake: those are the next layer's peers, not this one's. What
  it produced was MIR in which `$finish(0)` stands as the condition of an `if`, which is not
  software; and it moved into MIR a decomposition MIR-to-LIR already did correctly.

- **A suspension that carries its own wakeup.** The wakeup is registered by the call before it, so a
  suspension names nothing; `lir.md` forbids a scheduling concept as a node for the same reason.

- **Not desensitizing at all, and dropping what arrives while stopped.** This is SystemC's
  `suspend()`, whose kernel leaves the sensitivity intact and marks the process instead. It answers
  the event-control position and no other: a delay whose deadline passes while stopped never
  continues, and a `wait` whose condition became true hangs, because nothing further is coming.
  SystemC needs no re-establishment precisely because it never withdraws anything -- and its
  `disable()`, which does withdraw, declines to re-establish on `enable()`.

- **Re-entering the body to make the wait again.** This is what an operating system does with a
  stopped thread: the wait stays inside the system call and the kernel re-enters it, or returns
  `EINTR` so the caller re-issues. The clause forecloses it -- a process resumed into a delay it is
  still waiting for must not run a statement -- so the ability to re-establish has to be handed over
  rather than re-derived by running the body.

- **A typed taxonomy the scheduler branches on.** Which construct a wait came from is the
  construct's own business; the scheduler asks one question and gets one answer.

## Consequences

- Every position of LRM 9.7 answers the same on both backends, and the conformance case covers four:
  a delay not yet transpired, an event occurrence during the stop, a condition that became true
  during the stop, and a process stopped after it was woken but before it ran.

- `wait (cond)` is its own runtime operation. It watches the same leaves as an event control over
  the same reads, and the two part company only where a stopped process is started again.

- The whole of the fix is below MIR -- what the shared declaration names, and what the runtime keeps
  -- plus one library type the direct-rendering target awaits. MIR's one change came later, with the
  revision of D2.

- A suspension costs one allocation for what is being waited for. Whether that is visible in a
  simulation's throughput has not been measured; the shape to reach for if it is, is storage on the
  activation rather than a taxonomy.

## Cross-references

- [activation-disposition](activation-disposition.md) -- the model this completes. Its consequence
  list gives the per-construct restart rules as "an edge re-subscribes, a delay compares its
  absolute deadline, a monotonic condition re-checks", which is the clause's three minus the wait
  condition; D5 above is that missing one, and it is where one of the three wrong answers lived.
- [jit-process-suspension](jit-process-suspension.md) -- D3 there already required one registration
  call per construct, naming a level wait among them; this is that requirement met. Its D5, that the
  scheduler's token is runtime-owned on the execution backend, is unchanged: who owns the token is a
  realization difference, and it was never a licence for a second wait protocol.
- `../architecture/mir.md` -- invariant 10, which asks HIR-to-MIR to emit a combination of existing
  primitives rather than a node a consumer must interpret.
- `../architecture/backend_contract.md` -- invariant 6 and invariant 8, the two this is measured
  against.
