# Activation

## Purpose

An activation is the runtime's one shape for "a concrete, in-flight execution instance." A
SystemVerilog process (`initial`, `always*`, `final`), a task enable, and a fork-join branch are
each an activation: a specific execution that has begun, can suspend and resume, and settles exactly
one terminal outcome. An activation is not the callable declaration it runs (that is `callable.md`'s
subject) -- it is what exists after a callable is invoked.

The activation concept draws three lines that the rest of the runtime depends on:

- **Execution control versus completion.** An activation has a scheduler-visible execution core
  (suspend / resume / wait state / process identity) that is _payload-neutral_, and a typed
  completion slot (the value, the exception, or a cancellation) that the scheduler never sees.
- **The completion is a terminal outcome, not a return register.** Every activation settles one of
  `Succeeded(T)`, `Faulted(exception)`, or `Cancelled`. A consumer reads that typed outcome; the
  scheduler reads only "parked / runnable / terminated."
- **Three relations are distinct.** Who _owns_ an activation, who _consumes_ its outcome, and who
  _cancels_ it are separate relations that happen to coincide for a direct task enable and diverge
  for a fork.

This is the runtime peer of how an async runtime separates a task from its scheduler and its result:
the scheduler parks and resumes opaque execution handles; the typed result is a property of the task
the awaiter consumes, not of the scheduler.

## Owns

- The **activation**: a concrete suspendable execution instance, distinct from the callable it runs.
- The **activation token**: the payload-neutral, scheduler-visible handle to an activation's
  execution core (process identity, wait state, the resume entry). The scheduler holds this and
  nothing else.
- The **completion slot**: the single typed terminal outcome an activation settles --
  `Succeeded(T)`, `Faulted(exception)`, or `Cancelled` -- and the rule that a consumer reads it
  through the activation, never through the scheduler.
- The three activation relations: **ownership** (who releases it), **continuation** (who consumes
  the outcome and resumes on completion), and **cancellation domain** (who is cancelled together).
- The **process lineage**: the parent-child tree of processes (LRM 9.5), which realizes ownership
  and cancellation membership for a spawned process. A lineage node outlives the execution it named,
  for as long as any descendant is live.
- The **registration set**: every external reference to a parked activation (a region queue slot, a
  delay slot, an event waiter entry, a value-change subscription, a join aggregator entry) as a
  revocable registration owned by the activation. A registration is the activation's _current
  enrollment_ in a target, not the wait itself.
- The **activation disposition**: how an activation currently participates in execution --
  `Executing`, `Runnable`, `Blocked`, `Suspended`, or `Terminal`. This is the activation's
  authoritative state; the registration set and completion slot are resources constrained by it, not
  independent facts. `Suspended` carries the disposition the activation held before it was
  suspended.
- The **pending wait**: for a `Blocked` (or `Suspended`-from-blocked) activation, the retainable
  ability to re-establish its wait and to report whether the wait is already satisfied -- distinct
  from the registration, which only records the current enrollment. It is a uniform capability every
  suspending construct supplies, never a taxonomy the scheduler branches on.
- The **active leaf**: the relation from a process to the one activation currently carrying its
  thread. A process is a single thread (a task or function call runs in the caller's thread, LRM
  9.5), so when it is not executing exactly one leaf activation -- the innermost frame -- is
  enrolled or runnable. Process control names the process and acts on its active leaf.
- The **join state**: the completion aggregator for a fork's branches and its join-mode condition.

## Does Not Own

- The stratified region scheduler -- the queues, the region order, the suspension verbs. That is
  `scheduling.md`. The activation only exposes the token the scheduler parks and resumes.
- The callable declaration and its signature, environment, and result type (`callable.md`,
  `mir.md`). An activation runs a callable; it is not one.
- Deferred effects -- a non-blocking assignment's commit, a postponed `$strobe`, a deferred
  assertion action. Those are closures submitted to a region (`scheduling.md`), not activations:
  they capture an environment, run later, and never suspend their submitter or settle a completion
  an awaiter consumes.
- Storage layout, frame allocation, and the machine-level suspend/resume protocol (LIR and below).

## Core Invariants

1. **An activation is the minimal suspendable execution entity, and the scheduler sees it through a
   payload-neutral token.** Process, task enable, and fork branch are all activations. The scheduler
   parks and resumes an activation token that carries process identity and wait state but never the
   completion type. _Consequence: one scheduler serves every activation kind; adding a completion
   shape never touches the scheduler._

2. **Completion is a typed terminal outcome held in the activation's completion slot, invisible to
   the scheduler.** An activation settles exactly one of `Succeeded(T)`, `Faulted(exception)`, or
   `Cancelled`. The typed value and the exception live in the completion slot; the scheduler
   observes only parked / runnable / terminated. _Consequence: the value and the exception travel
   together as one outcome, read by the activation's consumer, never transported through a side
   channel the scheduler or a backend invents._

3. **Ownership, continuation, and cancellation membership are three distinct relations.** Ownership
   decides who releases the activation; continuation decides who consumes its outcome and resumes on
   completion; cancellation membership decides who is cancelled when a domain is disabled. A direct
   task enable makes the enabler all three; a fork branch separates them -- attached to the spawning
   process's lineage, which determines storage ownership and cancellation membership, while its
   outcome is aggregated by the join state of the fork statement that spawned it, with no single
   resuming continuation. _Consequence: these relations are not one pointer; a model that collapses
   them cannot express fork or selective cancellation. The lineage edge realizes two of them; the
   join state realizes the third._

4. **Every external reference to a parked activation is a revocable registration, and a frame is
   destroyed only after no scheduler structure can name its token.** When an activation suspends,
   each place that can later name it -- a region queue, a delay slot, an event waiter list, a
   value-change subscription, a join aggregator -- holds a registration the activation can revoke.
   Destruction is gated on the registration set being empty. A registration is **one record**: the
   activation owns it and the target links it, so the relation is stored once and reached from both
   ends. The activation's set and the target's list are two indexes over that record, never two
   descriptions of it. A registration records the activation's _current enrollment_ in a target, not
   the wait it is serving: revoking a registration detaches the enrollment and forgets nothing the
   activation still needs, because the ability to re-establish the wait lives in the pending wait
   (invariant 7), not the registration. _Consequence: an activation can be cancelled and torn down
   with no dangling token left in any queue, waiter, or subscription -- and revoking is a detach, so
   neither end ever searches the other, and neither can hold a belief the other has abandoned._

5. **A typed await consumes the typed terminal outcome.** Awaiting an activation yields its outcome:
   `Succeeded(T)` produces `T`, `Faulted` re-raises the exception into the awaiter, `Cancelled`
   propagates cancellation. A pure suspension (a delay, an event control, an output-less task) is
   the `T = Void` case of the same await. _Consequence: nested enable is one uniform value flow -- a
   task awaiting a task gets a typed result with no per-call special handling._

6. **The activity taxonomy is a fixed set of activations with different consumers; a deferred effect
   is not an activation.** A process is owned by its scope, has process identity, completes `Void`,
   and is observed by the engine (no continuation). A task activation inherits the enabler's process
   identity, completes with a typed payload, and resumes the enabler. A fork branch has its own
   process identity, is owned by the spawning process's lineage, completes `Void`, and reports to a
   join state. A deferred effect is a closure submitted to a region, not an activation.
   _Consequence: each kind reuses the activation core but binds its own ownership / continuation /
   completion; deferred work never enters the activation/completion model._

7. **An activation's disposition is authoritative, and suspension saves the disposition it
   replaces.** A non-terminal activation is `Executing`, `Runnable` (entitled to run; the region it
   sits in is the engine's placement, not part of the disposition), or `Blocked` (waiting on a
   condition, enrolled by a registration and holding a pending wait). `Suspended` is not another
   kind of wait: it is process control (LRM 9.7) revoking an activation's scheduler participation
   while saving the disposition it held -- `Suspended(Runnable)` or `Suspended(Blocked)` -- so
   resume restores exactly that. A saved `Runnable` resume re-takes an execution entitlement; a
   saved `Blocked` resume asks its pending wait to re-establish, which either re-enrolls or reports
   the wait already satisfied. The pending wait is one uniform capability every suspending construct
   supplies -- re-establish, report-satisfied, discard -- so no scheduler or activation path
   branches on which construct the wait came from; the construct-specific knowledge stays in the
   construct's own registration, exactly as a wakeup registration is one per-construct runtime call
   and the suspend itself is construct-neutral. _Consequence: the disposition is one state machine
   with one authoritative owner; the registration set, the pending wait, and run-queue membership
   are resources that must agree with it, never independent truths that drift._

8. **Publishing an activation's terminal outcome commits that it runs no more user code.** A
   consumer that reads the outcome, or a waiter woken by the activation's completion, may reclaim
   shared state the activation touched, so the terminal state and the waiter wakeups are published
   only after the body has run its last statement. When an activation cannot be torn down
   synchronously -- its frame is executing (a process disabling itself or an ancestor), or a foreign
   call must unwind cooperatively across a boundary the runtime does not own -- termination is a
   two-step transition: a request first revokes the activation's scheduler participation and records
   the cause, and the outcome is published only once the body reaches a safe boundary. The request
   is not the outcome; between them the activation is un-nameable by any scheduler structure yet
   still live. _Consequence: `Cancelled` may be requested while a frame is still running, but is
   settled -- frame released, waiters drained -- only at a safe boundary. Settling on the request,
   publishing the outcome before the body stops, lets a waiter observe a terminated activation that
   is still executing._

## Boundary to Adjacent Layers

- **To the scheduler (`scheduling.md`).** The activation exposes a token the engine parks on a
  region queue and resumes; the engine reaches process identity and wait state through the token and
  nothing more. The engine is construct-neutral -- it branches on queue and region, never on the
  activation's kind or completion type.
- **To the callable (`callable.md`).** A callable value is invoked to produce an activation. The
  callable owns the signature and result type (`Coroutine<T>` for a suspending callable); the
  activation owns the running instance and its terminal outcome.
- **To MIR (`mir.md`).** The typed completion is MIR semantics: a suspending callable's result type
  is `Coroutine<T>`, and `await : Coroutine<T> -> T` consumes the terminal outcome. The activation
  model is how a backend realizes that semantics at runtime; the MIR shape is backend-neutral.
- **To LIR.** A backend lowers an activation's suspend, resume, success, exception, and cancel
  points to explicit control edges; the completion slot's outcome is the value an await's resume
  edge carries.

## Forbidden Shapes

- **A scheduler that branches on completion type or callable kind.** The scheduler parks and resumes
  a payload-neutral token; an engine that asks "is this a task / does this return a tuple" has let
  completion shape leak into execution control (invariant 1).

- **A completion value transported outside the completion slot** -- a hidden out-parameter, a
  backend-invented result sink, a side-channel reference writeback. The terminal outcome is the
  activation's completion slot; a value reaching the consumer any other way is a backend inventing
  transport MIR did not state (invariant 2).

- **`output` / `inout` as a live alias of the caller's storage.** They are completion-payload
  components settled once, not storage the activation writes during its run; only a `ref` is a live
  alias. A live-alias `output` lets a caller observe an intermediate write while the activation is
  suspended (invariant 2, `callable.md`).

- **Collapsing ownership, continuation, and cancellation into one relation.** A single "parent
  pointer" that means owner and consumer and cancellation scope cannot express a fork (owned by the
  domain, consumed by a join, not resumed as a continuation) or selective cancellation (invariant
  3).

- **An un-revocable reference to a parked activation** -- a raw token pushed into a queue, waiter
  list, or subscription set with no registration the activation can later revoke. It dangles the
  moment the activation is cancelled or destroyed (invariant 4).

- **The same membership recorded on both sides** -- a target that stores its own record of which
  activations it holds while each activation separately stores which targets hold it. Two
  authoritative copies of one relation must be reconciled, so revoking degrades into searching the
  other side, enrolling has to defend against drift, and each end needs teardown logic whose only
  job is to repair the other. The relation is one record with an index at each end (invariant 4).

- **A deferred effect modeled as an activation with a completion** -- a non-blocking assignment, a
  postponed `$strobe`, or a deferred assertion action given an activation token, a completion slot,
  or an await edge. These are closures submitted to a region; they neither suspend their submitter
  nor settle an outcome a consumer awaits (invariant 6).

- **A central taxonomy of wait kinds the scheduler or activation core branches on** -- a
  `variant`/enum of delay / event / join / await blocks switched over on suspend, resume, or wake.
  The pending wait is a uniform capability; a suspending construct already registers its own wakeup
  through its own call, so re-establishing it is that same construct's business, dispatched
  uniformly. A kind switch on the execution path reintroduces the source-language timing concept the
  engine is forbidden to know (invariant 7, `scheduling.md`).

- **A second authoritative copy of the wait's state** -- a blocked-operation object that duplicates
  the deadline / observable / target the suspending construct already holds, kept in sync with it.
  The wait's state has one home (the construct's own retained state); the pending wait is the
  capability to re-establish from that home, not a mirror of it. Two copies is the registration
  double-encoding forbidden by invariant 4, one level up.

- **Scheduling placement folded into the disposition** -- a `Runnable(region)` that pins which
  region or queue a runnable activation must be restored to. `Runnable` is the semantic entitlement
  to run; the region is the engine's placement, chosen when the activation is scheduled (resume
  places a runnable-suspended activation into the active region, LRM 9.7). A disposition that
  carries region lets scheduler placement leak into activation semantics (invariant 7).

- **`Suspended` modeled as a new kind of wait, or the disposition split across independent fields**
  -- a suspended activation given its own wait target to re-fire, or its state inferred from
  execution flags plus registration-emptiness plus queue membership plus pending-wait presence, each
  separately authoritative. Suspension saves the prior disposition; the disposition is one
  authoritative state its resources must agree with (invariant 7).

- **A fork branch modeled as a task the parent awaits.** A branch has its own process identity, is
  owned by the spawning process's lineage, and reports to a join state under a join-mode condition;
  `join_none` has no parent wait at all. Reusing task-enable ownership for a branch conflates the
  two (invariant 3, invariant 6).

- **A cancellation domain scoped to the fork statement rather than to the process.** `disable fork`
  terminates every descendant of the calling process, spawned by any fork that process ever
  executed, and `wait fork` observes the immediate children accumulated across all of them (LRM
  9.6.1, 9.6.3). A per-fork-statement domain cannot answer either question. The join state is
  per-fork-statement because the join condition is; the lineage is per-process because cancellation
  and child observation are.

- **A lineage node whose lifetime is its activation's execution.** `disable fork` reaches "the
  descendants of subprocesses that have already terminated" (LRM 9.6.3), so a terminated process
  stays reachable from its parent while any descendant is still live. Releasing the node with the
  frame loses that reach; retaining the frame to keep the node pins the activation storage the frame
  solely owns. Node lifetime and execution lifetime are distinct.

## Notes / Examples

A direct task enable is the case where the three relations coincide. `int x = <enable t(a)>`:

```text
enable t(a)  ->  child task activation, completion slot typed `int`
  ownership            = the enabling frame
  continuation         = the enabling frame (resumes, consumes the outcome)
  cancellation domain  = the enabling frame's domain (disabling the enabler cancels the child)
await  ->  Succeeded(int) yields x ; Faulted re-raises ; Cancelled propagates
```

A fork splits them:

```text
fork branch  ->  branch activation, completion slot `Void`, own process identity
  ownership            = the spawning process's lineage (it releases the branch)
  continuation         = none (the branch resumes no one)
  cancellation domain  = the spawning process (an ancestor's `disable fork` reaches it)
join state  <-  each branch reports terminated / faulted / cancelled
  join / join_any / join_none decide when the forking process resumes
```

Ownership and cancellation membership ride the same lineage edge here; continuation rides the join
state. That the first two coincide for a branch is not a collapse of the relations -- what invariant
3 forbids is one edge that also carries the consumer.

The C++ backend realizes an activation as a coroutine frame. Its activation token is the coroutine
promise's non-templated base (the scheduler holds a pointer to it); its completion slot is the typed
result the promise carries; a task enable is `co_await` of the activation, with symmetric transfer
realizing the continuation and `await_resume` consuming the terminal outcome. A suspending construct
realizes a pending wait by the state it retains to re-establish itself -- a delay its absolute
deadline, an event control its observables and edges -- reached uniformly through the pending-wait
capability. The promise base, the `coroutine_handle`, and symmetric transfer are this realization's
mechanics; the activation, completion slot, registration set, disposition, and pending wait are the
model they realize. A future LIR / LLVM backend realizes the same model with coroutine intrinsics
and explicit control edges instead, the pending wait re-established by re-issuing the construct's
own wakeup-registration call rather than re-entering the suspended body.
