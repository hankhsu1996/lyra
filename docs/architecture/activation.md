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
- The **registration set**: every external reference to a parked activation (a region queue slot, a
  delay slot, an event waiter entry, a value-change subscription, a join aggregator entry) as a
  revocable registration owned by the activation.
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
   task enable makes the enabler all three; a fork branch separates them -- owned and cancelled by
   the fork domain, its outcome aggregated by a join state, with no single resuming continuation.
   _Consequence: these relations are not one pointer; a model that collapses them cannot express
   fork or selective cancellation._

4. **Every external reference to a parked activation is a revocable registration, and a frame is
   destroyed only after no scheduler structure can name its token.** When an activation suspends,
   each place that can later name it -- a region queue, a delay slot, an event waiter list, a
   value-change subscription, a join aggregator -- holds a registration the activation can revoke.
   Destruction is gated on the registration set being empty. _Consequence: an activation can be
   cancelled and torn down with no dangling token left in any queue, waiter, or subscription._

5. **A typed await consumes the typed terminal outcome.** Awaiting an activation yields its outcome:
   `Succeeded(T)` produces `T`, `Faulted` re-raises the exception into the awaiter, `Cancelled`
   propagates cancellation. A pure suspension (a delay, an event control, an output-less task) is
   the `T = Void` case of the same await. _Consequence: nested enable is one uniform value flow -- a
   task awaiting a task gets a typed result with no per-call special handling._

6. **The activity taxonomy is a fixed set of activations with different consumers; a deferred effect
   is not an activation.** A process is owned by its scope, has process identity, completes `Void`,
   and is observed by the engine (no continuation). A task activation inherits the enabler's process
   identity, completes with a typed payload, and resumes the enabler. A fork branch has its own
   process identity, is owned by the fork domain, completes `Void`, and reports to a join state. A
   deferred effect is a closure submitted to a region, not an activation. _Consequence: each kind
   reuses the activation core but binds its own ownership / continuation / completion; deferred work
   never enters the activation/completion model._

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

- **A deferred effect modeled as an activation with a completion** -- a non-blocking assignment, a
  postponed `$strobe`, or a deferred assertion action given an activation token, a completion slot,
  or an await edge. These are closures submitted to a region; they neither suspend their submitter
  nor settle an outcome a consumer awaits (invariant 6).

- **A fork branch modeled as a task the parent awaits.** A branch has its own process identity, is
  owned and cancelled by the fork domain, and reports to a join state under a join-mode condition;
  `join_none` has no parent wait at all. Reusing task-enable ownership for a branch conflates the
  two (invariant 3, invariant 6).

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
  ownership            = the fork domain
  continuation         = none (the branch resumes no one)
  cancellation domain  = the fork domain
join state  <-  each branch reports terminated / faulted / cancelled
  join / join_any / join_none decide when the forking process resumes
```

The C++ backend realizes an activation as a coroutine frame. Its activation token is the coroutine
promise's non-templated base (the scheduler holds a pointer to it); its completion slot is the typed
result the promise carries; a task enable is `co_await` of the activation, with symmetric transfer
realizing the continuation and `await_resume` consuming the terminal outcome. The promise base, the
`coroutine_handle`, and symmetric transfer are this realization's mechanics; the activation,
completion slot, registration set, and join state are the model they realize. A future LIR / LLVM
backend realizes the same model with coroutine intrinsics and explicit control edges instead.
