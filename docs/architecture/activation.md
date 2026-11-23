# Activation

The runtime does not yet hold the disposition of invariant 7 as one state. What it holds is read off
several fields: whether a process was suspended or terminated are flags of the process, whether its
leaf is blocked is whether the leaf names a wait, and the rest is what the execution running it
reports. Resume restores what suspension found, as the invariant requires, by reading those fields;
the single authoritative state the invariant asks for, which keeps them from drifting apart, is not
built. Everything else here binds every layer as written.

## Purpose

An activation is the runtime's one shape for "a concrete, in-flight execution instance." A
SystemVerilog process (`initial`, `always*`), a task enable, and a fork-join branch are each an
activation: a specific execution that has begun, can suspend and resume, and settles exactly one
terminal outcome. An activation is not the callable declaration it runs (that is
[`callable.md`](callable.md)'s subject) -- it is what exists after a callable is invoked.

The activation concept draws three lines that the rest of the runtime depends on:

- **Execution control versus completion.** An activation has a scheduler-visible execution core
  (suspend / resume / wait state / process identity) that is _payload-neutral_, and a typed
  completion slot (the value it produced, or the departure that left it) that the scheduler never
  sees.
- **The completion is a terminal outcome, not a return register.** A body is left in exactly two
  ways -- it returns, or it departs -- so every activation settles either `Succeeded(T)` or the
  departure that reached its landing. A consumer reads that typed outcome; the scheduler reads only
  "parked / runnable / terminated."
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
- The **completion slot**: the single typed terminal outcome an activation settles -- the value it
  produced, or the departure that reached its landing -- and the rule that a consumer reads it
  through the activation, never through the scheduler.
- The three activation relations: **ownership** (who releases it), **continuation** (who consumes
  the outcome and resumes on completion), and **cancellation domain** (who is cancelled together).
- The **process lineage**: the parent-child tree of processes (LRM 9.5), which realizes ownership
  and cancellation membership for a spawned process. A lineage node outlives the execution it named,
  for as long as any descendant is live.
- The **memberships** that can reach an activation: every place that can later resume or reach it (a
  region queue, a delay slot, an event, a value change, a process's termination, a disable target)
  holds a membership owned by what shares its life -- the activation for its queue place, the wait
  the body holds for what that wait watches, the process's record of a disable target for its being
  inside it.
- The **targets a process is inside**: each disable target its execution entered or was spawned
  inside, with the generation it captured there, held by the running process rather than read off
  where a body is written ([`nonlocal_control_flow.md`](nonlocal_control_flow.md)).
- The **activation disposition**: how an activation currently participates in execution --
  `Executing`, `Runnable`, `Blocked`, `Suspended`, or `Terminal`. This is the activation's
  authoritative state; its queue place, the wait it is parked on, and its completion slot are
  resources constrained by it, not independent facts. `Suspended` carries the disposition the
  activation held before it was suspended.
- **What the activation is waiting for**: for a `Blocked` (or `Suspended`-from-blocked) one, the
  wait it is parked on, which can arrange that wait and report whether what it waits for has already
  happened. What runs the body holds every wait -- its frame, or the reaction holding its one --
  built around its stop, or outside the loop that repeats it; the activation names the one it is
  parked on for as long as it waits, so it is never a taxonomy the scheduler branches on.
- The **active leaf**: the relation from a process to the one activation currently carrying its
  thread. A process is a single thread (a task or function call runs in the caller's thread, LRM
  9.5), so when it is not executing exactly one leaf activation -- the innermost frame -- is
  enrolled or runnable. Process control names the process and acts on its active leaf.
- The **join**: the wait on a fork's branches terminating, under its join-mode condition.

## Does Not Own

- The stratified region scheduler -- the queues, the region order, the suspension verbs. That is
  [`scheduling.md`](scheduling.md). The activation only exposes the token the scheduler parks and
  resumes.
- The callable declaration and its signature, environment, and result type
  ([`callable.md`](callable.md), [`mir.md`](mir.md)). An activation runs a callable; it is not one.
- Deferred effects -- a non-blocking assignment's commit, a postponed `$strobe`, a deferred
  assertion's report. Those are closures submitted to a region ([`scheduling.md`](scheduling.md)),
  not activations: they capture an environment, run later, and never suspend their submitter or
  settle a completion an awaiter consumes. Where what a deferred effect does may wait, the effect
  starts an execution of no lineage to do it (invariant 6).
- What a departure is, where it lands, and how a disable target is invalidated
  ([`nonlocal_control_flow.md`](nonlocal_control_flow.md)).
- Storage layout, frame allocation, and the machine-level suspend/resume protocol (LIR and below).

## Core Invariants

1. **An activation is the minimal suspendable execution entity, and the scheduler sees it through a
   payload-neutral token.** Process, task enable, and fork branch are all activations. The scheduler
   parks and resumes an activation token that carries process identity and wait state but never the
   completion type. _Consequence: one scheduler serves every activation kind; adding a completion
   shape never touches the scheduler._

2. **Completion is a typed terminal outcome held in the activation's completion slot, invisible to
   the scheduler.** An activation settles exactly one of `Succeeded(T)` and the departure that
   reached its landing. A departure is carried outward until something claims it, and only two
   things can -- a region naming the target it names, or the landing itself -- so what its two forms
   say is which of those was possible, never whether the execution was in error. The scheduler
   observes only parked / runnable / terminated. _Consequence: what a consumer reads is what the
   source language can read, a value the body produced or a departure a region lands, never
   transported through a side channel the scheduler or a backend invents._

3. **Ownership, continuation, and cancellation membership are three distinct relations.** Ownership
   decides who releases the activation; continuation decides who consumes its outcome and resumes on
   completion; cancellation membership decides who is cancelled when a domain is disabled. A direct
   task enable makes the enabler all three; a fork branch separates them -- attached to the spawning
   process's lineage, which determines storage ownership and cancellation membership, while its
   termination is what the join of the fork statement that spawned it waits for, with no single
   resuming continuation. Ownership is shared, and whatever runs an execution holds a share for the
   whole step. The step's own ending can drop every other share, as the last initial procedure of a
   program does when it ends the program's threads (LRM 24.3). _Consequence: these relations are not
   one pointer; a model that collapses them cannot express fork or selective cancellation. The
   lineage edge realizes two of them; the join realizes the third._

4. **Every external reference to a parked activation is a membership that ends with its owner, and a
   frame is destroyed only after no scheduler structure can name its token.** Each place that can
   later reach an activation -- a region queue, a delay slot, an event, a value change, a process's
   termination, a disable target -- holds a membership. A membership is **one record**: one end owns
   it and the target links it, so the relation is stored once and reached from both ends. The owner
   is whatever shares the membership's life: the activation for its place in a queue; the wait the
   body holds for what that wait watches, which names the activation only while the activation is
   parked on it; the process's record of a disable target for its being inside. A target's list
   holds memberships of one kind, and the scheduler's own queues and delay slots are targets like
   any other, so the engine is never searched. Leaving a list is a detach by the membership's own
   links, which needs neither the target nor its list head, so no membership points back at its
   target and no interface stands over the kinds of target; an owner keeps its memberships where
   they never move, since a target's list points at them, and a target released while memberships
   are still linked to it detaches them. What describes a pair of wait and watched storage -- the
   bits an event control compares -- belongs to the wait, not to either end. Stopping an activation
   from outside takes it off its queue and off the wait it is parked on, which keeps the wait
   itself, because starting it again waits for that same thing (invariant 7). What a wait keeps in
   order to answer it -- the closure an event control asks whether its expression moved -- belongs
   to the wait and ends with it, since it may refer to anything the frame holds; a foreign task's
   body, which runs on a stack of its own, is never outlived by the frame either, because an
   execution inside a foreign call is torn down only once that call has returned, or once the run is
   over, by unwinding the stack the call stands on, which passes through the call first (invariant
   8). _Serves LRM 9.6.2 and 9.7, which release an activation while it is parked: cancelling and
   tearing it down leaves no dangling token in any queue or wait -- its frame's waits and its queue
   place end with it, and so does every closure that could call back into the frame -- and
   cancelling one parked anywhere costs the same constant time._

5. **A typed await consumes the typed terminal outcome.** Awaiting an activation yields its outcome:
   `Succeeded(T)` produces `T`, and a departure carries on past the awaiting frame, which is not the
   landing. A pure suspension (a delay, an event control, an output-less task) is the `T = Void`
   case of the same await. _Consequence: nested enable is one uniform value flow -- a task awaiting
   a task gets a typed result with no per-call special handling._

6. **The activity taxonomy is a fixed set of activations with different consumers; a deferred effect
   is not an activation.** A process is owned by the run, has process identity, completes `Void`,
   and is observed by the engine (no continuation). A task activation inherits the enabler's process
   identity, completes with a typed payload, and resumes the enabler. A fork branch has its own
   process identity, is owned by the spawning process's lineage, completes `Void`, and is waited for
   by its fork's join. A deferred effect is a closure submitted to a region, not an activation. An
   execution of no lineage -- the carrier of an update due on an event, an assertion's action -- is
   a process in every respect but lineage: owned by the run, completes `Void`, observed by the
   engine, and reached by nothing that names processes. A deferred effect may start one, which is
   how deferred work that can wait enters this model. _Consequence: each kind reuses the activation
   core but binds its own ownership / continuation / completion; a deferred effect itself never
   enters the activation/completion model, and what it starts does so as an execution._

7. **An activation's disposition is authoritative, and suspension saves the disposition it
   replaces.** A non-terminal activation is `Executing`, `Runnable` (entitled to run; the region it
   sits in is the engine's placement, not part of the disposition), or `Blocked` (waiting on a
   condition, parked on the wait its frame holds for it). `Suspended` is not another kind of wait:
   it is process control (LRM 9.7) withdrawing an activation from whatever would resume it while
   saving the disposition it held -- `Suspended(Runnable)` or `Suspended(Blocked)` -- so resume
   restores exactly that. A saved `Runnable` resume re-takes an execution entitlement; a saved
   `Blocked` resume asks the wait it was parked on again whether to park there or carry on, while
   the wait's memberships stand throughout. Stopping at a wait and stopping there again are asked of
   the same thing and answered the same way whatever the construct, so no scheduler or activation
   path branches on which construct a wait came from; the construct-specific knowledge stays inside
   the wait the construct built, exactly as building it is one per-construct runtime call and the
   stop at it is construct-neutral. _Consequence: the disposition is one state machine with one
   authoritative owner; the wait the activation is parked on and its queue place are resources that
   must agree with it, never independent truths that drift._

8. **Publishing an activation's terminal outcome commits that it runs no more user code.** A
   consumer that reads the outcome, or a waiter woken by the activation's completion, may reclaim
   shared state the activation touched, so the terminal state and the waiter wakeups are published
   only after the body has run its last statement. When an activation cannot be torn down
   synchronously -- its frame is executing (a process disabling itself or an ancestor), or a foreign
   call must unwind cooperatively across a boundary the runtime does not own -- termination is a
   two-step transition: a request first takes the activation off whatever could resume it and
   records the cause, and the outcome is published only once the body reaches a safe boundary. The
   request is not the outcome; between them the activation is un-nameable by any scheduler structure
   yet still live, so a process handle reports a process inside a foreign call terminated only once
   that call has returned and nothing of it can run again. _Consequence: `Cancelled` may be
   requested while a frame is still running, but is settled -- frame released, what awaits it woken
   -- only at a safe boundary. Settling on the request, publishing the outcome before the body
   stops, lets a waiter observe a terminated activation that is still executing._

9. **An activation has exactly one live next-resume entitlement, and every handover withdraws the
   prior.** The single means by which an activation will next run -- its place on the running stack,
   its place in a scheduler queue, its being parked on a wait, or a saved suspended disposition --
   is one entitlement at any instant, never two. A transition -- a wake, a suspend, a cancellation
   -- withdraws the prior entitlement before establishing the next, so no two holders can resume the
   same activation. _Consequence: a cancellation invalidates the one entitlement and lets every
   affected activation reconcile at a single gate, rather than branching on which holder currently
   has it._

## Boundary to Adjacent Layers

- **To the scheduler ([`scheduling.md`](scheduling.md)).** The activation exposes a token the engine
  parks on a region queue and resumes; the engine reaches process identity and wait state through
  the token and nothing more. The engine is construct-neutral -- it branches on queue and region,
  never on the activation's kind or completion type.
- **To the callable ([`callable.md`](callable.md)).** A callable value is invoked to produce an
  activation. The callable owns the signature and result type (`Coroutine<T>` for a suspending
  callable); the activation owns the running instance and its terminal outcome.
- **To non-local control flow ([`nonlocal_control_flow.md`](nonlocal_control_flow.md)).** That
  document owns the region a departure lands in and the gate an execution passes where it regains
  control; this one owns the membership a process holds in each target it is inside, and the
  entitlement the gate reconciles.
- **To MIR ([`mir.md`](mir.md)).** The typed completion is MIR semantics: a suspending callable's
  result type is `Coroutine<T>`, and `await : Coroutine<T> -> T` consumes the terminal outcome. The
  activation model is how a backend realizes that semantics at runtime; the MIR shape is
  backend-neutral.
- **To LIR.** A backend lowers an activation's suspend, resume, success, and departure points to
  explicit control edges; the completion slot's outcome is the value an await's resume edge carries.

## Forbidden Shapes

Each names the requirement it fails.

- **A scheduler that branches on completion type or callable kind** fails invariant 1: an engine
  that asks "is this a task / does this return a tuple" has let completion shape into execution
  control, and adding a completion shape then changes the engine.
- **A completion value transported outside the completion slot** -- a hidden out-parameter, a
  backend-invented result sink, a side-channel reference writeback -- fails invariant 2: a value
  reaching the consumer any other way is a backend inventing transport MIR did not state.
- **`output` / `inout` as a live alias of the caller's storage** fails LRM 13.3: they are
  completion-payload components settled once, not storage the activation writes during its run, so a
  live alias lets a caller observe an intermediate write while the activation is suspended. Only a
  `ref` is a live alias ([`callable.md`](callable.md)).
- **Collapsing ownership, continuation, and cancellation into one relation** fails LRM 9.6: a single
  "parent pointer" meaning owner and consumer and cancellation scope cannot express a fork (owned by
  the lineage, consumed by a join, not resumed as a continuation) or selective cancellation.
- **A reference to a parked activation that nothing ends** -- a raw token pushed into a queue,
  waiter list, or subscription set with no membership whose owner ends it -- fails invariant 4 and
  LRM 9.7: it dangles the moment the activation is released while parked.
- **The same membership recorded on both sides** -- a target that stores its own record of which
  activations it holds while each activation separately stores which targets hold it -- fails
  invariant 4: two authoritative copies of one relation must be reconciled, so leaving degrades into
  searching the other side, enrolling has to defend against drift, and each end needs teardown logic
  whose only job is to repair the other.
- **Every membership owned by the activation** fails invariant 4 from the other side: a wait that
  stands across a loop outlives each stop the activation makes at it, and a process is inside a
  disable target across many activations, so tying each to the activation either builds and tears
  them down at every stop or gives the activation records of lives that are not its own.
- **A deferred effect modeled as an activation with a completion** -- a non-blocking assignment, a
  postponed `$strobe`, or a deferred assertion action given an activation token, a completion slot,
  or an await edge -- fails invariant 6: these neither suspend their submitter nor settle an outcome
  a consumer awaits.
- **A central taxonomy of wait kinds the scheduler or activation core branches on** -- a
  `variant`/enum of delay / event / join / await blocks switched over on suspend, resume, or wake --
  fails invariant 7 and the engine's construct-neutrality ([`scheduling.md`](scheduling.md)): what
  an activation waits for answers one surface whatever the construct, and a kind switch on the
  execution path reintroduces the source-language timing concept the engine must not know.
- **A second authoritative copy of the wait's state** -- a blocked-operation object that duplicates
  the deadline / observable / target the suspending construct already holds, kept in sync with it --
  fails invariant 4 one level up: the wait's state has one home, the wait the body's frame or its
  reaction holds.
- **Scheduling placement folded into the disposition** -- a `Runnable(region)` that pins which
  region or queue a runnable activation must be restored to -- fails LRM 9.7, which places a resumed
  runnable activation in the active region: `Runnable` is the entitlement to run, and the region is
  the engine's placement, chosen when the activation is scheduled.
- **`Suspended` modeled as a new kind of wait, or the disposition split across independent fields**
  -- a suspended activation given its own wait target to re-fire, or its state inferred from
  execution flags plus queue membership plus which wait it is parked on -- fails invariant 7:
  suspension saves the prior disposition, which is one state its resources must agree with.
- **A fork branch modeled as a task the parent awaits** fails LRM 9.3.2: a branch has its own
  process identity, is owned by the spawning process's lineage, and is waited for by its fork's join
  under a join-mode condition; `join_none` has no parent wait at all.
- **A cancellation domain scoped to the fork statement rather than to the process** fails LRM 9.6.1
  and 9.6.3: `disable fork` terminates every descendant of the calling process, spawned by any fork
  that process ever executed, and `wait fork` observes the immediate children accumulated across all
  of them. The join is per fork statement because the join condition is; the lineage is per process
  because cancellation and child observation are.
- **A lineage node whose lifetime is its activation's execution** fails LRM 9.6.3, which reaches
  "the descendants of subprocesses that have already terminated": releasing the node with the frame
  loses that reach, and retaining the frame to keep the node pins the activation storage the frame
  solely owns.
- **A cancellation or scheduling decision that branches on whether an activation is running,
  waiting, or runnable** fails invariant 9: an operation that asks which to decide how to reach it
  has taken on the scheduler's placement job and duplicated the holder state the disposition already
  owns.

## Notes / Examples

A direct task enable is the case where the three relations coincide. `int x = <enable t(a)>`:

```text
enable t(a)  ->  child task activation, completion slot typed `int`
  ownership            = the enabling frame
  continuation         = the enabling frame (resumes, consumes the outcome)
  cancellation domain  = the enabling frame's domain (disabling the enabler cancels the child)
await  ->  Succeeded(int) yields x ; a departure carries on past this frame
```

A fork splits them:

```text
fork branch  ->  branch activation, completion slot `Void`, own process identity
  ownership            = the spawning process's lineage (it releases the branch)
  continuation         = none (the branch resumes no one)
  cancellation domain  = the spawning process (an ancestor's `disable fork` reaches it)
join  <-  waits on each branch's termination, completed or killed
  join / join_any / join_none decide when the forking process resumes
```

Ownership and cancellation membership ride the same lineage edge here; continuation rides the join.
That the first two coincide for a branch is not a collapse of the relations -- what invariant 3
forbids is one edge that also carries the consumer.

A membership, and leaving it:

```text
membership  = one record: "this target can reach this owner"
  prev / next  -- linked into the target's list
  owned by     -- the wait (what it watches), the process (a disable target), the activation (a queue)
leave       = prev->next = next; next->prev = prev     // the node alone, never the target
```

Moving a region's whole set of activations to another queue -- taking its snapshot out before
draining it, so work the drain produces waits for the next pass -- relinks one list onto another,
and a bulk termination can wake activations as it goes even where a later step releases one of them,
since a queued activation leaves its queue with its frame.

A reaction is an activation that is no frame: a process whose body runs to its end each time it is
triggered, held by the run with the object its parts run on and the wait its setup builds. It runs a
static process, which has no lineage parent; it ends once its body returns, and its
scheduler-visible core -- disposition, wait, queue place -- is the same as a frame's; only the
resume entry differs, calling a part rather than resuming a frame. Its process identity is answered
by a process object made the first time the language asks for one.

The C++ backend realizes a frame's activation as a coroutine frame. Its activation token is a
non-templated member of the coroutine promise (the scheduler holds a pointer to it); its completion
slot is the typed result the promise carries, which keeps a value too large for a frame in storage
of its own; a task enable is `co_await` of the activation, with symmetric transfer realizing the
continuation and `await_resume` consuming the terminal outcome. Each timing control is a wait the
frame holds as a local, carrying what its construct needs to answer a stop there again -- a delay
its absolute deadline, an event control its observables and edges -- and the activation names the
one it is parked on for as long as it waits. The promise base, the `coroutine_handle`, and symmetric
transfer are this realization's mechanics; the activation, completion slot, memberships,
disposition, and what it waits for are the model they realize. The LIR / LLVM backend realizes the
same model with coroutine intrinsics and explicit control edges instead, reaching the same object
rather than re-entering the suspended body. There the activation token is a coroutine the runtime
owns, which drives the generated body's own coroutine through its handle -- resume, done, destroy --
so no generated frame embeds a runtime C++ type, and a departure the body handed over is raised
again from that coroutine's frame once the body is done. A call that suspends answers into a cell
the caller reads once the call completes, which nothing observes and whose write wakes no one.
