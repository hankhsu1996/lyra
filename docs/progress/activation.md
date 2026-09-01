# Activation model: gaps to the contract

Tracks where the runtime's execution code differs from the golden activation model in
`../architecture/activation.md`. Each entry names the current shape, the contract shape it must
reach, and what (if anything) blocks it.

The runtime already realizes the load-bearing core of the contract: an activation is a coroutine
frame; the scheduler holds a payload-neutral activation token and never sees the completion type; a
suspending callable's result type is `Coroutine<T>` and a typed await consumes its value; a spawned
process is attached to its spawner's lineage, which is both the ownership and the
cancellation-domain relation and outlives the execution it named while any descendant is live; and
its outcome is aggregated by a join state. The gaps below are where the current shape is still wrong
or incomplete relative to the contract.

## Items

- [x] **The terminal outcome is unified.** Contract invariant 2: an activation settles one
      completion slot holding `Succeeded(T)` / `Faulted(exception)` / `Cancelled`, and the
      scheduler-visible execution core carries none of it. The value and the exception now travel
      together in one typed completion slot held off the scheduler-visible core; a consumer reads
      the whole outcome once, which re-raises a fault or yields the value. The execution core the
      scheduler sees is purely suspend / resume / wait state / identity. The `Cancelled` slot
      alternative is not present: as the cancellation item below settles, killed-ness is realized as
      a persistent fact of the process node, read by `status()` / `await()`, not as a slot value.

- [x] **Registrations are revocable.** Contract invariant 4: every external reference to a parked
      activation -- a region queue slot, a delay slot, an event waiter entry, a value-change
      subscription, a join aggregator entry -- is a revocable registration the activation owns, and
      a frame is destroyed only after the registration set is empty. Every one of those is now the
      same thing: a membership recorded once, owned by the activation and merely linked by the
      target. Because the relation has a single record rather than a copy on each side, revoking is
      a detach -- neither end searches the other, and neither can hold a belief the other has
      abandoned. Releasing an activation detaches every membership it still holds, so nothing --
      queue, delay slot, waiter list, subscription, join condition -- is left able to resume it. No
      new registration kind may be added that an activation cannot revoke, and adding one means
      giving a target a list, not teaching the scheduler a new way to be searched.

- [x] **Cancellation over the dynamic domain settles a reader.** Contract: a cancellation domain --
      a relation distinct from ownership and continuation -- decides which activations are cancelled
      together; disabling revokes every registration, cancels the owned descendants, then releases
      the frame. Current shape: `disable fork` (LRM 9.6.3) and `kill` (LRM 9.7) both walk the
      process lineage, which is that domain, and terminate the whole descendant subtree -- including
      the descendants of subprocesses that have already terminated. Both funnel through one terminal
      transition: each terminated node is marked KILLED and its frame released, and releasing a
      frame revokes its registrations, so nothing -- queue, waiter, subscription, or a handle held
      past the kill -- is left able to resume it.

      Cancellation now has a reader. `process::status()` reports a killed process as KILLED through a
      handle that outlives it, and `process::await()` (LRM 9.7) suspends a process until another
      terminates -- normally or forcibly -- then reads that outcome. KILLED is realized as a
      persistent fact of the process node, a terminal cause distinguishing a finished process from a
      killed one, not as a `Cancelled` value in the completion slot: a cancelled activation is
      released while parked, so its slot is never read. The contract's three-way completion slot
      (`Succeeded` / `Faulted` / `Cancelled`) should be revised accordingly -- cancellation is a
      lifetime event observed through status / await, not a third terminal value the consumer reads
      from the slot.

      One gap: the killed subtree must be off the calling process's own execution stack, because
      terminating a process releases (destroys) its frame and a running body cannot destroy the frame
      it executes in. Killing the calling process or one of its ancestors is rejected for now.
      Supporting it needs a deferred safe-boundary termination -- mark the target, unwind the calling
      body to the scheduler's resume boundary, and tear the frame down there -- rather than the
      synchronous release used for an off-stack subtree.

- [x] **Pause and resume settle on the disposition model.** `process::suspend()` / `resume()` (LRM
      9.7) pause and restart a process. An activation carries an authoritative disposition and a
      blocked one holds a retainable pending wait, distinct from its registration (enrollment): a
      suspend detaches the enrollment and saves the disposition, and a resume re-establishes the
      pending wait -- re-enrolling, or becoming runnable if the condition already holds. Each
      suspending construct supplies that re-establish uniformly, so its own LRM resume rule reads in
      one place: an edge or named event re-subscribes (a trigger during suspension is missed), a
      delay compares its absolute deadline (a transpired delay resumes runnable), a monotonic
      condition (join, wait fork, await) re-checks. `status()` reports SUSPENDED. See the activation
      contract and the activation-disposition decision.

- [x] **Disable of a named block or task.** `disable` (LRM 9.6.2) selects its target by static block
      or task identity and reaches every execution currently inside it, without regard to the
      process lineage: each such execution resumes after the scope, and every activity enabled
      within it terminates. Unlike `disable fork`, the cancellation domain is static declaration
      identity, not the dynamic lineage. The target is invalidated and every affected execution
      leaves it through one control effect the naming region consumes (see the
      disable-scope-invalidation decision). Implemented for a named block, a named fork, and a task,
      at any nesting depth, reached from the same process or a concurrent one, and across every
      activation of a reentrant task. Membership is carried by the running process rather than
      derived from where a body is written, so it spans a call: a task called from inside a disabled
      scope terminates with it instead of running to completion. An activity spawned inside the
      scope takes that membership at the spawn, so a fork child terminates whether it was spawned
      directly, spawned by a task called from the scope, or not yet started when the disable landed;
      such a child reports KILLED (LRM 9.7), the same status a `disable fork` or `kill` gives it.
      The effect arises where an execution regains control -- a wait resuming, the `disable`
      statement itself -- and a spawned activity disabled before it ever runs is terminated without
      executing a statement. What a body carries is the region alone, and both backends run the
      construct: the one whose bodies can be unwound through reaches an execution inside a region
      that way, and the one whose bodies cannot asks, at those same points, whether a target it is
      inside has been disabled. Which targets an execution is inside is the execution's own state,
      so a spawned branch is enclosed by the targets its spawner was inside even though its body
      states no region, and a `disable` of a named `fork` reaches a branch parked on a delay. Two
      cases remain, both a located diagnostic until then: a target in another module instance or
      generate scope needs hierarchical addressing to the owning instance's scope, and a class
      method body is inside no structural scope that mints one. Disabling a task is the one form
      still waiting on the backend that has no exceptions, because enabling one there is an await on
      a body that completes as a coroutine.

- [ ] **Runtime vocabulary trails the model.** The execution code names the activation and its core
      in coroutine-implementation terms; the contract's vocabulary is activation / completion slot /
      cancellation domain / join state, with the coroutine mechanics as one realization. The
      execution-state axis is named for the model -- a process's states are execution states, and
      its end state is outcome-neutral termination rather than "completed" -- and the terminal
      outcome is now a named completion slot; the join-state vocabulary still trails. Rename the
      rest where it clarifies the boundary between execution control and completion. Low priority;
      unblocked.
