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

- [ ] **The terminal outcome is split, not unified.** Contract invariant 2: an activation settles
      one completion slot holding `Succeeded(T)` / `Faulted(exception)` / `Cancelled`, and the
      scheduler-visible execution core carries none of it. Current shape: the success value lives in
      the typed completion storage, but the exception is carried by the scheduler-visible execution
      core, separate from the value. That is wrong -- the exception is part of the terminal outcome,
      not execution control. Target: one completion slot owns the whole outcome (value and
      exception, later cancellation); a consumer reads a single terminal outcome; the execution core
      the scheduler sees is purely suspend / resume / wait state / identity. Unblocked; doable now.

- [ ] **Registrations are only partly revocable.** Contract invariant 4: every external reference to
      a parked activation -- a region queue slot, a delay slot, an event waiter entry, a
      value-change subscription, a join aggregator entry -- is a revocable registration the
      activation owns, and a frame is destroyed only after the registration set is empty. Current
      shape: value-change subscriptions and the completion-aggregator waiter (the parked parent of a
      `join` / `join_any`, and the parked process of a `wait fork`) are revocable registrations that
      detach safely from either end; but a delay slot, region-queue membership, and event-waiter
      entries still hold references the activation cannot itself revoke. Target: one revocable
      registration set covering every registration kind, with destruction gated on it. Until then,
      no new registration kind may be added that an activation cannot revoke. Partly gated on
      cancellation (below), but the general registration-set contract should land first.

- [ ] **Cancellation is absent.** Contract: the terminal outcome includes `Cancelled`; a
      cancellation domain -- a relation distinct from ownership and continuation -- decides which
      activations are cancelled together; disabling settles `Cancelled`, revokes every registration,
      cancels the owned descendants, then releases the frame. Current shape: the cancellation-domain
      relation now exists -- the process lineage, over which a `disable fork` would walk, with a
      terminated process retained while any descendant is live -- and a body's terminal execution
      state is outcome-neutral, ready to carry `Cancelled`; but there is no cancellation path and no
      `Cancelled` outcome yet. Target: the full cancellation model behind `disable` / `disable fork`
      (LRM 9.6.x). Blocked on the `disable` language feature; the activation lifetime must not
      foreclose it (it does not -- frame ownership already cascades, and the registration-set and
      outcome-unification items above are the remaining prerequisites).

- [ ] **Runtime vocabulary trails the model.** The execution code names the activation and its core
      in coroutine-implementation terms; the contract's vocabulary is activation / completion slot /
      cancellation domain / join state, with the coroutine mechanics as one realization. The
      execution-state axis is now named for the model -- a process's states are execution states,
      and its end state is outcome-neutral termination rather than "completed" -- but the
      completion-slot and join-state vocabulary still trails. Rename the rest where it clarifies the
      boundary between execution control and completion. Low priority; unblocked.
