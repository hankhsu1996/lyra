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
      scheduler sees is purely suspend / resume / wait state / identity. The `Cancelled` alternative
      is not present, and as realized it has no reader -- see the cancellation item below.

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

- [ ] **Cancellation covers the dynamic domain only, and settles no outcome.** Contract: the
      terminal outcome includes `Cancelled`; a cancellation domain -- a relation distinct from
      ownership and continuation -- decides which activations are cancelled together; disabling
      settles `Cancelled`, revokes every registration, cancels the owned descendants, then releases
      the frame. Current shape: cancellation over the dynamic domain is real. `disable fork` (LRM
      9.6.3) walks the process lineage, which is that domain, and cancels the whole descendant
      subtree -- including the descendants of subprocesses that have already terminated; releasing
      each frame revokes its registrations, so nothing is left able to resume it.

      Two gaps remain. `disable` of a named block or task (LRM 9.6.2) is not cancellation over the
      dynamic domain at all: it selects its targets from static, syntactic block identity, and it
      transfers control in the disabling process (execution resumes after the named block), so it is
      a control-flow construct layered on this model rather than a use of it.

      And no `Cancelled` value is settled. Cancelling releases the activation, so its completion slot
      is never read: the disabled subtree is destroyed whole, which leaves no surviving consumer
      awaiting a cancelled activation. A settled `Cancelled` first acquires a reader with LRM 9.7
      `process::status()` (a killed process reports `KILLED`), so the outcome is deliberately not
      materialized ahead of that reader. The contract's three-way completion slot should be revisited
      against this: as realized, cancellation is a lifetime event, not a third terminal value.

- [ ] **Runtime vocabulary trails the model.** The execution code names the activation and its core
      in coroutine-implementation terms; the contract's vocabulary is activation / completion slot /
      cancellation domain / join state, with the coroutine mechanics as one realization. The
      execution-state axis is named for the model -- a process's states are execution states, and
      its end state is outcome-neutral termination rather than "completed" -- and the terminal
      outcome is now a named completion slot; the join-state vocabulary still trails. Rename the
      rest where it clarifies the boundary between execution control and completion. Low priority;
      unblocked.
