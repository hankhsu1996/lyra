# An activation has one authoritative disposition, and a wait is a pending capability distinct from its enrollment

Date: 2026-07-15. Status: accepted.

## Why this decision matters

Process control (LRM 9.7) `suspend()` / `resume()` is the first operation that must **re-establish a
wait without re-entering the body that created it**. A running body registers a wait through a
runtime call (a delay, an event control, a level wait, an `await`) and then suspends; every other
mechanism -- normal wake, `disable`, `kill` -- either resumes the body past that point or destroys
it. None needs to reconstruct the wait. `resume()` does: it must re-sensitize a process the body
cannot re-enter to re-run the registration.

`activation-registration.md` gave the runtime a `Registration` -- the activation's revocable
membership in a wake target. That is enough to _detach_ a wait (revoke the enrollment) but not to
_re-establish_ one: a registration records where the activation is presently linked, and a revoke
forgets it. The wait itself -- the absolute deadline, the observables and edges, the target process
-- has no retainable home; it survives only because the awaitable temporary holds it inside the
opaque coroutine frame and the normal wake path re-enters `await_resume`. `suspend`/`resume` breaks
both assumptions.

## The failure this decision is drawn from

The first design answered this with a `RearmCapability`: an opaque
`move_only_function<void(RuntimeServices&)>` on the activation's frame core, installed at park,
invoked at resume. Its API would not settle -- one-shot vs retained, when to clear, `bool` vs enum
result, a special case for the delay bucket, a capture-ownership rule, a cached current frame. Each
question looked like closure tuning; together they were the tell of a missing state machine leaking
into a single slot. The closure was being asked to be, at once:

- the **blocked operation** (how to re-establish the wait),
- the **saved disposition** (was the activation blocked, or already runnable when suspended),
- the **wake state** (has the wait fired), and
- the **scheduling effect** (which region to resume into).

A slot whose surface is that many unrelated responsibilities is the wrong structure -- the same
reading `activation-registration.md` applied to the two-authoritative-copies registration: when the
machinery serves reconciliation rather than the relation, the model is missing a concept. Here the
missing concept is the activation's **disposition** and, under it, a **pending wait** that is
separate from its **registration**.

## The model

```text
ActivationDisposition   -- how an activation participates in execution; authoritative
  Executing
  Runnable              -- entitled to run; the region is the engine's placement, not carried here
  Blocked               -- waiting on a condition: one Registration (enrollment) + one PendingWait
  Suspended(saved)      -- scheduler participation revoked; saves Runnable or Blocked to restore
  Terminal

Registration  -- the activation's CURRENT enrollment in a wake target's list (unchanged).
PendingWait   -- a Blocked activation's retainable ability to re-establish its wait and to report
                 whether the wait is already satisfied. One uniform capability, not a taxonomy.
```

A `Blocked` activation holds both: the `Registration` is where it is linked now; the `PendingWait`
is how to link again. Revoking the registration (desensitize) leaves the pending wait intact.
Re-establishing (`resume`) asks the pending wait to re-enroll or to report the condition already
met. A normal wake consumes the pending wait and moves the disposition to `Runnable`; termination
discards it.

The wait's own state -- the deadline, the observables and edges, the target -- stays in the
construct that registered it (its retained awaiter/registration state); the pending wait is the
capability to re-establish _from_ that state, reached uniformly, never a second copy of it.

## The decisions

```text
D1. The disposition is the one authoritative state. The registration set, the pending wait, and
    run-queue membership are resources constrained to agree with it, never independent truths. Debug
    invariants tie them: Blocked has a pending wait and a registration; Suspended(Blocked) has a
    pending wait and no registration; Runnable has a run-queue registration; Terminal has neither.

D2. A registration is the current enrollment; a pending wait is the retainable wait. They are
    distinct. `suspend` of a blocked activation detaches the registration and keeps the pending wait;
    `resume` re-establishes through the pending wait.

D3. The pending wait is a UNIFORM capability -- re-establish, report-satisfied, discard -- supplied
    by each suspending construct. No scheduler or activation path branches on which construct a wait
    came from. This is the same construct-neutrality `jit-process-suspension.md` fixed for the
    execution backend, where a wakeup is one per-construct registration call and the suspend is a
    generic edge; re-establishing a wait is that same per-construct call issued again, dispatched
    uniformly.

D4. The wait's state has one home: the construct's own retained state. The pending wait re-establishes
    from it; it does not mirror it into a second authoritative object.

D5. `Suspended` is not another kind of wait. It saves the disposition it replaced --
    `Suspended(Runnable)` or `Suspended(Blocked)` -- and `resume` restores exactly that: a saved
    Runnable re-takes an execution entitlement; a saved Blocked re-establishes its pending wait,
    which re-enrolls or reports satisfied.

D6. `Runnable` carries no region. The region (active / inactive / next-delta / delay bucket) is the
    engine's placement, chosen when the activation is scheduled. `resume` places a runnable-suspended
    activation into the active region (LRM 9.7); it does not restore a saved region.

D7. A process has exactly one active leaf activation while not executing (a task/function call runs in
    the caller's thread, LRM 9.5, so a process is one frame stack with one innermost enrolled or
    runnable frame). Process control names the process and acts on that leaf. The process -> active
    leaf relation is a maintained invariant, not an incidental cache.
```

## Consequences

- The wobble in the rearm-capability API disappears, because each question it could not answer is
  now owned elsewhere: blocked-vs-runnable is the saved disposition (D5), when-to-clear is "wake
  consumes the pending wait" (D1), region is the engine's (D6). In particular, the ad-hoc "clear the
  rearm at the wake verb so the woken-but-not-resumed window re-queues instead of re-waiting" rule
  is not a rule at all: a wake moves `Blocked -> Runnable`, so a later `suspend` saves `Runnable`
  and `resume` re-queues -- it falls out of the state machine.
- The per-construct resume semantics live in one place per construct and read as the LRM rule: an
  edge re-subscribes (a trigger during suspension is missed), a delay compares its absolute deadline
  (a delay that transpired during suspension resumes runnable), a monotonic condition (join, wait
  fork, await) re-checks. None of this is a scheduler branch.
- The two backends realize the same model: the C++ backend keeps the construct's retained state in
  the awaiter frame and the pending wait as a constrained capability over it; the execution backend
  re-issues the construct's wakeup-registration call behind a generic suspend edge
  (`jit-process-suspension.md`). Neither needs a central wait-kind enum.

## Rejected alternatives

- **A typed taxonomy of blocked operations** --
  `variant<DelayBlock, ValueChangeBlock, JoinBlock, ...>` visited on suspend / resume / wake. It
  reads as more inspectable, but a kind switch on the execution path is exactly the source-language
  timing concept `scheduling.md` and `jit-process-suspension.md` forbid the scheduler to know, and
  it duplicates the wait state the construct already holds (D4). The construct-specific knowledge
  belongs in the construct's own registration, reached through a uniform capability, not in a
  central variant.

- **`Runnable(region)`** -- pinning the region a runnable activation must be restored to. `resume`
  restores an activation to the active region regardless of where it sat (LRM 9.7); carrying the
  region folds scheduler placement into activation semantics (D6).

- **A second authoritative copy of the wait's state** -- a blocked-operation object mirroring the
  deadline / observable / target the construct already holds. This is the two-authoritative-copies
  shape `activation-registration.md` rejected, one level up (D4).

- **"An opaque re-establish closure cannot map to the execution backend."** Retracted. Lyra's
  runtime effects are ordinary closures / per-construct calls already
  (`runtime-effects-as-generic-calls.md`, `jit-process-suspension.md`); the execution backend
  re-realizes a re-establish as re-issuing the construct's registration call behind a generic
  control edge, with no need to introspect a kind. The closure is a valid C++ realization of the
  pending-wait capability; it is not the model, and it is not backend lock-in.

## Cross-references

- `../architecture/activation.md` -- invariant 7 (disposition and pending wait), the refined
  invariant 4 (registration as enrollment), and the forbidden shapes this entry's D3 / D4 / D6
  state.
- `../architecture/scheduling.md` -- the engine as a construct-neutral mechanism; a wait kind is not
  a distinction the execution path may branch on.
- [activation-registration](activation-registration.md) -- the registration as one record owned by
  the activation; this entry separates that enrollment from the retainable wait it serves.
- [jit-process-suspension](jit-process-suspension.md) -- the execution-backend counterpart: a
  per-construct wakeup registration behind a generic suspend edge, which re-establishing a pending
  wait re-issues.
