# `disable` invalidates a target's generation, and leaving it is one control effect the naming region consumes

Date: 2026-07-18. Status: accepted; realized in both backends for a named block, a named fork, and a
task, including nested scopes, tasks called from within a scope, and activities spawned within one.
On the execution backend the cases reaching it are those needing no second activation, since a task
enable and a fork branch are not lowerable there yet.

## Why this decision matters

LRM 9.6.2 `disable <named block or task>` terminates the activity of a statically named block or
task: every execution currently inside it resumes after it, and every activity enabled within it is
terminated. It selects targets by static declaration identity, without regard to the dynamic process
lineage (LRM 9.6), so it can reach an execution in any process, and a recursive or reentrant task
has several concurrent executions of one target.

The naive shapes make `disable` branch on where each affected execution currently sits -- running,
waiting, or runnable. That branch is the defect: a cancellation that asks "is this execution
running, waiting, or runnable" has taken on the scheduler's placement job.

## The model

A **cancellation source** is the per-instance runtime endpoint of a disable-target named block or
task. It carries one monotonic **generation**. An execution entering the scope captures the
generation.

An activation has, at any instant, exactly one **next-resume entitlement** -- the single live means
by which it will run its next statement (activation.md). It is held by whichever part of the
scheduler substrate the activation's state uses: the running stack, a scheduler-queue registration,
a wait-target registration, or a saved suspended disposition. Runnable enrollment and blocked
enrollment are already one shape -- a `Registration` linked into a `RegistrationList`
(activation-registration.md).

`disable B` invalidates the scope by bumping its generation. Every entitlement bound to the old
generation is then re-presented to a uniform **validity gate** before the activation runs its next
user statement. The gate compares the captured generation against the scope's current one; a
mismatch means the entitlement is invalid.

## The decisions

D1. `disable B` is scope-generation invalidation. Its whole semantic decision is bumping the
generation and triggering the scope's cancellation source. It does not inspect whether an affected
activation is running, waiting, or runnable.

D2. A target is a thing an execution waits on, so `disable` releases waiters rather than searching
for them. Only the execution itself can leave a scope, so all a `disable` has to do is give control
back to the executions that would never regain it -- the blocked ones. Being blocked inside a target
is itself a wait: the execution waits for that target not to be disabled, alongside whatever else it
waits for. It therefore enrols in the target by the same means it enrols in an event, any one of
them releases the wait, and releasing revokes the rest (activation-registration.md D7, D8).
`disable` then fires a list, the same act an event trigger performs. The alternative -- no list on
the target, and a `disable` that searches the live executions for the ones inside it -- would be the
only place in the runtime that finds its waiters by searching instead of by firing a list.

D3. The check is generation mismatch alone. There is no resume-reason and no pending-control flag:
the captured generation versus the target's current generation is the whole signal, asked before the
next user statement. This is the construct-neutral, monotonic-state re-check the runtime already
uses (activation-disposition.md D3). It is asked in the runtime, at the points where an execution
regains control -- a wait resuming, and the `disable` statement itself -- so no comparison is
emitted and no body holds a generation of its own.

D3a. Membership in a target is a property of the running process, not of a body's lexical position.
The process carries the targets its execution is inside, each with the generation captured when it
was entered; this set spans a task call, because a called task is executing inside whatever targets
its caller was inside. Deriving membership from a body's lexical scope instead cannot see a target
the body is not written inside, which is the caller's target for a called task and the spawner's for
a fork branch.

D3b. An activity spawned inside a target takes that membership at the spawn, from the spawner's live
set -- not by rebuilding it when the activity starts running. LRM 9.6.2 makes an activity a member
by where its spawner was, and a spawned process does not start until its spawner blocks or
terminates (LRM 9.3.2), so a disable can land while the activity exists but has executed nothing.
Capturing at the spawn makes it a member from the instant it exists, and reaches targets that its
own body is not written inside at all.

D4. Leaving a disabled target is one control effect naming that target, raised by the runtime and
carried outward by the ordinary means a nested call already returns a result by. It is raised where
the execution regains control and names the outermost target it is inside that has been invalidated,
because leaving that one also leaves everything nested in it. It travels through the frames between
without those frames carrying anything for it, and stops at the region that names the same target,
which resumes past it: a named block continues after the block, a task named by a `disable`
completes normally so its enabling statement resumes. An execution with no such region -- a spawned
activity, which has nothing after the target to continue into -- is terminated by it, and settles
KILLED rather than FINISHED (LRM 9.7). This split is the only essential divergence, and it is one
the LRM mandates.

D5. No new runtime concept is introduced. There is no execution-entitlement object, no live-extent
registry, no stored exit continuation, and no traversal of the live executions: a target reuses the
registration substrate every wait target already uses, and membership -- which targets an execution
is inside, and the generation it captured entering each -- is state of the running execution.

D6. Only the region that consumes the effect is expressed in the compiled program. A suspend, a
call, and a `disable` are the same program whether or not any target encloses them -- the generation
check and the outward travel are the runtime's, never statements a body states for itself. What a
body does carry is the region itself: a scope that can be left from anywhere within it, including
from a callable it invoked, and whose continuation is the statement after it. That is a generic
control construct, not a cancellation mechanism, and each backend realizes it in its own terms: one
whose bodies can be unwound through reaches an execution inside a region that way, and one whose
bodies cannot asks the runtime, at the points where such an execution regains control, whether a
target it is inside has been disabled. Either way the effect's origin, its outward travel, and which
region consumes it are fixed here and re-decided by neither.

## Consequences

- `disable` gains no state branch: it invalidates, then releases its waiters through the existing
  wake verb; the running activation -- only ever the one executing the `disable` -- reaches the gate
  after its statement, and an already-runnable one reaches it when next drained.
- A suspend enrols in the targets its execution is inside, which are only the named scopes it has
  entered -- none, for a body that entered no named scope, so such a body pays nothing.
- The frontier of a recursive or reentrant disable is implicit: the cascade stops at the first valid
  generation, so no outermost-extent computation exists.
- An activation that enters the scope after the `disable` captures the new generation and is
  unaffected.
- The gate is an integer compare, the same in every backend. The region is not: realizing it needs a
  way to leave a scope from anywhere within it, which a backend either has or must build. Both have
  it now. A backend that unwinds gets it from the target language and touches nothing inside the
  region. A backend that cannot be unwound through builds it from the region's own structure: the
  ways out of a body are enumerable in a control-flow graph, so the extent's membership is bracketed
  by an entry and a cleanup that runs on each of them, and at the points inside the region where an
  execution regains control the body asks the runtime for the gate's answer. The check is still the
  runtime's -- what crosses is what it decided, not a comparison the body makes.

- The extent's two ends have to be stated, not implied. Marking membership with a value whose
  lifetime is the body's asks the target language to run code at scope exit, which is a facility
  only some have; the region states an entry and a cleanup instead, and each backend realizes the
  cleanup its own way. This also covers the ways out that are easy to forget: a `return` or a
  `break` leaving a named block leaves its target too.

## Rejected alternatives

- **Local control transfer (a goto to after the block).** Realizes only the current frame's exit. It
  drops static-identity selection, the all-activations requirement, and enclosed-activity
  termination, so it is correct only for a single-extent, fork-free block -- a case, not the
  construct.

- **Extent records and a computed per-thread transfer frontier.** A per-instance live-extent
  registry plus a computed outermost-extent frontier carried out of the affected execution. Rejected
  because the extent registry duplicates what the execution's own membership already records
  (identity_and_ownership.md forbids the id-plus-registry and duplicate-ownership shapes), and
  because no frontier need be computed: the outermost invalidated target is read from the
  execution's own membership, and the region that names it is found by the effect travelling
  outward.

- **A gate emitted after every resumption, unwinding by branch.** The check placed in the compiled
  body at each point an execution could regain control, leaving by a jump to a landing the lowering
  threaded in. It works, and it is what this decision first specified. It was replaced because it
  put cancellation into bodies that have nothing to do with it -- a task suspends and resumes
  identically whether or not any target encloses it, yet carried a check and a landing for one --
  and because the outward travel had to be re-threaded at every callable and spawn boundary the
  effect crossed. The check belongs where control is regained, which is the runtime, and the outward
  travel is what a nested call's ordinary result path already does.

  A backend that cannot be unwound through does ask at those points, and the difference from what is
  rejected here is what makes it not this shape. It asks only inside a region, so a body enclosing
  no target is untouched; it asks for an answer the runtime computed rather than comparing
  generations itself; and it threads nothing across a callable or spawn boundary, because an effect
  no region of a body claims is that activation's completion outcome and the boundary carries it as
  such. What was rejected was making the gate and the travel the mechanism; what a region realizes
  is the landing alone.

- **An explicit resume-reason or a pending-control state on the activation.** A single "disabled"
  signal cannot name which scope or how far to unwind, which recursion and nesting require; the
  generation supplies both. A new enum is a second source of truth beside the generation.

- **A dedicated execution-entitlement runtime object.** The single next-resume entitlement already
  exists implicitly as the disposition plus its one live registration; naming it as a parallel
  object re-labels what the registration and the disposition already are.

- **Membership rebuilt from the body's lexical scope, per callable.** Re-establishing the enclosing
  targets by re-emitting entry guards in each body -- which a fork branch did for its own coroutine
  -- makes membership a property of where a body is written. It cannot express a task called from
  inside a target (the task's body is not written inside it), nor a fork inside such a task, and it
  cannot cover a disable that lands before a spawned branch first runs, because there is no body
  executing yet to rebuild anything. Membership is carried by the running process and captured at
  the spawn instead (D3a, D3b).

## Interface contracts

Three interface contracts carry the model into the implementation and are where the care lives:

1. Waking a blocked execution through the same verb a normal wake uses, so its pending wait settles
   exactly once whichever source releases it.
2. Re-home as a claim-once transfer that stays single under races between a normal wake, a
   `disable`, a `kill`, and multiple simultaneously invalidated enclosing scopes.
3. The reconciliation's insertion points -- every point where an execution regains control: each
   resumption boundary, each awaited call's return, and a spawned branch's entry -- reading the
   process's enclosing targets with the generation each captured on entry.

One case is not yet reached: a disable whose target is in another module instance or generate scope,
which needs hierarchical addressing to the owning instance's cancellation source; it is a located
diagnostic until then.

## Cross-references

- architecture/activation.md -- the single next-resume entitlement invariant and the forbidden state
  branch.
- activation-registration.md -- one record owned by the activation, the target merely links it; a
  cancellation source is another such target.
- activation-disposition.md -- the authoritative disposition and the uniform, construct-neutral
  re-establish capability the gate reuses.
- architecture/scheduling.md -- the engine branches only on queue and region; the gate lives in
  generated code, not the engine.
- architecture/identity_and_ownership.md -- the id-plus-registry and duplicate-ownership shapes the
  rejected extent registry would have taken.
