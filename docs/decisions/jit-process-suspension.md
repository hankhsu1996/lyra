# Process suspension on the execution backend: a typed protocol, a suspend edge, and LLVM's coroutine machinery

Date: 2026-07-13 Status: accepted

## Context

A SystemVerilog process suspends: `#5`, `@(posedge clk)`, `wait (cond)`, `@e` each park the process
and resume it when the scheduler decides. The C++ backend realizes a process as a C++20 coroutine
and each suspension as a `co_await` over a runtime awaitable, whose `await_suspend` registers the
wakeup source and yields; the engine holds the coroutine's promise base as the activation token
(`scheduling.md`, `activation.md`).

The execution backend lowers a process to LIR and then to LLVM IR run in-process. The questions this
answers are: what carries a body's coroutine-ness through the pipeline, where a suspension appears,
who synthesizes the resumable form (the frame, the resume state, the values that must survive a
suspension), and what the scheduler is allowed to resume.

The value-realization precedent applies: [jit-value-realization](jit-value-realization.md) keeps
runtime values as opaque handles the runtime owns, and treats native in-frame layout as a later
optimization.

## Decision

### D1. Coroutine-ness is the callable's result type, carried unchanged into LIR

A body whose call protocol is the coroutine one says so in its result type (a coroutine type), the
way any call protocol is stated (`callable.md`). MIR-to-LIR carries that type through; it does not
erase it and replace it with a flag. A backend reads the protocol from the type.

### D2. A suspension is a control edge at LIR, not a timing node

Suspension appears as one terminator: it hands control back to the scheduler and names the block the
body resumes at. It schedules nothing and names no wakeup source. The source is registered by the
ordinary runtime calls that precede the terminator, so a delay, an event control, and a level wait
differ only in those calls and share one suspend. This keeps LIR free of a source-language timing
concept (`lir.md`): a suspend is a generic CFG edge, an event control is a runtime call.

### D3. A wakeup registration is one runtime call per construct, and it is token-implicit

Each suspending construct registers its wakeup through one runtime call named for the construct -- a
delay, an event control, a level wait -- not for the engine verb it happens to reach. A delay is a
single registration even though a zero delay enqueues on the inactive region while a positive one
enqueues at a future time; which construct-neutral verb the engine ends up running is the runtime's
business, not a distinction the boundary exposes.

The registration acts on the process the runtime currently has running, so no activation token
appears in LIR or in generated code. The runtime invariant is that the running-process context
identifies this process for the whole time generated code is executing, so a registration call
inside a body reaches the right token.

### D4. The LLVM backend states where a body suspends; LLVM's coroutine passes derive how it resumes

A coroutine body is emitted as an ordinary function carrying LLVM coroutine intrinsics: the ramp
(identity, frame allocation, begin), a suspension at each suspend edge, and a final suspension at
completion. The coroutine passes then derive the frame layout, the resume state machine, the
resume/destroy entries, and which values must survive a suspension.

That derivation is theirs, not the emitter's. The emitter stays a mechanical translation
(`backend_contract.md`): a coroutine result type maps to the coroutine shape, a suspend edge maps to
a suspension, and no entry branches on a body's kind to invent a signature or a state machine.

### D5. The engine's activation token is runtime-owned, and generated code carries no runtime C++ type in its frame

The token the engine schedules and resumes belongs to a runtime adapter coroutine, not to the
generated body. The adapter drives the generated coroutine through its handle: the body runs to its
next suspension, having already registered its wakeup, and the adapter parks until the engine runs
it again.

The reason is not that a code generator cannot produce a coroutine -- with coroutine intrinsics it
produces a real one. It is that the activation token is a C++ promise carrying non-trivial members,
and a generated frame must not have to embed a runtime C++ type's layout, construction, and
destruction to be schedulable. The adapter owns the promise on the generated body's behalf, so the
only thing crossing the boundary is a coroutine's resume, done, and destroy -- the stable surface of
a coroutine handle, not the layout of a runtime class.

This is the boundary, not a stage on the way to removing one. What may be replaced is how the
adapter drives the body; that the scheduler's token is runtime-owned is the invariant.

## Invariants

1. Coroutine-ness is a type. No flag on a function, a terminator, or a node restates it, and no
   backend infers it from the presence of a suspension.

2. A suspend edge registers nothing. Every wakeup source is registered by a runtime call preceding
   the terminator; the terminator is a pure control edge to the resume block.

3. No activation token appears in LIR or in generated code. Registration verbs act on the running
   process, which the runtime identifies for the whole time generated code runs.

4. The emitter does not synthesize a coroutine's frame, resume state, or spills. It emits coroutine
   intrinsics; the coroutine passes derive the resumable form.

5. The engine resumes only runtime-owned activation tokens. A code-generator-produced coroutine
   frame is never the token the engine stores or resumes.

## Rejected

- **A hand-rolled coroutine state machine in the emitter** -- a body given a bespoke signature with
  a resume-state parameter, an entry switch over that state, and hand-written spill and reload of
  the values that cross a suspension. It turns a mechanical backend into a compiler pass, and it
  re-implements, less correctly, what LLVM's coroutine passes already derive. The failure mode is
  concentrated exactly where it is hardest to see: the values live across a suspension.

- **An `is-coroutine` flag on the LIR function or on a return terminator.** Coroutine-ness is the
  result type (invariant 1); a flag beside it is a second source of truth, and two facts that must
  agree drift apart. A flag also appears only because an upstream layer erased the protocol from the
  type, which is the defect to fix.

- **A suspension terminator that names its wakeup** (a delay/event/wait variant). It reintroduces a
  scheduling concept as a LIR node, which `lir.md` forbids: the event control is a runtime call and
  the suspend is a generic edge, not one fused node.

- **The engine resuming a generated coroutine frame directly.** To be the token, a generated frame
  would have to embed the runtime's C++ promise -- its layout, its construction, its destruction --
  so the code generator's frame would depend on a runtime class rather than on a coroutine handle's
  stable surface. The adapter costs one thin frame and removes that dependency (D5).

- **Bundling non-blocking assignment into the suspension model.** A non-blocking assignment does not
  suspend its process; it submits a deferred closure to the NBA region (`scheduling.md`). Realizing
  it needs closure and environment lowering on the execution backend -- a separate foundation.

## Scope and consequences

- In scope: delay (`#N`, `#0`), event control (`@(...)`), level wait (`wait (cond)`), and
  named-event wait (`@e`). They differ only in the registration calls that precede an identical
  suspend edge. A level wait lowers to a re-check loop -- evaluate, continue if true, else register
  and suspend, then re-evaluate on resume -- so invariant 2 holds on each iteration.

- **A value that must outlive a suspension is still open.** The coroutine passes persist a body's
  slots across a suspension, but a value in this backend is an opaque handle into storage the
  runtime owns for the duration of one stretch of generated code. Persisting the slot does not
  persist the object it names. Closing this needs the value's storage to outlive the stretch that
  made it -- the activation-frame question [jit-value-realization](jit-value-realization.md) leaves
  open -- not more coroutine machinery.

- Deferred, each its own foundation: non-blocking assignment (a deferred closure submit); process
  re-arming (`always` / `always_ff`); timed task enables (a nested activation with a typed
  completion, `activation.md`); cancellation and `disable`.

- The two backends share the semantic model -- the same MIR, the same type-carried call protocol --
  and differ only in realization: the C++ backend's process coroutine is itself the token, because
  it is compiled by the same toolchain as the runtime and can hold the promise directly; a generated
  body cannot, so the adapter holds it (D5).

## Cross-references

- `architecture/callable.md` -- the result type is the call protocol; a kind flag beside it is
  forbidden.
- `architecture/lir.md` -- suspension as an explicit CFG edge, no source-language timing node, and
  physical realization below LIR.
- `architecture/backend_contract.md` -- a backend entry is a mechanical function of one node; a
  decision that changes the emitted shape is a design failure upstream.
- `architecture/scheduling.md` -- the engine as a construct-neutral mechanism and the verbs a wakeup
  registration bottoms out on.
- `architecture/activation.md` -- the activation and the payload-neutral token the scheduler holds.
- [jit-value-realization](jit-value-realization.md) -- the opaque-handle baseline, and the
  cross-suspension value lifetime it leaves open.
