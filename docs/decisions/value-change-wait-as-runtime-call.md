# A value-change wait is a runtime call, not a MIR node

Date: 2026-07-14 Status: accepted

## Context

[event-control-unification](event-control-unification.md) collapsed the four SV constructs that wait
on a signal -- an `always_comb` / `always_latch` body, an `@*`, an `@(...)`, a `wait (cond)`, and
the continuous-assignment body that shares their machinery -- into one shape: subscribe to a leaf
set of `(observable cell, bit projection)`, suspend, wake when a change to one of them is an event
for the wait. That unification stands.

It carried the shape in a dedicated MIR statement holding the leaf list. That carrier is what this
decision replaces.

The C++ backend could render the statement because it is free to write whatever text it likes: it
fabricated the runtime's name, the trigger brace-initializers, and the `co_await`, all from one
opaque node. An execution backend cannot fabricate, and the question forced the issue: to lower the
statement it would have to synthesize the wait's realization -- the subscription call and the
suspension -- out of a node that states neither. Two backends would each be inventing that
expansion, free to invent it differently.

Every other suspending construct had already moved to the generic shape. A delay is an awaited call
to the runtime's delay entry; a named-event wait is an awaited call to the event's await. Only the
value-change wait was still a node.

## Decision

A value-change wait is an ordinary runtime call, awaited like every other suspending call:

```text
await( wait_entry(services, [trigger(leaf) for leaf in leaves]) )
```

A **trigger** is one leaf of the wait: the observable cell it watches, the bit projection of that
cell's packed encoding it reads, and what decides whether a change there is an event. It is a
runtime-library value the IR constructs and forwards without inspecting -- the same category as a
print item or a format specification, built by a construct call and passed as an array. MIR carries
no node kind for an event control, and none for a trigger; the existing call, construct,
array-literal, and await primitives express the whole thing.

Every construct that waits on a signal produces this identical call, so a backend that can translate
one translates all of them, and adding a value-change construct adds no backend code. Reaching the
execution backend needed no new IR concept below MIR: the wait is a call, the suspension is the
suspend edge every await already lowers to.

The edge specifier is one enum shared by the compiler and the runtime, not a per-layer copy with
conversions between them. It is the compile-time/runtime agreement about what an event control was
written with, like the standard file descriptors or the DPI ABI classes.

### The whole trigger set goes in one call

The registration takes the set, not one call per leaf, because what waits is one execution and the
leaves are what it waits on: a call per leaf would leave a wait half-made between them, with no
moment at which the execution is waiting for the whole event control.

### The engine handle is an argument, and both backends read it the same way

The call carries the engine handle as its first argument, the way every runtime effect does, and it
is what the call reads to find the frame to resume.

**This section used to record an asymmetry, and the asymmetry is gone.** It said the C++ realization
did not consult the handle, because the language handed the frame to the awaitable that suspended --
and that a registration running as a statement before the suspension could not work, since asking
the runtime which process is running answers with the process's own frame and a wait inside an
enabled task must resume the task's frame. The second half was true of a runtime that did not track
which frame carries the thread. It does now, on both backends, because a called task takes the
thread over and gives it back ([waiting-is-an-operation](waiting-is-an-operation.md) D6) -- so the
running frame is a fact the runtime holds, the registration is an ordinary call that reads it, and
neither backend is handed a frame.

## Consequences

- The execution backend runs every value-change construct: `@(...)` with edges and event lists,
  `@*`, `always_comb` / `always_latch`, `wait (cond)`, and continuous assignment. An `always`
  re-arms through its forever loop with no scheduler involvement.
- The C++ backend's bespoke renderer for the wait is gone; the wait renders through the same generic
  call, construct, and array-literal paths as every other runtime effect. The generated text is more
  verbose (an edge is a packed literal, not a named enumerator), which is the standing trade of the
  uniform value model and a debug concern, not a semantic one.
- The trigger set the caller composes lives only for the duration of the registration call. Each
  leaf's projection is copied into the cell's subscriber record, and what an event control watches
  through is held there too, so nothing points back into the caller's set once the call returns.
  What does outlive the call is the execution's own copy of the leaves, which is how starting a
  stopped process waits for the same thing again (LRM 9.7).
- An empty trigger set is legal and means "never wake up" (`always_comb c = 7;`): the body runs
  once, then the process suspends forever. It is the zero case of the same loop, not a special form.

## Rejected

- **A dedicated MIR statement carrying the leaf set.** The carrier this replaces. It is a node kind
  invented to model a scheduling discipline, so every backend has to synthesize the wait's
  realization out of one opaque node -- which is a decision in value emission, and two backends are
  free to decide differently. A wait is a call against the runtime library's API, and the existing
  call vocabulary carries it with nothing added.

- **One registration call per leaf, then a bare suspension.** It reads as the more primitive shape.
  What rejects it is that an execution waits for one event control and its leaves are what that
  control watches: between two per-leaf calls the execution is enrolled on some of them and waiting
  for none, which is a state the language has no name for and which a process stopped there would
  restart into.

  This was originally rejected for a different reason -- that a call running before the suspension
  cannot be handed the awaiting frame, and asking the runtime which process is running answers with
  the wrong frame for a wait inside an enabled task. That reason no longer holds: the runtime tracks
  which frame carries the thread ([waiting-is-an-operation](waiting-is-an-operation.md) D6), so a
  call before the suspension can name the right frame. The rejection stands on the first reason.

- **A subscription verb on the engine.** Symmetric with the wake verb, and it would give the C++
  realization a use for the engine handle it otherwise ignores. Rejected because the subscription
  touches no engine state: the waiter lists live on the cells and the pending set lives on the
  frame. A verb that needs nothing from the engine is not an engine verb, and adding one to give an
  argument something to do is the argument wagging the design.

- **Dropping the engine handle from the call.** Neither realization can then name the process to
  wake, and having a backend fabricate the handle at the call site is the injection
  [runtime-effects-as-generic-calls](runtime-effects-as-generic-calls.md) rejects. This was
  originally argued against a narrower charge -- that the handle was an unused argument on the C++
  side -- which no longer applies to either side.

## Cross-references

- [event-control-unification](event-control-unification.md) -- the unification this refines: one
  shape for every value-change wait, over a per-leaf projection set.
- [runtime-effects-as-generic-calls](runtime-effects-as-generic-calls.md) -- a runtime effect is an
  ordinary call whose first argument is the engine handle; a backend never injects it.
- [jit-process-suspension](jit-process-suspension.md) -- the suspend edge every await lowers to, and
  why the engine resumes a runtime-owned coroutine rather than a generated frame.
- `architecture/mir.md` -- a node kind invented for a runtime library's shape or a scheduling
  discipline is forbidden; the falsifier is whether a mechanical backend can translate the node
  without decisions.
- `architecture/lir.md` -- an event control reaching LIR is an upstream leak, never a LIR node.
