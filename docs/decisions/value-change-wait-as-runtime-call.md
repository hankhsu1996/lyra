# A value-change wait is a runtime call, not a MIR node

Date: 2026-07-14 Status: accepted

## Context

[event-control-unification](event-control-unification.md) collapsed the four SV constructs that wait
on a signal -- an `always_comb` / `always_latch` body, an `@*`, an `@(...)`, a `wait (cond)`, and
the continuous-assignment body that shares their machinery -- into one shape: subscribe to a leaf
set of `(observable cell, bit projection, edge polarity)`, suspend, wake when a relevant leaf
changes. That unification stands.

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
cell's packed encoding it watches, and the edge polarity it watches for. It is a runtime-library
value the IR constructs and forwards without inspecting -- the same category as a print item or a
format specification, built by a construct call and passed as an array. MIR carries no node kind for
an event control, and none for a trigger; the existing call, construct, array-literal, and await
primitives express the whole thing.

Every construct that waits on a signal produces this identical call, so a backend that can translate
one translates all of them, and adding a value-change construct adds no backend code. Reaching the
execution backend needed no new IR concept below MIR: the wait is a call, the suspension is the
suspend edge every await already lowers to.

The edge polarity is one enum shared by the compiler and the runtime, not a per-layer copy with
conversions between them. It is the compile-time/runtime agreement about what a leaf's polarity
means, like the standard file descriptors or the DPI ABI classes.

### The whole trigger set goes in one call

The registration takes the set, not one call per leaf, because the registration must name the frame
to resume and only the suspension protocol knows it. A C++ coroutine is handed its own frame when it
suspends; a wait inside an enabled task must resume the task's frame, not the enabling process's,
and nothing outside the awaiting frame can tell which. A registration that ran as a statement before
the suspension would have to ask the runtime which process is running, and the answer -- the
process's engine-visible frame -- is the wrong frame for a nested wait.

### The engine handle is an argument, and the two backends read it differently

The call carries the engine handle as its first argument, the way every runtime effect does. The C++
realization does not consult it: the language hands the awaitable its frame. The execution backend's
realization needs it, because the frame it suspends is not a frame the engine ever sees -- the
engine resumes the runtime-owned coroutine that drives it
([jit-process-suspension](jit-process-suspension.md) D5) -- so the process to wake is the running
one, which only the runtime can name.

This is the same asymmetry the delay already has, and it is a property of the two execution models,
not a defect in the call: one backend gets the frame from the language, the other asks the runtime
for it.

## Consequences

- The execution backend runs every value-change construct: `@(...)` with edges and event lists,
  `@*`, `always_comb` / `always_latch`, `wait (cond)`, and continuous assignment. An `always`
  re-arms through its forever loop with no scheduler involvement.
- The C++ backend's bespoke renderer for the wait is gone; the wait renders through the same generic
  call, construct, and array-literal paths as every other runtime effect. The generated text is more
  verbose (an edge is a packed literal, not a named enumerator), which is the standing trade of the
  uniform value model and a debug concern, not a semantic one.
- The trigger set lives only for the duration of the registration call. The runtime copies each
  leaf's projection into the cell's subscriber record, so nothing points back into the set once the
  call returns -- which is why a wait needs no value to survive its own suspension.
- An empty trigger set is legal and means "never wake up" (`always_comb c = 7;`): the body runs
  once, then the process suspends forever. It is the zero case of the same loop, not a special form.

## Rejected

- **A dedicated MIR statement carrying the leaf set.** The carrier this replaces. It is a node kind
  invented to model a scheduling discipline, so every backend has to synthesize the wait's
  realization out of one opaque node -- which is a decision in value emission, and two backends are
  free to decide differently. A wait is a call against the runtime library's API, and the existing
  call vocabulary carries it with nothing added.

- **One registration call per leaf, then a bare suspension.** It reads as the more primitive shape
  and it matches how the execution backend's registration works. But a per-leaf call runs before the
  suspension, so it cannot be handed the awaiting frame and must ask the runtime which process is
  running -- and that answer is wrong for a wait inside an enabled task, whose task frame is the one
  the engine must resume. Passing the whole set to one call keeps the registration inside the
  suspension protocol, where the frame is known.

- **A subscription verb on the engine.** Symmetric with the wake verb, and it would give the C++
  realization a use for the engine handle it otherwise ignores. Rejected because the subscription
  touches no engine state: the waiter lists live on the cells and the pending set lives on the
  frame. A verb that needs nothing from the engine is not an engine verb, and adding one to give an
  argument something to do is the argument wagging the design.

- **Dropping the engine handle from the call so the C++ realization has no unused argument.** The
  execution backend cannot then name the process to wake, and having its backend fabricate the
  handle at the call site is the injection
  [runtime-effects-as-generic-calls](runtime-effects-as-generic-calls.md) rejects. The handle is a
  real input to the wait; that one realization can answer the question without it is a property of
  C++ coroutines, not evidence the input is spurious.

## Cross-references

- [event-control-unification](event-control-unification.md) -- the unification this refines: one
  shape for every value-change wait, per-leaf projection and edge, frontend LSB-reduce, runtime
  per-leaf filtering.
- [runtime-effects-as-generic-calls](runtime-effects-as-generic-calls.md) -- a runtime effect is an
  ordinary call whose first argument is the engine handle; a backend never injects it.
- [jit-process-suspension](jit-process-suspension.md) -- the suspend edge every await lowers to, and
  why the engine resumes a runtime-owned coroutine rather than a generated frame.
- `architecture/mir.md` -- a node kind invented for a runtime library's shape or a scheduling
  discipline is forbidden; the falsifier is whether a mechanical backend can translate the node
  without decisions.
- `architecture/lir.md` -- an event control reaching LIR is an upstream leak, never a LIR node.
