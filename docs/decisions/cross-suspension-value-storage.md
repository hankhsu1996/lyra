# Cross-suspension value storage: an activation value cell

Date: 2026-07-15 Status: accepted

Since 2026-09-24 a value the generated side makes lives in its own frame rather than in a
per-stretch arena ([a-value-lives-in-its-makers-frame](a-value-lives-in-its-makers-frame.md)), so
the context below describes the store this cell was an answer to, not one that exists. The cell
itself -- one per value-typed local of a suspending body, owned by the execution -- is unchanged,
and a load from it builds its copy in the reader's frame.

## Context

On the execution backend a runtime value is an opaque handle into runtime-owned storage
([jit-value-realization](jit-value-realization.md)); the handle a generated frame holds points into
a `GeneratedCallScope` arena that lives for one stretch of generated code and is released when that
stretch returns. A suspending body runs as several stretches -- the ramp, then each resume -- so its
per-stretch arena is torn down at every suspension.

The LLVM coroutine passes persist a body's slots across a suspension, but a persisted slot holds a
_handle_, and the object the handle names lived in the stretch's arena, now released. So a value
whose lifetime crosses a suspension -- a loop-carried local a clock generator updates
(`repeat (n) begin #d; clk = ~clk; end`), or a read-only local a body reads after it resumes -- has
its handle survive while the value it names is freed. This is the open cross-suspension lifetime
[jit-process-suspension](jit-process-suspension.md) names: not more coroutine machinery, but storage
that outlives the stretch that produced it.

Two architecture facts frame the fix. `scheduling.md` places a procedural local that lives across a
suspension in the coroutine frame; `lir.md` makes such a local a place -- frame storage -- and
treats the physical realization of that storage as a below-LIR concern. The C++ backend satisfies
both for free: a procedural local is a by-value member of its C++20 coroutine frame. The execution
backend cannot hold the value object in the generated frame -- its value ABI is the opaque handle --
so the frame realization has to put the value in runtime storage whose lifetime is the activation,
reached by a handle the frame holds.

## Decision

**A value-typed, non-managed procedural local in a suspending body is realized as an activation
value cell: a runtime value cell owned by the activation, overwritten in place by stores, reached
through a handle the generated frame holds across suspensions.** The value cell lives in an arena
the driving adapter coroutine owns for the activation's whole life, distinct from the per-stretch
`GeneratedCallScope`. Native in-frame value layout stays the deferred optimization
([jit-value-realization](jit-value-realization.md)), not this correctness path.

- **Coroutine value locals are activation cells, uniformly.** In a suspending body every
  value-typed, non-managed local and parameter is an activation cell, decided from the result type
  at MIR-to-LIR. A suspension is a statement boundary and an expression holds no suspension, so only
  a named local can be live across one; marking locals is therefore sufficient, and no
  cross-suspension liveness analysis is run. Making every such local a cell rather than only those
  that provably cross a suspension is admissible because the set that crosses is the coroutine
  passes' to compute, not the lowering's to predict, and a cell for a local that never crosses is a
  cost, never wrong. A non-suspending body keeps its locals as plain values or places, and a
  pointer, reference, or machine scalar is stable across a suspension on its own and is never made a
  cell.

  **An object handle was listed with those three and does not belong there**, which is worth stating
  because the grouping reads as one property and is two. An address stays an address across a
  suspension; a handle carries a claim on an object's life beside the address, and that claim lives
  wherever the stretch that answered the handle put it. It was excluded here not because it is
  stable but because the traced path below was meant to take it, and while that path did not exist
  the exclusion left a handle's storage owned by a stretch the suspension releases -- invariant 1
  below, unmet rather than inapplicable. A handle is now a variable like any other whose value the
  body does not hold the whole of
  ([a-body-holds-a-value-or-its-execution-stores-it](a-body-holds-a-value-or-its-execution-stores-it.md)).

- **The cell is a non-observable value cell.** A procedural local is not a signal, so a write is
  never an update event and wakes no subscriber. The cell shares the store discipline of a signal
  cell -- install the declared representation once, overwrite at that representation, preserve the
  declared type across writes -- but not its observation. The signal cell (`Var<T>`) and the
  activation value cell (`ActivationValueCell<T>`) are two roles over one storage core
  (`ValueStorageCore<T>`); the cell never routes through the signal cell's notifying `Set`.

- **The activation owns the cell storage; the frame holds a handle.** The adapter coroutine that
  drives the generated body (the activation) owns an arena for the whole activation and is destroyed
  on every path it can leave -- completion, cancellation, shutdown. The generated frame holds only a
  handle into that arena, which the coroutine passes persist across a suspension like any other
  slot; the object the handle names outlives the stretch. The adapter also RAII-owns the generated
  coroutine, so the generated frame and its cells are torn down together.

- **A store overwrites; a load copies out.** A store to the cell overwrites its contents in place --
  the first store installs the declared representation, since a procedural local has no construction
  step separate from its declaration -- so storage is one cell per local, reused across iterations,
  never an arena that accumulates a value per write. A load copies the current value into the
  per-stretch scope, the same transient lifetime any value the boundary hands back has. Copy-on-load
  is the baseline; aliasing the cell is a later optimization, not this decision.

- **The mechanism is gated on non-managed value types.** A managed value that must be traced does
  not live in this cell; that is the separate traceable-frame path (`lifetime.md`), routed by type.
  The cell mechanism reads the type and takes only the non-managed runtime value domains.

  **Superseded 2026-09-17 by
  [a-body-holds-a-value-or-its-execution-stores-it](a-body-holds-a-value-or-its-execution-stores-it.md).**
  This was written where no managed value reached this target, on the reading that when one did it
  would arrive together with the path it was being routed to. It arrived on its own, that path does
  not exist, and the gate was answering a legal program with a memory fault rather than with a
  refusal. What tracing adds is reclamation, which is not what storage outliving a stretch is for: a
  reference needs it for the same reason a loop counter does, so there is one mechanism and the gate
  was a second name for it. What decides a variable's home is whether the body holds the whole of
  its value.

## Invariants

1. A value whose lifetime crosses a suspension does not live in a per-stretch `GeneratedCallScope`.
   Its storage is an activation-owned cell reached through a frame-held handle.

2. In a suspending body, every value-typed non-managed local and parameter that is not lent is an
   activation cell. The choice is decided from the result type at MIR-to-LIR, never from a
   cross-suspension liveness analysis the emitter runs. A lent local is not one: it lives in a cell
   of the body's own frame whether or not the body suspends, because being lent is what decides its
   storage and the declaring scope is what decides its lifetime
   ([reference-binds-a-cell](reference-binds-a-cell.md)).

   **Superseded 2026-09-16 by
   [a-declared-variable-is-one-storage](a-declared-variable-is-one-storage.md).** Two of the things
   this invariant keeps apart are one: a declared variable is one storage whatever its body does
   with it, so neither being lent nor the body suspending selects anything. What holds is the half
   this states first -- the choice, such as it was, came from the declaration rather than from a
   liveness analysis -- and what fell is that there was a choice at all. The two homes were mutually
   exclusive, so a variable that was both lent and in a suspending body had none.

3. An activation value cell is not observable. A write to it is never an update event and wakes no
   subscriber; it never routes through the signal cell's notifying store.

4. The activation owns the cell arena and RAII-owns the generated coroutine. Both are destroyed on
   every path the activation can leave; neither leaks when the activation is released while its body
   is suspended.

5. The cell is an explicit LIR operation, decided at MIR-to-LIR. A value-typed local in a suspending
   body lowers to a cell allocation and cell load / store calls (the `ValueCellTarget` LIR
   operation) -- storage placement, which MIR-to-LIR owns -- so LIR states the storage access as a
   call, and LIR-to-LLVM realizes it mechanically, the way it realizes any call (the value domain
   names the runtime entry). The backend never derives a local's storage from the function's
   coroutine-ness or the local's type. MIR carries no storage-cell node: the activation frame is a
   below-MIR storage realization the C++ backend, which consumes MIR, never sees. The naming of the
   activation frame vs the per-stretch transient scope, and the invariant that a transient may not
   escape its stretch, are settled in
   [activation-frame-and-transient-scope](activation-frame-and-transient-scope.md).

## Rejected

- **Native in-frame value layout as the cross-suspension fix.** Laying a value's bytes in the
  coroutine frame (matching the C++ backend) covers a place and a spilled transient uniformly, but
  it needs the physical-layout derivation `lir.md` defers: a size and an alignment per concrete
  type, which is the monomorphization the value model still owes. So it stays the later
  optimization, not the correctness path.

  The half of this that was thought impossible turned out not to be. Ending a non-trivial value in
  the frame was read as the emitter's to place and out of its reach, on the ground that which slots
  the coroutine passes make frame-resident is their decision. It is not the same question: where the
  bytes end up is theirs, and where the ending runs is the emitter's, since the emitter is the one
  that knows the scopes. A lent local proves it -- its cell is a frame slot the compiler begins at
  the declaration and ends on every way out, the abandon path included
  ([reference-binds-a-cell](reference-binds-a-cell.md)).

  The other half, the size and alignment per concrete type, was never owed either: every value
  domain realizes one runtime type whatever source type it stands for, so a value has one size per
  domain, and temporaries now live in the frame on exactly that basis
  ([a-value-lives-in-its-makers-frame](a-value-lives-in-its-makers-frame.md)). Whether a value
  crossing a suspension should live in the frame too is not reopened here; the cell stands because
  it is the storage a reference binds, not because the frame could not hold the value.

- **A backend-private arena for cross-suspension values, invisible to the storage model.** Storage a
  value crosses a suspension in must be a first-class runtime concept the activation owns, not an
  arena the backend invents below the model. For a non-managed value the traceable-frame discipline
  does not apply, but the storage is still an activation-owned cell reached through the activation
  record, not arbitrary foreign storage.

- **A narrow cross-suspension liveness analysis to place only the values that cross.** The set of
  values live across a suspension is the coroutine passes' to compute; replicating it in the emitter
  reintroduces the hand-rolled spill analysis [jit-process-suspension](jit-process-suspension.md)
  rejects, to save allocating a cell for a value that does not cross. The blunt place rule is
  decidable from the result type and admissible because LIR does not minimize storage.

- **Reusing the signal cell (`Var<T>`) for a procedural local.** Its store fires the update event
  and carries the observable base a local never needs; a procedural write is not an LRM 4.3 update
  event. The two cells share the storage core, not the observation.

## Consequences

- The execution backend runs a suspending body that carries a value across a suspension -- a loop
  counter, a read-only local read after a resume -- so a clock generator written as a loop runs
  without spelling the clock out.
- `Var<T>` is factored into `ValueStorageCore<T>` (storage and store discipline) plus its observable
  behavior; `ActivationValueCell<T>` is the second role over the same core. The signal cell's public
  surface is unchanged.
- The adapter coroutine owns an activation frame and RAII-owns the generated coroutine, closing the
  generated-frame leak on cancellation and shutdown as a side effect of giving the cells a single
  destruction path.
- Managed cross-suspension values were left as future work here, on the reading that they route to a
  traceable frame by type. They do not: a reference lives in the same storage every other value
  does, and what a collector would add is reclamation rather than a second home
  ([a-body-holds-a-value-or-its-execution-stores-it](a-body-holds-a-value-or-its-execution-stores-it.md)).

## Cross-references

- [jit-value-realization](jit-value-realization.md) -- the opaque-handle baseline and the
  `GeneratedCallScope` transient scope this extends for the cross-suspension case.
- [jit-process-suspension](jit-process-suspension.md) -- the suspension protocol that leaves the
  cross-suspension value lifetime open, and the rejection of an emitter-side spill analysis.
- `architecture/scheduling.md` -- a procedural local live across a suspension lives in the coroutine
  frame.
- `architecture/lir.md` -- a place is frame storage; its physical realization is a below-LIR
  concern.
- `architecture/lifetime.md` -- the traceable activation frame for managed values, the separate path
  this mechanism is gated away from by type.
