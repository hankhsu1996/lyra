# Execution backend

Tracks the MIR / LIR -> LLVM execution backend's own realization -- the parts that are the
backend's, not a single SystemVerilog feature. Per-feature backend status lives in the feature files
(a DPI scalar in `dpi.md`, a timing control in `processes.md`); this file owns the backend
infrastructure: how a runtime value lives, what the backend can and cannot lower yet, and its
coverage. The roll-up entry is the "Execution backend" item in `architecture-reset.md`.

Contracts: `../architecture/backend_contract.md`, `../architecture/lir.md`,
`../architecture/runtime_distribution.md`, `../architecture/object_lifetime.md`.

## Runtime-value lifetime

A runtime value crosses the execution boundary as an opaque handle into runtime-owned storage
(`../decisions/jit-value-realization.md`); a transient lives in a per-stretch scope and is released
when that stretch returns. A value whose lifetime crosses a suspension needs storage that outlives
the stretch that made it.

- [x] **A value that crosses a suspension survives, for the value domains realized today.** A
      value-typed non-managed procedural local in a suspending body -- a loop counter, a read-only
      local read after it resumes, a local mutated across nested control flow -- lives in the
      activation frame, storage the running activation owns for its whole life and reaches through a
      handle the generated frame holds. A store copies into it; a load copies out; the store
      decision is made where storage placement belongs and the backend realizes it as an ordinary
      call, so it stays mechanical. Complete for the value domains the backend realizes today
      (packed, string, and the real family -- real, shortreal, realtime). Contracts and rationale:
      `../decisions/cross-suspension-value-storage.md`,
      `../decisions/activation-frame-and-transient-scope.md`.

- [x] **The storage lifetimes are named and separated.** The per-stretch transient scope, the
      activation frame, the activation's control identity, and the process lineage node are distinct
      names for distinct concepts; a per-stretch transient may not escape its stretch, and every
      store into longer-lived storage copies or promotes (the one non-copying path, a method return,
      stays in the caller's scope). Settled in
      `../decisions/activation-frame-and-transient-scope.md`.

The rest are further values that outlive the stretch that made them, none on the execution backend
yet. Each is another instance of the same lifetime question, so the first to land decides whether it
extends the activation frame or the backend adopts one lifetime discipline (a traced heap,
ownership, or native in-frame layout) for every value.

- [x] **The scalar real family** (real, shortreal, realtime) -- realized on the execution backend as
      a value domain alongside packed and string: arithmetic, comparison, the integer/real
      conversions, and real formatting run, a real signal member is an observable cell, and a real
      procedural local crosses a suspension as an activation-frame value. It extended the activation
      frame with another domain rather than forcing a new lifetime discipline, since a real is a
      non-managed value like a packed one.
- [x] **The chandle** (LRM 6.14) -- realized on the execution backend as a pointer-like value
      domain: the value is the pointer itself, carried inline rather than behind a handle to a
      runtime-owned object. A chandle defaults to null, assigns from null and from another chandle,
      takes the equality and case-equality families and the boolean test, and lives in a member slot
      as an owned inline value (not an observable cell, since no process subscribes to it). This is
      the first bare-pointer value domain; a class handle later reuses the shape.
- [ ] **The remaining value domains** (aggregates, containers) -- not realized on the execution
      backend at all yet, so no cross-suspension case exists for them.
- [ ] **A managed value (class handle) across a suspension.** A traceable frame and precise
      reclamation, none of which is implemented: the managed reference is realized as a
      reference-counted handle that does not reclaim cycles, and only in the C++ backend. Contract:
      `../architecture/object_lifetime.md`.
- [ ] **A reference, `output`, or `inout` argument aliasing a procedural local.** Taking the address
      of a suspending body's value local is not yet lowered.
- [ ] **Native in-frame layout for value members** -- the baseline keeps runtime-owned cells; a
      value's bytes in the generated frame is the deferred optimization
      (`../decisions/jit-value-realization.md`).

## Deferred effects and concurrency

Each defers or captures a value that outlives the current stretch, so each meets the same lifetime
question above; none is lowerable on the execution backend yet.

- [ ] Non-blocking assignment (a deferred closure submit). Rolled up in `processes.md` (P4).
- [ ] Fork / join and closures. Rolled up in `fork-join.md`.
- [ ] Named events across a suspension. Rolled up in `processes.md` (P9).

## Other backend surfaces

- [ ] By-pointer DPI-C marshaling (the by-value scalar surface runs; see `dpi.md`).
- [ ] The transient-escape rule is held by construction and naming, not by a checker.
- [ ] End-to-end coverage is a handful of backend-agreement tests (execution backend vs C++ backend,
      compared per source), not the full test corpus, which targets the C++ backend. There is no
      mechanism to run a corpus case against the execution backend.
