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
      call, so it stays mechanical. Complete for every non-managed value domain the backend realizes
      today, aggregates included -- a struct or dynamic-array local crosses as one activation-frame
      value. Contracts and rationale: `../decisions/cross-suspension-value-storage.md`,
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
- [x] **The unpacked struct** (LRM 7.2) -- realized on the execution backend as a product value
      domain: a runtime-owned product that owns its components by value and crosses as an opaque
      handle, so the generated side never inspects a component's representation. It default-
      constructs member-wise, builds from an assignment pattern, copies with value semantics, takes
      the equality and case-equality families, reads and writes a component (including a nested
      product and a string component), lives in a member slot as a whole-cell observable signal
      whose partial write fires subscribers, and crosses a suspension as an activation-frame value.
      A component write is a whole-value rebuild stored back through the value's owner, so an
      observable partial write never bypasses the cell's update semantics -- the aggregate
      partial-update protocol a container reuses later.
- [x] **The dynamic array** (LRM 7.5) -- realized on the execution backend as a run-time-sized
      container value domain, the first variable-size aggregate. It defaults to empty, builds from
      `new[N]` / `new[N](src)` and an assignment pattern, copies with value semantics, takes the
      equality and case-equality families, reports its size, reads and writes an element (an
      out-of-range read yields the element default and an out-of-range write is discarded, LRM 7.4.5
      / 7.4.6), empties under `delete`, lives in a member slot as a whole-cell observable signal
      whose element write fires subscribers, and crosses a suspension as an activation-frame value.
      An element write and `delete` are functional whole-value updates stored back through the owner
      -- never an in-place mutation of a value reached through a possibly-shared handle -- so value
      semantics hold across a copy; this is the mutating-container protocol the queue and
      associative array reuse.
- [ ] **The remaining collection domains** (unpacked array, queue, associative array) -- not
      realized on the execution backend yet. Each is a homogeneous or keyed collection whose element
      count is a runtime quantity; the dynamic array established the erased-container and
      functional-update shape they follow.
- [ ] **A managed value (class handle) across a suspension.** A traceable frame and precise
      reclamation, none of which is implemented: the managed reference is realized as a
      reference-counted handle that does not reclaim cycles, and only in the C++ backend. Contract:
      `../architecture/object_lifetime.md`.
- [ ] **A reference, `output`, or `inout` argument aliasing a procedural local.** Taking the address
      of a suspending body's value local is not yet lowered.

## Value realization: two tracks today, one native model deferred

The value layer is realized two ways, and the breadth work above runs against this split:

- The transitional C++ backend realizes each value type as a monomorphized target type -- the host
  C++ compiler expands one concrete type per element type, and an aggregate interior is written in
  place because that type owns real storage.
- The execution backend realizes each value as an opaque handle into a runtime-owned, type-erased
  object (`../decisions/jit-value-realization.md`, `../decisions/jit-aggregate-realization.md`): it
  emits generated code with no host compiler to expand a template, so an aggregate is one erased
  object and an interior write is a functional whole-value update.

Both are correct and agree per source (the backend-agreement tests check this), but they are two
implementations of the same value semantics. Every value domain added to the execution backend is a
second implementation beside the C++ one, so the two-track maintenance grows as the breadth fills.
This is deliberate, not overlooked: erasure is the uniform, correct baseline chosen so the
value-domain breadth can be filled first, and the C++ backend is transitional.

- [ ] **One native value model (physical value monomorphization).** The convergence that ends the
      two-track split: the execution backend generates specialized native code per concrete type --
      doing the type expansion itself, the way the host C++ compiler does it for the C++ backend --
      so a value's bytes live inline and its operations are native, reproducing the value layer's
      physical layout in generated code (`../decisions/jit-aggregate-realization.md` physical value
      monomorphization; `../decisions/jit-value-realization.md` native in-frame layout, member
      storage included). It is a deferred, value-model-wide endpoint gated behind the value-domain
      breadth being broad, never a per-domain step; once it lands the value model is native on both
      sides and the second implementation is no longer a separate track. A run-time-sized container
      keeps runtime-owned storage regardless -- its element count is a runtime quantity -- so this
      makes the fixed-arity aggregates and the element bytes native, not the container's own
      storage.

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
