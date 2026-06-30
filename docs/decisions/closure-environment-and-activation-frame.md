# Closure environment is a value tuple; activation frame is reference storage

Date: 2026-06-29 Status: accepted as the target model; not yet implemented.

## Why this decision matters

MIR carries several "ordered fields plus a construction" entities through unrelated shapes. The
closure environment is a closure-only world (`CaptureId` / `CaptureRef` / capture-init), the value
aggregates are `TupleType` / `UnionType`, the object model is `mir::Class`, and an escaped automatic
scope is promoted into a baseless `mir::Class` reached through a shared handle. The same idea -- "a
body reads compiler-generated stored state" -- is expressed two ways at once: a closure reads a
`CaptureRef`, while the promotion box is read as `handle->member` (an object member access), and a
detached fork already mixes them (the branch captures the box handle as a closure capture, then
reads its members as object accesses).

This entry settles what the compiler-generated storage actually is, so the two shapes collapse into
one model without crossing the value/reference spine. It is the design round R52
(`../progress/refactor.md`) asked for, now that the binding/capture contract has landed
(`../architecture/binding_and_capture.md`). The contract form is
`../architecture/compiler_generated_storage.md`; this entry records the reasoning and the rejected
alternatives behind it.

## The axis that decides it: value versus reference

MIR's type system keeps one deliberate top-level split (`mir.md`, reinforced by
`unpacked-struct-representation.md` and `unpacked-union-representation.md`): value types (the tuple
product, copied on assignment, interned by shape) versus reference types (reached through a pointer,
identity-bearing). The first question of the whole design is which side the closure environment is
on, because every other choice follows from it.

A closure value is copied by value: a fork branch is frame-copied, a deferred effect is an
independent value submitted to a region, a with-clause closure is used in place. When a closure is
copied, its environment is copied with it. So the closure environment is **value-semantic**, and the
one value product in MIR is `TupleType`. The sharing a detached fork needs (an enclosing automatic
local that the branch and the parent both mutate) is not the environment being shared -- it is a
single environment **field** holding a shared handle to the storage's owner. Sharing lives in a
field, not in the environment.

The storage that owns automatic locals -- the **activation frame** -- is the opposite. It has
identity, it is reached through a pointer, and a detached branch and its parent reach the same
frame. It is reference storage. A frame and a closure environment therefore sit on opposite sides of
the spine and are never the same shape.

## The decisions

```text
D1. The closure environment is a value TupleType. Captured state lives in its slots; the
    closure value is callable code plus that environment. The closure-only capture-storage IR
    (CaptureId / CaptureRef / capture-init) disappears -- a captured read is an ordinary
    value-aggregate slot projection.

D2. TupleType remains the only heterogeneous value-product type. The closure environment reuses
    it; no second value product is minted.

D3. A callable value has two type levels: a concrete closure value carrying its exact
    environment, and an erased Callable<Sig> reached through an explicit erasure. The type model
    is designed whole; implementation may follow real consumers.

D4. The activation frame is a thin, identity-bearing reference-storage entity. It is not a
    mir::Class and not a value tuple.

D5. The activation frame and the nominal object (Class) share one reference-storage substrate
    (identity-bearing slots, slot types, construction / destruction). They are not two unrelated
    object IR systems; the frame omits the object's methods / inheritance / dispatch / nominal
    identity, the object adds them.

D6. "Activation" remains the runtime execution instance (activation.md). The storage owner of
    automatic locals is the activation frame; an execution instance owns or refers to a frame.

D7. A capture is exactly one of: a snapshot (a value-typed field), a live-place alias (a RefType
    field), or a retained frame (a Shared<ActivationFrame> field plus a slot projection). The
    form is the field's type.

D8. HIR-to-MIR owns the SystemVerilog capture / lifetime policy that decides which form each
    construct and binding requires; MIR expresses only the resulting storage facts.
```

### Why the activation frame is not a `mir::Class`, yet not a second object IR (D4, D5)

The promotion box is a `mir::Class` today, which gives frame storage access to inheritance,
dispatch, lifecycle, and managed-handle machinery it must never use. The fix is not a fully
independent reference-aggregate universe either: that would grow a parallel layout, projection,
ownership, and backend lowering beside the object model's -- trading "the box is a baseless class"
for "the frame is a second object IR," which `object_model.md` invariant 1 forbids. The resolution
is one reference-storage substrate (identity-bearing field storage) with two specializations: the
frame (thin -- shared ownership, no behavior) and the object (rich -- methods, inheritance,
dispatch, lifecycle). This honors invariant 1 (no second object IR) and keeps frame storage from
inheriting object behavior. The substrate need not be a public mega-abstraction; it may be internal
shared layout / projection machinery. What is decided is that the frame and the object speak one
reference-storage language, and that the cross-tuple generalization (a single root over value tuples
and reference storage) is **not** built -- the value/reference spine is the top split.

### Why the binding's identity is the origin, not the tuple index (D1)

Reusing `TupleType` makes a captured read a positional `tuple_get(env, index)`. Left there, "which
binding was captured" degrades into "which slot it happens to occupy," and forwarding, dedup, stable
dumps, coroutine spill, and diagnostic provenance all become fragile. The resolution matches how an
unpacked struct already drops field names to positional access
(`unpacked-struct-representation.md`): the index is the representation, but it is assigned as a
deterministic function of the captured binding's origin identity (`binding_and_capture.md` already
requires origins to be deterministic and dump-stable). The closure-only `CaptureId` is not
preserved; it generalizes into an aggregate slot identity backed by the origin, materialized as a
tuple index.

### Why `RefType` is not a plain borrow (D7)

The live-place alias capture is a `RefType` field, and `RefType` is the observable-cell reference
(`reference-as-data-type.md`): a write through it fires the cell's update event. Calling it a
"borrow" invites a future reader to treat it as a transient raw pointer and bypass the update path.
Owner retention (`Shared<ActivationFrame>`) and place aliasing (`RefType`) are two different
dimensions, not two spellings of one: the first says "I keep the storage's owner alive," the second
says "I alias a cell." A live-place alias is legal only when the aliased storage is independently
known to outlive the closure; an automatic local of a frame that may finish first is captured by
retaining the frame, not by aliasing the cell.

### Why two callable type levels with an explicit erasure (D3)

A closure used at its construction site is concrete -- the call site knows its exact environment. A
region's deferred-effect queue holds many closures of one signature and differing environments, so
it needs an erased `Callable<Sig>`. Both levels are real: with only `Callable<Sig>` the model erases
too early and loses the concrete environment; with only the concrete value it cannot express the
heterogeneous queue. The erasure is an explicit MIR transition, never implicit, so ownership,
destruction, and placement do not become hidden behavior; the erased `Callable<Sig>` carries a
complete contract (invoke, ownership / destruction, copy / move) even before a backend optimizes its
placement. This is the first-class callable type `unified-callable-model.md` already committed to;
today the closure's MIR type is its call result type, a shorthand that survives only because every
closure is consumed at its construction site.

### Why deterministic shared ownership for frames, not GC (D4, D6)

A retained frame is reclaimed by deterministic shared ownership (`PointerType{kShared}`, an acyclic
DAG by construction, `lifetime-extended-automatic-scope.md`), released when the last holder drops
it. A SystemVerilog class handle is the managed, precisely-traced reference (`object-model.md`),
because class graphs form cycles. The two reference regimes stay distinct: a frame is never reached
through the managed handle and a class handle is never reached through the shared one. Garbage
collection is the wrong tool for frames -- it gives imprecise release timing, drags in root tracing
and scheduler queue scanning, and a GC-object environment would break the value-copy semantics of a
closure (two closure copies would share one environment object). Shared ownership gives precise,
deterministic release and composes with the value closure.

## Rejected alternatives

- **The closure environment is a `mir::Class` (or any nominal object).** Gives the environment
  inheritance, dispatch, lifecycle, and managed-handle semantics it must not have, and puts a
  value-semantic entity into the reference universe -- the same error
  `unpacked-struct-representation.md` rejected for unpacked struct. A closure value is copied by
  value; its environment is value storage.

- **A second value-product type for closure environments.** Two near-identical heterogeneous
  products violate `mir.md`'s one-product identity, exactly as a distinct `StructType` was rejected.
  The environment reuses `TupleType`.

- **The closure keeps its own `CaptureId` / `CaptureRef` / capture-init universe.** Keeps a
  closure-only storage world in MIR forever; scope activation, deferred work, and ordinary aggregate
  access drift into parallel, not-quite-consistent shapes. The environment joins the value-aggregate
  family instead.

- **The tuple index is the captured binding's identity.** Degrades a semantic identity into a
  positional accident; forwarding, dedup, dumps, spill, and diagnostics become fragile. The origin
  is the identity; the index is its deterministic materialization.

- **The activation frame stays a `mir::Class`.** Frame storage inherits object behavior it must
  never use. The frame is the thin reference-storage specialization.

- **The activation frame is a fully independent reference-aggregate IR.** Trades one awkwardness for
  another -- a second object IR with parallel layout, projection, ownership, and lowering, which
  `object_model.md` invariant 1 forbids. The frame shares the object's reference-storage substrate.

- **`RefType` treated as a borrowed pointer for alias captures.** Loses the observable-cell update
  protocol; a write through it would not wake sensitive processes. `RefType` is the reference type,
  not a raw pointer.

- **Per-variable boxing of an escaped automatic local.** Splits one logical cell across the frame
  slot and a box; spreads the storage decision across every read, write, and projection. The owning
  frame is retained as a whole (`lifetime-extended-automatic-scope.md`).

- **Garbage-collecting activation frames, or reusing the managed handle for them.** Imprecise
  release, root-tracing cost, broken value-closure copy semantics, and conflation with the SV
  class-handle regime. Frames use deterministic shared ownership.

- **Only `Callable<Sig>`, or implicit erasure.** Erases the concrete environment too early or hides
  ownership and placement behind implicit conversion. Two explicit type levels with an explicit
  erasure.

## Consequences

- `../architecture/compiler_generated_storage.md` states this model as contract.
- The closure-only capture IR (`CaptureId` / `CaptureRef` / capture-init) is removed; the closure
  environment is a `TupleType`, built by a tuple construction and read by tuple-slot projection,
  with slots assigned deterministically from binding origins.
- MIR gains a first-class callable value type with two levels (concrete and erased `Callable<Sig>`)
  and an explicit erasure operation; the closure's MIR type stops being its call result type. This
  realizes the first-class callable type `unified-callable-model.md` committed to.
- The promotion box stops being a `mir::Class`; the activation frame becomes the thin
  reference-storage specialization, and `mir::Class` and the frame are refactored onto one
  reference-storage substrate.
- The binding/capture contract (`binding_and_capture.md`) keeps its origin identity and forwarding;
  only the materialized capture-layout representation changes from a closure-only id space to value
  tuple slots.
- HIR-to-MIR keeps and generalizes the capture / lifetime policy (the current `CapturePolicy`
  handles fork; it extends to deferred scheduling, coroutine suspension, `ref` formals,
  observable-cell identity, and storage class).
- Staged implementation and the concrete cuts are tracked in `../progress/refactor.md` (R52).

## Cross-references

- `../architecture/compiler_generated_storage.md` -- the contract form of this decision.
- `../architecture/mir.md` -- `TupleType` as the one value product; the value/reference split; the
  no-redundant-field and no-re-derivation invariants.
- `../architecture/callable.md` -- the callable concept; the capture-by-field-type rule; the
  receiver.
- `../architecture/binding_and_capture.md` -- origin identity, per-body materialization, and capture
  forwarding, which this decision keeps; only the materialized layout representation changes.
- `../architecture/object_model.md` -- invariant 1 (no second object IR), which the shared
  reference-storage substrate honors.
- `../architecture/activation.md` -- the runtime execution instance; the activation frame is the
  storage it owns, not the instance.
- `unified-callable-model.md` -- the first-class callable type this decision's D3 realizes.
- `unpacked-struct-representation.md`, `unpacked-union-representation.md` -- the one-value-product
  invariant and positional access this decision reuses.
- `reference-as-data-type.md` -- `RefType` as the observable-cell reference, not a borrowed pointer.
- `lifetime-extended-automatic-scope.md` -- per-scope retention through a shared handle, which this
  decision reframes as retaining the activation frame.
- `object-model.md` -- the managed (traced) class-handle reference, distinct from the shared frame
  reference.
