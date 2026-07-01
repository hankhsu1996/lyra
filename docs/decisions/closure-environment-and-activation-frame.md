# Closure environment is a nominal value record; activation frame is reference storage

Date: 2026-06-29 (revised 2026-06-30). Status: accepted as the target model; not yet implemented.

## Why this decision matters

MIR carries several "ordered fields plus a construction" entities through unrelated shapes. A
closure holds captured state, an escaped automatic scope is promoted into a shared record, the value
aggregates are `TupleType` / `UnionType`, and the object model is `mir::Class`. The same idea -- "a
body reads compiler-generated stored state" -- risks being expressed several ways at once. This
entry settles what a closure's stored state actually is and how it relates to the activation frame
and the nominal object, so the shapes collapse into a small set of deliberate categories rather than
drifting apart.

An earlier form of this decision made the closure environment a value `TupleType`. Implementing it
exposed a structural mismatch: a tuple is **positional** (a capture's identity is its slot index),
but captures are **discovered incrementally while the body is lowered**, so the body must emit a
read before the canonical slot layout is known. That forced provisional slots, a canonical-reorder
pass, and a rebuild of the already-emitted body -- accidental complexity that is not inherent to
closures but is a direct consequence of forcing a positional structure onto late-discovered, named
state.

This revision replaces the tuple with a **per-closure-site nominal value record**. That is also how
every general-purpose language models a closure: C++ makes each lambda a unique unnamed closure
_class_ (capture fields plus a call operator, used as a value); Rust makes each closure a unique
anonymous struct of captured fields plus an `Fn`-family body. MIR is a generic programming language
(`../architecture/mir.md`), so its closures should take the shape its peer languages give them.

## The axis that decides it: value versus reference

MIR's type system keeps one deliberate top-level split (`../architecture/mir.md`): value types
(copied on assignment) versus reference types (reached through a pointer, identity-bearing). A
closure value is copied by value -- a fork branch is frame-copied, a deferred effect is an
independent value, a with-clause closure is used in place -- so when a closure is copied, its
captured state is copied with it. The closure record is therefore **value-semantic**.

"Value-semantic" does _not_ imply "a structural tuple". It implies only: the record is not a shared
object handle, and it carries no reference / object identity. A nominal value record satisfies that
exactly, while giving captures intrinsic named identity instead of positional slots.

The storage that owns automatic locals -- the **activation frame** -- is the opposite. It has
identity, is reached through a pointer, and a detached branch and its parent reach the same frame.
It is reference storage. A closure record and a frame therefore sit on opposite sides of the spine
and are never the same shape.

## The decisions

```text
D1. A closure is a per-closure-site nominal value record type (ClosureRecordType): named capture
    fields plus exactly one invoke body. Value copy / move semantics; no inheritance, no dispatch,
    no heap requirement, no managed / GC handle, no source-visible identity. A captured binding is a
    named field; a captured read is an ordinary field access over the closure receiver, never a
    tuple-slot projection or a closure-only capture-read node.

D2. TupleType remains the only GENERAL heterogeneous value product (a task output pack, an unpacked
    struct). A ClosureRecordType is not a general product: it is a compiler-generated, opaque,
    concrete callable value type. The one-product invariant stands.

D3. A callable value has two type levels: the concrete ClosureRecordType (carrying its exact capture
    fields) and an erased Callable<Sig> reached through an explicit erasure. The concrete record is
    the default representation; erasure is for a heterogeneous collection of callables of one
    signature (a region's deferred-effect queue, an unknown-callback boundary). Erasure is never
    implicit, so ownership and placement never become hidden behavior.

D4. Capture identity, closure-record field identity, and physical field layout are three separate
    layers:
        CaptureKey (a BindingOriginId)  ->  ClosureFieldId  ->  physical field ordinal.
    The invoke body reads a capture by its stable ClosureFieldId; canonical ordering fixes only the
    physical field layout and never rewrites the body.

D5. Capture-initialization evaluation order is independent of physical field layout order. Sources
    evaluate in semantic capture order (a side-effecting source is a sequenced temporary first); the
    record is then constructed, placing each evaluated source into its canonical field.

D6. A closure record is not a mir::Class: no inheritance, no dispatch, no lifecycle, no
    managed-handle semantics. It is a callable value type, not a nominal object.

D7. The activation frame is a thin, identity-bearing reference-storage entity -- not a mir::Class,
    not a value record copied by value. Work that may outlive the owning execution retains the whole
    frame through a Shared<ActivationFrame> handle (deterministic shared ownership, not GC).

D8. The activation frame and the nominal object share one reference-storage substrate. A closure
    record, an activation frame, and a nominal object may additionally share a NARROW field-storage
    vocabulary (field declaration, field id, field access, layout finalization); no universal Record
    IR is introduced. They remain three distinct semantic record categories (see the table below).

D9. HIR-to-MIR owns the SystemVerilog capture / lifetime policy; MIR expresses only the resulting
    storage facts. The receiver of a closure body is the closure record itself; a captured
    source-level self is an ordinary field of the record, distinct from the closure receiver.
```

The four categories, kept distinct:

```text
                  storage     behavior                     identity
TupleType         value       none                         structural / positional
ClosureRecordType value       one invoke body              nominal, per closure site
ActivationFrame   reference   none                         runtime storage identity
mir::Class        reference   methods / dispatch / cycle   nominal object identity
```

### Why a closure is a nominal record, not a structural tuple (D1)

A tuple's field identity _is_ its position, so a body read written before the canonical layout is
fixed must be rewritten once it is. A nominal record's field identity is a `ClosureFieldId` assigned
when the capture is discovered, independent of physical order, so the body reads
`FieldAccess(closure_receiver, ClosureFieldId)` immediately and correctly; the physical layout is
canonicalized later without touching the body. The whole provisional-slot / canonical-reorder /
body-rebuild machinery the tuple approach required disappears. This is not "less code for its own
sake"; it resolves a real representation mismatch (semantic field identity must not equal physical
layout index) that the tuple approach could only paper over.

### Why a closure record is not a general value product (D2)

A closure record has fields, so if it could be freely constructed, destructured, compared
structurally, or substituted for a `TupleType`, it would be a second general heterogeneous product
and would break the one-product invariant. It is not, because it is constrained to a concrete
callable implementation type:

```text
1. Compiler-generated only; no source program declares a closure record.
2. One unique nominal type per closure site; it is not structurally interned like a tuple.
3. Exactly one callable entrypoint; no general method lookup, overload, inheritance, or dispatch.
4. Field access occurs only inside the compiler-generated invoke body and lowering; there is no
   source-level record construction or destructuring over it.
```

Under these four constraints a closure record is the MIR equivalent of a C++ lambda closure object
-- a concrete callable value type, not a new general data product.

### Why concrete and erased are two levels with an explicit erasure (D3)

A closure used at its construction site is concrete -- the site knows its exact record type. A
region's deferred-effect queue holds many closures of one signature and differing records, so it
needs an erased `Callable<Sig>`. Both are real: with only the concrete record a heterogeneous queue
cannot be expressed; with only `Callable<Sig>` the concrete record (and its precise ownership /
placement) is erased too early. The concrete record is the default; erasure is an explicit MIR
transition to `Callable<Sig>` that carries a complete contract (invoke, ownership / destruction,
copy / move). This is the first-class callable type `unified-callable-model.md` committed to, split
into its two levels.

### Why field identity is not the physical layout index (D4)

The invoke body must be emittable during lowering, before every capture is discovered and before the
canonical field layout is fixed. Making the body reference a stable `ClosureFieldId` -- assigned per
capture at discovery, keyed by the binding's `CaptureKey` -- decouples "which capture" (semantic)
from "which physical field" (representation). Canonical layout ordering is still needed, but only
for representation: physical field order, generated-source field order, deterministic dump,
destruction order, and any future ABI / incremental-compilation key. It never touches the body. This
three-layer separation is what lets the body be written once, with no rebuild.

### Why the activation frame is reference storage, not a mir::Class, yet not a second object IR (D7, D8)

An escaped automatic scope needs identity-bearing storage a detached branch and its parent both
reach, and that a longer-lived branch keeps alive. That is reference storage retained by a shared
handle -- deterministic shared ownership (`PointerType{kShared}`, an acyclic DAG by construction,
`lifetime-extended-automatic-scope.md`), not the managed / traced reference an SV class handle uses
(`object-model.md`), which exists for cyclic class graphs. Making the frame a full `mir::Class`
gives it inheritance / dispatch / managed-handle machinery it must never use; making it a fully
independent reference-aggregate IR grows a parallel layout / projection / ownership system that
`object_model.md` invariant 1 forbids. The resolution is one reference-storage substrate with the
frame as the thin specialization (shared ownership, no behavior) and the object as the rich one
(methods, inheritance, dispatch, lifecycle).

### Why a narrow shared field vocabulary, not a universal Record IR (D8)

A closure record, an activation frame, and a nominal object all have ordered fields, so they can
share field-declaration, field-id, field-access, and layout-finalization vocabulary. They must not
be fused into one `Record<Storage, Behavior>` type or a `mir::Class` with `is_closure` / `is_frame`
flags: that recreates the tag-soup and universal-abstraction failure the reset removes, and it
crosses the value/reference spine (a closure is value; a frame and an object are reference). The
table above is an architecture map for checking semantics, not a Cartesian product to generate a
type hierarchy from.

## Rejected alternatives

- **The closure environment is a structural value `TupleType`.** A tuple is positional, so a
  capture's identity is its slot index; but captures are discovered while the body is lowered, so
  the canonical layout is unknown when the body's reads are emitted. This forces provisional slots,
  a canonical-reorder pass, and a rebuild of the already-emitted body. The mismatch is structural
  (positional access versus late, named discovery), not an implementation accident. A nominal record
  gives captures intrinsic named identity and removes the whole rebuild machinery.

- **The closure environment (or the closure) is a `mir::Class`.** Gives it inheritance, dispatch,
  lifecycle, and managed-handle semantics it must not have, and puts a value-semantic entity into
  the reference universe. A closure record is value-semantic with exactly one entrypoint and no
  object behavior.

- **A second general value-product type for closure environments.** Two near-identical general
  heterogeneous products violate `mir.md`'s one-product identity. A closure record avoids this by
  being an opaque concrete callable type under the four constraints, not a general product.

- **The closure keeps its own `CaptureId` / `CaptureRef` / capture-init universe.** Keeps a
  closure-only storage world in MIR forever. The captured state joins the nominal-record family
  instead, reading through ordinary field access.

- **Field identity is the physical layout index.** Degrades a semantic identity into a positional
  accident and reintroduces the body-rebuild problem. Identity is the `ClosureFieldId` (keyed by the
  origin); the physical ordinal is its representation.

- **The activation frame stays a `mir::Class`, or is a fully independent reference-aggregate IR.**
  The first inherits object behavior it must never use; the second grows a second object IR
  (`object_model.md` invariant 1). The frame is the thin reference-storage specialization of one
  substrate.

- **Merging the closure record with the coroutine / activation frame.** A closure record is a
  callable value's captured environment; a frame is an execution activation's persistent storage.
  They may retain or own one another, but they are different types on opposite sides of the spine
  (Rust keeps the closure and the coroutine it returns as distinct anonymous types for the same
  reason).

- **Only `Callable<Sig>`, or implicit erasure.** Erases the concrete record too early or hides
  ownership and placement behind implicit conversion. Two explicit type levels with an explicit
  erasure.

## Consequences

- The closure-only capture IR (`CaptureId` / `CaptureRef` / capture-init) and the value-tuple
  environment are both removed. A closure is a `ClosureRecordType` with named capture fields and one
  invoke body; a captured read is `FieldAccess(closure_receiver, ClosureFieldId)`.
- MIR gains a concrete callable value type (`ClosureRecordType`, per closure site) that is directly
  callable: a call resolves its signature from the concrete record's invoke, so no universal
  callable type is needed to make a closure callable. The erased `Callable<Sig>`
  (`unified-callable-model.md`) and its explicit erasure remain the target for a heterogeneous
  callable collection, but are introduced together with that erasure operation and a real consumer,
  not before one exists.
- Capture identity (`CaptureKey`), closure-record field identity (`ClosureFieldId`), and physical
  field layout are separate; canonical ordering affects only layout, never the body.
- A closure record, an activation frame, and `mir::Class` share a narrow field-storage vocabulary;
  no universal Record IR is introduced. The promotion box stops being a baseless `mir::Class` and
  becomes the thin reference-storage frame.
- The binding/capture contract (`binding_and_capture.md`) keeps its origin identity and forwarding;
  only the materialized capture representation changes from a tuple slot to a closure-record field.
- This supersedes the earlier "closure environment is a value `TupleType`" form of this decision
  (the intermediate `R52b` cut). The concrete / erased split and the activation-frame decisions are
  unchanged as the target. The signature-only callable type built by an earlier cut is removed here
  as dead surface -- it had a producer but no consumer; the erased level returns with its erasure
  operation when a heterogeneous consumer requires it.
- Staged implementation and the concrete cuts are tracked in `../progress/refactor.md` (R52).

## Cross-references

- `../architecture/compiler_generated_storage.md` -- the contract form of this decision.
- `../architecture/mir.md` -- the value/reference split; `TupleType` as the one general value
  product; the generic-programming-language identity and the "grow a primitive only for a genuine
  generic-language concept" rule.
- `../architecture/callable.md` -- the callable concept; the capture-by-field-type rule; the
  receiver.
- `../architecture/binding_and_capture.md` -- origin identity, per-body materialization, and capture
  forwarding, which this decision keeps; only the materialized representation changes.
- `../architecture/object_model.md` -- invariant 1 (no second object IR), which the shared
  reference-storage substrate honors; the nominal object the closure record is deliberately not.
- `../architecture/activation.md` -- the runtime execution instance; the activation frame is the
  storage it owns.
- `unified-callable-model.md` -- the first-class callable type this decision's D3 realizes.
- `reference-as-data-type.md` -- `RefType` as the observable-cell reference (an alias-capture
  field), not a borrowed pointer.
- `lifetime-extended-automatic-scope.md` -- per-scope retention through a shared handle.
- `object-model.md` -- the managed (traced) class-handle reference, distinct from the shared frame
  reference.
