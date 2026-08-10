# A value-aggregate interior write is an owner-relative value projection, not a place store

## Date

2026-07-23

## Status

Accepted

## Why this decision matters

A write to the interior of a value aggregate -- a struct component, a container element, a packed
slice, a union member, a string character -- reaches MIR as an assignment whose target is an lvalue
expression: a write-through reference the write composes onto. That is the reference / location
model. The functional whole-value-update model the layer contracts describe (`mir.md`, `lir.md`)
does not live in MIR; it lives one layer below, synthesized during MIR-to-LIR by the execution
backend, and only for one container type.

The consequence is that one MIR node is consumed two different ways. The C++ backend renders an
in-place lvalue mutation; the execution backend rewrites the target back into a read-whole /
functional-update / write-whole sequence, because its value realization forbids an in-place interior
write. That divergence is not a backend detail -- it is the shape `mir.md`'s Forbidden Shapes name:
a semantic fact two backends infer differently belongs in MIR, not in each backend's re-derivation.

This entry pins two things: what MIR states about an interior write, and how each backend realizes
that statement.

## Findings that shaped the decision

- **The asymmetry is forced by value realization, not an implementation gap.**
  `jit-value-realization.md` invariant 6: a runtime value handle is immutable from the generated
  side, so every apparent mutation is functional -- a new whole value stored back through the owner.
  The execution backend, holding only handles, cannot write an interior in place. The C++ backend,
  which owns real storage, can. Both realize the same value semantics.
- **The place-vs-projection fact is currently re-derived, and differently, by each backend.** The
  execution backend inspects the target expression's kind and walks it back to an owner to classify
  the write; the C++ backend treats it as an lvalue it may mutate. `mir.md`'s Forbidden Shapes: a
  fact two backends could infer differently is not yet in MIR and belongs there.
- **The layer below MIR already states the distinction.** `lir.md` separates a value-functional
  aggregate selector (extract / update, no addressable interior) from an addressable place
  projection (whose vocabulary is dereference and member -- it has no index or slice arm). MIR is
  the one layer still carrying the interior write as a reference.
- **The C++ in-place path is already proxy-driven and carries the LRM corner cases.** A dynamic
  array's element reference routes an out-of-range write to a discard target (LRM 7.4.5); a slice
  reference resolves its window; a string character reference writes through `putc` with its
  out-of-range / NUL rules (LRM 6.16.2); a union member reference activates the member first. The
  in-place write is already an optimization over a semantically complete proxy, not a naive store.
- **The below-MIR functional machinery is the abstraction gap made concrete.** A functional
  element-write primitive exists solely for the execution backend's rewrite and the C++ backend
  rejects it outright, and a single-container-type gate limits which interior writes take the
  functional path at all. Both are symptoms of the model living below MIR rather than in it.

## The decision

**D1. MIR states a write designator that distinguishes a place from a value projection.** A place is
independently addressable storage -- a whole variable, an object member, a dereferenced referent --
and its write is a store. A value projection is a selected part of a value aggregate -- a struct
component, a container element, a packed slice, a union member, a string character -- and its write
is a functional whole-value update. The boundary between the two -- the longest place prefix of the
target, with the value-aggregate descent as its suffix -- is stated as structure: the owner is a
place expression, and the value-projection suffix is a selector chain of ordinary sub-expressions. A
backend reads the boundary from the structure; it does not walk a nested lvalue expression to
recover it.

**D2. A value-projection write is a functional whole-value update stored back through the owner.**
Read the owner's whole value; produce a new whole value equal to the old one with the selected part
replaced along the selector chain; store that whole value back through the owner -- its place, its
activation cell, or its observable cell. The owner and each selector evaluate exactly once; this is
a stated MIR semantic. The materialization of that single evaluation into temporaries, and the read
/ update / store instruction sequence, belong to MIR-to-LIR (`lowering_boundaries.md`: HIR-to-MIR
introduces no storage placement; MIR-to-LIR introduces CFG and storage). MIR states the semantic; it
does not create cells.

**D3. The C++ in-place write is a behavior-preserving optimization, not a guarantee of an
addressable interior.** A backend may realize a value-projection write as an in-place mutation only
when its value library provides a write proxy semantically equivalent to the functional update --
carrying the out-of-range and x / z index behavior, slice window normalization, the per-container
and string special rules, the single evaluation of owner and selectors, and a single final writeback
into an observable owner. Absent such a proxy, the realization is the functional form. This is North
Star invariant 3: correctness is independent of optimization, and an in-place recovery is admitted
only when behavior-preserving. MIR never asserts that a value interior is addressable.

**D4. A `ref` bound to an unpacked interior is an owner-relative projection reference.** It is an
owner reference or capability plus selector state, realized functionally: a read is read-owner then
select; a write is read-owner, update the selected part, write-owner. It is never an in-place
pointer into a value's interior, and never a LIR indexed place projection -- a value interior is not
machine-addressable, which is why LIR's place vocabulary has no index or slice arm. SystemVerilog
bounds this to unpacked interiors; a packed slice or bit-select is not a legal `ref` actual.

**D5. This splits two write domains; it does not reverse the read-side or evaluate-once conclusions
of the prior decisions.** Genuine places keep the store model. What is retired is the requirement
that every write target be an op=-able address or location. A slice read still materializes an owned
value; compound assignment still evaluates its left-hand side once. Only the write side of a value
interior moves from "a location realized by a reference" to "a projection through its owner."

## Rejected alternatives

- **Classify the target but leave the owner and selectors for each backend to re-derive.** MIR would
  mark a target as a place or a projection but keep it as one nested lvalue expression, and each
  backend would walk that expression to find the owner boundary. Rejected: the boundary is then
  re-derived, and two backends can walk it to different results -- the exact Forbidden Shape in
  `mir.md`. The boundary must be structural.
- **State the owner and index by materializing temporaries in MIR.** Rejected: temporaries and
  storage placement are MIR-to-LIR's, not HIR-to-MIR's (`lowering_boundaries.md`). MIR states
  evaluate-once as a semantic; a lowering that creates cells at the MIR boundary is doing the next
  layer's work.
- **Re-encode the owner and selectors as node fields that restate the target expression's
  structure.** Rejected: `mir.md` forbids a field that duplicates what the surrounding structure
  already fixes. Owner and selectors remain structural children; only the owner boundary -- which
  the nested expression forces each backend to re-derive -- is stated.
- **Keep a backend-specific functional-write primitive as a MIR concept.** Rejected:
  `backend_contract.md` invariant 7 forbids a MIR primitive specialized for one backend. Under the
  designator the functional update is a stated MIR fact both backends realize; the runtime call that
  builds a new whole value is a realization of that fact below MIR, not a MIR concept one backend
  rejects.
- **Realize `ref`-to-interior as an in-place interior pointer or a LIR indexed place.** Rejected: an
  in-place interior reference would alias a value's part across copies, and value semantics forbid
  observing a sub-write through a copy. The reference is owner-relative and functional.

## Consequences

- Every value-aggregate interior write -- struct component, dynamic array, queue, associative array,
  packed slice, union member, string character -- routes through one functional path on the
  execution backend; the single-container-type gate and the per-family target switch are removed.
- A `ref`, `output`, or `inout` bound to an unpacked interior becomes expressible on the execution
  backend.
- The queue and associative-array value domains may land their representation and read operations
  independently, but their interior writes connect to the common designator path, never a new
  per-type gate.
- The whole-value store back through the owner is a semantic store (`value-store-discipline.md`),
  and for an observable owner it is the observable cell's write
  (`storage-access-as-place-formation.md`): one final writeback that fires subscribers once,
  whatever the selector chain's depth.

## Migration shape

The change reverses two accepted decisions for value interiors and is cross-cutting, so it lands in
reviewed cuts rather than one change. The staged shape:

1. A canonical target decomposition -- a target is a genuine place, or an owner place plus a
   selector chain -- including the owner-boundary rule. This is a migration adapter over the current
   nested-lvalue encoding, not the permanent design.
2. All value-interior families route through one functional-update lowering on the execution
   backend; the single-container-type gate and the backend-only branching are removed.
3. A `ref` / `const ref` bound to an interior becomes a projection reference.
4. The C++ backend gains value-library functional-update APIs with in-place recovery gated on proxy
   semantic-equivalence (D3); its render becomes a fixed function of the designator.
5. The canonical decomposition is promoted into the formal MIR write-designator node, and the old
   nested-lvalue write encoding is deleted.

The permanent design is the formal node of step 5. Step 1 exists to prove the selector vocabulary is
complete and to unblock the container families early; it is a labeled adapter, not the endpoint. The
danger it guards against is a container family adding another per-type branch, which would freeze
the reference model this decision removes.

## Relation to existing decisions

- `slice-value-semantics.md` -- a slice read still materializes an owned value; only the write side
  of a value interior is reframed, from an independently addressable reference to an owner-relative
  designator.
- `compound-assignment-write-location.md` -- the evaluate-once goal stands and is stated by D2 as
  the single evaluation of owner and selectors; the mechanism that required every target to be an
  op=-able location is retired for value interiors. Genuine places keep the store model.
- `jit-value-realization.md` -- invariant 6, that a handle is immutable and every apparent mutation
  is functional, is the driver for D2 and D3.
- `value-store-discipline.md` -- the whole-value writeback in D2 is a semantic store that preserves
  the owner's declared type.
- `storage-access-as-place-formation.md` -- a projection write into an observable owner reaches the
  cell through the cell's own write, so it fires subscribers through the same single writeback.
