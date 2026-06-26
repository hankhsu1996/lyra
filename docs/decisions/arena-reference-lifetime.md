# Arena reference lifetime: `Get` is a transient view

Date: 2026-06-26 Status: accepted

## Context

`Arena<T, Id>` (the universal append-only pool from
[generic-lowering-machinery](generic-lowering-machinery.md)) backs its elements in a relocatable
`std::vector<T>` and exposes `Get(id) -> const T&`. A `Get` reference therefore points into storage
that a later `Add` can relocate, so holding a `Get` reference across an `Add` to the same arena is a
dangling reference. Whether it actually faults depends on allocator timing, so the hazard is latent
until an unrelated change perturbs the growth pattern.

The design question this raises is whether the arena should guarantee reference stability -- backing
elements in storage that never relocates on append (a deque, a segmented or paged arena) -- so that
a `Get` reference stays valid across an `Add`.

The decision rests on what lowering actually needs. Every site that held a `Get` reference across a
same-arena `Add` was inspected. In every case the value still needed past the `Add` was a small,
projectable fact -- a `TypeId`, a `TypeKind`, an already-copied payload -- never the whole node held
as a live reference:

- An owned-child construction needed the slot pointer's pointee `TypeId` after interning the child's
  index-array type.
- An lvalue rewrite needed the operand's result `TypeId` after a recursive rewrite appended nodes.
- A structural signal needed the value type's `TypeKind` after interning the field's wrapper type.

No site needed a stable borrow of a whole node across a mutation. The recurring shape is "project a
fact from an existing node, then intern the sibling or derived nodes the lowering is building" --
the inspection and the interning are interleaved by narrative order, not by a data dependency on the
live reference.

## Decision

`Get` is a transient view; the `Id` is the only durable handle.

1. **The arena stays a relocatable `vector`.** Dense contiguous storage, the cheapest `Id -> T`
   indexing, and good iteration locality are kept. The arena does not promise that a `Get` reference
   survives an `Add`.

2. **Same-arena mutation invalidates prior `Get` results.** Any `Add` may relocate the backing
   storage, invalidating every reference, pointer, and iterator a prior `Get` returned to that
   arena. This contract lives on the `Arena` type.

3. **Lowering projects facts before mutating.** To carry information from a node past an `Add`, a
   caller projects the value it needs -- an `Id`, a kind, a copied field -- before the `Add`, and
   uses that value afterward. The currency that flows across lowering steps is ids and value facts,
   not node references. This is the same id-first model the identity rules already establish; the
   reference is an implementation-local view, not a unit of data flow.

4. **Value immutability is distinct from address stability.** An appended node's _value_ is final
   (load-bearing for id references and ownership-keyed caching). Its _address_ is not stable. The
   arena promises the former and explicitly not the latter.

## Rejected

- **Stable-reference storage (deque / segmented / paged) so `Get` references survive `Add`.** This
  extends the contract of every arena -- system-wide -- to serve a capability no site requires: the
  usage evidence shows the value needed past a mutation is always a projectable fact, never a live
  whole-node borrow. It trades contiguity, locality, and the cheapest indexing on a read-heavy hot
  path for a guarantee nothing depends on, and over-extends the abstraction from an incidental code
  shape. Reconsider only if a genuine need for a stable cross-mutation whole-node borrow appears.

- **`Get` returns by value.** Safe by construction, but copies a fat node (`Type`, `Expr`) on every
  read, on the hot path, when most reads touch a single field. The need is to copy the _one fact_
  that must outlive a mutation, not the whole node on every access.

- **Discipline alone ("remember to copy before `Add`").** Necessary but not sufficient as the only
  safeguard: an unwritten rule rots. It is paired with the contract stated on the type, projection
  at the call site, and review; a value fact projected before the mutation is the normal shape, and
  a `Get` reference read after a same-arena `Add` is the forbidden one.

## Consequences

- The `Arena` header states the contract: `Get` is a transient view; a same-arena `Add` invalidates
  prior references, pointers, and iterators; the `Id` is the only durable handle.
- A lowering routine reads a node, projects the facts that must outlive any interning it does, then
  interns / appends / recurses. Reads after a same-arena mutation go through the id or a projected
  value.
- High-frequency, semantically-meaningful projections (a type's kind, a pointer's pointee, an
  expression's type) may gain value-returning accessors as consumers appear, so the projecting shape
  reads cleanly; they are not added pre-emptively.
- Holding a `Get` reference across a same-arena `Add` and reading it afterward is a forbidden shape.
  Absent a reliable static check, it is caught by the contract and review, not by the data
  structure.

## Cross-references

- [generic-lowering-machinery](generic-lowering-machinery.md) (the universal `Arena<T>` this
  constrains).
- `architecture/identity_and_ownership.md` (local entities are referenced by typed id, not by
  pointer).
- `architecture/incremental_build.md` (caching keys are ownership-based; raw pointer identity is a
  forbidden key).
- `architecture/mir.md` (MIR ids are the single source of identity at that layer).
