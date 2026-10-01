# A closure is its own category; a lifted local is a cell

Date: 2026-06-29 (revised 2026-07-07, 2026-09-29, 2026-09-30). Status: accepted and locked. This is
the final form; it supersedes four earlier revisions (a value `TupleType`; then one unified
`StructType` with an optional invoke; then a scope struct with a field list of its own beside the
struct the source declares; then a struct the lowering declares, gathering a scope's lifted locals).

## Why this decision matters

MIR carries several "ordered parts plus a construction" entities. A closure holds captured state, an
automatic local that a detached branch can outlive is lifted into shared storage, the value
aggregates are `TupleType`, `StructType` and `UnionType`, and the object model is `mir::Class`. The
same idea -- "a body reads compiler-generated stored state" -- risks being expressed several ways at
once, or being over-fused into one. This entry settles what a closure's stored state is, what a
lifted local's storage is, and how they relate to the variable a body declares and to the nominal
object.

The decision arrived in five steps:

1. An early form made the closure environment a value `TupleType`. A tuple is **positional**, but
   captures are **discovered while the body is lowered**, so the body emitted reads before the
   layout was known -- forcing provisional slots, a reorder pass, and a body rebuild.
2. The tuple was replaced by a per-site **nominal record** (captures get named identity), and that
   record was then **fused with the promoted scope** into one `StructType` distinguished by an
   `optional invoke`.
3. That fusion was an over-merge: a closure is a **concrete callable value**, a scope is
   **storage**. They were split back into two categories sharing a field vocabulary, the scope
   keeping the `StructType` name with a field list of its own.
4. That left two structs, and the scope's became a struct the lowering declares, with no name and no
   methods, built from its default value and held through a shared pointer.
5. Every variable a body that can wait declares then became a cell that reports its writes
   ([a-variable-a-body-declares-reports-its-writes](a-variable-a-body-declares-reports-its-writes.md)),
   so a lifted local is a cell too. A cell is storage and not a value -- it cannot be copied or
   moved, since what waits on it is registered on it -- so a struct value of cells is not a thing
   any product can be. Each lifted local's cell is held on its own instead.

## The model

```text
Lifted automatic local             Closure
= the variable's own cell          = anonymous concrete callable value
  ObservableType{T}                  ClosureType{ClosureId}
= held through Shared<cell>,       = fields (captures) + exactly one invoke body
  made empty, then initialized     = reached by value / invoked in place,
= reached by dereferencing           a capture by field access
  its handle
```

They share `CallableCode` and nothing else. A lifted local shares everything else with a local of
the frame: its type, and every operation that reads, writes, lends or waits on it.

### Why a lifted local is its own cell

- It already is a cell wherever it is declared; lifting it changes only where the cell lives, which
  is what a handle to it says. Holding a cell past its frame is `make_shared<Var<T>>()`, and
  dereferencing the handle names the cell.
- A cell cannot be a component of a struct value: a struct is copied and moved, and a cell is where
  waiters are registered, so neither may happen to it. Gathering a scope's cells into one record
  would need a record that is storage rather than a value, with its own construction in place, its
  own member access and its own realization on each target -- the separate record step 3 had and
  step 4 removed.
- Swift boxes each captured `var` on its own (`alloc_box`), and clang does the same for a `__block`
  variable (its byref structure is per variable). C# gathers a scope's captured locals into one
  display class, which is a class -- storage -- and not a value. Our lifted locals are cells, so the
  per-variable form is the one that needs nothing new.

### Why a closure is not a struct

- A closure is callable: its declaration carries exactly one invoke body, and a struct carries none.
  Fusing them makes the invoke optional, a role discriminator every consumer re-derives.
- A closure's captures are discovered while its body is lowered, so a capture is named by a stable
  field id rather than by a position (D6); a struct's components are all known when it is declared.

### Why a closure is not merely a callable signature

Two closures of the same signature `(Args) -> R` can differ in capture fields, ownership, copy /
move / destroy behavior, coroutine-frame payload, and backend capture clause. A closure needs
concrete per-site identity (`ClosureId`), exactly as C++ gives each lambda a unique unnamed type. A
signature is not an identity.

## The decisions

```text
D1. A lifted automatic local is its own cell, the ObservableType a local of a waiting body is,
    held through Shared<cell>. The handle is made with the cell, empty, where the local's block is
    entered; the local's declaration initializes the cell through the handle; and every access to
    the local is an access to the dereferenced handle.

D2. A closure is an anonymous concrete callable value: ClosureType{ClosureId}. ClosureDecl carries
    capture fields plus exactly one invoke body. Callability is the unconditional presence of the
    invoke on the declaration, so there is no flag.

D3. A closure and a lifted local share only CallableCode. No universal Record<Storage, Behavior>
    type; no StructType with an is_closure / is_frame discriminator; no record of lifted locals.

D4. A closure's invoke reads captures through its receiver, a read-only borrow
    Borrowed<ClosureType> (the invoke body's locals[0]). A write to captured state is a write through
    a captured Ref<T> or through a captured Shared<cell> field, never a write to the capture slot
    itself.

D5. A callable value has two type levels: the concrete ClosureType (its exact captures) and an erased
    ErasedCallableType<Sig> reached through an explicit erasure, for a heterogeneous collection of
    callables of one signature. Erasure is never implicit and is introduced with a real consumer.
    ClosureType is one concrete callable-value producer (unified-callable-model.md); it is not the
    only conceivable one (a bound method value, a function item), so the concept name stays "closure,"
    not "the concrete callable value."

D6. Capture key, field id, and field emission order are separate relations:
        capture key (BindingOriginId) -> FieldId -> field_order -> (LIR) physical offset.
    The invoke body reads a capture by its stable FieldId; field_order is a deterministic
    declaration / emission listing and never rewrites the body.

D7. field_order and construction evaluation order are independent. field_order is the deterministic
    declaration / emission order over the fields. ClosureExpr.field_inits carries the source-semantic
    initializer evaluation order (a side-effecting source is a sequenced temporary first). Neither is
    derivable from the other, and sorting by field_order must never change initializer evaluation
    order.

D8. Neither category is a mir::Class. A ClosureType has one entrypoint and no inheritance / dispatch
    / managed handle; a cell has the operations of a variable. mir::Class stays the one object IR.
    The categories are four -- TupleType (structural product), StructType (named product),
    ClosureType (concrete callable value), mir::Class (rich object).

D9. HIR-to-MIR owns the SystemVerilog capture / lifetime policy; MIR expresses only the resulting
    storage facts.
```

The categories, kept distinct:

```text
                    storage-by     behavior                     identity
TupleType           value          none                         structural / positional
StructType          value          whole-value methods          its declaration
ClosureType         value          one invoke body              anonymous concrete callable value
mir::Class          managed ref    methods / dispatch           nominal object identity
```

### Realization is a backend fact, not a MIR definition

The C++ backend realizes a `ClosureType` as a struct with a call operator, and a lifted local's hold
as a `shared_ptr` to its `Var<T>`. The execution backend holds the cell in a counted hold the
runtime makes, empty, for the domain the cell holds. Those are each backend's realization, not the
MIR definition. Do not fix "ClosureType is a lambda" as an invariant; the invariant is the
capture-fields-plus-invoke shape.

### Why field identity is not the layout index (D6)

The invoke body must be emittable during lowering, before every capture is discovered. A stable
`FieldId` assigned at discovery decouples "which capture" (semantic) from "which physical field"
(representation), so the body reads `FieldAccess(receiver, FieldId)` immediately and correctly and
is never rewritten when the layout is canonicalized.

## Rejected alternatives

- **The closure environment is a structural value `TupleType`.** Positional identity forces a body
  rebuild when the late-discovered layout is fixed. A named capture identity removes it.
- **One unified `StructType` with an optional invoke for both closure and scope.** The optional
  invoke is a role discriminator; every consumer that touches the type re-derives whether it holds a
  closure or a scope.
- **A struct the lowering declares, gathering a scope's lifted locals.** Right while a local was a
  value; a lifted local is a cell, and a struct value of cells cannot be copied, moved or built from
  a default the way every struct is.
- **A record of a scope's lifted cells, built in place.** A record that is storage and not a value
  needs its own construction, member access and realization on each target -- the second record step
  3 had -- to save a handle per local, which only a branch naming several of them pays.
- **A closure is only a callable signature.** Loses concrete identity: same-signature closures
  differ in captures, ownership, and payload. Identity is the `ClosureId`.
- **A closure or a lifted local is a `mir::Class`.** Grants inheritance / dispatch / managed-handle
  machinery neither may use.

## Consequences

- `unit.structs` holds exactly the structs the source declares, each with a name and its methods. A
  separate closure registry holds `ClosureDecl`s.
- A lifted local's `Shared<cell>` handle is built by the generic constructor protocol with no
  operand, and the local's declaration initializes the cell through it.
- A branch naming several lifted locals captures one handle for each, where it captured one per
  scope; each is a counted hold, and each lifted local is its own allocation.
- A closure value is built by `ClosureExpr`, a first-class id-surfacing node over the `FieldInit`
  vocabulary.
- The binding / capture contract (`binding_and_capture.md`) keeps its origin identity and
  forwarding; the materialized capture is a `ClosureDecl` field.

## Cross-references

- `../architecture/compiler_generated_storage.md` -- the contract form of this decision.
- `../architecture/mir.md` -- `TupleType`, `StructType`, `ClosureType`, and `mir::Class` as distinct
  categories.
- `../architecture/callable.md` -- the callable concept and the receiver rule; a closure body's
  receiver is the `ClosureType` value itself.
- `../architecture/binding_and_capture.md` -- origin identity and capture forwarding; only the
  materialized capture representation is a `ClosureDecl` field.
- `../architecture/object_model.md` -- invariant 1 (no second object IR).
- `unified-callable-model.md` -- `ClosureType` is one concrete callable-value producer;
  `ErasedCallableType<Sig>` is the erased boundary type.
- [a-variable-a-body-declares-reports-its-writes](a-variable-a-body-declares-reports-its-writes.md)
  -- why a lifted local is a cell.
- `reference-as-data-type.md` -- `RefType` as the observable-cell reference (an alias-capture
  field).
- `lifetime-extended-automatic-scope.md` -- retention through a shared handle.
