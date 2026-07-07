# Closure and promoted scope are distinct nominal categories over a shared field substrate

Date: 2026-06-29 (revised 2026-07-07). Status: accepted and locked. This is the final form; it
supersedes the two earlier revisions (a value `TupleType`; then one unified `StructType` with an
optional invoke).

## Why this decision matters

MIR carries several "ordered fields plus a construction" entities. A closure holds captured state,
an escaped automatic scope is promoted into shared storage, the value aggregates are `TupleType` /
`UnionType`, and the object model is `mir::Class`. The same idea -- "a body reads compiler-generated
stored state" -- risks being expressed several ways at once, or being over-fused into one. This
entry settles what a closure's stored state is, what a promoted scope's storage is, and how they
relate to the nominal object.

The decision arrived in three steps, and the third is a correction of the second:

1. An early form made the closure environment a value `TupleType`. A tuple is **positional**, but
   captures are **discovered while the body is lowered**, so the body emitted reads before the
   layout was known -- forcing provisional slots, a reorder pass, and a body rebuild.
2. The tuple was replaced by a per-site **nominal record** (captures get named identity), and that
   record was then **fused with the promoted scope** into one `StructType` distinguished by an
   `optional invoke`.
3. That fusion was an over-merge. A closure and a promoted scope share a field substrate but are
   different kinds of thing: one is a **concrete callable value**, the other is **named shared
   storage**. They are split back into two nominal categories that share only the field vocabulary.

## The model: two categories, one shared field substrate

```text
Promoted automatic scope        Closure
= named generic StructType      = anonymous concrete callable value
  StructType{StructId}            ClosureType{ClosureId}
= fields, no invoke ever        = fields (captures) + exactly one invoke body
= reached via Shared<>          = reached by value / invoked in place
```

They share **only** the field substrate: `FieldDecl`, `FieldId`, `FieldAccess`, `FieldInit`, and
`CallableCode`. They do not share type identity, declaration, construction, or target realization.

### Why two categories, not one StructType with an optional invoke

- `StructDecl.invoke : optional<CallableCode>` is a role discriminator in disguise. Once render,
  LIR, and verification all branch on "does this struct have an invoke," it is not one category.
  Splitting removes the optional: invoke is unconditional on `ClosureDecl`, absent from `StructDecl`
  entirely.
- A closure and a scope are semantically different: a callable value versus data storage. Fusing
  them makes every consumer that touches the fused type re-derive which it is holding.
- Reference-versus-value for the scope is the ordinary wrapper (`Shared<StructType>`), so the scope
  needs no bespoke type -- but that is an argument for the scope being a plain `StructType`, not for
  the closure being one too.

### Why a closure is not merely a callable signature

Two closures of the same signature `(Args) -> R` can differ in capture fields, ownership, copy /
move / destroy behavior, coroutine-frame payload, and backend capture clause. A closure needs
concrete per-site identity (`ClosureId`), exactly as C++ gives each lambda a unique unnamed type. A
signature is not an identity.

## The decisions

```text
D1. A promoted automatic scope is a named generic StructType{StructId}. StructDecl carries fields
    only and NEVER an invoke (a scope is storage, not callable). It is reached through Shared<>. It
    is a compiler-generated nominal aggregate; in this cut it is not a source-level user-declarable
    struct feature.

D2. A closure is an anonymous concrete callable value: ClosureType{ClosureId}. ClosureDecl carries
    capture fields plus exactly one invoke body. Callability is the unconditional presence of the
    invoke on the declaration -- there is no invoke on a scope to compare against, so no flag.

D3. Closure and scope share ONLY the field substrate (FieldDecl, FieldId, FieldAccess, FieldInit,
    CallableCode). They do not share type identity, declaration, construction node, or target
    realization. No universal Record<Storage, Behavior> type; no StructType with an is_closure /
    is_frame discriminator.

D4. A closure's invoke reads captures through its receiver, which in this cut is a read-only borrow
    Borrowed<ClosureType> (the invoke body's locals[0]). A write to captured state is a write through
    a captured Ref<T> or through a captured Shared<StructType> field, never a write to the capture
    slot itself.

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

D8. A scope struct's identity and its emission nesting are separate stored relations. Identity is the
    StructId in the unit's struct registry. Emission nesting is an explicit list on the nesting class
    (mir::Class.structs), parallel to the child-class list (mir::Class.contained): lowering records a
    scope struct on the class whose body opens it. A backend that nests emits each struct by
    iterating that list -- a mechanical per-node render, never a walk over the body tree or a
    payload-driven guess at "is this a scope construction" (backend_contract.md forbids exactly that).
    The relation is NOT on the generic CallableCode -- a plain function carries no generated-storage
    list; it is on the class, the emission unit.

D9. Neither category is a mir::Class. A ClosureType has one entrypoint and no inheritance / dispatch /
    managed handle; a scope StructType has no behavior at all. mir::Class stays the one object IR.
    The nominal categories are four -- TupleType (structural product), StructType (named aggregate),
    ClosureType (concrete callable value), mir::Class (rich object) -- each a distinct concept over
    the shared field substrate.

D10. HIR-to-MIR owns the SystemVerilog capture / lifetime policy; MIR expresses only the resulting
    storage facts. "Activation frame" is a lowering role name for a Shared<StructType>, not a MIR
    type.
```

The categories, kept distinct:

```text
                    storage-by     behavior             identity
TupleType           value          none                 structural / positional
StructType          value / Shared none                 named aggregate; runtime storage identity
ClosureType         value          one invoke body      anonymous concrete callable value
mir::Class          managed ref    methods / dispatch   nominal object identity
```

### Realization is a backend fact, not a MIR definition

The current C++ backend realizes a `ClosureType` as an anonymous lambda and a scope `StructType` as
a named nested `struct` reached through `shared_ptr`. That is one backend's realization, not the MIR
definition. The MIR-level definition of a `ClosureType` is "an anonymous concrete callable value:
capture fields plus one invoke body" -- another backend may realize it as an aggregate payload plus
a direct invoke function (LLVM) or a runtime payload object plus a code pointer (a JIT). Do not fix
"ClosureType is a lambda" as an invariant; the invariant is the capture-fields-plus-invoke shape.

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
  closure or a scope, and the "anonymous lambda vs named struct" realization split becomes a
  conditional carried through LIR and the backend. Two categories make each fact type-level.
- **A closure is only a callable signature.** Loses concrete identity: same-signature closures
  differ in captures, ownership, and payload. Identity is the `ClosureId`.
- **A closure or a scope is a `mir::Class`.** Grants inheritance / dispatch / managed-handle
  machinery neither may use.
- **A backend derives a scope struct's emission nesting by walking the body tree** (finding
  constructions, guessing a construction's shape from a node's payload). That is a non-mechanical
  render `backend_contract.md` forbids -- render output must come from a node's own structural
  fields. The nesting is an explicit stored relation instead, iterated mechanically.
- **The generated-struct list lives on `CallableCode`.** A plain function then carries an
  always-empty backend-placement collection. The relation lives on the nesting class (the emission
  unit), parallel to its child-class containment list, instead.

## Consequences

- `StructType` (scopes) and `ClosureType` (closures) are two nominal categories; `unit.structs`
  holds scope `StructDecl`s, a separate closure registry holds `ClosureDecl`s. Both use the shared
  `Field` substrate.
- `StructDecl` has no invoke; the earlier `optional<CallableCode> invoke` is removed. A closure's
  invoke lives on `ClosureDecl`, unconditionally.
- The frame is `Shared<StructType>`; `FrameStorageType` / `ActivationFrame` as distinct types remain
  gone (that generalization stands). Only the closure-into-`StructType` fusion is undone: the
  closure regains its own `ClosureType`, now sitting on the shared field substrate.
- `CallableCode.generated_structs` and its recording helper are removed. Emission nesting is an
  explicit list on the nesting class (`mir::Class.structs`), populated at lowering and iterated by a
  nesting backend -- parallel to the child-class containment list, no body-tree walk.
- A struct value is built by `StructConstructExpr` (a named-field aggregate literal) and a closure
  by `ClosureExpr`; both are first-class, id-surfacing nodes over the shared `FieldInit` vocabulary.
  A scope's `Shared<StructType>` handle is allocated through the generic constructor protocol
  (`Construct` callee), reference ownership being the wrapper around the value.
- The binding / capture contract (`binding_and_capture.md`) keeps its origin identity and
  forwarding; the materialized capture is a `ClosureDecl` field.

## Cross-references

- `../architecture/compiler_generated_storage.md` -- the contract form of this decision.
- `../architecture/mir.md` -- `TupleType` (one structural product), `StructType` (one named
  aggregate), `ClosureType` (concrete callable value), and `mir::Class` (object) as distinct nominal
  categories over one field substrate.
- `../architecture/callable.md` -- the callable concept and the receiver rule; a closure body's
  receiver is the `ClosureType` value itself.
- `../architecture/binding_and_capture.md` -- origin identity and capture forwarding; only the
  materialized capture representation is a `ClosureDecl` field.
- `../architecture/object_model.md` -- invariant 1 (no second object IR), which `ClosureType` (a
  callable value, not an object) does not touch.
- `unified-callable-model.md` -- `ClosureType` is one concrete callable-value producer;
  `ErasedCallableType<Sig>` is the erased boundary type.
- `reference-as-data-type.md` -- `RefType` as the observable-cell reference (an alias-capture
  field).
- `lifetime-extended-automatic-scope.md` -- per-scope retention through a shared handle.
- `object-model.md` -- the managed (traced) class-handle reference, distinct from the shared scope
  reference.
