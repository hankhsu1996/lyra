# Compiler-Generated Storage

## Purpose

This doc owns the storage shape of the compiler-synthesized state that a lowering generates so a
deferred or concurrent body can reach the state it needs and so an automatic local can outlive the
execution that created it. Two source constructs produce that state, and they are **two different
kinds of thing** that share a field vocabulary:

- a **closure** -- an anonymous, concrete callable value: capture fields plus one invoke body;
- a **promoted automatic scope** -- named shared storage that owns a callable's lifetime-extended
  automatic locals (the lowering role historically called the "activation frame").

Neither is a nominal SystemVerilog object (`object_model.md`) and neither is loose program data
(`mir.md`'s value-type system).

> There are **two nominal categories**, not one. A closure is a **`ClosureType`** (an anonymous
> concrete callable value); a promoted scope is a **`StructType`** (a named generic aggregate)
> reached through a `Shared<>` wrapper. They share **only** the field substrate -- `FieldDecl`,
> `FieldId`, `FieldAccess`, `FieldInit`, `CallableCode` -- and share no type identity, declaration,
> construction node, or target realization.

An earlier model fused both into one `StructType` distinguished by an `optional invoke`. That
optional was a role discriminator: every consumer that touched the type re-derived whether it held a
closure or a scope, and the realization split (anonymous callable vs named storage) became a
conditional carried through LIR and the backend. Splitting the categories makes each fact
type-level.

## Owns

- The **`ClosureType`** category: an anonymous concrete callable value, `ClosureType{ClosureId}`,
  whose declaration (`ClosureDecl`) carries capture fields plus exactly one invoke body. Two
  closures of the same signature but different captures are different `ClosureType`s.
- The **`StructType`** category as used for a promoted scope: a named generic aggregate,
  `StructType{StructId}`, whose declaration carries fields and no invoke, reached through
  `Shared<>`. In this cut a `StructType` is compiler-generated only; it is not yet a source-level
  user-declarable aggregate feature.
- The three **capture forms** -- snapshot, live-place alias, retained scope -- and the rule that the
  form is carried by the captured field's type, not by a separate capture-mode axis.
- The separate relations over a captured field: its **capture key** (which binding), its **field
  id** (identity within the declaration, what the invoke body reads), its **field order** (a
  deterministic declaration / emission listing), and the **initializer evaluation order** at the
  construction site -- each kept distinct from the others and from physical layout.
- The **field-storage vocabulary** shared by the closure, the scope struct, and the nominal object.

## Does Not Own

- Which binding a reference names, per-body materialization, and capture forwarding. That is the
  lexical axis, owned by `binding_and_capture.md`.
- The callable concept -- code versus value, the signature, the result type as call protocol. That
  is `callable.md`. This doc owns the captured-state half of a closure.
- The nominal object model -- methods, inheritance, dispatch, lifecycle, managed handles
  (`object_model.md`). The object shares the field vocabulary and nothing else with these
  categories.
- The value-type system, including the tuple as the one structural product and the struct as the one
  named aggregate (`mir.md`).
- The runtime execution instance (`activation.md`). "Activation frame" is a lowering role name for a
  `Shared<StructType>`, not the execution instance.
- Which capture form a SystemVerilog construct requires. That is HIR-to-MIR capture / lifetime
  policy.
- **Target realization.** Whether a `ClosureType` is realized as an anonymous C++ lambda and a scope
  `StructType` as a named nested `struct` is the backend's choice. This doc owns the semantic shape,
  not its emitted form. Physical placement, physical field offsets, and retention realization are
  LIR and backend concerns.

## Core Invariants

Stated positively: each fixes one rule, and what is allowed or forbidden follows from it.

1. **A closure is a `ClosureType`; a promoted scope is a `StructType`.** They are two nominal
   categories. A `ClosureDecl` is capture fields plus exactly one invoke body; a scope `StructDecl`
   is fields and no invoke. There is no fused type and no `is_closure` / `is_frame` discriminator.

2. **The two categories share only the field substrate.** `FieldDecl`, `FieldId`, `FieldAccess`,
   `FieldInit`, and `CallableCode` are shared; type identity, declaration, construction node, and
   target realization are not. The shared vocabulary is narrow, so `object_model.md`'s
   single-object-IR rule stands and no universal `Record` type appears.

3. **A closure's invoke reads captures through a read-only receiver.** The receiver in this cut is a
   `Borrowed<ClosureType>` (the invoke body's `locals[0]`); a captured read is field access over it.
   A write to captured state is a write through a captured `RefType` or through a captured
   `Shared<StructType>` field, never a write to the capture slot itself.

4. **Value-versus-reference for a scope is the wrapper, not a bespoke type.** A scope is a plain
   `StructType` reached through a `Shared<>` handle; the reference semantics are the ordinary MIR
   wrapper. A closure is reached by value / invoked in place. Neither needs a reference-aggregate
   type category of its own.

5. **A captured field is a snapshot, a live-place alias, or a retained scope -- by its type.** A
   value-typed field is a snapshot; a `RefType` field is a live alias of an observable cell
   (`reference-as-data-type.md`); a `Shared<StructType>` field is a retained scope. The capture form
   is the field's type, never a separate axis.

6. **A captured field's identity is not its emission order and not its physical layout.** The
   **field id** (assigned at capture discovery) is what the invoke body reads. **field_order** is a
   deterministic declaration / emission listing over the fields; **physical offsets** belong to LIR.
   The invoke body reads by stable field id and is never rewritten to follow a layout change.

7. **field_order and initializer evaluation order are independent.** field_order orders the fields
   for declaration / emission; a closure construction's `field_inits` carries the source-semantic
   evaluation order (a side-effecting source is a sequenced temporary first). Neither derives from
   the other, and reordering by field_order must never change initializer evaluation order.

8. **Every automatic local belongs to exactly one scope struct, its semantic owner.** Placement --
   flat stack frame, inline coroutine frame, heap allocation, shared-owned record -- is not
   source-semantic. Only a scope something retains is materialized as shared-owned storage, retained
   whole through a `Shared<>` handle. An escaped automatic local is never individually boxed.

9. **A scope struct's identity and its emission nesting are separate stored relations.** Identity is
   the `StructId` in the unit's struct registry. Emission nesting is an explicit list on the nesting
   class, parallel to its child-class containment list: lowering records a scope struct on the class
   whose body opens it. A backend that nests emits each struct by iterating that list -- a
   mechanical per-node render, never a walk over the body tree or a payload-driven guess at a
   construction's shape (`backend_contract.md` forbids exactly that). The relation is not on the
   generic `CallableCode` -- a plain function carries no generated-storage list; it is on the class,
   the emission unit.

10. **Compiler-generated retained scopes and SystemVerilog class handles are distinct reference
    regimes.** A retained scope uses deterministic shared ownership (`PointerType{kShared}`, an
    acyclic DAG by construction); an SV class handle uses the managed, precisely-traced reference. A
    scope is never reached through the managed handle and a class handle is never reached through
    the shared one.

A later addition, when a real consumer requires it: an **`ErasedCallableType<Sig>`** for a
heterogeneous collection of closures of one signature. It is introduced with its explicit erasure
operation and its complete contract (invoke, ownership / destruction, copy / move), never through
implicit erasure of a concrete `ClosureType`.

## Boundary to Adjacent Layers

- **`mir.md`** owns the value/reference wrappers and the four nominal categories. This doc composes
  them: a closure is a `ClosureType`; a retained-scope field is a `Shared<StructType>`; an alias
  field is a `RefType`.
- **`callable.md`** owns the callable concept and the receiver rule. This doc owns the closure's
  captured fields; the closure body's receiver is the `ClosureType` value itself.
- **`binding_and_capture.md`** owns origin identity and capture forwarding. This doc states that the
  captured state is a `ClosureDecl` whose fields are keyed by those origins.
- **`object_model.md`** owns the nominal object. This doc states that the object shares only the
  field vocabulary with these categories.
- **`activation.md`** owns the runtime execution instance; "activation frame" here is a role name
  for a `Shared<StructType>`.
- **HIR-to-MIR** decides the capture form per source construct; LIR and the backend decide physical
  placement and target realization (lambda, functor, payload-plus-code-pointer).

## Forbidden Shapes

Each follows from an invariant above.

- One fused type for closure and scope, or a `StructType` with an `optional invoke` / `is_closure`
  flag. (Invariant 1.)
- A universal `Record<Storage, Behavior>` type, or sharing beyond the field substrate. (Invariant
  2.)
- A closure capture written as a write to the capture slot rather than through a captured `RefType`
  / `Shared<StructType>`. (Invariant 3.)
- A reference-aggregate type category minted for the scope; reference is the wrapper. (Invariant 4.)
- A closure-specific capture id space, a capture-read node distinct from field access, or a
  positional tuple standing in for the environment. (Invariant 2, 5.)
- A field's emission-order position or physical offset used as its identity, or a canonical-ordering
  pass that rewrites the invoke body, or field_order used to reorder initializer evaluation.
  (Invariant 6, 7.)
- A backend deriving a struct's emission nesting by walking the body tree or guessing a
  construction's shape from a node's payload, or the emission-nesting list stored on `CallableCode`
  rather than the nesting class. (Invariant 9.)
- A scope reached through the managed reference, or a class handle reached through the shared
  reference. (Invariant 10.)
- "`ClosureType` is a lambda" written as a MIR invariant. The lambda is one backend's realization;
  the invariant is capture-fields-plus-invoke.

## Notes / Examples

A fork branch that reads a lifetime-extended local and the enclosing object, in single-line lowered
form. `x` outlives the frame, so it lives in a scope `StructType` reached through a shared handle;
the branch is a closure capturing that handle and the object pointer.

```text
SV (inside a method M):
  int x = 5;               // lifetime-extended: a scope StructType, reached via Shared<>
  fork  #1 out <= x;  join_none

scope     : StructType M__scope0 { x: int }                  // named aggregate, reached via Shared<>
closure   : ClosureType C_k { self: M*, scope: Shared<M__scope0> } + invoke
invoke    : field_access(closure_receiver, field_of(scope)) then ... -> read scope.x through the handle
```

The current C++ backend realizes the scope as `struct M__scope0 {...}` + `shared_ptr` and the
closure as an anonymous lambda immediately invoked with `(self, scope)`. That is this backend's
realization, not the MIR definition.

The three capture forms differ only by field type:

```text
snapshot capture : field type = int                  // a value taken at construction
live-place alias : field type = Ref<logic>           // an observable-cell alias
retained scope   : field type = Shared<M__scope0>    // retain the owner; read scope.x through it
```

A closure submitted to a heterogeneous region queue is erased at the submit site once that consumer
exists: `concrete ClosureType C_k --[ explicit erase ]--> ErasedCallableType< () -> void >`.
