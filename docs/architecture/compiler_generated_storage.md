# Compiler-Generated Storage

## Purpose

This doc owns the storage shape of the compiler-synthesized state that a lowering generates so a
deferred or concurrent body can reach the state it needs and so an automatic local can outlive the
execution that created it. Two source constructs produce that state, and they are **two different
kinds of thing**:

- a **closure** -- an anonymous, concrete callable value: capture fields plus one invoke body;
- a **lifted automatic local** -- a lifetime-extended automatic local of a block, the observable
  cell every variable a waiting body declares is, held through shared ownership.

Neither is a nominal SystemVerilog object (`object_model.md`).

> A closure is a **`ClosureType`** (an anonymous concrete callable value). A lifted local is its own
> cell -- the `ObservableType` any variable a waiting body declares is -- reached through a
> `Shared<>` wrapper whose dereference names the cell. A closure's captures are fields of its
> declaration, reached by field access; a lifted local is reached through its own handle. The two
> share `CallableCode` and nothing else.

## Owns

- The **`ClosureType`** category: an anonymous concrete callable value, `ClosureType{ClosureId}`,
  whose declaration (`ClosureDecl`) carries capture fields plus exactly one invoke body. Two
  closures of the same signature but different captures are different `ClosureType`s.
- The **lifted local**: one `Shared<cell>` per lifetime-extended local, made empty where its block
  is entered and initialized where its declaration is reached.
- The three **capture forms** -- snapshot, live-place alias, retained cell -- and the rule that the
  form is carried by the captured field's type, not by a separate capture-mode axis.
- The separate relations over a captured field: its **capture key** (which binding), its **field
  id** (identity within the declaration, what the invoke body reads), its **field order** (a
  deterministic declaration / emission listing), and the **initializer evaluation order** at the
  construction site -- each kept distinct from the others and from physical layout.

## Does Not Own

- Which binding a reference names, per-body materialization, and capture forwarding. That is the
  lexical axis, owned by `binding_and_capture.md`.
- The callable concept -- code versus value, the signature, the result type as call protocol. That
  is `callable.md`. This doc owns the captured-state half of a closure.
- The nominal object model -- methods, inheritance, dispatch, lifecycle, managed handles
  (`object_model.md`). The object shares the field vocabulary with a closure and nothing with a
  lifted local.
- The value-type system and the observable cell a variable is (`mir.md`), and which variables are
  cells (`a-variable-a-body-declares-reports-its-writes` in `decisions/`).
- The runtime execution instance (`activation.md`).
- Which capture form a SystemVerilog construct requires. That is HIR-to-MIR capture / lifetime
  policy.
- **Target realization.** How a `ClosureType` and a lifted local's hold are spelled in a target
  language is the backend's choice. This doc owns the semantic shape, not its emitted form. Physical
  placement, physical offsets, and retention realization are LIR and backend concerns.

## Core Invariants

Stated positively: each fixes one rule, and what is allowed or forbidden follows from it.

1. **A closure is a `ClosureType`; a lifted local is a cell.** A `ClosureDecl` is capture fields
   plus exactly one invoke body; a lifted local is the cell its declaration would have had in the
   frame, held elsewhere. There is no fused type and no `is_closure` / `is_frame` discriminator.

2. **A lifted local is the variable it was, not a new kind of storage.** Its cell is the same
   `ObservableType` a local of a waiting body has, read, written, waited on and lent through the
   same operations; only where the cell lives differs.

3. **A closure's invoke reads captures through a read-only receiver.** The receiver is a
   `Borrowed<ClosureType>` (the invoke body's `locals[0]`); a captured read is field access over it.
   A write to captured state is a write through a captured `RefType` or through a captured
   `Shared<cell>`, never a write to the capture slot itself.

4. **Value-versus-reference for a lifted local is the wrapper, not a bespoke type.** The cell is
   reached through a `Shared<>` handle; the reference semantics are the ordinary MIR wrapper, and
   dereferencing the handle names the cell. The handle is made with the cell, empty, the way
   `make_shared<T>()` makes one, and the local's declaration then initializes the cell.

5. **A captured field is a snapshot, a live-place alias, or a retained cell -- by its type.** A
   value-typed field is a snapshot; a `RefType` field is a live alias of an observable cell; a
   `Shared<cell>` field retains a lifted local. The capture form is the field's type, never a
   separate axis.

6. **A captured field's identity is not its emission order and not its physical layout.** The
   **field id** (assigned at capture discovery) is what the invoke body reads. **field_order** is a
   deterministic declaration / emission listing over the fields; **physical offsets** belong to LIR.
   The invoke body reads by stable field id and is never rewritten to follow a layout change.

7. **field_order and initializer evaluation order are independent.** field_order orders the fields
   for declaration / emission; a closure construction's `field_inits` carries the source-semantic
   evaluation order (a side-effecting source is a sequenced temporary first). Neither derives from
   the other, and reordering by field_order must never change initializer evaluation order.

8. **Every reference to a lifted local resolves through its one handle.** Placement -- flat stack
   frame, inline coroutine frame, shared-owned cell -- is not source-semantic, and whichever it is,
   every read, write, reference and wait on the local reaches the same cell, so no two sites can see
   different backing.

9. **Compiler-generated retained cells and SystemVerilog class handles are distinct reference
   regimes.** A retained cell uses deterministic shared ownership (`PointerType{kShared}`, an
   acyclic DAG by construction); an SV class handle uses the managed, precisely-traced reference. A
   lifted local is never reached through the managed handle and a class handle is never reached
   through the shared one.

A later addition, when a real consumer requires it: an **`ErasedCallableType<Sig>`** for a
heterogeneous collection of closures of one signature. It is introduced with its explicit erasure
operation and its complete contract (invoke, ownership / destruction, copy / move), never through
implicit erasure of a concrete `ClosureType`.

## Boundary to Adjacent Layers

- **`mir.md`** owns the value/reference wrappers and the categories. This doc composes them: a
  closure is a `ClosureType`; a retained field is a `Shared<cell>`; an alias field is a `RefType`.
- **`callable.md`** owns the callable concept and the receiver rule. This doc owns the closure's
  captured fields; the closure body's receiver is the `ClosureType` value itself.
- **`binding_and_capture.md`** owns origin identity and capture forwarding. This doc states that the
  captured state is a `ClosureDecl` whose fields are keyed by those origins.
- **`object_model.md`** owns the nominal object. This doc states that the object shares only the
  field vocabulary with a closure.
- **`activation.md`** owns the runtime execution instance.
- **HIR-to-MIR** decides the capture form per source construct; LIR and the backend decide physical
  placement and target realization (lambda, functor, payload-plus-code-pointer).

## Forbidden Shapes

Each follows from an invariant above.

- One fused type for closure and lifted local, or a closure type with an `optional invoke` /
  `is_closure` flag. (Invariant 1.)
- A lifted local with a read, a write, a reference or a wait of its own beside the cell's.
  (Invariant 2.)
- A closure capture written as a write to the capture slot rather than through a captured `RefType`
  / `Shared<cell>`. (Invariant 3.)
- A reference-aggregate type category minted for lifted locals; reference is the wrapper. (Invariant
  4.)
- A closure-specific capture id space, a capture-read node distinct from field access, or a
  positional tuple standing in for the environment. (Invariant 5.)
- A field's emission-order position or physical offset used as its identity, or a canonical-ordering
  pass that rewrites the invoke body, or field_order used to reorder initializer evaluation.
  (Invariant 6, 7.)
- A lifted local some reference reaches in the frame while another reaches it through the handle.
  (Invariant 8.)
- A lifted local reached through the managed reference, or a class handle reached through the shared
  reference. (Invariant 9.)
- "`ClosureType` is a lambda" written as a MIR invariant. The lambda is one backend's realization;
  the invariant is capture-fields-plus-invoke.

## Notes / Examples

A fork branch that reads a lifetime-extended local and the enclosing object, in single-line lowered
form. `x` outlives the frame, so its cell is held through a shared handle; the branch is a closure
capturing that handle and the object pointer.

```text
SV (inside a method M):
  int x = 5;               // lifetime-extended: its cell is held via Shared<>
  fork  #1 out <= x;  join_none

entry     : x_cell = Shared<Var<int>>()                      // made empty with its block
decl      : initialize(*x_cell, 5)
closure   : ClosureType C_k { self: M*, x_cell: Shared<Var<int>> } + invoke
invoke    : load(*field_of(closure_receiver, x_cell))        // read x through its handle
```

The current C++ backend realizes the hold as a `shared_ptr` to the cell, made by `make_shared`; and
the closure as a struct holding its captures whose call operator is the invoke, reading each capture
through the closure the way the invoke's receiver does -- or, where the invoke is a coroutine,
started through a static function taking the closure by value, because the captures have to live in
the coroutine's frame past its first suspension. The execution backend holds the cell in a counted
hold the runtime makes, empty, for the domain the cell holds. That is each backend's realization,
not the MIR definition.

The three capture forms differ only by field type:

```text
snapshot capture : field type = int                  // a value taken at construction
live-place alias : field type = Ref<logic>           // an observable-cell alias
retained cell    : field type = Shared<Var<int>>     // retain a lifted local; reach it through this
```

A closure submitted to a heterogeneous region queue is erased at the submit site once that consumer
exists: `concrete ClosureType C_k --[ explicit erase ]--> ErasedCallableType< () -> void >`.
