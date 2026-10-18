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
- The closure's **kind** -- what its invoke does to it -- and the **erased callable** a closure
  becomes when it is handed to the runtime, with the cast that makes one.
- The **ownership** of a closure value: its one owner, and what ending it does.
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
- The value-type system and the observable cell a variable is (`mir.md`). Every variable a body that
  can wait declares is such a cell.
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

3. **A closure's invoke reaches its captures through its receiver, and the receiver's type is the
   closure's kind.** The receiver is the invoke body's `locals[0]`, and a captured read is field
   access over it. It is one of three, as rustc's closure kinds are: a read-only borrow of the
   closure (`Fn`), which only reads its captures; a mutable borrow (`FnMut`), which also writes
   them, carrying state of its own from one call to the next; or the closure's box (`FnOnce`), which
   the body takes over and ends when it ends -- a body started as a coroutine, which outlives the
   call. A write to a variable the closure did not make is a write through a captured
   `VariableRefType` or a captured `Shared<cell>`, never a write to the capture slot.

4. **Value-versus-reference for a lifted local is the wrapper, not a bespoke type.** The cell is
   reached through a `Shared<>` handle; the reference semantics are the ordinary MIR wrapper, and
   dereferencing the handle names the cell. The handle is made with the cell, empty, the way
   `make_shared<T>()` makes one, and the local's declaration then initializes the cell.

5. **A captured field is a snapshot, a live-place alias, or a retained cell -- by its type.** A
   value-typed field is a snapshot; a `VariableRefType` field is a live alias of an observable cell;
   a `Shared<cell>` field retains a lifted local. The capture form is the field's type, never a
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

10. **A closure handed to the runtime is held as an erased callable of its kind and its signature,
    and the entry taking it states what it asks of it, how long it keeps it, and the signature it
    calls it at.** The runtime is compiled before the program and cannot name a `ClosureType`, so
    what it takes is an `ErasedCallableType`: the closure's kind of invariant 3, then the parameters
    and the result, all machine types -- as a rustc `dyn FnMut(A) -> R` names its trait in its type.
    It is reached only through a box, which owns the closure, or a borrow, which does not. A box or
    a borrow of a concrete closure becomes one by a cast to the box or borrow of the erased type of
    the same kind and signature. The cast moves nothing, allocates nothing and checks nothing: the
    closure stays where it was made, and what changes is only what its holder knows of its type.
    Until that cast a box of a concrete closure is an ordinary box, ended directly as any box of a
    known type is. What can be done to an erased callable is to call it, and, through its box, to
    end it. Which kind the holder asks for -- calling it many times through a shared view, many
    times as its only holder, or once -- with the signature it calls it at, and how long it keeps it
    (`scheduling.md`), are the bounds a Rust callee states on its callable parameter
    (`F: FnMut(bool) -> bool + 'static`), so the runtime entry's own declaration states them, once,
    and the unit's verification holds what is handed to it to them, reading the kind and the
    signature off the erased type handed. A body the entry starts rather than calls has no
    signature, and the declaration says it is started instead.

11. **A closure value has exactly one owner at every moment, and every way out of that owner ends it
    exactly once.** The owner is the frame that made it, the box it was made in, or whatever took
    that box over -- a region queue, a wait, a timer, a started body. Ending a closure ends its
    captures; ending the box it was made in does that and then frees the storage. Either happens
    because its owner ends, by whatever path: having run, having been dropped unrun, a stopped
    execution, the end of the run. A closure is never ended because it ran, and never shared between
    owners.

## Boundary to Adjacent Layers

- **`mir.md`** owns the value/reference wrappers and the categories. This doc composes them: a
  closure is a `ClosureType`; a retained field is a `Shared<cell>`; an alias field is a
  `VariableRefType`.
- **`callable.md`** owns the callable concept and the receiver rule. This doc owns the closure's
  captured fields; the closure body's receiver is a borrow of the `ClosureType` value or its box, by
  its kind (invariant 3).
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
- A write to a variable the closure did not make, written as a write to a capture slot rather than
  through a captured `VariableRefType` / `Shared<cell>`. (Invariant 3.)
- A closure's kind stated anywhere but its invoke's receiver type, or what the runtime asks of a
  closure stated anywhere but the entry taking it -- each site handing one over then answers for the
  entry, and the runtime calls a body that writes its captures through a path that only lends them,
  or calls twice a body that takes its box, wherever one site answered wrongly. (Invariants 3, 10.)
- An erasure that is not a cast of a box or a borrow: one that copies the closure, wraps it in a new
  allocation, or hands ownership over as part of converting it. Each is an operation, and the
  program's MIR would not say it happened. (Invariant 10.)
- A closure held by two owners -- the runtime sharing one box between holders -- or ended because it
  ran rather than because its owner ended. (Invariant 11.)
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
form. `x` outlives the frame, so its cell is held through a shared handle. The object lives while
referenced, so the method holds a share of it from where it begins (LRM 8.29); the branch is a
closure capturing that handle and a copy of the share, and it reaches the object through the share
rather than through a pointer of its own.

```text
SV (inside a method of class M):
  int x = 5;               // lifetime-extended: its cell is held via Shared<>
  fork  #1 out <= x;  join_none

entry     : self_share = share_of(*self)                     // the method's hold on its object
entry     : x_cell = Shared<Var<int>>()                      // made empty with its block
decl      : initialize(*x_cell, 5)
closure   : ClosureType C_k { self_share: Managed<M>, x_cell: Shared<Var<int>> } + invoke
invoke    : self = &*field_of(closure_receiver, self_share)  // the object, through the share
invoke    : load(*field_of(closure_receiver, x_cell))        // read x through its handle
```

The current C++ backend realizes the hold as a `shared_ptr` to the cell, made by `make_shared`; and
the closure as a struct extending the runtime's closure part, holding its captures, whose call
operator is the invoke, reading each capture through the closure the way the invoke's receiver does
-- or, where the invoke is a coroutine, started through a static function taking the closure's box
over, because the captures have to live as long as the execution. The execution backend holds the
cell in a counted hold the runtime makes, empty, for the domain the cell holds. That is each
backend's realization, not the MIR definition.

The three capture forms differ only by field type:

```text
snapshot capture : field type = int                  // a value taken at construction
live-place alias : field type = VariableRef<logic>   // an observable-cell alias
retained cell    : field type = Shared<Var<int>>     // retain a lifted local; reach it through this
```

A closure submitted to a region queue is made in a box and erased where it is submitted, and a
sort's order is erased as a borrow for the call:

```text
deferred effect : Box<C_k>  --[ cast ]--> Box<dyn () -> void>        // the region owns it, asks FnOnce
watch           : Box<C_w>  --[ cast ]--> Box<dyn (bool) -> bool>    // the wait owns it, asks FnMut
sort order      : &C_s      --[ cast ]--> &dyn (i64, i64) -> bool    // lent for the call, asks Fn
```

The erased type names no kind; what each holder asks is stated by the entry it is handed to.

In the current implementation both backends realize the erased callable the same way: the closure's
storage starts with a pointer to a constant table its unit emits, the entry ending the closure and
then the one call its body is, at its signature, followed by the captures -- the shape of a rustc
vtable, which leads with the drop and carries the methods after it, and of a `dyn FnMut(A) -> R`
one, whose one method is the call. A body that is started rather than called has the ending entry
alone. No entry is chosen: the table's type is its signature, so a holder calls the one call at the
signature it was handed the closure at. The runtime reaches every closure through that table, so
neither backend's own callable wrapper crosses into it. Both realize the two casts the same way too:
the box cast hands the same storage on as a box ended through the table, as rustc's unsizing
coercion makes a `Box<dyn Fn>` of a `Box<T>`; and the borrow cast is made through the closure's
table, giving the library's one generic borrowed callable of that signature, a pointer to the
closure beside the entry to call.
