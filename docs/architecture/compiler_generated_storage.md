# Compiler-Generated Storage

## Purpose

This doc owns the storage shape of the two compiler-synthesized entities that carry state: the
**environment** a closure value holds, and the **activation frame** that owns a callable's automatic
locals. Neither is a nominal SystemVerilog object (`object_model.md`) and neither is loose program
data (`mir.md`'s value-type system). They are the storage the lowering generates so a deferred or
concurrent body can reach the state it needs and so an automatic local can outlive the execution
that created it.

MIR's type system has one deliberate top-level split: value types versus reference types. This doc
places both compiler-generated entities on that split and keeps them there:

> A closure environment is a **value** aggregate. An activation frame is **reference** storage with
> identity. They sit on opposite sides of the value/reference spine and are never the same shape.

The conceptual peers are the same as the rest of MIR: a closure is code plus a captured environment
(C++ lambda, Rust closure), and a frame is the activation record that owns a call's locals (the
stack frame a language promotes to the heap when a closure outlives it).

## Owns

- The closure environment as a **value aggregate** -- a `TupleType` -- and the closure value as
  callable code plus that environment.
- The two type levels of a callable value: the **concrete closure value** (carrying its exact
  environment) and the **erased `Callable<Sig>`**, and the explicit erasure boundary between them.
- The three **capture forms** -- snapshot, live-place alias, retained-frame -- and the rule that the
  form is carried by the captured field's type.
- The **activation frame**: identity-bearing reference storage that owns a callable's automatic
  locals, and its shared-ownership retention model.
- The **reference-storage substrate** that the activation frame and the nominal object both
  specialize: ordered slots, slot types, and construction / destruction rules for identity-bearing
  field storage.

## Does Not Own

- Which binding a reference names, per-body materialization, and capture forwarding across closure
  boundaries. That is the lexical axis, owned by `binding_and_capture.md`. This doc owns the storage
  shape the captured fields land in; that doc owns which origin fills each field and how it is
  forwarded.
- The callable concept -- code versus value, the signature, the result type as call protocol. That
  is `callable.md`. This doc owns the environment half of a callable value and the value's type
  levels.
- The nominal object model -- methods, inheritance, dispatch, lifecycle, managed handles. That is
  `object_model.md`. The activation frame shares the object's reference-storage substrate but is not
  a nominal object.
- The value-type system, including the tuple as the one heterogeneous product type. That is
  `mir.md`. This doc uses the tuple as the closure environment; it does not define the tuple.
- The runtime execution instance -- the process / task / fork activation, its scheduler token, and
  its completion slot. That is `activation.md`. This doc owns the frame storage an execution
  instance owns or refers to, never the execution instance itself.
- Which capture form a SystemVerilog construct requires (snapshot a loop variable, alias a `ref`,
  retain a frame for a detached branch). That is HIR-to-MIR capture / lifetime policy
  (`lowering_boundaries.md`); this doc owns only the resulting storage facts.
- Physical placement -- a flat stack frame, an inline coroutine frame, a heap allocation, a
  shared-owned record -- and the realization of retention (intrusive refcount, runtime handle).
  Those are LIR and backend concerns.

## Core Invariants

1. **A closure value is callable code plus a value environment.** The environment is a `TupleType`,
   the one heterogeneous value product (`mir.md`); it is copied when the closure value is copied.
   _Consequence: a closure carries its captured state by value, like a C++ lambda or a Rust closure
   struct; there is no closure-specific storage universe beside the value-type system._

2. **The environment is the only place captured state lives, and a captured field is a value-product
   slot.** A body reads a capture through the ordinary value-aggregate projection over its
   environment, not through a closure-specific reference node or a closure-specific id space.
   _Consequence: the capture-only `CaptureId` / `CaptureRef` / capture-init shapes do not exist; a
   captured read is an aggregate slot projection, the same primitive every other product access
   uses._

3. **A captured field is a snapshot, a live-place alias, or a retained frame -- by its type.** A
   value-typed field is a snapshot taken at construction; a `RefType` field is a live alias of an
   observable cell (not a raw borrowed pointer, `reference-as-data-type.md`); a
   `Shared<ActivationFrame>` field plus a slot projection is a retained frame. _Consequence:
   snapshot-versus-alias-versus-retain is the field's type, never a separate capture-mode axis
   (`callable.md` invariant 5); owner retention (`Shared<ActivationFrame>`) and place aliasing
   (`RefType`) are different dimensions, not two spellings of one._

4. **A captured binding's identity is its origin; the slot index is a deterministic materialization
   of it.** The environment's slot order is a deterministic function of each captured binding's
   origin identity (`binding_and_capture.md`), so a dump is stable and self-explanatory and a
   forwarding / dedup keys off the origin. _Consequence: a slot index is a representation a backend
   lowers to, never the stable identity of "which binding was captured"; the identity lives in the
   origin, recovered for dump and diagnostics from there._

5. **A callable value has two type levels with an explicit erasure between them.** A concrete
   closure value's type carries its exact environment; an erased `Callable<Sig>` carries only the
   signature. A concrete value keeps its environment type until an explicit erasure produces the
   abstract one. _Consequence: a heterogeneous collection of callables (a region's deferred-effect
   queue) holds `Callable<Sig>`; a closure used at its construction site stays concrete; erasure
   never happens implicitly, so ownership and placement never become hidden magic._

6. **An erased `Callable<Sig>` carries a complete contract.** It states an invoke entry, an
   ownership / destruction contract for its held environment, and a copy / move contract.
   _Consequence: a backend realizes the erased callable from a stated contract, not from a
   convention it invents; the first backend may leave placement unoptimized, but the IR contract is
   whole._

7. **Every automatic local belongs to exactly one activation frame, which is its semantic owner.**
   Where the frame is placed -- a flat stack frame, an inline coroutine frame, a heap allocation, a
   shared-owned record -- is not a source-semantic fact. _Consequence: a non-retained frame keeps
   the ordinary, optimized placement; only a frame that something retains is materialized as
   shared-owned storage. "Local belongs to a frame" is a semantic statement, not a demand that every
   local be wrapped in an explicit aggregate._

8. **An activation frame is identity-bearing reference storage, retained per-frame.** It is reached
   through a pointer, never copied by value, and is not a value product and not a nominal object.
   Work that may outlive the owning execution retains the whole frame through a
   `Shared<ActivationFrame>` handle. _Consequence: an escaped automatic local is never individually
   boxed; the work retains the frame that already owns it (`lifetime-extended-automatic-scope.md`),
   and one retained handle is both the access path and the lifetime guarantee._

9. **The activation frame and the nominal object are two specializations of one reference-storage
   substrate.** The substrate is identity-bearing field storage -- ordered slots, slot types,
   construction / destruction. The frame adds shared-ownership and omits methods, inheritance,
   dispatch, and nominal identity; the object adds them. _Consequence: there is no second,
   independent object IR; a frame is the thin reference-storage specialization, an object the rich
   one, and they share the storage substrate they sit on (`object_model.md` invariant 1 stands)._

10. **Compiler-generated retained frames and SystemVerilog class handles are distinct reference
    regimes.** A retained frame uses deterministic shared ownership (`PointerType{kShared}`, an
    acyclic DAG by construction); an SV class handle uses the managed, precisely-traced reference
    (`object-model.md`). _Consequence: a frame is never reached through the managed handle and a
    class handle is never reached through the shared one; deterministic release and traced
    reclamation are not interchanged._

## Boundary to Adjacent Layers

- **`mir.md`** owns the `TupleType` (the one heterogeneous value product) and the reference types
  (`PointerType` with `kShared` / `kBorrowed` / `kUnique`, `RefType`, the managed reference). This
  doc composes them: a closure environment is a `TupleType`; a retained-frame field is a
  `Shared<ActivationFrame>`; an alias field is a `RefType`.
- **`callable.md`** owns the callable concept and the receiver rule. This doc owns the environment
  as a value aggregate and the two value type levels; the receiver of a closure body is its
  environment, reached as an ordinary receiver binding.
- **`binding_and_capture.md`** owns origin identity and capture forwarding. This doc states that the
  capture layout it materializes is a value `TupleType` whose slots are the deterministic
  materialization of those origins; the snapshot / alias / retain view is the slot's type.
- **`object_model.md`** owns the nominal object. This doc states that the object and the activation
  frame share one reference-storage substrate, and that the frame is the thin specialization without
  the object's behavior.
- **`activation.md`** owns the runtime execution instance. This doc owns the frame storage an
  execution instance owns or refers to; the execution instance is not the frame.
- **HIR-to-MIR** decides which capture form each source construct and binding requires and emits the
  corresponding field type; the storage facts this doc owns are its output. LIR and the backend
  decide physical placement and realize retention.

## Forbidden Shapes

- A closure-specific capture id space, or a capture-read node distinct from value-aggregate
  projection. Captures are environment-tuple slots. (Invariant 2.)
- A second value-product type minted for closure environments. There is one heterogeneous value
  product, the `TupleType`. (Invariant 1, `unpacked-struct-representation.md`.)
- A tuple slot index used as the stable identity of a captured binding. The identity is the binding
  origin; the index is its deterministic materialization. (Invariant 4.)
- `RefType` described or used as a plain borrowed pointer. It is the observable-cell reference and
  routes a write through the cell's update path. (Invariant 3, `reference-as-data-type.md`.)
- A live-place alias (`RefType`) captured across an execution boundary that may outlive the aliased
  storage. That case retains the owning frame instead. (Invariant 3, 8.)
- Per-variable boxing of an escaped automatic local. The owning frame is retained as a whole.
  (Invariant 8.)
- An activation frame modeled as a `mir::Class`, or as a value tuple. It is the thin
  reference-storage specialization. (Invariant 8, 9.)
- A second reference-storage IR independent of the object model's substrate -- frame storage and
  object storage growing parallel layout, projection, ownership, and lowering. They share one
  substrate. (Invariant 9.)
- Implicit erasure of a concrete closure to `Callable<Sig>`. Erasure is an explicit operation.
  (Invariant 5.)
- An activation frame reached through the managed (traced) reference, or an SV class handle reached
  through the shared frame reference. (Invariant 10.)
- Naming the storage owner of automatic locals an "activation". The activation is the runtime
  execution instance (`activation.md`); the storage owner is the activation frame. (Invariant 7,
  boundary.)

## Notes / Examples

A fork branch that reads an enclosing local, in single-line lowered form:

```text
SV:
  int x = 5;
  fork  $display(x);  join_none

closure environment : Tuple< M*, int >          // slot 0 = class self, slot 1 = x snapshot
build environment   : env = tuple( read self, read x )
read x in body      : tuple_get( env, slot_of(x) )      // slot_of is deterministic from x's origin
class member access  : member_access( deref( tuple_get(env, slot_of(self)) ), <member> )
closure value type   : concrete closure over Tuple<M*, int>, signature () -> Coroutine<void>
```

The same body's three capture forms differ only by field type:

```text
snapshot capture     : env slot type = int                        // a value
live-place alias     : env slot type = Ref<logic>                 // an observable-cell alias
retained-frame       : env slot type = Shared<ActivationFrame>    // retain the owner; read frame.slot(x)
```

A detached branch that must keep an enclosing automatic local alive does not box `x`. It retains the
frame that owns `x`:

```text
env slot type = Shared<ActivationFrame>
read x        = member_access( deref( frame_handle ), slot_of(x) )
```

The frame stays alive while any holder retains its handle, and dies when the last handle drops --
deterministic shared ownership, distinct from the traced reclamation an SV class handle uses.

A closure submitted to a heterogeneous region queue is erased at the submit site:

```text
concrete closure over Tuple<...>, signature () -> void
  --[ explicit erase ]-->  Callable< () -> void >
```

The queue holds `Callable< () -> void >` values of identical signature and differing environments;
each erased value carries its invoke, ownership / destruction, and copy / move contract.
