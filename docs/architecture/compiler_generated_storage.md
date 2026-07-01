# Compiler-Generated Storage

## Purpose

This doc owns the storage shape of the two compiler-synthesized entities that carry state: the
**closure record** a closure value is, and the **activation frame** that owns a callable's automatic
locals. Neither is a nominal SystemVerilog object (`object_model.md`) and neither is loose program
data (`mir.md`'s value-type system). They are the storage the lowering generates so a deferred or
concurrent body can reach the state it needs and so an automatic local can outlive the execution
that created it.

MIR's type system has one deliberate top-level split: value types versus reference types. This doc
places both compiler-generated entities on that split and keeps them there:

> A closure is a **value** record. An activation frame is **reference** storage with identity. They
> sit on opposite sides of the value/reference spine and are never the same shape.

The conceptual peers are the same as the rest of MIR: a closure is a value that bundles captured
state with a call operation (a C++ lambda closure object, a Rust anonymous closure struct), and a
frame is the activation record that owns a call's locals (the stack frame a language promotes to the
heap when a closure outlives it).

## Owns

- The **closure record**: a per-closure-site nominal value type carrying named capture fields plus
  exactly one invoke body, copied by value like any value.
- The two type levels of a callable value: the **concrete closure record** (carrying its exact
  capture fields) and the **erased `Callable<Sig>`**, and the explicit erasure boundary between
  them.
- The three **capture forms** -- snapshot, live-place alias, retained-frame -- and the rule that the
  form is carried by the captured field's type.
- The three-layer identity of a captured field: capture key, closure-record field id, and physical
  field layout, kept distinct.
- The **activation frame**: identity-bearing reference storage that owns a callable's automatic
  locals, and its shared-ownership retention model.
- The **field-storage vocabulary** (field declaration, field id, field access, layout finalization)
  that the closure record, the activation frame, and the nominal object share.

## Does Not Own

- Which binding a reference names, per-body materialization, and capture forwarding across closure
  boundaries. That is the lexical axis, owned by `binding_and_capture.md`. This doc owns the storage
  shape the captured fields land in; that doc owns which origin fills each field and how it is
  forwarded.
- The callable concept -- code versus value, the signature, the result type as call protocol. That
  is `callable.md`. This doc owns the captured-state half of a closure value and the value's type
  levels.
- The nominal object model -- methods, inheritance, dispatch, lifecycle, managed handles. That is
  `object_model.md`. The activation frame shares the object's reference-storage substrate but is not
  a nominal object, and the closure record is not an object at all.
- The value-type system, including the tuple as the one general heterogeneous product type. That is
  `mir.md`. A closure record is not a general product; it reuses field vocabulary, not the tuple.
- The runtime execution instance -- the process / task / fork activation, its scheduler token, and
  its completion slot. That is `activation.md`. This doc owns the frame storage an execution
  instance owns or refers to, never the execution instance itself.
- Which capture form a SystemVerilog construct requires (snapshot a loop variable, alias a `ref`,
  retain a frame for a detached branch). That is HIR-to-MIR capture / lifetime policy
  (`lowering_boundaries.md`); this doc owns only the resulting storage facts.
- Physical placement -- a flat stack frame, an inline coroutine frame, a heap allocation, a
  shared-owned record, physical field order -- and the realization of retention (intrusive refcount,
  runtime handle). Those are LIR and backend concerns.

## Core Invariants

1. **A closure value is a nominal value record.** A closure site declares a unique
   `ClosureRecordType`: named capture fields plus exactly one invoke body, value copy / move
   semantics, no inheritance, no dispatch, no heap requirement, no managed / GC handle, no
   source-visible identity. _Consequence: a closure carries its captured state by value, like a C++
   lambda closure object or a Rust closure struct; there is no closure-specific storage universe and
   no structural tuple standing in for the environment._

2. **A captured read is field access over the closure receiver.** A body reads a capture through
   ordinary field access on its closure receiver, by a stable field id -- not through a
   closure-specific reference node, a closure-specific id space, or a positional tuple projection.
   _Consequence: the capture-only `CaptureId` / `CaptureRef` / capture-init shapes do not exist, and
   neither does a `tuple_get` over an environment tuple; a captured read is the same field-access
   primitive an object member read uses._

3. **A captured field is a snapshot, a live-place alias, or a retained frame -- by its type.** A
   value-typed field is a snapshot taken at construction; a `RefType` field is a live alias of an
   observable cell (not a raw borrowed pointer, `reference-as-data-type.md`); a
   `Shared<ActivationFrame>` field plus a field projection is a retained frame. _Consequence:
   snapshot-versus-alias-versus-retain is the field's type, never a separate capture-mode axis
   (`callable.md` invariant 5); owner retention and place aliasing are different dimensions, not two
   spellings of one._

4. **A captured field's identity is not its physical layout position.** Three layers stay distinct:
   the **capture key** (a binding origin, `binding_and_capture.md`) identifies which binding is
   captured; the **closure-record field id** identifies the field within the record and is what the
   invoke body reads; the **physical field layout** is a representation a backend lowers to. The
   field id is assigned when the capture is discovered and is independent of physical order.
   _Consequence: the invoke body is emitted once, reading a capture by its stable field id;
   canonical ordering fixes only the physical layout (dump, generated-source field order,
   destruction order, any future ABI key) and never rewrites the body._ A fourth concept stays
   distinct from all three: the **initializer evaluation order** at the construction site. A field
   initializer is a pure read of an already-materialized capture value -- a capture source with an
   observable evaluation effect is sequenced into a local before the record is constructed -- so the
   physical layout order the initializers are emitted in is independent of the order the sources
   were evaluated in.

5. **The concrete record is directly callable; the erased level is a distinct, later addition.**
   Callability is a property resolved from the concrete callee type: a call reads its signature from
   the record's invoke, so a closure needs no separate callable type to be invoked. The concrete
   `ClosureRecordType` carrying the exact capture fields is the default and the only callable type
   that exists until a heterogeneous collection of callables (a region's deferred-effect queue)
   requires the erased `Callable<Sig>` -- which is then introduced together with its explicit
   erasure operation and a real consumer, never as a signature-only type no value inhabits.
   _Consequence: ownership and placement never become hidden magic (there is no implicit erasure),
   and the type system grows no dead erased surface ahead of its consumer._

6. **An erased `Callable<Sig>` carries a complete contract.** It states an invoke entry, an
   ownership / destruction contract for its held record, and a copy / move contract. _Consequence: a
   backend realizes the erased callable from a stated contract, not from a convention it invents._

7. **A closure record is not a general value product, and not a nominal object.** It is a
   compiler-generated, opaque, concrete callable type: one unique type per closure site (not
   structurally interned), exactly one entrypoint (no method lookup, overload, inheritance, or
   dispatch), field access only inside the compiler-generated invoke body and lowering (no
   source-level construction or destructuring). _Consequence: `TupleType` remains the only general
   heterogeneous value product (`mir.md`), and `mir::Class` remains the only nominal object; the
   closure record breaks neither invariant._

8. **Every automatic local belongs to exactly one activation frame, which is its semantic owner.**
   Where the frame is placed -- a flat stack frame, an inline coroutine frame, a heap allocation, a
   shared-owned record -- is not a source-semantic fact. _Consequence: a non-retained frame keeps
   the ordinary, optimized placement; only a frame that something retains is materialized as
   shared-owned storage._

9. **An activation frame is identity-bearing reference storage, retained per-frame.** It is reached
   through a pointer, never copied by value, and is not a value record and not a nominal object.
   Work that may outlive the owning execution retains the whole frame through a
   `Shared<ActivationFrame>` handle. _Consequence: an escaped automatic local is never individually
   boxed; the work retains the frame that already owns it (`lifetime-extended-automatic-scope.md`),
   and one retained handle is both the access path and the lifetime guarantee._

10. **The closure record, the activation frame, and the nominal object share one field-storage
    vocabulary, not one type.** They share field declaration, field id, field access, and layout
    finalization. They remain three distinct semantic categories -- a value record with one body
    (closure), thin reference storage with no behavior (frame), and a reference object with methods
    / dispatch / inheritance (`object_model.md`, which the frame specializes). _Consequence: there
    is no universal `Record` IR and no `mir::Class` with `is_closure` / `is_frame` flags; the shared
    vocabulary is narrow, and `object_model.md` invariant 1 (no second object IR) stands._

11. **Compiler-generated retained frames and SystemVerilog class handles are distinct reference
    regimes.** A retained frame uses deterministic shared ownership (`PointerType{kShared}`, an
    acyclic DAG by construction); an SV class handle uses the managed, precisely-traced reference
    (`object-model.md`). _Consequence: a frame is never reached through the managed handle and a
    class handle is never reached through the shared one._

## Boundary to Adjacent Layers

- **`mir.md`** owns the value/reference type system and the reference wrappers (`PointerType` with
  `kShared` / `kBorrowed` / `kUnique`, `RefType`, the managed reference), and `TupleType` as the one
  general value product. This doc composes them: a closure record is a value type with fields; a
  retained-frame field is a `Shared<ActivationFrame>`; an alias field is a `RefType`.
- **`callable.md`** owns the callable concept and the receiver rule. This doc owns the closure
  record's captured fields and the two value type levels; the receiver of a closure body is the
  closure record itself, reached as an ordinary receiver binding.
- **`binding_and_capture.md`** owns origin identity and capture forwarding. This doc states that the
  captured state it materializes is a closure record whose fields are keyed by those origins; the
  snapshot / alias / retain view is the field's type.
- **`object_model.md`** owns the nominal object. This doc states that the object and the activation
  frame share one reference-storage substrate, that the frame is the thin specialization without the
  object's behavior, and that the closure record is not an object -- it only shares field
  vocabulary.
- **`activation.md`** owns the runtime execution instance. This doc owns the frame storage an
  execution instance owns or refers to; the execution instance is not the frame.
- **HIR-to-MIR** decides which capture form each source construct and binding requires and emits the
  corresponding field type; the storage facts this doc owns are its output. LIR and the backend
  decide physical placement (including physical field order) and realize retention.

## Forbidden Shapes

- A closure-specific capture id space, or a capture-read node distinct from field access. Captures
  are closure-record fields read by field access. (Invariant 2.)
- A structural value tuple standing in for the closure environment, with captures as positional
  slots. Captures are named record fields; the closure record is not a `TupleType`. (Invariant 1,
  2.)
- A closure-record field's physical layout position used as its identity. The identity is the field
  id (keyed by the binding origin); the ordinal is its representation. (Invariant 4.)
- A canonical-ordering pass that rewrites the invoke body. Ordering fixes only physical layout; the
  body reads by stable field id. (Invariant 4.)
- A second general value-product type minted for closure environments, or a closure record usable as
  a general product (freely constructed, destructured, or substituted for a tuple). (Invariant 7,
  `mir.md`.)
- `RefType` described or used as a plain borrowed pointer. It is the observable-cell reference and
  routes a write through the cell's update path. (Invariant 3, `reference-as-data-type.md`.)
- Per-variable boxing of an escaped automatic local. The owning frame is retained as a whole.
  (Invariant 9.)
- A closure record modeled as a `mir::Class`, or an activation frame modeled as a `mir::Class` or as
  a value record. (Invariant 1, 7, 9.)
- A universal `Record` IR, or a `mir::Class` carrying `is_closure` / `is_frame` classification
  flags, fusing the three categories into one type. The shared vocabulary is narrow. (Invariant 10.)
- Implicit erasure of a concrete closure record to `Callable<Sig>`. Erasure is an explicit
  operation. (Invariant 5.)
- An activation frame reached through the managed (traced) reference, or an SV class handle reached
  through the shared frame reference. (Invariant 11.)
- Naming the storage owner of automatic locals an "activation". The activation is the runtime
  execution instance (`activation.md`); the storage owner is the activation frame.

## Notes / Examples

A fork branch that reads an enclosing local and the enclosing object, in single-line lowered form:

```text
SV (inside a method M):
  int x = 5;
  fork  $display(x); use(this.field);  join_none

closure record   : Closure_k { captured_self: M*, captured_x: int }   // per-site nominal value type
invoke body      : field_access(closure_receiver, field_of(x))        // by stable field id
member access     : member_access( deref( field_access(closure_receiver, field_of(self)) ), <member> )
construct value  : Closure_k{ captured_self = read self, captured_x = read x }
closure value type: concrete Closure_k; erase-> Callable< () -> Coroutine<void> >
```

Two receivers, kept distinct:

```text
closure receiver  = the Closure_k value itself (the invoke body's receiver)
captured self     = Closure_k.captured_self    (an ordinary field, the source-level `self`)
```

The same body's three capture forms differ only by field type:

```text
snapshot capture     : field type = int                        // a value
live-place alias     : field type = Ref<logic>                 // an observable-cell alias
retained-frame       : field type = Shared<ActivationFrame>    // retain the owner; read frame.slot(x)
```

A closure submitted to a heterogeneous region queue is erased at the submit site:

```text
concrete Closure_k  --[ explicit erase ]-->  Callable< () -> void >
```

The queue holds `Callable< () -> void >` values of identical signature and differing concrete
records; each erased value carries its invoke, ownership / destruction, and copy / move contract.

A detached branch that must keep an enclosing automatic local alive does not box the local. It
retains the frame that owns it, held as a `Shared<ActivationFrame>` field of the closure record:

```text
Closure_k { retained_frame: Shared<ActivationFrame>, ... }
read x = member_access( deref( field_access(closure_receiver, field_of(retained_frame)) ), slot_of(x) )
```

The frame stays alive while any holder retains its handle, and dies when the last handle drops --
deterministic shared ownership, distinct from the traced reclamation an SV class handle uses.
