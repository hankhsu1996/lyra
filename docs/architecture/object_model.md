# Object Model

## Purpose

The object model is MIR's one shape for a nominal object type. A module instance, a generate scope,
and a SystemVerilog class are the same kind of entity: a named type with fields, methods, a base
lineage, and a construction protocol. They are not three object systems sharing some helpers; they
are one object system whose members differ only in which base they extend, which reference reaches
their instances, and which lifecycle they participate in.

The model is generic object-oriented -- its peers are the class systems of C++, Rust, and Python --
not a SystemVerilog construct and not a C++ class. SystemVerilog `class`, `extends`, `virtual`,
`interface class`, and `new` flow in through HIR and are translated into this generic vocabulary at
HIR-to-MIR; a module read as a class (see `runtime_model.md`) is the same translation from the other
direction. The governing thesis:

> One nominal-object reference, one generic declaration-level override relation, and one
> construction model. Compilation-unit boundaries determine how an object type's identity is
> represented, never its semantic category.

## Owns

- The nominal object type declaration: a name, instance fields, instance methods, an optional single
  concrete base, a set of interface conformances, and an associated namespace of type-level members.
- The nominal object reference: how one object type names another -- as a base, a field's pointee, a
  receiver type, a constructed type -- carrying identity whose representation follows the
  compilation-unit boundary.
- The reference kinds that reach an object instance and the lifetime / reachability each denotes:
  owning, borrowed, shared, and managed.
- The construction model: allocation, the constructor, and optional base-constructor chaining, as a
  concept distinct from an ordinary callable.
- The dispatch model: the override relation between a method and the base method it overrides, the
  logical dispatch slot, and the virtual and interface call forms.
- The receiver rule: an instance method carries a receiver; a type-associated (static) function does
  not.
- Type-associated storage and type-associated functions (static properties and static methods).
- Runtime-tree participation as a base-lineage fact: an object that extends the runtime scope base
  is a node in the runtime object tree and participates in the elaboration lifecycle.
- The unit-wide registry of local nominal object declarations, the single canonical local identity
  of each, and the separation of that identity from lexical name resolution and from backend
  emission nesting.

## Does Not Own

- The value-type system (integral, real, string, aggregates) and the non-object wrappers. Those are
  `mir.md`. The object model is the object half of MIR's type system; the value half stays there.
- The callable concept -- callable code versus value, the result type as call protocol, the
  environment and capture model. That is `callable.md`. Methods and constructors are callables; the
  object model uses them and does not redefine them.
- The ordering of the elaboration phases (build, resolve, initialize, activate) and when each runs.
  That is `elaboration_lifecycle.md`. The object model states that the lifecycle bodies are method
  overrides; that doc states when they execute.
- The runtime object tree's shape and its faithfulness to elaboration. That is
  `hierarchy_and_generate.md`.
- When and into what a cross-unit reference resolves, and the by-name mechanism. That is
  `reference_resolution.md` and `emission_model.md`. The object model states that a cross-unit
  object reference is by-name; those docs own the resolution.
- Physical layout: object storage layout, dispatch-table layout, frame allocation, and the
  realization of managed reachability (reference counting versus tracing collection). Those are LIR
  and the runtime.

## Core Invariants

1. **One nominal object type.** A module instance, a generate scope, and a SystemVerilog class are
   one declaration kind -- name, instance fields, instance methods, an optional concrete base,
   interface conformances, and an associated type-level namespace. There is no second object IR and
   no flag classifying a declaration as a module versus a class. _Object-model consequence: the
   differences between these objects are expressed through generic mechanisms -- which base, which
   reference, which lifecycle -- never through a source-category discriminator on the declaration._

2. **One nominal-object reference; identity representation follows the unit boundary.** Every place
   one object type names another uses one reference abstraction. Its identity is a stable
   compiler-owned id when the target is intra-unit, a by-name reference resolved against an imported
   interface when the target is in another compilation unit, and an imported library declaration
   when the target is a runtime-library type. Intra-unit and cross-unit identities never share a key
   space. _Object-model consequence: a consumer resolves any object reference through one entry
   point; the local-versus-external split lives in the resolver, not in three incompatible reference
   forms scattered across consumers._

3. **Inheritance is one concrete base plus a set of interface conformances.** The concrete base
   determines instance layout and constructor chaining; an interface conformance is a method
   contract that introduces no instance storage. These are two distinct relations, not one list of
   bases. _Object-model consequence: a class may conform to several interfaces while extending at
   most one concrete base; the two relations are represented separately because they carry different
   facts._

4. **The reference reaching an instance carries its lifetime, orthogonal to the object's category.**
   The kinds are owning (one owner controls the lifetime), borrowed (refers, owns nothing), shared
   (a reference-counted, acyclic-by-construction owner), and managed (a language-managed object
   handle: null is a legal value, identity is comparable, copies are shallow, the object is
   reachable while any handle reaches it, it is created by construction, and it is never explicitly
   freed). A SystemVerilog class handle is a managed reference, distinct from a shared one. Whether
   an object is a node in the runtime tree is read from its base lineage, never from the reference
   kind that reaches it. _Object-model consequence: topology, lifetime, and category are three axes;
   collapsing any two -- "a unique reference means a tree child", "a class handle is a shared
   pointer" -- loses a distinction a backend must read._

5. **Construction is its own concept.** Allocating an object and running its constructor, with
   optional base-constructor chaining and a defined initialization ordering, is a construction form
   distinct from an ordinary callable. A constructor body may reuse callable-body machinery, but a
   construction is never represented as a plain call to an ordinary method, and base-constructor
   ordering is stated, never left to a backend convention. _Object-model consequence: every object
   -- an owned module child and a heap class object alike -- is built through the one construction
   form._

6. **Override is a resolved relation; dispatch is by logical slot.** A method that overrides a base
   method records the overridden method as a resolved declaration reference, never a textual name. A
   dynamic call names a receiver and a logical dispatch slot; a method introduces, overrides, or
   finalizes a slot. The slot is a logical identity, not a physical table index. _Object-model
   consequence: a backend reads the override family and the dispatch target from stated structure;
   it never re-derives "which method does this override" or "is this call virtual" by matching
   names, signatures, or receiver types._

7. **The receiver is a property of instance methods only.** An instance method's first binding is
   its receiver, the pointer to the enclosing object, reached uniformly by every member access. A
   type-associated function has no receiver. _Object-model consequence: a static method is an
   associated function called without an instance; no fabricated receiver stands in for "no
   object"._

8. **Type-associated state is not an instance member.** A static property and a static method belong
   to the type's associated namespace, shared across instances, not to each instance's field or
   method set. The instance member is a (name, type) pair; type-associated storage is a separate
   category. _Object-model consequence: a static property is one cell owned by the type, never a
   field replicated into every instance._

9. **Managed reachability is the contract; its realization is not.** The model commits to the
   semantics that a managed object lives while any handle reaches it. Whether a backend realizes
   that by reference counting or tracing collection, and whether cyclic garbage is reclaimed, are
   realization choices outside the contract. _Object-model consequence: MIR states "managed
   reachability" and a backend chooses the mechanism; MIR never states "reference counted" as the
   meaning._

10. **A compilation unit owns one canonical registry of its local nominal object declarations, and
    identity, lexical name resolution, and emission nesting are three separate relations.** Every
    object type -- a module, a generate scope, a class -- is one registry record with one canonical
    local identity, and a reference names that identity. A lexical scope resolves a source name to
    an identity; the registry is not a flat global name table. Structural containment (which object
    builds or owns which) and a backend's emission nesting are separate relations over identities,
    neither of which is the identity. A forward (incomplete) declaration is a registry record that
    exists before its body, so mutually-referential and forward-declared types resolve against a
    stable identity. _Object-model consequence: declarations are owned and identified once,
    uniformly; lexical position, name, and emission layout are properties read off that identity,
    never substitutes for it, and one storage serves every object type._

## Boundary to Adjacent Layers

- **Peer to `mir.md` and `callable.md`.** `mir.md` owns the overall MIR shape and the value-type
  system; this doc owns the object model within it and is the canonical elaboration of MIR's object
  types. `callable.md` owns the callable concept that methods and constructors are; its receiver
  rule is the instance-method case of invariant 7, and a type-associated function is the case with
  no receiver.
- **HIR carries the source vocabulary.** `class`, `extends`, `virtual`, `interface class`, and `new`
  survive verbatim in HIR (`hir.md`); HIR-to-MIR translates each into this generic object model.
- **`elaboration_lifecycle.md` owns when the lifecycle runs.** This doc owns that the
  post-construction lifecycle bodies are method overrides of the runtime base; that doc owns their
  phase ordering.
- **`reference_resolution.md` and `emission_model.md` own cross-unit resolution.** This doc owns
  that a cross-unit object reference is by-name against an imported interface; those docs own when
  and how it resolves.
- **LIR owns physical realization.** Object layout, dispatch-table layout, and the
  managed-reachability mechanism are introduced below MIR.

## Forbidden Shapes

- A second object IR, or a flag on a declaration classifying it as a module versus a class. The
  category is expressed through base, reference, and lifecycle. (Invariant 1.)
- A global object-id space, or a cross-unit object reference that names another unit's internal id
  rather than its public name. (Invariant 2.)
- One flat list of bases mixing the concrete base with interface conformances; an interface
  conformance that introduces instance storage. (Invariant 3.)
- Deriving runtime-tree membership from a reference's ownership kind. (Invariant 4.)
- Reaching a SystemVerilog class handle through the shared (reference-counted,
  acyclic-by-construction) reference instead of the managed reference. (Invariant 4.)
- Representing construction as a plain call to an ordinary method, or leaving base-constructor
  ordering to a backend convention. (Invariant 5.)
- An override recorded as a textual method name; a backend re-deriving the override family by name
  or signature matching; whether a call is dynamic inferred from the receiver's type rather than
  stated at the call site. (Invariant 6.)
- A type-associated function carrying a receiver, or a fabricated receiver standing in for "no
  instance". (Invariant 7.)
- A static property modeled as an instance field replicated per object. (Invariant 8.)
- MIR stating a reference-counted realization, or the absence of cyclic reclamation, as the meaning
  of a managed reference rather than as a realization detail. (Invariant 9.)
- Two declaration-storage systems -- one for module and generate-scope objects, another for classes.
  There is one registry. (Invariant 10.)
- An object's lexical position or its emission nesting used as its identity; a second local identity
  inter-convertible with the first. (Invariant 10.)
- A backend-emission nesting parent stored on the generic object declaration; it is a backend
  relation derived from structural containment, not a field of the declaration. (Invariant 10.)

## Notes / Examples

A module instance, a generate scope, and a SystemVerilog class differ only along the three generic
axes:

- A module instance extends the runtime instance base, participates in the elaboration lifecycle,
  and is reached from its parent by an owning reference.
- A generate scope extends the runtime generate-scope base, participates in the same lifecycle, and
  is likewise an owning child.
- A SystemVerilog class extends another class or no class, is reached by a managed reference, and is
  built by `new`.

The same field, method, override, and construction machinery serves all three; none has a private
object system.

The post-construction lifecycle bodies are overrides of the runtime base's lifecycle methods,
through the same override relation a SystemVerilog `virtual` method uses. There is one override
machinery: the lifecycle is its first user, a user-defined virtual method is another, and a backend
renders both the same way.

A static method is an associated function under the type, invoked without an instance. A static
property is a single cell the type owns, observed identically from every instance. Neither is part
of an instance's member set, so the instance-member (name, type) rule is never bent to accommodate
them.
