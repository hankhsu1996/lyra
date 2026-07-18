# Object Model

Build one generic object model and put SystemVerilog class support on it. Module / scope objects and
SystemVerilog classes (LRM 8) are the same generic nominal object type -- fields, methods,
inheritance, dynamic dispatch, construction -- differing only by which base they extend, which
reference reaches their instances, and which lifecycle they participate in. Done when the last item
lands: a SystemVerilog class with fields, `new`, single inheritance, dynamic dispatch, interface
conformance, static members, and parameterization is supported, and module / scope objects share the
same object model with no separate object IR.

The converged design (the contract and the resolved trade-offs) lives in `../architecture` and
`../decisions`; see Cross-references. Items here are the staged implementation; they say what shape
each stage establishes, not how.

## Sub-Steps

- [x] An object's base is one generic nominal-object reference -- a runtime-library, intra-unit, or
      cross-unit base -- resolved through one path, not a closed runtime-base classification. A
      runtime-library base is an imported library declaration whose name renders through the one
      type-mapping path; whether the object is a runtime tree node, whether it exposes a def-name,
      and its constructor prefix are facts stated on the base and read off it, never decided in the
      backend. Behavior-neutral: the emitted module shape is unchanged. (Only the runtime-library
      arm has a producer today; the intra-unit arm -- naming the registry's canonical local identity
      -- and the cross-unit by-name arm land with class inheritance, each adding one arm and one
      resolver case with no consumer change.)

- [x] Each post-construction lifecycle body (the scope's resolve / initialize / activate work) is an
      ordinary method that records, as a first-class fact, which runtime-base method it overrides --
      a resolved declaration reference, not a textual name. The per-phase special fields on the
      object declaration are gone. The full dynamic-dispatch slot machinery is not introduced here;
      only the override relation. Behavior-neutral.

- [x] Object construction is already one generic, type-directed form -- what it builds (a value, an
      owned child, a managed handle) follows the result type, and a module's owned children
      construct through it today -- so no separate construction node is needed. The gap this stage
      closes is the foundation every object-naming site depends on once classes arrive: a unit-level
      identity for each local nominal object, and one nominal-object reference used wherever a
      member's pointee, a constructed type, or a receiver names an object, so one relation is never
      encoded two ways. The base naming role is unified separately when an object extends a local or
      cross-unit base. A user-written constructor body lands with the class; base-constructor
      chaining lands with inheritance.

- [x] A compilation unit owns one canonical registry of its local nominal object declarations: every
      object type -- module, generate scope, class -- is one record with one canonical local
      identity a reference names; structural containment and backend emission nesting are separate
      relations over identities. A forward (incomplete) declaration is a record that exists before
      its body, so mutually-referential and forward-declared types resolve. The lexical-tree object
      storage is now registry-backed, with structural containment retained for construction and for
      nested emission. Resolving a source name to an identity by lexical scope lands with
      SystemVerilog classes, the first references resolved by name.

- [ ] Managed-object lifetime: a class object's liveness is reachability, and it is reclaimed by a
      precise tracing collector at runtime safepoints, with cyclic object graphs reclaimed by
      reachability. Every language-visible value that can survive a safepoint lives in a
      compiler-described, runtime-traceable activation frame reached through Lyra-visible runtime
      records, never in opaque backend execution state. The storage discipline (managed references,
      activation frames, traceable scheduler payloads, root registration, receiver rooting) may land
      before the collector algorithm, but class support is not complete until reclamation works -- a
      never-reclaiming intermediate is not a terminal state.

- [ ] A SystemVerilog class is the same generic object type, reached through a managed object
      reference: nullable as a value, identity-comparable, shallow-copied, participating in managed
      reachability, allocated by `new`, never explicitly freed -- distinct from an owning, a
      borrowed, and a shared reference. Its lifetime and reclamation are the managed-object-lifetime
      item above. Minimal surface: fields, `new`, direct instance-method call, handle equality
      against another handle and against null.

- [x] Class inheritance's structural relation: a single concrete base is a resolved reference on the
      derived class's declaration, and each own member (field, method) belongs to the derived's own
      arena while an inherited member is reached through the base's arena. A cross-class reference
      to a member is owner-qualified: it names the class arena the member lives in, so an inherited
      method or property is identified by its declaring class rather than by the receiver's runtime
      class.

- [x] Dynamic dispatch through logical method slots: a method introduces, overrides, or finalizes a
      slot as a fact stated on its declaration, and a virtual call site names a receiver and the
      slot's canonical identity rather than a fixed callee. A backend renders the dispatch from the
      stated slot; no consumer re-derives which method overrides which by matching names or
      signatures. A handle whose static type is a base but whose dynamic type is a derived resolves
      the call to the derived's implementation.

- [x] `super` reference and the base-constructor call: an override body reaches its base-class
      implementation by name-independent reference, and a constructor forwards to the base's
      construction as its first act (LRM 8.7). The super qualifier is stated at the call site as a
      call-side fact independent of the callee's virtual role; the base-constructor call is stated
      on the class's construction protocol, present whenever the class extends a base -- explicit
      when the source wrote `super.new(args)`, an empty-args implicit forward otherwise -- so a
      backend never resorts to its target language's default-construction convention.

- [x] Pure-virtual and abstract classes (LRM 8.21): a virtual method with no body is a contract the
      derived must fill, and a class carrying such a slot is not directly constructible. Each layer
      states the "declared, no source body" fact as a structural property of the method, orthogonal
      to whether the source separately wrote an empty body -- the two forms are semantically
      distinct and remain distinct end-to-end.

- [ ] Interface-class conformance (LRM 8.26): a class may conform to several interfaces, a relation
      distinct from its single concrete base and carrying no second instance storage. Each
      conformance is a pure-virtual method contract the class must satisfy; a shared behaviour among
      unrelated concrete hierarchies is expressed as conformance to one interface class, not as a
      shared base.

- [x] Parameterized-class specialization (LRM 8.25): a generic class plus, per distinct set of
      parameter bindings, a materialized class record. Matching specializations of one generic
      definition are the same type; distinct bindings produce distinct classes with per-
      specialization instance layout, static-property cells, and inheritance edge. Rides on the same
      identity mechanism a parameterized module uses -- generic-def name plus a canonical content
      encoding of the bindings.

- [ ] Cross-unit class ownership: a class is owned by the compilation unit that declares it, and a
      reference from another unit reaches it by name -- the same by-name resolution package
      callables already use. The class-copied-into-each-referring-unit shape (which today duplicates
      a package class into every unit that names it) is out. This is what lets LRM 8.25's
      package-scope rule -- matching specializations of a package generic class are one type
      throughout the system -- hold across compilation units, and it applies uniformly to
      non-parameterized and parameterized classes.

- [x] Type-associated static storage and static methods (LRM 8.9 / 8.10): a static property is one
      cell owned by the type, distinct from a per-instance field replicated on every object; a
      static method has no receiver and cannot be virtual. Each layer keeps the two categories in
      disjoint arenas -- an instance member and a type-associated one never share identity space --
      and the initializer of a static property runs once at design init (LRM 10.5), before any
      initial or always procedure, from a class-level body separate from the per-instance
      constructor.

## Managed-object lifetime: current implementation status

The managed-object lifetime sub-step above states the terminal semantic target (precise tracing with
cyclic reclamation). Current backend coverage:

- C++ backend: shared-ownership interim. Acyclic garbage is reclaimed as the last handle drops; a
  cycle of handles that becomes unreachable is not reclaimed. Sufficient to unblock class surface
  work whose semantics do not depend on cyclic reclamation, which covers every SV class feature
  planned in this workstream.
- LLVM / JIT backend: managed execution is not implemented; a program that constructs an SV class
  object is not lowerable through this path.

Precise-tracing storage discipline and collector are deferred until a driver appears (a workload
that hits cycle leaks in practice, or LLVM / JIT SV-class execution becoming a priority). The design
space explored while scoping this deferral surfaced:

- Runtime `Traceable` inheritance protocol is rejected: it would force every managed-carrying
  emitted type into a runtime vtable shape and violates the mechanical-translation contract.
- Extending the lifetime-extended-automatic-scope promoter to also spill managed locals is rejected:
  it entangles lexical retained scope with GC root publication, two separate concerns.
- Conservative scanning of native stack or coroutine frames is rejected by the object-lifetime
  contract (precise-tracing invariant).
- Candidate mechanisms for the terminal design include typed activation containers with
  compiler-emitted descriptors, LIR-level explicit safepoint edges, per-safepoint liveness maps,
  LLVM stackmap / statepoint integration, and coarser strategies that trace whole frames without
  per-safepoint liveness. The trade-off between precision and infrastructure cost has not been
  settled.

## Open Questions and Deferred Choices

- [ ] The runtime library still ships three scope subclasses (unit-instance, generate scope, named
      procedural block). MIR now names them uniformly by qualified string, but the runtime library
      has not yet been collapsed to one scope class with the def-name facet as an optional field. Do
      it when the runtime cadence allows; MIR-side consumers already treat the three as one.
- Nullability stays a value-level fact, not a type axis, until an analysis that reads it (e.g.
  static null-safety) exists.
- The reference-representation axis gains a managed kind whose name no longer reads as pure
  ownership; renaming that axis is deferred to when the managed kind lands.
- Renaming the object declaration to a name that reads as generic (it will hold both modules and
  SystemVerilog classes) is a separate mechanical change, deferred until it holds both. The registry
  slice is its natural home.
- The registry's canonical local identity is a dedicated object id, separate from the type-system id
  (resolved). The type pool interns by content, but a class declaration's identity must be
  independent of any one type node, so it carries its own id; an object type names that id.

## Cross-references

- `../architecture/object_model.md` -- the object-model contract: the one nominal-object reference,
  the override relation, the construction model, the reference kinds, and the registry (invariant
  10).
- `../architecture/object_lifetime.md` -- the managed-object lifetime contract: reachability,
  precise tracing, activation frames, safepoints, and roots.
- `../architecture/mir.md`, `../architecture/callable.md`, `../architecture/runtime_model.md`,
  `../architecture/elaboration_lifecycle.md` -- the contracts this work satisfies and that the
  object model doc is a peer to.
- `../decisions/object-model.md` -- the managed-reference and instance-method-receiver trade-offs.
- `../decisions/object-model-storage.md` -- the unit-wide registry, the single canonical identity,
  and the identity / lexical-scope / emission-nesting separation.
- `../decisions/unified-callable-model.md` -- the callable contract this rides on; virtual
  dispatch's forward-looking shape is settled there.
- `refactor.md` R47 (the object-model design that this workstream implements) and R8e (external
  callable and virtual dispatch, realized here).
