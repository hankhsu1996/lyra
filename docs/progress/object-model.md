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

- [ ] Class inheritance: a single concrete base, `super` and the base-constructor call, and dynamic
      dispatch through logical method slots -- a method introduces, overrides, or finalizes a slot,
      and a virtual call names a receiver and a slot rather than a fixed callee. Pure-virtual /
      abstract classes. The override relation established earlier feeds slot inheritance.

- [ ] Interface-class conformance: a class may conform to several interfaces, a relation distinct
      from its single concrete base and carrying no second instance storage. Type-associated static
      storage and static methods, where a static method has no receiver. Parameterized-class
      specialization: a generic declaration plus, per specialization, a distinct materialized object
      record, its identity aligned with compilation-unit specialization (a stable id within a unit,
      a by-name canonical key across units) -- the same shape a parameterized module uses.

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
