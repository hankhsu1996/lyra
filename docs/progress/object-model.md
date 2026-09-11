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
      call-side fact independent of the callee's virtual role; the forward happens wherever the
      class extends a base -- carrying the arguments the source wrote, and none where it wrote no
      `super.new` -- so a backend never resorts to its target language's default-construction
      convention.

- [x] Pure-virtual and abstract classes (LRM 8.21): a virtual method with no body is a contract the
      derived must fill, and a class carrying such a slot is not directly constructible. Each layer
      states the "declared, no source body" fact as a structural property of the method, orthogonal
      to whether the source separately wrote an empty body -- the two forms are semantically
      distinct and remain distinct end-to-end.

- [x] Interface-class conformance (LRM 8.26): a class may conform to several interfaces, a relation
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

- [x] Cross-unit class ownership: a class is owned by the compilation unit that declares it, and a
      reference from another unit reaches it by name -- the same by-name resolution package
      callables already use. The class-copied-into-each-referring-unit shape is out. This is what
      lets LRM 8.25's package-scope rule -- matching specializations of a package generic class are
      one type throughout the system -- hold across compilation units, and it applies uniformly to
      non-parameterized and parameterized classes. Bringing one into existence is the same
      construction as any other: the object is allocated against the class's declaration and its
      constructor runs on it, both reached by the name the declaring unit links them under, so a
      property takes its declared initial value and a constructor takes its arguments whichever unit
      declared the class. A class declared elsewhere that extends one enters its base's construction
      the same way, and a class extending another states the complete argument list that
      construction carries however the source arrived at it -- an explicit `super.new`, or the
      arguments written on the extends specifier (LRM 8.17). Settled in
      `../decisions/constructing-another-units-class.md`.

- [x] A class's declaration scope reaches its compiled identity. A class declared inside a module, a
      generate scope, a package, or the compilation-unit scope (LRM 8.1, 27.6, 26, 3.12.1) is a
      distinct type per declaration scope, and SystemVerilog leans on that scoping for uniqueness:
      two modules, two sibling generate scopes of one module, or two packages may each declare a
      class of the same name. A unit holds every class it declares in one flat name space, so the
      identifier a class carries has to be unique there, and the declaring scope is what makes it
      so. Which unit declares a class travels with every reference to it, so a name bound to one and
      a name bound to another are two bindings wherever a type is chosen, including where a type
      parameter selects between them.

- [x] A class body reaches the declarations of the scope that declares it (LRM 23.9, 6.22). A class
      is a scope of the name tree, so a method of one declared inside a module, an interface, or a
      generate block reads and writes that scope's variables and calls its subroutines by name, the
      way a task of the scope does. Which copy it reaches follows from each instance with a type
      declared inside it creating a unique type: an object belongs to the one instance it was
      created in, records it, and reaches that instance and no other -- so two instances of one
      module carry two unrelated sets of objects. Construction is where the instance is supplied.
      Constructing such a class from another compilation unit is refused, since what crosses a unit
      boundary is that unit's signature and no instance of a scope inside it is on one.

- [x] Type-associated storage of a class a structural scope declares is one cell per instance of
      that scope, not one for the program: the static property, the static-lifetime locals of its
      methods, and the target a `disable` naming one of its blocks invalidates all follow what
      replicates the class declaration. Their initializers run where that instance brings up its own
      storage -- once per instance, before any process. A receiver-less method of such a class is
      handed the instance, having no object to reach it through, and a derived class forwards it to
      its base. Still open: a derived class records the instance a second time rather than reading
      the one its base already holds; both answer alike, so this costs a word per object.

- [x] Type-associated static storage and static methods (LRM 8.9 / 8.10): a static property is one
      cell owned by the type, distinct from a per-instance field replicated on every object; a
      static method has no receiver and cannot be virtual. Each layer keeps the two categories in
      disjoint arenas -- an instance member and a type-associated one never share identity space --
      and the initializer of a static property runs once per cell, before any initial or always
      procedure (LRM 10.5), rather than on each construction. What runs it is whatever brings up the
      thing that replicates the class declaration, so such a class needs no startup moment of its
      own; a property the source left without a value takes its type's default at that same moment
      rather than whatever its storage was born as. Because a static property needs no receiver, a
      name reaches it from outside every body of the class and it reads from a structural expression
      (a continuous-assignment right-hand side), which re-evaluates when the cell changes -- which
      is what makes it observable storage where an instance property, having no receiver to reach it
      through, is a plain value.

- [x] A static-lifetime local of a method (LRM 6.21): a variable a method declares `static` belongs
      to the class rather than to an object of it, so every call reaches the same cell however many
      objects exist, and its initializer is applied once per cell before any process starts rather
      than on each construction. How many cells that is follows the class's own replication, so a
      class a namespace unit declares keeps one and a class a structural scope declares keeps one
      per instance of that scope. A static method's own is the same cell, and two sibling blocks
      that each declare a static under one name still get one cell each. The method itself stays
      automatic (LRM 8.6), which governs its ordinary locals and not this one.

- [x] Class-method argument directions beyond `input` (LRM 13.5): an instance method and a static
      method carry an `output`, `inout`, `ref`, or `const ref` formal back to the caller's actual in
      every position the LRM allows, through a direct call and through a virtual override alike. A
      class property is legal as a ref actual and as an output one, so one object may be handed
      another's property to read or to write. One satisfier is still rejected: an interface class
      contract that a class meets by inheriting a base implementation rather than defining one,
      where the forwarding this needs would have to hand its own completion whatever the forwarded
      call's carried back (LRM 8.26.2).

- [x] Class task methods (LRM 8.6, 13.3): a `task` declared in a class body, in its instance,
      static, virtual and pure-virtual prototype forms. Enabling one suspends the enabler until it
      completes and carries its output formals back at that moment; the method is always automatic
      (LRM 13.3.1), so two activations running at once keep their own formals and locals while
      reaching one object through the receiver each was enabled on.

- [x] Out-of-block method definitions (LRM 8.24): a method declared as an `extern` prototype in the
      class body and defined outside it (`function ClassName::m; ... endfunction`), for a function
      and a task alike, with the prototype carrying the qualifiers and specifiers the definition
      drops. The body reaches every declaration of the class, a virtual prototype defined out of
      block still dispatches to the override a derived class defines the same way, and a return type
      the class declares is named through the class scope.

- [x] The `this` keyword (LRM 8.11): a property of the current instance -- one replicated per object
      or one the class owns -- a value parameter, or a method of it is named through `this`, in an
      instance method, a constructor, and a property initializer alike, and `type(this)` names its
      type. The qualified spelling names what the unqualified one names, so it reaches a property
      inherited from a base class and survives a suspension the same way, and where a subroutine
      argument or a local declaration shadows a property it is what still reaches the property. Used
      as a value in its own right the keyword yields the handle referring to the invoking object, so
      returning it, passing it, and comparing it all name the object the caller already holds a
      handle to. Where a handle is realized as a shared owner rather than as a traced pointer, an
      object can answer with one only by recording which owner refers to it, so every object carries
      that record; under a traced realization the record is unnecessary, because the handle is the
      pointer the body already holds.

- [ ] Constructor argument directions (LRM 8.7): a constructor declared with an `output`, `inout`,
      or `ref` formal is rejected. The standard gives it the argument conventions of any other
      subroutine call, but a construction yields the object it built and carries nothing a value
      could travel back to the caller in, so this waits on the construction model rather than on the
      method surface.

## Managed-object lifetime: current implementation status

The managed-object lifetime sub-step above states the terminal semantic target (precise tracing with
cyclic reclamation). Current backend coverage:

Both backends reclaim by shared ownership today, and both leak an unreachable cycle. What differs is
what a handle is made of, and the difference is worth stating because it is the shape the terminal
model has to reach on both.

- C++ backend: the handle is parameterized by the class it refers to, so a reference carries the
  static class in its representation and one is unspellable where that class has no name. Acyclic
  garbage is reclaimed as the last handle drops. Sufficient to unblock class surface work whose
  semantics do not depend on cyclic reclamation, which covers every SV class feature planned in this
  workstream.
- LLVM / JIT backend: the handle is one representation for every object a handle can name, with the
  object's own type erased and its class carried by the object rather than by the reference. This is
  sound because SystemVerilog classes are singly inherited (LRM 8.13), so a handle to a base and a
  handle to the derived object are the same address. Constructing an object, reaching a property,
  and dispatching a behavior all work; what refuses is the operation that recovers a handle from the
  object a body runs on (LRM 8.11 `this`), which a shared-owner realization needs and a traced one
  does not.

Nothing above is visible to any IR: no rule in HIR, MIR, LIR, or lowering states a reference count,
a shared owner, or a control block. The one exception is that recovery operation, which exists at
MIR level only because a body holds a borrowed pointer to its object and has to get a handle back
from it; under tracing the handle is what the body already holds, so the operation goes away with
the interim rather than being ported to it.

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

## Conformance gaps the corpus records

Behaviour the corpus asks for and does not get. Each is held by a check the corpus keeps rather than
deletes -- commented out where the rest of its case still runs, and the whole case parked where it
cannot run at all. Restoring either is manual: nothing detects that the behaviour became right, so
this list is what remembers.

- [ ] A base whose constructor declares a formal with a default value is refused. A derived class
      that writes no `super.new` and no arguments on its extends specifier forwards to its base with
      the arguments the base declared as defaults (LRM 8.7, 8.17), and nothing computes them. The
      refusal names the construct and is the same whichever unit declares the base. What it waits on
      is a default value crossing into the scope that needs it: the expression is written in the
      declaring class's own scope, so a class extending it cannot simply lower it in its own.

## Open Questions and Deferred Choices

- [ ] The runtime library still ships three scope subclasses (unit-instance, generate scope, named
      procedural block). MIR now names them uniformly by qualified string, but the runtime library
      has not yet been collapsed to one scope class with the def-name facet as an optional field. Do
      it when the runtime cadence allows; MIR-side consumers already treat the three as one.

- [x] A reference to an object whose class another design element declares. Such a class is a
      distinct type per instance of that element (LRM 6.22) and is nameable only inside the scope
      declaring it (LRM 23.9), so a referrer outside has no name for it and none could be published.
      The reference is still a complete static type stating that values of it are objects, and every
      operation that reads nothing the class holds -- testing against null, comparing identity,
      copying, assigning, being retained -- is defined on it, on both backends and in both
      directions of the hierarchy. An object's identity is what those operations read, and it is the
      same through a view naming an ancestor, a view naming a contract the class conforms to, and no
      view at all.

- [x] Reaching a property or entering a behavior through such a reference. The name resolves where
      the instance is known: the walk reaches the scope declaring the class, that scope answers
      which class the name means, and the class answers where the name lands on it -- all of it
      once, while the design elaborates, so an access applies what was settled and looks nothing up.
      Reading a property, writing one, entering a behavior that is not overridden and dispatching
      one that is all work, in both directions of the hierarchy, and a class the reader cannot name
      may still declare a property under a name its base already used without the reader reaching
      the wrong one. **Which class an access lands on belongs to the instance and not to the
      artifact**: one compiled body serves instances whose accesses reach classes with different
      layouts, which is what rules out settling any of it where the body is compiled. A backend that
      reaches a member by writing its name in the target language declines the form instead of
      realizing it, since a position settled at elaboration has no such name.

- [ ] Reaching a class's type-associated storage through such a reference (LRM 8.9, 8.10). A static
      property needs no object, so it is not reached through the handle at all (LRM 8.3): the cell
      belongs to whatever replicates the class declaration, which for a class a design element
      declares is that element's instance. Reaching it is therefore the separate question of reading
      another unit's instance storage by a name no signature carries, not the coordinate above, and
      it is refused.

- [x] A collection of such handles -- an unpacked array or a queue of them -- reached through such a
      reference. The coordinate side needed nothing new, since what follows a value with no class
      view is decided by the operation and not by the syntax that produced the value. What blocked
      it was that a handle was not a value either backend could hold as an element, which stopped
      the construct with no hierarchical name anywhere in sight and is settled in
      `../decisions/a-handle-is-a-value.md`.

- [ ] An instance is still a backend-private shell rather than a generic object over its definition.
      The runtime no longer reaches generated behavior through a C++ base class, so a scope's
      lifecycle and identity already arrive as data; what remains is member storage, which each
      backend still lays out its own way behind its own allocator. Until that unifies through the
      place model, the C++ backend emits a subclass per scope and the execution backend allocates a
      generic instance, which is two representations of one concept and the last thing keeping the
      allocator boundary. The target shape and why it was split from the dispatch work are settled
      in `../decisions/generated-behavior-boundary.md`.
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
