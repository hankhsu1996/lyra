# Decisions

Logged architectural decisions. Each entry records a decision with its rationale; the entry is
immutable once accepted, and a superseding decision links back to the one it replaces.

Decisions are reserved for choices with real trade-offs: rejected alternatives, load-bearing
invariants, or constraints that bind the codebase going forward. Housekeeping notes (e.g., "this
item is subsumed by an existing surface") do not warrant a decisions entry; record the reason inline
at the point it matters.

## Index

Grouped by subject so a decision is findable by concept, not only by filename. One line per entry;
the detail lives in the entry itself.

### Value types and representation

- [integral-representation](integral-representation.md) -- one fat `PackedArray` carries integral
  shape as runtime fields, not C++ template parameters.
- [value-store-discipline](value-store-discipline.md) -- a value is pure; preserving the
  destination's declared type across assignment lives at the store boundary, not the value.
- [value-type-concepts](value-type-concepts.md) -- the `lyra::value` operator surface is a lattice
  of composable C++ concepts, one per LRM operator family.
- [runtime-shape-and-default-value](runtime-shape-and-default-value.md) -- runtime shape lives on
  `PackedArray`; one OOB shield slot is both the canonical default and the out-of-bounds discard.
- [string-packed-conversion](string-packed-conversion.md) -- a `value::String` holds no NUL;
  packed-to-string strips NUL, `%s` formats bits without a string value.
- [enum-representation](enum-representation.md) -- an enum's semantic type identity is separate from
  its runtime value; the value is the base integral, never a distinct C++ type.

### Aggregate types and access

- [packed-array-representation](packed-array-representation.md) -- HIR represents a packed array
  recursively (one dim per node); MIR stays flat and HIR-to-MIR flattens.
- [unpacked-array-representation](unpacked-array-representation.md) -- representation of a
  fixed-size unpacked array.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- an unpacked struct is a
  value product and not an object, positional access, defaults synthesized at lowering.
- [unpacked-union-representation](unpacked-union-representation.md) -- the sibling the struct
  decision left open: overlapping storage is neither a product nor a sum, and this settles which one
  MIR models it as.
- [aggregate-names-are-type-content](aggregate-names-are-type-content.md) -- an aggregate the source
  declared names its members in its own type, whose value-domain projection is the product or the
  vector a type naming nothing already has.
- [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md) -- an unpacked array's index
  range is part of its type, not a size carried beside it; packed arrays are carved out.
- [selector-coordinate-resolution](selector-coordinate-resolution.md) -- `a[1:7]`, `b[7:1]`, and
  `c[0:6]` are three distinct types, so resolving a subscript to a coordinate is the type's job.
- [jit-aggregate-realization](jit-aggregate-realization.md) -- on the execution backend every
  aggregate is a runtime-owned opaque value (erasure), not structurally monomorphized; the choice is
  below LIR, and LIR's aggregate operations stay realization-agnostic.
- [slice-value-semantics](slice-value-semantics.md) -- a slice read materializes an owned value; the
  access model is value, not borrow.
- [value-projection-write](value-projection-write.md) -- a value-aggregate interior write is an
  owner-relative functional whole-value update through the owner, not a place store; the in-place
  write a target may realize it as is a property of that target. How the update is stated is
  superseded by [value-descent-as-named-calls](value-descent-as-named-calls.md).
- [value-descent-as-named-calls](value-descent-as-named-calls.md) -- every level of a descent into a
  value is a call whose entry HIR-to-MIR names, composed through the receiver, so no node names a
  part and no consumer classifies a step or recovers an owner. Supersedes the selector-path node of
  [value-projection-designator](value-projection-designator.md), which was never built.
- [queue-operators](queue-operators.md) -- queue access operators lower to built-in method calls;
  read and write are distinct methods chosen at lowering. Where it places the receiver is superseded
  by [call-receiver-on-the-callee](call-receiver-on-the-callee.md).
- [concatenation-realization](concatenation-realization.md) -- a join is a call rather than a node
  of its own, over every operand family, and reaches MIR already folded to the two operands every
  entry that performs it takes.
- [value-construction-forms](value-construction-forms.md) -- a construction says which form it is; a
  value that is its own parts is a primitive, a container built from one is a call, and what names a
  call is the type's own answer.
- [array-method-dispatch](array-method-dispatch.md) -- LRM 7.12 array-method runtime semantics;
  per-family dispatch superseded by [builtin-call-identity](builtin-call-identity.md).
- [array-manipulation-entry-stream](array-manipulation-entry-stream.md) -- LRM 7.12 locator /
  reduction / `map` operate over an ordered `(index, element)` stream; ordering stays in-place.
- [format-dispatch](format-dispatch.md) -- value formatting dispatches through `Formatter<T>` and
  `FormatArg`.

### Lowering and IR shape

- [exhaustive-alternative-consumption](exhaustive-alternative-consumption.md) -- a closed set of
  alternatives is consumed by a switch, never by `==`, so a fact the front end resolved cannot be
  dropped silently.
- [lowering-organization](lowering-organization.md) -- how lowering passes organize their internal
  objects (facts, registries, builders, walk frame).
- [storage-access-as-place-formation](storage-access-as-place-formation.md) -- a dereference of a
  capability wrapper's place names the storage it represents, and each backend supplies the protocol
  from the place's type; naming it that way is never a call, while reading it and replacing the
  whole of it are, per [owner-transition-and-observation](owner-transition-and-observation.md),
  which supersedes both of those answers here.
- [declarations-before-bodies](declarations-before-bodies.md) -- every structural declaration's
  identity and shape is CU-global and queryable before any executable lowering begins.
- [reporting-every-gap-in-one-run](reporting-every-gap-in-one-run.md) -- a refusal is collected and
  the stage goes on, so one run accounts for every unit and every member rather than for the first
  that stopped; a stage that reported anything is the last one that runs, and what it produced is
  discarded.
- [foreach-lowering](foreach-lowering.md) -- the lowering shape of `foreach`.
- [compound-assignment-write-location](compound-assignment-write-location.md) -- one node per write
  target, evaluating the left-hand side exactly once (LRM 11.4.1); revised so an operator the
  library performs is applied by the entry that performs it, and superseded for value interiors by
  [value-projection-write](value-projection-write.md).
- [conversion-folding](conversion-folding.md) -- when type conversions are folded.
- [shape-from-types-contents-from-expressions](shape-from-types-contents-from-expressions.md) -- a
  lowering reads a number it needs before run time from a type, never from an expression; an operand
  stays the expression it is and is never evaluated or matched for a literal, and a pattern key is a
  designator rather than an operand.
- [variable-initialization](variable-initialization.md) -- LRM 10.5 variable initialization as a
  constructor-scope statement.
- [variable-lifetime-storage](variable-lifetime-storage.md) -- storage of static-lifetime body
  locals.
- [lifetime-extended-automatic-scope](lifetime-extended-automatic-scope.md) -- an automatic scope a
  process may outlive is a shared-owned activation object; a detached branch captures the handle by
  value.
- [read-set-inference](read-set-inference.md) -- read-set inference via slang flow analysis.
- [runtime-effects-as-generic-calls](runtime-effects-as-generic-calls.md) -- runtime effects lower
  to ordinary `CallExpr` with the engine handle as one argument.
- [ambient-runtime-services](ambient-runtime-services.md) -- generated code reaches the runtime
  through a thread-local `current_runtime()` the attached Runtime publishes for its lifetime;
  `RuntimeEffects` is the narrow capability surface, `Runtime final` adds host orchestration; the
  receiver-based route is retired.
- [callable-receiver](callable-receiver.md) -- every callable body's first binding is `self`; how it
  is supplied differs per callable form.
- [unified-callable-model](unified-callable-model.md) -- one callable concept: callable code vs
  callable value (code + bound environment), with no kind tag on the body.
- [block-expression](block-expression.md) -- several steps in value position are a block expression,
  the one node that lifts any statement sequence into an expression; it sequences and nothing else,
  so its steps do not return. A closure invoked where it is built is rejected.
- [closure-environment-and-activation-frame](closure-environment-and-activation-frame.md) -- a
  closure (`ClosureType`, an anonymous concrete callable value: captures plus one invoke) and a
  promoted automatic scope (`StructType` reached via `Shared<>`, fields only, no invoke) are two
  distinct nominal categories sharing only the field substrate, not one fused type with an optional
  invoke; the callable value has a concrete `ClosureType` level and an erased
  `ErasedCallableType<Sig>` level with an explicit erasure.
- [builtin-call-identity](builtin-call-identity.md) -- built-in method calls carry a flat
  closed-namespace identifier (`support::BuiltinFn`) shared by HIR and MIR. Its positional receiver
  convention is superseded by the next entry.
- [call-receiver-on-the-callee](call-receiver-on-the-callee.md) -- the object a call dispatches on
  is a field of the callee, not the first of its arguments, so no consumer works out which operand
  is a receiver, and no callee target exists only to say whether there is one. Splitting every
  target by receiver-ness, putting the receiver on the call node, and verifying the positional
  convention are rejected.
- [address-of-primitive](address-of-primitive.md) -- MIR carries an explicit place-to-pointer
  operator (`AddressOfExpr`), dual to `DerefExpr`; the backend never injects `&`.
- [cast-is-a-pair-of-types](cast-is-a-pair-of-types.md) -- a cast is one node whose operand type and
  result type are its whole statement, so no kind sits beside them; a backend refuses a pair it
  cannot realize rather than passing the value through. One node per conversion, and a kind
  enumeration on the node, are rejected.
- [event-control-unification](event-control-unification.md) -- unified treatment of event control:
  every value-change wait (`always_comb` / `@*`, `@(...)`, `wait (cond)`, a continuous assignment)
  is one shape over a per-leaf `(observable, bit_range)` set. Its MIR carrier is superseded by the
  next entry, and what a leaf's bit range decides by the owner-transition entry.
- [value-change-wait-as-runtime-call](value-change-wait-as-runtime-call.md) -- that wait is an
  ordinary runtime call taking the trigger set, awaited like every other suspending call; MIR
  carries no event-control node, and one enum for the edge specifier is shared by compiler and
  runtime. A dedicated MIR statement, a per-leaf registration call, and an engine subscription verb
  are rejected.
- [update-due-on-an-event](update-due-on-an-event.md) -- an assignment whose update is due on an
  event states no slot where it stands, so the update is carried by an execution of no lineage that
  waits, places itself in the NBA region of the slot the event lands in, and writes there. A
  membership naming a closure, a `fork` branch, and submitting a closure after the event are
  rejected.
- [generic-lowering-machinery](generic-lowering-machinery.md) -- generic arena and shared
  context-free expression-handler templates over the pass class; node types stay typed.
- [arena-reference-lifetime](arena-reference-lifetime.md) -- `Arena::Get` is a transient view; the
  `Id` is the only durable handle, so lowering projects value facts before mutating.
- [mir-type-interning](mir-type-interning.md) -- the MIR type pool is a structural-equality
  interner; each semantic type has one canonical `TypeId`, enabling recursive class types.
- [lir-type-interning](lir-type-interning.md) -- the same for LIR, the layer that had not taken it:
  a LIR type's identity is its content, so a type built by the lowering and one translated from MIR
  are one type. Nothing needs excluding from the key, because LIR carries no source-language
  concept; the one field that did had no reader and is deleted.
- [hir-type-interning](hir-type-interning.md) -- the same for HIR, so a type published by one unit
  and read by another lands on the entry the reader already had; identity is the unit's own, never
  the frontend's.
- [context-free-call-lowering](context-free-call-lowering.md) -- one expression dispatcher template
  per boundary; the call family becomes a template once the `with`-clause element / index are
  co-equal closure parameters.

### References and construction

- [hierarchical-reference-routing](hierarchical-reference-routing.md) -- one semantic shape per
  hierarchical reference; per-segment classification by layout visibility; sealed endpoint on the
  hot path.
- [hierarchical-callable-dispatch](hierarchical-callable-dispatch.md) -- a subroutine a hierarchical
  name enables is the route to the declaring scope plus whatever answers the name there; a module
  publishing its subroutines is excluded because the dependency graph must stay acyclic, so its
  subroutines are answered by the scope itself. What a `disable` names is the same route ending at
  the scope's own activity, which no unit publishes and which needs no name.
- [binding-graph-resolution](binding-graph-resolution.md) -- resolution and sealing respect
  dependencies between references; forwarding chains collapse end-to-end.
- [hierarchical-reference-resolution](hierarchical-reference-resolution.md) (superseded) -- the
  prior decision, replaced by the two entries above.
- [specialization-identity](specialization-identity.md) -- a specialization's identity is the module
  name plus a content hash of its parameter bindings, computed independently by producer and
  consumer.
- [reference-as-data-type](reference-as-data-type.md) -- a reference is a direction at HIR and a
  data type at MIR; one type serves `ref` formals and `ref` ports, preserving the observable-cell
  protocol.
- [reference-binds-a-cell](reference-binds-a-cell.md) -- what that protocol makes a reference on the
  execution backend: the address of a value cell, so every referent is one and a local whose storage
  is lent gets a cell where it is declared. A callee has one formal, so the reference cannot vary
  with the storage its caller lends; the address of the referent's own storage, a runtime reference
  object, and a polymorphic storage core are rejected.
- [object-model](object-model.md) -- a module / scope and an SV class are one generic nominal object
  type; an SV class handle is a managed reference via precise tracing GC.
- [object-model-storage](object-model-storage.md) -- a compilation unit owns one canonical registry
  of local nominal object declarations; identity, lexical name resolution, and backend emission
  nesting are separate relations; the lexical-tree-only storage and a second identity are rejected.
- [entering-a-class-construction](entering-a-class-construction.md) -- a class's runtime definition
  carries no constructor, because the allocation site's static type chooses one and nothing a caller
  chooses belongs on a record every object shares: the runtime allocates, and the code that wrote
  the `new` enters the constructor, so its arguments cross as themselves. A registered entry taking
  a span of arguments, a per-arity signature on the definition, and splitting in the backend are
  rejected.
- [constructing-another-units-class](constructing-another-units-class.md) -- a construction reaches
  its constructor by the identity it reaches the class's declaration by, so the declaring unit
  decides where each answer is read and nothing after it; the constructor is named the way every
  other cross-unit method is, a construction reads nothing about it, and a class states the complete
  argument list its base construction carries where its own declaration is read. Putting what a base
  construction needs on the promise, letting an empty list mean two things, filling a default from
  the declaring scope, and a call target of LIR's own are rejected.
- [dispatch-position-is-a-lineage-coordinate](dispatch-position-is-a-lineage-coordinate.md) -- a
  class states the behaviors it introduces and the ones it takes over, never its lineage's; a
  behavior is named by the declaration that introduced it plus an ordinal within it, and flattening
  a lineage into positions is a layout question answered where the whole lineage is in hand rather
  than where a call is written. The runtime answers which body a value holds and the asking code
  enters it. An absolute position assigned while lowering, a record listing every body a class
  declares, and generated code reading the class record are rejected.
- [instance-array-multiplicity](instance-array-multiplicity.md) -- an array of children is one
  member whose type is a sequence of the child pointer, carrying multiplicity but no length, so
  which element a reference names is an operand of a projection rather than part of a member's
  identity; an interface port carrying a range is the same member over a borrowed pointer. A member
  per element with the coordinate in its name, a fixed-size aggregate carrying the count, an index
  step in the place vocabulary, one published member per element, and a simulation-value container
  are rejected.
- [procedural-storage-scope](procedural-storage-scope.md) -- HIR carries a lexical procedural scope
  tree (downward ownership, no backrefs) alongside its statement tree; a HIR-to-MIR two-pass
  scope-tree fold decides which named begin/ends materialize as runtime hierarchy children and where
  each static's storage physically lives; lexical owner and physical owner are distinct so an
  unnamed scope nested in a named one places its statics in the named scope's class without exposing
  them to cross-unit by-name lookup.
- [elaboration-lifecycle-phases](elaboration-lifecycle-phases.md) -- a generated constructor only
  allocates; elaboration is a staged build / resolve / initialize / activate protocol.
- [net-driver-resolution](net-driver-resolution.md) -- a net is a resolution node with node-owned
  driver contributions and capability-handle drivers, with the topology frozen at a Seal barrier;
  single-driver is N=1.
- [net-type-is-a-fold-and-a-contribution](net-type-is-a-fold-and-a-contribution.md) -- a net type
  states a fold and the contribution it makes to its own resolution, so strength decides between
  levels and the truth table within one; a per-bit strength on the resolved value, a value-only fold
  per net type, compile-time strength resolution, and the single-driver check at a barrier are
  rejected.
- [procedural-continuous-assignment](procedural-continuous-assignment.md) -- `assign` / `force` take
  a target over at one of two precedence levels through a call on its capability type, evaluated by
  the loop a continuous assignment already uses; a forced value is not a driver and needs no shadow
  storage, so reads cost nothing.
- [front-end-semantic-boundary](front-end-semantic-boundary.md) -- slang owns semantic resolution
  and sensitivity extraction; Lyra translates resolved facts to executable route and endpoint
  capability; sensitivity uses the correct per-consumer slang surface and never reclassifies from
  `ValueSymbol + global table + HopsTo`; reading, writing and observing one target consult the one
  translation instead of each recomputing it, so an assignment's target is validated nowhere.
- [cross-unit-class-translation](cross-unit-class-translation.md) -- AST-to-HIR splits class
  interning into a top-down `InternLocalClass` (never asks "which CU?") and a boundary
  `ResolveClassRef` (walks slang's parent chain only when a class is not already cached);
  design-wide precomputed maps and single-conflated interning are rejected.
- [class-declared-in-a-structural-scope](class-declared-in-a-structural-scope.md) -- a class a
  module, interface, or generate block declares is a type of that scope's instance (LRM 6.22, 23.9),
  so the object records which instance it belongs to and construction supplies it; the reference
  vocabulary is untouched because the hop is resolved once per body rather than once per reference.
  Putting the object in the runtime tree, reaching the scope by lexical capture, and refusing a
  construction reached through another unit's generic are rejected.
- [unit-scope-naming](unit-scope-naming.md) -- the anonymous `$unit` scope (LRM 3.12.1) is a
  namespace unit named by its compilation-unit input identity, recomputed table-free by producer and
  consumer; a design-wide unit id, a fixed name, a collection ordinal, and a content digest are all
  rejected.
- [interface-conformance-realization](interface-conformance-realization.md) -- inherited interface
  satisfaction (LRM 8.26.2) is resolved at AST-to-HIR and realized as a synthesized forwarding
  method (backend renders, never fabricates); the full method-to-slots dispatch representation is
  deferred until a physical-vtable backend reads it.
- [generated-behavior-boundary](generated-behavior-boundary.md) -- generated behavior reaches the
  runtime through an explicit, backend-neutral per-specialization unit definition (native lifecycle
  entries + a method dispatch table + constant metadata), not a backend-language object ABI; the C++
  subclass / vtable and a per-backend adapter are rejected as the boundary. Lifecycle and SV-virtual
  dispatch share a representation but are separate concepts; the definition holds the schema, never
  instance values.
- [jit-value-realization](jit-value-realization.md) -- the JIT represents every runtime value as an
  opaque handle into the runtime library (the baseline realization), and a `GeneratedCallScope` owns
  the transient values one generated entry creates -- the JIT counterpart of C++ stack/RAII.
  Physical-layout / in-frame value lowering is a later optimization, not a correctness prerequisite;
  cross-suspension and managed-value lifetime is out of scope for the call scope.
- [runtime-entry-naming](runtime-entry-naming.md) -- a runtime entry is named by the operation it
  performs and typed by the call that reaches it, so neither its symbol nor its signature is written
  down a second time; the symbol has one form, what the library does not realize is stated per
  builtin rather than inferred from a call, an overload set is two identities rather than one arity,
  and a check holds the prototype, the definition, and the binding to each other.
- [jit-process-suspension](jit-process-suspension.md) -- coroutine-ness is the callable's result
  type, a suspension is a generic LIR control edge whose wakeup is registered by preceding runtime
  calls, and the LLVM backend states where a body suspends while LLVM's coroutine passes derive the
  frame, resume state, and spills. The engine resumes a runtime-owned adapter, never a generated
  frame; a hand-rolled state machine in the emitter and an is-coroutine flag are rejected.
- [cross-suspension-value-storage](cross-suspension-value-storage.md) -- a value-typed non-managed
  procedural local in a suspending body is an activation-frame value: overwritten in place, owned by
  the activation (which also RAII-owns the generated coroutine), reached through a frame-held handle
  so its value outlives the per-stretch scope. Every coroutine value local gets one (no liveness
  analysis); the cell shares a storage core with the signal cell but is not observable, and the
  access is a `ValueCellTarget` LIR call so the backend stays mechanical. Native in-frame layout, a
  backend-private arena, and a narrow liveness pass are rejected.
- [managed-value-realization](managed-value-realization.md) -- a managed value never lives in
  storage this compiler does not describe, because the coroutine frame is delegated to LLVM and its
  contents are not enumerable. Three described storages -- the static instance tree, activation
  frames, and scheduler-held closures -- are the root set; the activation frame gains the slot
  description it lacked; safepoints coincide with runtime calls generated code already makes, so no
  instruction is added for the collector. Stack maps, a shadow stack, conservative scanning, moving
  the static tree into the heap, and a second traceable-frame path are rejected.
- [activation-frame-and-transient-scope](activation-frame-and-transient-scope.md) -- naming and the
  escape invariant: `RuntimeProcess` is the lineage/scheduler node, `activation` is the control
  identity, `ActivationValueStore` is one execution's cross-suspension value storage (named a store
  rather than a frame, because the generated body already has a frame and this is not it), and
  `GeneratedCallScope` is the per-stretch transient. A transient may not escape its stretch; every
  escaping store copies/promotes (the one non-copying path, a method return, stays in the caller's
  scope). A speculative slot/trace/GC shape is rejected. The entry's own rejection of a fused
  activation record is **withdrawn**: it left the storage nowhere to live but a coroutine body's
  local, which dies one step before the frame around it, and both readings that forbid it were
  already in `object_lifetime.md`.
- [root-unit-elaboration](root-unit-elaboration.md) -- design elaboration is the synthetic `$root`
  unit's `construct` entry, which builds the top-level modules as its owned children; there is no
  design-level free function. Engine / bind / run stay host runner policy and never enter MIR; both
  backends' host shells collapse to creating the engine, calling the root construct, then bind /
  run.
- [member-slot-storage](member-slot-storage.md) -- a member is a logical place (a base plus a
  projection chain, named by load, store, and address-of); a unit definition declares a member
  storage schema and a generic instance owns one storage object per member. The C++ backend realizes
  a member as a native field, the execution backend as runtime-owned storage; a cell is only ever
  addressed, never read as a value. Physical in-frame layout is a later optimization, the
  member-storage counterpart of the opaque-handle value baseline.
- [inherited-member-reference](inherited-member-reference.md) -- a member projection names the
  declaration that declares the member and the slot it gave it, so which storage a shadowed name
  reaches is stated rather than re-derived from the type the chain arrived at; an inherited member
  keeps its slot, so a base's unpublished addition moves nothing. Every kind of declaration that
  declares fields is named this way, not only the class. Flattening during lowering, a base
  subobject as a place step, and a per-access base offset are rejected.
- [closure-value-realization](closure-value-realization.md) -- on the execution backend a closure
  declaration publishes a definition (its body and its capture storage schema) and a closure value
  is an instance of it, so a capture is member storage, a capture read is a member place, and
  building the value is where a captured value is copied out of the stretch that made it. The invoke
  takes its receiver uniformly; a code-address-plus-product environment and an erased callable type
  are rejected.

### Foreign-language boundary

- [dpi-foreign-boundary](dpi-foreign-boundary.md) -- DPI-C is the foreign arm of the one callable
  model: a foreign symbol is a bodyless or bodied callable the unit owns, marshaling is a cross-ABI
  carrier conversion expressed in MIR at each call, and an export's context is a thread-local
  ambient handle.
- [dpi-open-array-boundary](dpi-open-array-boundary.md) -- an open array crosses as a canonical
  boundary object owning its own storage, never as a borrow of the actual; the formal's unsized
  shape rides the ABI carrier rather than the type system.

### Compile-time model and specialization

- [unit-signature](unit-signature.md) -- what each unit kind publishes and how that set is known to
  be complete; a signature member is named where the referrer compiles, a name past a signature
  resolves at elaboration; the signature is an artifact separate from code, and that split decides
  what a change recompiles.
- [reaching-past-a-published-class](reaching-past-a-published-class.md) -- a class promises what it
  declares and the class it extends, never what it inherited, so a referrer resolves an inherited
  property or behavior by walking that chain and the coordinate names where the walk landed; reading
  each promise is what records the dependency, so reaching past a class makes the introducer's unit
  a real dependency, and nothing bounds in advance which promises it may read. Flattening the
  promise at publish, resolving the chain at elaboration, a pre-computed consumed set, and
  publishing what a class keeps to itself are rejected.
- [published-member-placement](published-member-placement.md) -- a published member's position is
  its position in the signature, computed by producer and consumer and carried by neither; the
  referrer records the object it compiled against in its own IR, in a registry separate from the
  classes it compiles, so no pass below the one that consumes a signature reads one. Carrying the
  position on the reference, handing signatures to a lower pass, and a by-name lookup are rejected.
- [publishing-part-of-a-member](publishing-part-of-a-member.md) -- a connection point names a
  projection of a published member, a folded value, or nothing; a signature carries a closed
  selector path and never an expression, an interface publishes its modports, and the referrer
  applies the projection as an ordinary access so no endpoint category is added. A synthesized cell
  per point, carrying the source expression, reading it off the frontend, a member per point, and a
  per-modport member list are rejected. The write half of its D5, which published a name a view
  offers as a subroutine assigning to it, is superseded by
  [names-a-view-offers](names-a-view-offers.md).
- [names-a-view-offers](names-a-view-offers.md) -- an identifier a modport did not rename is the
  interface item and nothing about the view crosses; one it renamed is the storage it designates
  where the view admits a write, and the subroutine evaluating it where it does not, so every
  assignment form the language allows reaches it with no arm per form. Defining each failing form in
  terms of a subroutine pair, publishing both representations, reading the internal symbol off the
  frontend, and flattening the interface are rejected.
- [publishing-an-owned-instance](publishing-an-owned-instance.md) -- an interface publishes the
  interfaces it instantiates, so a name continues past a port into one; continuing through a
  published member is the step form of ending on one, and every route to a published name becomes
  typed at once. Carrying the inner unit's members inline, recording its object eagerly, walking
  bodies to bound the read set, and letting the reach fall to a by-name lookup are rejected. Its D3,
  bounding what a lowering may read, is superseded by
  [reaching-past-a-published-class](reaching-past-a-published-class.md).
- [identity-is-not-a-rendering](identity-is-not-a-rendering.md) -- what must distinguish is stored
  as its parts and composed into a name only by whoever knows the spelling rules; an identity splits
  exactly where the layer below it splits; naming another unit's object and holding what it
  published are two facts with two vocabulary items. Mangling at the composition site, tagging one
  conflated arm, and recording every transitively reachable unit are rejected.
- [calling-a-subroutine-on-another-units-object](calling-a-subroutine-on-another-units-object.md) --
  an interface publishes its subroutines, and enabling one is a route that ends at the object plus a
  name resolved against what that unit promised, so no sealed-endpoint category for a callable is
  needed; whether such a method takes a receiver is its declaration, carried down rather than
  re-derived below. Naming the instance the frontend resolved to, a callable endpoint category, and
  a by-name lookup for a published name are rejected. How that fact is carried is superseded by
  [call-receiver-on-the-callee](call-receiver-on-the-callee.md).
- [interface-port-binding](interface-port-binding.md) -- an interface port's declared type names the
  unit whose instance belongs there, by name, so it crosses a signature; the member holds a borrowed
  reference the parent binds once during elaboration, a fourth published storage kind; an interface
  publishes every net and variable it declares; and which interface a port carries feeds the
  module's specialization identity, without which two differently bound modules collide on one name.
- [parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) -- every
  parameter is treated as code-shape-affecting for now (conservative over-approximation);
  classification and constructor-input threading are deferred.
- [generate-variable-specialization](generate-variable-specialization.md) -- a generate variable is
  a specialization input like a parameter; demoting it to a runtime input is a proof-gated
  optimization.

### Runtime execution and scheduling

- [owner-transition-and-observation](owner-transition-and-observation.md) -- every mutation of
  observable storage reports one thing, whether the owner transitioned, and publication is a
  function of that alone; forming a designation is itself such a mutation; an event control's
  detection belongs to the armed observation, which compares its own expression against the baseline
  it took when it armed. Information about what a write could have affected may only eliminate
  candidate observations, never decide one.
- [activation-registration](activation-registration.md) -- an activation's membership in a wake
  target (an observable, an event, a join condition, a region queue, a delay slot) is one record the
  activation owns and the target merely links; the activation-side set and the target-side list are
  two indexes over it, revoking is a detach rather than a search, and the two-authoritative-copies
  shape is rejected.
- [activation-disposition](activation-disposition.md) -- an activation has one authoritative
  disposition (Executing / Runnable / Blocked / Suspended(saved) / Terminal); a wait is a retainable
  pending capability distinct from its registration (enrollment), supplied uniformly by each
  construct; suspension saves the prior disposition; a central wait-kind taxonomy,
  `Runnable(region)`, and mirroring the wait's state are rejected.
- [disable-scope-invalidation](disable-scope-invalidation.md) -- `disable` (LRM 9.6.2) invalidates a
  cancellation source's generation; every affected execution reconciles at one uniform validity gate
  before its next statement, reusing the registration entitlement substrate. Membership in a target
  is carried by the running process, so it spans a call, and is captured at a spawn. A local goto, a
  per-thread extent frontier carried in a `DisableUnwind` exception, an explicit resume-reason, a
  dedicated entitlement object, and membership rebuilt per callable from lexical scope are rejected.
- [run-time-failure-is-not-an-outcome](run-time-failure-is-not-an-outcome.md) -- an activation's
  terminal outcome carries only what the source language can consume, which is the produced value
  and the departure a region lands; SystemVerilog has no spelling for a failure, so a failure is the
  tool speaking and not an outcome. Failures are told apart by who acts on them, not by when they
  surface: an unsupported construct is a lowering's answer whenever it is found, a design's run-time
  error is a severity report whose level the tool chooses (LRM 20.10 admits one that does not end
  the run), and an internal inconsistency does not borrow the language's ending. Ending the run
  walks `$finish`'s tail so `final` procedures execute (9.2.3). Reverses the third alternative of
  `activation.md` invariant 2; the awaiter-side delivery every async runtime uses, a Rust-style
  error value, and a check after every fallible operation are rejected.
- [ending-a-run](ending-a-run.md) -- a report, a stop, and an unclaimable departure are three
  primitives, and every ending composes them; a design's run-time error is a report Lyra writes for
  the design rather than a failure crossing the engine. `final` procedures run when the simulation
  reached its end -- exhaustion, the design asking, or such an error -- and not when the tool could
  not carry on, while the cover report, the output drain and the exit status are owed either way.
  `$stop` ends the run where nothing can resume it and the Table 20-1 diagnostic is the whole of
  what separates it from `$finish`; that diagnostic prints where the task executes and carries no
  severity. The simulation's boundary follows the elaboration phases, so a time-zero initializer's
  error is a run-time error rather than an escape. Completes the reversal of `activation.md`
  invariant 2's third alternative. Catching the throw at the engine loop, skipping `final` after
  `$stop`, `$stop` as a failure, a fifth severity, and aborting at the raise site are rejected.
- [deferred-report-queue](deferred-report-queue.md) -- a pending report stays in the region's own
  list and withdrawal is a refusal to act, so it records the sources that can withdraw it and acts
  only if all still stand: two `disable` targets through the accepted cancellation-source
  generation, and the creating process's execution pass as a liveness token, because a source
  outlives a report and a process does not. Maturity discards the record, which is the one-way
  latch; what each construct records is how one queue serves LRM 16.4 and 12.4.2.1 with different
  flush-point sets. A per-process container the engine enumerates, a single per-process token (which
  cannot express `disable` of one assertion), and a notify-style cancellation token are rejected.
- [sampled-value-and-its-clock](sampled-value-and-its-clock.md) -- what a sampled value function
  reads is retained by the cell at the transition boundary every mutation already reports through,
  armed per instance at Activate, so cost follows changes rather than time slots and the time-zero
  rule falls out of the value the cell is armed with. The clocking event is the trigger set a
  value-change wait already builds and the front end resolves which one it is, so LRM 16.9.3's
  ordering and 16.14.6's conditions are not restated here. The sampler is a synthesized process
  because a history must record every tick, not only the ticks something waited for, and it commits
  in Postponed so that "strictly prior" holds by construction rather than by ordering. A Preponed
  scan, per-leaf history with replay, a second implementation of the clock inference, a node kind
  for the sampled read, and an armed observation are rejected.
- [concurrent-assertion-evaluation](concurrent-assertion-evaluation.md) -- derived from Annex F's
  formal semantics. One start rule covers both placements: an attempt begins where the clock ticks
  and the enabling condition holds, which is 1 for a declarative assertion and "control reached this
  statement" for a procedural one, so where an assertion sits in HIR follows from whether it has a
  condition to record rather than from the grammar. Three levels, and what may be pooled at each is
  decided by the quantifier over it: an evaluation is a set of positions in an automaton the
  HIR-to-MIR lowering builds, pooling because a sequence's operators are existential; an attempt is
  a list of evaluations that never pool because implication quantifies universally over match
  points; an assertion holds attempts that never pool because each reports its own result. A finite
  trace admits four answers, so an attempt reports when the trace settles it and is otherwise
  pending, and weak versus strong is which answer the statement demands. A synthesized process per
  assertion and clocking event submits each tick's advance to Observed and each resolved attempt's
  action to Reactive. A coroutine per attempt, one merged position set for the whole assertion, an
  interpreted transition table, building the automaton at AST-to-HIR, a MIR node kind for a
  sequence, and evaluating where the process wakes are rejected.

### Diagnostics

- [diagnostic-construction](diagnostic-construction.md) -- a diagnostic's kind is derived from its
  code at construction; construction is infallible; the `UnsupportedCategory` axis is removed.
- [qualified-statement-violation-check](qualified-statement-violation-check.md) -- `unique` /
  `unique0` / `priority` state two independent assertions, uniqueness and totality; an explicit
  `else` or `default` discharges totality, and what the live assertions need decides what the
  statement evaluates, so a `priority` carrying a catch-all lowers as the unqualified statement.

### Compiler inputs

- [project-file](project-file.md) -- `lyra.toml` is a manifest of the design, carrying what is true
  of it for everyone who builds it and never an invocation or machine property; material accumulates
  and selection is replaced, a path resolves against its own manifest, and naming sources on the
  command line uses no manifest at all. A project mode, a merged cascade, and a flag to suppress
  discovery are rejected.

### Conformance testing

- [conformance-case-shape](conformance-case-shape.md) -- a case is a self-checking SystemVerilog
  program indexed by LRM clause; it names no path, states no expected variable, and carries no
  manifest, and what a path cannot do is recorded once per path.
- [conformance-diagnostic-claims](conformance-diagnostic-claims.md) -- a requirement whose whole
  observable is a report is stated as a directive and written in both directions; a claim of silence
  is the run writing nothing rather than the absence of one wording, and golden output stays
  reserved for cases whose subject is the output channel.

### Measurement

- [benchmark-case-shape](benchmark-case-shape.md) -- a case fixes the shape of its work and takes
  the amount as a runtime argument, so the harness picks the amount and reports a rate; no iteration
  count is written down anywhere.

## File Naming

`kebab-case.md`. The name describes the decision, not when it was made; the date lives inside the
file. Existing example: `integral-representation.md`.

## Shape

There is no fixed template. The existing `integral-representation.md` is the reference for shape:
title, date, status, the model or findings that shaped the decision, the decision itself, and the
consequences that follow. Let the subject drive the structure; a decision with no rejected
alternative or no load-bearing invariant probably should not be a decision entry.
