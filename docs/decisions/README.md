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
- [unpacked-struct-representation](unpacked-struct-representation.md) -- an unpacked struct is the
  generic product type (MIR `TupleType`), positional access, defaults synthesized at lowering.
- [unpacked-union-representation](unpacked-union-representation.md) -- the sibling the struct
  decision left open: overlapping storage is neither a product nor a sum, and this settles which one
  MIR models it as.
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
  owner-relative value projection (a functional whole-value update through the owner), not a place
  store; MIR states the place-vs-projection designator, the C++ in-place write is a
  behavior-preserving optimization.
- [value-projection-designator](value-projection-designator.md) -- the formal shape of that
  designator: one node whose children are the owner place and a closed selector path, shared by the
  write path and by the projection reference a `ref`, an `output` / `inout` actual, and a
  nonblocking assignment bind; the nested-lvalue write encoding is deleted.
- [queue-operators](queue-operators.md) -- queue access operators lower to built-in method calls;
  read and write are distinct methods chosen at lowering.
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
  capability wrapper's place names the storage it represents; access is never a call, and each
  backend supplies the protocol from the place's type.
- [declarations-before-bodies](declarations-before-bodies.md) -- every structural declaration's
  identity and shape is CU-global and queryable before any executable lowering begins.
- [foreach-lowering](foreach-lowering.md) -- the lowering shape of `foreach`.
- [compound-assignment-write-location](compound-assignment-write-location.md) -- one uniform node
  evaluating the left-hand side exactly once (LRM 11.4.1); superseded for value interiors by
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
  closed-namespace identifier (`support::BuiltinFn`) shared by HIR and MIR.
- [address-of-primitive](address-of-primitive.md) -- MIR carries an explicit place-to-pointer
  operator (`AddressOfExpr`), dual to `DerefExpr`; the backend never injects `&`.
- [event-control-unification](event-control-unification.md) -- unified treatment of event control:
  every value-change wait (`always_comb` / `@*`, `@(...)`, `wait (cond)`, a continuous assignment)
  is one shape over a per-leaf `(observable, bit_range, edge)` set. Its MIR carrier is superseded by
  the next entry.
- [value-change-wait-as-runtime-call](value-change-wait-as-runtime-call.md) -- that wait is an
  ordinary runtime call taking the trigger set, awaited like every other suspending call; MIR
  carries no event-control node, and one enum for edge polarity is shared by compiler and runtime. A
  dedicated MIR statement, a per-leaf registration call, and an engine subscription verb are
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
- [procedural-storage-scope](procedural-storage-scope.md) -- HIR carries a lexical procedural scope
  tree (downward ownership, no backrefs) alongside its statement tree; a HIR-to-MIR two-pass
  scope-tree fold decides which named begin/ends materialize as runtime hierarchy children and where
  each static's storage physically lives; lexical owner and physical owner are distinct so an
  unnamed scope nested in a named one places its statics in the named scope's class without exposing
  them to cross-unit by-name lookup.
- [elaboration-lifecycle-phases](elaboration-lifecycle-phases.md) -- a generated constructor only
  allocates; elaboration is a staged build / resolve / initialize / activate protocol.
- [net-driver-resolution](net-driver-resolution.md) -- a net is a resolution node with node-owned
  driver contributions and capability-handle drivers, validated at a Seal barrier; single-driver is
  N=1.
- [front-end-semantic-boundary](front-end-semantic-boundary.md) -- slang owns semantic resolution
  and sensitivity extraction; Lyra translates resolved facts to executable route and endpoint
  capability; sensitivity uses the correct per-consumer slang surface and never reclassifies from
  `ValueSymbol + global table + HopsTo`.
- [cross-unit-class-translation](cross-unit-class-translation.md) -- AST-to-HIR splits class
  interning into a top-down `InternLocalClass` (never asks "which CU?") and a boundary
  `ResolveClassRef` (walks slang's parent chain only when a class is not already cached);
  design-wide precomputed maps and single-conflated interning are rejected.
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
  access is a `ActivationFrameTarget` LIR call so the backend stays mechanical. Native in-frame
  layout, a backend-private arena, and a narrow liveness pass are rejected.
- [managed-value-realization](managed-value-realization.md) -- a managed value never lives in
  storage this compiler does not describe, because the coroutine frame is delegated to LLVM and its
  contents are not enumerable. Three described storages -- the static instance tree, activation
  frames, and scheduler-held closures -- are the root set; the activation frame gains the slot
  description it lacked; safepoints coincide with runtime calls generated code already makes, so no
  instruction is added for the collector. Stack maps, a shadow stack, conservative scanning, moving
  the static tree into the heap, and a second traceable-frame path are rejected.
- [activation-frame-and-transient-scope](activation-frame-and-transient-scope.md) -- naming and the
  escape invariant: `RuntimeProcess` is the lineage/scheduler node, `activation` is the control
  identity, the `activation frame` (`ActivationFrameStorage`) is the cross-suspension value storage,
  and `GeneratedCallScope` is the per-stretch transient. A transient may not escape its stretch;
  every escaping store copies/promotes (the one non-copying path, a method return, stays in the
  caller's scope). A fused `RuntimeActivation` and a speculative slot/trace/GC shape are rejected.
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
- [published-member-placement](published-member-placement.md) -- a published member's position is
  its position in the signature, computed by producer and consumer and carried by neither; the
  referrer records the object it compiled against in its own IR, in a registry separate from the
  classes it compiles, so no pass below the one that consumes a signature reads one. Carrying the
  position on the reference, handing signatures to a lower pass, and a by-name lookup are rejected.
- [identity-is-not-a-rendering](identity-is-not-a-rendering.md) -- what must distinguish is stored
  as its parts and composed into a name only by whoever knows the spelling rules; an identity splits
  exactly where the layer below it splits; naming another unit's object and holding what it
  published are two facts with two vocabulary items. Mangling at the composition site, tagging one
  conflated arm, and recording every transitively reachable unit are rejected.
- [calling-a-subroutine-on-another-units-object](calling-a-subroutine-on-another-units-object.md) --
  an interface publishes its subroutines, and enabling one is a route that ends at the object plus a
  name resolved against what that unit promised, so no sealed-endpoint category for a callable is
  needed; an instance method and a type-associated one of another unit are different targets. Naming
  the instance the frontend resolved to, a callable endpoint category, and a by-name lookup for a
  published name are rejected.
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
