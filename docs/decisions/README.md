# Decisions

Logged architectural decisions. Each entry records a decision with its rationale; the entry is
immutable once accepted, and a superseding decision links back to the one it replaces.

Decisions are reserved for choices with real trade-offs: rejected alternatives, load-bearing
invariants, or constraints that bind the codebase going forward. Housekeeping notes (e.g., "this
archive item is subsumed by an existing surface") do not warrant a decisions entry; record the
reason inline at the point it matters.

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

### Aggregate types and access

- [packed-array-representation](packed-array-representation.md) -- HIR represents a packed array
  recursively (one dim per node); MIR stays flat and HIR-to-MIR flattens.
- [unpacked-array-representation](unpacked-array-representation.md) -- representation of a
  fixed-size unpacked array.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- an unpacked struct is the
  generic product type (MIR `TupleType`), positional access, defaults synthesized at lowering.
- [slice-value-semantics](slice-value-semantics.md) -- a slice read materializes an owned value; the
  access model is value, not borrow.
- [queue-operators](queue-operators.md) -- queue access operators lower to built-in method calls;
  read and write are distinct methods chosen at lowering.
- [array-method-dispatch](array-method-dispatch.md) -- LRM 7.12 array-method runtime semantics;
  per-family dispatch superseded by [builtin-call-identity](builtin-call-identity.md).
- [array-manipulation-entry-stream](array-manipulation-entry-stream.md) -- LRM 7.12 locator /
  reduction / `map` operate over an ordered `(index, element)` stream; ordering stays in-place.
- [format-dispatch](format-dispatch.md) -- value formatting dispatches through `Formatter<T>` and
  `FormatArg`.

### Lowering and IR shape

- [lowering-organization](lowering-organization.md) -- how lowering passes organize their internal
  objects (facts, registries, builders, walk frame).
- [declarations-before-bodies](declarations-before-bodies.md) -- every structural declaration's
  identity and shape is CU-global and queryable before any executable lowering begins.
- [foreach-lowering](foreach-lowering.md) -- the lowering shape of `foreach`.
- [conversion-folding](conversion-folding.md) -- when type conversions are folded.
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
- [callable-receiver](callable-receiver.md) -- every callable body's first binding is `self`; how it
  is supplied differs per callable form.
- [unified-callable-model](unified-callable-model.md) -- one callable concept: callable code vs
  callable value (code + bound environment). Target model; not yet built.
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
  driver slots and capability-handle drivers, validated at a Seal barrier; single-driver is N=1.
- [front-end-semantic-boundary](front-end-semantic-boundary.md) -- slang owns semantic resolution
  and sensitivity extraction; Lyra translates resolved facts to executable route and endpoint
  capability; sensitivity uses the correct per-consumer slang surface and never reclassifies from
  `ValueSymbol + global table + HopsTo`.
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

### Compile-time model and specialization

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

### Diagnostics

- [diagnostic-construction](diagnostic-construction.md) -- a diagnostic's kind is derived from its
  code at construction; construction is infallible; the `UnsupportedCategory` axis is removed.

## File Naming

`kebab-case.md`. The name describes the decision, not when it was made; the date lives inside the
file. Existing example: `integral-representation.md`.

## Shape

There is no fixed template. The existing `integral-representation.md` is the reference for shape:
title, date, status, the model or findings that shaped the decision, the decision itself, and the
consequences that follow. Let the subject drive the structure; a decision with no rejected
alternative or no load-bearing invariant probably should not be a decision entry.
