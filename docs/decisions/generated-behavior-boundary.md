# Generated behavior through a backend-neutral definition

Date: 2026-07-01 Status: accepted

## Context

The runtime drives every design instance through a small set of generated behaviors: the
post-construction lifecycle bodies (`ResolveState` / `InitializeState` / `CreateProcesses`, see
`elaboration-lifecycle-phases`) plus constant identity (def name, time precision). How generated
code supplies those behaviors to the runtime is the runtime/generated boundary this decision fixes.

The C++ backend supplies them by emitting a real subclass `class Test : public Instance` with
virtual overrides; the C++ compiler builds the vtable and the engine dispatches through it. An
LLVM/JIT backend emits free functions -- there is no C++ class and no vtable, so the engine's
virtual dispatch cannot reach them, and an adapter forwards each virtual to a looked-up function
pointer.

The underlying issue is not that a JIT cannot build a vtable. It is that **C++ inheritance binds
behavior to the object's concrete C++ type**, which makes "be a C++ subclass" the de-facto extension
ABI every code generator must mimic. A generator whose output is not C++ must fabricate a C++ object
to plug in. The boundary privileges the one backend whose language is the runtime's language.

This is out of step with the compiler's own model, which is already backend-neutral:

- `object_model` inv 6: dispatch is a logical slot, physically realized below MIR.
- `lir`: a member is a logical place (`self.x`); physical layout is derived below LIR; an observable
  signal is already a runtime-library call.
- `callable-receiver`: every callable body is `f(self, args...)` with explicit `self`, never an
  implicit C++ `this`.
- `activation`: an activation is a payload-neutral token; each backend realizes it its own way.
- `emission_model` inv 1: one artifact per unit specialization, linked not aggregated.

The C++ backend realizes each of these by borrowing a C++ language feature (vtable, field, `this`,
coroutine frame). That is a convenience for the backend whose language is the runtime's; it is not
the model.

## Decision

Generated behavior reaches the runtime through an explicit, backend-neutral **unit definition**, not
through a backend-language object ABI. Two principles, kept separate:

- **Backend-neutral behavior ABI (primary).** A code generator exposes an instance's behavior
  through an explicit runtime contract -- a definition of native entries and constant metadata --
  not through a backend-language-specific object ABI (a C++ vtable, subclass, or `this`). This is
  the reason for the decision.
- **Semantic state is Lyra-owned (parallel).** `object_lifetime` already requires language-visible
  mutable state to live in Lyra-owned, traceable storage, never in opaque backend execution state.
  This decision applies the same shape -- Lyra owns the model, the backend owns control-flow
  realization -- to behavior. The principle corroborates the direction but does not by itself imply
  data-driven dispatch; state ownership and dispatch mechanism are independent axes.

The shape splits into two layers, because a scope's lifecycle behavior and its constant metadata are
a **scope** concern (a generate scope has its own processes, def name, and precision) while
per-specialization identity and structural construction are a **unit** concern (a generate scope is
not a unit):

```
ScopeProgram (per scope; immutable):
  metadata:  def name (display only), time precision   (data, not entries)
  behavior:  the scope's own lifecycle entries over the generic Scope receiver:
             resolve_state / initialize_state / create_processes

UnitDefinition (per specialization; immutable; the compile-time artifact):
  root:      the unit root scope's ScopeProgram
  construct: the unit-level structural-construction entry (bootstrap-invoked)
  later:     the specialization identity (linking / dispatch), member/storage
             schema, child-definition references, and a method dispatch table
             for SV virtual methods

Scope (generic runtime object; per scope):
  a reference to its ScopeProgram; scope-local runtime state
  the engine drives behavior through the program, never through C++ virtual
Instance : Scope (a design-unit root scope):
  additionally a reference to its UnitDefinition; owns per-instance storage,
  children, process objects
GenScope : Scope (a generate naming scope):
  its own ScopeProgram
```

A native entry's receiver is the generic `Scope`, not `Instance` -- a generate scope's lifecycle
body operates on a `GenScope`, and the runtime must not force its generic scope to pose as an
`Instance`. An entry whose logical receiver is the unit root may treat `self` as an `Instance`, but
that is the entry's own shape, not a runtime requirement.

Both backends supply the same `ScopeProgram` / `UnitDefinition` -- no adapter, no backend
privileged:

```
C++ backend:    emits native-entry bodies + a ScopeProgram / UnitDefinition referencing them
LLVM/JIT:       looks up the emitted entries + fills a ScopeProgram / UnitDefinition
future backend: native entries/handles + a ScopeProgram / UnitDefinition
```

Behavior is composed (a generic object holds a behavior definition), not inherited (a concrete C++
subtype decides behavior). This matches the explicit-`self` callable shape `callable-receiver`
fixes: the definition removes the impedance between the IR's explicit-self model and a runtime
boundary realized as implicit-`this`.

### Lifecycle and virtual-method dispatch are separate concepts

The engine's lifecycle hooks and SV `virtual` methods share the underlying representation -- a
logical slot resolved to a native entry -- but are modeled as separate concepts on the definition:

- **Lifecycle** is a closed set the engine calls at fixed elaboration phases (construct /
  resolve_state / initialize_state / create_processes). It is the `ScopeProgram`.
- **Method dispatch** is the open set of user `virtual` methods with inheritance, override families,
  receiver dynamic type, and signatures. It is object-oriented dispatch, a later `UnitDefinition`
  table.

This does not contradict `object_model` inv 6 ("one override machinery, rendered the same way"):
that governs the MIR-level override relation and the shared dispatch representation (both are
native-entry slots). Keeping the lifecycle `ScopeProgram` separate from the method dispatch table
prevents the closed lifecycle contract's evolution from being coupled to open class-virtual
semantics, and the reverse. SV `virtual` is a class feature; the method dispatch table is designed
for it but need not exist until SV classes are lowered.

### The definition is per specialization; it holds the schema, never instance values

The definition's identity is a **specialization** -- a compilation unit together with its
code-shape-affecting inputs (`specialization_model`). A `generate for` count that only steers
runtime state is a constructor input: it does not fork the specialization; the `construct` entry
loops at runtime to build the children. A `generate if` on a code-shape input that changes the
member set is a different specialization. So one definition per specialization carries a fixed
member schema, lifecycle, and process declarations; the actual member storage and the generate-built
children are per-instance runtime facts. The elaborated scope shape is a runtime function of the
specialization, not an independent identity axis.

The identity used to **link** a definition to its native entries and to dispatch is the
specialization identity of `specialization-identity` (a deterministic name from the module name and
a content hash of the parameter bindings), not `DefName`. `DefName` is display / debug metadata
only: two specializations of one parameterized module (`M #(N=4)` and `M #(N=8)`) share a `DefName`
but are distinct definitions with distinct entries, so linking or dispatching on `DefName` is
ambiguous.

The definition describes what an instance of the specialization has; it never holds a given
instance's member values. Definition-level facts (unit/specialization identity, logical member
declarations, lifecycle, process declarations, metadata) are distinct from instance-level facts
(member storage, child instances, signal cells, process objects, mutable runtime state).

### The native-entry ABI is a formal contract

A lifecycle or method entry is a formal backend-neutral native ABI -- a fixed calling convention
over the generic `Scope` receiver and an execution context -- not "any C++ function pointer." The
C++ backend may realize an entry as a thunk over an arbitrary C++ helper; the LLVM backend emits an
ABI-compatible free function. A backend's internal implementation is not the public
generated-runtime ABI.

### Allocation, construction, and dispatch are three concerns

Three things the current C++ constructor conflates are distinct here:

- **Allocation** -- obtaining an empty runtime object of the correct concrete representation.
- **Construction** -- running the generated structural semantics (build children and signals, bind
  runtime-visible components) on an already-allocated object. This is the `construct` entry; it must
  complete before `BindDesign` / first resolve.
- **Dispatch** -- running a scope's lifecycle behavior at each phase (the `ScopeProgram` entries).

Construction cannot substitute for allocation: `construct` runs generated semantics on an object
that already exists; it does not turn an allocated generic `Scope` into a backend's concrete object.
So while dispatch and construction are backend-neutral entries, **allocation stays a backend
concern** until member storage is unified. A backend supplies an allocator alongside its definition
-- the C++ backend allocates its concrete object (so a body's downcast to that type is valid); the
LLVM backend allocates a generic instance with runtime-owned storage. This boundary is what makes it
honest that unifying dispatch does not yet unify representation, storage, or allocation.

`construct` is a **unit-level** entry on the `UnitDefinition`, not a per-scope `ScopeProgram` entry.
Construction and lifecycle are two different traversals: a lifecycle phase walks an already-existing
scope tree (the runtime recurses into children), but during construction the children do not exist
yet -- the unit root's `construct` is invoked once and builds the subtree top-down, assigning each
created scope its `ScopeProgram`. The runtime cannot walk to a child before `construct` has created
it, so construction is root-anchored and cascading, distinct from the child-recursing lifecycle walk
below.

### Constant metadata is data, not a body

A scope's def name (LRM 23.8) and time precision (LRM Table 20-2) are immutable facts known when the
definition is built; they are not behavior. They live as plain data on the `ScopeProgram`'s
metadata, not as entries a caller invokes -- a backend supplies the values, it does not supply a
function that computes them. A def name crosses the generated-runtime boundary as a pointer-plus-
length, not a C++ `std::string_view`, so a non-C++ backend fills it without depending on a C++
type's layout; the runtime projects it to a `string_view` on its own side. Modeling these as
per-query entries would force a backend to emit a body whose only job is to return a constant and
would drag a C++ aggregate return type across the ABI -- the concrete defect that surfaced this.

Metadata is per **scope** (every scope reports its own precision to the design-global minimum, and a
generate scope reports an empty def name), so it lives on the `ScopeProgram`, not solely on the
`UnitDefinition`. The two are independently owned: a fact belongs where it is read.

### Every runtime-to-generated call is a call scope

The runtime enters generated code at a small set of boundaries -- the `construct` entry, each
lifecycle entry, and each resumed process body. Each such crossing is one **generated-call scope**:
a dynamic context the runtime establishes around the call and drops when it returns. A backend that
hands values across the boundary as opaque handles allocates their transient storage into the
innermost scope's arena, released when the call returns; a native C++ backend enters the same scope
and allocates nothing. The scope is the runtime/generated boundary itself, not a backend-private
notion, so it lives in the runtime at each per-call site -- not spanned by a bootstrap driver across
a whole elaboration, which would let one call's temporaries outlive it and blur separate crossings.
The generated IR never names it.

### The runtime owns lifecycle sequencing

A `ScopeProgram` entry describes only its own scope's generated behavior. Child traversal, phase
ordering across the tree, and the once-only discipline are the runtime's, exactly as the engine owns
scheduling and is a mechanism, not a semantics holder (`scheduling`). Generated code never recurses
into children or re-orders phases; doing so would produce double resolution, split ordering, or
cross-backend divergence.

### Required entries are total

A required lifecycle phase always has an entry. A scope with no work for a phase points at the
runtime's canonical no-op entry, never at null. Null is reserved for "not supplied," which the
definition builder -- knowing from the unit's MIR which bodies exist -- treats as a linkage error at
build time, not a silent skip. Distinguishing "no semantic work" from "a missing entry" keeps a
dropped generated body from silently becoming a miscompiled simulation. Genuinely optional
capabilities are a distinct optional field or a feature bit, not an overloaded null.

## Migration (phased)

- **Phase 1 -- unify dispatch and metadata.** The runtime dispatches lifecycle through the
  `ScopeProgram`, not through C++ virtuals: the five former virtual consumption points --
  `Scope::Resolve` / `Initialize` / `Activate` and the engine's read of time precision and def name
  -- become three lifecycle entries on the program and two constant fields on the program's metadata
  (read as data, not called). The C++ backend keeps its subclass **as a backend-private state
  holder, allocated by the backend's own allocator** so a body's downcast to that type is valid; the
  runtime no longer reaches it through `Scope*` virtual dispatch. The LLVM/JIT backend supplies its
  entries, reads the metadata off the lowered unit, and allocates a generic instance with
  runtime-owned storage; the adapter is deleted. Each runtime-to-generated call is wrapped in a
  generated-call scope at the runtime's per-call boundary. `construct` is a unit-level entry that
  stays backend-specific (the C++ constructor; a JIT bootstrap-called entry), tied to allocation,
  and Phase 1 does **not** yet unify instance representation, member storage, or the allocation /
  destruction model -- those stay behind the backend allocator boundary above. This removes the
  runtime's dependency on the C++ vtable and the JIT adapter without forcing member-storage layout
  on the C++ backend.
- **Phase 2 -- generic instance.** Member storage is unified through the LIR place model and its
  physical-layout derivation; the backend allocator boundary collapses, the C++ backend drops the
  subclass shell, and instances become generic objects over a definition.

## Rejected alternatives

- **C++ `virtual` subclass as the extension ABI (status quo).** Makes "be a C++ subclass" the
  boundary every backend must mimic; natively serves only the backend whose language is the
  runtime's.
- **A per-backend adapter mirroring the virtual surface.** Workable as a transition, but an adapter
  per non-C++ backend is not the end state; it keeps C++ virtual as the true contract and other
  backends as imitators.
- **The JIT synthesizes a C++-ABI-compatible object and vtable.** Removes the adapter but forces the
  LLVM backend to encode Clang's vtable layout, RTTI, base-subobject offsets, and mangling -- a
  foreign, platform- and compiler-version-specific responsibility. Generated code should not pretend
  to be a C++ frontend.
- **One flat dispatch table collapsing lifecycle and SV virtual.** Couples the closed lifecycle
  contract's evolution to open class-virtual semantics. They share a representation, not a concept.
- **The GC / lifetime principle as the sole justification.** `object_lifetime`'s state-ownership
  rule does not by itself imply data-driven dispatch; using it as the primary argument over-extends
  it. The primary argument is the backend-neutral behavior ABI.
- **One definition on `Scope` with an `Instance`-typed entry receiver.** Conflates scope-level
  behavior (every scope, including a generate scope, has lifecycle bodies) with unit-level identity
  (only the unit root is an `Instance`). A `GenScope` is not an `Instance`, so its lifecycle entry
  cannot take `Instance*`. Behavior and constant metadata belong to the `ScopeProgram` over the
  generic `Scope`; per-specialization identity belongs to the `UnitDefinition` on the `Instance`.
- **Treating a generic instance allocation as sufficient while a backend keeps a concrete state
  holder.** Allocation is not dispatch. A body cannot downcast a generic `Scope` to a concrete type
  the object was not allocated as, and `construct` runs semantics on an existing object rather than
  changing its representation. Allocation stays a backend concern until member storage is unified.

## Consequences

- The runtime's `Scope` holds a reference to a `ScopeProgram` and an `Instance` additionally holds a
  `UnitDefinition`; the engine drives lifecycle through the program, not through C++ virtual
  dispatch. The lifecycle entry points can be non-virtual; whether the runtime keeps C++ virtual for
  its own internal polymorphism is independent of the generated boundary.
- The C++ backend emits native-entry bodies, a `ScopeProgram` / `UnitDefinition`, and an allocator;
  in Phase 1 the allocator constructs the subclass as a private state holder.
- The LLVM/JIT backend supplies its entries by symbol lookup keyed on the specialization identity
  and an allocator for a generic instance; the adapter is removed.
- Member storage for the generic-instance end state is the LIR place model realized by physical
  layout -- the layout work already required for the LLVM backend, not new work introduced here.
- This revisits `callable-receiver`'s rejected "free functions, no class membership" alternative:
  that rejection rested on the runtime reaching instance methods through `Scope*` polymorphism,
  which this decision replaces. The explicit-`self` static-body shape `callable-receiver` fixes is
  retained and reinforced; the "on the class" realization detail is a backend-private choice, not a
  runtime requirement.

### Constraints the realization must hold

- A `UnitDefinition` that references JIT-compiled entries pins the owning JIT module for as long as
  the definition -- and any instance or process reaching those entries -- is live.
- A `UnitDefinition` is complete before `BindDesign`: construct entry, child-definition references,
  and metadata all resolved. The engine performs no symbol lookup during a lifecycle phase.
- Child construction reaches other units through the existing runtime SDK / linked definition
  references (`emission_model`), never through a design-global name registry reintroduced by the
  definition model.

## Cross-references

- `docs/architecture/object_model.md` (dispatch as a logical slot; one override machinery)
- `docs/decisions/callable-receiver.md` (explicit-`self` callable bodies)
- `docs/architecture/activation.md` (payload-neutral activation token)
- `docs/architecture/object_lifetime.md` (semantic state is Lyra-owned)
- `docs/architecture/specialization_model.md` (what a specialization is)
- `docs/decisions/specialization-identity.md` (the deterministic specialization name used for
  linking)
- `docs/architecture/emission_model.md` (per-unit artifact; SDK as cross-unit substrate)
- `docs/architecture/lir.md` (member as a logical place; physical layout below LIR)
- `docs/architecture/scheduling.md` (the engine is a mechanism; the runtime owns sequencing)
