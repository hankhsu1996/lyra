# Decisions

Logged architectural decisions. Each entry records a decision with its rationale; the entry is
immutable once accepted, and a superseding decision links back to the one it replaces.

Decisions are reserved for choices with real trade-offs: rejected alternatives, load-bearing
invariants, or constraints that bind the codebase going forward. Housekeeping notes (e.g., "this
archive item is subsumed by an existing surface") do not warrant a decisions entry; record the
reason inline at the point it matters.

## Index

Grouped by subject so a decision is findable by concept, not only by filename. One line per entry.

### Value types and representation

- [integral-representation](integral-representation.md) -- one fat `PackedArray` carries integral
  shape (width / signedness / 4-state / dims) as runtime fields, not C++ template parameters.
- [value-store-discipline](value-store-discipline.md) -- a value is pure (declared type immutable at
  construction, assignment replaces the whole value); preserving the destination's declared type
  across assignment lives at the store boundary and the cell, not in the value's assignment
  operator.
- [value-type-concepts](value-type-concepts.md) -- the `lyra::value` operator surface is a lattice
  of composable C++ concepts, one per LRM operator family.
- [runtime-shape-and-default-value](runtime-shape-and-default-value.md) -- runtime shape lives on
  `PackedArray`; collections carry one OOB shield slot that is both the canonical-default source and
  the out-of-bounds-write discard target.
- [string-packed-conversion](string-packed-conversion.md) -- a `value::String` holds no NUL;
  packed-to-string conversion strips NUL (LRM 6.16) while `%s` formats bits without building a
  string value (LRM 21.2.1.7).

### Aggregate types and access

- [packed-array-representation](packed-array-representation.md) -- HIR represents a packed array
  recursively (one dim per node, element by `TypeId`, scalar bit as an interned leaf), aligning it
  with the other array families and the interned type graph; MIR stays flat and HIR-to-MIR flattens.
- [unpacked-array-representation](unpacked-array-representation.md) -- representation of a
  fixed-size unpacked array.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- an unpacked struct is the
  generic product type (MIR `TupleType`), not a new variant and not the object model; member access
  is positional, defaults are synthesized at lowering.
- [slice-value-semantics](slice-value-semantics.md) -- a slice read materializes an owned value; the
  access model is value, not borrow / view.
- [queue-operators](queue-operators.md) -- queue access operators (`$`, slice, concatenation,
  equality, append) lower to built-in method calls; read and write are different methods chosen at
  lowering.
- [array-method-dispatch](array-method-dispatch.md) -- LRM 7.12 array-method runtime semantics
  (empty-reduction zero, selection sort over standard introsort); the original per-family dispatch
  shape is superseded by [builtin-call-identity](builtin-call-identity.md).
- [array-manipulation-entry-stream](array-manipulation-entry-stream.md) -- LRM 7.12 locator /
  reduction / `map` families operate over an ordered `(index, element)` entry stream modeled on Rust
  `Iterator` / C++ `std::ranges`; the ordering family stays an in-place positional permutation.
- [format-dispatch](format-dispatch.md) -- value formatting dispatches through `Formatter<T>` and
  `FormatArg`.

### Lowering and IR shape

- [lowering-organization](lowering-organization.md) -- how lowering passes organize their internal
  objects (facts, registries, builders, walk frame).
- [declarations-before-bodies](declarations-before-bodies.md) -- within a compilation unit, every
  structural declaration's identity and shape is CU-global, canonical, and queryable through the
  compilation unit before any executable lowering begins; covers subroutine forward / mutual
  reference, cross-scope hierarchical reference's typed segments, SV class mutual reference, and
  hierarchical callable dispatch.
- [foreach-lowering](foreach-lowering.md) -- the lowering shape of `foreach`.
- [conversion-folding](conversion-folding.md) -- when type conversions are folded.
- [variable-initialization](variable-initialization.md) -- LRM 10.5 variable initialization as a
  constructor-scope statement.
- [variable-lifetime-storage](variable-lifetime-storage.md) -- storage of static-lifetime body
  locals.
- [lifetime-extended-automatic-scope](lifetime-extended-automatic-scope.md) -- an automatic scope
  borrowed by a process that may outlive it is realized as a shared-owned activation object
  (`PointerType{kShared}`'s first producer); a detached branch captures the activation handle by
  value, reached like `self` (`handle->local`), so one shared handle is both access and lifetime.
- [read-set-inference](read-set-inference.md) -- read-set inference via slang flow analysis.
- [runtime-effects-as-generic-calls](runtime-effects-as-generic-calls.md) -- runtime effects
  (`$display`, `$finish`, file IO) lower to ordinary `CallExpr` with the engine handle as one
  argument.
- [callable-receiver](callable-receiver.md) -- every callable body's first binding is `self`; how it
  is supplied differs per callable form.
- [unified-callable-model](unified-callable-model.md) -- one callable concept: callable code
  (signature + internal-body | external-symbol) versus callable value (code + bound environment).
  Result type is the call protocol (`Coroutine<T>`), parameter direction normalizes to data flow
  (`output` / `inout` to an output pack, `ref` to `Ref<T>`), DPI is an external-callable variant,
  process is an instance-bound value registered at constructor time. Target model; not yet built.
- [closure-environment-and-activation-frame](closure-environment-and-activation-frame.md) -- the
  closure environment is a value `TupleType` (not a closure-only capture universe, not an object);
  the callable value gains a concrete type and an erased `Callable<Sig>` with an explicit erasure;
  the activation frame is a thin reference-storage specialization sharing one substrate with the
  nominal object, not a baseless `mir::Class`. Target model; not yet built.
- [builtin-call-identity](builtin-call-identity.md) -- built-in method calls carry a flat
  closed-namespace identifier (`support::BuiltinFn`) shared between HIR and MIR; per-family enum
  splits are out.
- [address-of-primitive](address-of-primitive.md) -- MIR carries an explicit place-to-pointer
  operator (`AddressOfExpr`), dual to `DerefExpr`. Runtime APIs taking pointers receive an explicit
  address-taken operand; the backend never injects `&`.
- [event-control-unification](event-control-unification.md) -- unified treatment of event control.
- [generic-lowering-machinery](generic-lowering-machinery.md) -- generic arena and shared
  context-free expression-handler templates over the pass class; node types stay typed.
- [arena-reference-lifetime](arena-reference-lifetime.md) -- `Arena::Get` is a transient view; a
  same-arena `Add` invalidates prior references, and the `Id` is the only durable handle, so
  lowering projects value facts before mutating; stable-reference storage is rejected.
- [mir-type-interning](mir-type-interning.md) -- the MIR type pool is a structural-equality
  interner, not a plain arena: each semantic type has one canonical `TypeId`, object types keyed by
  class identity (enums by their scope-unique enumerators), enabling recursive class types.
- [context-free-call-lowering](context-free-call-lowering.md) -- one expression dispatcher template
  per boundary (context-free kinds listed once); the call family becomes a template across pass
  classes once the `with`-clause element / index are co-equal closure parameters rather than a
  procedural-body local.

### References and construction

- [hierarchical-reference-routing](hierarchical-reference-routing.md) -- one semantic shape per
  hierarchical reference; per-segment classification by layout visibility (typed for layout-owned
  segments, runtime SDK across unit boundaries); sealed endpoint consumed on the hot path; lexical
  form and source order are not mechanism dispatch keys.
- [binding-graph-resolution](binding-graph-resolution.md) -- resolution and sealing respect
  dependencies between references (a route requiring another's sealed endpoint resolves after it;
  forwarding chains collapse end-to-end); the resolution mechanism itself is not fixed by the
  architecture.
- [hierarchical-reference-resolution](hierarchical-reference-resolution.md) (superseded) -- the
  prior decision, replaced by the two entries above.
- [specialization-identity](specialization-identity.md) -- a specialization's identity is a
  deterministic name (module name + content hash of a canonical, structural serialization of its
  parameter bindings), computed independently by producer and consumer and matched by name;
  injective mangling, body fingerprinting, and a design-global key map are rejected.
- [reference-as-data-type](reference-as-data-type.md) -- a reference is a direction at HIR and a
  data type at MIR; one reference type serves both `ref` formals (body local) and `ref` ports (unit
  member), distinct from a borrowed pointer because it preserves the observable-cell protocol.
- [object-model](object-model.md) -- a module / scope and an SV class are one generic nominal object
  type; an SV class handle is a managed reference, not the acyclic shared one, realized by precise
  tracing GC (arena and reference counting rejected); the receiver is an instance-method property,
  not every callable's.
- [object-model-storage](object-model-storage.md) -- a compilation unit owns one canonical registry
  of local nominal object declarations; identity, lexical name resolution, and backend emission
  nesting are separate relations; the lexical-tree-only storage and a second identity are rejected.
- [elaboration-lifecycle-phases](elaboration-lifecycle-phases.md) -- a generated constructor only
  allocates; SystemVerilog elaboration is a staged build / resolve / initialize / activate protocol,
  with connections declarative until a single resolve phase and initializers running after it;
  recursive-constructor-as-executor is rejected.
- [net-driver-resolution](net-driver-resolution.md) -- a net is a resolution node with node-owned
  driver slots and capability-handle drivers; drivers attach during Resolve along the reference
  route and are validated at a materialized Seal barrier; single-driver is the N=1 case; net and
  variable stay distinct types.

### Compile-time model and specialization

- [parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) -- every
  parameter is treated as code-shape-affecting for now (a conservative over-approximation of the
  specialization key); parameter classification and constructor-input threading are deferred, and
  the instance-array storage shape stays forward-compatible (vector wrapper, count as a
  constructor-side value, never extent-in-the-type).

### Diagnostics

- [diagnostic-construction](diagnostic-construction.md) -- a diagnostic's kind is a property of its
  code, derived at construction; construction is infallible (no re-supplied value, no validating
  guard); the consumer-less `UnsupportedCategory` axis is removed.

## File Naming

`kebab-case.md`. The name describes the decision, not when it was made; the date lives inside the
file. Existing example: `integral-representation.md`.

## Shape

There is no fixed template. The existing `integral-representation.md` is the reference for shape:
title, date, status, the model or findings that shaped the decision, the decision itself, and the
consequences that follow. Let the subject drive the structure; a decision with no rejected
alternative or no load-bearing invariant probably should not be a decision entry.
