# MIR

## Purpose

Define the object-oriented semantic IR. MIR expresses the compiled program as objects, members,
callables, and actions, with lightweight structured control flow. MIR is not C++; C++ is one of
several possible backend targets for MIR, and does not define MIR's semantics.

## Owns

- Objects, members, and member access as first-class entities.
- A member as a `(name, type)` pair. The type is the sole carrier of the member's storage shape and,
  for a member that owns a child, of its child-scope kind and cardinality.
- The type system: value types (integral, real, string, event, ...); object types in two forms -- an
  intra-unit object (a structural scope of this unit) and an external-unit object (another
  compilation unit, named); and two composing wrappers, owning pointer and vector.
- Callables: functions, tasks, constructors, processes, and assertion actions.
- The identity of each object and member within a compilation unit.
- Lightweight structured control flow inside a callable body: `if`, loop, and sequence. No basic
  blocks at this layer.
- A primitive expression set: literals, references, unary / binary / conditional operators, calls,
  conversions, closures, element- and range-selects, concat, replication, array-literal.
  SystemVerilog syntactic sugar that decomposes into this set (`case`, `inside`, `casez` / `casex`,
  `foreach`) is lowered at the HIR-to-MIR boundary and never carried as a MIR node.
- Action shapes for constructs that bind behavior to schedule events (always blocks, continuous
  assignments, deferred assertions, concurrent assertions).
- A textual dumper that serializes MIR for inspection, validation, and golden testing. The dumper is
  not a backend; its output is not executable.

## Does Not Own

- Control-flow graphs, basic blocks, branches, or phi nodes.
- Storage placement, offsets, or memory layout.
- Scheduling, dirty tracking, or wakeup filtering.
- The frontend's symbol or string identity. MIR carries only MIR's own ids.

## Core Invariants

1. Every compiled entity is exactly one of: an object, a member of an object, a callable, or an
   action bound to a callable.
2. All hierarchy is expressed as object ownership. A hierarchical reference is navigation on the
   object graph, not a lookup in a side table.
3. Generate is lowered to constructor-time construction logic. A module's constructor builds its
   children via `if`, loop, and sequence over construction actions.
4. MIR ids are the single source of identity at this layer. Earlier-layer ids are not carried
   through.
5. MIR admits a pure textual dumper that is lossless with respect to MIR identity and structure. The
   dumper is the golden-test surface at this layer; it is not a compilation path.
6. MIR does not reconstruct topology or identity from side tables. A member is owned by exactly one
   object; a callable by exactly one owner.
7. A member is a `(name, type)` pair. An owned child -- a module instance or a named generate scope
   -- is a member whose type is an owning pointer to an object type. An array of children is a
   member whose type is a vector of that owning pointer; a multidimensional array nests the vector
   wrapper. No member carries a classification flag beside its type; the type is the classification.
8. An object type is exactly one of two forms: intra-unit, naming a structural scope of this unit,
   or external-unit, naming another compilation unit. This single distinction is the owned child's
   runtime scope kind -- a named generate scope versus a module instance. Nothing else encodes scope
   kind.
9. Instance multiplicity is the vector wrapper, a property of the member's type, and is orthogonal
   to whether the owned object is an intra-unit scope or an external unit. The same wrapper composes
   over either object form and to any depth.

## Boundary to Adjacent Layers

- Consumes HIR. HIR-to-MIR lowering produces MIR objects and members from HIR declarations and MIR
  callables from HIR processes, functions, tasks, and assertions.
- Produces input to LIR. MIR-to-LIR lowering introduces CFG structure, storage placement, and
  execution-oriented details.

## Forbidden Shapes

- Basic blocks, successor lists, or phi nodes in MIR nodes.
- Storage offsets, byte layouts, or alignment data in MIR nodes.
- A secondary hierarchy or identity system that shadows MIR ids (coordinate tuples, ordinals, symbol
  paths used as identity).
- Global semantic lookup used inside MIR. Relationships live on the owning object.
- A parallel list, flag, or side field that classifies members -- signal versus owned child,
  instance versus generate scope, scalar versus array -- outside the type system. The type is the
  classification, and a consumer reads it from the type.
- Re-deriving a member's scope kind, cardinality, or storage shape from anything other than its type
  (its name, a naming convention, an enclosing-construct category, or a separate enum).
- Side tables that recover topology from keys rather than direct ownership.
- Deferred or concurrent assertions represented as flags or lowering-pass side effects instead of
  explicit callables.
- A MIR node that preserves SystemVerilog syntactic sugar as an opaque payload: a `CaseStmt` /
  `CaseInsideStmt` in MIR, an `InsideExpr` / `CasezExpr` / `CasexExpr` in MIR's expression set, a
  `ForeachStmt` in MIR. Sugar of this kind is desugared to primitives upstream.
- A runtime helper invocation that wraps a primitive operator family as an opaque call to recover
  source-level shape (e.g., `Inside(lhs, items)`, `CaseMatch(sel, labels)`). Sugar collapses to
  primitives in MIR; readability of generated backend source is not recovered by reintroducing
  intrinsic-style calls downstream.

## Notes / Examples

Reviewing MIR via the dumper should feel like reading the compiled program's structure. The C++
backend emits MIR to C++ source: it is a backend output, not a debug view. The object-oriented
semantic model maps naturally to C++ surface syntax, but C++ is not MIR's semantics and more than
one backend may exist.

A module instance, a named generate scope, and an instance array differ only in one member's type:
`owning-ptr(external-unit object)`, `owning-ptr(intra-unit object)`, and `vector(owning-ptr(...))`
respectively. A two-dimensional array is `vector(vector(owning-ptr(...)))`. Because the
classification lives entirely in the type, a consumer that recurses on the type -- a vector layer
recurses on its element, an owning pointer to an object reaches the object -- handles every
combination, including arrays of any dimensionality, with no per-form branch. A consumer that
switches on whether a member "is an instance" or "is a generate scope" or "is an array" is
reconstructing what the type already states.

The primitive expression set above is shaped by the downstream optimizer. Each backend
(`backend::cpp` today, the LLVM backend via LIR) consumes MIR as primitives that its optimizer can
fold, propagate, and pattern-match across. A sugar node carried into MIR forces every backend to
either re-implement the desugaring or emit a runtime helper call; in the LLVM path the helper
appears as an opaque call through which constant folding, dead-arm elimination, and jump-table
synthesis cannot reach, and which LTO mitigates but does not eliminate. Backend-local readability of
generated artifacts is a debug concern and is recovered through value-type C++ operator overloads in
the runtime, not through sugar nodes in MIR.

Constructs in the expression set that look like sugar are not. `ConditionalExpr` (`?:`) is preserved
because MIR's `if` is a statement; there is no primitive rvalue branching in MIR's expression set
that decomposes the ternary. `ConcatExpr`, `ReplicationExpr`, and `ArrayLiteralExpr` are value-build
primitives without a smaller decomposition. Select expressions are access primitives. Each of these
stays in MIR for the same reason: removing it would require expanding into a statement-form rewrite
that does not fit the expression context.
