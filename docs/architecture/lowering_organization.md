# Lowering Organization

## How to Read This Document

The invariants below are **compressed conclusions, not laws.** Each one was derived from
first-principles reasoning about what state exists in a lowering pass and where it naturally lives.
The condensation is useful as a cache of past thought, but it is not authority.

When a design question arises:

1. Derive the answer from first principles in the new problem's own terms.
2. Then check the answer against these invariants.
3. If the first-principles answer contradicts an invariant, the invariant is the suspect, not the
   analysis -- it may have been over-applied, no longer fit the current shape, or have been wrong
   from the start. Re-derive the invariant from its own justification; if the justification has
   thinned, update the invariant.

Never end a design argument with "...invariant N forces it." End with "...because [first-principles
reasoning]; this matches / contradicts invariant N, which we should keep / revise."

## Notation

Each invariant has two parts:

- The **invariant body** states a constraint: a property the pass shape must preserve, expressed in
  terms that survive renames and implementation refactors. No file names, no identifier names, no
  thresholds.
- A _Current implementation:_ sub-line (when grounding is useful) names the concrete shape that
  currently satisfies the constraint. This is illustrative, not normative. When the implementation
  is refactored, only this sub-line is updated; the constraint body stands.

If a future design finds a different mechanism that satisfies the same constraint, the _Current
implementation:_ line moves; the constraint does not. Forbidden Shapes are the inverse: patterns
ruled out by the constraint, irrespective of how the constraint is currently realized.

## Purpose

Define how a lowering or rendering pass is structured: the class shape, the dispatcher method
signature, the three kinds of fields a class member may be, the per-kind handler shape, and the
rules that keep new concepts cheap to add and the class bounded.

## Owns

- The pattern that every lowering or rendering pass is implemented as a class scoped to one task
  instance (one process, one scope, one unit, etc.).
- The classification of every class member into one of three kinds: facts, registries, and the owned
  root output. Nested builders opened during the walk are stack-allocated inside the walker that
  opens them; they are never class members.
- The dispatcher method signature shape: two parameters (the node, the walk frame) and a
  `diag::Result<T>` return when the method can emit a diagnostic. The dispatcher is a method on the
  pass class; per-kind handlers it routes to are free functions in subsystem files.
- The constructor convention: facts are injected at construction; registries are owned by the class;
  the root output is constructed in the constructor (or at `Run`'s entry) and moved out of the class
  by `Run`'s return.
- The rule that the pattern is shared across lowering and rendering passes while concrete types are
  per pass.

## Does Not Own

- The concrete data carried by any one pass's class.
- IR layer shape (`hir.md`, `mir.md`, `lir.md`).
- Lowering permissions (`lowering_boundaries.md`).

## Core Invariants

1. Every lowering or rendering pass is implemented as a class named for the unit of work it
   processes and the action it performs (`ProcessLowerer`, `CppProcessRenderer`,
   `StructuralScopeLowerer`). The class is constructed once per task instance and destroyed when
   that task completes; one class instance does not span multiple task instances.

2. The class constructor receives the read-only facts the pass depends on. After construction those
   facts are not mutated. Registries the pass accumulates and the root output the pass is
   constructing are class members owned by the class instance, with lifetimes equal to the class
   instance's lifetime. Nested builders opened mid-walk are not class members.

3. The class exposes a public entry method (typically `Run`) that performs the task and returns its
   output, together with public dispatcher methods (`LowerExpr`, `LowerStmt`, `InternType`, etc.)
   that per-kind handlers recurse through. Internal helpers that are not recursion entry points are
   private. All methods on the class access class state through `this`.

4. Dispatcher method signatures are exactly
   `(const NodeType& node, WalkFrame frame) -> diag::Result<OutputType>` (or the raw `OutputType`
   when the method cannot emit a diagnostic). Dispatcher methods do not receive per-pass state as
   separate parameters; per-pass state lives on the class. Per-kind handlers, in contrast, are free
   functions in subsystem files with signature
   `(PassClass& pass, WalkFrame frame, const NodeKind& node, diag::SourceSpan span) ->  diag::Result<OutputType>`
   -- the pass class instance is passed explicitly because the handler lives outside the class, but
   it is the same single access channel (no separately-narrowed facts / registry / builder
   references in the signature).

5. Every class member is one of three kinds: **facts** (set at construction, never mutated
   thereafter), **registries** (append-only accumulators whose API exposes `Add` and `Lookup`), or
   the **owned root output** the pass exists to produce.
   - **Root output ownership.** The root lives as a member on the pass class, with lifetime equal to
     the pass instance, shared by every handler. `Run` moves it out at the end; after `Run` returns,
     the class holds no IR pointer. This is the load-bearing no-leak guarantee.
   - **Root output access.** Read access is exposed as a `const` accessor returning the in-progress
     output by reference; downstream consumers post-`Run` use the same interface, so no parallel
     read API exists on the pass class. Write access is exposed as narrow methods that encapsulate
     cross-state invariants the root cannot enforce alone (e.g., type dedup that coordinates the
     output's type table with a slang-keyed cache living in a class registry). Trivial delegating
     wrappers (a class method whose body is `unit_.AddX(...)` with no other state) are forbidden;
     they bloat the class surface without encapsulating anything.
   - **Nested builders are separate.** IR scopes opened mid-walk by a handler (a procedural scope
     for an if-branch, a closure body, a fork-branch body) are stack-allocated inside the walker
     that opens them, populated by recursion through `frame.current_*_scope`, finalized by move when
     the scope closes, and embedded into their owning slot in the parent IR. They are never class
     members.

   This three-way split (root on the class; nested builders on the walker stack; the rest on the
   class as facts / registries) is derived from actual lifetime and sharing reality, not from a
   uniform-treatment principle. An earlier formulation said "all builders including the root are
   stack-allocated inside `Run`, never class members," which conflated the legitimate no-leak
   constraint with an unjustified extension that the root must live on `Run`'s stack. The
   unjustified extension forced the root onto `WalkFrame`, which forced misleading `current_*`
   naming onto a slot that is invariant across the entire walk -- units are mutually isolated by
   contract, so there is only one unit per pass, never a "current vs other" notion. Owning the root
   on the class corrects that misplacement.

6. `WalkFrame` is a small value type holding **per-recursion traversal context only**: a pointer to
   the current write-target nested builder, the current procedural depth, an optional closure
   context, and any future stack-discipline state. It is passed by value between walker methods.
   Each recursion may construct a new frame with different fields set; the cost of copying is
   trivial. Walk-invariant facts (the unit currently being constructed, source mapper, builtins
   table) are class members on the pass class, not `WalkFrame` fields -- only state that genuinely
   changes from one recursion to the next belongs on `WalkFrame`. Writes to nested scopes go through
   `frame.current_*_scope->Add...`; writes to the root output go through the pass class's narrow
   methods (see invariant 5).

7. Adding a new fact is a class-member addition and a constructor-parameter addition. It does not
   change dispatcher or per-kind-handler signatures. Adding a new traversal-time concept is a
   `WalkFrame` struct-field addition. It does not change dispatcher or per-kind-handler signatures
   and it does not add a class member. This rule is the contract's load-bearing scalability promise.

8. The `const` qualifier marks a member's access pattern, not the class. A `*State` suffix does not
   appear on any pass class, any member type, or any field name. Type names describe what the type
   represents.

9. The pattern (class plus dispatcher methods plus per-kind handler free functions plus `WalkFrame`)
   is identical across lowering and rendering passes. Concrete types (`ProcessLowerer`,
   `CppProcessRenderer`) are per pass; no base type spans lowering or rendering pass boundaries.

10. Per-kind dispatch is centralised in exactly one location, and that location is decoupled from
    the pass class header so the header does not grow per kind. Per-kind handlers form a layer the
    pass class delegates to; they are not part of the class's public surface.

    _Current implementation:_ the dispatcher is a method on the pass class with body = one switch
    routing to per-kind handler free functions in subsystem files. Per-kind handlers have signature
    `(PassClass&, WalkFrame, const NodeKind&, diag::SourceSpan) -> diag::Result<T>`. Adding a kind
    touches the switch (in the class's `.cpp`), the subsystem header (declaration), and the
    subsystem `.cpp` (definition) -- the class header is not touched.

11. Per-kind handlers are grouped by semantic family (operators, references, calls, selects,
    aggregates, etc.) and live in subsystem header / implementation pairs. The boundaries of a
    subsystem are determined by LRM-level expression / statement family, not by individual node
    kind. A subsystem header declares the handlers it owns; a subsystem implementation defines them.
    The pass class's `.cpp` (which defines the dispatcher method) includes every subsystem header to
    wire the switch.

12. Subsystem files depend on exactly one contract surface for recursion -- the pass class itself.
    No parallel dispatch-contract carrier exists in any form (an external declaration file, a
    separate adapter type, a registration table, a function-pointer registry). Cross-lowerer
    operations reachable from a handler are reached by traversing fact references the pass class
    already holds, not through an external resolution mechanism.

    _Current implementation:_ the dispatcher is a method on the pass class declared in its header.
    Subsystem `.cpp` files include the pass class header to accept the instance by reference and to
    recurse via `lowerer.LowerExpr(...)` / `lowerer.LowerStmt(...)` /
    `lowerer.Module().InternType(...)`. `proc.Module()` returns the `ModuleLowerer&` the
    `ProcessLowerer` captured at construction.

## Boundary to Adjacent Layers

- A pass class is a layer-internal artifact. It is not exposed across lowering or rendering
  boundaries. The output of the pass (the constructed IR, the emitted source text) is the contract
  surface visible to downstream consumers.
- The class instance's lifetime is bounded by one task. The output is finalized in `Run` and
  returned by value; the class is destroyed.

## Forbidden Shapes

- A pass implemented as free functions threading a context-like object as a separate parameter.
- A `RenderContext`, `LoweringContext`, or any other `*Context` type used as the carrier for facts,
  builders, or walk-frame state. The name is an open invitation to unbounded growth.
- A class instance shared across multiple task instances (one `ProcessLowerer` reused for several
  processes).
- A class member whose lifetime exceeds the class's task instance.
- A dispatcher method on the pass class that takes per-pass state as a separate parameter alongside
  the `WalkFrame`. Per-pass state lives on the class and is reached through `this`. (Per-kind
  handler free functions are a different shape: they take the pass class instance explicitly because
  they live outside the class, but the instance is the single access channel -- no further narrowed
  references.)
- An immutable-context-with-`With*()` copy-on-descend pattern. The pattern's superficial appeal is
  symmetry with the walk frame; its cost is the Context name's open invitation, the `mutable` escape
  hatch required for accumulating state, and the parallel structure needed for the class that owns
  the accumulators.
- A walker struct passed as the single fat parameter. Same pathology as a context, with a different
  name.
- A `*State` suffix on a class, a member, or a field.
- A class member representing traversal state. Traversal state belongs to `WalkFrame`.
- A borrowed reference (`&` / pointer-to-stack) to the in-flight output held on a class member. The
  root output is the pass's own deliverable -- it is owned by the class as a real member, moved out
  by `Run`, and the class holds no IR pointer after `Run` returns. Nested scopes opened during the
  walk (a `mir::ProceduralScope` for an if-branch, a closure body, a fork-branch body) remain
  stack-local to the walker that opens them; only the pass's single root output lives on the class.
- A trivial delegating method on the pass class that forwards a single call into the in-flight
  output or the current nested scope (e.g., `lowerer.AddStmt(...)` whose body is
  `frame.current_scope->AddStmt(...)`, or `lowerer.AddType(...)` whose body is `unit_.AddType(...)`
  with no other state touched). Writes to nested scopes go directly through
  `frame.current_*_scope->Add...`; writes to the root output go through the IR's own interface
  reached via the const accessor (for reads) or through a narrow method on the class only when the
  write must coordinate multiple pieces of class state (e.g., dedup that touches both the output's
  type table and a registry cache). The distinguishing test: a method whose body touches exactly one
  member and adds no invariant is a forwarder and must be removed; a method whose body touches
  multiple members in coordination is encapsulating an invariant and is allowed.
- An ambient setter / getter pair on the class whose only purpose is install / restore of a
  walk-position concept.
- An `Add*Expr` helper that wraps `scope.AddExpr(Make...(...))` for a freshly built node and returns
  its `mir::ExprId`. A freshly built node is returned as a detached `mir::Expr` by a `Make*` /
  `Build*` factory and interned by the caller; `mir::ExprId`-returning helpers are reserved for
  transforms keyed on an input `mir::ExprId` (which read or pass through an existing arena node).
  See "Expression-Builder Helpers".
- A shared base class spanning lowering and rendering pass classes. The pattern is shared; types are
  per pass.
- A dispatcher method or per-kind handler that mutates the class's fact members. Facts are mutated
  only by the constructor.
- A bundle of multiple kinds (facts + registry + builder) into one type. Each member of a class is a
  single-kind object; a bundle that mixes kinds is a god object regardless of size.
- A separate dispatch-contract header (e.g. `dispatch.hpp`) declaring the dispatcher entries outside
  the pass class. The dispatcher is a method on the pass class; subsystem files reach it through the
  class header. Maintaining a parallel contract header duplicates the surface and adds a
  synchronization point that does no work.
- Per-kind handlers expressed as methods on the pass class. Methods grow the public class
  declaration and force every translation unit that includes the class header to recompile when a
  kind is added. The per-kind layer is free functions in subsystem files; the pass class exposes
  only the three kinds of members, the dispatcher methods, and the walker entry method.
- Per-kind handlers requiring narrowed access (separate facts / registry / builder references in the
  signature). The pass class instance is the single access pass for the task; narrowing the
  signature reproduces the sig-explosion shape this contract exists to escape.

## Multi-File Organization Within a Pass Layer

When a layer's expression / statement dispatch grows past a single readable file, the layer is split
into the pass class itself and one subsystem file per expression / statement family. The shape is
fixed:

- The pass class header (`process_lowerer.hpp`, `structural_scope_lowerer.hpp`,
  `module_lowerer.hpp`) declares the dispatcher methods (`LowerExpr`, `LowerStmt`, `InternType`,
  etc.), the registry operations, and the fact accessors. Per-kind handler declarations are not on
  the class.
- The pass class implementation (`process_lowerer.cpp`, etc.) defines the dispatcher methods. Each
  dispatcher method body is a single switch over node kind, with each case routing to a per-kind
  handler in a subsystem file.
- One subsystem header per family declares the per-kind handlers for that family as free functions
  (`operators.hpp` for unary / binary / conditional / conversion; `calls.hpp` for the call
  expression; `selects.hpp` for element-select / range-select / member-access; `aggregates.hpp` for
  concat / replication / assignment-pattern variants; `references.hpp` for name resolution;
  `assignment.hpp` for assignment / increment-decrement and their assignability validation;
  `inside.hpp` for the inside operator; analogous families on the statement side). Per-kind handler
  signatures are `(PassClass&, WalkFrame, const NodeKind&, diag::SourceSpan) -> diag::Result<T>`.
- One implementation `.cpp` per subsystem header defines the family's handlers and any
  anonymous-namespace helpers private to the family. Subsystem `.cpp` files include the pass class
  header (to accept the pass class instance as the first parameter and to recurse via
  `lowerer.LowerX(...)`) and the subsystem header.

Adding a new expression / statement kind touches three files: the subsystem header that owns its
family (declaration), the subsystem implementation (definition), and the pass class's `.cpp` (one
new switch case in the dispatcher method body). Other subsystem `.cpp` files do not recompile, and
the pass class header is not modified.

Adding a new family creates a new header / implementation pair and adds the corresponding switch
cases in the dispatcher method body. Existing families are not touched.

The pass class header's public surface is bounded: the dispatcher methods (one per node category),
the registry operations, the fact accessors, and the `Run` entry. Per-kind handlers are not methods
on the pass class -- they are free functions in subsystem files, so adding a kind does not bloat the
class header or trigger a header-level recompile cascade.

## Expression-Builder Helpers

Helpers that construct MIR expressions come in two shapes, distinguished by whether the helper is
keyed on an input `mir::ExprId`:

- **Factory** -- builds a fresh node (or a subtree whose top is a fresh node) from inputs that are
  not an arena handle: a `mir::TypeId`, an HIR node, a literal value, or the body's `self`. A
  factory returns the top node as a detached `mir::Expr`; the caller interns it with
  `scope.AddExpr(...)`. A composite factory interns its own children into
  `frame.current_procedural_scope` as it builds and returns only the top detached, so interning the
  result yields a self-contained arena subtree. The `mir::Make*Expr` family (pure structural
  constructors) and the lowering `Build*Expr` family (frame-aware) are factories. Examples:
  `MakeProceduralVarRefExpr`, `BuildSelfRefExpr`, `BuildServicesCallExpr`, `BuildDefaultValueExpr`,
  `BuildArrayConstructExpr`.
- **Transform** -- keyed on an input `mir::ExprId`: it reads `scope.GetExpr(id)` to decide what to
  build, or may return the input unchanged (pass-through). A transform returns a `mir::ExprId`,
  because it operates on already-interned arena nodes. Examples: `WrapUnpackedIndex` (returns its
  index input unchanged when the declared range needs no rebase), `CloneLhsExprForNbaBody` (rebuilds
  an lvalue node's structure into a closure body).

The rule follows from the dispatcher contract: `LowerExpr` returns a detached `mir::Expr` whose
children are already interned, so every hand-written expression builder obeys the same shape. The
only way to obtain a `mir::ExprId` is `scope.AddExpr(<an Expr produced by a factory>)`; a freshly
built node is never interned behind an `Add*` helper that hands back the id.

## Notes

The three kinds of class members correspond to distinct access patterns:

- **Facts** are constructor inputs. They are referenced from member functions through `this` and are
  never mutated post-construction. Examples: an injected type-lookup table, an injected builtins
  struct, an injected HIR module reference, an injected time resolution.
- **Registries** accumulate append-only entries. Their API exposes `Add` and `Lookup`; the contract
  is enforced by the registry type's surface, not by external `const` qualifiers. Examples: a
  procedural-var binding table, a static-locals list, a HIR-to-MIR type-translation map. Some
  registries are keyed by data from an adjacent layer (e.g., a slang-keyed type cache on an
  AST-to-HIR Lowerer); such registries cannot move into the IR they index into, because doing so
  would force the IR layer to depend on the adjacent layer it was deliberately built to be
  independent of.
- **Root output** is the IR the pass exists to produce. It lives as an owned member on the pass
  class with lifetime equal to the pass instance; every handler shares one. The class exposes read
  access via a `const` accessor (`Unit()` returning `const hir::ModuleUnit&`, or its equivalent per
  pass) -- the same interface downstream consumers use after `Run` returns. Writes that affect only
  one member are forbidden as trivial wrappers; writes that coordinate multiple pieces of class
  state are exposed as narrow methods that encapsulate the invariant (the canonical example:
  `InternType` ties the output's type table together with a slang-keyed cache, so the dedup
  invariant is enforced structurally). `Run` moves the root output out at the end; after `Run`
  returns, the class holds no IR pointer. This keeps the legitimate no-leak constraint while putting
  the root in the same scope as the registries that index into it, and keeps `WalkFrame` free of any
  "current vs other" pointer for something that has only ever been one.

Nested builders are not class members. They are the mutable IR scopes opened during the walk (e.g.,
a `mir::ProceduralScope` for an if-branch body, a closure body, a fork-branch body). Each is
stack-allocated inside the walker that opens it, populated by recursion, finalized by move when the
scope closes, and embedded into its owning slot in the parent IR. The walk frame carries the
current-scope pointer so every nested-scope write goes through one uniform call shape
(`frame. current_*_scope->Add...`) regardless of how deeply nested the current scope is.

Entry points outside the pass (e.g., a driver that constructs one `ProcessLowerer` per process)
construct the class with its facts, invoke `Run`, and consume the returned IR or text. They do not
see the class's internal walkers.
