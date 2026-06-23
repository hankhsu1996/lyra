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

A pass over one IR comes in two kinds, divided by whether it accumulates task-lifetime shared state:

- A **construction pass** builds a structured output whose parts cross-reference each other and
  accumulates shared registries (a dedup cache, a binding table) as it walks. HIR-to-MIR lowering is
  one; a future MIR-to-LLVM backend -- accumulating an SSA value map, building a cross-referenced
  function -- is another.
- A **rendering fold** maps each node to a fragment of an unstructured medium and composes the
  fragments by concatenation, accumulating nothing. The MIR-to-C++ text backend is one: a node's
  rendered text never references another node's, so there is no shared output to own.

The dividing question is always "what task-lifetime shared mutable state exists?" -- registries
justify the class; their absence is what makes a fold a fold. This document defines the
construction-pass shape (the class, the dispatcher signature, the three member kinds, the per-kind
handler shape, the `WalkFrame`); the rendering-fold shape (free functions, a read-only lookup
threaded down, a per-callable-body temp counter) is defined separately under "Rendering Folds."

## Owns

- The pattern that every construction pass is implemented as a class scoped to one task instance
  (one process, one scope, one unit, etc.).
- The classification of every class member into one of three kinds: facts, registries, and the owned
  root output. Nested builders opened during the walk are stack-allocated inside the walker that
  opens them; they are never class members.
- The dispatcher method signature shape: two parameters (the node, the walk frame) and a
  `diag::Result<T>` return when the method can emit a diagnostic. The dispatcher is a method on the
  pass class; per-kind handlers it routes to are free functions in subsystem files.
- The constructor convention: facts are injected at construction; registries are owned by the class;
  the root output is constructed in the constructor (or at `Run`'s entry) and moved out of the class
  by `Run`'s return.
- The rule that the construction-pass pattern is shared by every accumulating pass (HIR-to-MIR
  lowering, a future IR-emitting backend) while concrete types are per pass, and that a rendering
  fold is a distinct shape, not an instance of it.

## Does Not Own

- The concrete data carried by any one pass's class.
- IR layer shape (`hir.md`, `mir.md`, `lir.md`).
- Lowering permissions (`lowering_boundaries.md`).

## Core Invariants

1. Every construction pass is implemented as a class named for the unit of work it processes and the
   action it performs (`ProcessLowerer`, `ClassLowerer`). The class is constructed once per task
   instance and destroyed when that task completes; one class instance does not span multiple task
   instances. A rendering fold is not a class (see "Rendering Folds").

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
   - **Nested builders are separate.** IR blocks opened mid-walk by a handler (a block for an
     if-branch, a closure body, a fork-branch body) are stack-allocated inside the walker that opens
     them, populated by recursion through `frame.current_block`, finalized by move when the scope
     closes, and embedded into their owning slot in the parent IR. They are never class members.

   This three-way split (root on the class; nested builders on the walker stack; the rest on the
   class as facts / registries) is derived from actual lifetime and sharing reality, not from a
   uniform-treatment principle. An earlier formulation said "all builders including the root are
   stack-allocated inside `Run`, never class members," which conflated the legitimate no-leak
   constraint with an unjustified extension that the root must live on `Run`'s stack. The
   unjustified extension forced the root onto `WalkFrame`, which forced misleading `current_*`
   naming onto a slot that is invariant across the entire walk -- units are mutually isolated by
   contract, so there is only one unit per pass, never a "current vs other" notion. Owning the root
   on the class corrects that misplacement.

6. `WalkFrame` is a construction pass's **walk position** (see "The Walk Position"): a small value
   type holding **per-recursion traversal context only** -- the scope chain plus a pointer to the
   current write-target nested builder, the current block depth, an optional closure context, and
   any future stack-discipline state. It is passed by value between walker methods. Each recursion
   may construct a new frame with different fields set; the cost of copying is trivial.
   Walk-invariant facts (the unit currently being constructed, source mapper, builtins table) are
   class members on the pass class, not `WalkFrame` fields -- only state that genuinely changes from
   one recursion to the next belongs on `WalkFrame`. Writes to nested scopes go through
   `frame.current_*_scope->Add...`; writes to the root output go through the pass class's narrow
   methods (see invariant 5).

   _Current implementation:_ AST-to-HIR splits the walk position's write target by what the write
   needs, not into one uniform handle. Lowering an expression is a single arena append and is
   identical in either pass class, so the frame carries one shared expression sink (the current expr
   arena, reached through `Exprs()` and set on scope/body entry) that every expression handler
   appends into regardless of context -- this is precisely what lets the expression handlers be one
   template per kind rather than a procedural/structural pair (invariant 13). Statements, locals,
   and members stay behind the owning write target (the procedural body or the structural scope)
   because their writes are compound, not bare arena appends: declaring a local also registers a
   symbol binding on the pass class, a loop label bumps a counter on the body, a member append feeds
   scope-specific arenas. The asymmetry is intended -- the expression sink is shared because the
   write is context-free; the statement and member targets stay context-bound because the writes are
   not.

7. Adding a new fact is a class-member addition and a constructor-parameter addition. It does not
   change dispatcher or per-kind-handler signatures. Adding a new traversal-time concept is a
   `WalkFrame` struct-field addition. It does not change dispatcher or per-kind-handler signatures
   and it does not add a class member. This rule is the contract's load-bearing scalability promise.

8. The `const` qualifier marks a member's access pattern, not the class. A `*State` suffix does not
   appear on any pass class, any member type, or any field name. Type names describe what the type
   represents.

9. The dividing line between the class pattern and the fold shape is whether the pass accumulates
   task-lifetime shared state -- not whether it is called "lowering" or "rendering." Every
   construction pass (accumulates registries, builds a cross-referenced output) takes the
   class-plus-dispatcher-plus-handler-plus-`WalkFrame` shape, whether it lowers HIR to MIR or emits
   MIR to a structured backend IR. A rendering fold (composes an unstructured medium, accumulates
   nothing) takes the fold shape instead (see "Rendering Folds"). The earlier formulation claimed
   the class pattern was identical across lowering and rendering; that conflated a structured
   backend (which does accumulate, so is a construction pass) with a text fold (which does not).
   Concrete types are per pass; no base type spans pass boundaries.

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

13. A per-kind handler whose logic is shared across pass classes is one function template
    parameterized over the pass class, never a per-pass-class twin. A node's meaning that does not
    depend on which pass class encloses it -- an expression operating on already-lowered operands --
    is context-free, and its handler reaches the pass class through a uniform duck-typed surface
    (the recursion entry point, the sub-node accessor) rather than a shared base class. A handler
    stays per-pass-class only where the two genuinely differ: name resolution, where a bare name
    maps to different storage depending on the enclosing scope, and kinds that exist in only one
    context.

    _Current implementation:_ the context-free expression handlers (operators, selects, aggregate
    value-builds) are single function templates over the pass class on both lowering boundaries. At
    HIR-to-MIR the template recurses through `LowerExpr` / `LowerLhsExpr` and reaches
    sub-expressions through a uniform `HirExprs()` accessor; at AST-to-HIR it recurses through
    `LowerExpr` and interns through the walk position's single expression arena, so the pass class's
    own surface is just `LowerExpr` and `Module()`. Each is constrained on an `ExprLowerer` concept,
    with explicit instantiations for the two pass classes in the subsystem `.cpp`. Name resolution
    (`references.cpp` on each boundary) stays a per-pass-class pair, and kinds that exist in only
    one context (increment / decrement, replication, the dynamic-array constructor, queue `$`) stay
    procedural-only handlers.

## The Walk Position

Every pass over an IR is a recursion, and at each node the recursion needs context that is not in
the node but follows from where the node sits in the tree. That context is the **walk position**,
threaded by value down the recursion. Its universal core is the **scope chain**: a hops-relative
reference (a local reference at `hops > 0`, a member reference at `hops > 0`) resolves only by
climbing the chain of enclosing scopes. Every pass -- lowering, fold, or a future structured backend
-- carries this chain and resolves hops the same way (`EnclosingClassAtHops`, `BlockAtHops`).

What a pass adds on top of the scope chain follows from what it does:

- A **construction pass** adds write-targets (the nested scope being built) and the decision state
  it uses to compute what to build: the block depth (to _compute_ a reference's hop count at the
  write site), the active capture sink, the lvalue-target flag, the `self` binding, the
  coroutine-body flag. This is the fat walk position -- `WalkFrame` in HIR-to-MIR.
- A **fold** adds nothing. Its walk position is the read-only scope chain alone, because every
  decision the construction pass made is already baked into the MIR it reads. It does not _compute_
  a hop count; it _resolves_ an already-baked one to a name. This is the thin walk position -- the
  MIR-to-C++ read-only scope view.

The walk position thins as the pipeline descends: each pass that bakes a decision into the IR
removes state a later pass would otherwise carry. HIR-to-MIR carries a fat frame because it is still
deciding hops, captures, and callees; the C++ fold carries almost nothing because those decisions
are now MIR nodes. The same force that let R12 / R16 / R17 strip render-time decisions is what thins
the walk position.

Two consequences for type structure:

- **The concept is shared; the type is per pass.** A construction pass's `WalkFrame` and the fold's
  scope view are two instances of one concept, not one type. Merging them would drag the
  construction-only fields (write-targets, depth, capture sink) into a fold that never uses them.
  The class-chain mirror is already deliberate -- `WalkFrame`'s scope-chain node carries a comment
  that it mirrors the render side's parent link so both reach the same `MemberDecl` -- and naming
  the render side to match makes that mirror explicit instead of comment-enforced.
- **A fold need not split invariant facts from the walk position.** A construction pass keeps
  walk-invariant facts (the unit, builtins) on its class and only per-recursion state on
  `WalkFrame`, because facts must stay immutable and stay out of the copied-per-recursion frame. A
  fold has no class and mutates nothing, so its one immutable threaded value carries both the
  walk-invariant lookup (the unit, for type and arena reads) and the per-recursion scope chain. The
  split that protects a construction pass buys a fold nothing.
- **The two passes resolve the chain differently per axis, and that is correct.** For a local
  reference, a construction pass computes the hop count from a depth counter plus its binding
  registry (symbol -> declaration depth) and never reads the ancestor block; the fold has no
  registry, so it climbs the block chain to read the referenced var's name. The fold's block chain
  is the read-side substitute for the construction pass's binding registry -- the same
  registry-presence distinction that separates the two pass kinds. The class chain, by contrast,
  both passes climb, because a member's declared MIR storage type lives only in the constructed
  class and must be read there by whoever needs it (a member reference, or a static-lifetime local
  promoted to a per-instance member). Forcing the two axes onto one mechanism would either carry a
  chain a construction pass never reads or demand a name a counter cannot produce.

## Rendering Folds

A rendering fold emits an unstructured medium (today, C++ text) from an IR. It is not a construction
pass and does not take the class shape, because the two things the class exists to own are both
absent: there is no shared registry to accumulate (a node's text is computed and returned, never
written into a cross-walk accumulator), and there is no structured root to own (the output is text
composed by concatenation, with no node referencing another). What remains is genuinely small:

- Per-kind handlers are free functions that return the rendered fragment for their node; a parent
  composes its children's fragments.
- The only context threaded down is the fold's thin **walk position** (see "The Walk Position"): a
  read-only lookup carrying the unit (for type and arena reads) and the chain of enclosing scopes
  (to resolve a hops-relative reference to a name). It is immutable, it accumulates nothing, and it
  grows no member per concept -- so it is not the forbidden `*Context`, which is banned for being a
  mutable, growing carrier that stands in for a class. Threading an immutable lookup down a fold is
  the ordinary functional shape, not that anti-pattern.
- The one piece of mutable state -- a temp-name counter -- is local to a single callable body's
  render, declared where that body's render begins and threaded by reference through its statement
  handlers. It is not task-lifetime state and lives on no context object.

The fold has no `WalkFrame` (no traversal-position state beyond the scope chain in the lookup), no
registries, and no owned root. The opposite case is a structured backend (MIR to LLVM IR): it
accumulates an SSA value map and builds a cross-referenced function, so it is a construction pass
and takes the class shape above. "Backend" is therefore not one shape -- a text emit is a fold, a
structured-IR emit is a construction pass.

## Boundary to Adjacent Layers

- A pass class is a layer-internal artifact. It is not exposed across lowering or rendering
  boundaries. The output of the pass (the constructed IR, the emitted source text) is the contract
  surface visible to downstream consumers.
- The class instance's lifetime is bounded by one task. The output is finalized in `Run` and
  returned by value; the class is destroyed.

## Forbidden Shapes

- A construction pass implemented as free functions threading a context-like object as a separate
  parameter. The constraint is that an accumulating pass must be a class; it is not that free
  functions are forbidden everywhere -- a rendering fold is free functions by nature (see "Rendering
  Folds").
- A `RenderContext`, `LoweringContext`, or any other `*Context` type used as the carrier for facts,
  registries, builders, or walk-frame state -- anything the pass accumulates or mutates. The name is
  an open invitation to unbounded growth. A rendering fold's immutable read-only lookup (the unit
  plus the scope chain, growing no member per concept) is not this shape.
- A class instance shared across multiple task instances (one `ProcessLowerer` reused for several
  processes).
- A class member whose lifetime exceeds the class's task instance.
- A dispatcher method on the pass class that takes per-pass state as a separate parameter alongside
  the `WalkFrame`. Per-pass state lives on the class and is reached through `this`. (Per-kind
  handler free functions are a different shape: they take the pass class instance explicitly because
  they live outside the class, but the instance is the single access channel -- no further narrowed
  references.)
- An immutable-context-with-`With*()` copy-on-descend pattern used to carry a construction pass's
  state. Its cost is the Context name's open invitation, the `mutable` escape hatch a construction
  pass needs for its accumulators, and the parallel structure for the class that owns them. A
  rendering fold threading an immutable scope-chain lookup down with copy-on-descend is the ordinary
  functional shape, not this anti-pattern: a fold accumulates nothing, so there is no mutable escape
  hatch and no parallel accumulator class.
- A walker struct passed as the single fat parameter. Same pathology as a context, with a different
  name.
- A `*State` suffix on a class, a member, or a field.
- A class member representing traversal state. Traversal state belongs to `WalkFrame`.
- A borrowed reference (`&` / pointer-to-stack) to the in-flight output held on a class member. The
  root output is the pass's own deliverable -- it is owned by the class as a real member, moved out
  by `Run`, and the class holds no IR pointer after `Run` returns. Nested scopes opened during the
  walk (a `mir::Block` for an if-branch, a closure body, a fork-branch body) remain stack-local to
  the walker that opens them; only the pass's single root output lives on the class.
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
- A shared base class spanning construction-pass classes. The pattern is shared; types are per pass.
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
- A per-kind handler twin: two near-identical handlers for one node kind, one per pass class,
  differing only in the pass type and how a sub-node is reached. When the logic is context-free, the
  twin is one function template over the pass class (invariant 13); the twin shape lets the two
  drift, the exact failure the template removes.

## Multi-File Organization Within a Pass Layer

When a layer's expression / statement dispatch grows past a single readable file, the layer is split
into the pass class itself and one subsystem file per expression / statement family. The shape is
fixed:

- The pass class header (`process_lowerer.hpp`, `class_lowerer.hpp`, `module_lowerer.hpp`) declares
  the dispatcher methods (`LowerExpr`, `LowerStmt`, `InternType`, etc.), the registry operations,
  and the fact accessors. Per-kind handler declarations are not on the class.
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
  `scope.AddExpr(...)`. A composite factory interns its own children into `frame.current_block` as
  it builds and returns only the top detached, so interning the result yields a self-contained arena
  subtree. The `mir::Make*Expr` family (pure stateless constructors) and the lowering `Build*Expr`
  family (frame-aware) are factories. Examples: `MakeLocalRefExpr`, `BuildSelfRefExpr`,
  `BuildServicesCallExpr`, `BuildDefaultValueExpr`, `BuildArrayConstructExpr`.
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
  is enforced by the registry type's surface, not by external `const` qualifiers. Examples: a local
  binding table, a static-locals list, a HIR-to-MIR type-translation map. Some registries are keyed
  by data from an adjacent layer (e.g., a slang-keyed type cache on an AST-to-HIR Lowerer); such
  registries cannot move into the IR they index into, because doing so would force the IR layer to
  depend on the adjacent layer it was deliberately built to be independent of.
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
a `mir::Block` for an if-branch body, a closure body, a fork-branch body). Each is stack-allocated
inside the walker that opens it, populated by recursion, finalized by move when the scope closes,
and embedded into its owning slot in the parent IR. The walk frame carries the current-scope pointer
so every nested-scope write goes through one uniform call shape (`frame. current_*_scope->Add...`)
regardless of how deeply nested the current scope is.

Entry points outside the pass (e.g., a driver that constructs one `ProcessLowerer` per process)
construct the class with its facts, invoke `Run`, and consume the returned IR or text. They do not
see the class's internal walkers.
