# Lowering organization

Date: 2026-06-08 Status: accepted (rendering classification revised 2026-06-19 -- see "Revision")

## Context

The lowering layer carried god-objects -- `ProcessLoweringState`, `UnitLoweringState`,
`StructuralScopeLoweringState`, `ProceduralScopeLoweringState` -- each mixing several categories of
state. The smell was concrete: `const ProcessLoweringState&` qualifiers compensating for a
misleading `*State` name; helper signatures threading four references where typically one is the
real input; ambient mutable state on objects whose names claimed to be const-friendly.

Successive closure-shaped features (NBA submit, fork-join branch, scan IIFE, with-clause plus
iterator index) each added a new ambient field to `ProcessLoweringState` and a parallel install /
restore dance at every producer site. The pattern was unsustainable: each new walk-time concept
either added a sig-thrashing signature parameter or a parallel ambient field on the wrong object.

The MIR-to-C++ backend showed a related smell. `RenderContext` carried facts and walk-position state
in one immutable struct, with `With*()` copy-on-descend semantics; the `mutable owned_temp_counter_`
escape hatch existed because the immutable pattern did not fit accumulating state. Free `Render*`
functions took the Context as a separate parameter rather than being methods on a renderer class.

Three pass layers (AST-to-HIR, HIR-to-MIR, MIR-to-C++) each carried a variant of the same
organizational decision; the decisions had drifted enough that consistency and symmetry had been
lost. This decision is the architecture reset: it pins one shape used across every lowering and
rendering pass. The architecture contract is in `docs/architecture/lowering_organization.md`.

## Decision

**Every lowering or rendering pass is implemented as a class scoped to one task instance.** A class
named for the unit of work and the action (`ProcessLowerer`, `CppProcessRenderer`,
`StructuralScopeLowerer`) is constructed at the entry to one process / scope / unit lowering and
destroyed when that task completes.

**The constructor injects facts.** Read-only inputs (type tables, builtin TypeIds, structural-scope
facts, HIR references, time resolution) are constructor parameters. After construction, facts are
not mutated.

**The class owns its registries and the root output; nested builders live on the stack.** Every
class member is one of three kinds: facts (constructor-injected, never mutated), registries
(append-only accumulators with `Add` / `Lookup`), or the root output the pass is constructing. The
root output (`hir::CompilationUnit`, `mir::CompilationUnit`, etc.) is the deliverable; it lives as a
class member with lifetime equal to the pass instance, is shared by every handler, and is moved out
of the class by `Run`'s return -- after which the class holds no IR pointer. Nested IR scopes opened
mid-walk (a procedural scope for an if-branch, a closure body, a fork-branch body) are
stack-allocated inside the walker that opens them, populated by recursion through
`frame.current_*_scope`, finalized by move, and embedded into their owning slot in the parent IR.
The root and nested scopes are not treated uniformly; their lifetimes differ, so their homes differ.

**Dispatcher methods take exactly two parameters; per-kind handlers are free functions.** The pass
class exposes dispatcher methods (`LowerExpr`, `LowerStmt`, `InternType`, ...) with signature
`(const NodeType& node, WalkFrame frame) -> diag::Result<OutputType>`. Each dispatcher method's body
is one switch over node kind routing to per-kind handlers in subsystem files. Per-kind handlers are
free functions with signature
`(PassClass& pass, WalkFrame frame, const NodeKind& node, diag::SourceSpan span) ->  diag::Result<OutputType>`
-- the pass class instance is passed explicitly because the handler lives outside the class, but it
is the same single access channel. Subsystem `.cpp` files include the pass class header only; there
is no separate dispatch-contract header.

**`WalkFrame` is a small value type for per-recursion traversal state only.** It carries a pointer
to the current write-target nested builder, the current procedural depth, an optional closure
context, and any future stack-discipline traversal state. It is passed by value. Walk-invariant
facts (the unit being constructed, source mapper, builtins) are class members, not `WalkFrame`
fields. Nested-scope writes go through `frame.current_*_scope->Add...`; root-output writes go
through narrow methods on the class that encapsulate cross-state invariants.

**Scalability promise.** A new fact is a class-member addition and a constructor-parameter addition;
dispatcher and per-kind-handler signatures do not change. A new walk-time concept is a `WalkFrame`
struct-field addition; signatures and class members do not change. A new node kind is one switch
case in a dispatcher method body plus one per-kind handler in a subsystem file; the class header
does not change. The historical "every new concept thrashes every helper signature" pattern is
converted into "per-concept growth localized to one location."

**Pattern is shared across lowering and rendering passes; concrete types are not.** No base type
spans the pass boundary. The HIR-to-MIR `ProcessLowerer` and the MIR-to-C++ `CppProcessRenderer`
share only the pattern shape.

## Rejected alternatives

- **Free functions threading separate facts, registries, and builders as narrow references.** The
  sig-audit-strongest shape. Signature width grows with the product of helpers and concepts; the
  closure-shaped feature history (NBA, fork-join, scan IIFE, with-clause + iterator) is direct
  evidence that the thrash pattern is real and persistent.

- **`LoweringContext` god-bag carrying all of unit + process + scope + walk-state.** The
  hospitality-desk failure mode: anything that needs threading lands in the bag. Cross-kind bundling
  hides which references a helper actually mutates. The codebase has historical evidence of this
  exact failure.

- **`UnitContext` / `ProcessContext` per-scope bundles.** Smaller hospitality desk, same growth
  pattern. The boundary is lifetime, not role; transient buffers and scratch space qualify and
  accumulate.

- **`UnitLowering` / `ProcessLowering` multi-kind bundles** (facts plus registries plus builders in
  one type). A body-lowering helper holding such a bundle by `const&` and a decl-lowering helper
  holding it by `&` regenerates the `const State&` paradox this decision exists to remove.

- **Ambient `CaptureSink*` field on a facts-shaped class with RAII save / restore.** Closes the
  symptom (no caller forgets to restore) without removing the root cause (ambient mutable state on
  an object whose name claims const). Each new closure-shaped feature adds a parallel ambient field.

- **`MarkClosureBody` post-construction mutation on a scope state.** Two-phase construction violates
  the rule that an object's contract is fixed at construction.

- **Immutable Context with `With*()` copy-on-descend (the current MIR-to-C++ backend shape).** The
  superficial appeal is descent symmetry; the costs accumulate: the `Context` name is the same
  hospitality desk at every layer; the immutability is breached by a `mutable` escape hatch the
  moment accumulation is needed (the backend's `owned_temp_counter_` is exactly that breach); the
  class that ends up owning the accumulators sits beside the Context as a parallel structure,
  doubling the surface. A single class with a by-value walk frame is cleaner: identical descent
  semantics, no escape hatch, no parallel structure.

- **A single walker struct passed as the one fat parameter.** Same pathology as a context. Different
  name does not change the failure mode.

- **A shared base class spanning lowering and rendering pass classes.** Per-pass content is
  different; a base would be either empty (useless) or invite layer-specific fields to leak across,
  recreating the hospitality-desk failure mode at a higher altitude.

- **A `*State` suffix retained on the new class.** The suffix was the source of the const paradox.
  Removing it forces the choice of an honest name (`Lowerer`, `Renderer`, `Builder`).

## Consequences

- `ProcessLoweringState` is replaced by `ProcessLowerer`, a class scoped to one process lowering.
  Time resolution, static-frame scope reference, and the HIR process reference become facts injected
  at construction. The procedural-var binding table and the static-locals list become class-owned
  registries. Procedural depth, active capture sink, and active iterator-index binding move to
  `WalkFrame`.

- `UnitLoweringState` is replaced by per-purpose objects: the HIR-to-MIR type-translation map
  becomes a `TypeMap` registry; canonical TypeIds remain as `BuiltinMirTypes` facts. Their
  composition into the surrounding `StructuralScopeLowerer` / `ProcessLowerer` classes is per pass;
  no `UnitLoweringState`-equivalent god class survives.

- `StructuralScopeLoweringState` and `ProceduralScopeLoweringState` are dissolved into their
  corresponding IR scope types (`hir::StructuralScope`, `mir::Block`, etc.), which gain construction
  methods (`AddX` / `AppendX`) alongside their existing accessors -- the IR scope type _is_ the
  builder, with no parallel wrapper. Nested scope instances are stack-allocated inside the walker
  that opens them; the root output instance lives on the pass class. The walk frame carries a
  pointer to the current nested write target.

- `ScopeStack`, `ProceduralDepthGuard`, and `ScopeStackGuard` are absorbed by `WalkFrame`. No
  separate guard types survive.

- `RenderContext` in the MIR-to-C++ backend is replaced by `CppProcessRenderer` (and analogous
  classes per task instance: scope renderer, unit renderer). The `With*()` copy-on-descend
  convention becomes "construct a new `WalkFrame` and recurse"; `mutable owned_temp_counter_`
  becomes a class-owned registry. Free `Render*` functions become class methods.

- `LowerExpr`, `LowerStmt`, `RenderExpr`, `RenderStmt`, and their internal helpers in every pass
  change signature once. The signature shape after this is fixed.

- Subsequent closure-shaped features (assoc-array iterator forms, further iterator methods,
  additional capture patterns) add a `WalkFrame` field; no signature change. Subsequent facts add a
  class member and a constructor parameter; no walker signature change.

- AST-to-HIR adopts the same pattern (`StructuralScopeLowerer` and analogues); concrete facts,
  registries, and walk-frame fields are layer-specific. Nested builders are never class members at
  any layer; the root output is always a class member.

- This decision fixes the target shape; adopting it across all three passes is incremental
  implementation work.

## Follow-up Decision: Multi-File Organization Within an Expression / Statement Layer

### Context

After the AST-to-HIR migration landed, the expression-lowering and statement-lowering
implementations each lived in a single file (`expression/lower.cpp` at ~2,000 lines;
`statement/lower.cpp` at ~800 lines). The dispatch switch and every per-kind handler shared one
translation unit. Two concerns drove this follow-up decision:

1. The single-file shape does not scale. Each new expression / statement kind grows one file
   monotonically. Single-file lowering at this size becomes the next refactor target as more LRM
   features land.
2. The first decomposition idea -- per-subsystem private namespace (`expr_detail::`) plus a single
   internal contract header containing dispatchers plus shared utilities plus per-kind handler
   declarations -- introduced a new namespace convention not used elsewhere in the codebase, and the
   shared header drifted into the same shape (a junk drawer of `LowerInsideItemImpl`,
   `ValidateAssignableImpl`, `MakeRefExpr`, `TypeIdOfSlangExpr`, `MakeReturnConventionType`) that
   the architecture reset existed to prevent.

The lesson from `RenderContext` and `ProcessLoweringState` was: when a single header / class carries
"everything the layer needs", the structure invites unbounded growth and the convention that limits
it is one code review away from failing. The same trap was reappearing in the "internal contract
header" form.

### Decision

The AST-to-HIR expression and statement layers are decomposed so the dispatcher is a method on the
pass class and per-kind handlers live in per-subsystem header / implementation pairs, where each
subsystem corresponds to one LRM-level expression / statement family. The split is structural, not
stylistic.

- The pass class header (`process_lowerer.hpp`, `structural_scope_lowerer.hpp`,
  `module_lowerer.hpp`) declares the dispatcher methods (`LowerExpr`, `LowerStmt`, `InternType`,
  ...). The dispatcher method's body in the class's `.cpp` is one switch over node kind routing to
  per-kind handlers. There is no separate dispatch-contract header (`dispatch.hpp` is rejected --
  see below).
- One subsystem header per family declares the per-kind handlers it owns. Operators (unary, binary,
  conditional, conversion), references (named-value, hierarchical), calls, selects (element-select,
  range-select, member-access), aggregates (concat, replication, assignment-pattern,
  replicated-assignment-pattern, new-array), assignment (assignment, inc / dec, assignability
  validation), and inside (inside operator, inside-item). On the statement side: blocks, loops,
  branches, timing.
- One subsystem implementation defines the handlers and any anonymous-namespace helpers private to
  the family. Subsystem `.cpp` files include the pass class header (for the `Lowerer&` parameter
  type and for recursion via `lowerer.LowerX(...)`) and the subsystem header.
- All declarations live in the layer's main namespace (`lyra::lowering::ast_to_hir`); no `detail::`
  sub-namespace is introduced. The "internal" signal is the header's location inside the subsystem
  folder.
- Per-kind handlers are free functions taking the relevant pass class instance by reference
  (`ProcessLowerer&` or `StructuralScopeLowerer&`) plus the walk frame plus the slang node plus the
  source span. They are not methods on the pass class; methods would grow the public class
  declaration and the recompile cost of every translation unit that includes it, and would reproduce
  the kind-by-kind growth the architecture reset abolished at the lower-layer level.

### Rejected alternatives

- **Per-subsystem private namespace (`expr_detail::`) plus single shared header.** Introduces a new
  namespace convention not used elsewhere; the shared header drifts toward a junk drawer.

- **One mega internal header (`expression/handlers.hpp` or similar) listing every dispatcher, shared
  utility, and per-kind handler in one namespace.** The header itself becomes a coupling hub: every
  subsystem includes it for everything; any handler signature change forces every subsystem to
  recompile; the natural home for "shared between two subsystems" is "drop it in the mega header",
  which is the same Context-bloat pattern at the file level.

- **Per-kind handlers as methods on the pass class.** Grows the public class declaration by one
  method per kind; every consumer of the class header recompiles when a kind is added. The pass
  class identity drifts from "owns the process's facts / builder" to "knows how to lower every kind
  of expression", which is the god-bag failure mode at the class level.

- **Per-kind handlers as free functions with narrowed parameters (separate facts / registry /
  builder references in the signature).** Reproduces the sig-explosion shape the architecture reset
  exists to escape. The pass class instance is the single access pass to the task; narrowing the
  signature does not add safety because the pass class API is already bounded by the two-kind rule
  and the IR shape upstream.

- **Compile-time / CI structural enforcement on pass class method counts.** Considered as an
  additional safeguard against future drift on the pass class's derived-accessor API. Not adopted:
  the existing structural safeguards (the two-kind rule, the IR-shape cascade for any new state, the
  contract's forbidden shapes) cover the high-risk growth paths; the residual risk (a derived
  accessor that wraps existing state in a new method) has small blast radius and is left to code
  review.

### Consequences

- The single-file `expression/lower.cpp` and `statement/lower.cpp` become central dispatcher files
  plus per-subsystem files. Each file is bounded in scope and in line count.
- Adding a new expression / statement kind touches three files: the subsystem header that owns its
  family, the subsystem implementation, and the central dispatcher's switch. No other subsystem
  recompiles.
- Adding a new family creates a new subsystem header / implementation pair and adds switch cases in
  the central dispatcher; existing subsystems are not touched.
- `TypeIdOfSlangExpr` is replaced by `UnitLowerer::InternType`, a class method that encapsulates
  both the slang-keyed dedup cache and the output's type table together (the dedup invariant is
  enforced structurally, not by caller discipline). `MakeRefExpr` becomes a subsystem-local helper
  in `references.cpp` (its only consumer). `MakeReturnConventionType` becomes a subsystem-local
  helper in `calls.cpp` (its only consumer). `LowerInsideItemImpl` and `ValidateAssignableImpl` live
  in `inside.hpp` and `assignment.hpp` respectively.
- The architecture contract `lowering_organization.md` gains invariants 10-12 and the "Multi-File
  Organization Within a Pass Layer" section codifying this shape so the HIR-to-MIR (R10) and
  MIR-to-cpp (R11) migrations land on the same pattern.

## Revision (2026-06-19): rendering is a fold, not a construction pass

The original decision pinned one shape across "every lowering or rendering pass" and prescribed that
`RenderContext` become `CppProcessRenderer` (a class), with `mutable owned_temp_counter_` becoming a
class-owned registry. That classification of the MIR-to-C++ backend is revised here. The lowering
half of the decision is unchanged.

What actually happened diverged from the prescription. The render-to-class migration never landed:
the backend stayed free functions with an immutable threaded lookup, and R11 made the temp counter a
callable-body **local** (`std::size_t temp_counter` threaded by reference), not a class-owned
registry. The implementation had already chosen the fold shape.

The original rationale has thinned. The rejection of the immutable-Context shape rested on three
costs: the `Context` name as a hospitality desk; the `mutable` escape hatch "the moment accumulation
is needed (the backend's `owned_temp_counter_` is exactly that breach)"; and a parallel accumulator
class beside the Context. The second and third costs presuppose that rendering accumulates state.
Four cuts landed after this decision and removed every accumulation and every render-time decision
from the backend: R11 (temp counter to a local), R12 (observable `Get` / `Set` / `Mutate` to
explicit MIR calls), R16 (receiver to an explicit `self` member access in MIR), R17 (selector and
field access to explicit MIR calls). With no accumulation left, there is no `mutable` breach and no
accumulator class -- a render class would own only facts, with neither a registry nor a structured
root to justify it. Only the first cost (the name's invitation to grow) survives, and it is answered
by keeping the threaded lookup immutable and member-bounded rather than by wrapping the fold in a
class.

The corrected dividing line is whether a pass accumulates task-lifetime shared state, not whether it
is called lowering or rendering. A **construction pass** -- one that accumulates registries and
builds a cross-referenced output -- keeps the class shape from this decision; HIR-to-MIR lowering is
one, and a future MIR-to-LLVM backend (accumulating an SSA value map, building a cross-referenced
function) is another. The MIR-to-C++ text emit is a **rendering fold**: it composes an unstructured
medium by concatenation, accumulates nothing, and so is free functions with an immutable read-only
lookup (the unit plus the scope chain for hops resolution) threaded down and a callable-body-local
temp counter. The architecture contract reflects this in `lowering_organization.md` (invariant 9,
the "Rendering Folds" section, and the narrowed `*Context` forbidden shapes).

Everything else in this decision stands: the god-object / `*State` removal, class-per-task for every
accumulating pass, the constructor-injects-facts rule, the three member kinds, `WalkFrame` for
per-recursion traversal state, the two-parameter dispatcher signature, and the multi-file subsystem
organization.
