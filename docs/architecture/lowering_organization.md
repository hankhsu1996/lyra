# Lowering Organization

The two lowerings from source do not yet all take the handler shape of invariants 4 and 6:
HIR-to-MIR's per-kind handlers for randomization and covergroups take the walk position by reference
and no source span, and some take the result type as a trailing argument. Everything else here binds
every construction pass as written.

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
  accumulates shared registries (a dedup cache, a binding table) as it walks. AST-to-HIR and
  HIR-to-MIR are each one, and so are MIR-to-LIR and the LLVM backend, which keep a value map and
  build cross-referenced functions.
- A **rendering fold** maps each node to its text in an unstructured medium, written in the order
  the text is read, and accumulates nothing a later node reads. The C++ backend is one: a node's
  text never references another node's, so there is no shared output to own.

The dividing question is always "what task-lifetime shared mutable state exists?" -- registries
justify the class; their absence is what makes a fold a fold. This document defines the
construction-pass shape (the class, the dispatcher signature, the three member kinds, the per-kind
handler shape, the `WalkFrame`), how a construction pass is staged so peers can name each other, and
what a walk says about itself; the rendering-fold shape (free functions writing into a destination,
a read-only view of where the render stands) is defined separately under "Rendering Folds." The
dispatcher signature, the per-kind handlers and the `WalkFrame` are the shape of the two lowerings
from source, which still resolve references by where a node stands; a construction pass below MIR
takes the class and its three member kinds and threads only where a node is read from (see "The Walk
Position").

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
- The rule that the construction-pass pattern is shared by every accumulating pass while concrete
  types are per pass, and that a rendering fold is a distinct shape, not an instance of it.
- The staging of a construction pass whose output admits mutual reference: identities, then
  declarations, then bodies.
- What a walk says about the work it is doing, which is what a failure inside it reports.

## Does Not Own

- The concrete data carried by any one pass's class.
- IR layer shape ([`hir.md`](hir.md), [`mir.md`](mir.md), [`lir.md`](lir.md)).
- Lowering permissions ([`lowering_boundaries.md`](lowering_boundaries.md)).
- Which failures stop a run and where a run recovers
  ([`compiler_overview.md`](compiler_overview.md)).

## Core Invariants

1. Every construction pass is implemented as a class named for the unit of work it processes and the
   action it performs (`ProcessLowerer`, `StructuralScopeLowerer`). The class is constructed once
   per task instance and destroyed when that task completes; one class instance does not span
   multiple task instances. A rendering fold is not a class (see "Rendering Folds").

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
   thereafter), **registries** (accumulators the pass fills as it walks and reads back through a
   typed id), or the **owned root output** the pass exists to produce.
   - **A registry's API follows from whether it mints the identity it is keyed by.** One that mints
     appends and hands the identity back. One keyed by another pool's identities mints nothing, so
     it offers no append: it is built whole against that pool's own count, which is what makes it
     total and lets a read be a plain lookup rather than a search that can come back empty. Two
     pools claiming to mint one identity is two authorities for it, and the drift between them is
     what an append-in-the-right-order agreement then has to police.
   - **Root output ownership.** The root lives as a member on the pass class, with lifetime equal to
     the pass instance, shared by every handler. `Run` moves it out at the end; after `Run` returns,
     the class holds no IR pointer. This is the load-bearing no-leak guarantee.
   - **Root output access.** Read access is exposed as a `const` accessor returning the in-progress
     output by reference; downstream consumers post-`Run` use the same interface, so no parallel
     read API exists on the pass class. Write access is exposed as narrow methods that encapsulate
     cross-state invariants the root cannot enforce alone (e.g., type dedup that coordinates the
     output's type table with a slang-keyed cache living in a class registry).
   - **Nested builders are separate.** IR blocks opened mid-walk by a handler (a block for an
     if-branch, a closure body, a fork-branch body) are stack-allocated inside the walker that opens
     them, populated by recursion through `frame.current_block`, finalized by move when the scope
     closes, and embedded into their owning slot in the parent IR. They are never class members.

   The split follows lifetime and sharing: the root is shared by every handler for the whole task,
   so it lives on the class beside the registries that index into it; a nested scope lives exactly
   as long as the walker that opens it; and a unit's lowering builds one unit, so nothing on the
   walk position ever means "the current unit as opposed to another".

6. `WalkFrame` is a construction pass's **walk position** (see "The Walk Position"): a small value
   type holding **per-recursion traversal context only** -- the class chain, the binding context of
   the body being lowered, a pointer to the current write-target nested builder, the piece of source
   being lowered, and any further stack-discipline state. It is passed by value between walker
   methods. Each recursion may construct a new frame with different fields set; the cost of copying
   is trivial. Walk-invariant facts (the unit currently being constructed, builtins table) are class
   members on the pass class, not `WalkFrame` fields -- only state that genuinely changes from one
   recursion to the next belongs on `WalkFrame`. Writes to nested scopes go through
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
   symbol binding on the pass class, and a member append feeds scope-specific arenas. The asymmetry
   is intended -- the expression sink is shared because the write is context-free; the statement and
   member targets stay context-bound because the writes are not.

7. Adding a new fact is a class-member addition and a constructor-parameter addition. It does not
   change dispatcher or per-kind-handler signatures. Adding a new traversal-time concept is a
   `WalkFrame` struct-field addition. It does not change dispatcher or per-kind-handler signatures
   and it does not add a class member. This rule is the contract's load-bearing scalability promise.

8. The `const` qualifier marks a member's access pattern, not the class. A `*State` suffix does not
   appear on any pass class, any member type, or any field name. Type names describe what the type
   represents.

9. The dividing line between the class pattern and the fold shape is whether the pass accumulates
   task-lifetime shared state -- not whether it is called "lowering" or "rendering." Every
   construction pass (accumulates registries, builds a cross-referenced output) takes the class
   shape and its three member kinds, whether it lowers HIR to MIR or emits a structured backend IR.
   The dispatcher, per-kind handler and `WalkFrame` shape of invariants 4, 6, 7 and 10-13 is that of
   a pass still resolving references by where a node stands -- AST-to-HIR and HIR-to-MIR -- since
   below MIR every reference is stated directly and there is no position to thread but the block a
   node is read from (see "The Walk Position"). A rendering fold (writes an unstructured medium,
   accumulates nothing a later node reads) takes the fold shape instead (see "Rendering Folds").
   Concrete types are per pass: a construction pass's registries and `WalkFrame` write-targets are
   meaningless in a fold, so one shared type would drag dead state into whichever pass does not use
   it. What is shared is the shape itself -- the class-plus-dispatcher convention, the fold
   convention -- held by following it, not by a base type that owns cross-pass state. The harm to
   avoid is a base that couples one pass's state or machinery onto another, not any type two passes
   both name; a stateless boundary one pass declares and another consumes owns nothing and is not
   this shape.

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
    `lowerer.Owner().InternType(...)`. `proc.Owner()` returns the `UnitLowerer&` the
    `ProcessLowerer` captured at construction.

13. A per-kind handler whose logic is shared across pass classes is one function template
    parameterized over the pass class, never a per-pass-class twin. A node's meaning that does not
    depend on which pass class encloses it -- an expression operating on already-lowered operands --
    is context-free, and its handler reaches the pass class through a uniform duck-typed surface
    (the recursion entry point, the sub-node accessor) rather than a shared base class. A handler
    stays per-pass-class only where the two genuinely differ: name resolution, where a bare name
    maps to different storage depending on the enclosing scope, and kinds that exist in only one
    context. The pass classes themselves stay two, and the node types stay typed: a procedural body
    lowers to a callable and a structural scope to a class, a distinction MIR carries, so merging
    them would push a which-kind branch into every consumer the type system now carries.

    _Current implementation:_ the context-free expression handlers (operators, selects, aggregate
    value-builds) are single function templates over the pass class on both lowering boundaries. At
    HIR-to-MIR the template recurses through `LowerExpr` / `LowerAccessPath` and reaches
    sub-expressions through a uniform `HirExprs()` accessor; at AST-to-HIR it recurses through
    `LowerExpr` and interns through the walk position's single expression arena, so the pass class's
    own surface is just `LowerExpr` and the accessor reaching the enclosing unit. Each is
    constrained on an `ExprLowerer` concept, with explicit instantiations for the two pass classes
    in the subsystem `.cpp`. Name resolution (`references.cpp` on each boundary) stays a
    per-pass-class pair, because a name is the one thing that means something different in the two
    contexts. An expression that writes is not: what a write puts where is decided by the write, so
    an assignment and an increment are shared like everything else, and a loop generate's step is
    the structural one (LRM 27.4). The single kind still handled in one context alone is a queue's
    `$` bound, and that is this walk's reach rather than anything the language confines to a
    procedure.

14. Whoever walks says what it is working on, and nothing at a failing site does. The unit step
    names the unit; each lowering names the declaration, the statement and the expression it is
    translating, by the place in the source each was lowered from where it was lowered from one;
    each stage below HIR also names the function. A failure leaving such a walk collects what was
    said above it, innermost first, and nothing is composed unless that happens. _Requirement: a
    broken invariant on a design of a thousand units is reported at the part of the design being
    worked on ([`compiler_overview.md`](compiler_overview.md)), while the several hundred sites that
    can fail each hold only an id or a type, and saying where costs nothing when nothing fails._

    _Current implementation:_ each walk pushes a scope object on its thread naming its work, and an
    error unwinding through one adds what it named to the thread's trail; the trail is read where
    the unit's pipeline catches the failure, on the thread that ran it.

## The Walk Position

Every pass over an IR is a recursion, and at each node the recursion needs context that is not in
the node but follows from where the node sits in the tree. That context is the **walk position**,
threaded by value down the recursion. How much of it there is depends on how much of "where the node
sits" the IR it reads still leaves to the reader.

- A **construction pass** reads an IR that still states some references relative to where they
  stand, and it is the one deciding what to build. HIR-to-MIR resolves a member reference HIR states
  at some number of enclosing classes out by climbing the chain of classes around the one being
  built, and a name by the binding context of the body being lowered -- a closure body installs a
  child context whose parent is the enclosing one, so a capture forwards one callable boundary at a
  time. On top of those it carries the write target (the nested scope being built) and the decision
  state it uses to compute what to build: the source being lowered, how a read of an observable
  answers while a subtree is lowered, whether the body runs as a function call. This is the fat walk
  position -- `WalkFrame` in HIR-to-MIR.
- A **fold** reads MIR, which states every reference directly: a local against its callable's arena,
  a member through an explicit receiver. So it resolves nothing by climbing, and its walk position
  is only where the render stands -- the unit, the function, the current block, how what is written
  is linked -- with the report of what writing has left behind. This is the thin walk position --
  the MIR-to-C++ scope view.
- A **construction pass below MIR** reads an IR that states every reference directly, as a fold
  does, so the position it threads is only the block a node is read from. The scopes open where it
  is building are not a position passed down but the builder's own state, because a way out -- a
  `break`, a `return`, an unwind -- runs every scope between it and its target, not only the
  innermost; rustc's MIR builder keeps its scopes on the builder for the same reason.

The walk position thins as the pipeline descends: each pass that bakes a decision into the IR
removes state a later pass would otherwise carry. HIR-to-MIR carries a fat frame because it is still
deciding captures, receivers and callees; the C++ fold carries almost nothing because those
decisions are now MIR nodes. The same force that moved observability, the receiver and selector and
field access out of the render and into MIR is what thins the walk position.

Two consequences for type structure:

- **The concept is shared; the type is per pass.** A construction pass's `WalkFrame` and the fold's
  scope view are two instances of one concept, not one type. Merging them would drag the
  construction-only fields (write-targets, the class chain, the binding context) into a fold that
  never uses them.
- **A fold need not split invariant facts from the walk position.** A construction pass keeps
  walk-invariant facts (the unit, builtins) on its class and only per-recursion state on
  `WalkFrame`, because facts must stay immutable and stay out of the copied-per-recursion frame. A
  fold has no class, so its one threaded view carries both the walk-invariant lookup (the unit, for
  type and arena reads) and the per-recursion position. The split that protects a construction pass
  buys a fold nothing.

## Data Flow

A construction pass moves three kinds of fact, and each has one carrier. A fact that reaches a
consumer any other way is a defect, whatever it looks like locally.

- **Context flows down**, in the walk position: the enclosing class, the current block, the
  resolution the enclosing scope declared. A node reads what it needs from the frame it was handed.
- **Results flow up**, as return values the caller places: a handler returns a detached node and its
  caller interns it, a declare step returns the identity it minted. A produced fact reaches its
  consumer because the producer handed it over, never because the consumer went looking.
- **Lateral facts go through a declaration table**, read by identity. A reference to a peer -- not
  an ancestor, not a descendant -- cannot be served by either flow above, because the peer may not
  be built yet.

Laterality is the whole reason a pass ever needs more than one sweep. A language that admitted only
downward reference would lower in a single bottom-up walk; mutual reference between siblings is what
forces the declaring sweep to finish before the building sweep starts. So the test for whether a new
construct needs the staging is not "is it complex" but "can something name it from the side".

### Forbidden Data Flow

- **Recovering a fact by scanning output the pass already produced, when the producer knew it**,
  fails the up-flow: the producer had the fact and handing it over was free. A summary computed over
  a block the same function just built is not this shape; reconstructing an identity or a name is.
- **Deriving a name, key, or identity from the size of a pool holding more than one parent's
  entities** fails the stability a name exists for: the result shifts when an entity under another
  parent is added. A pool holding exactly the same-named children of one parent, in the order the
  parent gives them, counts the disambiguator
  [`identity_and_ownership.md`](identity_and_ownership.md) invariant 7 allows -- the bodies one
  unit's lowerings come out as, in the design's order, are such a pool.
- **Reading another walker's in-progress state to answer a lateral question** fails order
  independence: what it finds depends on how far that walker has got. The declaration table is the
  only lateral channel.
- **Computing where a peer pass will put something instead of reading where it did** fails the rule
  that one fact has one authority: two sides then hold it separately, and a check against drift is
  what the arrangement costs.

## Staging: identities, then declarations, then bodies

A construction pass whose output admits mutual reference -- two subroutines calling each other (LRM
13.7), two classes naming each other (LRM 8.24), a body reaching a sibling scope's member, an
override stating its role against its base's slot (LRM 8.20) -- runs in three stages over its unit:

1. **Take identities** for everything another declaration can name before that thing is settled:
   every declaration, and every member of one a peer refers to -- each method a class declares,
   which an override names at its position, and its construction. Each answer reads only the entity
   it names, so entities take theirs in any order.
2. **Settle declarations**: each declaration's shape -- a class's members and children, a method's
   signature, a type alias's target -- which may name any identity from stage 1 in any direction, so
   declarations settle in any order too.
3. **Lower bodies** into blocks attached to the declarations, every cross-declaration reference
   resolving through what stages 1 and 2 published.

There are three because there are three kinds of fact something else needs before its own kind can
be produced -- an identity, a declaration's content, a body -- and nothing ever needs another
entity's body to be built, which is why bodies come last and why a failed body leaves its siblings
lowerable. An identity is split from its content only where something names it that early; anything
nothing can name early -- a process, a continuous assignment, a method the lowering synthesizes --
takes its identity and its body together where it is built. A callable's signature is part of its
declaration's shape, and the pool that mints callable identity is the one the shape takes it from,
so the body stage fills the identity the shape handed out rather than appending where the shape
expected it to. A body attaches executable content to declarations that exist and grows no shape a
peer could read; a declaration only a body's own lowering reads -- a synthesized check site, a
capture descriptor -- may be added there. During the body stage a peer's shape is read from the
declaration table by identity, never through the registry the finished classes are committed to,
which holds a class only once its bodies are complete. The table lives exactly as long as the pass,
and the finished unit holds one class per identity, shape and bodies together, so no consumer pairs
two registries. Which order declarations happen to be visited in is not observable, so a refactor
that changes it is conforming when the program it produces means the same.

A pass below MIR whose work is found on its own output runs to a fixed point instead: the bodies a
unit's finished LIR asks for are built and lowered until nothing new is asked ([`lir.md`](lir.md),
Boundary).

## Conferred and Derived Identity

Before choosing a container, decide which kind of sameness the entity has, because the two kinds
admit opposite treatment and confusing them is the defect this section exists to name.

An entity is **nominal** when two of them declared separately are distinct however alike they look.
Its identity is _conferred_: nothing in the content determines it, so something has to mint the
distinction and remember it. An entity is **structural** when two that look the same are the same --
which is what lets it be interned. Its identity is _derived_ from its own content, and minting one
would be wrong, because a mint would make two identical things differ.

Three rules follow, and every one of them has been violated in this codebase at least once:

- **A conferred identity is recorded by whoever conferred it, and read by everyone else.** Where two
  layers each mint their own, neither determines the other; the mapping between them is a fact the
  minting side has and the reading side does not. Computing it instead asserts that two independent
  authorities agree, which is not something that can be asserted -- only arranged, and then relied
  on. The arrangement's cost is a check against drift, and the presence of such a check is the
  symptom.
- **A structural aggregate's parts are named by position, never by a minted identity.** Interning
  compares content, so content that carried mints would never compare equal. Position is all that is
  left, and it is enough.
- **A position is not an identity and takes a different type.** A position is arithmetic -- it
  orders, it indexes, it can be a tag -- and it needs no translation crossing a lowering boundary,
  because the layers agree about it by describing the same structure rather than by keeping a table.
  An identity is none of those things. Spelling them alike lets either be passed where the other is
  meant, and the tell is a supposed identity being unwrapped to a bare integer at every use.

The failure has two directions and they look nothing alike from close up. Treating a conferred
identity as derived shows up as a name or an id computed from a pool's size or from a peer layer's
numbering. Treating a position as conferred shows up as an identity type borrowed for a structural
slot, or -- once that feels wrong -- as a bare integer with no type at all. A concept that is
missing does not announce itself once; it appears in as many degenerate forms as there are sites
that need it.

## Pool Selection

Five containers carry the entities a pass produces, and which one is a function of three questions
asked in order. Reaching for a bare `std::vector` where one of them fits is how an unnamed pool with
hand-written bounds checks appears.

| Does this pass own the identity? | Is the identity conferred or derived? | Is there a window where the identity exists without its value? | Container           |
| -------------------------------- | ------------------------------------- | -------------------------------------------------------------- | ------------------- |
| Yes                              | Derived from the entity's content     | Not askable -- see below                                       | `base::Interner`    |
| Yes                              | Conferred on the entity               | No                                                             | `base::Arena`       |
| Yes                              | Conferred on the entity               | Yes -- a peer names it before its content exists               | `base::Registry`    |
| No -- another pool owns it       | Not askable -- this pool confers none | No -- answered in the source pool's own order                  | `base::Translation` |
| No -- another pool owns it       | Not askable -- this pool confers none | Yes -- answered in an order the writer does not choose         | `base::SymbolTable` |

The questions are ordered rather than independent, and the two cells the table cannot fill are the
reason. A pool that owns no identity confers nothing, so the second question has no subject. And a
derived identity cannot precede its value, because there is no content to derive it from until the
content exists -- which is not a limitation of the container but what "derived" means. A structure
that refers to itself reaches the cycle through a nominal entity, whose conferred identity exists
before its body, and that is what lets an interner need no reserve-then-fill step.

The first two questions are the same distinction the section above draws between conferred and
derived identity, asked at the moment a container is chosen. Asking only "does this pass mint?"
answers `base::Arena` for a structural pool, because an interner mints too -- it just mints once per
content rather than once per request. That is how a type pool ends up an arena by mistake. Every
layer's type pool is an interner ([`hir.md`](hir.md), [`mir.md`](mir.md), [`lir.md`](lir.md)).

A bare `std::vector` is right for a list nothing names from a distance: the components of one
construct, consumed whole. It is wrong the moment something holds an index into it, because the
index is then an identity with its type erased and its bounds unchecked.

A pool that grows while it is read -- an interner, an arena, a registry, a symbol table -- never
moves what it holds, so a reference a lookup returns stays good while the pool grows and a lowering
reads what it built while it goes on building. Such a reference reads a node and never names one:
what holds on to a node holds its id, since an id survives leaving the process and an address does
not ([`north_star.md`](north_star.md) invariants 4 and 5).

A transient marker the walk keeps about its own progress -- which nodes it has already visited -- is
not a pool. It answers "have I done this one", it is consumed by nothing outside the algorithm, and
it dies with the function.

## Free Functions

A pass is a class, but not everything around it is. A free function is correct where a member is
impossible or would invert a dependency:

- The type is a variant alias or belongs to a foreign library, so it has no member surface to join.
  A layer's own type sum is not this case: it is a class over its alternatives precisely so the
  vocabulary for asking what a type is can be members of it.
- The operation belongs to a higher layer than the type it reads. A lowering concern must not become
  a member of the IR type it consumes; that would make the lower layer know the higher one.
- It is one of the pass shapes defined above -- a per-kind handler, a node builder, a fold.

Outside those, an operation on a type this layer owns belongs to that type. The signal that one has
drifted out is a free function whose only meaningful parameter is the value it operates on, sitting
beside a type that already carries members of the same kind.

## Rendering Folds

A rendering fold writes an unstructured medium -- the C++ backend's text -- from an IR. It is not a
construction pass and does not take the class shape, because the two things the class exists to own
are both absent: there is no shared registry to accumulate, and there is no structured root to own,
since no node's text references another's. What remains is genuinely small:

- Per-kind handlers are free functions that write their node's text into the destination they are
  handed, each writing its own punctuation and asking its children to write theirs, in the order the
  text will be read, and answering with nothing. Each byte is written once, where it belongs, so the
  cost of the text follows its size and not how deeply its expressions nest
  ([`backend_contract.md`](backend_contract.md) invariant 11).
- The one context threaded down is the fold's thin **walk position** (see "The Walk Position"): a
  read-only view of where the render stands. It accumulates nothing a later node reads, and it grows
  no member per concept -- so it is not the forbidden `*Context`, which is banned for being a
  mutable, growing carrier that stands in for a class. Threading an immutable view down a fold is
  the ordinary functional shape, not that anti-pattern.
- What is mutable is the destination and the report of what writing left behind -- each node the
  target has no form for, each constant of the unit the text names -- both owned by whoever began
  writing the artifact, and neither read by a node to decide its own text. The destination owns what
  column a line opens at, the blank line that sets one section apart from the next, and the depth a
  body opens at.

The fold has no `WalkFrame`, no registries, and no owned root. The opposite case is a structured
backend (LIR to LLVM IR): it accumulates an SSA value map and builds a cross-referenced function, so
it is a construction pass and takes the class shape above. "Backend" is therefore not one shape -- a
text emit is a fold, a structured-IR emit is a construction pass.

## Boundary to Adjacent Layers

- A pass class is a layer-internal artifact. It is not exposed across lowering or rendering
  boundaries. The output of the pass (the constructed IR, the emitted source text) is the contract
  surface visible to downstream consumers.
- The class instance's lifetime is bounded by one task. The output is finalized in `Run` and
  returned by value; the class is destroyed.

## Forbidden Shapes

Each entry names the invariant or the promise it breaks. Most break invariant 7's scalability
promise -- a new fact or a new traversal concept changes no signature -- or invariant 5's no-leak
guarantee.

- **A construction pass implemented as free functions threading a context-like object as a separate
  parameter** fails invariant 7: every helper's signature then grows with the concepts it carries.
  It is not that free functions are forbidden everywhere -- a rendering fold is free functions by
  nature (see "Rendering Folds").
- **A `RenderContext`, `LoweringContext`, or any other `*Context` type used as the carrier for
  facts, registries, builders, or walk-frame state** -- anything the pass accumulates or mutates --
  fails invariants 5 and 7: its name invites anything that needs threading, and bundling kinds hides
  which references a helper mutates. A rendering fold's read-only view of where it stands is not
  this shape.
- **A class instance shared across multiple task instances** (one `ProcessLowerer` reused for
  several processes) fails invariant 1: state of one task leaks into the next.
- **A class member whose lifetime exceeds the class's task instance** fails invariant 5's no-leak
  guarantee.
- **A dispatcher method on the pass class that takes per-pass state as a separate parameter
  alongside the `WalkFrame`** fails invariant 4: per-pass state lives on the class and is reached
  through `this`. Per-kind handler free functions are a different shape: they take the pass class
  instance explicitly because they live outside the class, but the instance is the single access
  channel -- no further narrowed references.
- **An immutable context with `With*()` copy-on-descend carrying a construction pass's state** fails
  invariant 5: a construction pass needs accumulators, so the immutability is breached by a
  `mutable` escape hatch the moment it accumulates, and the class that ends up owning the
  accumulators stands beside the context as a parallel structure. A rendering fold threading a
  read-only view down with copy-on-descend is the ordinary functional shape: it accumulates nothing,
  so there is no escape hatch and no parallel class.
- **A walker struct passed as the single fat parameter** fails invariant 7 as a context does, under
  another name.
- **A `*State` suffix on a class, a member, or a field** fails invariant 8: it names a role in the
  pass rather than what the type represents, and was the source of a `const State&` that mutated.
- **A class member representing traversal state** fails invariant 6: traversal state belongs to
  `WalkFrame`, and a member holding it has to be saved and restored by hand around every descent.
  The scopes a builder below MIR keeps are not traversal state in this sense: every way out reads
  them whole (see "The Walk Position").
- **A borrowed reference to the in-flight output held on a class member** fails invariant 5's
  no-leak guarantee: the root output is owned by the class as a real member and moved out by `Run`,
  and the class holds no IR pointer after `Run` returns. Nested scopes opened during the walk remain
  stack-local to the walker that opens them.
- **A trivial delegating method on the pass class** that forwards a single call into the in-flight
  output or the current nested scope (`lowerer.AddStmt(...)` whose body is
  `frame.current_scope->AddStmt(...)`, `lowerer.AddType(...)` whose body is `unit_.AddType(...)`)
  fails invariant 3's bounded surface: it grows the class without encapsulating anything. The test
  is mechanical: a method whose body touches exactly one member and adds no invariant is a
  forwarder; a method whose body touches several members in coordination encapsulates an invariant
  and is allowed.
- **An ambient setter / getter pair on the class whose only purpose is install / restore of a
  walk-position concept** fails invariant 6: the concept is a `WalkFrame` field, which a recursion
  sets by making a new frame and nothing has to restore.
- **An `Add*Expr` helper that wraps `scope.AddExpr(Make...(...))` for a freshly built node and
  returns its `mir::ExprId`** fails the node-builder rule below: a freshly built node is returned
  detached and interned by the caller, so whether the arena was written is visible at the call site.
- **A base class that spans construction-pass classes to share their state or machinery** --
  registries, a dispatcher, `WalkFrame` fields, the root output -- fails invariant 9: every derived
  pass carries state it may not use. This forbids a state-coupling base, not every type two passes
  both name: a stateless boundary one pass declares and another consumes is a different thing. Even
  then, prefer a fact that models the relationship as it is -- a body that has no enclosing
  structural scope holds a null scope, it does not implement a scope interface that throws -- over
  an interface that makes a pass answer for a capability it does not have.
- **A dispatcher method or per-kind handler that mutates the class's fact members** fails invariant
  2: facts are mutated only by the constructor.
- **A bundle of multiple kinds (facts + registry + builder) into one type** fails invariant 5's
  three-way split: a bundle a body helper holds by `const&` and a declaration helper holds by `&` is
  the paradox the split removes, whatever its size.
- **A registry keyed by another pool's identities that accepts an append** fails invariant 5's
  registry rule: appending mints, and the identity is not this registry's to mint; what the pass
  then needs is an agreement that the caller appended in the source's order, plus a check that it
  did. Both disappear when the registry is built against the source's count instead.
- **Two registries keyed by the same identity** fail the rule that one fact has one authority:
  whatever the pass settled about one entity is one answer, so it is one registry whose value type
  carries the parts; split across two, the pair has to be kept in step by whoever writes them, and
  the drift when it comes surfaces as a wrong answer rather than a missing one. The test is
  mechanical: no two registry members of a pass share a key type.
- **A separate dispatch-contract header declaring the dispatcher entries outside the pass class**
  fails invariant 12: it duplicates the surface and adds a synchronization point that does no work.
- **Per-kind handlers expressed as methods on the pass class** fail invariant 10: methods grow the
  public class declaration and force every translation unit that includes the class header to
  recompile when a kind is added.
- **Per-kind handlers requiring narrowed access** -- separate facts / registry / builder references
  in the signature -- fail invariant 4: the pass class instance is the single access channel, and
  narrowing reproduces the signature explosion this contract exists to escape.
- **A per-kind handler twin** -- two near-identical handlers for one node kind, one per pass class,
  differing only in the pass type and how a sub-node is reached -- fails invariant 13: the twin lets
  the two drift, which one function template over the pass class makes impossible.
- **A lowering that orders declarations so that a base settles first, in place of taking identities
  first**, fails the staging's order independence: an order is a property of the whole dependency
  graph, stated in one place every new cross-declaration reference has to revisit.
- **A body stage that mints a declaration a peer's body may read, or a token a later stage
  backpatches into a forward reference**, fails the staging: a peer that lowers first cannot name
  it, and a token leaves the output partly valid until the patch, a state no consumer is promised.
- **A body lowering reading a peer's shape through the registry the finished classes are committed
  to, or through a sibling lowerer's in-progress state**, fails order independence: the read
  succeeds for a peer that happened to finish first and fails for one that did not.
- **The IR split into a declaration artifact and a body artifact joined by id** fails the rule that
  a SystemVerilog module, scope or class is one entity: every consumer would pair two registries on
  every access, and the staging belongs inside the pass that builds the IR.
- **A registry primitive that hands out a mutable reference to an entry** (`Mutate`, `Refine`) fails
  the declare-once contract: it advertises that entries change, and the next two-stage problem
  reaches for it instead of a value per stage.
- **A failing site handed where it is, or a failure's location captured by the error type**, fails
  invariant 14: the site does not know, a site that omits it fails silently, and the error type is
  shared with the runtime.

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
  `assignment.hpp` for assignment / increment-decrement; `inside.hpp` for the inside operator;
  analogous families on the statement side). Per-kind handler signatures are
  `(PassClass&, WalkFrame, const NodeKind&, diag::SourceSpan) -> diag::Result<T>`.
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

## Node-Builder Helpers

Helpers that construct an IR node -- an expression, a statement, a block, on either lowering
boundary -- come in two shapes, distinguished by whether the helper is keyed on an input arena
handle:

- **Factory** -- builds a fresh node (or a subtree whose top is a fresh node) from inputs that are
  not an arena handle: a `mir::TypeId`, an HIR node, a literal value, or the body's `self`. A
  factory carries one of two prefixes, split by whether it mutates a scope -- not by whether it
  reads the frame, and not by what kind of node it returns:
  - **`Make*`** is pure: it interns nothing. It assembles the returned node from ready-made parts
    and may read a `const` frame (the `self` binding, a builtin `mir::TypeId`), but it touches no
    arena. The caller interns the single returned node with `scope.AddExpr(...)`. Examples:
    `MakeLocalRefExpr`, `MakeSelfRefExpr`, `MakeServicesCallExpr`.
  - **`Build*`** is composite: it interns one or more child nodes into a scope
    (`frame.current_block` or a passed `mir::Block&`) as it builds, then returns the top node --
    detached as a `mir::Expr` for the caller to intern, or already interned as a `mir::ExprId`.
    Examples: `BuildServicesCallExpr` (interns the `self` child that `MakeServicesCallExpr` takes
    ready-made), `BuildDefaultValueExpr`, `BuildElementAccessCallExpr`,
    `BuildArrayConstructionCall`.

  The distinguishing test is the scope side effect: a helper that reads `self` off a
  `const WalkFrame&` and returns the node for the caller to intern is `Make*` however much frame
  state it consults, while one that interns even a single child is `Build*`.

- **Transform** -- keyed on an input `mir::ExprId`: it reads `scope.GetExpr(id)` to decide what to
  build, or may return the input unchanged (pass-through). A transform returns a `mir::ExprId`,
  because it operates on already-interned arena nodes. Examples: `WrapIndexAsPosition` (returns its
  index input unchanged where the declaration numbers from the index's own zero), `ConvertToType`
  (returns its operand unchanged where it already has the type asked for).

The rule follows from the dispatcher contract, and it is the same contract on both node kinds:
`LowerExpr` returns a detached `mir::Expr` whose children are already interned, and `LowerStmt`
returns a detached `mir::Stmt` the caller appends. So a hand-written builder of either kind obeys
one shape, and the prefix answers one question at the call site -- has the arena already been
written to? That question does not change with the node kind, which is why the split does not
either. The only way to obtain a `mir::ExprId` is `scope.AddExpr(<an Expr produced by a factory>)`;
a freshly built node is never interned behind an `Add*` helper that hands back the id.

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
  access via a `const` accessor (`Unit()` returning `const hir::CompilationUnit&`, or its equivalent
  per pass) -- the same interface downstream consumers use after `Run` returns. Writes that affect
  only one member are forbidden as trivial wrappers; writes that coordinate multiple pieces of class
  state are exposed as narrow methods that encapsulate the invariant (the canonical example:
  `InternType` ties the output's type table together with a slang-keyed cache, so the dedup
  invariant is enforced structurally). `Run` moves the root output out at the end; after `Run`
  returns, the class holds no IR pointer.

Nested builders are not class members. They are the mutable IR scopes opened during the walk (e.g.,
a `mir::Block` for an if-branch body, a closure body, a fork-branch body). Each is stack-allocated
inside the walker that opens it, populated by recursion, finalized by move when the scope closes,
and embedded into its owning slot in the parent IR. The walk frame carries the current-scope pointer
so every nested-scope write goes through one uniform call shape (`frame.current_*_scope->Add...`)
regardless of how deeply nested the current scope is.

Entry points outside the pass (e.g., a driver that constructs one `ProcessLowerer` per process)
construct the class with its facts, invoke `Run`, and consume the returned IR or text. They do not
see the class's internal walkers.

In the current implementation HIR-to-MIR takes identities in its unit lowerer's entry that publishes
the unit's declarations: every class and covergroup the unit names takes its identity before any
declaration settles, then every method slot and construction a peer names by position, then type
translations, records and the classes other units published.
