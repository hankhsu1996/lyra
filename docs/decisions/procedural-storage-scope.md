# Procedural Storage Scope

## Date

2026-08-13 (revised; original 2026-06-30)

## Status

Accepted

## Why this decision matters

A named procedural block (LRM 9.3.5 `begin : name ... end`) is a hierarchical-reference head (LRM
23.9): `Top.outer.x` reaches a static inside `outer` from another scope. The block is therefore not
statement-tree decoration -- it is part of the compilation unit's hierarchical structure. Once that
is true, every existing rule for structural declaration applies to it: it has a stable identity, its
layout and storage are settled before any executable lowering, and a peer body reaches it through a
route resolved before any body lowers, not by re-walking source.

This decision pins the named procedural block as a first-class structural concept, separates the
HIR-side lexical view from the MIR-side runtime realization, and forbids body lowering from creating
any of the structural state the runtime tree exposes.

## The invariant

A procedural body has two distinct semantic views, both first-class in HIR but never duplicated
between each other:

- the **statement tree** carries execution semantics (control flow, expression composition);
- the **procedural scope tree** carries lexical declaration semantics (which scope owns which
  declaration; which scope is nested inside which).

Lexical containment lives only in the scope tree, expressed downward: each scope holds the ids of
its direct declarations and of the scopes nested directly inside it, and both are reached by
descending from a body's `root_scope`. Nothing holds a link back up. This is the same direction the
enclosing structural scope uses for the same two relations, where the declarations and the child
scopes are arenas the scope owns outright. A scope is reached by walking the scope tree, never by
walking the statement tree.

Every procedural scope materializes as a runtime hierarchy child of the scope it nests in. Its
identity, the storage its declarations take on the enclosing class, and its parent-attachment are
all settled before any executable lowering runs; body lowering reads those bindings and never
creates new structural shape.

This is the strict reading of `declarations-before-bodies.md`'s D5: body lowering grows no
structural shape, not even shape that happens to be peer-unreferenced. Structural shape includes any
class, member, or contained edge that participates in the runtime object tree or the typed access
graph.

## Decisions

### D1. HIR carries a lexical procedural scope tree

Every procedural body has a root scope; every `begin/end` and `fork/join` introduces its own, and
they nest -- a block inside a block is two scopes, the inner one a child of the outer. A loop that
declares its own control variables gets one too, but not a kind of its own: LRM 12.7.1 and 12.7.3
define that scope as an implicit begin/end block around the loop statement, unnamed unless a
statement label names it, so it is an ordinary block scope. Each scope record holds:

- a kind (process root, subroutine root, begin/end block, named begin/end block, fork/join);
- a segment name, always present: the identifier the source gave the scope -- a `block_identifier`
  (LRM 9.3.5) or a subroutine name -- and a synthesized one for a scope the source did not name;
- the ids of the variables declared directly in that scope;
- the ids of the scopes nested directly inside it.

The name is uniform on purpose. Nothing downstream has to ask whether the source named a scope, so
no consumer carries a present / absent branch for it. What keeps a synthesized name off a
hierarchical path is the kind, never the spelling -- a synthesized stand-in is an ordinary legal SV
identifier, so reading addressability out of the string would be wrong as well as fragile.

The process root is the one kind SV does not itself define: LRM 23.9 lists the `begin/end`, not the
`always` or `initial` around it. It exists so that every body has exactly one root even when the
source wrote no block at all (`initial #7 f();`), which is what lets the root stay a plain id rather
than an optional every consumer branches on. It is never a child of another scope.

There is no parent field on a scope and no declaring-scope field on a var. Containment is stated
once, downward, and every reader descends it; a back-link would be the reverse of what all of them
want, and one of them will pay a scan to invert it. What a procedural scope cannot do is hold its
declarations outright, the way a structural scope holds its data objects: an identity a hierarchical
path can name is minted before any body lowers, so the arena is the body's and the scope holds ids
into it. That is a constraint on where storage lives, not a licence to flip the direction. The link
from the statement tree to the scope tree is one direction only: a block-shaped statement carries
the `ProceduralScopeId` it introduces; nothing in the scope tree carries a back-link into the
statement tree.

A scope's contents are known only once its subtree has been walked, so the walk that opens a scope
is the one that fills it in against the identity minted for it. That an identity is filled later
than it is minted is the ordinary case -- it is what lets a `disable` in another body name a block
before that block's body lowers. What must hold is that every minted identity is definitely filled,
which is why identity is minted ahead of the bodies only for what a name can reach, and every such
scope is one some body goes on to open.

A block introduces one unconditionally. Neither carrying a name nor holding a declaration is a
condition on the scope existing -- both are properties recorded on the scope, so a block with
neither is the same shape as one with both and no consumer carries an absent-scope branch. Which
blocks the frontend happens to record a symbol for is a fact about the frontend, not about which
scopes the language defines.

Identity is minted ahead of the bodies for exactly what a name can reach from elsewhere -- a
subroutine and a source-named block -- because a `disable` or a hierarchical reference in another
body has to name it before that body lowers. Everything else is minted by the walk that opens it,
which is also the only pass that knows the lexical nesting: the frontend records a scope for reasons
of its own (the bindings of a pattern arm, the control variables of a loop) and lists those symbols
as siblings of the process rather than inside it, so its member nesting is not the source's.

A scope's runtime addressability -- whether an SV path can name it -- follows from the kind alone,
and the kinds that qualify are the procedural entries in LRM 23.9's scope list: a task or function
root, and a named begin/end. Future named-fork support extends the kind set without reshaping the
registry. Addressability is not read off the name: the name is present on every kind, so it cannot
carry that distinction.

### D2. Three distinct layers: HIR lexical scope, MIR name node, MIR storage

```
HIR lexical scope tree = every declaration-bearing scope (root + unnamed + named)
MIR name node          = one runtime object per scope, carrying the identity a path matches
MIR storage            = a field of the class enclosing the body
```

The lexical tree preserves SV semantics (visibility, shadowing, and `disable`, which names a scope
by static declaration identity). The object graph has the same shape as that tree. Storage does not:
a static-lifetime local is one cell per instance (LRM 13.3.1), which says how many cells exist and
how long they live, and says nothing about which object holds one. Reachability is the separate
question, and the LRM answers it separately -- LRM 6.21 lets a hierarchical reference name any
static variable except one declared inside an unnamed block, so a path descends through the named
blocks to reach the cell.

Serving both with one mechanism -- putting the cell inside the block's object -- makes the first pay
for the second. Every read from the body then walks one object per enclosing block, and, because the
walk has to be known before any body lowers, a pre-pass has to compute and record each scope's whole
descent. Keeping them separate costs nothing: the name node answers reachability, and the cell sits
where every other per-instance cell already sits, so a body reads it directly off `self`.

### D3. Every scope materializes a name node

A procedural scope becomes a runtime object whatever the source called it and whether or not
anything was declared there. So one shape lowers every scope, and no property of a subtree decides
what an enclosing scope realizes.

Whether a hierarchical path can name a scope is a separate question, and it decides only what the
scope exposes. LRM 23.9 lists the constructs that define a scope; of the procedural ones a task, a
function, and a named begin/end are named by the source, and LRM 23.9's own example reaches into
them (`t.b.r`). LRM 23.6 shows the contents of an unnamed begin/end unreachable even though the
declaration still creates a scope. A scope the source did not name therefore carries no segment: it
holds the nodes below it together and stays off every hierarchical path, which is the LRM's answer
without a second realization to reach it.

A predicate here was tried and is what this decision replaces. Gating the object on "named AND
something reachable under it" gave one construct two realizations, so a name had to be rebuilt from
an object path plus a compile-time remainder. It also hid a defect: a scope no consumer ever reached
was a scope whose absence nobody noticed.

A declaration scope that is not itself part of the design hierarchy gives no scope a node at all. A
class is one: its object is reached by member select, not by scope name (LRM 23.7), so no
hierarchical path names a block inside a method and nothing there would answer for a name.

A scope's cancellation source -- what a `disable` of it invalidates (LRM 9.6.2) -- is one cell per
instance, so it is a field of the enclosing class rather than something the node holds. Which scopes
own one is LRM 9.6.2's own target set: the blocks and tasks a name reaches, so no pass has to first
find out which scopes some `disable` names, and a scope the source did not name owns none because
nothing can name it. A declaration scope outside the design hierarchy owns none at all, for the same
reason it gives no scope a node.

### D4. The lexical owner is the naming owner

A static's cell is a field of the class enclosing the body -- the same arena that holds the scope's
own variables, an instance member, or a routed-reference slot. A body reads it off `self` in one
step, and nothing has to know which blocks stand between the declaration and the body.

What the declaring scope owns is the name. A static declared in a scope the source named is
registered under its source spelling on that scope's name node, so a descent reaches the node by
name and takes the cell's address from it; one declared in a scope the source did not name is
registered nowhere, so `Top.outer.hidden` does not resolve for a `hidden` inside an unnamed block
under `outer` -- which is what SV's hierarchical-reference rules already say.

Reachability also decides the storage's shape. Storage in the design hierarchy is an observable
cell, because a hierarchical reference can read it and an event control can wait on it, so a write
has to reach subscribers. Storage on a class is a plain cell -- nothing outside the body can name
it.

### D5. A procedural scope is an ordinary scope class

A procedural scope's class is the same kind of thing as a generate scope's: same runtime base, same
attach-to-parent, same signal registration, same by-name lookup, same definition record. Nothing
distinguishes them at the type level, because nothing downstream acts on the distinction -- the
source-language origin is a fact of the HIR the class was lowered from, not of the class.

### D6. The shape phase reads the scope tree; nothing is inherited down it

The HIR-to-MIR shape phase reads the lexical scope tree, never the statement tree. What it gives a
scope -- a name node, a cancellation source if the source named it -- depends on that scope alone,
so it is one flat pass over the scope registry with nothing carried from a parent to a child. What
each static gets depends only on its declaring scope, so it is one descent per body, taken because a
declaration states its scope by sitting in it and holds no link back up.

There is no preliminary pass computing a property of a scope from the scopes around it. Where the
nesting matters is where the objects are built, and there the constructor descends the same lexical
tree once, reading each scope's node as it goes.

Both are shared by every declaration scope that owns bodies and re-derive nothing. There is one
answer to "what does this scope own", not one per owner kind.

The body lowering looks up the resulting binding per static var; it re-derives nothing and does not
visit the statement tree for any of this.

### D7. Intra-unit access to a procedural-scope static names the declaration, not the blocks

A reference of the form `outer.x` from a sibling process is an intra-unit typed route (LRM 23.9),
not a cross-unit by-name climb. What it names is the static declaration itself -- the body that
declares it and its id within that body. The named blocks between the static and its structural
scope are not steps of the route, and they are not steps of the access either: the cell is a field
of the structural scope's own class, so arriving at that scope is arriving at the cell.

This is why a static a hierarchical path can name is minted by the compilation unit's declaration
pass rather than by the body that declares it, and why it is minted **once**: the reference and the
declaration name the same id. An identity assigned during body lowering could not be named by a peer
body that lowers first, and giving such a static a second, reference-only identity would put two
name systems on one declaration.

Its executable realization is a typed enclosing climb to the storage's structural scope and then the
field -- typed member accesses, no by-name lookup anywhere. The runtime by-name walk serves only a
cross-compilation-unit descent, where the block labels are the only identity that crosses the
boundary.

### D8. Ownership is the runtime tree; the borrowed handle is naming only

A name node's lifetime is owned by the runtime object tree: it is attached to the node of the scope
around it, built through the one owned-child construction form, so the objects nest exactly as the
source nests and a by-name descent walks that nesting. The member the enclosing class keeps on that
node is a **borrowed handle** -- a typed non-owning pointer, filled at construction from the value
the runtime's owned-child construction returns. Ownership (the runtime tree) and naming (the
borrowed handle) are separate concerns: the handle never owns the child and never carries the
child's lifetime.

Where a node hangs and who keeps a handle on it are separate too: the enclosing class keeps one
handle per procedural scope however deeply the scope nests, so a body reaches its own node in one
step and nothing has to know what stands between. That the handles are flat while the objects nest
is the point -- reproducing the nesting in the handles would make every reader re-walk what the
runtime tree already holds.

The same split holds for an instance and a scalar generate-block child; each keeps a borrowed handle
for its scalar layout-visible head while the runtime tree owns the child. An array element (a
generate-for iteration, an instance array) keeps no handle and is reached by an indexed by-name
child lookup downcast to the child's typed class.

The handle is typed to the child's own class in every one of those cases, including the naming-only
one that asks the node for nothing but a name. That uniformity is what makes a class's layout a
complete statement of which objects the runtime builds under it, and a consumer that needs that set
-- an execution backend deciding which classes to give a runtime definition -- reads it from the
layout rather than from a list kept beside it.

### D9. `%m` is the object's own path

LRM 21.2.1.5 makes `%m` the hierarchical name of the design element, subroutine, named block, or
labeled statement that _invokes_ the system task. The scope is the one the call was written in, so a
deferred display (`$strobe`, an NBA) still reports the call site rather than wherever the output is
flushed.

The name is the path the runtime walks from that scope's object, and nothing else: an ancestor the
source named contributes its segment, one it did not contributes none. An unnamed block is not among
the four constructs LRM 21.2.1.5 names, so leaving it out is the standard's answer, not an elision.
`%m` therefore lowers to one value with no compile-time remainder, which is what lets a format width
apply to the whole name and what lets a format string parsed at run time answer `%m` from the same
operand a parsed-at-compile-time one does.

## Forbidden shapes

- A naming member that owns the child (an owning pointer, a lifetime-carrying member). It is a
  borrowed handle; the runtime object tree owns the child's lifetime. An owning one re-couples
  naming to ownership and reintroduces a member the runtime tree must not duplicate.
- An owned child reachable from its owner's layout only through an untyped pointer. The layout is
  where the set of objects the runtime builds is stated; a handle that erases the child's class
  drops the child out of that statement without anything reporting it missing.
- A `ProceduralScopeDecl` carrying a parent link, or a `ProceduralVarDecl` carrying its declaring
  scope. Containment is stated downward; a back-link is the reverse of what every reader descends,
  and one of them will pay a scan to invert it.
- An identity minted for a construct no pass goes on to fill. Minting ahead of the filling walk is
  the point of a declaration pass; minting for something the walk never reaches is what leaves a
  scope declared and never defined.
- Two passes enumerating the same constructs under separately-written conditions. The condition is a
  property of the construct; it is asked once and every pass reads that answer.
- A shape pass that walks the statement tree to derive scope ownership. The lexical scope tree is
  that pass's input; the statement tree carries execution semantics, not declaration semantics.
- A predicate deciding whether a scope becomes a runtime object. Every scope does; naming decides
  only what it exposes.
- A second identity for a static-lifetime local, minted so that a reference can name what the
  declaration arena assigns too late. One declaration has one id; if a reference cannot name it, the
  declaration is being minted at the wrong stage.
- A block label used as the identity of a scope or of the storage inside it. The label is the
  runtime segment; identity is the id the declaration pass mints.
- A static's cell inside a procedural scope's own object. Reachability is what the scope owns, and
  it is answered by registering a name on the scope's node; moving the cell there as well makes
  every read from the body pay for a question it did not ask.
- A pre-pass that computes, for each scope, a path assembled from the scopes above it. Whatever such
  a path is for is something a reader has to be handed rather than reach directly, and the pass
  exists only to hand it over.
- An unnamed scope's static exposed under its source name. Holding a cell does not imply
  hierarchical addressability; the source-name registration follows the declaring scope's kind.

## Consequences

- The HIR-to-MIR shape phase reads a small tree (the procedural scope tree is sparse compared to the
  statement tree) and dispatches on scope kind, never on statement variant; the number of
  statement-tree kinds it has to handle is zero.
- Every per-instance cell a body owns -- a static, a cancellation source -- is a field of the class
  enclosing the body, so it shares one declaration form, one access shape, and one render path with
  the scope's own variables.
- The owned-child binding registry carries an enlarged set of head kinds; the lookup path is
  unchanged.
- Every procedural scope contributes a class and one object per instance, built once at elaboration.
  Once the cells moved out, that object carries nothing of its own, which is the price of there
  being a single realization rather than two.

## Rejected alternatives

- **Stmt-tree visitor in the shape phase.** Walk the statement tree at HIR-to-MIR shape phase to
  discover named blocks and their statics. Rejected: the statement tree carries execution semantics;
  declaration semantics live in the scope tree. Mixing the two forces the pass to dispatch on every
  control-flow statement kind to find embedded declarations and creates a walker pattern that has no
  peer elsewhere in the HIR-to-MIR layer.
- **Back-links instead of downward lists** -- a parent on the scope, a declaring scope on the var --
  so that each record is complete the moment it is made. Rejected twice, for the same reason:
  readers descend containment, so a back-link is stored backwards and someone pays a scan to invert
  it (the dump did exactly that, for both). Completeness-at-creation is not the property being
  optimized; every minted identity being filled is, and that is a question about what gets minted.
- **Deriving the nesting from the statement tree.** A block-shaped statement already carries the
  scope it introduces, so the nesting is recoverable without storing it. Rejected: it makes every
  declaration-side consumer traverse execution structure and know which statement kinds can nest, to
  recover a fact the declaration view exists to state.
- **Materializing only the scopes a name can reach.** Give an object to a source-named scope with
  something reachable under it, and flow every other scope's storage outward to the nearest one that
  qualifies. Rejected: it gives one construct two realizations, and each of the three mechanisms it
  then needs -- outward placement, a record of where each static went, a name rebuilt from a path
  plus a compile-time remainder -- exists only to undo the split.
- **Body-lowering mints the scope on encounter.** The procedural-scope class is created when the
  body lowering enters the named begin/end. Rejected: re-binds structural graph to executable
  traversal even when the class id appears peer-unreferenced; the intra-unit downward-head rule (D7)
  shows the id IS peer-referenced.
- **A distinct MIR type kind for a procedural storage scope.** Separate it from a generate scope so
  dump and per-kind extensions can discriminate the source. Rejected: nothing downstream acts on the
  distinction, and a classification no consumer reads is a field that only has to be kept true.
- **The cell inside the declaring scope's object.** Held for a while, on the reading that the
  lexical owner should be the physical owner: "there is no second owner and no outward flow, so
  nothing has to record where a static ended up." That much held -- but the cell was never in danger
  of flowing outward, because per-instance lifetime already pins it to the instance. What the
  arrangement actually bought was reachability, and it charged every read for it: a body reached its
  own static through one object per enclosing block, and the chain of them had to be computed and
  recorded by a pre-pass before any body lowered -- the same record-and-consult the reading was
  meant to avoid, reintroduced for the access path instead of the placement. Registering the name on
  the node and leaving the cell on the instance answers reachability without charging anyone else.

## Cross-references

- `declarations-before-bodies.md` -- the strict-D5 invariant a procedural-scope class respects.
- `variable-lifetime-storage.md` -- the storage-owner rule for static-lifetime body locals, read
  through the addressable-scope concept this decision provides.
- `hierarchical-reference-routing.md` -- one routing path per access shape; a route into a
  procedural static is a typed enclosing climb to the structural scope that holds it, and the
  runtime by-name walk serves cross-unit access.
- `object-model-storage.md` -- the unit's class registry that holds the procedural-scope classes.
