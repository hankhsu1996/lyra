# Procedural Storage Scope

## Date

2026-10-28 (revised; earlier 2026-10-23, 2026-08-13, original 2026-06-30)

## Status

Accepted. **Every by-name mechanism this entry describes is removed by
[a-design-element-publishes-its-declarations](a-design-element-publishes-its-declarations.md)**:
signal registration and by-name lookup, D7's runtime walk for a cross-unit descent, and indexed
by-name lookup for an array element. A static of a named block is published with the named blocks it
sits in, and an array element is reached through the member or generate entry that holds it,
selected. **D2 to D5 and D8 were revised on 2026-10-28: a procedural scope has no run-time object**
(D3 says why). The rest stands.

## Why this decision matters

A named procedural block (LRM 9.3.5 `begin : name ... end`) is a hierarchical-reference head (LRM
23.9): `Top.outer.x` reaches a static inside `outer` from another scope. The block is therefore not
statement-tree decoration -- it is part of the compilation unit's hierarchical structure. Once that
is true, every existing rule for structural declaration applies to it: it has a stable identity, its
storage and what it publishes are settled before any executable lowering, and a peer body reaches it
through a route resolved before any body lowers, not by re-walking source.

This decision pins the named procedural block as a first-class structural concept of the text,
separates the HIR-side lexical view from what the run keeps, and forbids body lowering from creating
any structural state.

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

No procedural scope exists at run time. Its identity and the storage its declarations take on the
enclosing class are settled before any executable lowering runs; body lowering reads those bindings
and never creates new structural shape.

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

- a kind (process root, subroutine root, begin/end block, fork/join);
- the identifier the source gave the scope -- a `block_identifier` (LRM 9.3.5) or a subroutine name
  -- absent where it gave none;
- the ids of the variables declared directly in that scope;
- the ids of the scopes nested directly inside it.

Whether the source named a scope is recorded as the presence of that identifier, never as a kind: a
`begin ... end` is one construct whether or not a `block_identifier` follows it, and gaining named
forks therefore adds nothing here. Nothing stands in for the identifier a scope was not given.
Composing one out of the kind and the scope's own position would put it in the space the design
declares in, where a block the source did name can collide with it
(`a-name-is-a-relation-not-an-identity.md`); a scope with no identifier is reached by its position,
which is distinct by being one.

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

A scope's addressability -- whether an SV path can name it -- is exactly whether the source gave it
an identifier, which is what LRM 23.9's procedural entries come to: a task or function root, which
is always named, and a begin/end the source labelled. Nothing has to be read off the kind or out of
a spelling, and named forks need no new kind to qualify.

### D2. Two layers: HIR lexical scope, MIR storage on the enclosing class

```
HIR lexical scope tree = every scope the source wrote (root + unnamed + named)
MIR storage            = a field of the class enclosing the body
```

The lexical tree preserves SV semantics (visibility, shadowing, and `disable`, which names a scope
by static declaration identity). Storage is a separate question: a static-lifetime local of a body
in this hierarchy is one cell per instance (LRM 6.21, and `variable-lifetime-storage.md` for what
decides that count elsewhere), which says how many cells exist and how long they live, and says
nothing about which object holds one. Reachability is a third question, and the LRM answers it
separately -- LRM 6.21 lets a hierarchical reference name any static variable except one declared
inside an unnamed block, so a path descends through the named blocks to reach the cell. That path is
text: the enclosing scope publishes the static with the named blocks it sits in, and a route
resolves it at compile time to a member of the published class.

### D3. No procedural scope is an object

A procedure is no branch of the hierarchy (LRM 23.6), and of a block every run-time obligation is a
cell of the instance: its statics (D4), the target a `disable` of it ends (LRM 9.6.2), the events a
covergroup sampled at its beginning or end triggers (LRM 19.3). What reports it is text -- `%m`
(D9), a dump's `begin` scope, which the enclosing scope's own description states -- and what reaches
it from elsewhere is the enclosing scope's published class. So no scope of a body becomes a class, a
field or an object, named or not, declaring anything or not, and no predicate decides which would:
one shape still lowers every scope, and it is the absence of a realization.

An inspection interface would need the scope tree as data -- an instance's identity and the scope's
place in its unit -- and the lexical tree is where that is read; nothing of it asks for an object
the run constructs and walks.

A DPI scope is a module, program, interface or generate scope (LRM Annex H.9), so a block is no DPI
scope and `svGetScopeFromName` finds none.

This replaced, on 2026-10-28, "every scope materializes a name node". Its two reasons were that a
predicate gave one construct two realizations, with a name rebuilt from an object path plus a
compile-time remainder, and that a scope no consumer reached went unnoticed. The first needed a
realization to exist at all: with by-name lookup gone and `%m` a constant below the instance, none
of a scope's readers asked its node for anything. The second turned on its author: the nodes were
the unreached thing -- built in every constructor, walked through four empty phases, a handle no
body loaded -- and on a 41,190-line module of about 3,800 procedures they were 71,514 entries.

A scope's cancellation source -- what a `disable` of it invalidates (LRM 9.6.2) -- is one cell per
instance, so it is a field of the enclosing class. Which scopes own one is LRM 9.6.2's own target
set: the blocks and tasks a name reaches, so no pass has to first find out which scopes some
`disable` names, and a scope the source did not name owns none because nothing can name it. A
`disable` written in another unit reaches the cell as a member the enclosing scope publishes.

### D4. The lexical owner is the naming owner

A static's cell is a field of the class enclosing the body -- the same arena that holds the scope's
own variables, an instance member, or the slot a stored route fills. A body reads it off `self` in
one step, and nothing has to know which blocks stand between the declaration and the body.

What the declaring scope owns is the name. A static declared in a scope the source named is
published under its source spelling with the named blocks and subroutines it sits in, so a path
through them reaches the cell; one declared in a scope the source did not name is published nowhere,
so `Top.outer.hidden` does not resolve for a `hidden` inside an unnamed block under `outer` -- which
is what SV's hierarchical-reference rules already say (LRM 6.21).

Which scope declares it is the source's own answer and never the shape of the statements below it. A
task's `int counted;` is declared by the task however its statements are grouped, so it is published
under the task and `Top.c.tick.counted` resolves; a `begin ... end` the source wrote inside that
task is a scope of its own, and unnamed it publishes nothing. Reconstructing the declaring scope
from statement nesting instead puts every declaration written without a `begin ... end` behind a
block nobody wrote, where the rule above then correctly refuses to publish it.

Reachability also decides the storage's shape. Storage in the design hierarchy is an observable
cell, because a hierarchical reference can read it and an event control can wait on it, so a write
has to reach subscribers. Storage on a class is a plain cell -- nothing outside the body can name
it.

### D5. A procedural scope is no class

A generate scope is a class and an object, because how many blocks a loop counts out and which arm a
conditional takes are decided at construction (LRM 27.4, 27.5), so other units step into an object
the run made. A procedural scope's shape is fixed by the text, so nothing it has is decided at
construction, and the class enclosing the body states all of it.

### D6. The shape phase reads the scope tree; nothing is inherited down it

The HIR-to-MIR shape phase reads the lexical scope tree, never the statement tree. What it gives a
scope -- its reported name, a cancellation source if the source named it -- depends on that scope
alone, so it is one flat pass over the scope registry with nothing carried from a parent to a child.
What each static gets depends only on its declaring scope, so it is one descent per body, taken
because a declaration states its scope by sitting in it and holds no link back up.

There is no preliminary pass computing a property of a scope from the scopes around it. Where the
nesting matters -- the name a scope reports -- it is read by descending the same lexical tree once.

Both are shared by every declaration scope that owns bodies and re-derive nothing. There is one
answer to "what does this scope own", not one per owner kind.

The body lowering looks up the resulting binding per static var; it re-derives nothing and does not
visit the statement tree for any of this.

### D7. Intra-unit access to a procedural-scope static names the declaration, not the scopes above it

A reference of the form `outer.x` from a sibling process is an intra-unit typed route (LRM 23.9),
not a cross-unit by-name climb. What it names is the static declaration itself -- the body that
declares it and its id within that body. The procedural scopes between the static and its structural
scope -- named blocks, and the tasks and functions LRM 23.9 puts on that path beside them -- are not
steps of the route, and they are not steps of the access either: the cell is a field of the
structural scope's own class, so arriving at that scope is arriving at the cell.

This is why a static a hierarchical path can name is minted by the compilation unit's declaration
pass rather than by the body that declares it, and why it is minted **once**: the reference and the
declaration name the same id. An identity assigned during body lowering could not be named by a peer
body that lowers first, and giving such a static a second, reference-only identity would put two
name systems on one declaration.

Its executable realization is a typed enclosing climb to the storage's structural scope and then the
field -- typed member accesses, no by-name lookup anywhere. A descent from another unit takes the
same step through what the structural scope publishes, where the block labels are the names that
cross the boundary.

### D8. A borrowed handle names an object the run built

An instance and a scalar generate-block child each keep a borrowed handle -- a typed non-owning
pointer, filled at construction from the value the runtime's owned-child construction returns --
while the runtime tree owns the child. Ownership (the runtime tree) and naming (the borrowed handle)
are separate concerns: the handle never owns the child and never carries the child's lifetime. An
array element (a generate-for iteration, an instance array) keeps no handle and is reached through
the entry that holds it, selected. A procedural scope keeps none, being no object (D3).

The handle is typed to the child's own class, which is what makes a class's layout a complete
statement of which objects the runtime builds under it, and a consumer that needs that set -- an
execution backend deciding which classes to give a runtime definition -- reads it from the layout
rather than from a list kept beside it.

### D9. `%m` is the instance's path and the name the text fixes below it

LRM 21.2.1.5 makes `%m` the hierarchical name of the design element, subroutine, named block, or
labeled statement that _invokes_ the system task. The scope is the one the call was written in, so a
deferred display (`$strobe`, an NBA) still reports the call site rather than wherever the output is
flushed. A package is a design element (LRM 3.2) and a class method is a subroutine, so a call
written in either reports a name too.

One compiled body serves every instance of a design element, so the part of the name an instance
decides -- its own path, generate indices included -- is the instance's to answer at run time.
Everything below it is fixed by where the text is written: the package and classes around the
subroutine, as the front end spells them and joined to what they declare by `::`, then the
subroutine and each block the source named around the call, joined by `.`. A static variable's
initializer runs once, apart from every activation (LRM 6.21), and is still written in the scope
declaring the variable, so that scope's name is the one it reports. A scope the source did not name
contributes nothing, and an unnamed block is not among the four constructs LRM 21.2.1.5 names, so
leaving it out is the standard's answer, not an elision. Where no instance holds the declaration --
a package, `$unit`, a class declared in either -- the fixed part is the whole name and `%m` is a
constant, the way a C++ compiler makes `__func__` a string of the declaration around it and waits
for an instantiation only where that declaration is a template's.

Either way `%m` lowers to one value, which is what lets a format width apply to the whole name and
what lets a format string parsed at run time answer `%m` from the same operand a parsed-at-compile-
time one does. Every body takes the same realization, so no predicate chooses between two of them.

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
- A run-time object, class or handle for a procedural scope, for some scopes or for all. What a
  scope keeps is a cell of the instance and what reports it is text (D3); an object would be built
  and walked for no reader.
- A second identity for a static-lifetime local, minted so that a reference can name what the
  declaration arena assigns too late. One declaration has one id; if a reference cannot name it, the
  declaration is being minted at the wrong stage.
- A block label used as the identity of a scope or of the storage inside it. The label is a segment
  of the path text; identity is the id the declaration pass mints.
- A static's cell anywhere but the class enclosing the body. Reachability is answered by what that
  class publishes; a cell elsewhere makes every read from the body pay for a question it did not
  ask.
- A pre-pass that computes, for each scope, a path assembled from the scopes above it. Whatever such
  a path is for is something a reader has to be handed rather than reach directly, and the pass
  exists only to hand it over.
- An unnamed scope's static exposed under its source name. Holding a cell does not imply
  hierarchical addressability; the declaring scope publishes a static only where the source named
  it.

## Consequences

- The HIR-to-MIR shape phase reads a small tree (the procedural scope tree is sparse compared to the
  statement tree) and dispatches on scope kind, never on statement variant; the number of
  statement-tree kinds it has to handle is zero.
- Every cell a body of this hierarchy owns beyond an activation -- a static, a cancellation source
  -- is a field of the class enclosing the body, so it shares one declaration form, one access
  shape, and one render path with the scope's own variables.
- A procedural scope contributes nothing at run time beyond the cells of its declarations and its
  `disable` target, so a procedure costs its own body and no class, however many blocks it writes.

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
  meant to avoid, reintroduced for the access path instead of the placement. Publishing the name
  with the enclosing scope and leaving the cell on the instance answers reachability without
  charging anyone else.

## Cross-references

- `declarations-before-bodies.md` -- the strict-D5 invariant a procedural scope's storage respects.
- `variable-lifetime-storage.md` -- the storage-owner rule for static-lifetime body locals, read
  through the addressable-scope concept this decision provides.
- `hierarchical-reference-routing.md` -- one routing path per access shape; a route into a
  procedural static is a typed enclosing climb to the structural scope that holds it.
