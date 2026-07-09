# Procedural Storage Scope

## Date

2026-06-30

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

Lexical containment lives only in the scope tree, expressed as downward ownership: each scope holds
the ids of its direct declarations and its direct child scopes. No child holds a parent-pointer; no
var holds a declaring-scope pointer; no scope holds anything derivable from its children. A scope is
reached by walking from a body's `root_scope` through the `direct_child_scopes` lists -- never by
walking the statement tree.

A named procedural block whose subtree owns hierarchy-addressable storage, or whose descendants do,
materializes as a runtime hierarchy child of its nearest enclosing addressable scope. Its identity,
layout, and parent-attachment are settled before any executable lowering runs; body lowering
consumes the planner's bindings and never creates new structural shape.

This is the strict reading of `declarations-before-bodies.md`'s D5: body lowering grows no
structural shape, not even shape that happens to be peer-unreferenced. Structural shape includes any
class, member, or contained edge that participates in the runtime object tree or the typed access
graph.

## Decisions

### D1. HIR carries a lexical procedural scope tree

Every procedural body has a `root_scope`; every `begin/end`, `fork/join`, and `foreach` loop
introduces its own scope. Each scope record holds:

- a kind (process root, begin/end block, fork/join, foreach loop);
- an optional SV `block_identifier` (LRM 9.3.5) -- present only for a named begin/end;
- the ids of the variables declared directly in that scope;
- the ids of the scopes nested directly inside it.

There is no `parent` field on a scope and no `declaring_scope` field on a var: those facts are
already in the downward ownership tree and storing them again would be a cache that drifts. The link
from the statement tree to the scope tree is one direction only: a `BlockStmt` / `ForkStmt` for a
source-level block carries the `ProceduralScopeId` it introduces; nothing in the scope tree carries
a back-link into the statement tree.

A scope's runtime addressability (whether it is a valid hierarchical-reference head) is an abstract
property derived from kind + label, not a hardcoded enum check. Today only a named begin/end
qualifies; future named-fork support would extend that property without reshaping the registry.

### D2. Three distinct layers: HIR lexical scope, MIR materialization, MIR storage placement

```
HIR lexical scope tree   = every declaration-bearing scope (root + unnamed + named)
MIR materialization      = the subset of named scopes that need runtime hierarchy nodes
MIR storage placement    = which class actually owns each static's persistent storage
```

These layers are not the same and cannot be collapsed. The lexical tree preserves SV semantics
(visibility, shadowing, future scope-aware constructs like `disable` and named fork).
Materialization is a runtime-storage policy decided at HIR-to-MIR shape phase. Placement is the
final per-static decision that follows from materialization but is not identical to it: a static
inside an unnamed scope nested in a named one is **lexically** in the unnamed scope, but its
**physical** storage owner is the nearest materialized ancestor.

### D3. Materialization predicate

A procedural scope materializes iff it is runtime-addressable AND (it directly owns a static OR a
descendant scope is materialized). Process roots, unnamed begin/ends, foreach loops, and fork scopes
are never runtime-addressable in the supported subset today and therefore never materialize.

The descendant clause is load-bearing: in `begin : outer begin : inner static int y; end end`,
`outer` materializes even though it owns no static of its own, because `outer.inner.y` requires
`outer` to exist as a navigable step in the runtime path.

### D4. Lexical owner is not physical owner

The **lexical owner** of a static is the scope whose direct declarations list it -- this is the SV
truth, used for shadowing, visibility, and future scope-aware semantics. The **physical storage
owner** is the nearest materialized addressable ancestor (or the structural class when no such
ancestor exists); this is the MIR class whose member arena holds the per-instance storage.

A static lexically inside an unnamed scope nested in a named one flows through to the named scope's
class for storage, but does NOT become a hierarchically-addressable signal of the named scope: the
member is declared with its mangled name only, with no source-name and no by-name signal
registration. SV's hierarchical-reference rules already forbid `Top.outer.hidden` for a `hidden`
declared inside an unnamed block under `outer`; the placement rule mirrors that.

### D5. `kProceduralStorageScope` is a distinct MIR `TypeKind`

MIR enumerates the generate scope kind and a separate `kProceduralStorageScope`. A procedural
storage scope shares the runtime Scope base with a generate scope -- same attach-to-parent, same
signal registration, same by-name lookup -- but the two are not conflated. The kind axis
distinguishes them so dump, debug, and per-kind extensions classify on the source-language origin
without inferring from class name or scope position.

### D6. Planner is a two-pass fold over the scope tree

The HIR-to-MIR shape phase derives the storage plan by folding the lexical scope tree, not by
visiting the statement tree:

- **Pass 1 (post-order)** computes per-scope materialization. Each scope returns whether it is
  materialized; the parent uses its children's results plus its own direct statics to decide its own
  materialization.
- **Pass 2 (top-down)** assigns physical storage placement. Each scope receives the chain of
  materialized addressable ancestors above it; statics are placed on the nearest materialized
  ancestor's class (or the structural class), and a materialized scope contributes its companion to
  the chain before recursion descends into its children.

The body lowering looks up the planner's resulting binding per static var; it does not re-derive
placement and does not visit the statement tree for any planner concern.

### D7. Intra-unit access to a procedural-scope static is a typed layout-visible segment

The same owned-child binding registry that routes downward access through an instance or
generate-block head registers named procedural blocks too, so a reference of the form `outer.x` from
a sibling process routes as a downward head rather than a cross-unit by-name climb. A named
procedural block is an intra-unit layout-visible structural child (LRM 23.9). Its declaration-time
hierarchical identity is the SV label -- the stable name slang resolves the reference to, independent
of source order.

Its executable realization is a typed segment. The enclosing climb to the block's owning scope is
typed, and the block itself is reached through the materialized procedural scope's companion handle
on the enclosing class -- a typed member access, not a by-name lookup. HIR-to-MIR recovers that
companion from the label, so the label is the head's declaration-time identity while the companion is
its realization. The runtime by-name walk serves only a cross-compilation-unit descent past this
head.

### D8. Ownership is the runtime tree; the companion is navigation only

A materialized child scope's lifetime is owned by the runtime object tree: the parent's runtime
scope holds the child as an owning member, built through the one owned-child construction form. The
companion on the parent's generated class is a **borrowed** typed handle to that child -- a
layout-visible navigation endpoint for the typed segment (D7), filled at construction from the value
the runtime's owned-child construction returns. Ownership (the runtime tree) and navigation (the
companion) are separate concerns: the companion never owns the child and never carries the child's
lifetime. The same split holds for an instance and a scalar generate-block child; each keeps a
borrowed companion for its scalar layout-visible head while the runtime tree owns the child. An
array element (a generate-for iteration, an instance array) keeps no companion and is reached by an
indexed by-name child lookup downcast to the child's typed class.

## Forbidden shapes

- A companion that owns the child (an owning pointer, a lifetime-carrying member). The companion is
  a borrowed navigation handle; the runtime object tree owns the child's lifetime. An owning
  companion re-couples navigation to ownership and reintroduces a member the runtime tree must not
  duplicate.

- A `ProceduralVarDecl` carrying a declaring-scope pointer back to its enclosing scope. The downward
  ownership in the scope tree is the canonical fact; a backref is a cache that drifts.
- A `ProceduralScopeDecl` carrying a parent pointer to its enclosing scope. Same reason: the
  downward child-scope lists are the canonical fact.
- A planner that walks the statement tree to derive scope ownership. The lexical scope tree is the
  planner's input; the statement tree carries execution semantics, not declaration semantics.
- A materialization flag stored on a HIR scope decl. HIR holds lexical structure; the storage plan
  holds the realization, computed once at HIR-to-MIR shape phase.
- An unnamed scope's static exposed as a public signal of its nearest materialized ancestor.
  Physical placement does not imply hierarchical addressability; the source-name registration must
  follow the var's lexical scope, not its physical owner.
- A generate-scope MIR class standing in for a procedural storage scope. The kinds share a runtime
  base but are distinct structural concepts.

## Consequences

- The HIR-to-MIR planner is a small fold over a small tree (the procedural scope tree is sparse
  compared to the statement tree). It dispatches on scope kind, not on statement variant; the number
  of statement-tree kinds it has to handle is zero.
- A static's storage owner is a derived fact recorded once by the planner; the body lowering reads
  the resulting binding per reference without re-deriving.
- The owned-child binding registry carries an enlarged set of head kinds; the lookup path is
  unchanged.
- A named procedural block whose subtree owns no addressable storage anywhere never materializes and
  contributes nothing to the MIR class graph -- its lexical existence in HIR is sufficient for
  SV-side semantics (shadowing, future `disable`) without runtime cost.

## Rejected alternatives

- **Stmt-tree visitor in the planner.** Walk the statement tree at HIR-to-MIR shape phase to
  discover named blocks and their statics. Rejected: the statement tree carries execution semantics;
  declaration semantics live in the scope tree. Mixing the two forces the planner to dispatch on
  every control-flow statement kind to find embedded declarations and creates a walker pattern that
  has no peer elsewhere in the HIR-to-MIR layer.
- **Backref on var or scope decl.** Store a declaring-scope pointer on `ProceduralVarDecl` or a
  parent pointer on `ProceduralScopeDecl`. Rejected: duplicate of the canonical downward ownership;
  introduces drift risk and forces every IR-mutating pass to update the cache.
- **Lazy single-pass fold combining materialization and placement.** Compute both in one walk by
  returning partial state up the tree. Rejected: materialization is a bottom-up decision while
  placement is a top-down assignment; entangling them produces harder-to-verify code than two small
  clean passes over a small tree.
- **Body-lowering mints the scope on encounter.** The procedural-scope class is created when the
  body lowering enters the named begin/end. Rejected: re-binds structural graph to executable
  traversal even when the class id appears peer-unreferenced; the intra-unit downward-head rule (D7)
  shows the id IS peer-referenced.
- **Reuse the generate-scope MIR `TypeKind` for procedural storage scopes.** The runtime base is
  identical, so use one kind. Rejected: dump, debug, and per-kind semantics (named fork, disable
  label, lifetime extension) need to discriminate the source, and inference from class name is
  fragile.
- **Lexical owner equals physical owner for all cases.** Place a static in the lexical scope that
  contains it, materializing every scope that has any static. Rejected: would force materialization
  of unnamed blocks and other non-addressable lexical scopes that have no hierarchical
  addressability; the resulting runtime hierarchy would not match the SV-visible path namespace.

## Cross-references

- `declarations-before-bodies.md` -- the strict-D5 invariant a procedural-scope class respects.
- `variable-lifetime-storage.md` -- the storage-owner rule for static-lifetime body locals, read
  through the addressable-scope concept this decision provides.
- `hierarchical-reference-routing.md` -- one routing path per access shape; a named-block head
  routes as a downward head, a typed enclosing climb into the materialized scope's companion handle
  recovered from the block label; the runtime by-name walk serves cross-unit access.
- `object-model-storage.md` -- the unit's class registry that holds the procedural-scope classes.
