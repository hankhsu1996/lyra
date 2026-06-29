# Binding and Capture

## Purpose

Define how a value reference inside a callable body reaches the binding it names, and how a binding
crosses a callable (closure) boundary. Every value reference names a binding **materialized in the
referrer's own body**. Reaching a binding that lives in an enclosing body is **capture**, and
capture is the only edge that crosses a callable boundary.

This is the **lexical** axis of reference: which binding, in which callable body. It is distinct
from the **object-graph** axis -- reaching a member of another object across the hierarchy -- which
`hierarchy_and_generate.md` and `reference_resolution.md` own. A single source reference may use
both; this doc owns only the lexical half.

## Owns

- **Logical binding identity.** The stable, compilation-unit-local identity of a lexical thing being
  named: a procedural local, a callable parameter, the receiver, a `with`-clause iterator, a
  synthesized lowering temporary. This identity is independent of storage and of any body's
  materialization of it.
- **Per-body materialization.** Each callable body holds its own materialization of every binding it
  references: the receiver and parameters seeded at body entry, declared locals materialized at
  their declaration, captured bindings materialized as environment fields. A value reference names a
  body-local materialization.
- **Capture as the sole cross-body edge.** A body obtains a binding it does not itself declare by
  capturing it from the enclosing body, recursively, one callable boundary at a time.
- **The carrier / view distinction.** What crosses a boundary (a transport of the binding's storage
  or value -- a _carrier_) versus how a body uses what it captured (a snapshot, a live alias, or an
  owning handle -- a _view_).
- **The receiver as an ordinary binding.** `self` is one logical binding whose role is metadata; it
  carries no capture machinery of its own.

## Does Not Own

- **Object-graph navigation.** Reaching a member of an enclosing or sibling object through parent
  pointers and casts is runtime navigation on the object tree, owned by `hierarchy_and_generate.md`;
  cross-unit resolution timing is owned by `reference_resolution.md`. This doc resolves only the
  receiver _value_ that such navigation starts from.
- **The callable concept.** Callable code, callable value, and the environment as a structural
  concept are owned by `callable.md`. This doc defines what populates that environment and how.
- **MIR node shapes.** The concrete node that names a binding, and the per-body binding arena, are
  owned by `mir.md`. This doc states the contract those shapes must satisfy.
- **Lifetime-extension policy.** Which automatic storage is promoted to a stable object so a longer-
  lived body may capture it is owned by `runtime_model.md`. This doc requires that captured storage
  be stable; it does not decide which storage qualifies.
- **The other roles of block structure.** Declaration placement, lexical visibility validation, and
  cleanup ordering remain block concerns; this doc only removes reference resolution from them.

## Core Invariants

1. **Identity is independent of storage.** A logical binding has one identity across every body that
   references or forwards it; that identity is not any one body's physical materialization. The
   cross-body key is the logical identity, never a single body's local id.

2. **A reference names a binding in its own body.** Every value reference resolves to a binding
   materialized in the referrer's own callable body. A reference that resolves into another callable
   body is not representable.

3. **Capture is the only cross-body edge.** A binding declared in an enclosing body reaches an inner
   body only as a captured environment field of that inner body. No other construct crosses a
   callable boundary to reach a binding.

4. **Capture forwards recursively, one boundary at a time.** A body obtains an outer binding by
   ensuring the binding is materialized in its lexical parent first, then capturing the parent's
   materialization. An intervening body that never names the binding itself still materializes a
   forwarding capture so its child can capture from it. Reaching past an intervening body to an
   outer body's materialization is forbidden.

5. **Carrier is per-boundary; view is per-body.** What crosses a boundary is a carrier sufficient
   for the capturing body to obtain the binding (a value, a reference to an observable cell, an
   owning or shared handle). How a body uses a captured binding -- snapshot now, keep a live alias,
   retain ownership -- is decided by that body's own requirement. An inner body's view never
   dictates an intervening body's carrier.

6. **Lifetime is carried, never reached for.** A body that can outlive the frame that materialized a
   binding captures a carrier over stable storage (an owning or shared handle, or a value snapshot),
   never a path that depends on the materializing frame still being live.

7. **The receiver is an ordinary binding.** `self` is one logical binding with a receiver role used
   only by callable lowering and diagnostics; the role never changes capture semantics. A member
   access resolves the receiver binding through invariants 1-6, then applies object-graph navigation
   to that resolved receiver value.

8. **Block structure does not resolve references.** Block nesting governs declaration placement,
   lexical visibility, and cleanup ordering. It is never the mechanism by which a reference locates
   its binding.

## Boundary to Adjacent Layers

- **HIR** carries source-level lexical scope (declarations and the variable/receiver references that
  name them). **HIR-to-MIR** performs binding resolution and materialization: it seeds each body's
  receiver and parameters, materializes declared locals, and inserts forwarding captures across
  every callable boundary a reference crosses.
- **`callable.md`** owns the callable value and its environment as structure. The environment fields
  are exactly the captures this doc materializes; a captured field's snapshot-versus-alias nature is
  its carrier/view choice (invariant 5), carried by the field's type.
- **`mir.md`** owns the node that names a binding and the per-body binding arena. The node names a
  body-local binding (invariant 2); it carries no navigation distance to a declaration.
- **`reference_resolution.md`** owns the object-graph and cross-unit axis. A multi-segment source
  reference splits cleanly: the receiver value is resolved by this doc (lexical capture), and the
  object-graph hops from that receiver to the final member are resolved by that doc. The two never
  merge into one distance.
- **`hierarchy_and_generate.md`** owns the object tree that object-graph navigation walks.
- **`runtime_model.md`** owns which automatic storage is promoted to a stable object so a longer-
  lived body may capture an owning/shared carrier over it.

## Forbidden Shapes

- **A reference as a navigation distance to a declaration.** Naming a binding by "the original
  declaration plus a hop count" -- in particular expressing a capture source as a hop count that
  crosses a callable boundary. A reference names a body-local binding; it carries no route.
- **One materialization used as identity.** Using a single body's local id as the cross-body key for
  a binding, so deduplication or forwarding keys off a physical slot rather than the logical
  identity.
- **A receiver special case.** A privileged receiver capture slot, a per-form receiver mechanism, or
  any capture-time branch on "is this the receiver". The receiver is captured exactly as any
  binding.
- **Blind carrier propagation.** Forcing every intervening body to adopt an inner body's view --
  propagating "by value" or "by reference" unchanged through a forwarding chain instead of choosing
  a carrier per boundary and a view per body.
- **Climbing block scopes across a callable boundary** to reach an outer binding, or treating a
  callable boundary as a kind of block boundary.
- **Lifetime by reachability.** Relying on a parent frame still being live to reach a captured
  binding, instead of capturing an owning or shared carrier.
- **Reconstructing the object graph lexically.** Reaching an enclosing object's member by lexical
  capture instead of explicit object-graph navigation, or vice versa. The two axes stay separate.

## Notes / Examples

The whole model is one recursive resolution. Conceptually:

```text
EnsureAvailable(body, logical_binding, requirement) -> body_local_binding:
  if body already materializes logical_binding:        return it
  parent = EnsureAvailable(body.lexical_parent, logical_binding, transport)
  field  = capture parent into body as a carrier
  view   = expose field per requirement (snapshot / alias / owning)
  record body materializes logical_binding as view;    return view
```

Every closure shape is a consequence of this single recursion, with no per-shape handling:

- A nonblocking assignment captures its target as a place carrier and its value as a snapshot; its
  deferred body has no receiver because it names no member.
- A deferred `$strobe` and a `fork` branch capture whatever bindings their bodies name.
- A binding named two callable boundaries deep is forwarded through the intervening body even when
  that body never names it -- the intervening materialization is created by the recursion, not by
  syntactic use.
- A member access resolves its receiver binding by the same recursion (the receiver is captured per
  boundary), then navigates the object graph from the resolved receiver to the member.

The receiver sits at the intersection of the two axes: it is a lexical binding (captured per body by
this model) used as the starting object for object-graph navigation (resolved by
`reference_resolution.md`). Because the receiver is just a binding, no closure construction needs to
know it exists; a body that names a member captures the receiver as a side effect of the same
recursion that captures any local.

## Binding Representation and Closure Environment

The sections above state _that_ a reference names a binding in its own body and _that_ capture is
the only cross-body edge. This section states _what shapes_ carry those facts. It uses **binding
origin** for what the Purpose section calls "logical binding identity"; the two are the same
concept, and "binding origin" is its canonical name from here on.

A binding is one thing at the language level but appears at four representation layers, and keeping
them separate is the point: collapsing any two is what reintroduces the bug class this reset
removes.

```text
1. Binding origin           which variable / parameter / receiver / synthesized entity this is;
                            stable across every body that names or forwards it.
2. Body binding             what one callable body reads directly: an activation local, or a
                            captured environment field of that body.
3. Capture layout           the environment fields a closure's code requires, independent of any
                            single construction of it.
4. Capture initialization   for one construction of a closure value, where each field's value comes
                            from.
```

Layer 1 is identity; layer 2 is per-body materialization; layer 3 belongs to the callable code;
layer 4 belongs to the callable value.

### Binding origin (layer 1)

A binding origin is the stable, compilation-unit-local identity of a named thing, independent of
storage and of any body's materialization. It is the key under which a binding is deduplicated and
forwarded across boundaries.

- A **source origin** names a source-level entity: a procedural variable, a `with`-clause iterator,
  or a receiver parameter.
- A **synthesized origin** names a lowering-created entity that must cross a callable boundary (a
  non-blocking-assignment value snapshot, a shared-activation handle). It is identified by its
  deterministic semantic owner -- the source-level site that caused it, plus a purpose and an
  ordinal -- never a global counter, so identity is stable under incremental and parallel
  compilation and a dump is self-explanatory.

The receiver is not a separate kind: it is a parameter origin whose role is "receiver." The role is
metadata for callable lowering and diagnostics; it never changes capture semantics. A single body's
local slot is never an origin (that is the "one materialization used as identity" forbidden shape).

### Body binding (layer 2)

What a callable body reads directly is exactly one of two things:

```text
body binding = Local(local id) | Capture(capture id)
```

- `Local` names an entry in the callable's single activation arena. A parameter is a `Local`: the
  signature is a prefix of that arena. There is no separate parameter reference kind and no second
  id space for parameters.
- `Capture` names an entry in the callable's capture layout.
- The receiver is a `Local` (a receiver-role parameter at the start of the arena) when a callable is
  invoked directly, and a `Capture` when a closure binds it. It is never a privileged slot.

A reference carries no navigation distance: within a callable there are no hops (one arena); across
a callable it is a `Capture`, resolved by forwarding.

### Source resolution versus carrier availability

Not every source variable is a directly-readable body binding. Resolving a source reference is two
steps, kept distinct:

- **Source resolution** answers "how is this source variable realized in this body." It yields an
  ordinary MIR expression: a body-binding read for an automatic variable, or a member access over a
  carrier for a static or promoted variable. There is no separate place-path or projection language;
  a projection is an ordinary member-access (or deref / select) expression, the same nodes every
  other access uses.
- **Carrier availability** answers "make this _carrier_ -- a thing that can cross a boundary --
  available as a body binding here," forwarding along the lexical-parent-callable chain one boundary
  at a time.

The carrier is what crosses a boundary: a receiver, a shared-activation handle, or an ordinary local
-- never a static or promoted variable as such. Promotion is a storage strategy, not an identity
change: a promoted automatic variable keeps its own source origin, and is reached as a member access
over a handle carrier whose own origin is a synthesized one. The variable's origin never becomes the
handle's origin, so capture deduplication never sees one variable as two things.

### Capture layout and capture initialization (layers 3 and 4)

A closure's environment splits between the code (the layout it requires) and the value (one
construction's initializers):

```text
callable code . captures      : a list of capture declarations
capture declaration           { carrier_origin, field_type }
closure value . capture_inits : a list of capture initializers
capture initializer           { target capture, source expression }
```

- A captured field is **not** an activation local: it is a distinct id space, set once per closure
  construction, whereas an activation local is created per invocation.
- A capture declaration names the **carrier** transported across the boundary, not necessarily the
  source variable later reached through it. Several promoted variables sharing one activation handle
  capture that one handle once.
- Snapshot / alias / owning is the field's **type** -- a value type is a snapshot, a reference type
  aliases an enclosing cell, a shared handle owns. There is no separate capture-mode field.

**Capture-init sequencing.** A closure value's capture initializers are evaluated at the closure
construction site, in declaration order, before the value exists. An initializer that cannot be
evaluated safely in place -- one with a side effect, or one that must be computed exactly once at a
specific point -- is materialized as a sequenced temporary first, and the initializer reads that
temporary. A backend whose target does not guarantee that order at the construction site emits the
temporaries explicitly.

### Carrier and view

- **View** (per body): how a body uses a captured binding -- snapshot, alias, or owning. Decided by
  the capturing body's own requirement; an inner body's view never dictates an intervening body's
  carrier.
- **Carrier** (per boundary): what is read at the construction site to initialize the field.

The view is decided by closure-kind policy, not by lexical nesting depth and not lazily by the first
use. Two construction paths exist:

- **Direct capture:** the construction site captures a specific outer value and states the view
  itself. No relation classification is needed.
- **Forwarded capture:** while a body is lowered, a reference to an origin outside the body's
  boundary is captured automatically. The view is the closure kind's policy applied to the origin's
  relation to the closure's construction scope. For a `fork` branch, the construction scope is the
  explicit set of the fork's own block-item declarations: an origin in that set is snapshotted, an
  enclosing origin aliases, and an enclosing origin a longer-lived body could outlive is promoted
  upstream to an owning carrier. The relation is set membership over origins, never a depth.

A body's lexical declaration ownership lives in the block tree (where a declaration statement sits),
not in a stored scope id; the construction-scope set is the only additional fact a capture policy
needs, and it is explicit at the construction site.

### Resolver

One binding-resolution context per callable body holds: the callable's arenas, the lexical parent
callable, the construction site (the parent program point that owns closure construction and its
ordering), and the per-origin map of carriers already materialized in this body.

**Canonical carrier uniqueness.** Within one callable body, a carrier origin has exactly one
canonical carrier binding. A child closure chooses its own field type and initializes it from the
parent's canonical carrier; the parent never grows multiple same-origin carriers to serve different
children. If a body genuinely needs two representations of one origin, either one is derived from
the canonical carrier (a projection or temporary) or they are in fact distinct carrier origins.

### Representation invariants

1. A reference is a body binding -- a `Local` or a `Capture` of its own callable. It carries no
   distance and never names another callable's slot.
2. A binding origin is the cross-body key; a body's physical slot is never the key.
3. A captured field and an activation local are different id spaces -- set-once versus
   per-invocation.
4. A captured field's snapshot / alias / owning nature is its type; there is no parallel
   capture-mode field.
5. A source variable's origin is invariant under promotion; promotion changes its carrier, never its
   identity.
6. Capture initializers are sequenced at the construction site in declaration order; a non-inline
   initializer is materialized as a sequenced temporary.
7. Capture view is decided by closure-kind policy from the origin's relation to the construction
   scope, never by lexical depth and never by first-use order.
8. Every non-parameter local has exactly one declaration site; every reference to it is lexically
   dominated by that site and does not escape its lexical lifetime. (Flat per-callable locals lose
   the per-block arena's accidental guard, so this is a construction invariant and a dumper / verify
   check.)

### Additional forbidden shapes

- A captured field represented as an activation local distinguished only by appearing in an
  environment list. Captures are their own id space.
- A source-resolution entry point that returns a direct binding in one case and a carrier or member
  access in another. Source resolution and carrier availability are distinct steps.
- A capture-mode flag or enum beside the field type.
- A capture view chosen by lexical nesting depth, or by the first use encountered during traversal.
- A promoted variable whose origin becomes its activation handle (one variable, two origins).
- A capture initializer left as a detached side-effecting expression, relied upon to evaluate
  correctly without an explicit sequencing point.
- A stored lexical-scope id duplicating the block tree, used to decide capture policy.
- A binding subsystem inventing a second projection or place language instead of reusing the
  existing access expressions.

### Reconciliation with sibling contracts

- `mir.md`: the receiver is the body's first binding (`locals[0]`); the binding arena is the
  callable's, not a per-block one. `locals[0] == self` is a codegen convention; the generic
  invariant is that an instance callable has a receiver parameter binding.
- `callable.md`: the environment is the closure's capture layout (its own id space), not a prefix of
  the per-invocation parameter list. The snapshot-or-alias-by-type rule (`callable.md` invariant 5)
  is retained and is the reason capture mode is not a separate field.
