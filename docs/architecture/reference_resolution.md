# Reference Resolution

## Purpose

Define how a reference reaches its target, and when. Every reference resolves to a concrete target;
the compilation-unit boundary decides the timing. A reference whose target is in the same unit
resolves at compile time. A reference whose target is in another unit resolves once at construction.
The same resolution serves both accessing the target's value and observing its changes.

## Owns

- The classification of a reference as intra-unit or cross-unit by where its target lives relative
  to the referrer's compilation unit.
- The rule that an intra-unit reference resolves at compile time and a cross-unit reference resolves
  once at construction into a stored direct reference.
- The contract that cross-unit resolution is total: a resolution that fails is a compiler-invariant
  violation, not a user error.
- The contract that port connections, hierarchical references, and cross-instance triggers all reach
  across the unit boundary through a cross-unit reference on one shared resolution path.
- The contract that a cross-unit reference's resolution serves both accessing the target and
  observing its changes, through one resolved reference.
- The rule that connectivity is linkage between objects and never alters what an object owns.

## Does Not Own

- The shape of the object graph, the construction order, and the graph's faithfulness to the
  frontend's elaboration (see `hierarchy_and_generate.md`). This doc relies on that faithfulness but
  does not establish it.
- The compilation-unit boundary that defines what counts as intra-unit (see
  `compilation_unit_model.md`).
- The compile-time id kinds for intra-unit versus cross-unit references (see
  `identity_and_ownership.md`, `hir.md`, `mir.md`).
- How an observed cross-unit change wakes a dependent process (see `scheduling.md`).
- Storage placement and offsets of members (see `lir.md`).
- Net resolution and net merging across ports (a single simulated net shared by both sides). That is
  a design-global net-resolution concern, separate from per-object reference resolution.

## Core Invariants

1. Every reference resolves to a concrete target and is classified as intra-unit or cross-unit by
   where that target lives relative to the referrer's compilation unit.
2. An intra-unit reference resolves at compile time. The unit knows the layout of every object it
   constructs, so the target is a constructed-object reference the unit owns plus a compile-time
   offset. A reference into a generate scope is intra-unit even though the scope is a distinct
   runtime object: the generate scope belongs to the same unit, so its layout is known at compile
   time.
3. A cross-unit reference cannot be resolved at compile time, because the target lives inside
   another independently compiled unit whose layout is not visible during this unit's compilation.
   It is resolved once, at construction, when the program is linked and the target unit's layout is
   available, and stored as a direct reference to the target object's member. Simulation-time access
   reads the stored reference directly, with no per-access lookup. The same stored reference serves
   both value access and change observation; sensitivity to a cross-unit member rides this
   reference, not a separate path.
4. Cross-unit resolution is total. Every cross-unit reference is resolvable, because the frontend
   fully elaborated and validated it before lowering and the constructed object graph is faithful to
   that elaboration (see `hierarchy_and_generate.md`). A resolution that fails at construction is an
   `InternalError`, never a user diagnostic and never a runtime fallback.
5. Port connections, downward and upward hierarchical references, and cross-instance trigger
   subscriptions all reach across the unit boundary through a cross-unit reference and share the
   single resolution path. No form has its own parallel resolution.
6. Connectivity is linkage between objects. For a variable member it never removes the object's
   storage, never changes the object's layout, and never makes an intra-unit member's addressing
   depend on what it is wired to. (Merging nets into a single shared net is a design-global
   net-resolution concern, outside this contract.)

## Boundary to Adjacent Layers

- `compilation_unit_model.md` owns the unit boundary. Intra-unit is "inside this unit"; cross-unit
  is "across that boundary".
- `hierarchy_and_generate.md` owns the object graph that a cross-unit reference resolves into, and
  the faithfulness of that graph to the frontend's elaboration that makes cross-unit resolution
  total.
- `runtime_model.md` places resolution of cross-unit references in the constructor context and the
  access in the simulation context. A cross-unit reference resolves at t = 0 and is read or written
  at t >= 0.
- `identity_and_ownership.md` requires a cross-unit reference to be a distinct id kind from an
  intra-unit id.
- `scheduling.md` owns the wakeup that fires when a resolved cross-unit member changes.

## Forbidden Shapes

- Compiled body code that bakes in another unit's layout to resolve a cross-unit reference at
  compile time.
- Resolving a cross-unit reference from the target's side: the referenced unit wiring, pushing, or
  storing its own member into the units that reference it. A unit compiles against its own interface
  and cannot know who references it, so it never drives resolution for a consumer and never carries
  a list of its referrers. Every cross-unit reference -- downward or upward -- is driven by the
  referrer, which reaches the target by name at construction; the target only answers by-name
  queries about its own interface.
- The referrer reaching a cross-unit target by typed member access into the other unit's body
  (naming the other unit's members, fields, or child types) instead of by name against its
  interface. Typed access bakes the target unit's layout into the referrer and breaks independent
  compilation -- this holds even for a downward reference whose referrer instantiated the target,
  and even when the access is evaluated at construction rather than compile time. The referrer knows
  the target's name, never its layout.
- A resolution path for ports that is distinct from the one for hierarchical references. Two
  mechanisms for cross-unit access is the canonical violation.
- Resolving a cross-unit reference by flattened symbol-name lookup or a design-global path table
  that mirrors the object graph.
- A per-access runtime lookup on the simulation path. Cross-unit references resolve once at
  construction; the hot path reads a stored direct reference.
- A cross-unit reference keyed or resolved by a design-global coordinate, ordinal, or instance id.
- A user-facing diagnostic or a runtime fallback for a cross-unit reference that fails to resolve.
  Such a failure is a compiler-invariant violation and raises `InternalError`.
- Connectivity that eliminates an object's local storage for a member, or that changes the object's
  layout based on what the member is wired to.
- Intra-unit member addressing that differs between a wired and an unwired member.

## Notes / Examples

`always_comb r = c.x;` inside a parent module, where `c` is a child module instance: the target `x`
lives inside `c`'s unit, so this is a cross-unit reference. The parent's compiled body does not know
`x`'s offset. At construction, once `c` is built, the reference resolves to `c`'s storage for `x`
and is stored; the process then reads it directly. Because the body reads `c.x`, the process is also
sensitive to it: the re-evaluation when `c.x` changes rides the same resolved reference, not a
separate sensitivity path. An input port `.x(src)` and an upward reference `Parent.v` are also
cross-unit references and resolve through the same path; they are not separate features.

A multi-segment path mixes both kinds. In `top.gen.child.x`, navigating `top.gen` is intra-unit (the
generate scope belongs to `top`'s unit), reaching `gen.child` is an intra-unit reference to a child
object the unit owns, and `child.x` crosses into `child`'s unit. The reference's classification
follows its final target: `x` is cross-unit. Construction performs the full navigation once and
stores the direct reference to `x`.

A module port connection reaches across the unit boundary through a cross-unit reference, resolved
at construction like any other. The connection's form follows the port: an input or output port is a
continuous assignment between the two objects' own storage (LRM 23.3.3, 23.3.3.2), with the
cross-unit side resolved as a reference; a `ref` port is itself a hierarchical reference to the
connected variable, with no separate storage (LRM 23.3.3.2); an `inout` port is a bidirectional net
connection and belongs to the deferred net-resolution domain (LRM 23.3.3.2). For input, output, and
`ref` ports the cross-unit reach uses the one resolution path.

Resolution is deferred, not dynamic. The deferral to construction is a compilation-architecture
choice that keeps each unit independently and incrementally compilable; it carries no semantic
uncertainty. The frontend has already proven every cross-unit reference has a determinate target, so
construction always succeeds.
