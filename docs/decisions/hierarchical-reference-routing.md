# Hierarchical Reference Routing

## Date

2026-06-27

## Status

Accepted. Supersedes `hierarchical-reference-resolution.md`.

## Why this decision matters

A hierarchical reference reaches a target elsewhere on the elaborated object tree. The target's
identity, the route segments traversed to reach it, the runtime endpoint the route binds to, and the
hot-path access shape are four facts that must compose into one coherent model. Prior shapes
fragmented these facts across `slang::isUpward()` dispatch, constructor-time downward installs,
Resolve-time upward wrappers, and a HIR variant per lexical form. This entry fixes the model.

## The model

A hierarchical reference is a resolved route from a structural origin to a sealed endpoint.
Structural lowering preserves canonical target and segment identities independently of lexical form.
After Build materializes the runtime hierarchy, the route resolves: each segment uses typed
navigation when its source and target classes are both owned by the emitting artifact, and the
runtime SDK across unit boundaries. Seal commits each route's final endpoint. Process bodies and
sensitivity logic consume only sealed endpoints; they never perform hierarchy traversal or name
matching.

## Decisions

### D1. One semantic shape for every hierarchical reference

HIR carries one shape for every hierarchical reference, regardless of lexical form (bare name,
downward dotted, upward dotted, absolute `$root` path). Slang already returns aligning identity
across lexical forms (the resolved leaf is one symbol pointer regardless of the form that reached
it); Lyra preserves that alignment.

`slang::isUpward()`, source order, and AST lexical form do not classify the reference and do not
choose the mechanism. They are inputs to the route synthesis at AST-to-HIR; they do not appear
beyond it.

### D2. Routes are classified per segment by layout visibility

A route is a sequence of segments. Each segment is either:

- **Layout-visible** to the emitting artifact -- the segment's source class and target class are
  both owned by this artifact. Realization: typed navigation through stable in-artifact member
  identities. No string lookup, no SDK call.
- **Opaque** to the emitting artifact -- the segment crosses into another compilation unit's body.
  Realization: the runtime SDK's by-name resolution, executed once during Resolve.

A single route may alternate. `top.gen.child.x` where `top.gen` is layout-visible, `gen.child` is
layout-visible (a same-CU instance member's pointer field), and `child.x` is opaque (descent into
Child's body) is three segments -- typed, typed, opaque -- composed in route order.

Classification is per segment. "Intra-unit vs cross-unit" is not a property of a reference; it is a
property of each segment.

### D3. The endpoint inherits the target's access protocol

A reference's sealed endpoint is the access surface of the target it reaches. A reference whose leaf
is an observable storage cell seals to that cell; a reference whose leaf is a class object handle
seals to the handle, with property access happening through the class's existing access protocol; a
reference whose leaf is a named event seals to the event, with the existing event protocol. There is
no synthetic "endpoint type" that wraps each target; the endpoint is the target itself, reached
through a sealed direct path.

The MIR representation reuses existing type variants -- a borrowed pointer to the appropriate
observable cell, the managed reference for class handles, the event surface type. The fact that a
particular field is a sealed reference (rather than, for example, a constructor-stored pointer) is
recorded at the structural layer that records the reference; the type carries only the value's
runtime shape.

### D4. `ref` ports are forwarding links collapsed at Seal

A `ref` port aliases an upstream cell. The runtime carries the alias as one route-segment
realization, not as a value process bodies hold. Seal collapses every chain through `ref` ports to
the final cell they alias; a process body's view of a hierarchical reference that traverses a `ref`
port reaches the final cell directly, never an intermediate forwarding wrapper.

A hierarchical reference whose route terminates at a `ref` port depends on the port's own
resolution. After Seal, both denote the same final cell.

### D5. Open target family; new categories need a decision

The set of target categories a hierarchical reference may seal to is open. Adding a category
requires:

1. A SystemVerilog target whose access surface cannot be expressed through any existing protocol.
2. A defined access protocol with sealed-endpoint semantics (the operations a body performs, the
   sensitivity surface).
3. A defined resolution and sealing contribution: how the route segments leading to this target
   classify, how the sealing barrier validates the result.
4. A MIR representation reusing or extending the existing type system.
5. A hot-path realization defined for every supported backend.

Interface members, chandle observability, and hierarchical callable dispatch are candidate
extensions; each needs its own decision entry.

### D6. Conditional-generate non-selected arms are sealing failures

A hierarchical reference whose route traverses a conditional-generate arm not selected at Build
cannot seal -- the traversed runtime scope does not exist. The reference is rejected at Seal as an
illegal target with a user diagnostic, not `InternalError`; the SV elaboration semantics give the
case no value. The frontend's elaboration check eliminates the case before Seal where it can;
references that survive frontend elaboration but reach a non-selected arm at Build (the elaborated
parameter environment differs from what the source expression assumed) are rejected at Seal with the
same diagnostic.

## Forbidden shapes

- A HIR or MIR variant per lexical form (a "downward reference" kind, an "upward reference" kind, a
  "root reference" kind, a "same-unit reference" kind as separate species). One semantic shape.
- `slang::isUpward()`, source order, or AST lexical kind used as a mechanism dispatch key. The
  mechanism follows segment layout visibility.
- A reference installed in the constructor body. Every reference resolves during Resolve and seals
  during Seal.
- An IR vocabulary item modeling a particular runtime library's resolver shape -- a wrapper type
  carrying bind state, a named-method family for "register a by-name climb" / "register a root
  anchor" / "append a descent step". Runtime library shapes belong to the runtime; the IR names only
  the reference, its route segments, and its sealed endpoint.
- A "no runtime slot" or "always runtime slot" mandate for a route's endpoint. Storage placement of
  the sealed endpoint is owned by LIR; MIR states the reference, its route, and the access protocol
  the body uses, not the storage shape.
- A textual name used as the identity of a layout-visible segment step. Layout-visible segments use
  stable in-artifact member identities; canonical names appear only at the opaque-segment boundary
  where the SDK consumes them.
- A forwarding wrapper observed in a process body as a hierarchical reference's endpoint. Forwarding
  wrappers exist only as resolution intermediates; they collapse at Seal.
- A hot-path access that walks the runtime tree, calls a by-name lookup, or otherwise performs
  hierarchy traversal. After Seal, every nonlocal read, write, and observation reads from a sealed
  endpoint directly.
- A reference whose mechanism flips between routes across compilation invocations because the
  generate order changed. The mechanism is a function of the route, not of source order.

## Consequences

- `mir.md` carries no per-form hierarchical reference variant and no IR vocabulary for a particular
  runtime resolver wrapper. The reference appears through the existing access primitives against the
  target's existing type.
- `emission_model.md`'s "storage and fill by direction" is replaced by "storage and fill by segment
  layout visibility." Cross-unit downward and upward installs are not separate realizations; each is
  a segment classification on one shared route.
- `reference_resolution.md`'s intra-unit / cross-unit framing applies per segment, not per
  reference.
- The runtime SDK's by-name child and signal lookup family persists as the opaque-segment resolver.
  The upward-by-name wrapper (the runtime class today carrying upward bind state) ceases to be an IR
  concept; its function moves into ordinary resolve-time route code that reaches the SDK directly
  for opaque segments and the typed traversal for layout-visible ones.
- The runtime engine treats Seal as a property of the elaborated design as a whole, exposed through
  the elaboration entry. No scope carries a `Seal` method.

## Cross-references

- `../architecture/reference_resolution.md` -- the per-segment classification.
- `../architecture/emission_model.md` -- segment realization in each artifact.
- `../architecture/elaboration_lifecycle.md` -- the five-phase lifecycle, with Seal explicit.
- `../architecture/mir.md` -- the type vocabulary the sealed endpoint reuses.
- `binding-graph-resolution.md` -- how the resolution order respects dependencies among references.
- `reference-as-data-type.md` -- the alias data type that `ref` ports use as their forwarding link.
- `object-model-storage.md` -- class declaration identity that a class-handle endpoint references.
- `hierarchical-reference-resolution.md` (superseded) -- the prior decision.
