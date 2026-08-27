# Reference Resolution

## Purpose

Define how a reference reaches its target. A route from referrer to target is a sequence of
segments; each segment's realization follows whether the referrer has a declaration to compile
against for that step. The same resolution serves both accessing the target's value and observing
its changes.

## Owns

- The decomposition of a reference's route into segments and the rule that each segment is
  classified independently by whether the referrer has a declaration for its target.
- The rule that a declared segment realizes as typed navigation -- through an in-artifact member
  identity, or through a signature member's name resolved at the referrer's compile time -- and that
  an opaque segment realizes as a runtime-SDK by-name lookup.
- The contract that route execution is total: a route that fails to seal is either a
  user-diagnosable elaboration error (a non-constructed target, a forwarding cycle with no storage
  root) or a compiler-invariant violation, never a runtime fallback.
- The contract that port connections, hierarchical references, and cross-instance trigger
  subscriptions all flow through the same binding-graph route mechanism. No form has its own
  parallel resolution.
- The contract that a sealed endpoint serves both value access and change observation through one
  stored reference.
- The rule that connectivity is linkage between objects and never alters what an object owns.

## Does Not Own

- The shape of the object graph, the construction order, and the graph's faithfulness to the
  frontend's elaboration (see `hierarchy_and_generate.md`). This doc relies on that faithfulness but
  does not establish it.
- What each unit kind publishes, which is what decides whether a segment is declared (see
  `compilation_unit_model.md`).
- The compile-time identity kinds used to thread route segments (see `identity_and_ownership.md`,
  `hir.md`, `mir.md`).
- The lexical axis of reference: how a value reference names a binding in its own callable body, and
  how a binding crosses a closure boundary by capture (see `binding_and_capture.md`). This doc owns
  the object-graph and cross-unit axis; a multi-segment reference splits cleanly between the two,
  with the receiver value resolved lexically and the object-graph hops resolved here.
- How an observed change wakes a dependent process (see `scheduling.md`).
- Storage placement and offsets of members (see `lir.md`).
- The phases the route executes in (see `elaboration_lifecycle.md`).
- Net resolution and net merging across ports (a single simulated net shared by both sides). That is
  a design-global net-resolution concern, separate from per-object reference resolution; a net's own
  driver resolution is owned by `net_resolution.md`, which reaches its drivers through this route.

## Core Invariants

1. Every reference is a route from a structural origin to an endpoint. The route is a sequence of
   segments; the endpoint, once sealed, is the canonical access point for every read, write, and
   observation against the reference.
2. Each segment is classified by whether the referrer has a declaration to compile against for that
   step. A **declared** segment's target is one the referrer already compiles against -- a class its
   own artifact owns, or a member on a signature it consumes -- and its realization is typed
   navigation, with no string lookup and no SDK call at simulation or elaboration time. An
   **opaque** segment reaches past a unit's signature, where the referrer has no declaration; its
   realization is a runtime-SDK by-name lookup. A single route may alternate segments freely.

   A declared segment's identity comes from one of two places, which changes nothing about its cost
   or its realization: an in-artifact member identity when the emitting artifact owns both classes,
   and the member's name resolved against the consumed signature at the referrer's compile time when
   it does not. The second is not a weaker identity -- a name checked by the compiler that consumes
   it and a name looked up in a run-time registry share a spelling and nothing else.

3. A reference's route executes once during the binding-graph phase, producing a candidate endpoint;
   the endpoint is committed at the sealing barrier and read directly thereafter. Simulation-time
   access reads the sealed endpoint with no per-access lookup, no hierarchy traversal, and no name
   matching. One sealed endpoint serves both value access and change observation.
4. Route execution is total in the architecture's contract. The frontend fully elaborates and
   validates every reference, and the constructed object graph is faithful to that elaboration (see
   `hierarchy_and_generate.md`). A reference to a non-constructed runtime target (a non-selected
   conditional-generate arm, an out-of-range instance-array element) is rejected at the sealing
   barrier with a user diagnostic; a route whose segments cannot otherwise execute is an
   `InternalError`.
5. Port connections, hierarchical references, and cross-instance trigger subscriptions share one
   route mechanism. There is no parallel resolution path per reference kind, per direction, or per
   lexical form.
6. Connectivity is linkage between objects. For a variable member it never removes the object's
   storage, never changes the object's layout, and never makes a member's addressing depend on what
   it is wired to. (Merging nets into a single shared net is a design-global net-resolution concern,
   outside this contract.)
7. The endpoint inherits the access protocol of the target it reaches. A reference to an observable
   storage cell reads and writes through the cell's protocol; a reference to a class handle
   dereferences the handle, then operates on the class object; a reference to an event participates
   in the event's protocol. The endpoint is not a new access category; it is the target's access
   surface reached through a sealed direct path.

## Boundary to Adjacent Layers

- `compilation_unit_model.md` owns the signature each unit kind publishes, which is what decides
  whether a segment is declared to a given artifact.
- `hierarchy_and_generate.md` owns the object graph the routes navigate and the faithfulness of that
  graph to the frontend's elaboration that makes route execution total.
- `runtime_model.md` places route execution in the constructor context (the binding-graph phases at
  t = 0) and access in the simulation context (t >= 0).
- `elaboration_lifecycle.md` owns _when_ a route executes and seals. Routes execute in Resolve;
  endpoints commit in Seal; initializers and reads observe only sealed endpoints.
- `identity_and_ownership.md` owns the identity rules that route segments thread.
- `scheduling.md` owns the wakeup that fires when a sealed endpoint's underlying cell changes.

## Forbidden Shapes

- Compiled body code that bakes another unit's layout to realize an opaque segment at compile time.
  An opaque segment is by-name through the SDK by definition; baking the target's layout is the
  canonical artifact-boundary violation.
- Route mechanism dispatched on the frontend's lexical-form classification or on source order. The
  mechanism follows whether each segment is declared to the referrer; the form that named the target
  is not visible past the AST-to-HIR boundary.
- An opaque segment used for a name the target unit published. A published name has a declaration to
  compile against, so routing it through the SDK converts a compile-time check into an elaboration
  failure and leaves an unchecked cast where the check belonged.
- A reference shape per direction or per lexical form (as separate species). One semantic shape; one
  route decomposition.
- Resolution driven from the target's side: the referenced unit wiring, pushing, or storing its own
  member into the units that reference it. A unit compiles against its own signature and cannot know
  who references it, so it never drives resolution for a consumer and never carries a list of its
  referrers. Every route's opaque segment is driven by the referrer, which reaches the target by
  name at Resolve; the target only answers by-name queries about its own declarations.
- A typed segment naming a declaration the target unit did not publish -- an internal member, an
  internal type, a child it owns. A typed segment stops at what the referrer compiles against; the
  step that reaches past a signature is opaque by contract.
- A resolution path for ports that is distinct from the one for hierarchical references. Two
  mechanisms for cross-instance access is the canonical violation.
- Resolution by flattened symbol-name lookup or a design-global path table that mirrors the object
  graph.
- A per-access runtime lookup on the simulation path. Routes execute once during elaboration; the
  hot path reads a sealed endpoint. A hot-path read that performs any hierarchy traversal, parent
  walk, or by-name lookup is the canonical hot-path violation.
- A route keyed or resolved by a design-global coordinate, ordinal, or instance id.
- A reference whose sealing failure is silently swallowed at runtime. A user-diagnosable failure
  (non-constructed target, forwarding cycle without storage root) surfaces a user diagnostic; an
  impossible-by-contract failure raises `InternalError`. There is no runtime fallback.
- A binding installed in the constructor block. Routes execute in Resolve and seal in Seal; ctor
  allocates the shell only.
- A runtime library wrapper for a particular resolver strategy appearing as an IR concept. Wrappers
  belong to the runtime; HIR and MIR vocabulary names only the reference, its route, and its
  endpoint.
- A forwarding wrapper observed on the hot path. Forwarding chains collapse to the final endpoint at
  sealing; a sealed endpoint reaches its target directly.
- Connectivity that eliminates an object's local storage for a member, or that changes the object's
  layout based on what the member is wired to.
- Member addressing that differs between a wired and an unwired member.

## Notes / Examples

**Same-unit sibling reference.** `always_comb from_b = b.bx;` inside generate block `a` of `Top`,
where `b` is a sibling generate of `a`. The route has two segments: `a -> Top` (the typed parent
edge) and `Top -> b -> bx` (typed member access into the sibling generate's class then into its
variable). Both segments are declared: `Top`'s artifact owns `a`'s class, `b`'s class, and the `bx`
member. The route executes the typed chain once and seals to the variable's cell. The mechanism does
not depend on whether `a` is declared before or after `b`.

**The same cross-unit reference splits by what the child published.** `always_comb r = c.p;` and
`always_comb r = c.x;` inside a parent module, where `c` is a child module instance, `p` is one of
its ports, and `x` is an internal variable. Both routes begin with the same declared segment
`parent -> c`, reached through the `c` member's pointer type the parent's artifact owns. They differ
at the second step, and the child decides which: `c -> p` is declared, because `p` is on the
module's signature and the parent already compiles against it, so a renamed port fails where the
parent compiles; `c -> x` is opaque, because the child published nothing to compile against, so the
name resolves through the SDK at Resolve and a renamed `x` fails at elaboration. Both seal to a
cell, and the body and its sensitivity read the sealed endpoint either way.

**Multi-segment mixed route.** In `top.gen.child.x`, `top.gen` is declared (the generate scope
belongs to top's unit), `gen.child` is declared (`child` is a member of `gen`'s class in the same
unit), and `child.x` is opaque (`x` is not on `child`'s signature). The route alternates two typed
segments and one opaque segment; the SDK is reached only where the route passes a signature.

**Port connections share the routing.** An input or output port is a continuous-assignment edge
between the two objects' own storage (LRM 23.3.3); the cross-unit side reaches the partner cell
through one route, whose final segment is declared because a port is on the module's signature. A
`ref` port (LRM 23.3.3.2) is a forwarding link that resolves to the connected cell at sealing. An
`inout` port is a bidirectional net connection and belongs to the deferred net-resolution domain.
Every form whose cross-instance reach passes through Resolve shares the one route mechanism.

**Routing is deferred to Resolve, not dynamic.** The deferral keeps each unit independently and
incrementally compilable; it carries no semantic uncertainty. The frontend has already proven every
reference has a determinate target, so route execution always succeeds (or surfaces a
user-diagnosable elaboration error at the sealing barrier).
