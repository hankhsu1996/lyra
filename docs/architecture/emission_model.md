# Emission Model

## Purpose

Define how a backend turns per-unit MIR into runnable artifacts while preserving the independent,
parallel compilation that `north_star.md` requires. This doc owns the **emit-side realization** of
the compilation-unit boundary and of cross-unit reference resolution: what one unit's emitted
artifact may depend on, how cross-unit access is expressed without breaking unit independence, and
what role the runtime SDK plays as the link-time-resolution substrate. It is the missing layer
between the principle in `reference_resolution.md` (a cross-unit reference resolves at construction,
not at the referrer's compile time) and the backend code that must obey it.

The C++ backend exists today; an LLVM backend is the eventual target. The rules here are stated for
any backend. Where the current C++ backend takes a transitional shortcut, that is noted as
non-conforming code, not as a relaxation of the contract.

## Owns

- The rule that a backend emits one artifact per compilation-unit specialization, and that the
  program is assembled by linking those artifacts -- never by a single artifact that aggregates many
  units.
- The set of inputs one unit's emission may depend on: its own MIR, the interfaces of the units it
  instantiates, and the runtime SDK.
- The contract that a cross-unit reference into another unit's per-instance layout is realized at
  construction through the SDK / object graph, that a cross-unit call of a package's receiver-less
  callable (which has no per-instance layout) is instead a direct link-time symbol, and that either
  way the referrer's emitted artifact never embeds another unit's storage layout and never names a
  unit it neither instantiates nor calls.
- The role of the runtime SDK as the substrate that stands in for link-time symbol resolution and
  (under LLVM) intrinsics.

## Does Not Own

- Where the runtime library lives and how a binary locates it (see `runtime_distribution.md`).
- The compilation-unit boundary itself and what an interface is (see `compilation_unit_model.md`).
- When and into what a cross-unit reference resolves, semantically (see `reference_resolution.md`);
  this doc owns only how a backend _realizes_ that resolution.
- The object-graph shape the resolution navigates (see `hierarchy_and_generate.md`).
- IR-layer shapes (see `mir.md`, `lir.md`).

## Core Invariants

1. **One artifact per unit specialization; the program is linked, not aggregated.** A backend emits
   each unit's code as a self-contained artifact. No emitted artifact contains the bodies of more
   than one unit, and none enumerates "all units." The program is formed by linking the per-unit
   artifacts. This is what keeps compilation parallel and incremental (`north_star.md` inv 3, 4).
2. **A unit's emission depends only on itself, the interfaces of the units it references, and the
   SDK.** The inputs to emitting unit U are: U's own MIR; the interface (name, parameters, ports for
   an instantiated unit; declared callables and variables for a referenced package) of each unit U
   references -- one it instantiates, one it calls a receiver-less callable of (LRM 26.3), or one it
   reads or writes a static variable of by name (LRM 26.2); and the runtime SDK. U's artifact never
   depends on a unit it neither instantiates nor references, and never on another unit's body or
   internal layout (`compilation_unit_model.md` inv 8, which already scopes this to a unit U
   "instantiates or references").
3. **The runtime SDK is the link-time-resolution substrate.** Cross-unit operations the referrer
   cannot resolve from its own inputs are expressed as SDK operations. In the C++ backend the SDK is
   the runtime library; under LLVM its operations become intrinsics the linker resolves. A backend
   never invents a second cross-unit mechanism outside the SDK.
4. **A reference's route resolves once and seals into a sealed endpoint.** Routes execute in
   Resolve; each route produces a candidate endpoint that the sealing barrier commits as the
   reference's final access point. The simulation-time read and change observation read the sealed
   endpoint directly with no per-access lookup (`reference_resolution.md` inv 3, 5).
5. **Storage and fill by segment layout visibility.** A route is a sequence of segments; each
   segment's realization is determined by whether the emitting artifact owns the segment's source
   and target classes:
   - **Layout-visible segment** -- both classes live in the emitting artifact. Realization is typed
     navigation through stable MIR member identities. The emitted code reaches the next pointer /
     class member via a typed access expression; no string lookup, no SDK call.
   - **Opaque segment** -- the segment crosses into another compilation unit's body. Realization is
     one runtime-SDK by-name lookup, executed once at Resolve. The opaque segment carries the
     canonical name (interface or registered signal name) and any indices it needs; it never names
     the other unit's internal members, fields, or child types. A single route may alternate; an
     artifact emits typed prefix code for the layout-visible segments and SDK calls for the opaque
     ones, composed in route order. The sealed endpoint is one access point regardless of how many
     segments of each kind the route contained.
6. **A unit exposes its hierarchically reachable signals and owned children through the SDK.** So
   that any artifact's opaque segments can reach into a unit whose body it does not know, each unit
   registers its reachable signals and its owned children by name into the object graph node during
   construction, and the base SDK answers a by-name query from those registrations -- the unit never
   inspects who asks, and the dispatch is one generic scan, not a per-unit synthesized branch. The
   referrer's emission consumes those registrations through one route execution and stores the
   sealed endpoint; it never embeds another unit's layout.

## Boundary to Adjacent Layers

- `compilation_unit_model.md` defines the unit and its interface; this doc defines what a unit's
  _emitted artifact_ may depend on, which is exactly that interface plus the SDK.
- `reference_resolution.md` defines route segment classification and the sealing contract; this doc
  defines how a backend realizes each segment kind without breaking unit independence.
- `backend_contract.md` defines the per-node within-an-artifact realization rules: how a MIR node
  becomes target-language source. This doc draws the artifact boundary; `backend_contract.md`
  governs what happens inside.
- `runtime_distribution.md` owns where the SDK/runtime lives; this doc owns the SDK's role as the
  opaque-segment resolution substrate.
- `runtime_model.md` places route execution in the constructor context and the read in the
  simulation context.

## Forbidden Shapes

- An emitted artifact that contains more than one unit's bodies, or that enumerates all units (a
  global "wiring" file). This is the canonical violation: it serializes otherwise-independent
  compilation and reintroduces an undeclared whole-design dependency.
- A referrer's artifact that names, includes, or casts to the type of a unit it neither instantiates
  nor calls -- in particular, an opaque-segment realization naming the target unit's internal type,
  member, or field. Naming a called package's own declared callable is not this shape: a package
  exposes only namespace-level declarations and no instance layout, so a caller reaches its callable
  by the same by-name link every ordinary symbol uses, never by the target's internal layout.
- Embedding another unit's storage offset in the referrer's own emitted output. An opaque segment is
  by-name through the SDK; a layout-visible segment uses a stable in-artifact member identity, not
  an offset.
- A route mechanism dispatched on the frontend's lexical-form classification or on source order.
  Mechanism follows segment layout visibility.
- A reference shape that splits cross-unit and intra-unit references into separate IR species,
  separate install paths, separate vocabulary items. One reference, one route, one sealing.
- A design-global signal or path table that mirrors the object graph. Opaque segments resolve
  through local object-graph navigation (the parent chain, an owned child); a per-object by-name
  registration is local and permitted, a global flat table is not.
- A per-access cross-instance lookup on the simulation path; the hot path reads a sealed endpoint.
- A second cross-instance access mechanism that bypasses the SDK and the sealed endpoint.
- An IR vocabulary item modeling a particular SDK opaque-segment resolver shape (a named-method
  family carrying bind state, a wrapper-typed member kind). The SDK chooses how an opaque segment
  resolves; the IR vocabulary names only segments, route, and endpoint.
- A binding installed in the constructor block. Routes execute in Resolve and seal in Seal; ctor
  allocates the shell only.

## Notes / Examples

**Same-unit sibling reference.** `always_comb from_b = b.bx;` inside generate block `a` of `Top`.
The route has two segments: `a -> Top` (typed; the parent edge whose target class lives in Top's
artifact) and `Top -> b -> bx` (typed; sibling member access plus variable access, both in Top's
artifact). Top's emission produces a typed pointer chain; no SDK call. Resolve produces the
candidate endpoint; Seal commits the variable's cell.

**Cross-unit downward reference.** `always_comb r = c.x;`. The route has two segments: `parent -> c`
(typed; the parent's artifact owns the `c` member's pointer type) and `c -> x` (opaque; `x` lives
inside `c`'s unit). The parent's artifact emits the typed access for the first segment, then an SDK
by-name lookup for the second; the SDK answers from `c`'s registered signals.

**Cross-unit upward reference.** `always_comb x = Top.g;`. The referrer does not instantiate `Top`.
The entire route is opaque: the SDK climbs the runtime tree by canonical instance name to the scope
identified by the head, then performs a by-name signal lookup. The referrer's artifact carries zero
knowledge of Top's body; the route's segments arrive as ordinary MIR primitives in the emitted
resolve code, not as type payload -- `backend_contract.md` keeps render mechanical.

**Mixed route.** `top.gen.child.x` from outside `top`: the first two segments (`top -> gen`,
`gen -> child`) are typed because `top`, `gen`, and `gen.child` (a member of `gen`'s class) all live
in `top`'s artifact. The final segment (`child -> x`) is opaque because `x` lives in `child`'s unit.
The route alternates typed and opaque segments cleanly; the sealed endpoint is one access point
regardless.

**Cross-unit package call.** `r = pkg::add_base(23);`, where `pkg` is a package the caller neither
instantiates nor owns. Unlike the references above, this reaches no object and no per-instance cell:
a package has no instance layout, only namespace-level declarations, so its callable is an ordinary
link-time symbol. The caller's artifact renders the direct qualified call and includes the package's
own emitted header; the linker binds the symbol. There is no runtime object-graph traversal and so
no SDK step -- the SDK exists to resolve a reference into another unit's per-instance layout, which
a package does not have. This is the one cross-unit reference realized by a plain linked name rather
than the SDK, and it is sound precisely because there is no instance identity to resolve.

**Cross-unit package variable.** `x = pkg::cnt;` or `pkg::cnt = 7;` (LRM 26.2) is the storage
counterpart of the call. A package variable is one program-global cell, not a per-instance member,
so it too is an ordinary link-time symbol: the caller renders the direct qualified access and
includes the package's header, and the linker binds the one definition. Reading, writing, and waking
on the cell's change all reach that same linked cell directly -- there is no route, no per-instance
endpoint to seal, and no SDK step, again because a package has no instance identity. Its LRM 10.5
initialization is not a per-instance constructor action but two receiver-less callables the design
root invokes during the Initialize phase, before the top modules initialize: one installs every
package's cells (declared type and default) design-wide, then one runs each package's value
initializers -- so a value initializer that reads another package's cell always reaches installed
storage. The LRM leaves the relative order of initializers unspecified; the design root chooses a
stable, best-effort order, and a cell whose dependency is unknown or cyclic reads a default rather
than failing.

Any artifact that aggregates multiple units' bodies into one is forbidden, however a build step
packages the emitted sources: the per-unit artifact boundary and the segment-classification rules
above are the contract every backend must satisfy.
