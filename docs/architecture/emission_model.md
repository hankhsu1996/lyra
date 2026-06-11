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
- The contract that a cross-unit reference is realized at construction through the SDK / object
  graph, and that the referrer's emitted artifact never embeds another unit's storage layout and
  never names a unit it does not instantiate.
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
2. **A unit's emission depends only on itself, the interfaces it instantiates, and the SDK.** The
   inputs to emitting unit U are: U's own MIR; the interface (name, parameters, ports) of each unit
   U instantiates; and the runtime SDK. U's artifact never depends on a unit it does not
   instantiate, and never on another unit's body or internal layout (`compilation_unit_model.md` inv
   8).
3. **The runtime SDK is the link-time-resolution substrate.** Cross-unit operations the referrer
   cannot resolve from its own inputs are expressed as SDK operations. In the C++ backend the SDK is
   the runtime library; under LLVM its operations become intrinsics the linker resolves. A backend
   never invents a second cross-unit mechanism outside the SDK.
4. **A cross-unit reference resolves once into a stored direct reference.** It resolves once -- in
   the constructor for downward, at Bind for upward -- after which the simulation-time read and
   change observation read it directly with no per-access lookup (`reference_resolution.md` inv 3,
   5). How the reference is stored and filled differs by direction.
5. **Storage and fill by direction:**
   - **Downward** -- the target lives inside a unit the referrer instantiates, a declared
     dependency. The referrer holds a slot it fills in its constructor by navigating its owned child
     to the referenced member. Because the sub-module is a declared dependency, the navigation is
     bound when the artifacts are linked; the referrer emits the access by name and the link
     resolves it, carrying no offset.
   - **Upward** -- the ancestor is one the referrer does **not** instantiate, so it is not a
     dependency and is unknown to the referrer at compile time. The referrer holds the reference as
     an ordinary member whose type marks it external; at Bind that member navigates the runtime
     object graph (the parent chain) to the ancestor named by the reference, matching its instance
     name or module definition name (LRM 23.8), then steps down any remaining path through the
     ancestor's owned children by name, and obtains the leaf signal through the SDK. The referrer's
     artifact carries zero knowledge of the ancestor's or those children's units -- it never names
     their types and never includes their artifacts, and no cross-unit slot or side table records
     the upward reference.
6. **A unit exposes its hierarchically reachable signals and owned children through the SDK.** So
   that an upward referrer (which cannot name the ancestor's or an intervening child's type) can
   reach a signal in an ancestor found only at runtime, each unit registers its reachable signals
   and its owned children by name into the object graph node during construction, and the base SDK
   answers a by-name query from those registrations -- the unit never inspects who asks, and the
   dispatch is one generic scan, not a per-unit synthesized branch. The referrer walks that surface
   once at construction and stores the direct reference; it never embeds another unit's layout.

## Boundary to Adjacent Layers

- `compilation_unit_model.md` defines the unit and its interface; this doc defines what a unit's
  _emitted artifact_ may depend on, which is exactly that interface plus the SDK.
- `reference_resolution.md` defines that a cross-unit reference resolves once at construction into a
  stored direct reference; this doc defines how a backend realizes that without breaking unit
  independence (downward link-binding into a slot vs upward runtime navigation through an extern
  member).
- `runtime_distribution.md` owns where the SDK/runtime lives; this doc owns the SDK's role as the
  resolution substrate.
- `runtime_model.md` places the fill in the constructor context and the read in the simulation
  context.

## Forbidden Shapes

- An emitted artifact that contains more than one unit's bodies, or that enumerates all units (a
  global "wiring" file). This is the canonical violation: it serializes otherwise-independent
  compilation and reintroduces an undeclared whole-design dependency.
- A referrer's artifact that names, includes, or casts to the type of a unit it does not instantiate
  -- in particular, an upward reference naming its ancestor's unit type.
- Embedding another unit's storage offset in the referrer's own emitted output to resolve a
  cross-unit reference (as opposed to emitting a by-name access the link resolves).
- A design-global signal or path table that mirrors the object graph. Cross-unit resolution is local
  object-graph navigation (an owned child, or the parent chain), resolved once at construction; a
  per-object by-name signal lookup at construction is local and permitted, a global flat table is
  not.
- A per-access cross-unit lookup on the simulation path; the hot path reads the stored reference.
- A second cross-unit access mechanism that bypasses the SDK and the resolved stored reference.

## Notes / Examples

`always_comb r = c.x;` (downward): `c` is a unit the parent instantiates, so `c`'s interface is a
declared input to the parent's emission and `x` is bound when the artifacts are linked. The parent
emits a by-name access; the link resolves it; the parent's own output carries no offset.

`always_comb x = Top.g;` (upward): `Top` is an ancestor the referrer does not instantiate, so the
referrer knows nothing about `Top` at compile time. It cannot name `Top` or include its artifact.
The referrer holds the reference as an ordinary member whose type marks it external; at Bind that
member climbs the parent chain to the ancestor whose instance or module name matches, obtains the
`g` signal from that node through the SDK, and stores the direct reference. The simulation-time read
and change observation then read that member directly (`reference_resolution.md` inv 5); no
cross-unit slot or side table records the reference.

The current C++ backend collapses all units into a single translation unit (one `main.cpp` that
transitively includes every unit header, with bodies inline). That is a transitional convenience of
the C++ build step, not a relaxation of invariant 1: any artifact that aggregates multiple units'
bodies is still wrong and is marked for removal. The per-unit artifact boundary and the resolution
rules above are the contract the LLVM backend must satisfy and the C++ backend must converge to.
