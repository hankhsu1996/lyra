# Declarations Before Bodies

## Date

2026-06-28

## Status

Accepted

## Why this decision matters

A program admits mutual references between sibling declarations: two subroutines may call each other
(LRM 13.7), two classes may forward-declare each other and refer back in their bodies (LRM 8.24), a
process body in one generate scope may reach a sibling's signal, a hierarchical callable in a class
may dispatch through a base whose own body has not yet lowered. The pattern is universal across
programming-language compilers: a body cannot reference what is not yet declared, so every IR layer
that admits mutual reference within a structural scope must produce all identities before any body
is lowered.

The lowering today honors this for the smallest case -- a structural scope's subroutines reserve
their `MethodId`s before any subroutine body lowers, so a peer call resolves regardless of source
order. Larger cases (cross-scope hierarchical reference, class mutual reference, hierarchical
callable) need the same property at compilation-unit scope. Pinning the invariant once, with a
single mechanism, prevents the codebase from re-deriving it per feature and lets every consumer rely
on it without inspecting how each particular declaration was built.

## The invariant

> Within a compilation unit, every structural declaration's identity and shape is canonical,
> CU-global, and queryable through the compilation unit before any executable lowering begins.

The four properties carry distinct weight:

- **Identity** -- every declaration has a stable id (`ClassId`, `MethodId`, `MemberId`, `TypeId`,
  ...) minted before any body lowers.
- **Shape** -- the structural facts a body may need to read about the declaration (a class's members
  and contained children, a method's signature, a type alias's target, an instance's external-unit
  name) are populated alongside the identity.
- **CU-global** -- the source of truth is the compilation unit's class / type / method registries.
  Cross-declaration queries during body lowering reach declarations through the unit, not through a
  sibling lowerer's private state.
- **Queryable** -- after the declaration pass completes, any consumer can resolve any peer
  declaration's shape by id, with no ordering dependency on which body lowers first.

An IR layer that admits mutual reference must satisfy these four properties. An IR layer that does
not (a strictly linear declaration order, no forward reference) is free to interleave declaration
and body, but neither HIR nor MIR is such a layer.

## Decisions

### D1. Lowering is staged: declarations precede executable bodies

A lowering pass that produces a structural-declaration-bearing IR runs in two stages over the
compilation unit, in this order:

1. **Declare** -- mint every declaration's identity and populate its shape on the compilation unit's
   registries. No expression is lowered, no executable statement is emitted, no block accumulates.
2. **Lower bodies** -- lower every body (subroutine, process, initializer, action, constructor,
   install statement) into blocks attached to the relevant declarations. Cross-declaration
   references resolve through the registries populated in stage 1.

A lowering pass that violates this staging cannot satisfy mutual reference; the staging is not an
optimization.

### D2. Published shape is canonical for cross-declaration query during lowering

A declared shape becomes a canonical query target the moment the declare pass publishes it. Lowerer
objects, builder objects, and walk-time stack frames do not own published shape; they may hold local
state while constructing one shape value, but the queryable home is a registry whose contract is
"every id resolves to a complete shape". A consumer that wants to read peer shape during body
lowering reaches that registry by id, never another lowerer's private state.

The publication store's lifetime is the lowering pass. Once the pass returns its finished
compilation unit, the publication store is destroyed; the compilation unit holds the sole
authoritative class store thereafter (the final `mir::Class` registry). No half-alive,
partially-consumed shape registry survives into the finished unit.

### D3. Declaration ordering inside a CU is unspecified

The declare stage may visit declarations in any order that respects each declaration's own
dependencies (a member typed by an internal object type requires that object type's id minted
first). The lowering pipeline picks an order that satisfies these intra-declaration dependencies;
the order is an implementation matter and is not observable to body lowering, which reads
declarations by id without seeing how they were ordered. Source order does not classify and does not
select mechanism.

### D4. Reservation and population are one step per declaration

A declaration's identity and its shape are minted as one step. The registry does not expose a
half-state where an id exists but the shape is absent; either the id is in the registry with a
populated shape, or the id has not yet been minted. This is the operational form of "queryable after
declare stage": every minted id resolves to a usable shape.

A declaration may carry a forward-typed reference to another declaration that has not yet been
minted (a class that names another class as a field type). The pass orders the mints to satisfy
those dependencies, or uses a registry primitive that supports forward identity reservation followed
by shape attachment for the cyclic case; either way, the consumer-visible contract is "every
queryable id has a populated shape."

### D5. Executable bodies attach to existing declarations; they never grow new shape

The body-lowering stage may add executable content to existing declarations (a method's body, a
constructor's block, a process's coroutine). It does not mint new structural declarations a peer's
body might reference; if the body needs an identity not already minted, the declaration was missing
from stage 1 and the pipeline is wrong. This keeps body lowering an emission step against a fixed
declaration set.

This does not forbid stage 2 from adding declarations that no body references -- a synthesized
deferred-check site, a per-action capture descriptor -- whose existence is local to body lowering
and whose id is never read cross-declaration.

### D6. The shape store is the sole source of cross-class structural queries during body lowering

During the body-lowering stage, the compilation unit's class registry is a one-way commit sink: each
class is added once when its bodies are complete, and the registry's only use from inside the
lowering pipeline is `Define`, never `Get`. Any cross-class structural query a body lowering needs
(peer member ids, contained-child ids, method signatures, type aliases, name) reads from the shape
store, which holds the full set of class shapes from the moment the declare stage completes.

This invariant forbids a regression where a body lowering reads peer shape via the class registry:
such a read would succeed for classes that happened to finalize earlier and fail for classes that
have not yet finalized, silently reintroducing source-order dependence into what must be
order-independent.

### D7. Anticipated future extension: callable signatures join the shape

When a body lowering needs peer callable identity or signature -- the case for SV class virtual
dispatch and for hierarchical callable resolution -- the callable's signature joins the structural
shape, leaving the callable's body in the body-lowering stage. The expected concrete form is to
split `MethodDecl` into a signature part (name, parameter list, result type, override target -- in
the shape) and a body part (the executable block -- composed at finalize alongside the constructor
block and the other lifecycle blocks).

The split has not happened yet because the current consumer (cross-scope typed member access) needs
only member ids, not method ids. The split lands together with the work that first requires it; no
field on the shape is added speculatively before then.

## Applications

The invariant covers every IR boundary that admits mutual reference within a structural scope. The
following are the known applications; each is one instance of the same staging, not a separate
mechanism.

- **Subroutine forward / mutual reference within a structural scope** (LRM 13.7). Peer subroutine
  call resolves to a `MethodId` minted before any subroutine body lowers. Today's code already
  honors this with `MapStructuralSubroutine` reserving ids in advance.
- **Cross-scope hierarchical reference's typed segments**
  (`../decisions/hierarchical-reference-routing.md`). A sibling reference's typed `MemberAccess`
  chain reaches peer scope members via `MemberId`s minted before any body lowers. The structural
  declaration pass covers every scope in the CU; the body lowering pass reads peer members through
  the unit registry.
- **SV class forward / mutual reference** (LRM 8.24).
  `typedef class Pair; class Edge; Pair p; function ...; endclass class Pair; Edge e; endclass`.
  Both classes' shapes (fields, method signatures) live in the registry before either body lowers. A
  method body referencing the peer class resolves its `ClassId` through the registry.
- **Hierarchical callable dispatch.** A virtual call from a derived class through a base whose body
  has not yet lowered resolves through the base's already-minted method signatures.
- **Interface and modport reference.** An interface's externally visible surface (modport
  declarations, virtual interface targets) is shape; a body that references it reads through the
  registry.
- **Procedural storage scope materialization** (`procedural-storage-scope.md`). A named procedural
  block whose subtree owns hierarchy-addressable storage is a structural declaration: its class
  identity, its statics, the parent's owning-pointer companion member, and the contained-class edge
  are minted by the shape pass. A sibling process's body lowering reaches its peer block's static
  through a head the shape pass already registered; the body never mints any of these.

The same invariant applies to future structural concepts that admit cross-reference; new
applications do not introduce new mechanism.

## Rejected alternatives

- **Lowerer-owned `optional<mir::Class>` carrying the in-progress declaration.** Puts the source of
  truth for declared shape on a sibling lowerer object rather than on a published registry. Peer
  body lowering must thread the lowerer tree to find a peer's shape; cross-scope queries become
  walk-coupled. Race-prone in practice: a query against a peer whose lowerer has not yet populated
  its `optional` fails opaquely, and the lowering tree's traversal order leaks into what a body can
  read. Rejected.

- **Lazy `MemberId` / `ClassId` backpatch.** Bodies emit forward-reference tokens at lowering time;
  a finalize stage resolves them when the target shape eventually appears. Introduces an implicit
  third phase whose ordering is encoded in the resolution machinery; failure timing is opaque (a
  malformed reference does not surface until finalize); MIR sits in a partially-valid state between
  body lowering and finalization; backends inherit a token concept they should not have to know
  about. Rejected.

- **A synthetic endpoint type wrapping each cross-instance reference.** Pushes the cross-scope
  access through a wrapper protocol (`ReadBound(endpoint)`) rather than through ordinary access on
  the target's existing type. Mismatched with the architecture's contract that the endpoint is the
  target itself (`hierarchical-reference-routing.md` D3); the wrapper would also re-introduce
  hierarchy traversal at body-execution time when the target's storage is reached only through the
  wrapper's protocol. Rejected.

- **Source-order declaration with no forward reservation.** Lower each declaration's identity in
  source order, hope a body's references reach already-declared peers. Brittle under generate
  expansion, fails outright on cyclic mutual reference, and reintroduces the "lexical order shapes
  mechanism" failure that the architecture is removing elsewhere. Rejected.

- **Eager `DefineClass` interspersed with the walk.** A single-pass walk that defines each class as
  it is built. A consumer reading a peer class observes whatever subset of the peer's content has
  been built so far; the visible state of a peer depends on where its walk currently sits, which the
  consumer cannot know. Rejected; the registry's visibility contract is "all-or-nothing per stage",
  not "growing as the walk progresses".

- **Splitting the public IR into separate declaration and body artifacts (`ClassDecl` + `ClassBody`,
  joined externally by id).** Pure value-immutability for both halves, but exposes the two-stage
  construction as a permanent property of the IR. Consumers (backend, dump, validators) must pair
  two registries on every class access; methods split into separate `MethodSignature` and method
  body, requiring downstream code to join them. A SystemVerilog module / generate scope / class is
  one semantic entity; the IR keeps it one entity, with the staged construction hidden inside the
  lowering. Rejected.

- **A generic `Registry::Mutate(id) -> T&` primitive, even if only one caller is intended.** Once
  the primitive exists it advertises "registry entries are mutable" at the type level, and a future
  contributor confronting a new two-stage problem reaches for it as an escape hatch. The "staged
  population" need is solved by separate value-immutable artifacts (one per stage), each going
  through the original `Declare` / `Define`-once contract, not by relaxing the primitive. The
  parallel form `Refine(id, fn)` is the same relaxation in different syntax. Rejected.

- **Per-class consume that leaves the publication store partially moved-from while still alive.** At
  finalize time, each class's shape entry is moved into the composed `mir::Class`; the shape store
  still exists with the remaining slots populated and the moved-from slots holding empty values. Any
  later read of a moved-from slot returns an empty shape silently. Type / lifetime do not encode
  "this slot has been consumed". Rejected in favor of whole-store consume: the shape store's
  lifetime is the lowering pass; once the pass returns, the store is destroyed wholesale. Within the
  pass, individual finalizes copy shape into the composed `mir::Class`, leaving the shape store's
  slots intact for peer queries from later finalizes.

## Consequences

- **Shape and final class are two value-immutable artifacts under one identity.** A class's identity
  is one id. Its shape is a value built once during the declare pass and published to a registry
  that exists for the lifetime of the lowering pass; the registry's contract is the original "every
  minted id resolves to a populated value" (one `Declare` + one `Define` per id). Its final form is
  a second value built once during the body pass and posted to the compilation unit's own class
  registry through the same contract. Neither value is mutated after publication; the staged
  construction is purely internal to the lowering pass.

- **The publication store of shapes lives on the lowering pass, not on the compilation unit.** After
  the lowering pass returns its finished compilation unit, the shape store is destroyed. The
  finished unit holds the sole authoritative class store -- the registry of fully-composed
  `mir::Class` values. Downstream consumers (backend, dump, validators) see one class per id, with
  shape and bodies together; they never reach for a separate shape store, and no shape store
  survives in any form they could touch.

- **The composed `mir::Class` copies its shape fields from the published shape at finalize.** The
  shape store stays valid throughout the body pass so peer body lowerings can keep reading it
  through finalize boundaries; each per-class finalize copies the relevant shape into the freshly
  constructed `mir::Class` and adds the body parts. Memory cost is one extra arena per class for the
  shape's lifetime, traded for the clean property that the publication store is never partly
  moved-from while it is still being read.

- **Lowering passes split into two CU-wide stages.** A pass that produces declared-shape-bearing IR
  drives a recursive declare walk first and a recursive body-lowering walk second. Each stage's
  recursion is internal to the pass; the boundary between them is the architecture-visible point. A
  pass's class organization (per `decisions/lowering-organization.md`) is unchanged: dispatcher
  methods, walk-frame discipline, and per-kind handlers remain. The two stages are two entries on
  the pass class, not two pass classes.

- **`StructuralScopeLowerer` carries no in-progress declared shape.** After the declare stage, the
  scope's shape lives on the lowering pass's publication store. The lowerer's per-instance state
  shrinks to its HIR-to-MIR identity maps, the child-lowerer tree it owns for the body stage, and
  whichever local facts the body stage needs that derive from neither registry nor maps. The blocks
  the body stage accumulates (constructor, resolve, initialize, activate) are body-stage locals; the
  slot-var lists, instance-member-var lists, and generate binding tables an earlier draft of this
  refactor cached as parallel state are derivable from the publication store plus the existing
  identity maps and are not stored separately.

- **Cross-class shape query is one path.** Peer body lowering reads peer shape through the
  publication store's `GetClassShape(id)`. There is no parallel query route through the lowerer
  tree; the publication store is the single source of truth for shape during the body pass.

- **Acceptance for an architectural refactor under this contract is semantic equivalence, not
  byte-for-byte MIR identity.** Declaration ordering within the unit is unspecified; consumers must
  read by id, not by position. A refactor that changes declaration order while preserving the
  semantic outcome is conforming.

## Cross-references

- `../architecture/lowering_organization.md` -- the pass-class shape (dispatcher, walk frame,
  registries) the staged lowering builds on; this decision adds the staging discipline, not a new
  pass class shape.
- `../architecture/mir.md` -- the IR layer whose registries house the declared shape and the
  executable bodies.
- `lowering-organization.md` -- the prior decision establishing the pass-class shape.
- `hierarchical-reference-routing.md` -- one application: cross-scope typed segments read peer
  members through the staged registry.
- `binding-graph-resolution.md` -- the resolution-ordering decision the routing builds on;
  orthogonal to this decision but cited by the same lowering pipeline.
- `object-model-storage.md` -- the class-declaration registry that is the home of declared object
  shape.
