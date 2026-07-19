# Cross-Unit Class Translation

## Date

2026-07-18

## Status

Accepted

## Why this decision matters

Lyra treats each package, module, and interface as an independent compilation unit. Slang's frontend
elaborates the entire design into a single tree with global symbol identity -- every
`slang::ClassType*` is a global pointer, and compilation-unit boundaries are implicit, visible only
by walking `getParentScope()` chains. AST-to-HIR must reverse slang's global elaboration back into
per-CU HIR structures: a class this unit declares gets a compiler-owned local id, and a class
another unit declares gets a by-name reference. This entry pins how the AST-to-HIR class-reference
translator is structured, so the tension between slang's global identity and Lyra's per-CU model
resolves the same way at every future call site.

## The tension this addresses

Two forces conflict at AST-to-HIR when a class reference lands:

- **Top-down build discipline** (the codebase's dominant pattern): a lowering pass starts at its
  CU's root scope, walks down, mints identities as it descends. At any point during the walk "which
  CU am I in" is the walk's origin -- never queried.

- **Slang's global elaboration**: hands us `slang::ClassType*` pointers with no direct "declaring
  CU" accessor. To classify a class as local or external, some code must walk the class's parent
  chain until hitting a `Package` or `InstanceBody` symbol, then compare to this unit's own scope.

These conflict at the site where a class-reference expression sees a class pointer for the first
time: the walk-up "who am I from?" is a bottom-up query on slang's tree, not a top-down descent on
our own. Prior to this entry, a single `UnitLowerer::InternClass` function conflated both concerns
-- every intern call ran the parent-chain walk, whether the class was our own (walk redundant,
context already guaranteed local) or external (walk needed). Readers of a call site could not tell
which use case the function was serving, and the pre-pass that walks our own scope re-derived
information the walk-context already carried.

## The decision

### D1. The AST-to-HIR translator splits into two functions with disjoint concerns

- `InternLocalClass(cls)` -- **the top-down builder**. Called only from the pre-pass that walks this
  unit's own scope, and (indirectly, via `ResolveClassRef`) from a same-unit base link. Mints a
  `ClassId` in the unit's own class registry and populates the shape. **Never asks "which CU?"**
  because the caller's context guarantees local ownership.

- `ResolveClassRef(cls)` -- **the boundary translator**. Called from every expression site that
  names a class through a slang pointer, and from a class body's recursive base link. Cache lookup
  first: a class the pre-pass already interned resolves in O(1). On a cache miss, walks
  `cls.getParentScope()` up to a `Package` or `InstanceBody` to derive the declaring CU. If the
  declaring CU is this unit (a case the pre-pass did not cover -- for instance, a class nested
  inside a generate block), routes to `InternLocalClass` to mint it locally. If the declaring CU is
  a different unit, produces an `ExternalClassRef` naming the declaring unit and the class's
  canonical (specialization) name.

The class cache stores the full `slang::ClassType* -> hir::ClassRef` classification, not just local
ids, so a second reference to the same class re-uses the classification without re-walking.

### D2. The walk lives in exactly one function, whose name states its role

`ResolveClassRef` is the sole location where AST-to-HIR walks the slang parent chain to answer
"which CU declares this class?". This concentrates the slang / Lyra CU-model impedance into one
visible boundary translator: readers understand that walking is the mechanism for crossing the
boundary, not a pattern scattered through the lowering. The pre-pass and the top-down mint path are
**structurally walk-free** -- the function names say so. The codebase's top-down build discipline is
now visible in code shape, not just in individual call-site comments.

## Rejected alternatives

- **Single conflated `InternClass` (the prior shape).** One function that walks and either mints or
  classifies as external, distinguished by comparing the walk result to `scope_`. Concerns are
  entangled: the pre-pass runs the walk redundantly for every own class (walk is redundant, we know
  it's local), and readers cannot tell which use case the function is serving at a given call site.
  Rejected in favor of role-named separation.

- **Design-wide precomputed `slang::ClassType* -> CU_Name` map, populated at compilation setup.**
  Moves the walk to a driver-owned pre-pass so `InternClass` is O(1) lookup, walk-free at the
  lowering pass. Rejected because it violates `identity_and_ownership.md` ("A central registry or
  driver-owned map that holds the authoritative relationship between two architectural entities")
  and `compilation_unit_model.md` invariant 8 (units share no identifier space and exchange no
  internal state). Even though the map's key is slang-side, not Lyra identity, it introduces a
  design-wide shared table where independent per-CU compilation was the target. The walk is not
  eliminated -- only relocated -- and the architectural cost is real.

- **Threading source-syntax `pkg::` scope information from parse time**, avoiding the walk by
  carrying the source qualifier as a HIR-visible fact. Rejected as brittle: an unqualified reference
  through `import pkg::*;` has no source qualifier at the reference site, so the walk-up fallback is
  unavoidable anyway. Two mechanisms for one classification job.

## Consequences

- Cross-unit-class classification decisions are made in exactly one place (`ResolveClassRef`).
- The pre-pass (`InternOwnClassDeclarations`) is textually walk-free -- every own class is minted
  via `InternLocalClass`, which trusts context.
- The class cache stores the full classification, not just local ids, so an external reference walks
  the slang tree at most once per unit and thereafter is O(1).
- Recursive base-class references from `InternLocalClass` route through `ResolveClassRef`, correctly
  handling `Derived extends pkg::Base` where the base is external.
- A class textually local to this unit but not caught by the pre-pass (e.g. nested inside a generate
  block) is minted lazily by `ResolveClassRef` at first reference, funnelling through
  `InternLocalClass` -- both routes converge on the same minting operation.

## Relation to existing decisions

- `front-end-semantic-boundary.md` D1 -- "slang owns semantic resolution; Lyra owns translation."
  This entry names the class-side companion to the reference-side translator that decision records:
  both are boundary translators using slang's already-resolved structural facts, never re-deriving
  from degraded information.
- `declarations-before-bodies.md` -- the pre-pass is the CU-level analogue of the
  "mint-all-identities-before-bodies-lower" discipline applied to class declarations.
  `InternLocalClass` is the operation that mints; `ResolveClassRef` is what body lowerings and
  mutually-recursive base links use to reach peers.
- `object-model-storage.md` -- the class registry this decision populates is the same "one canonical
  registry of local nominal object declarations" that decision owns.
- `hir.md` invariant 2 -- this entry names the mechanism by which HIR's Local / External `ClassRef`
  arms are produced at AST-to-HIR.
- `specialization-identity.md` -- the canonical class name a cross-unit reference names is the
  specialization name that decision owns; the producer (the declaring unit) and the consumer (the
  referring unit's `ResolveClassRef`) compute the same name from the same bindings with no shared
  table.
