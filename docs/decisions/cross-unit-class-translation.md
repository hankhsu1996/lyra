# Cross-Unit Class Translation

## Date

2026-07-18

## Status

Accepted. Revised 2026-10-24: the boundary translator yields the unit's one identity for a class of
either unit.

## Why this decision matters

Lyra treats each package, module, and interface as an independent compilation unit. Slang's frontend
elaborates the entire design into a single tree with global symbol identity -- every
`slang::ClassType*` is a global pointer, and compilation-unit boundaries are implicit, visible only
by walking `getParentScope()` chains. AST-to-HIR must reverse slang's global elaboration back into
per-CU HIR structures: a class this unit declares is minted in the unit's class registry, and a
class another unit declares enters that registry under its key, the declaring unit and its name.
This entry pins how the AST-to-HIR class-reference translator is structured, so the tension between
slang's global identity and Lyra's per-CU model resolves the same way at every future call site.

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

- `InternOwnClass(cls)` -- **the top-down builder**. Called only from the pre-pass that walks this
  unit's own scope, and (indirectly, via `ResolveClass`) from a same-unit base link. Mints a
  `ClassId` in the unit's own class registry and populates the shape. **Never asks "which CU?"**
  because the caller's context guarantees local ownership.

- `ResolveClass(cls)` -- **the boundary translator**. Called from every expression site that names a
  class through a slang pointer, and from a class body's recursive base link. Cache lookup first: a
  class the pre-pass already interned resolves in O(1). On a cache miss, walks
  `cls.getParentScope()` up to a `Package` or `InstanceBody` to derive the declaring CU. If the
  declaring CU is this unit (a case the pre-pass did not cover -- for instance, a class nested
  inside a generate block), routes to `InternOwnClass` to mint it locally. If the declaring CU is a
  different unit, enters the class into the registry under its key -- the declaring unit and the
  class's canonical (specialization) name -- which yields the identity every later name of that key
  lands on.

Either way the answer is one identity of the unit's registry, which the class cache stores, so a
second reference to the same class re-uses it without re-walking.

### D2. The walk lives in exactly one function, whose name states its role

`ResolveClass` is the sole location where AST-to-HIR walks the slang parent chain to answer "which
CU declares this class?". This concentrates the slang / Lyra CU-model impedance into one visible
boundary translator: readers understand that walking is the mechanism for crossing the boundary, not
a pattern scattered through the lowering. The pre-pass and the top-down mint path are **structurally
walk-free** -- the function names say so.

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

- Cross-unit-class classification decisions are made in exactly one place (`ResolveClass`).
- The pre-pass mints every own class through the one minting operation, which trusts context and
  never asks which unit a class belongs to.
- The class cache stores the identity of every class reached, so a reference to another unit's class
  walks the slang tree at most once per unit and thereafter is O(1).
- Recursive base-class references from `InternOwnClass` route through `ResolveClass`, correctly
  handling `Derived extends pkg::Base` where the base is external.
- The pre-pass reaches every scope of the unit that can declare a class -- a generate block, and a
  class, both being scopes of the name tree (LRM 23.9) -- so which scope declares a class is settled
  before any body lowers rather than by whichever reference reaches it first. A reference still
  funnels through the same minting operation and finds the class already minted, so the two routes
  converge as before without the order of references deciding anything.

## Relation to existing decisions

- `front-end-semantic-boundary.md` D1 -- "slang owns semantic resolution; Lyra owns translation."
  This entry names the class-side companion to the reference-side translator that decision records:
  both are boundary translators using slang's already-resolved structural facts, never re-deriving
  from degraded information.
- `declarations-before-bodies.md` -- the pre-pass is the CU-level analogue of the
  "mint-all-identities-before-bodies-lower" discipline applied to class declarations.
  `InternOwnClass` is the operation that mints; `ResolveClass` is what body lowerings and
  mutually-recursive base links use to reach peers.
- `object-model-storage.md` -- the class registry this decision populates is the same "one canonical
  registry of local nominal object declarations" that decision owns.
- `hir.md` invariant 2 -- this entry names where AST-to-HIR turns a class's key into the unit's
  identity for it.
- `specialization-identity.md` -- the canonical class name a cross-unit reference names is the
  specialization name that decision owns; the producer (the declaring unit) and the consumer (the
  referring unit's `ResolveClass`) compute the same name from the same bindings with no shared
  table.
