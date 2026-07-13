# Member storage as runtime-owned typed storage

Date: 2026-07-09 Status: accepted

## Context

A design unit's construct writes into its own members. A scalar submodule instance is attached to
the runtime tree (`AddOwnedChild`) and, additionally, its borrowed typed handle is stored in a
companion member of the parent -- the layout-visible head a cross-unit reference projects
(`self -> companion -> ...`). This store is part of every construct that instantiates a scalar
submodule, including the synthesized `$root` unit constructing the top-level units.

The C++ backend realizes a member as a native C++ field on its instance subclass; the store is an
ordinary field assignment the C++ compiler lays out. The execution backend has no such subclass. It
allocates a generic instance, and until now that instance carried no member storage, so it could not
run any construct that writes a member -- which is every hierarchical construct, `$root` included.

This is a member-storage question, distinct from the value-realization one
([jit-value-realization](jit-value-realization.md)) already settled: that decision owns how a
_value_ (a `PackedArray`, a `String`) crosses the boundary and who owns its transient lifetime.
Member storage is where an instance's own _places_ live. The two are independent axes.

## Decision

A member is a logical place. A place is a base plus a projection chain of dereference / member /
index / ... steps, named by logical identity; a load, a store, and an address-of each name a place.
A member step carries a class-local member identity, never a physical offset. A unit definition
declares a member storage schema; a generic instance owns one storage object per member, and a
member place resolves to that object's address. This is the member-storage counterpart of the
opaque-handle value realization: the runtime owns the storage, the generated code addresses it
through the ABI, and no physical layout is derived.

- **A member is a logical place, realized per backend.** The C++ backend realizes it as a native
  field; the execution backend realizes it as a runtime-owned storage object. Both are the same
  logical member place -- a base and a member projection -- and only the realization differs. This
  matches the LIR contract that a place names storage by logical identity and its physical layout is
  derived below LIR.

- **The definition describes the storage schema; the instance owns the storage.** The
  per-specialization definition carries, per member, what storage that member needs: a box holding a
  borrowed handle, or an observable cell of a given value domain. A generic instance realizes one
  storage object per descriptor. A backend that lays members out natively leaves the schema empty
  and owns nothing.

- **What a member place means follows its storage kind.** A member whose storage holds a value is
  read and written through the place. A member whose storage _is_ a cell has no value to read out of
  it: the place is only ever addressed, and the library calls that operate on the cell take that
  address. LIR states which of the two a member is, through its type, and rejects a load or a store
  of the latter.

- **Physical in-frame layout is a later optimization.** Deriving native field offsets -- so member
  bytes live in the instance's own storage and value members fold into machine operations -- is the
  `CodegenLayout` maturity path, not a correctness prerequisite. The runtime-owned storage model is
  the baseline, exactly as opaque handles are the baseline for values.

## Rejected alternatives

- **Demand-driven companions (allocate a companion only when a cross-unit reference projects through
  it).** Whether a companion is needed is known to the _referrer_, which is a different unit than
  the parent that allocates the member. Deciding member allocation from another unit's body breaks
  per-unit independent lowering. Even restricted to `$root` -- whose companions are provably dead,
  since nothing references a top through the root's typed handle -- it special-cases the top level,
  which the elaboration model deliberately does not do. A dead companion on `$root` is a separate,
  legitimate later optimization, not the basis for avoiding member storage.

- **Move the typed route head into runtime-owned by-name storage instead of a member.** This would
  re-express the cross-unit route as a runtime lookup rather than a typed layout-visible slot,
  changing the hierarchical-routing model. That model is retained here; the route head stays a typed
  member.

- **Derive physical member layout now.** Making native in-frame layout a precondition turns a
  performance-and-maturity capability into a correctness gate, exactly the framing rejected for
  values. The slot model is already correct and uniform.

## Consequences

- The unit definition gains a member storage schema; a generic instance owns one storage object per
  descriptor, and a member place resolves to that object's address. The C++ backend's native fields
  are unaffected and it leaves the schema empty.
- LIR gains a class member schema and the place vocabulary -- a place is a base plus a projection
  chain, named by load, store, and address-of -- with a member step carrying a class-local member
  identity and a dereference step as the only way to cross a reference. A LIR verifier checks that a
  place's projections type-check, that an address-of yields a reference to its place's type, and
  that no cell is loaded or stored as if it held a value. The execution backend realizes a member
  place as a runtime storage address, the C++ backend from its own fields.
- The execution backend can run any construct that writes a member, so it elaborates hierarchical
  designs and the `$root` unit through the same construct path the C++ backend uses, and it runs
  procedural code over member variables.
- Physical layout remains future work: an observable cell is a runtime object addressed through the
  ABI rather than bytes in the instance's own storage. Reaching native layout does not change this
  baseline.

## Cross-references

- `docs/decisions/jit-value-realization.md` (the independent value-realization axis: opaque handles
  as the baseline, physical layout later)
- `docs/decisions/generated-behavior-boundary.md` (the unit definition; allocation stays a backend
  concern until member storage is unified)
- `docs/decisions/root-unit-elaboration.md` (the `$root` construct whose companion store this
  storage realizes)
- `docs/architecture/lir.md` (a member is a logical place; physical layout is derived below LIR)
