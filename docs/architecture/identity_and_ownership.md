# Identity and Ownership

## Purpose

Define the rules for identity and ownership across the compiler. Identity is how one entity
references another; ownership is which entity holds a given piece of state.

## Owns

- The rule that each compilation-unit-local entity has a typed, compilation-unit-local id.
- The rule that cross-unit or global references use explicit, named variants distinct from local
  ids.
- The rule that architecture-bearing state is owned by the entity it belongs to, not recovered from
  a side table.

## Does Not Own

- The specific id kinds per IR layer (see `hir.md`, `mir.md`).
- The hierarchy and generate ownership model (see `hierarchy_and_generate.md`).

## Core Invariants

1. Every compilation-unit-local declaration carries a typed id. References inside the unit use that
   typed id.
2. References that leave the compilation unit are a distinct kind with their own variant. Local and
   cross-unit key spaces never overlap.
3. Ownership is direct. An entity that logically owns another holds it as a typed field or member,
   not as an entry in a global map.
4. Identity is not inferred from position, name, or coordinate. Position and name are rendering
   properties; identity is the id assigned at construction.
5. Side tables exist only as caches of directly owned state. A cache never becomes the authoritative
   source.
6. Identity is sufficient for resolution within its scope. Given an id, a consumer resolves the
   target without consulting a separate registry or lookup table. If resolution requires an id plus
   a side table, the identity is incomplete.
7. Identity follows ownership, not traversal order. Semantic identity is scoped by ownership
   (compilation unit, callable, type, etc.). It does not depend on traversal order, enumeration
   order, or insertion position. Inserting, removing, or reordering one entity does not shift the
   identity of unrelated entities.

## Boundary to Adjacent Layers

- Each IR layer defines its own id kinds and ownership edges. This doc constrains the shape of those
  definitions but does not enumerate them.
- The runtime uses its own object-level identity (object pointers, handles). Compile-time ids and
  runtime ids do not alias.

## Forbidden Shapes

- `SymbolId` or any frontend-global id used as the identity of a compilation-unit-local entity.
- A resolver keyed on `(instance_id, local_symbol)` at compile time.
- A central registry or driver-owned map that holds the authoritative relationship between two
  architectural entities.
- A coordinate, ordinal, or path-string used as semantic identity.
- Mixing identity domains: a local id and a global id occupying the same key space or being treated
  as interchangeable.
- A compilation unit reaching out to a design-level or external table to answer a question about its
  own declarations.
- Duplicate ownership: the same relationship represented in two authoritative places.
- The "id + registry" pattern: an id that cannot be resolved without a separate lookup table.
  Identity must be self-sufficient for resolution within its scope.
- Global, compilation-unit-wide sequential id spaces used as semantic identity.
- An identity scheme where inserting, removing, or reordering one node shifts the identity of
  unrelated nodes.
- Identity derived from traversal order rather than ownership structure.

## Notes / Examples

If a bug is fixed by adding a new lookup through a shared map, the ownership is probably wrong. The
correct fix is usually to move the data onto the entity that needs it, so the lookup disappears.
