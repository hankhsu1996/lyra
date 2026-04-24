# Incremental Build

## Purpose

Define how incremental compilation works at the architectural level: what is cached, how identity is
defined across sessions, and how reuse is determined.

## Owns

- The query model: compilation expressed as a graph of memoized queries.
- The definition of stable keys that identify what each query computes.
- The definition of fingerprints that detect semantic change.
- The granularity at which incremental reuse is tracked.
- The rule that invalidation is bounded by ownership.

## Does Not Own

- Specific query implementations or on-disk storage formats.
- Build-tool integration (how the scheduler dispatches queries, what executable drives them).
- Runtime behavior or simulation outcomes.
- The shape of compilation units or IR layers (covered by other architecture docs).

## Core Invariants

1. **Incremental compilation is query-based.** Compilation is expressed as a directed acyclic graph
   of queries. Every query depends only on its explicitly declared inputs; implicit data flow is
   forbidden. Query results are memoized under their stable keys.
2. **Stable keys are ownership-based.** A query key is derived from ownership (compilation unit,
   callable, type, etc.). Keys do not depend on traversal order or insertion position.
3. **Fingerprints capture semantic meaning, not syntax spelling.** A fingerprint excludes
   non-semantic details such as identifier names, source spans, and source positions. Renaming a
   variable does not change the fingerprint if the semantics are unchanged.
4. **Identity has two roles: matching and semantic equivalence.** A key answers "what is being
   computed" and matches a current entity to its previous self. A fingerprint answers "has the
   meaning changed" and decides whether the cached result can be reused. The two are distinct values
   and must not collapse into one.
5. **Incremental reuse is bounded by ownership.** A change affects only the owning scope and its
   transitive dependents declared in the query graph. Unrelated callables and compilation units are
   not invalidated.
6. **No global identity coupling.** No design-global id space participates in incremental keys or
   fingerprints.
7. **Tracked granularity is coarse, not per-expression.** Reuse is tracked at, at minimum,
   compilation-unit, specialization, and callable or process level. Expression-level incrementality
   is not required.

## Boundary to Adjacent Layers

- `identity_and_ownership.md` establishes that identity follows ownership, not traversal order. This
  doc builds on that rule as the foundation of stable keys; it does not restate it.
- `compilation_unit_model.md` defines the compilation-unit ownership boundary. This doc uses that
  boundary as the coarsest granularity of incremental reuse.
- `specialization_model.md` defines specialization keys. Specialization keys feed the cache this doc
  describes; this doc does not redefine them.
- Each IR layer defines its own identity kinds. This doc uses those identities as key sources; it
  does not redefine them.

## Forbidden Shapes

- Using raw pointer identity as part of a key.
- Using global sequential ids as stable keys.
- Fingerprints derived from raw source text rather than from semantic content.
- Dependencies between queries that are not captured in the query graph.
- Recomputing entire compilation units in response to a local edit inside one callable.
- A key scheme that shifts when unrelated entities are inserted, removed, or reordered.
- Collapsing the key and the fingerprint into a single value, such that semantic equivalence cannot
  be detected independently of structural identity.

## Notes / Examples

Rename example. A variable is renamed inside one callable of one compilation unit:

- The callable's key is unchanged, because ownership is unchanged.
- The callable's fingerprint is unchanged, because the semantics are unchanged; only the name
  differs.
- The cached result for that callable is reused.
- Every other callable in the unit and every other compilation unit are unaffected.

If the rename caused cascading recomputation, the key or fingerprint scheme depends on non-semantic
details and violates this contract.
