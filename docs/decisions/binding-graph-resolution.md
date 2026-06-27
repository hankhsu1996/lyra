# Resolution Dependency Ordering

## Date

2026-06-27

## Status

Accepted

## Why this decision matters

Every cross-instance reference -- a hierarchical reference, a `ref` port alias, a port connection's
reactive endpoint -- needs a resolution that respects dependencies between references. A reference
whose route requires another's sealed endpoint must resolve after that reference seals; a chain of
forwarding aliases must collapse end-to-end before any of them is read; a route that traverses a
non-constructed runtime object must surface a user diagnostic. This entry settles the commitment
that resolution respects these dependencies and the criteria a resolution mechanism must satisfy. It
does not prescribe a particular mechanism today.

## The commitment

Resolution and sealing respect dependencies between references. Specifically:

- A reference whose route execution requires another reference's sealed endpoint resolves only after
  that other reference seals.
- A forwarding chain (the prototype is a `ref` port chain) collapses end-to-end at sealing; no
  reference observable to Initialize is a chain to another reference.
- A reference whose route traverses a non-constructed runtime object surfaces a user diagnostic at
  the sealing barrier, not `InternalError` and not a runtime fallback.
- A cycle in the dependency relation with no concrete storage root is a user diagnostic; a cycle
  that the language semantics forbid but the lowering should have rejected is an `InternalError`.

These commitments are the contract Resolve and Seal owe Initialize. The architecture states the
commitment; this entry records it as a decision so a downstream change does not weaken it under the
rationale that "no specific mechanism requires this."

## Mechanism is not fixed here

How resolution orders references against the dependency relation is not fixed by this decision. A
correctness-equivalent set of choices includes:

- **Implicit ordering from the elaboration walk.** The top-down walk over the runtime object tree
  satisfies the common dependency cases (a parent's `ref` port seals before its child reads the
  alias; a sibling's reference reads only sealed peers). Today's runtime resolution mechanism
  realizes ordering this way.
- **Explicit dependency graph with topological resolution.** A general mechanism that records each
  reference's `requires` set and executes them in topological order. Required if future endpoint
  families (class-handle-reachable bindings whose handles are themselves bindings) add dependencies
  the walk does not respect by accident.
- **Iterative resolution with progress detection.** A general mechanism that retries until no
  reference advances; cycles are detected by lack of progress.

The choice between these is an implementation matter. It does not change the commitment above and
does not change the architecture invariants.

## Criteria a mechanism must satisfy

Any resolution mechanism must:

1. Execute each route at most once during Resolve (modulo retry in iterative variants for
   forward-progress purposes only).
2. Produce a sealed endpoint for every successfully resolved reference at the sealing barrier,
   atomically with respect to Initialize.
3. Surface a user diagnostic for the failure modes above; never silently substitute a value.
4. Respect the dependency relation: a reference whose route requires another's sealed endpoint
   resolves only after that other reference seals.
5. Bound its work: a forwarding chain of length `N` resolves in `O(N)` work in the chain length, not
   `O(N^2)`; a non-forwarding reference resolves in `O(1)` work in the design's reference count.

## Rejected alternatives

- **Mechanism dispatched on lexical form.** The current code reaches this through
  `slang::isUpward()`: a forward-sibling reference (slang-upward) defers resolution to a runtime
  wrapper, while a backward-sibling reference (slang-downward) resolves at constructor time. This is
  a runtime-ordering escape hatch presented as a semantic distinction; it does not respect the
  dependency relation in the general case, and it forces the IR to carry per-form vocabulary that
  should not exist (`hierarchical-reference-routing.md` D1, forbidden shapes). Rejected.

- **Resolution interleaved with construction.** Resolving a reference inside the constructor that
  builds the subtree forces a child to be fully constructed before its parent can bind it, which is
  the pass-through `ref` hazard and the construction-time-read hazard (`elaboration_lifecycle.md`
  Forbidden Shapes). Resolution happens after Build, not within it.

- **Resolution and sealing collapsed into one phase.** Loses the contract barrier Initialize needs.
  Initialize must observe a sealed world or be deferred; a "Resolve that does both" muddles the
  failure modes (an unresolved route and a forwarding cycle become indistinguishable from
  Initialize's perspective).

- **Per-scope sealing.** A forwarding chain may cross scope boundaries; per-scope sealing cannot
  model cross-scope dependencies. Seal is a property of the elaborated design as a whole.

## Consequences

- `elaboration_lifecycle.md` invariant 4 ("Resolve respects dependency order") and invariant 5
  ("Seal is a global barrier") state the commitment as contract.
- The runtime engine exposes Resolve and Seal as elaboration steps the coordinator orchestrates; no
  scope carries a `Seal` method. The mechanism behind Resolve is unspecified by the architecture and
  may evolve as endpoint families grow.
- A change that adds a new endpoint category whose dependencies the current mechanism does not
  respect must either extend the mechanism or land its own decision entry justifying a new resolver.

## Cross-references

- `../architecture/elaboration_lifecycle.md` -- the lifecycle this commitment lives inside.
- `../architecture/reference_resolution.md` -- the references whose routes this orders.
- `hierarchical-reference-routing.md` -- the routing contract this resolution serves.
