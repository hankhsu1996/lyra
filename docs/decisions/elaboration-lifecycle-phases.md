# Elaboration Is a Staged Phase Protocol, Not Recursive Constructor Execution

## Date

2026-06-24

## Status

Accepted

## Why this decision matters

Bringing up `ref` ports exposed a foundational issue: we had been treating a generated C++
constructor as the executor of SystemVerilog elaboration. A recursive constructor builds a child to
completion -- including the child's variable initializers -- before its parent can wire it, which
makes a whole class of cross-instance orderings either wrong or impossible: a child reading its
`ref` port before the parent binds it, pass-through `ref`, a variable initializer that reads an
upward reference, and the time-zero visibility of an input port's value. These are not separate
bugs; they are one root cause. This record fixes the worldview so future cross-instance features
land in a defined place instead of adding another constructor-time exception. The full contract is
`docs/architecture/elaboration_lifecycle.md`.

## The decision

1. **A generated constructor is an allocation mechanism, not a SystemVerilog semantic executor.** It
   produces an inert instance shell (storage cells, port endpoint placeholders, child table) and
   runs no variable initializer, process body, port read, or reference dereference.

2. **Elaboration is a staged protocol: Build -> Resolve -> Initialize -> Activate.** Build
   constructs the shell graph and records connection facts declaratively (generate is Build-phase
   logic). Resolve realizes every cross-instance reference, in one phase for all directions, into
   stored direct references. Initialize runs variable initializers. Activate registers processes and
   starts simulation. The phases are a contract on ordering, not necessarily four entry points.

3. **Connections are declarative until Resolve, and Initialize runs after Resolve.** A port
   connection or cross-unit reference is a recorded fact after Build and a stored direct reference
   only after Resolve; a variable initializer therefore observes connected and bound values, never
   an unbound reference. This maps onto the language's own boundary: elaboration (connectivity)
   precedes simulation (initialization and processes) (LRM 3.12, LRM 4).

4. **References seal to direct final cells.** At the end of Resolve no reference remains a chain to
   another reference and none is unresolved -- so a pass-through `ref` flattens to the final cell,
   not a reference-to-a-reference.

5. **Parameter strategy is orthogonal.** Baking a parameter into a specialization or flowing it in
   as a constructor input does not change the phase ordering; structural parameters need only be
   available by Build.

## Consequences

- `ref` ports, pass-through `ref`, upward references in initializers, and input/output time-zero
  ordering all land in defined phases instead of depending on C++ constructor return timing.
- `runtime_model.md` (variable initialization and downward-reference resolution "in the
  constructor") and `reference_resolution.md` ("downward in the constructor, upward at bind") are
  refined: those placements move to Initialize and to a single Resolve phase.
- A `ref` port is realized as a one-time alias binding in Resolve, not as a persistent cross-unit
  slot -- removing the slot, the dead pointer, and the storage-classification flag the earlier shape
  required (`reference-as-data-type.md`).

## Alternatives considered

**Recursive constructor as the elaboration executor (the prior implicit model).** Rejected. It
forces a child to be fully constructed -- including its initializers -- before its parent can bind
it, so any incoming binding a child forwards to descendants, or reads at construction, is observed
before it exists. Every fix under this model is another constructor-time special case; the staged
protocol removes the root cause instead.

**Resolving downward references in the constructor and only upward references at a later bind.**
Rejected as the general rule. The split is what creates the interleaving hazard (a downward bind
racing a child's own construction). A single Resolve phase after the whole shell graph exists is
hazard-free and treats all directions uniformly.
