# Hierarchy and Generate

## Purpose

Define the canonical representation of hierarchy and the role of generate. Establish a single model
shared by all consumers: external references, child routing, and construction.

## Owns

- The rule that the canonical hierarchy is an object tree, built at construction time.
- The rule that generate constructs are constructor-time mechanisms that build the object tree.
  Generate is not a separate identity or topology system.
- The rule that a semantically visible generate scope (named, navigable) is represented as a
  first-class constructed object in the tree.
- The contract that every consumer of hierarchy navigates the same object tree.

## Does Not Own

- The IR-layer shape of declarations (see `hir.md`).
- Construction order at runtime. Ownership is static; order is a runtime concern.
- Storage layout for hierarchical state (see `lir.md`).

## Core Invariants

1. The canonical hierarchy is an object tree. Path identity derives from ownership on this tree, not
   from a parallel side table.
2. A compilation unit that has a generate structure contributes that structure as constructor-time
   construction logic. The constructor expands generate via `if`, loop, and sequence to build
   children.
3. A named generate scope is a first-class constructed object. An unnamed generate scope is not
   addressable and is not materialized as an object.
4. External references, runtime child routing, and the runtime constructor all navigate the same
   object tree. No consumer has its own topology representation.
5. The same compilation unit compiled once serves every instance of that unit. Hierarchy does not
   fork the unit's compile-time artifacts.
6. Parameters affect constructor-time construction, not compile-time identity. A parameter value
   changes which children are constructed, how many, and with which sub-parameter bindings, but it
   does not fork the unit's compile-time artifacts or its identity.

## Boundary to Adjacent Layers

- HIR holds the generate tree inside each module compilation unit. MIR lowers generate to
  constructor-time construction logic that builds the object tree.
- The runtime constructor executes this logic to produce the live object graph.

## Forbidden Shapes

- A coordinate or ordinal side system that numbers generate regions and is used as identity.
- A topology representation owned by the driver or a central map instead of by the object tree
  itself.
- Separate hierarchy models for compile-time and runtime. Only one model exists.
- Dual representation (object tree plus a coordinate or ordinal system).
- A child-routing or external-reference path that reconstructs hierarchy from flattened symbol
  names.
- Per-instance generate trees that duplicate the unit's construction logic.
- Parameter values used as part of compile-time identity. Parameters are constructor inputs that
  steer construction, not identity keys that fork compile-time artifacts.

## Notes / Examples

If resolving a hierarchical reference requires a lookup through a table that mirrors the object
tree, the table is redundant: the tree itself is the authority.
