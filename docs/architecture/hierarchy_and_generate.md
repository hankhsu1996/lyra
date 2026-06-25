# Hierarchy and Generate

## Purpose

Define the canonical representation of hierarchy and the role of generate. Establish a single model
shared by all consumers: external references, child routing, and construction.

## Owns

- The rule that the canonical hierarchy is an object tree, built at construction time.
- The rule that the object tree is single-rooted at the implicit `$root` scope, which owns every
  top-level block as a child.
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
   object tree. Typed members on the derived class are the ownership locus; a single non-owning
   generic adjacency list on the base scope, populated by one attachment operation per child, serves
   both deterministic traversal and by-name lookup. No second list of child pointers is maintained
   in parallel, and the parent never restates a child's identity that the child already carries.
5. The same compilation unit compiled once serves every instance of that unit. Hierarchy does not
   fork the unit's compile-time artifacts.
6. Parameters affect constructor-time construction, not compile-time identity. A parameter value
   changes which children are constructed, how many, and with which sub-parameter bindings, but it
   does not fork the unit's compile-time artifacts or its identity.
7. The constructed object tree is faithful to the frontend's elaboration. Every elaborated instance
   and named scope has a corresponding constructed object, and generate-produced objects preserve
   the elaborated index and identity. Reference resolution relies on this faithfulness (see
   `reference_resolution.md`).
8. The object tree has a single implicit root scope, `$root`, that owns every top-level block as a
   child. A design with multiple top-level modules (LRM 3.11) is still one tree rooted at `$root`;
   each elaborated, uninstantiated module is a child of `$root`, not a separate disconnected root.
   Path identity for a top-level block derives from this ownership, the same as for any other node.
   One top is the N = 1 case of the same model, not a special case. Which modules are top-level
   blocks is an elaboration property, distinct from the set of compiled units (see
   `compilation_unit_model.md`): the two coincide only when nothing is instantiated and must not be
   collapsed into one set.
9. Instance multiplicity and generate are orthogonal axes. An array of instances (`c[3]`) is a
   cardinality property of one member's type -- a vector of owned children (see `mir.md`) -- not a
   generate construct. A generate builds named child scopes as constructor-time logic; an array
   replicates one child member. Either may appear without the other, and a generate scope may itself
   contain instance arrays. Neither axis subsumes the other.
10. Scope identity is structural and complete at construction. A child's identity at one hierarchy
    edge is the pair `(parent, segment)`, where the segment is the source-level label plus any
    per-dimension elaborated indices. The child receives that segment from its constructor's caller,
    holds it for its lifetime, and remains the sole authoritative source of its own identity; no
    consumer reconstructs the bracketed name by reverse-searching a parent registry, and the parent
    never re-states an identity the child already carries. `%m`, hierarchical paths, debug output,
    and by-name lookup all read from this single source.

## Boundary to Adjacent Layers

- HIR holds the generate tree inside each module compilation unit. MIR lowers generate to
  constructor-time construction logic that builds the object tree.
- The runtime constructor executes this logic to produce the live object graph.
- `runtime_model.md` defines the constructor / simulation execution-context split that this doc
  builds on. Generate logic is constructor-context; processes are simulation-context.
- `reference_resolution.md` owns how a reference reaches state across the object tree and when it
  resolves. This doc owns the tree's shape and construction and its faithfulness to the frontend's
  elaboration; that doc relies on this faithfulness to make cross-unit resolution total.
- A node whose owned child is built from another compilation unit (an instantiation) crosses the
  unit boundary: the parent references the child's interface by name, never its body or internal
  ids. An owned child that belongs to the same unit (a named generate scope) is intra-unit and
  crosses no boundary. `compilation_unit_model.md` owns the interface and the name-based linkage;
  this doc owns only the resulting ownership edge in the object tree.

## Forbidden Shapes

- A coordinate or ordinal side system that numbers generate regions and is used as identity.
- A topology representation owned by the driver or a central map instead of by the object tree
  itself.
- Separate hierarchy models for compile-time and runtime. Only one model exists.
- Two independently maintained runtime child views that can drift -- e.g., one list for traversal
  plus a separate registry for by-name lookup, each written by a distinct opcode and at a distinct
  construction-time moment. One non-owning adjacency relation serves both, populated by one
  attachment operation per child.
- A child-routing or external-reference path that reconstructs hierarchy from flattened symbol
  names.
- Per-instance generate trees that duplicate the unit's construction logic.
- Treating an instance array as a generate construct, or modeling generate replication as an array
  type. Multiplicity is a property of a member's type; generate is constructor-time scope
  construction. The two axes must not be conflated or made to subsume each other.
- Parameter values used as part of compile-time identity. Parameters are constructor inputs that
  steer construction, not identity keys that fork compile-time artifacts.
- Splitting a child's hierarchy identity between the child and a parent-side registry -- the child
  carrying only an un-indexed label while bracketed indices live in a parent table that consumers
  reverse-search to recover the full name. The child carries its complete `(base, indices)` segment;
  any per-dim decoration is the child's own property, never parent metadata about the child.
- A scope's base constructor side-effecting its own attachment into the parent. Attachment is one
  explicit operation the parent performs once the child is fully constructed and its typed owner has
  committed, so a partially-built child is never visible in the parent's adjacency relation.

## Notes / Examples

If resolving a hierarchical reference requires a lookup through a table that mirrors the object
tree, the table is redundant: the tree itself is the authority.
