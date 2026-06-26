# Object model storage: a unit-wide nominal-object registry

Date: 2026-06-25 Status: accepted (one mechanism detail open, noted below)

## Why this decision matters

`../architecture/object_model.md` makes a module, a generate scope, and a SystemVerilog class one
generic nominal object type. That contract says nothing about **where** those declarations are
stored or **how** one is identified. The current implementation stores them as a **lexical tree** --
a top module object owning nested generate-scope objects, resolved by walking the enclosing chain.
That fits owned, lexically nested generate scopes, but it cannot carry SystemVerilog classes: unit-
or package-level types that reference one another, forward-declare (LRM 8.27), and are reached from
anywhere through a handle, with no lexical parent. This decision settles the storage and identity
model so one model serves every object type, and records why a single registry -- not two storage
systems, not a second identity -- is the answer.

A code-level check sharpened the problem: a local object type has no canonical per-declaration
identity today. Object types are synthesized by name, and the type arena does not deduplicate, so
one declaration is reached through several content-equal type entries tied together only by a name
string -- there is no id that resolves to the declaration on its own. This suffices for modules and
generate scopes, which are reached by hierarchical navigation and backend name nesting and never by
resolving an object-type identity; it cannot carry classes, which are named by identity from
arbitrary positions. So the gap is twofold: there is no canonical identity, and the **declaration
storage and lookup model** is lexical-tree-only.

## Decision

A compilation unit owns **one canonical registry of its local nominal object declarations**. Three
relations the lexical-tree model conflated are kept separate:

- **Canonical identity.** The registry owns every object declaration exactly once and gives each one
  canonical local identity. There is exactly one local nominal identity per declaration, and a
  reference -- a base, a field's object type, a receiver, a constructed type -- names that identity.
- **Lexical name resolution.** A separate lexical scope graph resolves a source name to a registry
  identity (a unit scope, a module scope, a package scope). The registry is not a flat global name
  table: "which declaration does `A` name here" is a lexical-scope question, answered against the
  enclosing scope, not a registry question.
- **Structural containment and backend emission nesting.** Which object builds or owns which (the
  runtime object tree) and how a backend nests its emitted code are separate relations over registry
  identities; neither is the identity. The current nested C++ class emission is preserved by
  deriving it from the structural relation, not by storing an emission parent on the declaration.

Incomplete (forward) declarations are first-class: a registry record exists before its body, in a
declared-but-undefined state, so mutually-referential and forward-declared types (LRM 8.27) resolve
against a stable identity. A parameterized class is a generic declaration plus, per specialization,
a distinct materialized object record in the registry, its identity aligned with the
compilation-unit specialization model (a stable id within a unit, a canonical public key across
units) -- the same "generic declaration plus materialized specializations" shape a parameterized
module uses.

**One mechanism detail is open.** Whether the canonical local identity is the object type's existing
type-system id reused directly, or a dedicated object id the object type carries, is pending. The
**principle** -- one canonical identity, no second inter-convertible id -- is settled; the mechanism
turns on whether an object declaration has a single canonical type-system id today (it may have
several content-equal ones, since type interning does not deduplicate). The lean is the dedicated
id, because the object type's payload is already moving off a name to a direct nominal reference,
and a dedicated id absorbs those content-equal type-system ids cleanly.

## Rejected alternatives

- **Two declaration-storage systems** (a lexical tree for module / generate-scope objects, a flat
  table for classes). Every consumer -- reference resolution, base, override, construction, dispatch
  -- would branch on which storage holds the target, and every later feature (nested classes,
  interface classes, specializations) widens the split. It contradicts the single generic object
  model.
- **The lexical tree as canonical storage and identity.** It cannot express a class with no lexical
  parent, a mutual reference, or a forward declaration, because resolution is a walk from a lexical
  position. Identity must not be a lexical position.
- **A second local identity inter-convertible with the existing one.** Dual identity forces every
  consumer (base reference, specialization key, dump, incremental rebuild) to choose which is
  canonical; the duplication has no value. (This is why the open mechanism above resolves to a
  single identity either way, never both.)
- **An emission-nesting parent stored on the generic object declaration.** Emission nesting is a C++
  backend layout decision a future LLVM backend does not need; it is derived from the structural
  relation, not a field on the declaration.

## Consequences

- The lexical-tree class storage is replaced by a unit-wide registry; generate-scope objects become
  registry records, with structural containment retained as a relation for construction and for
  nested emission. This lands as its own slice before SystemVerilog classes (see
  `../progress/object-model.md`); it is not pulled ahead of the lifecycle-override and construction
  cuts, and the existing non-registry storage is not flattened as a standalone change (a flatten
  would alter emitted C++ nesting for no behavioral gain).
- `object_model.md` invariant 10 states the registry and the three-relation separation as contract.
- Cross-unit references stay by public name (unit independence); the registry is unit-local.

## Cross-references

- `../architecture/object_model.md` -- the object-model contract; invariant 10 is this decision's
  contract form.
- `../architecture/identity_and_ownership.md` -- identity is self-sufficient for resolution and not
  inferred from position or name; the registry is that self-sufficiency for object declarations.
- `../architecture/reference_resolution.md`, `../architecture/compilation_unit_model.md` --
  cross-unit by name; the registry is unit-local.
- `specialization-identity.md` -- the canonical-key model a parameterized class's specializations
  align with.
- `object-model.md` -- the managed-handle and receiver decisions in the same object model.
