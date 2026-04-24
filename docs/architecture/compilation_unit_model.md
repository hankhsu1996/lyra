# Compilation Unit Model

## Purpose

Define what a compilation unit is, what it owns, and the rules that make it self-contained.

## Owns

- The identity of the compilation unit as the primary semantic boundary of the compiler.
- The enumeration of compilation-unit kinds: module, package, interface.
- The rule that a compilation unit compiles independently, given only its own contents and declared
  interface.
- The shape of instance records: minimal, structural, and free of semantic content that belongs
  inside the unit.

## Does Not Own

- The internal IR shape of any single layer (see `hir.md`, `mir.md`).
- The hierarchy and generate ownership model (see `hierarchy_and_generate.md`).
- Identity rules for declarations and references (see `identity_and_ownership.md`).

## Core Invariants

1. A compilation unit is the primary unit of compilation. Kinds include module, package, and
   interface. No other construct qualifies as a compilation unit.
2. A compilation unit's compile-time artifacts are class-level. They do not depend on any specific
   instance.
3. All semantic information needed to compile a unit is owned by the unit itself. Compilation never
   reaches outside the unit for semantic data.
4. Cross-unit access is only through explicit import or external reference mechanisms. Implicit
   cross-unit access is not allowed.
5. Parameters on a compilation unit are constructor or config inputs. They do not introduce
   per-instance compile-time identity.
6. Instance records carry only per-instance data: wiring, hierarchical position, parameter values,
   and runtime object identity.
7. The fully elaborated instance graph is not the compile-time model. The frontend may produce a
   full elaboration, but the compiler operates on compilation units and specializations. Instance
   expansion does not drive compilation, and compile-time identity does not depend on instance
   enumeration.

## Boundary to Adjacent Layers

- The compiler defines the compilation unit model. The frontend provides source material and
  elaboration hints; it does not define what a compilation unit is or which compilation units exist.
- The runtime constructor consumes compile-time artifacts and per-instance records to build the
  object graph.

## Forbidden Shapes

- Per-instance semantic tables consulted at compile time (variable lists, resolver maps keyed on
  instance identity).
- A compilation unit reaching through a global or design-level lookup to answer a question about its
  own declarations.
- Instance records that carry a copy of the unit's semantic state.
- Maps keyed by `(instance_id, local_symbol)` used to resolve a reference inside the unit.
- Implicit cross-unit access that bypasses explicit import or external reference.
- Treating "the design" as a compilation unit.
- Treating the frontend as the authority for compilation-unit identity, membership, or boundary. The
  frontend is input; the compiler is authority.
- Treating the elaborated instance graph as the compilation model, or allowing instance count to
  drive the number of compilation artifacts.
- Per-instance compilation: forking a compile-time artifact per instance rather than per
  specialization.

## Notes / Examples

If resolving a name inside a compilation unit requires information not reachable from the unit
itself, the compilation-unit boundary has been violated. The fix is to move ownership inward, not to
add another lookup path outward.
