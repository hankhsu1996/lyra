# MIR

## Purpose

Define the object-oriented semantic IR. MIR expresses the compiled program as objects, members,
callables, and actions, with lightweight structured control flow. MIR is not C++; C++ is one of
several possible backend targets for MIR, and does not define MIR's semantics.

## Owns

- Objects, members, and member access as first-class entities.
- Callables: functions, tasks, constructors, processes, and assertion actions.
- The identity of each object and member within a compilation unit.
- Lightweight structured control flow inside a callable body: `if`, loop, and sequence. No basic
  blocks at this layer.
- Action shapes for constructs that bind behavior to schedule events (always blocks, continuous
  assignments, deferred assertions, concurrent assertions).
- A textual dumper that serializes MIR for inspection, validation, and golden testing. The dumper is
  not a backend; its output is not executable.

## Does Not Own

- Control-flow graphs, basic blocks, branches, or phi nodes.
- Storage placement, offsets, or memory layout.
- Scheduling, dirty tracking, or wakeup filtering.
- The frontend's symbol or string identity. MIR carries only MIR's own ids.

## Core Invariants

1. Every compiled entity is exactly one of: an object, a member of an object, a callable, or an
   action bound to a callable.
2. All hierarchy is expressed as object ownership. A hierarchical reference is navigation on the
   object graph, not a lookup in a side table.
3. Generate is lowered to constructor-time construction logic. A module's constructor builds its
   children via `if`, loop, and sequence over construction actions.
4. MIR ids are the single source of identity at this layer. Earlier-layer ids are not carried
   through.
5. MIR admits a pure textual dumper that is lossless with respect to MIR identity and structure. The
   dumper is the golden-test surface at this layer; it is not a compilation path.
6. MIR does not reconstruct topology or identity from side tables. A member is owned by exactly one
   object; a callable by exactly one owner.

## Boundary to Adjacent Layers

- Consumes HIR. HIR-to-MIR lowering produces MIR objects and members from HIR declarations and MIR
  callables from HIR processes, functions, tasks, and assertions.
- Produces input to LIR. MIR-to-LIR lowering introduces CFG structure, storage placement, and
  execution-oriented details.

## Forbidden Shapes

- Basic blocks, successor lists, or phi nodes in MIR nodes.
- Storage offsets, byte layouts, or alignment data in MIR nodes.
- A secondary hierarchy or identity system that shadows MIR ids (coordinate tuples, ordinals, symbol
  paths used as identity).
- Global semantic lookup used inside MIR. Relationships live on the owning object.
- Side tables that recover topology from keys rather than direct ownership.
- Deferred or concurrent assertions represented as flags or lowering-pass side effects instead of
  explicit callables.

## Notes / Examples

Reviewing MIR via the dumper should feel like reading the compiled program's structure. The C++
backend emits MIR to C++ source: it is a backend output, not a debug view. The object-oriented
semantic model maps naturally to C++ surface syntax, but C++ is not MIR's semantics and more than
one backend may exist.
