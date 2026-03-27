# The Natural Model

Canonical definition of Lyra's foundational mental model. All other docs reference this; none duplicate it.

For how this model drives design decisions, see [architecture-principles.md](architecture-principles.md). For the compilation data model, see [compilation-model.md](compilation-model.md). For the migration from current state to this model, see [queues/specialization.md](queues/specialization.md).

## Core Definitions

A module is a type. An instance is an object of that type. Signals are its members. Processes are its behavior.

| Term              | Definition                                                            |
| ----------------- | --------------------------------------------------------------------- |
| Module definition | Type definition. Describes shape, members, and behavior.              |
| Specialization    | Compiled type. One per distinct compiled artifact shape.              |
| Instance          | Object. Owns its state. Addressed via object pointer + local offset.  |
| Signal / variable | Member of an instance. Object-local storage.                          |
| Process           | Behavior attached to an instance. Runs against that instance's state. |
| Constructor       | Builds instances from compiled specializations + design topology.     |
| Runtime execution | Processes running on their instances' state.                          |

## Ownership Phases

Three distinct ownership phases. Each produces facts for the next.

**Compile-time facts** (per-specialization, body-shaped):

- Packed layout, arithmetic representation, compiled code shape
- Static member offsets (SpecLayout)
- Process and kernel compiled code
- Body-local slot descriptors, storage specs

These facts describe the type. They are stable across all instances of the same specialization.

**Construction-time facts** (per-instance, resolved from design topology):

- Container sizes (unpacked array element counts)
- Per-instance constant values (value-only parameters)
- Connectivity wiring (which instance ports connect to what)
- Instance paths (hierarchy strings for debugging)

These facts build each instance. They do not change compiled code.

**Runtime-owned state** (mutable during simulation):

- Member values (signals, variables)
- Container contents
- Process suspension state

## Object-Local vs Design-Global

**Object-local** means the primary representation of an instance and its state. This is the core model:

- An instance owns its storage region
- Member access is object pointer + local offset
- Signal identity is scoped to the instance (member N of this object)
- Processes run against their instance's state via object pointer

**Design-global** means outer-layer coordination that operates across instances. This is the orchestration layer:

- Scheduler, time advancement, event queues
- Cross-instance dirty propagation and wakeup
- Connectivity routing between instances
- Design-wide metadata registries (for trace, debug, profiling)

The rule: design-global systems coordinate instances but do not define what an instance is. An instance's state representation, member layout, and access patterns are object-local concerns. The engine may internally use flat arrays or global indices for efficiency, but that is an implementation detail of the outer layer, not the core model.

## What Connectivity Must Not Do

Connectivity (port bindings, continuous assigns, forwarding) describes how instances are wired together. It must not:

- Eliminate an instance's local storage for a member
- Change the layout or shape of an instance's storage region
- Make an instance's member access depend on which other instance it is wired to
- Require compiled body code to use different addressing for wired vs unwired members

Connectivity is linkage between objects. It may be modeled as references, routing tables, or propagation descriptors. It must not redefine what the object owns.

## Regression Checks

When reviewing a change, ask:

- Does this introduce a new design-global coordinate as the primary identity for instance-local state?
- Does this allow connectivity topology to change an instance's storage shape or layout?
- Does this bypass object-local access and reintroduce direct design-global addressing for instance members?
- Does this dissolve instance identity into flat design-global metadata?
- Does this make compiled body code depend on design topology?

Any "yes" is a regression away from the natural model.
