# The Natural Model

> Before editing, see [documentation-guidelines.md](documentation-guidelines.md). Architecture docs describe the target, not history. No "current state," migration plans, or queue references.

Canonical definition of Lyra's foundational mental model. All other docs reference this; none duplicate it.

For how this model drives design decisions, see [architecture-principles.md](architecture-principles.md). For the compilation data model, see [compilation-model.md](compilation-model.md).

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

## Self-Local vs Non-Local Access

Member access within the natural model falls into two categories with fundamentally different resolution timing.

**Self-local access** is a process reading or writing a member of its own instance. This is `this->member` -- object pointer + local offset. The offset is a compile-time constant from SpecLayout. Self-local access is fully resolved during specialization compilation.

**Non-local access** is a process reading or writing a member of a different instance (parent, child, ancestor, or any other instance in the hierarchy). This includes hierarchical references (`child.signal`, `ancestor.value`) and port connection sources/targets.

Non-local access cannot be resolved during specialization compilation because the target instance does not yet exist. The target is created during construction, and its identity (which object, at what address) is a construction-time fact. Specialization compilation must represent non-local access as an **externally-bound handle** -- a typed reference that the body code can use, but that only acquires a concrete target when construction binds it.

The mental model is closure capture: the body code captures a typed external reference slot. The constructor fills it with a concrete binding when the instance and its neighbors are materialized. The body code never names the target directly.

This applies uniformly to all non-local access patterns:

- Hierarchical reads/writes to ancestors or descendants
- Port connection sources and targets
- Cross-instance trigger subscriptions

The distinction is not "compile-time vs runtime." It is "self-local (compile-time resolved) vs non-local (construction-time bound)."

## Generate-Created Structure

Generate constructs create descendants during construction. A parent body may contain generate blocks that conditionally or iteratively create children, but the final set of descendants is a construction-time fact, not a compile-time fact.

Because generate-created descendants are realized during construction, any model that requires specialization compilation to know the final descendant object set is invalid. Compiled body code must not assume a specific number of children, a specific ordering of descendants, or a specific hierarchy shape below it.

The specialization compiles all possible artifacts from all generate branches (see [compilation-model.md](compilation-model.md) "Specialization as Artifact Library"). The constructor selects which subset to install for each instance. The compiled code is indifferent to which selection was made.

This means: non-local access to generate-created descendants must use the same externally-bound handle model as all other non-local access. The body code references a typed handle. The constructor, which knows which descendants actually exist, binds it.

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
