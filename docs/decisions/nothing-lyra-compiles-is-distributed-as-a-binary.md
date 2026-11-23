# Nothing Lyra compiles is distributed as a binary

Status: Accepted, 2026-11-22.

## Requirement

Reaching a member of an object, calling a method and dispatching a virtual call cost no more than
the same operations in the same C++ program, since simulation time is part of the iteration loop
([`north_star.md`](../architecture/north_star.md) invariant 1). Every unit of a design is compiled
by Lyra itself, from source, and the store finds what an earlier build compiled
([`incremental_build.md`](../architecture/incremental_build.md)); a library a design depends on, a
supported port included, is source, its SystemVerilog and the C its imports call
([`project_model.md`](../architecture/project_model.md)).

## Choice

**What Lyra compiles reaches no consumer except as source compiled by Lyra, so a unit may know
everything another unit's published declaration states**, as Swift's library evolution document lets
code using a non-resilient module "know all the details of how the entity is declared". A published
class promises its storage whole; a class of another unit extending it is laid out, and its dispatch
tables built, when the program is compiled; a referrer reads where a member lies off the promise.

## Alternatives turned down

- **A resilient layout, loading a member's offset or a method's slot at run time**, as Objective-C's
  non-fragile instance variables, Swift's resilient types and the JVM's link-time field offsets do
  so that a library shipped compiled can grow without its clients recompiling. It fails an access
  costing what the same C++ program's does. The failure rests on there being no compiled library to
  protect. What it buys is a saved recompile, which the store already gives where the promise did
  not change.
- **Resolving a member or a dispatch by name when the program links or starts**, as a JVM resolves a
  symbolic reference. It fails the same requirement, and adds a lookup on the simulation path, where
  nothing is looked up by name ([`object_model.md`](../architecture/object_model.md)).

## Consequences across layers

- A change to what a class promises rebuilds every unit that read the promise, which is how every
  dependency is kept ([`incremental_build.md`](../architecture/incremental_build.md)).
- Shipping a compiled design or library -- an encrypted or precompiled IP block, a prebuilt port --
  reopens this choice, and with it every choice resting on it, among them storage stated whole,
  dispatch tables fixed at compile time, a flattened layout free of the fragile base class problem.
- The runtime library is the one compiled artifact Lyra ships, and it is a contract with the
  compiler of the same release only
  ([`runtime_distribution.md`](../architecture/runtime_distribution.md)).
