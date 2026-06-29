# Net Resolution

## Purpose

Define what a SystemVerilog net is and how its value is produced. A net's value is not written; it
is the resolution of a set of independent driver contributions under the net type's resolution
policy. This document owns the driver / contribution / resolution model and the distinction between
a net and a variable. It is the design-global net-resolution concern that `reference_resolution.md`
places outside its own scope.

## Owns

- The rule that a net is a resolved observable value: its value is `resolve(contributions)`, where
  the resolver is fixed by the net type and the contributions are supplied by the net's drivers.
- The decomposition of a net into three parts: a resolved observable value, a set of driver slots,
  and a resolver policy.
- The rule that each driver is an independent contribution with its own identity and provenance. A
  driver updates only its own contribution; it never writes the net's resolved value.
- The rule that the net re-resolves whenever any contribution changes, and publishes a change only
  when the resolved value itself changes.
- The undriven case: with zero drivers a net holds the net type's undriven value (high-impedance for
  the common net types).
- The distinction between a net and a variable as a property carried by type, not a flag: a variable
  owns mutable state and is written directly (a procedural write, or a single continuous driver); a
  net owns a resolver and a driver set and is only ever driven.
- The rule that a driver's contribution carries both a logic value and a drive strength, and that
  resolution consumes both.

## Does Not Own

- The route by which a driver reaches a net across compilation units, and by which a net's value is
  read across units (`reference_resolution.md`).
- The phases in which drivers attach, topology freezes, and contributions seed
  (`elaboration_lifecycle.md`).
- The capability-type family and the observable-cell access protocol that a net's resolved value and
  a driver handle are members of (`mir.md`).
- Waking dependent processes when a net's resolved value changes (`scheduling.md`).
- Merging two or more nets into a single shared simulated net (net collapse), and bidirectional
  (`inout`) net connectivity. Those unify distinct nets into one resolution domain and are a
  cross-net connectivity concern, separate from resolving one net's own drivers.

## Core Invariants

1. A net's value is the resolution of its drivers' contributions under the net type's resolver. With
   zero drivers the value is the net type's undriven value; a single driver and many drivers are the
   N=1 and N>1 cases of the same resolution, with no separate single-driver representation.
2. A driver is an independent contribution with identity and provenance. A driver writes only its
   own contribution and never the net's resolved value directly. The net owns the contribution
   storage; the driver names its contribution by a stable identity, never by a borrowed pointer into
   that storage.
3. The net re-resolves on any contribution change but publishes a change only when the resolved
   value changes. A contribution that moves without changing the resolved value wakes no observer.
4. A net is never the target of a direct or procedural write; it is only driven. This is carried by
   the net being a distinct capability type, not by a classifying flag beside a value type.
5. Resolution is a function of the drivers' current contributions for the wired and tri-state net
   types; only the charge-retaining net type carries resolver state. No resolver depends on anything
   but the current contributions and, for the one stateful type, its own retained value.
6. A single-driver constraint (a net type that admits only one driver) is a property of the attached
   driver set -- the count of drivers with identity -- not of any contribution's current value. A
   driver contributing high-impedance is still a driver.

## Boundary to Adjacent Layers

- `reference_resolution.md` owns the one route from a referrer to a target. A driver attaches to a
  net through that route, and a read of a net reaches the resolved value through that route; net
  resolution is the concern that doc names as design-global and out of its own scope.
- `elaboration_lifecycle.md` owns when. Drivers attach during Resolve; the driver topology is frozen
  and validated at the Seal barrier; contributions are seeded in Initialize; the processes that
  update contributions arm in Activate.
- `mir.md` owns the capability-type family. A net's resolved value is a capability type sibling to
  the plain observable cell (readable and observable, but not directly writable); a driver is a
  capability handle sibling to a reference.
- `scheduling.md` owns the wakeup that fires when a net's resolved value changes.

## Forbidden Shapes

- A net modeled as a plain writable cell that each driver writes directly, so the last write wins.
  Multiple drivers must resolve; a net's value is a projection of its contributions, never the most
  recent write.
- A net's net-ness carried by a flag beside a value type. The capability type carries it.
- One aggregate per-net process that reads every driver's expression and fixes the driver set at the
  net's owning-unit compile time. A net's drivers are not all knowable when its unit compiles -- a
  driver may be a cross-unit port connection authored in another unit -- so drivers attach
  independently, not as a statically gathered list.
- A driver holding a borrowed pointer into the net's contribution storage. The storage is the net's,
  reorganizable at will; a driver addresses its contribution by stable identity.
- A single-driver net represented as a variable, or any path that special-cases the single-driver
  case apart from the N-driver resolution.
- A single-driver constraint validated by inspecting current contribution values rather than
  counting attached drivers.
- A net publishing to its observers because a contribution changed while the resolved value did not.
- Two nets sharing one simulated cell introduced as a reference alias. Net collapse and `inout`
  connectivity are a separate cross-net concern; they are not a special case of single-net
  resolution.

## Notes / Examples

A single-driver wire `assign w = e;` has one contribution; resolving one contribution yields that
contribution's value, so `w` tracks `e`. This is the N=1 case of the general model, not a distinct
shape (LRM 6.5, 6.6.1).

A multiply-driven net `assign w = e1; assign w = e2;` together with a child output connected to `w`
has three contributions; the wire/tri resolver combines them per its truth table, yielding `x` where
they conflict and high-impedance where all contribute high-impedance (LRM Table 6-2). A `wand` /
`wor` net uses the and / or truth table instead (LRM 6.6.3).

A child's input net driven from its parent receives a driver the parent attaches through the route;
the child's net resolves its own drivers. The child cannot enumerate who drives it when it is
compiled, which is why a driver is an independently attached contribution rather than a member of a
list the net's unit builds at compile time (LRM 23.3.3).

The single-driver `uwire` net is the N=1 case constrained so that attaching a second driver is an
error; the constraint is checked against the count of attached drivers, each carrying its
provenance, so the diagnostic can name the conflicting drivers (LRM 6.6.2).
