# Hierarchical Reference Resolution

## Date

2026-06-22

## Status

Accepted

## Why this decision matters

A hierarchical reference names a signal elsewhere on the object tree: down into an owned child
(`c.x`), up into an ancestor (`Top.g`), or absolute from the root (`$root.Top.g`). A reference whose
target lives in another compilation unit cannot be resolved at the referrer's compile time -- the
referrer never sees another unit's internal layout. What had to be settled is how each such
reference reaches its target, once, and where that resolution runs.

## Decision

A cross-unit reference resolves **once** into a stored direct pointer to the leaf cell, read
directly on the simulation hot path with no per-access lookup (`reference_resolution.md`). It
reaches the leaf **by name** through the runtime scope SDK (`GetChild` / `GetSignal`), because the
referrer knows the target's name but not its layout (`emission_model.md`,
`compilation_unit_model.md`). The two directions differ in where the navigation is composed and when
it runs, by necessity:

- **Downward** (`c.x`): the referrer owns the head child, so the path from `self` is known at the
  referrer's compile time. The navigation is emitted as MIR by-name calls and runs in the referrer's
  **constructor**, after the child is built, filling a borrowed-pointer slot member.
- **Upward** (`Top.g`) and **`$root`**: the referrer neither owns nor knows the type of the
  ancestor, and the same artifact sits at many depths, so the path needs the whole tree -- which
  does not exist when the referrer constructs. The reference is carried as a member whose **type**
  is a runtime wrapper, and the climb-and-fetch runs as library behavior at the **bind step**, after
  the whole tree exists (`runtime_model.md`). The climb matches the ancestor by its module
  **definition** name (a module-instance head) or by the scope's **own** name (a named generate
  block, or the root).

This asymmetry -- emitted constructor navigation downward, library climb at bind upward -- is
`emission_model.md`'s "storage and fill by direction." Both share the single by-name SDK and the
resolve-once-into-a-stored-pointer rule (`reference_resolution.md`); neither is a parallel
resolution path.

## Why the cross-unit hop is by name, not a typed accessor

A unit's only cross-boundary surface is its interface -- name plus ports and parameters
(`compilation_unit_model.md`). An internal signal a hierarchical reference targets is body, not
interface. Reaching it by a typed member (`self.c.x`) or a per-signal getter would force the
target's body into its interface: a unit cannot enumerate its referrers, so it would have to expose
every internal, and any internal change would then recompile every referrer -- the opposite of
incremental compilation. A name sidesteps this: the target registers what it owns, the referrer
carries only the name, and neither learns the other's reference set. Under independent compilation
the referrer holds the target only through the generic scope handle, so there is no typed pointer to
call a getter on in any case.

## Why an upward reference is a self-climb

The referrer resolves alone, by climbing its own parent chain; the ancestor is never involved. The
depth is not a compile-time constant -- one artifact sits at many depths -- so it cannot be baked
in. The reference cannot be threaded down from the ancestor, because a unit does not know which
descendants reference it (a referrer list is forbidden, `compilation_unit_model.md`). And the climb
matches by name, never by a typed cast to the ancestor's class, which would name a unit the referrer
does not depend on. So the referrer holds a wrapper-typed member and the runtime climbs at bind.

## Rejected alternatives

- **Typed access across the unit boundary (public members or baked offsets).** Bakes the target's
  layout into the referrer; reordering or adding a signal recompiles every referrer. Breaks
  independent and incremental compilation.
- **A typed per-signal getter the target exposes.** ABI-stable, but a unit cannot know which
  internals are referenced, so it would expose a getter for every signal, promoting its whole body
  into its interface; and under independent compilation the referrer holds only the generic scope
  handle, with no typed pointer to call a getter on.
- **Threading the reference down from the ancestor, or climbing by RTTI / typed cast.** A unit
  cannot know its referrers; a cast names a non-dependency unit. Both contradict compile-per-unit.

## Consequences and scope

- **Intra-unit references are outside this decision and are a known gap.** `reference_resolution.md`
  says a reference whose target lives in the same unit -- including one into a same-unit named
  generate scope -- resolves at compile time as a typed member access plus a compile-time offset.
  The code currently lowers such a reference through the cross-unit by-name path instead. Closing
  that gap needs a MIR member-access form that descends into an owned child object's member; it is
  tracked separately, not decided here.
- **A reference whose head is a named procedural block** has no constructed scope to anchor on (its
  locals live on the enclosing unit), so it is not expressible in this model without first giving
  such blocks a navigable scope.
