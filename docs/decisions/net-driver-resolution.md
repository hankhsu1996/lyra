# A Net Is a Resolution Node with Independently Attached Driver Contributions

## Date

2026-06-29

## Status

Accepted

## Why this decision matters

A SystemVerilog net (`wire`, `tri`, `wand`, `wor`, ...) is not a variable that happens to be written
by a continuous assignment. Its value is the resolution of zero or more independent drivers under
the net type's resolution function (LRM 6.5, 6.6). The tempting shortcut -- model a net as an
observable cell that a driver writes, and worry about multiple drivers later -- produces a net that
is indistinguishable from a variable and has to be reshaped the moment a second driver, a drive
strength, or a cross-unit port driver appears. This record fixes the net model so that the
multi-driver case is the general one and single-driver is its N=1 degenerate, and so that the model
lands on the machinery the codebase already has rather than inventing a parallel one. It binds the
net-port work, the continuous-assignment lowering for net targets, and the eventual strength /
wired-logic / charge net types.

## Findings that shaped the design

### F1. The codebase models no driver or resolution today

A net declaration is dropped at AST-to-HIR; a continuous assignment to a net is rejected; a net port
connection is rejected. Multiple writers to one observable cell are last-write-wins with no
resolution, and drive strength on a continuous assignment is rejected. There is no driver,
contribution, resolution, or resolved-node concept anywhere. The resolution machinery is genuinely
new; everything around it already exists.

### F2. A net's drivers are not knowable at the net's owning-unit compile time

Under compile-per-unit / elaborate-at-runtime (`north_star.md`), a child net is driven by a port
connection authored in the parent, with a count and connected expressions that depend on the
parent's runtime elaboration (generate, instance arrays). The child cannot enumerate its drivers
when it compiles. So a net's driver set is a runtime topology assembled by attachment, not a
compile-time list the net's unit gathers.

### F3. Runtime attachment along the reference route already exists

A `ref` port already, during the Resolve phase, navigates the owned child by name along the
cross-unit route and binds a handle (a reference) into the child. Attaching a driver to a net is the
same shape: navigate the route, attach a handle. The cross-unit route, the Resolve-phase attach, the
per-connection update process, and the observable cell that publishes only on a real change
(`mir.md`, `reference_resolution.md`, `elaboration_lifecycle.md`) are all reused.

### F4. The capability-type family is where a resolved net belongs

Storage capability is carried by distinct MIR types, each with its own access protocol (the plain
observable cell, the reference, the upward reference, the managed handle). A net's resolved value is
the next member of that family -- readable and observable but not directly writable -- and a driver
is a capability handle sibling to a reference. This satisfies "the type is the sole carrier of
classification" (`mir.md`) by construction; no net flag is needed.

### F5. The Seal barrier is a lifecycle contract that the runtime had not realized

`elaboration_lifecycle.md` defines a Seal barrier between Resolve and Initialize: topology is frozen
and validated there. The runtime materialized Build / Resolve / Initialize / Activate but folded
Seal away. A net's single-driver constraint (`uwire`) is only meaningful once every driver across
the design has attached, which is exactly what a global Seal barrier provides.

## The decision

1. **A net is a resolution node**: a resolved observable value, a set of driver slots owned by the
   node, and a resolver fixed by the net type. The node is a distinct capability type sibling to the
   plain observable cell -- readable and observable, never directly written.

2. **A driver is a capability handle, not a slot pointer.** The node owns the slot storage and
   lifetime; a driver names exactly one slot by a stable identity. The handle is owned by the
   driver's source (for a port edge, the parent instance's connection state); the slot is owned by
   the target net. Updating a contribution goes only through the handle's update operation;
   generated code never touches a raw slot.

3. **All drivers attach during Resolve.** Build records driver descriptors as declarative facts; the
   attach itself -- local or cross-unit -- happens in Resolve. A local driver attaches to a node in
   its own unit; a cross-unit driver navigates the route first. The only difference between them is
   whether Resolve traverses a route, never which phase attaches.

4. **The Seal barrier is materialized** as a design-wide phase between Resolve and Initialize.
   Topology is immutable after Seal. The single-driver (`uwire`) constraint and unresolved-endpoint
   validation run there, against the count of attached drivers (a driver contributing high-impedance
   is still a driver), reporting each driver's provenance. Seal is the future home of forwarding
   collapse and net-collapse canonicalization.

5. **Drivers begin at high-impedance, are seeded in Initialize, and arm in Activate.** Each slot
   starts at the undriven value at attach; each continuous-assignment driver evaluates once in
   Initialize and updates its contribution, so the net's first resolved value is correct before any
   observer arms; the update processes arm in Activate. This makes N=0, N=1, and N>1 one mechanism.

6. **Resolution is inline and atomic; publication and scheduling are unchanged.** A contribution
   update mutates its slot, re-resolves the node, and publishes only if the resolved value changed
   -- reusing the observable cell's existing publish-on-change. Waking dependent processes stays
   with the scheduler. Future per-driver delay is scheduling of the contribution update, never a
   second scheduled resolution step.

7. **A net and a variable stay distinct types.** They share the observable read / subscription
   protocol, the cross-unit route, the scheduler, and source-expression evaluation; they differ in
   the write capability -- a variable is written directly (`Set`), a net is driven (attach a driver,
   update a contribution). They are not unified into one "driven cell": procedural writability and
   multi-driver resolution are orthogonal axes, and a variable is never multiply driven.

8. **A contribution carries a logic value and a drive-strength pair.** Strength is a property of a
   contribution, not of the net value. Pull and supply nets (`tri0` / `tri1` / `supply0` /
   `supply1`) are built-in, never-detached drivers attached at Resolve, preserving one uniform
   driver algebra rather than resolver special cases.

## Consequences

- Single-driver `wire` / `tri` is the N=1 case of one model; adding multi-driver resolution, wired
  logic (`wand` / `wor`), strength, pull / supply, and charge storage (`trireg`) is additive (more
  resolver policies, a richer contribution, a built-in driver), never a reshape.
- Cross-unit drivers attach at runtime, so a module stays independently compilable; the net's value
  depends on a topology assembled at elaboration, not on its own unit's compile-time view.
- The runtime gains a real Seal phase, which also gives forwarding collapse and future net-collapse
  a home and establishes the "topology immutable after Seal" invariant.
- A net is never accidentally writable as storage, and a variable is never accidentally a driver
  sink, because the capability is in the type.

## Alternatives considered

**One aggregate per-net process that reads all driver expressions and writes the net.** Rejected. It
fixes the driver set at the net's owning-unit compile time, which F2 shows is impossible for
cross-unit drivers; and it gives no per-driver identity, so per-driver delay, strength, detach, and
`uwire` provenance have nowhere to live.

**Model a single-driver net as a variable now, generalize later.** Rejected. It makes a net
indistinguishable from a variable and forces a reshape at the first second driver, strength, or port
driver -- the exact outcome this record exists to prevent.

**Unify variable and net into one "driven cell with a resolution policy".** Rejected. Procedural
writability and multi-driver resolution are orthogonal; a variable is never multiply driven and a
net is never procedurally written. They share a lower protocol (observable read, route, scheduler)
without being one semantic object.

**Let a driver hold a raw pointer to its contribution slot.** Rejected. The node's slot storage must
be reorganizable (a compaction table, a small-vector); a borrowed pointer would bind the design to
one storage layout. A driver names its slot by stable identity instead.

**Fold the single-driver check into the end of Resolve without a Seal phase.** Rejected. One scope's
Resolve finishing does not mean the whole elaborated design's driver topology is complete; the count
is only meaningful after every parent-to-child and child-to-parent attachment exists, which is a
global barrier -- precisely the Seal phase the lifecycle already specifies (F5).
