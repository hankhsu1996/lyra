# Nets and net resolution

Tracks support for SystemVerilog nets: a net is a value-carrying signal whose value is the
resolution of its drivers, not a directly written variable (LRM 6.5, 6.6). This covers net
declarations, the resolution of one or more drivers under each net type, drive strength, and
net-typed port connections. The model and its rationale are fixed by the contracts below; this file
tracks the delta between the code and that model.

Done when the in-scope net types carry correct driver-resolution semantics end to end --
declaration, single- and multi-driver resolution, the wired and tri-state net types, drive strength,
pull/supply, and charge storage -- and net-typed port connections behave as those nets across the
object graph.

## Contracts

This workstream reasons from these and does not restate them:

- `../architecture/net_resolution.md` -- a net is a resolved observable value plus a set of
  independently attached driver contributions under the net type's resolver; net vs variable; what
  is forbidden.
- `../decisions/net-driver-resolution.md` -- the settled model: a resolution node with node-owned
  driver slots and capability-handle drivers; attach during Resolve; the materialized Seal barrier;
  seed in Initialize; inline resolution reusing publish-on-change.
- `../architecture/elaboration_lifecycle.md` -- the phase protocol the attach / seal / seed steps
  ride on.

## Lifecycle prerequisite

- [ ] The Seal barrier is a design-global, coordinator-owned step, materialized when its first
      consumer needs it (N4 net-topology validation). It validates and freezes the whole design's
      driver topology at once -- a single-driver constraint or a forwarding chain spans scopes -- so
      it is an engine-level pass over the elaborated design, never a per-scope hook. Until that
      consumer lands, the Resolve-before-Initialize ordering the engine already enforces is the only
      barrier the in-scope sub-steps require.

## Sub-steps

- [x] N1 -- A `wire` / `tri` net declaration is a single-driver signal driven by one continuous
      assignment, in either form: an explicit `assign` or a net-declaration assignment
      (`wire w = expr`). The driver attaches at Resolve, seeds at Initialize, and updates in the
      activation process; a read observes the driver's value. Single-driver is the identity case of
      the resolution model, not a special path.
- [x] N2 -- Net-typed port connections (LRM 23.3.3), single-driver, both directions: a parent net or
      variable drives a child's input net, and a child's output net drives a parent variable or net,
      distributed across an instance array and through multi-level chains. A connection is one
      reactive edge -- the source is read, the sink is driven (a net sink attaches a driver, a
      variable sink writes) -- reusing N1's identity resolver; the new work is reaching the
      cross-unit net through the binding route. A net is a readable, well-typed observable from
      construction: it installs its empty-driver value (an undriven `wire` / `tri` reads `z` at its
      width), so a read before any driver attaches is valid rather than an uninitialized cell. This
      is the net facet of the port work in `hierarchy.md` (E5) and clears the first full-testbench
      wall in `ibex.md`.
- [x] N3 -- Multi-driver `wire` / `tri` resolution (LRM 6.6.1, Table 6-2): two or more drivers on
      one net -- local continuous assignments, sources arriving across ports, or both -- resolve
      under the tri-state truth table, where agreement passes through, conflict yields `x`, and
      all-high-impedance yields `z`. This is the N>=2 case of the resolver; the N=0 undriven value
      and the N=1 single driver are the identity cases N1 and N2 already establish.
- [ ] N4 -- A single-driver net type (`uwire`) reports a diagnostic when more than one driver
      attaches, naming each driver's source. The constraint is on the number of attached drivers,
      not on any current value.
- [ ] N5 -- The wired-logic net types resolve under their own truth tables: `wand` / `triand` by
      and-resolution and `wor` / `trior` by or-resolution (LRM 6.6.3).
- [ ] N6 -- Drive strength on continuous assignments and resolution by strength (LRM 28): a
      contribution carries a drive-0 / drive-1 strength, and stronger drivers dominate weaker ones.
- [ ] N7 -- Pull and supply nets (`tri0` / `tri1` / `supply0` / `supply1`) behave as built-in,
      never-detached drivers of the appropriate value and strength (LRM 6.6).
- [ ] N8 -- The charge-storage net type (`trireg`) retains its last driven value when undriven
      (capacitive state), with charge strength (LRM 6.6.4).

## Out of scope

- Bidirectional (`inout`) net connectivity and net-to-net collapse: unifying two or more nets into
  one shared simulated net with zero propagation delay. This is a cross-net connectivity domain
  distinct from resolving a single net's own drivers; it waits for its own workstream. A net-typed
  port that behaves as a single-driver continuous-assignment edge is in scope (N3); merging the two
  sides into one electrical net is not.
- Gate-level primitive instances and user-defined primitives as net drivers. Their outputs are net
  drivers in the same model, but the primitive instances themselves are a separate workstream.
