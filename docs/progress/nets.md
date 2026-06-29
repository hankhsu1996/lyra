# Nets and net resolution

Tracks support for SystemVerilog nets: a net is a value-carrying signal whose value is the
resolution of its drivers, not a directly written variable (LRM 6.5, 6.6). This covers net
declarations, the resolution of one or more drivers under each net type, drive strength, and
net-typed port connections. The model and its rationale are fixed by the contracts below; this file
tracks the delta between the code and that model.

Done when the in-scope net types carry correct driver-resolution semantics end to end --
declaration, single- and multi-driver resolution, the wired and tri-state net types, drive strength,
pull/supply, and charge storage -- and net-typed port connections behave as those nets across the
object graph. When the last item lands, the file is deleted.

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

- [x] The Seal barrier is realized as a design-wide phase between Resolve and Initialize. The
      lifecycle defines it; net topology validation (N4) is the first consumer that requires it.

## Sub-steps

- [x] N1 -- A `wire` / `tri` net declaration is a single-driver signal driven by one continuous
      assignment, in either form: an explicit `assign` or a net-declaration assignment
      (`wire w = expr`). The driver attaches at Resolve, seeds at Initialize, and updates in the
      activation process; a read observes the driver's value. Single-driver is the identity case of
      the resolution model, not a special path.
- [ ] N2 -- The `wire` / `tri` resolution function over the driver set: an undriven net (no driver)
      reads high-impedance (`z`) at the net's width, and two or more drivers resolve under the
      tri-state truth table (LRM 6.6.1) -- agreement passes through, conflict yields `x`,
      all-high-impedance yields high-impedance. The N=0 and N>=2 cases of the resolver that N1's
      single driver exercises as identity.
- [ ] N3 -- Net-typed port connections: a net driven across a module port in either direction (a
      child output driving a parent net, a parent net driving a child input net), single- and
      multi-driver, distributed across an instance array. This is the net facet of the port work
      tracked in `hierarchy.md` (E5) and the first full-testbench wall in `ibex.md`.
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
