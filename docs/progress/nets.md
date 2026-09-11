# Nets and net resolution

Tracks support for SystemVerilog nets: a net is a value-carrying signal whose value is the
resolution of its drivers, not a directly written variable (LRM 6.5, 6.6). This covers net
declarations, the resolution of one or more drivers under each net type, drive strength, net-typed
port connections, and which nets a connection makes one resolution. The model and its rationale are
fixed by the contracts below; this file tracks the delta between the code and that model.

Done when the in-scope net types carry correct driver-resolution semantics end to end --
declaration, single- and multi-driver resolution, the wired and tri-state net types, drive strength,
pull/supply, and charge storage -- and net-typed port connections behave as those nets across the
object graph, whether the connection drives in one direction or joins the two sides into one
resolution.

## Contracts

This workstream reasons from these and does not restate them:

- `../architecture/net_resolution.md` -- a net is a resolved observable value plus a set of
  independently attached driver contributions under the net type's resolver; net vs variable; what
  is forbidden.
- `../decisions/net-driver-resolution.md` -- the settled model: a resolution node with node-owned
  driver contributions and capability-handle drivers; attach during Resolve; the materialized Seal
  barrier; seed in Initialize; inline resolution reusing publish-on-change.
- `../decisions/net-type-is-a-fold-and-a-contribution.md` -- what a net type states: which truth
  table resolves contributions of equal strength, and the contribution the net type makes to the
  net's own resolution.
- `../decisions/joined-nets-are-one-resolution.md` -- what one resolution covers: the nets a
  bidirectional connection has joined, pooling their contributions rather than their results.
- `../architecture/elaboration_lifecycle.md` -- the phase protocol the attach / seal / seed steps
  ride on.

## Lifecycle prerequisite

- [ ] The Seal barrier is a design-global, coordinator-owned step, materialized when its first
      consumer needs it. No sub-step below needs it, and the two that were expected to have both
      turned out not to: how many drivers a net may have is decided over the elaborated design by
      the front end, so nothing here counts drivers; and joining nets into one resolution needs no
      barrier either, because every input a join takes is final before any route runs and nothing
      observes a net until Initialize. The Resolve-before-Initialize ordering the engine already
      enforces is the only barrier this workstream requires.

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
      construction: it fixes its declared type there and reads as the fold over no contributions at
      all (an undriven `wire` / `tri` reads `z` at its width), so a read before any driver attaches
      is valid rather than an uninitialized cell. This is the net facet of the port work in
      `hierarchy.md` (E5) and clears the first full-testbench wall in `ibex.md`.
- [x] N3 -- Multi-driver `wire` / `tri` resolution (LRM 6.6.1, Table 6-2): two or more drivers on
      one net -- local continuous assignments, sources arriving across ports, or both -- resolve
      under the tri-state truth table, where agreement passes through, conflict yields `x`, and
      all-high-impedance yields `z`. This is the N>=2 case of the resolver; the N=0 undriven value
      and the N=1 single driver are the identity cases N1 and N2 already establish.
- [x] N3a -- A net's data type may be a fixed-size unpacked array, unpacked struct, or union whose
      every element is itself valid for a net (LRM 6.7.1), which makes such a value one net
      resolving per bit rather than a collection of separate nets. An unpacked-array port declared
      with no data type is that net implicitly. Resolution is stated over any value a net may hold
      instead of over one value type, so the undriven value, the fold, and change detection reach an
      aggregate by recursing into its elements, and the set is closed: the frontend admits exactly
      the four shapes the clause allows and rejects the rest, so all four resolve. The one
      combination without an answer is two drivers on different members of an unpacked union, which
      LRM 7.3 leaves without a defined storage overlay.
- [x] N3b -- A continuous assignment naming only part of a net drives only that part: its driver
      contributes high-impedance everywhere it does not drive, so disjoint partial drivers compose
      and overlapping ones conflict exactly as whole-net drivers do. This is the whole-net rule read
      at bit granularity, so it needs no second mechanism -- a net is still never written, only
      driven, whichever part of it an assignment names.
- [x] N3c -- Which fold a net uses is decided once, where the net type is translated, and reaches
      the net as the install its construction names rather than being assumed anywhere below. Two
      nets of one data type resolve differently when their net types differ, so nothing downstream
      can recover it from the value type. `wire` and `tri` name the same tri-state fold, which is
      why the assumption held while they were the only net types; each type below adds its fold
      beside it instead of replacing one.
- [x] N4 -- A single-driver net type (`uwire`) resolves the one driver it admits exactly as any
      other net with one driver does (LRM 6.6.2), and connecting a second one is reported as the
      error the standard makes it, naming the net and both drivers, including where one of them
      arrives through a port connection authored in another unit. How many drivers a net may have is
      a property of the elaborated design rather than of any value, so it is decided in the front
      end and nothing below counts drivers.
- [x] N5 -- The wired-logic net types resolve under their own truth tables (LRM 6.6.3): `wand` /
      `triand` by and-resolution, where any driver at 0 forces the bit to 0, and `wor` / `trior` by
      or-resolution, where any driver at 1 forces it to 1. High impedance is the identity of either
      fold, so a driver holding a bit at z leaves it to the others and an undriven net reads z, and
      the fold reaches an unpacked-aggregate net by resolving each element. Both backends run it.
- [x] N6 -- Every contribution carries the strength it is driven at, and resolution consumes it:
      where two contributions differ the stronger determines the positions it drives (LRM 28.12.1),
      and only among equal strengths does the net type's truth table decide (LRM 28.12.4). A
      continuous assignment states its own strength, on the statement or on the net declaration it
      is part of, and one that states none drives at strong; a port connection is such an assignment
      and drives at strong. A specification whose 0 and 1 halves differ is refused: at a position
      where such a driver is unknown it occupies a range of strengths rather than one, and a range
      has no representation here.
- [x] N7 -- Pull and supply nets (`tri0` / `tri1` / `supply0` / `supply1`) contribute their own
      value at their own strength (LRM 6.6.5, 6.6.6): the pull is outranked by every ordinary driver
      and so shows only where nothing else drives, and the supply outranks them and so shows
      regardless.
- [x] N8 -- The charge-storage net type (`trireg`) retains its last driven value per bit when its
      drivers go to high impedance, holding it at the charge strength its declaration names and at
      medium where it names none (LRM 6.6.4, 6.7.1, 28.15.2). It reads x before anything drives it.
      Charge decay is a delay on the declaration and is refused with every other net delay.
- [x] N9 -- A bidirectional (`inout`) port connection joins the nets on both sides into one
      resolution over the contributions of both (LRM 23.3.3, 23.3.3.7). Every driver of either side
      meets the others at the strength it drives at, under one net type's truth table, so a pull in
      one module and a strong driver in the other resolve the way they would on a single net --
      which is what makes the connection non-strength-reducing. Both names show the result, wake
      what waits on them, and answer a force over the joined net; a chain of such ports is one
      resolution across every net in it, and a port left unconnected joins nothing and resolves
      alone. The two sides must state the same net type: where they differ, the standard names a
      dominating type per pair of nets and that relation does not extend to the set a chain joins,
      so the program is reported rather than answered. What a net type states carries no width, so
      two nets of unequal width state the same one.
- [x] N10 -- A bidirectional connection reaches a run of a net rather than always the whole of it
      (LRM 23.3.3.7, 10.11): a connection naming a part select or a single-bit select of a net puts
      those positions, and no others, in one resolution with the child's port net. The positions a
      connection does not reach go on resolving over their own drivers, and a driver of either side
      meets the others only where the run overlaps it. An instance array distributing a packed
      actual across its elements (LRM 23.3.3.5) is this with no select written anywhere in the
      source, which is how a design meets it without asking for it. A connection naming a whole net
      is the run that covers it, so it needs no path of its own.

## Out of scope

- A bidirectional port whose connection names a concatenation of nets, one whose own internal name
  is a select of a declaration, and the `alias` statement (LRM 10.11). Each is refused by name. What
  remains for all three is turning what the source wrote into the runs it names -- pairing the two
  sides and splitting whichever is wider, which the front end already does for `alias` -- since a
  resolution over runs is what the model now carries.
- A simulated net formed from dissimilar net types (LRM 23.3.3.7, Table 23-1), refused by name at
  elaboration. The standard defines the dominating type for a pair of nets, and the relation it
  tabulates is not transitive -- `tri0` dominates `trireg`, while `trireg` and `wand` tie, and so do
  `tri0` and `wand` -- so it gives no answer for the set of nets a chain of connections joins.
- Gate-level primitive instances and user-defined primitives as net drivers. Their outputs are net
  drivers in the same model, but the primitive instances themselves are a separate workstream. They
  are also what makes a resolved net's own strength observable -- a switch passes it on, and a
  three-state gate with an unknown control produces a range of strengths rather than one -- so both
  arrive together with an ambiguous-strength representation the current model does not carry.
- A net declaration's delay (LRM 10.3.3), including the charge decay a `trireg` specifies as its
  third delay (LRM 6.6.4.2). Refused by name rather than dropped, because dropping one answers at
  the wrong time instead of refusing.
- `interconnect` nets (LRM 6.6.8) and user-defined nettypes (LRM 6.6.7), each refused by name. A
  nettype declared with no resolution function admits one driver, which is the same rule `uwire`
  states and the same front-end check answers.
