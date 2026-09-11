# Net Resolution

## Purpose

Define what a SystemVerilog net is and how its value is produced. A net's value is not written; it
is the resolution of a set of independent driver contributions under the net type's resolution
policy. What that resolution covers is not always one net, and not always the whole of one: a
bidirectional connection and an `alias` each state that runs of positions across several nets are
the same physical net, resolving over the contributions of all of them, and a net nothing reached is
that same resolution with nothing coupled to it. This document owns the driver / contribution /
resolution model, what one resolution covers, and the distinction between a net and a variable. It
is the design-global net-resolution concern that `reference_resolution.md` places outside its own
scope.

## Owns

- The rule that a net is a resolved observable value: its value is `resolve(contributions)`, where
  the resolver is fixed by the net type and the contributions are supplied by the drivers of every
  name reaching those positions and by the net type itself.
- The decomposition of a net into three parts: a resolved observable value, a set of driver
  contributions, and a resolver policy.
- The rule that each driver is an independent contribution with its own identity and provenance. A
  driver updates only its own contribution; it never writes the net's resolved value.
- The rule that the net re-resolves whenever any contribution changes, and publishes a change only
  when the resolved value itself changes.
- The rule that a net type states two things and no more: the fold it performs, and the contribution
  it makes to the net's own resolution -- a value and the strength it holds that value at.
- The distinction between a net and a variable as a property carried by type, not a flag: a variable
  owns mutable state and is written directly (a procedural write, or a single continuous driver); a
  net owns a resolver and a driver set and is only ever driven.
- The rule that a driver's contribution carries both a logic value and a drive strength, and that
  resolution consumes both: strength decides between contributions of unequal strength, and the net
  type's fold decides among those of equal strength.
- What a position is, which the standard settles rather than this model: for a net of a built-in net
  type, resolution runs per bit. LRM 6.7.1 makes a net "composed entirely of 4-state bits", each
  carrying its own strength; LRM 6.5 makes every bit of a packed type an independent element; and
  LRM 6.6.7 names the unit outright -- an _atomic net_ is one "whose value is updated and resolved
  as a whole", and "a `logic` vector net is not an atomic net as each `logic` element is resolved
  and updated independently". So a vector net is as many resolutions as it has bits, and what the
  clause calls a single connection point is the set of them some connectivity relates.
- What resolves: a physical net, the set of positions the elaborated design's connectivity places in
  one resolution (LRM 10.11, 23.3.3.7). It is an object of its own, carrying the fold, the net
  type's own contribution and any procedural continuous assignment over those positions -- the facts
  a resolution needs and a name does not. Every name in one covers the whole of it, so a declared
  net reaches one per run of its own positions, holding its own contributions, its own observers and
  a copy of what each of those resolutions produced over the run it covers; a net no connection
  reached is the one name of the single resolution covering it exactly.

  **A physical net is therefore one object per run of positions the design connects identically,
  rather than one per position.** That is a compression of the bit-wise model above and is what
  gives it its own rule: a run is exactly a maximal set of positions that agree on what they are
  connected to, so a connection reaching part of one leaves it no longer maximal and it is cut. A
  design that connects whole nets writes one run per net and never meets the cut.

- The rule that a resolution pools the contributions reaching its positions and never makes one
  resolution's value an input to another's.
- The rule that one resolution has one net type, so a join requires the nets it reaches to state the
  same one -- the same fold, the same contribution of the net type's own, and the same answer to
  whether resolution keeps that contribution current. What a net type states carries no width, so
  nets of unequal width state the same one.

## Does Not Own

- The route by which a driver reaches a net across compilation units, and by which a net's value is
  read across units (`reference_resolution.md`).
- The phases in which drivers attach, connections join, topology freezes, and contributions seed
  (`elaboration_lifecycle.md`).
- The capability-type family and the observable-cell access protocol that a net's resolved value and
  a driver handle are members of (`mir.md`).
- Waking dependent processes when a net's resolved value changes (`scheduling.md`).
- Which connections join nets, and which run of each they reach. Whether a construct places nets in
  one resolution is a property of the construct -- a port's direction, an alias statement -- and so
  is which of their positions it reaches, which a select, a concatenation and an assignment pattern
  each state in their own terms. Turning what the source wrote into a run belongs to whatever owns
  that construct; this document owns what being in one resolution means.

## Core Invariants

1. A net's value is the resolution of its contributions under the net type's resolver. With zero
   drivers the value is what the net type's own contribution resolves to; a single driver and many
   drivers are the N=1 and N>1 cases of the same resolution, with no separate single-driver
   representation. Where a connection has placed runs of other nets in one resolution with a run of
   this one, "its contributions" are the contributions reaching every run that resolution covers,
   each read at the positions it occupies here; a net nothing reached is the case with nothing
   coupled to it and has no representation of its own.
2. A driver is an independent contribution with identity and provenance. A driver writes only its
   own contribution and never the net's resolved value directly. The net owns the contribution
   storage; the driver names its contribution by a stable identity, never by a borrowed pointer into
   that storage.
3. The net re-resolves on any contribution change but publishes a change only when the resolved
   value changes. A contribution that moves without changing the resolved value wakes no observer.
4. A net is never the target of a direct or procedural write; it is only driven. This is carried by
   the net being a distinct capability type, not by a classifying flag beside a value type.
5. Resolution is a function of the contributions as they stand and of nothing else. The one
   contribution resolution itself writes is the charge-retaining net type's own, which takes the
   resolved value at every position some stronger contribution decided; every other contribution is
   written by its driver or by the net type once.
6. A constraint on how many drivers a net may have is decided before this model. It is a property of
   the elaborated driver topology rather than of any contribution's value, so nothing here counts
   drivers and no net type is modelled as refusing a second one; a net type that admits one driver
   resolves the one it has exactly as a net that happens to have one does.
7. Strength orders contributions and nothing else: where two contributions differ in strength the
   stronger decides the positions it drives, and where they agree the net type's fold decides. A
   contribution at the high-impedance strength decides no position, which is what makes a net with
   no drivers resolve to its net type's own contribution rather than to a case of its own.
8. A net's resolved value carries no strength. This holds while no modelled construct makes one
   net's resolved value an input to another resolution; a switch and a gate primitive are the two
   that would, and both are outside this document's subject. A bidirectional connection is not one
   of them: it pools the contributions rather than the results, which is what makes it
   non-strength-reducing.
9. One resolution has one net type. A join therefore requires the nets it covers to state the same
   fold and the same contribution of the net type's own; where they differ, the standard names a
   dominating type per pair of nets and that relation does not extend to the set a chain of
   connections joins, so the program is reported rather than answered.

## Boundary to Adjacent Layers

- `reference_resolution.md` owns the one route from a referrer to a target. A driver attaches to a
  net through that route, and a read of a net reaches the resolved value through that route; net
  resolution is the concern that doc names as design-global and out of its own scope.
- `elaboration_lifecycle.md` owns when. Drivers attach and connections join during Resolve, which
  completes design-wide before anything observes a net; contributions are seeded in Initialize; the
  processes that update contributions arm in Activate.
- The front end owns which drivers a legal program may give a net, as `compiler_overview.md` places
  every question answered over the elaborated design before HIR. A rule the standard states as an
  error rather than as a value -- a net type that admits one driver, a nettype declared with no
  resolution function -- is decided there, and this model resolves whatever topology it is handed.
- `mir.md` owns the capability-type family. A net's resolved value is a capability type sibling to
  the plain observable cell (readable and observable, but not directly writable); a driver is a
  capability handle sibling to a reference.
- `scheduling.md` owns the wakeup that fires when a net's resolved value changes.

## Forbidden Shapes

- A net modeled as a plain writable cell that each driver writes directly, so the last write wins.
  Multiple drivers must resolve; a net's value is a projection of its contributions, never the most
  recent write. The one thing that displaces that projection is a procedural continuous assignment
  (LRM 10.6.2), which overrides every driver rather than joining them and is therefore not a driver
  at all -- the contributions go on being what the net answers with the moment it is released.
- A net's net-ness carried by a flag beside a value type. The capability type carries it.
- One aggregate per-net process that reads every driver's expression and fixes the driver set at the
  net's owning-unit compile time. A net's drivers are not all knowable when its unit compiles -- a
  driver may be a cross-unit port connection authored in another unit -- so drivers attach
  independently, not as a statically gathered list.
- A driver holding a borrowed pointer into the net's contribution storage. The storage is the net's,
  reorganizable at will; a driver addresses its contribution by stable identity.
- A single-driver net represented as a variable, or any path that special-cases the single-driver
  case apart from the N-driver resolution.
- A net type modelled here as refusing a second driver, or any count of drivers kept so that a
  constraint can be checked against it. What a legal program may connect is decided before this
  layer.
- A net type realized as a resolver of its own, or as an alternative anything below this layer
  branches on. A net type says which fold and what it contributes; a type that pulls, supplies, or
  stores charge differs from a plain one in that contribution and in nothing else.
- A value the net type contributes to itself modeled as a driver. It is never detached, never
  updated by anything outside the net, and never counted by a constraint on the driver set.
- A net publishing to its observers because a contribution changed while the resolved value did not.
- Two nets joined by making one a reference to the other, or by deleting one and rebinding its name.
  A net a connection joins goes on being read, waited on, sampled, forced, and reached by a
  hierarchical name under its own name, so nothing may move or remove its storage.
- A join realized by giving each side a driver fed by the other side's resolved value. That is
  strength-reducing, which is the one property the standard names for a bidirectional connection,
  and it turns a resolution into a fixpoint over values that never met at a common strength.
- A separate representation for a net no connection joined. Such a net is the one run of positions
  that covers it, reaching the one resolution over it, which is the same walk every other net takes
  rather than a case beside it.
- A fact the resolution owns kept on a name: the fold, the contribution the net type makes to its
  own resolution, or a procedural continuous assignment over the positions. A name cannot state one
  per run, so keeping any of them there makes two positions of one name unable to resolve
  differently, which the standard requires of them (LRM 23.3.3.7 is read per bit range).
- A resolution the names in it encode between them rather than an object they reach. A ring of the
  names, or a closed set of runs relating them pairwise, is correct for the values and wrong for
  everything a resolution owns, and costs a walk per name where an object costs one.

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

The single-driver `uwire` net resolves as any other net with one driver does; what makes it single
driver is that connecting a second one is an error (LRM 6.6.2), which is the same rule the standard
states generally for a nettype declared with no resolution function (LRM 6.6.7). Being unresolved
and admitting one driver are one fact, and it is a fact about the elaborated design rather than
about resolution, so no net type here is spelled differently for it.

A `tri0` net is "equivalent to a wire net with a continuous 0 value of `pull` strength driving it"
(LRM 6.6.5), which is the net type's own contribution: the tri-state fold decides every position
some driver drives, and the contribution decides the rest. A `supply0` net says the same thing at
the other end of the scale, so its contribution outranks every driver instead of deferring to them,
and a `trireg` says it with a contribution resolution itself keeps current (LRM 6.6.4, 6.7.1).

A net's data type may be an unpacked aggregate whose elements are themselves valid for a net (LRM
6.7.1), and the fold then reaches an aggregate by recursing into its elements: a contribution is a
whole-net-shaped value, and a driver that covers only part of the net carries the resolution
identity everywhere it does not drive. The one aggregate the fold cannot always answer for is the
unpacked union: LRM 7.3 gives it no required storage representation and, unlike a packed union, no
reading back of a member written as another, so two contributions that are both driving different
members overlay in no defined bit space. The cases a design has resolve exactly -- one driver on any
member, several on the same member, and the undriven seed, which is the identity whichever member it
carries -- and the combination the standard leaves undefined is reported as such rather than
answered with an invented value.

A bidirectional port connection is "a non-strength-reducing transistor connection" (LRM 23.3.3), and
the standard settles what that means by merging the nets on both sides into one simulated net (LRM
23.3.3.7). Merged, a pull inside the child and a strong driver in the parent meet at the strengths
they were driven at and the strong one decides, which is the property the phrase names; a driver in
either module reaches both names; and a chain of such connections is one resolution across every net
in it, with no step in the chain resolving anything of its own. The derivation and what it rejects
are in `decisions/joined-nets-are-one-resolution.md`.
