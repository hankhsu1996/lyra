# Nets a Bidirectional Connection Joins Are One Resolution

## Date

2026-09-11

## Status

Accepted. Extends [net-driver-resolution](net-driver-resolution.md) and
[net-type-is-a-fold-and-a-contribution](net-type-is-a-fold-and-a-contribution.md): what resolves is
the simulated net a connection forms rather than a single declared net, and a net nothing joins is
the one-net case of it. Decision 4 of the first record expected net collapse to be the Seal
barrier's first consumer; it is not, for the reason F5 gives.

## Why this decision matters

An `inout` port is the one port direction that is not a directional edge. LRM 23.3.3 makes it "a
non-strength-reducing transistor connection", and 23.3.3.7 settles what that means by merging the
nets on both sides into one _simulated net_. So the construct is not a second kind of driver: it
decides **which nets are one resolution**, which is a question the driver model never asks. Getting
it wrong in the obvious way -- letting each side drive the other with what it resolved to -- is
undetectable on the cases a first test writes and wrong on every case that has a strength, which is
the reason the standard's own wording rules it out. This record fixes the object that resolves, the
phase the joining happens in, and the one thing a connection must agree about before it may join.

## Findings that shaped the design

### F1. Re-driving the opposite side is not a model of a transistor connection

The tempting realization gives each net a driver fed by the other net's resolved value. It passes a
test where one side drives strongly and the other does not, and it is wrong wherever strength
matters. Two pull drivers facing each other -- `tri0` on one side, `tri1` on the other -- resolve as
one net to `x` at pull strength, because the two contributions meet at equal strength. Re-driven,
each side sees the other's result at the strength the re-driving assignment carries, which is a
fixpoint iteration over values that never had a common strength to be compared at. A charge-storing
net loses its charge the same way. "Non-strength-reducing" is exactly the property no re-driving
has, and the only realization that has it is one where the contributions themselves meet.

### F2. Table 23-1 is not transitive, so a simulated net type is defined for a pair and not for a set

Reading the dominance table as a rank and taking the maximum over the joined nets is wrong, and the
table says so: `tri0` strictly dominates `trireg`, `trireg` and `wand` tie (whichever is external
wins), and `tri0` and `wand` tie as well. A relation with `a > b`, `b ~ c` and `a ~ c` is not an
order, so folding it over three nets has no answer the standard gives. The object this record builds
is formed by transitively joining pairs, so the standard's rule does not reach it.

What follows is the constraint rather than a table: a join requires the two nets to state the same
net type -- the same fold and the same own contribution -- so the simulated net has one net type by
construction however many declared nets it covers. `wire` and `tri` state the same thing and join;
so do `wand` and `triand`. A connection between net types that state different things is refused by
name.

### F3. The front end already decides the dissimilar case, and already reports it

`NetType::getSimulatedNetType` is Table 23-1, implemented in slang, and
`DriverTracker::checkNetCollapsing` runs it per port connection over bit ranges and issues the
warning the table's `warn` cells call for. That pass is the one the design already turned on. So
Lyra neither implements the table nor duplicates its diagnostics; where the two sides differ it
refuses, and the front end has already said why the program is questionable.

### F4. A collapsed net has to keep answering for itself

Every name a simulated net covers goes on being read, waited on, forced, sampled, and reached by a
hierarchical name from another unit. Under compile-per-unit a child's net is a member its own
compiled body addresses directly, and the parent cannot be compiled into it, so nothing may move or
delete that storage. Two realizations answer this: reach the surviving net through a pointer on
every access, or let every net it covers hold the value the simulated net resolved. The first taxes
every net read in every design for a construct most designs do not use; the second costs one value
copy per extra net covered, only when that value changes. Reads dominate, so the second wins -- and
it leaves every consumer of a net unchanged, which is what makes the semantic surface of this record
small.

### F5. The join needs no barrier, because nothing it depends on is still moving

A net fixes what its declaration gives it in the constructor, so every net in the design is
installed before any route runs. Joining is commutative and associative, and no SystemVerilog body
observes a net before Initialize, which the engine reaches only after Resolve has run design-wide.
So a connection may join its two nets the moment its route resolves, and every simulated net is
final when Resolve ends without anything committing it. The Seal barrier stays unmaterialized and
unowned.

### F6. A second consumer of what a net type states must read it where it already is

Until this subject, what a net type states had one consumer: the operation that installs it on the
net. So the keyword was translated once, at the last compile-time layer, and what it states crossed
to the runtime as that operation's operands. The join adds a second consumer -- the check that two
nets agree -- and it sits at elaboration rather than at lowering.

Serving it from the connection was built first and is wrong by the consumers table. The signature
would publish the port's net type _keyword_, and the first thing the connecting unit does with a
keyword is derive what it states -- which is the row that says the producer under-stated, and that
the fact should be carried from where it is known. Carrying the stated facts on the signature
instead is a second carriage of one derived fact to a second consumer, and it puts the derivation in
a layer that deliberately keeps only the spelling.

What separates this from the other thing a connection must agree about -- that both nets hold the
same value representation -- is whether the layer has to derive anything. Both value types are
already stated where the connection is read, one by the child's signature and one by the actual's
own type, so that check is a comparison of facts in hand and belongs there. The net-type check is
not, and belongs where the facts are.

## The decision

1. **What resolves is the simulated net a connection forms (LRM 23.3.3.7): the declared nets it has
   joined, considered as one.** A net no connection joins is the simulated net over one declared
   net, and its resolution is the same walk over one. There is no separate single-net path. The word
   is the standard's rather than one minted here, and "domain" is deliberately not used for it --
   this codebase already spends that word on the representation a value is realized in.

2. **A join pools contributions, never results.** Every driver a simulated net covers contributes to
   one resolution at the strength it drives at, and no net's resolved value is ever an input to
   another net's resolution. This is what makes the connection non-strength-reducing (LRM 23.3.3),
   and it is why invariant 8 of `net_resolution.md` -- that a resolved value carries no strength --
   survives net collapse.

3. **A simulated net has exactly one net type, and a join is refused unless both sides state it.**
   The two facts a net type states -- the fold, and the contribution the net type makes to its own
   resolution -- must agree between the two nets a connection joins. Equality is transitive, so
   requiring it of each connection leaves a simulated net of any size with one net type.

4. **The refusal reads what the two nets state, not what the connection spelled**, and so lands
   where the join does: at elaboration, as a user diagnostic (F6). It also means a construct that
   joins nets without being a port connection is covered by the same check rather than needing its
   own. That both nets hold the same value representation is a separate agreement, checked where the
   connection is read, because both types are already stated there.

5. **A join happens where the connection's route resolves, and commits nothing.** It is one
   statement in the parent's resolve body, beside the `ref` port's bind and in place of the reactive
   process the two directional ports install. It attaches no driver and registers no process.

6. **Every declared net holds the value its simulated net resolved and publishes its own change.**
   Each keeps its own contributions, its own resolved value, and its own observable identity; one of
   them stands as the simulated net and carries what belongs to it -- the fold, the net type's own
   contribution, and any procedural continuous assignment in force over it. Reading a net, waiting
   on it, sampling it, forcing it, and reaching it by a hierarchical name are therefore unchanged by
   collapse.

## Consequences

- An `inout` port is realized with less machinery than an `input` or an `output`, not more: one
  statement against their process, sensitivity, and continuous-assignment edge. A transistor
  connection is not a reactive edge, and the shapes say so.
- Strength, the wired-logic folds, partial drivers, charge storage, and procedural continuous
  assignment all cross a joined connection with no code of their own, because each is a property of
  a contribution or of the simulated net and neither is re-stated per declared net.
- A chain of `inout` ports, several ports of one child on one net, and an instance array whose
  elements share an actual all fall out of joining and need nothing named for them.
- The Seal barrier stays a lifecycle contract with no consumer. The first construct that genuinely
  cannot be applied as its route resolves will materialize it.
- Net collapse is invisible to every consumer of a net, so no reader, waiter, sampler, or cross-unit
  route changes. What grows is the net itself, by the two facts that say which simulated net it
  belongs to and which nets that one covers.

## Alternatives considered

**Each side drives the other with what it resolved to.** Rejected by F1: it is strength-reducing,
which is the one property LRM 23.3.3 names, and it turns a resolution into a fixpoint iteration.

**Reach the surviving net through a pointer on every access.** Rejected by F4. It is what a
simulator that flattens the design at compile time gets for free, because the flattening resolves
the name to the node before any code is generated; compiling per unit means the pointer would be
read at run time instead, on every read of every net in every design.

**Delete the dominated net and rebind its name.** Rejected: a child's compiled body addresses its
own member, and the parent that would delete it is not compiled into the child. This is also the
shape `reference_resolution.md` forbids -- connectivity that removes an object's storage or changes
its layout according to what it is wired to.

**Carry the dominance table into the runtime so a simulated net can pick its type.** Rejected by F2
and F3: the table has no answer for a set, the front end already answers it for a pair and warns,
and a runtime that branches on which net type a net is is the shape `net_resolution.md` forbids.

**Publish the port's net type and refuse a dissimilar connection where it is read.** Rejected, after
building it. It moves the diagnostic from elaboration to lowering, which is worth little when both
are before any simulated behaviour, and it pays for that by adding a fact to every net a unit
publishes for one consumer's sake, and by needing what a net type states in a layer that
deliberately keeps only the spelling -- so the derivation would be relocated or written twice. The
nets themselves already hold the answer.

**Migrate the dominated net's contributions into the surviving net at the join.** Rejected: it
rewrites driver handles that sources already hold and moves contribution storage that drivers name
by index, to buy nothing -- a resolution that walks every net it covers reads the same contributions
where they already are.

**Form the simulated net at the Seal barrier.** Rejected by F5. Every input the join needs is final
before Resolve begins, so a barrier after Resolve would only be a later place to do the same work,
and materializing a design-global phase for it would make the first consumer of that phase one that
does not need it.
