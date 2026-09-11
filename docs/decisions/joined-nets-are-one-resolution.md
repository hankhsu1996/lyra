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

### F7. F4's two realizations are not the pair it took them for

F4 above frames the choice as: reach the surviving net through a pointer on every access, or let
every net hold what the resolution produced. It is not a pair. Both are what is available when there
is **no object for the resolution**, so choosing between them was choosing not to build one, and the
third shape has what each was picked for -- a node that exists only while the design runs, holding
what a resolution needs, and a copy of its result on every name that reaches it. Reads stay direct;
the fold runs once per node rather than once per name.

What the omission cost is visible in the shapes it forced, and none of it is a matter of degree:

- **The fold ends up on a name.** Two positions of one name then cannot resolve under different net
  types -- and they can: LRM 23.3.3.7 is read per bit range because "different bits may have
  different net types", and a concatenation of a `wand` and a `wor` across a bidirectional port
  gives one port net two folds. That program is legal, `lyra check` accepts it without a warning,
  and no arrangement of per-name folds represents it.
- **Resolution goes quadratic.** A joint of K names is K resolutions each folding K names'
  contributions, where a node is one fold and K copies -- so a bus through K modules costs O(K^2)
  per driver change rather than O(K).
- **Facts that belong to the resolution have nowhere to live.** An `interconnect` net (LRM 6.6.8)
  has no net type until elaboration picks one, which is after every name exists; a net delay (LRM
  28.16) sits between the resolution and what a name shows; a resolution function (LRM 6.6.7) takes
  the drivers as one array; and a resistive switch (LRM 28.14, Table 28-8) reads the strength of a
  resolved value, which a node can keep and a name does not want.

The standard names the object outright and this record quoted the sentence without taking it: LRM
10.11 declares names "for the same physical net, or bits within a net", and its members are signals
"whose bits share the same physical nets". `tran` is the same object again -- 28.13 gives it the
same "shall not affect signal strength" the port connection has.

### F8. A composed run is expressible only as a position, which settles where the rebase happens

`selector-coordinate-resolution.md` D2 puts the rebase from a declared coordinate to a storage
position "inside the value", and D3 makes that position private -- "never a coordinate that flows
between runtime components". A run carries positions and so reverses both, which is argued rather
than assumed.

The falsification is that the coordinate-facing form cannot express what this needs. Connectivity
composes: joining `a` to `b` and `b` to `c` puts a run of `a` and a run of `c` in one resolution,
and where they meet is a selector of neither net -- the two may be declared in opposite directions
(`[3:0]` against `[0:3]`), so there is no source-level select of `a` that names it. Composition has
meaning only in positions.

What makes the reversal safe is that D2's two stated defects cannot occur here. Both name a _runtime
selector_: a narrow rebase wrapping an out-of-range index, and a four-state selector against a
two-state bound. A connection's run is a constant the front end folded and range-checked, and
nothing selects while the simulation runs. And D4 of that same record already draws this line --
whole-value movement "is position-wise and range-agnostic (LRM 7.6) ... Only element and slice
selection consult the range" -- which is what a connection is: LRM 10.11 gives it a packed union's
bit overlay rules, not a select's.

So the range is consulted exactly once, where the source wrote the select, which is the lowering
that reads it. That is also the one place with the folded constants in hand, so nothing below
recomputes anything.

### F9. One node per name is falsified by the standard's own example, and the fix makes the node smaller

The first implementation gave each name one node and merged the smaller into the larger. That holds
while every connection reaches the whole of one of the two nets, which was true of every connection
the model was built against. LRM 10.11's byte-swap example is not: it relates `A[7:0]` to `B[31:24]`
and `A[15:8]` to `B[23:16]` in one statement. If each name sits at one base in one space, the two
demand that A's base be B's plus 24 and also plus 8, so no widening, permutation or choice of
coordinates satisfies them. The same shape arrives without a concatenation, from two aliases between
part selects of one pair of names.

What removes it is not a bigger node but a smaller one: **a node is a run that every name in it
covers entirely**, so a name reaches one node per run of its positions and a connection cuts the
runs its ends fall inside before relating them. A node then needs no width of its own beyond the
run, no alignment per name beyond the offset among that name's positions, and nothing is ever
rebased -- which is the property the one-node-per-name shape was chosen for and did not have.

**The example is how this was found; the reason it had to come out this way is one clause earlier,
and it makes the cut predictable instead of surprising.** The standard's own unit of resolution is
the bit: LRM 6.7.1 makes a net "composed entirely of 4-state bits", LRM 6.5 makes each bit of a
packed type an independent element, and LRM 6.6.7 names it -- an _atomic net_ is one "whose value is
updated and resolved as a whole", and "a `logic` vector net is not an atomic net as each `logic`
element is resolved and updated independently", each atomic net describing "a single connection
point in the design". So the object this record builds is not a new idea; it is one object per
**run** of atomic nets that share a connection point, which is the bit-wise model compressed.

Read that way the rule writes itself. A run means "these positions are connected identically", so it
has to be maximal with respect to that, and a connection reaching part of one destroys the property
-- the cut is what restores it. One node per _name_ asserts something else entirely, that positions
belong together because one declaration named them, which the language never says and which the
byte-swap example is simply the smallest program to disprove. Anything that keeps a whole name at
one alignment fails the same way, however its coordinates are chosen.

## The decision

1. **What resolves is the physical net (LRM 10.11, 23.3.3.7): a set of positions that resolve
   together, which the connectivity of the elaborated design forms and which exists as an object of
   its own.** A declared net is a name that reaches a run of one; a net no connection reaches is the
   one name of a node covering it exactly, which is the same walk over one member. There is no
   separate single-net path, and no case for "not joined". "Domain" is deliberately not used for any
   of this, since this codebase already spends that word on the representation a value is realized
   in.

   A node is a space of positions with names placed in it, rather than a set of names each carrying
   a range. **A name reaches one node per run of its own positions**, which is what lets a
   connection permuting runs state what it does -- LRM 10.11's byte-swap example relates four runs
   of one name to four of another at four alignments, and one node per name cannot hold that
   whatever its coordinates are, since a name at one base in one space cannot satisfy two alignments
   at once. F9 has the falsification; this clause first said joining never has to split anything,
   which held of every join that existed when it was written and of none that reaches part of a
   name.

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

6. **What resolves is a node the connectivity forms, and a declared net is a name that reaches a run
   of it.** The node holds what a resolution needs and a name does not: the fold, the contribution
   the net type makes to its own resolution, and any procedural continuous assignment in force over
   the positions. A name holds what belongs to it: its own contributions, its own observers, and a
   copy of what the node produced over the positions it reaches. Reading a net therefore reaches its
   own storage directly and never follows a pointer to get there, and reading, waiting, sampling,
   forcing and reaching by a hierarchical name are unchanged by collapse.

   A node exists only while the design runs, because which names reach it is the connectivity of the
   elaborated design. Nothing compiles against one, no name's storage moves or changes layout, and
   no name is designated to answer for another.

   Transitivity is a property of the object rather than something maintained: a chain of connections
   leaves every name it passes through in one node, so nothing is closed, composed, or rebuilt.

7. **A run is a position, and the declared range that named it is read once, where the source wrote
   the select.** What a connection states is a position-wise overlay -- LRM 10.11 gives it "the bit
   overlay rules ... for a packed union with the same member types", and LRM 7.6 makes whole-value
   correspondence positional rather than range-relative -- so below the lowering that reads the
   source, no layer needs the range a net names its own positions by.

8. **Each side of a connection is a sequence of runs, and what the connection states is the two laid
   over one another.** LRM 10.11 gives an overlay the bit overlay rules of a packed union, so
   correspondence runs position-wise from the most significant end; the two sides' runs need not
   fall at the same boundaries, so the statement is taken in the pieces both sides have whole, each
   as wide as the shorter of the two it stands between. An actual naming one net is the case where
   its side has one run, and needs no path of its own.

   Which runs a side names is answered where that side's source is: the connecting unit reads its
   own actual, and the declaring unit publishes the run its port stands for, because the descent it
   publishes is in coordinates only that unit's declarations give meaning to. A consumer that had to
   turn those into positions would be deriving what the producer already knew.

9. **What resolves is a run every name reaching it covers entirely, and a name reaches one per run
   of its own positions.** A connection cuts the runs its ends fall inside -- on both sides, and for
   every name already sharing them -- and then makes one resolution of two runs that now cover the
   same number of positions. So a name is never at two alignments in one resolution, nothing is ever
   rebased, and two positions of one name can take part in two resolutions, which is what LRM
   23.3.3.7 requires of a name whose bits meet different net types. A name no connection reached is
   the single run that covers it.

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
  route changes. What a name gains is which node it reaches; what it gives up is the fold, the net
  type's own contribution and any force over it, each of which belongs to the resolution and moves
  to the node with it.
- A fact that belongs to a resolution rather than to a name now has somewhere to be stated, which is
  what the constructs still out of scope were each waiting for: a net type elaboration picks (LRM
  6.6.8), a delay between the resolution and what a name shows (28.16), a resolution function over
  the drivers as one array (6.6.7), and the strength of a resolved value that a resistive switch
  reads (28.14). None of them is taken here; each stops needing a reshape first.

## Alternatives considered

**Each side drives the other with what it resolved to.** Rejected by F1: it is strength-reducing,
which is the one property LRM 23.3.3 names, and it turns a resolution into a fixpoint iteration.

**Reach the surviving net through a pointer on every access.** Rejected by F4, and the rejection
stands while the reasoning around it does not -- F7 is why. It is what a simulator that flattens the
design at compile time gets for free, because the flattening resolves the name to the node before
any code is generated; compiling per unit means the pointer would be read at run time instead, on
every read of every net in every design. What F4 drew from that, and F7 corrects, is that there is
therefore no node: a node with a copy of its result on every name reaching it costs no indirection
at all.

**No object for the resolution, with the nets encoding it between them.** Rejected by F7, after
building it twice -- as a ring of the names in one resolution, and as a transitively closed set of
runs between them. Both are correct for what they were built for and both put the fold on a name,
which no arrangement of names can make per-range; both make a joint of K names cost K resolutions
over K names' contributions; and neither has anywhere to state a fact that belongs to the resolution
rather than to a name. The tell, available from the start, is that the standard names the object in
the clause this record is derived from.

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
