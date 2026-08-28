# Placing a Published Member

## Date

2026-08-27

## Status

Accepted. Realizes `unit-signature.md` D4 and `emission_model.md` invariant 8 on the machine-code
path; reverses nothing.

## Why this decision matters

A signature already says what a unit publishes, and a referrer already names a published member
where it compiles. On one backend that is enough: the referrer includes a declaration-only artifact
and the target language resolves the name, places the member, and binds the use. A backend emitting
machine code borrows none of the three and must perform all three itself, so a cross-unit member
access is refused there -- which is every module port connection, the most basic thing a hierarchy
does.

What is missing is one fact: where a published member sits in its object. The rule itself is settled
-- a published member sits in a fixed prefix of its object, ahead of everything the unit did not
publish -- and this entry settles the rest of it: who computes the position, out of what, and what
carries it from the unit that decides to the units that read. Both wrong answers are expensive. A
position carried across the boundary makes producer and consumer share a numbering they are supposed
to derive independently. A position recovered from a name while the design elaborates is the
run-time lookup the signature exists to remove.

## The tension this addresses

Three constraints hold at once and do not obviously fit.

- **A signature member is identified by its name.** Producer and consumer derive the signature
  separately, so nothing shared numbers its members; `unit-signature.md` names a design-wide index
  standing in for a member's identity as a forbidden shape.
- **LIR names a member by position.** Which member a place reaches is logical storage topology,
  which LIR owns; a place still holding a name has not been lowered, it has been deferred.
- **A signature's types live in the pool of the unit that published them.** A consumer takes them
  into its own pool, and that import is HIR machinery. No layer below has a way to read a type it is
  handed from elsewhere.

The first two fix where a name becomes a position: not below the layer that can still read a
signature, and not above the layer that must name storage positionally. The third fixes the
direction the fact travels: it goes down through the referrer's own IR, translated at each lowering
like every other fact, rather than sideways by handing a signature to a lower pass.

## Decisions

### D1. The position is the signature's order, computed on both sides and carried by neither

A published member's position in its object is its position in the signature. The declaring unit
places its published members in that order at the front of its object; a referrer counts the same
order in the signature it consumed. Neither states a position to the other and neither reads the
other's storage, so the two agree for the same reason they agree on a specialization name: they
compute the same function of the same input.

The name stays the identity, which is what makes this sound rather than a numbering in disguise. A
referrer looks a member up **by name** in the signature it holds, and what it gets back is where
that member sits in the object the signature describes. A renamed member is not found and fails
where the referrer compiles. A member added ahead of another moves it, changes the signature, and
re-emits exactly the referrers that consume it -- which is the dependency being real.

### D2. A referrer records the object it compiled against; nothing below HIR reads a signature

The first time a unit reaches the object of a unit it references, it records what that unit promised
about it: the declaring unit, the name of the class an instance of it is, and the published members
in signature order, with their types taken into this unit's own pool. Every instance member and
every route ending on a published member names that one record instead of restating the two names.

The record is the whole of what travels. HIR-to-MIR and MIR-to-LIR translate it the way they
translate a class this unit declares, so no lowering below the one that consumes a signature is
handed one, and no layer reads a type it does not own. It also removes a restatement: the declaring
unit and its class name were spelled again at every instance member and at every reference into one,
with nothing making the copies agree.

### D3. An object another unit defines lives in its own registry, never among the classes this unit compiles

A described object and a compiled class answer different questions, and the difference is not
decoration: everything that walks the classes a unit compiles does so in order to emit them. Putting
a described object in that registry makes emitting another unit's class the default and correctness
a matter of every such walk remembering to filter -- and a walk that forgets emits a second
definition of a symbol another unit already defines, which is a link error a long way from its
cause.

So the two are separate registries and the invalid combination cannot be spelled. What they share is
the member vocabulary: a member step names a position, and the members it indexes are read through
one accessor that answers for either, so a projection over another unit's object and one over this
unit's own are the same node reaching the same kind of answer.

### D4. A published member states which storage it is, and one function turns that into a cell

A member's storage is a variable's observable cell, a net's resolved cell under a stated resolution,
or a reference aliasing the connected variable (LRM 6.5, 23.3.3.2). The signature states which,
because the declaring unit's own declaration is the only honest source and a referrer that read it
there would be reaching past the promise.

The mapping from that statement to the cell a member holds is total and pure, so both sides compute
it through one function rather than one side stating the answer. That is the same reasoning that
keeps a class's name a function of its unit rather than a field travelling beside it: when two
layers must agree on something either can compute, a shared function makes disagreement unspellable,
and a carried value only makes it invisible.

### D5. The producer places its published members while lowering, not by reordering what the user wrote

A unit's declarations stay in source order in HIR, because that order is what the source says and
other things read it as such. The prefix is established one layer down, where the object's members
are built: the published ones first, in signature order, then everything else in declaration order.
The permutation lives entirely in the mapping from a declaration to its member, which that lowering
already maintains.

Which declarations were published, and in what order, is recorded by the pass that derives the
signature, so the promise and the placement cannot describe different objects.

## Rejected alternatives

- **Carry the position on the reference.** The consumer resolves the name against the signature
  anyway, so the position is derived at both ends regardless; sending it as well adds a second
  statement of one fact and makes a stale reference express a wrong position instead of failing to
  find a name. It is also the numbering `unit-signature.md` forbids, wearing a per-reference
  disguise.

- **Hand the signatures to HIR-to-MIR, or to MIR-to-LIR.** Attractive because it looks like less
  plumbing, and it fails on types: a signature carries its types in its own pool, so a lower pass
  handed one would have to import HIR types into a layer that does not own them, or a second type
  translation would appear beside the one each lowering already has. It also widens what those
  passes may read about the rest of the design, which is exactly the property the narrowed
  consumption established.

- **Put the published members on the external object type instead of in a registry.** The type would
  then carry a member list, one interned entry per referenced object, and no new registry appears.
  Rejected because a type is not where a nominal entity's declaration goes -- the object has a name,
  it will grow an entry point when constructing an instance reaches the declaring unit's own
  constructor, and neither belongs on a type -- and because the member step would then read two
  differently shaped tables depending on which arm the base type took.

- **Put the described object in the class registry with a flag saying it is not compiled here.** One
  registry, one member accessor, no new identity kind. Rejected under D3: the flag makes emitting
  another unit's class a filter every emission walk must apply rather than a state it cannot reach.

- **Reorder the declaration arena so the published members come first.** Then the arena order is the
  placement and no permutation exists anywhere. Rejected because the arena is built while bodies
  lower, and its order is also the order initializers run in (LRM 10.5); reordering it to serve
  placement would move an observable behaviour to serve a layout rule.

- **Resolve the position by name at run time, through the by-name lookup that already answers for
  names past a signature.** It works and it needs none of this. Rejected because the referrer
  consumes the signature already: the lookup buys back an independence the declared dependency has
  spent, replaces a compile-time check with an unchecked cast, and puts a query on a path that has
  the answer at compile time.

## Consequences

- A cross-unit member access is an ordinary member access at every layer below the one that resolved
  the name. The machine-code path realizes a port connection, and the two paths refuse the same
  designs for the same reasons rather than differing by which one can borrow a name resolver.
- The object of another unit stops being a second vocabulary. Its type names a record rather than
  carrying two strings, and a member of it is the same reference form as a member of a struct or a
  closure -- the arena is fixed by the receiver's type, so the reference states only which member.
- A unit's object has a stable published prefix, which is the layout fact an incremental build
  needs: a change confined to unpublished declarations moves no published member.
- The signature states a published member's storage kind, so a referrer builds the cell it reaches
  without reading the declaring unit's declaration. That completes what a port part already promised
  by naming its member rather than restating it.

## Cross-references

- `unit-signature.md` -- D4 names a signature member where the referrer compiles; the placement rule
  and the forbidden design-wide numbering are stated there and applied here.
- `../architecture/emission_model.md` -- invariant 8 states the prefix rule and invariant 2 the
  inputs a unit's emission may depend on, which is what D2 keeps true below the consuming pass.
- `../architecture/lir.md` -- a place names storage by logical identity with physical layout derived
  below it, which is why the name becomes a position at MIR-to-LIR and not later.
- `member-slot-storage.md` -- a member is a logical place realized per backend; this entry says how
  a member of another unit's object reaches the same place vocabulary.
- `specialization-identity.md` -- the same independence: producer and consumer compute one answer
  from one input rather than sharing a table.
- `cross-unit-class-translation.md` -- how a reference to another unit's class is classified at
  AST-to-HIR, which is the pass D2 places the record in.
