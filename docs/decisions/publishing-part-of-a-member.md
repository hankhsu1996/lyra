# Publishing part of a member

## Date

2026-09-03

## Status

Accepted, with the write half of D5 superseded. Completes what a connection point may name on a
signature, which `unit-signature.md` already said had a projection standing in it; narrows one
sentence of `value-projection-designator.md`.

`names-a-view-offers.md` (2026-09-11) supersedes D5's answer for a name a modport offers for
writing, and narrows its reading of a plain port identifier. What it keeps is D5's answer for a name
offered only for reading, and every other decision here -- D2's projection is what the superseding
entry publishes. Read D5 with that entry beside it.

## Why this decision matters

A unit publishes connection points, and every one of them today stands for a whole declaration. The
language does not agree. A module port may name part of an internal name (LRM 23.2.2.1, 23.2.2.2)
and a modport port identifier may name part of an interface item (LRM 25.5.4), so what a referrer
reaches through the point is narrower than the storage behind it -- and the referrer never sees the
source that said so.

Both clauses describe the set in the same words:

> elements of arrays and structures, concatenations of elements, and assignment pattern expressions

so this is one language concept reached from two constructs, not two features that resemble each
other. What is missing on the signature is a way to say what a point reaches, and the question is
what shape that takes: an expression, a value, or something else. The answer decides whether a
signature ever carries an expression across a unit boundary, which is a property of the whole
boundary rather than of these two clauses.

## The tension this addresses

Three constraints hold at once.

- **Only the declaring unit saw the projection.** `published-member-placement.md` D1 lets producer
  and consumer derive a member's position independently, because both hold the signature. That
  argument does not extend here: `r[3:0]` is written in the interface's source, which the referrer
  never reads, so the fact must be carried rather than recomputed.
- **What crosses a boundary is structure, never an identity.** `compilation_unit_model.md` invariant
  12 -- a signature carries the storage its own identities index, and a consumer takes the structure
  into its own. An expression is a graph over the publishing unit's arenas, so it is the one shape
  that cannot cross without a second import machine beside the one that carries types.
- **The referrer must end up with an ordinary access.** `reference_resolution.md` invariant 7: an
  endpoint is the target's own access surface, not a new category. A mechanism that made a projected
  point a different kind of endpoint would put a second reach mechanism beside the one every other
  cross-unit name uses.

## Decisions

### D1. A module port's point names a projection, because a port connection is an assignment

The shared wording is the expression syntax, not the construct. What a module port _is_ settles the
mechanism, and LRM 23.3.3 states it: "Each port connection shall be a continuous assignment of
source to sink", with 23.3.3.2 repeating that a continuous assignment "shall be implied" for a
variable connected to an `input` or an `output` port.

A continuous assignment's sink is an lvalue, so the child side of a port is storage the child holds
-- which is also why the port expression is an lvalue whichever way the port runs. There is no
computed form of it to represent, and nothing here is a choice between a reference and something
more general: an assignment needs a place, and a projection is what names one.

So a module port's point names one of exactly two things:

| What it names            | What the referrer does with it                   |
| ------------------------ | ------------------------------------------------ |
| a projection of a member | reaches that member and descends the stated path |
| nothing                  | data crosses the point and reaches no storage    |

The second is the language's own case, not a defensive arm: LRM 23.2.2.1 says the port expression is
optional "because ports can be defined that do not connect to anything internal to the module".

A concatenation is not a third alternative. A port whose expression joins several names is already
several parts of one port (LRM 23.2.2.1 gives the first name written the most significant bits), and
each part names one of the two above.

A modport's port identifier is a different construct and D5 settles it. It is no connection carrying
data between two storages: LRM 25.5.4 makes it a name the view gives an expression, its `input` form
need not be an lvalue, and the front end accordingly admits any expression over what the interface
declared.

**So the two mechanisms answer two things the standard models differently**, and the shared syntax
between the clauses is syntax. A port is an assignment and needs a place; a name a view offers is an
expression and needs a way to evaluate it where it was written. Neither is the other's optimization,
and reading them as one construct with two representations is what makes a projection look like a
short cut it is not.

The temptation both halves rule out is the same. A constant looks like a further thing a point can
name, because its value is available early; it is not, it is one expression among the set a slot
admits, and giving it an alternative of its own puts a property of one value into the vocabulary
every consumer reads.

### D2. A projection is a published member plus a closed selector path, and the whole member is the empty path

A projection carries the member it descends into and a path of selectors, in owner-to-leaf order.
The selector set is drawn from the one a value projection already has, because descending into a
value is the same descent whoever wrote it; a step gains its arm with the construct that first needs
it, so a coordinate into a homogeneous value and a fixed-width window are the two the language's own
port and modport references reach.

Every coordinate is the source-level one, and each step states the type it lands on. Which storage
position a coordinate names follows from the declared range of the value it descends into, which the
step's own type carries, so neither side rebases and the two cannot disagree about a coordinate
system. Carrying the type per step is also what keeps a reader from having to know what selecting
from a type produces.

A point naming a whole declaration is the empty path. It is not a separate case with its own arm and
its own handling: the referrer descends a path of length zero and reaches the member, which is what
it reached before this entry existed.

### D3. A signature carries a selector path, never an expression

This is the general answer the two clauses forced. A selector path is closed, and every part of it
is a coordinate or a position -- structure, in invariant 12's sense, re-identified in the consumer's
own storage like a type. An expression is open: it names declarations, it indexes the publishing
unit's arenas, and reproducing it in a referrer means either a second import machine or the referrer
reading the publishing unit's source. Both are the boundary leaking.

So the translation from the source form to the path happens where the unit's own declarations are
read, and what a lowering downstream of that sees is a path. A source form that is not a path does
not become one by being carried further; it is refused where it is read.

### D4. Nothing here folds a constant, and no alternative is named for one

`shape-from-types-contents-from-expressions.md` S1 already binds: an operand is lowered as the
expression it is, nothing asks whether it is constant, and nothing evaluates one. This entry adds no
exception to it.

The reason to say so rather than leave it implied is that a connection point is where the exception
looks earned. `modport A (input .Q(2))` reads as a point whose value is simply known, and the
declaration's own value is a thing signatures do carry. Both readings are the same mistake: `2` is
not a kind of connection point, it is one expression out of a set the same syntactic slot admits,
and the rest of that set -- `{r[3:0], s}`, `'{...}`, `r[3:0] + 1` -- cannot be folded at all. An
alternative minted for the members of a set that happen to be computable early leaves every other
member with no representation, and states as a kind what is only a property of one value.

`input .Q(one)`, where `one` is a declaration of the interface, is not in that set at all: it is a
projection with an empty path, because the interface publishes `one` like any other member.

### D5. An interface publishes its modports

What forces the view onto the signature is the expression form. `Part` names no declaration of the
interface, so there is nothing on the member list to resolve it against, and a referrer would
otherwise have to read the interface's own declarations to find out what it means. A plain name
could be resolved without the view -- the identifier is then the item's own name (LRM 25.5.4) -- but
it is the same construct with a shorter expression, so it takes the same route rather than a case of
its own.

So a modport is published: its name, and per port identifier a name, the pair of subroutines the
interface carries out for it -- one evaluating the expression, one assigning to it, the second
absent where the view admits no write -- and the members that expression reads.

**That pair is what makes the identifier one concept rather than a set of kinds.** A designator, a
constant, a concatenation, an assignment pattern and an expression over the interface's members are
five spellings of one thing: an expression the interface evaluates. The LRM says so where it
explains the plain form -- a simple port identifier "is used as both a reference to an interface
item and a port identifier" -- which is the general case degenerating, not a separate case.
Publishing a callable is how an expression crosses a boundary it cannot cross as a graph, because a
callable is an expression that has been given a name; the expression itself stays in the unit that
wrote it and is lowered where its names resolve.

**The read set is what keeps the pair as capable as a reference.** A reference can be read, written
and waited on; a call answers the first two and shows nothing to wait on. So the view publishes the
members the expression reads, and a process waiting on the name waits on those -- any of them
changing changes what the name evaluates to. LRM 25.5 confines those names to the interface's own
declarations, so each is a member it already publishes and the set crosses as positions rather than
as anything new. Without it the pair would be the weaker representation, which is the shape that
would have made a projection look necessary rather than convenient.

This is not a modport becoming a runtime entity, a handle type, or a second way to reach a name --
it is resolved where the referrer compiles, and what it reaches is what the interface promised. It
is the view itself becoming part of that promise, which is what `unit-signature.md` D1 already calls
a modport: a named, direction-carrying subset of the signature.

Reaching a designator through a call rather than through the storage directly is a cost, and
removing it is an optimization over a representation that already states the expression -- not a
second representation to choose between at the point of publishing.

The direction the view gives a name is part of the language's own promise and not part of this one,
because nothing on the referring side reads it: a use running against the declared direction is
refused while the design elaborates, and admitting the field anyway is the shape `unit-signature.md`
forbids -- a fact on a signature because a reader named it rather than because a lowering reads it.

Which modport a connection selected is already part of the module's specialization identity, so a
module reached through two modports of one interface is two units and each compiles against the view
its own connection named.

### D6. The projection is applied by the referrer as an ordinary access, and no endpoint category is added

The route to a projected point is the route to its member, unchanged, and the projection is a select
over what that route reached. The sealed endpoint stays the member's own cell.

This is what keeps `hierarchical-reference-routing.md` D5 from being invoked at all: there is no new
target category, because the target has not changed. `i.Part` reaches exactly what `i.r[3:0]` would
reach if the module had written it and the modport allowed it, and the two produce the same access.
A write through it is the value projection a write into any interior already is; a read is the
select a read of any interior already is.

## Rejected alternatives

- **Give the point its own cell and continuously assign it to the part.** The port becomes an
  ordinary whole-member port and no signature changes -- which is what makes it tempting, and it is
  the shape a flattening simulator takes. Rejected on four counts, the first decisive: LRM 23.2.2.2
  says the port expression "shall not be considered an assignment-like context", and a synthesized
  continuous assignment is exactly that context, applying conversions the clause says do not happen.
  It also doubles the storage and inserts a propagation delay the clause does not have; it is the
  mirror of the shape `reference_resolution.md` forbids, where connectivity decides an object's
  layout and a wired member is addressed differently from an unwired one; and it does not work at
  all for a modport, which is a view -- two modports select different parts of one item, so a cell
  per modport port would leave the interface's own name and the module's view denoting different
  storage.

- **Carry the source expression on the signature and let the referrer lower it.** Rejected under D3.
  Beyond the import machine it needs, it would let a referrer's emitted output depend on a
  declaration the signature was supposed to bound, which is the property `unit-signature.md` D3
  turns into a theorem.

- **Let the referrer read the projection off the frontend, the way it reads a modport port's
  internal symbol today.** The shortest change, and it is the leak D3 names. It happens to be
  harmless for a plain modport item, because the fact it recovers is the name the signature carries
  anyway; it is not harmless here, because changing `r[3:0]` to `r[5:2]` in an interface would
  change a referrer's emitted output while changing no signature. That is silent once compiled units
  are reused.

- **Publish one member per projected point rather than a projection of one.** Then a point names a
  whole member again and nothing new crosses. Rejected because two points selecting disjoint parts
  of one name share one storage, and publishing them separately states one declaration twice -- the
  parent would drive two members where the child has one variable, and the interface's own name for
  it would be a third.

- **Publish the modport as a filtered copy of the member list.** It keeps a referrer resolving names
  against a member list and needs no new signature entity. Rejected because a published member's
  position is counted out of the published list, so a per-modport list gives one object as many
  layouts as it has modports.

## Narrowing an earlier decision

`value-projection-designator.md` D4 ends "A `ref` port still binds to a whole variable and seals to
a direct cell; ports are not projections." The clause is retained for what it was about and narrowed
for what it was not.

Its subject is the actual: D4 enumerates three constructs whose actual may be an interior, and the
sentence draws that list's boundary at a `ref` port's actual. What this entry adds is on the other
side of the port -- the declaring unit's own statement of what its connection point reaches -- which
that decision neither considered nor argued against, and which reaches no `ref` actual. The sentence
is corrected to say what it meant, so that it stops reading as a rule about the declaration side.

## Consequences

- A port expression and a modport expression are one mechanism. A construct that projects at the
  declaration reaches the same access a referrer would have written by hand, so a backend gains
  nothing to realize and refuses nothing new.
- The boundary's answer about expressions is settled in one place: a signature carries names, types,
  positions, coordinates, and folded values, and never an expression. A later construct that seems
  to need one is either a projection, a value, or out of scope.
- An interface's promise gains its views, so what a module compiled against a modport may name is
  checked where that module compiles rather than while the design elaborates.
- A referrer's emitted output stops depending on an interface declaration no signature carries,
  which is the purity property the caching a signature exists to enable rests on.

## Cross-references

- `../architecture/compilation_unit_model.md` -- invariant 12, what crossing a boundary means, which
  D3 applies to a projection.
- `../architecture/reference_resolution.md` -- the endpoint contract D6 leaves alone, and the
  connectivity shapes the rejected cell alternative matches.
- `unit-signature.md` -- what each unit kind publishes, the projection its port-part shape already
  named, and D3's purity property the rejected frontend-reading alternative breaks.
- `published-member-placement.md` -- why a position is derived on both sides, and why a projection
  cannot be.
- `value-projection-designator.md` -- the selector set D2 reuses and the sentence narrowed above.
- `shape-from-types-contents-from-expressions.md` -- the operand rule D4 stays inside.
- `interface-port-binding.md` -- the port whose members D5's views restrict.
- `specialization-identity.md` -- which modport a connection selected, already an input there, which
  is what makes D5's per-view compilation well defined.
