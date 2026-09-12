# Names a view offers

## Date

2026-09-11

## Status

Accepted. Supersedes the write half of `publishing-part-of-a-member.md` D5 and narrows its reading
of a plain port identifier; the read half of D5 stands unchanged, as does every other decision in
that entry.

## Why this decision matters

A module bound through a modport writes the names the view offers, and that is most of what
interface-based RTL does. Measured against a real design, one spelling of that write worked and
every other either refused or aborted: a nonblocking update and a compound assignment refused, and
an increment, a part-select target, an intra-assignment delay, a continuous assignment and a
procedural continuous assignment each reached an invariant break. A name the view declared `ref` was
refused outright with a rule the standard does not have.

All of them are one cause. `publishing-part-of-a-member.md` D5 publishes a port identifier as a
subroutine pair -- one evaluating the expression the view bound it to, one assigning to it -- so the
only write a referrer can state is an assignment that is exactly one call. Everything else needs
something a call cannot give: a location that outlives the statement, or a location at all.

## The tension this addresses

**The standard decides this, and it decides it twice in one clause.** LRM 25.5.4 says a modport item
written as a simple port identifier "is used as both a reference to an interface item and a port
identifier". It then says, of a written expression, that it "shall not be considered an
assignment-like context" and "shall resolve to a legal expression for the type of module port (see
23.3.3)" -- and LRM 23.3.3 makes a port connection a continuous assignment, whose sink is an lvalue.
The clause's own example draws the conclusion: a port whose expression is a constant "could not be
an output or inout".

So the clause separates three things, and the earlier entry read the first two as one:

| What the view wrote     | What the name is                  |
| ----------------------- | --------------------------------- |
| a plain identifier      | the interface item, serving twice |
| an expression, writable | storage, by 25.5.4 through 23.3.3 |
| an expression, readable | whatever the expression computes  |

**A second reach mechanism is already forbidden.** `unit-signature.md` states that a modport
"narrows which names a referrer may use and in which direction; it changes nothing about how a name
is reached", and lists "a modport treated as a runtime entity, a distinct handle type, or a second
reach mechanism" among its forbidden shapes. Routing a plain identifier through a pair of accessors
is that shape.

**And the pair had already been found insufficient once.** D5 adds a read set beside the pair
because "a call answers the first two and shows nothing to wait on". The write side is the same
finding a second time, and the remedy the first time was another field.

## The decision

### D1. An identifier the view did not rename is the interface item, and nothing about the view crosses

LRM 25.5.4 makes it the same name serving twice, so a referrer resolves it against the interface's
published members exactly as it resolves a name on an unrestricted port. The view publishes nothing
for it. What it may do with the name is the view's business and the front end's, settled while the
design elaborates; what the name _is_ was never in question.

This is not new machinery -- it is what the interface-port work already built, and what D5 subsumed
into the general case on the grounds that a plain name "is the same construct with a shorter
expression". That sentence is true of the syntax and false of the mechanism: the shorter expression
is the one the member list already answers.

### D2. A name the view renamed and admits a write to is published as the storage it designates

One part per declaration the expression joins, most significant first as LRM 23.2.2.1 orders the
names a concatenation bundles, each part a published member and the descent into it -- the same
projection D2 of the earlier entry already defines, and the same shape a module port's connection
point already uses. A name designating one declaration has one part, which is that shape with one
entry rather than a case of its own.

The name also carries its own type, because joining changes it: a concatenation is unsigned and as
wide as its parts together (LRM 11.4.12), so a view joining even one signed declaration gives the
name a type its part does not have. A referrer that reached only the part would read a different
value.

What a referrer does with this is what it does with any place it reaches. Nothing about writing it
is particular to a view, which is the whole point: the forms that refused and the forms that aborted
are not implemented here, they stop being separate questions.

### D3. A name offered only for reading stays the subroutine the interface evaluates it in

Nothing bounds that expression to an lvalue -- a constant, an arithmetic over the interface's
members, and a designator all reach the same slot -- so the one representation covering all of them
is the callable, with the members the expression reads beside it. This is D5's answer and it is
correct for exactly the case D5 argued from; the entry's error was extending it to the case the
standard had already decided.

**This is the one place a view is more than a subset, and it is worth saying plainly.**
`unit-signature.md` D1 calls a modport a direction-carrying subset of the signature that "changes
nothing about how a name is reached". A name the view computes is neither: it is a name the view
_adds_, and it is reached by a call where a member is reached by a route. The call is still inside
the family every other cross-unit reference uses -- a published callable on the published object,
which is how a package subroutine and a class method already cross -- so it is no second reach
mechanism. But D1's sentence is about the names a view narrows, and after this entry that is the
whole of what a view does except here.

### D4. Which of the two a name is, is the direction the view declared, and the direction is still on no signature

D5 declines to publish the direction, because "nothing on the referring side reads it": a use
running against the declared direction is refused while the design elaborates, and a field a
lowering does not read is a forbidden shape on a signature. That holds. The direction is not
published here either -- it decides _which alternative is present_, and the alternative is what a
lowering reads.

Every direction but `input` admits a write. `ref` gives the using module the item itself, which is a
wider access than `inout` rather than a narrower one, and reading it as read-only was the defect
that refused a legal program.

**This is not the reference shape per direction that `reference_resolution.md` forbids.** That
forbids a route mechanism chosen by which way a reference runs. Nothing here is: both answers are
reached over one route, the one the interface port already carries, and a read of a writable name
and a write of it take the same shape. What the direction decides is what the _declaration_ means,
which is a property of the name and not of any use of it -- the same kind of fact as whether a
member is a net.

## Rejected alternatives

- **Keep the pair and define each failing form in terms of it.** A compound assignment becomes a
  read then a write, an increment likewise; this is what C# does for a property, whose left-hand
  side may be "a variable, a property, or an indexer element" while `ref` assignment's may not. It
  fails here for a reason C# does not have: C# chose the pair as the language's own abstraction, so
  its users read the restriction in the specification. Ours would be an internal representation
  restricting a name IEEE 1800 guarantees is storage -- and it still cannot state a continuous
  assignment, a `force`, or a nonblocking update, each of which needs the location to outlive the
  statement.

- **Publish the pair and the projection both, using whichever fits.** One decision in two places,
  kept in step by nothing, and every consumer branches on which is present. The projection alone
  answers everything the pair answered for a writable name.

- **Publish an entry for every name a view offers, a plain one included, so nothing has to ask which
  kind it is.** The tempting one, and the strongest objection to D1: six sites ask whether the view
  defined the name, and this removes the question by making a plain identifier a projection with an
  empty path -- the degenerate form the projection already has. It is rejected because the entry
  would state what the member list already states, for every name of every view, and a referrer
  reading a plain name never reads it. That is a field admitted to a signature because it made a
  consumer uniform rather than because a lowering reads it, which `unit-signature.md` forbids in as
  many words, and the uniformity it buys is one line at each of six sites. The asking is not a
  decision re-derived, either: it is one predicate, spelled once and called.

- **Let the referrer read the modport port's internal symbol from the front end.** The shortest
  change, and the leak `publishing-part-of-a-member.md` D3 names: a referrer's emitted output would
  depend on a declaration no signature carries. That entry notes it is harmless for a plain item,
  because the fact recovered is the name the signature carries anyway -- which is D1 above, and is
  why D1 needs nothing published rather than needing this.

- **Flatten the interface into its users, as a whole-design compiler may.** Verilator resolves a
  modport port to the interface's own variable and emits a direct reference. It has no separately
  compiled unit boundary, so it can reach the declaration; `north_star.md` invariant 5 is exactly
  the condition it does not have.

## Consequences

- Every assignment form the language allows for the storage behind a name a view offers reaches it,
  with no arm per form: blocking, compound, increment, a part-select target, an intra-assignment
  delay, a nonblocking update, a continuous assignment, and a procedural continuous assignment.
- The subroutine that assigned to a name, its identity, its body, and the branch that routed an
  assignment into it are all gone. A write through a view is no longer a shape a lowering has to
  recognize.
- A view offering a name that reaches nothing inside its interface no longer stops the interface
  from compiling; it is refused where the name is used, which is what LRM 23.2.2.1 admits the
  declaration for.
- **What a view's name means comes out of the interface's published record, so a referrer reaches
  one exactly where it compiles against that interface.** An interface port gives it that, and so
  does declaring the instance; a hierarchical path descending through a module does not, because a
  module publishes no child of its own (LRM 25.10 gives that to an interface alone). The boundary is
  the signature's and not the view's -- an ordinary member of the same instance is reached over that
  path today, by name at run time, which is a reach that answers without a declaration to read.
- A signature now carries a type inside a modport, so importing one answers those types out of the
  reader's pool like every other type it carries. It is the first type to live there, and a copy
  carries an unlisted one unchanged -- right for a name or a position, wrong for a type.
- **Which storage a view's name designates is now on the signature, so editing a modport expression
  re-emits every unit bound through that view.** Under the superseded shape that edit changed only
  the interface's own body, because a referrer called a subroutine whose name did not move. The new
  cost is the dependency being real: a referrer now reaches the storage itself, so what it emits
  genuinely depends on which storage the view named. The old shape paid for that appearance of
  independence with an indirection that could not express most of the writes.
- A view offering a name it wrote no expression for publishes nothing, so a view that renames
  nothing has an empty entry. An interface whose views only narrow therefore carries no modport
  content at all, and a module bound through one compiles against the members exactly as one bound
  through a plain port does.

## Cross-references

- `publishing-part-of-a-member.md` -- D2's projection, reused unchanged; D3's rule that a signature
  carries a path and never an expression; D5, whose write half this supersedes.
- `unit-signature.md` -- the forbidden shapes this stays inside: no second reach mechanism, and no
  field a lowering does not read.
- `front-end-semantic-boundary.md` -- which verdict a refusal at this boundary may claim, and why
  refusing a `ref` name was the wrong one.
- `../architecture/north_star.md` -- invariant 5, the condition that separates this from a
  whole-design compiler's answer.
