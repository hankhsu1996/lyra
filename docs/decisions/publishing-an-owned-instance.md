# Publishing an instance a unit owns

## Date

2026-09-06

## Status

Accepted. Extends `interface-port-binding.md` D3 with a third kind of published declaration, widens
the readable-signature set `unit-signature.md` D3 bounds, and corrects one Forbidden Shape of
`../architecture/reference_resolution.md` whose wording stands in for the classifier its own
invariant states. It removes the refusal `calling-a-subroutine-on-another-units-object.md` D3 ends
on by changing that refusal's stated premise; nothing is reversed.

## Why this decision matters

An interface can contain smaller interfaces and be passed through ports (LRM 25.3), and access to
the objects an interface declares is available however the interface is reached, port connection
included (LRM 25.10). So a module bound to the outer one reaches the inner instance's members and
enables its subroutines through that port. That reach is what makes a bundle of bundles a bundle:
without it an interface composes only by copying its contents up a level.

It refuses today, and the cause is one line of what a unit publishes. An interface publishes what
holds a cell; an instance holds none, so the name stops at the port. The refusal is not in the route
and not in either backend -- the emitted access is one more `->` -- which is why the answer is about
what a signature says rather than about how a reference is realized.

## The tension this addresses

Three constraints meet here and none of them is about the reach itself.

- **A published member's storage has been a cell, and this one is an object.** The port member
  already broke that once, and the two are the same fact from the referrer's side: a pointer to
  another unit's object. What differs is who fills it.
- **The referrer needs two promises, and its own declarations name only one unit.** Where `mark`
  sits is Inner's promise, and nothing Leaf declares mentions Inner. The set of signatures a
  lowering may read is fixed before any body lowers, from declarations, which is exactly the input
  that does not name it.
- **Publishing changes a route that already works.** A hierarchical name into the inner instance
  resolves by name today, which is legitimate only while nothing published the name. Publishing it
  makes that route an opaque segment for a published name, which the reference contract forbids.

## What separate compilation does elsewhere

No SystemVerilog tool faces this question. A simulator that elaborates the whole design flattens the
hierarchy first, so reaching the inner instance is a pointer chase on the elaborated tree and no
promise is involved. The question exists only where units compile apart, and three answers are in
use there.

- **C++** puts the member in the outer's header with the inner's type, so a translation unit that
  includes the outer pulls the inner whether or not it names one. The dependency is eager, and its
  cost is the classic one.
- **Ada, Modula, Rust** have the author declare the dependency up front -- a `with` clause, a
  manifest entry -- so the compiler never discovers one and never over-approximates.
- **The JVM** records in a class file exactly the classes its code names and resolves them lazily.
  The dependency is a product of compiling the body, not an input to it.

Lyra discovers rather than being told, and its artifact's dependency list is already the third
answer: a referrer names another unit's object only where it reaches into one, and that record is
what an emitted artifact includes. What is not the third answer is the _bound_ on what a lowering
may read, which is predicted from declarations before any body lowers. D3 is where the two meet.

## Decisions

### D1. An interface publishes the interfaces it instantiates

LRM 25.3 makes a nested interface part of the hierarchical structure the construct provides, and LRM
25.10 makes the objects an interface declares reachable through a port connection. A nested instance
is therefore on the surface the port reaches, on the same footing as a variable: the port names the
interface's scope, and this is one of the things in it.

Nothing else joins the set, and each exclusion has its own reason rather than a shared one.

- A module can be neither declared nor instantiated in an interface (LRM 25.3), so the only child
  instance an interface has is an interface instance. There is no case to split on.
- A generate block is a scope of this unit rather than an object of another one. Publishing it would
  put this unit's own internal scope class on its promise, which is the thing a signature exists to
  keep inside. A name continuing through one is refused with its own reason.
- A module publishes its ports, so it publishes no child at all. What this entry adds is available
  to the unit kind whose promise is its members.

The member carries the two facts every published member carries: a type naming the unit whose
instances belong there, with the multiplicity a range gives it, and a storage kind saying it holds a
borrowed reference to an object. Those are the same two an interface port's member carries, because
from the referrer's side the two members are one thing. Who fills the pointer -- the parent's
connection, or this unit's own construction -- is not a fact any referrer reads, so it does not
reach the promise.

Deriving this reads the instance the unit itself declares, not another unit's signature, so every
unit's signature is still derivable at once and in any order.

### D2. Continuing past a published member is the step form of ending on one

A route already ends on a member another unit published, named against that unit's signature and
carried as the position that signature gave it. Reaching _through_ such a member is the same act
with the same identity: the same record, the same position, and a pointer to the next unit's object
instead of a cell. So the route gains one step form and no new identity kind, and the name is
resolved once, where the referrer compiles.

The step carries a position and never a name. A name is the identity only where a route passes a
signature, and this step does not pass one -- it lands on what a signature promised.

The two step forms that produce a pointer to another unit's object stay as they are, because their
identity is this unit's own declaration rather than another unit's promise: a step onto a child this
unit builds, and a step through a port this unit declares. A route with none of the new steps is
what it was, so the general case covers the old one with no branch.

### D3. A unit may read the signatures it can reach, not only those it declares

The set of signatures handed to a unit's lowering is a bound: it is what makes reading a signature
the unit has no business reading unspellable. `unit-signature.md` D3's purity theorem is stated over
what a lowering _consumes_, and holds under any bound; the narrowing buys the guarantee that a unit
cannot consume a signature it never declared a dependency on.

Reaching through a published object is a legitimate reach whose target the referrer's declarations
do not name. So the bound is the set the referrer can legitimately reach: the units it declares,
closed under the units their signatures name as published objects. That is exactly what a typed
reach can ask for and nothing else, which makes the bound tighter as a statement than "what it
declared" even though it admits more names.

**This widens what may be read, never what is recorded.** A referrer still names another unit's
object only where it reaches into one, which is `interface-port-binding.md` D6, and that record is
what an artifact's dependencies and its re-emission follow. The alternative D6 rejects -- recording
every unit named by a published member of an object the referrer holds -- stays rejected, and this
is not it: nothing here creates a record, and a unit that never reaches past the port names no unit
it did not before.

The closure terminates because it is computed over the units of an acyclic instance tree.

### D4. Publishing a name changes every route to it, so both change together

A name reaching the inner instance from the scope that owns the outer one resolves by name during
elaboration today. That is what `unit-signature.md` D5 is for, and it is correct exactly while the
target published nothing. Once the interface publishes the instance, the same route is an opaque
segment standing in for a published name, which `../architecture/reference_resolution.md` names as a
forbidden shape and which discards the check the signature exists to give.

So the two routes are not two features. Publishing the member is what makes the reach typed, and
every route that reaches it produces the same step -- the one through a port, and the one that
descends from the scope that owns the interface.

That doc's Forbidden Shapes names "a child it owns" among the declarations a typed segment may not
name. Its invariant 2 already states the classifier as whether the referrer has a declaration to
compile against, and ownership was standing in for publication because until now the two agreed on
every child. They stop agreeing here, and the classifier is the one that holds.

### D5. What a published member is called is one function of the member

Both sides of the boundary spell the member: the declaring unit emits the field, and the referrer
emits the access. A unit names a child's handle by a rule of its own and a published member by its
own name, which are two rules that never had to answer for one member before. Publishing a child
makes them, so the name is the member's own and is computed once.

This is `published-member-placement.md` D4's reasoning at the level of the name rather than the
cell: when two layers must agree on something either can compute, one shared function makes
disagreement unspellable, and a value carried between them only makes it invisible.

## Rejected alternatives

- **Carry the inner unit's published set inline on the outer's signature**, the way a signature
  carries its own type pool. It makes the outer's promise self-contained and the referrer needs one
  signature. Rejected because it publishes one fact twice with nothing making the copies agree, and
  because a change to the inner unit's members would then change the outer's signature and re-emit
  every referrer of the outer -- including the ones that never reach past it, which is the
  dependency the demand-driven record exists to avoid.

- **Record the inner object where the outer's signature is consumed.** The short fix, and what a
  transitive import does by default. Rejected under `interface-port-binding.md` D6: the record is
  what an artifact's dependency list is built from, so this grows every referrer's dependencies with
  nesting depth for reaches it does not make.

- **Discover the reach by walking the unit's bodies in the declaration pass**, so the bound is exact
  rather than a closure. Rejected because it decides in a pass with less information what the later
  pass already discovers where the reach happens: the record is made on reach either way, and what
  the earlier pass owes is a bound, not an answer.

- **Hand every signature in the design to every unit's lowering.** The question disappears. Rejected
  because so does the property that a unit cannot read a signature unrelated to anything it
  declares, which is the whole of what the narrowed set buys.

- **Let the reach fall to the by-name form when the inner signature is absent.** It runs today's
  machinery and needs no bound at all. Rejected because it is the forbidden shape stated twice over:
  a published name resolved while the design elaborates, with an unchecked cast where the
  compile-time check belonged. It also fails silently, since "not declared as a dependency" and "the
  unit published nothing" reach the referrer as one answer.

- **Publish only the nested instances some port actually reaches.** Smaller objects. Rejected for
  the reason the same shape was rejected for members: a published member's position is counted out
  of the published list, so a list that depends on who is looking gives two referrers two layouts of
  one object.

- **Give the step the member's name and resolve it at elaboration.** It needs no record on the step.
  Rejected because a textual name is the identity of a segment only where the route passes a
  signature; this step lands on what a signature promised, and naming it there reintroduces the
  lookup one step further along.

- **Merge the child-instance declaration with the interface-port declaration**, since the two now
  publish the same member. Rejected because what separates them is what every consumer of those
  arenas acts on: one is built and freed by this scope and one is bound by the parent. They agree on
  what they promise and disagree on what the unit does with them, which is a shared promise over two
  declarations rather than one declaration.

## Correcting an earlier statement

`calling-a-subroutine-on-another-units-object.md` D3 closes: "A path that continues past a port into
what the interface owns is refused, because the port's signature promised the interface and not a
route through it."

The reason is right, and its premise is what this entry changes. The signature now promises the
instances the interface owns, so a path continuing into one continues into something promised, and
it resolves for the same reason a member access through the port does. What stays refused is a path
continuing past what the interface did not publish -- a generate block it declares, or a name inside
one.

Two Forbidden Shapes are corrected the same way, and for the same reason: `unit-signature.md` and
`../architecture/reference_resolution.md` both name "a child it owns" among the declarations a
referrer may not reach typed. Ownership was standing in for publication because until now the two
agreed on every child, and `reference_resolution.md` invariant 2 already states the classifier as
publication. The invariant is the one that holds; both lists are reworded to say so.

## Consequences

- An interface composes: a bundle built out of smaller bundles is reached through a port at any
  depth, and what it reaches is the inner instance of whichever outer instance the port was bound
  to, because the route is the port's and not a name on the elaborated hierarchy.
- A subroutine of a nested interface is enabled through the port, since the receiver is a route
  ending at that instance and the callable is a name on what its unit published -- which is what
  `calling-a-subroutine-on-another-units-object.md` D2 already says, reached one step further along.
- A hierarchical name into a nested interface from the scope that owns the outer instance stops
  being a by-name lookup and becomes typed navigation, so it is checked where the referrer compiles.
- Both backends realize the reach as one more member step, so neither gains a case and the two
  refuse the same designs.
- What an interface publishes is now three kinds of declaration, and what a module publishes is
  still its ports. The difference is the construct's purpose rather than a rule about children.

## Cross-references

- `unit-signature.md` -- what each unit kind publishes, the two ways into another unit, and the
  purity theorem D3 here reads as a bound rather than a set.
- `interface-port-binding.md` -- D3 there is the set this entry extends, D5 the arena question, and
  D6 the demand-driven record this entry leaves alone.
- `published-member-placement.md` -- the position both sides count out of the signature, and D4's
  one-shared-function reasoning that D5 here applies to the member's name.
- `calling-a-subroutine-on-another-units-object.md` -- the receiver route and the refusal whose
  premise this changes.
- `publishing-part-of-a-member.md` -- what a connection point names when it is not a whole
  declaration, which is the other half of what a signature says about a member.
- `instance-array-multiplicity.md` -- a declaration standing for several objects is one member whose
  type carries the multiplicity, which is what a nested instance array is.
- `../architecture/reference_resolution.md` -- the per-segment classifier D2 and D4 rest on, and the
  Forbidden Shape D4 corrects.
- `../architecture/compilation_unit_model.md` -- invariant 8's "its ports and its members" for an
  interface, and invariant 11's emission purity.
