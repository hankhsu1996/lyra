# Binding an Interface Port

## Date

2026-08-28

## Status

Accepted. Extends `published-member-placement.md` D4 with a fourth storage kind and widens the input
`specialization-identity.md` names; reverses neither.

## Why this decision matters

An interface exists to be written against. A module names `b.data`, and the interface instance the
connection bound to `b` supplies it -- that access is the whole reason the construct exists. It is
also the first place in the language where a unit reaches inside an object it neither owns nor
built: an ordinary port carries a value across the boundary, while an interface port carries the
boundary itself.

Three questions have to be answered together, because each constrains the others. What is the port's
declared type, given that it has to survive being read by a unit that has none of the publisher's
arenas? What does the member hold, given that both sides build the same object independently? And
what is a module bound to two different interfaces, given that the two compile against different
types at different positions?

## The tension this addresses

A published member's type travels on the signature, and a signature is read where the publishing
unit's arenas are not. That rules out naming the interface by any identity local to the publisher --
the import that carries a type across has a type pool and nothing else, so it cannot mint one in the
destination either. The only identity that survives is the same one a cross-unit class reference
uses: a name both sides compute.

Against that, a name is not enough for the referrer's own lowering, which has to reach a _position_
in the interface's object. That position comes from the record the referrer keeps of what the
interface published -- so the type has to name a unit, and the declaration has to name the record,
and the two have to be the same interface without either restating the other.

## Decisions

### D1. The port's declared type names a unit, by name, and nothing else

The type of an interface port is the object an instance of the named unit is. There is no local form
of it, the way a class reference has one: an interface port always names another unit, so a variant
whose second arm can never be taken would be a case that exists only to be unreachable. The name is
the identity from everywhere, so the type crosses a signature unchanged and the import that carries
it is the identity function.

The referrer resolves that name against its own record of the objects it compiled against, which it
made when it consumed the interface's signature. Reaching another unit's object is what declares the
dependency on it, so a type naming a unit and a record of that unit's object are produced by the
same act and cannot describe different interfaces.

### D2. The port member is storage that holds no object: a borrowed reference the parent fills

A published member states which storage it is, and one function turns that into the cell the member
holds. An interface port is a fourth answer: it holds neither a variable's observable cell, nor a
net's resolved cell, nor a reference aliasing a connected variable, but a borrowed reference to an
object some enclosing scope owns.

That keeps the factorization the other three have -- the member's type is what it stands for, its
storage is how it holds it -- rather than folding the indirection into the type, which would leave
one storage kind meaning "add nothing" and put the same fact in two places.

The lifecycle is the one a `ref` port already has (LRM 23.3.3.2): the child declares the name, owns
nothing behind it, and the parent binds it once during elaboration, after the object tree is built
and before anything observes it. What differs is only the referent -- an object rather than a cell
-- so the connection installs no driver, arms no reactive edge, and waits on nothing.

### D3. An interface publishes its whole declared surface; a module publishes its ports

What a unit publishes is what another unit may reach by a name resolved where it compiles. For a
module that is its ports: everything else is reached, if at all, by a hierarchical name the runtime
answers while the design elaborates. For an interface it is every net and variable it declares,
because an interface port names the interface's scope and the members in it are exactly what the
port is for (LRM 25.3.2).

This is not a special case bolted onto the signature. It is the same rule -- publish what another
unit compiles against -- applied to a construct whose promise happens to be its members rather than
its ports, which is what `unit-signature.md` already states an interface's signature is.

### D4. A module with an interface port is generic over it, and the connection is where the argument is deduced

`module Leaf (Bus b)` names the interface _definition_, not a specialization of it, and the language
gives no way to name one: `Bus #(8) b` in a port list is rejected, because there `Bus` is a
definition being used as a type. So the module's source does not fix the width of anything reached
through `b`. What fixes it is the connection.

That makes the module a family, and the port's interface an argument of it -- one that is deduced at
the instantiation site rather than written in the header. It is not a base class and `Bus #(8)` is
not derived from `Bus`: two specializations of one interface share a name and a source text and
nothing else, and neither is substitutable for the other.

The argument is the interface's own specialization identity, never its parameter bindings. Those
belong to the interface, which already classifies them; naming them here would re-run that
classification in a unit that cannot read the interface's body, and would stop matching the moment
the interface's own sharing improved. Naming the identity composes instead: the interface gets
better at sharing, and every module bound to it inherits that with nothing changed on this side.

Leaving it out is not a missing feature but a collision: a module with no parameters and two
different interface connections would name one unit twice, and one of the two compiled bodies would
silently stand in for the other.

Reading the connection to name the unit reads no other unit's signature -- it reads a name, computed
by the same function the parent uses for the same instance -- so every unit's signature is still
derivable at once, in any order. The instance tree is acyclic, so the recursion through an interface
that itself has an interface port terminates.

### D5. The port declaration is its own kind, in neither the data-object arena nor the child arena

A scope's declarations are grouped by what the lowering does with them, and an interface port
matches neither existing group. It is not a data object -- it holds no value and no cell, and LRM
6.5 puts variables and nets in that group and nothing else. It is not an owned child -- the scope
neither constructs it nor frees it, which is the single fact every consumer of that arena acts on.

It is a third thing: a member the scope declares, does not build, and is dotted into. So it has its
own arena, and the list fixing where published members sit names either kind, since an interface
port is always published and always sits in that prefix.

### D6. A referrer holds a published member's representation, and its identity only where it reaches through

A unit that reaches a member on another unit's object holds what that unit published, which is what
having consumed its signature means. A published member it never reaches through is different: what
it needs from that member is enough to place the ones after it, and for a port standing for an
object that is a pointer -- one machine word, whatever it points at.

So the object a pointer points at is named only where the referrer reaches into it. Naming it
otherwise would declare a dependency the referrer's own output does not have: its artifact would
pull one it never references, and a change to that unit would re-emit it while changing nothing it
emits. This is the same rule the route already follows one level up -- what a unit compiles against
is what it reaches -- read at the member level.

## Rejected alternatives

- **Make the type the handle rather than the object** -- `UnitHandleType` lowering straight to a
  borrowed pointer. Rejected because the storage kind would still have to exist for the member's
  cell to be computed, and it would then be the one arm meaning "wrap in nothing"; the indirection
  would be stated twice, once in the type and once by the arm that declines to add it.

- **Name the interface by the referrer's record of it, not by unit name.** Shorter inside the unit,
  and impossible across the boundary: the record is an arena position of one unit, and the import
  that carries a type into another unit's pool has no way to make one there. The name is the only
  identity a signature can carry, which is the same conclusion the cross-unit class reference
  reached.

- **Reach the port's members by name at run time**, the way a hierarchical name past a signature is
  reached. It works, and it needs none of this. Rejected because the interface published those names
  and the module consumed that signature to compile: the query buys back an independence the
  declared dependency already spent, and replaces a check where the module compiles with an
  unchecked cast while the design elaborates.

- **Publish an interface's members lazily, only those some port reaches.** Smaller objects. Rejected
  because a published member's position is counted out of the published list, so a list that depends
  on who is looking gives two referrers two different layouts of one object.

- **Give a module one unit per interface _instance_ rather than per interface specialization.** Then
  no two connections share a compiled body and the identity question disappears. Rejected because it
  multiplies artifacts by instance count for nothing: two instances of one interface specialization
  present the same types at the same positions, so one body serves both -- which is also where the
  frontend splits, so the two agree with no adjustment.

- **Record every unit named by a published member of an object the referrer holds.** It is the short
  fix for the alternative to D6, and it is what a transitive import does by default. Rejected
  because it grows the referrer's dependencies with nesting depth, and every one of them is a
  dependency its emitted artifact does not have.

- **Treat an interface port as a data object with a handle type.** One arena, no new declaration
  kind. Rejected under D5: every consumer of that arena installs a cell, runs an initializer, or
  seeds a net, and an interface port wants none of the three, so the arm would exist to be skipped
  by each of them in turn.

- **Resolve a name on the port with a lookup of its own, beside the one that resolves a name on an
  instance.** It is the shorter thing to write, because the reference site already holds the port
  and the record it names. Rejected because the two are the same act -- resolving a name against a
  signature to get a position -- and stating it twice puts two answers where the signature promised
  one: a second place that decides what a published member's type and storage are, and a second
  reading of what "not published" means. What actually differs between the two is only which step
  produced the pointer to the other unit's object, which is a fact about the route and not about the
  resolution.

## Consequences

- A member reached through an interface port is an ordinary positional member access at every layer
  below the one that resolved the name, so both backends realize it and refuse the same designs.
- An interface's object has a stable published prefix like any other unit's, and a module bound to
  it is compiled once per interface specialization rather than once per instance.
- A module's ports and the interfaces its ports carry are the two things a parent selects, and both
  now feed one identity, so a change to either re-emits exactly the instantiations that chose it.
- An interface port needs no MIR vocabulary of its own: it is a borrowed pointer to another unit's
  object, which is what an instance member's handle already is, differing only in who owns the
  pointee.
- A name reached through a port and a name reached through an instance differ only in which step
  produced that pointer. Both end at a member resolved by name against the signature, through one
  function, so the port adds a step and nothing else -- there is no second name resolution and no
  second statement of what a published member's type or storage is.
- Which names a port reaches follows from what the interface publishes, so what it does not publish
  is decided in one place. A parameter named on a port folds to its value the way it does on any
  other hierarchical name, because the port changes how the target is reached and not what it is.

## Cross-references

- `unit-signature.md` -- what each unit kind publishes, including the interface's members, and the
  by-name resolution this rests on.
- `published-member-placement.md` -- how a published member's position is computed on both sides; D2
  here adds the fourth storage kind its D4 maps to a cell.
- `specialization-identity.md` -- the identity function D4 widens, and the reason producer and
  consumer can compute it independently.
- `reference-as-data-type.md` -- the `ref` port's fill-once-at-Resolve lifecycle, which the
  interface port's binding follows over an object instead of a cell.
- `hierarchical-reference-routing.md` -- the route vocabulary a member access through the port
  extends by one step kind.
