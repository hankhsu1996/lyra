# Calling a Subroutine on Another Unit's Object

## Date

2026-09-01

## Status

Accepted. Extends `unit-signature.md` D1 with what an interface publishes beyond its members, and
answers `hierarchical-reference-routing.md` D5 for the callable target it named as a candidate;
reverses neither.

## Why this decision matters

An interface exists to be written against, and LRM 25.7 puts its tasks and functions on that
surface: a module drives a bus by enabling `b.Write(...)` rather than by naming any of the bus's
wires. That is the same construct a `modport import` restricts, the same one a plain port offers
whole, and the same one a scope owning the instance reaches by hierarchical name.

Two facts have to meet for such a call, and neither was in place. The caller needs a **route to the
object** the subroutine acts on, and a **promise from the declaring unit** that the name is callable
on it. Getting either wrong is expensive in a way that does not surface immediately: a route that
names the instance bakes an elaboration fact into a per-specialization artifact, and a promise
recovered from the frontend's whole-design graph is a fact outside the query graph that no
fingerprint covers.

## The tension this addresses

The frontend elaborates per instance and Lyra compiles per specialization, so one call carries two
facts that must not be confused.

What a call's target states is which instance: `p.Write(...)` in a module bound to one bus and
`q.Write(...)` in the same module bound to another resolve to two different declarations. That is
the fact a receiver is recovered from -- and also the fact that must not be carried through, because
a module bound to two instances of one interface is one specialization compiled once.

What survives compiling once is how the name reached the object rather than what it landed on, and
the frontend states that: a call carries the hierarchical reference a value read has always carried,
so a port reach and a hierarchical reach stay apart even where they land together. That is a
property of the frontend this repository pins rather than of the released one, and it is why the
decisions below can be about routes at all.

## Decisions

### D1. A unit publishes its callables, and an interface's surface includes its subroutines

An interface publishes every net and variable it declares because a port names the interface's scope
and those members are what the port is for. A subroutine it declares is on the same footing: LRM
25.7 makes it callable through a port, so a referrer compiles against it exactly as it compiles
against a member.

What a published callable states is what a caller must know beyond the name: the call protocol (a
task enable suspends its caller until completion, LRM 13.3), the result its completion yields, and
per formal a direction and a type, which is what shapes the arguments the call passes and the
writeback it performs. A referrer takes that into its own record of the object when it consumes the
signature, so nothing below that consumption reads a signature or a type it does not own.

A module publishes no subroutine, because a module publishes its ports. That is not an omission:
what a module's subroutine is reached by, if anything, is a hierarchical name the target never
promised, which is D4.

### D2. The receiver is a route ending at the object, which is an endpoint that already exists

The route a call needs ends at the interface instance, not at the callable. Reaching an object
across an instance boundary is what an interface port connection already does, and what an endpoint
inherits is the access protocol of the target it reaches -- so the callable is reached by naming it
on that object, the way a member is reached by naming it there.

The receiver therefore seals like every other cross-instance reference: the route runs once during
resolve, the endpoint holds a pointer to the object, and the call reads it with no traversal. The
receiver leads the arguments, because a callable's receiver is its first ordinary parameter.

**No new sealed-endpoint category is introduced, and none is needed.** The candidate that stood open
was a route whose endpoint is a callable; what this shows is that a call across an instance boundary
does not have one. The object is the endpoint, and the callable is a name on what that object's unit
published.

### D3. The route follows how the name reached the object, not what it resolved to

A module may reach one interface instance two ways at once, and the two do not mean the same thing.
Naming a port reaches whatever that port was bound to, and follows the binding into every
instantiation; naming the instance hierarchically reaches what it names, in every instantiation
alike. In the one instance a lowering happens to see, both land on the same object, so what the name
resolved to cannot tell them apart -- and taking the port whenever a port is bound to the resolved
instance would silently make a hierarchical name follow a binding it never mentioned.

What separates them is the path the name travelled, which the front end resolves and states. So the
port is the route where the path went through a port, and the walk on the elaborated hierarchy is
the route otherwise, classified per segment like any other: typed where this unit declares what it
lands on, by name where it does not. A call is read the same way a member read is, from the same
stated path, so one rule covers both and neither recovers the distinction from source text.

A path that continues past a port into what the interface owns is refused, because the port's
signature promised the interface and not a route through it.

### D4. A reference is identified by its route, not by what it ends at

Two references that navigate the same way to the same target are one sealed endpoint. Two that reach
one target differently are two endpoints, even though a single instance shows them landing together
-- which is exactly the case D3 separates, so sharing an endpoint between them would undo it: the
second reference would follow the first's binding.

This is what identifies an endpoint at all. A target alone does not: it is the same object down
either route, and the routes are the whole of what differs. It holds for a value read and for a call
equally, because both are references, and neither the count of endpoints nor the storage they occupy
is a reason to conflate two.

### D5. A callable the declaring unit did not publish has nothing to compile against

A subroutine a module declares is reached, if at all, by a hierarchical name the module never
promised (LRM 23.6). Such a name has no signature to resolve against, so it resolves by name while
the design elaborates -- and a by-name lookup that answers with a callable is a runtime capability
that does not exist here. The call is refused, under a reason that names what is missing rather than
the construct that hit it.

That leaves the boundary exactly where the signature draws it: what a unit published is callable on
its object, and what it did not is not reachable yet at all.

### D6. An instance method and a type-associated one are different targets

A method of another unit that takes a receiver and one that does not are two targets, not one target
a consumer tells apart. The distinction is the callee's own declaration (LRM 8.10), read where the
callee is minted and carried down, because the alternative is a backend recovering it from whether
the leading argument happens to be pointer-typed -- which is a semantic decision re-derived below
the layer that knew it, and answers wrongly for a type-associated method whose first argument is a
handle.

## Rejected alternatives

- **Name the instance the frontend resolved to.** The receiver falls out with no derivation at all,
  and it is wrong for the reason the specialization model exists: a module with an interface port is
  generic over that interface, and two instances bound to different objects share one artifact. The
  route would be correct for whichever instance happened to be lowered and silently wrong for the
  rest.

- **Add a sealed-endpoint category whose target is a callable.** This is what the open target-family
  entry anticipated, and the derivation says it is not needed for a published callable: the route
  ends at the object either way, and the callable is a name resolved against a signature. A category
  admitted here would exist to describe the same route twice.

- **Publish nothing and resolve the callable by name during elaboration.** It works, needs no
  signature change, and is what the by-name escape hatch is for. Rejected because the interface
  published the name and the referrer consumed that signature to compile: the lookup buys back an
  independence the declared dependency has already spent, and replaces a check where the module
  compiles with one that fails while the design elaborates.

- **Read the callee's protocol and formals from the frontend's declaration, as a call to a package
  callable does today.** Shorter, and it is the existing shape. Rejected because it is a fact about
  another unit reached around that unit's signature: it holds only while one whole-design graph is
  in memory, and the day compiled units are reused it is a stale result rather than an error. The
  package call has the same leak and is not fixed here; what this entry fixes is that the new path
  does not add a second instance of it.

- **Evaluate the receiver route at each call instead of sealing it.** A route through a port is one
  member read, so sealing looks like a member that buys nothing. Rejected because the same route may
  cross a boundary the runtime answers by name, and an access that performs a by-name lookup on the
  simulation path is the shape the reference model exists to remove. One shape for every route costs
  a pointer in the cheap case and is correct in the expensive one.

- **Carry the receiver as an ordinary argument the caller writes.** Then no receiver concept is
  needed at the callee at all. Rejected because it is the receiver every callable already has: a
  body reaches its object through its first parameter, and a second way to supply one would make two
  kinds of call out of one.

- **Identify a sealed endpoint by the declaration it ends at.** Cheaper to look up than a route, and
  it was the shape in place. Rejected under D4: it makes two references that reach one target
  differently share an endpoint, so whichever was lowered second silently follows the first's route.
  A hierarchical name beside an interface port is where that shows, and a member read reaches it as
  readily as a call does.

## Consequences

- A subroutine an interface declares is callable through a port, through a modport that imports it,
  and by hierarchical name from a scope that owns the instance -- three spellings of one call, told
  apart nowhere below the frontend.
- A modport `import` needs nothing of its own. It narrows which names a referrer may use, which the
  front end enforces where the referrer compiles, so what reaches the lowering is the same call it
  would have made through an unrestricted port.
- The signature carries a second kind of promise, and the placement rule is untouched by it: a
  callable is reached by the symbol its unit emits it under, so it takes no position in the object.
- Both backends realize the call by naming that symbol -- one composes it from the unit, the class,
  and the method, and the other lets the target language resolve the same three names -- so the two
  refuse the same designs for the same reasons.
- A subroutine reached across a boundary the target unit published nothing across is refused with
  its own reason, so the record says which of the two walls a case is held at.
- A scope holds one endpoint per distinct route rather than per target reached, so a module that
  names one interface both through its port and by hierarchical name holds two, and each follows the
  name that produced it.

## Cross-references

- `unit-signature.md` -- what each unit kind publishes, and the two ways a reference reaches into
  another unit, of which D1 and D4 are the two halves applied to a callable.
- `interface-port-binding.md` -- the port member D3 routes through, and why the interface a port
  carries is part of the module's specialization identity.
- `published-member-placement.md` -- the record a referrer keeps of what a unit promised, which is
  where a published callable is read from below the consuming pass.
- `hierarchical-reference-routing.md` -- D5 there leaves the target family open and names
  hierarchical callable dispatch a candidate; D2 here answers it for a published callable, and D4
  here states what identifies one of that entry's sealed endpoints.
- `callable-receiver.md` -- the receiver as the callable's first ordinary parameter, which is what
  makes the route's endpoint the whole of what a cross-object call adds.
- `unified-callable-model.md` -- the completion payload a task's `output` formals ride back in,
  unchanged across the unit boundary.
