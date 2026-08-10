# Storage access as place formation

Date: 2026-08-03 Status: accepted

Supersedes the "Lowering observable reads and writes" section of
[value-type-concepts](value-type-concepts.md).

It also supersedes how [reference-as-data-type](reference-as-data-type.md) F3 says a reference's
access is realized -- through named wrapper methods -- while leaving F3's conclusion standing: a
reference is not a borrowed pointer, because a write through it fires the destination's update event
and a write through a raw address does not. That difference is carried by the type, which is what
each backend maps to its own realization.

## Context

A capability wrapper is a MIR type that represents a storage place instead of being a value: an
observable cell, a reference, a net's resolved value, a net driver's contribution. Its capability --
notifying waiters on change, aliasing another place, resolving contributions -- is a fact of the
type.

MIR stated that fact twice. The destination's type said "observable", and the access said it again
as a call against the wrapper's API. Two staters of one fact drift, and both drifts were realized:

- **Downstream.** A partial write interposed a mutation proxy -- a dereference of a call opening a
  scoped handle -- because that is the shape the C++ value library offers and the only node carrying
  the runtime handle to the write site. The proxy has no mechanical translation on the execution
  path, so that backend recognized the proxy and recovered the destination and handle back out of
  it. The designator the accepted model prescribes
  ([value-projection-designator](value-projection-designator.md) D1, D3) was built at every partial
  write and discarded in favour of one violating both.
- **Upstream.** Storing _into_ a wrapper (rebinding a reference at its formal) and storing _through_
  one (writing the storage it represents) produced the same node shape against the same destination
  type. Nothing structural separated them; correctness rested on which lowering path built the node,
  and on that path's author knowing not to use the general one.

The runtime handle is what made the proxy look necessary. It is not program data:
[ambient-runtime-services](ambient-runtime-services.md) fixes runtime access as an ambient
thread-local handle, reachable identically from every body kind. It has the standing of a stack
pointer, and a store does not carry one.

## Decisions

### D1. A wrapper's place and the storage it represents are distinct places, separated by dereference

A bare wrapper place denotes the wrapper: reading it yields the wrapper as a value, storing into it
rebinds the wrapper, taking its address yields the wrapper's address. A dereference of that place
denotes the storage: reading it yields the stored value, storing into it writes through the wrapper,
a designator rooted at it writes part of that storage, and passing it by reference lends that
storage to a callee.

This is the distinction every generic language draws between `p` and `*p`, and it is already LIR's
place vocabulary -- a place is a base local plus a chain of member and dereference steps. Rebinding
and writing-through become different programs written differently, so the upstream drift has no room
to exist. `mir.md` invariant 14 states this.

### D2. Reaching represented storage is never a call

An access is not an operation on an object; it is how the storage is named at all. Its entire
content is the place and the fact that the storage is wanted, both recoverable from the node and its
type. A node whose content is recoverable that way carries no semantic fact -- it carries a
realization, and the realization differs per backend: a method call in C++, a runtime-ABI call on
the execution path, a load or store against an address in LIR. Stating one of them in MIR obliges
every other consumer to decode it.

### D3. Operations on the wrapper stay calls

Installing a wrapper's declared representation and attaching a driver to a net act on the wrapper as
an object -- its representation, its lifetime, its place in the object graph. They are ordinary
calls against the library type's API, which is what `mir.md` requires of a backend storage
realization's operations.

The dividing question is what the operation acts on: the wrapper's own identity, lifetime, or graph
relationship, or the storage it represents. It is not whether the operation carries extra operands
-- an access may carry a mode or an ordering and still be an access, and a lifecycle operation may
carry none and still be one.

### D4. The access protocol comes from the place's type, through one dispatch per backend

Each backend answers three questions for a place of a given type: how a load through it is realized,
how a store through it is, and how it is lent by reference. One dispatch per MIR type variant,
sibling to type mapping, which is the same shape and the same discipline that already keeps a
runtime library's type spelling at a single site. Value emission asks that dispatch and never
inspects which wrapper kind a place has; a per-wrapper branch spread across value, assignment,
projection, and argument emission would trade one decoding problem for a scattering problem.
`backend_contract.md` invariant 4 states this.

### D5. MIR states one store; the whole store is the empty-path case

A store through a wrapper is: load the owner, update at the path, store the result back. With an
empty path the update is the value itself, so a whole store and a partial store are one operation at
different path lengths, not two protocols. A backend may still realize the whole store directly and
recover the partial store in place against an equivalent proxy
([value-projection-designator](value-projection-designator.md) D8) -- those are realizations chosen
from the place's type, and the two cases are distinguished by node structure rather than by a branch
in emission.

### D6. Reads and writes carry the rule together

The rule is "a dereference means access through". A state in which writes spell access as a
dereference and reads still spell it as a call leaves the rule true of half the accesses, a bare
wrapper place ambiguous in rvalue position, and every consumer carrying both protocols.

## Rejected alternatives

- **Keep the call encoding and give the designator a runtime-handle child.** This closes the gap
  that admitted the proxy, at the cost of making a store carry execution context as program data and
  binding MIR to a handle the runtime model deliberately made ambient. It also leaves D1's upstream
  drift untouched.
- **Discriminate rebinding from writing-through with a flag on the assignment.** A parallel
  discriminator beside the type is the shape `mir.md` invariant 7 forbids, and it puts the two
  staters back with a shorter distance between them.
- **Widen type mapping to return a store format.** Correct in substance, wrong in framing: it blurs
  what type mapping is. A named place-access dispatch is the same mechanism with its own name, and
  it gives a new backend a fixed contract to implement rather than a semantics question to
  re-litigate.
- **Model the access as a runtime effect, like `$display`.**
  [runtime-effects-as-generic-calls](runtime-effects-as-generic-calls.md) models a runtime service
  as an ordinary call with the engine handle as an argument, and that stands. A service has no
  destination; a store does, and its notification is a property of the destination's type rather
  than of the operation.

## What the superseded reasoning got wrong

The original decision required the method call to be explicit in MIR, citing `mir.md` invariant 10
-- that a backend must read a semantic fact from MIR and never re-derive one. The premise is right
and is satisfied here: the destination's type carries observability, and every backend reads it.

What does not follow is that MIR must therefore spell out the call. Invariant 10's own words forbid
MIR growing a node that carries "a backend storage realization, a runtime library's shape", and "the
access is a method call" is exactly one backend's realization. The decision cited the invariant to
justify the thing the invariant names, conflating "the backend must not re-decide a semantic fact"
with "MIR must state the realization".

The falsifier invariant 10 nominates settles it independently: could a mechanical LLVM IR backend
translate this without decisions? At the partial write the answer was no, and the pattern-matching
recovery on the execution path is the evidence.
