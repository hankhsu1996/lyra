# A bound reference leases what it reaches through

Date: 2026-09-11, revised 2026-10-23 Status: accepted

## Context

Storage a call binds by reference has to go on existing until the call returns, whatever the program
does meanwhile to what held it:

- An element of a dynamic array, a queue or an associative array passed by reference "shall continue
  to exist within the scope of the called subroutines until they complete", though the array removes
  it or is resized first (LRM 13.5.2).
- A class property passed by reference names storage inside an object, and LRM 8.29 promises that
  "users do not need to worry about dangling references, premature deallocation", though the program
  drops every handle to the object while the call runs -- across a wait in a task, or inside a
  function that drops the last handle itself. The standard does not say whether the reference makes
  the object reachable; it says the outcome
  ([referenceable-objects-have-stable-addresses](referenceable-objects-have-stable-addresses.md)
  D4).

[container-element-storage](container-element-storage.md) and
[array-element-storage](array-element-storage.md) state the container half; none of them, nor the
object record, says who keeps the storage, or for how long.

**The bound the language gives is static and structural.** The clause says the element "shall
continue to exist within the scope of the called subroutines until they complete", and three rules
close every route by which a reference could outlive that scope:

- A reference cannot be stored: the source language has no reference data type.
- A reference cannot reach a detached branch: LRM 9.3.2 makes a `ref` formal illegal inside
  `fork ... join_any` or `join_none` unless it is `ref static`, and 13.5.2 bars `ref static` from
  being passed "elements of dynamically sized array variables".
- A reference cannot be captured by a deferred effect: LRM 6.21 bars elements of dynamically sized
  array variables from being written by nonblocking, continuous or procedural continuous assignment,
  and adds that "references to automatic variables and elements or members of dynamic variables
  shall be limited to procedural blocks".

So the requirement is a lifetime bounded by a call, and nothing has to discover at run time how long
storage must live.

## Decision

**The binding of a `ref` or `const ref` actual takes a lease on each thing its reference reaches
through that the program can let go of while the call runs, and holds each lease until the call has
returned. The reference itself stays a non-owning alias.**

**D1. What is leased is the allocation, through its own owner.** A step that crosses one of these
takes a lease on it: a property step, on the object (a share of it, as a handle in the caller's
scope is -- LRM 8.29's "class handles referring to that object in any active scope"); an element
step into a queue or an associative array, on that element's storage; an element step into a dynamic
array, on the generation its elements lie in. A path crossing several takes one lease each: `h.d[1]`
holds the object, whose holder is what a write through the reference tells (LRM 9.4.2), and the
generation. A step into a fixed-size array, a structure member, or anything a module instance holds
crosses nothing the program can let go of, and takes none.

**D2. A container honours a lease on what it keeps.** Removing a leased element or replacing a
leased generation detaches it rather than ending it -- membership ends, the value stays -- and the
last lease to let go ends the value, then frees the storage. Ending the value is deferred, not only
freeing the bytes, so a string or an aggregate element goes on reading as it was. A queue that pops
a leased element hands back a copy, since the element goes on being read.

**D3. The lifetime is the call; the realization is the call's full-expression.** The lease is held
as a value the full-expression containing the call makes, which every target already ends at that
full-expression's end on every way out -- falling off it, an effect unwinding through it, a process
killed or a block disabled while parked inside the call. That is never shorter than the call, and
may be longer where calls nest (`f(g(h.n))` holds `g`'s lease to the end of the statement), which a
later change may shorten without changing what this decides. A binding whose reference is captured
by a closure built before the call -- receiving into a typeless mailbox (LRM 15.4.9) -- holds its
leases in the frame instead, which outlasts the call.

**D4. Forwarding is not a bind.** A `ref` formal handed on to another `ref` formal takes no lease;
the outer binding's leases cover the inner call. Only an actual that names storage through one of
D1's steps takes one.

**D5. Nothing reaches the access path.** Copying a reference, reading through it, writing through
it, and a wait built from it touch no lease. Only a bind and the end of the full-expression holding
it do, and both are statically placed.

## Invariants

1. A lease is taken where a reference is bound for a call, and only there; it is held by whatever
   holds the call, never by the reference.
2. A container never ends storage a lease holds; the last lease ends it.
3. Every way out of the full-expression holding a lease releases it.

## Rejected

- **A reference that owns what it reaches into**, as an interior pointer keeps its object in Go and
  .NET. Every copy of a reference would then hold the object -- a reference handed on to another
  formal, a wait built from it, a port bound to it -- so a wait would keep an object a weak
  reference is meant to see let go (LRM 8.30), and every forward would pay a count.
  `../architecture/storage.md` invariant 7 records the same: a reference aliases.

- **The callee sharing its formals' holders on entry**, as a method body holds its own object for as
  long as it runs. The callee cannot see the generation or the element slot a reference lies in, and
  it would pay on every call, binding a module variable included, deciding at run time what the
  binding knew when it was compiled.

- **A registry of the addresses calls hold, which every container's free paths consult.** An address
  is not an allocation: what a reference names may lie deep inside one, and what must stay is the
  allocation. A registry also defers freeing the bytes and not ending the value unless every free
  path carries the destructor too, makes every container mutation consult runtime-wide state, and
  stands in for a count the allocation can carry itself.

- **A new compiler concept for the call's extent**, which this record first proposed: a scope a call
  opens before its actuals bind and closes when it completes. A full-expression already holds the
  values made while evaluating it to its end in both targets, which is the bound wanted, so no new
  concept is needed.

- **Registering at block granularity.** Conservative rather than wrong, and rejected because the
  over-retention is unbounded: a loop that binds and detaches once per iteration keeps every
  detached element until the block ends. A full-expression ends every iteration.

- **Reference counting the storage at every copy.** What
  [container-element-storage](container-element-storage.md) rejected. A lease is counted at a bind
  and its release, never at a copy, a read or a write.

## Consequences

- **A slot and a generation carry a count of the leases on them.** A queue's or an associative
  array's element is storage of its own already, so the count lies beside it; a dynamic array has
  one per generation.

- **What a bind costs follows what it reaches through.** A bind naming a module variable, a local or
  a fixed-size array pays nothing; a bind through an object pays a share of it; one into a container
  pays a count on the element or the generation. Every element of a queue or an associative array,
  and every dynamic array generation, carries the count's storage whether or not anything binds it,
  and every removal reads it.

- **A suspending callee is covered without a special case.** A task call does not return until the
  task completes, so the full-expression spans the suspension, and a process ended while parked ends
  the full-expression.

## Cross-references

- [container-element-storage](container-element-storage.md) -- detachment for a queue and an
  associative array, which a lease realizes.
- [array-element-storage](array-element-storage.md) -- the retained generation.
- [referenceable-objects-have-stable-addresses](referenceable-objects-have-stable-addresses.md) --
  the object half: what keeps the object is the binding's share.
- [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) -- the reference this leaves
  untouched.
- `../architecture/storage.md` -- membership, position, identity and duration as four properties, of
  which this decision implements the last.
- `../architecture/lifetime.md` -- the regimes, and the rule that every exit owes the endings a
  scope opened.
