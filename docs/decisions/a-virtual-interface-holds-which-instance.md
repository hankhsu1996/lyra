# A Virtual Interface Holds Which Instance, and Its Type Says Where Everything Is

## Date

2026-09-28

## Status

Accepted. Builds on `interface-port-binding.md` and
`calling-a-subroutine-on-another-units-object.md`, reversing neither. The plan written before it
resolved a member by name at every access, and that was never required. Revised 2026-10-30: D2's
value is a reference typed by the interface's object, so the operation that read an untyped address
back out as that object is gone.

## Why this decision matters

A virtual interface is how a class-based testbench reaches the design: a transactor holds one,
drives a bus through it, and waits on the bus's signals (LRM 25.9, whose own example is
`@(posedge bus.grant)` inside a class method). Without it, nearly every testbench that is not a
module-only one fails to build.

It is also the one construct that reaches into another unit's object from a value rather than from
the hierarchy. An interface port is bound once, where the design elaborates; a virtual interface is
assigned while the simulation runs, may hold nothing, and lives in class properties, arrays and
subroutine arguments. So the questions are what that value is, what an access through it has to find
out at run time, and what a wait on something reached through it watches.

## The tension this addresses

LRM 25.9 puts the interface's parameters and the selected modport into the virtual interface's type.
The type therefore names exactly one unit, the same unit an interface port's type names. What is
chosen at run time is only which instance of that unit.

Against that, the value has to behave as a value: it is copied, compared, held in a cell, stored in
a container and printed, and it is `null` until assigned. A machine pointer to another unit's object
is not a value of the simulation's value system. It has no default, no comparison answering a bit,
and no place in a container.

## Decisions

### D1. The type fixes every position; only the instance is chosen at run time

A member, a subroutine, a name a modport defines, and a component of an interface the instance
itself instantiates are all found in what the interface's unit published, at the positions its
signature gave them, where the referring unit compiles. That is the lookup slang performs against
the type's own interface body, and what C++ does with `p->m` on a `T*`. An access evaluates the
handle, and then reaches through it the way an interface port reaches through the instance it is
bound to.

### D2. The value is a reference to the instance, which owns nothing

What a virtual interface holds is the instance's address or nothing. The instance lives as long as
the simulation, so holding it owns nothing. In MIR the value is a reference that owns nothing, typed
by what it refers to -- the object of the interface's unit, which D1 says the type fixes -- the same
type a chandle is with `void` for its pointee (LRM 6.14). It is a value of the simulation: compared
by identity, null by default, with no unknown state, and held, copied, stored and printed as one.
That is what separates it from a machine pointer, which has none of those, and not that it holds an
address.

An instance named as a value is the reference taken to the instance: its object's address, converted
to the reference. An access dereferences the reference to the interface's object directly, as an
access through a class handle dereferences that. Neither needs an operation of its own, because the
type carries which unit's object is reached; each backend realizes the dereference of its own
reference, and a conversion between the reference and the borrowed pointer to the same pointee is
the one MIR states, both ways.

Revised 2026-10-30. The first version held the value as the chandle, a value of no stated pointee,
because "a pointer to another unit's object is not a value of the simulation's value system". That
argued from a machine pointer, which is not what the value has to be: once the reference owning
nothing is itself a value type of MIR, typed by its pointee, the value system has a value pointing
at another unit's object. The operation that read the address back out and stated which unit's
object it was existed only because the value was untyped, and it went with the chandle-typed value.

### D3. Using an empty one fails the simulation

LRM 25.9 makes use of a null virtual interface a fatal run-time error, where LRM 8.4 leaves a null
class handle indeterminate. Every access therefore guards the evaluated handle and raises a
simulation error when it holds nothing. The handle is evaluated once, whatever the access then does
with it.

### D4. A wait watches the variable in the instance the handle holds

A wait whose expression reaches a variable through a virtual interface cannot name that variable's
cell ahead of time. The wait evaluates the handle and registers on the cell it reaches, alongside
the cells the expression names directly -- the handle's own variable among them, since which
instance it holds is a value it is compared by. The member is an interface's variable and is
observable like any other; what the wait did not know in advance is only which instance, and
assigning the handle while the wait is under way moves it onto the instance now held (LRM 9.4.2).

### D5. An interface a type names is compiled whether or not it is instantiated

A declaration whose parameters no instance has is legal and can only ever hold null. Code reaching
through it still compiles against that unit's promise. So the interface every declared type names is
collected as a unit exactly as an instantiated one is. This reads declarations only, never
expression bodies, so the front end elaborates nothing it would not have.

## Rejected alternatives

- **Resolving the member by name at each access.** What the plan first said. It turns an error the
  compiler can report into one found while the design runs, and it pays a lookup on every access,
  for a position the type already fixes.
- **A borrowed pointer as the value.** It was built first, and the emitted C++ did not compile: a
  cell of it, a container of it, a comparison answering a bit, and printing each needed something
  the pointer is not. The pointer exists only at an access, for as long as the access needs it.
- **An untyped handle read back as the object by a cast.** The C++ spelling happened to do the right
  thing. The execution backend read the value's storage address as the object and crashed, because a
  cast between two unrelated types was given a meaning neither type stated. The reference typed by
  its pointee is what removes that: its dereference is the reference's own, which each backend
  realizes, and the conversion to and from the borrowed pointer of the same pointee is one every
  backend defines alike.
- **Triggering every instance of the interface on a write through any handle**, as a statically
  scheduled simulator must. A write here wakes the written cell's own waiters, so nothing is gained.
- **Refusing a declaration whose parameters no instance has.** The program is legal, and a correct
  program is not refused because a unit was never needed.

## Consequences

- Class-based transactors run on both backends: assignment from an instance named directly, through
  a port or as an array element; comparison; member reads and writes; subroutine calls; a modport's
  own names; an interface the instance instantiates, reached into, called on, or held as a value
  itself; and waits.
- A wait follows the handle: its expression is evaluated again when the handle's variable is
  written, or, for a handle held in a class property, when that object is, which is the object event
  source `object-is-an-event-source.md` defines.

## Cross-references

- `interface-port-binding.md` -- the same unit named by a type, and the same published positions,
  reached from a port.
- `calling-a-subroutine-on-another-units-object.md` -- calling what an interface published, which a
  virtual interface does with a receiver chosen at run time.
- `object-is-an-event-source.md` -- the model a wait on an object's member, and a wait that follows
  a reassigned handle, both belong to.
