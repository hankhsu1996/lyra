# A Virtual Interface Holds Which Instance, and Its Type Says Where Everything Is

## Date

2026-09-28

## Status

Accepted. Builds on `interface-port-binding.md` and
`calling-a-subroutine-on-another-units-object.md`, reversing neither. The plan written before it
resolved a member by name at every access, and that was never required.

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
a container and printed, and it is `null` until assigned. A pointer to another unit's object is not
a value of the simulation's value system. It has no default, no comparison answering a bit, and no
place in a container.

## Decisions

### D1. The type fixes every position; only the instance is chosen at run time

A member, a subroutine, a name a modport defines, and a component of an interface the instance
itself instantiates are all found in what the interface's unit published, at the positions its
signature gave them, where the referring unit compiles. That is the lookup slang performs against
the type's own interface body, and what C++ does with `p->m` on a `T*`. An access evaluates the
handle, and then reaches through it the way an interface port reaches through the instance it is
bound to.

### D2. The value is which instance it holds

What a virtual interface holds is the instance's address or nothing. The instance lives as long as
the simulation, so holding it owns nothing. That value is exactly the runtime's pointer-identity
value, the chandle: compared by identity, null by default, with no unknown state. In MIR it is that
value, and it is held, copied, compared, stored and printed as one. An instance named as a value
builds one from the instance's address. An access reads the address back out and states which unit's
object it is. Both are operations MIR states, never a reinterpretation left to a backend: one
backend holds such a value by the address of its storage and the other as an object, so a cast would
mean something different on each.

### D3. Using an empty one fails the simulation

LRM 25.9 makes use of a null virtual interface a fatal run-time error, where LRM 8.4 leaves a null
class handle indeterminate. Every access therefore guards the evaluated handle and raises a
simulation error when it holds nothing. The handle is evaluated once, whatever the access then does
with it.

### D4. A wait watches the variable in the instance held when the wait begins

A wait whose expression reaches a variable through a virtual interface cannot name that variable's
cell ahead of time. The wait evaluates the handle where it begins and registers on the cell it
reaches, alongside the cells the expression names directly. The member is an interface's variable
and is observable like any other; what the wait did not know in advance is only which instance.

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
- **Converting between the pointer and the value by a cast.** The C++ spelling happened to do the
  right thing. The execution backend read the value's storage address as the object and crashed. A
  relation one backend realizes as reading a field and the other as nothing at all is an operation,
  and MIR states it.
- **Triggering every instance of the interface on a write through any handle**, as a statically
  scheduled simulator must. A write here wakes the written cell's own waiters, so nothing is gained.
- **Refusing a declaration whose parameters no instance has.** The program is legal, and a correct
  program is not refused because a unit was never needed.

## Consequences

- Class-based transactors run on both backends: assignment from an instance named directly, through
  a port or as an array element; comparison; member reads and writes; subroutine calls; a modport's
  own names; an interface the instance instantiates, reached into, called on, or held as a value
  itself; and waits.
- A wait keeps watching the instance it began with if the handle is assigned during the wait. LRM
  9.4.2 asks for the wait to move. Noticing the assignment needs the handle's storage to wake the
  waiter, which, for a handle held in a class property, is the object event source
  `object-is-an-event-source.md` defines.
- A wait on a property of a class object is refused rather than left never ending. Its expression
  names no cell either, and unlike an interface's variable a property publishes no write yet.

## Cross-references

- `interface-port-binding.md` -- the same unit named by a type, and the same published positions,
  reached from a port.
- `calling-a-subroutine-on-another-units-object.md` -- calling what an interface published, which a
  virtual interface does with a receiver chosen at run time.
- `object-is-an-event-source.md` -- the model a wait on an object's member, and a wait that follows
  a reassigned handle, both belong to.
