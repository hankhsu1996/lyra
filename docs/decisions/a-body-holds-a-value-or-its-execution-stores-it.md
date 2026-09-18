# A body holds a value or its execution stores it

Date: 2026-09-17 Status: accepted

## Context

On a target whose bodies are not written in a language with its own storage, an execution runs as
several stretches: the ramp, then each resume. A value made in one stretch lives in that stretch's
store and is released when it returns, so a variable that outlives the stretch needs storage the
execution owns. Which variables those are was decided by a list of type tests written in the
translation, under a sentence saying it named "every domain the runtime holds values of".

The list did not name that. A class handle is a domain the runtime holds values of and was absent,
so a variable of class type stayed a value of the body -- and what the body holds of one is the
address, while what keeps the object alive is the claim on its life that sits beside the address in
the runtime's own storage. Measured at `291e30c1` against the other target, which answers all of
these correctly:

| where the handle sits when the execution parks | answer                        |
| ---------------------------------------------- | ----------------------------- |
| a class property                               | correct                       |
| a module's own variable                        | correct                       |
| a block promoted out of its frame (LRM 6.21)   | correct                       |
| an element of an aggregate variable            | correct                       |
| a variable of automatic lifetime               | a member reached through null |
| a subroutine's formal                          | a memory fault                |
| a local of a class method                      | a memory fault                |

The first four are storage described by a schema, which owns what it holds. The last three are not,
and the last is every local of every method there is, because a class method's lifetime is automatic
whatever encloses it (LRM 8.6).

**This is the third cut of the same list and the second wrong answer from it.** The first removed
"can this body suspend" and "does anything lend this variable" as the questions deciding a
variable's home; the second found the list omitting every container, which the same reading found --
the sentence above it and the list under it named different things.

## Decision

**D1. A variable is a value of its body where the body holds the whole of that value, and storage
its execution owns where it does not.** Nothing about the body takes part, and nothing about what
the body does with the variable takes part; what decides is whether what crosses to the body is the
value or a handle onto a value living elsewhere.

A class handle is on the storage side and a chandle is not, and IEEE 1800 states the difference
directly. Table 8-1's "unreferenced objects are garbage collected" row answers **Yes** for an object
handle and **No** for a chandle and for a C pointer (LRM 8.4). Being collected when unreferenced is
a property that needs someone to count references, so holding an object handle is holding a claim on
the object's life, which no address carries on its own. A chandle's value is the address it carries
(LRM 6.14), so whoever holds the address holds the whole of it.

**D2. Which types those are is one total classification, stated where the alternatives are
declared.** Every type answers, and a type added later fails to compile until someone says which
side it is on. A list of the ones somebody remembered answers false for the rest, and that answer is
a wrong one rather than a refusal: the body goes on holding a handle after what it named is gone.

**D3. A variable naming an object answers a change of which object it names.** A body's variable is
storage a process may wait on, so D1 puts a handle in a cell that publishes changes -- and LRM 9.4.2
says exactly what such a wait means: "If the event expression is a reference to a simple object
handle or `chandle` variable, an event is created when a write to that variable is not equal to its
previous value." The clause's own example places a wait on a property beside a wait on the handle
naming it to say they are two different waits, and a write naming the object already named is no
event.

The chandle half of that sentence is not carried out, for a reason of its own rather than a
preference: a chandle's value is the pointer it carries, so a null chandle is a null pointer, and
the boundary a synthesized body answers across reads a null answer as no answer. Waiting on one
needs the domain to cross the way every other value domain does, which is a change to what a chandle
is rather than to what a wait reaches.

## Survey

Every system that runs a body in several stretches and also has to find its references does the same
thing, and none of them keeps a separate list of which slots hold one.

- **C++20 coroutines.** The coroutine state holds the parameters and "local variables and
  temporaries whose lifetime spans the current suspension point", and destroying the state runs
  their destructors (<https://en.cppreference.com/w/cpp/language/coroutines>). The compiler knows
  each local's type, so the state holds the object rather than a pointer to one, and membership is
  decided by the same transform that lays the state out.
- **Kotlin.** A suspending function compiles to a state-machine class holding "fields for local
  variables of the coroutine that are shared between states", decided at code generation
  (<https://github.com/Kotlin/KEEP/blob/master/proposals/coroutines.md>). The continuation is an
  ordinary heap object, so the collector reaches those locals with no mechanism of its own.
- **HotSpot virtual threads.** Parking copies the call stack into a `StackChunk` -- a regular heap
  object holding frames plus an oop bitmap so the collector can find the references inside them --
  and such stacks are deliberately not roots (JEP 444, <https://openjdk.org/jeps/444>).

All three move what crosses a stop into a described object the runtime owns, produced by the
compiler's own transform. **What none of them needs is a second statement of which slots hold a
reference, because the representation says so**: a JVM oop and a .NET object reference are reference
types at the machine level.

**Our condition is that no representation here can say it, and that is structural rather than
unbuilt.** Every value crosses to a body as a bare address, so two types that are both an address
there differ invisibly in whether the address carries a claim on anything. The difference has to be
stated in a description. Laying values out natively would not remove the need -- a slot would still
have to say whether it retains -- and a collector makes it sharper, since the description is then
also what root enumeration walks.

## Invariants

1. A declared variable's storage belongs to the execution that declared it whenever the body does
   not hold the whole of the variable's value. Neither the body's shape nor what it does with the
   variable takes part.

2. Which types the body holds the whole of is stated once, as a classification that answers for
   every type. A question of that kind belongs on the type beside its other named questions rather
   than at the site that asks it.

3. A variable naming an object answers a change of which object it names, and a write naming what it
   already named is no event. A wait on a property of the object is a different wait.

## Rejected

- **A second storage path for a managed value, gated by type.** The gate that produced this defect
  routed a managed value away from the cell and toward a traceable frame that does not exist. What
  tracing adds is reclamation, which is not what the storage is for: a handle needs storage that
  outlives a stretch for the same reason a loop counter does, and the two answers would be one
  mechanism under two names.

- **Holding the handle in the body's own frame across the stop.** The mechanism that persists a
  body's slots is the target's, and where it puts each one is its decision, so a reference held
  there is in state nothing can enumerate. It is also measured to be the wrong place already:
  storage put in such a frame was destroyed one step before the frame itself, and a subroutine's
  `output` came back as a default value with nothing reported.

- **A second alternative in the erased value carrier for the monomorphized handle.** A handle's
  value is which object it names; the pointer a target carries beside that is how it reaches a
  member, not part of the value. Two alternatives for one domain would be a second shape every
  reader then has to tell apart, so what crosses into the carrier is the identity and the pointer
  stays behind.

## Consequences

- A body that takes a handle, waits, and then uses it runs on both targets -- which is what every
  testbench written in classes does, since LRM 8.6 makes every method automatic.
- A variable of class type is an observable cell, so `@handle` runs. A chandle's is not, and the
  refusal that stands in front of it now gives the reason that holds rather than the one LRM 9.4.2
  contradicts.
- The translation reads a classification rather than keeping its own list, so the shape that gave
  two wrong answers is gone from this question. The enforcement that would have caught it reaches an
  enumeration read by `==` and a visit with an unnamed arm, and not a predicate built from a chain
  of type tests.

## Cross-references

- [a-declared-variable-is-one-storage](a-declared-variable-is-one-storage.md) -- the cut that
  removed the other two questions from this one, and whose own cross-reference already said the gate
  this removes was no longer how a home is chosen.
- [cross-suspension-value-storage](cross-suspension-value-storage.md) -- the cell, and the gate on
  non-managed types this supersedes.
- [activation-frame-and-transient-scope](activation-frame-and-transient-scope.md) -- the two storage
  lifetimes, and the slot schema it deferred until a managed value reached this target.
- [a-handle-is-a-value](a-handle-is-a-value.md) -- a handle's value is which object it names, and
  what a realization carries beyond that is never part of the value.
- [exhaustive-alternative-consumption](exhaustive-alternative-consumption.md) -- a closed set is
  consumed so that gaining a member breaks the build, and where a question of a large set answers
  for a handful it belongs on the type.
- `../architecture/lifetime.md` -- the managed regime, and why a value that survives a safepoint may
  not live in the target's own execution state.
