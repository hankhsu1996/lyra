# A function reports what a call of it reads

Date: 2026-09-29 Status: accepted. Realizes
[object-is-an-event-source](object-is-an-event-source.md) D3 and D7 for an expression that calls a
function.

## Context

A wait on an expression ends when the expression's value changes (LRM 9.4.2: a non-edge event is
"detected on any change in the value of the expression"), and the same clause adds that "changing
the value of object data members, aggregate elements, or the size of a dynamically sized array
referenced by a method or function shall cause the event expression to be reevaluated", while an
implementation "may" reevaluate for members the function does not reference. LRM 9.4.3 holds a
`wait (cond)` until the condition becomes true, and its examples are 9.4.2's own,
`wait (AOR.size() > 0)`. Only `@*` is limited to a call's arguments (LRM 9.2.2.2.2).

A wait's read set was the front end's analysis of the expression, which reaches a call's arguments
and not its body. So `@(f())` with `f` reading a module variable never woke, on both backends, with
nothing reported; the same for a static property, a package function, a function reading through a
module's handle or virtual interface, and a function calling another. A method call was refused.

Two conditions shape the answer:

- **Units compile separately.** What a function reads is known where the function is declared, and a
  unit waiting on a call of it may read only what that unit published -- never its body, and never
  what a unit it does not reference published (north star 4 and 5; `unit-signature` D6). A package
  function calling another package's function is the ordinary case.
- **A wait already collects its leaves where it stands, on every candidacy**, since #1281 made one
  follow what its expression reaches through a handle. So what a call reads can arrive at run time,
  with the handles it would be evaluated on in hand.

## Decision

**Every function takes one more parameter, where to report what a call of it reads. An ordinary call
hands none and the function runs; a wait collecting its leaves hands its report, and the function
records what it reads and returns its type's default without running its body.**

- **What a function reads is stated by the unit declaring it**, from its own body: the cells outside
  the body it reads, the objects a chain of property reads reaches from its object, a handle formal
  or storage outside it, the variables of an interface instance a virtual interface of those holds,
  and the functions it calls, each called in turn with the same report. The report never runs the
  body, so it evaluates only what the function holds before the body starts; an argument it cannot
  evaluate there is handed as its type's default.
- **An object reached through what the function's own variables hold** -- walking a list, recursing
  -- is covered by every object at once: a write to any object's property reevaluates the wait. The
  clause's permission to reevaluate for unreferenced members is what this spends.
- **A null link ends a chain.** A report stands where nothing has tested a handle, so each one is
  tested before being read through, and a call made through a handle is made only where it names
  something.
- **Reports nest as deep as calls do, and a bound stops a cycle.** Past it the report watches every
  object instead; each function past the bound is one a shallower level of the same cycle already
  reported.
- **What a report cannot state is refused when a wait asks, not when the function compiles.** A
  function compiles whether or not anything waits on it, so a read no leaf watches yet -- an
  interface variable reached through what its own variables hold -- fails the simulation at the wait
  that asked (`SimulationError`), and the function is otherwise unaffected.

## Rejected

- **Summarizing the callee's body in the waiting unit.** It is how slang infers an `always_comb`
  list (`AnalysisManager::getFunctionValUses`, which skips a non-static method's body). It reads
  another unit's body, and composing what a callee's callees read reaches units the waiter does not
  reference -- the dependency north star 5 forbids.
- **Publishing each function's read summary in its unit's signature.** A signature is derived from
  declarations alone and never grows a body's facts (`unit-signature` D6), and the transitive part
  still needs a signature of a unit the referrer does not reference.
- **A sibling "report" body per function.** Correct, but it needs its own identity in every space a
  function is reached through -- a class's and a namespace's names, a dispatch slot, an interface's
  promise class, the settled-body tables, a scope's by-name table, both backends' names -- and one
  missed is a wait that silently never ends. A parameter travels every one of those already, which
  is also why a virtual method, an interface's function and one named hierarchically report with
  nothing new to name. (MSVC's scalar deleting destructor is one body with a flag for the same
  reason; the Itanium ABI's D0/D1/D2 are siblings. Recalled, not read.)
- **Recording every read while evaluating (MobX).** A check in every read of every variable and
  property, or a second compiled form of every body; either is a cost on all code for a rare
  construct.
- **Polling the expression every evaluation round (Verilator, `V3Timing.cpp`).** Lyra wakes only
  what a write reached, and a change undone within one process run is still an event a poll misses.

## Consequences

- **A function call costs one more argument**, a null pointer, and the function one test of it on
  entry, and every function carries the code of its report. Measured 2026-09-29 against `664a06a6`
  on the execution backend: `call-chain` (`--release`, 20,000 iterations of sixteen calls) ran
  1,037,860,839 instructions against 1,036,580,846, about four per call; Ibex's program text grew
  from 12,174,033 bytes to 12,333,457 (1.3%), and Ibex still reaches `$finish` at 26548.
- **A wait whose expression calls a function collects its leaves on every candidacy**, as one
  reaching through a handle does; a wait over cells alone registers them once, as before.
- **One change can reach a wait through several places it watches**, now the common case, so an
  observation latches that a candidacy was an event until it is armed again. Before, the second
  place to ask compared the new value against itself and cleared the answer, which also left
  `@(p.a + p.b)` waiting forever.
- **A constructor called inside such a function is not followed**: what its own body reads is not
  reported.
