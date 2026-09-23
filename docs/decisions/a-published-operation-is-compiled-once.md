# A published operation is compiled once

Date: 2026-09-23 Status: accepted

## What this decides

**A function the runtime publishes is compiled in the runtime's own artifact, and a unit that uses
it emits a call.** Where the function is a template the library parameterizes over a set it
enumerates itself, the library states that specialization once and the header says so, which leaves
the definition where it is written. That covers constructing and destroying what the runtime
defines, which is where most of a unit's copies turned out to hide.

**What a unit's object may define of the runtime's is checked on the object, not on the headers.** A
test emits a design, compiles every unit, and fails naming each runtime definition an object holds
that no design decision explains.

## Why

The C++ backend hands a user a library and a set of translation units that call it. A unit was not
calling it: every function the shipped headers defined was compiled again by every unit that reached
it, along with everything that function's body reached. What a body reaches here is not small --
anything touching a runtime value instantiates the machinery of a variant over fourteen value
domains -- so a unit's object was mostly the library's own text.

Measured at `ccc3aca5` on a probe stating what every design's unit states: one class deriving from a
scope, one variable, one write, and one wait.

| The probe adds                        |  Object |
| ------------------------------------- | ------: |
| nothing (the surface, and a function) |   1,112 |
| a class deriving from a scope         |  32,040 |
| one variable                          |  61,464 |
| a write to it                         | 119,008 |
| **one wait on it**                    | 519,072 |

**One wait cost 400,064 bytes**, in 596 symbols of which the great majority were the variant's visit
and assignment machinery. The entry it called was a function of the library's, written in a header.

## What it costs a unit, and what it does not

Two mechanisms, and which applies is decided by whether the parameter is the library's or the
design's.

**A function the library alone decides is defined in the library.** The header states it and nothing
else, so a unit compiles a call. Every entry an emitted unit may reach is enumerated where the
compiler names them, and the ones still written in headers were the wait, the delay, the fork and
join, the process controls, the simulation-time and random and distribution families, the foreign
task, and the coroutine promise's own bookkeeping.

**A family the library parameterizes over the value domains is stated once there.** What a
declaration's storage may hold is a closed set the runtime already writes out, so every family over
it -- a cell, a reference, a sampled history, a resolved net, a completion -- is a family the
library can state in full. An explicit instantiation declaration in the header says it has, and a
unit then refers to that copy.

**Constructing and destroying what the runtime defines is the library's too.** A constructor or
destructor defaulted where it is declared is not user-provided (C++ [dcl.fct.def.default]), so a
unit constructing the object defines it itself and a declaration that the family is already compiled
does not reach it; one left implicit is the same. Measured with clang and GCC on a probe under the
declaration: an ordinary member is withheld from the unit and a defaulted constructor and destructor
are both defined there, with everything they reach. So a family a unit holds -- a variable cell, a
net and its driver, a sampled history, a frame's promise -- defaults its constructor and destructor
after the class, and a class the runtime defines that a unit constructs, copies or destroys -- an
event, a hierarchy segment, an observation, a trigger, a collected object -- defines those members
in its own source file. Emitted code also names two value types no design shapes, a class handle and
the empty product a body completes with when it produces nothing, so the families over those are
declared too.

**What only the library reaches stays in the header.** The engine asks an event control whether a
change was an event on every change it reaches; moved into a source file of its own, that question
became a call from the engine's other sources, and putting it back in the header took 2.3% off
sparse wakeup's instruction count. No unit calls it, so no unit compiles it where it is. The line is
who calls a function, not how small it is.

| Unoptimized                     |     Before |     After |
| ------------------------------- | ---------: | --------: |
| the probe above                 |    636,328 |    35,120 |
| a leaf unit of a 32-unit design |    756,496 |   155,232 |
| that design's objects, total    | 26,613,176 | 6,720,864 |

**It buys build time as well as disk**, which the class rule it follows did not, because a unit no
longer performs the instantiations either. Measured side by side, the same project builds warm in
12.5 s where moving the functions and families alone left it at 16.1 s; on an earlier reading,
moving those had taken it from 17.46 s to 13.66 s.

**It costs an optimized build a little.** Against moving the functions and families alone, a
representative block does 0.6% more work and sparse wakeup 0.8% more, by instruction count. At that
size the figure moves with how the compiler lays out the library, so it is recorded rather than
attributed.

## What was rejected

**Declaring the variant itself already compiled.** The obvious reach, since the variant's machinery
is what the objects were made of. It moves 288 bytes of 519,072: what a unit emits is not the
variant's own members but the visit helpers keyed by the visitor, and the visitor is a lambda inside
the library's header function that the unit compiled. Only moving that function moves them.

**Leaving the per-domain families alone and moving functions only.** It reaches most of the bytes
and stops at the ones a unit pays for holding a variable at all -- 56,616 of them on the probe
above.

**Writing a family's members `inline` so that an optimized unit folds them.** A declaration that the
family is already compiled withholds the body of a member that is not an inline function, so an
optimized unit can only call it (C++17 [temp.explicit]/10). Writing the variable cell's write
`inline` to undo that bought nothing measurable on sparse wakeup and cost a representative block
0.6%, because the library's own copy of the write compiled worse.

**Checking the headers for the shapes that copy.** Four mechanisms were found, each invisible in the
header that carries it: a function defined there, a class with no virtual function defined in the
library, a defaulted or implicit constructor or destructor, and a member template over the caller's
type. A rule per shape is a rule per mechanism someone has already found. The object is where all of
them show, whichever it was.

**Moving the small value operations too.** A caller's own operands decide what an arithmetic step on
a packed value compiles to, so the library publishes those definitions deliberately; they are what
an optimized build folds against the widths a design fixed. The line is not a size but who chooses
the parameter.

## What is left

What a unit's object holds of the runtime is what its design decided: families over the value types
it composed, the templates it instantiated over its own lambdas, and a fork's branch count. Two
single comparisons the write path asks on every store are written in the header on purpose, and the
test names them as such.

What else a unit holds is its own: its classes, and the value operations it performs, which the
library publishes so that an optimized build folds them against the widths a design fixed.
