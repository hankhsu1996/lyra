# A published operation is compiled once

Date: 2026-09-23 Status: accepted

## What this decides

**A function the runtime publishes is compiled in the runtime's own artifact, and a unit that uses
it emits a call.** Where the function is a template the library parameterizes over a set it
enumerates itself, the library states that specialization once and the header says so, which leaves
the definition where it is written.

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

**The second mechanism costs no optimization, which is the reason it is not the first.** The
template stays written in the header, so a build that asks the optimizer to read it still reads it:
measured at `-O2`, the same call site compiles to 134 bytes of code with the declaration and 153
without, while the unit's object goes from 22,048 to 9,400. What stops being emitted is the copy,
not the definition.

| Unoptimized                     |     Before |      After |
| ------------------------------- | ---------: | ---------: |
| the probe above                 |    636,328 |    151,136 |
| a leaf unit of a 32-unit design |    756,496 |    285,400 |
| that design's objects, total    | 26,613,176 | 11,043,672 |

**It buys build time as well as disk**, which the class rule it follows did not: the same project
builds in 13.66 s against 17.46 s warm, 34.4 s of processor time against 45.2 s, because a unit no
longer performs the instantiations either.

## What was rejected

**Declaring the variant itself already compiled.** The obvious reach, since the variant's machinery
is what the objects were made of. It moves 288 bytes of 519,072: what a unit emits is not the
variant's own members but the visit helpers keyed by the visitor, and the visitor is a lambda inside
the library's header function that the unit compiled. Only moving that function moves them.

**Leaving the per-domain families alone and moving functions only.** It reaches most of the bytes
and stops at the ones a unit pays for holding a variable at all -- 56,616 of them on the probe above
-- which no call can remove because the cell is the unit's own member.

**Moving the small value operations too.** A caller's own operands decide what an arithmetic step on
a packed value compiles to, so the library publishes those definitions deliberately; they are what
an optimized build folds against the widths a design fixed. The line is not a size but who chooses
the parameter.

## What is left

A unit of 9,754 bytes still produces a 285,400 byte object, of which 61,945 is code. What remains is
the standard-library machinery of the scope-construction surface itself -- a hierarchy segment holds
a string, a definition holds arrays -- instantiated in the unit because the unit constructs those
values rather than because it calls something. Removing it means changing what those types are,
which is a decision about the surface rather than about where its functions live.
