# The scope a context import makes current is an extent of what is running

Date: 2026-09-12 Status: accepted

## Context

LRM 35.5.3 gives a `context` DPI-C import a scope the foreign side observes, and states it over the
**import call chain** rather than over the subroutine: a context "is created that is equal to the
instantiated scope of the import declaration" when SystemVerilog calls the import, `svSetScope` sets
it, a call to an export restores it on return, and the chain ends when it "unwinds and returns to
the calling SystemVerilog subroutine at the root of the import call chain". The clause says the
property "adheres to the calling chain, not to an individual imported subroutine".

So what has to be expressed is a value that is in force for the duration of a call and given back
however that call ends. MIR expressed it as a local whose type's destructor gave it back: an inert
runtime value whose lifetime, not its contents, was the effect. Two things followed.

A backend with no destructors could not lower it at all, so every case reaching a context import
stopped one stage before the execution backend. And the chain lived on the executing process, so a
call made before any process exists -- a variable declaration assignment runs first (LRM 10.5, and
LRM 26.2 says the same for a package) -- aborted the run as a compiler invariant on a legal program.

## Decision

**What is in force while something runs is installed by an entering call and given back by a leaving
one, and the body states both.** Nothing is left for a target language to run at scope exit.

### The two ends are stated, because only one target has the other way

A region paired with a cleanup that runs on every way out of it is already MIR's vocabulary, and
`disable` settled the general form: marking membership with a value whose lifetime is the body's
"asks the target language to run code at scope exit, which is a facility only some have"
([disable-scope-invalidation](disable-scope-invalidation.md)). A context import's scope is the same
shape and was the one place still spelled the other way.

The survey says this is where the line falls in every system with both a source level and an IR.
LLVM IR has no destructor concept -- a `landingpad`'s `cleanup` clause exists because "C++
front-ends use this for calling objects' destructors", and the front end emits the cleanup code. The
JVM has no scope-exit concept either; JVMS 3.13 compiles `finally` as a block reached by an explicit
call from the normal path and from the exception handler alike. Common Lisp, which has no
destructors at all, states a dynamic binding as points of establishment and disestablishment with
`unwind-protect` between them. Go puts the region in the language as `defer` and lets the compiler
choose, during SSA construction, between open-coding the cleanup onto each exit path and a runtime
chain -- one stated construct, two realizations.

Our condition is Go's: one stated construct, two backends, and the realization is each backend's.
MIR sits at the IR level, where none of those systems keeps the destructor.

### The chain belongs to what is running, not to a process

A randomization call already draws from the generator installed for whatever is running
([static-initializer-draws-from-its-container](static-initializer-draws-from-its-container.md)),
because a subroutine body is compiled once and reached from both a process and a static
initialization. The DPI scope chain is a second fact of exactly that kind, reached the same way and
for the same reason, so it is installed the same way rather than kept on the process.

What the process placement was protecting is kept: two foreign calls suspended on different
processes still never share a chain, because a process owns what is in force while it runs and a
resume installs it. What it gains is the position it could not serve -- a context import reached
from a static initializer, which is every position that initializes static-lifetime state.

### The extent covers the statement that hands control to the foreign side

For a function import that is the boundary the call sits in; for a task import it is the await of
the fiber, because the scope has to be current before the native stack is entered and still current
when the fiber resumes. A value settled inside the extent and read after it lands in a binding
declared ahead of it, which is what a value produced inside a protected region does in every
language that has one.

LRM 35.5.3 instruments a call only where the import is `context`, so a plain import's boundary
carries no extent at all.

## Rejected alternatives

- **Keep the object and give the execution backend a destructor discipline.** It would put a
  language facility one target has into the IR both targets read, and every later extent -- a
  cancellable region, a static initialization -- would have had to choose between two spellings of
  one concept.

- **Bracket the call without a cleanup, on the reasoning that a foreign call returns normally.** It
  does not always: an export the foreign side calls back can raise a control effect or end the run,
  and an execution that then continues past the region would report a scope that is no longer
  current. The cleanup is what makes "however the call ends" true rather than usual.

- **Keep the chain on the process and give a static initialization a chain of its own.** Two homes
  for one fact, told apart by a branch at every reader, and the branch would answer "no process" by
  choosing rather than by finding what is running.

- **Let the extent cover the whole marshaling body rather than the boundary.** Marshaling is this
  side's own code and no foreign call can observe the scope during it, so the wider extent states
  more than the standard asks and hides where the boundary actually is.

## Consequences

- A `context` import is legal wherever the language allows a call, including every position that
  initializes static-lifetime state before any procedure starts.
- The execution backend lowers a context import; what it stops on next is its own.
- MIR's type vocabulary carries no value whose meaning is a destructor, and neither backend's type
  mapping names one.
- A namespace's declarations are never instantiated, so a context import declared in a package or at
  `$unit` scope observes no scope wherever it is called from -- from inside the namespace as well as
  from a unit that imported the name.

## Cross-references

- LRM 35.5.3 (context tasks and functions; the import call chain and its context), 35.5.4 (the
  declaration scope note), 10.5 and 26.2 (declaration assignments run before any procedure starts)
- [disable-scope-invalidation](disable-scope-invalidation.md) -- the general form: an extent states
  its entry and its cleanup, and a backend realizes the cleanup its own way
- [static-initializer-draws-from-its-container](static-initializer-draws-from-its-container.md) --
  the first fact installed by whatever is running
- [dpi-foreign-boundary](dpi-foreign-boundary.md) -- the import as a bodyless receiver-less callable
  whose scope the call site establishes around the foreign call
- [ambient-runtime-services](ambient-runtime-services.md) -- why a body reaches the runtime without
  being handed it
