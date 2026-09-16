# A design's foreign code is linked into its program, not loaded beside it

Date: 2026-09-14 Status: accepted

## Context

A DPI-C export defines a global symbol the foreign side calls by name: "every subroutine exported
from SystemVerilog defines a global symbol... their own global name space of linkage names,
different from compilation-unit scope name space" (LRM 35.4). The user's C is compiled separately,
against a generated header, and calls that name like any other.

The C++ backend meets this with no design decision at all: every artifact exists as an object file
before the program starts, so the system linker binds the name. The execution backend does not have
that property -- the design's code exists only inside an execution session that compiles it -- and
the question this settles is who binds the name there.

Until now the two directions of the boundary were resolved by two different parties. The design's
foreign sources were compiled into a shared library the session opened as a _source of symbols_, so
a generated foreign call found the C function; the reverse had no answer, and an export was refused.

## Decision

**A program has one linker. Whatever resolves a name between two of a program's artifacts resolves
every name between them, in both directions; a design's foreign code is an artifact of that program
rather than an environment around it.**

For the execution backend that means the foreign sources are compiled to relocatable objects and
linked into the execution session, which already resolves names across every module it holds. Both
directions then fall out of one mechanism: the design's calls out reach the object, and the object's
calls in reach the entry points the session compiled.

### Where the decision lives, by comparison

- **Verilator** emits the global C symbol as an ordinary definition in a translation unit of its
  own, forwarding to the model, and the system linker binds it. Its own generated comment records
  the consequence -- two designs exporting one name are a multiple-definition link error, "an
  unfortunate result of the DPI specification". The decision is at emission; the resolution is the
  linker's, before the program runs.
- **Julia** lets C call back into JIT-compiled code only through a pointer handed over at run time
  (`@cfunction`), never through a name. A managed runtime publishes an address because JIT-compiled
  code has no name the system loader knows.
- **LLVM's ORC**, which this backend runs on, "aims to emulate the linking and symbol resolution
  rules used by the static and dynamic linkers": a relocatable object added to a JITDylib has its
  external references resolved against that dylib's link order, while a search generator over a
  dynamic library answers only lookups that fail inside the JIT -- one direction, inward.

**The condition that differs is that the name is fixed and the code is not.** The standard fixes the
symbol and the user's C is compiled against it, so Julia's pointer handoff is unavailable; the
design's code does not exist until the session compiles it, so Verilator's system linker is
unavailable. What is left is the session, and an object is what a linker takes.

### A scope publishes what it answers, per name space

An exported subroutine is compiled once per specialization of the scope declaring it, while the
symbol is program-global (LRM 35.4), so the symbol resolves an entry against the scope the call
chain established. Each scope therefore states what it answers a foreign name with, beside what it
answers a hierarchical name with: two name spaces over one mechanism, and one declaration may answer
in both under different spellings.

The two tables are not filled the same way, and the reason is the entry rather than the table. What
a foreign name reaches is a body that converts between the boundary carriers and the subroutine's
own values (LRM 35.5.6) -- a body with a signature of its own. What a hierarchical name reaches has
the subroutine's signature already, so on a backend whose receiver is untyped the subroutine's own
body is that entry.

### A foreign call is an execution of the process, with storage of its own

A foreign call that can suspend is carried on a stack the runtime allocates, so the SV side can park
while the foreign frame stands. Two things follow, and both are properties of the call rather than
of any frame in it.

Generated code reached from that stack is a crossing into generated code like any other, so a
stretch of the call is a call scope. And what generated code materializes there may have to survive
a park -- the storage an exported subroutine's entry completes into -- so the call owns a value
store for its whole life: a per-stretch arena is released too early, and no frame outside the call
lives long enough.

## Rejected alternatives

- **Keeping the shared library and publishing the design's symbols to the system loader.** A loader
  binds from symbol tables of loaded images, and JIT-compiled code is in none; there is nothing to
  publish into. This is not a harder version of the chosen answer, it is unavailable.
- **Handing the foreign side a function pointer per export.** What a managed runtime does, and what
  the standard forecloses: the foreign source is compiled against a name, so a pointer would have to
  be delivered by some call the standard does not define and the user's C does not make.
- **Emitting the design ahead of time and linking everything with the system linker.** That is a
  different execution model rather than an answer for this one, and it would make an in-process run
  impossible for any design that crosses the boundary.
- **A second resolver for the inward direction only.** Two resolvers is the state this replaces. It
  is what let the outward direction work while the inward one had no answer, and each additional
  direction would need its own.
- **Stating the export entry's completion storage as a cell of the entry's caller.** The caller is
  foreign code, which owns no such storage; the entry's own frame is on the foreign stack, which the
  runtime cannot allocate into. The call is the thing whose life covers every frame on that stack.

## Consequences

- One mechanism serves both directions of the boundary on this backend, and a design that crosses it
  runs in process.
- The compiled foreign inputs are objects rather than one library. Nothing links them to each other;
  the session is where they meet the design.
- A wakeup registered while running under a foreign call records the vehicle that carries it, the
  same way one registered by an awaiting body does -- otherwise the scheduler resumes a frame whose
  continuation is buried under a native stack, and the foreign call is never returned to.

## Cross-references

- `docs/architecture/emission_model.md` (a backend is a choice of linker; an execution session is a
  linker)
- `docs/architecture/runtime_distribution.md` (the boundary surface a design publishes, and the
  design root as the owner of a program-global symbol)
- `docs/decisions/dpi-foreign-boundary.md` (the callable model, the export entry point, and the
  ambient run context it recovers from)
- `docs/decisions/generated-behavior-boundary.md` (the record a scope is built from, and the entries
  it publishes under a name a caller outside the design reaches it by)
- `docs/decisions/cross-suspension-value-storage.md` (what a value crossing a suspension needs, and
  why a per-stretch arena is not it)
- LRM 35: 35.4 (the global name space), 35.5.1.1 (a task may consume time), 35.7 (exported
  functions), 35.8 (exported tasks)
