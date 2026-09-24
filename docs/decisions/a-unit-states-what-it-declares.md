# A unit states what it declares, in what it emits

Date: 2026-09-23 Status: accepted

## Context

A compiled unit's code reaches the runtime through records the runtime owns: the definition a scope
is driven through, the one every value of a class carries, a closure's, the description one body's
variables need, and the storage a unit shares program-wide. The code names each by a symbol and
forwards its address without reading it.

Who supplies those records was never stated as a decision. On the C++ path MIR states each one as a
constant, built at HIR-to-MIR, and the emitted unit carries what that backend renders from it -- the
backend sits at MIR, where a render entry may not compose what it emits, so what it emits is stated
upstream. On the execution path a host built them after compiling -- from the lowered unit it still
held -- published each as an absolute data symbol, then looked up every entry the records name and
wrote the addresses back in.

That is not a second way of doing one thing. It is **half a program**: the module leaves the
compiler carrying code and a set of undefined symbols, and what defines them lives in the process
that compiled it and dies with it.

## The requirement

> Compiling a design and running it are separate acts: what compiling produces can be kept, moved to
> another machine, and run any number of times without compiling again.

Nothing about that sentence names a backend, and it is what the execution path could not answer. It
is also what the frontier measurement asks for -- the front half of the path that writes C++ misses
the whole budget before any compiler runs, and the path that writes none cannot be compared against
it until it produces something that can be timed on its own.

## What the field does

Every SystemVerilog simulator's compile step produces a named executable and running is a separate
invocation of it: Verilator's `--binary` is `--main --exe --build --timing` and yields
`obj_dir/V<top>`, and VCS always writes `simv`, with `-R` merely running it afterwards. Neither has
a mode that runs while leaving nothing behind.

The transferable half is not that, though -- it is **which party resolves what a compiled artifact
names, and when**. Julia's AOT emits an object and links it against the same `libjulia` its JIT
resolves from the running process, so one library serves both and only the resolver differs. LLVM's
own ORC documentation prefers "symbolic resolution using JIT symbol tables rather than hardcoding
addresses", and keeps a hand-written address table for "small, unchanging symbol sets".

Our conditions do not differ, and the answer is taken outright.

## The decisions

### D1. A unit states its declarations in a body its own artifact carries

Each unit emits one body that says what the unit declares, and asks the target to run it before the
program starts. Composing the program settles when that is: a linker collects such bodies into the
list the platform runs before `main`. The unit says nothing about who composes it, so anything that
composes programs the platform's way -- an execution session runs the same list on the way up --
takes it unchanged.

### D2. A declaration body reaches nothing a run brings into existence

It runs ahead of the engine and ahead of the design, before `main`, so what a unit declares may not
depend on a running simulation. That is a property of when declaration bodies run, not a convention
the composer keeps.

### D3. A class of another artifact is named by the cell holding its definition

The artifacts speak in whatever order the program was composed in, so a class another unit declares
may not have spoken when this one names it. A cell has an address from the moment the program is
composed and holds its value once every artifact has spoken, so a base, the introducer of a behavior
taken over, and a class a scope answers a name with are each named by a cell.

This is what removes the whole-program join by name. The host matched a class to its base by
comparing linkage strings across every unit it had loaded; the composer resolves a symbol instead,
which is the thing a composer is for.

### D4. Laying out a lineage is one step, run once every declaration stands

A class's layout extends its base's, and a base may be declared by any unit, so this is the only
step that reads every declaration at once. The program runs it where it starts, after every
declaration body and before anything is built from a definition.

### D5. The records are the runtime's to keep, for as long as the process lives

Storage is already the runtime's to own. The store is program-level because the declarations are.
Nobody discards it: the artifact is the process image, and the records live as long as anything can
call the code that names them.

### D6. What a unit's artifact is a function of

Its executable body and the facts that body carries none of. A unit's timescale (LRM Table 20-2) is
a constant the runtime is told rather than something the code computes, and the body it belongs
beside states no source-language concept -- so the artifact takes both halves, exactly as the
compiled unit that carries them does.

### D7. The record is realization, derived below the target-neutral layer

Every fact a unit-definition record holds is already a declaration at every layer: the entries a
scope is driven through, the callables and classes it answers for, the storage its members need, and
the timescale beside the executable body. What a record adds is only the runtime struct's shape
those facts are bundled into, and choosing a shape is realization. So the execution backend derives
each record from the declarations its layer carries, exactly as it derives every other physical fact
below that layer, and nothing about the record enters the layer above it. The labels it gives what
only its own artifact reads -- a description's bytes, a name's spelling, the declaration body itself
-- are its own for the same reason, and never names the program links.

### D8. The program starts at one entry, derived from the design root

Composing the program adds exactly one module to the units' own: an entry, derived from the design
root, that hands the arguments the program was started with to the runtime together with the root's
definition as the root's unit declared it. The platform starts the program there, so nothing that
composes it builds the root its own way. The entry's module passes through the same check against
the runtime library the units' modules do.

## Rejected alternatives

- **Routing the records through MIR-to-LIR, so both backends read one statement of them.** It puts a
  runtime struct's shape into the layer whose purpose is to know nothing of any target, which is the
  violation rather than a cure for two derivations. The records are stated in MIR only because the
  backend that reads MIR may not compose what it emits; they leave with that backend.

- **Emitting the records as initialized data.** The plain-data records could be, but the storage a
  member needs cannot: it is a variant over the value types the runtime realizes, constructed rather
  than laid down. That splits one statement into two shapes, and it puts a C++ struct's field
  offsets into the backend, where a wrong one is a silent wrong read rather than a build failure.
  Stating declarations through the runtime ABI keeps one shape and one agreement, and the ABI is
  already checked in both directions against the definitions it names.

- **Keeping the host builder and adding a second one for a linked program.** Two parties building
  one kind of record from one input, kept in step by nothing. The monomorphizing backend is what
  catches such a disagreement, and it cannot see this one.

- **Computing the lineage layout at compile time.** A unit cannot: a class may extend one no unit it
  can see declares. This is the same shape `program-facts-belong-after-compilation.md` met for
  namespace initialization order, and its reason transfers word for word -- both parties are
  borrowed, and the only moment they share is startup.

## Consequences

- The module a unit compiles to is self-contained: it carries code, the data its declarations are
  stated from, and references to what other units declare. Linking it is a link.
- A backend no longer reads a lowered unit after compiling it. What a host does with a compiled unit
  is compose it and run it.
- The refusal that names a runtime entry the library does not publish now covers the declaration
  entries too, so a unit stating something the runtime cannot realize is refused before the design
  stands rather than failing to come up -- and it runs before the link, so a design is refused by
  name rather than by an undefined symbol.

## Cross-references

- `generated-behavior-boundary.md` -- the definition this states, whose supply side this moves.
- `program-facts-belong-after-compilation.md` -- the same startup answer for the other program-level
  fact this compiler has.
- `the-request-names-its-products.md` -- what a compilation step answers with, which is what makes a
  compiled unit's two halves travel as one.
