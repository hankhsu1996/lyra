# A unit states what it declares, in what it emits

Date: 2026-09-23 (revised 2026-09-25, 2026-09-27) Status: superseded in part by
[an-object-model-fixed-at-compile-time](an-object-model-fixed-at-compile-time.md) D4 and D7. What a
unit states about its classes is emitted as one constant per class, which a unit names another's by
symbol, rather than declared by a body the program runs before `main`. So D1's body, D2, D3's cell,
D4's layout step and D5's store are gone, D8's entry hands the runtime the root unit's object entry
rather than its definition, and the rejections of initialized data and of a flattened table are
answered there, as is the rejection of stating each record's contents in MIR as a constant, since
MIR states the contents and never the structure's shape. The requirements stand, and so does D7's
split, under which what a unit says about its classes is stated once above the targets. D6's
timescale is now a field of the class's constant, and D7's runtime-held storage, flat table and
scope construction entry are superseded too.

## Context

A compiled unit's code reaches the runtime through records the runtime owns: the definition a scope
is driven through, the one every value of a class carries, a closure's, the description one body's
variables need, and the storage a unit shares program-wide. The code names each by a symbol and
forwards its address without reading it.

Who supplies those records was never stated as a decision. On the C++ path MIR stated each one as a
constant, built at HIR-to-MIR, and the emitted unit carried what that backend rendered from it --
the backend sits at MIR, where a render entry may not compose what it emits, so what it emitted was
stated upstream. On the execution path a host built them after compiling -- from the lowered unit it
still held -- published each as an absolute data symbol, then looked up every entry the records
named and wrote the addresses back in.

That is not a second way of doing one thing. It is **half a program**: the module leaves the
compiler carrying code and a set of undefined symbols, and what defines them lives in the process
that compiled it and dies with it.

The first form of this decision moved the records into the artifact but left each backend deriving,
from its own layer, which declarations a unit makes. The two derivations came to disagree about
whose timescale a scope carries, and nothing failed, because a field one side stated and the other
missed takes its default. That is what the revision of D6 and D7 answers.

## The requirement

> Compiling a design and running it are separate acts: what compiling produces can be kept, moved to
> another machine, and run any number of times without compiling again.

Nothing about that sentence names a backend, and it is what the execution path could not answer. It
is also what the frontier measurement asks for -- the front half of the path that writes C++ misses
the whole budget before any compiler runs, and the path that writes none cannot be compared against
it until it produces something that can be timed on its own.

The revision adds a second:

> What the runtime is told about a class is decided once, from the class's own declaration, so no
> two targets can tell it different things.

## What the field does

Every SystemVerilog simulator's compile step produces a named executable and running is a separate
invocation of it: Verilator's `--binary` is `--main --exe --build --timing` and yields
`obj_dir/V<top>`, and VCS always writes `simv`, with `-R` merely running it afterwards. Neither has
a mode that runs while leaving nothing behind.

The transferable half is not that, though -- it is **which party resolves what a compiled artifact
names, and when**. Julia's AOT emits an object and links it against the same `libjulia` its JIT
resolves from the running process, so one library serves both and only the resolver differs. LLVM's
own ORC documentation prefers "symbolic resolution using JIT symbol tables rather than hardcoding
addresses", and keeps a hand-written address table for "small, unchanging symbol sets". Our
conditions do not differ, and the answer is taken outright.

For the second requirement, the question is which layer states what a class's dispatch table holds.
Swift's SIL states it beside the class -- in the SIL reference, a `sil_vtable` maps each method of
the class to the function implementing it -- and code generation lays the binary table out from
that; where the ancestry is resilient, the runtime installs entries and field offsets when the class
is first used. Clang builds its tables at code generation directly from the AST, which works because
one code generator consumes it. Our conditions differ from Clang's in exactly that. Two backends
consume MIR, so the Swift split is the one that transfers.

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
it overrides, and a class a scope answers a name with are each named by a cell.

This is what removes the whole-program join by name. The host matched a class to its base by
comparing linkage strings across every unit it had loaded; the composer resolves a symbol instead,
which is the thing a composer is for.

### D4. Laying out a lineage is one step, run once every declaration stands

A class's dispatch positions, and the storage the runtime holds for it, extend its base's, and a
base may be declared by any unit, so this is the only step that reads every declaration at once. The
program runs it where it starts, after every declaration body and before anything is built from a
definition.

### D5. The records are the runtime's to keep, for as long as the process lives

Storage is already the runtime's to own. The store is program-level because the declarations are.
Nobody discards it: the artifact is the process image, and the records live as long as anything can
call the code that names them.

### D6. What a unit's artifact is a function of

Its executable body, and nothing beside it. Each module, program, package, or interface definition
is its own time scope (LRM 3.14.2.2), so a scope's time unit and precision are a fact of the class
that scope is an instance of rather than of the unit, and the declaration body states them as
arguments where it declares that class.

### D7. What a unit declares about its classes is stated once, above the targets; the storage the runtime holds is not

Two different things were treated as one here, and they sit at different layers.

**Which declarations a unit makes about its classes is an operation**: that a class exists, what it
extends, where each of its properties sits on a value, which behaviors it introduces and which it
overrides, the names a class answers a referrer that has no name for it (LRM 23.9), the names a
scope answers (LRM 23.6), the bodies the runtime drives a scope through, and its timescale. None of
that depends on the target, and two derivations of it are two parties deciding one thing, kept in
step by nothing -- the failure in Context. So the target-neutral layer states it, as one body per
unit made of calls into the runtime's declaration entries, and every backend translates that body as
it translates any other. An entry a declaration hands the runtime is a reference to a body, carrying
the prototype the runtime calls it through.

Where a property sits is stated the same way, as a body. Every class of the source language carries
one per field, answering the field's address from the value's own address, and the declaration body
hands each to the runtime in field order. A property named past a signature (LRM 6.22, 23.9) is
reached through that body, so the address comes from whichever party holds the value's members -- a
C++ object, or the runtime -- without either backend saying which. The value's own address serves
for every class in its lineage, because a base part starts where the value does; that is what a
behavior is already entered with. A class standing in the design hierarchy states none, since a name
reaches its storage by walking the tree, never by asking its class.

**The runtime struct those declarations build is realization**, and so is the storage the runtime
holds where a target leaves storage to it -- for a value's members, a body's variables, a closure's
captures, what a unit shares -- together with the entry the runtime builds a scope value with. The
execution backend leaves all of it to the runtime, and states it itself, the class's storage after
the unit's declaration body has run and the rest beside it. The C++ backend states neither, since
its object holds its own members and is built by its own constructor. That the two differ is not
something this decision selects -- after MIR the execution backend is meant to do what an optimizing
C++ compiler does with the C++ backend's output, which lays a class's members out at compile time,
and moving it there removes those statements and changes nothing in the declaration body. The
runtime builds one flat table of each lineage's behaviors from the declarations -- the lifecycle
stays on a scope's program -- which the execution backend dispatches through and which an access
settled while the design elaborates reads on every target.

The labels a backend gives what only its own artifact reads -- a description's bytes, a name's
spelling -- are its own for the same reason, and never names the program links.

### D8. The program starts at one entry, derived from the design root

Composing the program adds exactly one module to the units' own: an entry, derived from the design
root, that hands the arguments the program was started with to the runtime together with the root's
definition as the root's unit declared it. The platform starts the program there, so nothing that
composes it builds the root its own way. The entry's module passes through the same check against
the runtime library the units' modules do.

## Rejected alternatives

- **Each backend deriving the declarations from its own layer.** This decision's first form. Two
  derivations of one operation disagree silently. A declaration one of them omits leaves the
  runtime's field at its default, and neither target fails. Where the two genuinely differ -- who
  holds a value's members -- the backend that hands them to the runtime states that storage itself,
  which is all the latitude this alternative was buying.

- **A point in the declaration body each backend fills with its own layout.** This decision's second
  form. Filling it on the C++ path meant the render writing a call into the runtime that MIR never
  stated, which a backend may not do; and where each property sits turned out not to differ between
  the targets once it is asked of the value rather than computed for the runtime.

- **Stating each record's contents in MIR as a constant both backends render.** It puts a runtime
  struct's shape, and the layout a target's compiler chooses, into the layer whose purpose is to
  know nothing of any target. The declarations are what MIR states; the runtime builds the record
  from them.

- **Emitting the records as initialized data.** The plain-data records could be, but the storage a
  member needs cannot: it is a variant over the value types the runtime realizes, constructed rather
  than laid down. That splits one statement into two shapes, and it puts a C++ struct's field
  offsets into the backend, where a wrong one is a silent wrong read rather than a build failure.
  Stating declarations through the runtime ABI keeps one shape and one agreement, and the ABI is
  already checked in both directions against the definitions it names.

- **Keeping the host builder and adding a second one for a linked program.** Two parties building
  one kind of record from one input, kept in step by nothing. The monomorphizing backend is what
  catches such a disagreement, and it cannot see this one.

- **Flattening the lineage's table at compile time.** A unit names each behavior by its introducer
  and its ordinal there, which it knows, but where an introducer's positions begin in the flat table
  depends on how many its own lineage introduces ahead of it, and another unit states that. So the
  table is built where every unit has spoken; the C++ compiler's own numbering of its virtuals is
  realization the table does not depend on. This is the same shape
  `program-facts-belong-after-compilation.md` met for namespace initialization order, and its reason
  transfers word for word -- both parties are borrowed, and the only moment they share is startup.

## Consequences

- The module a unit compiles to is self-contained: it carries code, the data its declarations are
  stated from, and references to what other units declare. Linking it is a link.
- A backend no longer reads a lowered unit after compiling it. What a host does with a compiled unit
  is compose it and run it.
- A fact added to what the runtime is told about a class is added once, where the declaration body
  is built, and reaches every target by translation.
- The refusal that names a runtime entry the library does not publish now covers the declaration
  entries too, so a unit stating something the runtime cannot realize is refused before the design
  stands rather than failing to come up -- and it runs before the link, so a design is refused by
  name rather than by an undefined symbol.
- The definition an object of the design hierarchy carries is the realizing class's on every target,
  handed to the tree's class through what the unit promised;
  `a-referrer-calls-rather-than-navigates.md` D4a is revised to say so.

## Cross-references

- `generated-behavior-boundary.md` -- the definition this states, whose supply side this moves.
- `program-facts-belong-after-compilation.md` -- the same startup answer for the other program-level
  fact this compiler has.
- `the-request-names-its-products.md` -- what a compilation step answers with, which is what makes a
  compiled unit's two halves travel as one.
