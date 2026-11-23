# Runtime Distribution

A `lyra` finds its runtime only where Bazel staged the two together, so `run`, `build`, `emit cpp`
and every command reading a design that takes a supported library work only in the Bazel build tree
and its tests. The install-relative strategy below is the contract for a released `lyra` all the
same, and nothing else in this document depends on which strategy answers.

Every program a backend produces links the **Lyra C++ runtime**: a static library and, for the C++
backend's sources, its headers. The emitted code depends on nothing else from the Lyra toolchain;
the third-party code the runtime uses is inside the library. The runtime is a small library that
ships with the compiler and is compiled before any design exists, so everything a design's code says
to it crosses a boundary fixed when the library was built: the C++ backend's units reach it through
its headers, and the LLVM backend's modules through the C ABI it publishes. This document owns what
crosses that boundary, how the library ships and is found, what a run writes, and where the library
places a report it makes for the design.

## The notices a program carries

Some of that third-party code is under a license asking whoever distributes a program built from it
to pass its notice on. The build gathers each such library's license, as the library states it, into
one text that ships beside the runtime. Every program carries the same text inside its runtime and
prints it when run with `+lyra+licenses`, and `emit cpp` writes it beside a project as
`THIRD_PARTY_LICENSES`. Someone distributing a program Lyra built distributes that text with it.

## What form the runtime ships in

A build asked for `--lto` optimizes the design's code together with the runtime it calls, by the
compiler that compiles the design, so the runtime ships in a form each such compiler can read.

- **The static library carries machine code and bitcode in each object.** The bitcode is that of the
  LLVM release Lyra links, because the one link that reads it is the execution backend's link under
  `--lto`. Every other link reads the same objects as machine code.
- **Its sources ship beside it, with how each is compiled.** The C++ backend's design is compiled by
  the user's compiler, of any release or family, and source is the one form every such compiler
  reads. They are stated by the runtime's own build -- the translation units, their include
  directories and their macros -- so nothing that compiles them restates any of it. The same
  compiler builds them where it checks a program for its sanitizer.
- **A second build of the library is compiled for the address sanitizer, beside that release's
  sanitizer runtime.** Whoever instruments a design's code links the sanitizer runtime of the
  release that instrumented it, because instrumented code names its runtime's version; on the
  execution backend that is the LLVM release Lyra links, so a checked program links both. The
  library is checked as the design is, since a read of ended storage made inside the runtime as a
  design drives it is as much the program's defect as one in the design's code. It carries machine
  code alone, since the release refuses to instrument a fat object's modules a second time. The
  release's `llvm-symbolizer` ships beside its sanitizer runtime, which starts it to name the
  functions in a report ([building_a_program.md](building_a_program.md)).

## What a unit's object holds of the runtime

**Work that is the same for every unit of every build is done once, where the artifact those units
share is made.** A unit pays for what it contains, on every build and once per unit, so whatever it
repeats of the library grows the edit-compile-run loop with the size of the design (`north_star.md`
invariant 1), and a unit's object is as much a cost as its compile: a large design holds every one
of them on disk at once. So what a C++ backend unit hands the linker is its own classes, the value
operations it performs, and calls into the library.

- **A function the library alone decides is defined in the library.** The header declares it and a
  unit compiles a call. That covers every entry an emitted unit may reach, constructing and
  destroying what the runtime defines included.
- **A family the library parameterizes over a set it enumerates itself is stated once there.** What
  a declaration's storage may hold is a closed set the runtime writes out, so every family over it
  -- a cell, a reference, a sampled history, a resolved net, a completion -- is compiled in the
  library and declared in the header as already compiled, and a unit refers to that copy. A
  constructor or destructor defaulted where it is declared is defined by whatever unit constructs
  the object (C++ [dcl.fct.def.default]), so such a family defaults its constructor and destructor
  after the class, and a class the runtime defines that a unit constructs, copies or destroys
  defines those members in its own source file.
- **A class the runtime publishes with a virtual function defines at least one of them in its own
  source file.** Under the Itanium C++ ABI a class's dispatch table and type information are emitted
  where its first non-inline virtual function is defined, and in every object that references them
  where there is none, so the key function is what makes the library their one owner.
- **A member a unit calls on a family stated once carries no constraint.** The library is compiled
  by the pinned clang and a unit by whichever compiler the user has, and the Itanium ABI states no
  spelling of a requires-clause, so the two would name the member differently. What differs by type
  goes in the body, or in a default argument; a member some types of a family cannot have is
  declared for every type and left out where it cannot be built, and such a family is stated member
  by member, as libc++ states `basic_string`.
- **What only the library calls stays where it is written**, header included: the line is who calls
  a function, not how small it is.
- **What the caller's own types decide is published deliberately.** An arithmetic step on a packed
  value compiles to what its operands' widths fix, so the library publishes those definitions and an
  optimized unit folds them against the widths its design fixed.

What holds this is a check on the objects, never on the headers, because each mechanism that copies
library code into a unit -- a function defined in a header, a class with no virtual function defined
in the library, an implicit constructor or destructor, a member template over the caller's type --
is invisible in the header that carries it. A test emits a design, compiles every unit, and fails
naming each runtime definition an object holds that no design decision explains; a second reads what
a unit another compiler compiled asks for and fails naming each name the library does not define.

## How the execution backend reaches the library

The library is compiled once, before any design exists, and the execution backend's modules reach it
through a C ABI. Two facts have to agree at that boundary -- which entry is meant, and what crosses
to it -- and each is stated once, where it is decided, so that the two sides cannot drift apart
without a build failing.

**A runtime entry is named by the operation it performs and typed by the call that reaches it.** The
function type is built from what the call actually passes, after its arguments are marshalled, so an
entry and its call cannot disagree about how many values cross or what shape each has. The symbol is
`lyra_rt_` followed by what selects the entry, outermost first, and the operation last: the value
representation, where the library realizes the operation once per representation; the kind of
storage or the library object the operation acts on, where it acts on one; and nothing more where
the library realizes it once -- `lyra_rt_bitvec_cell_construct`, `lyra_rt_assoc_construct`,
`lyra_rt_dump_flush`. Each part's spelling comes from the name table of the closed set that
publishes it -- the builtins and the operators, which reach the backend already named, the execution
model's own operations, which the backend enumerates, and the representations, storage kinds and
library objects -- so a symbol is never composed from a string, and a set gaining a member fails to
compile until it is spelled. Naming is total over the builtin set: each builtin says whether its
entry stands alone, is named by a representation, or has no entry of its shape and which shape that
is. The same totality is owed by a construction: which entry builds a value is read from the type
being built, so a value gaining a kind says which entry brings one into existence or that nothing
does, and a type the library declares names its constructor as an entry of the builtin set. Two
requests are two entries, never one entry told apart by its arity. An entry exists as a prototype in
the ABI header, a definition beside the runtime it wraps, and a line in the list of what the library
publishes, which every generated module is checked against before it is linked; the host compiler
holds the definition to the prototype, and a policy check holds the rest -- every prototype listed,
every listing declared and listed once, and each fact an entry states on both sides (whether it
takes the engine handle, whether it can raise) stated alike.

**A type is told to the library as a record both sides read.** What clang gets from a template
argument on the C++ backend, the prebuilt library gets as data: a record laid out as C lays one out,
declared once in a library header, which the code generator lays out by `offsetof` on that
declaration and the library reads as a struct. It holds the address of the type's table, the size
and alignment of a value, and which of the records the library reads a type through it opens; a
record adding numbers of its own -- a tuple's components, a fixed array's count and element type, an
integral type's shape -- opens with it, so the type's address is the record's. The table is a record
of function pointers, each handed the type first, and holds what every holder of a value asks: a
copy, a move, an end, a write over a value already there, and whether a write changed the value,
which a cell, a net or an element asks on every write because any variable may be waited on (LRM
9.4.2). What a value answers about its parts by position is a second record of the same form. The
library defines its own kinds' types through the same declaration, and every integral type answers
through the library's one table.

Every other operation of a type is handed to what asks it, as a record of its own: comparing,
testing for unknown bits and streaming are one record per type, handed after the operands of a call
that compares or streams and kept by nothing, a container's naming its element's; how a net of an
aggregate type resolves its drivers is a record the net is built holding; which positions a takeover
holds is a function handed where it begins. A container type's record also holds the addresses of
the type's own bodies the library's steps call -- where a write through a position or an index
lands, and the array made a count long -- so the library's steps are handed only the array's type.
What a value of a type is before anything is written to it is MIR's, handed with the type as a
constant naming the type and the function built for its default; no type answers its own default.

**A library structure whose members generated code reaches -- a container's core, a queue's recycler
-- is laid out by one C declaration in a library header**, which the execution backend reads by
`offsetof` and the C++ backend's class for the type derives from. It is a contract between a
compiler and the library of the same release, as Go's slice header is, and never an interface a
program or another release reads.

**A type the library declares states once, on its declaration, how a holder holds it** -- by value
in storage of a stated size and alignment, as an object of its own, or by address -- because a layer
compiled without the runtime sizes a frame holding one by that statement. The runtime asserts the
stated storage against every class each backend builds there, in a header compiled where the library
is and where a program built against it is, so a frame holds what MIR counted. Adding a library type
pairs its declaration with those classes in one listing, without which the runtime does not compile.

The layout is the host's, since the backend compiles for the host; compiling for another target lays
the same declaration out for it, as Go's compiler does with `abi.Type`. Generated classes extending
the library's scope and object still follow the C++ ABI, since the C++ backend derives from them
([object_model.md](object_model.md)).

## Two consumers of the runtime

- **Bundled (portable artifact).** `emit cpp` copies the runtime -- headers and static library, and
  for a project built with `--lto` or `--sanitize address` its sources too -- into the output
  directory alongside the generated sources and a build recipe. The directory is then
  self-contained: it builds on another machine of the same platform with no Lyra toolchain present.
  A design that crosses the foreign boundary brings its foreign inputs too
  ([foreign_interface.md](foreign_interface.md)).
- **In place.** `build` and `run`, on either backend, compile and link the design against the
  runtime where it already lives, and copy nothing. What they produce is an executable, which
  carries the library inside it and runs wherever the machine it was linked for does.

Both reduce to one question: _where does the Lyra runtime live for this binary?_

## Locating the runtime

There is a single answer point: a resolver that, given the running binary's own path, returns the
location of each file it ships. Every consumer depends on the resolved location, never on how it was
found, so the resolution strategy changes without touching the emit, build, or run paths.

- **Runfiles (development).** When `lyra` is built and run by Bazel -- the development binary and
  the test suite -- the runtime is staged in the binary's runfiles tree and resolved from there.
- **Install-relative (release).** A released `lyra` is a plain binary with no runfiles. It locates
  its runtime relative to its own executable path -- the convention clang, gcc, and rustc use for
  their resource and sysroot directories -- and a distribution ships the binary alongside its
  runtime.

The ports of the third-party libraries Lyra supports ship the same way and are found by the same
strategies, from the same binary path ([project_model.md](project_model.md)). Which strategy answers
is a property of how `lyra` locates _its own_ runtime, not of the emitted output: an emitted project
carries its own runtime copy and build recipe and is independent of how `lyra` itself was
distributed.

## What a run writes, and where

`run` builds the design and executes the simulation, and says of the source what `build` says. Its
stdout is the simulation's own, so `run` is pipeable as the simulation's output. Its stderr is where
diagnostics go, the compiler's and the simulation's alike. What the compiler said of the source is
printed there before any simulation begins, and a compile error ends the command there. A caller
that wants the simulation's own stderr alone builds the program and runs it, which is what a test of
the simulation's reports does. This is the split `cargo run` makes between a compiler's warnings and
the program it starts. A warning the compiler gives is often the only thing that tells an author
what was made of their text ([language_conformance.md](language_conformance.md)), so no command that
compiles the design keeps one back.

**What the design wrote is delivered on every ending the process can still act on** -- a signal from
outside, a fault, the checker's report, a request for memory the host refuses -- because the last
lines a run printed are what say where it was, and the run stopped from outside is the one that was
not going to end. Each file's finished bytes are held in a buffer of the runtime's own, which
publishes how much of it is finished as each output call completes, and an ending hands that part
over with `write` alone, which is what a signal handler may call where it may call no library. The
buffer is handed over before anything else writes the same destination -- a report on the error
stream, foreign code through the C library's stream, a command the design runs -- so the order is
kept. An ending that runs no code, a kill from outside or the kernel's, loses at most one buffer per
file. A value dump is finished on the same endings, the refused request for memory excepted, by its
own writing thread ([runtime_model.md](runtime_model.md)).

**Output the system refused is reported where the design was never told of it.** At the run's end,
each file whose output was refused -- a buffer handed over, a write past the buffer, the drain of
the stream foreign code wrote, a close -- without `$ferror` having answered the design that refusal
(LRM 21.3.7) is reported once as an error, and the run exits failing. A design that asked and was
answered handles the loss itself.

## Where a report is placed

A failure the library meets on the design's behalf is reported at the statement that asked for it,
and the library is entered through few functions and reaches a failure through many. So the place
travels the way a request context does in a server, never as a parameter.

**A door binds the place for its call's extent.** A door is a function program code calls -- an
entry of the C ABI, or a method the C++ backend names. It binds the place it is handed until it
returns and then restores what was bound before; past it the library takes no place at all. A
function of the library's interior, a type's table among them, knows nothing of where it was called
from, and a raise or a report reads what is bound. A door reached through another door is handed the
place on, and a door handed no place binds nothing, so the place bound where the table was reached
stands. No door's extent spans a suspension, so nothing has to clear a binding.

**What the library keeps past a door keeps its place as a value** and binds it when it runs: a
`randomize()` problem, a dump opened at the end of a time step, a `context` import's call. A plain
import states no extent (LRM 35.5.3), so what its foreign code does wrong is the run's own and is
placed at no statement of the design's.

**A report made under no binding is the tool's failure.** Every way into the library that can fail
the design binds a place, so a failure of the design raised with none bound is an internal error of
Lyra's, never a report at the wrong statement.

**What a report names beside its place.** One the design's code makes names in its note the scope
that code is written in, built as `%m` builds it (LRM 20.10, 21.2.1.5). A failure the tool raises,
and a report made by a body of the tool's own text, names the procedure that was running, as a panic
names its thread.

## Out of scope

- The install layout and packaging of a `lyra` distribution: where the runtime sits relative to the
  installed binary and how the two are packaged together. The resolver above is the single seam
  where that work lands.

## Forbidden shapes

- **A function, a family member or a dispatch table of the library's compiled into every unit that
  reaches it.** It fails the requirement that work the same for every unit is done once where the
  shared artifact is made: each unit then pays the library's compile and disk again, in proportion
  to the design.
- **A constraint on a member a unit calls across the library boundary.** It fails compiling the
  design with whichever compiler the user has, since the compilers that build the library and the
  unit spell a constrained member's name differently (the premise: the Itanium ABI states no
  spelling of a constraint).
- **A check on the headers for the shapes that copy library code.** It fails the same requirement
  where nobody has yet found the shape; the object is where every such shape shows.
- **A runtime entry's signature written by hand beside its call, or its symbol composed from a
  string.** It fails the requirement that an entry and its call agree by construction: a declaration
  is held to the call's arguments and never to the runtime's definition, so a disagreement is
  undefined behaviour at run time.
- **Generated code posing as the library's C++ classes -- a table laid out by guessing the
  compiler's layout, a field found by measuring a probe object, a table named by its mangled
  symbol.** It fails the same requirement for types: it rests on a layout no header publishes, so a
  reordered declaration moves a call to the wrong function with nothing to say so.
- **One table per type holding every operation of the type.** It fails the requirement that code
  compiled once is handed only what its job reads: every holder of a value would carry comparison,
  streaming and net resolution it never asks.
- **Output flushed through the C library from a signal handler, or left in a buffer an ending does
  not hand over.** The first fails delivering on an ending the process can act on without
  deadlocking it; the second fails delivery itself.
- **A place passed as a parameter through every function of the library.** It fails the requirement
  that a failure added deep in the library changes no signature: every function between a door and
  the failure would gain the parameter.
- **A binding that falls back to the running procedure where none was made.** It fails a report
  placed at the statement that failed: the procedure's place is right for no failure of an
  operation, and the fallback hides the door that bound nothing.
