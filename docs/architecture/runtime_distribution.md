# Runtime Distribution

Every program a backend produces links the **Lyra C++ runtime**: a static library and, for the C++
backend's sources, its headers. The emitted code depends on nothing else from the Lyra toolchain;
the third-party code the runtime uses is inside the library. The runtime is, in effect, a small
library that ships with the compiler and that emitted programs link against. The LLVM backend's
modules reach it through the C ABI the library publishes rather than through its headers.

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
  sanitizer runtime.** The execution backend instruments a design's code with the LLVM release Lyra
  links, and instrumented code names its runtime's version, so a checked program links both. It
  carries machine code alone, since the release refuses to instrument a fat object's modules a
  second time. The release's `llvm-symbolizer` ships beside its sanitizer runtime, which starts it
  to name the functions in a report.

## Two consumers of the runtime

- **Bundled (portable artifact).** `emit cpp` copies the runtime -- headers and static library, and
  for a project built with `--lto` or `--sanitize address` its sources too -- into the output
  directory alongside the generated sources and a build recipe. The directory is then
  self-contained: it builds on another machine of the same platform with no Lyra toolchain present.
- **In place.** `build` and `run`, on either backend, compile and link the design against the
  runtime where it already lives, and copy nothing. What they produce is an executable, which
  carries the library inside it and runs wherever the machine it was linked for does.

Both reduce to one question: _where does the Lyra runtime live for this binary?_

## Locating the runtime

There is a single answer point: a resolver that, given the running binary's own path, returns the
each file it ships. Every consumer depends on the resolved location, never on how it was found. The
resolution strategy is therefore free to change without touching the emit, build, or run paths.

Resolution strategies:

- **Runfiles (development).** When `lyra` is built and run by Bazel -- the development binary and
  the test suite -- the runtime is staged in the binary's runfiles tree and resolved from there.
- **Install-relative (release).** A released `lyra` is a plain binary with no runfiles. It locates
  its runtime relative to its own executable path -- the convention clang, gcc, and rustc use for
  their resource and sysroot directories. A distribution ships the binary alongside its runtime, and
  the binary finds it from `argv0` / the executable path. This strategy drops into the same
  resolver; the emit, build, and run paths are unaffected.

Until the install-relative strategy exists, `run`, `emit cpp`, and `build` work only where runfiles
are present (the Bazel build tree and the tests). This is a property of how `lyra` locates _its own_
runtime, not of the emitted output: an emitted project, once produced, carries its own runtime copy
and build recipe and is independent of how `lyra` itself was distributed.

## The foreign-language boundary surface

A design that crosses the DPI-C boundary (LRM 35) has a second consumer of its output: the user's
own C sources. They need the prototype of every foreign name the design takes part in -- the imports
they must define and the exports they may call -- plus the standard header those prototypes are
spelled in. The design owes that boundary a second thing: a definition of every exported name, since
the C side calls a symbol nothing else defines. All of it is produced next to the emitted sources,
for every design, so a foreign source compiles against one include path and links against one
boundary it never has to restate by hand.

**Each unit states its own part of it, and the union is assembled by whatever collects the files.**
A DPI-C name is program-global and lives in its own name space rather than in any compilation unit's
(LRM 35.4, 35.7), which says the name space belongs to no unit -- not that stating a name is
something a unit may not do. Every unit taking part in a name writes that name's prototype, and the
header a foreign source includes names those fragments and states nothing itself. Repeating an
identical C prototype is what every C header does, so a name several units declare simply appears in
several fragments.

That every declaration of one name must publish the same prototype (LRM 35.5.4) is therefore
checked, for a name crossing units, by the user's own C compiler, which is the one party that sees
them together. There is no mechanism available for it here: LRM 35.4 requires C naming and forbids
overloading, so the mangling that turns a cross-unit type disagreement into a link error in C++ is
not on offer. Within one unit the front end already rejects declarations of one name that disagree.

**A symbol the design must define is defined by every unit that declares it, and the party
assembling the program keeps one.** Several scopes may export one name (LRM 35.4) and they may sit
in different units, so no unit owns the symbol -- but picking an owner is not the only way to reach
one definition, and it is the way that costs a read of every unit. The other way is the one C++ uses
for an inline function or a template instantiation: every artifact that needs the definition emits
it, and whoever resolves names across artifacts keeps one. Each such definition is generated from
the name and the prototype alone, so they are the same text wherever they arise.

The surface is target-language-neutral: it projects the same prototypes any backend links against,
so a foreign source compiled against it stays correct whichever backend runs the design. What
differs per backend is only which party does the keeping -- for a backend that links object files,
its linkage rule for a definition emitted more than once; for one that assembles the program by
textual inclusion, the preprocessor.

Whichever party it is, the rule it applies has to keep a definition nothing in the emitting language
references. A foreign entry point is reached only from outside that language, so a rule free to drop
an unreferenced definition drops it from every artifact at once and the program fails to link. That
is where the analogy to an inline function stops: the approach is the same, the linkage is not.

A bundled project carries this surface, and a copy of every foreign source it was given, so it
builds where neither Lyra nor the original foreign sources are reachable. The in-place path produces
the same surface in its work directory, and copies nothing else.

## Command output contract

`run` executes the simulation; its stdout and stderr are the simulation's own. Compile-phase
diagnostics do not bleed into them -- warnings are not shown during `run` (use `dump`, `emit cpp`,
or `build` to see them), and compile errors are reported and abort before any simulation begins.
This keeps `run` faithfully pipeable and testable as "the simulation's output".

## Out of scope

- The install layout and packaging of a `lyra` distribution: where the runtime sits relative to the
  installed binary and how the two are packaged together. The resolver above is the single seam
  where that work lands.
