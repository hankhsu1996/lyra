# The pinned clang builds the Lyra that is run

Status: Accepted, 2026-10-30. Replaces the earlier choice that every build of Lyra, by any compiler,
writes the same output.

## Requirement

For a fixed build of Lyra, design, set of options, target and LLVM release, everything Lyra writes
-- each intermediate form, the emitted C++, the program it links -- is the same on every run. A
reported failure is reproduced by the same revision built the way Lyra is built; a test comparing
what Lyra writes runs one executable; a kept build is found again by what it read, Lyra's own
executable among it. None of these needs two different builds of Lyra to write alike.

What raised it is that Lyra builds its intermediate forms by calls that append to an arena or a
builder, and many sites pass two such calls as arguments of one call, whose order C++ leaves to the
compiler ([expr.call], [intro.execution]). Built by GCC, Lyra numbered nodes and placed instructions
in the reverse of clang's order for most of the conformance corpus; the programs answered alike.

## Choice

- **The Lyra that is run is the one the pinned clang builds.** The build fetches that clang on every
  host
  ([a-build-compiles-for-the-oldest-platform-it-supports](a-build-compiles-for-the-oldest-platform-it-supports.md)),
  so whoever builds Lyra from source builds this one, and every test reading what Lyra writes runs
  it.
- **GCC builds Lyra for its warnings, and its output is not compared.** That build is no product, so
  the order GCC gives two building calls is no defect.
- **An order a result depends on is written as statements.** Where one operand reads what another
  changes, so that the answer and not only a numbering follows the order, the operands are bound to
  locals in the order meant, as ordinary C++ owes any host compiler, the pinned clang's next release
  included. A braced initializer, which C++ runs first to last ([dcl.init.list]), states one too.

Reproducible Builds defines a reproducible build against a fixed build environment, the compiler
part of it; Chromium and rustc each build with a compiler their build fetches.

## Alternatives turned down

- **Holding every compiler's build of Lyra to the same output**, by reading the source for two
  building operands on every pull request and comparing a clang-built and a GCC-built Lyra nightly.
  It fails no requirement above by being absent, and fails the five-minute pull-request budget by
  being present, since the reading needed a model of which calls build and took many times that
  budget.
- **A sanitizer run of the GCC build.** It fails checking the Lyra that is run, since a read of
  freed storage that only GCC's opposite operand order produces is a defect of a Lyra nobody runs.
  The pinned clang's build is checked by its own sanitizer run.
- **Dropping the GCC build.** It fails the merge gate's coverage of warnings, since each compiler
  warns of things the other does not and every warning is fatal.

## Consequences across layers

- The order of a SystemVerilog evaluation's operands is a different requirement, about what Lyra
  emits for the design, and is decided above every target
  ([an-evaluation-runs-its-operands-first-to-last](an-evaluation-runs-its-operands-first-to-last.md)).
