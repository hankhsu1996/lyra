# The pinned clang builds the Lyra that is run

Date: 2026-10-30. Status: accepted. Replaces the record #1839 wrote, which held every build of Lyra,
by any compiler, to writing the same output.

## The requirement

> For a fixed build of Lyra, design, set of options, target and LLVM release, everything Lyra writes
> -- each intermediate form, the emitted C++, the program it links -- is the same on every run.

A failure a user reports is reproduced by the same Lyra revision built the way Lyra is built. A test
that compares what Lyra writes, for one design at two sizes or against a recorded count, runs one
executable. A build Lyra keeps is found again by what it read, Lyra's own executable among it where
Lyra's code generator wrote the result, so a result one build of Lyra wrote is never found for
another that writes it differently. None of these needs two different builds of Lyra to write alike.

## The fact that raised it

Lyra builds each intermediate form by calls that append to an arena or a builder, and many sites
pass two such calls as arguments of one call. C++ leaves the order of a call's arguments to the
compiler ([expr.call]), and of most operators' operands ([intro.execution]), and clang and GCC
choose opposite ones. Built by GCC, Lyra numbered nodes, ordered locals and placed instructions
differently from the same Lyra built by clang, for most cases of the conformance corpus; the
programs answered alike. It was found when the sanitizer nightly, which then built Lyra with GCC,
failed two growth tests a clang-built Lyra passes.

## Decisions

**D1. The Lyra that is run is the one the pinned clang builds.** The build fetches that clang on
every host and needs nothing installed there
([a-build-compiles-for-the-oldest-platform-it-supports](a-build-compiles-for-the-oldest-platform-it-supports.md)),
so whoever builds Lyra from source builds this one. Every test that reads what Lyra writes runs that
build.

**D2. GCC builds Lyra for its warnings, and its output is not compared.** The merge gate builds Lyra
under GCC because every warning is fatal and each compiler warns of things the other does not. That
build is not run as a product, so the order GCC gives two building calls is not a defect, and no
check holds a source to an order between them. The sanitizer nightly runs the pinned clang's build.

**D3. An order a result depends on is written as statements.** Where one operand reads what another
changes, so that the answer and not only a numbering follows the order, the operands are bound to
locals in the order meant, as ordinary C++ owes for any host compiler, the pinned clang's next
release included. A braced initializer, whose clauses C++ runs first to last ([dcl.init.list]/4),
states one too.

## What the field does

- **Reproducible Builds** defines a reproducible build as one whose output is the same for the same
  source, build environment and instructions (reproducible-builds.org, "Definitions"); the compiler
  is part of the environment, not a variable the output is held constant across.
- **Chromium** builds with the clang its build fetches and does not support other compilers for its
  shipped build; **rustc** builds itself with a pinned earlier rustc it downloads.
- **LLVM and flang** are built and tested under several host compilers, and fix a site where two
  builder calls share an evaluation when a test written against one compiler's output fails under
  another (llvm-project #125055), without a rule or a check for the shape.

## Rejected

- **Holding every compiler's build of Lyra to the same output**, with a reading of the source for
  two building operands on every pull request and a nightly comparison of a clang-built and a
  GCC-built Lyra over the corpus. Built and run from #1839: the reading needed a model of which
  calls build -- type names, lambda bodies, exemptions -- and took 14 minutes on a 63-file pull
  request against a five-minute budget, to guard an equality no requirement above asks for.
- **Keeping a sanitizer run of the GCC build**, which GCC's opposite operand order would make find a
  read of storage one operand frees and another reads, where that order is the one that frees first.
  Such a read is a defect only of a Lyra built in that order, and none is run; a read the pinned
  clang's order makes is found by the sanitizer run of its build.
- **Dropping the GCC build.** Its warnings are what it is for, and they are worth a gate.
- **Undoing the sequencing #1839 wrote.** A building operand bound to a local states its order and
  reads no worse; what is dropped is the rule that every new site must.
