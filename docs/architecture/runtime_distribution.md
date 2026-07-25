# Runtime Distribution

The C++ backend emits a program that links the **Lyra C++ runtime**: a standard-library-only static
library and its headers. The emitted code depends on nothing else from the Lyra toolchain -- no
third-party libraries. The runtime is, in effect, a small library that ships with the compiler and
that emitted programs link against.

## Two consumers of the runtime

- **Bundled (portable artifact).** `emit cpp` and `compile` copy the runtime -- headers and static
  library -- into the output directory alongside the generated sources and a build recipe. The
  directory is then self-contained: it builds on another machine of the same platform with no Lyra
  toolchain present.
- **In place (ephemeral).** `run` builds the generated sources directly against the runtime where it
  already lives and executes the result. It copies nothing.

Both reduce to one question: _where does the Lyra runtime live for this binary?_

## Locating the runtime

There is a single answer point: a resolver that, given the running binary's own path, returns the
runtime's include root and static library. Every consumer depends on the resolved location, never on
how it was found. The resolution strategy is therefore free to change without touching the emit,
compile, or run paths.

Resolution strategies:

- **Runfiles (development).** When `lyra` is built and run by Bazel -- the development binary and
  the test suite -- the runtime is staged in the binary's runfiles tree and resolved from there.
- **Install-relative (release).** A released `lyra` is a plain binary with no runfiles. It locates
  its runtime relative to its own executable path -- the convention clang, gcc, and rustc use for
  their resource and sysroot directories. A distribution ships the binary alongside its runtime, and
  the binary finds it from `argv0` / the executable path. This strategy drops into the same
  resolver; the emit, compile, and run paths are unaffected.

Until the install-relative strategy exists, `run`, `emit cpp`, and `compile` work only where
runfiles are present (the Bazel build tree and the tests). This is a property of how `lyra` locates
_its own_ runtime, not of the emitted output: an emitted project, once produced, carries its own
runtime copy and build recipe and is independent of how `lyra` itself was distributed.

## The foreign-language boundary surface

A design that crosses the DPI-C boundary (LRM 35) has a second consumer of its output: the user's
own C sources. They need the prototype of every foreign name the design takes part in -- the imports
they must define and the exports they may call -- plus the standard header those prototypes are
spelled in. Both are produced next to the emitted sources, for every design, so a foreign source
compiles against one include path and never restates the ABI by hand.

This surface is one artifact for the whole design rather than one per unit, and that does not
contradict the per-unit artifact boundary. A DPI-C name is program-global and lives in its own name
space rather than in any compilation unit's (LRM 35.4, 35.7), and every declaration of one name must
publish the same prototype (LRM 35.5.4), so the surface is a program-level fact by construction.
Nothing the design itself compiles includes it; it sits outside the design's compilation graph
entirely, so it neither serializes nor constrains the per-unit compilation `emission_model.md`
protects. Splitting it per unit would instead invent a boundary the language says is not there, and
would leave the program-global uniqueness rule with nowhere to be checked.

The header is also target-language-neutral: it projects the same prototypes any backend links
against, so a foreign source compiled against it stays correct whichever backend runs the design.

A bundled project carries this surface, and a copy of every foreign source it was given, so it
builds where neither Lyra nor the original foreign sources are reachable. The in-place path produces
the same surface in its work directory; it copies nothing else, as before.

## Command output contract

`run` executes the simulation; its stdout and stderr are the simulation's own. Compile-phase
diagnostics do not bleed into them -- warnings are not shown during `run` (use `dump`, `emit cpp`,
or `compile` to see them), and compile errors are reported and abort before any simulation begins.
This keeps `run` faithfully pipeable and testable as "the simulation's output".

## Out of scope

- The install layout and packaging of a `lyra` distribution: where the runtime sits relative to the
  installed binary and how the two are packaged together. The resolver above is the single seam
  where that work lands.
