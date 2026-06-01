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

- **Runfiles (current).** When `lyra` is built and run by Bazel -- the development binary and the
  test suite -- the runtime is staged in the binary's runfiles tree and resolved from there.
- **Install-relative (release; not yet built).** A released `lyra` is a plain binary with no
  runfiles. It locates its runtime relative to its own executable path -- the convention clang, gcc,
  and rustc use for their resource and sysroot directories. A distribution ships the binary
  alongside its runtime, and the binary finds it from `argv0` / the executable path. This strategy
  drops into the same resolver; the emit, compile, and run paths are unaffected.

Until the install-relative strategy exists, `run`, `emit cpp`, and `compile` work only where
runfiles are present (the Bazel build tree and the tests). This is a property of how `lyra` locates
_its own_ runtime, not of the emitted output: an emitted project, once produced, carries its own
runtime copy and build recipe and is independent of how `lyra` itself was distributed.

## Command output contract

`run` executes the simulation; its stdout and stderr are the simulation's own. Compile-phase
diagnostics do not bleed into them -- warnings are not shown during `run` (use `dump`, `emit cpp`,
or `compile` to see them), and compile errors are reported and abort before any simulation begins.
This keeps `run` faithfully pipeable and testable as "the simulation's output".

## Out of scope (for now)

- The install layout and packaging of a `lyra` distribution: where the runtime sits relative to the
  installed binary and how the two are packaged together. The resolver above is the single seam
  where that work lands.
