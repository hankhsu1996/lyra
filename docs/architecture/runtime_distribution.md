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
spelled in. The design owes that boundary a second thing: a definition of every exported name, since
the C side calls a symbol nothing else defines. All of it is produced next to the emitted sources,
for every design, so a foreign source compiles against one include path and links against one
boundary it never has to restate by hand.

This surface is one artifact set for the whole design rather than one per unit, and that does not
contradict the per-unit artifact boundary. A DPI-C name is program-global and lives in its own name
space rather than in any compilation unit's (LRM 35.4, 35.7), and every declaration of one name must
publish the same prototype (LRM 35.5.4), so the surface is a program-level fact by construction. Two
scopes may even export the same name (LRM 35.4), so no single unit can own the symbol; splitting the
surface per unit would invent a boundary the language says is not there, and would leave the
program-global uniqueness rule with nowhere to be checked.

What keeps that from becoming the whole-design aggregate `emission_model.md` forbids is what the
surface may contain. It carries the foreign name space and nothing else: a name, its prototype, and
-- for an export -- a definition of the symbol whose body is one runtime-SDK call naming that same
name and prototype. It holds no unit's body, names no unit, and states no design semantics, so it
neither serializes nor constrains the per-unit compilation. How an exported call reaches the
subroutine behind it is the SDK's, which is the same substrate every other cross-unit operation
resolves through (`emission_model.md` inv 3); the emitted definition only binds the symbol to its
signature.

The declaration half is target-language-neutral: it projects the same prototypes any backend links
against, so a foreign source compiled against it stays correct whichever backend runs the design.
The definition half is stated in the design root's own IR, which is where the whole design is read
and the only place a program-global symbol has an owner. Each backend then emits it the way it emits
anything else that unit owns, so no backend carries emission machinery specific to this boundary.

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
