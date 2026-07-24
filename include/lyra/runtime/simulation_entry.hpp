#pragma once

#include <memory>

namespace lyra::runtime {

class Runtime;
class Scope;

// A design-derived callable that allocates the design's `$root` scope. The
// emitted host names one such function and passes its address; the runtime
// invokes it after building the Runtime but before binding. The builder owns
// the design-specific construction call (the root class, its constructor
// signature, the `$root` display name); nothing else about the host boundary
// leaks into emitted code. Services are reached from the constructed root
// (and every child it builds) through the thread-local `current_runtime()`
// the owning Runtime has already published on this thread; the builder
// takes no runtime argument.
using RootBuilder = std::unique_ptr<Scope> (*)();

// The host program's design-simulation entry. Collects the LRM 21.6
// command-line plusarg tokens off `argv`, constructs the Runtime seeded
// with those tokens, invokes `builder` to allocate the design's `$root`
// (whose generated constructor elaborates the design), binds the built
// tree, and drives the scheduler to completion. Returns the simulation's
// exit code, mapping any escaping C++ exception to EXIT_FAILURE the same
// way `RunSimulation` does. Never throws.
//
// This is the entry the emitted `main` calls. Consolidating every
// host-boundary concern here -- argv parsing, engine construction, bind,
// scheduler drive, exception mapping -- keeps the emitter's string
// surface a one-line hand-off, so future host concerns (e.g. seed CLI
// flags, a VCD sink path, a simulation deadline, verbosity, signal
// handling) are added by editing runtime C++ rather than growing the
// emitter.
auto RunDesignHost(int argc, char** argv, RootBuilder builder) -> int;

// Boundary between a host program and the simulation Runtime. Drives a
// bound Runtime to completion and converts any escaping C++ exception
// (an InternalError, std::bad_alloc, or other std::exception) into
// EXIT_FAILURE. Never throws.
//
// Runtime::Run is the scheduler proper -- its body describes the LRM
// region order and may throw on invariant violation or allocation
// failure. This function is the host-process boundary that catches
// such exceptions and maps them to an exit code. A host program that
// wants to construct its own Runtime and drive its own root allocation
// (an embedding API, a differential test) calls this directly instead
// of RunDesignHost.
auto RunSimulation(Runtime& runtime) -> int;

}  // namespace lyra::runtime
