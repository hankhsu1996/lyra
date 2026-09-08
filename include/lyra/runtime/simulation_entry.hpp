#pragma once

#include <functional>
#include <memory>
#include <string>
#include <string_view>

#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

class Runtime;

// A design-derived callable that allocates the design's `$root` scope. The
// runtime invokes it after building the Runtime but before binding. The builder
// owns the design-specific construction call (the root class, its constructor
// signature, the `$root` display name); nothing else about the host boundary
// leaks into emitted code. Services are reached from the constructed root
// (and every child it builds) through the thread-local `current_runtime()`
// the owning Runtime has already published on this thread; the builder
// takes no runtime argument.
using RootBuilder = std::function<std::unique_ptr<Scope>()>;

// The host program's design-simulation entry. Collects the LRM 21.6
// command-line plusarg tokens off `argv`, constructs the Runtime seeded
// with those tokens, invokes `builder` to allocate the design's `$root`
// (whose generated constructor elaborates the design), binds the built
// tree, and drives the scheduler to completion. Returns the simulation's
// exit code. Elaboration precedes the simulation (LRM 3.12), so a failure
// while the design is built or resolved is reported here and the run never
// starts; everything from time-zero initialization onward is the run's own.
//
// This is the entry the emitted `main` calls. Consolidating every
// host-boundary concern here -- argv parsing, engine construction, bind,
// scheduler drive, exception mapping -- keeps the emitter's string
// surface a one-line hand-off, so future host concerns (e.g. seed CLI
// flags, a VCD sink path, a simulation deadline, verbosity, signal
// handling) are added by editing runtime C++ rather than growing the
// emitter.
auto RunDesignHost(int argc, char** argv, const RootBuilder& builder) -> int;

// The whole of an emitted host's `main`. A design contributes two things and no
// others: the class its `$root` is an instance of, and the label that root
// carries. Allocating the root, standing the engine up, binding, driving, and
// mapping an escaping exception to an exit code all sit behind this call, so a
// host concern is added here rather than by growing what the emitter writes
// out. Allocation is realized per backend -- a JIT host allocates a generic
// runtime object where this allocates a concrete class -- so this entry serves
// the backends that emit target-language classes.
template <typename Root>
auto RunDesign(int argc, char** argv, std::string_view root_name) -> int {
  return RunDesignHost(argc, argv, [root_name]() -> std::unique_ptr<Scope> {
    return std::make_unique<Root>(
        nullptr, HierarchySegment{std::string{root_name}, {}});
  });
}

// Boundary between a host program and the simulation Runtime. Drives a bound
// Runtime to completion and reports whatever the run could not itself account
// for, mapping it to a failing exit code.
//
// The run accounts for a design's own run-time error and for a failure of the
// tool discovered while it is under way, so what reaches here is what happened
// outside that -- an invariant the engine established for itself, or the host
// running out of memory before the first initializer. A host program that wants
// to construct its own Runtime and drive its own root allocation (an embedding
// API, a differential test) calls this directly instead of RunDesignHost.
auto RunSimulation(Runtime& runtime) -> int;

}  // namespace lyra::runtime
