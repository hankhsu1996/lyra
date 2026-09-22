#pragma once

#include <functional>
#include <memory>
#include <string_view>

#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

class Runtime;

// The entry a design's root unit publishes for making its object, as a host
// reaches it: the owner every object takes -- none, for this one -- and the
// structural identity it carries. Both are the host's to supply, so an emitted
// program names the entry and the root's label and never composes a runtime
// value of either. Services are reached from the constructed root (and every
// child it builds) through the thread-local `current_runtime()` the owning
// Runtime has already published on this thread, so nothing about the runtime
// crosses here.
using RootFactory =
    std::function<std::unique_ptr<Scope>(Scope*, HierarchySegment)>;

// The host program's design-simulation entry. Collects the LRM 21.6
// command-line plusarg tokens off `argv`, constructs the Runtime seeded
// with those tokens, asks `make` for the design's `$root` under `root_name`
// (whose generated constructor elaborates the design), binds the built
// tree, and drives the scheduler to completion. Returns the simulation's
// exit code. Elaboration precedes the simulation (LRM 3.12), so a failure
// while the design is built or resolved is reported here and the run never
// starts; everything from time-zero initialization onward is the run's own.
//
// This is the entry the emitted `main` calls, and every host-boundary concern
// is behind it -- argv parsing, engine construction, the root's structural
// identity, bind, scheduler drive, exception mapping. That is what keeps an
// emitted program's whole contribution to two names, so a future host concern
// (a seed flag, a waveform sink, a deadline, verbosity, signal handling) is
// added by editing runtime C++ rather than by growing what the emitter writes.
auto RunDesignRoot(
    int argc, char** argv, std::string_view root_name, const RootFactory& make)
    -> int;

// Boundary between a host program and the simulation Runtime. Drives a bound
// Runtime to completion and reports whatever the run could not itself account
// for, mapping it to a failing exit code.
//
// The run accounts for a design's own run-time error and for a failure of the
// tool discovered while it is under way, so what reaches here is what happened
// outside that -- an invariant the engine established for itself, or the host
// running out of memory before the first initializer. A host program that wants
// to construct its own Runtime and make its own root (an embedding API, a
// differential test) calls this directly rather than the entry above, which
// does the whole of it.
auto RunSimulation(Runtime& runtime) -> int;

}  // namespace lyra::runtime
