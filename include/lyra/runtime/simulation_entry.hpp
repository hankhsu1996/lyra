#pragma once

#include <functional>
#include <memory>
#include <string_view>
#include <utility>

#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

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
// command-line plusarg tokens off `argv`, constructs the Runtime seeded with
// those tokens, asks `make` for the design's `$root` under `root_name` (whose
// generated constructor elaborates the design), binds the built tree, and
// drives the scheduler to completion. Returns the simulation's exit code.
// Elaboration precedes the simulation (LRM 3.12), so a failure while the
// design is built or resolved is reported here and the run never starts;
// everything from time-zero initialization onward is the run's own.
//
// From here on a request for memory the host refuses ends the program where it
// was made: one line on the diagnostic channel, a failing exit status, and no
// `final` procedure, since whatever ran next could ask again.
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

// The same for a root a target builds as its own class `T`, which is how the
// entry an emitted program names answers.
template <typename T>
auto RunDesignRoot(
    int argc, char** argv, std::string_view root_name,
    std::unique_ptr<T> (*make)(Scope*, HierarchySegment)) -> int {
  return RunDesignRoot(
      argc, argv, root_name,
      RootFactory([make](Scope* parent, HierarchySegment segment) {
        return std::unique_ptr<Scope>(make(parent, std::move(segment)));
      }));
}

}  // namespace lyra::runtime
